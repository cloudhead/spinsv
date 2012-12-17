{-# LANGUAGE DeriveDataTypeable #-}

module Main where
--
-- TODO use `IO Either` as return type when it makes sense
-- TODO handle ^C properly on tcp connection
--
import System.Posix.IO
import System.Posix (Fd)
import System.Posix.Process
import System.Posix.Types (ProcessID)
import System.Posix.Directory
import System.Exit
import System.Environment
import System.Console.GetOpt
import Control.Concurrent.STM
import Control.Concurrent
import Control.Concurrent.Async
import System.IO
import Control.Exception.Base hiding (handle)
import Data.Char
import Data.Maybe
import Control.Monad
import Data.Typeable

import qualified System.Posix.Signals as Sig
import qualified Network              as Net

type Cmd = (String, [String])

data Want = Up | Down deriving (Show, Eq)

data Config = Config
    { inCmd   :: String
    , outCmd  :: String
    , inArgs  :: [String]
    , outArgs :: [String]
    , port    :: Maybe Int
    , delay   :: Int
    , ident   :: Maybe String
    , maxRe   :: Maybe Int
    , dir     :: String
    , want    :: Want
    , help    :: Bool
    , version :: Bool
    } deriving Show

data Task = Task
    { tCmd      :: Cmd
    , tWant     :: TMVar Want
    , tRestarts :: TMVar Int
    }

data UserQuit = UserQuit deriving (Show, Typeable)
instance Exception UserQuit

data UserKill = UserKill deriving (Show, Typeable)
instance Exception UserKill

versionString :: String
versionString = "0.0.0"

tee :: String
tee = "tee"

prompt :: String
prompt = "> "

mkTask :: Config -> (Config -> String) -> (Config -> [String]) -> Want -> IO Task
mkTask cfg cmdf argsf w = do
    wants    <- newTMVarIO w
    restarts <- newTMVarIO 0

    return Task
        { tCmd      = (cmdf cfg, argsf cfg)
        , tWant     = wants
        , tRestarts = restarts
        }

transition :: Want -> Task -> STM ()
transition w t =
    putTMVar (tWant t) w

defaultConfig :: Config
defaultConfig = Config
    { inCmd   = tee
    , outCmd  = tee
    , inArgs  = []
    , outArgs = []
    , port    = Nothing
    , delay   = 1000
    , ident   = Nothing
    , maxRe   = Nothing
    , dir     = "."
    , want    = Up
    , help    = False
    , version = False
    }

helpString :: String -> String
helpString prog =
    usageInfo header options
    where
        header = unlines [ concat [ "usage: ", prog, " [<option>...]"]
                         ,"\nstart and monitor a service and its appendant log service\n"
                         , "options:"
                         ]

options :: [OptDescr (Config -> Config)]
options =
    [ Option [] ["in.cmd"]
        (ReqArg (\o cfg -> cfg{inCmd = o})                    "<cmd>")  "input command (tee)"
    , Option [] ["out.cmd"]
        (ReqArg (\o cfg -> cfg{outCmd = o})                   "<cmd>")  "output command (tee)"
    , Option [] ["in.arg"]
        (ReqArg (\o cfg -> cfg{inArgs = inArgs cfg ++ [o]})   "<arg>")  "input argument (may be given multiple times)"
    , Option [] ["out.arg"]
        (ReqArg (\o cfg -> cfg{outArgs = outArgs cfg ++ [o]}) "<arg>")  "output argument (may be given multiple times)"
    , Option [] ["port"]
        (ReqArg (\o cfg -> cfg{port = Just $ read o})         "<port>") "port to bind to (optional)"
    , Option [] ["id"]
        (ReqArg (\o cfg -> cfg{ident = Just $ o})               "<id>") "bind to an identifier (optional)"
    , Option [] ["restart-delay"]
        (ReqArg (\o cfg -> cfg{delay = read o})                 "<ms>") "restart delay in milliseconds (1000)"
    , Option [] ["max-restarts"]
        (ReqArg (\o cfg -> cfg{maxRe = Just $ read o})         "<num>") "max number of service restarts (optional)"
    , Option [] ["dir"]
        (ReqArg (\o cfg -> cfg{dir = o})                      "<dir>")  "directory to run in (.)"
    , Option [] ["down"]
        (NoArg  (\cfg   -> cfg{want = Down}))                           "start with the service down"
    , Option [] ["help"]
        (NoArg  (\cfg   -> cfg{help = True}))                           "print the help and exit"
    , Option [] ["version"]
        (NoArg  (\cfg   -> cfg{version = True}))                        "print the version and exit"
    ]

spawn :: Task -> Config -> [Maybe Fd] -> IO (Maybe ProcessID)
spawn t cfg fds =
    changeWorkingDirectory (dir cfg) >> loop (t, cfg, fds)

waitWant :: Want -> TMVar Want -> STM Want
waitWant w var = do
    v <- takeTMVar var

    if v == w then return v
    else           waitWant w var

loop :: (Task, Config, [Maybe Fd]) -> IO (Maybe ProcessID)
loop s@(t, cfg, fds) = do
    w <- atomically $ takeTMVar (tWant t)

    when (w == Up) $ do
        pid <- forkProcess $ child (tCmd t) fds

        -- Normally, we would close the pipe descriptors (`fds`) here,
        -- but we need to be able to pass them to subsequent child processes
        -- as they are restarted on failure, so we leave them open.

        exit <- async $ getProcessStatus True False pid

        decide pid =<< (atomically $ orElse (waitSTM  exit           >>= return . Left)
                                            (waitWant Down (tWant t) >>= return . Right))
    loop s

    where
        decide _ (Left (Just (Exited ExitSuccess))) = return ()
        decide _ (Left (Just _)) = do
            r <- atomically $ takeTMVar (tRestarts t)

            case maxRe cfg of
                Nothing        -> restart r
                Just m | r < m -> restart r
                _              -> return ()

        decide _   (Left Nothing) = return ()
        decide pid (Right _)      = Sig.signalProcess Sig.sigTERM pid

        restart r = do
            atomically  $ transition Up t >> putTMVar (tRestarts t) (r + 1)
            threadDelay $ 1000 * (delay cfg)

child :: Cmd -> [Maybe Fd] -> IO ()
child (cmd, args) fds' = do
    sequence $ zipWith maybeDup fds' [stdInput, stdOutput, stdError]
            ++ map closeFd' (catMaybes fds')

    executeFile cmd True args Nothing

    where
        maybeDup (Just fd) std = dupTo fd std >> return ()
        maybeDup Nothing   _   = return ()
        closeFd' fd            = catch (closeFd fd) ((\_ -> return ()) :: IOException -> IO ())

getCmd :: IO (Config, String)
getCmd = do
    a <- getArgs
    n <- getProgName

    case getOpt RequireOrder options a of
        (flags, [], []) ->
            return $ (foldl (\def t -> t def) defaultConfig flags, n)
        (_, nonOpts, []) ->
            error $ "unrecognized arguments: " ++ unwords nonOpts
        (_, _, msgs) ->
            error $ head msgs

handleReq :: (Task, Task) -> Config -> MVar Want -> String -> IO String
handleReq t c w [] = handleReq t c w "help"
handleReq (inTask, outTask) cfg wants line =
    case line of
        "status" -> status
        "config" -> return $ show cfg
        "up"     -> atomically (transition Up inTask) >> (swapMVar wants Up) >> return ok
        "down"   -> atomically (transition Down inTask) >> (swapMVar wants Down) >> return ok
        "kill"   -> atomically (mapM (transition Down) [inTask, outTask]) >> throwIO UserKill
        "id"     -> return $ fromMaybe "n/a" (ident cfg)
        "help"   -> return help'
        "q"      -> throwIO UserQuit
        ____     -> return $ err (" unknown command '" ++ line ++ "'")
    where
        ok     = "OK"
        err m  = "ERR" ++ m
        help'  = "status, up, down, id, kill, help, q"
        status = do
            w  <- readMVar wants
            rs <- atomically $ sequence [ readTMVar (tRestarts inTask)
                                        , readTMVar (tRestarts outTask) ]

            return $ unwords $ [(map toLower . show) w] ++ (map show rs)

recvTCP :: (Task, Task) -> Config -> Handle -> MVar Want -> IO a
recvTCP tasks cfg h w = forever $ do
    hPutStr h prompt >> hGetLine h >>= handleReq tasks cfg w >>= hPutStrLn h

acceptTCP :: (Task, Task) -> Config -> Net.Socket -> MVar Want -> IO a
acceptTCP tasks cfg s w = forever $ do
    (handle, _, _) <- Net.accept s
    hSetBuffering handle NoBuffering
    forkIO $ recvTCP tasks cfg handle w `catch` ((\_ -> return ()) :: UserQuit -> IO ())

maybeListenTCP :: (Task, Task) -> Config -> MVar Want -> IO (Maybe Net.Socket)
maybeListenTCP tasks cfg wants =
    case (port cfg) of
        Just p -> do
            sock <- Net.listenOn $ Net.PortNumber $ fromIntegral p
            forkIO $ acceptTCP tasks cfg sock wants
            return (Just sock)
        Nothing ->
            return Nothing

closeMaybeSock :: Maybe Net.Socket -> IO ()
closeMaybeSock (Just sock) =
    Net.sClose sock
closeMaybeSock _ =
    return ()

main :: IO ()
main =
    getCmd >>= execute

    where
        execute (cfg, n) | version cfg = putStrLn (unwords [n, "version", versionString]) >> exitSuccess
        execute (cfg, n) | help    cfg = putStrLn (helpString n) >> exitSuccess
        execute (cfg, _) = do
            wants <- newMVar (want cfg)

            Sig.installHandler Sig.sigPIPE Sig.Ignore Nothing
            Sig.blockSignals $ Sig.addSignal Sig.sigCHLD Sig.emptySignalSet

            (readfd, writefd) <- createPipe

            outTask <- mkTask cfg outCmd outArgs Up
            inTask  <- mkTask cfg inCmd inArgs (want cfg)

            maybeSock <- maybeListenTCP (inTask, outTask) cfg wants

            concurrently (spawn outTask cfg [Just readfd, Nothing, Nothing])
                         (spawn inTask  cfg [Nothing, Just writefd, Just writefd])

            closeMaybeSock maybeSock

