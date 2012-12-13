module Main where
--
-- TODO check 'async' module
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
import System.IO
import Control.Exception.Base hiding (handle)
import Data.Char
import Data.Maybe
import Control.Monad

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
    { tCmd  :: Cmd
    , tWant :: TMVar Want
    }

data Event = Exit (Maybe ProcessStatus) | Transition Want

data Action = Start
            | Restart
            | Ignore
            | Terminate ProcessID

versionString :: String
versionString = "0.0.0"

tee :: String
tee = "tee"

mkTask :: Config -> (Config -> String) -> (Config -> [String]) -> Want -> IO Task
mkTask cfg cmdf argsf w = do
    wants <- newTMVarIO w
    return Task{tCmd = (cmdf cfg, argsf cfg), tWant = wants}

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
    changeWorkingDirectory (dir cfg) >> newEmptyTMVarIO >>= \mvar ->
        loop (t, cfg, fds) 0 mvar (want cfg) Nothing

loop :: (Task, Config, [Maybe Fd]) -> Int -> TMVar Event -> Want -> Maybe ProcessID -> IO (Maybe ProcessID)
loop state@(t, cfg, fds) restarts mvar wants mpid = do
    e <- atomically $ orElse (takeTMVar mvar)
                             (takeTMVar (tWant t) >>= return . Transition)

    let wants' = case e of Transition w -> w
                           _            -> wants

    case decide e cfg wants mpid restarts of
        Start         -> start >>= awaitPid wants'
        Restart       -> restartDelay >> start >>= loop state (restarts + 1) mvar wants'
        Ignore        -> awaitPid wants' Nothing
        Terminate pid -> Sig.signalProcess Sig.sigTERM pid >> awaitPid wants' Nothing

    where
        start         = spawnProcess t fds mvar >>= return . Just
        awaitPid      = loop state restarts mvar
        restartDelay  = threadDelay $ 1000 * (delay cfg)

decide :: Event -> Config -> Want -> (Maybe ProcessID) -> Int -> Action
decide (Exit Nothing)                     _ _ _ _ = Ignore
decide (Exit (Just (Exited ExitSuccess))) _ _ _ _ = Ignore
decide (Exit (Just _)) cfg w _ restarts = failWith w (maxRe cfg)
    where
        failWith Up Nothing                 = Restart
        failWith Up (Just m) | restarts < m = Restart
        failWith Up   _                     = Ignore
        failWith Down _                     = Ignore

decide (Transition b)    _ a (Just _)   _ | a == b = Ignore
decide (Transition Up)   _ _ Nothing    _          = Start
decide (Transition Up)   _ _ (Just _)   _          = Ignore
decide (Transition Down) _ _ (Just pid) _          = Terminate pid
decide (Transition Down) _ _ Nothing    _          = Ignore

spawnProcess :: Task -> [Maybe Fd] -> TMVar Event -> IO ProcessID
spawnProcess t fds mvar = do
    pid <- forkProcess $ child fds

    -- Normally, we would close the pipe descriptors (`fds`) here,
    -- but we need to be able to pass them to subsequent child processes
    -- as they are restarted on failure, so we leave them open.

    forkIO $ waitForExit pid mvar

    return pid

    where
        child :: [Maybe Fd] -> IO ()
        child fds' = do
            sequence $ zipWith maybeDup fds' [stdInput, stdOutput, stdError]
                    ++ map closeFd' (catMaybes fds')

            executeFile cmd True args Nothing

            where
                (cmd, args)            = tCmd t
                maybeDup (Just fd) std = dupTo fd std >> return ()
                maybeDup Nothing   _   = return ()
                closeFd' fd            = catch (closeFd fd) ((\_ -> return ()) :: IOException -> IO ())

        waitForExit :: ProcessID -> TMVar Event -> IO ()
        waitForExit pid m = do
            ps <- getProcessStatus True False pid
            atomically $ putTMVar m (Exit ps)

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
handleReq _ _ _ [] = return "NOP"
handleReq (inTask, outTask) cfg wants line =
    case head line of
        's' -> fmap (map toLower . show) (readMVar wants)
        'u' -> atomically (mapM (transition Up) [outTask, inTask]) >> (swapMVar wants Up) >> return ok
        'd' -> atomically (transition Down inTask) >> (swapMVar wants Down) >> return ok
        'x' -> atomically (mapM (transition Down) [inTask, outTask]) >> exitSuccess
        'i' -> return $ fromMaybe "n/a" (ident cfg)
        ___ -> return $ err (" unknown command '" ++ line ++ "'")
    where
        ok            = "OK"
        err m         = "ERROR" ++ m

recvTCP :: (Task, Task) -> Config -> Handle -> MVar Want -> IO a
recvTCP tasks cfg handle w = forever $ do
    hGetLine handle >>= handleReq tasks cfg w >>= hPutStrLn handle

acceptTCP :: (Task, Task) -> Config -> Net.Socket -> MVar Want -> IO a
acceptTCP tasks cfg s w = forever $ do
    (handle, _, _) <- Net.accept s
    hSetBuffering handle NoBuffering
    forkIO $ recvTCP tasks cfg handle w

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

fork :: Task -> Config -> [Maybe Fd] -> IO (MVar ())
fork task cfg fds = do
    done <- newEmptyMVar
    forkFinally (spawn task cfg fds) (\_ -> putMVar done ())
    return done

main :: IO ()
main =
    getCmd >>= execute

    where
        execute (cfg, n) | version cfg = putStrLn (unwords [n, "version", versionString])  >> exitSuccess
        execute (cfg, n) | help    cfg = putStrLn (helpString n) >> exitSuccess
        execute (cfg, _) = do
            wants <- newMVar (want cfg)

            Sig.installHandler Sig.sigPIPE Sig.Ignore Nothing
            Sig.blockSignals $ Sig.addSignal Sig.sigCHLD Sig.emptySignalSet

            (readfd, writefd) <- createPipe

            outTask <- mkTask cfg outCmd outArgs (want cfg)
            inTask  <- mkTask cfg inCmd inArgs (want cfg)

            maybeSock <- maybeListenTCP (inTask, outTask) cfg wants

            outDone <- fork outTask cfg [Just readfd, Nothing, Nothing]
            inDone  <- fork inTask  cfg [Nothing, Just writefd, Just writefd]

            takeMVar inDone >> takeMVar outDone

            closeMaybeSock maybeSock

