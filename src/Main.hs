module Main where
--
-- TODO check 'async' module
-- TODO use `IO Either` as return type when it makes sense

import System.Posix.IO
import System.Posix (Fd)
import System.Posix.Signals
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

import qualified Network as Net

type Cmd = (String, [String])

data Want = Up | Down deriving (Show)

data Config = Config
    { inCmd   :: String
    , outCmd  :: String
    , inArgs  :: [String]
    , outArgs :: [String]
    , port    :: Maybe Int
    , delay   :: Int
    , ident   :: Maybe String
    , dir     :: String
    , want    :: Want
    , onExit  :: Maybe Int
    , help    :: Bool
    , version :: Bool
    } deriving Show

data Task = Task
    { tCmd  :: Cmd
    , tWakeup :: TMVar ()
    }

versionString :: String
versionString = "0.0.0"

tee :: String
tee = "tee"

mkTask :: Config -> (Config -> String) -> (Config -> [String]) -> IO Task
mkTask cfg cmdf argsf = do
    wake <- newTMVarIO ()
    return Task{tCmd = (cmdf cfg, argsf cfg), tWakeup = wake}

wakeTask :: Task -> STM ()
wakeTask t =
    putTMVar (tWakeup t) ()

defaultConfig :: Config
defaultConfig = Config
    { inCmd   = tee
    , outCmd  = tee
    , inArgs  = []
    , outArgs = []
    , port    = Nothing
    , delay   = 1000
    , ident   = Nothing
    , dir     = "."
    , want    = Up
    , onExit  = Nothing
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
        (ReqArg (\o cfg -> cfg{delay = read o})              "<delay>") "restart delay in milliseconds (1000)"
--  , Option [] ["on-exit"]
--      (ReqArg (\o cfg -> cfg{onExit = Just $ read o})     "<signal>") "send <signal> to output on input exit (IGNORED)"
    , Option [] ["dir"]
        (ReqArg (\o cfg -> cfg{dir = o})                      "<dir>")  "directory to run in (.)"
    , Option [] ["down"]
        (NoArg  (\cfg   -> cfg{want = Down}))                           "start with the service down"
    , Option [] ["help"]
        (NoArg  (\cfg   -> cfg{help = True}))                           "print the help and exit"
    , Option [] ["version"]
        (NoArg  (\cfg   -> cfg{version = True}))                        "print the version and exit"
    ]

data Event = Exit (Maybe ProcessStatus) | Wakeup

spawn :: Task -> TMVar Want -> Config -> [Maybe Fd] -> IO ()
spawn t wants cfg fds =
    changeWorkingDirectory (dir cfg) >> newEmptyTMVarIO >>= \mvar ->
        loop (wants, t, cfg, fds) mvar Nothing

loop :: (TMVar Want, Task, Config, [Maybe Fd]) -> TMVar Event -> Maybe ProcessID -> IO ()
loop state@(wants, t, cfg, fds) mvar mpid = do
    e <- atomically $ orElse (takeTMVar mvar)
                             (takeTMVar (tWakeup t) >> return Wakeup)
    case e of
        -- TODO Send signal to output process
        Exit (Just (Exited ExitSuccess)) ->
            return ()
        Exit (Just _) ->
            atomically (readTMVar wants) >>= failWith
            where
                failWith Up   = restartDelay >> start >>= loop state mvar
                failWith Down = loop state mvar Nothing
                restartDelay  = threadDelay $ 1000 * (delay cfg)
        Exit Nothing ->
            return ()
        Wakeup -> do
            w <- atomically $ readTMVar wants

            loop state mvar =<< case w of
                Up | isNothing mpid ->
                    start
                Up -> -- ignore
                    return mpid
                Down ->
                    case mpid of
                        Just pid ->
                            signalProcess sigTERM pid >> return Nothing
                        _ ->
                            return mpid
    where
        start :: IO (Maybe ProcessID)
        start =
            spawnProcess t fds mvar >>= return . Just

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
            sequence $ map closeFd' (catMaybes fds')

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

handleReq :: (Task, Task) -> Config -> TMVar Want -> String -> IO String
handleReq _ _ _ [] = return "NOP"
handleReq (inTask, outTask) cfg wants line =
    case head line of
        's' -> fmap (map toLower . show) (atomically $ readTMVar wants)
        'u' -> atomically (swapTMVar wants Up   >> wakeTask inTask) >> return ok
        'd' -> atomically (swapTMVar wants Down >> wakeTask inTask) >> return ok
        'x' -> atomically (swapTMVar wants Down >> mapM wakeTask [inTask, outTask]) >> exitSuccess
        'i' -> return $ fromMaybe "n/a" (ident cfg)
        ___ -> return $ err (" unknown command '" ++ line ++ "'")
    where
        ok            = "OK"
        err m         = "ERROR" ++ m

recvTCP :: (Task, Task) -> Config -> Handle -> TMVar Want -> IO a
recvTCP tasks cfg handle w = forever $ do
    hGetLine handle >>= handleReq tasks cfg w >>= hPutStrLn handle

acceptTCP :: (Task, Task) -> Config -> Net.Socket -> TMVar Want -> IO a
acceptTCP tasks cfg s w = forever $ do
    (handle, _, _) <- Net.accept s
    hSetBuffering handle NoBuffering
    forkIO $ recvTCP tasks cfg handle w

maybeListenTCP :: (Task, Task) -> Config -> TMVar Want -> IO (Maybe Net.Socket)
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

fork :: Task -> TMVar Want ->  Config -> [Maybe Fd] -> IO (MVar ())
fork task wants cfg fds = do
    done <- newEmptyMVar
    forkFinally (spawn task wants cfg fds) (\_ -> putMVar done ())
    return done

main :: IO ()
main =
    getCmd >>= execute

    where
        execute (cfg, _) | version cfg = putStrLn versionString  >> exitSuccess
        execute (cfg, n) | help    cfg = putStrLn (helpString n) >> exitSuccess
        execute (cfg, _) = do
            wants <- newTMVarIO (want cfg)

            installHandler sigPIPE Ignore Nothing
            blockSignals $ addSignal sigCHLD emptySignalSet

            (readfd, writefd) <- createPipe

            outTask <- mkTask cfg outCmd outArgs
            inTask  <- mkTask cfg inCmd inArgs

            maybeSock <- maybeListenTCP (inTask, outTask) cfg wants

            outDone <- fork outTask wants cfg [Just readfd, Nothing, Nothing]
            inDone  <- fork inTask  wants cfg [Nothing, Just writefd, Just writefd]

            takeMVar inDone >> takeMVar outDone

            closeMaybeSock maybeSock

