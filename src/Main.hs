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
    , dir     :: String
    , want    :: Want
    , onExit  :: Maybe Int
    , help    :: Bool
    , version :: Bool
    } deriving Show

data Task = Task
    { tCmd  :: Cmd
    , tPid  :: ProcessID
    , tCtrl :: TMVar Event
    }

versionString :: String
versionString = "0.0.0"

tee :: String
tee = "tee"

mkTask :: Config -> (Config -> String) -> (Config -> [String]) -> TMVar Event -> Task
mkTask cfg cmdf argsf ctrl =
    Task{tCmd = (cmdf cfg, argsf cfg), tPid = -1, tCtrl = ctrl}

defaultConfig :: Config
defaultConfig = Config
    { inCmd   = tee
    , outCmd  = tee
    , inArgs  = []
    , outArgs = []
    , port    = Nothing
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

data Event = TaskExited (Maybe ProcessStatus) | StartTask | StopTask

spawn :: TMVar Want -> String -> Task -> [Maybe Fd] -> IO ()
spawn wants wd t fds = do
    changeWorkingDirectory wd

    newEmptyTMVarIO >>= spawn' Nothing

    where
        spawn' :: (Maybe ProcessID) -> TMVar Event -> IO ()
        spawn' mpid mvar = do
            e <- atomically $ orElse (takeTMVar mvar) (takeTMVar $ tCtrl t)

            -- TODO use `wants` as `tCtrl` - put value back into MVar only when the process
            -- has exited. This allows us to easily stop the service process first, and
            -- the logging process second.

            case e of
                TaskExited ps -> do
                    -- TODO Send signal to output process
                    case ps of
                        Just status ->
                            case status of
                                Exited ExitSuccess -> return ()
                                _                  -> (atomically $ readTMVar wants) >>= failWith
                            where
                                failWith Up   = atomically (putTMVar (tCtrl t) StartTask) >> restartDelay >> spawn' Nothing mvar
                                failWith Down = return ()
                                restartDelay  = threadDelay 1000000 -- 1 second
                        Nothing ->
                            return ()
                StartTask -> do
                    pid <- forkProcess $ child fds
                    -- Normally, we would close the pipe descriptors (`fds`) here,
                    -- but we need to be able to pass them to subsequent child processes
                    -- as they are restarted on failure, so we leave them open.
                    forkIO $ waitForExit pid mvar
                    spawn' (Just pid) mvar
                StopTask -> case mpid of
                    Just pid ->
                        signalProcess sigTERM pid
                    _ ->
                        return ()

        waitForExit :: ProcessID -> TMVar Event -> IO ()
        waitForExit pid mvar = do
            ps <- getProcessStatus True False pid
            atomically $ putTMVar mvar (TaskExited ps)

        maybeClose :: Maybe Fd -> IO ()
        maybeClose (Just fd) = catch (closeFd fd) ((\_ -> return ()) :: IOException -> IO ())
        maybeClose _         = return ()

        child :: [Maybe Fd] -> IO ()
        child fds' = do
            sequence $ zipWith maybeDup fds' [stdInput, stdOutput, stdError]
            sequence $ map maybeClose fds'

            executeFile cmd True args Nothing

            where
                (cmd, args) = tCmd t
                maybeDup (Just fd) std = dupTo fd std >> return ()
                maybeDup Nothing   _   = return ()

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

handleReq :: (Task, Task) -> TMVar Want -> String -> IO String
handleReq (inTask, outTask) wants line =
    case line of
        "?" -> fmap (map toLower . show) (atomically $ readTMVar wants)
        "u" -> atomically (putTMVar wants Up)   >> startService inTask >> return ok
        "d" -> atomically (putTMVar wants Down) >> stopService inTask >> return ok
        "x" -> stopService inTask >> stopService outTask >> exitSuccess
        cmd -> return $ err (" unknown command '" ++ cmd ++ "'")
    where
        ok            = "OK"
        err m         = "ERROR" ++ m
        stopService t = atomically $ putTMVar (tCtrl t) StartTask
        startService t = atomically $ putTMVar (tCtrl t) StopTask

recvTCP :: (Task, Task) -> Handle -> TMVar Want -> IO a
recvTCP tasks handle w =
    forever $ hGetLine handle >>= handleReq tasks w >>= hPutStrLn handle
    -- Consider using hGetChar

acceptTCP :: (Task, Task) -> Net.Socket -> TMVar Want -> IO a
acceptTCP tasks s w = forever $ do
    (handle, _, _) <- Net.accept s
    hSetBuffering handle NoBuffering
    forkIO $ recvTCP tasks handle w

listenTCP :: (Task, Task) -> Int -> TMVar Want -> IO Net.Socket
listenTCP tasks p wants = do
    sock <- Net.listenOn $ Net.PortNumber $ fromIntegral p
    forkIO $ acceptTCP tasks sock wants
    return sock

main :: IO ()
main =
    getCmd >>= execute

    where
        execute (cfg, _) | version cfg = putStrLn versionString  >> exitSuccess
        execute (cfg, n) | help    cfg = putStrLn (helpString n) >> exitSuccess
        execute (cfg, _) = do
            wants   <- newTMVarIO (want cfg)
            ctrlIn  <- newTMVarIO StartTask
            ctrlOut <- newTMVarIO StartTask

            installHandler sigPIPE Ignore Nothing
            blockSignals $ addSignal sigCHLD emptySignalSet

            (readfd, writefd) <- createPipe

            done <- newEmptyMVar

            let fork task fds = forkFinally (spawn wants (dir cfg) task fds) (\_ -> putMVar done ()) in do

                fork (outTask ctrlOut) [Just readfd, Nothing, Nothing]
                fork (inTask ctrlIn)   [Nothing, Just writefd, Just writefd]

            sock <- case (port cfg) of
                Nothing -> return Nothing -- is it ok to duplicate tasks?
                Just p  -> return (Just $ listenTCP (inTask ctrlIn, outTask ctrlOut) p wants)

            takeMVar done >> takeMVar done

            case sock of
                Nothing -> return ()
                Just s  -> s >>= Net.sClose

            where
                inTask  = mkTask cfg inCmd inArgs
                outTask = mkTask cfg outCmd outArgs

