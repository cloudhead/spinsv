{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
--
-- TODO use `IO Either` as return type when it makes sense
-- TODO handle ^C properly on tcp connection
--
import System.Posix.IO
import System.Posix.Types (ProcessID)
import System.Posix (Fd)
import System.Posix.Process
import System.Posix.Directory
import System.Process (createProcess, waitForProcess, proc, close_fds)
import System.Exit
import System.Environment
import System.Console.GetOpt
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent.STM
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception hiding (handle)
import System.IO
import Data.Char
import Data.Maybe
import Data.List (elemIndex)
import Control.Monad
import Data.Typeable

import qualified System.Posix.Signals as Sig
import qualified Network              as Net

type Cmd = (String, [String])

-- | Want represents the desired service state.
data Want = Up | Down deriving (Show, Eq)

data Config = Config
    { inCmd   :: String
    , outCmd  :: String
    , killCmd :: Maybe String
    , killArgs:: [String]
    , inArgs  :: [String]
    , outArgs :: [String]
    , port    :: Maybe Int
    , delay   :: Int
    , ident   :: Maybe String
    , maxRe   :: Maybe Int
    , dir     :: String
    , exitSig :: Maybe Sig.Signal
    , once    :: Bool
    , want    :: Want
    , help    :: Bool
    , version :: Bool
    } deriving Show

data Task = Task                       -- Wraps a system process
    { tCmd      :: Cmd                 -- The command to run
    , tWant     :: TMVar Want          -- Whether this task should be 'up' or 'down' (See `Want` type)
    , tRestarts :: TVar  Int           -- Number of times this task was restarted
    , tPid      :: TVar  ProcessID     -- Last pid of the underlying process
    , tStatus   :: TMVar (Maybe ProcessStatus)
    , tEnv      :: TVar  [(String, String)]
    }

data State = State
    { sWant :: MVar Want
    , sRaw  :: MVar Bool
    }

data UserAction = UserKill | UserQuit deriving (Show, Typeable)
instance Exception UserAction

versionString :: String
versionString = "0.1.0"

tee :: String
tee = "tee"

promptString :: String
promptString = "> "

newTask :: Config -> (Config -> String) -> (Config -> [String]) -> IO Task
newTask cfg cmdf argsf = do
    wants    <- newEmptyTMVarIO
    restarts <- newTVarIO  0
    pid      <- newTVarIO  0
    status   <- newEmptyTMVarIO
    env      <- newTVarIO []

    return Task
        { tCmd      = (cmdf cfg, argsf cfg)
        , tWant     = wants
        , tRestarts = restarts
        , tPid      = pid
        , tStatus   = status
        , tEnv      = env
        }

defaultConfig :: Config
defaultConfig = Config
    { inCmd   = tee
    , outCmd  = tee
    , killCmd = Nothing
    , inArgs  = []
    , outArgs = []
    , killArgs= []
    , port    = Nothing
    , delay   = 1000
    , ident   = Nothing
    , maxRe   = Nothing
    , once    = False
    , dir     = "."
    , exitSig = Nothing
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
    , Option [] ["kill.cmd"]
        (ReqArg (\o cfg -> cfg{killCmd = Just o})              "<cmd>")  "kill command (kill)"
    , Option [] ["kill.arg"]
        (ReqArg (\o cfg -> cfg{killArgs = killArgs cfg ++ [o]}) "<arg>") "kill argument (may be given multiple times)"
    , Option [] ["port"]
        (ReqArg (\o cfg -> cfg{port = Just $ read o})         "<port>") "port to bind to (optional)"
    , Option [] ["id"]
        (ReqArg (\o cfg -> cfg{ident = Just o})                 "<id>") "bind to an identifier (optional)"
    , Option [] ["restart-delay"]
        (ReqArg (\o cfg -> cfg{delay = read o})                 "<ms>") "restart delay in milliseconds (1000)"
    , Option [] ["max-restarts"]
        (ReqArg (\o cfg -> cfg{maxRe = Just $ read o})         "<num>") "max number of service restarts (optional)"
    , Option [] ["dir"]
        (ReqArg (\o cfg -> cfg{dir = o})                      "<dir>")  "directory to run in (.)"
    , Option [] ["exit-signal"]
        (ReqArg (\o cfg -> cfg{exitSig = Just $ read o})    "<signum>") "send a signal to the log process when the service goes down"
    , Option [] ["down"]
        (NoArg  (\cfg   -> cfg{want = Down}))                           "start with the service down"
    , Option [] ["once"]
        (NoArg  (\cfg   -> cfg{once = True}))                           "run the process once, then exit"
    , Option [] ["help"]
        (NoArg  (\cfg   -> cfg{help = True}))                           "print the help and exit"
    , Option [] ["version"]
        (NoArg  (\cfg   -> cfg{version = True}))                        "print the version and exit"
    ]

-- | Used to exit the main thread from any child thread
exitVar :: MVar ExitCode
exitVar = unsafePerformIO newEmptyMVar

exit :: ExitCode -> IO ()
exit = putMVar exitVar

waitWant :: Want -> Task -> IO ()
waitWant w t = do
    v <- atomically $ takeTMVar (tWant t)
    unless (v == w) $ waitWant w t

waitStatus :: Task -> IO (Maybe ProcessStatus)
waitStatus t = atomically $ takeTMVar (tStatus t)

milliseconds :: Int
milliseconds = 1000

-- | Spawns a task and restarts it on failure, if needed.
--
-- We first fork the current process, passing the child a set
-- of file descriptors for stdin, stdout and stderr.
--
-- Then we wait for either of two things to happen: either the
-- process exits, or a user asks for the service to go down.
-- We then act accordingly, either by restarting the process after
-- the configured delay, or killing the process.
--
spawn :: Task -> Config -> MVar () -> [Maybe Fd] -> Want -> IO ()
spawn t cfg chld fds Down =
    waitWant Up t >> spawn t cfg chld fds Up
spawn t cfg chld fds Up = do

    env <- atomically $ readTVar (tEnv t)
    pid <- forkProcess $ child (tCmd t) env fds

    -- Normally, we would close the pipe descriptors (`fds`) here,
    -- but we need to be able to pass them to subsequent child processes
    -- as they are restarted on failure, so we leave them open.

    atomically $ writeTVar (tPid t) pid

    e <- race (waitExit pid) (waitDown pid)

    case e of
        Left (Exited ExitSuccess) ->
            exit ExitSuccess
        Left (Exited status) | once cfg ->
            exit status
        Left _ -> do
            threadDelay $ milliseconds * delay cfg
            atomically (do
                n <- getTaskRestarts t

                case maxRe cfg of
                    Just m | n == m -> return Down
                    _               -> setTaskRestarts t (n + 1) >> return Up) >>= continue

        Right terminate' -> -- terminate² the process
            terminate' >> reapChild pid >>= (atomically . updateTaskStatus t) >> continue Down

    where
        continue = spawn t cfg chld fds
        waitExit pid =
            takeMVar chld >> getProcessStatus False True pid >>= \s ->
                case s of
                    Nothing -> putMVar chld () >> yield >> waitExit pid
                    Just x  -> return x

        waitDown p = waitWant Down t >> return (terminate p)
        terminate p = case killCmd cfg of
            Just cmd -> runCmd cmd >>= reapCmd
            Nothing  -> Sig.signalProcess Sig.sigTERM p -- 2.
            where
                runCmd cmd = createProcess (proc cmd (killArgs cfg)){ close_fds = True }
                reapCmd (_, _, _, handle) = void $ waitForProcess handle

        getTaskRestarts t' = readTVar  (tRestarts t')
        setTaskRestarts t' = writeTVar (tRestarts t')
        updateTaskStatus t'= putTMVar (tStatus t')
        reapChild pid = takeMVar chld >> getProcessStatus True True pid

child :: Cmd -> [(String, String)] -> [Maybe Fd] -> IO ()
child (cmd, args) env fds' = do
    sequence_ $ zipWith maybeDup fds' [stdInput, stdOutput, stdError]
             ++ map closeFd' (catMaybes fds')

    executeFile cmd True args (Just env)

    where
        maybeDup (Just fd) std = void $ dupTo fd std
        maybeDup Nothing   _   = return ()
        closeFd' fd            = catch (closeFd fd) ((\_ -> return ()) :: IOException -> IO ())

getCmd :: IO (Config, String)
getCmd = do
    a <- getArgs
    n <- getProgName

    case getOpt RequireOrder options a of
        (flags, [], []) ->
            return (foldl (\def t -> t def) defaultConfig flags, n)
        (_, nonOpts, []) ->
            error $ "unrecognized arguments: " ++ unwords nonOpts
        (_, _, msgs) ->
            error $ head msgs

handleReq :: (Task, Task) -> Config -> State -> String -> IO String
handleReq t c s [] = handleReq t c s "help"
handleReq (inTask, outTask) cfg state line =
    case line of
        "status" -> status
        "config" -> return $ show cfg
        "up"     -> atomically (inTask ~> Up) >> swapMVar wants Up >> return ok
        "down"   -> down inTask >> swapMVar wants Down >> signalExit >> return ok
        "kill"   -> down inTask >> signalExit >> atomically (outTask ~> Down) >> throwIO UserKill
        "pid"    -> liftM show getProcessID
        "id"     -> return $ fromMaybe "n/a" (ident cfg)
        "help"   -> return help'
        "raw"    -> swapMVar (sRaw state) True >> return ok
        "env"    -> liftM show $ atomically (readTVar $ tEnv inTask)
        "q"      -> throwIO UserQuit
        other    -> case words other of
            "set" : [x] -> case elemIndex '=' x of
                Just i -> atomically $ do
                    vars <- readTVar (tEnv inTask)
                    writeTVar (tEnv inTask) $ (fst pair, tail $ snd pair) : vars
                    return ok
                    where pair = splitAt i x
                Nothing -> errcmd
            ["set"] -> return $ err " usage: set KEY=VALUE"
            ___     -> errcmd

    where
        ok     = "OK"
        err m  = "ERROR" ++ m
        errcmd = return $ err (" unknown command '" ++ line ++ "'")
        help'  = "status, config, up, down, id, kill, pid, raw, help, q"
        wants  = sWant state
        down t = atomically (t ~> Down) >> waitStatus t
        status = do
            w  <- readMVar wants
            rs <- atomically $ sequence [ readTVar (tRestarts inTask)
                                        , readTVar (tRestarts outTask) ]

            return $ unwords $ (map toLower . show) w : map show rs

        signalExit = case exitSig cfg of
            Just sig ->
                Sig.signalProcess sig =<< (atomically . readTVar $ tPid outTask)
            Nothing ->
                return ()
        (~>) t =
            putTMVar (tWant t)


showPrompt :: State -> Handle -> IO ()
showPrompt state h = do
    raw <- readMVar (sRaw state)
    unless raw $ hPutStr h promptString

recvTCP :: (Task, Task) -> Config -> Handle -> State -> IO ()
recvTCP tasks cfg h s =
    showPrompt s h >> hGetLine h >>= handleReq tasks cfg s >>= hPutStrLn h

acceptTCP :: (Task, Task) -> Config -> Net.Socket -> IO a
acceptTCP tasks cfg s = forever $ do
    (handle, _, _) <- Net.accept s

    w <- newMVar (want cfg)
    raw <- newMVar False

    hSetBuffering handle NoBuffering
    forkIO $ (forever $ recvTCP tasks cfg handle State{sWant=w, sRaw=raw})
        `catches`
            [ Handler $ userHandler handle
            , Handler ((\_ -> hClose handle) :: IOException -> IO ()) ]
    where
        userHandler :: Handle -> UserAction -> IO ()
        userHandler h e = hClose h >> case e of
            UserKill -> exit ExitSuccess
            _        -> return ()

maybeListenTCP :: (Task, Task) -> Config -> IO (Maybe Net.Socket)
maybeListenTCP tasks cfg =
    case port cfg of
        Just p -> do
            sock <- Net.listenOn $ Net.PortNumber $ fromIntegral p
            forkIO (acceptTCP tasks cfg sock `catch` (\(_ :: IOException) -> return ()))
            return $ Just sock
        Nothing ->
            return Nothing

closeMaybeSock :: Maybe Net.Socket -> IO ()
closeMaybeSock (Just sock) =
    Net.sClose sock
closeMaybeSock _ =
    return ()

signals :: Sig.SignalSet
signals = Sig.addSignal Sig.sigCHLD Sig.emptySignalSet

run :: Config -> IO ()
run cfg = do
    chld <- newEmptyMVar

    Sig.installHandler Sig.sigPIPE Sig.Ignore Nothing
    Sig.installHandler Sig.sigCHLD (Sig.Catch $ putMVar chld ()) Nothing

    (readfd, writefd) <- createPipe -- 1.

    outTask <- newTask cfg outCmd outArgs
    inTask  <- newTask cfg inCmd inArgs

    maybeSock <- maybeListenTCP (inTask, outTask) cfg

    changeWorkingDirectory (dir cfg)

    -- Spawn the service and the logger concurrently, passing the read
    -- end of the pipe¹ to the logger's stdin, and the write end to the
    -- service's stdout and stderr, such as the output of one is connected
    -- to the input of the other.
    fork $ concurrently (spawn outTask cfg chld [Just readfd, Nothing, Nothing] Up)
                        (spawn inTask  cfg chld [Nothing, Just writefd, Just writefd] (want cfg))

    code <- takeMVar exitVar
    closeMaybeSock maybeSock
    exitWith code

    where
        fork io = forkFinally io (\_ -> exit ExitSuccess)

main :: IO ()
main =
    getCmd >>= execute

    where
        execute (cfg, n) | version cfg = putStrLn (unwords [n, "version", versionString]) >> exitSuccess
        execute (cfg, n) | help    cfg = putStrLn (helpString n) >> exitSuccess
        execute (cfg, _)               = run cfg

