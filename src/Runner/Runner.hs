{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Runner where
--
-- TODO use `IO Either` as return type when it makes sense
-- TODO handle ^C properly on tcp connection
--
import Prelude hiding (mapM)
import System.Posix.IO
import System.Posix.Types (ProcessID)
import System.Posix (Fd)
import System.Posix.Process (ProcessStatus(..), getProcessID)
import System.Posix.Directory (changeWorkingDirectory)
import System.Exit
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent.STM
import Control.Concurrent
import Control.Concurrent.Async (race, concurrently)
import Control.Exception hiding (handle)
import Control.Monad hiding (forM, mapM)
import System.IO
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.List (elemIndex)
import Data.Traversable (mapM, forM)
import Data.Typeable (Typeable)

import qualified System.Process       as Proc
import qualified System.Posix.Signals as Sig
import qualified Network              as Net

type Cmd = (String, [String])

data System = System
    { spawnProcess     :: Cmd -> [(String, String)] -> [Maybe Fd] -> IO ProcessID
    , termProcess      :: ProcessID -> IO ()
    , killProcess      :: ProcessID -> IO ()
    , getProcessStatus :: Bool -> Bool -> ProcessID -> IO (Maybe ProcessStatus)
    , childExited      :: MVar ()
    , exit             :: ExitCode -> IO ()
    }

-- | Want represents the desired service state.
data Want = Up | Down deriving (Show, Eq)

data Config = Config
    { inCmd                  :: String
    , outCmd                 :: String
    , killCmd                :: Maybe String
    , killArgs               :: [String]
    , killGracePeriodSeconds :: Int
    , inArgs                 :: [String]
    , outArgs                :: [String]
    , port                   :: Maybe Net.PortID
    , delay                  :: Maybe Int
    , ident                  :: Maybe String
    , maxRe                  :: Maybe Int
    , dir                    :: String
    , exitSig                :: Maybe Sig.Signal
    , once                   :: Bool
    , rawMode                :: Bool
    , want                   :: Want
    , help                   :: Bool
    , version                :: Bool
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
    { inCmd                  = tee
    , outCmd                 = tee
    , killCmd                = Nothing
    , killGracePeriodSeconds = 7
    , inArgs                 = []
    , outArgs                = []
    , killArgs               = []
    , port                   = Nothing
    , delay                  = Nothing
    , ident                  = Nothing
    , maxRe                  = Nothing
    , once                   = False
    , rawMode                = False
    , dir                    = "."
    , exitSig                = Nothing
    , want                   = Up
    , help                   = False
    , version                = False
    }

-- | Used to signal threads that a SIGCHLD was received
chldVar :: MVar ()
chldVar = unsafePerformIO newEmptyMVar

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
spawn :: Task -> Config -> System -> [Maybe Fd] -> Want -> IO ()
spawn t cfg sys fds Down =
    waitWant Up t >> spawn t cfg sys fds Up
spawn t cfg sys@System{..} fds Up = do

    env <- atomically $ readTVar (tEnv t)
    pid <- spawnProcess (tCmd t) env fds

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
            mapM delayRestart (delay cfg)

            atomically (do
                n <- getTaskRestarts t

                case maxRe cfg of
                    Just m | n == m -> return Down
                    _               -> setTaskRestarts t (n + 1) >> return Up) >>= continue

        Right terminate' -> -- terminate² the process
            terminate' >> reapChild pid >>= (atomically . updateTaskStatus t) >> continue Down

    where
        continue = spawn t cfg sys fds
        delayRestart d = threadDelay $ milliseconds * d
        waitExit pid =
            takeMVar childExited >> getProcessStatus False True pid >>= \s ->
                case s of
                    Nothing -> putMVar childExited () >> yield >> waitExit pid
                    Just x  -> return x

        waitDown p = waitWant Down t >> return (terminate p)

        terminate p =
            do
              case killCmd cfg of
                Just cmd -> runCmd cmd >>= reapCmd
                Nothing -> return ()
              termProcess p
              threadDelay $ 1000000 * killGracePeriodSeconds cfg
              killProcess p
            where
                runCmd cmd = Proc.createProcess (Proc.proc cmd (killArgs cfg)){ Proc.close_fds = True }
                reapCmd (_, _, _, handle) = void $ Proc.waitForProcess handle

        getTaskRestarts t' = readTVar  (tRestarts t')
        setTaskRestarts t' = writeTVar (tRestarts t')
        updateTaskStatus t'= putTMVar (tStatus t')
        reapChild pid = takeMVar childExited >> getProcessStatus True True pid

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

        signalExit = forM (exitSig cfg) $ \sig ->
            Sig.signalProcess sig =<< (atomically . readTVar $ tPid outTask)
        (~>) t =
            putTMVar (tWant t)

showPrompt :: State -> Handle -> IO ()
showPrompt state h = do
    raw <- readMVar (sRaw state)
    unless raw $ hPutStr h promptString

recvTCP :: (Task, Task) -> Config -> Handle -> State -> IO ()
recvTCP tasks cfg h s =
    showPrompt s h >> hGetLine h >>= handleReq tasks cfg s >>= hPutStrLn h

acceptTCP :: (Task, Task) -> Config -> System -> Net.Socket -> IO a
acceptTCP tasks cfg System{..} s = forever $ do
    (handle, _, _) <- Net.accept s

    w <- newMVar (want cfg)
    raw <- newMVar (rawMode cfg)

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

listenTCP :: (Task, Task) -> Config -> System -> Net.PortID -> IO Net.Socket
listenTCP tasks cfg sys port' = do
    sock <- Net.listenOn port'
    forkIO (acceptTCP tasks cfg sys sock `catch` (\(_ :: IOException) -> return ()))
    return sock

signals :: Sig.SignalSet
signals = Sig.addSignal Sig.sigCHLD Sig.emptySignalSet

run :: Config -> System -> IO (Maybe Net.Socket)
run cfg sys@System{..} = do
    Sig.installHandler Sig.sigPIPE Sig.Ignore Nothing
    Sig.installHandler Sig.sigCHLD (Sig.Catch $ putMVar childExited ()) Nothing
    Sig.installHandler Sig.sigTERM (Sig.Catch $ exit $ ExitFailure 2) Nothing

    (readfd, writefd) <- createPipe -- 1.

    outTask <- newTask cfg outCmd outArgs
    inTask  <- newTask cfg inCmd inArgs

    maybeSock <- forM (port cfg) $ listenTCP (inTask, outTask) cfg sys

    changeWorkingDirectory (dir cfg)

    -- Spawn the service and the logger concurrently, passing the read
    -- end of the pipe¹ to the logger's stdin, and the write end to the
    -- service's stdout and stderr, such as the output of one is connected
    -- to the input of the other.
    fork $ concurrently (spawn outTask cfg sys [Just readfd, Nothing, Nothing] Up)
                        (spawn inTask  cfg sys [Nothing, Just writefd, Just writefd] (want cfg))

    return maybeSock

    where
        fork io = forkFinally io (\_ -> exit ExitSuccess)

