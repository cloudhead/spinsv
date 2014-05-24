{-# LANGUAGE RecordWildCards #-}

module Helper where

import           Prelude hiding (mapM)
import           Control.Monad hiding (mapM)
import           Control.Exception
import           Control.Concurrent.STM
import           Control.Concurrent
import           System.Posix.Types (ProcessID, CPid(..))
import           System.Posix.Process (ProcessStatus(..))
import           System.Posix (Fd)
import           System.IO
import           System.Exit
import           System.IO.Unsafe (unsafePerformIO)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Traversable (mapM)
import           Network
import           Runner

data Env = Env
    { processSpawned :: TChan Process
    , processes      :: TVar (Map ProcessID Process)
    }

data Process = Process
    { pCmd    :: Cmd
    , pEnv    :: [(String, String)]
    , pID     :: ProcessID
    , pStatus :: Maybe ProcessStatus
    , pWait   :: TMVar ProcessStatus
    }

config :: Config
config =
    defaultConfig { inCmd = "worker", outCmd = "logger", rawMode = True }

nextPid :: TVar ProcessID
nextPid = unsafePerformIO $ newTVarIO (CPid 1)

getProcesses :: Env -> IO (Map ProcessID Process)
getProcesses Env{..} =
    readTVarIO processes

getProcess :: Env -> ProcessID -> IO Process
getProcess Env{..} pid = do
    procs <- readTVarIO processes

    case Map.lookup pid procs of
        Just p  -> return p
        Nothing -> error $ "process " ++ (show pid) ++ " not found"

processExited :: Env -> ProcessID -> ExitCode -> IO ()
processExited env pid status = do
    proc <- getProcess env pid
    atomically $ do
        putTMVar (pWait proc) (Exited status)
        modifyTVar (processes env) (\ps ->
            Map.adjust (\p -> p { pStatus = Just (Exited status) }) pid ps)

    putMVar childExited' ()

waitForProcessExit :: Env -> Process -> IO ProcessStatus
waitForProcessExit env p = do
    mstatus <- getProcessStatus' env True False (pID p)
    case mstatus of
        Just status -> return status
        Nothing     -> error "process status could not be retrieved"

waitForNewProcess :: Env -> IO Process
waitForNewProcess env =
    atomically $ readTChan (processSpawned env)

runnerDown :: Handle -> IO String
runnerDown sock = do
    hPutStrLn sock "down"
    hGetLine sock

runnerUp :: Handle -> IO String
runnerUp sock = do
    hPutStrLn sock "up"
    hGetLine sock

connect :: PortID -> IO Handle
connect port = do
    runner <- connectTo "localhost" port
    hSetBuffering runner NoBuffering
    return runner

getRunnerStatus :: Handle -> IO (Want, Int, Int)
getRunnerStatus h = do
    hPutStrLn h "status"
    line <- hGetLine h

    case words line of
        ["up", rIn, rOut] ->
            return (Up, read rIn, read rOut)
        ["down", rIn, rOut] ->
            return (Down, read rIn, read rOut)
        _ ->
            error "couldn't parse status line"

killRunner :: Handle -> IO ()
killRunner h =
    hPutStrLn h "kill"

withRunner :: (Integral i) => Config -> i -> (Env -> IO a) -> IO a
withRunner cfg p io = do
    procSpawned <- newTChanIO
    procs       <- newTVarIO Map.empty

    let
        system = System
            { spawnProcess     = spawnProcess' env
            , killProcess      = killProcess' env
            , getProcessStatus = getProcessStatus' env
            , childExited      = childExited'
            , exit             = \_ -> return ()
            }

        env = Env { processes = procs, processSpawned = procSpawned }

    bracket (run cfg { port = Just (PortNumber $ fromIntegral p) } system) (mapM sClose) (\_ -> io env)

spawnProcess' :: Env -> Cmd -> [(String, String)] -> [Maybe Fd] -> IO ProcessID
spawnProcess' env cmd vars _fds = do
    pid <- atomically $ do
        n <- readTVar nextPid
        writeTVar nextPid (CPid $ succ (fromIntegral n))
        return n

    wait <- newEmptyTMVarIO

    let
        proc = Process { pCmd    = cmd
                       , pEnv    = vars
                       , pID     = pid
                       , pStatus = Nothing
                       , pWait   = wait
                       }

    atomically $ do
        modifyTVar (processes env) (\ps -> Map.insert pid proc ps)
        writeTChan (processSpawned env) proc

    return pid

killProcess' :: Env -> ProcessID -> IO ()
killProcess' env pid =
    processExited env pid (ExitFailure 1)

failProcess :: Env -> Process -> IO ()
failProcess env p =
    killProcess' env (pID p)

getProcessStatus' :: Env -> Bool -> Bool -> ProcessID -> IO (Maybe ProcessStatus)
getProcessStatus' env blk _ pid = do
    Process{..} <- getProcess env pid

    case pStatus of
        Nothing | blk ->
            liftM Just (atomically $ takeTMVar pWait)
        status ->
            return status

childExited' :: MVar ()
childExited' = unsafePerformIO newEmptyMVar

