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
    , dir     :: String
    , want    :: Want
    , onExit  :: Maybe Int
    , help    :: Bool
    , version :: Bool
    } deriving Show

data Task = Task
    { tCmd  :: Cmd
    , tPid  :: ProcessID
    , tWakeup :: TMVar ()
    }

versionString :: String
versionString = "0.0.0"

tee :: String
tee = "tee"

mkTask :: Config -> (Config -> String) -> (Config -> [String]) -> TMVar () -> Task
mkTask cfg cmdf argsf wake =
    Task{tCmd = (cmdf cfg, argsf cfg), tPid = -1, tWakeup = wake}

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

data Event = Exit (Maybe ProcessStatus) | Wakeup

spawn :: TMVar Want -> String -> Task -> [Maybe Fd] -> IO ()
spawn wants wd t fds = do
    changeWorkingDirectory wd

    newEmptyTMVarIO >>= spawn' Nothing

    where
        spawn' :: (Maybe ProcessID) -> TMVar Event -> IO ()
        spawn' mpid mvar = do
            e <- atomically $ orElse (takeTMVar mvar)
                                     (takeTMVar (tWakeup t) >> return Wakeup)
            case e of
                Exit ps -> do
                    -- TODO Send signal to output process
                    case ps of
                        Just status ->
                            case status of
                                Exited ExitSuccess -> return ()
                                _                  -> (atomically $ readTMVar wants) >>= failWith
                            where
                                failWith Up   = restartDelay >> spawn' Nothing mvar
                                failWith Down = spawn' Nothing mvar
                                restartDelay  = threadDelay 1000000 -- 1 second
                        Nothing ->
                            return ()
                Wakeup -> do
                    w <- atomically $ readTMVar wants

                    case w of
                        Up | isNothing mpid -> do
                            pid <- forkProcess $ child fds
                            -- Normally, we would close the pipe descriptors (`fds`) here,
                            -- but we need to be able to pass them to subsequent child processes
                            -- as they are restarted on failure, so we leave them open.
                            forkIO $ waitForExit pid mvar
                            spawn' (Just pid) mvar
                           | otherwise ->
                            spawn' mpid mvar
                        Down ->
                            case mpid of
                                Just pid -> do
                                    signalProcess sigTERM pid
                                    spawn' Nothing mvar
                                _ ->
                                    return ()

        waitForExit :: ProcessID -> TMVar Event -> IO ()
        waitForExit pid mvar = do
            ps <- getProcessStatus True False pid
            atomically $ putTMVar mvar (Exit ps)

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
handleReq _ _ [] = return "NOP"
handleReq (inTask, outTask) wants line@(c:_) =
    case c of
        '?' -> fmap (map toLower . show) (atomically $ readTMVar wants)
        'u' -> atomically (swapTMVar wants Up   >> wakeTask inTask) >> return ok
        'd' -> atomically (swapTMVar wants Down >> wakeTask inTask) >> return ok
        'x' -> atomically (swapTMVar wants Down >> wakeTask inTask  >> wakeTask outTask) >> exitSuccess
        ___ -> return $ err (" unknown command '" ++ line ++ "'")
    where
        ok            = "OK"
        err m         = "ERROR" ++ m

recvTCP :: (Task, Task) -> Handle -> TMVar Want -> IO a
recvTCP tasks handle w = do
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
            wakeIn  <- newTMVarIO ()
            wakeOut <- newTMVarIO ()

            installHandler sigPIPE Ignore Nothing
            blockSignals $ addSignal sigCHLD emptySignalSet

            (readfd, writefd) <- createPipe

            done <- newEmptyMVar

            let fork task fds = forkFinally (spawn wants (dir cfg) task fds) (\_ -> putMVar done ()) in do

                fork (outTask wakeOut) [Just readfd, Nothing, Nothing]
                fork (inTask wakeIn)   [Nothing, Just writefd, Just writefd]

            sock <- case (port cfg) of
                Nothing -> return Nothing
                Just p  -> do
                    listenTCP (inTask wakeIn, outTask wakeOut) p wants >>= return . Just

            takeMVar done >> takeMVar done

            case sock of
                Nothing -> return ()
                Just s  -> Net.sClose s

            where
                inTask  = mkTask cfg inCmd inArgs
                outTask = mkTask cfg outCmd outArgs

