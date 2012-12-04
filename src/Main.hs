module Main where

import System.Process
import System.Posix.IO
import System.Posix (Fd)
import System.Posix.Signals
import System.Exit
import System.Environment
import System.Console.GetOpt
import Control.Concurrent
import System.IO hiding (stdin, stdout, stderr)
import Data.Char
import qualified Data.Traversable as T
import qualified Network as Net

data Want = Up | Down deriving (Show)

data Config = Config
    { inCmd   :: String
    , outCmd  :: String
    , inArgs  :: [String]
    , outArgs :: [String]
    , port    :: Maybe Int
    , dir     :: String
    , help    :: Bool
    , version :: Bool
    } deriving Show

versionString :: String
versionString = "0.0.0"

tee :: String
tee = "tee"

defaultConfig :: Config
defaultConfig = Config
    { inCmd   = tee
    , outCmd  = tee
    , inArgs  = []
    , outArgs = []
    , port    = Nothing
    , dir     = "."
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
    , Option [] ["dir"]
        (ReqArg (\o cfg -> cfg{dir = o})                      "<dir>")  "directory to run in (.)"
    , Option [] ["help"]
        (NoArg  (\cfg   -> cfg{help = True}))                           "print the help and exit"
    , Option [] ["version"]
        (NoArg  (\cfg   -> cfg{version = True}))                        "print the version and exit"
    ]

spawn :: MVar () -> MVar Want -> String -> String -> [String] -> [Maybe Fd] -> IO ()
spawn done wants wd cmd args fds = do
    [stdin, stdout, stderr] <- T.mapM maybeFdToHandle fds

    p <- runProcess
        cmd args
        (Just wd)
        Nothing
        stdin
        stdout
        stderr

    w <- waitForProcess p

    case w of
        ExitSuccess   -> return ()
        ExitFailure _ -> do
            want <- readMVar wants

            case want of
                Up ->   -- restart
                    spawn done wants wd cmd args fds
                Down -> -- exit
                    putMVar done () >> return ()
    where
        maybeFdToHandle :: (Maybe Fd) -> IO (Maybe Handle)
        maybeFdToHandle fd = T.mapM (\x -> dup x >>= fdToHandle) fd

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

handleReq :: MVar Want -> String -> IO String
handleReq wants line =
    case line of
        "?" -> fmap (map toLower . show) (readMVar wants)
        "u" -> putMVar wants Up   >> {- TODO start service if stopped -} return ok
        "d" -> putMVar wants Down >> {- TODO send SIGTERM to service -}  return ok
        "x" -> {- TODO kill child services -} exitSuccess
        cmd -> return $ err (" unknown command '" ++ cmd ++ "'")
    where
        ok    = "OK"
        err m = "ERROR" ++ m

recvTCP :: (Handle, MVar Want) -> IO a
recvTCP ctx@(handle, w) =
    hGetLine handle >>= handleReq w >>= hPutStrLn handle >> recvTCP ctx
    -- Consider using hGetChar

acceptTCP :: Net.Socket -> MVar Want -> IO a
acceptTCP s w = do
    (handle, _, _) <- Net.accept s
    hSetBuffering handle NoBuffering
    forkIO $ recvTCP (handle, w)
    acceptTCP s w

listenTCP :: Int -> MVar Want -> IO Net.Socket
listenTCP p wants = do
    sock <- Net.listenOn $ Net.PortNumber $ fromIntegral p
    forkIO $ acceptTCP sock wants
    return sock

main :: IO ()
main =
    getCmd >>= execute

    where
        execute (cfg, _) | version cfg = putStrLn versionString  >> exitSuccess
        execute (cfg, n) | help    cfg = putStrLn (helpString n) >> exitSuccess
        execute (cfg, _) = do
            done <- newEmptyMVar
            wants <- newMVar Up

            installHandler sigPIPE Ignore Nothing
            blockSignals $ addSignal sigCHLD emptySignalSet

            (readfd, writefd) <- createPipe

            forkIO $ spawn done wants (dir cfg) (outCmd cfg) (outArgs cfg) [(Just readfd), Nothing, Nothing]
            forkIO $ spawn done wants (dir cfg) (inCmd cfg) (inArgs cfg) [Nothing, (Just writefd), (Just writefd)]
            
            sock <- case (port cfg) of
                Nothing -> return Nothing
                Just p  -> return (Just $ listenTCP p wants)

            takeMVar done >> takeMVar done

            case sock of
                Nothing -> return ()
                Just s  -> s >>= Net.sClose
            

