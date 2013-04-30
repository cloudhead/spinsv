{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (mapM)
import Network (sClose, PortID(..))
import System.Environment (getArgs, getProgName)
import System.Console.GetOpt
import System.Exit
import System.Posix.Signals (signalProcess, sigTERM)
import System.Posix.Process (forkProcess, executeFile)
import System.Posix (Fd)
import System.Posix.IO
import Data.Traversable (mapM)
import Data.Maybe
import Control.Concurrent
import Control.Exception
import Control.Monad hiding (mapM)
import qualified System.Posix.Process

import Runner

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
        (ReqArg (\o cfg -> cfg{port = Just $ readPort o})     "<port>") "port to bind to (optional)"
    , Option [] ["id"]
        (ReqArg (\o cfg -> cfg{ident = Just o})                 "<id>") "bind to an identifier (optional)"
    , Option [] ["restart-delay"]
        (ReqArg (\o cfg -> cfg{delay = Just $ read o})          "<ms>") "restart delay in milliseconds (optional)"
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
    , Option [] ["raw"]
        (NoArg  (\cfg   -> cfg{rawMode = True}))                        "don't show prompt in console (on)"
    , Option [] ["help"]
        (NoArg  (\cfg   -> cfg{help = True}))                           "print the help and exit"
    , Option [] ["version"]
        (NoArg  (\cfg   -> cfg{version = True}))                        "print the version and exit"
    ] where
        readPort = PortNumber . fromInt . read
        fromInt = fromIntegral :: (Num b) => Int -> b

helpString :: String -> String
helpString prog =
    usageInfo header options
    where
        header = unlines [ concat [ "usage: ", prog, " [<option>...]"]
                         ,"\nstart and monitor a service and its appendant log service\n"
                         , "options:"
                         ]

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

child :: Cmd -> [(String, String)] -> [Maybe Fd] -> IO ()
child (cmd, args) env fds' = do
    sequence_ $ zipWith maybeDup fds' [stdInput, stdOutput, stdError]
             ++ map closeFd' (catMaybes fds')

    executeFile cmd True args (Just env)

    where
        maybeDup (Just fd) std = void $ dupTo fd std
        maybeDup Nothing   _   = return ()
        closeFd' fd            = catch (closeFd fd) (\(_ :: IOException) -> return ())

runner :: Config -> IO ()
runner cfg = do
    exitVar <- newEmptyMVar
    maybeSock <- run cfg System
        { spawnProcess     = \c a f -> forkProcess $ child c a f -- cmd args fds
        , killProcess      = signalProcess sigTERM
        , getProcessStatus = System.Posix.Process.getProcessStatus
        , childExited      = chldVar
        , exit             = putMVar exitVar
        }

    code <- takeMVar exitVar -- blocks until a thread calls `exit`
    mapM sClose maybeSock
    exitWith code

main :: IO ()
main =
    getCmd >>= execute

    where
        execute (cfg, n) | version cfg = putStrLn (unwords [n, "version", versionString]) >> exitSuccess
        execute (cfg, n) | help    cfg = putStrLn (helpString n) >> exitSuccess
        execute (cfg, _)               = runner cfg

