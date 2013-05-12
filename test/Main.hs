{-# LANGUAGE RecordWildCards #-}

module Main where

import           Prelude hiding (mapM)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test)
import           Control.Monad hiding (mapM)
import           Network
import           Runner
import           Helper
import qualified Data.Map as Map

testDownFlag :: PortNumber -> IO ()
testDownFlag p = withRunner config{want = Down} p $ \env -> do
    runner <- connect (PortNumber p)
    (want, _, _) <- getRunnerStatus runner
    assertEqual "status should be 'down'" Down want

    procs <- getProcesses env
    assertEqual "there should be no only one process running" 1 (Map.size procs)

testProcessStartAndRestartOnFail :: PortNumber -> IO ()
testProcessStartAndRestartOnFail p = withRunner config p $ \env -> do
    p  <- waitForNewProcess env
    _  <- waitForNewProcess env

    failProcess env p >> waitForProcessExit env p

    p' <- waitForNewProcess env

    assertEqual "process commands should be equal" (pCmd p) (pCmd p')

    return ()

testConsoleUpDown :: PortNumber -> IO ()
testConsoleUpDown p = withRunner config p $ \env -> do

    p1 <- waitForNewProcess env
    p2 <- waitForNewProcess env

    let
        (worker, logger) =
            if fst (pCmd p1) == "worker" then (p1, p2) else (p2, p1)

    runner <- connect (PortNumber p)
    (want, _, _) <- getRunnerStatus runner
    assertEqual "status should be 'up'" Up want

    r <- runnerDown runner
    assertEqual "response should be 'OK'" "OK" r

    (want, _, _) <- getRunnerStatus runner
    assertEqual "status should be 'down'" Down want

    waitForProcessExit env worker

    r <- runnerUp runner
    assertEqual "response should be 'OK'" "OK" r

    (want, _, _) <- getRunnerStatus runner
    assertEqual "status should be 'up'" Up want

    p3 <- waitForNewProcess env
    assertEqual "process commands are equal" (pCmd worker) (pCmd p3)

    unless (pID p3 /= pID worker) $ assertFailure "process pids shouldn't match"

    status <- getProcessStatus' env False True (pID logger)

    case status of
        Nothing -> return ()
        _       -> assertFailure "logger process has exited"

testConsoleStatus :: PortNumber -> IO ()
testConsoleStatus p = withRunner config p $ \env -> do
    p1 <- waitForNewProcess env
    p2 <- waitForNewProcess env

    let
        (worker, logger) =
            if fst (pCmd p1) == "worker" then (p1, p2) else (p2, p1)

    runner <- connect (PortNumber p)
    (want, inRestarts, outRestarts) <- getRunnerStatus runner

    assertEqual "worker hasn't been restarted" 0 inRestarts
    assertEqual "logger hasn't been restarted" 0 outRestarts
    assertEqual "status should be 'up'" Up want

    failProcess env worker >> waitForProcessExit env worker

    (_, inRestarts, outRestarts) <- getRunnerStatus runner

    assertEqual "worker has been restarted once" 1 inRestarts
    assertEqual "logger hasn't been restarted" 0 outRestarts

    failProcess env logger >> waitForProcessExit env logger

    (_, inRestarts, outRestarts) <- getRunnerStatus runner

    assertEqual "worker has been restarted once" 1 inRestarts
    assertEqual "logger has been restarted once" 1 outRestarts

main :: IO ()
main =
    defaultMain [ testCase "Console 'status' command" (testConsoleStatus 9999)
                , testCase "Console 'up' and 'down' commands" (testConsoleUpDown 9998)
                , testCase "Process start and restart on fail" (testProcessStartAndRestartOnFail 9997)
                , testCase "'--down' flag" (testDownFlag 9999)
                ]
