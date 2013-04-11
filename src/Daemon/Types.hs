{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Aeson
import           Control.Applicative
import           Control.Monad
import           Data.Map (Map)

data Cmd = Cmd
    { cmdName :: String
    , cmdArgs :: [String]
    , cmdEnv  :: Maybe (Map String String)
    }

instance FromJSON Cmd where
    parseJSON (Object v) =
        Cmd <$>
            v .:  "cmd"  <*>
            v .:  "args" <*>
            v .:? "env"
    parseJSON _ = mzero

data Job = Job
    { _worker       :: Cmd
    , _logger       :: Cmd
    , _maxFailures  :: Maybe Int
    , _restartDelay :: Maybe Int
    , _port         :: Maybe Int
    , _cwd          :: Maybe FilePath
    }

instance FromJSON Job where
    parseJSON (Object v) =
        Job <$>
            v .:  "worker"        <*>
            v .:  "logger"        <*>
            v .:? "max-failures"  <*>
            v .:? "restart-delay" <*>
            v .:? "port"          <*>
            v .:? "cwd"
    parseJSON _ = mzero
