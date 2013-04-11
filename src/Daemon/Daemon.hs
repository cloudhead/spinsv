{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.HTTP.Types
import           System.Process (createProcess, proc, close_fds, cwd)
import           System.Console.GetOpt
import           System.Environment
import           Blaze.ByteString.Builder
import           Control.Exception hiding (handle)
import           Control.Monad.IO.Class (liftIO)
import           Data.Conduit
import           Data.Aeson
import           Data.Text (Text)
import           Data.Maybe (catMaybes)
import qualified Data.ByteString.Char8 as  BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Conduit.List as CL
import           Types

data Result = Ok | Err String

data Config = Config
    { cPort    :: Int
    , cRunner  :: String
    }

defaultConfig :: Config
defaultConfig = Config
    { cPort    = 8080
    , cRunner  = "spinsv"
    }

serve :: Config -> Request -> IO Response
serve cfg Request{..} =
    case parseMethod requestMethod of
        Left _  -> return $ response status400 (jsonError "bad request method")
        Right m -> do
            body <- runResourceT (requestBody $$ CL.consume)
            handleReq body `catch` handleException

            where
                handleException :: SomeException -> IO Response
                handleException e = do
                    putStrLn $ unwords ["ERR", show e]
                    return $ response status500 (BS.pack $ show e)
                handleReq body = handle m pathInfo (LBS.fromChunks body) cfg

runJob :: Job -> Config -> IO Main.Result
runJob Job{..} Config{..} = do
    putStrLn $ unwords ["JOB", cRunner, unwords args]
    createProcess (proc cRunner args){ close_fds = True, cwd = _cwd }
    return Ok

    where
        args = (inCmd : inArgs)
            ++ (outCmd : outArgs)
            ++ catMaybes [ fmap (\x -> "--port=" ++ show x) _port
                         , fmap (\x -> "--max-restarts=" ++ show x) _maxFailures
                         , fmap (\x -> "--restart-delay=" ++ show x) _restartDelay
                         ]

        inCmd   = "--in.cmd=" ++ cmdName _worker
        outCmd  = "--out.cmd=" ++ cmdName _logger
        inArgs  = map (\x -> "--in.arg="  ++ x) (cmdArgs _worker)
        outArgs = map (\x -> "--out.arg=" ++ x) (cmdArgs _logger)
    
handle :: StdMethod -> [Text] -> LBS.ByteString -> Config -> IO Response
handle POST ["jobs"] body cfg =
    case decode body of
        Just b ->
            runJob b cfg >>= respond
        Nothing ->
            return $ response status400 $ jsonError "JSON object is invalid"
    where
        respond (Ok)        = return $ response status201 BS.empty
        respond (Err str)   = return $ response status400 (jsonError $ BS.pack str)

handle _ _ _ _ =
    return $ response status404 $ jsonError "path not found"

jsonError :: BS.ByteString -> BS.ByteString
jsonError err =
    BS.concat ["{\"error\":\"", err, "\"}\n"]

response :: Status -> BS.ByteString -> Response
response status bytes | BS.length bytes > 0 =
    ResponseBuilder status headers (copyByteString bytes)

    where
        headers = [ ("Content-Type", "application/json")
                  , ("Content-Length", BS.pack . show $ BS.length bytes) ]

response status bytes =
    ResponseBuilder status [] (copyByteString bytes)

options :: [OptDescr (Config -> Config)]
options =
    [ Option [] ["port"]
        (ReqArg (\o cfg -> cfg{cPort = read o})   "<port>")  "listen port (default: 8080)"
    , Option [] ["runner"]
        (ReqArg (\o cfg -> cfg{cRunner = o})      "<cmd>")   "runner command (default: spinsv)"
    ]

getCmd :: IO Config
getCmd = do
    a <- getArgs

    case getOpt RequireOrder options a of
        (flags, [], []) ->
            return $ foldl (\def t -> t def) defaultConfig flags
        (_, nonOpts, []) ->
            error $ "unrecognized arguments: " ++ unwords nonOpts
        (_, _, msgs) ->
            error $ head msgs

main :: IO ()
main =
    getCmd >>= execute

    where
        execute cfg = run (cPort cfg) $ liftIO . serve cfg

