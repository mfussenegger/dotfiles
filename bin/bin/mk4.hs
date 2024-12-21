#!/usr/bin/env stack
{- stack script --optimize --resolver lts-23.1
 --package "bytestring aeson http-client http-client-tls process"
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

import Data.Aeson ( FromJSON, decode, eitherDecode)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (applyDigestAuth)
import System.Environment (getEnv)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics
import Text.Printf (printf)
import Control.Exception (try, handle)
import Data.Monoid (Any)
import Control.Exception.Base (Exception)
import System.Process (callProcess)


data State
  = FINISHED
  | IDLE
  | PRINTING
  | ATTENTION
  | BUSY
  deriving (Eq, Show, Generic, FromJSON)


data Printer = Printer
  { state :: State,
    temp_nozzle :: Float
  }
  deriving (Eq, Show, Generic, FromJSON)


data Job = Job
  { id :: Int,
    progress :: Float
  }
  deriving (Eq, Show, Generic, FromJSON)


data StatusResult = StatusResult
  { printer :: Printer,
    job :: Maybe Job
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON)


showResponse :: Either HttpException (Response BL.ByteString) -> IO ()
showResponse (Left _) = pure ()
showResponse (Right resp) = do
  let state = eitherDecode @StatusResult $ responseBody resp
  case state of
    Left err -> print err
    Right (StatusResult {printer = Printer { state = BUSY }}) -> do
      printf "🖨️ Busy"
    Right (StatusResult {printer = Printer { state = ATTENTION }}) -> do
      callProcess "swaymsg" ["bar", "mode", "dock"]
      printf "🖨️ ‼️"
    Right (StatusResult {job = Just Job {progress}, printer = Printer {state = PRINTING, temp_nozzle}}) ->
      printf "🖨️ %2.0f%% %3.1f 🌡️" progress temp_nozzle
    _ -> pure ()


main :: IO ()
main = do
  mk4Token <- BS.pack <$> getEnv "MK4_TOKEN"
  manager <- newManager defaultManagerSettings
  req <- parseRequest url
  try $ do
    Just req' <- applyDigestAuth "maker" mk4Token req { method = "GET" } manager
    httpLbs req' manager
  >>= showResponse
  where
    url = "http://prusamk4.home.lan/api/v1/status"
