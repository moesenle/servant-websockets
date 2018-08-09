{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Servant.API.WebSocketConduit (WebSocketConduit, WebSocketSource)

import Control.Concurrent       (threadDelay)
import Control.Monad.IO.Class   (MonadIO (..))
import Data.Aeson               (Value (..))
import Data.Conduit             (Conduit)
import Data.Conduit             (yield)
import Data.Monoid              ((<>))
import Network.Wai              (Application)
import Network.Wai.Handler.Warp (run)
import Servant                  ((:<|>) (..), (:>), Proxy (..), Server, serve)

import qualified Data.Conduit.List as CL


type API = "echo" :> WebSocketConduit Value Value
           :<|> "hello" :> WebSocketSource Value

startApp :: IO ()
startApp = do
  putStrLn "Starting server on http://localhost:8080"
  run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = echo :<|> helloWorld

echo :: Monad m => Conduit Value m Value
echo = CL.map id

helloWorld :: MonadIO m => Conduit () m Value
helloWorld = do
  yield (String "hello world")
  liftIO $ threadDelay 1000000
  helloWorld

main :: IO ()
main = startApp
