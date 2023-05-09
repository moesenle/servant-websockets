{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Servant.API.WebSocketConduit (WebSocketConduit, WebSocketSource)

import Control.Concurrent       (threadDelay)
import Control.Monad            (forever)
import Control.Monad.IO.Class   (MonadIO (..))
import Data.Aeson               (Value (..))
import Data.Conduit             (ConduitT, yield)
import Data.Text                (Text)
import Network.Wai              (Application)
import Network.Wai.Handler.Warp (run)
import Servant                  ((:<|>) (..), (:>), Proxy (..), Server, serve)

import qualified Data.Conduit.List as CL


type API = "echo" :> WebSocketConduit Text Text
           :<|> "hello" :> WebSocketSource Text

startApp :: IO ()
startApp = do
  putStrLn "Starting server on http://localhost:8080"
  run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = echo :<|> hello

echo :: Monad m => ConduitT Text Text m ()
echo = CL.map id

hello :: MonadIO m => ConduitT () Text m ()
hello = forever $ do
  yield "hello world"
  liftIO $ threadDelay 1000000

main :: IO ()
main = startApp
