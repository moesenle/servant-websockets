{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Servant.API.WebSocket (WebSocket)

import Control.Concurrent       (threadDelay)
import Control.Monad.IO.Class   (MonadIO, liftIO)
import Data.Foldable            (forM_)
import Data.Text                (pack)
import Network.Wai              (Application)
import Network.Wai.Handler.Warp (run)
import Network.WebSockets       (Connection, forkPingThread, sendTextData, withPingThread)
import Servant                  (Proxy (..), Server, serve, (:>))

type WebSocketApi = "stream" :> WebSocket

startApp :: IO ()
startApp = do
  putStrLn "Starting server on http://localhost:8080"
  run 8080 app

app :: Application
app = serve api server

api :: Proxy WebSocketApi
api = Proxy

server :: Server WebSocketApi
server = streamData
 where
   streamData :: MonadIO m => Connection -> m ()
   streamData c = liftIO . forM_ [1..] $ \i -> do
     withPingThread c 10 (pure ()) $ sendTextData c (pack $ show (i :: Int)) >> threadDelay 1000000

main :: IO ()
main = startApp
