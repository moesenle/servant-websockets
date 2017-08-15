{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Servant.API.WebSocketConduit (WebSocketConduit)

import Data.Conduit             (Conduit)
import Data.Monoid              ((<>))
import Data.Text                (Text)
import Network.Wai              (Application)
import Network.Wai.Handler.Warp (run)
import Servant                  (Proxy (..), Server, serve)

import qualified Data.Conduit.List as CL


type API = WebSocketConduit Text Text

startApp :: IO ()
startApp = do
  putStrLn "Starting server on http://localhost:8080"
  run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = echo

echo :: Monad m => Conduit Text m Text
echo = CL.map $ \t -> "received " <> t

main :: IO ()
main = startApp
