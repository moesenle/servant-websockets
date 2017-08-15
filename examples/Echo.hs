{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Servant.API.WebSocketConduit (WebSocketConduit)

import Data.Aeson               (Value)
import Data.Conduit             (Conduit)
import Data.Monoid              ((<>))
import Network.Wai              (Application)
import Network.Wai.Handler.Warp (run)
import Servant                  (Proxy (..), Server, serve)

import qualified Data.Conduit.List as CL


type API = WebSocketConduit Value Value

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

echo :: Monad m => Conduit Value m Value
echo = CL.map id

main :: IO ()
main = startApp
