{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Servant.API.WebSocketConduit where

import Control.Concurrent                         (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async                   (race_)
import Control.Monad                              (forever, (>=>))
import Control.Monad.Catch                        (handle)
import Control.Monad.IO.Class                     (liftIO)
import Control.Monad.Trans.Resource               (ResourceT, runResourceT)
import Data.Aeson                                 (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy                       (fromStrict)
import Data.Conduit                               (Conduit, runConduitRes, yieldM, (.|))
import Data.Proxy                                 (Proxy (..))
import Data.Text                                  (Text)
import Network.Wai.Handler.WebSockets             (websocketsOr)
import Network.WebSockets                         (ConnectionException, acceptRequest, defaultConnectionOptions,
                                                   forkPingThread, receiveData, receiveDataMessage, sendClose,
                                                   sendTextData)
import Servant.Server                             (HasServer (..), ServantErr (..), ServerT)
import Servant.Server.Internal.Router             (leafRouter)
import Servant.Server.Internal.RoutingApplication (RouteResult (..), runDelayed)

import qualified Data.Conduit.List as CL

-- | Endpoint for defining a route to provide a websocket. In contrast
-- to the 'WebSocket' endpoint, 'WebSocketConduit' provides a
-- higher-level interface. The handler function must be of type
-- @Conduit i m o@ with @i@ and @o@ being instances of 'FromJSON' and
-- 'ToJSON' respectively. 'await' reads from the web socket while
-- 'yield' writes to it.
--
-- Example:
--
-- >
-- > import Data.Aeson (Value)
-- > import qualified Data.Conduit.List as CL
-- >
-- > type WebSocketApi = "echo" :> WebSocketConduit Value Value
-- >
-- > server :: Server WebSocketApi
-- > server = echo
-- >  where
-- >   echo :: Monad m => Conduit Value m Value
-- >   echo = CL.map id
-- >
--
-- Note that the input format on the web socket is JSON, hence this
-- example only echos valid JSON data.
data WebSocketConduit i o

instance (FromJSON i, ToJSON o) => HasServer (WebSocketConduit i o) ctx where

  type ServerT (WebSocketConduit i o) m = Conduit i (ResourceT IO) o

#if MIN_VERSION_servant_server(0,12,0)
  hoistServerWithContext _ _ _ svr = svr
#endif

  route Proxy _ app = leafRouter $ \env request respond -> runResourceT $
    runDelayed app env request >>= liftIO . go request respond
   where
    go request respond (Route cond) =
      websocketsOr
        defaultConnectionOptions
        (runWSApp cond)
        (\_ _ -> respond $ Fail upgradeRequired)
        request (respond . Route)
    go _ respond (Fail e) = respond $ Fail e
    go _ respond (FailFatal e) = respond $ FailFatal e

    runWSApp cond = acceptRequest >=> \c -> handle (\(_ :: ConnectionException) -> return ()) $ do
      forkPingThread c 10
      i <- newEmptyMVar
      race_ (forever $ receiveData c >>= putMVar i) $ do
        runConduitRes $ forever (yieldM . liftIO $ takeMVar i)
                     .| CL.mapMaybe (decode . fromStrict)
                     .| cond
                     .| CL.mapM_ (liftIO . sendTextData c . encode)
        sendClose c ("Out of data" :: Text)
        -- After sending the close message, we keep receiving packages
        -- (and drop them) until the connection is actually closed,
        -- which is indicated by an exception.
        forever $ receiveDataMessage c

data WebSocketSource o

instance ToJSON o => HasServer (WebSocketSource o) ctx where

  type ServerT (WebSocketSource o) m = Conduit () (ResourceT IO) o

#if MIN_VERSION_servant_server(0,12,0)
  hoistServerWithContext _ _ _ svr = svr
#endif

  route Proxy _ app = leafRouter $ \env request respond -> runResourceT $
    runDelayed app env request >>= liftIO . go request respond
   where
    go request respond (Route cond) =
      websocketsOr
        defaultConnectionOptions
        (runWSApp cond)
        (\_ _ -> respond $ Fail upgradeRequired)
        request (respond . Route)
    go _ respond (Fail e) = respond $ Fail e
    go _ respond (FailFatal e) = respond $ FailFatal e

    runWSApp cond = acceptRequest >=> \c -> handle (\(_ :: ConnectionException) -> return ()) $ do
      forkPingThread c 10
      runConduitRes $ cond .| CL.mapM_ (liftIO . sendTextData c . encode)
      sendClose c ("Out of data" :: Text)
      -- After sending the close message, we keep receiving packages
      -- (and drop them) until the connection is actually closed,
      -- which is indicated by an exception.
      forever $ receiveDataMessage c

upgradeRequired :: ServantErr
upgradeRequired = ServantErr { errHTTPCode = 426
                             , errReasonPhrase = "Upgrade Required"
                             , errBody = mempty
                             , errHeaders = mempty
                             }
