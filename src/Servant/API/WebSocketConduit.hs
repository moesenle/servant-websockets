{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Servant.API.WebSocketConduit where

import Control.Concurrent                         (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async                   (race_)
import Control.Monad                              (forever, void, (>=>))
import Control.Monad.Catch                        (handle)
import Control.Monad.IO.Class                     (liftIO)
import Control.Monad.Trans.Control                (MonadBaseControl)
import Control.Monad.Trans.Resource               (MonadUnliftIO, ResourceT, runResourceT)
import Data.Aeson                                 (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy                       (fromStrict)
import Data.Conduit                               (ConduitT, runConduitRes, yieldM, (.|))
import Data.Proxy                                 (Proxy (..))
import Data.Text                                  (Text)
import Data.Void                                  (Void)
import Network.Wai.Handler.WebSockets             (websocketsOr)
import Network.WebSockets                         (Connection, ConnectionException, acceptRequest,
                                                   defaultConnectionOptions, forkPingThread, receiveData,
                                                   receiveDataMessage, sendClose, sendTextData)
import Servant.Server                             (HasServer (..), ServerError (..), ServerT)
import Servant.Server.Internal.Router             (leafRouter)
import Servant.Server.Internal.RouteResult        (RouteResult (..))
import Servant.Server.Internal.Delayed            (runDelayed)

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
-- >   echo :: Monad m => ConduitT Value Value m ()
-- >   echo = CL.map id
-- >
--
-- Note that the input format on the web socket is JSON, hence this
-- example only echos valid JSON data.
data WebSocketConduit i o

instance (FromJSON i, ToJSON o) => HasServer (WebSocketConduit i o) ctx where

  type ServerT (WebSocketConduit i o) m = ConduitT i o (ResourceT IO) ()

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
      i <- newEmptyMVar
      race_ (forever $ receiveData c >>= putMVar i) $
        runConduitWebSocket c $
          forever (yieldM . liftIO $ takeMVar i)
          .| CL.mapMaybe (decode . fromStrict)
          .| cond
          .| CL.mapM_ (liftIO . sendTextData c . encode)

-- | Endpoint for defining a route to provide a websocket. In contrast
-- to the 'WebSocketConduit', this endpoint only produces data. It can
-- be useful when implementing web sockets that simply just send data
-- to clients.
--
-- Example:
--
-- > import Data.Text (Text)
-- > import qualified Data.Conduit.List as CL
-- >
-- > type WebSocketApi = "hello" :> WebSocketSource Text
-- >
-- > server :: Server WebSocketApi
-- > server = hello
-- >  where
-- >   hello :: Monad m => Conduit Text m ()
-- >   hello = yield $ Just "hello"
-- >
--
data WebSocketSource o

instance ToJSON o => HasServer (WebSocketSource o) ctx where

  type ServerT (WebSocketSource o) m = ConduitT () o (ResourceT IO) ()

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

    runWSApp cond = acceptRequest >=> \c -> handle (\(_ :: ConnectionException) -> return ()) $
      race_ (forever . void $ (receiveData c :: IO Text)) $
        runConduitWebSocket c $ cond .| CL.mapM_ (liftIO . sendTextData c . encode)

runConduitWebSocket :: (MonadBaseControl IO m, MonadUnliftIO m) => Connection -> ConduitT () Void (ResourceT m) () -> m ()
runConduitWebSocket c a = do
  liftIO $ forkPingThread c 10
  void $ runConduitRes a
  liftIO $ do
    sendClose c ("Out of data" :: Text)
    -- After sending the close message, we keep receiving packages
    -- (and drop them) until the connection is actually closed,
    -- which is indicated by an exception.
    forever $ receiveDataMessage c

upgradeRequired :: ServerError
upgradeRequired = ServerError { errHTTPCode = 426
                              , errReasonPhrase = "Upgrade Required"
                              , errBody = mempty
                              , errHeaders = mempty
                              }
