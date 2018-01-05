{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Servant.API.WebSocket where

import Control.Monad                              (void, (>=>))
import Control.Monad.IO.Class                     (liftIO)
import Control.Monad.Trans.Resource               (runResourceT)
import Data.Proxy                                 (Proxy (..))
import Network.Wai.Handler.WebSockets             (websocketsOr)
import Network.WebSockets                         (Connection, PendingConnection, acceptRequest, defaultConnectionOptions)
import Servant.Server                             (HasServer (..), ServantErr (..), ServerT, runHandler)
import Servant.Server.Internal.Router             (leafRouter)
import Servant.Server.Internal.RoutingApplication (RouteResult (..), runDelayed)

-- | Endpoint for defining a route to provide a web socket. The
-- handler function gets an already negotiated websocket 'Connection'
-- to send and receive data.
--
-- Example:
--
-- > type WebSocketApi = "stream" :> WebSocket
-- >
-- > server :: Server WebSocketApi
-- > server = streamData
-- >  where
-- >   streamData :: MonadIO m => Connection -> m ()
-- >   streamData c = liftIO . forM_ [1..] $ \i -> do
-- >     forkPingThread c 10
-- >     sendTextData c (pack $ show (i :: Int)) >> threadDelay 1000000
data WebSocket

instance HasServer WebSocket ctx where

  type ServerT WebSocket m = Connection -> m ()

  #if MIN_VERSION_servant_server(0,12,0)
  hoistServerWithContext _ _ nat svr = nat . svr
  #endif

  route Proxy _ app = leafRouter $ \env request respond -> runResourceT $
    runDelayed app env request >>= liftIO . go request respond
   where
    go request respond (Route app') =
      websocketsOr defaultConnectionOptions (runApp app') (backupApp respond) request (respond . Route)
    go _ respond (Fail e) = respond $ Fail e
    go _ respond (FailFatal e) = respond $ FailFatal e

    runApp a = acceptRequest >=> \c -> void (runHandler $ a c)

    backupApp respond _ _ = respond $ Fail ServantErr { errHTTPCode = 426
                                                      , errReasonPhrase = "Upgrade Required"
                                                      , errBody = mempty
                                                      , errHeaders = mempty
                                                      }


-- | Endpoint for defining a route to provide a web socket. The
-- handler function gets a 'PendingConnection'. It can either
-- 'rejectConnection' or 'acceptRequest'. This function is provided
-- for greater flexibility to reject connections.
--
-- Example:
--
-- > type WebSocketApi = "stream" :> WebSocket
-- >
-- > server :: Server WebSocketApi
-- > server = streamData
-- >  where
-- >   streamData :: MonadIO m => Connection -> m ()
-- >   streamData c = liftIO . forM_ [1..] $ \i -> do
-- >     forkPingThread c 10
-- >     sendTextData c (pack $ show (i :: Int)) >> threadDelay 1000000
data WebSocketPending

instance HasServer WebSocketPending ctx where

  type ServerT WebSocketPending m = PendingConnection -> m ()

  #if MIN_VERSION_servant_server(0,12,0)
  hoistServerWithContext _ _ nat svr = nat . svr
  #endif

  route Proxy _ app = leafRouter $ \env request respond -> runResourceT $
    runDelayed app env request >>= liftIO . go request respond
   where
    go request respond (Route app') =
      websocketsOr defaultConnectionOptions (runApp app') (backupApp respond) request (respond . Route)
    go _ respond (Fail e) = respond $ Fail e
    go _ respond (FailFatal e) = respond $ FailFatal e

    runApp a c = void (runHandler $ a c)

    backupApp respond _ _ = respond $ Fail ServantErr { errHTTPCode = 426
                                                      , errReasonPhrase = "Upgrade Required"
                                                      , errBody = mempty
                                                      , errHeaders = mempty
                                                      }
