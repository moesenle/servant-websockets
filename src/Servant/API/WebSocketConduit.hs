{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RoleAnnotations          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}

module Servant.API.WebSocketConduit where

import           Conduit                             (ConduitT, MonadIO (liftIO), MonadUnliftIO (..), ResourceT, Void,
                                                      runConduitRes, runResourceT, yieldM, (.|))
import           Control.Concurrent                  (newEmptyMVar, putMVar, takeMVar)
import           Control.Concurrent.Async            (race_)
import           Control.Monad                       (forever, void, (>=>))
import           Control.Monad.Catch                 (handle)
import           Control.Monad.Trans.Control         (MonadBaseControl)
import           Data.Aeson                          (FromJSON, ToJSON, decode, encode)
import           Data.ByteString                     (ByteString, fromStrict)
import qualified Data.ByteString.Lazy                as L
import qualified Data.Conduit.List                   as CL
import           Data.Kind                           (Constraint, Type)
import           Data.Proxy                          (Proxy (..))
import           Data.String                         (IsString (..))
import           Data.Text                           (Text)
import           GHC.TypeLits                        (Symbol, symbolVal)
import           Network.Wai.Handler.WebSockets      (websocketsOr)
import           Network.WebSockets                  (AcceptRequest (acceptHeaders), Connection, ConnectionException,
                                                      PendingConnection (..), RequestHead (requestHeaders), ServerApp,
                                                      acceptRequest, acceptRequestWith, defaultAcceptRequest,
                                                      defaultConnectionOptions, receiveData, receiveDataMessage,
                                                      sendBinaryData, sendClose, sendTextData, withPingThread)
import           Servant.Server                      (HasServer (..), ServerError (..), ServerT)
import           Servant.Server.Internal.Delayed     (runDelayed)
import           Servant.Server.Internal.Router      (leafRouter)
import           Servant.Server.Internal.RouteResult (RouteResult (..))

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
type MessageType :: Type
data MessageType = JSONMessage | BinaryMessage

type role WebSocketConduitRaw nominal nominal nominal nominal
type WebSocketConduitRaw :: Maybe Symbol -> MessageType -> Type -> Type -> Type
data WebSocketConduitRaw msec mt i o

type SecWebSocketProtocol :: Symbol
type SecWebSocketProtocol = "Sec-WebSocket-Protocol"

secWebSocketProtocol :: IsString s => s
secWebSocketProtocol = fromString $ symbolVal (Proxy @SecWebSocketProtocol)

type WebSocketConduit = WebSocketConduitRaw 'Nothing 'JSONMessage
type WebSocketConduitBinary = WebSocketConduitRaw 'Nothing 'BinaryMessage
type WebSocketConduitWithSec = WebSocketConduitRaw ('Just SecWebSocketProtocol)

type AcceptsConnection :: Maybe Symbol -> Constraint
class AcceptsConnection a where acceptance :: PendingConnection -> IO Connection

instance AcceptsConnection ('Just a) where
  acceptance pending =
    let echoSec = case filter (\(hn, _) -> hn == secWebSocketProtocol)
          . requestHeaders $ pendingRequest pending of
          (_,yup):_ -> (:) (secWebSocketProtocol, yup)
          []        -> id
    in acceptRequestWith pending defaultAcceptRequest { acceptHeaders = echoSec $ acceptHeaders defaultAcceptRequest }

instance AcceptsConnection 'Nothing where
  acceptance pending = acceptRequestWith pending defaultAcceptRequest

type SocketBracket :: MessageType -> Type -> Type -> Constraint
class SocketBracket mt i o where
  socketBracket :: MonadIO m => ConduitT i o m () -> Connection -> ConduitT ByteString c m ()

instance (FromJSON i, ToJSON o) => SocketBracket 'JSONMessage i o where
  socketBracket cond c =
    CL.mapMaybe (decode . fromStrict) .| cond .| CL.mapM_ (liftIO . sendTextData c . encode)

instance SocketBracket 'BinaryMessage L.ByteString L.ByteString where
  socketBracket cond c =
    CL.map fromStrict .| cond .| CL.mapM_ (liftIO . sendBinaryData c)

instance (FromJSON i, ToJSON o, AcceptsConnection msec, SocketBracket mt i o) => HasServer (WebSocketConduitRaw msec mt i o) ctx where

  type ServerT (WebSocketConduitRaw msec mt i o) m = ConduitT i o (ResourceT IO) ()

#if MIN_VERSION_servant_server(0,12,0)
  hoistServerWithContext _ _ _ svr = svr
#endif

  route Proxy _ app = leafRouter $ \env request respond -> runResourceT $
    runDelayed app env request >>= liftIO . go request respond
   where
    go request respond (Route cond) = websocketsOr
      defaultConnectionOptions
      (runWSApp cond :: ServerApp)
      (\_ _ -> respond $ Fail upgradeRequired) request (respond . Route)

    go _ respond (Fail e) = respond $ Fail e
    go _ respond (FailFatal e) = respond $ FailFatal e

    runWSApp cond pending = do
      c <- acceptance @msec pending
      handle (\(_ :: ConnectionException) -> return ()) $ do
        i <- newEmptyMVar
        race_ (forever $ receiveData c >>= putMVar i) .
          runConduitWebSocket c $ forever (yieldM . liftIO $ takeMVar i)
            .| socketBracket @mt cond c

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
runConduitWebSocket c a = withRunInIO $ \run -> withPingThread c 10 (pure ()) $ do
  run . void $ runConduitRes a
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
