module HMem.Server.WebSocket
  ( -- * State
    WSState
  , newWSState
  , connectionCount
    -- * Broadcast
  , broadcast
    -- * WAI integration
  , wsMiddleware
  ) where

import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, modifyTVar')
import Control.Concurrent.STM qualified as STM
import Control.Exception (SomeException, catch, finally)
import Data.Aeson (encode)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Network.HTTP.Types.URI (parseQuery)
import Network.Wai qualified as Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS

import HMem.Config (AuthConfig(..), AuthMode(..), LocalAuthConfig(..), LocalBotTokenConfig(..), authStaticBearerEnabled, authStaticBearerToken)
import HMem.Server.Event (ChangeEvent)

-- | Server-wide WebSocket state: a map of connection IDs to live
-- WebSocket connections, protected by a TVar for concurrent access.
data WSState = WSState
  { connections :: !(TVar (Map UUID WS.Connection))
  }

-- | Create a fresh (empty) WebSocket state.
newWSState :: IO WSState
newWSState = WSState <$> newTVarIO Map.empty

-- | Number of currently connected clients.
connectionCount :: WSState -> IO Int
connectionCount st = Map.size <$> readTVarIO st.connections

------------------------------------------------------------------------
-- Connection management
------------------------------------------------------------------------

addConnection :: WSState -> WS.Connection -> IO UUID
addConnection st conn = do
  connId <- UUID.nextRandom
  STM.atomically $ modifyTVar' st.connections (Map.insert connId conn)
  pure connId

removeConnection :: WSState -> UUID -> IO ()
removeConnection st connId =
  STM.atomically $ modifyTVar' st.connections (Map.delete connId)

------------------------------------------------------------------------
-- Broadcast
------------------------------------------------------------------------

-- | Send a change event to every connected WebSocket client.
-- Individual send failures (e.g. broken pipe) are silently ignored
-- so that one bad connection does not block the others.
broadcast :: WSState -> ChangeEvent -> IO ()
broadcast st event = do
  let msg = encode event
  conns <- readTVarIO st.connections
  mapM_ (trySend msg) (Map.elems conns)
  where
    trySend msg conn =
      WS.sendTextData conn msg
        `catch` \(_ :: SomeException) -> pure ()

------------------------------------------------------------------------
-- WAI middleware
------------------------------------------------------------------------

-- | WAI middleware that intercepts WebSocket upgrade requests to
-- @/api/v1/ws@ and hands them to the WebSocket handler.  All other
-- requests pass through unchanged.
wsMiddleware :: AuthConfig -> WSState -> Wai.Middleware
wsMiddleware authCfg st app req respond
  | Wai.pathInfo req == ["api", "v1", "ws"] =
      websocketsOr WS.defaultConnectionOptions (wsApp authCfg st) app req respond
  | otherwise = app req respond

------------------------------------------------------------------------
-- WebSocket application
------------------------------------------------------------------------

-- | Handle a new WebSocket connection:
--
--   1. Authenticate (if the current legacy static bearer path is active) via @?token=…@ query param.
--   2. Accept or reject the pending connection.
--   3. Register in the connection map.
--   4. Keep alive with ping/pong; discard incoming messages.
--   5. Unregister on disconnect.
wsApp :: AuthConfig -> WSState -> WS.ServerApp
wsApp authCfg st pending
  | authStaticBearerEnabled authCfg = case tokenFromRequest (WS.pendingRequest pending) of
      Just token
        | authorizedToken authCfg (TE.decodeUtf8Lenient token) -> accept
      _ -> WS.rejectRequest pending "Unauthorized"
  | otherwise = accept
  where
    accept = do
      conn <- WS.acceptRequest pending
      connId <- addConnection st conn
      WS.withPingThread conn 30 (pure ()) $
        sinkMessages conn
          `finally` removeConnection st connId

-- | Read and discard all incoming messages until the connection closes.
sinkMessages :: WS.Connection -> IO ()
sinkMessages conn = do
  _ <- WS.receiveDataMessage conn
  sinkMessages conn

-- | Extract the @token@ query-string parameter from the WebSocket
-- request path (e.g. @/api/v1/ws?token=abc@).
tokenFromRequest :: WS.RequestHead -> Maybe ByteString
tokenFromRequest rh =
  let raw = WS.requestPath rh
      qs  = BS8.drop 1 (BS8.dropWhile (/= '?') raw)
  in case lookup "token" (parseQuery qs) of
       Just mVal -> mVal   -- Maybe ByteString inside the Maybe
       Nothing   -> Nothing

authorizedToken :: AuthConfig -> Text -> Bool
authorizedToken authCfg token = localBotTokenAuthorized authCfg token || maybe False (== token) (authStaticBearerToken authCfg)

localBotTokenAuthorized :: AuthConfig -> Text -> Bool
localBotTokenAuthorized authCfg token
  | authCfg.mode /= AuthModeLocal = False
  | otherwise = case authCfg of
      AuthConfig { local = LocalAuthConfig { botTokens = tokens } } -> any matches tokens
  where
    matches bot = case bot of
      LocalBotTokenConfig { token = botToken } -> botToken == token
