module HMem.Server.WebSocket
  ( -- * State
    WSState
  , newWSState
  , connectionCount
  , WorkspaceSubscription(..)
  , createTicket
  , consumeTicket
  , eventVisibleToSubscription
  , resolveLocalWebSocketAccess
    -- * Broadcast
  , broadcast
    -- * WAI integration
  , wsMiddleware
  ) where

import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, modifyTVar')
import Control.Concurrent.STM qualified as STM
import Control.Exception (SomeException, catch, finally)
import Control.Applicative ((<|>))
import Data.Aeson (encode)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDv4
import Network.HTTP.Types.URI (parseQuery)
import Network.Wai qualified as Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS

import HMem.Config (AuthConfig(..), AuthMode(..), LocalAuthConfig(..), LocalBotTokenConfig(..), authStaticBearerEnabled, authStaticBearerToken)
import HMem.DB.RequestContext (ActorType(..), Principal(..), PrincipalAuthority(..))
import HMem.Server.Event (ChangeEvent(..))
import HMem.Types (WebSocketTicketResponse(..))

-- | Server-wide WebSocket state: a map of connection IDs to live
-- WebSocket connections, protected by a TVar for concurrent access.
data WSState = WSState
  { connections :: !(TVar (Map UUID WSClient))
  , tickets     :: !(TVar (Map Text WebSocketTicket))
  }

data WSClient = WSClient
  { clientConnection   :: !WS.Connection
  , clientPrincipal    :: !(Maybe Principal)
  , clientSubscription :: !WorkspaceSubscription
  }

data WorkspaceSubscription
  = SubscribeAllWorkspaces
  | SubscribeWorkspace !UUID
  deriving (Show, Eq)

data WebSocketTicket = WebSocketTicket
  { ticketPrincipal   :: !Principal
  , ticketWorkspaceId :: !UUID
  , ticketExpiresAt   :: !UTCTime
  }

-- | Create a fresh (empty) WebSocket state.
newWSState :: IO WSState
newWSState = WSState <$> newTVarIO Map.empty <*> newTVarIO Map.empty

-- | Number of currently connected clients.
connectionCount :: WSState -> IO Int
connectionCount st = Map.size <$> readTVarIO st.connections

------------------------------------------------------------------------
-- Connection management
------------------------------------------------------------------------

addConnection :: WSState -> WS.Connection -> Maybe Principal -> WorkspaceSubscription -> IO UUID
addConnection st conn principal subscription = do
  connId <- UUIDv4.nextRandom
  let client = WSClient
        { clientConnection = conn
        , clientPrincipal = principal
        , clientSubscription = subscription
        }
  STM.atomically $ modifyTVar' st.connections (Map.insert connId client)
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
  mapM_ (trySend msg) (filter (\client -> eventVisibleToSubscription client.clientSubscription event) (Map.elems conns))
  where
    trySend msg client =
      WS.sendTextData client.clientConnection msg
        `catch` \(_ :: SomeException) -> pure ()

eventVisibleToSubscription :: WorkspaceSubscription -> ChangeEvent -> Bool
eventVisibleToSubscription SubscribeAllWorkspaces _ = True
eventVisibleToSubscription (SubscribeWorkspace workspaceId) event =
  event.workspaceId == Just workspaceId

createTicket :: WSState -> Principal -> UUID -> IO WebSocketTicketResponse
createTicket st principal workspaceId = do
  now <- getCurrentTime
  ticketId <- UUID.toText <$> UUIDv4.nextRandom
  let expires = addUTCTime ticketTtlSeconds now
      ticket = WebSocketTicket
        { ticketPrincipal = principal
        , ticketWorkspaceId = workspaceId
        , ticketExpiresAt = expires
        }
  STM.atomically $ do
    allTickets <- STM.readTVar st.tickets
    let pruned = Map.filter (\existing -> existing.ticketExpiresAt > now) allTickets
    STM.writeTVar st.tickets (Map.insert ticketId ticket pruned)
  pure WebSocketTicketResponse { ticket = ticketId, expiresAt = expires }

consumeTicket :: WSState -> Text -> IO (Maybe WebSocketTicket)
consumeTicket st ticketId = do
  now <- getCurrentTime
  STM.atomically $ do
    allTickets <- STM.readTVar st.tickets
    let pruned = Map.filter (\ticket -> ticket.ticketExpiresAt > now) allTickets
        found = Map.lookup ticketId pruned
        remaining = Map.delete ticketId pruned
    STM.writeTVar st.tickets remaining
    pure found

ticketTtlSeconds :: NominalDiffTime
ticketTtlSeconds = 60

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
  | authCfg.mode == AuthModeDeployed = case ticketFromRequest (WS.pendingRequest pending) of
      Just ticketText -> do
        mTicket <- consumeTicket st (TE.decodeUtf8Lenient ticketText)
        case mTicket of
          Just ticket -> accept (Just ticket.ticketPrincipal) (SubscribeWorkspace ticket.ticketWorkspaceId)
          Nothing -> WS.rejectRequest pending "Unauthorized"
      Nothing -> WS.rejectRequest pending "Unauthorized"
  | otherwise = case resolveLocalWebSocketAccess authCfg (TE.decodeUtf8Lenient <$> tokenFromRequest (WS.pendingRequest pending)) of
      Just (principal, subscription) -> accept principal subscription
      Nothing -> WS.rejectRequest pending "Unauthorized"
  where
    accept principal subscription = do
      conn <- WS.acceptRequest pending
      connId <- addConnection st conn principal subscription
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

ticketFromRequest :: WS.RequestHead -> Maybe ByteString
ticketFromRequest rh =
  let raw = WS.requestPath rh
      qs  = BS8.drop 1 (BS8.dropWhile (/= '?') raw)
  in case lookup "ticket" (parseQuery qs) of
       Just mVal -> mVal
       Nothing   -> Nothing

authorizedTokenPrincipal :: AuthConfig -> Text -> Maybe Principal
authorizedTokenPrincipal authCfg token =
  localBotPrincipal authCfg token <|> legacyPrincipal authCfg token

resolveLocalWebSocketAccess :: AuthConfig -> Maybe Text -> Maybe (Maybe Principal, WorkspaceSubscription)
resolveLocalWebSocketAccess authCfg mToken
  | authCfg.mode /= AuthModeLocal = Nothing
  | Just token <- mToken
  , Just principal <- authorizedTokenPrincipal authCfg token =
      Just (Just principal, SubscribeAllWorkspaces)
  | authStaticBearerEnabled authCfg = Nothing
  | AuthConfig { local = LocalAuthConfig { allowRemoteBootstrap = True } } <- authCfg = Nothing
  | otherwise = Just (defaultLocalPrincipal authCfg, SubscribeAllWorkspaces)

localBotPrincipal :: AuthConfig -> Text -> Maybe Principal
localBotPrincipal authCfg token
  | authCfg.mode /= AuthModeLocal = Nothing
  | otherwise = case authCfg of
      AuthConfig { local = LocalAuthConfig { botTokens = tokens } } -> do
        cfg <- find matches tokens
        pure Principal
          { actorType = ActorBot
          , actorId = "local-bot:" <> botLabel cfg
          , actorLabel = botLabel cfg
          , authority = PrincipalSyntheticLocalSuperadmin
          }
  where
    matches bot = case bot of
      LocalBotTokenConfig { token = botToken } -> botToken == token

    botLabel bot = case bot of
      LocalBotTokenConfig { label = lbl } -> lbl

legacyPrincipal :: AuthConfig -> Text -> Maybe Principal
legacyPrincipal authCfg token = do
  expected <- authStaticBearerToken authCfg
  if token == expected
    then Just Principal
      { actorType = ActorBot
      , actorId = "legacy-static-bearer"
      , actorLabel = "Legacy Static Bearer"
      , authority = PrincipalSyntheticLocalSuperadmin
      }
    else Nothing

defaultLocalPrincipal :: AuthConfig -> Maybe Principal
defaultLocalPrincipal authCfg
  | authCfg.mode == AuthModeLocal
  , AuthConfig { local = LocalAuthConfig { bootstrapEnabled = True } } <- authCfg =
      Just Principal
        { actorType = ActorUser
        , actorId = "local-user"
        , actorLabel = "Local User"
        , authority = PrincipalSyntheticLocalSuperadmin
        }
  | otherwise = Nothing
