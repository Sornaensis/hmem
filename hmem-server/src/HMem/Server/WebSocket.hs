module HMem.Server.WebSocket
  ( -- * State
    WSState
  , newWSState
  , connectionCount
  , WorkspaceSubscription(..)
  , createTicket
  , createTicketWithAudience
  , createCanonicalTicket
  , createCanonicalTicketWithTtl
  , createCanonicalTicketWithExpiry
  , consumeTicket
  , ticketEventVisible
  , eventVisibleToSubscription
  , resolveLocalWebSocketAccess
  , dispatchCanonicalOutbox
  , reauthorizeCanonicalConnections
  , handleCanonicalDispatchFailure
    -- * Legacy outbox adapter
  , broadcastLegacyOutbox
    -- * WAI integration
  , wsMiddleware
  ) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, newMVar, takeMVar, tryPutMVar, withMVar)
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, modifyTVar')
import Control.Concurrent.STM qualified as STM
import Control.Exception (AsyncException, catch, finally, fromException, throwIO)
import Control.Applicative ((<|>))
import Data.Aeson (Value(..), encode, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Key qualified as Key
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Int (Int64)
import Data.List (find, nub)
import Data.Maybe (catMaybes, isJust)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDv4
import Network.HTTP.Types.URI (parseQuery)
import Network.Wai qualified as Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS
import Control.Monad (forM, forM_, unless, void, when)
import Data.Pool (Pool)
import Hasql.Connection qualified as Hasql
import System.Timeout (timeout)

import HMem.Config (AuthConfig(..), AuthMode(..), LocalAuthConfig(..), LocalBotTokenConfig(..), authStaticBearerEnabled, authStaticBearerToken)
import HMem.DB.ChangeStream (ChangeScope(..), ChangeAudience(..), ResumeToken(..), ChangeStreamError(..), replayUnacknowledgedResumeToken, replacementResumeToken, acknowledgeReplayPage, rebaseResumeTokenAfterHidden, OutboxRecord(..), ReplayPage(..))
import HMem.DB.RequestContext (ActorType(..), Principal(..), PrincipalAuthority(..))
import HMem.DB.Auth qualified as Auth
import HMem.Server.Event (ChangeEvent(..), EntityType(..))
import HMem.Types (WebSocketTicketResponse(..))

-- | Server-wide WebSocket state: a map of connection IDs to live
-- WebSocket connections, protected by a TVar for concurrent access.
data WSState = WSState
  { connections :: !(TVar (Map UUID WSClient))
  , tickets     :: !(TVar (Map Text WebSocketTicket))
  , canonicalTickets :: !(TVar (Map Text CanonicalWebSocketTicket))
  , canonicalConnections :: !(TVar (Map UUID CanonicalClient))
  }

data WSClient = WSClient
  { clientConnection              :: !WS.Connection
  , clientPrincipal               :: !(Maybe Principal)
  , clientSubscription            :: !WorkspaceSubscription
  , clientReceivesGlobalGroupEvents :: !Bool
  , clientReceivesWorkspaceAdminEvents :: !Bool
  }

data WorkspaceSubscription
  = SubscribeAllWorkspaces
  | SubscribeWorkspace !UUID
  deriving (Show, Eq)

data WebSocketTicket = WebSocketTicket
  { ticketPrincipal                 :: !Principal
  , ticketWorkspaceId               :: !UUID
  , ticketReceivesGlobalGroupEvents :: !Bool
  , ticketReceivesWorkspaceAdminEvents :: !Bool
  , ticketExpiresAt                 :: !UTCTime
  }

-- Kept in a distinct one-use map so a legacy ticket can never accidentally
-- opt a connection into canonical delivery (or vice versa).
data CanonicalWebSocketTicket = CanonicalWebSocketTicket
  { canonicalPrincipal :: !Principal
  , canonicalScope :: !ChangeScope
  , canonicalResumeToken :: !ResumeToken
  , canonicalResumeTtl :: !NominalDiffTime
  , canonicalResumeExpiresAt :: !UTCTime
  , canonicalExpiresAt :: !UTCTime
  }

-- Canonical clients are deliberately separate from legacy clients.  A client
-- owns one scope and serializes its replay/control writes, so the dispatcher
-- can safely advance the durable bearer while its receive loop is blocked.
data CanonicalClient = CanonicalClient
  { canonicalClientConnection :: !WS.Connection
  , canonicalClientPrincipal :: !Principal
  , canonicalClientScope :: !ChangeScope
  , canonicalClientToken :: !(TVar ResumeToken)
  , canonicalClientResumeTtl :: !NominalDiffTime
  , canonicalClientExpiresAt :: !UTCTime
  , canonicalClientSendLock :: !(MVar ())
  }

-- | Create a fresh (empty) WebSocket state.
newWSState :: IO WSState
newWSState = WSState <$> newTVarIO Map.empty <*> newTVarIO Map.empty <*> newTVarIO Map.empty <*> newTVarIO Map.empty

-- | Number of currently connected clients.
connectionCount :: WSState -> IO Int
connectionCount st = Map.size <$> readTVarIO st.connections

------------------------------------------------------------------------
-- Connection management
------------------------------------------------------------------------

addConnection :: WSState -> WS.Connection -> Maybe Principal -> WorkspaceSubscription -> Bool -> Bool -> IO UUID
addConnection st conn principal subscription receivesGlobalGroupEvents receivesWorkspaceAdminEvents = do
  connId <- UUIDv4.nextRandom
  let client = WSClient
        { clientConnection = conn
        , clientPrincipal = principal
        , clientSubscription = subscription
        , clientReceivesGlobalGroupEvents = receivesGlobalGroupEvents
        , clientReceivesWorkspaceAdminEvents = receivesWorkspaceAdminEvents
        }
  STM.atomically $ modifyTVar' st.connections (Map.insert connId client)
  pure connId

removeConnection :: WSState -> UUID -> IO ()
removeConnection st connId =
  STM.atomically $ modifyTVar' st.connections (Map.delete connId)

-- | Compatibility delivery for the pre-canonical client population.  It is
-- fed only by committed durable outbox rows; REST handlers never call it.
-- Canonical ticket connections are intentionally absent from this map.
broadcastLegacyOutbox :: WSState -> OutboxRecord -> IO ()
broadcastLegacyOutbox st record = do
  conns <- readTVarIO st.connections
  let msg = encode (legacyProjection record.outboxEnvelope)
  mapM_ (trySend msg) (filter (legacyVisible record.outboxEnvelope) (Map.elems conns))
  where
    trySend msg client = ignoreSynchronous (WS.sendTextData client.clientConnection msg)

legacyVisible :: Value -> WSClient -> Bool
legacyVisible envelope client = case client.clientSubscription of
  SubscribeAllWorkspaces -> legacyAudienceVisible envelope client
  SubscribeWorkspace workspace ->
    legacyAudienceVisible envelope client &&
      (jsonText "workspace_id" envelope == Just (UUID.toText workspace)
        || (client.clientReceivesGlobalGroupEvents && jsonText "workspace_id" envelope == Nothing && entityType envelope == Just "workspace_group"))

legacyAudienceVisible :: Value -> WSClient -> Bool
legacyAudienceVisible envelope client = case invalidationAudiences envelope of
  [] -> True
  audiences ->
    client.clientReceivesWorkspaceAdminEvents && "workspace-admins" `elem` audiences
      || any (matchesPrincipal client.clientPrincipal) audiences

matchesPrincipal :: Maybe Principal -> Text -> Bool
matchesPrincipal (Just Principal { authority = PrincipalGrantUser userId }) audience = audience == "user:" <> UUID.toText userId
matchesPrincipal _ _ = False

invalidationAudiences :: Value -> [Text]
invalidationAudiences envelope = case lookupValue "invalidations" envelope of
  Just (Array values) -> catMaybes [jsonText "audience" value | value <- foldr (:) [] values]
  _ -> []

legacyProjection :: Value -> Value
legacyProjection envelope = object $
  [ "type" .= legacyType (entityAction envelope)
  , "entity_type" .= entityKind
  , "entity_id" .= entityIdentity
  , "timestamp" .= jsonValue "occurred_at" envelope
  ] <> optional "workspace_id" (jsonValue "workspace_id" envelope)
    <> optional "request_id" (maybe Null id (nestedValue "transaction" "request_id" envelope))
  where
    -- Targeted membership/session envelopes contain IDs for people other than
    -- the recipient.  The legacy adapter turns them into a workspace-level
    -- invalidation rather than forwarding those identifiers.
    targeted = not (null (invalidationAudiences envelope))
    entityKind = if targeted then String "workspace" else maybe Null String (entityType envelope)
    entityIdentity = if targeted then jsonValue "workspace_id" envelope else maybe Null String (entityId envelope)

legacyType :: Maybe Text -> Text
legacyType = \case
  Just "created" -> "entity_created"
  Just "deleted" -> "entity_deleted"
  _ -> "entity_updated"

entityType, entityId, entityAction :: Value -> Maybe Text
entityType = nestedText "entity" "type"
entityId = nestedText "entity" "id"
entityAction = nestedText "entity" "action"

jsonText :: Text -> Value -> Maybe Text
jsonText key = \case
  Object fields -> case KeyMap.lookup (Key.fromText key) fields of Just (String value) -> Just value; _ -> Nothing
  _ -> Nothing

nestedText :: Text -> Text -> Value -> Maybe Text
nestedText outer inner value = nestedValue outer inner value >>= \case String text -> Just text; _ -> Nothing

jsonValue :: Text -> Value -> Value
jsonValue key value = maybe Null id (lookupValue key value)

nestedValue :: Text -> Text -> Value -> Maybe Value
nestedValue outer inner value = lookupValue outer value >>= lookupValue inner

lookupValue :: Text -> Value -> Maybe Value
lookupValue key = \case Object fields -> KeyMap.lookup (Key.fromText key) fields; _ -> Nothing

optional :: Text -> Value -> [(Key.Key, Value)]
optional _ Null = []
optional key value = [(Key.fromText key, value)]

-- | Workspace-scoped subscriptions always receive events for their workspace.
-- Global workspace-group events are additionally visible only to connections
-- whose ticket was issued to a global superadmin.
eventVisibleToSubscription :: Bool -> WorkspaceSubscription -> ChangeEvent -> Bool
eventVisibleToSubscription _ SubscribeAllWorkspaces _ = True
eventVisibleToSubscription receivesGlobalGroupEvents (SubscribeWorkspace workspaceId) event =
  event.workspaceId == Just workspaceId
    || (receivesGlobalGroupEvents && event.workspaceId == Nothing && event.entityType == ETWorkspaceGroup)

createTicket :: WSState -> Principal -> UUID -> Bool -> IO WebSocketTicketResponse
createTicket st principal workspaceId receivesGlobalGroupEvents =
  createTicketWithAudience st principal workspaceId receivesGlobalGroupEvents False

createTicketWithAudience :: WSState -> Principal -> UUID -> Bool -> Bool -> IO WebSocketTicketResponse
createTicketWithAudience st principal workspaceId receivesGlobalGroupEvents receivesWorkspaceAdminEvents = do
  now <- getCurrentTime
  ticketId <- UUID.toText <$> UUIDv4.nextRandom
  let expires = addUTCTime ticketTtlSeconds now
      ticket = WebSocketTicket
        { ticketPrincipal = principal
        , ticketWorkspaceId = workspaceId
        , ticketReceivesGlobalGroupEvents = receivesGlobalGroupEvents
        , ticketReceivesWorkspaceAdminEvents = receivesWorkspaceAdminEvents
        , ticketExpiresAt = expires
        }
  STM.atomically $ do
    allTickets <- STM.readTVar st.tickets
    let pruned = Map.filter (\existing -> existing.ticketExpiresAt > now) allTickets
    STM.writeTVar st.tickets (Map.insert ticketId ticket pruned)
  pure WebSocketTicketResponse { ticket = ticketId, expiresAt = expires }

createCanonicalTicket :: WSState -> Principal -> ChangeScope -> ResumeToken -> IO WebSocketTicketResponse
createCanonicalTicket st principal scope resume =
  createCanonicalTicketWithTtl st canonicalResumeTtlDefault principal scope resume

createCanonicalTicketWithTtl :: WSState -> NominalDiffTime -> Principal -> ChangeScope -> ResumeToken -> IO WebSocketTicketResponse
createCanonicalTicketWithTtl st resumeTtl principal scope resume = do
  now <- getCurrentTime
  createCanonicalTicketWithExpiry st (addUTCTime resumeTtl now) principal scope resume

-- | The public ticket hand-off retains the durable bearer expiry validated by
-- the core state machine. In particular, a reconnect near the end of an
-- existing bearer must not obtain a fresh configured TTL merely by receiving
-- a new one-use ticket.
createCanonicalTicketWithExpiry :: WSState -> UTCTime -> Principal -> ChangeScope -> ResumeToken -> IO WebSocketTicketResponse
createCanonicalTicketWithExpiry st resumeExpiresAt principal scope resume = do
  now <- getCurrentTime
  ticketId <- UUID.toText <$> UUIDv4.nextRandom
  let expires = min (addUTCTime ticketTtlSeconds now) resumeExpiresAt
      resumeTtl = max 0 (diffUTCTime resumeExpiresAt now)
      canonical = CanonicalWebSocketTicket principal scope resume resumeTtl resumeExpiresAt expires
  STM.atomically $ do
    allTickets <- STM.readTVar st.canonicalTickets
    let pruned = Map.filter (\existing -> existing.canonicalExpiresAt > now) allTickets
    STM.writeTVar st.canonicalTickets (Map.insert ticketId canonical pruned)
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

consumeCanonicalTicket :: WSState -> Text -> IO (Maybe CanonicalWebSocketTicket)
consumeCanonicalTicket st ticketId = do
  now <- getCurrentTime
  STM.atomically $ do
    allTickets <- STM.readTVar st.canonicalTickets
    let pruned = Map.filter (\ticket -> ticket.canonicalExpiresAt > now) allTickets
        found = Map.lookup ticketId pruned
    STM.writeTVar st.canonicalTickets (Map.delete ticketId pruned)
    pure found

-- | The event-visibility decision retained in a deployed ticket.  Keeping
-- this next to ticket consumption makes the global-superadmin grant boundary
-- testable without exposing ticket internals.
ticketEventVisible :: WebSocketTicket -> ChangeEvent -> Bool
ticketEventVisible ticket = uncurry eventVisibleToSubscription (ticketSubscription ticket)

ticketSubscription :: WebSocketTicket -> (Bool, WorkspaceSubscription)
ticketSubscription ticket =
  (ticket.ticketReceivesGlobalGroupEvents, SubscribeWorkspace ticket.ticketWorkspaceId)

ticketTtlSeconds :: NominalDiffTime
ticketTtlSeconds = 60

------------------------------------------------------------------------
-- WAI middleware
------------------------------------------------------------------------

-- | WAI middleware that intercepts WebSocket upgrade requests to
-- @/api/v1/ws@ and hands them to the WebSocket handler.  All other
-- requests pass through unchanged.
wsMiddleware :: AuthConfig -> Pool Hasql.Connection -> WSState -> Wai.Middleware
wsMiddleware authCfg pool st app req respond
  | Wai.pathInfo req == ["api", "v1", "ws"] =
      websocketsOr WS.defaultConnectionOptions (wsApp authCfg pool st) app req respond
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
wsApp :: AuthConfig -> Pool Hasql.Connection -> WSState -> WS.ServerApp
wsApp authCfg pool st pending = case ticketFromRequest (WS.pendingRequest pending) of
  -- Canonical tickets are consumed before legacy tickets in every mode.  A
  -- local server must exercise the same one-use canonical hand-off rather
  -- than silently treating a canonical reconnect as legacy traffic.
  Just ticketText -> do
    canonical <- consumeCanonicalTicket st (TE.decodeUtf8Lenient ticketText)
    case canonical of
      Just canonicalTicket -> acceptCanonical canonicalTicket
      Nothing -> acceptLegacyTicket ticketText
  Nothing -> acceptWithoutTicket
  where
    acceptLegacyTicket ticketText
      | authCfg.mode /= AuthModeDeployed = WS.rejectRequest pending "Unauthorized"
      | otherwise = do
          mTicket <- consumeTicket st (TE.decodeUtf8Lenient ticketText)
          case mTicket of
            Just ticket ->
              let (receivesGlobalGroupEvents, subscription) = ticketSubscription ticket
               in accept (Just ticket.ticketPrincipal) subscription receivesGlobalGroupEvents ticket.ticketReceivesWorkspaceAdminEvents
            Nothing -> WS.rejectRequest pending "Unauthorized"

    acceptWithoutTicket
      | authCfg.mode == AuthModeDeployed = WS.rejectRequest pending "Unauthorized"
      | otherwise = case resolveLocalWebSocketAccess authCfg (TE.decodeUtf8Lenient <$> tokenFromRequest (WS.pendingRequest pending)) of
          Just (principal, subscription) -> accept principal subscription True True
          Nothing -> WS.rejectRequest pending "Unauthorized"

    accept principal subscription receivesGlobalGroupEvents receivesWorkspaceAdminEvents = do
      conn <- WS.acceptRequest pending
      connId <- addConnection st conn principal subscription receivesGlobalGroupEvents receivesWorkspaceAdminEvents
      WS.withPingThread conn 30 (pure ()) $
        sinkMessages conn
          `finally` removeConnection st connId

    -- A canonical ticket is never routed through the legacy in-memory
    -- broadcast map.  It first drains the retained durable outbox and only
    -- then acknowledges catch-up with an opaque replacement token.
    acceptCanonical canonicalTicket = do
      conn <- WS.acceptRequest pending
      connId <- addCanonicalConnection st conn canonicalTicket.canonicalPrincipal canonicalTicket.canonicalScope canonicalTicket.canonicalResumeToken canonicalTicket.canonicalResumeTtl canonicalTicket.canonicalResumeExpiresAt
      mClient <- Map.lookup connId <$> readTVarIO st.canonicalConnections
      forM_ mClient (advanceCanonicalClient pool st connId True)
      stillLive <- Map.member connId <$> readTVarIO st.canonicalConnections
      forM_ mClient $ \client -> when stillLive $ WS.withPingThread conn 30 (pure ()) $
        do
          finished <- newEmptyMVar
          _ <- forkIO $ expireCanonicalConnection st connId client finished
          sinkMessages conn `finally` (void (tryPutMVar finished ()) >> removeCanonicalConnection st connId)

addCanonicalConnection :: WSState -> WS.Connection -> Principal -> ChangeScope -> ResumeToken -> NominalDiffTime -> UTCTime -> IO UUID
addCanonicalConnection st conn principal scope token resumeTtl expiresAt = do
  connId <- UUIDv4.nextRandom
  bearer <- newTVarIO token
  sendLock <- newMVar ()
  STM.atomically $ modifyTVar' st.canonicalConnections (Map.insert connId (CanonicalClient conn principal scope bearer resumeTtl expiresAt sendLock))
  pure connId

removeCanonicalConnection :: WSState -> UUID -> IO ()
removeCanonicalConnection st connId =
  STM.atomically $ modifyTVar' st.canonicalConnections (Map.delete connId)

-- The only write path for a canonical client.  A page is scanned and its
-- replacement bearer is retained even when every record is hidden; callers
-- invoke this only for potentially visible outbox work, so hidden-only
-- activity never creates a checkpoint cadence observable by a subscriber.
replayUnlocked :: Pool Hasql.Connection -> NominalDiffTime -> WS.Connection -> Principal -> ChangeScope -> ResumeToken -> Bool -> IO (Maybe ResumeToken)
replayUnlocked pool resumeTtl conn principal scope initialToken forceCheckpoint = go initialToken False
  where
    audience = audienceFor principal
    go token sawVisible = do
      replayed <- replayUnacknowledgedResumeToken pool scope audience token canonicalReplayPageSize
      case replayed of
        Left err -> do
          sendFrameUnlocked conn (object ["type" .= terminalFrame err, "schema_version" .= (1 :: Int)])
          pure Nothing
        Right page -> do
          visible <- fmap catMaybes $ forM page.replayPageRecords (projectRecord pool principal scope)
          forM_ visible $ \event -> sendFrameUnlocked conn (object ["type" .= ("change" :: Text), "schema_version" .= (1 :: Int), "event" .= event])
          -- Do not rotate/supersede the bearer until all associated frames
          -- were accepted by the socket.  Send failure leaves this token live
          -- for a reconnect, which may redeliver the same event ID.
          let (acknowledgedThrough, expectedReplacement) = case reverse page.replayPageRecords of
                [] -> (Nothing, token)
                record:_ -> (Just record.outboxCursor, replacementResumeToken scope audience token record.outboxCursor)
          -- The terminal bearer is sent before its durable acknowledgement.
          -- If this write throws, the old bearer is still valid and reconnect
          -- simply repeats this at-least-once page.
          when (not page.replayPageHasMore && (forceCheckpoint || sawVisible || not (null visible))) $
            sendFrameUnlocked conn (object
              [ "type" .= ("checkpoint" :: Text), "schema_version" .= (1 :: Int)
              , "resume_token" .= resumeText expectedReplacement, "catch_up" .= ("complete" :: Text) ])
          acknowledged <- case acknowledgedThrough of
            Nothing -> pure (Right token)
            Just cursor -> acknowledgeReplayPage pool resumeTtl scope audience token cursor
          case acknowledged of
            Left err -> do
              sendFrameUnlocked conn (object ["type" .= terminalFrame err, "schema_version" .= (1 :: Int)])
              pure Nothing
            Right replacement
              | page.replayPageHasMore -> go replacement (sawVisible || not (null visible))
              | otherwise -> pure (Just replacement)

audienceFor :: Principal -> ChangeAudience
audienceFor principal = case principal.authority of
  PrincipalGrantUser userId -> AuthenticatedAudience (UUID.toText userId) userId
  PrincipalSyntheticLocalSuperadmin -> TrustedAudience ("local:" <> principal.actorId)
  PrincipalNoAuthority -> TrustedAudience "invalid"

terminalFrame :: ChangeStreamError -> Text
terminalFrame ResyncUnauthorized = "access_revoked"
terminalFrame _ = "resync_required"

sendFrame :: WS.Connection -> MVar () -> Value -> IO ()
sendFrame conn lock value = withMVar lock $ \_ -> sendFrameUnlocked conn value

sendFrameUnlocked :: WS.Connection -> Value -> IO ()
sendFrameUnlocked conn value = WS.sendTextData conn (encode value)

-- Only the invalidations that name this audience survive projection.  The
-- durable envelope remains internal; this prevents an admin-only or another
-- user's membership change from becoming a timing/data side-channel.
projectRecord :: Pool Hasql.Connection -> Principal -> ChangeScope -> OutboxRecord -> IO (Maybe Value)
projectRecord pool principal scope record = do
  let envelope = subscriberEvent record.outboxEnvelope
  case lookupValue "invalidations" envelope of
    Just (Array values) -> do
      visible <- fmap catMaybes $ forM (foldr (:) [] values) (visibleInvalidation pool principal scope)
      pure $ if null visible then Nothing else Just (replaceInvalidations envelope visible)
    _ -> pure (Just envelope)

visibleInvalidation :: Pool Hasql.Connection -> Principal -> ChangeScope -> Value -> IO (Maybe Value)
visibleInvalidation pool principal scope invalidation = case jsonText "audience" invalidation of
  Nothing -> pure (Just invalidation)
  Just audience | audience == "workspace-admins" -> do
    allowed <- case scope of
      WorkspaceScope workspace -> Auth.hasWorkspaceRole pool (Just principal) workspace Auth.WorkspaceRoleAdmin
      GlobalScope -> Auth.hasGlobalPermission pool (Just principal) Auth.GlobalSuperadmin
    pure (if allowed || isTrusted principal then Just invalidation else Nothing)
  Just audience -> pure $ case principal.authority of
    PrincipalGrantUser userId | audience == "user:" <> UUID.toText userId -> Just invalidation
    _ -> Nothing

isTrusted :: Principal -> Bool
isTrusted Principal { authority = PrincipalSyntheticLocalSuperadmin } = True
isTrusted _ = False

replaceInvalidations :: Value -> [Value] -> Value
replaceInvalidations (Object fields) values = Object (KeyMap.insert "invalidations" (toJSON values) fields)
replaceInvalidations value _ = value

-- Called by the supervised dispatcher after committed outbox rows are seen.
-- Each client advances from its own opaque token, never from dispatcher
-- process memory, eliminating the fetch/subscribe race on reconnect.
dispatchCanonicalOutbox :: Pool Hasql.Connection -> WSState -> [OutboxRecord] -> IO ()
dispatchCanonicalOutbox pool st records = do
  clients <- Map.toList <$> readTVarIO st.canonicalConnections
  forM_ clients $ \(connId, _) -> dispatchClient connId records
  where
    dispatchClient _ [] = pure ()
    dispatchClient connId batch = do
      mClient <- Map.lookup connId <$> readTVarIO st.canonicalConnections
      forM_ mClient $ \client -> do
        handleCanonicalDispatchFailure (removeCanonicalConnection st connId) $
          applyAccessControls pool st connId client batch
        stillConnected <- Map.member connId <$> readTVarIO st.canonicalConnections
        when stillConnected $ do
          visible <- anyM (isVisible client) (filter (recordInScope client.canonicalClientScope) batch)
          -- A single replay drains the batch through a fixed high-water and
          -- emits one visible replacement checkpoint. Hidden-only activity is
          -- deliberately left behind the bearer: it cannot change a connected
          -- client's cadence, and a reconnect can safely scan it later.
          if visible
            then advanceCanonicalClient pool st connId False client
            else forM_ (filter (recordInScope client.canonicalClientScope) batch) $ \record ->
              rebaseCanonicalClient pool st connId client record.outboxCursor

    isVisible client record = isJust <$> projectRecord pool client.canonicalClientPrincipal client.canonicalClientScope record

rebaseCanonicalClient :: Pool Hasql.Connection -> WSState -> UUID -> CanonicalClient -> Int64 -> IO ()
rebaseCanonicalClient pool st connId client hiddenThrough = withMVar client.canonicalClientSendLock $ \_ -> do
  oldToken <- readTVarIO client.canonicalClientToken
  rebased <- rebaseResumeTokenAfterHidden pool client.canonicalClientResumeTtl client.canonicalClientScope (audienceFor client.canonicalClientPrincipal) oldToken hiddenThrough
  case rebased of
    Right token -> STM.atomically $ STM.writeTVar client.canonicalClientToken token
    Left err -> do
      sendFrameUnlocked client.canonicalClientConnection (object ["type" .= terminalFrame err, "schema_version" .= (1 :: Int)])
      removeCanonicalConnection st connId

-- The worker scans a shared durable outbox, but canonical replay is scoped.
-- Targeted access controls are intentionally considered separately above;
-- they never make the socket subscribe to the newly granted workspace.
recordInScope :: ChangeScope -> OutboxRecord -> Bool
recordInScope scope record = case (scope, jsonText "workspace_id" record.outboxEnvelope >>= UUID.fromText) of
  (WorkspaceScope workspace, Just recordWorkspace) -> workspace == recordWorkspace
  (GlobalScope, Nothing) -> True
  _ -> False

advanceCanonicalClient :: Pool Hasql.Connection -> WSState -> UUID -> Bool -> CanonicalClient -> IO ()
advanceCanonicalClient pool st connId forceCheckpoint client =
  handleCanonicalDispatchFailure (removeCanonicalConnection st connId) $
    withMVar client.canonicalClientSendLock $ \_ -> do
      -- Reading, acknowledging, and storing this bearer occurs under the same
      -- client lock. A dispatcher tick that races initial replay starts only
       -- after the replacement has been retained.
       oldToken <- readTVarIO client.canonicalClientToken
       remainingTtl <- remainingCanonicalTtl client
       if remainingTtl <= 0
         then do
           sendFrameUnlocked client.canonicalClientConnection (object ["type" .= ("resync_required" :: Text), "schema_version" .= (1 :: Int)])
           removeCanonicalConnection st connId
           ignoreSynchronous $ WS.sendClose client.canonicalClientConnection ("canonical resume expired" :: Text)
         else do
           replacement <- replayUnlocked pool remainingTtl client.canonicalClientConnection client.canonicalClientPrincipal client.canonicalClientScope oldToken forceCheckpoint
           case replacement of
             Nothing -> removeCanonicalConnection st connId
             Just token -> STM.atomically $ STM.writeTVar client.canonicalClientToken token

-- | A connected canonical client has one fixed delivery deadline. Visible
-- activity may rotate its bearer for acknowledgement, but cannot prolong the
-- connection's capability lifetime; hidden activity keeps the bearer in place
-- and therefore cannot leak itself through a changed expiry cadence.
remainingCanonicalTtl :: CanonicalClient -> IO NominalDiffTime
remainingCanonicalTtl client = do
  now <- getCurrentTime
  pure $ max 0 (diffUTCTime client.canonicalClientExpiresAt now)

expireCanonicalConnection :: WSState -> UUID -> CanonicalClient -> MVar () -> IO ()
expireCanonicalConnection st connId client finished = do
  remaining <- remainingCanonicalTtl client
  expired <- waitForCanonicalExpiry remaining finished
  when expired $ do
    live <- Map.member connId <$> readTVarIO st.canonicalConnections
    when live $ do
      sendFrame client.canonicalClientConnection client.canonicalClientSendLock
        (object ["type" .= ("resync_required" :: Text), "schema_version" .= (1 :: Int)])
      removeCanonicalConnection st connId
      ignoreSynchronous $ WS.sendClose client.canonicalClientConnection ("canonical resume expired" :: Text)

-- 'threadDelay' is platform-bounded.  Long configured resume TTLs therefore
-- wait in bounded chunks instead of overflowing into an immediate expiry on
-- Windows, while a disconnect still wakes the timer promptly.
waitForCanonicalExpiry :: NominalDiffTime -> MVar () -> IO Bool
waitForCanonicalExpiry remaining finished
  | remaining <= 0 = pure True
  | otherwise = do
      let chunk = min remaining 60
      completed <- timeout (floor (chunk * 1000000)) (takeMVar finished)
      case completed of
        Just () -> pure False
        Nothing -> waitForCanonicalExpiry (remaining - chunk) finished

-- | Recover only synchronous delivery failures. Dispatcher cancellation is a
-- control-flow signal and must reach the supervising worker unchanged.
handleCanonicalDispatchFailure :: IO () -> IO () -> IO ()
handleCanonicalDispatchFailure cleanup action = action `catch` \err ->
  case fromException err :: Maybe AsyncException of
    Just _ -> throwIO err
    Nothing -> cleanup

-- Membership invalidations carry a user-targeted catalogue/session audience.
-- Emit controls only to that user's already-authenticated canonical socket;
-- the frame contains the scope identifier but never membership row data.
applyAccessControls :: Pool Hasql.Connection -> WSState -> UUID -> CanonicalClient -> [OutboxRecord] -> IO ()
applyAccessControls pool st connId client records = do
  let targets = nub (concatMap targetedWorkspaceUsers records)
  forM_ targets $ \(workspace, userId) -> case client.canonicalClientPrincipal.authority of
    PrincipalGrantUser currentUser | currentUser == userId -> do
      allowed <- scopeAuthorized pool client.canonicalClientPrincipal (WorkspaceScope workspace)
      case client.canonicalClientScope of
        WorkspaceScope current | current == workspace ->
          unless allowed $ do
            -- A revoke can terminate only the connection actually subscribed
            -- to that workspace. Another scoped socket for the same user must
            -- remain live and learn no cross-scope revocation activity.
            sendFrame client.canonicalClientConnection client.canonicalClientSendLock (accessFrame "access_revoked" workspace)
            removeCanonicalConnection st connId
            ignoreSynchronous $ WS.sendClose client.canonicalClientConnection ("access revoked" :: Text)
        _ ->
          -- Cross-scope controls update this user's catalogue/session state
          -- without changing the socket's subscription. A revoke therefore
          -- remains observable to the matching user, while this retained
          -- scope stays connected and can continue to receive its own work.
          sendFrame client.canonicalClientConnection client.canonicalClientSendLock
            (accessFrame (if allowed then "access_granted" else "access_revoked") workspace)
    _ -> pure ()

accessFrame :: Text -> UUID -> Value
accessFrame frame workspace = object
  [ "type" .= frame, "schema_version" .= (1 :: Int), "workspace_id" .= UUID.toText workspace ]

targetedWorkspaceUsers :: OutboxRecord -> [(UUID, UUID)]
targetedWorkspaceUsers record = case jsonText "workspace_id" record.outboxEnvelope >>= UUID.fromText of
  Nothing -> []
  Just workspace -> case lookupValue "invalidations" record.outboxEnvelope of
    Just (Array values) -> catMaybes (map (target workspace) (foldr (:) [] values))
    _ -> []
  where
    target workspace invalidation = do
      audience <- jsonText "audience" invalidation
      userText <- Text.stripPrefix "user:" audience
      userId <- UUID.fromText userText
      pure (workspace, userId)

reauthorizeCanonicalConnections :: Pool Hasql.Connection -> WSState -> IO ()
reauthorizeCanonicalConnections pool st = do
  clients <- Map.toList <$> readTVarIO st.canonicalConnections
  forM_ clients $ \(connId, client) -> do
    allowed <- scopeAuthorized pool client.canonicalClientPrincipal client.canonicalClientScope
    unless allowed $ do
      sendFrame client.canonicalClientConnection client.canonicalClientSendLock (object ["type" .= ("access_revoked" :: Text), "schema_version" .= (1 :: Int)])
      removeCanonicalConnection st connId
      ignoreSynchronous $ WS.sendClose client.canonicalClientConnection ("access revoked" :: Text)

scopeAuthorized :: Pool Hasql.Connection -> Principal -> ChangeScope -> IO Bool
scopeAuthorized _ principal _ | isTrusted principal = pure True
scopeAuthorized pool principal scope = case scope of
  WorkspaceScope workspace -> Auth.hasWorkspaceRole pool (Just principal) workspace Auth.WorkspaceRoleRead
  GlobalScope -> Auth.hasGlobalPermission pool (Just principal) Auth.GlobalSuperadmin

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ [] = pure False
anyM predicate (value:values) = do
  found <- predicate value
  if found then pure True else anyM predicate values

ignoreSynchronous :: IO () -> IO ()
ignoreSynchronous action = action `catch` \err ->
  case fromException err :: Maybe AsyncException of
    Just _ -> throwIO err
    Nothing -> pure ()

subscriberEvent :: Value -> Value
subscriberEvent = \case
  Object fields -> Object (KeyMap.delete "cursor" fields)
  other -> other

resumeText :: ResumeToken -> Text
resumeText (ResumeToken value) = value

canonicalResumeTtlDefault :: NominalDiffTime
canonicalResumeTtlDefault = 86400

canonicalReplayPageSize :: Int
canonicalReplayPageSize = 500

-- | Read and discard all incoming messages until the connection closes.
sinkMessages :: WS.Connection -> IO ()
sinkMessages conn = (do
  _ <- WS.receiveDataMessage conn
  sinkMessages conn) `catch` \(_ :: WS.ConnectionException) -> pure ()

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
