module HMem.Server.WebSocket
  ( -- * State
    WSState
  , newWSState
  , newWSStateWithDeliveryPool
  , deliveryPoolOf
  , deliveryPoolFor
  , withNativeWebSocketSettings
  , runPlainWithNativeWebSockets
  , beginWebSocketShutdown
  , awaitWebSocketDrain
  , connectionCount
    -- * Telemetry
  , attachWebSocketTelemetryLog
  , detachWebSocketTelemetryLog
  , flushWebSocketTelemetry
  , readWebSocketTelemetry
  , WorkspaceSubscription(..)
  , createTicket
  , createTicketWithAudience
  , createCanonicalTicket
  , createCanonicalTicketWithTtl
  , createCanonicalTicketWithExpiry
  , createActivityTicket
  , createActivityTicketWithTtl
  , consumeTicket
  , ticketEventVisible
  , eventVisibleToSubscription
  , resolveLocalWebSocketAccess
  , dispatchCanonicalOutbox
  , dispatchOutboxBatch
  , canonicalReplayPending
  , reauthorizeCanonicalConnections
  , handleCanonicalDispatchFailure
    -- * Legacy outbox adapter
  , broadcastLegacyOutbox
    -- * WAI integration
  , wsMiddleware
  ) where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, withMVar, threadDelay)
import Control.Concurrent.Async (asyncThreadId, cancel, waitAnyCatch, waitCatch, waitSTM, withAsync, withAsyncWithUnmask)
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, modifyTVar')
import Control.Concurrent.STM qualified as STM
import Control.Exception (SomeAsyncException, SomeException, bracket, catch, finally, fromException, mask, onException, throwIO, try)
import Control.Applicative ((<|>))
import Data.Aeson (Value(..), encode, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Key qualified as Key
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Int (Int64)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (find, nub)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDv4
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Types.URI (parseQuery)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (Settings)
import Network.Wai.Handler.WebSockets (isWebSocketsReq, websocketsOr)
import Network.WebSockets qualified as WS
import Control.Monad (forM, forM_, unless, void, when)
import Data.Pool (Pool)
import Hasql.Connection qualified as Hasql

import HMem.Config (AuthConfig(..), AuthMode(..), LocalAuthConfig(..), LocalBotTokenConfig(..), authStaticBearerEnabled, authStaticBearerToken)
import HMem.Config qualified as Config
import HMem.DB.ChangeStream (ChangeScope(..), ChangeAudience(..), ResumeToken(..), ChangeStreamError(..), readReplayScanPage, replacementResumeToken, acknowledgeReplayScan, rebaseResumeTokenAfterHidden, OutboxRecord(..), ReplayScanPage(..), ReplayScanWindow(..))
import HMem.DB.RequestContext (ActorType(..), Principal(..), PrincipalAuthority(..))
import HMem.DB.Auth qualified as Auth
import HMem.Server.Event (ChangeEvent(..), EntityType(..))
import HMem.Server.BotIdentity (requestBotIdentity)
import HMem.Server.WebSocketTransport
  ( NativeTransport, NativeConnection, DeliveryTurn, DeliveryResult(..), DeliveryFailure(..), DeliveryCleanupFailure(..)
  , abortNativeConnection, nativeConnectionClosing, currentNativeConnection, newNativeTransportWithLeases, runNativePlain, withNativeTransportSettings
  , reservePhysicalConnection, installControlledWriter, withDeliveryTurn, deliveryShouldYield, sendOwnedClose
  , beginNativeShutdown, awaitNativeDrain, nativeAdmissionOpen, nativeGauges, nativeTelemetry )
import HMem.Server.WebSocketTelemetry
  ( WebSocketTelemetry, TelemetryRecord(..), TicketKind(..), UpgradeOutcome(..), ConnectionOutcome(..)
  , ReplayPhase(..), ReplayOutcome(..), WebSocketTelemetrySnapshot(..)
  , attachTelemetryLog, detachTelemetryLog, droppedLogEvents, elapsedNanos, flushTelemetryLog, readOutcomeStats, recordTelemetry )
import System.Log.FastLogger (FastLogger)
import Network.HTTP.Types (hContentType, status400)
import HMem.Types (WebSocketTicketResponse(..))
import HMem.DB.Pool (PoolMetrics(..), getPoolMetricsFor)
import HMem.Server.WebSocketDelivery (defaultDeliveryLeases)

-- | Server-wide WebSocket state: a map of connection IDs to live
-- WebSocket connections, protected by a TVar for concurrent access.
data WSState = WSState
  { connections :: !(TVar (Map UUID WSClient))
  , tickets     :: !(TVar (Map Text WebSocketTicket))
  , canonicalTickets :: !(TVar (Map Text CanonicalWebSocketTicket))
  , canonicalConnections :: !(TVar (Map UUID CanonicalClient))
  , activityTickets :: !(TVar ActivityTickets)
  , activityConnections :: !(TVar (Map UUID ActivityClient))
  , nativeTransport :: !NativeTransport
  , telemetry :: !WebSocketTelemetry
  -- | The separate pool for leased delivery work and the dispatcher, or
  -- Nothing to share the caller's main pool (tests and the harness only).
  , deliveryPool :: !(Maybe (Pool Hasql.Connection))
  }

data WSClient = WSClient
  { clientConnection              :: !WS.Connection
  , clientNative                  :: !NativeConnection
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
  , canonicalClientNative :: !NativeConnection
  , canonicalClientPrincipal :: !Principal
  , canonicalClientScope :: !ChangeScope
  , canonicalClientToken :: !(TVar ResumeToken)
  , canonicalClientReplay :: !(TVar (Maybe CanonicalReplay))
  , canonicalClientResumeTtl :: !NominalDiffTime
  , canonicalClientExpiresAt :: !UTCTime
  , canonicalClientSendLock :: !(MVar ())
  , canonicalClientRetired :: !(STM.TMVar ())
  -- Telemetry only: the open replay cycle and its monotonic start.
  , canonicalClientReplayCycle :: !(TVar (Maybe (ReplayPhase, Word64)))
  }

data CanonicalReplay = CanonicalReplay
  { replayOriginalBearer :: !ResumeToken
  , replayWindow :: !(Maybe ReplayScanWindow)
  , replaySawVisible :: !Bool
  , replayInitialCheckpoint :: !Bool
  , replayDirty :: !Bool
  }

newCanonicalReplay :: ResumeToken -> Bool -> CanonicalReplay
newCanonicalReplay token initial = CanonicalReplay token Nothing False initial False

data ActivityTicket = ActivityTicket
  { activityTicketPrincipal :: !Principal
  , activityTicketExpiresAt :: !UTCTime
  , activityDeliveryExpiresAt :: !UTCTime
  }

-- Pending activity tickets by identifier, with an expiry index so issue and
-- consume cost O(log n) and only expired tickets are visited when pruning.
data ActivityTickets = ActivityTickets
  { pendingActivity :: !(Map Text ActivityTicket)
  , activityExpiry :: !(Set (UTCTime, Text))
  }

emptyActivityTickets :: ActivityTickets
emptyActivityTickets = ActivityTickets Map.empty Set.empty

-- Remove expired tickets; Nothing when none has expired (no write needed).
pruneActivityTickets :: UTCTime -> ActivityTickets -> Maybe ActivityTickets
pruneActivityTickets now pending = case Set.lookupMin pending.activityExpiry of
  Just (expiresAt, _) | expiresAt <= now ->
    let (expired, live) = Set.spanAntitone (\(at, _) -> at <= now) pending.activityExpiry
    in Just (ActivityTickets (foldr (Map.delete . snd) pending.pendingActivity (Set.toList expired)) live)
  _ -> Nothing

-- Nothing reserves a bounded slot during the WebSocket handshake. The slot is
-- never a delivery target until its connection has been registered.
data ActivityClient = ActivityClient
  { activityConnection :: !(Maybe WS.Connection)
  , activityNative :: !NativeConnection
  , activityPrincipal :: !Principal
  , activityExpiresAt :: !UTCTime
  , activitySendLock :: !(MVar ())
  , activityRetired :: !(TVar Bool)
  }

-- | Create a fresh (empty) WebSocket state whose delivery shares the main
-- pool passed to the middleware and dispatcher.
newWSState :: IO WSState
newWSState = newWSStateUsing Nothing

-- | Create a fresh (empty) WebSocket state whose leased delivery turns and
-- change-stream dispatcher use the given pool. Pre-admission authorization
-- stays on the main pool. The caller destroys this pool only after the
-- dispatcher has stopped and the WebSocket drain was clean.
--
-- Delivery has one lease per delivery-pool connection except one reserved
-- for the dispatcher, and at least four, so healthy turns never wait for a
-- delivery connection while fewer than every lease is held by stuck work.
newWSStateWithDeliveryPool :: Pool Hasql.Connection -> IO WSState
newWSStateWithDeliveryPool = newWSStateUsing . Just

newWSStateUsing :: Maybe (Pool Hasql.Connection) -> IO WSState
newWSStateUsing delivery = do
  leases <- case delivery of
    Nothing -> pure defaultDeliveryLeases
    Just deliveryPool -> (\metrics -> max defaultDeliveryLeases (metrics.maxConnections - 1)) <$> getPoolMetricsFor deliveryPool
  transport <- newNativeTransportWithLeases leases
  WSState <$> newTVarIO Map.empty <*> newTVarIO Map.empty <*> newTVarIO Map.empty
    <*> newTVarIO Map.empty <*> newTVarIO emptyActivityTickets <*> newTVarIO Map.empty <*> pure transport
    <*> nativeTelemetry transport <*> pure delivery

-- | The separate delivery pool, if this state has one.
deliveryPoolOf :: WSState -> Maybe (Pool Hasql.Connection)
deliveryPoolOf st = st.deliveryPool

-- | The pool for delivery work: the state's delivery pool, otherwise the
-- given main pool.
deliveryPoolFor :: Pool Hasql.Connection -> WSState -> Pool Hasql.Connection
deliveryPoolFor pool st = fromMaybe pool st.deliveryPool

withNativeWebSocketSettings :: WSState -> Settings -> (Settings -> IO a) -> IO a
withNativeWebSocketSettings st = withNativeTransportSettings st.nativeTransport

runPlainWithNativeWebSockets :: WSState -> Settings -> Wai.Application -> IO ()
runPlainWithNativeWebSockets st = runNativePlain st.nativeTransport

beginWebSocketShutdown :: WSState -> IO ()
beginWebSocketShutdown st = beginNativeShutdown st.nativeTransport

awaitWebSocketDrain :: WSState -> Int -> IO ()
awaitWebSocketDrain st = awaitNativeDrain st.nativeTransport

-- | Number of currently connected clients.
connectionCount :: WSState -> IO Int
connectionCount st = Map.size <$> readTVarIO st.connections

-- | Send noteworthy transport events to the server log. See
-- docs/reference/websocket-telemetry.md.
attachWebSocketTelemetryLog :: WSState -> FastLogger -> IO ()
attachWebSocketTelemetryLog st = attachTelemetryLog st.telemetry

-- | Emit pending events and stop logging; call before releasing the log.
detachWebSocketTelemetryLog :: WSState -> IO ()
detachWebSocketTelemetryLog st = detachTelemetryLog st.telemetry

-- | Emit pending telemetry log events. The change-stream dispatcher calls
-- this on every turn.
flushWebSocketTelemetry :: WSState -> IO ()
flushWebSocketTelemetry st = flushTelemetryLog st.telemetry

-- | Flush pending events, then read bounded outcome counters and
-- occupancy. Reading never reclaims retained delivery work.
readWebSocketTelemetry :: WSState -> IO WebSocketTelemetrySnapshot
readWebSocketTelemetry st = do
  flushTelemetryLog st.telemetry
  stats <- readOutcomeStats st.telemetry
  gauges <- nativeGauges st.nativeTransport
  canonical <- Map.elems <$> readTVarIO st.canonicalConnections
  cycles <- catMaybes <$> mapM (readTVarIO . (.canonicalClientReplayCycle)) canonical
  pending <- length . filter isJust <$> mapM (readTVarIO . (.canonicalClientReplay)) canonical
  activity <- Map.size . Map.filter (isJust . (.activityConnection)) <$> readTVarIO st.activityConnections
  legacy <- Map.size <$> readTVarIO st.connections
  dropped <- droppedLogEvents st.telemetry
  now <- getMonotonicTimeNSec
  pure WebSocketTelemetrySnapshot
    { outcomes = stats
    , delivery = gauges
    , canonicalClients = length canonical
    , activityClients = activity
    , legacyClients = legacy
    , replaysPending = pending
    , oldestPendingReplayMs = if null cycles then Nothing
        else Just (fromIntegral (elapsedNanos (minimum (map snd cycles)) now) / 1e6)
    , droppedEvents = dropped
    }

recordUpgrade :: WSState -> TicketKind -> UpgradeOutcome -> Word64 -> IO ()
recordUpgrade st kind outcome started = do
  now <- getMonotonicTimeNSec
  gauges <- nativeGauges st.nativeTransport
  recordTelemetry st.telemetry (UpgradeRecord kind outcome (elapsedNanos started now) gauges)

openReplayCycle :: CanonicalClient -> ReplayPhase -> Word64 -> STM.STM ()
openReplayCycle client phase now = modifyTVar' client.canonicalClientReplayCycle $ \case
  Nothing -> Just (phase, now)
  open -> open

-- Each replay cycle is recorded once, by whichever outcome closes it first.
closeReplayCycle :: WSState -> CanonicalClient -> ReplayOutcome -> IO ()
closeReplayCycle st client outcome = do
  now <- getMonotonicTimeNSec
  closed <- STM.atomically $ STM.swapTVar client.canonicalClientReplayCycle Nothing
  recordReplayCycle st closed outcome now

recordReplayCycle :: WSState -> Maybe (ReplayPhase, Word64) -> ReplayOutcome -> Word64 -> IO ()
recordReplayCycle st closed outcome now = forM_ closed $ \(phase, started) ->
  recordTelemetry st.telemetry (ReplayRecord phase outcome (elapsedNanos started now))

replayFailure :: ChangeStreamError -> ReplayOutcome
replayFailure ResyncUnauthorized = ReplayRevoked
replayFailure _ = ReplayResyncRequired

------------------------------------------------------------------------
-- Connection management
------------------------------------------------------------------------

addConnection :: WSState -> NativeConnection -> WS.Connection -> Maybe Principal -> WorkspaceSubscription -> Bool -> Bool -> IO UUID
addConnection st native conn principal subscription receivesGlobalGroupEvents receivesWorkspaceAdminEvents = do
  connId <- UUIDv4.nextRandom
  let client = WSClient
        { clientConnection = conn
        , clientNative = native
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
broadcastLegacyOutbox :: Pool Hasql.Connection -> WSState -> OutboxRecord -> IO ()
broadcastLegacyOutbox pool st record = legacyOutboxActions (deliveryPoolFor pool st) st [record] >>= runDeliveryBatch

legacyOutboxActions :: Pool Hasql.Connection -> WSState -> [OutboxRecord] -> IO [IO ()]
legacyOutboxActions pool st records = do
  clients <- Map.toList <$> readTVarIO st.connections
  pure [deliver identifier client | (identifier, client) <- clients]
  where
    deliver identifier client = case filter (\record -> legacyVisible record.outboxEnvelope client) records of
      [] -> pure ()
      visible -> runClientTurn client.clientNative retire $ \turn -> send turn visible
      where
        retire = removeConnection st identifier >> abortNativeConnection client.clientNative
        send _ [] = pure ()
        send turn (record:rest) = do
          yielding <- deliveryShouldYield turn
          -- Legacy has no durable continuation. Retire rather than silently
          -- leave a live compatibility socket behind an incomplete batch.
          if yielding then retire else do
            allowed <- case (client.clientPrincipal, client.clientSubscription) of
              (Just principal, SubscribeWorkspace workspace) -> scopeAuthorized pool principal (WorkspaceScope workspace)
              (Just principal, SubscribeAllWorkspaces) -> pure (isTrusted principal)
              (Nothing, _) -> pure True
            if allowed then do
              WS.sendTextData client.clientConnection (encode (legacyProjection record.outboxEnvelope))
              send turn rest
            else retire

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

createActivityTicket :: WSState -> Principal -> IO (Maybe WebSocketTicketResponse)
createActivityTicket st = createActivityTicketWithTtl st 300

-- | Activity tickets and clients are not capped (2026-11-17 user decision);
-- expired tickets are pruned through their expiry index. Issuance fails only
-- once WebSocket admission has closed for shutdown.
createActivityTicketWithTtl :: WSState -> NominalDiffTime -> Principal -> IO (Maybe WebSocketTicketResponse)
createActivityTicketWithTtl st requestedTtl principal = do
  now <- getCurrentTime
  ticketId <- UUID.toText <$> UUIDv4.nextRandom
  open <- nativeAdmissionOpen st.nativeTransport
  let deliveryExpires = addUTCTime (max 0 (min 300 requestedTtl)) now
      expires = min deliveryExpires (addUTCTime ticketTtlSeconds now)
  STM.atomically $ do
    stored <- STM.readTVar st.activityTickets
    let pending = fromMaybe stored (pruneActivityTickets now stored)
    if not open
      then mapM_ (STM.writeTVar st.activityTickets) (pruneActivityTickets now stored) >> pure Nothing
      else do
        STM.writeTVar st.activityTickets (ActivityTickets
          (Map.insert ticketId (ActivityTicket principal expires deliveryExpires) pending.pendingActivity)
          (Set.insert (expires, ticketId) pending.activityExpiry))
        pure (Just WebSocketTicketResponse {ticket=ticketId,expiresAt=expires})

-- | One use: an unexpired ticket is removed and returned. Writes only when a
-- ticket is consumed or expired tickets are pruned.
consumeActivityTicket :: WSState -> Text -> IO (Maybe ActivityTicket)
consumeActivityTicket st identifier = do
  now <- getCurrentTime
  STM.atomically $ do
    stored <- STM.readTVar st.activityTickets
    let pruned = pruneActivityTickets now stored
        pending = fromMaybe stored pruned
    case Map.lookup identifier pending.pendingActivity of
      Just ticket -> do
        STM.writeTVar st.activityTickets (ActivityTickets (Map.delete identifier pending.pendingActivity)
          (Set.delete (ticket.activityTicketExpiresAt, identifier) pending.activityExpiry))
        pure (Just ticket)
      Nothing -> mapM_ (STM.writeTVar st.activityTickets) pruned >> pure Nothing

reserveActivityClient :: WSState -> NativeConnection -> ActivityTicket -> IO (UUID, ActivityClient)
reserveActivityClient st native ticket = do
  identifier <- UUIDv4.nextRandom
  lock <- newMVar ()
  retired <- newTVarIO False
  let client=ActivityClient Nothing native ticket.activityTicketPrincipal ticket.activityDeliveryExpiresAt lock retired
  STM.atomically $ STM.modifyTVar' st.activityConnections (Map.insert identifier client)
  pure (identifier,client)

removeActivityClient :: WSState -> UUID -> IO ()
removeActivityClient st identifier = STM.atomically $ modifyTVar' st.activityConnections (Map.delete identifier)

retireActivityClient :: ActivityClient -> IO ()
retireActivityClient client = STM.atomically $ STM.writeTVar client.activityRetired True

activityPrincipalValid :: Pool Hasql.Connection -> Principal -> IO Bool
activityPrincipalValid _ principal | isTrusted principal = pure True
activityPrincipalValid pool principal = case principal.authority of
  PrincipalGrantUser user -> isJust <$> Auth.getUserGrants pool user
  _ -> pure False

activityBeforeDeadline :: ActivityClient -> IO Bool
activityBeforeDeadline client = (< client.activityExpiresAt) <$> getCurrentTime

-- Authorization queries may themselves cross the delivery deadline. The last
-- clock check belongs immediately before each frame, not once per batch.
activityMaySend :: Pool Hasql.Connection -> ActivityClient -> IO Bool
activityMaySend pool client = do
  valid <- activityPrincipalValid pool client.activityPrincipal
  retired <- readTVarIO client.activityRetired
  beforeDeadline <- activityBeforeDeadline client
  pure (valid && not retired && beforeDeadline)

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
  | Wai.pathInfo req == ["api", "v1", "ws"] = do
      started <- getMonotonicTimeNSec
      case requestBotIdentity (Wai.requestHeaders req) of
        Left message -> do
          let upgrade outcome = when (isWebSocketsReq req) $ recordUpgrade st TicketUnresolved outcome started
          received <- respond (Wai.responseLBS status400 [(hContentType, "application/json")]
            (encode (object ["error" .= ("invalid_bot_identity" :: Text), "message" .= message])))
            `onException` upgrade UpgradeFailed
          upgrade UpgradeRejectedInvalidIdentity
          pure received
        Right _ -> websocketsOr WS.defaultConnectionOptions (wsApp authCfg pool st started) app req respond
  | otherwise = app req respond

data HandshakeStage = Handshaking | HandshakeRejected | HandshakeAccepted !Word64

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
wsApp :: AuthConfig -> Pool Hasql.Connection -> WSState -> Word64 -> WS.ServerApp
wsApp authCfg pool st started pending = do
  kindRef <- newIORef TicketUnresolved
  stageRef <- newIORef Handshaking
  let decided stage outcome = do
        writeIORef stageRef stage
        kind <- readIORef kindRef
        recordUpgrade st kind outcome started
      acceptPending = do
        conn <- WS.acceptRequest pending
        acceptedAt <- getMonotonicTimeNSec
        decided (HandshakeAccepted acceptedAt) UpgradeAccepted
        pure conn
      rejectWith outcome response = response >> decided HandshakeRejected outcome
  -- The upgrade is recorded once at its decision; an accepted connection is
  -- recorded again when its application ends.
  mask $ \restore -> do
    result <- try @SomeException (restore (route (writeIORef kindRef) acceptPending rejectWith))
    ended <- getMonotonicTimeNSec
    kind <- readIORef kindRef
    readIORef stageRef >>= \case
      Handshaking -> recordUpgrade st kind UpgradeFailed started
      HandshakeRejected -> pure ()
      HandshakeAccepted acceptedAt -> do
        gauges <- nativeGauges st.nativeTransport
        recordTelemetry st.telemetry $ ConnectionRecord kind
          (either (const ConnectionFailed) (const ConnectionClosed) result) (elapsedNanos acceptedAt ended) gauges
    either throwIO pure result
  where
   route classify acceptPending rejectWith = case ticketFromRequest (WS.pendingRequest pending) of
    -- Canonical tickets are consumed before legacy tickets in every mode.  A
    -- local server must exercise the same one-use canonical hand-off rather
    -- than silently treating a canonical reconnect as legacy traffic.
    Just ticketText -> do
      canonical <- consumeCanonicalTicket st (TE.decodeUtf8Lenient ticketText)
      case canonical of
        Just canonicalTicket -> classify TicketCanonical >> acceptCanonical canonicalTicket
        Nothing -> consumeActivityTicket st (TE.decodeUtf8Lenient ticketText) >>= \case
          Just activityTicket -> classify TicketActivity >> acceptActivity activityTicket
          Nothing -> acceptLegacyTicket ticketText
    Nothing -> classify TicketNone >> acceptWithoutTicket
    where
    -- Pre-admission authorization uses the main pool; leased delivery turns
    -- use the delivery pool.
    delivery = deliveryPoolFor pool st
    rejectUnauthorized = rejectWith UpgradeRejectedUnauthorized (WS.rejectRequest pending "Unauthorized")

    acceptLegacyTicket ticketText
      | authCfg.mode /= AuthModeDeployed = rejectUnauthorized
      | otherwise = do
          mTicket <- consumeTicket st (TE.decodeUtf8Lenient ticketText)
          case mTicket of
            Just ticket -> do
              classify TicketLegacy
              let (receivesGlobalGroupEvents, subscription) = ticketSubscription ticket
              accept (Just ticket.ticketPrincipal) subscription receivesGlobalGroupEvents ticket.ticketReceivesWorkspaceAdminEvents
            Nothing -> rejectUnauthorized

    acceptWithoutTicket
      | authCfg.mode == AuthModeDeployed = rejectUnauthorized
      | otherwise = case resolveLocalWebSocketAccess authCfg (TE.decodeUtf8Lenient <$> tokenFromRequest (WS.pendingRequest pending)) of
          Just (principal, subscription) -> accept principal subscription True True
          Nothing -> rejectUnauthorized

    rejectUnavailable = rejectWith UpgradeRejectedUnavailable $ WS.rejectRequestWith pending WS.defaultRejectRequest
      { WS.rejectCode=503, WS.rejectMessage="Service Unavailable", WS.rejectHeaders=[("Retry-After","1")] }

    withPhysical use = currentNativeConnection st.nativeTransport >>= \case
      Nothing -> rejectUnavailable
      Just native -> do
        admitted <- reservePhysicalConnection native
        if admitted then use native else rejectUnavailable

    accept principal subscription receivesGlobalGroupEvents receivesWorkspaceAdminEvents = do
      allowed <- case (principal, subscription) of
        (Just actor, SubscribeWorkspace workspace) -> scopeAuthorized pool actor (WorkspaceScope workspace)
        _ -> pure True
      if not allowed then rejectUnauthorized else
        withPhysical $ \native -> do
          conn <- installControlledWriter native <$> acceptPending
          connId <- addConnection st native conn principal subscription receivesGlobalGroupEvents receivesWorkspaceAdminEvents
          WS.withPingThread conn 30 (pure ()) $
            sinkMessages conn
              `finally` removeConnection st connId

    -- A canonical ticket is never routed through the legacy in-memory
    -- broadcast map.  It first drains the retained durable outbox and only
    -- then acknowledges catch-up with an opaque replacement token.
    acceptCanonical canonicalTicket = do
      allowed <- scopeAuthorized pool canonicalTicket.canonicalPrincipal canonicalTicket.canonicalScope
      if not allowed then rejectUnauthorized else
       withPhysical $ \native -> do
        conn <- installControlledWriter native <$> acceptPending
        bracket
          (addCanonicalConnection st native conn canonicalTicket.canonicalPrincipal canonicalTicket.canonicalScope
            canonicalTicket.canonicalResumeToken canonicalTicket.canonicalResumeTtl canonicalTicket.canonicalResumeExpiresAt)
          (uncurry (endCanonicalConnection st)) $ \(connId, client) -> do
            let serve = do
                  withLiveCanonicalClient st connId client $ \turn ->
                    advanceCanonicalClientUnlocked delivery st connId client (deliveryShouldYield turn)
                  WS.withPingThread conn 30 (pure ()) $
                    withAsync (expireCanonicalConnection st connId client) $ \_ ->
                      sinkMessages conn
                waitForRetirement driver = STM.atomically $
                  (True <$ STM.readTMVar client.canonicalClientRetired)
                    `STM.orElse` (False <$ waitSTM driver)
            mask $ \restore -> withAsync (restore serve) $ \driver ->
              (do
                retired <- restore (waitForRetirement driver)
                when retired $ abortNativeConnection native)
              `onException` abortNativeConnection native

    acceptActivity activityTicket = do
      allowed <- activityPrincipalValid pool activityTicket.activityTicketPrincipal
      if not allowed then rejectUnauthorized else
       withPhysical $ \native -> do
        (identifier,reservation) <- reserveActivityClient st native activityTicket
        flip finally
            (withMVar reservation.activitySendLock $ \_ -> retireActivityClient reservation >> removeActivityClient st identifier) $ do
            conn <- installControlledWriter native <$> acceptPending
            let client=reservation {activityConnection=Just conn}
            -- Independent expiry output wakes native reads through the close
            -- handshake. Keep the physical admission until the owning
            -- application actually finishes, including for an expired
            -- non-acknowledging peer.
            withAsync (expireActivity conn client) $ \_ -> do
              withLiveActivityClient client $ \_ -> do
                STM.atomically $ modifyTVar' st.activityConnections (Map.insert identifier client)
                live <- activityMaySend delivery client
                when live $ sendFrameUnlocked conn (object ["type" .= ("session_activity_ready" :: Text),"schema_version" .= (1::Int)])
              WS.withPingThread conn 30 (pure ()) (sinkMessages conn)

    expireActivity conn client = do
      now <- getCurrentTime
      threadDelay (floor (max 0 (diffUTCTime client.activityExpiresAt now)*1000000))
      withLiveActivityClient client $ \_ -> do
          retireActivityClient client
          sendFrameUnlocked conn (object ["type" .= ("session_activity_expired" :: Text),"schema_version" .= (1::Int)])
          sendOwnedClose client.activityNative conn ("activity capability expired" :: Text)

addCanonicalConnection :: WSState -> NativeConnection -> WS.Connection -> Principal -> ChangeScope -> ResumeToken -> NominalDiffTime -> UTCTime -> IO (UUID, CanonicalClient)
addCanonicalConnection st native conn principal scope token resumeTtl expiresAt = do
  connId <- UUIDv4.nextRandom
  bearer <- newTVarIO token
  replay <- newTVarIO (Just (newCanonicalReplay token True))
  sendLock <- newMVar ()
  retired <- STM.newEmptyTMVarIO
  replayStarted <- getMonotonicTimeNSec
  replayCycle <- newTVarIO (Just (ReplayInitial, replayStarted))
  let client = CanonicalClient conn native principal scope bearer replay resumeTtl expiresAt sendLock retired replayCycle
  STM.atomically $ modifyTVar' st.canonicalConnections (Map.insert connId client)
  pure (connId, client)

removeCanonicalConnection :: WSState -> UUID -> IO ()
removeCanonicalConnection st connId =
  STM.atomically $ modifyTVar' st.canonicalConnections (Map.delete connId)

-- A replay still open when its connection ends was retired by the server
-- (its delivery turn, a native abort or shutdown) or ended by the peer.
endCanonicalConnection :: WSState -> UUID -> CanonicalClient -> IO ()
endCanonicalConnection st connId client = do
  removeCanonicalConnection st connId
  retired <- STM.atomically (not <$> STM.isEmptyTMVar client.canonicalClientRetired)
  aborted <- nativeConnectionClosing client.canonicalClientNative
  closeReplayCycle st client (if retired || aborted then ReplayRetired else ReplayDisconnected)

-- Unregister the exact client and wake its owning application atomically.
-- This path needs no successful write to end the transport.
retireCanonicalConnection :: WSState -> UUID -> CanonicalClient -> IO ()
retireCanonicalConnection st connId client = STM.atomically $ do
  modifyTVar' st.canonicalConnections (Map.delete connId)
  void $ STM.tryPutTMVar client.canonicalClientRetired ()

runClientTurn :: NativeConnection -> IO () -> (DeliveryTurn -> IO ()) -> IO ()
runClientTurn native cleanup action = handleCanonicalDispatchFailure cleanup $ do
  outcome <- withDeliveryTurn native action
  case outcome of
    DeliveryCompleted () -> pure ()
    DeliveryRetired -> cleanup

withLiveActivityClient :: ActivityClient -> (DeliveryTurn -> IO ()) -> IO ()
withLiveActivityClient client action =
  runClientTurn client.activityNative
    (retireActivityClient client >> abortNativeConnection client.activityNative) $ \turn ->
      withMVar client.activitySendLock (const (action turn))

withLiveCanonicalClient :: WSState -> UUID -> CanonicalClient -> (DeliveryTurn -> IO ()) -> IO ()
withLiveCanonicalClient st connId client action =
  runClientTurn client.canonicalClientNative (retireCanonicalConnection st connId client) $ \turn ->
    withMVar client.canonicalClientSendLock $ \_ -> do
      live <- Map.member connId <$> readTVarIO st.canonicalConnections
      when live (action turn)

-- Successful terminal delivery retains the peer Close handshake. A failed
-- control or Close write is handled by the surrounding delivery turn.
closeCanonicalConnectionUnlocked :: WSState -> UUID -> CanonicalClient -> ReplayOutcome -> Value -> Text -> IO ()
closeCanonicalConnectionUnlocked st connId client outcome frame reason =
  (do
    closeReplayCycle st client outcome
    removeCanonicalConnection st connId
    sendFrameUnlocked client.canonicalClientConnection frame
    sendOwnedClose client.canonicalClientNative client.canonicalClientConnection reason)
  `onException` retireCanonicalConnection st connId client

audienceFor :: Principal -> ChangeAudience
audienceFor principal = case principal.authority of
  PrincipalGrantUser userId -> AuthenticatedAudience (UUID.toText userId) userId
  PrincipalSyntheticLocalSuperadmin -> TrustedAudience ("local:" <> principal.actorId)
  PrincipalNoAuthority -> TrustedAudience "invalid"

terminalFrame :: ChangeStreamError -> Text
terminalFrame ResyncUnauthorized = "access_revoked"
terminalFrame _ = "resync_required"

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
dispatchCanonicalOutbox mainPool st records = do
  let pool = deliveryPoolFor mainPool st
  activity <- activityOutboxActions pool st records
  canonical <- canonicalOutboxActions pool st records
  runDeliveryBatch (activity <> canonical)

-- Each population is disjoint: one physical owner receives one action for
-- this shared bounded batch, rather than a new turn for every scope/record.
dispatchOutboxBatch :: Pool Hasql.Connection -> WSState -> [OutboxRecord] -> IO ()
dispatchOutboxBatch mainPool st records = do
  let pool = deliveryPoolFor mainPool st
  legacy <- legacyOutboxActions pool st records
  activity <- activityOutboxActions pool st records
  canonical <- canonicalOutboxActions pool st records
  runDeliveryBatch (legacy <> activity <> canonical)

canonicalOutboxActions :: Pool Hasql.Connection -> WSState -> [OutboxRecord] -> IO [IO ()]
canonicalOutboxActions pool st records = do
  clients <- Map.toList <$> readTVarIO st.canonicalConnections
  pure [deliver connId client | (connId, client) <- clients]
  where
    targetedUsers = nub (concatMap targetedWorkspaceUsers records)
    activityWorkspaces = nub (catMaybes (map activityWorkspace records))
    deliver connId client = do
      let scopedRecords = filter (recordInScope client.canonicalClientScope) records
          covered = catMaybes (map activityWorkspace scopedRecords)
          hints = filter (`notElem` covered) activityWorkspaces
          accessTargets = case client.canonicalClientPrincipal.authority of
            PrincipalGrantUser user -> [workspace | (workspace, target) <- targetedUsers, user == target]
            _ -> []
      pending <- isJust <$> readTVarIO client.canonicalClientReplay
      unless (not pending && null scopedRecords && null hints && null accessTargets) $
        withLiveCanonicalClient st connId client $ \turn -> do
          stillLive <- applyAccessControlsUnlocked pool st connId client accessTargets
          when stillLive $ dispatchClient connId client scopedRecords hints (deliveryShouldYield turn)

    dispatchClient connId client scopedRecords hints shouldYield = do
      -- Selected-scope changes already replay canonically. Cross-scope hints
      -- wake bounded catalogue probes without widening that replay.
      sendActivityHintsUnlocked pool client shouldYield hints
      pending <- readTVarIO client.canonicalClientReplay
      case pending of
        Just replay -> do
          forM_ replay.replayWindow $ \window ->
            when (not replay.replayDirty && any ((> window.replayScanHighWater) . (.outboxCursor)) scopedRecords) $
              STM.atomically $ STM.writeTVar client.canonicalClientReplay (Just replay { replayDirty = True })
          advanceCanonicalClientUnlocked pool st connId client shouldYield
        Nothing -> do
          visible <- visibleOrYield client shouldYield scopedRecords
          if visible then do
            token <- readTVarIO client.canonicalClientToken
            now <- getMonotonicTimeNSec
            STM.atomically $ do
              STM.writeTVar client.canonicalClientReplay (Just (newCanonicalReplay token False))
              openReplayCycle client ReplayLive now
            advanceCanonicalClientUnlocked pool st connId client shouldYield
          else rebaseHidden connId client shouldYield scopedRecords

    rebaseHidden _ _ _ [] = pure ()
    rebaseHidden connId client shouldYield (record:rest) = do
      yielding <- shouldYield
      if yielding then do
        token <- readTVarIO client.canonicalClientToken
        now <- getMonotonicTimeNSec
        STM.atomically $ do
          STM.writeTVar client.canonicalClientReplay (Just (newCanonicalReplay token False))
          openReplayCycle client ReplayLive now
      else do
        stillLive <- rebaseCanonicalClientUnlocked pool st connId client record.outboxCursor
        when stillLive $ rebaseHidden connId client shouldYield rest

    visibleOrYield _ _ [] = pure False
    visibleOrYield client shouldYield (record:rest) = do
      yielding <- shouldYield
      if yielding then pure True else do
        visible <- isJust <$> projectRecord pool client.canonicalClientPrincipal client.canonicalClientScope record
        if visible then pure True else visibleOrYield client shouldYield rest

-- A dispatcher can schedule continuation work even when no outbox rows
-- arrived on its current turn.
canonicalReplayPending :: WSState -> IO Bool
canonicalReplayPending st = STM.atomically $ do
  clients <- Map.elems <$> STM.readTVar st.canonicalConnections
  anyM (fmap isJust . STM.readTVar . (.canonicalClientReplay)) clients

activityOutboxActions :: Pool Hasql.Connection -> WSState -> [OutboxRecord] -> IO [IO ()]
activityOutboxActions _ _ [] = pure []
activityOutboxActions pool st records = do
  clients <- Map.elems <$> readTVarIO st.activityConnections
  pure [deliver client conn | client <- clients, Just conn <- [client.activityConnection]]
  where
    deliver client conn = withLiveActivityClient client $ \turn -> do
        retired <- readTVarIO client.activityRetired
        valid <- activityPrincipalValid pool client.activityPrincipal
        now <- getCurrentTime
        if retired then pure () else if not valid || now >= client.activityExpiresAt
          then do
            retireActivityClient client
            beforeDeadline <- activityBeforeDeadline client
            when (not valid && beforeDeadline) $ sendFrameUnlocked conn (object ["type" .= ("access_revoked" :: Text),"schema_version" .= (1::Int)])
            sendOwnedClose client.activityNative conn ("activity capability retired" :: Text)
          else do
            -- A user may learn about changes to their own membership even
            -- after losing read access; another user's controls remain hidden.
            forDeliveryRecords turn (nub (concatMap targetedWorkspaceUsers records)) $ \(workspace,user) ->
              when (client.activityPrincipal.authority == PrincipalGrantUser user) $ do
                allowed <- scopeAuthorized pool client.activityPrincipal (WorkspaceScope workspace)
                live <- activityMaySend pool client
                when live $ sendFrameUnlocked conn (accessFrame (if allowed then "access_granted" else "access_revoked") workspace)
            forDeliveryRecords turn (nub (catMaybes (map activityWorkspace records))) $ \workspace -> do
              allowed <- scopeAuthorized pool client.activityPrincipal (WorkspaceScope workspace)
              live <- activityMaySend pool client
              when (allowed && live) $ sendFrameUnlocked conn $
                object ["type" .= ("session_activity_changed" :: Text),"schema_version" .= (1::Int),"workspace_id" .= workspace]

-- Hints are lossy wakeups, never cursor advancement or canonical activity facts.
-- Each dispatcher batch is bounded; retain no subscription or historical IDs.
-- Check current authority immediately before the serialized write. Mount and
-- reconnect always establish the corresponding canonical HTTP baseline.
sendActivityHintsUnlocked :: Pool Hasql.Connection -> CanonicalClient -> IO Bool -> [UUID] -> IO ()
sendActivityHintsUnlocked pool client shouldYield = send
  where
    send [] = pure ()
    send (workspace:rest) = do
      yielding <- shouldYield
      unless yielding $ do
        allowed <- scopeAuthorized pool client.canonicalClientPrincipal (WorkspaceScope workspace)
        remaining <- remainingCanonicalTtl client
        when (allowed && remaining > 0) $ sendFrameUnlocked client.canonicalClientConnection $
          object ["type" .= ("session_activity_changed" :: Text), "schema_version" .= (1 :: Int), "workspace_id" .= workspace]
        send rest

forDeliveryRecords :: DeliveryTurn -> [a] -> (a -> IO ()) -> IO ()
forDeliveryRecords turn records action = go records
  where
    go [] = pure ()
    go (record:rest) = do
      yielding <- deliveryShouldYield turn
      unless yielding (action record >> go rest)

activityWorkspace :: OutboxRecord -> Maybe UUID
activityWorkspace record = do
  workspace <- case lookupValue "workspace_id" record.outboxEnvelope of
    Just (String value) -> UUID.fromText value
    Nothing -> workspaceEntity
    Just Null -> workspaceEntity
    _ -> Nothing
  let membershipChanged = case lookupValue "invalidations" record.outboxEnvelope of
        Just (Array values) -> any ((== Just ("active_sessions:" <> UUID.toText workspace)) . jsonText "target") (foldr (:) [] values)
        _ -> False
      structural = entityType record.outboxEnvelope `elem` map Just ["workspace","project","task"]
  if membershipChanged || structural then Just workspace else Nothing
  where
    -- Workspace lifecycle records belong to the Global outbox and therefore
    -- have no workspace_id. Derive only this canonical UUID, never a project,
    -- task or arbitrary malformed scope, before the usual read authorization.
    workspaceEntity
      | entityType record.outboxEnvelope == Just "workspace" = entityId record.outboxEnvelope >>= UUID.fromText
      | otherwise = Nothing

rebaseCanonicalClientUnlocked :: Pool Hasql.Connection -> WSState -> UUID -> CanonicalClient -> Int64 -> IO Bool
rebaseCanonicalClientUnlocked pool st connId client hiddenThrough = do
  oldToken <- readTVarIO client.canonicalClientToken
  rebased <- rebaseResumeTokenAfterHidden pool client.canonicalClientResumeTtl client.canonicalClientScope (audienceFor client.canonicalClientPrincipal) oldToken hiddenThrough
  case rebased of
    Right token -> do
      STM.atomically $ STM.writeTVar client.canonicalClientToken token
      pure True
    Left err -> do
      closeReplayCycle st client (replayFailure err)
      sendFrameUnlocked client.canonicalClientConnection (object ["type" .= terminalFrame err, "schema_version" .= (1 :: Int)])
      retireCanonicalConnection st connId client
      pure False

-- The worker scans a shared durable outbox, but canonical replay is scoped.
-- Targeted access controls are intentionally considered separately above;
-- they never make the socket subscribe to the newly granted workspace.
recordInScope :: ChangeScope -> OutboxRecord -> Bool
recordInScope scope record = case (scope, jsonText "workspace_id" record.outboxEnvelope >>= UUID.fromText) of
  (WorkspaceScope workspace, Just recordWorkspace) -> workspace == recordWorkspace
  (GlobalScope, Nothing) -> True
  _ -> False

-- Run one page while holding the client's delivery lock. A turn can yield
-- between records without keeping the page or superseding the original token.
advanceCanonicalClientUnlocked :: Pool Hasql.Connection -> WSState -> UUID -> CanonicalClient -> IO Bool -> IO ()
advanceCanonicalClientUnlocked pool st connId client shouldYield = do
  pending <- readTVarIO client.canonicalClientReplay
  forM_ pending $ \replay -> do
    remaining <- remainingCanonicalTtl client
    if remaining <= 0 then expire else do
      yielding <- shouldYield
      unless yielding $ do
        pageResult <- readReplayScanPage pool scope audience replay.replayOriginalBearer replay.replayWindow
        case pageResult of
          Left err -> reportFailure err
          Right page -> do
            saveReplay replay page.replayScanPageWindow
            deliver replay page.replayScanPageWindow page.replayScanPageWatermark page.replayScanPageRecords
  where
    conn = client.canonicalClientConnection
    principal = client.canonicalClientPrincipal
    scope = client.canonicalClientScope
    audience = audienceFor principal
    expire = closeCanonicalConnectionUnlocked st connId client ReplayExpired canonicalExpiryFrame "canonical resume expired"
    reportFailure err = do
      closeReplayCycle st client (replayFailure err)
      sendFrameUnlocked conn (object ["type" .= terminalFrame err, "schema_version" .= (1 :: Int)])
      retireCanonicalConnection st connId client
    sendAuthorized frame after = do
      allowed <- scopeAuthorized pool principal scope
      remaining <- remainingCanonicalTtl client
      if not allowed then reportFailure ResyncUnauthorized
      else if remaining <= 0 then expire
      else sendFrameUnlocked conn frame >> after
    saveReplay replay window = STM.atomically $
      STM.writeTVar client.canonicalClientReplay (Just replay { replayWindow = Just window })
    deliver replay window watermark [] = do
      yielding <- shouldYield
      when (not yielding && window.replayScanCursor == window.replayScanHighWater) $
        finish replay window.replayScanHighWater watermark
    deliver replay window watermark (record:remaining) = do
      yielding <- shouldYield
      unless yielding $ do
        projected <- projectRecord pool principal scope record
        let next = window { replayScanCursor = record.outboxCursor }
            continue updated = do
              saveReplay updated next
              deliver updated next watermark remaining
        case projected of
          Nothing -> continue replay
          Just event -> sendAuthorized (object ["type" .= ("change" :: Text), "schema_version" .= (1 :: Int), "event" .= event]) $
            continue replay { replaySawVisible = True }
    finish replay highWater watermark = do
      let original = replay.replayOriginalBearer
          replacement = if highWater == watermark then original else replacementResumeToken scope audience original highWater
          settle result = case result of
            Left ResumeExpired -> expire
            Left err -> reportFailure err
            Right token -> do
              now <- getMonotonicTimeNSec
              closed <- STM.atomically $ do
                STM.writeTVar client.canonicalClientToken token
                STM.writeTVar client.canonicalClientReplay
                  (if replay.replayDirty then Just (newCanonicalReplay token False) else Nothing)
                closed <- STM.swapTVar client.canonicalClientReplayCycle Nothing
                when replay.replayDirty $ openReplayCycle client ReplayLive now
                pure closed
              recordReplayCycle st closed ReplayCompleted now
      if replay.replayInitialCheckpoint || replay.replaySawVisible
        then sendAuthorized (object
          [ "type" .= ("checkpoint" :: Text), "schema_version" .= (1 :: Int)
          , "resume_token" .= resumeText replacement, "catch_up" .= ("complete" :: Text) ]) $
          if highWater == watermark then settle (Right original) else
            acknowledgeReplayScan pool client.canonicalClientExpiresAt scope audience original highWater >>= settle
        else do
          remaining <- remainingCanonicalTtl client
          if remaining <= 0 then expire else
            rebaseResumeTokenAfterHidden pool client.canonicalClientResumeTtl scope audience original highWater >>= settle

-- | A connected canonical client has one fixed delivery deadline. Visible
-- activity may rotate its bearer for acknowledgement, but cannot prolong the
-- connection's capability lifetime; hidden activity keeps the bearer in place
-- and therefore cannot leak itself through a changed expiry cadence.
remainingCanonicalTtl :: CanonicalClient -> IO NominalDiffTime
remainingCanonicalTtl client = do
  now <- getCurrentTime
  pure $ max 0 (diffUTCTime client.canonicalClientExpiresAt now)

expireCanonicalConnection :: WSState -> UUID -> CanonicalClient -> IO ()
expireCanonicalConnection st connId client = do
  remaining <- remainingCanonicalTtl client
  waitForCanonicalExpiry remaining
  withLiveCanonicalClient st connId client $ \_ ->
    closeCanonicalConnectionUnlocked st connId client ReplayExpired canonicalExpiryFrame "canonical resume expired"

canonicalExpiryFrame :: Value
canonicalExpiryFrame = object ["type" .= ("resync_required" :: Text), "schema_version" .= (1 :: Int)]

-- 'threadDelay' is platform-bounded.  Long configured resume TTLs therefore
-- wait in bounded chunks instead of overflowing into an immediate expiry on
-- Windows. The connection owner cancels this scoped timer when it exits.
waitForCanonicalExpiry :: NominalDiffTime -> IO ()
waitForCanonicalExpiry remaining = when (remaining > 0) $ do
  let chunk = min remaining 60
  threadDelay (floor (chunk * 1000000))
  waitForCanonicalExpiry (remaining - chunk)

-- | Recover only synchronous delivery failures. Dispatcher cancellation is a
-- control-flow signal and must reach the supervising worker unchanged.
handleCanonicalDispatchFailure :: IO () -> IO () -> IO ()
handleCanonicalDispatchFailure cleanup action = action `catch` \err ->
  case fromException err :: Maybe SomeAsyncException of
    Just _ -> throwIO err
    Nothing | uncleanDelivery err -> throwIO err
            | otherwise -> cleanup

uncleanDelivery :: SomeException -> Bool
uncleanDelivery err
  | Just (_ :: DeliveryCleanupFailure) <- fromException err = True
  | otherwise = case fromException err of
    Just DeliveryUnconfirmed -> True
    Just DeliveryQuarantined -> True
    _ -> False

-- Membership invalidations carry a user-targeted catalogue/session audience.
-- Emit controls only to that user's already-authenticated canonical socket;
-- the frame contains the scope identifier but never membership row data.
applyAccessControlsUnlocked :: Pool Hasql.Connection -> WSState -> UUID -> CanonicalClient -> [UUID] -> IO Bool
applyAccessControlsUnlocked pool st connId client = go
  where
    go [] = pure True
    go (workspace:rest) = do
      allowed <- scopeAuthorized pool client.canonicalClientPrincipal (WorkspaceScope workspace)
      case client.canonicalClientScope of
        WorkspaceScope current | current == workspace && not allowed -> do
          closeCanonicalConnectionUnlocked st connId client ReplayRevoked (accessFrame "access_revoked" workspace) "access revoked"
          pure False
        WorkspaceScope current | current == workspace -> go rest
        _ -> do
          -- Cross-scope controls update the matching user's catalogue while
          -- the subscribed workspace continues to receive its own changes.
          sendFrameUnlocked client.canonicalClientConnection
            (accessFrame (if allowed then "access_granted" else "access_revoked") workspace)
          go rest

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
reauthorizeCanonicalConnections mainPool st = do
  let pool = deliveryPoolFor mainPool st
  activity <- Map.elems <$> readTVarIO st.activityConnections
  clients <- Map.toList <$> readTVarIO st.canonicalConnections
  let activityChecks =
        [ withLiveActivityClient client $ \_ -> do
            retired <- readTVarIO client.activityRetired
            valid <- activityPrincipalValid pool client.activityPrincipal
            when (not valid && not retired) $ do
              retireActivityClient client
              beforeDeadline <- activityBeforeDeadline client
              when beforeDeadline $ sendFrameUnlocked conn (object ["type" .= ("access_revoked" :: Text), "schema_version" .= (1 :: Int)])
              sendOwnedClose client.activityNative conn ("activity access revoked" :: Text)
        | client <- activity, Just conn <- [client.activityConnection] ]
      canonicalChecks =
        [ withLiveCanonicalClient st connId client $ \_ -> do
            -- One fresh check under the send lock and managed lease. Idle
            -- checks no longer query outside the delivery deadline and repeat
            -- a denied query a second time after locking.
            allowed <- scopeAuthorized pool client.canonicalClientPrincipal client.canonicalClientScope
            unless allowed $ closeCanonicalConnectionUnlocked st connId client ReplayRevoked
              (object ["type" .= ("access_revoked" :: Text), "schema_version" .= (1 :: Int)]) "access revoked"
        | (connId, client) <- clients ]
  runDeliveryBatch (activityChecks <> canonicalChecks)

-- Four short-lived producers share the immutable batch and the existing
-- bounded physical-client snapshot. There is no frame queue or client watcher.
-- A producer owns its managed turn; all producers must actually finish before
-- the dispatcher can acknowledge any of the batch's scopes.
runDeliveryBatch :: [IO ()] -> IO ()
runDeliveryBatch [] = pure ()
runDeliveryBatch actions = mask $ \restore -> do
  remaining <- newMVar actions
  start <- newEmptyMVar
  let produce = readMVar start >> loop
        where
          loop = do
            next <- modifyMVar remaining $ \case
              [] -> pure ([], Nothing)
              action:rest -> pure (rest, Just action)
            forM_ next (\action -> action >> loop)
      withProducers 0 owned use = use owned
      withProducers count owned use = withAsyncWithUnmask (\unmask -> unmask produce) $ \producer ->
        withProducers (count - 1) (producer:owned) use
      awaitAll [] = pure ()
      awaitAll owned = do
        (finished, result) <- waitAnyCatch owned
        either throwIO pure result
        awaitAll (filter ((/= asyncThreadId finished) . asyncThreadId) owned)
      cancelAndJoin producer = do
        result <- try @SomeAsyncException (cancel producer >> waitCatch producer)
        -- Further cancellation cannot abandon a retained producer handle.
        -- Its native turn keeps its original bounded cleanup deadline.
        either (const (cancelAndJoin producer)) pure result
  withProducers (min 4 (length actions)) [] $ \owned -> do
    putMVar start ()
    outcome <- try @SomeException (restore (awaitAll owned))
    case outcome of
      Right () -> pure ()
      Left primary -> do
        modifyMVar_ remaining (const (pure []))
        joined <- mapM cancelAndJoin owned
        case [failure | Left failure <- joined, uncleanDelivery failure] of
          cleanupFailure:_ -> throwIO (DeliveryCleanupFailure primary cleanupFailure)
          [] -> throwIO primary

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

subscriberEvent :: Value -> Value
subscriberEvent = \case
  Object fields -> Object (KeyMap.delete "cursor" fields)
  other -> other

resumeText :: ResumeToken -> Text
resumeText (ResumeToken value) = value

canonicalResumeTtlDefault :: NominalDiffTime
canonicalResumeTtlDefault = 82800


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
  localHumanPrincipal authCfg token <|> localBotPrincipal authCfg token <|> legacyPrincipal authCfg token

localHumanPrincipal :: AuthConfig -> Text -> Maybe Principal
localHumanPrincipal cfg token
  | Config.authLocalUserTokenMatches cfg token = Just Principal
      { actorType = ActorUser, actorId = "local-user", actorLabel = "Local User"
      , authority = PrincipalSyntheticLocalSuperadmin }
  | otherwise = Nothing

resolveLocalWebSocketAccess :: AuthConfig -> Maybe Text -> Maybe (Maybe Principal, WorkspaceSubscription)
resolveLocalWebSocketAccess authCfg mToken
  | authCfg.mode /= AuthModeLocal = Nothing
  | isJust (Config.localUserTokenConfigError authCfg) = Nothing
  | Just token <- mToken
  , Just principal <- authorizedTokenPrincipal authCfg token =
      Just (Just principal, SubscribeAllWorkspaces)
  | authStaticBearerEnabled authCfg = Nothing
  | isJust (Config.authLocalUserToken authCfg) = Nothing
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
