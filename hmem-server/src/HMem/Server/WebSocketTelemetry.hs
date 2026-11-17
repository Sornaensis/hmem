-- | Bounded WebSocket transport telemetry: fixed-label outcome counters,
-- monotonic durations and occupancy gauges. Labels are total functions of
-- the enumerations below; tickets, frames, principals and workspace or
-- connection identifiers are never recorded. See
-- docs/reference/websocket-telemetry.md.
module HMem.Server.WebSocketTelemetry
  ( WebSocketTelemetry
  , newWebSocketTelemetry
  , attachTelemetryLog
  , detachTelemetryLog
  , flushTelemetryLog
    -- * Records
  , TelemetryRecord(..)
  , TicketKind(..)
  , UpgradeOutcome(..)
  , ConnectionOutcome(..)
  , ReplayPhase(..)
  , ReplayOutcome(..)
  , TurnOutcome(..)
  , WriteOutcome(..)
  , CleanupOutcome(..)
  , recordTelemetry
  , recordReclaimsSTM
  , awaitingLease
  , leaseWaiterCount
  , elapsedNanos
    -- * Snapshots
  , DeliveryGauges(..)
  , OutcomeKey
  , OutcomeStats(..)
  , WebSocketTelemetrySnapshot(..)
  , readOutcomeStats
  , droppedLogEvents
  ) where

import Control.Concurrent (MVar, modifyMVar_, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (SomeAsyncException, SomeException, finally, fromException, throwIO, try)
import Control.Monad (forM_, when)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Pair)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Log.FastLogger (FastLogger, toLogStr)

-- The log action is held under a lock so detaching waits for any emission
-- in progress; the flag lets STM recorders skip queueing when unattached.
data WebSocketTelemetry = WebSocketTelemetry
  { sink :: !(MVar (Maybe FastLogger))
  , attached :: !(TVar Bool)
  , stats :: !(TVar (Map OutcomeKey OutcomeStats))
  , pending :: !(TVar Pending)
  , dropped :: !(TVar Int)
  , waiters :: !(TVar Int)
  }

-- Events awaiting emission, newest first. A reclamation is observed inside
-- an STM transaction, so its quarantine age is measured when it is emitted.
data Pending = Pending !Int ![PendingEvent]

data PendingEvent
  = PendingRecord !UTCTime !TelemetryRecord
  | PendingReclaim !Word64

-- | Pending log events are bounded; overflow is counted, never retained.
pendingLimit :: Int
pendingLimit = 256

newWebSocketTelemetry :: IO WebSocketTelemetry
newWebSocketTelemetry =
  WebSocketTelemetry <$> newMVar Nothing <*> newTVarIO False <*> newTVarIO Map.empty <*> newTVarIO (Pending 0 [])
    <*> newTVarIO 0 <*> newTVarIO 0

-- | Route noteworthy events to the server log as one JSON line each.
attachTelemetryLog :: WebSocketTelemetry -> FastLogger -> IO ()
attachTelemetryLog telemetry logAction = modifyMVar_ telemetry.sink $ \_ -> do
  atomically $ writeTVar telemetry.attached True
  pure (Just logAction)

-- | Emit pending events, then stop logging. Call before releasing the log.
detachTelemetryLog :: WebSocketTelemetry -> IO ()
detachTelemetryLog telemetry = do
  flushTelemetryLog telemetry
  modifyMVar_ telemetry.sink $ \_ -> do
    atomically $ writeTVar telemetry.attached False
    pure Nothing

data TicketKind = TicketCanonical | TicketActivity | TicketLegacy | TicketNone | TicketUnresolved
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data UpgradeOutcome
  = UpgradeAccepted
  | UpgradeRejectedUnauthorized
  | UpgradeRejectedUnavailable
  | UpgradeRejectedInvalidIdentity
  | UpgradeFailed
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data ConnectionOutcome = ConnectionClosed | ConnectionFailed
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data ReplayPhase = ReplayInitial | ReplayLive
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data ReplayOutcome
  = ReplayCompleted
  | ReplayExpired
  | ReplayResyncRequired
  | ReplayRevoked
  | ReplayRetired
  | ReplayDisconnected
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data TurnOutcome
  = TurnCompleted
  | TurnFailed
  | TurnRetiredWriteFailed
  | TurnRetiredWriteTimeout
  | TurnRetiredDeadline
  | TurnRetiredClosing
  | TurnInterrupted
  | TurnRefused
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data WriteOutcome = WriteCompleted | WriteFailed | WriteTimedOut | WriteInterrupted
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data CleanupOutcome = CleanupJoined | CleanupUnfinished | CleanupAbortFailed | CleanupFailed
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Occupancy of the shared delivery hub. Quarantined turns stay counted
-- until the next hub admission or release reclaims them.
data DeliveryGauges = DeliveryGauges
  { physicalAdmitted :: !Int
  , physicalRetained :: !Int
  , leasesInUse :: !Int
  , leaseCapacity :: !Int
  , leasesQuarantined :: !Int
  , leaseWaiters :: !Int
  , oldestQuarantineMs :: !(Maybe Double)
  , admissionShut :: !Bool
  , admissionPoisoned :: !Bool
  } deriving stock (Show, Eq)

-- Durations are monotonic nanoseconds.
data TelemetryRecord
  = UpgradeRecord !TicketKind !UpgradeOutcome !Word64 !DeliveryGauges
  | ConnectionRecord !TicketKind !ConnectionOutcome !Word64 !DeliveryGauges
  | ReplayRecord !ReplayPhase !ReplayOutcome !Word64
  -- Lease wait, then turn duration from admission to its outcome.
  | TurnRecord !TurnOutcome !Word64 !Word64
  | WriteRecord !WriteOutcome !Word64
  | CleanupRecord !CleanupOutcome !Word64
  | ReclaimRecord !Word64

-- | (event, kind or phase, outcome); the middle label is empty when the
-- event has no kind.
type OutcomeKey = (Text, Text, Text)

-- | @timed@ counts recorded durations. It trails @count@ only for a
-- reclamation whose age has not been measured yet.
data OutcomeStats = OutcomeStats
  { count :: !Int
  , timed :: !Int
  , totalMs :: !Double
  , maxMs :: !Double
  } deriving stock (Show, Eq)

data WebSocketTelemetrySnapshot = WebSocketTelemetrySnapshot
  { outcomes :: !(Map OutcomeKey OutcomeStats)
  , delivery :: !DeliveryGauges
  , canonicalClients :: !Int
  , activityClients :: !Int
  , legacyClients :: !Int
  , replaysPending :: !Int
  , oldestPendingReplayMs :: !(Maybe Double)
  , droppedEvents :: !Int
  } deriving stock (Show, Eq)

elapsedNanos :: Word64 -> Word64 -> Word64
elapsedNanos start end = if end <= start then 0 else end - start

nanosToMs :: Word64 -> Double
nanosToMs nanos = fromIntegral nanos / 1e6

-- Routine successes are counted but not logged.
logged :: TelemetryRecord -> Bool
logged = \case
  UpgradeRecord{} -> True
  ConnectionRecord{} -> True
  ReplayRecord phase outcome _ -> phase == ReplayInitial || outcome /= ReplayCompleted
  TurnRecord outcome _ _ -> outcome `notElem` [TurnCompleted, TurnRefused]
  WriteRecord{} -> False
  CleanupRecord outcome _ -> outcome /= CleanupJoined
  ReclaimRecord{} -> True

recordTelemetry :: WebSocketTelemetry -> TelemetryRecord -> IO ()
recordTelemetry telemetry record = do
  stamp <- if logged record then Just <$> getCurrentTime else pure Nothing
  atomically $ do
    forM_ (recordStats record) $ \(key, nanos) -> modifyTVar' telemetry.stats (addTimed key nanos)
    forM_ stamp $ \at -> do
      attached <- readTVar telemetry.attached
      when attached $ enqueue telemetry (PendingRecord at record)

-- | Count reclaimed quarantined turns, given when each was quarantined.
recordReclaimsSTM :: WebSocketTelemetry -> [Word64] -> STM ()
recordReclaimsSTM telemetry quarantinedAt = forM_ quarantinedAt $ \since -> do
  modifyTVar' telemetry.stats (Map.insertWith combine reclaimKey (OutcomeStats 1 0 0 0))
  enqueue telemetry (PendingReclaim since)

-- | Count a producer waiting for one of the shared delivery leases.
awaitingLease :: WebSocketTelemetry -> IO a -> IO a
awaitingLease telemetry wait = do
  atomically $ modifyTVar' telemetry.waiters (+ 1)
  wait `finally` atomically (modifyTVar' telemetry.waiters (subtract 1))

leaseWaiterCount :: WebSocketTelemetry -> IO Int
leaseWaiterCount telemetry = readTVarIO telemetry.waiters

enqueue :: WebSocketTelemetry -> PendingEvent -> STM ()
enqueue telemetry event = do
  Pending size events <- readTVar telemetry.pending
  if size >= pendingLimit
    then modifyTVar' telemetry.dropped (+ 1)
    else writeTVar telemetry.pending (Pending (size + 1) (event : events))

reclaimKey :: OutcomeKey
reclaimKey = ("websocket_reclaim", "", "reclaimed")

recordStats :: TelemetryRecord -> [(OutcomeKey, Word64)]
recordStats = \case
  UpgradeRecord kind outcome nanos _ -> [(("websocket_upgrade", ticketLabel kind, upgradeLabel outcome), nanos)]
  ConnectionRecord kind outcome nanos _ -> [(("websocket_connection", ticketLabel kind, connectionLabel outcome), nanos)]
  ReplayRecord phase outcome nanos -> [(("websocket_replay", phaseLabel phase, replayLabel outcome), nanos)]
  TurnRecord outcome wait nanos ->
    [ (("websocket_lease", "", if outcome == TurnRefused then "refused" else "admitted"), wait)
    , (("websocket_turn", "", turnLabel outcome), nanos) ]
  WriteRecord outcome nanos -> [(("websocket_write", "", writeLabel outcome), nanos)]
  CleanupRecord outcome nanos -> [(("websocket_cleanup", "", cleanupLabel outcome), nanos)]
  ReclaimRecord nanos -> [(reclaimKey, nanos)]

addTimed :: OutcomeKey -> Word64 -> Map OutcomeKey OutcomeStats -> Map OutcomeKey OutcomeStats
addTimed key nanos = Map.insertWith combine key (OutcomeStats 1 1 ms ms)
  where ms = nanosToMs nanos

combine :: OutcomeStats -> OutcomeStats -> OutcomeStats
combine new old = OutcomeStats
  { count = old.count + new.count
  , timed = old.timed + new.timed
  , totalMs = old.totalMs + new.totalMs
  , maxMs = max old.maxMs new.maxMs
  }

-- | Measure pending reclamation ages and emit pending events to the
-- attached log. Synchronous log failures are counted as dropped events.
flushTelemetryLog :: WebSocketTelemetry -> IO ()
flushTelemetryLog telemetry = do
  events <- atomically $ do
    Pending _ events <- readTVar telemetry.pending
    writeTVar telemetry.pending (Pending 0 [])
    pure (reverse events)
  when (not (null events)) $ do
    now <- getMonotonicTimeNSec
    wall <- getCurrentTime
    let resolve = \case
          PendingRecord at record -> (at, record)
          PendingReclaim since -> (wall, ReclaimRecord (elapsedNanos since now))
        resolved = map resolve events
        ages = [nanos | (_, ReclaimRecord nanos) <- resolved]
    atomically $ forM_ ages $ \nanos ->
      modifyTVar' telemetry.stats (Map.adjust (\old -> combine (OutcomeStats 0 1 (nanosToMs nanos) (nanosToMs nanos)) old) reclaimKey)
    withMVar telemetry.sink $ \logger -> forM_ logger $ \logAction -> forM_ resolved $ \(at, record) -> do
      result <- try @SomeException (logAction (toLogStr (Aeson.encode (renderRecord at record)) <> "\n"))
      case result of
        Right () -> pure ()
        Left failure
          | isJust (fromException failure :: Maybe SomeAsyncException) -> throwIO failure
          | otherwise -> atomically (modifyTVar' telemetry.dropped (+ 1))

readOutcomeStats :: WebSocketTelemetry -> IO (Map OutcomeKey OutcomeStats)
readOutcomeStats telemetry = readTVarIO telemetry.stats

droppedLogEvents :: WebSocketTelemetry -> IO Int
droppedLogEvents telemetry = readTVarIO telemetry.dropped

renderRecord :: UTCTime -> TelemetryRecord -> Aeson.Value
renderRecord at record = Aeson.object $
  [ "timestamp" .= T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" at)
  , "level" .= (if warning record then "warn" else "info" :: Text)
  ] <> fields
  where
    fields = case record of
      UpgradeRecord kind outcome nanos gauges ->
        [ "event" .= ("websocket_upgrade" :: Text), "ticket_kind" .= ticketLabel kind
        , "outcome" .= upgradeLabel outcome, "handshake_ms" .= nanosToMs nanos ] <> gaugeFields gauges
      ConnectionRecord kind outcome nanos gauges ->
        [ "event" .= ("websocket_connection" :: Text), "ticket_kind" .= ticketLabel kind
        , "outcome" .= connectionLabel outcome, "lifetime_ms" .= nanosToMs nanos ] <> gaugeFields gauges
      ReplayRecord phase outcome nanos ->
        [ "event" .= ("websocket_replay" :: Text), "phase" .= phaseLabel phase
        , "outcome" .= replayLabel outcome, "replay_ms" .= nanosToMs nanos ]
      TurnRecord outcome wait nanos ->
        [ "event" .= ("websocket_turn" :: Text), "outcome" .= turnLabel outcome
        , "lease_wait_ms" .= nanosToMs wait, "turn_ms" .= nanosToMs nanos ]
      WriteRecord outcome nanos ->
        [ "event" .= ("websocket_write" :: Text), "outcome" .= writeLabel outcome, "write_ms" .= nanosToMs nanos ]
      CleanupRecord outcome nanos ->
        [ "event" .= ("websocket_cleanup" :: Text), "outcome" .= cleanupLabel outcome, "cleanup_ms" .= nanosToMs nanos ]
      ReclaimRecord nanos ->
        [ "event" .= ("websocket_reclaim" :: Text), "outcome" .= ("reclaimed" :: Text), "quarantine_ms" .= nanosToMs nanos ]

gaugeFields :: DeliveryGauges -> [Pair]
gaugeFields gauges =
  [ "admitted" .= gauges.physicalAdmitted, "leases" .= gauges.leasesInUse, "lease_capacity" .= gauges.leaseCapacity
  , "lease_waiters" .= gauges.leaseWaiters, "quarantined" .= gauges.leasesQuarantined ]

warning :: TelemetryRecord -> Bool
warning = \case
  UpgradeRecord _ outcome _ _ -> outcome `elem` [UpgradeRejectedUnavailable, UpgradeFailed]
  ConnectionRecord _ outcome _ _ -> outcome == ConnectionFailed
  ReplayRecord _ outcome _ -> outcome == ReplayRetired
  TurnRecord outcome _ _ -> outcome `notElem` [TurnCompleted, TurnRefused]
  WriteRecord outcome _ -> outcome /= WriteCompleted
  CleanupRecord outcome _ -> outcome /= CleanupJoined
  ReclaimRecord _ -> False

ticketLabel :: TicketKind -> Text
ticketLabel = \case
  TicketCanonical -> "canonical"
  TicketActivity -> "activity"
  TicketLegacy -> "legacy"
  TicketNone -> "none"
  TicketUnresolved -> "unresolved"

upgradeLabel :: UpgradeOutcome -> Text
upgradeLabel = \case
  UpgradeAccepted -> "accepted"
  UpgradeRejectedUnauthorized -> "rejected_unauthorized"
  UpgradeRejectedUnavailable -> "rejected_unavailable"
  UpgradeRejectedInvalidIdentity -> "rejected_invalid_identity"
  UpgradeFailed -> "failed"

connectionLabel :: ConnectionOutcome -> Text
connectionLabel = \case
  ConnectionClosed -> "closed"
  ConnectionFailed -> "failed"

phaseLabel :: ReplayPhase -> Text
phaseLabel = \case
  ReplayInitial -> "initial"
  ReplayLive -> "live"

replayLabel :: ReplayOutcome -> Text
replayLabel = \case
  ReplayCompleted -> "completed"
  ReplayExpired -> "expired"
  ReplayResyncRequired -> "resync_required"
  ReplayRevoked -> "revoked"
  ReplayRetired -> "retired"
  ReplayDisconnected -> "disconnected"

turnLabel :: TurnOutcome -> Text
turnLabel = \case
  TurnCompleted -> "completed"
  TurnFailed -> "failed"
  TurnRetiredWriteFailed -> "retired_write_failed"
  TurnRetiredWriteTimeout -> "retired_write_timeout"
  TurnRetiredDeadline -> "retired_deadline"
  TurnRetiredClosing -> "retired_closing"
  TurnInterrupted -> "interrupted"
  TurnRefused -> "refused"

writeLabel :: WriteOutcome -> Text
writeLabel = \case
  WriteCompleted -> "completed"
  WriteFailed -> "failed"
  WriteTimedOut -> "timed_out"
  WriteInterrupted -> "interrupted"

cleanupLabel :: CleanupOutcome -> Text
cleanupLabel = \case
  CleanupJoined -> "joined"
  CleanupUnfinished -> "unfinished"
  CleanupAbortFailed -> "abort_failed"
  CleanupFailed -> "failed"
