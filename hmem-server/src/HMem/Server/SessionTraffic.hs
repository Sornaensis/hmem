-- | Bounded session-capture traffic telemetry: fixed-label request outcome
-- counters for append, heartbeat and prefix-proof routes, internal
-- attribution of unknown proof operations, and server proof compute time.
-- Workspace, session, operation, principal and audience identifiers are never
-- recorded. See docs/reference/session-traffic-telemetry.md.
module HMem.Server.SessionTraffic
  ( SessionTraffic
  , newSessionTraffic
  , attachTrafficLog
  , detachTrafficLog
  , flushTrafficLog
    -- * Records
  , TrafficRoute(..)
  , TrafficOutcome(..)
  , MissReason(..)
  , ProofResult(..)
  , recordRequest
  , recordComputeSTM
    -- * Snapshots
  , RequestKey
  , RequestStats(..)
  , ComputeKey
  , ComputeStats(..)
  , SessionTrafficSnapshot(..)
  , readSessionTraffic
    -- * Labels
  , routeLabel
  , outcomeLabel
  , reasonLabel
  , resultLabel
  , sizeBucket
  ) where

import Control.Concurrent (MVar, modifyMVar_, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Control.Monad (forM_, when)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import System.Log.FastLogger (FastLogger, toLogStr)

-- The log action is held under a lock so detaching waits for any emission
-- in progress; the flag lets STM recorders skip queueing when unattached.
data SessionTraffic = SessionTraffic
  { sink :: !(MVar (Maybe FastLogger))
  , attached :: !(TVar Bool)
  , requests :: !(TVar (Map RequestKey RequestStats))
  , compute :: !(TVar (Map ComputeKey ComputeStats))
  , pending :: !(TVar Pending)
  , dropped :: !(TVar Int)
  }

data Pending = Pending !Int ![(UTCTime, Event)]

data Event
  = RequestEvent !TrafficRoute !TrafficOutcome
  | ComputeEvent !ProofResult !Int64 !NominalDiffTime

-- | Pending log events are bounded; overflow is counted, never retained.
pendingLimit :: Int
pendingLimit = 256

newSessionTraffic :: IO SessionTraffic
newSessionTraffic =
  SessionTraffic <$> newMVar Nothing <*> newTVarIO False <*> newTVarIO Map.empty <*> newTVarIO Map.empty
    <*> newTVarIO (Pending 0 []) <*> newTVarIO 0

-- | Route noteworthy events to the server log as one JSON line each.
attachTrafficLog :: SessionTraffic -> FastLogger -> IO ()
attachTrafficLog traffic logAction = modifyMVar_ traffic.sink $ \_ -> do
  atomically $ writeTVar traffic.attached True
  pure (Just logAction)

-- | Emit pending events, then stop logging. Call before releasing the log.
detachTrafficLog :: SessionTraffic -> IO ()
detachTrafficLog traffic = do
  flushTrafficLog traffic
  modifyMVar_ traffic.sink $ \_ -> do
    atomically $ writeTVar traffic.attached False
    pure Nothing

data TrafficRoute = RouteAppend | RouteHeartbeat | RouteProofSubmit | RouteProofLookup
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Why this process could not return a proof operation. The public response
-- is the same @verification_unknown@ for every reason.
data MissReason
  = MissUnknownOperation  -- ^ never retained by this process, which is older than an operation lifetime
  | MissUnknownSinceStart -- ^ never retained by this process, which started within an operation lifetime
  | MissExpired           -- ^ terminal result removed after its five-minute lifetime
  | MissEvicted           -- ^ terminal result evicted oldest-first on a later admission
  | MissTargetMismatch    -- ^ retained, but bound to another workspace, session or audience
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data TrafficOutcome
  = OutcomeCommitted
  | OutcomeReplayed
  | OutcomeAccepted
  | OutcomePending
  | OutcomeVerified
  | OutcomeMismatch
  | OutcomeFailed
  | OutcomeUnknown !MissReason
  | OutcomeSessionMissing
  | OutcomeUnauthorized
  | OutcomeInvalid
  | OutcomeConflict
  | OutcomeTerminated
  | OutcomeEvidenceUnavailable
  | OutcomeBusy
  | OutcomeUnavailable
  | OutcomeError
  deriving stock (Show, Eq, Ord)

data ProofResult
  = ResultVerified
  | ResultMismatch
  | ResultFailedDeadline
  | ResultFailedUnavailable
  | ResultFailedEvidence
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | (route, outcome, reason); reason is empty unless the outcome is
-- @verification_unknown@.
type RequestKey = (Text, Text, Text)

-- | @bytes@ counts newly committed append bytes.
data RequestStats = RequestStats { count :: !Int, bytes :: !Int64 }
  deriving stock (Show, Eq)

-- | (result, prefix size bucket).
type ComputeKey = (Text, Text)

-- | Server compute time runs from admission to the terminal result.
data ComputeStats = ComputeStats
  { proofs :: !Int, prefixBytes :: !Int64, totalMs :: !Double, maxMs :: !Double }
  deriving stock (Show, Eq)

data SessionTrafficSnapshot = SessionTrafficSnapshot
  { requestStats :: !(Map RequestKey RequestStats)
  , computeStats :: !(Map ComputeKey ComputeStats)
  , droppedEvents :: !Int
  } deriving stock (Show, Eq)

-- | Count one handled request. Noteworthy outcomes are logged when attached.
recordRequest :: SessionTraffic -> TrafficRoute -> TrafficOutcome -> Int64 -> IO ()
recordRequest traffic route outcome committed = do
  stamp <- if loggedRequest route outcome then Just <$> getCurrentTime else pure Nothing
  atomically $ do
    modifyTVar' traffic.requests
      (Map.insertWith combineRequest (requestKey route outcome) (RequestStats 1 (max 0 committed)))
    forM_ stamp $ \at -> enqueue traffic at (RequestEvent route outcome)
  flushTrafficLog traffic

-- | Record one terminal proof result inside the publishing transaction.
recordComputeSTM :: SessionTraffic -> UTCTime -> ProofResult -> Int64 -> NominalDiffTime -> STM ()
recordComputeSTM traffic at result prefix elapsed = do
  let ms = max 0 (realToFrac elapsed * 1000)
  modifyTVar' traffic.compute
    (Map.insertWith combineCompute (resultLabel result, sizeBucket prefix) (ComputeStats 1 (max 0 prefix) ms ms))
  enqueue traffic at (ComputeEvent result prefix elapsed)

enqueue :: SessionTraffic -> UTCTime -> Event -> STM ()
enqueue traffic at event = do
  attachedNow <- readTVar traffic.attached
  when attachedNow $ do
    Pending size events <- readTVar traffic.pending
    if size >= pendingLimit
      then modifyTVar' traffic.dropped (+ 1)
      else writeTVar traffic.pending (Pending (size + 1) ((at, event) : events))

combineRequest :: RequestStats -> RequestStats -> RequestStats
combineRequest new old = RequestStats (old.count + new.count) (old.bytes + new.bytes)

combineCompute :: ComputeStats -> ComputeStats -> ComputeStats
combineCompute new old = ComputeStats
  { proofs = old.proofs + new.proofs
  , prefixBytes = old.prefixBytes + new.prefixBytes
  , totalMs = old.totalMs + new.totalMs
  , maxMs = max old.maxMs new.maxMs
  }

-- Routine successes are counted but not logged; append and heartbeat
-- statuses are already visible in the request log.
loggedRequest :: TrafficRoute -> TrafficOutcome -> Bool
loggedRequest route outcome = case route of
  RouteAppend -> False
  RouteHeartbeat -> False
  _ -> outcome `notElem` [OutcomePending, OutcomeVerified]

-- | Emit pending events to the attached log. Synchronous log failures are
-- counted as dropped events; counters are unaffected.
flushTrafficLog :: SessionTraffic -> IO ()
flushTrafficLog traffic = do
  events <- atomically $ do
    Pending _ events <- readTVar traffic.pending
    writeTVar traffic.pending (Pending 0 [])
    pure (reverse events)
  when (not (null events)) $
    withMVar traffic.sink $ \logger -> forM_ logger $ \logAction -> forM_ events $ \(at, event) -> do
      result <- try @SomeException (logAction (toLogStr (Aeson.encode (renderEvent at event)) <> "\n"))
      case result of
        Right () -> pure ()
        Left failure
          | isJust (fromException failure :: Maybe SomeAsyncException) -> throwIO failure
          | otherwise -> atomically (modifyTVar' traffic.dropped (+ 1))

readSessionTraffic :: SessionTraffic -> IO SessionTrafficSnapshot
readSessionTraffic traffic = do
  flushTrafficLog traffic
  atomically $ SessionTrafficSnapshot <$> readTVar traffic.requests <*> readTVar traffic.compute <*> readTVar traffic.dropped

renderEvent :: UTCTime -> Event -> Aeson.Value
renderEvent at = \case
  RequestEvent route outcome -> Aeson.object $
    [ "timestamp" .= timestamp, "level" .= level (warnOutcome outcome)
    , "event" .= ("session_traffic" :: Text), "route" .= routeLabel route, "outcome" .= outcomeLabel outcome ]
    <> [ "reason" .= reasonLabel reason | OutcomeUnknown reason <- [outcome] ]
  ComputeEvent result prefix elapsed -> Aeson.object
    [ "timestamp" .= timestamp, "level" .= level (result `notElem` [ResultVerified])
    , "event" .= ("session_prefix_proof" :: Text), "outcome" .= resultLabel result
    , "prefix" .= sizeBucket prefix, "compute_ms" .= (max 0 (realToFrac elapsed * 1000) :: Double) ]
  where
    timestamp = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" at)
    level warn = if warn then "warn" else "info" :: Text

warnOutcome :: TrafficOutcome -> Bool
warnOutcome = \case
  OutcomeUnknown _ -> True
  OutcomeMismatch -> True
  OutcomeFailed -> True
  OutcomeBusy -> True
  OutcomeUnavailable -> True
  OutcomeError -> True
  _ -> False

requestKey :: TrafficRoute -> TrafficOutcome -> RequestKey
requestKey route outcome = (routeLabel route, outcomeLabel outcome, case outcome of
  OutcomeUnknown reason -> reasonLabel reason
  _ -> "")

routeLabel :: TrafficRoute -> Text
routeLabel = \case
  RouteAppend -> "append"
  RouteHeartbeat -> "heartbeat"
  RouteProofSubmit -> "proof_submit"
  RouteProofLookup -> "proof_lookup"

outcomeLabel :: TrafficOutcome -> Text
outcomeLabel = \case
  OutcomeCommitted -> "committed"
  OutcomeReplayed -> "replayed"
  OutcomeAccepted -> "accepted"
  OutcomePending -> "pending"
  OutcomeVerified -> "verified"
  OutcomeMismatch -> "mismatch"
  OutcomeFailed -> "failed"
  OutcomeUnknown _ -> "verification_unknown"
  OutcomeSessionMissing -> "session_missing"
  OutcomeUnauthorized -> "unauthorized"
  OutcomeInvalid -> "invalid"
  OutcomeConflict -> "conflict"
  OutcomeTerminated -> "terminated"
  OutcomeEvidenceUnavailable -> "evidence_unavailable"
  OutcomeBusy -> "busy"
  OutcomeUnavailable -> "unavailable"
  OutcomeError -> "error"

reasonLabel :: MissReason -> Text
reasonLabel = \case
  MissUnknownOperation -> "unknown_operation"
  MissUnknownSinceStart -> "unknown_since_start"
  MissExpired -> "expired"
  MissEvicted -> "evicted"
  MissTargetMismatch -> "target_mismatch"

resultLabel :: ProofResult -> Text
resultLabel = \case
  ResultVerified -> "verified"
  ResultMismatch -> "mismatch"
  ResultFailedDeadline -> "failed_deadline"
  ResultFailedUnavailable -> "failed_unavailable"
  ResultFailedEvidence -> "failed_evidence_unavailable"

-- | Coarse prefix size label; exact session sizes are never logged.
sizeBucket :: Int64 -> Text
sizeBucket size
  | size <= mib = "le_1mib"
  | size <= 16 * mib = "le_16mib"
  | size <= 128 * mib = "le_128mib"
  | size <= 1024 * mib = "le_1gib"
  | otherwise = "gt_1gib"
  where mib = 1048576
