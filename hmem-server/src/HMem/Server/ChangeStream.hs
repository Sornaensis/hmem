-- | Supervised delivery and bounded retention for the durable change stream.
module HMem.Server.ChangeStream
  ( ChangeStreamWorker, ChangeStreamMaintenance(..)
  , startChangeStreamWorker, startChangeStreamWorkerWithStartupHook
  , readChangeStreamMaintenance, stopChangeStreamWorker
  , ChangeStreamDispatchEvent(..), startChangeStreamWorkerWithObserver
  , retentionScopeTurn, retentionDue
  ) where

import Control.Concurrent (MVar, ThreadId, forkIOWithUnmask, killThread, modifyMVar_, newMVar, newEmptyMVar, putMVar, readMVar, threadDelay)
import Control.Exception (SomeAsyncException, SomeException, catch, fromException, mask_, throwIO, try)
import Control.Monad (forM, when)
import Data.Int (Int64)
import Data.List (sort)
import Data.Pool (Pool)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Hasql.Connection qualified as Hasql
import System.IO (hPutStrLn, stderr)

import HMem.Config (ChangeStreamConfig(..))
import HMem.DB.ChangeStream (ChangeScope, OutboxRecord(..), OutboxBacklog(..), cleanupChangeStream, listDispatchScopes, listOutboxAfter, listRetentionScopes, outboxBacklog, pruneOutboxScopeBefore)
import HMem.DB.Pool (DBException(..))
import HMem.Server.WebSocket (WSState, dispatchOutboxBatch, canonicalReplayPending, flushWebSocketTelemetry, reauthorizeCanonicalConnections)
import HMem.Server.WebSocketDelivery (DeliveryFailure(..), DeliveryCleanupFailure(..))

data ChangeStreamMaintenance = ChangeStreamMaintenance
  { lastAttemptAt :: !(Maybe UTCTime)
  , lastSuccessAt :: !(Maybe UTCTime)
  , lastDeletedRows :: !Int64
  , backlog :: !(Maybe OutboxBacklog)
  , lastError :: !(Maybe String)
  , failureCount :: !Int64
  } deriving stock (Show, Eq)

-- The observer is a synchronous causal boundary for focused diagnostics and
-- tests, not a retained log or an alternate dispatch/acknowledgement path.
data ChangeStreamDispatchEvent
  = DispatchBatchRead ![(ChangeScope, Int64, Int)]
  -- | Rotated scopes whose committed counter was past their acknowledgement
  -- and which were therefore read; 'DispatchBatchRead' still lists every
  -- rotated scope, with zero records for an unread one.
  | DispatchScopesScanned ![ChangeScope]
  | DispatchBatchCommitted ![(ChangeScope, Int64)]
  | DispatchMaintenanceAttempt
  | DispatchIdleAuthorization !Word64
  | DispatchIdle
  | DispatchDeferred
  deriving stock (Show, Eq)

data ChangeStreamWorker = ChangeStreamWorker ThreadId (MVar (Either SomeException ())) (MVar ChangeStreamMaintenance)

readChangeStreamMaintenance :: ChangeStreamWorker -> IO ChangeStreamMaintenance
readChangeStreamMaintenance (ChangeStreamWorker _ _ status) = readMVar status

-- | No UTC wall-clock jump changes the elapsed maintenance cadence.
retentionDue :: Word64 -> Maybe Word64 -> Bool
retentionDue _ Nothing = True
retentionDue now (Just previous) = now >= previous && now - previous >= 60000000000

-- | Fair turns use the scope identity rather than a mutable list index. Each
-- transaction owns only one scope, so cross-scope locking cannot invert the
-- workspace UUID then global order used by authorization writers.
retentionScopeTurn :: Maybe ChangeScope -> [ChangeScope] -> [ChangeScope]
retentionScopeTurn previous scopes = take 4 $ case previous of
  Nothing -> ordered
  Just cursor -> filter (> cursor) ordered <> filter (<= cursor) ordered
  where ordered = sort scopes

startChangeStreamWorker :: Pool Hasql.Connection -> ChangeStreamConfig -> WSState -> IO ChangeStreamWorker
startChangeStreamWorker = startChangeStreamWorkerWithStartupHook (pure ())

startChangeStreamWorkerWithStartupHook :: IO () -> Pool Hasql.Connection -> ChangeStreamConfig -> WSState -> IO ChangeStreamWorker
startChangeStreamWorkerWithStartupHook startupHook = startObserved startupHook (const (pure ()))

startChangeStreamWorkerWithObserver :: (ChangeStreamDispatchEvent -> IO ()) -> Pool Hasql.Connection -> ChangeStreamConfig -> WSState -> IO ChangeStreamWorker
startChangeStreamWorkerWithObserver = startObserved (pure ())

startObserved :: IO () -> (ChangeStreamDispatchEvent -> IO ()) -> Pool Hasql.Connection -> ChangeStreamConfig -> WSState -> IO ChangeStreamWorker
startObserved startupHook observe pool config wsState = mask_ $ do
  finished <- newEmptyMVar
  status <- newMVar (ChangeStreamMaintenance Nothing Nothing 0 Nothing Nothing 0)
  worker <- forkIOWithUnmask $ \unmask -> do
    result <- try (unmask (startupHook >> loop status Map.empty Nothing Nothing Nothing Nothing False Nothing))
    putMVar finished result
  pure (ChangeStreamWorker worker finished status)
  where
    loop status acknowledgements lastMaintenance lastScope maintenanceDueAt dispatchCursor sweepBusy lastAuthorization = do
      -- Emit WebSocket telemetry recorded since the previous turn.
      flushWebSocketTelemetry wsState
      -- Maintenance and idle authority have their own elapsed-time cadence;
      -- neither is multiplied by the number of empty retained scope counters.
      monotonic <- getMonotonicTimeNSec
      (nextMaintenance, nextScope, nextMaintenanceDue) <-
        if maybe False (<= monotonic) maintenanceDueAt || retentionDue monotonic lastMaintenance
          then observe DispatchMaintenanceAttempt >> maintain status monotonic lastMaintenance lastScope
          else pure (lastMaintenance, lastScope, maintenanceDueAt)
      let maintenanceImmediate = maybe False (<= monotonic) nextMaintenanceDue
      authorizationNow <- getMonotonicTimeNSec
      let authorizationDue = maybe True (\previous -> authorizationNow >= previous && authorizationNow - previous >= 1000000000) lastAuthorization
      authorized <- trySynchronous $ when authorizationDue $ do
        observe (DispatchIdleAuthorization authorizationNow)
        reauthorizeCanonicalConnections pool wsState
      scopesResult <- trySynchronous (listDispatchScopes pool dispatchCursor)
      case (authorized, scopesResult) of
        (Right (), Right counted) -> do
          let scopes = map fst counted
          delivered <- trySynchronous $ do
            -- A scope whose committed counter has not passed its
            -- acknowledgement has no unacknowledged record: every committed
            -- record's cursor is at most the counter, and later commits get
            -- larger cursors. Skipping its read loses nothing.
            batches <- forM counted $ \(scope, committed) -> do
              let previous = Map.findWithDefault 0 scope acknowledgements
              if committed <= previous
                then pure (scope, previous, Nothing, [])
                else do
                  records <- listOutboxAfter pool scope previous dispatchScopeLimit
                  pure (scope, previous, Just committed, records)
            observe $ DispatchScopesScanned [scope | (scope, _, Just _, _) <- batches]
            observe $ DispatchBatchRead [(scope, previous, length records) | (scope, previous, _, records) <- batches]
            pending <- canonicalReplayPending wsState
            let records = concat [rows | (_, _, _, rows) <- batches]
            when (not (null records) || pending) $ dispatchOutboxBatch pool wsState records
            pure (batches, not (null records))
          let cursor = case reverse scopes of [] -> dispatchCursor; value:_ -> Just value
              wrapped = null scopes || case dispatchCursor of
                Nothing -> length scopes < 4
                Just previous -> any (<= previous) scopes
              nextAuthorization = if authorizationDue then Just authorizationNow else lastAuthorization
          case delivered of
            Left _ -> do
              -- A failing turn retains every acknowledgement. Back off errors
              -- at the former polling cadence rather than hot-spinning; this
              -- is distinct from the work-conserving successful-data path.
              observe DispatchDeferred
              threadDelay 1000000
              loop status acknowledgements nextMaintenance nextScope nextMaintenanceDue cursor True nextAuthorization
            Right (batches, busy) -> do
              let nextAcknowledgements = foldl advanceAcknowledgement acknowledgements batches
              -- Publish only after all managed producers actually completed.
              observe $ DispatchBatchCommitted [(scope, Map.findWithDefault 0 scope nextAcknowledgements) | scope <- scopes]
              pending <- canonicalReplayPending wsState
              when (wrapped && not sweepBusy && not busy && not pending && not maintenanceImmediate) $ do
                observe DispatchIdle
                threadDelay 1000000
              let nextBusy = if wrapped then busy else sweepBusy || busy
              loop status nextAcknowledgements nextMaintenance nextScope nextMaintenanceDue cursor nextBusy nextAuthorization
        _ -> do
          observe DispatchDeferred
          threadDelay 1000000
          loop status acknowledgements nextMaintenance nextScope nextMaintenanceDue dispatchCursor True lastAuthorization

    maintain status monotonic previous previousScope = do
      now <- getCurrentTime
      let cutoff = addUTCTime (negate (fromIntegral config.retentionSeconds)) now
      modifyMVar_ status $ \value -> pure value { lastAttemptAt = Just now }
      -- Catch only synchronous failures. Cancellation is never a successful
      -- cleanup result and must propagate through the worker's join.
      result <- trySynchronous $ do
        scopes <- listRetentionScopes pool previousScope
        let selected = retentionScopeTurn previousScope scopes
        counts <- mapM (\scope -> do
          result <- trySynchronous (pruneOutboxScopeBefore pool scope cutoff 1000)
          pure (scope, result)) selected
        cleaned <- trySynchronous (cleanupChangeStream pool now)
        remaining <- trySynchronous (outboxBacklog pool cutoff)
        pure (selected, counts, cleaned, remaining)
      case result of
        Left err -> do
          recordFailure status ("retention_scope_scan_failed:" <> failureKind err)
          pure (Just monotonic, previousScope, Just (monotonic + 1000000000))
        Right (selected, counts, cleaned, measured) -> do
          before <- readMVar status
          let errors = ["scope_failed:" <> show scope <> ":" <> failureKind err | (scope, Left err) <- counts]
                <> either (\err -> ["token_cleanup_failed:" <> failureKind err]) (const []) cleaned
                <> either (\err -> ["backlog_measurement_failed:" <> failureKind err]) (const []) measured
              removed = sum [count | (_, Right count) <- counts]
              cursor = case reverse selected of [] -> previousScope; value:_ -> Just value
              remaining = either (const Nothing) Just measured
              pending = maybe False ((> 0) . (.expiredRowsLowerBound)) remaining
          modifyMVar_ status $ \value -> pure value
            { lastSuccessAt = if null errors then Just now else value.lastSuccessAt
            , lastDeletedRows = removed, backlog = remaining
            , lastError = if null errors then Nothing else Just (unwords errors)
            , failureCount = value.failureCount + fromIntegral (length errors) }
          -- Report periodic idle checks and transitions, not an unbounded
          -- stream of repeated per-scope errors while backlog drains.
          when (retentionDue monotonic previous
              || before.lastError /= (if null errors then Nothing else Just (unwords errors))
              || (not pending && maybe False ((> 0) . (.expiredRowsLowerBound)) before.backlog)) $
            hPutStrLn stderr ("change_stream_retention deleted=" <> show removed
              <> " expired_rows_lower_bound=" <> show (fmap (.expiredRowsLowerBound) remaining)
              <> " oldest_expired_age_seconds=" <> show (remaining >>= fmap (diffUTCTime now) . (.oldestExpiredAt))
              <> " errors=" <> show errors)
          -- Even a busy or locked scope advances the rotation; another scope
          -- gets its next turn. Failures retain their floor and remain backlog.
          -- Backlog is only a lower bound, not proof that its prefix can be
          -- deleted now. Continue actual deletion progress immediately; pace
          -- a locked/retained prefix or failed measurement independently of
          -- successful delivery and replay, which remain work-conserving.
          let nextDue = if not pending && null errors then Nothing else
                Just (monotonic + if removed > 0 then 0 else 1000000000)
          pure (Just monotonic, cursor, nextDue)

    recordFailure status reason = do
      modifyMVar_ status $ \value -> pure value
        { lastError = Just reason, failureCount = value.failureCount + 1, backlog = Nothing }
      hPutStrLn stderr ("change_stream_retention error=" <> reason)

dispatchScopeLimit :: Int
dispatchScopeLimit = 1000

-- | Applied only after a successfully dispatched turn. A short read returned
-- every retained record up to the counter observed before it, so a fully
-- pruned or caught-up scope is acknowledged through that counter and skipped
-- until a later commit. A full read acknowledges only its last record.
advanceAcknowledgement :: Map.Map ChangeScope Int64 -> (ChangeScope, Int64, Maybe Int64, [OutboxRecord]) -> Map.Map ChangeScope Int64
advanceAcknowledgement current (_, _, Nothing, _) = current
advanceAcknowledgement current (scope, previous, Just committed, records) =
  let latest = case reverse records of
        [] -> previous
        record:_ -> record.outboxCursor
      through = if length records < dispatchScopeLimit then max latest committed else latest
  in Map.insertWith max scope through current

stopChangeStreamWorker :: ChangeStreamWorker -> IO ()
stopChangeStreamWorker (ChangeStreamWorker worker finished _) = do
  killThread worker
  readMVar finished >>= \case
    Left failure | isUncleanDelivery failure -> throwIO failure
    _ -> pure ()

trySynchronous :: IO a -> IO (Either SomeException a)
trySynchronous action = (Right <$> action) `catch` \err ->
  case fromException err :: Maybe SomeAsyncException of
    Just _ -> throwIO err
    Nothing | isUncleanDelivery err -> throwIO err
            | otherwise -> pure (Left err)

isUncleanDelivery :: SomeException -> Bool
isUncleanDelivery err
  | Just (_ :: DeliveryCleanupFailure) <- fromException err = True
  | otherwise = case fromException err of
    Just DeliveryUnconfirmed -> True
    Just DeliveryQuarantined -> True
    _ -> False

-- Keep raw connection/server errors out of logs while distinguishing the
-- common bounded deferrals from an unavailable database or worker failure.
failureKind :: SomeException -> String
failureKind err = case fromException err of
  Just DBStatementTimeout -> "statement_timeout"
  Just (DBOtherError message) | "lock timeout" `T.isInfixOf` message -> "lock_timeout"
  Just (_ :: DBException) -> "database_error"
  Nothing -> "worker_error"
