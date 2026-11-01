-- | Supervised delivery and bounded retention for the durable change stream.
module HMem.Server.ChangeStream
  ( ChangeStreamWorker, ChangeStreamMaintenance(..)
  , startChangeStreamWorker, startChangeStreamWorkerWithStartupHook
  , readChangeStreamMaintenance, stopChangeStreamWorker
  , retentionScopeTurn, retentionDue
  ) where

import Control.Concurrent (MVar, ThreadId, forkIOWithUnmask, killThread, modifyMVar_, newMVar, newEmptyMVar, putMVar, readMVar, threadDelay)
import Control.Exception (SomeAsyncException, SomeException, catch, fromException, mask_, throwIO, try)
import Control.Monad (foldM, when)
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
import HMem.DB.ChangeStream (ChangeScope, OutboxRecord(..), OutboxBacklog(..), cleanupChangeStream, listOutboxAfter, listOutboxScopes, listRetentionScopes, outboxBacklog, pruneOutboxScopeBefore)
import HMem.DB.Pool (DBException(..))
import HMem.Server.WebSocket (WSState, broadcastLegacyOutbox, dispatchCanonicalOutbox, reauthorizeCanonicalConnections)

data ChangeStreamMaintenance = ChangeStreamMaintenance
  { lastAttemptAt :: !(Maybe UTCTime)
  , lastSuccessAt :: !(Maybe UTCTime)
  , lastDeletedRows :: !Int64
  , backlog :: !(Maybe OutboxBacklog)
  , lastError :: !(Maybe String)
  , failureCount :: !Int64
  } deriving stock (Show, Eq)

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
startChangeStreamWorkerWithStartupHook startupHook pool config wsState = mask_ $ do
  finished <- newEmptyMVar
  status <- newMVar (ChangeStreamMaintenance Nothing Nothing 0 Nothing Nothing 0)
  worker <- forkIOWithUnmask $ \unmask -> do
    result <- try (unmask (startupHook >> loop status Map.empty Nothing Nothing False))
    putMVar finished result
  pure (ChangeStreamWorker worker finished status)
  where
    loop status acknowledgements lastMaintenance lastScope continuing = do
      -- Maintain before delivery: a slow delivery pass must not count as a
      -- minute of successful retention, or hide its failure with a zero count.
      monotonic <- getMonotonicTimeNSec
      (nextMaintenance, nextScope, pending) <-
        if continuing || retentionDue monotonic lastMaintenance
          then maintain status monotonic lastMaintenance lastScope
          else pure (lastMaintenance, lastScope, False)
      scopes <- catchSynchronous (listOutboxScopes pool) (pure [])
      acknowledgements' <- foldM dispatchScope acknowledgements scopes
      threadDelay 1000000
      loop status acknowledgements' nextMaintenance nextScope pending

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
          pure (Just monotonic, previousScope, False)
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
          pure (Just monotonic, cursor, pending)

    recordFailure status reason = do
      modifyMVar_ status $ \value -> pure value
        { lastError = Just reason, failureCount = value.failureCount + 1, backlog = Nothing }
      hPutStrLn stderr ("change_stream_retention error=" <> reason)

    dispatchScope acknowledgements scope = do
      let acknowledged = Map.findWithDefault 0 scope acknowledgements
      delivered <- catchSynchronous (do
        records <- listOutboxAfter pool scope acknowledged 1000
        mapM_ (broadcastLegacyOutbox wsState) records
        dispatchCanonicalOutbox pool wsState records
        reauthorizeCanonicalConnections pool wsState
        pure records) (pure [])
      pure $ case delivered of
        [] -> acknowledgements
        records -> Map.insert scope (maximum (map (.outboxCursor) records)) acknowledgements

stopChangeStreamWorker :: ChangeStreamWorker -> IO ()
stopChangeStreamWorker (ChangeStreamWorker worker finished _) = killThread worker >> readMVar finished >> pure ()

trySynchronous :: IO a -> IO (Either SomeException a)
trySynchronous action = (Right <$> action) `catch` \err ->
  case fromException err :: Maybe SomeAsyncException of
    Just _ -> throwIO err
    Nothing -> pure (Left err)

catchSynchronous :: IO a -> IO a -> IO a
catchSynchronous action fallback = trySynchronous action >>= either (const fallback) pure

-- Keep raw connection/server errors out of logs while distinguishing the
-- common bounded deferrals from an unavailable database or worker failure.
failureKind :: SomeException -> String
failureKind err = case fromException err of
  Just DBStatementTimeout -> "statement_timeout"
  Just (DBOtherError message) | "lock timeout" `T.isInfixOf` message -> "lock_timeout"
  Just (_ :: DBException) -> "database_error"
  Nothing -> "worker_error"
