-- | Supervised delivery/maintenance loop for the durable change stream.
-- Canonical clients replay from their opaque bearer in 'WebSocket'; this loop
-- exists solely to keep the old client population on a committed-outbox,
-- invalidation-only adapter until its explicit cutover.
module HMem.Server.ChangeStream
  ( ChangeStreamWorker
  , startChangeStreamWorker
  , stopChangeStreamWorker
  ) where

import Control.Concurrent (MVar, ThreadId, forkFinally, killThread, newEmptyMVar, putMVar, readMVar, threadDelay)
import Control.Exception (AsyncException, SomeException, catch, fromException, throwIO)
import Control.Monad (foldM, when)
import Data.Pool (Pool)
import Data.Map.Strict qualified as Map
import Data.Time (addUTCTime, getCurrentTime)
import Hasql.Connection qualified as Hasql

import HMem.Config (ChangeStreamConfig(..))
import HMem.DB.ChangeStream (OutboxRecord(..), cleanupChangeStream, listOutboxAfter, listOutboxScopes, pruneOutboxBefore)
import HMem.Server.WebSocket (WSState, broadcastLegacyOutbox, dispatchCanonicalOutbox, reauthorizeCanonicalConnections)

data ChangeStreamWorker = ChangeStreamWorker ThreadId (MVar (Either SomeException ()))

startChangeStreamWorker :: Pool Hasql.Connection -> ChangeStreamConfig -> WSState -> IO ChangeStreamWorker
startChangeStreamWorker pool config wsState = do
  finished <- newEmptyMVar
  worker <- forkFinally (loop Map.empty 0) (putResult finished)
  pure (ChangeStreamWorker worker finished)
  where
    loop acknowledgements maintenanceTicks = do
      -- Cursors are monotonic only within a scope.  Scanning each durable
      -- scope stream prevents a late commit in one scope from being skipped by
      -- another scope's timestamp.  A cursor advances only after all dispatch
      -- work for that batch returns successfully, so a transient failure is
      -- retried from the same committed record on the next tick.
      scopes <- catchSynchronous (listOutboxScopes pool) (pure [])
      acknowledgements' <- foldM dispatchScope acknowledgements scopes
      now <- getCurrentTime
      let nextTick = maintenanceTicks + 1
      when (nextTick >= 60) $ do
        let cutoff = addUTCTime (negate (fromIntegral config.retentionSeconds)) now
        _ <- catchSynchronous (pruneOutboxBefore pool cutoff) (pure 0)
        _ <- catchSynchronous (cleanupChangeStream pool now) (pure (0, 0))
        pure ()
      threadDelay 1000000
      loop acknowledgements' (if nextTick >= 60 then 0 else nextTick)

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
stopChangeStreamWorker (ChangeStreamWorker worker finished) = killThread worker >> readMVar finished >> pure ()

putResult :: MVar (Either SomeException ()) -> Either SomeException () -> IO ()
putResult = putMVar

-- Do not swallow 'AsyncException': cancellation is part of the shutdown
-- contract and must stop the worker before its database pool is destroyed.
catchSynchronous :: IO a -> IO a -> IO a
catchSynchronous action fallback = action `catch` \err ->
  case fromException err :: Maybe AsyncException of
    Just _ -> throwIO err
    Nothing -> fallback
