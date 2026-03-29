module HMem.Server.AccessTracker
  ( AccessTracker
  , newAccessTracker
  , trackAccess
  , flushNow
  , bufferSize
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, catch)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Pool (Pool)
import Data.UUID (UUID)
import Hasql.Connection qualified as Hasql

import HMem.DB.Memory (touchMemoryBatch)

-- | Buffers memory access events and flushes them periodically
-- in a single batched UPDATE to reduce write amplification.
newtype AccessTracker = AccessTracker
  { atBuffer :: IORef (Map UUID Int)
  }

-- | Create a new access tracker that flushes every @flushIntervalSecs@ seconds.
newAccessTracker :: Pool Hasql.Connection -> Int -> IO AccessTracker
newAccessTracker pool flushIntervalSecs = do
  buf  <- newIORef Map.empty
  let tracker = AccessTracker buf
  _ <- forkIO $ flushLoop pool tracker flushIntervalSecs
  pure tracker

-- | Maximum number of distinct memory IDs to buffer before dropping
-- new events.  Prevents unbounded growth during prolonged DB outages.
maxBufferSize :: Int
maxBufferSize = 10000

-- | Record a memory access.  Does not touch the database; just
-- increments a counter in the in-memory buffer.
-- Drops the event when the buffer exceeds 'maxBufferSize' entries.
trackAccess :: AccessTracker -> UUID -> IO ()
trackAccess tracker mid =
  atomicModifyIORef' tracker.atBuffer $ \m ->
    if Map.size m >= maxBufferSize
    then (m, ())
    else (Map.insertWith (+) mid 1 m, ())

-- | Background loop that periodically drains the buffer and
-- writes accumulated access counts to the database.
flushLoop :: Pool Hasql.Connection -> AccessTracker -> Int -> IO ()
flushLoop pool tracker intervalSecs = go
  where
    go = do
      threadDelay (intervalSecs * 1000000)
      flush pool tracker
      go

flush :: Pool Hasql.Connection -> AccessTracker -> IO ()
flush pool tracker = do
  accesses <- atomicModifyIORef' tracker.atBuffer $ \m -> (Map.empty, m)
  case Map.toList accesses of
    [] -> pure ()
    xs -> touchMemoryBatch pool xs `catch` \(_ :: SomeException) ->
            -- Re-merge failed counts back into the buffer for retry
            -- on the next flush cycle instead of silently dropping them.
            -- Truncate to maxBufferSize to prevent unbounded growth.
            atomicModifyIORef' tracker.atBuffer $ \m ->
              let merged = Map.unionWith (+) accesses m
              in (Map.take maxBufferSize merged, ())

-- | Force an immediate flush of buffered access counts to the
-- database.  Intended for use during graceful shutdown.
flushNow :: Pool Hasql.Connection -> AccessTracker -> IO ()
flushNow = flush

-- | Current number of distinct memory IDs buffered.
bufferSize :: AccessTracker -> IO Int
bufferSize tracker = Map.size <$> readIORef tracker.atBuffer
