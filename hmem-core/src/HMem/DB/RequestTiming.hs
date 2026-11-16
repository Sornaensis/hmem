{-# LANGUAGE BangPatterns #-}

-- | Attribution of database pool and optional provider time to requests, and
-- process-wide pool checkout counters with fixed wait buckets. Contains no
-- identifiers. See docs/reference/request-performance.md.
module HMem.DB.RequestTiming
  ( -- * Per-request accumulation
    RequestTiming(..)
  , RequestTimer
  , withRequestTimer
  , readRequestTimer
  , currentRequestTimer
  , withInheritedRequestTimer
  , timeProviderCall
    -- * Pool checkout accounting (recorded by "HMem.DB.Pool")
  , timerConnectNs
  , recordConnect
  , connectCount
  , recordCheckout
  , recordHold
    -- * Process-wide checkout counters
  , PoolCheckoutMetrics(..)
  , getPoolCheckoutMetrics
  , checkoutWaitBoundsNs
  , capacityWaitThresholdNs
  ) where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Exception (bracket, finally)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.IO.Unsafe (unsafePerformIO)

------------------------------------------------------------------------
-- Per-request accumulation
------------------------------------------------------------------------

-- | Totals for one request. Concurrent checkouts made for the same request
-- are summed, so totals can exceed the request's wall-clock duration.
data RequestTiming = RequestTiming
  { dbCheckouts   :: !Int
  , dbWaitNs      :: !Word64  -- ^ Checkout request to connection handed over, including new connections.
  , dbConnectNs   :: !Word64  -- ^ Part of the wait spent establishing new connections.
  , dbHoldNs      :: !Word64  -- ^ Connection handed over to returned.
  , providerCalls :: !Int
  , providerNs    :: !Word64
  } deriving stock (Show, Eq)

emptyRequestTiming :: RequestTiming
emptyRequestTiming = RequestTiming 0 0 0 0 0 0

newtype RequestTimer = RequestTimer (IORef RequestTiming)

-- Keyed by thread like the request context. Forked work is unattributed
-- unless it explicitly inherits the timer.
{-# NOINLINE timersRef #-}
timersRef :: IORef (Map ThreadId RequestTimer)
timersRef = unsafePerformIO (newIORef Map.empty)

-- | Attribute this thread's pool and provider time to a new timer.
withRequestTimer :: (RequestTimer -> IO a) -> IO a
withRequestTimer action = do
  timer <- RequestTimer <$> newIORef emptyRequestTiming
  withInheritedRequestTimer (Just timer) (action timer)

-- | Attribute this thread's work to a timer captured on another thread with
-- 'currentRequestTimer'. The previous registration is restored afterwards.
withInheritedRequestTimer :: Maybe RequestTimer -> IO a -> IO a
withInheritedRequestTimer Nothing action = action
withInheritedRequestTimer (Just timer) action = bracket register restore (const action)
  where
    register = do
      tid <- myThreadId
      prior <- atomicModifyIORef' timersRef $ \m -> (Map.insert tid timer m, Map.lookup tid m)
      pure (tid, prior)
    restore (tid, prior) = atomicModifyIORef' timersRef $ \m ->
      (maybe (Map.delete tid m) (\p -> Map.insert tid p m) prior, ())

currentRequestTimer :: IO (Maybe RequestTimer)
currentRequestTimer = do
  tid <- myThreadId
  Map.lookup tid <$> readIORef timersRef

readRequestTimer :: RequestTimer -> IO RequestTiming
readRequestTimer (RequestTimer ref) = readIORef ref

addTiming :: Maybe RequestTimer -> (RequestTiming -> RequestTiming) -> IO ()
addTiming Nothing _ = pure ()
addTiming (Just (RequestTimer ref)) update = atomicModifyIORef' ref (\t -> (update t, ()))

-- | Time an optional provider call for the current request, including calls
-- that end with an exception or timeout.
timeProviderCall :: IO a -> IO a
timeProviderCall action = do
  timer <- currentRequestTimer
  started <- getMonotonicTimeNSec
  action `finally` do
    finished <- getMonotonicTimeNSec
    addTiming timer $ \t -> t { providerCalls = t.providerCalls + 1, providerNs = t.providerNs + elapsed started finished }

------------------------------------------------------------------------
-- Process-wide checkout counters
------------------------------------------------------------------------

-- | Cumulative since process start; all pools in the process share them.
data PoolCheckoutMetrics = PoolCheckoutMetrics
  { checkouts             :: !Word64
  , unattributedCheckouts :: !Word64  -- ^ Checkouts on threads without a request timer.
  , connects              :: !Word64
  , capacityWaits         :: !Word64  -- ^ See 'capacityWaitThresholdNs'.
  , waitTotalNs           :: !Word64
  , waitMaxNs             :: !Word64
  , connectTotalNs        :: !Word64
  , holdTotalNs           :: !Word64
  , waitBuckets           :: ![Word64] -- ^ One count per 'checkoutWaitBoundsNs' entry, then the overflow.
  } deriving stock (Show, Eq)

-- | Inclusive upper bounds: 0.1, 0.5, 1, 5, 10, 50, 100, 500 and 1000 ms.
checkoutWaitBoundsNs :: [Word64]
checkoutWaitBoundsNs =
  [100000, 500000, 1000000, 5000000, 10000000, 50000000, 100000000, 500000000, 1000000000]

-- | A capacity wait queued at least this long (1 ms), excluding connection
-- time, although checked-out and waiting checkouts together were fewer than
-- the configured maximum when it was requested, and no connection was
-- established meanwhile. resource-pool stripes cannot lend connections to each
-- other, so this is a lower bound on waits for a full stripe.
capacityWaitThresholdNs :: Word64
capacityWaitThresholdNs = 1000000

emptyMetrics :: PoolCheckoutMetrics
emptyMetrics = PoolCheckoutMetrics 0 0 0 0 0 0 0 0 (0 <$ (0 : checkoutWaitBoundsNs))

{-# NOINLINE metricsRef #-}
metricsRef :: IORef PoolCheckoutMetrics
metricsRef = unsafePerformIO (newIORef emptyMetrics)

getPoolCheckoutMetrics :: IO PoolCheckoutMetrics
getPoolCheckoutMetrics = readIORef metricsRef

timerConnectNs :: Maybe RequestTimer -> IO Word64
timerConnectNs = maybe (pure 0) (fmap (.dbConnectNs) . readRequestTimer)

-- | Record a new connection. Pool acquisition runs on the requesting thread.
recordConnect :: Word64 -> IO ()
recordConnect ns = do
  timer <- currentRequestTimer
  atomicModifyIORef' metricsRef $ \m -> (m { connects = m.connects + 1, connectTotalNs = m.connectTotalNs + ns }, ())
  addTiming timer $ \t -> t { dbConnectNs = t.dbConnectNs + ns }

connectCount :: IO Word64
connectCount = (.connects) <$> readIORef metricsRef

-- | Record a completed checkout: its wait, the part spent connecting for this
-- request, and whether it is a capacity-wait candidate.
recordCheckout :: Maybe RequestTimer -> Word64 -> Word64 -> Bool -> IO ()
recordCheckout timer waitNs connectNs belowCapacity = do
  let queued = waitNs - min waitNs connectNs
      capacity = belowCapacity && queued >= capacityWaitThresholdNs
  atomicModifyIORef' metricsRef $ \m ->
    ( m { checkouts = m.checkouts + 1
        , unattributedCheckouts = m.unattributedCheckouts + maybe 1 (const 0) timer
        , capacityWaits = m.capacityWaits + if capacity then 1 else 0
        , waitTotalNs = m.waitTotalNs + waitNs
        , waitMaxNs = max m.waitMaxNs waitNs
        , waitBuckets = bump waitNs checkoutWaitBoundsNs m.waitBuckets
        }
    , () )
  addTiming timer $ \t -> t { dbCheckouts = t.dbCheckouts + 1, dbWaitNs = t.dbWaitNs + waitNs }

recordHold :: Maybe RequestTimer -> Word64 -> IO ()
recordHold timer holdNs = do
  atomicModifyIORef' metricsRef $ \m -> (m { holdTotalNs = m.holdTotalNs + holdNs }, ())
  addTiming timer $ \t -> t { dbHoldNs = t.dbHoldNs + holdNs }

-- Strict in the whole spine so repeated updates cannot accumulate thunks.
bump :: Word64 -> [Word64] -> [Word64] -> [Word64]
bump value (bound : bounds) (count : counts)
  | value <= bound = let !next = count + 1 in next : counts
  | otherwise = let !rest = bump value bounds counts in count : rest
bump _ [] (count : counts) = let !next = count + 1 in next : counts
bump _ _ [] = []

elapsed :: Word64 -> Word64 -> Word64
elapsed start end
  | end <= start = 0
  | otherwise = end - start
