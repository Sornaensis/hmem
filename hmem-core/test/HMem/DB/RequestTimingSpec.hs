module HMem.DB.RequestTimingSpec (spec) where

import Control.Concurrent (getNumCapabilities, threadDelay)
import Control.Concurrent.Async (async, asyncOn, concurrently_, wait, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Control.Exception (ErrorCall(..), bracket, throwIO, try)
import GHC.Clock (getMonotonicTimeNSec)
import Control.Monad (void)
import Data.Maybe (isNothing)
import Data.Pool (destroyAllResources)
import Data.UUID (UUID)
import Data.Word (Word64)
import Hasql.Session qualified as Session
import Test.Hspec

import HMem.DB.Pool (DBException(..), PoolMetrics(..), createDeliveryPool, createDeliveryPoolWithCheckoutTimeout, createPool, destroyDeliveryPool, getPoolCheckoutMetricsFor, getPoolMetrics, getPoolMetricsFor, withConn)
import System.Timeout (timeout)
import HMem.DB.RequestTiming
import HMem.DB.Search (searchAll)
import HMem.DB.TestHarness
import HMem.Types

spec :: Spec
spec = around withTestEnv $ describe "request pool timing" $ do
  it "attributes checkout wait and hold to the waiting request under an exhausted pool" $ \env ->
    bracket (createPool env.testDb.testDbConnStr 1 30 30000) destroyAllResources $ \pool -> do
      startMetrics <- getPoolCheckoutMetrics
      holding <- newEmptyMVar
      release <- newEmptyMVar
      -- An unattributed holder owns the only connection until released.
      timing <- withAsync (withConn pool $ \conn -> select1 conn >> putMVar holding () >> takeMVar release) $ \holder -> do
        takeMVar holding
        timing <- withRequestTimer $ \timer ->
          withAsync (threadDelay 150000 >> putMVar release ()) $ \_ -> do
            withConn pool $ \conn -> select1 conn >> threadDelay 50000
            readRequestTimer timer
        wait holder
        pure timing
      endMetrics <- getPoolCheckoutMetrics
      timing.dbCheckouts `shouldBe` 1
      timing.dbWaitNs `shouldSatisfy` (>= ms 100)
      timing.dbConnectNs `shouldBe` 0
      timing.dbHoldNs `shouldSatisfy` (>= ms 40)
      timing.dbHoldNs `shouldSatisfy` (< timing.dbWaitNs)
      timing.providerCalls `shouldBe` 0
      endMetrics.checkouts - startMetrics.checkouts `shouldSatisfy` (>= 2)
      endMetrics.unattributedCheckouts - startMetrics.unattributedCheckouts `shouldSatisfy` (>= 1)
      endMetrics.waitMaxNs `shouldSatisfy` (>= ms 100)
      endMetrics.waitTotalNs - startMetrics.waitTotalNs `shouldSatisfy` (>= ms 100)
      -- The 100-500 ms bucket holds the request's wait. A full pool is not a
      -- capacity wait: every configured connection was checked out.
      bucket 7 endMetrics - bucket 7 startMetrics `shouldSatisfy` (>= 1)
      length endMetrics.waitBuckets `shouldBe` length checkoutWaitBoundsNs + 1
      endMetrics.capacityWaits `shouldBe` startMetrics.capacityWaits

  it "serves a same-capability checkout from an idle connection while another is held" $ \env -> do
    caps <- getNumCapabilities
    -- A pool striped by capability would make the second checkout wait for
    -- the held connection of its own stripe while the other stripe is idle.
    if caps < 2 then pendingWith "requires at least two capabilities" else
      bracket (createPool env.testDb.testDbConnStr 2 30 30000) destroyAllResources $ \pool -> do
        warmed <- newEmptyMVar
        let warm capability = asyncOn capability (withConn pool $ \conn -> select1 conn >> readMVar warmed)
        warmers <- mapM warm [0, 1]
        waitUntil ((>= 2) . (.activeConnections) <$> getPoolMetricsFor pool)
        putMVar warmed ()
        mapM_ wait warmers
        startMetrics <- getPoolCheckoutMetrics
        holding <- newEmptyMVar
        release <- newEmptyMVar
        holder <- asyncOn 0 (withConn pool $ \conn -> select1 conn >> putMVar holding () >> takeMVar release)
        takeMVar holding
        second <- timeout 2000000 $
          asyncOn 0 (withRequestTimer $ \timer -> withConn pool select1 >> readRequestTimer timer) >>= wait
        putMVar release ()
        wait holder
        endMetrics <- getPoolCheckoutMetrics
        fmap (.dbCheckouts) second `shouldBe` Just 1
        fmap (.dbWaitNs) second `shouldSatisfy` maybe False (< ms 100)
        endMetrics.connects `shouldBe` startMetrics.connects
        endMetrics.capacityWaits `shouldBe` startMetrics.capacityWaits

  it "never opens more connections at once than its configured size" $ \env -> do
    caps <- getNumCapabilities
    -- One more connection than capabilities: a pool with a stripe per
    -- capability would round each stripe's share up to two.
    let size = caps + 1
    if caps < 2 || caps > 64 then pendingWith "requires 2..64 capabilities" else
      bracket (createPool env.testDb.testDbConnStr size 30 30000) destroyAllResources $ \pool -> do
        held <- newIORef (0 :: Int)
        peak <- newIORef (0 :: Int)
        release <- newEmptyMVar
        let hold conn = do
              select1 conn
              now <- atomicModifyIORef' held (\n -> (n + 1, n + 1))
              atomicModifyIORef' peak (\p -> (max p now, ()))
              readMVar release
              atomicModifyIORef' held (\n -> (n - 1, ()))
            pinned = concatMap (\capability -> [capability, capability]) [0 .. caps - 1]
        holders <- mapM (\capability -> asyncOn capability (withConn pool hold)) pinned
        waitUntil ((>= size) <$> readIORef held)
        -- Give any connection beyond the bound time to open.
        threadDelay 300000
        observed <- readIORef peak
        putMVar release ()
        mapM_ wait holders
        observed `shouldBe` size
        (.maxConnections) <$> getPoolMetricsFor pool `shouldReturn` size

  it "abandons a checkout at the checkout timeout without leaking the queue or a connection" $ \env ->
    bracket (createDeliveryPoolWithCheckoutTimeout env.testDb.testDbConnStr 1 30 30000 1000) destroyDeliveryPool $ \pool -> do
      holding <- newEmptyMVar
      release <- newEmptyMVar
      holder <- async (withConn pool $ \conn -> select1 conn >> putMVar holding () >> takeMVar release)
      takeMVar holding
      started <- getMonotonicTimeNSec
      waited <- try @DBException (withConn pool select1)
      finished <- getMonotonicTimeNSec
      waited `shouldBe` Left DBPoolCheckoutTimeout
      (finished - started) `shouldSatisfy` (>= ms 1000)
      (finished - started) `shouldSatisfy` (< ms 2000)
      putMVar release ()
      wait holder
      getPoolMetricsFor pool `shouldReturn` PoolMetrics { activeConnections = 0, maxConnections = 1, pendingCheckouts = 0 }
      (.checkoutTimeouts) <$> getPoolCheckoutMetricsFor pool `shouldReturn` 1
      -- The connection was returned: the next checkout succeeds at once.
      timeout 500000 (withConn pool select1) `shouldReturn` Just ()
      getPoolMetricsFor pool `shouldReturn` PoolMetrics 0 1 0

  it "separates new-connection time from the wait of the request that created it" $ \env ->
    bracket (createPool env.testDb.testDbConnStr 1 30 30000) destroyAllResources $ \pool -> do
      startMetrics <- getPoolCheckoutMetrics
      timing <- withRequestTimer $ \timer -> withConn pool select1 >> readRequestTimer timer
      endMetrics <- getPoolCheckoutMetrics
      timing.dbCheckouts `shouldBe` 1
      timing.dbConnectNs `shouldSatisfy` (> 0)
      timing.dbWaitNs `shouldSatisfy` (>= timing.dbConnectNs)
      endMetrics.connects - startMetrics.connects `shouldSatisfy` (>= 1)
      endMetrics.connectTotalNs - startMetrics.connectTotalNs `shouldSatisfy` (>= timing.dbConnectNs)

  it "serves a second same-capability delivery checkout immediately from its single stripe" $ \env -> do
    caps <- getNumCapabilities
    if caps < 2 then pendingWith "requires at least two capabilities" else
      bracket (createDeliveryPool env.testDb.testDbConnStr 2 30 30000) destroyDeliveryPool $ \pool -> do
        holding <- newEmptyMVar
        release <- newEmptyMVar
        holder <- asyncOn 0 (withConn pool $ \conn -> select1 conn >> putMVar holding () >> takeMVar release)
        takeMVar holding
        second <- timeout 2000000 (asyncOn 0 (withConn pool select1) >>= wait)
        putMVar release ()
        wait holder
        second `shouldBe` Just ()
        (.maxConnections) <$> getPoolMetricsFor pool `shouldReturn` 2

  it "keeps delivery checkouts out of the main gauges, counters, capacity waits and request timers" $ \env ->
    bracket (createPool env.testDb.testDbConnStr 1 30 30000) destroyAllResources $ \mainPool ->
      bracket (createDeliveryPool env.testDb.testDbConnStr 2 30 30000) destroyDeliveryPool $ \delivery -> do
        mainStart <- getPoolCheckoutMetrics
        mainGauges <- getPoolMetrics
        deliveryStart <- getPoolCheckoutMetricsFor delivery
        holding <- newEmptyMVar
        release <- newEmptyMVar
        timing <- withRequestTimer $ \timer -> do
          withAsync (withInheritedRequestTimer (Just timer) $ withConn delivery $ \conn ->
              select1 conn >> putMVar holding () >> takeMVar release) $ \holder -> do
            takeMVar holding
            (.activeConnections) <$> getPoolMetricsFor delivery `shouldReturn` 1
            getPoolMetrics `shouldReturn` mainGauges
            getPoolMetricsFor mainPool `shouldReturn` mainGauges
            putMVar release ()
            wait holder
          withConn delivery select1
          withConn delivery select1
          withConn delivery $ \conn -> select1 conn >> threadDelay 5000
          readRequestTimer timer
        mainEnd <- getPoolCheckoutMetrics
        deliveryEnd <- getPoolCheckoutMetricsFor delivery
        timing `shouldBe` RequestTiming 0 0 0 0 0 0
        mainEnd `shouldBe` mainStart
        getPoolMetrics `shouldReturn` mainGauges
        deliveryEnd.checkouts - deliveryStart.checkouts `shouldBe` 4
        deliveryEnd.unattributedCheckouts - deliveryStart.unattributedCheckouts `shouldBe` 4
        deliveryEnd.connects - deliveryStart.connects `shouldSatisfy` (>= 1)
        deliveryEnd.holdTotalNs `shouldSatisfy` (> deliveryStart.holdTotalNs)
        (.activeConnections) <$> getPoolMetricsFor delivery `shouldReturn` 0
        -- A main-pool checkout is still attributed and counted as before.
        attributed <- withRequestTimer $ \timer -> withConn mainPool select1 >> readRequestTimer timer
        attributed.dbCheckouts `shouldBe` 1
        mainAfter <- getPoolCheckoutMetrics
        mainAfter.checkouts - mainEnd.checkouts `shouldBe` 1

  it "leaves forked work unattributed unless it inherits the request timer" $ \env -> do
    workspace <- createTestWorkspace env "request-timing-search"
    timing <- withRequestTimer $ \timer -> do
      concurrently_ (withConn env.pool select1) (pure ())
      forkedOnly <- readRequestTimer timer
      forkedOnly.dbCheckouts `shouldBe` 0
      inherited <- currentRequestTimer
      concurrently_ (withInheritedRequestTimer inherited (withConn env.pool select1)) (pure ())
      -- Unified search runs its entity branches concurrently.
      void $ searchAll env.pool (searchQuery workspace.id)
      readRequestTimer timer
    timing.dbCheckouts `shouldSatisfy` (>= 4)
    isNothing <$> currentRequestTimer `shouldReturn` True

  it "times provider calls, including failures, separately from database work" $ \_ -> do
    timing <- withRequestTimer $ \timer -> do
      timeProviderCall (threadDelay 20000)
      failed <- try (timeProviderCall (threadDelay 10000 >> throwIO (ErrorCall "provider failed")))
      either (\(ErrorCall _) -> pure ()) (const (expectationFailure "provider failure was swallowed")) failed
      readRequestTimer timer
    timing.providerCalls `shouldBe` 2
    timing.providerNs `shouldSatisfy` (>= ms 25)
    timing.dbCheckouts `shouldBe` 0
    -- The timer registration ends with its scope.
    isNothing <$> currentRequestTimer `shouldReturn` True
  where
    select1 conn = Session.run (Session.sql "SELECT 1") conn >>= either (fail . show) pure
    waitUntil condition = do
      let loop = condition >>= \done -> if done then pure () else threadDelay 5000 >> loop
      timeout 10000000 loop `shouldReturn` Just ()
    ms :: Word64 -> Word64
    ms = (* 1000000)
    bucket :: Int -> PoolCheckoutMetrics -> Word64
    bucket index m = m.waitBuckets !! index

searchQuery :: UUID -> UnifiedSearchQuery
searchQuery workspace = UnifiedSearchQuery
  { currentGitSha = Nothing, historyGitSha = Nothing, workspaceId = Just workspace, query = Just "needle"
  , entityTypes = Nothing, searchLanguage = Nothing, limit = Just 10, offset = Just 0, subjectKind = Nothing
  , subject = Nothing, gitSha = Nothing, projectStatus = Nothing, taskStatus = Nothing, taskPriority = Nothing
  , projectId = Nothing }
