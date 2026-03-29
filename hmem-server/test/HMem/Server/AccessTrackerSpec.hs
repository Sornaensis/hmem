module HMem.Server.AccessTrackerSpec (spec) where

import Control.Concurrent.Async (concurrently_)
import Data.UUID qualified as UUID
import Test.Hspec

import HMem.DB.TestHarness
import HMem.Server.AccessTracker

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Create a tracker with a very long flush interval (effectively manual).
-- Uses the real DB pool from the test harness.
withTracker :: (TestEnv -> AccessTracker -> IO a) -> IO a
withTracker action = withTestEnv $ \env -> do
  tracker <- newAccessTracker env.pool 99999
  action env tracker

uuid1, uuid2, uuid3 :: UUID.UUID
uuid1 = read "aaaaaaaa-bbbb-cccc-dddd-000000000001"
uuid2 = read "aaaaaaaa-bbbb-cccc-dddd-000000000002"
uuid3 = read "aaaaaaaa-bbbb-cccc-dddd-000000000003"

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

spec :: Spec
spec = do

  describe "trackAccess + bufferSize" $ do
    it "starts with an empty buffer" $ withTracker $ \_env tracker -> do
      sz <- bufferSize tracker
      sz `shouldBe` 0

    it "buffers access events without hitting DB" $ withTracker $ \_env tracker -> do
      trackAccess tracker uuid1
      trackAccess tracker uuid2
      trackAccess tracker uuid1  -- duplicate
      sz <- bufferSize tracker
      sz `shouldBe` 2  -- two distinct UUIDs

    it "accumulates counts for the same UUID" $ withTracker $ \_env tracker -> do
      trackAccess tracker uuid1
      trackAccess tracker uuid1
      trackAccess tracker uuid1
      sz <- bufferSize tracker
      sz `shouldBe` 1

  describe "flushNow" $ do
    it "drains the buffer to zero" $ withTracker $ \env tracker -> do
      trackAccess tracker uuid1
      trackAccess tracker uuid2
      bufferSize tracker >>= (`shouldBe` 2)
      -- flush writes to DB (no matching memory rows but won't error)
      flushNow env.pool tracker
      bufferSize tracker >>= (`shouldBe` 0)

    it "is safe to call with an empty buffer" $ withTracker $ \env tracker -> do
      flushNow env.pool tracker
      bufferSize tracker >>= (`shouldBe` 0)

    it "is safe to call multiple times" $ withTracker $ \env tracker -> do
      trackAccess tracker uuid1
      flushNow env.pool tracker
      flushNow env.pool tracker
      bufferSize tracker >>= (`shouldBe` 0)

  describe "concurrent access" $ do
    it "handles concurrent trackAccess calls safely" $ withTracker $ \_env tracker -> do
      let go uid n = mapM_ (\_ -> trackAccess tracker uid) [1..n :: Int]
      concurrently_
        (go uuid1 100)
        (concurrently_ (go uuid2 100) (go uuid3 100))
      sz <- bufferSize tracker
      sz `shouldBe` 3
