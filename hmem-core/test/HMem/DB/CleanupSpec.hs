{-# OPTIONS_GHC -Wno-x-partial -Wno-incomplete-uni-patterns #-}

module HMem.DB.CleanupSpec (spec) where

import Data.Functor.Contravariant ((>$<))
import Data.String (fromString)
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Hasql.Encoders qualified as E
import Hasql.Decoders qualified as D
import Data.UUID (UUID)
import Test.Hspec

import HMem.DB.Cleanup
import HMem.DB.Memory (createMemory, getMemory, linkMemories)
import HMem.DB.Pool (runSession)
import HMem.DB.TestHarness
import HMem.Types

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Create a short-term memory with a given importance.
mkMemory :: TestEnv -> UUID -> Int -> String -> IO Memory
mkMemory env wsId imp label = createMemory env.pool CreateMemory
  { workspaceId = wsId
  , content     = "cleanup-test: " <> fromString label
  , summary     = Nothing
  , memoryType  = ShortTerm
  , importance  = Just imp
  , metadata    = Nothing
  , expiresAt   = Nothing
  , source      = Nothing
  , confidence  = Nothing
  , pinned      = Nothing
  , tags        = Nothing
  , ftsLanguage = Nothing
  }

-- | Backdate a memory's created_at to N hours ago via raw SQL.
backdateMemory :: TestEnv -> UUID -> Int -> IO ()
backdateMemory env mid hours = do
  let stmt = Statement.Statement
        "UPDATE memories SET created_at = now() - ($1 * interval '1 hour') WHERE id = $2"
        (  (fromIntegral . fst >$< E.param (E.nonNullable E.int4))
        <> (snd               >$< E.param (E.nonNullable E.uuid))
        )
        D.noResult
        True
  runSession env.pool $ Session.statement (hours, mid) stmt

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

spec :: Spec
spec = beforeAll setupTestPool $ aroundWith withTestTransaction $ do

  describe "upsertCleanupPolicy + getCleanupPolicies" $ do
    it "creates and lists a policy" $ \env -> do
      ws <- createTestWorkspace env "cleanup-ws"
      let p = UpsertCleanupPolicy
            { workspaceId   = ws.id
            , memoryType    = ShortTerm
            , maxAgeHours   = Just 24
            , maxCount      = Just 100
            , minImportance = 3
            , enabled       = True
            }
      policy <- upsertCleanupPolicy env.pool p
      policy.memoryType `shouldBe` ShortTerm
      policy.minImportance `shouldBe` 3
      policy.enabled `shouldBe` True

      policies <- getCleanupPolicies env.pool ws.id Nothing Nothing
      length policies `shouldBe` 1

    it "upserts same (workspace, memory_type) without duplicating" $ \env -> do
      ws <- createTestWorkspace env "upsert-ws"
      let base = UpsertCleanupPolicy
            { workspaceId   = ws.id
            , memoryType    = LongTerm
            , maxAgeHours   = Just 48
            , maxCount      = Nothing
            , minImportance = 5
            , enabled       = True
            }
      _ <- upsertCleanupPolicy env.pool base
      updated <- upsertCleanupPolicy env.pool UpsertCleanupPolicy
        { workspaceId = base.workspaceId
        , memoryType = base.memoryType
        , maxAgeHours = base.maxAgeHours
        , maxCount = base.maxCount
        , minImportance = 2
        , enabled = base.enabled
        }
      updated.minImportance `shouldBe` 2

      policies <- getCleanupPolicies env.pool ws.id Nothing Nothing
      length policies `shouldBe` 1

  describe "runCleanup - maxCount" $ do
    it "deletes excess low-importance memories" $ \env -> do
      ws <- createTestWorkspace env "count-ws"
      -- Create 5 memories with importance 2 (below threshold)
      _ <- mapM (\i -> mkMemory env ws.id 2 ("low-" ++ show i)) [1..5 :: Int]
      -- Create 2 memories with high importance (should survive)
      high1 <- mkMemory env ws.id 8 "high-1"
      high2 <- mkMemory env ws.id 9 "high-2"

      -- Policy: keep at most 3, delete anything with importance < 5
      _ <- upsertCleanupPolicy env.pool UpsertCleanupPolicy
        { workspaceId   = ws.id
        , memoryType    = ShortTerm
        , maxAgeHours   = Nothing
        , maxCount      = Just 3
        , minImportance = 5
        , enabled       = True
        }

      result <- runCleanup env.pool ws.id
      result.deletedCount `shouldSatisfy` (> 0)

      -- High-importance memories must survive
      getMemory env.pool high1.id >>= (`shouldSatisfy` (/= Nothing))
      getMemory env.pool high2.id >>= (`shouldSatisfy` (/= Nothing))

  describe "runCleanup - minImportance protection" $ do
    it "does not delete memories at or above minImportance" $ \env -> do
      ws <- createTestWorkspace env "importance-ws"
      safe <- mkMemory env ws.id 5 "at-threshold"
      _ <- upsertCleanupPolicy env.pool UpsertCleanupPolicy
        { workspaceId   = ws.id
        , memoryType    = ShortTerm
        , maxAgeHours   = Just 1
        , maxCount      = Just 0
        , minImportance = 5
        , enabled       = True
        }
      -- Backdate the memory so it's old enough for age-based cleanup
      backdateMemory env safe.id 48
      _ <- runCleanup env.pool ws.id
      -- Memory at exactly minImportance (5) should NOT be deleted (< 5 threshold)
      getMemory env.pool safe.id >>= (`shouldSatisfy` (/= Nothing))

  describe "runCleanup - linked memories protected" $ do
    it "does not delete linked memories even if low importance" $ \env -> do
      ws <- createTestWorkspace env "linked-ws"
      src <- mkMemory env ws.id 1 "linked-source"
      tgt <- mkMemory env ws.id 1 "linked-target"
      linkMemories env.pool src.id CreateMemoryLink
        { targetId     = tgt.id
        , relationType = Related
        , strength     = Nothing
        }
      _ <- upsertCleanupPolicy env.pool UpsertCleanupPolicy
        { workspaceId   = ws.id
        , memoryType    = ShortTerm
        , maxAgeHours   = Just 1
        , maxCount      = Just 0
        , minImportance = 5
        , enabled       = True
        }
      backdateMemory env src.id 48
      backdateMemory env tgt.id 48
      _ <- runCleanup env.pool ws.id
      getMemory env.pool src.id >>= (`shouldSatisfy` (/= Nothing))
      getMemory env.pool tgt.id >>= (`shouldSatisfy` (/= Nothing))

  describe "runCleanup - maxAgeHours" $ do
    it "deletes old low-importance memories" $ \env -> do
      ws <- createTestWorkspace env "age-ws"
      old <- mkMemory env ws.id 2 "old-mem"
      fresh <- mkMemory env ws.id 2 "fresh-mem"
      _ <- upsertCleanupPolicy env.pool UpsertCleanupPolicy
        { workspaceId   = ws.id
        , memoryType    = ShortTerm
        , maxAgeHours   = Just 24
        , maxCount      = Nothing
        , minImportance = 5
        , enabled       = True
        }
      -- Backdate one memory to 48 hours ago
      backdateMemory env old.id 48
      result <- runCleanup env.pool ws.id
      result.deletedCount `shouldBe` 1
      getMemory env.pool old.id >>= (`shouldSatisfy` (== Nothing))
      getMemory env.pool fresh.id >>= (`shouldSatisfy` (/= Nothing))

  describe "runCleanup - disabled policy" $ do
    it "skips disabled policies entirely" $ \env -> do
      ws <- createTestWorkspace env "disabled-ws"
      m <- mkMemory env ws.id 1 "should-survive"
      _ <- upsertCleanupPolicy env.pool UpsertCleanupPolicy
        { workspaceId   = ws.id
        , memoryType    = ShortTerm
        , maxAgeHours   = Just 1
        , maxCount      = Just 0
        , minImportance = 10
        , enabled       = False  -- disabled
        }
      backdateMemory env m.id 48
      result <- runCleanup env.pool ws.id
      result.deletedCount `shouldBe` 0
      getMemory env.pool m.id >>= (`shouldSatisfy` (/= Nothing))

  describe "runCleanup - empty run" $ do
    it "returns 0 when no memories match" $ \env -> do
      ws <- createTestWorkspace env "empty-ws"
      _ <- upsertCleanupPolicy env.pool UpsertCleanupPolicy
        { workspaceId   = ws.id
        , memoryType    = ShortTerm
        , maxAgeHours   = Just 1
        , maxCount      = Just 100
        , minImportance = 5
        , enabled       = True
        }
      result <- runCleanup env.pool ws.id
      result.deletedCount `shouldBe` 0

    it "returns 0 when no policies exist" $ \env -> do
      ws <- createTestWorkspace env "nopolicy-ws"
      result <- runCleanup env.pool ws.id
      result.deletedCount `shouldBe` 0
