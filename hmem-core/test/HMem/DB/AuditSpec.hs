{-# OPTIONS_GHC -Wno-x-partial -Wno-incomplete-uni-patterns #-}

module HMem.DB.AuditSpec (spec) where

import Data.Aeson (Value(..))
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KM
import Data.Text qualified as T
import Test.Hspec

import HMem.DB.Memory
import HMem.DB.TestHarness
import HMem.Types

spec :: Spec
spec = around withTestEnv $ do

  describe "audit log" $ do
    it "records create and soft-delete snapshots for memories" $ \env -> do
      ws <- createTestWorkspace env "audit-ws"
      mem <- createMemory env.pool CreateMemory
        { workspaceId = ws.id
        , content = "audit me"
        , summary = Just "snapshot"
        , memoryType = ShortTerm
        , importance = Just 6
        , metadata = Nothing
        , expiresAt = Nothing
        , source = Nothing
        , confidence = Nothing
        , pinned = Nothing
        , tags = Nothing
        , ftsLanguage = Nothing
        }

      createdRows <- getAuditLogRows env.pool "memory" (T.pack (show mem.id))
      let [created] = createdRows
      created.action `shouldBe` "create"
      created.requestId `shouldBe` Nothing
      (created.newValues >>= lookupField "content") `shouldBe` Just (String "audit me")

      deleteMemory env.pool mem.id `shouldReturn` True

      allRows <- getAuditLogRows env.pool "memory" (T.pack (show mem.id))
      map (.action) allRows `shouldBe` ["create", "update"]

      let deleted = allRows !! 1
      (deleted.oldValues >>= lookupField "deleted_at") `shouldBe` Just Null
      (deleted.newValues >>= lookupField "deleted_at") `shouldSatisfy` \case
        Just (String _) -> True
        _               -> False

    it "ignores access-tracking updates for memories" $ \env -> do
      ws <- createTestWorkspace env "touch-ws"
      mem <- createMemory env.pool CreateMemory
        { workspaceId = ws.id
        , content = "read often"
        , summary = Nothing
        , memoryType = ShortTerm
        , importance = Nothing
        , metadata = Nothing
        , expiresAt = Nothing
        , source = Nothing
        , confidence = Nothing
        , pinned = Nothing
        , tags = Nothing
        , ftsLanguage = Nothing
        }

      touchMemoryBatch env.pool [(mem.id, 3)]

      rows <- getAuditLogRows env.pool "memory" (T.pack (show mem.id))
      map (.action) rows `shouldBe` ["create"]

lookupField :: T.Text -> Value -> Maybe Value
lookupField key (Object obj) = KM.lookup (fromText key) obj
lookupField _ _ = Nothing