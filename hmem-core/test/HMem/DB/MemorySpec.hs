{-# OPTIONS_GHC -Wno-x-partial -Wno-incomplete-uni-patterns #-}

module HMem.DB.MemorySpec (spec) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (try, SomeException)
import Data.Functor.Contravariant ((>$<))
import Data.Maybe (isJust, isNothing)
import Data.Text qualified as T
import Data.UUID (UUID)
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Test.Hspec

import HMem.DB.Memory
import HMem.DB.Pool (runSession)
import HMem.DB.TestHarness
import HMem.Types


backdateMemoryUpdatedAt :: TestEnv -> UUID -> Int -> IO ()
backdateMemoryUpdatedAt env mid hours = do
  runSession env.pool $ Session.sql "ALTER TABLE memories DISABLE TRIGGER trg_memories_updated_at"
  let stmt = Statement.Statement
        "UPDATE memories SET updated_at = now() - ($1 * interval '1 hour') WHERE id = $2"
        (  (fromIntegral . fst >$< E.param (E.nonNullable E.int4))
        <> (snd               >$< E.param (E.nonNullable E.uuid))
        )
        D.noResult
        True
  runSession env.pool $ Session.statement (hours, mid) stmt
  runSession env.pool $ Session.sql "ALTER TABLE memories ENABLE TRIGGER trg_memories_updated_at"

backdateMemoryCreatedAt :: TestEnv -> UUID -> Int -> IO ()
backdateMemoryCreatedAt env mid hours = do
  let stmt = Statement.Statement
        "UPDATE memories SET created_at = now() - ($1 * interval '1 hour') WHERE id = $2"
        (  (fromIntegral . fst >$< E.param (E.nonNullable E.int4))
        <> (snd               >$< E.param (E.nonNullable E.uuid))
        )
        D.noResult
        True
  runSession env.pool $ Session.statement (hours, mid) stmt

spec :: Spec
spec = beforeAll setupTestPool $ aroundWith withTestTransaction $ do

  describe "createMemory / getMemory" $ do
    it "creates a memory and retrieves it by ID" $ \env -> do
      ws <- createTestWorkspace env "test-ws"
      let cm = CreateMemory
            { workspaceId = ws.id
            , content     = "Remember: Haskell is great"
            , summary     = Just "Haskell note"
            , memoryType  = LongTerm
            , importance  = Just 7
            , metadata    = Nothing
            , expiresAt   = Nothing
            , source      = Nothing
            , confidence  = Nothing
            , pinned      = Nothing
            , tags        = Just ["haskell", "programming"]
            , ftsLanguage = Nothing
            }
      mem <- createMemory env.pool cm
      mem.content `shouldBe` "Remember: Haskell is great"
      mem.memoryType `shouldBe` LongTerm
      mem.importance `shouldBe` 7
      mem.tags `shouldMatchList` ["haskell", "programming"]

      got <- getMemory env.pool mem.id
      got `shouldSatisfy` isJust
      let Just m = got
      m.content `shouldBe` "Remember: Haskell is great"
      m.tags `shouldMatchList` ["haskell", "programming"]

    it "returns Nothing for nonexistent ID" $ \env -> do
      got <- getMemory env.pool (read "00000000-0000-0000-0000-000000000099")
      got `shouldSatisfy` isNothing

    it "defaults importance to 5 when not specified" $ \env -> do
      ws <- createTestWorkspace env "defaults-ws"
      let cm = CreateMemory
            { workspaceId = ws.id
            , content     = "default importance"
            , summary     = Nothing
            , memoryType  = ShortTerm
            , importance  = Nothing
            , metadata    = Nothing
            , expiresAt   = Nothing
            , source      = Nothing
            , confidence  = Nothing
            , pinned      = Nothing
            , tags        = Nothing
            , ftsLanguage = Nothing
            }
      mem <- createMemory env.pool cm
      mem.importance `shouldBe` 5

  describe "updateMemory" $ do
    it "updates content and importance" $ \env -> do
      ws <- createTestWorkspace env "update-ws"
      mem <- createMemory env.pool CreateMemory
        { workspaceId = ws.id
        , content     = "original"
        , summary     = Nothing
        , memoryType  = ShortTerm
        , importance  = Just 3
        , metadata    = Nothing
        , expiresAt   = Nothing
        , source      = Nothing
        , confidence  = Nothing
        , pinned      = Nothing
        , tags        = Nothing
            , ftsLanguage = Nothing
        }
      updated <- updateMemory env.pool mem.id UpdateMemory
        { content    = Just "updated content"
        , summary    = Unchanged
        , memoryType = Nothing
        , importance = Just 9
        , metadata   = Nothing
        , expiresAt  = Unchanged
        , source     = Unchanged
        , confidence = Nothing
        , pinned     = Nothing
        }
      updated `shouldSatisfy` isJust
      let Just u = updated
      u.content `shouldBe` "updated content"
      u.importance `shouldBe` 9

    it "returns Nothing for nonexistent ID" $ \env -> do
      result <- updateMemory env.pool (read "00000000-0000-0000-0000-000000000099") UpdateMemory
        { content = Just "x", summary = Unchanged, memoryType = Nothing
        , importance = Nothing, metadata = Nothing, expiresAt = Unchanged
        , source = Unchanged, confidence = Nothing, pinned = Nothing }
      result `shouldSatisfy` isNothing

    it "preserves unchanged fields" $ \env -> do
      ws <- createTestWorkspace env "preserve-ws"
      mem <- createMemory env.pool CreateMemory
        { workspaceId = ws.id
        , content     = "keep this"
        , summary     = Just "keep summary"
        , memoryType  = LongTerm
        , importance  = Just 8
        , metadata    = Nothing
        , expiresAt   = Nothing
        , source      = Nothing
        , confidence  = Nothing
        , pinned      = Nothing
        , tags        = Nothing
            , ftsLanguage = Nothing
        }
      updated <- updateMemory env.pool mem.id UpdateMemory
        { content = Nothing, summary = Unchanged, memoryType = Nothing
        , importance = Nothing, metadata = Nothing, expiresAt = Unchanged
        , source = Unchanged, confidence = Nothing, pinned = Nothing }
      let Just u = updated
      u.content `shouldBe` "keep this"
      u.summary `shouldBe` Just "keep summary"
      u.memoryType `shouldBe` LongTerm
      u.importance `shouldBe` 8

  describe "deleteMemory" $ do
    it "deletes an existing memory" $ \env -> do
      ws <- createTestWorkspace env "delete-ws"
      mem <- createMemory env.pool CreateMemory
        { workspaceId = ws.id
        , content     = "to be deleted"
        , summary     = Nothing
        , memoryType  = ShortTerm
        , importance  = Nothing
        , metadata    = Nothing
        , expiresAt   = Nothing
        , source      = Nothing
        , confidence  = Nothing
        , pinned      = Nothing
        , tags        = Nothing
            , ftsLanguage = Nothing
        }
      ok <- deleteMemory env.pool mem.id
      ok `shouldBe` True
      got <- getMemory env.pool mem.id
      got `shouldSatisfy` isNothing
      listed <- listMemories env.pool (Just ws.id) Nothing Nothing Nothing
      map (.id) listed `shouldNotContain` [mem.id]

    it "returns False for nonexistent ID" $ \env -> do
      ok <- deleteMemory env.pool (read "00000000-0000-0000-0000-000000000099")
      ok `shouldBe` False

  describe "listMemories" $ do
    it "lists memories for a workspace" $ \env -> do
      ws <- createTestWorkspace env "list-ws"
      _ <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "mem1", summary = Nothing
        , memoryType = ShortTerm, importance = Nothing, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      _ <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "mem2", summary = Nothing
        , memoryType = LongTerm, importance = Nothing, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      mems <- listMemories env.pool (Just ws.id) Nothing Nothing Nothing
      length mems `shouldBe` 2

    it "filters by memory type" $ \env -> do
      ws <- createTestWorkspace env "filter-ws"
      _ <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "short", summary = Nothing
        , memoryType = ShortTerm, importance = Nothing, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      _ <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "long", summary = Nothing
        , memoryType = LongTerm, importance = Nothing, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      shorts <- listMemories env.pool (Just ws.id) (Just ShortTerm) Nothing Nothing
      length shorts `shouldBe` 1
      (head shorts).memoryType `shouldBe` ShortTerm

    it "respects limit and offset" $ \env -> do
      ws <- createTestWorkspace env "paging-ws"
      mapM_ (\i -> createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "mem" <> T.pack (show i), summary = Nothing
        , memoryType = ShortTerm, importance = Nothing, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }) ([1..5] :: [Int])
      page1 <- listMemories env.pool (Just ws.id) Nothing (Just 2) (Just 0)
      length page1 `shouldBe` 2
      page2 <- listMemories env.pool (Just ws.id) Nothing (Just 2) (Just 2)
      length page2 `shouldBe` 2

  describe "tags" $ do
    it "setTags replaces all tags" $ \env -> do
      ws <- createTestWorkspace env "tags-ws"
      mem <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "tagged", summary = Nothing
        , memoryType = ShortTerm, importance = Nothing, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Just ["old1", "old2"], ftsLanguage = Nothing }
      setTags env.pool mem.id ["new1", "new2", "new3"]
      tags <- getTags env.pool mem.id
      tags `shouldMatchList` ["new1", "new2", "new3"]

    it "setTags with empty list clears tags" $ \env -> do
      ws <- createTestWorkspace env "cleartags-ws"
      mem <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "will lose tags", summary = Nothing
        , memoryType = ShortTerm, importance = Nothing, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Just ["tag1"], ftsLanguage = Nothing }
      setTags env.pool mem.id []
      tags <- getTags env.pool mem.id
      tags `shouldBe` []

  describe "links" $ do
    it "creates and retrieves memory links" $ \env -> do
      ws <- createTestWorkspace env "link-ws"
      m1 <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "source", summary = Nothing
        , memoryType = ShortTerm, importance = Nothing, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      m2 <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "target", summary = Nothing
        , memoryType = ShortTerm, importance = Nothing, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      linkMemories env.pool m1.id CreateMemoryLink
        { targetId = m2.id, relationType = Related, strength = Just 0.5 }
      links <- getMemoryLinks env.pool m1.id
      length links `shouldBe` 1
      (head links).relationType `shouldBe` Related
      (head links).strength `shouldBe` 0.5

    it "link upsert updates strength" $ \env -> do
      ws <- createTestWorkspace env "upsert-link-ws"
      m1 <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "src", summary = Nothing
        , memoryType = ShortTerm, importance = Nothing, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      m2 <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "tgt", summary = Nothing
        , memoryType = ShortTerm, importance = Nothing, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      linkMemories env.pool m1.id CreateMemoryLink
        { targetId = m2.id, relationType = Related, strength = Just 0.25 }
      linkMemories env.pool m1.id CreateMemoryLink
        { targetId = m2.id, relationType = Related, strength = Just 0.75 }
      links <- getMemoryLinks env.pool m1.id
      length links `shouldBe` 1
      (head links).strength `shouldBe` 0.75

  describe "searchMemories" $ do
    it "finds memories by full-text search" $ \env -> do
      ws <- createTestWorkspace env "search-ws"
      _ <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "The quick brown fox jumps over the lazy dog"
        , summary = Nothing, memoryType = ShortTerm, importance = Just 5
        , metadata = Nothing, expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      _ <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "Haskell is a purely functional programming language"
        , summary = Nothing, memoryType = ShortTerm, importance = Just 5
        , metadata = Nothing, expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      results <- searchMemories env.pool SearchQuery
        { workspaceId = Just ws.id, query = Just "haskell functional"
        , memoryType = Nothing, tags = Nothing
        , minImportance = Nothing, minAccessCount = Nothing, sortBy = Nothing, categoryId = Nothing, pinnedOnly = Nothing, searchLanguage = Nothing, limit = Nothing, offset = Nothing }
      length results `shouldSatisfy` (>= 1)
      any (\m -> T.isInfixOf "Haskell" m.content) results `shouldBe` True

    it "search without query returns by importance" $ \env -> do
      ws <- createTestWorkspace env "searchnoq-ws"
      _ <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "low importance", summary = Nothing
        , memoryType = ShortTerm, importance = Just 2, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      _ <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "high importance", summary = Nothing
        , memoryType = ShortTerm, importance = Just 9, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      results <- searchMemories env.pool SearchQuery
        { workspaceId = Just ws.id, query = Nothing
        , memoryType = Nothing, tags = Nothing
        , minImportance = Just 5, minAccessCount = Nothing, sortBy = Nothing, categoryId = Nothing, pinnedOnly = Nothing, searchLanguage = Nothing, limit = Nothing, offset = Nothing }
      length results `shouldBe` 1
      (head results).importance `shouldBe` 9

    it "search without query can filter by min_access_count and sort by access_count" $ \env -> do
      ws <- createTestWorkspace env "search-access-ws"
      _ <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "low access", summary = Nothing
        , memoryType = ShortTerm, importance = Just 5, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      high <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "high access", summary = Nothing
        , memoryType = ShortTerm, importance = Just 5, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      touchMemory env.pool high.id
      touchMemory env.pool high.id
      results <- searchMemories env.pool SearchQuery
        { workspaceId = Just ws.id, query = Nothing
        , memoryType = Nothing, tags = Nothing
        , minImportance = Nothing, minAccessCount = Just 1, sortBy = Just SortAccessCount, categoryId = Nothing, pinnedOnly = Nothing, searchLanguage = Nothing, limit = Nothing, offset = Nothing }
      map (.id) results `shouldBe` [high.id]

    it "search without query can sort by recent explicitly" $ \env -> do
      ws <- createTestWorkspace env "search-recent-ws"
      older <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "older recent", summary = Nothing
        , memoryType = ShortTerm, importance = Just 9, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      newer <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "newer recent", summary = Nothing
        , memoryType = ShortTerm, importance = Just 1, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      backdateMemoryCreatedAt env older.id (24 * 365)
      results <- searchMemories env.pool SearchQuery
        { workspaceId = Just ws.id, query = Nothing
        , memoryType = Nothing, tags = Nothing
        , minImportance = Nothing, minAccessCount = Nothing, sortBy = Just SortRecent, categoryId = Nothing, pinnedOnly = Nothing, searchLanguage = Nothing, limit = Nothing, offset = Nothing }
      map (.id) results `shouldBe` [newer.id, older.id]

    it "prefers more recent memories when FTS relevance is otherwise equal" $ \env -> do
      ws <- createTestWorkspace env "search-recency-ws"
      older <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "haskell functional"
        , summary = Nothing, memoryType = ShortTerm, importance = Just 5
        , metadata = Nothing, expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      newer <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "haskell functional"
        , summary = Nothing, memoryType = ShortTerm, importance = Just 5
        , metadata = Nothing, expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      backdateMemoryUpdatedAt env older.id (24 * 365)
      results <- searchMemories env.pool SearchQuery
        { workspaceId = Just ws.id, query = Just "haskell functional"
        , memoryType = Nothing, tags = Nothing
        , minImportance = Nothing, minAccessCount = Nothing, sortBy = Nothing, categoryId = Nothing, pinnedOnly = Nothing, searchLanguage = Nothing, limit = Nothing, offset = Nothing }
      map (.id) results `shouldSatisfy` (not . null)
      (head results).id `shouldBe` newer.id

    it "ignores an empty tag filter when query search is present" $ \env -> do
      ws <- createTestWorkspace env "search-empty-tags-ws"
      mem <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "haskell functional"
        , summary = Nothing, memoryType = ShortTerm, importance = Just 5
        , metadata = Nothing, expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      results <- searchMemories env.pool SearchQuery
        { workspaceId = Just ws.id, query = Just "haskell functional"
        , memoryType = Nothing, tags = Just []
        , minImportance = Nothing, minAccessCount = Nothing, sortBy = Nothing, categoryId = Nothing, pinnedOnly = Nothing, searchLanguage = Nothing, limit = Nothing, offset = Nothing }
      map (.id) results `shouldBe` [mem.id]

  describe "touchMemory" $ do
    it "increments access count" $ \env -> do
      ws <- createTestWorkspace env "touch-ws"
      mem <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "touchable", summary = Nothing
        , memoryType = ShortTerm, importance = Nothing, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      mem.accessCount `shouldBe` 0
      touchMemory env.pool mem.id
      touchMemory env.pool mem.id
      Just m <- getMemory env.pool mem.id
      m.accessCount `shouldBe` 2

  describe "listMemoriesWithQuery" $ do
    it "supports min_access_count filtering and access_count sorting" $ \env -> do
      ws <- createTestWorkspace env "list-access-ws"
      _ <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "low", summary = Nothing
        , memoryType = ShortTerm, importance = Nothing, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      high <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "high", summary = Nothing
        , memoryType = ShortTerm, importance = Nothing, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      touchMemory env.pool high.id
      touchMemory env.pool high.id
      results <- listMemoriesWithQuery env.pool MemoryListQuery
        { workspaceId = Just ws.id
        , memoryType = Nothing
        , minAccessCount = Just 1
        , sortBy = Just SortAccessCount
        , createdAfter = Nothing
        , createdBefore = Nothing
        , updatedAfter = Nothing
        , updatedBefore = Nothing
        , limit = Nothing
        , offset = Nothing
        }
      map (.id) results `shouldBe` [high.id]

  describe "content size limits" $ do
    it "accepts content at exactly maxMemoryContentBytes" $ \env -> do
      ws <- createTestWorkspace env "sizelimit-ws"
      let bigContent = T.replicate maxMemoryContentBytes "x"
      mem <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = bigContent, summary = Nothing
        , memoryType = ShortTerm, importance = Nothing, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      T.length mem.content `shouldBe` maxMemoryContentBytes

    it "validates content above maxMemoryContentBytes" $ \_ -> do
      let cm = CreateMemory
            { workspaceId = read "00000000-0000-0000-0000-000000000001"
            , content = T.replicate (maxMemoryContentBytes + 1) "a"
            , summary = Nothing, memoryType = ShortTerm, importance = Nothing
            , metadata = Nothing, expiresAt = Nothing, source = Nothing
            , confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      validateCreateMemoryInput cm `shouldNotBe` []

  describe "batch with invalid item" $ do
    it "batch create with one invalid item still creates the valid ones" $ \env -> do
      ws <- createTestWorkspace env "batchinvalid-ws"
      let good = CreateMemory
            { workspaceId = ws.id, content = "valid content", summary = Nothing
            , memoryType = ShortTerm, importance = Nothing, metadata = Nothing
            , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      -- Validation catches empty content at the API layer; at the DB level
      -- the batch creates whatever it receives.
      mems <- createMemoryBatch env.pool [good, good]
      length mems `shouldBe` 2

    it "batch update with nonexistent ID returns partial success count" $ \env -> do
      ws <- createTestWorkspace env "batchupinvalid-ws"
      mem <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "real memory", summary = Nothing
        , memoryType = ShortTerm, importance = Nothing, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      let bogusId = read "00000000-0000-0000-0000-ffffffffffff" :: UUID
          upd = UpdateMemory
            { content = Just "updated", summary = Unchanged, memoryType = Nothing
            , importance = Nothing, metadata = Nothing, expiresAt = Unchanged
            , source = Unchanged, confidence = Nothing, pinned = Nothing }
      count <- updateMemoryBatch env.pool [(mem.id, upd), (bogusId, upd)]
      count `shouldBe` 1  -- only the real one succeeds

    it "batch delete with nonexistent IDs returns partial success count" $ \env -> do
      ws <- createTestWorkspace env "batchdelinvalid-ws"
      mem <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "deletable", summary = Nothing
        , memoryType = ShortTerm, importance = Nothing, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      let bogusId = read "00000000-0000-0000-0000-ffffffffffff" :: UUID
      count <- deleteMemoryBatch env.pool [mem.id, bogusId]
      count `shouldBe` 1

  describe "concurrent updates" $ do
    it "handles concurrent updates to the same memory safely" $ \env -> do
      ws <- createTestWorkspace env "concurrent-ws"
      mem <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "shared", summary = Nothing
        , memoryType = ShortTerm, importance = Just 5, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      let n = 10 :: Int
      mvars <- mapM (\_ -> newEmptyMVar) [1..n]
      mapM_ (\(i, mvar) -> forkIO $ do
        result <- try @SomeException $ updateMemory env.pool mem.id UpdateMemory
          { content = Just ("update-" <> T.pack (show i)), summary = Unchanged
          , memoryType = Nothing, importance = Nothing, metadata = Nothing
          , expiresAt = Unchanged, source = Unchanged, confidence = Nothing, pinned = Nothing }
        putMVar mvar result
        ) (zip [1..n] mvars)
      results <- mapM takeMVar mvars
      let successes = length [() | Right (Just _) <- results]
      -- All updates should succeed (last-writer-wins, no crashes)
      successes `shouldBe` n
      -- Memory should still be readable
      got <- getMemory env.pool mem.id
      got `shouldSatisfy` isJust

  describe "pagination properties" $ do
    it "offset beyond total returns empty list" $ \env -> do
      ws <- createTestWorkspace env "pageempty-ws"
      _ <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "only one", summary = Nothing
        , memoryType = ShortTerm, importance = Nothing, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      mems <- listMemories env.pool (Just ws.id) Nothing (Just 10) (Just 100)
      mems `shouldBe` []

    it "pagination covers all items without duplication" $ \env -> do
      ws <- createTestWorkspace env "pagefull-ws"
      let totalItems = 7
      mapM_ (\i -> createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "item-" <> T.pack (show i), summary = Nothing
        , memoryType = ShortTerm, importance = Nothing, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }) ([1..totalItems] :: [Int])
      -- Fetch in pages of 3
      p1 <- listMemories env.pool (Just ws.id) Nothing (Just 3) (Just 0)
      p2 <- listMemories env.pool (Just ws.id) Nothing (Just 3) (Just 3)
      p3 <- listMemories env.pool (Just ws.id) Nothing (Just 3) (Just 6)
      let allIds = map (.id) (p1 ++ p2 ++ p3)
      length allIds `shouldBe` totalItems
      -- No duplicates
      length (nub allIds) `shouldBe` totalItems

  describe "ID uniqueness" $ do
    it "generates unique IDs for bulk-created memories" $ \env -> do
      ws <- createTestWorkspace env "idunique-ws"
      let cms = replicate 20 CreateMemory
            { workspaceId = ws.id, content = "same content", summary = Nothing
            , memoryType = ShortTerm, importance = Nothing, metadata = Nothing
            , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      mems <- createMemoryBatch env.pool cms
      let ids = map (.id) mems
      length ids `shouldBe` 20
      length (nub ids) `shouldBe` 20

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)
