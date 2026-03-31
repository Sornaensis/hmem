{-# OPTIONS_GHC -Wno-x-partial #-}

module HMem.MCP.ToolsSpec (spec) where

import Data.Aeson
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KM
import Data.Either (isLeft, isRight)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Test.Hspec

import HMem.MCP.Tools
import HMem.Types

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | A fixed UUID for tests.
testUUID :: Text
testUUID = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"

testUUID2 :: Text
testUUID2 = "11111111-2222-3333-4444-555555555555"

parsedUUID :: UUID.UUID
parsedUUID = read "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"

parsedUUID2 :: UUID.UUID
parsedUUID2 = read "11111111-2222-3333-4444-555555555555"

------------------------------------------------------------------------
-- parseToolCall
------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "parseToolCall" $ do

    it "parses memory_create with required fields" $ do
      let args = object
            [ "workspace_id" .= testUUID
            , "content"      .= ("hello" :: Text)
            , "memory_type"  .= ("short_term" :: Text)
            ]
      case parseToolCall "memory_create" args of
        Right (MemoryCreate cm) -> do
          cm.workspaceId `shouldBe` parsedUUID
          cm.content `shouldBe` "hello"
          cm.memoryType `shouldBe` ShortTerm
        other -> expectationFailure $ "Expected MemoryCreate, got: " <> show other

    it "parses memory_create with optional fields" $ do
      let args = object
            [ "workspace_id" .= testUUID
            , "content"      .= ("hello" :: Text)
            , "memory_type"  .= ("long_term" :: Text)
            , "importance"   .= (8 :: Int)
            , "summary"      .= ("sum" :: Text)
            , "metadata"     .= object ["source" .= ("unit-test" :: Text)]
            , "expires_at"   .= ("2026-03-31T10:00:00Z" :: Text)
            , "pinned"       .= True
            , "tags"         .= (["tag1", "tag2"] :: [Text])
            , "fts_language" .= ("spanish" :: Text)
            ]
      case parseToolCall "memory_create" args of
        Right (MemoryCreate cm) -> do
          cm.memoryType `shouldBe` LongTerm
          cm.importance `shouldBe` Just 8
          cm.summary `shouldBe` Just "sum"
          cm.metadata `shouldBe` Just (object ["source" .= ("unit-test" :: Text)])
          cm.expiresAt `shouldBe` Just (read "2026-03-31 10:00:00 UTC")
          cm.pinned `shouldBe` Just True
          cm.tags `shouldBe` Just ["tag1", "tag2"]
          cm.ftsLanguage `shouldBe` Just "spanish"
        other -> expectationFailure $ "Expected MemoryCreate, got: " <> show other

    it "rejects memory_create missing required fields" $ do
      let args = object [ "workspace_id" .= testUUID ]  -- missing content, memory_type
      parseToolCall "memory_create" args `shouldSatisfy` isLeft

    it "parses memory_get" $ do
      let args = object [ "memory_id" .= testUUID ]
      case parseToolCall "memory_get" args of
        Right (MemoryGet mid) -> mid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected MemoryGet, got: " <> show other

    it "parses memory_delete" $ do
      let args = object [ "memory_id" .= testUUID ]
      case parseToolCall "memory_delete" args of
        Right (MemoryDelete mid) -> mid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected MemoryDelete, got: " <> show other

    it "parses memory_restore" $ do
      let args = object [ "memory_id" .= testUUID ]
      case parseToolCall "memory_restore" args of
        Right (MemoryRestore mid) -> mid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected MemoryRestore, got: " <> show other

    it "parses memory_purge" $ do
      let args = object [ "memory_id" .= testUUID ]
      case parseToolCall "memory_purge" args of
        Right (MemoryPurge mid) -> mid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected MemoryPurge, got: " <> show other

    it "parses memory_update" $ do
      let args = object
            [ "memory_id" .= testUUID
            , "content"   .= ("updated" :: Text)
            ]
      case parseToolCall "memory_update" args of
        Right (MemoryUpdate mid _um) -> mid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected MemoryUpdate, got: " <> show other

    it "parses memory_search with no filters" $ do
      let args = object []
      parseToolCall "memory_search" args `shouldSatisfy` isRight

    it "parses memory_search with query and filters" $ do
      let args = object
            [ "workspace_id" .= testUUID
            , "query"        .= ("find this" :: Text)
            , "memory_type"  .= ("long_term" :: Text)
            , "limit"        .= (10 :: Int)
            , "offset"       .= (5 :: Int)
            ]
      case parseToolCall "memory_search" args of
        Right (MemorySearch sq _detail) -> do
          sq.workspaceId `shouldBe` Just parsedUUID
          sq.query `shouldBe` Just "find this"
          sq.memoryType `shouldBe` Just LongTerm
          sq.limit `shouldBe` Just 10
          sq.offset `shouldBe` Just 5
        other -> expectationFailure $ "Expected MemorySearch, got: " <> show other

    it "parses memory_list with date filters" $ do
      let args = object
            [ "workspace_id" .= testUUID
            , "memory_type" .= ("long_term" :: Text)
            , "created_after" .= ("2026-03-30T10:00:00Z" :: Text)
            ]
      case parseToolCall "memory_list" args of
        Right (MemoryList mq _detail) -> do
          mq.workspaceId `shouldBe` Just parsedUUID
          mq.memoryType `shouldBe` Just LongTerm
          mq.createdAfter `shouldBe` Just (read "2026-03-30 10:00:00 UTC")
        other -> expectationFailure $ "Expected MemoryList, got: " <> show other

    it "parses memory_link" $ do
      let args = object
            [ "source_id"     .= testUUID
            , "target_id"     .= testUUID2
            , "relation_type" .= ("related" :: Text)
            ]
      case parseToolCall "memory_link" args of
        Right (LinkMemories sid _cl) -> sid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected LinkMemories, got: " <> show other

    it "parses memory_set_tags" $ do
      let args = object
            [ "memory_id" .= testUUID
            , "tags"      .= (["a", "b"] :: [Text])
            ]
      case parseToolCall "memory_set_tags" args of
        Right (MemorySetTags mid tags) -> do
          mid `shouldBe` parsedUUID
          tags `shouldBe` ["a", "b"]
        other -> expectationFailure $ "Expected MemorySetTags, got: " <> show other

    it "parses memory_create_batch" $ do
      let item = object
            [ "workspace_id" .= testUUID
            , "content"      .= ("batch item" :: Text)
            , "memory_type"  .= ("short_term" :: Text)
            ]
          args = object [ "memories" .= [item, item] ]
      case parseToolCall "memory_create_batch" args of
        Right (MemoryCreateBatch cms) -> length cms `shouldBe` 2
        other -> expectationFailure $ "Expected MemoryCreateBatch, got: " <> show other

    it "parses memory_pin and memory_unpin" $ do
      let args = object [ "memory_id" .= testUUID ]
      case parseToolCall "memory_pin" args of
        Right (MemoryPin mid) -> mid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected MemoryPin, got: " <> show other
      case parseToolCall "memory_unpin" args of
        Right (MemoryUnpin mid) -> mid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected MemoryUnpin, got: " <> show other

    it "parses memory_adjust_importance" $ do
      let args = object [ "memory_id" .= testUUID, "importance" .= (7 :: Int) ]
      case parseToolCall "memory_adjust_importance" args of
        Right (MemoryAdjustImp mid imp) -> do
          mid `shouldBe` parsedUUID
          imp `shouldBe` 7
        other -> expectationFailure $ "Expected MemoryAdjustImp, got: " <> show other

    it "rejects unknown tool name" $ do
      parseToolCall "nonexistent_tool" (object []) `shouldSatisfy` isLeft

  describe "validateToolCall" $ do

    it "clamps importance to 1-10" $ do
      let cm = CreateMemory
            { workspaceId = parsedUUID, content = "x", summary = Nothing
            , memoryType = ShortTerm, importance = Just 99
            , metadata = Nothing, expiresAt = Nothing, source = Nothing
            , confidence = Nothing, pinned = Nothing, tags = Nothing
            , ftsLanguage = Nothing
            }
      case validateToolCall (MemoryCreate cm) of
        Right (MemoryCreate cm') -> cm'.importance `shouldBe` Just 10
        other -> expectationFailure $ "Expected MemoryCreate, got: " <> show other

    it "clamps importance minimum to 1" $ do
      let cm = CreateMemory
            { workspaceId = parsedUUID, content = "x", summary = Nothing
            , memoryType = ShortTerm, importance = Just (-5)
            , metadata = Nothing, expiresAt = Nothing, source = Nothing
            , confidence = Nothing, pinned = Nothing, tags = Nothing
            , ftsLanguage = Nothing
            }
      case validateToolCall (MemoryCreate cm) of
        Right (MemoryCreate cm') -> cm'.importance `shouldBe` Just 1
        other -> expectationFailure $ "Expected MemoryCreate, got: " <> show other

    it "clamps confidence to 0.0-1.0" $ do
      let cm = CreateMemory
            { workspaceId = parsedUUID, content = "x", summary = Nothing
            , memoryType = ShortTerm, importance = Nothing
            , metadata = Nothing, expiresAt = Nothing, source = Nothing
            , confidence = Just 5.0, pinned = Nothing, tags = Nothing
            , ftsLanguage = Nothing
            }
      case validateToolCall (MemoryCreate cm) of
        Right (MemoryCreate cm') -> cm'.confidence `shouldBe` Just 1.0
        other -> expectationFailure $ "Expected MemoryCreate, got: " <> show other

    it "rejects invalid fts_language on memory_create" $ do
      let cm = CreateMemory
            { workspaceId = parsedUUID, content = "x", summary = Nothing
            , memoryType = ShortTerm, importance = Nothing
            , metadata = Nothing, expiresAt = Nothing, source = Nothing
            , confidence = Nothing, pinned = Nothing, tags = Nothing
            , ftsLanguage = Just "klingon"
            }
      validateToolCall (MemoryCreate cm) `shouldSatisfy` isLeft

    it "accepts valid fts_language" $ do
      let cm = CreateMemory
            { workspaceId = parsedUUID, content = "x", summary = Nothing
            , memoryType = ShortTerm, importance = Nothing
            , metadata = Nothing, expiresAt = Nothing, source = Nothing
            , confidence = Nothing, pinned = Nothing, tags = Nothing
            , ftsLanguage = Just "german"
            }
      validateToolCall (MemoryCreate cm) `shouldSatisfy` isRight

    it "rejects batch with >100 items" $ do
      let cm = CreateMemory
            { workspaceId = parsedUUID, content = "x", summary = Nothing
            , memoryType = ShortTerm, importance = Nothing
            , metadata = Nothing, expiresAt = Nothing, source = Nothing
            , confidence = Nothing, pinned = Nothing, tags = Nothing
            , ftsLanguage = Nothing
            }
      validateToolCall (MemoryCreateBatch (replicate 101 cm)) `shouldSatisfy` isLeft

    it "accepts batch with <=100 items" $ do
      let cm = CreateMemory
            { workspaceId = parsedUUID, content = "x", summary = Nothing
            , memoryType = ShortTerm, importance = Nothing
            , metadata = Nothing, expiresAt = Nothing, source = Nothing
            , confidence = Nothing, pinned = Nothing, tags = Nothing
            , ftsLanguage = Nothing
            }
      validateToolCall (MemoryCreateBatch (replicate 100 cm)) `shouldSatisfy` isRight

    it "rejects batch items with empty content" $ do
      let cm = CreateMemory
            { workspaceId = parsedUUID, content = "   ", summary = Nothing
            , memoryType = ShortTerm, importance = Nothing
            , metadata = Nothing, expiresAt = Nothing, source = Nothing
            , confidence = Nothing, pinned = Nothing, tags = Nothing
            , ftsLanguage = Nothing
            }
      validateToolCall (MemoryCreateBatch [cm]) `shouldSatisfy` isLeft

    it "rejects oversized project descriptions" $ do
      let cp = CreateProject
            { workspaceId = parsedUUID
            , parentId = Nothing
            , name = "project"
            , description = Just (T.replicate (maxDescriptionBytes + 1) "a")
            , priority = Nothing
            , metadata = Nothing
            }
      validateToolCall (ProjectCreate cp) `shouldSatisfy` isLeft

    it "clamps memory_adjust_importance to 1-10" $ do
      case validateToolCall (MemoryAdjustImp parsedUUID 50) of
        Right (MemoryAdjustImp _ imp) -> imp `shouldBe` 10
        other -> expectationFailure $ "Expected MemoryAdjustImp, got: " <> show other

    it "clamps search limit to 1-200" $ do
      let sq = SearchQuery
            { workspaceId = Nothing, query = Nothing
            , memoryType = Nothing, tags = Nothing
            , minImportance = Nothing, categoryId = Nothing
            , pinnedOnly = Nothing, searchLanguage = Nothing
            , limit = Just 999, offset = Just 0
            }
      case validateToolCall (MemorySearch sq False) of
        Right (MemorySearch sq' _) -> sq'.limit `shouldBe` Just 200
        other -> expectationFailure $ "Expected MemorySearch, got: " <> show other

    it "rejects empty embedding vector" $ do
      validateToolCall (MemorySetEmbedding parsedUUID []) `shouldSatisfy` isLeft

    it "accepts non-empty embedding vector" $ do
      validateToolCall (MemorySetEmbedding parsedUUID [0.1, 0.2, 0.3]) `shouldSatisfy` isRight

    it "rejects task_list with neither workspace_id nor project_id" $ do
      validateToolCall (TaskList TaskListQuery
        { workspaceId = Nothing
        , projectId = Nothing
        , status = Nothing
        , priority = Nothing
        , createdAfter = Nothing
        , createdBefore = Nothing
        , updatedAfter = Nothing
        , updatedBefore = Nothing
        , limit = Nothing
        , offset = Nothing
        }) `shouldSatisfy` isLeft

    it "accepts task_list with workspace_id" $ do
      validateToolCall (TaskList TaskListQuery
        { workspaceId = Just parsedUUID
        , projectId = Nothing
        , status = Nothing
        , priority = Nothing
        , createdAfter = Nothing
        , createdBefore = Nothing
        , updatedAfter = Nothing
        , updatedBefore = Nothing
        , limit = Nothing
        , offset = Nothing
        }) `shouldSatisfy` isRight

    it "passes through simple tool calls unchanged" $ do
      validateToolCall (MemoryGet parsedUUID) `shouldBe` Right (MemoryGet parsedUUID)
      validateToolCall (MemoryDelete parsedUUID) `shouldBe` Right (MemoryDelete parsedUUID)
      validateToolCall (MemoryRestore parsedUUID) `shouldBe` Right (MemoryRestore parsedUUID)
      validateToolCall (MemoryPurge parsedUUID) `shouldBe` Right (MemoryPurge parsedUUID)

  describe "parseToolCall (project tools)" $ do

    it "parses project_create" $ do
      let args = object
            [ "workspace_id" .= testUUID
            , "name"         .= ("My Project" :: Text)
            , "metadata"     .= object ["kind" .= ("epic" :: Text)]
            ]
      case parseToolCall "project_create" args of
        Right (ProjectCreate cp) -> do
          cp.workspaceId `shouldBe` parsedUUID
          cp.name `shouldBe` "My Project"
          cp.metadata `shouldBe` Just (object ["kind" .= ("epic" :: Text)])
        other -> expectationFailure $ "Expected ProjectCreate, got: " <> show other

    it "parses project_get" $ do
      let args = object [ "project_id" .= testUUID ]
      case parseToolCall "project_get" args of
        Right (ProjectGet pid) -> pid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected ProjectGet, got: " <> show other

    it "parses project_update" $ do
      let args = object
            [ "project_id" .= testUUID
            , "name"       .= ("Renamed" :: Text)
            , "parent_id"  .= testUUID2
            , "metadata"   .= object ["lane" .= ("current" :: Text)]
            ]
      case parseToolCall "project_update" args of
        Right (ProjectUpdate pid up) -> do
          pid `shouldBe` parsedUUID
          up.parentId `shouldBe` SetTo parsedUUID2
          up.metadata `shouldBe` Just (object ["lane" .= ("current" :: Text)])
        other -> expectationFailure $ "Expected ProjectUpdate, got: " <> show other

    it "parses project_update with null parent_id to clear hierarchy" $ do
      let args = object
            [ "project_id" .= testUUID
            , "parent_id"  .= Null
            ]
      case parseToolCall "project_update" args of
        Right (ProjectUpdate pid up) -> do
          pid `shouldBe` parsedUUID
          up.parentId `shouldBe` SetNull
        other -> expectationFailure $ "Expected ProjectUpdate, got: " <> show other

    it "parses project_delete" $ do
      let args = object [ "project_id" .= testUUID ]
      case parseToolCall "project_delete" args of
        Right (ProjectDelete pid) -> pid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected ProjectDelete, got: " <> show other

    it "parses project_restore" $ do
      let args = object [ "project_id" .= testUUID ]
      case parseToolCall "project_restore" args of
        Right (ProjectRestore pid) -> pid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected ProjectRestore, got: " <> show other

    it "parses project_purge" $ do
      let args = object [ "project_id" .= testUUID ]
      case parseToolCall "project_purge" args of
        Right (ProjectPurge pid) -> pid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected ProjectPurge, got: " <> show other

    it "parses project_list with status filter" $ do
      let args = object
            [ "workspace_id" .= testUUID
            , "status"       .= ("active" :: Text)
            , "limit"        .= (10 :: Int)
            , "updated_before" .= ("2026-03-30T12:00:00Z" :: Text)
            ]
      case parseToolCall "project_list" args of
        Right (ProjectList pq) -> do
          pq.workspaceId `shouldBe` Just parsedUUID
          pq.status `shouldBe` Just ProjActive
          pq.limit `shouldBe` Just 10
          pq.updatedBefore `shouldBe` Just (read "2026-03-30 12:00:00 UTC")
        other -> expectationFailure $ "Expected ProjectList, got: " <> show other

    it "parses project_link_memory and project_unlink_memory" $ do
      let args = object [ "project_id" .= testUUID, "memory_id" .= testUUID2 ]
      case parseToolCall "project_link_memory" args of
        Right (ProjectLinkMem pid mid) -> do
          pid `shouldBe` parsedUUID
          mid `shouldBe` parsedUUID2
        other -> expectationFailure $ "Expected ProjectLinkMem, got: " <> show other
      case parseToolCall "project_unlink_memory" args of
        Right (ProjectUnlinkMem pid mid) -> do
          pid `shouldBe` parsedUUID
          mid `shouldBe` parsedUUID2
        other -> expectationFailure $ "Expected ProjectUnlinkMem, got: " <> show other

    it "parses project_list_memories" $ do
      let args = object [ "project_id" .= testUUID ]
      case parseToolCall "project_list_memories" args of
        Right (ProjectListMem pid) -> pid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected ProjectListMem, got: " <> show other

    it "parses project_delete_batch and project_update_batch" $ do
      let deleteArgs = object [ "ids" .= ([testUUID, testUUID2] :: [Text]) ]
          updateArgs = object
            [ "items" .= [ object [ "id" .= testUUID, "name" .= ("Renamed" :: Text) ] ]
            ]
      case parseToolCall "project_delete_batch" deleteArgs of
        Right (ProjectDeleteBatch ids) -> ids `shouldBe` [parsedUUID, parsedUUID2]
        other -> expectationFailure $ "Expected ProjectDeleteBatch, got: " <> show other
      case parseToolCall "project_update_batch" updateArgs of
        Right (ProjectUpdateBatch items) -> length items `shouldBe` 1
        other -> expectationFailure $ "Expected ProjectUpdateBatch, got: " <> show other

  describe "parseToolCall (task tools)" $ do

    it "parses task_create" $ do
      let args = object
            [ "workspace_id" .= testUUID
            , "title"        .= ("Do something" :: Text)
            , "metadata"     .= object ["estimate" .= (3 :: Int)]
            ]
      case parseToolCall "task_create" args of
        Right (TaskCreate ct) -> do
          ct.workspaceId `shouldBe` parsedUUID
          ct.title `shouldBe` "Do something"
          ct.metadata `shouldBe` Just (object ["estimate" .= (3 :: Int)])
        other -> expectationFailure $ "Expected TaskCreate, got: " <> show other

    it "parses task_get" $ do
      let args = object [ "task_id" .= testUUID ]
      case parseToolCall "task_get" args of
        Right (TaskGet tid) -> tid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected TaskGet, got: " <> show other

    it "parses task_update" $ do
      let args = object
            [ "task_id" .= testUUID
            , "project_id" .= testUUID2
            , "parent_id" .= Null
            , "metadata" .= object ["owner" .= ("agent" :: Text)]
            , "due_at" .= Null
            , "status"  .= ("in_progress" :: Text)
            ]
      case parseToolCall "task_update" args of
        Right (TaskUpdate tid ut) -> do
          tid `shouldBe` parsedUUID
          ut.projectId `shouldBe` SetTo parsedUUID2
          ut.parentId `shouldBe` SetNull
          ut.metadata `shouldBe` Just (object ["owner" .= ("agent" :: Text)])
          ut.dueAt `shouldBe` SetNull
        other -> expectationFailure $ "Expected TaskUpdate, got: " <> show other

    it "parses task_delete" $ do
      let args = object [ "task_id" .= testUUID ]
      case parseToolCall "task_delete" args of
        Right (TaskDelete tid) -> tid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected TaskDelete, got: " <> show other

    it "parses task_restore" $ do
      let args = object [ "task_id" .= testUUID ]
      case parseToolCall "task_restore" args of
        Right (TaskRestore tid) -> tid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected TaskRestore, got: " <> show other

    it "parses task_purge" $ do
      let args = object [ "task_id" .= testUUID ]
      case parseToolCall "task_purge" args of
        Right (TaskPurge tid) -> tid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected TaskPurge, got: " <> show other

    it "parses task_list with workspace filter" $ do
      let args = object
            [ "workspace_id" .= testUUID
            , "status"       .= ("todo" :: Text)
            , "priority"     .= (7 :: Int)
            , "limit"        .= (25 :: Int)
            ]
      case parseToolCall "task_list" args of
        Right (TaskList tq) -> do
          tq.workspaceId `shouldBe` Just parsedUUID
          tq.status `shouldBe` Just Todo
          tq.priority `shouldBe` Just 7
          tq.limit `shouldBe` Just 25
        other -> expectationFailure $ "Expected TaskList, got: " <> show other

    it "parses task_dependency_add" $ do
      let args = object [ "task_id" .= testUUID, "depends_on_id" .= testUUID2 ]
      case parseToolCall "task_dependency_add" args of
        Right (TaskDepAdd tid did) -> do
          tid `shouldBe` parsedUUID
          did `shouldBe` parsedUUID2
        other -> expectationFailure $ "Expected TaskDepAdd, got: " <> show other

    it "parses task_dependency_remove" $ do
      let args = object [ "task_id" .= testUUID, "depends_on_id" .= testUUID2 ]
      case parseToolCall "task_dependency_remove" args of
        Right (TaskDepRemove tid did) -> do
          tid `shouldBe` parsedUUID
          did `shouldBe` parsedUUID2
        other -> expectationFailure $ "Expected TaskDepRemove, got: " <> show other

    it "parses task_link_memory and task_unlink_memory" $ do
      let args = object [ "task_id" .= testUUID, "memory_id" .= testUUID2 ]
      case parseToolCall "task_link_memory" args of
        Right (TaskLinkMem tid mid) -> do
          tid `shouldBe` parsedUUID
          mid `shouldBe` parsedUUID2
        other -> expectationFailure $ "Expected TaskLinkMem, got: " <> show other
      case parseToolCall "task_unlink_memory" args of
        Right (TaskUnlinkMem tid mid) -> do
          tid `shouldBe` parsedUUID
          mid `shouldBe` parsedUUID2
        other -> expectationFailure $ "Expected TaskUnlinkMem, got: " <> show other

    it "parses task_list_memories" $ do
      let args = object [ "task_id" .= testUUID ]
      case parseToolCall "task_list_memories" args of
        Right (TaskListMem tid) -> tid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected TaskListMem, got: " <> show other

  describe "parseToolCall (workspace and category restore/batch tools)" $ do

    it "parses workspace_restore" $ do
      let args = object [ "workspace_id" .= testUUID ]
      case parseToolCall "workspace_restore" args of
        Right (WsRestore wid) -> wid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected WsRestore, got: " <> show other

    it "parses category_restore and category_list_memories" $ do
      let args = object [ "category_id" .= testUUID ]
      case parseToolCall "category_restore" args of
        Right (CategoryRestore cid) -> cid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected CategoryRestore, got: " <> show other
      case parseToolCall "category_list_memories" args of
        Right (CategoryListMem cid) -> cid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected CategoryListMem, got: " <> show other

    it "parses category_delete_batch and category_link_memories_batch" $ do
      let deleteArgs = object [ "ids" .= ([testUUID, testUUID2] :: [Text]) ]
          linkArgs = object
            [ "category_id" .= testUUID
            , "memory_ids" .= ([testUUID, testUUID2] :: [Text])
            ]
      case parseToolCall "category_delete_batch" deleteArgs of
        Right (CategoryDeleteBatch ids) -> ids `shouldBe` [parsedUUID, parsedUUID2]
        other -> expectationFailure $ "Expected CategoryDeleteBatch, got: " <> show other
      case parseToolCall "category_link_memories_batch" linkArgs of
        Right (CategoryLinkMemBatch cid mids) -> do
          cid `shouldBe` parsedUUID
          mids `shouldBe` [parsedUUID, parsedUUID2]
        other -> expectationFailure $ "Expected CategoryLinkMemBatch, got: " <> show other

  describe "validateToolCall (list clamping)" $ do

    it "clamps project_list limit to 1-200" $ do
      case validateToolCall (ProjectList ProjectListQuery
        { workspaceId = Just parsedUUID
        , status = Nothing
        , createdAfter = Nothing
        , createdBefore = Nothing
        , updatedAfter = Nothing
        , updatedBefore = Nothing
        , limit = Just 500
        , offset = Nothing
        }) of
        Right (ProjectList pq) -> pq.limit `shouldBe` Just 200
        other -> expectationFailure $ "Expected ProjectList, got: " <> show other

    it "rejects invalid task_list priority and time range" $ do
      validateToolCall (TaskList TaskListQuery
        { workspaceId = Just parsedUUID
        , projectId = Nothing
        , status = Nothing
        , priority = Just 99
        , createdAfter = Nothing
        , createdBefore = Nothing
        , updatedAfter = Just (read "2026-03-30 11:00:00 UTC")
        , updatedBefore = Just (read "2026-03-30 10:00:00 UTC")
        , limit = Nothing
        , offset = Nothing
        }) `shouldSatisfy` isLeft

    it "clamps category_list limit" $ do
      case validateToolCall (CategoryList Nothing (Just 0) Nothing) of
        Right (CategoryList _ ml _) -> ml `shouldBe` Just 1
        other -> expectationFailure $ "Expected CategoryList, got: " <> show other

    it "clamps workspace_list offset" $ do
      case validateToolCall (WorkspaceList Nothing (Just 99999)) of
        Right (WorkspaceList _ mo) -> mo `shouldBe` Just 10000
        other -> expectationFailure $ "Expected WorkspaceList, got: " <> show other

    it "validates project and category batch operations" $ do
      validateToolCall (ProjectDeleteBatch []) `shouldSatisfy` isLeft
      validateToolCall (CategoryDeleteBatch [parsedUUID]) `shouldBe` Right (CategoryDeleteBatch [parsedUUID])
      validateToolCall (CategoryLinkMemBatch parsedUUID []) `shouldSatisfy` isLeft

  describe "toolDefinitions" $ do

    it "advertises maxLength for memory content" $ do
      let Just schema = inputSchemaFor "memory_create"
          Just properties = objectField "properties" schema
          Just contentSchema = objectField "content" properties
      numberField "maxLength" contentSchema `shouldBe` Just (fromIntegral maxMemoryContentBytes)

    it "advertises batch size bounds" $ do
      let Just schema = inputSchemaFor "memory_create_batch"
          Just properties = objectField "properties" schema
          Just memoriesSchema = objectField "memories" properties
      numberField "minItems" memoriesSchema `shouldBe` Just 1
      numberField "maxItems" memoriesSchema `shouldBe` Just 100

    it "advertises list filter timestamps and task priority" $ do
      let Just memorySchema = inputSchemaFor "memory_list"
          Just memoryProps = objectField "properties" memorySchema
          Just projectSchema = inputSchemaFor "project_list"
          Just projectProps = objectField "properties" projectSchema
          Just taskSchema = inputSchemaFor "task_list"
          Just taskProps = objectField "properties" taskSchema
          createdAfterType = objectField "created_after" memoryProps >>= textField "type"
          updatedBeforeType = objectField "updated_before" projectProps >>= textField "type"
          priorityType = objectField "priority" taskProps >>= textField "type"
      createdAfterType `shouldBe` Just "string"
      updatedBeforeType `shouldBe` Just "string"
      priorityType `shouldBe` Just "integer"

    it "defines restore and new batch/category tools" $ do
      inputSchemaFor "memory_restore" `shouldSatisfy` (/= Nothing)
      inputSchemaFor "workspace_restore" `shouldSatisfy` (/= Nothing)
      inputSchemaFor "project_delete_batch" `shouldSatisfy` (/= Nothing)
      inputSchemaFor "project_update_batch" `shouldSatisfy` (/= Nothing)
      inputSchemaFor "category_list_memories" `shouldSatisfy` (/= Nothing)
      inputSchemaFor "category_link_memories_batch" `shouldSatisfy` (/= Nothing)

  describe "parseToolCall (saved view tools)" $ do

    it "parses saved_view_create" $ do
      let args = object
            [ "workspace_id" .= testUUID
            , "name"         .= ("My View" :: Text)
            , "entity_type"  .= ("memory_search" :: Text)
            , "query_params" .= object ["tags" .= (["haskell"] :: [Text])]
            ]
      case parseToolCall "saved_view_create" args of
        Right (SavedViewCreate csv) -> do
          csv.workspaceId `shouldBe` parsedUUID
          csv.name `shouldBe` "My View"
          csv.entityType `shouldBe` "memory_search"
        other -> expectationFailure $ "Expected SavedViewCreate, got: " <> show other

    it "parses saved_view_list" $ do
      let args = object
            [ "workspace_id" .= testUUID
            , "limit"        .= (20 :: Int)
            ]
      case parseToolCall "saved_view_list" args of
        Right (SavedViewList wid ml _mo) -> do
          wid `shouldBe` parsedUUID
          ml `shouldBe` Just 20
        other -> expectationFailure $ "Expected SavedViewList, got: " <> show other

    it "parses saved_view_get" $ do
      let args = object [ "view_id" .= testUUID ]
      case parseToolCall "saved_view_get" args of
        Right (SavedViewGet vid) -> vid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected SavedViewGet, got: " <> show other

    it "parses saved_view_update" $ do
      let args = object
            [ "view_id" .= testUUID
            , "name"    .= ("Renamed" :: Text)
            ]
      case parseToolCall "saved_view_update" args of
        Right (SavedViewUpdate vid usv) -> do
          vid `shouldBe` parsedUUID
          usv.name `shouldBe` Just "Renamed"
        other -> expectationFailure $ "Expected SavedViewUpdate, got: " <> show other

    it "parses saved_view_delete" $ do
      let args = object [ "view_id" .= testUUID ]
      case parseToolCall "saved_view_delete" args of
        Right (SavedViewDelete vid) -> vid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected SavedViewDelete, got: " <> show other

    it "parses saved_view_restore" $ do
      let args = object [ "view_id" .= testUUID ]
      case parseToolCall "saved_view_restore" args of
        Right (SavedViewRestore vid) -> vid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected SavedViewRestore, got: " <> show other

    it "parses saved_view_purge" $ do
      let args = object [ "view_id" .= testUUID ]
      case parseToolCall "saved_view_purge" args of
        Right (SavedViewPurge vid) -> vid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected SavedViewPurge, got: " <> show other

    it "parses saved_view_execute with optional params" $ do
      let args = object
            [ "view_id" .= testUUID
            , "limit"   .= (10 :: Int)
            , "detail"  .= True
            ]
      case parseToolCall "saved_view_execute" args of
        Right (SavedViewExecute vid ml _mo md) -> do
          vid `shouldBe` parsedUUID
          ml `shouldBe` Just 10
          md `shouldBe` Just True
        other -> expectationFailure $ "Expected SavedViewExecute, got: " <> show other

  describe "validateToolCall (saved views)" $ do

    it "clamps saved_view_list limit" $ do
      case validateToolCall (SavedViewList parsedUUID (Just 500) Nothing) of
        Right (SavedViewList _ ml _) -> ml `shouldBe` Just 200
        other -> expectationFailure $ "Expected SavedViewList, got: " <> show other

    it "clamps saved_view_execute limit and offset" $ do
      case validateToolCall (SavedViewExecute parsedUUID (Just 999) (Just 99999) Nothing) of
        Right (SavedViewExecute _ ml mo _) -> do
          ml `shouldBe` Just 200
          mo `shouldBe` Just 10000
        other -> expectationFailure $ "Expected SavedViewExecute, got: " <> show other

    it "passes through saved_view_get unchanged" $ do
      validateToolCall (SavedViewGet parsedUUID) `shouldBe` Right (SavedViewGet parsedUUID)

    it "passes through saved_view_delete unchanged" $ do
      validateToolCall (SavedViewDelete parsedUUID) `shouldBe` Right (SavedViewDelete parsedUUID)

  describe "toolDefinitions (saved views)" $ do

    it "defines saved_view_create with required fields" $ do
      let Just schema = inputSchemaFor "saved_view_create"
          Just properties = objectField "properties" schema
      objectField "workspace_id" properties `shouldSatisfy` (/= Nothing)
      objectField "name" properties `shouldSatisfy` (/= Nothing)
      objectField "entity_type" properties `shouldSatisfy` (/= Nothing)

    it "defines saved_view_execute" $ do
      inputSchemaFor "saved_view_execute" `shouldSatisfy` (/= Nothing)

inputSchemaFor :: Text -> Maybe Value
inputSchemaFor toolName = case filter ((== Just toolName) . textField "name") toolDefinitions of
  (Object obj:_) -> KM.lookup "inputSchema" obj
  _              -> Nothing

objectField :: Key -> Value -> Maybe Value
objectField key (Object obj) = KM.lookup key obj
objectField _ _ = Nothing

textField :: Key -> Value -> Maybe Text
textField key (Object obj) = case KM.lookup key obj of
  Just (String value) -> Just value
  _                   -> Nothing
textField _ _ = Nothing

numberField :: Key -> Value -> Maybe Scientific
numberField key (Object obj) = case KM.lookup key obj of
  Just (Number value) -> Just value
  _                   -> Nothing
numberField _ _ = Nothing
