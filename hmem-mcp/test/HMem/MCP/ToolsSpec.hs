{-# OPTIONS_GHC -Wno-x-partial #-}

module HMem.MCP.ToolsSpec (spec) where

import Data.Aeson
import Data.Aeson.Key qualified as Key
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

    it "parses entity_lifecycle for memory delete/restore/purge" $ do
      let args action = object [ "entity_type" .= ("memory" :: Text), "entity_id" .= testUUID, "action" .= (action :: Text) ]
      case parseToolCall "entity_lifecycle" (args "delete") of
        Right (MemoryDelete mid) -> mid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected MemoryDelete, got: " <> show other
      case parseToolCall "entity_lifecycle" (args "restore") of
        Right (MemoryRestore mid) -> mid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected MemoryRestore, got: " <> show other
      case parseToolCall "entity_lifecycle" (args "purge") of
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
            [ "action"        .= ("create" :: Text)
            , "source_id"     .= testUUID
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

    it "parses memory_create with items (batch)" $ do
      let item = object
            [ "workspace_id" .= testUUID
            , "content"      .= ("batch item" :: Text)
            , "memory_type"  .= ("short_term" :: Text)
            ]
          args = object [ "items" .= [item, item] ]
      case parseToolCall "memory_create" args of
        Right (MemoryCreateBatch cms) -> length cms `shouldBe` 2
        other -> expectationFailure $ "Expected MemoryCreateBatch, got: " <> show other

    it "parses memory_link with action=create" $ do
      let args = object [ "action" .= ("create" :: Text), "source_id" .= testUUID, "target_id" .= testUUID2, "relation_type" .= ("related" :: Text) ]
      case parseToolCall "memory_link" args of
        Right (LinkMemories sid _) -> sid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected LinkMemories, got: " <> show other

    it "parses memory_link with action=remove" $ do
      let args = object [ "action" .= ("remove" :: Text), "source_id" .= testUUID, "target_id" .= testUUID2, "relation_type" .= ("related" :: Text) ]
      case parseToolCall "memory_link" args of
        Right (MemoryUnlink sid tid _) -> do
          sid `shouldBe` parsedUUID
          tid `shouldBe` parsedUUID2
        other -> expectationFailure $ "Expected MemoryUnlink, got: " <> show other

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

    it "clamps search limit to 1-200" $ do
      let sq = SearchQuery
            { workspaceId = Nothing, query = Nothing
            , memoryType = Nothing, tags = Nothing
            , minImportance = Nothing, minAccessCount = Just (-5), sortBy = Just SortAccessCount, categoryId = Nothing
            , pinnedOnly = Nothing, searchLanguage = Nothing
            , limit = Just 999, offset = Just 0
            }
      case validateToolCall (MemorySearch sq False) of
        Right (MemorySearch sq' _) -> do
          sq'.limit `shouldBe` Just 200
          sq'.minAccessCount `shouldBe` Just 0
          sq'.sortBy `shouldBe` Just SortAccessCount
        other -> expectationFailure $ "Expected MemorySearch, got: " <> show other

    it "rejects task_list with neither workspace_id nor project_id" $ do
      validateToolCall (TaskList TaskListQuery
        { workspaceId = Nothing
        , projectId = Nothing
        , status = Nothing
        , priority = Nothing
        , query = Nothing
        , searchLanguage = Nothing
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
        , query = Nothing
        , searchLanguage = Nothing
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

    it "parses entity_lifecycle for project delete/restore/purge" $ do
      let args action = object [ "entity_type" .= ("project" :: Text), "entity_id" .= testUUID, "action" .= (action :: Text) ]
      case parseToolCall "entity_lifecycle" (args "delete") of
        Right (ProjectDelete pid) -> pid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected ProjectDelete, got: " <> show other
      case parseToolCall "entity_lifecycle" (args "restore") of
        Right (ProjectRestore pid) -> pid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected ProjectRestore, got: " <> show other
      case parseToolCall "entity_lifecycle" (args "purge") of
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

    it "parses link_memory for project link/unlink" $ do
      let linkArgs = object [ "entity_type" .= ("project" :: Text), "entity_id" .= testUUID, "action" .= ("link" :: Text), "memory_ids" .= [testUUID2 :: Text] ]
      case parseToolCall "link_memory" linkArgs of
        Right (ProjectLinkMem pid mid) -> do
          pid `shouldBe` parsedUUID
          mid `shouldBe` parsedUUID2
        other -> expectationFailure $ "Expected ProjectLinkMem, got: " <> show other
      let unlinkArgs = object [ "entity_type" .= ("project" :: Text), "entity_id" .= testUUID, "action" .= ("unlink" :: Text), "memory_ids" .= [testUUID2 :: Text] ]
      case parseToolCall "link_memory" unlinkArgs of
        Right (ProjectUnlinkMem pid mid) -> do
          pid `shouldBe` parsedUUID
          mid `shouldBe` parsedUUID2
        other -> expectationFailure $ "Expected ProjectUnlinkMem, got: " <> show other

    it "parses list_entity_memories for project" $ do
      let args = object
            [ "entity_type" .= ("project" :: Text)
            , "entity_id" .= testUUID
            , "query" .= ("alpha" :: Text)
            , "tags" .= (["tag1", "tag2"] :: [Text])
            , "min_importance" .= (6 :: Int)
            , "memory_type" .= ("long_term" :: Text)
            , "min_access_count" .= (3 :: Int)
            ]
      case parseToolCall "list_entity_memories" args of
        Right (ProjectListMem pid lq) -> do
          pid `shouldBe` parsedUUID
          lq.query `shouldBe` Just "alpha"
          lq.tags `shouldBe` Just ["tag1", "tag2"]
          lq.minImportance `shouldBe` Just 6
          lq.memoryType `shouldBe` Just LongTerm
          lq.minAccessCount `shouldBe` Just 3
        other -> expectationFailure $ "Expected ProjectListMem, got: " <> show other

    it "parses entity_lifecycle batch delete for project" $ do
      let deleteArgs = object [ "entity_type" .= ("project" :: Text), "ids" .= ([testUUID, testUUID2] :: [Text]), "action" .= ("delete" :: Text) ]
      case parseToolCall "entity_lifecycle" deleteArgs of
        Right (ProjectDeleteBatch ids) -> ids `shouldBe` [parsedUUID, parsedUUID2]
        other -> expectationFailure $ "Expected ProjectDeleteBatch, got: " <> show other

    it "parses project_update with items (batch)" $ do
      let updateArgs = object
            [ "items" .= [ object [ "id" .= testUUID, "name" .= ("Renamed" :: Text) ] ]
            ]
      case parseToolCall "project_update" updateArgs of
        Right (ProjectUpdateBatch items) -> length items `shouldBe` 1
        other -> expectationFailure $ "Expected ProjectUpdateBatch, got: " <> show other

    it "parses project_overview with extra_context" $ do
      let args = object [ "project_id" .= testUUID, "extra_context" .= True ]
      case parseToolCall "project_overview" args of
        Right (ProjectOverviewCall pid extraContext) -> do
          pid `shouldBe` parsedUUID
          extraContext `shouldBe` True
        other -> expectationFailure $ "Expected ProjectOverviewCall, got: " <> show other

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

    it "parses task_overview with extra_context" $ do
      let args = object [ "task_id" .= testUUID, "extra_context" .= True ]
      case parseToolCall "task_overview" args of
        Right (TaskOverviewCall tid extraContext) -> do
          tid `shouldBe` parsedUUID
          extraContext `shouldBe` True
        other -> expectationFailure $ "Expected TaskOverviewCall, got: " <> show other

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

    it "parses entity_lifecycle for task delete/restore/purge" $ do
      let args action = object [ "entity_type" .= ("task" :: Text), "entity_id" .= testUUID, "action" .= (action :: Text) ]
      case parseToolCall "entity_lifecycle" (args "delete") of
        Right (TaskDelete tid) -> tid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected TaskDelete, got: " <> show other
      case parseToolCall "entity_lifecycle" (args "restore") of
        Right (TaskRestore tid) -> tid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected TaskRestore, got: " <> show other
      case parseToolCall "entity_lifecycle" (args "purge") of
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

    it "parses task_dependency with action=add" $ do
      let args = object [ "action" .= ("add" :: Text), "task_id" .= testUUID, "depends_on_id" .= testUUID2 ]
      case parseToolCall "task_dependency" args of
        Right (TaskDepAdd tid did) -> do
          tid `shouldBe` parsedUUID
          did `shouldBe` parsedUUID2
        other -> expectationFailure $ "Expected TaskDepAdd, got: " <> show other

    it "parses task_dependency with action=remove" $ do
      let args = object [ "action" .= ("remove" :: Text), "task_id" .= testUUID, "depends_on_id" .= testUUID2 ]
      case parseToolCall "task_dependency" args of
        Right (TaskDepRemove tid did) -> do
          tid `shouldBe` parsedUUID
          did `shouldBe` parsedUUID2
        other -> expectationFailure $ "Expected TaskDepRemove, got: " <> show other

    it "parses link_memory for task link/unlink" $ do
      let linkArgs = object [ "entity_type" .= ("task" :: Text), "entity_id" .= testUUID, "action" .= ("link" :: Text), "memory_ids" .= [testUUID2 :: Text] ]
      case parseToolCall "link_memory" linkArgs of
        Right (TaskLinkMem tid mid) -> do
          tid `shouldBe` parsedUUID
          mid `shouldBe` parsedUUID2
        other -> expectationFailure $ "Expected TaskLinkMem, got: " <> show other
      let unlinkArgs = object [ "entity_type" .= ("task" :: Text), "entity_id" .= testUUID, "action" .= ("unlink" :: Text), "memory_ids" .= [testUUID2 :: Text] ]
      case parseToolCall "link_memory" unlinkArgs of
        Right (TaskUnlinkMem tid mid) -> do
          tid `shouldBe` parsedUUID
          mid `shouldBe` parsedUUID2
        other -> expectationFailure $ "Expected TaskUnlinkMem, got: " <> show other

    it "parses list_entity_memories for task" $ do
      let args = object
            [ "entity_type" .= ("task" :: Text)
            , "entity_id" .= testUUID
            , "query" .= ("beta" :: Text)
            , "min_importance" .= (4 :: Int)
            ]
      case parseToolCall "list_entity_memories" args of
        Right (TaskListMem tid lq) -> do
          tid `shouldBe` parsedUUID
          lq.query `shouldBe` Just "beta"
          lq.minImportance `shouldBe` Just 4
        other -> expectationFailure $ "Expected TaskListMem, got: " <> show other

  describe "parseToolCall (workspace and category restore/batch tools)" $ do

    it "parses entity_lifecycle for workspace restore" $ do
      let args = object [ "entity_type" .= ("workspace" :: Text), "entity_id" .= testUUID, "action" .= ("restore" :: Text) ]
      case parseToolCall "entity_lifecycle" args of
        Right (WsRestore wid) -> wid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected WsRestore, got: " <> show other

    it "parses entity_lifecycle for category restore and list_entity_memories for category" $ do
      let restoreArgs = object [ "entity_type" .= ("category" :: Text), "entity_id" .= testUUID, "action" .= ("restore" :: Text) ]
      case parseToolCall "entity_lifecycle" restoreArgs of
        Right (CategoryRestore cid) -> cid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected CategoryRestore, got: " <> show other
      let listArgs = object [ "entity_type" .= ("category" :: Text), "entity_id" .= testUUID ]
      case parseToolCall "list_entity_memories" listArgs of
        Right (CategoryListMem cid) -> cid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected CategoryListMem, got: " <> show other

    it "parses entity_lifecycle batch delete for category and link_memory batch for category" $ do
      let deleteArgs = object [ "entity_type" .= ("category" :: Text), "ids" .= ([testUUID, testUUID2] :: [Text]), "action" .= ("delete" :: Text) ]
          linkArgs = object
            [ "entity_type" .= ("category" :: Text)
            , "entity_id" .= testUUID
            , "action" .= ("link" :: Text)
            , "memory_ids" .= ([testUUID, testUUID2] :: [Text])
            ]
      case parseToolCall "entity_lifecycle" deleteArgs of
        Right (CategoryDeleteBatch ids) -> ids `shouldBe` [parsedUUID, parsedUUID2]
        other -> expectationFailure $ "Expected CategoryDeleteBatch, got: " <> show other
      case parseToolCall "link_memory" linkArgs of
        Right (CategoryLinkMemBatch cid mids) -> do
          cid `shouldBe` parsedUUID
          mids `shouldBe` [parsedUUID, parsedUUID2]
        other -> expectationFailure $ "Expected CategoryLinkMemBatch, got: " <> show other

  describe "validateToolCall (list clamping)" $ do

    it "clamps project_list limit to 1-200" $ do
      case validateToolCall (ProjectList ProjectListQuery
        { workspaceId = Just parsedUUID
        , status = Nothing
        , query = Nothing
        , searchLanguage = Nothing
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
        , query = Nothing
        , searchLanguage = Nothing
        , createdAfter = Nothing
        , createdBefore = Nothing
        , updatedAfter = Just (read "2026-03-30 11:00:00 UTC")
        , updatedBefore = Just (read "2026-03-30 10:00:00 UTC")
        , limit = Nothing
        , offset = Nothing
        }) `shouldSatisfy` isLeft

    it "rejects invalid search_language for project_list and task_list" $ do
      validateToolCall (ProjectList ProjectListQuery
        { workspaceId = Just parsedUUID
        , status = Nothing
        , query = Nothing
        , searchLanguage = Just "not-a-language"
        , createdAfter = Nothing
        , createdBefore = Nothing
        , updatedAfter = Nothing
        , updatedBefore = Nothing
        , limit = Nothing
        , offset = Nothing
        }) `shouldSatisfy` isLeft
      validateToolCall (TaskList TaskListQuery
        { workspaceId = Just parsedUUID
        , projectId = Nothing
        , status = Nothing
        , priority = Nothing
        , query = Nothing
        , searchLanguage = Just "not-a-language"
        , createdAfter = Nothing
        , createdBefore = Nothing
        , updatedAfter = Nothing
        , updatedBefore = Nothing
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

  describe "validateToolCall (query reconstruction)" $ do

    it "preserves non-clamped fields for memory_search" $ do
      case validateToolCall (MemorySearch SearchQuery
        { workspaceId = Just parsedUUID
        , query = Just "preserve-memory-search"
        , memoryType = Just LongTerm
        , tags = Just ["alpha", "beta"]
        , minImportance = Just 99
        , minAccessCount = Just (-1)
        , sortBy = Just SortImportance
        , categoryId = Just parsedUUID2
        , pinnedOnly = Just True
        , searchLanguage = Just "german"
        , limit = Just 500
        , offset = Just (-1)
        } True) of
        Right (MemorySearch sq detail) -> do
          sq.workspaceId `shouldBe` Just parsedUUID
          sq.query `shouldBe` Just "preserve-memory-search"
          sq.memoryType `shouldBe` Just LongTerm
          sq.tags `shouldBe` Just ["alpha", "beta"]
          sq.categoryId `shouldBe` Just parsedUUID2
          sq.pinnedOnly `shouldBe` Just True
          sq.searchLanguage `shouldBe` Just "german"
          sq.minImportance `shouldBe` Just 10
          sq.minAccessCount `shouldBe` Just 0
          sq.sortBy `shouldBe` Just SortImportance
          sq.limit `shouldBe` Just 200
          sq.offset `shouldBe` Just 0
          detail `shouldBe` True
        other -> expectationFailure $ "Expected MemorySearch, got: " <> show other

    it "preserves non-clamped fields for memory_list" $ do
      let createdAfterTs = read "2026-03-30 09:00:00 UTC"
          createdBeforeTs = read "2026-03-30 10:00:00 UTC"
          updatedAfterTs = read "2026-03-30 11:00:00 UTC"
          updatedBeforeTs = read "2026-03-30 12:00:00 UTC"
      case validateToolCall (MemoryList MemoryListQuery
        { workspaceId = Just parsedUUID
        , memoryType = Just ShortTerm
        , minAccessCount = Just (-2)
        , sortBy = Just SortAccessCount
        , createdAfter = Just createdAfterTs
        , createdBefore = Just createdBeforeTs
        , updatedAfter = Just updatedAfterTs
        , updatedBefore = Just updatedBeforeTs
        , limit = Just 0
        , offset = Just 10001
        } False) of
        Right (MemoryList mq detail) -> do
          mq.workspaceId `shouldBe` Just parsedUUID
          mq.memoryType `shouldBe` Just ShortTerm
          mq.minAccessCount `shouldBe` Just 0
          mq.sortBy `shouldBe` Just SortAccessCount
          mq.createdAfter `shouldBe` Just createdAfterTs
          mq.createdBefore `shouldBe` Just createdBeforeTs
          mq.updatedAfter `shouldBe` Just updatedAfterTs
          mq.updatedBefore `shouldBe` Just updatedBeforeTs
          mq.limit `shouldBe` Just 1
          mq.offset `shouldBe` Just 10000
          detail `shouldBe` False
        other -> expectationFailure $ "Expected MemoryList, got: " <> show other

    it "preserves non-clamped fields for project_list" $ do
      let createdAfterTs = read "2026-03-30 09:00:00 UTC"
          createdBeforeTs = read "2026-03-30 10:00:00 UTC"
          updatedAfterTs = read "2026-03-30 11:00:00 UTC"
          updatedBeforeTs = read "2026-03-30 12:00:00 UTC"
      case validateToolCall (ProjectList ProjectListQuery
        { workspaceId = Just parsedUUID
        , status = Just ProjPaused
        , query = Just "preserve-project-list"
        , searchLanguage = Just "english"
        , createdAfter = Just createdAfterTs
        , createdBefore = Just createdBeforeTs
        , updatedAfter = Just updatedAfterTs
        , updatedBefore = Just updatedBeforeTs
        , limit = Just 250
        , offset = Just (-5)
        }) of
        Right (ProjectList pq) -> do
          pq.workspaceId `shouldBe` Just parsedUUID
          pq.status `shouldBe` Just ProjPaused
          pq.query `shouldBe` Just "preserve-project-list"
          pq.searchLanguage `shouldBe` Just "english"
          pq.createdAfter `shouldBe` Just createdAfterTs
          pq.createdBefore `shouldBe` Just createdBeforeTs
          pq.updatedAfter `shouldBe` Just updatedAfterTs
          pq.updatedBefore `shouldBe` Just updatedBeforeTs
          pq.limit `shouldBe` Just 200
          pq.offset `shouldBe` Just 0
        other -> expectationFailure $ "Expected ProjectList, got: " <> show other

    it "preserves non-clamped fields for task_list" $ do
      let createdAfterTs = read "2026-03-30 09:00:00 UTC"
          createdBeforeTs = read "2026-03-30 10:00:00 UTC"
          updatedAfterTs = read "2026-03-30 11:00:00 UTC"
          updatedBeforeTs = read "2026-03-30 12:00:00 UTC"
      case validateToolCall (TaskList TaskListQuery
        { workspaceId = Just parsedUUID
        , projectId = Just parsedUUID2
        , status = Just Blocked
        , priority = Just 7
        , query = Just "preserve-task-list"
        , searchLanguage = Just "french"
        , createdAfter = Just createdAfterTs
        , createdBefore = Just createdBeforeTs
        , updatedAfter = Just updatedAfterTs
        , updatedBefore = Just updatedBeforeTs
        , limit = Just 0
        , offset = Just 10001
        }) of
        Right (TaskList tq) -> do
          tq.workspaceId `shouldBe` Just parsedUUID
          tq.projectId `shouldBe` Just parsedUUID2
          tq.status `shouldBe` Just Blocked
          tq.priority `shouldBe` Just 7
          tq.query `shouldBe` Just "preserve-task-list"
          tq.searchLanguage `shouldBe` Just "french"
          tq.createdAfter `shouldBe` Just createdAfterTs
          tq.createdBefore `shouldBe` Just createdBeforeTs
          tq.updatedAfter `shouldBe` Just updatedAfterTs
          tq.updatedBefore `shouldBe` Just updatedBeforeTs
          tq.limit `shouldBe` Just 1
          tq.offset `shouldBe` Just 10000
        other -> expectationFailure $ "Expected TaskList, got: " <> show other

  describe "request path construction" $ do

    it "forwards project_list query and search_language" $ do
      let path = buildProjectListPath ProjectListQuery
            { workspaceId = Just parsedUUID
            , status = Just ProjPaused
            , query = Just "project-query"
            , searchLanguage = Just "english"
            , createdAfter = Nothing
            , createdBefore = Nothing
            , updatedAfter = Nothing
            , updatedBefore = Nothing
            , limit = Just 25
            , offset = Just 5
            }
      path `shouldContain` "/api/v1/projects?"
      path `shouldContain` "workspace_id=aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
      path `shouldContain` "status=paused"
      path `shouldContain` "query=project-query"
      path `shouldContain` "search_language=english"
      path `shouldContain` "limit=25"
      path `shouldContain` "offset=5"

    it "forwards task_list query and search_language" $ do
      let path = buildTaskListPath TaskListQuery
            { workspaceId = Just parsedUUID
            , projectId = Just parsedUUID2
            , status = Just Blocked
            , priority = Just 7
            , query = Just "task-query"
            , searchLanguage = Just "french"
            , createdAfter = Nothing
            , createdBefore = Nothing
            , updatedAfter = Nothing
            , updatedBefore = Nothing
            , limit = Just 50
            , offset = Just 10
            }
      path `shouldContain` "/api/v1/tasks?"
      path `shouldContain` "workspace_id=aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
      path `shouldContain` "project_id=11111111-2222-3333-4444-555555555555"
      path `shouldContain` "status=blocked"
      path `shouldContain` "priority=7"
      path `shouldContain` "query=task-query"
      path `shouldContain` "search_language=french"
      path `shouldContain` "limit=50"
      path `shouldContain` "offset=10"

    it "UTF-8 encodes non-ASCII project_list query text" $ do
      let path = buildProjectListPath ProjectListQuery
            { workspaceId = Just parsedUUID
            , status = Nothing
            , query = Just "café"
            , searchLanguage = Just "english"
            , createdAfter = Nothing
            , createdBefore = Nothing
            , updatedAfter = Nothing
            , updatedBefore = Nothing
            , limit = Nothing
            , offset = Nothing
            }
      path `shouldContain` "query=caf%C3%A9"

    it "preserves access_count and sort_by for memory_list" $ do
      case validateToolCall (MemoryList MemoryListQuery
        { workspaceId = Just parsedUUID
        , memoryType = Just ShortTerm
        , minAccessCount = Just 3
        , sortBy = Just SortAccessCount
        , createdAfter = Nothing
        , createdBefore = Nothing
        , updatedAfter = Nothing
        , updatedBefore = Nothing
        , limit = Just 50
        , offset = Just 0
        } False) of
        Right (MemoryList mq _) -> do
          mq.minAccessCount `shouldBe` Just 3
          mq.sortBy `shouldBe` Just SortAccessCount
        other -> expectationFailure $ "Expected MemoryList, got: " <> show other

  describe "toolDefinitions" $ do

    it "advertises maxLength for memory content" $ do
      schema <- requireJust "memory_create schema" (inputSchemaFor "memory_create")
      properties <- requireJust "memory_create properties" (objectField "properties" schema)
      contentSchema <- requireJust "memory_create content schema" (objectField "content" properties)
      numberField "maxLength" contentSchema `shouldBe` Just (fromIntegral maxMemoryContentBytes)

    it "advertises batch items support on memory_create" $ do
      schema <- requireJust "memory_create schema" (inputSchemaFor "memory_create")
      properties <- requireJust "memory_create properties" (objectField "properties" schema)
      itemsSchema <- requireJust "memory_create items schema" (objectField "items" properties)
      objectField "items" itemsSchema `shouldSatisfy` (/= Nothing)

    it "advertises list filter timestamps and task priority" $ do
      memorySchema <- requireJust "memory_list schema" (inputSchemaFor "memory_list")
      memoryProps <- requireJust "memory_list properties" (objectField "properties" memorySchema)
      projectSchema <- requireJust "project_list schema" (inputSchemaFor "project_list")
      projectProps <- requireJust "project_list properties" (objectField "properties" projectSchema)
      taskSchema <- requireJust "task_list schema" (inputSchemaFor "task_list")
      taskProps <- requireJust "task_list properties" (objectField "properties" taskSchema)
      let createdAfterType = objectField "created_after" memoryProps >>= textField "type"
          updatedBeforeType = objectField "updated_before" projectProps >>= textField "type"
          priorityType = objectField "priority" taskProps >>= textField "type"
      createdAfterType `shouldBe` Just "string"
      updatedBeforeType `shouldBe` Just "string"
      priorityType `shouldBe` Just "integer"

    it "defines entity_lifecycle and new unified tools" $ do
      inputSchemaFor "entity_lifecycle" `shouldSatisfy` (/= Nothing)
      inputSchemaFor "link_memory" `shouldSatisfy` (/= Nothing)
      inputSchemaFor "list_entity_memories" `shouldSatisfy` (/= Nothing)
      inputSchemaFor "category" `shouldSatisfy` (/= Nothing)
      inputSchemaFor "saved_view" `shouldSatisfy` (/= Nothing)

    it "defines task_overview tool" $ do
      taskOverviewSchema <- requireJust "task_overview schema" (inputSchemaFor "task_overview")
      taskOverviewProps <- requireJust "task_overview properties" (objectField "properties" taskOverviewSchema)
      objectField "extra_context" taskOverviewProps `shouldSatisfy` (/= Nothing)

    it "defines project_overview extra_context and memory search/list access_count fields" $ do
      projectOverviewSchema <- requireJust "project_overview schema" (inputSchemaFor "project_overview")
      projectOverviewProps <- requireJust "project_overview properties" (objectField "properties" projectOverviewSchema)
      memorySearchSchema <- requireJust "memory_search schema" (inputSchemaFor "memory_search")
      memorySearchProps <- requireJust "memory_search properties" (objectField "properties" memorySearchSchema)
      memoryListSchema <- requireJust "memory_list schema" (inputSchemaFor "memory_list")
      memoryListProps <- requireJust "memory_list properties" (objectField "properties" memoryListSchema)
      entityMemsSchema <- requireJust "list_entity_memories schema" (inputSchemaFor "list_entity_memories")
      entityMemsProps <- requireJust "list_entity_memories properties" (objectField "properties" entityMemsSchema)
      objectField "extra_context" projectOverviewProps `shouldSatisfy` (/= Nothing)
      (objectField "extra_context" projectOverviewProps >>= textField "type") `shouldBe` Just "boolean"
      (objectField "min_access_count" memorySearchProps >>= textField "type") `shouldBe` Just "integer"
      (objectField "sort_by" memorySearchProps >>= objectField "enum") `shouldBe`
        Just (toJSON (["recent", "importance", "access_count"] :: [Text]))
      (objectField "min_access_count" memoryListProps >>= textField "type") `shouldBe` Just "integer"
      (objectField "sort_by" memoryListProps >>= objectField "enum") `shouldBe`
        Just (toJSON (["recent", "importance", "access_count"] :: [Text]))
      (objectField "min_access_count" entityMemsProps >>= textField "type") `shouldBe` Just "integer"

  describe "parseToolCall (saved view tools)" $ do

    it "parses saved_view with action=create" $ do
      let args = object
            [ "action"       .= ("create" :: Text)
            , "workspace_id" .= testUUID
            , "name"         .= ("My View" :: Text)
            , "entity_type"  .= ("memory_search" :: Text)
            , "query_params" .= object ["tags" .= (["haskell"] :: [Text])]
            ]
      case parseToolCall "saved_view" args of
        Right (SavedViewCreate csv) -> do
          csv.workspaceId `shouldBe` parsedUUID
          csv.name `shouldBe` "My View"
          csv.entityType `shouldBe` "memory_search"
        other -> expectationFailure $ "Expected SavedViewCreate, got: " <> show other

    it "parses saved_view with action=list" $ do
      let args = object
            [ "action"       .= ("list" :: Text)
            , "workspace_id" .= testUUID
            , "limit"        .= (20 :: Int)
            ]
      case parseToolCall "saved_view" args of
        Right (SavedViewList wid ml _mo) -> do
          wid `shouldBe` parsedUUID
          ml `shouldBe` Just 20
        other -> expectationFailure $ "Expected SavedViewList, got: " <> show other

    it "parses saved_view with action=get" $ do
      let args = object [ "action" .= ("get" :: Text), "view_id" .= testUUID ]
      case parseToolCall "saved_view" args of
        Right (SavedViewGet vid) -> vid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected SavedViewGet, got: " <> show other

    it "parses saved_view with action=update" $ do
      let args = object
            [ "action"  .= ("update" :: Text)
            , "view_id" .= testUUID
            , "name"    .= ("Renamed" :: Text)
            ]
      case parseToolCall "saved_view" args of
        Right (SavedViewUpdate vid usv) -> do
          vid `shouldBe` parsedUUID
          usv.name `shouldBe` Just "Renamed"
        other -> expectationFailure $ "Expected SavedViewUpdate, got: " <> show other

    it "parses entity_lifecycle for saved_view delete/restore/purge" $ do
      let args action = object [ "entity_type" .= ("saved_view" :: Text), "entity_id" .= testUUID, "action" .= (action :: Text) ]
      case parseToolCall "entity_lifecycle" (args "delete") of
        Right (SavedViewDelete vid) -> vid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected SavedViewDelete, got: " <> show other
      case parseToolCall "entity_lifecycle" (args "restore") of
        Right (SavedViewRestore vid) -> vid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected SavedViewRestore, got: " <> show other
      case parseToolCall "entity_lifecycle" (args "purge") of
        Right (SavedViewPurge vid) -> vid `shouldBe` parsedUUID
        other -> expectationFailure $ "Expected SavedViewPurge, got: " <> show other

    it "parses saved_view with action=execute and optional params" $ do
      let args = object
            [ "action"  .= ("execute" :: Text)
            , "view_id" .= testUUID
            , "limit"   .= (10 :: Int)
            , "detail"  .= True
            ]
      case parseToolCall "saved_view" args of
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

    it "defines saved_view with action-based schema" $ do
      schema <- requireJust "saved_view schema" (inputSchemaFor "saved_view")
      properties <- requireJust "saved_view properties" (objectField "properties" schema)
      objectField "action" properties `shouldSatisfy` (/= Nothing)
      objectField "workspace_id" properties `shouldSatisfy` (/= Nothing)

inputSchemaFor :: Text -> Maybe Value
inputSchemaFor toolName = case filter ((== Just toolName) . textField "name") toolDefinitions of
  (Object obj:_) -> KM.lookup "inputSchema" obj
  _              -> Nothing

requireJust :: String -> Maybe a -> IO a
requireJust label = 
  maybe
    (expectationFailure ("Missing expected value: " <> label) >> fail ("Missing expected value: " <> label))
    pure

objectField :: Text -> Value -> Maybe Value
objectField key (Object obj) = KM.lookup (Key.fromText key) obj
objectField _ _ = Nothing

textField :: Text -> Value -> Maybe Text
textField key (Object obj) = case KM.lookup (Key.fromText key) obj of
  Just (String value) -> Just value
  _                   -> Nothing
textField _ _ = Nothing

numberField :: Text -> Value -> Maybe Scientific
numberField key (Object obj) = case KM.lookup (Key.fromText key) obj of
  Just (Number value) -> Just value
  _                   -> Nothing
numberField _ _ = Nothing
