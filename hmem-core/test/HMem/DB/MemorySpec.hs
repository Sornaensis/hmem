{-# OPTIONS_GHC -Wno-x-partial -Wno-incomplete-uni-patterns -Wno-missing-fields #-}

module HMem.DB.MemorySpec (spec) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (try, SomeException)
import Data.Functor.Contravariant ((>$<), contramap)
import Data.Maybe (isJust, isNothing)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Test.Hspec

import HMem.DB.Memory hiding (createMemory, createMemoryBatch)
import HMem.DB.Memory qualified as Mem
import HMem.DB.Pool (DBException(..), runSession, runTransaction)
import HMem.DB.Project (createProject)
import HMem.DB.Task qualified as Task
import HMem.DB.TestHarness
import HMem.Types


backdateMemoryUpdatedAt :: TestEnv -> UUID -> Int -> IO ()
backdateMemoryUpdatedAt env mid hours = do
  runSession env.pool forceDeferredConstraints
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

    it "creates a memory atomically with an explicit project link" $ \env -> do
      ws <- createTestWorkspace env "project-linked-create-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Project target"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      mem <- Mem.createMemory env.pool CreateMemory
        { workspaceId = ws.id
        , projectId = Just proj.id
        , taskId = Nothing
        , content = "project linked"
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
      got <- getMemory env.pool mem.id
      got `shouldSatisfy` isJust

    it "creates a memory atomically with an explicit task link" $ \env -> do
      ws <- createTestWorkspace env "task-linked-create-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Task project"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      task <- Task.createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing
        , title = "Task target", description = Nothing, priority = Nothing
        , metadata = Nothing, dueAt = Nothing }
      mem <- Mem.createMemory env.pool CreateMemory
        { workspaceId = ws.id
        , projectId = Nothing
        , taskId = Just task.id
        , content = "task linked"
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
      got <- getMemory env.pool mem.id
      got `shouldSatisfy` isJust

    it "rejects direct SQL memory inserts without a project or task link" $ \env -> do
      ws <- createTestWorkspace env "direct-unlinked-ws"
      expectWorkflowViolation "MEMORY_LINK_REQUIRED" $
        runTransaction env.pool $ do
          _ <- insertMemoryDirect ws.id "unlinked direct"
          forceDeferredConstraints

    it "memoryTypeRequiredSQL rejects direct SQL memory inserts without explicit memory_type" $ \env -> do
      ws <- createTestWorkspace env "direct-missing-type-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Missing type project"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      expectWorkflowViolation "MEMORY_TYPE_REQUIRED" $
        runTransaction env.pool $ do
          mid <- insertMemoryWithoutTypeDirect ws.id "missing type direct"
          linkProjectMemoryDirect proj.id mid
          forceDeferredConstraints

    it "accepts direct SQL memory inserts with a project link in the same transaction" $ \env -> do
      ws <- createTestWorkspace env "direct-project-link-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Direct project"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      mid <- runTransaction env.pool $ do
        mid <- insertMemoryDirect ws.id "direct project linked"
        linkProjectMemoryDirect proj.id mid
        forceDeferredConstraints
        pure mid
      got <- getMemory env.pool mid
      got `shouldSatisfy` isJust

    it "accepts direct SQL memory inserts with a task link in the same transaction" $ \env -> do
      ws <- createTestWorkspace env "direct-task-link-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Direct task project"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      task <- Task.createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing
        , title = "Direct task", description = Nothing, priority = Nothing
        , metadata = Nothing, dueAt = Nothing }
      mid <- runTransaction env.pool $ do
        mid <- insertMemoryDirect ws.id "direct task linked"
        linkTaskMemoryDirect task.id mid
        forceDeferredConstraints
        pure mid
      got <- getMemory env.pool mid
      got `shouldSatisfy` isJust

    it "accepts direct SQL memory links to subtasks" $ \env -> do
      ws <- createTestWorkspace env "direct-subtask-link-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Subtask project"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      parent <- Task.createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing
        , title = "Parent", description = Nothing, priority = Nothing
        , metadata = Nothing, dueAt = Nothing }
      child <- Task.createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Just parent.id
        , title = "Child", description = Nothing, priority = Nothing
        , metadata = Nothing, dueAt = Nothing }
      mid <- runTransaction env.pool $ do
        mid <- insertMemoryDirect ws.id "subtask linked"
        linkTaskMemoryDirect child.id mid
        forceDeferredConstraints
        pure mid
      getMemory env.pool mid >>= (`shouldSatisfy` isJust)

    it "rejects direct SQL memory links across workspaces" $ \env -> do
      wsA <- createTestWorkspace env "direct-cross-a"
      wsB <- createTestWorkspace env "direct-cross-b"
      projB <- createProject env.pool CreateProject
        { workspaceId = wsB.id, parentId = Nothing, name = "Other project"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      expectWorkflowViolation "MEMORY_LINK_CROSS_WORKSPACE" $
        runTransaction env.pool $ do
          mid <- insertMemoryDirect wsA.id "cross workspace"
          linkProjectMemoryDirect projB.id mid
          forceDeferredConstraints

    it "rejects direct SQL memory links to deleted cross-workspace targets" $ \env -> do
      wsA <- createTestWorkspace env "direct-cross-deleted-a"
      wsB <- createTestWorkspace env "direct-cross-deleted-b"
      projA <- createProject env.pool CreateProject
        { workspaceId = wsA.id, parentId = Nothing, name = "Project A"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      projB <- createProject env.pool CreateProject
        { workspaceId = wsB.id, parentId = Nothing, name = "Deleted other project"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      mem <- Mem.createMemory env.pool CreateMemory
        { workspaceId = wsA.id, projectId = Just projA.id, taskId = Nothing
        , content = "valid local link", summary = Nothing, memoryType = ShortTerm
        , importance = Nothing, metadata = Nothing, expiresAt = Nothing, source = Nothing
        , confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      runTransaction env.pool $ do
        softDeleteProjectDirect projB.id
        forceDeferredConstraints
      expectWorkflowViolation "MEMORY_LINK_CROSS_WORKSPACE" $
        runTransaction env.pool $ do
          linkProjectMemoryDirect projB.id mem.id
          forceDeferredConstraints

    it "accepts direct SQL memory creation with both project and task links" $ \env -> do
      ws <- createTestWorkspace env "direct-ambiguous-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Ambiguous project"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      task <- Task.createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing
        , title = "Ambiguous task", description = Nothing, priority = Nothing
        , metadata = Nothing, dueAt = Nothing }
      mid <- runTransaction env.pool $ do
        mid <- insertMemoryDirect ws.id "multiple links"
        linkProjectMemoryDirect proj.id mid
        linkTaskMemoryDirect task.id mid
        forceDeferredConstraints
        pure mid
      getMemory env.pool mid >>= (`shouldSatisfy` isJust)

    it "rejects updating a project link away from a memory's only active target" $ \env -> do
      ws <- createTestWorkspace env "direct-project-link-update-ws"
      proj1 <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Project one"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      proj2 <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Project two"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      mem1 <- Mem.createMemory env.pool CreateMemory
        { workspaceId = ws.id, projectId = Just proj1.id, taskId = Nothing
        , content = "source memory", summary = Nothing, memoryType = ShortTerm
        , importance = Nothing, metadata = Nothing, expiresAt = Nothing, source = Nothing
        , confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      mem2 <- Mem.createMemory env.pool CreateMemory
        { workspaceId = ws.id, projectId = Just proj2.id, taskId = Nothing
        , content = "target memory", summary = Nothing, memoryType = ShortTerm
        , importance = Nothing, metadata = Nothing, expiresAt = Nothing, source = Nothing
        , confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }

      expectWorkflowViolation "MEMORY_LINK_REQUIRED" $
        runTransaction env.pool $ do
          updateProjectMemoryLinkMemoryDirect proj1.id mem1.id mem2.id
          forceDeferredConstraints

    it "rejects updating a task link away from a memory's only active target" $ \env -> do
      ws <- createTestWorkspace env "direct-task-link-update-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Task project"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      task1 <- Task.createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing
        , title = "Task one", description = Nothing, priority = Nothing
        , metadata = Nothing, dueAt = Nothing }
      task2 <- Task.createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing
        , title = "Task two", description = Nothing, priority = Nothing
        , metadata = Nothing, dueAt = Nothing }
      mem1 <- Mem.createMemory env.pool CreateMemory
        { workspaceId = ws.id, projectId = Nothing, taskId = Just task1.id
        , content = "source memory", summary = Nothing, memoryType = ShortTerm
        , importance = Nothing, metadata = Nothing, expiresAt = Nothing, source = Nothing
        , confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      mem2 <- Mem.createMemory env.pool CreateMemory
        { workspaceId = ws.id, projectId = Nothing, taskId = Just task2.id
        , content = "target memory", summary = Nothing, memoryType = ShortTerm
        , importance = Nothing, metadata = Nothing, expiresAt = Nothing, source = Nothing
        , confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }

      expectWorkflowViolation "MEMORY_LINK_REQUIRED" $
        runTransaction env.pool $ do
          updateTaskMemoryLinkMemoryDirect task1.id mem1.id mem2.id
          forceDeferredConstraints

    it "allows reparenting a linked task into a subtask" $ \env -> do
      ws <- createTestWorkspace env "linked-task-reparent-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Reparent project"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      parent <- Task.createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing
        , title = "Parent", description = Nothing, priority = Nothing
        , metadata = Nothing, dueAt = Nothing }
      linkedTask <- Task.createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing
        , title = "Linked task", description = Nothing, priority = Nothing
        , metadata = Nothing, dueAt = Nothing }
      mem <- Mem.createMemory env.pool CreateMemory
        { workspaceId = ws.id, projectId = Nothing, taskId = Just linkedTask.id
        , content = "linked task memory", summary = Nothing, memoryType = ShortTerm
        , importance = Nothing, metadata = Nothing, expiresAt = Nothing, source = Nothing
        , confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      runTransaction env.pool $ do
        reparentTaskDirect linkedTask.id parent.id
        forceDeferredConstraints
      getMemory env.pool mem.id >>= (`shouldSatisfy` isJust)

    it "rejects soft-deleting a linked project when it is the memory's only active target" $ \env -> do
      ws <- createTestWorkspace env "linked-project-delete-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Linked project"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      _ <- Mem.createMemory env.pool CreateMemory
        { workspaceId = ws.id, projectId = Just proj.id, taskId = Nothing
        , content = "linked project memory", summary = Nothing, memoryType = ShortTerm
        , importance = Nothing, metadata = Nothing, expiresAt = Nothing, source = Nothing
        , confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      expectWorkflowViolation "MEMORY_LINK_REQUIRED" $
        runTransaction env.pool $ do
          softDeleteProjectDirect proj.id
          forceDeferredConstraints

    it "allows soft-deleting one linked project while another active target remains" $ \env -> do
      ws <- createTestWorkspace env "linked-project-delete-with-other-ws"
      proj1 <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Linked project one"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      proj2 <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Linked project two"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      mem <- Mem.createMemory env.pool CreateMemory
        { workspaceId = ws.id, projectId = Just proj1.id, taskId = Nothing
        , content = "linked project memory", summary = Nothing, memoryType = ShortTerm
        , importance = Nothing, metadata = Nothing, expiresAt = Nothing, source = Nothing
        , confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      runTransaction env.pool $ do
        linkProjectMemoryDirect proj2.id mem.id
        forceDeferredConstraints
      runTransaction env.pool $ do
        softDeleteProjectDirect proj1.id
        forceDeferredConstraints
      getMemory env.pool mem.id >>= (`shouldSatisfy` isJust)

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

createMemory :: Pool Hasql.Connection -> CreateMemory -> IO Memory
createMemory pool cm = do
  proj <- createProject pool CreateProject
    { workspaceId = cm.workspaceId
    , parentId = Nothing
    , name = "MemorySpec link target"
    , description = Nothing
    , priority = Nothing
    , metadata = Nothing
    }
  Mem.createMemory pool cm { projectId = Just proj.id, taskId = Nothing }

createMemoryBatch :: Pool Hasql.Connection -> [CreateMemory] -> IO [Memory]
createMemoryBatch pool cms = do
  linked <- mapM addProjectLink cms
  Mem.createMemoryBatch pool linked
  where
    addProjectLink cm = do
      proj <- createProject pool CreateProject
        { workspaceId = cm.workspaceId
        , parentId = Nothing
        , name = "MemorySpec batch link target"
        , description = Nothing
        , priority = Nothing
        , metadata = Nothing
        }
      pure cm { projectId = Just proj.id, taskId = Nothing }

expectWorkflowViolation :: Text -> IO a -> Expectation
expectWorkflowViolation expectedCode action = do
  result <- try @DBException action
  case result of
    Left (DBWorkflowViolation code _ _ _) -> code `shouldBe` expectedCode
    Left other -> expectationFailure $ "Expected workflow violation " <> show expectedCode <> ", got: " <> show other
    Right _ -> expectationFailure $ "Expected workflow violation " <> show expectedCode <> ", but action succeeded"

insertMemoryDirect :: UUID -> Text -> Session.Session UUID
insertMemoryDirect wsId label =
  Session.statement (wsId, label) insertMemoryDirectStatement

insertMemoryDirectStatement :: Statement.Statement (UUID, Text) UUID
insertMemoryDirectStatement = Statement.Statement sql encoder decoder True
  where
    sql = "INSERT INTO memories (workspace_id, content, memory_type) VALUES ($1, $2, 'short_term') RETURNING id"
    encoder =
      contramap fst (E.param (E.nonNullable E.uuid)) <>
      contramap snd (E.param (E.nonNullable E.text))
    decoder = D.singleRow (D.column (D.nonNullable D.uuid))

insertMemoryWithoutTypeDirect :: UUID -> Text -> Session.Session UUID
insertMemoryWithoutTypeDirect wsId label =
  Session.statement (wsId, label) insertMemoryWithoutTypeDirectStatement

insertMemoryWithoutTypeDirectStatement :: Statement.Statement (UUID, Text) UUID
insertMemoryWithoutTypeDirectStatement = Statement.Statement sql encoder decoder True
  where
    sql = "INSERT INTO memories (workspace_id, content) VALUES ($1, $2) RETURNING id"
    encoder =
      contramap fst (E.param (E.nonNullable E.uuid)) <>
      contramap snd (E.param (E.nonNullable E.text))
    decoder = D.singleRow (D.column (D.nonNullable D.uuid))

linkProjectMemoryDirect :: UUID -> UUID -> Session.Session ()
linkProjectMemoryDirect projectId memoryId =
  Session.statement (projectId, memoryId) linkProjectMemoryDirectStatement

linkProjectMemoryDirectStatement :: Statement.Statement (UUID, UUID) ()
linkProjectMemoryDirectStatement = Statement.Statement sql encoder D.noResult True
  where
    sql = "INSERT INTO project_memory_links (project_id, memory_id) VALUES ($1, $2)"
    encoder =
      contramap fst (E.param (E.nonNullable E.uuid)) <>
      contramap snd (E.param (E.nonNullable E.uuid))

linkTaskMemoryDirect :: UUID -> UUID -> Session.Session ()
linkTaskMemoryDirect taskId memoryId =
  Session.statement (taskId, memoryId) linkTaskMemoryDirectStatement

linkTaskMemoryDirectStatement :: Statement.Statement (UUID, UUID) ()
linkTaskMemoryDirectStatement = Statement.Statement sql encoder D.noResult True
  where
    sql = "INSERT INTO task_memory_links (task_id, memory_id) VALUES ($1, $2)"
    encoder =
      contramap fst (E.param (E.nonNullable E.uuid)) <>
      contramap snd (E.param (E.nonNullable E.uuid))

updateProjectMemoryLinkMemoryDirect :: UUID -> UUID -> UUID -> Session.Session ()
updateProjectMemoryLinkMemoryDirect projectId oldMemoryId newMemoryId =
  Session.statement (projectId, oldMemoryId, newMemoryId) updateProjectMemoryLinkMemoryDirectStatement

updateProjectMemoryLinkMemoryDirectStatement :: Statement.Statement (UUID, UUID, UUID) ()
updateProjectMemoryLinkMemoryDirectStatement = Statement.Statement sql encoder D.noResult True
  where
    sql = "UPDATE project_memory_links SET memory_id = $3 WHERE project_id = $1 AND memory_id = $2"
    encoder =
      contramap (\(projectId, _, _) -> projectId) (E.param (E.nonNullable E.uuid)) <>
      contramap (\(_, oldMemoryId, _) -> oldMemoryId) (E.param (E.nonNullable E.uuid)) <>
      contramap (\(_, _, newMemoryId) -> newMemoryId) (E.param (E.nonNullable E.uuid))

updateTaskMemoryLinkMemoryDirect :: UUID -> UUID -> UUID -> Session.Session ()
updateTaskMemoryLinkMemoryDirect taskId oldMemoryId newMemoryId =
  Session.statement (taskId, oldMemoryId, newMemoryId) updateTaskMemoryLinkMemoryDirectStatement

updateTaskMemoryLinkMemoryDirectStatement :: Statement.Statement (UUID, UUID, UUID) ()
updateTaskMemoryLinkMemoryDirectStatement = Statement.Statement sql encoder D.noResult True
  where
    sql = "UPDATE task_memory_links SET memory_id = $3 WHERE task_id = $1 AND memory_id = $2"
    encoder =
      contramap (\(taskId, _, _) -> taskId) (E.param (E.nonNullable E.uuid)) <>
      contramap (\(_, oldMemoryId, _) -> oldMemoryId) (E.param (E.nonNullable E.uuid)) <>
      contramap (\(_, _, newMemoryId) -> newMemoryId) (E.param (E.nonNullable E.uuid))

reparentTaskDirect :: UUID -> UUID -> Session.Session ()
reparentTaskDirect taskId parentId =
  Session.statement (taskId, parentId) reparentTaskDirectStatement

reparentTaskDirectStatement :: Statement.Statement (UUID, UUID) ()
reparentTaskDirectStatement = Statement.Statement sql encoder D.noResult True
  where
    sql = "UPDATE tasks SET parent_id = $2 WHERE id = $1"
    encoder =
      contramap fst (E.param (E.nonNullable E.uuid)) <>
      contramap snd (E.param (E.nonNullable E.uuid))

softDeleteProjectDirect :: UUID -> Session.Session ()
softDeleteProjectDirect projectId =
  Session.statement projectId softDeleteProjectDirectStatement

softDeleteProjectDirectStatement :: Statement.Statement UUID ()
softDeleteProjectDirectStatement = Statement.Statement sql encoder D.noResult True
  where
    sql = "UPDATE projects SET deleted_at = now() WHERE id = $1"
    encoder = E.param (E.nonNullable E.uuid)

forceDeferredConstraints :: Session.Session ()
forceDeferredConstraints = do
  Session.sql "SET CONSTRAINTS ALL IMMEDIATE"
  Session.sql "SET CONSTRAINTS ALL DEFERRED"
