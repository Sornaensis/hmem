{-# OPTIONS_GHC -Wno-x-partial -Wno-incomplete-uni-patterns #-}

module HMem.DB.ProjectSpec (spec) where

import Control.Exception (try)
import Control.Monad (forM_, void)
import Data.ByteString.Char8 qualified as BS8
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Hasql.Session qualified as Session

import HMem.DB.Pool (DBException(..), runSession)
import HMem.DB.Memory (createMemory, getProjectMemories, touchMemory, updateMemory)
import HMem.DB.Project
import HMem.DB.Task (createTask, deleteTask, getTask, listTasksByWorkspace, restoreTask, updateTask)
import HMem.DB.TestHarness
import HMem.Types
import Data.UUID qualified as UUID

spec :: Spec
spec = beforeAll setupTestPool $ aroundWith withTestTransaction $ do

  describe "createProject / getProject" $ do
    it "creates and retrieves a project" $ \env -> do
      ws <- createTestWorkspace env "proj-ws"
      let cp = CreateProject
            { workspaceId = ws.id
            , parentId    = Nothing
            , name        = "Test Project"
            , description = Just "A test project"
            , priority    = Just 8
            , metadata    = Nothing
            }
      proj <- createProject env.pool cp
      proj.name `shouldBe` "Test Project"
      proj.priority `shouldBe` 8
      proj.status `shouldBe` ProjActive  -- default

      got <- getProject env.pool proj.id
      got `shouldSatisfy` isJust
      let Just p = got
      p.name `shouldBe` "Test Project"
      p.description `shouldBe` Just "A test project"

    it "returns Nothing for nonexistent ID" $ \env -> do
      got <- getProject env.pool (read "00000000-0000-0000-0000-000000000099")
      got `shouldSatisfy` isNothing

    it "defaults priority to 5" $ \env -> do
      ws <- createTestWorkspace env "projdef-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "default pri"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      proj.priority `shouldBe` 5

  describe "updateProject" $ do
    it "updates name and status" $ \env -> do
      ws <- createTestWorkspace env "projup-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Original"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      updated <- updateProject env.pool proj.id UpdateProject
        { name = Just "Renamed", description = Unchanged
        , parentId = Unchanged
        , status = Just ProjPaused, priority = Nothing, metadata = Nothing }
      updated `shouldSatisfy` isJust
      let Just u = updated
      u.name `shouldBe` "Renamed"
      u.status `shouldBe` ProjPaused

    it "preserves unchanged fields" $ \env -> do
      ws <- createTestWorkspace env "projpres-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Keep"
        , description = Just "keep this", priority = Just 7, metadata = Nothing }
      updated <- updateProject env.pool proj.id UpdateProject
        { name = Nothing, description = Unchanged
        , parentId = Unchanged
        , status = Nothing, priority = Nothing, metadata = Nothing }
      let Just u = updated
      u.name `shouldBe` "Keep"
      u.description `shouldBe` Just "keep this"
      u.priority `shouldBe` 7

    it "reparents a project" $ \env -> do
      ws <- createTestWorkspace env "projmove-ws"
      leftParent <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Left"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      rightParent <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Right"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      child <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Just leftParent.id, name = "Child"
        , description = Nothing, priority = Nothing, metadata = Nothing }

      updated <- updateProject env.pool child.id UpdateProject
        { name = Nothing, description = Unchanged
        , parentId = SetTo rightParent.id
        , status = Nothing, priority = Nothing, metadata = Nothing }

      let Just moved = updated
      moved.parentId `shouldBe` Just rightParent.id

    it "clears a project's parent" $ \env -> do
      ws <- createTestWorkspace env "projclear-ws"
      parent <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Parent"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      child <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Just parent.id, name = "Child"
        , description = Nothing, priority = Nothing, metadata = Nothing }

      updated <- updateProject env.pool child.id UpdateProject
        { name = Nothing, description = Unchanged
        , parentId = SetNull
        , status = Nothing, priority = Nothing, metadata = Nothing }

      let Just detached = updated
      detached.parentId `shouldBe` Nothing

    it "rejects cycles when reparenting" $ \env -> do
      ws <- createTestWorkspace env "projcycle-ws"
      root <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Root"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      child <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Just root.id, name = "Child"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      grandchild <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Just child.id, name = "Grandchild"
        , description = Nothing, priority = Nothing, metadata = Nothing }

      result <- try @DBException $ updateProject env.pool root.id UpdateProject
        { name = Nothing, description = Unchanged
        , parentId = SetTo grandchild.id
        , status = Nothing, priority = Nothing, metadata = Nothing }
      case result of
        Left (DBCycleDetected _) -> pure ()
        other -> expectationFailure $ "Expected DBCycleDetected, got: " <> show other

    it "rejects parents from a different workspace" $ \env -> do
      leftWs <- createTestWorkspace env "projscope-left"
      rightWs <- createTestWorkspace env "projscope-right"
      foreignParent <- createProject env.pool CreateProject
        { workspaceId = leftWs.id, parentId = Nothing, name = "Foreign"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      proj <- createProject env.pool CreateProject
        { workspaceId = rightWs.id, parentId = Nothing, name = "Local"
        , description = Nothing, priority = Nothing, metadata = Nothing }

      result <- try @DBException $ updateProject env.pool proj.id UpdateProject
        { name = Nothing, description = Unchanged
        , parentId = SetTo foreignParent.id
        , status = Nothing, priority = Nothing, metadata = Nothing }
      case result of
        Left (DBCheckViolation _) -> pure ()
        other -> expectationFailure $ "Expected DBCheckViolation, got: " <> show other

    forM_ [(ProjActive, "active" :: Text), (ProjPaused, "paused")] $ \(childStatus, suffix) ->
      it ("rejects closing a project while a descendant project is " <> T.unpack suffix) $ \env -> do
        ws <- createTestWorkspace env ("proj-open-child-" <> suffix)
        root <- createProject env.pool CreateProject
          { workspaceId = ws.id, parentId = Nothing, name = "Root"
          , description = Nothing, priority = Nothing, metadata = Nothing }
        child <- createProject env.pool CreateProject
          { workspaceId = ws.id, parentId = Just root.id, name = "Child"
          , description = Nothing, priority = Nothing, metadata = Nothing }
        whenProjectStatus childStatus $ \statusValue ->
          void $ updateProject env.pool child.id (projectStatusUpdate statusValue)

        result <- try @DBException $ updateProject env.pool root.id (projectStatusUpdate ProjCompleted)
        expectLifecycle "PROJECT_COMPLETION_BLOCKED" result

    forM_ [(ProjCompleted, "completed" :: Text), (ProjArchived, "archived")] $ \(closingStatus, suffix) ->
      it ("rejects setting a project " <> T.unpack suffix <> " with open tasks in its subtree") $ \env -> do
        ws <- createTestWorkspace env ("proj-open-task-" <> suffix)
        root <- createProject env.pool CreateProject
          { workspaceId = ws.id, parentId = Nothing, name = "Root"
          , description = Nothing, priority = Nothing, metadata = Nothing }
        child <- createProject env.pool CreateProject
          { workspaceId = ws.id, parentId = Just root.id, name = "Child"
          , description = Nothing, priority = Nothing, metadata = Nothing }
        _ <- createTask env.pool CreateTask
          { workspaceId = ws.id, projectId = Just child.id, parentId = Nothing, title = "Open task"
          , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }

        result <- try @DBException $ updateProject env.pool root.id (projectStatusUpdate closingStatus)
        expectLifecycle "PROJECT_COMPLETION_BLOCKED" result

    it "allows closing a project after descendant projects and tasks are closed" $ \env -> do
      ws <- createTestWorkspace env "proj-closed-subtree-ws"
      root <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Root"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      child <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Just root.id, name = "Child"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      doneTask <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just child.id, parentId = Nothing, title = "Done task"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      cancelledTask <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just child.id, parentId = Nothing, title = "Cancelled task"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }

      void $ updateTask env.pool doneTask.id (taskStatusUpdate Done)
      void $ updateTask env.pool cancelledTask.id (taskStatusUpdate Cancelled)
      void $ updateProject env.pool child.id (projectStatusUpdate ProjCompleted)
      updated <- updateProject env.pool root.id (projectStatusUpdate ProjCompleted)

      let Just closedRoot = updated
      closedRoot.status `shouldBe` ProjCompleted

    it "rejects creating an active project under a closed project" $ \env -> do
      ws <- createTestWorkspace env "proj-create-under-closed-ws"
      parent <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Parent"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      void $ updateProject env.pool parent.id (projectStatusUpdate ProjCompleted)

      result <- try @DBException $ createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Just parent.id, name = "Child"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      expectLifecycle "PROJECT_OPEN_UNDER_CLOSED_PROJECT" result

    it "rejects reparenting an active project under a closed project" $ \env -> do
      ws <- createTestWorkspace env "proj-reparent-under-closed-ws"
      closedParent <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Closed parent"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      child <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Child"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      void $ updateProject env.pool closedParent.id (projectStatusUpdate ProjCompleted)

      result <- try @DBException $ updateProject env.pool child.id UpdateProject
        { name = Nothing, description = Unchanged, parentId = SetTo closedParent.id
        , status = Nothing, priority = Nothing, metadata = Nothing }
      expectLifecycle "PROJECT_OPEN_UNDER_CLOSED_PROJECT" result

    it "rejects restoring an active project under a closed project" $ \env -> do
      ws <- createTestWorkspace env "proj-restore-under-closed-ws"
      parent <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Parent"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      child <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Just parent.id, name = "Child"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      deleteProject env.pool child.id `shouldReturn` True
      void $ updateProject env.pool parent.id (projectStatusUpdate ProjCompleted)

      result <- try @DBException $ restoreProject env.pool child.id
      expectLifecycle "PROJECT_OPEN_UNDER_CLOSED_PROJECT" result

    it "rejects creating an open task inside a closed project" $ \env -> do
      ws <- createTestWorkspace env "task-create-in-closed-project-ws"
      project <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Project"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      void $ updateProject env.pool project.id (projectStatusUpdate ProjCompleted)

      result <- try @DBException $ createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just project.id, parentId = Nothing, title = "Open task"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      expectLifecycle "TASK_OPEN_UNDER_CLOSED_PROJECT" result

    it "rejects restoring an open task inside a closed project" $ \env -> do
      ws <- createTestWorkspace env "task-restore-in-closed-project-ws"
      project <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Project"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      task <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just project.id, parentId = Nothing, title = "Open task"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      deleteTask env.pool task.id `shouldReturn` True
      void $ updateProject env.pool project.id (projectStatusUpdate ProjCompleted)

      result <- try @DBException $ restoreTask env.pool task.id
      expectLifecycle "TASK_OPEN_UNDER_CLOSED_PROJECT" result

    it "rejects direct SQL project completion with open descendants" $ \env -> do
      ws <- createTestWorkspace env "proj-direct-sql-gate-ws"
      root <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Root"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      _ <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Just root.id, name = "Child"
        , description = Nothing, priority = Nothing, metadata = Nothing }

      result <- try @DBException $ execSql env $ "UPDATE projects SET status = 'completed' WHERE id = '" <> UUID.toString root.id <> "'"
      expectLifecycle "PROJECT_COMPLETION_BLOCKED" result

    -- The transaction-isolated DB spec uses a single pooled connection, so
    -- these parent-first/child-first cases cover the same lifecycle endpoints
    -- that concurrent close/open races exercise without a brittle two-session
    -- timing test.
    it "rejects parent-first batch project completion while a child project is open" $ \env -> do
      ws <- createTestWorkspace env "proj-batch-gate-fail-ws"
      root <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Root"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      child <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Just root.id, name = "Child"
        , description = Nothing, priority = Nothing, metadata = Nothing }

      parentFirst <- try @DBException $ updateProjectBatch env.pool
        [ (root.id, projectStatusUpdate ProjCompleted)
        , (child.id, projectStatusUpdate ProjCompleted)
        ]
      expectLifecycle "PROJECT_COMPLETION_BLOCKED" parentFirst

    it "allows child-first batch project completion" $ \env -> do
      ws <- createTestWorkspace env "proj-batch-gate-success-ws"
      root <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Root"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      child <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Just root.id, name = "Child"
        , description = Nothing, priority = Nothing, metadata = Nothing }

      childFirst <- updateProjectBatch env.pool
        [ (child.id, projectStatusUpdate ProjCompleted)
        , (root.id, projectStatusUpdate ProjCompleted)
        ]
      childFirst `shouldBe` 2

    it "returns Nothing for nonexistent ID" $ \env -> do
      result <- updateProject env.pool (read "00000000-0000-0000-0000-000000000099") UpdateProject
        { name = Just "x", description = Unchanged, parentId = Unchanged, status = Nothing
        , priority = Nothing, metadata = Nothing }
      result `shouldSatisfy` isNothing

  describe "deleteProject" $ do
    it "deletes an existing project" $ \env -> do
      ws <- createTestWorkspace env "projdel-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Doomed"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      ok <- deleteProject env.pool proj.id
      ok `shouldBe` True
      got <- getProject env.pool proj.id
      got `shouldSatisfy` isNothing

    it "returns False for nonexistent ID" $ \env -> do
      ok <- deleteProject env.pool (read "00000000-0000-0000-0000-000000000099")
      ok `shouldBe` False

    it "keeps tasks by clearing their project_id" $ \env -> do
      ws <- createTestWorkspace env "projtask-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "To Archive"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      task <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Keep me"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }

      ok <- deleteProject env.pool proj.id
      ok `shouldBe` True

      got <- getTask env.pool task.id
      got `shouldSatisfy` isJust
      let Just detached = got
      detached.projectId `shouldBe` Nothing

      tasks <- listTasksByWorkspace env.pool ws.id Nothing Nothing Nothing Nothing
      map (.id) tasks `shouldContain` [task.id]

  describe "listProjects" $ do
    it "lists projects for a workspace" $ \env -> do
      ws <- createTestWorkspace env "projlist-ws"
      _ <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "P1"
        , description = Nothing, priority = Just 3, metadata = Nothing }
      _ <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "P2"
        , description = Nothing, priority = Just 9, metadata = Nothing }
      projs <- listProjects env.pool ws.id Nothing Nothing Nothing
      length projs `shouldBe` 2
      -- Should be ordered by priority DESC
      (head projs).priority `shouldBe` 9

    it "filters by status" $ \env -> do
      ws <- createTestWorkspace env "projfilt-ws"
      p1 <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Active"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      _ <- updateProject env.pool p1.id UpdateProject
        { name = Nothing, description = Unchanged, parentId = Unchanged, status = Just ProjCompleted
        , priority = Nothing, metadata = Nothing }
      _ <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "StillActive"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      active <- listProjects env.pool ws.id (Just ProjActive) Nothing Nothing
      length active `shouldBe` 1
      (head active).name `shouldBe` "StillActive"

  describe "memory links" $ do
    it "links and unlinks project to memory" $ \env -> do
      ws <- createTestWorkspace env "projmem-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Linked Proj"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      mem <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "linked memory", summary = Nothing
        , memoryType = ShortTerm, importance = Nothing, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      -- Link should not throw
      linkProjectMemory env.pool proj.id mem.id
      -- Linking again should be idempotent (DoNothing)
      linkProjectMemory env.pool proj.id mem.id
      -- Unlink
      unlinkProjectMemory env.pool proj.id mem.id

    it "filters linked project memories by query, tags, importance, type, and access count" $ \env -> do
      ws <- createTestWorkspace env "projmem-filter-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Filter Proj"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      matching <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "alpha haskell note", summary = Nothing
        , memoryType = LongTerm, importance = Just 8, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Just ["keep"], ftsLanguage = Nothing }
      nonMatching <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "alpha haskell note", summary = Nothing
        , memoryType = ShortTerm, importance = Just 3, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Just ["drop"], ftsLanguage = Nothing }
      linkProjectMemory env.pool proj.id matching.id
      linkProjectMemory env.pool proj.id nonMatching.id
      touchMemory env.pool matching.id
      _ <- updateMemory env.pool matching.id UpdateMemory
        { content = Nothing, summary = Unchanged, memoryType = Nothing, importance = Nothing
        , metadata = Nothing, expiresAt = Unchanged, source = Unchanged, confidence = Nothing, pinned = Just True }
      filtered <- getProjectMemories env.pool proj.id LinkedMemoryListQuery
        { query = Just "alpha haskell"
        , tags = Just ["keep"]
        , minImportance = Just 5
        , memoryType = Just LongTerm
        , minAccessCount = Just 1
        }
      map (.id) filtered `shouldBe` [matching.id]

projectStatusUpdate :: ProjectStatus -> UpdateProject
projectStatusUpdate statusValue = UpdateProject
  { name = Nothing
  , description = Unchanged
  , parentId = Unchanged
  , status = Just statusValue
  , priority = Nothing
  , metadata = Nothing
  }

taskStatusUpdate :: TaskStatus -> UpdateTask
taskStatusUpdate statusValue = UpdateTask
  { title = Nothing
  , description = Unchanged
  , projectId = Unchanged
  , parentId = Unchanged
  , status = Just statusValue
  , priority = Nothing
  , metadata = Nothing
  , dueAt = Unchanged
  }

whenProjectStatus :: ProjectStatus -> (ProjectStatus -> IO ()) -> IO ()
whenProjectStatus ProjActive _ = pure ()
whenProjectStatus statusValue action = action statusValue

expectLifecycle :: Text -> Either DBException a -> Expectation
expectLifecycle expected result = case result of
  Left (DBLifecycleViolation actual _ _ _) -> actual `shouldBe` expected
  Left other -> expectationFailure $ "Expected DBLifecycleViolation " <> show expected <> ", got: " <> show other
  Right _ -> expectationFailure $ "Expected DBLifecycleViolation " <> show expected <> ", got success"

execSql :: TestEnv -> String -> IO ()
execSql env sql = runSession env.pool (Session.sql (BS8.pack sql))
