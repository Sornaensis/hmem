{-# OPTIONS_GHC -Wno-x-partial -Wno-incomplete-uni-patterns #-}

module HMem.DB.TaskSpec (spec) where

import Control.Exception (try)
import Control.Monad (forM_, void)
import Data.ByteString.Char8 qualified as BS8
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Test.Hspec
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement

import HMem.DB.Memory (createMemory, getMemory, getTaskMemories, touchMemory, updateMemory)
import HMem.DB.Pool (DBException(..), runSession)
import HMem.DB.Project (createProject, updateProject)
import HMem.DB.Task
import HMem.DB.TestHarness
import HMem.Types

import Data.UUID (UUID)
import Data.UUID qualified as UUID

spec :: Spec
spec = beforeAll setupTestPool $ aroundWith withTestTransaction $ do

  describe "createTask / getTask" $ do
    it "creates and retrieves a task" $ \env -> do
      ws <- createTestWorkspace env "task-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Task Proj"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      let ct = CreateTask
            { workspaceId = ws.id
            , projectId   = Just proj.id
            , parentId    = Nothing
            , title       = "Write tests"
            , description = Just "We need tests"
            , priority    = Just 9
            , metadata    = Nothing
            , dueAt       = Nothing
            }
      task <- createTask env.pool ct
      task.title `shouldBe` "Write tests"
      task.priority `shouldBe` 9
      task.status `shouldBe` Todo  -- default
      task.completedAt `shouldSatisfy` isNothing

      got <- getTask env.pool task.id
      got `shouldSatisfy` isJust
      let Just t = got
      t.title `shouldBe` "Write tests"
      t.description `shouldBe` Just "We need tests"

    it "returns Nothing for nonexistent ID" $ \env -> do
      got <- getTask env.pool (read "00000000-0000-0000-0000-000000000099")
      got `shouldSatisfy` isNothing

    it "defaults priority to 5" $ \env -> do
      ws <- createTestWorkspace env "taskdef-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "TP"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      task <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "default"
        , description = Nothing, priority = Nothing, metadata = Nothing
        , dueAt = Nothing }
      task.priority `shouldBe` 5

  describe "updateTask" $ do
    it "updates title and status" $ \env -> do
      ws <- createTestWorkspace env "taskup-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "UP"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      task <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Original"
        , description = Nothing, priority = Nothing, metadata = Nothing
        , dueAt = Nothing }
      updated <- updateTask env.pool task.id UpdateTask
        { title = Just "Updated Title", description = Unchanged
        , projectId = Unchanged, parentId = Unchanged
        , status = Just InProgress, priority = Nothing
        , metadata = Nothing, dueAt = Unchanged }
      updated `shouldSatisfy` isJust
      let Just u = updated
      u.title `shouldBe` "Updated Title"
      u.status `shouldBe` InProgress

    it "sets completedAt when status becomes Done" $ \env -> do
      ws <- createTestWorkspace env "taskdone-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Done"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      task <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Finish me"
        , description = Nothing, priority = Nothing, metadata = Nothing
        , dueAt = Nothing }
      task.completedAt `shouldSatisfy` isNothing
      updated <- updateTask env.pool task.id UpdateTask
        { title = Nothing, description = Unchanged
        , projectId = Unchanged, parentId = Unchanged
        , status = Just Done, priority = Nothing
        , metadata = Nothing, dueAt = Unchanged }
      let Just u = updated
      u.status `shouldBe` Done
      u.completedAt `shouldSatisfy` isJust

    it "preserves unchanged fields" $ \env -> do
      ws <- createTestWorkspace env "taskpres-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Pres"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      task <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Keep"
        , description = Just "keep this", priority = Just 7
        , metadata = Nothing, dueAt = Nothing }
      updated <- updateTask env.pool task.id UpdateTask
        { title = Nothing, description = Unchanged, projectId = Unchanged, parentId = Unchanged, status = Nothing
        , priority = Nothing, metadata = Nothing, dueAt = Unchanged }
      let Just u = updated
      u.title `shouldBe` "Keep"
      u.description `shouldBe` Just "keep this"
      u.priority `shouldBe` 7

    it "moves a task between projects and cascades descendants" $ \env -> do
      ws <- createTestWorkspace env "taskmove-ws"
      sourceProj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Source"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      targetProj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Target"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      parent <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just sourceProj.id, parentId = Nothing, title = "Parent"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      child <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just sourceProj.id, parentId = Just parent.id, title = "Child"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }

      _ <- updateTask env.pool child.id UpdateTask
        { title = Nothing, description = Unchanged, projectId = Unchanged, parentId = Unchanged
        , status = Just Done, priority = Nothing, metadata = Nothing, dueAt = Unchanged }

      _ <- updateTask env.pool parent.id UpdateTask
        { title = Nothing, description = Unchanged, projectId = Unchanged, parentId = Unchanged
        , status = Just Done, priority = Nothing, metadata = Nothing, dueAt = Unchanged }

      moved <- updateTask env.pool parent.id UpdateTask
        { title = Nothing, description = Unchanged, projectId = SetTo targetProj.id, parentId = Unchanged
        , status = Nothing, priority = Nothing, metadata = Nothing, dueAt = Unchanged }

      let Just updatedParent = moved
      updatedParent.projectId `shouldBe` Just targetProj.id
      updatedParent.status `shouldBe` Done
      updatedParent.completedAt `shouldSatisfy` isJust

      descendant <- getTask env.pool child.id
      let Just updatedChild = descendant
      updatedChild.projectId `shouldBe` Just targetProj.id

    it "reparents a task within a project" $ \env -> do
      ws <- createTestWorkspace env "taskreparent-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Proj"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      leftParent <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Left"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      rightParent <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Right"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      child <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Just leftParent.id, title = "Child"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }

      updated <- updateTask env.pool child.id UpdateTask
        { title = Nothing, description = Unchanged, projectId = Unchanged, parentId = SetTo rightParent.id
        , status = Nothing, priority = Nothing, metadata = Nothing, dueAt = Unchanged }

      let Just moved = updated
      moved.parentId `shouldBe` Just rightParent.id
      moved.projectId `shouldBe` Just proj.id

    it "moves a task to workspace scope" $ \env -> do
      ws <- createTestWorkspace env "taskdetach-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Proj"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      task <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Scoped"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }

      updated <- updateTask env.pool task.id UpdateTask
        { title = Nothing, description = Unchanged, projectId = SetNull, parentId = SetNull
        , status = Nothing, priority = Nothing, metadata = Nothing, dueAt = Unchanged }

      let Just detached = updated
      detached.projectId `shouldBe` Nothing
      detached.parentId `shouldBe` Nothing

    it "rejects mismatched parent and project placement" $ \env -> do
      ws <- createTestWorkspace env "taskmismatch-ws"
      leftProj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Left"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      rightProj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Right"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      parent <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just leftProj.id, parentId = Nothing, title = "Parent"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      child <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just leftProj.id, parentId = Just parent.id, title = "Child"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }

      result <- try @DBException $ updateTask env.pool child.id UpdateTask
        { title = Nothing, description = Unchanged, projectId = SetTo rightProj.id, parentId = Unchanged
        , status = Nothing, priority = Nothing, metadata = Nothing, dueAt = Unchanged }
      case result of
        Left (DBCheckViolation _) -> pure ()
        other -> expectationFailure $ "Expected DBCheckViolation, got: " <> show other

    it "rejects cycles when reparenting tasks" $ \env -> do
      ws <- createTestWorkspace env "taskcyclehier-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Proj"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      root <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Root"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      child <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Just root.id, title = "Child"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      grandchild <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Just child.id, title = "Grandchild"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }

      result <- try @DBException $ updateTask env.pool root.id UpdateTask
        { title = Nothing, description = Unchanged, projectId = Unchanged, parentId = SetTo grandchild.id
        , status = Nothing, priority = Nothing, metadata = Nothing, dueAt = Unchanged }
      case result of
        Left (DBCycleDetected _) -> pure ()
        other -> expectationFailure $ "Expected DBCycleDetected, got: " <> show other

    forM_ [(Todo, "todo" :: Text), (InProgress, "inprogress"), (Blocked, "blocked")] $ \(childStatus, suffix) ->
      it ("rejects marking a task done while a " <> T.unpack suffix <> " descendant is open") $ \env -> do
        ws <- createTestWorkspace env ("task-open-desc-" <> suffix)
        proj <- createProject env.pool CreateProject
          { workspaceId = ws.id, parentId = Nothing, name = "Proj"
          , description = Nothing, priority = Nothing, metadata = Nothing }
        parent <- createTask env.pool CreateTask
          { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Parent"
          , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
        child <- createTask env.pool CreateTask
          { workspaceId = ws.id, projectId = Just proj.id, parentId = Just parent.id, title = "Child"
          , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
        whenStatus childStatus $ \statusValue ->
          void $ updateTask env.pool child.id (taskStatusUpdate statusValue)

        result <- try @DBException $ updateTask env.pool parent.id (taskStatusUpdate Done)
        expectLifecycle "TASK_COMPLETION_BLOCKED" result

    it "allows marking a task done when descendants are done or cancelled" $ \env -> do
      ws <- createTestWorkspace env "task-closed-desc-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Proj"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      parent <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Parent"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      doneChild <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Just parent.id, title = "Done child"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      cancelledChild <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Just parent.id, title = "Cancelled child"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }

      void $ updateTask env.pool doneChild.id (taskStatusUpdate Done)
      void $ updateTask env.pool cancelledChild.id (taskStatusUpdate Cancelled)
      updated <- updateTask env.pool parent.id (taskStatusUpdate Done)

      let Just parentDone = updated
      parentDone.status `shouldBe` Done
      parentDone.completedAt `shouldSatisfy` isJust

    it "rejects creating an open child under a done task" $ \env -> do
      ws <- createTestWorkspace env "task-open-child-done-parent-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Proj"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      parent <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Parent"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      void $ updateTask env.pool parent.id (taskStatusUpdate Done)

      result <- try @DBException $ createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Just parent.id, title = "Open child"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      expectLifecycle "TASK_OPEN_UNDER_DONE_TASK" result

    it "rejects reopening a descendant under a done task" $ \env -> do
      ws <- createTestWorkspace env "task-reopen-under-done-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Proj"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      parent <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Parent"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      child <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Just parent.id, title = "Child"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      void $ updateTask env.pool child.id (taskStatusUpdate Done)
      void $ updateTask env.pool parent.id (taskStatusUpdate Done)

      result <- try @DBException $ updateTask env.pool child.id (taskStatusUpdate InProgress)
      expectLifecycle "TASK_OPEN_UNDER_DONE_TASK" result

    it "rejects reparenting an open task under a done task" $ \env -> do
      ws <- createTestWorkspace env "task-reparent-under-done-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Proj"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      doneParent <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Done parent"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      openTask <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Open task"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      void $ updateTask env.pool doneParent.id (taskStatusUpdate Done)

      result <- try @DBException $ updateTask env.pool openTask.id UpdateTask
        { title = Nothing, description = Unchanged, projectId = Unchanged, parentId = SetTo doneParent.id
        , status = Nothing, priority = Nothing, metadata = Nothing, dueAt = Unchanged }
      expectLifecycle "TASK_OPEN_UNDER_DONE_TASK" result

    it "rejects reparenting a cancelled task subtree with open descendants under a done task" $ \env -> do
      ws <- createTestWorkspace env "task-reparent-cancelled-subtree-under-done-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Proj"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      doneParent <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Done parent"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      cancelledParent <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Cancelled parent"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      _ <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Just cancelledParent.id, title = "Open child"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      void $ updateTask env.pool doneParent.id (taskStatusUpdate Done)
      void $ updateTask env.pool cancelledParent.id (taskStatusUpdate Cancelled)

      result <- try @DBException $ updateTask env.pool cancelledParent.id UpdateTask
        { title = Nothing, description = Unchanged, projectId = Unchanged, parentId = SetTo doneParent.id
        , status = Nothing, priority = Nothing, metadata = Nothing, dueAt = Unchanged }
      expectLifecycle "TASK_OPEN_UNDER_DONE_TASK" result

    it "rejects moving a cancelled task subtree with open descendants into a closed project" $ \env -> do
      ws <- createTestWorkspace env "task-move-cancelled-subtree-closed-project-ws"
      sourceProj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Source"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      closedProj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Closed"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      cancelledParent <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just sourceProj.id, parentId = Nothing, title = "Cancelled parent"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      _ <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just sourceProj.id, parentId = Just cancelledParent.id, title = "Open child"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      void $ updateTask env.pool cancelledParent.id (taskStatusUpdate Cancelled)
      void $ updateProject env.pool closedProj.id (projectStatusUpdate ProjCompleted)

      result <- try @DBException $ updateTask env.pool cancelledParent.id UpdateTask
        { title = Nothing, description = Unchanged, projectId = SetTo closedProj.id, parentId = SetNull
        , status = Nothing, priority = Nothing, metadata = Nothing, dueAt = Unchanged }
      expectLifecycle "TASK_OPEN_UNDER_CLOSED_PROJECT" result

    it "rejects restoring an open child under a done task" $ \env -> do
      ws <- createTestWorkspace env "task-restore-under-done-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Proj"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      parent <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Parent"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      child <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Just parent.id, title = "Child"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      deleteTask env.pool child.id `shouldReturn` True
      void $ updateTask env.pool parent.id (taskStatusUpdate Done)

      result <- try @DBException $ restoreTask env.pool child.id
      expectLifecycle "TASK_OPEN_UNDER_DONE_TASK" result

    it "rejects direct SQL completion with open descendants" $ \env -> do
      ws <- createTestWorkspace env "task-direct-sql-gate-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Proj"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      parent <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Parent"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      _ <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Just parent.id, title = "Child"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }

      result <- try @DBException $ execSql env $ "UPDATE tasks SET status = 'done' WHERE id = '" <> UUID.toString parent.id <> "'"
      expectLifecycle "TASK_COMPLETION_BLOCKED" result

    -- The transaction-isolated DB spec uses a single pooled connection, so
    -- these parent-first/child-first cases cover the same lifecycle endpoints
    -- that concurrent close/open races exercise without a brittle two-session
    -- timing test.
    it "rejects parent-first batch task completion while a child is open" $ \env -> do
      ws <- createTestWorkspace env "task-batch-gate-fail-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Proj"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      parent <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Parent"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      child <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Just parent.id, title = "Child"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }

      parentFirst <- try @DBException $ updateTaskBatch env.pool
        [ (parent.id, taskStatusUpdate Done)
        , (child.id, taskStatusUpdate Done)
        ]
      expectLifecycle "TASK_COMPLETION_BLOCKED" parentFirst

    it "allows child-first batch task completion" $ \env -> do
      ws <- createTestWorkspace env "task-batch-gate-success-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Proj"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      parent <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Parent"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      child <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Just parent.id, title = "Child"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }

      childFirst <- updateTaskBatch env.pool
        [ (child.id, taskStatusUpdate Done)
        , (parent.id, taskStatusUpdate Done)
        ]
      childFirst `shouldBe` 2

    it "returns Nothing for nonexistent ID" $ \env -> do
      result <- updateTask env.pool (read "00000000-0000-0000-0000-000000000099") UpdateTask
        { title = Just "x", description = Unchanged, projectId = Unchanged, parentId = Unchanged, status = Nothing
        , priority = Nothing, metadata = Nothing, dueAt = Unchanged }
      result `shouldSatisfy` isNothing

  describe "deleteTask" $ do
    it "deletes an existing task" $ \env -> do
      ws <- createTestWorkspace env "taskdel-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Del"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      task <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Doomed"
        , description = Nothing, priority = Nothing, metadata = Nothing
        , dueAt = Nothing }
      ok <- deleteTask env.pool task.id
      ok `shouldBe` True
      got <- getTask env.pool task.id
      got `shouldSatisfy` isNothing

    it "returns False for nonexistent ID" $ \env -> do
      ok <- deleteTask env.pool (read "00000000-0000-0000-0000-000000000099")
      ok `shouldBe` False

    it "deletes subtasks when deleting a parent task" $ \env -> do
      ws <- createTestWorkspace env "task-subdel-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Nested"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      parent <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Parent"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      child <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Just parent.id, title = "Child"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }

      ok <- deleteTask env.pool parent.id
      ok `shouldBe` True

      getTask env.pool parent.id `shouldReturn` Nothing
      getTask env.pool child.id `shouldReturn` Nothing

  describe "listTasks" $ do
    it "lists tasks for a project ordered by priority" $ \env -> do
      ws <- createTestWorkspace env "tasklist-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "List"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      _ <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Low"
        , description = Nothing, priority = Just 2, metadata = Nothing
        , dueAt = Nothing }
      _ <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "High"
        , description = Nothing, priority = Just 9, metadata = Nothing
        , dueAt = Nothing }
      tasks <- listTasks env.pool proj.id Nothing Nothing Nothing
      length tasks `shouldBe` 2
      (head tasks).priority `shouldBe` 9

    it "filters by status" $ \env -> do
      ws <- createTestWorkspace env "taskfilt-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Filt"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      t1 <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "todo1"
        , description = Nothing, priority = Nothing, metadata = Nothing
        , dueAt = Nothing }
      _ <- updateTask env.pool t1.id UpdateTask
        { title = Nothing, description = Unchanged, projectId = Unchanged, parentId = Unchanged, status = Just Done
        , priority = Nothing, metadata = Nothing, dueAt = Unchanged }
      _ <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "todo2"
        , description = Nothing, priority = Nothing, metadata = Nothing
        , dueAt = Nothing }
      todos <- listTasks env.pool proj.id (Just Todo) Nothing Nothing
      length todos `shouldBe` 1
      (head todos).title `shouldBe` "todo2"

  describe "dependencies" $ do
    it "adds and removes task dependencies" $ \env -> do
      ws <- createTestWorkspace env "taskdep-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Dep"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      t1 <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "First"
        , description = Nothing, priority = Nothing, metadata = Nothing
        , dueAt = Nothing }
      t2 <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Second"
        , description = Nothing, priority = Nothing, metadata = Nothing
        , dueAt = Nothing }
      -- Should not throw
      addDependency env.pool t2.id t1.id
      -- Adding again should be idempotent (DoNothing)
      addDependency env.pool t2.id t1.id
      -- Remove
      removeDependency env.pool t2.id t1.id

    it "rejects self-referential dependency" $ \env -> do
      ws <- createTestWorkspace env "taskself-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Self"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      t1 <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Self"
        , description = Nothing, priority = Nothing, metadata = Nothing
        , dueAt = Nothing }
      result <- try @DBException $ addDependency env.pool t1.id t1.id
      case result of
        Left (DBCheckViolation _) -> pure ()
        other -> expectationFailure $ "Expected DBCheckViolation, got: " <> show other

    it "rejects direct 2-node cycle" $ \env -> do
      ws <- createTestWorkspace env "taskcyc2-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Cyc2"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      t1 <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "A"
        , description = Nothing, priority = Nothing, metadata = Nothing
        , dueAt = Nothing }
      t2 <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "B"
        , description = Nothing, priority = Nothing, metadata = Nothing
        , dueAt = Nothing }
      addDependency env.pool t2.id t1.id  -- B depends on A (OK)
      result <- try @DBException $ addDependency env.pool t1.id t2.id  -- A depends on B (cycle!)
      case result of
        Left (DBCycleDetected _) -> pure ()
        other -> expectationFailure $ "Expected DBCycleDetected, got: " <> show other

    it "rejects multi-node cycle (A->B->C->A)" $ \env -> do
      ws <- createTestWorkspace env "taskcyc3-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Cyc3"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      let mkTask title = createTask env.pool CreateTask
            { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = title
            , description = Nothing, priority = Nothing, metadata = Nothing
            , dueAt = Nothing }
      t1 <- mkTask "A"
      t2 <- mkTask "B"
      t3 <- mkTask "C"
      addDependency env.pool t2.id t1.id  -- B depends on A
      addDependency env.pool t3.id t2.id  -- C depends on B
      result <- try @DBException $ addDependency env.pool t1.id t3.id  -- A depends on C (cycle!)
      case result of
        Left (DBCycleDetected _) -> pure ()
        other -> expectationFailure $ "Expected DBCycleDetected, got: " <> show other

    it "allows valid DAG (diamond shape)" $ \env -> do
      ws <- createTestWorkspace env "taskdag-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "DAG"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      let mkTask title = createTask env.pool CreateTask
            { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = title
            , description = Nothing, priority = Nothing, metadata = Nothing
            , dueAt = Nothing }
      t1 <- mkTask "Root"
      t2 <- mkTask "Left"
      t3 <- mkTask "Right"
      t4 <- mkTask "Sink"
      -- Diamond: t2->t1, t3->t1, t4->t2, t4->t3
      addDependency env.pool t2.id t1.id
      addDependency env.pool t3.id t1.id
      addDependency env.pool t4.id t2.id
      addDependency env.pool t4.id t3.id
      -- All should succeed -- no cycle in a diamond

    it "auto-blocks a task with an open dependency and unblocks when the dependency is done" $ \env -> do
      ws <- createTestWorkspace env "taskdep-autoblock-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "AutoBlock"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      dependency <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Dependency"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      dependent <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Dependent"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }

      addDependency env.pool dependent.id dependency.id
      getTask env.pool dependent.id >>= (\case
        Just task -> task.status `shouldBe` Blocked
        Nothing -> expectationFailure "dependent task not found")

      _ <- updateTask env.pool dependency.id UpdateTask
        { title = Nothing, description = Unchanged, projectId = Unchanged, parentId = Unchanged
        , status = Just Done, priority = Nothing, metadata = Nothing, dueAt = Unchanged }
      getTask env.pool dependent.id >>= (\case
        Just task -> task.status `shouldBe` Todo
        Nothing -> expectationFailure "dependent task not found")

    it "auto-blocks a done task when a new open dependency is added" $ \env -> do
      ws <- createTestWorkspace env "taskdep-done-autoblock-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Done AutoBlock"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      dependency <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Dependency"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      dependent <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Dependent"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      _ <- updateTask env.pool dependent.id UpdateTask
        { title = Nothing, description = Unchanged, projectId = Unchanged, parentId = Unchanged
        , status = Just Done, priority = Nothing, metadata = Nothing, dueAt = Unchanged }

      addDependency env.pool dependent.id dependency.id
      getTask env.pool dependent.id >>= (\case
        Just task -> do
          task.status `shouldBe` Blocked
          task.completedAt `shouldSatisfy` isNothing
        Nothing -> expectationFailure "dependent task not found")

    it "preserves manually blocked tasks when dependencies are removed" $ \env -> do
      ws <- createTestWorkspace env "taskdep-manual-blocked-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Manual Blocked"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      dependency <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Dependency"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      dependent <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Dependent"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }

      _ <- updateTask env.pool dependent.id UpdateTask
        { title = Nothing, description = Unchanged, projectId = Unchanged, parentId = Unchanged
        , status = Just Blocked, priority = Nothing, metadata = Nothing, dueAt = Unchanged }
      addDependency env.pool dependent.id dependency.id
      removeDependency env.pool dependent.id dependency.id
      getTask env.pool dependent.id >>= (\case
        Just task -> task.status `shouldBe` Blocked
        Nothing -> expectationFailure "dependent task not found")

    it "does not reopen an auto-blocked task after it is cancelled" $ \env -> do
      ws <- createTestWorkspace env "taskdep-cancelled-autoblock-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Cancelled AutoBlock"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      dependency <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Dependency"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      dependent <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Dependent"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      addDependency env.pool dependent.id dependency.id
      _ <- updateTask env.pool dependent.id UpdateTask
        { title = Nothing, description = Unchanged, projectId = Unchanged, parentId = Unchanged
        , status = Just Cancelled, priority = Nothing, metadata = Nothing, dueAt = Unchanged }
      taskAutoBlocked env dependent.id `shouldReturn` False

      _ <- updateTask env.pool dependency.id UpdateTask
        { title = Nothing, description = Unchanged, projectId = Unchanged, parentId = Unchanged
        , status = Just Done, priority = Nothing, metadata = Nothing, dueAt = Unchanged }
      getTask env.pool dependent.id >>= (\case
        Just task -> task.status `shouldBe` Cancelled
        Nothing -> expectationFailure "dependent task not found")

    it "auto-blocks recursively through task ancestors" $ \env -> do
      ws <- createTestWorkspace env "taskdep-recursive-autoblock-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Recursive AutoBlock"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      root <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Root"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      child <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Just root.id, title = "Child"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      leaf <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Just child.id, title = "Leaf"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }

      getTask env.pool child.id >>= (\case
        Just task -> task.status `shouldBe` Blocked
        Nothing -> expectationFailure "child task not found")
      getTask env.pool root.id >>= (\case
        Just task -> task.status `shouldBe` Blocked
        Nothing -> expectationFailure "root task not found")

      _ <- updateTask env.pool leaf.id UpdateTask
        { title = Nothing, description = Unchanged, projectId = Unchanged, parentId = Unchanged
        , status = Just Done, priority = Nothing, metadata = Nothing, dueAt = Unchanged }
      getTask env.pool child.id >>= (\case
        Just task -> task.status `shouldBe` Todo
        Nothing -> expectationFailure "child task not found")
      getTask env.pool root.id >>= (\case
        Just task -> task.status `shouldBe` Blocked
        Nothing -> expectationFailure "root task not found")

      _ <- updateTask env.pool child.id UpdateTask
        { title = Nothing, description = Unchanged, projectId = Unchanged, parentId = Unchanged
        , status = Just Done, priority = Nothing, metadata = Nothing, dueAt = Unchanged }
      getTask env.pool root.id >>= (\case
        Just task -> task.status `shouldBe` Todo
        Nothing -> expectationFailure "root task not found")

    it "auto-blocks across transitive dependency chains" $ \env -> do
      ws <- createTestWorkspace env "taskdep-transitive-autoblock-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Transitive AutoBlock"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      c <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "C"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      b <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "B"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      a <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "A"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }

      _ <- updateTask env.pool c.id UpdateTask
        { title = Nothing, description = Unchanged, projectId = Unchanged, parentId = Unchanged
        , status = Just Done, priority = Nothing, metadata = Nothing, dueAt = Unchanged }
      addDependency env.pool b.id c.id
      _ <- updateTask env.pool b.id UpdateTask
        { title = Nothing, description = Unchanged, projectId = Unchanged, parentId = Unchanged
        , status = Just Done, priority = Nothing, metadata = Nothing, dueAt = Unchanged }
      addDependency env.pool a.id b.id
      getTask env.pool a.id >>= (\case
        Just task -> task.status `shouldBe` Todo
        Nothing -> expectationFailure "A task not found")

      _ <- updateTask env.pool c.id UpdateTask
        { title = Nothing, description = Unchanged, projectId = Unchanged, parentId = Unchanged
        , status = Just Todo, priority = Nothing, metadata = Nothing, dueAt = Unchanged }

      getTask env.pool b.id >>= (\case
        Just task -> task.status `shouldBe` Blocked
        Nothing -> expectationFailure "B task not found")
      getTask env.pool a.id >>= (\case
        Just task -> task.status `shouldBe` Blocked
        Nothing -> expectationFailure "A task not found")

    it "moves blocking status when a subtask is reparented" $ \env -> do
      ws <- createTestWorkspace env "task-reparent-autoblock-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Reparent AutoBlock"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      oldParent <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Old parent"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      newParent <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "New parent"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      child <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Just oldParent.id, title = "Child"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      getTask env.pool oldParent.id >>= (\case
        Just task -> task.status `shouldBe` Blocked
        Nothing -> expectationFailure "old parent not found")

      _ <- updateTask env.pool child.id UpdateTask
        { title = Nothing, description = Unchanged, projectId = Unchanged, parentId = SetTo newParent.id
        , status = Nothing, priority = Nothing, metadata = Nothing, dueAt = Unchanged }

      getTask env.pool oldParent.id >>= (\case
        Just task -> task.status `shouldBe` Todo
        Nothing -> expectationFailure "old parent not found")
      getTask env.pool newParent.id >>= (\case
        Just task -> task.status `shouldBe` Blocked
        Nothing -> expectationFailure "new parent not found")

    it "unblocks and re-blocks ancestors when an open subtask is deleted and restored" $ \env -> do
      ws <- createTestWorkspace env "task-restore-autoblock-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Restore AutoBlock"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      parent <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Parent"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      child <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Just parent.id, title = "Child"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      getTask env.pool parent.id >>= (\case
        Just task -> task.status `shouldBe` Blocked
        Nothing -> expectationFailure "parent not found")

      deleteTask env.pool child.id `shouldReturn` True
      getTask env.pool parent.id >>= (\case
        Just task -> task.status `shouldBe` Todo
        Nothing -> expectationFailure "parent not found")

      restoreTask env.pool child.id `shouldReturn` True
      getTask env.pool parent.id >>= (\case
        Just task -> task.status `shouldBe` Blocked
        Nothing -> expectationFailure "parent not found")

    it "unblocks dependent tasks when a blocking dependency is deleted" $ \env -> do
      ws <- createTestWorkspace env "task-delete-dependency-autoblock-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Delete Dependency AutoBlock"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      dependency <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Dependency"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      dependent <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Dependent"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      addDependency env.pool dependent.id dependency.id
      getTask env.pool dependent.id >>= (\case
        Just task -> task.status `shouldBe` Blocked
        Nothing -> expectationFailure "dependent not found")

      deleteTask env.pool dependency.id `shouldReturn` True
      getTask env.pool dependent.id >>= (\case
        Just task -> task.status `shouldBe` Todo
        Nothing -> expectationFailure "dependent not found")

    it "preserves auto-blocked status across project batch moves" $ \env -> do
      ws <- createTestWorkspace env "task-batch-move-autoblock-ws"
      sourceProj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Source"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      targetProj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Target"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      parent <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just sourceProj.id, parentId = Nothing, title = "Parent"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      child <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just sourceProj.id, parentId = Just parent.id, title = "Child"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      getTask env.pool parent.id >>= (\case
        Just task -> task.status `shouldBe` Blocked
        Nothing -> expectationFailure "parent not found")

      moveTasksBatch env.pool [parent.id, child.id] (Just targetProj.id) `shouldReturn` 2
      getTask env.pool parent.id >>= (\case
        Just task -> do
          task.projectId `shouldBe` Just targetProj.id
          task.status `shouldBe` Blocked
        Nothing -> expectationFailure "parent not found")
      getTask env.pool child.id >>= (\case
        Just task -> task.projectId `shouldBe` Just targetProj.id
        Nothing -> expectationFailure "child not found")

    it "rejects cross-workspace task dependencies at the database layer" $ \env -> do
      wsA <- createTestWorkspace env "taskdep-cross-a"
      wsB <- createTestWorkspace env "taskdep-cross-b"
      projA <- createProject env.pool CreateProject
        { workspaceId = wsA.id, parentId = Nothing, name = "A"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      projB <- createProject env.pool CreateProject
        { workspaceId = wsB.id, parentId = Nothing, name = "B"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      taskA <- createTask env.pool CreateTask
        { workspaceId = wsA.id, projectId = Just projA.id, parentId = Nothing, title = "A"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      taskB <- createTask env.pool CreateTask
        { workspaceId = wsB.id, projectId = Just projB.id, parentId = Nothing, title = "B"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }

      result <- try @DBException $ addDependency env.pool taskA.id taskB.id
      case result of
        Left (DBCheckViolation _) -> pure ()
        other -> expectationFailure $ "Expected DBCheckViolation, got: " <> show other

  describe "memory links" $ do
    it "keeps task memory link creation idempotent" $ \env -> do
      ws <- createTestWorkspace env "taskmem-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Mem"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      task <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Linked"
        , description = Nothing, priority = Nothing, metadata = Nothing
        , dueAt = Nothing }
      mem <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, projectId = Nothing, taskId = Just task.id, content = "linked mem", summary = Nothing
        , memoryType = ShortTerm, importance = Nothing, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      -- Idempotent
      linkTaskMemory env.pool task.id mem.id

    it "soft-deletes and restores task-linked memories with the task" $ \env -> do
      ws <- createTestWorkspace env "taskmem-delete-restore-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Task Memory Project"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      task <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing
        , title = "Task with memory", description = Nothing, priority = Nothing
        , metadata = Nothing, dueAt = Nothing }
      mem <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, projectId = Nothing, taskId = Just task.id, content = "task scoped memory", summary = Nothing
        , memoryType = ShortTerm, importance = Nothing, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }

      deleteTask env.pool task.id `shouldReturn` True
      getTask env.pool task.id >>= (`shouldSatisfy` isNothing)
      getMemory env.pool mem.id >>= (`shouldSatisfy` isNothing)

      restoreTask env.pool task.id `shouldReturn` True
      getTask env.pool task.id >>= (`shouldSatisfy` isJust)
      getMemory env.pool mem.id >>= (`shouldSatisfy` isJust)

    it "preserves task-linked memories while another active target remains" $ \env -> do
      ws <- createTestWorkspace env "taskmem-multiple-delete-restore-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Task Memory Project"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      task1 <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing
        , title = "Task one", description = Nothing, priority = Nothing
        , metadata = Nothing, dueAt = Nothing }
      task2 <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing
        , title = "Task two", description = Nothing, priority = Nothing
        , metadata = Nothing, dueAt = Nothing }
      mem <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, projectId = Nothing, taskId = Just task1.id, content = "shared task memory", summary = Nothing
        , memoryType = ShortTerm, importance = Nothing, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      linkTaskMemory env.pool task2.id mem.id

      deleteTask env.pool task1.id `shouldReturn` True
      getMemory env.pool mem.id >>= (`shouldSatisfy` isJust)

      deleteTask env.pool task2.id `shouldReturn` True
      getMemory env.pool mem.id >>= (`shouldSatisfy` isNothing)

      restoreTask env.pool task1.id `shouldReturn` True
      getMemory env.pool mem.id >>= (`shouldSatisfy` isJust)

    it "filters linked task memories by query, tags, importance, type, and access count" $ \env -> do
      ws <- createTestWorkspace env "taskmem-filter-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Mem"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      task <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Linked"
        , description = Nothing, priority = Nothing, metadata = Nothing
        , dueAt = Nothing }
      matching <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, projectId = Nothing, taskId = Just task.id, content = "beta haskell note", summary = Nothing
        , memoryType = LongTerm, importance = Just 7, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Just ["keep"], ftsLanguage = Nothing }
      _nonMatching <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, projectId = Nothing, taskId = Just task.id, content = "beta haskell note", summary = Nothing
        , memoryType = ShortTerm, importance = Just 2, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Just ["drop"], ftsLanguage = Nothing }
      touchMemory env.pool matching.id
      _ <- updateMemory env.pool matching.id UpdateMemory
        { content = Nothing, summary = Unchanged, memoryType = Nothing, importance = Nothing
        , metadata = Nothing, expiresAt = Unchanged, source = Unchanged, confidence = Nothing, pinned = Just True }
      filtered <- getTaskMemories env.pool task.id LinkedMemoryListQuery
        { query = Just "beta haskell"
        , tags = Just ["keep"]
        , minImportance = Just 5
        , memoryType = Just LongTerm
        , minAccessCount = Just 1
        }
      map (.id) filtered `shouldBe` [matching.id]

  describe "batch update" $ do
    it "batch update with nonexistent ID returns partial success count" $ \env -> do
      ws <- createTestWorkspace env "taskbatchup-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "BatchUp"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      task <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Real"
        , description = Nothing, priority = Nothing, metadata = Nothing
        , dueAt = Nothing }
      let bogusId = read "00000000-0000-0000-0000-ffffffffffff" :: UUID
          upd = UpdateTask
            { title = Just "updated", description = Unchanged, projectId = Unchanged
            , parentId = Unchanged, status = Nothing, priority = Nothing
            , metadata = Nothing, dueAt = Unchanged }
      count <- updateTaskBatch env.pool [(task.id, upd), (bogusId, upd)]
      count `shouldBe` 1

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

projectStatusUpdate :: ProjectStatus -> UpdateProject
projectStatusUpdate statusValue = UpdateProject
  { name = Nothing
  , description = Unchanged
  , parentId = Unchanged
  , status = Just statusValue
  , priority = Nothing
  , metadata = Nothing
  }

whenStatus :: TaskStatus -> (TaskStatus -> IO ()) -> IO ()
whenStatus Todo _ = pure ()
whenStatus statusValue action = action statusValue

expectLifecycle :: Text -> Either DBException a -> Expectation
expectLifecycle expected result = case result of
  Left (DBLifecycleViolation actual _ _ _) -> actual `shouldBe` expected
  Left other -> expectationFailure $ "Expected DBLifecycleViolation " <> show expected <> ", got: " <> show other
  Right _ -> expectationFailure $ "Expected DBLifecycleViolation " <> show expected <> ", got success"

execSql :: TestEnv -> String -> IO ()
execSql env sql = runSession env.pool (Session.sql (BS8.pack sql))

taskAutoBlocked :: TestEnv -> UUID -> IO Bool
taskAutoBlocked env taskId = runSession env.pool $ Session.statement taskId taskAutoBlockedStatement

taskAutoBlockedStatement :: Statement.Statement UUID Bool
taskAutoBlockedStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT auto_blocked FROM tasks WHERE id = $1"
    encoder = E.param (E.nonNullable E.uuid)
    decoder = D.singleRow (D.column (D.nonNullable D.bool))
