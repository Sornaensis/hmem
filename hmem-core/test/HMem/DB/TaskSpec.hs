{-# OPTIONS_GHC -Wno-x-partial -Wno-incomplete-uni-patterns #-}

module HMem.DB.TaskSpec (spec) where

import Control.Exception (try)
import Data.Maybe (isJust, isNothing)
import Test.Hspec

import HMem.DB.Memory (createMemory)
import HMem.DB.Pool (DBException(..))
import HMem.DB.Project (createProject)
import HMem.DB.Task
import HMem.DB.TestHarness
import HMem.Types

import Data.UUID (UUID)

spec :: Spec
spec = around withTestEnv $ do

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

  describe "memory links" $ do
    it "links and unlinks task to memory" $ \env -> do
      ws <- createTestWorkspace env "taskmem-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Mem"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      task <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Linked"
        , description = Nothing, priority = Nothing, metadata = Nothing
        , dueAt = Nothing }
      mem <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "linked mem", summary = Nothing
        , memoryType = ShortTerm, importance = Nothing, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      linkTaskMemory env.pool task.id mem.id
      -- Idempotent
      linkTaskMemory env.pool task.id mem.id
      unlinkTaskMemory env.pool task.id mem.id

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
