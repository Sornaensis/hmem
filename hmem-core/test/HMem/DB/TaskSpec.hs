module HMem.DB.TaskSpec (spec) where

import Control.Exception (try)
import Data.Aeson (Value(..), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Either (isLeft)
import Data.Functor.Contravariant (contramap)
import Data.List (sort)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Data.UUID (UUID)
import Test.Hspec

import HMem.DB.Observation
import HMem.DB.Overview (getTaskOverview, taskCardSummaries)
import HMem.DB.ChangeStream (ChangeScope(..), OutboxRecord(..), listOutboxAfter)
import HMem.DB.Pool (DBException(..), withConn, runSession)
import HMem.DB.Project qualified as Project
import HMem.DB.Task
import HMem.DB.TestHarness
import HMem.Types

spec :: Spec
spec = do
  around withTestEnv $ describe "Subtask rejection lifecycle" $ do
    it "auto-blocks parents while eligible subtasks progress and returns parents to todo" $ \env -> do
      workspace <- createTestWorkspace env "subtask-auto-block"
      project <- createProjectFor env workspace.id "project"
      parent <- createTaskFor env workspace.id (Just project.id) Nothing "parent"
      child <- createTaskFor env workspace.id (Just project.id) (Just parent.id) "child"
      fmap (fmap (.status)) (getTask env.pool parent.id) `shouldReturn` Just Blocked
      expectLifecycleCode "TASK_START_BLOCKED_BY_SUBTASKS" $ updateTask env.pool parent.id (cancelStatusUpdate InProgress)
      candidates <- listNextTasks env.pool project.id False 20
      map (\candidate -> candidate.task.id) candidates `shouldBe` [child.id]
      _ <- updateTask env.pool child.id (cancelStatusUpdate InProgress)
      Just overview <- getTaskOverview env.pool parent.id
      overview.task.status `shouldBe` Blocked
      overview.readinessRollup.inProgressSubtaskCount `shouldBe` 1
      overview.readinessRollup.canStartSubtasks `shouldBe` True
      [card] <- taskCardSummaries env.pool [overview.task]
      card.readinessRollup `shouldBe` overview.readinessRollup
      _ <- updateTask env.pool child.id (cancelStatusUpdate Done)
      fmap (fmap (.status)) (getTask env.pool parent.id) `shouldReturn` Just Todo
      Just closed <- getTaskOverview env.pool parent.id
      closed.readinessRollup.inProgressSubtaskCount `shouldBe` 0
      closed.readinessRollup.completionReady `shouldBe` True
      _ <- updateTask env.pool parent.id (cancelStatusUpdate InProgress)
      fmap (fmap (.status)) (getTask env.pool parent.id) `shouldReturn` Just InProgress

    it "revalidates running-child restoration against manual parent and child prerequisite blockers" $ \env -> do
      workspace <- createTestWorkspace env "subtask-restoration"
      parent <- createTaskFor env workspace.id Nothing Nothing "parent"
      child <- createTaskFor env workspace.id Nothing (Just parent.id) "child"
      _ <- updateTask env.pool child.id (cancelStatusUpdate InProgress)
      _ <- deleteTask env.pool child.id
      _ <- updateTask env.pool parent.id (cancelStatusUpdate Blocked)
      expectLifecycleCode "TASK_SUBTASK_START_BLOCKED" $ restoreTask env.pool child.id
      _ <- updateTask env.pool parent.id (cancelStatusUpdate Todo)
      restoreTask env.pool child.id `shouldReturn` True
      prerequisite <- createTaskFor env workspace.id Nothing Nothing "prerequisite"
      addDependency env.pool child.id prerequisite.id
      fmap (fmap (.status)) (getTask env.pool child.id) `shouldReturn` Just Blocked
      _ <- updateTask env.pool child.id (cancelStatusUpdate InProgress)
      fmap (fmap (.status)) (getTask env.pool child.id) `shouldReturn` Just Blocked

    it "preserves explicit manual blocking after a subtask auto-block, including direct SQL" $ \env -> do
      workspace <- createTestWorkspace env "subtask-manual-conversion"
      let checkManual block = do
            parent <- createTaskFor env workspace.id Nothing Nothing "parent"
            running <- createTaskFor env workspace.id Nothing (Just parent.id) "running"
            waiting <- createTaskFor env workspace.id Nothing (Just parent.id) "waiting"
            _ <- updateTask env.pool running.id (cancelStatusUpdate InProgress)
            block parent.id
            block parent.id -- Repeating the explicit pause must remain manual.
            Just overview <- getTaskOverview env.pool parent.id
            overview.task.status `shouldBe` Blocked
            overview.readinessRollup.canStartSubtasks `shouldBe` False
            fmap (fmap (.status)) (getTask env.pool running.id) `shouldReturn` Just InProgress
            expectLifecycleCode "TASK_SUBTASK_START_BLOCKED" $ updateTask env.pool waiting.id (cancelStatusUpdate InProgress)
            _ <- updateTask env.pool running.id (cancelStatusUpdate Done)
            _ <- updateTask env.pool waiting.id (cancelStatusUpdate Cancelled)
            fmap (fmap (.status)) (getTask env.pool parent.id) `shouldReturn` Just Blocked
      checkManual (\taskId -> updateTask env.pool taskId (cancelStatusUpdate Blocked) >> pure ())
      checkManual (\taskId -> runSession env.pool $ Session.statement taskId blockTaskStatement)

    it "preserves automatic attribution during direct recomputation of legacy running parents" $ \env -> do
      workspace <- createTestWorkspace env "subtask-direct-recompute"
      parent <- createTaskFor env workspace.id Nothing Nothing "parent"
      _ <- createTaskFor env workspace.id Nothing (Just parent.id) "child"
      runSession env.pool $ Session.sql "ALTER TABLE tasks DISABLE TRIGGER USER"
      runSession env.pool $ Session.statement parent.id startTaskStatement
      runSession env.pool $ Session.sql "ALTER TABLE tasks ENABLE TRIGGER USER"
      runSession env.pool $ Session.statement parent.id recomputeTaskStatement
      Just overview <- getTaskOverview env.pool parent.id
      overview.task.status `shouldBe` Blocked
      overview.readinessRollup.canStartSubtasks `shouldBe` True
      _ <- updateTask env.pool parent.id (cancelStatusUpdate Blocked)
      Just manual <- getTaskOverview env.pool parent.id
      manual.readinessRollup.canStartSubtasks `shouldBe` False
  around withTestEnv $ describe "Paused task placement" $ do
    it "reconciles subtask reparenting and cancelled/done reopen through the project blocker" $ \env -> do
      workspace <- createTestWorkspace env "paused-task-reopen"
      project <- createProjectFor env workspace.id "project"
      parent <- createTaskFor env workspace.id (Just project.id) Nothing "parent"
      child <- createTaskFor env workspace.id (Just project.id) Nothing "child"
      closed <- createTaskFor env workspace.id (Just project.id) Nothing "closed"
      _ <- updateTask env.pool closed.id (cancelStatusUpdate Cancelled)
      _ <- Project.updateProject env.pool project.id (projectStatusUpdate ProjPaused)
      _ <- updateTask env.pool child.id (UpdateTask Nothing Unchanged Unchanged (SetTo parent.id) Nothing Nothing Nothing Unchanged)
      _ <- updateTask env.pool closed.id (cancelStatusUpdate Todo)
      mapM (fmap (fmap (.status)) . getTask env.pool) [child.id, closed.id] `shouldReturn` replicate 2 (Just Blocked)
      Just paused <- getTaskOverview env.pool parent.id
      paused.readinessRollup.canStartSubtasks `shouldBe` False
      _ <- updateTask env.pool child.id (cancelStatusUpdate Done)
      _ <- updateTask env.pool child.id (cancelStatusUpdate Todo)
      fmap (fmap (.status)) (getTask env.pool child.id) `shouldReturn` Just Blocked
      _ <- Project.updateProject env.pool project.id (projectStatusUpdate ProjActive)
      mapM (fmap (fmap (.status)) . getTask env.pool) [parent.id, child.id, closed.id]
        `shouldReturn` [Just Blocked, Just Todo, Just Todo]

    it "preserves a repeated explicit manual block established during inherited automatic blocking" $ \env -> do
      workspace <- createTestWorkspace env "paused-task-manual"
      project <- createProjectFor env workspace.id "project"
      task <- createTaskFor env workspace.id (Just project.id) Nothing "task"
      _ <- Project.updateProject env.pool project.id (projectStatusUpdate ProjPaused)
      _ <- updateTask env.pool task.id (cancelStatusUpdate Blocked)
      _ <- Project.updateProject env.pool project.id (projectStatusUpdate ProjActive)
      fmap (fmap (.status)) (getTask env.pool task.id) `shouldReturn` Just Blocked
      _ <- updateTask env.pool task.id (cancelStatusUpdate Todo)
      fmap (fmap (.status)) (getTask env.pool task.id) `shouldReturn` Just Todo

    it "rejects project-subtree placement and full project restore with running subtasks atomically" $ \env -> do
      workspace <- createTestWorkspace env "paused-project-placement"
      paused <- createProjectFor env workspace.id "paused"
      moving <- createProjectFor env workspace.id "moving"
      parent <- createTaskFor env workspace.id (Just moving.id) Nothing "parent"
      child <- createTaskFor env workspace.id (Just moving.id) (Just parent.id) "running child"
      _ <- updateTask env.pool child.id (cancelStatusUpdate InProgress)
      _ <- Project.updateProject env.pool paused.id (projectStatusUpdate ProjPaused)
      let move = UpdateProject Nothing Unchanged (SetTo paused.id) Nothing Nothing Nothing
      expectLifecycleCode "PROJECT_PAUSE_BLOCKED_BY_RUNNING_TASKS" $ Project.updateProject env.pool moving.id move
      fmap (fmap (.parentId)) (Project.getProject env.pool moving.id) `shouldReturn` Just Nothing
      _ <- Project.deleteProject env.pool moving.id
      runSession env.pool $ Session.statement (moving.id, paused.id) pausedDeletedProjectMoveStatement
      expectLifecycleCode "TASK_RUNNING_UNDER_PAUSED_PROJECT" $ Project.restoreProject env.pool moving.id
      Project.getProject env.pool moving.id `shouldReturn` Nothing
      getTask env.pool child.id `shouldReturn` Nothing

  taskLifecycleSpec

taskLifecycleSpec :: Spec
taskLifecycleSpec = beforeAll setupTestPool $ aroundWith withTestTransaction $
  describe "Task lifecycle" $ do
    it "keeps manual and prerequisite blocked parents ineligible without pausing running children" $ \env -> do
      workspace <- createTestWorkspace env "subtask-parent-blockers"
      manual <- createTaskFor env workspace.id Nothing Nothing "manual"
      _ <- updateTask env.pool manual.id (cancelStatusUpdate Blocked)
      manualChild <- createTaskFor env workspace.id Nothing (Just manual.id) "manual child"
      expectLifecycleCode "TASK_SUBTASK_START_BLOCKED" $ updateTask env.pool manualChild.id (cancelStatusUpdate InProgress)
      _ <- updateTask env.pool manualChild.id (cancelStatusUpdate Done)
      fmap (fmap (.status)) (getTask env.pool manual.id) `shouldReturn` Just Blocked
      parent <- createTaskFor env workspace.id Nothing Nothing "parent"
      first <- createTaskFor env workspace.id Nothing (Just parent.id) "first"
      second <- createTaskFor env workspace.id Nothing (Just parent.id) "second"
      _ <- updateTask env.pool first.id (cancelStatusUpdate InProgress)
      prerequisite <- createTaskFor env workspace.id Nothing Nothing "prerequisite"
      addDependency env.pool parent.id prerequisite.id
      expectLifecycleCode "TASK_SUBTASK_START_BLOCKED" $ updateTask env.pool second.id (cancelStatusUpdate InProgress)
      fmap (fmap (.status)) (getTask env.pool first.id) `shouldReturn` Just InProgress
      Just blocked <- getTaskOverview env.pool parent.id
      blocked.readinessRollup.canStartSubtasks `shouldBe` False
      _ <- updateTask env.pool first.id (cancelStatusUpdate Done)
      _ <- updateTask env.pool second.id (cancelStatusUpdate Cancelled)
      fmap (fmap (.status)) (getTask env.pool parent.id) `shouldReturn` Just Blocked
      _ <- updateTask env.pool prerequisite.id (cancelStatusUpdate Done)
      fmap (fmap (.status)) (getTask env.pool parent.id) `shouldReturn` Just Todo

    it "recomputes both parents on running-child placement, reopening and deletion" $ \env -> do
      workspace <- createTestWorkspace env "subtask-placement"
      first <- createTaskFor env workspace.id Nothing Nothing "first"
      second <- createTaskFor env workspace.id Nothing Nothing "second"
      child <- createTaskFor env workspace.id Nothing (Just first.id) "child"
      _ <- updateTask env.pool child.id (cancelStatusUpdate InProgress)
      _ <- updateTask env.pool child.id ((cancelStatusUpdate InProgress) { parentId = SetTo second.id })
      mapM (fmap (fmap (.status)) . getTask env.pool) [first.id, second.id] `shouldReturn` [Just Todo, Just Blocked]
      _ <- updateTask env.pool child.id (cancelStatusUpdate Done)
      fmap (fmap (.status)) (getTask env.pool second.id) `shouldReturn` Just Todo
      _ <- updateTask env.pool child.id (cancelStatusUpdate InProgress)
      fmap (fmap (.status)) (getTask env.pool second.id) `shouldReturn` Just Blocked
      _ <- deleteTask env.pool child.id
      fmap (fmap (.status)) (getTask env.pool second.id) `shouldReturn` Just Todo
      restoreTask env.pool child.id `shouldReturn` True
      fmap (fmap (.status)) (getTask env.pool second.id) `shouldReturn` Just Blocked
      _ <- updateTask env.pool child.id ((cancelStatusUpdate InProgress) { parentId = SetNull })
      fmap (fmap (.status)) (getTask env.pool second.id) `shouldReturn` Just Todo
      _ <- updateTask env.pool second.id (cancelStatusUpdate Blocked)
      expectLifecycleCode "TASK_SUBTASK_START_BLOCKED" $
        updateTask env.pool child.id ((cancelStatusUpdate InProgress) { parentId = SetTo second.id })

    it "sees same-statement inserts and transitions through the authoritative child eligibility predicate" $ \env -> do
      workspace <- createTestWorkspace env "subtask-same-statement"
      runSession env.pool $ Session.statement workspace.id insertRunningChildWithParentStatement
      parent <- createTaskFor env workspace.id Nothing Nothing "parent"
      child <- createTaskFor env workspace.id Nothing (Just parent.id) "child"
      runSession env.pool $ Session.statement (parent.id, child.id) finishChildAndStartParentStatement
      mapM (fmap (fmap (.status)) . getTask env.pool) [parent.id, child.id] `shouldReturn` [Just InProgress, Just Done]
      runSession env.pool $ Session.statement (parent.id, child.id) reopenChildAndResetParentStatement
      mapM (fmap (fmap (.status)) . getTask env.pool) [parent.id, child.id] `shouldReturn` [Just Blocked, Just InProgress]

    it "rejects direct SQL parent starts atomically and normalizes metadata-only legacy running parents" $ \env -> do
      workspace <- createTestWorkspace env "subtask-raw-start"
      parent <- createTaskFor env workspace.id Nothing Nothing "parent"
      child <- createTaskFor env workspace.id Nothing (Just parent.id) "child"
      before <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 50
      result <- withConn env.pool $ \connection -> do
        ensureSession "savepoint" =<< Session.run (Session.sql "SAVEPOINT parent_start") connection
        attempted <- Session.run (Session.statement parent.id startTaskStatement) connection
        ensureSession "rollback" =<< Session.run (Session.sql "ROLLBACK TO SAVEPOINT parent_start") connection
        ensureSession "release" =<< Session.run (Session.sql "RELEASE SAVEPOINT parent_start") connection
        pure attempted
      result `shouldSatisfy` isLeft
      listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 50 `shouldReturn` before
      runSession env.pool $ Session.sql "ALTER TABLE tasks DISABLE TRIGGER USER"
      runSession env.pool $ Session.statement parent.id startTaskStatement
      runSession env.pool $ Session.sql "ALTER TABLE tasks ENABLE TRIGGER USER"
      _ <- updateTask env.pool parent.id ((cancelStatusUpdate Todo) { status = Nothing, title = Just "renamed" })
      fmap (fmap (.status)) (getTask env.pool parent.id) `shouldReturn` Just Blocked
      _ <- updateTask env.pool child.id (cancelStatusUpdate InProgress)
      fmap (fmap (.status)) (getTask env.pool child.id) `shouldReturn` Just InProgress

    it "cascade cancellation closes unfinished subtasks and preserves done rows and dependency edges" $ \env -> do
      workspace <- createTestWorkspace env "task-cascade-cancel"
      parent <- createTaskFor env workspace.id Nothing Nothing "parent"
      child <- createTaskFor env workspace.id Nothing (Just parent.id) "unfinished"
      doneChild <- createTaskFor env workspace.id Nothing (Just parent.id) "done"
      deletedChild <- createTaskFor env workspace.id Nothing (Just parent.id) "deleted"
      unrelated <- createTaskFor env workspace.id Nothing Nothing "unrelated"
      _ <- updateTask env.pool doneChild.id (cancelStatusUpdate Done)
      _ <- deleteTask env.pool deletedChild.id
      beforeDeleted <- runSession env.pool $ Session.statement deletedChild.id taskRowForCascadeStatement
      beforeDone <- getTask env.pool doneChild.id
      addDependency env.pool unrelated.id child.id
      _ <- updateTask env.pool parent.id (cancelStatusUpdate Cancelled)
      mapM (fmap (fmap (.status)) . getTask env.pool) [parent.id, child.id, unrelated.id]
        `shouldReturn` [Just Cancelled, Just Cancelled, Just Todo]
      getTask env.pool doneChild.id `shouldReturn` beforeDone
      runSession env.pool (Session.statement deletedChild.id taskRowForCascadeStatement) `shouldReturn` beforeDeleted
      dependencyIds env unrelated.id `shouldReturn` [child.id]
      _ <- updateTask env.pool parent.id (cancelStatusUpdate Cancelled)
      getTask env.pool doneChild.id `shouldReturn` beforeDone

    it "cascade cancellation shares direct SQL and batch semantics while explicit done roots remain cancellable" $ \env -> do
      workspace <- createTestWorkspace env "task-cascade-writers"
      first <- createTaskFor env workspace.id Nothing Nothing "first"
      firstChild <- createTaskFor env workspace.id Nothing (Just first.id) "first child"
      second <- createTaskFor env workspace.id Nothing Nothing "second"
      secondChild <- createTaskFor env workspace.id Nothing (Just second.id) "second child"
      runSession env.pool $ Session.statement first.id cancelTaskStatement
      fmap (fmap (.status)) (getTask env.pool firstChild.id) `shouldReturn` Just Cancelled
      updateTaskBatch env.pool [(second.id, cancelStatusUpdate Cancelled)] `shouldReturn` 1
      fmap (fmap (.status)) (getTask env.pool secondChild.id) `shouldReturn` Just Cancelled
      finished <- createTaskFor env workspace.id Nothing Nothing "explicit done root"
      _ <- updateTask env.pool finished.id (cancelStatusUpdate Done)
      _ <- updateTask env.pool finished.id (cancelStatusUpdate Cancelled)
      fmap (fmap (.status)) (getTask env.pool finished.id) `shouldReturn` Just Cancelled

    it "cascade cancellation requires reopening the parent before creating or reopening unfinished children" $ \env -> do
      workspace <- createTestWorkspace env "task-cascade-closed-parent"
      parent <- createTaskFor env workspace.id Nothing Nothing "parent"
      child <- createTaskFor env workspace.id Nothing (Just parent.id) "child"
      _ <- updateTask env.pool parent.id (cancelStatusUpdate Cancelled)
      created <- try (createTaskFor env workspace.id Nothing (Just parent.id) "late child")
      created `shouldSatisfy` (isLeft :: Either DBException Task -> Bool)
      reopened <- try (updateTask env.pool child.id (cancelStatusUpdate Todo))
      reopened `shouldSatisfy` (isLeft :: Either DBException (Maybe Task) -> Bool)
      started <- try (updateTask env.pool child.id (cancelStatusUpdate InProgress))
      started `shouldSatisfy` (\case Left (DBLifecycleViolation "TASK_OPEN_UNDER_CANCELLED_TASK" _ _ _) -> True; _ -> False)
      moving <- createTaskFor env workspace.id Nothing Nothing "moving"
      moved <- try (updateTask env.pool moving.id ((cancelStatusUpdate Todo) { parentId = SetTo parent.id }))
      moved `shouldSatisfy` (\case Left (DBLifecycleViolation "TASK_OPEN_UNDER_CANCELLED_TASK" _ _ _) -> True; _ -> False)
      _ <- updateTask env.pool parent.id (cancelStatusUpdate Todo)
      fmap (fmap (.status)) (getTask env.pool child.id) `shouldReturn` Just Cancelled
      _ <- updateTask env.pool child.id (cancelStatusUpdate Todo)
      fmap (fmap (.status)) (getTask env.pool child.id) `shouldReturn` Just Todo
    it "does not mutate independent observations" $ \env -> do
      workspace <- createTestWorkspace env "task-observation-isolation"
      observation <- createObservation env.pool (CreateObservation workspace.id [ObservationSubject SubjectFile "src/Task.hs"] "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "independent")
      task <- createTask env.pool CreateTask
        { workspaceId = workspace.id, projectId = Nothing, parentId = Nothing, title = "task", description = Nothing
        , priority = Nothing, metadata = Nothing, dueAt = Nothing }
      deleteTaskCascade env.pool task.id >>= (`shouldSatisfy` isJust)
      getObservation env.pool workspace.id observation.id >>= (`shouldSatisfy` isJust)
    it "rejects a multi-hop dependency cycle atomically" $ \env -> do
      workspace <- createTestWorkspace env "task-dependency-cycle"
      let create title = createTask env.pool CreateTask
            { workspaceId = workspace.id, projectId = Nothing, parentId = Nothing, title = title, description = Nothing
            , priority = Nothing, metadata = Nothing, dueAt = Nothing }
      taskA <- create "A"
      taskB <- create "B"
      taskC <- create "C"
      addDependency env.pool taskA.id taskB.id
      addDependency env.pool taskB.id taskC.id
      before <- mapM (getTaskOverview env.pool) [taskA.id, taskB.id, taskC.id]
      result <- withConn env.pool $ \connection -> do
        ensureSession "create cycle savepoint" =<< Session.run (Session.sql "SAVEPOINT dependency_cycle") connection
        attempted <- Session.run (Session.statement (taskC.id, taskA.id) insertDependencyDirect) connection
        ensureSession "rollback cycle savepoint" =<< Session.run (Session.sql "ROLLBACK TO SAVEPOINT dependency_cycle") connection
        ensureSession "release cycle savepoint" =<< Session.run (Session.sql "RELEASE SAVEPOINT dependency_cycle") connection
        pure attempted
      result `shouldSatisfy` isLeft
      after <- mapM (getTaskOverview env.pool) [taskA.id, taskB.id, taskC.id]
      after `shouldBe` before

    it "moves an eight-task dependency DAG atomically and preserves edges, state, and change-stream transaction metadata" $ \env -> do
      workspace <- createTestWorkspace env "task-batch-move-dag"
      source <- createProjectFor env workspace.id "source"
      destination <- createProjectFor env workspace.id "destination"
      tasks <- mapM (createTaskFor env workspace.id (Just source.id) Nothing . ("task-" <>) . showText) [1 .. 8 :: Int]
      let ids = map (.id) tasks
          edgeIndexes =
            [ (0, 1), (0, 2), (0, 3), (1, 2), (1, 3), (1, 4)
            , (2, 3), (2, 4), (2, 5), (3, 4), (3, 5), (3, 6)
            , (4, 5), (4, 6), (5, 6), (5, 7), (6, 7)
            ]
      mapM_ (\(taskIndex, dependencyIndex) -> addDependency env.pool (ids !! taskIndex) (ids !! dependencyIndex)) edgeIndexes
      beforeEdges <- mapM (dependencyIds env) ids
      beforeState <- mapM (taskState env) ids
      existing <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100
      let beforeCursor = maximum (0 : map (.outboxCursor) existing)
      moveTasksBatch env.pool ids (Just destination.id) `shouldReturn` 8
      afterEdges <- mapM (dependencyIds env) ids
      afterState <- mapM (taskState env) ids
      beforeEdges `shouldBe` afterEdges
      map (\(taskId, taskStatus, taskParent, _) -> (taskId, taskStatus, taskParent)) afterState
        `shouldBe` map (\(taskId, taskStatus, taskParent, _) -> (taskId, taskStatus, taskParent)) beforeState
      map (\(_, _, _, projectId) -> projectId) afterState `shouldBe` replicate 8 (Just destination.id)
      records <- listOutboxAfter env.pool (WorkspaceScope workspace.id) beforeCursor 20
      length records `shouldBe` 8
      let transactionIds = map outboxTransactionId records
          entities = map (outboxField "entity") records
      transactionIds `shouldSatisfy` \case
        first : rest -> first /= Nothing && all (== first) rest
        [] -> False
      sort [entityId | Just (Object fields) <- entities, Just (String entityId) <- [KeyMap.lookup "id" fields]]
        `shouldBe` sort (map showText ids)
      mapM_ (\record -> do
        (outboxField "entity" record >>= objectField "type") `shouldBe` Just (String "task")
        (outboxField "entity" record >>= objectField "action") `shouldBe` Just (String "updated")
        (outboxField "transaction" record >>= objectField "cause") `shouldBe` Just (String "core")
        (outboxField "transaction" record >>= objectField "request_id") `shouldBe` Just Null) records
      mapM_ (\record -> do
        outboxInvalidations record `shouldSatisfy` containsInvalidation "readiness" ("project:" <> showText source.id)
        outboxInvalidations record `shouldSatisfy` containsInvalidation "readiness" ("project:" <> showText destination.id)) records

    it "rejects an incomplete dependency component with no partial project mutation" $ \env -> do
      workspace <- createTestWorkspace env "task-batch-move-incomplete"
      source <- createProjectFor env workspace.id "source"
      destination <- createProjectFor env workspace.id "destination"
      dependent <- createTaskFor env workspace.id (Just source.id) Nothing "dependent"
      prerequisite <- createTaskFor env workspace.id (Just source.id) Nothing "prerequisite"
      addDependency env.pool dependent.id prerequisite.id
      expectLifecycleCode "TASK_DEPENDENCY_CROSS_PROJECT" $
        moveTasksBatch env.pool [dependent.id] (Just destination.id)
      mapM (taskProject env) [dependent.id, prerequisite.id]
        `shouldReturn` [Just source.id, Just source.id]
      dependencyIds env dependent.id `shouldReturn` [prerequisite.id]

    it "expands requested task roots to active descendants and rejects separating a child from its external parent" $ \env -> do
      workspace <- createTestWorkspace env "task-batch-move-subtree"
      source <- createProjectFor env workspace.id "source"
      destination <- createProjectFor env workspace.id "destination"
      parent <- createTaskFor env workspace.id (Just source.id) Nothing "parent"
      child <- createTaskFor env workspace.id (Just source.id) (Just parent.id) "child"
      expectLifecycleCode "TASK_PARENT_PROJECT_MISMATCH" $
        moveTasksBatch env.pool [child.id] (Just destination.id)
      mapM (taskProject env) [parent.id, child.id] `shouldReturn` [Just source.id, Just source.id]
      moveTasksBatch env.pool [parent.id, parent.id] (Just destination.id) `shouldReturn` 2
      mapM (taskProject env) [parent.id, child.id] `shouldReturn` [Just destination.id, Just destination.id]
      fmap (fmap (.parentId)) (getTask env.pool child.id) `shouldReturn` Just (Just parent.id)

    it "accepts a paused destination and rejects cross-workspace sources and a closed destination" $ \env -> do
      workspace <- createTestWorkspace env "task-batch-move-paused"
      otherWorkspace <- createTestWorkspace env "task-batch-move-other"
      source <- createProjectFor env workspace.id "source"
      paused <- createProjectFor env workspace.id "paused"
      closed <- createProjectFor env workspace.id "closed"
      _ <- Project.updateProject env.pool paused.id (projectStatusUpdate ProjPaused)
      _ <- Project.updateProject env.pool closed.id (projectStatusUpdate ProjCompleted)
      task <- createTaskFor env workspace.id (Just source.id) Nothing "task"
      otherTask <- createTaskFor env otherWorkspace.id Nothing Nothing "foreign"
      moveTasksBatch env.pool [task.id] (Just paused.id) `shouldReturn` 1
      taskProject env task.id `shouldReturn` Just paused.id
      expectLifecycleCode "TASK_BATCH_MOVE_CROSS_WORKSPACE" $
        moveTasksBatch env.pool [task.id, otherTask.id] Nothing
      expectLifecycleCode "TASK_OPEN_UNDER_CLOSED_PROJECT" $
        moveTasksBatch env.pool [task.id] (Just closed.id)

createProjectFor :: TestEnv -> UUID -> Text -> IO Project
createProjectFor env workspaceId name = Project.createProject env.pool CreateProject
  { workspaceId = workspaceId, parentId = Nothing, name = name, description = Nothing
  , priority = Nothing, metadata = Nothing }

cancelStatusUpdate :: TaskStatus -> UpdateTask
cancelStatusUpdate status = UpdateTask
  { title = Nothing, description = Unchanged, projectId = Unchanged, parentId = Unchanged
  , status = Just status, priority = Nothing, metadata = Nothing, dueAt = Unchanged }

cancelTaskStatement :: Statement.Statement UUID ()
cancelTaskStatement = Statement.Statement
  "UPDATE tasks SET status = 'cancelled' WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

startTaskStatement :: Statement.Statement UUID ()
startTaskStatement = Statement.Statement
  "UPDATE tasks SET status = 'in_progress' WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

pausedDeletedProjectMoveStatement :: Statement.Statement (UUID, UUID) ()
pausedDeletedProjectMoveStatement = Statement.Statement "UPDATE projects SET parent_id = $2 WHERE id = $1"
  (contramap fst (Enc.param (Enc.nonNullable Enc.uuid)) <> contramap snd (Enc.param (Enc.nonNullable Enc.uuid))) Dec.noResult True

blockTaskStatement :: Statement.Statement UUID ()
blockTaskStatement = Statement.Statement
  "UPDATE tasks SET status = 'blocked' WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

recomputeTaskStatement :: Statement.Statement UUID ()
recomputeTaskStatement = Statement.Statement
  "SELECT hmem_recompute_task_auto_blocking(ARRAY[$1]::uuid[])"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

insertRunningChildWithParentStatement :: Statement.Statement UUID ()
insertRunningChildWithParentStatement = Statement.Statement
  "WITH ids AS (SELECT gen_random_uuid() AS parent_id, gen_random_uuid() AS child_id) INSERT INTO tasks (id,workspace_id,parent_id,title,status) SELECT parent_id,$1,NULL,'same parent','todo'::task_status_enum FROM ids UNION ALL SELECT child_id,$1,parent_id,'same child','in_progress'::task_status_enum FROM ids"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

finishChildAndStartParentStatement :: Statement.Statement (UUID, UUID) ()
finishChildAndStartParentStatement = Statement.Statement
  "UPDATE tasks SET status=CASE WHEN id=$1 THEN 'in_progress'::task_status_enum ELSE 'done'::task_status_enum END WHERE id IN ($1,$2)"
  (contramap fst (Enc.param (Enc.nonNullable Enc.uuid)) <> contramap snd (Enc.param (Enc.nonNullable Enc.uuid))) Dec.noResult True

reopenChildAndResetParentStatement :: Statement.Statement (UUID, UUID) ()
reopenChildAndResetParentStatement = Statement.Statement
  "UPDATE tasks SET status=CASE WHEN id=$1 THEN 'todo'::task_status_enum ELSE 'in_progress'::task_status_enum END WHERE id IN ($1,$2)"
  (contramap fst (Enc.param (Enc.nonNullable Enc.uuid)) <> contramap snd (Enc.param (Enc.nonNullable Enc.uuid))) Dec.noResult True

taskRowForCascadeStatement :: Statement.Statement UUID Value
taskRowForCascadeStatement = Statement.Statement
  "SELECT to_jsonb(t) FROM tasks t WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid))
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.jsonb))) True

createTaskFor :: TestEnv -> UUID -> Maybe UUID -> Maybe UUID -> Text -> IO Task
createTaskFor env workspaceId projectId parentId title = createTask env.pool CreateTask
  { workspaceId = workspaceId, projectId = projectId, parentId = parentId, title = title
  , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }

projectStatusUpdate :: ProjectStatus -> UpdateProject
projectStatusUpdate status = UpdateProject
  { name = Nothing, description = Unchanged, parentId = Unchanged, status = Just status
  , priority = Nothing, metadata = Nothing }

taskProject :: TestEnv -> UUID -> IO (Maybe UUID)
taskProject env taskId = fmap (.projectId) <$> getTask env.pool taskId >>= maybe (fail "expected task") pure

taskState :: TestEnv -> UUID -> IO (UUID, TaskStatus, Maybe UUID, Maybe UUID)
taskState env taskId = getTask env.pool taskId >>= maybe (fail "expected task") (\task -> pure (task.id, task.status, task.parentId, task.projectId))

dependencyIds :: TestEnv -> UUID -> IO [UUID]
dependencyIds env taskId = getTaskOverview env.pool taskId >>= maybe (fail "expected task overview") (pure . map (.id) . (.dependencies))

expectLifecycleCode :: Show a => Text -> IO a -> Expectation
expectLifecycleCode expected action = do
  result <- try action
  case result of
    Left (DBLifecycleViolation code _ _ _) -> code `shouldBe` expected
    Left other -> expectationFailure ("expected lifecycle violation, got " <> show other)
    Right value -> expectationFailure ("expected lifecycle violation, got " <> show value)

outboxField :: Text -> OutboxRecord -> Maybe Value
outboxField field record = case record.outboxEnvelope of
  Object fields -> KeyMap.lookup (Key.fromText field) fields
  _ -> Nothing

objectField :: Text -> Value -> Maybe Value
objectField field (Object fields) = KeyMap.lookup (Key.fromText field) fields
objectField _ _ = Nothing

outboxTransactionId :: OutboxRecord -> Maybe Value
outboxTransactionId record = outboxField "transaction" record >>= \case
  Object fields -> KeyMap.lookup "id" fields
  _ -> Nothing

outboxInvalidations :: OutboxRecord -> Value
outboxInvalidations record = maybe Null id (outboxField "invalidations" record)

containsInvalidation :: Text -> Text -> Value -> Bool
containsInvalidation kind target (Array values) = object ["kind" .= kind, "target" .= target] `elem` values
containsInvalidation _ _ _ = False

showText :: Show a => a -> Text
showText = T.pack . show

insertDependencyDirect :: Statement.Statement (UUID, UUID) ()
insertDependencyDirect = Statement.Statement
  "INSERT INTO task_dependencies (task_id, depends_on_id) VALUES ($1, $2)"
  (contramap fst (Enc.param (Enc.nonNullable Enc.uuid)) <> contramap snd (Enc.param (Enc.nonNullable Enc.uuid)))
  Dec.noResult
  True

ensureSession :: String -> Either a () -> IO ()
ensureSession _ (Right ()) = pure ()
ensureSession label (Left _) = fail (label <> " failed")
