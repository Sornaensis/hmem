module HMem.DB.ProjectSpec (spec) where

import Data.Aeson (Value(..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Maybe (isJust)
import Data.Functor.Contravariant (contramap)
import Control.Exception (bracket_, try)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Test.Hspec

import HMem.DB.ChangeStream
import HMem.DB.Observation
import HMem.DB.Overview (getTaskOverview)
import HMem.DB.Pool (runSession, DBException(..))
import HMem.DB.Project
import HMem.DB.Task qualified as Task
import HMem.DB.TestHarness
import HMem.DB.WorkspaceGroup qualified as WorkspaceGroup
import HMem.Types

spec :: Spec
spec = do
  around withTestEnv $ describe "Project pause lifecycle" $ do
    it "pauses deep open descendants and hierarchy-only subtasks while preserving closed and terminal rows" $ \env -> do
      workspace <- createTestWorkspace env "pause-deep"
      root <- pauseProject env workspace.id Nothing "root"
      child <- pauseProject env workspace.id (Just root.id) "child"
      grandchild <- pauseProject env workspace.id (Just child.id) "grandchild"
      closed <- pauseProject env workspace.id (Just child.id) "closed"
      _ <- updateProject env.pool closed.id (archiveStatusUpdate ProjCompleted)
      parent <- pauseTask env workspace.id (Just grandchild.id) Nothing "parent"
      subtask <- runSession env.pool $ Session.statement (workspace.id, parent.id) unassignedArchiveChildStatement
      doneTask <- pauseTask env workspace.id (Just child.id) Nothing "done"
      cancelled <- pauseTask env workspace.id (Just child.id) Nothing "cancelled"
      _ <- Task.updateTask env.pool doneTask.id (pauseTaskStatus Done)
      _ <- Task.updateTask env.pool cancelled.id (pauseTaskStatus Cancelled)
      beforeDone <- Task.getTask env.pool doneTask.id
      beforeCancelled <- Task.getTask env.pool cancelled.id
      beforeClosed <- getProject env.pool closed.id
      updated <- updateProject env.pool root.id (archiveStatusUpdate ProjPaused)
      fmap (.status) updated `shouldBe` Just ProjPaused
      mapM (fmap (fmap (.status)) . getProject env.pool) [root.id, child.id, grandchild.id]
        `shouldReturn` replicate 3 (Just ProjPaused)
      mapM (fmap (fmap (.status)) . Task.getTask env.pool) [parent.id, subtask]
        `shouldReturn` replicate 2 (Just Blocked)
      Just overview <- getTaskOverview env.pool parent.id
      overview.readinessRollup.canStartSubtasks `shouldBe` False
      Task.getTask env.pool doneTask.id `shouldReturn` beforeDone
      Task.getTask env.pool cancelled.id `shouldReturn` beforeCancelled
      getProject env.pool closed.id `shouldReturn` beforeClosed
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjActive)
      fmap (fmap (.status)) (Task.getTask env.pool subtask) `shouldReturn` Just Todo
      fmap (fmap (.status)) (Task.getTask env.pool parent.id) `shouldReturn` Just Blocked
      Task.getTask env.pool doneTask.id `shouldReturn` beforeDone
      Task.getTask env.pool cancelled.id `shouldReturn` beforeCancelled

    it "preserves frozen historical project and task branches while refusing their running descendants" $ \env -> do
      workspace <- createTestWorkspace env "pause-frozen-history"
      root <- pauseProject env workspace.id Nothing "root"
      closed <- pauseProject env workspace.id (Just root.id) "closed branch"
      historical <- pauseProject env workspace.id (Just closed.id) "historical open grandchild"
      waiting <- pauseTask env workspace.id (Just historical.id) Nothing "frozen waiting"
      terminalParent <- pauseTask env workspace.id (Just root.id) Nothing "terminal parent"
      child <- pauseTask env workspace.id (Just root.id) (Just terminalParent.id) "historical open child"
      bracket_
        (runSession env.pool $ Session.sql "ALTER TABLE projects DISABLE TRIGGER USER; ALTER TABLE tasks DISABLE TRIGGER USER")
        (runSession env.pool $ Session.sql "ALTER TABLE projects ENABLE TRIGGER USER; ALTER TABLE tasks ENABLE TRIGGER USER") $ do
          _ <- updateProject env.pool closed.id (archiveStatusUpdate ProjCompleted)
          runSession env.pool $ Session.statement terminalParent.id freezePauseTaskStatement
      projectsBefore <- mapM (getProject env.pool) [closed.id, historical.id]
      tasksBefore <- mapM (Task.getTask env.pool) [waiting.id, terminalParent.id, child.id]
      auditsBefore <- mapM (getAuditLogRows env.pool "project" . T.pack . show) [closed.id, historical.id]
      taskAuditsBefore <- mapM (getAuditLogRows env.pool "task" . T.pack . show) [waiting.id, terminalParent.id, child.id]
      let preserve = do
            mapM (getProject env.pool) [closed.id, historical.id] `shouldReturn` projectsBefore
            mapM (Task.getTask env.pool) [waiting.id, terminalParent.id, child.id] `shouldReturn` tasksBefore
            mapM (getAuditLogRows env.pool "project" . T.pack . show) [closed.id, historical.id] `shouldReturn` auditsBefore
            mapM (getAuditLogRows env.pool "task" . T.pack . show) [waiting.id, terminalParent.id, child.id] `shouldReturn` taskAuditsBefore
      before <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjPaused)
      preserve
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjActive)
      preserve
      after <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100
      length after `shouldBe` length before + 2 -- only the explicit root pause/resume
      bracket_
        (runSession env.pool $ Session.sql "ALTER TABLE tasks DISABLE TRIGGER USER")
        (runSession env.pool $ Session.sql "ALTER TABLE tasks ENABLE TRIGGER USER") $
        runSession env.pool $ Session.statement waiting.id startFrozenPauseTaskStatement
      refused <- try @DBException $ updateProject env.pool root.id (archiveStatusUpdate ProjPaused)
      refused `shouldSatisfy` \case Left (DBLifecycleViolation "PROJECT_PAUSE_BLOCKED_BY_RUNNING_TASKS" _ _ _) -> True; _ -> False
      fmap (fmap (.status)) (getProject env.pool root.id) `shouldReturn` Just ProjActive
      listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100 `shouldReturn` after

    it "retains independent child pauses and manual/dependency/subtask blockers on resume" $ \env -> do
      workspace <- createTestWorkspace env "pause-causes"
      root <- pauseProject env workspace.id Nothing "root"
      own <- pauseProject env workspace.id (Just root.id) "own"
      inherited <- pauseProject env workspace.id (Just root.id) "inherited"
      ready <- pauseTask env workspace.id (Just inherited.id) Nothing "ready"
      manual <- pauseTask env workspace.id (Just inherited.id) Nothing "manual"
      dependent <- pauseTask env workspace.id (Just inherited.id) Nothing "dependent"
      prerequisite <- pauseTask env workspace.id (Just inherited.id) Nothing "prerequisite"
      ownTask <- pauseTask env workspace.id (Just own.id) Nothing "own task"
      _ <- Task.updateTask env.pool manual.id (pauseTaskStatus Blocked)
      Task.addDependency env.pool dependent.id prerequisite.id
      _ <- updateProject env.pool own.id (archiveStatusUpdate ProjPaused)
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjPaused)
      -- Metadata-only updates of an inherited paused project must not own it.
      _ <- updateProject env.pool inherited.id (UpdateProject (Just "renamed") Unchanged Unchanged Nothing Nothing Nothing)
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjActive)
      mapM (fmap (fmap (.status)) . getProject env.pool) [root.id, inherited.id, own.id]
        `shouldReturn` [Just ProjActive, Just ProjActive, Just ProjPaused]
      mapM (fmap (fmap (.status)) . Task.getTask env.pool) [ready.id, manual.id, dependent.id, ownTask.id]
        `shouldReturn` [Just Todo, Just Blocked, Just Blocked, Just Blocked]
      _ <- updateProject env.pool own.id (archiveStatusUpdate ProjActive)
      fmap (fmap (.status)) (Task.getTask env.pool ownTask.id) `shouldReturn` Just Todo
      _ <- Task.updateTask env.pool prerequisite.id (pauseTaskStatus Done)
      fmap (fmap (.status)) (Task.getTask env.pool dependent.id) `shouldReturn` Just Todo
      fmap (fmap (.status)) (Task.getTask env.pool manual.id) `shouldReturn` Just Blocked

    it "honors repeated own intent, overlapping SQL targets and remaining paused ancestry" $ \env -> do
      workspace <- createTestWorkspace env "pause-overlap"
      root <- pauseProject env workspace.id Nothing "root"
      child <- pauseProject env workspace.id (Just root.id) "child"
      runSession env.pool $ Session.statement [root.id, child.id] pauseProjectsStatement
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjActive)
      fmap (fmap (.status)) (getProject env.pool child.id) `shouldReturn` Just ProjPaused
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjPaused)
      updated <- updateProject env.pool child.id (archiveStatusUpdate ProjActive)
      fmap (.status) updated `shouldBe` Just ProjPaused
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjActive)
      fmap (fmap (.status)) (getProject env.pool child.id) `shouldReturn` Just ProjActive
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjPaused)
      _ <- updateProject env.pool child.id (archiveStatusUpdate ProjPaused)
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjActive)
      fmap (fmap (.status)) (getProject env.pool child.id) `shouldReturn` Just ProjPaused

    it "rejects pause atomically for a running descendant subtask with unchanged audit/outbox" $ \env -> do
      workspace <- createTestWorkspace env "pause-running"
      root <- pauseProject env workspace.id Nothing "root"
      child <- pauseProject env workspace.id (Just root.id) "child"
      parent <- pauseTask env workspace.id (Just child.id) Nothing "parent"
      subtask <- pauseTask env workspace.id (Just child.id) (Just parent.id) "running child"
      _ <- Task.updateTask env.pool subtask.id (pauseTaskStatus InProgress)
      beforeEvents <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100
      beforeAudit <- getAuditLogRows env.pool "project" (T.pack (show root.id))
      pause <- try @DBException $ updateProject env.pool root.id (archiveStatusUpdate ProjPaused)
      pause `shouldSatisfy` \case Left (DBLifecycleViolation "PROJECT_PAUSE_BLOCKED_BY_RUNNING_TASKS" _ _ _) -> True; _ -> False
      mapM (fmap (fmap (.status)) . getProject env.pool) [root.id, child.id]
        `shouldReturn` replicate 2 (Just ProjActive)
      fmap (fmap (.status)) (Task.getTask env.pool subtask.id) `shouldReturn` Just InProgress
      listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100 `shouldReturn` beforeEvents
      getAuditLogRows env.pool "project" (T.pack (show root.id)) `shouldReturn` beforeAudit

    it "requires reopening a closed root before pausing" $ \env -> do
      workspace <- createTestWorkspace env "pause-closed"
      root <- pauseProject env workspace.id Nothing "root"
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjCompleted)
      rejected <- try @DBException $ updateProject env.pool root.id (archiveStatusUpdate ProjPaused)
      rejected `shouldSatisfy` \case Left (DBLifecycleViolation "PROJECT_PAUSE_REQUIRES_REOPEN" _ _ _) -> True; _ -> False
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjActive)
      fmap (fmap (.status)) (updateProject env.pool root.id (archiveStatusUpdate ProjPaused)) `shouldReturn` Just ProjPaused

    it "guards own-intent-only SQL writes and restores false intent on an already inherited paused row" $ \env -> do
      workspace <- createTestWorkspace env "pause-own-authority"
      root <- pauseProject env workspace.id Nothing "root"
      child <- pauseProject env workspace.id (Just root.id) "child"
      task <- pauseTask env workspace.id (Just child.id) Nothing "task"
      _ <- Task.updateTask env.pool task.id (pauseTaskStatus InProgress)
      rejected <- try @DBException $ runSession env.pool $ Session.statement (root.id, True) ownPauseProjectStatement
      rejected `shouldSatisfy` \case Left (DBLifecycleViolation "PROJECT_PAUSE_BLOCKED_BY_RUNNING_TASKS" _ _ _) -> True; _ -> False
      fmap (fmap (.status)) (getProject env.pool root.id) `shouldReturn` Just ProjActive
      _ <- Task.updateTask env.pool task.id (pauseTaskStatus Todo)
      runSession env.pool $ Session.statement (root.id, True) ownPauseProjectStatement
      fmap (fmap (.status)) (getProject env.pool child.id) `shouldReturn` Just ProjPaused
      runSession env.pool $ Session.statement (child.id, False) ownPauseProjectStatement
      -- Replaying effective paused plus own=false must retain inherited intent,
      -- including false->false, rather than treating it as a repeated own pause.
      runSession env.pool $ do
        Session.statement child.id setRestoredPauseIntentStatement
        Session.statement [child.id] pauseProjectsStatement
        Session.sql "SELECT set_config('hmem.project_pause_intent', '', true)"
      runSession env.pool $ Session.statement (root.id, False) ownPauseProjectStatement
      mapM (fmap (fmap (.status)) . getProject env.pool) [root.id, child.id]
        `shouldReturn` replicate 2 (Just ProjActive)
      fmap (fmap (.status)) (Task.getTask env.pool task.id) `shouldReturn` Just Todo
      _ <- Task.updateTask env.pool task.id (pauseTaskStatus Done)
      _ <- updateProject env.pool child.id (archiveStatusUpdate ProjCompleted)
      closed <- try @DBException $ runSession env.pool $ Session.statement (child.id, True) ownPauseProjectStatement
      closed `shouldSatisfy` \case Left (DBLifecycleViolation "PROJECT_PAUSE_REQUIRES_REOPEN" _ _ _) -> True; _ -> False

    it "reconciles canonical creation, project moves, batch task moves and inherited restores" $ \env -> do
      workspace <- createTestWorkspace env "pause-placement"
      paused <- pauseProject env workspace.id Nothing "paused"
      active <- pauseProject env workspace.id Nothing "active"
      moving <- pauseProject env workspace.id (Just active.id) "moving"
      task <- pauseTask env workspace.id (Just moving.id) Nothing "moving task"
      _ <- updateProject env.pool paused.id (archiveStatusUpdate ProjPaused)
      inherited <- pauseProject env workspace.id (Just paused.id) "inherited"
      inherited.status `shouldBe` ProjPaused
      created <- pauseTask env workspace.id (Just inherited.id) Nothing "created blocked"
      created.status `shouldBe` Blocked
      _ <- updateProject env.pool moving.id (UpdateProject Nothing Unchanged (SetTo paused.id) Nothing Nothing Nothing)
      fmap (fmap (.status)) (Task.getTask env.pool task.id) `shouldReturn` Just Blocked
      _ <- updateProject env.pool moving.id (UpdateProject Nothing Unchanged (SetTo active.id) Nothing Nothing Nothing)
      fmap (fmap (.status)) (Task.getTask env.pool task.id) `shouldReturn` Just Todo
      _ <- Task.moveTasksBatch env.pool [task.id] (Just inherited.id)
      fmap (fmap (.status)) (Task.getTask env.pool task.id) `shouldReturn` Just Blocked
      _ <- Task.moveTasksBatch env.pool [task.id] (Just active.id)
      fmap (fmap (.status)) (Task.getTask env.pool task.id) `shouldReturn` Just Todo
      _ <- deleteProject env.pool inherited.id
      restoreProject env.pool inherited.id `shouldReturn` True
      _ <- updateProject env.pool paused.id (archiveStatusUpdate ProjActive)
      fmap (fmap (.status)) (getProject env.pool inherited.id) `shouldReturn` Just ProjActive
      fmap (fmap (.status)) (Task.getTask env.pool created.id) `shouldReturn` Just Todo

    it "refuses task starts, running subtree moves and restores into paused ancestry" $ \env -> do
      workspace <- createTestWorkspace env "pause-running-placement"
      paused <- pauseProject env workspace.id Nothing "paused"
      active <- pauseProject env workspace.id Nothing "active"
      waiting <- pauseTask env workspace.id (Just paused.id) Nothing "waiting"
      moving <- pauseTask env workspace.id (Just active.id) Nothing "moving"
      _ <- Task.updateTask env.pool moving.id (pauseTaskStatus InProgress)
      _ <- updateProject env.pool paused.id (archiveStatusUpdate ProjPaused)
      let rejected action = do
            result <- try @DBException action
            result `shouldSatisfy` \case Left (DBLifecycleViolation "TASK_RUNNING_UNDER_PAUSED_PROJECT" _ _ _) -> True; _ -> False
      rejected $ Task.updateTask env.pool waiting.id (pauseTaskStatus InProgress)
      rejected $ Task.moveTasksBatch env.pool [moving.id] (Just paused.id)
      _ <- Task.deleteTask env.pool moving.id
      -- Restore uses the current project's ancestry, including a moved project.
      _ <- updateProject env.pool active.id (UpdateProject Nothing Unchanged (SetTo paused.id) Nothing Nothing Nothing)
      rejected $ Task.restoreTask env.pool moving.id
      fmap (fmap (.status)) (Task.getTask env.pool waiting.id) `shouldReturn` Just Blocked

    it "protects indirect done dependents during pause/resume but resets origin before unrelated dependency edits" $ \env -> do
      workspace <- createTestWorkspace env "pause-terminal-propagation"
      root <- pauseProject env workspace.id Nothing "root"
      prerequisite <- pauseTask env workspace.id (Just root.id) Nothing "prerequisite"
      doneTask <- pauseTask env workspace.id (Just root.id) Nothing "done dependent"
      indirect <- pauseTask env workspace.id (Just root.id) Nothing "indirect done"
      _ <- Task.updateTask env.pool prerequisite.id (pauseTaskStatus Done)
      Task.addDependency env.pool doneTask.id prerequisite.id
      _ <- Task.updateTask env.pool doneTask.id (pauseTaskStatus Done)
      Task.addDependency env.pool indirect.id doneTask.id
      _ <- Task.updateTask env.pool indirect.id (pauseTaskStatus Done)
      -- Legacy done dependents of an open prerequisite, impossible to create
      -- through normal readiness recomputation, expose V029's transitive branch.
      bracket_
        (runSession env.pool $ Session.sql "ALTER TABLE tasks DISABLE TRIGGER USER")
        (runSession env.pool $ Session.sql "ALTER TABLE tasks ENABLE TRIGGER USER") $
        runSession env.pool $ Session.statement prerequisite.id pauseReopenTaskStatement
      beforeDone <- Task.getTask env.pool doneTask.id
      beforeIndirect <- Task.getTask env.pool indirect.id
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjPaused)
      Task.getTask env.pool doneTask.id `shouldReturn` beforeDone
      Task.getTask env.pool indirect.id `shouldReturn` beforeIndirect
      runSession env.pool $ do
        Session.statement root.id resumeProjectStatement
        -- The very next unrelated dependency trigger in this transaction must
        -- retain the existing ability to reopen done dependents.
        Session.statement (doneTask.id, prerequisite.id) pauseTouchDependencyStatement
      mapM (fmap (fmap (.status)) . Task.getTask env.pool) [doneTask.id, indirect.id]
        `shouldReturn` replicate 2 (Just Blocked)

    it "retains ordinary done-dependent reopening and dependency insertion inside paused ancestry" $ \env -> do
      workspace <- createTestWorkspace env "pause-unrelated-dependencies"
      root <- pauseProject env workspace.id Nothing "root"
      prerequisite <- pauseTask env workspace.id (Just root.id) Nothing "prerequisite"
      dependent <- pauseTask env workspace.id (Just root.id) Nothing "dependent"
      newDependent <- pauseTask env workspace.id (Just root.id) Nothing "new dependent"
      _ <- Task.updateTask env.pool prerequisite.id (pauseTaskStatus Done)
      Task.addDependency env.pool dependent.id prerequisite.id
      _ <- Task.updateTask env.pool dependent.id (pauseTaskStatus Done)
      _ <- Task.updateTask env.pool newDependent.id (pauseTaskStatus Done)
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjPaused)
      mapM (fmap (fmap (.status)) . Task.getTask env.pool) [dependent.id, newDependent.id]
        `shouldReturn` replicate 2 (Just Done)
      _ <- Task.updateTask env.pool prerequisite.id (pauseTaskStatus Todo)
      fmap (fmap (.status)) (Task.getTask env.pool dependent.id) `shouldReturn` Just Blocked
      Task.addDependency env.pool newDependent.id prerequisite.id
      fmap (fmap (.status)) (Task.getTask env.pool newDependent.id) `shouldReturn` Just Blocked

  beforeAll setupTestPool $ aroundWith withTestTransaction $ describe "Project lifecycle" $ do
    it "cascade archival archives completed descendant projects and cancels unfinished tasks without changing done rows" $ \env -> do
      workspace <- createTestWorkspace env "project-cascade-archive"
      let create parent title = createProject env.pool CreateProject
            { workspaceId = workspace.id, parentId = parent, name = title, description = Nothing, priority = Nothing, metadata = Nothing }
          task project parent title = Task.createTask env.pool CreateTask
            { workspaceId = workspace.id, projectId = Just project, parentId = parent, title = title, description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
          projectStatus status = UpdateProject Nothing Unchanged Unchanged (Just status) Nothing Nothing
          taskStatus status = UpdateTask Nothing Unchanged Unchanged Unchanged (Just status) Nothing Nothing Unchanged
      root <- create Nothing "root"
      active <- create (Just root.id) "active child"
      completed <- create (Just active.id) "completed grandchild"
      _ <- updateProject env.pool completed.id (projectStatus ProjCompleted)
      parent <- task active.id Nothing "parent task"
      child <- task active.id (Just parent.id) "subtask"
      -- The DB permits a legacy/direct-SQL child without its parent's project.
      -- Archive must still follow the task hierarchy beyond project seed rows.
      unassignedChild <- runSession env.pool $ Session.statement (workspace.id, parent.id) unassignedArchiveChildStatement
      doneTask <- task active.id Nothing "done task"
      _ <- Task.updateTask env.pool doneTask.id (taskStatus Done)
      beforeDone <- Task.getTask env.pool doneTask.id
      _ <- updateProject env.pool root.id (projectStatus ProjArchived)
      mapM (fmap (fmap (.status)) . getProject env.pool) [root.id, active.id, completed.id]
        `shouldReturn` replicate 3 (Just ProjArchived)
      mapM (fmap (fmap (.status)) . Task.getTask env.pool) [parent.id, child.id, unassignedChild]
        `shouldReturn` replicate 3 (Just Cancelled)
      Task.getTask env.pool doneTask.id `shouldReturn` beforeDone
    it "cascade archival normalizes a historical root only on explicit status intent" $ \env -> do
      workspace <- createTestWorkspace env "project-cascade-repeat"
      root <- createProject env.pool (CreateProject workspace.id Nothing "root" Nothing Nothing Nothing)
      child <- createProject env.pool (CreateProject workspace.id (Just root.id) "completed" Nothing Nothing Nothing)
      _ <- updateProject env.pool child.id (archiveStatusUpdate ProjCompleted)
      -- Simulate already-archived historical state without sweeping it.
      bracket_
        (runSession env.pool $ Session.sql "ALTER TABLE projects DISABLE TRIGGER USER")
        (runSession env.pool $ Session.sql "ALTER TABLE projects ENABLE TRIGGER USER") $
        runSession env.pool $ Session.statement root.id archiveProjectStatement
      _ <- updateProject env.pool root.id ((archiveStatusUpdate ProjArchived) { status = Nothing, name = Just "renamed" })
      fmap (fmap (.status)) (getProject env.pool child.id) `shouldReturn` Just ProjCompleted
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjArchived)
      fmap (fmap (.status)) (getProject env.pool child.id) `shouldReturn` Just ProjArchived

    it "cascade archival from direct SQL preserves strict completion refusal" $ \env -> do
      workspace <- createTestWorkspace env "project-cascade-sql"
      root <- createProject env.pool (CreateProject workspace.id Nothing "root" Nothing Nothing Nothing)
      child <- createProject env.pool (CreateProject workspace.id (Just root.id) "child" Nothing Nothing Nothing)
      runSession env.pool $ Session.statement root.id archiveProjectStatement
      mapM (fmap (fmap (.status)) . getProject env.pool) [root.id, child.id] `shouldReturn` replicate 2 (Just ProjArchived)
      strict <- createProject env.pool (CreateProject workspace.id Nothing "strict completion" Nothing Nothing Nothing)
      _ <- createProject env.pool (CreateProject workspace.id (Just strict.id) "still open" Nothing Nothing Nothing)
      -- Expected SQL failures end this rollback-wrapped example: resource-pool
      -- discards the connection on exceptions, including its outer transaction.
      completion <- try (updateProject env.pool strict.id (archiveStatusUpdate ProjCompleted))
      completion `shouldSatisfy` (\case Left (DBLifecycleViolation "PROJECT_COMPLETION_BLOCKED" _ _ _) -> True; _ -> False)

    it "creates every initial task in order with project and task outbox events" $ \env -> do
      workspace <- createTestWorkspace env "project-spec-success"
      let input = CreateProjectSpec workspace.id "spec project" (Just "durable project description") (Just 7)
            [ ProjectSpecTask "first" (Just "first description") (Just 8)
            , ProjectSpecTask "second" Nothing Nothing
            , ProjectSpecTask "third" Nothing (Just 2) ]
      validateCreateProjectSpecInput input `shouldBe` []
      created <- createProjectSpec env.pool input
      project <- getProject env.pool created.projectId
      fmap (.name) project `shouldBe` Just "spec project"
      tasks <- mapM (Task.getTask env.pool) created.taskIds
      map (fmap (.title)) tasks `shouldBe` map Just ["first", "second", "third"]
      map (fmap (.projectId)) tasks `shouldBe` replicate 3 (Just (Just created.projectId))
      records <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 10
      length records `shouldBe` 4
      let transactionId record = field "transaction" record.outboxEnvelope >>= field "id"
      map transactionId records `shouldSatisfy` \case [Just a, Just b, Just c, Just d] -> a == b && b == c && c == d; _ -> False

    it "does not mutate independent observations" $ \env -> do
      workspace <- createTestWorkspace env "project-observation-isolation"
      observation <- createObservation env.pool (CreateObservation workspace.id [ObservationSubject SubjectFile "src/Project.hs"] "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "independent")
      project <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Nothing, name = "project", description = Nothing, priority = Nothing, metadata = Nothing }
      deleteProjectCascade env.pool project.id >>= (`shouldSatisfy` isJust)
      getObservation env.pool workspace.id observation.id >>= (`shouldSatisfy` isJust)
    it "captures committed project mutations in the workspace outbox" $ \env -> do
      workspace <- createTestWorkspace env "project-change-stream"
      _ <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Nothing, name = "project", description = Nothing, priority = Nothing, metadata = Nothing }
      records <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 10
      case records of
        [record] -> record.outboxCursor `shouldBe` 1
        _ -> expectationFailure "expected exactly one project outbox record"
    it "publishes the exact sanitized project invalidations" $ \env -> do
      workspace <- createTestWorkspace env "project-change-stream-invalidations"
      project <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Nothing, name = "project", description = Nothing, priority = Nothing, metadata = Nothing }
      records <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 10
      case records of
        [record] -> case record.outboxEnvelope of
          Object envelope -> do
            KeyMap.lookup "invalidations" envelope `shouldBe` Just (toInvalidations workspace.id project.id)
            KeyMap.member "old_values" envelope `shouldBe` False
            KeyMap.member "new_values" envelope `shouldBe` False
            KeyMap.member "embedding" envelope `shouldBe` False
          _ -> expectationFailure "outbox envelope must be an object"
        _ -> expectationFailure "expected exactly one project outbox record"
    it "does not emit an outbox record for an ignored no-op update" $ \env -> do
      workspace <- createTestWorkspace env "project-change-stream-noop"
      project <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Nothing, name = "project", description = Nothing, priority = Nothing, metadata = Nothing }
      runSession env.pool $ Session.statement project.id touchProjectStatement
      listOutboxAfter env.pool (WorkspaceScope workspace.id) 1 10 `shouldReturn` []
    it "records every project cascade with contiguous cursors and one transaction id" $ \env -> do
      workspace <- createTestWorkspace env "project-change-stream-cascade"
      root <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Nothing, name = "root", description = Nothing, priority = Nothing, metadata = Nothing }
      _child <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Just root.id, name = "child", description = Nothing, priority = Nothing, metadata = Nothing }
      deleteProjectCascade env.pool root.id >>= (`shouldSatisfy` isJust)
      records <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 10
      map (.outboxCursor) records `shouldBe` [1, 2, 3, 4]
      let deleted = drop 2 records
          action record = Just record.outboxEnvelope >>= field "entity" >>= field "action"
          transactionId record = Just record.outboxEnvelope >>= field "transaction" >>= field "id"
      map action deleted `shouldBe` [Just (String "deleted"), Just (String "deleted")]
      map transactionId deleted `shouldSatisfy` \case [Just first, Just second] -> first == second; _ -> False
    it "captures task, observation, dependency, global group, and group-member writes with exact targets" $ \env -> do
      workspace <- createTestWorkspace env "change-stream-direct-matrix"
      project <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Nothing, name = "project", description = Nothing, priority = Nothing, metadata = Nothing }
      prerequisite <- Task.createTask env.pool CreateTask
        { workspaceId = workspace.id, projectId = Just project.id, parentId = Nothing, title = "prerequisite", description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      dependent <- Task.createTask env.pool CreateTask
        { workspaceId = workspace.id, projectId = Just project.id, parentId = Just prerequisite.id, title = "dependent", description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      Task.addDependency env.pool dependent.id prerequisite.id
      _ <- createObservation env.pool (CreateObservation workspace.id [ObservationSubject SubjectFile "src/ChangeStream.hs"] "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "captured")
      group <- WorkspaceGroup.createGroup env.pool (CreateWorkspaceGroup "change-stream-group" Nothing)
      _ <- WorkspaceGroup.addMember env.pool group.id workspace.id
      workspaceEvents <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 50
      globalEvents <- listOutboxAfter env.pool GlobalScope 0 50
      let matching kind entityId = [record | record <- workspaceEvents, (field "entity" record.outboxEnvelope >>= field "type") == Just (String kind), (field "entity" record.outboxEnvelope >>= field "id") == Just (String entityId)]
          taskExpected = Aeson.toJSON
            [ object ["kind" .= ("entity" :: Text), "target" .= ("task:" <> T.pack (show prerequisite.id))]
            , object ["kind" .= ("collection" :: Text), "target" .= ("tasks:" <> T.pack (show workspace.id))]
            , object ["kind" .= ("tree" :: Text), "target" .= ("workspace:" <> T.pack (show workspace.id))]
            , object ["kind" .= ("readiness" :: Text), "target" .= ("task:" <> T.pack (show prerequisite.id))]
            , object ["kind" .= ("next_task" :: Text), "target" .= ("workspace:" <> T.pack (show workspace.id))]
            , object ["kind" .= ("search" :: Text), "target" .= ("workspace:" <> T.pack (show workspace.id))]
            , object ["kind" .= ("readiness" :: Text), "target" .= ("project:" <> T.pack (show project.id))]
            ]
          memberExpected = Aeson.toJSON
            [ object ["kind" .= ("entity" :: Text), "target" .= ("group_membership:" <> T.pack (show group.id) <> ":" <> T.pack (show workspace.id))]
            , object ["kind" .= ("collection" :: Text), "target" .= ("group:" <> T.pack (show group.id) <> ":members")]
            , object ["kind" .= ("collection" :: Text), "target" .= ("workspace:" <> T.pack (show workspace.id) <> ":groups")]
            ]
          globalInvalidations entityId = Aeson.toJSON
            [ object ["kind" .= ("entity" :: Text), "target" .= ("group:" <> entityId)]
            , object ["kind" .= ("collection" :: Text), "target" .= ("workspace-groups" :: Text)]
            ]
      case matching "task" (T.pack (show prerequisite.id)) of
        (record:_) -> field "invalidations" record.outboxEnvelope `shouldBe` Just taskExpected
        _ -> expectationFailure "missing direct task outbox record"
      case matching "workspace_group_membership" (T.pack (show group.id) <> ":" <> T.pack (show workspace.id)) of
        (record:_) -> field "invalidations" record.outboxEnvelope `shouldBe` Just memberExpected
        _ -> expectationFailure "missing direct group-member outbox record"
      let groupEvents =
            [ record
            | record <- globalEvents
            , (field "entity" record.outboxEnvelope >>= field "type") == Just (String "workspace_group")
            , (field "entity" record.outboxEnvelope >>= field "id") == Just (String (T.pack (show group.id)))
            ]
      case groupEvents of
        (record:_) -> field "invalidations" record.outboxEnvelope `shouldBe` Just (globalInvalidations (T.pack (show group.id)))
        _ -> expectationFailure "missing global workspace-group outbox record"
      let workspaceEventsGlobal = [record | record <- globalEvents, (field "entity" record.outboxEnvelope >>= field "type") == Just (String "workspace"), (field "entity" record.outboxEnvelope >>= field "id") == Just (String (T.pack (show workspace.id)))]
          workspaceExpected = Aeson.toJSON
            [ object ["kind" .= ("entity" :: Text), "target" .= ("workspace:" <> T.pack (show workspace.id))]
            , object ["kind" .= ("catalogue" :: Text), "target" .= ("workspace-catalog" :: Text)]
            , object ["kind" .= ("collection" :: Text), "target" .= ("workspace-groups" :: Text)]
            ]
      case workspaceEventsGlobal of
        (record:_) -> field "invalidations" record.outboxEnvelope `shouldBe` Just workspaceExpected
        _ -> expectationFailure "missing global workspace-catalogue outbox record"
      let workspaceCaptured = any (\record -> (field "entity" record.outboxEnvelope >>= field "type") == Just (String "observation")) workspaceEvents
          dependencyCaptured = any (\record -> (field "entity" record.outboxEnvelope >>= field "type") == Just (String "task_dependency")) workspaceEvents
      workspaceCaptured `shouldBe` True
      dependencyCaptured `shouldBe` True
    it "captures every task cascade in one transaction" $ \env -> do
      workspace <- createTestWorkspace env "task-change-stream-cascade"
      root <- Task.createTask env.pool CreateTask
        { workspaceId = workspace.id, projectId = Nothing, parentId = Nothing, title = "root", description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      _child <- Task.createTask env.pool CreateTask
        { workspaceId = workspace.id, projectId = Nothing, parentId = Just root.id, title = "child", description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      Task.deleteTaskCascade env.pool root.id >>= (`shouldSatisfy` isJust)
      records <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 2 20
      let deleted = [record | record <- records, (field "entity" record.outboxEnvelope >>= field "action") == Just (String "deleted")]
          transactionId record = field "transaction" record.outboxEnvelope >>= field "id"
      length deleted `shouldBe` 2
      map transactionId deleted `shouldSatisfy` \case [Just first, Just second] -> first == second; _ -> False

  beforeAll setupTestPool $ describe "Project spec transaction rollback" $
    it "rolls back the project and all tasks when the middle insert fails" $ \env -> do
      workspace <- createTestWorkspace env "project-spec-rollback"
      let addConstraint = runSession env.pool $ Session.sql "ALTER TABLE tasks ADD CONSTRAINT project_spec_reject_middle CHECK (title <> 'reject-middle')"
          dropConstraint = runSession env.pool $ Session.sql "ALTER TABLE tasks DROP CONSTRAINT project_spec_reject_middle"
      bracket_ addConstraint dropConstraint $ do
        let input = CreateProjectSpec workspace.id "rolled-back project" Nothing Nothing
              [ ProjectSpecTask "first" Nothing Nothing
              , ProjectSpecTask "reject-middle" Nothing Nothing
              , ProjectSpecTask "third" Nothing Nothing ]
        result <- try @DBException (createProjectSpec env.pool input)
        result `shouldSatisfy` \case Left (DBCheckViolation _) -> True; _ -> False
        projects <- listProjects env.pool workspace.id Nothing Nothing Nothing
        projects `shouldBe` []
        tasks <- Task.listTasksByWorkspace env.pool workspace.id Nothing Nothing Nothing Nothing
        tasks `shouldBe` []
        listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 10 `shouldReturn` []

toInvalidations :: UUID -> UUID -> Value
toInvalidations workspaceId projectId =
  let workspace = show workspaceId
      project = T.pack (show projectId)
  in Aeson.toJSON
    [ object ["kind" .= ("entity" :: Text), "target" .= ("project:" <> project)]
    , object ["kind" .= ("collection" :: Text), "target" .= ("projects:" <> T.pack workspace)]
    , object ["kind" .= ("tree" :: Text), "target" .= ("workspace:" <> T.pack workspace)]
    , object ["kind" .= ("readiness" :: Text), "target" .= ("project:" <> project)]
    , object ["kind" .= ("next_task" :: Text), "target" .= ("workspace:" <> T.pack workspace)]
    , object ["kind" .= ("search" :: Text), "target" .= ("workspace:" <> T.pack workspace)]
    ]

touchProjectStatement :: Statement.Statement UUID ()
touchProjectStatement = Statement.Statement
  "UPDATE projects SET updated_at = updated_at WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

field :: Text -> Value -> Maybe Value
field key = \case
  Object objectValue -> KeyMap.lookup (AesonKey.fromText key) objectValue
  _ -> Nothing

archiveStatusUpdate :: ProjectStatus -> UpdateProject
archiveStatusUpdate status = UpdateProject Nothing Unchanged Unchanged (Just status) Nothing Nothing

archiveProjectStatement :: Statement.Statement UUID ()
archiveProjectStatement = Statement.Statement
  "UPDATE projects SET status = 'archived' WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

unassignedArchiveChildStatement :: Statement.Statement (UUID, UUID) UUID
unassignedArchiveChildStatement = Statement.Statement
  "INSERT INTO tasks (workspace_id, parent_id, title) VALUES ($1, $2, 'unassigned hierarchy child') RETURNING id"
  (contramap fst (Enc.param (Enc.nonNullable Enc.uuid)) <> contramap snd (Enc.param (Enc.nonNullable Enc.uuid)))
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.uuid))) True

pauseProject :: TestEnv -> UUID -> Maybe UUID -> Text -> IO Project
pauseProject env workspace parent name = createProject env.pool (CreateProject workspace parent name Nothing Nothing Nothing)

pauseTask :: TestEnv -> UUID -> Maybe UUID -> Maybe UUID -> Text -> IO Task
pauseTask env workspace project parent title = Task.createTask env.pool (CreateTask workspace project parent title Nothing Nothing Nothing Nothing)

pauseTaskStatus :: TaskStatus -> UpdateTask
pauseTaskStatus status = UpdateTask Nothing Unchanged Unchanged Unchanged (Just status) Nothing Nothing Unchanged

pauseProjectsStatement :: Statement.Statement [UUID] ()
pauseProjectsStatement = Statement.Statement
  "UPDATE projects SET status = 'paused' WHERE id = ANY($1)"
  (Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))) Dec.noResult True

resumeProjectStatement :: Statement.Statement UUID ()
resumeProjectStatement = Statement.Statement "UPDATE projects SET status = 'active' WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

pauseReopenTaskStatement :: Statement.Statement UUID ()
pauseReopenTaskStatement = Statement.Statement "UPDATE tasks SET status = 'todo' WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

freezePauseTaskStatement :: Statement.Statement UUID ()
freezePauseTaskStatement = Statement.Statement "UPDATE tasks SET status = 'done' WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

startFrozenPauseTaskStatement :: Statement.Statement UUID ()
startFrozenPauseTaskStatement = Statement.Statement "UPDATE tasks SET status = 'in_progress' WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

pauseTouchDependencyStatement :: Statement.Statement (UUID, UUID) ()
pauseTouchDependencyStatement = Statement.Statement
  "UPDATE task_dependencies SET task_id = task_id WHERE task_id = $1 AND depends_on_id = $2"
  (contramap fst (Enc.param (Enc.nonNullable Enc.uuid)) <> contramap snd (Enc.param (Enc.nonNullable Enc.uuid))) Dec.noResult True

ownPauseProjectStatement :: Statement.Statement (UUID, Bool) ()
ownPauseProjectStatement = Statement.Statement "UPDATE projects SET own_paused = $2 WHERE id = $1"
  (contramap fst (Enc.param (Enc.nonNullable Enc.uuid)) <> contramap snd (Enc.param (Enc.nonNullable Enc.bool))) Dec.noResult True

setRestoredPauseIntentStatement :: Statement.Statement UUID ()
setRestoredPauseIntentStatement = Statement.Statement
  "SELECT set_config('hmem.project_pause_intent', jsonb_build_object('id', $1::uuid, 'own_paused', false)::text, true)"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True
