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
import HMem.DB.Pool (runSession, DBException(..))
import HMem.DB.Project
import HMem.DB.Task qualified as Task
import HMem.DB.TestHarness
import HMem.DB.WorkspaceGroup qualified as WorkspaceGroup
import HMem.Types

spec :: Spec
spec = do
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
