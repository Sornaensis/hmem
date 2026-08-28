module HMem.DB.ProjectSpec (spec) where

import Data.Aeson (Value(..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Maybe (isJust)
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
import HMem.DB.Pool (runSession)
import HMem.DB.Project
import HMem.DB.Task qualified as Task
import HMem.DB.TestHarness
import HMem.DB.WorkspaceGroup qualified as WorkspaceGroup
import HMem.Types

spec :: Spec
spec = beforeAll setupTestPool $ aroundWith withTestTransaction $
  describe "Project lifecycle" $ do
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
