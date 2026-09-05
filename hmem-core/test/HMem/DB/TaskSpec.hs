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
import HMem.DB.Overview (getTaskOverview)
import HMem.DB.ChangeStream (ChangeScope(..), OutboxRecord(..), listOutboxAfter)
import HMem.DB.Pool (DBException(..), withConn)
import HMem.DB.Project qualified as Project
import HMem.DB.Task
import HMem.DB.TestHarness
import HMem.Types

spec :: Spec
spec = beforeAll setupTestPool $ aroundWith withTestTransaction $
  describe "Task lifecycle" $ do
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

expectLifecycleCode :: Text -> IO Int -> Expectation
expectLifecycleCode expected action = do
  result <- try action
  case result of
    Left (DBLifecycleViolation code _ _ _) -> code `shouldBe` expected
    Left other -> expectationFailure ("expected lifecycle violation, got " <> show other)
    Right value -> expectationFailure ("expected lifecycle violation, moved " <> show value <> " tasks")

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
