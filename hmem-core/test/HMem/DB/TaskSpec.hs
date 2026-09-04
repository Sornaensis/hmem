module HMem.DB.TaskSpec (spec) where

import Data.Either (isLeft)
import Data.Functor.Contravariant (contramap)
import Data.Maybe (isJust)
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Data.UUID (UUID)
import Test.Hspec

import HMem.DB.Observation
import HMem.DB.Overview (getTaskOverview)
import HMem.DB.Pool (withConn)
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

insertDependencyDirect :: Statement.Statement (UUID, UUID) ()
insertDependencyDirect = Statement.Statement
  "INSERT INTO task_dependencies (task_id, depends_on_id) VALUES ($1, $2)"
  (contramap fst (Enc.param (Enc.nonNullable Enc.uuid)) <> contramap snd (Enc.param (Enc.nonNullable Enc.uuid)))
  Dec.noResult
  True

ensureSession :: String -> Either a () -> IO ()
ensureSession _ (Right ()) = pure ()
ensureSession label (Left _) = fail (label <> " failed")
