module HMem.DB.OverviewSpec (spec) where

import Data.Text (Text)
import Data.UUID (UUID)
import Test.Hspec

import HMem.DB.Observation
import HMem.DB.Overview
import HMem.DB.Project
import HMem.DB.Task
import HMem.DB.TestHarness
import HMem.Types

spec :: Spec
spec = beforeAll setupTestPool $ aroundWith withTestTransaction $
  describe "Overview" $ do
    it "returns project/task hierarchy, dependencies, and readiness without observation links" $ \env -> do
      workspace <- createTestWorkspace env "overview-without-observation-links"
      _observation <- createObservation env.pool CreateObservation
        { workspaceId = workspace.id, subjectKind = SubjectFile, subject = "src/Overview.hs"
        , gitSha = canonicalSha, content = "independent observation" }
      project <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Nothing, name = "project", description = Nothing
        , priority = Nothing, metadata = Nothing }
      child <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Just project.id, name = "child", description = Nothing
        , priority = Nothing, metadata = Nothing }
      dependency <- createTask env.pool (newTask workspace.id (Just project.id) "dependency")
      task <- createTask env.pool (newTask workspace.id (Just project.id) "task")
      addDependency env.pool task.id dependency.id

      projectOverview <- getProjectOverview env.pool project.id
      case projectOverview of
        Nothing -> expectationFailure "Expected project overview"
        Just overview -> do
          map (.id) overview.tasks `shouldMatchList` [dependency.id, task.id]
          map (.id) overview.subprojects `shouldBe` [child.id]
          overview.readinessRollup.completionReady `shouldBe` False

      taskOverview <- getTaskOverview env.pool task.id
      case taskOverview of
        Nothing -> expectationFailure "Expected task overview"
        Just overview -> do
          map (.id) overview.dependencies `shouldBe` [dependency.id]
          overview.readinessRollup.completionReady `shouldBe` True

newTask :: UUID -> Maybe UUID -> Text -> CreateTask
newTask workspace project title = CreateTask
  { workspaceId = workspace, projectId = project, parentId = Nothing, title = title
  , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }

canonicalSha :: Text
canonicalSha = "0123456789abcdef0123456789abcdef01234567"
