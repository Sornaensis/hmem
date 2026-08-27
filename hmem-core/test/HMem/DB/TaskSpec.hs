module HMem.DB.TaskSpec (spec) where

import Data.Maybe (isJust)
import Test.Hspec

import HMem.DB.Observation
import HMem.DB.Task
import HMem.DB.TestHarness
import HMem.Types

spec :: Spec
spec = beforeAll setupTestPool $ aroundWith withTestTransaction $
  describe "Task lifecycle" $
    it "does not mutate independent observations" $ \env -> do
      workspace <- createTestWorkspace env "task-observation-isolation"
      observation <- createObservation env.pool (CreateObservation workspace.id [ObservationSubject SubjectFile "src/Task.hs"] "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "independent")
      task <- createTask env.pool CreateTask
        { workspaceId = workspace.id, projectId = Nothing, parentId = Nothing, title = "task", description = Nothing
        , priority = Nothing, metadata = Nothing, dueAt = Nothing }
      deleteTaskCascade env.pool task.id >>= (`shouldSatisfy` isJust)
      getObservation env.pool workspace.id observation.id >>= (`shouldSatisfy` isJust)
