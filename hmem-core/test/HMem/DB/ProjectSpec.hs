module HMem.DB.ProjectSpec (spec) where

import Data.Maybe (isJust)
import Test.Hspec

import HMem.DB.Observation
import HMem.DB.Project
import HMem.DB.TestHarness
import HMem.Types

spec :: Spec
spec = beforeAll setupTestPool $ aroundWith withTestTransaction $
  describe "Project lifecycle" $
    it "does not mutate independent observations" $ \env -> do
      workspace <- createTestWorkspace env "project-observation-isolation"
      observation <- createObservation env.pool (CreateObservation workspace.id [ObservationSubject SubjectFile "src/Project.hs"] "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "independent")
      project <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Nothing, name = "project", description = Nothing, priority = Nothing, metadata = Nothing }
      deleteProjectCascade env.pool project.id >>= (`shouldSatisfy` isJust)
      getObservation env.pool workspace.id observation.id >>= (`shouldSatisfy` isJust)
