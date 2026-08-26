module HMem.DB.AuthSpec (spec) where

import Test.Hspec

import HMem.DB.Auth
import HMem.DB.Observation
import HMem.DB.TestHarness
import HMem.Types

spec :: Spec
spec = beforeAll setupTestPool $ aroundWith withTestTransaction $
  describe "Entity scope kinds" $
    it "resolves observations to their workspace" $ \env -> do
      workspace <- createTestWorkspace env "observation-auth-scope"
      observation <- createObservation env.pool (CreateObservation workspace.id SubjectFile "src/Auth.hs" "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "scope")
      resolveEntityScope env.pool EntityObservation observation.id
        `shouldReturn` Just (EntityWorkspaceScope workspace.id)
