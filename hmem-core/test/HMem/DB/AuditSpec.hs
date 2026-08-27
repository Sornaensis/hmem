module HMem.DB.AuditSpec (spec) where

import Data.UUID qualified as UUID
import Test.Hspec

import HMem.DB.Audit
import HMem.DB.Observation
import HMem.DB.TestHarness
import HMem.Types

spec :: Spec
spec = beforeAll setupTestPool $ aroundWith withTestTransaction $
  describe "Audit" $
    it "looks up observation audit records without legacy entity coupling" $ \env -> do
      workspace <- createTestWorkspace env "observation-audit"
      observation <- createObservation env.pool (CreateObservation workspace.id [ObservationSubject SubjectFile "src/Audit.hs"] "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "audited")
      entries <- getAuditByEntity env.pool "observation" (UUID.toText observation.id) Nothing
      entries `shouldSatisfy` (not . null)
