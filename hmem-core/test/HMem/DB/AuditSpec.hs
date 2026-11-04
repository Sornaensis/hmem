module HMem.DB.AuditSpec (spec) where

import Data.UUID qualified as UUID
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Test.Hspec

import HMem.DB.Audit
import HMem.DB.Observation
import HMem.DB.Pool (runSession)
import HMem.DB.TestHarness
import HMem.Types

spec :: Spec
spec = beforeAll setupTestPool $ aroundWith withTestTransaction $
  describe "Audit" $ do
    it "looks up observation audit records without legacy entity coupling" $ \env -> do
      workspace <- createTestWorkspace env "observation-audit"
      observation <- createObservation env.pool (CreateObservation workspace.id [ObservationSubject SubjectFile "src/Audit.hs"] "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "audited")
      entries <- getAuditByEntity env.pool "observation" (UUID.toText observation.id) Nothing
      entries `shouldSatisfy` (not . null)

    it "excludes both session entity types before filtering and paging Audit rows" $ \env -> do
      workspace <- createTestWorkspace env "session-audit-filter"
      runSession env.pool $ Session.statement workspace.id $ Statement.Statement
        "INSERT INTO audit_log(workspace_id, entity_type, entity_id, action, changed_at) \
        \SELECT $1, entity_type, entity_id, action::audit_action_enum, stamp::timestamptz FROM (VALUES \
        \ ('category', 'legacy-category', 'create', '2024-01-01T12:00:00Z'), \
        \ ('task', 'task-event', 'update', '2024-01-02T12:00:00Z'), \
        \ ('project', 'project-event', 'create', '2024-01-03T12:00:00Z'), \
        \ ('agent_session', 'session-', 'create', '2024-01-04T12:00:00Z'), \
        \ ('task_session', 'connection-', 'create', '2024-01-05T12:00:00Z')) AS seed(entity_type, entity_id, action, stamp) \
        \CROSS JOIN LATERAL generate_series(1, CASE WHEN entity_type IN ('agent_session','task_session') THEN 4 ELSE 1 END)"
        (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True
      let query inclusion pageOffset = AuditLogQuery (Just workspace.id) Nothing Nothing Nothing
            (Just (read "2024-01-01 00:00:00 UTC")) (Just (read "2024-01-06 00:00:00 UTC")) (Just 2) pageOffset inclusion
          kinds q = map (.entityType) <$> getAuditLog env.pool q
          excluding = query (Just False) Nothing
      kinds (query Nothing Nothing) `shouldReturn` ["task_session", "task_session"]
      kinds (query (Just True) (Just 3)) `shouldReturn` ["task_session", "agent_session"]
      kinds excluding `shouldReturn` ["project", "task"]
      kinds (query (Just False) (Just 2)) `shouldReturn` ["category"]
      kinds (excluding { action = Just AuditCreate, until = Just (read "2024-01-03 00:00:00 UTC"), includeAgentSessions = Just False }) `shouldReturn` ["category"]
      kinds (excluding { entityType = Just "agent_session", includeAgentSessions = Just False }) `shouldReturn` []
      kinds (excluding { entityType = Just "category", includeAgentSessions = Just False }) `shouldReturn` ["category"]
