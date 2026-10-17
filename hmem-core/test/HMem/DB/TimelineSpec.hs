module HMem.DB.TimelineSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS8
import Data.Functor.Contravariant (contramap)
import Data.List (sort)
import Data.Text (Text)
import Data.Time (UTCTime(..), addDays, addUTCTime, getCurrentTime, utctDay)
import Data.UUID (UUID)
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Test.Hspec

import HMem.DB.Observation
import HMem.DB.ObservationFixture (reviewed, writeObservation)
import HMem.DB.Pool (runSession)
import HMem.DB.Project
import HMem.DB.Task qualified as Task
import HMem.DB.TestHarness
import HMem.DB.Timeline
import HMem.DB.Workspace (getWorkspacePlanningCounts)
import HMem.Types

spec :: Spec
spec = beforeAll setupTestPool $ aroundWith withTestTransaction $
  describe "Timeline buckets" $ do
    it "reconstructs observation carry-in and visible/out-of-range hard deletes at exclusive cutoffs" $ \env -> do
      workspace <- createTestWorkspace env "timeline-observation-carry"
      first <- stockObservation env workspace.id
      laterDeleted <- stockObservation env workspace.id
      retimePending env workspace.id (utc "2020-01-01 00:00:00 UTC")
      _ <- stockObservation env workspace.id
      retimePending env workspace.id (utc "2020-01-03 00:00:00 UTC")
      deleteObservation env.pool workspace.id first.id `shouldReturn` True
      retimePending env workspace.id (utc "2020-01-04 00:00:00 UTC")
      deleteObservation env.pool workspace.id laterDeleted.id `shouldReturn` True
      retimePending env workspace.id (utc "2020-01-06 00:00:00 UTC")
      buckets <- listWorkspaceTimelineBuckets env.pool workspace.id (utc "2020-01-02 00:00:00 UTC") (utc "2020-01-05 00:00:00 UTC") "day"
      map (.timelineBucketObservationTotal) buckets `shouldBe` map Just [2,3,2]
      map (.timelineBucketObservationStatus) buckets `shouldBe` replicate 3 "available"
      map (.timelineBucketObservationAsOf) buckets `shouldBe` map (.timelineBucketOpenAsOf) buckets
      map (\b -> b.timelineBucketSeries.seriesObservation.created) buckets `shouldBe` [0,1,0]
      map (\b -> b.timelineBucketSeries.seriesObservation.deleted) buckets `shouldBe` [0,0,1]

    it "counts multi-subject observations once and ignores edits and re-audit versions" $ \env -> do
      workspace <- createTestWorkspace env "timeline-observation-versions"
      other <- createTestWorkspace env "timeline-observation-other"
      observation <- stockObservation env workspace.id
      _ <- stockObservation env other.id
      retimePending env workspace.id (utc "2020-01-01 00:00:00 UTC")
      _ <- writeObservation env.pool workspace.id observation.id (reviewed "corrected")
      retimePending env workspace.id (utc "2020-01-03 00:00:00 UTC")
      _ <- writeObservation env.pool workspace.id observation.id (ReviewedObservationUpdate "corrected" "cccccccccccccccccccccccccccccccccccccccc")
      retimePending env workspace.id (utc "2020-01-04 00:00:00 UTC")
      mapM_ (\size -> do
        buckets <- listWorkspaceTimelineBuckets env.pool workspace.id (utc "2020-01-02 00:00:00 UTC") (utc "2020-01-04 12:00:00 UTC") size
        map (.timelineBucketObservationTotal) buckets `shouldSatisfy` all (== Just 1)
        map (.timelineBucketObservationStatus) buckets `shouldSatisfy` all (== "available")
        (last buckets).timelineBucketObservationAsOf `shouldBe` Just (utc "2020-01-04 12:00:00 UTC")) ["day", "week", "month", "quarter"]

    it "reconciles tied observation creation/deletion without imposing an audit UUID order" $ \env -> do
      workspace <- createTestWorkspace env "timeline-observation-ties"
      observation <- stockObservation env workspace.id
      _ <- writeObservation env.pool workspace.id observation.id (reviewed "changed at the same instant")
      deleteObservation env.pool workspace.id observation.id `shouldReturn` True
      retimePending env workspace.id (utc "2020-01-02 00:00:00 UTC")
      buckets <- listWorkspaceTimelineBuckets env.pool workspace.id (utc "2020-01-01 00:00:00 UTC") (utc "2020-01-04 00:00:00 UTC") "day"
      map (.timelineBucketObservationTotal) buckets `shouldBe` replicate 3 (Just 0)
      map (.timelineBucketObservationStatus) buckets `shouldBe` replicate 3 "available"
      mutateAudit env workspace.id "DELETE FROM audit_log WHERE workspace_id = $1 AND entity_type = 'observation' AND action = 'create'"
      noCreate <- listWorkspaceTimelineBuckets env.pool workspace.id (utc "2020-01-01 00:00:00 UTC") (utc "2020-01-04 00:00:00 UTC") "day"
      map (.timelineBucketObservationTotal) noCreate `shouldBe` [Nothing, Just 0, Just 0]
      map (.timelineBucketObservationStatus) noCreate `shouldBe` ["history_unavailable", "available", "available"]

    it "keeps missing and malformed observation history independent of valid open inventory" $ \env -> do
      mapM_ (\damage -> do
        workspace <- createTestWorkspace env "timeline-observation-gap"
        _ <- stockTask env workspace.id Nothing Nothing
        observation <- stockObservation env workspace.id
        retimePending env workspace.id (utc "2020-01-01 00:00:00 UTC")
        _ <- writeObservation env.pool workspace.id observation.id (reviewed "edited")
        retimePending env workspace.id (utc "2020-01-03 00:00:00 UTC")
        mutateAudit env workspace.id damage
        buckets <- listWorkspaceTimelineBuckets env.pool workspace.id (utc "2020-01-01 00:00:00 UTC") (utc "2020-01-04 00:00:00 UTC") "day"
        map (.timelineBucketObservationTotal) buckets `shouldBe` [Nothing, Nothing, Just 1]
        map (.timelineBucketObservationStatus) buckets `shouldBe` ["history_unavailable", "history_unavailable", "available"]
        map (.timelineBucketOpenCounts) buckets `shouldBe` replicate 3 (Just (TimelineOpenCounts 0 1)))
        [ "DELETE FROM audit_log WHERE workspace_id = $1 AND entity_type = 'observation' AND action = 'create'"
        , "UPDATE audit_log SET old_values = NULL WHERE workspace_id = $1 AND entity_type = 'observation' AND action = 'update'"
        , "UPDATE audit_log SET new_values = new_values - 'workspace_id' WHERE workspace_id = $1 AND entity_type = 'observation' AND action = 'update'"
        ]

    it "rejects contradictory observation anchors and missing complete history without clamping" $ \env -> do
      mapM_ (\damage -> do
        workspace <- createTestWorkspace env "timeline-observation-anchor"
        _ <- stockObservation env workspace.id
        retimePending env workspace.id (utc "2020-01-01 00:00:00 UTC")
        mutateAudit env workspace.id damage
        historical <- listWorkspaceTimelineBuckets env.pool workspace.id (utc "2020-01-02 00:00:00 UTC") (utc "2020-01-03 00:00:00 UTC") "day"
        map (.timelineBucketObservationTotal) historical `shouldBe` [Nothing]
        map (.timelineBucketObservationStatus) historical `shouldBe` ["history_unavailable"]
        now <- getCurrentTime
        current <- listWorkspaceTimelineBuckets env.pool workspace.id (addUTCTime (-1) now) (addUTCTime 1 now) "day"
        (last current).timelineBucketObservationTotal `shouldBe` Just 1
        (last current).timelineBucketObservationStatus `shouldBe` "available")
        [ "DELETE FROM audit_log WHERE workspace_id = $1 AND entity_type = 'observation'"
        , "INSERT INTO audit_log (workspace_id, entity_type, entity_id, action, old_values, new_values, changed_at) SELECT workspace_id, entity_type, entity_id, action, old_values, new_values, changed_at FROM audit_log WHERE workspace_id = $1 AND entity_type = 'observation'"
        ]

    it "rejects internal net-cancelling existence gaps even without retained creates" $ \env -> do
      workspace <- createTestWorkspace env "timeline-observation-internal-gap"
      let entity = "00000000-0000-0000-0000-000000000381"
          snapshot = Just (Aeson.object ["id" Aeson..= entity, "workspace_id" Aeson..= workspace.id])
      -- Missing create/delete transitions around the retained metadata cancel
      -- in total, but its reconstructed absent boundary contradicts the row.
      insertAudit env workspace.id "observation" entity "create" Nothing snapshot (utc "2020-01-01 00:00:00 UTC")
      insertAudit env workspace.id "observation" entity "delete" snapshot Nothing (utc "2020-01-02 00:00:00 UTC")
      insertAudit env workspace.id "observation" entity "update" snapshot snapshot (utc "2020-01-04 00:00:00 UTC")
      insertAudit env workspace.id "observation" entity "create" Nothing snapshot (utc "2020-01-06 00:00:00 UTC")
      insertAudit env workspace.id "observation" entity "delete" snapshot Nothing (utc "2020-01-07 00:00:00 UTC")
      let assertGap = do
            historical <- listWorkspaceTimelineBuckets env.pool workspace.id (utc "2020-01-04 00:00:00 UTC") (utc "2020-01-04 12:00:00 UTC") "day"
            map (.timelineBucketObservationTotal) historical `shouldBe` [Nothing]
            map (.timelineBucketObservationStatus) historical `shouldBe` ["history_unavailable"]
            map (.timelineBucketOpenCounts) historical `shouldBe` [Just (TimelineOpenCounts 0 0)]
      assertGap
      mutateAudit env workspace.id "DELETE FROM audit_log WHERE workspace_id = $1 AND changed_at = '2020-01-01 00:00:00+00'::timestamptz AND entity_type = 'observation'"
      assertGap

    it "supports legacy snapshot attribution and isolates observation coverage from project gaps" $ \env -> do
      workspace <- createTestWorkspace env "timeline-observation-legacy"
      other <- createTestWorkspace env "timeline-observation-legacy-other"
      _ <- stockObservation env workspace.id
      _ <- stockObservation env other.id
      retimePending env workspace.id (utc "2020-01-01 00:00:00 UTC")
      retimePending env other.id (utc "2020-01-01 00:00:00 UTC")
      mutateAudit env workspace.id "UPDATE audit_log SET workspace_id = NULL WHERE workspace_id = $1 AND entity_type = 'observation'"
      _ <- stockTask env workspace.id Nothing Nothing
      mutateAudit env workspace.id "DELETE FROM audit_log WHERE workspace_id = $1 AND entity_type = 'task'"
      historical <- listWorkspaceTimelineBuckets env.pool workspace.id (utc "2020-01-02 00:00:00 UTC") (utc "2020-01-03 00:00:00 UTC") "day"
      map (.timelineBucketObservationTotal) historical `shouldBe` [Just 1]
      map (.timelineBucketOpenCounts) historical `shouldBe` [Nothing]
      isolated <- listWorkspaceTimelineBuckets env.pool other.id (utc "2020-01-02 00:00:00 UTC") (utc "2020-01-03 00:00:00 UTC") "day"
      map (.timelineBucketObservationTotal) isolated `shouldBe` [Just 1]

    it "returns exact empty observation stock and no future forecast" $ \env -> do
      workspace <- createTestWorkspace env "timeline-observation-empty"
      historical <- listWorkspaceTimelineBuckets env.pool workspace.id (utc "2020-01-02 00:00:00 UTC") (utc "2020-01-03 00:00:00 UTC") "day"
      map (.timelineBucketObservationTotal) historical `shouldBe` [Just 0]
      map (.timelineBucketObservationStatus) historical `shouldBe` ["available"]
      now <- getCurrentTime
      future <- listWorkspaceTimelineBuckets env.pool workspace.id (addUTCTime 172800 now) (addUTCTime 259200 now) "day"
      map (.timelineBucketObservationTotal) future `shouldSatisfy` all (== Nothing)
      map (.timelineBucketObservationAsOf) future `shouldSatisfy` all (== Nothing)
      map (.timelineBucketObservationStatus) future `shouldSatisfy` all (== "future")

    it "carries pre-range inventory through empty buckets and reverses exact-boundary transitions" $ \env -> do
      workspace <- createTestWorkspace env "timeline-stock"
      root <- stockProject env workspace.id Nothing
      nested <- stockProject env workspace.id (Just root.id)
      parent <- stockTask env workspace.id (Just root.id) Nothing
      child <- stockTask env workspace.id (Just root.id) (Just parent.id)
      unassigned <- stockTask env workspace.id Nothing Nothing
      retimePending env workspace.id (utc "2020-01-01 00:00:00 UTC")
      setTaskStatus env child.id Done
      setTaskStatus env parent.id Done
      setTaskStatus env unassigned.id Cancelled
      setProjectStatus env nested.id ProjArchived
      retimePending env workspace.id (utc "2020-01-03 00:00:00 UTC")
      buckets <- listWorkspaceTimelineBuckets env.pool workspace.id (utc "2020-01-02 00:00:00 UTC") (utc "2020-01-05 00:00:00 UTC") "day"
      map (.timelineBucketOpenCounts) buckets `shouldBe` map Just [TimelineOpenCounts 2 3, TimelineOpenCounts 1 0, TimelineOpenCounts 1 0]
      map (.timelineBucketOpenStatus) buckets `shouldBe` replicate 3 "available"
      map (.timelineBucketOpenAsOf) buckets `shouldBe` map (Just . utc) ["2020-01-03 00:00:00 UTC", "2020-01-04 00:00:00 UTC", "2020-01-05 00:00:00 UTC"]
      setTaskStatus env unassigned.id Todo
      retimePending env workspace.id (utc "2020-01-05 00:00:00 UTC")
      deleteProjectCascade env.pool root.id >>= (`shouldSatisfy` (/= Nothing))
      retimePending env workspace.id (utc "2020-01-06 00:00:00 UTC")
      restoreProject env.pool root.id `shouldReturn` True
      retimePending env workspace.id (utc "2020-01-07 00:00:00 UTC")
      restored <- listWorkspaceTimelineBuckets env.pool workspace.id (utc "2020-01-05 00:00:00 UTC") (utc "2020-01-08 00:00:00 UTC") "day"
      map (.timelineBucketOpenCounts) restored `shouldBe` map Just [TimelineOpenCounts 1 1, TimelineOpenCounts 0 1, TimelineOpenCounts 1 1]

    it "preserves stock across pause, automatic blocking, metadata and same-workspace moves" $ \env -> do
      workspace <- createTestWorkspace env "timeline-stock-invariance"
      root <- stockProject env workspace.id Nothing
      nested <- stockProject env workspace.id (Just root.id)
      task <- stockTask env workspace.id (Just nested.id) Nothing
      retimePending env workspace.id (utc "2020-01-01 00:00:00 UTC")
      setProjectStatus env root.id ProjPaused
      retimePending env workspace.id (utc "2020-01-02 00:00:00 UTC")
      setProjectStatus env root.id ProjActive
      _ <- updateProject env.pool nested.id UpdateProject
        { name = Just "renamed", description = Unchanged, parentId = SetNull, status = Nothing, priority = Nothing, metadata = Nothing }
      _ <- Task.updateTask env.pool task.id UpdateTask
        { title = Just "moved", description = Unchanged, projectId = SetNull, parentId = Unchanged, status = Nothing, priority = Nothing, metadata = Nothing }
      retimePending env workspace.id (utc "2020-01-03 00:00:00 UTC")
      buckets <- listWorkspaceTimelineBuckets env.pool workspace.id (utc "2020-01-01 00:00:00 UTC") (utc "2020-01-05 00:00:00 UTC") "day"
      map (.timelineBucketOpenCounts) buckets `shouldBe` replicate 4 (Just (TimelineOpenCounts 2 1))

    it "uses legacy snapshot workspace identity and isolates other workspaces" $ \env -> do
      workspace <- createTestWorkspace env "timeline-stock-legacy"
      other <- createTestWorkspace env "timeline-stock-other"
      _ <- stockProject env workspace.id Nothing
      _ <- stockTask env workspace.id Nothing Nothing
      _ <- stockProject env other.id Nothing
      retimePending env workspace.id (utc "2020-01-01 00:00:00 UTC")
      runSession env.pool $ Session.statement workspace.id $ Statement.Statement
        "UPDATE audit_log SET workspace_id = NULL WHERE workspace_id = $1 AND entity_type IN ('project','task')"
        (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True
      buckets <- listWorkspaceTimelineBuckets env.pool workspace.id (utc "2020-01-02 00:00:00 UTC") (utc "2020-01-04 00:00:00 UTC") "day"
      map (.timelineBucketOpenCounts) buckets `shouldBe` replicate 2 (Just (TimelineOpenCounts 1 1))

    it "counts actual cancellation and archive cascades and explicit reopening once per entity" $ \env -> do
      workspace <- createTestWorkspace env "timeline-stock-cascades"
      root <- stockProject env workspace.id Nothing
      nested <- stockProject env workspace.id (Just root.id)
      parent <- stockTask env workspace.id (Just nested.id) Nothing
      child <- stockTask env workspace.id (Just nested.id) (Just parent.id)
      retimePending env workspace.id (utc "2020-01-01 00:00:00 UTC")
      setTaskStatus env parent.id Cancelled
      retimePending env workspace.id (utc "2020-01-02 00:00:00 UTC")
      setTaskStatus env parent.id Todo
      setTaskStatus env child.id Todo
      retimePending env workspace.id (utc "2020-01-03 00:00:00 UTC")
      setProjectStatus env root.id ProjArchived
      retimePending env workspace.id (utc "2020-01-04 00:00:00 UTC")
      buckets <- listWorkspaceTimelineBuckets env.pool workspace.id (utc "2020-01-01 00:00:00 UTC") (utc "2020-01-05 00:00:00 UTC") "day"
      map (.timelineBucketOpenCounts) buckets `shouldBe` map Just [TimelineOpenCounts 2 2, TimelineOpenCounts 2 0, TimelineOpenCounts 2 2, TimelineOpenCounts 0 0]

    it "bounds missing-create coverage and detects a retained-history anchor mismatch" $ \env -> do
      workspace <- createTestWorkspace env "timeline-stock-coverage"
      task <- stockTask env workspace.id Nothing Nothing
      retimePending env workspace.id (utc "2020-01-01 00:00:00 UTC")
      setTaskStatus env task.id Done
      retimePending env workspace.id (utc "2020-01-02 00:00:00 UTC")
      mutateAudit env workspace.id "DELETE FROM audit_log WHERE workspace_id = $1 AND entity_type = 'task' AND action = 'create'"
      partial <- listWorkspaceTimelineBuckets env.pool workspace.id (utc "2020-01-01 00:00:00 UTC") (utc "2020-01-04 00:00:00 UTC") "day"
      map (.timelineBucketOpenStatus) partial `shouldBe` ["history_unavailable", "available", "available"]
      map (.timelineBucketOpenCounts) partial `shouldBe` [Nothing, Just (TimelineOpenCounts 0 0), Just (TimelineOpenCounts 0 0)]
      other <- createTestWorkspace env "timeline-stock-mismatch"
      otherTask <- stockTask env other.id Nothing Nothing
      retimePending env other.id (utc "2020-01-01 00:00:00 UTC")
      setTaskStatus env otherTask.id Done
      retimePending env other.id (utc "2020-01-02 00:00:00 UTC")
      mutateAudit env other.id "DELETE FROM audit_log WHERE workspace_id = $1 AND entity_type = 'task' AND action = 'update'"
      mismatch <- listWorkspaceTimelineBuckets env.pool other.id (utc "2020-01-03 00:00:00 UTC") (utc "2020-01-04 00:00:00 UTC") "day"
      map (.timelineBucketOpenStatus) mismatch `shouldBe` ["history_unavailable"]
      map (.timelineBucketOpenCounts) mismatch `shouldBe` [Nothing]

    it "returns gaps for missing or unusable history while keeping the current anchor exact" $ \env -> do
      workspace <- createTestWorkspace env "timeline-stock-missing"
      _ <- stockTask env workspace.id Nothing Nothing
      retimePending env workspace.id (utc "2020-01-01 00:00:00 UTC")
      runSession env.pool $ Session.statement workspace.id $ Statement.Statement
        "DELETE FROM audit_log WHERE workspace_id = $1 AND entity_type = 'task'"
        (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True
      missing <- listWorkspaceTimelineBuckets env.pool workspace.id (utc "2020-01-01 00:00:00 UTC") (utc "2020-01-03 00:00:00 UTC") "day"
      map (.timelineBucketOpenCounts) missing `shouldBe` [Nothing, Nothing]
      map (.timelineBucketOpenStatus) missing `shouldBe` replicate 2 "history_unavailable"
      now <- getCurrentTime
      current <- listWorkspaceTimelineBuckets env.pool workspace.id (addUTCTime (-1) now) (addUTCTime 1 now) "day"
      map (.timelineBucketOpenCounts) current `shouldBe` [Just (TimelineOpenCounts 0 1)]
      Just canonical <- getWorkspacePlanningCounts env.pool workspace.id
      canonical `shouldBe` WorkspacePlanningCounts 0 1
      case current of
        [bucket] -> case bucket.timelineBucketOpenAsOf of
          Just sample -> do
            sample `shouldSatisfy` (>= now)
            sample `shouldSatisfy` (< addUTCTime 1 now)
          Nothing -> expectationFailure "current inventory must carry a cutoff"
        _ -> expectationFailure "expected one current bucket"
      malformedWorkspace <- createTestWorkspace env "timeline-stock-malformed"
      _ <- stockTask env malformedWorkspace.id Nothing Nothing
      retimePending env malformedWorkspace.id (utc "2020-01-02 00:00:00 UTC")
      mutateAudit env malformedWorkspace.id "UPDATE audit_log SET new_values = new_values - 'status' WHERE workspace_id = $1 AND entity_type = 'task'"
      malformed <- listWorkspaceTimelineBuckets env.pool malformedWorkspace.id (utc "2020-01-01 00:00:00 UTC") (utc "2020-01-02 00:00:00 UTC") "day"
      map (.timelineBucketOpenCounts) malformed `shouldBe` [Nothing]
      map (.timelineBucketOpenStatus) malformed `shouldBe` ["history_unavailable"]

    it "rejects no-create history whose retained endpoint disagrees with the current row" $ \env -> do
      workspace <- createTestWorkspace env "timeline-stock-truncated-endpoint"
      task <- stockTask env workspace.id Nothing Nothing
      retimePending env workspace.id (utc "2020-01-01 00:00:00 UTC")
      setTaskStatus env task.id Done
      retimePending env workspace.id (utc "2020-01-02 00:00:00 UTC")
      setTaskStatus env task.id Todo
      retimePending env workspace.id (utc "2020-01-03 00:00:00 UTC")
      mutateAudit env workspace.id "DELETE FROM audit_log WHERE workspace_id = $1 AND entity_type = 'task' AND (action = 'create' OR new_values ->> 'status' = 'todo')"
      historical <- listWorkspaceTimelineBuckets env.pool workspace.id (utc "2020-01-02 00:00:00 UTC") (utc "2020-01-02 12:00:00 UTC") "day"
      map (.timelineBucketOpenCounts) historical `shouldBe` [Nothing]
      map (.timelineBucketOpenStatus) historical `shouldBe` ["history_unavailable"]
      now <- getCurrentTime
      current <- listWorkspaceTimelineBuckets env.pool workspace.id (addUTCTime (-1) now) (addUTCTime 1 now) "day"
      map (.timelineBucketOpenCounts) current `shouldBe` [Just (TimelineOpenCounts 0 1)]

    it "uses the requested partial cutoff for every bucket size and does not project future stock" $ \env -> do
      workspace <- createTestWorkspace env "timeline-stock-cutoffs"
      _ <- stockTask env workspace.id Nothing Nothing
      retimePending env workspace.id (utc "2020-01-01 00:00:00 UTC")
      let since = utc "2020-01-02 00:00:00 UTC"
          untilTime = utc "2020-01-02 12:00:00 UTC"
      mapM_ (\size -> do
        buckets <- listWorkspaceTimelineBuckets env.pool workspace.id since untilTime size
        map (.timelineBucketOpenCounts) buckets `shouldBe` [Just (TimelineOpenCounts 0 1)]
        map (.timelineBucketOpenAsOf) buckets `shouldBe` [Just untilTime]) ["day", "week", "month", "quarter"]
      now <- getCurrentTime
      future <- listWorkspaceTimelineBuckets env.pool workspace.id (addUTCTime 172800 now) (addUTCTime 259200 now) "day"
      map (.timelineBucketOpenStatus) future `shouldSatisfy` all (== "future")
      map (.timelineBucketOpenCounts) future `shouldSatisfy` all (== Nothing)
      map (.timelineBucketOpenAsOf) future `shouldSatisfy` all (== Nothing)

    it "rejects an internal gap in no-create history even when its latest endpoint matches" $ \env -> do
      workspace <- createTestWorkspace env "timeline-stock-internal-gap"
      task <- stockTask env workspace.id Nothing Nothing
      retimePending env workspace.id (utc "2020-01-01 00:00:00 UTC")
      setTaskStatus env task.id Done
      retimePending env workspace.id (utc "2020-01-02 00:00:00 UTC")
      setTaskStatus env task.id Todo
      retimePending env workspace.id (utc "2020-01-03 00:00:00 UTC")
      setTaskStatus env task.id Done
      retimePending env workspace.id (utc "2020-01-04 00:00:00 UTC")
      mutateAudit env workspace.id "DELETE FROM audit_log WHERE workspace_id = $1 AND entity_type = 'task' AND (action = 'create' OR new_values ->> 'status' = 'todo')"
      historical <- listWorkspaceTimelineBuckets env.pool workspace.id (utc "2020-01-03 00:00:00 UTC") (utc "2020-01-03 12:00:00 UTC") "day"
      map (.timelineBucketOpenCounts) historical `shouldBe` [Nothing]
      map (.timelineBucketOpenStatus) historical `shouldBe` ["history_unavailable"]

    it "reconciles complete no-create histories with unordered transitions at the same timestamp" $ \env -> do
      workspace <- createTestWorkspace env "timeline-stock-tied-history"
      task <- stockTask env workspace.id Nothing Nothing
      setTaskStatus env task.id Done
      setTaskStatus env task.id Todo
      setTaskStatus env task.id Done
      retimePending env workspace.id (utc "2020-01-02 00:00:00 UTC")
      mutateAudit env workspace.id "DELETE FROM audit_log WHERE workspace_id = $1 AND entity_type = 'task' AND action = 'create'"
      buckets <- listWorkspaceTimelineBuckets env.pool workspace.id (utc "2020-01-01 00:00:00 UTC") (utc "2020-01-04 00:00:00 UTC") "day"
      map (.timelineBucketOpenStatus) buckets `shouldBe` ["history_unavailable", "available", "available"]
      map (.timelineBucketOpenCounts) buckets `shouldBe` [Nothing, Just (TimelineOpenCounts 0 0), Just (TimelineOpenCounts 0 0)]

    it "rejects net-cancelling gaps at retained internal timestamp boundaries" $ \env -> do
      workspace <- createTestWorkspace env "timeline-stock-balanced-gap"
      task <- stockTask env workspace.id Nothing Nothing
      retimePending env workspace.id (utc "2020-01-01 00:00:00 UTC")
      setTaskStatus env task.id Done
      retimePending env workspace.id (utc "2020-01-02 00:00:00 UTC")
      setTaskStatus env task.id Todo
      retimePending env workspace.id (utc "2020-01-03 00:00:00 UTC")
      _ <- Task.updateTask env.pool task.id UpdateTask
        { title = Just "metadata while open", description = Unchanged, projectId = Unchanged, parentId = Unchanged, status = Nothing, priority = Nothing, metadata = Nothing }
      retimePending env workspace.id (utc "2020-01-04 00:00:00 UTC")
      setTaskStatus env task.id Done
      retimePending env workspace.id (utc "2020-01-05 00:00:00 UTC")
      setTaskStatus env task.id Todo
      retimePending env workspace.id (utc "2020-01-06 00:00:00 UTC")
      mutateAudit env workspace.id "DELETE FROM audit_log WHERE workspace_id = $1 AND entity_type = 'task' AND (action = 'create' OR changed_at IN ('2020-01-03 00:00:00+00'::timestamptz, '2020-01-05 00:00:00+00'::timestamptz))"
      historical <- listWorkspaceTimelineBuckets env.pool workspace.id (utc "2020-01-04 00:00:00 UTC") (utc "2020-01-04 12:00:00 UTC") "day"
      map (.timelineBucketOpenCounts) historical `shouldBe` [Nothing]
      map (.timelineBucketOpenStatus) historical `shouldBe` ["history_unavailable"]

    it "emits canonical lifecycle series without changing legacy histogram totals" $ \env -> do
      workspace <- createTestWorkspace env "timeline-series"
      rangeStart <- addUTCTime (-1) <$> getCurrentTime
      root <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Nothing, name = "root", description = Nothing, priority = Nothing, metadata = Nothing }
      nested <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Just root.id, name = "nested", description = Nothing, priority = Nothing, metadata = Nothing }
      rootTask <- Task.createTask env.pool CreateTask
        { workspaceId = workspace.id, projectId = Just root.id, parentId = Nothing, title = "root task", description = Nothing, priority = Nothing, metadata = Nothing }
      subtask <- Task.createTask env.pool CreateTask
        { workspaceId = workspace.id, projectId = Just root.id, parentId = Just rootTask.id, title = "subtask", description = Nothing, priority = Nothing, metadata = Nothing }
      observation <- createObservation env.pool (CreateObservation workspace.id [ObservationSubject SubjectFile "src/Timeline.hs"] "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "timeline")
      _ <- updateProject env.pool nested.id UpdateProject
        { name = Nothing, description = Unchanged, parentId = Unchanged, status = Just ProjArchived, priority = Nothing, metadata = Nothing }
      _ <- Task.updateTask env.pool subtask.id UpdateTask
        { title = Nothing, description = Unchanged, projectId = Unchanged, parentId = Unchanged, status = Just Done, priority = Nothing, metadata = Nothing }
      _ <- Task.updateTask env.pool rootTask.id UpdateTask
        { title = Nothing, description = Unchanged, projectId = Unchanged, parentId = Unchanged, status = Just Done, priority = Nothing, metadata = Nothing }
      _ <- updateProject env.pool root.id UpdateProject
        { name = Nothing, description = Unchanged, parentId = Unchanged, status = Just ProjCompleted, priority = Nothing, metadata = Nothing }
      deleteObservation env.pool workspace.id observation.id `shouldReturn` True
      deleteProjectCascade env.pool root.id >>= (`shouldSatisfy` (/= Nothing))
      rangeEnd <- addUTCTime 1 <$> getCurrentTime
      buckets <- listWorkspaceTimelineBuckets env.pool workspace.id rangeStart rangeEnd "day"
      let total get = sum (map get buckets)
          sumSeries get = foldr addSeries (TimelineBucketSeriesCounts 0 0 0 0 0) (map get buckets)
          project = sumSeries (\bucket -> bucket.timelineBucketSeries.seriesProject)
          task = sumSeries (\bucket -> bucket.timelineBucketSeries.seriesTask)
          child = sumSeries (\bucket -> bucket.timelineBucketSeries.seriesSubtask)
          observationSeries = sumSeries (\bucket -> bucket.timelineBucketSeries.seriesObservation)
          seriesTotals = sumSeries (.timelineBucketSeriesTotals)
      project.created `shouldBe` 2
      project.completed `shouldBe` 1
      project.deleted `shouldBe` 2
      project.archived `shouldBe` 1
      task.created `shouldBe` 1
      task.completed `shouldBe` 1
      task.deleted `shouldBe` 1
      child.created `shouldBe` 1
      child.completed `shouldBe` 1
      child.deleted `shouldBe` 1
      observationSeries.created `shouldBe` 1
      observationSeries.completed `shouldBe` 0
      observationSeries.deleted `shouldBe` 1
      observationSeries.archived `shouldBe` 0
      observationSeries.cancelled `shouldBe` 0
      seriesTotals.created `shouldBe` 5
      seriesTotals.completed `shouldBe` 3
      seriesTotals.deleted `shouldBe` 5
      total (\bucket -> bucket.timelineBucketCounts.subprojectCounts.created) `shouldBe` 1
      total (\bucket -> bucket.timelineBucketTotals.created) `shouldBe` 4

    it "zero-fills a future half-open range in chronological order" $ \env -> do
      workspace <- createTestWorkspace env "timeline-zero-fill"
      now <- getCurrentTime
      let since = UTCTime (addDays 1 (utctDay now)) 0
          untilTime = addUTCTime (3 * 86400) since
      buckets <- listWorkspaceTimelineBuckets env.pool workspace.id since untilTime "day"
      length buckets `shouldBe` 3
      map (.timelineBucketStart) buckets `shouldBe` sort (map (.timelineBucketStart) buckets)
      map (.timelineBucketSeriesTotals) buckets `shouldBe` replicate 3 (TimelineBucketSeriesCounts 0 0 0 0 0)

    it "uses audit-time ownership and half-open UTC ranges for canonical series" $ \env -> do
      workspace <- createTestWorkspace env "timeline-deterministic"
      otherWorkspace <- createTestWorkspace env "timeline-isolated"
      let since = utc "2024-01-01 00:00:00 UTC"
          untilTime = utc "2024-01-03 00:00:00 UTC"
          inRange = utc "2024-01-01 00:00:00 UTC"
          endBoundary = utc "2024-01-03 00:00:00 UTC"
          rootParent = "00000000-0000-0000-0000-000000000010" :: Text
          newParent = "00000000-0000-0000-0000-000000000011" :: Text
          payload fields = Aeson.object fields
      insertAudit env workspace.id "project" "00000000-0000-0000-0000-000000000101" "create" Nothing (Just (payload [])) inRange
      insertAudit env workspace.id "project" "00000000-0000-0000-0000-000000000102" "update"
        (Just (payload ["status" Aeson..= ("active" :: Text), "parent_id" Aeson..= rootParent]))
        (Just (payload ["status" Aeson..= ("completed" :: Text), "parent_id" Aeson..= rootParent])) inRange
      insertAudit env workspace.id "project" "00000000-0000-0000-0000-000000000103" "update"
        (Just (payload ["deleted_at" Aeson..= Aeson.Null]))
        (Just (payload ["deleted_at" Aeson..= ("2024-01-01T01:00:00Z" :: Text)])) inRange
      insertAudit env workspace.id "task" "00000000-0000-0000-0000-000000000201" "create" Nothing (Just (payload ["parent_id" Aeson..= Aeson.Null])) inRange
      insertAudit env workspace.id "task" "00000000-0000-0000-0000-000000000202" "create" Nothing (Just (payload ["parent_id" Aeson..= newParent])) inRange
      insertAudit env workspace.id "task" "00000000-0000-0000-0000-000000000203" "update"
        (Just (payload ["status" Aeson..= ("todo" :: Text), "parent_id" Aeson..= newParent]))
        (Just (payload ["status" Aeson..= ("done" :: Text), "parent_id" Aeson..= newParent])) inRange
      insertAudit env workspace.id "task" "00000000-0000-0000-0000-000000000204" "update"
        (Just (payload ["parent_id" Aeson..= rootParent, "deleted_at" Aeson..= Aeson.Null]))
        (Just (payload ["parent_id" Aeson..= newParent, "deleted_at" Aeson..= ("2024-01-01T01:00:00Z" :: Text)])) inRange
      insertAudit env workspace.id "observation" "00000000-0000-0000-0000-000000000301" "create" Nothing (Just (payload [])) inRange
      insertAudit env workspace.id "observation" "00000000-0000-0000-0000-000000000302" "delete" (Just (payload ["subject" Aeson..= ("src/Deleted.hs" :: Text)])) Nothing inRange
      insertAudit env workspace.id "task" "00000000-0000-0000-0000-000000000205" "update"
        (Just (payload ["deleted_at" Aeson..= ("2024-01-01T01:00:00Z" :: Text)]))
        (Just (payload ["deleted_at" Aeson..= Aeson.Null])) inRange
      insertAudit env workspace.id "task" "00000000-0000-0000-0000-000000000206" "update"
        (Just (payload ["title" Aeson..= ("before" :: Text)]))
        (Just (payload ["title" Aeson..= ("after" :: Text)])) inRange
      insertAudit env workspace.id "task" "00000000-0000-0000-0000-000000000207" "update"
        (Just (payload ["status" Aeson..= ("todo" :: Text)]))
        (Just (payload ["status" Aeson..= ("cancelled" :: Text)])) inRange
      insertAudit env workspace.id "task" "00000000-0000-0000-0000-000000000208" "update"
        (Just (payload ["status" Aeson..= ("todo" :: Text), "parent_id" Aeson..= rootParent]))
        (Just (payload ["status" Aeson..= ("cancelled" :: Text), "parent_id" Aeson..= rootParent])) inRange
      insertAudit env otherWorkspace.id "observation" "00000000-0000-0000-0000-000000000399" "create" Nothing (Just (payload [])) inRange
      insertAudit env workspace.id "observation" "00000000-0000-0000-0000-000000000303" "create" Nothing (Just (payload [])) endBoundary
      earliestWorkspaceLifecycle env.pool workspace.id untilTime `shouldReturn` Just inRange
      earliestWorkspaceLifecycle env.pool workspace.id inRange `shouldReturn` Nothing
      buckets <- listWorkspaceTimelineBuckets env.pool workspace.id since untilTime "day"
      let series = foldr addTimelineSeries emptyTimelineSeries (map (.timelineBucketSeries) buckets)
      series.seriesProject `shouldBe` TimelineBucketSeriesCounts 1 1 1 0 0
      series.seriesTask `shouldBe` TimelineBucketSeriesCounts 1 0 0 0 1
      series.seriesSubtask `shouldBe` TimelineBucketSeriesCounts 1 1 1 0 1
      series.seriesObservation `shouldBe` TimelineBucketSeriesCounts 1 0 1 0 0

    it "uses exact UTC edges for every bucket size" $ \env -> do
      workspace <- createTestWorkspace env "timeline-edges"
      assertEdges env workspace.id "day" (utc "2024-01-01 12:00:00 UTC") (utc "2024-01-03 00:00:00 UTC")
        [utc "2024-01-01 00:00:00 UTC", utc "2024-01-02 00:00:00 UTC"]
      assertEdges env workspace.id "week" (utc "2024-01-02 00:00:00 UTC") (utc "2024-01-16 00:00:00 UTC")
        [utc "2024-01-01 00:00:00 UTC", utc "2024-01-08 00:00:00 UTC", utc "2024-01-15 00:00:00 UTC"]
      assertEdges env workspace.id "month" (utc "2024-01-15 00:00:00 UTC") (utc "2024-03-01 00:00:00 UTC")
        [utc "2024-01-01 00:00:00 UTC", utc "2024-02-01 00:00:00 UTC"]
      assertEdges env workspace.id "quarter" (utc "2024-01-15 00:00:00 UTC") (utc "2024-07-01 00:00:00 UTC")
        [utc "2024-01-01 00:00:00 UTC", utc "2024-04-01 00:00:00 UTC"]

addSeries :: TimelineBucketSeriesCounts -> TimelineBucketSeriesCounts -> TimelineBucketSeriesCounts
addSeries left right = TimelineBucketSeriesCounts
  { created = left.created + right.created
  , completed = left.completed + right.completed
  , deleted = left.deleted + right.deleted
  , archived = left.archived + right.archived
  , cancelled = left.cancelled + right.cancelled
  }

emptyTimelineSeries :: TimelineBucketSeries
emptyTimelineSeries = TimelineBucketSeries zero zero zero zero
  where zero = TimelineBucketSeriesCounts 0 0 0 0 0

addTimelineSeries :: TimelineBucketSeries -> TimelineBucketSeries -> TimelineBucketSeries
addTimelineSeries left right = TimelineBucketSeries
  { seriesProject = addSeries left.seriesProject right.seriesProject
  , seriesTask = addSeries left.seriesTask right.seriesTask
  , seriesSubtask = addSeries left.seriesSubtask right.seriesSubtask
  , seriesObservation = addSeries left.seriesObservation right.seriesObservation
  }

assertEdges :: TestEnv -> UUID -> Text -> UTCTime -> UTCTime -> [UTCTime] -> Expectation
assertEdges env workspace bucket since untilTime expected = do
  buckets <- listWorkspaceTimelineBuckets env.pool workspace since untilTime bucket
  map (.timelineBucketStart) buckets `shouldBe` expected

utc :: String -> UTCTime
utc = read

stockProject :: TestEnv -> UUID -> Maybe UUID -> IO Project
stockProject env workspace parent = createProject env.pool CreateProject
  { workspaceId = workspace, parentId = parent, name = "stock project", description = Nothing, priority = Nothing, metadata = Nothing }

stockObservation :: TestEnv -> UUID -> IO Observation
stockObservation env workspace = createObservation env.pool (CreateObservation workspace
  [ObservationSubject SubjectFile "src/Timeline.hs", ObservationSubject SubjectGlob "src/**/*.hs"]
  "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "timeline inventory")

stockTask :: TestEnv -> UUID -> Maybe UUID -> Maybe UUID -> IO Task
stockTask env workspace project parent = Task.createTask env.pool CreateTask
  { workspaceId = workspace, projectId = project, parentId = parent, title = "stock task", description = Nothing, priority = Nothing, metadata = Nothing }

setTaskStatus :: TestEnv -> UUID -> TaskStatus -> IO ()
setTaskStatus env task state = do
  _ <- Task.updateTask env.pool task UpdateTask
    { title = Nothing, description = Unchanged, projectId = Unchanged, parentId = Unchanged, status = Just state, priority = Nothing, metadata = Nothing }
  pure ()

setProjectStatus :: TestEnv -> UUID -> ProjectStatus -> IO ()
setProjectStatus env project state = do
  _ <- updateProject env.pool project UpdateProject
    { name = Nothing, description = Unchanged, parentId = Unchanged, status = Just state, priority = Nothing, metadata = Nothing }
  pure ()

-- Only this transaction's workspace audit rows are retimed. Earlier phases
-- already have 2020 timestamps, so each phase preserves all per-entity snapshots.
retimePending :: TestEnv -> UUID -> UTCTime -> IO ()
retimePending env workspace timestamp = runSession env.pool $ Session.statement (workspace, timestamp) $ Statement.Statement
  "UPDATE audit_log SET changed_at = $2 WHERE workspace_id = $1 AND changed_at >= statement_timestamp() - interval '1 day'"
  (contramap fst (Enc.param (Enc.nonNullable Enc.uuid)) <> contramap snd (Enc.param (Enc.nonNullable Enc.timestamptz))) Dec.noResult True

mutateAudit :: TestEnv -> UUID -> BS8.ByteString -> IO ()
mutateAudit env workspace sql = runSession env.pool $ Session.statement workspace $ Statement.Statement sql
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

insertAudit :: TestEnv -> UUID -> Text -> Text -> Text -> Maybe Aeson.Value -> Maybe Aeson.Value -> UTCTime -> IO ()
insertAudit env workspace entityType entityId action oldValues newValues changedAt = runSession env.pool $
  Session.statement (workspace, entityType, entityId, action, oldValues, newValues, changedAt) insertAuditStatement

insertAuditStatement :: Statement.Statement (UUID, Text, Text, Text, Maybe Aeson.Value, Maybe Aeson.Value, UTCTime) ()
insertAuditStatement = Statement.Statement
  "INSERT INTO audit_log (workspace_id, entity_type, entity_id, action, old_values, new_values, changed_at) VALUES ($1, $2, $3, $4::audit_action_enum, $5, $6, $7)"
  ((contramap (\(a,_,_,_,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid))) <>
   (contramap (\(_,b,_,_,_,_,_) -> b) (Enc.param (Enc.nonNullable Enc.text))) <>
   (contramap (\(_,_,c,_,_,_,_) -> c) (Enc.param (Enc.nonNullable Enc.text))) <>
   (contramap (\(_,_,_,d,_,_,_) -> d) (Enc.param (Enc.nonNullable Enc.text))) <>
   (contramap (\(_,_,_,_,e,_,_) -> e) (Enc.param (Enc.nullable Enc.jsonb))) <>
   (contramap (\(_,_,_,_,_,f,_) -> f) (Enc.param (Enc.nullable Enc.jsonb))) <>
   (contramap (\(_,_,_,_,_,_,g) -> g) (Enc.param (Enc.nonNullable Enc.timestamptz))))
  Dec.noResult
  True
