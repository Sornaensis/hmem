module HMem.DB.TimelineSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS8
import Data.Functor.Contravariant (contramap)
import Data.List (sort)
import Data.Text (Text)
import Data.Time (UTCTime(..), addDays, addUTCTime, diffUTCTime, getCurrentTime, utctDay)
import Data.Pool (Pool)
import Data.Text qualified as T
import Hasql.Connection qualified as Hasql
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
import HMem.DB.Timeline hiding (listWorkspaceTimelineBuckets)
import HMem.DB.Timeline qualified as Timeline
import HMem.DB.Workspace (getWorkspacePlanningCounts)
import HMem.Types

spec :: Spec
spec = beforeAll setupTestPool $ aroundWith withTestTransaction $
  describe "Timeline buckets" $ do
    it "plans bucket inventory without whole-audit sequential scans or per-bucket range joins" $ \env -> do
      workspace <- createTestWorkspace env "timeline-plan"
      _ <- stockProject env workspace.id Nothing
      retimePending env workspace.id (utc "2020-01-01 00:00:00 UTC")
      mutateAudit env workspace.id "UPDATE audit_log SET workspace_id = NULL WHERE workspace_id = $1 AND entity_type = 'project'"
      plans <- mapM (\mode -> explainBuckets env mode workspace.id) ["force_generic_plan", "force_custom_plan"]
      mapM_ (\plan -> do
        filter (T.isInfixOf "Seq Scan on audit_log") plan `shouldBe` []
        -- Buckets and inventory samples join by key, never by time range.
        filter (\line -> T.isInfixOf "Join Filter" line && T.isInfixOf ">=" line) plan `shouldBe` []) plans
      -- The workspace stays correct with its legacy rows.
      _ <- listWorkspaceTimelineBuckets env.pool workspace.id (utc "2019-12-30 00:00:00 UTC") (utc "2020-01-06 00:00:00 UTC") "week"
      pure ()

    it "matches the pre-repair statement across bucket sizes and range boundaries" $ \env -> do
      workspace <- createTestWorkspace env "timeline-equivalence"
      root <- stockProject env workspace.id Nothing
      nested <- stockProject env workspace.id (Just root.id)
      parent <- stockTask env workspace.id (Just root.id) Nothing
      child <- stockTask env workspace.id (Just root.id) (Just parent.id)
      loose <- stockTask env workspace.id Nothing Nothing
      firstObservation <- stockObservation env workspace.id
      _ <- stockObservation env workspace.id
      retimePending env workspace.id (utc "2020-01-30 10:00:00 UTC")
      setTaskStatus env child.id Done
      setTaskStatus env loose.id Cancelled
      setProjectStatus env root.id ProjPaused
      retimePending env workspace.id (utc "2020-02-03 00:00:00 UTC")
      _ <- Task.updateTask env.pool child.id UpdateTask
        { title = Nothing, description = Unchanged, projectId = Unchanged, parentId = SetNull, status = Nothing, priority = Nothing, metadata = Nothing }
      setProjectStatus env root.id ProjActive
      deleteObservation env.pool workspace.id firstObservation.id `shouldReturn` True
      retimePending env workspace.id (utc "2020-03-31 23:59:59 UTC")
      deleteProjectCascade env.pool nested.id >>= (`shouldSatisfy` (/= Nothing))
      retimePending env workspace.id (utc "2020-04-01 00:00:00 UTC")
      restoreProject env.pool nested.id `shouldReturn` True
      setTaskStatus env loose.id Todo
      retimePending env workspace.id (utc "2020-07-15 12:00:00 UTC")
      mutateAudit env workspace.id "UPDATE audit_log SET workspace_id = NULL WHERE workspace_id = $1 AND entity_type = 'observation'"
      mapM_ (\(size, since, untilTime) -> listWorkspaceTimelineBuckets env.pool workspace.id (utc since) (utc untilTime) size)
        [ ("day", "2020-01-29 00:00:00 UTC", "2020-02-05 00:00:00 UTC")
        , ("day", "2020-03-31 12:00:00 UTC", "2020-04-02 06:00:00 UTC")
        , ("week", "2020-01-01 00:00:00 UTC", "2020-08-01 00:00:00 UTC")
        , ("month", "2019-11-15 00:00:00 UTC", "2020-09-01 00:00:00 UTC")
        , ("quarter", "2019-10-01 00:00:00 UTC", "2021-01-01 00:00:00 UTC")
        , ("quarter", "2020-02-03 00:00:00 UTC", "2020-04-01 00:00:00 UTC")
        , ("day", "2019-01-01 00:00:00 UTC", "2020-03-01 00:00:00 UTC") ]

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

-- The pre-repair bucket statement, kept verbatim as an equivalence oracle.
oracleBucketsStatement :: Statement.Statement (UUID, UTCTime, UTCTime, Text) [WorkspaceTimelineBucket]
oracleBucketsStatement = Statement.Statement sql encoder (Dec.rowList timelineBucketRowDecoder) True
  where
    sql = BS8.pack $ unlines $
      [ "WITH bounds AS ("
      , "  SELECT"
      , "    date_trunc($4, $2 AT TIME ZONE 'UTC') AS series_start,"
      , "    date_trunc($4, ($3 - interval '1 microsecond') AT TIME ZONE 'UTC') AS series_end,"
      , "    CASE $4"
      , "      WHEN 'day' THEN interval '1 day'"
      , "      WHEN 'week' THEN interval '1 week'"
      , "      WHEN 'month' THEN interval '1 month'"
      , "      WHEN 'quarter' THEN interval '3 months'"
      , "    END AS step"
      , "), buckets AS ("
      , "  SELECT"
      , "    gs.bucket_start AS bucket_start_utc,"
      , "    gs.bucket_start + bounds.step AS bucket_end_utc"
      , "  FROM bounds"
      , "  CROSS JOIN LATERAL generate_series(bounds.series_start, bounds.series_end, bounds.step) AS gs(bucket_start)"
      , ")"
      ] <> oracleInventoryCtes <>
      [ ", candidates AS ("
      , "  SELECT"
      , "    a.id AS audit_id,"
      , "    a.workspace_id AS workspace_id,"
      , "    a.entity_id::uuid AS entity_uuid,"
      , "    a.changed_at AS occurred_at,"
      , "    a.entity_type AS raw_entity_type,"
      , "    a.action::text AS action_text,"
      , "    NULLIF(a.old_values ->> 'status', '') AS old_status,"
      , "    NULLIF(a.new_values ->> 'status', '') AS new_status,"
      , "    NULLIF(a.old_values ->> 'deleted_at', '') AS old_deleted_at,"
      , "    NULLIF(a.new_values ->> 'deleted_at', '') AS new_deleted_at,"
      , "    NULLIF(CASE"
      , "      WHEN a.new_values ? 'parent_id' THEN a.new_values ->> 'parent_id'"
      , "      WHEN a.old_values ? 'parent_id' THEN a.old_values ->> 'parent_id'"
      , "      ELSE t.parent_id::text"
      , "    END, '')::uuid AS parent_task_uuid,"
      , "    NULLIF(CASE"
      , "      WHEN a.new_values ? 'parent_id' THEN a.new_values ->> 'parent_id'"
      , "      WHEN a.old_values ? 'parent_id' THEN a.old_values ->> 'parent_id'"
      , "      ELSE p.parent_id::text"
      , "    END, '')::uuid AS parent_project_uuid"
      , "  FROM audit_log a"
      , "  LEFT JOIN tasks t ON a.entity_type = 'task' AND t.id = a.entity_id::uuid"
      , "  LEFT JOIN projects p ON a.entity_type = 'project' AND p.id = a.entity_id::uuid"
      , "  WHERE a.workspace_id = $1"
      , "    AND a.entity_type IN ('project', 'task', 'observation')"
      , "    AND a.action::text IN ('create', 'update', 'delete')"
      , "    AND a.changed_at >= $2"
      , "    AND a.changed_at < $3"
      , "), events AS ("
      , "  SELECT"
      , "    c.occurred_at,"
      , "    CASE"
      , "      WHEN c.raw_entity_type = 'project' AND c.parent_project_uuid IS NOT NULL THEN 'subproject'"
      , "      WHEN c.raw_entity_type = 'project' THEN 'project'"
      , "      WHEN c.raw_entity_type = 'task' AND c.parent_task_uuid IS NOT NULL THEN 'subtask'"
      , "      ELSE 'task'"
      , "    END AS legacy_entity_kind,"
      , "    CASE"
      , "      WHEN c.raw_entity_type = 'project' AND c.action_text = 'create' THEN 'created'"
      , "      WHEN c.raw_entity_type = 'project' AND c.action_text = 'update' AND c.old_status IS NOT NULL AND c.new_status IN ('completed', 'archived') AND c.old_status <> c.new_status THEN 'completed'"
      , "      WHEN c.raw_entity_type = 'task' AND c.action_text = 'create' THEN 'created'"
      , "      WHEN c.raw_entity_type = 'task' AND c.action_text = 'update' AND c.old_status IS NOT NULL AND c.new_status = 'done' AND c.old_status <> c.new_status THEN 'completed'"
      , "      WHEN c.raw_entity_type = 'task' AND c.action_text = 'update' AND c.old_status IS NOT NULL AND c.new_status = 'cancelled' AND c.old_status <> c.new_status THEN 'cancelled'"
      , "      ELSE NULL"
      , "    END AS legacy_lifecycle_action,"
      , "    CASE"
      , "      WHEN c.raw_entity_type = 'project' THEN 'project'"
      , "      WHEN c.raw_entity_type = 'task' AND c.parent_task_uuid IS NOT NULL THEN 'subtask'"
      , "      WHEN c.raw_entity_type = 'task' THEN 'task'"
      , "      WHEN c.raw_entity_type = 'observation' THEN 'observation'"
      , "    END AS canonical_entity_kind,"
      , "    CASE"
      , "      WHEN c.action_text = 'create' THEN 'created'"
      , "      WHEN c.raw_entity_type = 'project' AND c.action_text = 'update' AND c.old_status IS DISTINCT FROM c.new_status AND c.new_status = 'completed' THEN 'completed'"
      , "      WHEN c.raw_entity_type = 'task' AND c.action_text = 'update' AND c.old_status IS DISTINCT FROM c.new_status AND c.new_status = 'done' THEN 'completed'"
      , "      WHEN c.raw_entity_type = 'project' AND c.action_text = 'update' AND c.old_status IS DISTINCT FROM c.new_status AND c.new_status = 'archived' THEN 'archived'"
      , "      WHEN c.raw_entity_type = 'task' AND c.action_text = 'update' AND c.old_status IS DISTINCT FROM c.new_status AND c.new_status = 'cancelled' THEN 'cancelled'"
      , "      WHEN c.action_text = 'delete' THEN 'deleted'"
      , "      WHEN c.raw_entity_type IN ('project', 'task') AND c.action_text = 'update' AND c.old_deleted_at IS NULL AND c.new_deleted_at IS NOT NULL THEN 'deleted'"
      , "      ELSE NULL"
      , "    END AS canonical_lifecycle_action"
      , "  FROM candidates c"
      , "), lifecycle_buckets AS ("
      , "SELECT"
      , "  b.bucket_start_utc AT TIME ZONE 'UTC' AS bucket_start,"
      , "  b.bucket_end_utc AT TIME ZONE 'UTC' AS bucket_end,"
      , "  CASE $4"
      , "    WHEN 'quarter' THEN ('Q' || EXTRACT(quarter FROM b.bucket_start_utc)::int::text || ' ' || to_char(b.bucket_start_utc, 'YYYY'))"
      , "    WHEN 'month' THEN to_char(b.bucket_start_utc, 'Mon YYYY')"
      , "    ELSE to_char(b.bucket_start_utc, 'YYYY-MM-DD')"
      , "  END AS label,"
      , "  (COUNT(e.legacy_lifecycle_action) FILTER (WHERE e.legacy_entity_kind = 'project' AND e.legacy_lifecycle_action = 'created'))::int4 AS project_created,"
      , "  (COUNT(e.legacy_lifecycle_action) FILTER (WHERE e.legacy_entity_kind = 'project' AND e.legacy_lifecycle_action = 'completed'))::int4 AS project_completed,"
      , "  (COUNT(e.legacy_lifecycle_action) FILTER (WHERE e.legacy_entity_kind = 'project' AND e.legacy_lifecycle_action = 'cancelled'))::int4 AS project_cancelled,"
      , "  (COUNT(e.legacy_lifecycle_action) FILTER (WHERE e.legacy_entity_kind = 'subproject' AND e.legacy_lifecycle_action = 'created'))::int4 AS subproject_created,"
      , "  (COUNT(e.legacy_lifecycle_action) FILTER (WHERE e.legacy_entity_kind = 'subproject' AND e.legacy_lifecycle_action = 'completed'))::int4 AS subproject_completed,"
      , "  (COUNT(e.legacy_lifecycle_action) FILTER (WHERE e.legacy_entity_kind = 'subproject' AND e.legacy_lifecycle_action = 'cancelled'))::int4 AS subproject_cancelled,"
      , "  (COUNT(e.legacy_lifecycle_action) FILTER (WHERE e.legacy_entity_kind = 'task' AND e.legacy_lifecycle_action = 'created'))::int4 AS task_created,"
      , "  (COUNT(e.legacy_lifecycle_action) FILTER (WHERE e.legacy_entity_kind = 'task' AND e.legacy_lifecycle_action = 'completed'))::int4 AS task_completed,"
      , "  (COUNT(e.legacy_lifecycle_action) FILTER (WHERE e.legacy_entity_kind = 'task' AND e.legacy_lifecycle_action = 'cancelled'))::int4 AS task_cancelled,"
      , "  (COUNT(e.legacy_lifecycle_action) FILTER (WHERE e.legacy_entity_kind = 'subtask' AND e.legacy_lifecycle_action = 'created'))::int4 AS subtask_created,"
      , "  (COUNT(e.legacy_lifecycle_action) FILTER (WHERE e.legacy_entity_kind = 'subtask' AND e.legacy_lifecycle_action = 'completed'))::int4 AS subtask_completed,"
      , "  (COUNT(e.legacy_lifecycle_action) FILTER (WHERE e.legacy_entity_kind = 'subtask' AND e.legacy_lifecycle_action = 'cancelled'))::int4 AS subtask_cancelled,"
      , "  (COUNT(e.canonical_lifecycle_action) FILTER (WHERE e.canonical_entity_kind = 'project' AND e.canonical_lifecycle_action = 'created'))::int4 AS series_project_created,"
      , "  (COUNT(e.canonical_lifecycle_action) FILTER (WHERE e.canonical_entity_kind = 'project' AND e.canonical_lifecycle_action = 'completed'))::int4 AS series_project_completed,"
      , "  (COUNT(e.canonical_lifecycle_action) FILTER (WHERE e.canonical_entity_kind = 'project' AND e.canonical_lifecycle_action = 'deleted'))::int4 AS series_project_deleted,"
      , "  (COUNT(e.canonical_lifecycle_action) FILTER (WHERE e.canonical_entity_kind = 'project' AND e.canonical_lifecycle_action = 'archived'))::int4 AS series_project_archived,"
      , "  (COUNT(e.canonical_lifecycle_action) FILTER (WHERE e.canonical_entity_kind = 'project' AND e.canonical_lifecycle_action = 'cancelled'))::int4 AS series_project_cancelled,"
      , "  (COUNT(e.canonical_lifecycle_action) FILTER (WHERE e.canonical_entity_kind = 'task' AND e.canonical_lifecycle_action = 'created'))::int4 AS series_task_created,"
      , "  (COUNT(e.canonical_lifecycle_action) FILTER (WHERE e.canonical_entity_kind = 'task' AND e.canonical_lifecycle_action = 'completed'))::int4 AS series_task_completed,"
      , "  (COUNT(e.canonical_lifecycle_action) FILTER (WHERE e.canonical_entity_kind = 'task' AND e.canonical_lifecycle_action = 'deleted'))::int4 AS series_task_deleted,"
      , "  (COUNT(e.canonical_lifecycle_action) FILTER (WHERE e.canonical_entity_kind = 'task' AND e.canonical_lifecycle_action = 'archived'))::int4 AS series_task_archived,"
      , "  (COUNT(e.canonical_lifecycle_action) FILTER (WHERE e.canonical_entity_kind = 'task' AND e.canonical_lifecycle_action = 'cancelled'))::int4 AS series_task_cancelled,"
      , "  (COUNT(e.canonical_lifecycle_action) FILTER (WHERE e.canonical_entity_kind = 'subtask' AND e.canonical_lifecycle_action = 'created'))::int4 AS series_subtask_created,"
      , "  (COUNT(e.canonical_lifecycle_action) FILTER (WHERE e.canonical_entity_kind = 'subtask' AND e.canonical_lifecycle_action = 'completed'))::int4 AS series_subtask_completed,"
      , "  (COUNT(e.canonical_lifecycle_action) FILTER (WHERE e.canonical_entity_kind = 'subtask' AND e.canonical_lifecycle_action = 'deleted'))::int4 AS series_subtask_deleted,"
      , "  (COUNT(e.canonical_lifecycle_action) FILTER (WHERE e.canonical_entity_kind = 'subtask' AND e.canonical_lifecycle_action = 'archived'))::int4 AS series_subtask_archived,"
      , "  (COUNT(e.canonical_lifecycle_action) FILTER (WHERE e.canonical_entity_kind = 'subtask' AND e.canonical_lifecycle_action = 'cancelled'))::int4 AS series_subtask_cancelled,"
      , "  (COUNT(e.canonical_lifecycle_action) FILTER (WHERE e.canonical_entity_kind = 'observation' AND e.canonical_lifecycle_action = 'created'))::int4 AS series_observation_created,"
      , "  (COUNT(e.canonical_lifecycle_action) FILTER (WHERE e.canonical_entity_kind = 'observation' AND e.canonical_lifecycle_action = 'completed'))::int4 AS series_observation_completed,"
      , "  (COUNT(e.canonical_lifecycle_action) FILTER (WHERE e.canonical_entity_kind = 'observation' AND e.canonical_lifecycle_action = 'deleted'))::int4 AS series_observation_deleted,"
      , "  (COUNT(e.canonical_lifecycle_action) FILTER (WHERE e.canonical_entity_kind = 'observation' AND e.canonical_lifecycle_action = 'archived'))::int4 AS series_observation_archived,"
      , "  (COUNT(e.canonical_lifecycle_action) FILTER (WHERE e.canonical_entity_kind = 'observation' AND e.canonical_lifecycle_action = 'cancelled'))::int4 AS series_observation_cancelled"
      , "FROM buckets b"
      , "LEFT JOIN events e"
      , "  ON (e.legacy_lifecycle_action IS NOT NULL OR e.canonical_lifecycle_action IS NOT NULL)"
      , " AND e.occurred_at >= (b.bucket_start_utc AT TIME ZONE 'UTC')"
      , " AND e.occurred_at < (b.bucket_end_utc AT TIME ZONE 'UTC')"
      , "GROUP BY b.bucket_start_utc, b.bucket_end_utc"
      , "ORDER BY b.bucket_start_utc ASC"
      , "LIMIT 367"
      , ")"
      , "SELECT l.*,"
      , "  CASE WHEN s.open_status = 'available' THEN s.projects END,"
      , "  CASE WHEN s.open_status = 'available' THEN s.tasks END,"
      , "  CASE WHEN s.open_status <> 'future' THEN s.cutoff END, s.open_status,"
      , "  CASE WHEN s.observation_status = 'available' THEN s.observations END,"
      , "  CASE WHEN s.observation_status <> 'future' THEN s.cutoff END, s.observation_status"
      , "FROM lifecycle_buckets l JOIN inventory_samples s ON s.bucket_start_utc AT TIME ZONE 'UTC' = l.bucket_start"
      , "ORDER BY l.bucket_start ASC"
      ]
    encoder =
      contramap (\(a,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid)) <>
      contramap (\(_,b,_,_) -> b) (Enc.param (Enc.nonNullable Enc.timestamptz)) <>
      contramap (\(_,_,c,_) -> c) (Enc.param (Enc.nonNullable Enc.timestamptz)) <>
      contramap (\(_,_,_,d) -> d) (Enc.param (Enc.nonNullable Enc.text))

oracleInventoryCtes :: [String]
oracleInventoryCtes =
  [ ", inventory_clock AS (SELECT statement_timestamp() AS as_of), inventory_current AS ("
  , "  SELECT 'project'::text AS kind, id::text AS entity_id, (deleted_at IS NULL AND status IN ('active', 'paused'))::int AS open FROM projects WHERE workspace_id = $1"
  , "  UNION ALL SELECT 'task', id::text, (deleted_at IS NULL AND status IN ('todo', 'in_progress', 'blocked'))::int FROM tasks WHERE workspace_id = $1"
  , "  UNION ALL SELECT 'observation', id::text, 1 FROM observations WHERE workspace_id = $1"
  , "), inventory_audit AS MATERIALIZED ("
  , "  SELECT a.*, a.action::text AS action_text FROM audit_log a"
  , "  WHERE a.entity_type IN ('project', 'task', 'observation') AND (a.workspace_id = $1 OR (a.workspace_id IS NULL AND"
  , "    (a.new_values ->> 'workspace_id' = $1::text OR a.old_values ->> 'workspace_id' = $1::text)))"
  , "), inventory_states AS ("
  , "  SELECT a.entity_type AS kind, a.entity_id, a.changed_at, a.action_text,"
  , "    CASE WHEN side = 'old' THEN -1 ELSE 1 END AS sign, value,"
  , "    (a.action_text = 'create' AND side = 'old') OR (a.action_text = 'delete' AND side = 'new') AS absent"
  , "  FROM inventory_audit a CROSS JOIN LATERAL (VALUES ('old', a.old_values), ('new', a.new_values)) snapshots(side, value)"
  , "), inventory_state_values AS MATERIALIZED ("
  , "  SELECT s.*, CASE WHEN absent THEN 0 ELSE 1 END AS existence,"
  , "    CASE WHEN NOT absent AND value ->> 'deleted_at' IS NULL AND"
  , "      ((kind = 'project' AND value ->> 'status' IN ('active', 'paused')) OR (kind = 'task' AND value ->> 'status' IN ('todo', 'in_progress', 'blocked')) OR kind = 'observation') THEN 1 ELSE 0 END AS open"
  , "  FROM inventory_states s"
  , "), inventory_deltas AS MATERIALIZED ("
  , "  SELECT kind, entity_id, changed_at, bool_or(action_text = 'create') AS has_create,"
  , "    sum(open * sign) AS delta, sum(existence * sign) AS existence_delta,"
  , "    bool_and(CASE WHEN absent THEN value IS NULL OR value = 'null'::jsonb ELSE"
  , "      COALESCE(value ?& ARRAY['id','workspace_id'] AND value ->> 'id' = entity_id AND value ->> 'workspace_id' = $1::text AND"
  , "      (kind = 'observation' OR (value ?& ARRAY['status','deleted_at'] AND"
  , "       ((kind = 'project' AND value ->> 'status' IN ('active','paused','completed','archived')) OR"
  , "        (kind = 'task' AND value ->> 'status' IN ('todo','in_progress','blocked','done','cancelled'))))), false) END"
  , "      AND action_text IN ('create','update','delete')) AS valid"
  , "  FROM inventory_state_values GROUP BY kind, entity_id, changed_at"
  , "), inventory_history AS ("
  , "  SELECT kind, entity_id, min(changed_at) AS first_at, max(changed_at) FILTER (WHERE NOT valid) AS invalid_at, bool_or(has_create) AS has_create"
  , "  FROM inventory_deltas GROUP BY kind, entity_id"
  , "), inventory_group_ends AS ("
  , "  SELECT d.*, COALESCE(c.open, 0) - COALESCE(sum(d.delta) OVER later, 0) AS after_open,"
  , "    CASE WHEN c.entity_id IS NULL THEN 0 ELSE 1 END - COALESCE(sum(d.existence_delta) OVER later, 0) AS after_existence"
  , "  FROM inventory_deltas d LEFT JOIN inventory_current c USING (kind, entity_id)"
  , "  WINDOW later AS (PARTITION BY d.kind, d.entity_id ORDER BY d.changed_at ROWS BETWEEN 1 FOLLOWING AND UNBOUNDED FOLLOWING)"
  , "), inventory_group_boundaries AS ("
  , "  SELECT g.*, after_open - delta AS before_open, after_existence - existence_delta AS before_existence FROM inventory_group_ends g"
  , "), inventory_group_checks AS ("
  , "  SELECT g.kind, g.entity_id, g.changed_at,"
  , "    g.before_existence IN (0,1) AND g.after_existence IN (0,1) AND g.before_open IN (0,1) AND g.after_open IN (0,1)"
  , "    AND g.before_open <= g.before_existence AND g.after_open <= g.after_existence"
  , "    AND bool_or(s.sign = -1 AND s.existence = g.before_existence AND s.open = g.before_open)"
  , "    AND bool_or(s.sign = 1 AND s.existence = g.after_existence AND s.open = g.after_open) AS agrees"
  , "  FROM inventory_group_boundaries g JOIN inventory_state_values s USING (kind, entity_id, changed_at)"
  , "  GROUP BY g.kind, g.entity_id, g.changed_at, g.before_existence, g.after_existence, g.before_open, g.after_open"
  , "), inventory_reconciliation AS ("
  , "  SELECT kind, entity_id, bool_and(agrees) AS agrees FROM inventory_group_checks GROUP BY kind, entity_id"
  , "), inventory_coverage AS ("
  , "  SELECT max(unknown_through) FILTER (WHERE kind <> 'observation') AS open_unknown_through,"
  , "    max(unknown_through) FILTER (WHERE kind = 'observation') AS observation_unknown_through FROM ("
  , "  SELECT kind, CASE WHEN h.entity_id IS NULL THEN clock.as_of"
  , "    WHEN NOT reconciliation.agrees THEN clock.as_of"
  , "    ELSE greatest(h.invalid_at, CASE WHEN NOT h.has_create THEN h.first_at END) END AS unknown_through"
  , "  FROM inventory_current c FULL JOIN inventory_history h USING (kind, entity_id)"
  , "  LEFT JOIN inventory_reconciliation reconciliation USING (kind, entity_id) CROSS JOIN inventory_clock clock"
  , "  ) coverage"
  , "), inventory_anchor AS ("
  , "  SELECT COALESCE(sum(open) FILTER (WHERE kind = 'project'), 0) AS projects, COALESCE(sum(open) FILTER (WHERE kind = 'task'), 0) AS tasks,"
  , "    count(*) FILTER (WHERE kind = 'observation') AS observations FROM inventory_current"
  , "), inventory_cutoffs AS ("
  , "  SELECT b.*, clock.as_of, least(b.bucket_end_utc AT TIME ZONE 'UTC', $3, clock.as_of) AS cutoff"
  , "  FROM buckets b CROSS JOIN inventory_clock clock"
  , "), inventory_samples AS ("
  , "  SELECT b.bucket_start_utc, b.cutoff,"
  , "    (a.projects - COALESCE(sum(d.delta) FILTER (WHERE d.kind = 'project'), 0))::bigint AS projects,"
  , "    (a.tasks - COALESCE(sum(d.delta) FILTER (WHERE d.kind = 'task'), 0))::bigint AS tasks,"
  , "    (a.observations - COALESCE(sum(d.existence_delta) FILTER (WHERE d.kind = 'observation'), 0))::bigint AS observations,"
  , "    CASE WHEN b.bucket_start_utc AT TIME ZONE 'UTC' >= b.as_of THEN 'future'"
  , "      WHEN b.cutoff < b.as_of AND b.cutoff <= coverage.open_unknown_through THEN 'history_unavailable' ELSE 'available' END AS open_status,"
  , "    CASE WHEN b.bucket_start_utc AT TIME ZONE 'UTC' >= b.as_of THEN 'future'"
  , "      WHEN b.cutoff < b.as_of AND b.cutoff <= coverage.observation_unknown_through THEN 'history_unavailable' ELSE 'available' END AS observation_status"
  , "  FROM inventory_cutoffs b CROSS JOIN inventory_anchor a CROSS JOIN inventory_coverage coverage"
  , "  LEFT JOIN inventory_deltas d ON d.changed_at >= b.cutoff AND b.cutoff < b.as_of"
  , "  GROUP BY b.bucket_start_utc, b.cutoff, b.as_of, a.projects, a.tasks, a.observations, coverage.open_unknown_through, coverage.observation_unknown_through"
  , ")"
  ]

-- Every bucket listing in this spec is also checked against the oracle.
-- Values derived from the statement clock may differ only for a bucket that
-- reaches the current time.
listWorkspaceTimelineBuckets :: Pool Hasql.Connection -> UUID -> UTCTime -> UTCTime -> Text -> IO [WorkspaceTimelineBucket]
listWorkspaceTimelineBuckets pool workspace since untilTime size = do
  actual <- Timeline.listWorkspaceTimelineBuckets pool workspace since untilTime size
  expected <- runSession pool (Session.statement (workspace, since, untilTime, size) oracleBucketsStatement)
  now <- getCurrentTime
  let recent value = maybe False (\at -> abs (diffUTCTime at now) < 60) value
      clockFree bucket
        | recent bucket.timelineBucketOpenAsOf || recent bucket.timelineBucketObservationAsOf =
            bucket { timelineBucketOpenAsOf = Nothing, timelineBucketObservationAsOf = Nothing }
        | otherwise = bucket
  map clockFree actual `shouldBe` map clockFree expected
  pure actual

-- EXPLAIN of the bucket statement under a forced generic or custom plan, with
-- sequential scans disabled so any remaining one has no index alternative.
explainBuckets :: TestEnv -> BS8.ByteString -> UUID -> IO [Text]
explainBuckets env mode workspace = runSession env.pool $ do
  let Statement.Statement sqlText _ _ _ = Timeline.listWorkspaceTimelineBucketsStatement
  Session.sql ("SET LOCAL enable_seqscan = off; SET LOCAL plan_cache_mode = " <> mode)
  Session.sql ("PREPARE timeline_bucket_plan(uuid, timestamptz, timestamptz, text) AS " <> sqlText)
  plan <- Session.statement () (Statement.Statement
    ("EXPLAIN EXECUTE timeline_bucket_plan('" <> BS8.pack (show workspace) <> "', '2019-06-01T00:00:00Z', '2021-01-01T00:00:00Z', 'week')")
    Enc.noParams (Dec.rowList (Dec.column (Dec.nonNullable Dec.text))) False)
  Session.sql "DEALLOCATE timeline_bucket_plan"
  pure plan
