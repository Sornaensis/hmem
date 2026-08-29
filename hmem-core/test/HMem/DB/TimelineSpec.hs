module HMem.DB.TimelineSpec (spec) where

import Data.Aeson qualified as Aeson
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
import HMem.DB.Pool (runSession)
import HMem.DB.Project
import HMem.DB.Task qualified as Task
import HMem.DB.TestHarness
import HMem.DB.Timeline
import HMem.Types

spec :: Spec
spec = beforeAll setupTestPool $ aroundWith withTestTransaction $
  describe "Timeline buckets" $ do
    it "emits canonical lifecycle series without changing legacy histogram totals" $ \env -> do
      workspace <- createTestWorkspace env "timeline-series"
      rangeStart <- addUTCTime (-1) <$> getCurrentTime
      root <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Nothing, name = "root", description = Nothing, priority = Nothing, metadata = Nothing }
      nested <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Just root.id, name = "nested", description = Nothing, priority = Nothing, metadata = Nothing }
      rootTask <- Task.createTask env.pool CreateTask
        { workspaceId = workspace.id, projectId = Just root.id, parentId = Nothing, title = "root task", description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      subtask <- Task.createTask env.pool CreateTask
        { workspaceId = workspace.id, projectId = Just root.id, parentId = Just rootTask.id, title = "subtask", description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
      observation <- createObservation env.pool (CreateObservation workspace.id [ObservationSubject SubjectFile "src/Timeline.hs"] "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "timeline")
      _ <- updateProject env.pool nested.id UpdateProject
        { name = Nothing, description = Unchanged, parentId = Unchanged, status = Just ProjArchived, priority = Nothing, metadata = Nothing }
      _ <- Task.updateTask env.pool subtask.id UpdateTask
        { title = Nothing, description = Unchanged, projectId = Unchanged, parentId = Unchanged, status = Just Done, priority = Nothing, metadata = Nothing, dueAt = Unchanged }
      _ <- Task.updateTask env.pool rootTask.id UpdateTask
        { title = Nothing, description = Unchanged, projectId = Unchanged, parentId = Unchanged, status = Just Done, priority = Nothing, metadata = Nothing, dueAt = Unchanged }
      _ <- updateProject env.pool root.id UpdateProject
        { name = Nothing, description = Unchanged, parentId = Unchanged, status = Just ProjCompleted, priority = Nothing, metadata = Nothing }
      deleteObservation env.pool workspace.id observation.id `shouldReturn` True
      deleteProjectCascade env.pool root.id >>= (`shouldSatisfy` (/= Nothing))
      rangeEnd <- addUTCTime 1 <$> getCurrentTime
      buckets <- listWorkspaceTimelineBuckets env.pool workspace.id rangeStart rangeEnd "day"
      let total get = sum (map get buckets)
          sumSeries get = foldr addSeries (TimelineBucketSeriesCounts 0 0 0) (map get buckets)
          project = sumSeries (\bucket -> bucket.timelineBucketSeries.seriesProject)
          task = sumSeries (\bucket -> bucket.timelineBucketSeries.seriesTask)
          child = sumSeries (\bucket -> bucket.timelineBucketSeries.seriesSubtask)
          observationSeries = sumSeries (\bucket -> bucket.timelineBucketSeries.seriesObservation)
          seriesTotals = sumSeries (.timelineBucketSeriesTotals)
      project.created `shouldBe` 2
      project.completed `shouldBe` 1
      project.deleted `shouldBe` 2
      task.created `shouldBe` 1
      task.completed `shouldBe` 1
      task.deleted `shouldBe` 1
      child.created `shouldBe` 1
      child.completed `shouldBe` 1
      child.deleted `shouldBe` 1
      observationSeries.created `shouldBe` 1
      observationSeries.completed `shouldBe` 0
      observationSeries.deleted `shouldBe` 1
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
      map (.timelineBucketSeriesTotals) buckets `shouldBe` replicate 3 (TimelineBucketSeriesCounts 0 0 0)

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
      insertAudit env otherWorkspace.id "observation" "00000000-0000-0000-0000-000000000399" "create" Nothing (Just (payload [])) inRange
      insertAudit env workspace.id "observation" "00000000-0000-0000-0000-000000000303" "create" Nothing (Just (payload [])) endBoundary
      buckets <- listWorkspaceTimelineBuckets env.pool workspace.id since untilTime "day"
      let series = foldr addTimelineSeries emptyTimelineSeries (map (.timelineBucketSeries) buckets)
      series.seriesProject `shouldBe` TimelineBucketSeriesCounts 1 1 1
      series.seriesTask `shouldBe` TimelineBucketSeriesCounts 1 0 0
      series.seriesSubtask `shouldBe` TimelineBucketSeriesCounts 1 1 1
      series.seriesObservation `shouldBe` TimelineBucketSeriesCounts 1 0 1

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
  }

emptyTimelineSeries :: TimelineBucketSeries
emptyTimelineSeries = TimelineBucketSeries zero zero zero zero
  where zero = TimelineBucketSeriesCounts 0 0 0

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
