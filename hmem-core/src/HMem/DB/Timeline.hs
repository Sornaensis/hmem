module HMem.DB.Timeline
  ( listWorkspaceTimeline
  , listWorkspaceTimelineBuckets
  ) where

import Data.ByteString.Char8 qualified as BS8
import Data.Functor.Contravariant (contramap)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement

import HMem.DB.Pool (runSession)
import HMem.Types
  ( TimelineActor(..)
  , TimelineBucketCounts(..)
  , TimelineBucketEntityCounts(..)
  , TimelineNavigation(..)
  , TimelineProjectContext(..)
  , TimelineStatusTransition(..)
  , TimelineTaskContext(..)
  , WorkspaceTimelineBucket(..)
  , WorkspaceTimelineEvent(..)
  , capPaginationOverfetch
  )

listWorkspaceTimeline
  :: Pool Hasql.Connection
  -> UUID        -- ^ workspace_id
  -> Maybe Text  -- ^ entity_type filter: project, task, or subtask
  -> Maybe Text  -- ^ event_type filter
  -> Maybe UTCTime -- ^ inclusive occurred_at lower bound
  -> Maybe UTCTime -- ^ exclusive occurred_at upper bound
  -> Maybe Int   -- ^ limit
  -> Maybe Int   -- ^ offset
  -> IO [WorkspaceTimelineEvent]
listWorkspaceTimeline pool wsId mEntityType mEventType mSince mUntil mlimit moffset = do
  let (lim, off) = capPaginationOverfetch mlimit moffset
      params =
        ( wsId
        , mEntityType
        , mEventType
        , mSince
        , mUntil
        , fromIntegral lim :: Int32
        , fromIntegral off :: Int32
        )
  runSession pool $ Session.statement params listWorkspaceTimelineStatement

type TimelineParams = (UUID, Maybe Text, Maybe Text, Maybe UTCTime, Maybe UTCTime, Int32, Int32)

listWorkspaceTimelineStatement :: Statement.Statement TimelineParams [WorkspaceTimelineEvent]
listWorkspaceTimelineStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH candidates AS ("
      , "  SELECT"
      , "    a.id AS audit_id,"
      , "    ('audit:' || a.id::text) AS event_id,"
      , "    a.workspace_id AS workspace_id,"
      , "    a.entity_id::uuid AS entity_uuid,"
      , "    a.changed_at AS occurred_at,"
      , "    a.actor_type::text AS actor_type,"
      , "    a.actor_id AS actor_id,"
      , "    a.actor_label AS actor_label,"
      , "    a.entity_type AS raw_entity_type,"
      , "    a.action::text AS action_text,"
      , "    NULLIF(a.old_values ->> 'status', '') AS old_status,"
      , "    NULLIF(a.new_values ->> 'status', '') AS new_status,"
      , "    NULLIF(CASE"
      , "      WHEN a.new_values ? 'parent_id' THEN a.new_values ->> 'parent_id'"
      , "      WHEN a.old_values ? 'parent_id' THEN a.old_values ->> 'parent_id'"
      , "      ELSE t.parent_id::text"
      , "    END, '')::uuid AS parent_task_uuid,"
      , "    NULLIF(CASE"
      , "      WHEN a.new_values ? 'project_id' THEN a.new_values ->> 'project_id'"
      , "      WHEN a.old_values ? 'project_id' THEN a.old_values ->> 'project_id'"
      , "      ELSE t.project_id::text"
      , "    END, '')::uuid AS task_project_uuid,"
      , "    COALESCE(a.new_values ->> 'title', a.old_values ->> 'title', t.title, 'Untitled task') AS task_title,"
      , "    COALESCE(a.new_values ->> 'name', a.old_values ->> 'name', p.name, 'Untitled project') AS project_title"
      , "  FROM audit_log a"
      , "  LEFT JOIN tasks t ON a.entity_type = 'task' AND t.id = a.entity_id::uuid"
      , "  LEFT JOIN projects p ON a.entity_type = 'project' AND p.id = a.entity_id::uuid"
      , "  WHERE a.workspace_id = $1"
      , "    AND a.entity_type IN ('project', 'task')"
      , "    AND a.action::text IN ('create', 'update')"
      , "    AND ($4::timestamptz IS NULL OR a.changed_at >= $4)"
      , "    AND ($5::timestamptz IS NULL OR a.changed_at < $5)"
      , "), events AS ("
      , "  SELECT"
      , "    c.*,"
      , "    CASE"
      , "      WHEN c.raw_entity_type = 'project' AND c.action_text = 'create' THEN 'project_created'"
      , "      WHEN c.raw_entity_type = 'project' AND c.action_text = 'update' AND c.old_status IS NOT NULL AND c.new_status = 'completed' AND c.old_status <> c.new_status THEN 'project_completed'"
      , "      WHEN c.raw_entity_type = 'project' AND c.action_text = 'update' AND c.old_status IS NOT NULL AND c.new_status = 'archived' AND c.old_status <> c.new_status THEN 'project_archived'"
      , "      WHEN c.raw_entity_type = 'task' AND c.action_text = 'create' AND c.parent_task_uuid IS NULL THEN 'task_created'"
      , "      WHEN c.raw_entity_type = 'task' AND c.action_text = 'create' AND c.parent_task_uuid IS NOT NULL THEN 'subtask_created'"
      , "      WHEN c.raw_entity_type = 'task' AND c.action_text = 'update' AND c.old_status IS NOT NULL AND c.new_status = 'done' AND c.old_status <> c.new_status AND c.parent_task_uuid IS NULL THEN 'task_completed'"
      , "      WHEN c.raw_entity_type = 'task' AND c.action_text = 'update' AND c.old_status IS NOT NULL AND c.new_status = 'done' AND c.old_status <> c.new_status AND c.parent_task_uuid IS NOT NULL THEN 'subtask_completed'"
      , "      WHEN c.raw_entity_type = 'task' AND c.action_text = 'update' AND c.old_status IS NOT NULL AND c.new_status = 'cancelled' AND c.old_status <> c.new_status AND c.parent_task_uuid IS NULL THEN 'task_cancelled'"
      , "      WHEN c.raw_entity_type = 'task' AND c.action_text = 'update' AND c.old_status IS NOT NULL AND c.new_status = 'cancelled' AND c.old_status <> c.new_status AND c.parent_task_uuid IS NOT NULL THEN 'subtask_cancelled'"
      , "      ELSE NULL"
      , "    END AS event_type,"
      , "    CASE"
      , "      WHEN c.raw_entity_type = 'task' AND c.parent_task_uuid IS NOT NULL THEN 'subtask'"
      , "      ELSE c.raw_entity_type"
      , "    END AS display_entity_type"
      , "  FROM candidates c"
      , ")"
      , "SELECT"
      , "  e.event_id,"
      , "  e.workspace_id,"
      , "  e.event_type,"
      , "  e.display_entity_type,"
      , "  e.entity_uuid,"
      , "  CASE WHEN e.display_entity_type = 'project' THEN e.project_title ELSE e.task_title END AS title,"
      , "  e.occurred_at,"
      , "  e.actor_type,"
      , "  e.actor_id,"
      , "  e.actor_label,"
      , "  e.task_project_uuid,"
      , "  COALESCE(project_ctx.name, 'Untitled project') AS project_name,"
      , "  e.parent_task_uuid,"
      , "  COALESCE(parent_ctx.title, 'Untitled task') AS parent_task_title,"
      , "  CASE WHEN e.action_text = 'update' THEN e.old_status ELSE NULL END AS status_from,"
      , "  CASE WHEN e.action_text = 'update' THEN e.new_status ELSE NULL END AS status_to,"
      , "  e.audit_id"
      , "FROM events e"
      , "LEFT JOIN projects project_ctx ON project_ctx.id = e.task_project_uuid"
      , "LEFT JOIN tasks parent_ctx ON parent_ctx.id = e.parent_task_uuid"
      , "WHERE e.event_type IS NOT NULL"
      , "  AND ($2::text IS NULL OR e.display_entity_type = $2)"
      , "  AND ($3::text IS NULL OR e.event_type = $3)"
      , "ORDER BY e.occurred_at DESC, e.audit_id DESC"
      , "LIMIT $6 OFFSET $7"
      ]
    encoder =
      contramap (\(a,_,_,_,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid)) <>
      contramap (\(_,b,_,_,_,_,_) -> b) (Enc.param (Enc.nullable Enc.text)) <>
      contramap (\(_,_,c,_,_,_,_) -> c) (Enc.param (Enc.nullable Enc.text)) <>
      contramap (\(_,_,_,d,_,_,_) -> d) (Enc.param (Enc.nullable Enc.timestamptz)) <>
      contramap (\(_,_,_,_,e,_,_) -> e) (Enc.param (Enc.nullable Enc.timestamptz)) <>
      contramap (\(_,_,_,_,_,f,_) -> f) (Enc.param (Enc.nonNullable Enc.int4)) <>
      contramap (\(_,_,_,_,_,_,g) -> g) (Enc.param (Enc.nonNullable Enc.int4))
    decoder = Dec.rowList timelineRowDecoder

listWorkspaceTimelineBuckets
  :: Pool Hasql.Connection
  -> UUID
  -> UTCTime
  -> UTCTime
  -> Text
  -> IO [WorkspaceTimelineBucket]
listWorkspaceTimelineBuckets pool wsId since untilTime bucketSize = do
  let params = (wsId, since, untilTime, bucketSize)
  runSession pool $ Session.statement params listWorkspaceTimelineBucketsStatement

type TimelineBucketParams = (UUID, UTCTime, UTCTime, Text)

listWorkspaceTimelineBucketsStatement :: Statement.Statement TimelineBucketParams [WorkspaceTimelineBucket]
listWorkspaceTimelineBucketsStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
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
      , "), candidates AS ("
      , "  SELECT"
      , "    a.id AS audit_id,"
      , "    a.workspace_id AS workspace_id,"
      , "    a.entity_id::uuid AS entity_uuid,"
      , "    a.changed_at AS occurred_at,"
      , "    a.entity_type AS raw_entity_type,"
      , "    a.action::text AS action_text,"
      , "    NULLIF(a.old_values ->> 'status', '') AS old_status,"
      , "    NULLIF(a.new_values ->> 'status', '') AS new_status,"
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
      , "    AND a.entity_type IN ('project', 'task')"
      , "    AND a.action::text IN ('create', 'update')"
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
      , "    END AS entity_kind,"
      , "    CASE"
      , "      WHEN c.raw_entity_type = 'project' AND c.action_text = 'create' THEN 'created'"
      , "      WHEN c.raw_entity_type = 'project' AND c.action_text = 'update' AND c.old_status IS NOT NULL AND c.new_status IN ('completed', 'archived') AND c.old_status <> c.new_status THEN 'completed'"
      , "      WHEN c.raw_entity_type = 'task' AND c.action_text = 'create' THEN 'created'"
      , "      WHEN c.raw_entity_type = 'task' AND c.action_text = 'update' AND c.old_status IS NOT NULL AND c.new_status = 'done' AND c.old_status <> c.new_status THEN 'completed'"
      , "      WHEN c.raw_entity_type = 'task' AND c.action_text = 'update' AND c.old_status IS NOT NULL AND c.new_status = 'cancelled' AND c.old_status <> c.new_status THEN 'cancelled'"
      , "      ELSE NULL"
      , "    END AS lifecycle_action"
      , "  FROM candidates c"
      , ")"
      , "SELECT"
      , "  b.bucket_start_utc AT TIME ZONE 'UTC' AS bucket_start,"
      , "  b.bucket_end_utc AT TIME ZONE 'UTC' AS bucket_end,"
      , "  CASE $4"
      , "    WHEN 'quarter' THEN ('Q' || EXTRACT(quarter FROM b.bucket_start_utc)::int::text || ' ' || to_char(b.bucket_start_utc, 'YYYY'))"
      , "    WHEN 'month' THEN to_char(b.bucket_start_utc, 'Mon YYYY')"
      , "    ELSE to_char(b.bucket_start_utc, 'YYYY-MM-DD')"
      , "  END AS label,"
      , "  (COUNT(e.lifecycle_action) FILTER (WHERE e.entity_kind = 'project' AND e.lifecycle_action = 'created'))::int4 AS project_created,"
      , "  (COUNT(e.lifecycle_action) FILTER (WHERE e.entity_kind = 'project' AND e.lifecycle_action = 'completed'))::int4 AS project_completed,"
      , "  (COUNT(e.lifecycle_action) FILTER (WHERE e.entity_kind = 'project' AND e.lifecycle_action = 'cancelled'))::int4 AS project_cancelled,"
      , "  (COUNT(e.lifecycle_action) FILTER (WHERE e.entity_kind = 'subproject' AND e.lifecycle_action = 'created'))::int4 AS subproject_created,"
      , "  (COUNT(e.lifecycle_action) FILTER (WHERE e.entity_kind = 'subproject' AND e.lifecycle_action = 'completed'))::int4 AS subproject_completed,"
      , "  (COUNT(e.lifecycle_action) FILTER (WHERE e.entity_kind = 'subproject' AND e.lifecycle_action = 'cancelled'))::int4 AS subproject_cancelled,"
      , "  (COUNT(e.lifecycle_action) FILTER (WHERE e.entity_kind = 'task' AND e.lifecycle_action = 'created'))::int4 AS task_created,"
      , "  (COUNT(e.lifecycle_action) FILTER (WHERE e.entity_kind = 'task' AND e.lifecycle_action = 'completed'))::int4 AS task_completed,"
      , "  (COUNT(e.lifecycle_action) FILTER (WHERE e.entity_kind = 'task' AND e.lifecycle_action = 'cancelled'))::int4 AS task_cancelled,"
      , "  (COUNT(e.lifecycle_action) FILTER (WHERE e.entity_kind = 'subtask' AND e.lifecycle_action = 'created'))::int4 AS subtask_created,"
      , "  (COUNT(e.lifecycle_action) FILTER (WHERE e.entity_kind = 'subtask' AND e.lifecycle_action = 'completed'))::int4 AS subtask_completed,"
      , "  (COUNT(e.lifecycle_action) FILTER (WHERE e.entity_kind = 'subtask' AND e.lifecycle_action = 'cancelled'))::int4 AS subtask_cancelled"
      , "FROM buckets b"
      , "LEFT JOIN events e"
      , "  ON e.lifecycle_action IS NOT NULL"
      , " AND e.occurred_at >= (b.bucket_start_utc AT TIME ZONE 'UTC')"
      , " AND e.occurred_at < (b.bucket_end_utc AT TIME ZONE 'UTC')"
      , "GROUP BY b.bucket_start_utc, b.bucket_end_utc"
      , "ORDER BY b.bucket_start_utc ASC"
      , "LIMIT 367"
      ]
    encoder =
      contramap (\(a,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid)) <>
      contramap (\(_,b,_,_) -> b) (Enc.param (Enc.nonNullable Enc.timestamptz)) <>
      contramap (\(_,_,c,_) -> c) (Enc.param (Enc.nonNullable Enc.timestamptz)) <>
      contramap (\(_,_,_,d) -> d) (Enc.param (Enc.nonNullable Enc.text))
    decoder = Dec.rowList timelineBucketRowDecoder


timelineRowDecoder :: Dec.Row WorkspaceTimelineEvent
timelineRowDecoder = do
  eventId <- Dec.column (Dec.nonNullable Dec.text)
  timelineWorkspaceId <- Dec.column (Dec.nonNullable Dec.uuid)
  timelineEventType <- Dec.column (Dec.nonNullable Dec.text)
  timelineEntityType <- Dec.column (Dec.nonNullable Dec.text)
  timelineEntityId <- Dec.column (Dec.nonNullable Dec.uuid)
  timelineTitle <- Dec.column (Dec.nonNullable Dec.text)
  timelineOccurredAt <- Dec.column (Dec.nonNullable Dec.timestamptz)
  mActorType <- Dec.column (Dec.nullable Dec.text)
  mActorId <- Dec.column (Dec.nullable Dec.text)
  mActorLabel <- Dec.column (Dec.nullable Dec.text)
  mProjectId <- Dec.column (Dec.nullable Dec.uuid)
  mProjectName <- Dec.column (Dec.nullable Dec.text)
  mParentTaskId <- Dec.column (Dec.nullable Dec.uuid)
  mParentTaskTitle <- Dec.column (Dec.nullable Dec.text)
  mStatusFrom <- Dec.column (Dec.nullable Dec.text)
  mStatusTo <- Dec.column (Dec.nullable Dec.text)
  mAuditId <- Dec.column (Dec.nullable Dec.uuid)
  let timelineActor = case (mActorType, mActorId, mActorLabel) of
        (Nothing, Nothing, Nothing) -> Nothing
        _ -> Just TimelineActor
          { actorType = mActorType
          , actorId = mActorId
          , actorLabel = mActorLabel
          }
      timelineProject = case mProjectId of
        Nothing -> Nothing
        Just projectId -> Just TimelineProjectContext
          { projectContextId = projectId
          , projectContextName = fromMaybe "Untitled project" mProjectName
          }
      timelineParentTask = case mParentTaskId of
        Nothing -> Nothing
        Just parentTaskId -> Just TimelineTaskContext
          { taskContextId = parentTaskId
          , taskContextTitle = fromMaybe "Untitled task" mParentTaskTitle
          }
      timelineStatusTransition = TimelineStatusTransition <$> mStatusFrom <*> mStatusTo
      timelineNavigation = TimelineNavigation
        { navigationEntityType = if timelineEntityType == "project" then "project" else "task"
        , navigationEntityId = timelineEntityId
        }
  pure WorkspaceTimelineEvent
    { id = eventId
    , workspaceId = timelineWorkspaceId
    , eventType = timelineEventType
    , entityType = timelineEntityType
    , entityId = timelineEntityId
    , title = timelineTitle
    , occurredAt = timelineOccurredAt
    , actor = timelineActor
    , project = timelineProject
    , parentTask = timelineParentTask
    , statusTransition = timelineStatusTransition
    , navigation = timelineNavigation
    , sourceAuditId = mAuditId
    }


timelineBucketRowDecoder :: Dec.Row WorkspaceTimelineBucket
timelineBucketRowDecoder = do
  bucketStart <- Dec.column (Dec.nonNullable Dec.timestamptz)
  bucketEnd <- Dec.column (Dec.nonNullable Dec.timestamptz)
  bucketLabel <- Dec.column (Dec.nonNullable Dec.text)
  projectCreated <- intColumn
  projectCompleted <- intColumn
  projectCancelled <- intColumn
  subprojectCreated <- intColumn
  subprojectCompleted <- intColumn
  subprojectCancelled <- intColumn
  taskCreated <- intColumn
  taskCompleted <- intColumn
  taskCancelled <- intColumn
  subtaskCreated <- intColumn
  subtaskCompleted <- intColumn
  subtaskCancelled <- intColumn
  let projectCounts = TimelineBucketCounts projectCreated projectCompleted projectCancelled
      subprojectCounts = TimelineBucketCounts subprojectCreated subprojectCompleted subprojectCancelled
      taskCounts = TimelineBucketCounts taskCreated taskCompleted taskCancelled
      subtaskCounts = TimelineBucketCounts subtaskCreated subtaskCompleted subtaskCancelled
      totalCounts = TimelineBucketCounts
        { created = projectCreated + subprojectCreated + taskCreated + subtaskCreated
        , completed = projectCompleted + subprojectCompleted + taskCompleted + subtaskCompleted
        , cancelled = projectCancelled + subprojectCancelled + taskCancelled + subtaskCancelled
        }
  pure WorkspaceTimelineBucket
    { timelineBucketStart = bucketStart
    , timelineBucketEnd = bucketEnd
    , timelineBucketLabel = bucketLabel
    , timelineBucketCounts = TimelineBucketEntityCounts
        { projectCounts = projectCounts
        , subprojectCounts = subprojectCounts
        , taskCounts = taskCounts
        , subtaskCounts = subtaskCounts
        }
    , timelineBucketTotals = totalCounts
    }
  where
    intColumn = fromIntegral <$> Dec.column (Dec.nonNullable Dec.int4)
