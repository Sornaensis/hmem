module HMem.DB.Timeline
  ( listWorkspaceTimeline
  , listWorkspaceTimelineBuckets
  , earliestWorkspaceLifecycle
  ) where

import Data.ByteString.Char8 qualified as BS8
import Data.Aeson qualified as Aeson
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
  , TimelineBucketSeries(..)
  , TimelineBucketSeriesCounts(..)
  , TimelineOpenCounts(..)
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
      , "    a.bot_identity AS bot_identity,"
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
      , "  e.bot_identity,"
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
      ] <> timelineOpenInventoryCtes <>
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
      , "  CASE WHEN s.open_status <> 'future' THEN s.cutoff END, s.open_status"
      , "FROM lifecycle_buckets l JOIN inventory_samples s ON s.bucket_start_utc AT TIME ZONE 'UTC' = l.bucket_start"
      , "ORDER BY l.bucket_start ASC"
      ]
    encoder =
      contramap (\(a,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid)) <>
      contramap (\(_,b,_,_) -> b) (Enc.param (Enc.nonNullable Enc.timestamptz)) <>
      contramap (\(_,_,c,_) -> c) (Enc.param (Enc.nonNullable Enc.timestamptz)) <>
      contramap (\(_,_,_,d) -> d) (Enc.param (Enc.nonNullable Enc.text))
    decoder = Dec.rowList timelineBucketRowDecoder

-- Current rows are the exact anchor. Reverse full-row open-predicate deltas,
-- never lifecycle totals, across the entire retained history. A missing initial
-- create limits coverage; malformed snapshots or a failed per-entity anchor
-- reconciliation invalidate the affected earlier samples. Arbitrary manual
-- purges that leave a consistent history cannot always be detected.
-- Audit timestamps can tie within a transaction. Reconcile each timestamp's
-- implied before/after inventory against its old/new snapshot candidates rather
-- than inventing an order from UUIDs, including when the create is not retained.
timelineOpenInventoryCtes :: [String]
timelineOpenInventoryCtes =
  [ ", inventory_clock AS (SELECT statement_timestamp() AS as_of), inventory_current AS ("
  , "  SELECT 'project'::text AS kind, id::text AS entity_id, (deleted_at IS NULL AND status IN ('active', 'paused'))::int AS open FROM projects WHERE workspace_id = $1"
  , "  UNION ALL SELECT 'task', id::text, (deleted_at IS NULL AND status IN ('todo', 'in_progress', 'blocked'))::int FROM tasks WHERE workspace_id = $1"
  , "), inventory_audit AS MATERIALIZED ("
  , "  SELECT a.*, a.action::text AS action_text FROM audit_log a"
  , "  WHERE a.entity_type IN ('project', 'task') AND (a.workspace_id = $1 OR (a.workspace_id IS NULL AND"
  , "    (a.new_values ->> 'workspace_id' = $1::text OR a.old_values ->> 'workspace_id' = $1::text)))"
  , "), inventory_states AS ("
  , "  SELECT a.entity_type AS kind, a.entity_id, a.changed_at, a.action_text,"
  , "    CASE WHEN side = 'old' THEN -1 ELSE 1 END AS sign, value,"
  , "    (a.action_text = 'create' AND side = 'old') OR (a.action_text = 'delete' AND side = 'new') AS absent"
  , "  FROM inventory_audit a CROSS JOIN LATERAL (VALUES ('old', a.old_values), ('new', a.new_values)) snapshots(side, value)"
  , "), inventory_state_values AS MATERIALIZED ("
  , "  SELECT s.*, CASE WHEN absent THEN 0 ELSE 1 END AS existence,"
  , "    CASE WHEN NOT absent AND value ->> 'deleted_at' IS NULL AND"
  , "      ((kind = 'project' AND value ->> 'status' IN ('active', 'paused')) OR (kind = 'task' AND value ->> 'status' IN ('todo', 'in_progress', 'blocked'))) THEN 1 ELSE 0 END AS open"
  , "  FROM inventory_states s"
  , "), inventory_deltas AS MATERIALIZED ("
  , "  SELECT kind, entity_id, changed_at, bool_or(action_text = 'create') AS has_create,"
  , "    sum(open * sign) AS delta, sum(existence * sign) AS existence_delta,"
  , "    bool_and(CASE WHEN absent THEN value IS NULL OR value = 'null'::jsonb ELSE"
  , "      COALESCE(value ?& ARRAY['id','workspace_id','status','deleted_at'] AND value ->> 'id' = entity_id AND value ->> 'workspace_id' = $1::text AND"
  , "      ((kind = 'project' AND value ->> 'status' IN ('active','paused','completed','archived')) OR"
  , "       (kind = 'task' AND value ->> 'status' IN ('todo','in_progress','blocked','done','cancelled'))), false) END"
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
  , "  SELECT max(CASE WHEN h.entity_id IS NULL THEN clock.as_of"
  , "    WHEN NOT reconciliation.agrees THEN clock.as_of"
  , "    ELSE greatest(h.invalid_at, CASE WHEN NOT h.has_create THEN h.first_at END) END) AS unknown_through"
  , "  FROM inventory_current c FULL JOIN inventory_history h USING (kind, entity_id)"
  , "  LEFT JOIN inventory_reconciliation reconciliation USING (kind, entity_id) CROSS JOIN inventory_clock clock"
  , "), inventory_anchor AS ("
  , "  SELECT COALESCE(sum(open) FILTER (WHERE kind = 'project'), 0) AS projects, COALESCE(sum(open) FILTER (WHERE kind = 'task'), 0) AS tasks FROM inventory_current"
  , "), inventory_cutoffs AS ("
  , "  SELECT b.*, clock.as_of, least(b.bucket_end_utc AT TIME ZONE 'UTC', $3, clock.as_of) AS cutoff"
  , "  FROM buckets b CROSS JOIN inventory_clock clock"
  , "), inventory_samples AS ("
  , "  SELECT b.bucket_start_utc, b.cutoff,"
  , "    (a.projects - COALESCE(sum(d.delta) FILTER (WHERE d.kind = 'project'), 0))::bigint AS projects,"
  , "    (a.tasks - COALESCE(sum(d.delta) FILTER (WHERE d.kind = 'task'), 0))::bigint AS tasks,"
  , "    CASE WHEN b.bucket_start_utc AT TIME ZONE 'UTC' >= b.as_of THEN 'future'"
  , "      WHEN b.cutoff < b.as_of AND b.cutoff <= coverage.unknown_through THEN 'history_unavailable' ELSE 'available' END AS open_status"
  , "  FROM inventory_cutoffs b CROSS JOIN inventory_anchor a CROSS JOIN inventory_coverage coverage"
  , "  LEFT JOIN inventory_deltas d ON d.changed_at >= b.cutoff AND b.cutoff < b.as_of"
  , "  GROUP BY b.bucket_start_utc, b.cutoff, b.as_of, a.projects, a.tasks, coverage.unknown_through"
  , ")"
  ]


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
  identityJson <- Dec.column (Dec.nullable Dec.jsonb)
  identity <- traverse (\value -> case Aeson.fromJSON value of
    Aeson.Success result -> pure result
    Aeson.Error message -> fail message) identityJson
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
          , botIdentity = identity
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
  seriesProjectCreated <- intColumn
  seriesProjectCompleted <- intColumn
  seriesProjectDeleted <- intColumn
  seriesProjectArchived <- intColumn
  seriesProjectCancelled <- intColumn
  seriesTaskCreated <- intColumn
  seriesTaskCompleted <- intColumn
  seriesTaskDeleted <- intColumn
  seriesTaskArchived <- intColumn
  seriesTaskCancelled <- intColumn
  seriesSubtaskCreated <- intColumn
  seriesSubtaskCompleted <- intColumn
  seriesSubtaskDeleted <- intColumn
  seriesSubtaskArchived <- intColumn
  seriesSubtaskCancelled <- intColumn
  seriesObservationCreated <- intColumn
  seriesObservationCompleted <- intColumn
  seriesObservationDeleted <- intColumn
  seriesObservationArchived <- intColumn
  seriesObservationCancelled <- intColumn
  openProjects <- Dec.column (Dec.nullable Dec.int8)
  openTasks <- Dec.column (Dec.nullable Dec.int8)
  openAsOf <- Dec.column (Dec.nullable Dec.timestamptz)
  openStatus <- Dec.column (Dec.nonNullable Dec.text)
  let projectCounts = TimelineBucketCounts projectCreated projectCompleted projectCancelled
      subprojectCounts = TimelineBucketCounts subprojectCreated subprojectCompleted subprojectCancelled
      taskCounts = TimelineBucketCounts taskCreated taskCompleted taskCancelled
      subtaskCounts = TimelineBucketCounts subtaskCreated subtaskCompleted subtaskCancelled
      totalCounts = TimelineBucketCounts
        { created = projectCreated + subprojectCreated + taskCreated + subtaskCreated
        , completed = projectCompleted + subprojectCompleted + taskCompleted + subtaskCompleted
        , cancelled = projectCancelled + subprojectCancelled + taskCancelled + subtaskCancelled
        }
      seriesProject = TimelineBucketSeriesCounts seriesProjectCreated seriesProjectCompleted seriesProjectDeleted seriesProjectArchived seriesProjectCancelled
      seriesTask = TimelineBucketSeriesCounts seriesTaskCreated seriesTaskCompleted seriesTaskDeleted seriesTaskArchived seriesTaskCancelled
      seriesSubtask = TimelineBucketSeriesCounts seriesSubtaskCreated seriesSubtaskCompleted seriesSubtaskDeleted seriesSubtaskArchived seriesSubtaskCancelled
      seriesObservation = TimelineBucketSeriesCounts seriesObservationCreated seriesObservationCompleted seriesObservationDeleted seriesObservationArchived seriesObservationCancelled
      seriesTotals = TimelineBucketSeriesCounts
        { created = seriesProjectCreated + seriesTaskCreated + seriesSubtaskCreated + seriesObservationCreated
        , completed = seriesProjectCompleted + seriesTaskCompleted + seriesSubtaskCompleted + seriesObservationCompleted
        , deleted = seriesProjectDeleted + seriesTaskDeleted + seriesSubtaskDeleted + seriesObservationDeleted
        , archived = seriesProjectArchived + seriesTaskArchived + seriesSubtaskArchived + seriesObservationArchived
        , cancelled = seriesProjectCancelled + seriesTaskCancelled + seriesSubtaskCancelled + seriesObservationCancelled
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
    , timelineBucketSeries = TimelineBucketSeries
        { seriesProject = seriesProject
        , seriesTask = seriesTask
        , seriesSubtask = seriesSubtask
        , seriesObservation = seriesObservation
        }
    , timelineBucketSeriesTotals = seriesTotals
    , timelineBucketOpenCounts = TimelineOpenCounts <$> openProjects <*> openTasks
    , timelineBucketOpenAsOf = openAsOf
    , timelineBucketOpenStatus = openStatus
    }
  where
    intColumn = fromIntegral <$> Dec.column (Dec.nonNullable Dec.int4)

-- Only lifecycle-eligible audit rows determine the All time lower bound.
earliestWorkspaceLifecycle :: Pool Hasql.Connection -> UUID -> UTCTime -> IO (Maybe UTCTime)
earliestWorkspaceLifecycle pool ws untilTime = runSession pool $ Session.statement (ws, untilTime) statement
  where
    statement = Statement.Statement (BS8.pack $ unlines
      [ "SELECT min(changed_at) FROM audit_log WHERE workspace_id = $1 AND changed_at < $2"
      , "AND entity_type IN ('project', 'task', 'observation') AND (action::text IN ('create', 'delete')"
      , "OR (action::text = 'update' AND entity_type IN ('project', 'task') AND ("
      , "(NULLIF(old_values ->> 'deleted_at', '') IS NULL AND NULLIF(new_values ->> 'deleted_at', '') IS NOT NULL)"
      , "OR (old_values ->> 'status' IS DISTINCT FROM new_values ->> 'status' AND ((entity_type = 'project' AND new_values ->> 'status' IN ('completed', 'archived'))"
      , "OR (entity_type = 'task' AND new_values ->> 'status' IN ('done', 'cancelled')))))))"
      ]) (contramap fst (Enc.param (Enc.nonNullable Enc.uuid)) <> contramap snd (Enc.param (Enc.nonNullable Enc.timestamptz)))
      (Dec.singleRow (Dec.column (Dec.nullable Dec.timestamptz))) True
