BEGIN;

------------------------------------------------------------------------
-- Normalize legacy nested subtasks to the flat one-layer task model
------------------------------------------------------------------------

-- This migration repairs active (deleted_at IS NULL) task hierarchy shape only.
-- Soft-deleted task rows are left untouched so restores continue to validate
-- through lifecycle triggers.  Active tasks whose parent is missing or
-- soft-deleted are detached and reported.  Active parent cycles are broken by
-- detaching the rows seen in cyclic paths and reported.  Remaining active
-- descendants below depth 1 are moved directly under their top-level ancestor,
-- and dependency edges from moved task -> former direct parent preserve the old
-- nesting relationship when doing so is safe.  Those edges are normal task
-- dependencies: after the graph
-- is repaired, V014 auto-blocking is recomputed, so a previously done moved task
-- can become blocked if its new former-parent dependency is still open.

CREATE TABLE IF NOT EXISTS task_flatten_migration_report (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  task_id UUID REFERENCES tasks(id) ON DELETE CASCADE,
  issue TEXT NOT NULL,
  detail JSONB NOT NULL DEFAULT '{}'::jsonb,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (task_id, issue)
);

-- Disable task lifecycle/auto-blocking triggers before touching legacy corrupt
-- rows.  Older recursive lifecycle helpers predate the flat model and do not
-- tolerate active parent cycles; V018 breaks those cycles first, then
-- recomputes auto-blocking after the hierarchy is flat and acyclic again.
-- The dependency cycle trigger also uses an older unguarded recursive walk, so
-- V018 performs its own path-guarded iterative cycle check while adding edges.
-- Dependency workspace validation remains active.
ALTER TABLE tasks DISABLE TRIGGER trg_task_flat_subtask_lifecycle;
ALTER TABLE tasks DISABLE TRIGGER trg_task_lifecycle_invariants;
ALTER TABLE tasks DISABLE TRIGGER trg_task_auto_blocking_from_task;
ALTER TABLE task_dependencies DISABLE TRIGGER trg_task_auto_blocking_from_dependency;
ALTER TABLE task_dependencies DISABLE TRIGGER trg_task_dep_no_cycle;

-- Active children cannot remain attached to missing or soft-deleted parents
-- under the flat model.  Detach them instead of attempting to infer a deleted
-- ancestor chain, and report missing-parent corruption separately from normal
-- soft-delete ancestry.
WITH detached_parent_tasks AS (
  SELECT child.id AS task_id,
         child.parent_id AS legacy_parent_id,
         parent.id IS NULL AS parent_missing,
         parent.deleted_at AS parent_deleted_at
    FROM tasks child
    LEFT JOIN tasks parent ON parent.id = child.parent_id
   WHERE child.deleted_at IS NULL
     AND child.parent_id IS NOT NULL
     AND (parent.id IS NULL OR parent.deleted_at IS NOT NULL)
)
INSERT INTO task_flatten_migration_report (task_id, issue, detail)
SELECT task_id,
       CASE WHEN parent_missing THEN 'missing_parent_detached' ELSE 'deleted_parent_detached' END,
       jsonb_build_object(
         'legacy_parent_id', legacy_parent_id,
         'parent_missing', parent_missing,
         'parent_deleted_at', parent_deleted_at
       )
  FROM detached_parent_tasks
ON CONFLICT (task_id, issue) DO NOTHING;

WITH detached_parent_tasks AS (
  SELECT child.id AS task_id
    FROM tasks child
    LEFT JOIN tasks parent ON parent.id = child.parent_id
   WHERE child.deleted_at IS NULL
     AND child.parent_id IS NOT NULL
     AND (parent.id IS NULL OR parent.deleted_at IS NOT NULL)
)
UPDATE tasks task_to_update
   SET parent_id = NULL
  FROM detached_parent_tasks dpt
 WHERE task_to_update.id = dpt.task_id;

-- Detect active hierarchy cycles using a path-tracked walk so corrupt legacy
-- data cannot make recursive traversal loop indefinitely.  Detaching every row
-- seen in a cyclic path is conservative and idempotent.
WITH RECURSIVE walk(origin_id, current_id, parent_id, path, cycle_found) AS (
  SELECT t.id, t.id, t.parent_id, ARRAY[t.id], false
    FROM tasks t
   WHERE t.deleted_at IS NULL
  UNION ALL
  SELECT walk.origin_id,
         parent.id,
         parent.parent_id,
         walk.path || parent.id,
         parent.id = ANY(walk.path)
    FROM walk
    JOIN tasks parent ON parent.id = walk.parent_id
   WHERE parent.deleted_at IS NULL
     AND NOT walk.cycle_found
),
cycle_nodes AS (
  SELECT DISTINCT unnest(path) AS task_id
    FROM walk
   WHERE cycle_found
)
INSERT INTO task_flatten_migration_report (task_id, issue, detail)
SELECT task_id,
       'hierarchy_cycle_detached',
       jsonb_build_object('reason', 'active parent cycle detected during flat-subtask migration')
  FROM cycle_nodes
ON CONFLICT (task_id, issue) DO NOTHING;

WITH RECURSIVE walk(origin_id, current_id, parent_id, path, cycle_found) AS (
  SELECT t.id, t.id, t.parent_id, ARRAY[t.id], false
    FROM tasks t
   WHERE t.deleted_at IS NULL
  UNION ALL
  SELECT walk.origin_id,
         parent.id,
         parent.parent_id,
         walk.path || parent.id,
         parent.id = ANY(walk.path)
    FROM walk
    JOIN tasks parent ON parent.id = walk.parent_id
   WHERE parent.deleted_at IS NULL
     AND NOT walk.cycle_found
),
cycle_nodes AS (
  SELECT DISTINCT unnest(path) AS task_id
    FROM walk
   WHERE cycle_found
)
UPDATE tasks task_to_update
   SET parent_id = NULL
  FROM cycle_nodes cn
 WHERE task_to_update.id = cn.task_id;

CREATE TEMP TABLE hmem_flatten_task_plan ON COMMIT DROP AS
WITH RECURSIVE task_tree(task_id, old_parent_id, root_id, depth, path) AS (
  SELECT t.id, t.parent_id, t.id, 0, ARRAY[t.id]
    FROM tasks t
   WHERE t.deleted_at IS NULL
     AND t.parent_id IS NULL
  UNION ALL
  SELECT child.id,
         child.parent_id,
         task_tree.root_id,
         task_tree.depth + 1,
         task_tree.path || child.id
    FROM task_tree
    JOIN tasks child ON child.parent_id = task_tree.task_id
   WHERE child.deleted_at IS NULL
     AND NOT child.id = ANY(task_tree.path)
)
SELECT task_id, old_parent_id, root_id AS new_parent_id, depth
  FROM task_tree
 WHERE depth > 1;

-- Preserve old nesting with dependency edges where possible.  Existing edges,
-- self edges, cross-workspace edges, and dependency cycles are skipped.  Insert
-- candidates one at a time so each cycle check sees earlier accepted migration
-- edges instead of validating only against the pre-migration dependency graph.
DO $$
DECLARE
  candidate_edge RECORD;
BEGIN
  FOR candidate_edge IN
    SELECT plan.task_id, plan.old_parent_id AS depends_on_id, plan.depth
      FROM hmem_flatten_task_plan plan
      JOIN tasks task_to_move ON task_to_move.id = plan.task_id
      JOIN tasks former_parent ON former_parent.id = plan.old_parent_id
     WHERE plan.task_id <> plan.old_parent_id
       AND task_to_move.deleted_at IS NULL
       AND former_parent.deleted_at IS NULL
       AND task_to_move.workspace_id = former_parent.workspace_id
     ORDER BY plan.depth ASC, plan.task_id ASC
  LOOP
    IF NOT EXISTS (
      WITH RECURSIVE dependency_chain(id, path) AS (
        SELECT td.depends_on_id, ARRAY[td.depends_on_id]
          FROM task_dependencies td
         WHERE td.task_id = candidate_edge.depends_on_id
        UNION ALL
        SELECT td.depends_on_id, dependency_chain.path || td.depends_on_id
          FROM task_dependencies td
          JOIN dependency_chain ON dependency_chain.id = td.task_id
         WHERE NOT td.depends_on_id = ANY(dependency_chain.path)
      )
      SELECT 1
        FROM dependency_chain
       WHERE id = candidate_edge.task_id
    ) THEN
      INSERT INTO task_dependencies (task_id, depends_on_id)
      VALUES (candidate_edge.task_id, candidate_edge.depends_on_id)
      ON CONFLICT DO NOTHING;
    ELSE
      INSERT INTO task_flatten_migration_report (task_id, issue, detail)
      VALUES (
        candidate_edge.task_id,
        'dependency_cycle_edge_skipped',
        jsonb_build_object('depends_on_id', candidate_edge.depends_on_id)
      )
      ON CONFLICT (task_id, issue) DO NOTHING;
    END IF;
  END LOOP;
END;
$$ LANGUAGE plpgsql;

DO $$
DECLARE
  planned_task RECORD;
BEGIN
  FOR planned_task IN
    SELECT task_id, new_parent_id
      FROM hmem_flatten_task_plan
     ORDER BY depth DESC, task_id ASC
  LOOP
    UPDATE tasks
       SET parent_id = planned_task.new_parent_id
     WHERE id = planned_task.task_id
       AND deleted_at IS NULL
       AND parent_id IS DISTINCT FROM planned_task.new_parent_id;
  END LOOP;
END;
$$ LANGUAGE plpgsql;

ALTER TABLE task_dependencies ENABLE TRIGGER trg_task_auto_blocking_from_dependency;
ALTER TABLE task_dependencies ENABLE TRIGGER trg_task_dep_no_cycle;
ALTER TABLE tasks ENABLE TRIGGER trg_task_auto_blocking_from_task;
ALTER TABLE tasks ENABLE TRIGGER trg_task_lifecycle_invariants;
ALTER TABLE tasks ENABLE TRIGGER trg_task_flat_subtask_lifecycle;

-- The parent rewrites and preserved dependency edges can change derived blocked
-- states.  Run one explicit recomputation now that recursive helpers can safely
-- traverse the repaired task graph.
DO $$
DECLARE
  seed_ids UUID[] := ARRAY[]::UUID[];
BEGIN
  SELECT coalesce(array_agg(id), ARRAY[]::UUID[])
    INTO seed_ids
    FROM tasks
   WHERE deleted_at IS NULL;

  PERFORM hmem_recompute_task_auto_blocking(seed_ids);
END;
$$ LANGUAGE plpgsql;

COMMIT;
