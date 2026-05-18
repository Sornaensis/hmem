BEGIN;

------------------------------------------------------------------------
-- Cascading task/project deletion semantics
------------------------------------------------------------------------

-- Soft-delete is the recoverable lifecycle path and now cascades through
-- task/project subtrees.  Purge remains hard deletion after soft-delete; the
-- service layer deletes task rows before project rows so project purges do not
-- detach tasks through the project_id ON DELETE SET NULL constraint.
--
-- This migration also repairs legacy rows that stayed active below a deleted
-- task/project parent, and records those repairs for operators.

CREATE TABLE IF NOT EXISTS delete_cascade_migration_report (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  entity_type TEXT NOT NULL,
  entity_id UUID NOT NULL,
  issue TEXT NOT NULL,
  detail JSONB NOT NULL DEFAULT '{}'::jsonb,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (entity_type, entity_id, issue)
);

CREATE OR REPLACE FUNCTION hmem_soft_delete_memories_for_deleted_links(
  deleted_project_ids UUID[],
  deleted_task_ids UUID[],
  cascade_deleted_at TIMESTAMPTZ
)
RETURNS INTEGER AS $$
DECLARE
  affected_count INTEGER := 0;
BEGIN
  WITH updated AS (
    UPDATE memories m
       SET deleted_at = cascade_deleted_at
     WHERE m.deleted_at IS NULL
       AND (
         EXISTS (
           SELECT 1
             FROM project_memory_links pml
            WHERE pml.memory_id = m.id
              AND pml.project_id = ANY(deleted_project_ids)
         )
         OR EXISTS (
           SELECT 1
             FROM task_memory_links tml
            WHERE tml.memory_id = m.id
              AND tml.task_id = ANY(deleted_task_ids)
         )
       )
       AND NOT EXISTS (
         SELECT 1
           FROM project_memory_links pml_active
           JOIN projects p_active ON p_active.id = pml_active.project_id
          WHERE pml_active.memory_id = m.id
            AND p_active.deleted_at IS NULL
            AND p_active.workspace_id = m.workspace_id
            AND p_active.id <> ALL(deleted_project_ids)
       )
       AND NOT EXISTS (
         SELECT 1
           FROM task_memory_links tml_active
           JOIN tasks t_active ON t_active.id = tml_active.task_id
          WHERE tml_active.memory_id = m.id
            AND t_active.deleted_at IS NULL
            AND t_active.workspace_id = m.workspace_id
            AND t_active.id <> ALL(deleted_task_ids)
       )
     RETURNING id
  )
  SELECT count(*)::INTEGER INTO affected_count FROM updated;

  RETURN affected_count;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION hmem_cascade_task_soft_delete()
RETURNS TRIGGER AS $$
DECLARE
  affected_task_ids UUID[] := ARRAY[]::UUID[];
BEGIN
  IF NEW.deleted_at IS NOT NULL AND OLD.deleted_at IS NULL THEN
    WITH RECURSIVE task_tree(id) AS (
      SELECT NEW.id
      UNION
      SELECT child.id
        FROM tasks child
        JOIN task_tree parent ON child.parent_id = parent.id
       WHERE child.deleted_at IS NULL
    ),
    updated_tasks AS (
      UPDATE tasks task_to_update
         SET deleted_at = NEW.deleted_at
        FROM task_tree
       WHERE task_to_update.id = task_tree.id
         AND task_to_update.id <> NEW.id
         AND task_to_update.deleted_at IS NULL
       RETURNING task_to_update.id
    ),
    all_tasks AS (
      SELECT NEW.id AS id
      UNION
      SELECT id FROM updated_tasks
    )
    SELECT coalesce(array_agg(id), ARRAY[]::UUID[])
      INTO affected_task_ids
      FROM all_tasks;

    DELETE FROM task_dependencies
     WHERE task_id = ANY(affected_task_ids)
        OR depends_on_id = ANY(affected_task_ids);

    PERFORM hmem_soft_delete_memories_for_deleted_links(
      ARRAY[]::UUID[],
      affected_task_ids,
      NEW.deleted_at
    );
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trg_task_soft_delete_cascade ON tasks;
CREATE TRIGGER trg_task_soft_delete_cascade
  AFTER UPDATE OF deleted_at ON tasks
  FOR EACH ROW EXECUTE FUNCTION hmem_cascade_task_soft_delete();

CREATE OR REPLACE FUNCTION hmem_cascade_project_soft_delete()
RETURNS TRIGGER AS $$
DECLARE
  affected_project_ids UUID[] := ARRAY[]::UUID[];
  affected_task_ids UUID[] := ARRAY[]::UUID[];
BEGIN
  IF NEW.deleted_at IS NOT NULL AND OLD.deleted_at IS NULL THEN
    WITH RECURSIVE project_tree(id) AS (
      SELECT NEW.id
      UNION
      SELECT child.id
        FROM projects child
        JOIN project_tree parent ON child.parent_id = parent.id
       WHERE child.deleted_at IS NULL
    ),
    updated_projects AS (
      UPDATE projects project_to_update
         SET deleted_at = NEW.deleted_at
        FROM project_tree
       WHERE project_to_update.id = project_tree.id
         AND project_to_update.id <> NEW.id
         AND project_to_update.deleted_at IS NULL
       RETURNING project_to_update.id
    ),
    all_projects AS (
      SELECT NEW.id AS id
      UNION
      SELECT id FROM updated_projects
    )
    SELECT coalesce(array_agg(id), ARRAY[]::UUID[])
      INTO affected_project_ids
      FROM all_projects;

    WITH RECURSIVE task_tree(id) AS (
      SELECT task_row.id
        FROM tasks task_row
       WHERE task_row.project_id = ANY(affected_project_ids)
         AND task_row.deleted_at IS NULL
      UNION
      SELECT child.id
        FROM tasks child
        JOIN task_tree parent ON child.parent_id = parent.id
       WHERE child.deleted_at IS NULL
    ),
    updated_tasks AS (
      UPDATE tasks task_to_update
         SET deleted_at = NEW.deleted_at
        FROM task_tree
       WHERE task_to_update.id = task_tree.id
         AND task_to_update.deleted_at IS NULL
       RETURNING task_to_update.id
    )
    SELECT coalesce(array_agg(id), ARRAY[]::UUID[])
      INTO affected_task_ids
      FROM updated_tasks;

    DELETE FROM task_dependencies
     WHERE task_id = ANY(affected_task_ids)
        OR depends_on_id = ANY(affected_task_ids);

    PERFORM hmem_soft_delete_memories_for_deleted_links(
      affected_project_ids,
      affected_task_ids,
      NEW.deleted_at
    );
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trg_project_soft_delete_cascade ON projects;
CREATE TRIGGER trg_project_soft_delete_cascade
  AFTER UPDATE OF deleted_at ON projects
  FOR EACH ROW EXECUTE FUNCTION hmem_cascade_project_soft_delete();

-- Backfill active child projects under already-deleted parents.  Updating each
-- direct child invokes the project cascade trigger for its subtree.
WITH stale_child_projects AS (
  SELECT child.id AS child_id,
         parent.id AS deleted_parent_id,
         parent.deleted_at AS cascade_deleted_at
    FROM projects child
    JOIN projects parent ON parent.id = child.parent_id
   WHERE child.deleted_at IS NULL
     AND parent.deleted_at IS NOT NULL
)
INSERT INTO delete_cascade_migration_report (entity_type, entity_id, issue, detail)
SELECT 'project', child_id, 'active_child_project_deleted_with_parent',
       jsonb_build_object('deleted_parent_id', deleted_parent_id)
  FROM stale_child_projects
ON CONFLICT (entity_type, entity_id, issue) DO NOTHING;

WITH RECURSIVE stale_project_tree(deleted_project_id, project_id) AS (
  SELECT parent.id, child.id
    FROM projects child
    JOIN projects parent ON parent.id = child.parent_id
   WHERE child.deleted_at IS NULL
     AND parent.deleted_at IS NOT NULL
  UNION
  SELECT stale_project_tree.deleted_project_id, child.id
    FROM projects child
    JOIN stale_project_tree ON child.parent_id = stale_project_tree.project_id
   WHERE child.deleted_at IS NULL
),
stale_project_tasks AS (
  SELECT task_row.id AS task_id,
         stale_project_tree.deleted_project_id
    FROM tasks task_row
    JOIN stale_project_tree ON stale_project_tree.project_id = task_row.project_id
   WHERE task_row.deleted_at IS NULL
)
INSERT INTO delete_cascade_migration_report (entity_type, entity_id, issue, detail)
SELECT 'task', task_id, 'active_task_deleted_with_project',
       jsonb_build_object('deleted_project_id', deleted_project_id)
  FROM stale_project_tasks
ON CONFLICT (entity_type, entity_id, issue) DO NOTHING;

WITH stale_child_projects AS (
  SELECT child.id AS child_id,
         parent.deleted_at AS cascade_deleted_at
    FROM projects child
    JOIN projects parent ON parent.id = child.parent_id
   WHERE child.deleted_at IS NULL
     AND parent.deleted_at IS NOT NULL
)
UPDATE projects project_to_update
   SET deleted_at = stale.cascade_deleted_at
  FROM stale_child_projects stale
 WHERE project_to_update.id = stale.child_id
   AND project_to_update.deleted_at IS NULL;

-- Backfill active tasks that still belong to deleted project trees.
WITH stale_project_tasks AS (
  SELECT task_row.id AS task_id,
         project_row.id AS deleted_project_id,
         project_row.deleted_at AS cascade_deleted_at
    FROM tasks task_row
    JOIN projects project_row ON project_row.id = task_row.project_id
   WHERE task_row.deleted_at IS NULL
     AND project_row.deleted_at IS NOT NULL
)
INSERT INTO delete_cascade_migration_report (entity_type, entity_id, issue, detail)
SELECT 'task', task_id, 'active_task_deleted_with_project',
       jsonb_build_object('deleted_project_id', deleted_project_id)
  FROM stale_project_tasks
ON CONFLICT (entity_type, entity_id, issue) DO NOTHING;

WITH stale_project_tasks AS (
  SELECT task_row.id AS task_id,
         project_row.deleted_at AS cascade_deleted_at
    FROM tasks task_row
    JOIN projects project_row ON project_row.id = task_row.project_id
   WHERE task_row.deleted_at IS NULL
     AND project_row.deleted_at IS NOT NULL
)
UPDATE tasks task_to_update
   SET deleted_at = stale.cascade_deleted_at
  FROM stale_project_tasks stale
 WHERE task_to_update.id = stale.task_id
   AND task_to_update.deleted_at IS NULL;

-- Backfill active child tasks under already-deleted parent tasks.  Updating each
-- direct child invokes the task cascade trigger for its subtree.
WITH stale_child_tasks AS (
  SELECT child.id AS child_id,
         parent.id AS deleted_parent_id,
         parent.deleted_at AS cascade_deleted_at
    FROM tasks child
    JOIN tasks parent ON parent.id = child.parent_id
   WHERE child.deleted_at IS NULL
     AND parent.deleted_at IS NOT NULL
)
INSERT INTO delete_cascade_migration_report (entity_type, entity_id, issue, detail)
SELECT 'task', child_id, 'active_child_task_deleted_with_parent',
       jsonb_build_object('deleted_parent_id', deleted_parent_id)
  FROM stale_child_tasks
ON CONFLICT (entity_type, entity_id, issue) DO NOTHING;

WITH stale_child_tasks AS (
  SELECT child.id AS child_id,
         parent.deleted_at AS cascade_deleted_at
    FROM tasks child
    JOIN tasks parent ON parent.id = child.parent_id
   WHERE child.deleted_at IS NULL
     AND parent.deleted_at IS NOT NULL
)
UPDATE tasks task_to_update
   SET deleted_at = stale.cascade_deleted_at
  FROM stale_child_tasks stale
 WHERE task_to_update.id = stale.child_id
   AND task_to_update.deleted_at IS NULL;

-- Remove stale dependencies involving deleted tasks, including rows that
-- predated this migration.
DELETE FROM task_dependencies dep
 USING tasks task_row
 WHERE (dep.task_id = task_row.id OR dep.depends_on_id = task_row.id)
   AND task_row.deleted_at IS NOT NULL;

COMMIT;
