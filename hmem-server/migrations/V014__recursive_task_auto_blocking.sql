BEGIN;

------------------------------------------------------------------------
-- Recursive task auto-blocking
------------------------------------------------------------------------

ALTER TABLE tasks
  ADD COLUMN IF NOT EXISTS auto_blocked BOOLEAN NOT NULL DEFAULT false;

-- A task is automatically blocked when it has an active open dependency or
-- any active open descendant task.  Done/cancelled dependencies and
-- descendants are closed for this purpose because hmem_is_open_task_status
-- is defined as todo/in_progress/blocked in V012.
CREATE OR REPLACE FUNCTION hmem_task_has_open_dependencies(task_id_to_check UUID)
RETURNS BOOLEAN AS $$
  SELECT EXISTS (
    SELECT 1
      FROM task_dependencies dep_link
      JOIN tasks dep ON dep.id = dep_link.depends_on_id
     WHERE dep_link.task_id = task_id_to_check
       AND dep.deleted_at IS NULL
       AND hmem_is_open_task_status(dep.status)
  );
$$ LANGUAGE sql STABLE;

CREATE OR REPLACE FUNCTION hmem_task_has_auto_blockers(task_id_to_check UUID)
RETURNS BOOLEAN AS $$
  WITH RECURSIVE task_descendants(id, status) AS (
    SELECT child.id, child.status
      FROM tasks child
     WHERE child.parent_id = task_id_to_check
       AND child.deleted_at IS NULL
    UNION ALL
    SELECT child.id, child.status
      FROM tasks child
      JOIN task_descendants td ON child.parent_id = td.id
     WHERE child.deleted_at IS NULL
  )
  SELECT hmem_task_has_open_dependencies(task_id_to_check)
    OR EXISTS (
      SELECT 1
        FROM task_descendants
       WHERE hmem_is_open_task_status(status)
    );
$$ LANGUAGE sql STABLE;

-- Legacy databases can already contain open tasks or done tasks with open
-- dependencies inside completed/archived project trees or under done task
-- ancestors.  V012 prevents new writes like that, but does not backfill old
-- rows.  Auto-blocking those rows during this migration would turn closed work
-- back into an open "blocked" state and trip the V012 lifecycle triggers.
-- Treat closed project/task trees as frozen for auto-block recomputation; they
-- can be normalized by reopening the project/task or explicitly editing the
-- affected tasks later.
CREATE OR REPLACE FUNCTION hmem_task_is_inside_closed_project(task_id_to_check UUID)
RETURNS BOOLEAN AS $$
  WITH RECURSIVE task_ancestors(id, parent_id, project_id) AS (
    SELECT task_to_check.id, task_to_check.parent_id, task_to_check.project_id
      FROM tasks task_to_check
     WHERE task_to_check.id = task_id_to_check
       AND task_to_check.deleted_at IS NULL
    UNION ALL
    SELECT parent.id, parent.parent_id, parent.project_id
      FROM tasks parent
      JOIN task_ancestors child ON child.parent_id = parent.id
     WHERE parent.deleted_at IS NULL
  ),
  project_seeds(id) AS (
    SELECT DISTINCT project_id
      FROM task_ancestors
     WHERE project_id IS NOT NULL
  ),
  project_ancestors(id, parent_id, status) AS (
    SELECT project.id, project.parent_id, project.status
      FROM projects project
      JOIN project_seeds seed ON seed.id = project.id
     WHERE project.deleted_at IS NULL
    UNION ALL
    SELECT parent.id, parent.parent_id, parent.status
      FROM projects parent
      JOIN project_ancestors child ON child.parent_id = parent.id
     WHERE parent.deleted_at IS NULL
  )
  SELECT EXISTS (
    SELECT 1
      FROM project_ancestors
     WHERE hmem_is_closed_project_status(status)
  );
$$ LANGUAGE sql STABLE;

CREATE OR REPLACE FUNCTION hmem_task_has_done_ancestor(task_id_to_check UUID)
RETURNS BOOLEAN AS $$
  WITH RECURSIVE task_ancestors(id, parent_id, status) AS (
    SELECT parent.id, parent.parent_id, parent.status
      FROM tasks task_to_check
      JOIN tasks parent ON parent.id = task_to_check.parent_id
     WHERE task_to_check.id = task_id_to_check
       AND task_to_check.deleted_at IS NULL
       AND parent.deleted_at IS NULL
    UNION ALL
    SELECT parent.id, parent.parent_id, parent.status
      FROM tasks parent
      JOIN task_ancestors child ON child.parent_id = parent.id
     WHERE parent.deleted_at IS NULL
  )
  SELECT EXISTS (
    SELECT 1
      FROM task_ancestors
     WHERE status = 'done'::task_status_enum
  );
$$ LANGUAGE sql STABLE;

CREATE OR REPLACE FUNCTION hmem_enforce_task_dependency_workspace()
RETURNS TRIGGER AS $$
DECLARE
  task_workspace_id UUID;
  dependency_workspace_id UUID;
BEGIN
  SELECT workspace_id INTO task_workspace_id
    FROM tasks
   WHERE id = NEW.task_id;

  SELECT workspace_id INTO dependency_workspace_id
    FROM tasks
   WHERE id = NEW.depends_on_id;

  IF task_workspace_id IS DISTINCT FROM dependency_workspace_id THEN
    RAISE EXCEPTION USING
      ERRCODE = '23514',
      MESSAGE = 'Task dependencies must stay within a workspace.';
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Recompute automatic blocked/todo status for a set of changed tasks, their
-- active ancestors, and tasks that transitively depend on the changed tasks.
-- The loop reaches a fixed point because updating one task can affect other
-- tasks through parent and dependency edges.
CREATE OR REPLACE FUNCTION hmem_recompute_task_auto_blocking(seed_ids UUID[])
RETURNS VOID AS $$
DECLARE
  changed_count INTEGER := 0;
BEGIN
  IF seed_ids IS NULL OR array_length(seed_ids, 1) IS NULL THEN
    RETURN;
  END IF;

  LOOP
    WITH RECURSIVE seeds(id) AS (
      SELECT DISTINCT seed_id
        FROM unnest(seed_ids) AS seed_id
       WHERE seed_id IS NOT NULL
    ),
    dependency_dependents(id) AS (
      SELECT td.task_id
        FROM task_dependencies td
        JOIN seeds s ON s.id = td.depends_on_id
        JOIN tasks dependent ON dependent.id = td.task_id
       WHERE dependent.deleted_at IS NULL
      UNION
      SELECT td.task_id
        FROM task_dependencies td
        JOIN dependency_dependents dd ON dd.id = td.depends_on_id
        JOIN tasks dependent ON dependent.id = td.task_id
       WHERE dependent.deleted_at IS NULL
    ),
    direct_targets(id) AS (
      SELECT t.id
        FROM tasks t
        JOIN seeds s ON s.id = t.id
       WHERE t.deleted_at IS NULL
      UNION
      SELECT id FROM dependency_dependents
    ),
    affected_tasks(id, parent_id) AS (
      SELECT t.id, t.parent_id
        FROM tasks t
        JOIN direct_targets dt ON dt.id = t.id
       WHERE t.deleted_at IS NULL
      UNION
      SELECT parent.id, parent.parent_id
        FROM tasks parent
        JOIN affected_tasks child ON child.parent_id = parent.id
       WHERE parent.deleted_at IS NULL
    ),
    targets(id) AS (
      SELECT DISTINCT id FROM affected_tasks
    ),
    candidate_states AS (
      SELECT task_to_check.id,
             task_to_check.status,
             task_to_check.auto_blocked,
             hmem_task_has_auto_blockers(task_to_check.id) AS has_blockers,
             hmem_task_has_open_dependencies(task_to_check.id) AS has_open_dependencies
        FROM tasks task_to_check
        JOIN targets ON targets.id = task_to_check.id
       WHERE task_to_check.deleted_at IS NULL
         AND NOT hmem_task_is_inside_closed_project(task_to_check.id)
         AND NOT hmem_task_has_done_ancestor(task_to_check.id)
    )
    UPDATE tasks task_to_update
       SET status = CASE
             WHEN candidate_states.has_blockers
               AND task_to_update.status IN (
                 'todo'::task_status_enum,
                 'in_progress'::task_status_enum
               )
               THEN 'blocked'::task_status_enum
             WHEN candidate_states.has_open_dependencies
               AND task_to_update.status = 'done'::task_status_enum
               THEN 'blocked'::task_status_enum
             WHEN NOT candidate_states.has_blockers
               AND task_to_update.status = 'blocked'::task_status_enum
               AND task_to_update.auto_blocked
               THEN 'todo'::task_status_enum
             ELSE task_to_update.status
           END,
           auto_blocked = CASE
             WHEN candidate_states.has_blockers
               AND task_to_update.status IN (
                 'todo'::task_status_enum,
                 'in_progress'::task_status_enum
               )
               THEN true
             WHEN candidate_states.has_open_dependencies
               AND task_to_update.status = 'done'::task_status_enum
               THEN true
             WHEN task_to_update.status <> 'blocked'::task_status_enum
               AND task_to_update.auto_blocked
               THEN false
             WHEN NOT candidate_states.has_blockers
               AND task_to_update.auto_blocked
               THEN false
             ELSE task_to_update.auto_blocked
           END
      FROM candidate_states
     WHERE task_to_update.id = candidate_states.id
       AND (
         (candidate_states.has_blockers
           AND task_to_update.status IN (
             'todo'::task_status_enum,
             'in_progress'::task_status_enum
           ))
         OR
         (candidate_states.has_open_dependencies
           AND task_to_update.status = 'done'::task_status_enum)
         OR
         (task_to_update.status <> 'blocked'::task_status_enum
           AND task_to_update.auto_blocked)
         OR
         (NOT candidate_states.has_blockers
           AND task_to_update.status = 'blocked'::task_status_enum
           AND task_to_update.auto_blocked)
       );

    GET DIAGNOSTICS changed_count = ROW_COUNT;
    EXIT WHEN changed_count = 0;
  END LOOP;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION hmem_recompute_task_auto_blocking_from_task()
RETURNS TRIGGER AS $$
BEGIN
  -- Avoid recursive trigger cascades for the internal UPDATEs performed by
  -- hmem_recompute_task_auto_blocking; that function already loops to a fixed
  -- point for the affected graph.
  IF pg_trigger_depth() > 1 THEN
    IF TG_OP = 'DELETE' THEN
      RETURN OLD;
    END IF;
    RETURN NEW;
  END IF;

  IF TG_OP = 'INSERT' THEN
    PERFORM hmem_recompute_task_auto_blocking(ARRAY[NEW.id, NEW.parent_id]);
    RETURN NEW;
  ELSIF TG_OP = 'UPDATE' THEN
    PERFORM hmem_recompute_task_auto_blocking(ARRAY[NEW.id, OLD.id, NEW.parent_id, OLD.parent_id]);
    RETURN NEW;
  ELSE
    PERFORM hmem_recompute_task_auto_blocking(ARRAY[OLD.id, OLD.parent_id]);
    RETURN OLD;
  END IF;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION hmem_recompute_task_auto_blocking_from_dependency()
RETURNS TRIGGER AS $$
BEGIN
  IF pg_trigger_depth() > 1 THEN
    IF TG_OP = 'DELETE' THEN
      RETURN OLD;
    END IF;
    RETURN NEW;
  END IF;

  IF TG_OP = 'INSERT' THEN
    PERFORM hmem_recompute_task_auto_blocking(ARRAY[NEW.task_id, NEW.depends_on_id]);
    RETURN NEW;
  ELSIF TG_OP = 'UPDATE' THEN
    PERFORM hmem_recompute_task_auto_blocking(ARRAY[NEW.task_id, OLD.task_id, NEW.depends_on_id, OLD.depends_on_id]);
    RETURN NEW;
  ELSE
    PERFORM hmem_recompute_task_auto_blocking(ARRAY[OLD.task_id, OLD.depends_on_id]);
    RETURN OLD;
  END IF;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trg_task_dependencies_same_workspace ON task_dependencies;
CREATE TRIGGER trg_task_dependencies_same_workspace
  BEFORE INSERT OR UPDATE OF task_id, depends_on_id ON task_dependencies
  FOR EACH ROW EXECUTE FUNCTION hmem_enforce_task_dependency_workspace();

-- Backfill existing active task graphs once at migration time.  Manually
-- blocked tasks are preserved because auto_blocked defaults to false; only
-- tasks with derived blockers are marked as auto-blocked.
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

DROP TRIGGER IF EXISTS trg_task_auto_blocking_from_task ON tasks;
CREATE TRIGGER trg_task_auto_blocking_from_task
  AFTER INSERT OR UPDATE OF status, parent_id, deleted_at ON tasks
  FOR EACH ROW EXECUTE FUNCTION hmem_recompute_task_auto_blocking_from_task();

DROP TRIGGER IF EXISTS trg_task_auto_blocking_from_dependency ON task_dependencies;
CREATE TRIGGER trg_task_auto_blocking_from_dependency
  AFTER INSERT OR UPDATE OR DELETE ON task_dependencies
  FOR EACH ROW EXECUTE FUNCTION hmem_recompute_task_auto_blocking_from_dependency();

COMMIT;
