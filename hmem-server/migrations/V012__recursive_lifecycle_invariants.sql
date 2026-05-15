BEGIN;

------------------------------------------------------------------------
-- Recursive lifecycle invariant helpers
------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION hmem_is_open_task_status(task_status_value task_status_enum)
RETURNS BOOLEAN AS $$
  SELECT task_status_value IN (
    'todo'::task_status_enum,
    'in_progress'::task_status_enum,
    'blocked'::task_status_enum
  );
$$ LANGUAGE sql IMMUTABLE;

CREATE OR REPLACE FUNCTION hmem_is_open_project_status(project_status_value project_status_enum)
RETURNS BOOLEAN AS $$
  SELECT project_status_value IN (
    'active'::project_status_enum,
    'paused'::project_status_enum
  );
$$ LANGUAGE sql IMMUTABLE;

CREATE OR REPLACE FUNCTION hmem_is_closed_project_status(project_status_value project_status_enum)
RETURNS BOOLEAN AS $$
  SELECT project_status_value IN (
    'completed'::project_status_enum,
    'archived'::project_status_enum
  );
$$ LANGUAGE sql IMMUTABLE;

-- Lock project ancestors in a deterministic root-to-leaf order.  The CTE
-- first walks leaf-to-root from each seed, then computes a root-depth value
-- so the final FOR UPDATE locks project rows in a stable order.
CREATE OR REPLACE FUNCTION hmem_lock_project_ancestors(start_ids UUID[])
RETURNS VOID AS $$
DECLARE
  project_id_to_lock UUID;
BEGIN
  IF start_ids IS NULL OR array_length(start_ids, 1) IS NULL THEN
    RETURN;
  END IF;

  FOR project_id_to_lock IN
    WITH RECURSIVE seeds(id) AS (
      SELECT DISTINCT seed_id
        FROM unnest(start_ids) AS seed_id
       WHERE seed_id IS NOT NULL
    ),
    ancestors(seed_id, id, parent_id, distance_from_leaf) AS (
      SELECT s.id, p.id, p.parent_id, 0
        FROM seeds s
        JOIN projects p ON p.id = s.id
       WHERE p.deleted_at IS NULL
      UNION ALL
      SELECT a.seed_id, p.id, p.parent_id, a.distance_from_leaf + 1
        FROM projects p
        JOIN ancestors a ON p.id = a.parent_id
       WHERE p.deleted_at IS NULL
    ),
    rooted AS (
      SELECT id,
             max(distance_from_leaf) OVER (PARTITION BY seed_id) - distance_from_leaf AS depth
        FROM ancestors
    )
    SELECT id
      FROM rooted
     GROUP BY id
     ORDER BY min(depth) ASC, id ASC
  LOOP
    PERFORM 1 FROM projects WHERE id = project_id_to_lock FOR UPDATE;
  END LOOP;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION hmem_lock_task_ancestors(start_id UUID)
RETURNS VOID AS $$
DECLARE
  task_id_to_lock UUID;
BEGIN
  IF start_id IS NULL THEN
    RETURN;
  END IF;

  FOR task_id_to_lock IN
    WITH RECURSIVE ancestors(id, parent_id, distance_from_leaf) AS (
      SELECT t.id, t.parent_id, 0
        FROM tasks t
       WHERE t.id = start_id
         AND t.deleted_at IS NULL
      UNION ALL
      SELECT parent.id, parent.parent_id, ancestors.distance_from_leaf + 1
        FROM tasks parent
        JOIN ancestors ON parent.id = ancestors.parent_id
       WHERE parent.deleted_at IS NULL
    ),
    rooted AS (
      SELECT id,
             max(distance_from_leaf) OVER () - distance_from_leaf AS depth
        FROM ancestors
    )
    SELECT id
      FROM rooted
     GROUP BY id
     ORDER BY min(depth) ASC, id ASC
  LOOP
    PERFORM 1 FROM tasks WHERE id = task_id_to_lock FOR UPDATE;
  END LOOP;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION hmem_task_implicated_project_ids(task_project_id UUID, task_parent_id UUID)
RETURNS UUID[] AS $$
DECLARE
  project_ids UUID[];
BEGIN
  WITH RECURSIVE task_ancestors(id, parent_id, project_id) AS (
    SELECT t.id, t.parent_id, t.project_id
      FROM tasks t
     WHERE t.id = task_parent_id
       AND t.deleted_at IS NULL
    UNION ALL
    SELECT parent.id, parent.parent_id, parent.project_id
      FROM tasks parent
      JOIN task_ancestors ta ON parent.id = ta.parent_id
     WHERE parent.deleted_at IS NULL
  ),
  project_candidates(project_id) AS (
    SELECT task_project_id
     WHERE task_project_id IS NOT NULL
    UNION
    SELECT project_id
      FROM task_ancestors
     WHERE project_id IS NOT NULL
  )
  SELECT coalesce(array_agg(DISTINCT project_id ORDER BY project_id), ARRAY[]::UUID[])
    INTO project_ids
    FROM project_candidates;

  RETURN project_ids;
END;
$$ LANGUAGE plpgsql STABLE;

CREATE OR REPLACE FUNCTION hmem_task_subtree_has_open_tasks(root_task_id UUID)
RETURNS BOOLEAN AS $$
  WITH RECURSIVE task_tree(id, status) AS (
    SELECT t.id, t.status
      FROM tasks t
     WHERE t.id = root_task_id
       AND t.deleted_at IS NULL
    UNION ALL
    SELECT child.id, child.status
      FROM tasks child
      JOIN task_tree tt ON child.parent_id = tt.id
     WHERE child.deleted_at IS NULL
  )
  SELECT EXISTS (
    SELECT 1
      FROM task_tree
     WHERE hmem_is_open_task_status(status)
  );
$$ LANGUAGE sql STABLE;

------------------------------------------------------------------------
-- Task lifecycle invariant
------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION hmem_enforce_task_lifecycle()
RETURNS TRIGGER AS $$
DECLARE
  blocker_count INTEGER := 0;
  blocker_ids UUID[] := ARRAY[]::UUID[];
  project_ids UUID[] := ARRAY[]::UUID[];
BEGIN
  -- Soft-deleted rows are outside the active lifecycle invariant.  Restore
  -- updates have NEW.deleted_at IS NULL and therefore validate like inserts.
  IF NEW.deleted_at IS NOT NULL THEN
    RETURN NEW;
  END IF;

  IF hmem_task_subtree_has_open_tasks(NEW.id) THEN
    project_ids := hmem_task_implicated_project_ids(NEW.project_id, NEW.parent_id);
    PERFORM hmem_lock_project_ancestors(project_ids);
    PERFORM hmem_lock_task_ancestors(NEW.parent_id);

    WITH RECURSIVE task_ancestors(id, parent_id, status) AS (
      SELECT t.id, t.parent_id, t.status
        FROM tasks t
       WHERE t.id = NEW.parent_id
         AND t.deleted_at IS NULL
      UNION ALL
      SELECT parent.id, parent.parent_id, parent.status
        FROM tasks parent
        JOIN task_ancestors ta ON parent.id = ta.parent_id
       WHERE parent.deleted_at IS NULL
    ),
    done_ancestors AS (
      SELECT id
        FROM task_ancestors
       WHERE status = 'done'::task_status_enum
    )
    SELECT count(*)::INTEGER,
           coalesce(ARRAY(SELECT id FROM done_ancestors ORDER BY id LIMIT 5), ARRAY[]::UUID[])
      INTO blocker_count, blocker_ids
      FROM done_ancestors;

    IF blocker_count > 0 THEN
      RAISE EXCEPTION USING
        ERRCODE = 'HM102',
        MESSAGE = 'Cannot place an open task under a done task.',
        DETAIL = jsonb_build_object(
          'blocker_count', blocker_count,
          'blocker_ids', blocker_ids
        )::text,
        HINT = 'Reopen the ancestor task before adding or reopening open subtasks.';
    END IF;

    WITH RECURSIVE task_ancestors(id, parent_id, project_id) AS (
      SELECT t.id, t.parent_id, t.project_id
        FROM tasks t
       WHERE t.id = NEW.parent_id
         AND t.deleted_at IS NULL
      UNION ALL
      SELECT parent.id, parent.parent_id, parent.project_id
        FROM tasks parent
        JOIN task_ancestors ta ON parent.id = ta.parent_id
       WHERE parent.deleted_at IS NULL
    ),
    project_candidates(project_id) AS (
      SELECT NEW.project_id
       WHERE NEW.project_id IS NOT NULL
      UNION
      SELECT project_id
        FROM task_ancestors
       WHERE project_id IS NOT NULL
    ),
    project_ancestors(id, parent_id, status) AS (
      SELECT p.id, p.parent_id, p.status
        FROM projects p
        JOIN project_candidates pc ON pc.project_id = p.id
       WHERE p.deleted_at IS NULL
      UNION
      SELECT parent.id, parent.parent_id, parent.status
        FROM projects parent
        JOIN project_ancestors pa ON parent.id = pa.parent_id
       WHERE parent.deleted_at IS NULL
    ),
    closed_projects AS (
      SELECT id
        FROM project_ancestors
       WHERE hmem_is_closed_project_status(status)
    )
    SELECT count(*)::INTEGER,
           coalesce(ARRAY(SELECT id FROM closed_projects ORDER BY id LIMIT 5), ARRAY[]::UUID[])
      INTO blocker_count, blocker_ids
      FROM closed_projects;

    IF blocker_count > 0 THEN
      RAISE EXCEPTION USING
        ERRCODE = 'HM203',
        MESSAGE = 'Cannot place an open task inside a completed or archived project.',
        DETAIL = jsonb_build_object(
          'blocker_count', blocker_count,
          'blocker_ids', blocker_ids
        )::text,
        HINT = 'Reopen the project before adding or reopening open tasks.';
    END IF;
  END IF;

  IF NEW.status = 'done'::task_status_enum THEN
    WITH RECURSIVE task_descendants(id, status) AS (
      SELECT child.id, child.status
        FROM tasks child
       WHERE child.parent_id = NEW.id
         AND child.deleted_at IS NULL
      UNION ALL
      SELECT child.id, child.status
        FROM tasks child
        JOIN task_descendants td ON child.parent_id = td.id
       WHERE child.deleted_at IS NULL
    ),
    open_descendants AS (
      SELECT id
        FROM task_descendants
       WHERE hmem_is_open_task_status(status)
    )
    SELECT count(*)::INTEGER,
           coalesce(ARRAY(SELECT id FROM open_descendants ORDER BY id LIMIT 5), ARRAY[]::UUID[])
      INTO blocker_count, blocker_ids
      FROM open_descendants;

    IF blocker_count > 0 THEN
      RAISE EXCEPTION USING
        ERRCODE = 'HM101',
        MESSAGE = 'Cannot mark task done while descendant tasks are still open.',
        DETAIL = jsonb_build_object(
          'blocker_count', blocker_count,
          'blocker_ids', blocker_ids
        )::text,
        HINT = 'Complete or cancel open descendant tasks first.';
    END IF;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trg_task_lifecycle_invariants ON tasks;
CREATE TRIGGER trg_task_lifecycle_invariants
  AFTER INSERT OR UPDATE OF status, parent_id, project_id, deleted_at ON tasks
  FOR EACH ROW EXECUTE FUNCTION hmem_enforce_task_lifecycle();

------------------------------------------------------------------------
-- Project lifecycle invariant
------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION hmem_enforce_project_lifecycle()
RETURNS TRIGGER AS $$
DECLARE
  open_project_count INTEGER := 0;
  open_project_ids UUID[] := ARRAY[]::UUID[];
  open_task_count INTEGER := 0;
  open_task_ids UUID[] := ARRAY[]::UUID[];
BEGIN
  -- Soft-deleted rows are outside the active lifecycle invariant.  Restore
  -- updates have NEW.deleted_at IS NULL and therefore validate like inserts.
  IF NEW.deleted_at IS NOT NULL THEN
    RETURN NEW;
  END IF;

  IF hmem_is_open_project_status(NEW.status) THEN
    PERFORM hmem_lock_project_ancestors(ARRAY[NEW.parent_id]);

    WITH RECURSIVE project_ancestors(id, parent_id, status) AS (
      SELECT p.id, p.parent_id, p.status
        FROM projects p
       WHERE p.id = NEW.parent_id
         AND p.deleted_at IS NULL
      UNION ALL
      SELECT parent.id, parent.parent_id, parent.status
        FROM projects parent
        JOIN project_ancestors pa ON parent.id = pa.parent_id
       WHERE parent.deleted_at IS NULL
    ),
    closed_projects AS (
      SELECT id
        FROM project_ancestors
       WHERE hmem_is_closed_project_status(status)
    )
    SELECT count(*)::INTEGER,
           coalesce(ARRAY(SELECT id FROM closed_projects ORDER BY id LIMIT 5), ARRAY[]::UUID[])
      INTO open_project_count, open_project_ids
      FROM closed_projects;

    IF open_project_count > 0 THEN
      RAISE EXCEPTION USING
        ERRCODE = 'HM202',
        MESSAGE = 'Cannot place an active or paused project under a completed or archived project.',
        DETAIL = jsonb_build_object(
          'blocker_count', open_project_count,
          'blocker_ids', open_project_ids
        )::text,
        HINT = 'Reopen the parent project before adding or reopening active child projects.';
    END IF;
  END IF;

  IF hmem_is_closed_project_status(NEW.status) THEN
    WITH RECURSIVE project_tree(id, status) AS (
      SELECT p.id, p.status
        FROM projects p
       WHERE p.parent_id = NEW.id
         AND p.deleted_at IS NULL
      UNION ALL
      SELECT child.id, child.status
        FROM projects child
        JOIN project_tree pt ON child.parent_id = pt.id
       WHERE child.deleted_at IS NULL
    ),
    open_projects AS (
      SELECT id
        FROM project_tree
       WHERE hmem_is_open_project_status(status)
    )
    SELECT count(*)::INTEGER,
           coalesce(ARRAY(SELECT id FROM open_projects ORDER BY id LIMIT 5), ARRAY[]::UUID[])
      INTO open_project_count, open_project_ids
      FROM open_projects;

    WITH RECURSIVE project_tree(id) AS (
      SELECT NEW.id
      UNION ALL
      SELECT child.id
        FROM projects child
        JOIN project_tree pt ON child.parent_id = pt.id
       WHERE child.deleted_at IS NULL
    ),
    seed_tasks(id, status) AS (
      SELECT t.id, t.status
        FROM tasks t
        JOIN project_tree pt ON t.project_id = pt.id
       WHERE t.deleted_at IS NULL
    ),
    task_tree(id, status) AS (
      SELECT id, status FROM seed_tasks
      UNION
      SELECT child.id, child.status
        FROM tasks child
        JOIN task_tree tt ON child.parent_id = tt.id
       WHERE child.deleted_at IS NULL
    ),
    open_tasks AS (
      SELECT id
        FROM task_tree
       WHERE hmem_is_open_task_status(status)
    )
    SELECT count(*)::INTEGER,
           coalesce(ARRAY(SELECT id FROM open_tasks ORDER BY id LIMIT 5), ARRAY[]::UUID[])
      INTO open_task_count, open_task_ids
      FROM open_tasks;

    IF open_project_count > 0 OR open_task_count > 0 THEN
      RAISE EXCEPTION USING
        ERRCODE = 'HM201',
        MESSAGE = 'Cannot close project while descendant projects or tasks are still open.',
        DETAIL = jsonb_build_object(
          'open_project_count', open_project_count,
          'open_project_ids', open_project_ids,
          'open_task_count', open_task_count,
          'open_task_ids', open_task_ids
        )::text,
        HINT = 'Complete or archive child projects and complete or cancel open tasks first.';
    END IF;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trg_project_lifecycle_invariants ON projects;
CREATE TRIGGER trg_project_lifecycle_invariants
  AFTER INSERT OR UPDATE OF status, parent_id, deleted_at ON projects
  FOR EACH ROW EXECUTE FUNCTION hmem_enforce_project_lifecycle();

COMMIT;
