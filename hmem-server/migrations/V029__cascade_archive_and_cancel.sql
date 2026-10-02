BEGIN;

-- Prospective lifecycle changes only: no historical rows are normalized here.
-- Rel8 writers include status in their SET list even for metadata edits; their
-- transaction-local intent preserves that distinction. Plain SQL UPDATE OF
-- status is explicit by default, including repeated terminal-status requests.
CREATE OR REPLACE FUNCTION hmem_cascade_task_cancellation()
RETURNS TRIGGER AS $$
DECLARE
  subtree_ids UUID[];
  changed_ids UUID[];
  prior_seeds UUID[];
  locked_id UUID;
BEGIN
  IF NEW.deleted_at IS NOT NULL OR NEW.status <> 'cancelled'::task_status_enum
     OR pg_trigger_depth() > 1
     OR current_setting('hmem.status_intent', true) = 'unchanged' THEN
    RETURN NEW;
  END IF;

  PERFORM hmem_lock_project_ancestors(hmem_task_implicated_project_ids(NEW.project_id, NEW.parent_id));
  PERFORM hmem_lock_task_ancestors(NEW.id);
  FOR locked_id IN
    WITH RECURSIVE subtree(id, depth) AS (
      SELECT t.id, 0 FROM tasks t
       WHERE t.id = NEW.id AND t.deleted_at IS NULL
      UNION ALL
      SELECT child.id, parent.depth + 1 FROM tasks child
        JOIN subtree parent ON child.parent_id = parent.id
       WHERE child.deleted_at IS NULL AND child.workspace_id = NEW.workspace_id
    )
    SELECT t.id FROM tasks t JOIN subtree s ON s.id = t.id
     ORDER BY s.depth, t.id FOR UPDATE OF t
  LOOP
    NULL;
  END LOOP;

  WITH RECURSIVE subtree(id) AS (
    SELECT NEW.id
    UNION
    SELECT child.id FROM tasks child JOIN subtree parent ON child.parent_id = parent.id
     WHERE child.deleted_at IS NULL AND child.workspace_id = NEW.workspace_id
  ) SELECT array_agg(id) INTO subtree_ids FROM subtree;

  WITH changed AS (
    UPDATE tasks SET status = 'cancelled'::task_status_enum, auto_blocked = false
     WHERE id = ANY(subtree_ids) AND id <> NEW.id AND deleted_at IS NULL
       AND status NOT IN ('done'::task_status_enum, 'cancelled'::task_status_enum)
     RETURNING id
  ) SELECT coalesce(array_agg(id), ARRAY[]::UUID[]) INTO changed_ids FROM changed;

  -- Nested row triggers deliberately skip readiness recomputation. The root
  -- AFTER trigger consumes all actual cancellations together with its own row.
  prior_seeds := coalesce(nullif(current_setting('hmem.cascade_task_seeds', true), '')::UUID[], ARRAY[]::UUID[]);
  PERFORM set_config('hmem.cascade_task_seeds', (prior_seeds || changed_ids)::text, true);
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Outer statement targets have all been written before AFTER propagation.
-- Run ahead of the cancelled-parent, readiness and closure checks; rewriting
-- a pending outer target from a BEFORE trigger would violate executor rules.
CREATE TRIGGER trg_task_0_cascade_cancellation
  AFTER UPDATE OF status ON tasks
  FOR EACH ROW EXECUTE FUNCTION hmem_cascade_task_cancellation();

CREATE OR REPLACE FUNCTION hmem_cascade_project_archival()
RETURNS TRIGGER AS $$
DECLARE
  project_ids UUID[];
  task_ids UUID[];
  changed_task_ids UUID[];
  locked_id UUID;
BEGIN
  IF NEW.deleted_at IS NOT NULL OR NEW.status <> 'archived'::project_status_enum
     OR pg_trigger_depth() > 1
     OR current_setting('hmem.status_intent', true) = 'unchanged' THEN
    RETURN NEW;
  END IF;

  PERFORM hmem_lock_project_ancestors(ARRAY[NEW.id]);
  FOR locked_id IN
    WITH RECURSIVE subtree(id, depth) AS (
      SELECT p.id, 0 FROM projects p WHERE p.id = NEW.id AND p.deleted_at IS NULL
      UNION ALL
      SELECT child.id, parent.depth + 1 FROM projects child JOIN subtree parent ON child.parent_id = parent.id
       WHERE child.deleted_at IS NULL AND child.workspace_id = NEW.workspace_id
    )
    SELECT p.id FROM projects p JOIN subtree s ON s.id = p.id
     ORDER BY s.depth, p.id FOR UPDATE OF p
  LOOP
    NULL;
  END LOOP;
  WITH RECURSIVE subtree(id) AS (
    SELECT NEW.id
    UNION
    SELECT child.id FROM projects child JOIN subtree parent ON child.parent_id = parent.id
     WHERE child.deleted_at IS NULL AND child.workspace_id = NEW.workspace_id
  ) SELECT array_agg(id) INTO project_ids FROM subtree;

  FOR locked_id IN
    WITH RECURSIVE subtree(id, depth) AS (
      SELECT t.id, 0 FROM tasks t WHERE t.project_id = ANY(project_ids) AND t.deleted_at IS NULL AND t.workspace_id = NEW.workspace_id
      UNION ALL
      SELECT child.id, parent.depth + 1 FROM tasks child JOIN subtree parent ON child.parent_id = parent.id
       WHERE child.deleted_at IS NULL AND child.workspace_id = NEW.workspace_id
    ), ordered AS (SELECT id, max(depth) AS depth FROM subtree GROUP BY id)
    SELECT t.id FROM tasks t JOIN ordered s ON s.id = t.id
     ORDER BY s.depth, t.id FOR UPDATE OF t
  LOOP
    NULL;
  END LOOP;
  WITH RECURSIVE subtree(id) AS (
    SELECT t.id FROM tasks t WHERE t.project_id = ANY(project_ids) AND t.deleted_at IS NULL AND t.workspace_id = NEW.workspace_id
    UNION
    SELECT child.id FROM tasks child JOIN subtree parent ON child.parent_id = parent.id
     WHERE child.deleted_at IS NULL AND child.workspace_id = NEW.workspace_id
  ) SELECT coalesce(array_agg(id), ARRAY[]::UUID[]) INTO task_ids FROM subtree;

  WITH changed AS (
    UPDATE tasks SET status = 'cancelled'::task_status_enum, auto_blocked = false
     WHERE id = ANY(task_ids) AND deleted_at IS NULL
       AND status NOT IN ('done'::task_status_enum, 'cancelled'::task_status_enum)
     RETURNING id
  ) SELECT coalesce(array_agg(id), ARRAY[]::UUID[]) INTO changed_task_ids FROM changed;

  -- A set-based statement finishes every descendant before its queued AFTER
  -- lifecycle checks run, including previously completed descendant projects.
  UPDATE projects SET status = 'archived'::project_status_enum
   WHERE id = ANY(project_ids) AND id <> NEW.id AND deleted_at IS NULL
     AND status <> 'archived'::project_status_enum;
  PERFORM hmem_recompute_task_auto_blocking(changed_task_ids);
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_project_cascade_archival
  AFTER UPDATE OF status ON projects
  FOR EACH ROW EXECUTE FUNCTION hmem_cascade_project_archival();

-- Keep the established done-parent error distinct from cancellation.
CREATE OR REPLACE FUNCTION hmem_enforce_cancelled_task_parent()
RETURNS TRIGGER AS $$
DECLARE
  blocker_ids UUID[];
BEGIN
  IF NEW.deleted_at IS NOT NULL OR NOT hmem_task_subtree_has_open_tasks(NEW.id) THEN
    RETURN NEW;
  END IF;
  PERFORM hmem_lock_project_ancestors(hmem_task_implicated_project_ids(NEW.project_id, NEW.parent_id));
  PERFORM hmem_lock_task_ancestors(NEW.parent_id);
  WITH RECURSIVE ancestors(id, parent_id, status) AS (
    SELECT t.id, t.parent_id, t.status FROM tasks t WHERE t.id = NEW.parent_id AND t.deleted_at IS NULL
    UNION ALL
    SELECT t.id, t.parent_id, t.status FROM tasks t JOIN ancestors a ON t.id = a.parent_id WHERE t.deleted_at IS NULL
  ) SELECT coalesce(array_agg(id ORDER BY id), ARRAY[]::UUID[]) INTO blocker_ids FROM ancestors WHERE status = 'cancelled'::task_status_enum;
  IF cardinality(blocker_ids) > 0 THEN
    RAISE EXCEPTION USING ERRCODE = 'HM106',
      MESSAGE = 'Cannot place an unfinished subtask under a cancelled task.',
      DETAIL = jsonb_build_object('blocker_count', cardinality(blocker_ids), 'blocker_ids', blocker_ids[1:5])::text,
      HINT = 'Reopen the parent task before adding, moving, or reopening unfinished subtasks.';
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_task_1_cancelled_parent
  AFTER INSERT OR UPDATE OF status, parent_id, project_id, deleted_at ON tasks
  FOR EACH ROW EXECUTE FUNCTION hmem_enforce_cancelled_task_parent();

CREATE OR REPLACE FUNCTION hmem_task_has_closed_ancestor(task_id_to_check UUID)
RETURNS BOOLEAN AS $$
  WITH RECURSIVE ancestors(id, parent_id, status) AS (
    SELECT parent.id, parent.parent_id, parent.status FROM tasks child
      JOIN tasks parent ON parent.id = child.parent_id
     WHERE child.id = task_id_to_check AND parent.deleted_at IS NULL
    UNION ALL
    SELECT parent.id, parent.parent_id, parent.status FROM tasks parent JOIN ancestors child ON parent.id = child.parent_id WHERE parent.deleted_at IS NULL
  ) SELECT EXISTS (SELECT 1 FROM ancestors WHERE status IN ('done'::task_status_enum, 'cancelled'::task_status_enum));
$$ LANGUAGE sql STABLE;
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
         AND NOT hmem_task_has_closed_ancestor(task_to_check.id)
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
DECLARE
  cascade_seeds UUID[];
BEGIN
  IF pg_trigger_depth() > 1 THEN
    IF TG_OP = 'DELETE' THEN RETURN OLD; END IF;
    RETURN NEW;
  END IF;
  IF TG_OP = 'INSERT' THEN
    PERFORM hmem_recompute_task_auto_blocking(ARRAY[NEW.id, NEW.parent_id]);
    RETURN NEW;
  ELSIF TG_OP = 'UPDATE' THEN
    cascade_seeds := coalesce(nullif(current_setting('hmem.cascade_task_seeds', true), '')::UUID[], ARRAY[]::UUID[]);
    PERFORM set_config('hmem.cascade_task_seeds', '', true);
    PERFORM hmem_recompute_task_auto_blocking(cascade_seeds || ARRAY[NEW.id, OLD.id, NEW.parent_id, OLD.parent_id]);
    RETURN NEW;
  ELSE
    PERFORM hmem_recompute_task_auto_blocking(ARRAY[OLD.id, OLD.parent_id]);
    RETURN OLD;
  END IF;
END;
$$ LANGUAGE plpgsql;

COMMIT;
