BEGIN;

-- Prospective rules only; V036 owns normalization of historical pause intent.
ALTER TABLE projects ADD COLUMN own_paused BOOLEAN NOT NULL DEFAULT false;

-- Serialize planning writes before PostgreSQL acquires their tuple locks.
-- A single planning gate deliberately also covers unassigned tasks and raw
-- multi-workspace SQL; workspace-specific gates cannot order those statements.
-- Application snapshot writers take this same gate before SELECT FOR UPDATE.
CREATE FUNCTION hmem_lock_planning_lifecycle() RETURNS VOID AS $$
  SELECT pg_advisory_xact_lock(1751999853, 35);
$$ LANGUAGE sql VOLATILE;
CREATE FUNCTION hmem_planning_write_gate() RETURNS TRIGGER AS $$
BEGIN
  PERFORM hmem_lock_planning_lifecycle();
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;
CREATE TRIGGER trg_project_0_write_gate BEFORE INSERT OR UPDATE OR DELETE ON projects
  FOR EACH STATEMENT EXECUTE FUNCTION hmem_planning_write_gate();
CREATE TRIGGER trg_task_0_write_gate BEFORE INSERT OR UPDATE OR DELETE ON tasks
  FOR EACH STATEMENT EXECUTE FUNCTION hmem_planning_write_gate();
CREATE TRIGGER trg_dependency_0_write_gate BEFORE INSERT OR UPDATE OR DELETE ON task_dependencies
  FOR EACH STATEMENT EXECUTE FUNCTION hmem_planning_write_gate();

CREATE FUNCTION hmem_project_has_pause_intent(project_id_to_check UUID)
RETURNS BOOLEAN AS $$
  WITH RECURSIVE ancestors(id, parent_id, own_paused) AS (
    SELECT id, parent_id, own_paused FROM projects
     WHERE id = project_id_to_check AND deleted_at IS NULL
    UNION
    SELECT p.id, p.parent_id, p.own_paused FROM projects p
      JOIN ancestors a ON p.id = a.parent_id WHERE p.deleted_at IS NULL
  ) SELECT EXISTS (SELECT 1 FROM ancestors WHERE own_paused);
$$ LANGUAGE sql STABLE;

CREATE FUNCTION hmem_projects_have_paused_ancestry(project_ids UUID[])
RETURNS BOOLEAN AS $$
  WITH RECURSIVE ancestors(id, parent_id, status) AS (
    SELECT id, parent_id, status FROM projects
     WHERE id = ANY(project_ids) AND deleted_at IS NULL
    UNION
    SELECT p.id, p.parent_id, p.status FROM projects p
      JOIN ancestors a ON p.id = a.parent_id WHERE p.deleted_at IS NULL
  ) SELECT EXISTS (SELECT 1 FROM ancestors WHERE status = 'paused'::project_status_enum);
$$ LANGUAGE sql STABLE;

CREATE FUNCTION hmem_task_is_inside_paused_project(task_id_to_check UUID)
RETURNS BOOLEAN AS $$
  SELECT coalesce(hmem_projects_have_paused_ancestry(
    hmem_task_implicated_project_ids(t.project_id, t.parent_id)), false)
    FROM tasks t WHERE t.id = task_id_to_check AND t.deleted_at IS NULL;
$$ LANGUAGE sql STABLE;

CREATE OR REPLACE FUNCTION hmem_task_has_auto_blockers(task_id_to_check UUID)
RETURNS BOOLEAN AS $$
  SELECT hmem_task_has_open_dependencies(task_id_to_check)
      OR hmem_task_has_open_subtasks(task_id_to_check)
      OR coalesce(hmem_task_is_inside_paused_project(task_id_to_check), false);
$$ LANGUAGE sql STABLE;

CREATE OR REPLACE FUNCTION hmem_task_can_start_subtasks(task_id_to_check UUID)
RETURNS BOOLEAN AS $$
  SELECT EXISTS (SELECT 1 FROM tasks parent
    WHERE parent.id = task_id_to_check AND parent.deleted_at IS NULL
      AND (parent.status IN ('todo'::task_status_enum, 'in_progress'::task_status_enum)
        OR (parent.status = 'blocked'::task_status_enum AND parent.auto_blocked))
      AND NOT hmem_task_has_open_dependencies(parent.id)
      AND NOT hmem_task_has_closed_ancestor(parent.id)
      AND NOT hmem_task_is_inside_closed_project(parent.id)
      AND NOT hmem_task_is_inside_paused_project(parent.id));
$$ LANGUAGE sql STABLE;

-- Track explicit pause intent independently of inherited effective status.
-- Rel8 metadata updates carry unchanged intent; internal cascades retain it.
CREATE FUNCTION hmem_project_pause_intent() RETURNS TRIGGER AS $$
DECLARE
  requested_intent JSONB;
  restored_intent BOOLEAN;
BEGIN
  IF pg_trigger_depth() > 1 THEN RETURN NEW; END IF;
  -- Private audit replay supplies a root-targeted stored intent. It may restore
  -- false even when effective status is already paused and false is unchanged.
  -- This changes intent, never the running/closed/placement lifecycle gates.
  requested_intent := nullif(current_setting('hmem.project_pause_intent', true), '')::jsonb;
  IF requested_intent ->> 'id' = NEW.id::text
     AND jsonb_typeof(requested_intent -> 'own_paused') = 'boolean' THEN
    restored_intent := (requested_intent ->> 'own_paused')::boolean;
  END IF;
  IF TG_OP = 'INSERT' THEN
    NEW.own_paused := NEW.status = 'paused'::project_status_enum;
  ELSIF current_setting('hmem.status_intent', true) IS DISTINCT FROM 'unchanged' THEN
    IF hmem_is_closed_project_status(OLD.status)
       AND (NEW.status = 'paused'::project_status_enum OR coalesce(restored_intent, false))
       AND NEW.deleted_at IS NULL THEN
      RAISE EXCEPTION USING ERRCODE = 'HM205',
        MESSAGE = 'Reopen a completed or archived project before pausing it.',
        DETAIL = jsonb_build_object('blocker_count', 1, 'blocker_ids', ARRAY[NEW.id])::text,
        HINT = 'Set the project to active before requesting paused.';
    END IF;
    NEW.own_paused := coalesce(restored_intent, NEW.status = 'paused'::project_status_enum);
  ELSE
    NEW.own_paused := OLD.own_paused;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
CREATE TRIGGER trg_project_0_pause_intent BEFORE INSERT OR UPDATE OF status ON projects
  FOR EACH ROW EXECUTE FUNCTION hmem_project_pause_intent();

CREATE FUNCTION hmem_project_own_pause_guard() RETURNS TRIGGER AS $$
BEGIN
  IF pg_trigger_depth() = 1 AND NEW.deleted_at IS NULL AND NEW.own_paused
     AND hmem_is_closed_project_status(OLD.status) THEN
    RAISE EXCEPTION USING ERRCODE = 'HM205',
      MESSAGE = 'Reopen a completed or archived project before pausing it.',
      DETAIL = jsonb_build_object('blocker_count', 1, 'blocker_ids', ARRAY[NEW.id])::text,
      HINT = 'Set the project to active before requesting independent pause intent.';
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
CREATE TRIGGER trg_project_0_own_pause_guard BEFORE UPDATE OF own_paused ON projects
  FOR EACH ROW EXECUTE FUNCTION hmem_project_own_pause_guard();

-- Reconcile after all outer statement targets have been written. In particular,
-- never rewrite a pending parent/child tuple from a BEFORE row trigger.
CREATE FUNCTION hmem_reconcile_project_pause(root_ids UUID[], allow_running BOOLEAN DEFAULT false)
RETURNS VOID AS $$
DECLARE
  project_ids UUID[];
  live_project_ids UUID[];
  task_ids UUID[];
  running_ids UUID[];
  locked_id UUID;
  previous_origin TEXT;
BEGIN
  PERFORM hmem_lock_planning_lifecycle();
  PERFORM hmem_lock_project_ancestors(root_ids);
  FOR locked_id IN
    WITH RECURSIVE subtree(id, depth) AS (
      SELECT id, 0 FROM projects WHERE id = ANY(root_ids) AND deleted_at IS NULL
      UNION ALL
      SELECT p.id, s.depth + 1 FROM projects p JOIN subtree s ON p.parent_id = s.id
       WHERE p.deleted_at IS NULL
    ), ordered AS (SELECT id, max(depth) AS depth FROM subtree GROUP BY id)
    SELECT p.id FROM projects p JOIN ordered s ON p.id = s.id
     ORDER BY s.depth, p.id FOR UPDATE OF p
  LOOP NULL; END LOOP;
  WITH RECURSIVE subtree(id) AS (
    SELECT id FROM projects WHERE id = ANY(root_ids) AND deleted_at IS NULL
    UNION
    SELECT p.id FROM projects p JOIN subtree s ON p.parent_id = s.id WHERE p.deleted_at IS NULL
  ) SELECT coalesce(array_agg(id), ARRAY[]::UUID[]) INTO live_project_ids FROM subtree;

  -- Closed project branches freeze their historical open descendants. The
  -- refusal scan below still includes every live descendant in those branches.
  WITH RECURSIVE subtree(id) AS (
    SELECT id FROM projects WHERE id = ANY(root_ids) AND deleted_at IS NULL
      AND hmem_is_open_project_status(status)
    UNION
    SELECT p.id FROM projects p JOIN subtree s ON p.parent_id = s.id
     WHERE p.deleted_at IS NULL AND hmem_is_open_project_status(p.status)
  ) SELECT coalesce(array_agg(id), ARRAY[]::UUID[]) INTO project_ids FROM subtree;

  UPDATE projects p SET status = CASE WHEN hmem_project_has_pause_intent(p.id)
    THEN 'paused'::project_status_enum ELSE 'active'::project_status_enum END
   WHERE p.id = ANY(project_ids) AND hmem_is_open_project_status(p.status)
     AND p.status IS DISTINCT FROM CASE WHEN hmem_project_has_pause_intent(p.id)
       THEN 'paused'::project_status_enum ELSE 'active'::project_status_enum END;

  FOR locked_id IN
    WITH RECURSIVE subtree(id, depth) AS (
      SELECT id, 0 FROM tasks WHERE project_id = ANY(live_project_ids) AND deleted_at IS NULL
      UNION ALL
      SELECT t.id, s.depth + 1 FROM tasks t JOIN subtree s ON t.parent_id = s.id
       WHERE t.deleted_at IS NULL
    ), ordered AS (SELECT id, max(depth) AS depth FROM subtree GROUP BY id)
    SELECT t.id FROM tasks t JOIN ordered s ON t.id = s.id
     ORDER BY s.depth, t.id FOR UPDATE OF t
  LOOP NULL; END LOOP;
  WITH RECURSIVE subtree(id) AS (
    SELECT id FROM tasks WHERE project_id = ANY(live_project_ids) AND deleted_at IS NULL
    UNION
    SELECT t.id FROM tasks t JOIN subtree s ON t.parent_id = s.id WHERE t.deleted_at IS NULL
  ) SELECT coalesce(array_agg(id), ARRAY[]::UUID[]) INTO task_ids FROM subtree;
  SELECT coalesce(array_agg(id ORDER BY id), ARRAY[]::UUID[]) INTO running_ids
    FROM tasks WHERE id = ANY(task_ids) AND status = 'in_progress'::task_status_enum
      AND hmem_task_is_inside_paused_project(id);
  IF NOT allow_running AND cardinality(running_ids) > 0 THEN
    RAISE EXCEPTION USING ERRCODE = 'HM204',
      MESSAGE = 'Cannot pause or place a project while affected descendant tasks are in progress.',
      DETAIL = jsonb_build_object('blocker_count', cardinality(running_ids), 'blocker_ids', running_ids[1:5])::text,
      HINT = 'Finish, cancel, or block running descendant tasks and subtasks before pausing or moving the project.';
  END IF;
  -- Keep task branches beneath terminal task/project ancestors frozen too;
  -- their rows were only read/locked for the prospective running guard.
  SELECT coalesce(array_agg(t.id), ARRAY[]::UUID[]) INTO task_ids
    FROM tasks t WHERE t.id = ANY(task_ids)
      AND NOT hmem_task_is_inside_closed_project(t.id)
      AND NOT hmem_task_has_closed_ancestor(t.id);
  previous_origin := current_setting('hmem.auto_block_origin', true);
  PERFORM set_config('hmem.auto_block_origin', 'project_pause', true);
  PERFORM hmem_recompute_task_auto_blocking(task_ids);
  PERFORM set_config('hmem.auto_block_origin', coalesce(previous_origin, ''), true);
END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION hmem_project_pause_from_row() RETURNS TRIGGER AS $$
BEGIN
  IF NEW.deleted_at IS NOT NULL OR pg_trigger_depth() > 1 THEN RETURN NEW; END IF;
  IF TG_OP = 'UPDATE' AND NEW.parent_id IS NOT DISTINCT FROM OLD.parent_id
     AND NEW.deleted_at IS NOT DISTINCT FROM OLD.deleted_at
     AND NEW.own_paused IS NOT DISTINCT FROM OLD.own_paused
     AND (current_setting('hmem.status_intent', true) = 'unchanged'
          OR NOT hmem_is_open_project_status(NEW.status)) THEN
    RETURN NEW;
  END IF;
  PERFORM hmem_reconcile_project_pause(ARRAY[NEW.id]);
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
CREATE TRIGGER trg_project_0_pause_cascade AFTER INSERT OR UPDATE OF status, own_paused, parent_id, deleted_at ON projects
  FOR EACH ROW EXECUTE FUNCTION hmem_project_pause_from_row();

CREATE FUNCTION hmem_task_paused_placement_guard() RETURNS TRIGGER AS $$
DECLARE running_ids UUID[];
BEGIN
  IF NEW.deleted_at IS NOT NULL OR pg_trigger_depth() > 1 THEN RETURN NEW; END IF;
  -- Include children with NULL/different project IDs: parent placement still
  -- carries project ancestry authority for raw SQL, restores and reparenting.
  WITH RECURSIVE subtree(id) AS (
    SELECT NEW.id
    UNION
    SELECT t.id FROM tasks t JOIN subtree s ON t.parent_id = s.id WHERE t.deleted_at IS NULL
  ) SELECT coalesce(array_agg(t.id ORDER BY t.id), ARRAY[]::UUID[]) INTO running_ids
    FROM tasks t JOIN subtree s ON t.id = s.id
   WHERE t.deleted_at IS NULL AND t.status = 'in_progress'::task_status_enum
     AND hmem_task_is_inside_paused_project(t.id);
  IF cardinality(running_ids) > 0 THEN
    RAISE EXCEPTION USING ERRCODE = 'HM108',
      MESSAGE = 'Cannot start or place running tasks inside a paused project.',
      DETAIL = jsonb_build_object('blocker_count', cardinality(running_ids), 'blocker_ids', running_ids[1:5])::text,
      HINT = 'Resume the project or block the running tasks before moving or restoring them.';
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
CREATE TRIGGER trg_task_1_paused_placement AFTER INSERT OR UPDATE OF status, parent_id, project_id, deleted_at ON tasks
  FOR EACH ROW EXECUTE FUNCTION hmem_task_paused_placement_guard();

-- Project placement changes are readiness changes even without a status edit.
DROP TRIGGER trg_task_auto_blocking_from_task ON tasks;
CREATE TRIGGER trg_task_auto_blocking_from_task
  AFTER INSERT OR UPDATE OF status, parent_id, project_id, deleted_at OR DELETE ON tasks
  FOR EACH ROW EXECUTE FUNCTION hmem_recompute_task_auto_blocking_from_task();

CREATE OR REPLACE FUNCTION hmem_recompute_task_auto_blocking(seed_ids UUID[])
RETURNS VOID AS $$
DECLARE
  changed_count INTEGER := 0;
  preserve_terminal BOOLEAN := current_setting('hmem.auto_block_origin', true) = 'project_pause';
BEGIN
  IF seed_ids IS NULL OR array_length(seed_ids, 1) IS NULL THEN
    RETURN;
  END IF;

  preserve_terminal := coalesce(preserve_terminal, false);

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
         AND (NOT preserve_terminal OR hmem_is_open_task_status(task_to_check.status))
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
  seed_ids UUID[];
  cascade_seeds UUID[];
  previous_origin TEXT;
  pause_placement BOOLEAN := false;
BEGIN
  IF pg_trigger_depth() > 1 THEN
    IF TG_OP = 'DELETE' THEN RETURN OLD; END IF;
    RETURN NEW;
  END IF;
  IF TG_OP = 'DELETE' THEN
    seed_ids := ARRAY[OLD.id, OLD.parent_id];
  ELSE
    seed_ids := ARRAY[NEW.id];
    IF TG_OP = 'UPDATE' THEN
      IF NEW.project_id IS DISTINCT FROM OLD.project_id OR NEW.parent_id IS DISTINCT FROM OLD.parent_id
         OR NEW.deleted_at IS DISTINCT FROM OLD.deleted_at THEN
        WITH RECURSIVE subtree(id) AS (
          SELECT NEW.id
          UNION
          SELECT t.id FROM tasks t JOIN subtree s ON t.parent_id = s.id WHERE t.deleted_at IS NULL
        ) SELECT coalesce(array_agg(id), ARRAY[]::UUID[]) INTO seed_ids FROM subtree;
      END IF;
    END IF;
    seed_ids := seed_ids || ARRAY[NEW.parent_id];
    IF TG_OP = 'UPDATE' THEN
      cascade_seeds := coalesce(nullif(current_setting('hmem.cascade_task_seeds', true), '')::UUID[], ARRAY[]::UUID[]);
      PERFORM set_config('hmem.cascade_task_seeds', '', true);
      seed_ids := seed_ids || cascade_seeds || ARRAY[OLD.id, OLD.parent_id];
      pause_placement := (NEW.project_id IS DISTINCT FROM OLD.project_id
        OR NEW.parent_id IS DISTINCT FROM OLD.parent_id OR NEW.deleted_at IS DISTINCT FROM OLD.deleted_at)
        AND (hmem_projects_have_paused_ancestry(hmem_task_implicated_project_ids(OLD.project_id, OLD.parent_id))
          OR EXISTS (SELECT 1 FROM unnest(seed_ids) s(id) WHERE hmem_task_is_inside_paused_project(s.id)));
    END IF;
  END IF;
  previous_origin := current_setting('hmem.auto_block_origin', true);
  IF pause_placement THEN PERFORM set_config('hmem.auto_block_origin', 'project_pause', true); END IF;
  PERFORM hmem_recompute_task_auto_blocking(seed_ids);
  PERFORM set_config('hmem.auto_block_origin', coalesce(previous_origin, ''), true);
  IF TG_OP = 'DELETE' THEN RETURN OLD; END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- The rule installation and ledger registration are one transaction.
INSERT INTO schema_migrations (version, name) VALUES (35, 'V035__project_pause_lifecycle.sql');
COMMIT;
