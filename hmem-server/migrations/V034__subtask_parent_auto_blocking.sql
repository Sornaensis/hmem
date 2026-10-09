BEGIN;

-- An explicit pause owns its blocking intent even if the status was already
-- automatically blocked. V029 never rewrites an automatic blocked row to
-- blocked: it retains it or resolves it to todo, including direct backfills.
CREATE OR REPLACE FUNCTION hmem_preserve_manual_block_intent()
RETURNS TRIGGER AS $$
BEGIN
  IF pg_trigger_depth() = 1 AND NEW.status = 'blocked'::task_status_enum
     AND OLD.status = 'blocked'::task_status_enum AND OLD.auto_blocked
     AND current_setting('hmem.status_intent', true) IS DISTINCT FROM 'unchanged' THEN
    NEW.auto_blocked := false;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
CREATE TRIGGER trg_task_manual_block_intent
  BEFORE UPDATE OF status ON tasks
  FOR EACH ROW EXECUTE FUNCTION hmem_preserve_manual_block_intent();

-- Keep V029 cascade, status-intent, fixed-point and closed-ancestor safeguards.
-- Only direct, active unfinished children join prerequisites as auto blockers.
CREATE OR REPLACE FUNCTION hmem_task_has_open_subtasks(task_id_to_check UUID)
RETURNS BOOLEAN AS $$
  SELECT EXISTS (SELECT 1 FROM tasks child
    WHERE child.parent_id = task_id_to_check AND child.deleted_at IS NULL
      AND hmem_is_open_task_status(child.status));
$$ LANGUAGE sql STABLE;

CREATE OR REPLACE FUNCTION hmem_task_has_auto_blockers(task_id_to_check UUID)
RETURNS BOOLEAN AS $$
  SELECT hmem_task_has_open_dependencies(task_id_to_check)
      OR hmem_task_has_open_subtasks(task_id_to_check);
$$ LANGUAGE sql STABLE;

-- Authoritative eligibility used by child starts/placement and read summaries.
-- An automatic subtask block does not stop the work that resolves that block.
CREATE OR REPLACE FUNCTION hmem_task_can_start_subtasks(task_id_to_check UUID)
RETURNS BOOLEAN AS $$
  SELECT EXISTS (SELECT 1 FROM tasks parent
    WHERE parent.id = task_id_to_check AND parent.deleted_at IS NULL
      AND (parent.status IN ('todo'::task_status_enum, 'in_progress'::task_status_enum)
        OR (parent.status = 'blocked'::task_status_enum AND parent.auto_blocked))
      AND NOT hmem_task_has_open_dependencies(parent.id)
      AND NOT hmem_task_has_closed_ancestor(parent.id)
      AND NOT hmem_task_is_inside_closed_project(parent.id));
$$ LANGUAGE sql STABLE;

-- Check requested starts before automatic recomputation, after V029 cancellation.
-- AFTER checks see every outer statement target without rewriting pending rows.
CREATE OR REPLACE FUNCTION hmem_enforce_parent_start()
RETURNS TRIGGER AS $$
DECLARE blocker_ids UUID[];
BEGIN
  IF NEW.deleted_at IS NOT NULL OR NEW.status <> 'in_progress'::task_status_enum
     OR pg_trigger_depth() > 1
     OR (TG_OP = 'UPDATE' AND OLD.deleted_at IS NULL
         AND current_setting('hmem.status_intent', true) = 'unchanged') THEN
    RETURN NEW;
  END IF;
  PERFORM hmem_lock_project_ancestors(hmem_task_implicated_project_ids(NEW.project_id, NEW.parent_id));
  PERFORM hmem_lock_task_ancestors(NEW.id);
  SELECT coalesce(array_agg(id ORDER BY id), ARRAY[]::UUID[]) INTO blocker_ids
    FROM tasks WHERE parent_id = NEW.id AND deleted_at IS NULL
      AND hmem_is_open_task_status(status);
  IF cardinality(blocker_ids) > 0 THEN
    RAISE EXCEPTION USING ERRCODE = 'HM107',
      MESSAGE = 'Cannot start a task with incomplete subtasks.',
      DETAIL = jsonb_build_object('blocker_count', cardinality(blocker_ids), 'blocker_ids', blocker_ids[1:5])::text,
      HINT = 'Finish, cancel, delete, or detach incomplete subtasks before starting the parent task.';
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
CREATE TRIGGER trg_task_2_parent_start
  AFTER INSERT OR UPDATE OF status, deleted_at ON tasks
  FOR EACH ROW EXECUTE FUNCTION hmem_enforce_parent_start();
CREATE OR REPLACE FUNCTION hmem_enforce_flat_subtask_lifecycle()
RETURNS TRIGGER AS $$
DECLARE
  parent_parent_id UUID;
  parent_status task_status_enum;
  direct_child_id UUID;
BEGIN
  IF NEW.deleted_at IS NOT NULL THEN
    RETURN NEW;
  END IF;

  IF NEW.parent_id IS NOT NULL THEN
    SELECT parent.parent_id, parent.status
      INTO parent_parent_id, parent_status
      FROM tasks parent
     WHERE parent.id = NEW.parent_id
       AND parent.deleted_at IS NULL;

    IF parent_status IS NULL THEN
      RAISE EXCEPTION USING
        ERRCODE = 'HM104',
        MESSAGE = 'Cannot attach a task to an inactive parent task.',
        DETAIL = jsonb_build_object(
          'blocker_count', 1,
          'blocker_ids', ARRAY[NEW.parent_id]
        )::text,
        HINT = 'Attach the task to an active top-level task or clear parent_id.';
    END IF;

    IF parent_parent_id IS NOT NULL THEN
      RAISE EXCEPTION USING
        ERRCODE = 'HM104',
        MESSAGE = 'Cannot create or move a task under a subtask.',
        DETAIL = jsonb_build_object(
          'blocker_count', 1,
          'blocker_ids', ARRAY[NEW.parent_id]
        )::text,
        HINT = 'Attach subtasks only to top-level tasks.';
    END IF;

    SELECT child.id
      INTO direct_child_id
      FROM tasks child
     WHERE child.parent_id = NEW.id
       AND child.deleted_at IS NULL
     ORDER BY child.id
     LIMIT 1;

    IF direct_child_id IS NOT NULL THEN
      RAISE EXCEPTION USING
        ERRCODE = 'HM104',
        MESSAGE = 'Cannot move a task with subtasks under another task.',
        DETAIL = jsonb_build_object(
          'blocker_count', 1,
          'blocker_ids', ARRAY[direct_child_id]
        )::text,
        HINT = 'Move, delete, or detach existing subtasks before making this task a subtask.';
    END IF;

    IF NEW.status = 'in_progress'::task_status_enum
       AND NOT hmem_task_can_start_subtasks(NEW.parent_id)
       AND (
         TG_OP = 'INSERT'
         OR OLD.deleted_at IS NOT NULL
         OR NEW.parent_id IS DISTINCT FROM OLD.parent_id
         OR OLD.status IS DISTINCT FROM 'in_progress'::task_status_enum
       ) THEN
      RAISE EXCEPTION USING
        ERRCODE = 'HM105',
        MESSAGE = 'Cannot start a subtask while its parent task is not eligible for subtask work.',
        DETAIL = jsonb_build_object(
          'blocker_count', 1,
          'blocker_ids', ARRAY[NEW.parent_id]
        )::text,
        HINT = 'Resolve parent prerequisites, manual blocking, or closed ancestors before starting subtasks.';
    END IF;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trg_task_flat_subtask_lifecycle ON tasks;
CREATE TRIGGER trg_task_2_flat_subtask_lifecycle
  AFTER INSERT OR UPDATE OF status, parent_id, deleted_at ON tasks
  FOR EACH ROW EXECUTE FUNCTION hmem_enforce_flat_subtask_lifecycle();

-- Normalize existing eligible parents through V029, preserving manual blocks,
-- terminal states, and frozen closed ancestor trees.
DO $$
DECLARE seed_ids UUID[];
BEGIN
  SELECT coalesce(array_agg(id), ARRAY[]::UUID[]) INTO seed_ids FROM tasks WHERE deleted_at IS NULL;
  PERFORM hmem_recompute_task_auto_blocking(seed_ids);
END;
$$ LANGUAGE plpgsql;

COMMIT;
