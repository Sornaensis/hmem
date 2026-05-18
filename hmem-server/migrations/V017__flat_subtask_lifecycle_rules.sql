BEGIN;

------------------------------------------------------------------------
-- Flat subtask lifecycle rules
------------------------------------------------------------------------

-- Enforce the clarified task model for all lifecycle-affecting task writes:
-- top-level tasks may have direct subtasks, but subtasks may not have
-- children; and a subtask may only be moved to in_progress after its parent is
-- already in_progress.  Existing legacy nested rows are not rewritten by this
-- migration, but subsequent active lifecycle writes must satisfy the flat
-- model.
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
       AND parent_status <> 'in_progress'::task_status_enum
       AND (
         TG_OP = 'INSERT'
         OR OLD.deleted_at IS NOT NULL
         OR NEW.parent_id IS DISTINCT FROM OLD.parent_id
         OR OLD.status IS DISTINCT FROM 'in_progress'::task_status_enum
       ) THEN
      RAISE EXCEPTION USING
        ERRCODE = 'HM105',
        MESSAGE = 'Cannot start a subtask while its parent task is not in progress.',
        DETAIL = jsonb_build_object(
          'blocker_count', 1,
          'blocker_ids', ARRAY[NEW.parent_id]
        )::text,
        HINT = 'Start the parent task before moving a subtask to in_progress.';
    END IF;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trg_task_flat_subtask_lifecycle ON tasks;
CREATE TRIGGER trg_task_flat_subtask_lifecycle
  AFTER INSERT OR UPDATE OF status, parent_id, deleted_at ON tasks
  FOR EACH ROW EXECUTE FUNCTION hmem_enforce_flat_subtask_lifecycle();

COMMIT;
