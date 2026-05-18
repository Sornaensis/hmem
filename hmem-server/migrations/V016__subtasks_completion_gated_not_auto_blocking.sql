BEGIN;

------------------------------------------------------------------------
-- Subtasks are completion-gated, not automatic blockers
------------------------------------------------------------------------

-- V014 treated open descendants like dependency blockers and automatically
-- moved parent tasks to blocked.  The clarified workflow model keeps parents
-- startable/visible and reports open descendants as completion-gated context
-- from the next-task query instead.
CREATE OR REPLACE FUNCTION hmem_task_has_auto_blockers(task_id_to_check UUID)
RETURNS BOOLEAN AS $$
  SELECT hmem_task_has_open_dependencies(task_id_to_check);
$$ LANGUAGE sql STABLE;

-- Re-evaluate existing active task graphs so tasks that were auto-blocked only
-- because of open descendants are restored to todo.  Tasks with open
-- dependencies remain auto-blocked, and manually blocked tasks are preserved
-- because auto_blocked is false for manual blocks.
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
