BEGIN;

-- Bound cycle detection on reconvergent DAGs by visiting each reachable task
-- once, while retaining the dedicated lifecycle SQLSTATE used by callers.
CREATE OR REPLACE FUNCTION hmem_check_task_dep_cycle()
RETURNS TRIGGER AS $$
BEGIN
  IF EXISTS (
    WITH RECURSIVE chain AS (
      SELECT depends_on_id AS id
        FROM task_dependencies
       WHERE task_id = NEW.depends_on_id
      UNION
      SELECT td.depends_on_id
        FROM task_dependencies td
        JOIN chain c ON td.task_id = c.id
    )
    SELECT 1 FROM chain WHERE id = NEW.task_id
  ) THEN
    RAISE EXCEPTION 'Cycle detected in task dependencies' USING ERRCODE = 'HD301';
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trg_task_dep_no_cycle ON task_dependencies;
CREATE TRIGGER trg_task_dep_no_cycle
  BEFORE INSERT OR UPDATE OF task_id, depends_on_id ON task_dependencies
  FOR EACH ROW EXECUTE FUNCTION hmem_check_task_dep_cycle();

-- Workspace metadata does not affect dependency readiness. Keep automatic
-- blocking updates for topology changes without recomputing on V022's
-- workspace_id-only backfill.
DROP TRIGGER IF EXISTS trg_task_auto_blocking_from_dependency ON task_dependencies;
CREATE TRIGGER trg_task_auto_blocking_from_dependency
  AFTER INSERT OR DELETE OR UPDATE OF task_id, depends_on_id ON task_dependencies
  FOR EACH ROW EXECUTE FUNCTION hmem_recompute_task_auto_blocking_from_dependency();

COMMIT;
