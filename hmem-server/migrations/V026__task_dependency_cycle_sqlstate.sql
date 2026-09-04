-- Give the task-dependency graph trigger a stable, dedicated SQLSTATE.  Other
-- PL/pgSQL validation failures may legitimately use PostgreSQL's generic
-- P0001, and must not be exposed as dependency-cycle errors.
CREATE OR REPLACE FUNCTION hmem_check_task_dep_cycle()
RETURNS TRIGGER AS $$
BEGIN
  IF EXISTS (
    WITH RECURSIVE chain AS (
      SELECT depends_on_id AS id
        FROM task_dependencies
       WHERE task_id = NEW.depends_on_id
      UNION ALL
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
