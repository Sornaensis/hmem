BEGIN;
SELECT set_config('hmem.change_cause', 'migration', true);

-- A prerequisite belongs to the same scheduling level as its dependent:
-- roots depend on roots, and subtasks depend on siblings of the same parent.
-- Terminal and soft-deleted rows retain their classification. No hierarchy
-- links are rewritten, and the existing explicit DAG check remains in force.
CREATE FUNCTION hmem_task_dependency_scope_diagnostics(workspace UUID, sample_limit INTEGER DEFAULT 100)
RETURNS JSONB LANGUAGE sql STABLE AS $$
  WITH violations AS MATERIALIZED (
    SELECT link.task_id, link.depends_on_id, dependent.parent_id AS task_parent_id,
      prerequisite.parent_id AS prerequisite_parent_id
    FROM task_dependencies link
    JOIN tasks dependent ON dependent.id=link.task_id
    JOIN tasks prerequisite ON prerequisite.id=link.depends_on_id
    WHERE dependent.workspace_id=workspace
      AND dependent.parent_id IS DISTINCT FROM prerequisite.parent_id
  ), sample AS (
    SELECT * FROM violations ORDER BY task_id, depends_on_id
      LIMIT greatest(0, least(sample_limit, 100))
  )
  SELECT jsonb_build_object('workspace_id', workspace,
    'total_count', (SELECT count(*) FROM violations),
    'items', coalesce((SELECT jsonb_agg(to_jsonb(sample)) FROM sample), '[]'::jsonb),
    'truncated', (SELECT count(*) FROM violations) > (SELECT count(*) FROM sample));
$$;

CREATE FUNCTION hmem_enforce_task_dependency_scope()
RETURNS TRIGGER LANGUAGE plpgsql AS $$
DECLARE
  changed_ids UUID[]; violation RECORD;
BEGIN
  IF TG_TABLE_NAME='tasks' THEN
    IF TG_OP='INSERT' THEN
      SELECT array_agg(id) INTO changed_ids FROM dependency_scope_new_tasks;
    ELSE
      SELECT array_agg(previous.id) INTO changed_ids
      FROM dependency_scope_old_tasks previous JOIN dependency_scope_new_tasks current USING(id)
      WHERE previous.parent_id IS DISTINCT FROM current.parent_id
        OR (previous.deleted_at IS NOT NULL AND current.deleted_at IS NULL);
    END IF;
    IF changed_ids IS NULL THEN RETURN NULL; END IF;
    SELECT link.task_id, link.depends_on_id, dependent.parent_id AS task_parent_id,
      prerequisite.parent_id AS prerequisite_parent_id INTO violation
    FROM task_dependencies link
    JOIN tasks dependent ON dependent.id=link.task_id
    JOIN tasks prerequisite ON prerequisite.id=link.depends_on_id
    WHERE (link.task_id=ANY(changed_ids) OR link.depends_on_id=ANY(changed_ids))
      AND dependent.parent_id IS DISTINCT FROM prerequisite.parent_id
    ORDER BY link.task_id, link.depends_on_id LIMIT 1;
  ELSE
    -- A metadata-only dependency update must not trap removal of unrelated
    -- historical violations. Check newly introduced endpoint pairs only.
    IF TG_OP='UPDATE' THEN
      SELECT link.task_id, link.depends_on_id, dependent.parent_id AS task_parent_id,
        prerequisite.parent_id AS prerequisite_parent_id INTO violation
      FROM (SELECT task_id, depends_on_id FROM dependency_scope_new_links
        EXCEPT SELECT task_id, depends_on_id FROM dependency_scope_old_links) link
      JOIN tasks dependent ON dependent.id=link.task_id
      JOIN tasks prerequisite ON prerequisite.id=link.depends_on_id
      WHERE dependent.parent_id IS DISTINCT FROM prerequisite.parent_id
      ORDER BY link.task_id, link.depends_on_id LIMIT 1;
    ELSE
      SELECT link.task_id, link.depends_on_id, dependent.parent_id AS task_parent_id,
        prerequisite.parent_id AS prerequisite_parent_id INTO violation
      FROM dependency_scope_new_links link
      JOIN tasks dependent ON dependent.id=link.task_id
      JOIN tasks prerequisite ON prerequisite.id=link.depends_on_id
      WHERE dependent.parent_id IS DISTINCT FROM prerequisite.parent_id
      ORDER BY link.task_id, link.depends_on_id LIMIT 1;
    END IF;
  END IF;
  IF violation.task_id IS NOT NULL THEN
    RAISE EXCEPTION USING ERRCODE='HM109',
      MESSAGE='Task prerequisites must be roots or siblings under the same parent.',
      DETAIL=to_jsonb(violation)::TEXT,
      HINT='Remove the conflicting prerequisite before changing task hierarchy; roots may depend only on roots and subtasks only on siblings.';
  END IF;
  RETURN NULL;
END;
$$;

-- Transition tables judge all endpoint placements after the whole statement,
-- including bulk attachment/detachment. V035 already serializes planning
-- statements before row locks, covering raw SQL and concurrent application
-- writers. Status-only and metadata-only updates add no classification.
CREATE TRIGGER trg_dependency_zz_scope_insert AFTER INSERT ON task_dependencies
  REFERENCING NEW TABLE AS dependency_scope_new_links
  FOR EACH STATEMENT EXECUTE FUNCTION hmem_enforce_task_dependency_scope();
CREATE TRIGGER trg_dependency_zz_scope_update AFTER UPDATE ON task_dependencies
  REFERENCING OLD TABLE AS dependency_scope_old_links NEW TABLE AS dependency_scope_new_links
  FOR EACH STATEMENT EXECUTE FUNCTION hmem_enforce_task_dependency_scope();
CREATE TRIGGER trg_task_zz_dependency_scope_insert AFTER INSERT ON tasks
  REFERENCING NEW TABLE AS dependency_scope_new_tasks
  FOR EACH STATEMENT EXECUTE FUNCTION hmem_enforce_task_dependency_scope();
CREATE TRIGGER trg_task_zz_dependency_scope_update AFTER UPDATE ON tasks
  REFERENCING OLD TABLE AS dependency_scope_old_tasks NEW TABLE AS dependency_scope_new_tasks
  FOR EACH STATEMENT EXECUTE FUNCTION hmem_enforce_task_dependency_scope();

-- Explicitly authorized upgrade cleanup: remove only forbidden parent-level
-- edges. Keep root/root and same-parent sibling edges, all parent_id values,
-- normal readiness reconciliation, audit records and canonical outbox events.
DELETE FROM task_dependencies link USING tasks dependent, tasks prerequisite
WHERE dependent.id=link.task_id AND prerequisite.id=link.depends_on_id
  AND dependent.parent_id IS DISTINCT FROM prerequisite.parent_id;

INSERT INTO schema_migrations(version, name) VALUES (40, 'V040__task_dependency_scopes.sql');
COMMIT;
