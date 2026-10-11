BEGIN;
SELECT set_config('hmem.change_cause', 'migration', true);
SELECT hmem_lock_planning_lifecycle();

-- Compatibility metadata preserves independent legacy pause without touching
-- excluded deleted/frozen project rows or their historical audit/outbox.
CREATE TABLE project_retained_pause_intents (
  project_id UUID PRIMARY KEY REFERENCES projects(id) ON DELETE CASCADE
);

CREATE FUNCTION hmem_project_pause_eligible(project_id_to_check UUID)
RETURNS BOOLEAN AS $$
  WITH RECURSIVE ancestors(id, parent_id, status, deleted_at) AS (
    SELECT id, parent_id, status, deleted_at FROM projects WHERE id = project_id_to_check
    UNION
    SELECT p.id, p.parent_id, p.status, p.deleted_at FROM projects p
      JOIN ancestors a ON p.id = a.parent_id
  ) SELECT EXISTS (SELECT 1 FROM ancestors)
      AND NOT EXISTS (SELECT 1 FROM ancestors WHERE deleted_at IS NOT NULL
        OR hmem_is_closed_project_status(status));
$$ LANGUAGE sql STABLE;

-- Private snapshots must resolve ownership before requesting provenance.
-- Missing and foreign identities return the same value, including deleted rows.
CREATE FUNCTION hmem_project_retained_pause_intent(workspace_id_to_check UUID, project_id_to_check UUID)
RETURNS BOOLEAN AS $$
  SELECT EXISTS (SELECT 1 FROM project_retained_pause_intents r
    JOIN projects p ON p.id = r.project_id
    WHERE p.id = project_id_to_check AND p.workspace_id = workspace_id_to_check);
$$ LANGUAGE sql STABLE;

INSERT INTO project_retained_pause_intents(project_id)
SELECT id FROM projects WHERE status = 'paused'::project_status_enum
  AND NOT hmem_project_pause_eligible(id);

-- Preserve the reviewed V035 reconciliation as the prospective guard/body.
-- The forward wrapper materializes only eligible retained identities first.
ALTER FUNCTION hmem_reconcile_project_pause(UUID[], BOOLEAN)
  RENAME TO hmem_reconcile_project_pause_v035;
CREATE FUNCTION hmem_reconcile_project_pause(root_ids UUID[], allow_running BOOLEAN DEFAULT false)
RETURNS VOID AS $$
DECLARE
  eligible_ids UUID[];
  locked_id UUID;
  previous_materializing TEXT;
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
    SELECT id FROM projects WHERE id = ANY(root_ids) AND hmem_project_pause_eligible(id)
    UNION
    SELECT p.id FROM projects p JOIN subtree s ON p.parent_id = s.id
     WHERE p.deleted_at IS NULL AND hmem_is_open_project_status(p.status)
  ) SELECT coalesce(array_agg(id), ARRAY[]::UUID[]) INTO eligible_ids FROM subtree;
  previous_materializing := current_setting('hmem.retained_pause_materializing', true);
  PERFORM set_config('hmem.retained_pause_materializing', 'true', true);
  WITH materialized AS (
    UPDATE projects p SET own_paused = true
      FROM project_retained_pause_intents r
     WHERE p.id = r.project_id AND p.id = ANY(eligible_ids)
     RETURNING p.id
  ) DELETE FROM project_retained_pause_intents r USING materialized m WHERE r.project_id = m.id;
  PERFORM set_config('hmem.retained_pause_materializing', coalesce(previous_materializing, ''), true);
  PERFORM hmem_reconcile_project_pause_v035(root_ids, allow_running);
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION hmem_project_pause_from_row() RETURNS TRIGGER AS $$
BEGIN
  IF NEW.deleted_at IS NOT NULL OR pg_trigger_depth() > 1
     OR current_setting('hmem.retained_pause_materializing', true) = 'true' THEN RETURN NEW; END IF;
  IF TG_OP = 'UPDATE' AND NEW.parent_id IS NOT DISTINCT FROM OLD.parent_id
     AND NEW.deleted_at IS NOT DISTINCT FROM OLD.deleted_at
     AND NEW.own_paused IS NOT DISTINCT FROM OLD.own_paused
     AND (current_setting('hmem.status_intent', true) = 'unchanged'
          OR NOT hmem_is_open_project_status(NEW.status)) THEN RETURN NEW; END IF;
  PERFORM hmem_reconcile_project_pause(ARRAY[NEW.id]);
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION hmem_project_pause_intent() RETURNS TRIGGER AS $$
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
  IF TG_OP = 'UPDATE'
     AND current_setting('hmem.status_intent', true) IS DISTINCT FROM 'unchanged' THEN
    -- Explicit status supersedes deferred legacy intent. Private audit replay
    -- may restore it only for the exact workspace/project snapshot identity.
    DELETE FROM project_retained_pause_intents WHERE project_id = NEW.id;
    IF requested_intent ->> 'id' = NEW.id::text
       AND requested_intent ->> 'workspace_id' = NEW.workspace_id::text
       AND requested_intent -> 'retained_paused' = 'true'::jsonb
       AND NEW.status = 'paused'::project_status_enum THEN
      INSERT INTO project_retained_pause_intents(project_id) VALUES (NEW.id);
    END IF;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;


-- Administrative normalization accepts only historical running work. Suspend
-- these two prospective guards in this transaction; audit/outbox stay enabled.
-- Both guards are restored before self-registration/COMMIT and on rollback.
ALTER TABLE projects DISABLE TRIGGER trg_project_0_pause_cascade;
ALTER TABLE tasks DISABLE TRIGGER trg_task_1_paused_placement;
UPDATE projects SET own_paused = true
 WHERE status = 'paused'::project_status_enum AND hmem_project_pause_eligible(id)
   AND NOT own_paused;
DO $$
DECLARE
  roots UUID[];
  previous_status_intent TEXT;
BEGIN
  SELECT coalesce(array_agg(id), ARRAY[]::UUID[]) INTO roots FROM projects
   WHERE status = 'paused'::project_status_enum AND hmem_project_pause_eligible(id);
  -- Direct administrative reconciliation has no enclosing row trigger.
  -- Preserve own intent on effective writes, then restore the prior setting.
  previous_status_intent := current_setting('hmem.status_intent', true);
  PERFORM set_config('hmem.status_intent', 'unchanged', true);
  PERFORM hmem_reconcile_project_pause(roots, true);
  PERFORM set_config('hmem.status_intent', coalesce(previous_status_intent, ''), true);
END;
$$ LANGUAGE plpgsql;
ALTER TABLE tasks ENABLE TRIGGER trg_task_1_paused_placement;
ALTER TABLE projects ENABLE TRIGGER trg_project_0_pause_cascade;

INSERT INTO schema_migrations(version, name) VALUES (36, 'V036__normalize_project_pause.sql');
COMMIT;

