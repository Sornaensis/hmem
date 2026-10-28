BEGIN;
SELECT set_config('hmem.change_cause', 'migration', true);

ALTER TABLE sessions ADD COLUMN import_task_id UUID;
ALTER TABLE sessions ADD COLUMN import_bytes BIGINT;
ALTER TABLE sessions ADD COLUMN import_sha256 TEXT;
ALTER TABLE sessions ADD CONSTRAINT session_import_manifest CHECK (
  (import_task_id IS NULL AND import_bytes IS NULL AND import_sha256 IS NULL)
  OR (import_task_id IS NOT NULL AND import_bytes IS NOT NULL AND import_bytes>=0
    AND import_sha256 IS NOT NULL AND import_sha256 ~ '^[0-9a-f]{64}$'));
ALTER TABLE sessions ADD CONSTRAINT session_import_task_workspace
  FOREIGN KEY(import_task_id,workspace_id) REFERENCES tasks(id,workspace_id);

CREATE FUNCTION hmem_session_import_guard() RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP='UPDATE' AND
    (NEW.import_task_id,NEW.import_bytes,NEW.import_sha256) IS DISTINCT FROM
    (OLD.import_task_id,OLD.import_bytes,OLD.import_sha256) THEN
    RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Historical import manifest is immutable.';
  END IF;
  IF NEW.import_task_id IS NOT NULL THEN
    IF TG_OP='UPDATE' AND (NEW.runtime_identity,NEW.runtime_revision) IS DISTINCT FROM (OLD.runtime_identity,OLD.runtime_revision) THEN
      RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Historical runtime identity is fixed at import.';
    END IF;
    IF NEW.committed_bytes>NEW.import_bytes OR (NEW.state='terminated' AND
      (NEW.final_bytes IS DISTINCT FROM NEW.import_bytes OR NEW.final_sha256 IS DISTINCT FROM NEW.import_sha256)) THEN
      RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Historical import must match its complete manifest.';
    END IF;
  END IF;
  RETURN NEW;
END;
$$;
CREATE TRIGGER session_import_guard BEFORE INSERT OR UPDATE ON sessions
  FOR EACH ROW EXECUTE FUNCTION hmem_session_import_guard();

CREATE OR REPLACE FUNCTION hmem_session_initial_task_guard() RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  PERFORM t.id FROM task_sessions link
    JOIN tasks t ON t.id=link.task_id AND t.workspace_id=link.workspace_id
    JOIN workspaces w ON w.id=t.workspace_id
    WHERE link.session_id=NEW.id AND link.workspace_id=NEW.workspace_id
      AND t.deleted_at IS NULL AND w.deleted_at IS NULL
      AND ((NEW.import_task_id IS NULL AND t.status='in_progress') OR t.id=NEW.import_task_id)
    LIMIT 1 FOR SHARE OF t;
  IF NOT FOUND THEN
    RAISE EXCEPTION USING ERRCODE='23514', CONSTRAINT='session_requires_in_progress_task',
      MESSAGE='A session requires its initial same-workspace task; live capture requires in_progress.';
  END IF;
  RETURN NULL;
END;
$$;

CREATE OR REPLACE FUNCTION hmem_task_session_guard() RETURNS TRIGGER LANGUAGE plpgsql AS $$
DECLARE task_status TEXT;
BEGIN
  IF TG_OP<>'INSERT' THEN
    RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Session task associations are additive.';
  END IF;
  IF EXISTS (SELECT 1 FROM task_sessions WHERE task_id=NEW.task_id AND session_id=NEW.session_id AND workspace_id=NEW.workspace_id) THEN RETURN NULL; END IF;
  SELECT status::text INTO task_status FROM tasks WHERE id=NEW.task_id AND workspace_id=NEW.workspace_id AND deleted_at IS NULL FOR SHARE;
  IF NOT FOUND THEN
    RAISE EXCEPTION USING ERRCODE='23503', MESSAGE='Session association requires a live same-workspace task.';
  END IF;
  IF task_status<>'in_progress' AND NOT EXISTS(SELECT 1 FROM sessions WHERE id=NEW.session_id
      AND workspace_id=NEW.workspace_id AND import_task_id=NEW.task_id AND state='active'
      AND NOT EXISTS(SELECT 1 FROM task_sessions WHERE session_id=NEW.session_id)) THEN
    RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Session association requires task status in_progress; current status is ' || task_status || '.';
  END IF;
  RETURN NEW;
END;
$$;

CREATE OR REPLACE FUNCTION hmem_task_has_active_session(target UUID) RETURNS BOOLEAN LANGUAGE SQL STABLE AS $$
  WITH RECURSIVE task_tree(id,workspace_id) AS (
    SELECT id,workspace_id FROM tasks WHERE id=target AND deleted_at IS NULL
    UNION SELECT child.id,child.workspace_id FROM tasks child JOIN task_tree parent ON child.parent_id=parent.id AND child.workspace_id=parent.workspace_id WHERE child.deleted_at IS NULL
  ) SELECT EXISTS(SELECT 1 FROM task_tree t JOIN task_sessions link ON link.task_id=t.id AND link.workspace_id=t.workspace_id JOIN sessions s ON s.id=link.session_id AND s.workspace_id=t.workspace_id WHERE s.state='active' AND s.import_task_id IS NULL);
$$;
CREATE OR REPLACE FUNCTION hmem_project_has_active_session(target UUID) RETURNS BOOLEAN LANGUAGE SQL STABLE AS $$
  SELECT EXISTS(SELECT 1 FROM projects p CROSS JOIN LATERAL hmem_session_project_tasks(p.workspace_id,p.id) t JOIN task_sessions link ON link.task_id=t.id AND link.workspace_id=p.workspace_id JOIN sessions s ON s.id=link.session_id AND s.workspace_id=p.workspace_id WHERE p.id=target AND p.deleted_at IS NULL AND s.state='active' AND s.import_task_id IS NULL);
$$;

CREATE OR REPLACE FUNCTION hmem_agent_session_audit_value(s sessions) RETURNS JSONB LANGUAGE SQL STABLE AS $$
  SELECT jsonb_build_object('id',s.id,'workspace_id',s.workspace_id,'format_label',s.format_label,'title',s.title,'parent_session_id',s.parent_session_id,
    'state',s.state,'created_at',s.created_at,'last_seen_at',s.last_seen_at,'committed_bytes',s.committed_bytes,'committed_records',s.committed_records,
    'revision',s.revision,'ends_with_newline',s.ends_with_newline,'terminated_at',s.terminated_at,'final_bytes',s.final_bytes,'final_sha256',s.final_sha256,
    'runtime_identity',s.runtime_identity,'runtime_revision',s.runtime_revision,
    'import_task_id',s.import_task_id,'import_bytes',s.import_bytes,'import_sha256',s.import_sha256);
$$;

INSERT INTO schema_migrations(version,name) VALUES(49,'V049__historical_session_import.sql');
COMMIT;
