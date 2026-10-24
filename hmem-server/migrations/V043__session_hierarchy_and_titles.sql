BEGIN;
SELECT set_config('hmem.change_cause', 'migration', true);

CREATE FUNCTION hmem_session_title_valid(value TEXT) RETURNS BOOLEAN LANGUAGE SQL IMMUTABLE AS $$
  SELECT value IS NOT NULL AND octet_length(value) BETWEEN 1 AND 80 AND value<>''
    AND value=btrim(value,E' \t\n\r\f' || chr(11) || chr(160) || chr(5760) || chr(8192) || chr(8193) || chr(8194) || chr(8195) || chr(8196) || chr(8197) || chr(8198) || chr(8199) || chr(8200) || chr(8201) || chr(8202) || chr(8232) || chr(8233) || chr(8239) || chr(8287) || chr(12288));
$$;

-- Null is retained only for pre-upgrade sessions, including sealed evidence.
ALTER TABLE sessions ADD COLUMN title TEXT;
ALTER TABLE sessions ADD COLUMN parent_session_id UUID;
ALTER TABLE sessions ADD CONSTRAINT session_title_length CHECK
  (title IS NULL OR hmem_session_title_valid(title));
ALTER TABLE sessions ADD CONSTRAINT session_parent_workspace FOREIGN KEY(parent_session_id,workspace_id) REFERENCES sessions(id,workspace_id);
ALTER TABLE sessions ADD CONSTRAINT session_parent_not_self CHECK(parent_session_id IS DISTINCT FROM id);
CREATE INDEX idx_sessions_parent ON sessions(workspace_id,parent_session_id,created_at DESC,id DESC);

CREATE FUNCTION hmem_session_tree_guard() RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP='UPDATE' THEN
    IF (NEW.title,NEW.parent_session_id) IS DISTINCT FROM (OLD.title,OLD.parent_session_id) THEN
      RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Session title and parent are fixed at creation.';
    END IF;
    RETURN NEW;
  END IF;
  IF NOT hmem_session_title_valid(NEW.title) THEN
    RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='A very short descriptive session title is required (1 to 80 UTF-8 bytes).';
  END IF;
  IF NEW.parent_session_id IS NOT NULL THEN
    -- Requiring an already existing parent also rejects cycles in multirow
    -- INSERTs: their first forward reference cannot establish a parent.
    IF NEW.parent_session_id=NEW.id THEN
      RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='A session cannot be its own parent.';
    END IF;
    PERFORM 1 FROM sessions WHERE id=NEW.parent_session_id AND workspace_id=NEW.workspace_id FOR KEY SHARE;
    IF NOT FOUND THEN
      RAISE EXCEPTION USING ERRCODE='23503', MESSAGE='Parent session must already exist in the same workspace.';
    END IF;
  END IF;
  RETURN NEW;
END;
$$;
CREATE TRIGGER session_tree_guard BEFORE INSERT OR UPDATE ON sessions FOR EACH ROW EXECUTE FUNCTION hmem_session_tree_guard();

CREATE OR REPLACE FUNCTION hmem_task_session_guard() RETURNS TRIGGER LANGUAGE plpgsql AS $$
DECLARE task_status TEXT;
BEGIN
  IF TG_OP<>'INSERT' THEN
    RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Session task associations are additive.';
  END IF;
  -- BEFORE INSERT also runs before ON CONFLICT. Historical links are no-ops,
  -- rather than new associations, even after their task has completed.
  IF EXISTS (SELECT 1 FROM task_sessions WHERE task_id=NEW.task_id AND session_id=NEW.session_id AND workspace_id=NEW.workspace_id) THEN RETURN NULL; END IF;
  SELECT status::text INTO task_status FROM tasks WHERE id=NEW.task_id AND workspace_id=NEW.workspace_id AND deleted_at IS NULL FOR SHARE;
  IF NOT FOUND THEN
    RAISE EXCEPTION USING ERRCODE='23503', MESSAGE='Session association requires a live same-workspace task.';
  END IF;
  IF task_status<>'in_progress' THEN
    RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Session association requires task status in_progress; current status is ' || task_status || '.';
  END IF;
  RETURN NEW;
END;
$$;

CREATE FUNCTION hmem_session_project_tasks(workspace UUID, project UUID) RETURNS TABLE(id UUID) LANGUAGE SQL STABLE AS $$
  WITH RECURSIVE project_tree(id) AS (
    SELECT p.id FROM projects p WHERE p.id=project AND p.workspace_id=workspace AND p.deleted_at IS NULL
    UNION SELECT p.id FROM projects p JOIN project_tree parent ON p.parent_id=parent.id WHERE p.workspace_id=workspace AND p.deleted_at IS NULL
  ), task_tree(id) AS (
    SELECT t.id FROM tasks t JOIN project_tree p ON t.project_id=p.id WHERE t.workspace_id=workspace AND t.deleted_at IS NULL
    UNION SELECT t.id FROM tasks t JOIN task_tree parent ON t.parent_id=parent.id WHERE t.workspace_id=workspace AND t.deleted_at IS NULL
  ) SELECT id FROM task_tree;
$$;
CREATE FUNCTION hmem_task_has_active_session(target UUID) RETURNS BOOLEAN LANGUAGE SQL STABLE AS $$
  WITH RECURSIVE task_tree(id,workspace_id) AS (
    SELECT id,workspace_id FROM tasks WHERE id=target AND deleted_at IS NULL
    UNION SELECT child.id,child.workspace_id FROM tasks child JOIN task_tree parent ON child.parent_id=parent.id AND child.workspace_id=parent.workspace_id WHERE child.deleted_at IS NULL
  ) SELECT EXISTS(SELECT 1 FROM task_tree t JOIN task_sessions link ON link.task_id=t.id AND link.workspace_id=t.workspace_id JOIN sessions s ON s.id=link.session_id AND s.workspace_id=t.workspace_id WHERE s.state='active');
$$;
CREATE FUNCTION hmem_project_has_active_session(target UUID) RETURNS BOOLEAN LANGUAGE SQL STABLE AS $$
  SELECT EXISTS(SELECT 1 FROM projects p CROSS JOIN LATERAL hmem_session_project_tasks(p.workspace_id,p.id) t JOIN task_sessions link ON link.task_id=t.id AND link.workspace_id=p.workspace_id JOIN sessions s ON s.id=link.session_id AND s.workspace_id=p.workspace_id WHERE p.id=target AND p.deleted_at IS NULL AND s.state='active');
$$;

-- At most sixteen compact ancestors per result, independent of page size.
CREATE FUNCTION hmem_session_ancestor_context(target UUID) RETURNS JSONB LANGUAGE SQL STABLE AS $$
  WITH RECURSIVE chain AS (
    SELECT p.id,p.title,p.format_label,p.state,p.parent_session_id,p.workspace_id,1 AS depth FROM sessions s JOIN sessions p ON p.id=s.parent_session_id AND p.workspace_id=s.workspace_id WHERE s.id=target
    UNION ALL SELECT p.id,p.title,p.format_label,p.state,p.parent_session_id,p.workspace_id,child.depth+1 FROM chain child JOIN sessions p ON p.id=child.parent_session_id AND p.workspace_id=child.workspace_id WHERE child.depth<16
  ) SELECT jsonb_build_object('ancestors',coalesce(jsonb_agg(jsonb_build_object('id',id,'title',title,'format_label',format_label,'state',state,'parent_session_id',parent_session_id) ORDER BY depth DESC),'[]'::jsonb),'ancestors_truncated',coalesce(bool_or(depth=16 AND parent_session_id IS NOT NULL),false)) FROM chain;
$$;

CREATE OR REPLACE FUNCTION hmem_agent_session_audit_value(s sessions) RETURNS JSONB LANGUAGE SQL STABLE AS $$
  SELECT jsonb_build_object('id',s.id,'workspace_id',s.workspace_id,'format_label',s.format_label,'title',s.title,'parent_session_id',s.parent_session_id,
    'state',s.state,'created_at',s.created_at,'last_seen_at',s.last_seen_at,'committed_bytes',s.committed_bytes,'committed_records',s.committed_records,
    'revision',s.revision,'ends_with_newline',s.ends_with_newline,'terminated_at',s.terminated_at,'final_bytes',s.final_bytes,'final_sha256',s.final_sha256);
$$;
CREATE OR REPLACE FUNCTION hmem_agent_session_change() RETURNS TRIGGER LANGUAGE plpgsql AS $$
DECLARE before_value JSONB; after_value JSONB := hmem_agent_session_audit_value(NEW); targets JSONB;
BEGIN
  IF TG_OP = 'UPDATE' THEN
    before_value := hmem_agent_session_audit_value(OLD);
    IF before_value = after_value THEN RETURN NULL; END IF;
  END IF;
  INSERT INTO audit_log(entity_type, entity_id, action, old_values, new_values,
    request_id, workspace_id, actor_type, actor_id, actor_label, bot_identity)
  VALUES ('agent_session', NEW.id::text,
    CASE WHEN TG_OP = 'INSERT' THEN 'create'::audit_action_enum ELSE 'update'::audit_action_enum END,
    before_value, after_value, nullif(current_setting('hmem.request_id',true),''), NEW.workspace_id,
    nullif(current_setting('hmem.actor_type',true),'')::actor_type_enum,
    nullif(current_setting('hmem.actor_id',true),''), nullif(current_setting('hmem.actor_label',true),''),
    nullif(current_setting('hmem.bot_identity',true),'')::jsonb);
  targets := jsonb_build_array(jsonb_build_object('kind','entity','target','agent_session:' || NEW.id::text),jsonb_build_object('kind','collection','target','sessions:' || NEW.workspace_id::text));
  IF TG_OP='INSERT' OR OLD.state IS DISTINCT FROM NEW.state THEN
    targets := targets || jsonb_build_array(jsonb_build_object('kind','collection','target','active_sessions:' || NEW.workspace_id::text));
  END IF;
  PERFORM hmem_change_stream_record('workspace', NEW.workspace_id, 'agent_session', NEW.id::text,
    CASE WHEN TG_OP='INSERT' THEN 'created' ELSE 'updated' END,targets);
  RETURN NULL;
END;
$$;

CREATE OR REPLACE FUNCTION hmem_task_session_change() RETURNS TRIGGER LANGUAGE plpgsql AS $$
DECLARE identity TEXT := NEW.session_id::text || ':' || NEW.task_id::text;
BEGIN
  INSERT INTO audit_log(entity_type, entity_id, action, old_values, new_values,
    request_id, workspace_id, actor_type, actor_id, actor_label, bot_identity)
  VALUES ('task_session', identity, 'create'::audit_action_enum, NULL,
    jsonb_build_object('workspace_id',NEW.workspace_id,'session_id',NEW.session_id,
      'task_id',NEW.task_id,'created_at',NEW.created_at),
    nullif(current_setting('hmem.request_id',true),''), NEW.workspace_id,
    nullif(current_setting('hmem.actor_type',true),'')::actor_type_enum,
    nullif(current_setting('hmem.actor_id',true),''), nullif(current_setting('hmem.actor_label',true),''),
    nullif(current_setting('hmem.bot_identity',true),'')::jsonb);
  PERFORM hmem_change_stream_record('workspace', NEW.workspace_id, 'task_session', identity, 'created',
    jsonb_build_array(
      jsonb_build_object('kind','entity','target','task_session:' || identity),
      jsonb_build_object('kind','entity','target','agent_session:' || NEW.session_id::text),
      jsonb_build_object('kind','collection','target','sessions:' || NEW.workspace_id::text),
      jsonb_build_object('kind','collection','target','task_sessions:' || NEW.task_id::text),
      jsonb_build_object('kind','collection','target','active_sessions:' || NEW.workspace_id::text)));
  RETURN NULL;
END;
$$;

INSERT INTO schema_migrations(version,name) VALUES (43,'V043__session_hierarchy_and_titles.sql');
COMMIT;
