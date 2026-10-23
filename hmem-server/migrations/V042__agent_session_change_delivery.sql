BEGIN;
SELECT set_config('hmem.change_cause', 'migration', true);

-- An explicit allowlist: never serialize evidence chunks or a session row.
CREATE FUNCTION hmem_agent_session_audit_value(s sessions) RETURNS JSONB
LANGUAGE SQL STABLE AS $$
  SELECT jsonb_build_object(
    'id', s.id, 'workspace_id', s.workspace_id, 'format_label', s.format_label,
    'state', s.state, 'created_at', s.created_at, 'last_seen_at', s.last_seen_at,
    'committed_bytes', s.committed_bytes, 'committed_records', s.committed_records,
    'revision', s.revision, 'ends_with_newline', s.ends_with_newline,
    'terminated_at', s.terminated_at, 'final_bytes', s.final_bytes, 'final_sha256', s.final_sha256);
$$;

CREATE FUNCTION hmem_agent_session_change() RETURNS TRIGGER LANGUAGE plpgsql AS $$
DECLARE before_value JSONB; after_value JSONB := hmem_agent_session_audit_value(NEW);
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
  PERFORM hmem_change_stream_record('workspace', NEW.workspace_id, 'agent_session', NEW.id::text,
    CASE WHEN TG_OP = 'INSERT' THEN 'created' ELSE 'updated' END,
    jsonb_build_array(
      jsonb_build_object('kind','entity','target','agent_session:' || NEW.id::text),
      jsonb_build_object('kind','collection','target','sessions:' || NEW.workspace_id::text)));
  RETURN NULL;
END;
$$;
CREATE TRIGGER trg_agent_session_change AFTER INSERT OR UPDATE ON sessions
  FOR EACH ROW EXECUTE FUNCTION hmem_agent_session_change();

CREATE FUNCTION hmem_task_session_change() RETURNS TRIGGER LANGUAGE plpgsql AS $$
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
      jsonb_build_object('kind','collection','target','task_sessions:' || NEW.task_id::text)));
  RETURN NULL;
END;
$$;
CREATE TRIGGER trg_task_session_change AFTER INSERT ON task_sessions
  FOR EACH ROW EXECUTE FUNCTION hmem_task_session_change();

-- Existing full_v1/workspace_shell_v1 snapshot items and bearers remain valid.
-- Session resources are refreshed separately through bounded canonical HTTP.
INSERT INTO schema_migrations(version,name) VALUES (42,'V042__agent_session_change_delivery.sql');
COMMIT;
