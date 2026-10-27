BEGIN;
SELECT set_config('hmem.change_cause', 'migration', true);

-- Heartbeats still refresh live session views, but do not add audit entries.
CREATE OR REPLACE FUNCTION hmem_agent_session_change() RETURNS TRIGGER LANGUAGE plpgsql AS $$
DECLARE before_value JSONB; after_value JSONB := hmem_agent_session_audit_value(NEW); targets JSONB;
BEGIN
  IF TG_OP = 'UPDATE' THEN
    before_value := hmem_agent_session_audit_value(OLD);
    IF before_value = after_value THEN RETURN NULL; END IF;
  END IF;
  IF TG_OP = 'INSERT' OR (before_value - 'last_seen_at') IS DISTINCT FROM (after_value - 'last_seen_at') THEN
    INSERT INTO audit_log(entity_type, entity_id, action, old_values, new_values,
      request_id, workspace_id, actor_type, actor_id, actor_label, bot_identity)
    VALUES ('agent_session', NEW.id::text,
      CASE WHEN TG_OP = 'INSERT' THEN 'create'::audit_action_enum ELSE 'update'::audit_action_enum END,
      before_value, after_value, nullif(current_setting('hmem.request_id',true),''), NEW.workspace_id,
      nullif(current_setting('hmem.actor_type',true),'')::actor_type_enum,
      nullif(current_setting('hmem.actor_id',true),''), nullif(current_setting('hmem.actor_label',true),''),
      nullif(current_setting('hmem.bot_identity',true),'')::jsonb);
  END IF;
  targets := jsonb_build_array(jsonb_build_object('kind','entity','target','agent_session:' || NEW.id::text),jsonb_build_object('kind','collection','target','sessions:' || NEW.workspace_id::text));
  IF TG_OP='INSERT' OR OLD.state IS DISTINCT FROM NEW.state THEN
    targets := targets || jsonb_build_array(jsonb_build_object('kind','collection','target','active_sessions:' || NEW.workspace_id::text));
  END IF;
  PERFORM hmem_change_stream_record('workspace', NEW.workspace_id, 'agent_session', NEW.id::text,
    CASE WHEN TG_OP='INSERT' THEN 'created' ELSE 'updated' END,targets);
  RETURN NULL;
END;
$$;

-- Match both historical snapshot shapes and current ones without discarding
-- any other changed field. Incomplete or non-object snapshots are retained.
DELETE FROM audit_log
WHERE entity_type = 'agent_session' AND action = 'update'
  AND CASE WHEN jsonb_typeof(old_values) = 'object' AND jsonb_typeof(new_values) = 'object' THEN
    jsonb_typeof(old_values -> 'last_seen_at') = 'string'
    AND jsonb_typeof(new_values -> 'last_seen_at') = 'string'
    AND (old_values -> 'last_seen_at') IS DISTINCT FROM (new_values -> 'last_seen_at')
    AND (old_values - 'last_seen_at') = (new_values - 'last_seen_at')
  ELSE false END;

INSERT INTO schema_migrations(version,name) VALUES (48,'V048__session_timestamp_audit_noise.sql');
COMMIT;
