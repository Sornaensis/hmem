BEGIN;
SELECT set_config('hmem.change_cause', 'migration', true);

-- Legacy runtime is genuinely unknown; neither creator declarations nor the
-- transcript format establish which model produced the captured session.
ALTER TABLE sessions ADD COLUMN runtime_identity JSONB;
ALTER TABLE sessions ADD COLUMN runtime_revision BIGINT NOT NULL DEFAULT 0 CHECK(runtime_revision>=0);

CREATE FUNCTION hmem_session_runtime_text_valid(value TEXT, maximum_bytes INTEGER) RETURNS BOOLEAN LANGUAGE SQL IMMUTABLE AS $$
  SELECT value IS NOT NULL AND octet_length(value) BETWEEN 1 AND maximum_bytes AND value<>''
    AND value=btrim(value,E' \t\n\r\f' || chr(11) || chr(160) || chr(5760) || chr(8192) || chr(8193) || chr(8194) || chr(8195) || chr(8196) || chr(8197) || chr(8198) || chr(8199) || chr(8200) || chr(8201) || chr(8202) || chr(8232) || chr(8233) || chr(8239) || chr(8287) || chr(12288));
$$;
CREATE FUNCTION hmem_session_runtime_identity_valid(value JSONB) RETURNS BOOLEAN LANGUAGE SQL IMMUTABLE AS $$
  SELECT CASE WHEN value IS NULL THEN true WHEN jsonb_typeof(value)<>'object' THEN false ELSE coalesce(
    (value-ARRAY['model','provider','reasoning'])='{}'::jsonb
    AND jsonb_typeof(value->'model')='string' AND hmem_session_runtime_text_valid(value->>'model',256)
    AND (NOT value ? 'provider' OR (jsonb_typeof(value->'provider')='string' AND hmem_session_runtime_text_valid(value->>'provider',128)))
    AND (NOT value ? 'reasoning' OR (jsonb_typeof(value->'reasoning')='string' AND hmem_session_runtime_text_valid(value->>'reasoning',128))),false) END;
$$;
ALTER TABLE sessions ADD CONSTRAINT session_runtime_identity_valid CHECK(hmem_session_runtime_identity_valid(runtime_identity));

CREATE OR REPLACE FUNCTION hmem_session_metadata_guard() RETURNS TRIGGER LANGUAGE plpgsql AS $$
DECLARE chunk session_chunks%ROWTYPE;
BEGIN
  IF TG_OP='DELETE' THEN
    RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Agent session evidence cannot be deleted.';
  END IF;
  IF TG_OP='INSERT' THEN
    IF NEW.state<>'active' OR NEW.committed_bytes<>0 OR NEW.committed_records<>0
       OR NEW.revision<>0 OR NEW.runtime_revision<>0 OR NOT NEW.ends_with_newline THEN
      RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Agent sessions must start empty and active.';
    END IF;
    RETURN NEW;
  END IF;
  IF OLD.state='terminated' THEN
    RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Terminated session metadata is immutable.';
  END IF;
  IF (NEW.runtime_identity,NEW.runtime_revision) IS DISTINCT FROM (OLD.runtime_identity,OLD.runtime_revision) THEN
    IF NEW.runtime_identity IS NOT DISTINCT FROM OLD.runtime_identity OR NEW.runtime_revision<>OLD.runtime_revision+1
       OR (to_jsonb(NEW)-ARRAY['runtime_identity','runtime_revision']) IS DISTINCT FROM (to_jsonb(OLD)-ARRAY['runtime_identity','runtime_revision']) THEN
      RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Runtime identity changes require their own revision and cannot change other session metadata.';
    END IF;
    RETURN NEW;
  END IF;
  IF (NEW.id,NEW.workspace_id,NEW.format_label,NEW.creator_actor_type,NEW.creator_actor_id,
      NEW.creator_actor_label,NEW.creator_bot_identity,NEW.created_at)
    IS DISTINCT FROM (OLD.id,OLD.workspace_id,OLD.format_label,OLD.creator_actor_type,OLD.creator_actor_id,
      OLD.creator_actor_label,OLD.creator_bot_identity,OLD.created_at)
    OR NEW.last_seen_at<OLD.last_seen_at THEN
    RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Session identity and creator metadata are immutable.';
  END IF;
  IF NEW.state='terminated' THEN
    IF (NEW.committed_bytes,NEW.committed_records,NEW.ends_with_newline,NEW.last_seen_at)
      IS DISTINCT FROM (OLD.committed_bytes,OLD.committed_records,OLD.ends_with_newline,OLD.last_seen_at)
      OR NEW.revision<>OLD.revision+1 THEN
      RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Termination cannot change committed transcript bytes.';
    END IF;
  ELSIF (NEW.committed_bytes,NEW.committed_records,NEW.revision,NEW.ends_with_newline)
      IS DISTINCT FROM (OLD.committed_bytes,OLD.committed_records,OLD.revision,OLD.ends_with_newline) THEN
    SELECT * INTO chunk FROM session_chunks WHERE session_id=OLD.id AND start_offset=OLD.committed_bytes AND revision=OLD.revision+1;
    IF chunk.session_id IS NULL OR NEW.committed_bytes<>OLD.committed_bytes+octet_length(chunk.data)
      OR NEW.committed_records<>OLD.committed_records+chunk.record_count OR NEW.revision<>chunk.revision
      OR NEW.ends_with_newline<>chunk.ends_with_newline THEN
      RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Session counters must follow one committed chunk.';
    END IF;
  END IF;
  RETURN NEW;
END;
$$;

-- The existing change trigger emits ordinary session metadata invalidation for
-- identity changes. Its active-membership target remains creation/state only.
CREATE OR REPLACE FUNCTION hmem_agent_session_audit_value(s sessions) RETURNS JSONB LANGUAGE SQL STABLE AS $$
  SELECT jsonb_build_object('id',s.id,'workspace_id',s.workspace_id,'format_label',s.format_label,'title',s.title,'parent_session_id',s.parent_session_id,
    'state',s.state,'created_at',s.created_at,'last_seen_at',s.last_seen_at,'committed_bytes',s.committed_bytes,'committed_records',s.committed_records,
    'revision',s.revision,'ends_with_newline',s.ends_with_newline,'terminated_at',s.terminated_at,'final_bytes',s.final_bytes,'final_sha256',s.final_sha256,
    'runtime_identity',s.runtime_identity,'runtime_revision',s.runtime_revision);
$$;
INSERT INTO schema_migrations(version,name) VALUES(44,'V044__session_runtime_identity.sql');
COMMIT;
