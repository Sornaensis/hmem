BEGIN;

-- Add immutable declaration snapshots without inventing metadata for history.
ALTER TABLE public.audit_log ADD COLUMN bot_identity jsonb;
ALTER TABLE public.observation_revision_events ADD COLUMN bot_identity jsonb;

-- Preserve every old nullable key. Only the newly optional declaration key is
-- omitted when absent, so pre-V039 current projections remain byte-for-byte valid.
CREATE FUNCTION public.hmem_observation_event_projection(event public.observation_revision_events)
RETURNS jsonb LANGUAGE sql STABLE AS $$
  SELECT CASE WHEN (event).bot_identity IS NULL THEN to_jsonb(event) - 'observation_id' - 'bot_identity'
              ELSE to_jsonb(event) - 'observation_id' END
$$;

CREATE OR REPLACE FUNCTION public.hmem_observation_provenance(
  seq bigint, kind text, sha text, version uuid, body text)
RETURNS jsonb LANGUAGE sql VOLATILE AS $$
  SELECT jsonb_build_object(
    'sequence', seq, 'event_kind', kind, 'reviewed_git_sha', sha,
    'content_version', version,
    'content_digest', encode(sha256(convert_to(body, 'UTF8')), 'hex'),
    'recorded_at', clock_timestamp(),
    'actor_type', nullif(current_setting('hmem.actor_type', true), ''),
    'actor_id', nullif(current_setting('hmem.actor_id', true), ''),
    'actor_label', nullif(current_setting('hmem.actor_label', true), ''))
  || CASE WHEN nullif(current_setting('hmem.bot_identity', true), '') IS NULL THEN '{}'::jsonb
     ELSE jsonb_build_object('bot_identity', current_setting('hmem.bot_identity', true)::jsonb) END
$$;

CREATE OR REPLACE FUNCTION public.hmem_guard_observation_revision_event()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE parent public.observations; previous bigint;
BEGIN
  IF TG_OP = 'DELETE' THEN
    -- FK cascades run after the parent row has ceased to exist. Direct
    -- deletion while a live parent exists is never permitted.
    IF NOT EXISTS (SELECT 1 FROM public.observations WHERE id = OLD.observation_id) THEN
      RETURN OLD;
    END IF;
  ELSIF TG_OP = 'INSERT' THEN
    SELECT * INTO parent FROM public.observations WHERE id = NEW.observation_id FOR UPDATE;
    SELECT coalesce(max(sequence), 0) INTO previous
      FROM public.observation_revision_events WHERE observation_id = NEW.observation_id;
    IF parent.id IS NOT NULL AND NEW.sequence = previous + 1
       AND NEW.sequence = parent.latest_sequence
       AND hmem_observation_event_projection(NEW) = parent.current_provenance THEN
      RETURN NEW;
    END IF;
  END IF;
  RAISE EXCEPTION USING ERRCODE = '23514',
    MESSAGE = 'Observation revision events are append-only and server maintained.';
END;
$$;

-- All three audit writers and canonical outbox retain their established semantics.
CREATE OR REPLACE FUNCTION hmem_audit_change()
RETURNS TRIGGER AS $$
DECLARE
  ignored_fields TEXT[] := ARRAY[]::TEXT[];
  key_columns TEXT[] := string_to_array(COALESCE(NULLIF(TG_ARGV[1], ''), 'id'), ',');
  key_name TEXT;
  old_row JSONB;
  new_row JSONB;
  entity_identifier TEXT;
  key_data JSONB := '{}'::jsonb;
  key_value TEXT;
  idx INTEGER;
  current_workspace TEXT;
  current_actor_type TEXT;
  row_workspace TEXT;
BEGIN
  IF TG_NARGS > 2 THEN
    FOR idx IN 2..TG_NARGS - 1 LOOP
      ignored_fields := array_append(ignored_fields, TG_ARGV[idx]);
    END LOOP;
  END IF;

  CASE TG_OP
    WHEN 'INSERT' THEN
      old_row := NULL;
      new_row := hmem_jsonb_without_keys(to_jsonb(NEW), ignored_fields);
    WHEN 'UPDATE' THEN
      old_row := hmem_jsonb_without_keys(to_jsonb(OLD), ignored_fields);
      new_row := hmem_jsonb_without_keys(to_jsonb(NEW), ignored_fields);
      IF old_row = new_row THEN
        RETURN NULL;
      END IF;
    WHEN 'DELETE' THEN
      old_row := hmem_jsonb_without_keys(to_jsonb(OLD), ignored_fields);
      new_row := NULL;
    ELSE
      RETURN NULL;
  END CASE;

  FOREACH key_name IN ARRAY key_columns LOOP
    key_name := btrim(key_name);
    IF key_name <> '' THEN
      key_value := coalesce(new_row ->> key_name, old_row ->> key_name);
      IF key_value IS NOT NULL THEN
        key_data := key_data || jsonb_build_object(key_name, key_value);
      END IF;
    END IF;
  END LOOP;

  entity_identifier := CASE
    WHEN array_length(key_columns, 1) = 1 THEN
      coalesce(new_row ->> btrim(key_columns[1]), old_row ->> btrim(key_columns[1]), coalesce(new_row, old_row)::text)
    WHEN key_data <> '{}'::jsonb THEN
      key_data::text
    ELSE
      coalesce(new_row, old_row)::text
  END;

  current_workspace := NULLIF(current_setting('hmem.workspace_id', true), '');
  current_actor_type := NULLIF(current_setting('hmem.actor_type', true), '');
  row_workspace := coalesce(new_row ->> 'workspace_id', old_row ->> 'workspace_id');

  IF current_workspace IS NULL AND TG_ARGV[0] = 'workspace' THEN
    current_workspace := coalesce(new_row ->> 'id', old_row ->> 'id');
  END IF;

  IF current_workspace IS NULL THEN
    current_workspace := row_workspace;
  END IF;

  IF current_workspace IS NULL AND TG_TABLE_NAME = 'task_dependencies' THEN
    SELECT t.workspace_id::text INTO current_workspace
      FROM tasks t
     WHERE t.id = COALESCE((new_row ->> 'task_id')::uuid, (old_row ->> 'task_id')::uuid);
  END IF;

  INSERT INTO audit_log (
    entity_type, entity_id, action, old_values, new_values, request_id,
    workspace_id, actor_type, actor_id, actor_label, bot_identity
  )
  VALUES (
    TG_ARGV[0],
    entity_identifier,
    CASE TG_OP
      WHEN 'INSERT' THEN 'create'::audit_action_enum
      WHEN 'UPDATE' THEN 'update'::audit_action_enum
      ELSE 'delete'::audit_action_enum
    END,
    old_row,
    new_row,
    NULLIF(current_setting('hmem.request_id', true), ''),
    CASE WHEN current_workspace IS NULL THEN NULL ELSE current_workspace::UUID END,
    CASE WHEN current_actor_type IS NULL THEN NULL ELSE current_actor_type::actor_type_enum END,
    NULLIF(current_setting('hmem.actor_id', true), ''),
    NULLIF(current_setting('hmem.actor_label', true), ''),
    nullif(current_setting('hmem.bot_identity', true), '')::jsonb
  );

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION hmem_insert_observation_audit(
  audit_action audit_action_enum,
  old_row JSONB,
  new_row JSONB
) RETURNS VOID AS $$
DECLARE
  current_workspace TEXT := NULLIF(current_setting('hmem.workspace_id', true), '');
  current_actor_type TEXT := NULLIF(current_setting('hmem.actor_type', true), '');
BEGIN
  IF current_workspace IS NULL THEN
    current_workspace := coalesce(new_row ->> 'workspace_id', old_row ->> 'workspace_id');
  END IF;

  INSERT INTO audit_log (
    entity_type, entity_id, action, old_values, new_values, request_id,
    workspace_id, actor_type, actor_id, actor_label, bot_identity
  ) VALUES (
    'observation',
    coalesce(new_row ->> 'id', old_row ->> 'id'),
    audit_action,
    old_row,
    new_row,
    NULLIF(current_setting('hmem.request_id', true), ''),
    CASE WHEN current_workspace IS NULL THEN NULL ELSE current_workspace::UUID END,
    CASE WHEN current_actor_type IS NULL THEN NULL ELSE current_actor_type::actor_type_enum END,
    NULLIF(current_setting('hmem.actor_id', true), ''),
    NULLIF(current_setting('hmem.actor_label', true), ''),
    nullif(current_setting('hmem.bot_identity', true), '')::jsonb
  );
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION hmem_project_pause_audit_change() RETURNS TRIGGER AS $$
DECLARE
  captures JSONB := coalesce(nullif(current_setting('hmem.project_pause_audit_capture', true), ''), '{}')::jsonb;
  target_project_id UUID := CASE WHEN TG_OP = 'DELETE' THEN OLD.id ELSE NEW.id END;
  capture_key TEXT := CASE WHEN TG_OP='INSERT' THEN 'INSERT:' || NEW.id::text ELSE TG_OP || ':' || TG_RELID::text || ':' || OLD.ctid::text END;
  retained_before BOOLEAN;
  retained_after BOOLEAN;
  old_row JSONB;
  new_row JSONB;
BEGIN
  -- A missing capture is an authority error, never inferred from row status.
  IF NOT captures ? capture_key THEN RAISE EXCEPTION 'Missing project pause audit capture: trigger=% op=% depth=% target=% captures=%', TG_NAME,TG_OP,pg_trigger_depth(),target_project_id,captures; END IF;
  retained_before := (captures -> capture_key ->> 'old')::boolean;
  retained_after := (captures -> capture_key ->> 'new')::boolean;
  IF retained_before IS NULL OR retained_after IS NULL THEN RAISE EXCEPTION 'Incomplete project pause audit capture'; END IF;
  IF captures -> capture_key ? 'resume' THEN
    captures := jsonb_set(captures,ARRAY[capture_key],jsonb_build_object(
      'project_id',target_project_id,'scope',captures -> capture_key -> 'scope',
      'resume',captures -> capture_key -> 'resume'));
  ELSE
    captures := captures-capture_key;
  END IF;
  PERFORM set_config('hmem.project_pause_audit_capture',captures::text,true);
  IF TG_OP <> 'INSERT' THEN
    old_row := hmem_jsonb_without_keys(to_jsonb(OLD), ARRAY['updated_at', 'search_vector'])
      || jsonb_build_object('retained_paused', retained_before);
  END IF;
  IF TG_OP <> 'DELETE' THEN
    new_row := hmem_jsonb_without_keys(to_jsonb(NEW), ARRAY['updated_at', 'search_vector'])
      || jsonb_build_object('retained_paused', retained_after);
  END IF;
  IF TG_OP = 'UPDATE' AND old_row = new_row THEN RETURN NULL; END IF;
  INSERT INTO audit_log (entity_type, entity_id, action, old_values, new_values, request_id,
                        workspace_id, actor_type, actor_id, actor_label, bot_identity)
  VALUES ('project', target_project_id::text,
    CASE TG_OP WHEN 'INSERT' THEN 'create'::audit_action_enum WHEN 'UPDATE' THEN 'update'::audit_action_enum ELSE 'delete'::audit_action_enum END,
    old_row, new_row, nullif(current_setting('hmem.request_id', true), ''),
    coalesce(nullif(current_setting('hmem.workspace_id', true), '')::uuid, CASE WHEN TG_OP = 'DELETE' THEN OLD.workspace_id ELSE NEW.workspace_id END),
    nullif(current_setting('hmem.actor_type', true), '')::actor_type_enum,
    nullif(current_setting('hmem.actor_id', true), ''), nullif(current_setting('hmem.actor_label', true), ''), nullif(current_setting('hmem.bot_identity', true), '')::jsonb);
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION hmem_change_stream_record(
  p_scope TEXT,
  p_workspace_id UUID,
  p_entity_type TEXT,
  p_entity_id TEXT,
  p_action TEXT,
  p_invalidations JSONB
) RETURNS VOID AS $$
DECLARE counter change_stream_scope_counters%ROWTYPE;
DECLARE assigned_cursor BIGINT;
DECLARE event UUID := gen_random_uuid();
DECLARE tx UUID := hmem_change_stream_transaction_id();
DECLARE cause TEXT := coalesce(NULLIF(current_setting('hmem.change_cause', true), ''), 'core');
DECLARE raw_actor TEXT := NULLIF(current_setting('hmem.actor_type', true), '');
DECLARE envelope JSONB;
DECLARE occurred TIMESTAMPTZ;
BEGIN
  counter := hmem_change_stream_lock(p_scope, p_workspace_id);
  UPDATE change_stream_scope_counters
     SET next_cursor = next_cursor + 1
   WHERE scope = p_scope AND workspace_id IS NOT DISTINCT FROM p_workspace_id
   RETURNING next_cursor INTO assigned_cursor;
  -- @now()@ is the transaction-start timestamp.  Capture wall time only after
  -- acquiring the scope lock so retention order cannot contradict cursors.
  occurred := clock_timestamp();
  envelope := jsonb_build_object(
    'schema_version', 1,
    'event_id', event,
    'scope', p_scope,
    'workspace_id', p_workspace_id,
    'cursor', assigned_cursor,
    'occurred_at', occurred,
    'transaction', jsonb_build_object(
      'id', tx, 'cause', cause,
      'request_id', NULLIF(current_setting('hmem.request_id', true), '')
    ),
    'actor', jsonb_build_object(
      'type', CASE raw_actor WHEN 'user' THEN 'user' WHEN 'bot' THEN 'service' ELSE 'system' END,
      'id', NULLIF(current_setting('hmem.actor_id', true), '')
    ) || CASE WHEN nullif(current_setting('hmem.bot_identity', true), '') IS NULL THEN '{}'::jsonb
         ELSE jsonb_build_object('bot_identity', current_setting('hmem.bot_identity', true)::jsonb) END,
    'entity', jsonb_build_object('type', p_entity_type, 'id', p_entity_id, 'action', p_action),
    'invalidations', coalesce(p_invalidations, '[]'::jsonb)
  );
  INSERT INTO change_stream_outbox(
    event_id, scope, workspace_id, cursor, transaction_id, transaction_cause,
    request_id, actor_type, actor_id, envelope, occurred_at
  ) VALUES (
    event, p_scope, p_workspace_id, assigned_cursor, tx, cause,
    NULLIF(current_setting('hmem.request_id', true), ''),
    CASE raw_actor WHEN 'user' THEN 'user' WHEN 'bot' THEN 'service' ELSE 'system' END,
    NULLIF(current_setting('hmem.actor_id', true), ''), envelope, occurred
  );
END;
$$ LANGUAGE plpgsql;

INSERT INTO schema_migrations(version, name) VALUES (39, 'V039__declared_bot_identity.sql');
COMMIT;
