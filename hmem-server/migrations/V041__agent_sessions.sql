BEGIN;
SELECT set_config('hmem.change_cause', 'migration', true);

CREATE TABLE sessions (
  id UUID PRIMARY KEY,
  workspace_id UUID NOT NULL REFERENCES workspaces(id),
  format_label TEXT NOT NULL CHECK (octet_length(format_label) BETWEEN 1 AND 128 AND btrim(format_label) <> ''),
  state TEXT NOT NULL DEFAULT 'active' CHECK (state IN ('active','terminated')),
  creator_actor_type TEXT CHECK (creator_actor_type IN ('user','bot')),
  creator_actor_id TEXT,
  creator_actor_label TEXT,
  creator_bot_identity JSONB,
  created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  last_seen_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  committed_bytes BIGINT NOT NULL DEFAULT 0 CHECK (committed_bytes >= 0),
  committed_records BIGINT NOT NULL DEFAULT 0 CHECK (committed_records >= 0),
  revision BIGINT NOT NULL DEFAULT 0 CHECK (revision >= 0),
  ends_with_newline BOOLEAN NOT NULL DEFAULT TRUE,
  terminated_at TIMESTAMPTZ,
  final_bytes BIGINT,
  final_sha256 TEXT,
  UNIQUE (id, workspace_id),
  CHECK ((state = 'active' AND terminated_at IS NULL AND final_bytes IS NULL AND final_sha256 IS NULL)
    OR (state = 'terminated' AND terminated_at IS NOT NULL AND final_bytes IS NOT NULL AND final_bytes = committed_bytes
        AND final_sha256 IS NOT NULL AND final_sha256 ~ '^[0-9a-f]{64}$'))
);
CREATE INDEX idx_sessions_workspace_page ON sessions(workspace_id,created_at DESC,id DESC);

CREATE TABLE session_chunks (
  session_id UUID NOT NULL REFERENCES sessions(id),
  start_offset BIGINT NOT NULL CHECK (start_offset >= 0),
  revision BIGINT NOT NULL CHECK (revision > 0),
  data BYTEA NOT NULL CHECK (octet_length(data) BETWEEN 1 AND 1048576),
  allow_final_record BOOLEAN NOT NULL,
  record_count BIGINT NOT NULL CHECK (record_count >= 0),
  ends_with_newline BOOLEAN NOT NULL,
  PRIMARY KEY (session_id,start_offset),
  UNIQUE (session_id,revision)
);

-- A composite FK prevents a later workspace move from invalidating a link.
CREATE UNIQUE INDEX uq_tasks_id_workspace_sessions ON tasks(id,workspace_id);
CREATE TABLE task_sessions (
  workspace_id UUID NOT NULL,
  task_id UUID NOT NULL,
  session_id UUID NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  PRIMARY KEY (task_id,session_id),
  FOREIGN KEY (task_id,workspace_id) REFERENCES tasks(id,workspace_id),
  FOREIGN KEY (session_id,workspace_id) REFERENCES sessions(id,workspace_id)
);
CREATE INDEX idx_task_sessions_session ON task_sessions(session_id,task_id);

CREATE FUNCTION hmem_session_metadata_guard() RETURNS TRIGGER LANGUAGE plpgsql AS $$
DECLARE chunk session_chunks%ROWTYPE;
BEGIN
  IF TG_OP = 'DELETE' THEN
    RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Agent session evidence cannot be deleted.';
  END IF;
  IF TG_OP = 'INSERT' THEN
    IF NEW.state <> 'active' OR NEW.committed_bytes <> 0 OR NEW.committed_records <> 0
       OR NEW.revision <> 0 OR NOT NEW.ends_with_newline THEN
      RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Agent sessions must start empty and active.';
    END IF;
    RETURN NEW;
  END IF;
  IF OLD.state = 'terminated' THEN
    RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Terminated session metadata is immutable.';
  END IF;
  IF (NEW.id,NEW.workspace_id,NEW.format_label,NEW.creator_actor_type,NEW.creator_actor_id,
      NEW.creator_actor_label,NEW.creator_bot_identity,NEW.created_at)
    IS DISTINCT FROM (OLD.id,OLD.workspace_id,OLD.format_label,OLD.creator_actor_type,OLD.creator_actor_id,
      OLD.creator_actor_label,OLD.creator_bot_identity,OLD.created_at)
    OR NEW.last_seen_at < OLD.last_seen_at THEN
    RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Session identity and creator metadata are immutable.';
  END IF;
  IF NEW.state = 'terminated' THEN
    IF (NEW.committed_bytes,NEW.committed_records,NEW.ends_with_newline,NEW.last_seen_at)
      IS DISTINCT FROM (OLD.committed_bytes,OLD.committed_records,OLD.ends_with_newline,OLD.last_seen_at)
      OR NEW.revision <> OLD.revision + 1 THEN
      RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Termination cannot change committed transcript bytes.';
    END IF;
  ELSIF (NEW.committed_bytes,NEW.committed_records,NEW.revision,NEW.ends_with_newline)
      IS DISTINCT FROM (OLD.committed_bytes,OLD.committed_records,OLD.revision,OLD.ends_with_newline) THEN
    SELECT * INTO chunk FROM session_chunks
      WHERE session_id=OLD.id AND start_offset=OLD.committed_bytes AND revision=OLD.revision+1;
    IF chunk.session_id IS NULL OR NEW.committed_bytes <> OLD.committed_bytes + octet_length(chunk.data)
      OR NEW.committed_records <> OLD.committed_records + chunk.record_count
      OR NEW.revision <> chunk.revision OR NEW.ends_with_newline <> chunk.ends_with_newline THEN
      RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Session counters must follow one committed chunk.';
    END IF;
  END IF;
  RETURN NEW;
END;
$$;
CREATE TRIGGER session_metadata_guard BEFORE INSERT OR UPDATE OR DELETE ON sessions
  FOR EACH ROW EXECUTE FUNCTION hmem_session_metadata_guard();

CREATE FUNCTION hmem_session_chunk_guard() RETURNS TRIGGER LANGUAGE plpgsql AS $$
DECLARE parent sessions%ROWTYPE; body TEXT; records TEXT[]; record TEXT; n INTEGER; i INTEGER;
BEGIN
  IF TG_OP <> 'INSERT' THEN
    RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Session chunks are append-only.';
  END IF;
  SELECT * INTO parent FROM sessions WHERE id=NEW.session_id FOR UPDATE;
  IF parent.id IS NULL OR parent.state <> 'active' OR NEW.start_offset <> parent.committed_bytes
    OR NEW.revision <> parent.revision+1 THEN
    RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Session chunk requires the active committed cursor.';
  END IF;
  IF octet_length(NEW.data) NOT BETWEEN 1 AND 1048576 THEN
    RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Session chunk exceeds the 1 MiB limit or is empty.';
  END IF;
  BEGIN
    body := convert_from(NEW.data,'UTF8');
    -- An earlier explicit EOF flush committed a complete record without LF.
    -- The next append must begin with its separator; whitespace is not one.
    IF NOT parent.ends_with_newline THEN
      IF left(body,2)=E'\r\n' THEN body:=substr(body,3);
      ELSIF left(body,1)=E'\n' THEN body:=substr(body,2);
      ELSE RAISE EXCEPTION 'missing separator'; END IF;
    END IF;
    NEW.ends_with_newline := right(convert_from(NEW.data,'UTF8'),1)=E'\n';
    NEW.record_count := 0;
    IF body <> '' THEN
      records := string_to_array(body,E'\n');
      n := array_length(records,1);
      IF right(body,1)=E'\n' THEN n:=n-1;
      ELSIF NOT NEW.allow_final_record THEN RAISE EXCEPTION 'incomplete tail'; END IF;
      FOR i IN 1..n LOOP
        record:=records[i];
        IF octet_length(record)>1048576 THEN RAISE EXCEPTION 'record limit'; END IF;
        -- json validates arbitrary JSON values without jsonb's numeric/NUL restrictions.
        PERFORM record::json;
        NEW.record_count:=NEW.record_count+1;
      END LOOP;
    END IF;
  EXCEPTION WHEN OTHERS THEN
    -- Never include a raw record or PostgreSQL parser detail in the error.
    RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Invalid UTF-8 JSONL, pending final record, or missing line separator.';
  END;
  RETURN NEW;
END;
$$;
CREATE TRIGGER session_chunk_guard BEFORE INSERT OR UPDATE OR DELETE ON session_chunks
  FOR EACH ROW EXECUTE FUNCTION hmem_session_chunk_guard();

CREATE FUNCTION hmem_session_chunk_committed() RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  UPDATE sessions SET committed_bytes=committed_bytes+octet_length(NEW.data),
    committed_records=committed_records+NEW.record_count,revision=NEW.revision,
    ends_with_newline=NEW.ends_with_newline,last_seen_at=greatest(last_seen_at,clock_timestamp())
    WHERE id=NEW.session_id;
  RETURN NULL;
END;
$$;
CREATE TRIGGER session_chunk_committed AFTER INSERT ON session_chunks
  FOR EACH ROW EXECUTE FUNCTION hmem_session_chunk_committed();

CREATE FUNCTION hmem_task_session_guard() RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP <> 'INSERT' THEN
    RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Session task associations are additive.';
  END IF;
  PERFORM 1 FROM tasks WHERE id=NEW.task_id AND workspace_id=NEW.workspace_id AND deleted_at IS NULL FOR SHARE;
  IF NOT FOUND THEN
    RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Session association requires an active same-workspace task.';
  END IF;
  RETURN NEW;
END;
$$;
CREATE TRIGGER task_session_guard BEFORE INSERT OR UPDATE OR DELETE ON task_sessions
  FOR EACH ROW EXECUTE FUNCTION hmem_task_session_guard();

-- TRUNCATE does not run row triggers, including when reached through CASCADE.
CREATE FUNCTION hmem_session_reject_truncate() RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Agent session evidence and task links cannot be truncated.';
END;
$$;
CREATE TRIGGER session_evidence_no_truncate BEFORE TRUNCATE ON sessions
  EXECUTE FUNCTION hmem_session_reject_truncate();
CREATE TRIGGER session_evidence_no_truncate BEFORE TRUNCATE ON session_chunks
  EXECUTE FUNCTION hmem_session_reject_truncate();
CREATE TRIGGER session_evidence_no_truncate BEFORE TRUNCATE ON task_sessions
  EXECUTE FUNCTION hmem_session_reject_truncate();

-- No transcript audit trigger: chunks never enter audit values or the outbox.
INSERT INTO schema_migrations(version,name) VALUES (41,'V041__agent_sessions.sql');
COMMIT;
