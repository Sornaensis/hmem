BEGIN;

-- Sidecars never update sessions or remove session_chunks. Codec verification
-- is performed by the trusted worker; these guards enforce its storage contract.
CREATE TABLE session_archive_jobs (
  session_id UUID PRIMARY KEY,
  workspace_id UUID NOT NULL,
  state TEXT NOT NULL DEFAULT 'queued' CHECK(state IN ('queued','running','retry','ready','failed')),
  attempt BIGINT NOT NULL DEFAULT 0 CHECK(attempt>=0),
  generation_id UUID,
  lease_token UUID,
  lease_expires_at TIMESTAMPTZ,
  retry_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  phase TEXT NOT NULL DEFAULT 'queued' CHECK(phase IN ('queued','compressing','verifying','ready')),
  raw_progress BIGINT NOT NULL DEFAULT 0 CHECK(raw_progress>=0),
  compressed_progress BIGINT NOT NULL DEFAULT 0 CHECK(compressed_progress>=0),
  error_code TEXT CHECK(error_code ~ '^[a-z][a-z0-9_]{0,63}$'),
  FOREIGN KEY(session_id,workspace_id) REFERENCES sessions(id,workspace_id),
  UNIQUE(session_id,workspace_id),
  CHECK((state='running')=(lease_token IS NOT NULL AND lease_expires_at IS NOT NULL)),
  CHECK((state='ready')=(phase='ready'))
);
CREATE INDEX idx_session_archive_jobs_due ON session_archive_jobs(retry_at,session_id) WHERE state IN ('queued','retry','running');
CREATE INDEX idx_sessions_archive_eligible ON sessions(terminated_at,id) WHERE state='terminated';

CREATE TABLE session_archive_generations (
  id UUID PRIMARY KEY,
  session_id UUID NOT NULL,
  workspace_id UUID NOT NULL,
  attempt BIGINT NOT NULL CHECK(attempt>0),
  lease_token UUID NOT NULL,
  codec TEXT NOT NULL CHECK(codec='gzip-v1'),
  codec_implementation TEXT NOT NULL CHECK(octet_length(codec_implementation) BETWEEN 1 AND 128),
  codec_parameters JSONB NOT NULL CHECK(codec_parameters='{"level":6,"window_bits":31,"memory_level":8,"strategy":"default","mtime":0,"os":255,"checkpoint_raw_bytes":1048576}'::jsonb),
  raw_bytes BIGINT NOT NULL CHECK(raw_bytes>=0),
  raw_records BIGINT NOT NULL CHECK(raw_records>=0),
  raw_revision BIGINT NOT NULL CHECK(raw_revision>0),
  raw_sha256 TEXT NOT NULL CHECK(raw_sha256 ~ '^[0-9a-f]{64}$'),
  created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  published_at TIMESTAMPTZ,
  compressed_bytes BIGINT CHECK(compressed_bytes>=20),
  compressed_sha256 TEXT CHECK(compressed_sha256 ~ '^[0-9a-f]{64}$'),
  part_count BIGINT CHECK(part_count>0),
  checkpoint_count BIGINT CHECK(checkpoint_count>0),
  ledger_count BIGINT CHECK(ledger_count>=0),
  metadata_sha256 TEXT CHECK(metadata_sha256 ~ '^[0-9a-f]{64}$'),
  FOREIGN KEY(session_id,workspace_id) REFERENCES session_archive_jobs(session_id,workspace_id),
  UNIQUE(id,session_id,workspace_id),
  UNIQUE(session_id,attempt),
  CHECK((published_at IS NULL AND compressed_bytes IS NULL AND compressed_sha256 IS NULL AND part_count IS NULL AND checkpoint_count IS NULL AND ledger_count IS NULL AND metadata_sha256 IS NULL)
    OR (published_at IS NOT NULL AND compressed_bytes IS NOT NULL AND compressed_sha256 IS NOT NULL AND part_count IS NOT NULL AND checkpoint_count IS NOT NULL AND ledger_count IS NOT NULL AND metadata_sha256 IS NOT NULL))
);
ALTER TABLE session_archive_jobs ADD CONSTRAINT session_archive_current_generation
  FOREIGN KEY(generation_id,session_id,workspace_id) REFERENCES session_archive_generations(id,session_id,workspace_id) DEFERRABLE INITIALLY DEFERRED;
CREATE UNIQUE INDEX uq_session_archive_published ON session_archive_generations(session_id) WHERE published_at IS NOT NULL;

CREATE TABLE session_archive_parts (
  generation_id UUID NOT NULL,
  session_id UUID NOT NULL,
  workspace_id UUID NOT NULL,
  ordinal BIGINT NOT NULL CHECK(ordinal>=0),
  start_offset BIGINT NOT NULL CHECK(start_offset>=0),
  end_offset BIGINT NOT NULL CHECK(end_offset>start_offset),
  data BYTEA NOT NULL CHECK(octet_length(data) BETWEEN 1 AND 1048576 AND end_offset-start_offset=octet_length(data)),
  data_sha256 TEXT NOT NULL CHECK(data_sha256 ~ '^[0-9a-f]{64}$'),
  binding_sha256 TEXT NOT NULL,
  PRIMARY KEY(generation_id,ordinal),
  UNIQUE(generation_id,start_offset),
  FOREIGN KEY(generation_id,session_id,workspace_id) REFERENCES session_archive_generations(id,session_id,workspace_id)
);
ALTER TABLE session_archive_parts ALTER COLUMN data SET STORAGE EXTERNAL;

CREATE TABLE session_archive_checkpoints (
  generation_id UUID NOT NULL,
  session_id UUID NOT NULL,
  workspace_id UUID NOT NULL,
  ordinal BIGINT NOT NULL CHECK(ordinal>=0),
  raw_offset BIGINT NOT NULL CHECK(raw_offset>=0),
  compressed_offset BIGINT NOT NULL CHECK(compressed_offset>=10),
  unused_bits SMALLINT NOT NULL CHECK(unused_bits BETWEEN 0 AND 7),
  dictionary BYTEA NOT NULL CHECK(octet_length(dictionary)<=32768),
  dictionary_sha256 TEXT NOT NULL CHECK(dictionary_sha256 ~ '^[0-9a-f]{64}$'),
  binding_sha256 TEXT NOT NULL,
  PRIMARY KEY(generation_id,ordinal),
  UNIQUE(generation_id,raw_offset),
  FOREIGN KEY(generation_id,session_id,workspace_id) REFERENCES session_archive_generations(id,session_id,workspace_id)
);
ALTER TABLE session_archive_checkpoints ALTER COLUMN dictionary SET STORAGE EXTENDED;

CREATE TABLE session_archive_ledger (
  generation_id UUID NOT NULL,
  session_id UUID NOT NULL,
  workspace_id UUID NOT NULL,
  revision BIGINT NOT NULL CHECK(revision>0),
  start_offset BIGINT NOT NULL CHECK(start_offset>=0),
  end_offset BIGINT NOT NULL CHECK(end_offset>start_offset AND end_offset-start_offset<=1048576),
  allow_final_record BOOLEAN NOT NULL,
  record_count BIGINT NOT NULL CHECK(record_count>=0),
  ends_with_newline BOOLEAN NOT NULL,
  raw_sha256 TEXT NOT NULL CHECK(raw_sha256 ~ '^[0-9a-f]{64}$'),
  binding_sha256 TEXT NOT NULL,
  PRIMARY KEY(generation_id,revision),
  UNIQUE(generation_id,start_offset),
  FOREIGN KEY(generation_id,session_id,workspace_id) REFERENCES session_archive_generations(id,session_id,workspace_id)
);

-- A versioned hash chain commits positions and payload hashes without building
-- a transcript-sized aggregate. Empty chain heads are represented by ''.
CREATE FUNCTION hmem_archive_binding(previous TEXT, metadata JSONB) RETURNS TEXT LANGUAGE SQL IMMUTABLE STRICT AS $$
  SELECT encode(sha256(convert_to('hmem-archive-binding-v1:'||previous||':'||metadata::text,'UTF8')),'hex');
$$;

CREATE FUNCTION hmem_archive_job_guard() RETURNS TRIGGER LANGUAGE plpgsql AS $$
DECLARE s sessions%ROWTYPE; g session_archive_generations%ROWTYPE;
BEGIN
  IF TG_OP='DELETE' THEN RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Archive jobs cannot be deleted.'; END IF;
  SELECT * INTO s FROM sessions WHERE id=NEW.session_id AND workspace_id=NEW.workspace_id;
  IF s.id IS NULL OR s.state<>'terminated' THEN RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Archive jobs require a terminated same-workspace session.'; END IF;
  IF TG_OP='INSERT' THEN
    IF NEW.state<>'queued' OR NEW.attempt<>0 OR NEW.generation_id IS NOT NULL OR NEW.lease_token IS NOT NULL
       OR NEW.raw_progress<>0 OR NEW.compressed_progress<>0 THEN RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Archive jobs start queued.'; END IF;
  ELSE
    IF (NEW.session_id,NEW.workspace_id) IS DISTINCT FROM (OLD.session_id,OLD.workspace_id) OR OLD.state='ready' THEN
      RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Archive identity and ready jobs are immutable.';
    END IF;
    IF NEW.attempt<OLD.attempt OR NEW.attempt>OLD.attempt+1 OR
      ((NEW.generation_id,NEW.lease_token) IS DISTINCT FROM (OLD.generation_id,OLD.lease_token)
       AND NOT(NEW.state='running' AND NEW.attempt=OLD.attempt+1
        OR NEW.state IN ('retry','failed') AND NEW.generation_id IS NULL AND NEW.lease_token IS NULL
        OR NEW.state='ready' AND NEW.generation_id=OLD.generation_id AND NEW.lease_token IS NULL)) THEN
      RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Archive attempts require a fresh generation fence.';
    END IF;
    IF NEW.state='running' AND NEW.attempt=OLD.attempt+1 AND
       (OLD.state NOT IN ('queued','retry','running') OR OLD.state='running' AND OLD.lease_expires_at>clock_timestamp() OR OLD.retry_at>clock_timestamp()) THEN
      RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Archive attempt is not due.';
    END IF;
    IF NEW.state='running' AND NEW.attempt=OLD.attempt AND
      (OLD.state<>'running' OR OLD.lease_expires_at<=clock_timestamp()
       OR (NEW.generation_id,NEW.lease_token) IS DISTINCT FROM (OLD.generation_id,OLD.lease_token)) THEN
      RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Expired archive leases cannot be renewed.';
    END IF;
    IF NEW.state='running' AND NEW.attempt=OLD.attempt+1 AND
      (NEW.generation_id IS NOT DISTINCT FROM OLD.generation_id OR NEW.lease_token IS NOT DISTINCT FROM OLD.lease_token
       OR NEW.raw_progress<>0 OR NEW.compressed_progress<>0 OR NEW.phase<>'compressing') THEN
      RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='A new attempt requires fresh generation and lease tokens.';
    END IF;
    IF NEW.state='running' AND (NEW.generation_id IS NULL OR NEW.lease_expires_at<=clock_timestamp() OR NEW.lease_expires_at>clock_timestamp()+interval '60 seconds') THEN
      RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Archive lease must be live and at most sixty seconds.';
    END IF;
    IF NEW.state='ready' THEN
      SELECT * INTO g FROM session_archive_generations WHERE id=NEW.generation_id AND session_id=NEW.session_id AND workspace_id=NEW.workspace_id;
      IF g.published_at IS NULL THEN RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Ready requires a published generation.'; END IF;
    END IF;
  END IF;
  IF NEW.raw_progress>s.final_bytes THEN RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Archive progress exceeds sealed receipt.'; END IF;
  RETURN NEW;
END;
$$;
CREATE TRIGGER session_archive_job_guard BEFORE INSERT OR UPDATE OR DELETE ON session_archive_jobs FOR EACH ROW EXECUTE FUNCTION hmem_archive_job_guard();

-- Every path locks job before generation. Expiry is tested after those locks.
CREATE FUNCTION hmem_archive_generation_guard() RETURNS TRIGGER LANGUAGE plpgsql AS $$
DECLARE j session_archive_jobs%ROWTYPE; s sessions%ROWTYPE;
  p session_archive_parts%ROWTYPE; c session_archive_checkpoints%ROWTYPE; l session_archive_ledger%ROWTYPE; digest TEXT;
BEGIN
  SELECT * INTO j FROM session_archive_jobs WHERE session_id=coalesce(NEW.session_id,OLD.session_id) FOR UPDATE;
  IF TG_OP='DELETE' THEN
    IF OLD.published_at IS NOT NULL OR j.generation_id=OLD.id OR EXISTS(SELECT 1 FROM session_archive_parts WHERE generation_id=OLD.id)
      OR EXISTS(SELECT 1 FROM session_archive_checkpoints WHERE generation_id=OLD.id) OR EXISTS(SELECT 1 FROM session_archive_ledger WHERE generation_id=OLD.id) THEN
      RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Only empty abandoned staging can be deleted.';
    END IF;
    RETURN OLD;
  END IF;
  SELECT * INTO s FROM sessions WHERE id=NEW.session_id AND workspace_id=NEW.workspace_id;
  IF s.state IS DISTINCT FROM 'terminated' OR (NEW.raw_bytes,NEW.raw_records,NEW.raw_revision,NEW.raw_sha256)
       IS DISTINCT FROM (s.final_bytes,s.committed_records,s.revision,s.final_sha256) OR j.workspace_id<>NEW.workspace_id THEN
    RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Archive receipt does not match sealed evidence.';
  END IF;
  IF j.state<>'running' OR j.attempt<>NEW.attempt OR j.lease_token<>NEW.lease_token OR j.lease_expires_at<=clock_timestamp() THEN
    RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Archive generation lost its lease.';
  END IF;
  IF TG_OP='INSERT' THEN
    IF NEW.published_at IS NOT NULL OR j.generation_id IS DISTINCT FROM NEW.id THEN RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Archive generations start unpublished with the current job fence.'; END IF;
    RETURN NEW;
  END IF;
  IF OLD.published_at IS NOT NULL OR (to_jsonb(NEW)-ARRAY['published_at','compressed_bytes','compressed_sha256','part_count','checkpoint_count','ledger_count','metadata_sha256'])
    IS DISTINCT FROM (to_jsonb(OLD)-ARRAY['published_at','compressed_bytes','compressed_sha256','part_count','checkpoint_count','ledger_count','metadata_sha256'])
    OR NEW.published_at IS NULL OR j.generation_id<>NEW.id THEN
    RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Generation identity is immutable; only current complete publication is allowed.';
  END IF;
  SELECT * INTO p FROM session_archive_parts WHERE generation_id=NEW.id ORDER BY ordinal DESC LIMIT 1;
  SELECT * INTO c FROM session_archive_checkpoints WHERE generation_id=NEW.id ORDER BY ordinal DESC LIMIT 1;
  SELECT * INTO l FROM session_archive_ledger WHERE generation_id=NEW.id ORDER BY revision DESC LIMIT 1;
  IF p.ordinal IS NULL OR c.ordinal IS NULL OR NEW.part_count IS DISTINCT FROM p.ordinal+1 OR NEW.compressed_bytes IS DISTINCT FROM p.end_offset
    OR NEW.checkpoint_count IS DISTINCT FROM c.ordinal+1 OR c.compressed_offset>=NEW.compressed_bytes-8 OR NEW.raw_bytes-c.raw_offset>1048576
    OR NEW.ledger_count IS DISTINCT FROM coalesce(l.revision,0) OR NEW.raw_bytes IS DISTINCT FROM coalesce(l.end_offset,0)
    OR NEW.ledger_count<>NEW.raw_revision-1 THEN
    RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Archive parts, checkpoints or original ledger are incomplete.';
  END IF;
  digest:=hmem_archive_binding('',jsonb_build_array(NEW.id,NEW.session_id,NEW.workspace_id,NEW.codec,NEW.codec_implementation,NEW.codec_parameters,
    NEW.raw_bytes,NEW.raw_records,NEW.raw_revision,NEW.raw_sha256,NEW.compressed_bytes,NEW.compressed_sha256,
    NEW.part_count,NEW.checkpoint_count,NEW.ledger_count,p.binding_sha256,c.binding_sha256,coalesce(l.binding_sha256,'')));
  IF NEW.metadata_sha256 IS DISTINCT FROM digest THEN RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Archive manifest binding mismatch.'; END IF;
  NEW.published_at:=clock_timestamp();
  RETURN NEW;
END;
$$;
CREATE TRIGGER session_archive_generation_guard BEFORE INSERT OR UPDATE OR DELETE ON session_archive_generations FOR EACH ROW EXECUTE FUNCTION hmem_archive_generation_guard();

CREATE FUNCTION hmem_archive_stage_guard() RETURNS TRIGGER LANGUAGE plpgsql AS $$
DECLARE j session_archive_jobs%ROWTYPE; g session_archive_generations%ROWTYPE;
  p session_archive_parts%ROWTYPE; c session_archive_checkpoints%ROWTYPE; l session_archive_ledger%ROWTYPE; source session_chunks%ROWTYPE;
BEGIN
  SELECT * INTO j FROM session_archive_jobs WHERE session_id=coalesce(NEW.session_id,OLD.session_id) FOR UPDATE;
  SELECT * INTO g FROM session_archive_generations WHERE id=coalesce(NEW.generation_id,OLD.generation_id) FOR UPDATE;
  IF TG_OP='DELETE' THEN
    IF g.published_at IS NOT NULL OR j.generation_id=g.id THEN RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Current or published archive data cannot be deleted.'; END IF;
    RETURN OLD;
  END IF;
  IF TG_OP<>'INSERT' OR g.id IS NULL OR g.published_at IS NOT NULL OR j.generation_id IS DISTINCT FROM g.id OR j.state<>'running'
    OR j.lease_token IS DISTINCT FROM g.lease_token OR j.lease_expires_at<=clock_timestamp()
    OR (NEW.session_id,NEW.workspace_id) IS DISTINCT FROM (g.session_id,g.workspace_id) THEN
    RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Archive staging requires the current live generation.';
  END IF;
  IF TG_TABLE_NAME='session_archive_parts' THEN
    SELECT * INTO p FROM session_archive_parts WHERE generation_id=g.id ORDER BY ordinal DESC LIMIT 1;
    IF NEW.ordinal<>coalesce(p.ordinal+1,0) OR NEW.start_offset<>coalesce(p.end_offset,0) OR NEW.data_sha256<>encode(sha256(NEW.data),'hex') THEN
      RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Archive part order or payload hash mismatch.';
    END IF;
    NEW.binding_sha256:=hmem_archive_binding(coalesce(p.binding_sha256,''),jsonb_build_array(g.id,NEW.ordinal,NEW.start_offset,NEW.end_offset,NEW.data_sha256));
  ELSIF TG_TABLE_NAME='session_archive_checkpoints' THEN
    SELECT * INTO c FROM session_archive_checkpoints WHERE generation_id=g.id ORDER BY ordinal DESC LIMIT 1;
    IF NEW.ordinal<>coalesce(c.ordinal+1,0) OR NEW.dictionary_sha256<>encode(sha256(NEW.dictionary),'hex')
      OR (c.ordinal IS NULL AND (NEW.raw_offset<>0 OR NEW.compressed_offset<>10 OR NEW.unused_bits<>0 OR octet_length(NEW.dictionary)<>0))
      OR (c.ordinal IS NOT NULL AND (NEW.raw_offset<=c.raw_offset OR NEW.raw_offset-c.raw_offset>1048576 OR NEW.compressed_offset<=c.compressed_offset
          OR NEW.raw_offset>=g.raw_bytes OR octet_length(NEW.dictionary)<>least(32768,NEW.raw_offset))) THEN
      RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Archive checkpoint order or dictionary hash mismatch.';
    END IF;
    NEW.binding_sha256:=hmem_archive_binding(coalesce(c.binding_sha256,''),jsonb_build_array(g.id,NEW.ordinal,NEW.raw_offset,NEW.compressed_offset,NEW.unused_bits,NEW.dictionary_sha256));
  ELSE
    SELECT * INTO l FROM session_archive_ledger WHERE generation_id=g.id ORDER BY revision DESC LIMIT 1;
    SELECT * INTO source FROM session_chunks WHERE session_id=g.session_id AND start_offset=NEW.start_offset;
    IF NEW.revision<>coalesce(l.revision+1,1) OR NEW.start_offset<>coalesce(l.end_offset,0) OR source.session_id IS NULL
      OR (NEW.revision,NEW.end_offset,NEW.allow_final_record,NEW.record_count,NEW.ends_with_newline,NEW.raw_sha256)
       IS DISTINCT FROM (source.revision,source.start_offset+octet_length(source.data),source.allow_final_record,source.record_count,source.ends_with_newline,encode(sha256(source.data),'hex')) THEN
      RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Archive ledger differs from original append evidence.';
    END IF;
    NEW.binding_sha256:=hmem_archive_binding(coalesce(l.binding_sha256,''),jsonb_build_array(g.id,NEW.revision,NEW.start_offset,NEW.end_offset,NEW.allow_final_record,NEW.record_count,NEW.ends_with_newline,NEW.raw_sha256));
  END IF;
  RETURN NEW;
END;
$$;
CREATE TRIGGER session_archive_stage_guard BEFORE INSERT OR UPDATE OR DELETE ON session_archive_parts FOR EACH ROW EXECUTE FUNCTION hmem_archive_stage_guard();
CREATE TRIGGER session_archive_stage_guard BEFORE INSERT OR UPDATE OR DELETE ON session_archive_checkpoints FOR EACH ROW EXECUTE FUNCTION hmem_archive_stage_guard();
CREATE TRIGGER session_archive_stage_guard BEFORE INSERT OR UPDATE OR DELETE ON session_archive_ledger FOR EACH ROW EXECUTE FUNCTION hmem_archive_stage_guard();

CREATE FUNCTION hmem_archive_published() RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  UPDATE session_archive_jobs SET state='ready',phase='ready',lease_token=NULL,lease_expires_at=NULL,
    raw_progress=NEW.raw_bytes,compressed_progress=NEW.compressed_bytes,error_code=NULL WHERE session_id=NEW.session_id;
  RETURN NULL;
END;
$$;
CREATE TRIGGER session_archive_published AFTER UPDATE ON session_archive_generations FOR EACH ROW EXECUTE FUNCTION hmem_archive_published();

DO $$ DECLARE target TEXT; BEGIN
  FOREACH target IN ARRAY ARRAY['session_archive_jobs','session_archive_generations','session_archive_parts','session_archive_checkpoints','session_archive_ledger'] LOOP
    EXECUTE format('CREATE TRIGGER session_evidence_no_truncate BEFORE TRUNCATE ON %I EXECUTE FUNCTION hmem_session_reject_truncate()',target);
  END LOOP;
END $$;

-- Ordinary readers resolve only this authoritative publication binding.
CREATE VIEW session_archives AS SELECT g.* FROM session_archive_generations g JOIN session_archive_jobs j
  ON (j.generation_id,j.session_id,j.workspace_id)=(g.id,g.session_id,g.workspace_id)
  WHERE j.state='ready' AND g.published_at IS NOT NULL;

INSERT INTO schema_migrations(version,name) VALUES(50,'V050__sealed_session_archives.sql');
COMMIT;
