BEGIN;

-- Empty until an operator installs a validated policy after compatible readers
-- and maintenance are deployed. Ordinary job insertion cannot set a snapshot.
CREATE TABLE session_archive_replacement_policies (
  format_label TEXT COLLATE "C" PRIMARY KEY CHECK(octet_length(format_label) BETWEEN 1 AND 128),
  eligible_from TIMESTAMPTZ NOT NULL,
  snapshot BYTEA NOT NULL CHECK(octet_length(snapshot) BETWEEN 1 AND 65536)
);
REVOKE ALL ON session_archive_replacement_policies FROM PUBLIC;

ALTER TABLE session_archive_jobs
  ADD COLUMN replacement_snapshot BYTEA CHECK(octet_length(replacement_snapshot) BETWEEN 1 AND 65536),
  ADD COLUMN replacement_eligible_from TIMESTAMPTZ,
  ADD COLUMN replacement_policy_sha256 TEXT CHECK(replacement_policy_sha256 ~ '^[0-9a-f]{64}$'),
  ADD CHECK((replacement_snapshot IS NULL)=(replacement_eligible_from IS NULL)),
  ADD CHECK((replacement_snapshot IS NULL)=(replacement_policy_sha256 IS NULL));

CREATE FUNCTION hmem_archive_freeze_replacement() RETURNS TRIGGER
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog AS $$
BEGIN
  IF TG_OP='INSERT' THEN
    SELECT p.snapshot,p.eligible_from INTO NEW.replacement_snapshot,NEW.replacement_eligible_from
    FROM public.sessions s JOIN public.session_archive_replacement_policies p
      ON p.format_label=s.format_label COLLATE "C" AND s.terminated_at>=p.eligible_from
    WHERE s.id=NEW.session_id AND s.workspace_id=NEW.workspace_id AND s.state='terminated';
    NEW.replacement_policy_sha256:=encode(sha256(NEW.replacement_snapshot),'hex');
  ELSIF (NEW.replacement_snapshot,NEW.replacement_eligible_from,NEW.replacement_policy_sha256)
     IS DISTINCT FROM (OLD.replacement_snapshot,OLD.replacement_eligible_from,OLD.replacement_policy_sha256) THEN
    RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Archive replacement snapshot is immutable.';
  END IF;
  RETURN NEW;
END $$;
REVOKE ALL ON FUNCTION hmem_archive_freeze_replacement() FROM PUBLIC;
CREATE TRIGGER session_archive_freeze_replacement BEFORE INSERT OR UPDATE OF replacement_snapshot,replacement_eligible_from,replacement_policy_sha256
  ON session_archive_jobs FOR EACH ROW EXECUTE FUNCTION hmem_archive_freeze_replacement();

CREATE TABLE session_archive_sanitized_contents (
  generation_id UUID PRIMARY KEY,
  session_id UUID NOT NULL,
  workspace_id UUID NOT NULL,
  representation TEXT NOT NULL DEFAULT 'sanitized-v1' CHECK(representation='sanitized-v1'),
  policy_sha256 TEXT NOT NULL CHECK(policy_sha256 ~ '^[0-9a-f]{64}$'),
  content_bytes BIGINT NOT NULL CHECK(content_bytes>=0),
  content_sha256 TEXT NOT NULL CHECK(content_sha256 ~ '^[0-9a-f]{64}$'),
  replacement_count BIGINT NOT NULL CHECK(replacement_count>=0),
  FOREIGN KEY(generation_id,session_id,workspace_id) REFERENCES session_archive_generations(id,session_id,workspace_id)
);

CREATE TABLE session_archive_content_blocks (
  generation_id UUID NOT NULL REFERENCES session_archive_sanitized_contents(generation_id),
  session_id UUID NOT NULL,
  workspace_id UUID NOT NULL,
  ordinal BIGINT NOT NULL CHECK(ordinal>=0),
  start_offset BIGINT NOT NULL CHECK(start_offset>=0),
  end_offset BIGINT NOT NULL CHECK(end_offset>start_offset AND end_offset-start_offset<=1048576),
  content_sha256 TEXT NOT NULL CHECK(content_sha256 ~ '^[0-9a-f]{64}$'),
  binding_sha256 TEXT NOT NULL CHECK(binding_sha256 ~ '^[0-9a-f]{64}$'),
  PRIMARY KEY(generation_id,ordinal),
  UNIQUE(generation_id,start_offset),
  FOREIGN KEY(generation_id,session_id,workspace_id) REFERENCES session_archive_generations(id,session_id,workspace_id)
);

CREATE FUNCTION hmem_archive_sanitized_stage_guard() RETURNS TRIGGER LANGUAGE plpgsql AS $$
DECLARE j session_archive_jobs%ROWTYPE; g session_archive_generations%ROWTYPE;
  v session_archive_sanitized_contents%ROWTYPE; b session_archive_content_blocks%ROWTYPE;
BEGIN
  PERFORM 1 FROM session_archive_control WHERE singleton FOR UPDATE;
  SELECT * INTO j FROM session_archive_jobs WHERE session_id=coalesce(NEW.session_id,OLD.session_id) FOR UPDATE;
  SELECT * INTO g FROM session_archive_generations WHERE id=coalesce(NEW.generation_id,OLD.generation_id) FOR UPDATE;
  IF TG_OP='DELETE' THEN
    IF g.published_at IS NOT NULL OR j.generation_id=g.id THEN
      RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Current or published sanitized evidence cannot be deleted.';
    END IF;
    RETURN OLD;
  END IF;
  IF TG_OP<>'INSERT' OR g.id IS NULL OR g.published_at IS NOT NULL OR j.generation_id IS DISTINCT FROM g.id OR j.state<>'running'
    OR j.lease_token IS DISTINCT FROM g.lease_token OR j.lease_expires_at<=clock_timestamp()
    OR (NEW.session_id,NEW.workspace_id) IS DISTINCT FROM (g.session_id,g.workspace_id) OR j.replacement_snapshot IS NULL THEN
    RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Sanitized evidence requires the current live generation and frozen policy.';
  END IF;
  IF TG_TABLE_NAME='session_archive_sanitized_contents' THEN
    IF NEW.policy_sha256 IS DISTINCT FROM j.replacement_policy_sha256 THEN
      RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Sanitized content policy binding mismatch.';
    END IF;
  ELSE
    SELECT * INTO v FROM session_archive_sanitized_contents WHERE generation_id=g.id;
    SELECT * INTO b FROM session_archive_content_blocks WHERE generation_id=g.id ORDER BY ordinal DESC LIMIT 1;
    IF v.generation_id IS NULL OR NEW.ordinal<>coalesce(b.ordinal+1,0) OR NEW.start_offset<>coalesce(b.end_offset,0)
      OR NEW.end_offset>v.content_bytes OR (NEW.end_offset<v.content_bytes AND NEW.end_offset-NEW.start_offset<>1048576) THEN
      RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Sanitized content block boundaries mismatch.';
    END IF;
    NEW.binding_sha256:=hmem_archive_binding(coalesce(b.binding_sha256,''),jsonb_build_array(
      g.id,NEW.ordinal,NEW.start_offset,NEW.end_offset,NEW.content_sha256));
  END IF;
  RETURN NEW;
END $$;

DO $$ DECLARE target TEXT; BEGIN
  FOREACH target IN ARRAY ARRAY['session_archive_sanitized_contents','session_archive_content_blocks'] LOOP
    EXECUTE format('CREATE TRIGGER session_archive_sanitized_stage_guard BEFORE INSERT OR UPDATE OR DELETE ON %I FOR EACH ROW EXECUTE FUNCTION hmem_archive_sanitized_stage_guard()',target);
    EXECUTE format('CREATE TRIGGER session_archive_z_account AFTER INSERT OR DELETE ON %I FOR EACH ROW EXECUTE FUNCTION hmem_archive_stage_account()',target);
    EXECUTE format('CREATE TRIGGER session_evidence_no_truncate BEFORE TRUNCATE ON %I EXECUTE FUNCTION hmem_session_reject_truncate()',target);
  END LOOP;
END $$;

-- Extend the exact digest without changing its format for existing generations.
CREATE FUNCTION hmem_archive_sanitized_binding(exact TEXT, generation UUID) RETURNS TEXT LANGUAGE SQL STABLE STRICT AS $$
  SELECT hmem_archive_binding(exact,jsonb_build_array(
    v.representation,v.policy_sha256,v.content_bytes,v.content_sha256,v.replacement_count,
    coalesce(b.ordinal+1,0),coalesce(b.binding_sha256,'')))
  FROM session_archive_sanitized_contents v
  LEFT JOIN LATERAL (SELECT ordinal,binding_sha256 FROM session_archive_content_blocks
    WHERE generation_id=generation ORDER BY ordinal DESC LIMIT 1) b ON TRUE
  WHERE v.generation_id=generation;
$$;

-- No existing exact reader or maintenance admission can select sanitized bytes.
CREATE OR REPLACE VIEW session_archives AS SELECT g.* FROM session_archive_generations g JOIN session_archive_jobs j
  ON (j.generation_id,j.session_id,j.workspace_id)=(g.id,g.session_id,g.workspace_id)
  WHERE j.state='ready' AND g.published_at IS NOT NULL AND j.replacement_snapshot IS NULL;

CREATE OR REPLACE FUNCTION hmem_archive_generation_guard() RETURNS TRIGGER LANGUAGE plpgsql AS $$
DECLARE j session_archive_jobs%ROWTYPE; s sessions%ROWTYPE;
  p session_archive_parts%ROWTYPE; c session_archive_checkpoints%ROWTYPE; l session_archive_ledger%ROWTYPE; digest TEXT;
  v session_archive_sanitized_contents%ROWTYPE; b session_archive_content_blocks%ROWTYPE; content_end BIGINT;
BEGIN
  SELECT * INTO j FROM session_archive_jobs WHERE session_id=coalesce(NEW.session_id,OLD.session_id) FOR UPDATE;
  IF TG_OP='DELETE' THEN
    IF OLD.published_at IS NOT NULL OR j.generation_id=OLD.id OR EXISTS(SELECT 1 FROM session_archive_parts WHERE generation_id=OLD.id)
      OR EXISTS(SELECT 1 FROM session_archive_checkpoints WHERE generation_id=OLD.id) OR EXISTS(SELECT 1 FROM session_archive_ledger WHERE generation_id=OLD.id)
      OR EXISTS(SELECT 1 FROM session_archive_sanitized_contents WHERE generation_id=OLD.id) THEN
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
  SELECT * INTO v FROM session_archive_sanitized_contents WHERE generation_id=NEW.id;
  content_end:=coalesce(v.content_bytes,NEW.raw_bytes);
  IF (v.generation_id IS NULL)<>(j.replacement_snapshot IS NULL) THEN
    RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Archive representation differs from the frozen policy.';
  END IF;
  IF v.generation_id IS NOT NULL THEN
    SELECT * INTO b FROM session_archive_content_blocks WHERE generation_id=NEW.id ORDER BY ordinal DESC LIMIT 1;
    IF coalesce(b.end_offset,0)<>content_end THEN
      RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Sanitized content blocks are incomplete.';
    END IF;
  END IF;
  IF p.ordinal IS NULL OR c.ordinal IS NULL OR NEW.part_count IS DISTINCT FROM p.ordinal+1 OR NEW.compressed_bytes IS DISTINCT FROM p.end_offset
    OR NEW.checkpoint_count IS DISTINCT FROM c.ordinal+1 OR c.compressed_offset>=NEW.compressed_bytes-8 OR (c.raw_offset>content_end OR content_end-c.raw_offset>1048576)
    OR NEW.ledger_count IS DISTINCT FROM coalesce(l.revision,0) OR NEW.raw_bytes IS DISTINCT FROM coalesce(l.end_offset,0)
    OR NEW.ledger_count<>NEW.raw_revision-1 THEN
    RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Archive parts, checkpoints or original ledger are incomplete.';
  END IF;
  digest:=hmem_archive_binding('',jsonb_build_array(NEW.id,NEW.session_id,NEW.workspace_id,NEW.codec,NEW.codec_implementation,NEW.codec_parameters,
    NEW.raw_bytes,NEW.raw_records,NEW.raw_revision,NEW.raw_sha256,NEW.compressed_bytes,NEW.compressed_sha256,
    NEW.part_count,NEW.checkpoint_count,NEW.ledger_count,p.binding_sha256,c.binding_sha256,coalesce(l.binding_sha256,'')));
  IF v.generation_id IS NOT NULL THEN digest:=hmem_archive_sanitized_binding(digest,NEW.id); END IF;
  IF NEW.metadata_sha256 IS DISTINCT FROM digest THEN RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Archive manifest binding mismatch.'; END IF;
  NEW.published_at:=clock_timestamp();
  RETURN NEW;
END;
$$;

CREATE OR REPLACE FUNCTION hmem_archive_stage_guard() RETURNS TRIGGER LANGUAGE plpgsql AS $$
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
          OR NEW.raw_offset>=coalesce((SELECT content_bytes FROM session_archive_sanitized_contents WHERE generation_id=g.id),g.raw_bytes) OR octet_length(NEW.dictionary)<>least(32768,NEW.raw_offset))) THEN
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

CREATE OR REPLACE FUNCTION hmem_archive_stage_account() RETURNS TRIGGER LANGUAGE plpgsql AS $$
DECLARE amount BIGINT; c session_archive_control%ROWTYPE;
BEGIN
  SELECT * INTO STRICT c FROM session_archive_control WHERE singleton FOR UPDATE;
  IF TG_TABLE_NAME='session_archive_parts' THEN amount:=octet_length(coalesce(NEW.data,OLD.data))+512;
  ELSIF TG_TABLE_NAME='session_archive_checkpoints' THEN amount:=octet_length(coalesce(NEW.dictionary,OLD.dictionary))+512;
  ELSE amount:=1024; END IF;
  IF TG_OP='INSERT' THEN
    IF c.staged_bytes+amount>least(c.staging_budget,c.reserved_bytes) AND EXISTS(
      SELECT 1 FROM session_archive_jobs WHERE session_id=NEW.session_id AND replacement_snapshot IS NOT NULL) THEN
      IF c.staged_bytes+amount>c.staging_budget THEN
        RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Sanitized archive staging capacity exhausted';
      END IF;
      RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Sanitized archive reservation exhausted';
    END IF;
    IF c.staged_bytes+amount>c.staging_budget THEN RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Archive staging capacity exhausted'; END IF;
    UPDATE session_archive_control SET staged_bytes=staged_bytes+amount WHERE singleton;
  ELSE UPDATE session_archive_control SET staged_bytes=greatest(0,staged_bytes-amount) WHERE singleton;
  END IF;
  RETURN coalesce(NEW,OLD);
END $$;

-- The exact capability requires a verification row. Deny creating that
-- attestation for sanitized generations until their independent verifier exists.
CREATE FUNCTION hmem_archive_exact_reclamation_guard() RETURNS TRIGGER
LANGUAGE plpgsql SET search_path=pg_catalog AS $$
BEGIN
  IF EXISTS(SELECT 1 FROM public.session_archive_jobs WHERE session_id=NEW.session_id
    AND workspace_id=NEW.workspace_id AND replacement_snapshot IS NOT NULL) THEN
    RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Sanitized archives require dedicated reclamation verification.';
  END IF;
  RETURN NEW;
END $$;
CREATE TRIGGER session_archive_exact_reclamation_guard BEFORE INSERT OR UPDATE
  ON session_archive_reclamation FOR EACH ROW EXECUTE FUNCTION hmem_archive_exact_reclamation_guard();

INSERT INTO schema_migrations(version,name) VALUES(62,'V062__sanitized_session_archives.sql');
COMMIT;
