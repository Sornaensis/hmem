BEGIN;
SELECT set_config('hmem.change_cause','migration',true);

ALTER TABLE document_generations ADD COLUMN preview_metadata JSONB
  CHECK(preview_metadata IS NULL OR (jsonb_typeof(preview_metadata)='object' AND octet_length(preview_metadata::text)<=4096));

CREATE TABLE document_preview_runtime (
  singleton BOOLEAN PRIMARY KEY DEFAULT TRUE CHECK(singleton),
  worker_limit INTEGER NOT NULL DEFAULT 2 CHECK(worker_limit BETWEEN 1 AND 2),
  queue_limit INTEGER NOT NULL DEFAULT 32 CHECK(queue_limit BETWEEN 1 AND 32),
  available_until TIMESTAMPTZ,
  policy_version TEXT,
  image_id TEXT,
  limits_sha256 TEXT,
  CHECK(policy_version IS NULL OR octet_length(policy_version) BETWEEN 1 AND 128),
  CHECK(image_id IS NULL OR image_id ~ '^sha256:[0-9a-f]{64}$'),
  CHECK(limits_sha256 IS NULL OR limits_sha256 ~ '^[0-9a-f]{64}$')
);
INSERT INTO document_preview_runtime(singleton) VALUES(TRUE);
CREATE TABLE document_preview_jobs (
  document_id UUID PRIMARY KEY,
  workspace_id UUID NOT NULL,
  state TEXT NOT NULL CHECK(state IN ('queued','processing','ready','failed','quarantined')),
  version BIGINT NOT NULL DEFAULT 1 CHECK(version>0),
  attempts INTEGER NOT NULL DEFAULT 0 CHECK(attempts BETWEEN 0 AND 3),
  max_attempts INTEGER NOT NULL DEFAULT 3 CHECK(max_attempts BETWEEN 1 AND 3),
  run_id UUID,
  available_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  error_code TEXT CHECK(error_code IS NULL OR error_code ~ '^[a-z0-9_]{1,80}$'),
  retryable BOOLEAN NOT NULL DEFAULT FALSE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  UNIQUE(document_id,workspace_id),
  FOREIGN KEY(document_id,workspace_id) REFERENCES documents(id,workspace_id)
);
CREATE INDEX document_preview_jobs_due ON document_preview_jobs(available_at,document_id) WHERE state='queued';
CREATE TABLE document_preview_runs (
  id UUID PRIMARY KEY,
  workspace_id UUID NOT NULL,
  document_id UUID NOT NULL,
  job_version BIGINT NOT NULL CHECK(job_version>0),
  controller_id UUID NOT NULL,
  lease_token UUID NOT NULL,
  original_sha256 TEXT NOT NULL CHECK(original_sha256 ~ '^[0-9a-f]{64}$'),
  policy_version TEXT NOT NULL CHECK(octet_length(policy_version) BETWEEN 1 AND 128),
  image_id TEXT NOT NULL CHECK(image_id ~ '^sha256:[0-9a-f]{64}$'),
  limits JSONB NOT NULL CHECK(jsonb_typeof(limits)='object' AND octet_length(limits::text)<=4096),
  limits_sha256 TEXT NOT NULL CHECK(limits_sha256 ~ '^[0-9a-f]{64}$'),
  deadline_at TIMESTAMPTZ NOT NULL,
  lease_until TIMESTAMPTZ NOT NULL,
  phase TEXT NOT NULL DEFAULT 'allocated' CHECK(phase IN ('allocated','started','inspected','admitted','rendering','closed','quarantined')),
  native_pid BIGINT CHECK(native_pid>0),
  native_exited BOOLEAN NOT NULL DEFAULT FALSE,
  cleanup_verified BOOLEAN NOT NULL DEFAULT FALSE,
  creation_uncertain BOOLEAN NOT NULL DEFAULT TRUE,
  plan_sha256 TEXT CHECK(plan_sha256 IS NULL OR plan_sha256 ~ '^[0-9a-f]{64}$'),
  output_count INTEGER CHECK(output_count BETWEEN 1 AND 401),
  page_count INTEGER CHECK(page_count BETWEEN 0 AND 200),
  reserved_bytes BIGINT CHECK(reserved_bytes BETWEEN 0 AND 105906176),
  error_code TEXT CHECK(error_code IS NULL OR error_code ~ '^[a-z0-9_]{1,80}$'),
  released_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  UNIQUE(id,workspace_id,document_id),
  FOREIGN KEY(document_id,workspace_id) REFERENCES document_preview_jobs(document_id,workspace_id),
  CHECK(deadline_at>created_at AND deadline_at<=created_at+interval '120 seconds'),
  CHECK(lease_until>=deadline_at AND lease_until<=deadline_at+interval '15 seconds'),
  CHECK(NOT cleanup_verified OR native_exited),
  CHECK(released_at IS NULL OR (native_exited AND cleanup_verified AND NOT creation_uncertain))
);
ALTER TABLE document_preview_jobs ADD FOREIGN KEY(run_id,workspace_id,document_id)
  REFERENCES document_preview_runs(id,workspace_id,document_id);
CREATE INDEX document_preview_runs_unreleased ON document_preview_runs(lease_until,id) WHERE released_at IS NULL;

CREATE FUNCTION hmem_document_preview_record_guard() RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP<>'UPDATE' THEN RAISE EXCEPTION 'document_preview_ownership_is_durable' USING ERRCODE='23514'; END IF;
  IF TG_TABLE_NAME='document_preview_runs' THEN
    IF (to_jsonb(NEW)-ARRAY['phase','native_pid','native_exited','cleanup_verified','creation_uncertain','plan_sha256','output_count','page_count','reserved_bytes','error_code','released_at'])
      IS DISTINCT FROM (to_jsonb(OLD)-ARRAY['phase','native_pid','native_exited','cleanup_verified','creation_uncertain','plan_sha256','output_count','page_count','reserved_bytes','error_code','released_at']) THEN
      RAISE EXCEPTION 'document_preview_run_target_is_immutable' USING ERRCODE='23514';
    END IF;
    IF OLD.released_at IS NOT NULL AND NEW IS DISTINCT FROM OLD
      OR OLD.native_exited AND NOT NEW.native_exited OR OLD.cleanup_verified AND NOT NEW.cleanup_verified
      OR OLD.native_pid IS NOT NULL AND NEW.native_pid IS DISTINCT FROM OLD.native_pid
      OR OLD.plan_sha256 IS NOT NULL AND (NEW.plan_sha256,NEW.output_count,NEW.page_count,NEW.reserved_bytes)
         IS DISTINCT FROM (OLD.plan_sha256,OLD.output_count,OLD.page_count,OLD.reserved_bytes) THEN
      RAISE EXCEPTION 'document_preview_run_receipt_is_immutable' USING ERRCODE='23514';
    END IF;
  END IF;
  RETURN NEW;
END;
$$;
CREATE TRIGGER document_preview_runs_guard BEFORE UPDATE OR DELETE ON document_preview_runs FOR EACH ROW EXECUTE FUNCTION hmem_document_preview_record_guard();
CREATE TRIGGER document_preview_jobs_no_delete BEFORE DELETE ON document_preview_jobs FOR EACH ROW EXECUTE FUNCTION hmem_document_preview_record_guard();
CREATE TRIGGER document_preview_runtime_no_delete BEFORE DELETE ON document_preview_runtime FOR EACH ROW EXECUTE FUNCTION hmem_document_preview_record_guard();
DO $$ DECLARE target TEXT; BEGIN
  FOREACH target IN ARRAY ARRAY['document_preview_runs','document_preview_jobs','document_preview_runtime'] LOOP
    EXECUTE format('CREATE TRIGGER session_evidence_no_truncate BEFORE TRUNCATE ON %I EXECUTE FUNCTION hmem_document_preview_record_guard()',target);
  END LOOP;
END $$;

CREATE FUNCTION hmem_document_preview_runtime_lock() RETURNS document_preview_runtime LANGUAGE plpgsql AS $$
DECLARE result document_preview_runtime;
BEGIN
  INSERT INTO document_preview_runtime(singleton) VALUES(TRUE) ON CONFLICT DO NOTHING;
  SELECT * INTO STRICT result FROM document_preview_runtime WHERE singleton FOR UPDATE;
  RETURN result;
END;
$$;
CREATE FUNCTION hmem_document_preview_admitted() RETURNS BIGINT LANGUAGE sql STABLE AS $$
  SELECT count(*) FROM (
    SELECT document_id FROM document_preview_jobs WHERE state IN ('queued','processing','quarantined')
    UNION SELECT document_id FROM document_preview_runs WHERE released_at IS NULL
  ) admitted;
$$;
CREATE FUNCTION hmem_document_preview_poisoned() RETURNS BOOLEAN LANGUAGE sql STABLE AS $$
  SELECT EXISTS(SELECT 1 FROM document_preview_runs WHERE released_at IS NULL AND phase='quarantined');
$$;
CREATE FUNCTION hmem_document_preview_heartbeat(worker_count INTEGER,admission_count INTEGER,policy TEXT,image TEXT,limits_hash TEXT)
RETURNS VOID LANGUAGE plpgsql AS $$
BEGIN
  PERFORM hmem_document_preview_runtime_lock();
  UPDATE document_preview_runtime SET worker_limit=worker_count,queue_limit=admission_count,policy_version=policy,
    image_id=image,limits_sha256=limits_hash,available_until=clock_timestamp()+interval '5 seconds' WHERE singleton;
END;
$$;

CREATE FUNCTION hmem_document_preview_enqueue() RETURNS TRIGGER LANGUAGE plpgsql AS $$
DECLARE runtime document_preview_runtime; failure TEXT;
BEGIN
  IF NEW.state<>'queued' THEN RETURN NULL; END IF;
  PERFORM 1 FROM document_budgets WHERE workspace_id=NEW.workspace_id FOR UPDATE;
  runtime=hmem_document_preview_runtime_lock();
  IF hmem_document_preview_poisoned() THEN failure='cleanup_unverified';
  ELSIF runtime.available_until IS NULL OR runtime.available_until<=clock_timestamp() THEN failure='renderer_unavailable';
  ELSIF hmem_document_preview_admitted()>=runtime.queue_limit THEN failure='queue_full';
  END IF;
  INSERT INTO document_preview_jobs(document_id,workspace_id,state,error_code,retryable)
    VALUES(NEW.document_id,NEW.workspace_id,CASE WHEN failure IS NULL THEN 'queued' ELSE 'failed' END,failure,failure IS NOT NULL AND failure<>'cleanup_unverified')
    ON CONFLICT(document_id) DO NOTHING;
  IF failure IS NOT NULL THEN
    UPDATE document_previews SET state='failed',error_code=failure WHERE document_id=NEW.document_id AND state='queued' AND attempt_id IS NULL;
  END IF;
  RETURN NULL;
END;
$$;
CREATE TRIGGER document_preview_enqueue AFTER INSERT ON document_previews FOR EACH ROW EXECUTE FUNCTION hmem_document_preview_enqueue();

-- Preserve trusted fixture/manual publication without admitting a duplicate job.
CREATE FUNCTION hmem_document_preview_terminal_sync() RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.state IN ('ready','failed','unsupported') THEN
    UPDATE document_preview_jobs j SET state=CASE WHEN NEW.state='ready' THEN 'ready' ELSE 'failed' END,
      error_code=NEW.error_code,retryable=NEW.state='failed' AND coalesce(NEW.error_code,'')<>'cleanup_unverified',updated_at=clock_timestamp()
      WHERE j.document_id=NEW.document_id AND j.state='queued' AND j.run_id IS NULL;
  END IF;
  RETURN NULL;
END;
$$;
CREATE TRIGGER document_preview_terminal_sync AFTER UPDATE ON document_previews FOR EACH ROW EXECUTE FUNCTION hmem_document_preview_terminal_sync();

CREATE FUNCTION hmem_document_preview_retry(ws UUID,target UUID,identifier UUID) RETURNS BOOLEAN LANGUAGE plpgsql AS $$
DECLARE job document_preview_jobs; runtime document_preview_runtime; failure TEXT;
BEGIN
  PERFORM hmem_document_check_task(ws,target);
  IF NOT EXISTS(SELECT 1 FROM documents d WHERE d.id=identifier AND d.workspace_id=ws AND
    (EXISTS(SELECT 1 FROM task_document_sources WHERE task_id=target AND workspace_id=ws AND document_id=identifier)
     OR EXISTS(SELECT 1 FROM task_document_evidence WHERE task_id=target AND workspace_id=ws AND document_id=identifier))) THEN
    RAISE EXCEPTION 'document_not_found' USING ERRCODE='23514';
  END IF;
  PERFORM 1 FROM document_budgets WHERE workspace_id=ws FOR UPDATE;
  runtime=hmem_document_preview_runtime_lock();
  SELECT * INTO job FROM document_preview_jobs WHERE document_id=identifier AND workspace_id=ws FOR UPDATE;
  IF NOT FOUND OR job.state IN ('queued','processing','ready','quarantined') THEN RETURN FALSE; END IF;
  IF hmem_document_preview_poisoned() THEN failure='cleanup_unverified';
  ELSIF runtime.available_until IS NULL OR runtime.available_until<=clock_timestamp() THEN failure='renderer_unavailable';
  ELSIF hmem_document_preview_admitted()>=runtime.queue_limit THEN failure='queue_full';
  END IF;
  IF failure IS NOT NULL THEN
    UPDATE document_preview_jobs SET error_code=failure,retryable=failure<>'cleanup_unverified',updated_at=clock_timestamp() WHERE document_id=identifier;
    UPDATE document_previews SET state='failed',error_code=failure WHERE document_id=identifier;
    RETURN FALSE;
  END IF;
  UPDATE document_preview_jobs SET state='queued',version=version+1,attempts=0,run_id=NULL,error_code=NULL,retryable=FALSE,
    available_at=clock_timestamp(),updated_at=clock_timestamp() WHERE document_id=identifier;
  UPDATE document_previews SET state='queued',error_code=NULL,attempt_id=NULL,generation_id=NULL WHERE document_id=identifier;
  RETURN TRUE;
END;
$$;

-- A disabled controller can retire leftover queued work only after every shared
-- healthy heartbeat has expired. It cannot disable another live controller or
-- infer physical closure of a run that already owns a producer.
CREATE FUNCTION hmem_document_preview_unavailable(ws UUID,identifier UUID) RETURNS BOOLEAN LANGUAGE plpgsql AS $$
DECLARE runtime document_preview_runtime; job document_preview_jobs;
BEGIN
  PERFORM 1 FROM document_budgets WHERE workspace_id=ws FOR UPDATE;
  runtime=hmem_document_preview_runtime_lock();
  SELECT * INTO job FROM document_preview_jobs WHERE workspace_id=ws AND document_id=identifier FOR UPDATE;
  IF NOT FOUND OR job.state<>'queued' OR job.run_id IS NOT NULL
    OR (runtime.available_until IS NOT NULL AND runtime.available_until>clock_timestamp()) THEN RETURN FALSE; END IF;
  PERFORM 1 FROM document_previews WHERE document_id=identifier FOR UPDATE;
  IF NOT EXISTS(SELECT 1 FROM document_previews WHERE document_id=identifier AND state='queued' AND attempt_id IS NULL) THEN RETURN FALSE; END IF;
  UPDATE document_preview_jobs SET state='failed',error_code='renderer_unavailable',retryable=TRUE,updated_at=clock_timestamp() WHERE document_id=identifier;
  UPDATE document_previews SET state='failed',error_code='renderer_unavailable' WHERE document_id=identifier;
  RETURN TRUE;
END;
$$;

CREATE FUNCTION hmem_document_preview_claim(ws UUID,identifier UUID,owner UUID,controller UUID,token UUID,
  policy TEXT,image TEXT,limits_value JSONB,limits_hash TEXT,attempt_limit INTEGER,seconds INTEGER)
RETURNS JSONB LANGUAGE plpgsql AS $$
DECLARE job document_preview_jobs; runtime document_preview_runtime; d documents; started TIMESTAMPTZ;
BEGIN
  PERFORM 1 FROM document_budgets WHERE workspace_id=ws FOR UPDATE;
  runtime=hmem_document_preview_runtime_lock();
  SELECT * INTO job FROM document_preview_jobs WHERE workspace_id=ws AND document_id=identifier FOR UPDATE;
  IF NOT FOUND OR job.state<>'queued' OR job.available_at>clock_timestamp()
    OR runtime.available_until IS NULL OR runtime.available_until<=clock_timestamp()
    OR hmem_document_preview_poisoned()
    OR (SELECT count(*) FROM document_preview_runs WHERE released_at IS NULL)>=runtime.worker_limit
    OR runtime.policy_version IS DISTINCT FROM policy OR runtime.image_id IS DISTINCT FROM image OR runtime.limits_sha256 IS DISTINCT FROM limits_hash
    OR NOT EXISTS(SELECT 1 FROM document_previews WHERE document_id=identifier AND state='queued' AND attempt_id IS NULL) THEN RETURN NULL; END IF;
  SELECT * INTO STRICT d FROM documents WHERE id=identifier AND workspace_id=ws;
  IF seconds<6 OR seconds>(CASE WHEN d.media_type='application/pdf' THEN 120 ELSE 30 END) THEN
    RAISE EXCEPTION 'document_preview_deadline_bound' USING ERRCODE='23514';
  END IF;
  started=clock_timestamp();
  INSERT INTO document_preview_runs(id,workspace_id,document_id,job_version,controller_id,lease_token,original_sha256,
    policy_version,image_id,limits,limits_sha256,deadline_at,lease_until,created_at)
    VALUES(owner,ws,identifier,job.version,controller,token,d.raw_sha256,policy,image,limits_value,limits_hash,
      started+make_interval(secs=>seconds),started+make_interval(secs=>seconds+15),started);
  UPDATE document_preview_jobs SET state='processing',attempts=attempts+1,max_attempts=attempt_limit,run_id=owner,error_code=NULL,
    retryable=FALSE,updated_at=started WHERE document_id=identifier;
  UPDATE document_previews SET state='processing',error_code=NULL,generation_id=NULL WHERE document_id=identifier;
  RETURN (SELECT to_jsonb(r)||jsonb_build_object('raw_bytes',d.raw_bytes,'raw_sha256',d.raw_sha256,'media_type',d.media_type)
    FROM document_preview_runs r WHERE id=owner);
END;
$$;

CREATE FUNCTION hmem_document_preview_require_run(owner UUID,token UUID,require_deadline BOOLEAN) RETURNS document_preview_runs LANGUAGE plpgsql AS $$
DECLARE run document_preview_runs; job document_preview_jobs; runtime document_preview_runtime;
BEGIN
  SELECT * INTO STRICT run FROM document_preview_runs WHERE id=owner;
  PERFORM 1 FROM document_budgets WHERE workspace_id=run.workspace_id FOR UPDATE;
  runtime=hmem_document_preview_runtime_lock();
  SELECT * INTO STRICT job FROM document_preview_jobs WHERE document_id=run.document_id FOR UPDATE;
  SELECT * INTO STRICT run FROM document_preview_runs WHERE id=owner FOR UPDATE;
  IF run.lease_token<>token OR run.released_at IS NOT NULL OR job.run_id IS DISTINCT FROM owner
    OR job.version<>run.job_version OR job.state NOT IN ('processing','quarantined')
    OR require_deadline AND (run.deadline_at<=clock_timestamp() OR run.lease_until<=clock_timestamp()
      OR runtime.policy_version IS DISTINCT FROM run.policy_version OR runtime.image_id IS DISTINCT FROM run.image_id
      OR runtime.limits_sha256 IS DISTINCT FROM run.limits_sha256) THEN
    RAISE EXCEPTION 'document_preview_stale_run' USING ERRCODE='23514';
  END IF;
  PERFORM 1 FROM document_previews WHERE document_id=run.document_id FOR UPDATE;
  RETURN run;
END;
$$;

CREATE FUNCTION hmem_document_preview_started(owner UUID,token UUID,pid BIGINT) RETURNS VOID LANGUAGE plpgsql AS $$
DECLARE run document_preview_runs;
BEGIN
  run=hmem_document_preview_require_run(owner,token,TRUE);
  IF run.phase<>'allocated' THEN RAISE EXCEPTION 'document_preview_phase' USING ERRCODE='23514'; END IF;
  UPDATE document_preview_runs SET native_pid=pid,phase='started' WHERE id=owner;
END;
$$;
CREATE FUNCTION hmem_document_preview_inspected(owner UUID,token UUID) RETURNS VOID LANGUAGE plpgsql AS $$
DECLARE run document_preview_runs;
BEGIN
  run=hmem_document_preview_require_run(owner,token,TRUE);
  IF run.phase<>'started' THEN RAISE EXCEPTION 'document_preview_phase' USING ERRCODE='23514'; END IF;
  UPDATE document_preview_runs SET phase='inspected',creation_uncertain=FALSE WHERE id=owner;
END;
$$;
CREATE FUNCTION hmem_document_preview_reserve_run(owner UUID,token UUID,plan_hash TEXT,outputs INTEGER,pages INTEGER,budget BIGINT) RETURNS VOID LANGUAGE plpgsql AS $$
DECLARE run document_preview_runs;
BEGIN
  run=hmem_document_preview_require_run(owner,token,TRUE);
  IF run.phase<>'inspected' OR run.deadline_at<=clock_timestamp()+interval '5 seconds' THEN
    RAISE EXCEPTION 'document_preview_phase' USING ERRCODE='23514';
  END IF;
  PERFORM hmem_document_reserve_preview(run.workspace_id,run.document_id,run.id,run.policy_version,outputs,budget);
  UPDATE document_preview_runs SET phase='admitted',plan_sha256=plan_hash,output_count=outputs,page_count=pages,reserved_bytes=budget WHERE id=owner;
END;
$$;
CREATE FUNCTION hmem_document_preview_rendering(owner UUID,token UUID) RETURNS VOID LANGUAGE plpgsql AS $$
DECLARE run document_preview_runs;
BEGIN
  run=hmem_document_preview_require_run(owner,token,TRUE);
  IF run.phase<>'admitted' OR run.deadline_at<=clock_timestamp()+interval '5 seconds' THEN
    RAISE EXCEPTION 'document_preview_phase' USING ERRCODE='23514';
  END IF;
  UPDATE document_preview_runs SET phase='rendering',creation_uncertain=TRUE WHERE id=owner;
END;
$$;
CREATE FUNCTION hmem_document_preview_closed(owner UUID,token UUID,verified BOOLEAN,uncertain BOOLEAN) RETURNS VOID LANGUAGE plpgsql AS $$
DECLARE run document_preview_runs;
BEGIN
  run=hmem_document_preview_require_run(owner,token,FALSE);
  UPDATE document_preview_runs SET native_exited=TRUE,cleanup_verified=verified,creation_uncertain=uncertain,
    phase=CASE WHEN verified AND NOT uncertain THEN 'closed' ELSE 'quarantined' END WHERE id=owner;
  IF NOT verified OR uncertain THEN
    UPDATE document_preview_jobs SET state='quarantined',error_code='cleanup_unverified',retryable=FALSE,updated_at=clock_timestamp() WHERE document_id=run.document_id;
    UPDATE document_previews SET state='failed',error_code='cleanup_unverified' WHERE document_id=run.document_id AND state='processing'
      AND attempt_id IS NOT DISTINCT FROM CASE WHEN run.output_count IS NULL THEN NULL::uuid ELSE run.id END;
  END IF;
END;
$$;

CREATE FUNCTION hmem_document_preview_fail(owner UUID,token UUID,code TEXT,automatic_retry BOOLEAN) RETURNS BOOLEAN LANGUAGE plpgsql AS $$
DECLARE run document_preview_runs; job document_preview_jobs; current_preview BOOLEAN; retry BOOLEAN;
BEGIN
  SELECT * INTO STRICT run FROM document_preview_runs WHERE id=owner;
  PERFORM 1 FROM document_budgets WHERE workspace_id=run.workspace_id FOR UPDATE;
  PERFORM hmem_document_preview_runtime_lock();
  SELECT * INTO STRICT job FROM document_preview_jobs WHERE document_id=run.document_id FOR UPDATE;
  SELECT * INTO STRICT run FROM document_preview_runs WHERE id=owner FOR UPDATE;
  IF run.lease_token<>token OR run.released_at IS NOT NULL THEN RETURN FALSE; END IF;
  IF NOT run.native_exited OR NOT run.cleanup_verified OR run.creation_uncertain THEN
    UPDATE document_preview_runs SET phase='quarantined',error_code='cleanup_unverified' WHERE id=owner;
    IF job.run_id=owner AND job.version=run.job_version THEN
      UPDATE document_preview_jobs SET state='quarantined',error_code='cleanup_unverified',retryable=FALSE,updated_at=clock_timestamp() WHERE document_id=run.document_id;
      UPDATE document_previews SET state='failed',error_code='cleanup_unverified' WHERE document_id=run.document_id AND state='processing'
        AND attempt_id IS NOT DISTINCT FROM CASE WHEN run.output_count IS NULL THEN NULL::uuid ELSE run.id END;
    END IF;
    RETURN FALSE;
  END IF;
  UPDATE document_preview_runs SET phase='closed',released_at=clock_timestamp(),error_code=code WHERE id=owner;
  DELETE FROM document_reservations WHERE workspace_id=run.workspace_id AND id=owner AND kind='preview';
  IF job.run_id IS DISTINCT FROM owner OR job.version<>run.job_version THEN RETURN FALSE; END IF;
  SELECT EXISTS(SELECT 1 FROM document_previews WHERE document_id=run.document_id AND state IN ('processing','failed')
    AND attempt_id IS NOT DISTINCT FROM CASE WHEN run.output_count IS NULL THEN NULL::uuid ELSE run.id END) INTO current_preview;
  IF NOT current_preview THEN
    UPDATE document_preview_jobs SET state=CASE WHEN EXISTS(SELECT 1 FROM document_previews WHERE document_id=run.document_id AND state='ready') THEN 'ready' ELSE 'failed' END,
      error_code='superseded',retryable=FALSE,updated_at=clock_timestamp() WHERE document_id=run.document_id;
    RETURN FALSE;
  END IF;
  retry=automatic_retry AND job.attempts<job.max_attempts;
  UPDATE document_preview_jobs SET state=CASE WHEN retry THEN 'queued' ELSE 'failed' END,run_id=NULL,error_code=CASE WHEN retry THEN NULL ELSE code END,
    retryable=NOT retry,available_at=clock_timestamp()+make_interval(secs=>least(5,job.attempts)),updated_at=clock_timestamp() WHERE document_id=run.document_id;
  UPDATE document_previews SET state=CASE WHEN retry THEN 'queued' ELSE 'failed' END,error_code=CASE WHEN retry THEN NULL ELSE code END,
    attempt_id=CASE WHEN retry THEN NULL ELSE attempt_id END,generation_id=NULL WHERE document_id=run.document_id;
  RETURN TRUE;
END;
$$;

-- A complete generation is immutable, including its verified work/target receipt.
CREATE FUNCTION hmem_document_publish_preview_metadata(ws UUID,identifier UUID,generation UUID,policy TEXT,descriptors JSONB,metadata JSONB)
RETURNS VOID LANGUAGE plpgsql AS $$
DECLARE r document_reservations; d documents; p document_payloads; item JSONB; actual BIGINT:=0; used BIGINT; reserved BIGINT;
BEGIN
  SELECT * INTO STRICT d FROM documents WHERE workspace_id=ws AND id=identifier;
  IF NOT EXISTS(SELECT 1 FROM document_previews WHERE document_id=identifier AND attempt_id=generation AND state='processing') THEN
    RAISE EXCEPTION 'document_stale_preview_attempt' USING ERRCODE='23514';
  END IF;
  SELECT * INTO STRICT r FROM document_reservations WHERE workspace_id=ws AND id=generation AND kind='preview' AND expires_at>clock_timestamp();
  IF r.binding<>jsonb_build_object('document_id',identifier,'original_sha256',d.raw_sha256,'policy_version',policy,'output_count',jsonb_array_length(descriptors)) THEN
    RAISE EXCEPTION 'document_generation_binding_mismatch' USING ERRCODE='23514';
  END IF;
  IF (SELECT count(*) FROM (SELECT value->>'variant',value->>'page' FROM jsonb_array_elements(descriptors) GROUP BY 1,2) variants)<>jsonb_array_length(descriptors) THEN
    RAISE EXCEPTION 'document_duplicate_derivative' USING ERRCODE='23514';
  END IF;
  FOR item IN SELECT value FROM jsonb_array_elements(descriptors) LOOP
    SELECT * INTO STRICT p FROM document_payloads WHERE workspace_id=ws AND id=(item->>'payload_id')::uuid AND NOT original AND published_at IS NOT NULL;
    IF item->'manifest'<>jsonb_build_object('codec',p.codec,'raw_bytes',p.raw_bytes,'raw_sha256',p.raw_sha256,'compressed_bytes',p.compressed_bytes,'compressed_sha256',p.compressed_sha256,'part_count',p.part_count) THEN
      RAISE EXCEPTION 'document_derivative_manifest_mismatch' USING ERRCODE='23514';
    END IF;
    actual=actual+p.compressed_bytes;
  END LOOP;
  SELECT coalesce(sum(compressed_bytes),0) INTO used FROM document_payloads WHERE workspace_id=ws AND published_at IS NOT NULL;
  SELECT coalesce(sum(reserved_binary),0) INTO reserved FROM document_reservations WHERE workspace_id=ws AND id<>generation;
  IF actual>r.reserved_binary OR used+reserved>(SELECT binary_limit FROM document_budgets WHERE workspace_id=ws) THEN
    RAISE EXCEPTION 'document_quota_exceeded' USING ERRCODE='23514';
  END IF;
  INSERT INTO document_generations(id,workspace_id,document_id,original_sha256,policy_version,outputs,preview_metadata)
    VALUES(generation,ws,identifier,d.raw_sha256,policy,descriptors,metadata);
  DELETE FROM document_reservations WHERE workspace_id=ws AND id=generation;
  UPDATE document_previews SET state='ready',error_code=NULL,generation_id=generation WHERE document_id=identifier;
END;
$$;
CREATE OR REPLACE FUNCTION hmem_document_publish_preview(ws UUID,identifier UUID,generation UUID,policy TEXT,descriptors JSONB)
RETURNS VOID LANGUAGE sql AS $$ SELECT hmem_document_publish_preview_metadata(ws,identifier,generation,policy,descriptors,NULL); $$;

CREATE FUNCTION hmem_document_preview_publish_run(owner UUID,token UUID,descriptors JSONB,metadata JSONB) RETURNS VOID LANGUAGE plpgsql AS $$
DECLARE run document_preview_runs;
BEGIN
  run=hmem_document_preview_require_run(owner,token,TRUE);
  IF run.phase<>'closed' OR NOT run.native_exited OR NOT run.cleanup_verified OR run.creation_uncertain
    OR metadata IS NULL OR jsonb_typeof(metadata)<>'object'
    OR metadata->>'verification' IS DISTINCT FROM 'verified' OR metadata->>'original_sha256' IS DISTINCT FROM run.original_sha256
    OR metadata->>'policy_version' IS DISTINCT FROM run.policy_version OR metadata->>'image_id' IS DISTINCT FROM run.image_id
    OR metadata->>'plan_sha256' IS DISTINCT FROM run.plan_sha256 OR metadata->>'limits_sha256' IS DISTINCT FROM run.limits_sha256
    OR (metadata->>'output_count')::int IS DISTINCT FROM run.output_count OR (metadata->>'page_count')::int IS DISTINCT FROM run.page_count
    OR jsonb_array_length(descriptors)<>run.output_count THEN
    RAISE EXCEPTION 'document_preview_publication_receipt' USING ERRCODE='23514';
  END IF;
  PERFORM hmem_document_publish_preview_metadata(run.workspace_id,run.document_id,run.id,run.policy_version,descriptors,metadata);
  IF run.deadline_at<=clock_timestamp() OR run.lease_until<=clock_timestamp() THEN
    RAISE EXCEPTION 'document_preview_deadline' USING ERRCODE='23514';
  END IF;
  UPDATE document_preview_runs SET released_at=clock_timestamp() WHERE id=owner;
  UPDATE document_preview_jobs SET state='ready',error_code=NULL,retryable=FALSE,updated_at=clock_timestamp() WHERE document_id=run.document_id;
  -- Include verification, generation insertion, preview refresh fanout and final
  -- job/lease transitions in the same rollback fence, not only entry admission.
  IF run.deadline_at<=clock_timestamp() OR run.lease_until<=clock_timestamp() THEN
    RAISE EXCEPTION 'document_preview_deadline' USING ERRCODE='23514';
  END IF;
END;
$$;

-- Expiry is a scheduling hint, never proof of physical quiescence.
CREATE FUNCTION hmem_document_preview_recover(owner UUID) RETURNS BOOLEAN LANGUAGE plpgsql AS $$
DECLARE run document_preview_runs;
BEGIN
  SELECT * INTO STRICT run FROM document_preview_runs WHERE id=owner;
  IF run.released_at IS NOT NULL OR run.lease_until>clock_timestamp() THEN RETURN FALSE; END IF;
  RETURN hmem_document_preview_fail(owner,run.lease_token,'worker_lost',run.native_exited AND run.cleanup_verified AND NOT run.creation_uncertain);
END;
$$;

-- Existing queued originals become explicit retryable failures. Original bytes
-- and legacy ready generations are preserved during the upgrade.
INSERT INTO document_preview_jobs(document_id,workspace_id,state,error_code,retryable)
SELECT p.document_id,p.workspace_id,'failed','renderer_unavailable',TRUE FROM document_previews p WHERE p.state='queued';
UPDATE document_previews SET state='failed',error_code='renderer_unavailable' WHERE state='queued';

INSERT INTO schema_migrations(version,name) VALUES(60,'V060__document_preview_jobs.sql');
COMMIT;
