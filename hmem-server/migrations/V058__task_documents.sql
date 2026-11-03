BEGIN;
SELECT set_config('hmem.change_cause','migration',true);

CREATE TABLE document_budgets (
  workspace_id UUID PRIMARY KEY REFERENCES workspaces(id),
  original_limit BIGINT NOT NULL DEFAULT 26214400 CHECK(original_limit BETWEEN 0 AND 26214400),
  raw_limit BIGINT NOT NULL DEFAULT 1073741824 CHECK(raw_limit >= 0),
  binary_limit BIGINT NOT NULL DEFAULT 4294967296 CHECK(binary_limit >= 0),
  staging_limit BIGINT NOT NULL DEFAULT 268435456 CHECK(staging_limit >= 0),
  task_link_limit INTEGER NOT NULL DEFAULT 1000 CHECK(task_link_limit BETWEEN 1 AND 1000)
);

CREATE TABLE document_reservations (
  id UUID NOT NULL,
  workspace_id UUID NOT NULL REFERENCES workspaces(id),
  kind TEXT NOT NULL CHECK(kind IN ('upload','preview')),
  binding JSONB NOT NULL,
  reserved_raw BIGINT NOT NULL CHECK(reserved_raw >= 0),
  reserved_binary BIGINT NOT NULL CHECK(reserved_binary >= 0),
  created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  expires_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp()+interval '1 hour',
  PRIMARY KEY(workspace_id,id)
);

CREATE TABLE document_payloads (
  id UUID PRIMARY KEY,
  workspace_id UUID NOT NULL REFERENCES workspaces(id),
  codec TEXT NOT NULL CHECK(codec='gzip-v1'),
  raw_bytes BIGINT NOT NULL CHECK(raw_bytes BETWEEN 0 AND 104857600),
  raw_sha256 TEXT NOT NULL CHECK(raw_sha256 ~ '^[0-9a-f]{64}$'),
  compressed_bytes BIGINT NOT NULL CHECK(compressed_bytes BETWEEN 20 AND 105028000),
  compressed_sha256 TEXT NOT NULL CHECK(compressed_sha256 ~ '^[0-9a-f]{64}$'),
  part_count INTEGER NOT NULL CHECK(part_count BETWEEN 1 AND 101),
  original BOOLEAN NOT NULL,
  published_at TIMESTAMPTZ,
  UNIQUE(id,workspace_id),
  UNIQUE(workspace_id,raw_sha256,raw_bytes,original)
);
CREATE TABLE document_payload_parts (
  payload_id UUID NOT NULL,
  workspace_id UUID NOT NULL,
  ordinal INTEGER NOT NULL CHECK(ordinal >= 0),
  data BYTEA NOT NULL CHECK(octet_length(data) BETWEEN 1 AND 1048576),
  PRIMARY KEY(payload_id,ordinal),
  FOREIGN KEY(payload_id,workspace_id) REFERENCES document_payloads(id,workspace_id)
);
ALTER TABLE document_payload_parts ALTER COLUMN data SET STORAGE EXTERNAL;

CREATE TABLE documents (
  id UUID PRIMARY KEY,
  workspace_id UUID NOT NULL,
  payload_id UUID NOT NULL,
  filename TEXT NOT NULL CHECK(octet_length(filename) BETWEEN 1 AND 255),
  title TEXT NOT NULL CHECK(octet_length(title) BETWEEN 1 AND 1024),
  media_type TEXT NOT NULL CHECK(media_type IN ('image/png','image/jpeg','image/webp','application/pdf','text/html','image/svg+xml','text/plain','text/markdown','application/vnd.openxmlformats-officedocument.wordprocessingml.document','application/vnd.openxmlformats-officedocument.presentationml.presentation','application/vnd.openxmlformats-officedocument.spreadsheetml.sheet','application/x-figma','application/x-sketch')),
  raw_bytes BIGINT NOT NULL CHECK(raw_bytes BETWEEN 0 AND 26214400),
  raw_sha256 TEXT NOT NULL CHECK(raw_sha256 ~ '^[0-9a-f]{64}$'),
  uploader JSONB NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  UNIQUE(id,workspace_id),
  FOREIGN KEY(payload_id,workspace_id) REFERENCES document_payloads(id,workspace_id)
);
CREATE TABLE task_document_sources (
  id UUID PRIMARY KEY,
  workspace_id UUID NOT NULL,
  task_id UUID NOT NULL,
  document_id UUID NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  UNIQUE(workspace_id,task_id,document_id),
  FOREIGN KEY(task_id,workspace_id) REFERENCES tasks(id,workspace_id),
  FOREIGN KEY(document_id,workspace_id) REFERENCES documents(id,workspace_id)
);
CREATE TABLE task_document_evidence (
  id UUID PRIMARY KEY,
  workspace_id UUID NOT NULL,
  task_id UUID NOT NULL,
  document_id UUID NOT NULL,
  snapshot JSONB NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  UNIQUE(workspace_id,task_id,document_id),
  FOREIGN KEY(task_id,workspace_id) REFERENCES tasks(id,workspace_id),
  FOREIGN KEY(document_id,workspace_id) REFERENCES documents(id,workspace_id)
);
CREATE INDEX task_document_sources_page ON task_document_sources(workspace_id,task_id,created_at DESC,id DESC);
CREATE INDEX task_document_evidence_page ON task_document_evidence(workspace_id,task_id,created_at DESC,id DESC);

CREATE TABLE document_requests (
  workspace_id UUID NOT NULL REFERENCES workspaces(id),
  request_id UUID NOT NULL,
  binding JSONB NOT NULL,
  document_id UUID NOT NULL,
  link_id UUID NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  PRIMARY KEY(workspace_id,request_id),
  FOREIGN KEY(document_id,workspace_id) REFERENCES documents(id,workspace_id)
);

-- Attempt identity outlives its temporary budget commitment. Budget cleanup
-- must never make an old worker UUID eligible to claim the document again.
CREATE TABLE document_preview_attempts (
  id UUID PRIMARY KEY,
  workspace_id UUID NOT NULL,
  document_id UUID NOT NULL,
  binding JSONB NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  UNIQUE(id,workspace_id),
  FOREIGN KEY(document_id,workspace_id) REFERENCES documents(id,workspace_id)
);
CREATE TABLE document_previews (
  document_id UUID PRIMARY KEY,
  workspace_id UUID NOT NULL,
  state TEXT NOT NULL CHECK(state IN ('queued','processing','ready','unsupported','failed')),
  error_code TEXT,
  generation_id UUID,
  attempt_id UUID,
  FOREIGN KEY(document_id,workspace_id) REFERENCES documents(id,workspace_id)
);
CREATE TABLE document_generations (
  id UUID PRIMARY KEY,
  workspace_id UUID NOT NULL,
  document_id UUID NOT NULL,
  original_sha256 TEXT NOT NULL CHECK(original_sha256 ~ '^[0-9a-f]{64}$'),
  policy_version TEXT NOT NULL CHECK(octet_length(policy_version) BETWEEN 1 AND 128),
  outputs JSONB NOT NULL CHECK(jsonb_typeof(outputs)='array' AND jsonb_array_length(outputs) BETWEEN 1 AND 401),
  created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  UNIQUE(id,workspace_id),
  FOREIGN KEY(document_id,workspace_id) REFERENCES documents(id,workspace_id),
  FOREIGN KEY(id,workspace_id) REFERENCES document_preview_attempts(id,workspace_id)
);
ALTER TABLE document_previews ADD FOREIGN KEY(generation_id,workspace_id) REFERENCES document_generations(id,workspace_id);

CREATE FUNCTION hmem_document_immutable() RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN RAISE EXCEPTION 'document evidence and published originals are immutable' USING ERRCODE='23514'; END;
$$;
CREATE TRIGGER documents_immutable BEFORE UPDATE OR DELETE ON documents FOR EACH ROW EXECUTE FUNCTION hmem_document_immutable();
CREATE TRIGGER document_evidence_immutable BEFORE UPDATE OR DELETE ON task_document_evidence FOR EACH ROW EXECUTE FUNCTION hmem_document_immutable();
CREATE TRIGGER document_requests_immutable BEFORE UPDATE OR DELETE ON document_requests FOR EACH ROW EXECUTE FUNCTION hmem_document_immutable();
CREATE TRIGGER document_generations_immutable BEFORE UPDATE OR DELETE ON document_generations FOR EACH ROW EXECUTE FUNCTION hmem_document_immutable();
CREATE TRIGGER document_preview_attempts_immutable BEFORE UPDATE OR DELETE ON document_preview_attempts FOR EACH ROW EXECUTE FUNCTION hmem_document_immutable();
CREATE TRIGGER document_sources_no_update BEFORE UPDATE ON task_document_sources FOR EACH ROW EXECUTE FUNCTION hmem_document_immutable();
DO $$ DECLARE target TEXT; BEGIN
  FOREACH target IN ARRAY ARRAY['document_payloads','document_payload_parts','documents','task_document_evidence','document_requests','document_generations','document_preview_attempts'] LOOP
    EXECUTE format('CREATE TRIGGER session_evidence_no_truncate BEFORE TRUNCATE ON %I EXECUTE FUNCTION hmem_document_immutable()',target);
  END LOOP;
END $$;

CREATE FUNCTION hmem_document_payload_guard() RETURNS TRIGGER LANGUAGE plpgsql AS $$
DECLARE p document_payloads;
BEGIN
  IF TG_TABLE_NAME='document_payloads' THEN
    IF OLD.published_at IS NOT NULL THEN RAISE EXCEPTION 'published payload is immutable' USING ERRCODE='23514'; END IF;
    IF TG_OP='DELETE' THEN RETURN OLD; END IF;
    IF NEW IS DISTINCT FROM OLD AND NEW.published_at IS NULL THEN RAISE EXCEPTION 'payload manifest is immutable' USING ERRCODE='23514'; END IF;
    IF (to_jsonb(NEW)-'published_at') IS DISTINCT FROM (to_jsonb(OLD)-'published_at') THEN RAISE EXCEPTION 'payload manifest is immutable' USING ERRCODE='23514'; END IF;
    RETURN NEW;
  END IF;
  IF TG_OP='UPDATE' THEN RAISE EXCEPTION 'payload parts are append-only' USING ERRCODE='23514'; END IF;
  SELECT * INTO p FROM document_payloads WHERE id=coalesce(NEW.payload_id,OLD.payload_id) FOR UPDATE;
  IF p.published_at IS NOT NULL THEN RAISE EXCEPTION 'published payload parts are immutable' USING ERRCODE='23514'; END IF;
  IF TG_OP='DELETE' THEN RETURN OLD; END IF;
  IF NEW.ordinal >= p.part_count THEN RAISE EXCEPTION 'part ordinal exceeds manifest' USING ERRCODE='23514'; END IF;
  RETURN NEW;
END;
$$;
CREATE TRIGGER document_payload_guard BEFORE UPDATE OR DELETE ON document_payloads FOR EACH ROW EXECUTE FUNCTION hmem_document_payload_guard();
CREATE TRIGGER document_part_guard BEFORE INSERT OR UPDATE OR DELETE ON document_payload_parts FOR EACH ROW EXECUTE FUNCTION hmem_document_payload_guard();

CREATE FUNCTION hmem_document_json(d documents) RETURNS JSONB LANGUAGE SQL STABLE AS $$
  SELECT to_jsonb(d) || jsonb_build_object('preview_state',coalesce(p.state,'unsupported'),'preview_error',p.error_code)
  FROM document_previews p WHERE p.document_id=d.id
  UNION ALL SELECT to_jsonb(d) || jsonb_build_object('preview_state','unsupported','preview_error',NULL)
  WHERE NOT EXISTS(SELECT 1 FROM document_previews WHERE document_id=d.id);
$$;

CREATE FUNCTION hmem_document_event(ws UUID, task UUID, identifier UUID, operation TEXT, value JSONB) RETURNS VOID LANGUAGE plpgsql AS $$
BEGIN
  INSERT INTO audit_log(entity_type,entity_id,action,new_values,request_id,workspace_id,actor_type,actor_id,actor_label,bot_identity)
  VALUES('task_document',identifier::text,CASE WHEN operation='removed' THEN 'delete'::audit_action_enum ELSE 'create'::audit_action_enum END,
    value,nullif(current_setting('hmem.request_id',true),''),ws,
    nullif(current_setting('hmem.actor_type',true),'')::actor_type_enum,nullif(current_setting('hmem.actor_id',true),''),
    nullif(current_setting('hmem.actor_label',true),''),nullif(current_setting('hmem.bot_identity',true),'')::jsonb);
  PERFORM hmem_change_stream_record('workspace',ws,'task_document',identifier::text,operation,
    jsonb_build_array(jsonb_build_object('kind','entity','target','task:'||task::text),
      jsonb_build_object('kind','collection','target','task_documents:'||task::text)));
END;
$$;

-- One workspace row serializes all reservations and publications. Reservations
-- are visible budget commitments before the HTTP layer consumes any body.
CREATE FUNCTION hmem_document_reserve(ws UUID, identifier UUID, reservation_kind TEXT, input JSONB, raw_count BIGINT, binary_count BIGINT) RETURNS VOID LANGUAGE plpgsql AS $$
DECLARE b document_budgets; old document_reservations; used_raw BIGINT; used_binary BIGINT; staged BIGINT;
BEGIN
  IF NOT EXISTS(SELECT 1 FROM workspaces WHERE id=ws AND deleted_at IS NULL FOR SHARE) THEN RAISE EXCEPTION 'document_workspace_not_found' USING ERRCODE='23514'; END IF;
  INSERT INTO document_budgets(workspace_id) VALUES(ws) ON CONFLICT DO NOTHING;
  SELECT * INTO b FROM document_budgets WHERE workspace_id=ws FOR UPDATE;
  DELETE FROM document_reservations WHERE workspace_id=ws AND expires_at<=clock_timestamp();
  SELECT * INTO old FROM document_reservations WHERE workspace_id=ws AND id=identifier;
  IF FOUND THEN
    IF old.kind<>reservation_kind OR old.binding<>input OR old.reserved_binary<>binary_count THEN RAISE EXCEPTION 'document_request_conflict' USING ERRCODE='23514'; END IF;
    RETURN;
  END IF;
  IF raw_count<0 OR binary_count<0 OR reservation_kind='upload' AND (input->'manifest'->>'raw_bytes')::bigint>b.original_limit THEN RAISE EXCEPTION 'document_size_exceeded' USING ERRCODE='23514'; END IF;
  SELECT coalesce(sum(raw_bytes) FILTER(WHERE original),0),coalesce(sum(compressed_bytes),0) INTO used_raw,used_binary FROM document_payloads WHERE workspace_id=ws AND published_at IS NOT NULL;
  SELECT coalesce(sum(reserved_raw),0)+used_raw,coalesce(sum(reserved_binary),0) INTO used_raw,staged FROM document_reservations WHERE workspace_id=ws;
  IF used_raw+raw_count>b.raw_limit OR used_binary+staged+binary_count>b.binary_limit OR staged+binary_count>b.staging_limit THEN RAISE EXCEPTION 'document_quota_exceeded' USING ERRCODE='23514'; END IF;
  INSERT INTO document_reservations(id,workspace_id,kind,binding,reserved_raw,reserved_binary) VALUES(identifier,ws,reservation_kind,input,raw_count,binary_count);
END;
$$;

CREATE FUNCTION hmem_document_validate_payload(identifier UUID, ws UUID) RETURNS VOID LANGUAGE plpgsql AS $$
DECLARE p document_payloads; actual BIGINT; count_parts INTEGER; whole BYTEA;
BEGIN
  SELECT * INTO STRICT p FROM document_payloads WHERE id=identifier AND workspace_id=ws FOR UPDATE;
  SELECT count(*),coalesce(sum(octet_length(data)),0),string_agg(data,''::bytea ORDER BY ordinal) INTO count_parts,actual,whole FROM document_payload_parts WHERE payload_id=identifier;
  IF count_parts<>p.part_count OR actual<>p.compressed_bytes OR encode(sha256(whole),'hex')<>p.compressed_sha256 OR
    EXISTS(SELECT 1 FROM generate_series(0,p.part_count-1) n WHERE NOT EXISTS(SELECT 1 FROM document_payload_parts WHERE payload_id=identifier AND ordinal=n)) THEN
    RAISE EXCEPTION 'document_compressed_manifest_mismatch' USING ERRCODE='23514';
  END IF;
  UPDATE document_payloads SET published_at=clock_timestamp() WHERE id=identifier AND published_at IS NULL;
END;
$$;

CREATE FUNCTION hmem_document_check_task(ws UUID, task UUID) RETURNS VOID LANGUAGE plpgsql AS $$
BEGIN
  IF NOT EXISTS(SELECT 1 FROM tasks t JOIN workspaces w ON w.id=t.workspace_id WHERE t.id=task AND t.workspace_id=ws AND t.deleted_at IS NULL AND w.deleted_at IS NULL FOR SHARE OF t,w) THEN
    RAISE EXCEPTION 'document_task_not_found' USING ERRCODE='23514';
  END IF;
END;
$$;
CREATE FUNCTION hmem_document_check_link_limit(ws UUID, task UUID) RETURNS VOID LANGUAGE plpgsql AS $$
BEGIN
  IF (SELECT count(*) FROM (SELECT id FROM task_document_sources WHERE workspace_id=ws AND task_id=task UNION ALL SELECT id FROM task_document_evidence WHERE workspace_id=ws AND task_id=task) links)>=(SELECT task_link_limit FROM document_budgets WHERE workspace_id=ws) THEN
    RAISE EXCEPTION 'document_task_link_limit' USING ERRCODE='23514';
  END IF;
END;
$$;

CREATE FUNCTION hmem_document_publish(ws UUID, request UUID, payload UUID, identifier UUID, association UUID) RETURNS JSONB LANGUAGE plpgsql AS $$
DECLARE r document_reservations; p document_payloads; d documents; target UUID; selected_role TEXT; original JSONB; actor JSONB;
BEGIN
  PERFORM 1 FROM document_budgets WHERE workspace_id=ws FOR UPDATE;
  SELECT * INTO STRICT r FROM document_reservations WHERE workspace_id=ws AND id=request AND kind='upload' AND expires_at>clock_timestamp();
  target=(r.binding->>'task_id')::uuid; selected_role=r.binding->>'role'; original=r.binding->'manifest';
  PERFORM hmem_document_check_task(ws,target);
  PERFORM hmem_document_check_link_limit(ws,target);
  SELECT * INTO STRICT p FROM document_payloads pp WHERE pp.id=payload AND pp.workspace_id=ws AND pp.original=true;
  IF p.raw_bytes<>(original->>'raw_bytes')::bigint OR p.raw_sha256<>original->>'raw_sha256' OR p.compressed_bytes>r.reserved_binary THEN RAISE EXCEPTION 'document_manifest_mismatch' USING ERRCODE='23514'; END IF;
  PERFORM hmem_document_validate_payload(payload,ws);
  actor=jsonb_build_object('actor_type',nullif(current_setting('hmem.actor_type',true),''),'actor_id',nullif(current_setting('hmem.actor_id',true),''),'actor_label',nullif(current_setting('hmem.actor_label',true),''),'bot_identity',nullif(current_setting('hmem.bot_identity',true),'')::jsonb);
  INSERT INTO documents(id,workspace_id,payload_id,filename,title,media_type,raw_bytes,raw_sha256,uploader)
  VALUES(identifier,ws,payload,original->>'filename',original->>'title',original->>'media_type',p.raw_bytes,p.raw_sha256,actor) RETURNING * INTO d;
  INSERT INTO document_previews(document_id,workspace_id,state) VALUES(identifier,ws,CASE WHEN d.media_type IN ('image/png','image/jpeg','image/webp','application/pdf','text/html') THEN 'queued' ELSE 'unsupported' END);
  IF selected_role='source' THEN
    INSERT INTO task_document_sources(id,workspace_id,task_id,document_id) VALUES(association,ws,target,identifier);
  ELSIF selected_role='evidence' THEN
    INSERT INTO task_document_evidence(id,workspace_id,task_id,document_id,snapshot) VALUES(association,ws,target,identifier,to_jsonb(d));
  ELSE RAISE EXCEPTION 'document_invalid_role' USING ERRCODE='23514'; END IF;
  INSERT INTO document_requests(workspace_id,request_id,binding,document_id,link_id) VALUES(ws,request,r.binding,identifier,association);
  DELETE FROM document_reservations WHERE workspace_id=ws AND id=request;
  PERFORM hmem_document_event(ws,target,association,'created',jsonb_build_object('task_id',target,'document_id',identifier,'role',selected_role,'filename',d.filename,'raw_bytes',d.raw_bytes,'raw_sha256',d.raw_sha256));
  RETURN hmem_document_json(d);
END;
$$;

CREATE FUNCTION hmem_document_request_conflict() RETURNS JSONB LANGUAGE plpgsql AS $$
BEGIN RAISE EXCEPTION 'document_request_conflict' USING ERRCODE='23514'; END;
$$;
CREATE FUNCTION hmem_document_require_reservation(ws UUID, identifier UUID, input JSONB) RETURNS VOID LANGUAGE plpgsql AS $$
BEGIN
  IF NOT EXISTS(SELECT 1 FROM document_reservations WHERE workspace_id=ws AND id=identifier AND binding=input AND kind='upload' AND expires_at>clock_timestamp()) THEN
    RAISE EXCEPTION 'document_reservation_missing_or_conflicting' USING ERRCODE='23514';
  END IF;
END;
$$;

CREATE FUNCTION hmem_document_add_evidence(ws UUID, task UUID, identifier UUID, request UUID, association UUID) RETURNS JSONB LANGUAGE plpgsql AS $$
DECLARE input JSONB := jsonb_build_object('operation','add_evidence','task_id',task,'document_id',identifier); old document_requests; d documents; existing UUID;
BEGIN
  PERFORM hmem_document_check_task(ws,task);
  SELECT * INTO old FROM document_requests WHERE workspace_id=ws AND request_id=request;
  IF FOUND THEN
    IF old.binding<>input THEN RETURN hmem_document_request_conflict(); END IF;
    RETURN (SELECT hmem_document_json(x) FROM documents x WHERE x.workspace_id=ws AND x.id=identifier);
  END IF;
  IF NOT EXISTS(SELECT 1 FROM task_document_sources WHERE workspace_id=ws AND task_id=task AND document_id=identifier)
    AND NOT EXISTS(SELECT 1 FROM task_document_evidence WHERE workspace_id=ws AND task_id=task AND document_id=identifier) THEN
    RAISE EXCEPTION 'document_not_found' USING ERRCODE='23514';
  END IF;
  SELECT * INTO STRICT d FROM documents WHERE workspace_id=ws AND id=identifier;
  SELECT id INTO existing FROM task_document_evidence WHERE workspace_id=ws AND task_id=task AND document_id=identifier;
  IF existing IS NULL THEN
    PERFORM hmem_document_check_link_limit(ws,task);
    INSERT INTO task_document_evidence(id,workspace_id,task_id,document_id,snapshot) VALUES(association,ws,task,identifier,to_jsonb(d));
    existing=association;
    PERFORM hmem_document_event(ws,task,association,'created',jsonb_build_object('task_id',task,'document_id',identifier,'role','evidence','filename',d.filename,'raw_bytes',d.raw_bytes,'raw_sha256',d.raw_sha256));
  END IF;
  INSERT INTO document_requests(workspace_id,request_id,binding,document_id,link_id) VALUES(ws,request,input,identifier,existing);
  RETURN hmem_document_json(d);
END;
$$;

CREATE FUNCTION hmem_document_remove_source(ws UUID, task UUID, identifier UUID) RETURNS VOID LANGUAGE plpgsql AS $$
DECLARE association UUID;
BEGIN
  DELETE FROM task_document_sources WHERE workspace_id=ws AND task_id=task AND document_id=identifier RETURNING id INTO association;
  IF association IS NOT NULL THEN
    PERFORM hmem_document_event(ws,task,association,'removed',jsonb_build_object('task_id',task,'document_id',identifier,'role','source'));
  END IF;
END;
$$;

CREATE FUNCTION hmem_document_reserve_preview(ws UUID, identifier UUID, generation UUID, policy TEXT, output_count INTEGER, budget BIGINT) RETURNS VOID LANGUAGE plpgsql AS $$
DECLARE d documents; input JSONB; retry BOOLEAN; active UUID; prior document_preview_attempts;
BEGIN
  PERFORM 1 FROM document_budgets WHERE workspace_id=ws FOR UPDATE;
  SELECT * INTO STRICT d FROM documents WHERE workspace_id=ws AND id=identifier;
  input=jsonb_build_object('document_id',identifier,'original_sha256',d.raw_sha256,'policy_version',policy,'output_count',output_count);
  IF EXISTS(SELECT 1 FROM document_generations WHERE id=generation) THEN RAISE EXCEPTION 'document_generation_already_published' USING ERRCODE='23514'; END IF;
  SELECT * INTO prior FROM document_preview_attempts WHERE id=generation;
  retry=FOUND;
  SELECT attempt_id INTO active FROM document_previews WHERE document_id=identifier;
  IF retry AND (prior.workspace_id<>ws OR prior.document_id<>identifier OR prior.binding<>input) THEN
    RAISE EXCEPTION 'document_preview_attempt_conflict' USING ERRCODE='23514';
  END IF;
  IF retry AND NOT EXISTS(SELECT 1 FROM document_reservations WHERE workspace_id=ws AND id=generation AND expires_at>clock_timestamp()) THEN
    RAISE EXCEPTION 'document_stale_preview_attempt' USING ERRCODE='23514';
  END IF;
  IF retry AND active IS DISTINCT FROM generation THEN RAISE EXCEPTION 'document_stale_preview_attempt' USING ERRCODE='23514'; END IF;
  PERFORM hmem_document_reserve(ws,generation,'preview',input,0,budget);
  IF NOT retry THEN
    INSERT INTO document_preview_attempts(id,workspace_id,document_id,binding) VALUES(generation,ws,identifier,input);
    UPDATE document_previews SET state='processing',error_code=NULL,attempt_id=generation WHERE document_id=identifier;
  END IF;
END;
$$;

CREATE FUNCTION hmem_document_publish_preview(ws UUID, identifier UUID, generation UUID, policy TEXT, descriptors JSONB) RETURNS VOID LANGUAGE plpgsql AS $$
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
  INSERT INTO document_generations(id,workspace_id,document_id,original_sha256,policy_version,outputs) VALUES(generation,ws,identifier,d.raw_sha256,policy,descriptors);
  DELETE FROM document_reservations WHERE workspace_id=ws AND id=generation;
  UPDATE document_previews SET state='ready',error_code=NULL,generation_id=generation WHERE document_id=identifier;
END;
$$;

CREATE FUNCTION hmem_document_preview_change() RETURNS TRIGGER LANGUAGE plpgsql AS $$
DECLARE task UUID;
BEGIN
  IF TG_OP='UPDATE' AND NEW IS NOT DISTINCT FROM OLD THEN RETURN NULL; END IF;
  FOR task IN SELECT task_id FROM task_document_sources WHERE document_id=NEW.document_id UNION SELECT task_id FROM task_document_evidence WHERE document_id=NEW.document_id LOOP
    PERFORM hmem_change_stream_record('workspace',NEW.workspace_id,'document',NEW.document_id::text,'updated',
      jsonb_build_array(jsonb_build_object('kind','entity','target','task:'||task::text),jsonb_build_object('kind','collection','target','task_documents:'||task::text)));
  END LOOP;
  RETURN NULL;
END;
$$;
CREATE TRIGGER document_preview_change AFTER UPDATE ON document_previews FOR EACH ROW EXECUTE FUNCTION hmem_document_preview_change();

INSERT INTO schema_migrations(version,name) VALUES(58,'V058__task_documents.sql');
COMMIT;
