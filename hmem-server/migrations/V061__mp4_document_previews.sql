BEGIN;

ALTER TABLE documents DROP CONSTRAINT documents_media_type_check;
ALTER TABLE documents ADD CONSTRAINT documents_media_type_check CHECK (media_type IN (
  'image/png','image/jpeg','image/webp','video/mp4','application/pdf','text/html','image/svg+xml','text/plain','text/markdown',
  'application/vnd.openxmlformats-officedocument.wordprocessingml.document',
  'application/vnd.openxmlformats-officedocument.presentationml.presentation',
  'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet','application/x-figma','application/x-sketch'
));

-- Publication remains atomic; MP4 now enters the same bounded derivative queue.
CREATE OR REPLACE FUNCTION hmem_document_publish(ws UUID, request UUID, payload UUID, identifier UUID, association UUID) RETURNS JSONB LANGUAGE plpgsql AS $$
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
  INSERT INTO document_previews(document_id,workspace_id,state) VALUES(identifier,ws,CASE WHEN d.media_type IN ('image/png','image/jpeg','image/webp','video/mp4','application/pdf','text/html') THEN 'queued' ELSE 'unsupported' END);
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

INSERT INTO schema_migrations(version,name) VALUES(61,'V061__mp4_document_previews.sql');
COMMIT;
