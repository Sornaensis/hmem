BEGIN;
SELECT set_config('hmem.change_cause','migration',true);

CREATE TABLE document_download_grants (
  id UUID PRIMARY KEY,
  token_sha256 TEXT NOT NULL UNIQUE CHECK(token_sha256 ~ '^[0-9a-f]{64}$'),
  workspace_id UUID NOT NULL,
  task_id UUID NOT NULL,
  document_id UUID NOT NULL,
  binding JSONB NOT NULL CHECK(jsonb_typeof(binding)='object'),
  issuer JSONB NOT NULL CHECK(jsonb_typeof(issuer)='object' AND octet_length(issuer::text)<=4096
    AND coalesce(issuer->'credential_origin'->>'kind' IN ('pat','browser','local'),false)),
  authority_epoch UUID NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  expires_at TIMESTAMPTZ NOT NULL,
  revoked_at TIMESTAMPTZ,
  CHECK(expires_at>created_at AND expires_at<=created_at+interval '15 minutes'),
  CHECK(binding->>'workspace_id'=workspace_id::text AND binding->>'task_id'=task_id::text
    AND binding->>'document_id'=document_id::text),
  FOREIGN KEY(task_id,workspace_id) REFERENCES tasks(id,workspace_id),
  FOREIGN KEY(document_id,workspace_id) REFERENCES documents(id,workspace_id)
);
CREATE INDEX document_download_grants_task ON document_download_grants(workspace_id,task_id,created_at DESC,id);

-- One resolver binds exact role/link identity and the current complete generation.
-- Source removal/recreation cannot revive a grant for the old association UUID.
CREATE FUNCTION hmem_document_grant_binding(ws UUID,target UUID,identifier UUID,selected_role TEXT,
  association UUID,selected_variant TEXT,selected_page INTEGER,selected_generation UUID)
RETURNS JSONB LANGUAGE plpgsql STABLE AS $$
DECLARE d documents; p document_payloads; g document_generations; descriptor JSONB;
BEGIN
  IF selected_role NOT IN ('source','evidence') OR selected_variant NOT IN ('original','page','thumbnail','styled_html_preview')
    OR selected_page<0 OR selected_page>200 THEN RETURN NULL; END IF;
  SELECT doc.* INTO d FROM documents doc
    JOIN tasks t ON t.id=target AND t.workspace_id=ws AND t.deleted_at IS NULL
    JOIN workspaces w ON w.id=ws AND w.deleted_at IS NULL
    WHERE doc.workspace_id=ws AND doc.id=identifier AND (
      (selected_role='source' AND EXISTS(SELECT 1 FROM task_document_sources s
        WHERE s.id=association AND s.workspace_id=ws AND s.task_id=target AND s.document_id=identifier))
      OR (selected_role='evidence' AND EXISTS(SELECT 1 FROM task_document_evidence e
        WHERE e.id=association AND e.workspace_id=ws AND e.task_id=target AND e.document_id=identifier)));
  IF NOT FOUND THEN RETURN NULL; END IF;
  IF selected_variant='original' THEN
    IF selected_page<>0 OR selected_generation IS NOT NULL THEN RETURN NULL; END IF;
    SELECT * INTO p FROM document_payloads WHERE id=d.payload_id AND workspace_id=ws AND original AND published_at IS NOT NULL;
  ELSE
    IF selected_generation IS NULL OR (selected_variant='styled_html_preview' AND selected_page<>0) THEN RETURN NULL; END IF;
    SELECT gen.* INTO g FROM document_previews current_preview JOIN document_generations gen
      ON gen.id=current_preview.generation_id AND gen.workspace_id=current_preview.workspace_id
      WHERE current_preview.document_id=identifier AND current_preview.workspace_id=ws AND current_preview.state='ready'
        AND gen.id=selected_generation AND gen.document_id=identifier AND gen.original_sha256=d.raw_sha256;
    IF NOT FOUND THEN RETURN NULL; END IF;
    SELECT jsonb_agg(value) INTO descriptor FROM jsonb_array_elements(g.outputs)
      WHERE value->>'variant'=selected_variant AND (value->>'page')::integer=selected_page;
    IF descriptor IS NULL OR jsonb_array_length(descriptor)<>1 THEN RETURN NULL; END IF;
    descriptor=descriptor->0;
    IF (selected_variant='styled_html_preview' AND descriptor->>'media_type'<>'text/html')
      OR (selected_variant<>'styled_html_preview' AND descriptor->>'media_type' NOT IN ('image/png','image/jpeg','image/webp'))
      THEN RETURN NULL; END IF;
    SELECT * INTO p FROM document_payloads WHERE id=(descriptor->>'payload_id')::uuid AND workspace_id=ws
      AND NOT original AND published_at IS NOT NULL;
  END IF;
  IF NOT FOUND THEN RETURN NULL; END IF;
  RETURN jsonb_build_object('workspace_id',ws,'task_id',target,'document_id',identifier,
    'role',selected_role,'association_id',association,'variant',selected_variant,'page',selected_page,
    'generation_id',g.id,'policy_version',g.policy_version,'original_sha256',d.raw_sha256,
    'payload_id',p.id,'raw_bytes',p.raw_bytes,'raw_sha256',p.raw_sha256,
    'media_type',CASE WHEN selected_variant='original' THEN d.media_type ELSE descriptor->>'media_type' END,'filename',d.filename);
END;
$$;

-- Grants have immutable scope/issuer/epoch; only one-way explicit revocation is mutable.
CREATE FUNCTION hmem_document_grant_guard() RETURNS TRIGGER LANGUAGE plpgsql AS $$
DECLARE expected JSONB;
BEGIN
  IF TG_OP='INSERT' THEN
    expected=hmem_document_grant_binding(NEW.workspace_id,NEW.task_id,NEW.document_id,NEW.binding->>'role',
      (NEW.binding->>'association_id')::uuid,NEW.binding->>'variant',(NEW.binding->>'page')::integer,
      (NEW.binding->>'generation_id')::uuid);
    IF expected IS NULL OR expected<>NEW.binding OR NEW.revoked_at IS NOT NULL THEN
      RAISE EXCEPTION 'document grant binding is not current' USING ERRCODE='23514';
    END IF;
    RETURN NEW;
  END IF;
  IF TG_OP='UPDATE' AND OLD.revoked_at IS NULL AND NEW.revoked_at IS NOT NULL
    AND (to_jsonb(NEW)-'revoked_at')=(to_jsonb(OLD)-'revoked_at') THEN RETURN NEW; END IF;
  RAISE EXCEPTION 'document grant scope and issuer are immutable' USING ERRCODE='23514';
END;
$$;
CREATE TRIGGER document_grant_guard BEFORE INSERT OR UPDATE OR DELETE ON document_download_grants
  FOR EACH ROW EXECUTE FUNCTION hmem_document_grant_guard();
CREATE TRIGGER session_evidence_no_truncate BEFORE TRUNCATE ON document_download_grants
  FOR EACH STATEMENT EXECUTE FUNCTION hmem_document_immutable();

CREATE FUNCTION hmem_document_grant_public_record(g document_download_grants) RETURNS JSONB LANGUAGE sql STABLE AS $$
  SELECT jsonb_build_object('id',g.id,'binding',g.binding,'issuer',g.issuer,'authority_epoch',g.authority_epoch,
    'created_at',g.created_at,'expires_at',g.expires_at,'revoked_at',g.revoked_at)
$$;

CREATE FUNCTION hmem_document_grant_event(g document_download_grants,operation TEXT) RETURNS VOID LANGUAGE plpgsql AS $$
BEGIN
  INSERT INTO audit_log(entity_type,entity_id,action,new_values,request_id,workspace_id,actor_type,actor_id,actor_label,bot_identity)
  VALUES('task_document_grant',g.id::text,CASE WHEN operation='revoked' THEN 'delete'::audit_action_enum ELSE 'create'::audit_action_enum END,
    jsonb_build_object('grant_id',g.id),
    nullif(current_setting('hmem.request_id',true),''),g.workspace_id,
    nullif(current_setting('hmem.actor_type',true),'')::actor_type_enum,nullif(current_setting('hmem.actor_id',true),''),
    nullif(current_setting('hmem.actor_label',true),''),nullif(current_setting('hmem.bot_identity',true),'')::jsonb);
  -- Temporary grants change no workspace resource projection. Audit only; do
  -- not introduce an unsupported canonical entity/action for live clients.
END;
$$;

CREATE FUNCTION hmem_document_grant_create(identifier UUID,token_hash TEXT,origin_issuer JSONB,epoch UUID,
  exact_binding JSONB,expiry TIMESTAMPTZ) RETURNS JSONB LANGUAGE plpgsql AS $$
DECLARE g document_download_grants;
BEGIN
  IF expiry<=clock_timestamp() OR expiry>clock_timestamp()+interval '15 minutes' THEN
    RAISE EXCEPTION 'document grant lifetime is invalid' USING ERRCODE='23514';
  END IF;
  INSERT INTO document_download_grants(id,token_sha256,workspace_id,task_id,document_id,binding,issuer,authority_epoch,expires_at)
    VALUES(identifier,token_hash,(exact_binding->>'workspace_id')::uuid,(exact_binding->>'task_id')::uuid,
      (exact_binding->>'document_id')::uuid,exact_binding,origin_issuer,epoch,expiry) RETURNING * INTO g;
  PERFORM hmem_document_grant_event(g,'created');
  RETURN hmem_document_grant_public_record(g);
END;
$$;

CREATE FUNCTION hmem_document_grant_revoke(ws UUID,target UUID,document UUID,identifier UUID)
RETURNS BOOLEAN LANGUAGE plpgsql AS $$
DECLARE g document_download_grants;
BEGIN
  UPDATE document_download_grants SET revoked_at=clock_timestamp()
    WHERE id=identifier AND workspace_id=ws AND task_id=target AND document_id=document AND revoked_at IS NULL RETURNING * INTO g;
  IF NOT FOUND THEN RETURN false; END IF;
  PERFORM hmem_document_grant_event(g,'revoked');
  RETURN true;
END;
$$;

INSERT INTO schema_migrations(version,name) VALUES(59,'V059__document_download_grants.sql');
COMMIT;
