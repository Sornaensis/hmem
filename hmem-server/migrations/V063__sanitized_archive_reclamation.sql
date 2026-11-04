BEGIN;

-- Extend the existing isolated capability without changing its signature,
-- owner, grants or the controls used by exact-only reclamation.
ALTER TABLE session_archive_control
  ADD COLUMN sanitized_qualified BOOLEAN NOT NULL DEFAULT FALSE,
  ADD COLUMN sanitized_reclaim_enabled BOOLEAN NOT NULL DEFAULT FALSE,
  ADD CHECK(NOT sanitized_reclaim_enabled OR sanitized_qualified);

ALTER TABLE session_archive_reclamation
  ADD COLUMN sanitized_attestation_sha256 TEXT CHECK(sanitized_attestation_sha256 ~ '^[0-9a-f]{64}$');

CREATE FUNCTION hmem_archive_sanitized_runtime_safe() RETURNS BOOLEAN
LANGUAGE SQL STABLE SET search_path=pg_catalog AS $$
 SELECT public.hmem_archive_runtime_safe()
   AND NOT EXISTS(SELECT 1 FROM pg_proc p CROSS JOIN LATERAL aclexplode(coalesce(p.proacl,acldefault('f',p.proowner))) access
     WHERE p.oid='public.hmem_archive_reclaim(uuid,uuid,uuid,text,uuid)'::regprocedure
       AND access.grantee=0 AND access.privilege_type='EXECUTE')
   AND NOT EXISTS(SELECT 1 FROM pg_roles r WHERE pg_has_role(session_user,r.oid,'MEMBER')
     AND (r.rolsuper OR r.rolcreaterole OR r.rolbypassrls OR r.rolreplication))
   AND NOT EXISTS(SELECT 1 FROM pg_class t JOIN pg_namespace n ON n.oid=t.relnamespace
     WHERE n.nspname='public' AND t.relrowsecurity AND t.relname IN
       ('sessions','workspaces','session_occurrences','session_chunks','session_archive_control',
        'session_archive_jobs','session_archive_generations','session_archive_reclamation',
        'session_archive_ledger','session_archive_sanitized_contents'))
   AND NOT EXISTS(SELECT 1 FROM pg_class t JOIN pg_namespace n ON n.oid=t.relnamespace
     WHERE n.nspname='public' AND t.relname IN
       ('session_archive_sanitized_contents','session_archive_content_blocks','session_archive_replacement_policies','session_occurrences','workspaces')
     AND pg_has_role(session_user,t.relowner,'MEMBER'))
   AND NOT EXISTS(SELECT 1 FROM pg_proc p JOIN pg_namespace n ON n.oid=p.pronamespace
     WHERE n.nspname='public' AND p.proname IN ('hmem_session_occurrence_retention_guard','hmem_session_reject_truncate')
     AND pg_has_role(session_user,p.proowner,'MEMBER'))
   AND NOT EXISTS(SELECT 1 FROM pg_class t JOIN pg_namespace n ON n.oid=t.relnamespace
     CROSS JOIN pg_roles a WHERE n.nspname='public'
       AND t.relname IN ('session_archive_sanitized_contents','session_archive_content_blocks',
                         'session_archive_replacement_policies','sessions','session_occurrences','workspaces')
       AND pg_has_role(session_user,a.oid,'MEMBER') AND has_table_privilege(a.oid,t.oid,'TRIGGER'))
   AND NOT EXISTS(SELECT 1 FROM pg_trigger hook JOIN pg_class t ON t.oid=hook.tgrelid
     JOIN pg_namespace n ON n.oid=t.relnamespace JOIN pg_proc callback ON callback.oid=hook.tgfoid
     WHERE n.nspname='public' AND t.relname IN ('session_archive_sanitized_contents','session_archive_content_blocks',
       'session_archive_replacement_policies','sessions','session_occurrences','workspaces')
     AND pg_has_role(session_user,callback.proowner,'MEMBER'))
$$;

CREATE OR REPLACE FUNCTION hmem_archive_control_guard() RETURNS TRIGGER
LANGUAGE plpgsql SET search_path=pg_catalog AS $$
BEGIN
  IF NEW.reclaim_enabled AND NOT OLD.reclaim_enabled AND NOT public.hmem_archive_runtime_safe() THEN
    RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Reclamation requires a restricted runtime caller with an explicit capability grant';
  END IF;
  IF NEW.sanitized_reclaim_enabled AND NOT OLD.sanitized_reclaim_enabled
    AND NOT public.hmem_archive_sanitized_runtime_safe() THEN
    RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Sanitized reclamation requires a restricted runtime caller with an explicit capability grant';
  END IF;
  RETURN NEW;
END $$;

-- Activation serializes with every archive-job insertion, including on-demand
-- insertion. Its clock and all normalized per-format rows change together.
CREATE OR REPLACE FUNCTION hmem_archive_freeze_replacement() RETURNS TRIGGER
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog AS $$
BEGIN
  IF TG_OP='INSERT' THEN
    PERFORM 1 FROM public.session_archive_control WHERE singleton FOR SHARE;
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

CREATE FUNCTION hmem_archive_sanitized_attestation(g session_archive_generations, policy TEXT) RETURNS TEXT
LANGUAGE SQL IMMUTABLE STRICT SET search_path=pg_catalog AS $$
  SELECT public.hmem_archive_binding('',jsonb_build_array('sanitized-reclamation-v1',
    (g).id,(g).workspace_id,(g).session_id,(g).metadata_sha256,policy,
    (g).raw_bytes,(g).raw_records,(g).raw_revision,(g).raw_sha256))
$$;

-- The trusted worker can write a bounded verification receipt while it holds
-- the current permit. It cannot write, reset or discard deletion progress.
CREATE OR REPLACE FUNCTION hmem_archive_exact_reclamation_guard() RETURNS TRIGGER
LANGUAGE plpgsql SET search_path=pg_catalog AS $$
DECLARE j public.session_archive_jobs%ROWTYPE; g public.session_archive_generations%ROWTYPE;
  c public.session_archive_control%ROWTYPE;
BEGIN
  IF TG_OP<>'INSERT' THEN
    SELECT * INTO j FROM public.session_archive_jobs WHERE session_id=OLD.session_id AND workspace_id=OLD.workspace_id;
    IF j.replacement_snapshot IS NOT NULL THEN
      IF TG_OP='DELETE' THEN
        RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Sanitized reclamation progress cannot be discarded';
      END IF;
      IF (NEW.generation_id,NEW.session_id,NEW.workspace_id,NEW.metadata_sha256)
        IS DISTINCT FROM (OLD.generation_id,OLD.session_id,OLD.workspace_id,OLD.metadata_sha256) THEN
        RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Sanitized reclamation identity is immutable';
      END IF;
      IF current_user='hmem_archive_maintenance_owner' THEN RETURN NEW; END IF;
      IF (NEW.deleted_revision,NEW.deleted_bytes,NEW.complete) IS DISTINCT FROM
        (OLD.deleted_revision,OLD.deleted_bytes,OLD.complete)
        OR (OLD.complete AND NEW IS DISTINCT FROM OLD)
        OR (OLD.deleted_bytes>0 AND (NEW.verified_at,NEW.sanitized_attestation_sha256)
          IS DISTINCT FROM (OLD.verified_at,OLD.sanitized_attestation_sha256)) THEN
        RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Sanitized deletion progress requires the maintenance capability';
      END IF;
      IF (NEW.verified_at,NEW.sanitized_attestation_sha256) IS NOT DISTINCT FROM
        (OLD.verified_at,OLD.sanitized_attestation_sha256) THEN RETURN NEW; END IF;
    END IF;
  END IF;
  IF TG_OP='DELETE' THEN RETURN OLD; END IF;
  SELECT * INTO j FROM public.session_archive_jobs WHERE session_id=NEW.session_id AND workspace_id=NEW.workspace_id;
  IF j.replacement_snapshot IS NULL THEN
    IF NEW.sanitized_attestation_sha256 IS NOT NULL THEN
      RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Exact archives cannot carry a sanitized attestation';
    END IF;
    RETURN NEW;
  END IF;
  SELECT * INTO g FROM public.session_archive_generations
    WHERE id=NEW.generation_id AND session_id=NEW.session_id AND workspace_id=NEW.workspace_id;
  SELECT * INTO STRICT c FROM public.session_archive_control WHERE singleton;
  IF j.state IS DISTINCT FROM 'ready' OR j.generation_id IS DISTINCT FROM g.id
    OR g.published_at IS NULL OR NEW.metadata_sha256 IS DISTINCT FROM g.metadata_sha256
    OR c.permit_kind IS DISTINCT FROM 'maintenance' OR c.permit_generation IS DISTINCT FROM g.id
    OR c.permit_expires_at IS NULL OR c.permit_expires_at<=clock_timestamp()
    OR NEW.deleted_revision<>0 OR NEW.deleted_bytes<>0 OR NEW.complete
    OR NEW.sanitized_attestation_sha256 IS DISTINCT FROM
      (CASE WHEN NEW.verified_at IS NOT NULL THEN public.hmem_archive_sanitized_attestation(g,j.replacement_policy_sha256) END)
    OR (NEW.verified_at IS NOT NULL AND NEW.error_code IS NOT NULL) THEN
    RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Sanitized attestation requires a fenced verified publication';
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER session_archive_exact_reclamation_guard ON session_archive_reclamation;
CREATE TRIGGER session_archive_exact_reclamation_guard BEFORE INSERT OR UPDATE OR DELETE
  ON session_archive_reclamation FOR EACH ROW EXECUTE FUNCTION hmem_archive_exact_reclamation_guard();
CREATE TRIGGER session_evidence_no_truncate BEFORE TRUNCATE ON session_archive_reclamation
  FOR EACH STATEMENT EXECUTE FUNCTION hmem_session_reject_truncate();

-- The original-derived projection remains evidence after source retirement.
-- The maintenance capability locks its row before the first delete, in the
-- same session-to-occurrence order as the derivation worker.
CREATE FUNCTION hmem_session_occurrence_retention_guard() RETURNS TRIGGER
LANGUAGE plpgsql SET search_path=pg_catalog AS $$
BEGIN
  IF EXISTS(SELECT 1 FROM public.session_archive_reclamation r JOIN public.session_archive_jobs j
      ON (j.generation_id,j.session_id,j.workspace_id)=(r.generation_id,r.session_id,r.workspace_id)
    WHERE r.session_id=OLD.session_id AND r.workspace_id=OLD.workspace_id
      AND j.replacement_snapshot IS NOT NULL AND r.deleted_bytes>0) THEN
    RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Retired original occurrence projection cannot be replaced';
  END IF;
  IF TG_OP='DELETE' THEN RETURN OLD; END IF;
  RETURN NEW;
END $$;
CREATE TRIGGER session_occurrence_retention_guard BEFORE UPDATE OR DELETE ON session_occurrences
  FOR EACH ROW EXECUTE FUNCTION hmem_session_occurrence_retention_guard();
CREATE TRIGGER session_evidence_no_truncate BEFORE TRUNCATE ON session_occurrences
  FOR EACH STATEMENT EXECUTE FUNCTION hmem_session_reject_truncate();

GRANT SELECT ON sessions,workspaces,session_occurrences,session_archive_sanitized_contents TO hmem_archive_maintenance_owner;
-- Row locks require UPDATE on one column. The fixed-path capability also
-- versions the occurrence row with an unchanged session_id at first deletion.
GRANT UPDATE(id) ON sessions TO hmem_archive_maintenance_owner;
GRANT UPDATE(session_id) ON session_occurrences TO hmem_archive_maintenance_owner;

CREATE OR REPLACE FUNCTION hmem_archive_reclaim(workspace UUID, session UUID, generation UUID, manifest TEXT, token UUID) RETURNS BIGINT
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog AS $$
DECLARE c public.session_archive_control%ROWTYPE; j public.session_archive_jobs%ROWTYPE;
  g public.session_archive_generations%ROWTYPE; v public.session_archive_reclamation%ROWTYPE;
  r public.session_chunks%ROWTYPE; l public.session_archive_ledger%ROWTYPE;
  s public.sessions%ROWTYPE; o public.session_occurrences%ROWTYPE;
  sanitized BOOLEAN;
  count BIGINT:=0; bytes BIGINT:=0; last_revision BIGINT;
BEGIN
  SELECT * INTO STRICT c FROM public.session_archive_control WHERE singleton FOR UPDATE;
  SELECT * INTO j FROM public.session_archive_jobs WHERE session_id=session AND workspace_id=workspace FOR UPDATE;
  SELECT * INTO g FROM public.session_archive_generations WHERE id=generation AND session_id=session AND workspace_id=workspace FOR UPDATE;
  SELECT * INTO v FROM public.session_archive_reclamation WHERE generation_id=generation AND session_id=session AND workspace_id=workspace FOR UPDATE;
  sanitized:=j.replacement_snapshot IS NOT NULL;
  IF NOT public.hmem_archive_runtime_safe() OR j.state IS DISTINCT FROM 'ready'
    OR c.permit_kind IS DISTINCT FROM 'maintenance' OR (c.permit_generation,c.permit_token) IS DISTINCT FROM (generation,token)
    OR c.permit_expires_at<=clock_timestamp()
    OR j.generation_id IS DISTINCT FROM generation OR g.published_at IS NULL OR v.verified_at IS NULL
    OR (NOT sanitized AND v.error_code IS NOT NULL)
    OR g.metadata_sha256 IS DISTINCT FROM manifest OR v.metadata_sha256 IS DISTINCT FROM manifest THEN
    RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Raw reclamation requires enabled verified same-workspace publication';
  END IF;
  -- A control withdrawal after successful readback defers this bounded batch;
  -- it does not invalidate the immutable generation's verification attestation.
  IF c.paused THEN RETURN 0; END IF;
  IF sanitized THEN
    IF NOT c.sanitized_qualified OR NOT c.sanitized_reclaim_enabled OR v.retry_at>clock_timestamp() THEN RETURN 0; END IF;
    IF NOT public.hmem_archive_sanitized_runtime_safe()
      OR v.sanitized_attestation_sha256 IS DISTINCT FROM
        public.hmem_archive_sanitized_attestation(g,j.replacement_policy_sha256)
      OR NOT EXISTS(SELECT 1 FROM public.session_archive_sanitized_contents content
        WHERE (content.generation_id,content.session_id,content.workspace_id,content.policy_sha256)
          =(generation,session,workspace,j.replacement_policy_sha256)) THEN
      RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Sanitized reclamation requires its verified policy and publication';
    END IF;
    IF v.deleted_bytes=0 THEN
      SELECT canonical.* INTO s FROM public.sessions canonical
        JOIN public.workspaces w ON w.id=canonical.workspace_id
        WHERE canonical.id=session AND canonical.workspace_id=workspace AND w.deleted_at IS NULL
        FOR SHARE OF canonical;
      IF s.state IS DISTINCT FROM 'terminated'
        OR (s.final_bytes,s.committed_bytes,s.committed_records,s.revision,s.final_sha256)
          IS DISTINCT FROM (g.raw_bytes,g.raw_bytes,g.raw_records,g.raw_revision,g.raw_sha256) THEN
        RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Canonical sealed source changed before reclamation';
      END IF;
      SELECT * INTO o FROM public.session_occurrences
        WHERE session_id=session AND workspace_id=workspace FOR UPDATE;
      IF (o.derivation_version,o.processed_bytes,octet_length(o.pending))
        IS DISTINCT FROM (1,g.raw_bytes,0) THEN RETURN 0; END IF;
      IF g.raw_bytes>0 THEN
        -- Version the row before first deletion. Stale repeatable-read
        -- writers cannot replace the retained projection using old progress.
        UPDATE public.session_occurrences SET session_id=o.session_id
          WHERE session_id=session AND workspace_id=workspace;
      END IF;
    END IF;
  ELSIF NOT c.qualified OR NOT c.reclaim_enabled THEN
    RETURN 0;
  END IF;
  last_revision:=v.deleted_revision;
  FOR r IN SELECT * FROM public.session_chunks WHERE session_id=session ORDER BY revision LIMIT 8 FOR UPDATE LOOP
    SELECT * INTO l FROM public.session_archive_ledger WHERE generation_id=generation AND revision=r.revision;
    IF l.revision IS NULL OR r.revision<>last_revision+1 OR r.start_offset<>v.deleted_bytes+bytes
      OR (r.start_offset,r.start_offset+octet_length(r.data),r.allow_final_record,r.record_count,r.ends_with_newline,encode(sha256(r.data),'hex'))
       IS DISTINCT FROM (l.start_offset,l.end_offset,l.allow_final_record,l.record_count,l.ends_with_newline,l.raw_sha256)
      OR l.binding_sha256 IS DISTINCT FROM public.hmem_archive_binding(coalesce((SELECT p.binding_sha256 FROM public.session_archive_ledger p WHERE p.generation_id=generation AND p.revision=l.revision-1),''),jsonb_build_array(generation,l.revision,l.start_offset,l.end_offset,l.allow_final_record,l.record_count,l.ends_with_newline,l.raw_sha256)) THEN
      RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Raw row differs from verified append ledger';
    END IF;
    IF bytes+octet_length(r.data)>8388608 THEN EXIT; END IF;
    DELETE FROM public.session_chunks WHERE session_id=session AND revision=r.revision;
    IF NOT FOUND THEN
      RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Verified raw row was not deleted';
    END IF;
    count:=count+1; bytes:=bytes+octet_length(r.data); last_revision:=r.revision;
  END LOOP;
  IF count=0 AND (last_revision<>g.ledger_count OR v.deleted_bytes<>g.raw_bytes) THEN
    RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Raw reclamation progress is incomplete';
  END IF;
  UPDATE public.session_archive_reclamation SET deleted_revision=last_revision,deleted_bytes=deleted_bytes+bytes,
    complete=(last_revision=g.ledger_count AND deleted_bytes+bytes=g.raw_bytes),error_code=NULL WHERE generation_id=generation;
  RETURN count;
END $$;

REVOKE ALL ON FUNCTION public.hmem_archive_reclaim(UUID,UUID,UUID,TEXT,UUID) FROM PUBLIC;

INSERT INTO schema_migrations(version,name) VALUES(63,'V063__sanitized_archive_reclamation.sql');
COMMIT;
