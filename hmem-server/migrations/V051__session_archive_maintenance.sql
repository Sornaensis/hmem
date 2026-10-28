BEGIN;

-- Provisioning must run as an administrator. The runtime role must never be a
-- member of this capability owner, including through another role.
DO $$ BEGIN
  IF NOT (SELECT rolsuper FROM pg_catalog.pg_roles WHERE rolname=current_user) THEN
    RAISE EXCEPTION 'V051 requires a PostgreSQL administrator to provision the isolated maintenance owner';
  END IF;
  IF NOT EXISTS (SELECT 1 FROM pg_catalog.pg_roles WHERE rolname='hmem_archive_maintenance_owner') THEN
    CREATE ROLE hmem_archive_maintenance_owner NOLOGIN NOINHERIT NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION NOBYPASSRLS;
  END IF;
  IF EXISTS (SELECT 1 FROM pg_catalog.pg_roles WHERE rolname='hmem_archive_maintenance_owner'
    AND (rolcanlogin OR rolinherit OR rolsuper OR rolcreatedb OR rolcreaterole OR rolreplication OR rolbypassrls))
    OR EXISTS (SELECT 1 FROM pg_catalog.pg_auth_members m JOIN pg_catalog.pg_roles r ON r.oid=m.roleid OR r.oid=m.member WHERE r.rolname='hmem_archive_maintenance_owner') THEN
    RAISE EXCEPTION 'Unsafe archive maintenance owner attributes or membership';
  END IF;
END $$;

-- pg_restore loads session CHECK constraints under an empty search_path. Keep
-- V046's identity grammar and inlining, qualifying only its helper lookups.
CREATE OR REPLACE FUNCTION public.hmem_session_runtime_identity_valid(value JSONB) RETURNS BOOLEAN LANGUAGE SQL IMMUTABLE AS $$
  SELECT CASE WHEN value IS NULL THEN true WHEN jsonb_typeof(value)<>'object' THEN false ELSE coalesce(
    (value-ARRAY['model','provider','reasoning','role'])='{}'::jsonb
    AND jsonb_typeof(value->'model')='string' AND public.hmem_session_runtime_text_valid(value->>'model',256)
    AND (NOT value ? 'provider' OR (jsonb_typeof(value->'provider')='string' AND public.hmem_session_runtime_text_valid(value->>'provider',128)))
    AND (NOT value ? 'reasoning' OR (jsonb_typeof(value->'reasoning')='string' AND public.hmem_session_runtime_text_valid(value->>'reasoning',128)))
    AND (NOT value ? 'role' OR (jsonb_typeof(value->'role')='string' AND public.hmem_session_runtime_text_valid(value->>'role',128))),false) END;
$$;

CREATE TABLE session_archive_control (
  singleton BOOLEAN PRIMARY KEY DEFAULT TRUE CHECK(singleton),
  cutover_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  paused BOOLEAN NOT NULL DEFAULT FALSE,
  backfill_enabled BOOLEAN NOT NULL DEFAULT FALSE,
  reclaim_enabled BOOLEAN NOT NULL DEFAULT FALSE,
  qualified BOOLEAN NOT NULL DEFAULT FALSE,
  cursor_time TIMESTAMPTZ,
  cursor_id UUID,
  staging_budget BIGINT NOT NULL DEFAULT 8589934592 CHECK(staging_budget>=0),
  wal_budget BIGINT NOT NULL DEFAULT 34359738368 CHECK(wal_budget>=0),
  staged_bytes BIGINT NOT NULL DEFAULT 0 CHECK(staged_bytes>=0),
  reserved_bytes BIGINT NOT NULL DEFAULT 0 CHECK(reserved_bytes>=0),
  permit_generation UUID,
  permit_token UUID,
  permit_kind TEXT CHECK(permit_kind IN ('compression','maintenance')),
  permit_expires_at TIMESTAMPTZ,
  blocked_reason TEXT,
  CHECK((cursor_time IS NULL)=(cursor_id IS NULL)),
  CHECK((permit_generation IS NULL)=(permit_token IS NULL)),
  CHECK((permit_generation IS NULL)=(permit_kind IS NULL)),
  CHECK((permit_generation IS NULL)=(permit_expires_at IS NULL)),
  CHECK(NOT reclaim_enabled OR qualified),
  CHECK(NOT backfill_enabled OR qualified)
);
INSERT INTO session_archive_control(singleton,staged_bytes)
SELECT TRUE,coalesce((SELECT sum(octet_length(p.data)+512) FROM session_archive_parts p JOIN session_archive_generations g ON g.id=p.generation_id WHERE g.published_at IS NULL),0)
 +coalesce((SELECT sum(octet_length(c.dictionary)+512) FROM session_archive_checkpoints c JOIN session_archive_generations g ON g.id=c.generation_id WHERE g.published_at IS NULL),0)
 +coalesce((SELECT count(*)*1024 FROM session_archive_ledger l JOIN session_archive_generations g ON g.id=l.generation_id WHERE g.published_at IS NULL),0);

CREATE TABLE session_archive_requests (
  session_id UUID PRIMARY KEY,
  workspace_id UUID NOT NULL,
  requested_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  FOREIGN KEY(session_id,workspace_id) REFERENCES sessions(id,workspace_id)
);

CREATE TABLE session_archive_reclamation (
  generation_id UUID PRIMARY KEY,
  session_id UUID NOT NULL,
  workspace_id UUID NOT NULL,
  metadata_sha256 TEXT NOT NULL,
  verified_at TIMESTAMPTZ,
  retry_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  deleted_revision BIGINT NOT NULL DEFAULT 0 CHECK(deleted_revision>=0),
  deleted_bytes BIGINT NOT NULL DEFAULT 0 CHECK(deleted_bytes>=0),
  complete BOOLEAN NOT NULL DEFAULT FALSE,
  error_code TEXT CHECK(error_code ~ '^[a-z][a-z0-9_]{0,63}$'),
  FOREIGN KEY(generation_id,session_id,workspace_id) REFERENCES session_archive_generations(id,session_id,workspace_id)
);

-- Capacity reservations are conservative logical allowances, not observations
-- of tablespace free space or a promise about PostgreSQL's physical WAL volume.
CREATE FUNCTION hmem_archive_reservation(raw BIGINT, revisions BIGINT) RETURNS NUMERIC
LANGUAGE SQL IMMUTABLE STRICT SET search_path=pg_catalog AS $$
 SELECT raw::numeric+ceil(raw::numeric/500)+greatest(1,ceil(raw::numeric/1048576))*65536+revisions::numeric*1024+8388608
$$;

-- Called after the application takes control before job. The trigger fences
-- accidental direct claims too; callers must retain the documented lock order.
CREATE FUNCTION hmem_archive_permit_guard() RETURNS TRIGGER LANGUAGE plpgsql AS $$
DECLARE c session_archive_control%ROWTYPE; s sessions%ROWTYPE; required NUMERIC;
BEGIN
  SELECT * INTO STRICT c FROM session_archive_control WHERE singleton FOR UPDATE;
  IF NEW.state='running' THEN
    IF NEW.attempt<>OLD.attempt THEN
      SELECT * INTO STRICT s FROM sessions WHERE id=NEW.session_id;
      required:=hmem_archive_reservation(s.final_bytes,s.revision-1);
      IF c.paused OR (NOT c.backfill_enabled AND s.terminated_at<c.cutover_at
          AND NOT EXISTS(SELECT 1 FROM session_archive_requests WHERE session_id=NEW.session_id AND workspace_id=NEW.workspace_id))
        OR (c.permit_expires_at>clock_timestamp())
        OR EXISTS(SELECT 1 FROM session_archive_generations g WHERE g.published_at IS NULL)
        OR required+c.staged_bytes>c.staging_budget OR required*4>c.wal_budget THEN
        RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Archive claim lacks global admission';
      END IF;
      UPDATE session_archive_control SET permit_generation=NEW.generation_id,permit_token=NEW.lease_token,
        permit_kind='compression',permit_expires_at=NEW.lease_expires_at,reserved_bytes=required::bigint,blocked_reason=NULL WHERE singleton;
    ELSE
      IF c.permit_kind IS DISTINCT FROM 'compression' OR (c.permit_generation,c.permit_token) IS DISTINCT FROM (NEW.generation_id,NEW.lease_token)
        OR c.permit_expires_at<=clock_timestamp() THEN
        RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Archive global permit expired';
      END IF;
      UPDATE session_archive_control SET permit_expires_at=NEW.lease_expires_at WHERE singleton;
    END IF;
  ELSIF c.permit_generation=OLD.generation_id AND c.permit_token=OLD.lease_token THEN
    UPDATE session_archive_control SET permit_generation=NULL,permit_token=NULL,permit_kind=NULL,permit_expires_at=NULL,reserved_bytes=0 WHERE singleton;
  END IF;
  RETURN NEW;
END $$;
CREATE TRIGGER session_archive_0_permit BEFORE UPDATE ON session_archive_jobs FOR EACH ROW EXECUTE FUNCTION hmem_archive_permit_guard();

CREATE FUNCTION hmem_archive_stage_account() RETURNS TRIGGER LANGUAGE plpgsql AS $$
DECLARE amount BIGINT; c session_archive_control%ROWTYPE;
BEGIN
  SELECT * INTO STRICT c FROM session_archive_control WHERE singleton FOR UPDATE;
  IF TG_TABLE_NAME='session_archive_parts' THEN amount:=octet_length(coalesce(NEW.data,OLD.data))+512;
  ELSIF TG_TABLE_NAME='session_archive_checkpoints' THEN amount:=octet_length(coalesce(NEW.dictionary,OLD.dictionary))+512;
  ELSE amount:=1024; END IF;
  IF TG_OP='INSERT' THEN
    IF c.staged_bytes+amount>c.staging_budget THEN RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Archive staging capacity exhausted'; END IF;
    UPDATE session_archive_control SET staged_bytes=staged_bytes+amount WHERE singleton;
  ELSE UPDATE session_archive_control SET staged_bytes=greatest(0,staged_bytes-amount) WHERE singleton;
  END IF;
  RETURN coalesce(NEW,OLD);
END $$;
CREATE TRIGGER session_archive_z_account AFTER INSERT OR DELETE ON session_archive_parts FOR EACH ROW EXECUTE FUNCTION hmem_archive_stage_account();
CREATE TRIGGER session_archive_z_account AFTER INSERT OR DELETE ON session_archive_checkpoints FOR EACH ROW EXECUTE FUNCTION hmem_archive_stage_account();
CREATE TRIGGER session_archive_z_account AFTER INSERT OR DELETE ON session_archive_ledger FOR EACH ROW EXECUTE FUNCTION hmem_archive_stage_account();

-- Admission drains all detached generations first, so only this one permit can
-- contribute staging when it publishes. Published payloads are no longer staging.
CREATE FUNCTION hmem_archive_generation_account() RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  IF OLD.published_at IS NULL AND NEW.published_at IS NOT NULL THEN
    UPDATE session_archive_control SET staged_bytes=0 WHERE singleton;
  END IF;
  RETURN NEW;
END $$;
CREATE TRIGGER session_archive_z_account AFTER UPDATE ON session_archive_generations FOR EACH ROW EXECUTE FUNCTION hmem_archive_generation_account();

-- Keep the existing append validator intact. Only the unassumable function
-- owner can reach DELETE; it has no general-purpose SQL execution capability.
ALTER FUNCTION hmem_session_chunk_guard() RENAME TO hmem_session_chunk_append_guard;
CREATE FUNCTION hmem_session_chunk_guard() RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP='DELETE' AND current_user='hmem_archive_maintenance_owner' THEN RETURN OLD; END IF;
  RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Session chunks are append-only';
END $$;
DROP TRIGGER session_chunk_guard ON session_chunks;
CREATE TRIGGER session_chunk_insert_guard BEFORE INSERT ON session_chunks FOR EACH ROW EXECUTE FUNCTION hmem_session_chunk_append_guard();
CREATE TRIGGER session_chunk_guard BEFORE UPDATE OR DELETE ON session_chunks FOR EACH ROW EXECUTE FUNCTION hmem_session_chunk_guard();

CREATE FUNCTION hmem_archive_reclaim(workspace UUID, session UUID, generation UUID, manifest TEXT, token UUID) RETURNS BIGINT
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog AS $$
DECLARE c public.session_archive_control%ROWTYPE; j public.session_archive_jobs%ROWTYPE;
  g public.session_archive_generations%ROWTYPE; v public.session_archive_reclamation%ROWTYPE;
  r public.session_chunks%ROWTYPE; l public.session_archive_ledger%ROWTYPE;
  count BIGINT:=0; bytes BIGINT:=0; last_revision BIGINT;
BEGIN
  SELECT * INTO STRICT c FROM public.session_archive_control WHERE singleton FOR UPDATE;
  SELECT * INTO j FROM public.session_archive_jobs WHERE session_id=session AND workspace_id=workspace FOR UPDATE;
  SELECT * INTO g FROM public.session_archive_generations WHERE id=generation AND session_id=session AND workspace_id=workspace FOR UPDATE;
  SELECT * INTO v FROM public.session_archive_reclamation WHERE generation_id=generation AND session_id=session AND workspace_id=workspace FOR UPDATE;
  IF NOT public.hmem_archive_runtime_safe() OR j.state IS DISTINCT FROM 'ready'
    OR c.permit_kind IS DISTINCT FROM 'maintenance' OR (c.permit_generation,c.permit_token) IS DISTINCT FROM (generation,token)
    OR c.permit_expires_at<=clock_timestamp()
    OR j.generation_id IS DISTINCT FROM generation OR g.published_at IS NULL OR v.verified_at IS NULL OR v.error_code IS NOT NULL
    OR g.metadata_sha256 IS DISTINCT FROM manifest OR v.metadata_sha256 IS DISTINCT FROM manifest THEN
    RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Raw reclamation requires enabled verified same-workspace publication';
  END IF;
  -- A control withdrawal after successful readback defers this bounded batch;
  -- it does not invalidate the immutable generation's verification attestation.
  IF c.paused OR NOT c.qualified OR NOT c.reclaim_enabled THEN RETURN 0; END IF;
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
    count:=count+1; bytes:=bytes+octet_length(r.data); last_revision:=r.revision;
  END LOOP;
  IF count=0 AND (last_revision<>g.ledger_count OR v.deleted_bytes<>g.raw_bytes) THEN
    RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Raw reclamation progress is incomplete';
  END IF;
  UPDATE public.session_archive_reclamation SET deleted_revision=last_revision,deleted_bytes=deleted_bytes+bytes,
    complete=(last_revision=g.ledger_count AND deleted_bytes+bytes=g.raw_bytes),error_code=NULL WHERE generation_id=generation;
  RETURN count;
END $$;
REVOKE ALL ON FUNCTION hmem_archive_reclaim(UUID,UUID,UUID,TEXT,UUID) FROM PUBLIC;
GRANT USAGE ON SCHEMA public TO hmem_archive_maintenance_owner;
GRANT SELECT,UPDATE ON session_archive_control,session_archive_jobs,session_archive_generations,session_archive_reclamation TO hmem_archive_maintenance_owner;
GRANT SELECT ON session_archive_ledger TO hmem_archive_maintenance_owner;
GRANT SELECT,DELETE ON session_chunks TO hmem_archive_maintenance_owner;
-- PostgreSQL row locking requires UPDATE privilege on at least one column;
-- the ordinary UPDATE trigger still rejects every UPDATE, including this owner.
GRANT UPDATE(session_id) ON session_chunks TO hmem_archive_maintenance_owner;
GRANT EXECUTE ON FUNCTION hmem_archive_binding(TEXT,JSONB) TO hmem_archive_maintenance_owner;
ALTER FUNCTION hmem_archive_reclaim(UUID,UUID,UUID,TEXT,UUID) OWNER TO hmem_archive_maintenance_owner;
-- Deployment grants EXECUTE only to the configured trusted server/ctl role.
-- The migrating administrator can call it without a PUBLIC grant.

CREATE FUNCTION hmem_archive_runtime_safe() RETURNS BOOLEAN LANGUAGE SQL STABLE SET search_path=pg_catalog AS $$
 SELECT NOT r.rolsuper AND NOT r.rolcreaterole
   AND NOT pg_has_role(session_user,'hmem_archive_maintenance_owner','MEMBER')
   AND NOT EXISTS(SELECT 1 FROM pg_roles a WHERE pg_has_role(session_user,a.oid,'MEMBER')
     AND has_schema_privilege(a.oid,'public','CREATE'))
   AND NOT EXISTS(SELECT 1 FROM pg_class t JOIN pg_namespace n ON n.oid=t.relnamespace
     WHERE n.nspname='public' AND t.relname IN ('sessions','session_chunks','session_archive_control','session_archive_jobs','session_archive_generations','session_archive_parts','session_archive_checkpoints','session_archive_ledger','session_archive_reclamation','session_archive_requests')
       AND pg_has_role(session_user,t.relowner,'MEMBER'))
   AND NOT EXISTS(SELECT 1 FROM pg_proc p JOIN pg_namespace n ON n.oid=p.pronamespace
     WHERE n.nspname='public' AND (p.proname LIKE 'hmem_archive_%' OR p.proname LIKE 'hmem_session_chunk_%')
       AND pg_has_role(session_user,p.proowner,'MEMBER'))
   -- Invoker triggers run inside the definer's authority. Reject both rights
   -- to attach one (including assumable NOINHERIT roles) and already attached
   -- callbacks controlled by that caller, even after TRIGGER is revoked.
   AND NOT EXISTS(SELECT 1 FROM pg_class t JOIN pg_namespace n ON n.oid=t.relnamespace
     CROSS JOIN pg_roles a WHERE n.nspname='public'
       AND t.relname IN ('session_chunks','session_archive_control','session_archive_jobs','session_archive_generations','session_archive_reclamation')
       AND pg_has_role(session_user,a.oid,'MEMBER') AND has_table_privilege(a.oid,t.oid,'TRIGGER'))
   AND NOT EXISTS(SELECT 1 FROM pg_trigger hook JOIN pg_class t ON t.oid=hook.tgrelid
     JOIN pg_namespace n ON n.oid=t.relnamespace JOIN pg_proc callback ON callback.oid=hook.tgfoid
     WHERE n.nspname='public'
       AND t.relname IN ('session_chunks','session_archive_control','session_archive_jobs','session_archive_generations','session_archive_reclamation')
       AND pg_has_role(session_user,callback.proowner,'MEMBER'))
   AND has_function_privilege(session_user,'public.hmem_archive_reclaim(uuid,uuid,uuid,text,uuid)','EXECUTE')
   AND EXISTS(SELECT 1 FROM pg_roles o WHERE o.rolname='hmem_archive_maintenance_owner'
     AND NOT o.rolcanlogin AND NOT o.rolinherit AND NOT o.rolsuper AND NOT o.rolcreatedb AND NOT o.rolcreaterole AND NOT o.rolreplication AND NOT o.rolbypassrls)
   AND NOT EXISTS(SELECT 1 FROM pg_auth_members m JOIN pg_roles o ON o.oid=m.roleid OR o.oid=m.member WHERE o.rolname='hmem_archive_maintenance_owner')
 FROM pg_roles r WHERE r.rolname=session_user
$$;
CREATE FUNCTION hmem_archive_control_guard() RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.reclaim_enabled AND NOT OLD.reclaim_enabled AND NOT public.hmem_archive_runtime_safe() THEN
    RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='Reclamation requires a restricted runtime caller with an explicit capability grant';
  END IF;
  RETURN NEW;
END $$;
CREATE TRIGGER session_archive_control_guard BEFORE UPDATE ON session_archive_control FOR EACH ROW EXECUTE FUNCTION hmem_archive_control_guard();

INSERT INTO schema_migrations(version,name) VALUES(51,'V051__session_archive_maintenance.sql');
COMMIT;
