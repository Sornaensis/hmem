BEGIN;

ALTER TABLE public.audit_maintenance_control
  ADD COLUMN operation JSONB CHECK(operation IS NULL OR jsonb_typeof(operation)='object' AND octet_length(operation::text)<=32768),
  ADD COLUMN index_cooldowns JSONB NOT NULL DEFAULT '{}' CHECK(jsonb_typeof(index_cooldowns)='object' AND octet_length(index_cooldowns::text)<=8192);

CREATE FUNCTION public.hmem_audit_operation_catalog() RETURNS JSONB
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog AS $$
DECLARE result JSONB;
BEGIN
  PERFORM public.hmem_audit_require_owner();
  SELECT jsonb_agg(jsonb_build_object('oid',r.relation_oid::bigint,'kind',r.kind,'name',r.relation_name,
    'bytes',r.allocated,'filenode',pg_relation_filenode(r.relation_oid)::bigint,
    'path',pg_relation_filepath(r.relation_oid),'valid',r.valid,
    'parent_oid',i.indrelid::bigint,'ready',i.indisready,'live',i.indislive,
    'definition',CASE WHEN i.indexrelid IS NOT NULL THEN pg_get_indexdef(i.indexrelid) END,
    'constraint_oids',(SELECT coalesce(jsonb_agg(c.oid::bigint ORDER BY c.oid),'[]'::jsonb)
      FROM (SELECT oid FROM pg_constraint WHERE conindid=r.relation_oid ORDER BY oid LIMIT 65) c)) ORDER BY r.relation_oid)
    INTO result FROM public.hmem_audit_relations() r
    LEFT JOIN pg_index i ON i.indexrelid=r.relation_oid;
  IF jsonb_array_length(result)>64 OR octet_length(result::text)>12288
    OR EXISTS(SELECT 1 FROM jsonb_array_elements(result) entry WHERE jsonb_array_length(entry->'constraint_oids')>64) THEN
    RAISE EXCEPTION USING ERRCODE='54000',MESSAGE='audit_maintenance_catalog_limit'; END IF;
  RETURN result;
END $$;

CREATE FUNCTION public.hmem_audit_operation_begin(p_id UUID,p_kind TEXT,p_target OID,p_scheduled BOOLEAN) RETURNS JSONB
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog AS $$
DECLARE previous JSONB; catalog JSONB; selected JSONB; result JSONB;
BEGIN
  PERFORM public.hmem_audit_require_owner();
  SELECT operation INTO previous FROM public.audit_maintenance_control WHERE singleton;
  IF previous->>'state' IN('prepared','running','uncertain') THEN
    RAISE EXCEPTION USING ERRCODE='55000',MESSAGE='audit_maintenance_reconciliation_required'; END IF;
  IF (SELECT paused FROM public.audit_maintenance_control WHERE singleton) OR pg_is_in_recovery() THEN
    RAISE EXCEPTION USING ERRCODE='55000',MESSAGE='audit_maintenance_admission_withdrawn'; END IF;
  catalog:=public.hmem_audit_operation_catalog();
  SELECT entry INTO selected FROM jsonb_array_elements(catalog) entry WHERE (entry->>'oid')::oid=p_target;
  IF p_id IS NULL OR p_kind IS NULL OR selected IS NULL OR NOT(p_kind='heap' AND p_target='public.audit_log'::regclass
    OR p_kind='index' AND selected->>'kind'='index' AND (selected->>'parent_oid')::oid='public.audit_log'::regclass
      AND (selected->>'valid')::boolean AND (selected->>'ready')::boolean AND (selected->>'live')::boolean) THEN
    RAISE EXCEPTION USING ERRCODE='22023',MESSAGE='audit_maintenance_invalid_target'; END IF;
  result:=jsonb_build_object('id',p_id,'kind',p_kind,'state','prepared','scheduled',p_scheduled,
    'target_oid',p_target::bigint,'target_name',selected->>'name','before',catalog,
    'backend_pid',pg_backend_pid(),'backend_start_us',(SELECT floor(extract(epoch FROM backend_start)*1000000)::bigint FROM pg_stat_activity WHERE pid=pg_backend_pid()),
    'postmaster_start',pg_postmaster_start_time(),'database_oid',(SELECT oid::bigint FROM pg_database WHERE datname=current_database()),
    'system_identifier',(SELECT system_identifier::text FROM pg_control_system()),'started_at',clock_timestamp(),
    'wal_lsn',pg_current_wal_lsn()::text);
  UPDATE public.audit_maintenance_control SET operation=result WHERE singleton;
  RETURN result;
END $$;

CREATE FUNCTION public.hmem_audit_operation_running(p_id UUID) RETURNS VOID
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog AS $$
BEGIN
  PERFORM public.hmem_audit_require_owner();
  UPDATE public.audit_maintenance_control SET operation=operation||jsonb_build_object('state','running','dispatch_after',clock_timestamp())
    WHERE singleton AND operation->>'id'=p_id::text AND operation->>'state'='prepared'
      AND (operation->>'backend_pid')::int=pg_backend_pid();
  IF NOT FOUND THEN RAISE EXCEPTION USING ERRCODE='55000',MESSAGE='audit_maintenance_operation_changed'; END IF;
END $$;

-- This observer never opens audit heap/index/TOAST relations. It remains usable
-- while VACUUM FULL holds AccessExclusiveLock. Every result has a short caller
-- statement budget; native capacity is sampled separately by its retained owner.
CREATE FUNCTION public.hmem_audit_operation_monitor(p_id UUID) RETURNS JSONB
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog AS $$
DECLARE op JSONB; is_paused BOOLEAN; activity RECORD; slot_bytes NUMERIC; slots INT; archive_bad BOOLEAN;
DECLARE archive_files BIGINT; archive_bytes BIGINT;
BEGIN
  IF NOT public.hmem_audit_runtime_safe() THEN
    RAISE EXCEPTION USING ERRCODE='42501',MESSAGE='audit_maintenance_requires_narrow_role'; END IF;
  SELECT operation,paused INTO op,is_paused FROM public.audit_maintenance_control WHERE singleton;
  IF op->>'id' IS DISTINCT FROM p_id::text THEN RETURN jsonb_build_object('reason','operation_changed'); END IF;
  IF op->>'system_identifier' IS DISTINCT FROM (SELECT system_identifier::text FROM pg_control_system())
    OR (op->>'database_oid')::oid IS DISTINCT FROM (SELECT oid FROM pg_database WHERE datname=current_database()) THEN
    RETURN jsonb_build_object('reason','operation_identity_changed'); END IF;
  SELECT state,wait_event_type,query_start INTO activity FROM pg_stat_activity WHERE pid=(op->>'backend_pid')::int
    AND floor(extract(epoch FROM backend_start)*1000000)::bigint=(op->>'backend_start_us')::bigint
    AND datid=(op->>'database_oid')::oid AND usesysid=(SELECT oid FROM pg_roles WHERE rolname=session_user);
  SELECT count(*),coalesce(sum(pg_wal_lsn_diff(pg_current_wal_lsn(),restart_lsn)),0)
    INTO slots,slot_bytes FROM pg_replication_slots WHERE restart_lsn IS NOT NULL;
  SELECT coalesce(last_failed_time>last_archived_time OR last_failed_time IS NOT NULL AND last_archived_time IS NULL,FALSE)
    INTO archive_bad FROM pg_stat_archiver;
  SELECT count(*),count(*)*pg_size_bytes(current_setting('wal_segment_size')) INTO archive_files,archive_bytes
    FROM pg_ls_archive_statusdir() WHERE name LIKE '%.ready';
  RETURN jsonb_build_object('reason',CASE WHEN archive_files>4096 THEN 'archive_measurement_limit' ELSE 'ok' END,'id',p_id,'paused',is_paused,'at',clock_timestamp(),
    'state',op->>'state','postmaster_start',pg_postmaster_start_time(),
    'backend_present',activity.state IS NOT NULL,'backend_state',activity.state,'wait_event_type',activity.wait_event_type,
    'command_started',activity.state='active' AND activity.query_start>=(op->>'dispatch_after')::timestamptz,
    'wal_growth_bytes',pg_wal_lsn_diff(pg_current_wal_lsn(),(op->>'wal_lsn')::pg_lsn),
    'slot_count',slots,'slot_retained_bytes',slot_bytes,'archive_failed',archive_bad,'archive_pending_bytes',archive_bytes,
    'foreground_waiters',(SELECT count(*) FROM pg_locks WHERE NOT granted AND locktype='relation'
      AND relation=(SELECT (entry->>'oid')::oid FROM jsonb_array_elements(op->'before') entry WHERE entry->>'kind'='heap' LIMIT 1)
      AND pid<>(op->>'backend_pid')::int));
END $$;

CREATE FUNCTION public.hmem_audit_operation_uncertain(p_id UUID) RETURNS VOID
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog AS $$
BEGIN
  IF NOT public.hmem_audit_runtime_safe() THEN
    RAISE EXCEPTION USING ERRCODE='42501',MESSAGE='audit_maintenance_requires_narrow_role'; END IF;
  UPDATE public.audit_maintenance_control SET operation=operation||jsonb_build_object('state','uncertain')
    WHERE singleton AND operation->>'id'=p_id::text AND operation->>'state' IN('prepared','running');
END $$;

CREATE FUNCTION public.hmem_audit_operation_finish(p_id UUID,p_state TEXT,p_outcome JSONB,p_after JSONB) RETURNS VOID
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog AS $$
DECLARE op JSONB; cooldowns JSONB; completed TIMESTAMPTZ:=clock_timestamp();
BEGIN
  PERFORM public.hmem_audit_require_owner();
  SELECT operation,index_cooldowns INTO op,cooldowns FROM public.audit_maintenance_control WHERE singleton;
  IF p_id IS NULL OR op->>'id' IS DISTINCT FROM p_id::text OR p_state IS NULL OR p_state NOT IN('succeeded','cancelled','failed','reconciled','deferred','partial')
    OR p_outcome IS NULL OR jsonb_typeof(p_outcome)<>'object' OR octet_length(p_outcome::text)>32768
    OR p_after IS NULL OR jsonb_typeof(p_after)<>'array' OR octet_length(p_after::text)>12288 THEN
    RAISE EXCEPTION USING ERRCODE='22023',MESSAGE='audit_maintenance_invalid_outcome'; END IF;
  IF op->>'state' NOT IN('prepared','running','uncertain') THEN RETURN; END IF;
  IF p_state IN('succeeded','reconciled') OR p_state='partial' AND op->>'kind'='heap' AND p_outcome->>'rewrite_observed'='true' THEN
    SELECT coalesce(jsonb_object_agg(i.indexrelid::text,
      CASE WHEN op->>'kind'='heap' OR EXISTS(SELECT 1 FROM jsonb_array_elements(p_after) entry
          WHERE (entry->>'oid')::oid=i.indexrelid AND entry->>'name'=op->>'target_name')
        THEN to_jsonb(completed) ELSE cooldowns->i.indexrelid::text END)
      FILTER(WHERE op->>'kind'='heap' OR cooldowns ? i.indexrelid::text OR EXISTS(SELECT 1 FROM jsonb_array_elements(p_after) entry
          WHERE (entry->>'oid')::oid=i.indexrelid AND entry->>'name'=op->>'target_name')),'{}'::jsonb)
      INTO cooldowns FROM pg_index i WHERE i.indrelid='public.audit_log'::regclass AND i.indisvalid;
  END IF;
  UPDATE public.audit_maintenance_control SET
    operation=op||jsonb_build_object('state',p_state,'finished_at',completed,'after',p_after),
    last_assessment=p_outcome,last_cycle=CASE WHEN (op->>'scheduled')::boolean THEN p_outcome ELSE last_cycle END,
    last_heap_at=CASE WHEN (p_state IN('succeeded','reconciled') OR p_state='partial' AND p_outcome->>'rewrite_observed'='true') AND op->>'kind'='heap' THEN completed ELSE last_heap_at END,
    last_index_at=CASE WHEN p_state IN('succeeded','reconciled') AND op->>'kind'='index' THEN completed ELSE last_index_at END,
    index_cooldowns=cooldowns WHERE singleton;
END $$;

-- Only a trusted administrator/control owner may attest a separately verified
-- backup+restore. The runtime login receives no EXECUTE grant and cannot bless
-- its own heap admission. The hashes are attestations, not server-generated proof.
CREATE FUNCTION public.hmem_audit_backup_attest(p_receipt JSONB) RETURNS VOID
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog AS $$
BEGIN
  IF NOT EXISTS(SELECT 1 FROM pg_class WHERE oid='public.audit_maintenance_control'::regclass
    AND pg_has_role(session_user,relowner,'MEMBER')) THEN
    RAISE EXCEPTION USING ERRCODE='42501',MESSAGE='audit_backup_administrator_required'; END IF;
  IF p_receipt IS NULL OR jsonb_typeof(p_receipt)<>'object' OR octet_length(p_receipt::text)>4096
    OR p_receipt->>'system_identifier' IS DISTINCT FROM (SELECT system_identifier::text FROM pg_control_system())
    OR (p_receipt->>'database_oid')::bigint IS DISTINCT FROM (SELECT oid::bigint FROM pg_database WHERE datname=current_database())
    OR p_receipt->'restore_verified' IS DISTINCT FROM 'true'::jsonb
    OR NOT coalesce(p_receipt->>'backup_sha256' ~ '^[0-9a-f]{64}$',FALSE)
    OR NOT coalesce(p_receipt->>'restore_evidence_sha256' ~ '^[0-9a-f]{64}$',FALSE)
    OR (p_receipt->>'verified_at')::timestamptz IS NULL OR (p_receipt->>'verified_at')::timestamptz>clock_timestamp()
    OR p_receipt->>'recovery_point' IS NULL OR (p_receipt->>'recovery_point')::pg_lsn>pg_current_wal_lsn() THEN
    RAISE EXCEPTION USING ERRCODE='22023',MESSAGE='audit_backup_invalid_receipt'; END IF;
  UPDATE public.audit_maintenance_control SET backup_receipt=p_receipt WHERE singleton;
EXCEPTION WHEN invalid_text_representation OR invalid_datetime_format OR datetime_field_overflow OR numeric_value_out_of_range THEN
  RAISE EXCEPTION USING ERRCODE='22023',MESSAGE='audit_backup_invalid_receipt';
END $$;

CREATE OR REPLACE FUNCTION public.hmem_audit_status() RETURNS JSONB
LANGUAGE SQL SECURITY DEFINER SET search_path=pg_catalog AS $$
  SELECT jsonb_build_object('runtime_safe',public.hmem_audit_runtime_safe(),
    'operation_active',EXISTS(SELECT 1 FROM pg_locks WHERE locktype='advisory' AND classid=717783 AND objid=1 AND objsubid=2 AND granted),
    'server_version',current_setting('server_version_num')::int,'paused',c.paused,
    'last_scheduled_date',c.last_scheduled_date,'last_scheduled_zone',c.last_scheduled_zone,'last_claimed_at',c.last_claimed_at,
    'last_heap_at',c.last_heap_at,'last_index_at',c.last_index_at,'last_assessment',c.last_assessment,
    'last_cycle',c.last_cycle,'backup_receipt',c.backup_receipt,'operation',c.operation,'index_cooldowns',c.index_cooldowns)
  FROM public.audit_maintenance_control c WHERE singleton
$$;

REVOKE ALL ON FUNCTION public.hmem_audit_operation_catalog(),public.hmem_audit_operation_begin(UUID,TEXT,OID,BOOLEAN),
  public.hmem_audit_operation_running(UUID),public.hmem_audit_operation_monitor(UUID),public.hmem_audit_operation_uncertain(UUID),
  public.hmem_audit_operation_finish(UUID,TEXT,JSONB,JSONB),public.hmem_audit_backup_attest(JSONB) FROM PUBLIC;
INSERT INTO schema_migrations(version,name) VALUES(57,'V057__audit_maintenance_operations.sql');
COMMIT;
