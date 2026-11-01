BEGIN;

-- No extension is installed here. Missing native diagnostics are an explicit
-- runtime capability result, so ordinary migration/startup still succeeds.
CREATE TABLE public.audit_maintenance_control (
  singleton BOOLEAN PRIMARY KEY DEFAULT TRUE CHECK(singleton),
  paused BOOLEAN NOT NULL DEFAULT FALSE,
  last_scheduled_date DATE,
  last_scheduled_zone TEXT CHECK(octet_length(last_scheduled_zone)<=128),
  last_claimed_at TIMESTAMPTZ,
  last_heap_at TIMESTAMPTZ,
  last_index_at TIMESTAMPTZ,
  last_assessment JSONB,
  last_cycle JSONB,
  backup_receipt JSONB,
  CHECK(last_assessment IS NULL OR octet_length(last_assessment::text)<=32768),
  CHECK(last_cycle IS NULL OR octet_length(last_cycle::text)<=32768),
  CHECK(backup_receipt IS NULL OR octet_length(backup_receipt::text)<=4096)
);
INSERT INTO public.audit_maintenance_control(singleton) VALUES(TRUE);
REVOKE ALL ON public.audit_maintenance_control FROM PUBLIC;

CREATE FUNCTION public.hmem_audit_runtime_safe() RETURNS BOOLEAN
LANGUAGE plpgsql STABLE SET search_path=pg_catalog AS $$
DECLARE target OID:=to_regclass('public.audit_log');
BEGIN
  IF current_setting('server_version_num')::int<180000 OR current_setting('server_version_num')::int>=190000
    OR target IS NULL THEN RETURN FALSE; END IF;
  RETURN EXISTS(SELECT 1 FROM pg_roles r WHERE r.rolname=session_user
      AND NOT(r.rolsuper OR r.rolcreatedb OR r.rolcreaterole OR r.rolreplication OR r.rolbypassrls))
    AND has_table_privilege(session_user,target,'MAINTAIN')
    AND NOT EXISTS(SELECT 1 FROM pg_roles r WHERE pg_has_role(session_user,r.oid,'MEMBER')
      AND (r.rolsuper OR r.rolcreatedb OR r.rolcreaterole OR r.rolreplication OR r.rolbypassrls))
    AND NOT EXISTS(SELECT 1 FROM pg_roles r WHERE r.rolname IN
      ('pg_maintain','pg_read_server_files','pg_write_server_files','pg_execute_server_program','pg_read_all_data','pg_write_all_data','pg_stat_scan_tables')
      AND pg_has_role(session_user,r.oid,'MEMBER'))
    AND NOT EXISTS(SELECT 1 FROM pg_roles r WHERE pg_has_role(session_user,r.oid,'MEMBER')
      AND (has_schema_privilege(r.oid,'public','CREATE')
        OR EXISTS(SELECT 1 FROM pg_extension x WHERE x.extname='pgstattuple' AND has_schema_privilege(r.oid,x.extnamespace,'CREATE'))))
    AND NOT EXISTS(SELECT 1 FROM pg_class c WHERE c.oid IN(target,'public.audit_maintenance_control'::regclass)
      AND pg_has_role(session_user,c.relowner,'MEMBER'))
    AND NOT EXISTS(SELECT 1 FROM pg_proc p JOIN pg_namespace n ON n.oid=p.pronamespace
      WHERE n.nspname='public' AND p.proname LIKE 'hmem_audit_%' AND pg_has_role(session_user,p.proowner,'MEMBER'))
    AND NOT EXISTS(SELECT 1 FROM pg_roles r WHERE pg_has_role(session_user,r.oid,'MEMBER')
      AND has_table_privilege(r.oid,'public.audit_maintenance_control','TRIGGER'))
    AND NOT EXISTS(SELECT 1 FROM pg_trigger t JOIN pg_proc p ON p.oid=t.tgfoid
      WHERE t.tgrelid='public.audit_maintenance_control'::regclass AND pg_has_role(session_user,p.proowner,'MEMBER'));
END $$;

CREATE FUNCTION public.hmem_audit_require_owner() RETURNS VOID
LANGUAGE plpgsql SET search_path=pg_catalog AS $$
BEGIN
  IF NOT public.hmem_audit_runtime_safe() THEN
    RAISE EXCEPTION USING ERRCODE='42501',MESSAGE='audit_maintenance_requires_narrow_role';
  END IF;
  IF NOT EXISTS(SELECT 1 FROM pg_locks WHERE locktype='advisory' AND pid=pg_backend_pid()
    AND classid=717783 AND objid=1 AND objsubid=2 AND granted AND mode='ExclusiveLock') THEN
    RAISE EXCEPTION USING ERRCODE='55000',MESSAGE='audit_maintenance_singleton_required';
  END IF;
END $$;

CREATE FUNCTION public.hmem_audit_status() RETURNS JSONB
LANGUAGE SQL SECURITY DEFINER SET search_path=pg_catalog AS $$
  SELECT jsonb_build_object('runtime_safe',public.hmem_audit_runtime_safe(),
    'operation_active',EXISTS(SELECT 1 FROM pg_locks WHERE locktype='advisory' AND classid=717783 AND objid=1 AND objsubid=2 AND granted),
    'server_version',current_setting('server_version_num')::int,
    'paused',c.paused,'last_scheduled_date',c.last_scheduled_date,'last_scheduled_zone',c.last_scheduled_zone,
    'last_claimed_at',c.last_claimed_at,'last_heap_at',c.last_heap_at,'last_index_at',c.last_index_at,
    'last_assessment',c.last_assessment,'last_cycle',c.last_cycle,'backup_receipt',c.backup_receipt)
  FROM public.audit_maintenance_control c WHERE singleton
$$;

CREATE FUNCTION public.hmem_audit_pause(p_pause BOOLEAN) RETURNS JSONB
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog AS $$
BEGIN
  IF NOT public.hmem_audit_runtime_safe() THEN
    RAISE EXCEPTION USING ERRCODE='42501',MESSAGE='audit_maintenance_requires_narrow_role'; END IF;
  UPDATE public.audit_maintenance_control SET paused=p_pause WHERE singleton;
  RETURN public.hmem_audit_status();
END $$;

CREATE FUNCTION public.hmem_audit_boundary(p_day DATE,p_time TIME,p_zone TEXT) RETURNS TIMESTAMPTZ
LANGUAGE SQL STABLE SET search_path=pg_catalog AS $$
  SELECT candidate FROM generate_series(((p_day+p_time)-interval '36 hours') AT TIME ZONE 'UTC',
    ((p_day+p_time)+interval '36 hours') AT TIME ZONE 'UTC',interval '1 minute') candidate
  WHERE candidate AT TIME ZONE p_zone>=p_day+p_time
    AND candidate AT TIME ZONE p_zone<p_day+interval '1 day'
  ORDER BY candidate AT TIME ZONE p_zone,candidate LIMIT 1
$$;

CREATE FUNCTION public.hmem_audit_next_eligibility(p_now TIMESTAMPTZ,p_start TIME,p_end TIME,p_zone TEXT,p_first_date DATE)
RETURNS TIMESTAMPTZ LANGUAGE SQL STABLE SET search_path=pg_catalog AS $$
  WITH dates AS (SELECT p_first_date+n AS actual_date FROM generate_series(0,7) n),
  windows AS (SELECT actual_date,actual_date+offset_day AS opening_date FROM dates
    CROSS JOIN LATERAL generate_series(CASE WHEN p_end<p_start THEN -1 ELSE 0 END,0) offset_day),
  bounds AS MATERIALIZED (SELECT actual_date,
    public.hmem_audit_boundary(actual_date,'00:00'::time,p_zone) AS day_start,
    public.hmem_audit_boundary(opening_date,p_start,p_zone) AS opening,
    public.hmem_audit_boundary(opening_date+CASE WHEN p_end<p_start THEN 1 ELSE 0 END,p_end,p_zone) AS closing
    FROM windows),
  candidates AS (SELECT *,greatest(p_now,day_start,opening) AS instant FROM bounds)
  SELECT min(instant) FROM candidates WHERE day_start IS NOT NULL AND opening IS NOT NULL AND closing IS NOT NULL
    AND closing>opening AND instant<closing AND (instant AT TIME ZONE p_zone)::date=actual_date
$$;

CREATE FUNCTION public.hmem_audit_schedule(p_now TIMESTAMPTZ,p_start TEXT,p_end TEXT,p_zone TEXT,p_days INT,p_claim BOOLEAN)
RETURNS JSONB LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog AS $$
DECLARE moment TIMESTAMPTZ:=coalesce(p_now,clock_timestamp());
DECLARE zone TEXT:=CASE WHEN p_zone='local' THEN current_setting('TimeZone') ELSE p_zone END;
DECLARE local_now TIMESTAMP; actual_day DATE; opening_day DATE; opening TIMESTAMPTZ; closing TIMESTAMPTZ;
DECLARE first_date DATE; next_open TIMESTAMPTZ; previous DATE; is_paused BOOLEAN; reason TEXT; claimed BOOLEAN:=FALSE;
BEGIN
  PERFORM public.hmem_audit_require_owner();
  IF p_zone='local' AND NOT EXISTS(SELECT 1 FROM pg_settings WHERE name='TimeZone'
      AND source IN('default','configuration file','command line','environment variable')) THEN
    RETURN jsonb_build_object('eligible',FALSE,'claimed',FALSE,'reason','unproven_server_local_timezone'); END IF;
  IF p_days NOT BETWEEN 1 AND 3650 OR p_start !~ '^([01][0-9]|2[0-3]):[0-5][0-9]$'
    OR p_end !~ '^([01][0-9]|2[0-3]):[0-5][0-9]$' OR p_start=p_end THEN
    RAISE EXCEPTION USING ERRCODE='22023',MESSAGE='audit_maintenance_invalid_schedule'; END IF;
  IF NOT EXISTS(SELECT 1 FROM pg_timezone_names WHERE name=zone) THEN
    RETURN jsonb_build_object('eligible',FALSE,'claimed',FALSE,'reason','unsupported_timezone'); END IF;
  local_now:=moment AT TIME ZONE zone;
  actual_day:=local_now::date;
  opening_day:=actual_day;
  IF p_end<p_start AND local_now::time<p_end::time THEN opening_day:=opening_day-1; END IF;
  opening:=public.hmem_audit_boundary(opening_day,p_start::time,zone);
  closing:=public.hmem_audit_boundary(opening_day+CASE WHEN p_end<p_start THEN 1 ELSE 0 END,p_end::time,zone);
  SELECT last_scheduled_date,paused INTO previous,is_paused FROM public.audit_maintenance_control WHERE singleton;
  reason:=CASE WHEN is_paused THEN 'paused'
    WHEN opening IS NULL OR closing IS NULL OR closing<=opening THEN 'collapsed_window'
    WHEN moment<opening THEN 'window_closed' WHEN moment>=closing THEN 'window_closed'
    WHEN p_claim AND previous IS NOT NULL AND (actual_day<=previous OR actual_day-previous<p_days) THEN 'date_already_claimed_or_cadence'
    ELSE 'eligible' END;
  IF reason='eligible' AND p_claim THEN
    UPDATE public.audit_maintenance_control SET last_scheduled_date=actual_day,last_scheduled_zone=zone,last_claimed_at=moment
      WHERE singleton AND (last_scheduled_date IS NULL OR actual_day>last_scheduled_date AND actual_day-last_scheduled_date>=p_days);
    claimed:=FOUND;
    IF NOT claimed THEN reason:='date_already_claimed_or_cadence'; END IF;
    IF claimed THEN previous:=actual_day; END IF;
  END IF;
  first_date:=greatest(actual_day,coalesce(previous+p_days,actual_day));
  next_open:=CASE WHEN is_paused THEN NULL ELSE
    public.hmem_audit_next_eligibility(moment,p_start::time,p_end::time,zone,first_date) END;
  RETURN jsonb_build_object('eligible',reason='eligible','claimed',claimed,'reason',reason,
    'date',actual_day,'window_opening_date',opening_day,'timezone',zone,'window_start',opening,'window_end',closing,'next_eligibility',next_open);
END $$;

CREATE FUNCTION public.hmem_audit_relations() RETURNS TABLE(relation_oid OID,kind TEXT,relation_name TEXT,
  allocated BIGINT,directory TEXT,valid BOOLEAN)
LANGUAGE SQL STABLE SET search_path=pg_catalog AS $$
  WITH audit AS (SELECT oid,reltoastrelid FROM pg_class WHERE oid=to_regclass('public.audit_log')),
  targets AS (SELECT oid FROM audit UNION SELECT reltoastrelid FROM audit WHERE reltoastrelid<>0
    UNION SELECT indexrelid FROM pg_index WHERE indrelid IN(SELECT oid FROM audit UNION SELECT reltoastrelid FROM audit))
  SELECT c.oid,CASE WHEN c.oid=to_regclass('public.audit_log') THEN 'heap' WHEN c.relkind='t' THEN 'toast' ELSE 'index' END,
    format('%I.%I',n.nspname,c.relname),
    pg_relation_size(c.oid,'main')+pg_relation_size(c.oid,'fsm')+pg_relation_size(c.oid,'vm')+pg_relation_size(c.oid,'init'),
    rtrim(current_setting('data_directory'),'/\\')||'/'||regexp_replace(pg_relation_filepath(c.oid),'/[^/]+$',''),
    coalesce(i.indisvalid,TRUE)
  FROM targets t JOIN pg_class c ON c.oid=t.oid JOIN pg_namespace n ON n.oid=c.relnamespace
  LEFT JOIN pg_index i ON i.indexrelid=c.oid ORDER BY c.oid LIMIT 65
$$;

CREATE FUNCTION public.hmem_audit_identity() RETURNS JSONB
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog AS $$
DECLARE relations JSONB;
BEGIN
  PERFORM public.hmem_audit_require_owner();
  SELECT jsonb_agg(jsonb_build_object('oid',r.relation_oid::bigint,'kind',r.kind,'name',r.relation_name,
    'allocated',r.allocated,'directory',r.directory,'valid',r.valid)) INTO relations FROM public.hmem_audit_relations() r;
  IF jsonb_array_length(relations)>64 THEN
    RETURN jsonb_build_object('reason','relation_limit'); END IF;
  RETURN jsonb_build_object('reason','ok','server_address',host(inet_server_addr()),'server_port',inet_server_port(),
    'client_address',host(inet_client_addr()),'client_port',inet_client_port(),'backend_pid',pg_backend_pid(),
    'postmaster_pid',split_part(pg_read_file('postmaster.pid',0,64),E'\n',1)::int,
    'backend_start_us',(SELECT floor(extract(epoch FROM backend_start)*1000000)::bigint FROM pg_stat_activity WHERE pid=pg_backend_pid()),
    'postmaster_start_us',floor(extract(epoch FROM pg_postmaster_start_time())*1000000)::bigint,
    'data_directory',current_setting('data_directory'),'system_identifier',(SELECT system_identifier::text FROM pg_control_system()),
    'database_oid',(SELECT oid::bigint FROM pg_database WHERE datname=current_database()),'relations',relations);
END $$;

CREATE FUNCTION public.hmem_audit_assess() RETURNS JSONB
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog AS $$
DECLARE extension_schema TEXT; heap RECORD; toast RECORD; relation RECORD; statistic RECORD;
DECLARE indexes JSONB:='[]'; heap_bytes BIGINT; toast_bytes BIGINT:=0; wal_bytes BIGINT; archive_bytes BIGINT;
DECLARE slot_bytes NUMERIC; slot_count BIGINT; archive_bad BOOLEAN; reclaim BIGINT; total_bytes BIGINT;
BEGIN
  PERFORM public.hmem_audit_require_owner();
  SELECT n.nspname INTO extension_schema FROM pg_extension e JOIN pg_namespace n ON n.oid=e.extnamespace WHERE e.extname='pgstattuple';
  IF extension_schema IS NULL THEN RETURN jsonb_build_object('reason','diagnostic_missing'); END IF;
  IF (SELECT count(*) FROM public.hmem_audit_relations())>64 THEN RETURN jsonb_build_object('reason','relation_limit'); END IF;
  EXECUTE format('SELECT * FROM %I.pgstattuple($1::regclass)',extension_schema) INTO heap USING 'public.audit_log';
  heap_bytes:=pg_relation_size('public.audit_log');
  SELECT reltoastrelid::regclass AS oid INTO relation FROM pg_class WHERE oid='public.audit_log'::regclass;
  IF relation.oid::oid<>0 THEN
    EXECUTE format('SELECT * FROM %I.pgstattuple($1::regclass)',extension_schema) INTO toast USING relation.oid;
    toast_bytes:=greatest(0,toast.dead_tuple_len+toast.free_space-8192);
  END IF;
  reclaim:=greatest(0,heap.dead_tuple_len+heap.free_space-8192)+toast_bytes;
  FOR relation IN SELECT r.*,a.amname FROM public.hmem_audit_relations() r JOIN pg_class c ON c.oid=r.relation_oid
      LEFT JOIN pg_am a ON a.oid=c.relam WHERE r.kind='index' LOOP
    IF relation.amname='btree' AND relation.valid THEN
      EXECUTE format('SELECT * FROM %I.pgstatindex($1::regclass)',extension_schema) INTO statistic USING relation.relation_oid;
      IF statistic.leaf_pages<0 OR statistic.empty_pages<0 OR statistic.deleted_pages<0 OR
          (statistic.leaf_pages>0 AND NOT coalesce(statistic.avg_leaf_density>=0 AND statistic.avg_leaf_density<=100,FALSE)) THEN
        RETURN jsonb_build_object('reason','diagnostic_invalid_index_density'); END IF;
      indexes:=indexes||jsonb_build_array(jsonb_build_object('oid',relation.relation_oid::bigint,'name',relation.relation_name,
        'allocated',relation.allocated,'valid',TRUE,'confidence','full_scan_non_atomic',
        'reclaimable',least(greatest(0,relation.allocated-8192),greatest(0,((statistic.empty_pages+statistic.deleted_pages)*8192+
          CASE WHEN statistic.leaf_pages>0 AND statistic.avg_leaf_density>=0 AND statistic.avg_leaf_density<=100
            THEN statistic.leaf_pages*8192*greatest(0,0.90-statistic.avg_leaf_density/100.0) ELSE 0 END))::bigint)));
    ELSE indexes:=indexes||jsonb_build_array(jsonb_build_object('oid',relation.relation_oid::bigint,'name',relation.relation_name,
      'allocated',relation.allocated,'valid',relation.valid,'confidence','unsupported_index','reclaimable',NULL)); END IF;
  END LOOP;
  SELECT coalesce(sum(size),0)::bigint INTO wal_bytes FROM pg_ls_waldir();
  SELECT count(*)*pg_size_bytes(current_setting('wal_segment_size')) INTO archive_bytes
    FROM pg_ls_archive_statusdir() WHERE name LIKE '%.ready';
  SELECT count(*),coalesce(max(pg_wal_lsn_diff(pg_current_wal_lsn(),restart_lsn)),0) INTO slot_count,slot_bytes FROM pg_replication_slots;
  SELECT coalesce(last_failed_time>last_archived_time OR last_failed_time IS NOT NULL AND last_archived_time IS NULL,FALSE)
    INTO archive_bad FROM pg_stat_archiver;
  SELECT sum(allocated) INTO total_bytes FROM public.hmem_audit_relations();
  RETURN jsonb_build_object('reason',CASE WHEN slot_count>256 THEN 'slot_limit' ELSE 'ok' END,
    'confidence','full_scan_non_atomic','heap_allocated',heap_bytes,'total_allocated',total_bytes,
    'heap_toast_reclaimable',least(reclaim,total_bytes),'indexes',indexes,'wal_bytes',wal_bytes,
    'slot_retained_bytes',slot_bytes,'archive_pending_bytes',archive_bytes,'archive_failed',archive_bad);
END $$;

CREATE FUNCTION public.hmem_audit_record(p_assessment JSONB,p_cycle BOOLEAN) RETURNS VOID
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog AS $$
BEGIN
  PERFORM public.hmem_audit_require_owner();
  IF jsonb_typeof(p_assessment)<>'object' OR octet_length(p_assessment::text)>32768 THEN
    RAISE EXCEPTION USING ERRCODE='22023',MESSAGE='audit_maintenance_outcome_limit'; END IF;
  UPDATE public.audit_maintenance_control SET last_assessment=p_assessment,
    last_cycle=CASE WHEN p_cycle THEN p_assessment ELSE last_cycle END WHERE singleton;
END $$;

-- Fixed helper capabilities are opt-in grants to a separately provisioned
-- maintenance login. There is no PUBLIC grant, extension installation or
-- application-role elevation. Administrators retain existing bootstrap rights.
REVOKE ALL ON FUNCTION public.hmem_audit_runtime_safe(),public.hmem_audit_require_owner(),public.hmem_audit_status(),
  public.hmem_audit_pause(BOOLEAN),public.hmem_audit_boundary(DATE,TIME,TEXT),
  public.hmem_audit_next_eligibility(TIMESTAMPTZ,TIME,TIME,TEXT,DATE),
  public.hmem_audit_schedule(TIMESTAMPTZ,TEXT,TEXT,TEXT,INT,BOOLEAN),public.hmem_audit_relations(),
  public.hmem_audit_identity(),public.hmem_audit_assess(),public.hmem_audit_record(JSONB,BOOLEAN) FROM PUBLIC;

INSERT INTO schema_migrations(version,name) VALUES(56,'V056__audit_maintenance_controls.sql');
COMMIT;
