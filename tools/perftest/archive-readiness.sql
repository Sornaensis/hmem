-- Read-only sealed-archive footprint and reclamation-readiness snapshot.
--
-- Usage (stdout is exactly one JSON document; errors go to stderr):
--   psql -X -q -v ON_ERROR_STOP=1 -At -d <database> -f tools/perftest/archive-readiness.sql
--
-- Everything runs in one REPEATABLE READ, READ ONLY transaction with bounded
-- statement/lock timeouts and ends with ROLLBACK. It never installs extensions,
-- reads transcript or gzip payload bytes (octet_length/pg_column_size read TOAST
-- pointers only), vacuums, enables reclamation or changes controls.
--
-- Run it as the runtime login (the server and hmem-ctl login): the fence
-- functions judge session_user, and that login already reads the session and
-- archive tables, which this script requires: USAGE on schema public, SELECT on
-- sessions, session_chunks, workspaces, schema_migrations and the
-- session_archive_{generations,jobs,parts,checkpoints,ledger,control,requests,
-- reclamation,sanitized_contents} tables, and EXECUTE on
-- hmem_archive_runtime_safe() and hmem_archive_sanitized_runtime_safe(). A
-- missing required privilege stops the script with an error. Optional items
-- are decided before the snapshot query, so unreadable objects never enter it.
-- Each is reported as null and named in "not_readable" when the login lacks the
-- privilege (or the object): pg_ls_waldir EXECUTE (wal retained_*), SELECT on
-- session_archive_replacement_policies (V062 revokes it from PUBLIC),
-- hmem_audit_status EXECUTE, SELECT on audit_maintenance_control, and
-- pgstattuple_approx EXECUTE. "pgstattuple_approx" is also null when that
-- extension is absent. Do not call GET .../archive manifests for readiness
-- checks: that route records an archive request for the session.

\set QUIET on
\set ON_ERROR_STOP on
\pset tuples_only on
\pset format unaligned
\pset pager off

BEGIN TRANSACTION ISOLATION LEVEL REPEATABLE READ READ ONLY;
SET LOCAL statement_timeout = '120s';
SET LOCAL lock_timeout = '1s';

-- Privilege tests use catalog OIDs only. Name lookups (to_regclass,
-- to_regprocedure, text arguments to has_*_privilege) raise when the login
-- lacks USAGE on the named schema, which would abort the script.
SELECT coalesce((SELECT has_function_privilege(p.oid, 'EXECUTE')
                 FROM pg_proc p JOIN pg_namespace n ON n.oid = p.pronamespace
                 WHERE n.nspname = 'pg_catalog' AND p.proname = 'pg_ls_waldir' AND p.pronargs = 0), false) AS can_ls_waldir,
       coalesce((SELECT has_schema_privilege(n.oid, 'USAGE') AND has_table_privilege(c.oid, 'SELECT')
                 FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace
                 WHERE n.nspname = 'public' AND c.relname = 'session_archive_replacement_policies'), false) AS can_read_policies,
       coalesce((SELECT has_schema_privilege(n.oid, 'USAGE') AND has_function_privilege(p.oid, 'EXECUTE')
                 FROM pg_proc p JOIN pg_namespace n ON n.oid = p.pronamespace
                 WHERE n.nspname = 'public' AND p.proname = 'hmem_audit_status' AND p.pronargs = 0), false) AS can_audit_status,
       coalesce((SELECT has_schema_privilege(n.oid, 'USAGE') AND has_table_privilege(c.oid, 'SELECT')
                 FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace
                 WHERE n.nspname = 'public' AND c.relname = 'audit_maintenance_control'), false) AS can_audit_control,
       EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'pgstattuple') AS has_pgstattuple,
       coalesce((SELECT has_schema_privilege(x.extnamespace, 'USAGE') AND has_function_privilege(p.oid, 'EXECUTE')
                 FROM pg_extension x JOIN pg_proc p ON p.pronamespace = x.extnamespace
                 WHERE x.extname = 'pgstattuple' AND p.proname = 'pgstattuple_approx'
                   AND p.proargtypes = '2205'::oidvector), false) AS can_pgstattuple,
       coalesce((SELECT n.nspname FROM pg_extension x JOIN pg_namespace n ON n.oid = x.extnamespace
                 WHERE x.extname = 'pgstattuple'), 'public') AS pgstattuple_schema \gset

-- Optional values as JSON text ('' = not read), captured in the same snapshot.
\set wal_json ''
\set policies_json ''
\set audit_status_json ''
\set audit_backup_json ''
\set pgstattuple_json ''
\if :can_ls_waldir
SELECT jsonb_build_object('retained_segments', count(*) FILTER (WHERE name ~ '^[0-9A-F]{24}$'),
         'retained_bytes', sum(size)::bigint)::text AS wal_json FROM pg_ls_waldir() \gset
\endif
\if :can_read_policies
SELECT coalesce(jsonb_agg(jsonb_build_object('format_label', format_label, 'eligible_from', eligible_from)
         ORDER BY format_label), '[]')::text AS policies_json
FROM public.session_archive_replacement_policies \gset
\endif
\if :can_audit_status
SELECT (public.hmem_audit_status() - 'last_assessment' - 'last_cycle')::text AS audit_status_json \gset
\endif
\if :can_audit_control
SELECT coalesce((SELECT to_jsonb(backup_receipt IS NOT NULL)::text
                 FROM public.audit_maintenance_control WHERE singleton), 'null') AS audit_backup_json \gset
\endif
\if :can_pgstattuple
SELECT coalesce(jsonb_object_agg(r.name, jsonb_build_object('heap', to_jsonb(h), 'toast', to_jsonb(t))), '{}')::text
         AS pgstattuple_json
FROM (VALUES ('session_chunks'), ('session_archive_parts'), ('session_archive_checkpoints'),
             ('session_archive_ledger'), ('change_stream_outbox'), ('audit_log')) r(name)
JOIN pg_class c ON c.oid = to_regclass('public.' || r.name)
CROSS JOIN LATERAL :"pgstattuple_schema".pgstattuple_approx(c.oid) h
LEFT JOIN LATERAL (SELECT * FROM :"pgstattuple_schema".pgstattuple_approx(c.reltoastrelid)
                   WHERE c.reltoastrelid <> 0) t ON true \gset
\endif

WITH rel(name) AS (
  VALUES ('session_chunks'), ('session_archive_parts'), ('session_archive_checkpoints'),
         ('session_archive_ledger'), ('session_archive_generations'), ('session_archive_jobs'),
         ('session_archive_reclamation'), ('session_archive_requests'),
         ('session_archive_sanitized_contents'), ('session_archive_content_blocks'),
         ('session_archive_replacement_policies'), ('session_occurrences'), ('sessions'),
         ('change_stream_outbox'), ('audit_log')
), sizes AS (
  SELECT r.name, c.oid, c.reltoastrelid, c.reltuples,
         pg_table_size(c.oid) - coalesce(pg_total_relation_size(nullif(c.reltoastrelid, 0)), 0) AS heap,
         coalesce(pg_total_relation_size(nullif(c.reltoastrelid, 0)), 0) AS toast,
         pg_indexes_size(c.oid) AS indexes,
         pg_total_relation_size(c.oid) AS total
  FROM rel r JOIN pg_class c ON c.oid = to_regclass('public.' || r.name)
), vac AS (
  SELECT s.name,
         jsonb_build_object('live_tuples', st.n_live_tup, 'dead_tuples', st.n_dead_tup,
           'toast_live_tuples', ts.n_live_tup, 'toast_dead_tuples', ts.n_dead_tup,
           'heap_pages', c.relpages, 'heap_all_visible_pages', c.relallvisible,
           'toast_pages', t.relpages, 'toast_all_visible_pages', t.relallvisible,
           'last_vacuum', greatest(st.last_vacuum, st.last_autovacuum),
           'toast_last_vacuum', greatest(ts.last_vacuum, ts.last_autovacuum)) AS v
  FROM sizes s JOIN pg_class c ON c.oid = s.oid
  LEFT JOIN pg_class t ON t.oid = nullif(s.reltoastrelid, 0)
  LEFT JOIN pg_stat_all_tables st ON st.relid = s.oid
  LEFT JOIN pg_stat_all_tables ts ON ts.relid = nullif(s.reltoastrelid, 0)
), chunk_state AS (
  SELECT CASE WHEN s.state <> 'terminated' THEN 'active'
              WHEN j.state = 'ready' THEN 'sealed_ready_archive'
              ELSE 'sealed_without_ready_archive' END AS class,
         count(*) AS chunk_rows, count(DISTINCT c.session_id) AS sessions,
         sum(octet_length(c.data))::bigint AS logical_bytes,
         sum(pg_column_size(c.data))::bigint AS stored_value_bytes
  FROM session_chunks c JOIN sessions s ON s.id = c.session_id
  LEFT JOIN session_archive_jobs j ON j.session_id = s.id AND j.workspace_id = s.workspace_id
  GROUP BY 1
), session_state AS (
  SELECT s.state, count(*) AS sessions, sum(s.committed_bytes)::bigint AS committed_bytes,
         sum(s.final_bytes)::bigint AS final_bytes
  FROM sessions s GROUP BY s.state
), gen AS (
  SELECT g.*, j.state AS job_state, j.replacement_policy_sha256 AS job_policy,
         s.final_bytes, s.final_sha256, v.generation_id IS NOT NULL AS sanitized
  FROM session_archive_generations g
  JOIN session_archive_jobs j ON (j.generation_id, j.session_id, j.workspace_id) = (g.id, g.session_id, g.workspace_id)
  JOIN sessions s ON s.id = g.session_id
  LEFT JOIN session_archive_sanitized_contents v ON v.generation_id = g.id
  WHERE j.state = 'ready' AND g.published_at IS NOT NULL
), part_agg AS (
  SELECT generation_id, count(*) AS n, sum(octet_length(data))::bigint AS bytes,
         sum(pg_column_size(data))::bigint AS stored
  FROM session_archive_parts GROUP BY generation_id
), ckpt_agg AS (
  SELECT generation_id, count(*) AS n, sum(octet_length(dictionary))::bigint AS dict,
         sum(pg_column_size(dictionary))::bigint AS dict_stored
  FROM session_archive_checkpoints GROUP BY generation_id
), ledger_agg AS (
  SELECT generation_id, count(*) AS n FROM session_archive_ledger GROUP BY generation_id
), ctl AS (
  SELECT * FROM session_archive_control WHERE singleton
), eligible AS (
  SELECT s.id, s.workspace_id, s.format_label, s.terminated_at,
         (c.backfill_enabled OR s.terminated_at >= c.cutover_at
          OR EXISTS (SELECT 1 FROM session_archive_requests q WHERE q.session_id = s.id AND q.workspace_id = s.workspace_id)) AS discoverable,
         j.state AS job_state, j.replacement_policy_sha256 IS NOT NULL AS frozen_sanitized,
         p.format_label IS NOT NULL AND s.terminated_at >= p.eligible_from AS policy_match
  FROM sessions s JOIN workspaces w ON w.id = s.workspace_id CROSS JOIN ctl c
  LEFT JOIN session_archive_jobs j ON j.session_id = s.id AND j.workspace_id = s.workspace_id
  LEFT JOIN jsonb_to_recordset(nullif(:'policies_json', '')::jsonb) AS p(format_label text, eligible_from timestamptz)
    ON p.format_label = s.format_label COLLATE "C"
  WHERE s.state = 'terminated' AND w.deleted_at IS NULL
)
SELECT jsonb_pretty(jsonb_build_object(
  'schema', 'hmem/archive-readiness-snapshot/v1',
  'captured_at', clock_timestamp(),
  'environment', jsonb_build_object(
    'server_version', current_setting('server_version'),
    'schema_migration', (SELECT max(version) FROM schema_migrations),
    'session_user', session_user::text,
    'block_size', current_setting('block_size')::int,
    'wal_segment_size', current_setting('wal_segment_size'),
    'max_wal_size', current_setting('max_wal_size'),
    'wal_keep_size', current_setting('wal_keep_size'),
    'archive_mode', current_setting('archive_mode'),
    'autovacuum', current_setting('autovacuum')),
  'database_bytes', pg_database_size(current_database()),
  'not_readable', (SELECT coalesce(jsonb_agg(item ORDER BY item), '[]') FROM (VALUES
      ('wal.retained_segments and wal.retained_bytes (pg_ls_waldir EXECUTE)', :'can_ls_waldir'::boolean),
      ('eligibility.installed_policy_formats and future_policy_matches_without_job (session_archive_replacement_policies SELECT)', :'can_read_policies'::boolean),
      ('maintenance_prerequisites.audit_status (hmem_audit_status EXECUTE)', :'can_audit_status'::boolean),
      ('maintenance_prerequisites.audit_backup_receipt_present (audit_maintenance_control SELECT)', :'can_audit_control'::boolean),
      ('pgstattuple_approx (pgstattuple_approx EXECUTE)', :'can_pgstattuple'::boolean OR NOT :'has_pgstattuple'::boolean))
    v(item, readable) WHERE NOT readable),
  'wal', jsonb_build_object(
    'current_lsn', pg_current_wal_lsn()::text,
    'retained_segments', nullif(:'wal_json', '')::jsonb -> 'retained_segments',
    'retained_bytes', nullif(:'wal_json', '')::jsonb -> 'retained_bytes',
    'replication_slots', (SELECT count(*) FROM pg_replication_slots),
    'slot_retained_bytes', (SELECT max(pg_wal_lsn_diff(pg_current_wal_lsn(), restart_lsn))::bigint
                            FROM pg_replication_slots WHERE restart_lsn IS NOT NULL)),
  'relations', (SELECT jsonb_object_agg(s.name, jsonb_build_object(
      'row_estimate', s.reltuples::bigint, 'heap_bytes', s.heap, 'toast_bytes', s.toast,
      'index_bytes', s.indexes, 'total_bytes', s.total) || v.v)
    FROM sizes s JOIN vac v ON v.name = s.name),
  'sessions', (SELECT jsonb_object_agg(state, jsonb_build_object('sessions', sessions,
      'committed_bytes', committed_bytes, 'final_bytes', final_bytes)) FROM session_state),
  'raw_chunks', (SELECT jsonb_object_agg(class, jsonb_build_object('sessions', sessions,
      'rows', chunk_rows, 'logical_bytes', logical_bytes, 'stored_value_bytes', stored_value_bytes))
    FROM chunk_state),
  'archives', (SELECT jsonb_build_object(
      'published_generations', count(*),
      'exact_generations', count(*) FILTER (WHERE NOT sanitized),
      'sanitized_generations', count(*) FILTER (WHERE sanitized),
      'raw_bytes', coalesce(sum(raw_bytes), 0), 'compressed_bytes', coalesce(sum(compressed_bytes), 0),
      'part_rows', coalesce(sum(part_count), 0), 'checkpoint_rows', coalesce(sum(checkpoint_count), 0),
      'ledger_rows', coalesce(sum(ledger_count), 0),
      'max_raw_bytes', max(raw_bytes), 'max_compressed_bytes', max(compressed_bytes),
      'codec_implementations', (SELECT jsonb_object_agg(ci, n) FROM (SELECT codec_implementation ci, count(*) n FROM gen GROUP BY 1) x),
      'dictionary_logical_bytes', (SELECT coalesce(sum(dict), 0) FROM ckpt_agg),
      'dictionary_stored_bytes', (SELECT coalesce(sum(dict_stored), 0) FROM ckpt_agg),
      'part_logical_bytes', (SELECT coalesce(sum(bytes), 0) FROM part_agg),
      'part_stored_bytes', (SELECT coalesce(sum(stored), 0) FROM part_agg))
    FROM gen),
  'publication_consistency', (SELECT jsonb_build_object(
      'scope', 'structural metadata only; not gzip/digest readback',
      'unpublished_generations', (SELECT count(*) FROM session_archive_generations WHERE published_at IS NULL),
      'ready_jobs_without_published_generation', (SELECT count(*) FROM session_archive_jobs j
         LEFT JOIN session_archive_generations g ON g.id = j.generation_id
         WHERE j.state = 'ready' AND (g.id IS NULL OR g.published_at IS NULL)),
      'receipt_mismatches', count(*) FILTER (WHERE g.raw_bytes IS DISTINCT FROM g.final_bytes OR g.raw_sha256 IS DISTINCT FROM g.final_sha256),
      'part_count_or_bytes_mismatches', count(*) FILTER (WHERE pa.n IS DISTINCT FROM g.part_count OR pa.bytes IS DISTINCT FROM g.compressed_bytes),
      'checkpoint_count_mismatches', count(*) FILTER (WHERE coalesce(ca.n, 0) IS DISTINCT FROM g.checkpoint_count),
      'ledger_count_mismatches', count(*) FILTER (WHERE coalesce(la.n, 0) IS DISTINCT FROM g.ledger_count),
      'sanitized_policy_binding_mismatches', count(*) FILTER (WHERE g.sanitized AND g.job_policy IS NULL))
    FROM gen g LEFT JOIN part_agg pa ON pa.generation_id = g.id
    LEFT JOIN ckpt_agg ca ON ca.generation_id = g.id LEFT JOIN ledger_agg la ON la.generation_id = g.id),
  'jobs', (SELECT jsonb_build_object(
      'by_state', coalesce((SELECT jsonb_object_agg(state, n) FROM (SELECT state, count(*) n FROM session_archive_jobs GROUP BY 1) x), '{}'),
      'by_phase', coalesce((SELECT jsonb_object_agg(coalesce(phase, 'null'), n) FROM (SELECT phase, count(*) n FROM session_archive_jobs GROUP BY 1) x), '{}'),
      'error_codes', coalesce((SELECT jsonb_object_agg(error_code, n) FROM (SELECT error_code, count(*) n FROM session_archive_jobs WHERE error_code IS NOT NULL GROUP BY 1) x), '{}'),
      'live_leases', (SELECT count(*) FROM session_archive_jobs WHERE lease_expires_at > clock_timestamp()),
      'unfinished', (SELECT coalesce(jsonb_agg(x), '[]') FROM (
         SELECT j.session_id, j.state, j.phase, j.attempt, j.raw_progress, s.final_bytes, j.retry_at, j.error_code
         FROM session_archive_jobs j JOIN sessions s ON s.id = j.session_id
         WHERE j.state <> 'ready' ORDER BY j.retry_at LIMIT 20) x))),
  'eligibility', (SELECT jsonb_build_object(
      'terminated_sessions', count(*),
      'discoverable_exact', count(*) FILTER (WHERE discoverable),
      'not_discoverable_without_backfill_or_request', count(*) FILTER (WHERE NOT discoverable),
      'with_job', count(*) FILTER (WHERE job_state IS NOT NULL),
      'ready', count(*) FILTER (WHERE job_state = 'ready'),
      'frozen_sanitized_jobs', count(*) FILTER (WHERE frozen_sanitized),
      'installed_policy_formats', nullif(:'policies_json', '')::jsonb,
      'terminated_by_format', (SELECT jsonb_object_agg(coalesce(f, 'null'), n) FROM (SELECT format_label f, count(*) n FROM eligible GROUP BY 1) x),
      'future_policy_matches_without_job', CASE WHEN :'can_read_policies'::boolean
        THEN count(*) FILTER (WHERE policy_match AND job_state IS NULL) END)
    FROM eligible),
  'reclamation', (SELECT jsonb_build_object(
      'rows', count(*), 'verified', count(*) FILTER (WHERE verified_at IS NOT NULL),
      'sanitized_attested', count(*) FILTER (WHERE sanitized_attestation_sha256 IS NOT NULL),
      'in_progress', count(*) FILTER (WHERE deleted_bytes > 0 AND NOT complete),
      'complete', count(*) FILTER (WHERE complete),
      'deleted_bytes', coalesce(sum(deleted_bytes), 0),
      'error_codes', coalesce((SELECT jsonb_object_agg(error_code, n) FROM (SELECT error_code, count(*) n FROM session_archive_reclamation WHERE error_code IS NOT NULL GROUP BY 1) x), '{}'))
    FROM session_archive_reclamation),
  'control', (SELECT to_jsonb(c) - 'permit_token' || jsonb_build_object(
      'permit_token_present', c.permit_token IS NOT NULL,
      'permit_live', c.permit_expires_at > clock_timestamp()) FROM ctl c),
  'fences', jsonb_build_object(
    'runtime_safe_for_session_user', public.hmem_archive_runtime_safe(),
    'sanitized_runtime_safe_for_session_user', public.hmem_archive_sanitized_runtime_safe(),
    'session_user_attributes', (SELECT jsonb_build_object('superuser', rolsuper, 'createrole', rolcreaterole,
       'createdb', rolcreatedb, 'replication', rolreplication, 'bypassrls', rolbypassrls)
       FROM pg_roles WHERE rolname = session_user),
    'capability_owner', (SELECT jsonb_build_object('login', rolcanlogin, 'inherit', rolinherit,
       'superuser', rolsuper, 'createrole', rolcreaterole, 'memberships',
       (SELECT count(*) FROM pg_auth_members m WHERE m.roleid = o.oid OR m.member = o.oid))
       FROM pg_roles o WHERE rolname = 'hmem_archive_maintenance_owner'),
    'reclaim_function_owner', (SELECT pg_get_userbyid(proowner)::text FROM pg_proc
       WHERE oid = 'public.hmem_archive_reclaim(uuid,uuid,uuid,text,uuid)'::regprocedure),
    'reclaim_execute_grantees', (SELECT coalesce(jsonb_agg(CASE WHEN a.grantee = 0 THEN 'PUBLIC' ELSE pg_get_userbyid(a.grantee)::text END ORDER BY a.grantee), '[]')
       FROM pg_proc p CROSS JOIN LATERAL aclexplode(coalesce(p.proacl, acldefault('f', p.proowner))) a
       WHERE p.oid = 'public.hmem_archive_reclaim(uuid,uuid,uuid,text,uuid)'::regprocedure AND a.privilege_type = 'EXECUTE'),
    'login_roles', (SELECT jsonb_agg(jsonb_build_object('role', rolname, 'superuser', rolsuper, 'createrole', rolcreaterole) ORDER BY rolname)
       FROM pg_roles WHERE rolcanlogin AND rolname !~ '^pg_'),
    'guard_triggers_not_enabled', (SELECT coalesce(jsonb_agg(t.tgrelid::regclass::text || '.' || t.tgname), '[]')
       FROM pg_trigger t WHERE NOT t.tgisinternal AND t.tgenabled <> 'O'
         AND t.tgrelid::regclass::text IN ('session_chunks', 'sessions', 'session_archive_control', 'session_archive_jobs',
           'session_archive_generations', 'session_archive_parts', 'session_archive_checkpoints', 'session_archive_ledger',
           'session_archive_reclamation', 'session_occurrences')),
    'guard_trigger_count', (SELECT count(*) FROM pg_trigger t WHERE NOT t.tgisinternal
         AND t.tgrelid::regclass::text IN ('session_chunks', 'session_archive_control', 'session_archive_jobs',
           'session_archive_generations', 'session_archive_parts', 'session_archive_checkpoints', 'session_archive_ledger',
           'session_archive_reclamation'))),
  'maintenance_prerequisites', jsonb_build_object(
    'audit_status', nullif(:'audit_status_json', '')::jsonb,
    'audit_backup_receipt_present', nullif(:'audit_backup_json', '')::jsonb),
  'pgstattuple_approx', nullif(:'pgstattuple_json', '')::jsonb
));

ROLLBACK;
