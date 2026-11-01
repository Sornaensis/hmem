# Audit maintenance controls

The server owns one audit maintenance worker. `hmem-ctl audit-maintenance status
--json` shows effective non-secret settings, the last assessment and any physical
attempt. `pause` persists a pause and requests cancellation/drain of active work;
`resume` clears it. `run-once` uses the same safety and window gates as the worker.
Eligible heap compaction takes priority over individual concurrent index rebuilds.
An assessment that executes no physical command states `operation: assessment`
and `executed: false`.

An absent `audit_maintenance` block enables daily eligibility during 03:00–04:00
in the database server's configured local timezone. Missing fields keep their
defaults; explicit disable wins. See
[`config/audit-maintenance.yaml.example`](../../config/audit-maintenance.yaml.example).
An invalid typed configuration file disables audit maintenance during the
existing ordinary-config fallback. An absent configuration file uses defaults.
Separate credentials are required before the worker connects.
When `hmem-server --db` supplies an application connection override, the worker
reports `application_target_override_unresolved` before opening any configured
database connection. Ordinary service continues on the override target. Configure
the shared target through YAML/environment instead to enable maintenance.

The local timezone is accepted only when the dedicated connection still uses a
server default, configuration-file or command-line timezone. A database, role,
client or session override returns `unproven_server_local_timezone`; remove that
override or set an explicit supported timezone name in YAML. Gap boundaries move
to the first valid local minute, repeated boundaries use their earliest instant,
and collapsed windows are skipped. The actual execution date in that timezone
is claimed durably before assessment/safety gates; an overnight window's opening
date is used only for its bounds. Unsafe or
interrupted cycles consume that date; restarting cannot catch up outside the
window or repeat the date. `interval_days` counts actual local calendar dates.
For overnight windows the next eligible date may begin at its resolved midnight
inside the previous evening's window. Next-eligibility search is bounded to eight
local dates and reports unknown when no valid candidate exists in that range.

## Narrow database setup

Apply V056 and V057 through the existing migration procedure. They install no extension
and grants no maintenance capability to the application role. An administrator
may install the native PostgreSQL 18 `pgstattuple` extension explicitly. Missing
diagnostics defer with `diagnostic_missing` while ordinary service remains
available.

Create a separate login, using your normal credential provisioning:

```sql
CREATE ROLE hmem_audit_maint LOGIN NOINHERIT NOSUPERUSER NOCREATEDB
  NOCREATEROLE NOREPLICATION NOBYPASSRLS;
GRANT CONNECT ON DATABASE hmem TO hmem_audit_maint;
GRANT USAGE ON SCHEMA public TO hmem_audit_maint;
GRANT MAINTAIN ON public.audit_log TO hmem_audit_maint;
GRANT EXECUTE ON FUNCTION public.hmem_audit_runtime_safe(),
  public.hmem_audit_status(), public.hmem_audit_pause(boolean),
  public.hmem_audit_schedule(timestamptz,text,text,text,int,boolean),
  public.hmem_audit_identity(), public.hmem_audit_assess(),
  public.hmem_audit_record(jsonb,boolean),
  public.hmem_audit_operation_catalog(),
  public.hmem_audit_operation_begin(uuid,text,oid,boolean),
  public.hmem_audit_operation_running(uuid),
  public.hmem_audit_operation_monitor(uuid),
  public.hmem_audit_operation_uncertain(uuid),
  public.hmem_audit_operation_finish(uuid,text,jsonb,jsonb) TO hmem_audit_maint;
```

Set `HMEM_AUDIT_MAINTENANCE_DB_USER` and
`HMEM_AUDIT_MAINTENANCE_DB_PASSWORD` for the server/CLI process, or use the
corresponding optional YAML fields. Host, port, database and TLS settings stay
those of the configured database; the actual PostgreSQL session users must
differ. Passwords and usernames are excluded from control output and `Show`.

The login must not assume a privileged role, wrapper/control owner, audit owner,
schema creator, broad maintenance/data role or server-file/program role. This
includes indirect and `NOINHERIT` memberships that permit `SET ROLE`. Diagnostic
wrappers accept no filenames, relation names or arbitrary commands. They expose
only the audit relation, bound indexes/TOAST, connection identity and aggregate
WAL pressure. A session advisory lock spans the complete assessment and any
following operation; competing server/CLI callers report `maintenance_busy`.

## Measurements and bounds

`pgstattuple`/`pgstatindex` use bounded statement and lock budgets. Their full
scans are non-atomic during concurrent writes: reclaimable bytes are estimates,
not a promise of exact reduction. Unsupported index methods remain explicit.
At most 64 owned relation entries are assessed. Each relation's heap/index and
auxiliary forks is counted once; TOAST is not added twice.

Actual native host capacity requires local NTFS storage and a proven local PostgreSQL 18 connection,
the same effective Windows user SID **and logon AuthenticationId**, matching
backend/postmaster lifetime, exact loopback endpoint, cluster identity and fixed
directory chain. The native bridge retains process, ancestor and original
reparse handles. It rejects remote/unknown paths before following them and uses
volume GUIDs to group data/tablespace/WAL requirements. Different logons,
unsupported devices or unknown capacity defer; client-drive free space and
configured archive allowances are not substitutes. Normal Windows `pg_ctl`
restricted-token launch has been qualified in a disposable owned job. Other
filesystems, including ReFS, defer because this bridge's qualified file identity
uses the NTFS 64-bit file index.

The conservative assessment reserves two times current allocation on each data
volume and three times total allocation on the WAL volume, plus one configured
free-space reserve per distinct volume. It checks actual WAL allocation,
replication-slot retention, pending archive segments and unresolved archive
failures. Unknown measurements fail closed. The worker's cooperative five-second
stop retains its actual owner on an unjoined native/query call; callers must
retry the same owner and preserve dependent resources rather than claiming a
clean drain.

Before heap admission, a trusted administrator must provide a fresh verified
backup/restore receipt bound to the cluster system identifier and database OID.
The receipt records `verified_at`, `backup_sha256`, `recovery_point` (a PostgreSQL
WAL LSN), `restore_evidence_sha256` and `restore_verified: true`. Unknown, stale, future or
mismatched receipts fail readiness. This metadata is an explicit administrator
attestation. After independently taking and restoring a backup, the control owner
calls `public.hmem_audit_backup_attest(receipt_jsonb)` with these fields plus
`system_identifier` and `database_oid`. The runtime login must receive no EXECUTE
grant on this function and cannot attest even if accidentally granted EXECUTE.
Creating controls does not take a backup.

## Physical operations and recovery

Heap work executes top-level `VACUUM (FULL, ANALYZE) ONLY public.audit_log`;
inheritance children are outside its authority. Index work executes top-level
`REINDEX INDEX CONCURRENTLY` for one validated, valid index directly bound to the
audit table. Reclaim thresholds apply to each index, with bounded per-index
cooldowns. A heap rewrite cools all current audit indexes to avoid immediately
rebuilding them again. Both kinds require the configured window to remain open,
actual data/WAL headroom, and bounded lock/operation budgets.

`VACUUM FULL` holds `ACCESS EXCLUSIVE`, so all audit readers and application
writes that append audit history can wait during the rewrite. The short lock
budget bounds acquisition; the worker observes foreground waiters and requests
cancellation when they appear. Cancellation, joining and recovery can also take
time, so callers may see a wait. Unjoined work retains its owner and is reported
as cleanup pending rather than a completed drain.

Physical cancellation currently requires the qualified native Windows libpq 17+
modern cancellation API. Older clients and other platforms defer physical work;
ordinary service can still start. The bridge resolves optional symbols from the
actual linked libpq module. It retains one separate cancel object per operation.
A cancellation signal is not completion: query and cancel transport must join.
After any attempted cancel, the original client is retired and its retained
native backend handle must report exit before a new connection can reconcile.
The worker keeps its singleton/native owner while samples or commands remain
unjoined. A CLI caller enters a cleanup-pending wait on that same owner and does
not admit a retry or report a clean drain.

V057 retains one bounded operation UUID/state with original backend identity,
target and before/after catalog evidence. Unacknowledged completion is reconciled
against actual filenodes, index validity, definitions and constraints under the
singleton. Cancellation may leave a committed heap rewrite or partial concurrent
index phases; SQLSTATE 57014 is not proof of rollback. Unknown/invalid index
artifacts require administrator remediation. The worker never drops an index
because its name ends in `_ccnew` or `_ccold`. A failure after admission remains
physical and uncertain; it is never relabeled as an unexecuted assessment.

The migrations retain one control row, a last assessment and a last scheduled-cycle
receipt (each at most 32 KiB), and a backup receipt of at most 4 KiB. It deletes no
audit, session, planning or replay history. Disable or pause the worker to roll
back eligibility; this does not undo completed physical rewrites or restore already
pruned outbox events. Keep the independent
[one-day replay recovery policy](change-stream-retention.md).
