# Upgrade reference

## Session lifecycle audit retention (V052)

Take a verified backup, then apply the ordered migration chain before serving
the matching build. V052 keeps session start, actual termination and new task
connection audit records; it removes other historical session-family audit
operations using their stored snapshots. Missing or malformed transition states
are not termination evidence. All other audit entities, transcript bytes,
session metadata, task links and canonical change-stream history are preserved.

The trigger replacement, historical cleanup and ledger registration commit
atomically; a failed migration can be retried after correcting its cause. There
is no down migration for removed audit records. Recover them from the verified
pre-change backup. See the [schema policy](schema.md#session-lifecycle-audit-policy-v052).

## Sealed archive maintenance (V050 and V051)

Apply the ordered chain with a separate PostgreSQL migration administrator before
starting the archive-compatible server/ctl build. V051 atomically provisions its
restricted NOLOGIN capability owner, control/progress relations, guards and
migration ledger row. It leaves historical discovery and raw reclamation off;
new seals and explicitly requested older sealed views still use the archive
worker. A failed preflight or ledger insertion rolls back the migration.

Use a runtime role that owns no evidence/maintenance tables or guard functions,
cannot CREATE in `public` or hold TRIGGER rights on protected write targets,
directly or through assumable roles, is not a superuser or CREATEROLE role and
cannot assume the maintenance owner. Remove caller-controlled attached trigger
callbacks even if their former TRIGGER grants have already been revoked. Grant
the trusted server/ctl role EXECUTE on
`public.hmem_archive_reclaim(uuid,uuid,uuid,text,uuid)` explicitly; PUBLIC receives no
grant. Existing application DML grants and server authorization are still needed.
The database refuses reclamation enablement and work from an unsafe runtime role.
An administrative connection may migrate and compress new seals, but cannot
qualify that runtime boundary by acting as an ordinary application role.

V051 also schema-qualifies the existing runtime-identity validator's helper
calls, preserving its accepted values and IMMUTABLE behavior. This permits
`pg_restore` to load session CHECK constraints under its empty search path.

Install and qualify compatible range/proof/replay/ZIP readers and the complete
archive viewer before enabling historical backfill or deletion. Inspect
`hmem-ctl archives status --json`; configure capacity, then use the explicit
`backfill-enable --qualified` and `reclaim-enable --qualified` controls only in
the approved change window. Completion of implementation or tests does not
enable production maintenance. See the [maintenance reference](sealed-session-archives.md#maintenance-and-recovery-v051)
for commands, capacity meaning, pause/retry and backup verification.

Take and verify a backup after reclamation as well as the pre-change backup.
Restore the post-cleanup backup into an empty replacement database and verify
archive-only ranges, historical proofs, exact replay and ZIP before retiring the
old database. Roll back code only to a tested archive-compatible build. A raw-only
build requires restoring the pre-cleanup backup and loses later writes unless
they are separately recovered. Do not delete the archive schema or ledger as a
rollback step.

## Agent sessions (V041 and V042)

Deploy matching server, `hmem-ctl`, MCP and frontend artifacts. Stop old writers
and process-owned followers, take the operator-owned verified backup described
below, and run the normal ordered migration chain before serving this release.
V041 creates immutable session/chunk evidence and additive task links; V042 adds
compact attributed audit and durable change delivery. Require both migrations
to succeed before exposing session routes or the Sessions tab. Do not run old
writers alongside the new session contract. The existing maintenance migration
command and failure/backup rules below still apply.

No workspace snapshot profile or item shape changes for sessions. Session
transcripts remain available only through authorized bounded HTTP/MCP reads;
stream and audit payloads contain metadata invalidations. Existing workspace
read/edit permissions, cookie CSRF and bearer/declaration rules remain the
authority. There is no uploader-owned seal or new creator-only permission.

Follower registries belong to one MCP process and are not durable jobs. Restart
loses their local controls and receipts; explicit start with the same UUID/file
verifies the entire committed prefix before resume. Process loss leaves active
sessions active and eventually stale. Stop/flush receipts describe a finite
captured boundary; a managing agent separately terminates the verified committed
prefix. Later producer output is outside the evidence. Refresh MCP discovery
and reload matching browser assets after rollout. See the
[capture workflow](../guides/agent-sessions.md#capture-a-local-producer-with-mcp).

For rollback, preserve the original database and restore the compatible backup
into a new empty replacement database using the
[recovery procedure](../../docker.md#persistence-backups-and-recovery). Select
the matching previous release before serving it. An older archive restored in
place against the upgraded schema can conflict with the new session foreign
keys; do not bypass evidence guards to force that restore.

## Upgrading across V038

V038 removes task/subtask deadlines from storage and current REST, MCP and
frontend contracts. It also rejects the five obsolete due-key spellings,
including null; see [the API contract](../../api.md#removed-due-fields).
Removed values cannot be recovered from current task rows. Old server/MCP
writers with the new schema, or mixed old/new frontend contracts, are unsupported.
Fresh installations apply V038 through the normal ordered migration chain.

Use a maintenance window and one named deployment operator:

1. Select matched server, `hmem-ctl`, MCP and frontend artifacts from one release;
   retain the previous compatible artifacts for rollback. Stop or quiesce old
   server and MCP writers, including external stdio clients and automatic service
   restarts. Keep the configured PostgreSQL database running independently.
2. Take a restorable pre-upgrade database backup with the operator's configured
   PostgreSQL connection. Protect the backup and separately supplied secrets;
   verify backup success and restorability. Record its path, the named operator
   who owns it, and an explicit UTC expiry/disposition before proceeding (for
   example, deletion seven days after an accepted rollout). Honor existing backup
   obligations; any extension needs a new finite expiry and owner. The owner
   removes temporary copies and disposes of the retained copy at expiry.
   [Docker backup commands](../../docker.md#persistence-backups-and-recovery) cover the
   default Compose database. Native operators supply their configured connection
   and backup location; no database path or credential is assumed here.
3. Install the matched artifacts while writers remain stopped. From the matching
   native source checkout, run:

   ```bash
   hmem-ctl migrate --migrations-dir hmem-server/migrations
   ```

   This command uses the normal database configuration and requires PostgreSQL
   already running. Its default migration directory is the container path
   `/opt/hmem/migrations`, so native use must supply the matching directory.
   `hmem-ctl stop` also stops managed PostgreSQL: if used, restart PostgreSQL
   alone through your configured mechanism before backup/migration. Do not use
   old application startup to bring the database back. For Compose, use the
   [packaged migration procedure](#v038-maintenance-upgrade).
4. Require successful migration/version-38 registration before serving. If it
   fails, keep writers stopped, correct the reported cause, and retry; V038's
   schema changes, incompatible snapshot retirement and ledger entry roll back
   together. Already applied migrations are skipped. Start the matched server
   and MCP, refresh MCP tool discovery, and reload browsers with the matching
   frontend assets. Refused legacy snapshot/resume tokens need a fresh resync.
5. Check representative authorized due-free project/task/subtask create, update,
   detail and overview flows, completion/readiness and dependency effects, fresh
   snapshot recovery, and historical audit inspection/replay. Confirm obsolete
   keys are rejected and due-only replay shows its structured refusal.

Rollback requires stopping all writers, restoring the pre-upgrade database backup
with its required PostgreSQL extensions/configuration, and deploying the previous
compatible server/ctl/MCP/frontend artifacts before reopening access. There is no
automatic down migration. Restoring the backup also discards later database
writes, so the operator must account for them before restoring. Historical audit
records are evidence, not a substitute for that backup.

## V038 maintenance upgrade

Use the preparation, acceptance and rollback requirements above. Keep the previous
image under a distinct tag before rebuilding a reused tag. For standard Compose:

```bash
docker compose --profile mcp stop hmem mcp
# Take and verify the backup before continuing.
docker compose build hmem migrate mcp
docker compose run --rm migrate
docker compose --profile mcp up -d hmem mcp
```

Run the final command only after migration succeeds. Omit `mcp` from stop/build/
start if you do not use the sidecar. All three services use the selected image;
`migrate` dispatches `hmem-ctl migrate` against bundled `/opt/hmem/migrations`.
Server startup also migrates before serving. The standalone maintenance command
is `docker compose run --rm migrate`.

For rollback, follow [Docker restore](../../docker.md#persistence-backups-and-recovery)
through successful `pg_restore`, then select the previous compatible image before
restarting. Starting the new image would apply V038 again. The named backup owner
disposes of host and container copies at the recorded expiry.

## Historical pause normalization (V036)

Startup applies migrations before serving. V036 makes each
existing paused project an independent pause, pauses its eligible open descendants,
and blocks their unfinished tasks, including legacy running work. Done/cancelled
tasks, manual blocks, deleted rows, and frozen closed branches keep their history.
Deleted or frozen legacy pauses retain separate provenance until an authorized
restore, move, or ancestor reopening can materialize it. Ordinary new pause
requests still refuse running descendants. If migration fails, its data changes,
compatibility metadata, events, and version registration roll back together;
resolve the reported failure before retrying startup.
