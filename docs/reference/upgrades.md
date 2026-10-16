# Upgrade reference

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
