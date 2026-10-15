# hmem

hmem stores repository observations, projects, and tasks in PostgreSQL, with a web
interface, HTTP API, and MCP tools.

## Native installation

For Windows or Linux, install Haskell Stack, PostgreSQL, and a supported Node.js
LTS release (20 or later) with npm. Put `stack`, `initdb`, `pg_ctl`, `createdb`,
`psql`, and `npm` on `PATH`. Run these commands from this repository's root:

```bash
stack install
stack run build-frontend -- --install
hmem-ctl
hmem-ctl start
hmem-ctl status
```

Run `stack path --local-bin` to find the executable directory and add it to `PATH`
before running `hmem-ctl`. Setup creates a local database and
`~/.hmem/config.yaml`, and registers services to start automatically. Use
`hmem-ctl init` instead of the
bare `hmem-ctl` command if you want to manage startup yourself.

If the frontend build reports that it was skipped, install Node.js/npm and rerun
the build command. The API can run without the web interface.

## Upgrading across V038

V038 removes task/subtask deadlines from storage and current REST, MCP and
frontend contracts. It also rejects the five obsolete due-key spellings,
including null; see [the API contract](api.md#removed-due-fields).
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
   [Docker backup commands](docker.md#persistence-backups-and-recovery) cover the
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
   [packaged migration procedure](docker.md#v038-maintenance-upgrade).
4. Require successful migration/version-38 registration before serving. If it
   fails, keep writers stopped, correct the reported cause, and retry; V038's
   schema changes, incompatible snapshot retirement and ledger entry roll back
   together. Already applied migrations are skipped. Start the matched server
   and MCP, refresh MCP tool discovery, and reload browsers with the matching
   frontend assets. Refused legacy snapshot/resume tokens need a fresh resync.
5. Check representative authorized due-free project/task/subtask create, update,
   detail and overview flows, completion/readiness and dependency effects, fresh
   snapshot recovery, and historical audit inspection/replay. Confirm obsolete
   keys are rejected and due-only replay shows its structured refusal. These are
   operator acceptance checks, not a claim that this deployment has been tested.

Rollback requires stopping all writers, restoring the pre-upgrade database backup
with its required PostgreSQL extensions/configuration, and deploying the previous
compatible server/ctl/MCP/frontend artifacts before reopening access. There is no
automatic down migration. Restoring the backup also discards later database
writes, so the operator must account for them before restoring. Historical audit
records are evidence, not a substitute for that backup.

## First use

Open [http://127.0.0.1:8420](http://127.0.0.1:8420). The native default allows
local access without a login. Keep it on loopback; configure
[authentication](auth.md) before enabling shared access.

From the repository you want to track, select or create its workspace:

```bash
hmem-ctl workspace
```

This writes `.hmem.workspace` in the current directory with the selected workspace
UUID. Use that workspace in the browser or pass its UUID to API and MCP calls.
See the [API guide](api.md) for creating observations, managing tasks, and handling
update conflicts.

Use `hmem-ctl status` to check services and `hmem-ctl stop` to stop them.
For container installation, follow the [Docker guide](docker.md), including its
required bearer token and browser sign-in step.

## MCP

Configure your MCP client to launch this command as a stdio server:

```bash
hmem-mcp --server-url http://127.0.0.1:8420
```

Keep hmem-server running. After the MCP initialization handshake, call
`set_workspace` with both required arguments before ordinary tools:

```json
{"workspace_id":null,"identity":{"role":"Developer","model-id":"codex:gpt-6.1-sol:high"}}
```

You can now use `workspace_list` or `workspace_register`. Call `set_workspace`
again with the selected UUID and the complete identity so scoped tools can omit
`workspace_id`. Explicit workspace IDs do not bypass identity declaration. The
context belongs to that MCP process; the bridge does not read `.hmem.workspace`
automatically. Repeated initialization preserves it; process exit clears it.
Older clients must upgrade their startup call. See the [MCP guide](hmem-mcp/README.md)
for selection, clearing, validation, and error examples.

Local bootstrap needs no token. Add `--no-auth` to explicitly suppress credential
forwarding. Declared role/model values are self-reported attribution and do not
change local superadmin access or server permissions.

For a server that requires a token, set `HMEM_MCP_AUTH_TOKEN` in the MCP process's
environment. Use a service/PAT token for shared installations; see
[authentication](auth.md#service-and-mcp-tokens). Use HTTPS when connecting to a shared server.

## Optional similarity search

Full-text search works without embeddings. Similarity search requires pgvector
and a caller-supplied 1536-dimensional query vector in the same space as the stored
vectors. Automatic vectorization is off by default; its optional GPU setup has
no CPU fallback. Follow [embeddings and similarity search](embeddings.md) to
enable pgvector, import vectors, or configure automatic generation.

## Guides

| Topic | Guide |
| --- | --- |
| Login, permissions, and tokens | [Authentication](auth.md) |
| Requests, updates, and errors | [API](api.md) |
| Containers, backups, and restore | [Docker](docker.md) |
| Vectors and optional GPU setup | [Embeddings](embeddings.md) |
| Current tables and relationships | [Database schema](database.md) |
| Building and testing the web interface | [Frontend development](hmem-server/frontend/README.md) |
