# Docker deployment

## Compose quick start

Install Docker with Compose v2. Run these steps from the repository directory.
The default stack serves the web UI and API locally, with PostgreSQL for storage
and automatic vectorization disabled.

1. Copy the two configuration templates:

   ```bash
   cp .env.example .env
   cp config/compose.env.example config/compose.env
   ```

2. Set a strong `POSTGRES_PASSWORD` in `.env`. Leave
   `HMEM_HTTP_BIND=127.0.0.1` for local use.

   In `config/compose.env`, uncomment and set `HMEM_AUTH_LOCAL_USER_TOKEN` to
   a strong browser credential. For automated API/MCP clients, also set a
   distinct `HMEM_API_KEY`. The local profile needs at least one credential.
   Keep secrets out of Git and stable across container replacements.
   A later change to `POSTGRES_PASSWORD` does not change the password already
   stored in an existing PostgreSQL volume.
3. Build and start:

   ```bash
   docker compose up --build -d
   docker compose ps
   docker compose logs -f postgres hmem
   ```

   hmem waits for PostgreSQL and applies migrations before serving. Wait until
   the hmem service is healthy.
4. Open `http://127.0.0.1:8420/#hmem_token=<your-HMEM_AUTH_LOCAL_USER_TOKEN>`, replacing the
   placeholder with the token you supplied. A healthy service still requires
   browser authentication. The frontend removes the fragment after processing;
   its default token storage is `localStorage`. Set
   `HMEM_FRONTEND_AUTH_TOKEN_STORAGE=session` or `memory` in the runtime env file
   if you prefer shorter-lived storage.

Automated API clients send `HMEM_API_KEY` as `Authorization: Bearer <token>`
and declare `X-HMem-Bot-Identity` as described in [authentication](auth.md).
Browser writes use the separate human credential.
For shared browser access, use deployed OIDC authentication below.

## Configuration and secrets

| File | Used for |
| --- | --- |
| `.env` | Compose interpolation: PostgreSQL user/database/password/image, hmem image tag, host bind/port, and runtime env-file path. |
| `config/compose.env` | Runtime settings passed to hmem, migration, and MCP containers. Change its path with `HMEM_ENV_FILE` in `.env`. |

Putting authentication values in `.env` alone does not pass them to hmem.
Compose supplies the database connection from its `environment` settings;
those override the same values in the runtime env file. After editing runtime
settings, recreate the service with `docker compose up -d hmem`.

For secrets mounted into containers, use `HMEM_AUTH_LOCAL_USER_TOKEN_FILE`, `HMEM_API_KEY_FILE`,
`HMEM_DB_PASSWORD_FILE`, and the OIDC/MCP `*_FILE` options in the
[configuration reference](config/container-runtime-contract.md).
Setting a file path does not mount the file: add the read-only secret/bind mount
to your deployment and make it readable by UID 10001. File values take precedence
over their matching environment values. Never put secrets in frontend runtime
configuration.

The port is published on host loopback by default. For remote access, configure
deployed authentication and HTTPS through your reverse proxy or the paired
`HMEM_TLS_CERT_FILE`/`HMEM_TLS_KEY_FILE` settings before changing
`HMEM_HTTP_BIND`. Keep CORS empty for the same-origin UI/API, or restrict it to
trusted frontend origins when they are hosted separately.

Direct `docker run` deployments need an external PostgreSQL connection, mounted
secrets, and writable temporary directories despite the read-only image.
Use the [container runtime reference](config/container-runtime-contract.md)
for the required mounts, ownership, limits, and environment settings.

## Shared OIDC authentication

Set these values in `config/compose.env`, replacing the examples:

```env
HMEM_AUTH_MODE=deployed
HMEM_AUTH_DEPLOYED_ISSUER=https://issuer.example
HMEM_AUTH_DEPLOYED_AUDIENCE=hmem-web
HMEM_AUTH_DEPLOYED_DISCOVERY_URL=https://issuer.example/.well-known/openid-configuration
HMEM_AUTH_DEPLOYED_CLIENT_ID=hmem-web
HMEM_AUTH_DEPLOYED_CLIENT_SECRET=replace-with-client-secret
HMEM_AUTH_DEPLOYED_REDIRECT_URI=https://hmem.example.com/api/v1/auth/callback
HMEM_AUTH_DEPLOYED_TOKEN_HASH_SECRET=replace-with-stable-secret
```

Prefer mounted `*_FILE` secrets for deployed credentials. Register the exact
callback URL with your provider and keep the token-hash secret stable. Start the
stack, then bootstrap its first administrator:

```bash
docker compose run --rm hmem hmem-ctl auth bootstrap-superadmin \
  --auth-subject oidc-subject-from-provider --display-name "Primary Operator"
```

See [authentication](auth.md) for provider subjects, permissions, browser login,
and token rotation/revocation. Run its other operator commands by prefixing them
with `docker compose run --rm hmem`.

Keep local bootstrap disabled in containers. hmem listens on `0.0.0.0` inside
the Compose network even when the host port is loopback-only; allowing remote
bootstrap exposes implicit superadmin access.

## Persistence, backups, and recovery

`postgres-data` stores application and authentication data. Normal
`docker compose down`, upgrades, and container replacements retain it.
Generated hmem configuration and server log files use temporary storage under
`/var/lib/hmem` and disappear when the container stops. Supply persistent
database and authentication secrets yourself.

**`docker compose down -v` deletes data volumes**, including `postgres-data`
and any declared legacy `hmem-data` volume. The legacy volume is otherwise
unmounted and untouched. Use plain `down` to stop the stack safely.

For the default database/user names, create a PostgreSQL custom-format backup
and copy it to the host:

```bash
docker compose exec -T postgres pg_dump -U hmem -d hmem \
  --format=custom --file=/tmp/hmem-backup.dump
docker compose cp postgres:/tmp/hmem-backup.dump ./hmem-backup.dump
```

Check both commands succeeded, then keep the backup outside container storage.
Use a fresh backup filename for each retained copy. It contains authentication
data; protect it along with the separately supplied secrets. This single-database
backup does not include PostgreSQL roles or configuration.

Restore into a distinctly named **new, empty replacement database**, keeping the
current database intact. Stop hmem, disconnect MCP clients and stop all other writers
first. For the default administrator role and a new name `hmem_recovered`:

```bash
docker compose stop hmem
docker compose cp ./hmem-backup.dump postgres:/tmp/hmem-backup.dump
docker compose exec -T postgres createdb -U hmem --owner=hmem \
  --template=template0 hmem_recovered
docker compose exec -T postgres pg_restore -U hmem -d hmem_recovered \
  --no-owner --exit-on-error --single-transaction /tmp/hmem-backup.dump
```

Choose a fresh replacement name if it already exists. Check every command
succeeds. Use a PostgreSQL image with the extensions required by the archive and
the application release matched to that backup. Before restarting, set
`POSTGRES_DB=hmem_recovered` in Compose's `.env` and update any mounted database
configuration to the same target. `compose.yaml` derives `HMEM_DB_NAME` from
`POSTGRES_DB`, overriding the runtime env file's value. Recreate the PostgreSQL
service with `docker compose up -d postgres`, then start hmem using the matched
release and reconnect MCP clients only after restore succeeds.
Substitute your configured role/database names as needed; keep the old database
until recovery is verified.

Do not restore an older archive in place with `--clean --if-exists`: that option
only drops objects covered by the archive. For example, pre-V041 archives do not
contain the session tables and their foreign keys, so restoring them into an
upgraded database can fail while dropping tasks or workspaces. The empty
replacement target avoids those dependencies without disabling evidence guards.
See PostgreSQL's [pg_dump](https://www.postgresql.org/docs/17/app-pgdump.html)
and [pg_restore](https://www.postgresql.org/docs/17/app-pgrestore.html) references
for other backup/restore needs.

For schema changes, use the [upgrade reference](docs/reference/upgrades.md),
including its matched-release and backup requirements.

## Optional vectors and GPU setup

The default `postgres:17-bookworm` image does not supply pgvector. To add vector
storage/search to the Compose database, set this in `.env` while retaining its
PostgreSQL 17 data volume:

```env
POSTGRES_IMAGE=pgvector/pgvector:pg17
```

Recreate the PostgreSQL service, then follow [embedding readiness](embeddings.md)
to inspect/enable the database capability. Adding pgvector does not generate
vectors or require deleting the volume.

Automatic GPU vectorization is opt-in. The qualified managed setup uses
Linux/WSL Docker, an NVIDIA RTX 5090 (compute capability 12.0, driver 596.49),
and the locked native TEI CUDA/F16 runtime. Other GPU/driver combinations need
their own validation; there is no CPU fallback.

Prefetch the model/runtime versions listed in the
[GPU reference](config/managed-embedding-gpu-contract.md). With Python and Stack
available, prepare the bundle from a clean committed checkout. Its output and
report must be new, outside the checkout, in the same existing private parent:

```bash
python docker/prepare-managed-gpu.py --root . \
  --model-root /path/to/prefetched-model --runtime-root /path/to/prefetched-tei-runtime \
  --output /private/new-managed-bundle --report /private/new-bundle-report.json
```

Preparation verifies/copies local inputs; it does not download them.
Set `HMEM_MANAGED_BUNDLE_CONTEXT=/private/new-managed-bundle` in `.env`,
ensure pgvector is ready, then use the GPU override:

```bash
docker compose -f compose.yaml -f compose.gpu.yaml up --build -d
docker compose -f compose.yaml -f compose.gpu.yaml ps
```

Use both Compose files for later GPU-stack commands too. A healthy API does not
mean vectorization is ready: check hmem logs for provider validation failures.
Missing pgvector, GPU, or locked assets leaves automatic vectors unavailable
while ordinary hmem operations remain usable. The application port is the only
published port.

For a separately operated validated GPU endpoint, use the HTTP provider settings
in the [runtime reference](config/container-runtime-contract.md). Input-size,
timeout, and compatibility limits are listed there. See [embeddings](embeddings.md)
for manual imports and matching query vectors.

## MCP stdio client

Set `HMEM_MCP_AUTH_TOKEN` in the runtime env file to a deployed service token or,
for a personal installation, the same value as `HMEM_API_KEY`. You can also use
`HMEM_MCP_AUTH_TOKEN_FILE` with a secret file mounted in the `hmem` service.
Start the stack and wait for `hmem` to become healthy, then configure each MCP
client to run this stdio command from the Compose project directory (using the
same Compose files and project options as the running stack):

```bash
docker compose exec -T hmem hmem-entrypoint hmem-mcp
```

Each client gets an independent MCP process inside the existing `hmem` container.
Workspace, identity and transcript upload state belong to that process. The
explicit `hmem-entrypoint` loads MCP secret files without regenerating server
configuration, running migrations or changing frontend assets.

The client exchanges JSON-RPC over stdin/stdout; `-T` keeps the stream free of
terminal formatting. Closing stdin drains that bridge's accepted calls and exits
without stopping the service or other clients. Keep it attached to its client.
Restart or reconnect MCP clients after replacing or restarting the service.
The previous `docker compose run --rm -T mcp` command created a new container for
each client; replace it with the command above.

The default upstream is `http://127.0.0.1:8420`. For a TLS-enabled hmem service, set
`HMEM_MCP_SERVER_URL` in Compose's `.env` to an HTTPS URL whose hostname matches
the server certificate. The `hmem` container must trust that certificate's CA.
Recreate the service after changing its environment. For local transcript
uploads, mount the required host transcript directory read-only in `hmem` and
pass the path as seen inside the container; host paths are not visible by default.
Credential choices and precedence are in [authentication](auth.md); the
[README setup](README.md) also supports clients against the published server.

## Troubleshooting

| Problem | Check |
| --- | --- |
| Missing local bearer token | Supply the human browser token or automated API key in the runtime env file, or mount its matching `*_FILE` secret. |
| PostgreSQL won't start/connect | Password, service health, configured database/user, and SSL mode. An existing volume keeps its old role password. |
| Migration/schema failure | `docker compose run --rm migrate`; check the selected database and migration output before restarting. |
| Unhealthy hmem | `docker compose logs hmem`; check DB/migrations and token-file readability. Any explicit healthcheck token must be accepted by the server; otherwise it uses the API credential, then the human credential. |
| Browser unauthorized | Open the quick-start token fragment URL, or check deployed login using [authentication](auth.md). |
| Blank page/404 | `HMEM_WEB_ENABLED=true`, `HMEM_WEB_STATIC_DIR=/run/hmem/static`, and reverse-proxy browser paths. |
| pgvector unavailable | Follow [embedding readiness](embeddings.md); this is expected with the default PostgreSQL image. |
| GPU vectors unavailable | Confirm the GPU override, device access, verified bundle, pgvector readiness, and provider errors in hmem logs. |

Container startup/migration output is available with `docker compose logs hmem`.
For the rotating application log while hmem is running:

```bash
docker compose exec hmem tail -f /var/lib/hmem/.hmem/logs/hmem-server.log
```
