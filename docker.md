# Docker deployment

## Compose quick start

Install Docker with Compose v2. Run these steps from the repository directory.
The default stack serves the web UI and API locally, with PostgreSQL for storage
and automatic vectorization disabled.
For image and styled HTML previews, use the [recommended preview setup](#compose-setup-with-document-previews)
with a native hmem server and Compose-managed PostgreSQL.

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

Document bearer links require explicit reachable public origins in
`document_delivery`; they are never derived from Host headers. Styled HTML also
requires a dedicated cookieless origin outside every application/proxy auth
cookie scope and an explicit complete proxy-cookie-domain declaration. Route
that virtual host only to the dedicated loopback preview listener, with HTTPS
termination in the same host/network namespace. An ordinary separate Compose
container cannot reach another container's loopback listener; arrange that
network boundary explicitly. The default Compose setup does not enable styled
preview isolation automatically.

Keep queries and credential headers out of all proxy/access/tracing logs: use
query-free `$uri`, not `$request_uri`, `$request` or `$args`, in an Nginx access
format. The application request logger records query-free paths; this does not
certify upstream operator-controlled layers. See the
[document capability deployment settings and log example](docs/reference/document-capabilities.md#configuration).
Do not forward application Cookie/Authorization headers to the preview virtual
host or install browser authentication there. It sets no cookies and serves no
application/API/static routes. Preserve the original public Host authority and
port: the preview listener rejects aliases, application/download hosts, missing
or duplicate Host, and cannot authorize from forwarded-host headers. Its
configured rate admission also covers public bearer reads. Restart invalidates outstanding grant epochs;
the PostgreSQL document originals, evidence and grant audit records remain.

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

## Compose setup with document previews

For previews, run `hmem-server` on the trusted host and PostgreSQL in Compose.
The [native configuration example](config/compose-preview.yaml.example) enables
the worker and a dedicated styled HTML listener by default. The all-container
quick start has no host launcher and keeps originals downloadable when previews
are unavailable.

1. Install the native hmem binaries and build the frontend in local auth mode.
   This lets the browser accept the sign-in fragment in step 5. Keep the
   repository as the working directory when starting the native server.

   ```bash
   stack install
   VITE_HMEM_AUTH_MODE=local stack run build-frontend
   ```
2. Set `HMEM_CONFIG_DIR` to a private, absolute directory on the host. Copy
   `config/compose-preview.yaml.example` to `config.yaml` and `.env.example` to
   `compose.env` inside that directory. Set a strong `POSTGRES_PASSWORD` in
   the private `compose.env` file, and set `HMEM_DB_PASSWORD` in the host shell
   to the same value. Set `HMEM_AUTH_LOCAL_USER_TOKEN` in the host shell to a
   separate strong browser token. Keep both secrets out of Git. If you change
   `POSTGRES_DB`, `POSTGRES_USER`, or `HMEM_PREVIEW_DB_PORT` in `compose.env`,
   match the corresponding database settings in `config.yaml`. The overlay
   publishes PostgreSQL only on `127.0.0.1`, at port `55432` by default.
3. Build and [qualify the local worker image](docs/reference/document-preview-worker.md#validation).
   Set `document_previews.image_id` in `config.yaml` to the resulting immutable
   image ID. Set `python_executable` and `launcher_path` to absolute paths to
   host Python and this checkout's `hmem-preview-worker/launch.py`. The host
   account running hmem must have access to the local Docker runtime.
4. Keep the local authentication mode and loopback bindings in the example.
   The app and downloads use `http://localhost:8420`. The cookieless HTML listener
   uses `http://127.0.0.1:8421`. The two hostnames keep app cookies off the
   preview origin. The example declares no upstream authentication cookies.
   If you change the app port, update `server.port`, `application_origin`, and
   `download_origin` together. If you change the preview port, update
   `preview_origin` and `preview_listen_port` together. See the
   [document delivery reference](docs/reference/document-capabilities.md#configuration)
   for deployments with HTTPS or a reverse proxy.
5. Start the database in a separate Compose project, migrate it, and run the
   native server in the same shell.

   ```bash
   docker compose -p hmem-preview --env-file "$HMEM_CONFIG_DIR/compose.env" \
     -f compose.yaml -f compose.preview.yaml up -d --wait postgres
   hmem-ctl migrate --migrations-dir hmem-server/migrations
   hmem-server
   ```

   Open `http://localhost:8420/#hmem_token=<your-HMEM_AUTH_LOCAL_USER_TOKEN>`,
   using the configured app port and token. Use the exact `application_origin`
   hostname in the browser so the HTML listener allows the app to embed previews.
   hmem reads these settings at startup. To requeue a failed generation after
   the worker is available, open the attachment and select **Retry preview**.

The host launcher supervises the confined decoder. The decoder receives no
application credentials, host mounts, or network access. Keep Docker authority
on the trusted host. Missing launcher, image, or confinement support leaves the
original downloadable. See the [preview runtime reference](docs/reference/document-previews.md)
for configuration, failure handling, and recovery.
