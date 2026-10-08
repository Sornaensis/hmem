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

   In `config/compose.env`, uncomment and set `HMEM_API_KEY` to a strong
   bearer token. The default local profile requires it before startup.
   Keep both secrets out of Git and stable across container replacements.
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
4. Open `http://127.0.0.1:8420/#hmem_token=<your-HMEM_API_KEY>`, replacing the
   placeholder with the token you supplied. A healthy service still requires
   browser authentication. The frontend removes the fragment after processing;
   its default token storage is `localStorage`. Set
   `HMEM_FRONTEND_AUTH_TOKEN_STORAGE=session` or `memory` in the runtime env file
   if you prefer shorter-lived storage.

API clients send the same token as `Authorization: Bearer <token>`.
For shared browser access, use deployed OIDC authentication below.

## Configuration and secrets

| File | Used for |
| --- | --- |
| `.env` | Compose interpolation: PostgreSQL user/database/password/image, hmem image tag, host bind/port, and runtime env-file path. |
| `config/compose.env` | Runtime settings passed to hmem, migration, and MCP containers. Change its path with `HMEM_ENV_FILE` in `.env`. |

Putting `HMEM_API_KEY` in `.env` alone does not pass it to hmem.
Compose supplies the database connection from its `environment` settings;
those override the same values in the runtime env file. After editing runtime
settings, recreate the service with `docker compose up -d hmem`.

For secrets mounted into containers, use `HMEM_API_KEY_FILE`,
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

To replace the database contents with that backup, stop hmem and any other
writers. The following restore drops/recreates objects covered by the backup:

```bash
docker compose stop hmem
docker compose cp ./hmem-backup.dump postgres:/tmp/hmem-backup.dump
docker compose exec -T postgres pg_restore -U hmem -d hmem \
  --clean --if-exists --no-owner --exit-on-error --single-transaction /tmp/hmem-backup.dump
docker compose up -d hmem
```

Restart hmem only after restore succeeds. Substitute your configured database
and user names if they differ, and use a PostgreSQL image with any extensions
required by the backup. See PostgreSQL's [pg_dump](https://www.postgresql.org/docs/17/app-pgdump.html)
and [pg_restore](https://www.postgresql.org/docs/17/app-pgrestore.html) references
for other backup/restore needs.

To run bundled migrations separately during maintenance:

```bash
docker compose run --rm migrate
```

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

## MCP sidecar

Set `HMEM_MCP_AUTH_TOKEN` in the runtime env file to a deployed service token or,
for a personal installation, the same value as `HMEM_API_KEY`. You can also use
`HMEM_MCP_AUTH_TOKEN_FILE` with a mounted secret file. Then start the optional profile:

```bash
docker compose --profile mcp up --build -d mcp
docker compose logs -f mcp
```

For an interactive stdio MCP client, use [the README setup](README.md) against
the published hmem server. Credential choices and precedence are in
[authentication](auth.md).

## Troubleshooting

| Problem | Check |
| --- | --- |
| Missing local bearer token | Supply `HMEM_API_KEY` in the runtime env file or mount `HMEM_API_KEY_FILE`. |
| PostgreSQL won't start/connect | Password, service health, configured database/user, and SSL mode. An existing volume keeps its old role password. |
| Migration/schema failure | `docker compose run --rm migrate`; check the selected database and migration output before restarting. |
| Unhealthy hmem | `docker compose logs hmem`; check DB/migrations and token-file readability. Any explicit healthcheck token must be accepted by the server; otherwise let it use `HMEM_API_KEY`. |
| Browser unauthorized | Open the quick-start token fragment URL, or check deployed login using [authentication](auth.md). |
| Blank page/404 | `HMEM_WEB_ENABLED=true`, `HMEM_WEB_STATIC_DIR=/run/hmem/static`, and reverse-proxy browser paths. |
| pgvector unavailable | Follow [embedding readiness](embeddings.md); this is expected with the default PostgreSQL image. |
| GPU vectors unavailable | Confirm the GPU override, device access, verified bundle, pgvector readiness, and provider errors in hmem logs. |

Container startup/migration output is available with `docker compose logs hmem`.
For the rotating application log while hmem is running:

```bash
docker compose exec hmem tail -f /var/lib/hmem/.hmem/logs/hmem-server.log
```
