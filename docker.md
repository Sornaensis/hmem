# Docker deployment

This guide covers the production Docker image and the `compose.yaml` stack. It is
for operators who want hmem, PostgreSQL, the packaged web UI, and optional MCP in
containers.

Do not commit real secrets. Copy the example files, keep secrets in your local
`.env`, a secret manager, or mounted `*_FILE` paths, and commit only placeholder
values.

## Image and runtime layout

Build the default local image tag:

```bash
docker build -t hmem:local .
```

The Compose stack uses the same tag by default via `${HMEM_IMAGE:-hmem:local}`.
Set `HMEM_IMAGE=registry.example/hmem:tag` in `.env` when you want Compose to use
or publish a different tag.

Runtime defaults in the image:

- Exposes container port `8420`.
- Runs as the non-root `hmem` user.
- Uses `/var/lib/hmem` for `HOME`, generated config, state, and logs.
- Writes server logs below `/var/lib/hmem/.hmem/logs`.
- Serves packaged frontend assets from `/opt/hmem/static` when
  `HMEM_WEB_ENABLED=true`.
- Bundles SQL migrations in `/opt/hmem/migrations`.
- Uses `/usr/local/bin/hmem-entrypoint` as `ENTRYPOINT` and `hmem-server` as the
  default command.
- Includes `/usr/local/bin/hmem-healthcheck`, which probes
  `/api/v1/health` and requires JSON `status: "ok"`.

For direct `docker run` deployments, copy
`config/container-runtime.env.example` to `config/container-runtime.env`, point
its `*_FILE` settings at mounted secrets, supply an external PostgreSQL
connection, and mount a persistent state volume:

```bash
docker run --rm --name hmem \
  --env-file config/container-runtime.env \
  --mount type=bind,src=/path/to/secrets/hmem_db_password,dst=/run/secrets/hmem_db_password,readonly \
  --mount type=bind,src=/path/to/secrets/hmem_api_key,dst=/run/secrets/hmem_api_key,readonly \
  -e HMEM_DB_HOST=host.docker.internal \
  -e HMEM_DB_PASSWORD_FILE=/run/secrets/hmem_db_password \
  -e HMEM_API_KEY_FILE=/run/secrets/hmem_api_key \
  -p 127.0.0.1:8420:8420 \
  -v hmem-data:/var/lib/hmem \
  hmem:local
```

The direct container profile is useful when PostgreSQL is managed elsewhere. The
Compose workflow below is the recommended local/self-contained path.

## Docker Compose quick start

The Compose stack starts PostgreSQL, hmem, an optional migration helper, and an
optional MCP sidecar.

1. Copy the interpolation values and set local-only secrets:

   ```bash
   cp .env.example .env
   ```

   Edit `.env` and set at least `POSTGRES_PASSWORD`. Leave the default
   `HMEM_HTTP_BIND=127.0.0.1` unless the deployment already has safe auth and a
   trusted network boundary.

2. Copy the hmem runtime env file:

   ```bash
   cp config/compose.env.example config/compose.env
   ```

   Edit `config/compose.env` and choose exactly one safe auth profile. The
   default profile is local static bearer auth and intentionally fails fast until
   `HMEM_API_KEY` or `HMEM_API_KEY_FILE` is supplied.

3. Build and start:

   ```bash
   docker compose up --build
   ```

   Detached mode is also fine:

   ```bash
   docker compose up --build -d
   docker compose logs -f hmem
   ```

Expected defaults:

- Image: `hmem:local` unless `HMEM_IMAGE` is set.
- PostgreSQL image: `postgres:17-bookworm` unless `POSTGRES_IMAGE` is set.
- Published HTTP/API/web port: `127.0.0.1:8420 -> hmem:8420` unless
  `HMEM_HTTP_BIND` or `HMEM_HTTP_PORT` is set.
- Data volumes: logical Compose volumes `postgres-data` for
  `/var/lib/postgresql/data` and `hmem-data` for `/var/lib/hmem`.
- Runtime env file: `${HMEM_ENV_FILE:-./config/compose.env}`.

Open <http://127.0.0.1:8420/> after the `hmem` service is healthy. If you use the default local static-bearer profile, the browser still needs an accepted bearer token; see [Local static bearer auth](#local-static-bearer-auth).

Useful status and log commands:

```bash
docker compose ps
docker compose logs -f postgres hmem
docker compose logs migrate
```

Cleanup commands:

```bash
# Stop and remove containers/networks; keep named volumes and data.
docker compose down

# Remove containers/networks and the postgres-data/hmem-data volumes.
# This deletes the database and /var/lib/hmem state/logs.
docker compose down -v

# Remove the default local image tag after the stack is down.
docker image rm hmem:local
```

## Environment files

Compose reads two files by default:

- `.env` is consumed by Docker Compose for interpolation in `compose.yaml`.
  It should not be committed with real secrets.
- `config/compose.env` is passed to the `hmem`, `migrate`, and `mcp` services as
  runtime environment. Copy it from `config/compose.env.example` and keep real
  secrets out of git.

`config/container-runtime.env.example` is the fuller direct-container contract.
See also `config/container-runtime-contract.md` for validation rules and exact
YAML mappings.

### `.env` reference

| Variable | Purpose | Secret? |
| --- | --- | --- |
| `POSTGRES_DB` | Database created by the postgres container and used by hmem. | No |
| `POSTGRES_USER` | Database role created by the postgres container and used by hmem. | No |
| `POSTGRES_PASSWORD` | Database role password. Official postgres refuses an empty value. | **Yes** |
| `HMEM_DB_SSLMODE` | libpq SSL mode for hmem's DB connection. Default `disable` for the Compose network. | No |
| `HMEM_IMAGE` | Image tag built/used by the hmem services. Default `hmem:local`. | No |
| `HMEM_HTTP_BIND` | Host address for published port `8420`. Default `127.0.0.1`. | No |
| `HMEM_HTTP_PORT` | Host port published to container port `8420`. Default `8420`. | No |
| `POSTGRES_IMAGE` | PostgreSQL image. Use `pgvector/pgvector:pg17` for pgvector support. | No |
| `HMEM_ENV_FILE` | Path to the hmem runtime env file. Default `./config/compose.env`. | No |

### `config/compose.env` runtime reference

Database settings are injected by `compose.yaml` from `.env` for the app and
migration services: `HMEM_DB_HOST=postgres`, `HMEM_DB_PORT=5432`,
`HMEM_DB_NAME`, `HMEM_DB_USER`, `HMEM_DB_PASSWORD`, and `HMEM_DB_SSLMODE`.
Prefer `HMEM_DB_PASSWORD_FILE` with a mounted secret when your orchestrator
supports it.

Server, pool, logging, CORS, and web settings:

| Variable | Purpose | Secret? |
| --- | --- | --- |
| `HOME` | Must remain `/var/lib/hmem` in the image. | No |
| `HMEM_SERVER_HOST` | Container listen address. Compose sets `0.0.0.0`. | No |
| `HMEM_SERVER_PORT` | Container listen port. Compose sets `8420`. | No |
| `HMEM_POOL_SIZE` | Database pool size. | No |
| `HMEM_POOL_IDLE_TIMEOUT` | Pool idle timeout in seconds. | No |
| `HMEM_POOL_STATEMENT_TIMEOUT_MS` | Statement timeout in milliseconds. | No |
| `HMEM_LOG_LEVEL` | Server log level such as `info` or `debug`. | No |
| `HMEM_LOG_MAX_SIZE_MB` | Rotating log size limit. | No |
| `HMEM_LOG_BACKUP_COUNT` | Rotating log backup count. | No |
| `HMEM_CORS_ALLOWED_ORIGINS` | Comma-separated trusted browser origins. Leave empty for same-origin UI/API. | No |
| `HMEM_WEB_ENABLED` | Serve packaged frontend static assets. | No |
| `HMEM_WEB_STATIC_DIR` | Static asset directory. Default `/opt/hmem/static`. | No |

Auth settings:

| Variable | Purpose | Secret? |
| --- | --- | --- |
| `HMEM_AUTH_MODE` | `local` for static bearer/single-operator use, `deployed` for shared OIDC/session deployments. | No |
| `HMEM_AUTH_ENABLED` | Enables legacy local static bearer auth when `HMEM_AUTH_MODE=local`. | No |
| `HMEM_API_KEY` / `HMEM_API_KEY_FILE` | Static bearer token for local mode. Prefer `_FILE`. | **Yes** |
| `HMEM_AUTH_LOCAL_BOOTSTRAP_ENABLED` | Dev-only implicit local superadmin bootstrap. Keep `false` for containers. | No |
| `HMEM_AUTH_LOCAL_ALLOW_REMOTE_BOOTSTRAP` | Dangerous dev-only override for remote bootstrap exposure. | No |
| `HMEM_AUTH_DEPLOYED_ISSUER` | OIDC issuer for deployed auth. | No |
| `HMEM_AUTH_DEPLOYED_AUDIENCE` | OIDC audience expected by hmem. | No |
| `HMEM_AUTH_DEPLOYED_DISCOVERY_URL` / `HMEM_AUTH_DEPLOYED_JWKS_URL` / `HMEM_AUTH_DEPLOYED_JWKS_FILE` | Provider metadata/JWKS source. | `JWKS_FILE` can be sensitive in some deployments |
| `HMEM_AUTH_DEPLOYED_TOKEN_HASH_SECRET` / `_FILE` | HMAC secret for newly issued service/PAT token hashes. Prefer `_FILE`. | **Yes** |
| `HMEM_AUTH_DEPLOYED_CLIENT_ID` | OIDC browser client ID. | Usually no |
| `HMEM_AUTH_DEPLOYED_CLIENT_SECRET` / `_FILE` | OIDC browser client secret. Prefer `_FILE`. | **Yes** |
| `HMEM_AUTH_DEPLOYED_REDIRECT_URI` | OIDC callback URL, for example `https://hmem.example.com/api/v1/auth/callback`. | No |
| `HMEM_AUTH_DEPLOYED_SCOPES` | OIDC scopes; must include `openid`. | No |
| `HMEM_AUTH_DEPLOYED_SESSION_*` and `HMEM_AUTH_DEPLOYED_COOKIE_*` | Cookie/session/CSRF names and policy. | No |

Frontend runtime settings are non-secret only. The entrypoint may generate
`/opt/hmem/static/hmem-runtime-config.js` from these values before serving the
static frontend. Never put bearer tokens, database passwords, OIDC client
secrets, PATs, or MCP tokens into frontend runtime config.

| Variable | Purpose | Secret? |
| --- | --- | --- |
| `HMEM_FRONTEND_API_URL` | Public API origin. Defaults to the browser origin. | No |
| `HMEM_FRONTEND_WS_URL` | Public WebSocket URL. Defaults from the API URL. | No |
| `HMEM_FRONTEND_AUTH_MODE` | Frontend auth mode when it must differ from `HMEM_AUTH_MODE`. | No |
| `HMEM_FRONTEND_LOGIN_URL` / `HMEM_FRONTEND_LOGOUT_URL` | Browser login/logout endpoints for deployed OIDC. | No |
| `HMEM_FRONTEND_LOGOUT_REDIRECT_URL` | Post-logout redirect path/URL. | No |
| `HMEM_FRONTEND_SERVER_SIDE_OIDC_LOGIN` | Enables server-side authorization-code login. | No |
| `HMEM_FRONTEND_CSRF_COOKIE_NAME` / `HMEM_FRONTEND_CSRF_HEADER_NAME` | Must match deployed auth CSRF settings when customized. | No |
| `HMEM_FRONTEND_AUTH_TOKEN_STORAGE` | Explicit bearer fallback storage: `local`, `session`, or `memory`. | No |
| `HMEM_FRONTEND_AUTH_TOKEN_STORAGE_KEY` | Browser storage key name. | No |
| `HMEM_FRONTEND_AUTH_TOKEN_URL_PARAMS` | URL fragment parameter names for explicit bearer fallback flows. | No |
| `HMEM_FRONTEND_REQUIRE_AUTH_STATE` | Require OIDC state checks. Keep `true` for provider redirects. | No |

MCP settings:

| Variable | Purpose | Secret? |
| --- | --- | --- |
| `HMEM_SERVER_URL` | URL used by `hmem-mcp`. The Compose `mcp` service sets `http://hmem:8420`. | No |
| `HMEM_MCP_AUTH_TOKEN` / `_FILE` | Preferred bearer token for MCP. Prefer `_FILE`. | **Yes** |
| `HMEM_AUTH_TOKEN` / `_FILE` | Fallback bearer token used by MCP clients. Prefer `_FILE`. | **Yes** |

## Auth workflows

### Shared deployments: OIDC/session auth

Use deployed mode for any shared or remotely reachable installation:

```env
HMEM_AUTH_MODE=deployed
HMEM_AUTH_DEPLOYED_ISSUER=https://issuer.example
HMEM_AUTH_DEPLOYED_AUDIENCE=hmem-web
HMEM_AUTH_DEPLOYED_DISCOVERY_URL=https://issuer.example/.well-known/openid-configuration
HMEM_AUTH_DEPLOYED_CLIENT_ID=hmem-web
HMEM_AUTH_DEPLOYED_CLIENT_SECRET_FILE=/run/secrets/hmem_oidc_client_secret
HMEM_AUTH_DEPLOYED_REDIRECT_URI=https://hmem.example.com/api/v1/auth/callback
HMEM_AUTH_DEPLOYED_TOKEN_HASH_SECRET_FILE=/run/secrets/hmem_token_hash_secret
HMEM_AUTH_DEPLOYED_COOKIE_SECURE=true
```

After migrations have run, bootstrap the first deployed superadmin explicitly
from inside the container:

```bash
docker compose run --rm hmem hmem-ctl auth bootstrap-superadmin \
  --auth-subject oidc-subject-from-provider \
  --display-name "Primary Operator" \
  --email operator@example.com
```

The bootstrap command is idempotent for the same `--auth-subject`. If another
superadmin already exists, use `--force` only as an audited break-glass action.
Verify the browser login and `/api/v1/session` as that provider subject before
production traffic.

To create a grant-bearing service user and issue a display-once service/PAT
token:

```bash
docker compose run --rm hmem hmem-ctl auth users upsert \
  --auth-subject service:mcp-sidecar \
  --display-name "MCP sidecar" \
  --can-create-workspace

# Use the user UUID reported by the upsert command or the admin API.
docker compose run --rm hmem hmem-ctl auth tokens issue \
  --grant-user-id user-uuid-with-required-permissions \
  --actor-label mcp-sidecar \
  --expires-at <future-UTC-expiry>
```

Store the raw token immediately in a secret manager or mounted secret file. hmem
prints issued tokens once and stores token hashes in the database.

### Local static bearer auth

Use local static bearer mode only for single-operator local deployments and
trusted automation:

```env
HMEM_AUTH_MODE=local
HMEM_AUTH_ENABLED=true
HMEM_AUTH_LOCAL_BOOTSTRAP_ENABLED=false
HMEM_AUTH_LOCAL_ALLOW_REMOTE_BOOTSTRAP=false
HMEM_API_KEY_FILE=/run/secrets/hmem_api_key
```

API and MCP clients send the token as `Authorization: Bearer <token>`. Keep the default
`HMEM_HTTP_BIND=127.0.0.1`, or set a strong token and a trusted network boundary
before publishing the port remotely.

For local browser testing, do not put the bearer token into frontend runtime
config. Use a URL fragment parameter named by `HMEM_FRONTEND_AUTH_TOKEN_URL_PARAMS`,
for example `http://127.0.0.1:8420/#hmem_token=<token>`. The frontend strips the
fragment after processing and stores the token according to
`HMEM_FRONTEND_AUTH_TOKEN_STORAGE`; use `session` or `memory` instead of `local`
when you do not want cross-tab persistence. Use deployed OIDC/session auth for
shared browser deployments.

### Dev-only local bootstrap escape hatch

Implicit local superadmin bootstrap is unsafe for normal containers. The Compose
service listens on `0.0.0.0` inside the container, so enabling local bootstrap in
Compose will fail unless `HMEM_AUTH_LOCAL_ALLOW_REMOTE_BOOTSTRAP=true` is also
set. That override may expose the implicit local superadmin to remote clients and
must be limited to isolated development environments.

Prefer local static bearer mode for local containers and deployed OIDC for shared
use.

## Migrations and admin commands

When the `hmem` service starts, the entrypoint:

1. Reads secret files and validates runtime env.
2. Generates `/var/lib/hmem/.hmem/config.yaml` with mode `0600`.
3. Runs `hmem-ctl migrate --migrations-dir /opt/hmem/migrations` with database
   connection retries.
4. Starts `hmem-server` only after migrations complete.

Migrations are idempotent. Applied versions are recorded in `schema_migrations`,
and later runs report `No pending migrations.` when the database is current.

Run migrations manually before a change window or while the app is stopped:

```bash
docker compose run --rm migrate
# or the equivalent hmem-ctl path:
docker compose run --rm hmem hmem-ctl migrate --migrations-dir /opt/hmem/migrations
```

The `migrate` service is assigned the `admin` profile for `up` workflows. You can
also run it with an explicit profile:

```bash
docker compose --profile admin run --rm migrate
```

If migration validation fails, check the `postgres` and one-shot `migrate` logs
before restarting the app.

## pgvector optionality

The default `postgres:17-bookworm` image does not provide the pgvector package.
hmem remains usable without it: Observations, full-text search, tasks, projects,
auth, REST/MCP non-vector operations, and the web UI continue to work. Embedding
and similarity operations report that the optional capability is unavailable.

The PostgreSQL package/control file and the extension/schema in an individual
database are separate layers. To use similarity with the Compose database,
select a PostgreSQL image that contains pgvector for the same PostgreSQL major
version, then start or recreate the `postgres` service while retaining its
volume. For the example Compose version:

```env
POSTGRES_IMAGE=pgvector/pgvector:pg17
```

Inspect the configured database before making a change:

```bash
docker compose up -d postgres
docker compose run --rm hmem hmem-ctl pgvector status
```

A not-ready status exits 2 and provides a `next action`. If the package is
available and the reported state is safely provisionable, back up a production
database, schedule a change window, and run:

```bash
docker compose run --rm hmem hmem-ctl pgvector enable
docker compose run --rm hmem hmem-ctl pgvector status
```

`enable` uses the database selected by the normal hmem container configuration.
It cannot install pgvector into a PostgreSQL image or host, start/stop an
externally managed server, or replay hmem migrations. It installs the database
extension when needed and atomically adds and verifies the nullable
`vector(1536)` Observation column and exact HNSW cosine index. A repeat run is a
verified no-op. Its regular schema and index creation can block Observation
writes until the transaction commits, so size the maintenance window for the
existing table and load.

The configured database role must be permitted to create the extension and
alter `public.observations`. Incompatible existing extensions, tables, columns,
or indexes are reported as drift and are not dropped or rewritten. Review the
reported object and PostgreSQL logs, repair it under normal operator change
control, then rerun status/enable. A failed or refused transaction commits no
partial hmem pgvector changes.

For externally managed PostgreSQL, its administrator owns package installation,
server lifecycle, backups, and database privileges. Point hmem's container
configuration at that database and run the same `hmem-ctl pgvector` commands in
the hmem container; they do not invoke native process management. Native Windows
installations likewise operate on the database selected by normal hmem
configuration.

pgvector stores, indexes, and compares vectors; it does not make embeddings, and
hmem does not invoke a model. See
[pgvector and embedding operations](database.md#pgvector-and-embedding-operations)
for the external-producer, NDJSON backfill/refresh, REST, MCP, and model-change
workflow.

## Healthchecks, logs, and persistence

The Compose `hmem` healthcheck executes `/usr/local/bin/hmem-healthcheck` every
30 seconds. It calls `/api/v1/health` on the container-local server and expects a
JSON response with `status: "ok"`.

Token behavior:

- In local static bearer mode, the server accepts the configured `HMEM_API_KEY`
  / `HMEM_API_KEY_FILE` value or configured local bot tokens. The simplest
  healthcheck setup is to omit healthcheck-specific token variables and let the
  helper fall back to `HMEM_API_KEY` / `HMEM_API_KEY_FILE`. If you set
  `HMEM_HEALTHCHECK_BEARER_TOKEN` or `HMEM_HEALTHCHECK_BEARER_TOKEN_FILE`, it
  must contain the same accepted bearer value; a separate health-only token will
  make the service unhealthy.
- In deployed mode, the health endpoint is unauthenticated and no health token is
  required.

Logs are available in two places:

- Container stdout/stderr through `docker compose logs hmem` for entrypoint,
  migration, and process output.
- Rotating server logs under `/var/lib/hmem/.hmem/logs`, persisted by the
  `hmem-data` volume.

Follow the server log file inside a running container when you need app/request
logger output:

```bash
docker compose exec hmem tail -f /var/lib/hmem/.hmem/logs/hmem-server.log
```

Data persistence:

- `postgres-data` stores the PostgreSQL data directory. Keep this volume for
  normal upgrades/restarts.
- `hmem-data` stores generated config, runtime state, and logs.
- `docker compose down` preserves both volumes.
- `docker compose down -v` deletes both volumes and all hmem data.

## Frontend static serving

The image builds the Elm/Vite frontend and copies static assets to
`/opt/hmem/static`. With `HMEM_WEB_ENABLED=true`, hmem serves those assets from
the same origin as the API. Same-origin deployments should leave
`HMEM_CORS_ALLOWED_ORIGINS` empty.

At startup, the entrypoint can generate
`/opt/hmem/static/hmem-runtime-config.js` with non-secret runtime values such as
public API URLs, login/logout URLs, CSRF names, and auth mode. Do not inject
secrets into frontend runtime config.

Set `HMEM_WEB_ENABLED=false` only when another frontend or reverse proxy serves
all browser assets.

## MCP sidecar profile

The optional `mcp` service is behind the `mcp` profile and connects to
`http://hmem:8420` on the Compose network.

1. Issue a least-privilege deployed service/PAT token, or create a strong local
   bearer token for single-operator local mode.
2. Store it in `config/compose.env` as `HMEM_MCP_AUTH_TOKEN_FILE` (preferred) or
   `HMEM_MCP_AUTH_TOKEN`.
3. Start the profile:

   ```bash
   docker compose --profile mcp up --build -d mcp
   docker compose logs -f mcp
   ```

MCP token precedence is CLI `--auth-token`, `HMEM_MCP_AUTH_TOKEN`,
`HMEM_AUTH_TOKEN`, then loopback-only local legacy static bearer config when
applicable.

## Troubleshooting

### `local static-bearer profile requires HMEM_API_KEY or HMEM_API_KEY_FILE`

The default container auth profile fails closed until a static bearer token is
provided. Set a strong `HMEM_API_KEY_FILE` or switch to deployed OIDC auth. Do
not disable auth to make a shared deployment start.

### `unsafe local bootstrap`

Local bootstrap was enabled while the server could be reached remotely or CORS
was broad. For containers, turn bootstrap off and use static bearer auth, or use
`HMEM_AUTH_MODE=deployed`. Use `HMEM_AUTH_LOCAL_ALLOW_REMOTE_BOOTSTRAP=true` only
for isolated development.

### PostgreSQL refuses to start or hmem cannot connect

Common causes:

- `POSTGRES_PASSWORD` is empty in `.env`.
- `POSTGRES_PASSWORD` was changed after the `postgres-data` volume was created;
  the existing database role still has the old password.
- `postgres` is not yet healthy or was started with a different `POSTGRES_DB` /
  `POSTGRES_USER` than hmem expects.
- `HMEM_DB_SSLMODE` is wrong for an external database.

Check `docker compose logs postgres hmem`. For throwaway local data, `docker
compose down -v` recreates the database from scratch; do not use it on data you
need to keep.

### Missing migrations or schema errors

Run the one-shot migration helper and inspect its output:

```bash
docker compose run --rm migrate
```

Errors mentioning `schema_migrations` mean hmem could not verify the migration
tracking table. Errors such as `relation does not exist` often mean the app
started against an unmigrated or different database.

### `pgvector extension is not installed`

This is expected with the default PostgreSQL image and hmem remains usable
without vector features. Diagnose the package and database layers explicitly:

```bash
docker compose run --rm hmem hmem-ctl pgvector status
```

If it reports `package_missing`, select/install a matching pgvector-enabled
PostgreSQL package or image first. If the package is available, follow the
reported `next action` and use `hmem-ctl pgvector enable`; do not recreate a
data volume merely to add this optional capability.

### Healthcheck is unhealthy

Check `docker compose logs hmem`. In local static bearer mode, make sure the
healthcheck can read an accepted bearer token through `HMEM_API_KEY[_FILE]`, or
that any explicit `HMEM_HEALTHCHECK_BEARER_TOKEN[_FILE]` points to the same
accepted bearer value. Also check DB connectivity, migration failures, and
whether `/var/lib/hmem/.hmem/logs` is writable.

### Frontend 404 or blank page

Confirm `HMEM_WEB_ENABLED=true` and `HMEM_WEB_STATIC_DIR=/opt/hmem/static`. The
Dockerfile verifies `index.html` at build time, so runtime 404s usually come from
overriding the static directory or serving through a reverse proxy that does not
forward browser paths to hmem.

### Missing runtime libraries in custom images

The provided Dockerfile runs `ldd` on `hmem-server`, `hmem-ctl`, and `hmem-mcp`
during the image build and installs required Debian runtime libraries. If a
custom runtime image fails with `error while loading shared libraries`, compare
its installed packages with the runtime stage of `Dockerfile` and rebuild.
