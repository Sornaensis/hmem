# Container runtime contract

For image and entrypoint maintainers. Operator setup is in [Docker deployment](../docker.md)
and [authentication](../auth.md); this file defines the required runtime behavior.

## Filesystem and identity

- The runtime user must have both `HOME` and its passwd home set to the same writable directory: `/var/lib/hmem`.
- `~/.hmem/config.yaml` therefore resolves to `/var/lib/hmem/.hmem/config.yaml`.
- Server logs are written below `/var/lib/hmem/.hmem/logs`; the entrypoint must create this directory and fail if it is not writable.
- In the external-PostgreSQL Compose stack, server and migration/admin HOME is a
  private 128 MiB tmpfs (`uid=10001,gid=10001,mode=0700,noexec,nosuid,nodev`).
  Generated config and rotating logs disappear when their container stops;
  application/auth state stays in PostgreSQL. Keep auth and database secrets
  stable in external environment or secret files across restarts.
- `/opt/hmem/static` holds immutable baked frontend assets. When web is enabled,
  the entrypoint checks their size and file type and stages at most 512 entries
  and 32 MiB into the writable `/run/hmem/static` tmpfs. It writes
  `hmem-runtime-config.js` by temporary file and atomic rename in that parent.
  `web.static_dir` defaults to `/run/hmem/static`; an explicit
  `HMEM_WEB_STATIC_DIR` override remains supported when its directory is
  writable. API-only mode skips staging.
- Generated config files must be mode `0600` and owned by the runtime user.

## Config generation rules

The entrypoint writes `/var/lib/hmem/.hmem/config.yaml` from the environment, then starts hmem with selected secret values still in process environment. Empty strings are treated as unset unless a field explicitly allows an empty value.

Existing hmem env overrides must be used instead of writing these values into generated YAML:

- `HMEM_DB_PASSWORD` -> `database.password`
- `HMEM_API_KEY` -> `auth.api_key`
- `HMEM_DB_SSLMODE` -> `database.sslmode`

If `_FILE` variants are supported by the entrypoint, it reads the file and exports the base variable above before starting hmem. The `_FILE` variable itself is never written into YAML.

Secrets that do not currently have hmem runtime env overrides, such as OIDC `client_secret` or `token_hash_secret`, may be written only to the generated `0600` YAML file when required. They must never be emitted in logs or frontend runtime config.

## Server, database, pool, logging, CORS, web, rate-limit, and TLS env

| Env | Default | YAML field | Notes |
| --- | --- | --- | --- |
| `HMEM_SERVER_HOST` | `0.0.0.0` | `server.host` | Container default intentionally binds all interfaces. Auth validation below prevents unsafe implicit local bootstrap. |
| `HMEM_SERVER_PORT` | `8420` | `server.port` | Valid range `1..65535`. |
| `HMEM_DB_HOST` | `postgres` | `database.host` | Compose/service default. Override for an external database. |
| `HMEM_DB_PORT` | `5432` | `database.port` | Valid range `1..65535`. |
| `HMEM_DB_NAME` | `hmem` | `database.name` | Must be non-empty. |
| `HMEM_DB_USER` | `hmem` | `database.user` | Must be non-empty when set. |
| `HMEM_DB_PASSWORD` | unset | env override only | Required when the DB role needs a password. Do not write to YAML. |
| `HMEM_DB_PASSWORD_FILE` | unset | none | Optional secret-file source for `HMEM_DB_PASSWORD`. |
| `HMEM_DB_SSLMODE` | `disable` | env override only | Export to hmem instead of writing `database.sslmode`. Use `require`/`verify-full` for external TLS databases. |
| `HMEM_POOL_SIZE` | `10` | `pool.size` | Valid range `1..1000`. |
| `HMEM_POOL_IDLE_TIMEOUT` | `60` | `pool.idle_timeout` | Seconds; valid range `1..3600`. |
| `HMEM_POOL_STATEMENT_TIMEOUT_MS` | `30000` | `pool.statement_timeout_ms` | Valid range `1000..300000`. |
| `HMEM_LOG_LEVEL` | `info` | `logging.level` | Accepted by server logger, e.g. `debug`, `info`, `warn`, `error`. |
| `HMEM_LOG_MAX_SIZE_MB` | `10` | `logging.max_size_mb` | Valid range `1..10000`, subject to the private HOME rotation allocation bound below. |
| `HMEM_LOG_BACKUP_COUNT` | `5` | `logging.backup_count` | Valid range `0..100`; `(backup_count + 1) * max_size_mb` must be at most 96 MiB, leaving 32 MiB of the 128 MiB HOME cap for config and headroom. |
| `HMEM_CORS_ALLOWED_ORIGINS` | empty list | `cors.allowed_origins` | Comma-separated exact origins. `*` is forbidden with deployed cookie auth. Same-origin frontend deployments should leave this empty. |
| `HMEM_WEB_ENABLED` | `true` | `web.enabled` | Set `false` for API-only containers. |
| `HMEM_WEB_STATIC_DIR` | `/run/hmem/static` | `web.static_dir` | The default is staged from immutable `/opt/hmem/static`; overrides must exist and be writable when web is enabled. |
| `HMEM_RATE_LIMIT_ENABLED` | `false` | `rate_limit.enabled` | Enables token-bucket middleware. |
| `HMEM_RATE_LIMIT_REQUESTS_PER_SECOND` | `10.0` | `rate_limit.requests_per_second` | Valid range `0.1..10000.0`. |
| `HMEM_RATE_LIMIT_BURST` | `20` | `rate_limit.burst` | Valid range `1..100000`. |
| `HMEM_TLS_CERT_FILE` | unset | `tls.cert_file` | TLS is enabled only when both cert and key are set. |
| `HMEM_TLS_KEY_FILE` | unset | `tls.key_file` | Fail fast if only one TLS file is set. |

## Embedding configuration

The entrypoint writes only existing `embedding` YAML fields. It does not
configure a CPU or ONNX fallback. An unavailable managed or HTTP provider
leaves vector operations unavailable while ordinary API health remains usable.

| Env | Default | YAML field | Rule |
| --- | --- | --- | --- |
| `HMEM_EMBEDDING_PROVIDER` | `disabled` | `embedding.mode` | `disabled`, `managed-tei`, or `http`. The optional GPU Compose override sets `managed-tei` on the existing hmem service. |
| `HMEM_EMBEDDING_ENDPOINT` | unset | `embedding.endpoint` | Required only for `http`; the application accepts a restricted absolute HTTP(S) `/embed` endpoint with no credentials, query, or fragment. Managed mode supplies its own loopback endpoint. |
| `HMEM_EMBEDDING_GPU_PROFILE` | unset | `embedding.gpu_profile` | Managed and HTTP modes both emit the required locked `native-tei-gte-qwen2-1.5b-instruct-cuda-sm120-f16-v1` profile; an explicit different value is rejected. Disabled mode omits it. HTTP still requires an independently supplied validated GPU endpoint and never starts an inference child. |
| `HMEM_EMBEDDING_BATCH_SIZE` | `1` | `embedding.batch_size` | Valid `1..256`; the logical client admits at most four and the production worker claims one per iteration. |
| `HMEM_EMBEDDING_TIMEOUT_MS` | `300000` | `embedding.timeout_ms` | Valid `100..300000`; a compact logical request is capped at `min(configured, 30000)` ms and a long request at `min(configured, 300000)` ms. Each deadline includes queueing, validation/retries, serial requests, decoding, and acknowledged local cleanup. |
| `HMEM_EMBEDDING_RETRY_ATTEMPTS` | `0` | `embedding.retry_attempts` | Valid `0..5`, within the original logical deadline. |

The generated YAML leaves the fixed qualified space fingerprint to the
application's committed default. There are no transport or inference fallback
fields in this packaging surface.

Standalone HTTP provider availability/compatibility has a 120-second ceiling.
Managed startup uses one distinct 120-second composition that includes
compatibility and cleanup; neither is the configured logical-request timeout.

## Image selection and immutable managed bundle

`docker build -t hmem:local .` and ordinary `docker compose up --build` select
the final `runtime` stage and require no GPU, TEI image, or model context. Both
images ship `hmem-server`, `hmem-ctl`, `hmem-mcp`, and the actual sibling
`hmem-embedding-http-helper` executable in `/usr/local/bin`. The default image
contains no CUDA/model layers. The explicit `gpu-runtime` target is based on
the pinned native TEI OCI index in `config/managed-embedding-provenance.yaml`.

GPU preparation and build commands are in [Docker deployment](../docker.md).
Preparation must accept only new private output paths, reject symlinks and
non-regular artifacts, and verify both source and copied trees with the
[independent checker](../scripts/check-managed-embedding-provenance.hs).
It must not download model or runtime files. The GPU override extends the same
`hmem` service, reserves one NVIDIA GPU, publishes only application HTTP, and
mounts no Docker socket. Model, router, and manifest remain immutable under
`/opt/hmem/managed-embedding`; notices and executable/library inventories stay
outside the strict model/runtime trees.

The pinned runtime advertises 32768 model tokens. The application accepts at
most 32767 fully formatted UTF-8 input bytes and uses the existing compact
golden validation before enabling a target. No serving assets are fetched at
container startup. Image builds record their actual base/platform digests and
four-binary ELF closure; the final image must pass its own closure check.

## Process, filesystem, and stop contract

The image has `STOPSIGNAL SIGINT`. The `hmem` Compose service runs with
`init: true`, `stop_signal: SIGINT`, and `stop_grace_period: 180s`. Equivalent
direct deployment uses `docker run --init --stop-timeout 180` and SIGINT as
the configured stop signal. The entrypoint `exec`s the server, so SIGINT
unwinds its worker and, when managed mode owns one, its child; 180 seconds is
a grace budget, not a prediction from observed stops. Verify the server
shutdown message and, only when a managed supervisor existed, its stopped
message, plus the disabled persisted target and closed API port before removal.
Container absence alone does not prove cleanup.
This contract does not claim arbitrary SIGTERM cleanup, HTTP request draining,
or remote CUDA preemption.

Compose makes the application root filesystem read-only. Private
`/var/lib/hmem` HOME/config/logs are bounded by the 128 MiB hardened tmpfs;
`/tmp` and `/run/hmem` are separately bounded noexec/nosuid/nodev tmpfs
mounts, and the latter contains staged frontend assets. Both images run
nonroot with a matching passwd home. The existing `hmem-data` volume is
intentionally unmounted but untouched: Compose performs no deletion, copying,
or migration of its unknown contents. Native-host `hmem-ctl` init/service and
custom HOME workflows are outside this ephemeral external-PostgreSQL contract.
Operators choosing persistent HOME/log storage must arrange finite capacity,
noexec/nosuid/nodev restrictions, ownership, secret permissions, and log
retention themselves; an ordinary Docker local volume is not a quota.
Operator-supplied static overrides must provide their own bounded writable
directory.

## Auth env and safe defaults

The container must never start with the implicit local superadmin exposed on `0.0.0.0` by default.

| Env | Default | YAML field | Notes |
| --- | --- | --- | --- |
| `HMEM_AUTH_MODE` | `local` | `auth.mode` | `local` or `deployed`. Default is local static bearer mode, not implicit bootstrap. |
| `HMEM_AUTH_ENABLED` | `true` | `auth.enabled` | Enables local legacy static bearer auth when `HMEM_AUTH_MODE=local` and `HMEM_API_KEY` is set. |
| `HMEM_API_KEY` | unset | env override only | Static bearer secret for local mode. Required for the default local container profile. Do not write to YAML. |
| `HMEM_API_KEY_FILE` | unset | none | Optional secret-file source for `HMEM_API_KEY`. |
| `HMEM_AUTH_LOCAL_BOOTSTRAP_ENABLED` | `false` | `auth.local.bootstrap_enabled` | Must remain false for normal container deployments. |
| `HMEM_AUTH_LOCAL_ALLOW_REMOTE_BOOTSTRAP` | `false` | `auth.local.allow_remote_bootstrap` | Dev-only escape hatch. If true, log a high-severity warning naming the remote-bootstrap risk. |
| `HMEM_AUTH_LOCAL_BOT_TOKENS_FILE` | unset | `auth.local.bot_tokens` | Optional JSON/YAML array of `{label, token}`. Because tokens are bearer secrets, prefer `HMEM_API_KEY` for v1 static container auth unless per-bot attribution is required. |

Local-mode startup is valid only when one of these profiles is true:

1. `HMEM_AUTH_LOCAL_BOOTSTRAP_ENABLED=false`, `HMEM_AUTH_ENABLED=true`, and `HMEM_API_KEY` is present.
2. `HMEM_AUTH_LOCAL_BOOTSTRAP_ENABLED=true` while the server is bound to loopback and CORS is loopback-only.
3. Dev-only: `HMEM_AUTH_LOCAL_BOOTSTRAP_ENABLED=true` and `HMEM_AUTH_LOCAL_ALLOW_REMOTE_BOOTSTRAP=true`, with an explicit warning that the implicit local superadmin may be reachable by remote clients.

The default container profile uses case 1 and must fail fast until `HMEM_API_KEY` or `HMEM_API_KEY_FILE` is supplied.

## Deployed OIDC/session env

These fields are used when `HMEM_AUTH_MODE=deployed`.

| Env | Default | YAML field | Notes |
| --- | --- | --- | --- |
| `HMEM_AUTH_DEPLOYED_ISSUER` | unset | `auth.deployed.issuer` | Required for provider JWT validation. Must be `https://` outside isolated tests. |
| `HMEM_AUTH_DEPLOYED_AUDIENCE` | unset | `auth.deployed.audience` | Required for provider JWT validation. |
| `HMEM_AUTH_DEPLOYED_DISCOVERY_URL` | unset | `auth.deployed.discovery_url` | Required for server-side browser OIDC unless both authorization and token endpoints are supplied. Also counts as the JWKS/discovery source for JWT validation. Must be `https://` outside isolated tests. |
| `HMEM_AUTH_DEPLOYED_JWKS_URL` | unset | `auth.deployed.jwks_url` | Alternative JWKS source. Must be `https://` outside isolated tests. |
| `HMEM_AUTH_DEPLOYED_JWKS_FILE` | unset | `auth.deployed.jwks` | Optional inline JWKS JSON source; entrypoint reads the file and writes JSON to YAML. |
| `HMEM_AUTH_DEPLOYED_TOKEN_LOOKUP` | `database` | `auth.deployed.token_lookup` | Only supported value today. |
| `HMEM_AUTH_DEPLOYED_TOKEN_HASH_SECRET` | unset | `auth.deployed.token_hash_secret` | Recommended before issuing PAT/service tokens. Use `_FILE` where possible. |
| `HMEM_AUTH_DEPLOYED_TOKEN_HASH_SECRET_FILE` | unset | `auth.deployed.token_hash_secret` | Optional secret-file source. |
| `HMEM_AUTH_DEPLOYED_CLIENT_ID` | unset | `auth.deployed.client_id` | Required for browser OIDC login. |
| `HMEM_AUTH_DEPLOYED_CLIENT_SECRET` | unset | `auth.deployed.client_secret` | Required for browser OIDC login. Use `_FILE` where possible. |
| `HMEM_AUTH_DEPLOYED_CLIENT_SECRET_FILE` | unset | `auth.deployed.client_secret` | Optional secret-file source. |
| `HMEM_AUTH_DEPLOYED_REDIRECT_URI` | unset | `auth.deployed.redirect_uri` | Required for browser OIDC login. |
| `HMEM_AUTH_DEPLOYED_SCOPES` | `openid,profile,email` | `auth.deployed.scopes` | Comma-separated list. Must include `openid`. |
| `HMEM_AUTH_DEPLOYED_AUTHORIZATION_ENDPOINT` | unset | `auth.deployed.authorization_endpoint` | Required with `HMEM_AUTH_DEPLOYED_TOKEN_ENDPOINT` for server-side browser OIDC when discovery is absent. Must be `https://` outside isolated tests. |
| `HMEM_AUTH_DEPLOYED_TOKEN_ENDPOINT` | unset | `auth.deployed.token_endpoint` | Required with `HMEM_AUTH_DEPLOYED_AUTHORIZATION_ENDPOINT` for server-side browser OIDC when discovery is absent. Must be `https://` outside isolated tests. |
| `HMEM_AUTH_DEPLOYED_SESSION_COOKIE_NAME` | `hmem_session` | `auth.deployed.session_cookie_name` | Non-secret cookie name. |
| `HMEM_AUTH_DEPLOYED_CSRF_COOKIE_NAME` | `hmem_csrf` | `auth.deployed.csrf_cookie_name` | Non-secret; mirror into frontend runtime config. |
| `HMEM_AUTH_DEPLOYED_CSRF_HEADER_NAME` | `X-CSRF-Token` | `auth.deployed.csrf_header_name` | Non-secret; mirror into frontend runtime config. |
| `HMEM_AUTH_DEPLOYED_SESSION_TTL_SECONDS` | `28800` | `auth.deployed.session_ttl_seconds` | Must be at least `60`. |
| `HMEM_AUTH_DEPLOYED_COOKIE_SECURE` | `true` | `auth.deployed.cookie_secure` | Must be true when `cookie_same_site=None`. |
| `HMEM_AUTH_DEPLOYED_COOKIE_SAME_SITE` | `Lax` | `auth.deployed.cookie_same_site` | One of `Lax`, `Strict`, `None`. |

Deployed-mode startup must fail fast unless provider JWT validation can be configured (`issuer`, `audience`, and one JWKS/discovery source). Browser OIDC login additionally requires `client_id`, `client_secret`, `redirect_uri`, and either `discovery_url` or both `authorization_endpoint` and `token_endpoint`.

## Browser runtime-config strategy

Static frontend assets must not be rebuilt or patched with secrets. The entrypoint may generate a small runtime JavaScript file, loaded before the Elm bundle, that assigns only non-secret values to `window.HMEM_CONFIG`.

Generated file: `/run/hmem/static/hmem-runtime-config.js` by default.

Allowed frontend env surface:

| Env | Default | `window.HMEM_CONFIG` key | Notes |
| --- | --- | --- | --- |
| `HMEM_FRONTEND_API_URL` | unset | `apiUrl` | When unset, frontend uses `window.location.origin`. |
| `HMEM_FRONTEND_WS_URL` | unset | `wsUrl` | When unset, frontend derives `/api/v1/ws` from `apiUrl`. |
| `HMEM_FRONTEND_AUTH_MODE` | `HMEM_AUTH_MODE` | `authMode` / `runtimeMode` | Non-secret mode string. |
| `HMEM_FRONTEND_LOGIN_URL` | `/api/v1/auth/login` in deployed mode; unset in local mode | `loginUrl` | Non-secret. |
| `HMEM_FRONTEND_LOGOUT_URL` | `/api/v1/auth/logout` in deployed mode; unset in local mode | `logoutUrl` | Non-secret. |
| `HMEM_FRONTEND_LOGOUT_REDIRECT_URL` | `/` | `logoutRedirectUrl` | Non-secret. |
| `HMEM_FRONTEND_SERVER_SIDE_OIDC_LOGIN` | `true` in deployed mode | `serverSideOidcLogin` | Non-secret. |
| `HMEM_FRONTEND_CSRF_COOKIE_NAME` | deployed CSRF cookie name | `csrfCookieName` | Should match backend `auth.deployed.csrf_cookie_name`. |
| `HMEM_FRONTEND_CSRF_HEADER_NAME` | deployed CSRF header name | `csrfHeaderName` | Should match backend `auth.deployed.csrf_header_name`. |
| `HMEM_FRONTEND_AUTH_TOKEN_STORAGE` | `local` | `authTokenStorage` | `local`, `session`, or `memory` for explicit bearer fallback flows. |
| `HMEM_FRONTEND_AUTH_TOKEN_STORAGE_KEY` | `hmem-auth-token` | `authTokenStorageKey` | Non-secret storage key name. |
| `HMEM_FRONTEND_AUTH_TOKEN_URL_PARAMS` | `hmem_token,auth_token,access_token` | `authTokenUrlParams` | For fragment-only bearer fallback callbacks. |
| `HMEM_FRONTEND_REQUIRE_AUTH_STATE` | `true` | `requireAuthState` | Should remain true for provider redirects. |

The generator must never put `HMEM_API_KEY`, `HMEM_AUTH_TOKEN`, `HMEM_MCP_AUTH_TOKEN`, DB passwords, OIDC client secrets, PATs, or service tokens into `window.HMEM_CONFIG`. In particular, do not generate `window.HMEM_CONFIG.authToken` by default.

## MCP env

The MCP process reads existing runtime env directly; these values are not part of server YAML.

| Env | Default | Notes |
| --- | --- | --- |
| `HMEM_SERVER_URL` | `http://127.0.0.1:${HMEM_SERVER_PORT}` | URL for `hmem-mcp` to reach `hmem-server`. Override to a service URL in multi-container Compose. |
| `HMEM_MCP_AUTH_TOKEN` | unset | Preferred bearer token for MCP. Secret; do not write to YAML or frontend config. |
| `HMEM_MCP_AUTH_TOKEN_FILE` | unset | Optional secret-file source for `HMEM_MCP_AUTH_TOKEN`. |
| `HMEM_AUTH_TOKEN` | unset | Fallback bearer token used by MCP clients. Secret; do not write to YAML or frontend config. |
| `HMEM_AUTH_TOKEN_FILE` | unset | Optional secret-file source for `HMEM_AUTH_TOKEN`. |

MCP token precedence remains: CLI `--auth-token`, `HMEM_MCP_AUTH_TOKEN`, `HMEM_AUTH_TOKEN`, then loopback-only local legacy static bearer config when applicable.

## Fail-fast validation

The entrypoint must refuse to start when any of these are true:

- `HOME` and the passwd home differ, or `/var/lib/hmem` / `/var/lib/hmem/.hmem/logs` is not writable.
- Any numeric env is missing a valid number or is outside the ranges listed above.
- Any boolean env is not one of `true`, `false`, `1`, `0`, `yes`, or `no`.
- Any constrained string env is outside its allowed set, including `HMEM_AUTH_MODE` (`local|deployed`), `HMEM_AUTH_DEPLOYED_TOKEN_LOOKUP` (`database`), `HMEM_AUTH_DEPLOYED_COOKIE_SAME_SITE` (`Lax|Strict|None`), or `HMEM_FRONTEND_AUTH_TOKEN_STORAGE` (`local|session|memory`).
- `HMEM_AUTH_MODE=local`, `HMEM_AUTH_LOCAL_BOOTSTRAP_ENABLED=true`, and `HMEM_AUTH_LOCAL_ALLOW_REMOTE_BOOTSTRAP=false` with either a non-loopback `HMEM_SERVER_HOST` or remote/wildcard `HMEM_CORS_ALLOWED_ORIGINS`.
- `HMEM_AUTH_MODE=local` has `HMEM_AUTH_LOCAL_BOOTSTRAP_ENABLED=false` but `HMEM_AUTH_ENABLED` is not true, or neither `HMEM_API_KEY` nor `HMEM_API_KEY_FILE` is supplied.
- `HMEM_AUTH_MODE=deployed` is missing `issuer`, `audience`, or all JWKS/discovery sources.
- Browser OIDC login is enabled in deployed mode but `client_id`, `client_secret`, `redirect_uri`, or the login endpoint source (`discovery_url` or both `authorization_endpoint` and `token_endpoint`) is missing.
- Deployed cookie sessions use wildcard or otherwise over-broad credentialed CORS, or `cookie_same_site=None` with `cookie_secure=false`.
- Only one of `HMEM_TLS_CERT_FILE` and `HMEM_TLS_KEY_FILE` is set.
- `HMEM_WEB_ENABLED=true` but `HMEM_WEB_STATIC_DIR` does not exist.

Warnings, not failures:

- `HMEM_AUTH_LOCAL_ALLOW_REMOTE_BOOTSTRAP=true`: print a clear dev-only warning before startup.
- `HMEM_CORS_ALLOWED_ORIGINS` is empty: same-origin browser deployments still work, cross-origin browsers do not.
- `HMEM_AUTH_DEPLOYED_TOKEN_HASH_SECRET` is unset: PAT/service tokens can still resolve legacy hashes, but new HMAC-hashed token issuance should wait until it is configured.
