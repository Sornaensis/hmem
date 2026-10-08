# Authentication

Use local mode for a personal installation and deployed mode for shared access.
For container configuration, see [Docker](docker.md); for HTTP requests, see
[the API guide](api.md).

## Local installation

Local mode is the native default. With bootstrap enabled, local requests have
superadmin access without a login:

```yaml
auth:
  mode: local
  local:
    bootstrap_enabled: true
    allow_remote_bootstrap: false
```

Keep this server on loopback with local CORS origins. A remote bind or broad CORS
is rejected unless `auth.local.allow_remote_bootstrap: true` is set; that option
exposes superadmin access and is only suitable for trusted private development
networks.

To require a bearer token in local mode, set `auth.enabled: true`, supply
`HMEM_API_KEY` (or `auth.api_key`), and turn bootstrap off. Local bot tokens under
`auth.local.bot_tokens` can label automated actions, but have superadmin access
too. Local credentials and bootstrap do not work in deployed mode.

## Shared installation

1. Apply database migrations and configure an OIDC client with the callback URL
   `https://hmem.example.com/api/v1/auth/callback`.
2. Set the following in `~/.hmem/config.yaml`, replacing the example values.
   Keep secrets in your deployment's secret store; container environment settings
   are listed in the [configuration reference](config/container-runtime-contract.md).

   ```yaml
   auth:
     mode: deployed
     deployed:
       issuer: https://issuer.example
       audience: hmem-web
       discovery_url: https://issuer.example/.well-known/openid-configuration
       client_id: hmem-web
       client_secret: replace-with-oidc-client-secret
       redirect_uri: https://hmem.example.com/api/v1/auth/callback
       token_hash_secret: replace-with-stable-secret
   ```

   The issuer must match the provider, and `audience` must match bearer JWTs
   intended for hmem. Browser login validates the ID token against `client_id`.
   Serve hmem over HTTPS: session cookies are secure by default, and OIDC
   authorization and token endpoints require HTTPS.
3. Run this operator command against the same configured database. Use the
   provider's stable `sub` claim, rather than the user's email address:

   ```bash
   hmem-ctl auth bootstrap-superadmin \
     --auth-subject oidc-subject-from-provider \
     --display-name "Primary Operator" --email operator@example.com
   ```

   Repeating it for the same subject is safe. If another superadmin exists, it
   refuses; use `--force` only for deliberate recovery or an additional superadmin.
4. Sign in and check `GET /api/v1/session` for the `superadmin` permission.
   Other provider users must also be registered in hmem before they can sign in.

Register a user who can create workspaces:

```bash
hmem-ctl auth users upsert \
  --auth-subject oidc-subject-from-provider \
  --display-name "Workspace Creator" --can-create-workspace
```

Use `--no-create-workspace`, `--superadmin`, or `--no-superadmin` to change global
grants. `--disabled` denies that user's access; `--active` re-enables it.

## Permissions

| Grant or role | Access |
| --- | --- |
| Global `create_workspace` | Create a workspace and become its admin. |
| Global `superadmin` | Administer all workspaces and view the global audit log. |
| Workspace `read` | View that workspace's resources. |
| Workspace `edit` | Read, create, edit, reorder, restore, and delete its resources. |
| Workspace `admin` | Edit access plus permanent deletion and workspace audit access. |

A service token inherits its linked user's grants and workspace roles. It does
not grant access by itself. Existing workspace membership has no public management
command or HTTP endpoint; `users upsert` changes global grants only.

## Service and MCP tokens

Issue a token for a user with the needed permissions:

```bash
hmem-ctl auth tokens issue \
  --grant-user-id user-uuid-with-required-permissions \
  --actor-label deploy-bot --expires-at 2027-01-01T00:00:00Z
```

Save the raw token in your secret manager when it is printed; it is shown once.
Keep the returned token ID for rotation and revocation. `--actor-type` defaults
to `bot`; use `user` for a personal token. Expiry is optional.

Rotate, switch clients to the replacement, then revoke the old token:

```bash
hmem-ctl auth tokens rotate --token-id existing-token-uuid
hmem-ctl auth tokens revoke --token-id existing-token-uuid
```

Rotation leaves both tokens valid by default and keeps the old expiry unless
`--expires-at` is supplied. Add `--revoke-old` to rotate with immediate revocation.
Keep `auth.deployed.token_hash_secret` stable across the server and operator
commands: changing or removing it invalidates tokens issued with the old secret.

Pass the raw token as `Authorization: Bearer <token>` to HTTP clients. For MCP:

```bash
HMEM_SERVER_URL=https://hmem.example.com \
HMEM_MCP_AUTH_TOKEN=replace-with-service-token \
hmem-mcp
```

MCP uses the first available token: `--auth-token`, `HMEM_MCP_AUTH_TOKEN`,
`HMEM_AUTH_TOKEN`, then local static bearer configuration for loopback servers
only. `--no-auth` suppresses forwarding. See [the README](README.md) for MCP setup.

## Browser login

Set `window.HMEM_CONFIG.loginUrl` to `/api/v1/auth/login` and `logoutUrl` to
`/api/v1/auth/logout`, including any reverse-proxy prefix. The frontend handles
login redirects, session cookies, and logout.

Cookie-authenticated writes, including logout, need the `hmem_csrf` cookie value
in the `X-CSRF-Token` header. If you customize the server's CSRF names, match them
with frontend `csrfCookieName` and `csrfHeaderName`. Keep `cors.allowed_origins`
restricted to trusted frontend origins; leave it empty for same-origin hosting.

An explicit bearer token takes precedence over a session cookie, even if the
token is invalid. Remove an old bearer token before relying on cookie login.
For browser bearer fallback, return tokens in URL fragments, never query strings.
Bearer storage defaults to `localStorage`, where script access can expose it.
Set `window.HMEM_CONFIG.authTokenStorage` to `session` for per-tab storage or
`memory` to discard the token when the page closes or reloads.

## Access problems

| Symptom | Check |
| --- | --- |
| Login unavailable | OIDC discovery/endpoints, client ID/secret, and registered callback URL. |
| Login callback rejected | The provider subject is an active hmem user; issuer and client ID match. Start login again if the state cookie expired. |
| HTTP 401 | Token expiry/revocation, disabled user, issuer/audience, and any stale bearer overriding a cookie. |
| HTTP 403 | Required global grant or workspace role. Check `/api/v1/session?workspace_id=<uuid>` with the same credential. |
| `csrf_required` | Matching CSRF cookie/header on cookie-authenticated writes, including logout. |
| `unsafe local bootstrap` | Loopback bind and restricted CORS, or switch to bearer/deployed authentication. |
