# hmem auth guide

This page describes hmem authentication modes, permissions, and client configuration.

## Modes

hmem supports two auth modes.

### Local mode

Use local mode for personal development and single-user automation.

- Default mode: `auth.mode: local`
- Local bootstrap is enabled by default.
- With local bootstrap enabled, the local user has superadmin privileges.
- The implicit local superadmin is loopback/local-CORS only by default; binding local mode to a non-loopback host or permissive CORS requires the explicit `auth.local.allow_remote_bootstrap: true` escape hatch and should only be used on trusted private networks.
- Optional local bot tokens label automated actions in audit/events.
- Legacy `auth.enabled: true` plus `auth.api_key` / `HMEM_API_KEY` is local-only static bearer compatibility. Do not use it for deployed auth.

Minimal local config:

```yaml
auth:
  mode: local
  local:
    bootstrap_enabled: true
    allow_remote_bootstrap: false
    bot_tokens:
      - label: local-service
        token: replace-with-local-service-token
```

### Deployed mode

Use deployed mode for shared installations.

- Set `auth.mode: deployed` explicitly.
- Users authenticate with provider-backed bearer tokens such as JWT/OIDC tokens.
- Service access uses bearer tokens resolved through database-backed `access_tokens` rows.
- Legacy local static bearer auth does not work in deployed mode.
- Protected requests without a valid deployed principal fail closed.

Example deployed config:

```yaml
auth:
  mode: deployed
  deployed:
    issuer: https://issuer.example
    audience: hmem-web
    jwks_url: https://issuer.example/.well-known/jwks.json
    token_lookup: database
```

## Permission model

hmem has two global permissions and three workspace roles.

### Global permissions

| Permission | Allows |
| --- | --- |
| `create_workspace` | Create workspaces. The creator becomes `admin` of the new workspace. |
| `superadmin` | Bypass authorization checks, administer every workspace, purge resources, and view the global audit log. |

### Workspace roles

| Role | Allows |
| --- | --- |
| `read` | View workspace-scoped resources. |
| `edit` | `read` plus create/update/link/unlink/reorder/restore/soft-delete workspace resources. |
| `admin` | `edit` plus purge, workspace audit-log access, and workspace membership administration. |

Bot and service tokens identify automated clients in audit/events. In deployed mode, permissions come from the token's grant-bearing user.

## Deployed setup checklist

1. Apply database migrations before enabling deployed auth.
2. Configure `auth.mode: deployed` and provider verification settings.
3. Bootstrap at least one `superadmin` user using the supported operator workflow.
4. Grant `create_workspace` or workspace roles to non-superadmin users as needed.
5. For automated clients, create service/PAT tokens linked to a grant-bearing user using the token workflow below.
6. Store raw service tokens in your secret manager or runtime environment; hmem stores token hashes.
7. Verify `/api/v1/session`, one protected read, and one protected write before production traffic.

### First superadmin bootstrap workflow

The supported first-user bootstrap path is an operator-run `hmem-ctl` command that connects directly to the configured database after migrations have run:

```bash
hmem-ctl auth bootstrap-superadmin \
  --auth-subject oidc-subject-from-provider \
  --display-name "Primary Operator" \
  --email operator@example.com
```

Required behavior for the implemented workflow:

- `--auth-subject` is the stable subject claim that deployed bearer/JWT authentication resolves later.
- The command creates or updates exactly that user with `is_superadmin = true` and `can_create_workspace = true`.
- Running it again for the same `--auth-subject` is idempotent.
- If a different superadmin already exists, the command refuses unless the operator passes an explicit break-glass `--force` override.
- Logs and output identify the affected user and bootstrap decision, but never print bearer tokens or provider credentials.
- Operators must verify the result by authenticating as that provider subject and checking that `/api/v1/session` reports `superadmin`.

### User and global-grant administration

Operators can create or update deployed users and global grants with `hmem-ctl auth users upsert`:

```bash
hmem-ctl auth users upsert \
  --auth-subject oidc-subject-from-provider \
  --display-name "Workspace Creator" \
  --email creator@example.com \
  --can-create-workspace
```

Use `--superadmin` / `--no-superadmin` and `--can-create-workspace` / `--no-create-workspace` to update global grants. Use `--disabled` to make JWT and PAT authentication for that grant-bearing user fail closed, and `--active` to re-enable the user. Workspace memberships remain managed by workspace-admin APIs.

### Service/PAT token lifecycle

Operators can issue, rotate, and revoke deployed service/PAT tokens with `hmem-ctl auth tokens`. Issuance generates a high-entropy raw token, stores only `token_hash`, and prints the raw token exactly once:

```bash
hmem-ctl auth tokens issue \
  --grant-user-id user-uuid-with-required-permissions \
  --actor-label deploy-bot \
  --expires-at 2026-05-01T00:00:00Z
```

Rotation creates an overlapping replacement by default; revoke the old token after clients switch, or pass `--revoke-old` for immediate cutover:

```bash
hmem-ctl auth tokens rotate --token-id old-token-row-uuid
hmem-ctl auth tokens revoke --token-id old-token-row-uuid
```

Officially issued tokens use the `hmem_pat_v1_` prefix plus 96 lowercase hexadecimal characters from UUIDv4 random material, exceeding the 256-bit entropy floor for hmem bearer tokens. Tokens are operator-managed in v1, not self-service UI objects. Use `--actor-type bot|user`, stable `--actor-label` values, and least-privilege grant-bearing users so automated clients inherit only the permissions they need.

If an operator pre-provisions an `access_tokens` row outside `hmem-ctl`, the raw bearer secret must still be generated from at least 256 bits of cryptographically secure randomness. Do not use short, human-chosen, or reusable secrets; store only the canonical `token_hash`, and record the raw token only in a secret manager.

## Frontend and MCP

The server authorizes requests. Clients forward credentials and display server-provided session state.

Configure the frontend with the HTTP and WebSocket server URLs. Load session state before fetching protected data.

MCP can point at a separate server:

```bash
HMEM_SERVER_URL=https://hmem.example.com \
HMEM_MCP_AUTH_TOKEN=replace-with-service-token \
hmem-mcp
```

MCP token precedence is:

1. `--auth-token`
2. `HMEM_MCP_AUTH_TOKEN`
3. `HMEM_AUTH_TOKEN`
4. loopback-only local legacy static bearer config when `auth.enabled` is set and `auth.api_key` / `HMEM_API_KEY` provides the token

Use `--no-auth` to suppress bearer forwarding entirely.
