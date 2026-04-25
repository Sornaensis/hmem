# hmem Auth Specification

This document is the canonical auth specification for hmem. Server, frontend, MCP, migration, audit, and rollout work should derive from this specification.

## Goals

- Support two runtime modes: `local` and `deployed`
- Keep one shared principal and authorization model across all clients
- Make the server authoritative for all authorization decisions
- Preserve low-friction local use while enabling real multi-user deployed operation
- Keep the initial rollout cohesive enough to land as one SQL migration by default

## Core principles

1. **One principal model** — all requests resolve to a principal, regardless of source.
2. **One policy model** — local mode changes principal resolution, not the authorization rules themselves.
3. **Server-authoritative authz** — frontend and MCP may present session state, but they do not decide access.
4. **Workspace-scoped tenancy** — most access decisions are made against a workspace boundary.
5. **Minimal public permission surface** — keep user-visible permissions limited to the documented global permissions and workspace roles.

## Runtime modes

## Local mode

Local mode exists to preserve the current frictionless single-user experience while still distinguishing human and agent actions.

- The local human user resolves to a **synthetic superadmin principal**.
- Local human requests should resolve through a server-owned local bootstrap/session path controlled by local auth bootstrap settings. In the current transitional implementation, that bootstrap path may be expressed as an explicit config-gated local bootstrap fallback in server middleware until fuller session mechanics are implemented; it should still be treated as a local bootstrap mechanism rather than anonymous loopback trust.
- The local human user skips normal permission checks because superadmin bypass already covers all actions.
- Local bot tokens still resolve to bot principals so audit and event attribution can distinguish agent actions from human actions.
- Local mode does **not** define a second authorization system; it only changes how principals are resolved.

## Deployed mode

Deployed mode enables real multi-user access control.

- Human users authenticate through provider-backed user tokens (for example JWT/OIDC-backed bearer tokens).
- Bot or service access uses bearer tokens resolved by the server into bot/service principals.
- Access is enforced through the shared policy model described below.
- The first deployed superadmin should be created through operator/bootstrap provisioning outside normal end-user authorization flows.

## Principal model

Every authenticated request resolves to a principal with these canonical actor fields:

- `actor_type` — `user` or `bot`
- `actor_id` — stable internal identifier for the resolved principal
- `actor_label` — human-readable label suitable for audit and UI attribution

These fields should be reused consistently in request context, audit records, session/principal APIs, and emitted change events.

## Canonical v1 grant model

To keep v1 authorization coherent and avoid a second permission system for bots, grant storage should be canonicalized as follows:

- Human grant-bearing accounts live in `users`.
- Workspace role grants live in `workspace_memberships`.
- Global permissions (`create_workspace`, `superadmin`) live on the grant-bearing `users` row.
- Bot/service tokens resolve to a **bot actor** for attribution plus a **grant-bearing user row** for authorization.

In other words, a bot principal may present `actor_type = bot`, but its global permissions and workspace memberships are evaluated through its linked grant-bearing `users` row rather than through a separate bot-only grants table.

The canonical minimal v1 token storage model should therefore include:

- token identity and lifecycle fields
- actor attribution fields such as label
- a foreign key such as `grant_user_id` pointing at the grant-bearing `users` row whose permissions are evaluated

This keeps `workspace_memberships` canonical in v1 and avoids introducing parallel user-membership and bot-membership systems.

## User principals

User principals may carry two global permissions:

- `create_workspace`
- `superadmin`

### Global permissions

#### `create_workspace`

- Allows creating new workspaces
- Automatically grants `admin` membership on any workspace the user creates

#### `superadmin`

- Bypasses all authorization checks
- Is implicitly `admin` of every workspace
- Can create, delete, restore, and purge any resource
- Can view the full global audit log

## Bot principals

Bots are authenticated principals used for MCP, agent, or service access.

- Bots exist primarily for authentication and attribution in v1
- Bot identity should remain minimal in v1: label, token identity, lifecycle metadata, and whatever linkage is needed to resolve the principal
- Bots should use the same shared policy model as users by resolving to a linked grant-bearing `users` row; they do not get a separate bot-only authorization system in v1.
- Local named bots may be provisioned through local bootstrap/operator flows and may link to a broad-access grant-bearing user row for compatibility, but that access should still be represented through the same principal-and-policy model.
- Transitional local compatibility exception: until local bootstrap provisions real grant-bearing rows for configured local bot tokens and the legacy static-bearer token, the server may resolve those local-only bot principals to the same synthetic local superadmin authority used for the implicit local user. This exception must remain local-mode-only and must not be used for deployed bot/PAT resolution.
- Token lifecycle in v1 should default to config/bootstrap or operator-managed workflows rather than a large self-service token-management product

## Workspace roles

Workspace-visible permissions are role-based:

- `read`
- `edit`
- `admin`

### `read`

- View the workspace and workspace-scoped entities
- Run workspace-scoped reads and searches permitted by the API

### `edit`

- Includes `read`
- Create, update, restore, and soft-delete workspace-scoped entities
- Manage normal edit-time relationships such as links and dependencies where applicable

### `admin`

- Includes `edit`
- Purge soft-deleted entities within the workspace
- View the workspace audit log
- Manage workspace memberships and permissions within that workspace

## Authorization model

Authorization should be expressible as:

- a resolved principal
- an internal action/capability
- a resource scope (`global` or a specific `workspace`)

The evaluation order is:

1. If the principal is superadmin, allow.
2. If the action is global, evaluate global permissions.
3. If the action is workspace-scoped, evaluate the principal's role in that workspace.

## Minimum policy matrix

The following endpoint/action classes are the minimum canonical mapping for v1 implementation:

- **Unauthenticated by default:** health and static/openapi endpoints, unless later hardened separately
- **Authenticated session introspection:** session/principal context endpoint
- **Global create:** workspace creation requires `create_workspace` or `superadmin`
- **Global audit:** full audit-log access requires `superadmin`
- **Workspace read:** workspace listing/filtering, entity get/list/search/overview/history within a workspace require `read`
- **Workspace edit:** create/update/link/unlink/reorder/restore/soft-delete operations within a workspace require `edit`
- **Workspace admin:** purge, workspace audit views, and workspace membership management require `admin`

Any handler that operates by entity ID must first resolve the owning workspace and then apply the above matrix.

## Resource scoping rules

- Endpoints that already accept `workspace_id` must still validate access through server-side authorization.
- Endpoints that operate by entity ID (for example project/task/memory/category/saved-view IDs) must first resolve the owning workspace before applying authorization.
- Audit visibility is workspace-admin scoped for workspace audit views and superadmin scoped for global audit views.

## Session and client model

The server should expose a session/principal context API that acts as the authoritative client-facing summary of:

- auth mode
- current principal actor fields
- global permissions
- workspace-role context for an explicitly requested workspace

The minimum contract should therefore be sufficient to return:

- `auth_mode`
- `principal` with `actor_type`, `actor_id`, and `actor_label`
- `global_permissions`
- `workspace_role` for a supplied workspace context, or an equivalent unambiguous workspace-scoped lookup contract

Frontend bootstrap should rely on this API rather than inferring permissions from token contents.

## Minimum config contract

The auth config contract should be explicit enough that server and MCP implementations share the same meaning for each field.

The minimum v1 shape should include:

- `auth.mode = local | deployed`
- `auth.local.*` settings for local bootstrap/session behavior
- `auth.local.bot_tokens[]` or an equivalent local bootstrap token list with label and secret/token material
- `auth.deployed.*` settings for provider-backed user-token validation, such as issuer plus audience and either discovery/JWKS configuration or equivalent verifier inputs
- `auth.deployed.token_lookup = database` or equivalent semantics indicating that PAT/bot/service tokens are resolved through persisted token storage

The current `enabled/api_key` model should be treated as a legacy compatibility surface for the currently implemented **local static bearer** path, not as the long-term canonical design for deployed auth.

## Transport model

### HTTP

- Clients authenticate with bearer credentials appropriate to the current mode.
- The server resolves the principal and applies the shared policy model.

### WebSocket

- WebSocket auth is an extension of the same server auth core, not a separate auth system.
- Deployed mode should prefer a safer ticket/session exchange over long-lived query tokens.
- Local mode may remain lightweight, but still needs attributable human-vs-bot actor resolution.
- Event delivery must be scoped so unauthorized clients do not receive workspace activity.

### MCP

- MCP stays thin.
- MCP loads config, forwards auth, preserves workspace-context behavior, and fails clearly.
- MCP does not define its own authorization rules.

## SQL schema expectations

The auth rollout should be implemented as **one cohesive SQL migration by default** unless a concrete safety or implementation constraint forces a split.

The expected schema additions are:

- auth-related enum types as needed (at minimum `workspace_role_enum`)
- `users`, with deployed-identity linkage plus at minimum `create_workspace` and `superadmin` state
- `workspace_memberships`, with at minimum `(workspace_id, user_id, role)`
- a minimal token/principal storage model for bot/service/PAT access, with token hash, label, and lifecycle metadata
- canonical actor-attribution additions to `audit_log`, with `workspace_id` preferred for efficient workspace-admin audit queries
- supporting indexes and compatibility/backfill SQL

`database.md` must be updated to reflect the finalized post-migration schema.

## Invariants and non-goals

### Invariants

- Local mode resolves to a synthetic superadmin rather than introducing a second policy system.
- The server remains authoritative for authorization.
- The public permission model stays small: two global permissions and three workspace roles.
- Separate deployment of frontend, MCP, and server must remain supported.

### Non-goals for v1

- A large self-service token-management platform
- Per-endpoint custom grants exposed as a user-facing permission system
- Divergent local and deployed business logic beyond principal resolution/bootstrap differences
