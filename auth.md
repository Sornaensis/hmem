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

### Minimal bot identity surface

The v1 bot/service identity surface is intentionally small. A bot is not a separate account type with its own permissions table; it is an attributable actor plus a token and a grant-bearing user link.

The canonical persisted identity for deployed bot/service tokens is an `access_tokens` row with:

- `id` — stable token identity used as the `actor_id` for bot/service actors
- `grant_user_id` — the `users.id` whose global permissions and workspace memberships are evaluated
- `actor_type` — `bot` for service/agent tokens, or `user` only for user-attributed PAT-style tokens when needed
- `actor_label` — stable, non-secret display label used in audit, session/principal APIs, events, and UI attribution
- `token_hash` — hash of the bearer secret; raw token material is not stored in deployed token rows
- lifecycle timestamps: `expires_at`, `revoked_at`, `last_used_at`, `created_at`

For `actor_type = bot`, the resolved request principal should use:

- `actor_type = bot`
- `actor_id = access_tokens.id`
- `actor_label = access_tokens.actor_label`
- authorization authority from `grant_user_id`

For PAT-style `actor_type = user` tokens, the resolved request principal may use the grant-bearing user as both actor and authority, with `actor_label` still available to distinguish the token in audit and operator views. This is optional v1 behavior; bot/service tokens are the primary remote-agent path.

Bot labels are operator-facing attribution, not security boundaries. They should be:

- human-readable and non-secret
- stable enough that old audit rows remain meaningful after token rotation
- specific enough to distinguish agents, environments, or deployment roles, for example `codex-local`, `ci-indexer-prod`, or `support-agent-staging`
- shown consistently anywhere actor attribution is surfaced

Changing a label only affects future events unless historical audit rows are explicitly rewritten; audit rows store the label observed at write time.

### Local named bots vs deployed service tokens

Local mode keeps named bots lightweight so local agent workflows remain low-friction:

- `auth.local.bot_tokens[]` may define `{ label, token }` entries directly in local config or equivalent local bootstrap material.
- Matching local bot tokens resolve to `actor_type = bot` with a local label-derived actor id such as `local-bot:<label>`.
- During the transitional local compatibility period, these local bot principals may use the synthetic local superadmin authority for authorization while still preserving bot attribution.
- Local bot token material is local operator configuration; it must not be treated as the deployed token storage model.

Deployed mode treats agent access as governed service-token access:

- Bot/service tokens are persisted as hashed `access_tokens` rows.
- Each token must link to a `grant_user_id` with explicit global permissions and workspace memberships.
- Deployed bot tokens must not fall back to synthetic local superadmin authority.
- A deployed service token should normally be scoped through a least-privilege grant-bearing user rather than a broad superadmin user.

This distinction keeps local mode convenient while preserving the same principal and policy model in both modes.

### Token lifecycle v1

V1 token lifecycle is intentionally operator-managed. The goal is safe provisioning and rotation for agents without creating a full self-service token-management product.

#### Issuance

- Token issuance happens through config/bootstrap/operator workflows.
- Deployed token issuance creates a `users` grant row if needed, grants any required global permissions or workspace memberships, generates high-entropy bearer material, stores only `accessTokenHash(token)` in `access_tokens.token_hash`, and records a non-secret `actor_label`.
- Raw deployed token material is shown or exported only at creation time and should be stored by the operator in their secret manager or agent environment.
- Local bot token issuance may remain a local config edit or bootstrap step using `auth.local.bot_tokens[]`.

#### Rotation

- Create a replacement token row with the same `grant_user_id`, intended scope, and an updated or stable `actor_label`.
- Deploy the new raw token to the agent or service.
- Confirm use through `last_used_at` or operational checks.
- Revoke the old token by setting `revoked_at` or, for local config tokens, removing it from the local config and restarting/reloading the server as applicable.

Rotation should prefer overlapping validity windows so agents can switch without downtime, but old tokens should be revoked promptly after cutover.

#### Revocation and expiry

- Persisted token revocation is represented by `access_tokens.revoked_at`.
- Expiration is represented by `expires_at`; deployed service tokens should use finite expiration when operationally feasible.
- A token is valid only when it matches `token_hash`, `revoked_at IS NULL`, and `expires_at IS NULL OR expires_at > now()`.
- Revoked, expired, or unknown tokens resolve to no principal and should fail as unauthenticated.
- Local configured bot-token revocation is removal from local config/bootstrap material.

#### Observability

- Successful persisted token use updates `last_used_at`.
- Audit rows record `actor_type`, `actor_id`, and `actor_label`; they do not need to expose token hashes or raw token material.
- Operators should diagnose agent auth failures through safe auth error messages, token lifecycle fields, grant-user permissions, and workspace memberships rather than by inspecting raw tokens.

### V1 token-management boundary

V1 deliberately does **not** require:

- a self-service token creation UI
- end-user token rotation flows
- a separate bot-permission editor
- token-scoped permission grants independent of `users` and `workspace_memberships`
- per-endpoint token grants exposed as a public permission model

If a first-class token-management API is added later, it should preserve the same canonical storage and policy model: token rows provide authentication and attribution; `grant_user_id` provides authorization.

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

## Migration and bootstrap rollout plan

The auth rollout must support both fresh installs and upgrades from existing local single-token deployments. The migration and bootstrap sequence should preserve local usability by default while making deployed auth explicit and operator-controlled.

### Schema rollout and backfill sequence

Schema changes should be applied before any component assumes deployed auth is available.

1. Apply the base auth schema migration.
   - Create auth enum types such as `workspace_role_enum` and `actor_type_enum`.
   - Create `users`, `workspace_memberships`, and `access_tokens`.
   - Add canonical actor-attribution columns to `audit_log`.
   - Install audit triggers and indexes for the new auth tables.
2. Apply follow-up hardening migrations.
   - Migrations after the base auth schema may tighten audit, compatibility, or rollout behavior for databases that already applied the base version.
   - Security-sensitive corrections must be additive migrations, not edits to already-applied migration versions.
   - Example: the access-token audit redaction migration recreates the `access_tokens` audit trigger with `token_hash` ignored and scrubs existing `access_token` audit snapshots that may have copied `token_hash` during an earlier version.
3. Backfill only data that is safe and deterministic.
   - Existing workspace/task/memory/project rows do not need ownership backfills for authorization; access is evaluated through new principals and workspace memberships.
   - Existing audit rows may keep null actor fields; old rows should not be rewritten to invent principals.
   - Secret-adjacent material such as token hashes should be removed from historical audit snapshots when discovered.
4. Deploy auth-capable server code with compatibility defaults.
   - `auth.mode = local` remains the safe default for existing local installations.
   - Local bootstrap/session fallback keeps the implicit local human user usable without creating deployed user records.
   - Deployed mode remains opt-in until operators have configured identity provider verification or persisted service/PAT tokens and first-admin bootstrap data.
5. Switch frontend, MCP, and agents after server compatibility is present.
   - Frontend should obtain session/principal context from the server before showing protected data.
   - MCP should forward the configured bearer credential without defining authorization rules.
   - Agents should be moved to named local bot tokens or deployed persisted service tokens as appropriate.

All migrations must be idempotent where practical (`IF NOT EXISTS`, trigger replacement with `DROP TRIGGER IF EXISTS`, and safe cleanup statements) because local development and operator recovery often involve partially applied setup attempts.

### Bootstrap strategy

#### Local mode

Local mode bootstraps without requiring database user setup:

- The implicit local human principal is synthetic and superadmin-authorized when local bootstrap is enabled.
- Existing no-token local usage remains low-friction through server-owned local bootstrap/session behavior.
- `auth.local.bot_tokens[]` can define named local bot tokens for agent attribution.
- During the transitional compatibility window, configured local bot tokens and the legacy local static-bearer path may resolve to synthetic local superadmin authority, but only in `auth.mode = local`.
- Operators who want stricter local testing may still provision grant-bearing `users` rows and persisted `access_tokens`, but that is not required for the default local path.

Local bootstrap should never be used as a deployed-mode fallback. Changing to deployed mode must fail closed unless the request resolves through deployed user-token validation or persisted token lookup.

#### Deployed mode

Deployed mode needs explicit operator bootstrap before it can serve real users safely:

- At least one initial superadmin must be provisioned outside normal end-user authorization flows.
- First-admin provisioning may be an operator SQL/bootstrap command that creates or updates a `users` row with `is_superadmin = true`.
- If workspace creation should be delegated without full superadmin, bootstrap may also create grant-bearing users with `can_create_workspace = true`.
- Deployed bot/service tokens must be inserted as hashed `access_tokens` rows linked to a grant-bearing user with explicit global permissions and workspace memberships.
- Deployed token bootstrap should emit raw token material only once and store only `accessTokenHash(token)` in the database.

Recommended first-deployed bootstrap order:

1. Configure `auth.mode = deployed` plus provider verifier settings or persisted token lookup.
2. Create the first deployed superadmin `users` row, usually keyed by `auth_subject` from the identity provider.
3. Optionally create least-privilege grant-bearing users for service agents.
4. Grant workspace memberships or global permissions to those users.
5. Create persisted service/PAT tokens only after their grant-bearing user scopes are in place.
6. Verify `/api/v1/session` and a small protected read/write smoke flow before exposing the deployment broadly.

### Upgrade path from current single-token installs

Existing installations that use `auth.enabled` plus `auth.api_key` are treated as local static-bearer compatibility deployments, not as the long-term deployed auth model.

For local upgrades:

- Keep `auth.mode = local` unless the operator intentionally opts into deployed mode.
- Existing `auth.api_key` / `HMEM_API_KEY` continues to work only through the config-gated local legacy static-bearer path.
- Operators should prefer `auth.local.bot_tokens[]` for named local agents so audit and events distinguish agent actions from the implicit local human user.
- No database data migration is required to preserve the local human superadmin workflow.

For deployed upgrades:

- Do not reuse the legacy local static bearer as a deployed authorization mechanism.
- Create explicit `users` rows for deployed humans and grant global permissions or workspace roles.
- Replace shared static bearer usage with persisted hashed PAT/service-token rows linked to grant-bearing users.
- Move agent secrets to deployment secret management and provide them to MCP/agents via `HMEM_MCP_AUTH_TOKEN`, `HMEM_AUTH_TOKEN`, or equivalent runtime configuration.
- Revoke/remove legacy local static bearer configuration once deployed token lookup and provider-backed user auth have been verified.

Operators should treat the transition as a staged cutover: schema first, server compatibility second, explicit principals and tokens third, client/agent credential switch fourth, and cleanup of legacy local-only credentials last.

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
