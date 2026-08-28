# Canonical change-stream contract

## Decision and scope

This is the binding contract for the planned canonical change stream. A
sanitized database outbox/change log, written in the **same database
transaction** as a material mutation, is the sole source of change records.
After commit, a dispatcher may deliver retained records through WebSocket or a
future transport. WebSocket is a transport, never the record of truth.

The contract applies equally to REST handlers, audit reverts, bulk/core calls,
database-triggered effects, and future writers. MCP remains a REST-proxy
compatibility path; calling core/database code directly is a required writer-
independence test, not an alternate event mechanism. It does not implement
runtime behavior, exactly-once delivery, collaborative editing, SavedView, or
auth-administration events.

`HMem.Server.API.emit` and its in-memory `ChangeEvent` are legacy
handler-local delivery. They must not be used by canonical writers.

## Record, scope, and visibility

Each material committed row mutation creates one canonical record in the
outbox. The write path allocates `event_id` and a strictly increasing `cursor`
under the transaction's scope counter/lock, then writes the affected row(s)
and outbox row(s) before commit. `workspace` scope uses that workspace ID;
`global` scope is for the workspace catalogue and workspace-group lifecycle.
The cursor is monotonic only within `(scope, workspace_id)`; there is no
cross-scope order, cursor comparison, or atomic multi-scope stream.

No record is observable before commit. A failed transaction, rollback, rejected
operation, and an UPDATE/DELETE affecting no material row publish nothing.
Delivery failures never roll back the committed mutation; the dispatcher
retries from the retained outbox. A listener restart therefore resumes from its
internal outbox watermark rather than inventing events.

Canonical envelope, persisted in the outbox as a versioned JSON object:

```json
{
  "schema_version": 1,
  "event_id": "uuid",
  "scope": "workspace",
  "workspace_id": "uuid-or-null-for-global",
  "cursor": 42,
  "occurred_at": "RFC-3339 UTC timestamp",
  "transaction": { "id": "uuid", "cause": "rest|mcp|audit_revert|core|migration", "request_id": "uuid-or-null" },
  "actor": { "type": "user|service|system", "id": "opaque-or-null" },
  "entity": { "type": "task", "id": "uuid", "action": "created|updated|deleted|restored" },
  "invalidations": [{ "kind": "entity|collection|rollup", "target": "..." }],
  "payload": { "allowlisted REST-shaped fields only" }
}
```

`schema_version`, `event_id`, scope, cursor, `occurred_at`, and entity/action/id
are required. `transaction.id` is also required and non-null: it is a newly
allocated opaque UUID for every committed canonical database transaction, even
when no caller supplied a request ID. Every outbox row from that transaction,
including cascades and trigger-derived rows, carries that exact ID.
`transaction.cause` is required and names the initiating writer; `request_id`
and actor values are nullable metadata, never authorization proof. `payload` is optional: a complete,
redaction-reviewed canonical REST-shaped projection may be patched by a client;
otherwise it is omitted and the client refetches only the listed invalidation
targets. Delete records retain identity and invalidations, not the deleted
snapshot. The dispatcher uses the canonical `cursor` only to order and retain
records; it omits that field from the subscriber-visible event projection. The
remaining fields are still subject to the authorization and redaction rules
below.

## Redaction and authorization

Invalidation-first is the default. Payloads are allowlisted per entity and must
match an authorized REST representation. The stream must never contain
`audit_log.old_values`, `audit_log.new_values`, access-token/session material,
ticket/bearer secrets, raw embeddings, internal audit snapshots, or unreviewed
metadata. Observation content/provenance is included only if its normal REST
read policy permits it; embedding updates always invalidate/refetch.

Before dispatch/replay, authorization is evaluated for each current subscriber
and record. Workspace subscribers receive only their workspace scope; global
records require the current global-superadmin entitlement. A past subscription
or recorded actor does not grant future access.

Canonical cursors are server-internal ordering and retention positions; they
are never sent to subscribers. A subscription instead has an audience-specific,
opaque resume token. The server maps the token to the current authorization
audience, its authorization epoch, and an internal retained-outbox watermark.
Tokens are unguessable, rotate on successful replay and authorization changes,
and are not derived from, numerically ordered with, or advanced once per
canonical cursor. A client receives only records it is currently authorized to
see, plus a replacement token after a completed delivery batch. It receives no
placeholder for a hidden record. This avoids exposing hidden-event occurrence,
count, ordering, or timing through frames or cursor changes.

On replay, the server uses the token's internal watermark to find retained
records, filters them using current authorization, delivers the permitted
records in canonical order, and atomically stores a replacement token whose
watermark is after the scanned range. A token is valid only for its audience
and authorization epoch. An absent, invalid, expired, superseded, or
retention-pruned token does not reveal why it failed and requires resync; it
cannot be used to infer a hidden gap. A connection that loses workspace access
is sent a targeted `access_revoked` control, clears the workspace session/cache,
and is closed; it receives no further frames for that scope. These controls and
resume tokens are transport protocol state, not canonical outbox events.

## Coverage matrix

Every listed source mutation is material. `writer` includes any REST handler,
audit-revert path, MCP-through-REST invocation, direct core/database call, or
trigger/cascade that can perform it. “Refetch” means refetch the precise target
named in `invalidations`; a payload is optional as specified above.

| Subject | Writers and source table/operation | Scope; entity/action | Payload or invalidations | Derived impacts | Client action |
| --- | --- | --- | --- | --- | --- |
| Workspace catalogue | workspace registration/lifecycle writer; `workspaces` INSERT/UPDATE/soft-delete/restore | global; `workspace` created/updated/deleted/restored | allowlisted workspace or `workspace-catalog` | group membership and workspace selection | patch if allowed; otherwise refetch catalogue |
| Workspace membership | `Auth.upsertWorkspaceMembership`, `Auth.deleteWorkspaceMembership`, creator grant, REST/MCP-through-REST, or direct core writer; `workspace_memberships` INSERT, `ON CONFLICT DO UPDATE`, or DELETE | affected workspace; `workspace_membership` created/updated/deleted, identity `workspace_id:user_id` | no membership payload. Invalidations are `collection/workspace:{id}:memberships` with `audience: workspace-admins`, plus `catalogue/workspace-catalog`, `session_authorization/session-authorization`, and `permission_cache/permission-cache`, each with `audience: user:{affected-user-id}` | workspace visibility (`listVisibleWorkspaces`), role permissions, current subscriptions, admin member list | administrators refetch memberships; an affected user's authenticated connections receive `access_granted` or `access_revoked`: grant invalidates/refetches their workspace catalogue then opens the workspace stream, while revoke clears that workspace's entities/selection and permission cache, aborts in-flight workspace requests, and closes/reconnects only after a fresh catalogue fetch |
| Workspace group | group REST/core writer; `workspace_groups` INSERT/UPDATE/DELETE | global; `workspace_group` created/updated/deleted | allowlisted group or `workspace-groups` | member lists and global access view | refetch groups; privileged clients only |
| Group membership | group-member REST/core writer; `workspace_group_members` INSERT/DELETE, including group/workspace cascade | affected workspace; `workspace_group_membership` created/deleted | `group:{id}:members` and affected `workspace:{id}:groups` | recipient authorization and group views | refetch group members/workspace groups; reconnect if access changes |
| Observation | REST/core/MCP writer; `observations` INSERT/UPDATE/DELETE plus creation of `observation_subjects` | workspace; `observation` created/updated/deleted | allowlisted observation only after review; otherwise observation/entity and observation-list/query invalidations | subject search vector and provenance views | normally refetch affected list/entity |
| Observation embedding | embedding writer; `observations` embedding UPDATE | workspace; `observation` updated | observation entity, similarity/search invalidations; no embedding payload | similarity/query results | refetch observation/search results |
| Project | REST/core/MCP/audit revert; `projects` INSERT/UPDATE/soft-delete/restore | workspace; `project` created/updated/deleted/restored | allowlisted project or project entity/list/tree invalidations | project overview, next tasks, readiness and parent tree | patch only reviewed projection; otherwise refetch entity/tree/rollups |
| Task | REST/core/MCP/audit revert/batch writer; `tasks` INSERT/UPDATE/soft-delete/restore/move | workspace; `task` created/updated/deleted/restored | allowlisted task or task entity/list invalidations | task overview, parent/project readiness, next tasks, dependent readiness | refetch listed overview/rollups |
| Dependency edge | REST/core/MCP writer; `task_dependencies` INSERT/DELETE | workspace; `task_dependency` created/deleted, with edge ID `task_id:depends_on_id` | edge, both task overviews, and affected readiness rollups | auto-block/status changes and next-task candidates | refetch edge/task overviews and rollups |
| Project/task cascade | project/task delete, purge, or database cascade; affected `projects`, `tasks`, `task_dependencies` operations | workspace; one record **per affected project, task, and dependency edge**, each deleted | identity plus entity/list/tree/rollup invalidations; no aggregate-only substitute | all affected parent/project/task/dependency projections | apply/reload in delivery order; refetch invalidations |
| Trigger-derived status/readiness | lifecycle/auto-blocking/search-vector triggers and SQL that materially updates `tasks`/`projects` | workspace; affected row gets its own `task`/`project` updated record; pure computed projection gets no snapshot event | source entity plus task/project readiness and next-task invalidations | status, completion gates, readiness, search | refetch invalidated rollups/query results |

Unsupported core-only surfaces (including SavedView and auth administration)
must neither silently publish nor borrow a nearby entity type. They require a
future matrix row and allowlist before becoming canonical. Pure reads and
computed query changes do not create records.

Cascade records share the required `transaction.id` and set `cause` to the
initiating operation (for example `audit_revert` or `rest`); they retain
individual entity/action identities. Trigger-derived *material row updates* are
recorded individually. Derived projections such as readiness rollups,
next-task results, and search results are invalidated from their source
mutations, not emitted as unsafe snapshots.

## Consumption, replay, and recovery

Delivery is at-least-once. Consumers deduplicate by `event_id`, retain the last
accepted opaque resume token independently for each scope, and apply only
supported schema versions. They must tolerate duplicate delivery and transport
reordering without trying to infer a canonical gap or ordering from tokens. An
unknown future `schema_version`, malformed record, rejected token replacement,
or failed client ordering check pauses incremental application and triggers
scoped resync rather than guessing.

Fresh connection and resync use an atomic snapshot-to-resume-token handoff,
never an ordinary REST fetch followed by an independently observed current
cursor. The client starts it with `POST /api/v1/change-stream/resync`, no page
token, a bounded `page_size`, and a client-generated opaque
`start_idempotency_key` with at least 32 characters of CSPRNG material. The
server stores only the key hash alongside the immutable session and its page
size, while its snapshot bearer comes from server-generated random session
lineage: retrying a lost response with the same live key returns the original
first page/session, while retrying after expiry starts a fresh lineage that
cannot collide with an older resume bearer. Continuations cannot change the
stored page size. For each start attempt, the server opens a serializable (or
equivalent transactionally consistent) transaction and **first** acquires the
scope counter lock. Every membership/role mutation that can change access to
that scope uses this same lock before its membership write and outbox write.
While holding the lock and in that transaction's snapshot, the server
evaluates authorization, reads the internal scope high-water cursor `H`,
materializes the authorized, redaction-reviewed REST-shaped snapshot as
immutable ordinal items, and creates an opaque pending snapshot-session token.
Global snapshots include only public workspace and workspace-group DTOs; they
never include workspace-group membership rows. Membership state is instead
handled through targeted authorization controls and invalidations.
The transaction commits before the first page is returned. It does **not**
keep a database transaction open between HTTP pages. A serialization failure
retries the whole start transaction; failed authorization rolls back and
returns a denial only--never a snapshot, high-water value, session token, or
resume token.

Membership upserts acquire the workspace counter lock even when their role and
grantor are unchanged, but those no-op writes neither advance the authorization
epoch nor emit an outbox record. Disabling or re-enabling a user increments the
epoch for each of that user's current workspace memberships and, when the user
is a superadmin, for the global scope. Multi-scope invalidation locks workspace
counters by ascending workspace UUID and locks the global counter last.

The first response contains the first ordinal page and a next page token when
more items remain. Later `POST` calls carrying that opaque body page token
read the exact same immutable ordinal range; retries return the same page,
continuation, and terminal items rather than consuming a token. Each one rechecks current
authorization, the audience authorization epoch, and session expiry before
returning data. The terminal page atomically marks the session terminal and
creates/returns the audience-specific opaque resume token at `H`. Repeated
page calls are idempotent while the session is valid, but a token is never
activated before the terminal page. Any expired, mismatched, revoked, or
epoch-invalid session requires a fresh resync and reveals no partial snapshot
or hidden cursor.

Defaults are configurable: retained outbox records are kept for 604800 seconds
(7 days); resume tokens expire 86400 seconds (24 hours) after terminal
activation; pending snapshot sessions expire after 300 seconds. The settings
are `change_stream.retention_seconds`,
`change_stream.resume_token_ttl_seconds`, and
`change_stream.snapshot_session_ttl_seconds`, with environment overrides
`HMEM_CHANGE_STREAM_OUTBOX_RETENTION_SECONDS`,
`HMEM_CHANGE_STREAM_RESUME_TOKEN_TTL_SECONDS`, and
`HMEM_CHANGE_STREAM_SNAPSHOT_SESSION_TTL_SECONDS`. Configuration validation
requires the resume-token plus session TTL budget to be strictly less than
outbox retention.

Trusted MCP attribution additionally requires the bridge's private
`HMEM_MCP_PROVENANCE_TOKEN` to match the server's
`auth.mcp_provenance_token`. The forwarded user bearer and an
`X-HMem-Change-Cause` header alone always remain REST.

Consequently the snapshot contains every committed material mutation through
the internal `H`; the next writer can allocate only `H + 1` or later. The
client discards that scope's cached state, applies the snapshot, persists the
opaque `resume_token`, and establishes the stream with it. Events committed
between that commit and stream establishment are retained and replayed, so none
can be lost. A terminal checkpoint writes its deterministic successor bearer
before acknowledging the delivered page; a failed socket write leaves the old
bearer valid for at-least-once reconnect. The server reauthorizes the subscription and filters replay as
specified above. An absent, invalid, expired, superseded, or retention-pruned
resume token, a malformed record, unknown schema, or failed client ordering
check requires this handoff again. A membership grant
uses `access_granted` on the user's authenticated control channel to invalidate
the workspace catalogue; the client then performs this handoff before
subscribing. A membership revoke uses `access_revoked` and must not attempt to
continue the old workspace resume token. A dispatcher/listener restart replays
undelivered retained records; it does not depend on connections that existed at
commit time. Reconnects are therefore safe without a dual in-memory broadcast
path.

## Rollout and retirement

1. Add transactional outbox, scope counters, retention, authorization-aware
   replay, and instrumentation while still delivering only the legacy stream.
2. Shadow-verify canonical records against the matrix without exposing them to
   clients; exercise REST, audit revert, MCP proxy, and direct core/database
   writers.
3. Cut each client population to canonical replay in one release boundary.
   For a population, delivery is **legacy or canonical, never both**; do not
   fan out the same mutation through `API.emit` and the outbox.
4. Once every consumer uses canonical delivery, remove all `API.emit` call
   sites, the `Broadcast` dependency, and the legacy `ChangeEvent` protocol.
   The outbox remains the sole record source.

## Downstream test matrix

| Layer | Required evidence |
| --- | --- |
| Database/core | One outbox row per committed material row; scope-local contiguous cursors; required shared transaction IDs for cascade/trigger rows; no row on no-op/rollback; workspace-membership create/update/delete; cascade rows/edges; trigger-derived rows and invalidations; redaction assertions. |
| REST/audit | Create/update/delete/restore and audit-revert records have transaction/request/cause metadata; no handler-local emit; payload allowlists match REST authorization. |
| MCP | Each mutating MCP tool reaches the REST path and yields the same canonical record; direct core/database invocation yields an equivalent record without REST/MCP. |
| WebSocket/dispatcher | Post-commit only; current authorization filtering with audience-specific opaque resume tokens; policy test that inserting/removing a hidden record does not change a subscriber's frames, token contents, or observable timing; replay returns a replacement token advanced only through its last scanned cursor plus explicit `has_more`, audience/epoch rejection, retention expiry, and resync; membership grant/revoke controls; duplicate delivery; atomic serializable snapshot-to-token handoff with response-loss retries for nonterminal and terminal pages; revoke-versus-`begin_resync` interleavings (revoke first yields denial with no snapshot/token; resync first yields an authorized snapshot, then revoke prevents further delivery); reconnect; and listener restart. |
| Elm/front end | Per-scope resume-token/event-id persistence; replacement-token handling without state mutation; unknown-version and ordering-failure resync; invalidation refetch; no unsafe patch; workspace-membership grant/revoke clearing/catalogue refresh/reconnect behavior; and workspace/global authorization changes. |
| Two clients | Concurrent same-workspace mutations, duplicate/reordered delivery, cascades, reconnect during dispatch, and no legacy/canonical double application. |

## Repository cross-check

This contract is grounded in the current handler-local event model in
`hmem-server/src/HMem/Server/Event.hs`, `API.hs`, and `WebSocket.hs`; the
MCP tool catalogue in `hmem-mcp/src/HMem/MCP/Tools.hs`; project/task cascade
and dependency writers in `hmem-core/src/HMem/DB/Project.hs` and `Task.hs`;
audit, lifecycle, cascade, and observation triggers in
`hmem-server/migrations/`; and the existing event decoding/refetch behavior in
`hmem-server/frontend/src/Api.elm` and `Feature/WebSocket.elm`.
