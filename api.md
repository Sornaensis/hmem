# API reference

The HTTP API is available at `/api/v1` on your hmem server, normally
`http://127.0.0.1:8420` for a local installation. Requests and responses use JSON.

## Connect and discover endpoints

For shared access, choose a credential using [the authentication guide](auth.md):
service and MCP clients can forward a bearer token, and the browser normally uses
an OIDC session. Local native bootstrap supports tokenless API and MCP access;
the default Docker setup requires a token. Cookie-authenticated POST, PUT, and
DELETE requests need the CSRF header, including POST endpoints that only search
or count.

With your token in `HMEM_AUTH_TOKEN`, check your session and fetch the server's
OpenAPI document:

```bash
curl --fail-with-body -H "Authorization: Bearer $HMEM_AUTH_TOKEN" \
  http://127.0.0.1:8420/api/v1/session
curl --fail-with-body -H "Authorization: Bearer $HMEM_AUTH_TOKEN" \
  http://127.0.0.1:8420/api/v1/openapi.json -o hmem-openapi.json
```

Use that OpenAPI document for the current routes, request fields, and response
schemas. Send `Content-Type: application/json` with JSON bodies. Use HTTPS for
shared installations. [MCP setup](README.md) shows how to use hmem through tools;
its startup declares a workspace (or null) and bot identity, with a bearer
credential only when required by the server.

## Declared attribution

Automated HTTP callers send one JSON-valued `X-HMem-Bot-Identity` header. This
local example uses bootstrap without credentials:

```bash
curl --fail-with-body \
  -H 'X-HMem-Bot-Identity: {"role":"Reviewer","model-id":"pi:gpt-6.1-sol:extra-high"}' \
  http://127.0.0.1:8420/api/v1/session
```

For a credential-protected server, also send `Authorization: Bearer $HMEM_AUTH_TOKEN`
(or the normal cookie and required CSRF header). Known credential-authenticated
bots must include the declaration on unsafe `/api/v1` requests, including POST
search/count operations. Humans and undeclared local bootstrap requests may omit
it. See [validation and local persona rules](auth.md#declared-bot-identity) for
UTF-8 bounds, canonicalization, and deterministic same-declaration grouping.

| Response | Meaning |
| --- | --- |
| 400 `invalid_bot_identity` | Malformed, repeated, oversized, or invalid declaration, even on a read or WebSocket upgrade. |
| 400 `bot_identity_required` | A known credential bot omitted the declaration on an unsafe API request. |
| 401 / 403 | The ordinary authentication, authorization, or cookie CSRF check failed; declaring a role/model grants no access. |

The session response retains the original `principal`, global permissions, and
workspace access. Its optional `effective_actor` is attribution only. Audit,
Timeline, Observation history and `current_provenance` expose optional
`bot_identity` using the public keys `role` and `model-id`. Canonical live events
carry it inside `actor`, whose types remain `user`, `service`, and `system`; bots
use `service`. Snapshot/resume/ticket audiences stay bound to the original
principal across declaration changes. Historical payloads without the new
metadata remain valid and are not retroactively attributed.

## Workspace lifecycle graph

`GET /workspaces/{id}/timeline/buckets` exposes canonical `series` counts for
`created`, `completed`, `deleted`, `archived`, and `cancelled`. Legacy `counts`
and `totals` retain their existing meaning. Observation archive and cancel counts
are zero because Observations do not support those lifecycle actions.

Each bucket also returns nullable `open_counts: {projects, tasks}`, nullable
`open_as_of` (UTC), and `open_status`. Available inventory counts every live
active/paused project and todo/in_progress/blocked task once, including nested
projects, parents, subtasks and unassigned tasks. It is independent of lifecycle
line filters. The exclusive sample cutoff is the earliest of bucket end,
requested `until`, and the server statement snapshot time. Events exactly at
that cutoff belong to the next sample. A partial current bucket uses current
base rows; wholly future buckets return null counts/time and `future`.

Historical inventory includes the pre-range baseline and carries through buckets
without events. It reverses full old/new audit snapshots from current inventory,
including deletion, restoration and reopening; no present-day hierarchy joins
or lifecycle-event subtraction determine inventory. Retained legacy audits with
null workspace columns use their snapshot workspace IDs. Missing initial
history, unusable snapshots or failed per-entity reconciliation return null
counts and `history_unavailable` for affected history, retaining its cutoff.
Available samples use `available`. Retained audit history is assumed intact:
arbitrary manual purges that leave consistent snapshots cannot always be detected.

The optional `paged=true` query supports long graph ranges while each response
keeps the existing ten-year and 366-bucket bounds. Follow `next_since`, keeping
`until` and `bucket` unchanged, until it is null. With `paged=true`, omitting
`since` starts at the workspace's earliest eligible lifecycle audit event. Without
the opt-in, `since` remains required and oversized ranges still return HTTP 400.

## Workspaces and planning

Workspaces scope your data and permissions. Create one with
`POST /api/v1/workspaces`, using `name` and `workspace_type` (`repository`,
`personal`, `organization`, or `planning`). Creation needs the global `create_workspace` grant
or superadmin access. List accessible workspaces with `GET /api/v1/workspaces`.

Read `GET /api/v1/workspaces/{workspaceId}/planning-counts` with workspace read
access for `{ "open_projects": 3, "open_tasks": 12 }`. These authoritative totals
count each live active/paused project and todo/in_progress/blocked task once,
including unassigned tasks and subtasks. Deleted and terminal rows are excluded.
Both counts use one consistent snapshot and are independent of loaded
navigation pages, filters and overlapping project subtree rollups. Unauthenticated,
forbidden and missing/deleted workspace requests retain the usual 401/403/404 behavior.

Projects and tasks can belong to any workspace. Observations require an active
`repository` workspace, including when requested by ID. Use `workspace_id` in
workspace-scoped requests; possession of a resource UUID does not grant access.

For planning clients, create projects/tasks through their OpenAPI routes or use
`POST /api/v1/projects/spec` to create a project with 1–50 initial tasks.
Tasks allow one level of subtasks. Parents with incomplete subtasks automatically
become blocked and cannot start. Subtasks can start beneath a todo or automatically
blocked parent when its prerequisites and ancestors permit work; manual parent
blocks prevent child starts. Finish or cancel open children before marking a task
done or a project completed. A lifecycle conflict returns HTTP 409 with a
`code`, `message`, and optional `detail`/`hint`; resolve the reported blockers
before retrying. Archiving a project and cancelling a task can affect descendants,
so check the returned result rather than assuming only one record changed.

Task overview and navigation readiness summaries derive `in_progress_subtask_count`
and `can_start_subtasks` from the database, including children outside the loaded
view. Closing the last child returns an automatically blocked parent to todo once
its prerequisites are closed; starting or completing the parent remains explicit.

Most list routes return `{"items":[...],"has_more":true}`. Start at `offset=0`,
advance by the number of returned items, and stop when `has_more` is false.
Ordinary list pages default to 50 items and allow up to 200; navigation pages
allow up to 100. Observation limits are 1–200 and offsets 0–100000. Check OpenAPI
for route-specific pagination, especially unified search and similarity, whose
response shapes differ.

## Removed due fields

Current task and subtask JSON omits due fields in detail, lists, navigation
summaries, overviews, mutation/dependency effects, project-embedded tasks, and
fresh snapshots. Project JSON was already due-free. MCP tool discovery and
compact task results also omit due properties; completion timestamps, hierarchy,
priority, readiness and lifecycle rules retain their existing meaning.

The exact keys `due_at`, `dueAt`, `due`, `due_date`, and `dueDate` are rejected
at modeled planning input boundaries, including an explicit null. This covers
project/task create and update bodies, project specifications and their nested
task entries, batch-update wrappers/items, task batch-move bodies, and dependency
link bodies. MCP also checks planning create/update/spec/archive, task
move/dependency/start/finish
arguments before sending HTTP. Remove these keys rather than setting them null.

REST JSON decoding returns HTTP 400 with parser error text containing
`Due scheduling fields are no longer supported`; this is not a dedicated JSON
error code. MCP returns a tool result with `isError: true` and a text content
message, without dispatching the rejected HTTP request. Unrelated unknown keys
retain their existing handling; the check does not recursively scrub free-form
`metadata` or saved-view query data. Other nullable update fields retain their
omit/null/value semantics. For example, this task update is due-free:

```json
{"title":"Prepare release","description":null,"priority":7}
```

Authorized historical task-update replay applies only changed supported mutable
fields (`title`, `description`, `project_id`, `parent_id`, `status`, `priority`,
`metadata`). Mixed historical updates can revert those fields while ignoring
obsolete due values; unchanged placement/status do not establish new intent.
An update that changed only due fields returns HTTP 409 before any write:

```json
{"error":"audit_due_operation_obsolete","message":"This historical update only changed due scheduling, which is no longer supported."}
```

Historical payloads remain inspectable. Existing authorization, create/delete and
soft-delete replay behavior still applies; Observation reversion remains
unsupported. Retired due-bearing snapshot tokens require a fresh canonical
resync; see [V038 storage behavior](docs/reference/schema.md#due-field-removal-v038) and the
[upgrade procedure](docs/reference/upgrades.md#upgrading-across-v038).

## Create an Observation

An Observation records an insight about repository files at a Git revision.
For example, send this body to `POST /api/v1/observations` with edit access:

```json
{
  "workspace_id": "00000000-0000-0000-0000-000000000001",
  "subjects": [
    {"subject_kind": "file", "subject": "src/Main.hs"},
    {"subject_kind": "glob", "subject": "src/**/*.hs"}
  ],
  "git_sha": "0123456789abcdef0123456789abcdef01234567",
  "content": "The server loads configuration before connecting to PostgreSQL."
}
```

Replace the example UUID and SHA with your workspace and reviewed revision.
SHAs must be 40 lowercase hexadecimal characters. Subjects use repository-relative
forward-slash paths without `./`, `..`, absolute paths, or backslashes.
Glob subjects support `*`, `?`, and whole-component `**`; file subjects are
concrete paths. Supply 1–256 subjects, each at most 4096 UTF-8 bytes and together
at most 256 KiB. Content must be nonempty and at most 512 KiB.

Creation returns the Observation, including its ID and `content_version`.
Its workspace, ordered subjects, and original `git_sha` cannot be changed.
Create a new Observation when correcting that identity. The server accepts your
revision assertion; it does not inspect a Git checkout.

## Update without overwriting another edit

Read the Observation and keep its `content_version`. Send that UUID as one
strong quoted `If-Match` header, with the new content and the revision at which
you reviewed it:

```http
PUT /api/v1/observations/00000000-0000-0000-0000-000000000010
Content-Type: application/json
If-Match: "00000000-0000-0000-0000-000000000002"

{"content":"Updated repository insight","reviewed_git_sha":"0123456789abcdef0123456789abcdef01234567"}
```

Add your authentication header, or session cookie and CSRF header.

| Response | Next step |
| --- | --- |
| 200 | Keep the returned Observation and its new version. |
| 428 `observation_content_precondition_required` | Read the Observation and supply `If-Match`. |
| 400 | Correct the body or header. Use one quoted lowercase UUID; weak tags, `*`, lists, repeated headers, and unquoted values are invalid. |
| 409 `observation_content_conflict` | Review the returned `latest` Observation, reconcile your draft, and deliberately retry with its version. |
| 404 | The Observation is absent or deleted; do not recreate it automatically. |

A successful same-text update is a new review assertion and advances the version.
Content writes clear the stored embedding, so similarity needs a replacement
vector. Embedding-only writes leave the content version unchanged.
`X-Request-Id` identifies a request; it does not deduplicate repeated updates.

MCP `observation_update` uses `observation_id`, `content`, `reviewed_git_sha`,
and `expected_content_version` instead of an HTTP header.

## History and revision filters

`GET /api/v1/observations/{id}/history?limit=50&offset=0` returns
`{items,has_more}` in descending sequence order, with a maximum page size of 200.
MCP exposes `observation_history`. History records revision assertions, time,
and actor attribution; it contains no previous content or diffs and cannot
restore an Observation. Older creation claims may have unknown attribution and
no binding to current content. A nullable `current_provenance` in an Observation
means there may be no reviewed assertion for its current content.

For Observation listing, search, matching, and similarity:

| Filter | Matches |
| --- | --- |
| `git_sha` | Original creation revision. |
| `current_git_sha` | Revision asserted for current content. |
| `history_git_sha` | Any recorded revision claim, including older creation claims. |

Supplied filters combine with AND. A historical match returns current content,
not content as it existed at that revision. If history changes while paging,
restart at offset 0 to refresh.

## Match files and search by vector

Send concrete repository-relative paths to `POST /api/v1/observations/match`:

```json
{"workspace_id":"00000000-0000-0000-0000-000000000001","paths":["src/Main.hs",".github/workflows/ci.yml"],"limit":50,"offset":0}
```

The response includes each matching Observation once with `matched_paths`,
`matched_subjects`, and `path_matches`. Paths match stored file/glob subjects;
the request does not expand caller globs or read your filesystem. Path count and
byte limits are the same as the subject limits above.

For similarity, supply an externally generated vector to
`POST /api/v1/observations/similar` or MCP `observation_similar`.
Store an individual vector with `PUT /api/v1/observations/{id}/embedding` or MCP
`observation_set_embedding`. Vectors need 1536 finite numbers and matching
embedding spaces. There is no raw-text query embedding endpoint. See
[embeddings](embeddings.md) for setup, request compatibility, and bulk import.

## Deletion and errors

Observation deletion is permanent, including its history. Workspace deletion
requires admin access and soft-deletes the workspace while retaining its contents.
Project/task deletion also retains records for audit-based restoration; permanent
purge requires admin access. Workspace deletion has no MCP tool.

HTTP 401 means the credential needs attention; 403 means permission or CSRF is
missing. A 503 `capability_unavailable` on vector operations means pgvector is
not ready. Check [authentication](auth.md) or [embedding readiness](embeddings.md)
for recovery. For validation and lifecycle errors, use the response message and
hint; avoid blindly retrying the same rejected request.

## Pause and resume project work

Set a project's `status` to `paused` with `PUT /api/v1/projects/{id}`.
Open descendant projects inherit pause, and unfinished tasks and subtasks become
automatically blocked. Any live running descendant prevents pause atomically,
including running historical work in a frozen branch. A completed or archived
root must first be reopened as `active`.

Setting `active` clears that project's independent pause. Descendants with their
own pause, or with another paused ancestor, remain paused. Eligible automatically
blocked tasks return to `todo` when all project, dependency and subtask blockers
clear. Manual blocks and done/cancelled tasks are preserved.

Project update responses retain the plain Project fields and add `pause_effects`.
It contains `project_count`, `task_count`, deterministic `projects` and `tasks`
samples (at most twenty of each), `has_more_projects`, `has_more_tasks`,
`remaining_own_pause`, and `remaining_ancestor_pause`. Refresh affected tree,
readiness and next-task views when a sample is truncated. Task effects include
prior/current status, automatic blocking, dependency/subtask counts and
`previous_project_paused`/`project_paused`, so a pause cause can be added or
removed while another cause still blocks the task. MCP keeps these counts,
causes and retained-pause guidance in its compact acknowledgement.

Next-task discovery excludes effectively paused work by default.
`include_blocked=true` includes paused diagnostic candidates, including subtasks,
with `project_paused=true`. Use the authoritative readiness rollup to decide
whether subtasks can start. Lifecycle conflicts return a code, message, bounded
blocker IDs/count and a recovery hint; rejected cascades leave no partial events.

Audit reverts require workspace admin access. Metadata reverts preserve current
pause intent; pause transitions restore their recorded independent and retained
intent through the same lifecycle guards. `own_paused` and `retained_paused` are
read-only canonical audit provenance and cannot be set through public project
input. Some historical transitions predate retained provenance: an old paused
snapshot with known false own intent but missing retained intent is ambiguous.
Such a transition returns `409 audit_pause_provenance_missing` after authorization;
choose the desired project status explicitly. Metadata and unambiguous historical
transitions remain usable. Malformed stored provenance returns
`409 audit_pause_provenance_invalid`.

Expanded project cards consume `GET /api/v1/projects/:id/next-task-page` to exhaustion. It returns `{items, has_more}`, defaults to 50 items, caps pages at 100, and accepts `offset` and `include_blocked`. Diagnostic pages include parent-gated subtasks with `parent_blocked`; closed/deleted placements stay excluded. The legacy `next-tasks` list keeps its existing limits and candidate eligibility. Dependency summaries in task overviews and dependency pages include authoritative `status` for closed-work grouping.
