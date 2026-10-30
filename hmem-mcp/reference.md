# MCP context reference

After the MCP initialization handshake, call `set_workspace` before ordinary
tools. Both `workspace_id` and `identity` are required:

```json
{"workspace_id":null,"identity":{"role":"Developer","model-id":"codex:gpt-6.1-sol:high"}}
```

A null workspace allows `workspace_list` and `workspace_register` before selecting
a workspace. Select one by calling `set_workspace` again with its UUID and the
complete identity:

```json
{"workspace_id":"00000000-0000-0000-0000-000000000001","identity":{"role":"Developer","model-id":"codex:gpt-6.1-sol:high"}}
```

These are `set_workspace` arguments; in a raw JSON-RPC `tools/call`, put them in
`params.arguments` and use `params.name: "set_workspace"`. Scoped tools can then
omit `workspace_id`; an explicit value on an ordinary call takes precedence.
Missing workspace values still fail scoped
tool validation when no workspace is selected.

The role is nonblank and limited to 128 UTF-8 bytes; `model-id` is limited to 256
UTF-8 bytes. Both limits apply before trimming. Model-id has exactly three
nonempty `harness:model:reasoning` components.
Components use ASCII letters, digits, `.`, `_`, `/`, or `-`. Outer whitespace is
trimmed, control and format characters are rejected, and identity accepts only
`role` and `model-id`. Invalid declarations leave the previous workspace and
identity unchanged. `get_workspace` and successful `set_workspace` return the
accepted values, without claiming a server-resolved actor ID.

Setting a null workspace clears only the workspace and accepts the supplied
identity. Repeating `initialize` preserves the context. A fresh MCP process starts
without either value and requires its own declaration. Protocol/tool discovery
and the local context controls are available before declaration; all HTTP
forwarding requires an accepted identity, including calls with explicit workspace
IDs and global workspace tools.

Each accepted ordinary request captures the complete context. Context controls
drain earlier queued and active requests before changing it; ordinary requests
within an epoch remain concurrent. Both HTTP forwarding paths send one
`X-HMem-Bot-Identity` header containing the accepted JSON identity. Declaration is
attribution metadata: existing bearer credentials and MCP provenance are forwarded
unchanged, and local tokenless operation requires no new credentials. Launch
with `--no-auth` to suppress configured credential forwarding explicitly. Shared
servers still require their ordinary credentials and grants.

## Errors and compatibility

Session summary/detail and canonical delivery metadata include nullable
`last_activity_at`, the maximum eligible transcript occurrence. Missing fields
from older servers decode as unknown. This value is independent of uploader
acknowledgements, `last_seen_at`, lifecycle and task connections. Pending or
unrecognized evidence stays null; see [occurrence rules](../docs/reference/session-occurrence.md).

Before declaration, ordinary calls return a tool result with `isError: true` and
instructions to call `set_workspace`; supplying `workspace_id` on that call does
not bypass startup. Missing identity, unknown keys, invalid workspace values, or
invalid role/model values also return `isError: true` without changing either
accepted context value. Older clients that sent only a workspace must include
identity on every `set_workspace` call, including when clearing with null.

Identity remains accepted when the workspace is null; scoped calls still need a
workspace, while discovery/registration can proceed. `get_workspace` reports both
values as null before the first declaration. Restarting the MCP process requires
a new declaration; repeating `initialize` is not a reset.

Declarations are [self-reported attribution](../auth.md#declared-bot-identity).
The server derives a local persona only for implicit local bootstrap: the same
canonical role/model pair groups across sessions and workspaces. Credential bot
IDs, human identities, permissions, and stream audiences remain unchanged. New
records expose optional `bot_identity`; no historical attribution is inferred.

## Agent sessions

Session tools use the accepted workspace and identity. An explicit `workspace_id`
overrides the selected workspace; the HTTP server still checks permissions and
requires the session and task to belong to that resolved workspace. Reads require
read access; creation, association, runtime changes and termination require edit access.

| Tool | Arguments beyond optional `workspace_id` | Result |
| --- | --- | --- |
| `session_create` | Caller-owned `session_id`, required `task_id`, `format_label`, very short `title`, optional `parent_session_id` and `initial_runtime_identity` | Atomic new session plus initial in-progress task link; retries preserve canonical metadata/links |
| `session_set_runtime` | `session_id`, `expected_runtime_revision`, required nullable `runtime_identity` | Guarded active runtime declaration; explicit null clears to unknown |
| `session_list` | Optional `task_id`, `limit`, `activity`, paired `before_created_at` / `before_id` | Compact workspace or task sessions |
| `session_detail` | `session_id` | Metadata including creator attribution |
| `session_tasks` | `session_id`, optional `limit`, `after_task_id` | Visible associated tasks |
| `session_associate` | `session_id`, `task_id` | New links require `in_progress`; existing links remain idempotent after task completion |
| `session_terminate` | `session_id`, `expected_offset`, `expected_revision`, `final_bytes`, `final_sha256` | Explicit immutable seal receipt |
| `session_read` | `session_id`, optional `offset`, `limit` | Active transcript byte range |
| `session_evidence` | `session_id`, optional `offset`, `limit` | Terminated evidence byte range |
| `task_triage` | `task_id`, `path` | Verified ZIP of all linked saved sessions to a new local destination |
| `session_import` | `session_id`, `task_id`, `path`, `format_label`, `title`, optional parent/runtime | Pending historical upload; final receipt via `session_upload_status` |

Lists default to 50 rows and accept 1..100. Session lists retain the server's
descending immutable `(created_at, id)` keyset; task links continue by UUID.
Execute a returned `next_request` unchanged: it pins the resolved workspace and
task/session identity, so changing the selected workspace does not retarget it.
Final pages, including empty pages, omit `next_request`. Pages reflect current
visibility and additive associations rather than a frozen snapshot.

Metadata and mutation acknowledgements never include transcript bytes. Compact
metadata retains full IDs, format, state, timestamps, committed bytes/records,
revision, newline status and any final byte count/SHA-256. Results keep immutable
`creator_*` connection-caller attribution separate from nullable declared
`runtime_identity` and its `runtime_revision`. Unknown runtime is never inferred
from the caller or format. All new session results include `structuredContent`.

`initial_runtime_identity` initializes NEW sessions only, for example
`{"model":"actual-target-model","role":"developer","harness":"codex"}`;
`harness`, `provider` and `reasoning` are optional known values. Harness names the
actual runtime application; never derive it from provider, format or caller.
New nonnull declarations require the running agent's
actual role, independently of the caller. Model names contain 1–256 UTF-8 bytes;
role and optional values contain 1–128.
Values must be trimmed and nonblank. Omitted/null means unknown. Creation
retries preserve and return canonical runtime, including after later switches.
Legacy roleless declarations remain readable and resumable without a backfill;
append, heartbeat, flush/stop and sealing continue to preserve their evidence.
The accepted MCP identity still describes the caller, not necessarily the target.

`session_set_runtime` requires `expected_runtime_revision` and an explicit
`runtime_identity` value, including JSON null to clear it. It only edits active
sessions. Changed declarations increment the runtime revision once; identical
current-value updates do nothing, and an identical retry at the immediately
preceding revision succeeds without another audit/change event. Other stale
updates return `session_runtime_conflict`; refetch `session_detail` before retry.
This revision is independent of append/termination `revision`: runtime changes
do not alter transcript bytes, byte/hash receipts, heartbeat activity or creator.
These declarations are self-reported metadata, not verified execution identity.

Range reads default to 4096 bytes, accept 1..1048576, and return `session`,
`offset`, canonical padded `data_base64`, `next_offset` and `has_more`. Decode each
range as bytes and concatenate in offset order; a range may split a UTF-8
character or CRLF pair. Execute `next_request` until absent to reconstruct the
entire original JSONL, then compare its byte count and SHA-256 with the sealed
receipt. Active reads describe the committed prefix at that read. Unsupported
windows fail validation; no bytes are silently clipped. Each HTTP response,
including errors, is capped at 2 MiB before JSON decoding. Overflow returns
`response_too_large`; request a smaller range or list window where applicable.
Errors carry a safe code and optional HTTP status, without raw response content,
parser diagnostics or credentials.

Titles describe the work, for example `Repair session filters`; `format_label`
identifies the provider-neutral transcript format. Both create and upload start
require a title. New titles and parents are immutable. Legacy sessions with no
stored title remain readable/resumable without assigning one. A new task link to
a non-running task returns `session_task_not_in_progress` with the actual status;
start the task first. Termination does not change task status.

Both creation tools require `task_id`. For a NEW session it must identify a live
same-workspace `in_progress` task; the server commits creation and its initial
link atomically. Missing/foreign/deleted tasks return `session_task_not_found`;
non-running tasks return `session_task_not_in_progress` with task/status guidance.
Existing session retries and resumes preserve their canonical associations,
including legacy history and tasks that later completed. Supplying another
creation-only `task_id` on resume does not add a link; call `session_associate`.

Termination seals only the already committed server prefix. Supply its expected
offset/revision and final byte count/SHA-256; the server verifies them atomically.
An exact retry uses the original pre-seal revision. Only an explicit
`session_terminate` requests this transition: task completion, upload start,
flush, stop, EOF, caught-up state and process loss never imply termination.
The usual management sequence is to stop/flush an uploader, inspect its receipt,
then explicitly terminate that committed prefix. Direct termination also works
without a local file or live uploader, in any uploader state; later ingestion is
refused while task association remains available. The normal tools above do not
start uploaders or read local files. HTTP ingestion is described in the
[session API reference](../api.md#agent-sessions).

### Local upload followers

`session_upload_start` requires explicit `session_id`, `task_id`, `path`, `format_label` and a
very short descriptive `title` (normally 3–8 words; 1–80 UTF-8 bytes). Optional
`parent_session_id` fixes its existing same-workspace parent at creation;
`workspace_id` may override context. It returns promptly with `status: validating`;
validation and upload continue in the MCP process without further agent turns.
`validating` is registration, not proof that a remote session was created. A
missing or malformed task UUID is rejected immediately. A server task refusal
ends validation with `status: failed`, a safe `error` and `error_details` naming
`task_id`, the required status and any known current status. No session or link
is committed; the source handle and active follower/HTTP capacity are released,
so the same UUID can be retried with a valid running task.
The path must identify a readable regular file. Only this tool accepts a local
path; paths, credentials and raw errors never appear in receipts or HTTP metadata.
An existing active session is resumed only after its entire committed prefix
matches the source. A second active follower for the same UUID is refused.

Optional `initial_runtime_identity` describes the actual target model and agent
role when a new session is created. Resume preserves canonical runtime even if its startup
declaration is older; it never reasserts that declaration after a model switch.
Use `session_set_runtime` for existing-session declarations or changes. Uploader
status reports the last acknowledged remote `runtime_identity` and
`runtime_revision`; those values can lag a concurrent change until the next
metadata/append/heartbeat acknowledgement. Caller identity remains captured for
request attribution and is never substituted for runtime.

`session_upload_status`, `session_upload_flush` and `session_upload_stop` accept
`session_id` and optional `workspace_id`. They address this process's registry;
a different resolved workspace cannot control the job. Context changes retain
ordinary request ordering, while registered jobs keep their captured workspace,
identity, bearer credential and provenance configuration. Context changes do not
retarget or wait for background jobs. Restarting MCP loses the registry; start
again explicitly with the same UUID and source to verify and resume.

The registry admits at most four active jobs. Two fixed workers take fair turns;
one shared gate permits at most two follower HTTP requests globally, including
creation, metadata, verification, append, heartbeat and retry. Normal MCP tools,
including explicit termination, use independent HTTP capacity. Each turn reads
at most 1 MiB and performs at most one HTTP request; prefix verification uses
64 KiB pages with a frozen end, interleaved with following and heartbeats.
Nominal source checks occur every second, idle heartbeats every ten seconds;
`stale` independently becomes true thirty seconds after the last HTTP
acknowledgement. File/HTTP latency and other jobs can delay a turn.

Each HTTP operation has a five-second deadline and a 2 MiB response cap. A
transient failure preserves the exact pending append for retry, with exponential
backoff (1, 2, 4, 8 seconds) and failure after five attempts. Cursor conflicts
reconcile through canonical metadata and complete prefix verification. Revoked
authorization fails closed. Status distinguishes `validating`, `following`,
`caught_up`, `pending_tail`, `retry`, `finalizing`, `stopped`, `failed` and `terminated`.
The registry retains at most sixteen terminal entries for five minutes.

Normal following uploads complete LF-terminated records, preserving UTF-8 and
CRLF bytes. Partial tails stay pending; malformed terminated records, oversized
records, replacement, truncation and observed committed-prefix changes fail the
job without skipping bytes. The source handle supplies both identity and reads;
each check compares a separately opened current path. Windows uses volume plus
128-bit file ID; POSIX uses device/inode identity. Unsupported identity queries
fail closed. Ongoing bounded sweeps detect persistent edits even with unchanged
size and modification time.

Flush and stop serialize after current work and snapshot one finite source-size
boundary. They may append a complete final JSON record without LF; incomplete or
invalid final tails stay pending. A later record must begin after a line
separator. These calls wait at most one second, then return an explicit pending
`requested_operation` receipt pinned to the accepted operation UUID; inspect status's
`operation` until its receipt or error is available. Admission and terminal receipt
publication are atomic. During `finalizing`, stop is closing the source and further
controls are refused; every accepted operation receives a receipt or error. A successful
receipt includes `committed_offset`, `revision`, `capture_boundary`, canonical
`sha256`, `pending_tail` and `source_verified`. It requires complete canonical
prefix hashing, byte comparison and final identity/size/cursor checks. Later
growth does not extend that capture. Flush resumes following; stop closes the
source. Arbitrary external writes that change and restore bytes between
observations cannot be disproved without writer cooperation or a filesystem
snapshot. The receipt certifies the canonical server prefix and the source
observations, rather than an atomic snapshot of concurrent external writes.

For final evidence, stop the uploader, inspect the completed receipt, then call
`session_terminate` with `expected_offset: committed_offset`,
`expected_revision: revision`, `final_bytes: committed_offset` and
`final_sha256: sha256`. Confirm `pending_tail` before deciding which committed
prefix to seal. Direct termination remains available in any uploader state. A
follower discovering a seal retires without later mutation. No upload control,
EOF, caught-up status, failure or shutdown invokes termination.

At process shutdown, new registrations stop, workers are cancelled and joined
within seven seconds (the five-second HTTP deadline plus two seconds for
retirement and joining), followed by a separate two-second handle-cleanup budget.
Each follower request owns a short-lived helper process using the same compiled
MCP binary, default verified TLS, system resolver and proxy behavior. At most two
helpers run under the existing shared request cap. Private pipes carry requests;
credentials and source paths never enter helper arguments or logs. A five-second
monotonic budget includes startup, DNS, connect, TLS, body and cleanup, reserving
one second for cleanup. The parent kills and reaps the child before joining pipe
workers or releasing its slot. Frames are capped at 4 MiB; raw HTTP bodies retain
the 2 MiB cap without JSON re-encoding expansion. The ordinary MCP manager stays
independent. Parent loss kills owned helpers on Windows and Linux.
The server returns successfully only after confirmed worker completion and handle
cleanup; incomplete cleanup raises an explicit failure. A stuck native OS operation may
outlive that budget; its handle is not closed concurrently with an active read.
Unconfirmed helper ownership closes follower admission and retains its lease for
cleanup, so retries cannot reuse capacity occupied by an unconfirmed owner.
Ordinary local file/network cancellation is covered by the handle tests. Server
sessions remain active unless the managing agent explicitly sealed them.

## Unified search

`search` retrieves stored workspace records with all-term lexical matching. Omit
`query` to browse; blank queries are invalid. Boolean `OR` and phrase operators
are not interpreted. Projects and tasks use English stemming by default;
Observations use simple tokenization. Use `rg` for repository source text,
`observation_match` for concrete paths, and detail/get tools for complete content.

Omitted `entity_types` selects all three types; an explicit array must be nonempty.
Observation provenance filters affect only Observations, `project_status` only
Projects, and `task_status`/`project_id` only Tasks. `project_id` uses exact direct
project membership and excludes descendant projects. Mixed filters are valid;
`ignored_filters` lists supplied filters that no selected type consumes.

Every success includes the resolved `workspace_id`, compact arrays, `has_more`,
`next_offset`, and `next_requests`. Execute each `next_requests` entry as a
`tools/call` params object. It pins the original workspace and retains only that
type's applicable filters, including `search_language` for Projects/Tasks and
`task_priority` for Tasks when supplied. No entry means no further page for that type.
Offset pages use the current result set; concurrent changes can alter membership
or order. A required offset beyond 2147483647 returns `continuation_limit`.

Search and Observation discovery advertise `outputSchema` and return
`structuredContent` as optional hmem
backport extensions. Initialization retains protocol version `2024-11-05`, including
fallback from newer requested versions, and advertises experimental capability
`hmem/structured-tool-output` version 1. This does not claim newer protocol
conformance; older clients may ignore the extensions. Successful JSON text and
structured content encode the same payload. Errors retain readable text and
`isError: true`, with `error.code`, nullable `error.field`, and `error.message` in
structured content. HTTP errors also include status and bounded server details;
`details_truncated` reports truncation. Other tools retain their existing shapes.

## Raw-text Observation discovery

`observation_discovery` sends one request to the authorized server's existing
discovery endpoint. Create a context with raw text and provenance filters:

```json
{"filters":{"subject_query":"src/","content_query":"authorization"},"view":"list","limit":20}
```

An empty `filters` object browses. The server chooses hybrid retrieval when its
optional semantic provider is usable, or content FTS with a safe fallback reason.
MCP does not generate vectors, enable providers, or cache search contexts. This
does not change unified `search` semantics.

Results retain `context_token`, absolute `expires_at`, `retrieval.mode`/`reason`,
`complete`, and `page` with compact Observation `items`, `counts`, `has_more` and
optional `tree`. Counts describe distinct Observations; tree nodes and typed
terminals retain their own counts. Use `observation_get` for full content.

Continue with the same workspace/context and presentation selectors only:
`view` (`list`, `tree`, `count`), tree `prefix`, list-leaf
`terminal: {subject_kind, subject}`, `limit` and `offset`. Defaults are list,
empty prefix, 50 and 0. Execute `next_request` when supplied: it pins the workspace
and token, omits creation fields, and advances by actual list rows or tree
children, never Observation totals. Count and final/empty pages have no next
request. If a successful page has more rows but its successor exceeds
2147483647, the data remains available with typed
`continuation_error.code: "continuation_limit"` and no unusable request. A server
page claiming more rows while returning none similarly reports
`invalid_continuation`; tree failures identify `page.tree.items`.

Contexts expire after five minutes without sliding expiry and remain bound to
their server, workspace and resolved audience. Each use reauthorizes access.
To change filters, explicitly create a context with optional
`replace_context_token`. Optional caller-owned `search_session` and increasing
`generation` (1..2147483647) must be paired on creation; continuations omit both.
MCP synthesizes no session state and never replaces an expired context
automatically. A 409 `discovery_restart_required` requires a fresh query; a 429
`discovery_capacity` preserves the current query for retry. Both retain structured
HTTP status/details. Nested SHA validation errors identify the actual
`filters.git_sha`, `filters.current_git_sha`, or `filters.history_git_sha`, with
null attribution when several are invalid.

## Task triage and historical import

`task_triage` downloads the authorized task evidence ZIP, including all pages of
linked terminated sessions. Active sessions are excluded. The explicit local
destination's parent directory must exist. Existing destinations are refused,
including a path created concurrently during download. A same-directory temporary
file is removed on failure; only a complete ZIP64 with validated central index,
entry sizes and CRCs is atomically published. Empty evidence is a valid archive.
The compact receipt contains `archive_bytes`, locally calculated `sha256`,
`session_count` and `verified`. The archive digest identifies the local download;
the server does not advertise a transfer digest. The operation has a 15-minute
total deadline and 30-second HTTP response timeout.

`session_import` accepts an explicitly supplied completed regular JSONL file and
returns promptly. It shares the live uploader's four-job registry, two HTTP slots,
captured identity/workspace, request limits and cleanup. Before remote
registration, bounded fair passes validate every UTF-8 JSON record and hash the
entire fixed file boundary. CRLF and a complete final record without LF are
preserved. Invalid/incomplete records are rejected rather than omitted. File
replacement, truncation, growth or observed byte mutation fail. The import has a
15-minute deadline; process loss leaves a resumable pending import.

The server binds an immutable historical task/size/hash manifest. Any visible
same-workspace task status is accepted without changing it. Importer attribution
is the actual connecting caller. Optional `initial_runtime_identity` describes
the historical producer only when explicitly known; no identity is inferred from
JSONL. Historical runtime is frozen. Ordinary live create/associate rules remain.

Poll `session_upload_status` until `status: "imported"` and a receipt with
`source_verified: true`, `state: "terminated"`, full `committed_offset` and
`sha256`. A failed job reports a fixed error code. Live flush/stop controls return
`import_control_unsupported` for imports. Retry `session_import` with the same
UUID, file and metadata to resume after failure/process loss or reconcile a lost
acknowledgement; an exact active retry returns its current status. A sealed retry
rehashes the local file and checks the final receipt. Conflicting UUID metadata
or source manifest fails. Authorized managers can resume without impersonating
or replacing the immutable original creator.

Finite receipts describe observed source bytes and identity. They cannot detect
an external mutate-and-restore write entirely between observations.
