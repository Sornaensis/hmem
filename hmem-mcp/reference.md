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
read access; creation, association and termination require edit access.

| Tool | Arguments beyond optional `workspace_id` | Result |
| --- | --- | --- |
| `session_create` | Caller-owned `session_id`, `format_label` | Idempotent metadata acknowledgement |
| `session_list` | Optional `task_id`, `limit`, paired `before_created_at` / `before_id` | Compact workspace or task sessions |
| `session_detail` | `session_id` | Metadata including creator attribution |
| `session_tasks` | `session_id`, optional `limit`, `after_task_id` | Visible associated tasks |
| `session_associate` | `session_id`, `task_id` | Idempotent link acknowledgement, also after sealing |
| `session_terminate` | `session_id`, `expected_offset`, `expected_revision`, `final_bytes`, `final_sha256` | Explicit immutable seal receipt |
| `session_read` | `session_id`, optional `offset`, `limit` | Active transcript byte range |
| `session_evidence` | `session_id`, optional `offset`, `limit` | Terminated evidence byte range |

Lists default to 50 rows and accept 1..100. Session lists retain the server's
descending immutable `(created_at, id)` keyset; task links continue by UUID.
Execute a returned `next_request` unchanged: it pins the resolved workspace and
task/session identity, so changing the selected workspace does not retarget it.
Final pages, including empty pages, omit `next_request`. Pages reflect current
visibility and additive associations rather than a frozen snapshot.

Metadata and mutation acknowledgements never include transcript bytes. Compact
metadata retains full IDs, format, state, timestamps, committed bytes/records,
revision, newline status and any final byte count/SHA-256. `session_detail` also
returns creator attribution. All new session results include `structuredContent`.

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
