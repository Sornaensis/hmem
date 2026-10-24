# Schema reference

## Agent session change delivery (V042)

`V042__agent_session_change_delivery.sql` installs dedicated audit/outbox triggers
and its ledger entry in one transaction. A ledger failure rolls back installation;
registered reruns do not repeat effects. It neither backfills historical events
nor retires existing full/shell snapshots or resume bearers.

Actual session creates, append counter updates, active heartbeats and termination
write one `agent_session` audit and workspace event. Exact create/append/seal
retries and unchanged raw updates emit nothing. The explicit audit projection
contains identity, format, state, timestamps, committed byte/record counts,
revision, newline state and final receipt. No complete row or chunk is serialized.
Audit request/actor/declaration fields and outbox cause/actor retain the existing
transaction context semantics.

Each additive link emits one `task_session` audit/event, identified by
`session_id:task_id`; its audit projection contains only workspace/session/task
IDs and creation time. A duplicate link emits nothing, including after sealing.
Session events invalidate one metadata resource and the workspace session
collection. Link events additionally identify the link and its one task panel.
These fixed targets do not grow with transcript length or the number of links.
No event contains transcript bytes, and `session_chunks` has no audit/outbox hook.

The `full_v1` and `workspace_shell_v1` item allowlists stay unchanged. Session
metadata, links and byte windows remain separate bounded authorized HTTP reads.
Clients retire session response generations on accepted snapshots/resync and
reload demanded resources, including when a valid snapshot has no session items.

## Agent session evidence (V041)

`V041__agent_sessions.sql` adds `sessions`, ordered `session_chunks`, and the
many-to-many `task_sessions` relation. The migration owns its transaction and
version-41 ledger entry. Existing tasks and evidence are unchanged; a failed
migration rolls back its DDL and ledger together and can be retried. There is no
destructive down migration for captured evidence; use the existing database
backup and upgrade recovery procedure.

A caller-supplied session UUID fixes its workspace, format label (1–128 UTF-8
bytes), creator attribution and creation time. An exact create retry returns the
existing row, including after termination; different immutable metadata conflicts.
Creator actor/declaration fields come from the effective request context and
grant no authority. Sessions are distinct from Observations and task status.
Task links are unique, additive and restricted by composite foreign keys to the
same workspace. Soft deletion hides linked tasks from both list directions;
restoration exposes their retained links. Links can be added after termination.

Append takes expected byte offset/revision, original bytes, and an explicit
`allow_final_record` flag. Each chunk contains 1–1,048,576 bytes and complete
UTF-8 JSON records; the record limit is also 1,048,576 bytes. JSON values may use
any provider schema. Validation does not rewrite bytes, JSON, CRLF or whitespace.
Empty records and incomplete/invalid tails are rejected atomically. Ordinary
append requires a trailing LF. An explicit finite-snapshot flush can commit a
complete final JSON record without LF while the session stays active. Its next
append must start with LF or CRLF, even when the final JSON accepts trailing
whitespace. A separator-only append is allowed at that boundary and adds no
record. Each new chunk advances revision once; an exact replay matches its
original offset, revision, bytes and flush flag and returns the current canonical
session without mutation. Gaps, conflicting overlap and stale cursors fail.

`HMem.DB.AgentSession.terminateSession` is the canonical seal primitive. Under
the session row lock it verifies expected offset/revision, final byte count and
lowercase SHA-256 of the already committed prefix. It reads one indexed chunk at
a time into an incremental crypton hash, with memory bounded by the chunk limit;
there is no total transcript size cap or growing transcript rewrite. Termination
advances revision once, fixes the final receipt and freezes every session column
and chunk. An identical termination retry succeeds; a different receipt conflicts.
It never reads a local path, uploads bytes, inspects an uploader, or changes a
task. Heartbeats, EOF and absent/stopped/lost upload processes cannot terminate a
session. Heartbeats update only the active session's `last_seen_at`, independently
of the transcript revision. Freshness is derived by consumers from that timestamp.

Database triggers also reject raw chunk edits/deletes, malformed or out-of-order
inserts, counter drift, identity edits and changes to already terminated metadata.
Statement guards reject `TRUNCATE` of each evidence table, including cascades
from task/workspace tables. Only the privileged test harness suspends those exact
three guards during its authorized fixture reset, restoring them before commit
on the same connection and rolling back on failure or cancellation.
Canonical core termination owns digest verification; these guards do not claim
to prevent a privileged database administrator from forging an initial seal or
disabling triggers. Transcript bytes have no audit/outbox trigger and never enter
workspace snapshots. The HTTP authorization and live invalidation adapters are
separate boundaries over these primitives.

Lists reject limits outside 1–100. Session lists use immutable descending
`(created_at,id)` keyset cursors; task references use ascending task UUID cursors.
Newer sessions do not shift a previously issued page cursor. Links added behind
a cursor appear after a fresh list; lists are not historical snapshots. Ranges
reject negative offsets, starts beyond the committed prefix, and windows outside
1–1,048,576 bytes. A range holds a shared session lock while reading its bounded
window and returns the canonical cursor, next offset and `has_more`. Local core
inputs/results use `ByteString`; transport adapters own binary encoding.

Core errors distinguish not found, conflict, terminated and invalid input through
`SessionException`. Database JSONL/direct-write guards report a fixed
`DBCheckViolation` without embedding raw record/parser text. No mutation retries
automatically; callers may retry an exact request after an uncertain response and
must inspect the returned canonical cursor before submitting another append.

## Declared attribution (V039)

`V039__declared_bot_identity.sql` adds nullable `bot_identity` JSONB to
`audit_log` and `observation_revision_events`. The validated object contains only
`role` and `model-id`. Transaction-local settings carry the effective actor and
optional declaration to generic, Observation, and project-pause audit writers,
revision events, Timeline reads, and the durable outbox. Pooled transactions
explicitly clear absent actor/declaration settings so requests cannot inherit
an earlier caller's attribution. Authorization continues to use the original
Principal and its grants.

Observation events and `current_provenance` use the same canonical projection:
omit only the new `bot_identity` key when absent, retain existing mandatory
nullable actor fields, and require exact event/projection equality. Append-only
history, contiguous sequences, and the current head remain enforced. Outbox
actors keep `user`, `service`, and `system`; a bot maps to `service`, and genuinely
absent request context maps to `system`.

The migration owns its transaction and version-39 ledger entry. Existing history,
audit records, current projections, and stored outbox envelopes are not rewritten
or assigned inferred declarations. This additive metadata does not change
snapshot envelope versions or require a new resync of otherwise valid tokens.
Older payloads remain readable. New same-declaration local personas group across
workspaces/sessions; credential-backed bots keep their credential actor IDs
instead of substituting authorization grant-user IDs. See
[the trust contract](../../auth.md#declared-bot-identity).

## Due-field removal (V038)

`V038__remove_task_due.sql` drops `idx_tasks_due` and `tasks.due_at`, including
values on soft-deleted tasks. Subtasks use the same table; projects have no due
column. Fresh installs run the ordered migration chain and reach this schema.
The migration owns one transaction containing snapshot retirement, schema
changes, and its version-38 ledger entry. A failure rolls these changes back;
after correcting the failure, retry through the migration runner. An already
registered V038 is skipped.

Only saved workspace `full_v1` snapshot sessions containing a task item with a
`due_at` key (even null) are retired, together with their linked resume tokens,
including successor tokens, and cascading cached items/page tokens. Clients
must obtain a fresh snapshot when those tokens are refused. Compatible global,
workspace-shell, and empty full snapshots remain valid. Fresh task snapshot
envelopes retain schema version 1 and omit due fields.

Historical audit values and persisted outbox envelopes are retained unchanged;
they can still contain obsolete due data. Task IDs, unrelated values and
timestamps are preserved. There is no down migration or recovery of removed
deadline values from current task rows. Follow the
[coordinated upgrade and backup rollback procedure](../../docs/reference/upgrades.md#upgrading-across-v038).

Older versioned migrations retain their due-column references to describe and
upgrade the historical schema. Audit/outbox fixtures may retain obsolete due
values as compatibility evidence. Operational retry times, job eligibility,
leases, token expiry and request timeouts are separate from removed planning
deadlines and remain supported.

## Session tree and titles (V043)

V043 adds nullable `title` and `parent_session_id` columns to `sessions`.
Null titles identify pre-upgrade rows; the migration does not rewrite them,
their timestamps, creator, counters, transcript or final receipt. New inserts
require a trimmed, nonblank title of 1–80 UTF-8 bytes. Parent selection is optional
and immutable, references an already existing same-workspace session through a
composite FK, and rejects self/cyclic insertion. Task links remain additive:
existing duplicates are no-ops, while new links lock a live same-workspace task
and require `in_progress`. Status changes do not remove historical links.

The migration installs stable active-task/project and bounded ancestor-context
projections. Session audit values keep an explicit metadata allowlist including
title/parent, with no evidence bytes. Only creation, lifecycle termination and
new task links add the `active_sessions` collection invalidation. Append and
heartbeat keep their ordinary metadata/list invalidations. Schema changes,
functions, guards and ledger registration commit or roll back together.
