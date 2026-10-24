# Session explorer

The frontend presents stored JSONL without changing its original bytes. Native
Codex response-item messages, function calls/results and reasoning summaries, and
Claude message content blocks render as conversations. Adjacent opposite Codex message envelopes are paired one-to-one only when their
content matches and valid timestamps differ by at most 100 ms. The event envelope
is suppressed; missing or invalid timestamps preserve both records. Thinking and tool payloads start collapsed. Encrypted reasoning is a placeholder, and unknown records/content blocks are hidden. Tool inputs and outputs are formatted JSON with bounded syntax-token spans; non-JSON strings use an input/output wrapper. Markdown links permit HTTP, HTTPS, mailto
and local fragments; raw HTML is not executed and images display their alt text.

## Bounded presentation and transport

- Session list and task-section pages contain at most 50 summaries. Each task has
  independent active/evidence cursors and a finite 16-entry previous-page history.
- The hierarchy’s mounted rows and pins bound task demand. Active hints retain
  only current mounted-task status. Expanded task cards retain one current page
  for each section, with a maximum of 64 metadata pages. Offscreen or collapsed
  cards discard those pages; transcript bytes never enter this cache.
- Settled metadata pages rotate out of the session delivery queue, allowing all
  mounted cards to converge under its unchanged 16-resource and two-request caps.
  Pending physical requests retain their admissions until their exact callbacks.
  Invalidation makes cached results stale and queues a canonical refresh; errors
  stay paused until retry or invalidation. Workspace/authority epochs clear caches.
- The viewer retains at most 100 records and 2 MiB of original bytes. A record may
  occupy at most 1 MiB. A mid-record window omits its leading fragment; overlapping
  internal older windows recover complete records. The current UI follows the tail without window controls. Presentation text is not truncated to
  a separate short preview limit.

Session titles are distinct from transcript format labels. The list is a bounded
preorder of matching active sessions, with at most three levels of indentation.
Missing or excluded parents appear as compact ancestor context, not inactive
result cards. Each metadata result carries at most sixteen ancestors; deeper
context is explicitly marked as omitted. Native title/parent fields are fixed at
creation; legacy null titles display the format label.

Project/task facets use their own authorized 50-option UUID pages, independent of
session pages and freshness. A validated selected option is returned separately
so a selection beyond the current option page remains visible. Errors preserve
selection; only successful authority can retire a deleted/moved/inactive choice.

Direct links use `#tab=sessions&session=<UUID>`. Adding `task=<UUID>` keeps the
workspace list scoped to that task through selection, close and browser history.
The active-state and Live/Quiet activity predicates apply before pagination; Live
is the default. Saved sessions remain reachable through task Evidence links.
Transcript model identity uses declared creator attribution. Event times and list
activity use the bounded local timestamp cache; the browser-local day decides
whether to omit the date. Linked-task cards show two items initially, then one
bounded page on expansion.

## Downloads

Downloads read the canonical terminated session in chunks of at most 64 KiB,
checking its immutable receipt on every chunk and its final byte count and
SHA-256 before completion. The displayed record window is never the download
source. Workspace, authority or selected-session changes retire the writer.

Where available, browser-private temporary file storage stages the bytes. The
source and object URL remain available for 60 seconds after the verified browser
download starts, then are disposed; page exit also requests disposal. Cleanup removes only the current operation’s owned files; another tab’s live
staging is never removed by age. Files orphaned by a crashed page may remain
until the browser clears the site’s storage. If temporary storage is
unavailable, a finite in-memory fallback supports sessions up to 32 MiB; larger
sessions report the storage limitation. MCP `session_evidence` remains the bounded
retrieval interface independently of the browser explorer.


Task-card sections use compact responsive grids. Active activity is `lastSeenAt`;
Evidence uses `terminatedAt`, falling back to `lastSeenAt`. Declared creator role
and model identity remain complete and selectable.

Evidence **Download all** requests one authenticated task-scoped binary response.
The server walks all linked terminated sessions in pages of 50 independently of
visible section pagination. It retains one page and one entry receipt, checks
strict descending timestamp/UUID progress, and verifies each original byte count
and SHA-256. This live keyset walk is not an atomic snapshot of concurrent links.

The server emits ZIP64 with actual raw DEFLATE compression (method 8), incremental
CRC32, sanitized bounded labels and full session UUIDs. One 64 KiB source range,
one compressor and a temporary central-index sidecar bound memory; the sidecar
is also copied in 64 KiB chunks and removed on success, failure or cancellation.
Two server exports may run concurrently, with a 15-minute deadline. Excess
requests return 503 with Retry-After. Responses are no-store binary attachments.
A failed receipt or stream produces no successful archive footer and fails the
HTTP transfer; exception details and transcript contents are not exposed.

The browser streams the opaque response into private temporary storage through
the existing 16-resource, two-request admission and exclusive download reservation.
Current bearer/cookie authentication is applied privately to the fetch. A physical
admission remains held until its exact fetch/abort callback. The client never
assembles ZIP entries or enumerates sessions for export. Without private storage,
a 32 MiB fallback retains immutable compressed Blob parts. Only a complete,
successful response is offered as a download. Cancel or authority/context retirement
aborts the fetch and joins staging cleanup before reuse. Cleanup failure blocks
reuse until reload. Verified archives retain their source for the same 60-second
browser download lifetime as individual evidence; only operation-owned files are
removed, and crash-orphan files require browser site-storage cleanup.

## Activity lights

Green lights mean at least one session is explicitly active, regardless of its
heartbeat age or task/project status. A project includes every live descendant
project and task, following task parent links even when a child has no project ID.
A workspace includes active sessions with no task links. Deleted targets and
terminated sessions do not contribute. The shared ring is decorative; reduced
motion leaves it static and an accessible label identifies activity.

`GET /api/v1/session-activity` returns boolean rows for at most 50 combined unique
`workspace_id` and `project_id` targets. Project IDs require one explicit
`project_workspace_id`; missing, deleted or inaccessible targets are not reported
as inactive. Server authorization precedes traversal and no transcript bytes or
hierarchy rows are returned. Unknown/error results remain distinct in the client.

The client probes rendered workspace/project cards and expanded sidebar workspace
entries, deduplicates grouped
workspaces, and fairly rotates batches under one physical request. Retired
requests retain their occupancy until the exact callback; authority, visibility
and target generations reject stale replies. Idle probes refresh about every ten
seconds; selected-workspace session/link and hierarchy events or reconnects
request a coalesced refresh, at most once a second for already loaded targets.
Unselected workspace cards converge through polling without broadening global
change events or snapshots. Hidden pages and unmounted cards do not poll.

The Projects **Has Active Session** filter is evaluated in server navigation
queries, including unloaded descendants and ancestor retention. It combines with
status, priority, query, show-only and empty-project predicates. Canonical
`active_sessions` invalidations refresh its membership and the dropdown facets
on create, termination and new links. Ordinary append/heartbeat updates do not
restart hierarchy membership passes. Project/task placement and deletion also
invalidate scoped lists and facets. Existing request/authority guards and
transport/mounting bounds remain unchanged.
