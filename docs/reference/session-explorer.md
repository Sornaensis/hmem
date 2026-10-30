# Session explorer

The frontend presents stored JSONL without changing its original bytes. Native
Codex response-item messages, function calls/results and reasoning summaries, and
Claude message content blocks render as conversations. Adjacent opposite Codex message envelopes are paired one-to-one only when their
content matches and valid timestamps differ by at most 100 ms. The event envelope
is suppressed; missing or invalid timestamps preserve both records. A paired
occurrence keeps its first source-position presentation anchor even when the
canonical envelope arrives later or its hidden counterpart leaves the window.
The retained rows carry only bounded pairing metadata; raw offsets, delimiters
and evidence bytes still describe the canonical record.

Consecutive thinking, tool calls and results share a collapsed activity group,
including a run that currently has only one event. User and agent prose stays
outside the disclosure and splits runs, including within mixed Claude content.
Hidden unknown and empty blocks neither create groups nor split them. Each group
shows its first timestamp and event count. Individual thinking and formatted tool
payload disclosures remain available inside it; encrypted reasoning shows only
*Thinking...*. Source-position keys preserve native disclosure state as a run
grows or a message closes it. State belongs to the current session/authority and
retained window; it is not an unbounded history of expanded groups.

Tool inputs and outputs are formatted JSON with bounded syntax-token spans;
non-JSON strings use an input/output wrapper. Markdown links permit HTTP, HTTPS, mailto
and local fragments; raw HTML is not executed and images display their alt text.

## Native transcript coverage

Copilot support targets persisted `events.jsonl` envelopes (`type`, `data`,
`timestamp`, `id`, `parentId`), validated against the official
[v0.1.32 generated event schema](https://github.com/github/copilot-sdk/blob/396e8b3c04175dcf2fd1c7c34950c3fc0a5395e8/python/copilot/generated/session_events.py).
The synthetic non-secret fixtures follow that schema; this is not a claim that
every future CLI/SDK version has been validated. User/assistant content,
message reasoning, standalone reasoning, tool requests, execution starts and
success/error results render in source order. Requests and execution starts are
distinct labelled occurrences with the same tool-call ID; a request is not proof
of execution. Request-only and execution-only windows remain readable. Separate
reasoning events and message reasoning are retained because the schema does not
guarantee their equivalence. Tool result disclosures retain both model-facing
and detailed content. Ephemeral deltas/progress and routine session/turn metadata
stay hidden. Native model fields do not replace authoritative runtime metadata.

Unsupported meaningful records and content blocks show a short indication of
their type without dumping their JSON. Original evidence remains available
through the verified download path. Windows containing only unsupported meaningful
content settle with an indication. Discriminator/flag-only marker metadata stays
hidden and does not consume the visible history reserve. Known hook, command and
permission failures have an explicit failure indication even without error details;
successful controls remain hidden. Unknown-content inspection visits at most 256
values and 16 levels; larger ambiguous structures receive an indication.
Malformed JSON reports a settled framing error. Incomplete finite tails retain
their bytes and show a boundary notice until completed; neither state read-loops.

pi coverage targets persisted session schema **3**, pinned to official
[pi v1.4.0 session entries](https://github.com/earendil-works/pi/blob/4a080252f06caf63a525d7873c5d94d3c23887e0/packages/coding-agent/src/core/session-manager.ts),
[message types](https://github.com/earendil-works/pi/blob/4a080252f06caf63a525d7873c5d94d3c23887e0/packages/coding-agent/src/core/messages.ts)
and [content types](https://github.com/earendil-works/pi/blob/4a080252f06caf63a525d7873c5d94d3c23887e0/packages/ai/src/types.ts).
Nested user/assistant/system messages, thinking, tool calls/results, visible
custom messages, bash execution, compaction and branch summaries are presented.
Redacted thinking and opaque signatures are hidden. Images and Copilot
attachments receive a bounded unsupported-content notice. Custom extension
state and `display: false` messages remain hidden; bash `excludeFromContext`
affects model context rather than display visibility.

Each displayed pi entry identifies its native root or entry/parent relationship.
This is source-order history that may include alternate branches and later roots;
it does not select an active branch or flatten ancestry into an implied linear
conversation. Ancestry is not expanded and a parent may be outside the bounded
window/page. Hidden metadata is not mistaken for a history boundary. Native
entry ancestry is separate from hmem session parents. Context edits receive an
indication while original displayed history remains intact. Headerless windows
state that their schema version is unverified; known unsupported versions show
notices rather than being interpreted as v3. Both framing paths retain only one
header-version scalar, and archive pieces retain it for identical rehydration.

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
  stay paused until retry or invalidation outside the selected-read recovery below.
  Workspace/authority epochs clear caches.
  Same-authority canonical refresh retains the last admitted metadata pages in
  that bounded cache, so lists, linked tasks and model labels do not flash to
  loading placeholders or move the transcript. Ordinary output refresh can admit a
  monotonic canonical metadata prefix while newer demand remains outstanding, so
  a busy producer cannot starve initial lists, links or byte reads. Membership
  changes, canonical resync and authority transitions retire this read proof;
  pre-retirement callbacks cannot restore it. Mutations keep their independent
  fresh-authority checks.
  Current errors clear the stale payload; successful empty or changed results
  replace it. The dock exposes refresh through `aria-busy` without inserting a
  temporary row above the transcript. Transport byte payloads and their bounded
  range history are discarded on canonical refresh, not reused as metadata.
- The selected session’s metadata and current transcript range retry once, 500 ms
  after a completed timeout, network error or HTTP 500/502/503/504. The retry keeps
  its exact offset, end and active/evidence target and re-enters the shared
  scheduler without occupying a slot during backoff. The HTTP timeout remains
  20 seconds. Selection, range, workspace, authority and read-proof changes retire
  delayed tickets; stale callbacks release only their own physical admission.
  Ordinary progress invalidation preserves waiting and exhausted attempts. A second
  failure leaves the existing manual Retry action; success or explicit Retry
  restores the budget. Lists, background-only reads, downloads, archives and
  permanent/authentication/decoding/invalid-response failures do not automatically retry.
- Ordinary session updates reuse a demanded session metadata read and patch its
  existing proven page entries. Without that demand, only pages displaying the
  session refresh. Missing fresh-filter members and stale-filter membership still
  require canonical reads; freshness timers retain their boundary checks. Pages
  keep their immutable created-at/ID ordering. Creation, lifecycle, task-link,
  hierarchy and authorization changes retain conservative membership invalidation.
  Metadata and transcript reads receive at most three consecutive admissions
  before the oldest waiting background resource, within the same two slots.
- Mounted project/task descriptions retain a separate read-only refresh snapshot
  for up to 32 demanded or pinned cards and 2 MiB of estimated UTF-16 key/content
  storage plus fixed metadata. Oversized descriptions are omitted intact.
  Confirmed empty descriptions remain empty; fresh details, errors and deletion
  replace or retire the snapshot. Active editing drafts stay independent. Known
  eligibility only suppresses a temporary body warning while canonical reads are
  pending; disabled actions still require fresh readiness. Principal or workspace
  permission changes retire display and logical detail proofs, including changes
  that preserve the session epoch. Physical request occupancy remains unchanged.
  Hierarchy restoration retains signed displacement above the hierarchy origin;
  row lookups and native scrolling retain their independent nonnegative bounds.
- The live presentation retains the latest 100 prose-bearing occurrences and
  latest 100 activity-only occurrences independently, each bounded to 2 MiB of
  original record bytes (combined 200/4 MiB), plus a 1 MiB incomplete-record carry.
  Hidden metadata and paired event envelopes consume neither visible reserve.
  A record occupies at most 1 MiB. Transport uses finite 2 MiB windows in 64 KiB
  chunks under the shared two-request admission. A finite window publishes and
  positions once; same-authority synchronization preserves retained disclosures
  and reading position. Only readers at the tail follow appends. Original evidence
  bytes and bounded downloads are independent of this presentation retention.

Session titles are distinct from transcript format labels. The list is a bounded
preorder of matching active sessions, with at most three levels of indentation.
Missing or excluded parents appear as compact ancestor context, not inactive
result cards. Each metadata result carries at most sixteen ancestors; deeper
context is explicitly marked as omitted. Native title/parent fields are fixed at
creation; legacy null titles display the format label.

Project/task facets use their own authorized 50-option UUID pages, independent of
session pages and freshness. A validated selected option is returned separately
so a selection beyond the current option page remains visible. Errors preserve
selection; successful authority retires only deleted, foreign-workspace or
out-of-project selections, while valid zero-match activity scopes remain selected.

The workspace completed reader omits archive search and raw-byte jumps. Its
ordinary bounded paging controls sit in the footer, with availability derived
from the accepted local page request and result. Runtime identity and compact
verification feedback precede the transcript without a growing utility area.
Embedded readers retain their existing search, offset and paging controls.
Changing the existing reader owner resets local query/offset options; ordinary
paging replaces them, while body/disclosure hydration preserves its origin page.

Direct links use `#tab=sessions&session=<UUID>`. Adding `task=<UUID>` keeps the
workspace list scoped to that task through selection, close and browser history.
The selected Live/Quiet/Completed activity union applies before pagination; Live
is the default. Completed includes canonical terminated sessions independently of
heartbeat age or linked task status. Saved sessions also remain reachable through
task Evidence links.
The server reconciles abandoned sessions at startup and periodically. An active
session qualifies after strictly more than one hour since `last_seen_at` when
all explicit task links have status `done` or `cancelled`; no links qualifies.
Unfinished soft-deleted tasks still block cleanup. Creation age and ancestry do
not affect this policy. Full committed-prefix hashing uses one pooled chunk at
a time, outside the exclusive session lock. A final transaction locks the
session and its linked tasks, and rechecks activity and transcript cursors before
sealing. Heartbeat, append or task changes during hashing prevent a stale seal.
The ordinary canonical termination event refreshes active-session indicators;
original bytes, runtime identity, task status and evidence links are preserved.
Runtime labels visibly render `[role] harness:model:reasoning` from explicit
target-session runtime metadata. Every missing component displays `unknown`.
Provider appears only in the tooltip and is never substituted for harness.
Immutable creator attribution identifies the connecting caller separately from
the runtime and remains hidden in these reader labels. Runtime changes refresh
these labels without changing heartbeat freshness. Event times and list activity use the bounded local timestamp cache; the browser-local day decides
whether to omit the date. The Linked Tasks disclosure above Session Hierarchy
shows one bounded page of dependency-style read-only task cards when expanded,
with Jump navigation and continuation controls.

## Downloads

Downloads read the canonical terminated session in chunks of at most 64 KiB,
checking its immutable receipt on every chunk and its final byte count and
SHA-256 before completion. The displayed record window is never the download
source. Workspace, authority or selected-session changes retire the writer.
Canonical resynchronization also retires downloads. Session membership and
hierarchy changes retire read proofs independently, preserving a pending native
writer and its physical HTTP admission through completion.

Where available, browser-private temporary file storage stages the bytes. The
source and object URL remain available for 60 seconds after the verified browser
download starts, then are disposed; page exit also requests disposal. Cleanup removes only the current operation’s owned files; another tab’s live
staging is never removed by age. Files orphaned by a crashed page may remain
until the browser clears the site’s storage. If temporary storage is
unavailable, a finite in-memory fallback supports sessions up to 32 MiB; larger
sessions report the storage limitation. MCP `session_evidence` remains the bounded
retrieval interface independently of the browser explorer.


Task-card sections use compact responsive grids. Active activity is `lastSeenAt`;
Evidence uses `terminatedAt`, falling back to `lastSeenAt`. Runtime role badges and
the same compact card renderer are shared with the Sessions tree and task dock.
An open dock grows the task card to about 1.4 times its normal maximum width,
clamped to the viewport, and stacks on narrow screens. The transcript stays inside
the card.

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

The 2026-10-30 frozen-ring investigation used the optimized production frontend
in headless Chromium at 1440 × 1000, with the pinned synthetic Copilot and pi
histories above, unsupported-only, failure-without-detail, malformed and partial
streams. A connected sidebar ring was sampled every 80 ms through live loading,
append, canonical refresh, task docking, active-to-terminated archive preparation,
paging and byte-verified download. Copilot advanced 3549.9 ms over 46 observations;
pi advanced 3483.2 ms over 45. Controls advanced 1466.6–1533.3 ms over 20–21
observations. All stayed visible with normal motion, a running 1.8-second
animation, changing transforms, zero animation replacements/resets and no
sampled progression gap above 93.6 ms. The rAF fallback saw a maximum 16.8 ms
frame gap; the available Long Tasks observer reported none. Reduced-motion
controls were intentionally static and retained their accessible activity label.

No stopped rotation was reproduced, so no causal CSS/animation fix is claimed.
The original failing transcript and interactive browser/display/GPU conditions
were unavailable; that reproduction gap remains. Separate failing-before browser
checks established two reader-status defects: a finite-tail notice was stored
but not displayed, and malformed empty input could still say “Reading transcript”.
Both view cases were corrected without changing activity lifecycle or animation.
Run the targeted native regression from `hmem-server/frontend` with
`node --test --test-concurrency=1 tests-js/native-sessions.browser.test.js` after
`npm run build`.

`GET /api/v1/session-activity` returns boolean rows for at most 50 combined unique
`workspace_id` and `project_id` targets. Project IDs require one explicit
`project_workspace_id`; missing, deleted or inaccessible targets are not reported
as inactive. Server authorization precedes traversal and no transcript bytes or
hierarchy rows are returned. Unknown/error results remain distinct in the client.

The client probes rendered workspace/project cards and expanded sidebar workspace
entries, deduplicates grouped
workspaces, and fairly rotates batches under one physical request. Retired
requests retain their occupancy until the exact callback; authority, visibility
and target generations reject stale replies. Activity HTTP probes follow server hints, with initial/mount/visibility/reconnect
baselines and one-second coalescing for loaded targets. There is no periodic
activity polling. Membership-affecting session/link/hierarchy events wake the
selected workspace. Authorized `session_activity_changed` controls contain only
a workspace UUID and wake rendered targets in other workspaces; ordinary
append/heartbeat/runtime changes do not trigger activity probes. Hints never
advance a canonical cursor or prove that a target is inactive.

Readers without a covering canonical socket use one separate control-only
subscription. `POST /api/v1/session-activity/ticket` applies normal authentication
and cookie CSRF, returning a one-use, 60-second ticket for the existing WebSocket
URL. This capability grants no global canonical scope, snapshot or resume token.
The server registers the client before `session_activity_ready`; that frame
forces another baseline to cover the initial HTTP-to-subscribe gap.
The five-minute delivery capability expires with `session_activity_expired` and a
Close frame. The server stops hints; a cooperative client reconnects with a new
ticket and readiness baseline. A peer that does not complete Close retains its
physical slot until the handler exits.
Ticket and live-client registries each cap at 128 globally and eight per principal.
Each hint rechecks workspace read access; own membership controls refresh session
and catalogue authority. The client bounds ticket and ready/handshake phases to
15 seconds, retains the physical fetch admission through its callback, and uses
one retry timer with capped backoff. Hiding, leaving or changing identity retires
the subscription and rejects late callbacks. Unknown controls close the channel.
Global change permissions, envelopes and snapshots remain unchanged.
The Projects **Has Active Session** filter is evaluated in server navigation
queries, including unloaded descendants and ancestor retention. It combines with
status, priority, query, show-only and empty-project predicates. Canonical
`active_sessions` invalidations refresh its membership and the dropdown facets
on create, termination and new links. Ordinary append/heartbeat updates do not
restart hierarchy membership passes. Project/task placement and deletion also
invalidate scoped lists and facets. Existing request/authority guards and
transport/mounting bounds remain unchanged.

Workspace Sessions uses independent Live, Quiet and Completed toggles. Live is
active with a heartbeat within 30 seconds; Quiet is active with an older
heartbeat; Completed is canonical terminated state, regardless of linked task
status or heartbeat age. The default selection is Live. Project/task filters
combine with the selected activity union. Changing activities resets list and
facet cursors; continuation keeps the same selection.

HTTP workspace/task session lists and `/sessions/filters` accept optional
`activity=live,quiet,completed` (a unique comma-separated subset). An explicit
empty value selects none; malformed or duplicate values return 400. The list union
is applied before keyset pagination. On lists, legacy
`state`/`freshness` constraints still combine conjunctively; omission preserves
the old list contract. Omitted activity on facets preserves active-only options.
MCP `session_list` forwards optional activity and retains it in continuation
requests. Completed never means that a linked task is done.

The workspace reader fills the remaining measured space below app chrome and
wrapped filters. Its transcript and sidebar scroll internally. Linked Tasks is
a disclosure above Session Hierarchy, with the dependency card presentation and
Jump navigation, available cached metadata, and bounded linked-task paging.

Activity-aware facet options preserve freshness independence: either Live or
Quiet includes active-session options; Completed includes terminated-session
options. An opt-in selected project/task is validated independently of session
activity, so zero matches preserve the scope. Deleted, foreign-workspace and
out-of-project selections remain invalid. Omitted activity retains legacy
active-only facet membership and selected-option behavior.
