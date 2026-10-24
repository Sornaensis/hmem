# Session explorer

The frontend presents stored JSONL without changing its original bytes. Native
Codex response-item messages, function calls/results and reasoning summaries, and
Claude message content blocks render as conversations. Adjacent opposite Codex message envelopes are paired one-to-one only when their
content matches and valid timestamps differ by at most 100 ms. The event envelope
is suppressed; missing or invalid timestamps preserve both records. Thinking and tool payloads start collapsed. Unknown JSON
remains available behind a disclosure. Markdown links permit HTTP, HTTPS, mailto
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
  older windows recover complete records. Presentation text is not truncated to
  a separate short preview limit.

Direct links use `#tab=sessions&session=<UUID>`. Adding `task=<UUID>` keeps the
workspace list scoped to that task through selection, close and browser history.
The status/activity filters apply before pagination.

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

Evidence **Download all** starts at the first terminated-session cursor independently
of the visible page. It retains one metadata page of at most 50 sessions and one
entry receipt, validates strict descending UTC timestamp/UUID keysets without
rounding fractional seconds, and streams entries through the same 16-resource,
two-request admission and exclusive download reservation. This live keyset walk
is not an atomic snapshot of concurrent task associations.

Archives use ZIP64 STORE, incremental CRC32 and the existing per-entry SHA-256
receipt verification. Names contain a sanitized bounded label and full session
UUID. An OPFS archive and central-index sidecar keep content and directory memory
bounded; index copying also uses 64 KiB chunks. Without OPFS, a 32 MiB aggregate
fallback includes archive and index overhead. Acknowledged chunks become immutable
Blob parts; final assembly composes those parts without retaining a second
Uint8Array copy. Index transfers count both retained and appended parts until
acknowledged. No archive is emitted until every
entry verifies. Cancel or authority/context retirement aborts staging and joins
pending work before reuse. Cleanup failure holds the reservation and requires a
page reload. Verified archives retain their temporary source for the same
60-second browser download lifetime as individual evidence.
