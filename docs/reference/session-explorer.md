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
