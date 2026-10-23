# Agent sessions and evidence

Open a workspace’s **Sessions** tab to explore agent conversations. Filter by
**Status** (active or saved) and **Activity** (live or quiet), then select a card.
Quiet means there has been no heartbeat for 30 seconds; it does not end a session.

Conversation text renders as Markdown. Tool calls and results expand on demand,
and thinking starts hidden. **Original JSON** keeps the current records available
in one disclosure; unknown records have their own collapsed fallback.

The transcript follows new activity by default. **Pause** preserves your position,
**Follow tail** catches up and **Older messages** opens an earlier window. Reading
or pausing never ends a session.

A task card’s session icon opens all sessions linked to that task. Expanded task
cards have separate **Active sessions** and **Evidence** disclosures above and below
their dependencies. Select a session to explore it, or use **Download** on saved
evidence. Linked tasks in the session view start collapsed and open their task cards.

**Download** retrieves the original saved JSONL and checks its final byte count and
SHA-256 before handing it to the browser. Keep the page open until the download
starts. Active sessions remain available for exploration; explicit termination
makes their captured prefix downloadable. See the [explorer reference](../reference/session-explorer.md)
for retrieval limits and browser storage behavior.

Uploading and termination are independent operations. A stopped uploader can
leave an active session, and an explicit termination seals only the server's
committed prefix. Inspect the receipt rather than assuming local file EOF or task
status established evidence completeness.

## Capture a local producer with MCP

Initialize MCP and declare the workspace and your identity with `set_workspace`.
Choose a session UUID and call `session_upload_start` with that UUID, an explicit
regular-file `path` and `format_label`. Registration returns `validating`
promptly. The process continues following while you use ordinary MCP tools;
`session_associate` links the session to one or several tasks. Each task can also
link to several sessions. The local path stays in the MCP process.

Inspect `session_upload_status` while capture proceeds. A partial final record
remains pending. `session_upload_flush` captures one finite file-size boundary
and may commit a complete JSON record without a final newline while the session
remains active. A later record must begin with LF or CRLF after that EOF record.
Malformed or incomplete final JSON is reported as pending rather than skipped.
When a control returns `requested_operation.pending=true`, inspect status until
that operation has a completed receipt or error; the pending response is not a
completed capture.

For evidence, call `session_upload_stop`, then inspect its completed verified
receipt. Copy `committed_offset`, `revision` and `sha256` into a separate normal
`session_terminate` call as `expected_offset`, `expected_revision`, `final_bytes`
and `final_sha256`. The final byte count is the committed offset, which can be
smaller than `capture_boundary` when an unfinished tail remains. Termination
checks that exact server prefix atomically; it does not upload, read the file or
require the uploader. Another authorized editor may perform it. An exact retry
uses the original pre-seal expected revision.

Stop, flush, EOF, an idle uploader and MCP process loss never seal the session.
After a restart, declare context again and explicitly start with the same UUID
and file to verify the complete committed prefix before resuming. Existing
followers keep their original workspace and identity when MCP context changes.
The process admits four active followers and two shared follower HTTP requests;
normal MCP calls, including termination, retain independent capacity. A live
follower that observes termination stops safely.

Evidence is the finite captured prefix. Output written later by the producer,
including an acknowledgement of termination, is intentionally outside it. It
does not prove task acceptance and does not change task status. To reconstruct
original evidence through MCP, call `session_evidence` and execute each returned
`next_request` unchanged until absent. Decode and stream bytes in offset order,
then compare their final count and SHA-256 with the immutable receipt. Links may
still be added after sealing, but evidence cannot be edited or reopened.
