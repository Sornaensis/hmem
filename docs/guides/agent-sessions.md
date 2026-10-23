# Agent sessions and evidence

Open a workspace's **Sessions** tab to inspect its agent activity. The list shows
50 sessions at a time. Use Lifecycle and Heartbeat filters to find active,
terminated, fresh or stale sessions, and Older/Newer to move through pages.
Heartbeat becomes stale after 30 seconds without a server acknowledgement; it
does not mean that the session has terminated or that a local uploader has
stopped. The connection indicator describes live change delivery separately.

Select a session to see its creator, committed byte/record counts, linked tasks
and transcript. A direct link uses `#tab=sessions&session=<session UUID>` in the
workspace URL. Linked tasks open their existing task focus; that task's Sessions
and evidence panel links back to its sessions. A later task association can also
refer to evidence that was already terminated.

The transcript follows the tail by default. **Pause** preserves your readable
position while the new-byte count increases. **Follow tail** catches up, and
**Load older window** reads a bounded earlier section. The viewer shows at most
100 records and 2 MiB at once. A window starting inside a record omits that
leading fragment with a notice; older windows recover earlier complete records.
Provider-specific objects, arrays and scalar JSON remain available through
**Original JSON**, with their byte positions. Reading or pausing never seals a
session.

A terminated session displays its immutable final byte count and SHA-256.
**Save original evidence** opens your browser's file picker and streams the
authorized canonical bytes to that file. The application verifies the final
count and SHA-256 before completing the save. Keep the page and workspace open
until verification completes; cancelling or changing authority aborts the save.
Browsers without the required writable-file capability show that limitation.
The bounded MCP `session_evidence` tool remains available for retrieving original
evidence in chunks. Neither path turns the displayed, bounded record window into
the source of the saved evidence.

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
