# Agent sessions and evidence

Open a workspace’s **Sessions** tab to explore active agent conversations. **Live**
is selected initially; use the **Live** and **Quiet** buttons to filter activity.
Use the **Project** and **Task** dropdowns to narrow the list. They offer only
projects/tasks with active sessions; a project includes its descendant projects
and nested tasks. **More projects/tasks** opens the next option page. Sessions
form a parent tree; compact ancestor paths keep child sessions understandable
when their parent is outside the selected activity/filter page.

In the Projects tab, **Has Active Session** shows active-session work and the
ancestor paths needed to reach it, combined with the other hierarchy filters.

Quiet means there has been no heartbeat for 30 seconds; it does not end a session.

Conversation text renders as Markdown. Tool calls and results expand on demand,
and thinking starts hidden. Expanded tool payloads display formatted, highlighted
JSON. Encrypted thinking displays *Thinking...*; unrecognized records stay hidden.

The transcript follows new activity while you are near its end. Scroll up to read
without losing your position; returning to the end resumes following. Event times use your browser’s local time:
HH:MM today, or YYYY-MM-DD HH:MM on other days. Session cards show last activity,
and green lights in the workspace sidebar indicate active sessions, including quiet ones.

A task card’s session icon opens all sessions linked to that task. Expanded task
cards have separate **Active sessions** and **Evidence** disclosures above and below
their dependencies. Select a session to explore it, or use **Download** on saved
evidence. The first two linked tasks appear as compact cards that focus their tasks.
**Show All Linked Tasks** reveals additional links, with bounded pages when needed.

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
regular-file `path`, `format_label` and a very short descriptive `title`, normally
3–8 words such as “Repair session filters”. The title describes the work; the
format label describes the transcript. An optional `parent_session_id` selects
one existing session in that workspace and stays fixed. Registration returns `validating`
promptly. The process continues following while you use ordinary MCP tools;
`session_associate` links the session to one or several **in_progress** tasks;
start a task before adding a session. Existing links remain valid after completion. Each task can also
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


Task cards show active conversations and saved Evidence in compact session cards,
with last activity and the declared creator model. Select a card to explore that
session. Use its Download button for original JSONL, or Evidence **Download all**
for a server-compressed ZIP of every linked saved session, including other pages. Progress and
Cancel stay in that task’s Evidence section. Keep the page open until the archive
finishes; an error produces no partial download. The archive follows the current
paginated task links while it runs.
