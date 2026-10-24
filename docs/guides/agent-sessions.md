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

**Running model** identifies the declared model executing the session, separately
from **Connected by**, the original caller that created its connection metadata.
These may be different models. A running model is explicitly declared, not
independently detected; missing declarations display **Unknown**. Saved evidence
uses **Session model** for its last declaration. Resuming capture does not replace
the original caller.

Conversation text renders as Markdown. Tool calls and results expand on demand,
and thinking starts hidden. Consecutive thinking, tool calls and results appear
as one collapsed **Activity** group with a timestamp and event count. Expand it
to inspect the individual events; user and agent messages stay visible outside
the group. Messages split activity runs, including messages mixed with tools in
one native record. An expanded run stays expanded as live events arrive or the
view synchronizes.
Expanded tool payloads display formatted, highlighted
JSON. Encrypted thinking displays *Thinking...*; unrecognized records stay hidden.

The transcript follows new activity while you are near its end. Scroll up to read
without losing your position; returning to the end resumes following. Event times use your browser’s local time:
HH:MM today, or YYYY-MM-DD HH:MM on other days. Session cards show last activity,
and green lights in the workspace sidebar indicate active sessions, including quiet ones.

A task card’s **Active Agent →** button docks its active sessions and transcript
beside the card, or below it on a narrow screen. Select a session there to read
it without leaving the task. Expanded task cards keep saved captures under
**Evidence**; use **Download** on saved evidence. The first two linked tasks
appear as compact cards that focus their tasks.
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
Start the task first so its status is **in_progress**. Choose a session UUID and
call `session_upload_start` with that UUID, the required initial `task_id`, an explicit
regular-file `path`, `format_label` and a very short descriptive `title`, normally
3–8 words such as “Repair session filters”. The title describes the work; the
format label describes the transcript. An optional `parent_session_id` selects
one existing session in that workspace and stays fixed. Registration returns `validating`
promptly while the server validates and creates the task-linked session. A
rejected task leaves no new session; inspect upload status for the task/status
error, start the task and retry. The process continues following while you use
ordinary MCP tools. `session_associate` adds other **in_progress** tasks.
Existing links and resumed captures remain valid after completion. Each task can also
link to several sessions. The local path stays in the MCP process.

When known, supply `initial_runtime_identity`, for example
`{"model":"actual-target-model"}`. Add `provider` or `reasoning` only when known.
This describes the captured session, not necessarily your own accepted MCP
identity. Omit it or use null if unknown. It initializes a new session only;
creation retries and resumed uploaders preserve the current declaration.

For a model switch, read `runtime_revision` with `session_detail`, then call
`session_set_runtime` with that value as `expected_runtime_revision` and the new
`runtime_identity`. Explicit null clears the declaration to Unknown. An identical
retry at the immediately preceding runtime revision is safe; other stale updates
conflict and require fresh metadata. This changes neither transcript bytes nor
heartbeat activity. Saved sessions reject runtime changes.

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
with last activity and separate runtime-model and original-caller labels. Select a card to explore that
session. Use its Download button for original JSONL, or Evidence **Download all**
for a server-compressed ZIP of every linked saved session, including other pages. Progress and
Cancel stay in that task’s Evidence section. Keep the page open until the archive
finishes; an error produces no partial download. The archive follows the current
paginated task links while it runs.
