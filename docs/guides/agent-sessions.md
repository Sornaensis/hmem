# Agent sessions and evidence

Open a workspace’s **Sessions** tab to explore agent conversations. **Live**
is selected initially. Toggle **Live**, **Quiet** and **Completed** independently
to combine activity states; selecting none shows no sessions. Completed means
the session is terminated, regardless of its heartbeat age or linked task status.
Use the **Project** and **Task** dropdowns to narrow the list. They offer
projects/tasks with active sessions when Live or Quiet is selected, and saved
sessions when Completed is selected. A valid selected scope remains selected
even when no sessions match; a project includes its descendant projects
and nested tasks. **More projects/tasks** opens the next option page. Sessions
form a parent tree; compact ancestor paths keep child sessions understandable
when their parent is outside the selected activity/filter page.

In the Projects tab, **Has Active Session** shows active-session work and the
ancestor paths needed to reach it, combined with the other hierarchy filters.

Quiet means there has been no heartbeat for 30 seconds; it does not itself end a
session. The server automatically closes a session after more than one hour
without activity when all its linked tasks are done or cancelled. Sessions with
no task links also qualify. A task in any other status, including a deleted task,
keeps the session open. Closing preserves the captured evidence and updates the
green activity lights.

Audit history records session start, actual termination and new task connections.
Upload progress, heartbeats and runtime-label changes refresh live session views
without adding audit entries. Repeating an operation adds no duplicate entry.

Session cards and readers show `[role] harness:model:reasoning`. Each missing
component displays `unknown`; long labels wrap. Provider metadata remains in the
tooltip. Runtime identity is explicitly supplied, never inferred from the
connection caller, provider or transcript format. The original caller remains
stored with the evidence.
Conversation text renders as Markdown. Tool calls and results expand on demand,
and thinking starts hidden. Consecutive thinking, tool calls and results appear
as one collapsed **Activity** group with a timestamp and event count. Expand it
to inspect the individual events; user and agent messages stay visible outside
the group. Messages split activity runs, including messages mixed with tools in
one native record. An expanded run stays expanded as live events arrive or the
view synchronizes.
Expanded tool payloads display formatted, highlighted
JSON. Encrypted or redacted thinking displays *Thinking...*. Copilot and pi
native captures show their messages and tools; unsupported meaningful content
has a short notice while routine metadata stays hidden. Copilot tool requests
and execution starts are separately labelled and share their tool-call ID.
pi history shows each native entry's parent or root: source order can include
alternate branches, and ancestry outside the current window is not expanded.

The transcript follows new activity while you are near its end. Scroll up to read
without losing your position; returning to the end resumes following. Event times use your browser’s local time:
HH:MM today, or YYYY-MM-DD HH:MM on other days. Session cards show last activity,
and green lights in the workspace sidebar indicate active sessions, including quiet ones.

A task card’s **Active Agent →** button docks its active sessions and transcript
inside the card, which widens by about 40% where space permits and stacks on a narrow screen. Its active sessions form a parent tree. Select a session there to read
it without leaving the task. Expanded task cards keep saved captures under
**Evidence**; use **Download** on saved evidence. In workspace Sessions, expand
**Linked Tasks** above **Session Hierarchy** to see its task cards. **Jump to task**
focuses a linked task; **More tasks** and **First tasks** navigate bounded pages.
The transcript, linked tasks and hierarchy scroll within the available viewport.

The completed reader in workspace Sessions places **First**, **Previous**, **Next**
and **Last** beneath the conversation. **Cancel** appears while an archive operation
is running; **Retry** appears after a recoverable error or cancellation. A cleanup
failure requires reloading the page. Expand activity,
complete-message and presentation controls when needed to read larger content.

**Download** retrieves the original saved JSONL and checks its final byte count and
SHA-256 before handing it to the browser. Keep the page open until the download
starts. Active sessions remain available for exploration; termination
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
`{"model":"actual-target-model","role":"Developer","harness":"codex"}`.
A new non-null declaration requires the actual short agent role and model.
Add `harness`, `provider` or `reasoning` only when known.
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

Stop, flush, EOF, an idle uploader and MCP process loss never themselves seal the
session. The independent server cleanup policy above may later seal its committed
prefix; the uploader does not own that decision.
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

## Retrieve task evidence with MCP

Call `task_triage` with `task_id` and `path` pointing to a new local ZIP file.
The parent directory must exist. The tool downloads every linked saved session,
checks the complete archive, and returns its byte count, SHA-256 and session
count. Existing files are preserved; a failed download produces no destination.
Active sessions are excluded, as in **Download all**.

## Import a completed conversation

For a session that could not be connected while running, call `session_import`
with an explicit completed JSONL `path`, stable `session_id`, `task_id`,
`format_label` and short `title`. The historical task can already be done or
cancelled. The tool leaves task status unchanged and validates the whole file
before registering it. An optional parent and explicitly known historical runtime
identity can be supplied; caller attribution records the importer separately.

Inspect `session_upload_status` until it reports **imported** and a verified
terminated receipt. The finite import uploads and seals the complete declared
file; invalid tails and source changes fail instead of saving a partial prefix.
Live flush/stop controls do not apply. Retry the same import UUID, source and
metadata to resume or confirm a completed import after a lost acknowledgement.
Pending imports stay out of live conversations and saved evidence. Completed
imports appear under the task's **Evidence** and are included by `task_triage`.
