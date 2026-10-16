# Web interface guide

## Timeline and history

The workspace Timeline shows Create, Complete, Delete, Archive and Cancel events.
Choose an entity, time window and bucket size; these choices persist per workspace.
The default window is the last 30 days with weekly buckets. **All time** starts
at the earliest event. Dates use the browser timezone from page load; reload
after changing it. Missing dates show **—**, pending dates **…**, and invalid
dates **Invalid timestamp** or **Unknown date**.

Observation cards show the current reviewed revision, or the creation revision
for legacy observations. Expand **History** for recorded assertions, copyable
SHAs and dates. Collapse to stop loading; use **Retry** after a failed page.
Actor labels identify people or declared model roles. The sidebar identifies
the signed-in user; declarations do not change permissions.

Use **View diff** in Audit or History to compare changed project names, task
titles and descriptions. Choose a field to compare its complete Before and
After values. Empty, absent and unavailable values remain distinguishable;
Markdown is shown as source. Opening the comparison leaves a collapsed Audit
entry collapsed, and closing returns focus to the original control.

**Against current** loads a fixed current snapshot for that opening. Switching
fields reuses it; **Refresh** replaces it. Check the loaded-at time and entity
update time. Observed changes mark it stale; disconnection makes freshness
unknown. A failed current read leaves the historical comparison usable.

## Observation drafts and search

Keep one unsaved Observation draft while moving between rows, tabs or Back
within a workspace. Use **Return to draft**, **Save draft** or **Discard draft**.
Save or discard before leaving the workspace; a save cannot be interrupted by
discard. Reload, closing the tab, access loss or deletion can retire the draft.
Drafts are not saved in browser storage.

A competing edit preserves your draft and shows a conflict. **Keep my draft**
uses the latest version on the next save; **Use latest version** replaces your
text. A pending version check may delay these choices; a failed check keeps the
draft. Workspace, subjects, creation SHA and creation time cannot be edited.

**Apply filters** submits filter inputs. **Match files** submits concrete paths
and filters; wildcards are rejected and duplicate paths are removed. Refresh,
save, delete and Retry reuse the applied query. Apply or Revert changed filters
before paging. Unsubmitted paths leave previous matches active; a selected
Subject remains locked while filtering its results.

Back and shared links restore applied queries, paths and selection with fresh
results. Drafts and open disclosures stay local. An oversized link keeps the
query active but uses a bounded marker; restoring that marker or a malformed
link shows defaults with a notice. Failed reads retain available results and
offer **Retry**. **Copy full content** preserves stored line endings; UUID,
path, glob and SHA controls copy complete values even when abbreviated.

## Projects and tasks

Workspace headers count all open projects and tasks, including collapsed,
unloaded, nested and unassigned work. Filters do not change these totals.
Loading, updating and unavailable totals are labeled; failures offer **Retry**.

Scroll to reach loaded hierarchy and Observation results. Root **Load more** is
explicit; expanded branches continue automatically. Incomplete or failed branches
show feedback and Retry. Filters preserve scroll position where the new content
allows it. **Show empty projects** defaults on and is saved per workspace.
Uncheck it to require a matching task in the subtree; ancestors stay visible.
Workspace group disclosure is saved globally.

Stop or finish running descendants before pausing a project. Reopen a completed
or archived project first. Resume clears that project's pause; independent child
pauses, manual blocks, dependencies and incomplete subtasks still apply. Next-task
rationales show combined blockers. Expanded project cards load all next tasks;
blocked work and closed dependencies have separate, initially collapsed count
disclosures. Open dependencies appear directly on task cards.

Tasks have no deadline editor, due filter or due timeline series. Completion dates,
priority, hierarchy and readiness remain available. History labels old deadlines
**Due at (obsolete)**. A replay that changed only deadlines is refused; supported
fields in mixed historical updates can still be replayed.

### Moving cards

Editors can use the **Move** arrow to open **Select Destination**. Search names
and descriptions; Active and Paused filters are independent of hierarchy filters,
with Completed and Archived optional. The tree includes empty projects and reveals
the current location with its ancestors. Badges identify the source, current
parent, selection and carried subtree. Expand branches separately from selecting
a destination, then use **Confirm**.

Projects can move to **Workspace root**; tasks can move to **No project**. A
subtask changing projects becomes a top-level task; descendants follow their root.
Current or subtree destinations cannot be selected. Refused moves retain the
selection and explain recovery. Arrow keys and Home/End reach offscreen rows.
The dialog starts focus in search and returns it to the Move action on close.

## Administration

Workspace admins can soft-delete a workspace after confirmation; deleting the
selected workspace returns home and retains its contents. MCP has no workspace
delete tool. The **Administration** tab requires explicit workspace admin access;
implicit local superadmin sessions keep it hidden. Failed membership writes retain
the form and list. Wait for the permission recheck, or use **Retry session** if it fails.
