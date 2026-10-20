# Web interface guide

## Timeline and history

Opening a focused project or task keeps your current hierarchy filters and search
text. Its contextual controls let you adjust these filters while exploring.
Switching focus or choosing the same focus link keeps those selections and closes
previous search results. The focused target stays accessible even when excluded
by the filters; its descendants follow the selected filters.
The workspace toolbar keeps its own filters throughout. Editing it updates only
your workspace preferences. Global search has a separate input and keeps your
focused view for return when cleared. Closing focus or returning to its Timeline or Audit source uses those
workspace preferences. Focused changes never replace saved workspace filters.

The workspace Timeline shows Create, Complete, Delete, Archive and Cancel events.
Choose an entity, time window and bucket size; these choices persist per workspace.
The default window is the last 30 days with weekly buckets. **All time** starts
at the earliest event. Dates use the browser timezone from page load; reload
after changing it. Missing dates show **—**, pending dates **…**, and invalid
dates **Invalid timestamp** or **Unknown date**.

Observations have their own line counting additions in each interval, independent
of the lifecycle action filters. Hover or focus a line to see selected lifecycle
activity and Observation Created, Deleted and historical Total counts. Total
counts surviving Observations, including those created before the visible range.
Hover or focus a bar to see Open projects and Open tasks + subtasks for the whole
workspace; bars keep their own right axis. Zero rows and empty hints are hidden.
Unavailable history and future totals are labeled instead of shown as zero.

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

## Search all entities

Type in **Search all entities** to search after a short pause. The **Search Entities**
bar selects Projects, Tasks and, in repositories, Observations. Choices are saved
for the signed-in actor and workspace, separately from hierarchy filters. Selecting
no types pauses searching. The bar stays visible when you clear the query and
return to your previous view. While updating or after a failure, the last completed
results remain labelled with their query; use **Retry search** to try again.

Results use compact previews with copyable IDs. **Jump To** opens the canonical
project, task or Observation, including targets outside your current filters.
Observation targets show their full content in a read-only detail view.
**Back to search results** or browser Back restores the query, selected types,
scroll position and Jump To control without changing your saved filters. If a
target is deleted or access changes, the return control remains available.

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

Observation text filters update after a short pause. Clearing text, selecting a
kind, and completing or clearing a SHA filter update immediately. Enter a full
lowercase Git SHA; incomplete values stay local while the last completed results
remain visible. Subject Tree and All Observations remember separate pairs of search fields; Files keeps its own search text. All Observations remains the initial mode.
Advanced shows the number of active filters when closed. Kind, exact subject in
All Observations, and creation/current/history SHA filters apply live; closing Advanced retains
them. Incomplete SHAs show associated feedback until completed or cleared.
Both Subject Tree and All Observations show **Subjects** above **Search observations**.
Subjects narrows stored paths literally and case-sensitively, including wildcard
characters. Search observations combines full-text and semantic matches when
available; the status reports hybrid search or a full-text fallback. Both fields
apply together. Clearing either returns to the top while retaining the other.
Matching ancestors remain visible in Subject Tree;
counts include distinct observations. A path label selects and focuses it; its
chevron, Enter or Space expands its children and file/glob observation lists.
Several branches can stay open. Closing a parent remembers its descendants and
open observation card. Arrow keys, Home and End move focus without changing path
selection; card controls keep their normal keys. Deep paths retain visible level
cues and full canonical copying when indentation is compressed. Root and observation-list continuation is explicit;
expanded child branches continue as needed. An open card stays visible while live
updates follow any pages needed to confirm its membership. Shared links reveal their ancestors
and focus the requested path or observation.
**Match files** explicitly submits concrete paths; wildcards are rejected and
duplicate paths are removed. Editing the path list leaves previous matches active.

Back and shared links restore queries, paths and selection with fresh
results. Filter typing replaces the current history entry; explicit navigation
creates a new entry. Workspace preferences retain supported queries and
disclosures for the current actor. Content drafts stay local. An oversized link keeps the
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
allows it. Without saved workspace filters, **Show** defaults to **All**,
**Show empty projects** is checked, project filters select **Active** and
**Paused**, task filters select **Todo**, **In Progress** and **Blocked**, and
**Priority** is **Any**. Filter changes are saved per workspace.
Uncheck **Show empty projects** to require a matching task in the subtree;
ancestors stay visible.
Workspace group disclosure is saved globally.

Tasks with **In Progress** status and **Blocked** parents with a running subtask
appear first, followed by Todo, other Blocked, Done and Cancelled tasks. Within
each group, higher priority comes first, then title; the parent's status stays Blocked.

Stop or finish running descendants before pausing a project. Reopen a completed
or archived project first. Resume clears that project's pause; independent child
pauses, manual blocks, dependencies and incomplete subtasks still apply. Next-task
rationales show combined blockers. Expanded project cards load all next tasks;
blocked work and closed dependencies have separate, initially collapsed count
disclosures. Open dependencies appear directly on task cards.

Root tasks can depend only on other root tasks. Subtasks can depend only on
siblings under the same parent. Remove conflicting dependencies before moving
a task into or out of a subtask group.

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
