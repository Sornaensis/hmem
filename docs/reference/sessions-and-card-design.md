# Sessions Navigator and card design specification

Implement the approved Navigator composition in workspace Sessions and inside task cards. Use a compact description brief and expandable Details in both task and project cards. This is the implementation target; it does not claim the interface already behaves this way.

The accepted visual references are the **Navigator** stage of `hmem-sessions.html` and the card in `hmem-task-sessions.html`. The Focus alternative is excluded. The requirements below are authoritative and self-contained: mock records, counts, dates, abbreviated UUIDs and surrounding page chrome are illustrative, not production requirements.

## Scope and precedence

Workspace Sessions may change its local toolbar, session navigator, selected-session header and reader. Retain the existing application shell, workspace header, overview, tabs, navigation and permissions.

On Projects, change only task/subtask and project card headers, interiors and footers. The task-card reference's surrounding project heading and overview are excluded. Preserve the outer Projects page, filters, project hierarchy, indentation, ordering, containment, sibling widths and current card-width and measurement ownership. Allow natural vertical height and flow changes from the new brief and Details, with truthful row measurements; do not freeze old heights. Use scoped styles; do not restyle the shell or stretch the entire hierarchy to fit a card. Project cards receive the shared brief, Details and full UUID treatment, not a new session explorer.

This specification supersedes only the Sessions pane order, embedded task-card pane order and summary splitter target, full-description placement, local reader presentation, and scoped UUID presentation in the [UI composition reference](../../hmem-server/frontend/ui-composition-contract.md). Its unrelated surface contracts and historical qualification records remain intact. Existing [session reader behavior](session-explorer.md), authorization, bounded loading, evidence integrity and synchronization contracts continue to apply.

## Shared visual treatment

Retain the current dark palette in both OS appearance preferences: canvas `#0f172a`, surface `#1e293b`, elevated surface `#334155`, text `#f8fafc`, muted text `#94a3b8`, border `#475569`, indigo `#6366f1` and `#818cf8`. Use existing semantic status and priority colors. Selection uses the established indigo surface treatment rather than a new color system.

Use the existing system font stack, 14px base text, approximately 1.5 line height, 14px spacing and 8px corner radii. Keep metadata at 12px and compact labels at least 11px. Distinguish panes with spacing and a single divider; avoid nested bordered cards for every session row. Titles and identity values wrap instead of becoming unreadable fragments. Keep prose near 70–80 characters per line, centered within a wide reader; code and structured output may use the reader's full width.

## Workspace Sessions Navigator

### Composition

Place the existing Sessions title and local filters above one explorer. Retain independent Live, Quiet and Completed toggle buttons, followed by authorized Project and Task facets. Wrap the toolbar when necessary. A count may be shown only when its scope is explicit and supported by current data; do not present a loaded page count as a total or invent whole-workspace activity counts.

The explorer has a left session navigator and a flexible right reader. Use a navigator near 240px on ordinary content widths, increasing toward 310px when space allows, within a 232–320px range. Reserve at least 360px for the reader. Below 640px of available explorer width, put the navigator above the reader; retain a tree rather than turning related sessions into an unrelated tile grid. Base reflow on available content space, including 200% enlargement, not on assumptions about the shell width.

Retain the measured remaining-height ownership of workspace Sessions. Navigator and conversation remain independently scrollable within that space; headers, filters, errors and controls remain reachable in short viewports. Do not introduce a second full-page scroll surface or fixed mockup height.

### Navigator

Use compact selectable rows with an activity indicator, wrapping session title, role and runtime identity, and Last activity. Retain parent-child connectors, at most three indentation levels, compact context for excluded parents, and the explicit sixteen-ancestor truncation notice. Ancestor context is not a matching session result and is not counted as one.

Keep a clear selected row using indigo fill and a leading accent. All row content remains selectable or accessible without relying on hover. Preserve list loading, empty, error/Retry, Newer and Older sessions states and bounded pagination. Changing filters keeps the existing URL, cursor and facet semantics.

Workspace Sessions retains explicit selection: opening it without a selected session shows a compact instruction in the reader, alongside the useful navigator. Do not auto-open a session because the wireframe shows a populated example. Closing a selected session returns to this state without clearing valid filters.

### Selected session

The reader header contains the session title, canonical activity/lifecycle label, runtime role and model identity, Last activity, existing close action, and eligible saved-session download action. Put parent context near the title and linked tasks in a compact disclosure below it. Keep long task titles, actual task status and priority readable when expanded, with existing task destinations and pagination.

Session termination and linked task completion are independent. Always render a linked task's status from its actual task data; a completed session must never make the linked task appear Done.

## Shared transcript presentation

Add local **Conversation**, **Activity** and **Full transcript** presentation controls to both workspace and embedded readers. These are new presentation modes over the current authorized, bounded reader data; they are not new API scopes, evidence formats or an instruction to fetch an entire active session.

| Mode | Presentation |
|---|---|
| Conversation | User and assistant prose in source order, with adjacent tool/reasoning activity collapsed into the existing expandable groups. Fold identifiable system/developer context into clearly labelled disclosures at its source position. |
| Activity | Expandable activity groups and source-derived message markers in source order. Use event type, occurrence time and real counts; do not generate progress summaries or infer success from a tool request. |
| Full transcript | Retain the loaded bounded transcript's original chronology and access to original message text and existing raw/structured record inspection. System/developer records remain identifiable and expandable. This does not mean all historical records are simultaneously mounted, an unbounded active read, or an active-session download. |

Default to Conversation for a new selection. Switching modes must preserve the nearest surviving source occurrence and reading position, with a predictable fallback when that occurrence is hidden in the new mode. Keep disclosure state and modes local to the current reader/session authority; clear stale payloads and incompatible state on selection or authority change. No presentation mode may alter canonical transcript bytes or discard access to records because their content looks like instructions.

Fold context only when its native record role/type identifies it. Do not classify text by keywords, merge nonadjacent records into a synthetic summary, or reinterpret quoted instructions as executable instructions. A disclosure must expose the original content. Preserve encrypted reasoning placeholders, unsupported-event inspection, bounded JSON formatting, attachment metadata treatment, framing/tail notices and integrity/error states.

Activity group counts and timestamps retain their existing definitions: counts describe the actual group, and the displayed time is the final event's eligible occurrence time. Missing time is **Time unavailable**. A message or group does not acquire a time from a nearby event or receipt clock.

Keep progressive publication, continuous older-history loading, refetchable gaps, rendering limits, manual scroll intent, source anchors and default following. **Jump to latest** retains its current visibility rule after manual upward input; loading, a mode change or a disclosure alone must not reveal it. Its acquisition, Retry and End-key behavior remain unchanged. Mode changes must not issue unrelated history demand or reset a session to the tail.

Preserve saved-session verification, cancellation, retry and eligible downloads. A download remains gated by terminated state and the final byte-count/SHA-256 receipt. Full transcript adds no active evidence download capability.

## Task cards with an embedded Navigator

### Card structure

Use one full-width task header, the shared compact brief and Details area, an agents section, and an always-visible metadata footer. When agents are open, place the task's navigator left of its reader below the brief. Remove the old description/reader/right-tree three-column arrangement. Do not repeat the task title inside the session header; show the selected session's title there.

The card retains its current width allocation and containment. The explorer must fit inside the owning card. Use a navigator near 232px, increasing toward 300px when sufficient room remains for a readable reader. Below 640px of available card-body width, stack navigator before reader; at 640px and above, use the two-pane composition with at least 360px for the reader. This replaces the old 950px three-column threshold only inside the card. Measure the card/container, not the browser viewport; do not change outer page or sibling widths to satisfy it.

Intentionally move the existing task summary resize affordance to the navigator/reader boundary. Update its accessible name to **Resize session navigator and conversation**. Preserve pointer dragging, Arrow Left/Right adjustments, Home/End limits and mounted-lifetime reset. Keep the equivalent 20–60% allocation range, constrained further when necessary to preserve readable panes. The separator is absent in one-column layout, where its hidden geometry must not receive focus. Do not retain an extra description column solely for the old splitter.

### Task actions and state

Retain View Agents/Hide Agents, authoritative association checks, activity indicators and refreshing/unavailable states. Keep an open Hide Agents control usable while metadata refreshes. Closing agents collapses only the embedded explorer; it does not collapse Details, discard an edit draft or mutate the task.

The embedded navigator shows this task's active sessions, including quiet active sessions. Do not copy the workspace Live/Quiet/Completed toolbar into it or add saved sessions to its active list. Retain pagination, Retry, loading and no-active-sessions feedback. Opening the dock keeps the existing first-fresh-active-session selection behavior; retain the selected session while it remains valid. Maintain one shared selected reader and the existing retirement rules when a card unmounts.

Keep **Jump To Session ↗** visible in the reader header or footer. It opens the full Sessions view for that session and task filter, preserving supported reading/disclosure continuity and return-to-task behavior. It is navigation, not a new fullscreen overlay or outer Projects redesign. Preserve the task header's existing focus, move, status, priority and delete controls, and the card's subtask, Details and hierarchy actions with their existing permission and lifecycle gates.

## Shared task and project Details

### Brief and full description

Derive the brief deterministically from the existing description: render the first nonempty top-level Markdown block as an excerpt, clamped to three visual lines. Use the existing safe Markdown rules; a first list or code block may serve as that block. Do not generate a summary, save a second description field or change source Markdown to create the brief. An empty description keeps the existing honest empty-description treatment.

Provide an explicit **Read full description** action for a nonempty description. It opens Details and selects Description regardless of the previously selected tab. An eligible empty-description edit affordance also opens Description. The full, unclamped Markdown description and existing editor live in that tab, with the same save/cancel, errors, permission checks and draft protection. The brief is not an editing surface or a replacement for the complete text.

### Details tabs

Both task/subtask and project Details use **Description**, **Work**, **Evidence** and **History**, in that order. Description is the new first tab; it does not replace Work. The first opening defaults to Description. Subsequent ordinary reopening retains the selected tab for that mounted entity; Read full description always selects Description. Selection and expansion belong to each entity, not to a global shared tab.

| Tab | Task or subtask | Project |
|---|---|---|
| Description | Full task description and permitted editor. | Full project description and permitted editor. |
| Work | Existing project context when applicable, loaded-subtask destinations and dependencies. Keep their loading, permission and lifecycle behavior. | Existing Next tasks panel and its supported actions. |
| Evidence | Existing saved task sessions, eligible per-session download and task evidence download behavior. Preserve availability/disabled states. | Existing informational direction to open a task to view its evidence. Do not add project transcript, project session explorer or project evidence download. |
| History | Existing task entity audit history and permitted history actions. | Existing project entity audit history and permitted history actions. |

Retain Details/Hide Details controls and accessible expansion state. Preserve project and task creation, focus, move, status, priority, delete, child/subtask actions, readiness feedback and audit confirmations wherever currently supported. Hidden or forbidden operations must not reappear because the mockup omits authorization states. A reader with no edit permission can expand descriptions and inspect authorized content without gaining mutation controls.

## Full UUIDs and metadata

Every entity UUID already displayed as interface identity metadata in these scoped surfaces must show the full canonical value. This includes task/subtask and project metadata footers and any displayed session, ancestor, linked entity or workspace UUID in a touched Sessions surface. Preserve existing visibility rules; this does not require adding an ID to every navigator row or changing the outer workspace header. The rule does not apply to UUID-like text inside preserved transcript records, descriptions or source prose; never rewrite that content to change or expand an ID.

Keep the task/project footer visible when Details and agents are collapsed. Render the full UUID as its copy control, alongside existing Created, Updated and supported completion metadata. Allow the UUID and metadata to wrap at narrow widths and 200% text enlargement. Do not use shortened text, ellipsis, clipping, a tooltip-only value or a hidden full value as a substitute for displaying it.

Copying uses the complete canonical UUID and preserves native keyboard activation, propagation protection and existing success/failure feedback. Copying an ID must not focus the entity, toggle Details or select a different session. Keep actual timestamp formatting and honest unavailable values; do not substitute mock dates.

## Data and interaction invariants

Live means active with a heartbeat within 30 seconds; Quiet means active with an older heartbeat; Completed means canonical terminated state. Workspace toggles select their union, with Live initially selected. Task status, session state, heartbeat freshness and Last activity remain distinct.

Last activity uses nullable canonical `lastActivityAt`, independent of the loaded transcript window. A missing value is **Unavailable**, without created/last-seen/receipt-time fallback. Runtime role, model, harness, reasoning and provider come from declared runtime identity; missing components remain unknown. Session creator attribution is separate and must not be promoted into the runtime or native message author.

Use actual authorized entity data for titles, state, priority, counts and destinations. Do not infer completed work, generate a qualitative progress score, fabricate counts or repeat wireframe fixtures as application data. Preserve authorization generations, stale-response rejection, bounded physical admissions, mounted-row limits and source/disclosure anchors across layout changes.

Controls need visible focus, semantic labels and existing keyboard behavior. Presentation tabs expose selected state and connected panels. Disclosures expose expanded state. Preserve at least the existing fine-pointer targets and 44px coarse-pointer targets, compact status/priority controls, reduced-motion behavior and native scroll access. Reflow controls and text without overlap at 320px width, short viewports and 200% enlargement.

## Source map

These are the existing seams to adapt, not permission to broaden the change. The source baseline is `bf22a5bd2c99728dc7eaaaa9a903c0a94afbc81d`; follow symbols if line numbers move.

| Source | Relevant behavior |
|---|---|
| [Sessions.elm](../../hmem-server/frontend/src/Feature/Sessions.elm), `view`, `viewFacet`, `sessionTree`, `viewDetail`, `viewLinkedTasks` near lines 953–1100 | Workspace filters, navigator, explicit selection, parent and linked-task context. |
| [Sessions.elm](../../hmem-server/frontend/src/Feature/Sessions.elm), `reconcile`, `viewTaskIcon`, `viewTaskActiveChoices`, `viewTaskActiveWithChoices` | Dock selection, association proof, task hierarchy, embedded reader and Jump To Session. |
| [Sessions.elm](../../hmem-server/frontend/src/Feature/Sessions.elm), `viewRecords`, `viewSealedRecords`, `viewLatestControl`, `viewSessionIdentity`, `viewSessionActivity` | Shared active/verified presentation, following, identity, last activity and download gates. |
| [SessionTranscript.elm](../../hmem-server/frontend/src/SessionTranscript.elm), [session-viewer.js](../../hmem-server/frontend/src/session-viewer.js), [session-save.js](../../hmem-server/frontend/src/session-save.js) | Bounded transcript fragments, disclosures, scrolling, archives and integrity. |
| [Cards.elm](../../hmem-server/frontend/src/Feature/Cards.elm), project Details near 1832–1847 and `viewTaskCardBody` near 2223–2510 | Description editors, entity controls, task dock composition, Work/Evidence/History and metadata. |
| [CardSections.elm](../../hmem-server/frontend/src/CardSections.elm), `tabsWithEvidence`, `cardDisclosure`, `metadata`; [Helpers.elm](../../hmem-server/frontend/src/Helpers.elm), `copyableValue` | Shared tabs/Details, footer and copy interaction. |
| [style.css](../../hmem-server/frontend/src/style.css), tokens, sessions and task-card rules; [task-card-layout.js](../../hmem-server/frontend/src/task-card-layout.js) | Existing palette, measured layout, responsive containment and accessible splitter. |
| [Api.elm](../../hmem-server/frontend/src/Api.elm), `AgentSession`, `SessionRuntimeIdentity`, `SessionAncestor`; [session occurrence reference](session-occurrence.md) | Canonical metadata and timestamp meaning. |

## Implementation units and acceptance

The following units are planned implementation work. Completing this specification does not start or complete any of them. Keep one source owner for shared reader and card files during each implementation window; use explicit dependency edges for integration acceptance.

| Unit | Scope and local acceptance |
|---|---|
| Shared reader presentation | Add the three local modes and native-role context folding in both readers. Preserve bounded rendering/loading, source chronology/access, disclosure/scroll anchors, following and verified-download behavior. |
| Workspace Navigator | Recompose only Sessions local content into the left navigator and right reader; contextualize linked tasks; preserve filters, explicit selection, pagination and scoped full UUID display. Integrate the shared reader modes when available. |
| Task and project cards | Add deterministic briefs and Description-first Details without losing Work/Evidence/History. Put the active task navigator left of its reader, move the accessible splitter, retain task actions and show full copyable footer UUIDs. Project cards get shared brief, Details and full UUID changes, without an embedded session explorer. |
| Combined acceptance | After the three implementation units, verify shared-file integration, permissions, responsive behavior and all invariants below against matching implementation inputs. |

Implementation acceptance must cover:

1. A workspace without selection, selected live/quiet/completed sessions, all toggle unions including none, facet selection beyond the loaded page, hierarchy truncation, list/linked-task pagination, empty/error/Retry states, and actual independent task status.
2. Task dock first-fresh selection, switching, quiet active sessions, no-active/loading/stale/failed association states, close during refresh, Jump To Session and supported return continuity. Saved evidence stays in Details → Evidence.
3. Conversation/Activity/Full transcript switching on ordinary prose, native system/developer messages, tools/results, missing times, encrypted reasoning, unsupported records, large structured content, incomplete tails and recoverable errors. Assert record access and source order without expanding fetch or DOM limits.
4. Task, subtask and project brief derivation; first Details opening; Read full description overriding another selected tab; full Markdown/editor save/cancel and protected drafts; each entity's original Work/Evidence/History content and read-only gates.
5. Full visible/copyable UUIDs in every existing scoped ID surface, including collapsed card footers, long labels, 320px width and 200% text. Verify exact clipboard payload and that copy does not trigger card/session navigation.
6. Navigator-before-reader reflow, short-height native scroll access, wide prose measure, splitter pointer/keyboard limits and reset, focus restoration, coarse-pointer targets and reduced motion. Assert outer Projects shell, hierarchy containment/indentation, sibling widths, measurement ownership and navigation are unchanged while card heights remeasure naturally.
7. Progressive history/appends, manual upward scrolling, conditional Jump to latest, mode changes, same-authority surface transfer and active-to-verified transition preserve the reading occurrence and reject stale replies. Saved-session cancellation/retry/integrity/download gates remain intact.

Use the repository's supported focused Elm/bridge/browser checks for affected behavior, followed by matching build and combined acceptance. Existing historical qualification captures do not prove fidelity for this redesign and must not be overwritten or relabelled. Keep future validation evidence bounded to the implemented scope and its actual inputs.
