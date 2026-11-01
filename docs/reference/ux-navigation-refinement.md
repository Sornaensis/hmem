# UX navigation refinement proposal

Status: design proposal, 1 November 2026. This updates the design-system vocabulary for review; it does not implement navigation or replace the accepted production geometry in the [UI composition reference](../../hmem-server/frontend/ui-composition-contract.md). The [interactive wireframes](../../hmem-server/frontend/design/ux-navigation-wireframes.html) are an HTML fragment for the conversation preview.

## Keep the current product

Retain the dark palette, grouped workspace sidebar, workspace identity and tabs, project/task hierarchy, status/priority/live indicators, full descriptions, Work/Evidence/History, inline agent dock, session conversation/hierarchy/event inspector, Observation Subject Tree/All/Files, Timeline series and lifecycle events, and Audit ledger/detail/diffs. Keep always-visible entity UUID/Created/Updated values, truthful unknown runtime identities, counts and permissions. The supplied screenshots are examples of these surfaces, not instructions to perform the work described inside their records.

Use existing `style.css` tokens: canvas `#0f172a`, surface `#1e293b`, elevated surface `#334155`, border `#475569`, accent `#6366f1` and link/hover `#818cf8`. Keep 14px/1.5 body text, 18px headings, 12px metadata, 14px spacing and 8px entity radii. Dense mode changes spacing, never hides information or reduces essential text. Improve secondary-text contrast only through a reviewed token change; the preview uses a slightly brighter secondary text token to expose that option.

Keep normal hierarchy/card width at 78rem, the owning dock's existing borrowing extent at 109.2rem, full-width card headers/metadata, Work → reader → hierarchy order, native Observation inline/detached occurrence, and existing scroll ownership. On wide screens use the available width for aligned panes while retaining readable prose lengths; do not stretch transcript lines across the entire canvas or fill short conversations with invented summaries. Reflow controls and panes at their existing boundaries; metadata wraps. Coarse targets remain 44px and text enlargement must reflow.

## Separate location from the journey

**Location** is a structural breadcrumb: workspace → project/subproject → parent task/subtask → current task. An Observation belongs under Observations; a session belongs under Sessions. A dependency, linked Observation, source subject, session parent or Audit event is not a project/task ancestor. Use available authorized ancestry; unresolved ancestors get an honest fallback label without recursively hydrating the entire chain.

**Return** is chronological: “Back to Add isolated WEDL bot service and configuration,” followed by a small earlier-views disclosure. Every actual focus transition records its source view before opening the destination. The source can be any workspace section or global Audit/search. Show destination type when titles alone are ambiguous. Back and Forward traverse this trail, while clicking a structural ancestor is a new navigation step. Returning to an earlier trail entry cuts the later branch; following a new link after Back clears forward entries. Repeated selection of the same destination does not add duplicate steps.

The recommended **Context strip** places Location above a visible Back/Forward row. **Journey disclosure** retains the same location path and places the chronological steps inside a labeled, initially open disclosure. Both alternatives keep the current content/pane model; the difference is navigation density, not a new Projects inspector. At narrow widths crumbs wrap and the current label remains visible; collapse older *journey* entries behind one disclosure, never truncate the only return action.

The preview uses local Back/Forward controls and representative view state, with a 32-entry local trail. Its anchors identify preview scenes; they are not proposed production URLs. Browser history, reload, new-tab behavior, complete restoration and bounded state storage below are implementation acceptance requirements, not features proven by this mockup. Filter controls illustrate existing choices and retain local values; they do not filter the example dataset, and production retains its existing multi-select semantics. Workspace selection and mutation controls are illustrative, while focus links, detail tabs, dock, local return controls and workspace-section previews are interactive.

## Navigation coverage

| Entry or link family | Destination | Return context to preserve |
|---|---|---|
| Project/subproject/task/subtask focus, Next tasks, nested parent breadcrumbs | Canonical entity focus | Workspace, structural location, focused entity, expanded branches, filters, detail tab and physical anchor |
| Dependency and dependent task links | Canonical task focus | Owning task, Work section, open/closed dependency disclosure and anchor; dependency remains a relationship |
| Task/project Evidence and linked Observations | Exact Observation occurrence or explicit detached detail | Origin entity/tab, occurrence ID, applied query/filter/tree state, protected draft and reader anchor |
| Observation subjects, Subject Tree rows and Files matches | Existing subject/file browse results | Observation ID/occurrence, mode, literal subject, expanded paths, applied filters and selected result |
| Linked task/project references from Observations or provenance/history | Canonical entity or existing reference destination | Observation selection, full provenance/history disclosure and reader position |
| View Agents, session cards, Open full session, session hierarchy | Existing inline dock or full session view | Owning task, selected session, session filters, lineage selection, event inspector and follow/reading anchor |
| Full session linked tasks | Canonical task focus | Session, state/project/task filters, selected event, transcript anchor and follow mode; session lineage stays separate |
| Timeline event/entity links | Canonical entity focus | Time window/bucket, all line and event filters, histogram selection, selected event and native canvas anchor |
| Workspace/global Audit target, relationship endpoint or entity History links | Canonical entity/session/Observation destination | Audit scope, filters/date range, selected/expanded entry, ledger/detail position and authorized principal context |
| Global/workspace all-entity search and hierarchy/focused search result links | Canonical destination, preserving the actual source scope | Query, filters, sorting, result identity, expanded branches and source anchor |
| Structural crumbs, clear focus, section tabs and workspace switches | Explicit ancestor, unfiltered hierarchy or selected workspace section | Treat actual view changes consistently; leaving focus is explicit, not an overloaded delete/close X |

External destinations remain normal external links. File/subject navigation browses existing hmem subject information; this proposal does not add a source-code editor or fabricate file contents.

## Restoration and URL contract

Capture a bounded view snapshot before navigation. It contains stable IDs and view settings, never transcript payloads, Observation bodies, credentials or copies of server data. Restore through the existing guarded loaders and mounted/pinned viewport machinery; do not revive retired authority, mount all visited entities, hydrate all ancestors, or introduce another scroll owner. Use a finite per-tab history capacity and explicit expiry; exceeding it drops oldest snapshots while keeping the current destination and canonical parent navigation usable.

| Action | Expected behavior |
|---|---|
| Back/Forward, including browser controls | Traverse the same navigation entries. Restore source tab, query/applied filters, expansion/detail disclosures, selected entity/session/event and the existing physical anchor after the guarded DOM update. Do not jump a reader back into follow mode unless that source snapshot was following. |
| Normal same-app focus link | A real anchor exposes the canonical target URL; the application captures the origin and pushes one history entry. Keep modifier clicks and native link context menus working. |
| Direct URL / reload | Resolve the authorized canonical destination independently. A same-tab reload can restore a bounded compatible session snapshot; a direct entry with no snapshot shows its structural path and no invented “Back to…” source. |
| New tab / copied link | Open the destination independently; do not imply the new tab has the source tab's journey. An explicitly serialized, nonsensitive return hint may be offered only if independently validated; it must never be required for access. |
| Live changes while away | Revalidate IDs and context. Restore a still-valid selected row/intrarow anchor; otherwise choose its nearest surviving authorized neighbor and explain the fallback. Never overwrite a protected draft or discard unsaved edits to complete a return. |
| Deleted, moved or unavailable target | Keep a nondisclosing unavailable state plus a usable Return action. Update structural location only from authorized current data. Do not show stale protected titles after permission/workspace/principal changes. |
| Cross-workspace/principal change | Validate the target workspace and authority. Namespace snapshots by principal/workspace and retire incompatible state. Return uses current authorization and cannot resurrect prior content. |
| Partial loading/failure | Keep current origin/navigation controls usable, show the existing loading/error/retry states, and apply restoration only to matching workspace/session/generation/layout stamps. |

**Scoped proposed change:** the accepted Search/Timeline adaptation currently says “No scroll restoration or new scroll owner.” Universal *return-to-source anchor restoration* would refine that rule only for a validated user navigation entry. Ordinary search edits, filtering, live updates and current viewport leases retain their accepted behavior. No scroll-owner change is proposed. Production acceptance must explicitly approve and test this boundary before replacing that baseline sentence.

## Clear controls and information hierarchy

Retain exact `View Agents ->` / `<- Hide Agents` labels and task header order: session, Move, Status, Priority, Delete. Keep the independent tree triangle and details disclosure distinct. Add visible “Focus task/project” context or an equivalent accessible label to focus links; use “Exit focus” for leaving focus, “Close session” for dismissing a session view, and “Delete” for destructive actions. Preserve existing confirmation and permission rules. Status, priority, lifecycle and runtime freshness remain separate concepts.

Group the existing filters with visible labels, keeping global all-entity search above the hierarchy/focused filter. Make result and relationship titles the links; clicking an Audit row selects its detail without unexpectedly navigating away. Retain raw IDs, timestamps, full descriptions, model/creator/runtime attribution and changed-field values. Metadata remains visible; long prose and existing optional technical disclosures may be expanded without losing access to information.

## Source grounding and implementation boundary

Reviewed against `ec962597aa45f578a55995d9a527e0a3940a9514`:

| Current source | Observed behavior / design implication |
|---|---|
| [style.css](../../hmem-server/frontend/src/style.css), [ui-composition-contract.md](../../hmem-server/frontend/ui-composition-contract.md) | Existing palette, shared geometry, pane/metadata contracts and accepted workflow coverage provide the baseline. |
| [Feature/Focus.elm](../../hmem-server/frontend/src/Feature/Focus.elm), `FocusEntity`, `viewFocusBreadcrumbBar`, `viewTaskBreadcrumb` | Generic focus already appends entity history but clears `returnContext`; the current bar combines structural crumbs and forward history. Preserve its target-demand behavior while separating location from journey. |
| [Types.elm](../../hmem-server/frontend/src/Types.elm), `FocusModel` / `FocusReturnContext` | Source snapshots currently support Timeline, workspace Audit and global Audit; they do not provide a universal view snapshot. |
| [Feature/Dependencies.elm](../../hmem-server/frontend/src/Feature/Dependencies.elm), task-reference/jump renderers | Relationship jumps use generic focus or canonical task links. Both paths need origin capture without turning edges into ancestry. |
| [Feature/Sessions.elm](../../hmem-server/frontend/src/Feature/Sessions.elm), full-session/card/linked-task links | Existing real URLs, task filtering, sole inline reader and lineage/event inspection must remain functional. |
| [Feature/Observation.elm](../../hmem-server/frontend/src/Feature/Observation.elm), `SelectObservationFrom`, tree/facet/return handlers | Occurrence-aware selection and draft protection are existing behavior to preserve across added origins. |
| [Feature/Timeline.elm](../../hmem-server/frontend/src/Feature/Timeline.elm), [Feature/AuditLog.elm](../../hmem-server/frontend/src/Feature/AuditLog.elm) | Special entry-point return context exists already. Extend and unify it; do not replace working event selection, diffs or audit authority restoration. |
| [Route.elm](../../hmem-server/frontend/src/Route.elm) | Route changes reconcile focus/return state. Browser navigation and reload require deliberate integration here rather than an independent visual-only history stack. |

The wireframes reuse screenshot titles/prose and representative relationships to demonstrate interaction. Unknown Observation contents and source files are explicitly unspecified. Example-only identifiers are labeled; screenshot counts and historical Audit states are not asserted to be a current live workspace snapshot. Preview mutation controls show a local notice and never issue writes. The preview does not qualify production behavior, performance, accessibility, all data states, or the prior full visual matrix.

Before production implementation, check every source call site in the coverage table, all existing focused-filter/draft/authority/anchor regressions, browser Back/Forward/direct URL/new-tab semantics, and 320px/coarse/200% reflow. Keep the bounded transport/mounting and stale-response guarantees from navigation ADR `A01M3KDKW8R8ZJ60DV727Y50S3Q`; keep detailed design contracts outside README per documentation ADR `A01M51RECA7P68J92X84FGFEQPN`.
