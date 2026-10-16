# Frontend development

Use a supported Node.js LTS release that meets [package.json](package.json)
(Node.js >=20). Run these commands from `hmem-server/frontend`:

```sh
npm ci
npm run test:browser:install
npm test
npm run dev
```

The browser install command provisions the Chromium revision required by the
locked Playwright dependency. Vite serves development on port 3000 and proxies
API and WebSocket traffic to the server on port 8420.

```sh
npm run build
npm run preview
```

The build replaces `hmem-server/static`. Use the
[live integration reference](tests-js/testing.md) for an isolated API and the
[performance reference](perf/performance.md) for qualification commands and evidence rules.
Focused browser scripts are listed in [package.json](package.json). Conditional
and populated Observation tests need the native test harness and configured
PostgreSQL tools; their isolated fixtures expire within ten minutes and remove
their browser, server, PostgreSQL process, and sandbox. The conditional script
rebuilds the harness; the populated script uses the installed harness, which must
be relinked after snapshot migrations. Navigation checks include 320 CSS-pixel
reflow and enlarged text; they do not qualify native browser zoom or assistive
technology behavior. Local bridge/layout tests do not replace production browser
and performance acceptance.

## Configuration

`window.HMEM_CONFIG` is loaded before Elm starts and overrides the build-time
`VITE_HMEM_API_URL`, `VITE_HMEM_WS_URL`, and `VITE_HMEM_AUTH_MODE` values. Without
API or WebSocket overrides, the app uses its origin and `/api/v1/ws`.
See the [container runtime contract](../../config/container-runtime-contract.md#browser-runtime-config-strategy)
for the non-secret runtime fields and [authentication](../../auth.md) for login,
token storage, and CSRF setup. Never bake secrets into assets or runtime config.

Set `HMEM_CONFIG.workspaceSnapshotProfile` before startup to `full_v1` for complete
workspace snapshots. The default is `workspace_shell_v1`; no other value is
accepted. The setting is read once and does not change global snapshots or REST
pagination. Reload always requests a fresh snapshot.

## Editing and navigation contracts

Current task, subtask and project-embedded cards consume due-free detail and
summary payloads and show no due badge or editor. Completion dates and the
existing priority, hierarchy, dependency, readiness and lifecycle presentation
remain available. There is no supported due filter, sort or timeline series;
free-form metadata and saved-view data are not rewritten.

Historical audit values remain visible with the label **Due at (obsolete)** and
normal minute-precision date formatting. Authorized mixed-field replay follows
the server's supported-field projection; a due-only replay shows the structured
`audit_due_operation_obsolete` refusal and leaves planning state untouched.
Reload matching assets after the [V038 upgrade](../../docs/reference/upgrades.md#upgrading-across-v038)
and obtain a fresh canonical snapshot when an old due-bearing snapshot token is
refused. Compatible shell/global snapshots retain their existing behavior.

The focused `tests-js/ui-adjustments.browser.test.js` regression exercises
due-free task/subtask/project cards, canonical resync and obsolete audit replay
guidance against production Main with intercepted HTTP/WebSocket fixtures.
These checks do not establish a deployed upgrade or a new performance result.

The regular and sticky workspace headers share authoritative open-project and
open-task totals from the authorized planning-counts read. They include collapsed,
unloaded, nested and unassigned work and remain independent of navigation pages,
focus and view filters. Both headers show explicit zero totals, loading or updating
feedback, and an unavailable state with Retry when a read fails. Canonical planning
changes and resynchronization refresh the totals through one coalesced physical
request; workspace, session and audience changes retire old responses.

Startup count reads and the first ordinary navigation wave share six admission
credits under the current workspace, session and audience. Completed or retired
requests do not refund those credits. A settled canonical stream, root and header
presentation lead through a closure paint and a separate continuation paint before
normal background waves resume. Observations and Timeline allow count continuation
without granting ordinary navigation work. Failed or deferred totals remain visibly
unavailable or updating. Mandatory root recovery requests reduce the initial
allowance even when it is exhausted; transport resync and ticket retries remain
independently accounted by the browser request receipts. The healthy startup
qualification budget is not a universal ceiling on offline recovery traffic.

The hierarchy and Observation views use scrolling viewports; cached results
remain reachable even when their rows are not mounted. Root **Load more** is
explicit; expanded hierarchy branches continue automatically. Failed or
incomplete branches retain Retry and do not claim completion. Interactive
viewport loading is separate from canonical snapshot/replay, which can still
traverse a full workspace in pages. The
[navigation decision](../../architecture/adrai/decisions/R01M/R01M48140QPDRRS113KC3AERB1H--keep-interactive-workspace-navigation-bounded-and-stale-response.decision.md)
defines the transport and rendering limits.

Project filters preserve the current scroll position while replacement rows load;
the final content can clamp the position at its bottom. Subtasks share their
parent task's enclosure while each mounted row remains independently measured.

The performance harness hashes the frontend README as a source input. Historical
qualification covers its recorded source/assets and measurement phase; it does
not qualify later documentation revisions. Preserve historical baselines and
receipts when changing that input.

Project pause controls use the canonical descendant running count, including
unloaded tasks and subtasks. Stop or finish running work before pausing; reopen a
completed or archived project first. Server lifecycle refusals still provide
bounded blocker examples when cached readiness is unavailable or out of date.
Resume releases the project pause cause while independent child pauses, manual
blocks, dependencies and incomplete subtasks continue to apply. Next-task
rationales show the combined blockers.

Pause acknowledgements and live project events refresh loaded descendants through
canonical summary batches and the existing guarded membership loader. They do not
infer descendant states from a truncated response sample. Project mutations,
readiness and next-task callbacks use workspace, session and target generations,
so delayed responses cannot revive a retired workspace or overwrite newer state.
The project-pause browser regression uses two independent production Elm pages
with intercepted canonical authority. See [test scope](tests-js/testing.md).

The chooser automatically loads matching roots and expanded children through existing 50-project navigation pages, with a 10,000 offset ceiling and explicit incomplete/retry states. All loaded projects are reachable in one continuous scrolling tree: at most 25 ordinary rows mount, plus root placement and one native-focus pin, with spacers for omitted rows. The current location is revealed automatically; arrow keys and Home/End reach offscreen rows. Live changes stage complete membership passes while preserving valid selection, expansion and the scroll row's intrarow position. Selected destinations are verified directly with their canonical ancestry, including collapsed parents. Independent picker requests share the four physical navigation and six physical detail limits; ordinary membership revalidation waits until the chooser retires and its transport completes. Retired requests retain occupancy until exact completion. Native modal focus stays inside the chooser, starts in search, and returns to the originating Move action or the main content fallback on close. `tests/MoveTest.elm` and `tests-js/move.browser.test.js` cover these local contracts against production Main with intercepted HTTP/WebSocket fixtures; they do not qualify a deployed backend or new performance result.

## Product behavior

See the [web interface guide](guide.md) for editing, history, navigation and moves.

## Display and history contracts

Timestamp formatting uses per-instant DST and `YYYY-MM-DD HH:MM`; Timeline headings
use local event dates while API timestamps and histogram bounds stay UTC. Pending
groups show **Date pending**. Format in batches of at most 128 with one outstanding
batch and a demand-bounded cache. Observation history loads sequential 25-record
pages and groups each revision/date above its actor.

Attribution shows the declared role and complete `harness:model:reasoning`, keeping
the human name on declared human actions. Names wrap without truncation. Preserve
historical bot/service/system/Local User labels; unknown legacy attribution falls
back to Local User without inferred metadata. Sidebar identity uses the authorized
principal, separately from event attribution.

The shared read-only text diff compares complete literal snapshots with numbered
unified hunks and word emphasis. Null, empty and unavailable/malformed snapshots
remain distinct. Large inputs use complete simplified Before/After text. The
initial field is changed name/title, then description. Current reads are authorized
once per opening; Refresh replaces that frozen snapshot. Mark changes observed
during a pending read stale too. Global Audit has no all-workspace subscription;
loaded-at and Refresh establish freshness. A 404 alone does not establish deletion.

All/Subject switches preserve scroll, including scrolling during load. Observation
and Project card surfaces have a responsive 78rem maximum outer width; controls
and lifecycle graphs use available width. Observation details return to their
originating card if present, otherwise results. Preserve copied content line endings
and complete UUID/path/glob/SHA values.

Shared links exceeding 4096 UTF-8 bytes use a bounded marker while keeping the
query active. Restoring markers or malformed versioned links uses defaults with a
notice. Offset pagination does not provide a snapshot of concurrent writes.
