# Frontend development

Use a supported Node.js LTS release that meets [package.json](package.json)
(Node.js 20.x or >=22). Run these commands from `hmem-server/frontend`:

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
Focused browser scripts are listed in [package.json](package.json).

Elm assertions run with `elm-test-rs`; pass Elm file paths to select a suite (for
example, `npx elm-test-rs tests/AuditDiffTest.elm`). This replaces `elm-test` to
remove its unpatched `braces` dependency. Vite stays on the patched 6.4 release
line to retain the current Node and browser requirements. A scoped npm override
updates `node-elm-compiler`'s pinned `cross-spawn` to the compatible 6.0.6 security
patch; retain it until the compiler dependency includes that fix.

`npm run test:observation-state` checks live query ownership and explorer state;
`npm run test:observation-state-browser` checks URL, preferences and navigation
against the current production assets. `npm run test:observation-tree` checks
literal subjects, lazy reveal, explorer keys and staged live replacement;
`npm run test:observation-tree-regression` covers the affected discovery, counts,
URL, scroll and recovery flows. Run `npm run build` before these browser scripts. Text debounce is 250 ms and retains workspace/session/actor/mode tokens;
SHA validation issues no read until every populated SHA is canonical. Requested
and last completed query identities remain separate during replacement reads.

Conditional
and populated Observation tests need the native test harness and configured
PostgreSQL tools; their isolated fixtures expire within ten minutes and remove
their browser, server, PostgreSQL process, and sandbox. The conditional script
rebuilds the harness; the populated script uses the installed harness, which must
be relinked after snapshot migrations. For current-source qualification, build
`hmem-server:exe:hmem-test-harness` with Stack, obtain the SHA-256 of that build's
`stack path --local-install-root` executable, then run
`npm run test:observation-populated-current -- --current-source-harness-sha256=<digest>`.
The supplied full digest must match before startup; the default populated command
retains its historical binary pin. Logical reachability scans inspect actual mounted
rows after normal scroll/paint, advance by one viewport, and require contiguous
coverage within the existing 15-second scan deadline. Navigation checks include 320 CSS-pixel
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

Subject Tree path cards use decorative parent and sibling connectors derived from
the logical preorder. Incomplete sibling lists keep a dashed continuation.
Connector intervals stop within their owning tree across viewport gaps; deep
indentation compresses with the same gutter as the lines. Accessible depth and
canonical path copying retain the full hierarchy.

Subject Tree ordinary branch and terminal reads pause while its tab is hidden.
In-flight replies retain their slots until their exact callbacks; returning resumes
incomplete expanded paths without discarding cached state. Explicit selection
reveal remains limited to its own ancestor chain and typed terminal.

Repository workspace Observation totals have their own authority, invalidation and
physical request identity. They load and refresh on every tab, including Subject
Tree, independently of browse filters and match/tree totals. Mode changes preserve
a settled total; canonical Observation changes and resynchronization refresh it.
Project startup settlement uses this independent header read.

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
remain reachable even when their rows are not mounted. Navigation sends independent project/task demand; completed or paused streams
are skipped by the server. A skipped empty page leaves that stream's cached
membership and continuation state intact. Root **Load more** is
explicit; expanded hierarchy branches continue automatically. Failed or
incomplete branches retain Retry and do not claim completion. Interactive
viewport loading is separate from canonical snapshot/replay, which can still
traverse a full workspace in pages. The
[navigation decision](../../architecture/adrai/decisions/R01M/R01M54TJAY4K07ZQ2SYFHBMCPRP--keep-interactive-workspace-navigation-bounded-and-stale-response.decision.md)
defines the transport and rendering limits.

Expanded owners selected in one scheduling wave share a navigation batch POST
(up to four owners). Every owner keeps its independent cursor, filter and read
guards, admission credit and conservative slot until the shared callback, even
after collapse or session retirement. Complete response identities are validated
before applying any outcome; owner errors retain successful siblings and retry
independently. Root GET remains independent. Capacity accounting preserves the
existing asymmetry: ordinary branch admission counts branch and Move slots;
Move admission additionally counts an in-flight root request. Batching does not
add a new admission lane or change the startup paint permits.

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
Dependency events refresh ancestor readiness through deduplicated summaries
batches of at most 100 unique raw IDs. Each target keeps its own canonical
generation and planning-read revision; a stale target does not discard current
siblings in the same batch. These reads update readiness only. Dependency panels
retain their richer paginated reads, and readiness-driven ordering reconciles each
affected navigation owner once.

The project-pause browser regression uses two independent production Elm pages
with intercepted canonical authority. See [test scope](tests-js/testing.md).

The chooser automatically loads matching roots and expanded children through existing 50-project navigation pages, with a 10,000 offset ceiling and explicit incomplete/retry states. All loaded projects are reachable in one continuous scrolling tree: at most 25 ordinary rows mount, plus root placement and one native-focus pin, with spacers for omitted rows. The current location is revealed automatically; arrow keys and Home/End reach offscreen rows. Live changes stage complete membership passes while preserving valid selection, expansion and the scroll row's intrarow position. Selected destinations are verified directly with their canonical ancestry, including collapsed parents. Independent picker requests share the four conservative navigation admissions and six physical detail slots; ordinary membership revalidation waits until the chooser retires and its transport completes. Retired requests retain occupancy until exact completion. Native modal focus stays inside the chooser, starts in search, and returns to the originating Move action or the main content fallback on close. `tests/MoveTest.elm` and `tests-js/move.browser.test.js` cover these local contracts against production Main with intercepted HTTP/WebSocket fixtures; they do not qualify a deployed backend or new performance result.

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

All Observations/Subject Tree switches retain their scroll context, including scrolling during load. Observation
and Project card surfaces have a responsive 78rem maximum outer width; controls
and lifecycle graphs use available width. Observation details return to their
originating card if present, otherwise results. Observation card disclosure uses the
shared compact +/− control; Subject Tree path chevrons remain independent.
Preserve copied content line endings
and complete UUID/path/glob/SHA values.

Shared links exceeding 4096 UTF-8 bytes use a bounded marker while keeping the
query active. Restoring markers or malformed versioned links uses defaults with a
notice. Offset pagination does not provide a snapshot of concurrent writes.

## Session change delivery

`Feature.SessionDelivery` owns canonical session resource demand; widgets do not
subscribe to transcript-bearing events. `DemandSessionResource` explicitly admits
workspace/keyset pages, task session panels, metadata, linked-task pages or a
focused byte range. The ceiling is 16 distinct resource targets; a focus requires
both metadata and range targets and is rejected atomically with `demand_limit`
when they do not fit. Repeating existing demand does not add queue entries.
Closing focus removes its implicitly owned metadata demand. A separately
demanded metadata target remains owned by that consumer.

A fair queue admits at most two physical HTTP requests, independently of planning
loaders. List/link pages contain at most 50 rows. Focus reads use 64 KiB windows
and retain the latest four base64 windows. Validate canonical padding/zero pad
bits, exact decoded length, byte cursors and `has_more` before advancing. Metadata
refresh decides whether more committed bytes are missing and whether the range
uses active `/transcript` or sealed `/evidence`; no widget action seals a session.
Heartbeat freshness is derived from `last_seen_at`, separately from lifecycle.

Dedicated session/link actions coalesce invalidations. A dirty version and the
existing workspace/session/audience/target-generation guard fence each callback.
Invalidation during a read preserves both its physical admission and a later
refresh obligation. Only that exact callback releases its slot, even after
closing demand, changing workspace/principal or revoking permission. Errors
pause a target for explicit demand/retry or a newer invalidation; 401/403 retire
demand and follow the existing authorization path.

Every accepted workspace snapshot, including old/default full and root-only
full/shell profiles, retires session generations and dirties demanded resources.
Accepted empty global snapshots do the same only for session resources. Existing
workspace-root validation and snapshot item allowlists remain intact. With no
current authorized demand, this module sends no HTTP requests. Session reset
clears demand/cache while retaining outstanding physical slots and monotonic IDs
until their callbacks arrive. These are delivery primitives; session widgets and
route/filter presentation are separate consumers.

## Sessions workspace UI

`Feature.Sessions` owns the Sessions tab and the single card-local task dock.
Direct session links use `#tab=sessions&session=<UUID>`. Lifecycle and heartbeat
filters are canonical server predicates applied before keyset pagination; each
filter/cursor combination has a separate demand identity. Lists contain 50 rows
and retain at most sixteen navigation cursors. Hidden pages and closed focus
remove their demand without releasing outstanding physical request slots.

The viewer consumes finite windows of at most 2 MiB, one 64 KiB response at a
time. JavaScript frames bytes before UTF-8 decoding, with one incomplete 1 MiB
JSONL carry and one bounded adjacent-pair candidate, independently of the four
raw-range cache slots. Presentation reserves the latest 100 prose-bearing
occurrences and latest 100 activity-only occurrences independently, each limited
to 2 MiB of original record bytes: at most 200 occurrences and 4 MiB combined.
These are raw-byte budgets, not heap-size guarantees. A mixed occurrence counts
once in the prose reserve. Unknown metadata and paired event envelopes consume
neither reserve; genuine repeated messages remain separate source occurrences.
An initial mid-record fragment is omitted. CRLF and byte offsets remain exact.

Each finite window publishes its completed presentation once while framing
acknowledgements continue per chunk. Its captured end stays fixed through an
unacknowledged-chunk retry, even if metadata reports newly committed bytes.
Same-owner canonical resynchronization retires HTTP and bridge generations while
preserving the framer, retained rows and native disclosure nodes. Source run
anchors survive head eviction and preserve boundaries across omitted messages.
Reading above the tail preserves a visible occurrence anchor; following and
anchor callbacks yield to newer viewer interaction and owner/generation changes.
Changing the session, dock owner, workspace or authorization retires presentation.
Intermediate `viewer-progress` acknowledgements carry only the guarded cursor;
only the finite window's final acknowledgement decodes a replacement row list.
Transport Retry keeps its failed cursor. Decode Retry keeps displayed rows while
reframing at most the current 2 MiB finite tail, never all elapsed session bytes.

`npm run test:session-groups` includes the live append/synchronization regressions.
The native replay additionally takes `SESSION_LIVE_NATIVE_PATH` and its proven
`SESSION_LIVE_NATIVE_ID`; optional `SESSION_LIVE_NATIVE_PARENT` and
`SESSION_LIVE_NATIVE_AGENT_PATH` verify producer lineage. The source working
directory must resolve to this repository. `SESSION_LIVE_NATIVE_MAX_BYTES` bounds
the final prefix between 2 and 4 MiB (default 4 MiB); both prefixes end at original
LF boundaries. The test requires initial agent prose and a tool occurrence that
remain within the final retention reserves, then reveals the later unchanged
native bytes. A configured source is required for native replay qualification;
an ordinary unconfigured run skips that local-source case.

Evidence saving uses a separate one-chunk `SessionSave` target within the same
sixteen-target/two-request admission limits. The immediate click opens the File
System Access picker. Production `session-save.js` streams each validated chunk
to the writable and updates pinned `@noble/hashes` 1.8.0 SHA-256 state. Only a
completed write acknowledgement re-arms the next HTTP chunk. Immutable receipt,
byte count and digest must match before close; cancellation, scope retirement or
verification failure aborts the writable. The one-writer reservation spans
picker acquisition, pending writes and joined abort cleanup. Unsupported browser
capability is explicit; no whole-session Blob or digest buffer is constructed.
