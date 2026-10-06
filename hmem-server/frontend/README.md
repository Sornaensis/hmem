# Frontend validation

Use Node.js 20 LTS.

For a clean browser-test installation, run:

```sh
npm ci
npm run test:browser:install
npm test
```

`test:browser:install` provisions the exact Chromium revision required by the locked Playwright version. The timeline browser test starts from a new, empty browser context and runs the compiled production Elm fixture without authentication or backend dependencies.

## Observation drafts

Observation editing retains one dirty or saving draft during same-workspace row,
tab, and Back navigation. Return to draft, Save draft, and Discard draft remain
reachable when browsing elsewhere. Editing another observation returns to the
retained draft. Leaving the workspace requires saving or explicitly discarding
it; discard cannot interrupt an in-flight save. Permission/session revocation
and authoritative deletion retire unavailable editing. This bounded in-session
retention does not persist through a full reload or closing the tab.

Activating an Observation card by keyboard or pointer focuses and reveals the
detail heading. Back to results restores the exact originating card and scroll
position, including cards repeated in file-match groups; if a save has moved the
card, it reveals that same card at its new position. Direct links or removed
cards return focus to results. This navigation keeps the retained draft available.
Leaving the Observation tab retires its card origin; a subsequently reopened
detail returns to results. The compact workspace header scrolls with Observation
content so keyboard targets remain visible.

Each content save sends the opaque version captured when editing began. A
competing write produces a conflict and preserves the draft, even before its
live notification arrives. Keep my draft explicitly adopts the latest version
as the next save base; Use latest version replaces the draft with that content.
If a delayed reply disagrees with an already observed version, those choices wait
for one current-version check. A failed check preserves the draft and explicitly
offers the retained version; retrying that version remains conditional and can
conflict again.
Workspace, ordered subjects, Git SHA, and creation time remain immutable.
Current scaling fixtures use DTO schema 2 with required deterministic content
version UUIDs; preserved historical performance evidence describes earlier inputs.

`npm run test:observation-conditional` rebuilds the native test harness and runs
production Elm against an isolated PostgreSQL-backed API with an independent
writer and delayed genuine WebSocket deliveries. It requires the configured
test PostgreSQL tools and locked Playwright Chromium. Its fixture expires within
ten minutes and removes its browser, server, PostgreSQL process, and sandbox.

`npm run test:observation-navigation` builds production assets and checks the
navigation bridge and populated browser flows. It covers keyboard entry/return,
repeated cards, off-page links, long paths, edit/delete controls, 320 CSS-pixel
reflow (equivalent to a 1280 CSS-pixel viewport at 400 percent zoom), and enlarged
text. These automated checks do not qualify native browser zoom or assistive
technology behavior.

## Observation queries

Observation requests keep a complete applied query separate from filter inputs.
Apply filters commits the filter drafts; Match files commits concrete path input.
Save, delete, live resynchronization, and Refresh results reuse the applied query.
Unapplied filter changes pause paging until Apply filters or Revert filters;
unapplied path input leaves the previously matched files active. The displayed
applied filters describe the query behind the current results.

All observations searches saved text; By subject lists stored file and glob
subjects with exact provenance results. For files opens a composer without
changing those results. Match files applies its trimmed, deduplicated concrete
paths and filter drafts; wildcard input is rejected. Advanced filters toggles
the kind, exact subject, and Git SHA controls without resetting their values or
the applied query. Search also submits with Enter. A selected subject stays
locked while filtering its exact results.

Copy link shares the complete applied mode, search, kind, manual subject, revision,
locked facet, ordered matched paths, and selected Observation. Filter/path drafts,
content drafts, disclosures, and cached pages stay local. Version 1 fragments use
`ov=1` and a percent-encoded eight-position JSON `oq` tuple:
`[mode, search, kind, manualSubject, gitSha, facetKind, facetSubject, paths]`.
Modes are `flat`, `facets`, `exact`, and `match`; absent kind/facet values are null.
Legacy tab, focus, and Observation links remain supported. Invalid versioned
contexts restore default results atomically and show a notice.

Changed Apply, mode, facet, selection, return, and tab actions push history entries;
canonical cleanup replaces them. Back restores applied context with fresh request
guards when its query changes, retaining protected drafts in the same workspace.
Observation hits in unified search use the same intentional history behavior.
After reauthorization, validated public URL context is restored with fresh requests;
retired drafts and caches remain cleared. Reload does not persist content drafts.
Complete encoded URLs are limited to 4096
UTF-8 bytes. Larger valid queries remain active in the page and disable Copy link.
They replace the current entry with a bounded, fresh `ox` marker, carrying only
navigation context. Further oversized transitions replace that entry; a smaller
complete context pushes a new entry. Back, reload, and shared markers restore
default filters with an explicit notice rather than partially restoring paths.
No query history is stored outside the URL and the current page state.

`npm run test:observation-url` builds production assets and checks populated
restoration, history, clipboard payloads, malformed links, oversized queries,
selection cleanup, and permission admission in controlled browser fixtures.

Cards show whitespace-collapsed plain-text previews of at most 240 Unicode
codepoints and three lines. The primary subject uses at most 96 codepoints and
two lines; selection button names use at most 180 codepoints. Full content stays
in detail. Each card's separate provenance disclosure exposes the full revision
and ordered subjects, including copy actions, outside the selection button.
The compact revision uses 12 SHA characters. Content update metadata includes
time and timezone; the immutable provenance revision does not imply automatic
staleness or change when content is edited.
Content above 16 KiB uses a labelled native read-only detail reader with its
exact full value available for scrolling, selection, and copying.
Native text fields normalize line endings for display; Copy full content copies
the canonical stored text, preserving its line endings.

Failed page or refresh requests keep previously loaded results visible. Retry
results repeats the failed applied query and offset with a fresh request identity,
even when filter inputs have changed. A successful page-zero refresh replaces
membership. Retry detail can recover linked observations outside the loaded page
while keeping an owned content draft available.

## Expanded hierarchy loading

Navigation transports 50 summaries per kind per request. Expanded nodes whose
ancestors are also visible and expanded continue automatically, including the
default expanded state and Expand All. Root lists still load through explicit
navigation demand. The branch queue is fair and admits at most four physical
HTTP requests. Collapse, filter, workspace, and session changes retire logical
generations; a stale response or error still releases its physical admission.
Session resets preserve those admissions until completion.

Project and task streams finish independently. Same-filter branch refreshes
stage fresh membership until each kind finishes, preserving its displayed
cache while fresh pages arrive. Ordering or membership invalidation discards
the partial pass and coalesces a restart at offset zero. Errors pause until
retry or reopening; repeated pages without new IDs stop that kind. The client
automatic offset ceiling is 10,000 (the server allows 100,000); reaching it
reports an incomplete branch and preserves `hasMore` rather than claiming an
exhausted stream. The wire `hasMore` contract guarantees full 50-item pages;
offsets advance by transport size while cached counts deduplicate IDs.

The hierarchy is one continuous logical preorder over all cached root and
expanded child summaries. Loading another page adds reachable siblings; root
**Load more** requests the next 50-item transport page directly. Branches
continue automatically, and their end rows expose loading, completion or an
incomplete/error state with Retry.

Rendering mounts at most 25 ordinary rows across the entire viewport, plus a
bounded set of active focus, editor, inline-create, drag and native DOM-focus
pins. Every omitted run has a spacer, including runs around distant pins.
Mounted row wrappers own their spacing and are measured with ResizeObserver;
estimated geometry and mounted-only fallback measurements keep the initial
render bounded when that API is absent. A cached height-sum index handles scroll
and measurement updates without rebuilding the hierarchy. Structural changes
preserve the current row and intrarow scroll anchor when it remains present.
Offscreen focus and keyboard navigation first mount their logical target,
then scroll/focus after the stamped DOM update. Drop boundaries use logical
same-parent siblings, including neighbors outside the viewport.

Detail hydration admits at most six physical requests. Ordinary demand comes
from the globally bounded mounted/overscan set; active targets are prioritized
separately. Deep focus requests the target rather than hydrating or mounting
its entire ancestor chain. Workspace/session/generation/layout stamps reject
stale measurement and focus work. Canonical snapshot and replay behavior
remains governed by its separate transport contract. Production browser and
performance compatibility is validated by the following integration task;
these local layout and bridge tests do not replace those gates.

## Large-workspace performance qualification

The versioned harness in `perf/` runs production `Main` in locked Chromium
with intercepted HTTP and canonical WebSocket transport. It requires no backend
or external network. The unchanged seed produces the small and large fixtures
recorded in `baseline.v1.json`; fixture cardinalities, route ordering, numeric
budgets, two warmups, five samples, and nearest-rank p95 remain frozen.

For the expanded hierarchy qualification, select its immutable review base:

```powershell
$env:HMEM_EVIDENCE_BASE_COMMIT='d9ff753763f086fa4faef078b451f2e48ca1a7a4'
npm run perf:self-check
npm run perf:record-after
npm run perf:check
```

Run the record/check pair only after production and harness sources are frozen.
Both commands build production assets. Record writes distinct
`final-working-tree.*.expanded-hierarchy.v1` evidence, including its own
complete diff; it preserves `baseline.v1.json`, `budgets.v1.json`, the shared
historical complete diff, and previous after records. The explicit record
authorization preserves actual budget failures in the record; check exits
nonzero on a budget violation. Check also requires the exact recorded fixture,
configuration, budget, source, and production asset hashes before opening
Chromium, and both modes reject input drift during measurement.

Cold readiness ends at the accepted authorized `workspace_shell_v1` shell and
first root-summary anchor visible in the scrolling viewport after two paints.
Readiness joins current successfully fulfilled and validated workspace/session/
snapshot receipts to unique same-response Server-Timing identities. The final
paint evaluation reads matching same-origin Resource Timing responseEnd values
in the browser clock, with a conservative 2 ms precision guard (1 ms per reading).
Missing, duplicated, retired, unmatched or boundary-uncertain timing fails closed;
no host/browser clock synchronization is assumed. A shell completed between the
two frames may qualify the final paint; one completed after it cannot. Request
and body-byte accounting still ends after the evaluation returns to the host.
Cold sampling starts directly after `goto(domcontentloaded)`, without a separate
tree-selector wait. Its existing two-frame observation rejects missing or loading
DOM until the current authorized root is visible; the host accounting cut follows
that observation unchanged.

Each cold run retains passive request/completion identities and first, decisive,
and latest readiness evaluations with explicit omission counts. Browser response
and paint timestamps remain separate from host evaluation/return/cut timestamps.
The accounting cut is saved before diagnostic formatting; post-cut traffic does
not enter its history. Failed writer or owned-cleanup receipts retain this bounded
chronology too. Paint permits and acknowledgments are not observed by these
diagnostics, and a DOM stamp does not prove an acknowledgment. These diagnostics
add no evaluation, wait, request, predicate, or performance metric.
The production shell contains one workspace item. Preserved legacy full-fixture
resync records contain 155 small or 4,951 large items; their cold timing is not
phase-comparable with this authorized painted-root cut.
It counts all HTTP arrivals and fixture bytes before that cut, including
responses held by an asynchronous gate. Active requests and response completion
remain separate measurements. This phase differs from the historical full
snapshot bootstrap; older phase/configuration hashes remain historical evidence,
not equivalent measurements.

Background completion independently verifies exact project/task membership and
order, terminal pagination for every effectively expanded non-leaf branch,
bounded physical transport, and finite scroll reachability of every member.
Collapsed cached branches remain lazy. The run captures DOM, hierarchy-row,
observer, and physical-request high-water marks through drain, scrolling and
subsequent interactions. Observation, Timeline, direct focus, local interactions,
and the 50-frame live batch retain their separate five-sample request/byte
deltas and p95 budgets. Live settlement excludes a separate 500 ms stability
window. Timing and retained heap gates require the recorded environment
fingerprint; a mismatch is explicitly informational under the unchanged policy.

The fixture isolates frontend scaling and transport amplification; it does not
measure database or real network latency. Source route-fidelity self-checks
retain canonical ordering, list/overview DTOs, Observation token search and
facets, and UTC Timeline aggregation. The fixed browser clock makes the default
weekly Timeline request return 13 rows from the unchanged 20/500 backing sources.

Versioned after, trace, validation and evidence manifests retain bounded raw
samples, record/check evaluation, task/base/input provenance and verified trace
fingerprints as historical qualification evidence. Command receipts label the measured
Node harness phase explicitly; the preceding npm build is excluded from that
duration. Success receipts are finalized only after independently bounded browser,
owned Chromium process, server and trace retirement. A failed retirement attempts
the remaining cleanup actions and persists a failed qualification receipt. Each
required action must supply a callable `close` callback; a missing or invalid
callback is a retirement failure.
The transient trace is owned
by this task under `.scratch/expanded-navigation-perf/` and removed after
verification or failure cleanup; its manifest states that the archive is no
longer available. These performance receipts complement the production browser
tests and do not replace frontend or parent acceptance.

Live-settle timing includes completion of the current root/filter lifetime and
every effective-expanded descendant pass. New offset-zero admissions retire
prior terminal coverage immediately; untouched cached passes and explicitly
collapsed branches retain their scope. Both project and task streams must be
terminal before two paints and the separate unchanged 500 ms stability check.

Current navigation completion is indexed at request admission and response completion. Fresh passes retire old coverage before responses arrive; filter and session changes invalidate their lifetime. Changed branches dirty their ancestor proofs, while untouched completed subtrees retain their checked membership. The live-settle timer includes every new proof update and any affected subtree verification, followed by the unchanged 500 ms stability window. Independent replay self-checks cover queued descendants, stale completions, root demand, unequal streams and effective collapse.

The first accepted sparse workspace shell preserves bootstrap navigation ownership. Later authoritative snapshots retire descendant coverage and old callbacks, while selective root refreshes preserve untouched branches. Explicit continuation retries replace current coverage at each kind's requested offset and retain its valid prefix, even when the companion kind has advanced; superseded callbacks cannot complete or poison the current attempt. The self-checks exercise the actual intercepted session, navigation and resync responder callbacks without starting a browser or qualification run.

Paused navigation kinds remain incomplete even when a healthy companion response carries valid bytes for them. Unstamped continuations accept only automatic pending kinds. An explicitly selected retry also preserves any healthy automatic-pending sibling; errored and terminal companions remain unchanged. The Retry automation helper binds an explicit selected rendered Retry button to a one-shot owner, session, filter, pass and offset intent; it consumes that intent at matching admission and cancels it after click failure. Lifetime changes and collapse retire unused intents. Ambiguous unstamped retries fail closed.

Retry automation rejects an owner with automatic work queued or physical branch/root/snapshot work still active. It checks quiescence before actionability and again before arming the click; snapshot/root admissions retire stale selections. This avoids an automatic companion consuming retry intent before the selected DOM handler fires, without a timing delay. Direct producer controls still cover selecting a retry while a healthy sibling is queued.

Root completion proof accepts only the project or task kinds admitted by the current root pass. Manual root pages extend that kind’s demand; refresh continuation stops at previously demanded spans. Valid companion payloads cannot clear a paused kind. Initial root loading and its manual pages retain the producer’s whole-request retry mask: a failed bootstrap retries both pending kinds, while a failed single-kind manual page keeps its terminal companion unchanged. Established same-context refreshes and later authoritative snapshots use staged per-kind acceptance. The root Retry helper selects the actual per-kind “Retry loading” control and stamps that request; a selected retry at offset zero preserves independent companion coverage. Session, filter, and accepted authoritative snapshot retirement distinguish genuine fresh root lifetimes from ambiguous unstamped zero-offset retries.

Complete evidence collection accepts binary Git output with explicit limits: 32 MiB per command, 128 MiB across the collection, 30 seconds per command and 60 seconds overall. Required untracked source and failed-record archives remain in the complete diff. A no-index exit code of 1 is accepted only with intact bounded output; overflow, timeout, signals and command errors fail qualification. Evidence files are replaced atomically, and persistence invalidates an earlier success manifest before writing. Partial or failed writes retain an explicit failed receipt, or remove stale success receipts if writing the failure is also unavailable. These writer limits do not change the measured performance budgets or the request-accounting cut.
