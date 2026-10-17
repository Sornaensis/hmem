# Browser performance qualification

For frontend maintainers. Run from `hmem-server/frontend` after installing the
locked Chromium as described in the [development reference](../development.md).
The harness runs production Elm with deterministic intercepted HTTP/WebSocket
fixtures, without a backend, database, or external network.

## Commands and result policy

For focused filtering verification, set PowerShell
`$env:HMEM_EVIDENCE_BASE_COMMIT = '30e428e65f5f6b34ae24fe6e5a32ab3aa1efb547'`,
then run `npm run perf:self-check`, `npm run perf:record-after`, and
`npm run perf:check` sequentially against stable sources and assets, without
overlapping browser workloads. Both fresh evaluations and trace retirement must
pass. The historical baseline, budgets and fixtures remain the qualification inputs.

Final task `a70d06e3-8993-4af2-a328-914e0dc35c37`, project
`a606c62b-9675-44bc-9013-75d1624551b2`, owns the `focused-filtering.v1`
after, trace-manifest, validation-record, evidence-manifest and complete-diff
receipts in the verified OS temporary directory's
`hmem-focused-filtering-a70d06e3-8993-4af2-a328-914e0dc35c37` child.
Retain these five files through project closure plus 30 days, then dispose.
Each receipt/archive is bounded to 64 MiB; verify/hash and retire the sole
`temporary/large-focused-filtering-trace.zip` before success. Unresolved
diagnostics expire on resolution or task closure.

```sh
npm run perf:self-check
npm run perf:record
npm run perf:check
```

`perf:record` explicitly authorizes replacement of `baseline.v1.json`; direct
record invocation also requires `--authorize-baseline`. It retains actual
PASS/FAIL results and exits zero when only target budgets fail. Harness,
selector, schema, build, and runtime errors fail the command. A zero record exit
therefore does not establish budget compliance. Check emits each scenario/metric
result and exits nonzero on an enforced violation. Task-qualified record/check
write separate artifacts and preserve the historical baseline.

Both modes use two warmups and five samples per scale; nearest-rank p95 is the
maximum of those five samples. Named interactions retain separate latency and
`{ count, bytes, routes, routeBytes }` deltas, including the 50-frame live batch.
Selectors, expected row/card effects, request/byte/DOM/row limits, reload/resync,
duplicate-request, and console invariants apply on every machine. Timing and
retained heap apply only when the full recorded environment fingerprint matches;
otherwise they remain visible and informational.

[budgets.v1.json](budgets.v1.json) is the frozen budget authority. Do not weaken
it without an explicit project decision. The large-workspace ceilings include
2,500 DOM nodes, 250 collection rows, 100 ms local-interaction p95, and two
requests/500 ms p95 for direct focus and Observation load-more. Live limits are
50 frames, 12 follow-ups, 500 ms p95, no whole-workspace reload, and no duplicate
request per repeated target; attributable heap is 64 MiB. Comparable timing/heap
requires the recorded OS, CPU, RAM, Node/npm/Elm/Vite/Playwright/Chromium versions,
viewport, and headless fingerprint.

## Task profiles

Review [evidence-profile.mjs](evidence-profile.mjs) and freeze production/harness
sources before choosing a mapped review base. Unknown bases fail before
measurement. These recipes select distinct task-owned outputs:

| Profile | `HMEM_EVIDENCE_BASE_COMMIT` | Task |
| --- | --- | --- |
| Observation scaling | `c5230077a643d06e7ca5a4ff6b5616cfb90862ec` | `b9f1de65-f016-4178-89a0-ac13a5d7fff8` |
| Observation rendering | `ed3aca53afe346f928dc00f2e2a7b0a8e07aaecb` | `cbb38fd2-fc89-449c-a308-168374f23f82` |
| Expanded hierarchy | `d9ff753763f086fa4faef078b451f2e48ca1a7a4` | `555ebf6e-8442-43f4-a75d-c33fcffc76f4` |
| Planning counts | `f546cafd26adae37772d2383262e87596ea7151b` | `b401c358-1a68-404e-bd18-86f40bb3b771` |
| Loading efficiency | `dc44350bcb8360a7979d82f33271ad8dcdcb9b83` | `0a677d44-065a-4205-af9b-6e1d9b36a833` |
| Due removal | `33abd80dd13dbc6c850397eeb13f79047d0f7c91` | `1e420484-81b9-4251-b485-82abca34c800` |

For example:

```powershell
$env:HMEM_EVIDENCE_BASE_COMMIT = 'd9ff753763f086fa4faef078b451f2e48ca1a7a4'
npm run perf:self-check
npm run perf:record-after
npm run perf:check
```

Record-after/check build production assets and reject input drift. Check requires
the exact task/base/revision, recorded evaluation, source/assets/contracts,
budget/configuration/fixture hashes, and trace manifest before qualified success.
The historical after-record argument spelling maps to the profile's fresh paths;
arbitrary output redirection and baseline recording under Observation task
profiles are rejected. The measured Node harness duration excludes the npm build.
Production browser tests and parent acceptance remain separate gates.

Planning counts uses the original project review base above and the ordinary
navigation fixtures and interactions. Its isolated evidence ownership does not
enable Observation scaling. Use the same self-check, record-after and check
recipe with `HMEM_EVIDENCE_BASE_COMMIT` set to the planning-count base. Arbitrary
redirects and baseline recording are rejected for this profile too. Its exact GET
`/api/v1/workspaces/:id/planning-counts` response counts current nondeleted open
project/task identities once, including unassigned tasks and subtasks. Counts
follow fixture status, placement and deletion changes at request time. Request
and body-byte accounting includes these reads under the unchanged budgets.

Current ordinary Observation pagination is source-anchored at 200 items, including
at the original planning-count review base. Its pagination contract and exact
production source hash are recorded separately from the historical harness
configuration. Each initial, filter, reset and eligible load-more proof retains
the actual method, path, workspace, filters, offset, limit, ordered response IDs
and `has_more`; logical membership remains separate from physical DOM budgets.
The small 60-observation fixture proves a complete first page, no load-more
button, filter membership of one and reset membership of 60. Its load-more
receipt is explicitly inapplicable and contains no invented timing or no-op
request count. Large loads 200 then exercises offset 200/limit 200 to reach 400,
filters to one and resets to 200. All five large samples require real exercised
timings and requests, retaining the two-request and comparable 500 ms p95 limits.
Mixed eligibility, missing numeric evidence or incomplete terminal proof fails.
Current record/check timing compares the same 200-item phase; retained 50-item
pagination timings are explicitly not phase-comparable. Historical records,
budgets and fixture generators remain unchanged.

The five output kinds are `after`, `trace-manifest`, `validation-record`,
`evidence-manifest`, and `complete.diff`, named
`final-working-tree.*.<profile>.v1` (the diff ends in `.diff`).
Observation outputs live under the OS temporary directory in
`hmem-observation-scaling-<task>` or `hmem-observation-rendering-<task>`.
The frontend owner retains these five reproducibility artifacts through project
closure plus 30 days, then disposes of them; each file is bounded to 64 MiB.
Expanded hierarchy writes its separate artifacts in this directory and keeps
the baseline, budgets, historical complete diff, previous after records, and
failed-record archives unchanged. Preserve existing versioned evidence.

Planning-count outputs use the single OS temporary directory
`hmem-planning-counts-b401c358-1a68-404e-bd18-86f40bb3b771`, owned by that adapter
task. Only its five declared reproducibility artifacts and temporary trace are
admitted, with a 64 MiB limit per file. The task owner retains the five artifacts
through project closure plus 30 days, then disposes of them. Targeted unresolved
failure diagnostics expire on resolution or task closure. No planning-count
qualification writes historical evidence in this directory.

### Loading-efficiency qualification

Use the ordinary fixtures, interactions, measurement phases and unchanged budgets:

```powershell
$env:HMEM_EVIDENCE_BASE_COMMIT = 'dc44350bcb8360a7979d82f33271ad8dcdcb9b83'
npm run perf:self-check
npm run perf:record-after
npm run perf:check
```

The storage-only `loading-efficiency.v1` profile belongs to final task
`0a677d44-065a-4205-af9b-6e1d9b36a833`, project
`d2ba5afb-4f51-4328-aa1a-fcb95510f495`. It enables no Observation scaling,
rendering, planning-counts or due-removal scenarios. The review base remains
separate from the committed implementation revision recorded with exact input
and production-asset hashes. Historical baselines and mappings remain unchanged.

Its five declared artifacts live outside the repository under the verified OS
temporary directory `hmem-loading-efficiency-0a677d44-065a-4205-af9b-6e1d9b36a833`.
The final-verification owner retains them through project closure plus 30 days,
then disposes of them. Each file is limited to 64 MiB. Targeted unresolved failure
diagnostics expire at resolution or task closure. The sole temporary archive,
`temporary/large-loading-efficiency-trace.zip`, uses the existing bounded capture,
double-hash verification and resource retirement before successful qualification.
Redirected roots, output paths and foreign task/project/base/source identities fail.

Navigation fixtures accept the optional `include_projects` and `include_tasks`
booleans with legacy default `true`. A skipped kind returns an empty page without
selecting or decorating its rows; receipt proofs leave that kind's existing state
untouched. Request and response-byte accounting retain these actual wire payloads.
Readiness batches reuse the existing bounded summaries route. These browser
measurements establish frontend/transport behavior, not database or network latency.

## Measurement boundaries and evidence

### Due-removal qualification

After the matched source, assets and documentation freeze, the final-verification
owner runs:

```powershell
$env:HMEM_EVIDENCE_BASE_COMMIT = '33abd80dd13dbc6c850397eeb13f79047d0f7c91'
npm run perf:self-check
npm run perf:record-after
npm run perf:check
```

The `due-removal.v1` profile belongs to final task
`1e420484-81b9-4251-b485-82abca34c800` in project
`9f1a1a40-afc5-4825-851c-b71aaf498b58`. Its immutable review base is separate
from the current implementation commit recorded in source provenance and input
fingerprints. Baseline bytes must equal those at that review base. It uses the
ordinary navigation fixtures and existing measurement phases, runtime comparison
rules and all unchanged budgets; it does not enable Observation scaling.
Current generator goldens omit due payloads. A legacy projection in
`due-removal.test.mjs` reproduces the original exact hashes, proving that all
other fixture bytes remain unchanged. Historical profiles and evidence stay intact.

The existing record-after argument spellings map to the five isolated files
`final-working-tree.{after,trace-manifest,validation-record,evidence-manifest}.due-removal.v1.json`
and `final-working-tree.complete.due-removal.v1.diff` in the verified OS temporary
directory `hmem-due-removal-1e420484-81b9-4251-b485-82abca34c800`. Check reads
those same files. Baseline recording, arbitrary redirects, foreign owner/base/
revision and unadmitted or redirected roots fail without historical-output writes.
Each retained file is bounded to 64 MiB. The parent final-verification owner
retains them through project closure plus 30 days, then disposes of them;
unresolved targeted diagnostics expire at resolution or task closure.

The only transient archive is `temporary/large-due-removal-trace.zip`, bounded to
64 MiB and captured during the complete final large sample with snapshots on,
screenshots/source capture off. Size and double-hash verification, owned archive
retirement, browser/Chromium/server retirement, and record/check evaluations all
precede qualification success. The manifest truthfully marks the archive
unavailable after retirement. Local routing/self-check tests do not constitute
fresh measurement or qualification; final verification owns record and check.

The [harness](harness.mjs), [contracts](contracts.mjs),
[fixtures](fixtures.mjs), and [Observation overlay](observation-scaling.mjs)
define exact phases, route fidelity, ordering, and assertions. Read their
self-checks when changing a measured behavior.

Expanded-hierarchy cold readiness ends at the authorized
`workspace_shell_v1` shell and first visible root-summary anchor after two
paints. Current response identities and browser timing must match; missing,
duplicate, retired, unmatched, or boundary-uncertain timing fails closed, using
the existing 2 ms precision guard. Request/body-byte accounting ends after that
evaluation returns to the host and includes responses held by asynchronous gates.
Browser and host timestamps remain separate. Passive diagnostics retain bounded
first/decisive/latest evaluations and omission counts without adding waits or
changing the measured cut.

Background completion verifies exact membership/order, terminal pagination for
every effectively expanded branch, physical request bounds, and scroll
reachability. Cold/live and subsequent interactions retain DOM, row, observer,
and request high-water marks. Current root/filter and expanded-descendant passes
must finish before live settlement's two paints. The separate 500 ms stability
window is excluded from the settle timer and rejects in-flight or late traffic.
Collapsed branches stay lazy; partial or paused streams cannot prove completion.

Navigation batches count one actual HTTP request and its complete encoded body
once. Linked logical owner receipts retain independent page authority, cursors,
filters and terminal coverage; they contribute no additional HTTP or byte counts.
Malformed complete outcome sets cannot establish any owner authority. The live
followup ceiling and 500 ms post-settle guard apply without adjustment.

Settled expanded-navigation qualification rejects branch feedback rows. Root lists
remain driven by explicit demand: an enabled project/task root Load-more control is
valid when its current admitted, fulfilled 50-item wire pages prove the exact ordered
prefix and independent remaining membership. Loading, errors, Retry, disabled or
uncorroborated controls fail. Scrolling verifies admitted members without clicking
root pagers; physical row/observer and transport limits remain unchanged. This
semantic feedback contract is fingerprinted separately for current qualification.

Current live hydration qualification retains the historical route-only reload
Boolean and reports a separate receipt classification. A single Observation
first-page refresh qualifies only with the same-run fulfilled unfiltered reset,
its executed exact 200-member guard, unchanged intervening context, and one
fulfilled offset-0/limit-200 response whose unique ordered IDs match both the
validated reset and independent fixture prefix, with more members remaining.
Complete successful request/byte accounting is mandatory; every request remains
in the unchanged live budgets. Session retirement, enumeration, extra pages,
restarts, filtered or unproven traffic fails. Retained inactive membership is
inferred from reset proof, source and the executed recipe; offscreen model state
is not directly observed. This current contract/source proof is separately hashed;
historical records and their route-only results are preserved.

Individual Observation detail refreshes qualify separately from list enumeration:
each distinct successful GET must target an ID updated by the dispatched fixture
frames and present in the same-run validated reset cache. Response ID/workspace,
complete DTO hash and byte size must match the independent fixture response.
Duplicate, unreferenced, foreign, missing, failed, or whole-cache detail walks fail.
The current small case records 16 individual receipts within all 18 live requests;
the existing 12-request large-only budget gains no small-scale cap.

Historical full snapshots contain 155 small or 4,951 large items, transported in
two or 50 pages. Terminal-page readiness and canonical byte/order equality are
required for that phase. Their cold timing is not phase-comparable with the
current one-item workspace shell and painted-root cut. Direct focus starts
with an empty model and paused canonical resync, observes the last root project
(small 9, large 249) for 750 ms, then verifies the untouched complete resync.

The clock is fixed at `2026-08-30T12:00:00Z`. Current Timeline qualification uses
the source-anchored 30-day default, August 1 through August 31 exclusive, with five
weekly buckets. Exact fulfilled event-list (limit 50) and histogram request/DTO
receipts prove the ordered event cards and all five enabled lifecycle SVG series.
Each point's accessible count, half-open range, title and roving keyboard position
must match the canonical buckets; zero activity remains valid. Physical collection
and lifetime high-water metrics count these point controls and retain the legacy
table selector under unchanged budgets. The current contract and its production
source hashes are fingerprinted separately from historical configuration.

Historical Timeline fixtures retain the June 1 through August 31 range and its
13 weekly rows from unchanged 20/500 backing rows. Their table workload is not
timing-comparable to the current 30-day SVG workload. Search ranking and UTC bucket aggregation are
deterministic approximations, with production route ordering and caps checked
by self-tests. Historical path-match profiles use empty valid results;
Observation profiles use the declared nonempty matcher.

Observation scaling retains schema-2 DTOs, deterministic multiline content,
one exact 512 KiB value, ordered overlapping file/glob subjects, and loads of
50/100/150 large or 60 terminal small observations. Repeated mounted cards count
toward unchanged DOM/card ceilings. Match load-more retains the two-request and
comparable 500 ms p95 limits; initial application/detail batching is excluded.
Disclosure/editor/input paint retains the comparable 100 ms limit without waiting
for HTTP. Active-editor heap/Observation-only live follow-ups at 64 MiB/12 requests
are research triggers requiring evidence review, not additional acceptance gates.
Rendering scroll checks prove complete cached membership outside timed intervals.

Complete Git evidence collection is bounded to 32 MiB per command, 128 MiB total,
30 s per command, and 60 s overall. Required untracked source and failed archives
remain in the complete diff. No-index exit 1 is allowed only with intact bounded
output; overflow, timeout, signals, or command errors fail qualification.
Evidence replacement is atomic and invalidates prior success before writing.
Partial/failed writes retain a failed receipt, or remove stale success if failure
persistence also fails. Rejected physical parent/root ownership stops persistence
without fallback writes. These bounds change neither budgets nor accounting cuts.

Successful finalization requires bounded browser, owned Chromium, server, and
trace retirement, including callable `close` callbacks. Failed retirement
attempts remaining cleanup and records failed qualification; informational timing
PASS cannot override record, prerequisite, or retirement failure.

Observation traces use `temporary/large-observation-trace.zip`; expanded hierarchy
uses task-owned `.scratch/expanded-navigation-perf/`. Verify/hash/size and retire
the owned transient archive before success; manifests state it is unavailable.
Observation capture spans the full final large sample with DOM/network snapshots,
source capture disabled, and optional screenshot previews disabled. Exact options
are fingerprinted. Double hashing and the 64 MiB trace limit remain required;
omitting previews does not guarantee fit.

Planning counts uses `temporary/large-planning-counts-trace.zip` in its verified
task directory, with the same full final large-sample interval, DOM/network
snapshots and disabled source/screenshots. Capture options and the planning-count
response contract join its input fingerprints. The owned trace must pass size
and double-hash verification and be removed before qualified success; only its
manifest remains available.

Preserve existing failed trace diagnostics until a fresh full-interval archive
passes size/double-hash verification and owned retirement. Failures report bounded
phase/size/options while leaving unresolved diagnostics intact. Resolution replaces
them with current evidence; no extra journal is created. Only bounded targeted
unresolved diagnostics remain until resolution. Every write/removal revalidates
the admitted contained parent/root lifetime.

## Historical authority and limitations

For the scoped due-removal causal diagnostic, after its reviewed commit and with
the matched production build, run once from this directory with
`HMEM_EVIDENCE_BASE_COMMIT=33abd80dd13dbc6c850397eeb13f79047d0f7c91 node perf/diagnose-live.mjs`
(PowerShell: set `$env:HMEM_EVIDENCE_BASE_COMMIT` before the command). It executes
one unchanged large scenario without tracing or qualification and refuses to
overwrite `live-causal-diagnostic.json` in the separately verified final-task
failure archive. Passive host boundaries retain at most 32 request summaries and
first/decisive/latest readiness observations; no DTO bodies or additional browser
polls are collected. Host and browser clocks are distinct. Readiness observations
bracket existing evaluations and cannot by themselves separate decoding, Elm
updates/rendering and polling. The receipt is limited to 256 KiB, owned by final
task `1e420484-81b9-4251-b485-82abca34c800`, and expires at issue resolution or
task/project closure. It does not replace the enforced record/check budgets.

[baseline.v1.json](baseline.v1.json) records commit
`ca04f51c3494c83c433d12bd7791f89be876daea`, environment, scales, raw samples,
route evidence, evaluations, and trace metadata. Its final trace was generated
under ignored `perf/.artifacts/large-baseline-trace.zip`, read and hashed twice,
then removed. [trace-manifest.v1.json](trace-manifest.v1.json) retains its digest,
size, capture point, and retention policy; the archive is not available.

The harness hashes the frontend README along with source, contracts, and assets.
Historical receipts qualify only their recorded inputs and phase. Editing the
README does not refresh qualification; do not replace baselines or receipts to
make a documentation change pass. Earlier browser results retain their original
authority and limitations.

These fixtures measure frontend scaling and deterministic transport amplification,
not database, real-network, native-zoom, assistive-technology, or human-usability
performance. Historical hotspot ownership remains
`bd2eba73-6e33-4bc1-8a45-3b024c662343` for API/bootstrap amplification and
`2503e08f-ff82-4f2c-adff-24e14fec8299` for Elm rendering/live scaling.

For a bounded visual smoke, build first, run `node perf/serve.mjs large`, and open
the printed URL in a named Playwright CLI session. The deterministic server does
not modify production assets. This smoke does not replace qualification.
