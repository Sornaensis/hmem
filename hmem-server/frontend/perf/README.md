# Browser performance qualification

For frontend maintainers. Run from `hmem-server/frontend` after installing the
locked Chromium as described in the [development guide](../README.md).
The harness runs production Elm with deterministic intercepted HTTP/WebSocket
fixtures, without a backend, database, or external network.

## Commands and result policy

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

## Measurement boundaries and evidence

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

Historical full snapshots contain 155 small or 4,951 large items, transported in
two or 50 pages. Terminal-page readiness and canonical byte/order equality are
required for that phase. Their cold timing is not phase-comparable with the
current one-item workspace shell and painted-root cut. Direct focus starts
with an empty model and paused canonical resync, observes the last root project
(small 9, large 249) for 750 ms, then verifies the untouched complete resync.

The clock is fixed at `2026-08-30T12:00:00Z`. Timeline's default range is
June 1 through August 31 exclusive and returns 13 weekly rows from the unchanged
20/500 backing source rows. Search ranking and UTC bucket aggregation are
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

Preserve existing failed trace diagnostics until a fresh full-interval archive
passes size/double-hash verification and owned retirement. Failures report bounded
phase/size/options while leaving unresolved diagnostics intact. Resolution replaces
them with current evidence; no extra journal is created. Only bounded targeted
unresolved diagnostics remain until resolution. Every write/removal revalidates
the admitted contained parent/root lifetime.

## Historical authority and limitations

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
