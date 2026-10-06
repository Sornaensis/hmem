# Large-workspace browser baseline

This directory is test-only. `fixtures.mjs` generates byte-stable, schema-valid
small and large workspaces from `hmem-large-workspace-v1`. `harness.mjs` serves
the optimized Vite output, runs the real Elm `Main` program in the repository's
locked Chromium, and intercepts every API/change-stream response with fixture
data. No backend, database, or external network is used.

## Commands and result policy

```sh
npm run perf:self-check
npm run perf:record
npm run perf:check
```

`npm run perf:record` is the explicit authorization to replace the approved
baseline (direct record invocation additionally requires
`--authorize-baseline`). It always preserves the actual PASS/FAIL evaluation in
`baseline.v1.json` and exits zero when only target budgets fail; harness,
selector, schema, build, and runtime errors still fail. Record is therefore not
a way to silently normalize a product violation. Task-qualified check mode writes
its validation/evidence receipts; it preserves the recorded measurement and the
historical baseline. It
verifies the complete budget, harness-configuration, and small/large fixture
hashes against both the baseline and trace manifest before opening a browser.
The frozen provenance also hashes the direct-focus contract and each canonical
ordered snapshot array.

Both modes use two warmups followed by five samples at each scale. Every named
interaction retains its own `{ count, bytes, routes, routeBytes }` request
delta alongside its five-sample latency series and nearest-rank p95 (for five
samples, the maximum); the aggregate retains the same per-sample route evidence.
The 50-frame live batch retains and aggregates that exact request delta too.
Unrelated interactions are not pooled. Check emits one
PASS/FAIL line per scenario and metric and exits nonzero if any enforced budget
fails. Required selectors and expected row/card effects fail hard. Structural
request, byte, DOM, row, reload (including change-stream resync), duplicate-
request, and console invariants are enforced on every machine. Timing and
retained heap are enforced only when the full environment fingerprint matches
the checked baseline; otherwise they remain visible but informational.

`baseline.v1.json` is bound to commit
`ca04f51c3494c83c433d12bd7791f89be876daea`. It includes UTC time, OS/CPU/RAM,
Node/npm/Elm/Vite/Playwright/Chromium versions, fixture/configuration/budget
hashes and exact scale, every raw measured run, aggregates, route counts/bytes,
budget results, and verified large-trace metadata. Version probes are required
and use platform-native executable names on Windows and POSIX. Comparable
timing/heap enforcement requires the exact recorded OS, CPU, RAM, tool, browser,
viewport, and headless fingerprint.

The final large record trace is generated under ignored
`perf/.artifacts/large-baseline-trace.zip`; record reads and hashes it twice and
stores its digest, byte size, capture point, and retention policy in the
versioned `trace-manifest.v1.json`. The opaque archive is intentionally removed
after verification and is not claimed to remain available.

The current `POST /api/v1/change-stream/resync` fixture follows production
semantics exactly: it transports all 155 small snapshot items in two 100-item
pages and all 4,951 large items in 50 pages. Readiness before the terminal page,
or a synthetic bounded response on that route, is a hard harness failure. Paged
list/entity handlers still expose the same complete backing fixtures. Protocol-
agnostic readiness can describe a future bounded route, but that branch remains
dormant until production actually requests such a protocol.

Snapshot materialization independently sorts every workspace, project, task,
dependency, and Observation item by the production kind rank and wire identity;
dependency identity is the `task_id:depends_on_id` pair. Self-checks freeze the
first, last, and every kind boundary, and prove reversed generator arrays yield
the same canonical sequence. Project and Task list fixtures use the production
priority/name and priority/created ordering, public pagination defaults to 50
and caps at 200, overview lists use the production query order, and entity DTOs
include the same metadata/dependency fields as their real route.

Observation list and facet handlers model PostgreSQL `simple`
`plainto_tsquery` as tokenized AND search, apply an exact subject-kind/subject
pair to the same subject row, and use production rank, recency, ID, count, and
facet tie-break ordering. The measured query
`Observation 00001 evidence` intentionally matches exactly Observation 1;
route tests freeze its ID and page boundaries. Timeline events include only
production project/task audit events and are newest-first by `occurred_at` then
audit ID. The fixture retains exactly 20/500 deterministic hourly bucket-source
rows, but the measured bucket route aggregates only the requested inclusive-
`since`/exclusive-`until` range into PostgreSQL-compatible UTC day, Monday-week,
month, or quarter boundaries. It returns ascending full bucket intervals,
recomputes component totals, and applies the production 367-row SQL sentinel /
366-row response limit. Invalid or oversized requests fail explicitly.

The browser clock is fixed at `2026-08-30T12:00:00Z`, so the real Elm default
range is always `2026-06-01T00:00:00Z` through
`2026-08-31T00:00:00Z` exclusive. Its weekly query returns 13 rows, from the
week beginning June 1 through the week ending August 31. Readiness asserts this
exact request and row count; response bytes and Timeline DOM measurements
therefore account for the production-shaped response rather than all 500
backing rows.

Direct focus runs in a fresh context whose URL names the focus before bootstrap.
The unchanged canonical resync array is paused before its first response, so the
measured model starts empty. The selected target is the last deterministic root
project—small project 9 and large project 249—so a direct entity response is
hierarchy-coherent without parent prehydration. During a 750 ms observation
window the harness records whether the focus path requests and renders the
target, exact route deltas, and a stable failure reason. False booleans are
product/budget failures that record mode preserves; malformed transport remains
a harness error. It then releases the untouched array and verifies the complete
155/4,951-item resync. Tests freeze byte/order equality and its SHA-256, not only
set membership. No WebSocket invalidation prehydrates the target.

Before the live scenario the harness restores an expanded, unfiltered,
unfocused Projects workspace. Readiness proves the exact full current resync is
complete, then requires deterministic representative anchors. It never requires
all backing entities in the DOM. Actual DOM nodes and every rendered
project/task/Observation/timeline row remain measured and budget-gated.

The 50-frame live settle timer ends after the browser dispatch turn, completion
of every follow-up that actually started, transport idle, a representative
Projects anchor, and two UI paints. Zero follow-up requests is valid. A
separate, excluded 500 ms stability window hard-fails any in-flight or late
request. Post-live readiness uses the same full-model and anchor signals.
Retained heap is captured at that representative state after stability and
explicit Chromium garbage collection.

For a bounded visual smoke, build first, run `node perf/serve.mjs large`, then
open the printed URL with a named Playwright CLI session. The server injects the
same deterministic canonical transport without modifying the production build.

## Hotspot ownership

- `bd2eba73-6e33-4bc1-8a45-3b024c662343` owns API/bootstrap amplification.
  `src/Feature/DataLoading.elm` around lines 297 and 349 recursively consumes
  200-row project/task pages and then fans out one overview request per loaded
  entity. Canonical direct-route bootstrap currently replaces that legacy path
  with a canonical full snapshot. The harness counts every one of the large
  resync's 50 pages and 4,951 items, while retaining lazy list/entity handlers
  for actual requests. This baseline changes no production path.
- `2503e08f-ff82-4f2c-adff-24e14fec8299` owns Elm render/update/live scaling.
  Projects, Observations, and Timeline record DOM/row maxima and tab/filter/
  expand/direct-focus/load-more latency. The 50-frame canonical burst mixes
  project/task/Observation targets, repeats targets, and records coalescing,
  whole-workspace refreshes, per-route bytes, and settle time.

The mocked transport deliberately measures frontend scaling and deterministic
response amplification, not database/query or real-network latency. Budgets in
`budgets.v1.json` must not be weakened without an explicit project decision.
Search ranking is a deterministic fixture approximation of PostgreSQL
`ts_rank`; the intentionally single-result measured query avoids rank ambiguity.
Bucket counts are aggregated in JavaScript from deterministic hourly source
rows rather than by executing PostgreSQL, while matching its requested range,
calendar boundaries, ordering, and cap. Historical profiles return an empty
valid path-match page. The Observation profile below uses a nonempty deterministic
matcher and measures that workflow. These test-transport
simplifications exclude database and network latency.

## Observation scaling profile

Set `HMEM_EVIDENCE_BASE_COMMIT=c5230077a643d06e7ca5a4ff6b5616cfb90862ec`
for task `b9f1de65-f016-4178-89a0-ac13a5d7fff8`, then run
`npm run perf:self-check`, `npm run perf:record-after`, and `npm run perf:check`.
Record/check require an explicit mapped base; unknown revisions fail before
measurement. The after-record script's historical argument spelling is mapped
to this profile's fresh paths; arbitrary output redirection is rejected. Baseline
recording is unavailable under this task profile. Review the runner before
measurement, and qualify success only after check passes on matching inputs.

The five retained artifacts are under the operating-system temporary directory,
in `hmem-observation-scaling-b9f1de65-f016-4178-89a0-ac13a5d7fff8`:
`final-working-tree.after.observation-scaling.v1.json`,
`final-working-tree.trace-manifest.observation-scaling.v1.json`,
`final-working-tree.validation-record.observation-scaling.v1.json`,
`final-working-tree.evidence-manifest.observation-scaling.v1.json`, and
`final-working-tree.complete.observation-scaling.v1.diff`. The frontend owner
retains these reproducibility artifacts until project closure plus 30 days,
then disposes of them. Each retained file is bounded to 64 MiB. Temporary trace
`temporary/large-observation-trace.zip` is retired before successful finalization;
its verified digest/size remain in the manifest. Only bounded unresolved failure
diagnostics remain until resolution. Historical artifacts and budget bytes remain
unchanged. Check validates task/base/revision, complete source/assets/contracts,
recorded evaluation, and retirement before issuing qualified success.
This profile captures the complete final large-sample interval with DOM/network
snapshots enabled, source capture disabled, and optional screenshot timeline
previews disabled. Historical/default capture options remain unchanged. Exact
options are fingerprinted by the scaling contract and recorded in trace metadata;
the actual archive is still double-hashed, sized, and retired. A future size
rejection reports the exact actual bytes, 64 MiB limit and capture options in
bounded failure metadata before removal. Omitting previews is a storage strategy,
not a guarantee that the archive fits or a change to any performance budget.
Existing failed trace diagnostics survive startup. Until a fresh full-interval
archive passes size/double-hash verification and owned retirement, a subsequent
failure leaves those files intact and reports its bounded phase/size/options
through the tool output. After verification/retirement resolves that issue,
normal current evidence replaces the old diagnostics. Prior success qualification
is still invalidated at startup; no additional diagnostic files or journal are
created.
Every evidence write/removal, including atomic rename/temporary cleanup and
failure-receipt persistence, revalidates the admitted physical parent/root
lifetime and contained paths. Rejected ownership stops persistence; the command
reports the failure without writing fallback receipts into redirected storage.
Check exits nonzero whenever the complete record/check/prerequisite/retirement
qualification fails, even if the latest check's informational timing passes.

The schema-2 overlay keeps the original generator intact and adds deterministic
multiline evidence, one exact 512 KiB value, and ordered overlapping file/glob
subjects for two concrete paths. Its matcher reuses list ranking/text/SHA filters,
preserves caller-path and matched-subject order, and supports exactly the declared
fixture glob. Large samples load 50, 100, then 150 observations; small samples
terminate at 60. All subject groups expand, counting repeated mounted cards
without ID deduplication. The native reader exposes the boundary value, editing
preserves its draft through 50 real-shaped mocked canonical invalidations, and
the original Projects heap/mixed-live phases still run separately.

Existing unconditional DOM (2500) and collection-card (250) ceilings cover every
new stage and lifetime high-water. Match load-more has the same complete-settle
endpoint as flat load-more: at most two requests unconditionally, and nearest-rank
p95 at most 500 ms only with complete environment comparability. Initial Match
application/detail batching is excluded from that phase. Disclosure/editor/input
paint records activation through two animation frames without waiting for HTTP;
the existing comparable-environment 100 ms local ceiling applies. Two warmups and
five measured samples remain unchanged. Active-editor heap and Observation-only
live follow-ups are separately reported research triggers at 64 MiB/12 requests;
they require evidence review rather than inventing additional acceptance gates.
Record exiting zero alone does not establish budget compliance. These deterministic
transport measurements make no database, network, native zoom, assistive-technology,
or human-usability claim.
