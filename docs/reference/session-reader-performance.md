# Session reader qualification

The reader now admits useful older history independently of retained overlap,
publishes complete live records during serialized backfill, and coalesces native
scroll/resize work into one owned animation frame. Follow notifications include
changes to the source reading anchor as well as follow mode. These mechanisms
preserve historical reading intent while keeping Elm's retained source window
current. A mode-only notification cache would lose meaningful anchor updates.

The reproducible frontend qualification lives in
`hmem-server/frontend/perf/session-reader-history.mjs`; its bounded receipt is
`session-reader-history.qualification.v1.json` beside the runner. Run from
`hmem-server/frontend` after the supported `npm test` build:

```text
node perf/session-reader-history.mjs --revision <Git-MCP-verified-40-character-implementation-revision> --self-check
node perf/session-reader-history.mjs --revision <same-revision>
```

`--only <context>-clean` or `--only <context>-instrumented` runs one diagnostic
cell without overwriting the canonical receipt. The contexts are
`active-workspace`, `active-task`, `sealed-workspace`, and `sealed-task`.
The final receipt requires all eight cells. Source, harness, workload, compiled
main/worker, supporting asset, browser executable, Node, OS, CPU, viewport and
Playwright identities bind measurements to their actual inputs. Instrumented
asset hashes and exact binding-span hashes are separate from clean asset hashes.
The supplied revision identifies the measured implementation; a later evidence
commit does not retroactively change it. The runner rechecks input freshness
before writing its receipt.

## Actual surfaces and workload

| Lifecycle and scope | Measured surface | Entry |
| --- | --- | --- |
| Active workspace | Sessions | Canonical workspace Sessions route |
| Active task | Embedded task dock | Projects task Active Agents button |
| Sealed workspace | Sessions | Canonical workspace Sessions route |
| Sealed task-linked | Full Sessions, filtered to the task | Canonical task-filtered Sessions route |

The embedded dock is active-only. Terminated metadata retires that viewer;
`tests-js/session-dock.browser.test.js` covers that contract. A sealed embedded
reader is not a supported cell. The sealed task measurement starts at the
canonical filtered destination; clicking Evidence to reach it is not measured.

The deterministic 2,866,428-byte UTF-8 JSONL workload contains 1,082 records:
mixed-size prose, 900 distinct adjacent tool calls, a separating short prose
anchor, and a 196,099-byte final record split across 64 KiB transfers. Known byte
offsets and the workload SHA-256 are recorded without retaining a fixture copy.
Each cell uses a fresh nonpersistent Chromium context. Archive cells verify a
gzip manifest and content hashes through the production worker/index path.

The workflow holds a later live recovery response while checking that earlier
complete rows are already visible. It then exercises native scroll/history,
width changes, the exact 900-contribution activity summary and disclosure,
logical latest, and active appends while following and manually paused. Samples
assert ordered unique occurrence offsets, at most 200 mounted rows, at most 201
source gaps, and at most 1 MiB of presentation cost. Live transfer limits stay
at 64 KiB and the fixture's source-request concurrency stays at most two.
Instrumented publication samples additionally check the 4 MiB source retention
budget. These are sampled reader budgets, not a whole-process heap measurement.

## What the phase numbers mean

Clean cells execute the compiled production assets. Light RAF/long-task sensors,
automation observations, and CDP tracing still impose measurement overhead.
Separate instrumented cells add numeric timers at unique production bindings;
zero or multiple matches fail qualification rather than guessing a binding.

| Receipt metric | Actual interval and limits |
| --- | --- |
| Request round trip | Automation-host request to request-finished/failed; synthetic local HTTP fixture, including deliberate held response. Endpoint counts distinguish metadata, transcript, manifest and compressed content. |
| Bridge command entry | Synchronous invocation of the real subscribed callback, including nested synchronous parse/framing. Returning a Promise ends this entry interval. |
| Bridge command lifetime | For Promise-returning callbacks, settlement elapsed including awaits, worker/IDB waits and other asynchronous delays. It is not CPU time. The original returned Promise and rejection behavior are preserved. |
| Raw parse | Only the guarded `JSON.parse(raw)` of complete JSONL records, in active framer or sealed processor worker. Elapsed is milliseconds; the `units` size counter counts UTF-16 code units. It excludes UTF-8 decode, byte framing, semantic classification and storage. |
| Elm update | The actual `A2(update, message, model)` call inside platform dispatch; excludes stepper, draw and effect enqueue. |
| Elm draw entry | The animator's actual draw argument, including view construction, VDOM diff and DOM patch. It can include synchronous browser style/layout. |
| CDP Script/Task/Layout/RecalcStyle | Browser metric deltas over the reader workflow, with native CPU categories and counts. Duration units are seconds. |
| Paint/PrePaint trace | Complete timeline-event duration sums in milliseconds, processed in memory and discarded. Counts identify whether the browser supplied events. |
| RAF/long tasks | Observed frame interval maximum and intervals over 50 ms; main-thread long-task count/duration. These do not measure display presentation latency. |
| Input to two frames | Automation-host wheel dispatch through two animation-frame callbacks. Includes protocol/scheduling delay; it is neither paint CPU nor INP. |

Bridge entry includes nested parsing; draw may include work also reported as
browser layout. Worker parse intervals use worker-local clocks. Never add these
overlapping totals into exclusive CPU or elapsed time. The instrumented worker
reports only cumulative numeric aggregates attached to existing responses; no
new product message kind or raw transcript telemetry is introduced. Per-worker
snapshots survive processor detach/termination and are bounded to 16 workers.

## Interpretation and validation

The retained run completed all eight cells. Its clean workflow elapsed times
were 8.01 s for active workspace, 9.33 s for active task dock, 3.61 s for sealed
workspace, and 3.65 s for sealed task-linked Sessions. These totals include the
deliberate response hold, automation and quiet waits; they are not interaction
latency budgets. Sampled maxima across cells were 120 mounted rows, three source
gaps and 1,026,936 presentation-cost bytes. Source request concurrency peaked at
two and each live physical range remained at most 65,536 bytes.

| Instrumented context | JSONL raw parse | Actual Elm update | Animator draw |
| --- | ---: | ---: | ---: |
| Active workspace | 7.4 ms | 811.9 ms | 1,906.4 ms |
| Active task dock | 9.8 ms | 973.3 ms | 3,173.6 ms |
| Sealed workspace | 3.7 ms, worker | 111.1 ms | 42.7 ms |
| Sealed task-linked Sessions | 3.4 ms, worker | 128.3 ms | 59.5 ms |

Both sealed processor snapshots counted all 1,082 raw records before detach.
Within these local workflows, actual Elm update/draw elapsed exceeded guarded
raw JSON parse elapsed. This supports investigating retained-window/model work
and presentation before attributing the observed active workflow cost to
`JSON.parse`. It does not compare equivalent active/sealed algorithms: active
cells performed 91–106 physical range requests and repeated composition;
sealed cells used verified compressed content and the worker-backed index.
The receipt retains request, bridge, CPU/style/layout, paint and frame facts
needed to interpret that difference. These overlapping intervals are not summed.

The receipt establishes observed local workflow completion, useful intermediate
progress and supported reader bounds on its recorded machine. Compare phase
counts and elapsed intervals with the recorded inputs before attributing a
latency change. Synthetic request timing is not production backend latency;
worker JSON parse is not total archive verification/indexing time; a two-frame
proxy is not a paint-time measurement. No universal machine-independent latency
threshold or cross-browser guarantee is introduced. Workflow/cleanup deadlines
are finite correctness guards.

The configured `npm test` passed against implementation
`052cccba3775d95ec1fa5da987b5598a8d90bf96`, including the production build used
by the receipt. Earlier repair gates separately covered 119 Elm tests, bridge
and archive units, stale generation/chunk/ACK fences, omitted-middle hydration,
occurrence pairing and manual reading anchors. The geometry repair's 100-event
production regression measured zero synchronous subtree/record geometry scans
and one bounded frame: active 11 queries/264 element rectangles/9 text ranges;
sealed 10/137/8. Cheap scalar intent reads remain synchronous. Its row-based
geometry assertion applies to its fixed flat 24-row fixture, not arbitrary rich
message content. The final runner measures the representative mixed workload
and retains its own observed counters and durations separately.

The project maintainer owns the bounded qualification JSON as release evidence
until explicitly superseded. It is capped at 128 KiB. Raw traces, screenshots,
transcripts, browser profiles and per-event samples are not retained. Task-owned
scratch runners are removed at closure; preexisting output and worktrees remain.
