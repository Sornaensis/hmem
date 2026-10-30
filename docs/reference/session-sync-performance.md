# Session synchronization performance

The synchronization baseline and final qualification use the native Windows
server, isolated PostgreSQL, production MCP scheduling, production Elm assets
and three Chromium clients. They are separate from the integrated correctness
qualification in [agent-session-qualification.md](agent-session-qualification.md).
The retained compact measurements belong to repository maintainers and remain
until the corresponding before/after qualification is superseded. Generated
transcripts, database clusters, browser profiles and raw diagnostic logs are
task scratch and are removed after their processes have joined.

## Contract

Representative profiles cover 1 MiB, 100 MiB and 1 GiB committed prefixes,
one/four followers, ordinary MCP calls, small chunks, continuous output,
UTF-8/CRLF records, typical 1 KiB/64 KiB records and the 1 MiB record boundary.
The background history contains 53 terminated sessions so pagination crosses its
50-row boundary. The controlled path shares 100 Mbit/s **per direction** across
all HTTP and WebSocket connections and adds 50 ms round-trip latency. It is a
loopback transport emulator; it does not claim to reproduce packet loss or a
public Internet route.

The approved local p95 limits are source-to-commit 2 s, commit-to-visible 2 s,
catch-up progress 250 ms and local interaction 100 ms. The interaction target
preserves the stronger existing frontend gate. Initial/latest-window limits are
2 s local and 3 s on the controlled path. Control calls complete or report
truthful pending within 1 s. Resume, finite receipt completion and sealing are
bounded by twice the same-profile calibrated verified scan/transfer time plus
5 s. Prefix verification may use a bound cryptographic proof; it does not have
to transfer the server's complete prefix over HTTP.

Reference capacity uses fixed-duration stepped-rate trials with non-growing
backlog, constrained by source generation, authorized append/database work and
network capacity. The fixed workload is 70% of the measured stable reference
capacity. Reference calibration is distinct from production-follower timing.
Exact hardware/configuration, byte and record rates, chunk distribution,
resource ceilings and absolute 1 GiB finite-operation targets must be frozen
before optimization. Existing admission/window limits remain; working memory
must not scale with transcript size. Unrelated foreground/ordinary-MCP p95 may
regress by at most 10% against matching baseline inputs.

Baseline sampling includes at least 100 real source observations per live
profile and five warm/three fresh-process attempts of each finite operation.
Final qualification requires at least 100 independent commit, delivery and
render cohorts for the corresponding latency budget, and completed finite
operations, including actual 1 GiB cases. Multiple markers represented by one
displayed revision and three views of one source are correlated observations.
Baseline old-path cases may stop at a declared cap. A capped operation has no
completion percentile and its observed progress is not a completed result.
Cold-state reports must identify the processes and buffers actually reset;
uncontrolled OS filesystem caches are never described as cold disk.

## Measurement boundaries

Transcript occurrence time (`last_activity_at`) is content metadata, not a
performance measurement clock. Source-to-commit and transport freshness keep the
authorities below. Occurrence extraction handles one bounded append before its
evidence transaction; historical correction advances independent one-MiB turns.
See [occurrence semantics](session-occurrence.md). These correctness changes do
not claim a new performance qualification for the historical matrix.

- Source time is recorded at the actual source write and mapped to its byte
  boundary. PostgreSQL `pg_xact_commit_timestamp(session_chunks.xmin)` supplies
  the corresponding committed chunk's timestamp on the same host clock.
- Native transport observation preserves production delivery. A DOM/rAF-ready
  marker is not a paint timestamp. A successful discarded browser screenshot
  after the marker intersects the viewport and every ancestor scroll clip
  supplies a conservative rendered upper bound. The witness is checked before
  and after the raster; screenshot bytes are neither printed nor retained.
  One initial action places the selected transcript in view before source
  growth. Later markers are never forcibly scrolled into view.
- Commit/outbox transaction correlation and observed WebSocket egress provide
  a dispatch-plus-loopback upper bound. The wire observer discards checkpoint
  tokens and raw payloads. It does not claim an exact server-send timestamp.
- HTTP upstream-read-end and downstream-finish/close are separate phases.
  Frontend connections and the proxy's bounded keep-alive backend connections
  are different resources. Proxy connections do not establish application
  connection reuse.
- Database SQL execution counts describe logical query work. They are not
  physical TCP round trips. Lock waiters and active connections are sampled
  observations with the stated sampling interval, rather than exact cumulative
  wait duration.
- The optional `HMEM_TEST_SESSION_DIAGNOSTICS=1` setting applies only to a newly
  created native test cluster. It enables commit timestamps and I/O timing.
  Where the bundled library exists it preloads `pg_stat_statements`; otherwise
  statement logging suppresses parameter values. Raw logs stay inside the
  contained sandbox and are disposed at cleanup. Ordinary tests and production
  PostgreSQL settings are unchanged. Measurement overhead requires a paired
  diagnostics-on/off reference comparison.

## Commands

From the repository root:

```text
stack build hmem-mcp:exe:hmem-mcp hmem-server:exe:hmem-test-harness --fast
stack exec node -- --test hmem-server/frontend/perf/session-sync.test.mjs
stack exec node -- hmem-server/frontend/perf/session-sync.mjs probe --base-sha <exact-40-character-input-SHA>
stack exec node -- hmem-server/frontend/perf/session-sync.mjs repair --base-sha <exact-40-character-input-SHA> --output <absolute-perf-output-path>
stack exec node -- hmem-server/frontend/perf/session-sync.mjs resources --base-sha <exact-40-character-input-SHA> --output <absolute-perf-output-path>
```

The coordinator supplies the exact source revision; the runner also records
artifact hashes and the actual runtime fingerprint. The probe validates
measurement and cleanup paths. It does not establish a calibrated workload or
pass the performance qualification.

The `repair` baseline reuses each newly seeded large prefix across reference,
live and finite measurements within one sandbox. Producer pacing is independent
of screenshot and observation waits. Aggregate byte rate includes marker bytes
and is divided evenly among the admitted followers. Live output contains
1 KiB/64 KiB records written in 1 KiB filesystem chunks. A slow observer is
paused, restored and disconnected while ordinary MCP and the other browsers
continue; this does not claim saturation of the operating system send buffer.
For final sampling, `--independent-min 100 --max-live-ms 180000` continues
rate-matched output until independent cohorts and ordinary/input observations
are available or the finite maximum is reached. Insufficient cohorts remain an
explicit qualification failure.

Each finite ledger distinguishes five warm and three fresh-process attempts of
**each** flush and stop, plus separate small completed-receipt witnesses.
Fresh attempts replace the MCP process, follower verification/transport state
and browser process; native server/database buffers stay warm and OS cache is
uncontrolled. Every attempt records canonical N, frozen source boundary B,
pending bytes B−N, control reply, completion or censoring, receipt hash checks,
and independent sealing/readback. The 8 s old-path completion cap is distinct
from the strict 1 s control-response budget. Managing-MCP benchmark requests
have a bounded 60 s observer deadline for large independent seals; the shared
helper's ordinary default remains 15 s and the product control budget remains
1 s.

## Recorded baseline

The compact artifacts are in `hmem-server/frontend/perf/session-sync.*.json`.
`session-sync.profile.json` consolidates the frozen contract, actual profile
counts, rate/record distributions, targets, resource formulas and exact receipt
hashes for subsequent comparison.
Its corrected visibility accounting separates displayed, represented,
superseded and censored events. Original receipts retain their exact bytes and
hashes; their older `supersededIntermediateSequences` field means all events not
individually displayed, including censored events. For example, controlled
browser 0 has 6 displayed, 55 represented, 49 superseded and 45 censored events,
not 94 superseded events. The retained sequence arrays support this correction.
`session-sync.repaired-baseline.json` is the authoritative baseline after API
response disposal and cleanup corrections. The earlier `baseline` artifact's
capacity/resource receipt is historical; its retained response bodies make it
unsuitable for a bounded-memory claim. The repaired receipt retains its exact
loaded method and helper/native/asset hashes. Its textual pending-transfer rule
still quotes the historical 40.513 s value; the authoritative numeric
`absoluteOneGiBFiniteTargetMs` is 40,302.109 ms.
They record Windows 10.0.26200, a Core Ultra 9 285K with 24 logical CPUs and
68,207,595,520 bytes RAM, GHC 9.10.2, PostgreSQL 18.2 and Chromium 151.0.7922.34.
Artifact hashes are specific to each actual ephemeral origin/build.

| Committed prefix, no pending tail | Verified scan reference | Frozen finite target | Old production resume |
| --- | ---: | ---: | --- |
| 1 MiB | 32.058 ms | 5.064 s | completed in 23.060 s |
| 100 MiB | 1.657 s | 8.313 s | incomplete after 20.181 s |
| 1 GiB | 17.651 s | 40.302 s | incomplete after 20.096 s |

The scan reference includes a complete local SHA-256 scan and an independent
bounded database scan whose hash matches the local source. Each source size has
five same-process and three fresh-process scans; OS cache was uncontrolled.
Database scans were repeated warm scans. The large resume cases contain one
large primary and three independent 1 MiB peers, not four large prefixes.
Canonical committed length N equals frozen source boundary B in these cases;
pending bytes B−N are zero. For a nonempty tail, add its measured bounded transfer
reference on the actual wire profile before applying the 2× plus 5 s rule. The
40.302 s target is not a promise to upload an uncommitted 1 GiB backlog over the
controlled link.

The eight five-second stepped-rate trials contain 356 actual authorized HTTP
appends. The highest tested stable lower bound was 262,144 bytes/s for both
paths; it is not a measured peak capacity. The frozen 70% workload is
183,500.8 bytes/s. Calibration has zero browsers and must not be presented as a
combined three-client result. Background terminated history was 53 sessions;
active session counts grew from eight reference sessions to 9, 13 and 17 during
the successive resume cases. Only one/four of those sessions had followers.

The matching zero-browser calibration's sampled stable-process peak RSS was
943,767,552 bytes. Its ceiling is 1,246,818,304 bytes (25% plus 64 MiB headroom).
CPU and logical SQL ceilings are normalized for that exact workflow:
10.494 CPU seconds per completed reference GiB (25% headroom), and 49.031 SQL
calls per completed database MiB (10% headroom). Denominators and observer PID
scope are recorded in the artifact. These are neither three-browser RSS limits
nor totals comparable to a different final matrix. Short-lived helper CPU/peaks
were not fully captured. These values do not estimate browser screenshot
overhead. The paired diagnostics-on/off receipt identifies its own method and
eight actual 1 MiB authorized appends per arm. The disposal-corrected
`overhead-revalidated` comparison measured a +1.013% median difference. It
estimates diagnostic/resource observation cost, not browser-raster overhead.

The one-event three-browser probe initially left the transcript partly outside
the window. Its earlier screenshot-only timing is invalid for on-screen
acceptance and remains marked superseded. The clipped-witness run correctly
recorded no visible result. A documented initial viewing action, once before
source growth, placed the transcript in the viewport; subsequent native
auto-follow produced genuine conservative rendered bounds of 345–369 ms, with
two physical session HTTP admissions. No marker was scrolled into view after
the source update. This is one independent live event observed by three
browsers, not 100 samples.

The repaired combined baseline has three actual profiles. Each selected source
produced 100 events concurrently with 100 ordinary MCP calls and three browsers.
The controlled profiles also exercised a paused/restored/disconnected observer.

| Profile | Actual aggregate bytes/s | Unique commit/outbox events | Rendered revisions per browser | Represented source events per browser |
| --- | ---: | ---: | --- | --- |
| Local, one 1 MiB follower | 183,271 | 3 / 3 | 3 / 3 / 3 | 100 / 100 / 100 |
| Controlled, four 1 MiB followers | 183,460 | 11 / 11 | 6 / 6 / 5 | 55 / 55 / 55 |
| Controlled, 1 GiB + 100 MiB + two 1 MiB followers | 183,427 | 9 / 9 | 2 / 9 / 9 | 31 / 100 / 100 |

The selected transcript in the mixed case is a small peer. The three source-to-
commit p95 values were 4.965 s, 4.711 s and 7.132 s. They fail the 2 s target.
Censoring and coalesced revisions prevent a qualification claim for the render
budget. Even the local represented-event estimate includes 2,002.272 ms in one
browser; it is not rounded into a 2 s pass. The Shift-key-to-two-rAF measurement
includes driver delay and does not replace or pass the existing 100 ms local
interaction gate. The original repaired run has initial geometry and wire
observations but lacks final geometry for its censored browsers; the resource
follow-up additionally records final viewport, scroll clips and focus.

Each profile attempted five warm and three fresh-process flushes **and** stops:
48 primary controls in total. All remained pending at the declared 8 s cap;
their replies slightly exceeded 1 s. All 48 primary canonical-prefix seals and
all six separate 1 KiB receipt/hash witnesses verified. An additional actual
1 GiB canonical seal completed in 4.806 s with independent hash/cursor readback:
N=1,073,741,824, B=1,075,457,024, pending=1,715,200 bytes. It sealed only N and
did not upload that tail. The earlier finite-local artifact's malformed seal
acknowledgement interpretation is marked invalid; the separate seal-revalidated
receipt preserves its corrected 16/16 results without pretending to remeasure
controls.

Combined stable-process resource measurements have their own `resources`
receipt. This reproduces follower prefixes, three clients, metadata population,
load and network; non-followed population padding contains empty transcripts
rather than replaying prior finite payloads. It starts a fresh server/database,
warms seeded prefixes and leaves OS cache uncontrolled. Comparisons must use
that same resource profile. The owned process tree includes Chromium children
and PostgreSQL workers, reports actual sampling intervals, and excludes the
sampling PowerShell process; very short-lived helpers may be missed. Resource
ceilings are profile-scoped, with one common fixed combined RSS ceiling, a
per-generated-MiB live CPU/SQL allowance and one linear verified-prefix reference
allowance. RSS never receives an allowance proportional to transcript size.

The resource follow-up sampled 22/32/30 times for local/controlled/mixed profiles,
with zero sampling failures and actual intervals of 2.255–3.018 s (the requested
500 ms pause excludes process-inventory cost). Sampled peaks were
2,163,228,672 / 2,599,251,968 / 2,696,499,200 bytes. The common combined RSS
ceiling is 3,437,732,864 bytes. Observed CPU was 31.469 / 54.969 / 48.344 seconds
in separate 56.265 / 85.311 / 78.236 s windows; these are not whole-matrix totals
or fully observed short-helper CPU. Exact CPU/SQL allowance terms remain in the
receipt for matching comparisons.

In the resource follow-up the transcript region stayed within the viewport and
the active element was `.session-records`, while its scroll position remained
far from the new end. For example, local final scrollTop was 74,536 with
scrollHeight 372,196 and clientHeight 735. This distinguishes that censored
observation from the initial offscreen setup mistake. The baseline performs
only Shift-key interaction after its initial End action and preserves the
natural follow behavior for the later client repair.

Final qualification still needs sufficient independent latency cohorts,
completed finite controls, a genuine concurrent large-prefix maximum envelope,
and all integrity/failure cases. No performance qualification is claimed by
these baseline records.

The runner's cooperative phase guard is 20 minutes, over three times the
measured calibration duration. Single-phase modes use that budget; repaired and
resource suites reset it for each profile after their initial preparation phase,
giving four phase budgets and an 80-minute maximum suite budget. Subprocess,
HTTP and database deadlines are limited by the remaining phase budget. Observer
and stack cleanup have separate finite bounds totaling at most three minutes.
Cleanup failure retains the owned
scratch with `sync_developer` responsible for resolution; it never deletes an
unjoined sandbox or adopts an existing scratch directory.

The native MCP component mode uses an explicit task and scratch owner:

```powershell
node hmem-server/frontend/perf/session-sync.mjs mcp --base-sha <exact-coordinator-base> --task-id 6e70b1ee-a329-4b22-95a6-cc49d3474499 --scratch .tmp/responsive-sessions-mcp-6e70b1ee --output hmem-server/frontend/perf/session-sync.mcp.json
```

This mode starts the real native server and MCP without frontend assets or
browsers. It measures a local 1 MiB follower and a controlled-link 1 GiB primary
with three independent 1 MiB peers, alongside ordinary MCP calls. Seeding occurs
before each measured wire profile. Resume has `N=B`; each later flush/stop adds
a fresh complete 4 KiB tail and records pre-control canonical `N`, frozen source
`B`, pending bytes, reply time, completion, locally checked receipt and independent
seal. Its completion cap uses the frozen 1 GiB zero-tail target plus twice the
pending bytes at the frozen 183,500.8 B/s reference. This conservative component
cap is not a tighter small-prefix target or a full-backlog promise.

Concurrent producers divide that aggregate sustainable rate among followers;
each offers 100 source records without waiting for observation. Canonical commit
cohorts count independently of source markers. Sampled native process resources,
HTTP requests, proxy connection reuse and ordinary MCP latency retain their
explicit scopes. These zero-browser components do not establish combined browser
visibility, interaction, matched combined resource limits, or the final required
five warm/three fresh-process and independent latency sample counts. The mode
uses one 20-minute cooperative suite guard and the existing finite cleanup bounds.
New component receipts use `session-sync.mcp*.json`; committed baseline receipts
and the frozen profile remain unchanged.

The task-local result is retained in
`hmem-server/frontend/perf/session-sync.mcp.json` (SHA-256
`4204fa18635c173c27d21a3bc45c3f91900c6e24ea9b2ab7f67579a11ad483b9`).
It records launch base `6586c58`, loaded runner `b61342dc…`, native binary and
source hashes, and the frozen baseline input. The later client-only commit
`9cbfcd83` did not change these backend inputs. Maintainers own these compact
benchmark receipts until the corresponding qualification evidence is superseded.
Generated JSONL sources, database, private logs and processes were joined and
removed after the run; no raw transcript fixture or diagnostic log is retained.

| Component observation | Local, one follower | Controlled link, four followers |
| --- | ---: | ---: |
| Initial committed prefixes | 1 MiB | 1 GiB + three independent 1 MiB peers |
| Resume, `N=B`, zero pending bytes | 1,196.861 ms | 14,902.129 ms |
| Actual aggregate production rate | 183,466.10 B/s | 183,495.70 B/s |
| Source events committed | 100/100 | 400/400 |
| Independent commit cohorts | 13 | 32 / 27 / 25 / 28 |
| Source-to-commit p95 | 999.099 ms | 1,104.842 / 1,212.145 / 1,213.274 / 1,049.675 ms |
| Flush reply / completed receipt | 371.924 / 371.935 ms | 901.680 / 14,450.082 ms |
| Stop reply / completed receipt | 909.415 / 1,017.770 ms | 914.067 / 14,515.756 ms |
| Ordinary MCP calls, failures | 13, zero | 81, zero |
| Ordinary MCP p95 / maximum | 52.967 / 52.967 ms | 99.480 / 1,350.367 ms |
| Completed follower HTTP requests | 32 | 594 |
| Peak overlapping completed follower requests | 1 | 2 |
| Full raw transcript HTTP reads | zero | zero |

Every finite control added 4,096 source bytes. For the large case flush froze
`N=1,075,457,024`, `B=1,075,461,120`; stop froze `N=1,075,461,120`,
`B=1,075,465,216`. All four control receipts matched independent local SHA-256,
exact boundaries/revisions and `pending_tail=false`. All five independent seals
matched the canonical immutable hash; the large seal took 5,643.974 ms. Seed time
(142.035 s for 1 GiB) is separate from verification. The fresh workspace started
with 53 terminated history sessions; by construction the large case also included
the already sealed local predecessor. Source commit totals were read directly
after the bounded drain; the runner does not serialize an exact drain completion
timestamp, and no drain-latency claim is made.

The observed reply and source-commit values meet their named component targets,
but this is not final sampling qualification. Each individual follower has fewer
than 100 independent commit cohorts; the four-follower aggregate has 112. There
are no browser/render/interaction samples or five-warm/three-fresh-process finite
series. Ordinary MCP results are not a matched before/after regression comparison;
the observed 1.350 s maximum remains visible. The HTTP overlap is a lower bound
from completed physical request intervals, supported by separate causal admission
and cancellation tests, not an inferred total helper count.

Sampled zero-browser working-set peaks were 749,469,696 and 1,521,475,584 bytes
from 4 and 21 observations, with no sampling failures. Requested pauses were
500 ms, but actual intervals included inventory work (up to 4.944 / 6.700 s).
The windows include benchmark verification and independent sealing, and differ
in follower count and prefix/load size. They neither establish prefix-independent
memory nor a matched combined resource or CPU/SQL regression pass. Those remain
part of final qualification after the separately owned sealing work.

The preceding 15,542-byte
`hmem-server/frontend/perf/session-sync.mcp.setup-failed.json` receipt (SHA-256
`7c7ec567a96894f294843345bbf78b1419f26a8e8f3e6b87fed7ea033322ec91`)
is retained under the same owner/expiry. Its loaded runner `ccd232e7…` supplied
a title different from the canonical immutable title. The uploader rejected that
binding before verification; its failed resume timings are setup evidence only.
The corrected helper takes title, format, parent, session and workspace from the
seeded canonical binding, with the atomically linked task and initialized MCP
workspace. A causal binding test and the first real local case now guard admission
of an expensive large fixture. The failure receipt and its classification remain
separate from the completed component observations.

Explicit database sealing now snapshots the caller's exact active cursor, hashes
immutable bytes without a session row lock, then locks and rechecks the same
workspace/session/count/revision before publishing. A racing append wins with a
conflict; an exact already completed seal remains retry-safe. Runtime metadata
and task eligibility stay independent of the explicit seal. Idle cleanup retains
its additional activity and linked-task checks. Hashing reads at most four
1 MiB windows per pipeline and returns the pool lease before hashing callbacks
or the next group. A range read freezes its metadata boundary and reads immutable
bytes without a SHARE row lock; gaps fail closed. These are structural bounds,
not measured lock-duration or peak-contention claims.

The optional native protocol measurement compares equal 1 MiB prefixes. It
counts PostgreSQL frontend Sync/simple-query and backend ReadyForQuery frames
while discarding every payload byte; these are not TCP packets or network RTTs.
`session-sync.db.cycles-before.json` and `session-sync.db.cycles-after.json`
retain commands, source/binary inputs, exact cache state and counts. With 1,024
1 KiB chunks, completed synchronization cycles fell from 1,035 to 31. A single
1 MiB chunk rose from 15 to 33 cycles because snapshot, group and final-check
transactions add fixed request-context work. No universal cycle reduction is
claimed. Node is required only for the explicitly enabled measurement; ordinary
core regression leaves that telemetry case pending.

The core regression also exposed two inherited creation-race tests that held the
fixture's only connection and then reused that pool for their observer or
independent winner. Exact base `2e27f89` contains both defects. The repair uses a
third pool and finite phase waits while retaining the original ordering and
refusal assertions. The hung test's exact process and native PostgreSQL sandbox
were verified, stopped and joined before disposal. This was test supervision,
not a production sealing failure. The renewed creation cases passed, followed
by 50 agent-session examples with zero failures and one optional telemetry
pending; this does not substitute for the large native performance cases.
The additional later-pipeline SQL-error case separately passed, restored its
isolated schema fixture, kept the active cursor unchanged and completed an exact
subsequent seal.

The database component command requires a completed build pairing:

```text
node hmem-server/frontend/perf/session-sync.mjs db --base-sha <exact-launch-base> --task-id e04fbd45-9e61-405b-9196-c51a153f8cd8 --scratch .tmp/responsive-sessions-db-e04fbd45 --build-inputs hmem-server/frontend/perf/session-sync.db.build-inputs.json --output hmem-server/frontend/perf/session-sync.db.json
```

Capture production source/package inputs before the coordinated native compile,
freeze them through its actual successful completion, then bind the installed
MCP, harness and normal server hashes. Launch refuses drift or an incomplete
build receipt. The retained receipt also classifies failed build attempts without
claiming native or timing success. Fixture COPY and guarded insertion calls each
contain at most 4 MiB and 256 rows under their existing 30-second SQL deadline;
the 20-minute suite guard does not extend that deadline. Temporary staging and
COPY files are disposed independently. Maintainers retain compact database
receipts until the corresponding qualification is superseded.

The completed native database component receipt is `session-sync.db.json`, with
its compact accounting in `session-sync.db.summary.json`. Its three single
attempts used exact `N=B` boundaries and no pending tail:

| Prefix / chunk size | Guarded seed | Seal HTTP/MCP call | Frozen seal limit | Authorized range-read p95 |
| --- | --- | --- | --- | --- |
| 1 MiB / 1 KiB | 0.667 s | 60.551 ms | 5,064.116 ms | 307.235 ms |
| 100 MiB / 1 MiB | 2.734 s | 649.967 ms | 8,313.019 ms | 402.409 ms |
| 1 GiB / 64 KiB | 28.153 s | 6,222.369 ms | 40,302.109 ms | 236.154 ms |

All three independent canonical seal hashes and all 16 active/evidence range
reads matched local source bytes. Seal call times exclude local hash preparation
and subsequent canonical readback, which the receipt records separately.
The first two paths were loopback; the 1 GiB case shaped only HTTP/WS traffic at
50 ms RTT and a shared 100 Mbit/s per direction. PostgreSQL observer connections,
server database hashing and local source hashing remained local and unshaped.
The complete 1 GiB prefix was not transferred across that HTTP link to seal it.

Actual custom/generic prepared plans covered active/fresh lists, historical task
membership and descendant project/task activity. Their largest execution time
was 15.177 ms for historical membership; all other measured plans were at most
2.088 ms. These observations did not justify an index or schema change. Logical
statement counts include context and background work and remain distinct from
the opt-in synchronization-cycle comparison.

Resource sampling returned only 1/1/2 observations, with a 5.650 s interval in
the large case. These samples do not establish peak use or size-independent
memory. Bounded hash memory follows the four-window implementation and causal
pool-release checks; combined resource and end-to-end qualification remain open.
The three-case run completed without a retry or guard extension and removed its
owned scratch after independent cleanup. Native build pairing, later diagnostic
test binaries, retained earlier failures and topology are identified separately
in the compact summary. This component result does not claim the final five
warm/three fresh-process or 100-independent-cohort acceptance.

After the shared native test fixture changed, the directly paired core executable
reran the complete agent-session and migration groups: 51 examples, zero failures,
one optional protocol-telemetry pending, in 91.905 seconds. A fresh paired MCP
test executable then passed the seven selected database integration cases:
four prefix-proof integrity cases, lost-ack replay/restart/mismatch, a finite stop
boundary during verification, and explicit live termination. Their four filters
took 31.052 / 12.173 / 10.784 / 5.164 seconds, with zero failures or pending cases.
Every direct process joined and each owned fixture directory was empty after
teardown. These tests use accelerated correctness polling and a default backend
without WebSocket traffic; they do not qualify dispatch lifecycle or performance.
Their input and binary bindings are separate from the earlier component build in
`session-sync.db.summary.json`; the original component receipts remain unchanged.
