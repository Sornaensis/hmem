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
