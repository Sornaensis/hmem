# Server performance qualification

[`tools/server-performance-qualification.json`](../../tools/server-performance-qualification.json)
records the combined contention qualification of 17 November 2026. It compares
the server at the performance review revision `7127a3d9` (before) with
`8d5101d0` (after) on one matched workload in the throwaway Docker environment
of the [container sample](request-performance.md#container-sample). It is **not
a production sample**; the live system was not contacted. 2 of 8 gates fail;
task acceptance is not met. A later
[production-default rerun](#production-default-rerun) of `f206ff72` passes the
measured parts of all eight gates; the replay cut and revocation were not
re-verified in that run.

## Matching

Both revisions have the same migrations, so both ran against byte-identical
copies of one seeded volume (4.85 GB: six planning workspaces, 562 archived and
12 active sessions, 413,000 audit rows). Each run started a fresh server on a
fresh copy. Matched runs were ordered before, after, before, after; gates use
the mean of the two runs per revision. The before image is built from the
repository `Dockerfile` with the `hmem-core/cbits` copy; its Stack dependency
layer was resolved from `8d5101d0` package metadata (same `stack.yaml` and lock
file) before the `7127a3d9` package files were restored. Every run used the
same workload definition; runner versions differ only in output fields, and
before-3's runner hash was not recorded (receipt `inputs.runner_versions`).

`tools/perftest/combined.mjs` drives one run against the running stack:

| Phase | Workload |
| --- | --- |
| Setup | 16 empty workspaces, a blocked-scope workspace, proof and bulk sessions |
| Snapshots | Full and shell resyncs of 4,480-4,533 items beside a same-scope writer |
| Sockets | 27 canonical WebSockets: hot, measured, slowly written and 16 empty scopes |
| Idle | 30 s, no writes |
| Contention | 120 s: 4 foreground clients over 10 ordinary routes, 16 open-loop hot writes/s on one workspace and 1/s on another, 4 capture appenders, 2 proof sessions, a stalled reader, full and shell resyncs, two archive streams (86 and 35 MB), twelve 1 MB appends |
| Stuck catch-up | The `6d9ef144` case: a replay scope lock held 8 s while hot writes continue and new sockets connect |
| Evidence and drain | Exact terminate receipts, evidence read-back, SIGINT shutdown |

The stalled reader runs in a node container on the perftest network and stops
reading after the upgrade. Request phases, pool counters and WebSocket and
session-traffic telemetry exist only in the after revision; comparisons use
client timings and `pg_stat_statements`.

## Gates

| Gate | Limit | Before | After | Result |
| --- | --- | --- | --- | --- |
| Ordinary foreground p95 | ≤ 10% over baseline | 139.9, 123.0 ms | 156.4, 152.7 ms (+17.6%) | **Fail** |
| Archive incremental working set | ≤ 128 MiB | 108, 111 MiB | 55, 49 MiB | Pass (86 and 35 MB streams only) |
| Stalled reader retired, healthy sockets progress | 250 ms write deadline; every change delivered | not retired; 13-18% of changes delivered | retired after 0.3 MB; 100% delivered, p95 0.59-0.70 s | Pass |
| Stuck catch-up beside healthy peers | admission, dispatch, healthy progress, reclamation, drain | all dispatch stalled until release | healthy sockets retired | **Fail** (healthy progress) |
| Snapshot write round trips, replay cut | fewer; cut preserved | 4,533 item inserts | 18 item inserts | Pass |
| Proof and evidence receipts | all verified and exact | 44 proofs, 4 receipts | same | Pass |
| Idle authorization per empty scope | not per scope | 24 checks/30 s | none | Pass |
| Upgrade telemetry | truthful | upgrades logged as 500 | `upgrade_closed`, no status | Pass |

The foreground gate is the 10% limit of
[session synchronization](session-sync-performance.md) and
[sealed archives](sealed-session-archives.md#budgets-and-remaining-qualification).
Seven of ten routes regress 2.3 to 3.1 times at p95 (task read, task search,
project and focus navigation, planning counts, project overview, Observation
search); task updates, root navigation and unified search do not. Capture
append p95 rises 49%. No request failed.

The DB-backed focused specs for skewed rotation, grant-user idle
reauthorization and revocation, pool and snapshot-lock contention, the
snapshot/replay cut, the stuck catch-up and owned WebSocket delivery pass
(23 examples). They cover the revocation and per-user paths the container
cannot reach.

## Findings

1. **Replay scans are the leading explanation for the foreground
   regression.** Per 120 s phase the after revision runs 7,100 to 7,800 outbox
   scans (67 to 74 s of statement time) and 3,200 to 3,400 scope locks,
   against 220 to 280 scans (about 1 s) and 220 to 320 scope locks before.
   Scans average about 9.5 ms; [finding 3](request-performance.md#container-findings)
   attributes this to the replay predicate not being index-compatible. About
   half of the slowest 5% of after foreground time is pool wait. This is
   correlation, not isolation: before has no pool counters, scan and
   scope-lock statement time is only 14 to 15% of after's held connection
   time, and no ablation separates the scans from other after-only changes
   (request, pool and WebSocket telemetry). The
   [replay repair rerun](#replay-repair-rerun) removes the scan cost but not
   the per-route regression.
2. **A stuck catch-up retires healthy sockets.** Three or four of four healthy
   hot-workspace sockets were retired by the 750 ms work budget in each after
   run. The leading explanation is that the blocked catch-up keeps its
   one-connection pool stripe for the whole lock hold, so delivery turns that
   need that stripe overrun the budget. The focused spec, which warms every
   stripe and dispatches one batch directly, passes. One unmatched diagnostic
   run with four multi-connection stripes (`GHCRTS=-N4`) retired no healthy
   socket, but it also cuts RTS capabilities from 24 to 4, so it does not
   isolate striping. Before kept every socket but delayed all delivery until
   the lock was released (up to 8 s).
3. **Stalled readers no longer block delivery.** Before, one reader that
   stopped reading held live dispatch for every client.
4. **Snapshots** write items in batches: 18 inserts instead of 4,533, and a
   same-scope writer waits at most 0.27 to 0.34 s instead of 0.81 to 0.94 s.
5. **Idle polling scales with retained scopes.** After sends no idle frames or
   authorization checks but scans every retained scope once per idle sweep:
   about 18 scans/s (5.2 s of statement time per 30 s) against 8/s before.
6. **Memory** under contention peaks at 336 to 348 MiB against 377 to 400 MiB
   before.

## Replay repair rerun

[`tools/outbox-replay.qualification.json`](../../tools/outbox-replay.qualification.json)
records an ablation of the replay repair on 17 November 2026 with the same
method on a freshly seeded volume of the same scale. Variant **a** makes the
two replay predicates match the `(scope, coalesce(workspace_id, zero), cursor)`
index; **a+b** also skips the dispatcher read of a scope whose committed
counter has not passed its acknowledgement. Both are built from `d4d70f78`.
Runs were ordered before, a, a+b, a+b, a, before.

| Measure | Before | a | a+b |
| --- | --- | --- | --- |
| Pooled foreground p95 | 87.8, 96.3 ms | 93.1, 96.9 ms (+3.2%) | 99.1, 102.2 ms (+9.3%) |
| Routes over +10% at p95 | | 7 of 10 (1.3-3.2x) | 7 of 10 (1.4-3.6x) |
| Seeded capture append p95 | 57.3, 62.1 ms | 56.2, 42.3 ms (-17%) | 45.9, 69.4 ms (-3%) |
| Outbox reads per 120 s | 213-229 (0.8 s) | 12,725-16,315 (0.2 s) | 2,340-2,396 (0.13 s) |
| Outbox reads per idle 30 s | 248-252 (1.5 s) | 668-692 (4 ms) | 0 |
| Scope locks per 120 s | 255-271 (0.5 s) | 4,005-5,113 (8-10 s) | 3,053-3,143 (7 s) |
| Pool wait in the foreground tail | | 35-42% | 39-42% |

Every new generic and custom plan, for the global scope and the hot
workspace, is an ordered index scan with an index condition on scope,
workspace identity and cursor, and no sort or sequential scan. The pooled and
capture gates pass; the per-route gate still fails. The routes that regress are
the cheap reads (task read and list, planning counts, project and focus
navigation, project overview, Observation search). The skip does not change
foreground latency, so outbox reads no longer explain the regression. The
leading remaining candidate is per-socket delivery scope locking waiting behind
hot writers on one-connection pool stripes; it is not isolated. The stuck
catch-up still retires healthy sockets in three of four after runs. The
[production-default rerun](#production-default-rerun) shows neither problem at
production pool defaults, but it names no cause for this per-route regression
at pool size 10.

## Production-default rerun

[`tools/postpool-qualification.json`](../../tools/postpool-qualification.json)
records a matched rerun on 17 November 2026 of `f206ff72` (replay repair and
separate delivery pool) against `7127a3d9` at the production pool defaults
(main 15, delivery 10, GHC `-N`), on a freshly seeded volume of the same scale.
A third, measurement-only image removes the scope lock from the three
per-socket delivery calls (replay page read, acknowledgement, hidden rebase)
and reads the scope counter row without locking instead. It drops
serialization against revocation and retention pruning, so it is unsafe and
was never committed. Runs were ordered before, after, ablation, ablation,
after, before. The before image has one pool, so it ran with 15 connections
(earlier receipts used 10) while after holds up to 25 (15 main, 10 delivery):
the pass is at production defaults, not at equal total connections. Every run
used a scratch copy of `combined.mjs` that adds output only (delivery-pool
counters, pool gauges and two health GETs outside the lock hold); its hash and
diff hash are in the receipt `inputs`.

This is the matched rerun for tasks `033d1704` and `17e8cd32`. The final
`073d6bfb` qualification will run on the build that includes task `37f4f786`
(single-stripe main pool and fair HTTP admission, for hundreds of concurrent
requests).

| Gate | Before | After | Ablation |
| --- | --- | --- | --- |
| Foreground p95, pooled | 151.2, 157.6 ms | 162.5, 170.3 ms (+7.8%) | 195.2, 162.4 ms (+15.8%) |
| Foreground p95, worst route | | search 1.035 | search 1.21, task update 1.14 |
| Seeded capture append p95 | 77.1, 46.1 ms | 35.1, 34.8 ms (-43%) | 72.8, 50.1 ms (0%) |
| Archive working set (≤ 128 MiB) | 118, 99 MiB | 57, 52 MiB | 53, 54 MiB |
| Stalled reader retired, healthy progress | not retired; 26-61% delivered | retired; 100% delivered | retired; abl-1 1,917 of 1,918 by phase end, abl-2 100% |
| Stuck catch-up beside healthy peers | delivery stalled up to 8 s | only the blocked owner retired (754-756 ms) and reclaimed; every write delivered; clean drain | not exercised (no lock wait) |
| Snapshot item inserts | 4,547 | 18 | 18 |
| Proofs and receipts | 44 verified, exact | same | same |
| Idle authorization checks per 30 s | 22-28 | 0 | 0 |
| Upgrade telemetry | logged as 500 | `upgrade_closed` | `upgrade_closed` |

After passes the measured part of every gate. Cheap reads are now faster than
before (task read 5.5-5.8 ms against 7.9-29.4 ms). The delivery pool had no
capacity waits; the main pool's share of the foreground tail spent waiting was
18-19%, against 35-42% in the replay repair rerun. That comparison sets main 15
against the earlier receipt's 10 in a different absolute regime (before pooled
p95 151-158 ms here, 88-96 ms there). The ablation cut per-phase scope-lock
calls from 3,067-3,167 (13-16 s) to 48 snapshot calls but left cheap reads
unchanged and heavy routes slower. It does not confirm scope locks as a cause.
No cause is named for the earlier per-route regression at pool size 10.

Limits: before-1's cheap reads were 2-8 times slower than before-2's. After
still passes against before-2 alone (worst route 1.008, pooled 1.056). Unified
search p95 varies from 156 to 317 ms across runs. Hot writes succeeded 1,920 of
1,920 in both after runs but 1,918 in abl-1 and 1,914 in before-2; those
failures are unexplained. The replay cut and revocation were not re-verified
in this run.

Open defect: a snapshot resync under contention returned an unexplained HTTP
500 on both revisions (a shell resync in after-1 here, a full resync in one
before run of the first qualification). Server logs were on tmpfs, so the
cause was not captured. Reproduce it with persisted server logs before
deployment or the final qualification.

## Production rollout decisions

These need separate decisions or scoped work; none is made by this receipt.

| Decision | Condition |
| --- | --- |
| Deploy the change-stream delivery repairs ([candidate 4](request-performance.md#disposition)) | At production pool defaults `f206ff72` passes the measured parts of all eight container gates (see the [production-default rerun](#production-default-rerun)). Open: the snapshot resync HTTP 500, and the final `073d6bfb` qualification. |
| Change HTTP pool striping ([candidate 2](request-performance.md#disposition)) | Task `37f4f786` (single-stripe main pool and fair HTTP admission) for HTTP concurrency of hundreds of requests. The separate delivery pool already keeps healthy sockets progressing beside a stuck catch-up at 15/10. |
| Accept idle polling cost at production scope count and outbox size (seven times this seed) | The dispatcher skip removes idle outbox reads; each turn still reads four scope counters. Measure passively after deployment. |
| OIDC audiences, TLS, reverse proxies and production pool and RTS settings | Not covered by the container; confirm with passive production telemetry. |

## Limits

Two matched runs per revision on one shared host; before runs differed by 13%
pooled p95. The most favourable pairing still regresses 9.1% pooled and every
cheap read two to three times. Foreground clients are closed-loop, so the
before revision completed 9 to 18% more foreground requests; hot writes were
open-loop and matched. The archive gate streamed 86 and 35 MB archives only,
not the 1 MiB, 100 MiB and 1 GiB fixed-budget series. Delivery latency starts
at the write response, so it is a lower bound. RSS is sampled once per second. Local authentication makes
every principal the trusted superadmin, so revocation is covered only by the
focused specs. MCP follower diagnostics were not collected; capture used HTTP
clients. Admission limits were not approached (27 sockets in one audience).
The [concurrency runner](concurrency-runner.md) drives 300 to 1,000
concurrent clients.

## Reproduce

Seed and start the [container environment](request-performance.md#container-sample),
copy the stopped `hmem-perftest-pgdata` volume to a base volume, then for each
run restore the base copy, start the stack with `PERFTEST_IMAGE` set to the
revision's image and run:

```sh
node tools/perftest/combined.mjs --label <run>
```

The runner writes one summary to `.tmp/perftest/combined` (or
`PERFTEST_OUT`) and stops the server. The receipt's `commands` list the exact
build, restore and focused-spec commands. The production-default rerun ran a
scratch runner copy, not the committed file (see its receipt `inputs`).

The committed runner now reports what that copy added. The idle, contention
and stuck catch-up phases each carry health deltas: `checkout` (main pool, as
before), `delivery_checkout`, `pools` (active and maximum connections of both
pools at the phase start and end) and `http_admission` (counter deltas and end
gauges). Fields for a pool or admission object the image does not report are
null. The stuck-phase deltas come from health GETs before the lock holder
starts and 1.5 s after it releases, outside the hold. Statement classes match
the full normalized statement text; earlier receipts matched a 160-character
prefix and missed statements marked later, so their class counts are lower
bounds. `top` statements keep a 160-character prefix and add `queryid`. The
workload and timings are unchanged. `node tools/perftest/combined.mjs
--self-test` checks classification and the delta computation without Docker;
receipts record the runner's SHA-256.
