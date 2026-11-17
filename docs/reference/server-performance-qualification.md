# Server performance qualification

[`tools/server-performance-qualification.json`](../../tools/server-performance-qualification.json)
records the combined contention qualification of 17 November 2026. It compares
the server at the performance review revision `7127a3d9` (before) with
`8d5101d0` (after, server sources identical to the current `master`) on one
matched workload in the throwaway Docker environment of the
[container sample](request-performance.md#container-sample). It is **not a
production sample**; the live system was not contacted. 2 of 8 gates fail;
task acceptance is not met.

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
   (request, pool and WebSocket telemetry).
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

## Production rollout decisions

These need separate decisions or scoped work; none is made by this receipt.

| Decision | Condition |
| --- | --- |
| Deploy the change-stream delivery repairs before the replay predicate is index-compatible ([candidate 4](request-performance.md#disposition)) | A matched rerun after that repair must pass the 10% foreground gate first. |
| Change pool striping ([candidate 2](request-performance.md#disposition)) or keep blocked database work outside delivery-turn budgets | Rerun the stuck case with production pool settings. |
| Accept idle polling cost at production scope count and outbox size (seven times this seed) | Measure passively after deployment or bound it first. |
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
build, restore and focused-spec commands.
