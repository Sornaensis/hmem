# Request performance

`hmem-server` attributes each request's latency to connection-pool checkout
wait, connection hold time, optional embedding provider time and the remaining
application work. The phase fields extend [request logging](request-logging.md);
process-wide checkout counters are part of `GET /api/v1/health`. Neither records
identifiers, paths or query values.

## Phases

All durations use the monotonic clock and are attributed to the thread that
the request logger admitted.

| Phase | Boundary |
| --- | --- |
| Pool wait (`db_wait_ms`) | From requesting a pooled connection to receiving it. Includes queueing and establishing new connections. |
| Connect (`db_connect_ms`) | Part of the wait spent opening a connection and setting its statement timeout. |
| Hold (`db_hold_ms`) | From receiving a connection to returning it: statements, transaction control and any work done while holding it. |
| Provider (`provider_ms`) | Observation discovery's provider availability check and query embedding, including failures and timeouts. |
| Application | `complete_ms` minus wait, hold and provider time: routing, authorization logic, encoding and transmission. |

Phases are sums over the request's checkouts. Unified search runs its three
entity queries concurrently and passes the request's timer to them, so its
hold time can exceed its duration and the application residual is then a lower
bound. Other work started on another thread is not attributed to the request;
it is counted as `unattributed` below. Background workers, such as the access
tracker flush, change-stream delivery and archive work, are unattributed.
The health check's own non-blocking database probe is not a checkout, but a
connection it opens is counted in `connects`, `connect_total_ms` and that
request's `db_connect_ms`, with `db_checkouts` and `db_wait_ms` 0.

## Health counters

`pool.checkout` in the health response is cumulative since process start for
all pools in the process:

| Field | Meaning |
| --- | --- |
| `checkouts`, `unattributed` | Completed checkouts, and those on threads without a request timer. |
| `connects`, `connect_total_ms` | New connections and their total time. |
| `wait_total_ms`, `wait_max_ms`, `hold_total_ms` | Total and maximum checkout wait, and total hold time. |
| `wait_buckets` | Checkout waits by inclusive upper bound `le_ms` of 0.1, 0.5, 1, 5, 10, 50, 100, 500 and 1000 ms, then `null` for longer waits. |
| `capacity_waits` | Checkouts that queued at least 1 ms, excluding connection time, although checked-out and waiting checkouts together were fewer than the configured maximum when they were requested and no connection was opened meanwhile. |

`resource-pool` divides the configured connections evenly among stripes, one
per GHC capability up to the pool size, and a checkout can only use its
thread's stripe. A capacity wait therefore shows that a stripe was full while
another had idle capacity. The count is a lower bound: waits behind a full
stripe while the whole pool is also in demand are not counted. The counters
are process-wide, so they are only meaningful with one pool per process, as in
the server.

## Isolated baseline

[`tools/request-latency.qualification.json`](../../tools/request-latency.qualification.json)
records the 16 November 2026 baseline, its matched plans and limits. The opt-in
`RequestLatencyMeasurement` and `RequestPlanModes` examples in
`hmem-server/test/HMem/Server/RequestLatencyMeasurementSpec.hs` reproduce it
with `HMEM_REQUEST_LATENCY_MEASUREMENT=1` or `HMEM_REQUEST_PLAN_MODES=1`, plus
`HMEM_TEST_SESSION_DIAGNOSTICS=1` for `pg_stat_statements` and
`HMEM_REQUEST_LATENCY_OUTPUT` for the summary directory.

The fixture is the large perf shape from a fixed seed: one repository
workspace with 250 projects, 1,500 tasks, 722 dependencies, 2,000 Observations
and 5,789 audit rows spread over 90 days. The server uses the production pool
defaults (10 connections, 127.0.0.1 DSN) against an isolated native PostgreSQL
18.2 on a 24-capability Windows host, so the pool has 10 one-connection
stripes. Shared buffers are warm; the embedding provider is disabled.

Sequential warm requests (n=199 per variant, milliseconds, p99 is the
second-largest sample; wait, hold and application are means):

| Variant | Route | p50 | p95 | p99 | Checkouts | Wait | Hold | App |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| discovery content | `/observations/discovery` | 538.5 | 578.5 | 653.0 | 3 | 0.01 | 541.7 | 1.6 |
| discovery continuation | `/observations/discovery` | 538.4 | 582.1 | 807.4 | 3 | 0.01 | 546.3 | 1.4 |
| timeline day buckets | `/workspaces/{workspaceId}/timeline/buckets` | 247.1 | 259.2 | 276.6 | 2 | 0.00 | 245.6 | 2.8 |
| timeline events | `/workspaces/{workspaceId}/timeline` | 233.9 | 240.3 | 259.3 | 2 | 0.00 | 229.7 | 1.0 |
| timeline week buckets | `/workspaces/{workspaceId}/timeline/buckets` | 139.6 | 149.4 | 204.0 | 2 | 0.00 | 140.7 | 0.6 |
| timeline all-time buckets | `/workspaces/{workspaceId}/timeline/buckets` | 139.4 | 147.2 | 152.7 | 3 | 0.01 | 139.5 | 0.6 |
| navigation batch | `/workspaces/{workspaceId}/navigation/batch` | 40.1 | 51.5 | 63.5 | 28.3 | 0.05 | 40.6 | 0.7 |
| task status update | `/tasks/{taskId}` | 36.5 | 41.6 | 45.9 | 9 | 0.01 | 36.2 | 0.3 |
| task field update | `/tasks/{taskId}` | 36.4 | 42.4 | 45.8 | 9.8 | 0.01 | 36.2 | 0.2 |
| navigation root | `/workspaces/{workspaceId}/navigation` | 24.3 | 26.0 | 27.4 | 5 | 0.01 | 23.4 | 1.0 |
| navigation focus | `/workspaces/{workspaceId}/navigation/focus/{entityType}/{entityId}` | 11.7 | 14.0 | 16.3 | 13 | 0.02 | 11.5 | 0.3 |
| navigation project | `/workspaces/{workspaceId}/navigation` | 11.2 | 14.9 | 18.9 | 7.8 | 0.01 | 11.0 | 0.3 |
| subject tree | `/observations/subject-tree` | 10.9 | 12.0 | 13.2 | 3 | 0.01 | 10.7 | 0.2 |
| Observation search | `/observations` | 10.6 | 17.9 | 19.4 | 3 | 0.01 | 10.5 | 1.2 |
| project overview | `/projects/{projectId}/overview` | 10.3 | 11.9 | 14.1 | 6 | 0.01 | 10.0 | 0.5 |
| unified search | `/search` | 9.6 | 19.2 | 87.6 | 6 | 4.16 | 15.8 | 0.0 |
| discovery subject tree | `/observations/discovery` | 6.3 | 346.1 | 354.2 | 3 | 0.01 | 70.8 | 0.5 |
| task search | `/tasks` | 5.6 | 6.7 | 7.2 | 3 | 0.01 | 5.0 | 0.7 |
| task read | `/tasks/{taskId}` | 2.7 | 3.9 | 4.6 | 3 | 0.01 | 2.6 | 0.1 |
| planning counts | `/workspaces/{workspaceId}/planning-counts` | 2.3 | 3.0 | 3.2 | 2 | 0.00 | 2.2 | 0.1 |

Sequential tails are database execution. Unified search waits only while the
pool opens connections for its concurrent branches.

Concurrent profiles with disjoint write targets (n=100 for 4 clients, 128 for
16; p95 in milliseconds):

| Variant | Sequential | 4 clients | 16 clients | 16 clients, one stripe |
| --- | --- | --- | --- | --- |
| navigation batch | 51 | 1,968 | 2,714 | 921 |
| navigation project | 15 | 1,137 | 1,869 | 307 |
| navigation root | 26 | 944 | 1,445 | 198 |
| navigation focus | 14 | 95 | 674 | 507 |
| unified search | 19 | 284 | 860 | 146 |
| task field update | 42 | 65 | 756 | 404 |
| task status update | 42 | 69 | 381 | 408 |
| task read | 4 | 10 | 441 | 124 |
| task search | 7 | 13 | 64 | 105 |
| planning counts | 3 | 6 | 24 | 104 |
| discovery content | 578 | 599 | 1,363 | 784 |
| timeline week buckets | 149 | 160 | 843 | 265 |

One stripe was mixed: against the first 16-client run, p95 ranged from 4.3
times higher (planning counts) to 7.3 times lower (navigation root).

## Findings

1. **Pool stripes.** With 4 clients and at most 3 active backends, 2,652 of
   11,716 checkouts were capacity waits and checkout wait totalled 111 s in a
   112 s phase. Batch, root and project navigation tails were at least 97%
   pool wait. With one stripe the 16-client profile finished in 33.7 s instead
   of 55.2 s and 48.2 s, and capacity waits fell from 206 and 548 to 1, but it
   is not a clear win: p95 rose for planning counts, task search and task
   status updates, navigation focus improved only 1.3 times, and the median
   rose for every database route except unified search and navigation root
   (task read 9 to 32 ms). Requests making many checkouts suffer most: 28 per
   navigation batch, 9 to 10 per task update.
2. **Observation discovery.** Hold is 99.7% of 538 ms. Matched plans show
   nested-loop joins over CTE scans estimated at one row, removing 1.0 to
   2.9 million rows per statement, about 95% of statement time. The discovery
   subject tree takes about 350 ms when half of the Observations match and
   6 ms when none do.
3. **Timeline events.** The second to fifth execution on a connection takes
   21 ms with a custom plan. From the sixth, PostgreSQL's generic plan
   misestimates the optional range predicates and takes 220 ms; forced custom
   plans stay at 33 to 44 ms. Forcing custom plans for every statement slowed
   other routes.
4. **Timeline buckets.** Each bucket rescans the inventory deltas, and the
   inventory spills about 14 MB to temporary files per call.
5. **No change needed** for lock activity (lock waits, all advisory, in at most
   94 of 1,099 samples, at most 3 ungranted locks, no deadlocks), connection setup (47.9 ms on 127.0.0.1,
   66.5 ms on `localhost`) or application work (at most 3.4 ms).

## Disposition

These are candidates, not changes. Each needs the passive sample below and a
matched rerun before implementation.

| Priority | Candidate | Condition |
| --- | --- | --- |
| 1 | Configure the pool with one stripe, or stripes holding several connections each. | One stripe is not a clear win (finding 1). Production `capacity_waits` or tail pool wait, and a matched rerun with lower 4- and 16-client tails without an unacceptable light-load median. |
| 2 | Rank discovery candidates without joins over CTE scans. | Identical discovery results and a matched plan. |
| 3 | Avoid the generic plan for the timeline events statement only. | Matched executions after the fifth on one connection. |
| 4 | Accumulate timeline inventory per bucket instead of rescanning. | Identical bucket output. |
| 5 | Fewer transactions per request where semantics allow. | After priority 1, with contention evidence. |

## Sample limits

The passive native sample was not collected: live stack/DB access not
authorized in this run, 2026-11-16. Production stripes, request mix,
concurrency and data sizes are therefore unknown; request-log phase fields and
the health counters are the instruments for that sample. The isolated run is
one host and one workspace with warm buffers. Results vary between runs on
this shared host: an earlier matched run had 2 to 6 times higher sequential p99
for discovery and timeline routes. Hybrid discovery and provider time were not
measured.
