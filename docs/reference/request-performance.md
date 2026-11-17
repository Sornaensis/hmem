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

## Isolated findings

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

## Container sample

The receipt's `containerSample` records a second, passive sample: the server's
own request-log phase fields and `pool.checkout` counters, from an isolated
Docker container on synthetic data at the stated per-family scale. It is **not
a production sample**. The live system was not contacted.

The image is `master` 8d5101d0, built with the `hmem-core/cbits` copy that
`Dockerfile` now includes. PostgreSQL is the 17.11 official image with default
128 MB shared buffers and `pg_stat_statements`; Docker Desktop has 24 CPUs, so
the pool has 10 one-connection stripes. Data was created through the HTTP API
in six workspaces of the perf shape above. Sessions were uploaded, terminated
and archived by the server's worker. API task updates were then replicated in
SQL to reach the audit volume, and history was spread over 90 days:

| Family | 2 Nov production | Container | Scale |
| --- | --- | --- | --- |
| Database | 6.25 GB | 5.05 GB | 0.81 |
| Terminated sessions, raw / gzip | 562, 3.27 / 1.34 GB | 562, 3.27 / 1.34 GB | 1.00 |
| Active sessions | 12, 347 MB | 12, 348 MB | 1.00 |
| `session_chunks` | 2.83 GB | 2.41 GB (33,825 chunks) | 0.85 |
| `session_archive_parts` | 1.45 GB | 1.44 GB | 0.99 |
| Archive checkpoints and ledger | 0.17 GB | 0.08 GB | 0.45 |
| `change_stream_outbox` | 0.91 GB | 0.13 GB (77,220 rows) | 0.14 |
| `audit_log` | 0.80 GB | 0.85 GB (412,738 rows) | 1.07 |

Production planning counts are unknown. The container has 1,500 projects,
9,153 tasks and 12,000 Observations. The measured workspace has 250 projects,
1,559 tasks, 719 dependencies, 2,000 Observations and 86,438 audit rows before
load (87,068 at the matched plans).

`tools/perftest/compose.yaml` defines the environment and
`tools/perftest/perftest.mjs` seeds and drives it. Put random hex values for
`POSTGRES_PASSWORD`, `HMEM_API_KEY` and `HMEM_AUTH_LOCAL_USER_TOKEN` in the
gitignored `.tmp/perftest/secrets.env`, one `KEY=value` per line (for example
from `node -e "console.log(require('crypto').randomBytes(32).toString('hex'))"`),
then build and start it. The runner requires the `hmem.perftest.revision`
label; set `PERFTEST_IMAGE` for a tag other than `hmem-perftest:8d5101d0`.

```sh
docker build -f Dockerfile --target runtime --label hmem.perftest.revision=$(git rev-parse HEAD) -t hmem-perftest:$(git rev-parse --short=8 HEAD) .
docker compose -p hmem-perftest -f tools/perftest/compose.yaml --env-file .tmp/perftest/secrets.env up -d
```

The receipt's `containerSample.commands` lists the recorded commands in order.
Each profile runs the isolated route mix after one warm-up round, joining
client samples to log entries by `X-Request-Id`. Profiles: sequential (n=79
per variant), 4 clients (n=80), 16 clients (n=112), and 16 clients plus four
session capture appenders (n=112, 1,023 appends). Times are milliseconds; hold
is the sequential mean. The tail share is pool wait in the slowest 5% of the
capture profile.

| Variant | Seq p50 | Seq p95 | Seq p99 | Hold | 4 p95 | 16 p95 | 16+capture p95 | 16+capture p99 | Tail wait |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| timeline day buckets | 4,383 | 4,648 | 4,817 | 4,398 | 5,599 | 11,187 | 7,599 | 8,149 | 48% |
| timeline week buckets | 3,381 | 3,711 | 3,747 | 3,406 | 5,454 | 7,584 | 6,474 | 7,065 | 54% |
| timeline all-time buckets | 3,371 | 3,635 | 3,727 | 3,396 | 5,492 | 8,856 | 6,462 | 7,243 | 56% |
| discovery content | 527 | 557 | 581 | 525 | 567 | 4,111 | 858 | 3,394 | 83% |
| discovery continuation | 523 | 564 | 580 | 522 | 809 | 4,133 | 3,495 | 6,430 | 91% |
| discovery subject tree | 8.1 | 328 | 345 | 66 | 2,456 | 4,547 | 3,407 | 3,803 | 98% |
| task field update | 55 | 140 | 149 | 53 | 2,863 | 4,400 | 3,527 | 4,502 | 99% |
| task status update | 55 | 65 | 129 | 53 | 101 | 3,417 | 3,069 | 3,537 | 99% |
| navigation batch | 54 | 66 | 98 | 52 | 108 | 4,867 | 3,655 | 12,267 | 99% |
| navigation root | 48 | 59 | 119 | 44 | 113 | 5,540 | 4,121 | 5,993 | 99% |
| navigation project | 15 | 21 | 48 | 14 | 2,629 | 3,692 | 3,915 | 5,862 | 100% |
| navigation focus | 17 | 20 | 24 | 16 | 34 | 4,168 | 320 | 4,003 | 99% |
| unified search | 15 | 27 | 39 | 22 | 2,935 | 6,973 | 4,783 | 6,634 | 123% |
| timeline events | 8.9 | 35 | 47 | 8.0 | 1,933 | 3,731 | 3,143 | 5,543 | 100% |
| task read | 4.1 | 42 | 66 | 3.9 | 31 | 1,642 | 3,182 | 6,981 | 100% |
| session append | | | | | | | 3,076 | 4,239 | 100% |

Other variants stayed below 60 ms sequential p95 and below 530 ms p95 with
capture. All requests completed, apart from a runner defect, since fixed, that
caused 17 `429 discovery_capacity` responses in a discarded first capture
profile. Unified search waits for three concurrent branches, so its tail wait
exceeds its duration.

| Profile | Duration | Checkouts | Unattributed | Capacity waits | Wait total | Waits over 1 s |
| --- | --- | --- | --- | --- | --- | --- |
| sequential | 1,019 s | 107,503 | 97,933 | 1,603 | 578 s | 153 |
| 4 clients | 283 s | 19,764 | 10,133 | 1,960 | 713 s | 214 |
| 16 clients | 211 s | 17,895 | 4,419 | 124 | 2,165 s | 535 |
| 16 clients + capture | 163 s | 25,534 | 5,912 | 180 | 1,962 s | 608 |

## Container findings

1. **Timeline buckets dominate at production-scale history.** With about 87,000
   audit rows in the workspace, buckets take 3.4 to 4.4 s sequentially, against
   140 to 250 ms with the isolated baseline's 5,789. The matched plan reads the
   whole 0.85 GB audit table, removes 327,692 rows by filter (about 326,300 from
   other workspaces) and writes 0.4 to 0.6 GB of temporary files. For 93 day
   buckets its per-bucket loop then evaluates 8.0 million joined rows and keeps
   half. The statement was 71% of all pooled hold time sequentially and 84 to
   85% under concurrency.
2. **Pool waits follow those holds.** A bucket request holds one stripe's only
   connection for 3 to 5 s. Requests and background checkouts on that stripe
   wait meanwhile, even when the pool has idle connections elsewhere.
   With 4 clients, timeline events, project navigation, the discovery subject
   tree, task field updates and unified search reached 1.9 to 2.9 s p95; task
   status updates stayed at 101 ms. With capture, pool wait was 98 to 100% of
   the tail for every database variant except timeline buckets (48 to 56%),
   discovery content (83%), discovery continuation (91%) and unified search
   (123%). Session capture
   appends had a 13 ms median but a 3.1 s p95 under 16 clients. Even the
   sequential profile had 1,603 capacity waits and 575 s of background wait.
3. **Background checkouts are most checkouts.** Unattributed checkouts were
   91% sequentially (about 96/s) and 23 to 51% under load. One source is the
   change-stream replay statement (`HMem.DB.ChangeStream`, about six polls/s).
   Its `workspace_id IS NOT DISTINCT FROM $2` predicate is not an index
   condition: the custom plan uses `idx_change_stream_outbox_replay` on scope
   and cursor only and filters `workspace_id`, and the generic plan scans the
   whole outbox: 12,645 buffers, 13 ms, at one seventh of the production outbox.
4. **Discovery is unchanged** at 523 to 527 ms p50, over 99% hold, with the same
   2,000 Observations per workspace.
5. **The timeline events generic-plan tail did not reproduce**: 8.9 ms p50 and
   47 ms p99 on PostgreSQL 17.11, against 234 ms in the native PostgreSQL 18.2
   run.
6. **No change needed** for locks (lock waits in at most 54 of 6,533 samples,
   advisory or transaction-id; at most 2 ungranted; no deadlocks), connection
   setup (5.5 ms in the container) or application work (at most 7.2 ms).

## Disposition

These are candidates, not changes. Each needs a matched rerun, in the
container or a later production sample, before implementation.

| Priority | Candidate | Condition |
| --- | --- | --- |
| 1 | Bucket timeline inventory without a whole-table audit scan and per-bucket rescans; accumulate per bucket. | Identical bucket output; matched plan and container rerun at this history size. |
| 2 | Configure the pool with one stripe, or stripes holding several connections each. | After priority 1. The isolated one-stripe probe was mixed. A container rerun must lower 4- and 16-client tails and capacity waits without an unacceptable light-load median. |
| 3 | Rank discovery candidates without joins over CTE scans. | Identical discovery results and a matched plan. |
| 4 | Make the change-stream replay predicate index-compatible. | Identical replay results; matched plan at production outbox size. |
| 5 | Fewer transactions per request where semantics allow, including session appends (6 checkouts). | After priority 2, with contention evidence. |
| 6 | Avoid the generic plan for the timeline events statement only. | Reproduce it on the production PostgreSQL major version first. |

In the [server performance qualification](server-performance-qualification.md),
candidate 4's replay scans are the leading explanation for a foreground
regression under WebSocket load, and candidate 2's one-connection stripes for
healthy sockets retired beside a stuck catch-up. No ablation or matched run
isolated either cause.

## Sample limits

No passive native sample was collected. On 16 November 2026 the user chose
the isolated container instead of the live system. Production request mix,
concurrency, PostgreSQL version and settings, chunk sizes and planning
history therefore remain unknown. The container's Linux VM, PostgreSQL 17
and 5.5 ms connections differ from the native Windows host's 47.9 ms
connections. Container data is synthetic. Production's archive ledger is 8.5
times larger, suggesting smaller appends. The outbox is a seventh of
production's, and audit history was replicated from API updates and spread
uniformly. The measured workspace may hold more or less history than the
busiest production workspace. Sample sizes: n=79 sequential and 80 with
4 clients, so p99 is the maximum; n=112 with 16 clients, so p99 is the
second-largest sample. Shared buffers (128 MB) hold less than the audit table,
so bucket calls read it from the VM page cache. The isolated run was one
host and one workspace with warm buffers. Results vary between runs on the
shared host: an earlier matched isolated run had 2 to 6 times higher
sequential p99 for discovery and timeline routes. Hybrid discovery and
provider time were not measured.
