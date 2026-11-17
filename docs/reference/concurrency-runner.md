# Concurrency runner

`tools/perftest/concurrency.mjs` drives hundreds of concurrent requests
against the [container sample](request-performance.md#container-sample)
environment and records the evidence needed to name the bottleneck. It reads
the `perftest.mjs` seed state and secrets, needs the `hmem.perftest.revision`
image label, and rejects unknown, repeated or valueless flags before
contacting anything. It exits non-zero unless `/api/v1/health` answers
HTTP 200 within 120 s.

```sh
node tools/perftest/concurrency.mjs --steps 300,500,1000 --label NAME
node tools/perftest/concurrency.mjs --self-test
```

`--self-test` checks latency censoring and status restoration offline.

- **Clients.** Closed loop: each client has one request in flight, no think
  time and its own keep-alive connection. Each step ramps for 15 s
  (`--ramp-s`), holds for 120 s (`--hold-s`), and measures only requests
  started in the hold. The client timeout is 30 s.
- **Workers.** By default, clients run in `hmem-perftest-load-*` containers of
  the pinned node image on `hmem-perftest-net`. The runner directory is
  mounted read-only, the API key is passed in the environment, and the
  containers are removed on exit. `--client local` runs host processes
  through the loopback port instead. On Windows with Node 24.15, host workers
  intermittently crashed natively at 1,000 connections.
- **Mix.** Weighted reads and writes over all six workspaces (`--weights`).
  About 10% are task field and status updates on tasks partitioned per client.
  Field titles are unique per run. Before the first step the runner sets every
  status target to `todo` and stops if one cannot be set. After each step,
  once health recovers or 30 s pass, it puts `todo` on every status target a
  client attempted, whatever the outcome. After an abort or a failed restore it
  puts `todo` on every status target.
- **Discovery and background.** 8 discovery clients per run audience
  (`--discovery-clients`) keep one search session each and wait 1 s after a
  rejection. 32 WebSocket observers in one audience (`--observers`) are kept
  open for the whole run. 8 capture appenders (`--appenders`) write fresh
  sessions and prove each prefix exactly after every step.
- **Outcomes.** Each request is 2xx, 4xx or 429 with its error code,
  `503 server_busy` (with `Retry-After` checked), other 5xx, refused, reset or
  client timeout.
- **Latency.** Every client distribution is reported twice: over 2xx
  responses only (`*_2xx_ms`) and over all outcomes (`*_all_ms`). In the
  all-outcome form a client timeout is censored at the timeout, so a
  percentile that falls on one reads `>=30000`.
- **Evidence.** Per route: client p50/p95/p99 in both forms, and from the
  request log joined by `X-Request-Id`, pool wait, hold, tail wait share and
  transport overhead. The runner also records:
  - the in-flight gauge and completions per client (minimum, mean, Jain index)
  - deltas of `pool`, `delivery_pool` and `http_admission` from the health
    endpoint, polled every 5 s
  - `pg_stat_activity` every 0.5 s, plus `pg_stat_database` and
    `pg_stat_statements` deltas
  - server CPU and RSS, `docker stats`
  - WebSocket delivery per observer, aggregated over observers open through
    the drain, with closed or retired observers listed apart; telemetry, and
    recovery after the load stops
  - host limits: `somaxconn`, open files, RTS options and dynamic ports
- **Validity.** A step whose worker event-loop delay p99 exceeds 50 ms is
  marked client-bound and invalid. The `docker exec` and `psql` calls at the
  hold boundaries are synchronous and block the main process, which
  timestamps WebSocket frames and runs the appenders. They are excluded from
  `main_loop_delay_ms` and reported as `main_blocking_ms`.
- **Output.** One summary JSON in `.tmp/perftest/concurrency`, rewritten after
  every step. The `pg_stat_activity` sampler writes
  `/tmp/hmem-concurrency-<run>-*` in the PostgreSQL container; the runner
  stops it and removes those files on exit.

## Baseline at f206ff72

[`tools/concurrency-baseline.qualification.json`](../../tools/concurrency-baseline.qualification.json)
records one run per step at `f206ff72`, with the production pool defaults
(15 main, 10 delivery) on a fresh volume at the container sample scale. It ran
an earlier runner, before the status precondition, all-outcome percentiles
and per-observer delivery were added. Times are client milliseconds over the
120 s hold. The p50 to p99 columns cover 2xx responses only. The raw samples
are gone, so the all-outcome p99 is derived from the recorded counts, with
each client timeout counted as at least 30 s.

| Clients | Requests/s | 2xx p50 | 2xx p95 | 2xx p99 | Timeouts | All-outcome p99 | Mean connections held | Bucket share of hold | Health |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 300 | 130 | 842 | 8,523 | 13,650 | none | 13,650 | 14.8 of 15 | 61% | degraded |
| 500 | 137 | 1,792 | 11,634 | 19,559 | 28 | at least 19,559 | 14.5 of 15 | 69% | degraded |
| 1,000 | 152 | 4,723 | 17,996 | 25,518 | 261 | at least 30,000 | 14.8 of 15 | 69% | degraded |

- **300 clients.** All 300 stayed in flight and every request completed, but
  only by queueing on the main pool. Pool wait is at least 97% of the slowest
  5% of most routes.
- **Main pool first.** The 15-connection main pool saturates first:
  throughput stays flat while latency grows with the client count.
  Timeline buckets are under 2% of requests but hold 61-69% of pool
  connection time. hmem-server uses under 1 core, PostgreSQL about 15 of 24,
  and the load workers 0.15.
- **No admission control.** `f206ff72` has none. There is no `503` or
  refusal; requests wait until the client times out. The first timeouts are
  at 500 clients, nearly all on navigation batch, which queues once for each
  of its many checkouts. At 1,000 clients, 240 of 714 navigation batch
  requests timed out: its 2xx p50 is 19.0 s, its all-outcome median lies
  between 19.0 and 28.6 s, and its all-outcome p95 is at least 30 s. Health
  reports `degraded` throughout each hold because its non-blocking probe
  finds no idle connection, and recovers 15-18 s after the load stops.
- **Stripes and delivery.** At `f206ff72` (one stripe per capability), stripe
  capacity waits stay at 0, because a stripe-only wait is not counted while
  the whole pool is in demand. Change frames reach observers before the
  writer's response, because write handlers queue for checkouts after commit.
- **Missing frames.** At 300, 41 change frames were missing because the
  baseline runner set no status precondition: status targets that aborted
  runs had left in progress got no-op updates. During the 500 step the server
  retired one healthy observer on its delivery deadline under pool saturation
  (`websocket_turn` `retired_deadline`). This is an open anomaly owned by
  task `fe98a2eb`. Roughly 40 to 55 missing frames at 500 and about 20 at
  1,000 are not attributable to that observer and remain unexplained.

The receipt lists the limits, including the seed changes made by earlier
runner runs and the single principal of local authentication.
