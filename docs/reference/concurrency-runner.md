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

`--self-test` checks latency censoring, status restoration, outcome classes,
health deltas, recovery polling (with a fake clock), argument rejection, slow
sender trickling and the slow-client transport. The transport checks use an
in-process server on 127.0.0.1; nothing else is contacted.

## Flags

Integers are decimal. Invalid arguments exit with status 2.

| Flag | Default | Meaning |
| --- | --- | --- |
| `--steps` | `300,500,1000` | Clients per step, 1–2000 each. Every step needs more clients than `--discovery-clients`. |
| `--ramp-s`, `--hold-s`, `--cooldown-s` | 15, 120, 30 | Ramp (0–300), measured hold (5–1800) and pause between steps (0–600). |
| `--timeout-s` | 30 | Client timeout, 1–300. |
| `--client` | `network` | `network` workers or `local` host workers. |
| `--workers` | `auto` | Worker processes, 1–32. `auto` uses one per 125 clients, including slow clients, from 2 to 16. |
| `--discovery-clients`, `--discovery-audiences` | 16, 1 | Discovery clients (0–256) and the audiences they share (1 to the client count). |
| `--observers`, `--observer-audiences` | 200, 1 | WebSocket observers (0–1000) and the audiences they share (1 to the observer count). |
| `--slow-clients` | 10% of the step, at most 200 | Slow clients per step, 0–1000, in addition to the step's clients. |
| `--appenders` | 8 | Capture appenders, 0–32. |
| `--weights` | built in | `route=weight` pairs for the mix routes. |
| `--seed`, `--label` | 1, revision prefix | Mix seed, and the summary file label (1–64 letters, digits, `.`, `_` or `-`). |

## Behaviour

- **Clients.** Closed loop: each client has one request in flight, no think
  time and its own keep-alive connection. Each step runs an optional probe,
  ramps, holds, and measures only requests started in the hold.
- **Workers.** By default, clients run in `hmem-perftest-load-*` containers of
  the pinned node image on `hmem-perftest-net`. The runner directory is
  mounted read-only, the API key is passed in the environment, and the
  containers are removed on exit. `--client local` runs host processes
  through the loopback port instead. On Windows with Node 24.15, host workers
  intermittently crashed natively at 1,000 connections.
- **Mix.** Weighted reads and writes over all six workspaces (`--weights`).
  About 10% are task field and status updates on tasks partitioned per client.
  Field titles are unique per run. Before the first step the runner sets every
  status target to `todo` and stops if one cannot be set. After each step's
  drain and recovery polling, it puts `todo` on every status target a client
  attempted, whatever the outcome. After an abort or a failed restore it
  puts `todo` on every status target.
- **Discovery.** Discovery clients share run-scoped audience identities, keep
  one search session each and wait 1 s after a rejection.
- **Observers.** Observers open 16 at a time before the first step, spread
  over their audiences, and stay open for the whole run. A failed resync,
  ticket or upgrade is recorded on the observer and the run continues. The
  WebSocket API does not expose the status of a failed handshake, so a non-101
  answer and a network failure both count as an upgrade rejection.
- **Appenders.** Capture appenders write fresh sessions from the ramp start to
  the hold end and prove each prefix exactly after every step. A busy proof
  submission (`503` `verification_busy`) is retried every second for up to
  30 s.
- **Slow clients.** Half are senders, half readers, each on its own raw
  connection with `Connection: close`, one request at a time, waiting 1 s after
  a failure. Senders send the request line and headers of a read-only search
  on connecting, so the server has the request while its JSON body follows in
  5 parts 5 s apart; the last part leaves 25 s after connecting, below Warp's
  60 s inactivity timeout. Readers read evidence ranges of up to the server's
  1 MiB maximum from the 64 largest terminated `seed-sessions` transcripts of
  at least 64 KiB, at no more than 32 KiB/s. The socket's stream high-water
  mark is 16 KiB, but Node reads from the kernel in allocations of up to
  64 KiB, so about 80 KiB can be buffered in the process before TCP flow
  control holds the rest back; Node cannot set the kernel receive buffer.
  Before the first step each candidate session gets a 1-byte evidence read
  with a slow-client bot identity. A bot may read evidence only when every
  task linked to the session is done or cancelled, so sessions that fail are
  dropped (counted in `config.slow`). Without any left, readers read a
  200-task list page. Slow requests time out at `--timeout-s` plus 60 s.
- **Probe.** Before each step's ramp, the slow clients run alone for 15 s,
  starting over the first 5 s. The runner samples the main pool's
  `active_connections` every second. It is expected to stay about 0, because a
  slow client should hold no pool connection while it sends or reads. Steps
  without slow clients have no probe.
- **Outcomes.** Each request is `2xx` (status 200–299 only), its status with
  its error code (`429:` and `5xx:` prefixed, for example
  `5xx:503:database_unavailable`; a response without a parsable status is
  `0`), refused, reset (including a connection closed before a complete
  response), client timeout or `error:<code>` for another network error. No
  status or code has a class of its own.
- **Latency.** Every client distribution is reported twice: over 2xx
  responses only (`*_2xx_ms`) and over all outcomes (`*_all_ms`). In the
  all-outcome form a client timeout is censored at the timeout, so a
  percentile that falls on one reads, for example, `>=30000`. Slow requests
  are excluded from the mix percentiles and fairness but included in every
  outcome and error count.
- **Validity.** A step whose worker event-loop delay p99 exceeds 50 ms is
  marked client-bound and invalid. The `docker exec` and `psql` calls at the
  hold boundaries are synchronous and block the main process, which
  timestamps WebSocket frames and runs the appenders. They are excluded from
  `main_loop_delay_ms` and reported as `main_blocking_ms`.
- **Recovery.** Clients and appenders stop issuing at the hold end. Once the
  hold-end counters are read (`main_blocking_ms.hold_end` after the hold end),
  the runner polls health about every second, concurrently with the drain of
  in-flight requests, including the slow cohort's. A poll is idle when health
  is `ok` and neither pool has a connection checked out. Times count from the
  hold end (`*_after_hold_ms`, recorded within 30 s of it) and from the drain
  end (`*_after_drain_ms`, from polls at or after it, within 30 s of it).
  Polling stops when both idle times are known or their windows have passed.
  The `fe98a2eb` recovery gate is `idle_after_hold_ms`: slow clients hold no
  pool connection while they send or read, so their drain does not delay it
  beyond the brief checkout of a slow sender's completed request.

## Output

One summary JSON in `.tmp/perftest/concurrency` (`PERFTEST_OUT`), rewritten
after every step, whose path is printed at the end. The `pg_stat_activity`
sampler writes `/tmp/hmem-concurrency-<run>-*` in the PostgreSQL container;
the runner stops it and removes those files on exit.

The run records `config` (including the slow cohort's pacing, read target,
evidence candidates, dropped sessions and their failure classes), host
`limits`, the `status_precondition`,
`observers` (requested, audiences, opened, `upgrade_rejections` and failures
by phase and class), any `error` or `abort_status_restore`, and `steps`. Each
step has:

| Section | Contents |
| --- | --- |
| `clients` … `slowReaders`, `workers`, `worker_env`, `clock_*` | Step shape and worker clocks. |
| `window`, `valid`, `invalidReason` | Probe, ramp, hold and drain times, and validity. |
| `inFlight`, `requests`, `throughput_rps`, `mix_requests`, `mix_throughput_rps`, `outcomes`, `non_2xx_with_retry_after` | The hold's in-flight gauge (slow requests included), request count and throughput over all cohorts and over the mix alone, and outcome classes. |
| `cohort_errors` | Per cohort (`mix`, `discovery`, `slow_send`, `slow_read`, `client_error`, `capture`, `proof`), every request of the step from the probe to the proofs: requests, non-2xx, refused, reset, timeout, 429, other 4xx (429 excluded), 5xx and all classes. Other network errors and statuses appear only in `non_2xx` and `classes`. `proof` covers every proof submit and poll, including busy submissions, and counts `busy_retries`. |
| `pooled_*`, `light_*`, `heavy_*` | Mix latency in both forms. |
| `routes` | Per route, including the slow cohort: client latency in both forms and outcomes; from the request log joined by `X-Request-Id`, server time, pool wait, hold, checkouts, tail wait share and transport overhead. |
| `probe` | The main-pool and delivery-pool `active_connections` samples, their health status and the slow requests started in the probe. |
| `slow` | Slow cohort size, timeout and read target, with outcomes, latency, response bytes and read rate per kind, over the whole step and over the hold. |
| `fairness` | Completions per mix client: minimum, mean, maximum, Jain index and clients below half the mean. |
| `client` | Loop delays, CPU, connections opened and the status restore. |
| `server` | Deltas of `pool` and `delivery_pool` from the health endpoint over the hold, including `checkout_timeouts` (null when the server does not report it); `checkout_timeouts_step` from the probe start to recovery; active maxima, health probes, RSS, threads, CPU, `docker stats` and restarts. Other health fields, such as an older server's `http_admission`, are ignored. |
| `postgres` | `pg_stat_activity` every 0.5 s, `pg_stat_database` and `pg_stat_statements` deltas. |
| `websocket` | Upgrade rejections, never-opened and retired observers; delivery from commit to frame aggregated over observers open through the drain, with the others listed apart; frame types. |
| `capture` | Appender latency and outcomes over the hold, proof states and `proof_busy_retries`. |
| `recovery` | `drain_after_hold_ms`; `ok_after_hold_ms`, `idle_after_hold_ms`, `ok_after_drain_ms` and `idle_after_drain_ms` (null when not seen within the window); the number of polls and the last poll's `pool_active` and `delivery_active`. |
| `websocket_events`, `log` | WebSocket telemetry from the server log and the log join rate. |

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
