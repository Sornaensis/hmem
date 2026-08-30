# Frontend validation

Use Node.js 20 LTS.

For a clean browser-test installation, run:

```sh
npm ci
npm run test:browser:install
npm test
```

`test:browser:install` provisions the exact Chromium revision required by the locked Playwright version. The timeline browser test starts from a new, empty browser context and runs the compiled production Elm fixture without authentication or backend dependencies.

## Large-workspace performance baseline

The versioned harness in `perf/` runs the real production `Main` application in
the locked Chromium while Playwright intercepts HTTP and canonical WebSocket
transport. It needs no backend or external network. Its fixed seed produces a
small fixture (10 projects, 40 top-level tasks plus 20 subtasks, 24 dependency
edges, 60 Observations, 40 timeline events, and 20 timeline buckets) and a large
fixture (250 projects, 1,000 top-level tasks plus 500 subtasks, 1,200 dependency
edges, 2,000 Observations, 1,000 events, 500 buckets, and 50 mixed live frames).

```sh
npm run perf:self-check
npm run perf:record
npm run perf:check
```

`perf:record` explicitly authorizes a production rebuild and baseline
replacement, takes two warmups and five measured runs at each scale, preserves
the actual evaluation in `perf/baseline.v1.json`, and exits successfully when
only target budgets fail. `perf:check` never writes: before opening Chromium it
requires the exact fixture, configuration, and full-budget hashes recorded in
the baseline and trace manifest, then repeats the measurement and exits nonzero
on a budget violation. Each named interaction has five samples, its own
nearest-rank p95, and a per-run `{ count, bytes, routes, routeBytes }` request
delta retained in both raw and aggregate evidence; the live batch does too.
Request, byte, DOM, rendered-row, selector/effect, and live-
reload budgets are exact. Timing and representative retained-heap budgets are
enforced only when the current OS/hardware/tool/browser fingerprint matches the
recorded baseline; on a different machine they are printed as informational.
Workspace readiness comes from complete current-protocol transport and stable
representative anchors, not from requiring every entity in the DOM. The current
resync route faithfully transports all 155 small items in two pages and all
4,951 large items in 50 pages. Future bounded readiness stays dormant until the
application invokes an actual bounded protocol. Direct focus uses a fresh,
focus-first session while the untouched canonical resync is paused before its
first response, leaving an empty coherent state. The target is a root project,
so a focus-triggered entity response needs no missing ancestry. The harness
records whether the product requests and renders it within 750 ms, then releases
and verifies the complete byte/order-identical snapshot; no invalidation
prehydrates the target. Live
settlement permits zero follow-ups and excludes a separately enforced 500 ms
no-in-flight-or-late-request window.

Fixture self-checks also freeze production route fidelity: canonical snapshot
kind-rank/identity order (including dependency pairs), capped list pagination,
Project/Task DTO and overview ordering, Observation token-AND search and facet
ordering, newest-first Timeline events, and requested UTC day/week/month/quarter
bucket aggregation. The 20/500 backing bucket-source rows remain exact, while a
fixed browser clock makes the real default weekly request return 13 ascending
rows instead of rendering the entire backing set. The measured Observation
query deliberately has one known result, so its ID and paging are deterministic
without pretending to benchmark PostgreSQL ranking.

The browser fixture isolates frontend scaling and transport amplification; it
does not measure database or network latency. Generated Playwright traces live
under ignored `perf/.artifacts/` and are removed after record verification. The
checked baseline retains the raw five-run samples, per-scenario aggregates,
environment/tool versions, and fixture/configuration/budget hashes;
`perf/trace-manifest.v1.json` retains the twice-verified trace hash, size,
capture point, and explicit non-retention policy.
