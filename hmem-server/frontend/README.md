# Frontend validation

Use Node.js 20 LTS.

For a clean browser-test installation, run:

```sh
npm ci
npm run test:browser:install
npm test
```

`test:browser:install` provisions the exact Chromium revision required by the locked Playwright version. The timeline browser test starts from a new, empty browser context and runs the compiled production Elm fixture without authentication or backend dependencies.

## Expanded hierarchy loading

Navigation transports 50 summaries per kind per request. Expanded nodes whose
ancestors are also visible and expanded continue automatically, including the
default expanded state and Expand All. Root lists still load through explicit
navigation demand. The branch queue is fair and admits at most four physical
HTTP requests. Collapse, filter, workspace, and session changes retire logical
generations; a stale response or error still releases its physical admission.
Session resets preserve those admissions until completion.

Project and task streams finish independently. Same-filter branch refreshes
stage fresh membership until each kind finishes, preserving its displayed
cache while fresh pages arrive. Ordering or membership invalidation discards
the partial pass and coalesces a restart at offset zero. Errors pause until
retry or reopening; repeated pages without new IDs stop that kind. The client
automatic offset ceiling is 10,000 (the server allows 100,000); reaching it
reports an incomplete branch and preserves `hasMore` rather than claiming an
exhausted stream. The wire `hasMore` contract guarantees full 50-item pages;
offsets advance by transport size while cached counts deduplicate IDs.

The hierarchy is one continuous logical preorder over all cached root and
expanded child summaries. Loading another page adds reachable siblings; root
**Load more** requests the next 50-item transport page directly. Branches
continue automatically, and their end rows expose loading, completion or an
incomplete/error state with Retry.

Rendering mounts at most 25 ordinary rows across the entire viewport, plus a
bounded set of active focus, editor, inline-create, drag and native DOM-focus
pins. Every omitted run has a spacer, including runs around distant pins.
Mounted row wrappers own their spacing and are measured with ResizeObserver;
estimated geometry and mounted-only fallback measurements keep the initial
render bounded when that API is absent. A cached height-sum index handles scroll
and measurement updates without rebuilding the hierarchy. Structural changes
preserve the current row and intrarow scroll anchor when it remains present.
Offscreen focus and keyboard navigation first mount their logical target,
then scroll/focus after the stamped DOM update. Drop boundaries use logical
same-parent siblings, including neighbors outside the viewport.

Detail hydration admits at most six physical requests. Ordinary demand comes
from the globally bounded mounted/overscan set; active targets are prioritized
separately. Deep focus requests the target rather than hydrating or mounting
its entire ancestor chain. Workspace/session/generation/layout stamps reject
stale measurement and focus work. Canonical snapshot and replay behavior
remains governed by its separate transport contract. Production browser and
performance compatibility is validated by the following integration task;
these local layout and bridge tests do not replace those gates.

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
