# Timeline live integration

The shared Audit/History text comparison is covered by pure Elm tests in
`tests/AuditDiffTest.elm` and production Main browser tests in
`tests-js/audit-diff.browser.test.js`. Both run in `npm test`; for a targeted run
use `npx elm-test tests/AuditDiffTest.elm`, then `npm run build` and
`node --test tests-js/audit-diff.browser.test.js` from the frontend directory.
The browser fixture intercepts authorized full-record reads and accepted
canonical transport to exercise frozen snapshots, refresh/retirement races,
error recovery, and native modal keyboard/mobile behavior. It does not qualify
a deployed backend or performance matrix. Routine successful runs retain no
raw logs or screenshots. Task-owned temporary diagnosis, if needed, is contained
in `.audit-diff-scratch-901a79eb`, owned by the implementing task and removed at
task closure; preexisting fixture and performance evidence remains untouched.

Build from `hmem-server/frontend` with explicit loopback endpoints:

```powershell
$env:VITE_HMEM_API_URL = 'http://127.0.0.1:5180'
$env:VITE_HMEM_WS_URL = 'ws://127.0.0.1:5180/api/v1/ws'
$env:VITE_HMEM_AUTH_MODE = 'local'
npm run build
```

In a second terminal at the repository root, start the isolated API:

```powershell
stack run hmem-test-harness -- local --interactive --port 5180
```

Copy the contents of `hmem-server/static` into the printed **Static dir**.
Stop with Enter or Ctrl-C; the harness deletes its sandbox. For Playwright CLI
sessions, fulfill `**/hmem-runtime-config.js` with
`window.HMEM_CONFIG = {};` before navigating so the explicit build URLs apply.

Run the lifecycle matrix from `hmem-server/frontend`:

```powershell
$env:HMEM_TIMELINE_API = 'http://127.0.0.1:5180'
$env:HMEM_REPO_ROOT = (Resolve-Path ../..).Path
npm run test:timeline-live
```

The test deliberately fails when `HMEM_TIMELINE_API` is absent. It checks
planning/Observation lifecycle, audit restoration, cascade counts, and canonical
event/bucket projections. Its real `hmem-mcp` stdio process uses an explicit
`--server-url` and session-local `set_workspace`; it does not change the active
Codex MCP workspace.

Use named Playwright CLI sessions against this same isolated harness for the
two-client request-budget, selected-bucket, background catch-up, disconnect/replay,
and forced-resync checks. Retain the required screenshots, snapshots, network
output, and traces under `output/playwright/<label>/`, with the executing task
owning its label and specifying the retention/cleanup disposition. Preserve
existing required evidence. Performance qualification has separate
[commands and evidence rules](../perf/performance.md).
