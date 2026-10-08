# Timeline live integration

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
[commands and evidence rules](../perf/README.md).
