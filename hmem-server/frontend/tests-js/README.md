# Timeline live-refresh integration

Build the final frontend with explicit loopback API/WebSocket URLs from
`hmem-server/frontend`:

```powershell
$env:VITE_HMEM_API_URL = 'http://127.0.0.1:5180'
$env:VITE_HMEM_WS_URL = 'ws://127.0.0.1:5180/api/v1/ws'
$env:VITE_HMEM_AUTH_MODE = 'local'
npm run build
```

In a second terminal at the repository root, start the isolated interactive
harness and note the printed `Static dir` path:

```powershell
stack run hmem-test-harness -- local --interactive --port 5180
```

Copy `hmem-server/static` into that printed sandbox static directory. The
harness deletes its sandbox when Enter or Ctrl-C stops it. For Playwright CLI
sessions, fulfill `**/hmem-runtime-config.js` with
`window.HMEM_CONFIG = {};` before navigating; the explicit build-time URLs
remain authoritative. Then run the lifecycle matrix from
`hmem-server/frontend`:

```powershell
$env:HMEM_TIMELINE_API = 'http://127.0.0.1:5180'
$env:HMEM_REPO_ROOT = 'D:\Projects\hmem'
npm run test:timeline-live
```

The live test deliberately fails without `HMEM_TIMELINE_API`. It covers
project/task create, update, complete, soft-delete and audit restore;
observation create/hard-delete; project/task cascade counts; authoritative
event and bucket projections; and a real `hmem-mcp` stdio process launched with
an explicit `--server-url` and session-local `set_workspace` call. This does not
read or redirect the active Codex MCP workspace context.

The two-client request-budget, selected-bucket, background catch-up,
disconnect/replay, and forced-resync checks are run with named Playwright CLI
sessions against the same isolated harness. Store their screenshots, snapshots,
network output, and traces under `output/playwright/<label>/`.
