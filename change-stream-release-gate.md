# Canonical change-stream release gate

This appendix binds the executable evidence for the durable outbox → atomic
snapshot/resume → ticket/replay/live server → canonical-only Elm pipeline. It
uses only ephemeral PostgreSQL sandboxes and loopback HTTP/WebSocket endpoints.
No external identity provider, database, or service is part of the gate.

## Gate invariants

- Only a committed canonical outbox record can reach a client. No-op and
  rolled-back writes emit neither a record nor a transport frame.
- A workspace record is visible only to a currently eligible member,
  workspace administrator, or superadmin. Global records require current
  superadmin entitlement. Hidden activity emits no placeholder, checkpoint, or
  token-cadence signal.
- Snapshot start fixes an authorized, redaction-reviewed view at high-water
  mark H. Pagination reuses its stored page size and immutable items. Commits
  after H are replayed before the terminal checkpoint.
- Subscribers see cursor-free schema-v1 frames and opaque tokens. Duplicate or
  reversed delivery is idempotent by `event_id`; a malformed/unknown frame or
  unusable token performs a scoped resync, never a legacy connection or broad
  workspace reload.
- Every worker and socket is cancellable. Harness worker acquisition is
  immediately bracketed, so normal shutdown and any later setup failure stop it
  before pool destruction. Test servers, browser tabs, ports, processes, and
  sandboxes are removed after a run.

## Executable matrix

The quoted phrases are Hspec/Elm/Node test names or stable match fragments.

### A. Writers

| Writer/path | Commit and canonical record | Targeting and convergence evidence |
| --- | --- | --- |
| REST mutation | Server `"holds a paginated HTTP snapshot at H"`; API `"records Observation changes without projecting handler-local payloads"` | Two-client smoke creates a project and task in client B; client A gains exactly one matching card through targeted reconciliation. |
| Direct core/database | Core Project `"captures committed project mutations"` and `"direct matrix"`; server snapshot-H test commits directly after H | The post-H direct project is absent from the immutable snapshot, present through REST read-back, then replayed as the exact durable `event_id`. |
| Audit revert | API `"records a project audit revert with the audit_revert cause"` | The durable envelope carries `transaction.cause=audit_revert`; ordinary project invalidations drive the same client action as other writers. |
| Project/task/dependency/move/membership cascades | Core Project `"records every project cascade"` and `"captures every task cascade"`; core ChangeStream dependency FK cascade, task-move invalidations, and membership invalidations | Two-client smoke deletes a project containing a task; both clients remove the project and cascaded task without a broad reload. |
| MCP-through-REST | MCP `"carries a real MCP proxy mutation through REST"` | The real loopback MCP bridge produces one sanitized project record with trusted `cause=mcp`, the item appears in the canonical snapshot and REST read-back, and browser smoke shows the real MCP-created project in both clients. |
| Rollback/no-op | Core ChangeStream `"does not retain an outbox record when its writer transaction rolls back"`; Project no-op and membership same-value tests | Empty outbox-after assertions prove there is no record available to dispatch or replay. |

### B. Recipients

| Audience | Executable assertion |
| --- | --- |
| Workspace member and administrator | Server `"delivers only to the eligible workspace and global audiences"` opens separate canonical sockets; both receive the same workspace `event_id`. |
| Superadmin global | The same server test opens a canonical global socket and receives the workspace-group record only; core global-superadmin tests reject stale entitlement. |
| Unrelated workspace/user | The unrelated workspace socket's first post-checkpoint frame is its own later project. The hidden workspace-A record produces no frame or checkpoint first. |
| Hidden membership row | Server `"suppresses hidden-only membership activity without a frame or checkpoint"`; exact audience-filtered invalidations are asserted in core. |
| Token/frame cadence | Hidden-record tests preserve the stored bearer expiry and observe no replacement token/checkpoint for hidden-only activity. |

### C. Delivery and recovery

| Scenario | Executable assertion |
| --- | --- |
| Snapshot H plus concurrent commit | Server snapshot-H test paginates with size 1, rejects an explicit size-2 continuation with HTTP 409/internal `SnapshotOutOfOrder`, then succeeds when the continuation omits `page_size`; it commits after page one, proves the new row is absent from snapshot items but present through the API, then receives it in replay. |
| Paginated response-loss recovery | Core `"retries nonterminal and terminal snapshot pages with their identical ranges"`; API retries the same start key byte-for-byte; Node preserves continuation request bodies. |
| Disconnect during replay | Core `"keeps the prior bearer reconnectable until an explicitly acknowledged replay page"`; browser smoke closes both clients, commits through real MCP, then both reconnect and converge. |
| Persisted-token reconnect | Server one-use ticket/replacement-token test and Node latest-token/storage tests; browser reconnect retains the prior project and gains the offline project exactly once. |
| Duplicate/reversed delivery | Elm ChangeStream duplicate, reversed action, stale-generation, and coalescing tests; Node replay batch/checkpoint tests. |
| Listener/dispatcher restart | Server `"replays retained commits after dispatcher restart and cancels workers cleanly"`. |
| Retention/expired token | Core retention-pruned and expiry tests return one of the internal errors; API/WebSocket project all non-authorization causes to the same `resync_required` outcome. Frontend performs scoped resync. |

`resync_required` is deliberately non-oracular: the public HTTP response is
409 and the WebSocket terminal frame contains only schema version and type. It
does not distinguish absent, expired, superseded, retention-pruned, malformed,
or audience-mismatched bearer state.

### D. Authorization controls

| Control | Executable assertion |
| --- | --- |
| Grant/revoke preserving another scope | Server targeted grant, targeted revoke, and cross-scope revoke tests; retained workspace receives a later live change. |
| Disable/re-enable user | Core rejects old bearers while disabled and permits only a fresh session after re-enable. |
| Superadmin change | Core requires current global-superadmin entitlement and invalidates the old global bearer. |
| Selected workspace deletion/restore | Core invalidates snapshot/resume bearers across delete/restore; Elm revoke and workspace-delete reducers clear only the selected scope and preserve unrelated state. |

### E. Frontend budgets

Elm tests cover every V022 invalidation target, strict snapshot/frame decoding,
snapshot replacement, dedupe eviction/persistence, coalescing, stale request
generations, grant/revoke, wrong scope, and scoped resync. Node tests cover
scope-keyed socket isolation, canonical ticket shape, CSPRNG start keys,
pagination, latest-token reconnect, one-use tickets, cancellation, storage
corruption, bounded retry, and disconnect-all.

The budget is one command per normalized target per coalesced checkpoint. An
ordinary project/task/dependency event never calls the broad workspace reload.
The browser transport requests only `/api/v1/change-stream/resync`,
`/api/v1/change-stream/ticket`, and `/api/v1/ws`; it never uses the legacy
ticket route or decodes a legacy frame.

## Loopback browser smoke

The manual smoke used two fresh browser tabs (independent Elm runtimes) against
Vite on `127.0.0.1:5173` and `hmem-test-harness local --interactive` on
`127.0.0.1:58420`. The harness used an ephemeral PostgreSQL database on
`127.0.0.1:55050`, one repository workspace, local synthetic-superadmin auth,
and no external service. Credentials and opaque stream/ticket tokens were not
recorded.

1. Both clients entered the seeded workspace and reported `Connected`.
2. Client B created `Browser REST project`; client A displayed one matching
   project card without a toast or duplicate.
3. Client B created `Browser cascade task`; client A displayed one task under
   that project. A REST cascade delete removed the project and task from both
   clients, which remained connected.
4. A real `hmem-mcp` JSON-RPC `project_create` produced `Browser MCP project`;
   both clients displayed the same single project identity.
5. Both tabs closed. While disconnected, real MCP created
   `Offline replay project`. Two fresh tabs reopened the selected workspace and
   displayed exactly the prior MCP project plus the offline project, once each,
   with `Connected` state.
6. From the global catalogue, client B created `Browser global group`; client A
   received the group and both views showed it once.
7. Both browser consoles had zero warning/error entries.

The browser surface does not expose raw WebSocket capture. Cursor omission,
schema/type, exact `event_id`, checkpoint ordering, no hidden frame, and
canonical-only route assertions therefore come from the executable
server/Elm/Node tests above rather than visual inference. The two tabs had
independent Elm/socket runtimes but shared the in-app browser profile; separate
storage-profile isolation comes from the deterministic Node multi-socket tests,
not this manual pass. The local harness has one synthetic principal, so
multi-principal grant/revoke is exercised by deterministic core/server socket
tests instead of the manual browser pass. External Keycloak mode remains the
pre-existing opt-in case and is excluded from this isolated release gate.

Cleanup was complete: MCP, Vite, and harness processes exited; the harness
sandbox was removed; no listeners remained on ports 5173, 58420, or 55050; and
all agent-created browser tabs were closed.

## Validation commands

Run from the repository root unless a directory is shown:

```text
stack test hmem-core:hmem-core-test
stack test hmem-server:hmem-server-test
stack test hmem-mcp:hmem-mcp-test
cd hmem-server/frontend
npm test
npm run build
```

Recorded isolated run:

- `hmem-core:hmem-core-test`: 161 passed, 0 failed.
- `hmem-server:hmem-server-test`: 89 passed, 0 failed, 2 pre-existing
  opt-in cases pending.
- `hmem-mcp:hmem-mcp-test`: 46 passed, 0 failed (588.23 seconds); the
  focused `carries` integration test also passed independently.
- Frontend: 93 Elm tests and 19 Node tests passed; the optimized Vite build
  succeeded. Vite retained its existing non-fatal warning that the runtime
  configuration script is intentionally not bundled without `type="module"`.
- `hmem-server:exe:hmem-test-harness` built successfully with `--fast`.

Focused release checks may be selected with Hspec matches `holds`, `eligible`,
and `carries`. The browser smoke uses `hmem-test-harness -- local --interactive
--port 58420`, Vite with explicit loopback API/WebSocket URLs, and the real
`hmem-mcp --server-url` bridge. Port numbers are illustrative; each run must
first prove its chosen ports are free.
