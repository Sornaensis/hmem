# Session traffic telemetry

`hmem-server` counts agent-session capture requests and prefix-proof work with
fixed labels. There is no metrics endpoint. Noteworthy proof events are written
as JSON lines to the server log beside [request logging](request-logging.md);
in-process callers and tests read the complete counters through the prefix
registry's `prefixTraffic` and `readSessionTraffic`.

Workspace, session, operation, principal and audience identifiers, paths and
transcript bytes are never recorded. Prefix sizes appear only as the buckets
`le_1mib`, `le_16mib`, `le_128mib`, `le_1gib` and `gt_1gib`.

## Request outcomes

Each append, heartbeat, proof submission and proof lookup is counted once by
route and outcome. HTTP responses are unchanged.

| Route | Outcomes |
| --- | --- |
| `append` | `committed` (with committed bytes), `replayed` |
| `heartbeat` | `accepted` |
| `proof_submit`, `proof_lookup` | `pending`, `verified`, `mismatch`, `failed` |
| `proof_lookup` | `verification_unknown` with a `reason` |
| all | `session_missing`, `unauthorized`, `invalid`, `conflict`, `terminated`, `evidence_unavailable`, `busy`, `unavailable`, `error` |

Rejections are classified from their fixed status and error code. `error`
counts unexpected failures; the request log records them as well.

## Unknown proof operations

Every unknown operation returns the same `404 verification_unknown` response
and cannot certify verification. The registry keeps a bounded ring of the 256
most recently removed operations so the counter and log can say why:

| Reason | Meaning |
| --- | --- |
| `expired` | The terminal result reached its five-minute lifetime. |
| `evicted` | A later admission evicted the terminal result oldest-first. |
| `target_mismatch` | The operation is bound to another workspace, session or audience. |
| `unknown_since_start` | This process never retained the operation and started less than ten minutes ago, the maximum operation lifetime. A restart or a different server process produces this reason. |
| `unknown_operation` | This process never retained the operation, or its record left the ring, and it has run longer than an operation lifetime. |

A live operation that misses its five-minute deadline is not unknown: it is
`failed` with `verification_deadline`.

## Proof compute

Each terminal proof records its result (`verified`, `mismatch`,
`failed_deadline`, `failed_unavailable` or `failed_evidence_unavailable`),
prefix bucket and server compute time from admission to the terminal result,
measured by the registry's deadline clock. Snapshots keep the count, total
prefix bytes, total and maximum milliseconds per result and bucket.

## Log lines

| Event | Fields | Logged |
| --- | --- | --- |
| `session_traffic` | `route`, `outcome`, `reason` | Proof submissions and lookups other than `pending` and `verified`. |
| `session_prefix_proof` | `outcome`, `prefix`, `compute_ms` | Every terminal proof. |

Append and heartbeat outcomes are counted but not logged; their statuses are in
the request log. Unknown, failed, mismatched, busy, unavailable and unexpected
outcomes use level `warn`. At most 256 events wait for emission; overflow and
failed log writes are counted as dropped events without affecting counters.
Closing the registry emits pending events and detaches the log.

## Follower diagnostics

`hmem-mcp` keeps per-lifecycle follower counters in process. With
`HMEM_SESSION_TRAFFIC_DIAGNOSTICS=1`, each follower writes one
`session_follower_traffic` JSON line to the MCP error stream when it reaches a
terminal state. The line contains the terminal status and error code,
`lifetime_ms`, request counts by route (`metadata`, `create`, `append`,
`heartbeat`, `proof_submit`, `proof_poll`, `terminate`) and outcome,
`committed_bytes`, `local_hash_bytes`, `proof_recoveries` and
`transient_retries`. It never contains paths, identifiers or transcript bytes.
The setting is off by default and changes no tool output.
