+++
schema = "adrai/decision/v1"
adr = "A01M39QPB23CYS9QHCWQSATSJE9"
record = "R01M5MX2KH95T5B3RR541NQF01T"
title = "Expose core operations through one authorized HTTP API and an MCP adapter"
summary = "Keep MCP as a bounded stdio-to-HTTP client and order workspace-context changes after previously accepted calls."
domains = ["api-boundary"]
+++

**Context**

Browser and LLM clients access the same workspaces, Observations, Projects, and Tasks. MCP requests can be processed concurrently, while their active workspace and declared bot identity can change. This amendment records startup, attribution, and ordering rules for that context without changing the server's authority.

**Decision**

The Haskell server defines the `/api/v1` HTTP contract and handles authorization and persistence for core operations. The MCP process accepts line-delimited JSON-RPC tool calls over stdio and maps them to server HTTP requests, forwarding a configured bearer credential when present. After initialization, require `set_workspace` with both `workspace_id` (UUID or explicit null) and `identity` containing exactly the validated `role` and `model-id` strings before any ordinary HTTP-forwarding tool, including global workspace tools and calls with an explicit workspace ID. A null workspace permits declaration before workspace discovery or registration. Validate the whole declaration before atomically replacing the complete workspace/identity context; invalid declarations change neither value. Setting a null workspace clears only the workspace and accepts the supplied identity. Repeating `initialize` preserves both values, and process exit forgets them. Protocol/tool discovery and local context controls remain available before declaration.

Capture the complete workspace/identity context when an ordinary request is accepted, carry that snapshot through a bounded queue and fixed worker pool, and inject the workspace ID when the call does not supply one explicitly. Both HTTP forwarding paths send one `X-HMem-Bot-Identity` header with the accepted declaration. Existing bearer credentials and authenticated MCP provenance forwarding remain unchanged. Declaration metadata is self-reported attribution, not authentication or authorization: tokenless local bootstrap remains supported, and role/model values grant no permissions. The HTTP server validates declarations and resolves effective actor IDs; MCP context responses report accepted metadata without claiming a server-resolved actor ID.

Treat a workspace/identity context control as an epoch boundary. Before applying that control, drain previously accepted ordinary requests from the queue and active workers. Ordinary requests within an epoch may execute concurrently. This prevents their externally visible effects from overtaking a later context switch while each request keeps its acceptance-time workspace/identity snapshot. A full queue returns an overload error.

Explicit local uploader registration is an ordinary admitted request that creates process-owned background work with immutable captured workspace, declared identity, credentials and forwarding configuration. After registration, follower jobs run outside the ordinary request queue, active-request accounting and epoch drains. Later context controls neither retarget nor await them. Keep a bounded registry, fixed fair workers, a shared global bound across every follower HTTP request, finite request/control/retry deadlines and finite shutdown cleanup. Ordinary MCP calls, including the separate explicit session termination operation, remain independent of follower HTTP slots.

Followers use the authorized HTTP API for metadata, prefix verification, append and heartbeat. Read only explicitly supplied regular files through stable opened-handle identity and compare the current path's opened identity. Verify the complete committed prefix before resume and continue bounded verification sweeps while following. Replacement, truncation and observed committed-prefix mismatch stop the job without skipping or overwriting server bytes. Preserve raw UTF-8/JSONL bytes and CRLF; hold partial tails. Flush and stop capture one finite source boundary and return metadata-only receipts after complete verification, or explicit pending status while bounded work continues. A receipt certifies the canonical committed prefix and observed source verification; it cannot prove absence of arbitrary external mutate-and-restore writes between observations.

Uploader start, caught-up state, EOF, flush, stop, failures and process shutdown never terminate a session. Only the normal explicit management operation seals a verified already committed server prefix. A follower observing terminated state retires safely without later append or heartbeat mutation. Process loss leaves the server session active and requires explicit resume. Close terminal file handles, bound retained terminal receipts and cancel/join owned workers within a finite shutdown budget; report cleanup failure truthfully.

**Consequences**

Browser and MCP operations pass through the same server policy and data boundary. Concurrent MCP calls retain stable workspace scoping and declared attribution, and context changes have an ordered handoff. Identity-less clients must upgrade their startup declaration; explicit workspace IDs do not bypass it. MCP depends on server/HTTP availability, must provide backpressure, and can delay a context switch while earlier accepted requests finish.

**Evidence**

- `hmem-server/src/HMem/Server/API.hs`: `/api/v1` resources and server authorization.
- `hmem-mcp/src/HMem/MCP/Server.hs`: stdio JSON-RPC loop, acceptance-time context snapshot, bounded queue and workers, drain before context control, and overload response.
- `hmem-mcp/src/HMem/MCP/Tools.hs`: HTTP request mapping and workspace injection.
- `hmem-mcp/src/HMem/MCP/Config.hs`: server URL and credential resolution.

**Historical rationale**

The original reason is not established by these sources; this ADR describes the observed implementation and the approved thin-MCP boundary.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0iLCJtIjoiY29kZXg6Z3B0LTYuMS1zb2w6aGlnaCJ9LCJiIjoiZjhhZDUzZDk3MjcwNWM0MmE2NDI4Y2YzZThiN2ZkYmYwM2YzZjM0NCIsImkiOiJzaGEyNTY6c1hQOHFMTEZrQmZOUFZPWFRxekp1SDRfSWpLWlZzSWEwd1ZTLXMzVm9XMCIsImsiOiJkZWNpc2lvbi5hbWVuZCIsIm8iOiJSMDFNNU1YMktIOTVUNUIzUlI1NDFOUUYwMVQiLCJvcCI6Ik8wMU01TVgyS0g5NVQ1QjNSUjU0MU5RRjAxVCIsInAiOlsiUjAxTTRaQVlENUVUNVY1M1hXOE02UlNOSkZIIl0sInIiOiJtYXN0ZXIiLCJzIjoic2hhMjU2OkRxTUFwaGowT3p5NV9MXzExRVNJSTl4azZBU0h3V0F3VlRZbm9PcGZaQWMiLCJ0IjoxNzkyNzc2Njg3MTQ1LCJ2IjoxLCJ4IjoiYWRyYWkvMS4wLjAifQ -->
