+++
schema = "adrai/decision/v1"
adr = "A01M39QPB23CYS9QHCWQSATSJE9"
record = "R01M4ZAYD5ET5V53XW8M6RSNJFH"
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

**Consequences**

Browser and MCP operations pass through the same server policy and data boundary. Concurrent MCP calls retain stable workspace scoping and declared attribution, and context changes have an ordered handoff. Identity-less clients must upgrade their startup declaration; explicit workspace IDs do not bypass it. MCP depends on server/HTTP availability, must provide backpressure, and can delay a context switch while earlier accepted requests finish.

**Evidence**

- `hmem-server/src/HMem/Server/API.hs`: `/api/v1` resources and server authorization.
- `hmem-mcp/src/HMem/MCP/Server.hs`: stdio JSON-RPC loop, acceptance-time context snapshot, bounded queue and workers, drain before context control, and overload response.
- `hmem-mcp/src/HMem/MCP/Tools.hs`: HTTP request mapping and workspace injection.
- `hmem-mcp/src/HMem/MCP/Config.hs`: server URL and credential resolution.

**Historical rationale**

The original reason is not established by these sources; this ADR describes the observed implementation and the approved thin-MCP boundary.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0ifSwiYiI6IjI0MWY3NWFiMjAzZDQ0NWJkYWJmZmExYmFhMmJkNzA5ZDUxNjBjY2QiLCJpIjoic2hhMjU2OmNkSFNJOEgyQU1HNDN1QmliRzFiOElnZGE2SnlOT0ZpVHJ0M21JSFk4T1kiLCJrIjoiZGVjaXNpb24uYW1lbmQiLCJvIjoiUjAxTTRaQVlENUVUNVY1M1hXOE02UlNOSkZIIiwib3AiOiJPMDFNNFpBWUQ1RVQ1VjUzWFc4TTZSU05KRkgiLCJwIjpbIlIwMU0zS0ROSkg1VFk3S1JWM1JWWTJWQU5LVyJdLCJyIjoiY29kZXgvZGVjbGFyZWQtYm90LWlkZW50aXR5IiwicyI6InNoYTI1NjpMU2NHOGNCaEIzbzZxWm1kX3d6cGgxZ3VBbTZUb0RzeURmdTFyeTBPUk1ZIiwidCI6MTc5MjA1MzAzMjExMCwidiI6MSwieCI6ImFkcmFpLzEuMC4wIn0 -->
