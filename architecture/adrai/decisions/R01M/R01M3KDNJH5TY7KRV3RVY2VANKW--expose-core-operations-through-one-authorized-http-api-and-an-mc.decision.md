+++
schema = "adrai/decision/v1"
adr = "A01M39QPB23CYS9QHCWQSATSJE9"
record = "R01M3KDNJH5TY7KRV3RVY2VANKW"
title = "Expose core operations through one authorized HTTP API and an MCP adapter"
summary = "Keep MCP as a bounded stdio-to-HTTP client and order workspace-context changes after previously accepted calls."
domains = ["api-boundary"]
+++

**Context**

Browser and LLM clients access the same workspaces, Observations, Projects, and Tasks. MCP requests can be processed concurrently, while the active MCP workspace context can change. This amendment records the ordering rule for that context without changing the server’s authority.

**Decision**

The Haskell server defines the `/api/v1` HTTP contract and handles authorization and persistence for core operations. The MCP process accepts line-delimited JSON-RPC tool calls over stdio and maps them to server HTTP requests, forwarding a configured bearer credential when present. It captures the active workspace context when an ordinary request is accepted, carries that snapshot through a bounded queue and fixed worker pool, and injects the workspace ID when the call does not supply one explicitly.

Treat a workspace-context control as an epoch boundary. Before applying that control, drain previously accepted ordinary requests from the queue and active workers. Ordinary requests within an epoch may execute concurrently. This prevents their externally visible effects from overtaking a later context switch while each request keeps its acceptance-time workspace snapshot. A full queue returns an overload error.

**Consequences**

Browser and MCP operations pass through the same server policy and data boundary. Concurrent MCP calls retain stable workspace scoping, and context changes have an ordered handoff. MCP depends on server/HTTP availability, must provide backpressure, and can delay a context switch while earlier accepted requests finish.

**Evidence**

- `hmem-server/src/HMem/Server/API.hs`: `/api/v1` resources and server authorization.
- `hmem-mcp/src/HMem/MCP/Server.hs`: stdio JSON-RPC loop, acceptance-time context snapshot, bounded queue and workers, drain before context control, and overload response.
- `hmem-mcp/src/HMem/MCP/Tools.hs`: HTTP request mapping and workspace injection.
- `hmem-mcp/src/HMem/MCP/Config.hs`: server URL and credential resolution.

**Historical rationale**

The original reason is not established by these sources; this ADR describes the observed implementation and the approved thin-MCP boundary.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0ifSwiYiI6ImJmZmE4ODU3YjhkZjIyMDE4MGZkMTVjZWMzNzcxNTBmZWM2MGVmMzciLCJpIjoic2hhMjU2OnRoSXdYX3BvNlRUS00xcjRlS2JVOWY1dDYxWlZQY0ZhaFlTUFBUS3ZHMUUiLCJrIjoiZGVjaXNpb24uYW1lbmQiLCJvIjoiUjAxTTNLRE5KSDVUWTdLUlYzUlZZMlZBTktXIiwib3AiOiJPMDFNM0tETkpINVRZN0tSVjNSVlkyVkFOS1ciLCJwIjpbIlIwMU0zOVFQQjU2NlBBOFdFNUcyOFY2UEY4MSJdLCJyIjoibWFzdGVyIiwicyI6InNoYTI1NjpxVnY1UXNSQmRIWlBNNzNzYmZ3SkJEVzR2M04yd2hPY1FpZzBOZ3A0TDAwIiwidCI6MTc5MDU3OTQ5MzQxMywidiI6MSwieCI6ImFkcmFpLzEuMC4wIn0 -->
