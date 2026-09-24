+++
schema = "adrai/decision/v1"
adr = "A01M39QPB23CYS9QHCWQSATSJE9"
record = "R01M39QPB566PA8WE5G28V6PF81"
title = "Expose core operations through one authorized HTTP API and an MCP adapter"
summary = "Keep MCP as a bounded stdio-to-HTTP client of the server rather than a second database API."
domains = ["api-boundary"]
+++

Context: Browser and LLM clients access the same workspaces, Observations, projects, and tasks. This record describes the current implementation; the historical reason for the choice is not established by these sources.

Observed decision: The Haskell server defines the /api/v1 HTTP contract and handles authorization and persistence for core operations. The MCP process accepts line-delimited JSON-RPC tool calls over stdio and maps them to server HTTP requests, forwarding a configured bearer credential when present. It captures the active workspace context when an ordinary request is accepted, carries that snapshot through its bounded queue and fixed worker pool, and injects the workspace ID when the call does not supply one explicitly.

Consequences: Browser and MCP operations pass through the same server policy and data boundary. MCP depends on the server and HTTP transport being available and must manage concurrent requests and backpressure; a full queue returns an overload error.

Evidence:
- hmem-server/src/HMem/Server/API.hs: defines /api/v1 resources and applies server authorization to workspace and entity handlers.
- hmem-mcp/src/HMem/MCP/Server.hs: implements the stdio JSON-RPC loop, workspace snapshot, bounded queue, fixed workers, and overload response.
- hmem-mcp/src/HMem/MCP/Tools.hs: maps tool calls to /api/v1 HTTP requests and forwards a configured bearer credential.
- hmem-mcp/src/HMem/MCP/Config.hs: resolves the target server URL and forwarded credential sources.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0ifSwiYiI6IjExNjEyNDI0MWZmZGI4Y2MyZmRmZDcwZjc3ZWQ5YjE5OWFhZmI3YTQiLCJpIjoic2hhMjU2OmVTTjlyREFiTlBSckxYNXNhNkhxTkQ3NjQyZFRQY3pyNTVkempkYjVfMDAiLCJrIjoiZGVjaXNpb24uY3JlYXRlIiwibyI6IlIwMU0zOVFQQjU2NlBBOFdFNUcyOFY2UEY4MSIsIm9wIjoiTzAxTTM5UVBCNTY2UEE4V0U1RzI4VjZQRjgxIiwiciI6Im1hc3RlciIsInMiOiJzaGEyNTY6NVpIWXh0d2FoS3FIMWc0LTY3dnYteWdOckdMR2pPaElTMWtHTVMtTl9QOCIsInQiOjE3OTAyNTQ0NjAwNzAsInYiOjEsIngiOiJhZHJhaS8xLjAuMCJ9 -->
