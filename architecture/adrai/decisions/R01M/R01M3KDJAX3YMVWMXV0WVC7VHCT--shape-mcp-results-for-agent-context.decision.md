+++
schema = "adrai/decision/v1"
adr = "A01M3KDJARNRK4M33PDRS42AGV4"
record = "R01M3KDJAX3YMVWMXV0WVC7VHCT"
title = "Shape MCP results for agent context"
summary = "Return operation-specific acknowledgements and compact Observation summaries while keeping explicit detail retrieval."
domains = ["mcp-response-contract"]
+++

**Context**

MCP tools call the authorized HTTP API, but forwarding every HTTP response verbatim spends agent context on repeated fields, timestamps, and content the caller just supplied. The existing HTTP/MCP ADR establishes transport and policy ownership; it does not establish MCP output shapes.

**Decision**

Give MCP its own operation-specific result projection. Mutations return acknowledgements with action, entity type, identifiers, and useful summary/status fields. Observation list, match, search, and similarity results return provenance and a bounded content preview; `observation_get` returns full content when explicitly requested. Keep pagination and match evidence that tell a caller how to continue or why an Observation matched. Preserve structured errors.

This records the current shaping boundary, not a universal response-size guarantee. The current Project and Task summary shapers still retain descriptions, including in some aggregate responses. Any future reduction of those fields is a separate contract change that must leave a way to retrieve the full durable specification.

**Consequences**

Common agent operations use fewer low-value tokens while detail remains available. MCP response DTOs differ from REST DTOs and need their own regression fixtures. Large Project or Task descriptions can still enlarge current aggregate responses.

**Evidence**

- `hmem-mcp/src/HMem/MCP/Tools.hs`: HTTP dispatch, mutation acknowledgements, Observation preview/detail projections, Project/Task shapers, and pagination fields.
- `hmem-mcp/test/HMem/MCP/ToolsSpec.hs` and `hmem-mcp/test/fixtures/mcp-compact-responses.json`: compact-output regression checks.
- Completed hmem project `9b1826d3-8bf5-42d6-8ddc-7baae246eb6c`: explicitly aims for operation-appropriate, signal-only MCP outputs.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0ifSwiYiI6IjE3NmQ2YzdjMjljN2E1MmUxNTA0Yjc4NDFhY2UxOThiNzA4MTU0ODAiLCJpIjoic2hhMjU2OmZ6NVF3TU9ZOGdlOExPOTktRnVzNFVrZzhJTzhuSWpVdy1yaDdKNkdXR0UiLCJrIjoiZGVjaXNpb24uY3JlYXRlIiwibyI6IlIwMU0zS0RKQVgzWU1WV01YVjBXVkM3VkhDVCIsIm9wIjoiTzAxTTNLREpBWDNZTVZXTVhWMFdWQzdWSENUIiwiciI6Im1hc3RlciIsInMiOiJzaGEyNTY6N3JsR3BTbFAtRFNZbGtldE9Yb0pRaU1YSG1pNU40WUFIekhNZ0R1ZDFzNCIsInQiOjE3OTA1NzkzODcyOTksInYiOjEsIngiOiJhZHJhaS8xLjAuMCJ9 -->
