+++
schema = "adrai/decision/v1"
adr = "A01M3KDJARNRK4M33PDRS42AGV4"
record = "R01M3KP2DJPHCD67F6TBSZZNQSP"
title = "Shape MCP results for agent context"
summary = "Return compact Project and Task aggregates and acknowledgements while keeping complete descriptions in detail tools."
domains = ["mcp-response-contract"]
+++

**Context**

MCP tools call the authorized HTTP API. Project and Task descriptions are durable specifications that can be long. Repeating them in search, overviews, candidate lists, and mutation replies consumes agent context and can clip results. The existing HTTP/MCP ADR establishes transport and policy ownership; this decision establishes MCP output shapes.

**Decision**

Give MCP its own operation-specific result projection. Project and Task aggregate records and mutation acknowledgements contain compact summaries without descriptions. Summaries retain full identifiers, names or titles, status, priority, hierarchy, project, due date, and applicable readiness or continuation fields. `project_detail` and `task_detail` return the complete stored description and full identifiers. Task mutation acknowledgements retain compact `dependency_effects`, including status and auto-blocking changes. Dependency mutation replies likewise keep compact affected-task records.

Observation list, match, search, and similarity results return provenance and a bounded content preview; `observation_get` returns full content when explicitly requested. Keep pagination and match evidence that tell a caller how to continue or why an Observation matched. Preserve structured errors. This is an operation-specific shape contract, not a universal response-size guarantee for unrelated fields.

**Consequences**

Project and Task aggregate response size is independent of description length while focused detail remains lossless. MCP response DTOs differ from REST DTOs and need regression fixtures and dispatch coverage, including null hierarchy, full UUIDs, and dependency status changes.

**Evidence**

- `hmem-mcp/src/HMem/MCP/Tools.hs`: HTTP dispatch, mutation acknowledgements, and Project/Task summary and detail projections.
- `hmem-mcp/test/HMem/MCP/ToolsSpec.hs` and `hmem-mcp/test/fixtures/mcp-compact-responses.json`: compact-output regression checks.
- Completed hmem project `9b1826d3-8bf5-42d6-8ddc-7baae246eb6c`: operation-appropriate MCP outputs.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0ifSwiYiI6IjUwM2Q0NWZiY2Y1Yjg4ZWVkNzVmYWM3MjBkYTZmNjY4ODI3Y2IyZGMiLCJpIjoic2hhMjU2OjhqTF9MR0FpT1NacG1vR2JQbjJweHBpNk83cG03VVlNYzA4dnVzU0lTRGsiLCJrIjoiZGVjaXNpb24uYW1lbmQiLCJvIjoiUjAxTTNLUDJESlBIQ0Q2N0Y2VEJTWlpOUVNQIiwib3AiOiJPMDFNM0tQMkRKUEhDRDY3RjZUQlNaWk5RU1AiLCJwIjpbIlIwMU0zS0RKQVgzWU1WV01YVjBXVkM3VkhDVCJdLCJyIjoibWFzdGVyIiwicyI6InNoYTI1Njplb19IaE5SWjMtbWNpdlFCaWhsUTFtb2JrcWlhNlVib3Z5Y3NrZTVadTY0IiwidCI6MTc5MDU4ODMwMjkzNCwidiI6MSwieCI6ImFkcmFpLzEuMC4wIn0 -->
