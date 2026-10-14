+++
schema = "adrai/decision/v1"
adr = "A01M3KDJARNRK4M33PDRS42AGV4"
record = "R01M4XWPQK1P7CY5E5Y9V0AZMX9"
title = "Shape MCP results for agent context"
summary = "Return compact Project and Task aggregates and acknowledgements while keeping complete descriptions in detail tools."
domains = ["mcp-response-contract"]
+++

**Context**

MCP tools call the authorized HTTP API. Project and Task descriptions are durable specifications that can be long. Repeating them in search, overviews, candidate lists, and mutation replies consumes agent context and can clip results. The existing HTTP/MCP ADR establishes transport and policy ownership; this decision establishes MCP output shapes.

**Decision**

Give MCP its own operation-specific result projection. Project and Task aggregate records and mutation acknowledgements contain compact summaries without descriptions. Summaries retain full identifiers, names or titles, status, priority, hierarchy, project, and applicable readiness or continuation fields. Project and Task summaries and detail results, including subtasks and tasks embedded in Project responses, omit modeled due scheduling fields. `project_detail` and `task_detail` return the complete stored description and full identifiers. Task mutation acknowledgements retain compact `dependency_effects`, including status and auto-blocking changes. Dependency mutation replies likewise keep compact affected-task records.

Observation list, match, search, and similarity results return provenance and a bounded content preview; `observation_get` returns full content when explicitly requested. Keep pagination and match evidence that tell a caller how to continue or why an Observation matched. Preserve structured errors. This is an operation-specific shape contract, not a universal response-size guarantee for unrelated fields.

**Consequences**

Project and Task aggregate response size is independent of description length while focused detail remains lossless. MCP response DTOs differ from REST DTOs and need regression fixtures and dispatch coverage, including null hierarchy, full UUIDs, and dependency status changes.

**Evidence**

- `hmem-mcp/src/HMem/MCP/Tools.hs`: HTTP dispatch, mutation acknowledgements, and Project/Task summary and detail projections.
- `hmem-mcp/test/HMem/MCP/ToolsSpec.hs` and `hmem-mcp/test/fixtures/mcp-compact-responses.json`: compact-output regression checks.
- Completed hmem project `9b1826d3-8bf5-42d6-8ddc-7baae246eb6c`: operation-appropriate MCP outputs.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0ifSwiYiI6IjMzYWJkODBkZDEzZGJjNmM4NTAzOTdlZWIxM2Y3OTA0N2QwZjdjOTEiLCJpIjoic2hhMjU2OlhsVjlHM1l5UFVHMktVNVZ0X0FkNkZxRXN6X1ZhRmZZOFBmR0xKUDNvcE0iLCJrIjoiZGVjaXNpb24uYW1lbmQiLCJvIjoiUjAxTTRYV1BRSzFQN0NZNUU1WTlWMEFaTVg5Iiwib3AiOiJPMDFNNFhXUFFLMVA3Q1k1RTVZOVYwQVpNWDkiLCJwIjpbIlIwMU0zS1AyREpQSENENjdGNlRCU1paTlFTUCJdLCJyIjoibWFzdGVyIiwicyI6InNoYTI1Njp4MElxMlJ0YUhMamplbHhnLS1rTjcyU1Q4T3FrNVNxU3pyY2N5TXlNcm1zIiwidCI6MTc5MjAwNDU0NjE0NSwidiI6MSwieCI6ImFkcmFpLzEuMC4wIn0 -->
