+++
schema = "adrai/connection/v1"
connection = "C01M4QDREHW5SZVZ1J5EHYC08D2"
relation = "applies_to"
added = ["hmem-core/test/HMem/TypesSpec.hs", "hmem-mcp/src/HMem/MCP/Tools.hs", "hmem-mcp/test/HMem/MCP/ToolsSpec.hs", "hmem-server/migrations/V037__project_pause_audit_provenance.sql", "hmem-server/src/HMem/Server/OpenAPI.hs", "hmem-server/test/HMem/Server/APISpec.hs"]
applies_to = ["api.md", "docker.md", "hmem-core/src/HMem/DB/Overview.hs", "hmem-core/src/HMem/DB/Pool.hs", "hmem-core/src/HMem/DB/Project.hs", "hmem-core/src/HMem/DB/Task.hs", "hmem-core/src/HMem/Types.hs", "hmem-core/test/HMem/DB/ChangeStreamSpec.hs", "hmem-core/test/HMem/DB/OverviewSpec.hs", "hmem-core/test/HMem/DB/ProjectSpec.hs", "hmem-core/test/HMem/DB/TaskSpec.hs", "hmem-core/test/HMem/DB/TestHarnessSpec.hs", "hmem-core/test/HMem/TypesSpec.hs", "hmem-mcp/src/HMem/MCP/Tools.hs", "hmem-mcp/test/HMem/MCP/ToolsSpec.hs", "hmem-server/hmem-server.cabal", "hmem-server/migrations/V017__flat_subtask_lifecycle_rules.sql", "hmem-server/migrations/V027__bounded_task_dependency_cycle_check.sql", "hmem-server/migrations/V029__cascade_archive_and_cancel.sql", "hmem-server/migrations/V034__subtask_parent_auto_blocking.sql", "hmem-server/migrations/V035__project_pause_lifecycle.sql", "hmem-server/migrations/V036__normalize_project_pause.sql", "hmem-server/migrations/V037__project_pause_audit_provenance.sql", "hmem-server/src/HMem/Server/API.hs", "hmem-server/src/HMem/Server/OpenAPI.hs", "hmem-server/test/HMem/Server/APISpec.hs"]
change = "expand"
parent_connections = ["C01M4NJSA6AN5GT3KJ70Z2E00P5"]
removed = []
subject_adr = "A01M39R0FHX6Y5VC60CAHQDAX0D"
+++

Cover canonical pause effects and private audit replay across forward authority, schemas and compact transport tests

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0iLCJtIjoiZ3B0LTYuMS1zb2wifSwiYiI6IjFmY2JhNTU5Nzg3MDQzYjNhY2U2MGRhYThkOGI3MmIwZmQ2NTA2NTUiLCJrIjoic2NvcGUuZXhwYW5kIiwibyI6IkMwMU00UURSRUhXNVNaVloxSjVFSFlDMDhEMiIsIm9wIjoiTzAxTTRRRFJFSFc1U1pWWjFKNUVIWUMwOEQyIiwicCI6WyJDMDFNNE5KU0E2QU41R1QzS0o3MFoyRTAwUDUiXSwiciI6Im1hc3RlciIsInMiOiJzaGEyNTY6OFJIRWVsYnI2TnEyYlNaTjEwQS01bmJIcGpNcGlXcDJJWGRVUzNQY194MCIsInQiOjE3OTE3ODc1NDcxOTYsInYiOjEsIngiOiJhZHJhaS8xLjAuMCJ9 -->
