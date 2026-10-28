+++
schema = "adrai/connection/v1"
connection = "C01M60RZ2BXQGTHG39DEJX1KGD6"
relation = "applies_to"
added = ["hmem-core/src/HMem/DB/SessionArchive.hs", "hmem-core/test/HMem/DB/SessionArchiveSpec.hs", "hmem-server/migrations/V050__sealed_session_archives.sql"]
applies_to = ["api.md", "docs/reference/sealed-session-archives.md", "docs/reference/session-sync-performance.md", "hmem-core/src/HMem/AgentSession/Transport.hs", "hmem-core/src/HMem/DB/AgentSession.hs", "hmem-core/src/HMem/DB/SessionArchive.hs", "hmem-core/test/HMem/DB/AgentSessionSpec.hs", "hmem-core/test/HMem/DB/SessionArchiveSpec.hs", "hmem-mcp/app/Main.hs", "hmem-mcp/cbits/session_file.c", "hmem-mcp/cbits/session_process.c", "hmem-mcp/hmem-mcp.cabal", "hmem-mcp/package.yaml", "hmem-mcp/reference.md", "hmem-mcp/src/HMem/MCP/Config.hs", "hmem-mcp/src/HMem/MCP/Server.hs", "hmem-mcp/src/HMem/MCP/SessionFile.hs", "hmem-mcp/src/HMem/MCP/SessionFollower.hs", "hmem-mcp/src/HMem/MCP/SessionHTTP.hs", "hmem-mcp/src/HMem/MCP/SessionProcess.hs", "hmem-mcp/src/HMem/MCP/Tools.hs", "hmem-mcp/test/HMem/MCP/ServerSpec.hs", "hmem-mcp/test/HMem/MCP/SessionFollowerSpec.hs", "hmem-mcp/test/HMem/MCP/ToolsSpec.hs", "hmem-server/app-harness/Main.hs", "hmem-server/app/Main.hs", "hmem-server/frontend/perf/session-sync.mjs", "hmem-server/hmem-server.cabal", "hmem-server/migrations/V050__sealed_session_archives.sql", "hmem-server/package.yaml", "hmem-server/src/HMem/Server/API.hs", "hmem-server/src/HMem/Server/App.hs", "hmem-server/src/HMem/Server/OpenAPI.hs", "hmem-server/src/HMem/Server/SessionPrefix.hs", "hmem-server/test/HMem/Server/AgentSessionSpec.hs", "hmem-server/test/HMem/Server/SessionPrefixSpec.hs", "tools/session-archive-probe.baseline.json", "tools/session-archive-probe.c"]
change = "expand"
parent_connections = ["C01M60PSQ2B6N9T23FB3N7CFHPS"]
removed = []
subject_adr = "A01M39QPB23CYS9QHCWQSATSJE9"
+++

Archive-storage-enforces-the-selected-sealed-session-contract

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0iLCJtIjoiZ3B0LTYuMS1zb2wifSwiYiI6ImY0YjQxZDAyYmJhY2UyMDliMjU5MWI5MjNkZWFmNGVmOTQ1M2M1NzkiLCJrIjoic2NvcGUuZXhwYW5kIiwibyI6IkMwMU02MFJaMkJYUUdUSEczOURFSlgxS0dENiIsIm9wIjoiTzAxTTYwUloyQlhRR1RIRzM5REVKWDFLR0Q2IiwicCI6WyJDMDFNNjBQU1EyQjZOOVQyM0ZCM043Q0ZIUFMiXSwiciI6Im1hc3RlciIsInMiOiJzaGEyNTY6SEhWbHU1QnBnMGNaeUt3TDl4bnJTeXJxVkozT2FBNWtPZjZ5cUt1S1dRRSIsInQiOjE3OTMxNzUwMzAxNDEsInYiOjEsIngiOiJhZHJhaS8xLjAuMCJ9 -->
