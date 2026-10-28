+++
schema = "adrai/connection/v1"
connection = "C01M60TMYQA765FSK13JKZDFHRZ"
relation = "applies_to"
added = ["hmem-core/cbits/session_archive.c", "hmem-core/src/HMem/SessionArchive/Codec.hs", "hmem-core/test/HMem/SessionArchive/CodecSpec.hs", "hmem-server/src/HMem/Server/Archive/Worker.hs", "hmem-server/test/HMem/Server/Archive/WorkerSpec.hs"]
applies_to = ["api.md", "docs/reference/sealed-session-archives.md", "docs/reference/session-sync-performance.md", "hmem-core/cbits/session_archive.c", "hmem-core/src/HMem/AgentSession/Transport.hs", "hmem-core/src/HMem/DB/AgentSession.hs", "hmem-core/src/HMem/DB/SessionArchive.hs", "hmem-core/src/HMem/SessionArchive/Codec.hs", "hmem-core/test/HMem/DB/AgentSessionSpec.hs", "hmem-core/test/HMem/DB/SessionArchiveSpec.hs", "hmem-core/test/HMem/SessionArchive/CodecSpec.hs", "hmem-mcp/app/Main.hs", "hmem-mcp/cbits/session_file.c", "hmem-mcp/cbits/session_process.c", "hmem-mcp/hmem-mcp.cabal", "hmem-mcp/package.yaml", "hmem-mcp/reference.md", "hmem-mcp/src/HMem/MCP/Config.hs", "hmem-mcp/src/HMem/MCP/Server.hs", "hmem-mcp/src/HMem/MCP/SessionFile.hs", "hmem-mcp/src/HMem/MCP/SessionFollower.hs", "hmem-mcp/src/HMem/MCP/SessionHTTP.hs", "hmem-mcp/src/HMem/MCP/SessionProcess.hs", "hmem-mcp/src/HMem/MCP/Tools.hs", "hmem-mcp/test/HMem/MCP/ServerSpec.hs", "hmem-mcp/test/HMem/MCP/SessionFollowerSpec.hs", "hmem-mcp/test/HMem/MCP/ToolsSpec.hs", "hmem-server/app-harness/Main.hs", "hmem-server/app/Main.hs", "hmem-server/frontend/perf/session-sync.mjs", "hmem-server/hmem-server.cabal", "hmem-server/migrations/V050__sealed_session_archives.sql", "hmem-server/package.yaml", "hmem-server/src/HMem/Server/API.hs", "hmem-server/src/HMem/Server/App.hs", "hmem-server/src/HMem/Server/Archive/Worker.hs", "hmem-server/src/HMem/Server/OpenAPI.hs", "hmem-server/src/HMem/Server/SessionPrefix.hs", "hmem-server/test/HMem/Server/AgentSessionSpec.hs", "hmem-server/test/HMem/Server/Archive/WorkerSpec.hs", "hmem-server/test/HMem/Server/SessionPrefixSpec.hs", "tools/session-archive-probe.baseline.json", "tools/session-archive-probe.c"]
change = "expand"
parent_connections = ["C01M60RZ2BXQGTHG39DEJX1KGD6"]
removed = []
subject_adr = "A01M39QPB23CYS9QHCWQSATSJE9"
+++

Apply-existing-sealed-archive-contract-to-codec-and-worker.

<!-- @adrai:eyJhIjp7ImkiOiJkZXZlbG9wZXIiLCJrIjoibGxtIiwibSI6ImNvZGV4OmdwdC02LjEtc29sOmhpZ2gifSwiYiI6ImFhNTFhMTRjODBkNzU0ZTZjZGI4YjA5NTBhNWU4NWJmOTQ1MzQxYzEiLCJrIjoic2NvcGUuZXhwYW5kIiwibyI6IkMwMU02MFRNWVFBNzY1RlNLMTNKS1pERkhSWiIsIm9wIjoiTzAxTTYwVE1ZUUE3NjVGU0sxM0pLWkRGSFJaIiwicCI6WyJDMDFNNjBSWjJCWFFHVEhHMzlERUpYMUtHRDYiXSwiciI6Im1hc3RlciIsInMiOiJzaGEyNTY6eHdHNHF6Rno4eDlxV19aSXl6NTZhdGFBTTByaUxaNVQ1dm9zUTRNSEtTbyIsInQiOjE3OTMxNzY3OTU4ODIsInYiOjEsIngiOiJhZHJhaS8xLjAuMCJ9 -->
