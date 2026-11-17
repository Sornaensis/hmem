+++
schema = "adrai/connection/v1"
connection = "C01M7MV30JQD62538XZB5KEF7Z2"
relation = "applies_to"
added = ["hmem-server/src/HMem/Server/Admission.hs", "hmem-server/test/HMem/Server/AdmissionSpec.hs"]
applies_to = ["api.md", "docs/reference/sealed-session-archives.md", "docs/reference/session-sync-performance.md", "docs/reference/session-traffic-telemetry.md", "hmem-core/cbits/session_archive.c", "hmem-core/src/HMem/AgentSession/Transport.hs", "hmem-core/src/HMem/DB/AgentSession.hs", "hmem-core/src/HMem/DB/SessionArchive.hs", "hmem-core/src/HMem/SessionArchive/Codec.hs", "hmem-core/test/HMem/DB/AgentSessionSpec.hs", "hmem-core/test/HMem/DB/SessionArchiveSpec.hs", "hmem-core/test/HMem/SessionArchive/CodecSpec.hs", "hmem-mcp/app/Main.hs", "hmem-mcp/cbits/session_file.c", "hmem-mcp/cbits/session_process.c", "hmem-mcp/hmem-mcp.cabal", "hmem-mcp/package.yaml", "hmem-mcp/reference.md", "hmem-mcp/src/HMem/MCP/Config.hs", "hmem-mcp/src/HMem/MCP/Server.hs", "hmem-mcp/src/HMem/MCP/SessionFile.hs", "hmem-mcp/src/HMem/MCP/SessionFollower.hs", "hmem-mcp/src/HMem/MCP/SessionHTTP.hs", "hmem-mcp/src/HMem/MCP/SessionProcess.hs", "hmem-mcp/src/HMem/MCP/Tools.hs", "hmem-mcp/test/HMem/MCP/ServerSpec.hs", "hmem-mcp/test/HMem/MCP/SessionFollowerSpec.hs", "hmem-mcp/test/HMem/MCP/ToolsSpec.hs", "hmem-server/app-harness/Main.hs", "hmem-server/app/Main.hs", "hmem-server/frontend/perf/session-sync.mjs", "hmem-server/hmem-server.cabal", "hmem-server/migrations/V050__sealed_session_archives.sql", "hmem-server/package.yaml", "hmem-server/src/HMem/Server/API.hs", "hmem-server/src/HMem/Server/Admission.hs", "hmem-server/src/HMem/Server/App.hs", "hmem-server/src/HMem/Server/Archive/Worker.hs", "hmem-server/src/HMem/Server/OpenAPI.hs", "hmem-server/src/HMem/Server/SessionPrefix.hs", "hmem-server/src/HMem/Server/SessionTraffic.hs", "hmem-server/test/HMem/Server/AdmissionSpec.hs", "hmem-server/test/HMem/Server/AgentSessionSpec.hs", "hmem-server/test/HMem/Server/Archive/WorkerSpec.hs", "hmem-server/test/HMem/Server/SessionPrefixSpec.hs", "tools/session-archive-probe.baseline.json", "tools/session-archive-probe.c", "tools/session-capture-traffic.qualification.json"]
change = "expand"
parent_connections = ["C01M7JNJ6BS2Z9ZCZEBW9BPXZPG"]
removed = []
subject_adr = "A01M39QPB23CYS9QHCWQSATSJE9"
+++

Server-wide HTTP admission is part of the API boundary decision

<!-- @adrai:eyJhIjp7ImkiOiJjbGF1ZGUtY29kZSIsImsiOiJsbG0iLCJtIjoiY2xhdWRlLW9wdXMtNS01In0sImIiOiJkN2ZlZGRjYTMzMjY2ZTYwMDQwZmZmYzViYjcwYWNmZTI3ZTYxYjNhIiwiayI6InNjb3BlLmV4cGFuZCIsIm8iOiJDMDFNN01WMzBKUUQ2MjUzOFhaQjVLRUY3WjIiLCJvcCI6Ik8wMU03TVYzMEpRRDYyNTM4WFpCNUtFRjdaMiIsInAiOlsiQzAxTTdKTko2QlMyWjlaQ1pFQlc5QlBYWlBHIl0sInIiOiJtYXN0ZXIiLCJzIjoic2hhMjU2Old6YWt1YjA5d09BcnQyUGdQNTBpX2Rhd0lJZTlyX01kaVdlZTBOV01wVm8iLCJ0IjoxNzk0OTIyMDg2OTk5LCJ2IjoxLCJ4IjoiYWRyYWkvMS4wLjAifQ -->
