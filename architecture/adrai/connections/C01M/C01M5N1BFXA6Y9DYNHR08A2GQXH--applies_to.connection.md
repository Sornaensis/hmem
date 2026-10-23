+++
schema = "adrai/connection/v1"
connection = "C01M5N1BFXA6Y9DYNHR08A2GQXH"
relation = "applies_to"
added = ["hmem-mcp/app/Main.hs", "hmem-mcp/cbits/session_process.c", "hmem-mcp/src/HMem/MCP/SessionProcess.hs"]
applies_to = ["hmem-mcp/app/Main.hs", "hmem-mcp/cbits/session_file.c", "hmem-mcp/cbits/session_process.c", "hmem-mcp/hmem-mcp.cabal", "hmem-mcp/package.yaml", "hmem-mcp/reference.md", "hmem-mcp/src/HMem/MCP/Config.hs", "hmem-mcp/src/HMem/MCP/Server.hs", "hmem-mcp/src/HMem/MCP/SessionFile.hs", "hmem-mcp/src/HMem/MCP/SessionFollower.hs", "hmem-mcp/src/HMem/MCP/SessionHTTP.hs", "hmem-mcp/src/HMem/MCP/SessionProcess.hs", "hmem-mcp/src/HMem/MCP/Tools.hs", "hmem-mcp/test/HMem/MCP/ServerSpec.hs", "hmem-mcp/test/HMem/MCP/SessionFollowerSpec.hs", "hmem-mcp/test/HMem/MCP/ToolsSpec.hs", "hmem-server/src/HMem/Server/API.hs"]
change = "mixed"
parent_connections = ["C01M5N125GT6Z2992NA89V2F00N"]
removed = ["hmem-mcp/cbits/session_socket.c", "hmem-mcp/src/HMem/MCP/SessionSocket.hs"]
subject_adr = "A01M39QPB23CYS9QHCWQSATSJE9"
+++

Own cross-platform per-request HTTP helpers including hidden dispatch, process lifetime and private pipes; replace unimplemented socket-helper scope

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0iLCJtIjoiY29kZXg6Z3B0LTYuMS1zb2w6aGlnaCJ9LCJiIjoiMjlhNGFjMjNmMzQwODY4MzBiZjBlZDAxZjU1MDk0ZWE2YjJiY2QwMCIsImsiOiJzY29wZS5taXhlZCIsIm8iOiJDMDFNNU4xQkZYQTZZOURZTkhSMDhBMkdRWEgiLCJvcCI6Ik8wMU01TjFCRlhBNlk5RFlOSFIwOEEyR1FYSCIsInAiOlsiQzAxTTVOMTI1R1Q2WjI5OTJOQTg5VjJGMDBOIl0sInIiOiJtYXN0ZXIiLCJzIjoic2hhMjU2OktENHFyakJYbzFrMG1IYkxpeFY3UUZaakFYZF92cFU0bFpPME5kTFNqdDgiLCJ0IjoxNzkyNzgxMTcyNjUwLCJ2IjoxLCJ4IjoiYWRyYWkvMS4wLjAifQ -->
