+++
schema = "adrai/connection/v1"
connection = "C01M5C2Y1N77VBGJHQGQX388N6M"
relation = "applies_to"
added = ["api.md", "embeddings.md", "hmem-server/app/Main.hs", "hmem-server/src/HMem/Server/App.hs", "hmem-server/src/HMem/Server/ObservationDiscovery.hs", "hmem-server/test/HMem/Server/APISpec.hs", "hmem-server/test/HMem/Server/ObservationDiscoverySpec.hs"]
applies_to = ["README.md", "api.md", "database.md", "embeddings.md", "hmem-core/src/HMem/DB/Observation.hs", "hmem-core/src/HMem/Types.hs", "hmem-mcp/src/HMem/MCP/Tools.hs", "hmem-server/app/Main.hs", "hmem-server/migrations/V020__replace_memories_with_observations.sql", "hmem-server/src/HMem/Server/API.hs", "hmem-server/src/HMem/Server/App.hs", "hmem-server/src/HMem/Server/CtlPgvector.hs", "hmem-server/src/HMem/Server/Embedding/Provider.hs", "hmem-server/src/HMem/Server/ObservationDiscovery.hs", "hmem-server/test/HMem/Server/APISpec.hs", "hmem-server/test/HMem/Server/ObservationDiscoverySpec.hs"]
change = "expand"
parent_connections = ["C01M3KDDTNK2QFG6FDW897S2SGP"]
removed = []
subject_adr = "A01M38ZM4YHAEMP41G8KFHWEG75"
+++

Cover authorized discovery query preparation, shared provider lifecycle, capability and contract tests

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0iLCJtIjoiZ3B0LTYuMS1zb2wifSwiYiI6IjExMGNkY2VkNGI0OTRlOTk5MDA3N2UyN2I3ZDVlZDBjOGFiMDQzZGEiLCJrIjoic2NvcGUuZXhwYW5kIiwibyI6IkMwMU01QzJZMU43N1ZCR0pIUUdRWDM4OE42TSIsIm9wIjoiTzAxTTVDMlkxTjc3VkJHSkhRR1FYMzg4TjZNIiwicCI6WyJDMDFNM0tERFROSzJRRkc2RkRXODk3UzJTR1AiXSwiciI6ImNvZGV4L2xpdmUtb2JzZXJ2YXRpb24tc2VhcmNoIiwicyI6InNoYTI1NjpodWFlOXBBazZ5STd6NUNCUVdyeWVtbVhrUUZHbHZEZnVvSkhxZ2lGLTAwIiwidCI6MTc5MjQ4MDgzOTMzNSwidiI6MSwieCI6ImFkcmFpLzEuMC4wIn0 -->
