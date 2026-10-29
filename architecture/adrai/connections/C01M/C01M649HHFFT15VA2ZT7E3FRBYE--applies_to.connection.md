+++
schema = "adrai/connection/v1"
connection = "C01M649HHFFT15VA2ZT7E3FRBYE"
relation = "applies_to"
added = ["docs/guides/agent-sessions.md", "docs/reference/schema.md", "docs/reference/upgrades.md", "hmem-core/test/HMem/DB/AgentSessionSpec.hs", "hmem-server/migrations/V052__session_lifecycle_audit.sql", "hmem-server/test/HMem/Server/SessionReaperSpec.hs"]
applies_to = ["database.md", "docs/guides/agent-sessions.md", "docs/reference/schema.md", "docs/reference/upgrades.md", "hmem-core/src/HMem/DB/Audit.hs", "hmem-core/test/HMem/DB/AgentSessionSpec.hs", "hmem-server/migrations/V002__soft_deletes_audit_and_integrity.sql", "hmem-server/migrations/V020__replace_memories_with_observations.sql", "hmem-server/migrations/V052__session_lifecycle_audit.sql", "hmem-server/src/HMem/Server/API.hs", "hmem-server/test/HMem/Server/SessionReaperSpec.hs"]
change = "expand"
parent_connections = ["C01M39RCB3S0ZN5FMPMW55P4Z9Q"]
removed = []
subject_adr = "A01M39RCB0NC51143VRNQDG4R5Y"
+++

Apply the session lifecycle audit policy to its migration, checks and documentation.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0iLCJtIjoiZ3B0LTYuMS1zb2wifSwiYiI6IjU2MmRiMDhlMzU1OTdmY2I5ZjVhOWYyMmVkOWY5ODE5NGRlODk0MjkiLCJrIjoic2NvcGUuZXhwYW5kIiwibyI6IkMwMU02NDlISEZGVDE1VkEyWlQ3RTNGUkJZRSIsIm9wIjoiTzAxTTY0OUhIRkZUMTVWQTJaVDdFM0ZSQllFIiwicCI6WyJDMDFNMzlSQ0IzUzBaTjVGTVBNVzU1UDRaOVEiXSwiciI6Im1hc3RlciIsInMiOiJzaGEyNTY6N24tUVlnMGxkYkdYY0JqS1NxTUJzaFFmTlZjTVlPT2w2QktnOGFkbkhjdyIsInQiOjE3OTMyOTMwNzU5NTEsInYiOjEsIngiOiJhZHJhaS8xLjAuMCJ9 -->
