+++
schema = "adrai/decision/v1"
adr = "A01M38ZM4YHAEMP41G8KFHWEG75"
record = "R01M38ZM53AQCNP1WBA6ZRNKY1Q"
title = "Keep pgvector and automatic vectorization optional"
summary = "Provision vector storage explicitly and use external vectors unless a validated GPU provider is enabled."
domains = ["semantic-retrieval"]
+++

Context: Observation storage and full-text search operate without semantic vectors. This record describes the current implementation; the historical reason for the choice is not established by these sources.

Observed decision: Keep pgvector optional. The baseline migration creates vector storage and its cosine HNSW index only if the extension already exists. Otherwise an operator installs the matching PostgreSQL package and explicitly runs hmem-ctl pgvector enable, which inspects readiness and provisions the extension, nullable 1536-dimensional column, and index. Automatic vectorization is disabled by default. Operators may supply compatible finite vectors and matching query vectors from an external producer through REST, MCP, or NDJSON. Automatic generation runs only when explicitly enabled with pgvector ready and a validated GPU provider; the documented modes have no CPU inference fallback.

Consequences: Ordinary Observation and full-text operations remain available without pgvector, while similarity requires separate provisioning and produced vectors. Manual use places model, formatting, and vector-space compatibility on the external producer. Automatic use adds GPU-provider validation and operation; provider loss does not prevent ordinary API operations.

Evidence:
- README.md: optional semantic similarity, explicit pgvector commands, disabled automatic default, manual vectors, and opt-in GPU deployment.
- database.md: non-vector operations without extension, provisioning steps, external manual producer, and validated GPU worker conditions.
- hmem-server/migrations/V020__replace_memories_with_observations.sql: conditional vector column and HNSW index creation.
- hmem-server/src/HMem/Server/CtlPgvector.hs: readiness inspection and explicit transactional extension, column, and index provisioning.
- hmem-server/src/HMem/Server/Embedding/Provider.hs: disabled provider behavior and validated enabled-provider boundary.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0ifSwiYiI6ImUyNjVmMTc3ZWNhYzFiMzdiY2U0MDQ2NDc3MWVhYWJiMTM2ZDNlOTUiLCJpIjoic2hhMjU2OkZIRkJWVTVDNlA4MmJWdks4UEdPaVJxRlVnZ3NDTTR2Q3JPY2wzcnV3OG8iLCJrIjoiZGVjaXNpb24uY3JlYXRlIiwibyI6IlIwMU0zOFpNNTNBUUNOUDFXQkE2WlJOS1kxUSIsIm9wIjoiTzAxTTM4Wk01M0FRQ05QMVdCQTZaUk5LWTFRIiwiciI6Im1hc3RlciIsInMiOiJzaGEyNTY6RWthdTdDZTRJQmo1eVBGQkwyMUxKNUlNVVlrTTI3d3lac0RNSmxvd1h2MCIsInQiOjE3OTAyMjkyMjI1MDYsInYiOjEsIngiOiJhZHJhaS8xLjAuMCJ9 -->
