+++
schema = "adrai/decision/v1"
adr = "A01M38ZM4YHAEMP41G8KFHWEG75"
record = "R01M3KDCBSX9DH5QBQHX3WV2BZP"
title = "Keep pgvector and automatic vectorization optional"
summary = "Provision vectors explicitly, compare only within exact embedding spaces using caller-supplied query vectors, and enable automatic GPU generation only by choice."
domains = ["semantic-retrieval"]
+++

**Context**

Observation storage and full-text search operate without semantic vectors. When vectors are present, results are meaningful only when stored and query vectors share an embedding space. This amendment records the current query and storage contract and confirms it as the approved design; it does not infer the original historical rationale.

**Decision**

Keep pgvector optional. The baseline migration creates vector storage and its cosine HNSW index only if the extension already exists. Otherwise an operator installs the matching PostgreSQL package and explicitly runs `hmem-ctl pgvector enable`, which inspects readiness and provisions the extension, nullable 1536-dimensional column, and index.

Each Observation stores at most one vector with one embedding-space fingerprint. A similarity request supplies an already-produced, finite 1536-dimensional query vector. Retrieval compares only Observations whose stored fingerprint exactly equals the requested fingerprint. Omitting the fingerprint selects `hmem:legacy-manual:v1`; it does not search all spaces. hmem does not expose an endpoint that embeds raw query text. The caller or external producer owns the matching model revision and input formatting for document and query vectors.

Automatic vectorization is disabled by default. Operators may supply compatible manual vectors through REST, MCP, or NDJSON. Automatic generation runs only when explicitly enabled with pgvector ready and a validated GPU provider; the documented modes have no CPU inference fallback. The leased-job ADR separately governs how delayed automatic results are fenced before storage.

**Consequences**

Ordinary Observation and full-text operations remain available without pgvector. Exact-space filtering avoids comparisons between incompatible vectors. A change of model or preprocessing space requires replacing or recomputing an Observation’s single stored vector; parallel vectors for multiple spaces are not stored. Manual and raw-text query workflows require an external producer. Enabled automatic use adds GPU-provider validation and operation, while provider loss leaves ordinary API operations available.

**Evidence**

- `README.md`: optional semantic similarity, caller-supplied query vectors, disabled automatic default, and GPU-only opt-in deployment.
- `database.md`: one vector and fingerprint per Observation, exact-space retrieval, legacy default, external query production, and optional GPU worker.
- `hmem-core/src/HMem/DB/Observation.hs`: similarity query defaults to the legacy space and filters on exact `embedding_space_fingerprint`.
- `hmem-core/src/HMem/Types.hs`: validates query vector and optional space fingerprint.
- `hmem-server/src/HMem/Server/API.hs` and `hmem-mcp/src/HMem/MCP/Tools.hs`: expose vector-taking similarity operations.
- `hmem-server/src/HMem/Server/CtlPgvector.hs` and `hmem-server/src/HMem/Server/Embedding/Provider.hs`: explicit provisioning and enabled-provider boundary.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0ifSwiYiI6ImJhNTRiNGU5MTNlZWE0MzZiNmZiMTM1NTljZjY0NDI0MTJiNGJjNjAiLCJpIjoic2hhMjU2OmRGUGl6MFI2R0U3dGZ3NTBXUEtIRmhnQVRiUFRweWRtNEdVT1dlTVk2Z2siLCJrIjoiZGVjaXNpb24uYW1lbmQiLCJvIjoiUjAxTTNLRENCU1g5REg1UUJRSFgzV1YyQlpQIiwib3AiOiJPMDFNM0tEQ0JTWDlESDVRQlFIWDNXVjJCWlAiLCJwIjpbIlIwMU0zOFpNNTNBUUNOUDFXQkE2WlJOS1kxUSJdLCJyIjoibWFzdGVyIiwicyI6InNoYTI1Njp6WS1lYVdNZjNnRDJYNl9hWmVQbGRGNGFNQTZxdmpXdEt0VEtrWEpYRlNRIiwidCI6MTc5MDU3OTE5MTYxMywidiI6MSwieCI6ImFkcmFpLzEuMC4wIn0 -->
