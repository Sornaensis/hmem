+++
schema = "adrai/decision/v1"
adr = "A01M38ZM4YHAEMP41G8KFHWEG75"
record = "R01M5C2TN240ZGDQGHZYHEXG321"
title = "Keep pgvector and automatic vectorization optional"
summary = "Keep vectors optional, compare exact embedding spaces, and permit bounded discovery queries through the explicitly enabled validated GPU provider."
domains = ["semantic-retrieval"]
+++

**Context**

Observation storage and full-text search operate without semantic vectors. Stored and query vectors must share an exact embedding space. Interactive Observation discovery may reuse an explicitly enabled, validated GPU provider; ordinary vector-taking similarity clients retain ownership of their query production.

**Decision**

Keep pgvector optional. The baseline migration creates vector storage and its cosine HNSW index only if the extension already exists. Otherwise an operator installs the matching PostgreSQL package and explicitly runs `hmem-ctl pgvector enable`, which inspects readiness and provisions the extension, nullable 1536-dimensional column, and index.

Each Observation stores at most one vector with one embedding-space fingerprint. Existing similarity requests supply already-produced, finite 1536-dimensional vectors and compare only Observations whose stored fingerprint exactly equals the requested fingerprint. Omitting the fingerprint selects `hmem:legacy-manual:v1`; it never searches all spaces. Those callers or external producers own matching model revision and document/query formatting.

Permit an authorized raw-text Observation discovery service to prepare a server-private query vector using the already configured, explicitly enabled and validated GPU provider. Authorize read access to an active repository workspace before any storage or provider work. Require optional vector storage, compatible stored vectors in that workspace, pinned query instruction formatting, finite normalized 1536-dimensional output and the exact embedding-space fingerprint. Share the lifecycle-owned provider and its bounded HTTP admission gate; do not construct a second transport or start/install/enable a model in response to discovery. Apply an interactive deadline and bounded input profile. Withdrawal invalidates in-flight preparation and stale generations cannot regain authority. Do not expose generated vectors, input text, endpoints, credentials or provider configuration in capability/fallback output.

For every nonempty body query with usable semantics, combine content FTS candidates with qualified same-space similarity candidates even when FTS has hits. Deduplicate the union and use deterministic documented rank fusion, preserving FTS-only and semantic-only rows. Subjects and provenance filters apply with AND before membership, counts, tree pruning and pagination. Candidate bounds/truncation must be explicit and shared across those operations. Disabled, absent, timed-out, malformed, incompatible or retired providers, missing storage and absent compatible vectors retain ordinary content FTS with fixed secret-safe outcome codes. There is no CPU inference or cross-space fallback and no public query-vector generation endpoint.

Automatic vectorization remains disabled by default. Operators may supply compatible manual vectors through REST, MCP, or NDJSON. Automatic generation runs only when explicitly enabled with pgvector ready and a validated GPU provider. The leased-job ADR separately governs delayed automatic results before storage.

**Consequences**

Ordinary Observation and full-text operations remain available without vectors or query generation. Exact-space filtering avoids incompatible comparisons. Changing model/preprocessing space requires replacing or recomputing an Observation's single stored vector; parallel vectors for multiple spaces are not stored. Opt-in discovery can generate private query vectors while existing vector-taking interfaces remain compatible. Provider loss degrades discovery to FTS and leaves ordinary API operations available.

**Contract references**

- `embeddings.md` and `api.md`: optional provider operation, safe capability output and discovery query bounds.
- `hmem-server/src/HMem/Server/ObservationDiscovery.hs`, `Embedding/GteQwen2.hs`, `Embedding/Http.hs`: private preparation, exact semantic contract and shared bounded transport.
- `hmem-server/app/Main.hs`, `hmem-server/src/HMem/Server/App.hs` and `API.hs`: lifecycle publication and authorized request wiring.
- `hmem-core/src/HMem/DB/Observation.hs` and `Types.hs`: optional storage readiness, exact-space retrieval and vector validation.
- Existing pgvector provisioning, MCP vector-taking tools and immutable provenance contracts remain applicable.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0iLCJtIjoiZ3B0LTYuMS1zb2wifSwiYiI6ImI3M2Q1ZDVkMGIwN2YyZjI3YzVlOTgyNzY0YjE4ODZkNWNhZmM0MWEiLCJpIjoic2hhMjU2OlJFQVJuOVMyUzdOa3pDNFVYZTVWZkhYUlpKbkQ3RFlVVFNOU3hZLVZIQm8iLCJrIjoiZGVjaXNpb24uYW1lbmQiLCJvIjoiUjAxTTVDMlROMjQwWkdEUUdIWllIRVhHMzIxIiwib3AiOiJPMDFNNUMyVE4yNDBaR0RRR0haWUhFWEczMjEiLCJwIjpbIlIwMU0zS0RDQlNYOURINVFCUUhYM1dWMkJaUCJdLCJyIjoiY29kZXgvbGl2ZS1vYnNlcnZhdGlvbi1zZWFyY2giLCJzIjoic2hhMjU2OmZ6RXUwQjg3ajgtZEJUZF85NEU4bm9HRkZuR2NMSVhZc29mZzY0QlpZNDAiLCJ0IjoxNzkyNDgwNzI4MTMyLCJ2IjoxLCJ4IjoiYWRyYWkvMS4wLjAifQ -->
