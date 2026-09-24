+++
schema = "adrai/decision/v1"
adr = "A01M39S6TTDPN91BC692YQX6RFN"
record = "R01M39S6TXGQFCBGPS93DFKYD8B"
title = "Reconcile automatic embeddings through leased durable jobs"
summary = "Fence generated vector writes by exact observation content, embedding space, and lease ownership."
domains = ["embedding-reconciliation"]
+++

Context
A provider call can continue after an Observation's content changes, an enabled embedding target moves to another space, or a worker stops and restarts. Automatic vector generation therefore needs durable work and an exact check before a delayed result is stored.

Observed decision
PostgreSQL stores at most one embedding job per Observation, keyed to its workspace, content fingerprint, and embedding space. Reconciliation enqueues missing or wrong-space vectors for the enabled target. Workers claim due pending or expired jobs in bounded batches with leases and a fresh attempt owner, renew current leases, and release failures for bounded retry or terminal failure. Before completing a job, the database code checks the current Observation content fingerprint, enabled target space, job attempt count, owner, state, and unexpired lease under locks. The vector and space label are written together only for that current attempt, and the job is settled in the same transaction. The worker handles provider cancellation and failures by releasing current claims.

Consequences
Persisted jobs let work resume after worker failure, while exact attempt and target checks fence stale provider output after content or space changes. This adds job reconciliation, lease renewal, retry, and worker lifecycle complexity. Automatic vectorization is enabled only when a validated provider and pgvector are available.

Historical rationale
The original rationale is not recorded here; this ADR describes the observed implementation.

Evidence
hmem-server/migrations/V028__durable_embedding_jobs_and_space_isolation.sql
hmem-core/src/HMem/DB/Embedding.hs
hmem-server/src/HMem/Server/Embedding/Worker.hs
database.md

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0ifSwiYiI6IjczYmQ1MDUxMzY5ZWM5MzIzNWQ1YzQyNWY1MWUxZTU0MGI1ZWQ2ZjMiLCJpIjoic2hhMjU2OmxKbnZxQkoxd3BNRGxsbklCZkNSNndRbDVoSHBFMFE0SGE3eEZhcnVTQVkiLCJrIjoiZGVjaXNpb24uY3JlYXRlIiwibyI6IlIwMU0zOVM2VFhHUUZDQkdQUzkzREZLWUQ4QiIsIm9wIjoiTzAxTTM5UzZUWEdRRkNCR1BTOTNERktZRDhCIiwiciI6Im1hc3RlciIsInMiOiJzaGEyNTY6NHdySVM2akZJTmNLSGMycUxUZnRmVzRHVURacnE5eDFBdng3NG03NHJiMCIsInQiOjE3OTAyNTYwNDkwNzIsInYiOjEsIngiOiJhZHJhaS8xLjAuMCJ9 -->
