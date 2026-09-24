+++
schema = "adrai/decision/v1"
adr = "A01M39RCB0NC51143VRNQDG4R5Y"
record = "R01M39RCB3S47RNV388YXBX79GV"
title = "Audit entity changes and restore soft-deleted planning work"
summary = "Record attributed database changes and allow controlled recovery of projects and tasks."
domains = ["audit-and-recovery"]
+++

Context: Users need to inspect changes to planning state and recover some deletions. This record describes the current implementation; the historical reason for the choice is not established by these sources.

Observed decision: PostgreSQL audit triggers record entity action, prior and new values, request ID, workspace, and actor attribution. Workspaces, projects, and tasks use deleted_at lifecycle markers. The authorized audit API permits selected reversions of project and task entries, including restoration after soft deletion. Observation deletion is a hard delete, and Observation audit entries cannot be reverted. The legacy memory-family audit records created before the Observation migration were removed with that family.

Consequences: The database provides an attributed change trail and controlled recovery for planning work. Audit retention and sensitive-value handling require care, and revert support is deliberately selective; an Observation cannot be recovered by replaying its audit entry.

Evidence:
- hmem-server/migrations/V002__soft_deletes_audit_and_integrity.sql: adds deleted_at to planning tables and defines audit_log, old/new values, request ID, and entity triggers for the legacy schema.
- hmem-server/migrations/V020__replace_memories_with_observations.sql: removes legacy memory-family audit records, adds workspace and actor attribution to audit writes, and audits Observation changes.
- hmem-core/src/HMem/DB/Audit.hs: reads audit entries and filtered logs with old/new values and request/actor fields.
- hmem-server/src/HMem/Server/API.hs: authorizes audit access and supports selected project/task reversions while rejecting all Observation audit reverts.
- database.md: documents Observation hard deletion and the audit trail for entity changes.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0ifSwiYiI6ImVkZDQzNTA4NDFiZGY5NTNkM2U5ZmIyOGU0ZDk4YzI0MmQ2N2Q3MjMiLCJpIjoic2hhMjU2Om9JeHNHdmlicGNnZXlGOHpubk10UTAyMEhrOUs4clMyQVg4Skk3OTdOWmsiLCJrIjoiZGVjaXNpb24uY3JlYXRlIiwibyI6IlIwMU0zOVJDQjNTNDdSTlYzODhZWEJYNzlHViIsIm9wIjoiTzAxTTM5UkNCM1M0N1JOVjM4OFlYQlg3OUdWIiwiciI6Im1hc3RlciIsInMiOiJzaGEyNTY6ZjhCYlN0TkR3aWY1VHJ2bVZfVFV3SVJJWDBKVHNKeGVZTkViYUVsNmdvayIsInQiOjE3OTAyNTUxODA5MjEsInYiOjEsIngiOiJhZHJhaS8xLjAuMCJ9 -->
