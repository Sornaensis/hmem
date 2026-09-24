+++
schema = "adrai/decision/v1"
adr = "A01M39P98JWSRWQ7DQG8XM2BZ3R"
record = "R01M39PZ09CWX7BJ5K95RMDAAYJ"
title = "Keep PostgreSQL durable and apply ordered versioned schema migrations"
summary = "Use PostgreSQL for persistent application state and migrate before serving a replaceable container runtime."
domains = ["persistence-and-migrations"]
+++

Context: hmem persists Observations, planning records, identity, and synchronization state. Container builds package code and static assets. This record describes the observed implementation; the historical reason for the choice is not established by these sources.

Observed decision: Store durable application and authentication data in PostgreSQL. The migration runner sorts versioned SQL files, checks schema_migrations, then runs each pending file and attempts idempotent ledger registration in the same session. Migration scripts own transaction boundaries. V001 commits its schema before the runner registers its version, so the two steps are not uniformly atomic. V020 inserts its own ledger row before COMMIT, making that destructive schema transition and registration indivisible; the runner's later insert is a no-op. In the documented Compose deployment, PostgreSQL uses the postgres-data volume while application HOME uses temporary storage. The application entrypoint runs migrations with connection retries before starting hmem-server.

Consequences: Compose application containers can be replaced without moving the durable database. Operation depends on PostgreSQL readiness, backups, and disciplined migration scripts and ledger handling. The native hmem-ctl installation may instead keep locally managed PostgreSQL data under ~/.hmem; the Compose storage layout is not a universal deployment rule.

Evidence:
- README.md: identifies PostgreSQL as the application backing store, documents native installation, and links the Compose quick start.
- hmem-core/src/HMem/DB/Migration.hs: sorts versioned SQL files, checks schema_migrations, and runs migration SQL followed by ledger registration in one session.
- hmem-server/migrations/V001__initial_schema.sql: defines schema_migrations and commits before the runner's registration.
- hmem-server/migrations/V020__replace_memories_with_observations.sql: registers V020 inside its own transaction before COMMIT.
- docker.md: documents the PostgreSQL volume, temporary application HOME, and migration-before-server startup.
- docker/entrypoint.sh: retries hmem-ctl migrate and starts hmem-server only after success.
- compose.yaml: mounts postgres-data for PostgreSQL and tmpfs for the hmem application HOME.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0ifSwiYiI6IjE5MTVlZTJkOTUxMzIyNzc4OWRkYzA1YWQ1YjRiNzIwMzlhMzgyZDIiLCJpIjoic2hhMjU2OmRBNVlIMm9aRmFXNURtellRN2NTc21PN09wZjBuZzY3S3ZvTDEtcDNVR1EiLCJrIjoiZGVjaXNpb24uYW1lbmQiLCJvIjoiUjAxTTM5UFowOUNXWDdCSjVLOTVSTURBQVlKIiwib3AiOiJPMDFNMzlQWjA5Q1dYN0JKNUs5NVJNREFBWUoiLCJwIjpbIlIwMU0zOVA5OFNTOENUUFMxN1FEUDRRVzZZUiJdLCJyIjoibWFzdGVyIiwicyI6InNoYTI1NjpVT29sU2M5TnRqY0FfRDRlMEtZZUhaN3NvaVF2SFhpdENjZk5lQ2tDbnEwIiwidCI6MTc5MDI1MzY5NTI3NiwidiI6MSwieCI6ImFkcmFpLzEuMC4wIn0 -->
