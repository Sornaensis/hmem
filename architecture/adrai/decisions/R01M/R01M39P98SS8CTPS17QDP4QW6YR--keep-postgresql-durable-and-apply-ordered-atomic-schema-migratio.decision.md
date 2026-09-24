+++
schema = "adrai/decision/v1"
adr = "A01M39P98JWSRWQ7DQG8XM2BZ3R"
record = "R01M39P98SS8CTPS17QDP4QW6YR"
title = "Keep PostgreSQL durable and apply ordered atomic schema migrations"
summary = "Use PostgreSQL for persistent application state and migrate before serving a replaceable container runtime."
domains = ["persistence-and-migrations"]
+++

Context: hmem persists Observations, planning records, identity, and synchronization state. Container builds package code and static assets. This record describes the observed implementation; the historical reason for the choice is not established by these sources.

Observed decision: Store durable application and authentication data in PostgreSQL. Name SQL migrations with ordered version prefixes, check schema_migrations for applied versions, and run each pending migration with its schema_migrations registration in one transactional session. In the documented Compose deployment, PostgreSQL uses the postgres-data volume while the application HOME uses temporary storage. The application entrypoint runs migrations with connection retries before starting hmem-server.

Consequences: Compose application containers can be replaced without moving the durable database. Operation depends on PostgreSQL readiness, orderly migration changes, and backups of the PostgreSQL volume. The native hmem-ctl installation may instead keep locally managed PostgreSQL data under ~/.hmem; the Compose storage layout is not a universal deployment rule.

Evidence:
- README.md: identifies PostgreSQL as the application backing store, documents native installation, and links the Compose quick start.
- hmem-core/src/HMem/DB/Migration.hs: sorts versioned SQL files, checks schema_migrations, and combines migration SQL with ledger registration in one session.
- hmem-server/migrations/V001__initial_schema.sql: defines the schema_migrations table.
- docker.md: documents the PostgreSQL volume, temporary application HOME, and migration-before-server startup.
- docker/entrypoint.sh: retries hmem-ctl migrate and starts hmem-server only after success.
- compose.yaml: mounts postgres-data for PostgreSQL and tmpfs for the hmem application HOME.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0ifSwiYiI6IjgxZjFkZmRhMjZjOWIzN2VhNGY5NGQwNGFkZTBhYmQyZWRmOTk5ZGUiLCJpIjoic2hhMjU2OmU0UlFIOWFZaWl5U0Y0QV9Nb3RmSlZVajBhelFoX2JyV3NpamRKZ2drYzAiLCJrIjoiZGVjaXNpb24uY3JlYXRlIiwibyI6IlIwMU0zOVA5OFNTOENUUFMxN1FEUDRRVzZZUiIsIm9wIjoiTzAxTTM5UDk4U1M4Q1RQUzE3UURQNFFXNllSIiwiciI6Im1hc3RlciIsInMiOiJzaGEyNTY6RTdxNVV6bjZ5VFdFeXlSekljNl9wTVE0Mm52TndzaXhidmJncVlPdE5LbyIsInQiOjE3OTAyNTI5ODMwOTcsInYiOjEsIngiOiJhZHJhaS8xLjAuMCJ9 -->
