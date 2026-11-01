+++
schema = "adrai/decision/v1"
adr = "A01M39RCB0NC51143VRNQDG4R5Y"
record = "R01M6CHB3JEFW4G65W7X1HEJMRQ"
title = "Audit entity changes and restore soft-deleted planning work"
summary = "Keep attributed planning recovery and retain only session lifecycle and task-connection audit events."
domains = ["audit-and-recovery"]
+++

Record entity actions, prior and new values, request identity, workspace and actor attribution in PostgreSQL. Projects and tasks support controlled audit reversions, including soft-deletion recovery. Observations are hard-deleted and cannot be recovered through audit replay. Legacy memory-family audit entries are retired with that entity family.

Session audit history records one agent_session/create for a new session, one agent_session/update for an actual active-to-terminated transition and one task_session/create for each new task connection, including the initial connection. Explicit manager sealing, abandoned-session cleanup and historical import completion share the termination rule. Exact retries and rejected operations add no duplicate audit rows. Append counters, heartbeat timestamps, runtime declarations and import progress add no audit records; their canonical live invalidations remain independent and unchanged.

An ordered transactional migration removes other historical session-family audit operations. Retain session creates and connection creates. Retain session updates only when their recorded object snapshots explicitly show active-to-terminated state; older and current projections use the same top-level state field. Missing, null, scalar, composite and unrecognized state values do not prove termination. Never infer a historical transition from the current session row. Preserve all other audit entities, transcript bytes, session metadata, task connections and canonical change-stream history. Deleted audit records require a prior backup for recovery.

This keeps the attributed planning trail and selective recovery while making session history useful without coupling audit retention to live synchronization.

Reclaim audit storage through scoped physical maintenance rather than age-based deletion of audit rows. Preserve all audit values, attribution, planning reversions and live constraints. Default to enabled daily eligibility from 03:00 to 04:00 in the database server's configured local timezone, with explicit window, timezone, calendar cadence and enablement overrides. Persist the actual execution date before scheduled admission so restart cannot repeat or catch up outside the window.

Use a dedicated, separately credentialed narrow maintenance connection and a singleton owner for assessment, top-level audit-only heap rewriting or one validated concurrent index rebuild. Require proven database-host capacity, bounded diagnostics and data/WAL pressure budgets; require a fresh administrator-attested verified restore before heap admission. Prefer eligible heap work and use per-index cooldowns. Unknown capabilities or unsafe identities defer. A cancellation signal is not a join: retain ownership through query/native completion and reconcile uncertain or partial outcomes against actual catalogs. Never infer artifact ownership from its name or drop it automatically. Pause stops admission and requests bounded cooperative drain; rollout and backup attestation remain explicit operator actions.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0iLCJtIjoiZ3B0LTYuMS1zb2wifSwiYiI6IjBiNjM2ZGNmYjNkN2EzODQwODFmNGY5ZDdlOTkzM2U2MjA5N2MxYTMiLCJpIjoic2hhMjU2OjhMUXlEZlFuYUhuVzhRaHdtNllFcEMyNnJ6VmRZYUU5TkNzc1NSSGdzRTQiLCJrIjoiZGVjaXNpb24uYW1lbmQiLCJvIjoiUjAxTTZDSEIzSkVGVzRHNjVXN1gxSEVKTVJRIiwib3AiOiJPMDFNNkNIQjNKRUZXNEc2NVc3WDFIRUpNUlEiLCJwIjpbIlIwMU02NDlEMjJTMllNVEpEVjNISkY0WkY5QSJdLCJyIjoibWFzdGVyIiwicyI6InNoYTI1NjpXVVFYYWRINXc1SnJaeDFrbXlsVzdGWk14Z0xrRFZZRjRWci1tRl91M0dvIiwidCI6MTc5MzU2OTY4OTE2NiwidiI6MSwieCI6ImFkcmFpLzEuMC4wIn0 -->
