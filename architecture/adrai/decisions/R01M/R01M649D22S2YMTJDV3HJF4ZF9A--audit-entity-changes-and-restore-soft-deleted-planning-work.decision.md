+++
schema = "adrai/decision/v1"
adr = "A01M39RCB0NC51143VRNQDG4R5Y"
record = "R01M649D22S2YMTJDV3HJF4ZF9A"
title = "Audit entity changes and restore soft-deleted planning work"
summary = "Keep attributed planning recovery and retain only session lifecycle and task-connection audit events."
domains = ["audit-and-recovery"]
+++

Record entity actions, prior and new values, request identity, workspace and actor attribution in PostgreSQL. Projects and tasks support controlled audit reversions, including soft-deletion recovery. Observations are hard-deleted and cannot be recovered through audit replay. Legacy memory-family audit entries are retired with that entity family.

Session audit history records one agent_session/create for a new session, one agent_session/update for an actual active-to-terminated transition and one task_session/create for each new task connection, including the initial connection. Explicit manager sealing, abandoned-session cleanup and historical import completion share the termination rule. Exact retries and rejected operations add no duplicate audit rows. Append counters, heartbeat timestamps, runtime declarations and import progress add no audit records; their canonical live invalidations remain independent and unchanged.

An ordered transactional migration removes other historical session-family audit operations. Retain session creates and connection creates. Retain session updates only when their recorded object snapshots explicitly show active-to-terminated state; older and current projections use the same top-level state field. Missing, null, scalar, composite and unrecognized state values do not prove termination. Never infer a historical transition from the current session row. Preserve all other audit entities, transcript bytes, session metadata, task connections and canonical change-stream history. Deleted audit records require a prior backup for recovery.

This keeps the attributed planning trail and selective recovery while making session history useful without coupling audit retention to live synchronization.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0iLCJtIjoiZ3B0LTYuMS1zb2wifSwiYiI6IjQ3ODcyN2E4ODM5YWJmNjE4ZTMxYTE1ZmFhNjQ5ZTRlNTNkOTUyYTAiLCJpIjoic2hhMjU2Ok5vbTE3a0ZEWEd5alRVTmF6RTItUHhPT2Z0ZzlmYlF1M0ZKRGhick9Gc1kiLCJrIjoiZGVjaXNpb24uYW1lbmQiLCJvIjoiUjAxTTY0OUQyMlMyWU1USkRWM0hKRjRaRjlBIiwib3AiOiJPMDFNNjQ5RDIyUzJZTVRKRFYzSEpGNFpGOUEiLCJwIjpbIlIwMU0zOVJDQjNTNDdSTlYzODhZWEJYNzlHViJdLCJyIjoibWFzdGVyIiwicyI6InNoYTI1NjpBaEVCVUFQbWJCZmU0NFBOOGlSX1UtcXNaZ2NCUXVkNzE0UlBKUEQyTjNzIiwidCI6MTc5MzI5MjkyOTExMywidiI6MSwieCI6ImFkcmFpLzEuMC4wIn0 -->
