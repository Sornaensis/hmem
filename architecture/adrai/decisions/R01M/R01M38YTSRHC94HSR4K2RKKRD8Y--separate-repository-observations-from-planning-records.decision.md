+++
schema = "adrai/decision/v1"
adr = "A01M38YTSKN0YXGCJ32PYV7WJDX"
record = "R01M38YTSRHC94HSR4K2RKKRD8Y"
title = "Separate repository Observations from planning records"
summary = "Store repository evidence as workspace-scoped Observations without project or task links."
domains = ["data-model"]
+++

Context: hmem stores repository evidence and planning work in the same workspace. This record describes the current implementation; the historical reason for the choice is not established by these sources.

Observed decision: Store repository evidence as workspace-scoped Observations, separate from Project and Task planning records. Observations have a workspace foreign key and repository provenance, but no project or task link columns or join tables. Project and Task APIs remain distinct from the Observation API.

Consequences: Evidence remains scoped to its repository workspace without being attached to a particular plan. An Observation row does not directly identify a project or task; consumers must use its workspace and repository subject context rather than assume a planning link.

Evidence:
- database.md: relationships and observations sections state the separation and absence of project/task links.
- hmem-server/migrations/V020__replace_memories_with_observations.sql: removes legacy project/task memory link tables and creates workspace-bound observations while retaining project/task data.
- hmem-server/src/HMem/Server/API.hs: defines separate Observation, Project, and Task APIs and routes Observation creation through repository-workspace authorization.
- hmem-core/src/HMem/DB/Observation.hs: inserts Observations using workspace, subjects, Git SHA, and content, without a project/task field.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0ifSwiYiI6IjVhZDQzZTdhMmE4NGY4OWUzMDViZTQ5YmZiNTYyOTM2M2U0MGVjNzgiLCJpIjoic2hhMjU2OkduV2J6eHBTRDFpNXNZejFvQmN5WEhkR0VFV2p2Sm55dU9ZdXR3WnkySWciLCJrIjoiZGVjaXNpb24uY3JlYXRlIiwibyI6IlIwMU0zOFlUU1JIQzk0SFNSNEsyUktLUkQ4WSIsIm9wIjoiTzAxTTM4WVRTUkhDOTRIU1I0SzJSS0tSRDhZIiwiciI6Im1hc3RlciIsInMiOiJzaGEyNTY6OGZZMlZELXAxTlViMmpFZUVZN0M3SGVHMUFtak1FcFN5SVBrOW9UY2xzZyIsInQiOjE3OTAyMjgzOTE2OTcsInYiOjEsIngiOiJhZHJhaS8xLjAuMCJ9 -->
