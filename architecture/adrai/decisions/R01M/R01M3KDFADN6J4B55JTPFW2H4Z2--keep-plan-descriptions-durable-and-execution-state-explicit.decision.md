+++
schema = "adrai/decision/v1"
adr = "A01M3KDFA96ZX84BARQ5DT3BXPY"
record = "R01M3KDFADN6J4B55JTPFW2H4Z2"
title = "Keep plan descriptions durable and execution state explicit"
summary = "Use Project and Task descriptions for lasting scope and acceptance intent, statuses for progress, and subtasks for newly discovered atomic work."
domains = ["planning-semantics"]
+++

**Context**

Humans and agents may return to a plan across sessions. A Project or Task description needs to remain a useful statement of what is to be done, while execution progress changes separately. The existing hierarchy ADR defines structural and lifecycle constraints but does not define the meaning of descriptions.

**Decision**

Treat Project and Task descriptions as durable, editable specifications: aims, scope, constraints, approach, and acceptance intent. Record execution progress in status fields. Create newly discovered atomic work as a Task or subtask rather than turning the parent description into a running progress log. A description may be revised when the specification changes; “durable” does not mean immutable. Keep repository insights in provenance-bound Observations rather than using them as procedural progress notes.

This semantic rule is expressed in the MCP tool contract and approved architecture. Project and Task rows store descriptions and statuses separately; the database does not validate whether prose is a good specification.

**Consequences**

Workers can retrieve a stable work contract and inspect status independently. Scope changes require deliberate specification edits, while new work requires a task record. The system needs audit and task history to show how a specification evolved; prose quality remains a human and agent responsibility.

**Evidence**

- `hmem-mcp/src/HMem/MCP/Tools.hs`: Project and Task create/update/spec tools describe descriptions as durable specifications, status as execution state, and later work as subtasks.
- `hmem-core/src/HMem/DB/Project.hs` and `hmem-core/src/HMem/DB/Task.hs`: persist description and status as distinct fields.
- Completed hmem project `ff504577-0cd7-4557-98c8-b1e0ee2fbc3a`: explicitly specifies the durable-description and status distinction.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0ifSwiYiI6IjI1ODk4YWQwMGUwZjlkOWEwOTk3NzJkNDZkMjQ0MjM0MTVjZjcyMzYiLCJpIjoic2hhMjU2Oi1RaFg2NHp3SXVLVkM5MEFhMlVzSE41RmE5MldobGpVN083cVc2S1ExWWciLCJrIjoiZGVjaXNpb24uY3JlYXRlIiwibyI6IlIwMU0zS0RGQURONko0QjU1SlRQRlcySDRaMiIsIm9wIjoiTzAxTTNLREZBRE42SjRCNTVKVFBGVzJINFoyIiwiciI6Im1hc3RlciIsInMiOiJzaGEyNTY6OExlNnJ0cGI4VkRUWXBsaG5wU28yZnIxUlp6c3AtcUhMRXBpbVhnV1hkbyIsInQiOjE3OTA1NzkyODg1MDEsInYiOjEsIngiOiJhZHJhaS8xLjAuMCJ9 -->
