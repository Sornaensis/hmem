+++
schema = "adrai/decision/v1"
adr = "A01M39R0FHX6Y5VC60CAHQDAX0D"
record = "R01M3YDHCKS856BYWSEMC1Z3YF7"
title = "Model work as hierarchical projects and constrained task dependencies"
summary = "Keep planning hierarchy and an acyclic prerequisite graph with database-enforced lifecycle gates."
domains = ["planning-workflow"]
+++

Context: Projects organize planning work, tasks can have subtasks, and tasks can depend on other tasks. This record describes observed behavior; the historical reason for the original choice is not established by these sources.

Observed decision: Store project and task parent links and task_dependencies in PostgreSQL. Reject dependency cycles. Keep active task nesting flat: top-level tasks may have subtasks, but a subtask may not have children and can start only while its parent is in progress. Gate task completion on closing open descendants and project completion on closing descendant projects and tasks. Derive next actionable tasks recursively across eligible project subtrees, distinguishing open-prerequisite blocking from the separate open-descendant completion gate.

Explicit project archival recursively archives active descendant projects, including completed projects, and cancels active non-done tasks throughout the project and task hierarchy. Explicit task cancellation cancels active non-done subtasks. Preserve done cascade-descendant tasks, including their completion and update timestamps, and leave soft-deleted rows untouched. An explicitly cancelled root task may transition from done according to the existing API semantics. Preserve prerequisite edges; cancellation does not traverse dependency edges, while normal readiness recomputation includes dependents of all actually changed tasks.

Require reopening a cancelled parent before creating, moving, reparenting, or reopening an unfinished subtask beneath it. Reject the requested unfinished write with TASK_OPEN_UNDER_CANCELLED_TASK rather than silently cancelling it. Completed children may remain under a cancelled parent. Reopening the parent does not reopen its children. Preserve the separate done-parent, closed-project, flat-subtask and parent-in-progress rules, and verify placement ownership before revealing parent lifecycle state.

Enforce cascades and cancelled-parent closure prospectively through database triggers, without backfilling historical rows. Explicit repeated terminal status requests normalize descendants; application metadata-only updates carry transaction-local status intent so they do not sweep historical descendants. Ordered AFTER propagation completes descendants before closure checks and avoids rewriting pending outer UPDATE targets when a statement includes both parent and child. Use deterministic project-before-task and root-to-leaf locking, suppress recursive propagation, and recompute readiness over the actual changed task seeds. Core single and batch updates, direct SQL, REST, MCP and audit reverts share this authority and preserve atomic per-entity audit and canonical outbox attribution.

Consequences: The hierarchy and prerequisite graph make work order explicit. Completion remains a deliberate gated transition; archive and cancel explicitly close unfinished hierarchy work while retaining completed task history. Invalid placement and lifecycle transitions fail with structured guidance. Cascade updates, readiness effects, audit records and outbox events commit or roll back together.

Evidence:
- hmem-core/src/HMem/DB/Task.hs: validates placement, preserves explicit status intent, captures changed hierarchy and dependency snapshots, and computes next tasks with separate dependencyBlocked and completionGated annotations.
- hmem-core/src/HMem/DB/Project.hs: stores and traverses project parent links, coordinates project subtree operations and carries explicit status intent.
- hmem-server/migrations/V017__flat_subtask_lifecycle_rules.sql: enforces flat active subtasks and parent-in-progress eligibility.
- hmem-server/migrations/V027__bounded_task_dependency_cycle_check.sql: rejects dependency cycles with a bounded recursive reachability check.
- hmem-server/migrations/V012__recursive_lifecycle_invariants.sql: rejects task completion with open descendants, project completion with open descendant projects or tasks, and unfinished work under closed ancestors.
- hmem-server/migrations/V029__cascade_archive_and_cancel.sql: enforces archive/cancel cascades, cancelled-parent placement rejection and aggregate readiness recomputation without historical normalization.
- hmem-core/test/HMem/DB/ProjectSpec.hs, TaskSpec.hs and ChangeStreamSpec.hs: exercise cascade preservation, repeated intent, raw and overlapping SQL, batch parity, atomic rollback and concurrent child insertion.
- hmem-server/test/HMem/Server/APISpec.hs: exercises REST cascade behavior, structured cancelled-parent rejection and audit-revert parity.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0ifSwiYiI6ImU4NDc4ZjNiYWNkZDgwMTJmMmViN2ZlZjFmYmQxZWYwODYzODA3OWQiLCJpIjoic2hhMjU2OjdibXFwaGhva3hHUlhnOWZGTndKWnU4R01CVFQ3QVpDNHI5eG5yUE12cTQiLCJrIjoiZGVjaXNpb24uYW1lbmQiLCJvIjoiUjAxTTNZREhDS1M4NTZCWVdTRU1DMVozWUY3Iiwib3AiOiJPMDFNM1lESENLUzg1NkJZV1NFTUMxWjNZRjciLCJwIjpbIlIwMU0zOVIwRlYyMlFZVFNXNUM5R1k1RUpZRSJdLCJyIjoibWFzdGVyIiwicyI6InNoYTI1Njp3VXdZUGZpQWVub1QyV1hzOVhjQ0ZKRWgzUTJUTVJYeTFac2R5WXQ4NGdNIiwidCI6MTc5MDk0ODQ1NTAzMywidiI6MSwieCI6ImFkcmFpLzEuMC4wIn0 -->
