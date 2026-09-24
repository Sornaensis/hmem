+++
schema = "adrai/decision/v1"
adr = "A01M39R0FHX6Y5VC60CAHQDAX0D"
record = "R01M39R0FV22QYTSW5C9GY5EJYE"
title = "Model work as hierarchical projects and constrained task dependencies"
summary = "Keep planning hierarchy and an acyclic prerequisite graph with database-enforced lifecycle gates."
domains = ["planning-workflow"]
+++

Context: Projects organize planning work, tasks can have subtasks, and tasks can depend on other tasks. This record describes observed behavior; the historical reason for the choice is not established by these sources.

Observed decision: Store project and task parent links and task_dependencies in PostgreSQL. Reject dependency cycles. Keep active task nesting flat: top-level tasks may have subtasks, but a subtask may not have children and can start only while its parent is in progress. Gate task completion on closing open descendants and project closure on closing descendant projects and tasks. Derive next actionable tasks recursively across eligible project subtrees, distinguishing open-prerequisite blocking from the separate open-descendant completion gate.

Consequences: The hierarchy and prerequisite graph make work order explicit. Invalid transitions fail, and moving, reparenting, starting, or completing work requires checks against related parents, descendants, projects, and dependencies.

Evidence:
- hmem-core/src/HMem/DB/Task.hs: validates task placement and related dependencies, and computes next tasks across project subtrees with separate dependencyBlocked and completionGated annotations.
- hmem-core/src/HMem/DB/Project.hs: stores and traverses project parent links and coordinates project subtree operations with associated tasks.
- hmem-server/migrations/V017__flat_subtask_lifecycle_rules.sql: enforces flat active subtasks and parent-in-progress eligibility.
- hmem-server/migrations/V027__bounded_task_dependency_cycle_check.sql: rejects dependency cycles with a bounded recursive reachability check.
- hmem-server/migrations/V012__recursive_lifecycle_invariants.sql: rejects task completion with open descendants and project closure with open descendant projects or tasks.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0ifSwiYiI6IjBjN2I3MDY5NDcxODdiMGE3YmFhNDYzMjc2MDMzZGYwOWM0ZTY1ZGYiLCJpIjoic2hhMjU2OktFanhTNFFLbmc0UWY3MXhkcnVndXFKejVRYkpwLUw0cTdrbHJ1bE41NWciLCJrIjoiZGVjaXNpb24uY3JlYXRlIiwibyI6IlIwMU0zOVIwRlYyMlFZVFNXNUM5R1k1RUpZRSIsIm9wIjoiTzAxTTM5UjBGVjIyUVlUU1c1QzlHWTVFSllFIiwiciI6Im1hc3RlciIsInMiOiJzaGEyNTY6X1VWUExBWVJPNEY2UG1KVzFQclljSHA1SlYtRnNKQmZ3RDU0X1UzY3Q4RSIsInQiOjE3OTAyNTQ3OTI1NDYsInYiOjEsIngiOiJhZHJhaS8xLjAuMCJ9 -->
