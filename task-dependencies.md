# Task dependency scopes

Prerequisites connect tasks at the same hierarchy level. Both endpoints must
be roots, or both must have the same non-null `parent_id`. Done and cancelled
tasks retain this classification. Existing workspace authorization, project
placement checks and explicit dependency cycle checks still apply.

V040 enforces this rule in PostgreSQL for dependency inserts and endpoint
updates, and for incident links when tasks are attached, detached, reparented
or restored. Statement transition tables check settled bulk placements; moving
both endpoints together can remain valid. Existing planning serialization also
covers concurrent writers and raw SQL. A rejection rolls back the statement's
topology, readiness, audit and change-stream effects. REST reports HTTP 409
`lifecycle_conflict`, code `TASK_DEPENDENCY_SCOPE_MISMATCH`, with the conflicting
endpoints and parent IDs. MCP preserves that structured error. Explicit cycles
retain their HTTP 400 `dependency_cycle` response.

## Upgrade cleanup

The migration removes every saved prerequisite edge whose endpoint parent IDs
differ, including edges involving terminal or soft-deleted tasks. It keeps all
root-to-root and same-parent sibling edges and every task hierarchy link.
For example, if P owns C and W, edges P→C, P→W and root K→W are removed;
the root edge P→K remains. This does not close K or remove P's valid prerequisite
gate. P remains automatically blocked while it has unfinished subtasks, and
eligible children can start when P's remaining prerequisites and ancestry allow.

Cleanup uses normal readiness, audit and outbox triggers with migration
attribution. Its ledger entry commits in the same transaction. A failure rolls
back cleanup and registration together; rerunning a registered migration does
not repeat cleanup. No migration changes existing parent IDs or adds a new
cross-project dependency restriction.

## Read-only inspection

After V040, an operator with database access can inspect one workspace:

```sql
SELECT hmem_task_dependency_scope_diagnostics(
  '00000000-0000-0000-0000-000000000001'::uuid, 100);
```

Replace the example UUID with the workspace ID. The result includes an exact
`total_count`, up to 100 endpoint/parent pairs in `items`, and `truncated`.
The function makes no changes. A privileged historical import may still need
repair: remove its forbidden links before changing their hierarchy. Dependency
removal and unrelated metadata updates remain available while other violations
exist. Upgrade cleanup removes only hierarchy-scope violations; other historical
placement issues keep their existing handling.
