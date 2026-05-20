# Memory creation migration and compatibility plan

This note records the compatibility policy for the opinionated memory-creation
rules introduced by `V013__explicit_memory_creation_links.sql` and
`V015__require_explicit_memory_type.sql`.

## Current invariants

- New memories must specify `memory_type` as `short_term` or `long_term`.
- New memories must be created with at least one valid same-workspace project or
  task link.
- The HTTP API, web UI, MCP, and generated agent guidance expose the stricter
  user-facing creation rule: create memories only against projects or top-level
  tasks. If the context is a subtask, use the containing project or nearest
  top-level task instead.
- The low-level link tables and direct SQL maintenance path remain the
  compatibility boundary for existing subtask-linked rows while user-facing
  creation surfaces enforce the top-level-task targeting policy.

## Upgrade behavior

### Existing unlinked memories

Legacy active memories that were already present before the explicit-link
invariant are **grandfathered**. They are not auto-linked to a guessed project or
task because doing so would invent context and potentially change search or
audit semantics.

After `V013` is applied:

- existing unlinked active memories remain readable/searchable;
- new unlinked memory inserts fail with `MEMORY_LINK_REQUIRED` / SQLSTATE
  `HM301`;
- deleting the last active project/task link from a non-deleted memory fails;
- restoring a soft-deleted unlinked memory fails until it has a valid link.

Operators who want to remediate old unlinked rows can list them with:

```sql
SELECT m.id, m.workspace_id, m.summary, left(m.content, 120) AS content_preview
  FROM memories m
 WHERE m.deleted_at IS NULL
   AND NOT EXISTS (SELECT 1 FROM project_memory_links pml WHERE pml.memory_id = m.id)
   AND NOT EXISTS (SELECT 1 FROM task_memory_links tml WHERE tml.memory_id = m.id)
 ORDER BY m.created_at, m.id;
```

Remediation is deliberately manual: link each row to an appropriate project or
top-level task, or leave it grandfathered until the operator has enough context.

### Existing null/default memory types

`V015` backfills any drifted legacy `NULL` `memory_type` values to
`short_term`, drops any default, and installs a trigger that rejects later
attempts to insert or update a memory without an explicit type.

This keeps upgraded databases bootable while making all future writes explicit.
There is no compatibility flag to re-enable defaults.

### Saved views

Saved views are preserved as user data. Their `entity_type` values (for example
`memory_search` and `memory_list`) are saved-view categories, not MCP tool names,
so they are not rewritten by MCP tool-surface slimming. Older saved views remain
valid and can still be used by clients that understand those backend query
shapes. Future UI work may add a migration-assisted rename, but the safe default
is to keep existing saved views untouched.

### MCP agents and generated configs

MCP remains a strict agent-facing surface:

- `memory_create` requires `content`, explicit `memory_type`, and at least one
  creation target (`project_id` and/or top-level `task_id`);
- subtask task IDs are rejected with `SUBTASK_MEMORY_TARGET_NOT_ALLOWED`;
- `link_memory` attach and detach actions also reject subtask task IDs, so MCP
  agents cannot add or remove a direct subtask memory link;
- `task_finish` notes resolve a subtask to its nearest eligible top-level task or
  project instead of linking directly to the subtask;
- generated Copilot, Claude, and OpenCode configs document only the current slim
  MCP tools and the explicit targeting/type rules.

Older agent prompts that omit `memory_type` or targets should fail fast with
actionable validation errors rather than silently creating ambiguous memories.
If an operator must remediate an existing subtask-linked memory, use direct SQL
or a dedicated maintenance script in a maintenance window to add a replacement
project/top-level-task link before removing the subtask link; otherwise the
deferred creation-link invariant rejects removing the last active link.

### HTTP and older clients

The REST API keeps broad endpoints for compatibility, but it does not keep legacy
defaults:

| Client behavior | Current result |
| --- | --- |
| `POST /api/v1/memories` without `memory_type` | `400` validation error mentioning `memory_type`; DB-level drift maps to `MEMORY_TYPE_REQUIRED` |
| `POST /api/v1/memories` without project/task target | `400` validation error mentioning the missing target; DB-level drift maps to `MEMORY_LINK_REQUIRED` |
| `POST /api/v1/memories` with a subtask or deleted task target | `400` validation error mentioning active top-level task targets |
| `POST /api/v1/memories/batch` with any item missing `memory_type` or target | Entire batch is rejected with per-item validation text; no partial create is expected |
| Cross-workspace project/task target | `400` validation/workflow error / `MEMORY_LINK_CROSS_WORKSPACE` |
| Existing saved view with legacy entity label | Preserved; no automatic rewrite |
| Existing active unlinked memory | Grandfathered and readable/searchable |

The HTTP API now rejects subtask memory creation targets so REST, web, and MCP
creation semantics match. Existing subtask-linked rows are not rewritten by this
API rule; operator remediation remains a deliberate maintenance activity because
rewriting those rows requires project-specific context.

## Feature flags and rollout

No feature flag is provided. The rollout policy is:

1. Apply migrations normally through `hmem-ctl start` / setup.
2. Let `V013` grandfather existing unlinked rows and enforce links for new rows.
3. Let `V015` backfill drifted null types to `short_term` and enforce explicit
   types for future writes.
4. Update MCP agents, web UI flows, and any custom HTTP clients to submit
   explicit type and target values before or with deployment; legacy clients that
   omit either field will start receiving 400-class validation errors.
5. Optionally run the unlinked-memory query above and manually link rows where
   the correct context is known.

## Verification coverage

Upgrade behavior is covered by `hmem-core:test:hmem-core-test` in
`HMem.DB.TestHarnessSpec`:

- `legacyUnlinkedMigration preserves legacy unlinked memories when applying the
  explicit-link migration` verifies that pre-`V013` unlinked rows survive and new
  unlinked writes fail.
- `explicitMemoryTypeMigration backfills null rows and rejects new missing types`
  verifies that pre-`V015` null types become `short_term` and future missing-type
  writes fail.

API behavior is covered by `hmem-server:test:hmem-server-test` under the
`explicit memory creation links` examples for required type, required target,
same-workspace target validation, subtask/deleted task target rejection, and
atomic project/task link creation.

Saved-view preservation is covered indirectly by workspace lifecycle tests that
create `memory_list` saved views and verify they survive normal create/read and
soft-delete flows. A future compatibility test should seed pre-slimming
`memory_search`/`memory_list` saved views before applying memory migrations and
assert that the rows are neither rewritten nor rejected.
