# MCP compact response contract

This document defines the response-shaping contract for the slim hmem MCP tool
surface. It is the design target for MCP responses; the REST API can keep its
full resource shapes for compatibility while MCP returns agent-oriented,
signal-only JSON inside the MCP `content[0].text` envelope.

Contract id: `mcp-compact-response/v1`.

## Goals

- Keep IDs, statuses, names/titles, priorities, targets, and next actions visible.
- Omit fields that do not change an agent decision by default.
- Preserve structured errors so agents can branch on error codes and actionable
  details.
- Avoid expanding the slim MCP tool surface while compacting response payloads.
- Let callers move from summaries to detail with existing detail tools or explicit
  optional detail/include flags added in a backward-compatible way.

## MCP envelope

Successful tool calls continue to return MCP content blocks:

```json
{
  "content": [
    { "type": "text", "text": "{...compact JSON...}" }
  ]
}
```

The `text` value should be valid compact JSON for JSON-capable responses. Empty
REST responses must be converted to an explicit acknowledgement instead of an
empty string.

Errors keep MCP error semantics:

```json
{
  "isError": true,
  "content": [
    { "type": "text", "text": "[ERROR_CODE] Human-actionable message" }
  ],
  "error": {
    "type": "validation|workflow_conflict|lifecycle_conflict|...",
    "http_status": 400,
    "code": "ERROR_CODE",
    "message": "Human-actionable message",
    "hint": "Optional recovery hint",
    "required_action": "Optional next action",
    "detail": { "field": "optional structured diagnostics" }
  }
}
```

The `error` object is omitted only when no structured detail exists. Sensitive
server text remains redacted/truncated before it reaches MCP output.

## Default field omission policy

Default compact responses drop these fields unless a tool-specific row below says
they are actionable:

- timestamps: `created_at`, `updated_at`, `last_accessed_at`, `completed_at`;
- repeated `workspace_id` on workspace-scoped entities when the active MCP
  workspace context already supplies it;
- empty `metadata` objects;
- null fields, including null `description`, `parent_id`, `project_id`, `due_at`,
  `expires_at`, `source`, and GitHub owner/repo fields;
- full memory `content` and full project/task descriptions except detail tools;
- non-actionable counts, especially dependency/memory counts that are zero or not
  tied to a blocker, completion gate, pagination boundary, or dependency effect;
- transport/debug fields such as internal FTS language and access counters.

Fields retained by default:

- entity `id` values and relationship IDs required for follow-up calls;
- entity names/titles, statuses, memory type, importance, priority, tags, and
  pinned state when true;
- actionable dates such as non-null task `due_at`;
- readiness/blocking data that changes the next action;
- `has_more` on paginated or limit-bounded lists.

## Detail and full-output policy

The default for every MCP tool is compact. Full/detail output is allowed only in
one of these ways:

1. **Dedicated detail tools** already present in the slim surface:
   `memory_get`, `project_overview`, `task_overview`, `context_get`, and
   `task_start` after a successful start.
2. **Existing bounded detail knobs**, such as `detail_level` on `context_get` and
   `task_start`.
3. **Future optional flags** (`detail=true`, `include_content=true`,
   `include_descriptions=true`, `include_metadata=true`, or similarly named
   `include_*` fields). These flags must be optional, bounded, and documented;
   adding them must not create new required inputs or new tools.

Even in detail mode, MCP should still omit transport/debug fields, repeated
workspace IDs, empty metadata, and nulls unless the caller explicitly requested
them and they are useful for the current workflow.

## DTO vocabulary

### `MutationAck`

Acknowledgement for create/update/link/unlink/archive/finish operations.

Required fields:

- `ok: true`
- `action`: concise verb, for example `created`, `updated`, `linked`, `unlinked`,
  `archived`, `started`, or `finished`
- `entity_type`: `workspace_context`, `workspace`, `memory`, `memory_link`,
  `project`, `task`, or `task_dependency`

Optional fields:

- `id` or tool-specific IDs such as `memory_id`, `project_id`, `task_id`,
  `source_id`, `target_id`, `depends_on_id`
- `status` when lifecycle state changed
- `summary`: an `EntitySummary` for the mutated entity
- `changed_fields`: field names supplied to update-like mutations when known
- `dependency_effects`: non-empty `DependencyEffectSummary` rows
- `notes_memory`: `MemorySummary` when `task_finish` created a notes memory
- `warnings`: bounded strings for partial composite workflows

### `EntitySummary`

Shared compact identity fields for entities.

- `WorkspaceSummary`: `{ id, name, workspace_type }`, with GitHub owner/repo only
  when present and relevant.
- `MemorySummary`: `{ id, summary?, memory_type, importance, tags?, pinned? }`.
  `content_preview` may appear in list/search rows when no summary exists; full
  `content` is detail-only.
- `ProjectSummary`: `{ id, name, status, priority, parent_id? }` plus
  `description_preview` only when useful for search disambiguation.
- `TaskSummary`: `{ id, title, status, priority, project_id?, parent_id?, due_at? }`.
  `project_id` and `parent_id` are omitted only when null or already obvious from
  the surrounding object.

### `MemoryDetail`

Dedicated full-memory detail shape returned by `memory_get`.

Required fields:

- all `MemorySummary` fields;
- full `content`.

Optional fields:

- non-empty `metadata`;
- non-null `expires_at`, `source`, and `confidence` when useful to the caller;
- link targets or graph edges only when a future `include_links=true` flag is
  added.

It still omits timestamps, repeated workspace IDs, internal FTS language, and
access counters by default.

### `SearchRow`

Rows returned by unified search or browsing.

- `MemorySearchRow`: `MemorySummary` plus `content_preview` when the summary is
  missing or the query match needs context.
- `ProjectSearchRow`: `{ project: ProjectSummary, linked_memories? }`.
- `TaskSearchRow`: `{ task: TaskSummary, linked_memories? }`.
- `linked_memories` contains bounded `LinkedMemorySummary` rows.

`LinkedMemorySummary`: `{ id, summary?, importance, tags? }`. It never includes
full `content`.

Search results use:

```json
{
  "memories": [],
  "projects": [],
  "tasks": []
}
```

Omit empty arrays only if all entity sections are absent by request; otherwise keep
the requested sections visible so callers can distinguish "searched and empty"
from "not requested".

### `OverviewSummary`

Aggregated project/task detail responses.

- `ProjectOverviewSummary`:
  `{ project, tasks, subprojects, connected_memories, readiness_rollup }`
- `TaskOverviewSummary`:
  `{ task, dependencies, connected_memories, readiness_rollup }`

`tasks` and `subprojects` are summary rows.

`TaskDependencySummary`: `{ id, title|name, status? }`. Status is included when
known or when it explains why a task is blocked.

`ConnectedMemorySummary`: `{ id, summary, scope }`, where `scope` is `task`,
`project`, or `workspace`. It never includes full `content`.

`readiness_rollup` includes `completion_ready` and only counts that affect a next
action, such as non-zero open tasks, blocked tasks, dependency-blocked tasks, open
dependencies, or open subtasks. Zero counts can be omitted except where an empty
zero is the answer to an explicit readiness question.

### `ContextSummary`

Returned by `context_get` and successful `task_start`:

```json
{
  "task": { "id": "...", "title": "...", "status": "in_progress", "priority": 10 },
  "detail_level": "medium",
  "task_memories": [],
  "project_memories": [],
  "workspace_memories": []
}
```

Each memory row is `ConnectedMemorySummary`. The `detail_level` parameter bounds
how many rows per scope are returned; it does not authorize full memory content.
Use `memory_get` for full content.

### `GraphSummary`

No retained slim MCP tool currently exposes the full memory graph. The
`memory_link` list action returns a bounded edge list for one memory, not a graph
traversal. The compact DTO vocabulary reserves graph shapes so a future optional
graph response does not reintroduce raw `Memory`/`MemoryLink` payloads:

- `GraphNode`: `MemorySummary` plus optional `content_preview`.
- `GraphEdge`: `{ source_id, target_id, relation_type, strength? }`.

Timestamps on links remain omitted. Graph responses should be bounded by an
explicit depth/limit input.

### `NextTaskCandidateSummary`

Rows returned by `project_next_tasks` and ready-alternative diagnostics in
`task_start` errors:

```json
{
  "task": { "id": "...", "title": "...", "status": "todo", "priority": 10 },
  "dependency_blocked": false,
  "completion_gated": true,
  "open_descendant_count": 2,
  "open_dependency_count": 1
}
```

`completion_gated`, `open_descendant_count`, and `open_dependency_count` are
included only when they are true/non-zero or when `include_blocked=true` requests
blocked diagnostics.

### `DependencyEffectSummary` and `DependencyMutationSummary`

Dependency mutation responses summarize status changes caused by dependency
auto-blocking.

`DependencyEffectSummary`:

```json
{
  "task": { "id": "...", "title": "...", "status": "blocked", "priority": 7 },
  "previous_status": "todo",
  "current_status": "blocked",
  "auto_blocked": true,
  "open_dependency_count": 1,
  "reason": "dependency_added"
}
```

Omit unchanged previous/current fields and zero counts when there was no status or
auto-blocking change.

`DependencyMutationSummary`:

```json
{
  "ok": true,
  "action": "add",
  "entity_type": "task_dependency",
  "task_id": "...",
  "depends_on_id": "...",
  "affected_tasks": []
}
```

`affected_tasks` is present only when non-empty, except when an explicit
dependency operation needs to confirm that no dependent task status changed.

### `WorkflowSummary`

Composite tools return a `WorkflowSummary` that records the high-value outcome of
multi-step workflows:

- `task_start`: `ContextSummary` plus `started: true`; blocked starts return a
  `StructuredError` with `status_unchanged: true`, `blockers`, and bounded
  `ready_alternatives`.
- `task_finish`: `MutationAck` for the final task status and optional
  `notes_memory` summary.
- `project_spec`: project summary plus `tasks_created` task summaries and
  `tasks_failed` when non-zero.
- `project_archive`: archive acknowledgement plus optional summary-memory ID.

### `StructuredError`

Structured errors preserve every field needed for recovery:

- `code` is stable enough for agent branching.
- `message`, `hint`, and `required_action` are human/action oriented.
- `detail` may include field validation failures, blocker summaries, ready
  alternatives, or dependency effects.
- Error detail should already be compacted with the same DTO vocabulary.

## Tool-to-response mapping

| MCP tool | Default compact response shape | Detail/full path |
| --- | --- | --- |
| `set_workspace` | `MutationAck { ok, action: "set"|"cleared", entity_type: "workspace_context", workspace_id? }` | None. |
| `get_workspace` | `{ workspace_id: UUID|null }` | None. |
| `workspace_list` | `{ items: [WorkspaceSummary], has_more? }` | Future optional `detail=true` may include GitHub owner/repo when relevant. |
| `workspace_register` | `MutationAck` with `summary: WorkspaceSummary`. | Use `workspace_list`/future workspace detail if needed. |
| `search` | `{ memories: [MemorySearchRow], projects: [ProjectSearchRow], tasks: [TaskSearchRow] }`. | Use `memory_get`, `project_overview`, `task_overview`, or `context_get` for details. |
| `memory_create` | `MutationAck` with `summary: MemorySummary` and target IDs used at creation. | Use `memory_get` after creation for full content. |
| `memory_get` | `MemoryDetail`: `MemorySummary` plus full `content`, optional non-empty metadata, optional expiry/source/confidence, and tags. | This is the dedicated memory detail tool. |
| `memory_update` | `MutationAck` with updated `summary: MemorySummary`; include `changed_fields` and replaced `tags` when tags were changed. | Use `memory_get` for full content/metadata. |
| `memory_link` | For create/remove: `MutationAck` with `source_id`, `target_id`, and `relation_type`; for list: `{ links: [GraphEdge] }`. | Future optional `detail=true` may include bounded endpoint memory summaries. |
| `link_memory` | `MutationAck` with `entity_type`, `entity_id`, and `memory_id`. | Use the entity overview or `memory_get` for details. |
| `project_create` | `MutationAck` with `summary: ProjectSummary`. | Use `project_overview` for project detail. |
| `project_update` | `MutationAck` with updated `summary: ProjectSummary`; include `changed_fields` and `status` when changed. | Use `project_overview` for project detail. |
| `project_overview` | `ProjectOverviewSummary`. Root project may include full description; child tasks/subprojects remain summaries. | This is the dedicated project detail/overview tool; future `include_descriptions=true` may expand child descriptions. |
| `project_next_tasks` | `{ items: [NextTaskCandidateSummary] }` where each row includes `task: TaskSummary`, `dependency_blocked`, and only non-zero actionable gate counts. | `include_blocked=true` includes blocked diagnostics; `task_overview` explains a selected task. |
| `project_spec` | `WorkflowSummary` with `project: ProjectSummary`, `tasks_created: [TaskSummary]`, and `tasks_failed` only when non-zero. | Use `project_overview` after creation for full context. |
| `project_archive` | `MutationAck` with archived `ProjectSummary`, `changed_fields: ["status"]`, and optional summary-memory ID. | Use `memory_get` for summary-memory content if needed. |
| `task_create` | `MutationAck` with `summary: TaskSummary`. | Use `task_overview` or `context_get` for detail. |
| `task_update` | `MutationAck` with updated `summary: TaskSummary`, `changed_fields`, and non-empty `dependency_effects`. | Use `task_overview` for dependencies/memory context. |
| `task_overview` | `TaskOverviewSummary`. Root task may include full description; dependency and memory rows remain summaries. | This is the dedicated task detail/overview tool; future `include_description=false|true` can tune description size. |
| `context_get` | `ContextSummary` bounded by `detail_level`. | Increase `detail_level` for more summaries; use `memory_get` for full memory content. |
| `task_dependency` | `DependencyMutationSummary`: `ok`, action, `entity_type`, `task_id`, `depends_on_id`, and non-empty `affected_tasks` as `DependencyEffectSummary`. | Use `task_overview` for the resulting task state. |
| `task_start` | On success, `WorkflowSummary`/`ContextSummary` with `started: true`; on blocked preflight, `StructuredError` with blockers and ready alternatives. | `detail_level` controls context breadth; `task_overview` explains blockers. |
| `task_finish` | `MutationAck` with final task `status`, `changed_fields: ["status"]`, and optional `notes_memory`. | Use `memory_get` for saved notes or `task_overview` for final readiness. |

## Compatibility and deprecation notes

- This contract is MCP-only. REST endpoints can continue returning full server
  DTOs (`Memory`, `Project`, `Task`, paginated wrappers, and composite response
  types) for existing HTTP/web clients.
- Existing MCP clients that parse raw server DTO fields from `content[0].text`
  may need to follow the summary/detail flow instead of expecting full payloads
  from mutation or list-like tools.
- Rolling out this contract changes default MCP response shapes and should be
  treated as a breaking MCP response-contract change for brittle field-level
  parsers. Prefer a tool annotation/version bump plus release notes even when
  input schemas remain unchanged.
- No removed list/get/batch tools are restored. `search` remains the browsing
  replacement for legacy `memory_list`, `memory_search`, `project_list`, and
  `task_list` tools.
- Saved-view entity labels such as `memory_search` and `memory_list` are persisted
  user data and are not rewritten by this MCP response contract.
- Optional detail/include flags are additive compatibility affordances. They must
  not become required inputs and should not bypass redaction or bounded output
  limits.
- The `mcp-compact-response/v1` contract name identifies this design, not a
  promise that future response fields can never be removed. The stable migration
  path is summary by default plus explicit detail retrieval.
