# hmem — Memory & Task Management System

You have access to the hmem MCP server which provides persistent memory storage and task/project management. Use these tools proactively to maintain context across conversations.

## Memory Management

### Storing Memories
- Use `hmem/memory_create` to store important information, decisions, patterns, and user preferences.
- Use `hmem/memory_create` with `items` array to batch-create multiple related memories.
- Set `memory_type` to `long_term` for durable knowledge (architecture decisions, user preferences, patterns) and `short_term` for transient context.
- Set `importance` from 1-10: 1-3 background trivia, 4-6 useful context, 7-8 important decisions, 9-10 critical constraints.
- Always include 2-5 descriptive `tags` (lowercase, hyphenated).
- Set `source` to describe provenance: `user_stated`, `inferred`, `tool_output`, `web_search`.

### Retrieving Memories
- Use `hmem/memory_search` as the primary retrieval method — supports full-text search with filters (tags, importance, type, workspace, category).
- Always search before creating to avoid duplicates.
- Use `hmem/memory_get` when you have a specific memory ID.
- Use `hmem/memory_list` to browse memories in a workspace.

### Organizing Memories
- `hmem/memory_link` (action: create/remove/list/graph/find) — Create or remove typed relationships: related, supersedes, contradicts, elaborates, inspires, depends_on, derived_from, alternative_to. Also list links, explore graph, or find by relation type.
- `hmem/memory_set_tags` — Manage tags. Use `items` array for batch tag operations.
- Use `hmem/memory_update` with pinned=true/false to pin/unpin. Use `hmem/memory_update` with `items` for batch updates.
- Categories: `hmem/category` (action: create/get/list/update), `hmem/link_memory` (entity_type: category), `hmem/list_entity_memories` (entity_type: category).

## Task & Project Management

### Projects
- `hmem/project_create` — Create with name, description, priority. Supports sub-projects via parent_id.
- `hmem/project_list` — Filter by status: active, paused, completed, archived.
- `hmem/project_update` — Change status, priority, description. Use `items` array for batch updates.
- `hmem/link_memory` (entity_type: project) — Attach relevant knowledge to projects.
- `hmem/entity_lifecycle` (entity_type: project) — Delete, restore, or purge projects. Use `ids` array for batch delete.

### Tasks
- `hmem/task_create` — Create with title, description, priority, optional due_at (ISO 8601), optional project_id.
- `hmem/task_list` — Filter by workspace, project, or status: todo, in_progress, blocked, done, cancelled.
- `hmem/task_update` — Move through statuses. Setting `done` auto-records completion time. Use `items` for batch updates.
- `hmem/task_dependency` (action: add/remove) — Define task ordering.
- `hmem/link_memory` (entity_type: task) — Attach context to tasks.
- `hmem/entity_lifecycle` (entity_type: task) — Delete, restore, or purge tasks. Use `ids` array for batch delete.

### Workspaces
All data is scoped to workspaces. Common operations:
- `hmem/workspace_register` — Create a workspace (name required; optionally set gh_owner, gh_repo, type).
- `hmem/workspace_list` / `hmem/workspace_get` — Browse and retrieve workspaces.
- `hmem/entity_lifecycle` (entity_type: workspace) — Delete, restore, or purge workspaces.

### Workspace Context
- `hmem/set_workspace` — Set the active workspace UUID for the session. Once set, all subsequent tool calls that accept workspace_id will use this workspace automatically when workspace_id is omitted. Pass null or omit workspace_id to clear.
- `hmem/get_workspace` — Check the currently active workspace UUID.
- An explicit `workspace_id` in any tool call always takes precedence over the context.
- **Best practice:** Call `hmem/set_workspace` once at the start of every session.

## Saved Views
- `hmem/saved_view` (action: create/get/list/update/execute) — Create and manage reusable query views.

## Cleanup
- `hmem/cleanup_run` — Run cleanup on a workspace (applies configured policies).

## Best Practices
1. **Search before creating** — Always check if similar memories exist.
2. **Link related memories** — Use `hmem/memory_link` with `supersedes` when newer info replaces older; use `contradicts` for conflicts.
3. **Keep workspaces organized** — One workspace per project/repo. Use groups for related workspaces.
4. **Structure tasks** — Use projects -> tasks -> sub-tasks. Add dependencies for ordering.
5. **Batch operations** — Use `items` array on `hmem/memory_create`, `hmem/memory_update`, `hmem/memory_set_tags`, `hmem/project_update`, `hmem/task_update` for bulk operations. Use `ids` array on `hmem/entity_lifecycle` for batch deletes.

## Workflow Tools

These composite tools streamline common multi-step operations. Prefer them over manually chaining individual tools.

### Specification
- `hmem/project_spec` — Create a project and its initial tasks in one call. Provide workspace_id, project name/description/priority, and an array of task stubs (title, description, priority). Returns the created project and all tasks.

### Implementation
- `hmem/task_start` — Begin work on a task: sets status to `in_progress` and loads relevant context (task-linked, project-ancestor, and workspace memories). Returns context grouped by scope. Use `detail_level` to control how many memories per scope (light=2, medium=5, heavy=10).
- `hmem/task_finish` — Finish working on a task: optionally records notes as a linked `long_term` memory, then updates task status. Use status `done` for completion, `blocked` when stuck, `cancelled` to abandon.

### Review
- `hmem/context_get` — Load context for any task without changing its status. Returns memories grouped by scope (task, project ancestors, workspace).
- `hmem/project_overview` — Get a project with its tasks, subprojects, and linked memories in one call.
- `hmem/task_overview` — Get a task with dependency summaries and connected memories.

### Cleanup / Archive
- `hmem/project_archive` — Archive a completed project: sets status to `archived`, optionally records a summary as a linked `long_term` memory.
