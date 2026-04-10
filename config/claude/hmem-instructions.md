# hmem — Memory & Task Management System

You have access to the hmem MCP server which provides persistent memory storage and task/project management. Use these tools proactively to maintain context across conversations.

## Memory Management

### Storing Memories
- Use `memory_create` to store important information, decisions, patterns, and user preferences.
- Use `memory_create` with `items` array to batch-create multiple related memories.
- Set `memory_type` to `long_term` for durable knowledge (architecture decisions, user preferences, patterns) and `short_term` for transient context.
- Set `importance` from 1-10: 1-3 background trivia, 4-6 useful context, 7-8 important decisions, 9-10 critical constraints.
- Always include 2-5 descriptive `tags` (lowercase, hyphenated).
- Set `source` to describe provenance: `user_stated`, `inferred`, `tool_output`, `web_search`.

### Retrieving Memories
- Use `memory_search` as the primary retrieval method — supports full-text search with filters (tags, importance, type, workspace, category).
- Always search before creating to avoid duplicates.
- Use `memory_get` when you have a specific memory ID.
- Use `memory_list` to browse memories in a workspace.

### Organizing Memories
- `memory_link` (action: create/remove/list/graph/find) — Create or remove typed relationships: related, supersedes, contradicts, elaborates, inspires, depends_on, derived_from, alternative_to. Also list links, explore graph, or find by relation type.
- `memory_set_tags` — Manage tags. Use `items` array for batch tag operations.
- Use `memory_update` with pinned=true/false to pin/unpin. Use `memory_update` with `items` for batch updates.
- Categories: `category` (action: create/get/list/update), `link_memory` (entity_type: category), `list_entity_memories` (entity_type: category).

## Task & Project Management

### Projects
- `project_create` — Create with name, description, priority. Supports sub-projects via parent_id.
- `project_list` — Filter by status: active, paused, completed, archived.
- `project_update` — Change status, priority, description. Use `items` array for batch updates.
- `link_memory` (entity_type: project) — Attach relevant knowledge to projects.
- `entity_lifecycle` (entity_type: project) — Delete, restore, or purge projects. Use `ids` array for batch delete.

### Tasks
- `task_create` — Create with title, description, priority, optional due_at (ISO 8601), optional project_id.
- `task_list` — Filter by workspace, project, or status: todo, in_progress, blocked, done, cancelled.
- `task_update` — Move through statuses. Setting `done` auto-records completion time. Use `items` for batch updates.
- `task_dependency` (action: add/remove) — Define task ordering.
- `link_memory` (entity_type: task) — Attach context to tasks.
- `entity_lifecycle` (entity_type: task) — Delete, restore, or purge tasks. Use `ids` array for batch delete.

### Workspaces
All data is scoped to workspaces. Common operations:
- `workspace_register` — Create a workspace (name required; optionally set path, gh_owner, gh_repo, type).
- `workspace_list` / `workspace_get` — Browse and retrieve workspaces.
- `entity_lifecycle` (entity_type: workspace) — Delete, restore, or purge workspaces.

### Workspace Context
- `set_workspace` — Set the active workspace UUID for the session. Once set, all subsequent tool calls that accept workspace_id will use this workspace automatically when workspace_id is omitted. Pass null or omit workspace_id to clear.
- `get_workspace` — Check the currently active workspace UUID.
- An explicit `workspace_id` in any tool call always takes precedence over the context.
- **Best practice:** Call `set_workspace` once at the start of every session.

## Saved Views
- `saved_view` (action: create/get/list/update/execute) — Create and manage reusable query views.

## Cleanup
- `cleanup_run` — Run cleanup on a workspace (applies configured policies).

## Best Practices
1. **Search before creating** — Always check if similar memories exist.
2. **Link related memories** — Use `supersedes` when newer info replaces older; `contradicts` for conflicts.
3. **Keep workspaces organized** — One workspace per project/repo. Use groups for related workspaces.
4. **Structure tasks** — Use projects -> tasks -> sub-tasks. Add dependencies for ordering.
5. **Batch operations** — Use `items` array on `memory_create`, `memory_update`, `memory_set_tags`, `project_update`, `task_update` for bulk operations. Use `ids` array on `entity_lifecycle` for batch deletes.

## Workflow Tools

These composite tools streamline common multi-step operations. Prefer them over manually chaining individual tools.

### Specification
- `project_spec` — Create a project and its initial tasks in one call. Provide workspace_id, project name/description/priority, and an array of task stubs (title, description, priority). Returns the created project and all tasks.

### Implementation
- `task_start` — Begin work on a task: sets status to `in_progress` and loads relevant context (task-linked, project-ancestor, and workspace memories). Returns context grouped by scope. Use `detail_level` to control how many memories per scope (light=2, medium=5, heavy=10).
- `task_finish` — Finish working on a task: optionally records notes as a linked `long_term` memory, then updates task status. Use status `done` for completion, `blocked` when stuck, `cancelled` to abandon.

### Review
- `context_get` — Load context for any task without changing its status. Returns memories grouped by scope (task, project ancestors, workspace).
- `project_overview` — Get a project with its tasks, subprojects, and linked memories in one call.
- `task_overview` — Get a task with dependency summaries and connected memories.

### Cleanup / Archive
- `project_archive` — Archive a completed project: sets status to `archived`, optionally records a summary as a linked `long_term` memory.
