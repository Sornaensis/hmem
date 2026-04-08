# hmem — Memory & Task Management System

You have access to the hmem MCP server which provides persistent memory storage and task/project management. Use these tools proactively to maintain context across conversations.

## Memory Management

### Storing Memories
- Use `memory_create` to store important information, decisions, patterns, and user preferences.
- Use `memory_create_batch` when storing multiple related items.
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
- `memory_link` (action: create/remove) — Create or remove typed relationships: related, supersedes, contradicts, elaborates, inspires, depends_on, derived_from, alternative_to.
- `memory_set_tags` — Manage tags (use `memory_get` to see current tags).
- Use `memory_update` with pinned=true/false to pin/unpin. Use `memory_update` with importance=N to adjust importance.
- `memory_graph` — Explore relationship networks from a starting memory.
- Categories: `category_create`, `category_list`, `link_memory` (entity_type: category), `list_entity_memories` (entity_type: category).

## Task & Project Management

### Projects
- `project_create` — Create with name, description, priority. Supports sub-projects via parent_id.
- `project_list` — Filter by status: active, paused, completed, archived.
- `project_update` — Change status, priority, description.
- `link_memory` (entity_type: project) — Attach relevant knowledge to projects.
- `entity_lifecycle` (entity_type: project) — Delete, restore, or purge projects.

### Tasks
- `task_create` — Create with title, description, priority, optional due_at (ISO 8601), optional project_id.
- `task_list` — Filter by workspace, project, or status: todo, in_progress, blocked, done, cancelled.
- `task_update` — Move through statuses. Setting `done` auto-records completion time.
- `task_dependency` (action: add/remove) — Define task ordering.
- `link_memory` (entity_type: task) — Attach context to tasks.
- `entity_lifecycle` (entity_type: task) — Delete, restore, or purge tasks.

### Workspaces
All data is scoped to workspaces. Common operations:
- `workspace_register` — Create a workspace (name required; optionally set path, gh_owner, gh_repo, type).
- `workspace_list` / `workspace_get` — Browse and retrieve workspaces.
- `workspace_group` (action: create/add_member/...) — Organize workspaces into groups.
- `entity_lifecycle` (entity_type: workspace) — Delete, restore, or purge workspaces.

## Cleanup
- `cleanup_run` — Run cleanup on a workspace (applies configured policies).
- `cleanup_policy` (action: list/upsert) — Configure auto-cleanup rules (max age, max count, min importance thresholds).

## Activity
- `activity_timeline` — View recent activity across the system or within a workspace.

## Best Practices
1. **Search before creating** — Always check if similar memories exist.
2. **Link related memories** — Use `supersedes` when newer info replaces older; `contradicts` for conflicts.
3. **Keep workspaces organized** — One workspace per project/repo. Use groups for related workspaces.
4. **Structure tasks** — Use projects -> tasks -> sub-tasks. Add dependencies for ordering.
5. **Batch operations** — Use `memory_create_batch` for bulk storage.

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
