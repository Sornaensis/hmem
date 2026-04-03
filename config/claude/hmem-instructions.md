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
- Categories: `category_create`, `category_list`, `category_link_memory`, `category_unlink_memory`.

## Task & Project Management

### Projects
- `project_create` — Create with name, description, priority. Supports sub-projects via parent_id.
- `project_list` — Filter by status: active, paused, completed, archived.
- `project_update` — Change status, priority, description.
- `project_link_memory` — Attach relevant knowledge to projects.

### Tasks
- `task_create` — Create with title, description, priority, optional due_at (ISO 8601), optional project_id.
- `task_list` — Filter by workspace, project, or status: todo, in_progress, blocked, done, cancelled.
- `task_update` — Move through statuses. Setting `done` auto-records completion time.
- `task_dependency_add` / `task_dependency_remove` — Define task ordering.
- `task_link_memory` — Attach context to tasks.

### Workspaces
All data is scoped to workspaces. Common operations:
- `workspace_register` — Create a workspace (name required; optionally set path, gh_owner, gh_repo, type).
- `workspace_list` / `workspace_get` — Browse and retrieve workspaces.
- `workspace_group_create` / `workspace_group_add_member` — Organize workspaces into groups.

## Cleanup
- `cleanup_run` — Run cleanup on a workspace (applies configured policies).
- `cleanup_policy_upsert` — Configure auto-cleanup rules (max age, max count, min importance thresholds).

## Activity
- `activity_timeline` — View recent activity across the system or within a workspace.

## Best Practices
1. **Search before creating** — Always check if similar memories exist.
2. **Link related memories** — Use `supersedes` when newer info replaces older; `contradicts` for conflicts.
3. **Keep workspaces organized** — One workspace per project/repo. Use groups for related workspaces.
4. **Structure tasks** — Use projects -> tasks -> sub-tasks. Add dependencies for ordering.
5. **Batch operations** — Use `memory_create_batch` for bulk storage.
