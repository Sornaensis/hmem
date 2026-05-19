# hmem — Memory & Task Management System

You have access to the slim hmem MCP surface for persistent memory and task/project management.

## Workspace and discovery
- hmem/set_workspace / hmem/get_workspace — Set or inspect workspace context.
- hmem/workspace_list / hmem/workspace_register — Select or create a workspace.
- hmem/search — Single discovery and browsing tool for memories, projects, and tasks.

## Memories
- hmem/memory_create — Requires content, memory_type, and project_id and/or top-level task_id.
- hmem/memory_get / hmem/memory_update — Fetch or update memories; memory_update also replaces tags.
- hmem/memory_link — Memory-to-memory create/remove/list.
- hmem/link_memory — Attach/detach memory to project or top-level task.

## Projects and tasks
- hmem/project_create / hmem/project_update / hmem/project_overview / hmem/project_spec / hmem/project_archive.
- hmem/task_create / hmem/task_update / hmem/task_overview / hmem/context_get / hmem/task_dependency / hmem/task_start / hmem/task_finish.

## Best Practices
1. Set workspace context first and usually omit workspace_id afterwards.
2. Use search before creating to avoid duplicates.
3. Use long_term for durable knowledge and short_term for transient context.
4. Subtasks are one level only; never target a subtask for memory creation or attachment.
5. Prefer overview/context/workflow tools before complex status changes.
6. Do not call removed list/get/admin/meta/maintenance tools or batch aliases.
