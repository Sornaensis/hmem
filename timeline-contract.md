# Workspace timeline contract

Contract id: `workspace-timeline/v1`.

This document defines the curated workspace Timeline feature for the hmem web UI.
It is intentionally narrower than the raw audit log and more contextual than the
legacy `/api/v1/activity` feed.

## Scope

The workspace Timeline is a chronological view of task and project lifecycle
activity. It is not a general audit browser. The default view includes only
creation and terminal/completion-style events that help a user understand what
work was started or finished in the workspace.

Included by default:

- project creation
- project completion
- project archive
- top-level task creation
- subtask creation
- top-level task completion
- subtask completion
- top-level task cancellation
- subtask cancellation

Excluded by default:

- memory creates/updates/deletes
- task/project field-only edits such as title, description, priority, metadata,
  project move, or parent move when status is not terminal
- task dependency, tag, category, memory-link, workspace-group, auth, cleanup,
  and membership changes
- soft-delete/purge audit rows unless a later design explicitly adds deletion
  events to the Timeline

Cancellation is treated as a terminal lifecycle event because it closes work, but
it must be rendered with a distinct `*_cancelled` event type and label rather
than described as completion.

## Data source decision

Implement Timeline as a new backward-compatible REST shape derived on the server
from `audit_log`, joined with current task/project rows where available. Do not
change the existing `/api/v1/activity` or `/api/v1/audit` response contracts.

Recommended endpoint:

```http
GET /api/v1/workspaces/:workspace_id/timeline?limit=50&offset=0
```

Optional filters can be added without breaking the default contract:

- `entity_type=project|task|subtask`
- `event_type=<timeline event type>`

Rationale:

- `/api/v1/activity` is workspace-readable and compact, but it is based on row
  `updated_at` state, has only `created|updated|deleted`, lacks actors, parent
  context, and status transitions, and cannot distinguish field-only edits from
  completion/archive events.
- `/api/v1/audit` contains the needed raw material, but it is an admin-oriented
  audit surface and exposes raw old/new row JSON that the Timeline should not
  leak or render directly.
- A new endpoint can require normal workspace `read` permission, expose only
  curated fields, and remain additive/backward-compatible.

## Response shape

The endpoint returns the existing paginated wrapper:

```json
{
  "items": [WorkspaceTimelineEvent],
  "has_more": false
}
```

`WorkspaceTimelineEvent` fields:

| Field | Type | Required | Notes |
| --- | --- | --- | --- |
| `id` | string | yes | Stable opaque event id. For audit-backed events use `audit:<audit_log.id>`. |
| `workspace_id` | UUID | yes | Workspace containing the event. |
| `event_type` | enum | yes | One of the event types below. |
| `entity_type` | enum | yes | `project`, `task`, or `subtask`. |
| `entity_id` | UUID | yes | Project/task id to navigate to. |
| `title` | string | yes | Project name or task title at event time when available; fallback to current row name/title, then `Untitled <entity>`. |
| `occurred_at` | timestamp | yes | Event timestamp; for audit-backed events this is `audit_log.changed_at`. |
| `actor` | object/null | no | Actor summary from audit metadata, omitted when unknown. |
| `project` | object/null | no | Project context for task/subtask events when known. |
| `parent_task` | object/null | no | Parent task context for subtask events when known. |
| `status_transition` | object/null | no | Present for status-derived terminal events. |
| `navigation` | object | yes | Target for UI focus/navigation. |
| `source_audit_id` | UUID/null | no | Raw audit id for diagnostics or future deep links; not required for UI rendering. |

Nested shapes:

```json
{
  "actor": {
    "type": "user|bot",
    "id": "optional stable actor id",
    "label": "display name when available"
  },
  "project": {
    "id": "project uuid",
    "name": "project name"
  },
  "parent_task": {
    "id": "task uuid",
    "title": "parent task title"
  },
  "status_transition": {
    "from": "todo|in_progress|blocked|done|cancelled|active|paused|completed|archived",
    "to": "done|cancelled|completed|archived"
  },
  "navigation": {
    "entity_type": "project|task",
    "entity_id": "project or task uuid"
  }
}
```

`navigation.entity_type` should be `task` for both task and subtask events so the
existing focus/navigation code can open the task card. The display
`entity_type` remains `subtask` when the task has a parent.

## Event types and derivation

Timeline event types are stable snake_case strings:

| Event type | Entity type | Source audit row | Required predicate |
| --- | --- | --- | --- |
| `project_created` | `project` | `entity_type = 'project'`, `action = 'create'` | create row has a project id |
| `project_completed` | `project` | `entity_type = 'project'`, `action = 'update'` | `old_values.status != 'completed'` and `new_values.status = 'completed'` |
| `project_archived` | `project` | `entity_type = 'project'`, `action = 'update'` | `old_values.status != 'archived'` and `new_values.status = 'archived'` |
| `task_created` | `task` | `entity_type = 'task'`, `action = 'create'` | `new_values.parent_id` is missing/null/empty |
| `subtask_created` | `subtask` | `entity_type = 'task'`, `action = 'create'` | `new_values.parent_id` is present |
| `task_completed` | `task` | `entity_type = 'task'`, `action = 'update'` | top-level task and status changes to `done` |
| `subtask_completed` | `subtask` | `entity_type = 'task'`, `action = 'update'` | subtask and status changes to `done` |
| `task_cancelled` | `task` | `entity_type = 'task'`, `action = 'update'` | top-level task and status changes to `cancelled` |
| `subtask_cancelled` | `subtask` | `entity_type = 'task'`, `action = 'update'` | subtask and status changes to `cancelled` |

Status-derived events require both an old and new status value. Malformed or
partial legacy audit rows that cannot prove a terminal status transition are
excluded from the curated Timeline rather than guessed.

Creation events use `new_values` first because it reflects the row at event time.
Current task/project rows can fill display context if a field was absent in the
audit JSON. If the current row has been soft-deleted, the event should still be
emitted from audit data when the audit row has enough title/id/status context;
navigation may still point at the entity id and let normal route handling decide
whether it can be shown.

Subtask detection uses `parent_id` from the event-time audit JSON first. If the
audit JSON does not include `parent_id`, use the current task row as a fallback.
Do not classify a task as a subtask solely from UI tree position.

## Sorting and pagination

Default order is newest first:

1. `occurred_at DESC`
2. audit-backed events before non-audit fallback events for the same timestamp
3. `source_audit_id DESC` when present, otherwise opaque `id DESC`

The tie-breaker is for deterministic pagination only and has no semantic meaning.
The first implementation should use `limit` and `offset` with the same cap rules
as other hmem list endpoints. The endpoint should over-fetch one row to set
`has_more` consistently.

## Security and privacy

- Require workspace `read` permission for the requested workspace.
- Do not expose raw `old_values`, `new_values`, metadata, request ids, access
  token fields, hashes, secrets, or session data in Timeline events.
- Actor data is limited to `actor_type`, `actor_id`, and `actor_label` from the
  audit row. If all actor fields are absent, omit `actor`.
- Timeline output is safe for normal workspace readers; raw audit access remains
  admin-only.

## Test fixtures and coverage expectations

Representative backend/API tests should cover:

- project creation
- project status transition to `completed`
- project status transition to `archived`
- top-level task creation
- subtask creation using `parent_id`
- top-level task status transition to `done`
- subtask status transition to `done`
- task/subtask status transition to `cancelled`
- field-only task/project updates excluded from default results
- newest-first ordering with a stable same-timestamp tie-breaker
- empty workspace response

Representative UI tests or fixtures should cover:

- Timeline tab loading, empty, error, and populated states
- labels/icons for creation, completion, archive, and cancellation
- top-level task vs subtask display
- actor and project/parent context rendering when present
- navigation target for project and task/subtask events
