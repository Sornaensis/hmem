# Audit destinations

| Audit entity type | Destination |
| --- | --- |
| `project`, `task` | Bounded Projects focus with a current target read. |
| `observation` | Observations selected detail through `observation=<id>`. |
| `agent_session` | Sessions selected viewer through `session=<id>`, including sealed evidence. |
| `task_session` | Snapshot `session_id`; a separate link uses `task_id`. |
| `task_dependency` | Snapshot `task_id`; a separate link uses `depends_on_id`. |
| `workspace` | Workspace context without hierarchy focus. |
| Other types | Recorded values with an unavailable destination. |

Targets and workspaces require valid UUIDs. Relationship IDs come from new
snapshots, with old-snapshot fallback; composite relationship keys are never
focus IDs. Server authorization remains authoritative. Missing and forbidden
target reads use a nondisclosing unavailable outcome.

Each activation captures the current viewport under its own identity, audit query,
workspace, session epoch and principal. Only its current completion can navigate.
Return metadata follows the exact destination URL and the admitted principal.
Restored queries retain their source page, selection and viewport; newer filters
retire the old return request and scroll operation.
