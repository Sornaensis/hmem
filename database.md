# Database schema

hmem stores repository evidence as provenance-bound **Observations**, and stores
planning work separately as workspaces, projects, and tasks. This document
reflects the V020 schema.

> **V020 is destructive.** Applying V020 permanently deletes all legacy Memory
> rows and their content-bearing audit history. It does not convert or export
> them. Back up any data that must be retained before migrating, and upgrade
> REST, MCP, and web UI clients: the old Memory contract is incompatible with
> the Observation-only boundary.

## Relationships

```text
workspaces ──< observations
     │
     ├──< projects ──< tasks
     └───────────────< tasks

tasks ──< task_dependencies >── tasks
```

A repository workspace supplies the scope for an Observation. Projects and
tasks remain planning entities, but **Observations have no project or task link
columns or join tables**. An Observation is evidence about a repository subject,
not a task attachment.

## `observations`

The V020 Observation table has exactly these persisted columns:

| Column | Type | Notes |
| --- | --- | --- |
| `id` | `UUID` | Primary key; defaults to `gen_random_uuid()` |
| `workspace_id` | `UUID` | Required foreign key to `workspaces(id)`; deleting the workspace cascades |
| `subject_kind` | `observation_subject_kind` | Required enum: `file` or `glob` |
| `subject` | `TEXT` | Required repository-relative subject |
| `git_sha` | `TEXT` | Required lowercase 40-character Git SHA |
| `content` | `TEXT` | Required Observation content |
| `search_vector` | `TSVECTOR` | Required internal full-text index value; defaults to an empty vector |
| `created_at` | `TIMESTAMPTZ` | Required; defaults to `now()` |
| `updated_at` | `TIMESTAMPTZ` | Required; defaults to `now()` and is maintained by a trigger |
| `embedding` | `vector(1536)` | Optional: created only when pgvector is installed |

`subject` must be 1–4096 bytes and be a canonical repository-relative,
forward-slash path or glob: it cannot be absolute, begin with `./`, contain
backslashes, or include empty, `.` or `..` path segments. `content` is 1–524288
bytes. `git_sha` must match `^[0-9a-f]{40}$`.

The provenance tuple—`workspace_id`, `subject_kind`, `subject`, and `git_sha`—is
immutable after creation. The normal Observation update may replace only
`content`; the optional embedding is managed through a separate pgvector
operation. Changing the repository, subject, kind, or revision requires a new
Observation. Deleting an Observation is a hard delete, not a soft-delete
lifecycle state.

`search_vector` is maintained from subject and content. pgvector is optional:
without the extension, the `embedding` column and vector index are absent and
similarity operations report that capability as unavailable; all non-vector
Observation operations remain available.

## Other current tables

V020 retains the operational schema for access control, workspace groups,
workspaces, projects, tasks, task dependencies, saved views, audit records, and
schema migrations. Workspace, project, and task lifecycle fields use their
respective current enums; they do not establish a relationship to Observations.

The `audit_log` records current entity changes. V020 deliberately purges the
legacy Memory-family audit records together with the legacy data, so it cannot
be used to recover that deleted content.
