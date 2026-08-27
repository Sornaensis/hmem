# Database schema

hmem stores repository evidence as provenance-bound **Observations**, and stores
planning work separately as workspaces, projects, and tasks. This document
reflects the V021 schema.

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

The V021 parent table stores an Observation's workspace, Git provenance, and
content. Its immutable ordered repository subjects are stored in the related
`observation_subjects` table.

| Column | Type | Notes |
| --- | --- | --- |
| `id` | `UUID` | Primary key; defaults to `gen_random_uuid()` |
| `workspace_id` | `UUID` | Required foreign key to `workspaces(id)`; deleting the workspace cascades |
| `git_sha` | `TEXT` | Required lowercase 40-character Git SHA |
| `content` | `TEXT` | Required Observation content |
| `search_vector` | `TSVECTOR` | Required internal full-text index value; defaults to an empty vector |
| `created_at` | `TIMESTAMPTZ` | Required; defaults to `now()` |
| `updated_at` | `TIMESTAMPTZ` | Required; defaults to `now()` and is maintained by a trigger |
| `embedding` | `vector(1536)` | Optional: created only when pgvector is installed |

`observation_subjects` has `observation_id`, a zero-based `ordinal`,
`subject_kind` (`file` or `glob`), and `subject`. Every Observation has 1–256
subjects, in stored order, totalling at most 256 KiB. Each subject is 1–4096
bytes and is a canonical repository-relative forward-slash path: it cannot be
absolute, begin with `./`, contain backslashes, or include empty, `.` or `..`
path segments. File subjects are concrete. Glob subjects use only `*`, `?`,
and `**` as a complete path component; `*` and `?` never cross `/`, while
`**/` has the conventional zero-directory case. Dotfiles are ordinary path
components. Character classes, braces, escapes, embedded `**`, drive-letter
paths, control characters, and non-canonical forms are rejected. `content` is
1–524288 bytes. `git_sha` must match
`^[0-9a-f]{40}$`.

The provenance tuple—`workspace_id`, the complete ordered subject set, and
`git_sha`—is immutable after creation. The normal Observation update may replace only
`content`; the optional embedding is managed through a separate pgvector
operation. Changing the repository, subject, kind, or revision requires a new
Observation. Deleting an Observation is a hard delete, not a soft-delete
lifecycle state.

`search_vector` is maintained from every subject and content. pgvector is optional:
without the extension, the `embedding` column and vector index are absent and
similarity operations report that capability as unavailable; all non-vector
Observation operations remain available.

## Observation API and MCP compatibility

REST and MCP emit canonical `subjects`, for example:

```json
{"subjects":[{"subject_kind":"file","subject":"src/Main.elm"},{"subject_kind":"glob","subject":"src/**/*.elm"}]}
```

During the compatibility window, responses also project the first entry as the
deprecated top-level `subject_kind` and `subject` fields; clients should read
the canonical nonempty `subjects` list. Exact `subject_kind` and `subject`
filters match any subject in the set.

`POST /api/v1/observations/match` (and MCP `observation_match`) accepts 1–256
concrete, canonical repository-relative `paths`, at most 4096 UTF-8 bytes each
and 262144 UTF-8 bytes total. It returns each matching
Observation once, with `matched_paths` in caller path order and
`matched_subjects` in stored-subject order. This is an OR match over the given
paths, optionally narrowed by the normal exact kind, SHA, and text filters.
It does not read a repository, inspect the filesystem, expand a Git tree, or
expand caller globs. For example:

```json
{"workspace_id":"…","paths":["src/Main.elm",".github/workflows/ci.yml"],"subject_kind":"glob","limit":50,"offset":0}
```

The equivalent MCP call is `observation_match` with the same `paths` array and
optional `subject_kind`, `git_sha`, `query`, `limit`, and `offset` arguments.

## Other current tables

V021 retains the operational schema for access control, workspace groups,
workspaces, projects, tasks, task dependencies, saved views, audit records, and
schema migrations. Workspace, project, and task lifecycle fields use their
respective current enums; they do not establish a relationship to Observations.

The `audit_log` records current entity changes. V020 deliberately purges the
legacy Memory-family audit records together with the legacy data, so it cannot
be used to recover that deleted content.
