# Database schema

hmem stores repository evidence as provenance-bound **Observations**, and stores
planning work separately as workspaces, projects, and tasks.

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

The `observations` table stores an Observation's workspace, Git provenance, and
content. Its immutable ordered repository subjects are stored in
`observation_subjects`.

| Column | Type | Notes |
| --- | --- | --- |
| `id` | `UUID` | Primary key; defaults to `gen_random_uuid()` |
| `workspace_id` | `UUID` | Required foreign key to `workspaces(id)`; deleting the workspace cascades |
| `git_sha` | `TEXT` | Required lowercase 40-character Git SHA |
| `content` | `TEXT` | Required Observation content |
| `search_vector` | `TSVECTOR` | Required internal full-text index value; defaults to an empty vector |
| `created_at` | `TIMESTAMPTZ` | Required; defaults to `now()` |
| `updated_at` | `TIMESTAMPTZ` | Required; defaults to `now()` and is maintained by a trigger |
| `embedding` | `vector(1536)` | Optional, nullable, no default; present only after pgvector enablement |

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
`git_sha`—is immutable after creation. The normal Observation update may replace
only `content`. A content change clears any stored embedding in the same
mutation, so a vector made from old content is never reused silently. The
optional embedding is written through a separate pgvector operation. Changing
the repository, subject, kind, or revision requires a new Observation. Deleting
an Observation is a hard delete, not a soft-delete lifecycle state.

`search_vector` is maintained from every subject and content. pgvector is optional:
without the extension, the `embedding` column and vector index are absent and
similarity operations report that capability as unavailable; all non-vector
Observation operations remain available.

## pgvector and embedding operations

### Enable the database capability

The pgvector PostgreSQL package and the database objects are separate layers:

1. A PostgreSQL administrator installs the pgvector package/control file that
   matches the configured server.
2. `hmem-ctl pgvector enable` installs the `vector` extension in the configured
   database when needed, then adds and verifies the nullable `vector(1536)`
   column and exact valid HNSW `vector_cosine_ops` index.

Inspect both layers before and after enablement:

```text
hmem-ctl pgvector status
hmem-ctl pgvector enable
hmem-ctl pgvector status
```

`status` is read-only. It reports package availability, installed/default
extension versions, the Observation table, the exact column/index contract,
total/embedded/missing counts when those counts are meaningful, readiness, and
a next action. A diagnosed not-ready status exits 2; connection or inspection
failure exits 1; ready exits 0. `--json` returns the same fields as stable JSON.

`enable` uses only hmem's configured database connection. It cannot install an
operating-system/PostgreSQL package, start or stop PostgreSQL, or replay normal
hmem migrations. It is atomic and safe to repeat: success is either
`provisioned` with the reported changes or `already_ready` with no changes. It
does not drop or rewrite incompatible existing objects. When status reports
package unavailability, install pgvector on that PostgreSQL server first. When
it reports an incompatible extension, column, table, or index, inspect and
repair that object under normal database change control, then rerun status.

Creating the column and regular HNSW index can block writes to
`public.observations` until the transaction commits. Back up a production
database and schedule a change window appropriate to its size and write load.
The configured role must be allowed to create the extension and alter the
Observation table. A failed or refused enable reports an actionable error and
commits no partial schema changes.

### Who creates embeddings

pgvector stores, indexes, and compares vectors. It does not create embeddings,
and hmem does not invoke an embedding model, provider SDK, network service, or
subprocess. A user-controlled external producer—such as a local program,
service, agent, or batch job—chooses the model and exact input preprocessing,
owns any credentials, and returns exactly 1536 finite numeric values. hmem
validates and persists the supplied values.

Use one consistent model revision, preprocessing/canonicalization, and vector
space for both stored Observation vectors and similarity-query vectors. Vectors
from different spaces are not comparable. Changing any part of that identity
requires a controlled, complete re-export and re-embedding of the corpus; do
not mix old and new vectors during the transition.

New Observations start with a null embedding. Editing `content` clears the
embedding and returns the Observation to the missing set.

### Backfill and refresh with NDJSON

The portable file workflow is:

```text
hmem-ctl embeddings export --workspace <workspace-uuid> --output embedding-input.ndjson
# Run your external producer and write embedding-results.ndjson.
hmem-ctl embeddings import --input embedding-results.ndjson --json
```

Export is missing-only by default, providing a bounded, restart-safe backfill
and retry queue. Add `--all` to export every matching Observation, including
rows that already have vectors. `--page-size` controls bounded database paging.
Omit `--output` to stream versioned NDJSON to stdout; its human summary then
goes to stderr. Each export record contains `format_version`, stable
`observation_id` and `workspace_id`, immutable `git_sha` and ordered `subjects`,
exact `content`, and a `content_fingerprint`. It never contains a vector.

The external producer emits one import record with the same `format_version`,
`observation_id`, `workspace_id`, and `content_fingerprint`, plus an `embedding`
array of exactly 1536 finite numbers. Omit `--input` to read NDJSON from stdin.
The exchange format intentionally has no provider name, credentials, or model
selection; those remain the operator's responsibility.

Import validates each physical line and commits each valid record independently
with compare-and-set semantics. Outcomes are:

| Outcome | Meaning | Safe retry |
| --- | --- | --- |
| `applied` | The vector was stored for the exact exported content. | Yes |
| `already_satisfied` | The same vector is already stored. | Yes; this is the idempotent result. |
| `stale` | Content/provenance no longer matches the fingerprint. | Re-export, regenerate, and retry. |
| `not_found` | The Observation/workspace pair does not exist. | Reconcile identity before retrying. |
| `rejected` | The version, shape, fingerprint, dimensions, values, duplicate, or line limit is invalid. | Correct the record, then retry it. |
| `database_error` | That record could not be applied because of a database error. | Diagnose the database, then retry it. |

Human mode prints one redacted result per record and a final count summary;
`--json` prints NDJSON results and a summary without echoing vectors or content.
Exit 0 means every record was applied or already satisfied. Exit 2 means at
least one record was stale, not found, rejected, or had a database error; the
successful records remain committed and only failed records need to be retried.
Exit 1 is a setup, file I/O, connection, or unavailable-capability failure.

For routine refresh, export missing rows, generate their vectors in the same
space, and import them. If an import is stale because content changed during
processing, export missing rows again rather than forcing the old result. For a
model/preprocessing change, use `export --all`, generate every vector in the
new space, and import the complete corpus during an operator-controlled
consistency window.

```text
hmem-ctl embeddings export --all --workspace <workspace-uuid> --output replacement-input.ndjson
# Generate every result in the new space, then import that complete batch.
hmem-ctl embeddings import --input replacement-results.ndjson --json
```

### Online REST and MCP flow

For an individual Observation, create or update it normally, then give its
canonical content to the external producer. Set the returned vector with:

- REST `PUT /api/v1/observations/{observationId}/embedding`, whose request body
  is a JSON array of exactly 1536 finite numbers.
- MCP `observation_set_embedding` with `observation_id` and `embedding`. The
  server resolves the Observation's actual workspace from `observation_id` and
  requires edit authorization for that workspace.

Generate a query vector with the same producer and vector-space identity, then
search with REST `POST /api/v1/observations/similar` or MCP
`observation_similar`. The REST request supplies `workspace_id` explicitly; for
MCP, the active workspace supplies/injects `workspace_id` into the
workspace-scoped call. Both accept the 1536-number query vector plus their
documented filter/pagination fields. An Observation whose content was edited
remains absent from similarity results until a new embedding is set.

## Observation API and MCP

REST and MCP emit canonical `subjects`, for example:

```json
{"subjects":[{"subject_kind":"file","subject":"src/Main.elm"},{"subject_kind":"glob","subject":"src/**/*.elm"}]}
```

Clients must read the canonical nonempty `subjects` list. Exact
`subject_kind` and `subject` filters match any subject in the set.

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

## Other tables

The operational schema also contains access control, workspace groups,
workspaces, projects, tasks, task dependencies, saved views, audit records, and
schema-migration bookkeeping. Workspace, project, and task lifecycle fields do
not establish a relationship to Observations.

The `audit_log` records entity changes and their audit attribution.
