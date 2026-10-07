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
observations ──< observation_revision_events
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
| `git_sha` | `TEXT` | Immutable original creation SHA, lowercase 40-character Git SHA |
| `content` | `TEXT` | Required Observation content |
| `content_version` | `UUID` | Opaque content precondition; defaults to a fresh UUID and advances on every accepted content write |
| `search_vector` | `TSVECTOR` | Required internal full-text index value; defaults to an empty vector |
| `created_at` | `TIMESTAMPTZ` | Required; defaults to `now()` |
| `updated_at` | `TIMESTAMPTZ` | Required; defaults to `now()` and is maintained by a trigger |
| `embedding` | `vector(1536)` | Optional, nullable, no default; present only after pgvector enablement |
| `embedding_space_fingerprint` | `TEXT` | Identifies the stored vector's space; nullable when no vector is stored |

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

The creation provenance tuple—`workspace_id`, the complete ordered subject set,
and `git_sha`—is immutable. Correcting repository identity or subjects requires a
new Observation. Corrected content can be reviewed at another revision by
appending a compact assertion; this never replaces the original `git_sha`.
Every accepted content update or re-audit clears the stored embedding and its
space fingerprint and fences leased work atomically, including identical-text
writes. Optional embedding writes are separate operations. Observation deletion
is a hard delete, not a soft-delete lifecycle state.

Core reads return `content_version`. Conditional updates compare the expected
UUID with the workspace and Observation ID atomically. They return the applied
canonical Observation, a version mismatch with the latest canonical Observation,
or an absent record. A mismatch produces no content, embedding, job, audit, or
outbox mutation. A caller can consciously rebase against the returned version.
The canonical core update requires a reviewed SHA and an expected version; the
unconditional content-only updater is removed at the coordinated cutover.
Every accepted content write or re-audit, including identical bytes and SHA,
advances the token and appends one event. Embedding-only writes leave it intact;
direct token replacement is rejected. Versions carry no ordering, clock, Git
revision, or request-ID meaning. V030 backfills existing records and registers
the schema migration within its transaction. The revision-history migration is
a separate forward migration with its own atomic schema and ledger transaction.

## `observation_revision_events`

This approved contract is implemented through a coordinated core/API/MCP/Elm
release. See [the API contract](api.md) for request and compatibility details.
The table contains compact assertions, never past content or diffs:

| Column | Type | Notes |
| --- | --- | --- |
| `observation_id` | `UUID` | FK to Observation, cascades on deletion |
| `sequence` | `BIGINT` | Positive per-Observation server sequence; composite primary key with ID |
| `event_kind` | `TEXT` | `creation`, `update`, or `legacy_creation` |
| `reviewed_git_sha` | `TEXT` | Required lowercase full SHA; a legacy event labels only the original creation claim |
| `content_version` | `UUID` | Resulting version; null only for `legacy_creation` |
| `content_digest` | `TEXT` | SHA-256 of exact accepted UTF-8 bytes, 64 lowercase hex characters; null only for `legacy_creation` |
| `recorded_at` | `TIMESTAMPTZ` | Server time for new assertions; preserved historical creation time for legacy claim |
| `actor_type` | `TEXT`, nullable | Trusted Principal type (`user` or `bot`); null only for unknown legacy/absent context |
| `actor_id` | `TEXT`, nullable | Trusted Principal ID, including synthetic local IDs and deployed bot token IDs |
| `actor_label` | `TEXT`, nullable | Optional trusted Principal display label |

New creation appends sequence 1 with its accepted content bound to `git_sha`.
Each accepted update/re-audit appends the next sequence under the Observation
lock. Digesting never trims, normalizes Unicode, converts newlines, or inserts a
BOM. Version, digest, event, content, vector/job invalidation, audit, and outbox
effects commit together. A failed precondition or validation appends nothing.
Direct event replacement or deletion is rejected except FK deletion cascades;
the server owns sequence, time, actor, and digest. Sequences order this history;
opaque UUID versions remain only conditional-write tokens. Public sequence values
must stay in the exact JSON integer range (1–9007199254740991); exhausted sequence
allocation fails without mutation.

Actor fields mirror the existing server Principal/audit context, including known
local user/bot identities. A deployed bot's `actor_id` is its token ID, not the
separate grant-user ID that authorizes it. Do not replace known actors with null
or infer a user UUID from arbitrary text. Null type/ID is reserved for genuinely
absent context or unknown legacy attribution; clients cannot supply these fields.

Canonical reads include `latest_sequence` and nullable `current_provenance`.
The latter is the latest bound event with these same fields except
`observation_id`. It is an assertion made by the authorized caller, not a server
Git verification. The server does not inspect a checkout. History is separately
paged in descending sequence and never embedded unboundedly in normal reads.

Migration preserves every existing creation SHA claim as sequence 1
`legacy_creation`, with null version/digest/actor and the original `created_at`.
All migrated Observations expose null `current_provenance`: current legacy
content may have been corrected without a reviewed SHA. Timestamps, existing
versions, audit text, and embeddings do not establish historical bindings.
Migration invents no update events or SHAs and does not change content, version,
identity, subjects, or vectors. The first accepted post-cutover re-audit binds
current content and establishes current provenance. Permanent Observation or
workspace deletion cascades event rows with subjects and jobs. Existing audit
and outbox retention/authorization remains unchanged; no Observation restore is
introduced.

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

pgvector stores, indexes, and compares vectors; it does not create them. By
default hmem leaves automatic vectorization disabled. Operators can still
supply exactly 1536 finite coordinates through REST, MCP, or the manual NDJSON
workflow. The external producer owns its model, input formatting, credentials,
and matching query vectors. The `hmem-ctl embeddings` exchange itself never
invokes a provider.

When explicitly enabled with a validated GPU provider and pgvector ready,
production reconciliation creates durable `embedding_jobs` for missing or
wrong-space Observation vectors. The worker claims a job, generates a vector,
and uses content, space, lease, and ownership checks to write it. The tested
managed path uses pinned native TEI on a Linux/WSL Docker NVIDIA deployment;
an HTTP mode can use an independently validated GPU endpoint. Neither mode
falls back to CPU inference. Provider loss leaves ordinary API operations
available while automatic vectorization is unavailable.

Each Observation has **one** stored vector and one space fingerprint, not one
vector per space. A same-space manual vector can settle current work under the
compare-and-set rules. If a different provider space is enabled, reconciliation
may replace a different-space manual vector; do not treat manual vectors as
immutable or relabel old/unknown vectors. New Observations start with a null
embedding; editing `content` clears the vector and fences stale work.

Revision assertions retain the existing embedding input fingerprint encoding:
immutable creation SHA, ordered subjects, and exact content. Reviewed SHA,
history sequence, digest, and content version are excluded. The core and manual
NDJSON fingerprint implementations/export projections must remain equivalent.
Every accepted assertion resets a leased job's state and owner even if that
fingerprint is unchanged; its old attempt cannot complete or renew. Unchanged
manual export input remains reusable after a SHA-only or same-text re-audit.
Changed-content imports are stale. This distinguishes input compatibility from
automatic job ownership and preserves NDJSON versions 1/2 and optional pgvector.

Use the same model revision, input formatting, and space for stored and query
vectors. Different spaces are not comparable. The qualified GPU space is
`Alibaba-NLP/gte-Qwen2-1.5B-instruct@1cad2ab3ff41c2671f34e135d29831368ee26b68:1536:attention=noncausal:v1`.
It uses noncausal attention, EOS/PAD 151643, last-valid-token pooling, finite
1536-coordinate L2 normalization, unchanged document bytes, and the fixed
query prefix exactly once when a caller actually embeds textual queries.
Runtime precision is provenance, not a different space identity.

### Backfill and refresh with NDJSON

The portable file workflow is:

```text
hmem-ctl embeddings export --workspace <workspace-uuid> --output embedding-input.ndjson
# Run your external producer and write embedding-results.ndjson.
hmem-ctl embeddings import --input embedding-results.ndjson --json
```

Export selects Observations with a null vector **or** a vector in a different
space from the selected target. The default target is the legacy manual space
`hmem:legacy-manual:v1`; use `--space-fingerprint <target-space>` for a qualified
target. Add `--all` to export every matching Observation, including rows that
already have a vector in the selected space. `--page-size` controls bounded
database paging. Omit `--output` to stream NDJSON version 2 to stdout; its
human summary then goes to stderr. Each export record contains `format_version`,
the selected `space_fingerprint`, stable
`observation_id` and `workspace_id`, immutable `git_sha` and ordered `subjects`,
exact `content`, and a `content_fingerprint`. It never contains a vector.

The external producer preserves the exported `format_version` and
`space_fingerprint` in each result, together with `observation_id`,
`workspace_id`, and `content_fingerprint`, and adds an `embedding` array of
exactly 1536 finite numbers. Export always writes version 2, whose import
records require an explicit `space_fingerprint`. Older version-1 imports omit
it and remain in the legacy manual space. Omit `--input` to read NDJSON from
stdin.
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

For routine refresh, export null- or different-space rows for the selected
target, generate their vectors in that same space, and preserve the exported
space in each import. If an import is stale because content changed during
processing, export that selected space again rather than forcing the old
result. For a model/preprocessing change, use `export --all`, generate every
vector in the new space, and import the complete corpus during an
operator-controlled consistency window. An enabled automatic target may also
reconcile old-space rows; coordinate manual replacement with that target rather
than assuming a manual vector stays unchanged.

```text
hmem-ctl embeddings export --all --workspace <workspace-uuid> \
  --space-fingerprint <new-space-fingerprint> --output replacement-input.ndjson
# Generate every result in the selected space; preserve its exported space_fingerprint.
hmem-ctl embeddings import --input replacement-results.ndjson --json
```

### Online REST and MCP flow

For an individual Observation, create or update it normally, then give its
canonical content to the external producer. Set the returned vector with:

- REST `PUT /api/v1/observations/{observationId}/embedding` with either a
  legacy JSON array of exactly 1536 finite numbers or a qualified JSON object
  containing that `embedding` array and a `space_fingerprint` string.
  The envelope requires both fields; `space_fingerprint: null` is invalid.
- MCP `observation_set_embedding` with `observation_id`, `embedding`, and
  optional `space_fingerprint`. Omitting the space selects the legacy manual
  space; explicit null is invalid. The server resolves the Observation's
  actual workspace from `observation_id` and requires edit authorization.

Generate a query vector externally with the same model, formatting, and space,
then search with REST `POST /api/v1/observations/similar` or MCP
`observation_similar`. The REST request supplies `workspace_id`, `embedding`,
and, for a qualified space, `space_fingerprint` explicitly; for MCP, the active
workspace supplies the workspace scope. Omitting `space_fingerprint` selects
the legacy space, **not** all spaces; explicit null is invalid. A stored-vector
self-query tests retrieval and space isolation; it is not a numerical or
semantic-quality oracle. There is no raw-text query embedding endpoint. An
Observation whose content was edited remains absent from similarity results
until a new vector is stored.

## Observation API and MCP

REST and MCP emit canonical `subjects`, for example:

```json
{"subjects":[{"subject_kind":"file","subject":"src/Main.elm"},{"subject_kind":"glob","subject":"src/**/*.elm"}]}
```

Clients must read the canonical nonempty `subjects` list. Exact
`subject_kind` and `subject` filters match any subject in the set.

Canonical Observation reads and successful writes also return the opaque
`content_version` UUID. To prevent an edit from overwriting a competing content
write, send that base token as one strong quoted `If-Match` header with a
reviewed revision assertion:

```http
PUT /api/v1/observations/<observation-id>
Content-Type: application/json
If-Match: "00000000-0000-0000-0000-000000000001"

{"content":"Corrected repository insight","reviewed_git_sha":"0123456789abcdef0123456789abcdef01234567"}
```

The header accepts one canonical lowercase UUID in quotes, with optional outer
HTTP spaces/tabs. Weak tags, wildcard `*`, lists, repeated header lines, unquoted
tokens and malformed UUIDs return 400. A matching version is checked atomically
and success returns the advanced token. A stale version returns 409 JSON
`{"code":"observation_content_conflict","latest":<canonical Observation>}`
without changing content or side effects. Review `latest` and consciously retry
with its version to rebase. Authorized missing or hard-deleted IDs return 404;
reader/outsider restrictions and deployed-cookie CSRF requirements still apply.
`X-Request-Id` correlates a request and never supplies a precondition.

`If-Match` is required: omission returns 428
`observation_content_precondition_required`. Missing/null/malformed reviewed SHA
returns 400. MCP `observation_update` requires `reviewed_git_sha` and
`expected_content_version`; the thin adapter forwards both to canonical REST.
Content-only clients must upgrade at cutover. No unconditional transition path
is retained. Tokens have no timestamp, request-ID, Git provenance, or ordering
meaning. Same-text and same-SHA updates are valid explicit re-audits.

`GET /api/v1/observations/{observationId}/history` and MCP
`observation_history` return bounded `{items,has_more}` history, default limit 50,
maximum 200, and nonnegative offset. History fields follow the event table and
contain no prior content. Ordinary repository read authorization applies.
`git_sha` retains original-SHA filter meaning. Separate `current_git_sha` and
`history_git_sha` filters match the bound current assertion and any recorded SHA
claim respectively, including labeled legacy creation claims in history only.
Supplied filters combine with AND and never duplicate an Observation. These
semantics apply to lists, unified Observation search, matching, similarity,
aggregate counts, and subject facets so their totals remain consistent.

Observation snapshot envelopes use `schema_version: 2` with the mandatory
nullable `current_provenance` and `latest_sequence` fields. Planning envelope
kinds retain version 1. Cutover invalidates pre-cutover materialized snapshots
and replay/resume sessions, requiring authorized resync before using the new
canonical projection. Live events carry the same committed current fields;
history loads separately and stale responses must not overwrite current state.

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
