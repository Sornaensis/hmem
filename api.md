# API guide

The HTTP API is available at `/api/v1` on your hmem server, normally
`http://127.0.0.1:8420` for a local installation. Requests and responses use JSON.

## Connect and discover endpoints

Choose a credential using [the authentication guide](auth.md). Service and MCP
clients use a bearer token; the browser normally uses an OIDC session. Local
native bootstrap can work without a token, but the default Docker setup requires
one. Cookie-authenticated POST, PUT, and DELETE requests need the CSRF header,
including POST endpoints that only search or count.

With your token in `HMEM_AUTH_TOKEN`, check your session and fetch the server's
OpenAPI document:

```bash
curl --fail-with-body -H "Authorization: Bearer $HMEM_AUTH_TOKEN" \
  http://127.0.0.1:8420/api/v1/session
curl --fail-with-body -H "Authorization: Bearer $HMEM_AUTH_TOKEN" \
  http://127.0.0.1:8420/api/v1/openapi.json -o hmem-openapi.json
```

Use that OpenAPI document for the current routes, request fields, and response
schemas. Send `Content-Type: application/json` with JSON bodies. Use HTTPS for
shared installations. [MCP setup](README.md) shows how to use hmem through tools;
it uses a bearer credential and an active workspace context.

## Workspace lifecycle graph

`GET /workspaces/{id}/timeline/buckets` exposes canonical `series` counts for
`created`, `completed`, `deleted`, `archived`, and `cancelled`. Legacy `counts`
and `totals` retain their existing meaning. Observation archive and cancel counts
are zero because Observations do not support those lifecycle actions.

The optional `paged=true` query supports long graph ranges while each response
keeps the existing ten-year and 366-bucket bounds. Follow `next_since`, keeping
`until` and `bucket` unchanged, until it is null. With `paged=true`, omitting
`since` starts at the workspace's earliest eligible lifecycle audit event. Without
the opt-in, `since` remains required and oversized ranges still return HTTP 400.

## Workspaces and planning

Workspaces scope your data and permissions. Create one with
`POST /api/v1/workspaces`, using `name` and `workspace_type` (`repository`,
`personal`, `organization`, or `planning`). Creation needs the global `create_workspace` grant
or superadmin access. List accessible workspaces with `GET /api/v1/workspaces`.

Projects and tasks can belong to any workspace. Observations require an active
`repository` workspace, including when requested by ID. Use `workspace_id` in
workspace-scoped requests; possession of a resource UUID does not grant access.

For planning clients, create projects/tasks through their OpenAPI routes or use
`POST /api/v1/projects/spec` to create a project with 1–50 initial tasks.
Tasks allow one level of subtasks. Start a parent before starting its subtask,
satisfy dependency prerequisites, and finish open children before marking a task
done or a project completed. A lifecycle conflict returns HTTP 409 with a
`code`, `message`, and optional `detail`/`hint`; resolve the reported blockers
before retrying. Archiving a project and cancelling a task can affect descendants,
so check the returned result rather than assuming only one record changed.

Most list routes return `{"items":[...],"has_more":true}`. Start at `offset=0`,
advance by the number of returned items, and stop when `has_more` is false.
Ordinary list pages default to 50 items and allow up to 200; navigation pages
allow up to 100. Observation limits are 1–200 and offsets 0–100000. Check OpenAPI
for route-specific pagination, especially unified search and similarity, whose
response shapes differ.

## Create an Observation

An Observation records an insight about repository files at a Git revision.
For example, send this body to `POST /api/v1/observations` with edit access:

```json
{
  "workspace_id": "00000000-0000-0000-0000-000000000001",
  "subjects": [
    {"subject_kind": "file", "subject": "src/Main.hs"},
    {"subject_kind": "glob", "subject": "src/**/*.hs"}
  ],
  "git_sha": "0123456789abcdef0123456789abcdef01234567",
  "content": "The server loads configuration before connecting to PostgreSQL."
}
```

Replace the example UUID and SHA with your workspace and reviewed revision.
SHAs must be 40 lowercase hexadecimal characters. Subjects use repository-relative
forward-slash paths without `./`, `..`, absolute paths, or backslashes.
Glob subjects support `*`, `?`, and whole-component `**`; file subjects are
concrete paths. Supply 1–256 subjects, each at most 4096 UTF-8 bytes and together
at most 256 KiB. Content must be nonempty and at most 512 KiB.

Creation returns the Observation, including its ID and `content_version`.
Its workspace, ordered subjects, and original `git_sha` cannot be changed.
Create a new Observation when correcting that identity. The server accepts your
revision assertion; it does not inspect a Git checkout.

## Update without overwriting another edit

Read the Observation and keep its `content_version`. Send that UUID as one
strong quoted `If-Match` header, with the new content and the revision at which
you reviewed it:

```http
PUT /api/v1/observations/00000000-0000-0000-0000-000000000010
Content-Type: application/json
If-Match: "00000000-0000-0000-0000-000000000002"

{"content":"Updated repository insight","reviewed_git_sha":"0123456789abcdef0123456789abcdef01234567"}
```

Add your authentication header, or session cookie and CSRF header.

| Response | Next step |
| --- | --- |
| 200 | Keep the returned Observation and its new version. |
| 428 `observation_content_precondition_required` | Read the Observation and supply `If-Match`. |
| 400 | Correct the body or header. Use one quoted lowercase UUID; weak tags, `*`, lists, repeated headers, and unquoted values are invalid. |
| 409 `observation_content_conflict` | Review the returned `latest` Observation, reconcile your draft, and deliberately retry with its version. |
| 404 | The Observation is absent or deleted; do not recreate it automatically. |

A successful same-text update is a new review assertion and advances the version.
Content writes clear the stored embedding, so similarity needs a replacement
vector. Embedding-only writes leave the content version unchanged.
`X-Request-Id` identifies a request; it does not deduplicate repeated updates.

MCP `observation_update` uses `observation_id`, `content`, `reviewed_git_sha`,
and `expected_content_version` instead of an HTTP header.

## History and revision filters

`GET /api/v1/observations/{id}/history?limit=50&offset=0` returns
`{items,has_more}` in descending sequence order, with a maximum page size of 200.
MCP exposes `observation_history`. History records revision assertions, time,
and actor attribution; it contains no previous content or diffs and cannot
restore an Observation. Older creation claims may have unknown attribution and
no binding to current content. A nullable `current_provenance` in an Observation
means there may be no reviewed assertion for its current content.

For Observation listing, search, matching, and similarity:

| Filter | Matches |
| --- | --- |
| `git_sha` | Original creation revision. |
| `current_git_sha` | Revision asserted for current content. |
| `history_git_sha` | Any recorded revision claim, including older creation claims. |

Supplied filters combine with AND. A historical match returns current content,
not content as it existed at that revision. If history changes while paging,
restart at offset 0 to refresh.

## Match files and search by vector

Send concrete repository-relative paths to `POST /api/v1/observations/match`:

```json
{"workspace_id":"00000000-0000-0000-0000-000000000001","paths":["src/Main.hs",".github/workflows/ci.yml"],"limit":50,"offset":0}
```

The response includes each matching Observation once with `matched_paths`,
`matched_subjects`, and `path_matches`. Paths match stored file/glob subjects;
the request does not expand caller globs or read your filesystem. Path count and
byte limits are the same as the subject limits above.

For similarity, supply an externally generated vector to
`POST /api/v1/observations/similar` or MCP `observation_similar`.
Store an individual vector with `PUT /api/v1/observations/{id}/embedding` or MCP
`observation_set_embedding`. Vectors need 1536 finite numbers and matching
embedding spaces. There is no raw-text query embedding endpoint. See
[embeddings](embeddings.md) for setup, request compatibility, and bulk import.

## Deletion and errors

Observation deletion is permanent, including its history. Workspace deletion
requires admin access and soft-deletes the workspace while retaining its contents.
Project/task deletion also retains records for audit-based restoration; permanent
purge requires admin access. Workspace deletion has no MCP tool.

HTTP 401 means the credential needs attention; 403 means permission or CSRF is
missing. A 503 `capability_unavailable` on vector operations means pgvector is
not ready. Check [authentication](auth.md) or [embedding readiness](embeddings.md)
for recovery. For validation and lifecycle errors, use the response message and
hint; avoid blindly retrying the same rejected request.
