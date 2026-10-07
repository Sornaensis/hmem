# Observation revision API contract

This is the approved coordinated update contract for core, REST, MCP, OpenAPI,
and Elm. [Database semantics](database.md) define storage and migration.
The provenance ADR retains immutable creation identity and appends reviewed
assertions. Implementation ships these changes together; old content-only
clients must upgrade with that release. There is no retained unconditional
transition or server inspection of Git.

## Canonical shape

Observation reads keep `git_sha` as the original creation SHA and keep the
ordered `subjects`, workspace, content, and opaque UUID `content_version`.
Add required `latest_sequence` and required nullable `current_provenance`:

```json
{
  "latest_sequence": 2,
  "current_provenance": {
    "sequence": 2,
    "event_kind": "update",
    "reviewed_git_sha": "0123456789abcdef0123456789abcdef01234567",
    "content_version": "00000000-0000-0000-0000-000000000002",
    "content_digest": "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef",
    "recorded_at": "2026-10-07T12:00:00Z",
    "actor_type": "user",
    "actor_id": "local-user",
    "actor_label": "Local user"
  }
}
```

The illustrative digest is not an assertion about a particular content string.
The actual digest is SHA-256 of exact accepted UTF-8 content bytes as lowercase
64-character hex. No normalization, trimming, or newline conversion occurs.
The server owns positive sequence, resulting version, time, and actor attribution
from the authorized request context. `actor_type` (`user` or `bot`), textual
`actor_id`, and optional `actor_label` mirror the trusted Principal and existing
audit attribution. Preserve known synthetic local user/bot IDs. A deployed
bot's actor ID is its token ID, distinct from the grant-user ID that authorizes
it; never attribute that bot assertion to its grant holder. Null type/ID is
allowed only for genuinely absent context or unknown legacy attribution.
Client fields cannot override the actor. New creation uses `event_kind: creation`,
sequence 1, and the supplied creation SHA. An update/re-audit uses `update`.
Both are bound assertions. Migrated creation claims use `legacy_creation`,
original SHA, historical creation time, and null version/digest/actor. All
migrated records initially have null current provenance until a new assertion.
Canonical list/get/write/search/match/similarity/snapshot projections agree.
The digest/history API provides no old content, snapshots, or diffs.

## Update and conflict

`PUT /api/v1/observations/{observationId}` requires one strong quoted lowercase
UUID `If-Match` and JSON fields `content` and `reviewed_git_sha`. SHA must match
`^[0-9a-f]{40}$`; content retains its existing UTF-8 byte limits. Unknown request
fields do not grant access to server-owned event fields or immutable identity.

```http
PUT /api/v1/observations/00000000-0000-0000-0000-000000000010
Content-Type: application/json
If-Match: "00000000-0000-0000-0000-000000000001"

{"content":"Corrected repository insight","reviewed_git_sha":"0123456789abcdef0123456789abcdef01234567"}
```

After existing workspace authorization and cookie-CSRF checks:

| Outcome | HTTP response | Mutation |
| --- | --- | --- |
| Missing precondition | 428, `observation_content_precondition_required` | None |
| Malformed precondition or missing/null/invalid SHA/content | 400, `validation_error` | None |
| Absent/hard-deleted Observation | 404 | None |
| Stale expected version | 409, `observation_content_conflict`, `latest` canonical Observation | None |
| Matching version | 200, canonical Observation with fresh version/current event/head | One atomic assertion |

Preconditions allow optional outer HTTP spaces/tabs. Weak tags, wildcard, lists,
repeated headers, unquoted/malformed tokens are invalid. Two writers sharing a
base cannot both succeed: the loser receives the winner's canonical state and
must consciously rebase. Repeated SHA/text is accepted with fresh version and
event. There is no request-ID/SHA deduplication. Authorized actor, version,
sequence, digest, content, embedding invalidation, job reset, audit, and outbox
effects commit atomically; failure leaves them all unchanged. Readers/outsiders
and non-repository workspaces retain existing restrictions. Creation request
shape stays compatible and now binds its accepted content to creation SHA.

MCP `observation_update` requires `observation_id`, `content`,
`reviewed_git_sha`, and `expected_content_version` (canonical lowercase UUID).
It translates the token to REST `If-Match`, forwards server conflicts including
`latest`, and retains bounded response conventions. It adds no persistence or
Git inspection. Core removes its unconditional update API rather than providing
a permanent bypass. REST, MCP schema, OpenAPI, core callers, and Elm upgrade in
one coordinated cutover. Existing manually supplied embedding operations remain
separate and retain their contracts.

## History and filters

`GET /api/v1/observations/{observationId}/history?limit=50&offset=0` and MCP
`observation_history` (`observation_id`, optional `limit`, `offset`) return
`{items,has_more}`. Each item has the event fields shown above plus
`observation_id`. Sort descending by sequence; default limit is 50, maximum 200,
and offset is nonnegative with the existing integer bounds. Use bounded
overfetch to calculate `has_more`. Empty pages are valid. Concurrent appends
can shift offsets; clients refresh from offset 0 when canonical head changes
and must discard results stamped with old identity/session/head. History has
the same repository read authorization as the parent Observation; absent/deleted
parents return 404. Hard deletion cascades compact events; audit/outbox retention
follows existing policy and offers no Observation recovery.

Keep exact `git_sha` filters for original creation provenance. Add exact
`current_git_sha` for the bound current event and `history_git_sha` for any
recorded SHA claim (including the explicitly unbound legacy creation claim).
Null current provenance matches no current-SHA filter. Every supplied SHA must
be a lowercase full SHA; filters combine with AND and subject/text rules remain
unchanged. History uses EXISTS so repeated assertions never duplicate rows or
counts. Apply consistently to list, unified Observation search, path matching,
similarity, counts, and subject facets; embedding-space restrictions stay intact.
Paginated results keep their existing envelope and bounds.

## Embedding and live compatibility

Retain the current immutable-creation-SHA + ordered-subjects + exact-content
embedding fingerprint and its existing byte encoding in both core and manual
NDJSON code. Reviewed SHA, sequence, version, and event digest are excluded.
Every accepted assertion clears any vector and space label and resets leased
work regardless of equal fingerprint: old owners/attempts cannot renew or store
delayed vectors. Enabled jobs are requeued atomically. Manual exports with
unchanged input can still apply across SHA-only or same-text assertions;
changed-content imports are stale. Retain versions 1/2 of the manual exchange,
one stored vector, exact space matching, and operations without pgvector.

Observation snapshot items change to `schema_version: 2` and require nullable
current provenance and history head. Other snapshot kinds stay version 1.
Invalidate old materialized snapshots and replay/resume tokens at cutover
before serving the new contract. Clients resync on old/incompatible Observation
envelopes rather than silently accepting missing fields. Live event reductions
use the committed canonical projection and version/head to retire stale editor
and history results; full history remains a separately requested bounded page.

Elm editing requires a reviewed SHA and preserves the expected version captured
at edit start. Conflict handling preserves the draft and requires conscious
rebase. Read-only views distinguish Original revision, Current reviewed revision,
and unknown legacy current provenance. History shows SHA, sequence, version,
digest, time, actor/unknown attribution, and legacy claim labels. Session change,
deletion, authorization loss, or a newer canonical head retires stale history
work. It does not alter subject identity or introduce historical content editing.

## Required verification

Verify honest populated legacy migration and atomic ledger registration; exact
UTF-8 digests including Unicode/newlines; creation and repeated assertions;
two-writer conflicts with no rejected side effects; ordered bounded history;
immutable identity and event guards; original/current/history filters without
duplicates, including count/facet consistency; authorized reads/writes/history,
CSRF and deleted/absent behavior; pgvector present/absent;
and event/audit actor parity for local user/bot, deployed user and deployed bot
with distinct token/grant-holder IDs; vector invalidation
and old worker rejection even for identical input; reusable same-input manual
exports and stale changed-content imports with core/export fingerprint parity;
snapshot v2 and old-session resync; MCP/OpenAPI shape; and Elm editing/conflict/
unknown/history/stale-response flows. Local task checks and final cross-system
suite acceptance are separate. Routine success is reported with exact commands,
outcomes, and input provenance, without requiring retained raw success logs.
