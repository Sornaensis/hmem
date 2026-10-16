# Declared attribution reference

The object accepts exactly `role` and `model-id`; actor IDs are server-derived.
Both fields are required strings. Raw input is limited to 128 UTF-8 bytes for
role and 256 for model-id; the complete header is limited to 4096 bytes. Control
and Unicode format characters are rejected. Outer whitespace is stripped, while
case and internal role whitespace are preserved without Unicode normalization.
Model-id has exactly three nonempty `harness:model:reasoning` components, each
using ASCII letters, digits, `.`, `_`, `/`, or `-`. No model catalog is required.
Malformed or repeated declaration headers return `400 invalid_bot_identity`.
Known credential-authenticated bots must declare identity for unsafe `/api/v1`
requests or receive `400 bot_identity_required`; reads may omit it.

Declarations are self-reported metadata; the server does not verify that a caller
uses the named model or performs the named role. They describe attribution and
grant no permissions. Only local mode's implicit `local-user` with synthetic
superadmin authority becomes a declared bot
persona. Its ID is `local-declared-bot:v1:` followed by lowercase SHA-256 of the
UTF-8 JSON array `["hmem-local-declared-bot","v1",role,model-id]`, using canonical
field values and compact JSON encoding. Identical declarations share that local
persona across sessions and workspaces. Credential-backed bots keep their
credential actor IDs; authenticated humans keep their identities. The original
principal, grants, and session/stream audience remain unchanged. Undeclared
no-auth requests remain Local User because the server cannot identify them as
bots.

`GET /api/v1/session` keeps the original authority-bearing `principal` and exposes
attribution separately as optional `effective_actor`. New audit, Observation
revision, Timeline, and live-event records carry optional `bot_identity`; the
browser shows its role and complete model ID while retaining original actor
details. Historical records are not relabeled or given inferred declarations.
Genuine absent-context system events retain their system meaning.
