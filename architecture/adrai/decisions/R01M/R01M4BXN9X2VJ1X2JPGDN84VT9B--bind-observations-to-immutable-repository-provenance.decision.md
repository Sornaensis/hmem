+++
schema = "adrai/decision/v1"
adr = "A01M38Z69R241CHNFSBT53227BK"
record = "R01M4BXN9X2VJ1X2JPGDN84VT9B"
title = "Bind Observations to immutable repository provenance"
summary = "Fix workspace, Git SHA, and ordered subjects at creation while allowing content corrections."
domains = ["observation-provenance"]
+++

**Context**

An Observation's original repository identity must remain stable while corrected content can be explicitly reviewed at later revisions. The former content-only contract retained creation provenance but could not establish which revision supported corrected content. This amendment defines the approved replacement contract; implementation and combined verification follow separately.

**Decision**

Keep workspace, original `git_sha` (lowercase full 40-character SHA), and complete ordered canonical subjects immutable. Append compact per-Observation events for new creation, every accepted update, and explicit re-audit. Serialize a positive server-owned sequence under the Observation lock. A bound event records `event_kind`, caller-asserted `reviewed_git_sha`, resulting opaque `content_version`, SHA-256 of the exact accepted UTF-8 content as 64 lowercase hexadecimal characters, server `recorded_at`, and authorized server actor identity. No trimming, Unicode normalization, newline conversion, BOM insertion, or full-content snapshot/diff belongs to the digest/history. The server validates assertions but does not inspect Git or certify the caller's claim.

The canonical update requires content, reviewed SHA, and expected version. REST requires one strong quoted `If-Match` UUID; MCP requires `expected_content_version`. Cut over core, REST, MCP, and Elm together, removing the unconditional canonical updater in the same release. There is no retained content-only transition. Identical content and/or repeated reviewed SHA are valid re-audits: each accepted call appends one event and advances the UUID. A conflict returns the latest canonical Observation and changes no event, content, vector, job, audit, or outbox state. Commit the content, event/current projection, embedding invalidation, leased-job reset, audit attribution, and outbox effect atomically. Authorization and repository-workspace restrictions remain unchanged.

Actor attribution mirrors the trusted server Principal: nullable `actor_type` (`user` or `bot`), `actor_id` text, and optional `actor_label`. Preserve known synthetic local user/bot identities and deployed bot token identity exactly; a deployed bot's token ID is not its separate authorization grant-user ID. Null identity is allowed only for genuinely absent context or unknown legacy attribution. Client event actor fields cannot override this context. Verify event/audit parity for local user, local bot, authenticated user, and deployed token bot, including a token whose grant holder has a different ID.

Expose nullable `current_provenance` as the latest content-bound accepted event and `latest_sequence` as the committed history head. New creations bind their accepted content to their original SHA. Migrate all pre-cutover Observations honestly: retain one `legacy_creation` claim with original SHA and historical creation time, but null version, digest, and unknown actor; expose null current provenance even when timestamps match. Do not invent historical update SHAs or bind present legacy content to creation SHA. The first post-cutover accepted update/re-audit establishes a bound current assertion. History distinguishes legacy claims from bound events, uses descending sequence with bounded pagination, and contains no prior content.

Preserve `git_sha` filters as exact original-creation filters. Add separate exact `current_git_sha` and `history_git_sha` filters to list, search, match, and similarity. Current matches only the bound current assertion; history matches any recorded SHA claim, including the explicitly labeled legacy creation claim. Combine supplied filters with AND; use history EXISTS semantics so an Observation appears once. Sequence, UUID, time, and request ID have distinct meanings.

Retain the existing embedding input fingerprint of immutable creation SHA, ordered subjects, and exact content, with existing encoding and matching core/manual-export implementations. It excludes reviewed SHA, sequence, and version. Every accepted assertion clears the vector and its space label and resets existing leased work, even for identical input: old worker owner/attempt/state checks must fail. A manual export with unchanged input remains reusable after a SHA-only or same-text assertion; changed-content imports remain stale. Preserve exact embedding spaces, optional pgvector, and atomic lease fencing. No new provider infrastructure is introduced.

Observation snapshot envelopes move to schema version 2 with mandatory nullable current projection and history head. Invalidate pre-cutover materialized snapshots and resume sessions/cursors at the coordinated deployment boundary so old replay cannot silently preserve missing provenance; clients resync. Planning snapshot kinds retain their existing contract. Live updates carry the same committed canonical projection; paginated history is loaded separately.

Hard deletion cascades Observation events, current state, subjects, vectors, and jobs. It adds no restore or full-history recovery API. Existing attributed audit/outbox records follow their existing access and retention rules and cannot restore an Observation. The forward migration owns its transaction and ledger insertion and does not rewrite historical migrations.

**Consequences and contract authority**

The creation SHA keeps its historical meaning; current assertions and compact history make correction provenance explicit without reconstructing past content. Legacy current provenance starts unknown, old update clients must upgrade, and history pagination is read under ordinary workspace authorization. `database.md` and `api.md` specify event shape, compatibility, filters, migration, and test obligations. Existing subject grammar, audit, migration, embedding, thin-MCP, and live-synchronization ADRs continue to apply.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0ifSwiYiI6IjhlZWRiMWIxYzA4ZTMxMTQ4MjIzODA0ODIyOWM5MjBhNTE3Mzk2YWMiLCJpIjoic2hhMjU2OlI3VGVhRU1OT0t5UHJhREhpaE12OHY2LUd2UG1TbUdxSUpMQ0NqQW5hUmsiLCJrIjoiZGVjaXNpb24uYW1lbmQiLCJvIjoiUjAxTTRCWE45WDJWSjFYMkpQR0ROODRWVDlCIiwib3AiOiJPMDFNNEJYTjlYMlZKMVgySlBHRE44NFZUOUIiLCJwIjpbIlIwMU00QlhBRTlUU0U2WU1QN05BMkFXQTBINiJdLCJyIjoibWFzdGVyIiwicyI6InNoYTI1NjpwdTFFSkF2LXBDd28wZ1JyR25Fei02WGxYUWdidlhMRHhVRC1RNnQ3b2xzIiwidCI6MTc5MTQwMTU2ODE2MiwidiI6MSwieCI6ImFkcmFpLzEuMC4wIn0 -->
