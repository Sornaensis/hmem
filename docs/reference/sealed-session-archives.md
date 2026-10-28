# Sealed session archives

This is the implementation contract for post-seal storage and full-session
viewing. The codec probe below establishes feasibility, not completion of the
database, HTTP or browser implementation.

## Representation

Every explicitly or automatically terminated session gets one logical
`gzip-v1` object, including empty, tiny and incompressible transcripts. Active
append and live viewing keep their existing contracts.

Feed the original BYTEAs in increasing `start_offset` order into one zlib
compressor, checking contiguous offsets and append revisions. Preserve the exact
bytes: no JSON reserialization, newline conversion or added separator. Use
DEFLATE level 6, window bits 31, memory level 8, default strategy, mtime zero,
OS 255, and no filename, comment or extra header. Record the implementation
version with the codec parameters. Finalize once. Neither an original chunk nor
a physical output-part boundary starts another gzip member.

Use 64 KiB I/O buffers, source chunks and compressed output parts of at most
1 MiB, and one archive worker initially. Emit a `Z_BLOCK` boundary at every
1 MiB raw interval. This retains the compression dictionary; `Z_FULL_FLUSH`,
compressor resets and independent gzip members are outside this format. Output
part payloads use PostgreSQL `EXTERNAL` storage; checkpoint dictionaries remain
`EXTENDED`. Measure their actual TOAST and index sizes rather than assuming a
storage saving.

All authoritative byte counts, offsets and revision counters are checked
64-bit values. Use an opaque C shim around zlib's ABI and checked `uInt`
conversions only for bounded buffers. On Windows, `uLong` can be 32-bit. The
gzip ISIZE trailer is modulo 2^32 and never replaces `final_bytes`.

## Durable storage and publication

Use sidecars, leaving terminated session metadata and its final receipt intact:

| Relation | Required contents |
| --- | --- |
| Archive jobs | Session/workspace binding, queued/running/retry/ready/failed state, attempt, generation, lease token/expiry, retry time, progress and bounded error code |
| Archive generations | Immutable codec/parameters, original final count/SHA-256, compressed count/SHA-256, part/checkpoint/ledger counts and metadata digest, publication time |
| Archive parts | Generation, ordinal, compressed start/end offsets, payload and digest binding both position and payload |
| Archive checkpoints | Generation, raw/compressed offset, unused-bit count, dictionary, dictionary digest and metadata digest |
| Original append ledger | Session/workspace, original revision/start/end, `allow_final_record`, record count/end-newline and raw-chunk SHA-256 |

The SQL names and persistence boundary are listed below. All sidecars enforce session/workspace identity and foreign
keys; staged generations are invisible to ordinary readers. Ordinary writes
cannot mutate published parts, checkpoints or ledger entries, or delete raw
evidence. Cleanup uses a narrowly scoped maintenance operation, not a
caller-settable bypass of evidence guards.

Seal completion only makes work eligible. A fair scan also finds existing sealed
sessions, so a crash between seal and enqueue cannot lose work. Claim with a
generation-fenced lease: 60-second lease, renew at most 15 seconds apart, one
worker, and retry backoff from 1 to 60 seconds. Yield and release database
connections between bounded source/output batches. A failed attempt starts a
fresh generation; it never resumes an unproven compressor state.

Read back the persisted output with bounded decompression. Verify the exact
original count/SHA-256, complete single-member gzip termination, compressed
count/SHA-256, every part/checkpoint binding and the retained chunk ledger.
Generate and persist checkpoints incrementally during this pass. Never keep an
all-session vector of dictionaries or ledger rows in memory. Atomically publish
the complete verified generation only while its lease and original receipt
still match. An expired owner cannot publish. Keep the raw payload until the
compatible readers below are installed and publication has been verified.
Abandoned staging cleanup is bounded and cannot touch a published generation.

### Storage primitives (V050)

`V050__sealed_session_archives.sql` installs `session_archive_jobs`,
`session_archive_generations`, `session_archive_parts`,
`session_archive_checkpoints` and `session_archive_ledger`, including its migration
ledger entry, in one transaction. It does not enqueue existing evidence, change
sealing, install a worker, or enable raw cleanup. The `session_archives` view
resolves only a published generation bound to its ready job; ordinary readers
use that binding and pass its immutable generation ID on subsequent reads.

`HMem.DB.SessionArchive` scans eligible sessions with a bounded keyset cursor,
claims one due job with a fresh attempt/generation/token and a 60-second lease,
and renews only an unexpired current lease. Retry starts a new generation, resets
progress and uses 1–60-second exponential backoff. Worker code must renew at most
15 seconds apart. Each database operation releases its connection; source ledger
copy and abandoned cleanup handle at most eight rows per call. Parts are at most
1 MiB and checkpoint dictionaries at most 32 KiB. The caller continues ledger
copy from its last returned original revision. A completed scan restarts for
newly sealed sessions, including a seal whose enqueue was interrupted.

Staging serializes under job then generation locks, checks original receipts and
ordered positions, computes versioned SHA-256 chains over positions and payload
digests, and validates each ledger row against its original chunk. A manifest
hash commits codec parameters, receipts, counts and all three chain heads.
Publication checks those structural boundaries under the current lease and
atomically marks the generation and job ready. Published rows reject ordinary
updates, deletes and truncation. Raw evidence guards remain in force. There is
no caller-settable bypass; the existing privileged test fixture reset suspends
only its explicit list of evidence truncate guards within its owned transaction.

`ArchiveVerification` is an internal trusted-worker attestation, supplied only
after complete persisted-output readback. SQL checks lengths, counts and stored
bindings; it does not decode gzip or establish the attested whole-object digest.
The worker must verify single-member termination, the compressed digest and
original count/digest before calling `publishArchive`. Storage tests use opaque
bytes to exercise this boundary and cannot qualify codec, reader or client work.
Staged output is available only through an explicit lease-fenced readback
primitive; public-facing consumers use published reads.

Releasing a current lease to retry or terminal failure detaches its unpublished
generation, making its staging eligible for cleanup even when no retry follows.
Cleanup accepts a known obsolete unpublished generation, locks its job and
generation, and removes at most eight child rows per call, then its empty
generation on a later call. It cannot delete the current or a published
generation, the sealed receipt or original chunks. Operators retain raw evidence
on any failure. Recovery uses the existing database backup procedure; there is
no destructive down migration for published evidence.

### Server worker

`HMem.Server.Archive.Worker` owns one background loop in the production server,
dev server and interactive harness. Each turn scans at most 32 sealed sessions,
cleans at most eight obsolete staging rows, and claims one due job. Both explicit
termination and the idle-session reaper become eligible through that scan. The
worker never changes task state, the sealed receipt or original payloads.

`HMem.SessionArchive.Codec` uses an opaque C zlib context with fixed 64 KiB
buffers and checked 64-bit counts. Compression writes physical parts without
resetting the stream. The first persisted readback derives restart points; a
second independently derives and compares their exact offsets, bits and
dictionary bytes, verifies each original ledger boundary and raw-chunk hash,
and checks both whole-stream digests before publication. Index rows and original
chunks are processed individually. Failed attempts retain raw evidence and
retry with a fresh generation. A dedicated owned child renews the lease every
10 seconds with a five-second request deadline. Attempt and shutdown deadlines
remain those specified above; an unconfirmed join is an explicit failure.

Dev and harness entrypoints use the opt-in `SandboxLifetime` borrower helper.
A failed worker drain retains its exact handle, pool, database and sandbox
release actions. Recovery retries that same capability before releasing them
in dependency order. This opt-in path owns a foreground PostgreSQL handle,
disables its logging collector, retains one in-flight orderly-stop helper, and
requires both process joins and PID-file completion before deleting the root.
Stop and deletion failures propagate and retain the unfinished release actions.
Ordinary test helpers retain their existing behavior;
this guard does not implement the deferred general test supervisor.

## Raw reads, proofs and replay

Use zran-style restart points: raw offset, compressed byte offset, 0–7 unused
bits from the preceding byte, and up to 32 KiB of preceding raw dictionary.
The initial point follows the gzip header at raw offset zero and has an empty
dictionary. Restore with raw inflate, `inflatePrime` and
`inflateSetDictionary`. Checkpoint raw gaps, including the final tail, are at
most 1 MiB. The database selects a point by indexed lookup; the worker emits
one checkpoint at a time. The official zran example's growing in-memory index
is not the production storage model.

Arbitrary ranges locate the complete original chunks overlapping the request.
Restore at or before the first overlapping chunk's start, verify each complete
overlapping chunk against its immutable ledger hash, and only then return its
requested slice. Verify checkpoint metadata/dictionary and compressed part
digests before use. An incorrect dictionary can produce plausible output
without an inflate error; successful decompression alone is insufficient.
For a request of L bytes, chunk verification covers at most L + 2 MiB and
inflation covers at most L + 3 MiB including checkpoint skip. Empty and exact
end-of-session ranges retain their existing responses.

Keep exact replay semantics, including replay checked before terminal-state
rejection: decode and verify the original chunk, then compare actual payload
bytes and `allow_final_record`. Hash equality alone does not replace replay
equality. Historical prefix proofs continue to recognize original revision/end
boundaries. A proof hashes only its requested prefix and never reads beyond
that append boundary.

Prefix workers and ZIP exports own sequential decoder state across their fair
turns instead of recreating it for each 64 KiB window. Preserve the existing
two prefix workers and two ZIP admissions. For ordinary paginated evidence
reads, use an application-owned cache of four verified-byte entries keyed by
resolved authenticated principal/account and credential or authentication-session
context, workspace, session and immutable generation. An audience is that
resolved security identity, not a generic JWT `aud` value or transport channel;
reuse the existing equivalent application type where available. Each entry
has a 30-second idle TTL and at most two verified original chunks (2 MiB);
the global verified-chunk cache is at most 8 MiB. Serialize use of an entry.
Between ordinary requests, entries retain verified bytes only. A cache fill
owns its checkpoint decoder and closes it before returning; it does not retain
an advancing native decoder, transaction, connection or background reaper.
Repeated tiny pages within those verified chunks use the cached bytes. Crossing
the retained chunk boundary performs another bounded checkpoint fill. Prefix
and ZIP processing retain their explicitly owned sequential decoder contexts.
Recheck current authorization before lookup or reuse. No cached state owns a
transaction or pool connection between requests. Evict only idle entries;
admission failure returns retryable 503 with `Retry-After: 1`. Offset mismatch,
expiry, eviction or process restart uses the fixed-gap checkpoint fallback.
Measure both cache-hit paging and this bounded fallback; the standalone probe
only proves decoder reuse as a primitive.

Pin an immutable ready generation for archived reads. Existing READ COMMITTED
metadata/raw statements can race raw reclamation: on a missing/short raw read,
re-resolve the source and retry against the published archive once. If no
consistent complete source is available, fail closed. Range reads, proofs,
replay and ZIP scans must cover this race; a short read is never successful EOF.

Archived range service allocates 25 seconds to logical work and reserves five
seconds for database/native decoder cleanup within its 30-second budget.
Reader-owned lookup statements use transaction-local `lock_timeout` of at most
one second and `statement_timeout` of at most four seconds, preserving stricter
existing settings. Each guarded query owns its transaction and returns its pool
connection before decoding or consumer callbacks. A typed archive-read failure
retains the database cause internally; the HTTP API returns a fixed
`archive_unavailable` 503 with `Retry-After: 1`.

The terminated evidence route selects the dedicated
`readSessionEvidenceRange`/`readSessionEvidenceRangeWithCache` entrypoints using
its existing route identity. Their session metadata and raw-first SELECTs use
the same local query guard as the archive lookups. Active and direct legacy
range functions retain their existing behavior; selecting the guarded path
does not add a state-probe query or a foreground round trip.

This guard does not establish hard cancellation for the whole route. Existing
authentication, the readiness enqueue mutation, pool acquisition and transaction
context setup remain outside the guarded SELECT body. Active/direct-legacy raw
calls and arbitrary driver/network stalls retain their inherited behavior.
The 25-second timer requests cancellation; cleanup still must finish before
releasing cache/native ownership.
Active range requests retain their existing 30-second logical work deadline.

## HTTP and client

Add authorized session archive readiness/manifest and content routes alongside
the existing APIs. Under `/api/v1/workspaces/{workspaceId}/sessions/{sessionId}`:

| Method/path | Contract |
| --- | --- |
| GET `/archive` | 200 ready manifest; 202 preparing/retrying manifest with retry guidance; 503 failed manifest; 409 `session_state_conflict` for active sessions |
| GET `/archive/{generationId}/content` | 200 complete gzip or authorized 304 for that exact published generation; 404 for an unknown/unbound generation; 503 `archive_unavailable` if a published object cannot be read completely |

Both routes preserve existing not-found/authorization behavior and require
current read permission. The manifest shape is:

```json
{
  "workspace_id": "uuid", "session_id": "uuid", "state": "ready",
  "generation_id": "uuid", "codec": "gzip-v1", "codec_implementation": "zlib-version",
  "raw_bytes": "1073741824", "raw_records": "1048576", "raw_sha256": "lowercase-hex",
  "compressed_bytes": "42846669", "compressed_sha256": "lowercase-hex",
  "content_path": "/api/v1/workspaces/uuid/sessions/uuid/archive/uuid/content",
  "progress": {"phase": "ready", "raw_bytes": "1073741824", "compressed_bytes": "42846669"},
  "retry_after_ms": null, "error": null, "message": null, "retryable": false
}
```

`state` is `preparing`, `retrying`, `ready` or `failed`. Pending states expose
the fixed raw receipt, current-attempt progress and bounded `retry_after_ms`;
generation, codec implementation, final compressed fields and content path are
null until publication. Progress phase is `queued`, `compressing`, `verifying`
or `ready`. A transient retry uses 202 with `Retry-After` and resets attempt
progress honestly. Failed state uses 503 with a bounded `error`/`message` and
`retryable: false`; it requires operator repair rather than client polling.
Client network/transient-content failures may retry the same immutable object.
Ready content
is the complete stored object with `Content-Type: application/gzip`, compressed
`Content-Length`, compressed SHA-256, generation and an ETag bound to that
generation/digest. Bind response metadata using `X-HMem-Archive-Generation`,
`X-HMem-Archive-SHA256`, `X-HMem-Archive-Raw-Bytes` and
`X-HMem-Archive-Raw-SHA256`; the client compares them with its manifest before
accepting the body. Do not add `Content-Encoding: gzip` or run it through another
HTTP compressor: JavaScript must receive the stored compressed bytes. Use
private revalidated caching, authorize conditional reads before 304, and recheck
authorization between bounded streamed parts. Release pool leases before
network writes. New manifest counts/offsets use decimal strings so JavaScript
does not round 64-bit integers.

`HMem.Server.Archive.HTTP` implements these routes as application middleware.
It validates the immutable manifest and reads all bounded compressed parts once
before successful content headers, checking exact positions, bindings, count
and SHA-256. It reads the same pinned generation again for delivery. This costs
two compressed storage reads and must be included in full-download latency
measurements. Each pass retains at most one 1 MiB part and one hash context;
neither uses a transcript-sized buffer or temporary file. Preflight and transfer
each have a 15-minute logical work deadline. Disconnects and cancellation unwind
the request without a database lease spanning network backpressure; the query
guard and inherited cancellation limits above still apply.

The server re-resolves the request's credential or cookie session and checks
current workspace permission before metadata, conditional responses and every
bounded part fetch/write. Content cache policy is `private, no-cache,
no-transform` with `Vary: Authorization, Cookie`. The ETag includes the immutable
generation/digest and a hash binding the resolved authority and credential/session
context. An old context's ETag cannot authorize a 304 under a new context.
Manifests use `no-store`. Browser local-data ownership still requires the client
logout/principal-switch invalidation described below.

The sealed viewer waits for this object; it does not fall back to raw range
viewing. A worker stages the complete compressed download in a private local
generation and verifies compressed count/hash. Only after complete transfer
does it stream decompression into an OPFS raw file, incrementally hash bytes
with the existing SHA-256 library, and build a persistent record/event index.
Validate gzip termination and the original final count/hash before publishing
the local generation or showing transcript content. A failed transfer,
decompression, parse, hash or quota check publishes nothing.

Factor the existing incremental framing/presentation into an indexed sink.
Retain cross-record `pairOccurrence` matching, activity-group anchors/upserts,
disclosure identity and ordering; independently parsing each page is insufficient.
Store raw record offsets and event/occurrence references locally. Commit at most
256 index records or 1 MiB per batch, with backpressure. Keep worker messages,
navigation reads and the DOM bounded by the existing 200-row/4 MiB presentation
window while allowing navigation across the entire verified session.

Cancel download, decoding and indexing on selection change, explicit cancel,
authorization loss or disposal. Tag every message with session, generation and
load token; stale completions cannot replace the current view. Close worker
handles before deleting unverified staging. Reusing verified local data still
requires current server authorization and the same audience/workspace/generation.
Clear its ownership on logout, principal switch or credential/session change,
even when workspace and session IDs stay the same.

The initial qualification platform is desktop Windows Chromium, matching the
repository's native test platform; record actual browser/build/device versions.
Require Worker, gzip `DecompressionStream`, worker OPFS and IndexedDB support by
feature detection. Other browsers must pass the same integration gates before
being advertised. Missing capabilities or insufficient quota produce an explicit
unsupported/resource error, never a reduced raw-window view. Before transfer,
check storage estimates against compressed + raw bytes plus an index allowance;
recheck after each batch and handle actual quota errors. Estimates do not promise
that a large session will fit. Local disk grows with compressed bytes, raw bytes
and index size; processing memory must not grow with transcript size.

## Budgets and remaining qualification

These are prospective implementation gates, not measured product results:

| Work | Limit |
| --- | --- |
| Archive attempt through verified publication | max(30 s, 120 s × ceil(raw GiB)); timeout retains raw evidence and retries |
| Legacy range request | p95 ≤ max(500 ms, twice the matching raw baseline); finite 30 s service deadline remains required; inherited cancellation limits above remain unqualified |
| Archive-related incremental server working set | ≤ 128 MiB over a matching idle/foreground baseline, fixed across 1 MiB/100 MiB/1 GiB |
| Client incremental processing memory | ≤ 128 MiB over the matching viewer baseline; index/decoded data staged on disk |
| Full-view deadline at controlled 100 Mbit/s | 2 × compressed bytes / 12,500,000 + 120 s × ceil(raw GiB) + 5 s; empty sessions use one size unit |
| Loading/cancel acknowledgement | ≤ 250 ms; subsequent verified local navigation/interaction p95 ≤ 100 ms |
| Active/live and unrelated foreground work | Existing guarantees; matched p95 regression ≤ 10% |

The codec calibration below supports fixed buffer/index bounds and leaves broad
headroom for SQL, hashing and indexing in these gates. It does not establish
those implementation costs. Qualification must measure actual TOAST + heap +
index + dictionary + ledger storage, CPU, memory, WAL, transient staging and
vacuum/reuse behavior. No storage ratio is promised: every sealed transcript
still gets an archive when compression expands it. Keep the existing live-view
latency contract; full sealed viewing supersedes the latest-window timing only
for this new path.

Require byte fidelity and crash/publication/lease/reclamation races, authorization
loss, altered parts/checkpoints/dictionaries/ledger, exact replay, historical
proofs, ZIP parity, repeated tiny pages, random ranges, client stale-load and
quota cleanup, framing across index/page boundaries and complete 1 GiB viewing.
The final task owns matching database/HTTP/browser acceptance and rollout; an
isolated codec result cannot close those gates. Backfill and raw cleanup remain
disabled until compatible readers and the full-object path pass qualification.

## Reproducible codec baseline

Run the standalone C probe in a verified task-owned scratch directory with a
unique unused output prefix. It generates deterministic data without storing a
raw transcript, writes one checkpoint at a time to a temporary index, and removes
its compressed/index files on success. A failed run retains only those owned
files for diagnosis. Each process has a 120-second supervisor deadline. Use the
same input/build provenance for comparisons.

```text
stack exec ghc -- -no-hs-main -optc-O2 -optc-Wall -optc-Wextra tools/session-archive-probe.c -odir <scratch> -o <scratch>/codec_probe.exe -lz -optl-lpsapi
stack exec <scratch>/codec_probe.exe -- 2097169 1021 jsonl <scratch>/small
stack exec <scratch>/codec_probe.exe -- 0 65536 jsonl <scratch>/empty
stack exec <scratch>/codec_probe.exe -- 1 1 jsonl <scratch>/one
stack exec <scratch>/codec_probe.exe -- 1048576 65536 jsonl <scratch>/oneMiB
stack exec <scratch>/codec_probe.exe -- 104857600 65536 jsonl <scratch>/hundredMiB
stack exec <scratch>/codec_probe.exe -- 1073741824 65536 jsonl <scratch>/oneGiB
stack exec <scratch>/codec_probe.exe -- 1048576 1021 random <scratch>/randomMiB
stack exec <scratch>/codec_probe.exe -- 104857600 65536 random <scratch>/random100MiB
```

For Linux, omit `-optl-lpsapi`. The wrapper deadline is external to the probe;
do not launch it without finite supervision. The JSONL generator repeats
1024-byte records with UTF-8 and CRLF plus varying identifiers/content. `random`
is a deterministic poorly compressible binary codec case, not a valid JSONL
session fixture. Prefix-sized small cases can end within a generated record and
test codec fidelity only. Neither generator is a measured sample of production
transcripts.

On 2026-10-28, all eight cases exited zero in one sequential run at repository
base `562a16d35d24fe779ff743dd5b6ba363baee27a3`, source SHA-256
`cb21b3558cd442443bfebdc98ecba10d42c30361e715f80f4741e7c358d8c885`.
GHC 9.10.2 compiled the C probe; header zlib 1.2.13 and resolved runtime 1.3.2
passed native initialization (88-byte `z_stream`, 4-byte `uLong`). Node 24.15.0
only supervised child deadlines. These are single attempts with uncontrolled
OS caches, not percentiles or browser timings.

| Input | Gzip bytes | Actual dictionary bytes | Compress s | Verify/index s | Sequential decode s | Peak process bytes |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Empty | 20 | 0 | 0.000306 | 0.005441 | 0.000043 | 8,617,984 |
| 1 MiB JSONL | 41,937 | 0 | 0.004213 | 0.008002 | 0.001948 | 8,839,168 |
| 100 MiB JSONL | 4,184,331 | 3,244,032 | 0.382543 | 0.198473 | 0.190129 | 8,769,536 |
| 1 GiB JSONL | 42,846,669 | 33,521,664 | 3.976861 | 1.994341 | 2.049261 | 8,761,344 |
| 100 MiB binary | 104,889,618 | 3,244,032 | 1.593984 | 0.143243 | 0.130354 | 8,761,344 |

The 1 GiB case had 1024 checkpoints, 907 non-byte-aligned points and a maximum
1 MiB gap. All checkpoint seeks matched; deliberately wrong dictionaries and
restart bits were rejected by byte comparison. Its 262,614 nonempty output
pages reused one decoder. Small cases also produced identical gzip bytes under
different source-array cuts. Full decode required exact EOF after one member, and
empty/trailer handling completed. The >4 GiB check covers arithmetic only;
no >4 GiB fixture ran. The fixed probe index occupies 33,579,008 bytes at 1 GiB
because each disk record reserves a whole dictionary; production stores only
actual dictionary bytes and database overhead. This overhead can approach the
gzip size and must be included in savings comparisons.

### Isolated TOAST setting comparison

One bounded native PostgreSQL 18.2 fixture, using 8192-byte pages and default
`pglz`, compared seven synthetic payloads in separate one-row tables under both
storage settings. It ran without product migrations, API/browser code or a
production database. The same 1 MiB generator supplied raw bytes, its whole gzip
and the last 32 KiB dictionary. The complete 14-row measurements, Haskell
fixture source, finite supervisor source, exact package IDs/commands and input
hashes are in the machine-readable baseline. The fixture's nested cluster was
stopped and its contained directory removed before success was recorded.

| Payload | Logical bytes | EXTENDED stored value / method | EXTERNAL stored value | Total relation bytes EXTENDED / EXTERNAL |
| --- | ---: | --- | ---: | --- |
| Raw JSONL | 1,048,576 | 64,245 / pglz | 1,048,576 | 139,264 / 1,163,264 |
| Gzip JSONL | 41,937 | 41,937 / none | 41,937 | 114,688 / 114,688 |
| Gzip binary | 1,048,914 | 1,048,914 / none | 1,048,914 | 1,163,264 / 1,163,264 |
| JSONL dictionary | 32,768 | 2,096 / pglz | 32,768 | 49,152 / 106,496 |
| Binary dictionary | 32,768 | 32,768 / none | 32,768 | 106,496 / 106,496 |
| Empty gzip | 20 | 21 / none | 21 | 32,768 / 32,768 |

Keep `EXTERNAL` for already-compressed output and `EXTENDED` for dictionaries:
TOAST did not shrink either gzip payload here, while it substantially shrank
the compressible dictionary. This comparison establishes a storage-setting
choice, not a measured CPU or substring-read advantage. Whole-part integrity
checks already materialize each compressed part. The binary gzip value is a
bounded whole-object entropy check, slightly above the production 1 MiB part
limit; the production writer still splits output at that limit.

Per-value size excludes table/TOAST/index overhead; total-relation sizes include
it and are page-rounded. Separate one-row relations cannot establish the
combined archive schema's saving, and raw data already benefits from TOAST.
Do not extrapolate these rows into a measured 1 GiB production saving. Final
qualification measures shared relations, all ledgers/indexes, WAL and reclamation.

The [machine-readable baseline](../../tools/session-archive-probe.baseline.json)
retains all eight case aggregates, environment, source hash, exact commands and
the finite supervisor source needed to reproduce the run after scratch cleanup.
The compact table and reproducible source are owned by repository maintainers
until replaced by the matching integrated qualification; remove superseded
calibration detail then. Task scratch and generated archives are disposable
after review, with the implementation owner responsible for contained cleanup.

