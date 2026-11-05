# Sealed session archives

Session `last_activity_at` is a separate [occurrence projection](session-occurrence.md).
It can be rebuilt from verified exact gzip evidence after raw reclamation, without
changing sealed canonical metadata, original bytes or archive receipts. Archive
backfill/reclamation controls do not disable this bounded metadata correction.

This reference covers post-seal storage and full-session viewing. The sanitized
representation is available by explicit opt-in after lifecycle qualification.
Replacement policy installation and sanitized cleanup default off. The codec
probe and client qualification cover separate scopes. The aggregate qualification
covers database, HTTP and browser integration and rollout.

## Exact representation

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

## Sanitized representation contract

### Replacement policy

Sanitization is opt-in through an operator-installed database policy. The archive
control plane installs one validated policy atomically for all workers. An absent
policy disables sanitization. Reject an invalid installation and keep the prior
policy active. The v1 JSON policy has this shape.

```json
{
  "version": 1,
  "eligible_from": "2026-12-01T00:00:00Z",
  "formats": {
    "codex_jsonl": [
      {
        "match": {
          "/type": "response_item",
          "/payload/type": "function_call"
        },
        "path": "/payload/encrypted_content",
        "replacement": "Encrypted Payload"
      }
    ]
  }
}
```

Use exact, case-sensitive stored format labels and require every discriminator to
equal the configured JSON string. Use RFC 6901 pointers into the decoded object,
including canonical array indices. Require a nonempty rule list per format, a
nonempty discriminator map and non-root pointers. Reject unknown keys, duplicate
keys or decoded paths, invalid escapes and unsupported versions. Bound the policy
to 64 KiB UTF-8, with at most 16 formats, 16 rules per format, eight discriminators
per rule, 16 segments per pointer and 256 UTF-8 bytes per replacement. Omitted
`replacement` defaults to `Encrypted Payload`. An explicitly empty string is valid.

Record the effective cutoff as the later of `eligible_from` and the policy's
database activation time. Each job insertion path snapshots the applicable format's
normalized rules, version, effective cutoff and SHA-256 of the snapshot in the
same transaction as job creation. Use a versioned UTF-8 JSON encoding with sorted
object keys, compact separators, explicit defaults and a canonical UTC timestamp.
Keep the encoded snapshot immutable across retries, including after policy changes
or disablement. Sessions sealed before the cutoff, formats without rules, preexisting
jobs and published archives retain the exact representation. Historical or on-demand
enqueue uses the same cutoff. Keep production eligibility disabled until compatible
consumers and reclamation guards are deployed.

### Transformation and integrity

Verify the original seal while streaming the ordered raw chunks. Parse bounded
UTF-8 JSONL records and replace only the token spans of matched nonempty strings.
Evaluate all rules against the original record. Write replacement tokens with a
deterministic JSON string encoder. Preserve every other byte, including line
endings and a complete final record without a delimiter. Missing, empty or
nonstring targets and unmatched valid records pass through. Fail the attempt on
malformed or incomplete input, duplicate object keys, multiple matches for one
target or exceeded bounds. Limit both input and output records to 1 MiB and JSON
nesting to 64 levels. Check output and staging accounting before bounded writes.
Expose a bounded failure code for invalid or unsupported source records and for
resource exhaustion. Failure publishes no partial archive and keeps all original
chunks. Record the replacement count, including zero, on a successful
`sanitized-v1` generation.
The count describes configured replacements only.

Keep the nullable frozen policy on the job. A sanitized generation references
that job and stores its representation, policy digest, original seal byte count
and SHA-256, separate sanitized content byte count and SHA-256, replacement count,
compressed receipt, and part, checkpoint, block and original append-ledger
bindings. Bind every block row to its generation and uncompressed range. Verify
the stored gzip by bounded readback against these bindings before publishing the
generation. Keep `gzip-v1` and the existing physical parts. Checkpoints and
content reads use sanitized offsets. Authenticate bounded ranges against
generation-bound hashes of the overlapping 1 MiB sanitized blocks before
returning their slices. Keep the existing exact generation routes, manifest and
hash version.

Unqualified HTTP `/evidence` and MCP `session_evidence` continue to return
original bytes in original offsets with the original seal receipt. Selecting
sanitized evidence requires an explicit selection bound to an immutable
generation. Verify and label its content receipt in the manifest, range reads,
browser views and downloads. All pages of a range read use the selected
generation and its coordinates.

Existing original evidence exports, including task-triage ZIP, retain their
original-byte contract and fail if any required original is unavailable. Offer a
separate sanitized export selection or an explicit opt-in. In an opted-in task
ZIP, pin the selected representation of each entry and include its identity,
byte count and SHA-256 in an entry manifest. Validate each entry against its
selected receipt before publishing a local ZIP. Native receipts retain original
coordinates and commitments.

### Original retention and cleanup

Keep original reads, prefix rehash/proofs, occurrence rebuilds and exact sealed
append replay on original bytes while the complete source remains available.
Once deletion starts, original-only reads and proofs return
HTTP 409 `original_evidence_unavailable` and sealed replay returns HTTP 409
`session_state_conflict`. If a request races the first deletion and cannot
finish from a verified complete original, fail with the same unavailability
outcome and discard partial results.

Before the first deletion, independently reapply the frozen policy to the complete
original source and verify the published gzip, sanitized receipt, block hashes,
checkpoints and source bindings. Persist an attestation bound to the generation,
manifest, frozen policy and original seal. Reclamation requires separate sanitized
qualification, explicit enablement, safe runtime role and a valid fenced permit.
Before the first delete batch, verify that the current canonical occurrence
projection was computed from original bytes through the final original offset
with no pending record. Check the derivation version and source boundary in
the first-delete transaction. A complete projection may have no occurrence.
If verification is pending or fails, retain originals and defer deletion.

The first successful delete batch atomically marks the full original unavailable.
Preserve the verified occurrence result. Exclude the session from automatic
original-dependent occurrence repair once its original is unavailable, including
after a later derivation version change. Report original-dependent rebuild as
unavailable and keep the retained result identified by its verified version.
Later batches recheck the attestation and fences, verify surviving rows against
the original ledger and record progress with each bounded delete. The manifest
reports cleanup as pending, in progress or complete. Disabled, paused or failed
cleanup stays pending before deletion and preserves all original chunks. A
batch failure preserves the remaining chunks and retained attestation, applies
a 60-second backoff, and resumes from durable progress. An explicit retry can
clear the backoff. A verified empty original can complete cleanup with zero
deleted bytes and still serve the empty original.

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
Cleanup accepts a known obsolete unpublished generation, locks control, job and
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
compression path never changes task state, the sealed receipt or original
payloads. V051 adds the separately enabled verified reclamation turns described
below to this same worker lifetime.

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

## Maintenance and recovery (V051)

`session_archive_control` keeps operational state separate from immutable
evidence: a durable `(terminated_at,id)` discovery cursor, pause, qualification,
historical/reclaim switches, configured capacity and one generation/token/expiry
permit purpose. Compression and full reclamation verification share that one
durable permit, including owned renewal and confirmed child cleanup. Discovery
inserts at most 32 candidates and advances its cursor in the
same transaction. An exhausted sweep restarts. Historical background discovery
defaults off at the migration's cutover timestamp. A same-workspace explicit
archive request records one `session_archive_requests` row and may schedule an
older session without enabling the historical scan or changing a ready job.

All worker storage transactions lock control before job and generation. There
is one global attempt permit, renewed atomically with the job's 60-second lease.
Expiry detaches one stale generation; subsequent turns drain at most eight
staging rows before any fresh claim, including on-demand and newly sealed work.
Pause prevents new claims and raw deletion while the current bounded attempt
drains through its existing cancellation/renewal lifetime. Resume retains the
cursor, published archives and deletion progress.

Default staging and WAL headroom allowances are 8 GiB and 32 GiB. These are
configured logical capacity policies, not observations of PostgreSQL disk free
space or caps on actual WAL emission. Admission reserves
`raw + ceil(raw/500) + max(1,ceil(raw/1MiB))*65536 + append_count*1024 + 8MiB`
bytes and four times that allowance for WAL, using checked numeric arithmetic.
Actual staging accounting adds part bytes plus 512 bytes per part, dictionary
bytes plus 512 bytes per checkpoint, and 1024 bytes per append-ledger row.
Each bounded insertion checks the configured staging allowance; published data
ceases to be staging. An insufficient allowance produces `resource_blocked`
without classifying the session as unsupported. Operators may raise capacity
for larger supported transcripts. Physical disk, WAL retention, backups and
ordinary foreground database consumption require separate operational
monitoring; a client filesystem cannot establish remote PostgreSQL free space.

Before first deletion, an independent bounded readback verifies the pinned
manifest, exact single-member gzip termination, compressed and original receipt
counts/digests, every checkpoint and append-ledger binding, and original chunk
metadata. `session_archive_reclamation` stores the compact result bound to the
immutable generation/manifest. Failed verification stores no deletion eligibility
and retries after 60 seconds or an explicit retry command. No transcript-sized
buffer or raw temporary file is needed.
Pausing or disabling reclamation after successful verification defers deletion
while preserving the attestation, raw rows and deletion progress. Status remains
`draining` during a pause while its attempt permit remains live.

`public.hmem_archive_reclaim(workspace,session,generation,manifest,token)` locks control,
job and generation in order. It rechecks enablement, qualification, readiness,
verification, current maintenance permit and actual runtime-role safety, then compares each raw row's complete
metadata and SHA-256 with the immutable append ledger. It deletes at most eight
rows/8 MiB and commits progress in the same transaction. Any mismatch rolls back
the entire batch. Empty, repeated and interrupted batches are safe. The original
receipt, ledger, checkpoints and published gzip remain unchanged.

The SECURITY DEFINER capability has a fixed `pg_catalog` search path and qualified
relations, is revoked from PUBLIC and belongs to a protected NOLOGIN/NOINHERIT
owner with no role memberships. Its narrow grants include one UPDATE column only
because PostgreSQL row locking requires that privilege; ordinary UPDATE is still
rejected by the chunk guard. Runtime readiness rejects superusers, CREATEROLE,
public-schema CREATE, evidence/guard ownership and membership of the capability
owner, including authority available through assumable roles. It also rejects
TRIGGER rights on protected write targets and caller-controlled attached trigger
functions in any schema, even after attachment rights have been revoked.
Use a separate migration administrator and explicitly grant the trusted runtime
role EXECUTE on this one capability. Administrators can alter PostgreSQL guards;
they are not ordinary runtime callers. No caller setting, disabled trigger or
general evidence bypass authorizes production deletion.

```text
hmem-ctl archives status --json
hmem-ctl archives capacity --staging-bytes 8589934592 --wal-bytes 34359738368
hmem-ctl archives backfill-enable --qualified
hmem-ctl archives reclaim-enable --qualified
hmem-ctl archives replacement-install --policy-file policy.json --qualified
hmem-ctl archives sanitized-reclaim-enable --qualified
hmem-ctl archives pause
hmem-ctl archives resume
hmem-ctl archives retry --workspace UUID --session UUID
hmem-ctl archives sanitized-reclaim-disable
hmem-ctl archives replacement-disable
hmem-ctl archives reclaim-disable
hmem-ctl archives backfill-disable
```

The qualification switches acknowledge completed compatible-reader, complete-view
and backup gates. V063 leaves sanitized qualification and cleanup off. Install
the replacement policy with a trusted operator that has SELECT, INSERT and
DELETE on the policy table and access to archive controls and status. Installation
atomically replaces the format map. Disablement clears it for future jobs and
preserves every frozen job. Only an explicit `sanitized-reclaim-enable` using
the restricted runtime role can enable sanitized cleanup. It uses the existing
`public.hmem_archive_reclaim(uuid,uuid,uuid,text,uuid)` grant and checks extended
runtime authority over the new evidence and occurrence relations and callbacks.
`sanitized-reclaim-disable` affects sanitized cleanup alone. `reclaim-disable`
stops both representations. These commands preserve published archives and
committed progress. Status includes `sanitized_runtime_safe`, independent
qualification/enablement, and counts of pending, in-progress and complete
sanitized cleanup alongside the shared capacity, permit and failure counters.

Keep a verified pre-cleanup backup. Also back up **after** raw reclamation,
restore into an empty replacement database and verify archive-only ranges,
historical proofs, exact terminal replay and ZIP bytes for exact archives.
For sanitized archives verify the selected content receipt, range and export,
and explicit original unavailability. After the first sanitized deletion a
rollback must use a build with sanitized readers, or restore the verified
pre-cleanup backup to recover the originals. An exact-only reader cannot serve
that sanitized generation or reconstruct the retired original.
Logical DELETE makes space reclaimable. Ordinary VACUUM and later inserts may
reuse heap/TOAST/index pages; reusable space is different from OS file shrink.
Never run VACUUM FULL automatically. Measure raw baseline, ready-plus-raw,
post-delete and post-vacuum/reuse storage with separate non-double-counted heap,
TOAST, index, dictionary and ledger totals, plus actual WAL, staging and backup
sizes before making storage-saving claims.

The implementation owner's disposable qualification area is
`.tmp/sealed-archives-8ff1e216`, transferred to Task 8 after verified handoff.
Required receipts and the active rollback source/binary remain through frontend
rebind and aggregate approval, then Task 8 disposes that scratch. Existing
harness sandboxes separately own their ephemeral PostgreSQL databases and process
handles and clean them through their supported finite release paths. Routine raw
fixtures/logs are not retained. A compact measurement/recovery receipt is required
release evidence. The reproducible backend-only rollback derivative is defined
by [its patch](../../tools/session-archive-rollback-compat.patch) and
[source/build recipe](../../tools/session-archive-rollback-compat.json), owned by
repository maintainers until that target is replaced or retired. A source hash
alone is not qualification: require the matching executable, restored-database
operational probes and actual served frontend asset hashes. Keep the current
frontend, pause/drain maintenance and disable reclamation before switching
backends. Raw-only code still requires the pre-cleanup backup. Unresolved failure
diagnostics expire at issue closure.

### Backend maintenance fixture build

Run the timed maintenance fixture with matching optimized core, server and test
components. When switching a checkout from `--fast`, force one test-enabled
build before timing:

```text
stack test hmem-core:test:hmem-core-test hmem-server:test:hmem-server-test --no-run-tests --force-dirty --reconfigure --ghc-options=-O1
stack test hmem-server:test:hmem-server-test --ghc-options=-O1 --ta=--match=seeded
```

Stack's [`--fast` option](https://docs.haskellstack.org/en/stable/build_command/) adds `-O0`;
the default [`rebuild-ghc-options: false`](https://docs.haskellstack.org/en/stable/configure/yaml/non-project/#rebuild-ghc-options) can leave
those artifacts in use after a later command omits `--fast`. Verify effective
GHC overrides for both libraries and the test component; Cabal's
`NormalOptimisation` value alone does not expose a later `-O0` override. Keep
the test-enabled configuration for subsequent matching checks. The build
precondition does not replace the fixture's unchanged 250-second guard or its
integrity, recovery and resource assertions.

`HMEM_ARCHIVE_QUAL_DIAGNOSTICS=1` emits bounded per-path wall/CPU timings and
manifest counts without retaining raw transcript fixtures or query logs.

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
viewing. A processor Worker stages the complete compressed download in private
OPFS and verifies its generation, count and SHA-256 before reopening it for
single-member native gzip decoding. The browser and processor transfer one
64 KiB buffer back and forth for staging, with an explicit byte count on each
transfer. The processor writes and hashes only that prefix. It streams exact
raw bytes into another OPFS file while hashing and building an IndexedDB
semantic index. Complete records in each 64 KiB slab share a strict UTF-8 decode
and retain individual byte positions. Decoder EOF, JSONL framing and the
original 64-bit count/hash must all pass before
any transcript is published. Transfer, decoding, parsing, integrity and actual
quota failures publish nothing. Accepted download counts and decoding progress
are coalesced to about one update per second; phase entry and terminal events
remain immediate.

One admitted load has sequential processor and reader incarnations. The bridge
holds its per-generation Web Lock continuously from creation through deletion.
Before acknowledging detach, the processor joins its decoder/source pipeline,
every IndexedDB transaction terminal event and actual file-handle closure. The
bridge fences old callbacks and invokes terminate() before constructing the
reader. It first performs a current-authorized conditional content GET with the
stored authority-bound ETag; only the matching 304 permits adoption. This
revalidation cost is included in selection-to-view timing. The reader opens the
existing exact owner/schema/generation/codec/receipts/raw length without recovery,
creation, upgrade or another decode. Raw/index data stay immutable; bounded
disclosure updates remain supported. Public Worker termination provides no
native-thread or RSS-reclamation acknowledgement, so this contract does not
claim physical thread nonoverlap or immediate memory release.

The shared framing/semantic transition preserves adjacent occurrence matching,
activity anchors, disclosure identity and source order. Persist source/block
references and bounded cursor indexes rather than all-session maps or growing
group arrays. Record-boundary pages store one exact start offset and up to 256
packed byte lengths and delimiters, including for hidden records. Semantic
pieces and activity summaries use separate pages of up to 256 entries, encoded
in bounded UTF-8 buffers. Their last source/block key supports exact seeks. An
entry that needs the full 1 MiB budget stays in a single entry. An adjacent
event and canonical record can replace the pending piece, and a carried
activity can update its pending summary before the respective page is written.
Metadata reads seek only the group pages needed for the current pieces.
Index write batches hold at most 256 entries or 1 MiB. Each local acquisition
admits at most 64 rows; ordered overlapping batches compose a continuous window
bounded to 200 rows/4 MiB of retained serialized payload, including hydrated
bodies and Markdown source. A newly selected archive starts at its verified end.
In-flight Markdown source reserves twice its actual serialized UTF-8 size,
including escaped controls, against that same aggregate ceiling.
Scrolling automatically crosses exact source/block index cursors, render slices
and body parts. Refetchable omitted spans keep the full session reachable,
including huge groups and multi-block records. A single item that cannot fit the wire limit gives an
explicit local-view resource error; original downloads remain available.
Exact pretty output is generated lazily in at most 16 Ki UTF-16 units/64 KiB
UTF-8 parts, with lossless tokenization. Complete Markdown blocks retain fence,
reference, list and table context while rendered continuations bound expanded
attributes and DOM content before mounting. One shared rendered window charges
all fragments, wrappers and omitted markers against 1 MiB for normal Markdown
or 512 KiB for structured exact output. Keyed text fragments retain the user's
visible text position through prepending and hydration; continuation has no
presentation or body paging controls.

One explicit presentation exception applies to structured reasoning objects
whose exact pretty-printed JSON exceeds 1,048,576 UTF-8 bytes: show the complete
JSON through exact scroll-driven body continuation, with links and emphasis displayed literally.
Objects at or below that limit, normal text/thinking Markdown, pairing and
activity grouping keep their existing semantics. This exception does not
increase memory budgets or introduce a virtual Markdown parser.

Selection change, cancel, authorization loss and disposal immediately clear owned
transcript and inspector evidence and retire the exact load/incarnation. Stale
messages cannot repopulate it. Close streams, files and IndexedDB before deleting
staging; uncertain cleanup retains actual capabilities and blocks replacement.
Keep only the current private generation. Revisiting a session performs fresh
authorized manifest and content GETs; there is no cross-selection cache. Local
pages use no HTTP. Their verified raw-read cache has exactly two reused 64 KiB
buffers, copies returned bytes, and is invalidated on writes and retirement.
Logout, principal switch and credential/session changes retire ownership even
when workspace/session IDs remain the same.

Worker, gzip DecompressionStream, worker OPFS, IndexedDB and Web Locks are
required. These are same-origin resources: OPFS and Web Locks require a secure
context, and synchronous OPFS handles are dedicated-Worker APIs. See the
[File System specification](https://fs.spec.whatwg.org/) and
[Web Locks interfaces](https://w3c.github.io/web-locks/#navigator-mixins).
Production deployment requires HTTPS and the actual capabilities. The tested
HTTP loopback origin is potentially trustworthy under
[Secure Contexts](https://w3c.github.io/webappsec-secure-contexts/#is-origin-trustworthy);
this does not establish support for remote HTTP origins or cross-origin storage.

A per-local-UUID lock protects each live tab through actual cleanup; there is no
lifetime origin mutex. The versioned `hmem-session-archives-v1/<local-uuid>/owner.json` layout
marks owned generations. Recovery examines at most 128 entries and removes at
most eight marked stale UUIDs per admission, skipping live owners and unknown
entries. Exhausting the eight-stale cleanup budget permits a retry that makes
real progress. More than 128 preserved entries produces terminal
archive_recovery_limit with close-other-tabs/private-storage guidance; live and
unknown entries remain preserved.

The qualified platform is Windows Chromium in a normal persistent disk-backed
profile, with an actual close/reopen storage canary. Other browsers/profile modes
must pass the same integration gates before being advertised. Missing
capabilities produce an explicit unsupported state. Storage estimates are
advisory: low or unavailable estimates do not reject otherwise successful writes.
Actual QuotaExceededError is authoritative and follows owned cleanup. Local disk
grows with compressed bytes, raw bytes and index size; estimates do not promise
that a session will fit.

## Budgets and remaining qualification

These limits apply to implementation qualification. Client measurements below
cover the selected frontend fixture; server and aggregate gates remain separate:

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

### Client qualification scope

On 2026-10-29 the matching frontend candidate passed deterministic valid JSONL
archives at 1 MiB, 100 MiB and 1 GiB on Windows 10.0.26200 x64, Chromium
151.0.7922.34 and Node 24.15.0. The fixture streams 1024-byte UTF-8/CRLF records
with seed 0x51a7 through the controlled 100 Mbit/s loopback HTTP service. Exact
raw/gzip receipts, source/assets, process images and device inputs are retained
in the [bounded qualification record](../../hmem-server/frontend/perf/session-archive.qualification.json).
This is frontend/private-storage and mock HTTP authority qualification, not the
final PostgreSQL/server integration or rollout gate.
The full-view clock starts before selecting an already-ready archive and includes
content preflight, transfer, verification, reader handoff and first-page display.
It excludes seal/compression-to-archive-ready latency. Aggregate qualification
measures that interval and any click-while-preparing total separately.

| Raw size | Gzip bytes | Acknowledgement ms | Verified view ms | Navigation p95 ms | Sampled private increment MiB |
| --- | ---: | ---: | ---: | ---: | ---: |
| 1 MiB | 6,240 | 10.9 | 213.9 | 13.5 | 77.25 |
| 100 MiB | 573,001 | 17.8 | 8,420.8 | 13.4 | 86.50 |
| 1 GiB | 5,861,585 | 8.1 | 94,621.5 | 13.4 | 124.43 |

Each vector starts with a fresh normal profile and an ACTIVE viewer baseline
without an archive Worker or staged generation. The primary metric sums native
private footprints for all five owned browser processes across download,
indexing, processor/reader handoff and 20 local navigation actions plus final
Last. Background memory dumps are causally bound to one successful explicit
global-request span and exact per-process lineage; the CDP request GUID and
legacy export row ID are recorded separately. Ordered page/processor/reader
isolate measurements are secondary diagnostics. No forced GC or observer-cost
subtraction is used. These are sampled maxima, not continuous peaks or RSS:
maximum observation gaps were 397/1,145/1,393 ms respectively. Required disk
counts, file sizes and storage checks run after the final product-memory sample,
with verification/transaction closure and complete fixture cleanup still required
before success. All three vectors satisfied the original 128 MiB, acknowledgement,
navigation and full-view limits at this declared scope.

The retained matched active/live comparison used five warmups and 40 measured
selections and live updates per bundle, alternating historical b742/current
bundles. Selection and update p95 ratios were 1.018 and 1.019, below 1.10. Later
changes are archive-only and reuse that result by unchanged executed-path
validation; its original asset binding remains explicit. Earlier memory and
setup failures remain immutable in the current record's fixed encoded family
and [historical record](../../hmem-server/frontend/perf/session-archive.historical.json);
passing vectors do not relabel them.

An inherited elm-markdown 7.0.1 limitation remains: 1000 adjacent reference links
in one paragraph with an approximately 8 KiB destination overflow InlineParser's
stack in both current ACTIVE and historical b742 ACTIVE rendering. The failing
raw fixture SHA-256 is 4c6584a5f5063b5a4393f37b95ac2ce949d311a16f03e7af68559095f35d32e9.
It is not a passing archive gate. The supported continuation case uses 1000
separate paragraphs, verifies every exact destination and bounds more than 8 MiB
of expanded href content within the mounted ceiling. No parser fork or silent
plain-text fallback is used.

The two qualification files are limited to 1 MiB each/2 MiB total. Task 6 owns
them and its contained scratch through verified handoff, then Task 8/repository
maintainers retain required input and benchmark evidence until an approved
successor preserves or supersedes those facts. Raw fixtures/traces and routine
logs are not retained. The final task must reconcile served frontend/backend
bindings and integrated acceptance before enabling backfill or reclamation.

### Integrated database, HTTP and client qualification

The [integrated qualification record](../../tools/session-archive.integration-qualification.json)
retains actual 1 MiB, 100 MiB and 1 GiB isolated PostgreSQL runs on the same
Windows/persistent-browser target. The 100 MiB and 1 GiB cases complete real
publication, two independent full client transfers and verified views, raw
reclamation, 20 archived ranges, eight advancing evidence windows, first/half/full
prefix proofs, exact append replay and the entire streamed ZIP including raw
SHA-256 and CRC32. The 1 GiB case reads all 1,048,576 records; its full proof and
ZIP results are executed measurements. No large-path result is extrapolated.
Browser-on-restored-derivative and private-pipe handshake qualification are
explicitly deferred/unqualified after user-approved scope assessment and
independent review. The failed borrower produced no passing browser receipt;
its exact failed reply stage remains unknown. Its code was removed by an exact
inverse to the measured sources. The unchanged rollback HTTP/static selector
then completed with one unique fresh intended-case receipt; the Stack tool
omitted its numeric Hspec total. Independent aggregate review remains pending.
Follow-up `0c439241-64ce-4f39-8722-df9f5c68caed`, **Qualify restored prior-backend
browser orchestration**, owns the deferred orchestration qualification; it does
not change the data, security, resource or product limits above.

| Raw size | Actual gzip bytes | Seal to ready ms | Preparing selection to view ms | Fresh ready selection to view ms | Foreground p95 ratio |
| --- | ---: | ---: | ---: | ---: | ---: |
| 1 MiB | 6,236 | 2,327.0 | 2,642.6 | 435.3 | Inconclusive |
| 100 MiB | 575,969 | 7,202.8 | 18,179.6 | 11,085.5 | 0.881 |
| 1 GiB | 5,894,432 | 37,719.6 | 156,289.0 | 121,607.1 | 0.704 |

These are observed unadjusted qualification clocks. Seal-to-ready includes a
deliberate worker pause, raw baseline where enabled, idle sampling and foreground
baseline. Preparing-to-view includes the remaining paused work; both client view
clocks include deliberate compressed-EOF holds. No intervals are subtracted.
The GiB first observed-ready-to-verified-view clock was 124,420.0 ms; it and the
fresh ready-view clock independently satisfy the unchanged 125,943.1 ms product
formula. First/Last automation wall times are content checks, separate from
Task 6's client-side navigation p95 qualification.

Matching ordinary foreground phases use five warmups then 40 sequential
`GET /api/v1/workspaces` requests with 20 ms post-response pacing. The real
browser's passive readiness responses are observed in both phases; no manual
manifest polling is added to the timed load. All request start/header/body/end
times and identities are retained. Narrow private status requests outside the
timed phases calibrate the PostgreSQL clock. The union of caller clock intervals
is widened by 1 ms on each side; width above 10 ms or any load interval outside
the conservative persisted draft-created-to-published bracket is inconclusive.
This bracket measures processing/publication, not uninterrupted compression CPU.
Both larger cases have 40/40 coverage and satisfy the unchanged 1.10 p95 bound.
The tiny case published before admission and remains performance-inconclusive.

The largest sampled incremental current application working set was 15,847,424
bytes in the GiB run, below 128 MiB over the matching baseline. The harness
samples its current process with `WorkingSetSize` and `GetProcessTimes` at a
requested 20 ms cadence; actual counts and maximum gaps are retained by phase.
PostgreSQL buffers/processes are separate observations. This is no continuous
peak, browser RSS or whole-host bound. Deleted raw rows do not imply immediate
physical relation shrinkage; allocated pages and WAL are reported separately.

Earlier failed protocols remain in the record, including the 1.196 foreground
ratio from an unmatched observer schedule. The original 100 MiB cleanup receipt
also remains intact: its `recovered=true` label came from interpreting a
maintenance count as a recovery boolean. Actual normal native exit, pipe joins
and sandbox removal had independently passed. Command-correlated recovery
classification was repaired, causally checked and correctly reports false in the
GiB run. No repeated vector was run solely to change that historical label.

Restore evidence is composed at explicit scopes: a current-source entropy 2 MiB
strict dump into a template0 empty replacement checks archive-only full reads;
earlier GiB restore evidence retains its different V051/qualifier provenance.
The integrated GiB proof/ZIP/content result above closes those separate large
read gaps. The unchanged reviewed rollback derivative passed fresh strict empty
restore, archive-only HTTP fidelity and actual final served frontend hashes.
Its fresh receipt verifies the required six assets plus unchanged icon/logo SVGs.
Both native handles and dump/restore helpers joined, followed by strict borrowed
PostgreSQL and root cleanup before receipt creation. This HTTP check does not
replace the deferred restored-browser result. The removed borrower incorrectly
required exactly six entries while the selector supplies all eight; this is a
source-supported explanation, while its missing runtime error stage remains
unknown. Backfill and raw reclamation remain disabled by
default; qualification does not enable or deploy them.

This additional aggregate is limited to 2 MiB and is retained by repository
maintainers until replaced or feature retirement. Task 8 used one contained
scratch root with a configured 8 GiB admission ceiling and finite cleanup.
The exact Task 6, 7 and 8 project scratch roots were removed after required
receipts and scope decisions were retained: 711,909,718 bytes in total.

Cleanup remains partial. The stopped external fixture
`C:/Users/Sornaensis/AppData/Local/Temp/hmem-sandbox-81555910deb94fde`
is retained as cleanup debt, not release evidence: 87,573,976 bytes, 2,026 files
and 40 directories. Recorded exact native and PostgreSQL exits and scoped
closure checks establish no remaining live dependency; this is not a new global
process-absence claim. Automatic approval review rejected its deletion because
the human approval covered process shutdown, not irreversible external file
deletion. No retry or bypass occurred.

The canonical hmem maintainer/orchestrator owns this bounded custody under TODO
`a0bf4079-6fba-4442-855b-7f3e7e303492`, **Resolve stopped archive fixture file
custody**. No further writes, growth, runtime reuse, move, prune or deletion are
authorized. Review by **2026-11-05**; this is neither an automation nor an
automatic deletion date. Removal requires direct human file-deletion approval.
The deferral changes none of the integrity, security, resource or product limits.

### Sanitized lifecycle qualification

The [sanitized qualification record](../../tools/sanitized-session-archive.qualification.json)
binds the isolated PostgreSQL, native server, production frontend and Chromium
results to their executable and source inputs. It records sampled server working
set and all owned Chromium private memory over matched baselines. These loopback
runs exercise the full-view deadline and verified local navigation. The earlier
controlled 100 Mbit/s, loading acknowledgement and 20-action navigation p95
checks have a separate scope.

The 1 MiB and 100 MiB cases verified the browser view before reclamation and a
fresh view and sanitized ZIP after reclamation. The 1 MiB case also verified
the browser JSONL download. Their sampled browser and server increments stayed
within 128 MiB. The 100 MiB run met the foreground p95 limit with full overlap. The
1 MiB publication finished before the required foreground overlap.

The 1 GiB case verified the first full browser view and an independent HTTP
content receipt. Chromium used 159,666,176 additional bytes at the first
worker index and handoff peak. Sanitized reclamation did not complete within
the 90-second test gate. A repeat completed reclamation and verified the ZIP.
Its first view exceeded the memory limit during indexing, before the processor
reported ready. Its fresh view timed out after a sampled increment of
178,282,496 bytes. Keep replacement policy installation and sanitized
reclamation disabled for rollout until the resource and backup gates pass.

The 1 MiB storage fixture uses 256 distinct deterministic base64 payloads and
768 unchanged message rows. The 100 MiB and 1 GiB stress fixtures repeat an opaque
payload at the same density. Deleting the original rows removes their logical
bytes and stored values. In the 1 MiB fixture the allocated `session_chunks`
relation stayed at 409,600 bytes and the allocated database grew by 16,384 bytes
between ready publication and completed cleanup. These selected measurements do
not estimate archive savings for other data.

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

