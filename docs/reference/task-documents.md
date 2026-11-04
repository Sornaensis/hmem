# Task document persistence

Documents are separate from Observations and agent sessions. Originals and
preview derivatives live in PostgreSQL as immutable `gzip-v1` manifests and
ordered `BYTEA` parts of at most 1 MiB. Each manifest fixes raw/compressed byte
counts and lowercase SHA-256 hashes. The codec checks single-member framing,
complete input, bounded expanded bytes and both hashes before returning bytes.
Original reads reproduce exact uploaded bytes, including line endings.

## Upload and retry contract

`HMem.DB.Document.reserveUpload` validates task/workspace visibility and reserves
capacity before an authorized HTTP handler consumes input. The request UUID
binds the workspace/task, Source or Evidence role and complete original manifest:
filename, title, media type, byte count and hash. Existing exact published
requests return their original document; changed reuse fails. Removing a Source
does not erase its retry receipt or recreate the removed link on retry.

The HTTP/MCP owner must authorize before reservation/body processing, enforce
browser CSRF where applicable, bound the finite whole file and verify its source
boundary. `publishUpload` validates original format/signature/identity,
compresses and verifies it, then stores all parts and publishes the payload,
document, chosen link, immutable retry receipt and metadata event in one
transaction. Interrupted or rejected publication exposes none of those rows.
V1 does not offer resumable partial uploads. A durable reservation contains
metadata and budget only; file/compression buffers remain bounded in the owning
process. Explicit abort or expiry releases an unused reservation.

Within a workspace, identical original payload bytes share storage; each upload
retains a distinct document, uploader and creation time. Payloads never dedup
across workspaces. Source and Evidence are separate associations. Evidence fixes
the original metadata snapshot and is additive/immutable. Source removal affects
neither evidence nor published bytes. Task status does not gate documents;
completed/cancelled tasks remain eligible while nondeleted and authorized.

Original admission allows PNG, JPEG, WebP, MP4, PDF, HTML, SVG, UTF-8 TXT/Markdown,
DOCX, PPTX, XLSX, FIG and SKETCH. Media type, extension and applicable signatures
must agree. Office/design archives are not extracted. Acceptance as an original
does not authorize embedding or execution; the renderer owns safe derivatives.

## Accounting and operator limits

`document_budgets` contains one configurable row per workspace, created on first
reservation. An administrator can update its limits in a transaction; reductions
do not delete retained data and can refuse subsequent work. Defaults are 25 MiB
per original, 1 GiB distinct raw originals, 4 GiB compressed binary payloads plus
reservations, 256 MiB staging reservations, and 1000 Source/Evidence associations
per task. Original and link limits may be lowered within their hard maxima.
The binary cap accounts published original/derivative payloads and reserved
work; it does not cap PostgreSQL heap/WAL/index/backup overhead.

All reserve/publish/abort/expiry paths lock the workspace budget row. Concurrent
work cannot spend the same capacity. Uploads reserve the gzip worst-case bound;
raw capacity is reserved only if no identical published original exists. Actual
publication consumes the reservation and accounts actual shared bytes. An
unpublished reservation expires after 1 hour; `expireReservations` removes only such
expired metadata. No automatic deletion of published documents occurs.

## Preview storage boundary

`reservePreview` reserves a complete generation before rendering, bound to one
document/original hash, policy version, expected output count and byte budget.
`publishPreview` verifies every output's gzip hashes and total limits, persists
all immutable descriptors/parts, then atomically marks the generation ready.
Missing/duplicate outputs, a different policy/original or quota failure rolls
back the entire generation. Preview state is independent of original durability;
failed/unsupported preview leaves original download available.

The mutable preview row carries an active attempt UUID. A newer reservation
supersedes the older attempt; publication/failure checks that exact active
attempt and state. Retrying an older reservation cannot reclaim ownership or
overwrite a newer ready generation. `failPreview` accepts a nullable expected
attempt: null applies only to unclaimed queued admission failure, while a worker
can fail only its own processing attempt. Superseded work must abort its unused
reservation or let the 1 hour expiry release it.

Immutable `document_preview_attempts` retain each UUID's original binding after
its reservation is aborted or expired. An old attempt cannot become new work by
retrying after budget cleanup. A retry of expired/superseded work needs a fresh
UUID. Published generations have a same-workspace foreign key to their attempt.

This storage API is for a trusted isolated renderer, not arbitrary uploaded
preview bytes. The renderer task owns parsing/sanitization, image rewriting,
page completeness, worker admission/leases/retry, resource confinement and
dedicated-origin delivery. The stored descriptor contains variant, page, media
type, payload ID and manifest. Generations retain original hash and policy
version. No unsafe original is made safe merely by gzip/hash verification.

## Metadata reads and events

`listDocuments` uses descending creation-time/UUID keysets with at most 50 links
per page. `getTaskDocumentCounts` uses one SQL statement/snapshot to count Source
links, distinct Document evidence associations and distinct linked terminated
session IDs for the task, plus the evidence sum. It excludes descendants, active
sessions and Sources-only documents from Evidence. Counts are independent of
payload dedup and loaded-page lengths; a hidden task returns no count summary.

Document link audit and workspace-change events contain only allowlisted
metadata. Payload parts, local paths, credentials and capability URLs never
enter those events or existing snapshots. Preview status changes invalidate
linked task document resources. Existing session audit/download formats are
unchanged. Attribution comes from the effective request context, never client
actor fields or bot identity as authorization.

## Durability and scratch

Apply V058 with the normal ordered migration runner. Include all document tables
and binary parts in PostgreSQL backups; restoring only metadata cannot recover
originals. A database/service restart uses the same committed manifests, links,
receipts and reservations. Unpublished transaction data is rolled back; expired
reservations can be reconciled after restart. Keep existing backup/restore
procedures and verify byte count/hash after recovery.

Storage functions create no filesystem scratch. HTTP and renderer owners must
use one task/operation-owned contained scratch area with a size bound and finite
cleanup after their processes exit. Test scratch follows the existing isolated
test harness; routine successful logs/fixture copies are not retained.
