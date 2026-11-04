# Persisted document previews

The server converts immutable originals through the separately qualified
[confined worker](document-preview-worker.md). Originals remain downloadable
when rendering is unavailable, unsupported, over budget or fails.

## Configuration

The optional top-level JSON object `document_previews` defaults to disabled:

```json
{
  "document_previews": {
    "enabled": true,
    "python_executable": "python",
    "launcher_path": "/opt/hmem/hmem-preview-worker/launch.py",
    "image_id": "sha256:43d5c02754efce8f533ded982f75b1a0f9eca8202c37d186cf1a1b575da890b5",
    "workers": 2,
    "queue_limit": 32,
    "max_attempts": 3,
    "image_seconds": 30,
    "pdf_seconds": 120
  }
}
```

The configured launcher is trusted host code; the immutable image must already
exist locally. The launcher never pulls an image. Missing executable, launcher
or confinement support yields preview unavailability. There is no server-native
decoding fallback and no Docker socket mount into an application container.
Run the launcher on a host with the required restricted runtime.

Limits may only tighten the qualified defaults: 25 MiB original, 40 million
decoded image pixels across frames, 200 PDF pages, 16 million pixels per page,
4096-pixel side, 2 GiB cumulative decoded PDF bytes, 100 MiB output, 5 MiB HTML,
20,000 DOM nodes, depth 64, 2,000 CSS rules and 100 embedded images. The optional
`limits` object uses the worker's snake_case keys; omitted values use defaults.
Concurrency may be 1–2, queue capacity 1–32, attempts 1–3, and total deadlines 6–30 s
for images/HTML or 6–120 s for PDF. Five seconds of each deadline are reserved for
physical cleanup. Inspection, original reading, quota reservation, rendering,
native exit and cleanup all consume this same absolute generation deadline.

## Scheduling and publication

V060 stores one job per document and one immutable owner UUID per launch. New
original publication enqueues in the original transaction before the role link
is inserted; repeated associations reuse that job. Unsupported formats retain
their original with no decoder work. A global runtime row serializes admission
across server processes: two active workers and 32 total queued, active or
unresolved owners by default. Queue saturation fails only the preview.
After the shared availability heartbeat expires, a controller without a launcher
settles leftover queued work as retryable unavailable. It preserves queued work
while another healthy controller is heartbeating, and never releases an active
or uncertain physical owner on that basis.

Lock order is workspace document budget, global runtime, job/run, then preview.
Inspection validates output shape and binds an opaque plan SHA-256 to original,
policy, image and complete limits. The server reserves a conservative 101 MiB
compressed-generation budget before acknowledging rendering. It validates all
framing, descriptor identities, sizes, hashes, bounded work receipts, EOF and
native exit before publication. Image output is page 0; PDF pages and page
thumbnails use 1-based numbering; the document thumbnail is page 0.

All derivative payloads, immutable generation metadata, the current preview,
job completion and quota reconciliation commit in one transaction. Current
owner/lease/attempt checks fence both delayed success and delayed failure.
A final absolute-deadline check rolls back slow publication and event fanout.
Partial output never becomes a ready generation.

## Restart and cleanup

The controller persists owner identity before spawning and retains the actual
native process, sole reaper and pipe consumers.
Windows launchers retain process-job ownership. The native PID is snapshotted
from the already-owned open process handle before its reaper; it is diagnostic
identity and never authorizes later PID-based signalling. The qualified launcher verifies
and removes its exact image/owner-labelled inspection and render containers.
Killing a Docker CLI, signalling a PID, an expired lease, or observing that a
container is currently absent cannot establish physical quiescence after an
uncertain daemon create.

A lost owner without verified native/container closure becomes
`cleanup_unverified`. Its physical admission remains held and replacement work
fails closed. The existing payload reservation can expire under storage's
one-hour staging policy; releasing that byte reservation never releases physical
ownership or authorizes late publication. Operators must reconcile the exact
durable owner and actual producer before recovery; lease or reservation expiry
alone never releases physical admission.
Runs with verified closure can recover automatically with bounded retries.
Shutdown first closes admission and serializes against publication, then drains
owned work within its original deadline. A failed join is reported as unresolved,
not successful cleanup.

## HTTP and metadata

Authorized `POST .../documents/{id}/previews/retry` uses ordinary workspace
edit and cookie-CSRF rules. It returns 202 with `{"accepted":true}` when an
eligible failed job is admitted; 200/false preserves ready, queued, active,
unsupported or quarantined work. It neither starts the task nor changes its
status. Eligible failures can be retried after capacity/runtime recovery.

Current-preview metadata adds `retryable` and `generation_metadata`.
Nonready metadata is null. A legacy generation without a verified receipt is
`{"verification":"unknown"}`; consumers must not infer animation=false
from unknown metadata. Verified metadata contains verification, plan_sha256,
original_sha256, limits_sha256, policy_version, image_id, output_count,
page_count, decoded_pixels, pdf_decoded_bytes and animated_first_frame. It is
immutable and has no native PID, owner controls, local path or bearer token.

Raster routes remain generation-pinned and read-authorized. Styled HTML is
served only through the existing [cookieless capability origin](document-capabilities.md),
with response CSP sandboxing, no scripts or network access. Rendering never
creates a second grant authority or exposes original HTML as an application page.
