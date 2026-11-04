# Confined document preview worker

`hmem-preview-worker/` owns decoding and static sanitization only. PostgreSQL job
leases, quota reservation/publication, HTTP authorization, capability grants and
the dedicated HTML preview origin belong to the server integration. Worker output
is not an authorization grant or a document/session seal receipt.

## Deployment

Build the package Dockerfile, then configure the host launcher with the returned
immutable image ID. The base is pinned by registry digest; `requirements.lock`
pins wheel hashes for CPython 3.13 Linux x86_64 and the CPython 3.14 Windows test
environment. `tools/lock_dependencies.py` refreshes hashes for its explicit pinned
releases from PyPI. Review dependency/version changes and rerun container tests.
`image-reference.json` records the qualified local immutable image and exact
Dockerfile, lock and copied-module input hashes. It is excluded from image inputs.
The worker/integration coordinator owns this runtime pin: retain it while
configured, replace it only after a rebuilt image is verified and qualified, and
retire obsolete pins when unused. A local image ID is not a registry publication
or a promise that another host already has it; missing images fail unavailable
with runtime pulling disabled.

The launcher requires a separately supervised Docker CLI on a trusted host. It
creates fresh containers for **both** inspection and rendering. It passes no host
mounts, application credentials or caller environment into them. It enforces a
non-root UID, no network, read-only root, all capabilities dropped,
no-new-privileges, seccomp, 512 MiB memory with no swap, 64 PIDs, one CPU and a
64 MiB noexec/nosuid/nodev tmpfs. Container logs are disabled. The worker checks
the effective kernel controls and requires an empty PDFium feature-flags tuple
(no V8/XFA) before reading original bytes. The launcher verifies the container's
immutable image, ownership label and runtime configuration before starting it.

This is not permission to mount a Docker socket into the application container.
Deployments without a supported supervised launcher report preview unavailable;
there is no native decoder fallback. Docker daemon access is privileged host
authority, held outside the untrusted document container. Decoder bugs remain
possible; confinement and resource bounds limit their reach.

## Protocol

The worker consumes one request and EOF: a four-byte unsigned big-endian JSON
header length (at most 64 KiB), UTF-8 JSON, then the exact `original_bytes` bytes.
Duplicate JSON keys, extra fields, trailing bytes, bad size/hash and unknown
policy/operation are rejected. Requests contain `version:1`, `operation` (`inspect`
or `render`), `media_type`, `original_bytes`, `original_sha256`, `policy_version`,
`limits`, and a `plan` for render. There are no URLs or filesystem paths.

Inspect reads image dimensions, bounded MP4 track metadata or PDF page
geometry without producing raster derivatives. HTML inspection checks finite
UTF-8/input length only. Plans bind
the original hash, media type, policy version, complete limits hash, expected
output shape and maximum publication budget. Render recomputes and compares the
plan. Animation/frame and embedded-raster decoding happens only during render,
under one shared 40 MP budget; PDF raster work is cumulatively bounded at 2 GiB.

Responses start with the same four-byte header framing (maximum 256 KiB).
Successful render manifests are followed by each output's eight-byte big-endian
length and exact bytes, then EOF. The host verifies the complete shape, every
count, length and SHA-256, PNG signatures, total budget and final EOF before
returning any outputs for publication:

| Original | Required complete shape |
| --- | --- |
| PNG/JPEG/WebP/MP4 | `image` page 0 and `thumbnail` page 0, static PNG |
| PDF | `pdf_page` and `pdf_thumbnail` for every page 1..N, plus `thumbnail` page 0, all PNG |
| HTML | One `styled_html_preview` page 0, UTF-8 HTML |

There are at most 401 outputs and 100 MiB total output. Stderr is capped at 16 KiB.
The generation succeeds only after verification **and confirmed container
removal** finish within its deadline. Error responses contain fixed codes, never
parser exceptions, original markup, credentials or local paths.

## Admission and lifecycle

The Python `Generation` context holds a single monotonic deadline from inspection
through caller admission wait, render, native joins and container cleanup:
30 seconds for images/MP4/HTML or 120 seconds for PDF. Its `inspect()` returns the
plan; the caller reserves the complete generation count/budget before calling
`render()`. At most two generation contexts are admitted per launcher process;
the server's persisted bounded queue is separate.

For a server using a separate launcher process:

```text
python hmem-preview-worker/launch.py --image sha256:IMAGE_ID --owner-id UUID
```

Send an inspect request and original bytes without closing stdin. Read its framed
response, reserve the database generation, then send a framed JSON acknowledgment
`{"render_plan_sha256":"COPY_OF_INSPECT_RESPONSE_PLAN_SHA256"}` and close stdin.
Read the complete render response and EOF and require launcher exit 0. Copy the
verified opaque `plan_sha256`; callers need not reserialize floating-point page
geometry. Canonical JSON sorts keys, uses compact
separators, ASCII escaping and rejects non-finite numbers. The launcher retains
the same deadline across this exchange; its receipt includes total elapsed and
remaining milliseconds. Held stdin, missing admission and blocked stdout fail
within the deadline. Windows blocked writes cancel the exact writer's synchronous
IO and join it; Unix pipes use nonblocking deadline IO.

The server must allocate and retain the owner UUID **before** starting the native
launcher, and keep its process handle through exit. Container names are
`hmem-preview-UUID-inspect` and `hmem-preview-UUID-render`. On launcher loss, run
the same command with `--cleanup`; it verifies the exact label/image before
removing anything. Normal cleanup has a shared five-second Docker-control budget
plus finite native joins. Unknown creation completion or an unjoined process
retains ownership and poisons admission; current absence cannot prove that a
timed-out daemon create will not complete later. Report this state and reconcile
the retained owner rather than advertising successful cleanup.

No worker files are extracted onto the host. The normal protocol uses bounded
pipes; the container's tmpfs disappears with confirmed removal. Test/build work
uses one coordinator-assigned `.tmp/<task>/` directory, removed after owned
processes exit. Keep only bounded diagnostics for unresolved failures.

## MP4 poster policy

The worker accepts self-contained MP4 with major brand `isom`, `iso2`, `mp41`,
`mp42` or `avc1` and a single H.264 video track. The hash-pinned PyAV wheel
supplies FFmpeg. The MOV demuxer reads only in-memory original bytes. External
data references and secondary I/O fail closed, and the video decoder never
decodes audio. Stream admission, probing, packets, decoding threads, input
dimensions and frame allocation are bounded. The maximum decoded dimensions
are 4096 per side and 8,294,400 total pixels. The first decoded frame becomes
the PNG preview and thumbnail. The original download remains available when
rendering fails.

## Static HTML policy

Selectolax/Lexbor parses HTML5; tinycss2 parses CSS tokens/declarations and
cssselect2 validates the supported selectors. Output is rebuilt from explicit
static element/attribute allowlists, not serialized original markup. Native
controls become inert visual elements; scripts, SVG/MathML, navigation, active
embeds, events and external resources are removed. Unsupported images become
placeholders. Embedded PNG/JPEG/WebP is type-checked, cumulatively decoded and
re-encoded as static PNG. Animated images receive a first-frame label and work
metadata, including embedded animations.

Typed style rules cover bounded typography, colors, spacing, borders, dimensions,
flex/grid and static/relative/absolute positioning. Unknown/vendor properties,
loading URLs/functions, fonts/imports, animation, transitions, filters and shadows
are rejected. Custom variables are bounded, cycle-checked and substituted only
into accepted typed values; unsupported cascade indirection is dropped. Selectors,
expanded grid tracks and numeric dimensions are bounded. Sanitized HTML still
requires the dedicated cookieless origin, response CSP sandbox and preview-only
grant in the server; this package does not serve HTML on any origin.

## Validation

Create an isolated test environment under the task's scratch directory and install
`requirements.lock` with `pip --require-hashes`. Run:

```text
python -m unittest discover -s hmem-preview-worker/tests -v
docker build --iidfile .tmp/TASK/image.id -t hmem-preview-worker hmem-preview-worker
python hmem-preview-worker/tests/container_acceptance.py --image-file .tmp/TASK/image.id -v
```

The second test command exercises the actual immutable image and native Docker
lifecycle, including malformed/active inputs, all supported render formats,
kernel isolation, host credential non-forwarding, deadlines, blocked IO,
resource rejection and confirmed cleanup. Native policy tests alone do not
qualify confinement. Full server persistence/grant/origin/UI acceptance is a
separate integration gate.

Primary contracts: [Pillow security](https://pillow.readthedocs.io/en/stable/handbook/security.html),
[PDFium Python API](https://pypdfium2.readthedocs.io/en/stable/python_api.html),
[Lexbor HTML parser](https://selectolax.readthedocs.io/en/latest/lexbor.html),
[tinycss2 AST](https://doc.courtbouillon.org/tinycss2/latest/api_reference.html),
[PyAV file-like input](https://pyav.basswood.io/docs/stable/api/_globals.html),
[FFmpeg MP4 demuxer controls](https://www.ffmpeg.org/ffmpeg-formats.html#mov_002fmp4_002f3gp),
[Docker runtime controls](https://docs.docker.com/reference/cli/docker/container/run/).
