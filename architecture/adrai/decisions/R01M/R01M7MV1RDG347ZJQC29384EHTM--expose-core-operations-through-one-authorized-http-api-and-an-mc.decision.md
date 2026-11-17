+++
schema = "adrai/decision/v1"
adr = "A01M39QPB23CYS9QHCWQSATSJE9"
record = "R01M7MV1RDG347ZJQC29384EHTM"
title = "Expose core operations through one authorized HTTP API and an MCP adapter"
summary = "Keep MCP as a bounded stdio-to-HTTP client, order workspace-context changes after previously accepted calls, and admit HTTP requests through one bounded fair queue."
domains = ["api-boundary"]
+++

**Context**

Browser and LLM clients access the same workspaces, Observations, Projects, and Tasks. MCP requests can be processed concurrently, while their active workspace and declared bot identity can change. This amendment records startup, attribution, and ordering rules for that context without changing the server's authority. Hundreds of concurrent HTTP requests also share a small database pool; without request-level admission each checkout queues separately and long holds can occupy every connection.

**Decision**

The Haskell server defines the `/api/v1` HTTP contract and handles authorization and persistence for core operations. The MCP process accepts line-delimited JSON-RPC tool calls over stdio and maps them to server HTTP requests, forwarding a configured bearer credential when present. After initialization, require `set_workspace` with both `workspace_id` (UUID or explicit null) and `identity` containing exactly the validated `role` and `model-id` strings before any ordinary HTTP-forwarding tool, including global workspace tools and calls with an explicit workspace ID. A null workspace permits declaration before workspace discovery or registration. Validate the whole declaration before atomically replacing the complete workspace/identity context; invalid declarations change neither value. Setting a null workspace clears only the workspace and accepts the supplied identity. Repeating `initialize` preserves both values, and process exit forgets them. Protocol/tool discovery and local context controls remain available before declaration.

Capture the complete workspace/identity context when an ordinary request is accepted, carry that snapshot through a bounded queue and fixed worker pool, and inject the workspace ID when the call does not supply one explicitly. Both HTTP forwarding paths send one `X-HMem-Bot-Identity` header with the accepted declaration. Existing bearer credentials and authenticated MCP provenance forwarding remain unchanged. Declaration metadata is self-reported attribution, not authentication or authorization: tokenless local bootstrap remains supported, and role/model values grant no permissions. The HTTP server validates declarations and resolves effective actor IDs; MCP context responses report accepted metadata without claiming a server-resolved actor ID.

Treat a workspace/identity context control as an epoch boundary. Before applying that control, drain previously accepted ordinary requests from the queue and active workers. Ordinary requests within an epoch may execute concurrently. This prevents their externally visible effects from overtaking a later context switch while each request keeps its acceptance-time workspace/identity snapshot. A full queue returns an overload error.

Explicit local uploader registration is an ordinary admitted request that creates process-owned background work with immutable captured workspace, declared identity, credentials and forwarding configuration. After registration, follower jobs run outside the ordinary request queue, active-request accounting and epoch drains. Later context controls neither retarget nor await them. Keep a bounded registry, fixed fair workers, a shared global bound across every follower HTTP request, finite request/control/retry deadlines and finite shutdown cleanup. Ordinary MCP calls, including the separate explicit session termination operation, remain independent of follower HTTP slots.

Followers use the authorized HTTP API for metadata, prefix verification, append and heartbeat. Read only explicitly supplied regular files through stable opened-handle identity and compare the current path's opened identity. Verify the complete committed prefix before resume and continue bounded verification sweeps while following. Replacement, truncation and observed committed-prefix mismatch stop the job without skipping or overwriting server bytes. Preserve raw UTF-8/JSONL bytes and CRLF; hold partial tails. Flush and stop capture one finite source boundary and return metadata-only receipts after complete verification, or explicit pending status while bounded work continues. A receipt certifies the canonical committed prefix and observed source verification; it cannot prove absence of arbitrary external mutate-and-restore writes between observations.

Uploader start, caught-up state, EOF, flush, stop, failures and process shutdown never themselves terminate a session. Explicit management or an independently owned server cleanup policy may seal the verified already committed prefix. The cleanup policy applies only after more than one hour without canonical last_seen_at activity and when every explicitly associated task is done or cancelled. Zero associations qualify; unfinished soft-deleted linked tasks still block closure. Hash the immutable committed prefix outside an exclusive session lock, then atomically recheck activity, cursor and terminal task statuses before sealing. The policy never uploads or rewrites bytes or changes task status. A follower observing terminated state retires safely without later append or heartbeat mutation. Process loss leaves a session active until explicit resume or termination, or until the independent cleanup policy applies. Close terminal file handles, bound retained terminal receipts and cancel/join owned workers within a finite shutdown budget; report cleanup failure truthfully.

Useful bounded follower work is immediately eligible for the next fair turn; idle observation and retry backoff have explicit later deadlines. Admit at most twelve followers and two shared follower HTTP requests. Reuse at most two isolated helper transports and their HTTP connections, retaining finite per-request deadlines, cancellation, restart and native child/pipe ownership through confirmed shutdown. A timed-out or cancelled exchange retires its helper before the slot is reusable; failed joins poison admission rather than releasing ownership. Ordinary MCP requests and workspace controls never depend on follower transport leases.

Support additive asynchronous prefix verification through authorized submission and result reads. Bind each operation UUID to its exact workspace, session, authenticated audience, canonical append-boundary byte length, revision and lowercase SHA-256. Accept only the empty zero/zero boundary or an exact current or historical committed append boundary. Hash immutable committed bytes independently in bounded ordered windows, without holding a session row lock or a pool connection between turns. Every response includes the complete target binding and requires current workspace authorization. Exact tuple retries coalesce; conflicting UUID reuse fails. Limit the managed service to two fair workers, four live jobs and sixteen total retained entries. Fair requeue capacity covers all admitted jobs. Live work has a five-minute deadline. Terminal results have a maximum five-minute cache lifetime and may be evicted oldest-first on admission; never evict live work. Missing, expired, evicted or restarted operations are unknown and cannot certify verification.

The uploader hashes the complete relevant local prefix through its stable opened source handle, binds its proof state to that source generation and verifies the returned target before accepting proof. A completed historical sweep remains useful after append-only growth, but resumed append and finite receipts recheck the latest canonical cursor and verify the exact relevant target. Replacement, truncation, observed source/digest mismatch and authorization loss fail closed. Revalidation after lost operation state uses a new UUID and renewed source validation. Raw server-prefix transfer is optional; provisional append and weaker receipts are forbidden. Deploy the compatible server endpoint before a dependent uploader. Preserve existing append, replay, receipt, raw-byte and independent-sealing contracts.

Committed bytes at an exact append boundary are immutable, so a verified result for that boundary stays true. A periodic sweep needs no proof request when its complete local rehash through the same opened source reproduces the boundary, revision and digest of a verified result this job received less than five minutes earlier, and the latest authorized heartbeat still reports that boundary for an active session. A changed boundary, revision or digest, or an older result, requires a fresh operation; never look up a remembered operation older than the five-minute terminal lifetime. Per-turn identity and size checks, heartbeats and failure handling are unchanged. Resume, resumed append and finite receipts still require a server-confirmed verified result and the latest-cursor checkpoint.

**Sealed transcript representation**

After termination, derive one immutable single-member gzip object asynchronously.
Bounded physical output parts retain one compressor history. The default exact
representation preserves the ordered original transcript. An opt-in policy may
replace explicitly selected nonempty string tokens in newly eligible sealed
JSONL sessions. Match the exact stored format and structural discriminators.
Preserve every unmatched byte. Bind all attempts to the policy and eligibility
cutoff frozen at job creation. Fail without publishing on malformed, ambiguous
or over-limit input. Existing published archives retain their representation.

Sealing and its original count and SHA-256 receipt stay immutable. A sanitized
generation has its own identity, count, digest and offset space. Verify
the source against its seal and the persisted output before publication.
Readback verifies the selected content receipt,
gzip integrity and part, checkpoint and ledger bindings. Use bounded,
dictionary-preserving restart points and verify complete overlapping original
chunks or sanitized blocks before returning a range.

Exact archives preserve original reads, historical prefix proofs and exact
append replay after guarded reclamation. For sanitized archives, those
operations require complete original bytes and report unavailability or state
conflict once any original bytes are deleted. Keep the original seal and append
ledger as source commitments. A sanitized representation cannot certify an
original byte operation. Independently verify the frozen transform against
published content before deletion. Reclamation requires separate sanitized qualification,
enablement, safe role and a current fenced permit, with durable progress for
bounded retry. Verify and retain the current occurrence projection at the final
original boundary before first deletion. When the original becomes unavailable,
exclude the session from source-dependent occurrence repair and report rebuild
unavailability while preserving the verified result.

Unqualified evidence reads and exports continue to use original coordinates
and receipts. Sanitized reads and exports require an explicit selection bound
to immutable generations, including per-entry receipts in a task archive.

A sealed browser view receives the complete compressed object, verifies that
transfer, then decodes, incrementally hashes and indexes it locally. Verify and
label the representation before serving content in a view, download or evidence
export. Preparing, cancellation and failure never publish partial evidence or
fall back to the active raw viewer. Revalidate current authorization for downloads
and cached or local generations. Bind cache ownership to authenticated principal,
credential or session context, workspace and pinned generation. Invalidate local
ownership on logout or identity changes. A range
racing reclamation must resolve a complete source for the requested representation
or fail. Keep reader caches and processing buffers bounded, release database
connections between requests, and qualify full sessions of at least 1 GiB and
archive resource use separately from live upload and native-file behavior.

**Server-wide HTTP admission**

Admit HTTP requests that can use the main database pool through one server-wide, bounded, first-in-first-out queue before any middleware checks out a connection. Place it inside the request-ID, request-log, CORS and rate-limit middleware, which use no connections. A request waits once for one of a fixed number of slots, by default the main pool size. The main pool has a single first-in-first-out stripe, so any idle connection serves any admitted request and the process never opens more connections than configured. Release the slot when the application returns, after a streamed body ends, or on failure or cancellation.

Heavy routes with long connection holds, timeline events and buckets and Observation discovery, first take one of fewer first-in-first-out heavy permits (default a third of the slots, at least one) and then join the general queue. Heavy requests therefore never hold every slot, and a light request never waits behind a heavy request that is still waiting for its permit.

Bound the waiting requests across both queues (default 1024) and each wait from arrival (default ten seconds). Only a full queue or an expired wait returns HTTP 503 with JSON error `server_busy` and `Retry-After: 1`; the request is not executed, and the request log records the admission decision and wait. Add no pool checkout timeout: the statement timeout bounds connection holds.

Exempt OPTIONS, non-API paths (static assets and document grant redemption), health, WebSocket upgrades, which keep their separate physical admission, and routes with their own bounded admission or transfer deadlines: OIDC login, the task evidence ZIP, session archive content streams and document original downloads.

Configure concurrency, heavy concurrency, queue limit and queue timeout in server configuration and the container environment, with validated ranges; concurrency above the pool size is allowed with a warning. Health reports the limits, gauges, cumulative decisions and fixed-bucket waits without identifiers.

**Consequences**

Browser and MCP operations pass through the same server policy and data boundary. Concurrent MCP calls retain stable workspace scoping and declared attribution, and context changes have an ordered handoff. Identity-less clients must upgrade their startup declaration; explicit workspace IDs do not bypass it. MCP depends on server/HTTP availability, must provide backpressure, and can delay a context switch while earlier accepted requests finish.

Under HTTP overload, clients receive a fast, retryable `server_busy` instead of unbounded latency, and admitted requests share the pool fairly; exempt routes rely on their own bounds.

<!-- @adrai:eyJhIjp7ImkiOiJjbGF1ZGUtY29kZSIsImsiOiJsbG0iLCJtIjoiY2xhdWRlLW9wdXMtNS01In0sImIiOiIxMDUzZGY1ZTA3MGI2MzA3MGIwNjc1NWIwMWFjNTkxNGVjMTA3YmZjIiwiaSI6InNoYTI1NjpNb3BVSEY5VGU5WjVoaVNRMXV2ZlQ2YlhCeEtIT2oyQ1FkTlQwbEN4ZUxBIiwiayI6ImRlY2lzaW9uLmFtZW5kIiwibyI6IlIwMU03TVYxUkRHMzQ3WkpRQzI5Mzg0RUhUTSIsIm9wIjoiTzAxTTdNVjFSREczNDdaSlFDMjkzODRFSFRNIiwicCI6WyJSMDFNN01CU0VQNkRYWVBaN1BFM01SOEpGODkiXSwiciI6Im1hc3RlciIsInMiOiJzaGEyNTY6c3ZGQWlKWTFPOTRNN081QllDVUJ0cmowa20wNjVLNnFzNFFUUVNxXzV2VSIsInQiOjE3OTQ5MjIwNDU4NzIsInYiOjEsIngiOiJhZHJhaS8xLjAuMCJ9 -->
