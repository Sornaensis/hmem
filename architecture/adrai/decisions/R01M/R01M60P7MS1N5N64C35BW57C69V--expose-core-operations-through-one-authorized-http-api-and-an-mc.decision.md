+++
schema = "adrai/decision/v1"
adr = "A01M39QPB23CYS9QHCWQSATSJE9"
record = "R01M60P7MS1N5N64C35BW57C69V"
title = "Expose core operations through one authorized HTTP API and an MCP adapter"
summary = "Keep MCP as a bounded stdio-to-HTTP client and order workspace-context changes after previously accepted calls."
domains = ["api-boundary"]
+++

**Context**

Browser and LLM clients access the same workspaces, Observations, Projects, and Tasks. MCP requests can be processed concurrently, while their active workspace and declared bot identity can change. This amendment records startup, attribution, and ordering rules for that context without changing the server's authority.

**Decision**

The Haskell server defines the `/api/v1` HTTP contract and handles authorization and persistence for core operations. The MCP process accepts line-delimited JSON-RPC tool calls over stdio and maps them to server HTTP requests, forwarding a configured bearer credential when present. After initialization, require `set_workspace` with both `workspace_id` (UUID or explicit null) and `identity` containing exactly the validated `role` and `model-id` strings before any ordinary HTTP-forwarding tool, including global workspace tools and calls with an explicit workspace ID. A null workspace permits declaration before workspace discovery or registration. Validate the whole declaration before atomically replacing the complete workspace/identity context; invalid declarations change neither value. Setting a null workspace clears only the workspace and accepts the supplied identity. Repeating `initialize` preserves both values, and process exit forgets them. Protocol/tool discovery and local context controls remain available before declaration.

Capture the complete workspace/identity context when an ordinary request is accepted, carry that snapshot through a bounded queue and fixed worker pool, and inject the workspace ID when the call does not supply one explicitly. Both HTTP forwarding paths send one `X-HMem-Bot-Identity` header with the accepted declaration. Existing bearer credentials and authenticated MCP provenance forwarding remain unchanged. Declaration metadata is self-reported attribution, not authentication or authorization: tokenless local bootstrap remains supported, and role/model values grant no permissions. The HTTP server validates declarations and resolves effective actor IDs; MCP context responses report accepted metadata without claiming a server-resolved actor ID.

Treat a workspace/identity context control as an epoch boundary. Before applying that control, drain previously accepted ordinary requests from the queue and active workers. Ordinary requests within an epoch may execute concurrently. This prevents their externally visible effects from overtaking a later context switch while each request keeps its acceptance-time workspace/identity snapshot. A full queue returns an overload error.

Explicit local uploader registration is an ordinary admitted request that creates process-owned background work with immutable captured workspace, declared identity, credentials and forwarding configuration. After registration, follower jobs run outside the ordinary request queue, active-request accounting and epoch drains. Later context controls neither retarget nor await them. Keep a bounded registry, fixed fair workers, a shared global bound across every follower HTTP request, finite request/control/retry deadlines and finite shutdown cleanup. Ordinary MCP calls, including the separate explicit session termination operation, remain independent of follower HTTP slots.

Followers use the authorized HTTP API for metadata, prefix verification, append and heartbeat. Read only explicitly supplied regular files through stable opened-handle identity and compare the current path's opened identity. Verify the complete committed prefix before resume and continue bounded verification sweeps while following. Replacement, truncation and observed committed-prefix mismatch stop the job without skipping or overwriting server bytes. Preserve raw UTF-8/JSONL bytes and CRLF; hold partial tails. Flush and stop capture one finite source boundary and return metadata-only receipts after complete verification, or explicit pending status while bounded work continues. A receipt certifies the canonical committed prefix and observed source verification; it cannot prove absence of arbitrary external mutate-and-restore writes between observations.

Uploader start, caught-up state, EOF, flush, stop, failures and process shutdown never themselves terminate a session. Explicit management or an independently owned server cleanup policy may seal the verified already committed prefix. The cleanup policy applies only after more than one hour without canonical last_seen_at activity and when every explicitly associated task is done or cancelled. Zero associations qualify; unfinished soft-deleted linked tasks still block closure. Hash the immutable committed prefix outside an exclusive session lock, then atomically recheck activity, cursor and terminal task statuses before sealing. The policy never uploads or rewrites bytes or changes task status. A follower observing terminated state retires safely without later append or heartbeat mutation. Process loss leaves a session active until explicit resume or termination, or until the independent cleanup policy applies. Close terminal file handles, bound retained terminal receipts and cancel/join owned workers within a finite shutdown budget; report cleanup failure truthfully.

Useful bounded follower work is immediately eligible for the next fair turn; idle observation and retry backoff have explicit later deadlines. Admit at most four followers and two shared follower HTTP requests. Reuse at most two isolated helper transports and their HTTP connections, retaining finite per-request deadlines, cancellation, restart and native child/pipe ownership through confirmed shutdown. A timed-out or cancelled exchange retires its helper before the slot is reusable; failed joins poison admission rather than releasing ownership. Ordinary MCP requests and workspace controls never depend on follower transport leases.

Support additive asynchronous prefix verification through authorized submission and result reads. Bind each operation UUID to its exact workspace, session, authenticated audience, canonical append-boundary byte length, revision and lowercase SHA-256. Accept only the empty zero/zero boundary or an exact current or historical committed append boundary. Hash immutable committed bytes independently in bounded ordered windows, without holding a session row lock or a pool connection between turns. Every response includes the complete target binding and requires current workspace authorization. Exact tuple retries coalesce; conflicting UUID reuse fails. Limit the managed service to two fair workers, four live jobs and sixteen total retained entries. Fair requeue capacity covers all admitted jobs. Live work has a five-minute deadline. Terminal results have a maximum five-minute cache lifetime and may be evicted oldest-first on admission; never evict live work. Missing, expired, evicted or restarted operations are unknown and cannot certify verification.

The uploader hashes the complete relevant local prefix through its stable opened source handle, binds its proof state to that source generation and verifies the returned target before accepting proof. A completed historical sweep remains useful after append-only growth, but resumed append and finite receipts recheck the latest canonical cursor and verify the exact relevant target. Replacement, truncation, observed source/digest mismatch and authorization loss fail closed. Revalidation after lost operation state uses a new UUID and renewed source validation. Raw server-prefix transfer is optional; provisional append and weaker receipts are forbidden. Deploy the compatible server endpoint before a dependent uploader. Preserve existing append, replay, receipt, raw-byte and independent-sealing contracts.

**Sealed transcript representation**

After termination, derive one immutable single-member gzip object asynchronously
from the exact ordered transcript. Bounded physical output parts retain one
compressor history across all original byte arrays. Publish only after stored
readback verifies the original sealed byte count and SHA-256, compressed count
and digest, and part, checkpoint and append-ledger bindings. Sealing and its
immutable receipt remain unchanged.

Preserve raw-offset reads, historical append-boundary proofs and exact byte
replay after narrowly guarded raw-payload reclamation. Use bounded,
dictionary-preserving restart points. Verify complete overlapping original
chunks before slicing returned ranges: successful inflation alone cannot
establish range integrity. Authorized sequential readers reuse decoder state;
bounded caches keyed by the resolved authenticated principal and credential or
authentication-session context, workspace and generation never hold database transactions
or connections between calls. Pin the published representation, or resolve and
retry a raw read racing reclamation without returning a truncated prefix.

A sealed browser view receives the complete compressed object, verifies that
transfer, then decodes, incrementally hashes and indexes it locally. Verify the
original receipt before displaying transcript content. Preparing, cancellation
and failure never publish partial evidence or revert to the active raw-window
viewer. Revalidate current authorization for downloads and cached/local
generations; logout or principal/session changes invalidate local ownership even
when the workspace/session IDs are unchanged. Keep processing buffers, worker admission, decoder caches, index
batches and rendered windows bounded while supporting complete sessions of at
least 1 GiB raw size. Qualify actual storage, full-download and local processing
costs separately from the unchanged active/live guarantees.

**Consequences**

Browser and MCP operations pass through the same server policy and data boundary. Concurrent MCP calls retain stable workspace scoping and declared attribution, and context changes have an ordered handoff. Identity-less clients must upgrade their startup declaration; explicit workspace IDs do not bypass it. MCP depends on server/HTTP availability, must provide backpressure, and can delay a context switch while earlier accepted requests finish.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0iLCJtIjoiY29kZXg6Z3B0LTYuMS1zb2w6aGlnaCJ9LCJiIjoiNTYyYTE2ZDM1ZDI0ZmU3NzlmZjc0M2RkNWI2YmEzNjNiYWVlMjdhMyIsImkiOiJzaGEyNTY6RUVWbkQ2dWFVMTJWS0JadjR0akUyVGdhbXVLcjBuQUtJZk5Ba1ppVlozOCIsImsiOiJkZWNpc2lvbi5hbWVuZCIsIm8iOiJSMDFNNjBQN01TMU41TjY0QzM1Qlc1N0M2OVYiLCJvcCI6Ik8wMU02MFA3TVMxTjVONjRDMzVCVzU3QzY5ViIsInAiOlsiUjAxTTVTVzNUWkFKQVpCWVJUMjY5V1JUODQ2Il0sInIiOiJtYXN0ZXIiLCJzIjoic2hhMjU2OkFsSUhmMzRJbUVpN1Z1QWNYajFzQ2xaOUFXVWRBRGd3ZERwUUZ6a1phRnMiLCJ0IjoxNzkzMTcyMTY1NDA5LCJ2IjoxLCJ4IjoiYWRyYWkvMS4wLjAifQ -->
