+++
schema = "adrai/decision/v1"
adr = "A01M3KDKW8R8ZJ60DV727Y50S3Q"
record = "R01M45GQPWS59XT5KQVVDKPTD6A"
title = "Keep interactive workspace navigation bounded and stale-response safe"
summary = "Separate automatic expanded-branch transport, guarded detail demand, and subsequent scrolling viewport rendering."
domains = ["frontend-navigation"]
+++

**Context**

Large workspaces can make hierarchy navigation expensive, and responses may arrive after workspace, session, authorization, expansion, focus, filter, or ordering changes. Canonical snapshots and replay have a separate change-stream contract. Interactive transport, detail demand, and rendering require separate bounded state.

**Decision**

Fetch project and task summaries in independent 50-item transport pages. Root lists remain driven by explicit root navigation demand. Effectively expanded project/task branches continue automatically to exhaustion, including default expansion and Expand All, only while their canonical ancestors remain present, visible, and expanded. Use a fair queue with four admitted expanded-branch HTTP slots and one current logical page per branch. Keep physical admissions until their exact completion, including stale errors; logical retirement never frees an outstanding HTTP slot. Preserve admissions and monotonic request identities across session resets. Collapse retires hidden descendants, and reopening resumes current incomplete cache under fresh guards.

Match every accepted response against workspace, session epoch, generation, filter fingerprint, offsets, in-flight state, and effective ancestor visibility. Give retries fresh generations even at unchanged offsets. Keep project/task completion and errors independent, pause failed kinds for explicit retry or reopening, and stop nonprogress pages with a truthful incomplete error. Wire hasMore pages contain the requested 50 rows; advance transport offsets by 50 while deduplicating cached IDs. The automatic client offset ceiling is 10,000, below the server's 100,000 limit; at that ceiling preserve hasMore and report incomplete membership.

Offset pagination is not a mutation snapshot. Same-filter live invalidation stages a fresh guarded membership pass per kind, preserving displayed membership until that kind reaches exhaustion. Ordering or membership changes discard the staged pass and coalesce a restart at offset zero. Drop retired staging and reject hidden/orphan descendants instead of attempting to recover shifted rows by deduplication alone. Preserve authoritative first-page replacement for changed navigation lifetimes and additive later pages. Order cached presentation deterministically by status rank, descending priority, normalized title/name, then ID.

Full details are demand driven, with six physical detail slots and globally bounded ordinary demand. The transitional renderer still uses nominal 25-card moving presentation windows; it must not hydrate 25 cards independently for every loaded branch. Focus requests the target first, and focus/edit/inline-create/retry targets are distinct from logical ancestor projection. Keep stale-response fences, paused detail errors, and explicit queued retry. A workspace/session/generation-guarded visible-ID demand interface supports the next rendering integration.

The target rendering contract is one scrolling viewport over the logical expanded hierarchy, separate from transport cursors and caches, with bounded mounted rows, stable spacers/anchors, and active target pins. That viewport remains a subsequent implementation step; automatic branch transport does not claim it is already implemented. Canonical resynchronization may still traverse the full workspace in bounded snapshot pages.

**Consequences**

Expanded hierarchy data can converge without repeated Show More clicks while HTTP fanout remains bounded. Completed branch caches do not flicker down to fresh page one during same-filter revalidation. Missing ancestors, stale generations, and delayed physical completions cannot revive retired branches or oversubscribe admitted slots. Finite ceilings and failures remain distinguishable from completed membership. Rendering remains transitional until the viewport integration, and production browser/performance validation must evaluate that integrated behavior.

**Evidence**

- Feature/DataLoading.elm and Types.elm separate queued/admitted requests, guarded membership passes, transport and presentation cursors, and bounded detail demand.
- AppShell.elm preserves physical admission occupancy and monotonic counters through authorization/session retirement.
- DataLoadingTest.elm and AppShellSessionTest.elm cover automatic continuation, independent streams, fair caps, stale completions, collapse, paused errors, staged refresh, offset ceilings, orphan retirement, retry identity, and deep target demand.
- Api.elm requests project_limit=50 and task_limit=50. Server/API.hs uses limit+1 and takes the requested limit; DB/Overview.hs emits one summary per row.
- Frontend README distinguishes current automatic transport from the subsequent scrolling viewport and the independent canonical resync contract.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0ifSwiYiI6Ijg0ZWIwY2VjMTk2M2EyODdkNWI4ZGM2OTYxNmQ1NmRmMWFmOWY3NWIiLCJpIjoic2hhMjU2OnZ6dkFLei0xSzRXcXRZV1N4TnAxekhlVXFWLW50YjcyODN4UU1lS0pOVjQiLCJrIjoiZGVjaXNpb24uYW1lbmQiLCJvIjoiUjAxTTQ1R1FQV1M1OVhUNUtRVlZES1BURDZBIiwib3AiOiJPMDFNNDVHUVBXUzU5WFQ1S1FWVkRLUFRENkEiLCJwIjpbIlIwMU0zS0RLV05FNUJTWFdaQzE4QUtKQUVSVCJdLCJyIjoibWFzdGVyIiwicyI6InNoYTI1Njp0dEpLT212X0tDaHphOWpjLXZNTEwyaVROUnEwQk5EUzAxVnFKeGhseXhRIiwidCI6MTc5MTE4NjY4ODkyMSwidiI6MSwieCI6ImFkcmFpLzEuMC4wIn0 -->
