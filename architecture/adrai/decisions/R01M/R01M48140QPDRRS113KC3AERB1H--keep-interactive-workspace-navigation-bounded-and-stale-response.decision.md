+++
schema = "adrai/decision/v1"
adr = "A01M3KDKW8R8ZJ60DV727Y50S3Q"
record = "R01M48140QPDRRS113KC3AERB1H"
title = "Keep interactive workspace navigation bounded and stale-response safe"
summary = "Separate automatic expanded-branch transport, globally bounded detail demand, and the installed scrolling hierarchy viewport."
domains = ["frontend-navigation"]
+++

**Context**

Large workspaces can make hierarchy navigation expensive, and responses may arrive after workspace, session, authorization, expansion, focus, filter, or ordering changes. Canonical snapshots and replay have a separate change-stream contract. Interactive transport, detail demand, and rendering require separate bounded state.

**Decision**

Fetch project and task summaries in independent 50-item transport pages. Root lists remain driven by explicit root navigation demand. Effectively expanded project/task branches continue automatically to exhaustion, including default expansion and Expand All, only while their canonical ancestors remain present, visible, and expanded. Use a fair queue with four admitted expanded-branch HTTP slots and one current logical page per branch. Keep physical admissions until their exact completion, including stale errors; logical retirement never frees an outstanding HTTP slot. Preserve admissions and monotonic request identities across session resets. Collapse retires hidden descendants, and reopening resumes current incomplete cache under fresh guards.

Match every accepted response against workspace, session epoch, generation, filter fingerprint, offsets, in-flight state, and effective ancestor visibility. Give retries fresh generations even at unchanged offsets. Keep project/task completion and errors independent, pause failed kinds for explicit retry or reopening, and stop nonprogress pages with a truthful incomplete error. Wire hasMore pages contain the requested 50 rows; advance transport offsets by 50 while deduplicating cached IDs. The automatic client offset ceiling is 10,000, below the server's 100,000 limit; at that ceiling preserve hasMore and report incomplete membership.

Offset pagination is not a mutation snapshot. Same-filter live invalidation stages a fresh guarded membership pass per kind, preserving displayed membership until that kind reaches exhaustion. Ordering or membership changes discard the staged pass and coalesce a restart at offset zero. Drop retired staging and reject hidden/orphan descendants instead of attempting to recover shifted rows by deduplication alone. Preserve authoritative first-page replacement for changed navigation lifetimes and additive later pages. Order cached presentation deterministically by status rank, descending priority, normalized title/name, then ID.

Render the cached root and effectively expanded child summaries as one continuous logical preorder in a scrolling hierarchy viewport, separate from transport cursors and caches. Mount at most 25 ordinary rows across the whole viewport, plus a bounded set of active focus, editor, inline-create, drag, and native DOM-focus pins. Represent every omitted run with a spacer, including runs around distant pins. Measure mounted row wrappers with ResizeObserver; use estimated geometry and mounted-only fallback measurements when that API is absent. A cached height-sum index supports scroll and measurement updates without rebuilding the hierarchy. Preserve the current row and intrarow scroll anchor across structural changes when that row remains present. Offscreen focus and keyboard navigation first mount their logical target, then scroll or focus after the stamped DOM update. Drop boundaries use logical same-parent siblings, including offscreen neighbors.

Full details are demand driven, with six physical detail slots. Ordinary demand comes from the globally bounded mounted/overscan set, and active targets are prioritized separately. Deep focus requests the target rather than hydrating or mounting its entire ancestor chain. Keep stale-response fences, paused detail errors, and explicit queued retry. Workspace/session/generation/layout stamps reject stale measurement and focus work.

Canonical snapshot and replay behavior remains governed by its separate transport contract. Canonical resynchronization may still traverse the full workspace in bounded snapshot pages; interactive viewport mounting does not change that contract.

**Consequences**

Expanded hierarchy data can converge without repeated Show More clicks while HTTP fanout and mounted ordinary rows remain bounded. Completed branch caches do not flicker down to fresh page one during same-filter revalidation. Missing ancestors, stale generations, and delayed physical completions cannot revive retired branches or oversubscribe admitted slots. Finite ceilings and failures remain distinguishable from completed membership. The installed viewport makes cached members reachable by scrolling while retaining active targets and stable geometry.

Production browser compatibility and performance qualification remain separate acceptance gates. Local layout and bridge tests do not replace them. The versioned performance harness exercises production Main in locked Chromium with intercepted HTTP and canonical WebSocket transport; it isolates frontend scaling and transport amplification rather than database or real network latency. Timing and retained-heap gates depend on the recorded environment fingerprint, and historical records qualify their recorded input and asset hashes and measurement phase. This ADR does not establish a fresh qualification result for later repository revisions.

**Evidence**

- Feature/DataLoading.elm and Types.elm separate queued/admitted requests, guarded membership passes, transport cursors, and bounded detail demand.
- AppShell.elm preserves physical admission occupancy and monotonic counters through authorization/session retirement.
- DataLoadingTest.elm and AppShellSessionTest.elm cover automatic continuation, independent streams, fair caps, stale completions, collapse, paused errors, staged refresh, offset ceilings, orphan retirement, retry identity, and deep target demand.
- Api.elm requests project_limit=50 and task_limit=50. Server/API.hs uses limit+1 and takes the requested limit; DB/Overview.hs emits one summary per row.
- Feature/Cards.elm projects the logical hierarchy, selects bounded viewport rows and target pins, and derives visible detail demand. HierarchyViewport.elm maintains cached geometry and spacer pieces.
- hierarchy-viewport.js, Ports.elm, main.js, Main.elm, and UpdateRouter.elm connect stamped measurement, scroll, and focus work to the production update path.
- HierarchyViewportTest.elm, CardsFocusNavigationTest.elm, LifecycleBlockersTest.elm, and tests-js/hierarchy-viewport.test.js cover local viewport and bridge behavior.
- Frontend README describes the installed viewport, separate canonical resynchronization contract, and production browser/performance gates and qualification limits.
- perf/final-working-tree.validation-record.expanded-hierarchy.v1.json retains historical input/source/asset provenance and qualification receipts; those recorded results are not a new run performed by this amendment.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0ifSwiYiI6ImVlNDJhMDEwNmUyNjIzMGM0NWY2YTdmYjVkN2EzYTZkYWZmMzFjYzIiLCJpIjoic2hhMjU2OlBrOUQ5WFk5YmhtTVlmWmNzRU5KVjZ4UXNrWU44YTk0X0Q5N1hJZFV4bzAiLCJrIjoiZGVjaXNpb24uYW1lbmQiLCJvIjoiUjAxTTQ4MTQwUVBEUlJTMTEzS0MzQUVSQjFIIiwib3AiOiJPMDFNNDgxNDBRUERSUlMxMTNLQzNBRVJCMUgiLCJwIjpbIlIwMU00NUdRUFdTNTlYVDVLUVZWREtQVEQ2QSJdLCJyIjoibWFzdGVyIiwicyI6InNoYTI1NjpadF81UE96NURXcjIxeTRIcUY0Z2t4cjNPeVZOTkZpQmRDMnk5M25MS0xjIiwidCI6MTc5MTI3MDk3ODI5NCwidiI6MSwieCI6ImFkcmFpLzEuMC4wIn0 -->
