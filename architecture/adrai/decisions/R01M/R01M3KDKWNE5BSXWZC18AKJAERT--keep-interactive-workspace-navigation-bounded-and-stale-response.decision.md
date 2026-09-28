+++
schema = "adrai/decision/v1"
adr = "A01M3KDKW8R8ZJ60DV727Y50S3Q"
record = "R01M3KDKWNE5BSXWZC18AKJAERT"
title = "Keep interactive workspace navigation bounded and stale-response safe"
summary = "Separate 50-item transport pages from 25-card presentation windows and guard asynchronous responses by scope and generation."
domains = ["frontend-navigation"]
+++

**Context**

Large workspaces can make tree navigation expensive, and responses may arrive after the user switches workspace, session, branch, focus, or filter. The change-stream ADR governs canonical snapshot and replay; interactive navigation needs a separate rendering and request contract.

**Decision**

For root and expanded-branch navigation, request bounded 50-item transport pages and maintain independent presentation cursors for a nominal 25-card window. Reuse cached transport data when moving among presentation windows and preserve focused or edited cards while reserving ordinary navigation capacity. Fetch more transport data when the requested presentation window leaves the cache.

Accept an asynchronous navigation response only when its workspace, session epoch, request generation, filter fingerprint, and requested offsets still match current state. Support direct focus without first rendering an entire tree. This decision concerns interactive navigation; canonical change-stream resynchronization can still traverse the full workspace in bounded transport pages.

**Consequences**

Navigation and rendering can remain responsive as workspace size grows, and late responses cannot silently replace newer scoped state. The client must manage separate transport and presentation cursors, caches, focus pins, and invalidation guards. The 25-card value is a presentation target; the code’s pin handling deserves continued regression coverage rather than an unsupported claim that every possible pin set is a hard 25-card maximum.

**Evidence**

- `hmem-server/frontend/src/Feature/DataLoading.elm`: 50-item transport page size, independent navigation state, and response guards.
- `hmem-server/frontend/src/Helpers.elm`: 25-card presentation capacity and pin-aware ordinary capacity.
- `hmem-server/frontend/src/Types.elm`: separate transport and presentation cursors.
- `hmem-server/frontend/src/Feature/Cards.elm` and `hmem-server/frontend/tests/DataLoadingTest.elm`: rendering and navigation behavior.
- `hmem-server/frontend/README.md`: current full canonical resync behavior and performance limits.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0ifSwiYiI6IjI2YjFmZWQyNWJlN2VjZWJjYWQ2MjJhNDBlMTlhY2FmYTQxOGFkYzEiLCJpIjoic2hhMjU2OmN2R2N0TkxTLVJodHBxam9iTWg1NTFUWW1IQlVOVkx4M3lqQURDRDZQNlUiLCJrIjoiZGVjaXNpb24uY3JlYXRlIiwibyI6IlIwMU0zS0RLV05FNUJTWFdaQzE4QUtKQUVSVCIsIm9wIjoiTzAxTTNLREtXTkU1QlNYV1pDMThBS0pBRVJUIiwiciI6Im1hc3RlciIsInMiOiJzaGEyNTY6eUVVNzRVYk9uTFl5VHVtWWl4WlFJZ2hNMm1ncHh1TDUyVXk5eGVkaVp5YyIsInQiOjE3OTA1Nzk0MzgyNTQsInYiOjEsIngiOiJhZHJhaS8xLjAuMCJ9 -->
