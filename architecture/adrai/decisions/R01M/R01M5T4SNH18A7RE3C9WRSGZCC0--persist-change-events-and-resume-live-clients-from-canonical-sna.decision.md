+++
schema = "adrai/decision/v1"
adr = "A01M39RTNXZ8BWZ54DXPN50GCCM"
record = "R01M5T4SNH18A7RE3C9WRSGZCC0"
title = "Persist change events and resume live clients from canonical snapshots"
summary = "Use a PostgreSQL outbox, scoped cursors, and authorization-bound snapshot/resume tokens for live synchronization."
domains = ["live-synchronization"]
+++

Context

Clients can disconnect during live changes, and authorization can change between snapshot, replay and a socket write. A slow observer must not prevent unrelated clients from receiving committed changes.

Decision

Persist committed events in a PostgreSQL outbox with independent ordered workspace and global cursors, authorization epochs and retention floors. Materialize canonical snapshots under the scope lock and issue opaque snapshot/resume tokens bound to scope, audience, epoch and expiry. Every replay page rechecks authentication, authorization, token identity, epoch, expiry, supersession and retention. Superseded tokens remain rejected; clients recover through canonical resynchronization.

Rotate at most four scopes per dispatcher turn, reading at most one thousand records per scope. Deliver the resulting bounded batch once per physical client. Advance process-local scope acknowledgements only after managed dispatch succeeds; cancellation or failure retains the previous acknowledgement for at-least-once retry. Process acknowledgements do not acknowledge client delivery. Sleep only after a complete idle scope sweep with no pending replay continuation. Run retention maintenance by elapsed time.

Use four shared physical delivery leases across initial replay, live dispatch, reauthorization and legacy delivery. A client turn has a 750 millisecond work budget and a 250 millisecond cancellation/join cleanup budget; individual writes have a 250 millisecond deadline. Yield voluntarily only between completed records. Release leases only after the owned action, children and finalizers have finished. Unconfirmed cleanup retains its lease, poisons admission and fails the dispatcher rather than replacing workers or reporting clean shutdown. Rethrow asynchronous control exceptions. Overload retirement wakes the owning socket application; physical connection admissions remain occupied until that owner finishes. Preserve the ordinary expiry handshake and its physical-slot lifetime.

Keep constant-size replay continuation state per canonical client: original bearer, internal scan cursor, fixed high-water mark, visibility/initial-checkpoint flags and one dirty-wakeup bit. Read at most five hundred records per page. The private scan helper validates original watermark <= scan cursor <= fixed high-water <= current high-water and refreshes all authority and retention checks on every page. Retain the original bearer through intermediate pages and voluntary yields. Pending work remains eligible even when no new outbox records arrive; a dirty wakeup schedules changes beyond the frozen end.

At scan completion, write the replacement checkpoint before one durable acknowledgement, retaining the existing opaque token policy. Interruption before acknowledgement leaves the original bearer replayable, so duplicates are possible. If a final checkpoint is lost after acknowledgement, the old superseded bearer triggers canonical resynchronization; this does not guarantee redelivery of every historical event. A socket-write completion is not proof of client receipt. Preserve the fixed connection capability deadline.

Hidden-only activity produces no new checkpoint cadence or capability extension. Keep authorization immediately before sensitive delivery and serialize each client's replay and control writes. Activity hints contain metadata only, are lossy wakeups and never advance canonical cursors; legacy compatibility remains best effort. Reauthorize idle clients without repeating a full scan for every empty scope.

Consequences

Slow or failed observers receive explicit retirement while other admitted clients can progress. Durable tokens, fresh authorization and replay retention remain authoritative; bounded scheduling never promotes an internal scan cursor into a public resume token.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0iLCJtIjoiZ3B0LTYuMS1zb2wifSwiYiI6IjJlMjdmODljZWZiYzA2NGI2MmFmYTUyOTBkNjA2OGZiNzM0ZjAyZGYiLCJpIjoic2hhMjU2OnVxQnQxZ2hDNkp6dFY4VTdvR2t0bllrdmlfUzZqdURiYTJOOVZQeGc4NVUiLCJrIjoiZGVjaXNpb24uYW1lbmQiLCJvIjoiUjAxTTVUNFNOSDE4QTdSRTNDOVdSU0daQ0MwIiwib3AiOiJPMDFNNVQ0U05IMThBN1JFM0M5V1JTR1pDQzAiLCJwIjpbIlIwMU0zOVJUUDlCQlRYUjNZNFpRUVk0OEJSSyJdLCJyIjoibWFzdGVyIiwicyI6InNoYTI1Njo3Qm00aTQxN0RyR2NrUFRJUDBXaUliZkhCTUh2RlgtY2FySUJZMGd0SjdzIiwidCI6MTc5Mjk1MjU1NTA0MSwidiI6MSwieCI6ImFkcmFpLzEuMC4wIn0 -->
