+++
schema = "adrai/decision/v1"
adr = "A01M39RTNXZ8BWZ54DXPN50GCCM"
record = "R01M39RTP9BBTXR3Y4ZQQY48BRK"
title = "Persist change events and resume live clients from canonical snapshots"
summary = "Use a PostgreSQL outbox, scoped cursors, and authorization-bound snapshot/resume tokens for live synchronization."
domains = ["live-synchronization"]
+++

Context
Browser clients can miss changes during a disconnect, while workspace access or user authorization can change between snapshot and replay. Workspace and global views need a canonical hand-off to the live stream.

Observed decision
PostgreSQL records committed change events in a durable outbox with ordered cursors for workspace and global scopes. Scope counters retain an authorization epoch and replay retention floor. The server materializes a scoped snapshot under the scope lock, then issues opaque snapshot and resume tokens bound to scope, audience, epoch, and expiry. Snapshot page reads recheck authorization and epoch. Replay rejects expired, superseded, unauthorized, or retention-pruned tokens; the canonical WebSocket path drains retained events and sends a checkpoint with a replacement resume token. The browser change-stream reducer applies checkpoints and requests a new resync when the stream is invalidated.

Consequences
Clients can recover a scoped live view from a canonical snapshot and retained replay while valid authorization and tokens hold. Authorization changes invalidate stale replay or revoke access. This design adds outbox retention, snapshot/session and token lifecycle, and replay/transport complexity.

Historical rationale
The original rationale is not recorded here; this ADR describes the observed implementation.

Evidence
hmem-server/migrations/V022__change_stream_outbox.sql
hmem-core/src/HMem/DB/ChangeStream.hs
hmem-server/src/HMem/Server/Snapshot.hs
hmem-server/src/HMem/Server/WebSocket.hs
hmem-server/frontend/src/Feature/ChangeStream.elm

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0ifSwiYiI6Ijc0ZGVhZTk2NWU2NzA1ODcxZDM5NTIyZmExNTE1OGE0MTNkZGNlNmEiLCJpIjoic2hhMjU2OlZXdWJXbUQ1R1dfb3FGNVFPenBydmk0SW5Mc1diX0hlWnhwaFhKdXJLSDQiLCJrIjoiZGVjaXNpb24uY3JlYXRlIiwibyI6IlIwMU0zOVJUUDlCQlRYUjNZNFpRUVk0OEJSSyIsIm9wIjoiTzAxTTM5UlRQOUJCVFhSM1k0WlFRWTQ4QlJLIiwiciI6Im1hc3RlciIsInMiOiJzaGEyNTY6Z2RjNm43b2FYSi1fcGk5akQwd3VlTk1xaWNXUXJidFlCbXRfcTNvc3ltYyIsInQiOjE3OTAyNTU2NTExMTUsInYiOjEsIngiOiJhZHJhaS8xLjAuMCJ9 -->
