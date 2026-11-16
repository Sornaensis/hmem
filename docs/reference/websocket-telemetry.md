# WebSocket telemetry

`hmem-server` measures its `/api/v1/ws` transport with fixed-label outcome
counters, monotonic durations and occupancy gauges. There is no metrics
endpoint. Noteworthy events are written as JSON lines to the server log beside
[request logging](request-logging.md); in-process callers and tests read the
complete counters with `readWebSocketTelemetry`.

Labels are fixed enumerations. Tickets, frames, principals, user, workspace
and connection identifiers are never recorded.

## Events

| Event | Label | Outcomes | Duration |
| --- | --- | --- | --- |
| `websocket_upgrade` | `ticket_kind` | `accepted`, `rejected_unauthorized`, `rejected_unavailable`, `rejected_invalid_identity`, `failed` | `handshake_ms` |
| `websocket_connection` | `ticket_kind` | `closed`, `failed` | `lifetime_ms` |
| `websocket_replay` | `phase` | `completed`, `expired`, `resync_required`, `revoked`, `retired`, `disconnected` | `replay_ms` |
| `websocket_turn` | | `completed`, `failed`, `retired_write_failed`, `retired_write_timeout`, `retired_deadline`, `retired_closing`, `interrupted`, `refused` | `turn_ms`, `lease_wait_ms` |
| `websocket_write` | | `completed`, `failed`, `timed_out`, `interrupted` | `write_ms` |
| `websocket_cleanup` | | `joined`, `unfinished`, `abort_failed`, `failed` | `cleanup_ms` |
| `websocket_reclaim` | | `reclaimed` | `quarantine_ms` |

`ticket_kind` is `canonical`, `activity`, `legacy`, `none` (ticketless local
access) or `unresolved` (no ticket matched, or the bot identity was invalid).
`phase` is `initial` for the catch-up after acceptance and `live` for each
later dispatch cycle.

Upgrade and connection lines also carry `admitted` (physical admissions),
`leases` (delivery leases in use), `lease_waiters` and `quarantined`.

Every upgrade, connection and initial replay is logged. Successful live
replays, completed and refused turns, writes and joined cleanups are counted
but not logged; their failures are logged as turn and cleanup events. Failures,
retirements and unavailable rejections use level `warn`.

## Outcomes

- An upgrade is `accepted` once the 101 response is written, rejected once the
  rejection is written, and `failed` if the handshake throws first. An invalid
  bot identity is counted only for WebSocket upgrade requests.
- A connection is `closed` when its application returns and `failed` when it
  throws.
- A replay is `completed` when its final checkpoint or hidden-only rebase
  settles the client's bearer; `expired`, `resync_required` and `revoked` record the server's
  terminal decision; `retired` means the server retired its delivery turn or
  aborted the socket, including at shutdown; `disconnected` means the peer
  ended the connection first.
- A turn is retired for a write failure, a write past its 250 ms deadline, the
  750 ms work deadline without a write in progress, or owner closing. The
  parent's deadline decides, not the native writer.
- A cleanup is `unfinished` when the join exceeds its 250 ms budget; the turn
  stays quarantined until a later hub admission or release reclaims it.

## Durations

All durations use the monotonic clock. `handshake_ms` runs from the request
reaching the WebSocket middleware to the upgrade decision. `lifetime_ms` runs
from acceptance to the application's end. `replay_ms` runs from the start of a
replay cycle (acceptance, or the dispatch that found visible work) to its
outcome. `turn_ms` runs from lease admission to the turn's outcome;
`lease_wait_ms` precedes it. `write_ms` covers one framed write, including Close
and ping frames. `quarantine_ms` is measured when the reclamation is emitted.

A socket-write completion is not client receipt. Commit-to-delivery latency is
measured only by test clients that observe the frame themselves.

## Snapshot

`readWebSocketTelemetry` emits pending events and returns per-key `count`,
`timed`, `totalMs` and `maxMs`, keyed by event, label and outcome, plus:

- admitted and retained physical admissions, delivery leases in use,
  quarantined leases, lease waiters and the oldest quarantine age;
- canonical, activity and legacy clients, pending replays and the oldest open
  replay age;
- dropped log events.

Reading never reclaims retained delivery work.

## Emission

The change-stream dispatcher emits pending events on every turn, at least once
per second when idle. At most 256 events wait for emission; overflow and failed
log writes are counted as dropped events. Counters are not affected. Shutdown
emits pending events before the log closes.
