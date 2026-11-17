# HTTP request logging

`hmem-server` writes one JSON line per HTTP request to its rotating server log.
The logger records metadata only. It never reads request bodies, so downstream
authorization and body limits run first, including for rejected requests.
Streamed responses pass each chunk directly to the transport without retaining
it.

## Fields

| Field | Value |
| --- | --- |
| `timestamp`, `level` | UTC emission time; `info` for successful outcomes, otherwise `warn`. |
| `request_id` | `X-Request-Id`, when it is 1–128 visible ASCII characters (no spaces). |
| `method` | Standard HTTP method, or `OTHER`. |
| `route` | OpenAPI path template, such as `/api/v1/workspaces/{workspaceId}/sessions`, plus `/api/v1/ws`, `/api/v1/openapi.json` and `/api/v1/auth/{login,callback,logout}`. Other API paths log `unmatched`; non-API paths log `static`. |
| `status` | HTTP status code. It is `null` for raw upgrades and responses that were never prepared. |
| `response` | `buffered`, `stream`, `file`, `upgrade` or `null`. |
| `outcome` | See below. |
| `prepare_ms` | Admission to response preparation, or `null`. |
| `complete_ms` | Admission to completion or failure. |
| `db_checkouts` | Main connection pool checkouts attributed to the request. Change-stream delivery pool work, including a WebSocket's initial replay, is excluded from all `db_*` fields. |
| `db_wait_ms` | Total checkout wait, from requesting a connection to receiving it, including new connections. |
| `db_connect_ms` | Part of `db_wait_ms` spent establishing new connections. Exception: a connection opened by the health check's probe is counted here with no checkout or wait. |
| `db_hold_ms` | Total time connections were held: statements, transaction control and work done while holding. |
| `provider_ms` | Optional embedding provider time for Observation discovery, or `null` without a provider call. |

Paths, identifiers, query strings, headers and bodies are not logged.
[Request performance](request-performance.md) defines the phase fields and
their attribution limits.
[Session traffic telemetry](session-traffic-telemetry.md) adds fixed outcomes
for session capture routes, including why a prefix proof was unknown.

## Duration boundaries

Durations use the monotonic clock. Admission is when the request reaches the
logger, after Warp parses headers. Preparation is when the application hands
a response to the server. Completion is when the server finishes writing that
response, ends its stream or closes the raw upgrade connection. If the
application fails before responding, completion is that failure.

## Outcomes

| Outcome | Meaning |
| --- | --- |
| `completed` | The HTTP response was written. |
| `upgrade_closed` | The raw upgrade connection ended normally. The handshake status is not observable, so `status` is `null`; [WebSocket telemetry](websocket-telemetry.md) records the upgrade decision. |
| `upgrade_fallback` | The transport could not upgrade and sent the fallback HTTP response. |
| `application_failed` | The application threw before responding. |
| `response_failed` | A response failed outside a stream producer with a non-I/O error, such as a failing lazy body. |
| `stream_failed` | A streaming body producer threw without a failed write. |
| `transmission_failed` | A write failed: a stream write (even one the producer caught), or an I/O error writing a stream's header or end, or a buffered or file response. |
| `upgrade_failed` | The raw upgrade connection ended with an exception. |
| `interrupted` | An asynchronous exception, such as a timeout, ended the request. |
| `no_response` | The application returned without responding. |
