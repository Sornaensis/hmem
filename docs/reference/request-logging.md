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

Paths, identifiers, query strings, headers and bodies are not logged.

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
| `upgrade_closed` | The raw upgrade connection ended normally. The handshake status is not observable, so `status` is `null`. |
| `upgrade_fallback` | The transport could not upgrade and sent the fallback HTTP response. |
| `application_failed` | The application threw before responding. |
| `response_failed` | A response failed outside a stream producer with a non-I/O error, such as a failing lazy body. |
| `stream_failed` | A streaming body producer threw without a failed write. |
| `transmission_failed` | A write failed: a stream write (even one the producer caught), or an I/O error writing a stream's header or end, or a buffered or file response. |
| `upgrade_failed` | The raw upgrade connection ended with an exception. |
| `interrupted` | An asynchronous exception, such as a timeout, ended the request. |
| `no_response` | The application returned without responding. |
