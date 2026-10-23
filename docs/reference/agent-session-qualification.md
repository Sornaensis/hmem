# Agent session release qualification

This acceptance summary belongs to repository maintainers and stays with the
release documentation until superseded or retired. It retains commands,
provenance and limitations, without raw logs, credentials, transcript fixtures or
browser profiles. The qualification input is commit
`d92498db332be4e349b1218659d3578c10e815ae` plus the reviewed integrated test and
documentation changes accompanying this report.

## Integrated workflow

The production browser fixture starts the native deployed sandbox with isolated
PostgreSQL, builds production frontend assets for that sandbox's actual loopback
HTTP/WebSocket origins, and launches the freshly built MCP executable over
private stdio. Its seeded bearer stays in memory and the child environment.

The accepted path uses real follower registration, ordinary MCP work during
capture, many-to-many task links, active Elm records, pause/follow and navigation.
It kills the uploader process and proves that the session remains active, then
restarts MCP and verifies/resumes the committed prefix. A finite newline-free
flush remains active; a subsequent CRLF separator resumes capture. Stop returns
a verified receipt without sealing. A separate normal managing MCP client seals
that receipt, retries the exact seal and adds a later task link. Later local
producer output is excluded and task status is unchanged. A second normal MCP
seal while a real uploader is alive makes that follower retire safely.

Bounded MCP continuations and the production browser save independently verify
156 original bytes with SHA-256
`c7cfc4160201fc0ca3c8ffe243b2139849968cc6dff5f0352fa904cfe83f76bd`.
The representative larger session contains 5,280,331 bytes, including a
1,020,013-byte CRLF record crossing more than four viewer reads. Both retrieval
paths reconstruct SHA-256
`c32d23f9f18f64c1a35c52cfe1c6b120f69be0856d18bbd0a6926354e41c4acf`.
Independent Node hashing consumes saved file slices of at most 64 KiB.

The same fixture checks 53 summaries with 50-row keyset pages and filters before
pagination, empty/error/loading states, explicit retry, held genuine stale-query
responses, workspace switches and hidden-demand retirement. A genuine socket
close and invalid outbound resume token produce the server's real 409 and
canonical snapshot; paused rows and scroll position survive. Native XHR
send-to-completion accounting peaks at two requests per application lifetime.
Remote Playwright request-finished notifications can conservatively overlap a
completed request with its replacement and are reported separately.

## Commands and provenance

Run from the repository root; frontend commands run in `hmem-server/frontend`.
The qualification wrapper contains TEMP/TMP/TMPDIR under its task-owned scratch
directory and joins browser, MCP, helper and sandbox resources before disposal.

```text
stack build hmem-mcp:exe:hmem-mcp hmem-server:exe:hmem-test-harness --fast
stack test hmem-core --fast --ta '--match "Agent sessions" +RTS -N2 -RTS'
stack test hmem-mcp --fast --ta '--match "Session follower" +RTS -N2 -RTS'
stack test hmem-mcp --fast --ta '--match "Agent session MCP" +RTS -N2 -RTS'
stack test hmem-mcp --fast --ta '--match "HMem.MCP.Server" +RTS -N2 -RTS'
node --test tests-js/sessions.browser.test.js
node --test tests-js/session-mcp.test.js
```

All commands passed. The core family ran 21 examples (36.9053 s); the complete
follower family ran 22 examples; normal session MCP ran 10 (15.4637 s); ordinary
MCP server/context handling ran 21 (120.4459 s). The matching final browser case
passed in 79.1384 s and both causal client-cleanup tests passed. The follower
suite's elapsed summary was not retained; no elapsed value is inferred. Normal
exit requires every MCP parent PID gone and all three stdio streams destroyed;
every client receives cleanup even when another client fails. Repeated close
shares the same finite cleanup result, including timeout failures.

The final installed artifacts and production bundle have these SHA-256 values:

| Artifact | SHA-256 |
| --- | --- |
| `hmem-mcp.exe` | `37c151f38cc1db514c93ff37610a68707f23f276012f6cd4abedb6cac14cc3ca` |
| `hmem-test-harness.exe` | `2037fb987b8d252721c52975a22760cd8d0be7afb5e508f8c309c5871ca0aba4` |
| Production application JavaScript | `899bcf33857bc2854b5890d6ec8e8cb6376313578538ac785c9c9842d4acb2fc` |
| Production CSS | `91223439fffa0b1fb72b25855531383c4df1c247aaf4722cbd229b1714a76425` |
| Production vendor JavaScript | `c533b37dd70e3e6478382630cbd9ba1d3ced3d6e0ff0992c431effab0afe3147` |

The owning test build relinked MCP after the first build; the final browser run
used and hashed the resulting artifact above. Bundle hashes are specific to the
injected ephemeral sandbox HTTP/WebSocket origins. They are independently
recorded and are not asserted equal to an earlier UI test bundle. The committed
MCP source tree remains `61b80cfb2c3e0a79a8514723216924fb55ad22e7`, identical
to the reviewed follower implementation at `1e8865b5949acc9b693cf728fcf56c1063587610`.

Unchanged owning-input receipts also cover core change-stream tests,
server Agent session HTTP and canonical change-stream loopback tests, Elm
Sessions/ChangeStream/AppShell state tests, JavaScript framer/save/change-stream
tests and the production frontend build. These are reused component results,
not a claim that every suite ran again during final qualification.

## Boundaries

Component tests separately cover exact lost-ack replay, append/seal races,
unfinished/malformed records, file identity/rewrite detection, finite control
boundaries, fair follower caps and cleanup during blocked helper phases. The
helper phase probes are controlled native DNS/connect/TLS/body waits, not a slow
public DNS experiment. Existing deployed role fixtures test reader/editor/
outsider isolation and real membership revocation with socket closure. A
follower's injected 403 retirement test is separate response-handling evidence;
it is not described as an end-to-end membership change in this browser fixture.
Normal HTTP tests independently seal absent-uploader and stale server prefixes.

Runtime qualification is on Windows with GHC 9.10.2 and Chromium 151.0.7922.34,
using locked Playwright 1.62.1 at desktop 1440×1000 and narrow 430×900. Only the
file chooser is substituted with a real OPFS handle/native writable. Authorized
HTTP, streaming writes, incremental SHA-256 and verified close remain production
code. The native OS chooser and Linux execution were not exercised here.
Evidence certifies a finite server prefix and observed source verification;
it does not establish task acceptance or an atomic snapshot of arbitrary
external mutate-and-restore writes.
