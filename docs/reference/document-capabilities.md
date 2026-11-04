# Document capabilities and isolated previews

Document grants delegate an issuer's current read access to one exact task
Source/Evidence association and immutable representation. They are transferable
bearer capabilities: anyone holding the URL can redeem it until expiry or
revocation. The document UUID alone never selects the role being delegated.

The server supports PAT token IDs, browser authentication-session IDs, and
explicit local-mode loopback authority. It preserves the PAT's token actor and
separate grant-bearing user. JWT API authentication continues to work, but JWT
grant issuance returns `credential_authority_unavailable`: the service has no
issuer introspection or tracked JWT revocation authority. Provider logout cannot
be inferred from a valid JWT signature.

Tokens contain 32 cryptographically random bytes, encoded as URL-safe base64.
Only SHA-256 hashes are persisted. Grant IDs are nonsecret UUIDs; tokens occur
only in the response URL's query, never in paths, audit values, canonical change
events or document metadata. The default lifetime is five minutes and the
maximum fifteen minutes, capped by the originating credential's expiry.

Each response and each bounded streaming chunk rechecks the originating
credential/session, current enabled user and workspace read permission, live
task and exact role-association UUID, grant expiry/revocation, and immutable
generation/policy/output receipt. ID-based renewal is SELECT-only and does not
update token/session usage for every part. Removing a Source invalidates its
grants even if Evidence retains the same document; recreating a Source with a
different association UUID cannot revive them. A stale generation never serves
newer output. A fresh shared runtime authority epoch invalidates outstanding
grants on service restart or replacement of the authority configuration.

Grant creation and explicit revocation audit only the nonsecret grant ID. They
change no document/count resource and emit no canonical workspace outbox event.
Upload, evidence association and Source removal retain their existing canonical
resource refresh behavior.

## Configuration

`document_delivery` is disabled for bearer links until valid public origins are
configured. Authenticated original and raster reads do not depend on a public
download origin. Origins are exact scheme/host/port origins, without user info,
paths, query strings or fragments. The server never constructs URLs from Host
or forwarded request headers. HTTPS is required outside explicit local mode
with loopback configuration, requests and origins.

```yaml
document_delivery:
  application_origin: https://hmem.example.com
  download_origin: https://hmem.example.com
  preview_origin: https://hmem-previews.example.net
  proxy_auth_cookie_domains: []
  preview_listen_host: 127.0.0.1
  preview_listen_port: 8421
```

An explicit `proxy_auth_cookie_domains` list declares the complete additional
authentication-cookie scopes set by upstream proxies; `[]` declares there are
none. Missing or invalid declarations disable styled HTML serving. Current
application authentication cookies are host-only. The preview host must differ
from both application/download hosts and fall outside every declared cookie
domain and its subdomains. Different ports on one host do not isolate cookies.
Prefer a separate registrable site. The dedicated listener binds loopback;
terminate public HTTPS at a reverse proxy routed only to that listener. Changes
to these settings require server restart, which invalidates outstanding links.

## Representations and HTTP

The authenticated document base is
`/api/v1/workspaces/{workspace}/tasks/{task}/documents/{document}`.

- `GET /previews/current` returns state/error, current generation and policy,
  plus bounded descriptors with variant, page, media type, byte count and hash.
- `GET/HEAD /previews/{generation}/{variant}/{page}` serves only inert raster
  bytes and refuses stale generations explicitly. It cannot return styled HTML.
- `POST /download-grants` requires explicit `role`, `association_id`, `variant`
  and `page`, with `generation_id` for derivatives and optional `ttl_seconds`.
  Read-authorized callers may delegate their own reads. Browser mutations retain
  the existing CSRF requirement.
- `DELETE /download-grants/{grant_id}` permits the issuer or a workspace admin
  to revoke a grant.

Variants are `original` (page 0, no generation), `page`, `thumbnail`, and
`styled_html_preview` (page 0). Derivatives require an exact current generation;
pages are bounded to 0–200. Original responses force attachment disposition and
preserve the original bytes. Raster representations carry their verified image
media type. All grant responses are GET/HEAD only, with no redirects,
`Cache-Control: no-store`, `Referrer-Policy: no-referrer`, and `nosniff`.

Redemption uses `/document-grants/{grant_id}/{variant}/{page}?token=...` at the
configured download origin, or the dedicated preview origin for styled HTML.
The token does not authorize application APIs, session downloads, another role
association, page or representation. A failed preview preserves original access.

## Dedicated HTML application

The preview listener exposes only GET/HEAD grant redemption. It rejects Cookie
and Authorization headers, resolves no browser session, sets no cookies, and
has no application/static/API routes or fallback. Sanitized generations are
rendered by the separate confined worker; originals are never served inline.
It also requires exactly one Host authority matching the configured canonical
preview host and public port before inspecting the token or body. Application,
download, unknown, missing, duplicate and wrong-port authorities are rejected;
forwarded-host headers cannot substitute for Host. A reverse proxy must preserve
the original public Host authority, including a nonstandard public port, and
must not rewrite an alias/misrouted host to the configured preview host.

Public original/raster grants use the application's existing configured rate
limiter, including its shared admission state. The dedicated listener applies
the equivalent configured limiter. Admission errors retain no-store/referrer/
nosniff and the dedicated response CSP; bearer redemption bypasses browser
authentication but does not bypass configured request admission.

Styled snapshots enforce this response policy, including direct navigation:

```text
Content-Type: text/html;charset=utf-8
Content-Security-Policy: default-src 'none'; script-src 'none'; style-src 'unsafe-inline'; img-src data:; font-src 'none'; connect-src 'none'; frame-src 'none'; child-src 'none'; worker-src 'none'; object-src 'none'; media-src 'none'; base-uri 'none'; form-action 'none'; sandbox; frame-ancestors <configured-application-origin>
Cache-Control: no-store
Referrer-Policy: no-referrer
X-Content-Type-Options: nosniff
```

The application response policy permits frames only from the configured
validated preview origin. UI consumers must also use an empty iframe sandbox
and `referrerpolicy="no-referrer"`; trusted close/download controls stay outside
the frame. No scripts, resize messages or shared application credentials are
needed. Unproven isolation reports preview unavailable without an app-origin
fallback.

The application opts into a framed HTML response with `view=frame` on the
isolated grant URL. The response contains one sandboxed `srcdoc` child with the
escaped snapshot. The server completes that child's opening tag only after
sending the snapshot and rechecking the grant. The app reveals the loaded
iframe only when the child exists. Token-only requests still return the raw
sanitized snapshot and its byte count and digest.

## Query-free access logging

The application JSON request logger records `rawPathInfo`, without query strings.
Configure every upstream access log similarly. For example, an Nginx log format
can use `$uri` and must not include `$request`, `$request_uri`, `$args` or request
headers containing credentials:

```nginx
log_format hmem_paths '$remote_addr $request_method $uri $status $body_bytes_sent';
access_log /var/log/nginx/hmem-access.log hmem_paths;
```

Route the preview virtual host exclusively to the dedicated listener and do not
install authentication middleware/cookies there. Application tests verify the
application logger and metadata projections; operator-controlled proxy, tracing
and observability layers require their own matching query-free configuration.
