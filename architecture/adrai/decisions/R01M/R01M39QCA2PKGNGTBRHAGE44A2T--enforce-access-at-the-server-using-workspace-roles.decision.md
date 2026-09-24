+++
schema = "adrai/decision/v1"
adr = "A01M39QC9VF4S8ARWZ25NDSY8FJ"
record = "R01M39QCA2PKGNGTBRHAGE44A2T"
title = "Enforce access at the server using workspace roles"
summary = "Keep global grants and workspace read/edit/admin authorization in the server across browser and MCP clients."
domains = ["authorization"]
+++

Context: hmem supports local single-user use and deployed shared use by browser, service, and MCP clients. This record describes current behavior; these sources do not establish the historical reason for the choice.

Observed decision: Resolve each request principal at the server. Local mode can bootstrap a synthetic superadmin; deployed mode resolves browser OIDC cookie sessions or explicit bearer JWT and PAT credentials to grant-bearing users. Enforce global create-workspace and superadmin grants and workspace read, edit, and admin roles in server authorization checks used by API handlers. Require a matching CSRF token for unsafe requests authenticated with a deployed cookie session.

Consequences: Server-side checks provide a common authorization boundary for browser and MCP callers and protect workspace-scoped operations. Deployment also requires identity provider, user grant, membership, session, bearer-token, and CSRF configuration and operation.

Evidence:
- auth.md: documents local bootstrap, deployed OIDC sessions and bearer credentials, global permissions, workspace role hierarchy, and CSRF behavior.
- hmem-core/src/HMem/DB/Auth.hs: defines global grants and workspace roles, resolves grant-bearing users and memberships, and performs role and scope authorization.
- hmem-server/src/HMem/Server/App.hs: resolves local or deployed principals and checks CSRF for unsafe cookie-authenticated requests.
- hmem-server/src/HMem/Server/API.hs: applies global and workspace authorization checks to handlers for workspace and resource operations.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0ifSwiYiI6ImQwMTk5MjhlOTFmNTI0NWM4YTgyNmE0YTM4YThjZjI0ODVkMDhiYjgiLCJpIjoic2hhMjU2OjczQnhFbk5ocXBDQ28tZDkxTzJHLVhSbEhxdTNjc2s2NTJVbVJKQmg5b00iLCJrIjoiZGVjaXNpb24uY3JlYXRlIiwibyI6IlIwMU0zOVFDQTJQS0dOR1RCUkhBR0U0NEEyVCIsIm9wIjoiTzAxTTM5UUNBMlBLR05HVEJSSEFHRTQ0QTJUIiwiciI6Im1hc3RlciIsInMiOiJzaGEyNTY6SVVfdUhXOTE2U2xmTHBDNnNQaldTNWRrVkxoVWdlQThneVo3NEZaUnJWayIsInQiOjE3OTAyNTQxMzEyODYsInYiOjEsIngiOiJhZHJhaS8xLjAuMCJ9 -->
