# hmem

hmem stores repository observations, projects, and tasks in PostgreSQL, with a web
interface, HTTP API, and MCP tools.

## Native installation

For Windows or Linux, install Haskell Stack, PostgreSQL, and a supported Node.js
LTS release (20 or later) with npm. Put `stack`, `initdb`, `pg_ctl`, `createdb`,
`psql`, and `npm` on `PATH`. Run these commands from this repository's root:

```bash
stack install
stack run build-frontend -- --install
hmem-ctl
hmem-ctl start
hmem-ctl status
```

Run `stack path --local-bin` to find the executable directory and add it to `PATH`
before running `hmem-ctl`. Setup creates a local database and
`~/.hmem/config.yaml`, and registers services to start automatically. Use
`hmem-ctl init` instead of the
bare `hmem-ctl` command if you want to manage startup yourself.

If the frontend build reports that it was skipped, install Node.js/npm and rerun
the build command. The API can run without the web interface.

## First use

Open [http://127.0.0.1:8420](http://127.0.0.1:8420). The native default allows
local access without a login. Keep it on loopback; configure
[authentication](auth.md) before enabling shared access.

From the repository you want to track, select or create its workspace:

```bash
hmem-ctl workspace
```

This writes `.hmem.workspace` in the current directory with the selected workspace
UUID. Use that workspace in the browser or pass its UUID to API and MCP calls.
See the [API guide](api.md) for creating observations, managing tasks, and handling
update conflicts.

Use `hmem-ctl status` to check services and `hmem-ctl stop` to stop them.
For container installation, follow the [Docker guide](docker.md), including its
required bearer token and browser sign-in step.

## MCP

Configure your MCP client to launch this command as a stdio server:

```bash
hmem-mcp --server-url http://127.0.0.1:8420
```

Keep hmem-server running. In the client, use `workspace_list` to find a workspace,
then `set_workspace` with its UUID so scoped tools can omit `workspace_id`.
The context belongs to that MCP session; the bridge does not read
`.hmem.workspace` automatically.

For a server that requires a token, set `HMEM_MCP_AUTH_TOKEN` in the MCP process's
environment. Use a service/PAT token for shared installations; see
[authentication](auth.md#service-and-mcp-tokens). Use HTTPS when connecting to a shared server.

## Optional similarity search

Full-text search works without embeddings. Similarity search requires pgvector
and a caller-supplied 1536-dimensional query vector in the same space as the stored
vectors. Automatic vectorization is off by default; its optional GPU setup has
no CPU fallback. Follow [embeddings and similarity search](embeddings.md) to
enable pgvector, import vectors, or configure automatic generation.

## Guides

| Topic | Guide |
| --- | --- |
| Login, permissions, and tokens | [Authentication](auth.md) |
| Requests, updates, and errors | [API](api.md) |
| Containers, backups, and restore | [Docker](docker.md) |
| Vectors and optional GPU setup | [Embeddings](embeddings.md) |
| Current tables and relationships | [Database schema](database.md) |
| Building and testing the web interface | [Frontend development](hmem-server/frontend/README.md) |
