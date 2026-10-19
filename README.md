<img src="hmem-server/frontend/public/hmem-logo.svg" alt="hmem connected memory logo" width="366" height="96" />

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
See the [API reference](api.md) for creating observations, managing tasks, and handling
update conflicts.

Use `hmem-ctl status` to check services and `hmem-ctl stop` to stop them.
For container installation, follow the [Docker guide](docker.md), including its
required bearer token and browser sign-in step.

## MCP

Configure your MCP client to launch this command as a stdio server:

```bash
hmem-mcp --server-url http://127.0.0.1:8420
```

Keep hmem-server running. After the MCP initialization handshake, call
`set_workspace` with both required arguments before ordinary tools:

```json
{"workspace_id":null,"identity":{"role":"Developer","model-id":"codex:gpt-6.1-sol:high"}}
```

Use `workspace_list` or `workspace_register`, then call `set_workspace` with the
selected UUID and complete identity. For a shared server, set
`HMEM_MCP_AUTH_TOKEN` to a service token and use HTTPS. See
[MCP setup](hmem-mcp/README.md) and [authentication](auth.md#service-and-mcp-tokens).

## Optional similarity search

Full-text search works without embeddings. Add optional similarity search with
compatible vectors, or opt into GPU generation. See [embeddings](embeddings.md)
for setup and usage.

## Guides

| Topic | Guide |
| --- | --- |
| Login, permissions, and tokens | [Authentication](auth.md) |
| Using the web interface | [Web interface](hmem-server/frontend/guide.md) |
| Containers, backups, and restore | [Docker](docker.md) |
| Vectors and optional GPU setup | [Embeddings](embeddings.md) |

## References

- [API](api.md) and [MCP](hmem-mcp/reference.md)
- [Database diagram](database.md) and [schema behavior](docs/reference/schema.md)
- [Upgrades](docs/reference/upgrades.md)
- [Frontend development](hmem-server/frontend/development.md)
