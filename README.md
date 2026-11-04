


<img src="hmem-server/frontend/public/hmem-logo.svg" alt="hmem connected memory logo" width="366" height="96" />

hmem stores repository observations, projects, and tasks in PostgreSQL, with a web
interface, HTTP API, and MCP tools.

## Deploy with Docker Compose

Install Docker with Compose v2 and run these steps from the repository root.

1. Copy the configuration templates.

   ```bash
   cp .env.example .env
   cp config/compose.env.example config/compose.env
   ```

2. Set a strong `POSTGRES_PASSWORD` in `.env` and keep
   `HMEM_HTTP_BIND=127.0.0.1` for local access. In `config/compose.env`, uncomment
   and set `HMEM_AUTH_LOCAL_USER_TOKEN` to a strong URL-safe browser sign-in
   token, such as 32 random bytes encoded as hex. Keep both secrets out of Git.
3. Build and start the stack, then check its status.

   ```bash
   docker compose up --build -d
   docker compose ps
   ```

4. When `hmem` is healthy, open `http://127.0.0.1:8420/#hmem_token=<your-token>`
   with the token from `HMEM_AUTH_LOCAL_USER_TOKEN`. On the Workspaces page,
   select **+ Workspace** to create a repository workspace. Open it and copy
   the UUID from `/workspace/<workspace-uuid>` for your MCP client.

PostgreSQL keeps application data in the `postgres-data` volume. Use
`docker compose down` to stop the stack and retain that volume. For shared
access, configure [OIDC](docker.md#shared-oidc-authentication) and
[HTTPS](docker.md#configuration-and-secrets) before exposing the service. The
[Docker guide](docker.md) covers backups and recovery, troubleshooting, and
[setup for image and styled HTML previews](docker.md#compose-setup-with-document-previews).

## Connect an MCP client

For the local Compose stack, set `HMEM_API_KEY` in `config/compose.env` to a
strong token distinct from the browser token. Set `HMEM_MCP_AUTH_TOKEN` in the
same file to the `HMEM_API_KEY` value. If the stack is already running, apply the
settings with `docker compose up -d hmem` and wait for it to be healthy.

Configure your MCP client to launch this command from the Compose project
directory as a stdio server.

```bash
docker compose exec -T hmem hmem-entrypoint hmem-mcp
```

If you have `hmem-mcp` installed on the host, you can connect it directly to the
published port. Set `HMEM_MCP_AUTH_TOKEN` in the client's environment to the
local stack's `HMEM_API_KEY`, or use a [service token](auth.md#service-and-mcp-tokens)
and the server's HTTPS URL for a shared deployment.

```bash
hmem-mcp --server-url http://127.0.0.1:8420
```

After the MCP handshake, call `set_workspace` with your workspace UUID and both
identity fields.

```json
{"workspace_id":"<workspace-uuid>","identity":{"role":"Developer","model-id":"codex:gpt-6.1-sol:high"}}
```

Use your client's actual role and model identity. See the
[Docker MCP setup](docker.md#mcp-stdio-client) for container settings and
[MCP setup](hmem-mcp/README.md) for native clients.

## Walkthrough

![hmem walkthrough showing tasks, observations, and the timeline](docs/media/hmem-walkthrough.gif)

## Native installation

<details>
<summary>Show setup for Windows or Linux</summary>

For Windows or Linux, install Haskell Stack, PostgreSQL, and a supported Node.js
LTS release (20 or later) with npm. Put `stack`, `initdb`, `pg_ctl`, `createdb`,
`psql`, and `npm` on `PATH`. Run these commands from the repository root.

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
bare `hmem-ctl` command to manage startup yourself.

If the frontend build reports that it was skipped, install Node.js/npm and rerun
the build command. The API can run without the web interface.

Open [http://localhost:8420](http://localhost:8420). The native default allows
local access without a login. Keep it on loopback and configure
[authentication](auth.md) before enabling shared access.

From the repository you want to track, select or create its workspace.

```bash
hmem-ctl workspace
```

This writes `.hmem.workspace` in the current directory with the selected workspace
UUID. Use `hmem-ctl status` to check services and `hmem-ctl stop` to stop them.

</details>

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
- [Local scratch work](docs/reference/scratch-work.md)
