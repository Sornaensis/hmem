# hmem

A PostgreSQL-backed memory and task management system for LLMs, written in Haskell.

## Architecture

```
┌──────────────┐      stdio/JSON-RPC       ┌──────────────────┐
│   LLM / AI   │ ◄───────────────────────► │   hmem-mcp       │
│   Model      │                           │   (per-model)    │
└──────────────┘                           └────────┬─────────┘
                                                    │ HTTP
                                           ┌────────┴─────────┐
                                           │   hmem-server    │
                                           │   (single inst)  │
                                           └────────┬─────────┘
                                                    │ SQL
                                           ┌────────┴─────────┐
                                           │   PostgreSQL     │
                                           └──────────────────┘
```

- **hmem-server** — Single-instance HTTP server that owns the database.
- **hmem-mcp** — MCP (Model Context Protocol) server, spawned per-model session over stdio. Forwards to hmem-server via HTTP.
- **hmem-core** — Shared library: domain types, configuration, and database operations.

## Features

- **Sharded Memory System** — Short-term and long-term memories scoped by workspace
- **Full-Text Search** — PostgreSQL tsvector-based search with weighted ranking
- **Memory Interlinking** — Typed, weighted edges between memories (related, supersedes, contradicts, elaborates, …)
- **Hierarchical Categories** — Organize memories into nested categories
- **Tag System** — Flexible tagging for fast filtered queries
- **Project Management** — Workspace-scoped projects with sub-project hierarchy
- **Task Management** — Tasks with sub-tasks, dependencies (DAG), and memory references
- **Workspace Scoping** — Scope by filesystem path or GitHub owner/repo
- **Configurable Cleanup** — Automatic expiry and pruning by age, count, and importance thresholds
- **MCP Protocol** — JSON-RPC over stdio for direct LLM integration

## Installation

### Quick Setup (recommended)

Build and install the executables, then run the setup tool:

```bash
stack install
hmem-setup
```

This runs `hmem-setup init` followed by `hmem-setup install`, which:

1. **Initializes `~/.hmem/`** — creates config, data, and log directories
2. **Sets up PostgreSQL** — initializes a local data directory, starts PG temporarily, creates the `hmem` database, and runs all migrations
3. **Writes `~/.hmem/config.yaml`** — default configuration (DB credentials, server port, CORS origins)
4. **Copies migrations** to `~/.hmem/migrations/` for future upgrades
5. **Registers auto-start services**:
   - **Linux**: systemd user services (`hmem-postgres`, `hmem-server`)
   - **Windows**: scheduled task + start/stop `.bat` scripts
6. **Installs MCP server configs**:
   - **VS Code**: merges `hmem` entry into `~/.config/Code/User/mcp.json` (or `AppData/Roaming/Code/User/mcp.json` on Windows)
   - **Claude Desktop**: merges into `claude_desktop_config.json`
7. **Installs agent definitions** to `~/.hmem/agents/` (Copilot `.agent.md` files and Claude instructions)

**Prerequisites**: PostgreSQL must be installed with `initdb`, `pg_ctl`, `createdb`, and `psql` on PATH.

### Setup Subcommands

| Command | Description |
|---|---|
| `hmem-setup init` | Initialize `~/.hmem/`, PostgreSQL, and config only |
| `hmem-setup install` | Register auto-start services + install MCP/agent configs |
| `hmem-setup start` | Start PostgreSQL and hmem-server |
| `hmem-setup stop` | Stop hmem-server and PostgreSQL |
| `hmem-setup status` | Show whether services are running |
| `hmem-setup uninstall` | Stop services, remove auto-start, delete `~/.hmem/` |
| `hmem-setup reinstall` | Uninstall + full setup from scratch |

### Using the Agent Definitions

After installation, agent files are in `~/.hmem/agents/copilot/`:

- `hmem-hmem.agent.md` — General-purpose hmem agent (memory + tasks)
- `hmem-memory.agent.md` — Memory management specialist
- `hmem-task.agent.md` — Task/project management specialist

To use with **VS Code Copilot**, copy the `.agent.md` files to your project's `.github/agents/` directory, or to a global location recognized by your setup.

For **Claude**, the instructions are in `~/.hmem/agents/claude/hmem-instructions.md` — add to your Claude project instructions.

### Manual Setup

If you prefer to manage PostgreSQL yourself:

```bash
# 1. Build
stack build

# 2. Create database and apply migrations
createdb hmem
for f in sql/migrations/V*.sql; do psql hmem < "$f"; done

# 3. Start the server
stack exec hmem-server

# 4. Configure your MCP client to run hmem-mcp via stdio
stack exec hmem-mcp -- --server-url http://localhost:8420
```

The MCP server reads JSON-RPC on stdin and writes responses to stdout.

## License

This project is licensed under the Mozilla Public License 2.0. See the `LICENSE` file for the full text.
