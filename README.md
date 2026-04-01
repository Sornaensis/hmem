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
- **Soft Deletes + Purge** — Delete operations hide entities first; permanent removal is explicit via purge endpoints/tools
- **MCP Protocol** — JSON-RPC over stdio for direct LLM integration

## Installation

### Quick Setup (recommended)

Build and install the executables, then run the setup tool:

```bash
stack install
hmem-ctl
```

This runs `hmem-ctl init` followed by `hmem-ctl install`, which:

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