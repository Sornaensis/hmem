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
stack install                              # installs hmem-server, hmem-mcp, hmem-ctl to ~/.local/bin
stack run build-frontend -- --install      # builds + installs frontend to ~/.hmem/static/
hmem-ctl                                   # initial setup and install
hmem-ctl start                             # start the server
```

**Prerequisites**: PostgreSQL must be installed with `initdb`, `pg_ctl`, `createdb`, and `psql` on PATH.