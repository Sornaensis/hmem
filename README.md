# hmem

A PostgreSQL-backed memory and task management system for LLMs, written in Haskell.

See also:

- `auth.md` — canonical auth specification for local/deployed auth modes, principals, permissions, and policy rules
- `database.md` — current database schema overview

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
- **Workspace Scoping** — Scope by `.hmem.workspace` file in the project root or GitHub owner/repo
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

**Prerequisites**: PostgreSQL must be installed with `initdb`, `pg_ctl`, `createdb`, and `psql` on PATH. To build the web frontend, Node.js/npm must also be installed and `npm` must be on PATH.

## Frontend module structure

The Elm frontend is now split so `Main.elm` stays a thin entrypoint and orchestration layer.

- `Main.elm` — browser program wiring, flag decoding, init, top-level update/view/subscriptions delegation
- `AppShell.elm` — shell-owned init composition, shell handlers, full `Browser.Document` layout, page selection, subscriptions
- `UpdateRouter.elm` — exhaustive `Msg` router that delegates feature-owned messages to the correct module
- `Route.elm` — URL parsing and route-change handling
- `Page/` — page-level composition modules (`Home`, `Workspace`)
- `Feature/` — feature-specific update/view logic (`Search`, `Editing`, `Cards`, `Memory`, `AuditLog`, etc.)
- `Types.elm` — shared `Model`, `Msg`, and supporting frontend types
- `Helpers.elm` / `Ports.elm` / `Toast.elm` — shared helpers, port definitions, and toast handling

For frontend production verification, use:

```bash
stack run build-frontend
```

Note: `build-frontend` intentionally prints a warning and exits successfully if `npm` is not available, so a successful Stack run alone does not prove the frontend was actually built. Verify that Node.js/npm is installed and check for the final `Frontend built -> hmem-server/static/` message.
