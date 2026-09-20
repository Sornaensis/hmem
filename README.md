# hmem

A PostgreSQL-backed observation, project, and task management system for LLMs, written in Haskell.

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

### Optional semantic similarity

hmem works normally without pgvector. To store and search Observation embeddings,
install the pgvector package that matches the configured PostgreSQL server, then
inspect and enable the capability in that database:

```text
hmem-ctl pgvector status
hmem-ctl pgvector enable
hmem-ctl pgvector status
```

pgvector stores, indexes, and compares vectors; it does not create them. The
default hmem image leaves automatic vectorization disabled, while manual
1536-dimensional vectors through REST, MCP, or NDJSON remain available. An
optional Linux/WSL Docker GPU deployment can validate and run the pinned
native TEI model for automatic Observation vectorization; there is no CPU
inference fallback. See
[Database schema and embedding operations](database.md#pgvector-and-embedding-operations)
for manual and automatic workflows, and [Docker deployment](docker.md) for
the opt-in GPU setup. Similarity queries supply an already-produced vector;
hmem does not expose a raw-text query embedding endpoint.

### Docker / Compose

For containerized deployments, see [Docker deployment](docker.md). The Compose quick start builds the default `hmem:local` image, starts PostgreSQL, runs migrations, serves the web UI/API on port 8420, and documents auth and secret-handling choices.
