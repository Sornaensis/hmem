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

### Docker / Compose

For containerized deployments, see [Docker deployment](docker.md). The Compose quick start builds the default `hmem:local` image, starts PostgreSQL, runs migrations, serves the web UI/API on port 8420, and documents auth and secret-handling choices.
