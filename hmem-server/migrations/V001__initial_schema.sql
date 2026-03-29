-- hmem: PostgreSQL schema for LLM memory and task management
-- Requires PostgreSQL 14+

BEGIN;

------------------------------------------------------------------------
-- Extensions (optional)
------------------------------------------------------------------------

-- uuid-ossp and pgcrypto are NOT required: gen_random_uuid() is built
-- into PostgreSQL 13+.

-- pgvector: enable if available (needed for embedding similarity search)
DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM pg_available_extensions WHERE name = 'vector') THEN
    EXECUTE 'CREATE EXTENSION IF NOT EXISTS "vector"';
  END IF;
END;
$$;

------------------------------------------------------------------------
-- Enumeration types
------------------------------------------------------------------------

DO $$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'workspace_type_enum') THEN
    CREATE TYPE workspace_type_enum AS ENUM ('repository', 'planning', 'personal', 'organization');
  END IF;
END $$;

DO $$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'memory_type_enum') THEN
    CREATE TYPE memory_type_enum AS ENUM ('short_term', 'long_term');
  END IF;
END $$;

DO $$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'project_status_enum') THEN
    CREATE TYPE project_status_enum AS ENUM ('active', 'paused', 'completed', 'archived');
  END IF;
END $$;

DO $$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'task_status_enum') THEN
    CREATE TYPE task_status_enum AS ENUM ('todo', 'in_progress', 'blocked', 'done', 'cancelled');
  END IF;
END $$;

DO $$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'relation_type_enum') THEN
    CREATE TYPE relation_type_enum AS ENUM (
        'related', 'supersedes', 'contradicts', 'elaborates',
        'inspires', 'depends_on', 'derived_from', 'alternative_to'
    );
  END IF;
END $$;

------------------------------------------------------------------------
-- Helper: auto-update updated_at
------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION hmem_set_updated_at()
RETURNS TRIGGER AS $$
BEGIN
  NEW.updated_at = now();
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

------------------------------------------------------------------------
-- Workspaces
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS workspaces (
    id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    name            TEXT NOT NULL,
    workspace_type  workspace_type_enum NOT NULL DEFAULT 'repository',
    path            TEXT,
    gh_owner        TEXT,
    gh_repo         TEXT,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at      TIMESTAMPTZ NOT NULL DEFAULT now(),

    CONSTRAINT uq_workspace_path UNIQUE (path),
    CONSTRAINT uq_workspace_github UNIQUE (gh_owner, gh_repo)
);

DROP TRIGGER IF EXISTS trg_workspaces_updated_at ON workspaces;
CREATE TRIGGER trg_workspaces_updated_at
    BEFORE UPDATE ON workspaces
    FOR EACH ROW EXECUTE FUNCTION hmem_set_updated_at();

------------------------------------------------------------------------
-- Memory categories (hierarchical, per-workspace)
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS memory_categories (
    id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    workspace_id    UUID REFERENCES workspaces(id) ON DELETE CASCADE,
    name            TEXT NOT NULL,
    description     TEXT,
    parent_id       UUID REFERENCES memory_categories(id) ON DELETE SET NULL,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),

    CONSTRAINT uq_category_name_ws UNIQUE (workspace_id, name, parent_id)
);

-- Global categories (workspace_id IS NULL) get their own uniqueness
CREATE UNIQUE INDEX IF NOT EXISTS uq_global_category_name ON memory_categories (name, parent_id)
    WHERE workspace_id IS NULL;

------------------------------------------------------------------------
-- Memories
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS memories (
    id                UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    workspace_id      UUID NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
    content           TEXT NOT NULL,
    summary           TEXT,
    memory_type       memory_type_enum NOT NULL,
    importance        SMALLINT NOT NULL DEFAULT 5
                          CHECK (importance BETWEEN 1 AND 10),
    metadata          JSONB NOT NULL DEFAULT '{}',
    -- pgvector embedding column (NULL when no embedding has been set).
    -- Requires the 'vector' extension; column is only created if the
    -- extension is available (see DO block at end of schema).
    -- embedding      vector(1536),
    expires_at        TIMESTAMPTZ,
    source            TEXT,
    confidence        DOUBLE PRECISION NOT NULL DEFAULT 1.0
                          CHECK (confidence BETWEEN 0.0 AND 1.0),
    pinned            BOOLEAN NOT NULL DEFAULT false,
    last_accessed_at  TIMESTAMPTZ NOT NULL DEFAULT now(),
    access_count      INTEGER NOT NULL DEFAULT 0,
    fts_language      TEXT NOT NULL DEFAULT 'english'
                          CHECK (fts_language IN (
                            'simple', 'arabic', 'armenian', 'basque', 'catalan', 'danish',
                            'dutch', 'english', 'finnish', 'french', 'german', 'greek',
                            'hindi', 'hungarian', 'indonesian', 'irish', 'italian',
                            'lithuanian', 'nepali', 'norwegian', 'portuguese', 'romanian',
                            'russian', 'serbian', 'spanish', 'swedish', 'tamil', 'turkish',
                            'yiddish'
                          )),
    search_vector     tsvector NOT NULL DEFAULT ''::tsvector,
    created_at        TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at        TIMESTAMPTZ NOT NULL DEFAULT now()
);

------------------------------------------------------------------------
-- Memory tags (many-to-many)
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS memory_tags (
    memory_id   UUID NOT NULL REFERENCES memories(id) ON DELETE CASCADE,
    tag         TEXT NOT NULL,
    PRIMARY KEY (memory_id, tag)
);

CREATE OR REPLACE FUNCTION hmem_memories_search_vector()
RETURNS TRIGGER AS $$
DECLARE
  tag_text TEXT;
BEGIN
  SELECT coalesce(string_agg(tag, ' '), '') INTO tag_text
    FROM memory_tags WHERE memory_id = NEW.id;
  NEW.search_vector :=
      setweight(to_tsvector(NEW.fts_language::regconfig, coalesce(NEW.content, '')), 'A') ||
      setweight(to_tsvector(NEW.fts_language::regconfig, coalesce(NEW.summary, '')), 'B') ||
      setweight(to_tsvector(NEW.fts_language::regconfig, tag_text), 'C');
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trg_memories_search_vector ON memories;
CREATE TRIGGER trg_memories_search_vector
    BEFORE INSERT OR UPDATE OF content, summary, fts_language ON memories
    FOR EACH ROW EXECUTE FUNCTION hmem_memories_search_vector();

CREATE OR REPLACE FUNCTION hmem_memory_tags_reindex()
RETURNS TRIGGER AS $$
BEGIN
  UPDATE memories SET content = content
    WHERE id = coalesce(NEW.memory_id, OLD.memory_id);
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trg_memory_tags_reindex ON memory_tags;
CREATE TRIGGER trg_memory_tags_reindex
    AFTER INSERT OR DELETE ON memory_tags
    FOR EACH ROW EXECUTE FUNCTION hmem_memory_tags_reindex();

DROP TRIGGER IF EXISTS trg_memories_updated_at ON memories;
CREATE TRIGGER trg_memories_updated_at
    BEFORE UPDATE ON memories
    FOR EACH ROW EXECUTE FUNCTION hmem_set_updated_at();

------------------------------------------------------------------------
-- Memory ↔ category links
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS memory_category_links (
    memory_id    UUID NOT NULL REFERENCES memories(id) ON DELETE CASCADE,
    category_id  UUID NOT NULL REFERENCES memory_categories(id) ON DELETE CASCADE,
    PRIMARY KEY (memory_id, category_id)
);

------------------------------------------------------------------------
-- Memory ↔ memory links (typed, weighted edges)
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS memory_links (
    source_id       UUID NOT NULL REFERENCES memories(id) ON DELETE CASCADE,
    target_id       UUID NOT NULL REFERENCES memories(id) ON DELETE CASCADE,
    relation_type   relation_type_enum NOT NULL,
    strength        DOUBLE PRECISION NOT NULL DEFAULT 1.0
                        CHECK (strength BETWEEN 0.0 AND 1.0),
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    PRIMARY KEY (source_id, target_id, relation_type),
    CHECK (source_id != target_id)
);

------------------------------------------------------------------------
-- Projects (hierarchical sub-projects, workspace-scoped)
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS projects (
    id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    workspace_id    UUID NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
    parent_id       UUID REFERENCES projects(id) ON DELETE CASCADE,
    name            TEXT NOT NULL,
    description     TEXT,
    status          project_status_enum NOT NULL DEFAULT 'active',
    priority        SMALLINT NOT NULL DEFAULT 5
                        CHECK (priority BETWEEN 1 AND 10),
    metadata        JSONB NOT NULL DEFAULT '{}',
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at      TIMESTAMPTZ NOT NULL DEFAULT now()
);

DROP TRIGGER IF EXISTS trg_projects_updated_at ON projects;
CREATE TRIGGER trg_projects_updated_at
    BEFORE UPDATE ON projects
    FOR EACH ROW EXECUTE FUNCTION hmem_set_updated_at();

------------------------------------------------------------------------
-- Tasks (hierarchical sub-tasks within projects)
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS tasks (
    id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    workspace_id    UUID NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
    project_id      UUID REFERENCES projects(id) ON DELETE SET NULL,
    parent_id       UUID REFERENCES tasks(id) ON DELETE CASCADE,
    title           TEXT NOT NULL,
    description     TEXT,
    status          task_status_enum NOT NULL DEFAULT 'todo',
    priority        SMALLINT NOT NULL DEFAULT 5
                        CHECK (priority BETWEEN 1 AND 10),
    metadata        JSONB NOT NULL DEFAULT '{}',
    due_at          TIMESTAMPTZ,
    completed_at    TIMESTAMPTZ,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at      TIMESTAMPTZ NOT NULL DEFAULT now()
);

DROP TRIGGER IF EXISTS trg_tasks_updated_at ON tasks;
CREATE TRIGGER trg_tasks_updated_at
    BEFORE UPDATE ON tasks
    FOR EACH ROW EXECUTE FUNCTION hmem_set_updated_at();

------------------------------------------------------------------------
-- Task dependencies (DAG edges)
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS task_dependencies (
    task_id         UUID NOT NULL REFERENCES tasks(id) ON DELETE CASCADE,
    depends_on_id   UUID NOT NULL REFERENCES tasks(id) ON DELETE CASCADE,
    PRIMARY KEY (task_id, depends_on_id),
    CHECK (task_id != depends_on_id)
);

------------------------------------------------------------------------
-- Project ↔ memory links
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS project_memory_links (
    project_id  UUID NOT NULL REFERENCES projects(id) ON DELETE CASCADE,
    memory_id   UUID NOT NULL REFERENCES memories(id) ON DELETE CASCADE,
    PRIMARY KEY (project_id, memory_id)
);

------------------------------------------------------------------------
-- Task ↔ memory links
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS task_memory_links (
    task_id     UUID NOT NULL REFERENCES tasks(id) ON DELETE CASCADE,
    memory_id   UUID NOT NULL REFERENCES memories(id) ON DELETE CASCADE,
    PRIMARY KEY (task_id, memory_id)
);

------------------------------------------------------------------------
-- Cleanup policies (per workspace, per memory type)
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS cleanup_policies (
    id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    workspace_id    UUID NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
    memory_type     memory_type_enum NOT NULL,
    max_age_hours   INTEGER,
    max_count       INTEGER,
    min_importance  SMALLINT NOT NULL DEFAULT 1,
    enabled         BOOLEAN NOT NULL DEFAULT true,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at      TIMESTAMPTZ NOT NULL DEFAULT now(),

    CONSTRAINT uq_cleanup_policy UNIQUE (workspace_id, memory_type)
);

DROP TRIGGER IF EXISTS trg_cleanup_policies_updated_at ON cleanup_policies;
CREATE TRIGGER trg_cleanup_policies_updated_at
    BEFORE UPDATE ON cleanup_policies
    FOR EACH ROW EXECUTE FUNCTION hmem_set_updated_at();

------------------------------------------------------------------------
-- Cycle prevention triggers
------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION hmem_check_task_dep_cycle()
RETURNS TRIGGER AS $$
BEGIN
  IF EXISTS (
    WITH RECURSIVE chain AS (
      SELECT depends_on_id AS id
        FROM task_dependencies
       WHERE task_id = NEW.depends_on_id
      UNION ALL
      SELECT td.depends_on_id
        FROM task_dependencies td
        JOIN chain c ON td.task_id = c.id
    )
    SELECT 1 FROM chain WHERE id = NEW.task_id
  ) THEN
    RAISE EXCEPTION 'Cycle detected in task dependencies';
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trg_task_dep_no_cycle ON task_dependencies;
CREATE TRIGGER trg_task_dep_no_cycle
    BEFORE INSERT OR UPDATE ON task_dependencies
    FOR EACH ROW EXECUTE FUNCTION hmem_check_task_dep_cycle();

CREATE OR REPLACE FUNCTION hmem_check_project_cycle()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.parent_id IS NOT NULL THEN
    IF NEW.parent_id = NEW.id THEN
      RAISE EXCEPTION 'Project cannot be its own parent';
    END IF;
    IF EXISTS (
      WITH RECURSIVE ancestors AS (
        SELECT parent_id FROM projects WHERE id = NEW.parent_id
        UNION ALL
        SELECT p.parent_id
          FROM projects p
          JOIN ancestors a ON p.id = a.parent_id
         WHERE p.parent_id IS NOT NULL
      )
      SELECT 1 FROM ancestors WHERE parent_id = NEW.id
    ) THEN
      RAISE EXCEPTION 'Cycle detected in project hierarchy';
    END IF;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trg_project_no_cycle ON projects;
CREATE TRIGGER trg_project_no_cycle
    BEFORE INSERT OR UPDATE OF parent_id ON projects
    FOR EACH ROW EXECUTE FUNCTION hmem_check_project_cycle();

CREATE OR REPLACE FUNCTION hmem_check_task_cycle()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.parent_id IS NOT NULL THEN
    IF NEW.parent_id = NEW.id THEN
      RAISE EXCEPTION 'Task cannot be its own parent';
    END IF;
    IF EXISTS (
      WITH RECURSIVE ancestors AS (
        SELECT parent_id FROM tasks WHERE id = NEW.parent_id
        UNION ALL
        SELECT t.parent_id
          FROM tasks t
          JOIN ancestors a ON t.id = a.parent_id
         WHERE t.parent_id IS NOT NULL
      )
      SELECT 1 FROM ancestors WHERE parent_id = NEW.id
    ) THEN
      RAISE EXCEPTION 'Cycle detected in task hierarchy';
    END IF;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trg_task_no_cycle ON tasks;
CREATE TRIGGER trg_task_no_cycle
    BEFORE INSERT OR UPDATE OF parent_id ON tasks
    FOR EACH ROW EXECUTE FUNCTION hmem_check_task_cycle();

CREATE OR REPLACE FUNCTION hmem_check_category_cycle()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.parent_id IS NOT NULL THEN
    IF NEW.parent_id = NEW.id THEN
      RAISE EXCEPTION 'Category cannot be its own parent';
    END IF;
    IF EXISTS (
      WITH RECURSIVE ancestors AS (
        SELECT parent_id FROM memory_categories WHERE id = NEW.parent_id
        UNION ALL
        SELECT mc.parent_id
          FROM memory_categories mc
          JOIN ancestors a ON mc.id = a.parent_id
         WHERE mc.parent_id IS NOT NULL
      )
      SELECT 1 FROM ancestors WHERE parent_id = NEW.id
    ) THEN
      RAISE EXCEPTION 'Cycle detected in category hierarchy';
    END IF;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trg_category_no_cycle ON memory_categories;
CREATE TRIGGER trg_category_no_cycle
    BEFORE INSERT OR UPDATE OF parent_id ON memory_categories
    FOR EACH ROW EXECUTE FUNCTION hmem_check_category_cycle();

------------------------------------------------------------------------
-- Task completion consistency
------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION hmem_task_completion()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.status = 'done' AND (TG_OP = 'INSERT' OR OLD.status IS DISTINCT FROM 'done') THEN
    NEW.completed_at = now();
  ELSIF NEW.status != 'done' THEN
    NEW.completed_at = NULL;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trg_task_completion ON tasks;
CREATE TRIGGER trg_task_completion
    BEFORE INSERT OR UPDATE ON tasks
    FOR EACH ROW EXECUTE FUNCTION hmem_task_completion();

------------------------------------------------------------------------
-- Workspace groups (portfolios)
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS workspace_groups (
    id          UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    name        TEXT NOT NULL UNIQUE,
    description TEXT,
    created_at  TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at  TIMESTAMPTZ NOT NULL DEFAULT now()
);

DROP TRIGGER IF EXISTS trg_workspace_groups_updated_at ON workspace_groups;
CREATE TRIGGER trg_workspace_groups_updated_at
    BEFORE UPDATE ON workspace_groups
    FOR EACH ROW EXECUTE FUNCTION hmem_set_updated_at();

CREATE TABLE IF NOT EXISTS workspace_group_members (
    group_id     UUID NOT NULL REFERENCES workspace_groups(id) ON DELETE CASCADE,
    workspace_id UUID NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
    joined_at    TIMESTAMPTZ NOT NULL DEFAULT now(),
    PRIMARY KEY (group_id, workspace_id)
);

------------------------------------------------------------------------
-- Indexes
------------------------------------------------------------------------

CREATE INDEX IF NOT EXISTS idx_memories_workspace         ON memories (workspace_id);
CREATE INDEX IF NOT EXISTS idx_memories_workspace_type    ON memories (workspace_id, memory_type);
CREATE INDEX IF NOT EXISTS idx_memories_workspace_importance ON memories (workspace_id, importance DESC);
CREATE INDEX IF NOT EXISTS idx_memories_expires           ON memories (expires_at)
    WHERE expires_at IS NOT NULL;
CREATE INDEX IF NOT EXISTS idx_memories_last_accessed     ON memories (last_accessed_at);
CREATE INDEX IF NOT EXISTS idx_memories_created           ON memories (workspace_id, created_at DESC);
CREATE INDEX IF NOT EXISTS idx_memories_metadata          ON memories USING gin (metadata);
CREATE INDEX IF NOT EXISTS idx_memories_search            ON memories USING gin (search_vector);

CREATE INDEX IF NOT EXISTS idx_memory_tags_tag            ON memory_tags (tag);
CREATE INDEX IF NOT EXISTS idx_memory_tags_covering       ON memory_tags (tag, memory_id);

CREATE INDEX IF NOT EXISTS idx_memory_categories_ws       ON memory_categories (workspace_id);
CREATE INDEX IF NOT EXISTS idx_memory_categories_parent   ON memory_categories (parent_id)
    WHERE parent_id IS NOT NULL;

CREATE INDEX IF NOT EXISTS idx_memory_links_target        ON memory_links (target_id);

CREATE INDEX IF NOT EXISTS idx_projects_workspace         ON projects (workspace_id);
CREATE INDEX IF NOT EXISTS idx_projects_parent            ON projects (parent_id) WHERE parent_id IS NOT NULL;

CREATE INDEX IF NOT EXISTS idx_tasks_workspace            ON tasks (workspace_id);
CREATE INDEX IF NOT EXISTS idx_tasks_project              ON tasks (project_id) WHERE project_id IS NOT NULL;
CREATE INDEX IF NOT EXISTS idx_tasks_ws_status            ON tasks (workspace_id, status);

CREATE INDEX IF NOT EXISTS idx_task_deps_depends_on       ON task_dependencies (depends_on_id);

CREATE INDEX IF NOT EXISTS idx_project_mem_links_memory   ON project_memory_links (memory_id);
CREATE INDEX IF NOT EXISTS idx_task_mem_links_memory      ON task_memory_links (memory_id);

CREATE INDEX IF NOT EXISTS idx_ws_group_members_ws        ON workspace_group_members (workspace_id);

CREATE INDEX IF NOT EXISTS idx_projects_workspace_status  ON projects (workspace_id, status);

CREATE INDEX IF NOT EXISTS idx_tasks_project_status       ON tasks (project_id, status) WHERE project_id IS NOT NULL;
CREATE INDEX IF NOT EXISTS idx_tasks_parent               ON tasks (parent_id) WHERE parent_id IS NOT NULL;
CREATE INDEX IF NOT EXISTS idx_tasks_due                  ON tasks (due_at)
    WHERE due_at IS NOT NULL AND status NOT IN ('done', 'cancelled');

CREATE INDEX IF NOT EXISTS idx_memories_pinned            ON memories (workspace_id) WHERE pinned = true;

CREATE INDEX IF NOT EXISTS idx_memories_workspace_id      ON memories (workspace_id, id);

------------------------------------------------------------------------
-- pgvector: conditionally add embedding column + index
------------------------------------------------------------------------

DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'vector') THEN
    IF NOT EXISTS (
      SELECT 1 FROM information_schema.columns
       WHERE table_name = 'memories' AND column_name = 'embedding'
    ) THEN
      EXECUTE 'ALTER TABLE memories ADD COLUMN embedding vector(1536)';
    END IF;
    IF NOT EXISTS (
      SELECT 1 FROM pg_indexes
       WHERE tablename = 'memories' AND indexname = 'idx_memories_embedding'
    ) THEN
      EXECUTE 'CREATE INDEX idx_memories_embedding ON memories USING hnsw (embedding vector_cosine_ops)';
    END IF;
  END IF;
END;
$$;

------------------------------------------------------------------------
-- Schema migrations tracking
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS schema_migrations (
    version     INTEGER PRIMARY KEY,
    name        TEXT NOT NULL,
    applied_at  TIMESTAMPTZ NOT NULL DEFAULT now()
);

COMMIT;