BEGIN;

------------------------------------------------------------------------
-- Enumeration types
------------------------------------------------------------------------

DO $$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'audit_action_enum') THEN
    CREATE TYPE audit_action_enum AS ENUM ('create', 'update', 'delete');
  END IF;
END $$;

------------------------------------------------------------------------
-- Soft deletes
------------------------------------------------------------------------

ALTER TABLE workspaces ADD COLUMN IF NOT EXISTS deleted_at TIMESTAMPTZ;
ALTER TABLE memory_categories ADD COLUMN IF NOT EXISTS deleted_at TIMESTAMPTZ;
ALTER TABLE memories ADD COLUMN IF NOT EXISTS deleted_at TIMESTAMPTZ;
ALTER TABLE projects ADD COLUMN IF NOT EXISTS deleted_at TIMESTAMPTZ;
ALTER TABLE tasks ADD COLUMN IF NOT EXISTS deleted_at TIMESTAMPTZ;

------------------------------------------------------------------------
-- Size constraints
------------------------------------------------------------------------

DO $$ BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
     WHERE conname = 'chk_workspaces_name_octet_length'
       AND conrelid = 'workspaces'::regclass
  ) THEN
    ALTER TABLE workspaces
      ADD CONSTRAINT chk_workspaces_name_octet_length
      CHECK (octet_length(name) <= 1024);
  END IF;
END $$;

DO $$ BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
     WHERE conname = 'chk_memory_categories_name_octet_length'
       AND conrelid = 'memory_categories'::regclass
  ) THEN
    ALTER TABLE memory_categories
      ADD CONSTRAINT chk_memory_categories_name_octet_length
      CHECK (octet_length(name) <= 1024);
  END IF;
END $$;

DO $$ BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
     WHERE conname = 'chk_memories_content_octet_length'
       AND conrelid = 'memories'::regclass
  ) THEN
    ALTER TABLE memories
      ADD CONSTRAINT chk_memories_content_octet_length
      CHECK (octet_length(content) <= 524288);
  END IF;
END $$;

DO $$ BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
     WHERE conname = 'chk_memories_summary_octet_length'
       AND conrelid = 'memories'::regclass
  ) THEN
    ALTER TABLE memories
      ADD CONSTRAINT chk_memories_summary_octet_length
      CHECK (summary IS NULL OR octet_length(summary) <= 10240);
  END IF;
END $$;

DO $$ BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
     WHERE conname = 'chk_projects_name_octet_length'
       AND conrelid = 'projects'::regclass
  ) THEN
    ALTER TABLE projects
      ADD CONSTRAINT chk_projects_name_octet_length
      CHECK (octet_length(name) <= 1024);
  END IF;
END $$;

DO $$ BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
     WHERE conname = 'chk_projects_description_octet_length'
       AND conrelid = 'projects'::regclass
  ) THEN
    ALTER TABLE projects
      ADD CONSTRAINT chk_projects_description_octet_length
      CHECK (description IS NULL OR octet_length(description) <= 102400);
  END IF;
END $$;

DO $$ BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
     WHERE conname = 'chk_tasks_title_octet_length'
       AND conrelid = 'tasks'::regclass
  ) THEN
    ALTER TABLE tasks
      ADD CONSTRAINT chk_tasks_title_octet_length
      CHECK (octet_length(title) <= 1024);
  END IF;
END $$;

DO $$ BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
     WHERE conname = 'chk_tasks_description_octet_length'
       AND conrelid = 'tasks'::regclass
  ) THEN
    ALTER TABLE tasks
      ADD CONSTRAINT chk_tasks_description_octet_length
      CHECK (description IS NULL OR octet_length(description) <= 102400);
  END IF;
END $$;

DO $$ BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
     WHERE conname = 'chk_workspace_groups_name_octet_length'
       AND conrelid = 'workspace_groups'::regclass
  ) THEN
    ALTER TABLE workspace_groups
      ADD CONSTRAINT chk_workspace_groups_name_octet_length
      CHECK (octet_length(name) <= 1024);
  END IF;
END $$;

------------------------------------------------------------------------
-- Active-row uniqueness
------------------------------------------------------------------------

ALTER TABLE workspaces DROP CONSTRAINT IF EXISTS uq_workspace_path;
ALTER TABLE workspaces DROP CONSTRAINT IF EXISTS uq_workspace_github;
ALTER TABLE memory_categories DROP CONSTRAINT IF EXISTS uq_category_name_ws;
DROP INDEX IF EXISTS uq_global_category_name;

CREATE UNIQUE INDEX IF NOT EXISTS uq_workspace_path_active
    ON workspaces (path)
    WHERE path IS NOT NULL AND deleted_at IS NULL;

CREATE UNIQUE INDEX IF NOT EXISTS uq_workspace_github_active
    ON workspaces (gh_owner, gh_repo)
    WHERE gh_owner IS NOT NULL AND gh_repo IS NOT NULL AND deleted_at IS NULL;

CREATE UNIQUE INDEX IF NOT EXISTS uq_category_name_ws_active
    ON memory_categories (workspace_id, name, parent_id)
    WHERE workspace_id IS NOT NULL AND deleted_at IS NULL;

CREATE UNIQUE INDEX IF NOT EXISTS uq_global_category_name_active
    ON memory_categories (name, parent_id)
    WHERE workspace_id IS NULL AND deleted_at IS NULL;

------------------------------------------------------------------------
-- Audit log
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS audit_log (
    id          UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    entity_type TEXT NOT NULL,
    entity_id   TEXT NOT NULL,
    action      audit_action_enum NOT NULL,
    old_values  JSONB,
    new_values  JSONB,
    request_id  TEXT,
    changed_at  TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE OR REPLACE FUNCTION hmem_jsonb_without_keys(input JSONB, keys TEXT[])
RETURNS JSONB AS $$
DECLARE
  result JSONB := coalesce(input, '{}'::jsonb);
  key TEXT;
BEGIN
  FOREACH key IN ARRAY coalesce(keys, ARRAY[]::TEXT[]) LOOP
    result := result - key;
  END LOOP;
  RETURN result;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

CREATE OR REPLACE FUNCTION hmem_audit_change()
RETURNS TRIGGER AS $$
DECLARE
  ignored_fields TEXT[] := ARRAY[]::TEXT[];
  key_columns TEXT[] := string_to_array(COALESCE(NULLIF(TG_ARGV[1], ''), 'id'), ',');
  key_name TEXT;
  old_row JSONB;
  new_row JSONB;
  entity_identifier TEXT;
  key_data JSONB := '{}'::jsonb;
  key_value TEXT;
  idx INTEGER;
BEGIN
  IF TG_NARGS > 2 THEN
    FOR idx IN 2..TG_NARGS - 1 LOOP
      ignored_fields := array_append(ignored_fields, TG_ARGV[idx]);
    END LOOP;
  END IF;

  CASE TG_OP
    WHEN 'INSERT' THEN
      old_row := NULL;
      new_row := hmem_jsonb_without_keys(to_jsonb(NEW), ignored_fields);
    WHEN 'UPDATE' THEN
      old_row := hmem_jsonb_without_keys(to_jsonb(OLD), ignored_fields);
      new_row := hmem_jsonb_without_keys(to_jsonb(NEW), ignored_fields);
      IF old_row = new_row THEN
        RETURN NULL;
      END IF;
    WHEN 'DELETE' THEN
      old_row := hmem_jsonb_without_keys(to_jsonb(OLD), ignored_fields);
      new_row := NULL;
    ELSE
      RETURN NULL;
  END CASE;

  FOREACH key_name IN ARRAY key_columns LOOP
    key_name := btrim(key_name);
    IF key_name <> '' THEN
      key_value := coalesce(new_row ->> key_name, old_row ->> key_name);
      IF key_value IS NOT NULL THEN
        key_data := key_data || jsonb_build_object(key_name, key_value);
      END IF;
    END IF;
  END LOOP;

  entity_identifier := CASE
    WHEN array_length(key_columns, 1) = 1 THEN
      coalesce(new_row ->> btrim(key_columns[1]), old_row ->> btrim(key_columns[1]), coalesce(new_row, old_row)::text)
    WHEN key_data <> '{}'::jsonb THEN
      key_data::text
    ELSE
      coalesce(new_row, old_row)::text
  END;

  INSERT INTO audit_log (entity_type, entity_id, action, old_values, new_values, request_id)
  VALUES (
    TG_ARGV[0],
    entity_identifier,
    CASE TG_OP
      WHEN 'INSERT' THEN 'create'::audit_action_enum
      WHEN 'UPDATE' THEN 'update'::audit_action_enum
      ELSE 'delete'::audit_action_enum
    END,
    old_row,
    new_row,
    NULLIF(current_setting('hmem.request_id', true), '')
  );

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trg_workspaces_audit ON workspaces;
CREATE TRIGGER trg_workspaces_audit
    AFTER INSERT OR UPDATE OR DELETE ON workspaces
    FOR EACH ROW EXECUTE FUNCTION hmem_audit_change('workspace', 'id', 'updated_at');

DROP TRIGGER IF EXISTS trg_memory_categories_audit ON memory_categories;
CREATE TRIGGER trg_memory_categories_audit
    AFTER INSERT OR UPDATE OR DELETE ON memory_categories
    FOR EACH ROW EXECUTE FUNCTION hmem_audit_change('memory_category', 'id');

DROP TRIGGER IF EXISTS trg_memories_audit ON memories;
CREATE TRIGGER trg_memories_audit
    AFTER INSERT OR UPDATE OR DELETE ON memories
    FOR EACH ROW EXECUTE FUNCTION hmem_audit_change(
      'memory',
      'id',
      'updated_at',
      'search_vector',
      'last_accessed_at',
      'access_count'
    );

DROP TRIGGER IF EXISTS trg_memory_tags_audit ON memory_tags;
CREATE TRIGGER trg_memory_tags_audit
    AFTER INSERT OR UPDATE OR DELETE ON memory_tags
    FOR EACH ROW EXECUTE FUNCTION hmem_audit_change('memory_tag', 'memory_id,tag');

DROP TRIGGER IF EXISTS trg_memory_category_links_audit ON memory_category_links;
CREATE TRIGGER trg_memory_category_links_audit
    AFTER INSERT OR UPDATE OR DELETE ON memory_category_links
    FOR EACH ROW EXECUTE FUNCTION hmem_audit_change('memory_category_link', 'memory_id,category_id');

DROP TRIGGER IF EXISTS trg_memory_links_audit ON memory_links;
CREATE TRIGGER trg_memory_links_audit
    AFTER INSERT OR UPDATE OR DELETE ON memory_links
    FOR EACH ROW EXECUTE FUNCTION hmem_audit_change('memory_link', 'source_id,target_id,relation_type');

DROP TRIGGER IF EXISTS trg_projects_audit ON projects;
CREATE TRIGGER trg_projects_audit
    AFTER INSERT OR UPDATE OR DELETE ON projects
    FOR EACH ROW EXECUTE FUNCTION hmem_audit_change('project', 'id', 'updated_at');

DROP TRIGGER IF EXISTS trg_tasks_audit ON tasks;
CREATE TRIGGER trg_tasks_audit
    AFTER INSERT OR UPDATE OR DELETE ON tasks
    FOR EACH ROW EXECUTE FUNCTION hmem_audit_change('task', 'id', 'updated_at');

DROP TRIGGER IF EXISTS trg_task_dependencies_audit ON task_dependencies;
CREATE TRIGGER trg_task_dependencies_audit
    AFTER INSERT OR UPDATE OR DELETE ON task_dependencies
    FOR EACH ROW EXECUTE FUNCTION hmem_audit_change('task_dependency', 'task_id,depends_on_id');

DROP TRIGGER IF EXISTS trg_project_memory_links_audit ON project_memory_links;
CREATE TRIGGER trg_project_memory_links_audit
    AFTER INSERT OR UPDATE OR DELETE ON project_memory_links
    FOR EACH ROW EXECUTE FUNCTION hmem_audit_change('project_memory_link', 'project_id,memory_id');

DROP TRIGGER IF EXISTS trg_task_memory_links_audit ON task_memory_links;
CREATE TRIGGER trg_task_memory_links_audit
    AFTER INSERT OR UPDATE OR DELETE ON task_memory_links
    FOR EACH ROW EXECUTE FUNCTION hmem_audit_change('task_memory_link', 'task_id,memory_id');

DROP TRIGGER IF EXISTS trg_cleanup_policies_audit ON cleanup_policies;
CREATE TRIGGER trg_cleanup_policies_audit
    AFTER INSERT OR UPDATE OR DELETE ON cleanup_policies
    FOR EACH ROW EXECUTE FUNCTION hmem_audit_change('cleanup_policy', 'id', 'updated_at');

DROP TRIGGER IF EXISTS trg_workspace_groups_audit ON workspace_groups;
CREATE TRIGGER trg_workspace_groups_audit
    AFTER INSERT OR UPDATE OR DELETE ON workspace_groups
    FOR EACH ROW EXECUTE FUNCTION hmem_audit_change('workspace_group', 'id', 'updated_at');

DROP TRIGGER IF EXISTS trg_workspace_group_members_audit ON workspace_group_members;
CREATE TRIGGER trg_workspace_group_members_audit
    AFTER INSERT OR UPDATE OR DELETE ON workspace_group_members
    FOR EACH ROW EXECUTE FUNCTION hmem_audit_change('workspace_group_member', 'group_id,workspace_id');

------------------------------------------------------------------------
-- Additional indexes
------------------------------------------------------------------------

CREATE INDEX IF NOT EXISTS idx_memories_workspace_pinned
    ON memories (workspace_id, pinned);

CREATE INDEX IF NOT EXISTS idx_memory_links_source_relation
    ON memory_links (source_id, relation_type);

CREATE INDEX IF NOT EXISTS idx_project_memory_links_memory
    ON project_memory_links (memory_id);

CREATE INDEX IF NOT EXISTS idx_task_memory_links_memory
    ON task_memory_links (memory_id);

CREATE INDEX IF NOT EXISTS idx_task_dependencies_depends_on
    ON task_dependencies (depends_on_id);

CREATE INDEX IF NOT EXISTS idx_audit_log_entity_changed_at
    ON audit_log (entity_type, entity_id, changed_at DESC);

CREATE INDEX IF NOT EXISTS idx_audit_log_request_id
    ON audit_log (request_id)
    WHERE request_id IS NOT NULL;

CREATE INDEX IF NOT EXISTS idx_audit_log_changed_at
    ON audit_log (changed_at DESC);

COMMIT;