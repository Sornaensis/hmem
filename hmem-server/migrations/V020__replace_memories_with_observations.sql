BEGIN;

------------------------------------------------------------------------
-- Destructive replacement of legacy memory persistence
------------------------------------------------------------------------
--
-- Memories are intentionally not converted: observations require immutable
-- repository provenance that legacy rows do not possess.  Delete through the
-- legacy tables first so their audit triggers run, then remove every
-- memory-family audit record afterwards; this ensures the delete audit rows
-- cannot retain the content this migration is required to purge.  The
-- V013 memory-link checks are deferred constraint triggers, so remove them
-- before deletes to avoid pending trigger events blocking the table drops.

DROP TRIGGER IF EXISTS trg_memory_creation_link_required ON memories;
DROP TRIGGER IF EXISTS trg_project_memory_link_at_least_one ON project_memory_links;
DROP TRIGGER IF EXISTS trg_task_memory_link_at_least_one ON task_memory_links;
DROP TRIGGER IF EXISTS trg_project_memory_target_valid ON projects;
DROP TRIGGER IF EXISTS trg_task_memory_target_valid ON tasks;

DELETE FROM memories;
DELETE FROM cleanup_policies;
DELETE FROM memory_categories;

-- Only audit entities emitted by the removed memory family are purged.
-- Do not use a generic JSON "content" predicate: unrelated audit entities
-- may legitimately contain a field with that name and must be retained.
DELETE FROM audit_log
 WHERE entity_type IN (
   'memory',
   'memory_tag',
   'memory_category',
   'memory_category_link',
   'memory_link',
   'project_memory_link',
   'task_memory_link',
   'cleanup_policy'
 );

-- Saved views for memory entities cannot be executed once the legacy tables
-- are removed.  Keep unrelated project, task, and activity views intact.
DELETE FROM saved_views
 WHERE entity_type IN ('memory_search', 'memory_list');

ALTER TABLE saved_views DROP CONSTRAINT IF EXISTS chk_saved_views_entity_type;
ALTER TABLE saved_views
  ADD CONSTRAINT chk_saved_views_entity_type
  CHECK (entity_type IN (
    'observation_search', 'observation_list', 'project_list', 'task_list', 'activity'
  ));

-- Remove constraints on projects and tasks before removing the functions that
-- implemented memory-link validation.  Project and task data itself remains.
DROP TRIGGER IF EXISTS trg_project_memory_target_valid ON projects;
DROP TRIGGER IF EXISTS trg_task_memory_target_valid ON tasks;
DROP TRIGGER IF EXISTS trg_task_soft_delete_cascade ON tasks;
DROP TRIGGER IF EXISTS trg_project_soft_delete_cascade ON projects;

DROP TABLE IF EXISTS task_memory_links CASCADE;
DROP TABLE IF EXISTS project_memory_links CASCADE;
DROP TABLE IF EXISTS memory_links CASCADE;
DROP TABLE IF EXISTS memory_category_links CASCADE;
DROP TABLE IF EXISTS memory_tags CASCADE;
DROP TABLE IF EXISTS cleanup_policies CASCADE;
DROP TABLE IF EXISTS memories CASCADE;
DROP TABLE IF EXISTS memory_categories CASCADE;

DROP FUNCTION IF EXISTS hmem_validate_memory_creation_link(UUID);
DROP FUNCTION IF EXISTS hmem_check_memory_creation_link_from_memory();
DROP FUNCTION IF EXISTS hmem_check_memory_creation_link_from_project_link();
DROP FUNCTION IF EXISTS hmem_check_memory_creation_link_from_task_link();
DROP FUNCTION IF EXISTS hmem_check_memory_creation_link_from_project_target();
DROP FUNCTION IF EXISTS hmem_check_memory_creation_link_from_task_target();
DROP FUNCTION IF EXISTS hmem_require_explicit_memory_type();
DROP FUNCTION IF EXISTS hmem_memories_search_vector();
DROP FUNCTION IF EXISTS hmem_memory_tags_reindex();
DROP FUNCTION IF EXISTS hmem_check_category_cycle();
DROP FUNCTION IF EXISTS hmem_soft_delete_memories_for_deleted_links(UUID[], UUID[], TIMESTAMPTZ);

DROP TYPE IF EXISTS relation_type_enum;
DROP TYPE IF EXISTS memory_type_enum;

------------------------------------------------------------------------
-- Remove memory coupling from task/project deletion and audit attribution
------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION hmem_cascade_task_soft_delete()
RETURNS TRIGGER AS $$
DECLARE
  affected_task_ids UUID[] := ARRAY[]::UUID[];
BEGIN
  IF NEW.deleted_at IS NOT NULL AND OLD.deleted_at IS NULL THEN
    WITH RECURSIVE task_tree(id) AS (
      SELECT NEW.id
      UNION
      SELECT child.id
        FROM tasks child
        JOIN task_tree parent ON child.parent_id = parent.id
       WHERE child.deleted_at IS NULL
    ),
    updated_tasks AS (
      UPDATE tasks task_to_update
         SET deleted_at = NEW.deleted_at
        FROM task_tree
       WHERE task_to_update.id = task_tree.id
         AND task_to_update.id <> NEW.id
         AND task_to_update.deleted_at IS NULL
       RETURNING task_to_update.id
    ),
    all_tasks AS (
      SELECT NEW.id AS id
      UNION
      SELECT id FROM updated_tasks
    )
    SELECT coalesce(array_agg(id), ARRAY[]::UUID[])
      INTO affected_task_ids
      FROM all_tasks;

    DELETE FROM task_dependencies
     WHERE task_id = ANY(affected_task_ids)
        OR depends_on_id = ANY(affected_task_ids);
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_task_soft_delete_cascade
  AFTER UPDATE OF deleted_at ON tasks
  FOR EACH ROW EXECUTE FUNCTION hmem_cascade_task_soft_delete();

CREATE OR REPLACE FUNCTION hmem_cascade_project_soft_delete()
RETURNS TRIGGER AS $$
DECLARE
  affected_project_ids UUID[] := ARRAY[]::UUID[];
  affected_task_ids UUID[] := ARRAY[]::UUID[];
BEGIN
  IF NEW.deleted_at IS NOT NULL AND OLD.deleted_at IS NULL THEN
    WITH RECURSIVE project_tree(id) AS (
      SELECT NEW.id
      UNION
      SELECT child.id
        FROM projects child
        JOIN project_tree parent ON child.parent_id = parent.id
       WHERE child.deleted_at IS NULL
    ),
    updated_projects AS (
      UPDATE projects project_to_update
         SET deleted_at = NEW.deleted_at
        FROM project_tree
       WHERE project_to_update.id = project_tree.id
         AND project_to_update.id <> NEW.id
         AND project_to_update.deleted_at IS NULL
       RETURNING project_to_update.id
    ),
    all_projects AS (
      SELECT NEW.id AS id
      UNION
      SELECT id FROM updated_projects
    )
    SELECT coalesce(array_agg(id), ARRAY[]::UUID[])
      INTO affected_project_ids
      FROM all_projects;

    WITH RECURSIVE task_tree(id) AS (
      SELECT task_row.id
        FROM tasks task_row
       WHERE task_row.project_id = ANY(affected_project_ids)
         AND task_row.deleted_at IS NULL
      UNION
      SELECT child.id
        FROM tasks child
        JOIN task_tree parent ON child.parent_id = parent.id
       WHERE child.deleted_at IS NULL
    ),
    updated_tasks AS (
      UPDATE tasks task_to_update
         SET deleted_at = NEW.deleted_at
        FROM task_tree
       WHERE task_to_update.id = task_tree.id
         AND task_to_update.deleted_at IS NULL
       RETURNING task_to_update.id
    )
    SELECT coalesce(array_agg(id), ARRAY[]::UUID[])
      INTO affected_task_ids
      FROM updated_tasks;

    DELETE FROM task_dependencies
     WHERE task_id = ANY(affected_task_ids)
        OR depends_on_id = ANY(affected_task_ids);
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_project_soft_delete_cascade
  AFTER UPDATE OF deleted_at ON projects
  FOR EACH ROW EXECUTE FUNCTION hmem_cascade_project_soft_delete();

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
  current_workspace TEXT;
  current_actor_type TEXT;
  row_workspace TEXT;
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

  current_workspace := NULLIF(current_setting('hmem.workspace_id', true), '');
  current_actor_type := NULLIF(current_setting('hmem.actor_type', true), '');
  row_workspace := coalesce(new_row ->> 'workspace_id', old_row ->> 'workspace_id');

  IF current_workspace IS NULL AND TG_ARGV[0] = 'workspace' THEN
    current_workspace := coalesce(new_row ->> 'id', old_row ->> 'id');
  END IF;

  IF current_workspace IS NULL THEN
    current_workspace := row_workspace;
  END IF;

  IF current_workspace IS NULL AND TG_TABLE_NAME = 'task_dependencies' THEN
    SELECT t.workspace_id::text INTO current_workspace
      FROM tasks t
     WHERE t.id = COALESCE((new_row ->> 'task_id')::uuid, (old_row ->> 'task_id')::uuid);
  END IF;

  INSERT INTO audit_log (
    entity_type, entity_id, action, old_values, new_values, request_id,
    workspace_id, actor_type, actor_id, actor_label
  )
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
    NULLIF(current_setting('hmem.request_id', true), ''),
    CASE WHEN current_workspace IS NULL THEN NULL ELSE current_workspace::UUID END,
    CASE WHEN current_actor_type IS NULL THEN NULL ELSE current_actor_type::actor_type_enum END,
    NULLIF(current_setting('hmem.actor_id', true), ''),
    NULLIF(current_setting('hmem.actor_label', true), '')
  );

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

------------------------------------------------------------------------
-- Provenance-bound observations
------------------------------------------------------------------------

CREATE TYPE observation_subject_kind AS ENUM ('file', 'glob');

CREATE TABLE observations (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  workspace_id UUID NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  subject_kind observation_subject_kind NOT NULL,
  subject TEXT NOT NULL,
  git_sha TEXT NOT NULL,
  content TEXT NOT NULL,
  search_vector TSVECTOR NOT NULL DEFAULT ''::tsvector,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT chk_observations_subject_octet_length
    CHECK (octet_length(subject) BETWEEN 1 AND 4096),
  CONSTRAINT chk_observations_content_octet_length
    CHECK (octet_length(content) BETWEEN 1 AND 524288),
  CONSTRAINT chk_observations_git_sha
    CHECK (git_sha ~ '^[0-9a-f]{40}$')
);

CREATE OR REPLACE FUNCTION hmem_observations_search_vector()
RETURNS TRIGGER AS $$
BEGIN
  NEW.search_vector := to_tsvector('simple', NEW.subject || ' ' || NEW.content);
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION hmem_enforce_observation_provenance_immutable()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.workspace_id IS DISTINCT FROM OLD.workspace_id
     OR NEW.subject_kind IS DISTINCT FROM OLD.subject_kind
     OR NEW.subject IS DISTINCT FROM OLD.subject
     OR NEW.git_sha IS DISTINCT FROM OLD.git_sha THEN
    RAISE EXCEPTION USING
      ERRCODE = 'HM401',
      MESSAGE = 'Observation provenance is immutable.',
      HINT = 'Create a new observation for a different workspace, subject, or Git revision.';
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_observations_search_vector
  BEFORE INSERT OR UPDATE OF subject, content ON observations
  FOR EACH ROW EXECUTE FUNCTION hmem_observations_search_vector();

CREATE TRIGGER trg_observations_provenance_immutable
  BEFORE UPDATE OF workspace_id, subject_kind, subject, git_sha ON observations
  FOR EACH ROW EXECUTE FUNCTION hmem_enforce_observation_provenance_immutable();

CREATE TRIGGER trg_observations_updated_at
  BEFORE UPDATE ON observations
  FOR EACH ROW EXECUTE FUNCTION hmem_set_updated_at();

CREATE TRIGGER trg_observations_audit
  AFTER INSERT OR UPDATE OR DELETE ON observations
  FOR EACH ROW EXECUTE FUNCTION hmem_audit_change(
    'observation', 'id', 'updated_at', 'search_vector', 'embedding'
  );

CREATE INDEX idx_observations_workspace_git_sha
  ON observations (workspace_id, git_sha);
CREATE INDEX idx_observations_workspace_subject_kind_subject
  ON observations (workspace_id, subject_kind, subject);
CREATE INDEX idx_observations_workspace_git_sha_subject
  ON observations (workspace_id, git_sha, subject_kind, subject);
CREATE INDEX idx_observations_search
  ON observations USING gin (search_vector);

-- pgvector is optional.  Do not require it for a fresh database, but provide
-- the same 1536-dimensional cosine-search contract whenever it is installed.
DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'vector') THEN
    ALTER TABLE observations ADD COLUMN embedding vector(1536);
    CREATE INDEX idx_observations_embedding
      ON observations USING hnsw (embedding vector_cosine_ops);
  END IF;
END;
$$;

-- Migration files own their transaction boundaries.  Record V020 before
-- committing so the destructive schema transition and migration ledger are
-- indivisible; the runner's idempotent bookkeeping insert then becomes a no-op.
INSERT INTO schema_migrations (version, name)
VALUES (20, 'V020__replace_memories_with_observations.sql');

COMMIT;
