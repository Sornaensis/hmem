BEGIN;

------------------------------------------------------------------------
-- Add search_vector columns to projects and tasks
------------------------------------------------------------------------

ALTER TABLE projects
  ADD COLUMN IF NOT EXISTS search_vector tsvector NOT NULL DEFAULT ''::tsvector;

ALTER TABLE tasks
  ADD COLUMN IF NOT EXISTS search_vector tsvector NOT NULL DEFAULT ''::tsvector;

------------------------------------------------------------------------
-- GIN indexes for full-text search
------------------------------------------------------------------------

CREATE INDEX IF NOT EXISTS idx_projects_search_vector
  ON projects USING GIN (search_vector);

CREATE INDEX IF NOT EXISTS idx_tasks_search_vector
  ON tasks USING GIN (search_vector);

------------------------------------------------------------------------
-- Trigger function: projects search_vector
-- Weights: name = A, description = B
------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION hmem_projects_search_vector()
RETURNS TRIGGER AS $$
BEGIN
  NEW.search_vector :=
      setweight(to_tsvector('english', coalesce(NEW.name, '')), 'A') ||
      setweight(to_tsvector('english', coalesce(NEW.description, '')), 'B');
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trg_projects_search_vector ON projects;
CREATE TRIGGER trg_projects_search_vector
    BEFORE INSERT OR UPDATE OF name, description ON projects
    FOR EACH ROW EXECUTE FUNCTION hmem_projects_search_vector();

------------------------------------------------------------------------
-- Trigger function: tasks search_vector
-- Weights: title = A, description = B
------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION hmem_tasks_search_vector()
RETURNS TRIGGER AS $$
BEGIN
  NEW.search_vector :=
      setweight(to_tsvector('english', coalesce(NEW.title, '')), 'A') ||
      setweight(to_tsvector('english', coalesce(NEW.description, '')), 'B');
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trg_tasks_search_vector ON tasks;
CREATE TRIGGER trg_tasks_search_vector
    BEFORE INSERT OR UPDATE OF title, description ON tasks
    FOR EACH ROW EXECUTE FUNCTION hmem_tasks_search_vector();

------------------------------------------------------------------------
-- Backfill existing rows by touching name/title to fire triggers
------------------------------------------------------------------------

UPDATE projects SET name = name WHERE deleted_at IS NULL;
UPDATE tasks SET title = title WHERE deleted_at IS NULL;

------------------------------------------------------------------------
-- Update audit triggers to exclude search_vector from change detection
------------------------------------------------------------------------

DROP TRIGGER IF EXISTS trg_projects_audit ON projects;
CREATE TRIGGER trg_projects_audit
    AFTER INSERT OR UPDATE OR DELETE ON projects
    FOR EACH ROW EXECUTE FUNCTION hmem_audit_change('project', 'id', 'updated_at', 'search_vector');

DROP TRIGGER IF EXISTS trg_tasks_audit ON tasks;
CREATE TRIGGER trg_tasks_audit
    AFTER INSERT OR UPDATE OR DELETE ON tasks
    FOR EACH ROW EXECUTE FUNCTION hmem_audit_change('task', 'id', 'updated_at', 'search_vector');

COMMIT;
