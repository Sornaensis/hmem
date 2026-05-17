BEGIN;

------------------------------------------------------------------------
-- Explicit memory creation links
------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION hmem_validate_memory_creation_link(memory_id_to_check UUID)
RETURNS VOID AS $$
DECLARE
  mem_workspace_id UUID;
  valid_project_count INTEGER := 0;
  valid_project_ids UUID[] := ARRAY[]::UUID[];
  valid_task_count INTEGER := 0;
  valid_task_ids UUID[] := ARRAY[]::UUID[];
  cross_workspace_count INTEGER := 0;
  cross_workspace_ids UUID[] := ARRAY[]::UUID[];
  valid_total INTEGER := 0;
BEGIN
  SELECT workspace_id
    INTO mem_workspace_id
    FROM memories
   WHERE id = memory_id_to_check
     AND deleted_at IS NULL;

  -- Purged or soft-deleted memories are outside the active creation-link
  -- invariant. Restore operations validate because NEW.deleted_at is NULL.
  IF NOT FOUND THEN
    RETURN;
  END IF;

  WITH valid_projects AS (
    SELECT p.id
      FROM project_memory_links pml
      JOIN projects p ON p.id = pml.project_id
     WHERE pml.memory_id = memory_id_to_check
       AND p.deleted_at IS NULL
       AND p.workspace_id = mem_workspace_id
  )
  SELECT count(*)::INTEGER,
         coalesce(ARRAY(SELECT id FROM valid_projects ORDER BY id LIMIT 5), ARRAY[]::UUID[])
    INTO valid_project_count, valid_project_ids
    FROM valid_projects;

  WITH valid_tasks AS (
    SELECT t.id
      FROM task_memory_links tml
      JOIN tasks t ON t.id = tml.task_id
     WHERE tml.memory_id = memory_id_to_check
       AND t.deleted_at IS NULL
       AND t.workspace_id = mem_workspace_id
  )
  SELECT count(*)::INTEGER,
         coalesce(ARRAY(SELECT id FROM valid_tasks ORDER BY id LIMIT 5), ARRAY[]::UUID[])
    INTO valid_task_count, valid_task_ids
    FROM valid_tasks;

  WITH cross_workspace_links AS (
    SELECT p.id
      FROM project_memory_links pml
      JOIN projects p ON p.id = pml.project_id
     WHERE pml.memory_id = memory_id_to_check
       AND p.workspace_id <> mem_workspace_id
    UNION
    SELECT t.id
      FROM task_memory_links tml
      JOIN tasks t ON t.id = tml.task_id
     WHERE tml.memory_id = memory_id_to_check
       AND t.workspace_id <> mem_workspace_id
  )
  SELECT count(*)::INTEGER,
         coalesce(ARRAY(SELECT id FROM cross_workspace_links ORDER BY id LIMIT 5), ARRAY[]::UUID[])
    INTO cross_workspace_count, cross_workspace_ids
    FROM cross_workspace_links;

  IF cross_workspace_count > 0 THEN
    RAISE EXCEPTION USING
      ERRCODE = 'HM304',
      MESSAGE = 'Memory link target must belong to the same workspace as the memory.',
      DETAIL = jsonb_build_object(
        'memory_id', memory_id_to_check,
        'target_count', cross_workspace_count,
        'target_ids', cross_workspace_ids
      )::text,
      HINT = 'Create the memory in the same workspace as its project or task target.';
  END IF;

  valid_total := valid_project_count + valid_task_count;

  IF valid_total = 0 THEN
    RAISE EXCEPTION USING
      ERRCODE = 'HM301',
      MESSAGE = 'Memory creation requires at least one project or task link.',
      DETAIL = jsonb_build_object('memory_id', memory_id_to_check)::text,
      HINT = 'Provide at least one project_id or task_id when creating a memory.';
  END IF;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION hmem_check_memory_creation_link_from_memory()
RETURNS TRIGGER AS $$
BEGIN
  PERFORM hmem_validate_memory_creation_link(NEW.id);
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION hmem_check_memory_creation_link_from_project_link()
RETURNS TRIGGER AS $$
BEGIN
  IF TG_OP = 'DELETE' THEN
    PERFORM hmem_validate_memory_creation_link(OLD.memory_id);
  ELSIF TG_OP = 'UPDATE' THEN
    IF OLD.memory_id IS DISTINCT FROM NEW.memory_id THEN
      PERFORM hmem_validate_memory_creation_link(OLD.memory_id);
    END IF;
    PERFORM hmem_validate_memory_creation_link(NEW.memory_id);
  ELSE
    PERFORM hmem_validate_memory_creation_link(NEW.memory_id);
  END IF;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION hmem_check_memory_creation_link_from_task_link()
RETURNS TRIGGER AS $$
BEGIN
  IF TG_OP = 'DELETE' THEN
    PERFORM hmem_validate_memory_creation_link(OLD.memory_id);
  ELSIF TG_OP = 'UPDATE' THEN
    IF OLD.memory_id IS DISTINCT FROM NEW.memory_id THEN
      PERFORM hmem_validate_memory_creation_link(OLD.memory_id);
    END IF;
    PERFORM hmem_validate_memory_creation_link(NEW.memory_id);
  ELSE
    PERFORM hmem_validate_memory_creation_link(NEW.memory_id);
  END IF;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION hmem_check_memory_creation_link_from_project_target()
RETURNS TRIGGER AS $$
DECLARE
  linked_memory_id UUID;
BEGIN
  FOR linked_memory_id IN
    SELECT memory_id
      FROM project_memory_links
     WHERE project_id = NEW.id
  LOOP
    PERFORM hmem_validate_memory_creation_link(linked_memory_id);
  END LOOP;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION hmem_check_memory_creation_link_from_task_target()
RETURNS TRIGGER AS $$
DECLARE
  linked_memory_id UUID;
BEGIN
  FOR linked_memory_id IN
    SELECT memory_id
      FROM task_memory_links
     WHERE task_id = NEW.id
  LOOP
    PERFORM hmem_validate_memory_creation_link(linked_memory_id);
  END LOOP;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trg_memory_creation_link_required ON memories;
CREATE CONSTRAINT TRIGGER trg_memory_creation_link_required
  AFTER INSERT OR UPDATE OF workspace_id, deleted_at ON memories
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION hmem_check_memory_creation_link_from_memory();

DROP TRIGGER IF EXISTS trg_project_memory_link_exactly_one ON project_memory_links;
DROP TRIGGER IF EXISTS trg_project_memory_link_at_least_one ON project_memory_links;
CREATE CONSTRAINT TRIGGER trg_project_memory_link_at_least_one
  AFTER INSERT OR UPDATE OR DELETE ON project_memory_links
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION hmem_check_memory_creation_link_from_project_link();

DROP TRIGGER IF EXISTS trg_task_memory_link_exactly_one ON task_memory_links;
DROP TRIGGER IF EXISTS trg_task_memory_link_at_least_one ON task_memory_links;
CREATE CONSTRAINT TRIGGER trg_task_memory_link_at_least_one
  AFTER INSERT OR UPDATE OR DELETE ON task_memory_links
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION hmem_check_memory_creation_link_from_task_link();

DROP TRIGGER IF EXISTS trg_project_memory_target_valid ON projects;
CREATE CONSTRAINT TRIGGER trg_project_memory_target_valid
  AFTER UPDATE OF workspace_id, deleted_at ON projects
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION hmem_check_memory_creation_link_from_project_target();

DROP TRIGGER IF EXISTS trg_task_memory_target_valid ON tasks;
CREATE CONSTRAINT TRIGGER trg_task_memory_target_valid
  AFTER UPDATE OF workspace_id, deleted_at ON tasks
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION hmem_check_memory_creation_link_from_task_target();

COMMIT;
