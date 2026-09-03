-- A workspace rename is visible both to clients scoped to that workspace and
-- to catalogue consumers.  The workspace lock must precede the global lock.
BEGIN;

CREATE OR REPLACE FUNCTION hmem_change_stream_workspace_change()
RETURNS TRIGGER AS $$
DECLARE old_clean JSONB;
DECLARE new_clean JSONB;
DECLARE action TEXT;
DECLARE workspace_invalidations JSONB;
DECLARE global_invalidations JSONB;
BEGIN
  old_clean := CASE WHEN TG_OP = 'INSERT' THEN NULL ELSE to_jsonb(OLD) - 'updated_at' END;
  new_clean := CASE WHEN TG_OP = 'DELETE' THEN NULL ELSE to_jsonb(NEW) - 'updated_at' END;
  IF TG_OP = 'UPDATE' AND old_clean = new_clean THEN
    RETURN NULL;
  END IF;

  IF TG_OP = 'INSERT' THEN action := 'created';
  ELSIF TG_OP = 'DELETE' THEN action := 'deleted';
  ELSIF (old_clean ->> 'deleted_at') IS NULL AND (new_clean ->> 'deleted_at') IS NOT NULL THEN action := 'deleted';
  ELSIF (old_clean ->> 'deleted_at') IS NOT NULL AND (new_clean ->> 'deleted_at') IS NULL THEN action := 'restored';
  ELSE action := 'updated';
  END IF;

  global_invalidations := jsonb_build_array(
    jsonb_build_object('kind', 'entity', 'target', 'workspace:' || coalesce(new_clean ->> 'id', old_clean ->> 'id')),
    jsonb_build_object('kind', 'catalogue', 'target', 'workspace-catalog'),
    jsonb_build_object('kind', 'collection', 'target', 'workspace-groups')
  );

  IF TG_OP = 'UPDATE' AND NEW.deleted_at IS NULL AND OLD.name IS DISTINCT FROM NEW.name THEN
    PERFORM hmem_change_stream_lock('workspace', NEW.id);
    PERFORM hmem_change_stream_lock('global', NULL);
    workspace_invalidations := jsonb_build_array(
      jsonb_build_object('kind', 'entity', 'target', 'workspace:' || NEW.id::text)
    );
    PERFORM hmem_change_stream_record('workspace', NEW.id, 'workspace', NEW.id::text, 'updated', workspace_invalidations);
    PERFORM hmem_change_stream_record('global', NULL, 'workspace', NEW.id::text, 'updated', global_invalidations);
  ELSE
    PERFORM hmem_change_stream_record('global', NULL, 'workspace', coalesce(new_clean ->> 'id', old_clean ->> 'id'), action, global_invalidations);
  END IF;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trg_change_stream_workspaces ON workspaces;
CREATE TRIGGER trg_change_stream_workspaces
  AFTER INSERT OR UPDATE OR DELETE ON workspaces
  FOR EACH ROW EXECUTE FUNCTION hmem_change_stream_workspace_change();

COMMIT;
