BEGIN;
SELECT set_config('hmem.change_cause', 'migration', true);

-- Check only new sessions, after their same-transaction initial link exists.
-- Historical orphans and later task lifecycle changes are left untouched.
CREATE FUNCTION hmem_session_initial_task_guard() RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  PERFORM t.id FROM task_sessions link
    JOIN tasks t ON t.id=link.task_id AND t.workspace_id=link.workspace_id
    JOIN workspaces w ON w.id=t.workspace_id
    WHERE link.session_id=NEW.id AND link.workspace_id=NEW.workspace_id
      AND t.deleted_at IS NULL AND w.deleted_at IS NULL AND t.status='in_progress'
    LIMIT 1 FOR SHARE OF t;
  IF NOT FOUND THEN
    RAISE EXCEPTION USING ERRCODE='23514', CONSTRAINT='session_requires_in_progress_task',
      MESSAGE='A new session must be created with a same-workspace task whose status is in_progress.';
  END IF;
  RETURN NULL;
END;
$$;
CREATE CONSTRAINT TRIGGER session_initial_task_guard
  AFTER INSERT ON sessions DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION hmem_session_initial_task_guard();

INSERT INTO schema_migrations(version,name) VALUES (45,'V045__session_initial_task.sql');
COMMIT;
