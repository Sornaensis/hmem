BEGIN;

-- Saved full task representations still carry the removed field, including
-- explicit null. Retire only those workspace sessions and every resume bearer
-- linked to them; page tokens and saved items cascade with session deletion.
WITH incompatible AS MATERIALIZED (
  SELECT s.session_hash
  FROM change_stream_snapshot_sessions s
  WHERE s.scope = 'workspace'
    AND s.snapshot_profile = 'full_v1'
    AND EXISTS (
      SELECT 1 FROM change_stream_snapshot_items i
      WHERE i.session_hash = s.session_hash
        AND i.item ->> 'kind' = 'task'
        AND coalesce((i.item -> 'data') ? 'due_at', false)
    )
), retired_resumes AS (
  DELETE FROM change_stream_resume_tokens r
  USING incompatible i
  WHERE r.session_hash = i.session_hash
  RETURNING r.token_hash
)
DELETE FROM change_stream_snapshot_sessions s
USING incompatible i
WHERE s.session_hash = i.session_hash;

DROP INDEX idx_tasks_due;
ALTER TABLE tasks DROP COLUMN due_at;

INSERT INTO schema_migrations (version, name)
VALUES (38, 'V038__remove_task_due.sql');

COMMIT;
