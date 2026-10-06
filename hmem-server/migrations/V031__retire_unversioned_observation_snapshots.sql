BEGIN;

-- A saved Observation without a content version cannot be made canonical by
-- attaching the current row's token to its historical content. Retire that
-- full snapshot instead, including every successor resume bearer in its
-- lineage. Page bearers and saved items cascade with the session deletion.
WITH incompatible AS MATERIALIZED (
  SELECT s.session_hash
  FROM change_stream_snapshot_sessions s
  WHERE s.snapshot_profile = 'full_v1'
    AND EXISTS (
      SELECT 1 FROM change_stream_snapshot_items i
      WHERE i.session_hash = s.session_hash
        AND i.item ->> 'kind' = 'observation'
        AND NOT coalesce((i.item -> 'data') ? 'content_version', false)
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

INSERT INTO schema_migrations (version, name)
VALUES (31, 'V031__retire_unversioned_observation_snapshots.sql');

COMMIT;
