BEGIN;

-- Every pre-cutover workspace watermark can replay old Observation envelopes,
-- including shell/empty snapshots and resume bearers without a session link.
-- Remove all roots and successors before cascading snapshot pages/items.
-- Global catalogue streams cannot replay workspace Observation records and
-- retain their compatible catalogue envelopes and bearers.
DELETE FROM change_stream_resume_tokens WHERE scope = 'workspace';
DELETE FROM change_stream_snapshot_sessions WHERE scope = 'workspace';

INSERT INTO schema_migrations (version, name)
VALUES (33, 'V033__retire_pre_revision_observation_sessions.sql');

COMMIT;
