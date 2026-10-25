-- Bound the server-owned abandoned-session metadata scan before inspecting
-- linked tasks. Lifecycle and evidence guards remain unchanged.
BEGIN;
SELECT set_config('hmem.change_cause', 'migration', true);
CREATE INDEX sessions_active_last_seen_id_idx ON sessions(last_seen_at, id)
WHERE state = 'active';
INSERT INTO schema_migrations(version,name) VALUES (47,'V047__idle_session_scan.sql');
COMMIT;
