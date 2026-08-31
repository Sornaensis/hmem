BEGIN;

-- Projection is durable snapshot geometry.  Defaults preserve the exact
-- legacy wire contract for clients that omit snapshot_profile.
ALTER TABLE change_stream_snapshot_sessions
  ADD COLUMN snapshot_profile TEXT NOT NULL DEFAULT 'full_v1',
  ADD CONSTRAINT chk_change_stream_snapshot_profile
    CHECK (snapshot_profile IN ('full_v1', 'workspace_shell_v1'));

COMMIT;
