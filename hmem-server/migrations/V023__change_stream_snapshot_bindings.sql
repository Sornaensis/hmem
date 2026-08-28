BEGIN;

-- A start key is client-supplied opaque retry material. Only its hash is
-- persisted; the same authenticated scope/key returns the original immutable
-- session after a lost start response. Page size is session state, not a
-- continuation preference, so a token cannot be used to alter page shape.
ALTER TABLE change_stream_snapshot_sessions
  ADD COLUMN page_size INTEGER,
  ADD COLUMN start_idempotency_hash BYTEA;

ALTER TABLE change_stream_snapshot_sessions
  ADD CONSTRAINT chk_change_stream_snapshot_page_size
  CHECK (page_size IS NULL OR page_size BETWEEN 1 AND 1000);

CREATE UNIQUE INDEX uq_change_stream_snapshot_start_idempotency
  ON change_stream_snapshot_sessions (
    scope,
    coalesce(workspace_id, '00000000-0000-0000-0000-000000000000'::uuid),
    audience_kind,
    audience_key,
    coalesce(audience_user_id, '00000000-0000-0000-0000-000000000000'::uuid),
    start_idempotency_hash
  )
  WHERE start_idempotency_hash IS NOT NULL;

COMMIT;
