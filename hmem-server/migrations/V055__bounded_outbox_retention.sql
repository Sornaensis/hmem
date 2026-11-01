BEGIN;

-- One scope per transaction avoids multi-scope lock inversions. A caller must
-- apply a finite statement/lock timeout. Never jump over a retained cursor,
-- even if imported or corrected timestamps are not monotonic.
CREATE FUNCTION hmem_change_stream_prune_scope(
  p_scope TEXT, p_workspace_id UUID, p_cutoff TIMESTAMPTZ, p_limit INTEGER
) RETURNS BIGINT AS $$
DECLARE removed BIGINT;
BEGIN
  IF p_scope IS NULL OR p_scope NOT IN ('workspace', 'global')
     OR (p_scope = 'workspace') <> (p_workspace_id IS NOT NULL)
     OR p_cutoff IS NULL OR p_limit IS NULL OR p_limit NOT BETWEEN 1 AND 1000 THEN
    RAISE EXCEPTION 'invalid bounded outbox retention request';
  END IF;
  PERFORM hmem_change_stream_lock(p_scope, p_workspace_id);
  WITH candidates AS MATERIALIZED (
    SELECT event_id, cursor, occurred_at FROM change_stream_outbox
     WHERE scope = p_scope
       AND coalesce(workspace_id,'00000000-0000-0000-0000-000000000000'::uuid)
         = coalesce(p_workspace_id,'00000000-0000-0000-0000-000000000000'::uuid)
     ORDER BY cursor LIMIT p_limit
  ), deleted AS (
    DELETE FROM change_stream_outbox o USING candidates c
     WHERE o.event_id = c.event_id AND c.occurred_at < p_cutoff
       AND c.cursor < coalesce((SELECT min(cursor) FROM candidates WHERE occurred_at >= p_cutoff), 9223372036854775807)
     RETURNING o.cursor
  ), updated AS (
    UPDATE change_stream_scope_counters
       SET retained_from_cursor = GREATEST(retained_from_cursor, (SELECT max(cursor) + 1 FROM deleted))
     WHERE scope = p_scope AND workspace_id IS NOT DISTINCT FROM p_workspace_id
       AND EXISTS (SELECT 1 FROM deleted)
     RETURNING 1
  )
  SELECT count(*) + 0 * (SELECT count(*) FROM updated) INTO removed FROM deleted;
  RETURN removed;
END;
$$ LANGUAGE plpgsql;

-- Preserve the old internal helper and its workspace-UUID/global lock order,
-- but cap a compatibility call at 1000 removed rows. The worker uses the
-- single-scope primitive with a rotating scope cursor instead.
CREATE OR REPLACE FUNCTION hmem_change_stream_prune_outbox(p_cutoff TIMESTAMPTZ)
RETURNS BIGINT AS $$
DECLARE candidate RECORD;
DECLARE removed BIGINT := 0;
BEGIN
  FOR candidate IN
    SELECT scope, workspace_id FROM change_stream_scope_counters
     ORDER BY CASE WHEN scope = 'workspace' THEN 0 ELSE 1 END, workspace_id
  LOOP
    removed := removed + hmem_change_stream_prune_scope(candidate.scope, candidate.workspace_id, p_cutoff, (1000 - removed)::integer);
    EXIT WHEN removed >= 1000;
  END LOOP;
  RETURN removed;
END;
$$ LANGUAGE plpgsql;

INSERT INTO schema_migrations (version, name) VALUES (55, 'V055__bounded_outbox_retention.sql');
COMMIT;
