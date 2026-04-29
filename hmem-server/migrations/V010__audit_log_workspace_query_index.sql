-- Support workspace-scoped audit log review in descending timeline order.
-- V006 introduced a workspace/changed_at index; include id as the stable
-- tie-breaker used by the query ORDER BY.
CREATE INDEX IF NOT EXISTS idx_audit_log_workspace_changed_id
    ON audit_log (workspace_id, changed_at DESC, id DESC)
    WHERE workspace_id IS NOT NULL;
