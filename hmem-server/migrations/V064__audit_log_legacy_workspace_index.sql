-- Timeline inventory reads legacy audit rows that have no workspace_id and are
-- attributed by their snapshot's workspace. Index only those rows so the
-- inventory never scans the whole audit log; workspace rows already use the
-- workspace indexes.
CREATE INDEX IF NOT EXISTS idx_audit_log_legacy_workspace_entity
    ON audit_log (entity_type, changed_at)
    WHERE workspace_id IS NULL;
