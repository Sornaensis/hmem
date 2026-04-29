BEGIN;

------------------------------------------------------------------------
-- Avoid per-request audit noise from access_tokens.last_used_at updates.
--
-- last_used_at is operational usage metadata updated during deployed PAT
-- authentication.  Keeping it in the ignored-field list means an update that
-- only touches last_used_at is suppressed by hmem_audit_change, while token
-- lifecycle changes such as insert/revoke/delete remain audited.
------------------------------------------------------------------------

UPDATE audit_log
   SET old_values = old_values - 'token_hash' - 'last_used_at',
       new_values = new_values - 'token_hash' - 'last_used_at'
 WHERE entity_type = 'access_token'
   AND (COALESCE(old_values ? 'token_hash', false)
     OR COALESCE(new_values ? 'token_hash', false)
     OR COALESCE(old_values ? 'last_used_at', false)
     OR COALESCE(new_values ? 'last_used_at', false));

DELETE FROM audit_log
 WHERE entity_type = 'access_token'
   AND action = 'update'
   AND old_values IS NOT DISTINCT FROM new_values;

DROP TRIGGER IF EXISTS trg_access_tokens_audit ON access_tokens;
CREATE TRIGGER trg_access_tokens_audit
    AFTER INSERT OR UPDATE OR DELETE ON access_tokens
    FOR EACH ROW EXECUTE FUNCTION hmem_audit_change('access_token', 'id', 'token_hash', 'last_used_at');

COMMIT;
