BEGIN;

------------------------------------------------------------------------
-- Avoid copying token digests into access-token audit snapshots.
--
-- V006 created the access_tokens audit trigger before token_hash was
-- explicitly classified as secret-adjacent lifecycle data.  Recreate the
-- trigger with token_hash in the ignored-field list and scrub any V006-era
-- audit snapshots so both fresh and upgraded databases keep token material
-- and token digests out of audit row old_values/new_values.
------------------------------------------------------------------------

UPDATE audit_log
   SET old_values = old_values - 'token_hash',
       new_values = new_values - 'token_hash'
 WHERE entity_type = 'access_token'
   AND (COALESCE(old_values ? 'token_hash', false)
     OR COALESCE(new_values ? 'token_hash', false));

DROP TRIGGER IF EXISTS trg_access_tokens_audit ON access_tokens;
CREATE TRIGGER trg_access_tokens_audit
    AFTER INSERT OR UPDATE OR DELETE ON access_tokens
    FOR EACH ROW EXECUTE FUNCTION hmem_audit_change('access_token', 'id', 'token_hash');

COMMIT;
