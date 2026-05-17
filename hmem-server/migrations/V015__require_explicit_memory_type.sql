BEGIN;

------------------------------------------------------------------------
-- Require explicit memory_type on new memory writes
------------------------------------------------------------------------

-- Preserve any legacy rows from pre-release/dev databases before making the
-- invariant explicit.  The current schema already has NOT NULL and no default,
-- but keeping the backfill here makes the migration safe for drifted installs.
UPDATE memories
   SET memory_type = 'short_term'
 WHERE memory_type IS NULL;

ALTER TABLE memories ALTER COLUMN memory_type DROP DEFAULT;
ALTER TABLE memories ALTER COLUMN memory_type SET NOT NULL;

CREATE OR REPLACE FUNCTION hmem_require_explicit_memory_type()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.memory_type IS NULL THEN
    RAISE EXCEPTION USING
      ERRCODE = 'HM305',
      MESSAGE = 'Memory creation requires explicit memory_type.',
      DETAIL = jsonb_build_object('field', 'memory_type')::text,
      HINT = 'Provide memory_type as short_term or long_term.';
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trg_memory_type_required ON memories;
CREATE TRIGGER trg_memory_type_required
  BEFORE INSERT OR UPDATE OF memory_type ON memories
  FOR EACH ROW EXECUTE FUNCTION hmem_require_explicit_memory_type();

COMMIT;
