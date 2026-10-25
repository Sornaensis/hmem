BEGIN;
SELECT set_config('hmem.change_cause', 'migration', true);

-- Keep roleless historical declarations valid during unrelated lifecycle writes.
-- The prospective guard below requires a role only for genuinely new values.
CREATE OR REPLACE FUNCTION hmem_session_runtime_identity_valid(value JSONB) RETURNS BOOLEAN LANGUAGE SQL IMMUTABLE AS $$
  SELECT CASE WHEN value IS NULL THEN true WHEN jsonb_typeof(value)<>'object' THEN false ELSE coalesce(
    (value-ARRAY['model','provider','reasoning','role'])='{}'::jsonb
    AND jsonb_typeof(value->'model')='string' AND hmem_session_runtime_text_valid(value->>'model',256)
    AND (NOT value ? 'provider' OR (jsonb_typeof(value->'provider')='string' AND hmem_session_runtime_text_valid(value->>'provider',128)))
    AND (NOT value ? 'reasoning' OR (jsonb_typeof(value->'reasoning')='string' AND hmem_session_runtime_text_valid(value->>'reasoning',128)))
    AND (NOT value ? 'role' OR (jsonb_typeof(value->'role')='string' AND hmem_session_runtime_text_valid(value->>'role',128))),false) END;
$$;

CREATE FUNCTION hmem_session_runtime_role_guard() RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP='UPDATE' AND NEW.runtime_identity IS NOT DISTINCT FROM OLD.runtime_identity THEN
    RETURN NEW;
  END IF;
  IF NEW.runtime_identity IS NOT NULL AND
      (NOT NEW.runtime_identity ? 'role' OR jsonb_typeof(NEW.runtime_identity->'role') IS DISTINCT FROM 'string'
       OR NOT hmem_session_runtime_text_valid(NEW.runtime_identity->>'role',128)) THEN
    RAISE EXCEPTION USING ERRCODE='23514', CONSTRAINT='session_runtime_role_required',
      MESSAGE='A new session runtime declaration requires the running agent role.';
  END IF;
  RETURN NEW;
END;
$$;
CREATE TRIGGER session_runtime_role_guard BEFORE INSERT OR UPDATE OF runtime_identity ON sessions
  FOR EACH ROW EXECUTE FUNCTION hmem_session_runtime_role_guard();

INSERT INTO schema_migrations(version,name) VALUES(46,'V046__session_runtime_roles.sql');
COMMIT;
