BEGIN;

-- Harness is an optional explicit runtime declaration. Existing rows and
-- immutable connection attribution remain unchanged.
CREATE OR REPLACE FUNCTION public.hmem_session_runtime_identity_valid(value JSONB) RETURNS BOOLEAN LANGUAGE SQL IMMUTABLE AS $$
  SELECT CASE WHEN value IS NULL THEN true WHEN jsonb_typeof(value)<>'object' THEN false ELSE coalesce(
    (value-ARRAY['model','provider','reasoning','role','harness'])='{}'::jsonb
    AND jsonb_typeof(value->'model')='string' AND public.hmem_session_runtime_text_valid(value->>'model',256)
    AND (NOT value ? 'provider' OR (jsonb_typeof(value->'provider')='string' AND public.hmem_session_runtime_text_valid(value->>'provider',128)))
    AND (NOT value ? 'reasoning' OR (jsonb_typeof(value->'reasoning')='string' AND public.hmem_session_runtime_text_valid(value->>'reasoning',128)))
    AND (NOT value ? 'role' OR (jsonb_typeof(value->'role')='string' AND public.hmem_session_runtime_text_valid(value->>'role',128)))
    AND (NOT value ? 'harness' OR (jsonb_typeof(value->'harness')='string' AND public.hmem_session_runtime_text_valid(value->>'harness',128))),false) END;
$$;

INSERT INTO schema_migrations(version,name) VALUES (53,'V053__session_runtime_harness.sql');
COMMIT;
