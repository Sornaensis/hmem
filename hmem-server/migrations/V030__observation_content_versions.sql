BEGIN;

-- Opaque versions are independent of transaction clocks and request identity.
-- Adding the default backfills existing rows without emitting content updates.
ALTER TABLE public.observations
  ADD COLUMN content_version uuid NOT NULL DEFAULT gen_random_uuid();

CREATE FUNCTION public.hmem_advance_observation_content_version()
RETURNS trigger AS $$
BEGIN
  NEW.content_version := gen_random_uuid();
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_observations_content_version
  BEFORE UPDATE OF content ON public.observations
  FOR EACH ROW EXECUTE FUNCTION public.hmem_advance_observation_content_version();

-- A caller cannot manufacture a token by explicitly replacing it. The guard
-- runs before the content advancement trigger, which owns the new token.
CREATE FUNCTION public.hmem_guard_observation_content_version()
RETURNS trigger AS $$
BEGIN
  IF NEW.content_version IS DISTINCT FROM OLD.content_version THEN
    RAISE EXCEPTION USING ERRCODE = '23514',
      MESSAGE = 'Observation content versions are maintained by content writes.';
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_observations_00_guard_content_version
  BEFORE UPDATE OF content_version ON public.observations
  FOR EACH ROW EXECUTE FUNCTION public.hmem_guard_observation_content_version();

INSERT INTO schema_migrations (version, name)
VALUES (30, 'V030__observation_content_versions.sql');

COMMIT;
