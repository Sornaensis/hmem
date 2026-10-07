BEGIN;

-- Adding defaults and backfilling claims emit no Observation update/audit event.
ALTER TABLE public.observations
  ADD COLUMN latest_sequence bigint NOT NULL DEFAULT 1
    CHECK (latest_sequence BETWEEN 1 AND 9007199254740991),
  ADD COLUMN current_provenance jsonb;

CREATE TABLE public.observation_revision_events (
  observation_id uuid NOT NULL REFERENCES public.observations(id) ON DELETE CASCADE,
  sequence bigint NOT NULL CHECK (sequence BETWEEN 1 AND 9007199254740991),
  event_kind text NOT NULL CHECK (event_kind IN ('creation', 'update', 'legacy_creation')),
  reviewed_git_sha text NOT NULL CHECK (reviewed_git_sha ~ '^[0-9a-f]{40}$'),
  content_version uuid,
  content_digest text CHECK (content_digest ~ '^[0-9a-f]{64}$'),
  recorded_at timestamptz NOT NULL,
  actor_type text CHECK (actor_type IN ('user', 'bot')),
  actor_id text,
  actor_label text,
  PRIMARY KEY (observation_id, sequence),
  CHECK ((event_kind = 'legacy_creation' AND content_version IS NULL AND content_digest IS NULL
          AND actor_type IS NULL AND actor_id IS NULL AND actor_label IS NULL)
      OR (event_kind <> 'legacy_creation' AND content_version IS NOT NULL AND content_digest IS NOT NULL)),
  CHECK ((actor_type IS NULL AND actor_id IS NULL AND actor_label IS NULL)
      OR (actor_type IS NOT NULL AND actor_id IS NOT NULL AND actor_id <> ''))
);
CREATE INDEX idx_observation_revision_sha
  ON public.observation_revision_events(observation_id, reviewed_git_sha);

INSERT INTO public.observation_revision_events
  (observation_id, sequence, event_kind, reviewed_git_sha, recorded_at)
SELECT id, 1, 'legacy_creation', git_sha, created_at FROM public.observations;

CREATE FUNCTION public.hmem_observation_provenance(
  seq bigint, kind text, sha text, version uuid, body text)
RETURNS jsonb LANGUAGE sql VOLATILE AS $$
  SELECT jsonb_build_object(
    'sequence', seq, 'event_kind', kind, 'reviewed_git_sha', sha,
    'content_version', version,
    'content_digest', encode(sha256(convert_to(body, 'UTF8')), 'hex'),
    'recorded_at', clock_timestamp(),
    'actor_type', nullif(current_setting('hmem.actor_type', true), ''),
    'actor_id', nullif(current_setting('hmem.actor_id', true), ''),
    'actor_label', nullif(current_setting('hmem.actor_label', true), ''))
$$;

CREATE FUNCTION public.hmem_guard_observation_provenance()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  RAISE EXCEPTION USING ERRCODE = '23514',
    MESSAGE = 'Observation revision projections are server maintained.';
END;
$$;
CREATE TRIGGER trg_observations_00_guard_provenance
  BEFORE UPDATE OF latest_sequence, current_provenance ON public.observations
  FOR EACH ROW EXECUTE FUNCTION public.hmem_guard_observation_provenance();

CREATE FUNCTION public.hmem_prepare_observation_provenance()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE sha text;
BEGIN
  IF TG_OP = 'INSERT' THEN
    NEW.latest_sequence := 1;
    NEW.current_provenance := hmem_observation_provenance(
      1, 'creation', NEW.git_sha, NEW.content_version, NEW.content);
  ELSE
    sha := nullif(current_setting('hmem.reviewed_git_sha', true), '');
    IF sha IS NULL OR sha !~ '^[0-9a-f]{40}$' THEN
      RAISE EXCEPTION USING ERRCODE = '23514',
        MESSAGE = 'Observation content writes require an explicit reviewed Git SHA.';
    END IF;
    IF OLD.latest_sequence >= 9007199254740991 THEN
      RAISE EXCEPTION USING ERRCODE = '23514', MESSAGE = 'Observation revision sequence exhausted.';
    END IF;
    NEW.latest_sequence := OLD.latest_sequence + 1;
    NEW.current_provenance := hmem_observation_provenance(
      NEW.latest_sequence, 'update', sha, NEW.content_version, NEW.content);
  END IF;
  RETURN NEW;
END;
$$;
-- Alphabetically after the existing content_version BEFORE trigger; the
-- fresh token and provenance enter the single Observation mutation together.
CREATE TRIGGER trg_observations_revision_prepare
  BEFORE INSERT OR UPDATE OF content ON public.observations
  FOR EACH ROW EXECUTE FUNCTION public.hmem_prepare_observation_provenance();

CREATE FUNCTION public.hmem_guard_observation_revision_event()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE parent public.observations; previous bigint;
BEGIN
  IF TG_OP = 'DELETE' THEN
    -- FK cascades run after the parent row has ceased to exist. Direct
    -- deletion while a live parent exists is never permitted.
    IF NOT EXISTS (SELECT 1 FROM public.observations WHERE id = OLD.observation_id) THEN
      RETURN OLD;
    END IF;
  ELSIF TG_OP = 'INSERT' THEN
    SELECT * INTO parent FROM public.observations WHERE id = NEW.observation_id FOR UPDATE;
    SELECT coalesce(max(sequence), 0) INTO previous
      FROM public.observation_revision_events WHERE observation_id = NEW.observation_id;
    IF parent.id IS NOT NULL AND NEW.sequence = previous + 1
       AND NEW.sequence = parent.latest_sequence
       AND to_jsonb(NEW) - 'observation_id' = parent.current_provenance THEN
      RETURN NEW;
    END IF;
  END IF;
  RAISE EXCEPTION USING ERRCODE = '23514',
    MESSAGE = 'Observation revision events are append-only and server maintained.';
END;
$$;
CREATE TRIGGER trg_observation_revision_events_guard
  BEFORE INSERT OR UPDATE OR DELETE ON public.observation_revision_events
  FOR EACH ROW EXECUTE FUNCTION public.hmem_guard_observation_revision_event();

-- Explicit projection avoids the event's observation_id being duplicated by
-- jsonb_populate_record; emit just one compact event per content write.
CREATE FUNCTION public.hmem_append_observation_revision_event()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE p public.observation_revision_events;
BEGIN
  p := jsonb_populate_record(NULL::public.observation_revision_events, NEW.current_provenance);
  p.observation_id := NEW.id;
  INSERT INTO public.observation_revision_events SELECT p.*;
  RETURN NULL;
END;
$$;
CREATE TRIGGER trg_observations_revision_append
  AFTER INSERT OR UPDATE OF content ON public.observations
  FOR EACH ROW EXECUTE FUNCTION public.hmem_append_observation_revision_event();

INSERT INTO schema_migrations (version, name)
VALUES (32, 'V032__observation_revision_events.sql');

COMMIT;
