BEGIN;

-- pgvector remains optional.  Space metadata is useful on every installation,
-- but we only touch the optional vector column when V020 created it.
ALTER TABLE public.observations
  ADD COLUMN IF NOT EXISTS embedding_space_fingerprint text;

ALTER TABLE public.observations
  ADD CONSTRAINT observations_embedding_space_fingerprint_valid
  CHECK (embedding_space_fingerprint IS NULL OR
         (length(embedding_space_fingerprint) BETWEEN 1 AND 128 AND
          embedding_space_fingerprint !~ '[[:space:]]'));

DO $$
BEGIN
  IF EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'observations'
      AND column_name = 'embedding'
  ) THEN
    EXECUTE $sql$
      UPDATE public.observations
         SET embedding_space_fingerprint = 'hmem:legacy-manual:v1'
       WHERE embedding IS NOT NULL AND embedding_space_fingerprint IS NULL
    $sql$;
  END IF;
END $$;

-- There is one selected target, not a model registry.  A disabled target
-- preserves pending work but prevents claims and new reconciliation.
CREATE TABLE IF NOT EXISTS public.embedding_target_state (
  singleton boolean PRIMARY KEY DEFAULT TRUE CHECK (singleton),
  enabled boolean NOT NULL DEFAULT FALSE,
  space_fingerprint text,
  reconcile_cursor uuid,
  updated_at timestamptz NOT NULL DEFAULT now(),
  CHECK ((enabled AND space_fingerprint IS NOT NULL) OR NOT enabled),
  CHECK (space_fingerprint IS NULL OR
         (length(space_fingerprint) BETWEEN 1 AND 128 AND space_fingerprint !~ '[[:space:]]'))
);

-- The composite key makes a job's denormalized workspace an integrity
-- boundary, rather than merely a convenience for claim filtering.
ALTER TABLE public.observations
  ADD CONSTRAINT observations_id_workspace_id_key UNIQUE (id, workspace_id);

CREATE TABLE IF NOT EXISTS public.embedding_jobs (
  observation_id uuid PRIMARY KEY,
  workspace_id uuid NOT NULL REFERENCES public.workspaces(id) ON DELETE CASCADE,
  content_fingerprint text NOT NULL CHECK (content_fingerprint ~ '^[0-9a-f]{64}$'),
  space_fingerprint text NOT NULL CHECK (length(space_fingerprint) BETWEEN 1 AND 128 AND space_fingerprint !~ '[[:space:]]'),
  state text NOT NULL CHECK (state IN ('pending', 'leased', 'complete', 'failed', 'superseded')),
  attempts integer NOT NULL DEFAULT 0 CHECK (attempts >= 0 AND attempts <= 16),
  next_attempt_at timestamptz NOT NULL DEFAULT now(),
  lease_owner text CHECK (lease_owner IS NULL OR length(lease_owner) BETWEEN 1 AND 128),
  lease_expires_at timestamptz,
  failure_code text CHECK (failure_code IS NULL OR failure_code IN ('provider_unavailable', 'provider_timeout', 'provider_protocol', 'provider_cancelled', 'provider_exhausted', 'stale')),
  created_at timestamptz NOT NULL DEFAULT now(),
  updated_at timestamptz NOT NULL DEFAULT now(),
  CHECK ((state = 'leased') = (lease_owner IS NOT NULL AND lease_expires_at IS NOT NULL)),
  FOREIGN KEY (observation_id, workspace_id)
    REFERENCES public.observations(id, workspace_id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_embedding_jobs_claim
  ON public.embedding_jobs (state, next_attempt_at, lease_expires_at, observation_id);
CREATE INDEX IF NOT EXISTS idx_embedding_jobs_workspace
  ON public.embedding_jobs (workspace_id, state, observation_id);

-- Content writes use the shared Observation update primitive, which clears the
-- vector and its space in the same UPDATE only when the optional vector column
-- exists.  A trigger that names NEW.embedding is unsafe if pgvector is later
-- removed, so V028 intentionally has no schema-level vector reference here.
DROP TRIGGER IF EXISTS trg_observations_clear_embedding_space ON public.observations;
DROP FUNCTION IF EXISTS public.hmem_observation_clear_embedding_space();

COMMIT;
