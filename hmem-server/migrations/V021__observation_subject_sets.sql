BEGIN;

-- Subjects are immutable provenance, but an observation may describe several
-- repository paths or path globs.  Keeping them in their own ordered relation
-- avoids a lossy array encoding and makes exact and path-match predicates
-- indexable.
CREATE TABLE observation_subjects (
  observation_id UUID NOT NULL REFERENCES observations(id) ON DELETE CASCADE,
  ordinal SMALLINT NOT NULL,
  subject_kind observation_subject_kind NOT NULL,
  subject TEXT NOT NULL,
  PRIMARY KEY (observation_id, ordinal),
  CONSTRAINT uq_observation_subjects_kind_subject UNIQUE (observation_id, subject_kind, subject),
  CONSTRAINT chk_observation_subjects_ordinal CHECK (ordinal BETWEEN 0 AND 255),
  CONSTRAINT chk_observation_subjects_length CHECK (octet_length(subject) BETWEEN 1 AND 4096),
  CONSTRAINT chk_observation_subjects_canonical_path CHECK (
    subject !~ '(^/|^\./|^[A-Za-z]:/|\\|[[:cntrl:]]|//|/$|(^|/)\.{1,2}(/|$))'
  ),
  CONSTRAINT chk_observation_subjects_file_literal CHECK (
    subject_kind <> 'file' OR subject !~ '[*?]'
  ),
  -- Deliberately small glob language: literal path components plus *, ?, and
  -- ** as a complete component.  Character classes, braces and escapes are
  -- not accepted because their portability differs between match engines.
  CONSTRAINT chk_observation_subjects_safe_glob CHECK (
    subject_kind <> 'glob' OR (
      subject !~ '[\[\]{}]' AND subject !~ '\*\*\*' AND subject !~ '(^|/)[^*/]+\*\*[^/]*($|/)' AND subject !~ '(^|/)[^/]*\*\*[^*/]+($|/)'
    )
  )
);

ALTER TABLE observations ADD COLUMN subject_set_open BOOLEAN NOT NULL DEFAULT FALSE;

INSERT INTO observation_subjects (observation_id, ordinal, subject_kind, subject)
SELECT id, 0, subject_kind, subject FROM observations;

-- Backfilled observations are already complete. New core-created parents set
-- this private creation-state column to true and the child insert statement
-- immediately seals it.

CREATE INDEX idx_observation_subjects_observation ON observation_subjects (observation_id, ordinal);
CREATE INDEX idx_observation_subjects_exact ON observation_subjects (subject_kind, subject, observation_id);

-- Translate the deliberately restricted glob grammar to a PostgreSQL regular
-- expression.  This is the database counterpart of the pure core matcher.
CREATE OR REPLACE FUNCTION hmem_observation_subject_matches(
  candidate_kind observation_subject_kind,
  candidate_subject TEXT,
  candidate_path TEXT
) RETURNS BOOLEAN AS $$
BEGIN
  IF candidate_path IS NULL OR candidate_path = ''
     OR candidate_path ~ '(^/|^\./|^[A-Za-z]:/|\\|[[:cntrl:]]|//|/$|(^|/)\.{1,2}(/|$)|[*?])'
     OR candidate_subject ~ '(^/|^\./|^[A-Za-z]:/|\\|[[:cntrl:]]|//|/$|(^|/)\.{1,2}(/|$))'
     OR (candidate_kind = 'file' AND candidate_subject ~ '[*?]')
     OR (candidate_kind = 'glob' AND (candidate_subject ~ '[\[\]{}]' OR candidate_subject ~ '\*\*\*' OR candidate_subject ~ '(^|/)[^*/]+\*\*[^/]*($|/)' OR candidate_subject ~ '(^|/)[^/]*\*\*[^*/]+($|/)')) THEN
    RETURN FALSE;
  END IF;
  IF candidate_kind = 'file' THEN
    RETURN candidate_subject = candidate_path;
  END IF;
  RETURN hmem_observation_glob_matches(candidate_subject, candidate_path, 1, 1);
END;
$$ LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE;

CREATE OR REPLACE FUNCTION hmem_observation_glob_matches(
  pattern TEXT, candidate TEXT, pattern_at INTEGER, candidate_at INTEGER
) RETURNS BOOLEAN AS $$
DECLARE
  pattern_length INTEGER := char_length(pattern);
  expression TEXT := '^';
  current TEXT;
  cursor_at INTEGER := 1;
BEGIN
  -- Subject validation limits both inputs to 4096 bytes. Compile the tiny,
  -- restricted grammar once instead of recursively exploring alternatives.
  IF octet_length(pattern) > 4096 OR octet_length(candidate) > 4096 THEN
    RETURN FALSE;
  END IF;

  pattern := substr(pattern, pattern_at);
  candidate := substr(candidate, candidate_at);
  pattern_length := char_length(pattern);
  WHILE cursor_at <= pattern_length LOOP
    current := substr(pattern, cursor_at, 1);
    IF current = '*' AND substr(pattern, cursor_at + 1, 1) = '*' THEN
      IF substr(pattern, cursor_at + 2, 1) = '/' THEN
        -- **/ consumes only zero or more complete path components.
        expression := expression || '([^/]+/)*';
        cursor_at := cursor_at + 3;
      ELSE
        expression := expression || '.*';
        cursor_at := cursor_at + 2;
      END IF;
    ELSIF current = '*' THEN
      expression := expression || '[^/]*';
      cursor_at := cursor_at + 1;
    ELSIF current = '?' THEN
      expression := expression || '[^/]';
      cursor_at := cursor_at + 1;
    ELSIF current IN ('.', '^', '$', '+', '(', ')', '|') OR current = chr(92) THEN
      expression := expression || chr(92) || current;
      cursor_at := cursor_at + 1;
    ELSE
      expression := expression || current;
      cursor_at := cursor_at + 1;
    END IF;
  END LOOP;
  RETURN candidate ~ (expression || '$');
END;
$$ LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE;

CREATE OR REPLACE FUNCTION hmem_observation_subject_set_is_valid(observation UUID)
RETURNS BOOLEAN AS $$
  SELECT count(*) BETWEEN 1 AND 256
     AND coalesce(sum(octet_length(subject)), 0) <= 262144
     AND min(ordinal) = 0 AND max(ordinal) = count(*) - 1
  FROM observation_subjects
  WHERE observation_id = observation;
$$ LANGUAGE sql STABLE;

CREATE OR REPLACE FUNCTION hmem_enforce_observation_subject_set()
RETURNS TRIGGER AS $$
DECLARE
  observation UUID := COALESCE(NEW.observation_id, OLD.observation_id);
BEGIN
  IF TG_OP = 'UPDATE' THEN
    RAISE EXCEPTION USING ERRCODE = 'HM401', MESSAGE = 'Observation subjects are immutable.';
  END IF;
  IF TG_OP = 'DELETE' THEN
    -- At deferred constraint-trigger time a parent cascade has removed the
    -- parent row; a direct child delete has not.  This does not depend on
    -- trigger depth, which differs across PostgreSQL cascade implementations.
    IF EXISTS (SELECT 1 FROM observations WHERE id = observation) THEN
      RAISE EXCEPTION USING ERRCODE = 'HM401', MESSAGE = 'Observation subjects are immutable.';
    END IF;
    RETURN OLD;
  END IF;
  IF NOT hmem_observation_subject_set_is_valid(observation) THEN
    RAISE EXCEPTION USING ERRCODE = 'HM401', MESSAGE = 'Observations require 1 to 256 subjects with at most 256 KiB total subject text.';
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION hmem_authorize_observation_subject_insert()
RETURNS TRIGGER AS $$
BEGIN
  -- This is state held by the parent row, not a transaction GUC a client can
  -- set.  The statement-level trigger seals every inserted parent set.
  IF NOT EXISTS (SELECT 1 FROM observations WHERE id = NEW.observation_id AND subject_set_open) THEN
    RAISE EXCEPTION USING ERRCODE = 'HM401', MESSAGE = 'Observation subjects are immutable.';
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_observation_subjects_insert_immutable
  BEFORE INSERT ON observation_subjects
  FOR EACH ROW EXECUTE FUNCTION hmem_authorize_observation_subject_insert();

CREATE OR REPLACE FUNCTION hmem_seal_observation_subject_sets()
RETURNS TRIGGER AS $$
BEGIN
  UPDATE observations o
     SET subject_set_open = FALSE
   WHERE o.subject_set_open
     AND o.id IN (SELECT DISTINCT observation_id FROM new_subjects);
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_observation_subjects_seal
  AFTER INSERT ON observation_subjects
  REFERENCING NEW TABLE AS new_subjects
  FOR EACH STATEMENT EXECUTE FUNCTION hmem_seal_observation_subject_sets();

-- Observation subjects are stored outside their parent row, so the generic
-- row audit trigger cannot take a complete snapshot on creation (the child
-- rows are inserted by the next CTE).  Keep the audit record on the
-- Observation entity, but assemble its canonical and legacy-compatible
-- subject shape explicitly.
CREATE OR REPLACE FUNCTION hmem_observation_audit_snapshot(
  observation_row JSONB,
  observation UUID
) RETURNS JSONB AS $$
DECLARE
  snapshot_subjects JSONB;
BEGIN
  IF observation_row IS NULL THEN
    RETURN NULL;
  END IF;

  SELECT coalesce(
    jsonb_agg(
      jsonb_build_object('subject_kind', subject_kind::text, 'subject', subject)
      ORDER BY ordinal
    ),
    '[]'::jsonb
  )
  INTO snapshot_subjects
  FROM observation_subjects
  WHERE observation_id = observation;

  -- This fallback keeps pre-V021 historical audit rows readable even when
  -- their Observation was deleted before the migration.
  IF snapshot_subjects = '[]'::jsonb
     AND observation_row ? 'subject_kind'
     AND observation_row ? 'subject' THEN
    snapshot_subjects := jsonb_build_array(jsonb_build_object(
      'subject_kind', observation_row -> 'subject_kind',
      'subject', observation_row -> 'subject'
    ));
  END IF;

  observation_row := (observation_row - 'subject_kind' - 'subject')
    || jsonb_build_object('subjects', snapshot_subjects);

  IF jsonb_array_length(snapshot_subjects) > 0 THEN
    observation_row := observation_row || jsonb_build_object(
      'subject_kind', snapshot_subjects -> 0 -> 'subject_kind',
      'subject', snapshot_subjects -> 0 -> 'subject'
    );
  END IF;

  RETURN observation_row;
END;
$$ LANGUAGE plpgsql STABLE;

CREATE OR REPLACE FUNCTION hmem_insert_observation_audit(
  audit_action audit_action_enum,
  old_row JSONB,
  new_row JSONB
) RETURNS VOID AS $$
DECLARE
  current_workspace TEXT := NULLIF(current_setting('hmem.workspace_id', true), '');
  current_actor_type TEXT := NULLIF(current_setting('hmem.actor_type', true), '');
BEGIN
  IF current_workspace IS NULL THEN
    current_workspace := coalesce(new_row ->> 'workspace_id', old_row ->> 'workspace_id');
  END IF;

  INSERT INTO audit_log (
    entity_type, entity_id, action, old_values, new_values, request_id,
    workspace_id, actor_type, actor_id, actor_label
  ) VALUES (
    'observation',
    coalesce(new_row ->> 'id', old_row ->> 'id'),
    audit_action,
    old_row,
    new_row,
    NULLIF(current_setting('hmem.request_id', true), ''),
    CASE WHEN current_workspace IS NULL THEN NULL ELSE current_workspace::UUID END,
    CASE WHEN current_actor_type IS NULL THEN NULL ELSE current_actor_type::actor_type_enum END,
    NULLIF(current_setting('hmem.actor_id', true), ''),
    NULLIF(current_setting('hmem.actor_label', true), '')
  );
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION hmem_audit_observation_change()
RETURNS TRIGGER AS $$
DECLARE
  old_row JSONB;
  new_row JSONB;
BEGIN
  CASE TG_OP
    WHEN 'UPDATE' THEN
      old_row := hmem_observation_audit_snapshot(
        hmem_jsonb_without_keys(to_jsonb(OLD), ARRAY['updated_at', 'search_vector', 'embedding', 'subject_set_open']),
        OLD.id
      );
      new_row := hmem_observation_audit_snapshot(
        hmem_jsonb_without_keys(to_jsonb(NEW), ARRAY['updated_at', 'search_vector', 'embedding', 'subject_set_open']),
        NEW.id
      );
      IF old_row = new_row THEN
        RETURN NULL;
      END IF;
      PERFORM hmem_insert_observation_audit('update', old_row, new_row);
    WHEN 'DELETE' THEN
      -- This is deliberately BEFORE DELETE: the FK cascade has not yet
      -- removed the immutable subject rows needed for the old-value snapshot.
      old_row := hmem_observation_audit_snapshot(
        hmem_jsonb_without_keys(to_jsonb(OLD), ARRAY['updated_at', 'search_vector', 'embedding', 'subject_set_open']),
        OLD.id
      );
      PERFORM hmem_insert_observation_audit('delete', old_row, NULL);
      RETURN OLD;
  END CASE;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION hmem_audit_observation_creations()
RETURNS TRIGGER AS $$
DECLARE
  observation_row observations%ROWTYPE;
BEGIN
  -- A statement-level child trigger sees the entire immutable ordered set and
  -- emits exactly one parent create event, including for multi-subject CTEs.
  FOR observation_row IN
    SELECT o.*
    FROM observations o
    JOIN (SELECT DISTINCT observation_id FROM new_subjects) n ON n.observation_id = o.id
  LOOP
    PERFORM hmem_insert_observation_audit(
      'create',
      NULL,
      hmem_observation_audit_snapshot(
        hmem_jsonb_without_keys(to_jsonb(observation_row), ARRAY['updated_at', 'search_vector', 'embedding', 'subject_set_open']),
        observation_row.id
      )
    );
  END LOOP;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER trg_observations_audit ON observations;
CREATE TRIGGER trg_observations_audit
  AFTER UPDATE ON observations
  FOR EACH ROW EXECUTE FUNCTION hmem_audit_observation_change();

CREATE TRIGGER trg_observations_audit_delete
  BEFORE DELETE ON observations
  FOR EACH ROW EXECUTE FUNCTION hmem_audit_observation_change();

CREATE TRIGGER trg_observation_subjects_audit
  AFTER INSERT ON observation_subjects
  REFERENCING NEW TABLE AS new_subjects
  FOR EACH STATEMENT EXECUTE FUNCTION hmem_audit_observation_creations();

CREATE CONSTRAINT TRIGGER trg_observation_subject_set_valid
  AFTER INSERT OR DELETE OR UPDATE ON observation_subjects
  DEFERRABLE INITIALLY IMMEDIATE
  FOR EACH ROW EXECUTE FUNCTION hmem_enforce_observation_subject_set();

CREATE OR REPLACE FUNCTION hmem_enforce_observation_has_subjects()
RETURNS TRIGGER AS $$
BEGIN
  IF NOT hmem_observation_subject_set_is_valid(NEW.id) THEN
    RAISE EXCEPTION USING ERRCODE = 'HM401', MESSAGE = 'Observations require an immutable nonempty contiguous subject set.';
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER trg_observations_have_subjects
  AFTER INSERT ON observations
  DEFERRABLE INITIALLY IMMEDIATE
  FOR EACH ROW EXECUTE FUNCTION hmem_enforce_observation_has_subjects();

CREATE OR REPLACE FUNCTION hmem_observations_search_vector()
RETURNS TRIGGER AS $$
BEGIN
  NEW.search_vector := to_tsvector('simple', NEW.content || ' ' || coalesce((
    SELECT string_agg(subject, ' ' ORDER BY ordinal)
    FROM observation_subjects WHERE observation_id = NEW.id
  ), ''));
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER trg_observations_search_vector ON observations;
CREATE TRIGGER trg_observations_search_vector
  BEFORE INSERT OR UPDATE OF content ON observations
  FOR EACH ROW EXECUTE FUNCTION hmem_observations_search_vector();

CREATE OR REPLACE FUNCTION hmem_refresh_observation_search_vector()
RETURNS TRIGGER AS $$
BEGIN
  UPDATE observations
     SET search_vector = to_tsvector('simple', content || ' ' || coalesce((
       SELECT string_agg(subject, ' ' ORDER BY ordinal)
       FROM observation_subjects WHERE observation_id = COALESCE(NEW.observation_id, OLD.observation_id)
     ), ''))
   WHERE id = COALESCE(NEW.observation_id, OLD.observation_id);
  RETURN COALESCE(NEW, OLD);
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_observation_subjects_search_vector
  AFTER INSERT OR DELETE ON observation_subjects
  FOR EACH ROW EXECUTE FUNCTION hmem_refresh_observation_search_vector();

-- Parent provenance is now workspace plus revision.  Subject rows carry the
-- remaining immutable provenance and have their own guard above.
CREATE OR REPLACE FUNCTION hmem_enforce_observation_provenance_immutable()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.workspace_id IS DISTINCT FROM OLD.workspace_id
     OR NEW.git_sha IS DISTINCT FROM OLD.git_sha THEN
    RAISE EXCEPTION USING
      ERRCODE = 'HM401',
      MESSAGE = 'Observation provenance is immutable.',
      HINT = 'Create a new observation for a different workspace or Git revision.';
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER trg_observations_provenance_immutable ON observations;
CREATE TRIGGER trg_observations_provenance_immutable
  BEFORE UPDATE OF workspace_id, git_sha ON observations
  FOR EACH ROW EXECUTE FUNCTION hmem_enforce_observation_provenance_immutable();

DROP INDEX idx_observations_workspace_subject_kind_subject;
DROP INDEX idx_observations_workspace_git_sha_subject;
ALTER TABLE observations
  DROP CONSTRAINT chk_observations_subject_octet_length,
  DROP COLUMN subject_kind,
  DROP COLUMN subject;

-- V020 audit snapshots were shaped from the now-retired parent columns.
-- Normalize their preserved legacy singleton provenance so old audit entries
-- expose the same canonical-plus-legacy projection as V021 entries.
UPDATE audit_log
SET old_values = hmem_observation_audit_snapshot(
      old_values,
      CASE WHEN old_values ? 'id' THEN (old_values ->> 'id')::uuid ELSE NULL END
    ),
    new_values = hmem_observation_audit_snapshot(
      new_values,
      CASE WHEN new_values ? 'id' THEN (new_values ->> 'id')::uuid ELSE NULL END
    )
WHERE entity_type = 'observation'
  AND (
    (old_values IS NOT NULL AND NOT old_values ? 'subjects')
    OR (new_values IS NOT NULL AND NOT new_values ? 'subjects')
  );

CREATE INDEX idx_observations_workspace_git_sha_updated
  ON observations (workspace_id, git_sha, updated_at DESC, id DESC);

INSERT INTO schema_migrations (version, name)
VALUES (21, 'V021__observation_subject_sets.sql');

COMMIT;
