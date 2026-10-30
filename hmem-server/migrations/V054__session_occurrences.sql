BEGIN;
SELECT set_config('hmem.change_cause','migration',true);

-- A replaceable interpretation of immutable evidence, never canonical session
-- metadata. Partial maxima stay private until the exact committed cursor matches.
CREATE TABLE session_occurrences (
  session_id UUID PRIMARY KEY,
  workspace_id UUID NOT NULL,
  derivation_version INTEGER NOT NULL CHECK (derivation_version > 0),
  processed_bytes BIGINT NOT NULL CHECK (processed_bytes >= 0),
  pending BYTEA NOT NULL DEFAULT '' CHECK (octet_length(pending) <= 1048576),
  last_activity_at TIMESTAMPTZ,
  source_offset BIGINT,
  pi_version INTEGER,
  FOREIGN KEY (session_id,workspace_id) REFERENCES sessions(id,workspace_id),
  CHECK ((last_activity_at IS NULL) = (source_offset IS NULL)),
  CHECK (source_offset IS NULL OR (source_offset >= 0 AND source_offset < processed_bytes)),
  CHECK (octet_length(pending) <= processed_bytes)
);

CREATE FUNCTION hmem_session_occurrence(identifier UUID, committed BIGINT)
RETURNS JSONB LANGUAGE SQL STABLE AS $$
  SELECT jsonb_build_object('last_activity_at',
    (SELECT last_activity_at FROM session_occurrences
     WHERE session_id=identifier AND derivation_version=1 AND processed_bytes=committed AND octet_length(pending)=0));
$$;

-- Extend the existing canonical metadata helper. Earlier schemas remain
-- readable by compatible DTO decoding during ordered upgrade qualification.
CREATE OR REPLACE FUNCTION hmem_session_ancestor_context(target UUID) RETURNS JSONB LANGUAGE SQL STABLE AS $$
  WITH RECURSIVE chain AS (
    SELECT p.id,p.title,p.format_label,p.state,p.parent_session_id,p.workspace_id,1 AS depth FROM sessions s JOIN sessions p ON p.id=s.parent_session_id AND p.workspace_id=s.workspace_id WHERE s.id=target
    UNION ALL SELECT p.id,p.title,p.format_label,p.state,p.parent_session_id,p.workspace_id,child.depth+1 FROM chain child JOIN sessions p ON p.id=child.parent_session_id AND p.workspace_id=child.workspace_id WHERE child.depth<16
  ) SELECT jsonb_build_object('ancestors',coalesce(jsonb_agg(jsonb_build_object('id',id,'title',title,'format_label',format_label,'state',state,'parent_session_id',parent_session_id) ORDER BY depth DESC),'[]'::jsonb),'ancestors_truncated',coalesce(bool_or(depth=16 AND parent_session_id IS NOT NULL),false))
    || hmem_session_occurrence(target,(SELECT committed_bytes FROM sessions WHERE id=target)) FROM chain;
$$;

-- Projection completion invalidates canonical metadata only. No evidence,
-- payload, user audit entry, liveness clock or session revision is changed.
CREATE FUNCTION hmem_session_occurrence_publish(identifier UUID, workspace UUID) RETURNS BOOLEAN LANGUAGE plpgsql AS $$
BEGIN
  IF EXISTS(SELECT 1 FROM sessions s JOIN session_occurrences o ON o.session_id=s.id AND o.workspace_id=s.workspace_id
      WHERE s.id=identifier AND s.workspace_id=workspace AND o.derivation_version=1
      AND o.processed_bytes=s.committed_bytes AND octet_length(o.pending)=0) THEN
    PERFORM hmem_change_stream_record('workspace',workspace,'agent_session',identifier::text,'updated',
      jsonb_build_array(jsonb_build_object('kind','entity','target','agent_session:'||identifier::text),
        jsonb_build_object('kind','collection','target','sessions:'||workspace::text)));
    RETURN true;
  END IF;
  RETURN false;
END;
$$;

INSERT INTO schema_migrations(version,name) VALUES (54,'V054__session_occurrences.sql');
COMMIT;
