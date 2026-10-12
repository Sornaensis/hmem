BEGIN;
SELECT hmem_lock_planning_lifecycle();

-- Private replay can restore deferred intent while ancestry remains frozen.
-- Preserve those historical entity/task rows; original-root running guards and
-- locks stay separate from the pruned roots used for effective reconciliation.
CREATE FUNCTION hmem_reconcile_project_pause_operation(root_ids UUID[], allow_running BOOLEAN, resume_root UUID)
RETURNS VOID AS $$
DECLARE
  eligible_ids UUID[];
  eligible_roots UUID[];
  live_project_ids UUID[];
  live_task_ids UUID[];
  running_ids UUID[];
  locked_id UUID;
  previous_materializing TEXT;
  pure_resume BOOLEAN := resume_root IS NOT NULL;
  pending_request JSONB;
  statement_scope TEXT;
BEGIN
  PERFORM hmem_lock_planning_lifecycle();
  IF pure_resume AND (cardinality(root_ids) IS DISTINCT FROM 1 OR root_ids[1] IS DISTINCT FROM resume_root
      OR NOT EXISTS (SELECT 1 FROM projects WHERE id=resume_root AND deleted_at IS NULL
        AND hmem_is_open_project_status(status) AND NOT own_paused)
      OR EXISTS (SELECT 1 FROM project_retained_pause_intents WHERE project_id=resume_root)) THEN
    RAISE EXCEPTION 'Invalid singleton project resume authority';
  END IF;
  PERFORM hmem_lock_project_ancestors(root_ids);
  FOR locked_id IN
    WITH RECURSIVE subtree(id, depth) AS (
      SELECT id, 0 FROM projects WHERE id = ANY(root_ids) AND deleted_at IS NULL
      UNION ALL
      SELECT p.id, s.depth + 1 FROM projects p JOIN subtree s ON p.parent_id = s.id
       WHERE p.deleted_at IS NULL
    ), ordered AS (SELECT id, max(depth) AS depth FROM subtree GROUP BY id)
    SELECT p.id FROM projects p JOIN ordered s ON p.id = s.id
     ORDER BY s.depth, p.id FOR UPDATE OF p
  LOOP NULL; END LOOP;
  -- The original roots govern locks/refusal even if mutation is frozen.
  WITH RECURSIVE subtree(id) AS (
    SELECT id FROM projects WHERE id = ANY(root_ids) AND deleted_at IS NULL
    UNION SELECT p.id FROM projects p JOIN subtree s ON p.parent_id=s.id WHERE p.deleted_at IS NULL
  ) SELECT coalesce(array_agg(id), ARRAY[]::UUID[]) INTO live_project_ids FROM subtree;
  FOR locked_id IN
    WITH RECURSIVE subtree(id, depth) AS (
      SELECT id,0 FROM tasks WHERE project_id=ANY(live_project_ids) AND deleted_at IS NULL
      UNION ALL SELECT t.id,s.depth+1 FROM tasks t JOIN subtree s ON t.parent_id=s.id WHERE t.deleted_at IS NULL
    ), ordered AS (SELECT id,max(depth) AS depth FROM subtree GROUP BY id)
    SELECT t.id FROM tasks t JOIN ordered s ON t.id=s.id ORDER BY s.depth,t.id FOR UPDATE OF t
  LOOP NULL; END LOOP;
  WITH RECURSIVE subtree(id) AS (
    SELECT id FROM tasks WHERE project_id=ANY(live_project_ids) AND deleted_at IS NULL
    UNION SELECT t.id FROM tasks t JOIN subtree s ON t.parent_id=s.id WHERE t.deleted_at IS NULL
  ) SELECT coalesce(array_agg(id), ARRAY[]::UUID[]) INTO live_task_ids FROM subtree;
  -- Reject a pending genuine pause before resume readiness can mask its
  -- original running work. An orphan BEFORE capture is not a written target:
  -- require replacement of the old tuple plus the exact stored NEW state.
  statement_scope := coalesce(nullif(current_setting('hmem.project_pause_audit_scopes',true),''),'[]')::jsonb ->> -1;
  SELECT entry.value -> 'resume' INTO pending_request
    FROM jsonb_each(coalesce(nullif(current_setting('hmem.project_pause_audit_capture',true),''),'{}')::jsonb) entry
    JOIN projects p ON p.id::text=entry.value -> 'resume' ->> 'id'
   WHERE entry.value -> 'resume' ->> 'scope'=statement_scope
     AND entry.value -> 'resume' -> 'pause_requested'='true'::jsonb
     AND coalesce((entry.value -> 'resume' ->> 'running_count')::bigint,0)>0
     AND p.workspace_id::text=entry.value -> 'resume' ->> 'workspace_id'
     AND p.deleted_at IS NULL
     AND p.ctid::text IS DISTINCT FROM entry.value -> 'resume' ->> 'old_tuple'
     AND p.status::text=entry.value -> 'resume' ->> 'status'
     AND p.own_paused=(entry.value -> 'resume' ->> 'own_paused')::boolean
     AND EXISTS(SELECT 1 FROM project_retained_pause_intents r WHERE r.project_id=p.id)
       =(entry.value -> 'resume' ->> 'retained_paused')::boolean
   ORDER BY p.id LIMIT 1;
  IF pending_request IS NOT NULL THEN
    RAISE EXCEPTION USING ERRCODE='HM204',
      MESSAGE='Cannot pause or place a project while affected descendant tasks are in progress.',
      DETAIL=jsonb_build_object('blocker_count',(pending_request ->> 'running_count')::bigint,
        'blocker_ids',pending_request -> 'running_ids')::text,
      HINT='Finish, cancel, or block running descendant tasks and subtasks before pausing or moving the project.';
  END IF;
  SELECT coalesce(array_agg(id), ARRAY[]::UUID[]) INTO eligible_roots FROM projects
    WHERE id=ANY(root_ids) AND hmem_project_pause_eligible(id);
  WITH RECURSIVE subtree(id) AS (
    SELECT id FROM projects WHERE id = ANY(root_ids) AND hmem_project_pause_eligible(id)
    UNION
    SELECT p.id FROM projects p JOIN subtree s ON p.parent_id = s.id
     WHERE p.deleted_at IS NULL AND hmem_is_open_project_status(p.status)
  ) SELECT coalesce(array_agg(id), ARRAY[]::UUID[]) INTO eligible_ids FROM subtree;
  previous_materializing := current_setting('hmem.retained_pause_materializing', true);
  PERFORM set_config('hmem.retained_pause_materializing', 'true', true);
  WITH materialized AS (
    UPDATE projects p SET own_paused = true
      FROM project_retained_pause_intents r
     WHERE p.id = r.project_id AND p.id = ANY(eligible_ids)
     RETURNING p.id
  ) DELETE FROM project_retained_pause_intents r USING materialized m WHERE r.project_id = m.id;
  PERFORM set_config('hmem.retained_pause_materializing', coalesce(previous_materializing, ''), true);
  -- Only a validated pure intent-clear can permit legacy running history.
  PERFORM hmem_reconcile_project_pause_v035(eligible_roots, allow_running OR pure_resume);
  -- Inspect the original full scope against settled/projected pause. This
  -- also protects excluded retained roots without rewriting frozen rows.
  SELECT coalesce(array_agg(t.id ORDER BY t.id), ARRAY[]::UUID[]) INTO running_ids
    FROM tasks t WHERE t.id=ANY(live_task_ids) AND t.status='in_progress'::task_status_enum
      AND (hmem_task_is_inside_paused_project(t.id) OR EXISTS (
        SELECT 1 FROM unnest(hmem_task_implicated_project_ids(t.project_id,t.parent_id)) implicated(id)
         WHERE hmem_project_has_pause_intent(implicated.id)));
  IF NOT allow_running AND NOT pure_resume AND cardinality(running_ids)>0 THEN
    RAISE EXCEPTION USING ERRCODE='HM204',
      MESSAGE='Cannot pause or place a project while affected descendant tasks are in progress.',
      DETAIL=jsonb_build_object('blocker_count',cardinality(running_ids),'blocker_ids',running_ids[1:5])::text,
      HINT='Finish, cancel, or block running descendant tasks and subtasks before pausing or moving the project.';
  END IF;

END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION hmem_reconcile_project_pause(root_ids UUID[], allow_running BOOLEAN DEFAULT false)
RETURNS VOID AS $$
  SELECT hmem_reconcile_project_pause_operation(root_ids,allow_running,NULL);
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION hmem_project_pause_from_row() RETURNS TRIGGER AS $$
DECLARE
  captures JSONB := coalesce(nullif(current_setting('hmem.project_pause_audit_capture',true),''),'{}')::jsonb;
  capture_key TEXT;
  candidate JSONB;
  candidate_valid BOOLEAN := false;
  requested_status BOOLEAN := false;
  explicit_active BOOLEAN := false;
  pause_requested BOOLEAN := false;
  private_intent JSONB;
  pure_resume BOOLEAN := false;
BEGIN
  -- Consume the exact tuple/statement candidate before every early return.
  IF TG_OP='UPDATE' THEN
    capture_key := 'UPDATE:' || TG_RELID::text || ':' || OLD.ctid::text;
    candidate := captures -> capture_key -> 'resume';
    candidate_valid := candidate ->> 'id'=NEW.id::text
      AND candidate ->> 'workspace_id'=NEW.workspace_id::text
      AND candidate ->> 'scope'=(coalesce(nullif(current_setting('hmem.project_pause_audit_scopes',true),''),'[]')::jsonb ->> -1);
    IF coalesce(candidate_valid,false) THEN
      requested_status := coalesce((candidate ->> 'requested')::boolean,false);
      explicit_active := coalesce((candidate ->> 'active')::boolean,false);
      pause_requested := coalesce((candidate ->> 'pause_requested')::boolean,false);
    END IF;
    IF candidate IS NOT NULL THEN
      PERFORM set_config('hmem.project_pause_audit_capture',(captures-capture_key)::text,true);
    END IF;
  END IF;
  IF NEW.deleted_at IS NOT NULL OR pg_trigger_depth()>1
     OR current_setting('hmem.retained_pause_materializing',true)='true' THEN RETURN NEW; END IF;
  IF TG_OP='UPDATE' THEN
    IF NEW.parent_id IS NOT DISTINCT FROM OLD.parent_id
       AND NEW.deleted_at IS NOT DISTINCT FROM OLD.deleted_at
       AND NEW.own_paused IS NOT DISTINCT FROM OLD.own_paused
       AND (current_setting('hmem.status_intent',true)='unchanged'
         OR NOT hmem_is_open_project_status(NEW.status)
         OR (NOT requested_status AND NOT pause_requested AND NEW.status IS NOT DISTINCT FROM OLD.status)) THEN RETURN NEW; END IF;
    private_intent := nullif(current_setting('hmem.project_pause_intent',true),'')::jsonb;
    pure_resume := OLD.deleted_at IS NULL AND NEW.deleted_at IS NULL
      AND hmem_is_open_project_status(OLD.status) AND hmem_is_open_project_status(NEW.status)
      AND NEW.id=OLD.id AND NEW.workspace_id=OLD.workspace_id AND NEW.parent_id IS NOT DISTINCT FROM OLD.parent_id
      AND NOT NEW.own_paused AND NOT EXISTS(SELECT 1 FROM project_retained_pause_intents WHERE project_id=NEW.id)
      AND (OLD.own_paused OR explicit_active OR (
        private_intent ->> 'id'=NEW.id::text AND private_intent ->> 'workspace_id'=NEW.workspace_id::text
        AND private_intent -> 'own_paused'='false'::jsonb AND private_intent -> 'retained_paused'='false'::jsonb));
  END IF;
  IF coalesce(pure_resume,false) THEN
    PERFORM hmem_reconcile_project_pause_operation(ARRAY[NEW.id],false,NEW.id);
  ELSE
    PERFORM hmem_reconcile_project_pause(ARRAY[NEW.id]);
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- The sparse marker can be removed by the later BEFORE intent trigger. Keep
-- its prior value per row version until this row's AFTER audit is delivered.
-- FK cascades may deliver AFTER at a different depth from BEFORE; OLD.ctid
-- identifies the actual update/delete version independently of trigger depth.
CREATE FUNCTION hmem_project_pause_audit_begin() RETURNS TRIGGER AS $$
DECLARE
  scopes JSONB := coalesce(nullif(current_setting('hmem.project_pause_audit_scopes',true),''),'[]')::jsonb;
BEGIN
  PERFORM set_config('hmem.project_pause_audit_scopes', (scopes || to_jsonb(gen_random_uuid()::text))::text, true);
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION hmem_project_pause_audit_capture() RETURNS TRIGGER AS $$
DECLARE
  captures JSONB := coalesce(nullif(current_setting('hmem.project_pause_audit_capture', true), ''), '{}')::jsonb;
  target_project_id UUID := CASE WHEN TG_OP = 'INSERT' THEN NEW.id ELSE OLD.id END;
  capture_key TEXT := CASE WHEN TG_OP='INSERT' THEN 'INSERT:' || NEW.id::text ELSE TG_OP || ':' || TG_RELID::text || ':' || OLD.ctid::text END;
BEGIN
  captures := jsonb_set(captures, ARRAY[capture_key], jsonb_build_object('project_id', target_project_id, 'scope', (coalesce(nullif(current_setting('hmem.project_pause_audit_scopes',true),''),'[]')::jsonb ->> -1), 'old', EXISTS (
    SELECT 1 FROM project_retained_pause_intents WHERE project_retained_pause_intents.project_id = target_project_id
  )));
  PERFORM set_config('hmem.project_pause_audit_capture', captures::text, true);
  IF TG_OP = 'DELETE' THEN RETURN OLD; END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- UPDATE OF status distinguishes a raw repeated active request from metadata.
-- The audit adapter retains this tuple/statement candidate for the row hook;
-- scoped cleanup handles suppressed/conflict rows without global authority.
CREATE FUNCTION hmem_project_pause_resume_candidate() RETURNS TRIGGER AS $$
DECLARE
  captures JSONB := coalesce(nullif(current_setting('hmem.project_pause_audit_capture',true),''),'{}')::jsonb;
  capture_key TEXT := 'UPDATE:' || TG_RELID::text || ':' || OLD.ctid::text;
  requested BOOLEAN := current_setting('hmem.status_intent',true) IS DISTINCT FROM 'unchanged'
    AND pg_trigger_depth()=1 AND current_setting('hmem.retained_pause_materializing',true) IS DISTINCT FROM 'true';
  requested_status BOOLEAN := TG_ARGV[0]='status' AND requested;
  retained BOOLEAN;
  genuine_pause BOOLEAN;
  running_count BIGINT := 0;
  running_ids UUID[] := ARRAY[]::UUID[];
BEGIN
  SELECT EXISTS(SELECT 1 FROM project_retained_pause_intents WHERE project_id=NEW.id) INTO retained;
  genuine_pause := requested AND NEW.deleted_at IS NULL AND (
    (TG_ARGV[0]='own' AND NEW.own_paused)
    OR (TG_ARGV[0]='status' AND (NEW.own_paused OR (NEW.status='paused'::project_status_enum AND retained))));
  IF genuine_pause THEN
    WITH RECURSIVE project_tree(id) AS (
      SELECT OLD.id
      UNION SELECT p.id FROM projects p JOIN project_tree parent ON p.parent_id=parent.id WHERE p.deleted_at IS NULL
    ), task_tree(id) AS (
      SELECT t.id FROM tasks t JOIN project_tree p ON p.id=t.project_id WHERE t.deleted_at IS NULL
      UNION SELECT child.id FROM tasks child JOIN task_tree parent ON child.parent_id=parent.id WHERE child.deleted_at IS NULL
    ) SELECT count(*),coalesce((array_agg(t.id ORDER BY t.id))[1:5],ARRAY[]::UUID[])
      INTO running_count,running_ids FROM tasks t JOIN task_tree tree ON tree.id=t.id
      WHERE t.status='in_progress'::task_status_enum;
  END IF;
  captures := jsonb_set(captures,ARRAY[capture_key,'resume'],jsonb_build_object(
    'id',NEW.id,'workspace_id',NEW.workspace_id,'scope',captures -> capture_key -> 'scope',
    'old_tuple',OLD.ctid::text,'status',NEW.status::text,'own_paused',NEW.own_paused,'retained_paused',retained,
    'requested',requested_status,'active',requested_status AND NEW.status='active'::project_status_enum,
    'pause_requested',genuine_pause,'running_count',running_count,'running_ids',running_ids));
  PERFORM set_config('hmem.project_pause_audit_capture',captures::text,true);
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Freeze the new marker at the end of the BEFORE phase as well. An earlier
-- row's AFTER cascade may materialize a still-pending row in the same UPDATE.
CREATE FUNCTION hmem_project_pause_audit_finish() RETURNS TRIGGER AS $$
DECLARE
  captures JSONB := coalesce(nullif(current_setting('hmem.project_pause_audit_capture', true), ''), '{}')::jsonb;
  target_project_id UUID := CASE WHEN TG_OP = 'DELETE' THEN OLD.id ELSE NEW.id END;
  capture_key TEXT := CASE WHEN TG_OP='INSERT' THEN 'INSERT:' || NEW.id::text ELSE TG_OP || ':' || TG_RELID::text || ':' || OLD.ctid::text END;
  retained_after BOOLEAN;
BEGIN
  IF NOT captures ? capture_key THEN RAISE EXCEPTION 'Missing project pause audit capture: trigger=% op=% depth=% target=% captures=%', TG_NAME,TG_OP,pg_trigger_depth(),target_project_id,captures; END IF;
  SELECT EXISTS (SELECT 1 FROM project_retained_pause_intents WHERE project_id = target_project_id) INTO retained_after;
  -- V036 consumes its materialized marker immediately after this guarded write.
  IF TG_OP = 'DELETE' OR (TG_OP <> 'DELETE' AND NEW.own_paused
      AND current_setting('hmem.retained_pause_materializing', true) = 'true') THEN
    retained_after := false;
  END IF;
  captures := jsonb_set(captures, ARRAY[capture_key, 'new'], to_jsonb(retained_after));
  PERFORM set_config('hmem.project_pause_audit_capture', captures::text, true);
  IF TG_OP = 'DELETE' THEN RETURN OLD; END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Conflicts and suppressed rows can execute BEFORE without an AFTER row.
-- Statement tokens, rather than trigger depth, retain pending outer/FK writes.
CREATE FUNCTION hmem_project_pause_audit_cleanup() RETURNS TRIGGER AS $$
DECLARE
  captures JSONB := coalesce(nullif(current_setting('hmem.project_pause_audit_capture',true),''),'{}')::jsonb;
  scopes JSONB := coalesce(nullif(current_setting('hmem.project_pause_audit_scopes',true),''),'[]')::jsonb;
  scope_token TEXT := scopes ->> -1;
  remaining JSONB;
BEGIN
  SELECT coalesce(jsonb_object_agg(key,value), '{}') INTO remaining
    FROM jsonb_each(captures) WHERE value ->> 'scope' IS DISTINCT FROM scope_token;
  PERFORM set_config('hmem.project_pause_audit_capture', remaining::text, true);
  PERFORM set_config('hmem.project_pause_audit_scopes', (scopes - (jsonb_array_length(scopes)-1))::text, true);
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION hmem_project_pause_audit_change() RETURNS TRIGGER AS $$
DECLARE
  captures JSONB := coalesce(nullif(current_setting('hmem.project_pause_audit_capture', true), ''), '{}')::jsonb;
  target_project_id UUID := CASE WHEN TG_OP = 'DELETE' THEN OLD.id ELSE NEW.id END;
  capture_key TEXT := CASE WHEN TG_OP='INSERT' THEN 'INSERT:' || NEW.id::text ELSE TG_OP || ':' || TG_RELID::text || ':' || OLD.ctid::text END;
  retained_before BOOLEAN;
  retained_after BOOLEAN;
  old_row JSONB;
  new_row JSONB;
BEGIN
  -- A missing capture is an authority error, never inferred from row status.
  IF NOT captures ? capture_key THEN RAISE EXCEPTION 'Missing project pause audit capture: trigger=% op=% depth=% target=% captures=%', TG_NAME,TG_OP,pg_trigger_depth(),target_project_id,captures; END IF;
  retained_before := (captures -> capture_key ->> 'old')::boolean;
  retained_after := (captures -> capture_key ->> 'new')::boolean;
  IF retained_before IS NULL OR retained_after IS NULL THEN RAISE EXCEPTION 'Incomplete project pause audit capture'; END IF;
  IF captures -> capture_key ? 'resume' THEN
    captures := jsonb_set(captures,ARRAY[capture_key],jsonb_build_object(
      'project_id',target_project_id,'scope',captures -> capture_key -> 'scope',
      'resume',captures -> capture_key -> 'resume'));
  ELSE
    captures := captures-capture_key;
  END IF;
  PERFORM set_config('hmem.project_pause_audit_capture',captures::text,true);
  IF TG_OP <> 'INSERT' THEN
    old_row := hmem_jsonb_without_keys(to_jsonb(OLD), ARRAY['updated_at', 'search_vector'])
      || jsonb_build_object('retained_paused', retained_before);
  END IF;
  IF TG_OP <> 'DELETE' THEN
    new_row := hmem_jsonb_without_keys(to_jsonb(NEW), ARRAY['updated_at', 'search_vector'])
      || jsonb_build_object('retained_paused', retained_after);
  END IF;
  IF TG_OP = 'UPDATE' AND old_row = new_row THEN RETURN NULL; END IF;
  INSERT INTO audit_log (entity_type, entity_id, action, old_values, new_values, request_id,
                        workspace_id, actor_type, actor_id, actor_label)
  VALUES ('project', target_project_id::text,
    CASE TG_OP WHEN 'INSERT' THEN 'create'::audit_action_enum WHEN 'UPDATE' THEN 'update'::audit_action_enum ELSE 'delete'::audit_action_enum END,
    old_row, new_row, nullif(current_setting('hmem.request_id', true), ''),
    coalesce(nullif(current_setting('hmem.workspace_id', true), '')::uuid, CASE WHEN TG_OP = 'DELETE' THEN OLD.workspace_id ELSE NEW.workspace_id END),
    nullif(current_setting('hmem.actor_type', true), '')::actor_type_enum,
    nullif(current_setting('hmem.actor_id', true), ''), nullif(current_setting('hmem.actor_label', true), ''));
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

-- Lexical ordering captures before intent and audits before recursive cascades.
CREATE TRIGGER trg_project_00_pause_audit_begin BEFORE INSERT OR UPDATE OR DELETE ON projects
  FOR EACH STATEMENT EXECUTE FUNCTION hmem_project_pause_audit_begin();
CREATE TRIGGER trg_project_00_pause_audit_capture BEFORE INSERT OR UPDATE OR DELETE ON projects
  FOR EACH ROW EXECUTE FUNCTION hmem_project_pause_audit_capture();
CREATE TRIGGER zz_project_pause_own_candidate BEFORE UPDATE OF own_paused ON projects
  FOR EACH ROW EXECUTE FUNCTION hmem_project_pause_resume_candidate('own');
CREATE TRIGGER zz_project_pause_resume_candidate BEFORE UPDATE OF status ON projects
  FOR EACH ROW EXECUTE FUNCTION hmem_project_pause_resume_candidate('status');
CREATE TRIGGER zzz_project_pause_audit_finish BEFORE INSERT OR UPDATE OR DELETE ON projects
  FOR EACH ROW EXECUTE FUNCTION hmem_project_pause_audit_finish();
CREATE TRIGGER zzz_project_pause_audit_cleanup AFTER INSERT OR UPDATE OR DELETE ON projects
  FOR EACH STATEMENT EXECUTE FUNCTION hmem_project_pause_audit_cleanup();
DROP TRIGGER trg_projects_audit ON projects;
CREATE TRIGGER trg_project_00_pause_audit AFTER INSERT OR UPDATE OR DELETE ON projects
  FOR EACH ROW EXECUTE FUNCTION hmem_project_pause_audit_change();

INSERT INTO schema_migrations(version, name) VALUES (37, 'V037__project_pause_audit_provenance.sql');
COMMIT;
