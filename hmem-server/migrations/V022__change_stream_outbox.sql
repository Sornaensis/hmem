BEGIN;

-- Canonical durable stream state.  Tokens are stored only as SHA-256 hashes;
-- neither resume nor snapshot bearer values are ever persisted in clear text.
CREATE TABLE change_stream_scope_counters (
  scope TEXT NOT NULL CHECK (scope IN ('workspace', 'global')),
  workspace_id UUID,
  next_cursor BIGINT NOT NULL DEFAULT 0 CHECK (next_cursor >= 0),
  authorization_epoch BIGINT NOT NULL DEFAULT 0 CHECK (authorization_epoch >= 0),
  -- The first cursor which may still be replayed.  Keeping this durable makes
  -- a fully-pruned stream distinguishable from an empty, never-used stream.
  retained_from_cursor BIGINT NOT NULL DEFAULT 1 CHECK (retained_from_cursor >= 1)
);
CREATE UNIQUE INDEX uq_change_stream_scope_counters
  ON change_stream_scope_counters (scope, coalesce(workspace_id, '00000000-0000-0000-0000-000000000000'::uuid));

-- A dependency must retain its workspace identity after an endpoint is
-- hard-deleted by an FK cascade.  The outbox DELETE trigger cannot recover it
-- from @tasks@ once PostgreSQL has removed the parent row.
ALTER TABLE task_dependencies ADD COLUMN workspace_id UUID;
UPDATE task_dependencies td
   SET workspace_id = task.workspace_id
  FROM tasks task
 WHERE task.id = td.task_id;
ALTER TABLE task_dependencies ALTER COLUMN workspace_id SET NOT NULL;

CREATE TABLE change_stream_outbox (
  event_id UUID PRIMARY KEY,
  scope TEXT NOT NULL CHECK (scope IN ('workspace', 'global')),
  workspace_id UUID,
  cursor BIGINT NOT NULL CHECK (cursor > 0),
  occurred_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  transaction_id UUID NOT NULL,
  transaction_cause TEXT NOT NULL CHECK (transaction_cause IN ('rest', 'mcp', 'audit_revert', 'core', 'migration')),
  request_id TEXT,
  actor_type TEXT NOT NULL CHECK (actor_type IN ('user', 'service', 'system')),
  actor_id TEXT,
  envelope JSONB NOT NULL,
  CONSTRAINT chk_change_stream_scope_workspace CHECK ((scope = 'workspace') = (workspace_id IS NOT NULL)),
  CONSTRAINT uq_change_stream_cursor UNIQUE (scope, workspace_id, cursor)
);
-- PostgreSQL 14 treats NULL values as distinct in a normal unique constraint,
-- so retain global cursor uniqueness with an expression index as well.
CREATE UNIQUE INDEX uq_change_stream_global_cursor
  ON change_stream_outbox (scope, coalesce(workspace_id, '00000000-0000-0000-0000-000000000000'::uuid), cursor);
CREATE INDEX idx_change_stream_outbox_replay
  ON change_stream_outbox (scope, workspace_id, cursor);
CREATE INDEX idx_change_stream_outbox_retention
  ON change_stream_outbox (occurred_at);

CREATE TABLE change_stream_resume_tokens (
  token_hash BYTEA PRIMARY KEY,
  scope TEXT NOT NULL CHECK (scope IN ('workspace', 'global')),
  workspace_id UUID,
  -- Store the audience discriminator and its components separately.  A
  -- concatenated string could otherwise make an authenticated key/user pair
  -- collide with a trusted key.
  audience_kind TEXT NOT NULL CHECK (audience_kind IN ('authenticated', 'trusted')),
  audience_key TEXT NOT NULL,
  audience_user_id UUID,
  authorization_epoch BIGINT NOT NULL,
  watermark BIGINT NOT NULL CHECK (watermark >= 0),
  expires_at TIMESTAMPTZ NOT NULL,
  superseded_at TIMESTAMPTZ,
  -- Links a terminal session without retaining its bearer.  The snapshot
  -- table is declared below, so cleanup removes both tables explicitly.
  session_hash BYTEA,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT chk_change_stream_resume_scope_workspace CHECK ((scope = 'workspace') = (workspace_id IS NOT NULL)),
  CONSTRAINT chk_change_stream_resume_audience CHECK ((audience_kind = 'authenticated') = (audience_user_id IS NOT NULL))
);
CREATE INDEX idx_change_stream_resume_expiry ON change_stream_resume_tokens (expires_at);

CREATE TABLE change_stream_snapshot_sessions (
  session_hash BYTEA PRIMARY KEY,
  scope TEXT NOT NULL CHECK (scope IN ('workspace', 'global')),
  workspace_id UUID,
  audience_kind TEXT NOT NULL CHECK (audience_kind IN ('authenticated', 'trusted')),
  audience_key TEXT NOT NULL,
  audience_user_id UUID,
  authorization_epoch BIGINT NOT NULL,
  high_watermark BIGINT NOT NULL CHECK (high_watermark >= 0),
  expires_at TIMESTAMPTZ NOT NULL,
  terminal_at TIMESTAMPTZ,
  resume_token_hash BYTEA,
  -- Compatibility columns retained for the first V022 deployment.  Page
  -- bearer state lives in change_stream_snapshot_page_tokens below so retries
  -- return the identical ordinal range without consuming a bearer.
  page_token_hash BYTEA NOT NULL,
  terminal_page_token_hash BYTEA,
  next_ordinal BIGINT NOT NULL DEFAULT 0 CHECK (next_ordinal >= 0),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT chk_change_stream_snapshot_scope_workspace CHECK ((scope = 'workspace') = (workspace_id IS NOT NULL)),
  CONSTRAINT chk_change_stream_snapshot_audience CHECK ((audience_kind = 'authenticated') = (audience_user_id IS NOT NULL))
);
CREATE TABLE change_stream_snapshot_items (
  session_hash BYTEA NOT NULL REFERENCES change_stream_snapshot_sessions(session_hash) ON DELETE CASCADE,
  ordinal BIGINT NOT NULL CHECK (ordinal >= 0),
  item JSONB NOT NULL,
  PRIMARY KEY (session_hash, ordinal)
);
CREATE TABLE change_stream_snapshot_page_tokens (
  token_hash BYTEA PRIMARY KEY,
  session_hash BYTEA NOT NULL REFERENCES change_stream_snapshot_sessions(session_hash) ON DELETE CASCADE,
  start_ordinal BIGINT NOT NULL CHECK (start_ordinal >= 0),
  end_ordinal BIGINT CHECK (end_ordinal IS NULL OR end_ordinal >= start_ordinal),
  terminal BOOLEAN NOT NULL DEFAULT false
);
CREATE INDEX idx_change_stream_snapshot_page_tokens_session
  ON change_stream_snapshot_page_tokens (session_hash, start_ordinal);

-- Allocate the first scope row and hold its row lock.  Resync begins by
-- calling this helper before authorization/snapshot reads; membership writes
-- call the same helper in a BEFORE trigger, so they cannot cross the handoff.
CREATE OR REPLACE FUNCTION hmem_change_stream_lock(
  p_scope TEXT,
  p_workspace_id UUID
) RETURNS change_stream_scope_counters AS $$
DECLARE counter change_stream_scope_counters%ROWTYPE;
BEGIN
  INSERT INTO change_stream_scope_counters(scope, workspace_id)
  VALUES (p_scope, p_workspace_id)
  ON CONFLICT DO NOTHING;
  SELECT * INTO counter
    FROM change_stream_scope_counters
   WHERE scope = p_scope
     AND workspace_id IS NOT DISTINCT FROM p_workspace_id
   FOR UPDATE;
  RETURN counter;
END;
$$ LANGUAGE plpgsql;

-- beginResync must acquire the scope lock before it authorizes, so a
-- membership writer cannot cross its snapshot hand-off.  An unauthorized
-- caller therefore aborts the whole transaction, including a newly allocated
-- scope counter, rather than committing a durable lock-only row.
CREATE OR REPLACE FUNCTION hmem_change_stream_abort_resync_unauthorized()
RETURNS VOID AS $$
BEGIN
  RAISE EXCEPTION USING
    ERRCODE = 'HM501',
    MESSAGE = 'hmem change-stream resync unauthorized';
END;
$$ LANGUAGE plpgsql;

-- Retention takes the same multi-scope lock order as authorization
-- invalidation: workspace UUID ascending, then the global scope.  The loop
-- deliberately calls the common locking helper rather than relying on an SQL
-- planner's row-lock order.
CREATE OR REPLACE FUNCTION hmem_change_stream_prune_outbox(p_cutoff TIMESTAMPTZ)
RETURNS BIGINT AS $$
DECLARE candidate RECORD;
DECLARE deleted_count BIGINT;
BEGIN
  FOR candidate IN
    SELECT DISTINCT workspace_id
      FROM change_stream_outbox
     WHERE scope = 'workspace' AND occurred_at < p_cutoff
     ORDER BY workspace_id ASC
  LOOP
    PERFORM hmem_change_stream_lock('workspace', candidate.workspace_id);
  END LOOP;
  IF EXISTS (
    SELECT 1 FROM change_stream_outbox
     WHERE scope = 'global' AND occurred_at < p_cutoff
  ) THEN
    PERFORM hmem_change_stream_lock('global', NULL);
  END IF;
  WITH deleted AS (
    DELETE FROM change_stream_outbox
     WHERE occurred_at < p_cutoff
     RETURNING scope, workspace_id, cursor
  ), floors AS (
    SELECT scope, workspace_id, max(cursor) + 1 AS floor
      FROM deleted
     GROUP BY scope, workspace_id
  ), updated AS (
    UPDATE change_stream_scope_counters c
       SET retained_from_cursor = GREATEST(c.retained_from_cursor, floors.floor)
      FROM floors
     WHERE c.scope = floors.scope
       AND c.workspace_id IS NOT DISTINCT FROM floors.workspace_id
     RETURNING 1
  )
  SELECT count(*)::bigint + 0 * (SELECT count(*) FROM updated)
    INTO deleted_count
    FROM deleted;
  RETURN deleted_count;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION hmem_change_stream_transaction_id()
RETURNS UUID AS $$
DECLARE existing TEXT;
DECLARE allocated UUID;
BEGIN
  existing := NULLIF(current_setting('hmem.change_transaction_id', true), '');
  IF existing IS NOT NULL THEN
    RETURN existing::uuid;
  END IF;
  allocated := gen_random_uuid();
  PERFORM set_config('hmem.change_transaction_id', allocated::text, true);
  RETURN allocated;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION hmem_change_stream_record(
  p_scope TEXT,
  p_workspace_id UUID,
  p_entity_type TEXT,
  p_entity_id TEXT,
  p_action TEXT,
  p_invalidations JSONB
) RETURNS VOID AS $$
DECLARE counter change_stream_scope_counters%ROWTYPE;
DECLARE assigned_cursor BIGINT;
DECLARE event UUID := gen_random_uuid();
DECLARE tx UUID := hmem_change_stream_transaction_id();
DECLARE cause TEXT := coalesce(NULLIF(current_setting('hmem.change_cause', true), ''), 'core');
DECLARE raw_actor TEXT := NULLIF(current_setting('hmem.actor_type', true), '');
DECLARE envelope JSONB;
DECLARE occurred TIMESTAMPTZ;
BEGIN
  counter := hmem_change_stream_lock(p_scope, p_workspace_id);
  UPDATE change_stream_scope_counters
     SET next_cursor = next_cursor + 1
   WHERE scope = p_scope AND workspace_id IS NOT DISTINCT FROM p_workspace_id
   RETURNING next_cursor INTO assigned_cursor;
  -- @now()@ is the transaction-start timestamp.  Capture wall time only after
  -- acquiring the scope lock so retention order cannot contradict cursors.
  occurred := clock_timestamp();
  envelope := jsonb_build_object(
    'schema_version', 1,
    'event_id', event,
    'scope', p_scope,
    'workspace_id', p_workspace_id,
    'cursor', assigned_cursor,
    'occurred_at', occurred,
    'transaction', jsonb_build_object(
      'id', tx, 'cause', cause,
      'request_id', NULLIF(current_setting('hmem.request_id', true), '')
    ),
    'actor', jsonb_build_object(
      'type', CASE raw_actor WHEN 'user' THEN 'user' WHEN 'bot' THEN 'service' ELSE 'system' END,
      'id', NULLIF(current_setting('hmem.actor_id', true), '')
    ),
    'entity', jsonb_build_object('type', p_entity_type, 'id', p_entity_id, 'action', p_action),
    'invalidations', coalesce(p_invalidations, '[]'::jsonb)
  );
  INSERT INTO change_stream_outbox(
    event_id, scope, workspace_id, cursor, transaction_id, transaction_cause,
    request_id, actor_type, actor_id, envelope, occurred_at
  ) VALUES (
    event, p_scope, p_workspace_id, assigned_cursor, tx, cause,
    NULLIF(current_setting('hmem.request_id', true), ''),
    CASE raw_actor WHEN 'user' THEN 'user' WHEN 'bot' THEN 'service' ELSE 'system' END,
    NULLIF(current_setting('hmem.actor_id', true), ''), envelope, occurred
  );
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION hmem_change_stream_row_change()
RETURNS TRIGGER AS $$
DECLARE row_workspace UUID;
DECLARE entity_id TEXT;
DECLARE action TEXT;
DECLARE old_clean JSONB;
DECLARE new_clean JSONB;
DECLARE entity_type TEXT := TG_ARGV[0];
DECLARE scoped_global BOOLEAN := TG_ARGV[1] = 'global';
DECLARE invalidations JSONB;
DECLARE dependency_task TEXT;
DECLARE dependency_prerequisite TEXT;
DECLARE dependency_workspace UUID;
  DECLARE old_task_project TEXT;
  DECLARE new_task_project TEXT;
  DECLARE old_task_parent TEXT;
  DECLARE new_task_parent TEXT;
DECLARE dependent_readiness JSONB := '[]'::jsonb;
BEGIN
  old_clean := CASE WHEN TG_OP = 'INSERT' THEN NULL ELSE to_jsonb(OLD) - 'updated_at' - 'search_vector' - 'embedding' - 'subject_set_open' END;
  new_clean := CASE WHEN TG_OP = 'DELETE' THEN NULL ELSE to_jsonb(NEW) - 'updated_at' - 'search_vector' - 'embedding' - 'subject_set_open' END;
  -- Observation embeddings are intentionally absent from envelopes, but an
  -- embedding-only mutation must still invalidate search results.  All other
  -- ignored-column-only updates remain no-ops.
  IF TG_OP = 'UPDATE' AND old_clean = new_clean
     AND (entity_type <> 'observation'
          OR (to_jsonb(OLD) -> 'embedding') IS NOT DISTINCT FROM (to_jsonb(NEW) -> 'embedding'))
  THEN RETURN NULL; END IF;
  entity_id := coalesce(new_clean ->> 'id', old_clean ->> 'id');
  IF entity_type = 'task_dependency' THEN
    entity_id := coalesce(new_clean ->> 'task_id', old_clean ->> 'task_id') || ':' || coalesce(new_clean ->> 'depends_on_id', old_clean ->> 'depends_on_id');
  ELSIF entity_type = 'workspace_membership' THEN
    entity_id := coalesce(new_clean ->> 'workspace_id', old_clean ->> 'workspace_id') || ':' || coalesce(new_clean ->> 'user_id', old_clean ->> 'user_id');
  ELSIF entity_type = 'workspace_group_membership' THEN
    entity_id := coalesce(new_clean ->> 'group_id', old_clean ->> 'group_id') || ':' || coalesce(new_clean ->> 'workspace_id', old_clean ->> 'workspace_id');
  END IF;
  row_workspace := CASE WHEN scoped_global THEN NULL ELSE coalesce((new_clean ->> 'workspace_id')::uuid, (old_clean ->> 'workspace_id')::uuid) END;
  IF entity_type = 'task_dependency' THEN
    SELECT workspace_id INTO dependency_workspace
      FROM tasks
     WHERE id = coalesce((new_clean ->> 'task_id')::uuid, (old_clean ->> 'task_id')::uuid);
    row_workspace := coalesce(row_workspace, dependency_workspace);
  END IF;
  IF TG_OP = 'INSERT' THEN action := 'created';
  ELSIF TG_OP = 'DELETE' THEN action := 'deleted';
  ELSIF (old_clean ->> 'deleted_at') IS NULL AND (new_clean ->> 'deleted_at') IS NOT NULL THEN action := 'deleted';
  ELSIF (old_clean ->> 'deleted_at') IS NOT NULL AND (new_clean ->> 'deleted_at') IS NULL THEN action := 'restored';
  ELSE action := 'updated';
  END IF;
  IF entity_type = 'task_dependency' THEN
    dependency_task := coalesce(new_clean ->> 'task_id', old_clean ->> 'task_id');
    dependency_prerequisite := coalesce(new_clean ->> 'depends_on_id', old_clean ->> 'depends_on_id');
    invalidations := jsonb_build_array(
      jsonb_build_object('kind', 'entity', 'target', 'task_dependency:' || entity_id),
      jsonb_build_object('kind', 'collection', 'target', 'task_dependencies:' || row_workspace::text),
      jsonb_build_object('kind', 'entity', 'target', 'task:' || dependency_task),
      jsonb_build_object('kind', 'entity', 'target', 'task:' || dependency_prerequisite),
      jsonb_build_object('kind', 'readiness', 'target', 'task:' || dependency_task),
      jsonb_build_object('kind', 'readiness', 'target', 'task:' || dependency_prerequisite),
      jsonb_build_object('kind', 'next_task', 'target', 'workspace:' || row_workspace::text),
      jsonb_build_object('kind', 'search', 'target', 'workspace:' || row_workspace::text)
    );
  ELSIF entity_type = 'task' THEN
    old_task_project := old_clean ->> 'project_id';
    new_task_project := new_clean ->> 'project_id';
    old_task_parent := old_clean ->> 'parent_id';
    new_task_parent := new_clean ->> 'parent_id';
    SELECT coalesce(jsonb_agg(jsonb_build_object('kind', 'readiness', 'target', 'task:' || td.task_id::text)), '[]'::jsonb)
      INTO dependent_readiness
      FROM task_dependencies td
     WHERE td.depends_on_id = entity_id::uuid;
    invalidations := jsonb_build_array(
      jsonb_build_object('kind', 'entity', 'target', 'task:' || entity_id),
      jsonb_build_object('kind', 'collection', 'target', 'tasks:' || row_workspace::text),
      jsonb_build_object('kind', 'tree', 'target', 'workspace:' || row_workspace::text),
      jsonb_build_object('kind', 'readiness', 'target', 'task:' || entity_id),
      jsonb_build_object('kind', 'next_task', 'target', 'workspace:' || row_workspace::text),
      jsonb_build_object('kind', 'search', 'target', 'workspace:' || row_workspace::text)
    )
      -- A move changes the rollups of both source and destination.  Preserve
      -- the existing targets for an unchanged association, while emitting the
      -- source target exactly once when the association changed.
      || CASE WHEN old_task_project IS NULL OR old_task_project IS NOT DISTINCT FROM new_task_project THEN '[]'::jsonb ELSE jsonb_build_array(jsonb_build_object('kind', 'readiness', 'target', 'project:' || old_task_project)) END
      || CASE WHEN new_task_project IS NULL THEN '[]'::jsonb ELSE jsonb_build_array(jsonb_build_object('kind', 'readiness', 'target', 'project:' || new_task_project)) END
      || CASE WHEN old_task_parent IS NULL OR old_task_parent IS NOT DISTINCT FROM new_task_parent THEN '[]'::jsonb ELSE jsonb_build_array(jsonb_build_object('kind', 'readiness', 'target', 'task:' || old_task_parent)) END
      || CASE WHEN new_task_parent IS NULL THEN '[]'::jsonb ELSE jsonb_build_array(jsonb_build_object('kind', 'readiness', 'target', 'task:' || new_task_parent)) END
      || dependent_readiness;
  ELSIF entity_type = 'project' THEN
    invalidations := jsonb_build_array(
      jsonb_build_object('kind', 'entity', 'target', 'project:' || entity_id),
      jsonb_build_object('kind', 'collection', 'target', 'projects:' || row_workspace::text),
      jsonb_build_object('kind', 'tree', 'target', 'workspace:' || row_workspace::text),
      jsonb_build_object('kind', 'readiness', 'target', 'project:' || entity_id),
      jsonb_build_object('kind', 'next_task', 'target', 'workspace:' || row_workspace::text),
      jsonb_build_object('kind', 'search', 'target', 'workspace:' || row_workspace::text)
    );
  ELSIF entity_type = 'observation' THEN
    invalidations := jsonb_build_array(
      jsonb_build_object('kind', 'entity', 'target', 'observation:' || entity_id),
      jsonb_build_object('kind', 'collection', 'target', 'observations:' || row_workspace::text),
      jsonb_build_object('kind', 'search', 'target', 'workspace:' || row_workspace::text)
    );
  ELSIF entity_type = 'workspace_membership' THEN
    -- Membership details never leave the durable envelope.  Audience metadata
    -- lets the dispatcher target the admin list separately from the affected
    -- user's catalogue/session/permission invalidations.
    invalidations := jsonb_build_array(
      jsonb_build_object('kind', 'collection', 'target', 'workspace:' || row_workspace::text || ':memberships', 'audience', 'workspace-admins'),
      jsonb_build_object('kind', 'catalogue', 'target', 'workspace-catalog', 'audience', 'user:' || coalesce(new_clean ->> 'user_id', old_clean ->> 'user_id')),
      jsonb_build_object('kind', 'session_authorization', 'target', 'session-authorization', 'audience', 'user:' || coalesce(new_clean ->> 'user_id', old_clean ->> 'user_id')),
      jsonb_build_object('kind', 'permission_cache', 'target', 'permission-cache', 'audience', 'user:' || coalesce(new_clean ->> 'user_id', old_clean ->> 'user_id'))
    );
  ELSIF entity_type = 'workspace' THEN
    invalidations := jsonb_build_array(
      jsonb_build_object('kind', 'entity', 'target', 'workspace:' || entity_id),
      jsonb_build_object('kind', 'catalogue', 'target', 'workspace-catalog'),
      jsonb_build_object('kind', 'collection', 'target', 'workspace-groups')
    );
  ELSIF entity_type = 'workspace_group' THEN
    invalidations := jsonb_build_array(
      jsonb_build_object('kind', 'entity', 'target', 'group:' || entity_id),
      jsonb_build_object('kind', 'collection', 'target', 'workspace-groups')
    );
  ELSIF entity_type = 'workspace_group_membership' THEN
    invalidations := jsonb_build_array(
      jsonb_build_object('kind', 'entity', 'target', 'group_membership:' || entity_id),
      jsonb_build_object('kind', 'collection', 'target', 'group:' || coalesce(new_clean ->> 'group_id', old_clean ->> 'group_id') || ':members'),
      jsonb_build_object('kind', 'collection', 'target', 'workspace:' || row_workspace::text || ':groups')
    );
  ELSE
    invalidations := jsonb_build_array(
      jsonb_build_object('kind', 'entity', 'target', entity_type || ':' || entity_id),
      jsonb_build_object('kind', 'collection', 'target', entity_type || ':' || coalesce(row_workspace::text, 'global'))
    );
  END IF;
  PERFORM hmem_change_stream_record(
    CASE WHEN scoped_global THEN 'global' ELSE 'workspace' END,
    row_workspace, entity_type, entity_id, action,
    invalidations
  );
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION hmem_change_stream_membership_lock()
RETURNS TRIGGER AS $$
DECLARE workspace UUID := coalesce(NEW.workspace_id, OLD.workspace_id);
BEGIN
  -- Even an ON CONFLICT DO UPDATE with identical values acquires this lock so
  -- it cannot cross a resync authorization/snapshot handoff.  It is not a
  -- material access change, so epoch advancement happens in the AFTER trigger
  -- below.  A BEFORE INSERT fires for an ON CONFLICT attempt before PostgreSQL
  -- knows it will update an existing membership.
  PERFORM hmem_change_stream_lock('workspace', workspace);
  RETURN coalesce(NEW, OLD);
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION hmem_change_stream_membership_epoch()
RETURNS TRIGGER AS $$
DECLARE workspace UUID := coalesce(NEW.workspace_id, OLD.workspace_id);
BEGIN
  IF TG_OP = 'UPDATE'
     AND NEW.role IS NOT DISTINCT FROM OLD.role
     AND NEW.granted_by IS NOT DISTINCT FROM OLD.granted_by THEN
    RETURN NULL;
  END IF;
  UPDATE change_stream_scope_counters
     SET authorization_epoch = authorization_epoch + 1
    WHERE scope = 'workspace' AND workspace_id = workspace;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

-- A workspace soft-delete revokes every workspace-scoped credential while the
-- workspace is inactive.  Lock that workspace before the global workspace
-- catalogue scope, matching the multi-scope ordering used elsewhere, then
-- advance its epoch exactly once.  Restore advances it again: an old token
-- that was valid before deletion must not spring back to life.
CREATE OR REPLACE FUNCTION hmem_change_stream_workspace_active_state_lock()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.deleted_at IS NOT DISTINCT FROM OLD.deleted_at THEN
    RETURN NEW;
  END IF;
  PERFORM hmem_change_stream_lock('workspace', NEW.id);
  PERFORM hmem_change_stream_lock('global', NULL);
  UPDATE change_stream_scope_counters
     SET authorization_epoch = authorization_epoch + 1
   WHERE scope = 'workspace' AND workspace_id = NEW.id;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Dependency writers do not supply the denormalized workspace identity.
-- Populate it before the cycle-check and preserve it unchanged thereafter.
CREATE OR REPLACE FUNCTION hmem_change_stream_dependency_workspace()
RETURNS TRIGGER AS $$
BEGIN
  SELECT workspace_id INTO NEW.workspace_id FROM tasks WHERE id = NEW.task_id;
  IF NEW.workspace_id IS NULL THEN
    RAISE EXCEPTION 'Task dependency target has no workspace';
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- A user disable/re-enable and a global-entitlement change can be written in
-- the same UPDATE.  They must therefore share one trigger: separate BEFORE
-- triggers can otherwise lock global first merely because of trigger-name
-- ordering.  Acquire every affected workspace counter in UUID order and the
-- global counter last, once, before any downstream invalidation effects.
CREATE OR REPLACE FUNCTION hmem_change_stream_user_authorization_lock()
RETURNS TRIGGER AS $$
DECLARE membership RECORD;
BEGIN
  IF NEW.disabled_at IS NOT DISTINCT FROM OLD.disabled_at
     AND NEW.is_superadmin IS NOT DISTINCT FROM OLD.is_superadmin THEN
    RETURN NEW;
  END IF;
  -- A superadmin may read every active workspace without a membership.  Any
  -- change to that entitlement (including disabling/re-enabling the user)
  -- therefore invalidates every already-issued workspace bearer, not merely
  -- the bearers for direct memberships.  The ordered UNION keeps this lock
  -- acquisition compatible with retention and group-cascade locking.
  IF NEW.is_superadmin IS DISTINCT FROM OLD.is_superadmin
     OR (NEW.disabled_at IS DISTINCT FROM OLD.disabled_at
         AND (NEW.is_superadmin OR OLD.is_superadmin)) THEN
    FOR membership IN
      SELECT workspace_id
        FROM change_stream_scope_counters
       WHERE scope = 'workspace'
      UNION
      SELECT workspace_id
        FROM workspace_memberships
       WHERE user_id = NEW.id
      ORDER BY workspace_id
    LOOP
      PERFORM hmem_change_stream_lock('workspace', membership.workspace_id);
      UPDATE change_stream_scope_counters
         SET authorization_epoch = authorization_epoch + 1
       WHERE scope = 'workspace' AND workspace_id = membership.workspace_id;
    END LOOP;
  ELSIF NEW.disabled_at IS DISTINCT FROM OLD.disabled_at THEN
    FOR membership IN
      SELECT workspace_id FROM workspace_memberships
       WHERE user_id = NEW.id
       ORDER BY workspace_id
    LOOP
      PERFORM hmem_change_stream_lock('workspace', membership.workspace_id);
      UPDATE change_stream_scope_counters
         SET authorization_epoch = authorization_epoch + 1
       WHERE scope = 'workspace' AND workspace_id = membership.workspace_id;
    END LOOP;
  END IF;
  IF NEW.is_superadmin IS DISTINCT FROM OLD.is_superadmin
     OR (NEW.disabled_at IS DISTINCT FROM OLD.disabled_at
         AND (NEW.is_superadmin OR OLD.is_superadmin)) THEN
    PERFORM hmem_change_stream_lock('global', NULL);
    UPDATE change_stream_scope_counters
       SET authorization_epoch = authorization_epoch + 1
     WHERE scope = 'global' AND workspace_id IS NULL;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Deleting a group cascades one membership delete for each workspace and also
-- emits a global group event.  Pre-lock the complete scope set here, before
-- PostgreSQL starts the cascade, so the child row triggers cannot acquire
-- workspace counters in physical row order before the global scope.
CREATE OR REPLACE FUNCTION hmem_change_stream_group_delete_lock()
RETURNS TRIGGER AS $$
DECLARE member RECORD;
BEGIN
  FOR member IN
    SELECT workspace_id FROM workspace_group_members
     WHERE group_id = OLD.id
     ORDER BY workspace_id
  LOOP
    PERFORM hmem_change_stream_lock('workspace', member.workspace_id);
  END LOOP;
  PERFORM hmem_change_stream_lock('global', NULL);
  RETURN OLD;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_change_stream_projects
  AFTER INSERT OR UPDATE OR DELETE ON projects
  FOR EACH ROW EXECUTE FUNCTION hmem_change_stream_row_change('project', 'workspace');
CREATE TRIGGER trg_change_stream_tasks
  AFTER INSERT OR UPDATE OR DELETE ON tasks
  FOR EACH ROW EXECUTE FUNCTION hmem_change_stream_row_change('task', 'workspace');
CREATE TRIGGER trg_change_stream_observations
  AFTER INSERT OR UPDATE OR DELETE ON observations
  FOR EACH ROW EXECUTE FUNCTION hmem_change_stream_row_change('observation', 'workspace');
CREATE TRIGGER trg_change_stream_dependencies
  AFTER INSERT OR DELETE ON task_dependencies
  FOR EACH ROW EXECUTE FUNCTION hmem_change_stream_row_change('task_dependency', 'workspace');
CREATE TRIGGER trg_change_stream_dependencies_workspace
  BEFORE INSERT OR UPDATE OF task_id ON task_dependencies
  FOR EACH ROW EXECUTE FUNCTION hmem_change_stream_dependency_workspace();
CREATE TRIGGER trg_change_stream_workspaces
  AFTER INSERT OR UPDATE OR DELETE ON workspaces
  FOR EACH ROW EXECUTE FUNCTION hmem_change_stream_row_change('workspace', 'global');
CREATE TRIGGER trg_change_stream_workspace_active_state_lock
  BEFORE UPDATE OF deleted_at ON workspaces
  FOR EACH ROW EXECUTE FUNCTION hmem_change_stream_workspace_active_state_lock();
CREATE TRIGGER trg_change_stream_groups
  AFTER INSERT OR UPDATE OR DELETE ON workspace_groups
  FOR EACH ROW EXECUTE FUNCTION hmem_change_stream_row_change('workspace_group', 'global');
CREATE TRIGGER trg_change_stream_group_members
  AFTER INSERT OR DELETE ON workspace_group_members
  FOR EACH ROW EXECUTE FUNCTION hmem_change_stream_row_change('workspace_group_membership', 'workspace');
CREATE TRIGGER trg_change_stream_memberships_lock
  BEFORE INSERT OR UPDATE OR DELETE ON workspace_memberships
  FOR EACH ROW EXECUTE FUNCTION hmem_change_stream_membership_lock();
CREATE TRIGGER trg_change_stream_memberships
  AFTER INSERT OR UPDATE OR DELETE ON workspace_memberships
  FOR EACH ROW EXECUTE FUNCTION hmem_change_stream_row_change('workspace_membership', 'workspace');
CREATE TRIGGER trg_change_stream_memberships_epoch
  AFTER INSERT OR UPDATE OR DELETE ON workspace_memberships
  FOR EACH ROW EXECUTE FUNCTION hmem_change_stream_membership_epoch();
CREATE TRIGGER trg_change_stream_user_authorization_lock
  BEFORE UPDATE OF disabled_at, is_superadmin ON users
  FOR EACH ROW EXECUTE FUNCTION hmem_change_stream_user_authorization_lock();
CREATE TRIGGER trg_change_stream_group_delete_lock
  BEFORE DELETE ON workspace_groups
  FOR EACH ROW EXECUTE FUNCTION hmem_change_stream_group_delete_lock();

INSERT INTO schema_migrations (version, name)
VALUES (22, 'V022__change_stream_outbox.sql');

COMMIT;
