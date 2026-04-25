BEGIN;

------------------------------------------------------------------------
-- Auth enums
------------------------------------------------------------------------

DO $$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'workspace_role_enum') THEN
    CREATE TYPE workspace_role_enum AS ENUM ('read', 'edit', 'admin');
  END IF;
END $$;

DO $$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'actor_type_enum') THEN
    CREATE TYPE actor_type_enum AS ENUM ('user', 'bot');
  END IF;
END $$;

------------------------------------------------------------------------
-- Users
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS users (
    id                   UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    auth_subject         TEXT,
    email                TEXT,
    display_name         TEXT,
    can_create_workspace BOOLEAN NOT NULL DEFAULT false,
    is_superadmin        BOOLEAN NOT NULL DEFAULT false,
    created_at           TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at           TIMESTAMPTZ NOT NULL DEFAULT now(),

    CONSTRAINT chk_users_auth_subject_octet_length
        CHECK (auth_subject IS NULL OR octet_length(auth_subject) <= 1024),
    CONSTRAINT chk_users_email_octet_length
        CHECK (email IS NULL OR octet_length(email) <= 1024),
    CONSTRAINT chk_users_display_name_octet_length
        CHECK (display_name IS NULL OR octet_length(display_name) <= 1024)
);

DROP TRIGGER IF EXISTS trg_users_updated_at ON users;
CREATE TRIGGER trg_users_updated_at
    BEFORE UPDATE ON users
    FOR EACH ROW EXECUTE FUNCTION hmem_set_updated_at();

CREATE UNIQUE INDEX IF NOT EXISTS uq_users_auth_subject
    ON users(auth_subject)
    WHERE auth_subject IS NOT NULL;

CREATE INDEX IF NOT EXISTS idx_users_email
    ON users(email)
    WHERE email IS NOT NULL;

------------------------------------------------------------------------
-- Workspace memberships
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS workspace_memberships (
    workspace_id UUID NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
    user_id      UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    role         workspace_role_enum NOT NULL,
    granted_by   UUID REFERENCES users(id) ON DELETE SET NULL,
    created_at   TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at   TIMESTAMPTZ NOT NULL DEFAULT now(),

    PRIMARY KEY (workspace_id, user_id)
);

DROP TRIGGER IF EXISTS trg_workspace_memberships_updated_at ON workspace_memberships;
CREATE TRIGGER trg_workspace_memberships_updated_at
    BEFORE UPDATE ON workspace_memberships
    FOR EACH ROW EXECUTE FUNCTION hmem_set_updated_at();

CREATE INDEX IF NOT EXISTS idx_workspace_memberships_user
    ON workspace_memberships(user_id);

CREATE INDEX IF NOT EXISTS idx_workspace_memberships_granted_by
    ON workspace_memberships(granted_by)
    WHERE granted_by IS NOT NULL;

------------------------------------------------------------------------
-- Access tokens (bot/service/PAT storage)
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS access_tokens (
    id            UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    grant_user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    actor_type    actor_type_enum NOT NULL DEFAULT 'bot',
    actor_label   TEXT NOT NULL,
    token_hash    TEXT NOT NULL,
    expires_at    TIMESTAMPTZ,
    revoked_at    TIMESTAMPTZ,
    last_used_at  TIMESTAMPTZ,
    created_at    TIMESTAMPTZ NOT NULL DEFAULT now(),

    CONSTRAINT chk_access_tokens_actor_label_octet_length
        CHECK (octet_length(actor_label) <= 1024),
    CONSTRAINT chk_access_tokens_token_hash_octet_length
        CHECK (octet_length(token_hash) <= 2048),
    CONSTRAINT chk_access_tokens_expires_at_after_created_at
        CHECK (expires_at IS NULL OR expires_at >= created_at),
    CONSTRAINT chk_access_tokens_revoked_at_after_created_at
        CHECK (revoked_at IS NULL OR revoked_at >= created_at),
    CONSTRAINT chk_access_tokens_last_used_at_after_created_at
        CHECK (last_used_at IS NULL OR last_used_at >= created_at)
);

CREATE UNIQUE INDEX IF NOT EXISTS uq_access_tokens_token_hash
    ON access_tokens(token_hash);

CREATE INDEX IF NOT EXISTS idx_access_tokens_grant_user
    ON access_tokens(grant_user_id);

CREATE INDEX IF NOT EXISTS idx_access_tokens_not_revoked
    ON access_tokens(grant_user_id, actor_type)
    WHERE revoked_at IS NULL;

CREATE INDEX IF NOT EXISTS idx_access_tokens_last_used
    ON access_tokens(last_used_at DESC)
    WHERE last_used_at IS NOT NULL;

------------------------------------------------------------------------
-- Audit log auth attribution
------------------------------------------------------------------------

ALTER TABLE audit_log ADD COLUMN IF NOT EXISTS workspace_id UUID;
ALTER TABLE audit_log ADD COLUMN IF NOT EXISTS actor_type actor_type_enum;
ALTER TABLE audit_log ADD COLUMN IF NOT EXISTS actor_id TEXT;
ALTER TABLE audit_log ADD COLUMN IF NOT EXISTS actor_label TEXT;

CREATE OR REPLACE FUNCTION hmem_audit_change()
RETURNS TRIGGER AS $$
DECLARE
  ignored_fields TEXT[] := ARRAY[]::TEXT[];
  key_columns TEXT[] := string_to_array(COALESCE(NULLIF(TG_ARGV[1], ''), 'id'), ',');
  key_name TEXT;
  old_row JSONB;
  new_row JSONB;
  entity_identifier TEXT;
  key_data JSONB := '{}'::jsonb;
  key_value TEXT;
  idx INTEGER;
  current_workspace TEXT;
  current_actor_type TEXT;
  row_workspace TEXT;
BEGIN
  IF TG_NARGS > 2 THEN
    FOR idx IN 2..TG_NARGS - 1 LOOP
      ignored_fields := array_append(ignored_fields, TG_ARGV[idx]);
    END LOOP;
  END IF;

  CASE TG_OP
    WHEN 'INSERT' THEN
      old_row := NULL;
      new_row := hmem_jsonb_without_keys(to_jsonb(NEW), ignored_fields);
    WHEN 'UPDATE' THEN
      old_row := hmem_jsonb_without_keys(to_jsonb(OLD), ignored_fields);
      new_row := hmem_jsonb_without_keys(to_jsonb(NEW), ignored_fields);
      IF old_row = new_row THEN
        RETURN NULL;
      END IF;
    WHEN 'DELETE' THEN
      old_row := hmem_jsonb_without_keys(to_jsonb(OLD), ignored_fields);
      new_row := NULL;
    ELSE
      RETURN NULL;
  END CASE;

  FOREACH key_name IN ARRAY key_columns LOOP
    key_name := btrim(key_name);
    IF key_name <> '' THEN
      key_value := coalesce(new_row ->> key_name, old_row ->> key_name);
      IF key_value IS NOT NULL THEN
        key_data := key_data || jsonb_build_object(key_name, key_value);
      END IF;
    END IF;
  END LOOP;

  entity_identifier := CASE
    WHEN array_length(key_columns, 1) = 1 THEN
      coalesce(new_row ->> btrim(key_columns[1]), old_row ->> btrim(key_columns[1]), coalesce(new_row, old_row)::text)
    WHEN key_data <> '{}'::jsonb THEN
      key_data::text
    ELSE
      coalesce(new_row, old_row)::text
  END;

  current_workspace := NULLIF(current_setting('hmem.workspace_id', true), '');
  current_actor_type := NULLIF(current_setting('hmem.actor_type', true), '');
  row_workspace := coalesce(new_row ->> 'workspace_id', old_row ->> 'workspace_id');

  IF current_workspace IS NULL AND TG_ARGV[0] = 'workspace' THEN
    current_workspace := coalesce(new_row ->> 'id', old_row ->> 'id');
  END IF;

  IF current_workspace IS NULL THEN
    current_workspace := row_workspace;
  END IF;

  IF current_workspace IS NULL THEN
    CASE TG_TABLE_NAME
      WHEN 'memory_tags' THEN
        SELECT m.workspace_id::text INTO current_workspace
          FROM memories m
         WHERE m.id = COALESCE((new_row ->> 'memory_id')::uuid, (old_row ->> 'memory_id')::uuid);
      WHEN 'memory_category_links' THEN
        SELECT m.workspace_id::text INTO current_workspace
          FROM memories m
         WHERE m.id = COALESCE((new_row ->> 'memory_id')::uuid, (old_row ->> 'memory_id')::uuid);
      WHEN 'memory_links' THEN
        SELECT m.workspace_id::text INTO current_workspace
          FROM memories m
         WHERE m.id = COALESCE((new_row ->> 'source_id')::uuid, (old_row ->> 'source_id')::uuid);
      WHEN 'project_memory_links' THEN
        SELECT p.workspace_id::text INTO current_workspace
          FROM projects p
         WHERE p.id = COALESCE((new_row ->> 'project_id')::uuid, (old_row ->> 'project_id')::uuid);
      WHEN 'task_memory_links' THEN
        SELECT t.workspace_id::text INTO current_workspace
          FROM tasks t
         WHERE t.id = COALESCE((new_row ->> 'task_id')::uuid, (old_row ->> 'task_id')::uuid);
      WHEN 'task_dependencies' THEN
        SELECT t.workspace_id::text INTO current_workspace
          FROM tasks t
         WHERE t.id = COALESCE((new_row ->> 'task_id')::uuid, (old_row ->> 'task_id')::uuid);
      ELSE
        NULL;
    END CASE;
  END IF;

  INSERT INTO audit_log (
    entity_type,
    entity_id,
    action,
    old_values,
    new_values,
    request_id,
    workspace_id,
    actor_type,
    actor_id,
    actor_label
  )
  VALUES (
    TG_ARGV[0],
    entity_identifier,
    CASE TG_OP
      WHEN 'INSERT' THEN 'create'::audit_action_enum
      WHEN 'UPDATE' THEN 'update'::audit_action_enum
      ELSE 'delete'::audit_action_enum
    END,
    old_row,
    new_row,
    NULLIF(current_setting('hmem.request_id', true), ''),
    CASE
      WHEN current_workspace IS NULL THEN NULL
      ELSE current_workspace::UUID
    END,
    CASE
      WHEN current_actor_type IS NULL THEN NULL
      ELSE current_actor_type::actor_type_enum
    END,
    NULLIF(current_setting('hmem.actor_id', true), ''),
    NULLIF(current_setting('hmem.actor_label', true), '')
  );

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE INDEX IF NOT EXISTS idx_audit_log_workspace_changed_at
    ON audit_log (workspace_id, changed_at DESC)
    WHERE workspace_id IS NOT NULL;

CREATE INDEX IF NOT EXISTS idx_audit_log_actor_changed_at
    ON audit_log (actor_type, actor_id, changed_at DESC)
    WHERE actor_id IS NOT NULL;

------------------------------------------------------------------------
-- Audit triggers for auth tables
------------------------------------------------------------------------

DROP TRIGGER IF EXISTS trg_users_audit ON users;
CREATE TRIGGER trg_users_audit
    AFTER INSERT OR UPDATE OR DELETE ON users
    FOR EACH ROW EXECUTE FUNCTION hmem_audit_change('user', 'id', 'updated_at');

DROP TRIGGER IF EXISTS trg_workspace_memberships_audit ON workspace_memberships;
CREATE TRIGGER trg_workspace_memberships_audit
    AFTER INSERT OR UPDATE OR DELETE ON workspace_memberships
    FOR EACH ROW EXECUTE FUNCTION hmem_audit_change('workspace_membership', 'workspace_id,user_id', 'updated_at');

DROP TRIGGER IF EXISTS trg_access_tokens_audit ON access_tokens;
CREATE TRIGGER trg_access_tokens_audit
    AFTER INSERT OR UPDATE OR DELETE ON access_tokens
    FOR EACH ROW EXECUTE FUNCTION hmem_audit_change('access_token', 'id');

COMMIT;
