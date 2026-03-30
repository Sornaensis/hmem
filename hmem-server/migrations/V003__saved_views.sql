BEGIN;

------------------------------------------------------------------------
-- Saved views: persistent named queries that can be re-executed
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS saved_views (
    id            UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    workspace_id  UUID NOT NULL REFERENCES workspaces(id),
    name          TEXT NOT NULL,
    description   TEXT,
    entity_type   TEXT NOT NULL,
    query_params  JSONB NOT NULL DEFAULT '{}',
    deleted_at    TIMESTAMPTZ,
    created_at    TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at    TIMESTAMPTZ NOT NULL DEFAULT now(),
    CONSTRAINT chk_saved_views_name_octet_length
        CHECK (octet_length(name) <= 1024),
    CONSTRAINT chk_saved_views_description_octet_length
        CHECK (description IS NULL OR octet_length(description) <= 65536),
    CONSTRAINT chk_saved_views_entity_type
        CHECK (entity_type IN ('memory_search', 'memory_list', 'project_list', 'task_list', 'activity'))
);

CREATE INDEX IF NOT EXISTS idx_saved_views_workspace
    ON saved_views(workspace_id) WHERE deleted_at IS NULL;

COMMIT;
