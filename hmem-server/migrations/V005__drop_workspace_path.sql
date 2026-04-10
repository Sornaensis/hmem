-- Remove the path column from workspaces.
-- Workspace discovery now uses .hmem.workspace files instead of stored paths.

BEGIN;

DROP INDEX IF EXISTS uq_workspace_path_active;
ALTER TABLE workspaces DROP COLUMN IF EXISTS path;

COMMIT;
