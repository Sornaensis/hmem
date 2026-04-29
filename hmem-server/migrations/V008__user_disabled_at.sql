BEGIN;

ALTER TABLE users ADD COLUMN IF NOT EXISTS disabled_at TIMESTAMPTZ;

CREATE INDEX IF NOT EXISTS idx_users_disabled_at
    ON users(disabled_at)
    WHERE disabled_at IS NOT NULL;

COMMIT;
