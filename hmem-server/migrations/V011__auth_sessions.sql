BEGIN;

CREATE TABLE IF NOT EXISTS auth_sessions (
    id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    user_id         UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    session_hash    TEXT NOT NULL,
    csrf_token_hash TEXT NOT NULL,
    expires_at      TIMESTAMPTZ NOT NULL,
    revoked_at      TIMESTAMPTZ,
    last_used_at    TIMESTAMPTZ,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),

    CONSTRAINT chk_auth_sessions_session_hash_octet_length
        CHECK (octet_length(session_hash) <= 2048),
    CONSTRAINT chk_auth_sessions_csrf_token_hash_octet_length
        CHECK (octet_length(csrf_token_hash) <= 2048),
    CONSTRAINT chk_auth_sessions_expires_at_after_created_at
        CHECK (expires_at >= created_at),
    CONSTRAINT chk_auth_sessions_revoked_at_after_created_at
        CHECK (revoked_at IS NULL OR revoked_at >= created_at),
    CONSTRAINT chk_auth_sessions_last_used_at_after_created_at
        CHECK (last_used_at IS NULL OR last_used_at >= created_at)
);

CREATE UNIQUE INDEX IF NOT EXISTS uq_auth_sessions_session_hash
    ON auth_sessions(session_hash);

CREATE INDEX IF NOT EXISTS idx_auth_sessions_user_active
    ON auth_sessions(user_id, expires_at)
    WHERE revoked_at IS NULL;

CREATE INDEX IF NOT EXISTS idx_auth_sessions_expires_at
    ON auth_sessions(expires_at);

COMMIT;
