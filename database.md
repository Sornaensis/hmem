```
  ╔════════════════════════════════════════════════════════════════════════════╗
  ║                            AUTH / ACCESS                                  ║
  ╠════════════════════════════════════════════════════════════════════════════╣
  ║  users                                                                    ║
  ║    PK id                   UUID                                            ║
  ║       auth_subject         TEXT (UNIQUE when not null)                    ║
  ║       email                TEXT                                            ║
  ║       display_name         TEXT                                            ║
  ║       can_create_workspace BOOLEAN                                         ║
  ║       is_superadmin        BOOLEAN                                         ║
  ║       disabled_at          TIMESTAMPTZ                                     ║
  ║       created_at           TIMESTAMPTZ                                     ║
  ║       updated_at           TIMESTAMPTZ                                     ║
  ║                                                                            ║
  ║  workspace_memberships                                                    ║
  ║    PK/FK workspace_id      UUID ──► workspaces                            ║
  ║    PK/FK user_id           UUID ──► users                                 ║
  ║       role                 workspace_role_enum                            ║
  ║    FK granted_by           UUID ──► users                                 ║
  ║       created_at           TIMESTAMPTZ                                     ║
  ║       updated_at           TIMESTAMPTZ                                     ║
  ║                                                                            ║
  ║  access_tokens                                                            ║
  ║    PK id                   UUID                                            ║
  ║    FK grant_user_id        UUID ──► users                                 ║
  ║       actor_type           actor_type_enum                                ║
  ║       actor_label          TEXT                                            ║
  ║       token_hash           TEXT (UNIQUE)                                   ║
  ║       expires_at           TIMESTAMPTZ                                     ║
  ║       revoked_at           TIMESTAMPTZ                                     ║
  ║       last_used_at         TIMESTAMPTZ                                     ║
  ║       created_at           TIMESTAMPTZ                                     ║
  ╚════════════════════════════════════════════════════════════════════════════╝

                              ╔══════════════════════════════════╗
                              ║        workspace_groups          ║
                              ╠══════════════════════════════════╣
                              ║ PK id          UUID              ║
                              ║    name         TEXT (UNIQUE)    ║
                              ║    description  TEXT             ║
                              ║    created_at   TIMESTAMPTZ      ║
                              ║    updated_at   TIMESTAMPTZ      ║
                              ╚═══════════════╤══════════════════╝
                                              │
                                              │ 1
                              ╔═══════════════╧══════════════════╗
                              ║    workspace_group_members       ║
                              ╠══════════════════════════════════╣
                              ║ PK group_id     UUID  ───────────╫─── FK → workspace_groups
                              ║ PK workspace_id UUID  ───────────╫─┐  FK → workspaces
                              ║    joined_at    TIMESTAMPTZ      ║ │
                              ╚══════════════════════════════════╝ │
                                                                   │
    ╔══════════════════════════════════════════════════════════════╧═╗
    ║                          workspaces                            ║
    ╠════════════════════════════════════════════════════════════════╣
    ║ PK id              UUID                                        ║
    ║    name            TEXT                                        ║
    ║    workspace_type  workspace_type_enum                         ║
    ║    gh_owner        TEXT ─┐                                     ║
    ║    gh_repo         TEXT ─┘ (UNIQUE together)                   ║
    ║    deleted_at      TIMESTAMPTZ                                 ║
    ║    created_at      TIMESTAMPTZ                                 ║
    ║    updated_at      TIMESTAMPTZ                                 ║
    ╚═══╤═══════════╤════════════╤═══════════════╤══════╤════════════╝
        │           │            │               │      │
        │ 1         │ 1          │ 1             │ 1    │ 1
        │           │            │               │      │
   ┌────┘     ┌─────┘      ┌─────┘         ┌─────┘      └─────────────┐
   │          │            │               │                          │
   ▼ *        ▼ *          ▼ *             ▼ *                        ▼ *
╔══════════════════╗  ╔═══════════════════════════════════╗  ╔═══════════════════════════╗
║ memory_categories║  ║            memories               ║  ║    cleanup_policies       ║
╠══════════════════╣  ╠═══════════════════════════════════╣  ╠═══════════════════════════╣
║PK id             ║  ║ PK id               UUID          ║  ║ PK id            UUID     ║
║FK workspace_id   ║  ║ FK workspace_id      UUID         ║  ║ FK workspace_id   UUID    ║
║   name           ║  ║    content           TEXT         ║  ║    memory_type    ENUM    ║
║   description    ║  ║    summary           TEXT         ║  ║    max_age_hours  INT     ║
║FK parent_id ─────╫──╫──► (self)            UUID         ║  ║    max_count      INT     ║
║   deleted_at     ║  ║    memory_type       ENUM         ║  ║    min_importance SMALLINT║
║   created_at     ║  ║    importance        SMALLINT     ║  ║    enabled        BOOL    ║
╚════════╤═════════╝  ║    metadata          JSONB        ║  ║    created_at     TSTZ    ║
         │            ║    embedding?        vector(1536) ║  ║    updated_at     TSTZ    ║
         │            ║    expires_at        TIMESTAMPTZ  ║  ╚═══════════════════════════╝
         │            ║    source            TEXT         ║
         │            ║    confidence        FLOAT        ║
         │            ║    pinned            BOOLEAN      ║
         │            ║    last_accessed_at  TIMESTAMPTZ  ║
         │            ║    access_count      INTEGER      ║
         │            ║    fts_language      TEXT         ║
         │            ║    search_vector     TSVECTOR     ║
         │            ║    deleted_at        TIMESTAMPTZ  ║
         │            ║    created_at        TIMESTAMPTZ  ║
         │            ║    updated_at        TIMESTAMPTZ  ║
         │            ╚═╤═══════╤═══════════╤═════════════╝
         │              │       │           │
         │              │       │           │
         │     ┌────────┘       │           └───────────────────────────────┐
         │     │                │                                           │
         │     │ 1              │ 1                                         │ 1
         │     │                │                                           │
         │     ▼ *              ▼ *                                         │
         │  ╔════════════╗  ╔═════════════════════════════════════╗         │
         │  ║memory_tags ║  ║         memory_links                ║         │
         │  ╠════════════╣  ╠═════════════════════════════════════╣         │
         │  ║PK memory_id║  ║ PK source_id      UUID ──► memories ║         │
         │  ║PK tag  TEXT║  ║ PK target_id      UUID ──► memories ║         │
         │  ╚════════════╝  ║ PK relation_type  relation_type_enum║         │
         │                  ║    strength        FLOAT            ║         │
         ▼                  ║    created_at      TIMESTAMPTZ      ║         │
╔═══════════════════════╗   ╚═════════════════════════════════════╝         │
║ memory_category_links ║                                                   │
╠═══════════════════════╣                                                   │
║PK memory_id ──────────╫──► memories                                       │
║PK category_id ────────╫──► memory_categories                              │
╚═══════════════════════╝                                                   │
                                                                            │
   From workspaces ──────────────────────────────────┐                      │
                                                     │                      │
                                                     │ 1                    │
                                                     ▼ *                    │
                                              ╔═════════════════════════╗   │
                                              ║      projects           ║   │
                                              ╠═════════════════════════╣   │
                                              ║ PK id           UUID    ║   │
                                              ║ FK workspace_id UUID    ║   │
                                              ║ FK parent_id ───► (self)║   │
                                              ║    name         TEXT    ║   │
                                              ║    description  TEXT    ║   │
                                              ║    status       ENUM    ║   │
                                              ║    priority  SMALLINT   ║   │
                                              ║    metadata    JSONB    ║   │
                                              ║    deleted_at   TSTZ    ║   │
                                              ║    created_at   TSTZ    ║   │
                                              ║    updated_at   TSTZ    ║   │
                                              ╚══╤════════╤═════════════╝   │
                                                 │        │                 │
                          ┌──────────────────────┘        │                 │
                          │                               │ 1               │
                          │ 1                             │                 │
                          ▼ *                             ▼ *               │
               ╔══════════════════════════╗   ╔═══════════════════════════╗ │
               ║  project_memory_links    ║   ║        tasks              ║ │
               ╠══════════════════════════╣   ╠═══════════════════════════╣ │
               ║PK project_id ──► projects║   ║ PK id           UUID      ║ │
               ║PK memory_id ──► memories ║   ║ FK workspace_id UUID      ║ │
               ╚══════════════════════════╝   ║ FK project_id ──► projects║ │
                                              ║ FK parent_id ───► (self)  ║ │
                                              ║    title         TEXT     ║ │
                                              ║    description   TEXT     ║ │
                                              ║    status        ENUM     ║ │
                                              ║    priority   SMALLINT    ║ │
                                              ║    metadata     JSONB     ║ │
                                              ║    due_at        TSTZ     ║ │
                                              ║    completed_at  TSTZ     ║ │
                                              ║    deleted_at    TSTZ     ║ │
                                              ║    created_at    TSTZ     ║ │
                                              ║    updated_at    TSTZ     ║ │
                                              ╚═╤═════════════╤═══════════╝ │
                                                │             │             │
                                  ┌──────────═──┘             │             │
                                  │                           │ 1           │
                                  │ 1                         │             │
                                  ▼ *                         ▼ *           │
                   ╔═════════════════════════╗ ╔══════════════════════╗     │
                   ║   task_memory_links     ║ ║  task_dependencies   ║     │
                   ╠═════════════════════════╣ ╠══════════════════════╣     │
                   ║PK task_id ──► tasks     ║ ║PK task_id ──► tasks  ║     │
                   ║PK memory_id ──► memories║ ║PK depends_on_id      ║     │
                   ╚═════════════════════════╝ ║          ──► tasks   ║     │
                          │                    ╚══════════════════════╝     │
                          │                                                 │
                          └─────────────────────────────────────────────────┘
                                            (FK to memories)


   From workspaces ─────────────────────────┐
                                            │
                                            │ 1
                                            ▼ *
                              ╔══════════════════════════════════╗
                              ║          saved_views             ║
                              ╠══════════════════════════════════╣
                              ║ PK id            UUID            ║
                              ║ FK workspace_id  UUID            ║
                              ║    name          TEXT            ║
                              ║    description   TEXT            ║
                              ║    entity_type   TEXT            ║
                              ║    query_params  JSONB           ║
                              ║    deleted_at    TSTZ            ║
                              ║    created_at    TSTZ            ║
                              ║    updated_at    TSTZ            ║
                              ╚══════════════════════════════════╝


 ╔══════════════════════════════════╗
 ║           audit_log              ║
 ╠══════════════════════════════════╣
 ║ PK id           UUID             ║
 ║    workspace_id  UUID             ║
 ║    entity_type  TEXT             ║
 ║    entity_id    TEXT             ║
 ║    action       audit_action     ║
 ║    actor_type   actor_type_enum  ║
 ║    actor_id     TEXT             ║
 ║    actor_label  TEXT             ║
 ║    old_values   JSONB            ║
 ║    new_values   JSONB            ║
 ║    request_id   TEXT             ║
 ║    changed_at   TSTZ             ║
 ╚══════════════════════════════════╝

 ╔══════════════════════════╗
 ║    schema_migrations     ║     ┌───────────────────────────────────────────────────────┐
 ╠══════════════════════════╣     │           ENUM TYPES                                  │
 ║ PK version    INTEGER    ║     ├───────────────────────────────────────────────────────┤
 ║    name       TEXT       ║     │ workspace_type_enum:                                  │
 ║    applied_at TIMESTAMPTZ║     │   repository │ planning │ personal │ organization     │
 ╚══════════════════════════╝     │ memory_type_enum:                                     │
                                  │   short_term │ long_term                              │
                                  │ project_status_enum:                                  │
                                  │   active │ paused │ completed │ archived              │
                                  │ task_status_enum:                                     │
                                  │   todo │ in_progress │ blocked │ done │ cancelled     │
                                  │ relation_type_enum:                                   │
                                  │   related │ supersedes │ contradicts │ elaborates │   │
                                  │   inspires │ depends_on │ derived_from │              │
                                  │   alternative_to                                      │
                                  │ audit_action_enum:                                    │
                                  │   create │ update │ delete                            │
                                  │ workspace_role_enum:                                  │
                                  │   read │ edit │ admin                                │
                                  │ actor_type_enum:                                      │
                                  │   user │ bot                                          │
                                  └───────────────────────────────────────────────────────┘
```

## Auth/access table semantics

`users.disabled_at` disables provider JWT and persisted PAT/service-token
resolution for that grant-bearing user. Authorization helpers treat disabled
users as missing grants/roles, so disabled accounts fail closed even when their
workspace memberships or token rows remain in the database for later re-enable
or audit review.

The `access_tokens` table is the minimal v1 storage surface for PAT, bot, and
service-token access. It stores token identity and lifecycle metadata, not raw
bearer secrets.

- `token_hash` stores the canonical digest of the bearer token. Legacy rows use
  `sha256:`. When `auth.deployed.token_hash_secret` is configured, new
  operator-issued/rotated rows use `hmac-sha256-v1:` and the server still checks
  legacy `sha256:` as a compatibility fallback. Raw deployed token material is
  only available to the operator at issuance time.
- `actor_type = bot` means requests authenticate as a bot actor for audit and
  event attribution. The token row `id` is the stable token identity used as
  that token's bot `actor_id`; token rotation creates a replacement actor id.
- `actor_type = user` is reserved for PAT-style user attribution where the
  grant-bearing user is also the actor.
- `actor_label` is a non-secret, human-readable attribution label copied into
  audit rows and client-visible principal summaries.
- `grant_user_id` points to the canonical `users` row whose global permissions
  and `workspace_memberships` are evaluated. Bot/service tokens do not have a
  separate permissions table in v1.
- `expires_at` and `revoked_at` provide lifecycle controls for operator-managed
  issuance, rotation, and emergency revocation.
- `last_used_at` is operational usage metadata updated when persisted PATs are
  used. The access-token audit trigger ignores it so routine authentication does
  not create per-request audit noise.
- `token_hash` and `last_used_at` are omitted from `access_tokens` audit
  snapshots; audit rows should carry actor attribution and lifecycle changes,
  not secret digests or high-volume usage timestamps.

Local configured bot tokens may be represented in local config/bootstrap
material instead of `access_tokens` during the local compatibility period, but
deployed bot/service tokens should use persisted hashed rows.
