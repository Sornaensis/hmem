```
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
    ║    path            TEXT (UNIQUE)                               ║
    ║    gh_owner        TEXT ─┐                                     ║
    ║    gh_repo         TEXT ─┘ (UNIQUE together)                   ║
    ║    created_at      TIMESTAMPTZ                                 ║
    ║    updated_at      TIMESTAMPTZ                                 ║
    ╚═══╤═══════════╤════════════╤══════════════════╤════════════════╝
        │           │            │                  │
        │ 1         │ 1          │ 1                │ 1
        │           │            │                  │
   ┌────┘     ┌─────┘      ┌─────┘                  └──────────────────┐
   │          │            │                                           │
   ▼ *        ▼ *          ▼ *                                         ▼ *
╔══════════════════╗  ╔═══════════════════════════════════╗  ╔═══════════════════════════╗
║ memory_categories║  ║            memories               ║  ║    cleanup_policies       ║
╠══════════════════╣  ╠═══════════════════════════════════╣  ╠═══════════════════════════╣
║PK id             ║  ║ PK id               UUID          ║  ║ PK id            UUID     ║
║FK workspace_id   ║  ║ FK workspace_id      UUID         ║  ║ FK workspace_id   UUID    ║
║   name           ║  ║    content           TEXT         ║  ║    memory_type    ENUM    ║
║   description    ║  ║    summary           TEXT         ║  ║    max_age_hours  INT     ║
║FK parent_id ─────╫──╫──► (self)            UUID         ║  ║    max_count      INT     ║
║   created_at     ║  ║    memory_type       ENUM         ║  ║    min_importance  SMALL  ║
╚════════╤═════════╝  ║    importance        SMALLINT     ║  ║    enabled        BOOL    ║
         │            ║    metadata          JSONB        ║  ║    created_at     TSTZ    ║
         │            ║    embedding?        vector(1536) ║  ║    updated_at     TSTZ    ║
         │            ║    expires_at        TIMESTAMPTZ  ║  ╚═══════════════════════════╝
         │            ║    source            TEXT         ║
         │            ║    confidence        FLOAT        ║
         │            ║    pinned            BOOLEAN      ║
         │            ║    last_accessed_at  TIMESTAMPTZ  ║
         │            ║    access_count      INTEGER      ║
         │            ║    fts_language      TEXT         ║
         │            ║    search_vector     TSVECTOR     ║
         │            ║    created_at        TIMESTAMPTZ  ║
         │            ║    updated_at        TIMESTAMPTZ  ║
         │            ╚═╤═══════╤═══════════╤════════════╝
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

 ╔══════════════════════════╗
 ║    schema_migrations     ║     ┌──────────────────────────────────────────────┐
 ╠══════════════════════════╣     │           ENUM TYPES                         │
 ║ PK version    INTEGER    ║     ├──────────────────────────────────────────────┤
 ║    name       TEXT       ║     │ workspace_type_enum:                         │
 ║    applied_at TIMESTAMPTZ║     │   repository │ planning │ personal │ org     │
 ╚══════════════════════════╝     │ memory_type_enum:                            │
                                  │   short_term │ long_term                     │
                                  │ project_status_enum:                         │
                                  │   active │ paused │ completed │ archived     │
                                  │ task_status_enum:                            │
                                  │   todo │ in_progress │ blocked │ done │      │
                                  │   cancelled                                  │
                                  │ relation_type_enum:                          │
                                  │   related │ supersedes │ contradicts │       │
                                  │   elaborates │ inspires │ depends_on │       │
                                  │   derived_from │ alternative_to              │
                                  └──────────────────────────────────────────────┘
```