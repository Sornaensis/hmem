module HMem.MCP.Tools
  ( toolDefinitions
  , handleToolCall
  -- * Testing
  , parseToolCall
  , validateToolCall
    , WorkspaceVisualizationFormat(..)
  , ToolCall(..)
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int32)
import Data.List (intercalate)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Network.HTTP.Client
import Network.HTTP.Types.Header (hAccept)
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI (urlEncode)

import HMem.Types

------------------------------------------------------------------------
-- Tool definition schemas (sent to LLM via tools/list)
------------------------------------------------------------------------

toolDefinitions :: [Value]
toolDefinitions =
    [ mkTool "memory_create" "Create one or more memories in a workspace. For batch creation, pass items[] array (max 100) instead of top-level fields. Use this for initial capture, then refine later with updates, tags, links, or categories." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace"
          , "content"      .= propMaxLength "string" "The memory content" maxMemoryContentBytes
          , "summary"      .= propMaxLength "string" "Optional short summary" maxMemorySummaryBytes
          , "memory_type"  .= propEnum "string" "short_term or long_term" ["short_term", "long_term"]
          , "importance"   .= prop "integer" "1 (lowest) to 10 (highest), default 5"
          , "metadata"     .= prop "object" "Optional metadata JSON object for structured annotations"
          , "expires_at"   .= prop "string" "ISO 8601 expiration time"
          , "source"       .= prop "string" "Provenance: user_stated, inferred, tool_output, web_search"
          , "confidence"   .= prop "number" "Confidence level 0.0-1.0, default 1.0"
          , "pinned"       .= prop "boolean" "Pin this memory (default false)"
          , "tags"         .= object ["type" .= t "array", "items" .= object ["type" .= t "string"],
                                       "description" .= t "Tags for categorization"]
          , "fts_language" .= prop "string" "Full-text search language (default 'english'). Use a PostgreSQL regconfig name, e.g. 'spanish', 'german', 'simple'."
          , "items"        .= object ["type" .= t "array",
                                       "description" .= t "Batch: array of memory objects (same fields as top-level, max 100). When present, top-level fields are ignored.",
                                       "minItems" .= (1 :: Int), "maxItems" .= (100 :: Int),
                                       "items" .= object ["type" .= t "object"]]
          ]
      , "required" .= ([] :: [Text])
      ]

    , mkTool "memory_search" "Preferred discovery step before create, update, or linking work. Use 'query' for keyword search, or combine filters (workspace_id, memory_type, tags, min_importance, category_id, pinned_only) to narrow results. All parameters are optional; omit workspace_id for cross-workspace search. Returns compact results by default; set detail=true for full content and metadata." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id"   .= prop "string" "UUID of the workspace (omit for cross-workspace search)"
          , "query"          .= prop "string" "Full-text search query"
          , "memory_type"    .= propEnum "string" "Filter by type" ["short_term", "long_term"]
          , "tags"           .= object ["type" .= t "array", "items" .= object ["type" .= t "string"],
                                         "description" .= t "Filter by tags (any-match: returns memories with at least one of the given tags)"]
          , "min_importance" .= prop "integer" "Minimum importance threshold (1-10, where 10 is highest)"
          , "category_id"    .= prop "string" "Filter by category UUID"
          , "pinned_only"    .= prop "boolean" "If true, only return pinned memories"
          , "search_language" .= prop "string" "Language for query stemming (default 'english'). Use a PostgreSQL regconfig name."
          , "detail"         .= prop "boolean" "If true, return full content and metadata instead of compact summaries (default false)"
          , "limit"          .= prop "integer" "Max results (default 50)"
          , "offset"         .= prop "integer" "Offset for pagination (default 0)"
          ]
      , "required" .= ([] :: [Text])
      ]

    , mkTool "memory_get" "Get a memory by ID with full detail: content, summary, metadata, tags, importance, timestamps, source, and pinned status. Use memory_search or memory_list to discover IDs first. Tags are included in the response; no separate tag retrieval is needed." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "memory_id" .= prop "string" "UUID of the memory" ]
      , "required" .= [t "memory_id"]
      ]

    , mkTool "memory_update" "Enrich or correct existing memories. For single: pass memory_id + fields. For batch: pass items[] array (max 100) of {id, ...fields}. Use null to clear nullable fields such as summary, expires_at, or source. Also use this to pin/unpin or adjust importance." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "memory_id"   .= prop "string" "UUID of the memory to update (single mode)"
          , "content"     .= propMaxLength "string" "New content" maxMemoryContentBytes
          , "summary"     .= propMaxLength "string" "New summary (null to clear)" maxMemorySummaryBytes
          , "importance"  .= prop "integer" "New importance, 1 (lowest) to 10 (highest)"
          , "memory_type" .= propEnum "string" "New type" ["short_term", "long_term"]
          , "metadata"    .= prop "object" "New metadata JSON object"
          , "expires_at"  .= prop "string" "ISO 8601 expiration time (null to clear)"
          , "source"      .= prop "string" "Provenance (null to clear)"
          , "confidence"  .= prop "number" "Confidence level 0.0-1.0"
          , "pinned"      .= prop "boolean" "Pin or unpin this memory"
          , "items"       .= object ["type" .= t "array",
                                      "description" .= t "Batch: array of {id, ...update fields} objects (max 100). When present, top-level fields are ignored.",
                                      "minItems" .= (1 :: Int), "maxItems" .= (100 :: Int),
                                      "items" .= object ["type" .= t "object"]]
          ]
      , "required" .= ([] :: [Text])
      ]

    , mkTool "entity_lifecycle" "Soft-delete, restore, or permanently purge any entity. Use action 'delete' to soft-delete (hides from views, recoverable). Use 'restore' to undo a soft-delete. Use 'purge' to permanently and irreversibly remove (must be soft-deleted first). For batch soft-delete, pass ids[] instead of entity_id (max 100). For projects and tasks, delete/restore cascades to child subtrees." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "entity_type" .= propEnum "string" "Type of entity" ["memory", "project", "task", "category", "workspace", "saved_view"]
          , "entity_id"   .= prop "string" "UUID of the entity (for single operations)"
          , "ids"          .= object ["type" .= t "array", "items" .= object ["type" .= t "string"],
                                       "description" .= t "Array of entity UUIDs for batch delete (max 100, action must be 'delete')",
                                       "minItems" .= (1 :: Int), "maxItems" .= (100 :: Int)]
          , "action"       .= propEnum "string" "Lifecycle action" ["delete", "restore", "purge"]
          ]
      , "required" .= [t "entity_type", t "action"]
      ]

    , mkTool "memory_link" "Manage typed links between memories. Actions: 'create' adds a link, 'remove' deletes one, 'list' shows all links for a memory, 'graph' returns the connected subgraph up to a depth, 'find' finds all links of a relation type in a workspace. Relation types: related, supersedes, contradicts, elaborates, inspires, depends_on, derived_from, alternative_to." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "action"        .= propEnum "string" "Operation to perform" ["create", "remove", "list", "graph", "find"]
          , "source_id"     .= prop "string" "Source memory UUID (required for create, remove)"
          , "target_id"     .= prop "string" "Target memory UUID (required for create, remove)"
          , "relation_type" .= propEnum "string" "Relation type (required for create, remove, find)"
              ["related", "supersedes", "contradicts", "elaborates", "inspires", "depends_on", "derived_from", "alternative_to"]
          , "strength"      .= prop "number" "Link strength 0.0-1.0, default 1.0 (create only)"
          , "memory_id"     .= prop "string" "Memory UUID (required for list, graph)"
          , "depth"         .= prop "integer" "Max traversal depth 1-5, default 2 (graph only)"
          , "workspace_id"  .= prop "string" "Workspace UUID (required for find)"
          ]
      , "required" .= [t "action"]
      ]

    , mkTool "project_create" "Create a top-level or child project in a workspace. Set parent_id to create a subproject under an existing project. After creating, add tasks with task_create and link relevant memories with project_link_memory. Use project_overview to inspect the result." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace"
          , "name"         .= propMaxLength "string" "Project name" maxNameBytes
          , "description"  .= propMaxLength "string" "Project description" maxDescriptionBytes
          , "parent_id"    .= prop "string" "Parent project UUID for sub-projects"
          , "priority"     .= prop "integer" "1 (lowest) to 10 (highest), default 5"
          , "metadata"     .= prop "object" "Optional metadata JSON object"
          ]
      , "required" .= [t "workspace_id", t "name"]
      ]

    , mkTool "project_list" "Browse or filter projects to find IDs before update, linking, deletion, or task creation. Omit workspace_id for cross-workspace browsing." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace (omit for cross-workspace browsing)"
          , "status"       .= propEnum "string" "Filter by status"
              ["active", "paused", "completed", "archived"]
          , "created_after"  .= prop "string" "Filter for projects created on or after this ISO 8601 timestamp"
          , "created_before" .= prop "string" "Filter for projects created on or before this ISO 8601 timestamp"
          , "updated_after"  .= prop "string" "Filter for projects updated on or after this ISO 8601 timestamp"
          , "updated_before" .= prop "string" "Filter for projects updated on or before this ISO 8601 timestamp"
          , "limit"        .= prop "integer" "Max results (default 50)"
          , "offset"       .= prop "integer" "Offset for pagination (default 0)"
          ]
      , "required" .= ([] :: [Text])
      ]

    , mkTool "task_create" "Create a workspace- or project-scoped task. Use parent_id for subtasks; when parent_id is set, project_id should match the parent task's project. After creating, add ordering constraints with task_dependency, and attach relevant context with task_link_memory." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace"
          , "project_id"  .= prop "string" "UUID of the project (optional)"
          , "title"       .= propMaxLength "string" "Task title" maxNameBytes
          , "description" .= propMaxLength "string" "Task description" maxDescriptionBytes
          , "parent_id"   .= prop "string" "Parent task UUID for sub-tasks"
          , "priority"    .= prop "integer" "1 (lowest) to 10 (highest), default 5"
          , "metadata"    .= prop "object" "Optional metadata JSON object"
          , "due_at"      .= prop "string" "ISO 8601 due date"
          ]
      , "required" .= [t "workspace_id", t "title"]
      ]

    , mkTool "task_list" "Browse or filter tasks to find IDs before update, linking, dependency changes, or deletion. Use workspace_id for workspace-wide listing or project_id for project-scoped listing; both are optional." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace (use this or project_id)"
          , "project_id" .= prop "string" "UUID of the project (use this or workspace_id)"
          , "status"     .= propEnum "string" "Filter by status"
              ["todo", "in_progress", "blocked", "done", "cancelled"]
          , "priority"   .= prop "integer" "Filter by exact priority value (1-10, where 10 is highest)"
          , "created_after"  .= prop "string" "Filter for tasks created on or after this ISO 8601 timestamp"
          , "created_before" .= prop "string" "Filter for tasks created on or before this ISO 8601 timestamp"
          , "updated_after"  .= prop "string" "Filter for tasks updated on or after this ISO 8601 timestamp"
          , "updated_before" .= prop "string" "Filter for tasks updated on or before this ISO 8601 timestamp"
          , "limit"      .= prop "integer" "Max results (default 50)"
          , "offset"     .= prop "integer" "Offset for pagination (default 0)"
          ]
      , "required" .= ([] :: [Text])
      ]

    , mkTool "task_update" "Update one or more tasks. For single: pass task_id + fields. For batch: pass items[] array (max 100) of {id, ...fields}. Use project_id and/or parent_id to reorganize; null clears those fields. Moving a task across projects also moves its subtree." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "task_id"     .= prop "string" "UUID of the task (single mode)"
          , "title"       .= propMaxLength "string" "New title" maxNameBytes
          , "description" .= propMaxLength "string" "New description (null to clear)" maxDescriptionBytes
          , "project_id"  .= prop "string" "New project UUID (null to clear)"
          , "parent_id"   .= prop "string" "New parent task UUID (null to clear)"
          , "status"      .= propEnum "string" "New status"
              ["todo", "in_progress", "blocked", "done", "cancelled"]
          , "priority"    .= prop "integer" "New priority, 1 (lowest) to 10 (highest)"
          , "metadata"    .= prop "object" "New metadata JSON object"
          , "due_at"      .= prop "string" "ISO 8601 due date (null to clear)"
          , "items"       .= object ["type" .= t "array",
                                      "description" .= t "Batch: array of {id, ...update fields} objects (max 100). When present, top-level fields are ignored.",
                                      "minItems" .= (1 :: Int), "maxItems" .= (100 :: Int),
                                      "items" .= object ["type" .= t "object"]]
          ]
      , "required" .= ([] :: [Text])
      ]

    , mkTool "workspace_register" "Register a new workspace — the top-level container for memories, projects, and tasks. Use type 'repository' for code repos (set path), 'planning' for cross-repo coordination, 'personal' for individual notes, or 'organization' for team scope." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "name"           .= propMaxLength "string" "Workspace name" maxNameBytes
          , "path"           .= prop "string" "Filesystem path"
          , "gh_owner"       .= prop "string" "GitHub owner"
          , "gh_repo"        .= prop "string" "GitHub repository"
          , "workspace_type" .= propEnum "string" "Workspace type"
              ["repository", "planning", "personal", "organization"]
          ]
      , "required" .= [t "name"]
      ]

    , mkTool "workspace_list" "List registered workspaces to find IDs before updates, grouping, cleanup, or cross-workspace search." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "limit"  .= prop "integer" "Max results (default 50)"
          , "offset" .= prop "integer" "Offset for pagination (default 0)"
          ]
      ]

    , mkTool "workspace_get" "Get a workspace by ID with full detail: name, type, path, GitHub info, and timestamps." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace" ]
      , "required" .= [t "workspace_id"]
      ]

    , mkTool "workspace_visualization" "Generate a workspace visualization as SVG by default, or return the raw JSON graph with format=json. Supports project inclusion/exclusion, task status filtering for task-derived memory links, memory filtering, and SVG-only task rendering options. Task status summaries are shown in SVG by default unless disabled." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace"
          , "format" .= propEnum "string" "Output format (default svg)" ["svg", "json"]
          , "show_tasks" .= prop "boolean" "If true, include task nodes in SVG output (ignored for JSON)"
          , "show_task_status_summary" .= prop "boolean" "If false, suppress task status count summaries in SVG output (defaults to true)"
          , "show_descriptions" .= prop "boolean" "If true, show project/task descriptions in SVG cards (defaults to false)"
          , "include_project_ids" .= object
              [ "type" .= t "array"
              , "items" .= object ["type" .= t "string"]
              , "description" .= t "If provided, only include these project IDs"
              ]
          , "exclude_project_ids" .= object
              [ "type" .= t "array"
              , "items" .= object ["type" .= t "string"]
              , "description" .= t "Project IDs to exclude"
              ]
          , "task_statuses" .= object
              [ "type" .= t "array"
              , "items" .= object
                  [ "type" .= t "string"
                  , "enum" .= ([t "todo", t "in_progress", t "blocked", t "done", t "cancelled"] :: [Text])
                  ]
              , "description" .= t "Optional task statuses to include"
              ]
          , "memory_filter" .= object
              [ "type" .= t "object"
              , "description" .= t "Optional memory filter applied to returned memories and memory edges"
              , "properties" .= object
                  [ "memory_type" .= propEnum "string" "Filter by memory type" ["short_term", "long_term"]
                  , "tags" .= object ["type" .= t "array", "items" .= object ["type" .= t "string"], "description" .= t "Require at least one matching tag"]
                  , "min_importance" .= prop "integer" "Minimum importance threshold (1-10, where 10 is highest)"
                  , "pinned_only" .= prop "boolean" "If true, only include pinned memories"
                  ]
              ]
          ]
      , "required" .= [t "workspace_id"]
      ]

    , mkTool "workspace_update" "Update workspace identity or scoping fields. Use null to clear path or GitHub fields when needed." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id"   .= prop "string" "UUID of the workspace"
          , "name"           .= propMaxLength "string" "New workspace name" maxNameBytes
          , "workspace_type" .= propEnum "string" "New workspace type"
              ["repository", "planning", "personal", "organization"]
          , "path"           .= prop "string" "New filesystem path (null to clear)"
          , "gh_owner"       .= prop "string" "New GitHub owner (null to clear)"
          , "gh_repo"        .= prop "string" "New GitHub repository (null to clear)"
          ]
      , "required" .= [t "workspace_id"]
      ]



    , mkTool "memory_list" "Browse memories and collect IDs. Returns compact results by default; set detail=true for full content and metadata. Use memory_search instead when you need keyword or filtered retrieval." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace (omit for all workspaces)"
          , "memory_type"  .= propEnum "string" "Filter by type" ["short_term", "long_term"]
          , "created_after"  .= prop "string" "Filter for memories created on or after this ISO 8601 timestamp"
          , "created_before" .= prop "string" "Filter for memories created on or before this ISO 8601 timestamp"
          , "updated_after"  .= prop "string" "Filter for memories updated on or after this ISO 8601 timestamp"
          , "updated_before" .= prop "string" "Filter for memories updated on or before this ISO 8601 timestamp"
          , "detail"       .= prop "boolean" "If true, return full content and metadata instead of compact summaries (default false)"
          , "limit"        .= prop "integer" "Max results (default 50)"
          , "offset"       .= prop "integer" "Offset for pagination (default 0)"
          ]
      , "required" .= ([] :: [Text])
      ]

  , mkTool "list_entity_memories" "List all memories linked to a project, task, or category." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "entity_type" .= propEnum "string" "Type of entity" ["project", "task", "category"]
          , "entity_id"   .= prop "string" "UUID of the entity"
          ]
      , "required" .= [t "entity_type", t "entity_id"]
      ]

    , mkTool "cleanup_run" "Run cleanup policies for a workspace immediately. Use after reviewing policies or when you want explicit pruning now." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace" ]
      , "required" .= [t "workspace_id"]
      ]

  -- Issue 1: Missing project get/update/delete
    , mkTool "project_get" "Get a project by ID with full detail: name, description, status, priority, parent_id, metadata, and timestamps. Use project_list to discover IDs." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "project_id" .= prop "string" "UUID of the project" ]
      , "required" .= [t "project_id"]
      ]

    , mkTool "project_update" "Update one or more projects. For single: pass project_id + fields. For batch: pass items[] array (max 100) of {id, ...fields}. Use parent_id=null to move it back to the top level." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "project_id"  .= prop "string" "UUID of the project (single mode)"
          , "name"        .= propMaxLength "string" "New name" maxNameBytes
          , "description" .= propMaxLength "string" "New description (null to clear)" maxDescriptionBytes
          , "parent_id"   .= prop "string" "New parent project UUID (null to clear)"
          , "status"      .= propEnum "string" "New status"
              ["active", "paused", "completed", "archived"]
          , "priority"    .= prop "integer" "New priority, 1 (lowest) to 10 (highest)"
          , "metadata"    .= prop "object" "New metadata JSON object"
          , "items"       .= object ["type" .= t "array",
                                      "description" .= t "Batch: array of {id, ...update fields} objects (max 100). When present, top-level fields are ignored.",
                                      "minItems" .= (1 :: Int), "maxItems" .= (100 :: Int),
                                      "items" .= object ["type" .= t "object"]]
          ]
      , "required" .= ([] :: [Text])
      ]



  , mkTool "project_overview" "Get a project with its tasks, subprojects, and linked memories in one call. Use this to understand the full scope of a project before making changes, instead of calling project_get, task_list, and project_list_memories separately." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "project_id" .= prop "string" "UUID of the project" ]
      , "required" .= [t "project_id"]
      ]

  -- Issue 1: Missing task get/delete
    , mkTool "task_get" "Get a task by ID with full detail: title, description, status, priority, project_id, parent_id, due_at, metadata, and timestamps. Use task_list to discover IDs." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "task_id" .= prop "string" "UUID of the task" ]
      , "required" .= [t "task_id"]
      ]

    , mkTool "task_overview" "Get a task with dependency summaries and connected memories. Set extra_context=true to also pull in project- and workspace-linked memories for broader LLM context." $ object
            [ "type" .= t "object"
            , "properties" .= object
                    [ "task_id" .= prop "string" "UUID of the task"
                    , "extra_context" .= prop "boolean" "If true, include project and workspace memories in addition to task-linked memories"
                    ]
            , "required" .= [t "task_id"]
            ]

    , mkTool "context_get" "Get relevant memories for a task, automatically collecting from the task itself, all ancestor projects, and the workspace. Use detail_level to control how many memories per scope: light (2 each), medium (5 each, default), heavy (10 each). Returns memories grouped by scope, ranked by importance. Use this before starting work on a task to load relevant context." $ object
            [ "type" .= t "object"
            , "properties" .= object
                    [ "task_id"      .= prop "string" "UUID of the task"
                    , "detail_level" .= propEnum "string" "How many memories per scope: light=2, medium=5 (default), heavy=10" ["light", "medium", "heavy"]
                    ]
            , "required" .= [t "task_id"]
            ]



  -- Issue 3: Task dependencies
    , mkTool "task_dependency" "Add or remove an ordering dependency between tasks. Use action 'add' to declare that the first task depends on the second (must complete before it). Use action 'remove' to delete the constraint. For parent/child hierarchy, use parent_id in task_create instead." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "action"        .= propEnum "string" "Whether to add or remove the dependency" ["add", "remove"]
          , "task_id"       .= prop "string" "UUID of the task"
          , "depends_on_id" .= prop "string" "UUID of the task it depends on"
          ]
      , "required" .= [t "action", t "task_id", t "depends_on_id"]
      ]

  , mkTool "link_memory" "Link or unlink memories to/from a project, task, or category. Use action 'link' to attach memories, 'unlink' to detach. Supports single or batch operations via memory_ids array (max 100). For categories, link/unlink always operates on a single memory_id." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "entity_type" .= propEnum "string" "Type of entity to link memories to" ["project", "task", "category"]
          , "entity_id"   .= prop "string" "UUID of the entity"
          , "action"       .= propEnum "string" "Whether to link or unlink" ["link", "unlink"]
          , "memory_ids"   .= object ["type" .= t "array", "items" .= object ["type" .= t "string"],
                                       "description" .= t "Array of memory UUIDs (max 100)",
                                       "minItems" .= (1 :: Int), "maxItems" .= (100 :: Int)]
          ]
      , "required" .= [t "entity_type", t "entity_id", t "action", t "memory_ids"]
      ]

  -- Issue 7: Set tags on a memory
    , mkTool "memory_set_tags" "Set tags on one or more memories, replacing existing tags. For single: pass memory_id + tags. For batch: pass items[] array (max 100) of {memory_id, tags}. Check existing tags with memory_get first if you need to merge." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "memory_id" .= prop "string" "UUID of the memory (single mode)"
          , "tags"      .= object ["type" .= t "array", "items" .= object ["type" .= t "string"],
                                    "description" .= t "Tags to set on the memory (single mode)"]
          , "items"     .= object ["type" .= t "array",
                                    "description" .= t "Batch: array of {memory_id, tags} objects (max 100). When present, top-level fields are ignored.",
                                    "minItems" .= (1 :: Int), "maxItems" .= (100 :: Int),
                                    "items" .= object ["type" .= t "object"]]
          ]
      , "required" .= ([] :: [Text])
      ]

  -- Collapsed CRUD tools
  , mkTool "category" "Manage memory categories. Actions: 'create' (name required, optional workspace_id, description, parent_id), 'get' (category_id), 'list' (optional workspace_id, limit, offset), 'update' (category_id + fields to change). Lifecycle (delete/restore/purge) uses entity_lifecycle." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "action"       .= propEnum "string" "Operation to perform" ["create", "get", "list", "update"]
          , "category_id"  .= prop "string" "UUID of the category (required for get, update)"
          , "workspace_id" .= prop "string" "UUID of the workspace (optional for create, list)"
          , "name"         .= propMaxLength "string" "Category name (required for create)" maxNameBytes
          , "description"  .= prop "string" "Category description (create, update)"
          , "parent_id"    .= prop "string" "Parent category UUID (create, update)"
          , "limit"        .= prop "integer" "Max results for list (default 50)"
          , "offset"       .= prop "integer" "Offset for list pagination (default 0)"
          ]
      , "required" .= [t "action"]
      ]

  , mkTool "saved_view" "Manage saved views — reusable queries you can execute later. Actions: 'create' (workspace_id, name, entity_type, optional query_params), 'get' (view_id), 'list' (workspace_id), 'update' (view_id + fields), 'execute' (view_id, optional limit/offset/detail overrides). Lifecycle uses entity_lifecycle." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "action"       .= propEnum "string" "Operation to perform" ["create", "get", "list", "update", "execute"]
          , "view_id"      .= prop "string" "UUID of the saved view (required for get, update, execute)"
          , "workspace_id" .= prop "string" "UUID of the workspace (required for create, list)"
          , "name"         .= propMaxLength "string" "View name (required for create)" maxNameBytes
          , "description"  .= prop "string" "Optional description (create, update)"
          , "entity_type"  .= propEnum "string" "Which entity this view queries (required for create)" ["memory_search", "memory_list", "project_list", "task_list", "activity"]
          , "query_params" .= prop "object" "Query parameters matching the entity_type's schema (create, update)"
          , "limit"        .= prop "integer" "Max results (list, execute; default 50)"
          , "offset"       .= prop "integer" "Offset for pagination (list, execute; default 0)"
          , "detail"       .= prop "boolean" "If true, return full detail (execute only; default false)"
          ]
      , "required" .= [t "action"]
      ]

  -- ================================================================
  -- WORKFLOW TOOLS — composite operations for common task/project flows
  -- ================================================================

  , mkTool "task_start" "Begin work on a task: sets status to in_progress and loads relevant context (task-linked, project-ancestor, and workspace memories). Use this at the start of every work session on a task instead of manually calling task_update + context_get." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "task_id"      .= prop "string" "UUID of the task to start"
          , "detail_level" .= propEnum "string" "How many memories per scope: light=2, medium=5 (default), heavy=10" ["light", "medium", "heavy"]
          ]
      , "required" .= [t "task_id"]
      ]

  , mkTool "task_finish" "Finish working on a task: optionally records notes as a linked memory, then updates task status. Use status 'done' for completion, 'blocked' when stuck, 'cancelled' to abandon. Notes are stored as a long_term memory linked to the task." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "task_id" .= prop "string" "UUID of the task"
          , "status"  .= propEnum "string" "New task status" ["done", "blocked", "cancelled"]
          , "notes"   .= prop "string" "Optional work notes to save as a linked memory (findings, decisions, blockers)"
          , "tags"    .= object ["type" .= t "array", "items" .= object ["type" .= t "string"],
                                  "description" .= t "Tags for the notes memory (e.g. decision, blocker, finding)"]
          ]
      , "required" .= [t "task_id", t "status"]
      ]

  , mkTool "project_spec" "Create a project and its initial tasks in one call. Use this to set up a structured work plan from a specification. Tasks are created under the new project in the order given." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace"
          , "name"         .= propMaxLength "string" "Project name" maxNameBytes
          , "description"  .= propMaxLength "string" "Project description" maxDescriptionBytes
          , "priority"     .= prop "integer" "Project priority, 1 (lowest) to 10 (highest), default 5"
          , "tasks"        .= object
              [ "type" .= t "array"
              , "description" .= t "Tasks to create under the project"
              , "minItems" .= (1 :: Int)
              , "maxItems" .= (50 :: Int)
              , "items" .= object
                  [ "type" .= t "object"
                  , "properties" .= object
                      [ "title"       .= propMaxLength "string" "Task title" maxNameBytes
                      , "description" .= propMaxLength "string" "Task description" maxDescriptionBytes
                      , "priority"    .= prop "integer" "Task priority, 1 (lowest) to 10 (highest), default 5"
                      ]
                  , "required" .= [t "title"]
                  ]
              ]
          ]
      , "required" .= [t "workspace_id", t "name", t "tasks"]
      ]

  , mkTool "project_archive" "Archive a completed project: sets status to archived, optionally records a summary as a linked long_term memory. Use this when a project is done and you want to preserve a concise record of outcomes." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "project_id" .= prop "string" "UUID of the project to archive"
          , "summary"    .= prop "string" "Optional project summary to save as a linked long_term memory"
          ]
      , "required" .= [t "project_id"]
      ]

  , mkTool "search" "Unified full-text search across memories, projects, and tasks. Searches all three entity types in parallel by default; use entity_types to restrict scope. Project and task results include their linked memory summaries. Use this as the primary discovery tool before making changes." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id"     .= prop "string" "UUID of the workspace (omit for cross-workspace search)"
          , "query"            .= prop "string" "Full-text search query (required)"
          , "entity_types"     .= object ["type" .= t "array", "items" .= propEnum "string" "Entity types to search" ["memory", "project", "task"],
                                           "description" .= t "Which entity types to search (default: all three)"]
          , "search_language"  .= prop "string" "Language for query stemming (default 'english'). PostgreSQL regconfig name."
          , "limit"            .= prop "integer" "Max results per entity type (default 10)"
          , "offset"           .= prop "integer" "Offset for pagination (default 0)"
          -- Memory-specific filters
          , "memory_type"      .= propEnum "string" "Filter memories by type" ["short_term", "long_term"]
          , "tags"             .= object ["type" .= t "array", "items" .= object ["type" .= t "string"],
                                           "description" .= t "Filter memories by tags (any-match)"]
          , "min_importance"   .= prop "integer" "Minimum importance for memories (1-10)"
          , "category_id"      .= prop "string" "Filter memories by category UUID"
          , "pinned_only"      .= prop "boolean" "If true, only return pinned memories"
          -- Project-specific filters
          , "project_status"   .= propEnum "string" "Filter projects by status" ["active", "paused", "completed", "archived"]
          -- Task-specific filters
          , "task_status"      .= propEnum "string" "Filter tasks by status" ["todo", "in_progress", "blocked", "done", "cancelled"]
          , "task_priority"    .= prop "integer" "Filter tasks by exact priority (1-10)"
          , "project_id"       .= prop "string" "Filter tasks by project UUID"
          ]
      , "required" .= [t "query"]
      ]
  ]

------------------------------------------------------------------------
-- Typed tool calls
------------------------------------------------------------------------

data WorkspaceVisualizationFormat
    = WorkspaceVisualizationSvg
    | WorkspaceVisualizationJson
    deriving (Show, Eq)

instance FromJSON WorkspaceVisualizationFormat where
    parseJSON = withText "WorkspaceVisualizationFormat" $ \formatName ->
        case formatName of
            "svg" -> pure WorkspaceVisualizationSvg
            "json" -> pure WorkspaceVisualizationJson
            _ -> fail ("Unknown workspace visualization format: " <> T.unpack formatName)

data ToolCall
  = MemoryCreate   CreateMemory
  | MemoryCreateBatch [CreateMemory]
  | MemorySearch   SearchQuery Bool         -- query, detail
  | MemoryGet      UUID
  | MemoryUpdate   UUID UpdateMemory
  | MemoryDelete   UUID
    | MemoryRestore  UUID
    | MemoryPurge    UUID
  | LinkMemories   UUID CreateMemoryLink   -- source_id, link body
  | MemoryLinksList UUID
  | ProjectCreate  CreateProject
  | ProjectGet     UUID
  | ProjectUpdate  UUID UpdateProject
  | ProjectDelete  UUID
    | ProjectRestore UUID
    | ProjectPurge   UUID
    | ProjectList    ProjectListQuery
  | ProjectLinkMem UUID UUID               -- project_id, memory_id
  | ProjectUnlinkMem UUID UUID             -- project_id, memory_id
  | TaskCreate     CreateTask
  | TaskGet        UUID
    | TaskOverviewCall UUID Bool
    | ContextGetCall UUID ContextDetailLevel
  | TaskDelete     UUID
    | TaskRestore    UUID
    | TaskPurge      UUID
    | TaskList       TaskListQuery
  | TaskUpdate     UUID UpdateTask
  | TaskLinkMem    UUID UUID               -- task_id, memory_id
  | TaskUnlinkMem  UUID UUID               -- task_id, memory_id
  | TaskDepAdd     UUID UUID               -- task_id, depends_on_id
  | TaskDepRemove  UUID UUID               -- task_id, depends_on_id
  | WorkspaceList (Maybe Int) (Maybe Int)
  | WorkspaceGet   UUID
        | WorkspaceVisualizationCall UUID WorkspaceVisualizationQuery WorkspaceVisualizationFormat
  | WsUpdate       UUID UpdateWorkspace
  | WsDelete       UUID
    | WsRestore      UUID
    | WsPurge        UUID
  | CategoryCreate CreateMemoryCategory
  | CategoryGet    UUID
  | CategoryList   (Maybe UUID) (Maybe Int) (Maybe Int) -- maybe workspace_id, limit, offset
  | CategoryUpdate UUID UpdateMemoryCategory
  | CategoryDelete UUID
    | CategoryRestore UUID
    | CategoryPurge  UUID
  | CategoryLinkMem   UUID UUID            -- memory_id, category_id
  | CategoryUnlinkMem UUID UUID            -- memory_id, category_id
  | MemorySetTags   UUID [Text]            -- memory_id, tags
  | MemoryUnlink    UUID UUID RelationType -- source_id, target_id, relation_type
  | WorkspaceReg   CreateWorkspace
  | CleanupRun     UUID
    | MemoryList     MemoryListQuery Bool      -- query, detail
  | MemoryGraphCall UUID (Maybe Int)
  | MemoryFindByRelation UUID RelationType
  | ProjectListMem UUID
    | CategoryListMem UUID
  | TaskListMem    UUID
  | MemoryDeleteBatch [UUID]
  | TaskDeleteBatch [UUID]
    | ProjectDeleteBatch [UUID]
    | CategoryDeleteBatch [UUID]
  | TaskMoveBatch [UUID] (Maybe UUID)     -- task_ids, project_id
  | ProjectLinkMemBatch UUID [UUID]       -- project_id, memory_ids
    | CategoryLinkMemBatch UUID [UUID]      -- category_id, memory_ids
  | TaskLinkMemBatch UUID [UUID]          -- task_id, memory_ids
  | MemorySetTagsBatch [(UUID, [Text])]   -- [(memory_id, tags)]
  | MemoryUpdateBatch [(UUID, UpdateMemory)]  -- [(memory_id, update)]
    | ProjectUpdateBatch [(UUID, UpdateProject)] -- [(project_id, update)]
  | TaskUpdateBatch [(UUID, UpdateTask)]      -- [(task_id, update)]
  | SavedViewCreate CreateSavedView
  | SavedViewList UUID (Maybe Int) (Maybe Int) -- workspace_id, limit, offset
  | SavedViewGet UUID
  | SavedViewUpdate UUID UpdateSavedView
  | SavedViewDelete UUID
    | SavedViewRestore UUID
  | SavedViewPurge UUID
  | SavedViewExecute UUID (Maybe Int) (Maybe Int) (Maybe Bool) -- view_id, limit, offset, detail
  | ProjectOverviewCall UUID
  -- Workflow composite tools
  | TaskStartCall UUID ContextDetailLevel
  | TaskFinishCall UUID TaskStatus (Maybe Text) (Maybe [Text])   -- task_id, status, notes, tags
  | ProjectSpecCall UUID Text (Maybe Text) (Maybe Int32) [SpecTask] -- ws_id, name, desc, priority, tasks
  | ProjectArchiveCall UUID (Maybe Text)                         -- project_id, summary
  | UnifiedSearch UnifiedSearchQuery
  deriving (Show, Eq)

-- | A task stub for project_spec — just the fields needed to create a task.
data SpecTask = SpecTask
  { stTitle       :: Text
  , stDescription :: Maybe Text
  , stPriority    :: Maybe Int32
  } deriving (Show, Eq)

-- | Parse raw JSON-RPC params (containing "name" and "arguments") into
-- a typed ToolCall, validating all fields against HMem.Types.
parseToolCall :: Text -> Value -> Either String ToolCall
parseToolCall name args = case name of
    "memory_create"            -> do
        mItems <- opt "items" :: Either String (Maybe [Value])
        case mItems of
            Just items -> MemoryCreateBatch <$> mapM (parseEither parseJSON) items
            Nothing    -> MemoryCreate <$> parse args
    "memory_search"            -> MemorySearch <$> parse args <*> (maybe False id <$> opt "detail")
    "memory_get"               -> MemoryGet <$> need "memory_id"
    "memory_update"            -> do
        mItems <- opt "items" :: Either String (Maybe [Value])
        case mItems of
            Just items -> MemoryUpdateBatch <$> parseBatchUpdateItems' items
            Nothing    -> MemoryUpdate <$> need "memory_id" <*> parse args
    "entity_lifecycle"          -> do
        entityType <- need "entity_type" :: Either String Text
        action <- need "action" :: Either String Text
        mIds <- opt "ids" :: Either String (Maybe [UUID])
        mEid <- opt "entity_id" :: Either String (Maybe UUID)
        case (action, mIds) of
            ("delete", Just ids) -> case entityType of
                "memory"   -> Right $ MemoryDeleteBatch ids
                "project"  -> Right $ ProjectDeleteBatch ids
                "task"     -> Right $ TaskDeleteBatch ids
                "category" -> Right $ CategoryDeleteBatch ids
                _          -> Left $ "entity_lifecycle: batch delete not supported for " <> T.unpack entityType
            _ -> case mEid of
                Nothing -> Left "entity_lifecycle: entity_id is required for non-batch operations"
                Just eid -> case (entityType, action) of
                    ("memory",     "delete")  -> Right $ MemoryDelete eid
                    ("memory",     "restore") -> Right $ MemoryRestore eid
                    ("memory",     "purge")   -> Right $ MemoryPurge eid
                    ("project",    "delete")  -> Right $ ProjectDelete eid
                    ("project",    "restore") -> Right $ ProjectRestore eid
                    ("project",    "purge")   -> Right $ ProjectPurge eid
                    ("task",       "delete")  -> Right $ TaskDelete eid
                    ("task",       "restore") -> Right $ TaskRestore eid
                    ("task",       "purge")   -> Right $ TaskPurge eid
                    ("category",   "delete")  -> Right $ CategoryDelete eid
                    ("category",   "restore") -> Right $ CategoryRestore eid
                    ("category",   "purge")   -> Right $ CategoryPurge eid
                    ("workspace",  "delete")  -> Right $ WsDelete eid
                    ("workspace",  "restore") -> Right $ WsRestore eid
                    ("workspace",  "purge")   -> Right $ WsPurge eid
                    ("saved_view", "delete")  -> Right $ SavedViewDelete eid
                    ("saved_view", "restore") -> Right $ SavedViewRestore eid
                    ("saved_view", "purge")   -> Right $ SavedViewPurge eid
                    _ -> Left $ "entity_lifecycle: invalid entity_type/action: " <> T.unpack entityType <> "/" <> T.unpack action
    "memory_link"              -> do
        action <- need "action" :: Either String Text
        case action of
            "create" -> LinkMemories <$> need "source_id" <*> parse args
            "remove" -> MemoryUnlink <$> need "source_id" <*> need "target_id" <*> need "relation_type"
            "list"   -> MemoryLinksList <$> need "memory_id"
            "graph"  -> MemoryGraphCall <$> need "memory_id" <*> opt "depth"
            "find"   -> MemoryFindByRelation <$> need "workspace_id" <*> need "relation_type"
            _        -> Left "memory_link: action must be 'create', 'remove', 'list', 'graph', or 'find'"
    "project_create"           -> ProjectCreate <$> parse args
    "project_get"              -> ProjectGet <$> need "project_id"
    "project_update"           -> do
        mItems <- opt "items" :: Either String (Maybe [Value])
        case mItems of
            Just items -> ProjectUpdateBatch <$> parseBatchUpdateItems' items
            Nothing    -> ProjectUpdate <$> need "project_id" <*> parse args
    "project_list"             -> ProjectList <$> parse args
    "link_memory"              -> do
        entityType <- need "entity_type" :: Either String Text
        eid <- need "entity_id"
        action <- need "action" :: Either String Text
        mids <- need "memory_ids" :: Either String [UUID]
        case (entityType, action) of
            ("project",  "link")   -> case mids of
                [mid] -> Right $ ProjectLinkMem eid mid
                _     -> Right $ ProjectLinkMemBatch eid mids
            ("project",  "unlink") -> case mids of
                [mid] -> Right $ ProjectUnlinkMem eid mid
                _     -> Left "link_memory: project unlink only supports one memory_id at a time"
            ("task",     "link")   -> case mids of
                [mid] -> Right $ TaskLinkMem eid mid
                _     -> Right $ TaskLinkMemBatch eid mids
            ("task",     "unlink") -> case mids of
                [mid] -> Right $ TaskUnlinkMem eid mid
                _     -> Left "link_memory: task unlink only supports one memory_id at a time"
            ("category", "link")   -> case mids of
                [mid] -> Right $ CategoryLinkMem mid eid
                _     -> Right $ CategoryLinkMemBatch eid mids
            ("category", "unlink") -> case mids of
                [mid] -> Right $ CategoryUnlinkMem mid eid
                _     -> Left "link_memory: category unlink only supports one memory_id at a time"
            _ -> Left $ "link_memory: invalid entity_type/action: " <> T.unpack entityType <> "/" <> T.unpack action
    "list_entity_memories"     -> do
        entityType <- need "entity_type" :: Either String Text
        eid <- need "entity_id"
        case entityType of
            "project"  -> Right $ ProjectListMem eid
            "task"     -> Right $ TaskListMem eid
            "category" -> Right $ CategoryListMem eid
            _          -> Left $ "list_entity_memories: invalid entity_type: " <> T.unpack entityType
    "task_create"              -> TaskCreate <$> parse args
    "task_get"                 -> TaskGet <$> need "task_id"
    "task_overview"            -> TaskOverviewCall <$> need "task_id" <*> (maybe False id <$> opt "extra_context")
    "context_get"              -> ContextGetCall <$> need "task_id" <*> (maybe ContextMedium id <$> opt "detail_level")
    "task_list"                -> TaskList <$> parse args
    "task_update"              -> do
        mItems <- opt "items" :: Either String (Maybe [Value])
        case mItems of
            Just items -> TaskUpdateBatch <$> parseBatchUpdateItems' items
            Nothing    -> TaskUpdate <$> need "task_id" <*> parse args
    "task_dependency"          -> do
        action <- need "action" :: Either String Text
        case action of
            "add"    -> TaskDepAdd <$> need "task_id" <*> need "depends_on_id"
            "remove" -> TaskDepRemove <$> need "task_id" <*> need "depends_on_id"
            _        -> Left "task_dependency: action must be 'add' or 'remove'"
    "memory_set_tags"          -> do
        mItems <- opt "items" :: Either String (Maybe [Value])
        case mItems of
            Just items -> MemorySetTagsBatch <$> parseBatchSetTags' items
            Nothing    -> MemorySetTags <$> need "memory_id" <*> need "tags"
    "category"                 -> do
        action <- need "action" :: Either String Text
        case action of
            "create" -> CategoryCreate <$> parse args
            "get"    -> CategoryGet <$> need "category_id"
            "list"   -> CategoryList <$> opt "workspace_id" <*> opt "limit" <*> opt "offset"
            "update" -> CategoryUpdate <$> need "category_id" <*> parse args
            _        -> Left $ "category: invalid action: " <> T.unpack action
    "workspace_list"           -> WorkspaceList <$> opt "limit" <*> opt "offset"
    "workspace_get"            -> WorkspaceGet <$> need "workspace_id"
    "workspace_visualization"  -> WorkspaceVisualizationCall <$> need "workspace_id" <*> parse args <*> (maybe WorkspaceVisualizationSvg id <$> opt "format")
    "workspace_update"         -> WsUpdate <$> need "workspace_id" <*> parse args
    "workspace_register"       -> WorkspaceReg <$> parse args
    "cleanup_run"              -> CleanupRun <$> need "workspace_id"
    "memory_list"              -> MemoryList <$> parse args <*> (maybe False id <$> opt "detail")
    "saved_view"               -> do
        action <- need "action" :: Either String Text
        case action of
            "create"  -> SavedViewCreate <$> parse args
            "get"     -> SavedViewGet <$> need "view_id"
            "list"    -> SavedViewList <$> need "workspace_id" <*> opt "limit" <*> opt "offset"
            "update"  -> SavedViewUpdate <$> need "view_id" <*> parse args
            "execute" -> SavedViewExecute <$> need "view_id" <*> opt "limit" <*> opt "offset" <*> opt "detail"
            _         -> Left $ "saved_view: invalid action: " <> T.unpack action
    "project_overview"          -> ProjectOverviewCall <$> need "project_id"
    -- Workflow composite tools
    "task_start"                -> TaskStartCall <$> need "task_id" <*> (maybe ContextMedium id <$> opt "detail_level")
    "task_finish"               -> TaskFinishCall <$> need "task_id" <*> need "status" <*> opt "notes" <*> opt "tags"
    "project_spec"              -> do
        wsId <- need "workspace_id"
        pName <- need "name"
        pDesc <- opt "description"
        pPri  <- opt "priority"
        tasks <- parseSpecTasks args
        Right $ ProjectSpecCall wsId pName pDesc pPri tasks
    "project_archive"           -> ProjectArchiveCall <$> need "project_id" <*> opt "summary"
    "search"                    -> UnifiedSearch <$> parse args
    _                           -> Left $ "Unknown tool: " <> T.unpack name
  where
    parse :: FromJSON a => Value -> Either String a
    parse = parseEither parseJSON

    need :: FromJSON a => Key -> Either String a
    need k = parseEither (withObject "args" (.: k)) args

    opt :: FromJSON a => Key -> Either String (Maybe a)
    opt k = parseEither (withObject "args" (.:? k)) args

-- | Parse the batch set-tags items from a pre-extracted [Value].
parseBatchSetTags' :: [Value] -> Either String [(UUID, [Text])]
parseBatchSetTags' = mapM (parseEither parseItem)
  where
    parseItem = withObject "BatchSetTagsItem" $ \o -> do
      mid  <- o .: "memory_id"
      tags <- o .: "tags"
      pure (mid, tags)

-- | Parse batch update items from a pre-extracted [Value].
parseBatchUpdateItems' :: FromJSON a => [Value] -> Either String [(UUID, a)]
parseBatchUpdateItems' = mapM (parseEither parseItem)
  where
    parseItem = withObject "BatchUpdateItem" $ \o -> do
      uid <- o .: "id"
      upd <- parseJSON (Object o)
      pure (uid, upd)

-- | Parse the tasks array from project_spec arguments.
parseSpecTasks :: Value -> Either String [SpecTask]
parseSpecTasks = parseEither $ withObject "args" $ \o -> do
  items <- o .: "tasks" :: Parser [Value]
  mapM parseTask items
  where
    parseTask = withObject "SpecTask" $ \o -> do
      title <- o .: "title"
      desc  <- o .:? "description"
      pri   <- o .:? "priority"
      pure SpecTask { stTitle = title, stDescription = desc, stPriority = pri }

------------------------------------------------------------------------
-- Tool call dispatch
------------------------------------------------------------------------

handleToolCall :: Manager -> String -> Maybe Text -> Value -> IO Value
handleToolCall mgr serverUrl mApiKey params =
    case parseParams params of
        Left err -> pure $ mcpError (T.pack err)
        Right (name, args) ->
            case parseToolCall name args of
                Left err -> pure $ mcpError (T.pack err)
                Right call ->
                    either
                        (pure . mcpError . T.pack)
                        (executeToolCall mgr serverUrl mApiKey)
                        (validateToolCall call)

parseParams :: Value -> Either String (Text, Value)
parseParams = parseEither $ withObject "params" $ \o -> do
  name <- o .: "name"
  args <- o .:? "arguments" .!= object []
  pure (name, args)

------------------------------------------------------------------------
-- Input validation / clamping
------------------------------------------------------------------------

-- | Validate and clamp numeric fields on a parsed ToolCall so that
-- out-of-range values from the LLM are caught before hitting the
-- server.  Returns Left with a human-readable error on hard failures,
-- or Right with clamped values on soft corrections.
validateToolCall :: ToolCall -> Either String ToolCall
validateToolCall = \case
    MemoryCreate cm
        | not (validFtsLanguage cm.ftsLanguage) -> Left $ "Invalid fts_language: " <> show cm.ftsLanguage
        | otherwise ->
                let cm' = clampCreateMemory cm
                in MemoryCreate cm' <$ firstValidationError (validateCreateMemoryInput cm')
    MemoryCreateBatch cms
        | any (\cm -> not (validFtsLanguage cm.ftsLanguage)) cms ->
                Left "memory_create_batch: one or more items have an invalid fts_language"
        | otherwise ->
                let cms' = [clampCreateMemory cm | cm <- cms]
                in MemoryCreateBatch cms' <$ firstValidationError (validateCreateMemoryBatchInput cms')
    MemorySearch sq detail
        | not (validFtsLanguage sq.searchLanguage) -> Left $ "Invalid search_language: " <> show sq.searchLanguage
        | otherwise -> Right $ MemorySearch sq
                { limit = clampMaybe 1 200 <$> sq.limit
                , offset = clampMaybe 0 10000 <$> sq.offset
                , minImportance = clampMaybe 1 10 <$> sq.minImportance
                } detail
    MemoryUpdate mid um ->
        let um' = clampUpdateMemory um
        in MemoryUpdate mid um' <$ firstValidationError (validateUpdateMemoryInput um')
    MemoryGraphCall mid md -> Right $ MemoryGraphCall mid (clampMaybe 1 5 <$> md)
    MemoryList mq detail ->
        let mq' = clampMemoryListQuery mq
        in MemoryList mq' detail <$ firstValidationError (validateMemoryListQuery mq')
    ProjectCreate cp -> ProjectCreate cp <$ firstValidationError (validateCreateProjectInput cp)
    ProjectList pq ->
        let pq' = clampProjectListQuery pq
        in ProjectList pq' <$ firstValidationError (validateProjectListQuery pq')
    ProjectUpdate pid up -> ProjectUpdate pid up <$ firstValidationError (validateUpdateProjectInput up)
    TaskCreate ct -> TaskCreate ct <$ firstValidationError (validateCreateTaskInput ct)
    TaskList tq ->
        let tq' = clampTaskListQuery tq
        in TaskList tq' <$ firstValidationError (validateTaskListQuery tq')
    TaskOverviewCall tid extraContext -> Right $ TaskOverviewCall tid extraContext
    ContextGetCall tid level -> Right $ ContextGetCall tid level
    TaskUpdate tid ut -> TaskUpdate tid ut <$ firstValidationError (validateUpdateTaskInput ut)
    CategoryCreate cc -> CategoryCreate cc <$ firstValidationError (validateCreateMemoryCategoryInput cc)
    CategoryList mwid ml mo -> Right $ CategoryList mwid (clampMaybe 1 200 <$> ml) (clampMaybe 0 10000 <$> mo)
    CategoryUpdate cid uc -> CategoryUpdate cid uc <$ firstValidationError (validateUpdateMemoryCategoryInput uc)
    WorkspaceReg cw -> WorkspaceReg cw <$ firstValidationError (validateCreateWorkspaceInput cw)
    WorkspaceList ml mo -> Right $ WorkspaceList (clampMaybe 1 200 <$> ml) (clampMaybe 0 10000 <$> mo)
    WorkspaceVisualizationCall wid query format ->
        let query' = clampWorkspaceVisualizationQuery query
        in WorkspaceVisualizationCall wid query' format <$ firstValidationError (validateWorkspaceVisualizationQuery query')
    WsUpdate wid uw -> WsUpdate wid uw <$ firstValidationError (validateUpdateWorkspaceInput uw)
    MemoryDeleteBatch ids -> validateBatchIds "entity_lifecycle/batch_delete" ids (MemoryDeleteBatch ids)
    TaskDeleteBatch ids -> validateBatchIds "entity_lifecycle/batch_delete" ids (TaskDeleteBatch ids)
    ProjectDeleteBatch ids -> validateBatchIds "entity_lifecycle/batch_delete" ids (ProjectDeleteBatch ids)
    CategoryDeleteBatch ids -> validateBatchIds "entity_lifecycle/batch_delete" ids (CategoryDeleteBatch ids)
    TaskMoveBatch ids pid -> validateBatchIds "task_update/batch_move" ids (TaskMoveBatch ids pid)
    ProjectLinkMemBatch pid mids -> validateBatchIds "project_link_memories_batch" mids (ProjectLinkMemBatch pid mids)
    CategoryLinkMemBatch cid mids -> validateBatchIds "category_link_memories_batch" mids (CategoryLinkMemBatch cid mids)
    TaskLinkMemBatch tid mids -> validateBatchIds "task_link_memories_batch" mids (TaskLinkMemBatch tid mids)
    MemorySetTagsBatch items
        | null items -> Left "memory_set_tags: items must not be empty"
        | length items > 100 -> Left "memory_set_tags: items must contain at most 100 items"
        | otherwise -> Right $ MemorySetTagsBatch items
    MemoryUpdateBatch items
        | null items -> Left "memory_update: items must not be empty"
        | length items > 100 -> Left "memory_update: items must contain at most 100 items"
        | otherwise ->
            let items' = [(uid, clampUpdateMemory um) | (uid, um) <- items]
                errs = concat [validateUpdateMemoryInput um | (_, um) <- items']
            in MemoryUpdateBatch items' <$ firstValidationError errs
    ProjectUpdateBatch items
        | null items -> Left "project_update: items must not be empty"
        | length items > 100 -> Left "project_update: items must contain at most 100 items"
        | otherwise ->
            let errs = concat [validateUpdateProjectInput up | (_, up) <- items]
            in ProjectUpdateBatch items <$ firstValidationError errs
    TaskUpdateBatch items
        | null items -> Left "task_update: items must not be empty"
        | length items > 100 -> Left "task_update: items must contain at most 100 items"
        | otherwise ->
            let errs = concat [validateUpdateTaskInput ut | (_, ut) <- items]
            in TaskUpdateBatch items <$ firstValidationError errs
    SavedViewCreate csv -> SavedViewCreate csv <$ firstValidationError (validateCreateSavedViewInput csv)
    SavedViewList wid ml mo -> Right $ SavedViewList wid (clampMaybe 1 200 <$> ml) (clampMaybe 0 10000 <$> mo)
    SavedViewUpdate vid usv -> SavedViewUpdate vid usv <$ firstValidationError (validateUpdateSavedViewInput usv)
    SavedViewExecute vid ml mo md -> Right $ SavedViewExecute vid (clampMaybe 1 200 <$> ml) (clampMaybe 0 10000 <$> mo) md
    -- Workflow composite tools — lightweight validation
    TaskStartCall tid level -> Right $ TaskStartCall tid level
    TaskFinishCall tid status mNotes mTags -> Right $ TaskFinishCall tid status mNotes mTags
    ProjectSpecCall wsId pName pDesc pPri tasks
        | null tasks -> Left "project_spec: tasks must not be empty"
        | length tasks > 50 -> Left "project_spec: tasks must contain at most 50 items"
        | T.null (T.strip pName) -> Left "project_spec: name must not be blank"
        | any (T.null . T.strip . (.stTitle)) tasks -> Left "project_spec: all tasks must have non-blank titles"
        | otherwise -> Right $ ProjectSpecCall wsId pName pDesc (clampMaybe 1 10 <$> pPri)
            [st { stPriority = clampMaybe 1 10 <$> st.stPriority } | st <- tasks]
    ProjectArchiveCall pid mSummary -> Right $ ProjectArchiveCall pid mSummary
    UnifiedSearch usq
        | not (validFtsLanguage usq.searchLanguage) -> Left $ "Invalid search_language: " <> show usq.searchLanguage
        | otherwise ->
            let usq' = usq
                  { limit = clampMaybe 1 200 <$> usq.limit
                  , offset = clampMaybe 0 10000 <$> usq.offset
                  , minImportance = clampMaybe 1 10 <$> usq.minImportance
                  , taskPriority = clampMaybe 1 10 <$> usq.taskPriority
                  }
            in UnifiedSearch usq' <$ firstValidationError (validateUnifiedSearchQuery usq')
    other -> Right other

-- | Whitelist of valid PostgreSQL full-text search configurations.
validFtsLanguages :: Set.Set Text
validFtsLanguages = Set.fromList
  [ "simple", "arabic", "armenian", "basque", "catalan", "danish"
  , "dutch", "english", "finnish", "french", "german", "greek"
  , "hindi", "hungarian", "indonesian", "irish", "italian"
  , "lithuanian", "nepali", "norwegian", "portuguese", "romanian"
  , "russian", "serbian", "spanish", "swedish", "tamil", "turkish"
  , "yiddish"
  ]

-- | Check that an optional fts_language value is valid (Nothing = use default = ok).
validFtsLanguage :: Maybe Text -> Bool
validFtsLanguage Nothing  = True
validFtsLanguage (Just l) = Set.member l validFtsLanguages

-- | Validate a batch ID list (1-100 items).
validateBatchIds :: String -> [a] -> ToolCall -> Either String ToolCall
validateBatchIds toolName ids result
  | null ids  = Left $ toolName <> ": ids must not be empty"
  | length ids > 100 = Left $ toolName <> ": ids must contain at most 100 items"
  | otherwise = Right result

clamp :: Ord a => a -> a -> a -> a
clamp lo hi = Prelude.max lo . Prelude.min hi

clampMaybe :: Ord a => a -> a -> a -> a
clampMaybe lo hi = clamp lo hi

firstValidationError :: [Text] -> Either String ()
firstValidationError [] = Right ()
firstValidationError errs = Left (T.unpack (T.intercalate "; " errs))

-- | Clamp numeric fields on a CreateMemory without record update syntax
-- (avoids -Wambiguous-fields with DuplicateRecordFields).
clampCreateMemory :: CreateMemory -> CreateMemory
clampCreateMemory cm = CreateMemory
  { workspaceId = cm.workspaceId
  , content     = cm.content
  , summary     = cm.summary
  , memoryType  = cm.memoryType
  , importance  = clampMaybe 1 10 <$> cm.importance
  , metadata    = cm.metadata
  , expiresAt   = cm.expiresAt
  , source      = cm.source
  , confidence  = clampMaybe 0.0 1.0 <$> cm.confidence
  , pinned      = cm.pinned
  , tags        = cm.tags
  , ftsLanguage = cm.ftsLanguage
  }

-- | Clamp numeric fields on an UpdateMemory without record update syntax.
clampUpdateMemory :: UpdateMemory -> UpdateMemory
clampUpdateMemory um = UpdateMemory
  { content    = um.content
  , summary    = um.summary
  , memoryType = um.memoryType
  , importance = clampMaybe 1 10 <$> um.importance
  , metadata   = um.metadata
  , expiresAt  = um.expiresAt
  , source     = um.source
  , confidence = clampMaybe 0.0 1.0 <$> um.confidence
  , pinned     = um.pinned
  }

clampMemoryListQuery :: MemoryListQuery -> MemoryListQuery
clampMemoryListQuery mq = mq
    { limit = clampMaybe 1 200 <$> mq.limit
    , offset = clampMaybe 0 10000 <$> mq.offset
    }

clampProjectListQuery :: ProjectListQuery -> ProjectListQuery
clampProjectListQuery pq = pq
    { limit = clampMaybe 1 200 <$> pq.limit
    , offset = clampMaybe 0 10000 <$> pq.offset
    }

clampTaskListQuery :: TaskListQuery -> TaskListQuery
clampTaskListQuery tq = tq
    { limit = clampMaybe 1 200 <$> tq.limit
    , offset = clampMaybe 0 10000 <$> tq.offset
    }

clampWorkspaceVisualizationQuery :: WorkspaceVisualizationQuery -> WorkspaceVisualizationQuery
clampWorkspaceVisualizationQuery WorkspaceVisualizationQuery
    { includeProjectIds = includeIds
    , excludeProjectIds = excludeIds
    , taskStatuses = statuses
    , memoryFilter = memoryFilterValue
    , showTasks = showTasksValue
    , showTaskStatusSummary = showTaskStatusSummaryValue
    , showDescriptions = showDescriptionsValue
    } = WorkspaceVisualizationQuery
        { includeProjectIds = includeIds
        , excludeProjectIds = excludeIds
        , taskStatuses = statuses
        , memoryFilter = fmap clampWorkspaceVisualizationMemoryFilter memoryFilterValue
        , showTasks = showTasksValue
        , showTaskStatusSummary = showTaskStatusSummaryValue
        , showDescriptions = showDescriptionsValue
        }

clampWorkspaceVisualizationMemoryFilter :: WorkspaceVisualizationMemoryFilter -> WorkspaceVisualizationMemoryFilter
clampWorkspaceVisualizationMemoryFilter WorkspaceVisualizationMemoryFilter
    { memoryType = memoryTypeValue
    , tags = tagFilter
    , minImportance = minImportanceValue
    , pinnedOnly = pinnedOnlyValue
    } = WorkspaceVisualizationMemoryFilter
        { memoryType = memoryTypeValue
        , tags = tagFilter
        , minImportance = clampMaybe 1 10 <$> minImportanceValue
        , pinnedOnly = pinnedOnlyValue
        }

-- | Execute a typed tool call against the hmem-server HTTP API.
executeToolCall :: Manager -> String -> Maybe Text -> ToolCall -> IO Value
executeToolCall mgr base mApiKey = \case
    MemoryCreate cm     -> postJSON mgr base mApiKey "/api/v1/memories" cm
    MemoryCreateBatch cms -> postJSON mgr base mApiKey "/api/v1/memories/batch" cms
    MemorySearch sq detail -> postJSON mgr base mApiKey ("/api/v1/memories/search?compact=" <> if detail then "false" else "true") sq
    MemoryGet mid       -> getJSON  mgr base mApiKey ("/api/v1/memories/" <> uuidPath mid)
    MemoryUpdate mid um -> putJSON  mgr base mApiKey ("/api/v1/memories/" <> uuidPath mid) um
    MemoryDelete mid    -> delJSON  mgr base mApiKey ("/api/v1/memories/" <> uuidPath mid)
    MemoryRestore mid   -> postJSON mgr base mApiKey ("/api/v1/memories/" <> uuidPath mid <> "/restore") (object [])
    MemoryPurge mid     -> delJSON  mgr base mApiKey ("/api/v1/memories/" <> uuidPath mid <> "/purge")
    LinkMemories sid cl -> postJSON mgr base mApiKey ("/api/v1/memories/" <> uuidPath sid <> "/links") cl
    MemoryLinksList mid -> getJSON  mgr base mApiKey ("/api/v1/memories/" <> uuidPath mid <> "/links")
    ProjectCreate cp    -> postJSON mgr base mApiKey "/api/v1/projects" cp
    ProjectGet pid      -> getJSON  mgr base mApiKey ("/api/v1/projects/" <> uuidPath pid)
    ProjectUpdate pid up -> putJSON mgr base mApiKey ("/api/v1/projects/" <> uuidPath pid) up
    ProjectDelete pid   -> delJSON  mgr base mApiKey ("/api/v1/projects/" <> uuidPath pid)
    ProjectRestore pid  -> postJSON mgr base mApiKey ("/api/v1/projects/" <> uuidPath pid <> "/restore") (object [])
    ProjectPurge pid    -> delJSON  mgr base mApiKey ("/api/v1/projects/" <> uuidPath pid <> "/purge")
    ProjectList pq -> getJSON mgr base mApiKey ("/api/v1/projects" <> buildQuery
                            [ ("workspace_id", uuidPath <$> pq.workspaceId)
                            , ("status", encodeParam <$> pq.status)
                            , ("created_after", encodeParam <$> pq.createdAfter)
                            , ("created_before", encodeParam <$> pq.createdBefore)
                            , ("updated_after", encodeParam <$> pq.updatedAfter)
                            , ("updated_before", encodeParam <$> pq.updatedBefore)
                            , ("limit", show <$> pq.limit)
                            , ("offset", show <$> pq.offset)
                            ])
    ProjectLinkMem pid mid -> postJSON mgr base mApiKey ("/api/v1/projects/" <> uuidPath pid <> "/memories")
                              (object ["memory_id" .= mid])
    ProjectUnlinkMem pid mid -> delJSON mgr base mApiKey ("/api/v1/projects/" <> uuidPath pid <> "/memories/"
                                <> uuidPath mid)
    TaskCreate ct       -> postJSON mgr base mApiKey "/api/v1/tasks" ct
    TaskGet tid         -> getJSON  mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid)
    TaskOverviewCall tid extraContext ->
        getJSON mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid <> "/overview" <>
          buildQuery [("extra_context", Just $ if extraContext then "true" else "false")])
    ContextGetCall tid level ->
        let levelStr = case level of
              ContextLight  -> "light"
              ContextMedium -> "medium"
              ContextHeavy  -> "heavy"
        in getJSON mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid <> "/context" <>
             buildQuery [("detail_level", Just levelStr)])
    TaskDelete tid      -> delJSON  mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid)
    TaskRestore tid     -> postJSON mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid <> "/restore") (object [])
    TaskPurge tid       -> delJSON  mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid <> "/purge")
    TaskList tq -> getJSON mgr base mApiKey ("/api/v1/tasks" <> buildQuery
                            [ ("workspace_id", uuidPath <$> tq.workspaceId)
                            , ("project_id", uuidPath <$> tq.projectId)
                            , ("status", encodeParam <$> tq.status)
                            , ("priority", show <$> tq.priority)
                            , ("created_after", encodeParam <$> tq.createdAfter)
                            , ("created_before", encodeParam <$> tq.createdBefore)
                            , ("updated_after", encodeParam <$> tq.updatedAfter)
                            , ("updated_before", encodeParam <$> tq.updatedBefore)
                            , ("limit", show <$> tq.limit)
                            , ("offset", show <$> tq.offset)
                            ])
    TaskUpdate tid ut   -> putJSON  mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid) ut
    TaskLinkMem tid mid -> postJSON mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid <> "/memories")
                            (object ["memory_id" .= mid])
    TaskUnlinkMem tid mid -> delJSON mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid <> "/memories/"
                              <> uuidPath mid)
    TaskDepAdd tid did  -> postJSON mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid <> "/dependencies")
                            (object ["depends_on_id" .= did])
    TaskDepRemove tid did -> delJSON mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid <> "/dependencies/"
                              <> uuidPath did)
    CategoryCreate cc   -> postJSON mgr base mApiKey "/api/v1/categories" cc
    CategoryGet cid     -> getJSON  mgr base mApiKey ("/api/v1/categories/" <> uuidPath cid)
    CategoryList mwid ml mo -> getJSON mgr base mApiKey ("/api/v1/categories" <> buildQuery
                            [ ("workspace_id", uuidPath <$> mwid)
                            , ("limit", show <$> ml)
                            , ("offset", show <$> mo)
                            ])
    CategoryUpdate cid uc -> putJSON mgr base mApiKey ("/api/v1/categories/" <> uuidPath cid) uc
    CategoryDelete cid  -> delJSON  mgr base mApiKey ("/api/v1/categories/" <> uuidPath cid)
    CategoryRestore cid -> postJSON mgr base mApiKey ("/api/v1/categories/" <> uuidPath cid <> "/restore") (object [])
    CategoryPurge cid   -> delJSON  mgr base mApiKey ("/api/v1/categories/" <> uuidPath cid <> "/purge")
    CategoryLinkMem mid cid -> postJSON mgr base mApiKey "/api/v1/categories/link"
                               (object ["memory_id" .= mid, "category_id" .= cid])
    CategoryUnlinkMem mid cid -> postJSON mgr base mApiKey "/api/v1/categories/unlink"
                                  (object ["memory_id" .= mid, "category_id" .= cid])
    MemorySetTags mid tags -> putJSON mgr base mApiKey ("/api/v1/memories/" <> uuidPath mid <> "/tags")
                              (toJSON tags)
    MemoryUnlink sid tid rt -> delJSON mgr base mApiKey ("/api/v1/memories/" <> uuidPath sid <> "/links/"
                               <> uuidPath tid <> "/" <> T.unpack (relationTypeToText rt))
    WorkspaceList ml mo      -> getJSON  mgr base mApiKey ("/api/v1/workspaces" <> buildQuery
                            [ ("limit", show <$> ml)
                            , ("offset", show <$> mo)
                            ])
    WorkspaceGet wid        -> getJSON  mgr base mApiKey ("/api/v1/workspaces/" <> uuidPath wid)
    WorkspaceVisualizationCall wid query format ->
        postAcceptedText (workspaceVisualizationAccept format) mgr base mApiKey ("/api/v1/workspaces/" <> uuidPath wid <> "/visualization") query
    WsUpdate wid uw         -> putJSON  mgr base mApiKey ("/api/v1/workspaces/" <> uuidPath wid) uw
    WsDelete wid            -> delJSON  mgr base mApiKey ("/api/v1/workspaces/" <> uuidPath wid)
    WsRestore wid           -> postJSON mgr base mApiKey ("/api/v1/workspaces/" <> uuidPath wid <> "/restore") (object [])
    WsPurge wid             -> delJSON  mgr base mApiKey ("/api/v1/workspaces/" <> uuidPath wid <> "/purge")
    WorkspaceReg cw     -> postJSON mgr base mApiKey "/api/v1/workspaces" cw
    CleanupRun wid      -> postJSON mgr base mApiKey "/api/v1/cleanup/run"
                            (object ["workspace_id" .= wid])
    MemoryList mq detail -> getJSON mgr base mApiKey ("/api/v1/memories" <> buildQuery
                            [ ("workspace_id", uuidPath <$> mq.workspaceId)
                            , ("type", encodeParam <$> mq.memoryType)
                            , ("created_after", encodeParam <$> mq.createdAfter)
                            , ("created_before", encodeParam <$> mq.createdBefore)
                            , ("updated_after", encodeParam <$> mq.updatedAfter)
                            , ("updated_before", encodeParam <$> mq.updatedBefore)
                            , ("limit", show <$> mq.limit)
                            , ("offset", show <$> mq.offset)
                            , ("compact", Just $ if detail then "false" else "true")
                            ])
    MemoryGraphCall mid md -> getJSON mgr base mApiKey ("/api/v1/memories/" <> uuidPath mid <> "/graph"
                            <> buildQuery [("depth", show <$> md)])
    MemoryFindByRelation wid rt -> getJSON mgr base mApiKey ("/api/v1/memories/by-relation" <> buildQuery
                            [ ("workspace_id", Just $ uuidPath wid)
                            , ("relation_type", Just $ encodeParam rt)
                            ])
    ProjectListMem pid -> getJSON mgr base mApiKey ("/api/v1/projects/" <> uuidPath pid <> "/memories")
    CategoryListMem cid -> getJSON mgr base mApiKey ("/api/v1/categories/" <> uuidPath cid <> "/memories")
    TaskListMem tid -> getJSON mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid <> "/memories")
    MemoryDeleteBatch ids -> postJSON mgr base mApiKey "/api/v1/memories/batch-delete"
                              (object ["ids" .= ids])
    TaskDeleteBatch ids -> postJSON mgr base mApiKey "/api/v1/tasks/batch-delete"
                            (object ["ids" .= ids])
    ProjectDeleteBatch ids -> postJSON mgr base mApiKey "/api/v1/projects/batch-delete"
                               (object ["ids" .= ids])
    CategoryDeleteBatch ids -> postJSON mgr base mApiKey "/api/v1/categories/batch-delete"
                                (object ["ids" .= ids])
    TaskMoveBatch ids pid -> postJSON mgr base mApiKey "/api/v1/tasks/batch-move"
                              (object ["task_ids" .= ids, "project_id" .= pid])
    ProjectLinkMemBatch pid mids -> postJSON mgr base mApiKey ("/api/v1/projects/" <> uuidPath pid <> "/memories/batch")
                                    (object ["memory_ids" .= mids])
    CategoryLinkMemBatch cid mids -> postJSON mgr base mApiKey ("/api/v1/categories/" <> uuidPath cid <> "/memories/batch")
                                      (object ["memory_ids" .= mids])
    TaskLinkMemBatch tid mids -> postJSON mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid <> "/memories/batch")
                                  (object ["memory_ids" .= mids])
    MemorySetTagsBatch items -> postJSON mgr base mApiKey "/api/v1/memories/batch-set-tags"
                                (object ["items" .= [object ["memory_id" .= mid, "tags" .= tags] | (mid, tags) <- items]])
    MemoryUpdateBatch items -> postJSON mgr base mApiKey "/api/v1/memories/batch-update"
                                (object ["items" .= [case toJSON um of
                                    Object o -> Object (KM.insert "id" (toJSON uid) o)
                                    v        -> v
                                  | (uid, um) <- items]])
    ProjectUpdateBatch items ->
        let payload = object
              [ "items" .=
                  [ case toJSON up of
                      Object o -> Object (KM.insert "id" (toJSON uid) o)
                      v        -> v
                  | (uid, up) <- items
                  ]
              ]
        in postJSON mgr base mApiKey "/api/v1/projects/batch-update" payload
    TaskUpdateBatch items -> postJSON mgr base mApiKey "/api/v1/tasks/batch-update"
                              (object ["items" .= [case toJSON ut of
                                    Object o -> Object (KM.insert "id" (toJSON uid) o)
                                    v        -> v
                                  | (uid, ut) <- items]])
    SavedViewCreate csv -> postJSON mgr base mApiKey "/api/v1/saved-views" csv
    SavedViewList wid ml mo -> getJSON mgr base mApiKey ("/api/v1/saved-views" <> buildQuery
                            [ ("workspace_id", Just $ uuidPath wid)
                            , ("limit", show <$> ml)
                            , ("offset", show <$> mo)
                            ])
    SavedViewGet vid -> getJSON mgr base mApiKey ("/api/v1/saved-views/" <> uuidPath vid)
    SavedViewUpdate vid usv -> putJSON mgr base mApiKey ("/api/v1/saved-views/" <> uuidPath vid) usv
    SavedViewDelete vid -> delJSON mgr base mApiKey ("/api/v1/saved-views/" <> uuidPath vid)
    SavedViewRestore vid -> postJSON mgr base mApiKey ("/api/v1/saved-views/" <> uuidPath vid <> "/restore") (object [])
    SavedViewPurge vid -> delJSON mgr base mApiKey ("/api/v1/saved-views/" <> uuidPath vid <> "/purge")
    SavedViewExecute vid ml mo md -> postJSON mgr base mApiKey ("/api/v1/saved-views/" <> uuidPath vid <> "/execute"
                            <> buildQuery
                            [ ("limit", show <$> ml)
                            , ("offset", show <$> mo)
                            , ("detail", (\b -> if b then "true" else "false") <$> md)
                            ]) (object [])
    ProjectOverviewCall pid -> getJSON mgr base mApiKey ("/api/v1/projects/" <> uuidPath pid <> "/overview")
    UnifiedSearch usq -> postJSON mgr base mApiKey "/api/v1/search" usq

    -- ================================================================
    -- WORKFLOW COMPOSITE TOOLS
    -- These chain multiple HTTP calls to implement common task/project
    -- workflows in a single MCP tool invocation.
    -- ================================================================

    TaskStartCall tid level -> do
      -- 1. Update task status to in_progress (best-effort; may already be in_progress)
      _ <- rawPutJSON mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid)
             (object ["status" .= ("in_progress" :: Text)])
      -- 2. Load context for the task
      let levelStr = case level of
            ContextLight  -> "light"
            ContextMedium -> "medium"
            ContextHeavy  -> "heavy"
      getJSON mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid <> "/context" <>
        buildQuery [("detail_level", Just levelStr)])

    TaskFinishCall tid status mNotes mTags -> do
      -- 1. If notes provided, create a linked memory
      case mNotes of
        Just notes | not (T.null (T.strip notes)) -> do
          -- First get the task to find workspace_id
          taskResult <- rawGetJSON mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid)
          case taskResult of
            Right taskVal -> do
              let mWsId = case taskVal of
                    Object o -> case KM.lookup "workspace_id" o of
                      Just (String ws) -> Just ws
                      _                -> Nothing
                    _ -> Nothing
              case mWsId of
                Just wsId -> do
                  -- Create the notes memory
                  let memBody = object
                        [ "workspace_id" .= wsId
                        , "content"      .= notes
                        , "memory_type"  .= ("long_term" :: Text)
                        , "importance"   .= (6 :: Int)
                        , "source"       .= ("inferred" :: Text)
                        , "tags"         .= maybe ["task-notes" :: Text] id mTags
                        ]
                  memResult <- rawPostJSON mgr base mApiKey "/api/v1/memories" memBody
                  -- Link memory to task
                  case memResult of
                    Right memVal -> do
                      let mMemId = case memVal of
                            Object o -> case KM.lookup "id" o of
                              Just (String mid) -> Just mid
                              _                 -> Nothing
                            _ -> Nothing
                      case mMemId of
                        Just memId ->
                          void $ rawPostJSON mgr base mApiKey
                            ("/api/v1/tasks/" <> uuidPath tid <> "/memories")
                            (object ["memory_id" .= memId])
                        Nothing -> pure ()
                    Left _ -> pure ()  -- memory creation failed; still update status
                Nothing -> pure ()  -- couldn't find workspace_id; still update status
            Left _ -> pure ()  -- task fetch failed; still update status
        _ -> pure ()
      -- 2. Update task status
      putJSON mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid)
        (object ["status" .= status])

    ProjectSpecCall wsId pName pDesc pPri tasks -> do
      -- 1. Create the project
      let projBody = object $ filter ((/= Null) . snd)
            [ "workspace_id" .= wsId
            , "name"         .= pName
            , "description"  .= pDesc
            , "priority"     .= pPri
            ]
      projResult <- rawPostJSON mgr base mApiKey "/api/v1/projects" projBody
      case projResult of
        Left err -> pure $ mcpErrorCode "PROJECT_CREATE_FAILED" err
        Right projVal -> do
          let mProjId = case projVal of
                Object o -> case KM.lookup "id" o of
                  Just (String pid) -> Just pid
                  _                 -> Nothing
                _ -> Nothing
          case mProjId of
            Nothing -> pure $ mcpErrorCode "PROJECT_CREATE_FAILED" "Could not extract project ID"
            Just projId -> do
              -- 2. Create each task under the project
              taskResults <- mapM (\st -> do
                let taskBody = object $ filter ((/= Null) . snd)
                      [ "workspace_id" .= wsId
                      , "project_id"   .= projId
                      , "title"        .= st.stTitle
                      , "description"  .= st.stDescription
                      , "priority"     .= st.stPriority
                      ]
                rawPostJSON mgr base mApiKey "/api/v1/tasks" taskBody
                ) tasks
              let createdTasks = [v | Right v <- taskResults]
                  failedCount  = length [() | Left _ <- taskResults]
                  result = object
                    [ "project" .= projVal
                    , "tasks"   .= createdTasks
                    , "tasks_failed" .= failedCount
                    ]
              pure $ mcpResult (encode result)

    ProjectArchiveCall pid mSummary -> do
      -- 1. Get project to find workspace_id
      projResult <- rawGetJSON mgr base mApiKey ("/api/v1/projects/" <> uuidPath pid)
      case projResult of
        Left err -> pure $ mcpErrorCode "PROJECT_NOT_FOUND" err
        Right projVal -> do
          -- 2. If summary provided, create a linked memory
          case mSummary of
            Just summary | not (T.null (T.strip summary)) -> do
              let mWsId = case projVal of
                    Object o -> case KM.lookup "workspace_id" o of
                      Just (String ws) -> Just ws
                      _                -> Nothing
                    _ -> Nothing
              case mWsId of
                Just wsId -> do
                  let memBody = object
                        [ "workspace_id" .= wsId
                        , "content"      .= summary
                        , "memory_type"  .= ("long_term" :: Text)
                        , "importance"   .= (7 :: Int)
                        , "source"       .= ("inferred" :: Text)
                        , "tags"         .= (["project-summary" :: Text])
                        ]
                  memResult <- rawPostJSON mgr base mApiKey "/api/v1/memories" memBody
                  case memResult of
                    Right memVal -> do
                      let mMemId = case memVal of
                            Object o -> case KM.lookup "id" o of
                              Just (String mid) -> Just mid
                              _                 -> Nothing
                            _ -> Nothing
                      case mMemId of
                        Just memId ->
                          void $ rawPostJSON mgr base mApiKey
                            ("/api/v1/projects/" <> uuidPath pid <> "/memories")
                            (object ["memory_id" .= memId])
                        Nothing -> pure ()
                    Left _ -> pure ()
                Nothing -> pure ()
            _ -> pure ()
          -- 3. Archive the project
          putJSON mgr base mApiKey ("/api/v1/projects/" <> uuidPath pid)
            (object ["status" .= ("archived" :: Text)])

------------------------------------------------------------------------
-- Typed HTTP helpers
------------------------------------------------------------------------

postJSON :: ToJSON a => Manager -> String -> Maybe Text -> String -> a -> IO Value
postJSON mgr base mApiKey path body = httpJSON mgr mApiKey "POST" (base <> path) (Just (encode body))

getJSON :: Manager -> String -> Maybe Text -> String -> IO Value
getJSON mgr base mApiKey path = httpJSON mgr mApiKey "GET" (base <> path) Nothing

putJSON :: ToJSON a => Manager -> String -> Maybe Text -> String -> a -> IO Value
putJSON mgr base mApiKey path body = httpJSON mgr mApiKey "PUT" (base <> path) (Just (encode body))

delJSON :: Manager -> String -> Maybe Text -> String -> IO Value
delJSON mgr base mApiKey path = httpJSON mgr mApiKey "DELETE" (base <> path) Nothing

postAcceptedText :: ToJSON a => BS8.ByteString -> Manager -> String -> Maybe Text -> String -> a -> IO Value
postAcceptedText acceptHeader mgr base mApiKey path body = httpText mgr mApiKey acceptHeader "POST" (base <> path) (Just (encode body))

httpJSON :: Manager -> Maybe Text -> String -> String -> Maybe BL.ByteString -> IO Value
httpJSON mgr mApiKey httpMethod url mbody = do
  result <- try $ do
    initReq <- parseRequest url
    let authHeaders = maybe [] (\key -> [("Authorization", "Bearer " <> TE.encodeUtf8 key)]) mApiKey
    let req = initReq
          { method         = fromString httpMethod
        , requestHeaders = [("Content-Type", "application/json")] <> authHeaders
          , requestBody    = maybe (RequestBodyBS mempty) RequestBodyLBS mbody
          }
    resp <- httpLbs req mgr
    let code = statusCode (responseStatus resp)
        body = responseBody resp
    if code >= 200 && code < 300
      then pure $ mcpResult body
      else pure $ mcpErrorCode
        ("HTTP_" <> T.pack (show code))
        (decodeUtf8 body)
  case result of
    Right v  -> pure v
    Left (e :: SomeException) -> pure $ mcpErrorCode "CONNECTION_ERROR" (T.pack $ show e)

httpText :: Manager -> Maybe Text -> BS8.ByteString -> String -> String -> Maybe BL.ByteString -> IO Value
httpText mgr mApiKey acceptHeader httpMethod url mbody = do
    result <- try $ do
        initReq <- parseRequest url
        let authHeaders = maybe [] (\key -> [("Authorization", "Bearer " <> TE.encodeUtf8 key)]) mApiKey
        let req = initReq
                    { method         = fromString httpMethod
                    , requestHeaders = [("Content-Type", "application/json"), (hAccept, acceptHeader)] <> authHeaders
                    , requestBody    = maybe (RequestBodyBS mempty) RequestBodyLBS mbody
                    }
        resp <- httpLbs req mgr
        let code = statusCode (responseStatus resp)
        let body = responseBody resp
        if code >= 200 && code < 300
            then pure $ mcpResult body
            else pure $ mcpErrorCode
                ("HTTP_" <> T.pack (show code))
                (decodeUtf8 body)
    case result of
        Right v  -> pure v
        Left (e :: SomeException) -> pure $ mcpErrorCode "CONNECTION_ERROR" (T.pack $ show e)

-- | Raw HTTP helpers for composite tools — return Either instead of MCP-wrapped values.
-- These allow workflow handlers to chain calls and build combined responses.
rawHttpJSON' :: Manager -> Maybe Text -> String -> String -> Maybe BL.ByteString -> IO (Either Text Value)
rawHttpJSON' mgr mApiKey httpMethod url mbody = do
  result <- try $ do
    initReq <- parseRequest url
    let authHeaders = maybe [] (\key -> [("Authorization", "Bearer " <> TE.encodeUtf8 key)]) mApiKey
    let req = initReq
          { method         = fromString httpMethod
          , requestHeaders = [("Content-Type", "application/json")] <> authHeaders
          , requestBody    = maybe (RequestBodyBS mempty) RequestBodyLBS mbody
          }
    resp <- httpLbs req mgr
    let code = statusCode (responseStatus resp)
        body = responseBody resp
    if code >= 200 && code < 300
      then case eitherDecode body of
        Right v  -> pure (Right v)
        Left _   -> pure (Right (String (decodeUtf8 body)))
      else pure (Left (decodeUtf8 body))
  case result of
    Right v  -> pure v
    Left (e :: SomeException) -> pure (Left (T.pack $ show e))

rawGetJSON :: Manager -> String -> Maybe Text -> String -> IO (Either Text Value)
rawGetJSON mgr base mApiKey path = rawHttpJSON' mgr mApiKey "GET" (base <> path) Nothing

rawPostJSON :: ToJSON a => Manager -> String -> Maybe Text -> String -> a -> IO (Either Text Value)
rawPostJSON mgr base mApiKey path body = rawHttpJSON' mgr mApiKey "POST" (base <> path) (Just (encode body))

rawPutJSON :: ToJSON a => Manager -> String -> Maybe Text -> String -> a -> IO (Either Text Value)
rawPutJSON mgr base mApiKey path body = rawHttpJSON' mgr mApiKey "PUT" (base <> path) (Just (encode body))

workspaceVisualizationAccept :: WorkspaceVisualizationFormat -> BS8.ByteString
workspaceVisualizationAccept format = case format of
    WorkspaceVisualizationSvg -> "image/svg+xml"
    WorkspaceVisualizationJson -> "application/json"

------------------------------------------------------------------------
-- MCP content helpers
------------------------------------------------------------------------

-- | Wrap an API response for MCP, stripping verbose fields to reduce
-- LLM context consumption.
mcpResult :: BL.ByteString -> Value
mcpResult body =
  let trimmed = case eitherDecode body of
        Right v  -> encode (trimForLLM v)
        Left _   -> body   -- not JSON; pass through as-is
  in object
       [ "content" .= [object ["type" .= ("text" :: Text), "text" .= decodeUtf8 trimmed]] ]

-- | Strip fields that are noisy for LLM context but not actionable.
-- Removes: workspace_id, created_at, updated_at, last_accessed_at,
-- access_count, fts_language, metadata (when empty {}), confidence
-- (when null), source (when null), expires_at (when null),
-- dependency_count, memory_link_count.
trimForLLM :: Value -> Value
trimForLLM (Object o) =
  let cleaned = KM.filterWithKey keepField o
      keepField k v = not $ k `elem` dropKeys || isNullDrop k v || isEmptyMetadata k v
      dropKeys =
        [ "workspace_id", "created_at", "updated_at", "last_accessed_at"
        , "access_count", "fts_language", "dependency_count", "memory_link_count"
        ]
      isNullDrop k v = v == Null && k `elem` ["confidence", "source", "expires_at", "completed_at", "due_at", "parent_id", "project_id", "description", "gh_owner", "gh_repo", "path"]
      isEmptyMetadata k v = k == "metadata" && v == Object mempty
  in Object (KM.map trimForLLM cleaned)
trimForLLM (Array arr) = Array (fmap trimForLLM arr)
trimForLLM v = v

mcpError :: Text -> Value
mcpError msg = object
  [ "isError" .= True
  , "content" .= [object ["type" .= ("text" :: Text), "text" .= msg]]
  ]

-- | Structured error with an error code for programmatic handling by LLMs.
mcpErrorCode :: Text -> Text -> Value
mcpErrorCode code msg = object
  [ "isError" .= True
  , "content" .= [object
      [ "type" .= ("text" :: Text)
      , "text" .= ("[" <> code <> "] " <> msg)
      ]]
  ]

------------------------------------------------------------------------
-- Utility
------------------------------------------------------------------------

t :: Text -> Text
t = Prelude.id

-- | Current tool API version. Bump this when tool schemas change
-- (new required fields, renamed tools, changed semantics).
-- Adding new optional fields or new tools does not require a bump.
toolApiVersion :: Text
toolApiVersion = "0.5.0"

mkTool :: Text -> Text -> Value -> Value
mkTool name desc inputSchema = object
  [ "name"        .= name
  , "description" .= desc
  , "inputSchema" .= inputSchema
  , "annotations" .= object
      [ "version" .= toolApiVersion ]
  ]

prop :: Text -> Text -> Value
prop ty desc = object ["type" .= ty, "description" .= desc]

propMaxLength :: Text -> Text -> Int -> Value
propMaxLength ty desc maxLen = object
    [ "type" .= ty
    , "description" .= desc
    , "maxLength" .= maxLen
    ]

propEnum :: Text -> Text -> [Text] -> Value
propEnum ty desc vals = object ["type" .= ty, "description" .= desc, "enum" .= vals]

uuidPath :: UUID -> String
uuidPath = UUID.toString

-- | Build a query string from optional key-value pairs.
-- Returns "" if all values are Nothing, otherwise "?k1=v1&k2=v2..."
-- Values are percent-encoded to prevent injection of special characters.
buildQuery :: [(String, Maybe String)] -> String
buildQuery params = case [(k, v) | (k, Just v) <- params] of
  [] -> ""
  ps -> "?" <> intercalate "&" [k <> "=" <> encodeQueryValue v | (k, v) <- ps]

encodeQueryValue :: String -> String
encodeQueryValue = BS8.unpack . urlEncode True . BS8.pack

encodeParam :: ToJSON a => a -> String
encodeParam x = case toJSON x of
  String s -> T.unpack s
  v        -> T.unpack (decodeUtf8 (encode v))

decodeUtf8 :: BL.ByteString -> Text
decodeUtf8 = TE.decodeUtf8 . BL.toStrict
