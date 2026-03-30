module HMem.MCP.Tools
  ( toolDefinitions
  , handleToolCall
  -- * Testing
  , parseToolCall
  , validateToolCall
  , ToolCall(..)
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.List (intercalate)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI (urlEncode)

import HMem.Types

------------------------------------------------------------------------
-- Tool definition schemas (sent to LLM via tools/list)
------------------------------------------------------------------------

toolDefinitions :: [Value]
toolDefinitions =
    [ mkTool "memory_create" "Create a new memory in a workspace. Use this for initial capture, then refine later with updates, tags, links, or categories." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace"
          , "content"      .= propMaxLength "string" "The memory content" maxMemoryContentBytes
          , "summary"      .= propMaxLength "string" "Optional short summary" maxMemorySummaryBytes
          , "memory_type"  .= propEnum "string" "short_term or long_term" ["short_term", "long_term"]
          , "importance"   .= prop "integer" "1-10, default 5"
          , "metadata"     .= prop "object" "Optional metadata JSON object for structured annotations"
          , "expires_at"   .= prop "string" "ISO 8601 expiration time"
          , "source"       .= prop "string" "Provenance: user_stated, inferred, tool_output, web_search"
          , "confidence"   .= prop "number" "Confidence level 0.0-1.0, default 1.0"
          , "pinned"       .= prop "boolean" "Pin this memory (default false)"
          , "tags"         .= object ["type" .= t "array", "items" .= object ["type" .= t "string"],
                                       "description" .= t "Tags for categorization"]
          , "fts_language" .= prop "string" "Full-text search language (default 'english'). Use a PostgreSQL regconfig name, e.g. 'spanish', 'german', 'simple'."
          ]
      , "required" .= [t "workspace_id", t "content", t "memory_type"]
      ]

    , mkTool "memory_create_batch" "Create multiple related memories in a single transaction. Prefer this when capturing several items from one source or conversation. Max 100 items per batch." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "memories" .= object
              [ "type" .= t "array"
              , "description" .= t "Array of memory objects to create (same schema as memory_create)"
              , "minItems" .= (1 :: Int)
              , "maxItems" .= (100 :: Int)
              , "items" .= object
                  [ "type" .= t "object"
                  , "properties" .= object
                      [ "workspace_id" .= prop "string" "UUID of the workspace"
                      , "content"      .= propMaxLength "string" "The memory content" maxMemoryContentBytes
                      , "summary"      .= propMaxLength "string" "Optional short summary" maxMemorySummaryBytes
                      , "memory_type"  .= propEnum "string" "short_term or long_term" ["short_term", "long_term"]
                      , "importance"   .= prop "integer" "1-10, default 5"
                      , "metadata"     .= prop "object" "Optional metadata JSON object"
                      , "expires_at"   .= prop "string" "ISO 8601 expiration time"
                      , "source"       .= prop "string" "Provenance"
                      , "confidence"   .= prop "number" "Confidence level 0.0-1.0"
                      , "tags"         .= object ["type" .= t "array", "items" .= object ["type" .= t "string"],
                                                   "description" .= t "Tags for categorization"]
                      , "fts_language" .= prop "string" "Full-text search language (default 'english'). A PostgreSQL regconfig name."
                      , "pinned"       .= prop "boolean" "Pin this memory (default false)"
                      ]
                  , "required" .= [t "workspace_id", t "content", t "memory_type"]
                  ]
              ]
          ]
      , "required" .= [t "memories"]
      ]

    , mkTool "memory_search" "Preferred discovery step before create, update, or linking work. Use 'query' for keyword search, or combine filters (workspace_id, memory_type, tags, min_importance, category_id, pinned_only) to narrow results. All parameters are optional; omit workspace_id for cross-workspace search. Returns compact results by default; set detail=true for full content and metadata." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id"   .= prop "string" "UUID of the workspace (omit for cross-workspace search)"
          , "query"          .= prop "string" "Full-text search query"
          , "memory_type"    .= propEnum "string" "Filter by type" ["short_term", "long_term"]
          , "tags"           .= object ["type" .= t "array", "items" .= object ["type" .= t "string"],
                                         "description" .= t "Filter by tags (any-match: returns memories with at least one of the given tags)"]
          , "min_importance" .= prop "integer" "Minimum importance (1-10)"
          , "category_id"    .= prop "string" "Filter by category UUID"
          , "pinned_only"    .= prop "boolean" "If true, only return pinned memories"
          , "search_language" .= prop "string" "Language for query stemming (default 'english'). Use a PostgreSQL regconfig name."
          , "detail"         .= prop "boolean" "If true, return full content and metadata instead of compact summaries (default false)"
          , "limit"          .= prop "integer" "Max results (default 50)"
          , "offset"         .= prop "integer" "Offset for pagination (default 0)"
          ]
      , "required" .= ([] :: [Text])
      ]

    , mkTool "memory_get" "Get a memory by ID with full detail: content, summary, metadata, tags, importance, timestamps, and source. Use memory_search or memory_list to discover IDs first." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "memory_id" .= prop "string" "UUID of the memory" ]
      , "required" .= [t "memory_id"]
      ]

    , mkTool "memory_update" "Enrich or correct an existing memory. Use null to clear nullable fields such as summary, expires_at, or source." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "memory_id"   .= prop "string" "UUID of the memory to update"
          , "content"     .= propMaxLength "string" "New content" maxMemoryContentBytes
          , "summary"     .= propMaxLength "string" "New summary (null to clear)" maxMemorySummaryBytes
          , "importance"  .= prop "integer" "New importance (1-10)"
          , "memory_type" .= propEnum "string" "New type" ["short_term", "long_term"]
          , "metadata"    .= prop "object" "New metadata JSON object"
          , "expires_at"  .= prop "string" "ISO 8601 expiration time (null to clear)"
          , "source"      .= prop "string" "Provenance (null to clear)"
          , "confidence"  .= prop "number" "Confidence level 0.0-1.0"
          , "pinned"      .= prop "boolean" "Pin or unpin this memory"
          ]
      , "required" .= [t "memory_id"]
      ]

    , mkTool "memory_delete" "Soft-delete a memory, hiding it from all list and search results. Recoverable until permanently removed with memory_purge." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "memory_id" .= prop "string" "UUID of the memory to delete" ]
      , "required" .= [t "memory_id"]
      ]

    , mkTool "memory_purge" "Permanently and irreversibly remove a soft-deleted memory. The memory must be soft-deleted first via memory_delete." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "memory_id" .= prop "string" "UUID of the soft-deleted memory to purge" ]
      , "required" .= [t "memory_id"]
      ]

    , mkTool "memory_link" "Create a typed link between two existing memories by ID. Use after discovery with memory_search or memory_get." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "source_id"     .= prop "string" "Source memory UUID"
          , "target_id"     .= prop "string" "Target memory UUID"
          , "relation_type" .= propEnum "string" "Relation type"
              ["related", "supersedes", "contradicts", "elaborates", "inspires", "depends_on", "derived_from", "alternative_to"]
          , "strength"      .= prop "number" "Link strength 0.0-1.0, default 1.0"
          ]
      , "required" .= [t "source_id", t "target_id", t "relation_type"]
      ]

    , mkTool "project_create" "Create a top-level or child project in a workspace. Start minimal, then refine status, priority, metadata, or hierarchy later with project_update." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace"
          , "name"         .= propMaxLength "string" "Project name" maxNameBytes
          , "description"  .= propMaxLength "string" "Project description" maxDescriptionBytes
          , "parent_id"    .= prop "string" "Parent project UUID for sub-projects"
          , "priority"     .= prop "integer" "1-10, default 5"
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

    , mkTool "task_create" "Create a workspace- or project-scoped task. Use parent_id for subtasks; when parent_id is set, project_id should match the parent task's project." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace"
          , "project_id"  .= prop "string" "UUID of the project (optional)"
          , "title"       .= propMaxLength "string" "Task title" maxNameBytes
          , "description" .= propMaxLength "string" "Task description" maxDescriptionBytes
          , "parent_id"   .= prop "string" "Parent task UUID for sub-tasks"
          , "priority"    .= prop "integer" "1-10, default 5"
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
          , "priority"   .= prop "integer" "Filter by priority (1-10)"
          , "created_after"  .= prop "string" "Filter for tasks created on or after this ISO 8601 timestamp"
          , "created_before" .= prop "string" "Filter for tasks created on or before this ISO 8601 timestamp"
          , "updated_after"  .= prop "string" "Filter for tasks updated on or after this ISO 8601 timestamp"
          , "updated_before" .= prop "string" "Filter for tasks updated on or before this ISO 8601 timestamp"
          , "limit"      .= prop "integer" "Max results (default 50)"
          , "offset"     .= prop "integer" "Offset for pagination (default 0)"
          ]
      , "required" .= ([] :: [Text])
      ]

    , mkTool "task_update" "Edit, move, or reparent an existing task. Use project_id and/or parent_id to reorganize it; null clears those fields. Moving a task across projects also moves its task subtree." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "task_id"     .= prop "string" "UUID of the task"
          , "title"       .= propMaxLength "string" "New title" maxNameBytes
          , "description" .= propMaxLength "string" "New description (null to clear)" maxDescriptionBytes
          , "project_id"  .= prop "string" "New project UUID (null to clear)"
          , "parent_id"   .= prop "string" "New parent task UUID (null to clear)"
          , "status"      .= propEnum "string" "New status"
              ["todo", "in_progress", "blocked", "done", "cancelled"]
          , "priority"    .= prop "integer" "New priority (1-10)"
          , "metadata"    .= prop "object" "New metadata JSON object"
          , "due_at"      .= prop "string" "ISO 8601 due date (null to clear)"
          ]
      , "required" .= [t "task_id"]
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

  , mkTool "workspace_delete" "Soft-delete a workspace, hiding it and its contents from active views. Recoverable until permanently removed with workspace_purge." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace to delete" ]
      , "required" .= [t "workspace_id"]
      ]

  , mkTool "workspace_purge" "Permanently and irreversibly remove a soft-deleted workspace and all its data. Must be soft-deleted first via workspace_delete." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the soft-deleted workspace to purge" ]
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

  , mkTool "memory_graph" "Get the subgraph of memories reachable from a source memory via links, up to a configurable depth. Returns all nodes and edges in the connected neighborhood." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "memory_id" .= prop "string" "UUID of the source memory"
          , "depth"     .= prop "integer" "Max traversal depth (1-5, default 2)"
          ]
      , "required" .= [t "memory_id"]
      ]

  , mkTool "memory_find_by_relation" "Find all memory links of a specific relation type in a workspace. Useful for discovering dependency chains (depends_on), contradiction clusters (contradicts), or knowledge threads (elaborates)." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id"  .= prop "string" "UUID of the workspace"
          , "relation_type" .= propEnum "string" "Relation type to find"
              ["related", "supersedes", "contradicts", "elaborates", "inspires", "depends_on", "derived_from", "alternative_to"]
          ]
      , "required" .= [t "workspace_id", t "relation_type"]
      ]

  , mkTool "memory_adjust_importance" "Set a memory's importance to a new value (1-10). Higher importance protects against cleanup policy pruning and raises the memory in search relevance." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "memory_id"  .= prop "string" "UUID of the memory"
          , "importance" .= prop "integer" "New importance value (1-10)"
          ]
      , "required" .= [t "memory_id", t "importance"]
      ]

  , mkTool "project_list_memories" "List all memories linked to a project. Use project_link_memory and project_unlink_memory to manage these associations." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "project_id" .= prop "string" "UUID of the project" ]
      , "required" .= [t "project_id"]
      ]

  , mkTool "task_list_memories" "List all memories linked to a task. Use task_link_memory and task_unlink_memory to manage these associations." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "task_id" .= prop "string" "UUID of the task" ]
      , "required" .= [t "task_id"]
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

    , mkTool "project_update" "Rename, reprioritize, reparent, or detach an existing project. Use parent_id=null to move it back to the top level." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "project_id"  .= prop "string" "UUID of the project"
          , "name"        .= propMaxLength "string" "New name" maxNameBytes
          , "description" .= propMaxLength "string" "New description (null to clear)" maxDescriptionBytes
          , "parent_id"   .= prop "string" "New parent project UUID (null to clear)"
          , "status"      .= propEnum "string" "New status"
              ["active", "paused", "completed", "archived"]
          , "priority"    .= prop "integer" "New priority (1-10)"
          , "metadata"    .= prop "object" "New metadata JSON object"
          ]
      , "required" .= [t "project_id"]
      ]

  , mkTool "project_delete" "Soft-delete a project and its entire subtree of child projects. Recoverable until permanently removed with project_purge." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "project_id" .= prop "string" "UUID of the project to delete" ]
      , "required" .= [t "project_id"]
      ]

  , mkTool "project_purge" "Permanently and irreversibly remove a soft-deleted project. Must be soft-deleted first via project_delete." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "project_id" .= prop "string" "UUID of the soft-deleted project to purge" ]
      , "required" .= [t "project_id"]
      ]

  -- Issue 1: Missing task get/delete
    , mkTool "task_get" "Get a task by ID with full detail: title, description, status, priority, project_id, parent_id, due_at, metadata, and timestamps. Use task_list to discover IDs." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "task_id" .= prop "string" "UUID of the task" ]
      , "required" .= [t "task_id"]
      ]

  , mkTool "task_delete" "Soft-delete a task and its subtask tree. Recoverable until permanently removed with task_purge." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "task_id" .= prop "string" "UUID of the task to delete" ]
      , "required" .= [t "task_id"]
      ]

  , mkTool "task_purge" "Permanently and irreversibly remove a soft-deleted task. Must be soft-deleted first via task_delete." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "task_id" .= prop "string" "UUID of the soft-deleted task to purge" ]
      , "required" .= [t "task_id"]
      ]

  -- Issue 2: Memory categories
  , mkTool "category_create" "Create a category for organizing memories. Set workspace_id to scope it to one workspace, or omit for a global category. Use parent_id for nested sub-categories." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace (omit for a global category)"
          , "name"         .= propMaxLength "string" "Category name" maxNameBytes
          , "description"  .= prop "string" "Category description"
          , "parent_id"    .= prop "string" "Parent category UUID for sub-categories"
          ]
      , "required" .= [t "name"]
      ]

  , mkTool "category_get" "Get a category by ID with full detail: name, description, parent_id, workspace_id, and timestamps." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "category_id" .= prop "string" "UUID of the category" ]
      , "required" .= [t "category_id"]
      ]

    , mkTool "category_list" "List categories for a workspace, or omit workspace_id to browse global categories. Use this to find IDs before category updates or memory linking." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace (omit for global categories)"
          , "limit"        .= prop "integer" "Max results (default 50)"
          , "offset"       .= prop "integer" "Offset for pagination (default 0)"
          ]
      , "required" .= ([] :: [Text])
      ]

  , mkTool "category_update" "Update a category's name, description, or parent. Use category_list to find IDs first." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "category_id" .= prop "string" "UUID of the category"
          , "name"        .= propMaxLength "string" "New name" maxNameBytes
          , "description" .= prop "string" "New description"
          , "parent_id"   .= prop "string" "New parent category UUID"
          ]
      , "required" .= [t "category_id"]
      ]

  , mkTool "category_delete" "Soft-delete a category. Does not unlink already-linked memories. Recoverable until permanently removed with category_purge." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "category_id" .= prop "string" "UUID of the category to delete" ]
      , "required" .= [t "category_id"]
      ]

  , mkTool "category_purge" "Permanently and irreversibly remove a soft-deleted category. Must be soft-deleted first via category_delete." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "category_id" .= prop "string" "UUID of the soft-deleted category to purge" ]
      , "required" .= [t "category_id"]
      ]

  , mkTool "category_link_memory" "Link a memory to a category" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "memory_id"   .= prop "string" "UUID of the memory"
          , "category_id" .= prop "string" "UUID of the category"
          ]
      , "required" .= [t "memory_id", t "category_id"]
      ]

  , mkTool "category_unlink_memory" "Unlink a memory from a category" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "memory_id"   .= prop "string" "UUID of the memory"
          , "category_id" .= prop "string" "UUID of the category"
          ]
      , "required" .= [t "memory_id", t "category_id"]
      ]

  -- Issue 3: Task dependencies
    , mkTool "task_dependency_add" "Add an ordering dependency between two existing tasks. Use this for sequencing, not for parent/child hierarchy." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "task_id"       .= prop "string" "UUID of the task"
          , "depends_on_id" .= prop "string" "UUID of the task it depends on"
          ]
      , "required" .= [t "task_id", t "depends_on_id"]
      ]

    , mkTool "task_dependency_remove" "Remove an ordering dependency between tasks without changing hierarchy." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "task_id"       .= prop "string" "UUID of the task"
          , "depends_on_id" .= prop "string" "UUID of the dependency to remove"
          ]
      , "required" .= [t "task_id", t "depends_on_id"]
      ]

  -- Issue 4: Cross-entity memory links
  , mkTool "project_link_memory" "Link a memory to a project" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "project_id" .= prop "string" "UUID of the project"
          , "memory_id"  .= prop "string" "UUID of the memory"
          ]
      , "required" .= [t "project_id", t "memory_id"]
      ]

  , mkTool "project_unlink_memory" "Unlink a memory from a project" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "project_id" .= prop "string" "UUID of the project"
          , "memory_id"  .= prop "string" "UUID of the memory"
          ]
      , "required" .= [t "project_id", t "memory_id"]
      ]

  , mkTool "task_link_memory" "Link a memory to a task" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "task_id"   .= prop "string" "UUID of the task"
          , "memory_id" .= prop "string" "UUID of the memory"
          ]
      , "required" .= [t "task_id", t "memory_id"]
      ]

  , mkTool "task_unlink_memory" "Unlink a memory from a task" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "task_id"   .= prop "string" "UUID of the task"
          , "memory_id" .= prop "string" "UUID of the memory"
          ]
      , "required" .= [t "task_id", t "memory_id"]
      ]

  -- Issue 5: Memory link listing
  , mkTool "memory_links_list" "List all outgoing and incoming links for a memory, including relation types and connected memory IDs." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "memory_id" .= prop "string" "UUID of the memory" ]
      , "required" .= [t "memory_id"]
      ]

  -- Issue 7: Set tags on a memory
    , mkTool "memory_set_tags" "Set tags on a memory, replacing the existing tag set. Use memory_get_tags first if you need to merge manually." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "memory_id" .= prop "string" "UUID of the memory"
          , "tags"      .= object ["type" .= t "array", "items" .= object ["type" .= t "string"],
                                    "description" .= t "Tags to set on the memory"]
          ]
      , "required" .= [t "memory_id", t "tags"]
      ]

  -- Issue 8: Unlink memories
  , mkTool "memory_unlink" "Remove a link between two memories" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "source_id"     .= prop "string" "Source memory UUID"
          , "target_id"     .= prop "string" "Target memory UUID"
          , "relation_type" .= propEnum "string" "Relation type of the link to remove"
              ["related", "supersedes", "contradicts", "elaborates", "inspires", "depends_on", "derived_from", "alternative_to"]
          ]
      , "required" .= [t "source_id", t "target_id", t "relation_type"]
      ]

  -- Issue 10: Cleanup policy CRUD
    , mkTool "cleanup_policies_list" "List cleanup policies for a workspace before changing them or running cleanup." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace"
          , "limit"        .= prop "integer" "Max results (default 50)"
          , "offset"       .= prop "integer" "Offset for pagination (default 0)"
          ]
      , "required" .= [t "workspace_id"]
      ]

    , mkTool "cleanup_policy_upsert" "Create or update a cleanup policy for one memory type within a workspace." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id"   .= prop "string" "UUID of the workspace"
          , "memory_type"    .= propEnum "string" "Memory type to clean"
              ["short_term", "long_term"]
          , "max_age_hours"  .= prop "integer" "Max age in hours before cleanup"
          , "max_count"      .= prop "integer" "Max memory count before cleanup"
          , "min_importance" .= prop "integer" "Minimum importance to keep (1-10)"
          , "enabled"        .= prop "boolean" "Whether the policy is active"
          ]
      , "required" .= [t "workspace_id", t "memory_type", t "min_importance", t "enabled"]
      ]

  -- Pin / unpin
  , mkTool "memory_pin" "Pin a memory for quick access" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "memory_id" .= prop "string" "UUID of the memory to pin" ]
      , "required" .= [t "memory_id"]
      ]

  , mkTool "memory_unpin" "Unpin a memory" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "memory_id" .= prop "string" "UUID of the memory to unpin" ]
      , "required" .= [t "memory_id"]
      ]

  -- Workspace groups
  , mkTool "workspace_group_create" "Create a workspace group to organize related workspaces into a portfolio for cross-repo coordination or team organization." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "name"        .= propMaxLength "string" "Group name" maxNameBytes
          , "description" .= prop "string" "Group description"
          ]
      , "required" .= [t "name"]
      ]

  , mkTool "workspace_group_get" "Get a workspace group by ID with full detail: name, description, and timestamps." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "group_id" .= prop "string" "UUID of the workspace group" ]
      , "required" .= [t "group_id"]
      ]

  , mkTool "workspace_group_list" "List all workspace groups. Use this to find group IDs before adding or removing members." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "limit"  .= prop "integer" "Max results (default 50)"
          , "offset" .= prop "integer" "Offset for pagination (default 0)"
          ]
      , "required" .= ([] :: [Text])
      ]

  , mkTool "workspace_group_delete" "Delete a workspace group. Does not affect member workspaces themselves." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "group_id" .= prop "string" "UUID of the workspace group to delete" ]
      , "required" .= [t "group_id"]
      ]

  , mkTool "workspace_group_add_member" "Add a workspace to a group. A workspace can belong to multiple groups simultaneously." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "group_id"     .= prop "string" "UUID of the workspace group"
          , "workspace_id" .= prop "string" "UUID of the workspace to add"
          ]
      , "required" .= [t "group_id", t "workspace_id"]
      ]

  , mkTool "workspace_group_remove_member" "Remove a workspace from a group without affecting the workspace itself." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "group_id"     .= prop "string" "UUID of the workspace group"
          , "workspace_id" .= prop "string" "UUID of the workspace to remove"
          ]
      , "required" .= [t "group_id", t "workspace_id"]
      ]

  , mkTool "workspace_group_list_members" "List all workspaces belonging to a group." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "group_id" .= prop "string" "UUID of the workspace group" ]
      , "required" .= [t "group_id"]
      ]

  -- Activity timeline
    , mkTool "activity_timeline" "Browse recent changes across the system or within one workspace. Filter by entity_type to see only memory, project, or task events. Useful for audits and timeline-style review." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace (omit for all)"
          , "entity_type"  .= propEnum "string" "Filter to one entity type" ["memory", "project", "task"]
          , "limit"        .= prop "integer" "Max events to return (default 50)"
          ]
      , "required" .= ([] :: [Text])
      ]

  -- Tags
    , mkTool "memory_get_tags" "Get the current tags for a memory before replacing them with memory_set_tags." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "memory_id" .= prop "string" "UUID of the memory" ]
      , "required" .= [t "memory_id"]
      ]

  -- Embedding / vector similarity (requires pgvector extension)
  , mkTool "memory_similar" "Find memories similar to a given embedding vector (cosine similarity). Requires pgvector." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id"    .= prop "string" "UUID of the workspace"
          , "embedding"       .= object ["type" .= t "array", "items" .= object ["type" .= t "number"],
                                          "description" .= t "Query embedding vector (e.g. 1536 floats from OpenAI text-embedding-ada-002)"]
          , "limit"           .= prop "integer" "Max results (default 10)"
          , "min_similarity"  .= prop "number" "Minimum cosine similarity 0.0-1.0 (default 0.0)"
          ]
      , "required" .= [t "workspace_id", t "embedding"]
      ]

  , mkTool "memory_set_embedding" "Store an embedding vector on a memory. Requires pgvector." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "memory_id"  .= prop "string" "UUID of the memory"
          , "embedding"  .= object ["type" .= t "array", "items" .= object ["type" .= t "number"],
                                     "description" .= t "Embedding vector (e.g. 1536 floats)"]
          ]
      , "required" .= [t "memory_id", t "embedding"]
      ]

  -- Batch operations
  , mkTool "memory_delete_batch" "Soft-delete multiple memories at once. Max 100 IDs per call. Returns the number actually deleted (already-deleted or missing IDs are skipped)." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "ids" .= object ["type" .= t "array", "items" .= object ["type" .= t "string"],
                              "description" .= t "Array of memory UUIDs to soft-delete",
                              "minItems" .= (1 :: Int), "maxItems" .= (100 :: Int)]
          ]
      , "required" .= [t "ids"]
      ]

  , mkTool "task_delete_batch" "Soft-delete multiple tasks at once. Max 100 IDs per call. Does not cascade to subtasks. Returns the number actually deleted." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "ids" .= object ["type" .= t "array", "items" .= object ["type" .= t "string"],
                              "description" .= t "Array of task UUIDs to soft-delete",
                              "minItems" .= (1 :: Int), "maxItems" .= (100 :: Int)]
          ]
      , "required" .= [t "ids"]
      ]

  , mkTool "task_move_batch" "Move multiple tasks to a different project (or detach from all projects by passing null for project_id). Max 100 tasks per call." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "task_ids"   .= object ["type" .= t "array", "items" .= object ["type" .= t "string"],
                                     "description" .= t "Array of task UUIDs to move",
                                     "minItems" .= (1 :: Int), "maxItems" .= (100 :: Int)]
          , "project_id" .= prop "string" "Target project UUID (null to detach tasks from any project)"
          ]
      , "required" .= [t "task_ids"]
      ]

  , mkTool "project_link_memories_batch" "Link multiple memories to a single project at once. Idempotent: already-linked pairs are silently skipped. Max 100 memory IDs." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "project_id" .= prop "string" "UUID of the project"
          , "memory_ids" .= object ["type" .= t "array", "items" .= object ["type" .= t "string"],
                                     "description" .= t "Array of memory UUIDs to link",
                                     "minItems" .= (1 :: Int), "maxItems" .= (100 :: Int)]
          ]
      , "required" .= [t "project_id", t "memory_ids"]
      ]

  , mkTool "task_link_memories_batch" "Link multiple memories to a single task at once. Idempotent: already-linked pairs are silently skipped. Max 100 memory IDs." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "task_id"    .= prop "string" "UUID of the task"
          , "memory_ids" .= object ["type" .= t "array", "items" .= object ["type" .= t "string"],
                                     "description" .= t "Array of memory UUIDs to link",
                                     "minItems" .= (1 :: Int), "maxItems" .= (100 :: Int)]
          ]
      , "required" .= [t "task_id", t "memory_ids"]
      ]

  , mkTool "memory_set_tags_batch" "Set tags on multiple memories at once, each with its own tag list. Replaces existing tags for each memory. Max 100 items." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "items" .= object ["type" .= t "array",
                                "description" .= t "Array of {memory_id, tags} objects",
                                "minItems" .= (1 :: Int), "maxItems" .= (100 :: Int),
                                "items" .= object
                                    [ "type" .= t "object"
                                    , "properties" .= object
                                        [ "memory_id" .= prop "string" "UUID of the memory"
                                        , "tags" .= object ["type" .= t "array", "items" .= object ["type" .= t "string"],
                                                             "description" .= t "New tags for this memory"]
                                        ]
                                    , "required" .= [t "memory_id", t "tags"]
                                    ]]
          ]
      , "required" .= [t "items"]
      ]
  ]

------------------------------------------------------------------------
-- Typed tool calls
------------------------------------------------------------------------

data ToolCall
  = MemoryCreate   CreateMemory
  | MemoryCreateBatch [CreateMemory]
  | MemorySearch   SearchQuery Bool         -- query, detail
  | MemoryGet      UUID
  | MemoryUpdate   UUID UpdateMemory
  | MemoryDelete   UUID
    | MemoryPurge    UUID
  | LinkMemories   UUID CreateMemoryLink   -- source_id, link body
  | MemoryLinksList UUID
  | ProjectCreate  CreateProject
  | ProjectGet     UUID
  | ProjectUpdate  UUID UpdateProject
  | ProjectDelete  UUID
    | ProjectPurge   UUID
    | ProjectList    ProjectListQuery
  | ProjectLinkMem UUID UUID               -- project_id, memory_id
  | ProjectUnlinkMem UUID UUID             -- project_id, memory_id
  | TaskCreate     CreateTask
  | TaskGet        UUID
  | TaskDelete     UUID
    | TaskPurge      UUID
    | TaskList       TaskListQuery
  | TaskUpdate     UUID UpdateTask
  | TaskLinkMem    UUID UUID               -- task_id, memory_id
  | TaskUnlinkMem  UUID UUID               -- task_id, memory_id
  | TaskDepAdd     UUID UUID               -- task_id, depends_on_id
  | TaskDepRemove  UUID UUID               -- task_id, depends_on_id
  | WorkspaceList (Maybe Int) (Maybe Int)
  | WorkspaceGet   UUID
  | WsUpdate       UUID UpdateWorkspace
  | WsDelete       UUID
    | WsPurge        UUID
  | CategoryCreate CreateMemoryCategory
  | CategoryGet    UUID
  | CategoryList   (Maybe UUID) (Maybe Int) (Maybe Int) -- maybe workspace_id, limit, offset
  | CategoryUpdate UUID UpdateMemoryCategory
  | CategoryDelete UUID
    | CategoryPurge  UUID
  | CategoryLinkMem   UUID UUID            -- memory_id, category_id
  | CategoryUnlinkMem UUID UUID            -- memory_id, category_id
  | MemorySetTags   UUID [Text]            -- memory_id, tags
  | MemoryUnlink    UUID UUID RelationType -- source_id, target_id, relation_type
  | CleanupPoliciesList UUID (Maybe Int) (Maybe Int) -- workspace_id, limit, offset
  | CleanupPolicyUpsert UpsertCleanupPolicy
  | WorkspaceReg   CreateWorkspace
  | CleanupRun     UUID
    | MemoryList     MemoryListQuery Bool      -- query, detail
  | MemoryGraphCall UUID (Maybe Int)
  | MemoryFindByRelation UUID RelationType
  | MemoryAdjustImp UUID Int
  | ProjectListMem UUID
  | TaskListMem    UUID
  | MemoryPin      UUID
  | MemoryUnpin    UUID
  | WsGroupCreate  CreateWorkspaceGroup
  | WsGroupGet     UUID
  | WsGroupList    (Maybe Int) (Maybe Int)
  | WsGroupDelete  UUID
  | WsGroupAddMem  UUID UUID  -- group_id, workspace_id
  | WsGroupRmMem   UUID UUID  -- group_id, workspace_id
  | WsGroupListMem UUID
  | ActivityTimeline (Maybe UUID) (Maybe Text) (Maybe Int)
  | MemoryGetTags UUID
  | MemorySimilar SimilarQuery
  | MemorySetEmbedding UUID [Double]
  | MemoryDeleteBatch [UUID]
  | TaskDeleteBatch [UUID]
  | TaskMoveBatch [UUID] (Maybe UUID)     -- task_ids, project_id
  | ProjectLinkMemBatch UUID [UUID]       -- project_id, memory_ids
  | TaskLinkMemBatch UUID [UUID]          -- task_id, memory_ids
  | MemorySetTagsBatch [(UUID, [Text])]   -- [(memory_id, tags)]
  deriving (Show, Eq)

-- | Parse raw JSON-RPC params (containing "name" and "arguments") into
-- a typed ToolCall, validating all fields against HMem.Types.
parseToolCall :: Text -> Value -> Either String ToolCall
parseToolCall name args = case name of
    "memory_create"            -> MemoryCreate <$> parse args
    "memory_create_batch"      -> MemoryCreateBatch <$> need "memories"
    "memory_search"            -> MemorySearch <$> parse args <*> (maybe False id <$> opt "detail")
    "memory_get"               -> MemoryGet <$> need "memory_id"
    "memory_update"            -> MemoryUpdate <$> need "memory_id" <*> parse args
    "memory_delete"            -> MemoryDelete <$> need "memory_id"
    "memory_purge"             -> MemoryPurge <$> need "memory_id"
    "memory_link"              -> LinkMemories <$> need "source_id" <*> parse args
    "memory_links_list"        -> MemoryLinksList <$> need "memory_id"
    "project_create"           -> ProjectCreate <$> parse args
    "project_get"              -> ProjectGet <$> need "project_id"
    "project_update"           -> ProjectUpdate <$> need "project_id" <*> parse args
    "project_delete"           -> ProjectDelete <$> need "project_id"
    "project_purge"            -> ProjectPurge <$> need "project_id"
    "project_list"             -> ProjectList <$> parse args
    "project_link_memory"      -> ProjectLinkMem <$> need "project_id" <*> need "memory_id"
    "project_unlink_memory"    -> ProjectUnlinkMem <$> need "project_id" <*> need "memory_id"
    "task_create"              -> TaskCreate <$> parse args
    "task_get"                 -> TaskGet <$> need "task_id"
    "task_delete"              -> TaskDelete <$> need "task_id"
    "task_purge"               -> TaskPurge <$> need "task_id"
    "task_list"                -> TaskList <$> parse args
    "task_update"              -> TaskUpdate <$> need "task_id" <*> parse args
    "task_link_memory"         -> TaskLinkMem <$> need "task_id" <*> need "memory_id"
    "task_unlink_memory"       -> TaskUnlinkMem <$> need "task_id" <*> need "memory_id"
    "task_dependency_add"      -> TaskDepAdd <$> need "task_id" <*> need "depends_on_id"
    "task_dependency_remove"   -> TaskDepRemove <$> need "task_id" <*> need "depends_on_id"
    "category_create"          -> CategoryCreate <$> parse args
    "category_get"             -> CategoryGet <$> need "category_id"
    "category_list"            -> CategoryList <$> opt "workspace_id" <*> opt "limit" <*> opt "offset"
    "category_update"          -> CategoryUpdate <$> need "category_id" <*> parse args
    "category_delete"          -> CategoryDelete <$> need "category_id"
    "category_purge"           -> CategoryPurge <$> need "category_id"
    "category_link_memory"     -> CategoryLinkMem <$> need "memory_id" <*> need "category_id"
    "category_unlink_memory"   -> CategoryUnlinkMem <$> need "memory_id" <*> need "category_id"
    "memory_set_tags"          -> MemorySetTags <$> need "memory_id" <*> need "tags"
    "memory_unlink"            -> MemoryUnlink <$> need "source_id" <*> need "target_id" <*> need "relation_type"
    "cleanup_policies_list"    -> CleanupPoliciesList <$> need "workspace_id" <*> opt "limit" <*> opt "offset"
    "cleanup_policy_upsert"    -> CleanupPolicyUpsert <$> parse args
    "workspace_list"           -> WorkspaceList <$> opt "limit" <*> opt "offset"
    "workspace_get"            -> WorkspaceGet <$> need "workspace_id"
    "workspace_update"         -> WsUpdate <$> need "workspace_id" <*> parse args
    "workspace_delete"         -> WsDelete <$> need "workspace_id"
    "workspace_purge"          -> WsPurge <$> need "workspace_id"
    "workspace_register"       -> WorkspaceReg <$> parse args
    "cleanup_run"              -> CleanupRun <$> need "workspace_id"
    "memory_list"              -> MemoryList <$> parse args <*> (maybe False id <$> opt "detail")
    "memory_graph"             -> MemoryGraphCall <$> need "memory_id" <*> opt "depth"
    "memory_find_by_relation"  -> MemoryFindByRelation <$> need "workspace_id" <*> need "relation_type"
    "memory_adjust_importance" -> MemoryAdjustImp <$> need "memory_id" <*> need "importance"
    "project_list_memories"    -> ProjectListMem <$> need "project_id"
    "task_list_memories"       -> TaskListMem <$> need "task_id"
    "memory_pin"               -> MemoryPin <$> need "memory_id"
    "memory_unpin"             -> MemoryUnpin <$> need "memory_id"
    "workspace_group_create"   -> WsGroupCreate <$> parse args
    "workspace_group_get"      -> WsGroupGet <$> need "group_id"
    "workspace_group_list"     -> WsGroupList <$> opt "limit" <*> opt "offset"
    "workspace_group_delete"   -> WsGroupDelete <$> need "group_id"
    "workspace_group_add_member" -> WsGroupAddMem <$> need "group_id" <*> need "workspace_id"
    "workspace_group_remove_member" -> WsGroupRmMem <$> need "group_id" <*> need "workspace_id"
    "workspace_group_list_members" -> WsGroupListMem <$> need "group_id"
    "activity_timeline"        -> ActivityTimeline <$> opt "workspace_id" <*> opt "entity_type" <*> opt "limit"
    "memory_get_tags"          -> MemoryGetTags <$> need "memory_id"
    "memory_similar"           -> MemorySimilar <$> parse args
    "memory_set_embedding"     -> MemorySetEmbedding <$> need "memory_id" <*> need "embedding"
    "memory_delete_batch"       -> MemoryDeleteBatch <$> need "ids"
    "task_delete_batch"         -> TaskDeleteBatch <$> need "ids"
    "task_move_batch"           -> TaskMoveBatch <$> need "task_ids" <*> opt "project_id"
    "project_link_memories_batch" -> ProjectLinkMemBatch <$> need "project_id" <*> need "memory_ids"
    "task_link_memories_batch"  -> TaskLinkMemBatch <$> need "task_id" <*> need "memory_ids"
    "memory_set_tags_batch"     -> MemorySetTagsBatch <$> parseBatchSetTags args
    _                           -> Left $ "Unknown tool: " <> T.unpack name
  where
    parse :: FromJSON a => Value -> Either String a
    parse = parseEither parseJSON

    need :: FromJSON a => Key -> Either String a
    need k = parseEither (withObject "args" (.: k)) args

    opt :: FromJSON a => Key -> Either String (Maybe a)
    opt k = parseEither (withObject "args" (.:? k)) args

-- | Parse the batch set-tags items array into [(UUID, [Text])].
parseBatchSetTags :: Value -> Either String [(UUID, [Text])]
parseBatchSetTags = parseEither $ withObject "args" $ \o -> do
  items <- o .: "items" :: Parser [Value]
  mapM parseItem items
  where
    parseItem = withObject "BatchSetTagsItem" $ \o -> do
      mid  <- o .: "memory_id"
      tags <- o .: "tags"
      pure (mid, tags)

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
    MemoryAdjustImp mid imp -> Right $ MemoryAdjustImp mid (clamp 1 10 imp)
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
    TaskUpdate tid ut -> TaskUpdate tid ut <$ firstValidationError (validateUpdateTaskInput ut)
    CategoryCreate cc -> CategoryCreate cc <$ firstValidationError (validateCreateMemoryCategoryInput cc)
    CategoryList mwid ml mo -> Right $ CategoryList mwid (clampMaybe 1 200 <$> ml) (clampMaybe 0 10000 <$> mo)
    CategoryUpdate cid uc -> CategoryUpdate cid uc <$ firstValidationError (validateUpdateMemoryCategoryInput uc)
    WsGroupCreate cg -> WsGroupCreate cg <$ firstValidationError (validateCreateWorkspaceGroupInput cg)
    WsGroupList ml mo -> Right $ WsGroupList (clampMaybe 1 200 <$> ml) (clampMaybe 0 10000 <$> mo)
    WorkspaceReg cw -> WorkspaceReg cw <$ firstValidationError (validateCreateWorkspaceInput cw)
    WorkspaceList ml mo -> Right $ WorkspaceList (clampMaybe 1 200 <$> ml) (clampMaybe 0 10000 <$> mo)
    WsUpdate wid uw -> WsUpdate wid uw <$ firstValidationError (validateUpdateWorkspaceInput uw)
    CleanupPoliciesList wid ml mo -> Right $ CleanupPoliciesList wid (clampMaybe 1 200 <$> ml) (clampMaybe 0 10000 <$> mo)
    ActivityTimeline mws met ml -> Right $ ActivityTimeline mws met (clampMaybe 1 200 <$> ml)
    MemorySimilar sq -> Right $ MemorySimilar sq
        { limit = clampMaybe 1 200 <$> sq.limit
        , minSimilarity = clampMaybe 0.0 1.0 <$> sq.minSimilarity
        }
    MemorySetEmbedding mid vec
        | null vec  -> Left "memory_set_embedding: embedding must not be empty"
        | otherwise -> Right $ MemorySetEmbedding mid vec
    MemoryDeleteBatch ids -> validateBatchIds "memory_delete_batch" ids (MemoryDeleteBatch ids)
    TaskDeleteBatch ids -> validateBatchIds "task_delete_batch" ids (TaskDeleteBatch ids)
    TaskMoveBatch ids pid -> validateBatchIds "task_move_batch" ids (TaskMoveBatch ids pid)
    ProjectLinkMemBatch pid mids -> validateBatchIds "project_link_memories_batch" mids (ProjectLinkMemBatch pid mids)
    TaskLinkMemBatch tid mids -> validateBatchIds "task_link_memories_batch" mids (TaskLinkMemBatch tid mids)
    MemorySetTagsBatch items
        | null items -> Left "memory_set_tags_batch: items must not be empty"
        | length items > 100 -> Left "memory_set_tags_batch: items must contain at most 100 items"
        | otherwise -> Right $ MemorySetTagsBatch items
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

-- | Execute a typed tool call against the hmem-server HTTP API.
executeToolCall :: Manager -> String -> Maybe Text -> ToolCall -> IO Value
executeToolCall mgr base mApiKey = \case
    MemoryCreate cm     -> postJSON mgr base mApiKey "/api/v1/memories" cm
    MemoryCreateBatch cms -> postJSON mgr base mApiKey "/api/v1/memories/batch" cms
    MemorySearch sq detail -> postJSON mgr base mApiKey ("/api/v1/memories/search?compact=" <> if detail then "false" else "true") sq
    MemoryGet mid       -> getJSON  mgr base mApiKey ("/api/v1/memories/" <> uuidPath mid)
    MemoryUpdate mid um -> putJSON  mgr base mApiKey ("/api/v1/memories/" <> uuidPath mid) um
    MemoryDelete mid    -> delJSON  mgr base mApiKey ("/api/v1/memories/" <> uuidPath mid)
    MemoryPurge mid     -> delJSON  mgr base mApiKey ("/api/v1/memories/" <> uuidPath mid <> "/purge")
    LinkMemories sid cl -> postJSON mgr base mApiKey ("/api/v1/memories/" <> uuidPath sid <> "/links") cl
    MemoryLinksList mid -> getJSON  mgr base mApiKey ("/api/v1/memories/" <> uuidPath mid <> "/links")
    ProjectCreate cp    -> postJSON mgr base mApiKey "/api/v1/projects" cp
    ProjectGet pid      -> getJSON  mgr base mApiKey ("/api/v1/projects/" <> uuidPath pid)
    ProjectUpdate pid up -> putJSON mgr base mApiKey ("/api/v1/projects/" <> uuidPath pid) up
    ProjectDelete pid   -> delJSON  mgr base mApiKey ("/api/v1/projects/" <> uuidPath pid)
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
    TaskDelete tid      -> delJSON  mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid)
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
    CategoryPurge cid   -> delJSON  mgr base mApiKey ("/api/v1/categories/" <> uuidPath cid <> "/purge")
    CategoryLinkMem mid cid -> postJSON mgr base mApiKey "/api/v1/categories/link"
                               (object ["memory_id" .= mid, "category_id" .= cid])
    CategoryUnlinkMem mid cid -> postJSON mgr base mApiKey "/api/v1/categories/unlink"
                                  (object ["memory_id" .= mid, "category_id" .= cid])
    MemorySetTags mid tags -> putJSON mgr base mApiKey ("/api/v1/memories/" <> uuidPath mid <> "/tags")
                              (toJSON tags)
    MemoryUnlink sid tid rt -> delJSON mgr base mApiKey ("/api/v1/memories/" <> uuidPath sid <> "/links/"
                               <> uuidPath tid <> "/" <> T.unpack (relationTypeToText rt))
    CleanupPoliciesList wid ml mo -> getJSON mgr base mApiKey ("/api/v1/cleanup/policies" <> buildQuery
                            [ ("workspace_id", Just $ uuidPath wid)
                            , ("limit", show <$> ml)
                            , ("offset", show <$> mo)
                            ])
    CleanupPolicyUpsert cp  -> postJSON mgr base mApiKey "/api/v1/cleanup/policies" cp
    WorkspaceList ml mo      -> getJSON  mgr base mApiKey ("/api/v1/workspaces" <> buildQuery
                            [ ("limit", show <$> ml)
                            , ("offset", show <$> mo)
                            ])
    WorkspaceGet wid        -> getJSON  mgr base mApiKey ("/api/v1/workspaces/" <> uuidPath wid)
    WsUpdate wid uw         -> putJSON  mgr base mApiKey ("/api/v1/workspaces/" <> uuidPath wid) uw
    WsDelete wid            -> delJSON  mgr base mApiKey ("/api/v1/workspaces/" <> uuidPath wid)
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
    MemoryAdjustImp mid imp -> putJSON mgr base mApiKey ("/api/v1/memories/" <> uuidPath mid <> "/importance")
                              (object ["importance" .= imp])
    ProjectListMem pid -> getJSON mgr base mApiKey ("/api/v1/projects/" <> uuidPath pid <> "/memories")
    TaskListMem tid -> getJSON mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid <> "/memories")
    MemoryPin mid    -> postJSON mgr base mApiKey ("/api/v1/memories/" <> uuidPath mid <> "/pin") (object [])
    MemoryUnpin mid  -> postJSON mgr base mApiKey ("/api/v1/memories/" <> uuidPath mid <> "/unpin") (object [])
    WsGroupCreate cg -> postJSON mgr base mApiKey "/api/v1/groups" cg
    WsGroupGet gid   -> getJSON  mgr base mApiKey ("/api/v1/groups/" <> uuidPath gid)
    WsGroupList ml mo -> getJSON mgr base mApiKey ("/api/v1/groups" <> buildQuery
                            [ ("limit", show <$> ml)
                            , ("offset", show <$> mo)
                            ])
    WsGroupDelete gid -> delJSON mgr base mApiKey ("/api/v1/groups/" <> uuidPath gid)
    WsGroupAddMem gid wsId -> postJSON mgr base mApiKey ("/api/v1/groups/" <> uuidPath gid <> "/members")
                              (object ["workspace_id" .= wsId])
    WsGroupRmMem gid wsId -> delJSON mgr base mApiKey ("/api/v1/groups/" <> uuidPath gid <> "/members/"
                              <> uuidPath wsId)
    WsGroupListMem gid -> getJSON mgr base mApiKey ("/api/v1/groups/" <> uuidPath gid <> "/members")
    ActivityTimeline mws met ml -> getJSON mgr base mApiKey ("/api/v1/activity" <> buildQuery
                            [ ("workspace_id", uuidPath <$> mws)
                            , ("entity_type", T.unpack <$> met)
                            , ("limit", show <$> ml)
                            ])
    MemoryGetTags mid -> getJSON mgr base mApiKey ("/api/v1/memories/" <> uuidPath mid <> "/tags")
    MemorySimilar sq -> postJSON mgr base mApiKey "/api/v1/memories/similar" sq
    MemorySetEmbedding mid vec -> putJSON mgr base mApiKey ("/api/v1/memories/" <> uuidPath mid <> "/embedding")
                                  (toJSON vec)
    MemoryDeleteBatch ids -> postJSON mgr base mApiKey "/api/v1/memories/batch-delete"
                              (object ["ids" .= ids])
    TaskDeleteBatch ids -> postJSON mgr base mApiKey "/api/v1/tasks/batch-delete"
                            (object ["ids" .= ids])
    TaskMoveBatch ids pid -> postJSON mgr base mApiKey "/api/v1/tasks/batch-move"
                              (object ["task_ids" .= ids, "project_id" .= pid])
    ProjectLinkMemBatch pid mids -> postJSON mgr base mApiKey ("/api/v1/projects/" <> uuidPath pid <> "/memories/batch")
                                    (object ["memory_ids" .= mids])
    TaskLinkMemBatch tid mids -> postJSON mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid <> "/memories/batch")
                                  (object ["memory_ids" .= mids])
    MemorySetTagsBatch items -> postJSON mgr base mApiKey "/api/v1/memories/batch-set-tags"
                                (object ["items" .= [object ["memory_id" .= mid, "tags" .= tags] | (mid, tags) <- items]])

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

------------------------------------------------------------------------
-- MCP content helpers
------------------------------------------------------------------------

mcpResult :: BL.ByteString -> Value
mcpResult body = object
  [ "content" .= [object ["type" .= ("text" :: Text), "text" .= decodeUtf8 body]] ]

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
toolApiVersion = "0.1.0"

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
