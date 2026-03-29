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
import Data.Aeson.Types (parseEither)
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
  [ mkTool "memory_create" "Create a new memory in a workspace" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace"
          , "content"      .= prop "string" "The memory content"
          , "summary"      .= prop "string" "Optional short summary"
          , "memory_type"  .= propEnum "string" "short_term or long_term" ["short_term", "long_term"]
          , "importance"   .= prop "integer" "1-10, default 5"
          , "source"       .= prop "string" "Provenance: user_stated, inferred, tool_output, web_search"
          , "confidence"   .= prop "number" "Confidence level 0.0-1.0, default 1.0"
          , "pinned"       .= prop "boolean" "Pin this memory (default false)"
          , "tags"         .= object ["type" .= t "array", "items" .= object ["type" .= t "string"],
                                       "description" .= t "Tags for categorization"]
          , "fts_language" .= prop "string" "Full-text search language (default 'english'). Use a PostgreSQL regconfig name, e.g. 'spanish', 'german', 'simple'."
          ]
      , "required" .= [t "workspace_id", t "content", t "memory_type"]
      ]

  , mkTool "memory_create_batch" "Create multiple memories in a single transaction. Max 100 items per batch." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "memories" .= object
              [ "type" .= t "array"
              , "description" .= t "Array of memory objects to create (same schema as memory_create)"
              , "items" .= object
                  [ "type" .= t "object"
                  , "properties" .= object
                      [ "workspace_id" .= prop "string" "UUID of the workspace"
                      , "content"      .= prop "string" "The memory content"
                      , "summary"      .= prop "string" "Optional short summary"
                      , "memory_type"  .= propEnum "string" "short_term or long_term" ["short_term", "long_term"]
                      , "importance"   .= prop "integer" "1-10, default 5"
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

  , mkTool "memory_search" "Search memories using full-text search and/or filters. Use 'query' for keyword search, or combine filters (workspace_id, memory_type, tags, min_importance, category_id, pinned_only) to narrow results. All parameters are optional; omit workspace_id for cross-workspace search." $ object
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
          , "limit"          .= prop "integer" "Max results (default 50)"
          , "offset"         .= prop "integer" "Offset for pagination (default 0)"
          ]
      , "required" .= ([] :: [Text])
      ]

  , mkTool "memory_get" "Get a specific memory by ID" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "memory_id" .= prop "string" "UUID of the memory" ]
      , "required" .= [t "memory_id"]
      ]

  , mkTool "memory_update" "Update an existing memory" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "memory_id"   .= prop "string" "UUID of the memory to update"
          , "content"     .= prop "string" "New content"
          , "summary"     .= prop "string" "New summary (null to clear)"
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

  , mkTool "memory_delete" "Delete a memory" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "memory_id" .= prop "string" "UUID of the memory to delete" ]
      , "required" .= [t "memory_id"]
      ]

  , mkTool "memory_link" "Create a typed link between two memories" $ object
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

  , mkTool "project_create" "Create a new project in a workspace" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace"
          , "name"         .= prop "string" "Project name"
          , "description"  .= prop "string" "Project description"
          , "parent_id"    .= prop "string" "Parent project UUID for sub-projects"
          , "priority"     .= prop "integer" "1-10, default 5"
          ]
      , "required" .= [t "workspace_id", t "name"]
      ]

  , mkTool "project_list" "List projects in a workspace" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace"
          , "status"       .= propEnum "string" "Filter by status"
              ["active", "paused", "completed", "archived"]
          , "limit"        .= prop "integer" "Max results (default 50)"
          , "offset"       .= prop "integer" "Offset for pagination (default 0)"
          ]
      , "required" .= [t "workspace_id"]
      ]

  , mkTool "task_create" "Create a new task in a workspace (optionally in a project)" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace"
          , "project_id"  .= prop "string" "UUID of the project (optional)"
          , "title"       .= prop "string" "Task title"
          , "description" .= prop "string" "Task description"
          , "parent_id"   .= prop "string" "Parent task UUID for sub-tasks"
          , "priority"    .= prop "integer" "1-10, default 5"
          , "due_at"      .= prop "string" "ISO 8601 due date"
          ]
      , "required" .= [t "workspace_id", t "title"]
      ]

  , mkTool "task_list" "List tasks. Requires at least workspace_id or project_id." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace (use this or project_id)"
          , "project_id" .= prop "string" "UUID of the project (use this or workspace_id)"
          , "status"     .= propEnum "string" "Filter by status"
              ["todo", "in_progress", "blocked", "done", "cancelled"]
          , "limit"      .= prop "integer" "Max results (default 50)"
          , "offset"     .= prop "integer" "Offset for pagination (default 0)"
          ]
      , "required" .= ([] :: [Text])
      ]

  , mkTool "task_update" "Update an existing task" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "task_id"     .= prop "string" "UUID of the task"
          , "title"       .= prop "string" "New title"
          , "description" .= prop "string" "New description"
          , "status"      .= propEnum "string" "New status"
              ["todo", "in_progress", "blocked", "done", "cancelled"]
          , "priority"    .= prop "integer" "New priority (1-10)"
          ]
      , "required" .= [t "task_id"]
      ]

  , mkTool "workspace_register" "Register a new workspace" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "name"           .= prop "string" "Workspace name"
          , "path"           .= prop "string" "Filesystem path"
          , "gh_owner"       .= prop "string" "GitHub owner"
          , "gh_repo"        .= prop "string" "GitHub repository"
          , "workspace_type" .= propEnum "string" "Workspace type"
              ["repository", "planning", "personal", "organization"]
          ]
      , "required" .= [t "name"]
      ]

  , mkTool "workspace_list" "List all registered workspaces" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "limit"  .= prop "integer" "Max results (default 50)"
          , "offset" .= prop "integer" "Offset for pagination (default 0)"
          ]
      ]

  , mkTool "workspace_get" "Get a workspace by ID" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace" ]
      , "required" .= [t "workspace_id"]
      ]

  , mkTool "workspace_update" "Update a workspace" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id"   .= prop "string" "UUID of the workspace"
          , "name"           .= prop "string" "New workspace name"
          , "workspace_type" .= propEnum "string" "New workspace type"
              ["repository", "planning", "personal", "organization"]
          , "path"           .= prop "string" "New filesystem path (null to clear)"
          , "gh_owner"       .= prop "string" "New GitHub owner (null to clear)"
          , "gh_repo"        .= prop "string" "New GitHub repository (null to clear)"
          ]
      , "required" .= [t "workspace_id"]
      ]

  , mkTool "workspace_delete" "Delete a workspace" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace to delete" ]
      , "required" .= [t "workspace_id"]
      ]

  , mkTool "memory_list" "List memories (optionally filtered by workspace)" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace (omit for all workspaces)"
          , "memory_type"  .= propEnum "string" "Filter by type" ["short_term", "long_term"]
          , "limit"        .= prop "integer" "Max results (default 50)"
          , "offset"       .= prop "integer" "Offset for pagination (default 0)"
          ]
      , "required" .= ([] :: [Text])
      ]

  , mkTool "memory_graph" "Get a graph of related memories reachable from a source memory" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "memory_id" .= prop "string" "UUID of the source memory"
          , "depth"     .= prop "integer" "Max traversal depth (1-5, default 2)"
          ]
      , "required" .= [t "memory_id"]
      ]

  , mkTool "memory_find_by_relation" "Find all memory links of a specific relation type in a workspace" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id"  .= prop "string" "UUID of the workspace"
          , "relation_type" .= propEnum "string" "Relation type to find"
              ["related", "supersedes", "contradicts", "elaborates", "inspires", "depends_on", "derived_from", "alternative_to"]
          ]
      , "required" .= [t "workspace_id", t "relation_type"]
      ]

  , mkTool "memory_adjust_importance" "Adjust the importance of a memory (1-10)" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "memory_id"  .= prop "string" "UUID of the memory"
          , "importance" .= prop "integer" "New importance value (1-10)"
          ]
      , "required" .= [t "memory_id", t "importance"]
      ]

  , mkTool "project_list_memories" "List memories linked to a project" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "project_id" .= prop "string" "UUID of the project" ]
      , "required" .= [t "project_id"]
      ]

  , mkTool "task_list_memories" "List memories linked to a task" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "task_id" .= prop "string" "UUID of the task" ]
      , "required" .= [t "task_id"]
      ]

  , mkTool "cleanup_run" "Run memory cleanup for a workspace" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace" ]
      , "required" .= [t "workspace_id"]
      ]

  -- Issue 1: Missing project get/update/delete
  , mkTool "project_get" "Get a project by ID" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "project_id" .= prop "string" "UUID of the project" ]
      , "required" .= [t "project_id"]
      ]

  , mkTool "project_update" "Update an existing project" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "project_id"  .= prop "string" "UUID of the project"
          , "name"        .= prop "string" "New name"
          , "description" .= prop "string" "New description"
          , "status"      .= propEnum "string" "New status"
              ["active", "paused", "completed", "archived"]
          , "priority"    .= prop "integer" "New priority (1-10)"
          ]
      , "required" .= [t "project_id"]
      ]

  , mkTool "project_delete" "Delete a project" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "project_id" .= prop "string" "UUID of the project to delete" ]
      , "required" .= [t "project_id"]
      ]

  -- Issue 1: Missing task get/delete
  , mkTool "task_get" "Get a task by ID" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "task_id" .= prop "string" "UUID of the task" ]
      , "required" .= [t "task_id"]
      ]

  , mkTool "task_delete" "Delete a task" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "task_id" .= prop "string" "UUID of the task to delete" ]
      , "required" .= [t "task_id"]
      ]

  -- Issue 2: Memory categories
  , mkTool "category_create" "Create a memory category (optionally in a workspace; omit for global)" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace (omit for a global category)"
          , "name"         .= prop "string" "Category name"
          , "description"  .= prop "string" "Category description"
          , "parent_id"    .= prop "string" "Parent category UUID for sub-categories"
          ]
      , "required" .= [t "name"]
      ]

  , mkTool "category_get" "Get a memory category by ID" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "category_id" .= prop "string" "UUID of the category" ]
      , "required" .= [t "category_id"]
      ]

  , mkTool "category_list" "List memory categories. When workspace_id is provided, returns categories for that workspace. When workspace_id is omitted, returns global (cross-workspace) categories." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace (omit for global categories)"
          , "limit"        .= prop "integer" "Max results (default 50)"
          , "offset"       .= prop "integer" "Offset for pagination (default 0)"
          ]
      , "required" .= ([] :: [Text])
      ]

  , mkTool "category_update" "Update a memory category" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "category_id" .= prop "string" "UUID of the category"
          , "name"        .= prop "string" "New name"
          , "description" .= prop "string" "New description"
          , "parent_id"   .= prop "string" "New parent category UUID"
          ]
      , "required" .= [t "category_id"]
      ]

  , mkTool "category_delete" "Delete a memory category" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "category_id" .= prop "string" "UUID of the category to delete" ]
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
  , mkTool "task_dependency_add" "Add a dependency between tasks" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "task_id"       .= prop "string" "UUID of the task"
          , "depends_on_id" .= prop "string" "UUID of the task it depends on"
          ]
      , "required" .= [t "task_id", t "depends_on_id"]
      ]

  , mkTool "task_dependency_remove" "Remove a dependency between tasks" $ object
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
  , mkTool "memory_links_list" "List all links for a memory" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "memory_id" .= prop "string" "UUID of the memory" ]
      , "required" .= [t "memory_id"]
      ]

  -- Issue 7: Set tags on a memory
  , mkTool "memory_set_tags" "Set tags on a memory (replaces existing tags)" $ object
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
  , mkTool "cleanup_policies_list" "List cleanup policies for a workspace" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace"
          , "limit"        .= prop "integer" "Max results (default 50)"
          , "offset"       .= prop "integer" "Offset for pagination (default 0)"
          ]
      , "required" .= [t "workspace_id"]
      ]

  , mkTool "cleanup_policy_upsert" "Create or update a cleanup policy" $ object
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
  , mkTool "workspace_group_create" "Create a workspace group (portfolio)" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "name"        .= prop "string" "Group name"
          , "description" .= prop "string" "Group description"
          ]
      , "required" .= [t "name"]
      ]

  , mkTool "workspace_group_get" "Get a workspace group by ID" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "group_id" .= prop "string" "UUID of the workspace group" ]
      , "required" .= [t "group_id"]
      ]

  , mkTool "workspace_group_list" "List all workspace groups" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "limit"  .= prop "integer" "Max results (default 50)"
          , "offset" .= prop "integer" "Offset for pagination (default 0)"
          ]
      , "required" .= ([] :: [Text])
      ]

  , mkTool "workspace_group_delete" "Delete a workspace group" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "group_id" .= prop "string" "UUID of the workspace group to delete" ]
      , "required" .= [t "group_id"]
      ]

  , mkTool "workspace_group_add_member" "Add a workspace to a group" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "group_id"     .= prop "string" "UUID of the workspace group"
          , "workspace_id" .= prop "string" "UUID of the workspace to add"
          ]
      , "required" .= [t "group_id", t "workspace_id"]
      ]

  , mkTool "workspace_group_remove_member" "Remove a workspace from a group" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "group_id"     .= prop "string" "UUID of the workspace group"
          , "workspace_id" .= prop "string" "UUID of the workspace to remove"
          ]
      , "required" .= [t "group_id", t "workspace_id"]
      ]

  , mkTool "workspace_group_list_members" "List workspaces in a group" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "group_id" .= prop "string" "UUID of the workspace group" ]
      , "required" .= [t "group_id"]
      ]

  -- Activity timeline
  , mkTool "activity_timeline" "Get recent activity across the system" $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace (omit for all)"
          , "limit"        .= prop "integer" "Max events to return (default 50)"
          ]
      , "required" .= ([] :: [Text])
      ]

  -- Tags
  , mkTool "memory_get_tags" "Get the tags for a specific memory" $ object
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
  ]

------------------------------------------------------------------------
-- Typed tool calls
------------------------------------------------------------------------

data ToolCall
  = MemoryCreate   CreateMemory
  | MemoryCreateBatch [CreateMemory]
  | MemorySearch   SearchQuery
  | MemoryGet      UUID
  | MemoryUpdate   UUID UpdateMemory
  | MemoryDelete   UUID
  | LinkMemories   UUID CreateMemoryLink   -- source_id, link body
  | MemoryLinksList UUID
  | ProjectCreate  CreateProject
  | ProjectGet     UUID
  | ProjectUpdate  UUID UpdateProject
  | ProjectDelete  UUID
  | ProjectList    UUID (Maybe ProjectStatus) (Maybe Int) (Maybe Int)
  | ProjectLinkMem UUID UUID               -- project_id, memory_id
  | ProjectUnlinkMem UUID UUID             -- project_id, memory_id
  | TaskCreate     CreateTask
  | TaskGet        UUID
  | TaskDelete     UUID
  | TaskList       (Maybe UUID) (Maybe UUID) (Maybe TaskStatus) (Maybe Int) (Maybe Int)
  | TaskUpdate     UUID UpdateTask
  | TaskLinkMem    UUID UUID               -- task_id, memory_id
  | TaskUnlinkMem  UUID UUID               -- task_id, memory_id
  | TaskDepAdd     UUID UUID               -- task_id, depends_on_id
  | TaskDepRemove  UUID UUID               -- task_id, depends_on_id
  | WorkspaceList (Maybe Int) (Maybe Int)
  | WorkspaceGet   UUID
  | WsUpdate       UUID UpdateWorkspace
  | WsDelete       UUID
  | CategoryCreate CreateMemoryCategory
  | CategoryGet    UUID
  | CategoryList   (Maybe UUID) (Maybe Int) (Maybe Int) -- maybe workspace_id, limit, offset
  | CategoryUpdate UUID UpdateMemoryCategory
  | CategoryDelete UUID
  | CategoryLinkMem   UUID UUID            -- memory_id, category_id
  | CategoryUnlinkMem UUID UUID            -- memory_id, category_id
  | MemorySetTags   UUID [Text]            -- memory_id, tags
  | MemoryUnlink    UUID UUID RelationType -- source_id, target_id, relation_type
  | CleanupPoliciesList UUID (Maybe Int) (Maybe Int) -- workspace_id, limit, offset
  | CleanupPolicyUpsert UpsertCleanupPolicy
  | WorkspaceReg   CreateWorkspace
  | CleanupRun     UUID
  | MemoryList     (Maybe UUID) (Maybe MemoryType) (Maybe Int) (Maybe Int)
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
  | ActivityTimeline (Maybe UUID) (Maybe Int)
  | MemoryGetTags UUID
  | MemorySimilar SimilarQuery
  | MemorySetEmbedding UUID [Double]
  deriving (Show, Eq)

-- | Parse raw JSON-RPC params (containing "name" and "arguments") into
-- a typed ToolCall, validating all fields against HMem.Types.
parseToolCall :: Text -> Value -> Either String ToolCall
parseToolCall name args = case name of
  "memory_create"          -> MemoryCreate    <$> parse args
  "memory_create_batch"    -> MemoryCreateBatch <$> need "memories"
  "memory_search"          -> MemorySearch    <$> parse args
  "memory_get"             -> MemoryGet       <$> need "memory_id"
  "memory_update"          -> MemoryUpdate    <$> need "memory_id" <*> parse args
  "memory_delete"          -> MemoryDelete    <$> need "memory_id"
  "memory_link"            -> LinkMemories    <$> need "source_id" <*> parse args
  "memory_links_list"      -> MemoryLinksList <$> need "memory_id"
  "project_create"         -> ProjectCreate   <$> parse args
  "project_get"            -> ProjectGet      <$> need "project_id"
  "project_update"         -> ProjectUpdate   <$> need "project_id" <*> parse args
  "project_delete"         -> ProjectDelete   <$> need "project_id"
  "project_list"           -> ProjectList     <$> need "workspace_id" <*> opt "status" <*> opt "limit" <*> opt "offset"
  "project_link_memory"    -> ProjectLinkMem  <$> need "project_id" <*> need "memory_id"
  "project_unlink_memory"  -> ProjectUnlinkMem <$> need "project_id" <*> need "memory_id"
  "task_create"            -> TaskCreate      <$> parse args
  "task_get"               -> TaskGet         <$> need "task_id"
  "task_delete"            -> TaskDelete      <$> need "task_id"
  "task_list"              -> TaskList        <$> opt "workspace_id" <*> opt "project_id" <*> opt "status" <*> opt "limit" <*> opt "offset"
  "task_update"            -> TaskUpdate      <$> need "task_id" <*> parse args
  "task_link_memory"       -> TaskLinkMem     <$> need "task_id" <*> need "memory_id"
  "task_unlink_memory"     -> TaskUnlinkMem   <$> need "task_id" <*> need "memory_id"
  "task_dependency_add"    -> TaskDepAdd      <$> need "task_id" <*> need "depends_on_id"
  "task_dependency_remove" -> TaskDepRemove   <$> need "task_id" <*> need "depends_on_id"
  "category_create"        -> CategoryCreate  <$> parse args
  "category_get"           -> CategoryGet     <$> need "category_id"
  "category_list"          -> CategoryList    <$> opt "workspace_id" <*> opt "limit" <*> opt "offset"
  "category_update"        -> CategoryUpdate  <$> need "category_id" <*> parse args
  "category_delete"        -> CategoryDelete  <$> need "category_id"
  "category_link_memory"   -> CategoryLinkMem   <$> need "memory_id" <*> need "category_id"
  "category_unlink_memory" -> CategoryUnlinkMem <$> need "memory_id" <*> need "category_id"
  "memory_set_tags"        -> MemorySetTags <$> need "memory_id" <*> need "tags"
  "memory_unlink"          -> MemoryUnlink  <$> need "source_id" <*> need "target_id" <*> need "relation_type"
  "cleanup_policies_list"  -> CleanupPoliciesList <$> need "workspace_id" <*> opt "limit" <*> opt "offset"
  "cleanup_policy_upsert"  -> CleanupPolicyUpsert <$> parse args
  "workspace_list"         -> WorkspaceList    <$> opt "limit" <*> opt "offset"
  "workspace_get"          -> WorkspaceGet    <$> need "workspace_id"
  "workspace_update"       -> WsUpdate        <$> need "workspace_id" <*> parse args
  "workspace_delete"       -> WsDelete        <$> need "workspace_id"
  "workspace_register"     -> WorkspaceReg    <$> parse args
  "cleanup_run"            -> CleanupRun      <$> need "workspace_id"
  "memory_list"             -> MemoryList      <$> opt "workspace_id" <*> opt "memory_type" <*> opt "limit" <*> opt "offset"
  "memory_graph"            -> MemoryGraphCall <$> need "memory_id" <*> opt "depth"
  "memory_find_by_relation" -> MemoryFindByRelation <$> need "workspace_id" <*> need "relation_type"
  "memory_adjust_importance" -> MemoryAdjustImp <$> need "memory_id" <*> need "importance"
  "project_list_memories"   -> ProjectListMem  <$> need "project_id"
  "task_list_memories"      -> TaskListMem     <$> need "task_id"
  "memory_pin"              -> MemoryPin       <$> need "memory_id"
  "memory_unpin"            -> MemoryUnpin     <$> need "memory_id"
  "workspace_group_create"  -> WsGroupCreate   <$> parse args
  "workspace_group_get"     -> WsGroupGet      <$> need "group_id"
  "workspace_group_list"    -> WsGroupList     <$> opt "limit" <*> opt "offset"
  "workspace_group_delete"  -> WsGroupDelete   <$> need "group_id"
  "workspace_group_add_member"    -> WsGroupAddMem  <$> need "group_id" <*> need "workspace_id"
  "workspace_group_remove_member" -> WsGroupRmMem   <$> need "group_id" <*> need "workspace_id"
  "workspace_group_list_members"  -> WsGroupListMem <$> need "group_id"
  "activity_timeline"       -> ActivityTimeline <$> opt "workspace_id" <*> opt "limit"
  "memory_get_tags"          -> MemoryGetTags    <$> need "memory_id"
  "memory_similar"           -> MemorySimilar    <$> parse args
  "memory_set_embedding"     -> MemorySetEmbedding <$> need "memory_id" <*> need "embedding"
  _                        -> Left $ "Unknown tool: " <> T.unpack name
  where
    parse :: FromJSON a => Value -> Either String a
    parse = parseEither parseJSON

    need :: FromJSON a => Key -> Either String a
    need k = parseEither (withObject "args" (.: k)) args

    opt :: FromJSON a => Key -> Either String (Maybe a)
    opt k = parseEither (withObject "args" (.:? k)) args

------------------------------------------------------------------------
-- Tool call dispatch
------------------------------------------------------------------------

handleToolCall :: Manager -> String -> Value -> IO Value
handleToolCall mgr serverUrl params = case parseParams params of
  Left err -> pure $ mcpError (T.pack err)
  Right (name, args) -> case parseToolCall name args of
    Left err   -> pure $ mcpError (T.pack err)
    Right call -> case validateToolCall call of
      Left vErr -> pure $ mcpError (T.pack vErr)
      Right validCall -> executeToolCall mgr serverUrl validCall

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
    | otherwise -> Right $ MemoryCreate $ clampCreateMemory cm
  MemoryCreateBatch cms
    | length cms > 100 -> Left "memory_create_batch: max 100 items per batch"
    | any (\cm -> not (validFtsLanguage cm.ftsLanguage)) cms ->
        Left "memory_create_batch: one or more items have an invalid fts_language"
    | otherwise -> Right $ MemoryCreateBatch
      [ clampCreateMemory cm | cm <- cms ]
  MemorySearch sq
    | not (validFtsLanguage sq.searchLanguage) -> Left $ "Invalid search_language: " <> show sq.searchLanguage
    | otherwise -> Right $ MemorySearch sq
    { limit        = clampMaybe 1 200 <$> sq.limit
    , offset       = clampMaybe 0 10000 <$> sq.offset
    , minImportance = clampMaybe 1 10 <$> sq.minImportance
    }
  MemoryUpdate mid um -> Right $ MemoryUpdate mid $ clampUpdateMemory um
  MemoryAdjustImp mid imp -> Right $ MemoryAdjustImp mid (clamp 1 10 imp)
  MemoryGraphCall mid md -> Right $ MemoryGraphCall mid (clampMaybe 1 5 <$> md)
  MemoryList mws mt ml mo -> Right $ MemoryList mws mt (clampMaybe 1 200 <$> ml) (clampMaybe 0 10000 <$> mo)
  ProjectList wid ms ml mo -> Right $ ProjectList wid ms (clampMaybe 1 200 <$> ml) (clampMaybe 0 10000 <$> mo)
  TaskList mws mpid ms ml mo
    | Nothing <- mws, Nothing <- mpid -> Left "task_list requires at least workspace_id or project_id"
    | otherwise -> Right $ TaskList mws mpid ms (clampMaybe 1 200 <$> ml) (clampMaybe 0 10000 <$> mo)
  CategoryList mwid ml mo -> Right $ CategoryList mwid (clampMaybe 1 200 <$> ml) (clampMaybe 0 10000 <$> mo)
  WsGroupList ml mo -> Right $ WsGroupList (clampMaybe 1 200 <$> ml) (clampMaybe 0 10000 <$> mo)
  WorkspaceList ml mo -> Right $ WorkspaceList (clampMaybe 1 200 <$> ml) (clampMaybe 0 10000 <$> mo)
  CleanupPoliciesList wid ml mo -> Right $ CleanupPoliciesList wid (clampMaybe 1 200 <$> ml) (clampMaybe 0 10000 <$> mo)
  ActivityTimeline mws ml -> Right $ ActivityTimeline mws (clampMaybe 1 200 <$> ml)
  MemorySimilar sq -> Right $ MemorySimilar sq
    { limit = clampMaybe 1 200 <$> sq.limit
    , minSimilarity = clampMaybe 0.0 1.0 <$> sq.minSimilarity
    }
  MemorySetEmbedding mid vec
    | null vec  -> Left "memory_set_embedding: embedding must not be empty"
    | otherwise -> Right $ MemorySetEmbedding mid vec
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

clamp :: Ord a => a -> a -> a -> a
clamp lo hi = Prelude.max lo . Prelude.min hi

clampMaybe :: Ord a => a -> a -> a -> a
clampMaybe lo hi = clamp lo hi

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

-- | Execute a typed tool call against the hmem-server HTTP API.
executeToolCall :: Manager -> String -> ToolCall -> IO Value
executeToolCall mgr base = \case
  MemoryCreate cm     -> postJSON mgr base "/api/v1/memories" cm
  MemoryCreateBatch cms -> postJSON mgr base "/api/v1/memories/batch" cms
  MemorySearch sq     -> postJSON mgr base "/api/v1/memories/search?compact=true" sq
  MemoryGet mid       -> getJSON  mgr base ("/api/v1/memories/" <> uuidPath mid)
  MemoryUpdate mid um -> putJSON  mgr base ("/api/v1/memories/" <> uuidPath mid) um
  MemoryDelete mid    -> delJSON  mgr base ("/api/v1/memories/" <> uuidPath mid)
  LinkMemories sid cl -> postJSON mgr base ("/api/v1/memories/" <> uuidPath sid <> "/links") cl
  MemoryLinksList mid -> getJSON  mgr base ("/api/v1/memories/" <> uuidPath mid <> "/links")
  ProjectCreate cp    -> postJSON mgr base "/api/v1/projects" cp
  ProjectGet pid      -> getJSON  mgr base ("/api/v1/projects/" <> uuidPath pid)
  ProjectUpdate pid up -> putJSON mgr base ("/api/v1/projects/" <> uuidPath pid) up
  ProjectDelete pid   -> delJSON  mgr base ("/api/v1/projects/" <> uuidPath pid)
  ProjectList wid ms ml mo -> getJSON mgr base ("/api/v1/projects" <> buildQuery
                            [ ("workspace_id", Just $ uuidPath wid)
                            , ("status", encodeParam <$> ms)
                            , ("limit", show <$> ml)
                            , ("offset", show <$> mo)
                            ])
  ProjectLinkMem pid mid -> postJSON mgr base ("/api/v1/projects/" <> uuidPath pid <> "/memories")
                              (object ["memory_id" .= mid])
  ProjectUnlinkMem pid mid -> delJSON mgr base ("/api/v1/projects/" <> uuidPath pid <> "/memories/"
                                <> uuidPath mid)
  TaskCreate ct       -> postJSON mgr base "/api/v1/tasks" ct
  TaskGet tid         -> getJSON  mgr base ("/api/v1/tasks/" <> uuidPath tid)
  TaskDelete tid      -> delJSON  mgr base ("/api/v1/tasks/" <> uuidPath tid)
  TaskList mws mpid ms ml mo -> getJSON mgr base ("/api/v1/tasks" <> buildQuery
                            [ ("workspace_id", uuidPath <$> mws)
                            , ("project_id", uuidPath <$> mpid)
                            , ("status", encodeParam <$> ms)
                            , ("limit", show <$> ml)
                            , ("offset", show <$> mo)
                            ])
  TaskUpdate tid ut   -> putJSON  mgr base ("/api/v1/tasks/" <> uuidPath tid) ut
  TaskLinkMem tid mid -> postJSON mgr base ("/api/v1/tasks/" <> uuidPath tid <> "/memories")
                            (object ["memory_id" .= mid])
  TaskUnlinkMem tid mid -> delJSON mgr base ("/api/v1/tasks/" <> uuidPath tid <> "/memories/"
                              <> uuidPath mid)
  TaskDepAdd tid did  -> postJSON mgr base ("/api/v1/tasks/" <> uuidPath tid <> "/dependencies")
                            (object ["depends_on_id" .= did])
  TaskDepRemove tid did -> delJSON mgr base ("/api/v1/tasks/" <> uuidPath tid <> "/dependencies/"
                              <> uuidPath did)
  CategoryCreate cc   -> postJSON mgr base "/api/v1/categories" cc
  CategoryGet cid     -> getJSON  mgr base ("/api/v1/categories/" <> uuidPath cid)
  CategoryList mwid ml mo -> getJSON mgr base ("/api/v1/categories" <> buildQuery
                            [ ("workspace_id", uuidPath <$> mwid)
                            , ("limit", show <$> ml)
                            , ("offset", show <$> mo)
                            ])
  CategoryUpdate cid uc -> putJSON mgr base ("/api/v1/categories/" <> uuidPath cid) uc
  CategoryDelete cid  -> delJSON  mgr base ("/api/v1/categories/" <> uuidPath cid)
  CategoryLinkMem mid cid -> postJSON mgr base "/api/v1/categories/link"
                               (object ["memory_id" .= mid, "category_id" .= cid])
  CategoryUnlinkMem mid cid -> postJSON mgr base "/api/v1/categories/unlink"
                                  (object ["memory_id" .= mid, "category_id" .= cid])
  MemorySetTags mid tags -> putJSON mgr base ("/api/v1/memories/" <> uuidPath mid <> "/tags")
                              (toJSON tags)
  MemoryUnlink sid tid rt -> delJSON mgr base ("/api/v1/memories/" <> uuidPath sid <> "/links/"
                               <> uuidPath tid <> "/" <> T.unpack (relationTypeToText rt))
  CleanupPoliciesList wid ml mo -> getJSON mgr base ("/api/v1/cleanup/policies" <> buildQuery
                            [ ("workspace_id", Just $ uuidPath wid)
                            , ("limit", show <$> ml)
                            , ("offset", show <$> mo)
                            ])
  CleanupPolicyUpsert cp  -> postJSON mgr base "/api/v1/cleanup/policies" cp
  WorkspaceList ml mo      -> getJSON  mgr base ("/api/v1/workspaces" <> buildQuery
                            [ ("limit", show <$> ml)
                            , ("offset", show <$> mo)
                            ])
  WorkspaceGet wid        -> getJSON  mgr base ("/api/v1/workspaces/" <> uuidPath wid)
  WsUpdate wid uw         -> putJSON  mgr base ("/api/v1/workspaces/" <> uuidPath wid) uw
  WsDelete wid            -> delJSON  mgr base ("/api/v1/workspaces/" <> uuidPath wid)
  WorkspaceReg cw     -> postJSON mgr base "/api/v1/workspaces" cw
  CleanupRun wid      -> postJSON mgr base "/api/v1/cleanup/run"
                            (object ["workspace_id" .= wid])
  MemoryList mwid mt ml mo -> getJSON mgr base ("/api/v1/memories" <> buildQuery
                            [ ("workspace_id", uuidPath <$> mwid)
                            , ("type", encodeParam <$> mt)
                            , ("limit", show <$> ml)
                            , ("offset", show <$> mo)
                            , ("compact", Just "true")
                            ])
  MemoryGraphCall mid md -> getJSON mgr base ("/api/v1/memories/" <> uuidPath mid <> "/graph"
                            <> buildQuery [("depth", show <$> md)])
  MemoryFindByRelation wid rt -> getJSON mgr base ("/api/v1/memories/by-relation" <> buildQuery
                            [ ("workspace_id", Just $ uuidPath wid)
                            , ("relation_type", Just $ encodeParam rt)
                            ])
  MemoryAdjustImp mid imp -> putJSON mgr base ("/api/v1/memories/" <> uuidPath mid <> "/importance")
                              (object ["importance" .= imp])
  ProjectListMem pid -> getJSON mgr base ("/api/v1/projects/" <> uuidPath pid <> "/memories")
  TaskListMem tid -> getJSON mgr base ("/api/v1/tasks/" <> uuidPath tid <> "/memories")
  MemoryPin mid    -> postJSON mgr base ("/api/v1/memories/" <> uuidPath mid <> "/pin") (object [])
  MemoryUnpin mid  -> postJSON mgr base ("/api/v1/memories/" <> uuidPath mid <> "/unpin") (object [])
  WsGroupCreate cg -> postJSON mgr base "/api/v1/groups" cg
  WsGroupGet gid   -> getJSON  mgr base ("/api/v1/groups/" <> uuidPath gid)
  WsGroupList ml mo -> getJSON mgr base ("/api/v1/groups" <> buildQuery
                            [ ("limit", show <$> ml)
                            , ("offset", show <$> mo)
                            ])
  WsGroupDelete gid -> delJSON mgr base ("/api/v1/groups/" <> uuidPath gid)
  WsGroupAddMem gid wsId -> postJSON mgr base ("/api/v1/groups/" <> uuidPath gid <> "/members")
                              (object ["workspace_id" .= wsId])
  WsGroupRmMem gid wsId -> delJSON mgr base ("/api/v1/groups/" <> uuidPath gid <> "/members/"
                              <> uuidPath wsId)
  WsGroupListMem gid -> getJSON mgr base ("/api/v1/groups/" <> uuidPath gid <> "/members")
  ActivityTimeline mws ml -> getJSON mgr base ("/api/v1/activity" <> buildQuery
                            [ ("workspace_id", uuidPath <$> mws)
                            , ("limit", show <$> ml)
                            ])
  MemoryGetTags mid -> getJSON mgr base ("/api/v1/memories/" <> uuidPath mid <> "/tags")
  MemorySimilar sq -> postJSON mgr base "/api/v1/memories/similar" sq
  MemorySetEmbedding mid vec -> putJSON mgr base ("/api/v1/memories/" <> uuidPath mid <> "/embedding")
                                  (toJSON vec)

------------------------------------------------------------------------
-- Typed HTTP helpers
------------------------------------------------------------------------

postJSON :: ToJSON a => Manager -> String -> String -> a -> IO Value
postJSON mgr base path body = httpJSON mgr "POST" (base <> path) (Just (encode body))

getJSON :: Manager -> String -> String -> IO Value
getJSON mgr base path = httpJSON mgr "GET" (base <> path) Nothing

putJSON :: ToJSON a => Manager -> String -> String -> a -> IO Value
putJSON mgr base path body = httpJSON mgr "PUT" (base <> path) (Just (encode body))

delJSON :: Manager -> String -> String -> IO Value
delJSON mgr base path = httpJSON mgr "DELETE" (base <> path) Nothing

httpJSON :: Manager -> String -> String -> Maybe BL.ByteString -> IO Value
httpJSON mgr httpMethod url mbody = do
  result <- try $ do
    initReq <- parseRequest url
    let req = initReq
          { method         = fromString httpMethod
          , requestHeaders = [("Content-Type", "application/json")]
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
