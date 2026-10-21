module HMem.MCP.Tools
  ( toolDefinitions
  , handleToolCall
  , parseToolCall
  , validateToolCall
  , ToolCall(..)
  , compactObservationSummary
  , compactObservationDetail
  , compactObservationList
  , compactObservationMatches
  , compactSimilarObservations
  , compactSearchResults
  , compactProjectSummary
  , compactTaskSummary
  , mcpProvenanceHeadersFor
  , mcpResultWith
  , searchContextError
  ) where

import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Pair, parseEither)
import HMem.BotIdentity (BotIdentity)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (toList)
import Data.List (nub)
import Data.Maybe (catMaybes, fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDv4
import System.Environment (lookupEnv)
import Network.HTTP.Client
import Network.HTTP.Types (HeaderName)
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI (urlEncode)

import HMem.Types

-- The MCP surface deliberately mirrors the Observation-only public API.  The
-- workspace id is supplied by the MCP session context rather than advertised
-- on every scoped tool.
toolDefinitions :: [Value]
toolDefinitions =
  [ tool "set_workspace" "Declare workspace and bot identity before ordinary tools. Explicit null clears workspace and accepts identity; initialize preserves both. Identity lasts until process exit. Invalid declarations change neither value." (strictSchema ["workspace_id" .= nullableProp "Workspace UUID (explicit null clears workspace while accepting identity)", "identity" .= strictSchema ["role" .= prop "string" "Declared role (1..128 UTF-8 bytes)", "model-id" .= prop "string" "harness:model:reasoning (1..256 UTF-8 bytes)"] ["role", "model-id"]] ["workspace_id", "identity"])
  , tool "get_workspace" "Get the accepted workspace and declared bot identity; both are null before declaration." (schema [] [])
  , tool "workspace_list" "List registered workspaces." (schema ["limit" .= prop "integer" "Maximum results (default 50)"] [])
  , tool "workspace_register" "Register a workspace." (schema ["name" .= prop "string" "Workspace name", "workspace_type" .= enumProp "Workspace type" ["repository", "planning", "personal", "organization"]] ["name"])
  , tool "workspace_update" "Rename a workspace. Only its display name is mutable." (strictSchema ["workspace_id" .= prop "string" "Workspace UUID", "name" .= prop "string" "New workspace display name"] ["workspace_id", "name"])
  , tool "search" "Search stored workspace observations, projects, and tasks with all-term lexical matching; this does not search repository files. Omit query to browse; blank queries are invalid. Projects/tasks default to English stemming, observations use simple tokenization; OR and phrase syntax are unsupported. Defaults: all types, limit 10 per type (1..200), offset 0 (0..2147483647). Entity-specific filters affect only their type; ignored_filters lists supplied filters consumed by no requested type. project_id selects tasks directly in that project, excluding descendant projects. Results preserve compact summaries, full IDs and Observation provenance. Execute each next_requests entry to continue that type in the resolved workspace; has_more/next_offset remain available. Pages use the current result set, not a snapshot; continuation_limit reports offsets beyond the supported range. Use observation_match for concrete paths and detail/get tools for full content. Observations are notes about specific areas or files in a repository workspace; double-check them against the repository when used." (schema
      [ "workspace_id" .= prop "string" "Optional workspace UUID; explicit value overrides the active workspace, but still requires set_workspace identity declaration"
      , "query" .= prop "string" "Optional all-term lexical query; omit to browse; blank, OR and phrase syntax are not supported"
      , "entity_types" .= object ["type" .= ("array" :: Text), "minItems" .= (1 :: Int), "description" .= ("Nonempty entity selection; omitted means observation, project, task" :: Text), "items" .= object ["type" .= ("string" :: Text), "enum" .= (["observation", "project", "task"] :: [Text])]]
      , "subject_kind" .= enumProp "Exact kind of repository subject tied to observations" ["file", "glob"]
      , "subject" .= prop "string" "Exact repository-relative subject tied to observations"
      , "git_sha" .= prop "string" "Exact Git SHA where an Observation insight was established; use it to select potentially stale insights for re-audit"
      , "current_git_sha" .= prop "string" "Exact SHA bound to the current content assertion; legacy unknown rows do not match"
      , "history_git_sha" .= prop "string" "Exact SHA of any recorded assertion, including unbound legacy creation claims; returns current content once"
      , "project_status" .= enumProp "Project status" ["active", "paused", "completed", "archived"]
      , "task_status" .= enumProp "Task status" ["todo", "in_progress", "blocked", "done", "cancelled"]
      , "project_id" .= prop "string" "Exact direct project UUID for tasks only; excludes descendant projects and does not filter projects or observations"
      , "search_language" .= prop "string" "Optional PostgreSQL text-search configuration for projects and tasks only (default english); observations always use simple"
      , "task_priority" .= object ["type" .= ("integer" :: Text), "minimum" .= (1 :: Int), "maximum" .= (10 :: Int), "description" .= ("Exact priority for tasks only (1..10)" :: Text)]
      , "limit" .= prop "integer" "Maximum results per entity type (default 10, 1..200)"
      , "offset" .= prop "integer" "Result offset per entity type (0..2147483647)"
      ] [])
  , tool "observation_discovery" "Discover stored Observations using raw text through the authorized server. Create with filters (an empty object browses); content_query uses server-owned hybrid retrieval when available and content FTS with a safe reason otherwise. No query embeddings or provider setup are needed. Continue with context_token and presentation fields only. Contexts expire after five minutes without sliding expiry and belong to their workspace and audience. To change filters, create with optional replace_context_token; optional search_session/generation orders creations within a caller-owned search view. Never send filters, replacement, session or generation on continuation. Views: list, tree, count. Results preserve counts, tree, retrieval mode/reason, expiry and provenance with compact Observation previews. Execute next_request when present; count/final pages have none. Offset exhaustion is reported in continuation_error. A 409 discovery_restart_required requires an explicit fresh query; 429 discovery_capacity preserves the current query for retry. Use observation_get for full content; unified search remains lexical. Observations are notes about specific areas or files in a repository workspace; double-check them against the repository when used." (schema
      [ "workspace_id" .= prop "string" "Optional workspace UUID overriding the active workspace; still requires identity declaration"
      , "filters" .= schema
          [ "subject_query" .= prop "string" "Case-sensitive literal subject substring; ANDed with other filters on the same subject row (up to 4096 UTF-8 bytes)"
          , "content_query" .= prop "string" "Raw content query (up to 4096 UTF-8 bytes); server-owned hybrid retrieval or content FTS fallback; empty text adds no predicate"
          , "subject_kind" .= enumProp "Exact subject kind" ["file", "glob"]
          , "subject" .= prop "string" "Exact repository-relative subject"
          , "git_sha" .= prop "string" "Exact creation SHA; a staleness-audit sentinel"
          , "current_git_sha" .= prop "string" "Exact SHA bound to the current assertion"
          , "history_git_sha" .= prop "string" "Exact SHA of any recorded assertion; returns current content"
          ] []
      , "context_token" .= prop "string" "Returned context UUID for continuation; omit all create-only fields"
      , "replace_context_token" .= prop "string" "Context UUID to retire atomically with a new filter creation; never automatic on expiry"
      , "search_session" .= prop "string" "Optional caller-owned search-view UUID; create only, paired with generation"
      , "generation" .= object ["type" .= ("integer" :: Text), "minimum" .= (1 :: Int), "maximum" .= (2147483647 :: Int), "description" .= ("Increasing creation generation in search_session; create only" :: Text)]
      , "view" .= enumProp "Presentation (default list)" ["list", "tree", "count"]
      , "prefix" .= prop "string" "Tree branch only; empty/default selects the root"
      , "terminal" .= schema ["subject_kind" .= enumProp "Typed list-leaf subject kind" ["file", "glob"], "subject" .= prop "string" "Exact typed list-leaf subject; list view only"] ["subject_kind", "subject"]
      , "limit" .= object ["type" .= ("integer" :: Text), "minimum" .= (1 :: Int), "maximum" .= (200 :: Int), "description" .= ("Presentation rows per page (default 50)" :: Text)]
      , "offset" .= object ["type" .= ("integer" :: Text), "minimum" .= (0 :: Int), "maximum" .= maxUnifiedSearchOffset, "description" .= ("Presentation-row offset (default 0)" :: Text)]
      ] [])
  , tool "observation_create" "Create an Observation: a durable, non-obvious repository insight tied to one or more repository-relative file or glob subjects. git_sha records the repository state where the insight was established; use it as a sentinel to decide whether the insight needs re-audit, not as timeless proof. Pass subjects as an ordered array; they are OR alternatives and, with git_sha, immutable after creation. File subjects must be concrete paths. Glob subjects may use only *, ?, and ** path components (for example my/src/proj/**/*.java). Observations are notes about specific areas or files in a repository workspace; double-check them against the repository when used." (schema
      [ "subjects" .= object ["type" .= ("array" :: Text), "description" .= ("One to " <> T.pack (show maxObservationSubjects) <> " ordered repository-relative file or glob subjects tied to this durable insight; duplicate entries are removed in first-occurrence order"), "minItems" .= (1 :: Int), "maxItems" .= maxObservationSubjects, "items" .= object ["type" .= ("object" :: Text), "properties" .= object ["subject_kind" .= enumProp "Kind of repository subject tied to this durable insight" ["file", "glob"], "subject" .= prop "string" "Canonical repository-relative path or safe glob tied to this durable insight"], "required" .= (["subject_kind", "subject"] :: [Text])]]
      , "git_sha" .= prop "string" "Lowercase 40-character Git SHA for the repository state where this insight was established; a staleness-audit sentinel, not timeless proof"
      , "content" .= prop "string" "Durable, non-obvious note about specific areas or files in a repository workspace; double-check against the repository when used; not a progress update or routine fact"
      ] ["subjects", "git_sha", "content"])
  , tool "observation_get" "Get an observation by ID, including content and immutable provenance. Observations are notes about specific areas or files in a repository workspace; double-check them against the repository when used." (schema ["observation_id" .= prop "string" "Observation UUID"] ["observation_id"])
  , tool "observation_update" "Assert current content at reviewed_git_sha with the expected_content_version returned by a read. Repeated SHA/text assertions are accepted and advance version/history. Subjects and creation git_sha remain immutable; conflicts preserve latest canonical content. Observations are notes about specific areas or files in a repository workspace; double-check them against the repository when used." (strictSchema ["observation_id" .= prop "string" "Observation UUID", "content" .= prop "string" "Reviewed current content", "reviewed_git_sha" .= prop "string" "Lowercase 40-character SHA of the reviewed repository state", "expected_content_version" .= prop "string" "Opaque content_version UUID returned by a canonical read"] ["observation_id", "content", "reviewed_git_sha", "expected_content_version"])
  , tool "observation_history" "Get compact revision assertions in descending sequence order; historical claims never reconstruct previous content. Legacy creation claims have unknown content binding. Continue with next_offset when has_more is true." (schema ["observation_id" .= prop "string" "Observation UUID", "limit" .= prop "integer" "Maximum events (default 50, 1..200)", "offset" .= prop "integer" "Event offset (0..100000)"] ["observation_id"])
  , tool "observation_list" "List durable, non-obvious repository insights using exact subject and git_sha provenance filters and optional text search. git_sha is a sentinel for deciding when an insight needs re-audit, not timeless proof. When has_more is true, pass next_offset to retrieve the next page. Observations are notes about specific areas or files in a repository workspace; double-check them against the repository when used." (schema
      [ "subject_kind" .= enumProp "Exact kind of repository subject tied to observations" ["file", "glob"]
      , "subject" .= prop "string" "Exact repository-relative subject tied to observations"
      , "git_sha" .= prop "string" "Exact Git SHA where an Observation insight was established; use it to select potentially stale insights for re-audit"
      , "current_git_sha" .= prop "string" "Exact SHA bound to the current content assertion; legacy unknown rows do not match"
      , "history_git_sha" .= prop "string" "Exact SHA of any recorded assertion, including unbound legacy creation claims; returns current content once"
      , "query" .= prop "string" "Optional full-text query over durable repository insights"
      , "limit" .= prop "integer" "Maximum results (1-200)"
      , "offset" .= prop "integer" "Result offset"
      ] [])
  , tool "observation_match" "Find durable, non-obvious repository insights whose stored file subjects or safe glob subjects match any concrete repository-relative path supplied in paths. Paths are ORed; do not pass globs here and no repository filesystem is read. Optional filters compose with matching. git_sha is a staleness-audit sentinel, not timeless proof. Continue with next_offset until has_more is false. Observations are notes about specific areas or files in a repository workspace; double-check them against the repository when used." (schema
      [ "paths" .= object ["type" .= ("array" :: Text), "description" .= ("One to 256 concrete repository-relative files to match against stored Observation subjects; globs are rejected" :: Text), "minItems" .= (1 :: Int), "maxItems" .= (256 :: Int), "items" .= prop "string" "Concrete repository-relative file to match against Observation subjects"]
      , "subject_kind" .= enumProp "Filter matching repository subjects by kind" ["file", "glob"]
      , "git_sha" .= prop "string" "Exact Git SHA where an Observation insight was established; use it to select potentially stale insights for re-audit"
      , "current_git_sha" .= prop "string" "Exact SHA bound to the current content assertion; legacy unknown rows do not match"
      , "history_git_sha" .= prop "string" "Exact SHA of any recorded assertion, including unbound legacy creation claims; returns current content once"
      , "query" .= prop "string" "Optional full-text query over durable repository insights"
      , "limit" .= prop "integer" "Maximum results (1-200)"
      , "offset" .= prop "integer" "Result offset"
      ] ["paths"])
  , tool "observation_delete" "Delete an observation by ID." (schema ["observation_id" .= prop "string" "Observation UUID"] ["observation_id"])
  , tool "observation_set_embedding" "Set the exact 1536-dimension embedding for an observation." (schema
       [ "observation_id" .= prop "string" "Observation UUID"
       , "embedding" .= object ["type" .= ("array" :: Text), "description" .= ("Exactly 1536 finite numeric dimensions" :: Text), "minItems" .= (observationEmbeddingDimensions :: Int), "maxItems" .= (observationEmbeddingDimensions :: Int), "items" .= object ["type" .= ("number" :: Text)]]
       , "space_fingerprint" .= spaceFingerprintProp
      ] ["observation_id", "embedding"])
  , tool "observation_similar" "Find semantically similar durable, non-obvious repository insights. Subject and git_sha filters are exact provenance filters; git_sha is a staleness-audit sentinel, not timeless proof. To continue, add returned_count to offset and repeat until returned_count is less than limit or zero. Observations are notes about specific areas or files in a repository workspace; double-check them against the repository when used." (schema
      [ "subject_kind" .= enumProp "Exact kind of repository subject tied to observations" ["file", "glob"]
      , "subject" .= prop "string" "Exact repository-relative subject tied to observations"
      , "git_sha" .= prop "string" "Exact Git SHA where an Observation insight was established; use it to select potentially stale insights for re-audit"
      , "current_git_sha" .= prop "string" "Exact SHA bound to the current content assertion; legacy unknown rows do not match"
      , "history_git_sha" .= prop "string" "Exact SHA of any recorded assertion, including unbound legacy creation claims; returns current content once"
       , "embedding" .= object ["type" .= ("array" :: Text), "description" .= ("Exactly 1536 finite numeric dimensions" :: Text), "minItems" .= (observationEmbeddingDimensions :: Int), "maxItems" .= (observationEmbeddingDimensions :: Int), "items" .= object ["type" .= ("number" :: Text)]]
       , "space_fingerprint" .= spaceFingerprintProp
      , "min_similarity" .= prop "number" "Minimum similarity from 0 through 1"
      , "limit" .= prop "integer" "Maximum results (1-200)"
      , "offset" .= prop "integer" "Result offset"
      ] ["embedding"])
  , tool "project_create" "Create a project grouping tasks and subprojects in a hierarchy for long-running or short-lived work. Its description is a concise durable specification of goals and domain, excluding information already encoded in hmem's dependency graph; status records execution state." (schema ["name" .= prop "string" "Project name", "description" .= prop "string" "Optional concise durable project specification: goals and domain; omit information already encoded in hmem's dependency graph; not a log of progress or updates", "parent_id" .= prop "string" "Optional parent project UUID for hierarchy", "priority" .= prop "integer" "Priority 1 through 10"] ["name"])
  , tool "project_update" "Update a project's durable specification, hierarchy, or execution state. Supply project_id plus only fields to change; omitted fields stay unchanged, including description and parent_id, so do not copy them from a prior read. Explicit null clears only clearable fields. Descriptions are concise durable specifications of goals and domain, excluding information already encoded in hmem's dependency graph; not logs of progress or updates. Pausing requires no running descendant tasks or subtasks and cascades inherited pause/blocking. Resuming returns eligible automatically blocked tasks to todo and retains independent or ancestor pauses; acknowledgements include bounded pause effects. Completing requires closed descendants. Archiving also archives all descendant projects, including completed projects, and cancels non-done tasks while preserving done tasks." (schema ["project_id" .= prop "string" "Project UUID; the only required field", "name" .= prop "string" "Project name", "description" .= nullableProp "Concise durable project specification: goals and domain; omit information already encoded in hmem's dependency graph; omit this field to keep the existing description or use null to clear it; not a log of progress or updates", "parent_id" .= nullableProp "Parent project UUID for hierarchy; omit to keep the existing parent or use null to clear it", "status" .= enumProp "Execution state; record progress here, not in the description" ["active", "paused", "completed", "archived"], "priority" .= prop "integer" "Priority"] ["project_id"])
  , tool "project_detail" "Get project details, including its complete description." (schema ["project_id" .= prop "string" "Project UUID"] ["project_id"])
  , tool "project_overview" "Get a compact project overview with tasks and subprojects." (schema ["project_id" .= prop "string" "Project UUID"] ["project_id"])
  , tool "project_next_tasks" "Get actionable tasks for a project subtree." (schema ["project_id" .= prop "string" "Project UUID", "limit" .= prop "integer" "Maximum candidates", "include_blocked" .= prop "boolean" "Include blocked candidates"] ["project_id"])
  , tool "project_spec" "Atomically create a project and 1 to 50 initial atomic tasks in one request. Validation, authorization, or database rejection creates nothing. If the connection or response fails, the outcome is uncertain; check the workspace's projects before retrying. Returns the project ID and task IDs in input order. Projects group tasks and subprojects hierarchically for long-running or short-lived work. Tasks generally deliver a complete unit of work on its own; complex work can use subtasks as a directed checklist. Descriptions are concise durable specifications: project goals and domain, task steps and completion criteria, excluding information already encoded in hmem's dependency graph. Status records execution state. Create later-discovered atomic work as subtasks." (schema
      [ "name" .= prop "string" "Project name"
      , "description" .= prop "string" "Optional concise durable project specification: goals and domain; omit information already encoded in hmem's dependency graph; not a log of progress or updates"
      , "priority" .= prop "integer" "Project priority"
      , "tasks" .= object
          [ "type" .= ("array" :: Text)
          , "description" .= ("One to 50 initial tasks, generally complete units deliverable on their own, created in input order; use subtasks as a directed checklist for complex work and later-discovered atomic work, not a parent description" :: Text)
          , "minItems" .= (1 :: Int)
          , "maxItems" .= (50 :: Int)
          , "items" .= object
              [ "type" .= ("object" :: Text)
              , "properties" .= object
                  [ "title" .= prop "string" "Title for a complete unit of work generally deliverable on its own"
                  , "description" .= prop "string" "Concise durable task specification: steps and completion criteria; omit information already encoded in hmem's dependency graph; not a log of progress or updates"
                  , "priority" .= prop "integer" "Task priority"
                  ]
              , "required" .= (["title"] :: [Text])
              ]
          ]
      ] ["name", "tasks"])
  , tool "project_archive" "Archive a project and all descendant projects, including completed projects, and cancel non-done tasks in that subtree. Done tasks and dependency links are preserved; unrelated dependent tasks are not cancelled. Reopening does not reopen descendants." (schema ["project_id" .= prop "string" "Project UUID"] ["project_id"])
  , tool "task_create" "Create a task, generally a complete unit of work deliverable on its own. Complex work can use subtasks as a directed checklist. Its description is a concise durable specification of steps and completion criteria, excluding information already encoded in hmem's dependency graph; status records execution state. Create later-discovered atomic work as subtasks. Reopen a cancelled or done parent before adding unfinished subtasks." (schema ["project_id" .= prop "string" "Optional project UUID", "title" .= prop "string" "Title for a complete unit of work generally deliverable on its own", "description" .= prop "string" "Optional concise durable task specification: steps and completion criteria; omit information already encoded in hmem's dependency graph; not a log of progress or updates", "parent_id" .= prop "string" "Optional parent task UUID; subtasks can form a directed checklist for complex work or capture later-discovered atomic work", "priority" .= prop "integer" "Priority"] ["title"])
  , tool "task_update" "Update a task's durable specification, hierarchy, or execution state. Supply task_id plus only fields to change; omitted fields stay unchanged, including description, parent_id, and project_id, so do not copy them from a prior read. Explicit null clears only clearable fields. Descriptions are concise durable specifications of steps and completion criteria, excluding information already encoded in hmem's dependency graph; not logs of progress or updates. Use subtasks as a directed checklist for complex work; create later-discovered atomic work as subtasks. Cancelling also cancels non-done subtasks while preserving done subtasks. Reopen a cancelled parent before moving, reparenting, or reopening unfinished subtasks beneath it. Reopening does not reopen descendants." (schema ["task_id" .= prop "string" "Task UUID; the only required field", "title" .= prop "string" "Task title", "description" .= nullableProp "Concise durable task specification: steps and completion criteria; omit information already encoded in hmem's dependency graph; omit this field to keep the existing description or use null to clear it; not a log of progress or updates", "project_id" .= nullableProp "Project UUID; omit to keep the existing project or use null to clear it", "parent_id" .= nullableProp "Parent task UUID; subtasks can form a directed checklist for complex work or capture later-discovered atomic work; omit to keep the existing parent or use null to clear it", "status" .= enumProp "Execution state; record progress here, not in the description" ["todo", "in_progress", "blocked", "done", "cancelled"], "priority" .= prop "integer" "Priority"] ["task_id"])
  , tool "task_move_batch" "Atomically move one to 100 tasks and their active descendants to one project or no project. Submit every dependency-connected task that must move so no dependency edge crosses a project boundary." (strictSchema
      [ "task_ids" .= object
          [ "type" .= ("array" :: Text)
          , "description" .= ("Task UUIDs to move atomically; duplicates affect each task once" :: Text)
          , "minItems" .= (1 :: Int)
          , "maxItems" .= (100 :: Int)
          , "items" .= prop "string" "Task UUID"
          ]
      , "project_id" .= nullableProp "Destination project UUID, or null/omitted to detach the tasks from a project"
      ] ["task_ids"])
  , tool "task_detail" "Get task details, including its complete description." (schema ["task_id" .= prop "string" "Task UUID"] ["task_id"])
  , tool "task_overview" "Get a compact task overview and dependency summaries." (schema ["task_id" .= prop "string" "Task UUID"] ["task_id"])
  , tool "task_dependency" "Add or remove a prerequisite edge: task_id cannot proceed until depends_on_id is complete. Use dependencies for ordering, not logs of progress or updates." (schema ["task_id" .= prop "string" "Dependent task UUID", "depends_on_id" .= prop "string" "Prerequisite task UUID", "action" .= enumProp "Dependency mutation" ["add", "remove"]] ["task_id", "depends_on_id", "action"])
  , tool "task_start" "Set a task's execution state to in_progress. Preserve its description as a durable specification; status records progress." (schema ["task_id" .= prop "string" "Task UUID"] ["task_id"])
  , tool "task_finish" "Set a task's execution state to done, blocked, or cancelled. Status records progress; this does not create an observation. Done requires closing open descendants. Cancelled also cancels non-done subtasks while preserving done subtasks and dependency links; unrelated dependent tasks are not cancelled. Reopening does not reopen descendants." (schema ["task_id" .= prop "string" "Task UUID", "status" .= enumProp "Final execution state" ["done", "blocked", "cancelled"]] ["task_id", "status"])
  ]
  where
    tool name description inputSchema = object $
      ["name" .= (name :: Text), "description" .= (description :: Text), "inputSchema" .= inputSchema]
      <> ["outputSchema" .= searchOutputSchema | name == "search"]
      <> ["outputSchema" .= discoveryOutputSchema | name == "observation_discovery"]
    schema properties required = object ["type" .= ("object" :: Text), "properties" .= object properties, "required" .= (required :: [Text])]
    strictSchema properties required = object ["type" .= ("object" :: Text), "properties" .= object properties, "required" .= (required :: [Text]), "additionalProperties" .= False]
    prop typ description = object ["type" .= (typ :: Text), "description" .= (description :: Text)]
    spaceFingerprintProp = object
      [ "type" .= ("string" :: Text)
      , "description" .= ("Optional exact embedding space; omitted resolves to the legacy manual space" :: Text)
      , "minLength" .= (1 :: Int), "maxLength" .= (128 :: Int)
      , "pattern" .= ("^[^\\s\\x7f]+$" :: Text)
      ]
    nullableProp description = object ["description" .= (description :: Text), "anyOf" .= [object ["type" .= ("string" :: Text)], object ["type" .= ("null" :: Text)]]]
    enumProp description choices = object ["type" .= ("string" :: Text), "description" .= (description :: Text), "enum" .= (choices :: [Text])]
    arrayEnum description choices = object ["type" .= ("array" :: Text), "description" .= (description :: Text), "items" .= object ["type" .= ("string" :: Text), "enum" .= (choices :: [Text])]]

-- Search has an operation-specific structured contract. Core identity and
-- summary fields are required; optional hierarchy and legacy provenance values
-- retain the presence/nullability of the existing compact projections.
searchOutputSchema :: Value
searchOutputSchema = object
  [ "type" .= ("object" :: Text)
  , "anyOf" .= [success, failure]
  ]
  where
    success = outputObject
      [ "workspace_id" .= outputType "string"
      , "observations" .= outputArray observation
      , "projects" .= outputArray (planning "name" [])
      , "tasks" .= outputArray (planning "title" ["project_id" .= outputNullable (outputType "string")])
      , "has_more" .= typesMap (outputType "boolean")
      , "next_offset" .= typesMap (outputType "integer")
      , "next_requests" .= typesMap nextRequest
      , "ignored_filters" .= outputArray (outputType "string")
      ] ["workspace_id", "observations", "projects", "tasks", "has_more", "next_offset", "next_requests", "ignored_filters"]
    failure = structuredErrorOutputSchema
    planning label extra = outputObject
      (["id" .= outputType "string", Key.fromText label .= outputType "string", "status" .= outputType "string"
       , "priority" .= outputType "integer", "parent_id" .= outputNullable (outputType "string")] <> extra) ["id", label, "status", "priority"]
    observation = observationOutputSchema
    typesMap valueSchema = outputObject [Key.fromText key .= valueSchema | key <- ["observations", "projects", "tasks"]] []
    nextRequest = outputObject
      [ "name" .= object ["type" .= ("string" :: Text), "enum" .= (["search"] :: [Text])]
      , "arguments" .= outputObject
          ([Key.fromText key .= outputType "string" | key <- ["workspace_id", "query", "search_language", "subject_kind", "subject", "git_sha", "current_git_sha", "history_git_sha", "project_status", "task_status", "project_id"]]
           <> ["limit" .= outputType "integer", "offset" .= outputType "integer", "task_priority" .= outputType "integer"
              , "entity_types" .= object ["type" .= ("array" :: Text), "minItems" .= (1 :: Int), "maxItems" .= (1 :: Int), "items" .= object ["type" .= ("string" :: Text), "enum" .= (["observation", "project", "task"] :: [Text])]]])
          ["workspace_id", "limit", "offset", "entity_types"]
      ] ["name", "arguments"]

structuredErrorValueSchema :: Value
structuredErrorValueSchema = outputObject
  [ "code" .= outputType "string", "field" .= outputNullable (outputType "string")
  , "message" .= outputType "string", "http_status" .= outputType "integer"
  , "server_details" .= object [], "details_truncated" .= outputType "boolean"
  ] ["code", "field", "message"]

structuredErrorOutputSchema :: Value
structuredErrorOutputSchema = outputObject ["error" .= structuredErrorValueSchema] ["error"]

observationOutputSchema :: Value
observationOutputSchema = outputObject
  [ "id" .= outputType "string", "subjects" .= outputArray subjectOutputSchema
  , "subject_kind" .= outputType "string", "subject" .= outputType "string"
  , "git_sha" .= outputType "string", "content_preview" .= outputType "string"
  , "content_version" .= outputType "string", "latest_sequence" .= outputType "integer"
  , "current_provenance" .= outputNullable provenance
  , "provenance_match" .= outputObject [Key.fromText key .= outputType "string" | key <- ["original_git_sha", "current_git_sha", "history_git_sha"]] []
  ] ["id", "subjects", "git_sha", "content_preview", "content_version", "latest_sequence", "current_provenance"]
  where
    provenance = outputObject
      [ "sequence" .= outputType "integer", "event_kind" .= outputType "string", "reviewed_git_sha" .= outputType "string"
      , "content_version" .= outputNullable (outputType "string"), "content_digest" .= outputNullable (outputType "string")
      , "recorded_at" .= outputType "string", "actor_type" .= outputNullable (outputType "string")
      , "actor_id" .= outputNullable (outputType "string"), "actor_label" .= outputNullable (outputType "string")
      , "bot_identity" .= outputObject ["role" .= outputType "string", "model-id" .= outputType "string"] ["role", "model-id"]
      ] ["sequence", "event_kind", "reviewed_git_sha", "content_version", "content_digest", "recorded_at", "actor_type", "actor_id", "actor_label"]

subjectOutputSchema :: Value
subjectOutputSchema = outputObject ["subject_kind" .= outputType "string", "subject" .= outputType "string"] ["subject_kind", "subject"]

discoveryOutputSchema :: Value
discoveryOutputSchema = object ["type" .= ("object" :: Text), "anyOf" .= [success, structuredErrorOutputSchema]]
  where
    success = outputObject
      [ "workspace_id" .= outputType "string", "context_token" .= outputType "string", "expires_at" .= outputType "string"
      , "retrieval" .= outputObject ["mode" .= object ["type" .= ("string" :: Text), "enum" .= (["fts_only", "hybrid"] :: [Text])], "reason" .= outputNullable (outputType "string")] ["mode", "reason"]
      , "complete" .= outputType "boolean"
      , "page" .= outputObject
          [ "items" .= outputArray observationOutputSchema, "has_more" .= outputType "boolean"
          , "counts" .= outputObject ["workspace_id" .= outputType "string", "total_count" .= outputType "integer", "match_count" .= outputType "integer"] ["workspace_id", "total_count", "match_count"]
          , "tree" .= outputNullable tree
          ] ["items", "has_more", "counts"]
      , "next_request" .= outputObject
          [ "name" .= object ["type" .= ("string" :: Text), "enum" .= (["observation_discovery"] :: [Text])]
          , "arguments" .= outputObject
              [ "workspace_id" .= outputType "string", "context_token" .= outputType "string", "view" .= outputType "string"
              , "prefix" .= outputType "string", "terminal" .= subjectOutputSchema, "limit" .= outputType "integer", "offset" .= outputType "integer"
              ] ["workspace_id", "context_token", "view", "limit", "offset"]
          ] ["name", "arguments"]
      , "continuation_error" .= structuredErrorValueSchema
      ] ["workspace_id", "context_token", "expires_at", "retrieval", "complete", "page"]
    tree = outputObject
      [ "has_more" .= outputType "boolean", "total_count" .= outputType "integer", "match_count" .= outputType "integer"
      , "items" .= outputArray (outputObject
          [ "path" .= outputType "string", "segment" .= outputType "string", "has_children" .= outputType "boolean", "observation_count" .= outputType "integer"
          , "terminals" .= outputArray (outputObject ["subject_kind" .= outputType "string", "subject" .= outputType "string", "observation_count" .= outputType "integer"] ["subject_kind", "subject", "observation_count"])
          ] ["path", "segment", "has_children", "observation_count", "terminals"])
      ] ["items", "has_more", "total_count", "match_count"]

outputObject :: [Pair] -> [Text] -> Value
outputObject properties required = object
  ["type" .= ("object" :: Text), "properties" .= object properties, "required" .= required, "additionalProperties" .= False]

outputType :: Text -> Value
outputType typ = object ["type" .= typ]

outputArray :: Value -> Value
outputArray items = object ["type" .= ("array" :: Text), "items" .= items]

outputNullable :: Value -> Value
outputNullable value = object ["anyOf" .= [value, outputType "null"]]

-- Parsed calls retain current core DTOs; no compatibility aliases exist.
data ToolCall
  = ObservationCreate CreateObservation
  | ObservationGet UUID
  | ObservationUpdate UUID UUID ReviewedObservationUpdate
  | ObservationHistory UUID (Maybe Int) (Maybe Int)
  | ObservationList ObservationQuery
  | ObservationMatchCall ObservationMatchQuery
  | ObservationDiscoveryCall ObservationDiscoveryRequest
  | ObservationDelete UUID
  | ObservationSetEmbedding UUID ObservationEmbedding
  | ObservationSimilar SimilarObservationQuery
  | WorkspaceList (Maybe Int)
  | WorkspaceRegister CreateWorkspace
  | WorkspaceUpdate UUID UpdateWorkspace
  | UnifiedSearch UnifiedSearchQuery
  | ProjectCreate CreateProject
  | ProjectUpdate UUID UpdateProject
  | ProjectDetail UUID
  | ProjectOverviewCall UUID
  | ProjectNextTasks UUID (Maybe Int) Bool
  | ProjectSpec CreateProjectSpec
  | ProjectArchive UUID
  | TaskCreate CreateTask
  | TaskUpdate UUID UpdateTask
  -- The workspace context is injected by HMem.MCP.Server and deliberately is
  -- not part of BatchMoveTasksRequest, which remains the REST body.
  | TaskMoveBatch (Maybe UUID) BatchMoveTasksRequest
  | TaskDetail UUID
  | TaskOverviewCall UUID
  | TaskDependency UUID UUID Text
  | TaskStart UUID
  | TaskFinish UUID TaskStatus
  deriving (Show, Eq)

parseToolCall :: Text -> Value -> Either String ToolCall
parseToolCall name args = do
  if name `elem` ["project_create", "project_update", "project_spec", "project_archive", "task_create", "task_update", "task_move_batch", "task_dependency", "task_start", "task_finish"]
    then parseEither (withObject "planning arguments" rejectPlanningDueFields) args
    else pure ()
  case name of
    "observation_create" -> ObservationCreate <$> parseCreateObservation args
    "observation_get" -> ObservationGet <$> required "observation_id"
    "observation_update" -> ObservationUpdate <$> required "observation_id" <*> required "expected_content_version" <*> parseReviewedObservationUpdate args
    "observation_history" -> ObservationHistory <$> required "observation_id" <*> optional "limit" <*> optional "offset"
    "observation_list" -> ObservationList <$> parse args
    "observation_match" -> ObservationMatchCall <$> parse args
    "observation_discovery" -> ObservationDiscoveryCall <$> parse args
    "observation_delete" -> ObservationDelete <$> required "observation_id"
    "observation_set_embedding" -> ObservationSetEmbedding <$> required "observation_id" <*> (ObservationEmbedding <$> required "embedding" <*> optionalFingerprint args)
    "observation_similar" -> ObservationSimilar <$> parse args
    "workspace_list" -> WorkspaceList <$> optional "limit"
    "workspace_register" -> WorkspaceRegister <$> parse args
    "workspace_update" -> WorkspaceUpdate <$> required "workspace_id" <*> parseWorkspaceUpdate args
    "search" -> UnifiedSearch <$> parse args
    "project_create" -> ProjectCreate <$> parse args
    "project_update" -> ProjectUpdate <$> required "project_id" <*> parse args
    "project_detail" -> ProjectDetail <$> required "project_id"
    "project_overview" -> ProjectOverviewCall <$> required "project_id"
    "project_next_tasks" -> ProjectNextTasks <$> required "project_id" <*> optional "limit" <*> (fromMaybe False <$> optional "include_blocked")
    "project_spec" -> ProjectSpec <$> parse args
    "project_archive" -> ProjectArchive <$> required "project_id"
    "task_create" -> TaskCreate <$> parse args
    "task_update" -> TaskUpdate <$> required "task_id" <*> parse args
    "task_move_batch" -> uncurry TaskMoveBatch <$> parseBatchMoveTasks args
    "task_detail" -> TaskDetail <$> required "task_id"
    "task_overview" -> TaskOverviewCall <$> required "task_id"
    "task_dependency" -> TaskDependency <$> required "task_id" <*> required "depends_on_id" <*> required "action"
    "task_start" -> TaskStart <$> required "task_id"
    "task_finish" -> TaskFinish <$> required "task_id" <*> required "status"
    _ -> Left ("Unknown tool: " <> T.unpack name)
  where
    parse :: FromJSON a => Value -> Either String a
    parse = parseEither parseJSON
    required :: FromJSON a => Key.Key -> Either String a
    required key = parseEither (withObject "arguments" (.: key)) args
    optional :: FromJSON a => Key.Key -> Either String (Maybe a)
    optional key = parseEither (withObject "arguments" (.:? key)) args
    optionalFingerprint value = parseEither (withObject "arguments" $ \objectValue ->
      if KM.member "space_fingerprint" objectValue
        then Just <$> objectValue .: "space_fingerprint"
        else pure Nothing) value

parseReviewedObservationUpdate :: Value -> Either String ReviewedObservationUpdate
parseReviewedObservationUpdate = parseEither $ withObject "observation_update" $ \o -> do
  let unexpected = filter (`notElem` ["observation_id", "content", "reviewed_git_sha", "expected_content_version", "workspace_id"]) (Key.toText <$> KM.keys o)
  if null unexpected then ReviewedObservationUpdate <$> o .: "content" <*> o .: "reviewed_git_sha"
  else fail ("observation_update received server-owned or unexpected fields: " <> show unexpected)

parseWorkspaceUpdate :: Value -> Either String UpdateWorkspace
parseWorkspaceUpdate = parseEither $ withObject "workspace_update" $ \o -> do
  let unexpected = filter (`notElem` ["workspace_id", "name"]) (Key.toText <$> KM.keys o)
  if null unexpected then UpdateWorkspace <$> o .: "name"
  else fail ("workspace_update accepts only workspace_id and name; unexpected fields: " <> show unexpected)

parseBatchMoveTasks :: Value -> Either String (Maybe UUID, BatchMoveTasksRequest)
parseBatchMoveTasks = parseEither $ withObject "task_move_batch" $ \o -> do
  let unexpected = filter (`notElem` ["task_ids", "project_id", "workspace_id"]) (Key.toText <$> KM.keys o)
  if null unexpected then do
    injectedWorkspace <- case KM.lookup "workspace_id" o of
      Nothing -> pure Nothing
      Just (String workspaceText) -> case UUID.fromText workspaceText of
        Just workspace -> pure (Just workspace)
        Nothing -> fail "task_move_batch injected workspace_id must be a UUID"
      Just _ -> fail "task_move_batch injected workspace_id must be a UUID"
    batchRequest <- BatchMoveTasksRequest <$> o .: "task_ids" <*> o .:? "project_id"
    pure (injectedWorkspace, batchRequest)
  else fail ("task_move_batch accepts only task_ids and project_id; unexpected fields: " <> show unexpected)

-- | Core accepts the deprecated singleton form during the compatibility window.
-- The MCP registry advertises only @subjects@, but parsing retains the legacy
-- form so existing agents do not fail abruptly. Core parsing rejects mixed and
-- incomplete provenance forms before any HTTP request is made.
parseCreateObservation :: Value -> Either String CreateObservation
parseCreateObservation = parseEither $ withObject "observation_create" $ \o -> do
  let unexpected = filter (`notElem` ["workspace_id", "subjects", "subject_kind", "subject", "git_sha", "content"]) (Key.toText <$> KM.keys o)
  if null unexpected then parseJSON (Object o)
  else fail ("observation_create received unexpected fields: " <> show unexpected)

validateToolCall :: ToolCall -> Either String ToolCall
validateToolCall call = case call of
  ObservationCreate input -> checked (validateCreateObservationInput input) call
  ObservationUpdate _ _ input -> checked (validateReviewedObservationUpdate input) call
  ObservationHistory _ limit offset -> checked (validateObservationQuery (ObservationQuery UUID.nil Nothing Nothing Nothing Nothing limit offset Nothing Nothing)) call
  ObservationList input -> checked (validateObservationQuery input) call
  ObservationMatchCall input -> checked (validateObservationMatchQuery input) call
  ObservationDiscoveryCall input -> checked (validateObservationDiscoveryRequest input) call
  ObservationSetEmbedding _ (ObservationEmbedding values _) -> checked (validateEmbedding values) call
  ObservationSimilar input -> checked (validateSimilarObservationQuery input) call
  WorkspaceRegister input -> checked (validateCreateWorkspaceInput input) call
  -- The rename endpoint deliberately authorizes before decoding/validating its
  -- body so malformed names cannot disclose workspace existence to callers
  -- without edit access.  Forward name values unchanged and let that
  -- authoritative endpoint return its structured 400 response.
  WorkspaceUpdate _ _ -> Right call
  UnifiedSearch input -> checked (validateUnifiedSearchQuery input) call
  ProjectCreate input -> checked (validateCreateProjectInput input) call
  ProjectUpdate _ input -> checked (validateUpdateProjectInput input) call
  TaskCreate input -> checked (validateCreateTaskInput input) call
  TaskUpdate _ input -> checked (validateUpdateTaskInput input) call
  TaskMoveBatch _ input -> checked (validateBatchMoveTasksRequest input) call
  TaskDependency taskId dependsOnId action
    | taskId == dependsOnId -> Left "task_dependency: a task cannot depend on itself"
    | action `notElem` ["add", "remove"] -> Left "task_dependency: action must be add or remove"
    | otherwise -> Right call
  ProjectSpec input -> checked (validateCreateProjectSpecInput input) call
  WorkspaceList (Just n) | n < 1 || n > maxPaginationLimit -> Left "limit must be between 1 and 200"
  ProjectNextTasks _ (Just n) _ | n < 1 || n > maxPaginationLimit -> Left "limit must be between 1 and 200"
  _ -> Right call
  where
    checked errors value = case errors of [] -> Right value; _ -> Left (T.unpack (T.intercalate "; " errors))
    validateEmbedding values = ["embedding must contain exactly 1536 finite dimensions"
      | length values /= observationEmbeddingDimensions || any (\value -> isNaN value || isInfinite value) values]

data ForwardingContext = ForwardingContext (Maybe Text) BotIdentity

handleToolCall :: Manager -> String -> Maybe Text -> BotIdentity -> Value -> IO Value
handleToolCall manager base token identity params = case parseParams params of
  Left err -> pure (mcpError (T.pack err))
  Right (name, args) -> case parseToolCall name args of
    Left err -> pure (localError name "invalid_arguments" (T.pack err))
    Right parsed -> case validateToolCall parsed of
      Left err -> pure $ case parsed of
        UnifiedSearch input -> searchError "validation_error" (searchValidationField input (T.pack err)) (T.pack err) []
        ObservationDiscoveryCall input -> searchError "validation_error" (discoveryValidationField input (T.pack err)) (T.pack err) []
        _ -> mcpError (T.pack err)
      Right call -> execute manager base (ForwardingContext token identity) call
  where
    localError "search" code message = searchError code (searchErrorField message) message []
    localError "observation_discovery" code message = searchError code (discoveryErrorField message) message []
    localError _ _ message = mcpError message

parseParams :: Value -> Either String (Text, Value)
parseParams = parseEither $ withObject "params" $ \o -> (,) <$> o .: "name" <*> o .:? "arguments" .!= object []

execute :: Manager -> String -> ForwardingContext -> ToolCall -> IO Value
execute manager base apiKey = \case
  ObservationCreate input -> request manager base apiKey "POST" "/api/v1/observations" (Just (encode input)) (mutationAck "created" "observation" . compactObservationSummary)
  ObservationGet oid -> request manager base apiKey "GET" ("/api/v1/observations/" <> uuidPath oid) Nothing compactObservationDetail
  ObservationUpdate oid expected input -> requestWithHeaders manager base apiKey "PUT" ("/api/v1/observations/" <> uuidPath oid) (Just (encode input)) [("If-Match", TE.encodeUtf8 ("\"" <> UUID.toText expected <> "\""))] (mutationAck "updated" "observation" . compactObservationSummary)
  ObservationHistory oid limit offset -> request manager base apiKey "GET" ("/api/v1/observations/" <> uuidPath oid <> "/history" <> query [("limit", show <$> limit), ("offset", show <$> offset)]) Nothing (compactObservationHistory (fromMaybe 0 offset))
  ObservationList input -> request manager base apiKey "GET" (observationListPath input) Nothing (compactObservationList (fromMaybe 0 input.offset))
  ObservationMatchCall input -> request manager base apiKey "POST" "/api/v1/observations/match" (Just (encode input)) (compactObservationMatches (fromMaybe 0 input.offset))
  ObservationDiscoveryCall input -> do
    result <- rawRequestOutcome manager base apiKey "POST" "/api/v1/observations/discovery" (Just (encode (discoveryRequestBody input))) []
    pure $ either searchRequestError (mcpStructuredJSON . discoveryResult input) result
  ObservationDelete oid -> noContentRequest manager base apiKey "DELETE" ("/api/v1/observations/" <> uuidPath oid) Nothing (statusAck "deleted" "observation" oid)
  ObservationSetEmbedding oid embeddingValue -> noContentRequest manager base apiKey "PUT" ("/api/v1/observations/" <> uuidPath oid <> "/embedding") (Just (encode embeddingValue)) (statusAck "embedding_set" "observation" oid)
  ObservationSimilar input -> request manager base apiKey "POST" "/api/v1/observations/similar" (Just (encode input)) (compactSimilarObservations (fromMaybe 50 input.limit) (fromMaybe 0 input.offset))
  WorkspaceList limit -> request manager base apiKey "GET" ("/api/v1/workspaces" <> query [("limit", show <$> limit)]) Nothing compactWorkspaceList
  WorkspaceRegister input -> request manager base apiKey "POST" "/api/v1/workspaces" (Just (encode input)) (mutationAck "created" "workspace" . compactWorkspaceSummary)
  WorkspaceUpdate workspaceId input -> request manager base apiKey "PUT" ("/api/v1/workspaces/" <> uuidPath workspaceId) (Just (encode input)) (mutationAck "updated" "workspace" . compactWorkspaceSummary)
  UnifiedSearch input -> do
    result <- rawRequestOutcome manager base apiKey "POST" "/api/v1/search" (Just (encode input)) []
    pure $ either searchRequestError (mcpStructuredJSON . searchResult input) result
  ProjectCreate input -> request manager base apiKey "POST" "/api/v1/projects" (Just (encode input)) (mutationAck "created" "project" . compactProjectSummary)
  ProjectUpdate pid input -> request manager base apiKey "PUT" ("/api/v1/projects/" <> uuidPath pid) (Just (encode input)) (projectMutationAck "updated")
  ProjectDetail pid -> request manager base apiKey "GET" ("/api/v1/projects/" <> uuidPath pid) Nothing fullProjectDetail
  ProjectOverviewCall pid -> request manager base apiKey "GET" ("/api/v1/projects/" <> uuidPath pid <> "/overview") Nothing compactProjectOverview
  ProjectNextTasks pid limit includeBlocked -> request manager base apiKey "GET" ("/api/v1/projects/" <> uuidPath pid <> "/next-tasks" <> query [("limit", show <$> limit), ("include_blocked", if includeBlocked then Just "true" else Nothing)]) Nothing compactNextTasks
  ProjectSpec input -> executeProjectSpec manager base apiKey input
  ProjectArchive pid -> request manager base apiKey "PUT" ("/api/v1/projects/" <> uuidPath pid) (Just (encode (object ["status" .= ("archived" :: Text)]))) (projectMutationAck "archived")
  TaskCreate input -> request manager base apiKey "POST" "/api/v1/tasks" (Just (encode input)) (taskMutationAck "created")
  TaskUpdate tid input -> request manager base apiKey "PUT" ("/api/v1/tasks/" <> uuidPath tid) (Just (encode input)) (taskMutationAck "updated")
  TaskMoveBatch workspace input -> case workspace of
    Nothing -> dispatchBatchMove manager base apiKey input
    Just activeWorkspace -> do
      preflight <- validateBatchMoveWorkspace manager base apiKey activeWorkspace input
      case preflight of
        Left errorValue -> pure errorValue
        Right () -> dispatchBatchMove manager base apiKey input
  TaskDetail tid -> request manager base apiKey "GET" ("/api/v1/tasks/" <> uuidPath tid) Nothing fullTaskDetail
  TaskOverviewCall tid -> request manager base apiKey "GET" ("/api/v1/tasks/" <> uuidPath tid <> "/overview") Nothing compactTaskOverview
  TaskDependency tid depId "add" -> request manager base apiKey "POST" ("/api/v1/tasks/" <> uuidPath tid <> "/dependencies") (Just (encode (object ["depends_on_id" .= depId]))) compactTaskDependencyMutation
  TaskDependency tid depId "remove" -> request manager base apiKey "DELETE" ("/api/v1/tasks/" <> uuidPath tid <> "/dependencies/" <> uuidPath depId) Nothing compactTaskDependencyMutation
  TaskStart tid -> request manager base apiKey "PUT" ("/api/v1/tasks/" <> uuidPath tid) (Just (encode (object ["status" .= ("in_progress" :: Text)]))) (taskMutationAck "started")
  TaskFinish tid status -> request manager base apiKey "PUT" ("/api/v1/tasks/" <> uuidPath tid) (Just (encode (object ["status" .= status]))) (taskMutationAck "finished")

dispatchBatchMove :: Manager -> String -> ForwardingContext -> BatchMoveTasksRequest -> IO Value
dispatchBatchMove manager base apiKey input =
  request manager base apiKey "POST" "/api/v1/tasks/batch-move" (Just (encode input)) (compactTaskBatchMove input.projectId)

-- | A session workspace is an authorization boundary rather than a field in
-- the batch-move REST DTO.  Verify every explicitly supplied endpoint before
-- allowing the mutation so a stale or foreign task/project cannot be moved by
-- a contextual call.
validateBatchMoveWorkspace :: Manager -> String -> ForwardingContext -> UUID -> BatchMoveTasksRequest -> IO (Either Value ())
validateBatchMoveWorkspace manager base apiKey activeWorkspace input = do
  taskResults <- mapM (fetch "task" . ("/api/v1/tasks/" <>) . uuidPath) (nub input.taskIds)
  projectResult <- traverse (fetch "project" . ("/api/v1/projects/" <>) . uuidPath) input.projectId
  pure $ do
    mapM_ (>>= verifyWorkspace activeWorkspace) taskResults
    mapM_ (>>= verifyWorkspace activeWorkspace) projectResult
  where
    fetch entity path = do
      result <- rawRequest manager base apiKey "GET" path Nothing
      pure $ case result of
        Left errorValue -> Left errorValue
        Right value -> Right (entity, value)

verifyWorkspace :: UUID -> (Text, Value) -> Either Value ()
verifyWorkspace activeWorkspace (entity, value) = case textField "workspace_id" value >>= UUID.fromText of
  Just returnedWorkspace
    | returnedWorkspace == activeWorkspace -> Right ()
    | otherwise -> Left (mcpError ("task_move_batch " <> entity <> " workspace_id does not match the active workspace context"))
  Nothing -> Left (mcpError ("task_move_batch " <> entity <> " response omitted a valid workspace_id"))

executeProjectSpec :: Manager -> String -> ForwardingContext -> CreateProjectSpec -> IO Value
executeProjectSpec manager base apiKey input = do
  result <- rawRequest manager base apiKey "POST" "/api/v1/projects/spec" (Just (encode input))
  pure $ case result of
    Left errorValue -> errorValue
    Right response -> case parseEither parseJSON response of
      Left _ -> mcpError "Project spec response omitted valid project_id or task_ids; outcome is uncertain, so check the workspace's projects before retrying"
      Right (created :: ProjectSpecResult)
        | length created.taskIds /= length input.tasks -> mcpError "Project spec response returned an unexpected task count; outcome is uncertain, so check the workspace's projects before retrying"
        | otherwise -> mcpJSON (object
            [ "ok" .= True, "action" .= ("created" :: Text), "entity_type" .= ("project_spec" :: Text)
            , "project_id" .= created.projectId, "task_ids" .= created.taskIds ])

request :: Manager -> String -> ForwardingContext -> String -> String -> Maybe BL.ByteString -> (Value -> Value) -> IO Value
request manager base apiKey method path body shape = do
  result <- rawRequest manager base apiKey method path body
  pure $ either id (mcpJSON . shape) result

requestWithHeaders :: Manager -> String -> ForwardingContext -> String -> String -> Maybe BL.ByteString -> [(HeaderName, ByteString)] -> (Value -> Value) -> IO Value
requestWithHeaders manager base apiKey method path body headers shape = do
  result <- rawRequestWithHeaders manager base apiKey method path body headers
  pure $ either id (mcpJSON . shape) result

noContentRequest :: Manager -> String -> ForwardingContext -> String -> String -> Maybe BL.ByteString -> Value -> IO Value
noContentRequest manager base apiKey method path body acknowledgement = do
  outcome <- try $ do
    initial <- parseRequest (base <> path)
    provenance <- mcpProvenanceHeaders
    requestId <- UUIDv4.nextRandom
    let ForwardingContext credential identity = apiKey
        identityHeaders = [("X-HMem-Bot-Identity", BL.toStrict (encode identity))]
        auth = maybe [] (\token -> [("Authorization", TE.encodeUtf8 ("Bearer " <> token))]) credential
        requestValue = initial { method = fromString method, requestHeaders = ("Content-Type", "application/json") : ("X-Request-Id", TE.encodeUtf8 (UUID.toText requestId)) : provenance <> auth <> identityHeaders, requestBody = maybe (RequestBodyBS mempty) RequestBodyLBS body }
    response <- httpLbs requestValue manager
    if statusCode (responseStatus response) >= 200 && statusCode (responseStatus response) < 300
      then pure (Right ())
      else pure (Left (httpError (statusCode (responseStatus response)) (responseBody response)))
  case outcome of
    Right result -> pure $ either id (const (mcpJSON acknowledgement)) result
    Left (exception :: SomeException) -> do
      case fromException exception :: Maybe SomeAsyncException of Just _ -> throwIO exception; Nothing -> pure ()
      pure (mcpError "Could not connect to hmem-server")

rawRequest :: Manager -> String -> ForwardingContext -> String -> String -> Maybe BL.ByteString -> IO (Either Value Value)
rawRequest manager base apiKey method path body = rawRequestWithHeaders manager base apiKey method path body []

rawRequestWithHeaders :: Manager -> String -> ForwardingContext -> String -> String -> Maybe BL.ByteString -> [(HeaderName, ByteString)] -> IO (Either Value Value)
rawRequestWithHeaders manager base apiKey method path body extraHeaders = do
  result <- rawRequestOutcome manager base apiKey method path body extraHeaders
  pure $ case result of
    Right value -> Right value
    Left (InvalidResponse message) -> Left (mcpError message)
    Left (HttpFailure code responseBody) -> Left (httpError code responseBody)
    Left TransportFailure -> Left (mcpError "Could not connect to hmem-server")

data RequestFailure = InvalidResponse Text | HttpFailure Int BL.ByteString | TransportFailure

rawRequestOutcome :: Manager -> String -> ForwardingContext -> String -> String -> Maybe BL.ByteString -> [(HeaderName, ByteString)] -> IO (Either RequestFailure Value)
rawRequestOutcome manager base apiKey method path body extraHeaders = do
  outcome <- try $ do
    initial <- parseRequest (base <> path)
    provenance <- mcpProvenanceHeaders
    requestId <- UUIDv4.nextRandom
    let ForwardingContext credential identity = apiKey
        identityHeaders = [("X-HMem-Bot-Identity", BL.toStrict (encode identity))]
        auth = maybe [] (\token -> [("Authorization", TE.encodeUtf8 ("Bearer " <> token))]) credential
        requestValue = initial { method = fromString method, requestHeaders = ("Content-Type", "application/json") : ("X-Request-Id", TE.encodeUtf8 (UUID.toText requestId)) : provenance <> auth <> identityHeaders <> extraHeaders, requestBody = maybe (RequestBodyBS mempty) RequestBodyLBS body }
    response <- httpLbs requestValue manager
    if statusCode (responseStatus response) >= 200 && statusCode (responseStatus response) < 300
      then case eitherDecode (responseBody response) of
        Right value -> pure (Right value)
        Left err -> pure (Left (InvalidResponse ("Invalid JSON response: " <> T.pack err)))
      else pure (Left (HttpFailure (statusCode (responseStatus response)) (responseBody response)))
  case outcome of
    Right value -> pure value
    Left (exception :: SomeException) -> do
      case fromException exception :: Maybe SomeAsyncException of Just _ -> throwIO exception; Nothing -> pure ()
      pure (Left TransportFailure)

-- | Forwarded user authorization is not provenance.  The bridge adds this
-- independently configured private credential only when one is available.
mcpProvenanceHeaders :: IO [(HeaderName, ByteString)]
mcpProvenanceHeaders = mcpProvenanceHeadersFor <$> lookupEnv "HMEM_MCP_PROVENANCE_TOKEN"

mcpProvenanceHeadersFor :: Maybe String -> [(HeaderName, ByteString)]
mcpProvenanceHeadersFor configured =
  case configured >>= nonEmpty . T.strip . T.pack of
    Nothing -> [("X-HMem-Change-Cause", "mcp")]
    Just value -> [("X-HMem-Change-Cause", "mcp"), ("X-HMem-MCP-Provenance", TE.encodeUtf8 value)]
  where
    nonEmpty value
      | T.null value = Nothing
      | otherwise = Just value

compactObservationSummary :: Value -> Value
compactObservationSummary value = object (catMaybes [copy "id", copy "subjects", copy "subject_kind", copy "subject", copy "git_sha", copy "content_version", copy "latest_sequence", copy "current_provenance", copy "provenance_match", preview])
  where
    copy key = (Key.fromText key .=) <$> field key value
    preview = case field "content_preview" value of
      Just value' -> Just ("content_preview" .= value')
      Nothing -> case field "content" value of
        Just (String contentValue) -> Just ("content_preview" .= T.take 500 contentValue)
        _ -> Nothing

compactObservationDetail :: Value -> Value
compactObservationDetail value = object (catMaybes [copy "id", copy "subjects", copy "subject_kind", copy "subject", copy "git_sha", copy "content_version", copy "latest_sequence", copy "current_provenance", copy "provenance_match", copy "content"])
  where copy key = (Key.fromText key .=) <$> field key value

compactObservationList :: Int -> Value -> Value
compactObservationList offsetValue value = object
  ( [ "items" .= mapField "items" compactObservationSummary value
    , "has_more" .= hasMoreValue
    ] <> ["next_offset" .= (offsetValue + length (mapField "items" id value)) | hasMoreValue] )
  where
    hasMoreValue = field "has_more" value == Just (Bool True)

compactObservationHistory :: Int -> Value -> Value
compactObservationHistory offsetValue value = object
  ( [ "items" .= fromMaybe (toJSON ([] :: [Value])) (field "items" value)
    , "has_more" .= hasMoreValue
    ] <> ["next_offset" .= (offsetValue + length (mapField "items" id value)) | hasMoreValue] )
  where
    hasMoreValue = field "has_more" value == Just (Bool True)

compactObservationMatches :: Int -> Value -> Value
compactObservationMatches offsetValue value = object
  ( [ "items" .= rows
    , "has_more" .= hasMoreValue
    , "returned_count" .= returnedCount
    ] <> ["next_offset" .= (offsetValue + returnedCount) | hasMoreValue] )
  where
    sourceRows = mapField "items" id value
    returnedCount = length sourceRows
    rows = deduplicateObservationMatches sourceRows
    hasMoreValue = field "has_more" value == Just (Bool True)

deduplicateObservationMatches :: [Value] -> [Value]
deduplicateObservationMatches = go []
  where
    go _ [] = []
    go seen (row:rest) = case field "observation" row >>= field "id" of
      Just identifier | identifier `elem` seen -> go seen rest
      Just identifier -> compactMatch row : go (identifier : seen) rest
      Nothing -> compactMatch row : go seen rest
    compactMatch row = object (catMaybes
      [ ("observation" .=) . compactObservationSummary <$> field "observation" row
      , ("path_matches" .=) <$> field "path_matches" row
      , ("matched_paths" .=) <$> field "matched_paths" row
      , ("matched_subjects" .=) <$> field "matched_subjects" row
      ])

compactSimilarObservations :: Int -> Int -> Value -> Value
compactSimilarObservations limitValue offsetValue value = object
  [ "items" .= rows
  , "offset" .= offsetValue
  , "limit" .= limitValue
  , "returned_count" .= length rows
  ]
  where
    rows = case value of
      Array observations -> map compactSimilar (toList observations)
      _ -> []
    compactSimilar observationValue = object (catMaybes
      [ ("observation" .=) . compactObservationSummary <$> field "observation" observationValue
      , ("similarity" .=) <$> field "similarity" observationValue
      ])

compactProjectSummary :: Value -> Value
compactProjectSummary value = object (catMaybes [copy "id", copy "name", copy "status", copy "priority", copy "parent_id"])
  where copy key = (Key.fromText key .=) <$> field key value

fullProjectDetail :: Value -> Value
fullProjectDetail value = addDescription value (compactProjectSummary value)

compactTaskSummary :: Value -> Value
compactTaskSummary value = object (catMaybes [copy "id", copy "title", copy "status", copy "priority", copy "project_id", copy "parent_id"])
  where copy key = (Key.fromText key .=) <$> field key value

fullTaskDetail :: Value -> Value
fullTaskDetail value = addDescription value (compactTaskSummary value)

addDescription :: Value -> Value -> Value
addDescription source summary = case (field "description" source, summary) of
  (Just description, Object fields) -> Object (KM.insert "description" description fields)
  _ -> summary

compactTaskBatchMove :: Maybe UUID -> Value -> Value
compactTaskBatchMove destination value = object
  [ "ok" .= True
  , "action" .= ("moved" :: Text)
  , "entity_type" .= ("task" :: Text)
  , "affected" .= fromMaybe (Number 0) (field "affected" value)
  , "project_id" .= destination
  ]

compactSearchResults :: Value -> Value
compactSearchResults value = object
  [ "observations" .= mapField "observations" compactObservationSummary value
  , "projects" .= mapField "projects" compactProjectSummary value
  , "tasks" .= mapField "tasks" compactTaskSummary value
  , "has_more" .= fromMaybe (object []) (field "has_more" value)
  , "next_offset" .= fromMaybe (object []) (field "next_offset" value)
  ]

-- Continuations carry the resolved workspace even if the caller originally
-- relied on session injection. Filter ownership is shared with ignored_filters.
searchResult :: UnifiedSearchQuery -> Value -> Value
searchResult input value = case compactSearchResults value of
  Object result -> Object $ KM.insert "workspace_id" (toJSON input.workspaceId)
    $ KM.insert "ignored_filters" (toJSON ignored)
    $ KM.insert "next_requests" (object continuations) result
  other -> other
  where
    selected = fromMaybe [SearchObservation, SearchProject, SearchTask] input.entityTypes
    supplied = case toJSON input of Object fields -> fields; _ -> KM.empty
    ignored = [Key.toText key | (key, owners) <- searchFilterOwners, KM.member key supplied, not (any (`elem` selected) owners)]
    continuations =
      [ Key.fromText plural .= object ["name" .= ("search" :: Text), "arguments" .= object
          ([ "workspace_id" .= input.workspaceId, "entity_types" .= [entity]
           , "limit" .= fromMaybe 10 input.limit, "offset" .= next ]
           <> [key .= item | (key, item) <- KM.toList supplied, key == "query" || maybe False (entity `elem`) (lookup key searchFilterOwners)])]
      | (entity, plural) <- [(SearchObservation, "observations"), (SearchProject, "projects"), (SearchTask, "tasks")]
      , entity `elem` selected
      , Just next <- [field "next_offset" value >>= field plural]
      ]

searchFilterOwners :: [(Key.Key, [EntitySearchType])]
searchFilterOwners =
  [ ("current_git_sha", [SearchObservation]), ("git_sha", [SearchObservation]), ("history_git_sha", [SearchObservation])
  , ("project_id", [SearchTask]), ("project_status", [SearchProject])
  , ("search_language", [SearchProject, SearchTask]), ("subject", [SearchObservation]), ("subject_kind", [SearchObservation])
  , ("task_priority", [SearchTask]), ("task_status", [SearchTask])
  ]

-- Core owns creation/continuation validation. Its REST encoder emits nulls for
-- absent options; omit those at this boundary so continuations contain only
-- the context and presentation selectors, never create-only placeholders.
discoveryRequestBody :: ObservationDiscoveryRequest -> Value
discoveryRequestBody input = case toJSON input of
  Object fields -> Object (KM.filter (/= Null) fields)
  value -> value

discoveryResult :: ObservationDiscoveryRequest -> Value -> Value
discoveryResult input value = object $
  ["workspace_id" .= input.workspaceId]
  <> copies ["context_token", "expires_at", "complete"] value
  <> ["retrieval" .= object (copies ["mode", "reason"] (fromMaybe Null (field "retrieval" value)))
     ,"page" .= compactPage]
  <> continuation
  where
    copies keys source = catMaybes [fmap (Key.fromText key .=) (field key source) | key <- keys]
    page = fromMaybe Null (field "page" value)
    compactPage = object $ ["items" .= mapField "items" compactObservationSummary page]
      <> copies ["has_more", "counts", "tree"] page
    presented = case input.view of
      DiscoveryList -> page
      DiscoveryTree -> fromMaybe Null (field "tree" page)
      DiscoveryCount -> Null
    rows = case field "items" presented of Just (Array items) -> length items; _ -> 0
    next = toInteger (fromMaybe 0 input.offset) + toInteger rows
    continuation
      | field "has_more" presented /= Just (Bool True) = []
      | rows == 0 = ["continuation_error" .= object ["code" .= ("invalid_continuation" :: Text), "field" .= (if input.view == DiscoveryTree then "page.tree.items" else "page.items" :: Text), "message" .= ("The server reported more rows without returning a presentation row." :: Text)]]
      | next > toInteger maxUnifiedSearchOffset = ["continuation_error" .= object ["code" .= ("continuation_limit" :: Text), "field" .= ("offset" :: Text), "message" .= ("The page succeeded, but its next offset exceeds 2147483647." :: Text)]]
      | Just token <- textField "context_token" value =
          ["next_request" .= object ["name" .= ("observation_discovery" :: Text), "arguments" .= object
            (["workspace_id" .= input.workspaceId, "context_token" .= token, "view" .= input.view
             , "limit" .= fromMaybe 50 input.limit, "offset" .= next]
             <> ["prefix" .= input.treePrefix | not (T.null input.treePrefix)]
             <> maybe [] (\terminal -> ["terminal" .= terminal]) input.terminal)]]
      | otherwise = []

compactProjectOverview :: Value -> Value
compactProjectOverview value = object
  [ "project" .= maybe Null compactProjectSummary (field "project" value)
  , "tasks" .= mapField "tasks" compactTaskSummary value
  , "subprojects" .= mapField "subprojects" compactProjectSummary value
  , "readiness_rollup" .= fromMaybe (object []) (field "readiness_rollup" value)
  ]

compactTaskOverview :: Value -> Value
compactTaskOverview value = object
  [ "task" .= maybe Null compactTaskSummary (field "task" value)
  , "dependencies" .= fromMaybe (Array mempty) (field "dependencies" value)
  , "readiness_rollup" .= fromMaybe (object []) (field "readiness_rollup" value)
  ]

compactTaskDependencyMutation :: Value -> Value
compactTaskDependencyMutation value = object
  [ "ok" .= True
  , "action" .= fromMaybe Null (field "action" value)
  , "entity_type" .= ("task_dependency" :: Text)
  , "task_id" .= fromMaybe Null (field "task_id" value)
  , "depends_on_id" .= fromMaybe Null (field "depends_on_id" value)
  , "affected_tasks" .= mapField "affected_tasks" compactDependencyChange value
  ]

compactDependencyChange :: Value -> Value
compactDependencyChange change = object (catMaybes
  [ ("task" .=) . compactTaskSummary <$> field "task" change
  , copy "previous_status"
  , copy "current_status"
  , copy "previous_auto_blocked"
  , copy "auto_blocked"
  , copy "previous_open_dependency_count"
  , copy "open_dependency_count"
  , copy "previous_open_subtask_count"
  , copy "open_subtask_count"
  , copy "previous_project_paused"
  , copy "project_paused"
  , copy "reason"
  ])
  where copy key = (Key.fromText key .=) <$> field key change

compactNextTasks :: Value -> Value
compactNextTasks (Array rows) = toJSON (map compactRow (toList rows)) where
  compactRow row = case field "task" row of
    Just task -> object ["task" .= compactTaskSummary task, "completion_gated" .= fromMaybe (Bool False) (field "completion_gated" row), "open_descendant_count" .= fromMaybe (Number 0) (field "open_descendant_count" row), "project_paused" .= fromMaybe (Bool False) (field "project_paused" row), "dependency_blocked" .= fromMaybe (Bool False) (field "dependency_blocked" row), "open_dependency_count" .= fromMaybe (Number 0) (field "open_dependency_count" row)]
    Nothing -> compactTaskSummary row
compactNextTasks _ = toJSON ([] :: [Value])

compactWorkspaceSummary :: Value -> Value
compactWorkspaceSummary value = object (catMaybes [copy "id", copy "name", copy "workspace_type"])
  where copy key = (Key.fromText key .=) <$> field key value

compactWorkspaceList :: Value -> Value
compactWorkspaceList value = object ["items" .= mapField "items" compactWorkspaceSummary value, "has_more" .= fromMaybe (Bool False) (field "has_more" value)]

mutationAck :: Text -> Text -> Value -> Value
mutationAck action entity summary = object ( ["ok" .= True, "action" .= action, "entity_type" .= entity, "summary" .= summary] <> maybe [] (\identifier -> ["id" .= identifier]) (field "id" summary) <> maybe [] (\status -> ["status" .= status]) (field "status" summary) )

projectMutationAck :: Text -> Value -> Value
projectMutationAck action value = case mutationAck action "project" (compactProjectSummary value) of
  Object fields -> case field "pause_effects" value of
    Just effects -> Object (KM.insert "pause_effects" (case effects of
      Object values -> Object (KM.insert "tasks" (toJSON (mapField "tasks" compactDependencyChange effects)) values)
      other -> other) fields)
    Nothing -> Object fields
  other -> other

taskMutationAck :: Text -> Value -> Value
taskMutationAck action value = case mutationAck action "task" (compactTaskSummary value) of
  Object fields -> Object (KM.insert "dependency_effects" (toJSON (mapField "dependency_effects" compactDependencyChange value)) fields)
  other -> other

statusAck :: Text -> Text -> UUID -> Value
statusAck action entity identifier = object ["ok" .= True, "action" .= action, "entity_type" .= entity, "id" .= identifier]

field :: Text -> Value -> Maybe Value
field key (Object objectValue) = KM.lookup (Key.fromText key) objectValue
field _ _ = Nothing

textField :: Text -> Value -> Maybe Text
textField key value = case field key value of Just (String result) -> Just result; _ -> Nothing

mapField :: Text -> (Value -> Value) -> Value -> [Value]
mapField key shape value = case field key value of Just (Array rows) -> map shape (toList rows); _ -> []

query :: [(String, Maybe String)] -> String
query parameters = case [(key, value) | (key, Just value) <- parameters] of
  [] -> ""
  values -> "?" <> concat (zipWith (\index (key, value) -> (if index == 0 then "" else "&") <> key <> "=" <> value) [0 :: Int ..] values)

observationListPath :: ObservationQuery -> String
observationListPath (ObservationQuery workspace kind subjectValue sha queryValue limit offset currentSha historySha) =
  "/api/v1/observations" <> query
    [ ("workspace_id", Just (show workspace))
    , ("subject_kind", T.unpack . subjectKindToText <$> kind)
    , ("subject", queryText <$> subjectValue)
    , ("git_sha", queryText <$> sha)
    , ("current_git_sha", queryText <$> currentSha)
    , ("history_git_sha", queryText <$> historySha)
    , ("query", queryText <$> queryValue)
    , ("limit", show <$> limit)
    , ("offset", show <$> offset)
    ]

queryText :: Text -> String
queryText = T.unpack . TE.decodeUtf8 . urlEncode True . TE.encodeUtf8

uuidPath :: UUID -> String
uuidPath = show

mcpResultWith :: (Value -> Value) -> BL.ByteString -> Value
mcpResultWith shape body = case eitherDecode body of Right value -> mcpJSON (shape value); Left err -> mcpError ("Invalid JSON response: " <> T.pack err)

mcpJSON :: Value -> Value
mcpJSON value = object ["content" .= [object ["type" .= ("text" :: Text), "text" .= TE.decodeUtf8 (BL.toStrict (encode value))]]]

mcpStructuredJSON :: Value -> Value
mcpStructuredJSON value = case mcpJSON value of
  Object result -> Object (KM.insert "structuredContent" value result)
  result -> result

-- Keep the existing human-readable text on failures, with a bounded typed
-- envelope alongside it. Ordinary tools retain their existing wire shape.
searchError :: Text -> Maybe Text -> Text -> [Pair] -> Value
searchError code errorField message extra = object
  [ "content" .= [object ["type" .= ("text" :: Text), "text" .= message]]
  , "isError" .= True
  , "structuredContent" .= object ["error" .= object
      (["code" .= code, "field" .= errorField, "message" .= T.take 2000 message] <> extra)]
  ]

searchContextError :: Value
searchContextError = searchError "context_required" (Just "identity")
  "Call set_workspace with workspace_id and identity before using ordinary tools" []

searchErrorField :: Text -> Maybe Text
searchErrorField = retrievalErrorField
  ["workspace_id", "entity_types", "query", "search_language", "limit", "offset", "subject_kind", "subject", "current_git_sha", "history_git_sha", "git_sha", "project_status", "task_status", "task_priority", "project_id"]

-- Prefer the actual Aeson path over words in a diagnostic/value. Outside a
-- parser path, accept a single named validation prefix or missing-key report.
retrievalErrorField :: [Text] -> Text -> Maybe Text
retrievalErrorField keys message = case T.stripPrefix "Error in $" message of
  Just suffix | not (T.null (parserPath suffix)) ->
    let key = parserPath suffix
    in if key `elem` keys then Just key else Nothing
  _ -> case [key | key <- keys, (key <> " ") `T.isPrefixOf` message
                || ("invalid " <> key) `T.isPrefixOf` message
                || message == ("Error in $: key \"" <> key <> "\" not found")] of
    [key] -> Just key
    _ -> Nothing
  where
    parserPath = T.dropWhile (== '.')
      . T.takeWhile (\c -> c /= '[' && c /= ' ' && c /= '\n')
      . T.replace "\"]" "" . T.replace "[\"" "."
      . T.replace "']" "" . T.replace "['" "."
      . T.takeWhile (/= ':')

-- Core SHA validation intentionally shares one message across selectors. Use
-- the parsed input to avoid attributing a current/history failure to creation.
-- Multiple invalid selectors have no single truthful field attribution.
searchValidationField :: UnifiedSearchQuery -> Text -> Maybe Text
searchValidationField input message = case searchErrorField (T.takeWhile (/= ';') message) of
  Just "git_sha" -> case invalidSelectors of
    [selector] -> Just selector
    _ -> Nothing
  inferred -> inferred
  where
    invalidSelectors =
      [selector | (selector, Just sha) <- [("git_sha", input.gitSha), ("current_git_sha", input.currentGitSha), ("history_git_sha", input.historyGitSha)]
                , not (null (validateObservationQuery (ObservationQuery UUID.nil Nothing Nothing (Just sha) Nothing Nothing Nothing Nothing Nothing)))]

discoveryErrorField :: Text -> Maybe Text
discoveryErrorField = retrievalErrorField
  (["workspace_id", "replace_context_token", "context_token", "search_session", "generation", "filters", "view", "prefix", "terminal", "terminal.subject_kind", "terminal.subject", "limit", "offset"]
   <> map ("filters." <>) ["subject_query", "content_query", "subject_kind", "subject", "current_git_sha", "history_git_sha", "git_sha"])

discoveryValidationField :: ObservationDiscoveryRequest -> Text -> Maybe Text
discoveryValidationField input message
  | "git_sha" `T.isPrefixOf` firstIssue = case invalidSelectors of [selector] -> Just selector; _ -> Nothing
  | "subject_query" `T.isPrefixOf` firstIssue = Just "filters.subject_query"
  | "content_query" `T.isPrefixOf` firstIssue = Just "filters.content_query"
  | "context continuations" `T.isPrefixOf` firstIssue = Just "context_token"
  | "search_session and generation" `T.isPrefixOf` firstIssue =
      if input.searchSession == Nothing then Just "search_session" else Just "generation"
  | otherwise = discoveryErrorField firstIssue
  where
    firstIssue = T.takeWhile (/= ';') message
    invalidSelectors = case input.filters of
      Nothing -> []
      Just filters ->
        [ "filters." <> selector
        | (selector, Just sha) <- [("git_sha", filters.gitSha), ("current_git_sha", filters.currentGitSha), ("history_git_sha", filters.historyGitSha)]
        , not (null (validateObservationQuery (ObservationQuery UUID.nil Nothing Nothing (Just sha) Nothing Nothing Nothing Nothing Nothing)))]

searchRequestError :: RequestFailure -> Value
searchRequestError (InvalidResponse message) = searchError "invalid_response" Nothing message []
searchRequestError TransportFailure = searchError "transport_error" Nothing "Could not connect to hmem-server" []
searchRequestError (HttpFailure status body) = searchError code errorField legacyText
  ["http_status" .= status, "server_details" .= details, "details_truncated" .= truncated]
  where
    decoded = eitherDecode body :: Either String Value
    payload = either (const Nothing) Just decoded
    code = fromMaybe ("HTTP_" <> T.pack (show status)) (payload >>= \value -> textField "code" value `orElse` textField "error" value)
    errorField = payload >>= textField "field"
    rawText = TE.decodeUtf8With (\_ _ -> Just '?') (BL.toStrict (BL.take 8192 body))
    truncated = BL.length body > 8192
    details = if truncated then String (T.take 4096 rawText) else fromMaybe (String rawText) payload
    message = T.take 2000 (fromMaybe rawText (payload >>= textField "message"))
    legacyText = "[HTTP_" <> T.pack (show status) <> "] " <> message
      <> maybe "" (const ("\n" <> TE.decodeUtf8 (BL.toStrict (encode details)))) payload
    orElse (Just value) _ = Just value
    orElse Nothing alternative = alternative

mcpError :: Text -> Value
mcpError message = object ["content" .= [object ["type" .= ("text" :: Text), "text" .= message]], "isError" .= True]

httpError :: Int -> BL.ByteString -> Value
httpError code body =
  mcpError ("[HTTP_" <> T.pack (show code) <> "] " <> structuredMessage <> structuredPayload)
  where
    fallback = T.take 1000 (TE.decodeUtf8With (\_ _ -> Just '?') (BL.toStrict body))
    decodedPayload = eitherDecode body :: Either String Value
    structuredMessage = case decodedPayload of
      Right value -> fromMaybe fallback (textField "message" value)
      Left _ -> fallback
    -- MCP tool results carry text content, so retain the server's complete
    -- structured error payload alongside the HTTP status instead of reducing
    -- a machine-readable REST error to an opaque message.
    structuredPayload = case decodedPayload of
      Right value -> "\n" <> TE.decodeUtf8 (BL.toStrict (encode value))
      Left _ -> ""
