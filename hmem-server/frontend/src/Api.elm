module Api exposing
    ( Workspace, Project, Task, Memory, MemoryLink
    , WorkspaceGroup
    , TaskDependencySummary, TaskOverview
    , LinkedMemorySummary, ProjectSearchResult, TaskSearchResult, UnifiedSearchResults
    , WorkspaceVisualization, VisualizationMemory, VisualizationProjectMemoryLink, VisualizationTaskMemoryLink, VisualizationTaskDependency
    , AuditAction(..), AuditLogEntry, RevertResult
    , PaginatedResult
    , MemoryType(..), ProjectStatus(..), TaskStatus(..), WorkspaceType(..)
    , ChangeEvent, ChangeType(..), EntityType(..)
    , fetchWorkspaces, fetchWorkspace, updateWorkspace
    , fetchProjects, fetchProject
    , fetchTasks, fetchTask
    , fetchMemories, fetchMemory
    , fetchMemoryLinks
    , fetchWorkspaceLinks
    , fetchProjectMemories, fetchTaskMemories
    , linkProjectMemory, unlinkProjectMemory
    , linkTaskMemory, unlinkTaskMemory
    , fetchTaskOverview
    , fetchVisualization
    , addTaskDependency, removeTaskDependency
    , searchMemories, unifiedSearch
    , createProject, createProjectWithParent
    , updateProject, deleteProject
    , createTask, createTaskWithParent
    , updateTask, deleteTask
    , createMemory, updateMemory, deleteMemory, setTags
    , fetchWorkspaceGroups, createWorkspaceGroup, deleteWorkspaceGroup
    , fetchGroupMembers, addGroupMember, removeGroupMember
    , fetchAuditLog, fetchEntityHistory, revertAuditEntry
    , decodeChangeEvent
    , workspaceDecoder, projectDecoder, taskDecoder, memoryDecoder, auditLogEntryDecoder
    , memoryTypeToString, memoryTypeFromString, projectStatusToString, taskStatusToString, workspaceTypeToString
    , auditActionToString, auditActionFromString
    , projectStatusFromString, taskStatusFromString
    , projectStatusOrder, taskStatusOrder
    , allProjectStatuses, allTaskStatuses, allMemoryTypes
    )

import Http
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Encode as E
import Time



-- ENTITY TYPES


type alias Workspace =
    { id : String
    , name : String
    , workspaceType : WorkspaceType
    , ghOwner : Maybe String
    , ghRepo : Maybe String
    , createdAt : String
    , updatedAt : String
    }


type alias Project =
    { id : String
    , workspaceId : String
    , parentId : Maybe String
    , name : String
    , description : Maybe String
    , status : ProjectStatus
    , priority : Int
    , createdAt : String
    , updatedAt : String
    }


type alias Task =
    { id : String
    , workspaceId : String
    , projectId : Maybe String
    , parentId : Maybe String
    , title : String
    , description : Maybe String
    , status : TaskStatus
    , priority : Int
    , dueAt : Maybe String
    , completedAt : Maybe String
    , dependencyCount : Int
    , memoryLinkCount : Int
    , createdAt : String
    , updatedAt : String
    }


type alias Memory =
    { id : String
    , workspaceId : String
    , content : String
    , summary : Maybe String
    , memoryType : MemoryType
    , importance : Int
    , pinned : Bool
    , tags : List String
    , createdAt : String
    , updatedAt : String
    }


type alias WorkspaceGroup =
    { id : String
    , name : String
    , description : Maybe String
    , createdAt : String
    , updatedAt : String
    }


type alias MemoryLink =
    { sourceId : String
    , targetId : String
    , relationType : String
    , strength : Float
    }


type alias TaskDependencySummary =
    { id : String
    , name : String
    }


type alias TaskOverview =
    { task : Task
    , dependencies : List TaskDependencySummary
    }


type alias LinkedMemorySummary =
    { id : String
    , summary : Maybe String
    , tags : List String
    , importance : Int
    }


type alias ProjectSearchResult =
    { project : Project
    , linkedMemories : List LinkedMemorySummary
    }


type alias TaskSearchResult =
    { task : Task
    , linkedMemories : List LinkedMemorySummary
    }


type alias UnifiedSearchResults =
    { memories : List Memory
    , projects : List ProjectSearchResult
    , tasks : List TaskSearchResult
    }


type alias PaginatedResult a =
    { items : List a
    , hasMore : Bool
    }


type AuditAction
    = AuditCreate
    | AuditUpdate
    | AuditDelete


type alias AuditLogEntry =
    { id : String
    , entityType : String
    , entityId : String
    , action : AuditAction
    , oldValues : Maybe D.Value
    , newValues : Maybe D.Value
    , requestId : Maybe String
    , changedAt : String
    }


type alias RevertResult =
    { auditEntry : AuditLogEntry
    , entity : Maybe D.Value
    }



-- ENUMS


type MemoryType
    = ShortTerm
    | LongTerm


type ProjectStatus
    = ProjActive
    | ProjPaused
    | ProjCompleted
    | ProjArchived


type TaskStatus
    = Todo
    | InProgress
    | Blocked
    | Done
    | Cancelled


type WorkspaceType
    = Repository
    | Planning
    | Personal
    | Organization



-- ENUM HELPERS


memoryTypeToString : MemoryType -> String
memoryTypeToString mt =
    case mt of
        ShortTerm ->
            "short_term"

        LongTerm ->
            "long_term"


projectStatusToString : ProjectStatus -> String
projectStatusToString ps =
    case ps of
        ProjActive ->
            "active"

        ProjPaused ->
            "paused"

        ProjCompleted ->
            "completed"

        ProjArchived ->
            "archived"


projectStatusFromString : String -> ProjectStatus
projectStatusFromString s =
    case s of
        "active" ->
            ProjActive

        "paused" ->
            ProjPaused

        "completed" ->
            ProjCompleted

        "archived" ->
            ProjArchived

        _ ->
            ProjActive


taskStatusToString : TaskStatus -> String
taskStatusToString ts =
    case ts of
        Todo ->
            "todo"

        InProgress ->
            "in_progress"

        Blocked ->
            "blocked"

        Done ->
            "done"

        Cancelled ->
            "cancelled"


taskStatusFromString : String -> TaskStatus
taskStatusFromString s =
    case s of
        "todo" ->
            Todo

        "in_progress" ->
            InProgress

        "blocked" ->
            Blocked

        "done" ->
            Done

        "cancelled" ->
            Cancelled

        _ ->
            Todo


workspaceTypeToString : WorkspaceType -> String
workspaceTypeToString wt =
    case wt of
        Repository ->
            "repository"

        Planning ->
            "planning"

        Personal ->
            "personal"

        Organization ->
            "organization"


memoryTypeFromString : String -> MemoryType
memoryTypeFromString s =
    case s of
        "long_term" ->
            LongTerm

        _ ->
            ShortTerm


allProjectStatuses : List ProjectStatus
allProjectStatuses =
    [ ProjActive, ProjPaused, ProjCompleted, ProjArchived ]


allTaskStatuses : List TaskStatus
allTaskStatuses =
    [ Todo, InProgress, Blocked, Done, Cancelled ]


allMemoryTypes : List MemoryType
allMemoryTypes =
    [ ShortTerm, LongTerm ]


{-| Sort order for project statuses: active first, archived last.
-}
projectStatusOrder : ProjectStatus -> Int
projectStatusOrder ps =
    case ps of
        ProjActive -> 0
        ProjPaused -> 1
        ProjCompleted -> 2
        ProjArchived -> 3


{-| Sort order for task statuses: in-progress first, cancelled last.
-}
taskStatusOrder : TaskStatus -> Int
taskStatusOrder ts =
    case ts of
        InProgress -> 0
        Todo -> 1
        Blocked -> 2
        Done -> 3
        Cancelled -> 4


auditActionToString : AuditAction -> String
auditActionToString a =
    case a of
        AuditCreate ->
            "create"

        AuditUpdate ->
            "update"

        AuditDelete ->
            "delete"


auditActionFromString : String -> Maybe AuditAction
auditActionFromString s =
    case s of
        "create" ->
            Just AuditCreate

        "update" ->
            Just AuditUpdate

        "delete" ->
            Just AuditDelete

        _ ->
            Nothing



-- DECODERS


workspaceDecoder : Decoder Workspace
workspaceDecoder =
    D.succeed Workspace
        |> required "id" D.string
        |> required "name" D.string
        |> required "workspace_type" workspaceTypeDecoder
        |> optional "gh_owner" (D.nullable D.string) Nothing
        |> optional "gh_repo" (D.nullable D.string) Nothing
        |> required "created_at" D.string
        |> required "updated_at" D.string


projectDecoder : Decoder Project
projectDecoder =
    D.succeed Project
        |> required "id" D.string
        |> required "workspace_id" D.string
        |> optional "parent_id" (D.nullable D.string) Nothing
        |> required "name" D.string
        |> optional "description" (D.nullable D.string) Nothing
        |> required "status" projectStatusDecoder
        |> required "priority" D.int
        |> required "created_at" D.string
        |> required "updated_at" D.string


taskDecoder : Decoder Task
taskDecoder =
    D.succeed Task
        |> required "id" D.string
        |> required "workspace_id" D.string
        |> optional "project_id" (D.nullable D.string) Nothing
        |> optional "parent_id" (D.nullable D.string) Nothing
        |> required "title" D.string
        |> optional "description" (D.nullable D.string) Nothing
        |> required "status" taskStatusDecoder
        |> required "priority" D.int
        |> optional "due_at" (D.nullable D.string) Nothing
        |> optional "completed_at" (D.nullable D.string) Nothing
        |> optional "dependency_count" D.int 0
        |> optional "memory_link_count" D.int 0
        |> required "created_at" D.string
        |> required "updated_at" D.string


memoryDecoder : Decoder Memory
memoryDecoder =
    D.succeed Memory
        |> required "id" D.string
        |> required "workspace_id" D.string
        |> required "content" D.string
        |> optional "summary" (D.nullable D.string) Nothing
        |> required "memory_type" memoryTypeDecoder
        |> required "importance" D.int
        |> required "pinned" D.bool
        |> required "tags" (D.list D.string)
        |> required "created_at" D.string
        |> required "updated_at" D.string


workspaceGroupDecoder : Decoder WorkspaceGroup
workspaceGroupDecoder =
    D.succeed WorkspaceGroup
        |> required "id" D.string
        |> required "name" D.string
        |> optional "description" (D.nullable D.string) Nothing
        |> required "created_at" D.string
        |> required "updated_at" D.string


memoryLinkDecoder : Decoder MemoryLink
memoryLinkDecoder =
    D.succeed MemoryLink
        |> required "source_id" D.string
        |> required "target_id" D.string
        |> required "relation_type" D.string
        |> required "strength" D.float


paginatedDecoder : Decoder a -> Decoder (PaginatedResult a)
paginatedDecoder itemDecoder =
    D.succeed PaginatedResult
        |> required "items" (D.list itemDecoder)
        |> required "has_more" D.bool


memoryTypeDecoder : Decoder MemoryType
memoryTypeDecoder =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "short_term" ->
                        D.succeed ShortTerm

                    "long_term" ->
                        D.succeed LongTerm

                    _ ->
                        D.fail ("Unknown memory type: " ++ s)
            )


projectStatusDecoder : Decoder ProjectStatus
projectStatusDecoder =
    D.string
        |> D.andThen
            (\s ->
                D.succeed (projectStatusFromString s)
            )


taskStatusDecoder : Decoder TaskStatus
taskStatusDecoder =
    D.string
        |> D.andThen
            (\s ->
                D.succeed (taskStatusFromString s)
            )


linkedMemorySummaryDecoder : Decoder LinkedMemorySummary
linkedMemorySummaryDecoder =
    D.succeed LinkedMemorySummary
        |> required "id" D.string
        |> optional "summary" (D.nullable D.string) Nothing
        |> optional "tags" (D.list D.string) []
        |> required "importance" D.int


projectSearchResultDecoder : Decoder ProjectSearchResult
projectSearchResultDecoder =
    D.succeed ProjectSearchResult
        |> required "project" projectDecoder
        |> optional "linked_memories" (D.list linkedMemorySummaryDecoder) []


taskSearchResultDecoder : Decoder TaskSearchResult
taskSearchResultDecoder =
    D.succeed TaskSearchResult
        |> required "task" taskDecoder
        |> optional "linked_memories" (D.list linkedMemorySummaryDecoder) []


unifiedSearchResultsDecoder : Decoder UnifiedSearchResults
unifiedSearchResultsDecoder =
    D.succeed UnifiedSearchResults
        |> optional "memories" (D.list memoryDecoder) []
        |> optional "projects" (D.list projectSearchResultDecoder) []
        |> optional "tasks" (D.list taskSearchResultDecoder) []


workspaceTypeDecoder : Decoder WorkspaceType
workspaceTypeDecoder =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "repository" ->
                        D.succeed Repository

                    "planning" ->
                        D.succeed Planning

                    "personal" ->
                        D.succeed Personal

                    "organization" ->
                        D.succeed Organization

                    _ ->
                        D.fail ("Unknown workspace type: " ++ s)
            )


auditActionDecoder : Decoder AuditAction
auditActionDecoder =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "create" ->
                        D.succeed AuditCreate

                    "update" ->
                        D.succeed AuditUpdate

                    "delete" ->
                        D.succeed AuditDelete

                    _ ->
                        D.fail ("Unknown audit action: " ++ s)
            )


auditLogEntryDecoder : Decoder AuditLogEntry
auditLogEntryDecoder =
    D.succeed AuditLogEntry
        |> required "id" D.string
        |> required "entity_type" D.string
        |> required "entity_id" D.string
        |> required "action" auditActionDecoder
        |> optional "old_values" (D.nullable D.value) Nothing
        |> optional "new_values" (D.nullable D.value) Nothing
        |> optional "request_id" (D.nullable D.string) Nothing
        |> required "changed_at" D.string


revertResultDecoder : Decoder RevertResult
revertResultDecoder =
    D.succeed RevertResult
        |> required "audit_entry" auditLogEntryDecoder
        |> optional "entity" (D.nullable D.value) Nothing



-- CHANGE EVENTS (WebSocket)


type alias ChangeEvent =
    { changeType : ChangeType
    , entityType : EntityType
    , entityId : String
    , timestamp : String
    , payload : Maybe D.Value
    }


type ChangeType
    = Created
    | Updated
    | Deleted


type EntityType
    = EWorkspace
    | EProject
    | ETask
    | EMemory
    | EMemoryLink
    | ECategory
    | EWorkspaceGroup
    | ESavedView
    | ETaskDependency
    | ECategoryLink
    | ETag
    | EOther String


decodeChangeEvent : String -> Maybe ChangeEvent
decodeChangeEvent json =
    D.decodeString changeEventDecoder json
        |> Result.toMaybe


changeEventDecoder : Decoder ChangeEvent
changeEventDecoder =
    D.succeed ChangeEvent
        |> required "type" changeTypeDecoder
        |> required "entity_type" entityTypeDecoder
        |> required "entity_id" D.string
        |> required "timestamp" D.string
        |> optional "data" (D.nullable D.value) Nothing


changeTypeDecoder : Decoder ChangeType
changeTypeDecoder =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "entity_created" ->
                        D.succeed Created

                    "entity_updated" ->
                        D.succeed Updated

                    "entity_deleted" ->
                        D.succeed Deleted

                    _ ->
                        D.fail ("Unknown change type: " ++ s)
            )


entityTypeDecoder : Decoder EntityType
entityTypeDecoder =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "workspace" ->
                        D.succeed EWorkspace

                    "project" ->
                        D.succeed EProject

                    "task" ->
                        D.succeed ETask

                    "memory" ->
                        D.succeed EMemory

                    "memory_link" ->
                        D.succeed EMemoryLink

                    "category" ->
                        D.succeed ECategory

                    "workspace_group" ->
                        D.succeed EWorkspaceGroup

                    "saved_view" ->
                        D.succeed ESavedView

                    "task_dependency" ->
                        D.succeed ETaskDependency

                    "category_link" ->
                        D.succeed ECategoryLink

                    "tag" ->
                        D.succeed ETag

                    _ ->
                        D.succeed (EOther s)
            )



-- HTTP REQUESTS


fetchWorkspaces : String -> (Result Http.Error (PaginatedResult Workspace) -> msg) -> Cmd msg
fetchWorkspaces apiUrl toMsg =
    Http.get
        { url = apiUrl ++ "/api/v1/workspaces?limit=200"
        , expect = Http.expectJson toMsg (paginatedDecoder workspaceDecoder)
        }


fetchWorkspace : String -> String -> (Result Http.Error Workspace -> msg) -> Cmd msg
fetchWorkspace apiUrl wsId toMsg =
    Http.get
        { url = apiUrl ++ "/api/v1/workspaces/" ++ wsId
        , expect = Http.expectJson toMsg workspaceDecoder
        }


updateWorkspace : String -> String -> List ( String, E.Value ) -> (Result Http.Error Workspace -> msg) -> Cmd msg
updateWorkspace apiUrl wsId fields toMsg =
    Http.request
        { method = "PUT"
        , headers = []
        , url = apiUrl ++ "/api/v1/workspaces/" ++ wsId
        , body = Http.jsonBody (E.object fields)
        , expect = Http.expectJson toMsg workspaceDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


fetchProjects : String -> String -> (Result Http.Error (PaginatedResult Project) -> msg) -> Cmd msg
fetchProjects apiUrl wsId toMsg =
    Http.get
        { url = apiUrl ++ "/api/v1/projects?workspace_id=" ++ wsId ++ "&limit=200"
        , expect = Http.expectJson toMsg (paginatedDecoder projectDecoder)
        }


fetchProject : String -> String -> (Result Http.Error Project -> msg) -> Cmd msg
fetchProject apiUrl projId toMsg =
    Http.get
        { url = apiUrl ++ "/api/v1/projects/" ++ projId
        , expect = Http.expectJson toMsg projectDecoder
        }


fetchTasks : String -> String -> (Result Http.Error (PaginatedResult Task) -> msg) -> Cmd msg
fetchTasks apiUrl wsId toMsg =
    Http.get
        { url = apiUrl ++ "/api/v1/tasks?workspace_id=" ++ wsId ++ "&limit=200"
        , expect = Http.expectJson toMsg (paginatedDecoder taskDecoder)
        }


fetchTask : String -> String -> (Result Http.Error Task -> msg) -> Cmd msg
fetchTask apiUrl taskId toMsg =
    Http.get
        { url = apiUrl ++ "/api/v1/tasks/" ++ taskId
        , expect = Http.expectJson toMsg taskDecoder
        }


fetchMemories : String -> String -> (Result Http.Error (PaginatedResult Memory) -> msg) -> Cmd msg
fetchMemories apiUrl wsId toMsg =
    Http.get
        { url = apiUrl ++ "/api/v1/memories?workspace_id=" ++ wsId ++ "&limit=200"
        , expect = Http.expectJson toMsg (paginatedDecoder memoryDecoder)
        }


fetchMemory : String -> String -> (Result Http.Error Memory -> msg) -> Cmd msg
fetchMemory apiUrl memId toMsg =
    Http.get
        { url = apiUrl ++ "/api/v1/memories/" ++ memId
        , expect = Http.expectJson toMsg memoryDecoder
        }


fetchMemoryLinks : String -> String -> (Result Http.Error (List MemoryLink) -> msg) -> Cmd msg
fetchMemoryLinks apiUrl memId toMsg =
    Http.get
        { url = apiUrl ++ "/api/v1/memories/" ++ memId ++ "/links"
        , expect = Http.expectJson toMsg (D.list memoryLinkDecoder)
        }


fetchWorkspaceLinks : String -> String -> (Result Http.Error (List MemoryLink) -> msg) -> Cmd msg
fetchWorkspaceLinks apiUrl wsId toMsg =
    Http.get
        { url = apiUrl ++ "/api/v1/memories/workspace-links?workspace_id=" ++ wsId
        , expect = Http.expectJson toMsg (D.list memoryLinkDecoder)
        }


searchMemories : String -> String -> Maybe String -> (Result Http.Error (List Memory) -> msg) -> Cmd msg
searchMemories apiUrl query mWorkspaceId toMsg =
    let
        body =
            E.object
                ([ ( "query", E.string query ) ]
                    ++ (case mWorkspaceId of
                            Just wsId ->
                                [ ( "workspace_id", E.string wsId ) ]

                            Nothing ->
                                []
                       )
                )
    in
    Http.post
        { url = apiUrl ++ "/api/v1/memories/search"
        , body = Http.jsonBody body
        , expect = Http.expectJson toMsg (D.list memoryDecoder)
        }


unifiedSearch : String -> String -> Maybe String -> (Result Http.Error UnifiedSearchResults -> msg) -> Cmd msg
unifiedSearch apiUrl query mWorkspaceId toMsg =
    let
        body =
            E.object
                ([ ( "query", E.string query ) ]
                    ++ (case mWorkspaceId of
                            Just wsId ->
                                [ ( "workspace_id", E.string wsId ) ]

                            Nothing ->
                                []
                       )
                )
    in
    Http.post
        { url = apiUrl ++ "/api/v1/search"
        , body = Http.jsonBody body
        , expect = Http.expectJson toMsg unifiedSearchResultsDecoder
        }



-- MUTATION REQUESTS


createProject : String -> String -> String -> (Result Http.Error Project -> msg) -> Cmd msg
createProject apiUrl wsId name toMsg =
    Http.post
        { url = apiUrl ++ "/api/v1/projects"
        , body =
            Http.jsonBody
                (E.object
                    [ ( "workspace_id", E.string wsId )
                    , ( "name", E.string name )
                    ]
                )
        , expect = Http.expectJson toMsg projectDecoder
        }


updateProject : String -> String -> List ( String, E.Value ) -> (Result Http.Error Project -> msg) -> Cmd msg
updateProject apiUrl projectId fields toMsg =
    Http.request
        { method = "PUT"
        , headers = []
        , url = apiUrl ++ "/api/v1/projects/" ++ projectId
        , body = Http.jsonBody (E.object fields)
        , expect = Http.expectJson toMsg projectDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


deleteProject : String -> String -> (Result Http.Error () -> msg) -> Cmd msg
deleteProject apiUrl projectId toMsg =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = apiUrl ++ "/api/v1/projects/" ++ projectId
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


createTask : String -> String -> Maybe String -> String -> (Result Http.Error Task -> msg) -> Cmd msg
createTask apiUrl wsId mProjectId title toMsg =
    Http.post
        { url = apiUrl ++ "/api/v1/tasks"
        , body =
            Http.jsonBody
                (E.object
                    ([ ( "workspace_id", E.string wsId )
                     , ( "title", E.string title )
                     ]
                        ++ (case mProjectId of
                                Just pid ->
                                    [ ( "project_id", E.string pid ) ]

                                Nothing ->
                                    []
                           )
                    )
                )
        , expect = Http.expectJson toMsg taskDecoder
        }


updateTask : String -> String -> List ( String, E.Value ) -> (Result Http.Error Task -> msg) -> Cmd msg
updateTask apiUrl taskId fields toMsg =
    Http.request
        { method = "PUT"
        , headers = []
        , url = apiUrl ++ "/api/v1/tasks/" ++ taskId
        , body = Http.jsonBody (E.object fields)
        , expect = Http.expectJson toMsg taskDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


deleteTask : String -> String -> (Result Http.Error () -> msg) -> Cmd msg
deleteTask apiUrl taskId toMsg =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = apiUrl ++ "/api/v1/tasks/" ++ taskId
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


createMemory : String -> String -> String -> MemoryType -> (Result Http.Error Memory -> msg) -> Cmd msg
createMemory apiUrl wsId content mtype toMsg =
    Http.post
        { url = apiUrl ++ "/api/v1/memories"
        , body =
            Http.jsonBody
                (E.object
                    [ ( "workspace_id", E.string wsId )
                    , ( "content", E.string content )
                    , ( "memory_type", E.string (memoryTypeToString mtype) )
                    ]
                )
        , expect = Http.expectJson toMsg memoryDecoder
        }


updateMemory : String -> String -> List ( String, E.Value ) -> (Result Http.Error Memory -> msg) -> Cmd msg
updateMemory apiUrl memId fields toMsg =
    Http.request
        { method = "PUT"
        , headers = []
        , url = apiUrl ++ "/api/v1/memories/" ++ memId
        , body = Http.jsonBody (E.object fields)
        , expect = Http.expectJson toMsg memoryDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


deleteMemory : String -> String -> (Result Http.Error () -> msg) -> Cmd msg
deleteMemory apiUrl memId toMsg =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = apiUrl ++ "/api/v1/memories/" ++ memId
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


setTags : String -> String -> List String -> (Result Http.Error () -> msg) -> Cmd msg
setTags apiUrl memId tags toMsg =
    Http.request
        { method = "PUT"
        , headers = []
        , url = apiUrl ++ "/api/v1/memories/" ++ memId ++ "/tags"
        , body = Http.jsonBody (E.list E.string tags)
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }



-- PROJECT/TASK MEMORY LINKS


fetchProjectMemories : String -> String -> (Result Http.Error (List Memory) -> msg) -> Cmd msg
fetchProjectMemories apiUrl projectId toMsg =
    Http.get
        { url = apiUrl ++ "/api/v1/projects/" ++ projectId ++ "/memories"
        , expect = Http.expectJson toMsg (D.list memoryDecoder)
        }


fetchTaskMemories : String -> String -> (Result Http.Error (List Memory) -> msg) -> Cmd msg
fetchTaskMemories apiUrl taskId toMsg =
    Http.get
        { url = apiUrl ++ "/api/v1/tasks/" ++ taskId ++ "/memories"
        , expect = Http.expectJson toMsg (D.list memoryDecoder)
        }


linkProjectMemory : String -> String -> String -> (Result Http.Error () -> msg) -> Cmd msg
linkProjectMemory apiUrl projectId memoryId toMsg =
    Http.post
        { url = apiUrl ++ "/api/v1/projects/" ++ projectId ++ "/memories"
        , body = Http.jsonBody (E.object [ ( "memory_id", E.string memoryId ) ])
        , expect = Http.expectWhatever toMsg
        }


unlinkProjectMemory : String -> String -> String -> (Result Http.Error () -> msg) -> Cmd msg
unlinkProjectMemory apiUrl projectId memoryId toMsg =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = apiUrl ++ "/api/v1/projects/" ++ projectId ++ "/memories/" ++ memoryId
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


linkTaskMemory : String -> String -> String -> (Result Http.Error () -> msg) -> Cmd msg
linkTaskMemory apiUrl taskId memoryId toMsg =
    Http.post
        { url = apiUrl ++ "/api/v1/tasks/" ++ taskId ++ "/memories"
        , body = Http.jsonBody (E.object [ ( "memory_id", E.string memoryId ) ])
        , expect = Http.expectWhatever toMsg
        }


unlinkTaskMemory : String -> String -> String -> (Result Http.Error () -> msg) -> Cmd msg
unlinkTaskMemory apiUrl taskId memoryId toMsg =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = apiUrl ++ "/api/v1/tasks/" ++ taskId ++ "/memories/" ++ memoryId
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


createProjectWithParent : String -> String -> String -> String -> (Result Http.Error Project -> msg) -> Cmd msg
createProjectWithParent apiUrl wsId parentId name toMsg =
    Http.post
        { url = apiUrl ++ "/api/v1/projects"
        , body =
            Http.jsonBody
                (E.object
                    [ ( "workspace_id", E.string wsId )
                    , ( "parent_id", E.string parentId )
                    , ( "name", E.string name )
                    ]
                )
        , expect = Http.expectJson toMsg projectDecoder
        }


createTaskWithParent : String -> String -> Maybe String -> String -> String -> (Result Http.Error Task -> msg) -> Cmd msg
createTaskWithParent apiUrl wsId mProjectId parentId title toMsg =
    Http.post
        { url = apiUrl ++ "/api/v1/tasks"
        , body =
            Http.jsonBody
                (E.object
                    ([ ( "workspace_id", E.string wsId )
                     , ( "parent_id", E.string parentId )
                     , ( "title", E.string title )
                     ]
                        ++ (case mProjectId of
                                Just pid ->
                                    [ ( "project_id", E.string pid ) ]

                                Nothing ->
                                    []
                           )
                    )
                )
        , expect = Http.expectJson toMsg taskDecoder
        }



-- TASK DEPENDENCIES


fetchTaskOverview : String -> String -> (Result Http.Error TaskOverview -> msg) -> Cmd msg
fetchTaskOverview apiUrl taskId toMsg =
    Http.get
        { url = apiUrl ++ "/api/v1/tasks/" ++ taskId ++ "/overview"
        , expect = Http.expectJson toMsg taskOverviewDecoder
        }


addTaskDependency : String -> String -> String -> (Result Http.Error () -> msg) -> Cmd msg
addTaskDependency apiUrl taskId dependsOnId toMsg =
    Http.post
        { url = apiUrl ++ "/api/v1/tasks/" ++ taskId ++ "/dependencies"
        , body = Http.jsonBody (E.object [ ( "depends_on_id", E.string dependsOnId ) ])
        , expect = Http.expectWhatever toMsg
        }


removeTaskDependency : String -> String -> String -> (Result Http.Error () -> msg) -> Cmd msg
removeTaskDependency apiUrl taskId dependsOnId toMsg =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = apiUrl ++ "/api/v1/tasks/" ++ taskId ++ "/dependencies/" ++ dependsOnId
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


taskDependencySummaryDecoder : Decoder TaskDependencySummary
taskDependencySummaryDecoder =
    D.succeed TaskDependencySummary
        |> required "id" D.string
        |> required "name" D.string


taskOverviewDecoder : Decoder TaskOverview
taskOverviewDecoder =
    D.succeed TaskOverview
        |> required "task" taskDecoder
        |> required "dependencies" (D.list taskDependencySummaryDecoder)



-- WORKSPACE VISUALIZATION


type alias VisualizationMemory =
    { id : String
    , summary : String
    , memoryType : MemoryType
    , importance : Int
    , pinned : Bool
    }


type alias VisualizationProjectMemoryLink =
    { projectId : String
    , memoryId : String
    }


type alias VisualizationTaskMemoryLink =
    { taskId : String
    , memoryId : String
    }


type alias VisualizationTaskDependency =
    { taskId : String
    , dependsOnId : String
    }


type alias WorkspaceVisualization =
    { projects : List Project
    , tasks : List Task
    , memories : List VisualizationMemory
    , projectMemoryLinks : List VisualizationProjectMemoryLink
    , taskMemoryLinks : List VisualizationTaskMemoryLink
    , taskDependencies : List VisualizationTaskDependency
    , memoryLinks : List MemoryLink
    }


visualizationMemoryDecoder : Decoder VisualizationMemory
visualizationMemoryDecoder =
    D.succeed VisualizationMemory
        |> required "id" D.string
        |> required "summary" D.string
        |> required "memory_type" memoryTypeDecoder
        |> required "importance" D.int
        |> required "pinned" D.bool


visualizationProjectMemoryLinkDecoder : Decoder VisualizationProjectMemoryLink
visualizationProjectMemoryLinkDecoder =
    D.succeed VisualizationProjectMemoryLink
        |> required "project_id" D.string
        |> required "memory_id" D.string


visualizationTaskMemoryLinkDecoder : Decoder VisualizationTaskMemoryLink
visualizationTaskMemoryLinkDecoder =
    D.succeed VisualizationTaskMemoryLink
        |> required "task_id" D.string
        |> required "memory_id" D.string


visualizationTaskDependencyDecoder : Decoder VisualizationTaskDependency
visualizationTaskDependencyDecoder =
    D.succeed VisualizationTaskDependency
        |> required "task_id" D.string
        |> required "depends_on_id" D.string


workspaceVisualizationDecoder : Decoder WorkspaceVisualization
workspaceVisualizationDecoder =
    D.succeed WorkspaceVisualization
        |> required "projects" (D.list projectDecoder)
        |> required "tasks" (D.list taskDecoder)
        |> required "memories" (D.list visualizationMemoryDecoder)
        |> required "project_memory_links" (D.list visualizationProjectMemoryLinkDecoder)
        |> required "task_memory_links" (D.list visualizationTaskMemoryLinkDecoder)
        |> required "task_dependencies" (D.list visualizationTaskDependencyDecoder)
        |> required "memory_links" (D.list memoryLinkDecoder)


fetchVisualization : String -> String -> (Result Http.Error WorkspaceVisualization -> msg) -> Cmd msg
fetchVisualization apiUrl wsId toMsg =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = apiUrl ++ "/api/v1/workspaces/" ++ wsId ++ "/visualization"
        , body = Http.jsonBody (E.object [])
        , expect = Http.expectJson toMsg workspaceVisualizationDecoder
        , timeout = Nothing
        , tracker = Nothing
        }



-- WORKSPACE GROUPS


fetchWorkspaceGroups : String -> (Result Http.Error (PaginatedResult WorkspaceGroup) -> msg) -> Cmd msg
fetchWorkspaceGroups apiUrl toMsg =
    Http.get
        { url = apiUrl ++ "/api/v1/groups?limit=200"
        , expect = Http.expectJson toMsg (paginatedDecoder workspaceGroupDecoder)
        }


createWorkspaceGroup : String -> String -> Maybe String -> (Result Http.Error WorkspaceGroup -> msg) -> Cmd msg
createWorkspaceGroup apiUrl name mDescription toMsg =
    Http.post
        { url = apiUrl ++ "/api/v1/groups"
        , body =
            Http.jsonBody
                (E.object
                    ([ ( "name", E.string name ) ]
                        ++ (case mDescription of
                                Just desc ->
                                    [ ( "description", E.string desc ) ]

                                Nothing ->
                                    []
                           )
                    )
                )
        , expect = Http.expectJson toMsg workspaceGroupDecoder
        }


deleteWorkspaceGroup : String -> String -> (Result Http.Error () -> msg) -> Cmd msg
deleteWorkspaceGroup apiUrl groupId toMsg =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = apiUrl ++ "/api/v1/groups/" ++ groupId
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


fetchGroupMembers : String -> String -> (Result Http.Error (List String) -> msg) -> Cmd msg
fetchGroupMembers apiUrl groupId toMsg =
    Http.get
        { url = apiUrl ++ "/api/v1/groups/" ++ groupId ++ "/members"
        , expect = Http.expectJson toMsg (D.list D.string)
        }


addGroupMember : String -> String -> String -> (Result Http.Error () -> msg) -> Cmd msg
addGroupMember apiUrl groupId workspaceId toMsg =
    Http.post
        { url = apiUrl ++ "/api/v1/groups/" ++ groupId ++ "/members"
        , body = Http.jsonBody (E.object [ ( "workspace_id", E.string workspaceId ) ])
        , expect = Http.expectWhatever toMsg
        }


removeGroupMember : String -> String -> String -> (Result Http.Error () -> msg) -> Cmd msg
removeGroupMember apiUrl groupId workspaceId toMsg =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = apiUrl ++ "/api/v1/groups/" ++ groupId ++ "/members/" ++ workspaceId
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }



-- AUDIT LOG


fetchAuditLog : String -> { entityType : Maybe String, entityId : Maybe String, action : Maybe String, since : Maybe String, until : Maybe String, limit : Maybe Int, offset : Maybe Int } -> (Result Http.Error (PaginatedResult AuditLogEntry) -> msg) -> Cmd msg
fetchAuditLog apiUrl filters toMsg =
    let
        params =
            List.filterMap identity
                [ Maybe.map (\v -> "entity_type=" ++ v) filters.entityType
                , Maybe.map (\v -> "entity_id=" ++ v) filters.entityId
                , Maybe.map (\v -> "action=" ++ v) filters.action
                , Maybe.map (\v -> "since=" ++ v ++ "T00:00:00Z") filters.since
                , Maybe.map (\v -> "until=" ++ v ++ "T23:59:59Z") filters.until
                , Maybe.map (\v -> "limit=" ++ String.fromInt v) filters.limit
                , Maybe.map (\v -> "offset=" ++ String.fromInt v) filters.offset
                ]

        queryString =
            case params of
                [] ->
                    ""

                _ ->
                    "?" ++ String.join "&" params
    in
    Http.get
        { url = apiUrl ++ "/api/v1/audit" ++ queryString
        , expect = Http.expectJson toMsg (paginatedDecoder auditLogEntryDecoder)
        }


fetchEntityHistory : String -> String -> String -> Maybe Int -> (Result Http.Error (PaginatedResult AuditLogEntry) -> msg) -> Cmd msg
fetchEntityHistory apiUrl entityType entityId mLimit toMsg =
    let
        limitParam =
            case mLimit of
                Just n ->
                    "&limit=" ++ String.fromInt n

                Nothing ->
                    ""
    in
    Http.get
        { url = apiUrl ++ "/api/v1/audit?entity_type=" ++ entityType ++ "&entity_id=" ++ entityId ++ limitParam
        , expect = Http.expectJson toMsg (paginatedDecoder auditLogEntryDecoder)
        }


revertAuditEntry : String -> String -> (Result Http.Error RevertResult -> msg) -> Cmd msg
revertAuditEntry apiUrl auditId toMsg =
    Http.post
        { url = apiUrl ++ "/api/v1/audit/" ++ auditId ++ "/revert"
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg revertResultDecoder
        }
