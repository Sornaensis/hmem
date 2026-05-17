module Api exposing
    ( Workspace, Project, Task, Memory, MemoryLink
    , WorkspaceGroup, WorkspaceMembership
    , TaskDependencySummary, TaskOverview
    , LinkedMemorySummary, ProjectSearchResult, TaskSearchResult, UnifiedSearchResults
    , WorkspaceVisualization, VisualizationMemory, VisualizationProjectMemoryLink, VisualizationTaskMemoryLink, VisualizationTaskDependency
    , AuditAction(..), AuditLogEntry, RevertResult
    , ApiError, apiErrorToUserMessage, decodeApiErrorBody, isLifecycleConflict
    , PaginatedResult
    , SessionContext, SessionPrincipal, SessionGlobalPermissions, SessionWorkspaceContext
    , MemoryType(..), ProjectStatus(..), TaskStatus(..), WorkspaceType(..)
    , ChangeEvent, ChangeType(..), EntityType(..)
    , fetchSessionContext, fetchWorkspaces, fetchWorkspace, createWorkspace, updateWorkspace, deleteWorkspace, purgeWorkspace
    , fetchWorkspaceMemberships, upsertWorkspaceMembership, deleteWorkspaceMembership
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
    , allProjectStatuses, allTaskStatuses, allMemoryTypes, allWorkspaceTypes
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


type alias WorkspaceMembership =
    { workspaceId : String
    , userId : String
    , role : String
    , grantedBy : Maybe String
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
    , workspaceId : Maybe String
    , entityType : String
    , entityId : String
    , action : AuditAction
    , oldValues : Maybe D.Value
    , newValues : Maybe D.Value
    , requestId : Maybe String
    , actorType : Maybe String
    , actorId : Maybe String
    , actorLabel : Maybe String
    , changedAt : String
    }


type ApiError
    = StructuredApiError StructuredErrorBody
    | TransportError Http.Error
    | DecodeError String


type alias StructuredErrorBody =
    { error : String
    , code : Maybe String
    , message : String
    , detail : Maybe ErrorDetail
    , hint : Maybe String
    , requiredAction : Maybe String
    }


type alias ErrorDetail =
    { blockerCount : Maybe Int
    , blockerIds : List String
    , openProjectCount : Maybe Int
    , openProjectIds : List String
    , openTaskCount : Maybe Int
    , openTaskIds : List String
    }


expectJsonWithApiError : Decoder a -> (Result ApiError a -> msg) -> Http.Expect msg
expectJsonWithApiError decoder toMsg =
    Http.expectStringResponse toMsg (decodeApiResponse decoder)


decodeApiResponse : Decoder a -> Http.Response String -> Result ApiError a
decodeApiResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (TransportError (Http.BadUrl url))

        Http.Timeout_ ->
            Err (TransportError Http.Timeout)

        Http.NetworkError_ ->
            Err (TransportError Http.NetworkError)

        Http.BadStatus_ metadata body ->
            Err (decodeApiErrorBody metadata.statusCode body)

        Http.GoodStatus_ _ body ->
            case D.decodeString decoder body of
                Ok value ->
                    Ok value

                Err err ->
                    Err (DecodeError (D.errorToString err))


decodeApiErrorBody : Int -> String -> ApiError
decodeApiErrorBody statusCode body =
    case D.decodeString structuredApiErrorDecoder body of
        Ok apiError ->
            StructuredApiError apiError

        Err _ ->
            TransportError (Http.BadStatus statusCode)


structuredApiErrorDecoder : Decoder StructuredErrorBody
structuredApiErrorDecoder =
    D.succeed StructuredErrorBody
        |> required "error" D.string
        |> optional "code" (D.nullable D.string) Nothing
        |> optional "message" D.string "Server rejected the request."
        |> optional "detail" (D.nullable errorDetailDecoder) Nothing
        |> optional "hint" (D.nullable D.string) Nothing
        |> optional "required_action" (D.nullable D.string) Nothing


errorDetailDecoder : Decoder ErrorDetail
errorDetailDecoder =
    D.succeed ErrorDetail
        |> optional "blocker_count" (D.nullable D.int) Nothing
        |> optional "blocker_ids" (D.list D.string) []
        |> optional "open_project_count" (D.nullable D.int) Nothing
        |> optional "open_project_ids" (D.list D.string) []
        |> optional "open_task_count" (D.nullable D.int) Nothing
        |> optional "open_task_ids" (D.list D.string) []


apiErrorToUserMessage : String -> ApiError -> String
apiErrorToUserMessage fallback err =
    case err of
        StructuredApiError apiError ->
            structuredErrorToUserMessage fallback apiError

        TransportError _ ->
            fallback

        DecodeError _ ->
            fallback


isLifecycleConflict : ApiError -> Bool
isLifecycleConflict err =
    case err of
        StructuredApiError apiError ->
            apiError.error == "lifecycle_conflict"

        _ ->
            False


structuredErrorToUserMessage : String -> StructuredErrorBody -> String
structuredErrorToUserMessage fallback apiError =
    let
        baseMessage =
            case apiError.code of
                Just "TASK_COMPLETION_BLOCKED" ->
                    "Finish or cancel all subtasks before marking this task done."

                Just "TASK_OPEN_UNDER_DONE_TASK" ->
                    "Reopen the parent task before adding or reopening open subtasks."

                Just "PROJECT_COMPLETION_BLOCKED" ->
                    "Complete/archive child projects and finish or cancel all tasks before closing this project."

                Just "PROJECT_OPEN_UNDER_CLOSED_PROJECT" ->
                    "Reopen the parent project before adding or reopening active child projects."

                Just "TASK_OPEN_UNDER_CLOSED_PROJECT" ->
                    "Reopen the project before adding or reopening open tasks."

                _ ->
                    apiError.requiredAction
                        |> Maybe.withDefault (Maybe.withDefault apiError.message apiError.hint)

        detailText =
            apiError.detail
                |> Maybe.map errorDetailSummary
                |> Maybe.withDefault ""
    in
    if String.isEmpty baseMessage then
        fallback ++ detailText

    else
        baseMessage ++ detailText


errorDetailSummary : ErrorDetail -> String
errorDetailSummary detail =
    let
        parts =
            List.filterMap identity
                [ Maybe.map (\n -> String.fromInt n ++ " blocker" ++ plural n) detail.blockerCount
                , Maybe.map (\n -> String.fromInt n ++ " open project" ++ plural n) detail.openProjectCount
                , Maybe.map (\n -> String.fromInt n ++ " open task" ++ plural n) detail.openTaskCount
                ]

        ids =
            detail.blockerIds ++ detail.openProjectIds ++ detail.openTaskIds

        idText =
            case List.take 2 ids of
                [] ->
                    ""

                examples ->
                    "; examples: " ++ String.join ", " (List.map shortId examples)
    in
    if List.isEmpty parts && String.isEmpty idText then
        ""

    else
        " (" ++ String.join ", " parts ++ idText ++ ")"


plural : Int -> String
plural count =
    if count == 1 then
        ""

    else
        "s"


shortId : String -> String
shortId idValue =
    String.left 8 idValue


type alias SessionContext =
    { authMode : String
    , principal : SessionPrincipal
    , globalPermissions : SessionGlobalPermissions
    , workspace : Maybe SessionWorkspaceContext
    }


type alias SessionPrincipal =
    { actorType : String
    , actorId : String
    , actorLabel : String
    , authority : String
    , grantUserId : Maybe String
    }


type alias SessionGlobalPermissions =
    { createWorkspace : Bool
    , superadmin : Bool
    }


type alias SessionWorkspaceContext =
    { workspaceId : String
    , role : Maybe String
    , canRead : Bool
    , canEdit : Bool
    , canAdmin : Bool
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


allWorkspaceTypes : List WorkspaceType
allWorkspaceTypes =
    [ Repository, Planning, Personal, Organization ]


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


workspaceMembershipDecoder : Decoder WorkspaceMembership
workspaceMembershipDecoder =
    D.succeed WorkspaceMembership
        |> required "workspace_id" D.string
        |> required "user_id" D.string
        |> required "role" D.string
        |> optional "granted_by" (D.nullable D.string) Nothing
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


sessionContextDecoder : Decoder SessionContext
sessionContextDecoder =
    D.succeed SessionContext
        |> required "auth_mode" D.string
        |> required "principal" sessionPrincipalDecoder
        |> required "global_permissions" sessionGlobalPermissionsDecoder
        |> optional "workspace" (D.nullable sessionWorkspaceContextDecoder) Nothing


sessionPrincipalDecoder : Decoder SessionPrincipal
sessionPrincipalDecoder =
    D.succeed SessionPrincipal
        |> required "actor_type" D.string
        |> required "actor_id" D.string
        |> required "actor_label" D.string
        |> required "authority" D.string
        |> optional "grant_user_id" (D.nullable D.string) Nothing


sessionGlobalPermissionsDecoder : Decoder SessionGlobalPermissions
sessionGlobalPermissionsDecoder =
    D.succeed SessionGlobalPermissions
        |> required "create_workspace" D.bool
        |> required "superadmin" D.bool


sessionWorkspaceContextDecoder : Decoder SessionWorkspaceContext
sessionWorkspaceContextDecoder =
    D.succeed SessionWorkspaceContext
        |> required "workspace_id" D.string
        |> optional "role" (D.nullable D.string) Nothing
        |> required "can_read" D.bool
        |> required "can_edit" D.bool
        |> required "can_admin" D.bool


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
        |> optional "workspace_id" (D.nullable D.string) Nothing
        |> required "entity_type" D.string
        |> required "entity_id" D.string
        |> required "action" auditActionDecoder
        |> optional "old_values" (D.nullable D.value) Nothing
        |> optional "new_values" (D.nullable D.value) Nothing
        |> optional "request_id" (D.nullable D.string) Nothing
        |> optional "actor_type" (D.nullable D.string) Nothing
        |> optional "actor_id" (D.nullable D.string) Nothing
        |> optional "actor_label" (D.nullable D.string) Nothing
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
    , workspaceId : Maybe String
    , timestamp : String
    , requestId : Maybe String
    , actorType : Maybe String
    , actorId : Maybe String
    , actorLabel : Maybe String
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
        |> optional "workspace_id" (D.nullable D.string) Nothing
        |> required "timestamp" D.string
        |> optional "request_id" (D.nullable D.string) Nothing
        |> optional "actor_type" (D.nullable D.string) Nothing
        |> optional "actor_id" (D.nullable D.string) Nothing
        |> optional "actor_label" (D.nullable D.string) Nothing
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


fetchSessionContext : String -> Maybe String -> (Result Http.Error SessionContext -> msg) -> Cmd msg
fetchSessionContext apiUrl maybeWorkspaceId toMsg =
    let
        suffix =
            case maybeWorkspaceId of
                Just wsId ->
                    "?workspace_id=" ++ wsId

                Nothing ->
                    ""
    in
    Http.get
        { url = apiUrl ++ "/api/v1/session" ++ suffix
        , expect = Http.expectJson toMsg sessionContextDecoder
        }


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


createWorkspace : String -> String -> WorkspaceType -> Maybe String -> Maybe String -> String -> (Result Http.Error Workspace -> msg) -> Cmd msg
createWorkspace apiUrl name workspaceType mGhOwner mGhRepo requestId toMsg =
    let
        optionalFields =
            [ ( "gh_owner", Maybe.map E.string mGhOwner )
            , ( "gh_repo", Maybe.map E.string mGhRepo )
            ]
                |> List.filterMap (\( key, mValue ) -> Maybe.map (\value -> ( key, value )) mValue)

        fields =
            [ ( "name", E.string name )
            , ( "workspace_type", E.string (workspaceTypeToString workspaceType) )
            , ( "request_id", E.string requestId )
            ]
                ++ optionalFields
    in
    Http.request
        { method = "POST"
        , headers = requestIdHeaders fields
        , url = apiUrl ++ "/api/v1/workspaces"
        , body = Http.jsonBody (E.object fields)
        , expect = Http.expectJson toMsg workspaceDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


updateWorkspace : String -> String -> List ( String, E.Value ) -> (Result Http.Error Workspace -> msg) -> Cmd msg
updateWorkspace apiUrl wsId fields toMsg =
    let
        headers =
            requestIdHeaders fields
    in
    Http.request
        { method = "PUT"
        , headers = headers
        , url = apiUrl ++ "/api/v1/workspaces/" ++ wsId
        , body = Http.jsonBody (E.object fields)
        , expect = Http.expectJson toMsg workspaceDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


deleteWorkspace : String -> String -> String -> (Result Http.Error () -> msg) -> Cmd msg
deleteWorkspace apiUrl wsId requestId toMsg =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "X-Request-Id" requestId ]
        , url = apiUrl ++ "/api/v1/workspaces/" ++ wsId
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


purgeWorkspace : String -> String -> String -> (Result Http.Error () -> msg) -> Cmd msg
purgeWorkspace apiUrl wsId requestId toMsg =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "X-Request-Id" requestId ]
        , url = apiUrl ++ "/api/v1/workspaces/" ++ wsId ++ "/purge"
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


fetchWorkspaceMemberships : String -> String -> (Result Http.Error (PaginatedResult WorkspaceMembership) -> msg) -> Cmd msg
fetchWorkspaceMemberships apiUrl wsId toMsg =
    Http.get
        { url = apiUrl ++ "/api/v1/workspaces/" ++ wsId ++ "/memberships?limit=200"
        , expect = Http.expectJson toMsg (paginatedDecoder workspaceMembershipDecoder)
        }


upsertWorkspaceMembership : String -> String -> String -> String -> String -> (Result Http.Error WorkspaceMembership -> msg) -> Cmd msg
upsertWorkspaceMembership apiUrl wsId userId role requestId toMsg =
    let
        fields =
            [ ( "user_id", E.string userId )
            , ( "role", E.string role )
            , ( "request_id", E.string requestId )
            ]
    in
    Http.request
        { method = "POST"
        , headers = requestIdHeaders fields
        , url = apiUrl ++ "/api/v1/workspaces/" ++ wsId ++ "/memberships"
        , body = Http.jsonBody (E.object fields)
        , expect = Http.expectJson toMsg workspaceMembershipDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


deleteWorkspaceMembership : String -> String -> String -> String -> (Result Http.Error () -> msg) -> Cmd msg
deleteWorkspaceMembership apiUrl wsId userId requestId toMsg =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "X-Request-Id" requestId ]
        , url = apiUrl ++ "/api/v1/workspaces/" ++ wsId ++ "/memberships/" ++ userId
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
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


createProject : String -> String -> String -> String -> (Result ApiError Project -> msg) -> Cmd msg
createProject apiUrl wsId name requestId toMsg =
    Http.request
        { method = "POST"
        , headers = [ Http.header "X-Request-Id" requestId ]
        , url = apiUrl ++ "/api/v1/projects"
        , body =
            Http.jsonBody
                (E.object
                    [ ( "workspace_id", E.string wsId )
                    , ( "name", E.string name )
                    , ( "request_id", E.string requestId )
                    ]
                )
        , expect = expectJsonWithApiError projectDecoder toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


updateProject : String -> String -> List ( String, E.Value ) -> (Result ApiError Project -> msg) -> Cmd msg
updateProject apiUrl projectId fields toMsg =
    let
        headers =
            requestIdHeaders fields
    in
    Http.request
        { method = "PUT"
        , headers = headers
        , url = apiUrl ++ "/api/v1/projects/" ++ projectId
        , body = Http.jsonBody (E.object fields)
        , expect = expectJsonWithApiError projectDecoder toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


deleteProject : String -> String -> String -> (Result Http.Error () -> msg) -> Cmd msg
deleteProject apiUrl projectId requestId toMsg =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "X-Request-Id" requestId ]
        , url = apiUrl ++ "/api/v1/projects/" ++ projectId
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


createTask : String -> String -> Maybe String -> String -> String -> (Result ApiError Task -> msg) -> Cmd msg
createTask apiUrl wsId mProjectId title requestId toMsg =
    Http.request
        { method = "POST"
        , headers = [ Http.header "X-Request-Id" requestId ]
        , url = apiUrl ++ "/api/v1/tasks"
        , body =
            Http.jsonBody
                (E.object
                     ([ ( "workspace_id", E.string wsId )
                      , ( "title", E.string title )
                      , ( "request_id", E.string requestId )
                      ]
                        ++ (case mProjectId of
                                Just pid ->
                                    [ ( "project_id", E.string pid ) ]

                                Nothing ->
                                    []
                           )
                    )
                )
        , expect = expectJsonWithApiError taskDecoder toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


updateTask : String -> String -> List ( String, E.Value ) -> (Result ApiError Task -> msg) -> Cmd msg
updateTask apiUrl taskId fields toMsg =
    let
        headers =
            requestIdHeaders fields
    in
    Http.request
        { method = "PUT"
        , headers = headers
        , url = apiUrl ++ "/api/v1/tasks/" ++ taskId
        , body = Http.jsonBody (E.object fields)
        , expect = expectJsonWithApiError taskDecoder toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


deleteTask : String -> String -> String -> (Result Http.Error () -> msg) -> Cmd msg
deleteTask apiUrl taskId requestId toMsg =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "X-Request-Id" requestId ]
        , url = apiUrl ++ "/api/v1/tasks/" ++ taskId
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


createMemory : String -> String -> Maybe String -> Maybe String -> String -> MemoryType -> String -> (Result Http.Error Memory -> msg) -> Cmd msg
createMemory apiUrl wsId projectId taskId content mtype requestId toMsg =
    let
        targetFields =
            List.filterMap identity
                [ Maybe.map (\pid -> ( "project_id", E.string pid )) projectId
                , Maybe.map (\tid -> ( "task_id", E.string tid )) taskId
                ]
    in
    Http.request
        { method = "POST"
        , headers = [ Http.header "X-Request-Id" requestId ]
        , url = apiUrl ++ "/api/v1/memories"
        , body =
            Http.jsonBody
                (E.object
                    ([ ( "workspace_id", E.string wsId )
                     , ( "content", E.string content )
                     , ( "memory_type", E.string (memoryTypeToString mtype) )
                     , ( "request_id", E.string requestId )
                     ]
                        ++ targetFields
                    )
                )
        , expect = Http.expectJson toMsg memoryDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


updateMemory : String -> String -> List ( String, E.Value ) -> (Result Http.Error Memory -> msg) -> Cmd msg
updateMemory apiUrl memId fields toMsg =
    let
        headers =
            requestIdHeaders fields
    in
    Http.request
        { method = "PUT"
        , headers = headers
        , url = apiUrl ++ "/api/v1/memories/" ++ memId
        , body = Http.jsonBody (E.object fields)
        , expect = Http.expectJson toMsg memoryDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


deleteMemory : String -> String -> String -> (Result Http.Error () -> msg) -> Cmd msg
deleteMemory apiUrl memId requestId toMsg =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "X-Request-Id" requestId ]
        , url = apiUrl ++ "/api/v1/memories/" ++ memId
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


setTags : String -> String -> List String -> String -> (Result Http.Error () -> msg) -> Cmd msg
setTags apiUrl memId tags requestId toMsg =
    Http.request
        { method = "PUT"
        , headers = [ Http.header "X-Request-Id" requestId ]
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


linkProjectMemory : String -> String -> String -> String -> (Result Http.Error () -> msg) -> Cmd msg
linkProjectMemory apiUrl projectId memoryId requestId toMsg =
    Http.request
        { method = "POST"
        , headers = [ Http.header "X-Request-Id" requestId ]
        , url = apiUrl ++ "/api/v1/projects/" ++ projectId ++ "/memories"
        , body = Http.jsonBody (E.object [ ( "memory_id", E.string memoryId ), ( "request_id", E.string requestId ) ])
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


unlinkProjectMemory : String -> String -> String -> String -> (Result Http.Error () -> msg) -> Cmd msg
unlinkProjectMemory apiUrl projectId memoryId requestId toMsg =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "X-Request-Id" requestId ]
        , url = apiUrl ++ "/api/v1/projects/" ++ projectId ++ "/memories/" ++ memoryId
        , body = Http.jsonBody (E.object [ ( "memory_id", E.string memoryId ), ( "request_id", E.string requestId ) ])
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


linkTaskMemory : String -> String -> String -> String -> (Result Http.Error () -> msg) -> Cmd msg
linkTaskMemory apiUrl taskId memoryId requestId toMsg =
    Http.request
        { method = "POST"
        , headers = [ Http.header "X-Request-Id" requestId ]
        , url = apiUrl ++ "/api/v1/tasks/" ++ taskId ++ "/memories"
        , body = Http.jsonBody (E.object [ ( "memory_id", E.string memoryId ), ( "request_id", E.string requestId ) ])
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


unlinkTaskMemory : String -> String -> String -> String -> (Result Http.Error () -> msg) -> Cmd msg
unlinkTaskMemory apiUrl taskId memoryId requestId toMsg =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "X-Request-Id" requestId ]
        , url = apiUrl ++ "/api/v1/tasks/" ++ taskId ++ "/memories/" ++ memoryId
        , body = Http.jsonBody (E.object [ ( "memory_id", E.string memoryId ), ( "request_id", E.string requestId ) ])
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


createProjectWithParent : String -> String -> String -> String -> String -> (Result ApiError Project -> msg) -> Cmd msg
createProjectWithParent apiUrl wsId parentId name requestId toMsg =
    Http.request
        { method = "POST"
        , headers = [ Http.header "X-Request-Id" requestId ]
        , url = apiUrl ++ "/api/v1/projects"
        , body =
            Http.jsonBody
                (E.object
                    [ ( "workspace_id", E.string wsId )
                    , ( "parent_id", E.string parentId )
                    , ( "name", E.string name )
                    , ( "request_id", E.string requestId )
                    ]
                )
        , expect = expectJsonWithApiError projectDecoder toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


createTaskWithParent : String -> String -> Maybe String -> String -> String -> String -> (Result ApiError Task -> msg) -> Cmd msg
createTaskWithParent apiUrl wsId mProjectId parentId title requestId toMsg =
    Http.request
        { method = "POST"
        , headers = [ Http.header "X-Request-Id" requestId ]
        , url = apiUrl ++ "/api/v1/tasks"
        , body =
            Http.jsonBody
                (E.object
                     ([ ( "workspace_id", E.string wsId )
                      , ( "parent_id", E.string parentId )
                      , ( "title", E.string title )
                      , ( "request_id", E.string requestId )
                      ]
                        ++ (case mProjectId of
                                Just pid ->
                                    [ ( "project_id", E.string pid ) ]

                                Nothing ->
                                    []
                           )
                    )
                )
        , expect = expectJsonWithApiError taskDecoder toMsg
        , timeout = Nothing
        , tracker = Nothing
        }



-- TASK DEPENDENCIES


fetchTaskOverview : String -> String -> (Result Http.Error TaskOverview -> msg) -> Cmd msg
fetchTaskOverview apiUrl taskId toMsg =
    Http.get
        { url = apiUrl ++ "/api/v1/tasks/" ++ taskId ++ "/overview"
        , expect = Http.expectJson toMsg taskOverviewDecoder
        }


addTaskDependency : String -> String -> String -> String -> (Result Http.Error () -> msg) -> Cmd msg
addTaskDependency apiUrl taskId dependsOnId requestId toMsg =
    Http.request
        { method = "POST"
        , headers = [ Http.header "X-Request-Id" requestId ]
        , url = apiUrl ++ "/api/v1/tasks/" ++ taskId ++ "/dependencies"
        , body = Http.jsonBody (E.object [ ( "depends_on_id", E.string dependsOnId ), ( "request_id", E.string requestId ) ])
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


removeTaskDependency : String -> String -> String -> String -> (Result Http.Error () -> msg) -> Cmd msg
removeTaskDependency apiUrl taskId dependsOnId requestId toMsg =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "X-Request-Id" requestId ]
        , url = apiUrl ++ "/api/v1/tasks/" ++ taskId ++ "/dependencies/" ++ dependsOnId
        , body = Http.jsonBody (E.object [ ( "depends_on_id", E.string dependsOnId ), ( "request_id", E.string requestId ) ])
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


createWorkspaceGroup : String -> String -> Maybe String -> String -> (Result Http.Error WorkspaceGroup -> msg) -> Cmd msg
createWorkspaceGroup apiUrl name mDescription requestId toMsg =
    Http.request
        { method = "POST"
        , headers = [ Http.header "X-Request-ID" requestId ]
        , url = apiUrl ++ "/api/v1/groups"
        , body =
            Http.jsonBody
                (E.object
                    ([ ( "name", E.string name ), ( "request_id", E.string requestId ) ]
                        ++ (case mDescription of
                                Just desc ->
                                    [ ( "description", E.string desc ) ]

                                Nothing ->
                                    []
                           )
                    )
                )
        , expect = Http.expectJson toMsg workspaceGroupDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


deleteWorkspaceGroup : String -> String -> String -> (Result Http.Error () -> msg) -> Cmd msg
deleteWorkspaceGroup apiUrl groupId requestId toMsg =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "X-Request-ID" requestId ]
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


addGroupMember : String -> String -> String -> String -> (Result Http.Error () -> msg) -> Cmd msg
addGroupMember apiUrl groupId workspaceId requestId toMsg =
    Http.request
        { method = "POST"
        , headers = [ Http.header "X-Request-ID" requestId ]
        , url = apiUrl ++ "/api/v1/groups/" ++ groupId ++ "/members"
        , body = Http.jsonBody (E.object [ ( "workspace_id", E.string workspaceId ), ( "request_id", E.string requestId ) ])
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


removeGroupMember : String -> String -> String -> String -> (Result Http.Error () -> msg) -> Cmd msg
removeGroupMember apiUrl groupId workspaceId requestId toMsg =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "X-Request-ID" requestId ]
        , url = apiUrl ++ "/api/v1/groups/" ++ groupId ++ "/members/" ++ workspaceId
        , body = Http.jsonBody (E.object [ ( "workspace_id", E.string workspaceId ), ( "request_id", E.string requestId ) ])
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }



-- AUDIT LOG


fetchAuditLog : String -> { workspaceId : Maybe String, entityType : Maybe String, entityId : Maybe String, action : Maybe String, since : Maybe String, until : Maybe String, limit : Maybe Int, offset : Maybe Int } -> (Result Http.Error (PaginatedResult AuditLogEntry) -> msg) -> Cmd msg
fetchAuditLog apiUrl filters toMsg =
    let
        params =
            List.filterMap identity
                [ Maybe.map (\v -> "workspace_id=" ++ v) filters.workspaceId
                , Maybe.map (\v -> "entity_type=" ++ v) filters.entityType
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


revertAuditEntry : String -> String -> String -> (Result Http.Error RevertResult -> msg) -> Cmd msg
revertAuditEntry apiUrl auditId requestId toMsg =
    Http.request
        { method = "POST"
        , headers = [ Http.header "X-Request-ID" requestId ]
        , url = apiUrl ++ "/api/v1/audit/" ++ auditId ++ "/revert"
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg revertResultDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


requestIdHeaders : List ( String, E.Value ) -> List Http.Header
requestIdHeaders fields =
    case List.filter (\( key, _ ) -> key == "request_id") fields of
        ( _, value ) :: _ ->
            [ Http.header "X-Request-Id" (decodeEncodedString (E.encode 0 value)) ]

        _ ->
            []


decodeEncodedString : String -> String
decodeEncodedString encoded =
    if String.length encoded >= 2 && String.left 1 encoded == "\"" && String.right 1 encoded == "\"" then
        encoded |> String.dropLeft 1 |> String.dropRight 1

    else
        encoded
