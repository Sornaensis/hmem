module Api exposing
    ( ApiError
    , AuditAction(..)
    , AuditLogEntry
    , CanonicalEnvelope
    , CanonicalFrame(..)
    , CanonicalInvalidation
    , CascadeResult
    , ChangeEvent
    , ChangeStreamScope(..)
    , ChangeType(..)
    , DependencyMutationResult
    , EntityType(..)
    , LinkedMemorySummary
    , Memory
    , MemoryLink
    , MemoryType(..)
    , NextTaskCandidate
    , Observation
    , ObservationListQuery
    , ObservationMatch
    , ObservationMatchQuery
    , ObservationSearchHit
    , ObservationSubject
    , PaginatedResult
    , Project
    , ProjectOverview
    , ProjectReadinessRollup
    , ProjectSearchResult
    , ProjectStatus(..)
    , ResyncPage
    , RevertResult
    , SessionContext
    , SessionGlobalPermissions
    , SessionPrincipal
    , SessionWorkspaceContext
    , SnapshotItem
    , SubjectKind(..)
    , Task
    , TaskDependencyStatusChange
    , TaskDependencySummary
    , TaskMutationResult
    , TaskOverview
    , TaskReadinessRollup
    , TaskSearchResult
    , TaskStatus(..)
    , TimelineActor
    , TimelineBucketCounts
    , TimelineBucketActionTotals
    , TimelineBucketActionCounts
    , TimelineBucketEntityCounts
    , TimelineBucketSeries
    , TimelineNavigation
    , TimelineProjectContext
    , TimelineStatusTransition
    , TimelineTaskContext
    , UnifiedSearchResults
    , Workspace
    , WorkspaceGroup
    , WorkspaceMembership
    , WorkspaceProjectMemoryLink
    , WorkspaceTaskDependencyLink
    , WorkspaceTaskMemoryLink
    , WorkspaceTimelineBucket
    , WorkspaceTimelineBucketsResponse
    , WorkspaceTimelineEvent
    , WorkspaceType(..)
    , addGroupMember
    , addTaskDependency
    , allMemoryTypes
    , allProjectStatuses
    , allTaskStatuses
    , allWorkspaceTypes
    , apiErrorToUserMessage
    , auditActionFromString
    , auditActionToString
    , auditLogEntryDecoder
    , cascadeResultDecoder
    , createMemory
    , createProject
    , createProjectWithParent
    , createTask
    , createTaskWithParent
    , createWorkspace
    , createWorkspaceGroup
    , decodeApiErrorBody
    , decodeCanonicalFrame
    , decodeCanonicalTransportScope
    , decodeChangeEvent
    , deleteMemory
    , deleteProject
    , deleteTask
    , deleteWorkspace
    , deleteWorkspaceGroup
    , deleteWorkspaceMembership
    , dependencyMutationResultDecoder
    , fetchAuditLog
    , fetchChangeStreamResync
    , fetchEntityHistory
    , fetchGroupMembers
    , fetchMemories
    , fetchMemoriesPage
    , fetchMemory
    , fetchMemoryLinks
    , fetchObservation
    , fetchObservationMatches
    , fetchObservations
    , fetchObservationsPage
    , fetchProject
    , fetchProjectMemories
    , fetchProjectNextTasks
    , fetchProjectOverview
    , fetchProjects
    , fetchProjectsPage
    , fetchSessionContext
    , fetchTask
    , fetchTaskMemories
    , fetchTaskOverview
    , fetchTasks
    , fetchTasksPage
    , fetchWorkspace
    , fetchWorkspaceGroups
    , fetchWorkspaceLinks
    , fetchWorkspaceMemberships
    , fetchWorkspaceTimeline
    , fetchWorkspaceTimelineBuckets
    , fetchWorkspaceTimelineRange
    , fetchWorkspaces
    , isLifecycleConflict
    , linkProjectMemory
    , linkTaskMemory
    , memoryDecoder
    , memoryTypeFromString
    , memoryTypeToString
    , nextTaskCandidateDecoder
    , observationDecoder
    , observationListUrl
    , observationMatchBody
    , observationMatchDecoder
    , paginatedDecoder
    , projectDecoder
    , projectOverviewDecoder
    , projectStatusFromString
    , projectStatusOrder
    , projectStatusToString
    , purgeWorkspace
    , removeGroupMember
    , removeTaskDependency
    , resyncPageDecoder
    , revertAuditEntry
    , searchMemories
    , setTags
    , subjectKindFromString
    , subjectKindToString
    , taskDecoder
    , taskMutationResultDecoder
    , taskOverviewDecoder
    , taskStatusFromString
    , taskStatusOrder
    , taskStatusToString
    , unifiedSearch
    , unlinkProjectMemory
    , unlinkTaskMemory
    , updateMemory
    , updateProject
    , updateTask
    , updateWorkspace
    , upsertWorkspaceMembership
    , workspaceDecoder
    , workspaceGroupDecoder
    , workspaceTimelineBucketsResponseDecoder
    , workspaceTimelineEventDecoder
    , workspaceTypeToString
    )

import Http
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, optional, required)
import Json.Encode as E
import Time
import Url



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


type alias NextTaskCandidate =
    { task : Task
    , completionGated : Bool
    , openDescendantCount : Int
    , dependencyBlocked : Bool
    , openDependencyCount : Int
    }


type alias TaskDependencyStatusChange =
    { task : Task
    , previousStatus : TaskStatus
    , currentStatus : TaskStatus
    , previousAutoBlocked : Bool
    , autoBlocked : Bool
    , previousOpenDependencyCount : Int
    , openDependencyCount : Int
    , reason : String
    }


type alias DependencyMutationResult =
    { action : String
    , taskId : String
    , dependsOnId : String
    , affectedTasks : List TaskDependencyStatusChange
    }


type alias TaskMutationResult =
    { task : Task
    , dependencyEffects : List TaskDependencyStatusChange
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


type alias Observation =
    { id : String
    , workspaceId : String
    , subjects : List ObservationSubject

    -- Kept as the primary-subject compatibility projection for existing callers.
    , subjectKind : SubjectKind
    , subject : String
    , gitSha : String
    , content : String
    , createdAt : String
    , updatedAt : String
    }


type alias ObservationSubject =
    { subjectKind : SubjectKind
    , subject : String
    }


type alias ObservationListQuery =
    { workspaceId : String
    , subjectKind : Maybe SubjectKind
    , subject : Maybe String
    , gitSha : Maybe String
    , query : Maybe String
    , limit : Int
    , offset : Int
    }


type alias ObservationMatchQuery =
    { workspaceId : String
    , paths : List String
    , subjectKind : Maybe SubjectKind
    , gitSha : Maybe String
    , query : Maybe String
    , limit : Int
    , offset : Int
    }


type alias ObservationMatch =
    { observation : Observation
    , matchedPaths : List String
    , matchedSubjects : List ObservationSubject
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


type alias WorkspaceProjectMemoryLink =
    { projectId : String
    , memoryId : String
    }


type alias WorkspaceTaskMemoryLink =
    { taskId : String
    , memoryId : String
    }


type alias WorkspaceTaskDependencyLink =
    { taskId : String
    , dependsOnId : String
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
    , readinessRollup : TaskReadinessRollup
    }


type alias TaskReadinessRollup =
    { openSubtaskCount : Int
    , doneSubtaskCount : Int
    , cancelledSubtaskCount : Int
    , blockedSubtaskCount : Int
    , dependencyBlockedTaskCount : Int
    , openDependencyCount : Int
    , completionReady : Bool
    }


type alias ProjectOverview =
    { project : Project
    , tasks : List Task
    , subprojects : List Project
    , readinessRollup : ProjectReadinessRollup
    }


type alias ProjectReadinessRollup =
    { openProjectCount : Int
    , closedProjectCount : Int
    , openTaskCount : Int
    , doneTaskCount : Int
    , cancelledTaskCount : Int
    , blockedTaskCount : Int
    , dependencyBlockedTaskCount : Int
    , openDependencyCount : Int
    , completionReady : Bool
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


type alias ObservationSearchHit =
    { id : String
    , workspaceId : String
    , subjectKind : SubjectKind
    , subject : String
    , gitSha : String
    , contentPreview : String
    , updatedAt : String
    }


type alias UnifiedSearchResults =
    { observations : List ObservationSearchHit
    , projects : List Project
    , tasks : List Task
    }


type alias PaginatedResult a =
    { items : List a
    , hasMore : Bool
    }


type alias CascadeResult =
    { affected : Int
    , projectCount : Int
    , taskCount : Int
    , dependencyLinkCount : Int
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


type alias WorkspaceTimelineEvent =
    { id : String
    , workspaceId : String
    , eventType : String
    , entityType : String
    , entityId : String
    , title : String
    , occurredAt : String
    , actor : Maybe TimelineActor
    , project : Maybe TimelineProjectContext
    , parentTask : Maybe TimelineTaskContext
    , statusTransition : Maybe TimelineStatusTransition
    , navigation : TimelineNavigation
    , sourceAuditId : Maybe String
    }


type alias TimelineActor =
    { actorType : Maybe String
    , actorId : Maybe String
    , actorLabel : Maybe String
    }


type alias TimelineProjectContext =
    { id : String
    , name : String
    }


type alias TimelineTaskContext =
    { id : String
    , title : String
    }


type alias TimelineStatusTransition =
    { from : String
    , to : String
    }


type alias TimelineNavigation =
    { entityType : String
    , entityId : String
    }


type alias TimelineBucketCounts =
    { created : Int
    , completed : Int
    , cancelled : Int
    }


type alias TimelineBucketEntityCounts =
    { project : TimelineBucketCounts
    , subproject : TimelineBucketCounts
    , task : TimelineBucketCounts
    , subtask : TimelineBucketCounts
    }


type alias TimelineBucketSeries =
    { project : TimelineBucketActionCounts
    , task : TimelineBucketActionCounts
    , subtask : TimelineBucketActionCounts
    , observation : TimelineBucketActionCounts
    }


type alias TimelineBucketActionCounts =
    { created : Int
    , completed : Int
    , deleted : Int
    }


type alias TimelineBucketActionTotals =
    { created : Int
    , completed : Int
    , deleted : Int
    }


type alias WorkspaceTimelineBucket =
    { bucketStart : String
    , bucketEnd : String
    , label : String
    , counts : TimelineBucketEntityCounts
    , totals : TimelineBucketCounts
    , series : TimelineBucketSeries
    , seriesTotals : TimelineBucketActionTotals
    }


type alias WorkspaceTimelineBucketsResponse =
    { workspaceId : String
    , since : String
    , until : String
    , bucket : String
    , buckets : List WorkspaceTimelineBucket
    }


type ApiError
    = StructuredApiError StructuredErrorBody
    | TransportError Http.Error
    | DecodeError String


type alias StructuredErrorBody =
    { error : String
    , code : Maybe String
    , message : String
    , details : List String
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
        |> optional "details" (D.list D.string) []
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

                Just "TASK_SUBTASK_DEPTH_EXCEEDED" ->
                    "Subtasks can only be added to top-level tasks. Move existing subtasks before nesting this task."

                Just "TASK_SUBTASK_START_BLOCKED" ->
                    "Start the parent task before moving a subtask to in progress."

                Just "TASK_DEPENDENCY_CROSS_WORKSPACE" ->
                    "Move tasks only after removing or repairing dependencies that cross workspaces."

                Just "TASK_DEPENDENCY_CROSS_PROJECT" ->
                    "Move dependent tasks together, or remove the dependency before moving tasks across projects."

                Just "TASK_DEPENDENCY_HIERARCHY_CYCLE" ->
                    "Remove the dependency chain from the subtask to the target parent before moving it."

                Just "TASK_PARENT_PROJECT_MISMATCH" ->
                    "Move the parent task with the subtask, detach the subtask first, or choose the parent task's project."

                Just "TASK_BATCH_MOVE_CROSS_WORKSPACE" ->
                    "Move tasks from one workspace at a time."

                _ ->
                    if List.isEmpty apiError.details then
                        apiError.requiredAction
                            |> Maybe.withDefault (Maybe.withDefault apiError.message apiError.hint)

                    else
                        String.join "; " apiError.details

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


type SubjectKind
    = SubjectFile
    | SubjectGlob


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


subjectKindToString : SubjectKind -> String
subjectKindToString kind =
    case kind of
        SubjectFile ->
            "file"

        SubjectGlob ->
            "glob"


subjectKindFromString : String -> Maybe SubjectKind
subjectKindFromString value =
    case value of
        "file" ->
            Just SubjectFile

        "glob" ->
            Just SubjectGlob

        _ ->
            Nothing


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


memoryTypeFromString : String -> Maybe MemoryType
memoryTypeFromString s =
    case s of
        "short_term" ->
            Just ShortTerm

        "long_term" ->
            Just LongTerm

        _ ->
            Nothing


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
        ProjActive ->
            0

        ProjPaused ->
            1

        ProjCompleted ->
            2

        ProjArchived ->
            3


{-| Sort order for task statuses: in-progress first, cancelled last.
-}
taskStatusOrder : TaskStatus -> Int
taskStatusOrder ts =
    case ts of
        InProgress ->
            0

        Todo ->
            1

        Blocked ->
            2

        Done ->
            3

        Cancelled ->
            4


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


nextTaskCandidateDecoder : Decoder NextTaskCandidate
nextTaskCandidateDecoder =
    D.succeed NextTaskCandidate
        |> required "task" taskDecoder
        |> required "completion_gated" D.bool
        |> required "open_descendant_count" D.int
        |> required "dependency_blocked" D.bool
        |> required "open_dependency_count" D.int


taskDependencyStatusChangeDecoder : Decoder TaskDependencyStatusChange
taskDependencyStatusChangeDecoder =
    D.succeed TaskDependencyStatusChange
        |> required "task" taskDecoder
        |> required "previous_status" taskStatusDecoder
        |> required "current_status" taskStatusDecoder
        |> required "previous_auto_blocked" D.bool
        |> required "auto_blocked" D.bool
        |> required "previous_open_dependency_count" D.int
        |> required "open_dependency_count" D.int
        |> required "reason" D.string


dependencyMutationResultDecoder : Decoder DependencyMutationResult
dependencyMutationResultDecoder =
    D.succeed DependencyMutationResult
        |> required "action" D.string
        |> required "task_id" D.string
        |> required "depends_on_id" D.string
        |> required "affected_tasks" (D.list taskDependencyStatusChangeDecoder)


taskMutationResultDecoder : Decoder TaskMutationResult
taskMutationResultDecoder =
    D.map2 TaskMutationResult
        taskDecoder
        (D.oneOf
            [ D.field "dependency_effects" (D.list taskDependencyStatusChangeDecoder)
            , D.succeed []
            ]
        )


cascadeResultDecoder : Decoder CascadeResult
cascadeResultDecoder =
    D.succeed CascadeResult
        |> required "affected" D.int
        |> required "project_count" D.int
        |> required "task_count" D.int
        |> required "dependency_link_count" D.int


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


observationDecoder : Decoder Observation
observationDecoder =
    D.succeed observationFromFields
        |> required "id" D.string
        |> required "workspace_id" D.string
        |> custom observationSubjectsDecoder
        |> required "git_sha" D.string
        |> required "content" D.string
        |> required "created_at" D.string
        |> required "updated_at" D.string


observationFromFields : String -> String -> List ObservationSubject -> String -> String -> String -> String -> Observation
observationFromFields id workspaceId subjects gitSha content createdAt updatedAt =
    case subjects of
        primary :: _ ->
            { id = id
            , workspaceId = workspaceId
            , subjects = subjects
            , subjectKind = primary.subjectKind
            , subject = primary.subject
            , gitSha = gitSha
            , content = content
            , createdAt = createdAt
            , updatedAt = updatedAt
            }

        [] ->
            -- observationSubjectsDecoder rejects this before construction.
            { id = id
            , workspaceId = workspaceId
            , subjects = []
            , subjectKind = SubjectFile
            , subject = ""
            , gitSha = gitSha
            , content = content
            , createdAt = createdAt
            , updatedAt = updatedAt
            }


observationSubjectsDecoder : Decoder (List ObservationSubject)
observationSubjectsDecoder =
    D.oneOf
        [ D.map Just (D.field "subjects" D.value)
        , D.succeed Nothing
        ]
        |> D.andThen
            (\canonicalSubjects ->
                case canonicalSubjects of
                    Just subjectsValue ->
                        case D.decodeValue (D.list observationSubjectDecoder) subjectsValue of
                            Ok subjects ->
                                nonEmptySubjectsDecoder subjects

                            Err error ->
                                D.fail (D.errorToString error)

                    Nothing ->
                        D.map2 (\subjectKind subject -> [ { subjectKind = subjectKind, subject = subject } ])
                            (D.field "subject_kind" subjectKindDecoder)
                            (D.field "subject" D.string)
            )


nonEmptySubjectsDecoder : List ObservationSubject -> Decoder (List ObservationSubject)
nonEmptySubjectsDecoder subjects =
    if List.isEmpty subjects then
        D.fail "Observation subjects must not be empty"

    else
        D.succeed subjects


observationSubjectDecoder : Decoder ObservationSubject
observationSubjectDecoder =
    D.map2 ObservationSubject
        (D.field "subject_kind" subjectKindDecoder)
        (D.field "subject" D.string)


observationMatchDecoder : Decoder ObservationMatch
observationMatchDecoder =
    D.succeed ObservationMatch
        |> required "observation" observationDecoder
        |> required "matched_paths" (D.list D.string)
        |> required "matched_subjects" (D.list observationSubjectDecoder)


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


subjectKindDecoder : Decoder SubjectKind
subjectKindDecoder =
    D.string
        |> D.andThen
            (\value ->
                subjectKindFromString value
                    |> Maybe.map D.succeed
                    |> Maybe.withDefault (D.fail ("Unknown observation subject kind: " ++ value))
            )


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


observationSearchHitDecoder : Decoder ObservationSearchHit
observationSearchHitDecoder =
    D.succeed ObservationSearchHit
        |> required "id" D.string
        |> required "workspace_id" D.string
        |> required "subject_kind" subjectKindDecoder
        |> required "subject" D.string
        |> required "git_sha" D.string
        |> required "content_preview" D.string
        |> required "updated_at" D.string


unifiedSearchResultsDecoder : Decoder UnifiedSearchResults
unifiedSearchResultsDecoder =
    D.succeed UnifiedSearchResults
        |> optional "observations" (D.list observationSearchHitDecoder) []
        |> optional "projects" (D.list projectDecoder) []
        |> optional "tasks" (D.list taskDecoder) []


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


workspaceTimelineEventDecoder : Decoder WorkspaceTimelineEvent
workspaceTimelineEventDecoder =
    D.succeed WorkspaceTimelineEvent
        |> required "id" D.string
        |> required "workspace_id" D.string
        |> required "event_type" D.string
        |> required "entity_type" D.string
        |> required "entity_id" D.string
        |> required "title" D.string
        |> required "occurred_at" D.string
        |> optional "actor" (D.nullable timelineActorDecoder) Nothing
        |> optional "project" (D.nullable timelineProjectContextDecoder) Nothing
        |> optional "parent_task" (D.nullable timelineTaskContextDecoder) Nothing
        |> optional "status_transition" (D.nullable timelineStatusTransitionDecoder) Nothing
        |> required "navigation" timelineNavigationDecoder
        |> optional "source_audit_id" (D.nullable D.string) Nothing


timelineActorDecoder : Decoder TimelineActor
timelineActorDecoder =
    D.succeed TimelineActor
        |> optional "type" (D.nullable D.string) Nothing
        |> optional "id" (D.nullable D.string) Nothing
        |> optional "label" (D.nullable D.string) Nothing


timelineProjectContextDecoder : Decoder TimelineProjectContext
timelineProjectContextDecoder =
    D.succeed TimelineProjectContext
        |> required "id" D.string
        |> required "name" D.string


timelineTaskContextDecoder : Decoder TimelineTaskContext
timelineTaskContextDecoder =
    D.succeed TimelineTaskContext
        |> required "id" D.string
        |> required "title" D.string


timelineStatusTransitionDecoder : Decoder TimelineStatusTransition
timelineStatusTransitionDecoder =
    D.succeed TimelineStatusTransition
        |> required "from" D.string
        |> required "to" D.string


timelineNavigationDecoder : Decoder TimelineNavigation
timelineNavigationDecoder =
    D.succeed TimelineNavigation
        |> required "entity_type" D.string
        |> required "entity_id" D.string


workspaceTimelineBucketsResponseDecoder : Decoder WorkspaceTimelineBucketsResponse
workspaceTimelineBucketsResponseDecoder =
    D.succeed WorkspaceTimelineBucketsResponse
        |> required "workspace_id" D.string
        |> required "since" D.string
        |> required "until" D.string
        |> required "bucket" D.string
        |> required "buckets" (D.list workspaceTimelineBucketDecoder)


workspaceTimelineBucketDecoder : Decoder WorkspaceTimelineBucket
workspaceTimelineBucketDecoder =
    D.succeed WorkspaceTimelineBucket
        |> required "bucket_start" D.string
        |> required "bucket_end" D.string
        |> required "label" D.string
        |> required "counts" timelineBucketEntityCountsDecoder
        |> required "totals" timelineBucketCountsDecoder
        |> required "series" timelineBucketSeriesDecoder
        |> required "series_totals" timelineBucketActionTotalsDecoder


timelineBucketEntityCountsDecoder : Decoder TimelineBucketEntityCounts
timelineBucketEntityCountsDecoder =
    D.succeed TimelineBucketEntityCounts
        |> required "project" timelineBucketCountsDecoder
        |> required "subproject" timelineBucketCountsDecoder
        |> required "task" timelineBucketCountsDecoder
        |> required "subtask" timelineBucketCountsDecoder


timelineBucketCountsDecoder : Decoder TimelineBucketCounts
timelineBucketCountsDecoder =
    D.succeed TimelineBucketCounts
        |> required "created" D.int
        |> required "completed" D.int
        |> required "cancelled" D.int


timelineBucketSeriesDecoder : Decoder TimelineBucketSeries
timelineBucketSeriesDecoder =
    D.succeed TimelineBucketSeries
        |> required "project" timelineBucketActionCountsDecoder
        |> required "task" timelineBucketActionCountsDecoder
        |> required "subtask" timelineBucketActionCountsDecoder
        |> required "observation" timelineBucketActionCountsDecoder


timelineBucketActionCountsDecoder : Decoder TimelineBucketActionCounts
timelineBucketActionCountsDecoder =
    D.succeed TimelineBucketActionCounts
        |> required "created" D.int
        |> required "completed" D.int
        |> required "deleted" D.int


timelineBucketActionTotalsDecoder : Decoder TimelineBucketActionTotals
timelineBucketActionTotalsDecoder =
    D.succeed TimelineBucketActionTotals
        |> required "created" D.int
        |> required "completed" D.int
        |> required "deleted" D.int


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
    | EObservation
    | EMemoryLink
    | ECategory
    | EWorkspaceGroup
    | ESavedView
    | ETaskDependency
    | ECategoryLink
    | ETag
    | EOther String


{-| Public v1 change-stream values. These intentionally do not model a
cursor: cursor ordering is server-internal and clients only retain opaque
bearers plus event ids.
-}
type ChangeStreamScope
    = WorkspaceScope String
    | GlobalScope


type alias CanonicalInvalidation =
    { kind : String
    , target : String
    }


type alias CanonicalEnvelope =
    { eventId : String
    , scope : ChangeStreamScope
    , workspaceId : Maybe String
    , entityType : String
    , entityId : String
    , entityAction : String
    , invalidations : List CanonicalInvalidation
    }


type CanonicalFrame
    = CanonicalChange CanonicalEnvelope
    | CanonicalCheckpoint String
    | CanonicalAccessGranted String
    | CanonicalAccessRevoked (Maybe String)
    | CanonicalResyncRequired
    | CanonicalScoped ChangeStreamScope CanonicalFrame
    | CanonicalSnapshot ChangeStreamScope (List SnapshotItem) String
    | CanonicalBatch ChangeStreamScope (List CanonicalFrame)


type alias SnapshotItem =
    { kind : String
    , data : D.Value
    }


type alias ResyncPage =
    { items : List SnapshotItem
    , hasMore : Bool
    , nextPageToken : Maybe String
    , resumeToken : Maybe String
    }


scopeDecoder : Decoder ChangeStreamScope
scopeDecoder =
    D.field "scope" D.string
        |> D.andThen
            (\kind ->
                case kind of
                    "workspace" ->
                        D.map WorkspaceScope (D.field "workspace_id" nonEmptyStringDecoder)

                    "global" ->
                        D.succeed GlobalScope

                    _ ->
                        D.fail "unknown change-stream scope"
            )


canonicalInvalidationDecoder : Decoder CanonicalInvalidation
canonicalInvalidationDecoder =
    D.map2 CanonicalInvalidation
        (D.field "kind" nonEmptyStringDecoder)
        (D.field "target" nonEmptyStringDecoder)


nonEmptyStringDecoder : Decoder String
nonEmptyStringDecoder =
    D.string
        |> D.andThen
            (\value ->
                if String.isEmpty value then
                    D.fail "required string is empty"

                else
                    D.succeed value
            )


optionalNonEmptyField : String -> Decoder (Maybe String)
optionalNonEmptyField fieldName =
    D.value
        |> D.andThen
            (\object ->
                case D.decodeValue (D.field fieldName D.value) object of
                    Err _ ->
                        D.succeed Nothing

                    Ok raw ->
                        case D.decodeValue (D.nullable nonEmptyStringDecoder) raw of
                            Ok value ->
                                D.succeed value

                            Err _ ->
                                D.fail ("invalid optional field: " ++ fieldName)
            )


schemaVersionOneDecoder : Decoder ()
schemaVersionOneDecoder =
    D.field "schema_version" D.int
        |> D.andThen
            (\version ->
                if version == 1 then
                    D.succeed ()

                else
                    D.fail "unsupported change-stream schema"
            )


canonicalEntityTypeDecoder : Decoder String
canonicalEntityTypeDecoder =
    nonEmptyStringDecoder
        |> D.andThen
            (\entityType ->
                if List.member entityType [ "workspace", "workspace_group", "project", "task", "observation", "task_dependency", "workspace_group_membership", "workspace_membership" ] then
                    D.succeed entityType

                else
                    D.fail "unknown canonical entity type"
            )


canonicalEntityActionDecoder : Decoder String
canonicalEntityActionDecoder =
    nonEmptyStringDecoder
        |> D.andThen
            (\action ->
                if List.member action [ "created", "updated", "deleted", "restored" ] then
                    D.succeed action

                else
                    D.fail "unknown canonical entity action"
            )


nonEmptyListDecoder : Decoder a -> Decoder (List a)
nonEmptyListDecoder itemDecoder =
    D.list itemDecoder
        |> D.andThen
            (\items ->
                if List.isEmpty items then
                    D.fail "required list is empty"

                else
                    D.succeed items
            )


canonicalTransactionDecoder : Decoder ()
canonicalTransactionDecoder =
    D.map3 (\_ _ _ -> ())
        (D.field "id" nonEmptyStringDecoder)
        (D.field "cause" nonEmptyStringDecoder
            |> D.andThen
                (\cause ->
                    if List.member cause [ "rest", "mcp", "audit_revert", "core", "migration" ] then
                        D.succeed cause

                    else
                        D.fail "unknown canonical transaction cause"
                )
        )
        (D.field "request_id" (D.nullable nonEmptyStringDecoder))


canonicalActorDecoder : Decoder ()
canonicalActorDecoder =
    D.map2 (\_ _ -> ())
        (D.field "type" nonEmptyStringDecoder
            |> D.andThen
                (\actorType ->
                    if List.member actorType [ "user", "service", "system" ] then
                        D.succeed actorType

                    else
                        D.fail "unknown canonical actor type"
                )
        )
        (D.field "id" (D.nullable nonEmptyStringDecoder))


canonicalEnvelopeDecoder : Decoder CanonicalEnvelope
canonicalEnvelopeDecoder =
    let
        fieldsDecoder =
            D.map7 CanonicalEnvelope
                (D.field "event_id" nonEmptyStringDecoder)
                scopeDecoder
                (D.field "workspace_id" (D.nullable nonEmptyStringDecoder))
                (D.field "entity" (D.field "type" canonicalEntityTypeDecoder))
                (D.field "entity" (D.field "id" nonEmptyStringDecoder))
                (D.field "entity" (D.field "action" canonicalEntityActionDecoder))
                (D.field "invalidations" (nonEmptyListDecoder canonicalInvalidationDecoder))

        metadataDecoder =
            D.map4 (\envelope _ _ _ -> envelope)
                fieldsDecoder
                schemaVersionOneDecoder
                (D.field "occurred_at" nonEmptyStringDecoder)
                (D.map2 (\_ _ -> ())
                    (D.field "transaction" canonicalTransactionDecoder)
                    (D.field "actor" canonicalActorDecoder)
                )
    in
    metadataDecoder
        |> D.andThen
            (\envelope ->
                case ( envelope.scope, envelope.workspaceId ) of
                    ( WorkspaceScope scopeWorkspaceId, Just envelopeWorkspaceId ) ->
                        if scopeWorkspaceId == envelopeWorkspaceId then
                            D.succeed envelope

                        else
                            D.fail "workspace envelope scope mismatch"

                    ( GlobalScope, Nothing ) ->
                        D.succeed envelope

                    _ ->
                        D.fail "canonical envelope scope identity is inconsistent"
            )


canonicalFrameDecoder : Decoder CanonicalFrame
canonicalFrameDecoder =
    D.oneOf
        [ canonicalTransportDecoder
        , canonicalWireFrameDecoder
        ]


canonicalWireFrameDecoder : Decoder CanonicalFrame
canonicalWireFrameDecoder =
    schemaVersionOneDecoder
        |> D.andThen
            (\_ ->
                D.field "type" nonEmptyStringDecoder
                    |> D.andThen
                        (\frameType ->
                            case frameType of
                                "change" ->
                                    D.map CanonicalChange (D.field "event" canonicalEnvelopeDecoder)

                                "checkpoint" ->
                                    D.map CanonicalCheckpoint
                                        (D.field "catch_up" nonEmptyStringDecoder
                                            |> D.andThen
                                                (\catchUp ->
                                                    if catchUp == "complete" then
                                                        D.field "resume_token" nonEmptyStringDecoder

                                                    else
                                                        D.fail "checkpoint is not terminal"
                                                )
                                        )

                                "access_granted" ->
                                    D.map CanonicalAccessGranted (D.field "workspace_id" nonEmptyStringDecoder)

                                "access_revoked" ->
                                    D.map CanonicalAccessRevoked (optionalNonEmptyField "workspace_id")

                                "resync_required" ->
                                    D.succeed CanonicalResyncRequired

                                _ ->
                                    D.fail "unknown change-stream frame"
                        )
            )


canonicalTransportDecoder : Decoder CanonicalFrame
canonicalTransportDecoder =
    schemaVersionOneDecoder
        |> D.andThen
            (\_ ->
                D.field "transport" nonEmptyStringDecoder
                    |> D.andThen
                        (\transport ->
                            case transport of
                                "frame" ->
                                    D.map2 CanonicalScoped
                                        (D.field "scope" scopeDecoder)
                                        (D.field "frame" canonicalWireFrameDecoder)

                                "frames" ->
                                    D.map2 CanonicalBatch
                                        (D.field "scope" scopeDecoder)
                                        (D.field "frames" (nonEmptyListDecoder canonicalWireFrameDecoder))

                                "snapshot" ->
                                    D.map3 CanonicalSnapshot
                                        (D.field "scope" scopeDecoder)
                                        (D.field "items" (D.list snapshotItemDecoder))
                                        (D.field "resume_token" nonEmptyStringDecoder)

                                _ ->
                                    D.fail "unknown change-stream transport message"
                        )
            )


decodeCanonicalFrame : String -> Maybe CanonicalFrame
decodeCanonicalFrame raw =
    D.decodeString canonicalFrameDecoder raw |> Result.toMaybe


decodeCanonicalTransportScope : String -> Maybe ChangeStreamScope
decodeCanonicalTransportScope raw =
    D.decodeString
        (D.map2 (\_ scope -> scope)
            schemaVersionOneDecoder
            (D.field "scope" scopeDecoder)
        )
        raw
        |> Result.toMaybe


snapshotItemDecoder : Decoder SnapshotItem
snapshotItemDecoder =
    D.field "schema_version" D.int
        |> D.andThen
            (\version ->
                if version == 1 then
                    D.map2 SnapshotItem
                        (D.field "kind" nonEmptyStringDecoder
                            |> D.andThen
                                (\kind ->
                                    if List.member kind [ "workspace", "workspace_group", "project", "task", "task_dependency", "observation" ] then
                                        D.succeed kind

                                    else
                                        D.fail "unknown snapshot kind"
                                )
                        )
                        (D.field "data" D.value)

                else
                    D.fail "unsupported snapshot schema"
            )


resyncPageDecoder : Decoder ResyncPage
resyncPageDecoder =
    D.map4 ResyncPage
        (D.field "items" (D.list snapshotItemDecoder))
        (D.field "has_more" D.bool)
        (optionalNonEmptyField "next_page_token")
        (optionalNonEmptyField "resume_token")
        |> D.andThen
            (\page ->
                case ( page.hasMore, page.nextPageToken, page.resumeToken ) of
                    ( True, Just _, Nothing ) ->
                        D.succeed page

                    ( False, Nothing, Just _ ) ->
                        D.succeed page

                    _ ->
                        D.fail "inconsistent resync page tokens"
            )


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

                    "observation" ->
                        D.succeed EObservation

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


fetchChangeStreamResync : String -> ChangeStreamScope -> Maybe String -> String -> (Result Http.Error ResyncPage -> msg) -> Cmd msg
fetchChangeStreamResync apiUrl scope maybePageToken startKey toMsg =
    let
        scopeValue =
            case scope of
                WorkspaceScope workspaceId ->
                    E.object [ ( "scope", E.string "workspace" ), ( "workspace_id", E.string workspaceId ) ]

                GlobalScope ->
                    E.object [ ( "scope", E.string "global" ) ]

        body =
            case maybePageToken of
                Just pageToken ->
                    E.object [ ( "scope", scopeValue ), ( "page_token", E.string pageToken ) ]

                Nothing ->
                    E.object [ ( "scope", scopeValue ), ( "page_size", E.int 100 ), ( "start_idempotency_key", E.string startKey ) ]
    in
    Http.post
        { url = apiUrl ++ "/api/v1/change-stream/resync"
        , body = Http.jsonBody body
        , expect = Http.expectJson toMsg resyncPageDecoder
        }


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
    fetchProjectsPage apiUrl wsId 0 toMsg


fetchProjectsPage : String -> String -> Int -> (Result Http.Error (PaginatedResult Project) -> msg) -> Cmd msg
fetchProjectsPage apiUrl wsId offset toMsg =
    Http.get
        { url = apiUrl ++ "/api/v1/projects?workspace_id=" ++ wsId ++ "&limit=200&offset=" ++ String.fromInt offset
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
    fetchTasksPage apiUrl wsId 0 toMsg


fetchTasksPage : String -> String -> Int -> (Result Http.Error (PaginatedResult Task) -> msg) -> Cmd msg
fetchTasksPage apiUrl wsId offset toMsg =
    Http.get
        { url = apiUrl ++ "/api/v1/tasks?workspace_id=" ++ wsId ++ "&limit=200&offset=" ++ String.fromInt offset
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
    fetchMemoriesPage apiUrl wsId 0 toMsg


fetchMemoriesPage : String -> String -> Int -> (Result Http.Error (PaginatedResult Memory) -> msg) -> Cmd msg
fetchMemoriesPage apiUrl wsId offset toMsg =
    Http.get
        { url = apiUrl ++ "/api/v1/memories?workspace_id=" ++ wsId ++ "&limit=200&offset=" ++ String.fromInt offset
        , expect = Http.expectJson toMsg (paginatedDecoder memoryDecoder)
        }


fetchMemory : String -> String -> (Result Http.Error Memory -> msg) -> Cmd msg
fetchMemory apiUrl memId toMsg =
    Http.get
        { url = apiUrl ++ "/api/v1/memories/" ++ memId
        , expect = Http.expectJson toMsg memoryDecoder
        }


observationListUrl : String -> ObservationListQuery -> String
observationListUrl apiUrl listQuery =
    let
        optional name maybeValue =
            maybeValue |> Maybe.map (\value -> name ++ "=" ++ Url.percentEncode value)

        params =
            [ Just ("workspace_id=" ++ Url.percentEncode listQuery.workspaceId)
            , optional "subject_kind" (Maybe.map subjectKindToString listQuery.subjectKind)
            , optional "subject" listQuery.subject
            , optional "git_sha" listQuery.gitSha
            , optional "query" listQuery.query
            , Just ("limit=" ++ String.fromInt listQuery.limit)
            , Just ("offset=" ++ String.fromInt listQuery.offset)
            ]
                |> List.filterMap identity
    in
    apiUrl ++ "/api/v1/observations?" ++ String.join "&" params


fetchObservations : String -> ObservationListQuery -> (Result Http.Error (PaginatedResult Observation) -> msg) -> Cmd msg
fetchObservations apiUrl listQuery toMsg =
    fetchObservationsPage apiUrl listQuery toMsg


fetchObservationsPage : String -> ObservationListQuery -> (Result Http.Error (PaginatedResult Observation) -> msg) -> Cmd msg
fetchObservationsPage apiUrl listQuery toMsg =
    Http.get
        { url = observationListUrl apiUrl listQuery
        , expect = Http.expectJson toMsg (paginatedDecoder observationDecoder)
        }


fetchObservation : String -> String -> (Result Http.Error Observation -> msg) -> Cmd msg
fetchObservation apiUrl observationId toMsg =
    Http.get
        { url = apiUrl ++ "/api/v1/observations/" ++ Url.percentEncode observationId
        , expect = Http.expectJson toMsg observationDecoder
        }


fetchObservationMatches : String -> ObservationMatchQuery -> (Result Http.Error (PaginatedResult ObservationMatch) -> msg) -> Cmd msg
fetchObservationMatches apiUrl matchQuery toMsg =
    Http.request
        { method = "POST"
        , headers = []
        , url = apiUrl ++ "/api/v1/observations/match"
        , body = Http.jsonBody (observationMatchBody matchQuery)
        , expect = Http.expectJson toMsg (paginatedDecoder observationMatchDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


observationMatchBody : ObservationMatchQuery -> E.Value
observationMatchBody matchQuery =
    let
        optional name encodeValue value =
            value |> Maybe.map (\present -> ( name, encodeValue present ))
    in
    E.object
        ([ ( "workspace_id", E.string matchQuery.workspaceId )
         , ( "paths", E.list E.string matchQuery.paths )
         , ( "limit", E.int matchQuery.limit )
         , ( "offset", E.int matchQuery.offset )
         ]
            ++ List.filterMap identity
                [ optional "subject_kind" (E.string << subjectKindToString) matchQuery.subjectKind
                , optional "git_sha" E.string matchQuery.gitSha
                , optional "query" E.string matchQuery.query
                ]
        )


fetchWorkspaceTimeline : String -> String -> (Result Http.Error (PaginatedResult WorkspaceTimelineEvent) -> msg) -> Cmd msg
fetchWorkspaceTimeline apiUrl wsId toMsg =
    fetchWorkspaceTimelineRange apiUrl wsId Nothing Nothing toMsg


fetchWorkspaceTimelineRange : String -> String -> Maybe String -> Maybe String -> (Result Http.Error (PaginatedResult WorkspaceTimelineEvent) -> msg) -> Cmd msg
fetchWorkspaceTimelineRange apiUrl wsId maybeSince maybeUntil toMsg =
    let
        rangeParams =
            [ Maybe.map (\since -> "since=" ++ since) maybeSince
            , Maybe.map (\until -> "until=" ++ until) maybeUntil
            ]
                |> List.filterMap identity

        rangeQuery =
            case rangeParams of
                [] ->
                    ""

                _ ->
                    "&" ++ String.join "&" rangeParams
    in
    Http.get
        { url = apiUrl ++ "/api/v1/workspaces/" ++ wsId ++ "/timeline?limit=50" ++ rangeQuery
        , expect = Http.expectJson toMsg (paginatedDecoder workspaceTimelineEventDecoder)
        }


fetchWorkspaceTimelineBuckets : String -> String -> String -> String -> String -> (Result Http.Error WorkspaceTimelineBucketsResponse -> msg) -> Cmd msg
fetchWorkspaceTimelineBuckets apiUrl wsId since until bucket toMsg =
    Http.get
        { url = apiUrl ++ "/api/v1/workspaces/" ++ wsId ++ "/timeline/buckets?since=" ++ since ++ "&until=" ++ until ++ "&bucket=" ++ bucket
        , expect = Http.expectJson toMsg workspaceTimelineBucketsResponseDecoder
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


unifiedSearch : String -> String -> String -> List String -> (Result Http.Error UnifiedSearchResults -> msg) -> Cmd msg
unifiedSearch apiUrl query workspaceId entityTypes toMsg =
    let
        body =
            E.object
                [ ( "query", E.string query )
                , ( "workspace_id", E.string workspaceId )
                , ( "entity_types", E.list E.string entityTypes )
                ]
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


deleteProject : String -> String -> String -> (Result ApiError CascadeResult -> msg) -> Cmd msg
deleteProject apiUrl projectId requestId toMsg =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "X-Request-Id" requestId ]
        , url = apiUrl ++ "/api/v1/projects/" ++ projectId
        , body = Http.emptyBody
        , expect = expectJsonWithApiError cascadeResultDecoder toMsg
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


updateTask : String -> String -> List ( String, E.Value ) -> (Result ApiError TaskMutationResult -> msg) -> Cmd msg
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
        , expect = expectJsonWithApiError taskMutationResultDecoder toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


deleteTask : String -> String -> String -> (Result ApiError CascadeResult -> msg) -> Cmd msg
deleteTask apiUrl taskId requestId toMsg =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "X-Request-Id" requestId ]
        , url = apiUrl ++ "/api/v1/tasks/" ++ taskId
        , body = Http.emptyBody
        , expect = expectJsonWithApiError cascadeResultDecoder toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


createMemory : String -> String -> Maybe String -> Maybe String -> String -> MemoryType -> String -> (Result ApiError Memory -> msg) -> Cmd msg
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
        , expect = expectJsonWithApiError memoryDecoder toMsg
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


fetchProjectOverview : String -> String -> (Result Http.Error ProjectOverview -> msg) -> Cmd msg
fetchProjectOverview apiUrl projectId toMsg =
    Http.get
        { url = apiUrl ++ "/api/v1/projects/" ++ projectId ++ "/overview"
        , expect = Http.expectJson toMsg projectOverviewDecoder
        }


fetchProjectNextTasks : String -> String -> Int -> Bool -> (Result Http.Error (List NextTaskCandidate) -> msg) -> Cmd msg
fetchProjectNextTasks apiUrl projectId limit includeBlocked toMsg =
    let
        blockedParam =
            if includeBlocked then
                "true"

            else
                "false"
    in
    Http.get
        { url = apiUrl ++ "/api/v1/projects/" ++ projectId ++ "/next-tasks?limit=" ++ String.fromInt limit ++ "&include_blocked=" ++ blockedParam
        , expect = Http.expectJson toMsg (D.list nextTaskCandidateDecoder)
        }


addTaskDependency : String -> String -> String -> String -> (Result Http.Error DependencyMutationResult -> msg) -> Cmd msg
addTaskDependency apiUrl taskId dependsOnId requestId toMsg =
    Http.request
        { method = "POST"
        , headers = [ Http.header "X-Request-Id" requestId ]
        , url = apiUrl ++ "/api/v1/tasks/" ++ taskId ++ "/dependencies"
        , body = Http.jsonBody (E.object [ ( "depends_on_id", E.string dependsOnId ), ( "request_id", E.string requestId ) ])
        , expect = Http.expectJson toMsg dependencyMutationResultDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


removeTaskDependency : String -> String -> String -> String -> (Result Http.Error DependencyMutationResult -> msg) -> Cmd msg
removeTaskDependency apiUrl taskId dependsOnId requestId toMsg =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "X-Request-Id" requestId ]
        , url = apiUrl ++ "/api/v1/tasks/" ++ taskId ++ "/dependencies/" ++ dependsOnId
        , body = Http.jsonBody (E.object [ ( "depends_on_id", E.string dependsOnId ), ( "request_id", E.string requestId ) ])
        , expect = Http.expectJson toMsg dependencyMutationResultDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


taskDependencySummaryDecoder : Decoder TaskDependencySummary
taskDependencySummaryDecoder =
    D.succeed TaskDependencySummary
        |> required "id" D.string
        |> required "name" D.string


workspaceProjectMemoryLinkDecoder : Decoder WorkspaceProjectMemoryLink
workspaceProjectMemoryLinkDecoder =
    D.succeed WorkspaceProjectMemoryLink
        |> required "project_id" D.string
        |> required "memory_id" D.string


workspaceTaskMemoryLinkDecoder : Decoder WorkspaceTaskMemoryLink
workspaceTaskMemoryLinkDecoder =
    D.succeed WorkspaceTaskMemoryLink
        |> required "task_id" D.string
        |> required "memory_id" D.string


workspaceTaskDependencyLinkDecoder : Decoder WorkspaceTaskDependencyLink
workspaceTaskDependencyLinkDecoder =
    D.succeed WorkspaceTaskDependencyLink
        |> required "task_id" D.string
        |> required "depends_on_id" D.string


taskReadinessRollupDecoder : Decoder TaskReadinessRollup
taskReadinessRollupDecoder =
    D.succeed TaskReadinessRollup
        |> optional "open_subtask_count" D.int 0
        |> optional "done_subtask_count" D.int 0
        |> optional "cancelled_subtask_count" D.int 0
        |> optional "blocked_subtask_count" D.int 0
        |> optional "dependency_blocked_task_count" D.int 0
        |> optional "open_dependency_count" D.int 0
        |> optional "completion_ready" D.bool True


defaultTaskReadinessRollup : TaskReadinessRollup
defaultTaskReadinessRollup =
    { openSubtaskCount = 0
    , doneSubtaskCount = 0
    , cancelledSubtaskCount = 0
    , blockedSubtaskCount = 0
    , dependencyBlockedTaskCount = 0
    , openDependencyCount = 0
    , completionReady = True
    }


projectReadinessRollupDecoder : Decoder ProjectReadinessRollup
projectReadinessRollupDecoder =
    D.succeed ProjectReadinessRollup
        |> optional "open_project_count" D.int 0
        |> optional "closed_project_count" D.int 0
        |> optional "open_task_count" D.int 0
        |> optional "done_task_count" D.int 0
        |> optional "cancelled_task_count" D.int 0
        |> optional "blocked_task_count" D.int 0
        |> optional "dependency_blocked_task_count" D.int 0
        |> optional "open_dependency_count" D.int 0
        |> optional "completion_ready" D.bool True


defaultProjectReadinessRollup : ProjectReadinessRollup
defaultProjectReadinessRollup =
    { openProjectCount = 0
    , closedProjectCount = 0
    , openTaskCount = 0
    , doneTaskCount = 0
    , cancelledTaskCount = 0
    , blockedTaskCount = 0
    , dependencyBlockedTaskCount = 0
    , openDependencyCount = 0
    , completionReady = True
    }


taskOverviewDecoder : Decoder TaskOverview
taskOverviewDecoder =
    D.succeed TaskOverview
        |> required "task" taskDecoder
        |> required "dependencies" (D.list taskDependencySummaryDecoder)
        |> optional "readiness_rollup" taskReadinessRollupDecoder defaultTaskReadinessRollup


projectOverviewDecoder : Decoder ProjectOverview
projectOverviewDecoder =
    D.succeed ProjectOverview
        |> required "project" projectDecoder
        |> required "tasks" (D.list taskDecoder)
        |> required "subprojects" (D.list projectDecoder)
        |> optional "readiness_rollup" projectReadinessRollupDecoder defaultProjectReadinessRollup



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
