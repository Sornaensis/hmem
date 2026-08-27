module Types exposing (..)

import Api exposing (..)
import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Http
import Json.Encode as Encode
import Time
import Url



-- FLAGS


type alias Flags =
    { apiUrl : String
    , wsUrl : String
    , sessionId : String
    , runtimeMode : String
    , authTokenStorageKey : String
    , authTokenPresent : Bool
    , loginUrl : Maybe String
    , logoutUrl : Maybe String
    }



-- MODEL


type alias Model =
    { key : Maybe Nav.Key
    , url : Url.Url
    , page : Page
    , flags : Flags
    , auth : AuthModel
    , sessionContext : Maybe Api.SessionContext
    , sessionRequestEpoch : Int
    , selectedWorkspaceId : Maybe String
    , activeTab : WorkspaceTab
    , mainContentScrollY : Float
    , workspaces : Dict String Api.Workspace
    , projects : Dict String Api.Project
    , tasks : Dict String Api.Task
    , memories : Dict String Api.Memory
    , observations : ObservationModel
    , toast : ToastModel
    , webSocket : WebSocketModel
    , dataLoading : DataLoadingModel
    , search : SearchModel
    , editing : EditingModel
    , memory : MemoryModel
    , dependencies : DependenciesModel
    , cards : CardsModel
    , dragDrop : DragDropModel
    , focus : FocusModel
    , mutations : MutationsModel
    , groups : GroupsModel
    , auditLog : AuditLogModel
    , timeline : TimelineModel
    , workspaceAdmin : WorkspaceAdminModel
    }


type alias ToastModel =
    { toasts : List Toast
    , nextToastId : Int
    }


type alias AuthModel =
    { status : AuthStatus
    , mode : Maybe String
    }


type AuthStatus
    = AuthBooting
    | AuthReady
    | AuthRequired
    | AuthFailed String


type alias WebSocketModel =
    { state : WSState
    }


type alias DataLoadingModel =
    { loadingWorkspaces : Bool
    , activeWorkspaceListLoadToken : Maybe Int
    , nextWorkspaceListLoadToken : Int
    , loadingWorkspaceData : Bool
    , pendingWorkspaceLoads : Int
    , activeWorkspaceLoadToken : Maybe Int
    , nextWorkspaceLoadToken : Int
    , cardHydrationLoaded : Bool
    }


type alias SearchRequest =
    { workspaceId : String
    , token : Int
    , query : String
    }


type alias SearchModel =
    { query : String
    , unifiedResults : Maybe Api.UnifiedSearchResults
    , isSearching : Bool
    , searchError : Maybe String
    , activeRequestQuery : Maybe String
    , activeRequest : Maybe SearchRequest
    , nextRequestToken : Int
    , filterShowOnly : FilterShowOnly
    , filterPriority : FilterPriority
    , filterProjectStatuses : List String
    , filterTaskStatuses : List String
    , filterMemoryTypes : List String
    , filterImportance : FilterPriority
    , filterMemoryPinned : Maybe Bool
    , filterMemoryActiveLinked : Bool
    , filterTags : List String
    }


type alias EditingModel =
    { editState : Maybe EditState
    , createForm : Maybe CreateForm
    , inlineCreate : Maybe InlineCreate
    }


type alias ObservationDetailRequest =
    { workspaceId : String
    , observationId : String
    , token : Int
    }


type alias ObservationModel =
    { items : Dict String Api.Observation
    , orderedIds : List String
    , hasMore : Bool
    , loading : Bool
    , error : Maybe String
    , query : String
    , subjectKind : Maybe Api.SubjectKind
    , subject : String
    , gitSha : String
    , requestMode : ObservationRequestMode
    , matchPathsInput : String
    , matchValidationError : Maybe String
    , matchEvidence : Dict String Api.ObservationMatch
    , requestGeneration : Int
    , queryFingerprint : String
    , expectedOffset : Maybe Int
    , nextOffset : Int
    , selectedId : Maybe String
    , selectedDetail : Maybe Api.Observation
    , detailLoading : Bool
    , detailError : Maybe String
    , activeDetailRequest : Maybe ObservationDetailRequest
    , nextDetailRequestToken : Int
    }


type ObservationRequestMode
    = ObservationListMode
    | ObservationMatchMode


type alias MemoryModel =
    { entityMemories : Dict String (List Api.Memory)
    , entityMemoryIds : Dict String (List String)
    , linkingMemoryFor : Maybe LinkingState
    , linkingEntityFor : Maybe LinkingState
    }


type alias DependenciesModel =
    { taskDependencies : Dict String (List Api.TaskDependencySummary)
    , taskDependencyLinks : List Api.WorkspaceTaskDependencyLink
    , taskReadinessRollups : Dict String Api.TaskReadinessRollup
    , projectReadinessRollups : Dict String Api.ProjectReadinessRollup
    , addingDependencyFor : Maybe AddDependencyState
    }


type alias CardsModel =
    { expandedCards : Dict String Bool
    , collapsedNodes : Dict String Bool
    , deleteConfirmation : Maybe DeleteConfirmation
    , lastFocusClick : Maybe FocusClick
    , projectNextTasks : Dict String (List Api.NextTaskCandidate)
    , projectNextTaskDiagnostics : Dict String (List Api.NextTaskCandidate)
    , projectNextTasksLoading : Dict String Bool
    , projectNextTaskDiagnosticsLoading : Dict String Bool
    , projectNextTasksErrors : Dict String String
    , projectNextTaskDiagnosticsErrors : Dict String String
    }


type alias CascadeDeletePreview =
    { affected : Int
    , projectCount : Int
    , taskCount : Int
    }


type alias DeleteConfirmation =
    { entityType : String
    , entityId : String
    , preview : Maybe CascadeDeletePreview
    }


type alias FocusClick =
    { entityType : String
    , entityId : String
    , timeStampMs : Float
    }


type alias DragDropModel =
    { dragging : Maybe DragInfo
    , dragOver : Maybe DragTarget
    , dropActionModal : Maybe DropActionModal
    }


type alias FocusModel =
    { focusedEntity : Maybe ( String, String )
    , breadcrumbAnchor : Maybe ( String, String )
    , history : List ( String, String )
    , historyIndex : Int
    , returnContext : Maybe FocusReturnContext
    }


type alias FocusReturnContext =
    { source : FocusReturnSource
    , workspaceId : String
    , tab : WorkspaceTab
    , entryId : String
    , label : String
    , entityType : String
    , entityId : String
    , timelineEventId : Maybe String
    , timelineSourceAuditId : Maybe String
    , timelineOccurredAt : Maybe String
    , timelineEntityFilter : Maybe TimelineEntityFilter
    , timelineEventFilter : Maybe TimelineEventFilter
    , timelineHistogramSelection : Maybe TimelineHistogramSelection
    , timelineHistogramSince : Maybe String
    , timelineHistogramUntil : Maybe String
    , timelineHistogramBucket : Maybe String
    , auditFilters : Maybe AuditLogFilters
    , auditExpandedEntryId : Maybe String
    , auditEntryExpanded : Maybe Bool
    }


type FocusReturnSource
    = ReturnFromTimeline
    | ReturnFromWorkspaceAudit
    | ReturnFromGlobalAudit


type alias MutationsModel =
    { pendingMutationIds : Dict String Bool
    , pendingRequestIds : Dict String Bool
    , nextRequestId : Int
    }


type alias GroupsModel =
    { workspaceGroups : Dict String Api.WorkspaceGroup
    , groupMembers : Dict String (List String)
    , managingGroup : Maybe ManagingGroupState
    }


type alias AuditLogModel =
    { entityHistory : Dict String (List Api.AuditLogEntry)
    , entityHistoryHasMore : Dict String Bool
    , historyExpanded : Dict String Bool
    , entries : List Api.AuditLogEntry
    , entryBaseOffset : Int
    , hasMore : Bool
    , loading : Bool
    , loadingFilters : Maybe AuditLogFilters
    , filters : AuditLogFilters
    , expandedEntries : Dict String Bool
    , revertConfirmation : Maybe Api.AuditLogEntry
    , revertInFlight : Bool
    }


type alias TimelineModel =
    { events : List Api.WorkspaceTimelineEvent
    , hasMore : Bool
    , loading : Bool
    , loadingWorkspaceId : Maybe String
    , error : Maybe String
    , loadedWorkspaceId : Maybe String
    , eventsActiveRequest : Maybe TimelineEventsRequest
    , eventsLoadedRequest : Maybe TimelineEventsRequest
    , entityFilter : TimelineEntityFilter
    , eventFilter : TimelineEventFilter
    , histogramBuckets : List Api.WorkspaceTimelineBucket
    , histogramLoading : Bool
    , histogramError : Maybe String
    , histogramSince : String
    , histogramUntil : String
    , histogramBucket : String
    , histogramClockWorkspaceId : Maybe String
    , histogramActiveRequest : Maybe TimelineHistogramRequest
    , histogramLoadedRequest : Maybe TimelineHistogramRequest
    , histogramSelectedBucket : Maybe TimelineHistogramSelection
    }


type alias TimelineEventsRequest =
    { workspaceId : String
    , since : Maybe String
    , until : Maybe String
    }


type alias TimelineHistogramRequest =
    { workspaceId : String
    , since : String
    , until : String
    , bucket : String
    }


type alias TimelineHistogramSelection =
    { label : String
    , since : String
    , until : String
    }


type TimelineEntityFilter
    = TimelineAllEntities
    | TimelineProjectsOnly
    | TimelineTasksOnly
    | TimelineSubtasksOnly


type TimelineEventFilter
    = TimelineAllEvents
    | TimelineCreatedEvents
    | TimelineCompletedEvents
    | TimelineArchivedEvents
    | TimelineCancelledEvents


type alias WorkspaceAdminModel =
    { memberships : Dict String (List Api.WorkspaceMembership)
    , loadingMemberships : Dict String Bool
    , membershipUserId : String
    , membershipRole : String
    , purgeConfirmation : Maybe String
    }


type alias AuditLogFilters =
    { workspaceId : Maybe String
    , entityType : Maybe String
    , entityId : Maybe String
    , action : Maybe String
    , since : Maybe String
    , until : Maybe String
    , limit : Maybe Int
    , offset : Maybe Int
    }


type alias DragInfo =
    { entityType : String
    , entityId : String
    }


type alias DropActionModal =
    { dragTaskId : String
    , targetTaskId : String
    }


type DragTarget
    = OverCard String
    | OverZone DropZoneInfo


type alias DropZoneInfo =
    { parentType : String
    , parentId : Maybe String
    , projectId : Maybe String
    , abovePriority : Maybe Int
    , belowPriority : Maybe Int
    }


type Page
    = HomePage
    | WorkspacePage String
    | AuditLogPage
    | NotFound


type WSState
    = Disconnected
    | Connecting
    | Connected
    | ConnectionFailed String


type alias Toast =
    { id : Int
    , message : String
    , level : ToastLevel
    }


type ToastLevel
    = Info
    | Success
    | Warning
    | Error


type WorkspaceTab
    = ProjectsTab
    | ObservationsTab
    | TimelineTab
    | AuditTab



-- INLINE EDITING


type EditState
    = EditingField
        { entityType : String
        , entityId : String
        , field : String
        , value : String
        , original : String
        }


type CreateForm
    = CreateWorkspaceForm { name : String, workspaceType : Api.WorkspaceType, ghOwner : String, ghRepo : String }
    | CreateProjectForm { name : String }
    | CreateMemoryForm { content : String, memoryType : Maybe Api.MemoryType, target : String }
    | CreateGroupForm { name : String, description : String }


type InlineCreate
    = InlineCreateProject { parentId : Maybe String, name : String }
    | InlineCreateTask { projectId : Maybe String, parentId : Maybe String, title : String }
    | InlineCreateMemory { content : String, memoryType : Maybe Api.MemoryType, target : String }


type FilterShowOnly
    = ShowAll
    | ShowProjectsOnly
    | ShowTasksOnly


type FilterPriority
    = AnyPriority
    | ExactPriority Int
    | AbovePriority Int
    | BelowPriority Int


type alias LinkingState =
    { entityType : String
    , entityId : String
    , search : String
    }


type alias AddDependencyState =
    { taskId : String
    , search : String
    }


type alias ManagingGroupState =
    { groupId : String
    , addingWorkspace : Bool
    }



-- URL ROUTING


type Route
    = HomeRoute
    | WorkspaceRoute String
    | AuditLogRoute



-- UPDATE


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
      -- WebSocket
    | WsConnectedMsg
    | WsConnectingMsg
    | WsDisconnectedMsg
    | WsConnectionFailed String
    | WsMessageReceived String
    | AuthUnauthorized
    | AuthTokenChanged Bool
    | AuthSessionError String
    | LoginRequested
    | LogoutRequested
      -- HTTP responses
    | GotWorkspaces Int (Result Http.Error (Api.PaginatedResult Api.Workspace))
    | GotWorkspace String Int (Result Http.Error Api.Workspace)
    | GotSessionContext Int (Maybe String) (Result Http.Error Api.SessionContext)
    | GotProjects String (Maybe Int) Int (Result Http.Error (Api.PaginatedResult Api.Project))
    | GotTasks String (Maybe Int) Int (Result Http.Error (Api.PaginatedResult Api.Task))
    | GotMemories String (Maybe Int) Int (Result Http.Error (Api.PaginatedResult Api.Memory))
    | GotSingleMemory (Result Http.Error Api.Memory)
    | GotObservations String (Maybe Int) Int String Int (Result Http.Error (Api.PaginatedResult Api.Observation))
    | GotObservationMatches String Int String Int (Result Http.Error (Api.PaginatedResult Api.ObservationMatch))
    | GotObservationDetail String String Int (Result Http.Error Api.Observation)
    | GotInitialTaskOverview String Int String (Result Http.Error Api.TaskOverview)
    | GotInitialProjectOverview String Int String (Result Http.Error Api.ProjectOverview)
    | GotWorkspaceTimeline TimelineEventsRequest (Result Http.Error (Api.PaginatedResult Api.WorkspaceTimelineEvent))
    | GotTimelineHistogramClock String Time.Posix
    | GotWorkspaceTimelineBuckets TimelineHistogramRequest (Result Http.Error Api.WorkspaceTimelineBucketsResponse)
      -- Mutation responses
    | MutationDone String (Result Http.Error ())
    | ProjectCreated (Result Api.ApiError Api.Project)
    | TaskCreated (Result Api.ApiError Api.Task)
    | MemoryCreated (Result Api.ApiError Api.Memory)
    | ProjectUpdated (Result Api.ApiError Api.Project)
    | TaskUpdated (Result Api.ApiError Api.TaskMutationResult)
    | MemoryUpdated (Result Http.Error Api.Memory)
    | WorkspaceUpdated (Result Http.Error Api.Workspace)
    | WorkspaceCreated (Result Http.Error Api.Workspace)
    | WorkspaceDeleted String (Result Http.Error ())
    | WorkspacePurged String (Result Http.Error ())
    | WorkspaceDeletedForPurge String (Result Http.Error ())
      -- UI
    | SelectWorkspace String
    | SwitchTab WorkspaceTab
    | DismissToast Int
    | AutoDismissToast Int
    | SearchInput String
    | SubmitSearch
    | GotUnifiedSearchResults String Int String (Result Http.Error Api.UnifiedSearchResults)
    | NavigateToSearchResult String String
    | SetFilterShowOnly FilterShowOnly
    | SetFilterPriority FilterPriority
    | ToggleFilterProjectStatus String
    | ToggleFilterTaskStatus String
    | ToggleFilterMemoryType String
    | SetFilterImportance FilterPriority
    | SetFilterMemoryPinned (Maybe Bool)
    | ToggleFilterMemoryActiveLinked Bool
    | ToggleFilterTag String
    | SetObservationQuery String
    | SetObservationSubjectKind String
    | SetObservationSubject String
    | SetObservationGitSha String
    | ApplyObservationFilters
    | SetObservationMatchPaths String
    | ApplyObservationMatch
    | ClearObservationMatch
    | LoadMoreObservations
    | SelectObservation String
    | CopyObservationSubject String
      -- Inline editing
    | StartEdit String String String String
    | EditInput String
    | SaveEdit String String
    | CancelEdit
      -- Quick-change (dropdowns, toggles)
    | ChangeProjectStatus String Api.ProjectStatus
    | ChangeTaskStatus String Api.TaskStatus
    | ChangeProjectPriority String Int
    | ChangeTaskPriority String Int
    | ChangeMemoryImportance String Int
    | ToggleMemoryPin String Bool
    | ChangeMemoryType String Api.MemoryType
      -- Tags
    | RemoveTag String String
    | AddTag String String
      -- Create forms
    | ShowCreateForm CreateForm
    | UpdateCreateForm CreateForm
    | SubmitCreateForm
    | CancelCreateForm
      -- Inline create (in-card)
    | ShowInlineCreate InlineCreate
    | UpdateInlineCreate InlineCreate
    | SubmitInlineCreate
    | CancelInlineCreate
      -- Memory linking
    | StartLinkMemory String String
    | LinkMemorySearch String
    | PerformLinkMemory String String String
    | PerformUnlinkMemory String String String
    | MemoryLinkDone String (Result Http.Error ())
    | GotEntityMemories String (Result Http.Error (List Api.Memory))
    | CancelLinkMemory
      -- Entity linking (from memory cards)
    | StartLinkEntity String
    | LinkEntitySearch String
    | PerformLinkEntity String String String
    | PerformUnlinkEntity String String String
    | CancelLinkEntity
      -- Task dependencies
    | GotTaskDependencies String (Result Http.Error Api.TaskOverview)
    | GotProjectOverview String (Result Http.Error Api.ProjectOverview)
    | GotProjectNextTasks String (Result Http.Error (List Api.NextTaskCandidate))
    | GotProjectNextTaskDiagnostics String (Result Http.Error (List Api.NextTaskCandidate))
    | RefreshProjectNextTasks String
    | StartAddDependency String
    | DependencySearch String
    | PerformAddDependency String String
    | PerformRemoveDependency String String
    | DependencyMutationDone String (Result Http.Error Api.DependencyMutationResult)
    | CancelAddDependency
      -- Navigation
    | ScrollToEntity String
      -- Card expand/collapse
    | ToggleCardExpand String
      -- Tree collapse
    | ToggleTreeNode String
    | ExpandAllNodes
    | CollapseAllNodes
    | RegisterFocusClick String String Float
      -- Expand + edit in one click
    | ExpandAndEdit String String String String String
      -- Drag and drop
    | DragStartCard String String
    | DragOverCard String
    | DragOverZone DropZoneInfo
    | DropOnCard String String
    | DropOnZone DropZoneInfo
    | DragEndCard
      -- Drop action modal
    | DropActionMakeSubtask
    | DropActionMakeDependency
    | CancelDropAction
      -- Delete
    | ConfirmDelete String String
    | PerformDelete
    | CascadeDeleteDone DeleteConfirmation (Result Api.ApiError Api.CascadeResult)
    | CancelDelete
    | CopyId String
      -- Local storage
    | LocalStorageLoaded Encode.Value
      -- Focus mode
    | FocusEntity String String
    | FocusEntityKeepForward String String
    | NavigateToAuditEntity Api.AuditLogEntry
    | NavigateToTimelineEntity Api.WorkspaceTimelineEvent
    | ReturnToFocusSource
    | FocusBreadcrumbNav Int
    | ClearFocus
    | GlobalKeyDown Int
    | ClearPendingMutation String
      -- Workspace groups
    | GotWorkspaceGroups (Result Http.Error (Api.PaginatedResult Api.WorkspaceGroup))
    | GotGroupMembers String (Result Http.Error (List String))
    | GotWorkspaceMemberships String (Result Http.Error (Api.PaginatedResult Api.WorkspaceMembership))
    | CreateWorkspaceGroup String
    | WorkspaceGroupCreated (Result Http.Error Api.WorkspaceGroup)
    | DeleteWorkspaceGroup String
    | WorkspaceGroupDeleted String (Result Http.Error ())
    | ToggleManageGroup String
    | AddWorkspaceToGroup String String
    | RemoveWorkspaceFromGroup String String
    | GroupMembershipDone String (Result Http.Error ())
    | UpdateMembershipUserId String
    | UpdateMembershipRole String
    | SubmitWorkspaceMembership String
    | WorkspaceMembershipSaved String (Result Http.Error Api.WorkspaceMembership)
    | RemoveWorkspaceMembership String String
    | WorkspaceMembershipDeleted String String (Result Http.Error ())
    | ConfirmWorkspacePurge String
    | PerformWorkspacePurge
    | CancelWorkspacePurge
    | MainContentScrolled Float
    | ClearPendingRequest String
      -- Audit log
    | GotAuditLog AuditLogFilters (Result Http.Error (Api.PaginatedResult Api.AuditLogEntry))
    | GotEntityHistory String (Result Http.Error (Api.PaginatedResult Api.AuditLogEntry))
    | SetTimelineEntityFilter TimelineEntityFilter
    | SetTimelineEventFilter TimelineEventFilter
    | SetTimelineHistogramSince String
    | SetTimelineHistogramUntil String
    | SetTimelineHistogramBucket String
    | SelectTimelineHistogramBucket String String String
    | ResetTimelineHistogramSelection
    | ToggleEntityHistory String String
    | LoadMoreHistory String String
    | SetAuditFilter String String
    | ApplyAuditFilters
    | LoadMoreAuditLog
    | ToggleAuditExpand String
      -- Revert
    | ConfirmRevert Api.AuditLogEntry
    | PerformRevert
    | CancelRevert
    | GotRevertResult String String (Result Http.Error Api.RevertResult)
    | NoOp
