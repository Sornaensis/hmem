module Types exposing (..)

import Api exposing (..)
import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Feature.ChangeStream
import HierarchyViewport
import Array exposing (Array)
import ObservationViewport
import ObservationPreferences
import Http
import Json.Encode as Encode
import Set exposing (Set)
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
    , streams : Dict String Feature.ChangeStream.State
    , targetGenerations : Dict String Int
    }


type alias CanonicalRequestGuard =
    { scopeKey : String
    , targetKey : String
    , targetGeneration : Int
    , sessionEpoch : Int
    , routeWorkspace : Maybe String
    , audienceId : String
    }


type alias CanonicalNavigationRequestGuard =
    { request : CanonicalRequestGuard
    , entityGenerations : Dict String Int
    , navigationGeneration : Int
    , filterFingerprint : String
    }


type alias DataLoadingModel =
    { loadingWorkspaces : Bool
    , activeWorkspaceListLoadToken : Maybe Int
    , nextWorkspaceListLoadToken : Int
    , loadingWorkspaceData : Bool
    , pendingWorkspaceLoads : Int
    , activeWorkspaceLoadToken : Maybe Int
    , initialObservationLoad : Maybe ObservationBootstrapLoad
    , nextWorkspaceLoadToken : Int
    , cardHydrationLoaded : Bool
    , navigationGeneration : Int
    , rootNavigationRequest : Maybe NavigationBranchState
    , loadedNavigationBranches : Dict String NavigationBranchState
    , navigationQueue : List String
    , navigationAdmissions : Dict String NavigationBranchState
    , navigationPasses : Dict String NavigationPass
    , backgroundAdmission : BackgroundAdmission
    , cardDetailAdmissions : Set Int
    , cardDetailRetries : Set ( String, String )
    , visibleDetailDemand : Maybe ( Set String, Set String )
    , viewportDetailPins : Set ( String, String )
    , rootNavigationPresentation : Maybe NavigationPresentationState
    , navigationPresentations : Dict String NavigationPresentationState
    , projectCardSummaries : Dict String Api.ProjectCardSummary
    , taskCardSummaries : Dict String Api.TaskCardSummary
    , projectCardDetailRequests : Dict String CardDetailRequest
    , taskCardDetailRequests : Dict String CardDetailRequest
    , nextCardDetailRequestId : Int

    -- The bounded navigation API is authoritative for which cached cards are
    -- currently visible. Entity dictionaries may also contain detail/focus
    -- cache entries, so they cannot by themselves drive filtered tree output.
    , navigationVisibleProjectIds : Set String
    , navigationVisibleTaskIds : Set String
    , navigationVisibilityActive : Bool
    , activeNavigationFocus : Maybe NavigationFocusRequest
    , navigationFocuses : Dict String NavigationFocusRequest
    }


{-| One fair ordinary-work wave is admitted by one current painted viewport.
Physical request ledgers outlive this logical permit.
-}
type alias BackgroundAdmission =
    { workspaceId : Maybe String
    , sessionEpoch : Int
    , filterFingerprint : String
    , rootGeneration : Maybe Int
    , nonce : Int
    , acknowledged : Bool
    , remaining : Int
    , branches : Int
    , details : Int
    , branchRequests : Set String
    , detailRequests : Set Int
    }


type alias CardDetailRequest =
    { workspaceId : String
    , sessionEpoch : Int
    , navigationGeneration : Int
    , requestId : Int
    , expectedUpdatedAt : String
    , inFlight : Bool
    , succeeded : Bool
    }


{-| A fresh membership pass is independent of the displayed cache. Each kind
commits when it reaches its own end; errors keep the last authoritative cards.
-}
type alias NavigationPass =
    { refreshing : Bool
    , rootDemand : Maybe ( Int, Int )
    , projects : Dict String Api.ProjectCardSummary
    , tasks : Dict String Api.TaskCardSummary
    , projectError : Maybe String
    , taskError : Maybe String
    }


type alias NavigationBranchState =
    { workspaceId : String
    , sessionEpoch : Int
    , generation : Int
    , filterFingerprint : String
    , projectOffset : Int
    , taskOffset : Int
    , inFlight : Bool
    , succeeded : Bool
    , projectHasMore : Bool
    , taskHasMore : Bool
    , projectCardCount : Int
    , taskCardCount : Int
    , projectRequestPending : Bool
    , taskRequestPending : Bool
    }


{-| Presentation cursors are deliberately independent from transport cursors.
The server continues returning bounded 50-item data pages, while the UI moves
through cached 25-card windows without refetching overlapping data.
-}
type alias NavigationPresentationState =
    { workspaceId : String
    , sessionEpoch : Int
    , generation : Int
    , filterFingerprint : String
    , projectOffset : Int
    , taskOffset : Int
    }


type alias NavigationFocusRequest =
    { workspaceId : String
    , sessionEpoch : Int
    , generation : Int
    , filterFingerprint : String
    , entityType : String
    , entityId : String
    , ancestorOffset : Int
    , inFlight : Bool
    , succeeded : Bool
    }


type alias SearchRequest =
    { workspaceId : String
    , sessionEpoch : Int
    , token : Int
    , query : String
    }


type alias SearchModel =
    { query : String
    , submittedQuery : Maybe String
    , refreshPending : Bool
    , unifiedResults : Maybe Api.UnifiedSearchResults
    , isSearching : Bool
    , searchError : Maybe String
    , activeRequestQuery : Maybe String
    , activeRequest : Maybe SearchRequest
    , nextRequestToken : Int
    , filterShowOnly : FilterShowOnly
    , filterShowEmptyProjects : Bool
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


type alias ObservationHistoryRequest =
    { workspaceId : String, observationId : String, sessionEpoch : Int, head : Int, token : Int, offset : Int }


type alias ObservationHistoryState =
    { workspaceId : String, observationId : String, sessionEpoch : Int, head : Int
    , items : List Api.ObservationRevision, hasMore : Bool, nextOffset : Int
    , loading : Bool, error : Maybe String, active : Maybe ObservationHistoryRequest
    }


type alias ObservationDetailRequest =
    { workspaceId : String
    , observationId : String
    , sessionEpoch : Int
    , token : Int
    }


type alias ObservationMutationRequest =
    { workspaceId : String
    , observationId : String
    , sessionEpoch : Int
    , contextToken : Int
    , requestToken : Int
    }


type alias ObservationEditState =
    { workspaceId : String
    , observationId : String
    , sessionEpoch : Int
    , contextToken : Int
    , baseContent : String
    , baseContentVersion : String
    , baseUpdatedAt : String
    , draft : String
    , baseReviewedGitSha : String
    , reviewedGitShaDraft : String
    , latestCanonical : Api.Observation
    , conflict : Bool
    , saving : Bool
    , error : Maybe String
    , activeRequest : Maybe ObservationMutationRequest
    , activeCanonicalRequest : Maybe ObservationMutationRequest
    , canonicalProvisional : Bool
    }


type alias ObservationDeleteState =
    { workspaceId : String
    , observationId : String
    , sessionEpoch : Int
    , contextToken : Int
    , targetContent : String
    , deleting : Bool
    , error : Maybe String
    , activeRequest : Maybe ObservationMutationRequest
    }


type alias ObservationModel =
    { items : Dict String Api.Observation
    , counts : ObservationCountState
    , resultRows : Array ObservationResultRow
    , viewport : ObservationViewport.State
    , orderedIds : List String
    , hasMore : Bool
    , loading : Bool
    , resultsStale : Bool
    , refreshPass : Maybe ObservationRefreshPass
    , refreshPending : Bool
    , refreshError : Maybe String
    , expandedSubjects : Dict String Bool
    , preferenceOwner : Maybe ObservationPreferences.Owner
    , preferenceValue : ObservationPreferences.Preferences
    , preferenceHydrated : Bool
    , preferencePendingDetail : Bool
    , preferenceEntryHistory : Maybe String
    , preferenceTouch : Int
    , nextPreferenceRequest : Int
    , error : Maybe String
    , query : String
    , subjectKind : Maybe Api.SubjectKind
    , subject : String
    , selectedFacet : Maybe Api.ObservationSubject
    , gitSha : String
    , currentGitSha : String
    , historyGitSha : String
    , requestMode : ObservationRequestMode
    , fileComposerOpen : Bool
    , advancedFiltersOpen : Bool
    , appliedQuery : Maybe ObservationAppliedQuery
    , linkNotice : Maybe String
    , nextLinkToken : Int
    , pendingExcludedLink : Maybe ObservationLinkEcho
    , failedRequest : Maybe ObservationFailedRequest
    , matchPathsInput : String
    , matchAppliedPaths : List String
    , matchValidationError : Maybe String
    , matchEvidence : Dict String Api.ObservationMatch
    , expandedMatchGroups : Dict String Bool
    , browseReturn : Maybe ObservationBrowseReturn
    , requestGeneration : Int
    , requestSessionEpoch : Int
    , queryFingerprint : String
    , expectedOffset : Maybe Int
    , nextOffset : Int
    , facets : Dict String Api.ObservationSubjectFacet
    , facetKeys : List String
    , facetHasMore : Bool
    , facetLoading : Bool
    , facetError : Maybe String
    , facetRequestGeneration : Int
    , facetRequestSessionEpoch : Int
    , facetFingerprint : String
    , facetExpectedOffset : Maybe Int
    , facetNextOffset : Int
    , selectedId : Maybe String
    , inlineOwner : Maybe String
    , selectedDetail : Maybe Api.Observation
    , history : Maybe ObservationHistoryState
    , nextHistoryRequestToken : Int
    , detailLoading : Bool
    , detailError : Maybe String
    , activeDetailRequest : Maybe ObservationDetailRequest
    , nextDetailRequestToken : Int
    , detailNavigationEpoch : Int
    , detailNavigationToken : Int
    , detailReturnTarget : Maybe String
    , pendingReturnNavigation : Maybe ObservationReturnIntent
    , edit : Maybe ObservationEditState
    , deleteConfirmation : Maybe ObservationDeleteState
    , nextCurationContextToken : Int
    , nextMutationRequestToken : Int
    }


type alias ObservationCountGuard =
    { workspaceId : String, sessionEpoch : Int, actor : String, token : Int, generation : Int, fingerprint : String }


type alias ObservationCountState =
    { owner : Maybe ObservationCountGuard
    , active : Maybe ObservationCountGuard
    , nextToken : Int
    , generation : Int
    , pending : Bool
    , current : Bool
    , value : Maybe Api.ObservationCounts
    , valueFingerprint : String
    , settledFingerprint : String
    , error : Maybe String
    }


type alias ObservationRefreshPass =
    { query : ObservationAppliedQuery
    , targetOffset : Int
    , items : Dict String Api.Observation
    , orderedIds : List String
    , matchEvidence : Dict String Api.ObservationMatch
    , facets : Dict String Api.ObservationSubjectFacet
    , facetKeys : List String
    , invalidated : Bool
    }


type ObservationResultRow
    = ObservationCardRow String Api.Observation
    | ObservationFacetRow Api.ObservationSubjectFacet
    | ObservationPathRow String Bool
    | ObservationSubjectRow String String Api.SubjectKind String Int Bool


type alias ObservationReturnIntent =
    { intent : String
    , selectedId : Maybe String
    , workspaceId : String
    , sessionEpoch : Int
    , queryGeneration : String
    , navigationToken : Int
    , previousToken : Int
    , previousSelection : Maybe String
    , originKey : String
    , readyRevision : Maybe Int
    , fallback : Bool
    }


type alias ObservationBrowseReturn =
    { requestMode : ObservationRequestMode
    , subjectKind : Maybe Api.SubjectKind
    , subject : String
    , selectedFacet : Maybe Api.ObservationSubject
    , query : String
    , gitSha : String
    , currentGitSha : String
    , historyGitSha : String
    }


type alias ObservationAppliedQuery =
    { requestMode : ObservationRequestMode
    , query : String
    , subjectKind : Maybe Api.SubjectKind
    , subject : String
    , selectedFacet : Maybe Api.ObservationSubject
    , gitSha : String
    , currentGitSha : String
    , historyGitSha : String
    , matchAppliedPaths : List String
    }


type alias ObservationLinkEcho =
    { url : String
    , workspaceId : String
    , sessionEpoch : Int
    , generation : Int
    , facetGeneration : Int
    }


type alias ObservationBootstrapLoad =
    { workspaceId : String
    , sessionEpoch : Int
    , token : Int
    , generation : Int
    , fingerprint : String
    }


type alias ObservationFailedRequest =
    { workspaceId : String
    , sessionEpoch : Int
    , generation : Int
    , fingerprint : String
    , offset : Int
    , query : ObservationAppliedQuery
    }


type ObservationRequestMode
    = ObservationFlatMode
    | ObservationFacetMode
    | ObservationExactSubjectMode
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
    , taskDependencyHasMore : Dict String Bool
    , taskDependencyNextOffset : Dict String Int
    , taskDependencyLoading : Dict String Bool
    , taskDependencyRequests : Dict String DependencyPageRequest
    , taskDependencyRefreshItems : Dict String (List Api.TaskDependencySummary)
    , taskDependencyMutations : List DependencyMutationCorrelation
    , nextTaskDependencyRequestGeneration : Int
    }


type alias DependencyPageRequest =
    { workspaceId : String
    , sessionEpoch : Int
    , offset : Int
    , generation : Int
    }


type alias DependencyMutationCorrelation =
    { requestId : String
    , taskId : String
    , dependsOnId : String
    , action : String
    , workspaceId : String
    , sessionEpoch : Int
    , httpSucceeded : Bool
    , echoSeen : Bool
    }


type alias CardsModel =
    { viewport : HierarchyViewportState
    , expandedCards : Dict String Bool
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


type alias CardTreeProjection =
    { projects : List Api.Project
    , tasks : List Api.Task
    , projectsById : Dict String Api.Project
    , tasksById : Dict.Dict String Api.Task
    , projectChildren : Dict.Dict String (List Api.Project)
    , projectTasks : Dict.Dict String (List Api.Task)
    , taskChildren : Dict.Dict String (List Api.Task)
    , projectRollups : Dict.Dict String Api.ProjectReadinessRollup
    , taskRollups : Dict.Dict String Api.TaskReadinessRollup
    , projectAllowsOpenChildren : Dict.Dict String Bool
    , taskHasClosedAncestor : Dict.Dict String Bool
    , taskDirectOpenDependencyCounts : Dict.Dict String Int
    , projectCriteriaMatches : Set String
    , taskCriteriaMatches : Set String
    }


type alias HierarchyRow =
    { key : String
    , kind : String
    , entityId : String
    , depth : Int
    , parentKind : String
    , parentId : Maybe String
    , zone : Maybe DropZoneInfo
    }


type alias HierarchyViewportState =
    { workspaceId : Maybe String
    , sessionEpoch : Int
    , generation : Int
    , revision : Int
    , rows : Dict String HierarchyRow
    , index : HierarchyViewport.Index
    , projection : Maybe CardTreeProjection
    , top : Float
    , height : Float
    , nativePins : Set String
    , target : Maybe String
    , preserveScroll : Bool
    , filterExtent : Float
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
    , deletedWorkspaces : Set String
    , catalogueOwner : Maybe Api.SessionContext
    , catalogueEpoch : Int
    , collapsedGroups : Dict String Bool
    , workspaceDeletion : Maybe WorkspaceDeletion
    , nextDeletionToken : Int
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
    , eventsActiveIdentity : Maybe TimelineRequestIdentity
    , eventsLoadedRequest : Maybe TimelineEventsRequest
    , entityFilter : TimelineEntityFilter
    , eventFilter : TimelineEventFilter
    , histogramBuckets : List Api.WorkspaceTimelineBucket
    , histogramLoading : Bool
    , histogramError : Maybe String
    , histogramSince : String
    , histogramUntil : String
    , histogramBucket : String
    , histogramWindow : String
    , histogramClockWorkspaceId : Maybe String
    , histogramActiveRequest : Maybe TimelineHistogramRequest
    , histogramActiveIdentity : Maybe TimelineRequestIdentity
    , histogramLoadedRequest : Maybe TimelineHistogramRequest
    , histogramSelectedBucket : Maybe TimelineHistogramSelection
    , chartSeries : TimelineChartSeries
    , chartActions : Dict String Bool
    , chartPointFocus : Dict String Int
    , refreshGeneration : Int
    , refreshTimerGeneration : Maybe Int
    , refreshDirty : Bool
    , refreshEpoch : Int
    , nextRequestIdentity : Int
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


type alias TimelineRequestIdentity =
    { requestId : Int
    , refreshGeneration : Int
    , refreshEpoch : Int
    }


type alias TimelineHistogramSelection =
    { label : String
    , since : String
    , until : String
    }


type alias TimelineChartSeries =
    { projects : Bool
    , tasks : Bool
    , subtasks : Bool
    , observations : Bool
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
    , owner : Maybe MembershipOwner
    , nextRequestToken : Int
    , activeListRequest : Maybe MembershipRequestGuard
    , activeMutation : Maybe MembershipMutation
    , authorizationPending : Maybe MembershipOwner
    , authorizationFailure : Maybe String
    , membershipErrors : Dict String String
    , mutationError : Maybe String
    , purgeConfirmation : Maybe String
    }


type alias MembershipOwner =
    { workspaceId : String, sessionEpoch : Int, sessionKey : String }


type alias MembershipRequestGuard =
    { workspaceId : String, sessionEpoch : Int, sessionKey : String, token : Int }


type alias MembershipMutation =
    { guard : MembershipRequestGuard, userId : String, removing : Bool }


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
    | AdministrationTab



-- INLINE EDITING


type EditState
    = EditingField
        { entityType : String
        , entityId : String
        , field : String
        , value : String
        , original : String
        , requestId : Maybe String
        , workspaceGeneration : Maybe Int
        , error : Maybe String
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
    | ObservationViewportChanged Encode.Value
    | UrlChanged Url.Url
      -- WebSocket
    | WsConnectedMsg
    | WsConnectingMsg
    | WsDisconnectedMsg
    | WsConnectionFailed String
    | WsMessageReceived String
    | CanonicalWorkspaceFetched CanonicalRequestGuard String (Result Http.Error Api.Workspace)
    | CanonicalProjectFetched CanonicalRequestGuard String (Result Http.Error Api.Project)
    | CanonicalTaskFetched CanonicalRequestGuard String (Result Http.Error Api.Task)
    | CanonicalObservationFetched CanonicalRequestGuard String String (Result Http.Error Api.Observation)
    | CanonicalTaskOverviewFetched CanonicalRequestGuard String (Result Http.Error Api.TaskOverview)
    | CanonicalTaskReadinessFetched CanonicalRequestGuard String (Result Http.Error Api.TaskOverview)
    | CanonicalProjectOverviewFetched CanonicalRequestGuard String (Result Http.Error Api.ProjectOverview)
    | CanonicalNavigationSummariesFetched CanonicalNavigationRequestGuard String (List String) (List String) (Result Http.Error Api.NavigationSummariesResponse)
    | CanonicalCatalogueFetched CanonicalRequestGuard (Result Http.Error (Api.PaginatedResult Api.Workspace))
    | CanonicalGroupsFetched CanonicalRequestGuard (Result Http.Error (Api.PaginatedResult Api.WorkspaceGroup))
    | CanonicalGroupMembersFetched CanonicalRequestGuard String (Result Http.Error (List String))
    | CanonicalMembershipsFetched CanonicalRequestGuard String (Result Http.Error (Api.PaginatedResult Api.WorkspaceMembership))
    | CanonicalSessionFetched CanonicalRequestGuard Int (Maybe String) (Result Http.Error Api.SessionContext)
    | AuthUnauthorized
    | AuthTokenChanged Bool
    | AuthSessionError String
    | LoginRequested
    | LogoutRequested
      -- HTTP responses
    | GotWorkspaces Int (Result Http.Error (Api.PaginatedResult Api.Workspace))
    | GotWorkspace String Int (Result Http.Error Api.Workspace)
    | GotRootNavigation String Int (Maybe Int) Int String Int Int (Result Http.Error Api.NavigationBranchResponse)
    | GotNavigationBranch String Int Int String String Int Int (Result Http.Error Api.NavigationBranchResponse)
    | LoadNavigationBranchPage String String String
    | ShowPreviousNavigationBranchPage String String String
    | LoadRootNavigationPage String
    | ShowPreviousRootNavigationPage String
    | GotNavigationFocus String Int Int String String String Int (Result Http.Error Api.NavigationFocusResponse)
    | GotProjectCardDetail CardDetailRequest String (Result Http.Error Api.Project)
    | GotTaskCardDetail CardDetailRequest String (Result Http.Error Api.Task)
    | RetryCardDetail String String
    | GotSessionContext Int (Maybe String) (Result Http.Error Api.SessionContext)
    | GotProjects String (Maybe Int) Int (Result Http.Error (Api.PaginatedResult Api.Project))
    | GotTasks String (Maybe Int) Int (Result Http.Error (Api.PaginatedResult Api.Task))
    | GotMemories String (Maybe Int) Int (Result Http.Error (Api.PaginatedResult Api.Memory))
    | GotSingleMemory (Result Http.Error Api.Memory)
    | GotObservations String (Maybe Int) Int String Int (Result Http.Error (Api.PaginatedResult Api.Observation))
    | GotObservationMatches String Int Int String Int (Result Http.Error (Api.PaginatedResult Api.ObservationMatch))
    | GotObservationSubjectFacets String Int Int String Int (Result Http.Error (Api.PaginatedResult Api.ObservationSubjectFacet))
    | GotObservationDetail String String Int Int (Result Http.Error Api.Observation)
    | GotInitialTaskOverview String Int String (Result Http.Error Api.TaskOverview)
    | GotInitialProjectOverview String Int String (Result Http.Error Api.ProjectOverview)
    | GotWorkspaceTimeline TimelineRequestIdentity TimelineEventsRequest (Result Http.Error (Api.PaginatedResult Api.WorkspaceTimelineEvent))
    | GotTimelineHistogramClock String Time.Posix
    | GotWorkspaceTimelineBuckets TimelineRequestIdentity TimelineHistogramRequest (Result Http.Error Api.WorkspaceTimelineBucketsResponse)
    | RefreshTimelineAfterDebounce String Int Int
    | RetryTimelineRefresh
      -- Mutation responses
    | MutationDone String (Result Http.Error ())
    | ProjectCreated (Result Api.ApiError Api.Project)
    | TaskCreated (Result Api.ApiError Api.Task)
    | MemoryCreated (Result Api.ApiError Api.Memory)
    | ProjectUpdated (Result Api.ApiError Api.Project)
    | TaskUpdated (Result Api.ApiError Api.TaskMutationResult)
    | MemoryUpdated (Result Http.Error Api.Memory)
    | WorkspaceUpdated String (Result Api.ApiError Api.Workspace)
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
    | GotUnifiedSearchResults String Int Int String (Result Http.Error Api.UnifiedSearchResults)
    | NavigateToSearchResult String String
    | SetFilterShowOnly FilterShowOnly
    | SetFilterShowEmptyProjects Bool
    | ToggleWorkspaceGroup String
    | ConfirmWorkspaceDelete String
    | PerformWorkspaceDelete
    | CancelWorkspaceDelete
    | WorkspaceDeleteCompleted WorkspaceDeletion (Result Http.Error ())
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
    | SetObservationCurrentGitSha String
    | SetObservationHistoryGitSha String
    | ApplyObservationFilters
    | RevertObservationFilters
    | RefreshObservationResults
    | GotObservationCounts ObservationCountGuard (Result Http.Error Api.ObservationCounts)
    | RetryObservationCounts
    | RetryObservationResults
    | RetryObservationDetail
    | SetObservationBrowseMode ObservationRequestMode
    | SelectObservationFacet Api.SubjectKind String
    | SetObservationMatchPaths String
    | OpenObservationFileComposer
    | CloseObservationFileComposer
    | ToggleObservationAdvancedFilters
    | ApplyObservationMatch
    | ClearObservationMatch
    | LoadMoreObservations
    | LoadMoreObservationFacets
    | ToggleObservationMatchGroup String
    | ToggleObservationSubjects String
    | ObservationPreferencesReceived Encode.Value
    | SelectObservation String
    | SelectObservationFrom String String
    | ReturnObservationResults
    | CopyObservationSubject String
    | CopyObservationGitSha String
    | CopyObservationContent String
    | SynchronizeWorkspaceFragment
    | StartObservationEdit
    | ReturnToObservationDraft
    | SetObservationDraft String
    | SetObservationReviewedGitSha String
    | LoadObservationHistory
    | GotObservationHistory ObservationHistoryRequest (Result Http.Error (Api.PaginatedResult Api.ObservationRevision))
    | SaveObservationEdit
    | CancelObservationEdit
    | ReloadObservationEdit
    | RebaseObservationEdit
    | ObservationUpdated ObservationMutationRequest (Result Api.ObservationUpdateError Api.Observation)
    | ObservationConflictCanonicalFetched ObservationMutationRequest String (Result Http.Error Api.Observation)
    | OpenObservationDelete
    | ConfirmObservationDelete
    | CancelObservationDelete
    | ObservationDeleteDialogKeyDown String Bool String
    | ObservationDeleted ObservationMutationRequest (Result Http.Error ())
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
    | GotTaskDependencies String (Maybe String) Int Int (Result Http.Error Api.TaskOverview)
    | GotTaskDependencyPage String String Int Int Int (Result Http.Error Api.TaskDependencyPage)
    | LoadTaskDependencyPage String
    | GotProjectOverview String (Result Http.Error Api.ProjectOverview)
    | GotProjectNextTasks String (Result Http.Error (List Api.NextTaskCandidate))
    | GotProjectNextTaskDiagnostics String (Result Http.Error (List Api.NextTaskCandidate))
    | RefreshProjectNextTasks String
    | StartAddDependency String
    | DependencySearch String
    | PerformAddDependency String String
    | PerformRemoveDependency String String
    | DependencyMutationDone String String (Result Http.Error Api.DependencyMutationResult)
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
    | ClipboardResult Bool
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
    | GotWorkspaceMemberships MembershipRequestGuard (Result Http.Error (Api.PaginatedResult Api.WorkspaceMembership))
    | RetryWorkspaceMemberships String
    | RetryMembershipAuthorization
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
    | WorkspaceMembershipSaved MembershipRequestGuard (Result Http.Error Api.WorkspaceMembership)
    | RemoveWorkspaceMembership String String
    | WorkspaceMembershipDeleted MembershipRequestGuard String (Result Http.Error ())
    | ConfirmWorkspacePurge String
    | PerformWorkspacePurge
    | CancelWorkspacePurge
    | HierarchyViewportChanged Encode.Value
    | MainContentScrolled Float
    | ClearPendingRequest String
      -- Audit log
    | GotAuditLog AuditLogFilters (Result Http.Error (Api.PaginatedResult Api.AuditLogEntry))
    | GotEntityHistory String (Result Http.Error (Api.PaginatedResult Api.AuditLogEntry))
    | SetTimelineEntityFilter TimelineEntityFilter
    | SetTimelineEventFilter TimelineEventFilter
    | SetTimelineHistogramSince String
    | SetTimelineHistogramUntil String
    | SetTimelineHistogramWindow String
    | SetTimelineHistogramBucket String
    | SelectTimelineHistogramBucket String String String
    | ResetTimelineHistogramSelection
    | ToggleTimelineChartAction String
    | ToggleTimelineChartSeries String
    | FocusTimelineChartPoint String String Int
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


type alias WorkspaceDeletion =
    { workspaceId : String, token : Int, sessionKey : String, pending : Bool, error : Maybe String }
