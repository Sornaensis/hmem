module Types exposing (..)

import Api exposing (..)
import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Http
import Json.Encode as Encode
import Url



-- FLAGS


type alias Flags =
    { apiUrl : String
    , wsUrl : String
    }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , page : Page
    , flags : Flags
    , wsState : WSState
    , toasts : List Toast
    , nextToastId : Int

    -- Data
    , workspaces : Dict String Api.Workspace
    , projects : Dict String Api.Project
    , tasks : Dict String Api.Task
    , memories : Dict String Api.Memory
    , graphVisualization : Maybe Api.WorkspaceVisualization
    , entityMemories : Dict String (List Api.Memory)

    -- Loading states
    , loadingWorkspaces : Bool
    , loadingWorkspaceData : Bool

    -- Current workspace detail
    , selectedWorkspaceId : Maybe String
    , activeTab : WorkspaceTab

    -- Search/filter
    , searchQuery : String
    , unifiedSearchResults : Maybe Api.UnifiedSearchResults
    , isSearching : Bool
    , filterShowOnly : FilterShowOnly
    , filterPriority : FilterPriority
    , filterProjectStatuses : List String
    , filterTaskStatuses : List String

    -- Memory filters
    , filterMemoryTypes : List String
    , filterImportance : FilterPriority
    , filterMemoryPinned : Maybe Bool
    , filterTags : List String

    -- Inline editing
    , editing : Maybe EditState

    -- Create forms
    , createForm : Maybe CreateForm

    -- Inline create (in-card)
    , inlineCreate : Maybe InlineCreate

    -- Memory linking
    , linkingMemoryFor : Maybe LinkingState
    , linkingEntityFor : Maybe LinkingState

    -- Task dependencies
    , taskDependencies : Dict String (List Api.TaskDependencySummary)
    , addingDependencyFor : Maybe AddDependencyState

    -- Expanded cards
    , expandedCards : Dict String Bool

    -- Tree collapse state
    , collapsedNodes : Dict String Bool

    -- Graph page state
    , graphLoaded : Bool

    -- Drag state
    , dragging : Maybe DragInfo
    , dragOver : Maybe DragTarget

    -- Drop action modal (task-on-task)
    , dropActionModal : Maybe DropActionModal

    -- Focus mode
    , focusedEntity : Maybe ( String, String )
    , breadcrumbAnchor : Maybe ( String, String )
    , focusHistory : List ( String, String )
    , focusHistoryIndex : Int

    -- Delete confirmation
    , deleteConfirmation : Maybe ( String, String )

    -- Self-event suppression: entity IDs with recent local mutations
    , pendingMutationIds : Dict String Bool

    -- Workspace groups
    , workspaceGroups : Dict String Api.WorkspaceGroup
    , groupMembers : Dict String (List String)
    , managingGroup : Maybe ManagingGroupState

    -- Scroll tracking
    , mainContentScrollY : Float

    -- Entity history (audit log)
    , entityHistory : Dict String (List Api.AuditLogEntry)
    , entityHistoryHasMore : Dict String Bool
    , historyExpanded : Dict String Bool

    -- Audit log browser
    , auditLog : List Api.AuditLogEntry
    , auditLogHasMore : Bool
    , auditLogFilters : AuditLogFilters
    , auditLogExpanded : Dict String Bool

    -- Revert confirmation
    , revertConfirmation : Maybe Api.AuditLogEntry
    , revertInFlight : Bool
    }


type alias AuditLogFilters =
    { entityType : Maybe String
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
    | MemoryGraphPage
    | AuditLogPage
    | NotFound


type WSState
    = Disconnected
    | Connected


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
    | MemoriesTab



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
    = CreateProjectForm { name : String }
    | CreateMemoryForm { content : String, memoryType : Api.MemoryType }
    | CreateGroupForm { name : String, description : String }


type InlineCreate
    = InlineCreateProject { parentId : Maybe String, name : String }
    | InlineCreateTask { projectId : Maybe String, parentId : Maybe String, title : String }
    | InlineCreateMemory { content : String }


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
    | MemoryGraphRoute
    | AuditLogRoute



-- UPDATE


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
      -- WebSocket
    | WsConnectedMsg
    | WsDisconnectedMsg
    | WsMessageReceived String
      -- Cytoscape
    | CytoscapeNodeClicked String
    | CytoscapeEdgeClicked String
      -- HTTP responses
    | GotWorkspaces (Result Http.Error (Api.PaginatedResult Api.Workspace))
    | GotProjects (Result Http.Error (Api.PaginatedResult Api.Project))
    | GotTasks (Result Http.Error (Api.PaginatedResult Api.Task))
    | GotMemories (Result Http.Error (Api.PaginatedResult Api.Memory))
    | GotSingleMemory (Result Http.Error Api.Memory)
    | GotVisualization (Result Http.Error Api.WorkspaceVisualization)
      -- Mutation responses
    | MutationDone String (Result Http.Error ())
    | ProjectCreated (Result Http.Error Api.Project)
    | TaskCreated (Result Http.Error Api.Task)
    | MemoryCreated (Result Http.Error Api.Memory)
    | ProjectUpdated (Result Http.Error Api.Project)
    | TaskUpdated (Result Http.Error Api.Task)
    | MemoryUpdated (Result Http.Error Api.Memory)
    | WorkspaceUpdated (Result Http.Error Api.Workspace)
      -- UI
    | SelectWorkspace String
    | SwitchTab WorkspaceTab
    | DismissToast Int
    | AutoDismissToast Int
    | SearchInput String
    | SubmitSearch
    | GotUnifiedSearchResults (Result Http.Error Api.UnifiedSearchResults)
    | SetFilterShowOnly FilterShowOnly
    | SetFilterPriority FilterPriority
    | ToggleFilterProjectStatus String
    | ToggleFilterTaskStatus String
    | ToggleFilterMemoryType String
    | SetFilterImportance FilterPriority
    | SetFilterMemoryPinned (Maybe Bool)
    | ToggleFilterTag String
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
    | StartAddDependency String
    | DependencySearch String
    | PerformAddDependency String String
    | PerformRemoveDependency String String
    | DependencyMutationDone String (Result Http.Error ())
    | CancelAddDependency
      -- Navigation
    | ScrollToEntity String
      -- Card expand/collapse
    | ToggleCardExpand String
      -- Tree collapse
    | ToggleTreeNode String
    | ExpandAllNodes
    | CollapseAllNodes
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
    | CancelDelete
    | CopyId String
      -- Local storage
    | LocalStorageLoaded Encode.Value
      -- Graph workspace
    | LoadGraphForWorkspace String
      -- Focus mode
    | FocusEntity String String
    | FocusEntityKeepForward String String
    | NavigateToAuditEntity Api.AuditLogEntry
    | FocusBreadcrumbNav Int
    | ClearFocus
    | GlobalKeyDown Int
    | ClearPendingMutation String
      -- Workspace groups
    | GotWorkspaceGroups (Result Http.Error (Api.PaginatedResult Api.WorkspaceGroup))
    | GotGroupMembers String (Result Http.Error (List String))
    | CreateWorkspaceGroup String
    | WorkspaceGroupCreated (Result Http.Error Api.WorkspaceGroup)
    | DeleteWorkspaceGroup String
    | WorkspaceGroupDeleted String (Result Http.Error ())
    | ToggleManageGroup String
    | AddWorkspaceToGroup String String
    | RemoveWorkspaceFromGroup String String
    | GroupMembershipDone String (Result Http.Error ())
    | MainContentScrolled Float
      -- Audit log
    | GotAuditLog (Result Http.Error (Api.PaginatedResult Api.AuditLogEntry))
    | GotEntityHistory String (Result Http.Error (Api.PaginatedResult Api.AuditLogEntry))
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
