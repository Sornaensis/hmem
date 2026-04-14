port module Main exposing (main)

import Api exposing (..)
import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Markdown.Parser
import Markdown.Renderer
import Process
import Task as ElmTask
import Url
import Url.Parser as Parser exposing (Parser, (</>))



-- PORTS: WebSocket


port connectWebSocket : String -> Cmd msg


port disconnectWebSocket : () -> Cmd msg


port sendWebSocket : String -> Cmd msg


port wsConnected : (() -> msg) -> Sub msg


port wsDisconnected : (() -> msg) -> Sub msg


port wsMessage : (String -> msg) -> Sub msg



-- PORTS: Cytoscape


port initCytoscape : Encode.Value -> Cmd msg


port destroyCytoscape : () -> Cmd msg


port updateCytoscape : Encode.Value -> Cmd msg


port cytoscapeNodeClicked : (String -> msg) -> Sub msg


port cytoscapeEdgeClicked : (String -> msg) -> Sub msg


-- PORTS: Clipboard


port copyToClipboard : String -> Cmd msg


-- PORTS: Local storage


port saveToLocalStorage : Encode.Value -> Cmd msg


port requestLocalStorage : String -> Cmd msg


port localStorageReceived : (Encode.Value -> msg) -> Sub msg



-- PORTS: Scroll


port onMainContentScroll : (Float -> msg) -> Sub msg



-- FLAGS


type alias Flags =
    { apiUrl : String
    , wsUrl : String
    }


decodeFlags : Decode.Value -> { flags : Flags, storedFilters : Maybe Decode.Value }
decodeFlags raw =
    let
        flags =
            { apiUrl =
                Decode.decodeValue (Decode.field "apiUrl" Decode.string) raw
                    |> Result.withDefault ""
            , wsUrl =
                Decode.decodeValue (Decode.field "wsUrl" Decode.string) raw
                    |> Result.withDefault ""
            }

        storedFilters =
            Decode.decodeValue (Decode.field "storedFilters" Decode.value) raw
                |> Result.toMaybe
                |> Maybe.andThen
                    (\v ->
                        -- Decode.value passes null through; filter it out
                        case Decode.decodeValue (Decode.null Nothing) v of
                            Ok Nothing ->
                                Nothing

                            _ ->
                                Just v
                    )
    in
    { flags = flags, storedFilters = storedFilters }



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



-- INIT


init : Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init rawFlags url key =
    let
        decoded =
            decodeFlags rawFlags

        flags =
            decoded.flags

        page =
            urlToPage url

        frag =
            parseFragment url.fragment

        baseModel =
            { key = key
            , url = url
            , page = page
            , flags = flags
            , wsState = Disconnected
            , toasts = []
            , nextToastId = 0
            , workspaces = Dict.empty
            , projects = Dict.empty
            , tasks = Dict.empty
            , memories = Dict.empty
            , graphVisualization = Nothing
            , entityMemories = Dict.empty
            , loadingWorkspaces = True
            , loadingWorkspaceData = False
            , selectedWorkspaceId = Nothing
            , activeTab = frag.tab
            , searchQuery = ""
            , unifiedSearchResults = Nothing
            , isSearching = False
            , filterShowOnly = ShowAll
            , filterPriority = AnyPriority
            , filterProjectStatuses = []
            , filterTaskStatuses = []
            , filterMemoryTypes = []
            , filterImportance = AnyPriority
            , filterMemoryPinned = Nothing
            , filterTags = []
            , editing = Nothing
            , createForm = Nothing
            , inlineCreate = Nothing
            , linkingMemoryFor = Nothing
            , linkingEntityFor = Nothing
            , taskDependencies = Dict.empty
            , addingDependencyFor = Nothing
            , expandedCards = Dict.empty
            , collapsedNodes = Dict.empty
            , graphLoaded = False
            , dragging = Nothing
            , dragOver = Nothing
            , dropActionModal = Nothing
            , focusedEntity = frag.focus
            , breadcrumbAnchor = frag.focus
            , focusHistory =
                case frag.focus of
                    Just f ->
                        [ f ]

                    Nothing ->
                        []
            , focusHistoryIndex = 0
            , deleteConfirmation = Nothing
            , pendingMutationIds = Dict.empty
            , workspaceGroups = Dict.empty
            , groupMembers = Dict.empty
            , managingGroup = Nothing
            , mainContentScrollY = 0
            }

        model =
            case decoded.storedFilters of
                Just json ->
                    applyStoredFilters json baseModel

                Nothing ->
                    baseModel

        cmds =
            [ connectWebSocket flags.wsUrl
            , Api.fetchWorkspaces flags.apiUrl GotWorkspaces
            , Api.fetchWorkspaceGroups flags.apiUrl GotWorkspaceGroups
            ]

        pageCmd =
            case page of
                WorkspacePage wsId ->
                    loadWorkspaceData flags.apiUrl wsId

                _ ->
                    Cmd.none
    in
    ( { model
        | selectedWorkspaceId =
            case page of
                WorkspacePage wsId ->
                    Just wsId

                _ ->
                    Nothing
        , loadingWorkspaceData =
            case page of
                WorkspacePage _ ->
                    True

                _ ->
                    False
      }
    , Cmd.batch (cmds ++ [ pageCmd ])
    )


loadWorkspaceData : String -> String -> Cmd Msg
loadWorkspaceData apiUrl wsId =
    Cmd.batch
        [ Api.fetchProjects apiUrl wsId GotProjects
        , Api.fetchTasks apiUrl wsId GotTasks
        , Api.fetchMemories apiUrl wsId GotMemories
        ]



-- URL ROUTING


type Route
    = HomeRoute
    | WorkspaceRoute String
    | MemoryGraphRoute


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map HomeRoute Parser.top
        , Parser.map WorkspaceRoute (Parser.s "workspace" </> Parser.string)
        , Parser.map MemoryGraphRoute (Parser.s "memory-graph")
        ]


urlToPage : Url.Url -> Page
urlToPage url =
    case Parser.parse routeParser { url | fragment = Nothing } of
        Just HomeRoute ->
            HomePage

        Just (WorkspaceRoute wsId) ->
            WorkspacePage wsId

        Just MemoryGraphRoute ->
            MemoryGraphPage

        Nothing ->
            NotFound


parseFragment : Maybe String -> { tab : WorkspaceTab, focus : Maybe ( String, String ) }
parseFragment fragment =
    case fragment of
        Nothing ->
            { tab = ProjectsTab, focus = Nothing }

        Just frag ->
            let
                pairs =
                    String.split "&" frag
                        |> List.filterMap
                            (\s ->
                                case String.split "=" s of
                                    [ k, v ] ->
                                        Just ( k, v )

                                    _ ->
                                        Nothing
                            )

                tabVal =
                    List.foldl
                        (\( k, v ) acc ->
                            if k == "tab" then
                                v

                            else
                                acc
                        )
                        "projects"
                        pairs

                tab =
                    if tabVal == "memories" then
                        MemoriesTab

                    else
                        ProjectsTab

                focusVal =
                    List.foldl
                        (\( k, v ) acc ->
                            if k == "focus" then
                                Just v

                            else
                                acc
                        )
                        Nothing
                        pairs

                focus =
                    focusVal
                        |> Maybe.andThen
                            (\v ->
                                case String.split ":" v of
                                    [ t, id ] ->
                                        Just ( t, id )

                                    _ ->
                                        Nothing
                            )
            in
            { tab = tab, focus = focus }


buildFragment : WorkspaceTab -> Maybe ( String, String ) -> String
buildFragment tab focus =
    let
        tabPart =
            case tab of
                ProjectsTab ->
                    "tab=projects"

                MemoriesTab ->
                    "tab=memories"

        focusPart =
            case focus of
                Just ( t, id ) ->
                    "&focus=" ++ t ++ ":" ++ id

                Nothing ->
                    ""
    in
    tabPart ++ focusPart


replaceFragment : Model -> Cmd Msg
replaceFragment model =
    case model.selectedWorkspaceId of
        Just wsId ->
            Nav.replaceUrl model.key ("/workspace/" ++ wsId ++ "#" ++ buildFragment model.activeTab model.focusedEntity)

        Nothing ->
            Cmd.none


localStorageKey : String -> String
localStorageKey wsId =
    "hmem-ws-" ++ wsId


encodeFilterState : Model -> Encode.Value
encodeFilterState model =
    Encode.object
        [ ( "searchQuery", Encode.string model.searchQuery )
        , ( "filterShowOnly"
          , Encode.string
                (case model.filterShowOnly of
                    ShowAll ->
                        "all"

                    ShowProjectsOnly ->
                        "projects"

                    ShowTasksOnly ->
                        "tasks"
                )
          )
        , ( "filterPriority", encodeFilterPriority model.filterPriority )
        , ( "filterProjectStatuses", Encode.list Encode.string model.filterProjectStatuses )
        , ( "filterTaskStatuses", Encode.list Encode.string model.filterTaskStatuses )
        , ( "filterMemoryTypes", Encode.list Encode.string model.filterMemoryTypes )
        , ( "filterImportance", encodeFilterPriority model.filterImportance )
        , ( "filterMemoryPinned"
          , case model.filterMemoryPinned of
                Just True ->
                    Encode.string "true"

                Just False ->
                    Encode.string "false"

                Nothing ->
                    Encode.null
          )
        , ( "filterTags", Encode.list Encode.string model.filterTags )
        , ( "collapsedNodes"
          , model.collapsedNodes
                |> Dict.toList
                |> List.filter (\( _, v ) -> v)
                |> List.map (\( k, _ ) -> k)
                |> Encode.list Encode.string
          )
        ]


encodeFilterPriority : FilterPriority -> Encode.Value
encodeFilterPriority fp =
    case fp of
        AnyPriority ->
            Encode.null

        ExactPriority n ->
            Encode.object [ ( "exact", Encode.int n ) ]

        AbovePriority n ->
            Encode.object [ ( "above", Encode.int n ) ]

        BelowPriority n ->
            Encode.object [ ( "below", Encode.int n ) ]


decodeFilterPriority : Decode.Decoder FilterPriority
decodeFilterPriority =
    Decode.oneOf
        [ Decode.null AnyPriority
        , Decode.map ExactPriority (Decode.field "exact" Decode.int)
        , Decode.map AbovePriority (Decode.field "above" Decode.int)
        , Decode.map BelowPriority (Decode.field "below" Decode.int)
        ]


applyStoredFilters : Encode.Value -> Model -> Model
applyStoredFilters json model =
    let
        decodeShowOnly =
            Decode.string
                |> Decode.andThen
                    (\s ->
                        case s of
                            "projects" ->
                                Decode.succeed ShowProjectsOnly

                            "tasks" ->
                                Decode.succeed ShowTasksOnly

                            _ ->
                                Decode.succeed ShowAll
                    )

        decodePinned =
            Decode.oneOf
                [ Decode.null Nothing
                , Decode.string
                    |> Decode.andThen
                        (\s ->
                            if s == "true" then
                                Decode.succeed (Just True)

                            else
                                Decode.succeed (Just False)
                        )
                ]

        decodeCollapsed =
            Decode.list Decode.string
                |> Decode.map (\ids -> Dict.fromList (List.map (\id -> ( id, True )) ids))
    in
    { model
        | searchQuery = Decode.decodeValue (Decode.field "searchQuery" Decode.string) json |> Result.withDefault model.searchQuery
        , filterShowOnly = Decode.decodeValue (Decode.field "filterShowOnly" decodeShowOnly) json |> Result.withDefault model.filterShowOnly
        , filterPriority = Decode.decodeValue (Decode.field "filterPriority" decodeFilterPriority) json |> Result.withDefault model.filterPriority
        , filterProjectStatuses = Decode.decodeValue (Decode.field "filterProjectStatuses" (Decode.list Decode.string)) json |> Result.withDefault model.filterProjectStatuses
        , filterTaskStatuses = Decode.decodeValue (Decode.field "filterTaskStatuses" (Decode.list Decode.string)) json |> Result.withDefault model.filterTaskStatuses
        , filterMemoryTypes = Decode.decodeValue (Decode.field "filterMemoryTypes" (Decode.list Decode.string)) json |> Result.withDefault model.filterMemoryTypes
        , filterImportance = Decode.decodeValue (Decode.field "filterImportance" decodeFilterPriority) json |> Result.withDefault model.filterImportance
        , filterMemoryPinned = Decode.decodeValue (Decode.field "filterMemoryPinned" decodePinned) json |> Result.withDefault model.filterMemoryPinned
        , filterTags = Decode.decodeValue (Decode.field "filterTags" (Decode.list Decode.string)) json |> Result.withDefault model.filterTags
        , collapsedNodes = Decode.decodeValue (Decode.field "collapsedNodes" decodeCollapsed) json |> Result.withDefault model.collapsedNodes
    }


saveFiltersCmd : Model -> Cmd Msg
saveFiltersCmd model =
    case model.selectedWorkspaceId of
        Just wsId ->
            saveToLocalStorage
                (Encode.object
                    [ ( "key", Encode.string (localStorageKey wsId) )
                    , ( "value", encodeFilterState model )
                    ]
                )

        Nothing ->
            Cmd.none



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
    | RevertAuditEntry String
    | AuditRevertDone (Result Http.Error Api.RevertResult)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                page =
                    urlToPage url
            in
            case page of
                WorkspacePage wsId ->
                    let
                        isCurrentWorkspace =
                            case model.page of
                                WorkspacePage currentWsId ->
                                    currentWsId == wsId

                                _ ->
                                    False
                    in
                    if isCurrentWorkspace then
                        -- Same workspace, just a hash change
                        -- Note: Nav.replaceUrl triggers onUrlChange in Elm,
                        -- so this fires after every replaceFragment call.
                        -- Only reset focusHistory when the focus actually changed
                        -- externally (browser back/forward), not from our own handlers.
                        let
                            frag =
                                parseFragment url.fragment

                            focusChangedExternally =
                                frag.focus /= model.focusedEntity
                        in
                        ( { model
                            | url = url
                            , activeTab = frag.tab
                            , focusedEntity = frag.focus
                            , breadcrumbAnchor =
                                if focusChangedExternally then
                                    frag.focus

                                else
                                    model.breadcrumbAnchor
                            , focusHistory =
                                if focusChangedExternally then
                                    case frag.focus of
                                        Just f ->
                                            [ f ]

                                        Nothing ->
                                            []

                                else
                                    model.focusHistory
                            , focusHistoryIndex =
                                if focusChangedExternally then
                                    0

                                else
                                    model.focusHistoryIndex
                            , createForm = Nothing
                            , editing = Nothing
                            , inlineCreate = Nothing
                            , linkingMemoryFor = Nothing
                            , linkingEntityFor = Nothing
                          }
                        , Cmd.none
                        )

                    else
                        let
                            frag =
                                parseFragment url.fragment

                            destroyCmd =
                                if model.page == MemoryGraphPage then
                                    destroyCytoscape ()

                                else
                                    Cmd.none
                        in
                        ( { model
                            | url = url
                            , page = page
                            , selectedWorkspaceId = Just wsId
                            , loadingWorkspaceData = True
                            , activeTab = frag.tab
                            , focusedEntity = frag.focus
                            , breadcrumbAnchor = frag.focus
                            , focusHistory =
                                case frag.focus of
                                    Just f ->
                                        [ f ]

                                    Nothing ->
                                        []
                            , focusHistoryIndex = 0
                            , projects = Dict.empty
                            , tasks = Dict.empty
                            , memories = Dict.empty
                            , editing = Nothing
                            , createForm = Nothing
                            , inlineCreate = Nothing
                            , linkingMemoryFor = Nothing
                            , entityMemories = Dict.empty
                            , taskDependencies = Dict.empty
                            , addingDependencyFor = Nothing
                            , searchQuery = ""
                            , unifiedSearchResults = Nothing
                            , isSearching = False
                            , filterShowOnly = ShowAll
                            , filterPriority = AnyPriority
                            , filterProjectStatuses = []
                            , filterTaskStatuses = []
                            , filterMemoryTypes = []
                            , filterImportance = AnyPriority
                            , filterMemoryPinned = Nothing
                            , filterTags = []
                            , collapsedNodes = Dict.empty
                            , expandedCards = Dict.empty
                            , graphLoaded = False
                            , mainContentScrollY = 0
                          }
                        , Cmd.batch
                            [ loadWorkspaceData model.flags.apiUrl wsId
                            , requestLocalStorage (localStorageKey wsId)
                            , destroyCmd
                            ]
                        )

                MemoryGraphPage ->
                    ( { model | url = url, page = page, graphLoaded = False, graphVisualization = Nothing }
                    , case model.selectedWorkspaceId of
                        Just wsId ->
                            Api.fetchVisualization model.flags.apiUrl wsId GotVisualization

                        Nothing ->
                            Cmd.none
                    )

                _ ->
                    let
                        destroyCmd =
                            if model.page == MemoryGraphPage then
                                destroyCytoscape ()

                            else
                                Cmd.none
                    in
                    ( { model | url = url, page = page }, destroyCmd )

        -- WebSocket
        WsConnectedMsg ->
            ( { model | wsState = Connected }, Cmd.none )

        WsDisconnectedMsg ->
            addToast Warning "Connection lost. Reconnecting..."
                { model | wsState = Disconnected }

        WsMessageReceived raw ->
            case Api.decodeChangeEvent raw of
                Just event ->
                    handleChangeEvent event model

                Nothing ->
                    ( model, Cmd.none )

        -- Cytoscape
        CytoscapeNodeClicked _ ->
            ( model, Cmd.none )

        CytoscapeEdgeClicked _ ->
            ( model, Cmd.none )

        -- HTTP responses
        GotWorkspaces result ->
            case result of
                Ok paginated ->
                    ( { model
                        | workspaces = indexBy .id paginated.items
                        , loadingWorkspaces = False
                      }
                    , Cmd.none
                    )

                Err _ ->
                    addToast Error "Failed to load workspaces"
                        { model | loadingWorkspaces = False }

        GotProjects result ->
            case result of
                Ok paginated ->
                    ( { model
                        | projects = indexBy .id paginated.items
                        , loadingWorkspaceData = False
                      }
                    , Cmd.none
                    )

                Err _ ->
                    addToast Error "Failed to load projects"
                        { model | loadingWorkspaceData = False }

        GotTasks result ->
            case result of
                Ok paginated ->
                    ( { model | tasks = indexBy .id paginated.items }, Cmd.none )

                Err _ ->
                    addToast Error "Failed to load tasks" model

        GotMemories result ->
            case result of
                Ok paginated ->
                    ( { model | memories = indexBy .id paginated.items }, Cmd.none )

                Err _ ->
                    addToast Error "Failed to load memories" model

        GotSingleMemory result ->
            case result of
                Ok mem ->
                    ( { model | memories = Dict.insert mem.id mem model.memories }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GotVisualization result ->
            case result of
                Ok viz ->
                    let
                        newModel =
                            { model | graphVisualization = Just viz, graphLoaded = True }
                    in
                    ( newModel, initCytoscapeGraph newModel )

                Err err ->
                    let
                        errorMsg =
                            case err of
                                Http.BadBody body ->
                                    "Decode error: " ++ body

                                Http.BadStatus code ->
                                    "HTTP " ++ String.fromInt code

                                Http.BadUrl u ->
                                    "Bad URL: " ++ u

                                Http.Timeout ->
                                    "Request timed out"

                                Http.NetworkError ->
                                    "Network error"
                    in
                    addToast Error ("Graph load failed: " ++ errorMsg) model

        -- Mutation responses
        MutationDone entityType result ->
            case result of
                Ok _ ->
                    refreshAfterMutation model

                Err _ ->
                    addToast Error ("Failed to update " ++ entityType) model

        ProjectCreated result ->
            case result of
                Ok proj ->
                    let
                        updatedModel =
                            { model
                                | projects = Dict.insert proj.id proj model.projects
                                , createForm = Nothing
                                , inlineCreate = Nothing
                            }

                        ( trackedModel, trackCmd ) =
                            trackLocalMutation proj.id updatedModel

                        ( toastedModel, toastCmd ) =
                            addToast Success ("Created project: " ++ proj.name) trackedModel
                    in
                    ( toastedModel, Cmd.batch [ trackCmd, toastCmd ] )

                Err _ ->
                    addToast Error "Failed to create project" model

        TaskCreated result ->
            case result of
                Ok task ->
                    let
                        updatedModel =
                            { model
                                | tasks = Dict.insert task.id task model.tasks
                                , createForm = Nothing
                                , inlineCreate = Nothing
                            }

                        ( trackedModel, trackCmd ) =
                            trackLocalMutation task.id updatedModel

                        ( toastedModel, toastCmd ) =
                            addToast Success ("Created task: " ++ task.title) trackedModel
                    in
                    ( toastedModel, Cmd.batch [ trackCmd, toastCmd ] )

                Err _ ->
                    addToast Error "Failed to create task" model

        MemoryCreated result ->
            case result of
                Ok mem ->
                    let
                        updatedModel =
                            { model
                                | memories = Dict.insert mem.id mem model.memories
                                , createForm = Nothing
                            }

                        ( trackedModel, trackCmd ) =
                            trackLocalMutation mem.id updatedModel

                        ( toastedModel, toastCmd ) =
                            addToast Success "Memory created" trackedModel
                    in
                    ( toastedModel, Cmd.batch [ trackCmd, toastCmd ] )

                Err _ ->
                    addToast Error "Failed to create memory" model

        ProjectUpdated result ->
            case result of
                Ok proj ->
                    let
                        ( trackedModel, trackCmd ) =
                            trackLocalMutation proj.id
                                { model | projects = Dict.insert proj.id proj model.projects }
                    in
                    ( trackedModel, trackCmd )

                Err _ ->
                    addToast Error "Failed to update project" model

        TaskUpdated result ->
            case result of
                Ok task ->
                    let
                        ( trackedModel, trackCmd ) =
                            trackLocalMutation task.id
                                { model | tasks = Dict.insert task.id task model.tasks }
                    in
                    ( trackedModel, trackCmd )

                Err _ ->
                    addToast Error "Failed to update task" model

        MemoryUpdated result ->
            case result of
                Ok mem ->
                    let
                        ( trackedModel, trackCmd ) =
                            trackLocalMutation mem.id
                                { model | memories = Dict.insert mem.id mem model.memories }
                    in
                    ( trackedModel, trackCmd )

                Err _ ->
                    addToast Error "Failed to update memory" model

        WorkspaceUpdated result ->
            case result of
                Ok ws ->
                    let
                        ( trackedModel, trackCmd ) =
                            trackLocalMutation ws.id
                                { model | workspaces = Dict.insert ws.id ws model.workspaces }
                    in
                    ( trackedModel, trackCmd )

                Err _ ->
                    addToast Error "Failed to update workspace" model

        -- UI
        SelectWorkspace wsId ->
            ( model, Nav.pushUrl model.key ("/workspace/" ++ wsId) )

        SwitchTab tab ->
            let
                newModel =
                    { model | activeTab = tab, createForm = Nothing, editing = Nothing, inlineCreate = Nothing, linkingMemoryFor = Nothing, linkingEntityFor = Nothing }
            in
            ( newModel, replaceFragment newModel )

        DismissToast toastId ->
            ( { model | toasts = List.filter (\t -> t.id /= toastId) model.toasts }, Cmd.none )

        AutoDismissToast toastId ->
            ( { model | toasts = List.filter (\t -> t.id /= toastId) model.toasts }, Cmd.none )

        SearchInput query ->
            let
                newModel =
                    if String.isEmpty query then
                        { model | searchQuery = query, unifiedSearchResults = Nothing, isSearching = False }

                    else
                        { model | searchQuery = query }
            in
            ( newModel, saveFiltersCmd newModel )

        SubmitSearch ->
            let
                trimmed =
                    String.trim model.searchQuery
            in
            if String.isEmpty trimmed then
                ( { model | unifiedSearchResults = Nothing, isSearching = False }, Cmd.none )

            else
                ( { model | isSearching = True }
                , Api.unifiedSearch model.flags.apiUrl trimmed model.selectedWorkspaceId GotUnifiedSearchResults
                )

        GotUnifiedSearchResults result ->
            case result of
                Ok results ->
                    ( { model | unifiedSearchResults = Just results, isSearching = False }, Cmd.none )

                Err _ ->
                    ( { model | isSearching = False }
                    , Cmd.none
                    )

        SetFilterShowOnly show ->
            let
                newModel =
                    { model | filterShowOnly = show }
            in
            ( newModel, saveFiltersCmd newModel )

        SetFilterPriority pri ->
            let
                newModel =
                    { model | filterPriority = pri }
            in
            ( newModel, saveFiltersCmd newModel )

        ToggleFilterProjectStatus status ->
            let
                newStatuses =
                    if List.member status model.filterProjectStatuses then
                        List.filter (\s -> s /= status) model.filterProjectStatuses

                    else
                        status :: model.filterProjectStatuses

                newModel =
                    { model | filterProjectStatuses = newStatuses }
            in
            ( newModel, saveFiltersCmd newModel )

        ToggleFilterTaskStatus status ->
            let
                newStatuses =
                    if List.member status model.filterTaskStatuses then
                        List.filter (\s -> s /= status) model.filterTaskStatuses

                    else
                        status :: model.filterTaskStatuses

                newModel =
                    { model | filterTaskStatuses = newStatuses }
            in
            ( newModel, saveFiltersCmd newModel )

        ToggleFilterMemoryType mtype ->
            let
                newTypes =
                    if List.member mtype model.filterMemoryTypes then
                        List.filter (\s -> s /= mtype) model.filterMemoryTypes

                    else
                        mtype :: model.filterMemoryTypes

                newModel =
                    { model | filterMemoryTypes = newTypes }
            in
            ( newModel, saveFiltersCmd newModel )

        SetFilterImportance imp ->
            let
                newModel =
                    { model | filterImportance = imp }
            in
            ( newModel, saveFiltersCmd newModel )

        SetFilterMemoryPinned pinned ->
            let
                newModel =
                    { model | filterMemoryPinned = pinned }
            in
            ( newModel, saveFiltersCmd newModel )

        ToggleFilterTag tag ->
            let
                newTags =
                    if List.member tag model.filterTags then
                        List.filter (\t -> t /= tag) model.filterTags

                    else
                        tag :: model.filterTags

                newModel =
                    { model | filterTags = newTags }
            in
            ( newModel, saveFiltersCmd newModel )

        -- Inline editing
        StartEdit entityType entityId field currentValue ->
            let
                saveCmd =
                    case model.editing of
                        Just (EditingField state) ->
                            if state.value /= state.original then
                                saveEditCmd model.flags.apiUrl state

                            else
                                Cmd.none

                        Nothing ->
                            Cmd.none
            in
            ( { model
                | editing =
                    Just
                        (EditingField
                            { entityType = entityType
                            , entityId = entityId
                            , field = field
                            , value = currentValue
                            , original = currentValue
                            }
                        )
              }
            , Cmd.batch [ saveCmd, focusElement (editElementId entityId field) ]
            )

        EditInput newValue ->
            case model.editing of
                Just (EditingField state) ->
                    ( { model | editing = Just (EditingField { state | value = newValue }) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SaveEdit saveEntityId saveField ->
            case model.editing of
                Just (EditingField state) ->
                    if state.entityId == saveEntityId && state.field == saveField then
                        if state.value == state.original then
                            ( { model | editing = Nothing }, Cmd.none )

                        else
                            ( { model | editing = Nothing }, saveEditCmd model.flags.apiUrl state )

                    else
                        -- Editing state has moved to a different field; don't clear it
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        CancelEdit ->
            ( { model | editing = Nothing }, Cmd.none )

        -- Quick-change handlers
        ChangeProjectStatus projId newStatus ->
            ( model
            , Api.updateProject model.flags.apiUrl
                projId
                [ ( "status", Encode.string (Api.projectStatusToString newStatus) ) ]
                ProjectUpdated
            )

        ChangeTaskStatus taskId newStatus ->
            ( model
            , Api.updateTask model.flags.apiUrl
                taskId
                [ ( "status", Encode.string (Api.taskStatusToString newStatus) ) ]
                TaskUpdated
            )

        ChangeProjectPriority projId newPri ->
            ( model
            , Api.updateProject model.flags.apiUrl
                projId
                [ ( "priority", Encode.int newPri ) ]
                ProjectUpdated
            )

        ChangeTaskPriority taskId newPri ->
            ( model
            , Api.updateTask model.flags.apiUrl
                taskId
                [ ( "priority", Encode.int newPri ) ]
                TaskUpdated
            )

        ChangeMemoryImportance memId newImp ->
            ( model
            , Api.updateMemory model.flags.apiUrl
                memId
                [ ( "importance", Encode.int newImp ) ]
                MemoryUpdated
            )

        ToggleMemoryPin memId newPinned ->
            ( model
            , Api.updateMemory model.flags.apiUrl
                memId
                [ ( "pinned", Encode.bool newPinned ) ]
                MemoryUpdated
            )

        ChangeMemoryType memId newType ->
            ( model
            , Api.updateMemory model.flags.apiUrl
                memId
                [ ( "memory_type", Encode.string (Api.memoryTypeToString newType) ) ]
                MemoryUpdated
            )

        -- Tags
        RemoveTag memId tagToRemove ->
            case Dict.get memId model.memories of
                Just mem ->
                    let
                        newTags =
                            List.filter (\t -> t /= tagToRemove) mem.tags
                    in
                    ( model, Api.setTags model.flags.apiUrl memId newTags (MutationDone "tags") )

                Nothing ->
                    ( model, Cmd.none )

        AddTag memId newTag ->
            case Dict.get memId model.memories of
                Just mem ->
                    if String.isEmpty (String.trim newTag) then
                        ( model, Cmd.none )

                    else
                        let
                            newTags =
                                mem.tags ++ [ String.trim newTag ]
                        in
                        ( { model | editing = Nothing }
                        , Api.setTags model.flags.apiUrl memId newTags (MutationDone "tags")
                        )

                Nothing ->
                    ( model, Cmd.none )

        -- Create forms
        ShowCreateForm form ->
            ( { model | createForm = Just form }, Cmd.none )

        UpdateCreateForm form ->
            ( { model | createForm = Just form }, Cmd.none )

        SubmitCreateForm ->
            case model.createForm of
                Just (CreateGroupForm f) ->
                    if String.isEmpty (String.trim f.name) then
                        ( model, Cmd.none )

                    else
                        let
                            desc =
                                if String.isEmpty (String.trim f.description) then
                                    Nothing

                                else
                                    Just (String.trim f.description)
                        in
                        ( { model | createForm = Nothing }
                        , Api.createWorkspaceGroup model.flags.apiUrl (String.trim f.name) desc WorkspaceGroupCreated
                        )

                _ ->
                    case ( model.createForm, model.selectedWorkspaceId ) of
                        ( Just (CreateProjectForm f), Just wsId ) ->
                            if String.isEmpty (String.trim f.name) then
                                ( model, Cmd.none )

                            else
                                ( model, Api.createProject model.flags.apiUrl wsId f.name ProjectCreated )

                        ( Just (CreateMemoryForm f), Just wsId ) ->
                            if String.isEmpty (String.trim f.content) then
                                ( model, Cmd.none )

                            else
                                ( model, Api.createMemory model.flags.apiUrl wsId f.content f.memoryType MemoryCreated )

                        _ ->
                            ( model, Cmd.none )

        CancelCreateForm ->
            ( { model | createForm = Nothing }, Cmd.none )

        -- Card expand/collapse
        ToggleCardExpand cardId ->
            let
                current =
                    Dict.get cardId model.expandedCards |> Maybe.withDefault False

                newExpanded =
                    not current

                fetchMemCmd =
                    if newExpanded && not (Dict.member cardId model.entityMemories) then
                        if Dict.member cardId model.projects then
                            Api.fetchProjectMemories model.flags.apiUrl cardId (GotEntityMemories cardId)

                        else if Dict.member cardId model.tasks then
                            Api.fetchTaskMemories model.flags.apiUrl cardId (GotEntityMemories cardId)

                        else
                            Cmd.none

                    else
                        Cmd.none

                fetchDepCmd =
                    if newExpanded && Dict.member cardId model.tasks && not (Dict.member cardId model.taskDependencies) then
                        Api.fetchTaskOverview model.flags.apiUrl cardId (GotTaskDependencies cardId)

                    else
                        Cmd.none
            in
            ( { model | expandedCards = Dict.insert cardId newExpanded model.expandedCards, editing = Nothing }
            , Cmd.batch [ fetchMemCmd, fetchDepCmd ]
            )

        -- Tree collapse
        ToggleTreeNode nodeId ->
            let
                current =
                    Dict.get nodeId model.collapsedNodes |> Maybe.withDefault False

                newModel =
                    { model | collapsedNodes = Dict.insert nodeId (not current) model.collapsedNodes }
            in
            ( newModel, saveFiltersCmd newModel )

        ExpandAllNodes ->
            let
                newModel =
                    { model | collapsedNodes = Dict.empty }
            in
            ( newModel, saveFiltersCmd newModel )

        CollapseAllNodes ->
            let
                allProjects =
                    Dict.values model.projects

                allTasks =
                    Dict.values model.tasks

                projectNodes =
                    allProjects
                        |> List.map (\p -> ( "proj-" ++ p.id, True ))

                taskNodes =
                    allTasks
                        |> List.filter (\t -> List.any (\t2 -> t2.parentId == Just t.id) allTasks)
                        |> List.map (\t -> ( "task-" ++ t.id, True ))

                newModel =
                    { model | collapsedNodes = Dict.fromList (projectNodes ++ taskNodes) }
            in
            ( newModel, saveFiltersCmd newModel )

        -- Delete
        ConfirmDelete entityType entityId ->
            ( { model | deleteConfirmation = Just ( entityType, entityId ) }, Cmd.none )

        PerformDelete ->
            case model.deleteConfirmation of
                Just ( entityType, entityId ) ->
                    let
                        cmd =
                            case entityType of
                                "project" ->
                                    Api.deleteProject model.flags.apiUrl entityId (MutationDone "project")

                                "task" ->
                                    Api.deleteTask model.flags.apiUrl entityId (MutationDone "task")

                                "memory" ->
                                    Api.deleteMemory model.flags.apiUrl entityId (MutationDone "memory")

                                "group" ->
                                    Api.deleteWorkspaceGroup model.flags.apiUrl entityId (WorkspaceGroupDeleted entityId)

                                _ ->
                                    Cmd.none
                    in
                    ( { model | deleteConfirmation = Nothing }, cmd )

                Nothing ->
                    ( model, Cmd.none )

        CancelDelete ->
            ( { model | deleteConfirmation = Nothing }, Cmd.none )

        CopyId idStr ->
            let
                ( m2, toastCmd ) =
                    addToast Success "ID copied to clipboard" model
            in
            ( m2, Cmd.batch [ copyToClipboard idStr, toastCmd ] )

        LocalStorageLoaded json ->
            ( applyStoredFilters json model, Cmd.none )

        LoadGraphForWorkspace wsId ->
            ( { model
                | selectedWorkspaceId = Just wsId
                , graphLoaded = False
                , graphVisualization = Nothing
              }
            , Api.fetchVisualization model.flags.apiUrl wsId GotVisualization
            )

        ExpandAndEdit cardId entityType entityId field currentValue ->
            let
                saveCmd =
                    case model.editing of
                        Just (EditingField state) ->
                            if state.value /= state.original then
                                saveEditCmd model.flags.apiUrl state

                            else
                                Cmd.none

                        Nothing ->
                            Cmd.none

                fetchMemCmd =
                    if not (Dict.member cardId model.entityMemories) then
                        if Dict.member cardId model.projects then
                            Api.fetchProjectMemories model.flags.apiUrl cardId (GotEntityMemories cardId)

                        else if Dict.member cardId model.tasks then
                            Api.fetchTaskMemories model.flags.apiUrl cardId (GotEntityMemories cardId)

                        else
                            Cmd.none

                    else
                        Cmd.none

                fetchDepCmd =
                    if Dict.member cardId model.tasks && not (Dict.member cardId model.taskDependencies) then
                        Api.fetchTaskOverview model.flags.apiUrl cardId (GotTaskDependencies cardId)

                    else
                        Cmd.none
            in
            ( { model
                | expandedCards = Dict.insert cardId True model.expandedCards
                , editing =
                    Just
                        (EditingField
                            { entityType = entityType
                            , entityId = entityId
                            , field = field
                            , value = currentValue
                            , original = currentValue
                            }
                        )
              }
            , Cmd.batch [ saveCmd, focusElement (editElementId entityId field), fetchMemCmd, fetchDepCmd ]
            )

        DragStartCard entType entId ->
            ( { model | dragging = Just { entityType = entType, entityId = entId } }, Cmd.none )

        DragOverCard targetId ->
            ( { model | dragOver = Just (OverCard targetId) }, Cmd.none )

        DragOverZone zone ->
            ( { model | dragOver = Just (OverZone zone) }, Cmd.none )

        DropOnCard targetType targetId ->
            case model.dragging of
                Just drag ->
                    if drag.entityId == targetId then
                        ( { model | dragging = Nothing, dragOver = Nothing }, Cmd.none )

                    else if drag.entityType == "task" && targetType == "project" then
                        -- Drop task/subtask on project → move as direct task in that project
                        ( { model | dragging = Nothing, dragOver = Nothing }
                        , Api.updateTask model.flags.apiUrl drag.entityId
                            [ ( "project_id", Encode.string targetId )
                            , ( "parent_id", Encode.null )
                            ]
                            TaskUpdated
                        )

                    else if drag.entityType == "project" && targetType == "project" then
                        -- Drop project on project → make it a subproject
                        ( { model | dragging = Nothing, dragOver = Nothing }
                        , Api.updateProject model.flags.apiUrl drag.entityId
                            [ ( "parent_id", Encode.string targetId ) ]
                            ProjectUpdated
                        )

                    else if drag.entityType == "task" && targetType == "task" then
                        -- Drop task on task → show modal asking Subtask or Dependency
                        ( { model
                            | dragging = Nothing
                            , dragOver = Nothing
                            , dropActionModal = Just { dragTaskId = drag.entityId, targetTaskId = targetId }
                          }
                        , Cmd.none
                        )

                    else if drag.entityType == targetType then
                        -- Same type → swap priorities
                        ( { model | dragging = Nothing, dragOver = Nothing }
                        , swapPriorities model.flags.apiUrl drag targetType targetId model
                        )

                    else
                        ( { model | dragging = Nothing, dragOver = Nothing }, Cmd.none )

                Nothing ->
                    ( { model | dragging = Nothing, dragOver = Nothing }, Cmd.none )

        DragEndCard ->
            ( { model | dragging = Nothing, dragOver = Nothing }, Cmd.none )

        DropOnZone zone ->
            case model.dragging of
                Just drag ->
                    let
                        newPriority =
                            computeDropPriority zone.abovePriority zone.belowPriority
                    in
                    case ( drag.entityType, zone.parentType ) of
                        ( "task", "project-tasks" ) ->
                            ( { model | dragging = Nothing, dragOver = Nothing }
                            , Api.updateTask model.flags.apiUrl
                                drag.entityId
                                [ ( "project_id"
                                  , case zone.projectId of
                                        Just pid ->
                                            Encode.string pid

                                        Nothing ->
                                            Encode.null
                                  )
                                , ( "parent_id", Encode.null )
                                , ( "priority", Encode.int newPriority )
                                ]
                                TaskUpdated
                            )

                        ( "task", "task-subtasks" ) ->
                            ( { model | dragging = Nothing, dragOver = Nothing }
                            , Api.updateTask model.flags.apiUrl
                                drag.entityId
                                [ ( "parent_id"
                                  , case zone.parentId of
                                        Just pid ->
                                            Encode.string pid

                                        Nothing ->
                                            Encode.null
                                  )
                                , ( "project_id"
                                  , case zone.projectId of
                                        Just pid ->
                                            Encode.string pid

                                        Nothing ->
                                            Encode.null
                                  )
                                , ( "priority", Encode.int newPriority )
                                ]
                                TaskUpdated
                            )

                        ( "task", "orphan" ) ->
                            ( { model | dragging = Nothing, dragOver = Nothing }
                            , Api.updateTask model.flags.apiUrl
                                drag.entityId
                                [ ( "project_id", Encode.null )
                                , ( "parent_id", Encode.null )
                                , ( "priority", Encode.int newPriority )
                                ]
                                TaskUpdated
                            )

                        ( "project", "project" ) ->
                            ( { model | dragging = Nothing, dragOver = Nothing }
                            , Api.updateProject model.flags.apiUrl
                                drag.entityId
                                [ ( "priority", Encode.int newPriority )
                                , ( "parent_id"
                                  , case zone.parentId of
                                        Just pid ->
                                            Encode.string pid

                                        Nothing ->
                                            Encode.null
                                  )
                                ]
                                ProjectUpdated
                            )

                        _ ->
                            ( { model | dragging = Nothing, dragOver = Nothing }, Cmd.none )

                Nothing ->
                    ( { model | dragging = Nothing, dragOver = Nothing }, Cmd.none )

        -- Drop action modal
        DropActionMakeSubtask ->
            case model.dropActionModal of
                Just modal ->
                    let
                        targetTask =
                            Dict.get modal.targetTaskId model.tasks
                    in
                    ( { model | dropActionModal = Nothing }
                    , Api.updateTask model.flags.apiUrl modal.dragTaskId
                        [ ( "parent_id", Encode.string modal.targetTaskId )
                        , ( "project_id"
                          , case targetTask |> Maybe.andThen .projectId of
                                Just pid ->
                                    Encode.string pid

                                Nothing ->
                                    Encode.null
                          )
                        ]
                        TaskUpdated
                    )

                Nothing ->
                    ( model, Cmd.none )

        DropActionMakeDependency ->
            case model.dropActionModal of
                Just modal ->
                    ( { model | dropActionModal = Nothing }
                    , Api.addTaskDependency model.flags.apiUrl modal.dragTaskId modal.targetTaskId
                        (DependencyMutationDone modal.dragTaskId)
                    )

                Nothing ->
                    ( model, Cmd.none )

        CancelDropAction ->
            ( { model | dropActionModal = Nothing }, Cmd.none )

        -- Inline create
        ShowInlineCreate ic ->
            ( { model | inlineCreate = Just ic }, focusElement "inline-create-input" )

        UpdateInlineCreate ic ->
            ( { model | inlineCreate = Just ic }, Cmd.none )

        SubmitInlineCreate ->
            case ( model.inlineCreate, model.selectedWorkspaceId ) of
                ( Just (InlineCreateProject { parentId, name }), Just wsId ) ->
                    if String.isEmpty (String.trim name) then
                        ( model, Cmd.none )

                    else
                        case parentId of
                            Just pid ->
                                ( model, Api.createProjectWithParent model.flags.apiUrl wsId pid name ProjectCreated )

                            Nothing ->
                                ( model, Api.createProject model.flags.apiUrl wsId name ProjectCreated )

                ( Just (InlineCreateTask { projectId, parentId, title }), Just wsId ) ->
                    if String.isEmpty (String.trim title) then
                        ( model, Cmd.none )

                    else
                        case parentId of
                            Just pid ->
                                ( model, Api.createTaskWithParent model.flags.apiUrl wsId projectId pid title TaskCreated )

                            Nothing ->
                                ( model, Api.createTask model.flags.apiUrl wsId projectId title TaskCreated )

                ( Just (InlineCreateMemory { content }), Just wsId ) ->
                    if String.isEmpty (String.trim content) then
                        ( model, Cmd.none )

                    else
                        ( model, Api.createMemory model.flags.apiUrl wsId content Api.ShortTerm MemoryCreated )

                _ ->
                    ( model, Cmd.none )

        CancelInlineCreate ->
            ( { model | inlineCreate = Nothing }, Cmd.none )

        -- Memory linking
        StartLinkMemory entityType entityId ->
            ( { model | linkingMemoryFor = Just { entityType = entityType, entityId = entityId, search = "" } }, Cmd.none )

        LinkMemorySearch query ->
            case model.linkingMemoryFor of
                Just st ->
                    ( { model | linkingMemoryFor = Just { st | search = query } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        CancelLinkMemory ->
            ( { model | linkingMemoryFor = Nothing }, Cmd.none )

        PerformLinkMemory entityType entityId memoryId ->
            let
                cmd =
                    case entityType of
                        "project" ->
                            Api.linkProjectMemory model.flags.apiUrl entityId memoryId (MemoryLinkDone entityId)

                        "task" ->
                            Api.linkTaskMemory model.flags.apiUrl entityId memoryId (MemoryLinkDone entityId)

                        _ ->
                            Cmd.none
            in
            ( { model | linkingMemoryFor = Nothing }, cmd )

        PerformUnlinkMemory entityType entityId memoryId ->
            let
                cmd =
                    case entityType of
                        "project" ->
                            Api.unlinkProjectMemory model.flags.apiUrl entityId memoryId (MemoryLinkDone entityId)

                        "task" ->
                            Api.unlinkTaskMemory model.flags.apiUrl entityId memoryId (MemoryLinkDone entityId)

                        _ ->
                            Cmd.none
            in
            ( model, cmd )

        MemoryLinkDone entityId result ->
            case result of
                Ok () ->
                    let
                        fetchCmd =
                            if Dict.member entityId model.projects then
                                Api.fetchProjectMemories model.flags.apiUrl entityId (GotEntityMemories entityId)

                            else if Dict.member entityId model.tasks then
                                Api.fetchTaskMemories model.flags.apiUrl entityId (GotEntityMemories entityId)

                            else
                                Cmd.none
                    in
                    ( model, fetchCmd )

                Err _ ->
                    addToast Error "Failed to update memory link" model

        GotEntityMemories entityId result ->
            case result of
                Ok mems ->
                    ( { model | entityMemories = Dict.insert entityId mems model.entityMemories }, Cmd.none )

                Err _ ->
                    addToast Error "Failed to load linked memories" model

        -- Entity linking from memory cards
        StartLinkEntity memoryId ->
            ( { model | linkingEntityFor = Just { entityType = "memory", entityId = memoryId, search = "" } }, Cmd.none )

        LinkEntitySearch query ->
            case model.linkingEntityFor of
                Just st ->
                    ( { model | linkingEntityFor = Just { st | search = query } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        CancelLinkEntity ->
            ( { model | linkingEntityFor = Nothing }, Cmd.none )

        PerformLinkEntity entityType entityId memoryId ->
            let
                cmd =
                    case entityType of
                        "project" ->
                            Api.linkProjectMemory model.flags.apiUrl entityId memoryId (MemoryLinkDone entityId)

                        "task" ->
                            Api.linkTaskMemory model.flags.apiUrl entityId memoryId (MemoryLinkDone entityId)

                        _ ->
                            Cmd.none
            in
            ( { model | linkingEntityFor = Nothing }, cmd )

        PerformUnlinkEntity entityType entityId memoryId ->
            let
                cmd =
                    case entityType of
                        "project" ->
                            Api.unlinkProjectMemory model.flags.apiUrl entityId memoryId (MemoryLinkDone entityId)

                        "task" ->
                            Api.unlinkTaskMemory model.flags.apiUrl entityId memoryId (MemoryLinkDone entityId)

                        _ ->
                            Cmd.none
            in
            ( model, cmd )

        -- Task dependencies
        GotTaskDependencies taskId result ->
            case result of
                Ok overview ->
                    ( { model | taskDependencies = Dict.insert taskId overview.dependencies model.taskDependencies }, Cmd.none )

                Err _ ->
                    addToast Error "Failed to load task dependencies" model

        StartAddDependency taskId ->
            ( { model | addingDependencyFor = Just { taskId = taskId, search = "" } }, Cmd.none )

        DependencySearch query ->
            case model.addingDependencyFor of
                Just st ->
                    ( { model | addingDependencyFor = Just { st | search = query } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        CancelAddDependency ->
            ( { model | addingDependencyFor = Nothing }, Cmd.none )

        PerformAddDependency taskId dependsOnId ->
            ( { model | addingDependencyFor = Nothing }
            , Api.addTaskDependency model.flags.apiUrl taskId dependsOnId (DependencyMutationDone taskId)
            )

        PerformRemoveDependency taskId dependsOnId ->
            ( model
            , Api.removeTaskDependency model.flags.apiUrl taskId dependsOnId (DependencyMutationDone taskId)
            )

        DependencyMutationDone taskId result ->
            case result of
                Ok () ->
                    ( model, Api.fetchTaskOverview model.flags.apiUrl taskId (GotTaskDependencies taskId) )

                Err _ ->
                    addToast Error "Failed to update dependency" model

        ScrollToEntity entityId ->
            ( { model
                | expandedCards = Dict.insert entityId True model.expandedCards
              }
            , scrollToElement ("entity-" ++ entityId)
            )

        FocusEntity entityType entityId ->
            let
                entry =
                    ( entityType, entityId )

                -- Truncate any forward history, then append
                newHistory =
                    List.take (model.focusHistoryIndex + 1) model.focusHistory ++ [ entry ]

                newIndex =
                    List.length newHistory - 1

                newModel =
                    { model
                        | focusedEntity = Just entry
                        , breadcrumbAnchor = Just entry
                        , focusHistory = newHistory
                        , focusHistoryIndex = newIndex
                    }
            in
            ( newModel, replaceFragment newModel )

        FocusEntityKeepForward entityType entityId ->
            let
                entry =
                    ( entityType, entityId )

                -- Only change what's focused, don't modify history
                newModel =
                    { model | focusedEntity = Just entry }
            in
            ( newModel, replaceFragment newModel )

        FocusBreadcrumbNav idx ->
            let
                entry =
                    model.focusHistory
                        |> List.drop idx
                        |> List.head

                newModel =
                    { model
                        | focusedEntity = entry
                        , breadcrumbAnchor = entry
                        , focusHistoryIndex = idx
                    }
            in
            ( newModel, replaceFragment newModel )

        ClearFocus ->
            let
                newModel =
                    { model
                        | focusedEntity = Nothing
                        , breadcrumbAnchor = Nothing
                        , focusHistory = []
                        , focusHistoryIndex = 0
                    }
            in
            ( newModel, replaceFragment newModel )

        GlobalKeyDown keyCode ->
            if keyCode == 27 then
                -- Escape: dismiss any active popup/edit, inner-most first
                if model.deleteConfirmation /= Nothing then
                    ( { model | deleteConfirmation = Nothing }, Cmd.none )

                else if model.dropActionModal /= Nothing then
                    ( { model | dropActionModal = Nothing }, Cmd.none )

                else if model.addingDependencyFor /= Nothing then
                    ( { model | addingDependencyFor = Nothing }, Cmd.none )

                else if model.linkingMemoryFor /= Nothing then
                    ( { model | linkingMemoryFor = Nothing }, Cmd.none )

                else if model.linkingEntityFor /= Nothing then
                    ( { model | linkingEntityFor = Nothing }, Cmd.none )

                else if model.inlineCreate /= Nothing then
                    ( { model | inlineCreate = Nothing }, Cmd.none )

                else if model.editing /= Nothing then
                    ( { model | editing = Nothing }, Cmd.none )

                else
                    ( model, Cmd.none )

            else
                ( model, Cmd.none )

        GotAuditLog result ->
            case result of
                Ok _ ->
                    ( model, Cmd.none )

                Err _ ->
                    addToast Error "Failed to load audit log" model

        GotEntityHistory _ result ->
            case result of
                Ok _ ->
                    ( model, Cmd.none )

                Err _ ->
                    addToast Error "Failed to load entity history" model

        RevertAuditEntry auditId ->
            ( model, Api.revertAuditEntry model.flags.apiUrl auditId AuditRevertDone )

        AuditRevertDone result ->
            case result of
                Ok _ ->
                    addToast Success "Change reverted successfully" model

                Err _ ->
                    addToast Error "Failed to revert change" model

        NoOp ->
            ( model, Cmd.none )

        MainContentScrolled scrollY ->
            ( { model | mainContentScrollY = scrollY }, Cmd.none )

        ClearPendingMutation entityId ->
            ( { model | pendingMutationIds = Dict.remove entityId model.pendingMutationIds }
            , Cmd.none
            )

        -- Workspace groups
        GotWorkspaceGroups result ->
            case result of
                Ok paginated ->
                    let
                        groups =
                            indexBy .id paginated.items

                        fetchMembersCmds =
                            paginated.items
                                |> List.map (\g -> Api.fetchGroupMembers model.flags.apiUrl g.id (GotGroupMembers g.id))
                    in
                    ( { model | workspaceGroups = groups }
                    , Cmd.batch fetchMembersCmds
                    )

                Err _ ->
                    addToast Error "Failed to load workspace groups" model

        GotGroupMembers groupId result ->
            case result of
                Ok memberIds ->
                    ( { model | groupMembers = Dict.insert groupId memberIds model.groupMembers }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        CreateWorkspaceGroup name ->
            ( model
            , Api.createWorkspaceGroup model.flags.apiUrl name Nothing WorkspaceGroupCreated
            )

        WorkspaceGroupCreated result ->
            case result of
                Ok group ->
                    addToast Success ("Group \"" ++ group.name ++ "\" created")
                        { model | workspaceGroups = Dict.insert group.id group model.workspaceGroups }

                Err _ ->
                    addToast Error "Failed to create workspace group" model

        DeleteWorkspaceGroup groupId ->
            ( model
            , Api.deleteWorkspaceGroup model.flags.apiUrl groupId (WorkspaceGroupDeleted groupId)
            )

        WorkspaceGroupDeleted groupId result ->
            case result of
                Ok () ->
                    addToast Success "Group deleted"
                        { model
                            | workspaceGroups = Dict.remove groupId model.workspaceGroups
                            , groupMembers = Dict.remove groupId model.groupMembers
                            , managingGroup =
                                case model.managingGroup of
                                    Just st ->
                                        if st.groupId == groupId then
                                            Nothing

                                        else
                                            model.managingGroup

                                    Nothing ->
                                        Nothing
                        }

                Err _ ->
                    addToast Error "Failed to delete group" model

        ToggleManageGroup groupId ->
            case model.managingGroup of
                Just st ->
                    if st.groupId == groupId then
                        ( { model | managingGroup = Nothing }, Cmd.none )

                    else
                        ( { model | managingGroup = Just { groupId = groupId, addingWorkspace = False } }, Cmd.none )

                Nothing ->
                    ( { model | managingGroup = Just { groupId = groupId, addingWorkspace = False } }, Cmd.none )

        AddWorkspaceToGroup groupId workspaceId ->
            ( model
            , Api.addGroupMember model.flags.apiUrl groupId workspaceId (GroupMembershipDone groupId)
            )

        RemoveWorkspaceFromGroup groupId workspaceId ->
            ( model
            , Api.removeGroupMember model.flags.apiUrl groupId workspaceId (GroupMembershipDone groupId)
            )

        GroupMembershipDone groupId result ->
            case result of
                Ok () ->
                    ( model
                    , Api.fetchGroupMembers model.flags.apiUrl groupId (GotGroupMembers groupId)
                    )

                Err _ ->
                    addToast Error "Failed to update group membership" model


{-| Mark an entity ID as recently mutated locally. Returns the updated model
and a Cmd that clears the flag after 3 seconds.
-}
trackLocalMutation : String -> Model -> ( Model, Cmd Msg )
trackLocalMutation entityId model =
    ( { model | pendingMutationIds = Dict.insert entityId True model.pendingMutationIds }
    , Process.sleep 3000 |> ElmTask.perform (\_ -> ClearPendingMutation entityId)
    )


saveEditCmd : String -> { entityType : String, entityId : String, field : String, value : String, original : String } -> Cmd Msg
saveEditCmd apiUrl state =
    case state.entityType of
        "workspace" ->
            Api.updateWorkspace apiUrl
                state.entityId
                [ ( state.field, Encode.string state.value ) ]
                WorkspaceUpdated

        "project" ->
            Api.updateProject apiUrl
                state.entityId
                [ ( state.field, Encode.string state.value ) ]
                ProjectUpdated

        "task" ->
            Api.updateTask apiUrl
                state.entityId
                [ ( state.field, Encode.string state.value ) ]
                TaskUpdated

        "memory" ->
            Api.updateMemory apiUrl
                state.entityId
                [ ( state.field, Encode.string state.value ) ]
                MemoryUpdated

        _ ->
            Cmd.none


refreshAfterMutation : Model -> ( Model, Cmd Msg )
refreshAfterMutation model =
    case model.selectedWorkspaceId of
        Just wsId ->
            ( model, loadWorkspaceData model.flags.apiUrl wsId )

        Nothing ->
            ( model, Cmd.none )


handleChangeEvent : Api.ChangeEvent -> Model -> ( Model, Cmd Msg )
handleChangeEvent event model =
    let
        ( updatedModel, refreshCmd ) =
            applyChangeEvent event model

        isSelfEvent =
            Dict.member event.entityId model.pendingMutationIds
    in
    if isSelfEvent then
        -- Self-event: apply data (harmless upsert) but skip the toast
        ( updatedModel, refreshCmd )

    else
        let
            toastMsg =
                changeEventDescription event

            ( toastedModel, toastCmd ) =
                addToast Info toastMsg updatedModel
        in
        ( toastedModel, Cmd.batch [ refreshCmd, toastCmd ] )


{-| Try to apply the change event payload directly into the model.
Falls back to a full re-fetch when the payload is missing or cannot be decoded.
-}
applyChangeEvent : Api.ChangeEvent -> Model -> ( Model, Cmd Msg )
applyChangeEvent event model =
    case event.entityType of
        Api.EWorkspace ->
            case event.changeType of
                Api.Deleted ->
                    ( { model | workspaces = Dict.remove event.entityId model.workspaces }
                    , Cmd.none
                    )

                _ ->
                    case Maybe.andThen (tryDecode Api.workspaceDecoder) event.payload of
                        Just ws ->
                            ( { model | workspaces = Dict.insert ws.id ws model.workspaces }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model
                            , Api.fetchWorkspaces model.flags.apiUrl GotWorkspaces
                            )

        Api.EProject ->
            case event.changeType of
                Api.Deleted ->
                    ( { model | projects = Dict.remove event.entityId model.projects }
                    , Cmd.none
                    )

                _ ->
                    case Maybe.andThen (tryDecode Api.projectDecoder) event.payload of
                        Just proj ->
                            ( { model | projects = Dict.insert proj.id proj model.projects }
                            , Cmd.none
                            )

                        Nothing ->
                            withWorkspace model <|
                                \wsId -> Api.fetchProjects model.flags.apiUrl wsId GotProjects

        Api.ETask ->
            case event.changeType of
                Api.Deleted ->
                    ( { model | tasks = Dict.remove event.entityId model.tasks }
                    , Cmd.none
                    )

                _ ->
                    case Maybe.andThen (tryDecode Api.taskDecoder) event.payload of
                        Just task ->
                            ( { model | tasks = Dict.insert task.id task model.tasks }
                            , Cmd.none
                            )

                        Nothing ->
                            withWorkspace model <|
                                \wsId -> Api.fetchTasks model.flags.apiUrl wsId GotTasks

        Api.EMemory ->
            case event.changeType of
                Api.Deleted ->
                    ( { model | memories = Dict.remove event.entityId model.memories }
                    , Cmd.none
                    )

                _ ->
                    case Maybe.andThen (tryDecode Api.memoryDecoder) event.payload of
                        Just mem ->
                            ( { model | memories = Dict.insert mem.id mem model.memories }
                            , Cmd.none
                            )

                        Nothing ->
                            withWorkspace model <|
                                \wsId -> Api.fetchMemories model.flags.apiUrl wsId GotMemories

        Api.EMemoryLink ->
            -- Memory links affect the graph view; refresh visualization if on graph page
            withWorkspace model <|
                \wsId ->
                    case model.page of
                        MemoryGraphPage ->
                            Api.fetchVisualization model.flags.apiUrl wsId GotVisualization

                        _ ->
                            Cmd.none

        Api.ECategory ->
            -- Categories don't have a dedicated Dict yet; refresh memories
            -- since category changes can affect memory display
            withWorkspace model <|
                \wsId -> Api.fetchMemories model.flags.apiUrl wsId GotMemories

        Api.ETaskDependency ->
            -- Refresh task dependencies for the affected task
            ( model
            , Api.fetchTaskOverview model.flags.apiUrl event.entityId (GotTaskDependencies event.entityId)
            )

        Api.ECategoryLink ->
            -- Category-memory link changed; refresh memories
            withWorkspace model <|
                \wsId -> Api.fetchMemories model.flags.apiUrl wsId GotMemories

        Api.ETag ->
            -- Tags changed on a memory; refresh that memory
            ( model
            , Api.fetchMemory model.flags.apiUrl event.entityId GotSingleMemory
            )

        Api.EWorkspaceGroup ->
            -- Workspace groups affect the sidebar; refresh groups and workspaces
            ( model
            , Cmd.batch
                [ Api.fetchWorkspaces model.flags.apiUrl GotWorkspaces
                , Api.fetchWorkspaceGroups model.flags.apiUrl GotWorkspaceGroups
                ]
            )

        Api.ESavedView ->
            -- Saved views are not currently rendered in the main UI; no-op
            ( model, Cmd.none )

        Api.EOther _ ->
            ( model, Cmd.none )


{-| Helper: run a command that needs a workspace ID, or no-op if none selected.
-}
withWorkspace : Model -> (String -> Cmd Msg) -> ( Model, Cmd Msg )
withWorkspace model mkCmd =
    case model.selectedWorkspaceId of
        Just wsId ->
            ( model, mkCmd wsId )

        Nothing ->
            ( model, Cmd.none )


{-| Try to decode a JSON Value using a given decoder.
-}
tryDecode : Decode.Decoder a -> Decode.Value -> Maybe a
tryDecode decoder val =
    Result.toMaybe (Decode.decodeValue decoder val)


changeEventDescription : Api.ChangeEvent -> String
changeEventDescription event =
    let
        action =
            case event.changeType of
                Api.Created ->
                    "created"

                Api.Updated ->
                    "updated"

                Api.Deleted ->
                    "deleted"

        entity =
            case event.entityType of
                Api.EWorkspace ->
                    "Workspace"

                Api.EProject ->
                    "Project"

                Api.ETask ->
                    "Task"

                Api.EMemory ->
                    "Memory"

                Api.EMemoryLink ->
                    "Memory link"

                Api.ECategory ->
                    "Category"

                Api.EWorkspaceGroup ->
                    "Workspace group"

                Api.ESavedView ->
                    "Saved view"

                Api.ETaskDependency ->
                    "Task dependency"

                Api.ECategoryLink ->
                    "Category link"

                Api.ETag ->
                    "Tags"

                Api.EOther s ->
                    s
    in
    entity ++ " " ++ action



-- HELPERS


addToast : ToastLevel -> String -> Model -> ( Model, Cmd Msg )
addToast level message model =
    let
        tid =
            model.nextToastId

        dismissDelay =
            case level of
                Error ->
                    8000

                Warning ->
                    6000

                _ ->
                    4000
    in
    ( { model
        | toasts = model.toasts ++ [ Toast tid message level ]
        , nextToastId = tid + 1
      }
    , ElmTask.perform (\_ -> AutoDismissToast tid) (Process.sleep dismissDelay)
    )


indexBy : (a -> comparable) -> List a -> Dict comparable a
indexBy key items =
    List.foldl (\item acc -> Dict.insert (key item) item acc) Dict.empty items


editElementId : String -> String -> String
editElementId entityId field =
    "edit-" ++ entityId ++ "-" ++ field


focusElement : String -> Cmd Msg
focusElement elemId =
    Browser.Dom.focus elemId
        |> ElmTask.attempt (\_ -> NoOp)


scrollToElement : String -> Cmd Msg
scrollToElement elemId =
    Browser.Dom.getElement elemId
        |> ElmTask.andThen
            (\info ->
                Browser.Dom.setViewportOf "main-content-scroll" 0 (info.element.y - 100)
            )
        |> ElmTask.attempt (\_ -> NoOp)


formatDate : String -> String
formatDate dateStr =
    String.left 10 dateStr


truncateId : String -> String
truncateId id =
    String.left 8 id ++ "..."


dragOverClass : Model -> String -> String
dragOverClass model entityId =
    case ( model.dragOver, model.dragging ) of
        ( Just (OverCard overId), Just drag ) ->
            if overId == entityId && drag.entityId /= entityId then
                -- Task or project dragged over a project → green "move" indicator
                if (drag.entityType == "task" || drag.entityType == "project") && Dict.member entityId model.projects then
                    " drag-over drag-over-move"

                else
                    " drag-over"

            else
                ""

        _ ->
            ""


swapPriorities : String -> DragInfo -> String -> String -> Model -> Cmd Msg
swapPriorities apiUrl drag targetType targetId model =
    case drag.entityType of
        "project" ->
            let
                dragPri =
                    Dict.get drag.entityId model.projects |> Maybe.map .priority |> Maybe.withDefault 5

                targetPri =
                    Dict.get targetId model.projects |> Maybe.map .priority |> Maybe.withDefault 5
            in
            Cmd.batch
                [ Api.updateProject apiUrl drag.entityId [ ( "priority", Encode.int targetPri ) ] ProjectUpdated
                , Api.updateProject apiUrl targetId [ ( "priority", Encode.int dragPri ) ] ProjectUpdated
                ]

        "task" ->
            let
                dragPri =
                    Dict.get drag.entityId model.tasks |> Maybe.map .priority |> Maybe.withDefault 5

                targetPri =
                    Dict.get targetId model.tasks |> Maybe.map .priority |> Maybe.withDefault 5
            in
            Cmd.batch
                [ Api.updateTask apiUrl drag.entityId [ ( "priority", Encode.int targetPri ) ] TaskUpdated
                , Api.updateTask apiUrl targetId [ ( "priority", Encode.int dragPri ) ] TaskUpdated
                ]

        "memory" ->
            let
                dragImp =
                    Dict.get drag.entityId model.memories |> Maybe.map .importance |> Maybe.withDefault 5

                targetImp =
                    Dict.get targetId model.memories |> Maybe.map .importance |> Maybe.withDefault 5
            in
            Cmd.batch
                [ Api.updateMemory apiUrl drag.entityId [ ( "importance", Encode.int targetImp ) ] MemoryUpdated
                , Api.updateMemory apiUrl targetId [ ( "importance", Encode.int dragImp ) ] MemoryUpdated
                ]

        _ ->
            Cmd.none


collectDescendantProjectIds : List Api.Project -> String -> List String
collectDescendantProjectIds allProjects parentId =
    let
        directChildren =
            List.filter (\p -> p.parentId == Just parentId) allProjects
    in
    parentId :: List.concatMap (\c -> collectDescendantProjectIds allProjects c.id) directChildren


computeDropPriority : Maybe Int -> Maybe Int -> Int
computeDropPriority abovePri belowPri =
    case ( abovePri, belowPri ) of
        ( Just a, Just b ) ->
            (a + b) // 2

        ( Just a, Nothing ) ->
            Basics.max 0 (a - 1)

        ( Nothing, Just b ) ->
            Basics.min 10 (b + 1)

        ( Nothing, Nothing ) ->
            5


viewDropZone : Model -> DropZoneInfo -> Html Msg
viewDropZone model zone =
    case model.dragging of
        Nothing ->
            text ""

        Just drag ->
            let
                relevant =
                    case ( drag.entityType, zone.parentType ) of
                        ( "task", "project-tasks" ) ->
                            True

                        ( "task", "task-subtasks" ) ->
                            True

                        ( "task", "orphan" ) ->
                            True

                        ( "project", "project" ) ->
                            True

                        _ ->
                            False

                isActive =
                    case model.dragOver of
                        Just (OverZone z) ->
                            z == zone

                        _ ->
                            False
            in
            if relevant then
                div
                    [ class
                        (if isActive then
                            "drop-zone drop-zone-active"

                         else
                            "drop-zone"
                        )
                    , preventDefaultOn "dragover" (Decode.succeed ( DragOverZone zone, True ))
                    , preventDefaultOn "drop" (Decode.succeed ( DropOnZone zone, True ))
                    ]
                    []

            else
                text ""


viewTasksWithZones : Model -> String -> Maybe String -> Maybe String -> List Api.Task -> List ( String, Html Msg )
viewTasksWithZones model zoneType projectId parentTaskId tasks =
    case model.dragging of
        Nothing ->
            List.map (\t -> ( t.id, viewTaskCard False model t )) tasks

        Just _ ->
            let
                makeZone abovePri belowPri =
                    { parentType = zoneType
                    , parentId = parentTaskId
                    , projectId = projectId
                    , abovePriority = abovePri
                    , belowPriority = belowPri
                    }

                go remaining idx prevPri =
                    case remaining of
                        [] ->
                            [ ( "dz-" ++ zoneType ++ "-end", viewDropZone model (makeZone prevPri Nothing) ) ]

                        t :: rest ->
                            ( "dz-" ++ zoneType ++ "-" ++ String.fromInt idx, viewDropZone model (makeZone prevPri (Just t.priority)) )
                                :: ( t.id, viewTaskCard False model t )
                                :: go rest (idx + 1) (Just t.priority)
            in
            go tasks 0 Nothing


viewProjectsWithZones : Model -> (Api.Project -> Html Msg) -> Maybe String -> List Api.Project -> List ( String, Html Msg )
viewProjectsWithZones model renderProject parentId projects =
    case model.dragging of
        Nothing ->
            List.map (\p -> ( p.id, renderProject p )) projects

        Just _ ->
            let
                makeZone abovePri belowPri =
                    { parentType = "project"
                    , parentId = parentId
                    , projectId = Nothing
                    , abovePriority = abovePri
                    , belowPriority = belowPri
                    }

                go remaining idx prevPri =
                    case remaining of
                        [] ->
                            [ ( "dz-proj-end", viewDropZone model (makeZone prevPri Nothing) ) ]

                        p :: rest ->
                            ( "dz-proj-" ++ String.fromInt idx, viewDropZone model (makeZone prevPri (Just p.priority)) )
                                :: ( p.id, renderProject p )
                                :: go rest (idx + 1) (Just p.priority)
            in
            go projects 0 Nothing


isExpanded : Model -> String -> Bool
isExpanded model cardId =
    Dict.get cardId model.expandedCards |> Maybe.withDefault False


isCollapsed : Model -> String -> Bool
isCollapsed model nodeId =
    Dict.get nodeId model.collapsedNodes |> Maybe.withDefault False


editingValue : Model -> String -> String -> Maybe String
editingValue model entityId field =
    case model.editing of
        Just (EditingField state) ->
            if state.entityId == entityId && state.field == field then
                Just state.value

            else
                Nothing

        Nothing ->
            Nothing



-- CYTOSCAPE GRAPH


graphPositionsKey : String -> String
graphPositionsKey wsId =
    "hmem-graph-positions-" ++ wsId


initCytoscapeGraph : Model -> Cmd Msg
initCytoscapeGraph model =
    case model.graphVisualization of
        Nothing ->
            Cmd.none

        Just viz ->
            let
                projectNodes =
                    viz.projects
                        |> List.map
                            (\p ->
                                Encode.object
                                    [ ( "data"
                                      , Encode.object
                                            [ ( "id", Encode.string p.id )
                                            , ( "label", Encode.string (truncate 40 p.name) )
                                            ]
                                      )
                                    , ( "classes", Encode.string "project" )
                                    ]
                            )

                taskNodes =
                    viz.tasks
                        |> List.map
                            (\t ->
                                Encode.object
                                    [ ( "data"
                                      , Encode.object
                                            [ ( "id", Encode.string t.id )
                                            , ( "label", Encode.string (truncate 40 t.title) )
                                            ]
                                      )
                                    , ( "classes", Encode.string "task" )
                                    ]
                            )

                memNodes =
                    viz.memories
                        |> List.map
                            (\m ->
                                Encode.object
                                    [ ( "data"
                                      , Encode.object
                                            [ ( "id", Encode.string m.id )
                                            , ( "label", Encode.string (truncate 40 m.summary) )
                                            ]
                                      )
                                    , ( "classes", Encode.string "memory" )
                                    ]
                            )

                memoryLinkEdges =
                    viz.memoryLinks
                        |> List.map
                            (\link ->
                                Encode.object
                                    [ ( "data"
                                      , Encode.object
                                            [ ( "id", Encode.string ("ml-" ++ link.sourceId ++ "-" ++ link.targetId) )
                                            , ( "source", Encode.string link.sourceId )
                                            , ( "target", Encode.string link.targetId )
                                            , ( "label", Encode.string link.relationType )
                                            ]
                                      )
                                    ]
                            )

                projectMemoryEdges =
                    viz.projectMemoryLinks
                        |> List.map
                            (\link ->
                                Encode.object
                                    [ ( "data"
                                      , Encode.object
                                            [ ( "id", Encode.string ("pm-" ++ link.projectId ++ "-" ++ link.memoryId) )
                                            , ( "source", Encode.string link.projectId )
                                            , ( "target", Encode.string link.memoryId )
                                            ]
                                      )
                                    , ( "classes", Encode.string "entity-memory" )
                                    ]
                            )

                taskMemoryEdges =
                    viz.taskMemoryLinks
                        |> List.map
                            (\link ->
                                Encode.object
                                    [ ( "data"
                                      , Encode.object
                                            [ ( "id", Encode.string ("tm-" ++ link.taskId ++ "-" ++ link.memoryId) )
                                            , ( "source", Encode.string link.taskId )
                                            , ( "target", Encode.string link.memoryId )
                                            ]
                                      )
                                    , ( "classes", Encode.string "entity-memory" )
                                    ]
                            )

                taskDependencyEdges =
                    viz.taskDependencies
                        |> List.map
                            (\dep ->
                                Encode.object
                                    [ ( "data"
                                      , Encode.object
                                            [ ( "id", Encode.string ("td-" ++ dep.taskId ++ "-" ++ dep.dependsOnId) )
                                            , ( "source", Encode.string dep.taskId )
                                            , ( "target", Encode.string dep.dependsOnId )
                                            ]
                                      )
                                    , ( "classes", Encode.string "dependency" )
                                    ]
                            )

                allElements =
                    projectNodes ++ taskNodes ++ memNodes ++ memoryLinkEdges ++ projectMemoryEdges ++ taskMemoryEdges ++ taskDependencyEdges

                positionsKey =
                    case model.selectedWorkspaceId of
                        Just wsId ->
                            graphPositionsKey wsId

                        Nothing ->
                            ""
            in
            initCytoscape
                (Encode.object
                    [ ( "containerId", Encode.string "cytoscape-container" )
                    , ( "elements", Encode.list identity allElements )
                    , ( "positionsKey", Encode.string positionsKey )
                    ]
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ wsConnected (\_ -> WsConnectedMsg)
        , wsDisconnected (\_ -> WsDisconnectedMsg)
        , wsMessage WsMessageReceived
        , cytoscapeNodeClicked CytoscapeNodeClicked
        , cytoscapeEdgeClicked CytoscapeEdgeClicked
        , Browser.Events.onKeyDown (Decode.map GlobalKeyDown (Decode.field "keyCode" Decode.int))
        , localStorageReceived LocalStorageLoaded
        , onMainContentScroll MainContentScrolled
        ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "hmem"
    , body =
        [ div [ class "app" ]
            [ viewSidebar model
            , Keyed.node "div" [ class "main-content", id "main-content-scroll" ]
                [ ( pageKey model.page, viewPage model ) ]
            , viewToasts model.toasts
            , viewConnectionStatus model.wsState
            , viewCreateFormModal model
            , viewDropActionModal model
            , viewDeleteConfirmModal model
            ]
        ]
    }


viewSidebar : Model -> Html Msg
viewSidebar model =
    let
        allWs =
            Dict.values model.workspaces |> List.sortBy .name

        -- Collect all workspace IDs that belong to at least one group
        groupedWsIds =
            model.groupMembers
                |> Dict.values
                |> List.concat
                |> List.foldl (\wsId acc -> Dict.insert wsId True acc) Dict.empty

        ungroupedWs =
            allWs |> List.filter (\ws -> not (Dict.member ws.id groupedWsIds))

        groups =
            model.workspaceGroups |> Dict.values |> List.sortBy .name
    in
    nav [ class "sidebar" ]
        [ div [ class "sidebar-header" ]
            [ a [ href "/", class "sidebar-logo" ] [ text "hmem" ] ]
        , if not (List.isEmpty groups) then
            div []
                (List.map (viewSidebarGroup model) groups)

          else
            text ""
        , if not (List.isEmpty ungroupedWs) then
            div [ class "sidebar-section" ]
                [ div [ class "sidebar-section-title" ] [ text "Workspaces" ]
                , if model.loadingWorkspaces then
                    div [ class "sidebar-loading" ] [ text "Loading..." ]

                  else
                    ul [ class "sidebar-nav" ]
                        (ungroupedWs |> List.map (viewSidebarWorkspace model.selectedWorkspaceId))
                ]

          else if List.isEmpty groups then
            div [ class "sidebar-section" ]
                [ div [ class "sidebar-section-title" ] [ text "Workspaces" ]
                , if model.loadingWorkspaces then
                    div [ class "sidebar-loading" ] [ text "Loading..." ]

                  else
                    div [ class "sidebar-empty" ] [ text "No workspaces" ]
                ]

          else
            text ""
        , div [ class "sidebar-section" ]
            [ ul [ class "sidebar-nav" ]
                [ li []
                    [ a [ href "/memory-graph", class "sidebar-link" ]
                        [ span [ class "sidebar-icon" ] [ text "◉" ]
                        , text "Knowledge Graph"
                        ]
                    ]
                ]
            ]
        ]


viewSidebarGroup : Model -> Api.WorkspaceGroup -> Html Msg
viewSidebarGroup model group =
    let
        memberIds =
            Dict.get group.id model.groupMembers |> Maybe.withDefault []

        memberWs =
            memberIds
                |> List.filterMap (\wsId -> Dict.get wsId model.workspaces)
                |> List.sortBy .name

        collapsed =
            isCollapsed model ("group-" ++ group.id)
    in
    div [ class "sidebar-section" ]
        [ div
            [ class "sidebar-section-title sidebar-group-title"
            , onClick (ToggleTreeNode ("group-" ++ group.id))
            , style "cursor" "pointer"
            , style "display" "flex"
            , style "align-items" "center"
            , style "gap" "0.25rem"
            ]
            [ span [ class "collapse-toggle" ]
                [ text
                    (if collapsed then
                        "▸"

                     else
                        "▾"
                    )
                ]
            , text group.name
            , span [ style "margin-left" "auto", style "font-size" "0.75rem", style "opacity" "0.6" ]
                [ text (String.fromInt (List.length memberWs)) ]
            ]
        , if collapsed then
            text ""

          else
            ul [ class "sidebar-nav" ]
                (memberWs |> List.map (viewSidebarWorkspace model.selectedWorkspaceId))
        ]


viewSidebarWorkspace : Maybe String -> Api.Workspace -> Html Msg
viewSidebarWorkspace selectedId ws =
    let
        isSelected =
            selectedId == Just ws.id

        cls =
            if isSelected then
                "sidebar-link active"

            else
                "sidebar-link"

        icon =
            case ws.workspaceType of
                Api.Repository ->
                    "⌂"

                Api.Planning ->
                    "◇"

                Api.Personal ->
                    "●"

                Api.Organization ->
                    "◆"
    in
    li []
        [ a [ href ("/workspace/" ++ ws.id), class cls ]
            [ span [ class "sidebar-icon" ] [ text icon ]
            , text ws.name
            ]
        ]


pageKey : Page -> String
pageKey page =
    case page of
        HomePage ->
            "home"

        WorkspacePage wsId ->
            "workspace-" ++ wsId

        MemoryGraphPage ->
            "graph"

        NotFound ->
            "notfound"


viewPage : Model -> Html Msg
viewPage model =
    case model.page of
        HomePage ->
            viewHomePage model

        WorkspacePage wsId ->
            viewWorkspacePage wsId model

        MemoryGraphPage ->
            viewGraphPage model

        NotFound ->
            div [ class "page" ]
                [ h2 [] [ text "Not Found" ] ]



-- HOME PAGE


viewHomePage : Model -> Html Msg
viewHomePage model =
    let
        allWs =
            model.workspaces |> Dict.values |> List.sortBy .name

        groups =
            model.workspaceGroups |> Dict.values |> List.sortBy .name

        groupedWsIds =
            model.groupMembers
                |> Dict.values
                |> List.concat
                |> List.foldl (\wsId acc -> Dict.insert wsId True acc) Dict.empty

        ungroupedWs =
            allWs |> List.filter (\ws -> not (Dict.member ws.id groupedWsIds))
    in
    div [ class "page" ]
        [ h2 [ style "display" "flex", style "align-items" "center", style "gap" "0.75rem" ]
            [ text "Workspaces"
            , button
                [ class "btn-small"
                , onClick (ShowCreateForm (CreateGroupForm { name = "", description = "" }))
                ]
                [ text "+ Group" ]
            ]
        , if model.loadingWorkspaces then
            div [ class "loading-indicator" ] [ text "Loading workspaces..." ]

          else
            div []
                (List.map (viewHomeGroup model) groups
                    ++ [ if not (List.isEmpty ungroupedWs) then
                            div []
                                [ if not (List.isEmpty groups) then
                                    h3 [ style "margin-top" "1.5rem", style "margin-bottom" "0.75rem" ] [ text "Ungrouped" ]

                                  else
                                    text ""
                                , div [ class "card-grid" ]
                                    (ungroupedWs |> List.map viewWorkspaceCard)
                                ]

                         else
                            text ""
                       ]
                )
        ]


viewHomeGroup : Model -> Api.WorkspaceGroup -> Html Msg
viewHomeGroup model group =
    let
        memberIds =
            Dict.get group.id model.groupMembers |> Maybe.withDefault []

        memberWs =
            memberIds
                |> List.filterMap (\wsId -> Dict.get wsId model.workspaces)
                |> List.sortBy .name

        isManaging =
            case model.managingGroup of
                Just st ->
                    st.groupId == group.id

                Nothing ->
                    False

        allGroupedIds =
            model.groupMembers
                |> Dict.values
                |> List.concat
                |> List.foldl (\wsId acc -> Dict.insert wsId True acc) Dict.empty

        ungroupedWs =
            model.workspaces
                |> Dict.values
                |> List.filter (\ws -> not (Dict.member ws.id allGroupedIds))
                |> List.sortBy .name
    in
    div [ class "group-section" ]
        [ div [ class "group-header" ]
            [ h3 [ class "group-title" ]
                [ text group.name
                , case group.description of
                    Just desc ->
                        span [ class "group-description" ] [ text ("— " ++ desc) ]

                    Nothing ->
                        text ""
                ]
            , div [ class "group-actions" ]
                [ button
                    [ class
                        (if isManaging then
                            "btn-small group-manage-active"

                         else
                            "btn-small"
                        )
                    , onClick (ToggleManageGroup group.id)
                    , title
                        (if isManaging then
                            "Done managing"

                         else
                            "Manage members"
                        )
                    ]
                    [ text
                        (if isManaging then
                            "Done"

                         else
                            "Manage"
                        )
                    ]
                , button
                    [ class "btn-small btn-danger-subtle"
                    , onClick (ConfirmDelete "group" group.id)
                    , title "Delete group"
                    ]
                    [ text "Delete" ]
                ]
            ]
        , if isManaging then
            div [ class "group-manage-panel" ]
                [ if not (List.isEmpty memberWs) then
                    div [ class "group-member-list" ]
                        (memberWs
                            |> List.map
                                (\ws ->
                                    span [ class "group-member-chip" ]
                                        [ text ws.name
                                        , button
                                            [ class "chip-remove"
                                            , onClick (RemoveWorkspaceFromGroup group.id ws.id)
                                            , title ("Remove " ++ ws.name)
                                            ]
                                            [ text "×" ]
                                        ]
                                )
                        )

                  else
                    text ""
                , if not (List.isEmpty ungroupedWs) then
                    div [ class "group-add-section" ]
                        [ span [ class "group-add-label" ] [ text "Add workspace:" ]
                        , div [ class "group-add-options" ]
                            (ungroupedWs
                                |> List.map
                                    (\ws ->
                                        button
                                            [ class "group-add-chip"
                                            , onClick (AddWorkspaceToGroup group.id ws.id)
                                            ]
                                            [ text ("+ " ++ ws.name) ]
                                    )
                            )
                        ]

                  else
                    div [ class "group-add-section" ]
                        [ span [ class "group-add-label" , style "opacity" "0.5" ] [ text "All workspaces are grouped" ] ]
                ]

          else
            text ""
        , if List.isEmpty memberWs then
            div [ class "empty-state" ] [ text "No workspaces in this group." ]

          else
            div [ class "card-grid" ]
                (memberWs |> List.map viewWorkspaceCard)
        ]


viewWorkspaceCard : Api.Workspace -> Html Msg
viewWorkspaceCard ws =
    div [ class "card clickable", onClick (SelectWorkspace ws.id) ]
        [ div [ class "card-header" ]
            [ span [ class "card-title" ] [ text ws.name ]
            , span [ class ("badge badge-" ++ Api.workspaceTypeToString ws.workspaceType) ]
                [ text (Api.workspaceTypeToString ws.workspaceType) ]
            ]
        , div [ class "card-body" ]
            [ text ""
            ]
        ]



-- WORKSPACE PAGE


viewWorkspacePage : String -> Model -> Html Msg
viewWorkspacePage wsId model =
    case Dict.get wsId model.workspaces of
        Nothing ->
            div [ class "page" ]
                [ div [ class "loading-indicator" ] [ text "Loading..." ] ]

        Just ws ->
            let
                wsProjects =
                    model.projects |> Dict.values |> List.filter (\p -> p.workspaceId == wsId)

                wsTasks =
                    model.tasks |> Dict.values |> List.filter (\t -> t.workspaceId == wsId)

                wsMemories =
                    model.memories |> Dict.values |> List.filter (\m -> m.workspaceId == wsId)

                activeProjects =
                    wsProjects |> List.filter (\p -> p.status == Api.ProjActive || p.status == Api.ProjPaused) |> List.length

                activeTasks =
                    wsTasks |> List.filter (\t -> t.status == Api.Todo || t.status == Api.InProgress || t.status == Api.Blocked) |> List.length

                memoryCount =
                    List.length wsMemories

                summaryParts =
                    List.filterMap identity
                        [ if activeProjects > 0 then
                            Just (String.fromInt activeProjects ++ " open " ++ (if activeProjects > 1 then "projects" else "project"))

                          else
                            Nothing
                        , if activeTasks > 0 then
                            Just (String.fromInt activeTasks ++ " open " ++ (if activeTasks > 1 then "tasks" else "task"))

                          else
                            Nothing
                        , if memoryCount > 0 then
                            Just (String.fromInt memoryCount ++ " memor" ++ (if memoryCount > 1 then "ies" else "y"))

                          else
                            Nothing
                        ]
            in
            div [ class "page" ]
                [ viewStickyWorkspaceBar model ws summaryParts
                , div [ class "workspace-header" ]
                    [ div [ class "workspace-header-top" ]
                        [ div [ class "workspace-header-title" ]
                            [ span [ class ("badge badge-" ++ Api.workspaceTypeToString ws.workspaceType) ]
                                [ text (Api.workspaceTypeToString ws.workspaceType) ]
                            , viewEditableText model "workspace" ws.id "name" ws.name
                            ]
                        , viewCreateButton model.activeTab
                        ]
                    , if not (List.isEmpty summaryParts) then
                        div [ class "workspace-summary" ] [ text (String.join " · " summaryParts) ]

                      else
                        text ""
                    , div [ class "workspace-details" ]
                        [ div [ class "workspace-detail" ]
                            [ span [ class "workspace-detail-label" ] [ text "ID" ]
                            , span [ class "workspace-detail-value card-id card-id-copy", onClick (CopyId ws.id) ] [ text ws.id ]
                            ]
                        , case ws.ghOwner of
                            Just owner ->
                                div [ class "workspace-detail" ]
                                    [ span [ class "workspace-detail-label" ] [ text "GitHub" ]
                                    , span [ class "workspace-detail-value" ]
                                        [ text
                                            (owner
                                                ++ (case ws.ghRepo of
                                                        Just repo ->
                                                            "/" ++ repo

                                                        Nothing ->
                                                            ""
                                                   )
                                            )
                                        ]
                                    ]

                            Nothing ->
                                text ""
                        , div [ class "workspace-detail" ]
                            [ span [ class "workspace-detail-label" ] [ text "Created" ]
                            , span [ class "workspace-detail-value" ] [ text (formatDate ws.createdAt) ]
                            ]
                        , div [ class "workspace-detail" ]
                            [ span [ class "workspace-detail-label" ] [ text "Updated" ]
                            , span [ class "workspace-detail-value" ] [ text (formatDate ws.updatedAt) ]
                            ]
                        ]
                    ]
                , viewSearchBar model
                , viewTabs model.activeTab
                , if model.loadingWorkspaceData then
                    div [ class "loading-indicator" ] [ text "Loading..." ]

                  else
                    case model.unifiedSearchResults of
                        Just results ->
                            viewUnifiedSearchResults model results

                        Nothing ->
                            viewTabContent wsId model
                ]


viewStickyWorkspaceBar : Model -> Api.Workspace -> List String -> Html Msg
viewStickyWorkspaceBar model ws summaryParts =
    let
        stickyThreshold =
            200

        isVisible =
            model.mainContentScrollY > stickyThreshold
    in
    div
        [ class
            ("sticky-workspace-bar"
                ++ (if isVisible then
                        " sticky-workspace-bar--visible"

                    else
                        ""
                   )
            )
        ]
        [ div [ class "sticky-workspace-bar__header" ]
            [ span [ class ("badge badge-" ++ Api.workspaceTypeToString ws.workspaceType) ]
                [ text (Api.workspaceTypeToString ws.workspaceType) ]
            , span [ class "sticky-workspace-name" ] [ text ws.name ]
            , if not (List.isEmpty summaryParts) then
                span [ class "sticky-workspace-summary" ] [ text (String.join " · " summaryParts) ]

              else
                text ""
            ]
        , viewSearchBar model
        ]


viewSearchBar : Model -> Html Msg
viewSearchBar model =
    div [ class "search-filter-bar" ]
        [ Html.form [ class "search-bar", onSubmit SubmitSearch ]
            [ input
                [ class "search-input"
                , type_ "text"
                , placeholder "Search all entities... (Enter to search)"
                , value model.searchQuery
                , onInput SearchInput
                ]
                []
            , if model.isSearching then
                span [ class "search-spinner" ] [ text "…" ]

              else if not (String.isEmpty model.searchQuery) then
                button [ class "search-clear", onClick (SearchInput ""), type_ "button" ] [ text "✕" ]

              else
                text ""
            ]
        , case model.unifiedSearchResults of
            Just _ ->
                text ""

            Nothing ->
                case model.activeTab of
                    ProjectsTab ->
                        viewFilterBar model

                    MemoriesTab ->
                        viewMemoryFilterBar model
        ]


viewFilterBar : Model -> Html Msg
viewFilterBar model =
    div [ class "filter-bar" ]
        [ div [ class "filter-group" ]
            [ span [ class "filter-label" ] [ text "Show:" ]
            , viewFilterPill "All" (model.filterShowOnly == ShowAll) (SetFilterShowOnly ShowAll)
            , viewFilterPill "Projects" (model.filterShowOnly == ShowProjectsOnly) (SetFilterShowOnly ShowProjectsOnly)
            , viewFilterPill "Tasks" (model.filterShowOnly == ShowTasksOnly) (SetFilterShowOnly ShowTasksOnly)
            ]
        , div [ class "filter-group" ]
            [ span [ class "filter-label" ] [ text "Project:" ]
            , viewFilterPill "Active" (List.member "active" model.filterProjectStatuses) (ToggleFilterProjectStatus "active")
            , viewFilterPill "Paused" (List.member "paused" model.filterProjectStatuses) (ToggleFilterProjectStatus "paused")
            , viewFilterPill "Completed" (List.member "completed" model.filterProjectStatuses) (ToggleFilterProjectStatus "completed")
            , viewFilterPill "Archived" (List.member "archived" model.filterProjectStatuses) (ToggleFilterProjectStatus "archived")
            ]
        , div [ class "filter-group" ]
            [ span [ class "filter-label" ] [ text "Task:" ]
            , viewFilterPill "Todo" (List.member "todo" model.filterTaskStatuses) (ToggleFilterTaskStatus "todo")
            , viewFilterPill "In Progress" (List.member "in_progress" model.filterTaskStatuses) (ToggleFilterTaskStatus "in_progress")
            , viewFilterPill "Blocked" (List.member "blocked" model.filterTaskStatuses) (ToggleFilterTaskStatus "blocked")
            , viewFilterPill "Done" (List.member "done" model.filterTaskStatuses) (ToggleFilterTaskStatus "done")
            , viewFilterPill "Cancelled" (List.member "cancelled" model.filterTaskStatuses) (ToggleFilterTaskStatus "cancelled")
            ]
        , div [ class "filter-group" ]
            [ span [ class "filter-label" ] [ text "Priority:" ]
            , select
                [ class "filter-select"
                , onInput
                    (\s ->
                        case s of
                            "any" ->
                                SetFilterPriority AnyPriority

                            "exact" ->
                                SetFilterPriority (ExactPriority 5)

                            "above" ->
                                SetFilterPriority (AbovePriority 5)

                            "below" ->
                                SetFilterPriority (BelowPriority 5)

                            _ ->
                                SetFilterPriority AnyPriority
                    )
                ]
                [ option [ value "any", selected (model.filterPriority == AnyPriority) ] [ text "Any" ]
                , option
                    [ value "exact"
                    , selected
                        (case model.filterPriority of
                            ExactPriority _ ->
                                True

                            _ ->
                                False
                        )
                    ]
                    [ text "Exact" ]
                , option
                    [ value "above"
                    , selected
                        (case model.filterPriority of
                            AbovePriority _ ->
                                True

                            _ ->
                                False
                        )
                    ]
                    [ text "Above" ]
                , option
                    [ value "below"
                    , selected
                        (case model.filterPriority of
                            BelowPriority _ ->
                                True

                            _ ->
                                False
                        )
                    ]
                    [ text "Below" ]
                ]
            , case model.filterPriority of
                AnyPriority ->
                    text ""

                ExactPriority v ->
                    input
                        [ class "filter-priority-input"
                        , type_ "number"
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "10"
                        , value (String.fromInt v)
                        , onInput (\s -> SetFilterPriority (ExactPriority (Maybe.withDefault 5 (String.toInt s))))
                        ]
                        []

                AbovePriority v ->
                    input
                        [ class "filter-priority-input"
                        , type_ "number"
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "10"
                        , value (String.fromInt v)
                        , onInput (\s -> SetFilterPriority (AbovePriority (Maybe.withDefault 5 (String.toInt s))))
                        ]
                        []

                BelowPriority v ->
                    input
                        [ class "filter-priority-input"
                        , type_ "number"
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "10"
                        , value (String.fromInt v)
                        , onInput (\s -> SetFilterPriority (BelowPriority (Maybe.withDefault 5 (String.toInt s))))
                        ]
                        []
            ]
        ]


viewMemoryFilterBar : Model -> Html Msg
viewMemoryFilterBar model =
    div [ class "filter-bar" ]
        [ div [ class "filter-group" ]
            [ span [ class "filter-label" ] [ text "Type:" ]
            , viewFilterPill "Short Term" (List.member "short_term" model.filterMemoryTypes) (ToggleFilterMemoryType "short_term")
            , viewFilterPill "Long Term" (List.member "long_term" model.filterMemoryTypes) (ToggleFilterMemoryType "long_term")
            ]
        , div [ class "filter-group" ]
            [ span [ class "filter-label" ] [ text "Pinned:" ]
            , viewFilterPill "All" (model.filterMemoryPinned == Nothing) (SetFilterMemoryPinned Nothing)
            , viewFilterPill "Pinned" (model.filterMemoryPinned == Just True) (SetFilterMemoryPinned (Just True))
            , viewFilterPill "Unpinned" (model.filterMemoryPinned == Just False) (SetFilterMemoryPinned (Just False))
            ]
        , div [ class "filter-group" ]
            [ span [ class "filter-label" ] [ text "Importance:" ]
            , select
                [ class "filter-select"
                , onInput
                    (\s ->
                        case s of
                            "exact" ->
                                SetFilterImportance (ExactPriority 5)

                            "above" ->
                                SetFilterImportance (AbovePriority 5)

                            "below" ->
                                SetFilterImportance (BelowPriority 5)

                            _ ->
                                SetFilterImportance AnyPriority
                    )
                ]
                [ option [ value "any", selected (model.filterImportance == AnyPriority) ] [ text "Any" ]
                , option
                    [ value "exact"
                    , selected
                        (case model.filterImportance of
                            ExactPriority _ ->
                                True

                            _ ->
                                False
                        )
                    ]
                    [ text "Exact" ]
                , option
                    [ value "above"
                    , selected
                        (case model.filterImportance of
                            AbovePriority _ ->
                                True

                            _ ->
                                False
                        )
                    ]
                    [ text "Above" ]
                , option
                    [ value "below"
                    , selected
                        (case model.filterImportance of
                            BelowPriority _ ->
                                True

                            _ ->
                                False
                        )
                    ]
                    [ text "Below" ]
                ]
            , case model.filterImportance of
                AnyPriority ->
                    text ""

                ExactPriority v ->
                    input
                        [ class "filter-priority-input"
                        , type_ "number"
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "10"
                        , value (String.fromInt v)
                        , onInput (\s -> SetFilterImportance (ExactPriority (Maybe.withDefault 5 (String.toInt s))))
                        ]
                        []

                AbovePriority v ->
                    input
                        [ class "filter-priority-input"
                        , type_ "number"
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "10"
                        , value (String.fromInt v)
                        , onInput (\s -> SetFilterImportance (AbovePriority (Maybe.withDefault 5 (String.toInt s))))
                        ]
                        []

                BelowPriority v ->
                    input
                        [ class "filter-priority-input"
                        , type_ "number"
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "10"
                        , value (String.fromInt v)
                        , onInput (\s -> SetFilterImportance (BelowPriority (Maybe.withDefault 5 (String.toInt s))))
                        ]
                        []
            ]
        ]


viewFilterPill : String -> Bool -> Msg -> Html Msg
viewFilterPill label isActive msg =
    button
        [ class
            (if isActive then
                "filter-pill filter-pill-active"

             else
                "filter-pill"
            )
        , onClick msg
        ]
        [ text label ]


viewCreateButton : WorkspaceTab -> Html Msg
viewCreateButton tab =
    text ""


viewTabs : WorkspaceTab -> Html Msg
viewTabs activeTab =
    div [ class "tabs" ]
        [ viewTab ProjectsTab activeTab "Projects"
        , viewTab MemoriesTab activeTab "Memories"
        ]


viewTab : WorkspaceTab -> WorkspaceTab -> String -> Html Msg
viewTab tab activeTab label =
    button
        [ class
            (if tab == activeTab then
                "tab active"

             else
                "tab"
            )
        , onClick (SwitchTab tab)
        ]
        [ text label ]


viewTabContent : String -> Model -> Html Msg
viewTabContent wsId model =
    case model.activeTab of
        ProjectsTab ->
            viewProjectsTree wsId model

        MemoriesTab ->
            viewMemoriesList wsId model


viewUnifiedSearchResults : Model -> Api.UnifiedSearchResults -> Html Msg
viewUnifiedSearchResults model results =
    let
        totalCount =
            List.length results.memories + List.length results.projects + List.length results.tasks
    in
    div [ class "unified-search-results" ]
        [ div [ class "search-results-header" ]
            [ span [ class "search-results-count" ]
                [ text (String.fromInt totalCount ++ " results") ]
            ]
        , if not (List.isEmpty results.projects) then
            div [ class "search-results-section" ]
                [ h3 [ class "search-section-title" ]
                    [ text ("Projects (" ++ String.fromInt (List.length results.projects) ++ ")") ]
                , div [ class "entity-list" ]
                    (List.map (viewSearchProjectResult model) results.projects)
                ]

          else
            text ""
        , if not (List.isEmpty results.tasks) then
            div [ class "search-results-section" ]
                [ h3 [ class "search-section-title" ]
                    [ text ("Tasks (" ++ String.fromInt (List.length results.tasks) ++ ")") ]
                , div [ class "entity-list" ]
                    (List.map (viewSearchTaskResult model) results.tasks)
                ]

          else
            text ""
        , if not (List.isEmpty results.memories) then
            div [ class "search-results-section" ]
                [ h3 [ class "search-section-title" ]
                    [ text ("Memories (" ++ String.fromInt (List.length results.memories) ++ ")") ]
                , div [ class "entity-list" ]
                    (List.map (viewMemoryCard model) results.memories)
                ]

          else
            text ""
        , if totalCount == 0 then
            div [ class "empty-state" ] [ text "No results found." ]

          else
            text ""
        ]


viewSearchProjectResult : Model -> Api.ProjectSearchResult -> Html Msg
viewSearchProjectResult model result =
    div [ class "search-result-card" ]
        [ div [ class "card-header" ]
            [ span [ class "card-title" ] [ text result.project.name ]
            , span [ class ("badge badge-" ++ Api.projectStatusToString result.project.status) ]
                [ text (Api.projectStatusToString result.project.status) ]
            , span [ class "badge badge-priority" ]
                [ text ("P" ++ String.fromInt result.project.priority) ]
            ]
        , case result.project.description of
            Just desc ->
                div [ class "card-body" ] [ text desc ]

            Nothing ->
                text ""
        , if not (List.isEmpty result.linkedMemories) then
            div [ class "linked-memories-summary" ]
                (List.map viewLinkedMemorySummary result.linkedMemories)

          else
            text ""
        ]


viewSearchTaskResult : Model -> Api.TaskSearchResult -> Html Msg
viewSearchTaskResult model result =
    div [ class "search-result-card" ]
        [ div [ class "card-header" ]
            [ span [ class "card-title" ] [ text result.task.title ]
            , span [ class ("badge badge-" ++ Api.taskStatusToString result.task.status) ]
                [ text (Api.taskStatusToString result.task.status) ]
            , span [ class "badge badge-priority" ]
                [ text ("P" ++ String.fromInt result.task.priority) ]
            ]
        , case result.task.description of
            Just desc ->
                div [ class "card-body" ] [ text desc ]

            Nothing ->
                text ""
        , if not (List.isEmpty result.linkedMemories) then
            div [ class "linked-memories-summary" ]
                (List.map viewLinkedMemorySummary result.linkedMemories)

          else
            text ""
        ]


viewLinkedMemorySummary : Api.LinkedMemorySummary -> Html Msg
viewLinkedMemorySummary mem =
    div [ class "linked-memory-chip" ]
        [ span [ class "linked-memory-importance" ]
            [ text (String.fromInt mem.importance) ]
        , span [ class "linked-memory-text" ]
            [ text (Maybe.withDefault "(no summary)" mem.summary) ]
        ]



-- PROJECT TREE VIEW


viewProjectsTree : String -> Model -> Html Msg
viewProjectsTree wsId model =
    let
        wsProjects =
            model.projects
                |> Dict.values
                |> List.filter (\p -> p.workspaceId == wsId)

        wsTasks =
            model.tasks
                |> Dict.values
                |> List.filter (\t -> t.workspaceId == wsId)

        query =
            String.toLower (String.trim model.searchQuery)

        hasSearch =
            not (String.isEmpty query)

        hasActiveFilters =
            model.filterShowOnly /= ShowAll || model.filterPriority /= AnyPriority || not (List.isEmpty model.filterProjectStatuses) || not (List.isEmpty model.filterTaskStatuses)

        projectPassesFilters p =
            (model.filterShowOnly /= ShowTasksOnly)
                && passesStatusFilter model.filterProjectStatuses (Api.projectStatusToString p.status)
                && passesPriorityFilter model.filterPriority p.priority

        taskPassesFilters t =
            (model.filterShowOnly /= ShowProjectsOnly)
                && passesStatusFilter model.filterTaskStatuses (Api.taskStatusToString t.status)
                && passesPriorityFilter model.filterPriority t.priority

        allCollapsed =
            not (Dict.isEmpty model.collapsedNodes)

        expandCollapseBar =
            div [ class "tree-toolbar" ]
                [ button [ class "btn-small btn-ghost", onClick ExpandAllNodes ] [ text "Expand All" ]
                , button [ class "btn-small btn-ghost", onClick CollapseAllNodes ] [ text "Collapse All" ]
                ]

        inlineCreateView =
            viewInlineCreateInput model Nothing "project"

        focusBreadcrumbBar =
            case model.breadcrumbAnchor of
                Just ( aType, aId ) ->
                    let
                        -- Tree-based parent chain for the breadcrumb anchor (deepest focused entity)
                        treeCrumbs =
                            case aType of
                                "project" ->
                                    case Dict.get aId model.projects of
                                        Just proj ->
                                            buildProjectBreadcrumb model proj []

                                        Nothing ->
                                            []

                                "task" ->
                                    case Dict.get aId model.tasks of
                                        Just task ->
                                            buildTaskBreadcrumb model task []

                                        Nothing ->
                                            []

                                _ ->
                                    []

                        currentFocusId =
                            model.focusedEntity |> Maybe.map Tuple.second |> Maybe.withDefault ""

                        treeCrumbLinks =
                            treeCrumbs
                                |> List.map
                                    (\( cId, cName, cType ) ->
                                        if cId == currentFocusId then
                                            span [ class "focus-crumb focus-crumb-current" ] [ text cName ]

                                        else
                                            span
                                                [ class "focus-crumb focus-crumb-link"
                                                , onClick (FocusEntityKeepForward cType cId)
                                                ]
                                                [ text cName ]
                                    )
                                |> List.intersperse (span [ class "focus-crumb-sep" ] [ text " › " ])

                        -- Forward history entries
                        -- If the user navigated to a parent (focusedEntity differs from history entry),
                        -- include the history entry at the current index as part of forward crumbs
                        historyEntry =
                            model.focusHistory
                                |> List.drop model.focusHistoryIndex
                                |> List.head

                        isOnParent =
                            historyEntry /= model.focusedEntity

                        forwardStartIdx =
                            if isOnParent then
                                model.focusHistoryIndex

                            else
                                model.focusHistoryIndex + 1

                        forwardCrumbs =
                            model.focusHistory
                                |> List.drop forwardStartIdx
                                |> List.indexedMap
                                    (\i ( fType, fId ) ->
                                        let
                                            name =
                                                case fType of
                                                    "project" ->
                                                        Dict.get fId model.projects |> Maybe.map .name |> Maybe.withDefault "Project"

                                                    "task" ->
                                                        Dict.get fId model.tasks |> Maybe.map .title |> Maybe.withDefault "Task"

                                                    _ ->
                                                        "Entity"

                                            -- Skip forward entries that are already in the tree crumbs
                                            isDuplicate =
                                                List.any (\( cId, _, _ ) -> cId == fId) treeCrumbs
                                        in
                                        if isDuplicate then
                                            Nothing

                                        else
                                            Just
                                                ( span
                                                    [ class "focus-crumb focus-crumb-forward"
                                                    , onClick (FocusBreadcrumbNav (forwardStartIdx + i))
                                                    ]
                                                    [ text name ]
                                                )
                                    )
                                |> List.filterMap identity
                                |> List.intersperse (span [ class "focus-crumb-sep" ] [ text " › " ])

                        forwardSection =
                            if List.isEmpty forwardCrumbs then
                                []

                            else
                                span [ class "focus-crumb-sep" ] [ text " › " ] :: forwardCrumbs
                    in
                    div [ class "focus-breadcrumb-bar" ]
                        (button [ class "focus-clear-btn", onClick ClearFocus, title "Exit focus mode" ] [ text "✕" ]
                            :: span [ class "focus-crumb focus-crumb-link", onClick ClearFocus ] [ text "All" ]
                            :: (if not (List.isEmpty treeCrumbLinks) then
                                    span [ class "focus-crumb-sep" ] [ text " › " ] :: treeCrumbLinks ++ forwardSection

                                else
                                    []
                               )
                        )

                Nothing ->
                    text ""

        treeContent =
            case model.focusedEntity of
                Just ( "project", projId ) ->
                    case Dict.get projId model.projects of
                        Just proj ->
                            [ ( projId, viewProjectNode wsProjects model 0 proj hasSearch query ) ]

                        Nothing ->
                            []

                Just ( "task", taskId ) ->
                    case Dict.get taskId model.tasks of
                        Just task ->
                            [ ( taskId, viewFocusedTaskNode model task wsProjects hasSearch query hasActiveFilters taskPassesFilters ) ]

                        Nothing ->
                            []

                _ ->
                    case model.filterShowOnly of
                        ShowTasksOnly ->
                            let
                                rootTasks =
                                    wsTasks
                                        |> List.filter (\t -> t.parentId == Nothing)
                                        |> List.sortBy (\t -> ( Api.taskStatusOrder t.status, negate t.priority, String.toLower t.title ))

                                visibleRootTasks =
                                    rootTasks
                                        |> (if hasSearch then
                                                List.filter (taskTreeMatchesSearch query wsTasks)

                                            else
                                                identity
                                           )
                                        |> (if hasActiveFilters then
                                                List.filter (\t -> taskTreePassesFilters t wsTasks taskPassesFilters)

                                            else
                                                identity
                                           )
                            in
                            List.map (\t -> ( t.id, viewTaskCard False model t )) visibleRootTasks

                        _ ->
                            let
                                rootProjects =
                                    wsProjects
                                        |> List.filter (\p -> p.parentId == Nothing)
                                        |> List.sortBy (\p -> ( Api.projectStatusOrder p.status, negate p.priority, String.toLower p.name ))

                                visibleRootProjects =
                                    rootProjects
                                        |> (if hasSearch then
                                                List.filter (projectTreeMatchesSearch query wsProjects wsTasks)

                                            else
                                                identity
                                           )
                                        |> (if hasActiveFilters then
                                                List.filter (\p -> projectTreePassesFilters (\pp -> passesStatusFilter model.filterProjectStatuses (Api.projectStatusToString pp.status)) p wsProjects wsTasks projectPassesFilters taskPassesFilters)

                                            else
                                                identity
                                           )

                                orphanTasks =
                                    wsTasks
                                        |> List.filter (\t -> t.projectId == Nothing && t.parentId == Nothing)
                                        |> List.sortBy (\t -> ( Api.taskStatusOrder t.status, negate t.priority, String.toLower t.title ))

                                visibleOrphans =
                                    orphanTasks
                                        |> (if hasSearch then
                                                List.filter (taskTreeMatchesSearch query wsTasks)

                                            else
                                                identity
                                           )
                                        |> (if hasActiveFilters then
                                                List.filter (\t -> taskTreePassesFilters t wsTasks taskPassesFilters)

                                            else
                                                identity
                                           )
                            in
                            viewProjectsWithZones model (\p -> viewProjectNode wsProjects model 0 p hasSearch query) Nothing visibleRootProjects
                                ++ (if model.filterShowOnly /= ShowProjectsOnly && not (List.isEmpty visibleOrphans) then
                                        [ ( "orphan-tasks-section"
                                          , div [ class "orphan-tasks-section" ]
                                                [ div [ class "orphan-tasks-header" ] [ text "Unassigned Tasks" ]
                                                , Keyed.node "div" [ class "tree-tasks" ]
                                                    (viewTasksWithZones model "orphan" Nothing Nothing visibleOrphans)
                                                ]
                                          )
                                        ]

                                    else
                                        []
                                   )
    in
    Keyed.node "div"
        [ class "tree-view" ]
        (( "expand-collapse-bar", expandCollapseBar )
            :: ( "inline-create", inlineCreateView )
            :: ( "focus-breadcrumb", focusBreadcrumbBar )
            :: treeContent
        )


viewProjectNode : List Api.Project -> Model -> Int -> Api.Project -> Bool -> String -> Html Msg
viewProjectNode allProjects model depth project hasSearch query =
    let
        allTasks =
            Dict.values model.tasks

        hasActiveFilters =
            model.filterShowOnly /= ShowAll || model.filterPriority /= AnyPriority || not (List.isEmpty model.filterProjectStatuses) || not (List.isEmpty model.filterTaskStatuses)

        projectPassesFilters p =
            (model.filterShowOnly /= ShowTasksOnly)
                && passesStatusFilter model.filterProjectStatuses (Api.projectStatusToString p.status)
                && passesPriorityFilter model.filterPriority p.priority

        taskPassesFilters t =
            (model.filterShowOnly /= ShowProjectsOnly)
                && passesStatusFilter model.filterTaskStatuses (Api.taskStatusToString t.status)
                && passesPriorityFilter model.filterPriority t.priority

        children =
            allProjects
                |> List.filter (\p -> p.parentId == Just project.id)
                |> List.sortBy (\p -> ( Api.projectStatusOrder p.status, negate p.priority, String.toLower p.name ))

        visibleChildren =
            children
                |> (if hasSearch then
                        List.filter (projectTreeMatchesSearch query allProjects allTasks)

                    else
                        identity
                   )
                |> (if hasActiveFilters then
                        List.filter (\p -> projectTreePassesFilters (\pp -> passesStatusFilter model.filterProjectStatuses (Api.projectStatusToString pp.status)) p allProjects allTasks projectPassesFilters taskPassesFilters)

                    else
                        identity
                   )

        hasChildren =
            not (List.isEmpty children)

        collapsed =
            isCollapsed model ("proj-" ++ project.id)

        projectTasks =
            model.tasks
                |> Dict.values
                |> List.filter (\t -> t.projectId == Just project.id && t.parentId == Nothing)
                |> List.sortBy (\t -> ( Api.taskStatusOrder t.status, negate t.priority, String.toLower t.title ))

        visibleTasks =
            projectTasks
                |> (if hasSearch then
                        List.filter (taskTreeMatchesSearch query allTasks)

                    else
                        identity
                   )
                |> (if hasActiveFilters then
                        List.filter (\t -> taskTreePassesFilters t allTasks taskPassesFilters)

                    else
                        identity
                   )

        linkedMems =
            Dict.get project.id model.entityMemories |> Maybe.withDefault []
    in
    div [ class "tree-node", style "margin-left" (String.fromInt (depth * 20) ++ "px"), id ("entity-" ++ project.id) ]
        [ div
            [ class ("card tree-card card-project card-status-" ++ Api.projectStatusToString project.status ++ dragOverClass model project.id)
            , draggable "true"
            , on "dragstart" (Decode.succeed (DragStartCard "project" project.id))
            , preventDefaultOn "dragover" (Decode.succeed ( DragOverCard project.id, True ))
            , preventDefaultOn "drop" (Decode.succeed ( DropOnCard "project" project.id, True ))
            , on "dragend" (Decode.succeed DragEndCard)
            , onDoubleClick (FocusEntity "project" project.id)
            ]
            [ div [ class "card-header" ]
                [ div [ class "tree-toggle-row" ]
                    [ if hasChildren || not (List.isEmpty projectTasks) then
                        button [ class "tree-toggle", onClick (ToggleTreeNode ("proj-" ++ project.id)) ]
                            [ text
                                (if collapsed then
                                    "▶"

                                 else
                                    "▼"
                                )
                            ]

                      else
                        span [ class "tree-toggle-spacer" ] []
                    , span [ class "entity-type-label entity-type-project" ] [ text "PRJ" ]
                    , viewEditableText model "project" project.id "name" project.name
                    ]
                , div [ class "card-actions" ]
                    [ viewStatusSelect "project" project.id (Api.projectStatusToString project.status) Api.allProjectStatuses Api.projectStatusToString ChangeProjectStatus
                    , viewPrioritySelect "project" project.id project.priority ChangeProjectPriority
                    , button [ class "btn-icon btn-danger", onClick (ConfirmDelete "project" project.id), title "Delete" ] [ text "✕" ]
                    ]
                ]
            , let
                isProjectRemaining p =
                    p.status == Api.ProjActive || p.status == Api.ProjPaused

                isTaskRemaining t =
                    t.status == Api.Todo || t.status == Api.InProgress || t.status == Api.Blocked

                allDescendantProjectIds =
                    collectDescendantProjectIds allProjects project.id

                allDescendantProjects =
                    allProjects
                        |> List.filter (\p -> p.id /= project.id && List.member p.id allDescendantProjectIds)

                remainingSubprojects =
                    List.filter isProjectRemaining allDescendantProjects |> List.length

                completedSubprojects =
                    List.length allDescendantProjects - remainingSubprojects

                allProjectTasks =
                    model.tasks
                        |> Dict.values
                        |> List.filter (\t -> t.parentId == Nothing && (List.member (Maybe.withDefault "" (Maybe.map identity t.projectId)) allDescendantProjectIds))

                remainingTasks =
                    List.filter isTaskRemaining allProjectTasks |> List.length

                completedTasks =
                    List.length allProjectTasks - remainingTasks

                memCount =
                    List.length linkedMems

                summaryParts =
                    List.filterMap identity
                        [ countLabel remainingSubprojects completedSubprojects "subproject" "subprojects"
                        , countLabel remainingTasks completedTasks "task" "tasks"
                        , if memCount > 0 then
                            Just (String.fromInt memCount ++ " memor" ++ (if memCount > 1 then "ies" else "y"))

                          else
                            Nothing
                        ]
              in
              if List.isEmpty summaryParts then
                text ""

              else
                div [ class "card-summary" ] [ text (String.join " · " summaryParts) ]
            , div [ class "card-body card-expanded" ]
                [ div [ class "card-desc-row" ]
                    [ button [ class "btn-extras-toggle", onClick (ToggleCardExpand project.id) ]
                        [ text
                            (if isExpanded model project.id then
                                "−"

                             else
                                "+"
                            )
                        ]
                    , viewEditableTextarea model "project" project.id "description" (Maybe.withDefault "" project.description)
                    ]
                , if isExpanded model project.id then
                    div [ class "card-extras" ]
                        [ viewLinkedMemories model "project" project.id linkedMems
                        ]

                  else
                    text ""
                ]
            , div [ class "card-inline-actions" ]
                [ button
                    [ class "btn-inline-create"
                    , onClick (ShowInlineCreate (InlineCreateProject { parentId = Just project.id, name = "" }))
                    ]
                    [ text "+ Subproject" ]
                , button
                    [ class "btn-inline-create"
                    , onClick (ShowInlineCreate (InlineCreateTask { projectId = Just project.id, parentId = Nothing, title = "" }))
                    ]
                    [ text "+ Task" ]
                ]
            , viewInlineCreateInputForParent model (Just project.id) "project"
            , viewInlineCreateInputForParent model (Just project.id) "task"
            , div [ class "card-meta-group" ]
                [ div [ class "card-meta-row" ]
                    [ span [ class "card-meta" ] [ text ("Created: " ++ formatDate project.createdAt) ]
                    , span [ class "card-meta" ] [ text ("Updated: " ++ formatDate project.updatedAt) ]
                    , span [ class "card-meta card-id card-id-copy", onClick (CopyId project.id) ] [ text project.id ]
                    ]
                ]
            ]
        , if not collapsed then
            Keyed.node "div" [ class "tree-children" ]
                (viewProjectsWithZones model (\c -> viewProjectNode allProjects model (depth + 1) c hasSearch query) (Just project.id) visibleChildren
                    ++ (if not (List.isEmpty visibleTasks) then
                            [ ( "project-tasks-" ++ project.id
                              , Keyed.node "div" [ class "tree-tasks", style "margin-left" "20px" ]
                                    (viewTasksWithZones model "project-tasks" (Just project.id) Nothing visibleTasks)
                              )
                            ]

                        else
                            []
                       )
                )

          else
            text ""
        ]



viewFocusedTaskNode : Model -> Api.Task -> List Api.Project -> Bool -> String -> Bool -> (Api.Task -> Bool) -> Html Msg
viewFocusedTaskNode model task allProjects hasSearch query hasActiveFilters taskPassesFilters =
    div [ class "tree-node" ]
        [ viewTaskCard False model task
        ]


viewTaskCard : Bool -> Model -> Api.Task -> Html Msg
viewTaskCard showProject model task =
    let
        projectName =
            task.projectId
                |> Maybe.andThen (\pid -> Dict.get pid model.projects)
                |> Maybe.map .name

        hasChildren =
            model.tasks
                |> Dict.values
                |> List.any (\t -> t.parentId == Just task.id)

        collapsed =
            isCollapsed model ("task-" ++ task.id)

        isSubtask =
            task.parentId /= Nothing

        typeLabel =
            if isSubtask then
                "SUB"

            else
                "TSK"

        typeClass =
            if isSubtask then
                "entity-type-subtask"

            else
                "entity-type-task"

        cardClass =
            if isSubtask then
                "card-subtask"

            else
                "card-task"

        linkedMems =
            Dict.get task.id model.entityMemories |> Maybe.withDefault []
    in
    div
        ([ class ("card tree-card " ++ cardClass ++ " card-status-" ++ Api.taskStatusToString task.status ++ dragOverClass model task.id)
        , draggable "true"
        , id ("entity-" ++ task.id)
        , on "dragstart" (Decode.succeed (DragStartCard "task" task.id))
        , preventDefaultOn "dragover" (Decode.succeed ( DragOverCard task.id, True ))
        , preventDefaultOn "drop" (Decode.succeed ( DropOnCard "task" task.id, True ))
        , on "dragend" (Decode.succeed DragEndCard)
        ]
        ++ (if not isSubtask then
                [ onDoubleClick (FocusEntity "task" task.id) ]

            else
                []
           )
        )
        [ div [ class "card-header" ]
            [ div [ class "tree-toggle-row" ]
                [ if hasChildren then
                    button [ class "tree-toggle", onClick (ToggleTreeNode ("task-" ++ task.id)) ]
                        [ text
                            (if collapsed then
                                "▶"

                             else
                                "▼"
                            )
                        ]

                  else
                    span [ class "tree-toggle-spacer" ] []
                , span [ class ("entity-type-label " ++ typeClass) ] [ text typeLabel ]
                , viewEditableText model "task" task.id "title" task.title
                ]
            , div [ class "card-actions" ]
                [ viewStatusSelect "task" task.id (Api.taskStatusToString task.status) Api.allTaskStatuses Api.taskStatusToString ChangeTaskStatus
                , viewPrioritySelect "task" task.id task.priority ChangeTaskPriority
                , button [ class "btn-icon btn-danger", onClick (ConfirmDelete "task" task.id), title "Delete" ] [ text "✕" ]
                ]
            ]
        , let
            childTasks =
                model.tasks |> Dict.values |> List.filter (\t -> t.parentId == Just task.id)

            remainingSubtasks =
                childTasks |> List.filter (\t -> t.status == Api.Todo || t.status == Api.InProgress || t.status == Api.Blocked) |> List.length

            completedSubtasks =
                List.length childTasks - remainingSubtasks

            depCount =
                task.dependencyCount

            memCount =
                task.memoryLinkCount

            subtaskLabel =
                let
                    total =
                        remainingSubtasks + completedSubtasks
                in
                if total == 0 then
                    Nothing

                else if completedSubtasks == 0 then
                    Just (String.fromInt total ++ " subtask" ++ (if total > 1 then "s" else ""))

                else if remainingSubtasks == 0 then
                    Just (String.fromInt total ++ " subtask" ++ (if total > 1 then "s" else "") ++ " (all done)")

                else
                    Just (String.fromInt remainingSubtasks ++ "/" ++ String.fromInt total ++ " subtasks remaining")

            summaryParts =
                List.filterMap identity
                    [ subtaskLabel
                    , if depCount > 0 then
                        Just (String.fromInt depCount ++ " dep" ++ (if depCount > 1 then "s" else ""))

                      else
                        Nothing
                    , if memCount > 0 then
                        Just (String.fromInt memCount ++ " memor" ++ (if memCount > 1 then "ies" else "y"))

                      else
                        Nothing
                    ]
          in
          if List.isEmpty summaryParts then
            text ""

          else
            div [ class "card-summary" ] [ text (String.join " · " summaryParts) ]
        , let
            deps =
                Dict.get task.id model.taskDependencies |> Maybe.withDefault []

            extrasExpanded =
                isExpanded model task.id
          in
          div [ class "card-body card-expanded" ]
            [ div [ class "card-desc-row" ]
                [ button [ class "btn-extras-toggle", onClick (ToggleCardExpand task.id) ]
                    [ text
                        (if extrasExpanded then
                            "−"

                         else
                            "+"
                        )
                    ]
                , viewEditableTextarea model "task" task.id "description" (Maybe.withDefault "" task.description)
                ]
            , if extrasExpanded then
                div [ class "card-extras" ]
                    [ if showProject then
                        case projectName of
                            Just pname ->
                                div [ class "card-meta" ] [ text ("Project: " ++ pname) ]

                            Nothing ->
                                text ""

                      else
                        text ""
                    , viewTaskDependencies model task.id deps
                    , viewLinkedMemories model "task" task.id linkedMems
                    ]

              else
                text ""
            ]
        , div [ class "card-inline-actions" ]
            [ button
                [ class "btn-inline-create"
                , onClick (ShowInlineCreate (InlineCreateTask { projectId = task.projectId, parentId = Just task.id, title = "" }))
                ]
                [ text "+ Subtask" ]
            ]
        , viewInlineCreateInputForParent model (Just task.id) "subtask"
        , div [ class "card-meta-group" ]
            [ div [ class "card-meta-row" ]
                [ span [ class "card-meta" ] [ text ("Created: " ++ formatDate task.createdAt) ]
                , span [ class "card-meta" ] [ text ("Updated: " ++ formatDate task.updatedAt) ]
                , span [ class "card-meta card-id card-id-copy", onClick (CopyId task.id) ] [ text task.id ]
                ]
            , div [ class "card-meta-row" ]
                [ case task.dueAt of
                    Just due ->
                        span [ class "card-meta card-meta-due" ] [ text ("Due: " ++ formatDate due) ]

                    Nothing ->
                        text ""
                , case task.completedAt of
                    Just completed ->
                        span [ class "card-meta" ] [ text ("Completed: " ++ formatDate completed) ]

                    Nothing ->
                        text ""
                ]
            ]
        , if hasChildren && not collapsed then
            let
                childTasks =
                    model.tasks
                        |> Dict.values
                        |> List.filter (\t -> t.parentId == Just task.id)
                        |> List.sortBy (\t -> ( Api.taskStatusOrder t.status, negate t.priority, String.toLower t.title ))
            in
            Keyed.node "div" [ class "tree-children" ]
                (viewTasksWithZones model "task-subtasks" task.projectId (Just task.id) childTasks)

          else
            text ""
        ]



-- MEMORY LIST


viewMemoriesList : String -> Model -> Html Msg
viewMemoriesList wsId model =
    let
        query =
            String.toLower (String.trim model.searchQuery)

        hasSearch =
            not (String.isEmpty query)

        memoryPassesTypeFilter m =
            if List.isEmpty model.filterMemoryTypes then
                True

            else
                List.member (Api.memoryTypeToString m.memoryType) model.filterMemoryTypes

        memoryPassesPinnedFilter m =
            case model.filterMemoryPinned of
                Nothing ->
                    True

                Just pinned ->
                    m.pinned == pinned

        memoryPassesImportanceFilter m =
            passesPriorityFilter model.filterImportance m.importance

        memoryPassesTagFilter m =
            if List.isEmpty model.filterTags then
                True

            else
                List.any (\t -> List.member t m.tags) model.filterTags

        allWsMemories =
            model.memories
                |> Dict.values
                |> List.filter (\m -> m.workspaceId == wsId)

        allTags =
            allWsMemories
                |> List.concatMap .tags
                |> List.sort
                |> uniqueStrings

        wsMemories =
            allWsMemories
                |> (if hasSearch then
                        List.filter
                            (\m ->
                                String.contains query (String.toLower m.content)
                                    || (m.summary |> Maybe.map (\s -> String.contains query (String.toLower s)) |> Maybe.withDefault False)
                                    || List.any (\t -> String.contains query (String.toLower t)) m.tags
                            )

                    else
                        identity
                   )
                |> List.filter memoryPassesTypeFilter
                |> List.filter memoryPassesPinnedFilter
                |> List.filter memoryPassesImportanceFilter
                |> List.filter memoryPassesTagFilter
                |> List.sortBy (\m -> ( negate m.importance, String.toLower (m.summary |> Maybe.withDefault m.content) ))

        inlineCreateView =
            viewInlineCreateMemory model
    in
    Keyed.node "div" [ class "entity-list" ]
        (( "tag-cloud", viewTagCloud model allTags )
            :: ( "inline-create-memory", inlineCreateView )
            :: (if List.isEmpty wsMemories then
                    [ ( "empty-state", div [ class "empty-state" ] [ text "No memories yet." ] ) ]

                else
                    List.map (\m -> ( m.id, viewMemoryCard model m )) wsMemories
               )
        )


viewTagCloud : Model -> List String -> Html Msg
viewTagCloud model allTags =
    if List.isEmpty allTags then
        text ""

    else
        div [ class "tag-cloud" ]
            [ div [ class "tag-cloud-tags" ]
                (List.map
                    (\tag ->
                        button
                            [ class
                                (if List.member tag model.filterTags then
                                    "tag-cloud-tag tag-cloud-tag-active"

                                 else
                                    "tag-cloud-tag"
                                )
                            , onClick (ToggleFilterTag tag)
                            ]
                            [ text tag ]
                    )
                    allTags
                )
            ]


viewMemoryCard : Model -> Api.Memory -> Html Msg
viewMemoryCard model memory =
    let
        extrasExpanded =
            isExpanded model memory.id

        linkedEntities =
            model.entityMemories
                |> Dict.toList
                |> List.filterMap
                    (\( entityId, mems ) ->
                        if List.any (\m -> m.id == memory.id) mems then
                            Just entityId

                        else
                            Nothing
                    )

        linkedProjects =
            List.filterMap (\eid -> Dict.get eid model.projects) linkedEntities

        linkedTasks =
            List.filterMap (\eid -> Dict.get eid model.tasks) linkedEntities
    in
    div
        [ class ("card" ++ dragOverClass model memory.id)
        , draggable "true"
        , on "dragstart" (Decode.succeed (DragStartCard "memory" memory.id))
        , preventDefaultOn "dragover" (Decode.succeed ( DragOverCard memory.id, True ))
        , preventDefaultOn "drop" (Decode.succeed ( DropOnCard "memory" memory.id, True ))
        , on "dragend" (Decode.succeed DragEndCard)
        ]
        [ div [ class "card-header" ]
            [ case editingValue model memory.id "summary" of
                Just val ->
                    input
                        [ class "inline-edit-input"
                        , value val
                        , onInput EditInput
                        , onBlur (SaveEdit memory.id "summary")
                        , onKeyDown
                            (\keyCode ->
                                if keyCode == 13 then
                                    SaveEdit memory.id "summary"

                                else if keyCode == 27 then
                                    CancelEdit

                                else
                                    NoOp
                            )
                        , Html.Attributes.id (editElementId memory.id "summary")
                        ]
                        []

                Nothing ->
                    span
                        [ class "editable-text"
                        , onClick (StartEdit "memory" memory.id "summary" (Maybe.withDefault "" memory.summary))
                        , title "Click to edit summary"
                        ]
                        [ text
                            (case memory.summary of
                                Just s ->
                                    if String.isEmpty s then
                                        "<untitled>"

                                    else
                                        s

                                Nothing ->
                                    "<untitled>"
                            )
                        ]
            , div [ class "card-actions" ]
                [ viewMemoryTypeSelect memory.id memory.memoryType
                , viewImportanceSelect memory.id memory.importance
                , button
                    [ class
                        (if memory.pinned then
                            "btn-icon btn-pinned"

                         else
                            "btn-icon"
                        )
                    , onClick (ToggleMemoryPin memory.id (not memory.pinned))
                    , title "Pin"
                    ]
                    [ text "\u{1F4CC}" ]
                , button [ class "btn-icon btn-danger", onClick (ConfirmDelete "memory" memory.id), title "Delete" ] [ text "✕" ]
                ]
            ]
        , div [ class "card-body card-expanded" ]
            [ div [ class "card-desc-row" ]
                [ button [ class "btn-extras-toggle", onClick (ToggleCardExpand memory.id) ]
                    [ text
                        (if extrasExpanded then
                            "−"

                         else
                            "+"
                        )
                    ]
                , viewEditableTextarea model "memory" memory.id "content" memory.content
                ]
            , if extrasExpanded then
                div [ class "card-extras" ]
                    [ viewTagEditor model memory
                    , viewMemoryLinkedEntities model memory.id linkedProjects linkedTasks
                    ]

              else if not (List.isEmpty linkedProjects) || not (List.isEmpty linkedTasks) then
                div [ class "card-collapsed-badges" ]
                    [ span [ class "linked-memories-badge" ]
                        [ text ("🔗 " ++ String.fromInt (List.length linkedProjects + List.length linkedTasks) ++ " linked") ]
                    ]

              else
                text ""
            ]
        , div [ class "card-meta-group" ]
            [ div [ class "card-meta-row" ]
                [ span [ class "card-meta" ] [ text ("Created: " ++ formatDate memory.createdAt) ]
                , span [ class "card-meta" ] [ text ("Updated: " ++ formatDate memory.updatedAt) ]
                , span [ class "card-meta card-id card-id-copy", onClick (CopyId memory.id) ] [ text memory.id ]
                ]
            ]
        ]


viewMemoryLinkedEntities : Model -> String -> List Api.Project -> List Api.Task -> Html Msg
viewMemoryLinkedEntities model memoryId projects tasks =
    let
        items =
            List.map
                (\p ->
                    div [ class "linked-entity-item" ]
                        [ span [ class "entity-type-label entity-type-project" ] [ text "PRJ" ]
                        , span [ class "linked-entity-name", onClick (FocusEntity "project" p.id) ] [ text p.name ]
                        , button
                            [ class "btn-icon btn-danger"
                            , onClick (PerformUnlinkEntity "project" p.id memoryId)
                            , title "Unlink"
                            ]
                            [ text "✕" ]
                        ]
                )
                projects
                ++ List.map
                    (\t ->
                        div [ class "linked-entity-item" ]
                            [ span [ class "entity-type-label entity-type-task" ] [ text "TSK" ]
                            , span [ class "linked-entity-name", onClick (FocusEntity "task" t.id) ] [ text t.title ]
                            , button
                                [ class "btn-icon btn-danger"
                                , onClick (PerformUnlinkEntity "task" t.id memoryId)
                                , title "Unlink"
                                ]
                                [ text "✕" ]
                            ]
                    )
                    tasks

        selectorOpen =
            case model.linkingEntityFor of
                Just st ->
                    st.entityId == memoryId

                Nothing ->
                    False
    in
    div [ class "linked-entities-section" ]
        [ div [ class "linked-entities-title" ]
            [ text ("Linked Entities (" ++ String.fromInt (List.length items) ++ ")") ]
        , if List.isEmpty items then
            div [ class "linked-memories-empty" ] [ text "No linked entities" ]

          else
            div [ class "linked-entities-list" ] items
        , div [ class "card-inline-actions" ]
            [ div [ class "popover-anchor" ]
                [ button
                    [ class
                        (if selectorOpen then
                            "btn-inline-create popover-trigger-active"

                         else
                            "btn-inline-create"
                        )
                    , onClick
                        (if selectorOpen then
                            CancelLinkEntity

                         else
                            StartLinkEntity memoryId
                        )
                    ]
                    [ text "+ Link" ]
                , viewLinkEntityPopover model memoryId projects tasks
                ]
            ]
        ]


viewLinkEntityPopover : Model -> String -> List Api.Project -> List Api.Task -> Html Msg
viewLinkEntityPopover model memoryId linkedProjects linkedTasks =
    case model.linkingEntityFor of
        Just st ->
            if st.entityId == memoryId then
                let
                    linkedProjectIds =
                        List.map .id linkedProjects

                    linkedTaskIds =
                        List.map .id linkedTasks

                    query =
                        String.toLower st.search

                    availableProjects =
                        model.projects
                            |> Dict.values
                            |> List.filter (\p -> not (List.member p.id linkedProjectIds))
                            |> List.filter
                                (\p ->
                                    if String.isEmpty query then
                                        True

                                    else
                                        let
                                            crumbText =
                                                buildProjectBreadcrumb model p []
                                                    |> List.map (\( _, label, _ ) -> String.toLower label)
                                                    |> String.join " "
                                        in
                                        String.contains query (String.toLower p.name)
                                            || (p.description |> Maybe.map (\d -> String.contains query (String.toLower d)) |> Maybe.withDefault False)
                                            || String.contains query crumbText
                                )
                            |> List.sortBy (\p -> ( Api.projectStatusOrder p.status, negate p.priority, String.toLower p.name ))
                            |> List.take 10

                    availableTasks =
                        model.tasks
                            |> Dict.values
                            |> List.filter (\t -> not (List.member t.id linkedTaskIds))
                            |> List.filter
                                (\t ->
                                    if String.isEmpty query then
                                        True

                                    else
                                        let
                                            crumbText =
                                                buildTaskBreadcrumb model t []
                                                    |> List.map (\( _, label, _ ) -> String.toLower label)
                                                    |> String.join " "
                                        in
                                        String.contains query (String.toLower t.title)
                                            || (t.description |> Maybe.map (\d -> String.contains query (String.toLower d)) |> Maybe.withDefault False)
                                            || String.contains query crumbText
                                )
                            |> List.sortBy (\t -> ( Api.taskStatusOrder t.status, negate t.priority, String.toLower t.title ))
                            |> List.take 10

                    allItems =
                        List.map
                            (\p ->
                                let
                                    crumbs =
                                        buildProjectBreadcrumb model p []
                                            |> List.filter (\( eid, _, _ ) -> eid /= p.id)
                                in
                                div
                                    [ class "popover-card"
                                    , onClick (PerformLinkEntity "project" p.id memoryId)
                                    ]
                                    [ div [ class "popover-card-header" ]
                                        [ span [ class "entity-type-label entity-type-project" ] [ text "PRJ" ]
                                        , span [ class "popover-card-title" ] [ text p.name ]
                                        ]
                                    , if not (List.isEmpty crumbs) then
                                        div [ class "popover-card-breadcrumb" ]
                                            (List.intersperse (span [] [ text " › " ])
                                                (List.map (\( _, label, _ ) -> span [] [ text label ]) crumbs)
                                            )

                                      else
                                        text ""
                                    , div [ class "popover-card-meta" ]
                                        [ span [ class ("popover-card-status card-status-" ++ Api.projectStatusToString p.status) ]
                                            [ text (Api.projectStatusToString p.status) ]
                                        , case p.description of
                                            Just d ->
                                                span [ class "popover-card-desc" ] [ text d ]

                                            Nothing ->
                                                text ""
                                        ]
                                    ]
                            )
                            availableProjects
                            ++ List.map
                                (\t ->
                                    let
                                        crumbs =
                                            buildTaskBreadcrumb model t []
                                                |> List.filter (\( eid, _, _ ) -> eid /= t.id)
                                    in
                                    div
                                        [ class "popover-card"
                                        , onClick (PerformLinkEntity "task" t.id memoryId)
                                        ]
                                        [ div [ class "popover-card-header" ]
                                            [ span
                                                [ class
                                                    ("entity-type-label "
                                                        ++ (if t.parentId /= Nothing then
                                                                "entity-type-subtask"

                                                            else
                                                                "entity-type-task"
                                                           )
                                                    )
                                                ]
                                                [ text
                                                    (if t.parentId /= Nothing then
                                                        "SUB"

                                                     else
                                                        "TSK"
                                                    )
                                                ]
                                            , span [ class "popover-card-title" ] [ text t.title ]
                                            ]
                                        , if not (List.isEmpty crumbs) then
                                            div [ class "popover-card-breadcrumb" ]
                                                (List.intersperse (span [] [ text " › " ])
                                                    (List.map (\( _, label, _ ) -> span [] [ text label ]) crumbs)
                                                )

                                          else
                                            text ""
                                        , div [ class "popover-card-meta" ]
                                            [ span [ class ("popover-card-status card-status-" ++ Api.taskStatusToString t.status) ]
                                                [ text (Api.taskStatusToString t.status |> String.replace "_" " ") ]
                                            , span [ class "popover-card-priority" ] [ text ("P" ++ String.fromInt t.priority) ]
                                            , case t.description of
                                                Just d ->
                                                    span [ class "popover-card-desc" ] [ text d ]

                                                Nothing ->
                                                    text ""
                                            ]
                                        ]
                                )
                                availableTasks
                in
                div [ class "popover-container" ]
                    [ div [ class "popover-overlay", onClick CancelLinkEntity ] []
                    , div [ class "popover-menu", stopPropagationOn "click" (Decode.succeed ( NoOp, True )) ]
                        [ input
                            [ class "popover-search"
                            , placeholder "Search projects & tasks..."
                            , value st.search
                            , onInput LinkEntitySearch
                            , autofocus True
                            ]
                            []
                        , div [ class "popover-results" ]
                            (if List.isEmpty allItems then
                                [ div [ class "popover-empty" ] [ text "No matching entities" ] ]

                             else
                                allItems
                            )
                        ]
                    ]

            else
                text ""

        Nothing ->
            text ""



-- INLINE EDITABLE FIELDS


viewEditableText : Model -> String -> String -> String -> String -> Html Msg
viewEditableText model entityType entityId field currentValue =
    case editingValue model entityId field of
        Just val ->
            input
                [ class "inline-edit-input"
                , value val
                , onInput EditInput
                , onBlur (SaveEdit entityId field)
                , onKeyDown
                    (\keyCode ->
                        if keyCode == 13 then
                            SaveEdit entityId field

                        else if keyCode == 27 then
                            CancelEdit

                        else
                            NoOp
                    )
                , Html.Attributes.id (editElementId entityId field)
                ]
                []

        Nothing ->
            span
                [ class "editable-text"
                , onClick (StartEdit entityType entityId field currentValue)
                , title "Click to edit"
                ]
                [ text
                    (if String.isEmpty currentValue then
                        "(empty)"

                     else
                        currentValue
                    )
                ]


viewMarkdownContent : String -> Html Msg
viewMarkdownContent raw =
    case
        raw
            |> Markdown.Parser.parse
            |> Result.mapError (\_ -> "parse error")
            |> Result.andThen (Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer)
    of
        Ok rendered ->
            div [ class "markdown-content" ] rendered

        Err _ ->
            div [ class "markdown-content", style "white-space" "pre-wrap" ] [ text raw ]


viewEditableTextarea : Model -> String -> String -> String -> String -> Html Msg
viewEditableTextarea model entityType entityId field currentValue =
    case editingValue model entityId field of
        Just val ->
            let
                lineCount =
                    val
                        |> String.split "\n"
                        |> List.length

                rowCount =
                    Basics.max 10 (lineCount + 1)
            in
            textarea
                [ class "inline-edit-textarea"
                , value val
                , onInput EditInput
                , onBlur (SaveEdit entityId field)
                , rows rowCount
                , Html.Attributes.id (editElementId entityId field)
                ]
                []

        Nothing ->
            div
                [ class "editable-textarea"
                , onClick (StartEdit entityType entityId field currentValue)
                , title "Click to edit"
                ]
                [ if String.isEmpty currentValue then
                    text "Click to add description..."

                  else
                    viewMarkdownContent currentValue
                ]


onKeyDown : (Int -> Msg) -> Attribute Msg
onKeyDown toMsg =
    on "keydown" (Decode.map toMsg (Decode.field "keyCode" Decode.int))



-- STATUS / PRIORITY SELECTORS


viewStatusSelect : String -> String -> String -> List a -> (a -> String) -> (String -> a -> Msg) -> Html Msg
viewStatusSelect entityType entityId currentStr allValues toString toMsg =
    select
        [ class ("status-select badge badge-" ++ currentStr)
        , onInput
            (\s ->
                let
                    matched =
                        allValues |> List.filter (\v -> toString v == s) |> List.head
                in
                case matched of
                    Just v ->
                        toMsg entityId v

                    Nothing ->
                        NoOp
            )
        ]
        (List.map
            (\v ->
                let
                    str =
                        toString v
                in
                option [ value str, selected (str == currentStr) ]
                    [ text (str |> String.replace "_" " ") ]
            )
            allValues
        )


viewPrioritySelect : String -> String -> Int -> (String -> Int -> Msg) -> Html Msg
viewPrioritySelect entityType entityId currentPri toMsg =
    select
        [ class "priority-select"
        , title "Priority"
        , onInput
            (\s ->
                case String.toInt s of
                    Just n ->
                        toMsg entityId n

                    Nothing ->
                        NoOp
            )
        ]
        (List.map
            (\n ->
                option [ value (String.fromInt n), selected (n == currentPri) ]
                    [ text ("P" ++ String.fromInt n) ]
            )
            (List.range 1 10)
        )


viewImportanceSelect : String -> Int -> Html Msg
viewImportanceSelect memId currentImp =
    select
        [ class "priority-select"
        , title "Importance"
        , onInput
            (\s ->
                case String.toInt s of
                    Just n ->
                        ChangeMemoryImportance memId n

                    Nothing ->
                        NoOp
            )
        ]
        (List.map
            (\n ->
                option [ value (String.fromInt n), selected (n == currentImp) ]
                    [ text ("★" ++ String.fromInt n) ]
            )
            (List.range 1 10)
        )


viewMemoryTypeSelect : String -> Api.MemoryType -> Html Msg
viewMemoryTypeSelect memId currentType =
    select
        [ class ("status-select badge badge-" ++ Api.memoryTypeToString currentType)
        , onInput
            (\s ->
                ChangeMemoryType memId (Api.memoryTypeFromString s)
            )
        ]
        (List.map
            (\mt ->
                let
                    str =
                        Api.memoryTypeToString mt
                in
                option [ value str, selected (mt == currentType) ]
                    [ text (str |> String.replace "_" " ") ]
            )
            Api.allMemoryTypes
        )



-- TAG EDITOR


viewTagEditor : Model -> Api.Memory -> Html Msg
viewTagEditor model memory =
    div [ class "tag-editor" ]
        [ div [ class "tag-list" ]
            (List.map
                (\t ->
                    span [ class "tag tag-removable" ]
                        [ text t
                        , button [ class "tag-remove", onClick (RemoveTag memory.id t) ] [ text "x" ]
                        ]
                )
                memory.tags
            )
        , case editingValue model memory.id "tags" of
            Just val ->
                input
                    [ class "tag-input"
                    , placeholder "New tag..."
                    , value val
                    , onInput EditInput
                    , onBlur (SaveEdit memory.id "tags")
                    , onKeyDown
                        (\keyCode ->
                            if keyCode == 13 then
                                AddTag memory.id val

                            else if keyCode == 27 then
                                CancelEdit

                            else
                                NoOp
                        )
                    , autofocus True
                    ]
                    []

            Nothing ->
                button [ class "btn-icon btn-add-tag", onClick (StartEdit "memory" memory.id "tags" "") ]
                    [ text "+ tag" ]
        ]



-- BREADCRUMB NAVIGATION


viewTaskBreadcrumb : Model -> Api.Task -> Html Msg
viewTaskBreadcrumb model task =
    let
        crumbs =
            buildTaskBreadcrumb model task []
    in
    if List.length crumbs <= 1 then
        text ""

    else
        div [ class "breadcrumb" ]
            (List.intersperse (span [ class "breadcrumb-sep" ] [ text "›" ])
                (List.map
                    (\( eid, label, etype ) ->
                        span
                            [ class ("breadcrumb-item breadcrumb-" ++ etype)
                            , onClick (FocusEntity etype eid)
                            , title ("Jump to " ++ label)
                            ]
                            [ text label ]
                    )
                    crumbs
                )
            )


buildTaskBreadcrumb : Model -> Api.Task -> List ( String, String, String ) -> List ( String, String, String )
buildTaskBreadcrumb model task acc =
    let
        currentCrumb =
            ( task.id, task.title, "task" )

        withParentTask =
            case task.parentId of
                Just pid ->
                    case Dict.get pid model.tasks of
                        Just parentTask ->
                            buildTaskBreadcrumb model parentTask (currentCrumb :: acc)

                        Nothing ->
                            currentCrumb :: acc

                Nothing ->
                    currentCrumb :: acc
    in
    case task.projectId of
        Just projId ->
            case Dict.get projId model.projects of
                Just proj ->
                    buildProjectBreadcrumb model proj withParentTask

                Nothing ->
                    withParentTask

        Nothing ->
            withParentTask


buildProjectBreadcrumb : Model -> Api.Project -> List ( String, String, String ) -> List ( String, String, String )
buildProjectBreadcrumb model project acc =
    let
        currentCrumb =
            ( project.id, project.name, "project" )
    in
    case project.parentId of
        Just pid ->
            case Dict.get pid model.projects of
                Just parentProj ->
                    buildProjectBreadcrumb model parentProj (currentCrumb :: acc)

                Nothing ->
                    currentCrumb :: acc

        Nothing ->
            currentCrumb :: acc



-- TASK DEPENDENCIES


viewTaskDependencies : Model -> String -> List Api.TaskDependencySummary -> Html Msg
viewTaskDependencies model taskId deps =
    let
        selectorOpen =
            case model.addingDependencyFor of
                Just st ->
                    st.taskId == taskId

                Nothing ->
                    False
    in
    div [ class "task-dependencies-section" ]
        [ div [ class "task-dependencies-header" ]
            [ span [ class "task-dependencies-title" ] [ text ("Dependencies (" ++ String.fromInt (List.length deps) ++ ")") ]
            ]
        , if List.isEmpty deps then
            div [ class "task-dependencies-empty" ] [ text "No dependencies" ]

          else
            div [ class "task-dependencies-list" ]
                (List.map (viewDependencyItem model taskId) deps)
        , div [ class "card-inline-actions" ]
            [ div [ class "popover-anchor" ]
                [ button
                    [ class
                        (if selectorOpen then
                            "btn-inline-create popover-trigger-active"

                         else
                            "btn-inline-create"
                        )
                    , onClick
                        (if selectorOpen then
                            CancelAddDependency

                         else
                            StartAddDependency taskId
                        )
                    ]
                    [ text "+ Dep" ]
                , viewAddDependencyPopover model taskId deps
                ]
            ]
        ]


viewAddDependencyPopover : Model -> String -> List Api.TaskDependencySummary -> Html Msg
viewAddDependencyPopover model taskId deps =
    case model.addingDependencyFor of
        Just st ->
            if st.taskId == taskId then
                let
                    depIds =
                        List.map .id deps

                    query =
                        String.toLower st.search

                    availableTasks =
                        model.tasks
                            |> Dict.values
                            |> List.filter (\t -> t.id /= taskId && not (List.member t.id depIds))
                            |> List.filter
                                (\t ->
                                    if String.isEmpty query then
                                        True

                                    else
                                        let
                                            crumbText =
                                                buildTaskBreadcrumb model t []
                                                    |> List.map (\( _, label, _ ) -> String.toLower label)
                                                    |> String.join " "
                                        in
                                        String.contains query (String.toLower t.title)
                                            || (t.description |> Maybe.map (\d -> String.contains query (String.toLower d)) |> Maybe.withDefault False)
                                            || String.contains query crumbText
                                )
                            |> List.sortBy (\t -> ( Api.taskStatusOrder t.status, negate t.priority, String.toLower t.title ))
                            |> List.take 15
                in
                div [ class "popover-container" ]
                    [ div [ class "popover-overlay", onClick CancelAddDependency ] []
                    , div [ class "popover-menu", stopPropagationOn "click" (Decode.succeed ( NoOp, True )) ]
                        [ input
                            [ class "popover-search"
                            , placeholder "Search tasks..."
                            , value st.search
                            , onInput DependencySearch
                            , autofocus True
                            ]
                            []
                        , div [ class "popover-results" ]
                            (if List.isEmpty availableTasks then
                                [ div [ class "popover-empty" ] [ text "No matching tasks" ] ]

                             else
                                List.map
                                    (\t ->
                                        let
                                            crumbs =
                                                buildTaskBreadcrumb model t []
                                                    |> List.filter (\( eid, _, _ ) -> eid /= t.id)
                                        in
                                        div
                                            [ class "popover-card"
                                            , onClick (PerformAddDependency taskId t.id)
                                            ]
                                            [ div [ class "popover-card-header" ]
                                                [ span [ class ("entity-type-label " ++ (if t.parentId /= Nothing then "entity-type-subtask" else "entity-type-task")) ]
                                                    [ text
                                                        (if t.parentId /= Nothing then
                                                            "SUB"

                                                         else
                                                            "TSK"
                                                        )
                                                    ]
                                                , span [ class "popover-card-title" ] [ text t.title ]
                                                ]
                                            , if not (List.isEmpty crumbs) then
                                                div [ class "popover-card-breadcrumb" ]
                                                    (List.intersperse (span [] [ text " › " ])
                                                        (List.map (\( _, label, _ ) -> span [] [ text label ]) crumbs)
                                                    )

                                              else
                                                text ""
                                            , div [ class "popover-card-meta" ]
                                                [ span [ class ("popover-card-status card-status-" ++ Api.taskStatusToString t.status) ]
                                                    [ text (Api.taskStatusToString t.status |> String.replace "_" " ") ]
                                                , span [ class "popover-card-priority" ] [ text ("P" ++ String.fromInt t.priority) ]
                                                , case t.description of
                                                    Just d ->
                                                        span [ class "popover-card-desc" ] [ text d ]

                                                    Nothing ->
                                                        text ""
                                                ]
                                            ]
                                    )
                                    availableTasks
                            )
                        ]
                    ]

            else
                text ""

        Nothing ->
            text ""


viewDependencyItem : Model -> String -> Api.TaskDependencySummary -> Html Msg
viewDependencyItem model taskId dep =
    let
        depTask =
            Dict.get dep.id model.tasks
    in
    div [ class "dep-item popover-card" ]
        (case depTask of
            Just t ->
                let
                    crumbs =
                        buildTaskBreadcrumb model t []
                            |> List.filter (\( eid, _, _ ) -> eid /= t.id)
                in
                [ div [ class "popover-card-header" ]
                    [ span [ class ("entity-type-label " ++ (if t.parentId /= Nothing then "entity-type-subtask" else "entity-type-task")) ]
                        [ text
                            (if t.parentId /= Nothing then
                                "SUB"

                             else
                                "TSK"
                            )
                        ]
                    , span [ class "popover-card-title" ] [ text t.title ]
                    , div [ class "dep-item-actions" ]
                        [ button
                            [ class "btn-icon btn-jump"
                            , onClick (FocusEntity "task" dep.id)
                            , title "Jump to task"
                            ]
                            [ text "↗" ]
                        , button
                            [ class "btn-icon btn-danger"
                            , onClick (PerformRemoveDependency taskId dep.id)
                            , title "Remove dependency"
                            ]
                            [ text "✕" ]
                        ]
                    ]
                , if not (List.isEmpty crumbs) then
                    div [ class "popover-card-breadcrumb" ]
                        (List.intersperse (span [] [ text " › " ])
                            (List.map (\( _, label, _ ) -> span [] [ text label ]) crumbs)
                        )

                  else
                    text ""
                , div [ class "popover-card-meta" ]
                    [ span [ class ("popover-card-status card-status-" ++ Api.taskStatusToString t.status) ]
                        [ text (Api.taskStatusToString t.status |> String.replace "_" " ") ]
                    , span [ class "popover-card-priority" ] [ text ("P" ++ String.fromInt t.priority) ]
                    , case t.description of
                        Just d ->
                            span [ class "popover-card-desc" ] [ text d ]

                        Nothing ->
                            text ""
                    ]
                ]

            Nothing ->
                [ div [ class "popover-card-header" ]
                    [ span [ class "entity-type-label entity-type-task" ] [ text "TSK" ]
                    , span [ class "popover-card-title" ] [ text dep.name ]
                    , div [ class "dep-item-actions" ]
                        [ button
                            [ class "btn-icon btn-jump"
                            , onClick (FocusEntity "task" dep.id)
                            , title "Jump to task"
                            ]
                            [ text "↗" ]
                        , button
                            [ class "btn-icon btn-danger"
                            , onClick (PerformRemoveDependency taskId dep.id)
                            , title "Remove dependency"
                            ]
                            [ text "✕" ]
                        ]
                    ]
                ]
        )



-- INLINE CREATE INPUTS


viewInlineCreateMemory : Model -> Html Msg
viewInlineCreateMemory model =
    case model.inlineCreate of
        Just (InlineCreateMemory f) ->
            div [ class "inline-create-row" ]
                [ input
                    [ class "inline-create-input"
                    , Html.Attributes.id "inline-create-input"
                    , placeholder "New memory content..."
                    , value f.content
                    , onInput (\s -> UpdateInlineCreate (InlineCreateMemory { f | content = s }))
                    , onBlur CancelInlineCreate
                    , onKeyDown
                        (\keyCode ->
                            if keyCode == 13 then
                                SubmitInlineCreate

                            else if keyCode == 27 then
                                CancelInlineCreate

                            else
                                NoOp
                        )
                    ]
                    []
                ]

        _ ->
            div [ class "inline-create-row" ]
                [ button
                    [ class "btn-inline-create-top"
                    , onClick (ShowInlineCreate (InlineCreateMemory { content = "" }))
                    ]
                    [ text "+ New Memory" ]
                ]


viewInlineCreateInput : Model -> Maybe String -> String -> Html Msg
viewInlineCreateInput model parentId inputType =
    let
        isActive =
            case model.inlineCreate of
                Just (InlineCreateProject ic) ->
                    inputType == "project" && ic.parentId == parentId

                _ ->
                    False
    in
    if isActive then
        case model.inlineCreate of
            Just (InlineCreateProject f) ->
                div [ class "inline-create-row" ]
                    [ input
                        [ class "inline-create-input"
                        , Html.Attributes.id "inline-create-input"
                        , placeholder "New project name..."
                        , value f.name
                        , onInput (\s -> UpdateInlineCreate (InlineCreateProject { f | name = s }))
                        , onBlur CancelInlineCreate
                        , onKeyDown
                            (\keyCode ->
                                if keyCode == 13 then
                                    SubmitInlineCreate

                                else if keyCode == 27 then
                                    CancelInlineCreate

                                else
                                    NoOp
                            )
                        ]
                        []
                    ]

            _ ->
                text ""

    else if inputType == "project" && parentId == Nothing then
        div [ class "inline-create-row" ]
            [ button
                [ class "btn-inline-create-top"
                , onClick (ShowInlineCreate (InlineCreateProject { parentId = Nothing, name = "" }))
                ]
                [ text "+ New Project" ]
            ]

    else
        text ""


viewInlineCreateInputForParent : Model -> Maybe String -> String -> Html Msg
viewInlineCreateInputForParent model parentId inputType =
    case model.inlineCreate of
        Just (InlineCreateProject f) ->
            if inputType == "project" && f.parentId == parentId then
                div [ class "inline-create-row" ]
                    [ input
                        [ class "inline-create-input"
                        , Html.Attributes.id "inline-create-input"
                        , placeholder "Subproject name..."
                        , value f.name
                        , onInput (\s -> UpdateInlineCreate (InlineCreateProject { f | name = s }))
                        , onBlur CancelInlineCreate
                        , onKeyDown
                            (\keyCode ->
                                if keyCode == 13 then
                                    SubmitInlineCreate

                                else if keyCode == 27 then
                                    CancelInlineCreate

                                else
                                    NoOp
                            )
                        ]
                        []
                    ]

            else
                text ""

        Just (InlineCreateTask f) ->
            if (inputType == "task" && f.projectId == parentId && f.parentId == Nothing)
                || (inputType == "subtask" && f.parentId == parentId)
            then
                div [ class "inline-create-row" ]
                    [ input
                        [ class "inline-create-input"
                        , Html.Attributes.id "inline-create-input"
                        , placeholder
                            (if inputType == "subtask" then
                                "Subtask title..."

                             else
                                "Task title..."
                            )
                        , value f.title
                        , onInput (\s -> UpdateInlineCreate (InlineCreateTask { f | title = s }))
                        , onBlur CancelInlineCreate
                        , onKeyDown
                            (\keyCode ->
                                if keyCode == 13 then
                                    SubmitInlineCreate

                                else if keyCode == 27 then
                                    CancelInlineCreate

                                else
                                    NoOp
                            )
                        ]
                        []
                    ]

            else
                text ""

        Nothing ->
            text ""

        _ ->
            text ""



-- LINKED MEMORIES


viewLinkedMemories : Model -> String -> String -> List Api.Memory -> Html Msg
viewLinkedMemories model entityType entityId linkedMems =
    let
        selectorOpen =
            case model.linkingMemoryFor of
                Just st ->
                    st.entityType == entityType && st.entityId == entityId

                Nothing ->
                    False
    in
    div [ class "linked-memories-section" ]
        [ div [ class "linked-memories-header" ]
            [ span [ class "linked-memories-title" ] [ text ("Linked Memories (" ++ String.fromInt (List.length linkedMems) ++ ")") ]
            ]
        , if List.isEmpty linkedMems then
            div [ class "linked-memories-empty" ] [ text "No linked memories" ]

          else
            div [ class "linked-memories-list" ]
                (List.map (viewLinkedMemoryItem model entityType entityId) linkedMems)
        , div [ class "card-inline-actions" ]
            [ div [ class "popover-anchor" ]
                [ button
                    [ class
                        (if selectorOpen then
                            "btn-inline-create popover-trigger-active"

                         else
                            "btn-inline-create"
                        )
                    , onClick
                        (if selectorOpen then
                            CancelLinkMemory

                         else
                            StartLinkMemory entityType entityId
                        )
                    ]
                    [ text "+ Link" ]
                , viewLinkMemoryPopover model entityType entityId linkedMems
                ]
            ]
        ]


viewLinkMemoryPopover : Model -> String -> String -> List Api.Memory -> Html Msg
viewLinkMemoryPopover model entityType entityId linkedMems =
    case model.linkingMemoryFor of
        Just st ->
            if st.entityType == entityType && st.entityId == entityId then
                let
                    linkedIds =
                        List.map .id linkedMems

                    query =
                        String.toLower st.search

                    availableMemories =
                        model.memories
                            |> Dict.values
                            |> List.filter (\m -> not (List.member m.id linkedIds))
                            |> List.filter
                                (\m ->
                                    if String.isEmpty query then
                                        True

                                    else
                                        String.contains query (String.toLower m.content)
                                            || (m.summary |> Maybe.map (\s -> String.contains query (String.toLower s)) |> Maybe.withDefault False)
                                            || List.any (\tag -> String.contains query (String.toLower tag)) m.tags
                                )
                            |> List.sortBy (\m -> ( negate m.importance, String.toLower (m.summary |> Maybe.withDefault m.content) ))
                            |> List.take 15
                in
                div [ class "popover-container" ]
                    [ div [ class "popover-overlay", onClick CancelLinkMemory ] []
                    , div [ class "popover-menu", stopPropagationOn "click" (Decode.succeed ( NoOp, True )) ]
                        [ input
                            [ class "popover-search"
                            , placeholder "Search memories..."
                            , value st.search
                            , onInput LinkMemorySearch
                            , autofocus True
                            ]
                            []
                        , div [ class "popover-results" ]
                            (if List.isEmpty availableMemories then
                                [ div [ class "popover-empty" ] [ text "No matching memories" ] ]

                             else
                                List.map
                                    (\mem ->
                                        div
                                            [ class "popover-card"
                                            , onClick (PerformLinkMemory entityType entityId mem.id)
                                            ]
                                            [ div [ class "popover-card-header" ]
                                                [ span [ class "entity-type-label entity-type-memory" ]
                                                    [ text
                                                        (if mem.memoryType == Api.ShortTerm then
                                                            "STM"

                                                         else
                                                            "LTM"
                                                        )
                                                    ]
                                                , span [ class "popover-card-title" ]
                                                    [ text
                                                        (case mem.summary of
                                                            Just s ->
                                                                s

                                                            Nothing ->
                                                                mem.content
                                                        )
                                                    ]
                                                ]
                                            , div [ class "popover-card-meta" ]
                                                [ span [ class "popover-card-status" ]
                                                    [ text (Api.memoryTypeToString mem.memoryType |> String.replace "_" " ") ]
                                                , span [ class "popover-card-priority" ] [ text ("★ " ++ String.fromInt mem.importance) ]
                                                , if mem.pinned then
                                                    span [ class "popover-card-pinned" ] [ text "📌" ]

                                                  else
                                                    text ""
                                                ]
                                            , if not (List.isEmpty mem.tags) then
                                                div [ class "popover-card-tags" ]
                                                    (List.map (\tag -> span [ class "popover-card-tag" ] [ text tag ]) (List.take 4 mem.tags))

                                              else
                                                text ""
                                            , div [ class "popover-card-desc" ] [ text mem.content ]
                                            ]
                                    )
                                    availableMemories
                            )
                        ]
                    ]

            else
                text ""

        Nothing ->
            text ""


viewLinkedMemoryItem : Model -> String -> String -> Api.Memory -> Html Msg
viewLinkedMemoryItem model entityType entityId memory =
    div [ class "linked-memory-item popover-card" ]
        [ div [ class "popover-card-header" ]
            [ span [ class "entity-type-label entity-type-memory" ]
                [ text
                    (if memory.memoryType == Api.ShortTerm then
                        "STM"

                     else
                        "LTM"
                    )
                ]
            , span [ class "popover-card-title" ]
                [ text
                    (case memory.summary of
                        Just s ->
                            s

                        Nothing ->
                            memory.content
                    )
                ]
            , div [ class "dep-item-actions" ]
                [ button
                    [ class "btn-icon btn-danger"
                    , onClick (PerformUnlinkMemory entityType entityId memory.id)
                    , title "Unlink"
                    ]
                    [ text "✕" ]
                ]
            ]
        , div [ class "popover-card-meta" ]
            [ span [ class "popover-card-status" ]
                [ text (Api.memoryTypeToString memory.memoryType |> String.replace "_" " ") ]
            , span [ class "popover-card-priority" ] [ text ("★ " ++ String.fromInt memory.importance) ]
            , if memory.pinned then
                span [ class "popover-card-pinned" ] [ text "📌" ]

              else
                text ""
            ]
        , if not (List.isEmpty memory.tags) then
            div [ class "popover-card-tags" ]
                (List.map (\t -> span [ class "popover-card-tag" ] [ text t ]) (List.take 4 memory.tags))

          else
            text ""
        , div [ class "popover-card-desc" ] [ text memory.content ]
        ]



-- SEARCH HELPERS


projectTreeMatchesSearch : String -> List Api.Project -> List Api.Task -> Api.Project -> Bool
projectTreeMatchesSearch query allProjects allTasks project =
    matchesSearch query project.name
        || (project.description |> Maybe.map (matchesSearch query) |> Maybe.withDefault False)
        || List.any (projectTreeMatchesSearch query allProjects allTasks)
            (List.filter (\p -> p.parentId == Just project.id) allProjects)
        || List.any (taskMatchesSearch query)
            (List.filter (\t -> t.projectId == Just project.id) allTasks)


taskTreeMatchesSearch : String -> List Api.Task -> Api.Task -> Bool
taskTreeMatchesSearch query allTasks task =
    taskMatchesSearch query task
        || List.any (taskTreeMatchesSearch query allTasks)
            (List.filter (\t -> t.parentId == Just task.id) allTasks)


taskMatchesSearch : String -> Api.Task -> Bool
taskMatchesSearch query task =
    matchesSearch query task.title
        || (task.description |> Maybe.map (matchesSearch query) |> Maybe.withDefault False)


matchesSearch : String -> String -> Bool
matchesSearch query text_ =
    String.contains query (String.toLower text_)



-- FILTER HELPERS


passesStatusFilter : List String -> String -> Bool
passesStatusFilter statuses status =
    List.isEmpty statuses || List.member status statuses


passesPriorityFilter : FilterPriority -> Int -> Bool
passesPriorityFilter filter priority =
    case filter of
        AnyPriority ->
            True

        ExactPriority v ->
            priority == v

        AbovePriority v ->
            priority >= v

        BelowPriority v ->
            priority <= v


projectTreePassesFilters : (Api.Project -> Bool) -> Api.Project -> List Api.Project -> List Api.Task -> (Api.Project -> Bool) -> (Api.Task -> Bool) -> Bool
projectTreePassesFilters projStatusGate project allProjects allTasks projFilter taskFilter =
    -- Project must pass the status gate to be visible at all
    projStatusGate project
        && (projFilter project
                || List.any (\c -> projectTreePassesFilters projStatusGate c allProjects allTasks projFilter taskFilter)
                    (List.filter (\p -> p.parentId == Just project.id) allProjects)
                || List.any (\t -> taskTreePassesFilters t allTasks taskFilter)
                    (List.filter (\t -> t.projectId == Just project.id) allTasks)
           )


taskTreePassesFilters : Api.Task -> List Api.Task -> (Api.Task -> Bool) -> Bool
taskTreePassesFilters task allTasks taskFilter =
    taskFilter task
        || List.any (\t -> taskTreePassesFilters t allTasks taskFilter)
            (List.filter (\t -> t.parentId == Just task.id) allTasks)



-- GRAPH PAGE


viewGraphPage : Model -> Html Msg
viewGraphPage model =
    div [ class "page" ]
        [ div [ class "page-header" ]
            [ h2 [] [ text "Knowledge Graph" ]
            , viewGraphWorkspaceSelector model
            ]
        , case model.graphVisualization of
            Nothing ->
                case model.selectedWorkspaceId of
                    Nothing ->
                        div [ class "empty-state" ] [ text "Select a workspace to view its knowledge graph." ]

                    Just _ ->
                        if model.graphLoaded then
                            div [ class "empty-state" ] [ text "No data found for this workspace." ]

                        else
                            div [ class "loading-indicator" ] [ text "Loading graph..." ]

            Just _ ->
                div [ id "cytoscape-container", style "width" "100%", style "height" "calc(100vh - 8rem)" ] []
        ]


viewGraphWorkspaceSelector : Model -> Html Msg
viewGraphWorkspaceSelector model =
    let
        wsList =
            Dict.values model.workspaces |> List.sortBy .name
    in
    if List.isEmpty wsList then
        text ""

    else
        select
            [ class "form-input"
            , style "width" "auto"
            , style "max-width" "250px"
            , onInput
                (\s ->
                    if String.isEmpty s then
                        NoOp

                    else
                        LoadGraphForWorkspace s
                )
            ]
            (option [ value "", selected (model.selectedWorkspaceId == Nothing) ] [ text "Select workspace..." ]
                :: List.map
                    (\ws ->
                        option [ value ws.id, selected (model.selectedWorkspaceId == Just ws.id) ]
                            [ text ws.name ]
                    )
                    wsList
            )



-- CREATE FORM MODAL


viewCreateFormModal : Model -> Html Msg
viewCreateFormModal model =
    case model.createForm of
        Nothing ->
            text ""

        Just form ->
            div [ class "modal-overlay", onClick CancelCreateForm ]
                [ div [ class "modal", stopPropagationOn "click" (Decode.succeed ( NoOp, True )) ]
                    [ viewCreateFormContent form ]
                ]


viewCreateFormContent : CreateForm -> Html Msg
viewCreateFormContent form =
    case form of
        CreateProjectForm f ->
            div []
                [ h3 [ class "modal-title" ] [ text "New Project" ]
                , div [ class "form-group" ]
                    [ label [ class "form-label" ] [ text "Name" ]
                    , input
                        [ class "form-input"
                        , value f.name
                        , onInput (\s -> UpdateCreateForm (CreateProjectForm { f | name = s }))
                        , placeholder "Project name"
                        , autofocus True
                        , onKeyDown
                            (\keyCode ->
                                if keyCode == 13 then
                                    SubmitCreateForm

                                else if keyCode == 27 then
                                    CancelCreateForm

                                else
                                    NoOp
                            )
                        ]
                        []
                    ]
                , div [ class "modal-actions" ]
                    [ button [ class "btn btn-secondary", onClick CancelCreateForm ] [ text "Cancel" ]
                    , button [ class "btn btn-primary", onClick SubmitCreateForm ] [ text "Create" ]
                    ]
                ]

        CreateMemoryForm f ->
            div []
                [ h3 [ class "modal-title" ] [ text "New Memory" ]
                , div [ class "form-group" ]
                    [ label [ class "form-label" ] [ text "Content" ]
                    , textarea
                        [ class "form-input form-textarea"
                        , value f.content
                        , onInput (\s -> UpdateCreateForm (CreateMemoryForm { f | content = s }))
                        , placeholder "Memory content..."
                        , rows 6
                        , autofocus True
                        ]
                        []
                    ]
                , div [ class "form-group" ]
                    [ label [ class "form-label" ] [ text "Type" ]
                    , select
                        [ class "form-input"
                        , onInput (\s -> UpdateCreateForm (CreateMemoryForm { f | memoryType = Api.memoryTypeFromString s }))
                        ]
                        (List.map
                            (\mt ->
                                let
                                    str =
                                        Api.memoryTypeToString mt
                                in
                                option [ value str, selected (mt == f.memoryType) ]
                                    [ text (str |> String.replace "_" " ") ]
                            )
                            Api.allMemoryTypes
                        )
                    ]
                , div [ class "modal-actions" ]
                    [ button [ class "btn btn-secondary", onClick CancelCreateForm ] [ text "Cancel" ]
                    , button [ class "btn btn-primary", onClick SubmitCreateForm ] [ text "Create" ]
                    ]
                ]

        CreateGroupForm f ->
            div []
                [ h3 [ class "modal-title" ] [ text "New Group" ]
                , div [ class "form-group" ]
                    [ label [ class "form-label" ] [ text "Name" ]
                    , input
                        [ class "form-input"
                        , value f.name
                        , onInput (\s -> UpdateCreateForm (CreateGroupForm { f | name = s }))
                        , placeholder "Group name"
                        , autofocus True
                        , onKeyDown
                            (\keyCode ->
                                if keyCode == 13 then
                                    SubmitCreateForm

                                else if keyCode == 27 then
                                    CancelCreateForm

                                else
                                    NoOp
                            )
                        ]
                        []
                    ]
                , div [ class "form-group" ]
                    [ label [ class "form-label" ] [ text "Description (optional)" ]
                    , input
                        [ class "form-input"
                        , value f.description
                        , onInput (\s -> UpdateCreateForm (CreateGroupForm { f | description = s }))
                        , placeholder "Brief description"
                        , onKeyDown
                            (\keyCode ->
                                if keyCode == 13 then
                                    SubmitCreateForm

                                else if keyCode == 27 then
                                    CancelCreateForm

                                else
                                    NoOp
                            )
                        ]
                        []
                    ]
                , div [ class "modal-actions" ]
                    [ button [ class "btn btn-secondary", onClick CancelCreateForm ] [ text "Cancel" ]
                    , button [ class "btn btn-primary", onClick SubmitCreateForm ] [ text "Create" ]
                    ]
                ]



-- DROP ACTION MODAL


viewDropActionModal : Model -> Html Msg
viewDropActionModal model =
    case model.dropActionModal of
        Nothing ->
            text ""

        Just modal ->
            let
                dragName =
                    Dict.get modal.dragTaskId model.tasks
                        |> Maybe.map .title
                        |> Maybe.withDefault "Task"

                targetName =
                    Dict.get modal.targetTaskId model.tasks
                        |> Maybe.map .title
                        |> Maybe.withDefault "Task"
            in
            div [ class "modal-overlay", onClick CancelDropAction ]
                [ div [ class "modal drop-action-modal", stopPropagationOn "click" (Decode.succeed ( NoOp, True )) ]
                    [ h3 [ class "modal-title" ] [ text "Move Task" ]
                    , p [ class "drop-action-desc" ]
                        [ text "What should "
                        , strong [] [ text (truncate 40 dragName) ]
                        , text " become relative to "
                        , strong [] [ text (truncate 40 targetName) ]
                        , text "?"
                        ]
                    , div [ class "drop-action-buttons" ]
                        [ button [ class "btn drop-action-btn", onClick DropActionMakeSubtask ]
                            [ span [ class "drop-action-icon" ] [ text "↳" ]
                            , span [] [ text "Subtask" ]
                            ]
                        , button [ class "btn drop-action-btn", onClick DropActionMakeDependency ]
                            [ span [ class "drop-action-icon" ] [ text "⟶" ]
                            , span [] [ text "Dependency" ]
                            ]
                        ]
                    , div [ class "modal-actions" ]
                        [ button [ class "btn btn-secondary", onClick CancelDropAction ] [ text "Cancel" ]
                        ]
                    ]
                ]



viewDeleteConfirmModal : Model -> Html Msg
viewDeleteConfirmModal model =
    case model.deleteConfirmation of
        Nothing ->
            text ""

        Just ( entityType, entityId ) ->
            let
                entityName =
                    case entityType of
                        "project" ->
                            Dict.get entityId model.projects |> Maybe.map .name |> Maybe.withDefault "this project"

                        "task" ->
                            Dict.get entityId model.tasks |> Maybe.map .title |> Maybe.withDefault "this task"

                        "memory" ->
                            Dict.get entityId model.memories
                                |> Maybe.map (\m -> Maybe.withDefault (truncate 50 m.content) m.summary)
                                |> Maybe.withDefault "this memory"

                        "group" ->
                            Dict.get entityId model.workspaceGroups |> Maybe.map .name |> Maybe.withDefault "this group"

                        _ ->
                            "this item"

                typeLabel =
                    case entityType of
                        "project" ->
                            "project"

                        "task" ->
                            Dict.get entityId model.tasks
                                |> Maybe.andThen (\t -> t.parentId)
                                |> Maybe.map (\_ -> "subtask")
                                |> Maybe.withDefault "task"

                        "memory" ->
                            "memory"

                        "group" ->
                            "group"

                        _ ->
                            "item"
            in
            div [ class "modal-overlay", onClick CancelDelete ]
                [ div [ class "modal delete-confirm-modal", stopPropagationOn "click" (Decode.succeed ( NoOp, True )) ]
                    [ h3 [ class "modal-title" ] [ text ("Delete " ++ typeLabel ++ "?") ]
                    , p [ class "delete-confirm-desc" ]
                        [ text "Are you sure you want to delete "
                        , strong [] [ text (truncate 60 entityName) ]
                        , text "? This action cannot be undone."
                        ]
                    , div [ class "modal-actions" ]
                        [ button [ class "btn btn-danger", onClick PerformDelete ] [ text "Delete" ]
                        , button [ class "btn btn-secondary", onClick CancelDelete ] [ text "Cancel" ]
                        ]
                    ]
                ]



-- TOASTS


viewToasts : List Toast -> Html Msg
viewToasts toasts =
    div [ class "toast-container" ]
        (List.map viewToast toasts)


viewToast : Toast -> Html Msg
viewToast toast =
    div
        [ class ("toast toast-" ++ toastLevelClass toast.level)
        , onClick (DismissToast toast.id)
        ]
        [ text toast.message ]


toastLevelClass : ToastLevel -> String
toastLevelClass level =
    case level of
        Info ->
            "info"

        Success ->
            "success"

        Warning ->
            "warning"

        Error ->
            "error"


viewConnectionStatus : WSState -> Html Msg
viewConnectionStatus state =
    let
        ( statusClass, statusText ) =
            case state of
                Connected ->
                    ( "connected", "Connected" )

                Disconnected ->
                    ( "disconnected", "Disconnected" )
    in
    div [ class ("connection-status " ++ statusClass) ]
        [ text statusText ]



-- UTILITIES


countLabel : Int -> Int -> String -> String -> Maybe String
countLabel remaining completed noun pluralNoun =
    let
        total =
            remaining + completed
    in
    if total == 0 then
        Nothing

    else if completed == 0 then
        Just (String.fromInt total ++ " " ++ (if total > 1 then pluralNoun else noun))

    else if remaining == 0 then
        Just (String.fromInt total ++ " " ++ (if total > 1 then pluralNoun else noun) ++ " (all done)")

    else
        Just (String.fromInt remaining ++ "/" ++ String.fromInt total ++ " " ++ pluralNoun ++ " remaining")


truncate : Int -> String -> String
truncate maxLen str =
    if String.length str > maxLen then
        String.left maxLen str ++ "…"

    else
        str


uniqueStrings : List String -> List String
uniqueStrings list =
    List.foldl
        (\item acc ->
            if List.member item acc then
                acc

            else
                acc ++ [ item ]
        )
        []
        list



-- MAIN


main : Program Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
