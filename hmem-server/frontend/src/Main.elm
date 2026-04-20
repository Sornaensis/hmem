module Main exposing (main)

import Api exposing (..)
import Browser
import Browser.Events
import Browser.Navigation as Nav
import Dict
import Feature.AuditLog
import Feature.Cards
import Feature.DataLoading
import Feature.Dependencies
import Feature.DragDrop
import Feature.Editing
import Feature.Focus
import Feature.Graph
import Feature.Groups
import Feature.Memory
import Feature.Mutations
import Feature.Search
import Feature.WebSocket
import Helpers exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Keyed as Keyed
import Json.Decode as Decode
import Page.Home
import Page.Workspace
import Ports exposing (..)
import Route exposing (handleUrlChange, handleUrlRequest, loadWorkspaceData, urlToPage)
import Toast
import Types exposing (..)
import Url



-- FLAGS


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
            , sessionId =
                Decode.decodeValue (Decode.field "sessionId" Decode.string) raw
                    |> Result.withDefault "session"
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
            , selectedWorkspaceId = Nothing
            , activeTab = frag.tab
            , mainContentScrollY = 0
            , workspaces = Dict.empty
            , projects = Dict.empty
            , tasks = Dict.empty
            , memories = Dict.empty
            , toast =
                { toasts = []
                , nextToastId = 0
                }
            , webSocket =
                { state = Disconnected
                }
            , dataLoading =
                { loadingWorkspaces = True
                , loadingWorkspaceData = False
                , pendingWorkspaceLoads = 0
                , activeWorkspaceLoadToken = Nothing
                , nextWorkspaceLoadToken = 1
                }
            , search =
                { query = ""
                , unifiedResults = Nothing
                , isSearching = False
                , filterShowOnly = ShowAll
                , filterPriority = AnyPriority
                , filterProjectStatuses = []
                , filterTaskStatuses = []
                , filterMemoryTypes = []
                , filterImportance = AnyPriority
                , filterMemoryPinned = Nothing
                , filterTags = []
                }
            , editing =
                { editState = Nothing
                , createForm = Nothing
                , inlineCreate = Nothing
                }
            , memory =
                { entityMemories = Dict.empty
                , linkingMemoryFor = Nothing
                , linkingEntityFor = Nothing
                }
            , dependencies =
                { taskDependencies = Dict.empty
                , addingDependencyFor = Nothing
                }
            , cards =
                { expandedCards = Dict.empty
                , collapsedNodes = Dict.empty
                , deleteConfirmation = Nothing
                }
            , graph =
                { visualization = Nothing
                , loaded = False
                }
            , dragDrop =
                { dragging = Nothing
                , dragOver = Nothing
                , dropActionModal = Nothing
                }
            , focus =
                { focusedEntity = frag.focus
                , breadcrumbAnchor = frag.focus
                , history =
                    case frag.focus of
                        Just f ->
                            [ f ]

                        Nothing ->
                            []
                , historyIndex = 0
                }
            , mutations =
                { pendingMutationIds = Dict.empty
                , pendingRequestIds = Dict.empty
                , nextRequestId = 1
                }
            , groups =
                { workspaceGroups = Dict.empty
                , groupMembers = Dict.empty
                , managingGroup = Nothing
                }
            , auditLog =
                { entityHistory = Dict.empty
                , entityHistoryHasMore = Dict.empty
                , historyExpanded = Dict.empty
                , entries = []
                , hasMore = False
                , filters = { entityType = Nothing, entityId = Nothing, action = Nothing, since = Nothing, until = Nothing, limit = Just 50, offset = Nothing }
                , expandedEntries = Dict.empty
                , revertConfirmation = Nothing
                , revertInFlight = False
                }
            }

        model =
            case decoded.storedFilters of
                Just json ->
                    applyStoredFiltersIfCurrentWorkspace json baseModel

                Nothing ->
                    baseModel

        cmds =
            [ Feature.WebSocket.connectCmd flags.wsUrl
            , Api.fetchWorkspaces flags.apiUrl GotWorkspaces
            , Api.fetchWorkspaceGroups flags.apiUrl GotWorkspaceGroups
            ]

        pageCmd =
            case page of
                WorkspacePage wsId ->
                    loadWorkspaceData flags.apiUrl wsId (Just model.dataLoading.nextWorkspaceLoadToken)

                _ ->
                    Cmd.none

        currentDataLoading =
            model.dataLoading

        updatedDataLoading =
            { currentDataLoading
                | loadingWorkspaceData =
                    case page of
                        WorkspacePage _ ->
                            True

                        _ ->
                            False
                , pendingWorkspaceLoads =
                    case page of
                        WorkspacePage _ ->
                            3

                        _ ->
                            0
                , activeWorkspaceLoadToken =
                    case page of
                        WorkspacePage _ ->
                            Just model.dataLoading.nextWorkspaceLoadToken

                        _ ->
                            Nothing
                , nextWorkspaceLoadToken =
                    case page of
                        WorkspacePage _ ->
                            model.dataLoading.nextWorkspaceLoadToken + 1

                        _ ->
                            model.dataLoading.nextWorkspaceLoadToken
            }
    in
    ( { model
        | selectedWorkspaceId =
            case page of
                WorkspacePage wsId ->
                    Just wsId

                _ ->
                    Nothing
        , dataLoading = updatedDataLoading
      }
    , Cmd.batch (cmds ++ [ pageCmd ])
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested urlRequest ->
            handleUrlRequest urlRequest model

        UrlChanged url ->
            handleUrlChange url model

        -- WebSocket
        WsConnectedMsg ->
            Feature.WebSocket.update msg model

        WsDisconnectedMsg ->
            Feature.WebSocket.update msg model

        WsMessageReceived _ ->
            Feature.WebSocket.update msg model

        -- Cytoscape
        CytoscapeNodeClicked _ ->
            Feature.Graph.update msg model

        CytoscapeEdgeClicked _ ->
            Feature.Graph.update msg model

        -- HTTP responses
        GotWorkspaces _ ->
            Feature.DataLoading.update msg model

        GotProjects _ _ _ ->
            Feature.DataLoading.update msg model

        GotTasks _ _ _ ->
            Feature.DataLoading.update msg model

        GotMemories _ _ _ ->
            Feature.DataLoading.update msg model

        GotSingleMemory _ ->
            Feature.DataLoading.update msg model

        GotVisualization _ ->
            Feature.Graph.update msg model

        -- Mutation responses
        MutationDone _ _ ->
            Feature.Mutations.update msg model

        ProjectCreated _ ->
            Feature.Mutations.update msg model

        TaskCreated _ ->
            Feature.Mutations.update msg model

        MemoryCreated _ ->
            Feature.Mutations.update msg model

        ProjectUpdated _ ->
            Feature.Mutations.update msg model

        TaskUpdated _ ->
            Feature.Mutations.update msg model

        MemoryUpdated _ ->
            Feature.Mutations.update msg model

        WorkspaceUpdated _ ->
            Feature.Mutations.update msg model

        -- UI
        SelectWorkspace wsId ->
            ( model, Nav.pushUrl model.key ("/workspace/" ++ wsId) )

        SwitchTab tab ->
            let
                currentEditing =
                    model.editing

                updatedEditing =
                    { currentEditing
                        | createForm = Nothing
                        , editState = Nothing
                        , inlineCreate = Nothing
                    }

                currentMemory =
                    model.memory

                updatedMemory =
                    { currentMemory
                        | linkingMemoryFor = Nothing
                        , linkingEntityFor = Nothing
                    }

                newModel =
                    { model
                        | activeTab = tab
                        , editing = updatedEditing
                        , memory = updatedMemory
                    }
            in
            ( newModel, replaceFragment newModel )

        DismissToast _ ->
            Toast.update msg model

        AutoDismissToast _ ->
            Toast.update msg model

        SearchInput _ ->
            Feature.Search.update msg model

        SubmitSearch ->
            Feature.Search.update msg model

        GotUnifiedSearchResults _ ->
            Feature.Search.update msg model

        SetFilterShowOnly _ ->
            Feature.Search.update msg model

        SetFilterPriority _ ->
            Feature.Search.update msg model

        ToggleFilterProjectStatus _ ->
            Feature.Search.update msg model

        ToggleFilterTaskStatus _ ->
            Feature.Search.update msg model

        ToggleFilterMemoryType _ ->
            Feature.Search.update msg model

        SetFilterImportance _ ->
            Feature.Search.update msg model

        SetFilterMemoryPinned _ ->
            Feature.Search.update msg model

        ToggleFilterTag _ ->
            Feature.Search.update msg model

        -- Inline editing
        StartEdit _ _ _ _ ->
            Feature.Editing.update msg model

        EditInput _ ->
            Feature.Editing.update msg model

        SaveEdit _ _ ->
            Feature.Editing.update msg model

        CancelEdit ->
            Feature.Editing.update msg model

        -- Quick-change handlers
        ChangeProjectStatus _ _ ->
            Feature.Editing.update msg model

        ChangeTaskStatus _ _ ->
            Feature.Editing.update msg model

        ChangeProjectPriority _ _ ->
            Feature.Editing.update msg model

        ChangeTaskPriority _ _ ->
            Feature.Editing.update msg model

        ChangeMemoryImportance _ _ ->
            Feature.Editing.update msg model

        ToggleMemoryPin _ _ ->
            Feature.Editing.update msg model

        ChangeMemoryType _ _ ->
            Feature.Editing.update msg model

        -- Tags
        RemoveTag _ _ ->
            Feature.Editing.update msg model

        AddTag _ _ ->
            Feature.Editing.update msg model

        -- Create forms
        ShowCreateForm _ ->
            Feature.Editing.update msg model

        UpdateCreateForm _ ->
            Feature.Editing.update msg model

        SubmitCreateForm ->
            Feature.Editing.update msg model

        CancelCreateForm ->
            Feature.Editing.update msg model

        -- Card expand/collapse
        ToggleCardExpand _ ->
            Feature.Cards.update msg model

        -- Tree collapse
        ToggleTreeNode nodeId ->
            let
                current =
                    Dict.get nodeId model.cards.collapsedNodes |> Maybe.withDefault False

                currentCards =
                    model.cards

                updatedCards =
                    { currentCards | collapsedNodes = Dict.insert nodeId (not current) model.cards.collapsedNodes }

                newModel =
                    { model | cards = updatedCards }
            in
            ( newModel, saveFiltersCmd newModel )

        ExpandAllNodes ->
            let
                currentCards =
                    model.cards

                updatedCards =
                    { currentCards | collapsedNodes = Dict.empty }

                newModel =
                    { model | cards = updatedCards }
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

                currentCards =
                    model.cards

                updatedCards =
                    { currentCards | collapsedNodes = Dict.fromList (projectNodes ++ taskNodes) }

                newModel =
                    { model | cards = updatedCards }
            in
            ( newModel, saveFiltersCmd newModel )

        -- Delete
        ConfirmDelete _ _ ->
            Feature.Cards.update msg model

        PerformDelete ->
            Feature.Cards.update msg model

        CancelDelete ->
            Feature.Cards.update msg model

        CopyId _ ->
            Feature.Cards.update msg model

        LocalStorageLoaded json ->
            ( applyStoredFiltersIfCurrentWorkspace json model, Cmd.none )

        LoadGraphForWorkspace _ ->
            Feature.Graph.update msg model

        ExpandAndEdit _ _ _ _ _ ->
            Feature.Editing.update msg model

        DragStartCard _ _ ->
            Feature.DragDrop.update msg model

        DragOverCard _ ->
            Feature.DragDrop.update msg model

        DragOverZone _ ->
            Feature.DragDrop.update msg model

        DropOnCard _ _ ->
            Feature.DragDrop.update msg model

        DragEndCard ->
            Feature.DragDrop.update msg model

        DropOnZone _ ->
            Feature.DragDrop.update msg model

        DropActionMakeSubtask ->
            Feature.DragDrop.update msg model

        DropActionMakeDependency ->
            Feature.DragDrop.update msg model

        CancelDropAction ->
            Feature.DragDrop.update msg model

        -- Inline create
        ShowInlineCreate _ ->
            Feature.Editing.update msg model

        UpdateInlineCreate _ ->
            Feature.Editing.update msg model

        SubmitInlineCreate ->
            Feature.Editing.update msg model

        CancelInlineCreate ->
            Feature.Editing.update msg model

        -- Memory linking
        StartLinkMemory _ _ ->
            Feature.Memory.update msg model

        LinkMemorySearch _ ->
            Feature.Memory.update msg model

        CancelLinkMemory ->
            Feature.Memory.update msg model

        PerformLinkMemory _ _ _ ->
            Feature.Memory.update msg model

        PerformUnlinkMemory _ _ _ ->
            Feature.Memory.update msg model

        MemoryLinkDone _ _ ->
            Feature.Memory.update msg model

        GotEntityMemories _ _ ->
            Feature.Memory.update msg model

        -- Entity linking from memory cards
        StartLinkEntity _ ->
            Feature.Memory.update msg model

        LinkEntitySearch _ ->
            Feature.Memory.update msg model

        CancelLinkEntity ->
            Feature.Memory.update msg model

        PerformLinkEntity _ _ _ ->
            Feature.Memory.update msg model

        PerformUnlinkEntity _ _ _ ->
            Feature.Memory.update msg model

        -- Task dependencies
        GotTaskDependencies _ _ ->
            Feature.Dependencies.update msg model

        StartAddDependency _ ->
            Feature.Dependencies.update msg model

        DependencySearch _ ->
            Feature.Dependencies.update msg model

        CancelAddDependency ->
            Feature.Dependencies.update msg model

        PerformAddDependency _ _ ->
            Feature.Dependencies.update msg model

        PerformRemoveDependency _ _ ->
            Feature.Dependencies.update msg model

        DependencyMutationDone _ _ ->
            Feature.Dependencies.update msg model

        ScrollToEntity entityId ->
            let
                currentCards =
                    model.cards

                updatedCards =
                    { currentCards | expandedCards = Dict.insert entityId True model.cards.expandedCards }
            in
            ( { model | cards = updatedCards }
            , scrollToElement ("entity-" ++ entityId)
            )

        FocusEntity _ _ ->
            Feature.Focus.update msg model

        FocusEntityKeepForward _ _ ->
            Feature.Focus.update msg model

        NavigateToAuditEntity _ ->
            Feature.AuditLog.update msg model

        FocusBreadcrumbNav _ ->
            Feature.Focus.update msg model

        ClearFocus ->
            Feature.Focus.update msg model

        GlobalKeyDown keyCode ->
            if keyCode == 27 then
                -- Escape: dismiss any active popup/edit, inner-most first
                if model.cards.deleteConfirmation /= Nothing then
                    let
                        currentCards =
                            model.cards

                        updatedCards =
                            { currentCards | deleteConfirmation = Nothing }
                    in
                    ( { model | cards = updatedCards }, Cmd.none )

                else if model.dragDrop.dropActionModal /= Nothing then
                    let
                        currentDragDrop =
                            model.dragDrop

                        updatedDragDrop =
                            { currentDragDrop | dropActionModal = Nothing }
                    in
                    ( { model | dragDrop = updatedDragDrop }, Cmd.none )

                else if model.dependencies.addingDependencyFor /= Nothing then
                    let
                        currentDependencies =
                            model.dependencies

                        updatedDependencies =
                            { currentDependencies | addingDependencyFor = Nothing }
                    in
                    ( { model | dependencies = updatedDependencies }, Cmd.none )

                else if model.memory.linkingMemoryFor /= Nothing then
                    let
                        currentMemory =
                            model.memory

                        updatedMemory =
                            { currentMemory | linkingMemoryFor = Nothing }
                    in
                    ( { model | memory = updatedMemory }, Cmd.none )

                else if model.memory.linkingEntityFor /= Nothing then
                    let
                        currentMemory =
                            model.memory

                        updatedMemory =
                            { currentMemory | linkingEntityFor = Nothing }
                    in
                    ( { model | memory = updatedMemory }, Cmd.none )

                else if model.editing.inlineCreate /= Nothing then
                    let
                        currentEditing =
                            model.editing

                        updatedEditing =
                            { currentEditing | inlineCreate = Nothing }
                    in
                    ( { model | editing = updatedEditing }, Cmd.none )

                else if model.editing.editState /= Nothing then
                    let
                        currentEditing =
                            model.editing

                        updatedEditing =
                            { currentEditing | editState = Nothing }
                    in
                    ( { model | editing = updatedEditing }, Cmd.none )

                else
                    ( model, Cmd.none )

            else
                ( model, Cmd.none )

        GotAuditLog _ ->
            Feature.AuditLog.update msg model

        GotEntityHistory _ _ ->
            Feature.AuditLog.update msg model

        ToggleEntityHistory _ _ ->
            Feature.AuditLog.update msg model

        LoadMoreHistory _ _ ->
            Feature.AuditLog.update msg model

        SetAuditFilter _ _ ->
            Feature.AuditLog.update msg model

        ApplyAuditFilters ->
            Feature.AuditLog.update msg model

        LoadMoreAuditLog ->
            Feature.AuditLog.update msg model

        ToggleAuditExpand _ ->
            Feature.AuditLog.update msg model

        ConfirmRevert _ ->
            Feature.AuditLog.update msg model

        CancelRevert ->
            Feature.AuditLog.update msg model

        PerformRevert ->
            Feature.AuditLog.update msg model

        GotRevertResult _ _ _ ->
            Feature.AuditLog.update msg model

        NoOp ->
            ( model, Cmd.none )

        MainContentScrolled scrollY ->
            ( { model | mainContentScrollY = scrollY }, Cmd.none )

        ClearPendingMutation entityId ->
            let
                currentMutations =
                    model.mutations

                updatedMutations =
                    { currentMutations | pendingMutationIds = Dict.remove entityId model.mutations.pendingMutationIds }
            in
            ( { model | mutations = updatedMutations }
            , Cmd.none
            )

        ClearPendingRequest requestId ->
            let
                currentMutations =
                    model.mutations

                updatedMutations =
                    { currentMutations | pendingRequestIds = Dict.remove requestId model.mutations.pendingRequestIds }
            in
            ( { model | mutations = updatedMutations }, Cmd.none )

        -- Workspace groups
        GotWorkspaceGroups _ ->
            Feature.Groups.update msg model

        GotGroupMembers _ _ ->
            Feature.Groups.update msg model

        CreateWorkspaceGroup _ ->
            Feature.Groups.update msg model

        WorkspaceGroupCreated _ ->
            Feature.Groups.update msg model

        DeleteWorkspaceGroup _ ->
            Feature.Groups.update msg model

        WorkspaceGroupDeleted _ _ ->
            Feature.Groups.update msg model

        ToggleManageGroup _ ->
            Feature.Groups.update msg model

        AddWorkspaceToGroup _ _ ->
            Feature.Groups.update msg model

        RemoveWorkspaceFromGroup _ _ ->
            Feature.Groups.update msg model

        GroupMembershipDone _ _ ->
            Feature.Groups.update msg model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Feature.WebSocket.subscriptions
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
            [ Feature.Groups.viewSidebar model
            , Keyed.node "div" [ class "main-content", id "main-content-scroll" ]
                [ ( pageKey model.page, viewPage model ) ]
            , Toast.view model.toast
            , viewConnectionStatus model.webSocket.state
            , Feature.Editing.viewCreateFormModal model
            , Feature.DragDrop.viewDropActionModal model
            , Feature.Cards.viewDeleteConfirmModal model
            , Feature.AuditLog.viewRevertConfirmModal model
            ]
        ]
    }


pageKey : Page -> String
pageKey page =
    case page of
        HomePage ->
            "home"

        WorkspacePage wsId ->
            "workspace-" ++ wsId

        MemoryGraphPage ->
            "graph"

        AuditLogPage ->
            "audit"

        NotFound ->
            "notfound"


viewPage : Model -> Html Msg
viewPage model =
    case model.page of
        HomePage ->
            Page.Home.viewHomePage model

        WorkspacePage wsId ->
            Page.Workspace.viewWorkspacePage wsId model

        MemoryGraphPage ->
            Feature.Graph.viewGraphPage model

        AuditLogPage ->
            Feature.AuditLog.viewAuditLogPage model

        NotFound ->
            div [ class "page" ]
                [ h2 [] [ text "Not Found" ] ]



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
