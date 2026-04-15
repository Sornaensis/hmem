module Main exposing (main)

import Api exposing (..)
import Browser
import Browser.Events
import Browser.Navigation as Nav
import Dict
import Feature.AuditLog
import Feature.Cards
import Feature.Dependencies
import Feature.DragDrop
import Feature.Editing
import Feature.Focus
import Feature.Graph
import Feature.Groups
import Feature.Memory
import Feature.Search
import Helpers exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Json.Decode as Decode
import Ports exposing (..)
import Route exposing (handleUrlChange, handleUrlRequest, loadWorkspaceData, urlToPage)
import Toast exposing (addToast)
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
            , entityHistory = Dict.empty
            , entityHistoryHasMore = Dict.empty
            , historyExpanded = Dict.empty
            , auditLog = []
            , auditLogHasMore = False
            , auditLogFilters = { entityType = Nothing, entityId = Nothing, action = Nothing, since = Nothing, until = Nothing, limit = Just 50, offset = Nothing }
            , auditLogExpanded = Dict.empty
            , revertConfirmation = Nothing
            , revertInFlight = False
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
            Feature.Graph.update msg model

        CytoscapeEdgeClicked _ ->
            Feature.Graph.update msg model

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

        GotVisualization _ ->
            Feature.Graph.update msg model

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
        ConfirmDelete _ _ ->
            Feature.Cards.update msg model

        PerformDelete ->
            Feature.Cards.update msg model

        CancelDelete ->
            Feature.Cards.update msg model

        CopyId _ ->
            Feature.Cards.update msg model

        LocalStorageLoaded json ->
            ( applyStoredFilters json model, Cmd.none )

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
            ( { model
                | expandedCards = Dict.insert entityId True model.expandedCards
              }
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
            ( { model | pendingMutationIds = Dict.remove entityId model.pendingMutationIds }
            , Cmd.none
            )

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
            [ Feature.Groups.viewSidebar model
            , Keyed.node "div" [ class "main-content", id "main-content-scroll" ]
                [ ( pageKey model.page, viewPage model ) ]
            , Toast.view model.toasts
            , viewConnectionStatus model.wsState
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
            Feature.Groups.viewHomePage model

        WorkspacePage wsId ->
            viewWorkspacePage wsId model

        MemoryGraphPage ->
            Feature.Graph.viewGraphPage model

        AuditLogPage ->
            Feature.AuditLog.viewAuditLogPage model

        NotFound ->
            div [ class "page" ]
                [ h2 [] [ text "Not Found" ] ]



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
                            , Feature.Editing.viewEditableText model "workspace" ws.id "name" ws.name
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
                , Feature.Search.viewSearchBar model
                , viewTabs model.activeTab
                , if model.loadingWorkspaceData then
                    div [ class "loading-indicator" ] [ text "Loading..." ]

                  else
                    case model.unifiedSearchResults of
                        Just results ->
                            Feature.Search.viewUnifiedSearchResults (Feature.Memory.viewMemoryCard model) model results

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
        , Feature.Search.viewSearchBar model
        ]


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
            Feature.Cards.viewProjectsTree wsId model

        MemoriesTab ->
            Feature.Memory.viewMemoriesList wsId model



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
