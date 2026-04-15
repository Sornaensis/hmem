module Main exposing (main)

import Api exposing (..)
import Browser
import Browser.Events
import Browser.Navigation as Nav
import Dict
import Feature.AuditLog
import Feature.DragDrop
import Feature.Editing
import Feature.Focus exposing (buildTaskBreadcrumb)
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
            , viewDeleteConfirmModal model
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
            viewProjectsTree wsId model

        MemoriesTab ->
            Feature.Memory.viewMemoriesList wsId model



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
            Feature.Editing.viewInlineCreateInput model Nothing "project"

        focusBreadcrumbBar =
            Feature.Focus.viewFocusBreadcrumbBar model

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
            [ class ("card tree-card card-project card-status-" ++ Api.projectStatusToString project.status ++ Feature.DragDrop.dragOverClass model project.id)
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
                    , Feature.Editing.viewEditableText model "project" project.id "name" project.name
                    ]
                , div [ class "card-actions" ]
                    [ Feature.Editing.viewStatusSelect "project" project.id (Api.projectStatusToString project.status) Api.allProjectStatuses Api.projectStatusToString ChangeProjectStatus
                    , Feature.Editing.viewPrioritySelect "project" project.id project.priority ChangeProjectPriority
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
                    , Feature.Editing.viewEditableTextarea model "project" project.id "description" (Maybe.withDefault "" project.description)
                    ]
                , if isExpanded model project.id then
                    div [ class "card-extras" ]
                        [ Feature.Memory.viewLinkedMemories model "project" project.id linkedMems
                        , Feature.AuditLog.viewEntityHistory model "project" project.id
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
            , Feature.Editing.viewInlineCreateInputForParent model (Just project.id) "project"
            , Feature.Editing.viewInlineCreateInputForParent model (Just project.id) "task"
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
        ([ class ("card tree-card " ++ cardClass ++ " card-status-" ++ Api.taskStatusToString task.status ++ Feature.DragDrop.dragOverClass model task.id)
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
                , Feature.Editing.viewEditableText model "task" task.id "title" task.title
                ]
            , div [ class "card-actions" ]
                [ Feature.Editing.viewStatusSelect "task" task.id (Api.taskStatusToString task.status) Api.allTaskStatuses Api.taskStatusToString ChangeTaskStatus
                , Feature.Editing.viewPrioritySelect "task" task.id task.priority ChangeTaskPriority
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
                , Feature.Editing.viewEditableTextarea model "task" task.id "description" (Maybe.withDefault "" task.description)
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
                    , Feature.Memory.viewLinkedMemories model "task" task.id linkedMems
                    , Feature.AuditLog.viewEntityHistory model "task" task.id
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
        , Feature.Editing.viewInlineCreateInputForParent model (Just task.id) "subtask"
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
                                |> Maybe.map (\m -> Maybe.withDefault (truncateText 50 m.content) m.summary)
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
                        , strong [] [ text (truncateText 60 entityName) ]
                        , text "? This action cannot be undone."
                        ]
                    , div [ class "modal-actions" ]
                        [ button [ class "btn btn-danger", onClick PerformDelete ] [ text "Delete" ]
                        , button [ class "btn btn-secondary", onClick CancelDelete ] [ text "Cancel" ]
                        ]
                    ]
                ]



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
