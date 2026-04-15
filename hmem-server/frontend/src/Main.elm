module Main exposing (main)

import Api exposing (..)
import Browser
import Browser.Events
import Browser.Navigation as Nav
import Dict
import Feature.AuditLog
import Feature.Focus exposing (buildProjectBreadcrumb, buildTaskBreadcrumb)
import Feature.Graph
import Feature.Groups
import Feature.Search
import Helpers exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Json.Decode as Decode
import Json.Encode as Encode
import Markdown.Parser
import Markdown.Renderer
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

        LoadGraphForWorkspace _ ->
            Feature.Graph.update msg model

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
            , viewCreateFormModal model
            , viewDropActionModal model
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
                , Feature.Search.viewSearchBar model
                , viewTabs model.activeTab
                , if model.loadingWorkspaceData then
                    div [ class "loading-indicator" ] [ text "Loading..." ]

                  else
                    case model.unifiedSearchResults of
                        Just results ->
                            Feature.Search.viewUnifiedSearchResults (viewMemoryCard model) model results

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
            viewMemoriesList wsId model



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
                    , Feature.AuditLog.viewEntityHistory model "memory" memory.id
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
                        , strong [] [ text (truncateText 40 dragName) ]
                        , text " become relative to "
                        , strong [] [ text (truncateText 40 targetName) ]
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
