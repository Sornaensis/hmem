module Route exposing (..)

import Api
import Browser
import Browser.Navigation as Nav
import Dict
import Helpers exposing (localStorageKey, parseFragment)
import Permissions
import Ports exposing (destroyCytoscape, disconnectWebSocket, requestLocalStorage)
import Types exposing (..)
import Url
import Url.Parser as Parser exposing ((</>), Parser)



-- ROUTING


loadWorkspaceData : String -> String -> Maybe Int -> Cmd Msg
loadWorkspaceData apiUrl wsId maybeLoadToken =
    Cmd.batch
        [ Api.fetchProjects apiUrl wsId (GotProjects wsId maybeLoadToken 0)
        , Api.fetchTasks apiUrl wsId (GotTasks wsId maybeLoadToken 0)
        , Api.fetchMemories apiUrl wsId (GotMemories wsId maybeLoadToken 0)
        ]


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map HomeRoute Parser.top
        , Parser.map WorkspaceRoute (Parser.s "workspace" </> Parser.string)
        , Parser.map MemoryGraphRoute (Parser.s "memory-graph")
        , Parser.map AuditLogRoute (Parser.s "audit")
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

        Just AuditLogRoute ->
            AuditLogPage

        Nothing ->
            NotFound



-- URL HANDLERS


handleUrlRequest : Browser.UrlRequest -> Model -> ( Model, Cmd Msg )
handleUrlRequest urlRequest model =
    case urlRequest of
        Browser.Internal url ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
            ( model, Nav.load href )


handleUrlChange : Url.Url -> Model -> ( Model, Cmd Msg )
handleUrlChange url model =
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
                        frag.focus /= model.focus.focusedEntity

                    currentFocus =
                        model.focus

                    updatedFocus =
                        { currentFocus
                            | focusedEntity = frag.focus
                            , breadcrumbAnchor =
                                if focusChangedExternally then
                                    frag.focus

                                else
                                    model.focus.breadcrumbAnchor
                            , history =
                                if focusChangedExternally then
                                    case frag.focus of
                                        Just f ->
                                            [ f ]

                                        Nothing ->
                                            []

                                else
                                    model.focus.history
                            , historyIndex =
                                if focusChangedExternally then
                                    0

                                else
                                    model.focus.historyIndex
                        }

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
                    updatedModel =
                        { model
                            | url = url
                            , activeTab = frag.tab
                            , focus = updatedFocus
                            , editing = updatedEditing
                            , memory = updatedMemory
                        }
                            |> clearRouteConfirmations

                    ( finalModel, auditCmd ) =
                        prepareWorkspaceAuditFromRoute wsId frag.tab updatedModel
                in
                ( finalModel, auditCmd )

            else
                let
                    frag =
                        parseFragment url.fragment

                    destroyCmd =
                        if model.page == MemoryGraphPage then
                            destroyCytoscape ()

                        else
                            Cmd.none

                    currentDataLoading =
                        model.dataLoading

                    updatedDataLoading =
                        { currentDataLoading
                            | loadingWorkspaceData = True
                            , pendingWorkspaceLoads = 3
                            , activeWorkspaceLoadToken = Just currentDataLoading.nextWorkspaceLoadToken
                            , nextWorkspaceLoadToken = currentDataLoading.nextWorkspaceLoadToken + 1
                        }

                    currentFocus =
                        model.focus

                    updatedFocus =
                        { currentFocus
                            | focusedEntity = frag.focus
                            , breadcrumbAnchor = frag.focus
                            , history =
                                case frag.focus of
                                    Just f ->
                                        [ f ]

                                    Nothing ->
                                        []
                            , historyIndex = 0
                        }

                    currentEditing =
                        model.editing

                    updatedEditing =
                        { currentEditing
                            | editState = Nothing
                            , createForm = Nothing
                            , inlineCreate = Nothing
                        }

                    currentMemory =
                        model.memory

                    updatedMemory =
                        { currentMemory
                            | linkingMemoryFor = Nothing
                            , linkingEntityFor = Nothing
                            , entityMemories = Dict.empty
                        }

                    currentDependencies =
                        model.dependencies

                    updatedDependencies =
                        { currentDependencies
                            | taskDependencies = Dict.empty
                            , addingDependencyFor = Nothing
                        }

                    currentSearch =
                        model.search

                    updatedSearch =
                        { currentSearch
                            | query = ""
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

                    currentCards =
                        model.cards

                    updatedCards =
                        { currentCards
                            | collapsedNodes = Dict.empty
                            , expandedCards = Dict.empty
                        }

                    currentGraph =
                        model.graph

                    updatedGraph =
                        { currentGraph
                            | loaded = False
                            , visualization = Nothing
                        }

                    currentAuditLog =
                        model.auditLog

                    updatedAuditLog =
                        { currentAuditLog
                            | entityHistory = Dict.empty
                            , entityHistoryHasMore = Dict.empty
                            , historyExpanded = Dict.empty
                        }
                in
                ( { model
                    | url = url
                    , page = page
                    , auth = { status = AuthBooting, mode = model.auth.mode }
                    , sessionContext = Nothing
                    , webSocket = { state = Disconnected }
                    , selectedWorkspaceId = Just wsId
                    , activeTab = frag.tab
                    , projects = Dict.empty
                    , tasks = Dict.empty
                    , memories = Dict.empty
                    , mainContentScrollY = 0
                    , dataLoading = updatedDataLoading
                    , focus = updatedFocus
                    , editing = updatedEditing
                    , memory = updatedMemory
                    , dependencies = updatedDependencies
                    , search = updatedSearch
                    , cards = updatedCards
                    , graph = updatedGraph
                    , auditLog = updatedAuditLog
                  }
                    |> clearRouteConfirmations
                , Cmd.batch
                    [ Api.fetchSessionContext model.flags.apiUrl (Just wsId) (GotSessionContext (Just wsId))
                    , requestLocalStorage (localStorageKey wsId)
                    , disconnectWebSocket ()
                    , destroyCmd
                    ]
                )

        MemoryGraphPage ->
            let
                currentGraph =
                    model.graph

                updatedGraph =
                    { currentGraph | loaded = False, visualization = Nothing }
            in
            ( { model
                | url = url
                , page = page
                , auth = { status = AuthBooting, mode = model.auth.mode }
                , sessionContext = Nothing
                , webSocket = { state = Disconnected }
                , graph = updatedGraph
              }
                |> clearRouteConfirmations
            , Cmd.batch
                [ Api.fetchSessionContext model.flags.apiUrl model.selectedWorkspaceId (GotSessionContext model.selectedWorkspaceId)
                , disconnectWebSocket ()
                ]
            )

        AuditLogPage ->
            let
                destroyCmd =
                    if model.page == MemoryGraphPage then
                        destroyCytoscape ()

                    else
                        Cmd.none

                emptyFilters =
                    { workspaceId = Nothing, entityType = Nothing, entityId = Nothing, action = Nothing, since = Nothing, until = Nothing, limit = Just 50, offset = Nothing }

                currentAuditLog =
                    model.auditLog

                updatedAuditLog =
                    { currentAuditLog
                        | entries = []
                        , hasMore = False
                        , loading = False
                        , loadingFilters = Nothing
                        , filters = emptyFilters
                        , expandedEntries = Dict.empty
                        , revertConfirmation = Nothing
                        , revertInFlight = False
                    }
            in
            ( { model
                | url = url
                , page = page
                , auth = { status = AuthBooting, mode = model.auth.mode }
                , sessionContext = Nothing
                , selectedWorkspaceId = Nothing
                , webSocket = { state = Disconnected }
                , auditLog = updatedAuditLog
              }
                |> clearRouteConfirmations
            , Cmd.batch
                [ Api.fetchSessionContext model.flags.apiUrl Nothing (GotSessionContext Nothing)
                , disconnectWebSocket ()
                , destroyCmd
                ]
            )

        _ ->
            let
                destroyCmd =
                    if model.page == MemoryGraphPage then
                        destroyCytoscape ()

                    else
                        Cmd.none
            in
            ( { model | url = url, page = page, auth = { status = AuthBooting, mode = model.auth.mode }, sessionContext = Nothing, selectedWorkspaceId = Nothing, webSocket = { state = Disconnected } }
                |> clearRouteConfirmations
            , Cmd.batch
                [ Api.fetchSessionContext model.flags.apiUrl Nothing (GotSessionContext Nothing)
                , disconnectWebSocket ()
                , destroyCmd
                ]
            )


prepareWorkspaceAuditFromRoute : String -> WorkspaceTab -> Model -> ( Model, Cmd Msg )
prepareWorkspaceAuditFromRoute wsId tab model =
    if tab == AuditTab && Permissions.canViewCurrentWorkspaceAudit model && model.auditLog.filters.workspaceId /= Just wsId then
        let
            filters =
                workspaceAuditFilters wsId

            auditLog =
                resetAuditLogWithFilters filters model.auditLog
        in
        ( { model | auditLog = auditLog }
        , Api.fetchAuditLog model.flags.apiUrl filters (GotAuditLog filters)
        )

    else
        ( model, Cmd.none )


workspaceAuditFilters : String -> AuditLogFilters
workspaceAuditFilters wsId =
    { workspaceId = Just wsId
    , entityType = Nothing
    , entityId = Nothing
    , action = Nothing
    , since = Nothing
    , until = Nothing
    , limit = Just 50
    , offset = Nothing
    }


resetAuditLogWithFilters : AuditLogFilters -> AuditLogModel -> AuditLogModel
resetAuditLogWithFilters filters auditLog =
    { auditLog
        | entries = []
        , hasMore = False
        , loading = True
        , loadingFilters = Just filters
        , filters = filters
        , expandedEntries = Dict.empty
        , revertConfirmation = Nothing
        , revertInFlight = False
    }


clearRouteConfirmations : Model -> Model
clearRouteConfirmations model =
    let
        currentEditing =
            model.editing

        currentMemory =
            model.memory

        currentDependencies =
            model.dependencies

        currentCards =
            model.cards

        currentDragDrop =
            model.dragDrop

        currentAuditLog =
            model.auditLog

        currentWorkspaceAdmin =
            model.workspaceAdmin
    in
    { model
        | editing = { currentEditing | editState = Nothing, createForm = Nothing, inlineCreate = Nothing }
        , memory = { currentMemory | linkingMemoryFor = Nothing, linkingEntityFor = Nothing }
        , dependencies = { currentDependencies | addingDependencyFor = Nothing }
        , cards = { currentCards | deleteConfirmation = Nothing }
        , dragDrop = { currentDragDrop | dragging = Nothing, dragOver = Nothing, dropActionModal = Nothing }
        , auditLog = { currentAuditLog | revertConfirmation = Nothing, revertInFlight = False }
        , workspaceAdmin = { currentWorkspaceAdmin | purgeConfirmation = Nothing }
    }
