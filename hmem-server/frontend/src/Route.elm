module Route exposing (..)

import Api
import Browser
import Browser.Navigation as Nav
import Dict
import Helpers exposing (localStorageKey, parseFragment)
import Ports exposing (destroyCytoscape, requestLocalStorage)
import Types exposing (..)
import Url
import Url.Parser as Parser exposing ((</>), Parser)



-- ROUTING


loadWorkspaceData : String -> String -> Maybe Int -> Cmd Msg
loadWorkspaceData apiUrl wsId maybeLoadToken =
    Cmd.batch
        [ Api.fetchProjects apiUrl wsId (GotProjects wsId maybeLoadToken)
        , Api.fetchTasks apiUrl wsId (GotTasks wsId maybeLoadToken)
        , Api.fetchMemories apiUrl wsId (GotMemories wsId maybeLoadToken)
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
                in
                ( { model
                    | url = url
                    , activeTab = frag.tab
                    , focus = updatedFocus
                    , editing = updatedEditing
                    , memory = updatedMemory
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
                , Cmd.batch
                    [ loadWorkspaceData model.flags.apiUrl wsId updatedDataLoading.activeWorkspaceLoadToken
                    , requestLocalStorage (localStorageKey wsId)
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
                , graph = updatedGraph
              }
            , case model.selectedWorkspaceId of
                Just wsId ->
                    Api.fetchVisualization model.flags.apiUrl wsId GotVisualization

                Nothing ->
                    Cmd.none
            )

        AuditLogPage ->
            let
                destroyCmd =
                    if model.page == MemoryGraphPage then
                        destroyCytoscape ()

                    else
                        Cmd.none

                emptyFilters =
                    { entityType = Nothing, entityId = Nothing, action = Nothing, since = Nothing, until = Nothing, limit = Just 50, offset = Nothing }

                currentAuditLog =
                    model.auditLog

                updatedAuditLog =
                    { currentAuditLog
                        | entries = []
                        , hasMore = False
                        , filters = emptyFilters
                        , expandedEntries = Dict.empty
                        , revertConfirmation = Nothing
                        , revertInFlight = False
                    }
            in
            ( { model
                | url = url
                , page = page
                , auditLog = updatedAuditLog
              }
            , Cmd.batch
                [ Api.fetchAuditLog model.flags.apiUrl emptyFilters GotAuditLog
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
            ( { model | url = url, page = page }, destroyCmd )
