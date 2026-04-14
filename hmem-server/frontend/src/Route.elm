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


loadWorkspaceData : String -> String -> Cmd Msg
loadWorkspaceData apiUrl wsId =
    Cmd.batch
        [ Api.fetchProjects apiUrl wsId GotProjects
        , Api.fetchTasks apiUrl wsId GotTasks
        , Api.fetchMemories apiUrl wsId GotMemories
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
                    , entityHistory = Dict.empty
                    , entityHistoryHasMore = Dict.empty
                    , historyExpanded = Dict.empty
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

        AuditLogPage ->
            let
                destroyCmd =
                    if model.page == MemoryGraphPage then
                        destroyCytoscape ()

                    else
                        Cmd.none

                emptyFilters =
                    { entityType = Nothing, entityId = Nothing, action = Nothing, since = Nothing, until = Nothing, limit = Just 50, offset = Nothing }
            in
            ( { model
                | url = url
                , page = page
                , auditLog = []
                , auditLogHasMore = False
                , auditLogFilters = emptyFilters
                , auditLogExpanded = Dict.empty
                , revertConfirmation = Nothing
                , revertInFlight = False
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
