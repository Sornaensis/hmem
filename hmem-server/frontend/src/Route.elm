module Route exposing (..)

import Api
import Browser
import Browser.Navigation as Nav
import Dict
import Feature.DataLoading
import Feature.Observation
import Feature.Timeline
import Helpers exposing (localStorageKey, parseFragment, pushUrl)
import Permissions
import Ports exposing (disconnectWebSocket, requestLocalStorage)
import Set
import Types exposing (..)
import Url
import Url.Parser as Parser exposing ((</>), Parser)



-- ROUTING


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map HomeRoute Parser.top
        , Parser.map WorkspaceRoute (Parser.s "workspace" </> Parser.string)
        , Parser.map AuditLogRoute (Parser.s "audit")
        ]


urlToPage : Url.Url -> Page
urlToPage url =
    case Parser.parse routeParser { url | fragment = Nothing } of
        Just HomeRoute ->
            HomePage

        Just (WorkspaceRoute wsId) ->
            WorkspacePage wsId

        Just AuditLogRoute ->
            AuditLogPage

        Nothing ->
            NotFound



-- URL HANDLERS


handleUrlRequest : Browser.UrlRequest -> Model -> ( Model, Cmd Msg )
handleUrlRequest urlRequest model =
    case urlRequest of
        Browser.Internal url ->
            ( model, pushUrl model.key (Url.toString url) )

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

                    routeSupersedesSearch =
                        focusChangedExternally || frag.tab /= model.activeTab

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
                            , returnContext =
                                if focusChangedExternally then
                                    Nothing

                                else
                                    currentFocus.returnContext
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

                    currentSearch =
                        model.search

                    updatedSearch =
                        if routeSupersedesSearch then
                            { currentSearch
                                | unifiedResults = Nothing
                                , isSearching = False
                                , searchError = Nothing
                                , activeRequestQuery = Nothing
                                , activeRequest = Nothing
                            }

                        else
                            currentSearch

                    updatedModel =
                        { model
                            | url = url
                            , activeTab = frag.tab
                            , focus = updatedFocus
                            , editing = updatedEditing
                            , memory = updatedMemory
                            , search = updatedSearch
                        }
                            |> clearRouteConfirmations

                    ( observationModel, observationCmd ) =
                        if frag.tab == ObservationsTab then
                            if frag.observationId /= model.observations.selectedId then
                                case frag.observationId of
                                    Just observationId ->
                                        Feature.Observation.selectObservation observationId updatedModel

                                    Nothing ->
                                        ( { updatedModel | observations = Feature.Observation.clearSelection updatedModel.observations }, Cmd.none )

                            else
                                ( updatedModel, Cmd.none )

                        else
                            ( { updatedModel | observations = reconcileSameWorkspaceObservationRoute frag.tab updatedModel.observations }, Cmd.none )

                    ( auditModel, auditCmd ) =
                        prepareWorkspaceAuditFromRoute wsId frag.tab observationModel

                    ( finalModel, timelineCmd ) =
                        prepareWorkspaceTimelineFromRoute wsId frag.tab auditModel

                    ( focusedModel, focusCmd ) =
                        case frag.focus of
                            Just ( entityType, entityId ) ->
                                Feature.DataLoading.beginNavigationFocus wsId entityType entityId finalModel

                            Nothing ->
                                ( finalModel, Cmd.none )
                in
                ( focusedModel, Cmd.batch [ observationCmd, auditCmd, timelineCmd, focusCmd ] )

            else
                let
                    frag =
                        parseFragment url.fragment

                    initialObservations =
                        let
                            observations =
                                Feature.Observation.init
                        in
                        { observations | selectedId = frag.observationId }

                    currentDataLoading =
                        model.dataLoading

                    updatedDataLoading =
                        { currentDataLoading
                            | loadingWorkspaceData = True
                            , pendingWorkspaceLoads = 0
                             , activeWorkspaceLoadToken = Just currentDataLoading.nextWorkspaceLoadToken
                             , nextWorkspaceLoadToken = currentDataLoading.nextWorkspaceLoadToken + 1
                             , cardHydrationLoaded = False
                             -- A route workspace switch invalidates every
                             -- pending focus continuation as well as cached
                             -- branch/focus keys.  Session epoch validation is
                             -- the second guard; clearing here prevents an old
                             -- retry state from suppressing the new request.
                             , navigationGeneration = currentDataLoading.navigationGeneration + 1
                             , rootNavigationRequest = Nothing
                             , loadedNavigationBranches = Dict.empty
                             , projectCardSummaries = Dict.empty
                             , taskCardSummaries = Dict.empty
                             , navigationVisibleProjectIds = Set.empty
                             , navigationVisibleTaskIds = Set.empty
                             , navigationVisibilityActive = False
                             , activeNavigationFocus = Nothing
                             , navigationFocuses = Dict.empty
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
                            , returnContext = returnContextForFocusedRoute wsId frag.focus currentFocus
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
                            , entityMemoryIds = Dict.empty
                        }

                    currentDependencies =
                        model.dependencies

                    updatedDependencies =
                        { currentDependencies
                            | taskDependencies = Dict.empty
                            , taskDependencyLinks = []
                            , taskReadinessRollups = Dict.empty
                            , projectReadinessRollups = Dict.empty
                            , addingDependencyFor = Nothing
                        }

                    currentSearch =
                        model.search

                    updatedSearch =
                        { currentSearch
                            | query = ""
                            , unifiedResults = Nothing
                            , isSearching = False
                            , searchError = Nothing
                            , activeRequestQuery = Nothing
                            , activeRequest = Nothing
                            , filterShowOnly = ShowAll
                            , filterPriority = AnyPriority
                            , filterProjectStatuses = []
                            , filterTaskStatuses = []
                            , filterMemoryTypes = []
                            , filterImportance = AnyPriority
                            , filterMemoryPinned = Nothing
                            , filterMemoryActiveLinked = False
                            , filterTags = []
                        }

                    currentCards =
                        model.cards

                    updatedCards =
                        { currentCards
                            | collapsedNodes = Dict.empty
                            , expandedCards = Dict.empty
                            , lastFocusClick = Nothing
                            , projectNextTasks = Dict.empty
                            , projectNextTaskDiagnostics = Dict.empty
                            , projectNextTasksLoading = Dict.empty
                            , projectNextTaskDiagnosticsLoading = Dict.empty
                            , projectNextTasksErrors = Dict.empty
                            , projectNextTaskDiagnosticsErrors = Dict.empty
                        }

                    currentAuditLog =
                        model.auditLog

                    updatedAuditLog =
                        { currentAuditLog
                            | entityHistory = Dict.empty
                            , entityHistoryHasMore = Dict.empty
                            , historyExpanded = Dict.empty
                        }

                    updatedTimeline =
                        Feature.Timeline.init
                in
                ( { model
                    | url = url
                    , page = page
                    , auth = { status = AuthBooting, mode = model.auth.mode }
                    , sessionContext = Nothing
                    , sessionRequestEpoch = model.sessionRequestEpoch + 1
                    , webSocket = { state = Disconnected, streams = Dict.empty, targetGenerations = Dict.empty }
                    , selectedWorkspaceId = Just wsId
                    , activeTab = frag.tab
                    , projects = Dict.empty
                    , tasks = Dict.empty
                    , memories = Dict.empty
                    , observations = initialObservations
                    , mainContentScrollY = 0
                    , dataLoading = updatedDataLoading
                    , focus = updatedFocus
                    , editing = updatedEditing
                    , memory = updatedMemory
                    , dependencies = updatedDependencies
                    , search = updatedSearch
                    , cards = updatedCards
                    , auditLog = updatedAuditLog
                    , timeline = updatedTimeline
                  }
                    |> clearRouteConfirmations
                , Cmd.batch
                    [ Api.fetchSessionContext model.flags.apiUrl (Just wsId) (GotSessionContext (model.sessionRequestEpoch + 1) (Just wsId))
                    , requestLocalStorage (localStorageKey wsId)
                    , disconnectWebSocket ()
                    ]
                )

        AuditLogPage ->
            let
                emptyFilters =
                    { workspaceId = Nothing, entityType = Nothing, entityId = Nothing, action = Nothing, since = Nothing, until = Nothing, limit = Just 50, offset = Nothing }

                returnContext =
                    model.focus.returnContext

                restoredFilters =
                    case returnContext of
                        Just context ->
                            if context.source == ReturnFromGlobalAudit then
                                context.auditFilters |> Maybe.withDefault emptyFilters

                            else
                                emptyFilters

                        Nothing ->
                            emptyFilters

                restoredExpandedEntries =
                    case returnContext of
                        Just context ->
                            if context.source == ReturnFromGlobalAudit then
                                expandedEntriesForReturnContext context

                            else
                                Dict.empty

                        Nothing ->
                            Dict.empty

                currentAuditLog =
                    model.auditLog

                updatedAuditLog =
                    { currentAuditLog
                        | entries = []
                        , entryBaseOffset = restoredFilters.offset |> Maybe.withDefault 0
                        , hasMore = False
                        , loading = False
                        , loadingFilters = Nothing
                        , filters = restoredFilters
                        , expandedEntries = restoredExpandedEntries
                        , revertConfirmation = Nothing
                        , revertInFlight = False
                    }

                currentFocus =
                    model.focus

                updatedFocus =
                    { currentFocus | returnContext = Nothing }
            in
            ( { model
                | url = url
                , page = page
                , auth = { status = AuthBooting, mode = model.auth.mode }
                , sessionContext = Nothing
                , sessionRequestEpoch = model.sessionRequestEpoch + 1
                , selectedWorkspaceId = Nothing
                , webSocket = { state = Disconnected, streams = Dict.empty, targetGenerations = Dict.empty }
                , auditLog = updatedAuditLog
                , focus = updatedFocus
              }
                |> clearRouteConfirmations
            , Cmd.batch
                [ Api.fetchSessionContext model.flags.apiUrl Nothing (GotSessionContext (model.sessionRequestEpoch + 1) Nothing)
                , disconnectWebSocket ()
                ]
            )

        _ ->
            let
                currentFocus =
                    model.focus

                updatedFocus =
                    { currentFocus | returnContext = Nothing }
            in
            ( { model | url = url, page = page, auth = { status = AuthBooting, mode = model.auth.mode }, sessionContext = Nothing, sessionRequestEpoch = model.sessionRequestEpoch + 1, selectedWorkspaceId = Nothing, webSocket = { state = Disconnected, streams = Dict.empty, targetGenerations = Dict.empty }, focus = updatedFocus }
                |> clearRouteConfirmations
            , Cmd.batch
                [ Api.fetchSessionContext model.flags.apiUrl Nothing (GotSessionContext (model.sessionRequestEpoch + 1) Nothing)
                , disconnectWebSocket ()
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


prepareWorkspaceTimelineFromRoute : String -> WorkspaceTab -> Model -> ( Model, Cmd Msg )
prepareWorkspaceTimelineFromRoute wsId tab model =
    if tab == TimelineTab then
        let
            ( timeline, timelineCmd ) =
                Feature.Timeline.ensureLoaded model.flags.apiUrl wsId model.sessionRequestEpoch model.timeline
        in
        ( { model | timeline = timeline }, timelineCmd )

    else
        ( model, Cmd.none )


{-| Reconcile observation state for the same-workspace route path. Non-observation
workspace tabs must invalidate the detail request as well as the visible selection.
-}
reconcileSameWorkspaceObservationRoute : WorkspaceTab -> ObservationModel -> ObservationModel
reconcileSameWorkspaceObservationRoute tab observations =
    if tab == ObservationsTab then
        observations

    else
        Feature.Observation.clearSelection observations


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
        , entryBaseOffset = filters.offset |> Maybe.withDefault 0
        , hasMore = False
        , loading = True
        , loadingFilters = Just filters
        , filters = filters
        , expandedEntries = Dict.empty
        , revertConfirmation = Nothing
        , revertInFlight = False
    }


returnContextForFocusedRoute : String -> Maybe ( String, String ) -> FocusModel -> Maybe FocusReturnContext
returnContextForFocusedRoute wsId maybeFocus focusModel =
    case ( maybeFocus, focusModel.returnContext ) of
        ( Just ( focusType, focusId ), Just context ) ->
            if context.workspaceId == wsId && context.entityType == focusType && context.entityId == focusId then
                Just context

            else
                Nothing

        _ ->
            Nothing


expandedEntriesForReturnContext : FocusReturnContext -> Dict.Dict String Bool
expandedEntriesForReturnContext context =
    case ( context.auditExpandedEntryId, context.auditEntryExpanded ) of
        ( Just entryId, Just True ) ->
            Dict.singleton entryId True

        _ ->
            Dict.empty


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
        , cards = { currentCards | deleteConfirmation = Nothing, lastFocusClick = Nothing }
        , dragDrop = { currentDragDrop | dragging = Nothing, dragOver = Nothing, dropActionModal = Nothing }
        , auditLog = { currentAuditLog | revertConfirmation = Nothing, revertInFlight = False }
        , workspaceAdmin = { currentWorkspaceAdmin | purgeConfirmation = Nothing }
    }
