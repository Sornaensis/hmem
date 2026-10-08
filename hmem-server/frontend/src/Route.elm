module Route exposing (..)

import Api
import Browser
import Browser.Navigation as Nav
import Dict
import Feature.DataLoading
import Feature.Dependencies
import Feature.Editing
import Feature.Observation
import Feature.Timeline
import Feature.WorkspaceAdmin
import Helpers exposing (localStorageKey, parseFragment, pushUrl, replaceFragment)
import Permissions
import Ports exposing (disconnectChangeStreamScope, requestLocalStorage)
import Json.Encode as Encode
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
            if observationContextExit url model then
                Feature.Observation.refuseContextExit model

            else
                ( model, pushUrl model.key (Url.toString url) )

        Browser.External href ->
            if Feature.Observation.hasProtectedEdit model then
                Feature.Observation.refuseContextExit model

            else
                ( model, Nav.load href )


handleUrlChange : Url.Url -> Model -> ( Model, Cmd Msg )
handleUrlChange url model =
    if observationContextExit url model then
        let
            ( preserved, noticeCmd ) =
                Feature.Observation.refuseContextExit model
        in
        ( preserved, Cmd.batch [ noticeCmd, replaceFragment preserved ] )

    else
        let
            state = model.observations
            ownEcho = state.pendingExcludedLink |> Maybe.map (\echo -> echo.url == Url.toString url && model.selectedWorkspaceId == Just echo.workspaceId && model.sessionRequestEpoch == echo.sessionEpoch && state.requestGeneration == echo.generation && state.facetRequestGeneration == echo.facetGeneration) |> Maybe.withDefault False
            retired = { model | observations = { state | pendingExcludedLink = Nothing } }
        in
        if ownEcho then
            ( { retired | url = url }, Cmd.none )
        else
            handleUrlChangeWithoutProtectedExit url retired


observationContextExit : Url.Url -> Model -> Bool
observationContextExit url model =
    Feature.Observation.hasProtectedEdit model
        && (urlToPage url /= model.page || url.host /= model.url.host || url.protocol /= model.url.protocol || url.port_ /= model.url.port_)


handleUrlChangeWithoutProtectedExit : Url.Url -> Model -> ( Model, Cmd Msg )
handleUrlChangeWithoutProtectedExit url model =
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
                    context =
                        Helpers.observationUrlContext url

                    frag =
                        context.fragment

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
                            , editState =
                                if Feature.Editing.hasProtectedWorkspaceRename model then
                                    currentEditing.editState

                                else
                                    Nothing
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
                            |> clearRouteConfirmations True

                    queryChanged =
                        context.query /= Helpers.observationAppliedQuery model.observations

                    queryInput =
                        if queryChanged then Feature.DataLoading.retireInitialObservationLoad updatedModel else updatedModel

                    routeState =
                        if queryChanged then Helpers.restoreObservationQuery context.query queryInput.observations else queryInput.observations

                    ( queryModel, queryCmd ) =
                        let
                            restored = { queryInput | observations = { routeState | linkNotice = if context.notice == Nothing && not queryChanged then routeState.linkNotice else context.notice } }
                        in
                        if queryChanged then Feature.Observation.restoreRouteResults restored else ( restored, Cmd.none )

                    ( observationModel, observationCmd ) =
                        if frag.tab == ObservationsTab then
                            if frag.observationId /= model.observations.selectedId then
                                case frag.observationId of
                                    Just observationId ->
                                        Feature.Observation.selectObservation observationId queryModel

                                    Nothing ->
                                        ( { queryModel | observations = Feature.Observation.clearSelection queryModel.observations }, Cmd.none )

                            else
                                ( queryModel, Cmd.none )

                        else
                            ( { queryModel | observations = reconcileSameWorkspaceObservationRoute frag.tab queryModel.observations }, Cmd.none )

                    ( auditModel, auditCmd ) =
                        prepareWorkspaceAuditFromRoute wsId frag.tab observationModel

                    ( timelineModel, timelineCmd ) =
                        prepareWorkspaceTimelineFromRoute wsId frag.tab auditModel

                    ( finalModel, adminCmd ) =
                        if frag.tab == AdministrationTab then
                            Feature.WorkspaceAdmin.ensureMemberships False wsId timelineModel
                        else
                            ( timelineModel, Cmd.none )

                    ( focusedModel, focusCmd ) =
                        case frag.focus of
                            Just ( entityType, entityId ) ->
                                Feature.DataLoading.beginNavigationFocus wsId entityType entityId finalModel

                            Nothing ->
                                ( finalModel, Cmd.none )
                in
                if context.notice /= Nothing && context.notice /= Just Helpers.excludedObservationNotice then
                    let
                        ( repaired, repairCmd ) = Helpers.writeObservationHistory False focusedModel
                    in
                    ( repaired, Cmd.batch [ repairCmd, queryCmd, observationCmd, auditCmd, timelineCmd, adminCmd, focusCmd ] )
                else
                    ( focusedModel, Cmd.batch [ queryCmd, observationCmd, auditCmd, timelineCmd, adminCmd, focusCmd ] )

            else
                let
                    context =
                        Helpers.observationUrlContext url

                    frag =
                        context.fragment

                    initialObservations =
                        let
                            observations =
                                Feature.Observation.init
                        in
                        Helpers.restoreObservationQuery context.query { observations | selectedId = frag.observationId, linkNotice = context.notice }

                    currentDataLoading =
                        model.dataLoading

                    updatedDataLoading =
                        { currentDataLoading
                            | initialObservationLoad = Nothing
                            , loadingWorkspaceData = True
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
                             , rootNavigationPresentation = Nothing
                             , navigationPresentations = Dict.empty
                             , projectCardSummaries = Dict.empty
                             , taskCardSummaries = Dict.empty
                             , projectCardDetailRequests = Dict.empty
                             , taskCardDetailRequests = Dict.empty
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

                    updatedDependencies =
                        Feature.Dependencies.resetCache model.dependencies

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
                            , filterShowEmptyProjects = True
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
                    , webSocket = retireWorkspaceStreams model.webSocket
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
                    |> clearRouteConfirmations False
                , Cmd.batch
                    [ Api.fetchSessionContext model.flags.apiUrl (Just wsId) (GotSessionContext (model.sessionRequestEpoch + 1) (Just wsId))
                    , requestLocalStorage (localStorageKey wsId)
                    , disconnectRouteWorkspaces model
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
                , webSocket = retireWorkspaceStreams model.webSocket
                , auditLog = updatedAuditLog
                , dependencies = Feature.Dependencies.resetCache model.dependencies
                , focus = updatedFocus
              }
                |> clearRouteConfirmations False
            , Cmd.batch
                [ Api.fetchSessionContext model.flags.apiUrl Nothing (GotSessionContext (model.sessionRequestEpoch + 1) Nothing)
                , disconnectRouteWorkspaces model
                ]
            )

        _ ->
            let
                currentFocus =
                    model.focus

                updatedFocus =
                    { currentFocus | returnContext = Nothing }
            in
            ( { model | url = url, page = page, auth = { status = AuthBooting, mode = model.auth.mode }, sessionContext = Nothing, sessionRequestEpoch = model.sessionRequestEpoch + 1, selectedWorkspaceId = Nothing, webSocket = retireWorkspaceStreams model.webSocket, dependencies = Feature.Dependencies.resetCache model.dependencies, focus = updatedFocus }
                |> clearRouteConfirmations False
            , Cmd.batch
                [ Api.fetchSessionContext model.flags.apiUrl Nothing (GotSessionContext (model.sessionRequestEpoch + 1) Nothing)
                , disconnectRouteWorkspaces model
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


clearRouteConfirmations : Bool -> Model -> Model
clearRouteConfirmations preserveProtectedWorkspaceRename model =
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
        | editing =
            { currentEditing
                | editState =
                    if preserveProtectedWorkspaceRename && Feature.Editing.hasProtectedWorkspaceRename model then
                        currentEditing.editState

                    else
                        Nothing
                , createForm = Nothing
                , inlineCreate = Nothing
            }
        , memory = { currentMemory | linkingMemoryFor = Nothing, linkingEntityFor = Nothing }
        , dependencies = { currentDependencies | addingDependencyFor = Nothing }
        , cards = { currentCards | deleteConfirmation = Nothing, lastFocusClick = Nothing }
        , dragDrop = { currentDragDrop | dragging = Nothing, dragOver = Nothing, dropActionModal = Nothing }
        , auditLog = { currentAuditLog | revertConfirmation = Nothing, revertInFlight = False }
        , workspaceAdmin = { currentWorkspaceAdmin | purgeConfirmation = Nothing }
    }


-- Workspace routing retires only workspace audiences; the catalogue owns a
-- principal lifetime and remains subscribed while the new route is admitted.
retireWorkspaceStreams : WebSocketModel -> WebSocketModel
retireWorkspaceStreams webSocket =
    { webSocket | streams = Dict.filter (\key _ -> key == "global") webSocket.streams, targetGenerations = Dict.filter (\key _ -> String.startsWith "global|" key) webSocket.targetGenerations }


disconnectRouteWorkspaces : Model -> Cmd Msg
disconnectRouteWorkspaces model =
    (Dict.keys model.webSocket.streams |> List.filter (String.startsWith "workspace:") |> List.map (String.dropLeft 10))
        ++ (model.selectedWorkspaceId |> Maybe.map List.singleton |> Maybe.withDefault [])
        |> Set.fromList
        |> Set.toList
        |> List.map (\workspaceId -> disconnectChangeStreamScope (Encode.object [ ( "scope", Encode.string "workspace" ), ( "workspaceId", Encode.string workspaceId ) ]))
        |> Cmd.batch
