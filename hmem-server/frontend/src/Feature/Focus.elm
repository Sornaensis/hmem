module Feature.Focus exposing (auditDestinationStatus, auditReturnContext, buildProjectBreadcrumb, buildTaskBreadcrumb, clearReturnContext, focusReturnContextLabel, init, returnContextForUrl, restoreAuditReturnAuthority, shouldShowReturnContext, timelineReturnContext, update, viewAuditReturn, viewFocusBreadcrumbBar, viewTaskBreadcrumb)

import Api
import Feature.AuditRequest as AuditRequest
import Dict
import Feature.DataLoading
import Feature.FocusedFilters
import Helpers exposing (buildFragment, pushUrl, replaceFragment, scrollToElement)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Types exposing (..)
import Url


init : Maybe ( String, String ) -> FocusModel
init maybeFocus =
    { focusedEntity = maybeFocus
    , breadcrumbAnchor = maybeFocus
    , history = Maybe.map List.singleton maybeFocus |> Maybe.withDefault []
    , historyIndex = 0
    , returnContext = Nothing
    }


timelineReturnContext : String -> TimelineModel -> Api.WorkspaceTimelineEvent -> FocusReturnContext
timelineReturnContext workspaceId timeline event =
    { source = ReturnFromTimeline
    , workspaceId = workspaceId
    , tab = TimelineTab
    , entryId = event.id
    , label = event.title
    , entityType = event.navigation.entityType
    , entityId = event.navigation.entityId
    , timelineEventId = Just event.id
    , timelineSourceAuditId = event.sourceAuditId
    , timelineOccurredAt = Just event.occurredAt
    , timelineEntityFilter = Just timeline.entityFilter
    , timelineEventFilter = Just timeline.eventFilter
    , timelineHistogramSelection = timeline.histogramSelectedBucket
    , timelineHistogramSince = nonEmptyMaybe timeline.histogramSince
    , timelineHistogramUntil = nonEmptyMaybe timeline.histogramUntil
    , timelineHistogramBucket = nonEmptyMaybe timeline.histogramBucket
    , auditFilters = Nothing
    , auditExpandedEntryId = Nothing
    , auditEntryExpanded = Nothing
    , auditScrollY = Nothing
    , auditPrincipal = Nothing
    , auditAuthMode = Nothing
    }


auditReturnContext : FocusReturnSource -> String -> AuditLogFilters -> Bool -> Api.AuditLogEntry -> ( String, String ) -> FocusReturnContext
auditReturnContext source workspaceId filters entryExpanded entry ( targetType, targetId ) =
    { source = source
    , workspaceId = workspaceId
    , tab = AuditTab
    , entryId = entry.id
    , label = entry.entityType ++ " " ++ Api.auditActionToString entry.action
    , entityType = targetType
    , entityId = targetId
    , timelineEventId = Nothing
    , timelineSourceAuditId = Nothing
    , timelineOccurredAt = Nothing
    , timelineEntityFilter = Nothing
    , timelineEventFilter = Nothing
    , timelineHistogramSelection = Nothing
    , timelineHistogramSince = Nothing
    , timelineHistogramUntil = Nothing
    , timelineHistogramBucket = Nothing
    , auditFilters = Just filters
    , auditExpandedEntryId =
        if entryExpanded then
            Just entry.id

        else
            Nothing
    , auditEntryExpanded = Just entryExpanded
    , auditScrollY = Nothing
    , auditPrincipal = Nothing
    , auditAuthMode = Nothing
    }


nonEmptyMaybe : String -> Maybe String
nonEmptyMaybe value =
    if String.isEmpty value then
        Nothing

    else
        Just value


clearReturnContext : FocusModel -> FocusModel
clearReturnContext focusModel =
    { focusModel | returnContext = Nothing }


{-| Return ownership follows the actual selected destination, including tabs
whose selection is intentionally independent of hierarchy focus.
-}
returnContextForUrl : Url.Url -> FocusModel -> Maybe FocusReturnContext
returnContextForUrl url focusModel =
    let
        fragment = Helpers.parseFragment url.fragment
        value key = url.fragment |> Maybe.withDefault "" |> String.split "&" |> List.filterMap (\pair -> case String.split "=" pair of
            [ name, raw ] -> if name == key then Url.percentDecode raw else Nothing
            _ -> Nothing) |> List.head
    in
    focusModel.returnContext |> Maybe.andThen (\context ->
        let
            workspaceMatches = url.path == "/workspace/" ++ context.workspaceId
            destinationMatches = case context.entityType of
                "project" -> fragment.tab == ProjectsTab && fragment.focus == Just ( "project", context.entityId )
                "task" -> fragment.tab == ProjectsTab && fragment.focus == Just ( "task", context.entityId )
                "observation" -> fragment.tab == ObservationsTab && fragment.focus == Nothing && fragment.observationId == Just context.entityId
                "agent_session" -> fragment.tab == SessionsTab && fragment.focus == Nothing && value "session" == Just context.entityId
                "workspace" -> fragment.tab == ProjectsTab && fragment.focus == Nothing && context.entityId == context.workspaceId
                _ -> False
        in if workspaceMatches && destinationMatches then Just context else Nothing)


restoreAuditReturnAuthority : Model -> Api.SessionContext -> Model -> Model
restoreAuditReturnAuthority previous session model =
    let
        owns value = value.auditPrincipal == Just session.principal && value.auditAuthMode == Just session.authMode
        context = returnContextForUrl model.url previous.focus |> Maybe.andThen (\value ->
            if value.source == ReturnFromTimeline || owns value then Just value else Nothing)
        state = model.focus
        pending = previous.auditLog.pendingReturn |> Maybe.andThen (\value ->
            let sourceMatches = case model.page of
                    AuditLogPage -> value.source == ReturnFromGlobalAudit
                    WorkspacePage workspace -> value.source == ReturnFromWorkspaceAudit && model.activeTab == AuditTab && (value.auditFilters |> Maybe.andThen .workspaceId) == Just workspace
                    _ -> False
            in if owns value && sourceMatches then Just value else Nothing)
        audit = model.auditLog
        rejected = previous.auditLog.pendingReturn /= Nothing && pending == Nothing
        filters = audit.filters
        clearedFilters = { filters | workspaceId = case model.page of
                WorkspacePage workspace -> Just workspace
                _ -> Nothing
            , entityType = Nothing, entityId = Nothing, action = Nothing, since = Nothing, until = Nothing, offset = Nothing }
        restoredAudit = case pending of
            Just value -> { audit | pendingReturn = pending, filters = Maybe.withDefault audit.filters value.auditFilters, expandedEntries = expandedEntriesForReturn value, restoreScrollY = value.auditScrollY }
            Nothing -> if rejected then { audit | pendingReturn = Nothing, restoreScrollY = Nothing, filters = clearedFilters, entries = [], expandedEntries = Dict.empty, entryBaseOffset = 0, hasMore = False, loading = False, loadingFilters = Nothing, activeRequest = Nothing, activeNavigationId = Nothing, error = Nothing } else audit
    in
    { model | focus = { state | returnContext = context }, auditLog = restoredAudit }


viewAuditReturn : Model -> Html Msg
viewAuditReturn model =
    case model.focus.returnContext of
        Just context ->
            if context.source /= ReturnFromTimeline then
                div [ class "audit-return-bar" ] [ button [ class "btn btn-secondary focus-return-btn", onClick ReturnToFocusSource ] [ text (focusReturnContextLabel context) ] ]
            else text ""
        Nothing -> text ""


auditDestinationStatus : Model -> Maybe String
auditDestinationStatus model =
    model.focus.returnContext |> Maybe.andThen (\context ->
        if context.source /= ReturnFromTimeline && List.member context.entityType [ "project", "task" ] then
            case Dict.get (context.entityType ++ ":" ++ context.entityId) model.dataLoading.navigationFocuses of
                Just request ->
                    if request.inFlight then Just "Loading entity destination..."
                    else if not request.succeeded then Just "Entity destination unavailable. It may no longer exist or be accessible."
                    else Nothing
                Nothing -> Just "Loading entity destination..."
        else if context.source /= ReturnFromTimeline && context.entityType == "observation" && model.observations.detailError /= Nothing then
            Just "Entity destination unavailable. It may no longer exist or be accessible."
        else Nothing)


shouldShowReturnContext : FocusModel -> Bool
shouldShowReturnContext focusModel =
    focusModel.returnContext /= Nothing


focusReturnContextLabel : FocusReturnContext -> String
focusReturnContextLabel context =
    case context.source of
        ReturnFromTimeline ->
            "Back to Timeline event"

        ReturnFromWorkspaceAudit ->
            "Back to workspace Audit entry"

        ReturnFromGlobalAudit ->
            "Back to global Audit entry"


clearFocusForReturn : FocusModel -> FocusModel
clearFocusForReturn focusModel =
    { focusModel
        | focusedEntity = Nothing
        , breadcrumbAnchor = Nothing
        , history = []
        , historyIndex = 0
        , returnContext = Nothing
    }


expandedEntriesForReturn : FocusReturnContext -> Dict.Dict String Bool
expandedEntriesForReturn context =
    case ( context.auditExpandedEntryId, context.auditEntryExpanded ) of
        ( Just entryId, Just True ) ->
            Dict.singleton entryId True

        _ ->
            Dict.empty


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReturnToFocusSource ->
            returnToFocusSource model

        FocusEntity entityType entityId ->
            let
                entry =
                    ( entityType, entityId )

                focusModel =
                    model.focus

                -- Truncate any forward history, then append
                newHistory =
                    List.take (focusModel.historyIndex + 1) focusModel.history ++ [ entry ]

                newIndex =
                    List.length newHistory - 1

                newModel =
                    { model
                        | focus =
                            { focusModel
                                | focusedEntity = Just entry
                                , breadcrumbAnchor = Just entry
                                , history = newHistory
                                , historyIndex = newIndex
                                , returnContext = Nothing
                            }
                    }
                        |> Feature.FocusedFilters.enter
                        |> Feature.DataLoading.resetNavigationPresentations

                ( requestedModel, requestCmd ) =
                    requestMissingFocus entityType entityId newModel
            in
            ( requestedModel, Cmd.batch [ replaceFragment requestedModel, requestCmd ] )

        FocusEntityKeepForward entityType entityId ->
            let
                entry =
                    ( entityType, entityId )

                focusModel =
                    model.focus

                -- Only change what's focused, don't modify history
                newModel =
                    { model
                        | focus = { focusModel | focusedEntity = Just entry, returnContext = Nothing }
                    }
                        |> Feature.FocusedFilters.enter
                        |> Feature.DataLoading.resetNavigationPresentations
            in
            ( newModel, replaceFragment newModel )

        FocusBreadcrumbNav idx ->
            let
                focusModel =
                    model.focus

                entry =
                    focusModel.history
                        |> List.drop idx
                        |> List.head

                newModel =
                    { model
                        | focus =
                            { focusModel
                                | focusedEntity = entry
                                , breadcrumbAnchor = entry
                                , historyIndex = idx
                                , returnContext = Nothing
                            }
                    }
                        |> Feature.FocusedFilters.enter
                        |> Feature.DataLoading.resetNavigationPresentations
            in
            ( newModel, replaceFragment newModel )

        ClearFocus ->
            let
                focusModel =
                    model.focus

                newModel =
                    { model
                        | focus =
                            { focusModel
                                | focusedEntity = Nothing
                                , breadcrumbAnchor = Nothing
                                , history = []
                                , historyIndex = 0
                                , returnContext = Nothing
                            }
                    }
                        |> Feature.DataLoading.resetNavigationPresentations
            in
            ( newModel, replaceFragment newModel )

        _ ->
            ( model, Cmd.none )


{-| Focus links can target an entity outside the bounded root/branch cache.
The loader distinguishes a complete cached path from a cached target whose
ancestry still needs bounded summary hydration.
-}
requestMissingFocus : String -> String -> Model -> ( Model, Cmd Msg )
requestMissingFocus entityType entityId model =
    case model.selectedWorkspaceId of
        Just workspaceId ->
            Feature.DataLoading.beginNavigationFocus workspaceId entityType entityId model

        Nothing ->
            ( model, Cmd.none )


returnToFocusSource : Model -> ( Model, Cmd Msg )
returnToFocusSource model =
    case model.focus.returnContext of
        Nothing ->
            ( model, Cmd.none )

        Just context ->
            case context.source of
                ReturnFromTimeline ->
                    returnToTimelineSource context model

                ReturnFromWorkspaceAudit ->
                    returnToWorkspaceAuditSource context model

                ReturnFromGlobalAudit ->
                    returnToGlobalAuditSource model


returnToTimelineSource : FocusReturnContext -> Model -> ( Model, Cmd Msg )
returnToTimelineSource context model =
    let
        currentTimeline =
            model.timeline

        restoredTimeline =
            { currentTimeline
                | entityFilter = context.timelineEntityFilter |> Maybe.withDefault currentTimeline.entityFilter
                , eventFilter = context.timelineEventFilter |> Maybe.withDefault currentTimeline.eventFilter
                , histogramSelectedBucket = context.timelineHistogramSelection
                , histogramSince = context.timelineHistogramSince |> Maybe.withDefault currentTimeline.histogramSince
                , histogramUntil = context.timelineHistogramUntil |> Maybe.withDefault currentTimeline.histogramUntil
                , histogramBucket = context.timelineHistogramBucket |> Maybe.withDefault currentTimeline.histogramBucket
            }

        nextModel =
            { model
                | selectedWorkspaceId = Just context.workspaceId
                , activeTab = TimelineTab
                , focus = clearFocusForReturn model.focus
                , timeline = restoredTimeline
            }
    in
    ( nextModel
    , Cmd.batch
        [ pushUrl model.key ("/workspace/" ++ context.workspaceId ++ "#" ++ buildFragment TimelineTab Nothing Nothing)
        , scrollToElement ("timeline-event-" ++ context.entryId)
        ]
    )


returnToWorkspaceAuditSource : FocusReturnContext -> Model -> ( Model, Cmd Msg )
returnToWorkspaceAuditSource context model =
    let
        filters =
            context.auditFilters |> Maybe.withDefault { workspaceId = Just context.workspaceId, entityType = Nothing, entityId = Nothing, action = Nothing, since = Nothing, until = Nothing, limit = Just 50, offset = Nothing }

        currentAuditLog =
            model.auditLog

        restoredAuditLog =
            { currentAuditLog | expandedEntries = expandedEntriesForReturn context, restoreScrollY = context.auditScrollY, pendingReturn = Just context }

        nextModel =
            { model
                | selectedWorkspaceId = filters.workspaceId
                , activeTab = AuditTab
                , focus = clearFocusForReturn model.focus
                , auditLog = restoredAuditLog
            }

        ( requestedModel, requestCmd ) =
            AuditRequest.restore filters nextModel
    in
    ( requestedModel
    , Cmd.batch
        [ pushUrl model.key ("/workspace/" ++ Maybe.withDefault context.workspaceId filters.workspaceId ++ "#" ++ buildFragment AuditTab Nothing Nothing)
        , requestCmd
        ]
    )


returnToGlobalAuditSource : Model -> ( Model, Cmd Msg )
returnToGlobalAuditSource model =
    ( model
    , pushUrl model.key "/audit"
    )


viewFocusBreadcrumbBar : Model -> Html Msg
viewFocusBreadcrumbBar model =
    case model.focus.breadcrumbAnchor of
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
                    model.focus.focusedEntity |> Maybe.map Tuple.second |> Maybe.withDefault ""

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
                    model.focus.history
                        |> List.drop model.focus.historyIndex
                        |> List.head

                isOnParent =
                    historyEntry /= model.focus.focusedEntity

                forwardStartIdx =
                    if isOnParent then
                        model.focus.historyIndex

                    else
                        model.focus.historyIndex + 1

                forwardCrumbs =
                    model.focus.history
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

                returnSection =
                    case model.focus.returnContext of
                        Just context ->
                            if context.source /= ReturnFromTimeline then [] else
                            [ button [ class "focus-return-btn", onClick ReturnToFocusSource, title context.label ] [ text (focusReturnContextLabel context) ]
                            , span [ class "focus-crumb-sep" ] [ text " › " ]
                            ]

                        Nothing ->
                            []
            in
            div [ class "focus-breadcrumb-bar" ]
                ([ button [ class "focus-clear-btn", onClick ClearFocus, title "Exit focus mode" ] [ text "✕" ] ]
                    ++ returnSection
                    ++ [ span [ class "focus-crumb focus-crumb-link", onClick ClearFocus ] [ text "All" ] ]
                    ++ (if not (List.isEmpty treeCrumbLinks) then
                            span [ class "focus-crumb-sep" ] [ text " › " ] :: treeCrumbLinks ++ forwardSection

                        else
                            []
                       )
                )

        Nothing ->
            text ""


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

        withCurrent = currentCrumb :: acc
    in
    if List.any (\( id, _, kind ) -> id == task.id && kind == "task") acc then
        acc

    else
        case task.parentId |> Maybe.andThen (\id -> Dict.get id model.tasks) |> Maybe.andThen (\parent -> if parent.workspaceId == task.workspaceId then Just parent else Nothing) of
            Just parent ->
                buildTaskBreadcrumb model parent withCurrent

            Nothing ->
                case task.projectId |> Maybe.andThen (\id -> Dict.get id model.projects) |> Maybe.andThen (\project -> if project.workspaceId == task.workspaceId then Just project else Nothing) of
                    Just project -> buildProjectBreadcrumb model project withCurrent
                    Nothing -> withCurrent


buildProjectBreadcrumb : Model -> Api.Project -> List ( String, String, String ) -> List ( String, String, String )
buildProjectBreadcrumb model project acc =
    let
        currentCrumb =
            ( project.id, project.name, "project" )
    in
    if List.any (\( id, _, kind ) -> id == project.id && kind == "project") acc then
        acc

    else
        case project.parentId |> Maybe.andThen (\id -> Dict.get id model.projects) |> Maybe.andThen (\parent -> if parent.workspaceId == project.workspaceId then Just parent else Nothing) of
            Just parent ->
                buildProjectBreadcrumb model parent (currentCrumb :: acc)

            Nothing ->
                currentCrumb :: acc
