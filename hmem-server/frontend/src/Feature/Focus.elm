module Feature.Focus exposing (auditReturnContext, buildProjectBreadcrumb, buildTaskBreadcrumb, clearReturnContext, focusReturnContextLabel, init, shouldShowReturnContext, timelineReturnContext, update, viewFocusBreadcrumbBar, viewTaskBreadcrumb)

import Api
import Browser.Navigation as Nav
import Dict
import Helpers exposing (buildFragment, replaceFragment)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Types exposing (..)


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

                currentSearch =
                    model.search

                updatedSearch =
                    { currentSearch
                        | unifiedResults = Nothing
                        , isSearching = False
                        , searchError = Nothing
                        , activeRequestQuery = Nothing
                    }

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
                        , search = updatedSearch
                    }
            in
            ( newModel, replaceFragment newModel )

        FocusEntityKeepForward entityType entityId ->
            let
                entry =
                    ( entityType, entityId )

                focusModel =
                    model.focus

                currentSearch =
                    model.search

                updatedSearch =
                    { currentSearch
                        | unifiedResults = Nothing
                        , isSearching = False
                        , searchError = Nothing
                        , activeRequestQuery = Nothing
                    }

                -- Only change what's focused, don't modify history
                newModel =
                    { model
                        | focus = { focusModel | focusedEntity = Just entry, returnContext = Nothing }
                        , search = updatedSearch
                    }
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
            in
            ( newModel, replaceFragment newModel )

        _ ->
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
    , Nav.pushUrl model.key ("/workspace/" ++ context.workspaceId ++ "#" ++ buildFragment TimelineTab Nothing)
    )


returnToWorkspaceAuditSource : FocusReturnContext -> Model -> ( Model, Cmd Msg )
returnToWorkspaceAuditSource context model =
    let
        filters =
            context.auditFilters |> Maybe.withDefault { workspaceId = Just context.workspaceId, entityType = Nothing, entityId = Nothing, action = Nothing, since = Nothing, until = Nothing, limit = Just 50, offset = Nothing }

        currentAuditLog =
            model.auditLog

        restoredAuditLog =
            { currentAuditLog
                | entries = []
                , entryBaseOffset = filters.offset |> Maybe.withDefault 0
                , hasMore = False
                , loading = True
                , loadingFilters = Just filters
                , filters = filters
                , expandedEntries = expandedEntriesForReturn context
            }

        nextModel =
            { model
                | selectedWorkspaceId = Just context.workspaceId
                , activeTab = AuditTab
                , focus = clearFocusForReturn model.focus
                , auditLog = restoredAuditLog
            }
    in
    ( nextModel
    , Cmd.batch
        [ Nav.pushUrl model.key ("/workspace/" ++ context.workspaceId ++ "#" ++ buildFragment AuditTab Nothing)
        , Api.fetchAuditLog model.flags.apiUrl filters (GotAuditLog filters)
        ]
    )


returnToGlobalAuditSource : Model -> ( Model, Cmd Msg )
returnToGlobalAuditSource model =
    ( model
    , Nav.pushUrl model.key "/audit"
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

        withParentTask =
            case task.parentId of
                Just pid ->
                    case Dict.get pid model.tasks of
                        Just parentTask ->
                            buildTaskBreadcrumb model parentTask (currentCrumb :: acc)

                        Nothing ->
                            currentCrumb :: acc

                Nothing ->
                    currentCrumb :: acc
    in
    case task.projectId of
        Just projId ->
            case Dict.get projId model.projects of
                Just proj ->
                    buildProjectBreadcrumb model proj withParentTask

                Nothing ->
                    withParentTask

        Nothing ->
            withParentTask


buildProjectBreadcrumb : Model -> Api.Project -> List ( String, String, String ) -> List ( String, String, String )
buildProjectBreadcrumb model project acc =
    let
        currentCrumb =
            ( project.id, project.name, "project" )
    in
    case project.parentId of
        Just pid ->
            case Dict.get pid model.projects of
                Just parentProj ->
                    buildProjectBreadcrumb model parentProj (currentCrumb :: acc)

                Nothing ->
                    currentCrumb :: acc

        Nothing ->
            currentCrumb :: acc
