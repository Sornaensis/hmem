module Feature.Timeline exposing (ensureLoaded, eventInTimelineSelection, filterTimelineEvents, filterTimelineEventsForSelection, groupTimelineEvents, init, sortTimelineEvents, timelineBucketTotal, timelineDateKey, timelineEventLabel, timelineEventToneClass, timelineHistogramAcceptsResponse, timelineStatusSummary, update, viewWorkspaceTimelinePanel)

import Api
import Browser.Navigation as Nav
import Char
import Feature.Focus as Focus
import Helpers exposing (buildFragment, formatDate)
import Html exposing (..)
import Html.Attributes exposing (class, disabled, style, title, type_, value)
import Html.Events exposing (onClick, onInput)
import String
import Task
import Time
import Types exposing (..)


init : TimelineModel
init =
    { events = []
    , hasMore = False
    , loading = False
    , loadingWorkspaceId = Nothing
    , error = Nothing
    , loadedWorkspaceId = Nothing
    , eventsActiveRequest = Nothing
    , eventsLoadedRequest = Nothing
    , entityFilter = TimelineAllEntities
    , eventFilter = TimelineAllEvents
    , histogramBuckets = []
    , histogramLoading = False
    , histogramError = Nothing
    , histogramSince = ""
    , histogramUntil = ""
    , histogramBucket = "week"
    , histogramClockWorkspaceId = Nothing
    , histogramActiveRequest = Nothing
    , histogramLoadedRequest = Nothing
    , histogramSelectedBucket = Nothing
    }


ensureLoaded : String -> String -> TimelineModel -> ( TimelineModel, Cmd Msg )
ensureLoaded apiUrl wsId timeline =
    let
        ( eventsTimeline, eventCmd ) =
            ensureEventsLoaded apiUrl wsId timeline

        ( histogramTimeline, histogramCmd ) =
            ensureHistogramLoaded apiUrl wsId eventsTimeline
    in
    ( histogramTimeline, Cmd.batch [ eventCmd, histogramCmd ] )


ensureEventsLoaded : String -> String -> TimelineModel -> ( TimelineModel, Cmd Msg )
ensureEventsLoaded apiUrl wsId timeline =
    let
        request =
            timelineEventsRequest wsId timeline
    in
    if timeline.eventsLoadedRequest == Just request || timeline.eventsActiveRequest == Just request then
        ( timeline, Cmd.none )

    else
        startEventsFetch apiUrl request timeline


startEventsFetch : String -> TimelineEventsRequest -> TimelineModel -> ( TimelineModel, Cmd Msg )
startEventsFetch apiUrl request timeline =
    ( { timeline
        | loading = True
        , loadingWorkspaceId = Just request.workspaceId
        , error = Nothing
        , events = []
        , hasMore = False
        , loadedWorkspaceId = Nothing
        , eventsActiveRequest = Just request
        , eventsLoadedRequest = Nothing
      }
    , Api.fetchWorkspaceTimelineRange apiUrl request.workspaceId request.since request.until (GotWorkspaceTimeline request)
    )


timelineEventsRequest : String -> TimelineModel -> TimelineEventsRequest
timelineEventsRequest wsId timeline =
    case timeline.histogramSelectedBucket of
        Just selection ->
            { workspaceId = wsId
            , since = Just selection.since
            , until = Just selection.until
            }

        Nothing ->
            { workspaceId = wsId
            , since = Nothing
            , until = Nothing
            }


ensureHistogramLoaded : String -> String -> TimelineModel -> ( TimelineModel, Cmd Msg )
ensureHistogramLoaded apiUrl wsId timeline =
    case timelineHistogramRequest wsId timeline of
        Just request ->
            if timeline.histogramLoadedRequest == Just request || timeline.histogramActiveRequest == Just request then
                ( timeline, Cmd.none )

            else
                startHistogramFetch apiUrl request timeline

        Nothing ->
            if timeline.histogramClockWorkspaceId == Just wsId then
                ( timeline, Cmd.none )

            else
                ( { timeline | histogramLoading = True, histogramError = Nothing, histogramBuckets = [], histogramClockWorkspaceId = Just wsId }
                , Task.perform (GotTimelineHistogramClock wsId) Time.now
                )


startHistogramFetch : String -> TimelineHistogramRequest -> TimelineModel -> ( TimelineModel, Cmd Msg )
startHistogramFetch apiUrl request timeline =
    ( { timeline
        | histogramLoading = True
        , histogramError = Nothing
        , histogramBuckets = []
        , histogramClockWorkspaceId = Nothing
        , histogramActiveRequest = Just request
        , histogramLoadedRequest = Nothing
      }
    , Api.fetchWorkspaceTimelineBuckets apiUrl request.workspaceId request.since request.until request.bucket (GotWorkspaceTimelineBuckets request)
    )


timelineHistogramRequest : String -> TimelineModel -> Maybe TimelineHistogramRequest
timelineHistogramRequest wsId timeline =
    if completeDateInput timeline.histogramSince && completeDateInput timeline.histogramUntil && validTimelineBucket timeline.histogramBucket then
        Just
            { workspaceId = wsId
            , since = timeline.histogramSince ++ "T00:00:00Z"
            , until = timeline.histogramUntil ++ "T00:00:00Z"
            , bucket = timeline.histogramBucket
            }

    else
        Nothing


completeDateInput : String -> Bool
completeDateInput dateValue =
    String.length dateValue == 10


validTimelineBucket : String -> Bool
validTimelineBucket bucket =
    List.member bucket [ "day", "week", "month", "quarter" ]


defaultTimelineHistogramRange : Time.Posix -> { since : String, until : String }
defaultTimelineHistogramRange now =
    let
        millisPerDay =
            24 * 60 * 60 * 1000

        nowMillis =
            Time.posixToMillis now
    in
    { since = dateInputFromPosix (Time.millisToPosix (nowMillis - (90 * millisPerDay)))
    , until = dateInputFromPosix (Time.millisToPosix (nowMillis + millisPerDay))
    }


dateInputFromPosix : Time.Posix -> String
dateInputFromPosix posix =
    String.fromInt (Time.toYear Time.utc posix)
        ++ "-"
        ++ pad2 (monthNumber (Time.toMonth Time.utc posix))
        ++ "-"
        ++ pad2 (Time.toDay Time.utc posix)


monthNumber : Time.Month -> Int
monthNumber month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


pad2 : Int -> String
pad2 number =
    if number < 10 then
        "0" ++ String.fromInt number

    else
        String.fromInt number


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavigateToTimelineEntity event ->
            case model.selectedWorkspaceId of
                Just wsId ->
                    let
                        targetTab =
                            ProjectsTab

                        focusEntry =
                            ( event.navigation.entityType, event.navigation.entityId )

                        focusModel =
                            model.focus

                        newHistory =
                            List.take (focusModel.historyIndex + 1) focusModel.history ++ [ focusEntry ]

                        newIndex =
                            List.length newHistory - 1

                        updatedFocus =
                            { focusModel
                                | focusedEntity = Just focusEntry
                                , breadcrumbAnchor = Just focusEntry
                                , history = newHistory
                                , historyIndex = newIndex
                                , returnContext = Just (Focus.timelineReturnContext wsId model.timeline event)
                            }

                        currentSearch =
                            model.search

                        updatedSearch =
                            { currentSearch | unifiedResults = Nothing, isSearching = False }
                    in
                    ( { model | activeTab = targetTab, focus = updatedFocus, search = updatedSearch }
                    , Nav.pushUrl model.key ("/workspace/" ++ wsId ++ "#" ++ buildFragment targetTab (Just focusEntry))
                    )

                Nothing ->
                    ( model, Cmd.none )

        SetTimelineEntityFilter entityFilter ->
            let
                currentTimeline =
                    model.timeline
            in
            ( { model | timeline = { currentTimeline | entityFilter = entityFilter } }, Cmd.none )

        SetTimelineEventFilter eventFilter ->
            let
                currentTimeline =
                    model.timeline
            in
            ( { model | timeline = { currentTimeline | eventFilter = eventFilter } }, Cmd.none )

        SetTimelineHistogramSince since ->
            updateHistogramControls model (\timeline -> { timeline | histogramSince = since, histogramSelectedBucket = Nothing })

        SetTimelineHistogramUntil until ->
            updateHistogramControls model (\timeline -> { timeline | histogramUntil = until, histogramSelectedBucket = Nothing })

        SetTimelineHistogramBucket bucket ->
            updateHistogramControls model (\timeline -> { timeline | histogramBucket = bucket, histogramSelectedBucket = Nothing })

        SelectTimelineHistogramBucket label since until ->
            selectHistogramBucket model { label = label, since = since, until = until }

        ResetTimelineHistogramSelection ->
            resetHistogramSelection model

        GotWorkspaceTimeline request result ->
            if model.selectedWorkspaceId /= Just request.workspaceId || model.timeline.eventsActiveRequest /= Just request then
                ( model, Cmd.none )

            else
                let
                    currentTimeline =
                        model.timeline
                in
                case result of
                    Ok paginated ->
                        ( { model
                            | timeline =
                                { currentTimeline
                                    | events = paginated.items
                                    , hasMore = paginated.hasMore
                                    , loading = False
                                    , loadingWorkspaceId = Nothing
                                    , error = Nothing
                                    , loadedWorkspaceId = Just request.workspaceId
                                    , eventsActiveRequest = Nothing
                                    , eventsLoadedRequest = Just request
                                }
                          }
                        , Cmd.none
                        )

                    Err _ ->
                        ( { model
                            | timeline =
                                { currentTimeline
                                    | loading = False
                                    , loadingWorkspaceId = Nothing
                                    , error = Just "Failed to load timeline events."
                                    , loadedWorkspaceId = Nothing
                                    , eventsActiveRequest = Nothing
                                    , eventsLoadedRequest = Nothing
                                }
                          }
                        , Cmd.none
                        )

        GotTimelineHistogramClock wsId now ->
            if model.selectedWorkspaceId /= Just wsId || model.timeline.histogramClockWorkspaceId /= Just wsId then
                ( model, Cmd.none )

            else
                let
                    currentTimeline =
                        model.timeline

                    defaultRange =
                        defaultTimelineHistogramRange now

                    initializedTimeline =
                        { currentTimeline
                            | histogramSince =
                                if String.isEmpty currentTimeline.histogramSince then
                                    defaultRange.since

                                else
                                    currentTimeline.histogramSince
                            , histogramUntil =
                                if String.isEmpty currentTimeline.histogramUntil then
                                    defaultRange.until

                                else
                                    currentTimeline.histogramUntil
                            , histogramClockWorkspaceId = Nothing
                        }
                in
                updateHistogramControls { model | timeline = initializedTimeline } identity

        GotWorkspaceTimelineBuckets request result ->
            if model.selectedWorkspaceId /= Just request.workspaceId || not (timelineHistogramAcceptsResponse request model.timeline) then
                ( model, Cmd.none )

            else
                let
                    currentTimeline =
                        model.timeline
                in
                case result of
                    Ok response ->
                        ( { model
                            | timeline =
                                { currentTimeline
                                    | histogramBuckets = response.buckets
                                    , histogramLoading = False
                                    , histogramError = Nothing
                                    , histogramClockWorkspaceId = Nothing
                                    , histogramActiveRequest = Nothing
                                    , histogramLoadedRequest = Just request
                                }
                          }
                        , Cmd.none
                        )

                    Err _ ->
                        ( { model
                            | timeline =
                                { currentTimeline
                                    | histogramBuckets = []
                                    , histogramLoading = False
                                    , histogramError = Just "Failed to load timeline histogram."
                                    , histogramClockWorkspaceId = Nothing
                                    , histogramActiveRequest = Nothing
                                    , histogramLoadedRequest = Nothing
                                }
                          }
                        , Cmd.none
                        )

        _ ->
            ( model, Cmd.none )


selectHistogramBucket : Model -> TimelineHistogramSelection -> ( Model, Cmd Msg )
selectHistogramBucket model selection =
    let
        currentTimeline =
            model.timeline

        nextSelection =
            if currentTimeline.histogramSelectedBucket == Just selection then
                Nothing

            else
                Just selection

        updatedTimeline =
            { currentTimeline | histogramSelectedBucket = nextSelection }
    in
    fetchEventsForTimeline model updatedTimeline


resetHistogramSelection : Model -> ( Model, Cmd Msg )
resetHistogramSelection model =
    let
        currentTimeline =
            model.timeline

        updatedTimeline =
            { currentTimeline | histogramSelectedBucket = Nothing }
    in
    fetchEventsForTimeline model updatedTimeline


fetchEventsForTimeline : Model -> TimelineModel -> ( Model, Cmd Msg )
fetchEventsForTimeline model timeline =
    case model.selectedWorkspaceId of
        Just wsId ->
            let
                request =
                    timelineEventsRequest wsId timeline

                ( nextTimeline, cmd ) =
                    startEventsFetch model.flags.apiUrl request timeline
            in
            ( { model | timeline = nextTimeline }, cmd )

        Nothing ->
            ( { model | timeline = timeline }, Cmd.none )


updateHistogramControls : Model -> (TimelineModel -> TimelineModel) -> ( Model, Cmd Msg )
updateHistogramControls model updateTimeline =
    let
        updatedTimeline =
            updateTimeline model.timeline

        selectionChanged =
            model.timeline.histogramSelectedBucket /= updatedTimeline.histogramSelectedBucket

        finishWithOptionalEventRefresh nextTimeline histogramCmd =
            if selectionChanged then
                case model.selectedWorkspaceId of
                    Just wsId ->
                        let
                            eventRequest =
                                timelineEventsRequest wsId nextTimeline

                            ( eventTimeline, eventCmd ) =
                                startEventsFetch model.flags.apiUrl eventRequest nextTimeline
                        in
                        ( { model | timeline = eventTimeline }, Cmd.batch [ histogramCmd, eventCmd ] )

                    Nothing ->
                        ( { model | timeline = nextTimeline }, histogramCmd )

            else
                ( { model | timeline = nextTimeline }, histogramCmd )
    in
    case model.selectedWorkspaceId of
        Just wsId ->
            case timelineHistogramRequest wsId updatedTimeline of
                Just request ->
                    let
                        ( nextTimeline, cmd ) =
                            startHistogramFetch model.flags.apiUrl request updatedTimeline
                    in
                    finishWithOptionalEventRefresh nextTimeline cmd

                Nothing ->
                    finishWithOptionalEventRefresh
                        { updatedTimeline
                            | histogramLoading = False
                            , histogramClockWorkspaceId = Nothing
                            , histogramActiveRequest = Nothing
                            , histogramLoadedRequest = Nothing
                            , histogramBuckets = []
                            , histogramError = Nothing
                        }
                        Cmd.none

        Nothing ->
            ( { model | timeline = updatedTimeline }, Cmd.none )


timelineHistogramAcceptsResponse : TimelineHistogramRequest -> TimelineModel -> Bool
timelineHistogramAcceptsResponse request timeline =
    timeline.histogramActiveRequest == Just request


viewWorkspaceTimelinePanel : String -> Model -> Html Msg
viewWorkspaceTimelinePanel wsId model =
    let
        timeline =
            model.timeline
    in
    div [ class "timeline-panel" ]
        [ div [ class "timeline-panel-header" ]
            [ h3 [] [ text "Timeline" ]
            , p [] [ text "Curated task and project lifecycle events for this workspace." ]
            ]
        , viewTimelineHistogram timeline
        , if timeline.loading && List.isEmpty timeline.events then
            div [ class "loading-indicator" ] [ text "Loading timeline..." ]

          else
            viewTimelineBody wsId timeline
        ]


viewTimelineHistogram : TimelineModel -> Html Msg
viewTimelineHistogram timeline =
    div [ class "timeline-histogram-panel" ]
        [ div [ class "timeline-histogram-header" ]
            [ div []
                [ h4 [] [ text "Activity histogram" ]
                , p [] [ text "Lifecycle counts by UTC bucket. The event list below remains usable if this summary is unavailable." ]
                ]
            , viewTimelineHistogramLegend
            ]
        , viewTimelineHistogramControls timeline
        , viewTimelineHistogramContent timeline
        ]


viewTimelineHistogramControls : TimelineModel -> Html Msg
viewTimelineHistogramControls timeline =
    div [ class "timeline-histogram-controls" ]
        [ label [ class "timeline-histogram-control" ]
            [ span [] [ text "Since" ]
            , input
                [ type_ "date"
                , value timeline.histogramSince
                , onInput SetTimelineHistogramSince
                , disabled timeline.histogramLoading
                ]
                []
            ]
        , label [ class "timeline-histogram-control" ]
            [ span [] [ text "Until" ]
            , input
                [ type_ "date"
                , value timeline.histogramUntil
                , onInput SetTimelineHistogramUntil
                , disabled timeline.histogramLoading
                , title "Exclusive UTC date boundary"
                ]
                []
            ]
        , label [ class "timeline-histogram-control" ]
            [ span [] [ text "Bucket" ]
            , select
                [ value timeline.histogramBucket
                , onInput SetTimelineHistogramBucket
                , disabled timeline.histogramLoading
                ]
                [ option [ value "day" ] [ text "Day" ]
                , option [ value "week" ] [ text "Week" ]
                , option [ value "month" ] [ text "Month" ]
                , option [ value "quarter" ] [ text "Quarter" ]
                ]
            ]
        ]


viewTimelineHistogramLegend : Html Msg
viewTimelineHistogramLegend =
    div [ class "timeline-histogram-legend" ]
        [ span [ class "timeline-histogram-legend-item" ]
            [ span [ class "timeline-histogram-swatch timeline-histogram-created" ] []
            , text "Created"
            ]
        , span [ class "timeline-histogram-legend-item" ]
            [ span [ class "timeline-histogram-swatch timeline-histogram-completed" ] []
            , text "Completed"
            ]
        , span [ class "timeline-histogram-legend-item" ]
            [ span [ class "timeline-histogram-swatch timeline-histogram-cancelled" ] []
            , text "Cancelled"
            ]
        ]


viewTimelineHistogramContent : TimelineModel -> Html Msg
viewTimelineHistogramContent timeline =
    let
        hasBuckets =
            not (List.isEmpty timeline.histogramBuckets)
    in
    div [ class "timeline-histogram-content" ]
        [ if timeline.histogramLoading && not hasBuckets then
            div [ class "timeline-histogram-state" ] [ text "Loading histogram..." ]

          else
            text ""
        , case timeline.histogramError of
            Just message ->
                div [ class "timeline-histogram-state timeline-histogram-error" ] [ text message ]

            Nothing ->
                text ""
        , if (not timeline.histogramLoading) && timeline.histogramError == Nothing && not hasBuckets then
            div [ class "timeline-histogram-state" ] [ text "No histogram activity in this date range." ]

          else if hasBuckets then
            viewTimelineHistogramChart timeline

          else
            text ""
        ]


viewTimelineHistogramChart : TimelineModel -> Html Msg
viewTimelineHistogramChart timeline =
    let
        maxTotal =
            timeline.histogramBuckets
                |> List.map timelineBucketTotal
                |> List.maximum
                |> Maybe.withDefault 0
                |> max 1
    in
    div [ class "timeline-histogram-chart", title "Timeline bucket counts by lifecycle action and entity kind" ]
        (List.map (viewTimelineHistogramBucket timeline.histogramSelectedBucket maxTotal) timeline.histogramBuckets)


viewTimelineHistogramBucket : Maybe TimelineHistogramSelection -> Int -> Api.WorkspaceTimelineBucket -> Html Msg
viewTimelineHistogramBucket selectedBucket maxTotal bucket =
    let
        isSelected =
            selectedBucket == Just { label = bucket.label, since = bucket.bucketStart, until = bucket.bucketEnd }

        bucketClass =
            if isSelected then
                "timeline-histogram-bucket timeline-histogram-bucket-selected"

            else
                "timeline-histogram-bucket"
    in
    button
        [ class bucketClass
        , title (timelineBucketTooltip bucket)
        , onClick (SelectTimelineHistogramBucket bucket.label bucket.bucketStart bucket.bucketEnd)
        ]
        [ div [ class "timeline-histogram-bar" ]
            (timelineBucketSegments maxTotal bucket)
        , div [ class "timeline-histogram-bucket-label" ] [ text bucket.label ]
        , div [ class "timeline-histogram-bucket-total" ] [ text (String.fromInt (timelineBucketTotal bucket) ++ " events") ]
        , div [ class "timeline-histogram-entity-counts" ]
            [ span [] [ text ("P " ++ String.fromInt (timelineBucketEntityTotal bucket.counts.project)) ]
            , span [] [ text ("SP " ++ String.fromInt (timelineBucketEntityTotal bucket.counts.subproject)) ]
            , span [] [ text ("T " ++ String.fromInt (timelineBucketEntityTotal bucket.counts.task)) ]
            , span [] [ text ("ST " ++ String.fromInt (timelineBucketEntityTotal bucket.counts.subtask)) ]
            ]
        ]


viewTimelineBody : String -> TimelineModel -> Html Msg
viewTimelineBody wsId timeline =
    case timeline.error of
        Just message ->
            div [ class "empty-state timeline-state" ]
                [ h3 [] [ text "Timeline unavailable" ]
                , p [] [ text message ]
                , button [ class "btn-secondary", onClick (SwitchTab TimelineTab) ] [ text "Retry" ]
                ]

        Nothing ->
            if List.isEmpty timeline.events && timeline.histogramSelectedBucket == Nothing then
                div [ class "empty-state timeline-state" ]
                    [ h3 [] [ text "No timeline events yet" ]
                    , p [] [ text "Create or complete tasks and projects to populate this workspace timeline." ]
                    ]

            else
                let
                    visibleEvents =
                        timeline.events
                            |> filterTimelineEventsForSelection timeline.histogramSelectedBucket timeline.entityFilter timeline.eventFilter
                            |> sortTimelineEvents

                    groupedEvents =
                        groupTimelineEvents visibleEvents
                in
                div []
                    [ viewTimelineSelectionNote timeline.histogramSelectedBucket
                    , viewTimelineFilters timeline
                    , if timeline.hasMore then
                        p [ class "timeline-more-note" ] [ text (timelineMoreNote timeline.histogramSelectedBucket) ]

                      else
                        text ""
                    , if List.isEmpty visibleEvents then
                        viewEmptyTimelineSelection timeline.histogramSelectedBucket

                      else
                        div [ class "timeline-event-list", title ("Timeline for workspace " ++ wsId) ]
                            (List.map viewTimelineGroup groupedEvents)
                    ]


viewTimelineSelectionNote : Maybe TimelineHistogramSelection -> Html Msg
viewTimelineSelectionNote maybeSelection =
    case maybeSelection of
        Just selection ->
            div [ class "timeline-selection-note" ]
                [ span [] [ text ("Showing events for " ++ selection.label) ]
                , span [ class "timeline-selection-range" ] [ text (selection.since ++ " → " ++ selection.until) ]
                , button [ class "btn-secondary timeline-selection-reset", onClick ResetTimelineHistogramSelection ] [ text "Reset range" ]
                ]

        Nothing ->
            text ""


viewEmptyTimelineSelection : Maybe TimelineHistogramSelection -> Html Msg
viewEmptyTimelineSelection maybeSelection =
    case maybeSelection of
        Just selection ->
            div [ class "empty-state timeline-state" ]
                [ h3 [] [ text "No timeline events in this bucket" ]
                , p [] [ text ("No loaded events match " ++ selection.label ++ " after applying the current filters. Reset the range or broaden the entity/lifecycle filters.") ]
                , button [ class "btn-secondary", onClick ResetTimelineHistogramSelection ] [ text "Reset range" ]
                ]

        Nothing ->
            div [ class "empty-state timeline-state" ]
                [ h3 [] [ text "No loaded timeline events match these filters" ]
                , p [] [ text "Filters apply to the latest loaded events. Adjust them to broaden this timeline view." ]
                ]


timelineMoreNote : Maybe TimelineHistogramSelection -> String
timelineMoreNote maybeSelection =
    case maybeSelection of
        Just _ ->
            "Showing and filtering the latest 50 timeline events in the selected bucket."

        Nothing ->
            "Showing and filtering the latest 50 timeline events."


viewTimelineFilters : TimelineModel -> Html Msg
viewTimelineFilters timeline =
    div [ class "filter-bar timeline-filter-bar" ]
        [ div [ class "filter-group" ]
            [ span [ class "filter-label", title "Filters apply to the latest loaded timeline events." ] [ text "Entity:" ]
            , viewFilterPill "All" (timeline.entityFilter == TimelineAllEntities) (SetTimelineEntityFilter TimelineAllEntities)
            , viewFilterPill "Projects" (timeline.entityFilter == TimelineProjectsOnly) (SetTimelineEntityFilter TimelineProjectsOnly)
            , viewFilterPill "Tasks" (timeline.entityFilter == TimelineTasksOnly) (SetTimelineEntityFilter TimelineTasksOnly)
            , viewFilterPill "Subtasks" (timeline.entityFilter == TimelineSubtasksOnly) (SetTimelineEntityFilter TimelineSubtasksOnly)
            ]
        , div [ class "filter-group" ]
            [ span [ class "filter-label", title "Filters apply to the latest loaded timeline events." ] [ text "Lifecycle:" ]
            , viewFilterPill "All" (timeline.eventFilter == TimelineAllEvents) (SetTimelineEventFilter TimelineAllEvents)
            , viewFilterPill "Created" (timeline.eventFilter == TimelineCreatedEvents) (SetTimelineEventFilter TimelineCreatedEvents)
            , viewFilterPill "Completed" (timeline.eventFilter == TimelineCompletedEvents) (SetTimelineEventFilter TimelineCompletedEvents)
            , viewFilterPill "Archived" (timeline.eventFilter == TimelineArchivedEvents) (SetTimelineEventFilter TimelineArchivedEvents)
            , viewFilterPill "Cancelled" (timeline.eventFilter == TimelineCancelledEvents) (SetTimelineEventFilter TimelineCancelledEvents)
            ]
        ]


viewFilterPill : String -> Bool -> Msg -> Html Msg
viewFilterPill label active msg =
    button
        [ class
            (if active then
                "filter-pill filter-pill-active"

             else
                "filter-pill"
            )
        , onClick msg
        ]
        [ text label ]


timelineBucketSegments : Int -> Api.WorkspaceTimelineBucket -> List (Html Msg)
timelineBucketSegments maxTotal bucket =
    [ ( "timeline-histogram-created", bucket.totals.created, "Created" )
    , ( "timeline-histogram-completed", bucket.totals.completed, "Completed" )
    , ( "timeline-histogram-cancelled", bucket.totals.cancelled, "Cancelled" )
    ]
        |> List.filterMap
            (\( className, count, label ) ->
                if count <= 0 then
                    Nothing

                else
                    Just
                        (div
                            [ class ("timeline-histogram-segment " ++ className)
                            , style "height" (histogramSegmentHeight maxTotal count)
                            , title (label ++ ": " ++ String.fromInt count)
                            ]
                            []
                        )
            )


histogramSegmentHeight : Int -> Int -> String
histogramSegmentHeight maxTotal count =
    let
        percent =
            (toFloat count / toFloat (max 1 maxTotal)) * 100
    in
    String.fromInt (max 4 (round percent)) ++ "%"


timelineBucketTotal : Api.WorkspaceTimelineBucket -> Int
timelineBucketTotal bucket =
    timelineBucketEntityTotal bucket.totals


timelineBucketEntityTotal : Api.TimelineBucketCounts -> Int
timelineBucketEntityTotal counts =
    counts.created + counts.completed + counts.cancelled


timelineBucketTooltip : Api.WorkspaceTimelineBucket -> String
timelineBucketTooltip bucket =
    String.join "\n"
        [ bucket.label ++ " (" ++ bucket.bucketStart ++ " to " ++ bucket.bucketEnd ++ ")"
        , "Created: " ++ String.fromInt bucket.totals.created
        , "Completed: " ++ String.fromInt bucket.totals.completed
        , "Cancelled: " ++ String.fromInt bucket.totals.cancelled
        , "Projects: " ++ String.fromInt (timelineBucketEntityTotal bucket.counts.project)
        , "Subprojects: " ++ String.fromInt (timelineBucketEntityTotal bucket.counts.subproject)
        , "Tasks: " ++ String.fromInt (timelineBucketEntityTotal bucket.counts.task)
        , "Subtasks: " ++ String.fromInt (timelineBucketEntityTotal bucket.counts.subtask)
        ]


sortTimelineEvents : List Api.WorkspaceTimelineEvent -> List Api.WorkspaceTimelineEvent
sortTimelineEvents events =
    List.sortWith compareTimelineEvents events


compareTimelineEvents : Api.WorkspaceTimelineEvent -> Api.WorkspaceTimelineEvent -> Order
compareTimelineEvents left right =
    case compare right.occurredAt left.occurredAt of
        EQ ->
            compare (timelineTieBreaker right) (timelineTieBreaker left)

        order ->
            order


timelineTieBreaker : Api.WorkspaceTimelineEvent -> String
timelineTieBreaker event =
    event.sourceAuditId |> Maybe.withDefault event.id


filterTimelineEvents : TimelineEntityFilter -> TimelineEventFilter -> List Api.WorkspaceTimelineEvent -> List Api.WorkspaceTimelineEvent
filterTimelineEvents entityFilter eventFilter events =
    filterTimelineEventsForSelection Nothing entityFilter eventFilter events


filterTimelineEventsForSelection : Maybe TimelineHistogramSelection -> TimelineEntityFilter -> TimelineEventFilter -> List Api.WorkspaceTimelineEvent -> List Api.WorkspaceTimelineEvent
filterTimelineEventsForSelection maybeSelection entityFilter eventFilter events =
    events
        |> List.filter (eventInTimelineSelection maybeSelection)
        |> List.filter (timelineEntityMatches entityFilter)
        |> List.filter (timelineEventMatches eventFilter)


eventInTimelineSelection : Maybe TimelineHistogramSelection -> Api.WorkspaceTimelineEvent -> Bool
eventInTimelineSelection maybeSelection event =
    case maybeSelection of
        Just selection ->
            let
                occurredAt =
                    comparableTimelineInstant event.occurredAt

                since =
                    comparableTimelineInstant selection.since

                until =
                    comparableTimelineInstant selection.until
            in
            occurredAt >= since && occurredAt < until

        Nothing ->
            True


comparableTimelineInstant : String -> String
comparableTimelineInstant timestamp =
    let
        withoutZone =
            if String.endsWith "Z" timestamp then
                String.dropRight 1 timestamp

            else
                timestamp
    in
    case String.split "." withoutZone of
        base :: fraction :: _ ->
            base ++ "." ++ String.padRight 6 '0' (String.left 6 fraction) ++ "Z"

        base :: [] ->
            base ++ ".000000Z"

        [] ->
            timestamp


timelineEntityMatches : TimelineEntityFilter -> Api.WorkspaceTimelineEvent -> Bool
timelineEntityMatches entityFilter event =
    case entityFilter of
        TimelineAllEntities ->
            True

        TimelineProjectsOnly ->
            event.entityType == "project"

        TimelineTasksOnly ->
            event.entityType == "task"

        TimelineSubtasksOnly ->
            event.entityType == "subtask"


timelineEventMatches : TimelineEventFilter -> Api.WorkspaceTimelineEvent -> Bool
timelineEventMatches eventFilter event =
    case eventFilter of
        TimelineAllEvents ->
            True

        TimelineCreatedEvents ->
            String.contains "created" event.eventType

        TimelineCompletedEvents ->
            String.contains "completed" event.eventType

        TimelineArchivedEvents ->
            String.contains "archived" event.eventType

        TimelineCancelledEvents ->
            String.contains "cancelled" event.eventType


groupTimelineEvents : List Api.WorkspaceTimelineEvent -> List ( String, List Api.WorkspaceTimelineEvent )
groupTimelineEvents events =
    case events of
        [] ->
            []

        first :: rest ->
            let
                key =
                    timelineDateKey first

                ( sameDay, remaining ) =
                    spanTimelineGroup key rest
            in
            ( key, first :: sameDay ) :: groupTimelineEvents remaining


spanTimelineGroup : String -> List Api.WorkspaceTimelineEvent -> ( List Api.WorkspaceTimelineEvent, List Api.WorkspaceTimelineEvent )
spanTimelineGroup key events =
    case events of
        [] ->
            ( [], [] )

        first :: rest ->
            if timelineDateKey first == key then
                let
                    ( matching, remaining ) =
                        spanTimelineGroup key rest
                in
                ( first :: matching, remaining )

            else
                ( [], events )


timelineDateKey : Api.WorkspaceTimelineEvent -> String
timelineDateKey event =
    let
        datePart =
            String.left 10 event.occurredAt
    in
    if datePart == "" then
        "Unknown date"

    else
        datePart


viewTimelineGroup : ( String, List Api.WorkspaceTimelineEvent ) -> Html Msg
viewTimelineGroup ( dateLabel, events ) =
    div [ class "timeline-date-group" ]
        [ div [ class "timeline-date-heading" ] [ text dateLabel ]
        , div [ class "timeline-date-events" ]
            (List.map viewTimelineEvent events)
        ]


viewTimelineEvent : Api.WorkspaceTimelineEvent -> Html Msg
viewTimelineEvent event =
    button
        [ class ("timeline-event-card " ++ timelineEventToneClass event.eventType ++ " timeline-entity-" ++ event.entityType)
        , onClick (NavigateToTimelineEntity event)
        , title ("Open " ++ String.toLower (entityTypeLabel event.entityType))
        ]
        [ div [ class "timeline-event-rail" ]
            [ span [ class "timeline-event-marker" ] [ text (timelineEventIcon event.eventType) ] ]
        , div [ class "timeline-event-content" ]
            [ div [ class "timeline-event-topline" ]
                [ span [ class "timeline-event-label" ] [ text (timelineEventLabel event.eventType) ]
                , span [ class ("timeline-entity-pill timeline-entity-pill-" ++ event.entityType) ] [ text (entityTypeLabel event.entityType) ]
                , span [ class "timeline-event-time" ] [ text (formatDate event.occurredAt) ]
                ]
            , div [ class "timeline-event-title-row" ]
                [ span [ class "timeline-event-title" ] [ text event.title ]
                , span [ class "timeline-event-action" ] [ text ("Open " ++ entityTypeLabel event.entityType) ]
                ]
            , viewStatusTransition event.statusTransition
            , viewTimelineContext event
            ]
        ]


viewStatusTransition : Maybe Api.TimelineStatusTransition -> Html Msg
viewStatusTransition maybeTransition =
    case maybeTransition of
        Just transition ->
            div [ class "timeline-status-transition" ]
                [ span [ class "timeline-status-label" ] [ text "Status" ]
                , span [ class "timeline-status-from" ] [ text (formatTimelineStatus transition.from) ]
                , span [ class "timeline-status-arrow" ] [ text "→" ]
                , span [ class "timeline-status-to" ] [ text (formatTimelineStatus transition.to) ]
                ]

        Nothing ->
            text ""


viewTimelineContext : Api.WorkspaceTimelineEvent -> Html Msg
viewTimelineContext event =
    let
        contextItems =
            timelineContextSegments event
    in
    if List.isEmpty contextItems then
        text ""

    else
        div [ class "timeline-event-context" ]
            (List.map viewTimelineContextChip contextItems)


viewTimelineContextChip : String -> Html Msg
viewTimelineContextChip label =
    span [ class "timeline-context-chip" ] [ text label ]


timelineContextSegments : Api.WorkspaceTimelineEvent -> List String
timelineContextSegments event =
    List.filterMap identity
        [ eventActorLabel event.actor
        , event.project |> Maybe.map (\project -> "Project: " ++ project.name)
        , event.parentTask |> Maybe.map (\task -> "Parent: " ++ task.title)
        ]


eventActorLabel : Maybe Api.TimelineActor -> Maybe String
eventActorLabel maybeActor =
    maybeActor
        |> Maybe.andThen
            (\actor ->
                case ( actor.actorLabel, actor.actorType ) of
                    ( Just label, _ ) ->
                        Just ("By " ++ label)

                    ( Nothing, Just actorType ) ->
                        Just ("By " ++ actorType)

                    _ ->
                        Nothing
            )


timelineStatusSummary : Maybe Api.TimelineStatusTransition -> Maybe String
timelineStatusSummary maybeTransition =
    maybeTransition
        |> Maybe.map (\transition -> formatTimelineStatus transition.from ++ " → " ++ formatTimelineStatus transition.to)


formatTimelineStatus : String -> String
formatTimelineStatus status =
    status
        |> String.replace "_" " "
        |> capitalizeFirst


capitalizeFirst : String -> String
capitalizeFirst value =
    case String.uncons value of
        Just ( first, rest ) ->
            String.fromChar (Char.toUpper first) ++ rest

        Nothing ->
            ""


entityTypeLabel : String -> String
entityTypeLabel entityType =
    case entityType of
        "project" ->
            "Project"

        "subtask" ->
            "Subtask"

        "task" ->
            "Task"

        _ ->
            entityType


timelineEventLabel : String -> String
timelineEventLabel eventType =
    case eventType of
        "project_created" ->
            "Project created"

        "project_completed" ->
            "Project completed"

        "project_archived" ->
            "Project archived"

        "task_created" ->
            "Task created"

        "subtask_created" ->
            "Subtask created"

        "task_completed" ->
            "Task completed"

        "subtask_completed" ->
            "Subtask completed"

        "task_cancelled" ->
            "Task cancelled"

        "subtask_cancelled" ->
            "Subtask cancelled"

        _ ->
            eventType


timelineEventToneClass : String -> String
timelineEventToneClass eventType =
    if String.contains "created" eventType then
        "timeline-event-created"

    else if String.contains "archived" eventType then
        "timeline-event-archived"

    else if String.contains "cancelled" eventType then
        "timeline-event-cancelled"

    else if String.contains "completed" eventType then
        "timeline-event-completed"

    else
        "timeline-event-neutral"


timelineEventIcon : String -> String
timelineEventIcon eventType =
    if String.contains "created" eventType then
        "+"

    else if String.contains "archived" eventType then
        "A"

    else if String.contains "cancelled" eventType then
        "×"

    else
        "✓"
