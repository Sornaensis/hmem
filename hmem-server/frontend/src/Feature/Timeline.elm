module Feature.Timeline exposing (chartCanvasWidth, chartX, chartXWithWidth, chartY, chartTickValues, clampTimelinePointFocus, ensureLoaded, eventInTimelineSelection, filterTimelineEvents, filterTimelineEventsForSelection, groupTimelineEvents, init, lineChartMaximum, lineChartRenderDomain, markDirty, pointMarkerOffset, reset, sortTimelineEvents, timelineDateKey, timelineDebounceMs, timelineErrorIsBlocking, timelineEventLabel, timelineEventToneClass, timelineEventsRequest, timelineHistogramAcceptsResponse, timelinePath, timelinePointFocusKey, timelinePointId, timelinePointNextIndex, timelineRefreshDispatchAllowed, timelineRefreshPlan, timelineResponseIsCurrent, timelineStatusSummary, toggleTimelineChartSeries, update, viewTimelineHistogram, viewWorkspaceTimelinePanel)

import Api
import Char
import Dict
import Feature.Focus as Focus
import Helpers exposing (buildFragment, focusElement, formatDate, pushUrl)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, disabled, title, type_, value)
import Html.Events exposing (onClick, onInput, preventDefaultOn)
import Json.Decode as Decode
import Process
import String
import Svg
import Svg.Attributes as SA
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
    , eventsActiveIdentity = Nothing
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
    , histogramActiveIdentity = Nothing
    , histogramLoadedRequest = Nothing
    , histogramSelectedBucket = Nothing
    , chartSeries = { projects = True, tasks = True, subtasks = True, observations = True }
    , chartPointFocus = Dict.empty
    , refreshGeneration = 0
    , refreshTimerGeneration = Nothing
    , refreshDirty = False
    , refreshEpoch = 0
    , nextRequestIdentity = 1
    }


reset : Int -> TimelineModel
reset refreshEpoch =
    { init | refreshEpoch = refreshEpoch }


ensureLoaded : String -> String -> Int -> TimelineModel -> ( TimelineModel, Cmd Msg )
ensureLoaded apiUrl wsId sessionEpoch timeline =
    let
        currentTimeline =
            synchronizeRefreshEpoch sessionEpoch timeline
    in
    if currentTimeline.refreshDirty && currentTimeline.refreshTimerGeneration == Nothing then
        refreshNow apiUrl wsId currentTimeline

    else
        let
            ( eventsTimeline, eventCmd ) =
                ensureEventsLoaded apiUrl wsId currentTimeline

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
    startEventsFetchWithStaleData False apiUrl request timeline


startEventsFetchWithStaleData : Bool -> String -> TimelineEventsRequest -> TimelineModel -> ( TimelineModel, Cmd Msg )
startEventsFetchWithStaleData preserveData apiUrl request timeline =
    let
        identity =
            { requestId = timeline.nextRequestIdentity, refreshGeneration = timeline.refreshGeneration, refreshEpoch = timeline.refreshEpoch }
    in
    ( { timeline
        | loading = True
        , loadingWorkspaceId = Just request.workspaceId
        , error = Nothing
        , events =
            if preserveData then
                timeline.events

            else
                []
        , hasMore =
            if preserveData then
                timeline.hasMore

            else
                False
        , loadedWorkspaceId =
            if preserveData then
                timeline.loadedWorkspaceId

            else
                Nothing
        , eventsActiveRequest = Just request
        , eventsActiveIdentity = Just identity
        , eventsLoadedRequest =
            if preserveData then
                timeline.eventsLoadedRequest

            else
                Nothing
        , nextRequestIdentity = timeline.nextRequestIdentity + 1
      }
    , Api.fetchWorkspaceTimelineRange apiUrl request.workspaceId request.since request.until (GotWorkspaceTimeline identity request)
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
    startHistogramFetchWithStaleData False apiUrl request timeline


startHistogramFetchWithStaleData : Bool -> String -> TimelineHistogramRequest -> TimelineModel -> ( TimelineModel, Cmd Msg )
startHistogramFetchWithStaleData preserveData apiUrl request timeline =
    let
        identity =
            { requestId = timeline.nextRequestIdentity, refreshGeneration = timeline.refreshGeneration, refreshEpoch = timeline.refreshEpoch }
    in
    ( { timeline
        | histogramLoading = True
        , histogramError = Nothing
        , histogramBuckets =
            if preserveData then
                timeline.histogramBuckets

            else
                []
        , histogramClockWorkspaceId = Nothing
        , histogramActiveRequest = Just request
        , histogramActiveIdentity = Just identity
        , histogramLoadedRequest =
            if preserveData then
                timeline.histogramLoadedRequest

            else
                Nothing
        , nextRequestIdentity = timeline.nextRequestIdentity + 1
      }
    , Api.fetchWorkspaceTimelineBuckets apiUrl request.workspaceId request.since request.until request.bucket (GotWorkspaceTimelineBuckets identity request)
    )


refreshNow : String -> String -> TimelineModel -> ( TimelineModel, Cmd Msg )
refreshNow apiUrl wsId timeline =
    let
        readyTimeline =
            { timeline | refreshDirty = False, refreshTimerGeneration = Nothing }

        requestPlan =
            timelineRefreshPlan wsId readyTimeline

        ( eventsTimeline, eventCmd ) =
            startEventsFetchWithStaleData True apiUrl requestPlan.events readyTimeline

        ( histogramTimeline, histogramCmd ) =
            case requestPlan.histogram of
                Just histogramRequest ->
                    startHistogramFetchWithStaleData True apiUrl histogramRequest eventsTimeline

                Nothing ->
                    ensureHistogramLoaded apiUrl wsId eventsTimeline
    in
    ( histogramTimeline, Cmd.batch [ eventCmd, histogramCmd ] )


timelineRefreshPlan : String -> TimelineModel -> { events : TimelineEventsRequest, histogram : Maybe TimelineHistogramRequest }
timelineRefreshPlan wsId timeline =
    { events = timelineEventsRequest wsId timeline
    , histogram = timelineHistogramRequest wsId timeline
    }


timelineErrorIsBlocking : TimelineModel -> Bool
timelineErrorIsBlocking timeline =
    timeline.error /= Nothing && timeline.eventsLoadedRequest == Nothing


timelineDebounceMs : Float
timelineDebounceMs =
    250


timelineResponseIsCurrent : TimelineRequestIdentity -> Maybe TimelineRequestIdentity -> Int -> Int -> Bool
timelineResponseIsCurrent identity activeIdentity refreshGeneration refreshEpoch =
    activeIdentity == Just identity
        && identity.refreshGeneration == refreshGeneration
        && identity.refreshEpoch == refreshEpoch


timelineRefreshDispatchAllowed : Bool -> Bool -> Int -> Int -> Maybe Int -> Int -> Int -> Bool
timelineRefreshDispatchAllowed isTimelineActive isDirty refreshGeneration refreshEpoch timerGeneration firedGeneration firedEpoch =
    isTimelineActive
        && isDirty
        && refreshGeneration == firedGeneration
        && refreshEpoch == firedEpoch
        && timerGeneration == Just firedGeneration


synchronizeRefreshEpoch : Int -> TimelineModel -> TimelineModel
synchronizeRefreshEpoch sessionEpoch timeline =
    { timeline | refreshEpoch = max timeline.refreshEpoch sessionEpoch }


markDirty : Model -> ( Model, Cmd Msg )
markDirty model =
    case model.selectedWorkspaceId of
        Nothing ->
            ( model, Cmd.none )

        Just workspaceId ->
            let
                nextGeneration =
                    model.timeline.refreshGeneration + 1

                currentTimeline =
                    model.timeline

                epochTimeline =
                    synchronizeRefreshEpoch model.sessionRequestEpoch currentTimeline

                dirtyTimeline =
                    { epochTimeline
                        | refreshDirty = True
                        , refreshGeneration = nextGeneration
                    }
            in
            if model.activeTab == TimelineTab then
                ( { model | timeline = { dirtyTimeline | refreshTimerGeneration = Just nextGeneration } }
                , Process.sleep timelineDebounceMs
                    |> Task.perform (\_ -> RefreshTimelineAfterDebounce workspaceId nextGeneration dirtyTimeline.refreshEpoch)
                )

            else
                ( { model | timeline = { dirtyTimeline | refreshTimerGeneration = Nothing } }, Cmd.none )


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
                            { currentSearch
                                | unifiedResults = Nothing
                                , isSearching = False
                                , searchError = Nothing
                                , activeRequestQuery = Nothing
                                , activeRequest = Nothing
                            }
                    in
                    ( { model | activeTab = targetTab, focus = updatedFocus, search = updatedSearch }
                    , pushUrl model.key ("/workspace/" ++ wsId ++ "#" ++ buildFragment targetTab (Just focusEntry) Nothing)
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

        ToggleTimelineChartSeries series ->
            let
                currentTimeline =
                    model.timeline
            in
            ( { model | timeline = { currentTimeline | chartSeries = toggleTimelineChartSeries series currentTimeline.chartSeries } }, Cmd.none )

        FocusTimelineChartPoint action series index ->
            let
                currentTimeline =
                    model.timeline

                targetId =
                    timelinePointId action series index
            in
            ( { model | timeline = { currentTimeline | chartPointFocus = Dict.insert (timelinePointFocusKey action series) index currentTimeline.chartPointFocus } }
            , focusElement targetId
            )

        GotWorkspaceTimeline identity request result ->
            if model.selectedWorkspaceId /= Just request.workspaceId
                || model.timeline.eventsActiveRequest /= Just request
                || not (timelineResponseIsCurrent identity model.timeline.eventsActiveIdentity model.timeline.refreshGeneration model.timeline.refreshEpoch) then
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
                                    , eventsActiveIdentity = Nothing
                                    , eventsLoadedRequest = Just request
                                }
                          }
                        , Cmd.none
                        )

                    Err _ ->
                        let
                            hasLastGoodData =
                                currentTimeline.eventsLoadedRequest /= Nothing
                        in
                        ( { model
                            | timeline =
                                { currentTimeline
                                    | loading = False
                                    , loadingWorkspaceId = Nothing
                                    , error = Just "Failed to load timeline events."
                                    , loadedWorkspaceId =
                                        if hasLastGoodData then
                                            currentTimeline.loadedWorkspaceId

                                        else
                                            Nothing
                                    , eventsActiveRequest = Nothing
                                    , eventsActiveIdentity = Nothing
                                    , eventsLoadedRequest =
                                        if hasLastGoodData then
                                            currentTimeline.eventsLoadedRequest

                                        else
                                            Nothing
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

        GotWorkspaceTimelineBuckets identity request result ->
            if model.selectedWorkspaceId /= Just request.workspaceId
                || not (timelineHistogramAcceptsResponse request model.timeline)
                || not (timelineResponseIsCurrent identity model.timeline.histogramActiveIdentity model.timeline.refreshGeneration model.timeline.refreshEpoch) then
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
                                    , histogramActiveIdentity = Nothing
                                    , histogramLoadedRequest = Just request
                                    , chartPointFocus = clampTimelinePointFocus (List.length response.buckets) currentTimeline.chartPointFocus
                                }
                          }
                        , Cmd.none
                        )

                    Err _ ->
                        let
                            hasLastGoodData =
                                currentTimeline.histogramLoadedRequest /= Nothing
                        in
                        ( { model
                            | timeline =
                                { currentTimeline
                                    | histogramBuckets =
                                        if hasLastGoodData then
                                            currentTimeline.histogramBuckets

                                        else
                                            []
                                    , histogramLoading = False
                                    , histogramError = Just "Failed to load timeline histogram."
                                    , histogramClockWorkspaceId = Nothing
                                    , histogramActiveRequest = Nothing
                                    , histogramActiveIdentity = Nothing
                                    , histogramLoadedRequest =
                                        if hasLastGoodData then
                                            currentTimeline.histogramLoadedRequest

                                        else
                                            Nothing
                                }
                          }
                        , Cmd.none
                        )

        RefreshTimelineAfterDebounce workspaceId generation refreshEpoch ->
            if model.selectedWorkspaceId == Just workspaceId
                && model.timeline.refreshDirty
                && model.timeline.refreshGeneration == generation
                && model.timeline.refreshEpoch == refreshEpoch
                && model.timeline.refreshTimerGeneration == Just generation then
                if timelineRefreshDispatchAllowed
                    (model.activeTab == TimelineTab)
                    model.timeline.refreshDirty
                    model.timeline.refreshGeneration
                    model.timeline.refreshEpoch
                    model.timeline.refreshTimerGeneration
                    generation
                    refreshEpoch then
                    let
                        ( timeline, cmd ) =
                            refreshNow model.flags.apiUrl workspaceId model.timeline
                    in
                    ( { model | timeline = timeline }, cmd )

                else
                    let
                        currentTimeline =
                            model.timeline
                    in
                    ( { model | timeline = { currentTimeline | refreshTimerGeneration = Nothing } }, Cmd.none )

            else
                ( model, Cmd.none )

        RetryTimelineRefresh ->
            case model.selectedWorkspaceId of
                Just workspaceId ->
                    let
                        currentTimeline =
                            model.timeline

                        ( timeline, cmd ) =
                            refreshNow model.flags.apiUrl workspaceId
                                { currentTimeline
                                    | refreshDirty = True
                                    , refreshTimerGeneration = Nothing
                                    , refreshEpoch = max currentTimeline.refreshEpoch model.sessionRequestEpoch
                                }
                    in
                    ( { model | timeline = timeline }, cmd )

                Nothing ->
                    ( model, Cmd.none )

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
                currentTimeline =
                    synchronizeRefreshEpoch model.sessionRequestEpoch timeline

                request =
                    timelineEventsRequest wsId currentTimeline

                ( nextTimeline, cmd ) =
                    startEventsFetch model.flags.apiUrl request currentTimeline
            in
            ( { model | timeline = nextTimeline }, cmd )

        Nothing ->
            ( { model | timeline = timeline }, Cmd.none )


updateHistogramControls : Model -> (TimelineModel -> TimelineModel) -> ( Model, Cmd Msg )
updateHistogramControls model updateTimeline =
    let
        currentTimeline =
            synchronizeRefreshEpoch model.sessionRequestEpoch model.timeline

        updatedTimeline =
            updateTimeline currentTimeline

        selectionChanged =
            currentTimeline.histogramSelectedBucket /= updatedTimeline.histogramSelectedBucket

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
                            , histogramActiveIdentity = Nothing
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
    div [ class "timeline-chart-panel" ]
        [ div [ class "timeline-graph-header" ]
            [ div []
                [ h4 [] [ text "Lifecycle activity" ]
                , p [] [ text "Create, Complete, and Delete counts by ascending UTC bucket. Select a point or table row to filter the event cards below." ]
                ]
            ]
        , viewTimelineHistogramControls timeline
        , viewTimelineChartToggles timeline.chartSeries
        , viewTimelineHistogramContent timeline
        ]


viewTimelineHistogramControls : TimelineModel -> Html Msg
viewTimelineHistogramControls timeline =
    div [ class "timeline-graph-controls" ]
        [ label [ class "timeline-graph-control" ]
            [ span [] [ text "Since" ]
            , input
                [ type_ "date"
                , value timeline.histogramSince
                , onInput SetTimelineHistogramSince
                , disabled timeline.histogramLoading
                ]
                []
            ]
        , label [ class "timeline-graph-control" ]
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
        , label [ class "timeline-graph-control" ]
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


viewTimelineHistogramContent : TimelineModel -> Html Msg
viewTimelineHistogramContent timeline =
    let
        hasBuckets =
            not (List.isEmpty timeline.histogramBuckets)
    in
    div [ class "timeline-graph-content" ]
        [ if timeline.histogramLoading && not hasBuckets then
            div [ class "timeline-graph-state" ] [ text "Loading lifecycle graphs..." ]

          else
            text ""
        , case timeline.histogramError of
            Just message ->
                div [ class "timeline-graph-state timeline-graph-error" ] [ text message ]

            Nothing ->
                text ""
        , if (not timeline.histogramLoading) && timeline.histogramError == Nothing && not hasBuckets then
            div [ class "timeline-graph-state" ] [ text "No lifecycle activity in this date range." ]

          else if hasBuckets then
            viewTimelineHistogramChart timeline

          else
            text ""
        ]


viewTimelineHistogramChart : TimelineModel -> Html Msg
viewTimelineHistogramChart timeline =
    div [ class "timeline-line-graphs", title "Timeline line graphs by lifecycle action and entity kind" ]
        [ viewTimelineLineChart timeline "Create" "created"
        , viewTimelineLineChart timeline "Complete" "completed"
        , viewTimelineLineChart timeline "Delete" "deleted"
        ]


viewTimelineChartToggles : TimelineChartSeries -> Html Msg
viewTimelineChartToggles series =
    div [ class "timeline-chart-toggles", attribute "aria-label" "Visible entity series" ]
        [ viewTimelineChartToggle "projects" "Projects" series.projects
        , viewTimelineChartToggle "tasks" "Tasks" series.tasks
        , viewTimelineChartToggle "subtasks" "Subtasks" series.subtasks
        , viewTimelineChartToggle "observations" "Observations" series.observations
        ]


viewTimelineChartToggle : String -> String -> Bool -> Html Msg
viewTimelineChartToggle key label visible =
    button
        [ class ("timeline-series-toggle timeline-series-" ++ key)
        , attribute "aria-pressed" (if visible then "true" else "false")
        , onClick (ToggleTimelineChartSeries key)
        ]
        [ span [ class "timeline-series-marker" ] []
        , text label
        ]


toggleTimelineChartSeries : String -> TimelineChartSeries -> TimelineChartSeries
toggleTimelineChartSeries key series =
    case key of
        "projects" ->
            { series | projects = not series.projects }

        "tasks" ->
            { series | tasks = not series.tasks }

        "subtasks" ->
            { series | subtasks = not series.subtasks }

        "observations" ->
            { series | observations = not series.observations }

        _ ->
            series


viewTimelineLineChart : TimelineModel -> String -> String -> Html Msg
viewTimelineLineChart timeline actionLabel action =
    let
        visibleSeries =
            timelineChartSeries timeline.chartSeries action timeline.histogramBuckets

        actualMaximum =
            lineChartMaximum visibleSeries

        renderDomain =
            lineChartRenderDomain actualMaximum

        bucketCount =
            List.length timeline.histogramBuckets

        canvasWidth =
            chartCanvasWidth bucketCount
    in
    section [ class "timeline-line-chart-panel" ]
        [ h5 [] [ text actionLabel ]
        , p [ class "timeline-line-chart-summary" ] [ text (actionLabel ++ " maximum: " ++ String.fromInt actualMaximum) ]
        , div [ class "timeline-svg-scroll" ]
            [ Svg.svg
                [ SA.viewBox ("0 0 " ++ String.fromInt canvasWidth ++ " 280")
                , SA.width (String.fromInt canvasWidth)
                , SA.class "timeline-line-chart"
                , attribute "aria-labelledby" ("timeline-chart-" ++ action ++ "-title timeline-chart-" ++ action ++ "-description")
                ]
                ([ Svg.title [ SA.id ("timeline-chart-" ++ action ++ "-title") ] [ Svg.text (actionLabel ++ " lifecycle counts by UTC bucket") ]
                 , Svg.desc [ SA.id ("timeline-chart-" ++ action ++ "-description") ] [ Svg.text "Each interactive point selects its half-open UTC bucket for the event cards." ]
                 ]
                    ++ timelineChartGrid canvasWidth actualMaximum renderDomain
                    ++ timelineXLabels canvasWidth timeline.histogramBuckets
                    ++ List.concatMap (viewTimelineSeriesSvg timeline.chartPointFocus timeline.histogramSelectedBucket actionLabel action renderDomain canvasWidth bucketCount) visibleSeries
                )
            ]
        , viewTimelineValueTable timeline.histogramSelectedBucket actionLabel action timeline.chartSeries timeline.histogramBuckets
        ]


lineChartMaximum : List TimelineSeriesDefinition -> Int
lineChartMaximum series =
    series
        |> List.concatMap (\definition -> List.map .count definition.values)
        |> List.maximum
        |> Maybe.withDefault 0


lineChartRenderDomain : Int -> Int
lineChartRenderDomain actualMaximum =
    max 1 actualMaximum


type alias TimelineSeriesDefinition =
    { key : String
    , label : String
    , values : List TimelineSeriesPoint
    }


type alias TimelineSeriesPoint =
    { label : String
    , since : String
    , until : String
    , count : Int
    }


timelineChartSeries : TimelineChartSeries -> String -> List Api.WorkspaceTimelineBucket -> List TimelineSeriesDefinition
timelineChartSeries visibility action buckets =
    [ { visible = visibility.projects, key = "projects", label = "Projects", selectCounts = \bucket -> bucket.series.project }
    , { visible = visibility.tasks, key = "tasks", label = "Tasks", selectCounts = \bucket -> bucket.series.task }
    , { visible = visibility.subtasks, key = "subtasks", label = "Subtasks", selectCounts = \bucket -> bucket.series.subtask }
    , { visible = visibility.observations, key = "observations", label = "Observations", selectCounts = \bucket -> bucket.series.observation }
    ]
        |> List.filterMap
            (\definition ->
                if definition.visible then
                    Just
                        { key = definition.key
                        , label = definition.label
                        , values = List.map (\bucket -> { label = bucket.label, since = bucket.bucketStart, until = bucket.bucketEnd, count = timelineActionValue action (definition.selectCounts bucket) }) buckets
                        }

                else
                    Nothing
            )


timelineActionValue : String -> Api.TimelineBucketActionCounts -> Int
timelineActionValue action counts =
    case action of
        "created" ->
            counts.created

        "completed" ->
            counts.completed

        "deleted" ->
            counts.deleted

        _ ->
            0


timelineChartGrid : Int -> Int -> Int -> List (Svg.Svg Msg)
timelineChartGrid canvasWidth actualMaximum renderDomain =
    chartTickValues actualMaximum
        |> List.concatMap
            (\tick ->
                let
                    y = chartY renderDomain (toFloat tick)
                in
                [ Svg.line [ SA.x1 "44", SA.x2 (floatString (toFloat canvasWidth - 20)), SA.y1 (floatString y), SA.y2 (floatString y), SA.class "timeline-chart-grid" ] []
                , Svg.text_ [ SA.x "38", SA.y (floatString (y + 4)), SA.class "timeline-chart-axis" ] [ Svg.text (String.fromInt tick) ]
                ]
            )


chartTickValues : Int -> List Int
chartTickValues actualMaximum =
    if actualMaximum <= 3 then
        List.range 0 actualMaximum

    else
        let
            step =
                ceiling (toFloat actualMaximum / 4)
        in
        [ 0, step, step * 2, step * 3, actualMaximum ]
            |> List.filter (\tick -> tick <= actualMaximum)
            |> uniqueInts


uniqueInts : List Int -> List Int
uniqueInts values =
    List.foldl
        (\value collected ->
            if List.member value collected then
                collected

            else
                collected ++ [ value ]
        )
        []
        values


timelineXLabels : Int -> List Api.WorkspaceTimelineBucket -> List (Svg.Svg Msg)
timelineXLabels canvasWidth buckets =
    let
        bucketCount =
            List.length buckets

        labelStep =
            max 1 ((bucketCount + 7) // 8)
    in
    buckets
        |> List.indexedMap Tuple.pair
        |> List.filter (\( index, _ ) -> modBy labelStep index == 0 || index == bucketCount - 1)
        |> List.map
            (\( index, bucket ) ->
                Svg.text_
                    [ SA.x (floatString (chartXWithWidth canvasWidth bucketCount index))
                    , SA.y "264"
                    , SA.class "timeline-chart-x-axis"
                    ]
                    [ Svg.text bucket.label ]
            )


viewTimelineSeriesSvg : Dict.Dict String Int -> Maybe TimelineHistogramSelection -> String -> String -> Int -> Int -> Int -> TimelineSeriesDefinition -> List (Svg.Svg Msg)
viewTimelineSeriesSvg pointFocus selectedBucket actionLabel action renderDomain canvasWidth bucketCount series =
    let
        points =
            List.indexedMap (\index point -> { x = chartXWithWidth canvasWidth bucketCount index + pointMarkerOffset series.key, y = chartY renderDomain (toFloat point.count), label = point.label, since = point.since, until = point.until, count = point.count }) series.values
    in
    Svg.path [ SA.d (timelinePath points), SA.class ("timeline-line timeline-series-" ++ series.key), SA.fill "none" ] []
        :: List.indexedMap (viewTimelinePoint pointFocus selectedBucket actionLabel action series (List.length points)) points


viewTimelinePoint : Dict.Dict String Int -> Maybe TimelineHistogramSelection -> String -> String -> TimelineSeriesDefinition -> Int -> Int -> { x : Float, y : Float, label : String, since : String, until : String, count : Int } -> Svg.Svg Msg
viewTimelinePoint pointFocus selectedBucket actionLabel action series pointCount index point =
    let
        matchingBucket =
            -- labels are response labels; the table remains the keyboard activation path.
            (selectedBucket |> Maybe.map .label) == Just point.label

        displayX =
            point.x

        pointId =
            timelinePointId action series.key index

        isRovingTarget =
            Dict.get (timelinePointFocusKey action series.key) pointFocus
                |> Maybe.withDefault 0
                |> (==) index
    in
    Svg.g
        [ SA.class ("timeline-point-control timeline-series-" ++ series.key ++ if matchingBucket then " timeline-point-selected" else "")
        , SA.id pointId
        , attribute "role" "button"
        , attribute "tabindex" (if isRovingTarget then "0" else "-1")
        , attribute "aria-pressed" (if matchingBucket then "true" else "false")
        , attribute "aria-label" (actionLabel ++ ", " ++ series.label ++ ", " ++ point.label ++ ", " ++ point.since ++ " to " ++ point.until ++ " exclusive, " ++ String.fromInt point.count ++ ", " ++ if matchingBucket then "selected" else "not selected")
        , onClick (SelectTimelineHistogramBucket point.label point.since point.until)
        , onTimelinePointKey action series.key index pointCount (SelectTimelineHistogramBucket point.label point.since point.until)
        ]
        [ Svg.circle
            [ SA.cx (floatString displayX)
            , SA.cy (floatString point.y)
            , SA.r "11"
            , SA.class "timeline-point-hitarea"
            ]
            []
        , Svg.circle
            [ SA.cx (floatString displayX)
            , SA.cy (floatString point.y)
            , SA.r "6"
            , SA.class "timeline-point"
            ]
            []
        , Svg.title [] [ Svg.text (actionLabel ++ " — " ++ series.label ++ " — " ++ point.label ++ " [" ++ point.since ++ ", " ++ point.until ++ "): " ++ String.fromInt point.count) ]
        ]


pointMarkerOffset : String -> Float
pointMarkerOffset key =
    case key of
        "projects" ->
            -36

        "tasks" ->
            -12

        "subtasks" ->
            12

        "observations" ->
            36

        _ ->
            0


timelinePointFocusKey : String -> String -> String
timelinePointFocusKey action series =
    action ++ "-" ++ series


timelinePointId : String -> String -> Int -> String
timelinePointId action series index =
    "timeline-point-" ++ timelinePointFocusKey action series ++ "-" ++ String.fromInt index


timelinePointNextIndex : String -> Int -> Int -> Int
timelinePointNextIndex key pointCount index =
    case key of
        "ArrowLeft" ->
            max 0 (index - 1)

        "ArrowRight" ->
            min (pointCount - 1) (index + 1)

        "Home" ->
            0

        "End" ->
            max 0 (pointCount - 1)

        "PageUp" ->
            max 0 (index - 30)

        "PageDown" ->
            min (pointCount - 1) (index + 30)

        _ ->
            index


clampTimelinePointFocus : Int -> Dict.Dict String Int -> Dict.Dict String Int
clampTimelinePointFocus bucketCount pointFocus =
    let
        maximum =
            max 0 (bucketCount - 1)
    in
    Dict.map (\_ index -> min maximum (max 0 index)) pointFocus


onTimelinePointKey : String -> String -> Int -> Int -> Msg -> Html.Attribute Msg
onTimelinePointKey action series index pointCount activate =
    preventDefaultOn "keydown"
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    if key == "Enter" || key == " " || key == "Spacebar" then
                        Decode.succeed ( activate, True )

                    else if List.member key [ "ArrowLeft", "ArrowRight", "Home", "End", "PageUp", "PageDown" ] then
                        Decode.succeed ( FocusTimelineChartPoint action series (timelinePointNextIndex key pointCount index), True )

                    else
                        Decode.fail "Not an activation key"
                )
        )


chartX : Int -> Int -> Float
chartX count index =
    chartXWithWidth 720 count index


chartCanvasWidth : Int -> Int
chartCanvasWidth bucketCount =
    max 720 (160 + (max 0 (bucketCount - 1) * 104))


chartXWithWidth : Int -> Int -> Int -> Float
chartXWithWidth canvasWidth count index =
    if count <= 1 then
        toFloat canvasWidth / 2

    else
        80 + (toFloat index * (toFloat canvasWidth - 160) / toFloat (count - 1))


chartY : Int -> Float -> Float
chartY maxValue value =
    236 - (value / toFloat (max 1 maxValue) * 196)


timelinePath : List { x : Float, y : Float, label : String, since : String, until : String, count : Int } -> String
timelinePath points =
    points
        |> List.indexedMap (\index point -> (if index == 0 then "M " else "L ") ++ floatString point.x ++ " " ++ floatString point.y)
        |> String.join " "


floatString : Float -> String
floatString value =
    String.fromFloat value


viewTimelineValueTable : Maybe TimelineHistogramSelection -> String -> String -> TimelineChartSeries -> List Api.WorkspaceTimelineBucket -> Html Msg
viewTimelineValueTable selectedBucket actionLabel action visibility buckets =
    table [ class "timeline-value-table" ]
        [ caption [] [ text (actionLabel ++ " values by UTC bucket") ]
        , thead [] [ tr [] [ th [] [ text "Bucket" ], th [] [ text "Projects" ], th [] [ text "Tasks" ], th [] [ text "Subtasks" ], th [] [ text "Observations" ] ] ]
        , tbody []
            (List.map
                (\bucket ->
                    let
                        selected = selectedBucket == Just { label = bucket.label, since = bucket.bucketStart, until = bucket.bucketEnd }
                        valueFor enabled counts = if enabled then String.fromInt (timelineActionValue action counts) else "Hidden"
                    in
                    tr [ class (if selected then "timeline-value-row-selected" else "") ]
                        [ th [] [ button [ class "timeline-bucket-button", onClick (SelectTimelineHistogramBucket bucket.label bucket.bucketStart bucket.bucketEnd), attribute "aria-label" ("Show events for " ++ bucket.label ++ ", " ++ bucket.bucketStart ++ " through " ++ bucket.bucketEnd) ] [ text bucket.label ] ]
                        , td [] [ text (valueFor visibility.projects bucket.series.project) ]
                        , td [] [ text (valueFor visibility.tasks bucket.series.task) ]
                        , td [] [ text (valueFor visibility.subtasks bucket.series.subtask) ]
                        , td [] [ text (valueFor visibility.observations bucket.series.observation) ]
                        ]
                )
                buckets
            )
        ]


viewTimelineBody : String -> TimelineModel -> Html Msg
viewTimelineBody wsId timeline =
    case timeline.error of
        Just message ->
            if not (timelineErrorIsBlocking timeline) then
                div [ class "timeline-stale-notice" ]
                    [ p [] [ text (message ++ " Showing the last successful timeline.") ]
                    , button [ class "btn-secondary", onClick RetryTimelineRefresh ] [ text "Retry" ]
                    , viewTimelineEvents wsId timeline
                    ]

            else
                div [ class "empty-state timeline-state" ]
                    [ h3 [] [ text "Timeline unavailable" ]
                    , p [] [ text message ]
                    , button [ class "btn-secondary", onClick RetryTimelineRefresh ] [ text "Retry" ]
                    ]

        Nothing ->
            viewTimelineEvents wsId timeline


viewTimelineEvents : String -> TimelineModel -> Html Msg
viewTimelineEvents wsId timeline =
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
