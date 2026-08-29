module TimelineFixture exposing (main)

import Api
import Browser
import Dict
import Feature.Timeline
import Helpers exposing (focusElement)
import Html exposing (Html, button, div, h1, p, section, text)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (onClick)
import String
import Types exposing (TimelineModel)


type Fixture
    = Empty
    | Zero
    | Spike
    | Many
    | Error


type Msg
    = Choose Fixture
    | TimelineMessage Types.Msg


main : Program () TimelineModel Msg
main =
    Browser.element
        { init = \_ -> ( fixtureTimeline Spike, Cmd.none )
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


update : Msg -> TimelineModel -> ( TimelineModel, Cmd Msg )
update msg model =
    case msg of
        Choose fixture ->
            ( fixtureTimelineWithFocus model fixture, Cmd.none )

        TimelineMessage timelineMsg ->
            case timelineMsg of
                Types.ToggleTimelineChartSeries key ->
                    ( { model | chartSeries = Feature.Timeline.toggleTimelineChartSeries key model.chartSeries }, Cmd.none )

                Types.SelectTimelineHistogramBucket label since until ->
                    let
                        selection =
                            { label = label, since = since, until = until }
                    in
                    ( { model
                        | histogramSelectedBucket =
                            if model.histogramSelectedBucket == Just selection then
                                Nothing

                            else
                                Just selection
                      }
                    , Cmd.none
                    )

                Types.ResetTimelineHistogramSelection ->
                    ( { model | histogramSelectedBucket = Nothing }, Cmd.none )

                Types.FocusTimelineChartPoint action series index ->
                    ( { model | chartPointFocus = Dict.insert (Feature.Timeline.timelinePointFocusKey action series) index model.chartPointFocus }
                    , focusElement (Feature.Timeline.timelinePointId action series index) |> Cmd.map TimelineMessage
                    )

                _ ->
                    ( model, Cmd.none )


view : TimelineModel -> Html Msg
view model =
    div [ class "timeline-fixture", attribute "data-testid" "timeline-fixture" ]
        [ h1 [] [ text "Lifecycle line graph deterministic fixture" ]
        , p [] [ text "This local fixture renders the production chart with no network or authentication dependency." ]
        , div [ class "timeline-fixture-controls" ]
            [ fixtureButton Empty "Empty"
            , fixtureButton Zero "Zero"
            , fixtureButton Spike "Spike"
            , fixtureButton Many "Many buckets"
            , fixtureButton Error "Graph error"
            ]
        , Html.map TimelineMessage (Feature.Timeline.viewTimelineHistogram model)
        , section [ class "timeline-fixture-cards", attribute "data-testid" "timeline-fixture-cards" ]
            [ p [] [ text "Event cards remain available independently of the graph state." ]
            , case model.histogramSelectedBucket of
                Just selection ->
                    p [ attribute "data-testid" "timeline-fixture-selection" ] [ text ("Selected [" ++ selection.since ++ ", " ++ selection.until ++ ")") ]

                Nothing ->
                    p [ attribute "data-testid" "timeline-fixture-selection" ] [ text "No bucket selected" ]
            , div [ attribute "data-testid" "timeline-fixture-card-count" ] [ text (String.fromInt (List.length (fixtureVisibleEvents model))) ]
            , div [ class "timeline-event-card" ] (List.map fixtureEventCard (fixtureVisibleEvents model))
            ]
        ]


fixtureButton : Fixture -> String -> Html Msg
fixtureButton fixture label =
    button [ onClick (Choose fixture), attribute "data-testid" ("fixture-" ++ String.toLower (String.replace " " "-" label)) ] [ text label ]


fixtureTimeline : Fixture -> TimelineModel
fixtureTimeline fixture =
    let
        base =
            Feature.Timeline.init

        buckets =
            case fixture of
                Empty ->
                    []

                Zero ->
                    List.range 1 3 |> List.map (bucket 0)

                Spike ->
                    [ bucket 1 1, bucket 9 2, bucket 2 3 ]

                Many ->
                    List.range 0 365 |> List.map (\index -> manyBucket (modBy 11 index) index)

                Error ->
                    [ bucket 2 1, bucket 4 2 ]
    in
    { base
        | histogramSince = "2026-01-01"
        , histogramUntil = "2026-04-01"
        , histogramBuckets = buckets
        , histogramError =
            if fixture == Error then
                Just "Deterministic graph error"

            else
                Nothing
    }


fixtureTimelineWithFocus : TimelineModel -> Fixture -> TimelineModel
fixtureTimelineWithFocus previous fixture =
    let
        next =
            fixtureTimeline fixture
    in
    { next
        | chartPointFocus =
            Feature.Timeline.clampTimelinePointFocus (List.length next.histogramBuckets) previous.chartPointFocus
    }


bucket : Int -> Int -> Api.WorkspaceTimelineBucket
bucket count index =
    bucketForDates count (utcDate 2026 (index - 1)) (utcDate 2026 index) ("Jan " ++ String.fromInt index)


manyBucket : Int -> Int -> Api.WorkspaceTimelineBucket
manyBucket count dayOffset =
    let
        start =
            utcDate 2028 dayOffset
    in
    bucketForDates count start (utcDate 2028 (dayOffset + 1)) (String.left 10 start)


bucketForDates : Int -> String -> String -> String -> Api.WorkspaceTimelineBucket
bucketForDates count start ending label =
    let
        legacy =
            { created = 0, completed = 0, cancelled = 99 }

        project =
            { created = count, completed = 0, deleted = count }

        task =
            { created = modBy 7 (count + 2), completed = 0, deleted = modBy 6 (count + 3) }

        subtask =
            { created = modBy 5 (count + 1), completed = 0, deleted = modBy 4 (count + 2) }

        observation =
            { created = modBy 4 (count + 3), completed = 0, deleted = modBy 3 (count + 1) }
    in
    { bucketStart = start
    , bucketEnd = ending
    , label = label
    , counts = { project = legacy, subproject = legacy, task = legacy, subtask = legacy }
    , totals = legacy
    , series = { project = project, task = task, subtask = subtask, observation = observation }
    , seriesTotals = { created = project.created + task.created + subtask.created + observation.created, completed = 0, deleted = project.deleted + task.deleted + subtask.deleted + observation.deleted }
    }


utcDate : Int -> Int -> String
utcDate year dayOffset =
    let
        ( resolvedYear, month, day ) =
            calendarDate year dayOffset
    in
    String.fromInt resolvedYear ++ "-" ++ String.padLeft 2 '0' (String.fromInt month) ++ "-" ++ String.padLeft 2 '0' (String.fromInt day) ++ "T00:00:00Z"


calendarDate : Int -> Int -> ( Int, Int, Int )
calendarDate year dayOffset =
    calendarMonth year 1 dayOffset


calendarMonth : Int -> Int -> Int -> ( Int, Int, Int )
calendarMonth year month remainingDays =
    let
        days =
            monthLength year month
    in
    if remainingDays < days then
        ( year, month, remainingDays + 1 )

    else if month == 12 then
        calendarMonth (year + 1) 1 (remainingDays - days)

    else
        calendarMonth year (month + 1) (remainingDays - days)


monthLength : Int -> Int -> Int
monthLength year month =
    case month of
        2 ->
            if isLeapYear year then
                29

            else
                28

        4 ->
            30

        6 ->
            30

        9 ->
            30

        11 ->
            30

        _ ->
            31


isLeapYear : Int -> Bool
isLeapYear year =
    modBy 400 year == 0 || (modBy 4 year == 0 && modBy 100 year /= 0)


fixtureVisibleEvents : TimelineModel -> List Api.WorkspaceTimelineEvent
fixtureVisibleEvents model =
    Feature.Timeline.filterTimelineEventsForSelection model.histogramSelectedBucket model.entityFilter model.eventFilter fixtureEvents


fixtureEventCard : Api.WorkspaceTimelineEvent -> Html Msg
fixtureEventCard event =
    div [ attribute "data-testid" ("timeline-fixture-card-" ++ event.id) ] [ text event.title ]


fixtureEvents : List Api.WorkspaceTimelineEvent
fixtureEvents =
    [ fixtureEvent "inside" "2026-01-02T12:00:00Z"
    , fixtureEvent "outside" "2026-01-04T00:00:00Z"
    ]


fixtureEvent : String -> String -> Api.WorkspaceTimelineEvent
fixtureEvent id occurredAt =
    { id = id
    , workspaceId = "fixture-workspace"
    , eventType = "task_completed"
    , entityType = "task"
    , entityId = id
    , title = "Fixture " ++ id ++ " event"
    , occurredAt = occurredAt
    , actor = Nothing
    , project = Nothing
    , parentTask = Nothing
    , statusTransition = Nothing
    , navigation = { entityType = "task", entityId = id }
    , sourceAuditId = Nothing
    }
