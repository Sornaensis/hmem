module TimelineTest exposing (suite)

import Expect
import Dict
import Feature.Timeline
import Test exposing (..)


suite : Test
suite =
    describe "lifecycle line graph helpers"
        [ test "single buckets are centered and multi-bucket coordinates remain in bounds" <|
            \_ ->
                [ Feature.Timeline.chartX 0 0
                , Feature.Timeline.chartX 1 0
                , Feature.Timeline.chartX 2 0
                , Feature.Timeline.chartX 2 1
                , Feature.Timeline.chartY 0 0
                , Feature.Timeline.chartY 7 7
                ]
                    |> Expect.equal [ 360, 360, 80, 640, 236, 40 ]
        , test "actual maxima and safe render domains keep zero baselines honest" <|
            \_ ->
                let
                    empty =
                        []

                    zero =
                        [ { key = "projects", label = "Projects", values = [ { label = "A", since = "2026-01-01T00:00:00Z", until = "2026-01-02T00:00:00Z", count = 0 } ] } ]

                    spike =
                        [ { key = "projects", label = "Projects", values = [ { label = "A", since = "2026-01-01T00:00:00Z", until = "2026-01-02T00:00:00Z", count = 2 }, { label = "B", since = "2026-01-02T00:00:00Z", until = "2026-01-03T00:00:00Z", count = 47 } ] } ]
                in
                [ Feature.Timeline.lineChartMaximum empty
                , Feature.Timeline.lineChartMaximum zero
                , Feature.Timeline.lineChartMaximum spike
                , Feature.Timeline.lineChartRenderDomain 0
                , Feature.Timeline.lineChartRenderDomain 1
                , Feature.Timeline.lineChartRenderDomain 3
                , Feature.Timeline.lineChartRenderDomain 47
                ]
                    |> Expect.equal [ 0, 0, 47, 1, 1, 3, 47 ]
        , test "ticks retain exact actual maxima for zero, small, spike, and non-multiple domains" <|
            \_ ->
                [ Feature.Timeline.chartTickValues 0
                , Feature.Timeline.chartTickValues 1
                , Feature.Timeline.chartTickValues 2
                , Feature.Timeline.chartTickValues 3
                , Feature.Timeline.chartTickValues 7
                , Feature.Timeline.chartTickValues 5
                , Feature.Timeline.chartTickValues 101
                ]
                    |> Expect.equal [ [ 0 ], [ 0, 1 ], [ 0, 1, 2 ], [ 0, 1, 2, 3 ], [ 0, 2, 4, 6, 7 ], [ 0, 2, 4, 5 ], [ 0, 26, 52, 78, 101 ] ]
        , test "canvas grows with capped bucket counts while retaining interactive point spacing" <|
            \_ ->
                let
                    width =
                        Feature.Timeline.chartCanvasWidth 100

                    first =
                        Feature.Timeline.chartXWithWidth width 100 0

                    second =
                        Feature.Timeline.chartXWithWidth width 100 1

                    last =
                        Feature.Timeline.chartXWithWidth width 100 99
                in
                [ Feature.Timeline.chartCanvasWidth 1 == 720
                , width == 10456
                , second - first >= 104
                , first >= 80
                , last <= toFloat width - 80
                , List.all (\offset -> first + offset >= 11) [ -36, -12, 12, 36 ]
                , List.all (\offset -> last + offset <= toFloat width - 11) [ -36, -12, 12, 36 ]
                ]
                    |> Expect.equal [ True, True, True, True, True, True, True ]
        , test "short responses clamp roving focus to their final available bucket" <|
            \_ ->
                Feature.Timeline.clampTimelinePointFocus 3 (Dict.fromList [ ( "created-observations", 365 ), ( "deleted-tasks", -4 ) ])
                    |> Expect.equal (Dict.fromList [ ( "created-observations", 2 ), ( "deleted-tasks", 0 ) ])
        , test "coincident series markers use nonoverlapping pointer-target offsets" <|
            \_ ->
                [ Feature.Timeline.pointMarkerOffset "projects"
                , Feature.Timeline.pointMarkerOffset "tasks"
                , Feature.Timeline.pointMarkerOffset "subtasks"
                , Feature.Timeline.pointMarkerOffset "observations"
                ]
                    |> Expect.equal [ -36, -12, 12, 36 ]
        , test "roving point controls make nearby and boundary buckets keyboard-reachable" <|
            \_ ->
                ( Feature.Timeline.timelinePointFocusKey "created" "projects"
                , Feature.Timeline.timelinePointId "created" "projects" 183
                , [ Feature.Timeline.timelinePointNextIndex "ArrowRight" 366 182
                  , Feature.Timeline.timelinePointNextIndex "ArrowLeft" 366 0
                  , Feature.Timeline.timelinePointNextIndex "Home" 366 183
                  , Feature.Timeline.timelinePointNextIndex "End" 366 183
                  , Feature.Timeline.timelinePointNextIndex "PageDown" 366 153
                  , Feature.Timeline.timelinePointNextIndex "PageUp" 366 30
                  ]
                )
                    |> Expect.equal ( "created-projects", "timeline-point-created-projects-183", [ 183, 0, 0, 365, 183, 0 ] )
        , test "paths retain ordered zero-filled and spike coordinates while empty paths stay empty" <|
            \_ ->
                let
                    points =
                        [ { x = 44, y = 236, label = "A", since = "a", until = "b", count = 0 }
                        , { x = 116, y = 40, label = "B", since = "b", until = "c", count = 99 }
                        ]
                in
                [ Feature.Timeline.timelinePath []
                , Feature.Timeline.timelinePath points
                ]
                    |> Expect.equal [ "", "M 44 236 L 116 40" ]
        , test "series toggles are independent and allow every series to be hidden" <|
            \_ ->
                let
                    initial =
                        { projects = True, tasks = True, subtasks = True, observations = True }

                    hidden =
                        initial
                            |> Feature.Timeline.toggleTimelineChartSeries "projects"
                            |> Feature.Timeline.toggleTimelineChartSeries "tasks"
                            |> Feature.Timeline.toggleTimelineChartSeries "subtasks"
                            |> Feature.Timeline.toggleTimelineChartSeries "observations"
                in
                [ (Feature.Timeline.toggleTimelineChartSeries "projects" initial).projects
                , hidden.projects
                , hidden.tasks
                , hidden.subtasks
                , hidden.observations
                ]
                    |> Expect.equal [ False, False, False, False, False ]
        ]
