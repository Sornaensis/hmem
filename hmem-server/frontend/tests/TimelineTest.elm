module TimelineTest exposing (suite)

import Api
import AppShell
import Dict
import Expect
import Feature.Timeline
import Helpers
import Http
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Types exposing (Flags, Model, Msg(..), Page(..), TimelineModel, WorkspaceTab(..))
import Url


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
        , test "live refresh uses the frozen trailing debounce and accepts only the newest request identity" <|
            \_ ->
                let
                    first =
                        { requestId = 17, refreshGeneration = 4, refreshEpoch = 8 }

                    second =
                        { requestId = 18, refreshGeneration = 4, refreshEpoch = 8 }
                in
                { debounceMs = Feature.Timeline.timelineDebounceMs
                , activeTimerDispatches = Feature.Timeline.timelineRefreshDispatchAllowed True True 4 8 (Just 4) 4 8
                , backgroundTimerDoesNotDispatch = Feature.Timeline.timelineRefreshDispatchAllowed False True 4 8 (Just 4) 4 8
                , supersededTimerDoesNotDispatch = Feature.Timeline.timelineRefreshDispatchAllowed True True 5 8 (Just 4) 4 8
                , resetEpochTimerDoesNotDispatch = Feature.Timeline.timelineRefreshDispatchAllowed True True 4 9 (Just 4) 4 8
                , currentResponseAccepted = Feature.Timeline.timelineResponseIsCurrent first (Just first) 4 8
                , sameRangeOlderResponseRejected = Feature.Timeline.timelineResponseIsCurrent first (Just second) 4 8
                , reinvalidatedResponseRejected = Feature.Timeline.timelineResponseIsCurrent second (Just second) 5 8
                , reenteredWorkspaceResponseRejected = Feature.Timeline.timelineResponseIsCurrent second (Just second) 4 9
                }
                    |> Expect.equal
                        { debounceMs = 250
                        , activeTimerDispatches = True
                        , backgroundTimerDoesNotDispatch = False
                        , supersededTimerDoesNotDispatch = False
                        , resetEpochTimerDoesNotDispatch = False
                        , currentResponseAccepted = True
                        , sameRangeOlderResponseRejected = False
                        , reinvalidatedResponseRejected = False
                        , reenteredWorkspaceResponseRejected = False
                        }
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
        , test "live refresh preserves exact selected event and configured bucket projections" <|
            \_ ->
                let
                    selected =
                        { label = "2026-W07", since = "2026-02-09T00:00:00Z", until = "2026-02-16T00:00:00Z" }

                    initial =
                        Feature.Timeline.init

                    timeline =
                        { initial
                            | histogramSince = "2026-01-01"
                            , histogramUntil = "2026-04-01"
                            , histogramBucket = "week"
                            , histogramSelectedBucket = Just selected
                        }

                    plan =
                        Feature.Timeline.timelineRefreshPlan "workspace-a" timeline
                in
                Expect.equal
                    { events = { workspaceId = "workspace-a", since = Just selected.since, until = Just selected.until }
                    , histogram = Just { workspaceId = "workspace-a", since = "2026-01-01T00:00:00Z", until = "2026-04-01T00:00:00Z", bucket = "week" }
                    }
                    plan
        , test "a refresh failure is nonblocking only when a last-good event response exists" <|
            \_ ->
                let
                    request =
                        { workspaceId = "workspace-a", since = Nothing, until = Nothing }

                    initial =
                        Feature.Timeline.init
                in
                { firstLoadFailureBlocks = Feature.Timeline.timelineErrorIsBlocking { initial | error = Just "failed" }
                , cachedFailureRendersCards = not (Feature.Timeline.timelineErrorIsBlocking { initial | error = Just "failed", eventsLoadedRequest = Just request })
                , resetEpoch = (Feature.Timeline.reset 9).refreshEpoch
                , resetRequestIdentity = (Feature.Timeline.reset 9).nextRequestIdentity
                }
                    |> Expect.equal
                        { firstLoadFailureBlocks = True
                        , cachedFailureRendersCards = True
                        , resetEpoch = 9
                        , resetRequestIdentity = 1
                        }
        , test "same-workspace re-entry stamps the new session epoch before its first request" <|
            \_ ->
                let
                    ( reentered, _ ) =
                        Feature.Timeline.ensureLoaded "https://api.example" "workspace-1" 9 (Feature.Timeline.reset 0)
                in
                Expect.equal (Just 9) (reentered.eventsActiveIdentity |> Maybe.map .refreshEpoch)
        , test "a live refresh failure retains cards and renders a retry action" <|
            \_ ->
                let
                    request =
                        { workspaceId = "workspace-1", since = Nothing, until = Nothing }

                    identity =
                        { requestId = 8, refreshGeneration = 3, refreshEpoch = 4 }

                    event =
                        timelineEvent "retained-card"

                    initial =
                        Feature.Timeline.init

                    timeline =
                        { initial
                            | events = [ event ]
                            , loadedWorkspaceId = Just "workspace-1"
                            , eventsLoadedRequest = Just request
                            , eventsActiveRequest = Just request
                            , eventsActiveIdentity = Just identity
                            , loading = True
                            , refreshGeneration = 3
                            , refreshEpoch = 4
                        }

                    afterFailure =
                        Feature.Timeline.update
                            (GotWorkspaceTimeline identity request (Err Http.Timeout))
                            (timelineModel timeline)
                            |> Tuple.first

                    rendered =
                        Feature.Timeline.viewWorkspaceTimelinePanel "workspace-1" afterFailure
                            |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> Expect.equal [ event ] afterFailure.timeline.events
                    , \_ -> Expect.equal (Just request) afterFailure.timeline.eventsLoadedRequest
                    , \_ -> Expect.equal (Just "Failed to load timeline events.") afterFailure.timeline.error
                    , \_ -> Query.has [ Selector.class "timeline-stale-notice", Selector.text "Retry", Selector.text "retained-card" ] rendered
                    ]
                    ()
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


timelineModel : TimelineModel -> Model
timelineModel timeline =
    let
        sourceUrl =
            { protocol = Url.Https
            , host = "app.example"
            , port_ = Nothing
            , path = "/workspace/workspace-1"
            , query = Nothing
            , fragment = Just "tab=timeline"
            }

        initial =
            AppShell.initModel
                Nothing
                sourceUrl
                (WorkspacePage "workspace-1")
                timelineFlags
                Nothing
                (Helpers.parseFragment sourceUrl.fragment)
                |> AppShell.finalizeInit (WorkspacePage "workspace-1")
    in
    { initial
        | selectedWorkspaceId = Just "workspace-1"
        , activeTab = TimelineTab
        , timeline = timeline
    }


timelineFlags : Flags
timelineFlags =
    { apiUrl = "https://api.example"
    , wsUrl = "wss://api.example"
    , sessionId = "session-1"
    , runtimeMode = "test"
    , authTokenStorageKey = "hmem-auth-token"
    , authTokenPresent = False
    , loginUrl = Nothing
    , logoutUrl = Nothing
    }


timelineEvent : String -> Api.WorkspaceTimelineEvent
timelineEvent title =
    { id = "timeline-event"
    , workspaceId = "workspace-1"
    , eventType = "created"
    , entityType = "task"
    , entityId = "task-1"
    , title = title
    , occurredAt = "2026-08-29T12:00:00Z"
    , actor = Nothing
    , project = Nothing
    , parentTask = Nothing
    , statusTransition = Nothing
    , navigation = { entityType = "task", entityId = "task-1" }
    , sourceAuditId = Nothing
    }
