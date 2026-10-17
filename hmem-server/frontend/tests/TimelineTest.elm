module TimelineTest exposing (suite)

import Api
import AppShell
import Dict
import Expect
import Feature.Timeline
import Helpers
import Http
import Json.Decode as Decode
import TimestampDisplay
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Event as Event
import Test.Html.Selector as Selector
import Types exposing (Flags, Model, Msg(..), Page(..), TimelineModel, WorkspaceTab(..))
import Url


suite : Test
suite =
    describe "lifecycle line graph helpers"
        [ test "open inventory decoder distinguishes available, unknown, future and older payloads" <| \_ ->
            Expect.all
                [ \_ -> decodeOpenFields "" |> Expect.equal (Ok [ ( Nothing, Nothing, "history_unavailable" ) ])
                , \_ -> decodeOpenFields ",\"open_counts\":{\"projects\":2,\"tasks\":6},\"open_as_of\":\"2026-01-01T12:00:00Z\",\"open_status\":\"available\"" |> Expect.equal (Ok [ ( Just { projects = 2, tasks = 6 }, Just "2026-01-01T12:00:00Z", "available" ) ])
                , \_ -> decodeOpenFields ",\"open_counts\":null,\"open_as_of\":\"2026-01-02T00:00:00Z\",\"open_status\":\"history_unavailable\"" |> Expect.equal (Ok [ ( Nothing, Just "2026-01-02T00:00:00Z", "history_unavailable" ) ])
                , \_ -> decodeOpenFields ",\"open_counts\":null,\"open_as_of\":null,\"open_status\":\"future\"" |> Expect.equal (Ok [ ( Nothing, Nothing, "future" ) ])
                , \_ -> decodeOpenFields ",\"open_counts\":{\"projects\":-1,\"tasks\":6}" |> Expect.err
                ] ()
        , test "stock stacking uses its own total domain with finite zero and extreme geometries" <| \_ ->
            let
                normal = Feature.Timeline.openBarGeometry 8 { projects = 2, tasks = 6 }
                zero = Feature.Timeline.openBarGeometry 0 { projects = 0, tasks = 0 }
                huge = Feature.Timeline.openBarGeometry 4000000 { projects = 1000000, tasks = 3000000 }
                unavailable = { stockBucket | openStatus = "history_unavailable" }
            in
            Expect.all
                [ \_ -> normal |> Expect.equal { projectsY = 187, projectsHeight = 49, tasksY = 40, tasksHeight = 147, total = 8 }
                , \_ -> zero |> Expect.equal { projectsY = 236, projectsHeight = 0, tasksY = 236, tasksHeight = 0, total = 0 }
                , \_ -> List.all (\value -> not (isNaN value || isInfinite value) && value >= 0) [ huge.projectsY, huge.projectsHeight, huge.tasksY, huge.tasksHeight ] |> Expect.equal True
                , \_ -> List.map Feature.Timeline.openChartMaximum [ [], [ unavailable ], [ stockBucket ], [ stockBucket, { stockBucket | openCounts = Just { projects = 1000000, tasks = 3000000 } } ] ] |> Expect.equal [ 0, 0, 8, 4000000 ]
                ] ()
        , test "pointer hover wins over focus and blur does not erase a different hovered bar" <| \_ ->
            let
                initial = Feature.Timeline.init
                base = timelineModel { initial | histogramBuckets = [ stockBucket, stockBucket ] }
                focused = Feature.Timeline.update (FocusTimelineOpenBar (Just 0)) base |> Tuple.first
                hovered = Feature.Timeline.update (HoverTimelineOpenBar (Just 1)) focused |> Tuple.first
                blurred = Feature.Timeline.update (FocusTimelineOpenBar Nothing) hovered |> Tuple.first
                left = Feature.Timeline.update (HoverTimelineOpenBar Nothing) blurred |> Tuple.first
            in
            Expect.equal [ Just 0, Just 1, Just 1, Nothing ] (List.map (.timeline >> Feature.Timeline.activeOpenBar) [ focused, hovered, blurred, left ])
        , test "accepted bucket data and changed controls reset interaction while stale data cannot do so" <| \_ ->
            let
                initial = Feature.Timeline.init
                request = { workspaceId = "workspace-1", since = stockBucket.bucketStart, until = stockBucket.bucketEnd, bucket = "day" }
                identity = { requestId = 1, refreshGeneration = 0, refreshEpoch = 0 }
                base = timelineModel { initial | histogramBuckets = [ stockBucket ], histogramActiveRequest = Just request, histogramActiveIdentity = Just identity, chartBarHover = Just 0, chartBarFocus = Just 0 }
                response = { workspaceId = "workspace-1", since = request.since, until = request.until, bucket = "day", buckets = [ stockBucket ], nextSince = Nothing }
                accepted = Feature.Timeline.update (GotWorkspaceTimelineBuckets identity request (Ok response)) base |> Tuple.first
                stale = Feature.Timeline.update (GotWorkspaceTimelineBuckets { identity | requestId = 99 } request (Ok response)) base |> Tuple.first
                changed = Feature.Timeline.update (SetTimelineHistogramBucket "week") base |> Tuple.first
            in
            Expect.equal [ Nothing, Just 0, Nothing ] (List.map (.timeline >> Feature.Timeline.activeOpenBar) [ accepted, stale, changed ])
        , test "line filters leave stock bars, RHS and semantic inventory available" <| \_ ->
            let
                initial = Feature.Timeline.init
                timeline = { initial | histogramBuckets = [ stockBucket ], chartActions = Dict.map (\_ _ -> False) initial.chartActions, chartSeries = { projects = False, tasks = False, subtasks = False, observations = False } }
                rendered = Feature.Timeline.viewTimelineHistogram TimestampDisplay.empty timeline |> Query.fromHtml
                partial = { stockBucket | openAsOf = Just "2026-01-01T12:00:00Z" }
            in
            Expect.all
                [ \_ -> rendered |> Query.findAll [ Selector.class "timeline-line" ] |> Query.count (Expect.equal 0)
                , \_ -> rendered |> Query.findAll [ Selector.class "timeline-open-idle" ] |> Query.count (Expect.equal 1)
                , \_ -> rendered |> Query.findAll [ Selector.class "timeline-open-segment" ] |> Query.count (Expect.equal 2)
                , \_ -> rendered |> Query.has [ Selector.text "Open items", Selector.text "Open tasks + subtasks" ]
                , \_ -> Feature.Timeline.timestampSources { initial | histogramBuckets = [ partial ] } |> List.member "2026-01-01T12:00:00Z" |> Expect.equal True
                ] ()
        , test "observation activity is independent and never inflates supported lifecycle totals" <| \_ ->
            let
                initial = Feature.Timeline.init
                counts c co d a ca = { created = c, completed = co, deleted = d, archived = a, cancelled = ca }
                mixed = { stockBucket | series = { project = counts 2 3 4 5 99, task = counts 7 11 13 99 17, subtask = counts 19 23 29 99 31, observation = counts 101 99 103 99 99 } }
                timeline = { initial | histogramBuckets = [ mixed ] }
                values t = Feature.Timeline.lifecycleChartSeries t |> List.map (\series -> ( series.key, List.map .count series.values ))
                observationOnly = { timeline | chartActions = Dict.map (\_ _ -> False) timeline.chartActions }
                noObservations = { timeline | chartSeries = { projects = True, tasks = True, subtasks = True, observations = False } }
            in
            Expect.all
                [ \_ -> values timeline |> Expect.equal [ ( "created", [ 28 ] ), ( "completed", [ 37 ] ), ( "deleted", [ 46 ] ), ( "archived", [ 5 ] ), ( "cancelled", [ 48 ] ), ( "observations", [ 101 ] ) ]
                , \_ -> values observationOnly |> Expect.equal [ ( "observations", [ 101 ] ) ]
                , \_ -> values noObservations |> List.map Tuple.second |> Expect.equal [ [ 28 ], [ 37 ], [ 46 ], [ 5 ], [ 48 ] ]
                , \_ -> Feature.Timeline.selectedBucketSummary timeline mixed |> Expect.equal
                    [ "101 Observations", "2 Projects Created", "26 Tasks/Subtasks Created", "3 Projects Completed", "34 Tasks/Subtasks Completed", "4 Projects Deleted", "42 Tasks/Subtasks Deleted", "48 Tasks/Subtasks Cancelled", "5 Projects Archived" ]
                , \_ -> Feature.Timeline.selectedBucketSummary { observationOnly | chartSeries = { projects = False, tasks = False, subtasks = False, observations = False } } mixed |> Expect.equal [ "No activity selected." ]
                , \_ -> Feature.Timeline.selectedBucketSummary { timeline | chartSeries = { projects = False, tasks = False, subtasks = True, observations = False }, chartActions = Dict.fromList [ ( "completed", True ), ( "archived", True ) ] } mixed |> Expect.equal [ "23 Subtasks Completed" ]
                , \_ -> Feature.Timeline.selectedBucketSummary { timeline | chartSeries = { projects = False, tasks = True, subtasks = False, observations = False }, chartActions = Dict.fromList [ ( "created", True ) ] } stockBucket |> Expect.equal [ "0 Tasks Created" ]
                ] ()
        , test "line hover and focus retire on hidden lines range and accepted data but stale replies leave them unchanged" <| \_ ->
            let
                initial = Feature.Timeline.init
                interaction = Just { series = "created", index = 0 }
                request = { workspaceId = "workspace-1", since = stockBucket.bucketStart, until = stockBucket.bucketEnd, bucket = "day" }
                identity = { requestId = 1, refreshGeneration = 0, refreshEpoch = 0 }
                base = timelineModel { initial | histogramBuckets = [ stockBucket ], histogramActiveRequest = Just request, histogramActiveIdentity = Just identity, chartLineHover = interaction, chartLineFocus = interaction }
                response = { workspaceId = "workspace-1", since = request.since, until = request.until, bucket = "day", buckets = [ stockBucket ], nextSince = Nothing }
                next msg = Feature.Timeline.update msg base |> Tuple.first |> .timeline |> (\t -> ( t.chartLineHover, t.chartLineFocus ))
            in
            Expect.equal [ ( Nothing, Nothing ), ( Nothing, Nothing ), ( Nothing, Nothing ), ( Nothing, Nothing ), ( interaction, interaction ) ]
                [ next (ToggleTimelineChartAction "created"), next (ToggleTimelineChartSeries "tasks"), next (SetTimelineHistogramWindow "7"), next (GotWorkspaceTimelineBuckets identity request (Ok response)), next (GotWorkspaceTimelineBuckets { identity | requestId = 99 } request (Ok response)) ]
        , test "in-chart summary shares selected filters for a hovered line and removes lower disclosures and native hints" <| \_ ->
            let
                initial = Feature.Timeline.init
                timeline = { initial | histogramBuckets = [ stockBucket ], chartLineHover = Just { series = "created", index = 0 } }
                rendered = Feature.Timeline.viewTimelineHistogram TimestampDisplay.empty timeline |> Query.fromHtml
            in
            Expect.all
                [ \_ -> rendered |> Query.find [ Selector.class "timeline-chart-tooltip" ] |> Query.has [ Selector.text "0 Observations", Selector.text "0 Projects Created", Selector.text "0 Tasks/Subtasks Completed" ]
                , \_ -> rendered |> Query.find [ Selector.class "timeline-chart-tooltip" ] |> Query.hasNot [ Selector.text stockBucket.label, Selector.text "open items", Selector.text "exclusive", Selector.text "2026" ]
                , \_ -> rendered |> Query.findAll [ Selector.class "timeline-open-details" ] |> Query.count (Expect.equal 0)
                , \_ -> rendered |> Query.findAll [ Selector.class "timeline-open-tooltip" ] |> Query.count (Expect.equal 0)
                , \_ -> rendered |> Query.find [ Selector.id "timeline-point-lifecycle-created-0" ] |> Query.findAll [ Selector.tag "title" ] |> Query.count (Expect.equal 0)
                , \_ -> rendered |> Query.findAll [ Selector.class "timeline-line-active" ] |> Query.count (Expect.equal 1)
                ] ()
        , test "single buckets are centered and multi-bucket coordinates remain in bounds" <|
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
        , test "event rows show one inline actor while retaining title, status and project context" <| \_ ->
            let
                original = timelineEvent "Review completed"
                event =
                    { original
                        | actor = Just { actorType = Just "user", actorId = Just "raw-user-id", actorLabel = Just "Alice", botIdentity = Just { role = "Reviewer", modelId = "codex:gpt-6.1-sol:extra-high" } }
                        , project = Just { id = "project-1", name = "Release" }
                        , statusTransition = Just { from = "todo", to = "in_progress" }
                    }
                initial = Feature.Timeline.init
                rendered = Feature.Timeline.viewWorkspaceTimelinePanel "workspace-1" (timelineModel { initial | events = [ event ] }) |> Query.fromHtml
                card = rendered |> Query.find [ Selector.class "timeline-event-card" ]
            in
            Expect.all
                [ \_ -> card |> Query.has [ Selector.text "Review completed", Selector.text "Project: Release", Selector.text "In progress" ]
                , \_ -> card |> Event.simulate Event.click |> Event.expect (NavigateToTimelineEntity event)
                , \_ -> card |> Query.findAll [ Selector.class "actor-attribution" ] |> Query.count (Expect.equal 1)
                , \_ -> card |> Query.has [ Selector.text "Alice · Reviewer · codex:gpt-6.1-sol:extra-high" ]
                , \_ -> card |> Query.findAll [ Selector.class "timeline-actor-details" ] |> Query.count (Expect.equal 0)
                , \_ -> card |> Query.findAll [ Selector.tag "details" ] |> Query.count (Expect.equal 0)
                , \_ -> card |> Query.hasNot [ Selector.text "raw-user-id" ]
                , \_ -> card |> Query.findAll [ Selector.class "timeline-context-chip" ] |> Query.count (Expect.equal 1)
                ] ()
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
                , width == 12832
                , second - first >= 128
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
        , test "native coordinates share the path viewport percentage, including a single centered bucket" <|
            \_ ->
                [ Feature.Timeline.chartPercentX 720 (Feature.Timeline.chartXWithWidth 720 1 0)
                , Feature.Timeline.chartPercentX 12832 (Feature.Timeline.chartXWithWidth 12832 1 0)
                , Feature.Timeline.chartPercentX 720 180
                , Feature.Timeline.chartPercentX 12832 3208
                ]
                    |> Expect.equal [ "50%", "50%", "25%", "25%" ]
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


stockBucket : Api.WorkspaceTimelineBucket
stockBucket =
    let legacy = { created = 0, completed = 0, cancelled = 0 }
        zero = { created = 0, completed = 0, deleted = 0, archived = 0, cancelled = 0 }
    in
    { bucketStart = "2026-01-01T00:00:00Z", bucketEnd = "2026-01-02T00:00:00Z", label = "A"
    , counts = { project = legacy, subproject = legacy, task = legacy, subtask = legacy }, totals = legacy
    , series = { project = zero, task = zero, subtask = zero, observation = zero }, seriesTotals = zero
    , openCounts = Just { projects = 2, tasks = 6 }, openAsOf = Just "2026-01-02T00:00:00Z", openStatus = "available" }


decodeOpenFields : String -> Result Decode.Error (List ( Maybe Api.TimelineOpenCounts, Maybe String, String ))
decodeOpenFields fields =
    Decode.decodeString Api.workspaceTimelineBucketsResponseDecoder
        ("""{"workspace_id":"workspace-1","since":"2026-01-01T00:00:00Z","until":"2026-01-02T00:00:00Z","bucket":"day","buckets":[{"bucket_start":"2026-01-01T00:00:00Z","bucket_end":"2026-01-02T00:00:00Z","label":"A","counts":{"project":{"created":0,"completed":0,"cancelled":0},"subproject":{"created":0,"completed":0,"cancelled":0},"task":{"created":0,"completed":0,"cancelled":0},"subtask":{"created":0,"completed":0,"cancelled":0}},"totals":{"created":0,"completed":0,"cancelled":0},"series":{"project":{"created":0,"completed":0,"deleted":0},"task":{"created":0,"completed":0,"deleted":0},"subtask":{"created":0,"completed":0,"deleted":0},"observation":{"created":0,"completed":0,"deleted":0}},"series_totals":{"created":0,"completed":0,"deleted":0}""" ++ fields ++ "}]}")
        |> Result.map (\response -> List.map (\bucket -> ( bucket.openCounts, bucket.openAsOf, bucket.openStatus )) response.buckets)


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
