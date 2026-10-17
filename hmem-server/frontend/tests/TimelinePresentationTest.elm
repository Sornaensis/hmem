module TimelinePresentationTest exposing (suite)

import Api
import AppShell
import Dict
import Expect
import Feature.Timeline as Timeline
import Html.Attributes as Attributes
import Set
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import TimestampDisplay as Display
import Types exposing (..)
import Url


since : String
since = "2026-03-29T00:00:00Z"

until : String
until = "2026-03-30T00:00:00Z"

display : Display.State
display =
    let empty = Display.empty in
    { empty | cache = Dict.fromList
        [ ( since, Just { text = "2026-03-29 01:00", day = "2026-03-29" } )
        , ( until, Just { text = "2026-03-30 02:00", day = "2026-03-30" } )
        , ( "2026-01-01T00:15:00Z", Just { text = "2025-12-31 16:15", day = "2025-12-31" } )
        , ( "2026-01-01T08:15:00Z", Just { text = "2026-01-01 00:15", day = "2026-01-01" } )
        , ( "bad", Nothing )
        ] }

event : String -> String -> Api.WorkspaceTimelineEvent
event id occurredAt =
    { id = id, workspaceId = "w", eventType = "task_created", entityType = "task", entityId = id, title = "Raw " ++ occurredAt
    , occurredAt = occurredAt, actor = Nothing, project = Nothing, parentTask = Nothing, statusTransition = Nothing
    , navigation = { entityType = "task", entityId = id }, sourceAuditId = Nothing }

bucket : Api.WorkspaceTimelineBucket
bucket =
    let
        legacy = { created = 0, completed = 0, cancelled = 0 }
        zero = { created = 0, completed = 0, deleted = 0, archived = 0, cancelled = 0 }
        count = { zero | created = 5 }
    in
    { bucketStart = since, bucketEnd = until, label = "server-bucket-identity"
    , counts = { project = legacy, subproject = legacy, task = legacy, subtask = legacy }, totals = legacy
    , series = { project = zero, task = count, subtask = zero, observation = zero }, seriesTotals = count
    , openCounts = Just { projects = 2, tasks = 5 }, openAsOf = Just until, openStatus = "available" }

model : TimelineModel -> Model
model timeline =
    let
        initial = AppShell.initModel Nothing
            { protocol = Url.Https, host = "app.example", port_ = Nothing, path = "/workspace/w", query = Nothing, fragment = Nothing }
            (WorkspacePage "w")
            { apiUrl = "", wsUrl = "", sessionId = "fixture", runtimeMode = "test", authTokenStorageKey = "test", authTokenPresent = False, loginUrl = Nothing, logoutUrl = Nothing }
            Nothing { tab = TimelineTab, focus = Nothing, observationId = Nothing }
    in
    { initial | timeline = timeline, timestamps = display, selectedWorkspaceId = Just "w" }

suite : Test
suite =
    describe "Timeline browser-local presentation"
        [ test "local date headings and event time agree across UTC midnight and year rollover" <| \_ ->
            let
                first = event "first" "2026-01-01T08:15:00Z"
                second = event "second" "2026-01-01T00:15:00Z"
                initial = Timeline.init
                timeline = { initial | events = [ first, second ] }
                view = Timeline.viewWorkspaceTimelinePanel "w" (model timeline) |> Query.fromHtml
            in
            Expect.all
                [ \_ -> Timeline.groupTimelineEvents display (Timeline.visibleTimelineEvents timeline) |> List.map (\( day, values ) -> ( day, List.map .id values )) |> Expect.equal [ ( "2026-01-01", [ "first" ] ), ( "2025-12-31", [ "second" ] ) ]
                , \_ -> view |> Query.findAll [ Selector.class "timeline-date-heading" ] |> Query.count (Expect.equal 2)
                , \_ -> view |> Query.find [ Selector.id "timeline-event-second" ] |> Query.has [ Selector.text "2025-12-31 16:15", Selector.text second.title ]
                , \_ -> Timeline.visibleTimelineEvents timeline |> List.map .occurredAt |> Expect.equal [ first.occurredAt, second.occurredAt ]
                ] ()
        , test "DST endpoints localize graph axes and accessible names without changing selection identity or counts" <| \_ ->
            let
                initial = Timeline.init
                timeline = { initial | histogramBuckets = [ bucket ] }
                view = Timeline.viewTimelineHistogram display timeline |> Query.fromHtml
                point = view |> Query.find [ Selector.id "timeline-point-lifecycle-created-0" ]
            in
            Expect.all
                [ \_ -> view |> Query.find [ Selector.class "timeline-chart-x-axis" ] |> Query.has [ Selector.text "2026-03-29 01:00", Selector.attribute (Attributes.attribute "aria-label" "[2026-03-29 01:00, 2026-03-30 02:00)") ]
                , \_ -> point |> Query.has [ Selector.attribute (Attributes.attribute "aria-label" "0 Observations; 0 Projects Created; 5 Tasks/Subtasks Created; 0 Projects Completed; 0 Tasks/Subtasks Completed; 0 Projects Deleted; 0 Tasks/Subtasks Deleted; 0 Tasks/Subtasks Cancelled; 0 Projects Archived. Lifecycle activity, Create, 2026-03-29 01:00 to 2026-03-30 02:00 exclusive, 5, not selected") ]
                , \_ -> point |> Query.findAll [ Selector.tag "title" ] |> Query.count (Expect.equal 0)
                , \_ -> point |> Event.simulate Event.click |> Event.expect (SelectTimelineHistogramBucket bucket.label since until)
                , \_ -> view |> Query.hasNot [ Selector.text bucket.label ]
                , \_ -> timeline.histogramBuckets |> Expect.equal [ bucket ]
                ] ()
        , test "selected and empty intervals show local actual endpoints while request bounds stay raw" <| \_ ->
            let
                initial = Timeline.init
                selection = { label = bucket.label, since = since, until = until }
                timeline = { initial | histogramSelectedBucket = Just selection, histogramBuckets = [ bucket ] }
                view = Timeline.viewWorkspaceTimelinePanel "w" (model timeline) |> Query.fromHtml
            in
            Expect.all
                [ \_ -> view |> Query.find [ Selector.class "timeline-selection-range" ] |> Query.has [ Selector.text "[2026-03-29 01:00, 2026-03-30 02:00)" ]
                , \_ -> view |> Query.hasNot [ Selector.text bucket.label ]
                , \_ -> Timeline.timelineEventsRequest "w" timeline |> Expect.equal { workspaceId = "w", since = Just since, until = Just until }
                , \_ -> view |> Query.find [ Selector.id "timeline-point-lifecycle-created-0" ] |> Query.has [ Selector.attribute (Attributes.attribute "aria-pressed" "true") ]
                ] ()
        , test "display demand follows filters selection retained event data and independent graph buckets" <| \_ ->
            let
                initial = Timeline.init
                inside = event "inside" "2026-03-29T12:00:00Z"
                outside = event "outside" "2030-01-01T00:00:00Z"
                timeline = { initial | events = [ inside, outside ], histogramSelectedBucket = Just { label = bucket.label, since = since, until = until }, histogramBuckets = [ bucket ] }
                blocked = { timeline | error = Just "failed", eventsLoadedRequest = Nothing }
                retained = { blocked | eventsLoadedRequest = Just { workspaceId = "w", since = Just since, until = Just until } }
            in
            Expect.all
                [ \_ -> Timeline.timestampSources timeline |> Set.fromList |> Expect.equal (Set.fromList [ since, until, inside.occurredAt ])
                , \_ -> Timeline.visibleTimelineEvents timeline |> List.map .id |> Expect.equal [ "inside" ]
                , \_ -> Timeline.timestampSources blocked |> Expect.equal [ since, until ]
                , \_ -> Timeline.timestampSources retained |> List.member inside.occurredAt |> Expect.equal True
                ] ()
        , test "new canonical pages and refresh values enter current event demand without changing their source strings" <| \_ ->
            let
                initial = Timeline.init
                request = { workspaceId = "w", since = Nothing, until = Nothing }
                identity = { requestId = 1, refreshGeneration = 0, refreshEpoch = 0 }
                timeline = { initial | eventsActiveRequest = Just request, eventsActiveIdentity = Just identity, loading = True, loadingWorkspaceId = Just "w" }
                incoming = event "new" "2026-01-01T00:15:00Z"
                accepted = Timeline.update (GotWorkspaceTimeline identity request (Ok { items = [ incoming ], hasMore = True })) (model timeline) |> Tuple.first
            in
            Expect.all
                [ \_ -> Timeline.timestampSources accepted.timeline |> List.member incoming.occurredAt |> Expect.equal True
                , \_ -> accepted.timeline.events |> Expect.equal [ incoming ]
                , \_ -> accepted.timeline.hasMore |> Expect.equal True
                ] ()
        , test "missing invalid and pending dates use truthful grouping keys" <| \_ ->
            Expect.equal [ "Unknown date", "Unknown date", "Date pending" ]
                [ Timeline.timelineDateKey display (event "missing" ""), Timeline.timelineDateKey display (event "invalid" "bad"), Timeline.timelineDateKey Display.empty (event "pending" since) ]
        ]
