module CardsPlanningCountsTest exposing (suite)

import Api
import AppShell
import Dict
import Expect
import Feature.Cards as Cards
import Feature.DataLoading as DataLoading
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Types exposing (Flags, Model, Page(..), WorkspaceTab(..))
import Url


suite : Test
suite =
    describe "planning count presentation"
        [ test "two tasks sharing a prerequisite show two blocked tasks and two dependencies" <| \_ ->
            let
                base = project "shared"
                rollup = base.readinessRollup
                counted = { base | readinessRollup = { rollup | openTaskCount = 3, dependencyBlockedTaskCount = 2, openDependencyCount = 2 } }
                seeded = DataLoading.mergeNavigationSummaries [ counted ] (List.map (taskIn counted.id) [ "a", "b", "prerequisite" ]) model
                dependencies = seeded.dependencies
                linked = { seeded | dependencies = { dependencies | taskDependencyLinks = [ { taskId = "a", dependsOnId = "prerequisite" }, { taskId = "b", dependsOnId = "prerequisite" } ] } }
            in
            Expect.all
                [ \_ -> cardSummary counted.id linked |> Query.has [ Selector.text "3 tasks", Selector.text "2 blocked tasks", Selector.text "2 open dependencies" ]
                , \_ -> Cards.cardTreeProjection workspaceId linked |> .taskDirectOpenDependencyCounts |> Expect.equal (Dict.fromList [ ( "a", 1 ), ( "b", 1 ) ])
                ] ()
        , test "one task with several prerequisites counts dependencies separately from blocked tasks" <| \_ ->
            let
                base = project "multiple"
                rollup = base.readinessRollup
                counted = { base | readinessRollup = { rollup | openTaskCount = 4, dependencyBlockedTaskCount = 1, openDependencyCount = 3 } }
                seeded = DataLoading.mergeNavigationSummaries [ counted ] (List.map (taskIn counted.id) [ "a", "p1", "p2", "p3" ]) model
                dependencies = seeded.dependencies
                linked = { seeded | dependencies = { dependencies | taskDependencyLinks = List.map (\id -> { taskId = "a", dependsOnId = id }) [ "p1", "p2", "p3" ] } }
            in
            Expect.all
                [ \_ -> cardSummary counted.id linked |> Query.has [ Selector.text "4 tasks", Selector.text "1 blocked task", Selector.text "3 open dependencies" ]
                , \_ -> Cards.cardTreeProjection workspaceId linked |> .taskDirectOpenDependencyCounts |> Expect.equal (Dict.singleton "a" 3)
                ] ()
        , test "ancestor and child cards retain overlapping canonical subtree counts" <| \_ ->
            let
                parentBase = project "ancestor"
                childBase = project "child"
                parentRollup = parentBase.readinessRollup
                childRollup = childBase.readinessRollup
                parent = { parentBase | readinessRollup = { parentRollup | openProjectCount = 1, openTaskCount = 3 } }
                child = { childBase | parentId = Just parent.id, readinessRollup = { childRollup | openTaskCount = 2 } }
                seeded = DataLoading.mergeNavigationSummaries [ parent, child ] [] model
            in
            Expect.all
                [ \_ -> cardSummary parent.id seeded |> Query.has [ Selector.text "3 tasks" ]
                , \_ -> cardSummary child.id seeded |> Query.has [ Selector.text "2 tasks" ]
                ] ()
        , describe "project planning totals ignore cancelled counts"
            (List.concatMap projectPlanningTests planningCases)
        , describe "subtask planning totals ignore cancelled counts"
            (List.concatMap subtaskPlanningTests planningCases)
        , test "mixed open and terminal subtasks show remaining work" <| \_ ->
            let
                base = task "parent"
                rollup = base.readinessRollup
                parent = { base | readinessRollup = { rollup | openSubtaskCount = 1, doneSubtaskCount = 1, cancelledSubtaskCount = 1 } }
            in
            DataLoading.mergeNavigationSummaries [] [ parent ] model
                |> cardSummary parent.id
                |> Query.has [ Selector.text "1/2 subtasks remaining" ]
        , test "canonical fallback survives readiness reset, collapsed branches and missing task details" <| \_ ->
            let
                base = project "fallback"
                rollup = base.readinessRollup
                counted = { base | readinessRollup = { rollup | openTaskCount = 60, doneTaskCount = 10, cancelledTaskCount = 5, dependencyBlockedTaskCount = 2, openDependencyCount = 3 } }
                seeded = DataLoading.mergeNavigationSummaries [ counted ] [] model
                dependencies = seeded.dependencies
                cards = seeded.cards
                recovering = { seeded | dependencies = { dependencies | projectReadinessRollups = Dict.empty }, cards = { cards | collapsedNodes = Dict.singleton "proj-fallback" True }, tasks = Dict.empty }
            in
            cardSummary counted.id recovering |> Query.has [ Selector.text "60/70 tasks remaining", Selector.text "2 blocked tasks", Selector.text "3 open dependencies" ]
        , test "readiness overrides stale navigation counts for projects and tasks" <| \_ ->
            let
                p = project "authority-project"
                t = task "authority-task"
                pr = p.readinessRollup
                tr = t.readinessRollup
                seeded = DataLoading.mergeNavigationSummaries [ p ] [ t ] model
                deps = seeded.dependencies
                authoritative = { seeded | dependencies = { deps | projectReadinessRollups = Dict.singleton p.id { pr | openTaskCount = 2, doneTaskCount = 3, cancelledTaskCount = 4 }, taskReadinessRollups = Dict.singleton t.id { tr | openSubtaskCount = 2, doneSubtaskCount = 3, cancelledSubtaskCount = 4 } } }
            in
            Expect.all
                [ \_ -> cardSummary p.id authoritative |> Query.has [ Selector.text "2/5 tasks remaining" ]
                , \_ -> cardSummary t.id authoritative |> Query.has [ Selector.text "2/5 subtasks remaining" ]
                ] ()
        , describe "loaded child fallback counts Done explicitly"
            (List.map loadedSubtaskTest
                [ ( "mixed", [ Api.Todo, Api.InProgress, Api.Done, Api.Done, Api.Done, Api.Cancelled, Api.Cancelled ], "2/5 subtasks remaining" )
                , ( "done and cancelled", [ Api.Done, Api.Done, Api.Done, Api.Cancelled ], "3 subtasks (all done)" )
                , ( "cancelled only", [ Api.Cancelled, Api.Cancelled ], "" )
                ])
        , test "cancelled children remain in deletion impact counts" <| \_ ->
            let
                parent = Api.taskFromCardSummary (task "delete-parent")
                baseChild = Api.taskFromCardSummary (task "delete-child")
                cancelled = { baseChild | parentId = Just parent.id, status = Api.Cancelled }
            in
            Cards.taskCascadePreview parent.id [ parent, cancelled ]
                |> Maybe.map .taskCount
                |> Expect.equal (Just 2)
        , test "known zero remains visible without inventing dependency blockers" <| \_ ->
            let
                seeded = DataLoading.mergeNavigationSummaries [ project "empty" ] [] model
            in
            Expect.all
                [ \_ -> cardSummary "empty" seeded |> Query.has [ Selector.text "0 tasks" ]
                , \_ -> cardSummary "empty" seeded |> Query.hasNot [ Selector.text "blocked task" ]
                ] ()
        , test "missing authoritative totals stay unavailable" <| \_ ->
            let
                base = project "unknown"
                source = { model | projects = Dict.singleton base.id (Api.projectFromCardSummary base) }
            in
            Expect.all
                [ \_ -> cardSummary base.id source |> Query.has [ Selector.text "Task counts unavailable" ]
                , \_ -> cardSummary base.id source |> Query.hasNot [ Selector.text "0 tasks" ]
                ] ()
        , test "loading membership preserves canonical counts" <| \_ ->
            let
                base = project "loading"
                rollup = base.readinessRollup
                counted = { base | readinessRollup = { rollup | openTaskCount = 12 } }
                seeded = DataLoading.mergeNavigationSummaries [ counted ] [] model
                requested = DataLoading.beginNavigationBranch "project" workspaceId (Just counted.id) seeded |> Tuple.first
            in
            Expect.all
                [ \_ -> cardSummary counted.id requested |> Query.has [ Selector.text "12 tasks" ]
                , \_ -> Cards.viewProjectsTree workspaceId requested |> Query.fromHtml |> Query.has [ Selector.text "Loading more…" ]
                ] ()
        ]


planningCases : List ( String, ( Int, Int ), String )
planningCases =
    [ ( "mixed", ( 2, 3 ), "2/5" )
    , ( "done only", ( 0, 3 ), "3" )
    , ( "open only", ( 2, 0 ), "2" )
    , ( "empty", ( 0, 0 ), "0" )
    , ( "one done", ( 0, 1 ), "1" )
    ]


projectPlanningTests : ( String, ( Int, Int ), String ) -> List Test
projectPlanningTests ( label, ( open, done ), number ) =
    List.map (\cancelled -> test (label ++ ", cancelled=" ++ String.fromInt cancelled) <| \_ ->
        let
            base = project "closed-project"
            rollup = base.readinessRollup
            counted = { base | readinessRollup = { rollup | closedProjectCount = 2, openTaskCount = open, doneTaskCount = done, cancelledTaskCount = cancelled } }
            seeded = DataLoading.mergeNavigationSummaries [ counted ] [] model
            expected = number ++ (if open + done == 1 then " task" else " tasks") ++ (if open > 0 && done > 0 then " remaining" else if done > 0 then " (all done)" else "")
        in
        Expect.all
            [ \_ -> cardSummary base.id seeded |> Query.has [ Selector.text ("2 subprojects (all closed) · " ++ expected) ]
            , \_ -> counted.readinessRollup.cancelledTaskCount |> Expect.equal cancelled
            ] ()) [ 0, 4 ]


subtaskPlanningTests : ( String, ( Int, Int ), String ) -> List Test
subtaskPlanningTests ( label, ( open, done ), number ) =
    List.map (\cancelled -> test (label ++ ", cancelled=" ++ String.fromInt cancelled) <| \_ ->
        let
            base = task "closed-parent"
            rollup = base.readinessRollup
            counted = { base | dependencyCount = 1, readinessRollup = { rollup | openSubtaskCount = open, doneSubtaskCount = done, cancelledSubtaskCount = cancelled } }
            seeded = DataLoading.mergeNavigationSummaries [] [ counted ] model
            deps = seeded.dependencies
            cards = seeded.cards
            recovering = { seeded | dependencies = { deps | taskReadinessRollups = Dict.empty }, cards = { cards | collapsedNodes = Dict.singleton ("task-" ++ base.id) True } }
            expected = number ++ (if open + done == 1 then " subtask" else " subtasks") ++ (if open > 0 && done > 0 then " remaining" else if done > 0 then " (all done)" else "")
            summary = if open + done == 0 then "1 dependency" else expected ++ " · 1 dependency"
        in
        Expect.all
            [ \_ -> cardSummary base.id seeded |> Query.has [ Selector.text summary ]
            , \_ -> cardSummary base.id recovering |> Query.has [ Selector.text summary ]
            ] ()) [ 0, 4 ]


loadedSubtaskTest : ( String, List Api.TaskStatus, String ) -> Test
loadedSubtaskTest ( label, statuses, expected ) =
    test label <| \_ ->
        let
            parent = Api.taskFromCardSummary (task "loaded-parent")
            child index status =
                let
                    base = Api.taskFromCardSummary (task ("child-" ++ String.fromInt index))
                in
                { base | parentId = Just parent.id, status = status }
            source = { model | tasks = Dict.fromList (List.map (\value -> ( value.id, value )) (parent :: List.indexedMap child statuses)) }
            view = Cards.viewProjectsTree workspaceId source |> Query.fromHtml |> Query.find [ Selector.id ("entity-" ++ parent.id) ]
        in
        if String.isEmpty expected then
            view |> Query.hasNot [ Selector.class "card-summary" ]
        else
            view |> Query.find [ Selector.class "card-summary" ] |> Query.has [ Selector.text expected ]


cardSummary : String -> Model -> Query.Single Types.Msg
cardSummary id source =
    Cards.viewProjectsTree workspaceId source
        |> Query.fromHtml
        |> Query.find [ Selector.id ("entity-" ++ id) ]
        |> Query.find [ Selector.class "card-summary" ]


workspaceId : String
workspaceId =
    "workspace-1"


model : Model
model =
    AppShell.initModel Nothing url (WorkspacePage workspaceId) flags Nothing { tab = ProjectsTab, focus = Nothing, observationId = Nothing }
        |> AppShell.finalizeInit (WorkspacePage workspaceId)
        |> (\initial -> let cards = initial.cards in { initial | cards = { cards | nodesCollapsedByDefault = False } })


flags : Flags
flags =
    { apiUrl = "https://api.example", wsUrl = "wss://api.example", sessionId = "session-1", runtimeMode = "test", authTokenStorageKey = "hmem-auth-token", authTokenPresent = False, loginUrl = Nothing, logoutUrl = Nothing }


url : Url.Url
url =
    { protocol = Url.Https, host = "app.example", port_ = Nothing, path = "/workspace/workspace-1", query = Nothing, fragment = Nothing }


project : String -> Api.ProjectCardSummary
project id =
    { id = id, workspaceId = workspaceId, parentId = Nothing, name = id, status = Api.ProjActive, priority = 1, createdAt = "2026-01-01T00:00:00Z", updatedAt = "2026-01-01T00:00:00Z", directProjectCount = 0, directTaskCount = 0, hasChildren = True
    , readinessRollup = { openProjectCount = 0, inProgressTaskCount = 0, closedProjectCount = 0, openTaskCount = 0, doneTaskCount = 0, cancelledTaskCount = 0, blockedTaskCount = 0, dependencyBlockedTaskCount = 0, openDependencyCount = 0, completionReady = True }
    }


task : String -> Api.TaskCardSummary
task id =
    { id = id, workspaceId = workspaceId, projectId = Nothing, parentId = Nothing, title = id, status = Api.Todo, priority = 1, completedAt = Nothing, dependencyCount = 0, createdAt = "2026-01-01T00:00:00Z", updatedAt = "2026-01-01T00:00:00Z", directSubtaskCount = 0, hasChildren = False
    , readinessRollup = { openSubtaskCount = 0, doneSubtaskCount = 0, cancelledSubtaskCount = 0, blockedSubtaskCount = 0, dependencyBlockedTaskCount = 0, openDependencyCount = 0, completionReady = True, inProgressSubtaskCount = 0, canStartSubtasks = False, ownOpenDependencyCount = Just 0 }
    }


taskIn : String -> String -> Api.TaskCardSummary
taskIn projectId id =
    let
        base = task id
    in
    { base | projectId = Just projectId }
