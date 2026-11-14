module TaskGraphTest exposing (suite)

import Api
import Dict
import Expect
import Feature.TaskGraph as TaskGraph
import Json.Encode as Encode
import Set
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "isolated project task graph"
        [ test "subtree and direct incident endpoints preserve ownership and edge direction without external expansion" <|
            \_ ->
                case TaskGraph.acceptSnapshot guard fixture (TaskGraph.begin guard) |> .graph of
                    Nothing ->
                        Expect.fail "complete fixture did not become a graph"

                    Just graph ->
                        Expect.equal
                            { scoped = [ "a", "b", "c", "s1", "s2" ]
                            , external = [ "incoming", "outgoing" ]
                            , projects = [ "child", "external", "grandchild", "p" ]
                            , edges = [ ( "a", "b" ), ( "b", "outgoing" ), ( "incoming", "a" ), ( "incoming", "b" ), ( "s1", "s2" ) ]
                            , ownership = Just ( Just "child", Nothing, False )
                            , subtask = Just ( Just "a", Api.InProgress )
                            }
                            { scoped = Set.toList graph.inScopeTaskIds
                            , external = Set.toList graph.externalTaskIds
                            , projects = Dict.keys graph.projects
                            , edges = List.map (\edge -> ( edge.prerequisiteId, edge.dependentId )) graph.edges
                            , ownership = Dict.get "b" graph.tasks |> Maybe.map (\task -> ( task.projectId, task.parentId, task.external ))
                            , subtask = Dict.get "s2" graph.tasks |> Maybe.map (\task -> ( task.parentId, task.status ))
                            }
        , test "empty selected projects remain a complete empty graph" <|
            \_ ->
                let
                    state = TaskGraph.acceptSnapshot guard [ workspaceItem, projectItem "p" Nothing ] (TaskGraph.begin guard)
                in
                Expect.equal ( TaskGraph.Ready, Just ( [], [] ) )
                    ( state.status, state.graph |> Maybe.map (\graph -> ( Dict.keys graph.tasks, graph.edges )) )
        , test "missing selected project is unavailable rather than a fabricated empty result" <|
            \_ ->
                let
                    state = TaskGraph.acceptSnapshot guard [ workspaceItem ] (TaskGraph.begin guard)
                in
                Expect.equal ( TaskGraph.Failed "selected graph project is unavailable", Nothing ) ( state.status, state.graph )
        , test "unassigned direct external endpoints retain their actual lack of project" <|
            \_ ->
                let
                    state = TaskGraph.acceptSnapshot guard
                        [ workspaceItem, projectItem "p" Nothing, taskItem "a" (Just "p") Nothing "todo", taskItem "unassigned" Nothing Nothing "todo", dependency "a" "unassigned" ]
                        (TaskGraph.begin guard)
                in
                Expect.equal (Just ( Just Nothing, [ "p" ] ))
                    (state.graph |> Maybe.map (\graph -> ( Dict.get "unassigned" graph.tasks |> Maybe.map .projectId, Dict.keys graph.projects )))
        , test "missing dependency endpoint, owning project or parent never becomes ready" <|
            \_ ->
                let
                    variants =
                        [ [ workspaceItem, projectItem "p" Nothing, taskItem "a" (Just "p") Nothing "todo", dependency "a" "missing" ]
                        , [ workspaceItem, projectItem "p" Nothing, taskItem "a" (Just "p") (Just "missing") "todo" ]
                        , [ workspaceItem, projectItem "p" Nothing, taskItem "a" (Just "p") Nothing "todo", taskItem "x" (Just "missing") Nothing "todo", dependency "a" "x" ]
                        ]
                    rejected items =
                        let
                            state = TaskGraph.acceptSnapshot guard items (TaskGraph.begin guard)
                        in
                        state.graph == Nothing && state.status /= TaskGraph.Ready
                in
                Expect.equal [ True, True, True ] (List.map rejected variants)
        , test "selected project cycles fail without recursive traversal" <|
            \_ ->
                let
                    state = TaskGraph.acceptSnapshot guard [ workspaceItem, projectItem "p" (Just "child"), projectItem "child" (Just "p") ] (TaskGraph.begin guard)
                in
                Expect.equal (TaskGraph.Failed "project hierarchy contains a cycle") state.status
        , test "workspace, project, audience, session and target stale replies cannot publish or fail current state" <|
            \_ ->
                let
                    stale =
                        [ { guard | workspaceId = "other" }, { guard | projectId = "other" }, { guard | audienceId = "other" }, { guard | sessionEpoch = 9 }, { guard | targetGeneration = 9 } ]
                    state = TaskGraph.begin guard
                    unchanged value =
                        TaskGraph.acceptSnapshot value fixture state == state && TaskGraph.fail value "stale" state == state
                in
                Expect.equal [ True, True, True, True, True ] (List.map unchanged stale)
        , test "retirement and a fresh generation remove old data and ignore delayed completion" <|
            \_ ->
                let
                    ready = TaskGraph.acceptSnapshot guard fixture (TaskGraph.begin guard)
                    retired = TaskGraph.retire ready
                    fresh = TaskGraph.begin { guard | targetGeneration = 5 }
                in
                Expect.equal ( TaskGraph.init, fresh, Nothing )
                    ( TaskGraph.acceptSnapshot guard fixture retired, TaskGraph.acceptSnapshot guard fixture fresh, fresh.graph )
        , test "failure is explicit and cannot accept a late success in the same retired request" <|
            \_ ->
                let
                    failed = TaskGraph.fail guard "transport_timeout" (TaskGraph.begin guard)
                in
                Expect.equal ( TaskGraph.Failed "transport_timeout", Nothing, failed )
                    ( failed.status, failed.graph, TaskGraph.acceptSnapshot guard fixture failed )
        , test "foreign workspace and duplicate identities are rejected by canonical full snapshot validation" <|
            \_ ->
                let
                    duplicate = TaskGraph.acceptSnapshot guard (workspaceItem :: fixture) (TaskGraph.begin guard)
                    foreign = TaskGraph.acceptSnapshot { guard | workspaceId = "other" } fixture (TaskGraph.begin { guard | workspaceId = "other" })
                in
                Expect.equal ( ( Nothing, Nothing ), ( False, False ) )
                    ( ( duplicate.graph, foreign.graph ), ( duplicate.status == TaskGraph.Ready, foreign.status == TaskGraph.Ready ) )
        , test "invalid request identity never leaves an apparently loading request" <|
            \_ ->
                Expect.equal ( TaskGraph.Failed "invalid graph request", Nothing )
                    ( (TaskGraph.begin { guard | audienceId = "" }).status, (TaskGraph.begin { guard | sessionEpoch = -1 }).guard )
        , test "ordinary refresh keeps the previous graph until a complete replacement and increments only snapshot revision" <|
            \_ ->
                let
                    ready = TaskGraph.acceptSnapshot guard fixture (TaskGraph.begin guard) |> TaskGraph.markLive guard
                    refreshing = TaskGraph.refresh guard ready
                    next = TaskGraph.acceptSnapshot guard [ workspaceItem, projectItem "p" Nothing, taskItem "new" (Just "p") Nothing "done" ] refreshing
                in
                Expect.equal
                    { retained = ready.graph, status = TaskGraph.Refreshing, live = False, tasks = Just [ "new" ], revision = ready.revision + 1 }
                    { retained = refreshing.graph, status = refreshing.status, live = refreshing.live, tasks = next.graph |> Maybe.map (.tasks >> Dict.keys), revision = next.revision }
        , test "refresh failure retains stale graph and cannot become live while a read is pending" <|
            \_ ->
                let
                    ready = TaskGraph.acceptSnapshot guard fixture (TaskGraph.begin guard) |> TaskGraph.markLive guard
                    pending = TaskGraph.refresh guard ready |> TaskGraph.markLive guard
                    failed = TaskGraph.fail guard "read failed" pending
                in
                Expect.equal ( ( ready.graph, False, TaskGraph.Refreshing ), ( ready.graph, False, TaskGraph.Stale "read failed" ) )
                    ( ( pending.graph, pending.live, pending.status ), ( failed.graph, failed.live, failed.status ) )
        , test "authoritative project deletion and permission retirement clear stale sensitive data" <|
            \_ ->
                let
                    ready = TaskGraph.acceptSnapshot guard fixture (TaskGraph.begin guard)
                    deleted = TaskGraph.acceptSnapshot guard [ workspaceItem ] (TaskGraph.refresh guard ready)
                    revoked = TaskGraph.invalidate guard "revoked" ready
                in
                Expect.equal ( ( Nothing, TaskGraph.Failed "selected graph project is unavailable" ), ( Nothing, False ) )
                    ( ( deleted.graph, deleted.status ), ( revoked.graph, revoked.live ) )
        , test "semantic snapshot rejection survives checkpoints and reconnect until a valid snapshot is accepted" <|
            \_ ->
                let
                    ready = TaskGraph.acceptSnapshot guard fixture (TaskGraph.begin guard) |> TaskGraph.markLive guard
                    invalid = [ workspaceItem, projectItem "p" Nothing, taskItem "a" (Just "p") Nothing "todo", dependency "a" "missing" ]
                    rejected = TaskGraph.acceptSnapshot guard invalid (TaskGraph.refresh guard ready)
                    checkpoint = TaskGraph.markLive guard rejected
                    reconnect = checkpoint |> TaskGraph.recover guard |> TaskGraph.markLive guard
                    accepted = reconnect |> TaskGraph.refresh guard |> TaskGraph.acceptSnapshot guard fixture |> TaskGraph.markLive guard
                    ordinaryReconnect = ready |> TaskGraph.recover guard |> TaskGraph.markLive guard
                in
                Expect.all
                    [ \_ -> Expect.equal ( rejected.graph, rejected.status, False ) ( reconnect.graph, reconnect.status, reconnect.live )
                    , \_ -> Expect.equal rejected checkpoint
                    , \_ -> Expect.equal ( TaskGraph.Ready, True, ready.graph ) ( accepted.status, accepted.live, accepted.graph )
                    , \_ -> Expect.equal ( TaskGraph.Ready, True ) ( ordinaryReconnect.status, ordinaryReconnect.live )
                    ] ()
        ]


guard : TaskGraph.Guard
guard =
    { workspaceId = "w", projectId = "p", audienceId = "reader", sessionEpoch = 3, targetGeneration = 4 }


fixture : List Api.SnapshotItem
fixture =
    [ workspaceItem, projectItem "p" Nothing, projectItem "child" (Just "p"), projectItem "grandchild" (Just "child"), projectItem "external" Nothing
    , taskItem "a" (Just "p") Nothing "blocked", taskItem "b" (Just "child") Nothing "todo", taskItem "c" (Just "grandchild") Nothing "done"
    , taskItem "s1" (Just "p") (Just "a") "done", taskItem "s2" (Just "p") (Just "a") "in_progress"
    , taskItem "incoming" (Just "external") Nothing "done", taskItem "outgoing" (Just "external") Nothing "blocked", taskItem "remote" (Just "external") Nothing "todo"
    , dependency "b" "a", dependency "a" "incoming", dependency "b" "incoming", dependency "outgoing" "b", dependency "s2" "s1"
    , dependency "remote" "incoming", dependency "outgoing" "incoming"
    ]


workspaceItem : Api.SnapshotItem
workspaceItem =
    { kind = "workspace", data = Encode.object ([ ( "id", Encode.string "w" ), ( "name", Encode.string "Workspace" ), ( "workspace_type", Encode.string "repository" ) ] ++ timestamps) }


projectItem : String -> Maybe String -> Api.SnapshotItem
projectItem identity parent =
    { kind = "project", data = Encode.object
        ([ ( "id", Encode.string identity ), ( "workspace_id", Encode.string "w" ), ( "name", Encode.string identity ), ( "parent_id", maybeString parent ), ( "status", Encode.string "active" ), ( "priority", Encode.int 5 ) ] ++ timestamps) }


taskItem : String -> Maybe String -> Maybe String -> String -> Api.SnapshotItem
taskItem identity projectId parent status =
    { kind = "task", data = Encode.object
        ([ ( "id", Encode.string identity ), ( "workspace_id", Encode.string "w" ), ( "title", Encode.string identity ), ( "project_id", maybeString projectId ), ( "parent_id", maybeString parent ), ( "status", Encode.string status ), ( "priority", Encode.int 5 ) ] ++ timestamps) }


dependency : String -> String -> Api.SnapshotItem
dependency dependent prerequisite =
    { kind = "task_dependency", data = Encode.object [ ( "task_id", Encode.string dependent ), ( "depends_on_id", Encode.string prerequisite ) ] }


maybeString : Maybe String -> Encode.Value
maybeString =
    Maybe.map Encode.string >> Maybe.withDefault Encode.null


timestamps : List ( String, Encode.Value )
timestamps =
    [ ( "created_at", Encode.string "2026-01-01T00:00:00Z" ), ( "updated_at", Encode.string "2026-01-01T00:00:00Z" ) ]
