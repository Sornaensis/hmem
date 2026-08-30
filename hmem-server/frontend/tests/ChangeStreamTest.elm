module ChangeStreamTest exposing (suite)

import Api
import Dict
import Expect
import Feature.ChangeStream as ChangeStream
import Json.Encode as Encode
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "canonical change stream policy"
        [ test "deduplicates and ignores wrong scope events" <|
            \_ ->
                let
                    state =
                        ChangeStream.init (ChangeStream.Workspace "a") []

                    event =
                        { eventId = "event", scope = Api.WorkspaceScope "a", workspaceId = Just "a", entityType = "task", entityId = "t", entityAction = "updated", invalidations = [ { kind = "entity", target = "task:t" } ] }

                    wrong =
                        { event | scope = Api.WorkspaceScope "b" }
                in
                [ ChangeStream.reduceFrame (Api.CanonicalChange event) state |> Tuple.second
                , ChangeStream.reduceFrame (Api.CanonicalChange event) state |> Tuple.first |> ChangeStream.reduceFrame (Api.CanonicalChange event) |> Tuple.second
                , ChangeStream.reduceFrame (Api.CanonicalChange wrong) state |> Tuple.second
                ]
                    |> Expect.equal [ [ ChangeStream.RefetchEntity "task" "t", ChangeStream.RefreshTimeline ], [], [] ]
        , test "checkpoint becomes live without mutation actions" <|
            \_ ->
                let
                    ( state, actions ) =
                        ChangeStream.reduceFrame (Api.CanonicalCheckpoint "opaque") (ChangeStream.init ChangeStream.Global [])
                in
                Expect.equal ( True, Just "opaque", [] ) ( state.live, state.resumeToken, actions )
        , test "Timeline eligibility is one coalesced action for lifecycle envelopes only" <|
            \_ ->
                let
                    state =
                        ChangeStream.init (ChangeStream.Workspace "w") []

                    actions entityType entityAction invalidations =
                        ChangeStream.reduceFrame
                            (Api.CanonicalChange
                                { eventId = entityType ++ "-" ++ entityAction
                                , scope = Api.WorkspaceScope "w"
                                , workspaceId = Just "w"
                                , entityType = entityType
                                , entityId = "id"
                                , entityAction = entityAction
                                , invalidations = invalidations
                                }
                            )
                            state
                            |> Tuple.second
                            |> List.filter ((==) ChangeStream.RefreshTimeline)
                in
                [ actions "project" "created" [ { kind = "entity", target = "project:id" }, { kind = "collection", target = "projects:w" } ]
                , actions "project" "updated" [ { kind = "entity", target = "project:id" } ]
                , actions "project" "deleted" [ { kind = "entity", target = "project:id" } ]
                , actions "project" "restored" [ { kind = "entity", target = "project:id" } ]
                , actions "task" "created" [ { kind = "entity", target = "task:id" } ]
                , actions "task" "updated" [ { kind = "entity", target = "task:id" } ]
                , actions "task" "deleted" [ { kind = "entity", target = "task:id" } ]
                , actions "task" "restored" [ { kind = "entity", target = "task:id" } ]
                , actions "observation" "created" [ { kind = "entity", target = "observation:id" } ]
                , actions "observation" "deleted" [ { kind = "entity", target = "observation:id" } ]
                , actions "observation" "updated" [ { kind = "entity", target = "observation:id" } ]
                , actions "task_dependency" "updated" [ { kind = "entity", target = "task_dependency:id:other" } ]
                ]
                    |> Expect.equal
                        [ [ ChangeStream.RefreshTimeline ]
                        , [ ChangeStream.RefreshTimeline ]
                        , [ ChangeStream.RefreshTimeline ]
                        , [ ChangeStream.RefreshTimeline ]
                        , [ ChangeStream.RefreshTimeline ]
                        , [ ChangeStream.RefreshTimeline ]
                        , [ ChangeStream.RefreshTimeline ]
                        , [ ChangeStream.RefreshTimeline ]
                        , [ ChangeStream.RefreshTimeline ]
                        , [ ChangeStream.RefreshTimeline ]
                        , []
                        , []
                        ]
        , test "coalesces a burst and fails closed for an unknown invalidation" <|
            \_ ->
                let
                    state =
                        ChangeStream.init (ChangeStream.Workspace "a") []

                    duplicate =
                        { kind = "entity", target = "project:p" }

                    event =
                        { eventId = "event", scope = Api.WorkspaceScope "a", workspaceId = Just "a", entityType = "project", entityId = "p", entityAction = "updated", invalidations = [ duplicate, duplicate ] }

                    unknown =
                        { event | eventId = "next", invalidations = [ { kind = "unrecognised", target = "project:p" } ] }

                    deleted =
                        { event | eventId = "deleted", entityAction = "deleted", invalidations = [ duplicate, { kind = "readiness", target = "project:p" }, { kind = "entity", target = "task:t" } ] }

                    groupMembership =
                        { event | eventId = "group-membership", entityType = "workspace_group_membership", entityId = "g:a", entityAction = "created", invalidations = [ { kind = "entity", target = "group_membership:g:a" }, { kind = "collection", target = "group:g:members" }, { kind = "collection", target = "workspace:a:groups" } ] }

                    workspaceMembership =
                        { event | eventId = "workspace-membership", entityType = "workspace_membership", entityId = "a:u", entityAction = "updated", invalidations = [ { kind = "collection", target = "workspace:a:memberships" } ] }
                in
                [ ChangeStream.reduceFrame (Api.CanonicalChange event) state |> Tuple.second
                , ChangeStream.reduceFrame (Api.CanonicalChange unknown) state |> Tuple.second
                , ChangeStream.reduceFrame (Api.CanonicalChange deleted) state |> Tuple.second
                , ChangeStream.reduceFrame (Api.CanonicalChange groupMembership) state |> Tuple.second
                , ChangeStream.reduceFrame (Api.CanonicalChange workspaceMembership) state |> Tuple.second
                ]
                    |> Expect.equal [ [ ChangeStream.RefetchEntity "project" "p", ChangeStream.RefreshTimeline ], [ ChangeStream.BeginResync ], [ ChangeStream.RemoveEntity "project" "p", ChangeStream.RefreshReadiness "project" "p", ChangeStream.RefetchEntity "task" "t", ChangeStream.RefreshTimeline ], [ ChangeStream.RefreshGroupMembers "g", ChangeStream.NoAction ], [ ChangeStream.RefreshMemberships "a" ] ]
        , test "revocation clears only the current workspace scope" <|
            \_ ->
                let
                    state =
                        ChangeStream.init (ChangeStream.Workspace "a") []

                    actions =
                        ChangeStream.reduceFrame (Api.CanonicalAccessRevoked (Just "a")) state |> Tuple.second
                in
                Expect.equal [ ChangeStream.ClearWorkspace "a", ChangeStream.RefreshCatalogue, ChangeStream.RefreshSessionAuthorization ] actions
        , test "grant clears only the newly readable workspace bearer before authorization refresh" <|
            \_ ->
                let
                    state =
                        ChangeStream.init ChangeStream.Global []
                in
                ChangeStream.reduceFrame (Api.CanonicalAccessGranted "a") state
                    |> Tuple.second
                    |> Expect.equal [ ChangeStream.AccessGranted "a", ChangeStream.RefreshCatalogue, ChangeStream.RefreshSessionAuthorization ]
        , test "snapshot kinds are scope-validated before any replacement" <|
            \_ ->
                let
                    item =
                        { kind = "workspace_group", data = Encode.null }
                in
                ChangeStream.applySnapshot (ChangeStream.Workspace "a") [ item ]
                    |> Result.map (\_ -> ())
                    |> Expect.err
        , test "every V022 target shape is explicit and no-colon workspace-groups is handled" <|
            \_ ->
                let
                    workspaceState =
                        ChangeStream.init (ChangeStream.Workspace "w") []

                    workspaceEvent =
                        { eventId = "workspace-matrix"
                        , scope = Api.WorkspaceScope "w"
                        , workspaceId = Just "w"
                        , entityType = "task"
                        , entityId = "t"
                        , entityAction = "updated"
                        , invalidations =
                            [ { kind = "entity", target = "task:t" }
                            , { kind = "collection", target = "tasks:w" }
                            , { kind = "tree", target = "workspace:w" }
                            , { kind = "readiness", target = "task:t" }
                            , { kind = "readiness", target = "project:p" }
                            , { kind = "next_task", target = "workspace:w" }
                            , { kind = "search", target = "workspace:w" }
                            , { kind = "collection", target = "workspace:w:memberships" }
                            , { kind = "collection", target = "group:g:members" }
                            , { kind = "collection", target = "workspace:w:groups" }
                            , { kind = "catalogue", target = "workspace-catalog" }
                            , { kind = "session_authorization", target = "session-authorization" }
                            , { kind = "permission_cache", target = "permission-cache" }
                            ]
                        }

                    globalEvent =
                        { eventId = "global-matrix"
                        , scope = Api.GlobalScope
                        , workspaceId = Nothing
                        , entityType = "workspace"
                        , entityId = "w"
                        , entityAction = "updated"
                        , invalidations =
                            [ { kind = "entity", target = "workspace:w" }
                            , { kind = "catalogue", target = "workspace-catalog" }
                            , { kind = "collection", target = "workspace-groups" }
                            ]
                        }
                in
                [ ChangeStream.reduceFrame (Api.CanonicalChange workspaceEvent) workspaceState |> Tuple.second
                , ChangeStream.reduceFrame (Api.CanonicalChange globalEvent) (ChangeStream.init ChangeStream.Global []) |> Tuple.second
                ]
                    |> Expect.equal
                        [ [ ChangeStream.RefetchEntity "task" "t"
                          , ChangeStream.NoAction
                          , ChangeStream.RefreshTaskOverview "t"
                          , ChangeStream.RefreshReadiness "project" "p"
                          , ChangeStream.RefreshNextTasks "w"
                          , ChangeStream.RefreshSearch "w"
                          , ChangeStream.RefreshMemberships "w"
                          , ChangeStream.RefreshGroupMembers "g"
                          , ChangeStream.RefreshCatalogue
                          , ChangeStream.RefreshSessionAuthorization
                          , ChangeStream.RefreshTimeline
                          ]
                        , [ ChangeStream.RefetchEntity "workspace" "w", ChangeStream.RefreshCatalogue, ChangeStream.RefreshGroups ]
                        ]
        , test "unknown kind-target combinations fail closed without partial effects" <|
            \_ ->
                let
                    state =
                        ChangeStream.init (ChangeStream.Workspace "w") []

                    base =
                        { eventId = "bad"
                        , scope = Api.WorkspaceScope "w"
                        , workspaceId = Just "w"
                        , entityType = "task"
                        , entityId = "t"
                        , entityAction = "updated"
                        , invalidations = []
                        }

                    invalids =
                        [ [ { kind = "entity", target = "unknown:x" } ]
                        , [ { kind = "collection", target = "workspace-groups" } ]
                        , [ { kind = "collection", target = "tasks:other" } ]
                        , [ { kind = "tree", target = "project:p" } ]
                        , [ { kind = "next_task", target = "workspace:other" } ]
                        , [ { kind = "readiness", target = "workspace:w" } ]
                        , [ { kind = "catalogue", target = "wrong" } ]
                        , [ { kind = "entity", target = "task:" } ]
                        , [ { kind = "entity", target = "task:t" }, { kind = "future", target = "task:t" } ]
                        ]
                in
                invalids
                    |> List.indexedMap (\index invalidations -> ChangeStream.reduceFrame (Api.CanonicalChange { base | eventId = "bad-" ++ String.fromInt index, invalidations = invalidations }) state |> Tuple.second)
                    |> Expect.equal (List.repeat (List.length invalids) [ ChangeStream.BeginResync ])
        , test "coalesces duplicate targets across frames and checkpoint" <|
            \_ ->
                let
                    state =
                        ChangeStream.init (ChangeStream.Workspace "w") []

                    event eventId =
                        Api.CanonicalChange
                            { eventId = eventId
                            , scope = Api.WorkspaceScope "w"
                            , workspaceId = Just "w"
                            , entityType = "task"
                            , entityId = "t"
                            , entityAction = "updated"
                            , invalidations = [ { kind = "entity", target = "task:t" }, { kind = "next_task", target = "workspace:w" } ]
                            }

                    ( final, actions ) =
                        ChangeStream.reduceFrames [ event "one", event "two", Api.CanonicalCheckpoint "replacement" ] state
                in
                Expect.equal ( True, Just "replacement", [ ChangeStream.RefetchEntity "task" "t", ChangeStream.RefreshNextTasks "w", ChangeStream.RefreshTimeline ] ) ( final.live, final.resumeToken, actions )
        , test "entity transition coalescing preserves the final workspace, project, and task state" <|
            \_ ->
                let
                    workspace action eventId =
                        Api.CanonicalChange
                            { eventId = eventId
                            , scope = Api.GlobalScope
                            , workspaceId = Nothing
                            , entityType = "workspace"
                            , entityId = "w"
                            , entityAction = action
                            , invalidations = [ { kind = "entity", target = "workspace:w" } ]
                            }

                    entity kind identity action eventId =
                        Api.CanonicalChange
                            { eventId = eventId
                            , scope = Api.WorkspaceScope "w"
                            , workspaceId = Just "w"
                            , entityType = kind
                            , entityId = identity
                            , entityAction = action
                            , invalidations = [ { kind = "entity", target = kind ++ ":" ++ identity } ]
                            }

                    actions frames initial =
                        ChangeStream.reduceFrames frames initial |> Tuple.second
                in
                [ actions [ workspace "deleted" "wd", workspace "restored" "wr" ] (ChangeStream.init ChangeStream.Global [])
                , actions [ workspace "restored" "wr", workspace "deleted" "wd" ] (ChangeStream.init ChangeStream.Global [])
                , actions [ entity "project" "p" "deleted" "pd", entity "project" "p" "restored" "pr" ] (ChangeStream.init (ChangeStream.Workspace "w") [])
                , actions [ entity "project" "p" "restored" "pr", entity "project" "p" "deleted" "pd" ] (ChangeStream.init (ChangeStream.Workspace "w") [])
                , actions [ entity "task" "t" "deleted" "td", entity "task" "t" "restored" "tr" ] (ChangeStream.init (ChangeStream.Workspace "w") [])
                , actions [ entity "task" "t" "restored" "tr", entity "task" "t" "deleted" "td" ] (ChangeStream.init (ChangeStream.Workspace "w") [])
                ]
                    |> Expect.equal
                        [ [ ChangeStream.RefetchEntity "workspace" "w", ChangeStream.RefreshSessionAuthorization ]
                        , [ ChangeStream.RemoveEntity "workspace" "w", ChangeStream.ClearWorkspace "w", ChangeStream.RefreshSessionAuthorization ]
                        , [ ChangeStream.RefetchEntity "project" "p", ChangeStream.RefreshTimeline ]
                        , [ ChangeStream.RemoveEntity "project" "p", ChangeStream.RefreshTimeline ]
                        , [ ChangeStream.RefetchEntity "task" "t", ChangeStream.RefreshTimeline ]
                        , [ ChangeStream.RemoveEntity "task" "t", ChangeStream.RefreshTimeline ]
                        ]
        , test "dependency and readiness invalidations produce one normalized task-overview effect" <|
            \_ ->
                let
                    event action eventId =
                        Api.CanonicalChange
                            { eventId = eventId
                            , scope = Api.WorkspaceScope "w"
                            , workspaceId = Just "w"
                            , entityType = "task_dependency"
                            , entityId = "t:d"
                            , entityAction = action
                            , invalidations =
                                [ { kind = "entity", target = "task_dependency:t:d" }
                                , { kind = "readiness", target = "task:t" }
                                , { kind = "readiness", target = "task:t" }
                                , { kind = "entity", target = "task:t" }
                                ]
                            }
                in
                [ ChangeStream.reduceFrame (event "updated" "u") (ChangeStream.init (ChangeStream.Workspace "w") []) |> Tuple.second
                , ChangeStream.reduceFrame (event "deleted" "d") (ChangeStream.init (ChangeStream.Workspace "w") []) |> Tuple.second
                ]
                    |> Expect.equal
                        [ [ ChangeStream.RefreshTaskOverview "t", ChangeStream.RefetchEntity "task" "t" ]
                        , [ ChangeStream.RemoveEntity "task_dependency" "t:d", ChangeStream.RefreshTaskOverview "t", ChangeStream.RefetchEntity "task" "t" ]
                        ]
        , test "observation invalidations coalesce targeted and active-page reconciliation" <|
            \_ ->
                let
                    event action eventId =
                        Api.CanonicalChange
                            { eventId = eventId
                            , scope = Api.WorkspaceScope "w"
                            , workspaceId = Just "w"
                            , entityType = "observation"
                            , entityId = "o"
                            , entityAction = action
                            , invalidations =
                                [ { kind = "entity", target = "observation:o" }
                                , { kind = "collection", target = "observations:w" }
                                , { kind = "collection", target = "observations:w" }
                                ]
                            }

                    actions frames =
                        ChangeStream.reduceFrames frames (ChangeStream.init (ChangeStream.Workspace "w") []) |> Tuple.second
                in
                [ actions [ event "updated" "u" ]
                , actions [ event "updated" "u", event "deleted" "d" ]
                ]
                    |> Expect.equal
                        [ [ ChangeStream.RefetchEntity "observation" "o", ChangeStream.RefreshObservations ]
                        , [ ChangeStream.RemoveEntity "observation" "o", ChangeStream.RefreshObservations, ChangeStream.RefreshTimeline ]
                        ]
        , test "dedupe history is bounded and evicts the oldest accepted event" <|
            \_ ->
                let
                    final =
                        List.range 1 129
                            |> List.foldl
                                (\index state -> ChangeStream.acceptEvent (String.fromInt index) state |> Maybe.withDefault state)
                                (ChangeStream.init ChangeStream.Global [])
                in
                Expect.equal
                    { count = 128, oldestRetained = False, oldestCanBeAcceptedAgain = True, newestIsDuplicate = True }
                    { count = List.length final.eventIds
                    , oldestRetained = List.member "1" final.eventIds
                    , oldestCanBeAcceptedAgain = ChangeStream.acceptEvent "1" final /= Nothing
                    , newestIsDuplicate = ChangeStream.acceptEvent "129" final == Nothing
                    }
        , test "request guards reject stale target, route, auth epoch, and audience responses" <|
            \_ ->
                let
                    guard =
                        { scopeKey = "workspace:w"
                        , targetKey = "task-overview:t"
                        , targetGeneration = 4
                        , sessionEpoch = 7
                        , routeWorkspace = Just "w"
                        , audienceId = "actor-a"
                        }

                    generations =
                        Dict.fromList [ ( "workspace:w|task-overview:t", 4 ) ]
                in
                [ ChangeStream.requestGuardMatches guard 7 (Just "w") "actor-a" generations
                , ChangeStream.requestGuardMatches guard 7 (Just "w") "actor-a" (Dict.insert "workspace:w|task-overview:t" 5 generations)
                , ChangeStream.requestGuardMatches guard 7 (Just "other") "actor-a" generations
                , ChangeStream.requestGuardMatches guard 8 (Just "w") "actor-a" generations
                , ChangeStream.requestGuardMatches guard 7 (Just "w") "actor-b" generations
                ]
                    |> Expect.equal [ True, False, False, False, False ]
        , test "overlapping dependency and readiness responses share one generation fence" <|
            \_ ->
                let
                    oldGuard =
                        { scopeKey = "workspace:w"
                        , targetKey = "task-overview:t"
                        , targetGeneration = 4
                        , sessionEpoch = 7
                        , routeWorkspace = Just "w"
                        , audienceId = "actor"
                        }

                    newGuard =
                        { oldGuard | targetGeneration = 5 }

                    current =
                        Dict.fromList [ ( "workspace:w|task-overview:t", 5 ) ]
                in
                Expect.equal
                    { dependencyResponseAccepted = False, readinessResponseAccepted = True }
                    { dependencyResponseAccepted = ChangeStream.requestGuardMatches oldGuard 7 (Just "w") "actor" current
                    , readinessResponseAccepted = ChangeStream.requestGuardMatches newGuard 7 (Just "w") "actor" current
                    }
        , test "selected-workspace catalogue deletion explicitly removes and clears only that scope" <|
            \_ ->
                let
                    event =
                        { eventId = "workspace-deleted"
                        , scope = Api.GlobalScope
                        , workspaceId = Nothing
                        , entityType = "workspace"
                        , entityId = "w"
                        , entityAction = "deleted"
                        , invalidations =
                            [ { kind = "entity", target = "workspace:w" }
                            , { kind = "catalogue", target = "workspace-catalog" }
                            , { kind = "collection", target = "workspace-groups" }
                            ]
                        }
                in
                ChangeStream.reduceFrame (Api.CanonicalChange event) (ChangeStream.init ChangeStream.Global [])
                    |> Tuple.second
                    |> Expect.equal [ ChangeStream.RemoveEntity "workspace" "w", ChangeStream.ClearWorkspace "w", ChangeStream.RefreshSessionAuthorization, ChangeStream.RefreshCatalogue, ChangeStream.RefreshGroups ]
        , test "strict v1 decoder requires nested metadata, identities, and terminal tokens" <|
            \_ ->
                let
                    valid =
                        canonicalWire "restored" [ ( "entity", "task:t" ) ]

                    invalid =
                        [ "{\"schema_version\":1,\"type\":\"checkpoint\",\"catch_up\":\"complete\",\"resume_token\":\"\"}"
                        , "{\"schema_version\":1,\"type\":\"access_revoked\",\"workspace_id\":\"\"}"
                        , "{\"schema_version\":1,\"type\":\"change\",\"event\":{\"event_id\":\"e\",\"scope\":\"workspace\",\"workspace_id\":\"w\",\"entity\":{\"type\":\"task\",\"id\":\"t\",\"action\":\"updated\"},\"invalidations\":[{\"kind\":\"entity\",\"target\":\"task:t\"}]}}"
                        , "{\"schema_version\":1,\"type\":\"change\",\"event\":{\"schema_version\":1,\"event_id\":\"e\",\"scope\":\"global\",\"workspace_id\":\"other\",\"occurred_at\":\"now\",\"transaction\":{\"id\":\"tx\",\"cause\":\"rest\",\"request_id\":null},\"actor\":{\"type\":\"system\",\"id\":null},\"entity\":{\"type\":\"workspace\",\"id\":\"other\",\"action\":\"updated\"},\"invalidations\":[{\"kind\":\"entity\",\"target\":\"workspace:other\"}]}}"
                        , "{\"transport\":\"snapshot\",\"scope\":{\"scope\":\"global\"},\"items\":[],\"resume_token\":\"token\"}"
                        , "{\"schema_version\":1,\"transport\":\"snapshot\",\"scope\":{\"scope\":\"global\"},\"items\":[{\"schema_version\":1,\"kind\":\"future\",\"data\":{}}],\"resume_token\":\"token\"}"
                        ]
                in
                Expect.equal
                    { valid = True, invalid = List.repeat (List.length invalid) Nothing, failedScope = Just Api.GlobalScope }
                    { valid = Api.decodeCanonicalFrame valid /= Nothing
                    , invalid = List.map Api.decodeCanonicalFrame invalid
                    , failedScope = Api.decodeCanonicalTransportScope "{\"schema_version\":1,\"transport\":\"frames\",\"scope\":{\"scope\":\"global\"},\"frames\":[]}"
                    }
        , test "snapshot replacement validates dependency ownership after all pages are accumulated" <|
            \_ ->
                let
                    valid =
                        ChangeStream.applySnapshot (ChangeStream.Workspace "w")
                            [ workspaceItem "w", taskItem "a" "w", taskItem "b" "w", dependencyItem "a" "b" ]

                    missingTask =
                        ChangeStream.applySnapshot (ChangeStream.Workspace "w")
                            [ workspaceItem "w", taskItem "a" "w", dependencyItem "a" "missing" ]

                    duplicate =
                        ChangeStream.applySnapshot (ChangeStream.Workspace "w") [ workspaceItem "w", workspaceItem "w" ]
                in
                case valid of
                    Ok snapshot ->
                        Expect.equal
                            { workspaces = 1, tasks = 2, dependencies = 1, missingTaskRejected = True, duplicateRejected = True }
                            { workspaces = Dict.size snapshot.workspaces
                            , tasks = Dict.size snapshot.tasks
                            , dependencies = List.length snapshot.dependencies
                            , missingTaskRejected = Result.toMaybe missingTask == Nothing
                            , duplicateRejected = Result.toMaybe duplicate == Nothing
                            }

                    Err error ->
                        Expect.fail error
        ]


canonicalWire : String -> List ( String, String ) -> String
canonicalWire action invalidations =
    Encode.object
        [ ( "schema_version", Encode.int 1 )
        , ( "type", Encode.string "change" )
        , ( "event"
          , Encode.object
                [ ( "schema_version", Encode.int 1 )
                , ( "event_id", Encode.string "event" )
                , ( "scope", Encode.string "workspace" )
                , ( "workspace_id", Encode.string "w" )
                , ( "occurred_at", Encode.string "2026-01-01T00:00:00Z" )
                , ( "transaction", Encode.object [ ( "id", Encode.string "tx" ), ( "cause", Encode.string "rest" ), ( "request_id", Encode.null ) ] )
                , ( "actor", Encode.object [ ( "type", Encode.string "system" ), ( "id", Encode.null ) ] )
                , ( "entity", Encode.object [ ( "type", Encode.string "task" ), ( "id", Encode.string "t" ), ( "action", Encode.string action ) ] )
                , ( "invalidations", Encode.list (\( kind, target ) -> Encode.object [ ( "kind", Encode.string kind ), ( "target", Encode.string target ) ]) invalidations )
                ]
          )
        ]
        |> Encode.encode 0


workspaceItem : String -> Api.SnapshotItem
workspaceItem workspaceId =
    { kind = "workspace"
    , data =
        Encode.object
            [ ( "id", Encode.string workspaceId )
            , ( "name", Encode.string "Workspace" )
            , ( "workspace_type", Encode.string "repository" )
            , ( "created_at", Encode.string "2026-01-01T00:00:00Z" )
            , ( "updated_at", Encode.string "2026-01-01T00:00:00Z" )
            ]
    }


taskItem : String -> String -> Api.SnapshotItem
taskItem taskId workspaceId =
    { kind = "task"
    , data =
        Encode.object
            [ ( "id", Encode.string taskId )
            , ( "workspace_id", Encode.string workspaceId )
            , ( "title", Encode.string ("Task " ++ taskId) )
            , ( "status", Encode.string "todo" )
            , ( "priority", Encode.int 5 )
            , ( "dependency_count", Encode.int 0 )
            , ( "created_at", Encode.string "2026-01-01T00:00:00Z" )
            , ( "updated_at", Encode.string "2026-01-01T00:00:00Z" )
            ]
    }


dependencyItem : String -> String -> Api.SnapshotItem
dependencyItem taskId dependsOnId =
    { kind = "task_dependency"
    , data = Encode.object [ ( "task_id", Encode.string taskId ), ( "depends_on_id", Encode.string dependsOnId ) ]
    }
