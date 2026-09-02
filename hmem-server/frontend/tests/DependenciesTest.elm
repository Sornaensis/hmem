module DependenciesTest exposing (suite)

import Api
import AppShell
import Dict
import Expect
import Feature.Dependencies as Dependencies
import Helpers
import Http
import Test exposing (Test, describe, test)
import Types exposing (Flags, Model, Msg(..), Page(..), WorkspaceTab(..))
import Url


suite : Test
suite =
    describe "+Dep dependency reconciliation"
        [ test "the selected task is reconciled as the prerequisite of the current task" <|
            \_ ->
                let
                    updated =
                        Helpers.applyDependencyMutationResult addResult (modelWith Nothing Api.Todo)
                in
                Expect.equal
                    { target = dependentId
                    , prerequisite = prerequisiteId
                    , summaries = [ prerequisiteId ]
                    , links = [ ( dependentId, prerequisiteId ) ]
                    , count = 1
                    }
                    { target = addResult.taskId
                    , prerequisite = addResult.dependsOnId
                    , summaries = Helpers.taskDependencySummariesForTask updated dependentId |> List.map .id
                    , links = updated.dependencies.taskDependencyLinks |> List.map (\link -> ( link.taskId, link.dependsOnId ))
                    , count = dependencyCount updated
                    }
        , test "successful add and remove reconcile both present and absent summary caches" <|
            \_ ->
                let
                    absentAfterAdd =
                        Helpers.applyDependencyMutationResult addResult (modelWith Nothing Api.Todo)

                    presentAfterAdd =
                        Helpers.applyDependencyMutationResult addResult (modelWith (Just []) Api.Todo)

                    absentBeforeRemove =
                        modelWithLink Api.Todo

                    absentAfterRemove =
                        Helpers.applyDependencyMutationResult removeResult absentBeforeRemove

                    presentAfterRemove =
                        Helpers.applyDependencyMutationResult removeResult presentAfterAdd
                in
                Expect.equal
                    { absentAdd = ( [ prerequisiteId ], 1 )
                    , presentAdd = ( [ prerequisiteId ], 1 )
                    , absentRemove = ( [ prerequisiteId ], [], 0 )
                    , presentRemove = ( [], 0 )
                    }
                    { absentAdd = ( Helpers.taskDependencySummariesForTask absentAfterAdd dependentId |> List.map .id, dependencyCount absentAfterAdd )
                    , presentAdd = ( Helpers.taskDependencySummariesForTask presentAfterAdd dependentId |> List.map .id, dependencyCount presentAfterAdd )
                    , absentRemove =
                        ( Helpers.taskDependencySummariesForTask absentBeforeRemove dependentId |> List.map .id
                        , Helpers.taskDependencySummariesForTask absentAfterRemove dependentId |> List.map .id
                        , dependencyCount absentAfterRemove
                        )
                    , presentRemove = ( Helpers.taskDependencySummariesForTask presentAfterRemove dependentId |> List.map .id, dependencyCount presentAfterRemove )
                    }
        , test "empty affected tasks still immediately show completed and cancelled prerequisites" <|
            \_ ->
                let
                    reconcile status =
                        Helpers.applyDependencyMutationResult addResult (modelWith Nothing status)
                in
                Expect.equal
                    [ ( Api.Done, [ prerequisiteId ], 1 )
                    , ( Api.Cancelled, [ prerequisiteId ], 1 )
                    ]
                    [ reconcile Api.Done |> \updated -> ( prerequisiteStatus updated, Helpers.taskDependencySummariesForTask updated dependentId |> List.map .id, dependencyCount updated )
                    , reconcile Api.Cancelled |> \updated -> ( prerequisiteStatus updated, Helpers.taskDependencySummariesForTask updated dependentId |> List.map .id, dependencyCount updated )
                    ]
        , test "authoritative refresh replaces the full summary and clears its request state" <|
            \_ ->
                let
                    ( pendingRefresh, _ ) =
                        Dependencies.update (DependencyMutationDone dependentId (Ok addResult)) (modelWith Nothing Api.Todo)

                    ( refreshed, _ ) =
                        Dependencies.update
                            (GotTaskDependencyPage dependentId workspaceId pendingRefresh.sessionRequestEpoch 1 0
                                (Ok { items = [ { id = prerequisiteId, name = "Authoritative prerequisite" } ], hasMore = False })
                            )
                            pendingRefresh
                in
                Expect.equal
                    { summaries = [ ( prerequisiteId, "Authoritative prerequisite" ) ]
                    , count = 1
                    , loading = False
                    , requestPresent = False
                    , hasMore = False
                    , nextOffset = Just 1
                    }
                    { summaries = Helpers.taskDependencySummariesForTask refreshed dependentId |> List.map (\summary -> ( summary.id, summary.name ))
                    , count = dependencyCount refreshed
                    , loading = Dict.get dependentId refreshed.dependencies.taskDependencyLoading |> Maybe.withDefault True
                    , requestPresent = Dict.member dependentId refreshed.dependencies.taskDependencyRequests
                    , hasMore = Dict.get dependentId refreshed.dependencies.taskDependencyHasMore |> Maybe.withDefault True
                    , nextOffset = Dict.get dependentId refreshed.dependencies.taskDependencyNextOffset
                    }
        , test "failed and stale refreshes retain one confirmed edge" <|
            \_ ->
                let
                    ( pendingRefresh, _ ) =
                        Dependencies.update (DependencyMutationDone dependentId (Ok addResult)) (modelWith Nothing Api.Todo)

                    ( currentRefresh, _ ) =
                        Dependencies.beginDependencyRefresh dependentId pendingRefresh

                    stale =
                        Dependencies.update
                            (GotTaskDependencyPage dependentId workspaceId pendingRefresh.sessionRequestEpoch 1 0
                                (Ok { items = [], hasMore = False })
                            )
                            currentRefresh
                            |> Tuple.first

                    failed =
                        Dependencies.update
                            (GotTaskDependencyPage dependentId workspaceId pendingRefresh.sessionRequestEpoch 2 0 (Err Http.NetworkError))
                            stale
                            |> Tuple.first
                in
                Expect.equal
                    { summaries = [ prerequisiteId ], count = 1, linkCount = 1, loading = False, requestPresent = False }
                    { summaries = Helpers.taskDependencySummariesForTask failed dependentId |> List.map .id
                    , count = dependencyCount failed
                    , linkCount = failed.dependencies.taskDependencyLinks
                        |> List.filter (\link -> link.taskId == dependentId && link.dependsOnId == prerequisiteId)
                        |> List.length
                    , loading = Dict.get dependentId failed.dependencies.taskDependencyLoading |> Maybe.withDefault True
                    , requestPresent = Dict.member dependentId failed.dependencies.taskDependencyRequests
                    }
        , test "a failed POST leaves the visible dependency cache unchanged" <|
            \_ ->
                let
                    before =
                        modelWith (Just []) Api.Todo

                    after =
                        Dependencies.update (DependencyMutationDone dependentId (Err Http.NetworkError)) before
                            |> Tuple.first
                in
                Expect.equal
                    ( [], [], 0 )
                    ( Helpers.taskDependencySummariesForTask after dependentId |> List.map .id
                    , after.dependencies.taskDependencyLinks |> List.map (\link -> link.dependsOnId)
                    , dependencyCount after
                    )
        ]


workspaceId : String
workspaceId =
    "workspace-1"


dependentId : String
dependentId =
    "current-task"


prerequisiteId : String
prerequisiteId =
    "selected-prerequisite"


addResult : Api.DependencyMutationResult
addResult =
    { action = "add"
    , taskId = dependentId
    , dependsOnId = prerequisiteId
    , affectedTasks = []
    }


removeResult : Api.DependencyMutationResult
removeResult =
    { addResult | action = "remove" }


modelWith : Maybe (List Api.TaskDependencySummary) -> Api.TaskStatus -> Model
modelWith cachedSummaries prerequisiteTaskStatus =
    let
        base =
            model

        dependencies =
            base.dependencies

        taskDependencies =
            case cachedSummaries of
                Just summaries ->
                    Dict.singleton dependentId summaries

                Nothing ->
                    Dict.empty
    in
    { base
        | tasks =
            Dict.fromList
                [ ( dependentId, task dependentId "Current task" Api.Todo )
                , ( prerequisiteId, task prerequisiteId "Prerequisite" prerequisiteTaskStatus )
                ]
        , dependencies = { dependencies | taskDependencies = taskDependencies }
    }


modelWithLink : Api.TaskStatus -> Model
modelWithLink prerequisiteTaskStatus =
    let
        base =
            modelWith Nothing prerequisiteTaskStatus

        dependent =
            Dict.get dependentId base.tasks
                |> Maybe.withDefault (task dependentId "Current task" Api.Todo)

        dependencies =
            base.dependencies
    in
    { base
        | tasks = Dict.insert dependentId { dependent | dependencyCount = 1 } base.tasks
        , dependencies =
            { dependencies
                | taskDependencyLinks = [ { taskId = dependentId, dependsOnId = prerequisiteId } ]
            }
    }


dependencyCount : Model -> Int
dependencyCount currentModel =
    Dict.get dependentId currentModel.tasks
        |> Maybe.map .dependencyCount
        |> Maybe.withDefault -1


prerequisiteStatus : Model -> Api.TaskStatus
prerequisiteStatus currentModel =
    Dict.get prerequisiteId currentModel.tasks
        |> Maybe.map .status
        |> Maybe.withDefault Api.Todo


task : String -> String -> Api.TaskStatus -> Api.Task
task id title status =
    { id = id
    , workspaceId = workspaceId
    , projectId = Nothing
    , parentId = Nothing
    , title = title
    , description = Nothing
    , status = status
    , priority = 1
    , dueAt = Nothing
    , completedAt = Nothing
    , dependencyCount = 0
    , memoryLinkCount = 0
    , createdAt = "2026-01-01T00:00:00Z"
    , updatedAt = "2026-01-01T00:00:00Z"
    }


model : Model
model =
    AppShell.initModel Nothing url (WorkspacePage workspaceId) flags Nothing { tab = ProjectsTab, focus = Nothing, observationId = Nothing }
        |> AppShell.finalizeInit (WorkspacePage workspaceId)


flags : Flags
flags =
    { apiUrl = "https://api.example"
    , wsUrl = "wss://api.example"
    , sessionId = "session-1"
    , runtimeMode = "test"
    , authTokenStorageKey = "hmem-auth-token"
    , authTokenPresent = False
    , loginUrl = Nothing
    , logoutUrl = Nothing
    }


url : Url.Url
url =
    { protocol = Url.Https, host = "app.example", port_ = Nothing, path = "/workspace/workspace-1", query = Nothing, fragment = Nothing }
