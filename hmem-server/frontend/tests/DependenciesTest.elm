module DependenciesTest exposing (suite)

import Api
import AppShell
import Dict
import Expect
import Feature.Cards as Cards
import Feature.Dependencies as Dependencies
import Feature.WebSocket as WebSocket
import Helpers
import Http
import Json.Encode as Encode
import Route
import Set
import Test exposing (Test, describe, test)
import Types exposing (AuthStatus(..), Flags, Model, Msg(..), Page(..), WorkspaceTab(..))
import Url


suite : Test
suite =
    describe "dependency reconciliation and live refresh"
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
                    [ reconcile Api.Done |> (\updated -> ( prerequisiteStatus updated, Helpers.taskDependencySummariesForTask updated dependentId |> List.map .id, dependencyCount updated ))
                    , reconcile Api.Cancelled |> (\updated -> ( prerequisiteStatus updated, Helpers.taskDependencySummariesForTask updated dependentId |> List.map .id, dependencyCount updated ))
                    ]
        , test "authoritative refresh replaces the full summary and clears its request state" <|
            \_ ->
                let
                    ( pendingRefresh, _ ) =
                        modelWith Nothing Api.Todo
                            |> Dependencies.trackDependencyMutationRequest dependentId prerequisiteId "add" "request"
                            |> Dependencies.update (DependencyMutationDone dependentId "request" (Ok addResult))

                    ( refreshed, _ ) =
                        Dependencies.update
                            (GotTaskDependencyPage dependentId
                                workspaceId
                                pendingRefresh.sessionRequestEpoch
                                1
                                0
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
                        modelWith Nothing Api.Todo
                            |> Dependencies.trackDependencyMutationRequest dependentId prerequisiteId "add" "request"
                            |> Dependencies.update (DependencyMutationDone dependentId "request" (Ok addResult))

                    ( currentRefresh, _ ) =
                        Dependencies.beginDependencyRefresh dependentId pendingRefresh

                    stale =
                        Dependencies.update
                            (GotTaskDependencyPage dependentId
                                workspaceId
                                pendingRefresh.sessionRequestEpoch
                                1
                                0
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
                    , linkCount =
                        failed.dependencies.taskDependencyLinks
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
                            |> Dependencies.trackDependencyMutationRequest dependentId prerequisiteId "add" "request"

                    after =
                        Dependencies.update (DependencyMutationDone dependentId "request" (Err Http.NetworkError)) before
                            |> Tuple.first
                in
                Expect.equal
                    ( [], [], 0 )
                    ( Helpers.taskDependencySummariesForTask after dependentId |> List.map .id
                    , after.dependencies.taskDependencyLinks |> List.map (\link -> link.dependsOnId)
                    , dependencyCount after
                    )
        , test "remote add and remove events converge an expanded dependency page" <|
            \_ ->
                let
                    afterAddEvent =
                        WebSocket.update (WsMessageReceived (dependencyWire workspaceId "remote-add" "created")) (remoteModel True)
                            |> Tuple.first

                    afterAddPage =
                        Dependencies.update
                            (GotTaskDependencyPage dependentId
                                workspaceId
                                afterAddEvent.sessionRequestEpoch
                                1
                                0
                                (Ok { items = [ { id = prerequisiteId, name = "Remote prerequisite" } ], hasMore = False })
                            )
                            afterAddEvent
                            |> Tuple.first

                    afterRemoveEvent =
                        WebSocket.update (WsMessageReceived (dependencyWire workspaceId "remote-remove" "deleted")) afterAddPage
                            |> Tuple.first

                    afterRemovePage =
                        Dependencies.update
                            (GotTaskDependencyPage dependentId
                                workspaceId
                                afterRemoveEvent.sessionRequestEpoch
                                2
                                0
                                (Ok { items = [], hasMore = False })
                            )
                            afterRemoveEvent
                            |> Tuple.first
                in
                Expect.equal
                    { addRequest = Just 1
                    , afterAdd = [ prerequisiteId ]
                    , removeRequest = Just 2
                    , afterRemoveEvent = []
                    , afterRemove = []
                    }
                    { addRequest = Dict.get dependentId afterAddEvent.dependencies.taskDependencyRequests |> Maybe.map .generation
                    , afterAdd = Helpers.taskDependencySummariesForTask afterAddPage dependentId |> List.map .id
                    , removeRequest = Dict.get dependentId afterRemoveEvent.dependencies.taskDependencyRequests |> Maybe.map .generation
                    , afterRemoveEvent = Helpers.taskDependencySummariesForTask afterRemoveEvent dependentId |> List.map .id
                    , afterRemove = Helpers.taskDependencySummariesForTask afterRemovePage dependentId |> List.map .id
                    }
        , test "remote add crawls an overview-populated 60-item cache whose source had no page metadata" <|
            \_ ->
                let
                    oldItems =
                        List.range 1 60 |> List.map dependencySummary

                    newItem =
                        { id = prerequisiteId, name = "Dependency 999" }

                    base =
                        remoteModel True

                    overviewLoaded =
                        Dependencies.update
                            (GotTaskDependencies dependentId
                                base.selectedWorkspaceId
                                base.sessionRequestEpoch
                                base.dependencies.nextTaskDependencyRequestGeneration
                                (Ok
                                    { task = task dependentId "Current task" Api.Todo
                                    , dependencies = oldItems
                                    , readinessRollup = dependentTaskCardSummary.readinessRollup
                                    }
                                )
                            )
                            base
                            |> Tuple.first

                    afterEvent =
                        WebSocket.update (WsMessageReceived (dependencyWire workspaceId "remote-add-after-first-page" "created")) overviewLoaded
                            |> Tuple.first

                    afterFirstPage =
                        Dependencies.update
                            (GotTaskDependencyPage dependentId
                                workspaceId
                                afterEvent.sessionRequestEpoch
                                1
                                0
                                (Ok { items = List.take 50 oldItems, hasMore = True })
                            )
                            afterEvent
                            |> Tuple.first

                    afterStaleFirstPage =
                        Dependencies.update
                            (GotTaskDependencyPage dependentId
                                workspaceId
                                afterEvent.sessionRequestEpoch
                                1
                                0
                                (Ok { items = [], hasMore = False })
                            )
                            afterFirstPage
                            |> Tuple.first

                    afterLastPage =
                        Dependencies.update
                            (GotTaskDependencyPage dependentId
                                workspaceId
                                afterEvent.sessionRequestEpoch
                                2
                                50
                                (Ok { items = List.drop 50 oldItems ++ [ newItem ], hasMore = False })
                            )
                            afterStaleFirstPage
                            |> Tuple.first

                    finalItems =
                        Helpers.taskDependencySummariesForTask afterLastPage dependentId
                in
                Expect.equal
                    { sourceMetadata = Nothing
                    , overviewMetadata = Just False
                    , initialRequest = Just ( 1, 0 )
                    , cacheWhileRefreshing = oldItems ++ [ { id = prerequisiteId, name = "Prerequisite" } ]
                    , continuationRequest = Just ( 2, 50 )
                    , accumulatorSize = Just 50
                    , staleResponseIgnored = Just ( 2, 50 )
                    , finalCount = 61
                    , newItemVisible = True
                    , finalHasMore = Just False
                    , finalOffset = Just 61
                    , finalLoading = Just False
                    , finalRequest = Nothing
                    , finalAccumulator = Nothing
                    }
                    { sourceMetadata = Dict.get dependentId base.dependencies.taskDependencyHasMore
                    , overviewMetadata = Dict.get dependentId overviewLoaded.dependencies.taskDependencyHasMore
                    , initialRequest = Dict.get dependentId afterEvent.dependencies.taskDependencyRequests |> Maybe.map (\request -> ( request.generation, request.offset ))
                    , cacheWhileRefreshing = Helpers.taskDependencySummariesForTask afterFirstPage dependentId
                    , continuationRequest = Dict.get dependentId afterFirstPage.dependencies.taskDependencyRequests |> Maybe.map (\request -> ( request.generation, request.offset ))
                    , accumulatorSize = Dict.get dependentId afterFirstPage.dependencies.taskDependencyRefreshItems |> Maybe.map List.length
                    , staleResponseIgnored = Dict.get dependentId afterStaleFirstPage.dependencies.taskDependencyRequests |> Maybe.map (\request -> ( request.generation, request.offset ))
                    , finalCount = List.length finalItems
                    , newItemVisible = List.any (\item -> item.id == prerequisiteId) finalItems
                    , finalHasMore = Dict.get dependentId afterLastPage.dependencies.taskDependencyHasMore
                    , finalOffset = Dict.get dependentId afterLastPage.dependencies.taskDependencyNextOffset
                    , finalLoading = Dict.get dependentId afterLastPage.dependencies.taskDependencyLoading
                    , finalRequest = Dict.get dependentId afterLastPage.dependencies.taskDependencyRequests
                    , finalAccumulator = Dict.get dependentId afterLastPage.dependencies.taskDependencyRefreshItems
                    }
        , test "duplicate delivery is inert and a newer event fences an in-flight dependency response" <|
            \_ ->
                let
                    first =
                        WebSocket.update (WsMessageReceived (dependencyWire workspaceId "event-1" "created")) (remoteModel True)
                            |> Tuple.first

                    duplicate =
                        WebSocket.update (WsMessageReceived (dependencyWire workspaceId "event-1" "created")) first
                            |> Tuple.first

                    newer =
                        WebSocket.update (WsMessageReceived (dependencyWire workspaceId "event-2" "deleted")) duplicate
                            |> Tuple.first

                    afterStale =
                        Dependencies.update
                            (GotTaskDependencyPage dependentId
                                workspaceId
                                newer.sessionRequestEpoch
                                1
                                0
                                (Ok { items = [ { id = "stale", name = "Stale" } ], hasMore = False })
                            )
                            newer
                            |> Tuple.first

                    afterCurrent =
                        Dependencies.update
                            (GotTaskDependencyPage dependentId
                                workspaceId
                                newer.sessionRequestEpoch
                                2
                                0
                                (Ok { items = [ { id = prerequisiteId, name = "Current" }, { id = prerequisiteId, name = "Duplicate" } ], hasMore = False })
                            )
                            afterStale
                            |> Tuple.first
                in
                Expect.equal
                    { firstGeneration = Just 1
                    , duplicateGeneration = Just 1
                    , duplicateNextGeneration = 2
                    , newerGeneration = Just 2
                    , staleIgnored = []
                    , currentDeduplicated = [ prerequisiteId ]
                    }
                    { firstGeneration = Dict.get dependentId first.dependencies.taskDependencyRequests |> Maybe.map .generation
                    , duplicateGeneration = Dict.get dependentId duplicate.dependencies.taskDependencyRequests |> Maybe.map .generation
                    , duplicateNextGeneration = duplicate.dependencies.nextTaskDependencyRequestGeneration
                    , newerGeneration = Dict.get dependentId newer.dependencies.taskDependencyRequests |> Maybe.map .generation
                    , staleIgnored = Helpers.taskDependencySummariesForTask afterStale dependentId |> List.map .id
                    , currentDeduplicated = Helpers.taskDependencySummariesForTask afterCurrent dependentId |> List.map .id
                    }
        , test "collapsed and unrelated-workspace dependency events do not load a page" <|
            \_ ->
                let
                    collapsed =
                        WebSocket.update (WsMessageReceived (dependencyWire workspaceId "collapsed" "created")) (remoteModel False)
                            |> Tuple.first

                    unrelated =
                        WebSocket.update (WsMessageReceived (dependencyWire "workspace-2" "unrelated" "created")) (remoteModel True)
                            |> Tuple.first
                in
                Expect.equal
                    { collapsedRequested = False
                    , collapsedNextGeneration = 2
                    , unrelatedRequested = False
                    , unrelatedNextGeneration = 1
                    }
                    { collapsedRequested = Dict.member dependentId collapsed.dependencies.taskDependencyRequests
                    , collapsedNextGeneration = collapsed.dependencies.nextTaskDependencyRequestGeneration
                    , unrelatedRequested = Dict.member dependentId unrelated.dependencies.taskDependencyRequests
                    , unrelatedNextGeneration = unrelated.dependencies.nextTaskDependencyRequestGeneration
                    }
        , test "foreign dependency add and delete preserve selected-workspace caches, search, requests, and generations" <|
            \_ ->
                let
                    base =
                        remoteModel True

                    dependencies =
                        base.dependencies

                    webSocket =
                        base.webSocket

                    loading =
                        base.dataLoading

                    search =
                        base.search

                    existingRequest =
                        { workspaceId = workspaceId
                        , sessionEpoch = base.sessionRequestEpoch
                        , offset = 0
                        , generation = 7
                        }

                    before =
                        { base
                            | dependencies =
                                { dependencies
                                    | taskDependencies = Dict.singleton dependentId [ { id = prerequisiteId, name = "Prerequisite" } ]
                                    , taskDependencyLinks = [ { taskId = dependentId, dependsOnId = prerequisiteId } ]
                                    , taskDependencyLoading = Dict.singleton dependentId True
                                    , taskDependencyRequests = Dict.singleton dependentId existingRequest
                                    , nextTaskDependencyRequestGeneration = 8
                                }
                            , webSocket =
                                { webSocket
                                    | targetGenerations = Dict.singleton ("workspace:" ++ workspaceId ++ "|entity:task_dependency:" ++ dependentId ++ ":" ++ prerequisiteId) 4
                                }
                            , dataLoading =
                                { loading
                                    | taskCardSummaries = Dict.singleton dependentId dependentTaskCardSummary
                                    , navigationVisibleTaskIds = Set.singleton dependentId
                                    , navigationVisibilityActive = True
                                }
                            , search =
                                { search
                                    | query = "selected query"
                                    , unifiedResults = Just { observations = [], projects = [], tasks = [] }
                                    , isSearching = True
                                    , activeRequestQuery = Just "selected query"
                                    , activeRequest = Just { workspaceId = workspaceId, token = 5, query = "selected query" }
                                    , nextRequestToken = 6
                                }
                        }

                    afterAdd =
                        WebSocket.update (WsMessageReceived (dependencyWire "workspace-2" "foreign-add-loaded" "created")) before
                            |> Tuple.first

                    afterDelete =
                        WebSocket.update (WsMessageReceived (dependencyWire "workspace-2" "foreign-delete" "deleted")) afterAdd
                            |> Tuple.first

                    preservedState current =
                        { summaries = current.dependencies.taskDependencies
                        , links = current.dependencies.taskDependencyLinks
                        , loading = current.dependencies.taskDependencyLoading
                        , requests = current.dependencies.taskDependencyRequests
                        , refreshItems = current.dependencies.taskDependencyRefreshItems
                        , mutations = current.dependencies.taskDependencyMutations
                        , nextRequestGeneration = current.dependencies.nextTaskDependencyRequestGeneration
                        , targetGenerations = current.webSocket.targetGenerations
                        , search = current.search
                        , taskCards = current.dataLoading.taskCardSummaries
                        , visibleTasks = current.dataLoading.navigationVisibleTaskIds
                        }
                in
                Expect.equal
                    [ preservedState before, preservedState before ]
                    [ preservedState afterAdd, preservedState afterDelete ]
        , test "a collapsed cached dependency page is invalidated and refetched when reopened" <|
            \_ ->
                let
                    base =
                        remoteModel False

                    dependencies =
                        base.dependencies

                    cached =
                        { base
                            | dependencies =
                                { dependencies
                                    | taskDependencies = Dict.singleton dependentId [ { id = prerequisiteId, name = "Stale prerequisite" } ]
                                    , taskDependencyHasMore = Dict.singleton dependentId False
                                    , taskDependencyNextOffset = Dict.singleton dependentId 1
                                }
                        }

                    invalidated =
                        WebSocket.update (WsMessageReceived (dependencyWire workspaceId "collapsed-add" "created")) cached
                            |> Tuple.first

                    reopened =
                        Cards.update (ToggleCardExpand dependentId) invalidated
                            |> Tuple.first
                in
                Expect.equal
                    { cacheAfterEvent = False
                    , pageMetadataAfterEvent = ( Nothing, Nothing )
                    , requestAfterEvent = Nothing
                    , reopened = True
                    , reopenedRequest = Just 2
                    , reopenedOffset = Just 0
                    }
                    { cacheAfterEvent = Dict.member dependentId invalidated.dependencies.taskDependencies
                    , pageMetadataAfterEvent =
                        ( Dict.get dependentId invalidated.dependencies.taskDependencyHasMore
                        , Dict.get dependentId invalidated.dependencies.taskDependencyNextOffset
                        )
                    , requestAfterEvent = Dict.get dependentId invalidated.dependencies.taskDependencyRequests
                    , reopened = Dict.get dependentId reopened.cards.expandedCards |> Maybe.withDefault False
                    , reopenedRequest = Dict.get dependentId reopened.dependencies.taskDependencyRequests |> Maybe.map .generation
                    , reopenedOffset = Dict.get dependentId reopened.dependencies.taskDependencyRequests |> Maybe.map .offset
                    }
        , test "a collapsed event fences an in-flight page and its late response before reopen" <|
            \_ ->
                let
                    base =
                        remoteModel False

                    dependencies =
                        base.dependencies

                    inFlightRequest =
                        { workspaceId = workspaceId
                        , sessionEpoch = base.sessionRequestEpoch
                        , offset = 0
                        , generation = 7
                        }

                    inFlight =
                        { base
                            | dependencies =
                                { dependencies
                                    | taskDependencies = Dict.singleton dependentId [ { id = prerequisiteId, name = "Deleted prerequisite" } ]
                                    , taskDependencyLinks = [ { taskId = dependentId, dependsOnId = prerequisiteId } ]
                                    , taskDependencyLoading = Dict.singleton dependentId True
                                    , taskDependencyRequests = Dict.singleton dependentId inFlightRequest
                                    , nextTaskDependencyRequestGeneration = 8
                                }
                        }

                    invalidated =
                        WebSocket.update (WsMessageReceived (dependencyWire workspaceId "collapsed-delete" "deleted")) inFlight
                            |> Tuple.first

                    afterLateResponse =
                        Dependencies.update
                            (GotTaskDependencyPage dependentId
                                workspaceId
                                inFlight.sessionRequestEpoch
                                7
                                0
                                (Ok { items = [ { id = prerequisiteId, name = "Late stale prerequisite" } ], hasMore = False })
                            )
                            invalidated
                            |> Tuple.first

                    reopened =
                        Cards.update (ToggleCardExpand dependentId) afterLateResponse
                            |> Tuple.first
                in
                Expect.equal
                    { cacheAfterEvent = False
                    , linksAfterEvent = []
                    , requestAfterEvent = Nothing
                    , loadingAfterEvent = Nothing
                    , cacheAfterLateResponse = False
                    , requestAfterReopen = Just 9
                    }
                    { cacheAfterEvent = Dict.member dependentId invalidated.dependencies.taskDependencies
                    , linksAfterEvent = invalidated.dependencies.taskDependencyLinks
                    , requestAfterEvent = Dict.get dependentId invalidated.dependencies.taskDependencyRequests
                    , loadingAfterEvent = Dict.get dependentId invalidated.dependencies.taskDependencyLoading
                    , cacheAfterLateResponse = Dict.member dependentId afterLateResponse.dependencies.taskDependencies
                    , requestAfterReopen = Dict.get dependentId reopened.dependencies.taskDependencyRequests |> Maybe.map .generation
                    }
        , test "route-away and return clear every dependency page fence before a late response" <|
            \_ ->
                let
                    before =
                        staleDependencyRefreshModel

                    auditUrl =
                        { url | path = "/audit" }

                    away =
                        Route.handleUrlChange auditUrl before |> Tuple.first

                    returned =
                        Route.handleUrlChange url away |> Tuple.first

                    afterLateResponse =
                        Dependencies.update
                            (GotTaskDependencyPage dependentId
                                workspaceId
                                before.sessionRequestEpoch
                                7
                                0
                                (Ok { items = [ { id = "stale-route", name = "Stale route dependency" } ], hasMore = False })
                            )
                            returned
                            |> Tuple.first

                    dependencyPageState current =
                        { summaries = current.dependencies.taskDependencies
                        , links = current.dependencies.taskDependencyLinks
                        , hasMore = current.dependencies.taskDependencyHasMore
                        , offsets = current.dependencies.taskDependencyNextOffset
                        , loading = current.dependencies.taskDependencyLoading
                        , requests = current.dependencies.taskDependencyRequests
                        , refreshItems = current.dependencies.taskDependencyRefreshItems
                        , mutations = current.dependencies.taskDependencyMutations
                        , nextGeneration = current.dependencies.nextTaskDependencyRequestGeneration
                        }

                    awayClearedState =
                        dependencyPageState { before | dependencies = Dependencies.resetCache before.dependencies }

                    returnedClearedState =
                        dependencyPageState { before | dependencies = Dependencies.resetCache (Dependencies.resetCache before.dependencies) }
                in
                Expect.equal
                    { awayWorkspace = Nothing
                    , returnedWorkspace = Just workspaceId
                    , awayState = awayClearedState
                    , returnedState = returnedClearedState
                    , lateState = returnedClearedState
                    }
                    { awayWorkspace = away.selectedWorkspaceId
                    , returnedWorkspace = returned.selectedWorkspaceId
                    , awayState = dependencyPageState away
                    , returnedState = dependencyPageState returned
                    , lateState = dependencyPageState afterLateResponse
                    }
        , test "canonical resync and full snapshot clear dependency requests and reject late pages" <|
            \_ ->
                let
                    before =
                        staleDependencyRefreshModel

                    afterResync =
                        WebSocket.update (WsMessageReceived canonicalResyncWire) before |> Tuple.first

                    afterSnapshot =
                        WebSocket.update (WsMessageReceived canonicalSnapshotWire) before |> Tuple.first

                    afterSnapshotLateResponse =
                        Dependencies.update
                            (GotTaskDependencyPage dependentId
                                workspaceId
                                before.sessionRequestEpoch
                                7
                                0
                                (Ok { items = [ { id = "stale-snapshot", name = "Stale snapshot dependency" } ], hasMore = False })
                            )
                            afterSnapshot
                            |> Tuple.first

                    dependencyPageState current =
                        { summaries = current.dependencies.taskDependencies
                        , links = current.dependencies.taskDependencyLinks
                        , hasMore = current.dependencies.taskDependencyHasMore
                        , offsets = current.dependencies.taskDependencyNextOffset
                        , loading = current.dependencies.taskDependencyLoading
                        , requests = current.dependencies.taskDependencyRequests
                        , refreshItems = current.dependencies.taskDependencyRefreshItems
                        , mutations = current.dependencies.taskDependencyMutations
                        , nextGeneration = current.dependencies.nextTaskDependencyRequestGeneration
                        }

                    clearedState =
                        dependencyPageState { before | dependencies = Dependencies.resetCache before.dependencies }
                in
                Expect.equal
                    [ clearedState, clearedState, clearedState ]
                    [ dependencyPageState afterResync
                    , dependencyPageState afterSnapshot
                    , dependencyPageState afterSnapshotLateResponse
                    ]
        , test "late mutation success and failure are inert after every reset-class or scope guard change" <|
            \_ ->
                let
                    requestId =
                        flags.sessionId ++ "-req-1"

                    started =
                        Dependencies.update (PerformAddDependency dependentId prerequisiteId) (remoteModel True)
                            |> Tuple.first

                    auditUrl =
                        { url | path = "/audit" }

                    cacheReset =
                        { started | dependencies = Dependencies.resetCache started.dependencies }

                    routeReset =
                        Route.handleUrlChange auditUrl started |> Tuple.first

                    resyncReset =
                        WebSocket.update (WsMessageReceived canonicalResyncWire) started |> Tuple.first

                    sessionChanged =
                        { started | sessionRequestEpoch = started.sessionRequestEpoch + 1 }

                    workspaceChanged =
                        { started | selectedWorkspaceId = Just "workspace-2" }

                    resetClasses =
                        [ cacheReset, routeReset, resyncReset, sessionChanged, workspaceChanged ]

                    mutationState current =
                        { summaries = current.dependencies.taskDependencies
                        , links = current.dependencies.taskDependencyLinks
                        , requests = current.dependencies.taskDependencyRequests
                        , correlations = current.dependencies.taskDependencyMutations
                        , generation = current.dependencies.nextTaskDependencyRequestGeneration
                        , tasks = current.tasks
                        , toastCount = List.length current.toast.toasts
                        }

                    applyLate result current =
                        Dependencies.update (DependencyMutationDone dependentId requestId result) current
                            |> Tuple.first
                            |> mutationState
                in
                Expect.equal
                    (resetClasses |> List.concatMap (\current -> [ mutationState current, mutationState current ]))
                    (resetClasses
                        |> List.concatMap
                            (\current ->
                                [ applyLate (Ok addResult) current
                                , applyLate (Err Http.NetworkError) current
                                ]
                            )
                    )
        , test "remote edges immediately drive link counts, status options, and computed task/project readiness" <|
            \_ ->
                let
                    projectId =
                        "project-1"

                    base =
                        remoteModel True

                    loading =
                        base.dataLoading

                    baseDependentTask =
                        task dependentId "Current task" Api.Todo

                    dependentTask =
                        { baseDependentTask | projectId = Just projectId }

                    hydrated =
                        { base
                            | projects = Dict.singleton projectId (project projectId)
                            , tasks = Dict.insert dependentId dependentTask base.tasks
                            , dataLoading = { loading | cardHydrationLoaded = True }
                        }

                    afterAdd =
                        WebSocket.update (WsMessageReceived (dependencyWire workspaceId "link-add" "created")) hydrated
                            |> Tuple.first

                    afterRemove =
                        WebSocket.update (WsMessageReceived (dependencyWire workspaceId "link-remove" "deleted")) afterAdd
                            |> Tuple.first

                    projection current =
                        Cards.cardTreeProjection workspaceId current

                    directCount current =
                        Dict.get dependentId (projection current).taskDirectOpenDependencyCounts
                            |> Maybe.withDefault 0

                    canSelectDone current =
                        Cards.taskStatusOptionsForTask (directCount current > 0) dependentTask
                            |> List.member Api.Done

                    readiness current =
                        ( Helpers.taskReadinessRollupForTask current dependentId
                            |> Maybe.map (\rollup -> ( rollup.openDependencyCount, rollup.completionReady ))
                        , Helpers.projectReadinessRollupForProject current projectId
                            |> Maybe.map (\rollup -> ( rollup.openDependencyCount, rollup.dependencyBlockedTaskCount, rollup.completionReady ))
                        )
                in
                Expect.equal
                    { addLinks = [ ( dependentId, prerequisiteId ) ]
                    , addDependencyCount = 1
                    , addDirectCount = 1
                    , addCanSelectDone = False
                    , addReadiness = ( Just ( 1, True ), Just ( 1, 1, False ) )
                    , removeLinks = []
                    , removeDependencyCount = 0
                    , removeDirectCount = 0
                    , removeCanSelectDone = True
                    , removeReadiness = ( Just ( 0, True ), Just ( 0, 0, False ) )
                    }
                    { addLinks = afterAdd.dependencies.taskDependencyLinks |> List.map (\link -> ( link.taskId, link.dependsOnId ))
                    , addDependencyCount = dependencyCount afterAdd
                    , addDirectCount = directCount afterAdd
                    , addCanSelectDone = canSelectDone afterAdd
                    , addReadiness = readiness afterAdd
                    , removeLinks = afterRemove.dependencies.taskDependencyLinks |> List.map (\link -> ( link.taskId, link.dependsOnId ))
                    , removeDependencyCount = dependencyCount afterRemove
                    , removeDirectCount = directCount afterRemove
                    , removeCanSelectDone = canSelectDone afterRemove
                    , removeReadiness = readiness afterRemove
                    }
        , test "older overview results cannot cancel or overwrite an event-driven dependency crawl" <|
            \_ ->
                let
                    oldItems =
                        List.range 1 60 |> List.map dependencySummary

                    base =
                        remoteModel True

                    overviewGeneration =
                        base.dependencies.nextTaskDependencyRequestGeneration

                    oldOverviewResult =
                        Ok
                            { task = task dependentId "Stale overview task" Api.Todo
                            , dependencies = [ { id = "stale-overview", name = "Stale overview" } ]
                            , readinessRollup = dependentTaskCardSummary.readinessRollup
                            }

                    overviewLoaded =
                        Dependencies.update
                            (GotTaskDependencies dependentId
                                base.selectedWorkspaceId
                                base.sessionRequestEpoch
                                overviewGeneration
                                (Ok
                                    { task = task dependentId "Current task" Api.Todo
                                    , dependencies = oldItems
                                    , readinessRollup = dependentTaskCardSummary.readinessRollup
                                    }
                                )
                            )
                            base
                            |> Tuple.first

                    afterEvent =
                        WebSocket.update (WsMessageReceived (dependencyWire workspaceId "overview-fence" "created")) overviewLoaded
                            |> Tuple.first

                    afterOldOverview =
                        Dependencies.update (GotTaskDependencies dependentId base.selectedWorkspaceId base.sessionRequestEpoch overviewGeneration oldOverviewResult) afterEvent
                            |> Tuple.first

                    afterFirstPage =
                        Dependencies.update
                            (GotTaskDependencyPage dependentId workspaceId afterEvent.sessionRequestEpoch 1 0 (Ok { items = List.take 50 oldItems, hasMore = True }))
                            afterOldOverview
                            |> Tuple.first

                    afterOldDuringAccumulator =
                        Dependencies.update (GotTaskDependencies dependentId base.selectedWorkspaceId base.sessionRequestEpoch overviewGeneration oldOverviewResult) afterFirstPage
                            |> Tuple.first

                    afterFinalPage =
                        Dependencies.update
                            (GotTaskDependencyPage dependentId workspaceId afterEvent.sessionRequestEpoch 2 50 (Ok { items = List.drop 50 oldItems ++ [ { id = prerequisiteId, name = "Dependency 999" } ], hasMore = False }))
                            afterOldDuringAccumulator
                            |> Tuple.first

                    afterOldFollowingCompletion =
                        Dependencies.update (GotTaskDependencies dependentId base.selectedWorkspaceId base.sessionRequestEpoch overviewGeneration oldOverviewResult) afterFinalPage
                            |> Tuple.first

                    afterRouteReset =
                        Route.handleUrlChange { url | path = "/audit" } overviewLoaded |> Tuple.first

                    afterOldFollowingReset =
                        Dependencies.update (GotTaskDependencies dependentId base.selectedWorkspaceId base.sessionRequestEpoch overviewGeneration oldOverviewResult) afterRouteReset
                            |> Tuple.first
                in
                Expect.equal
                    { activeRequestAfterOld = Just ( 1, 0 )
                    , accumulatorAfterOld = Just 0
                    , activeRequestDuringAccumulator = Just ( 2, 50 )
                    , accumulatorDuringOld = Just 50
                    , finalCount = 61
                    , finalContainsNew = True
                    , finalContainsStale = False
                    , resetCacheEmpty = True
                    }
                    { activeRequestAfterOld = Dict.get dependentId afterOldOverview.dependencies.taskDependencyRequests |> Maybe.map (\request -> ( request.generation, request.offset ))
                    , accumulatorAfterOld = Dict.get dependentId afterOldOverview.dependencies.taskDependencyRefreshItems |> Maybe.map List.length
                    , activeRequestDuringAccumulator = Dict.get dependentId afterOldDuringAccumulator.dependencies.taskDependencyRequests |> Maybe.map (\request -> ( request.generation, request.offset ))
                    , accumulatorDuringOld = Dict.get dependentId afterOldDuringAccumulator.dependencies.taskDependencyRefreshItems |> Maybe.map List.length
                    , finalCount = Helpers.taskDependencySummariesForTask afterOldFollowingCompletion dependentId |> List.length
                    , finalContainsNew = Helpers.taskDependencySummariesForTask afterOldFollowingCompletion dependentId |> List.any (\dependency -> dependency.id == prerequisiteId)
                    , finalContainsStale = Helpers.taskDependencySummariesForTask afterOldFollowingCompletion dependentId |> List.any (\dependency -> dependency.id == "stale-overview")
                    , resetCacheEmpty = not (Dict.member dependentId afterOldFollowingReset.dependencies.taskDependencies)
                    }
        , test "a canonical echo only matches the exact locally mutated edge and action" <|
            \_ ->
                let
                    localRequestId =
                        flags.sessionId ++ "-req-1"

                    started =
                        Dependencies.update (PerformAddDependency dependentId prerequisiteId) (remoteModel True)
                            |> Tuple.first

                    wrongEdge =
                        WebSocket.update
                            (WsMessageReceived
                                (dependencyWireForEdgeWithRequestId workspaceId
                                    "same-request-wrong-edge"
                                    "created"
                                    localRequestId
                                    dependentId
                                    "different-prerequisite"
                                )
                            )
                            started
                            |> Tuple.first

                    wrongAction =
                        WebSocket.update
                            (WsMessageReceived
                                (dependencyWireForEdgeWithRequestId workspaceId
                                    "same-request-wrong-action"
                                    "deleted"
                                    localRequestId
                                    dependentId
                                    prerequisiteId
                                )
                            )
                            started
                            |> Tuple.first

                    correlation current =
                        current.dependencies.taskDependencyMutations
                            |> List.filter (\candidate -> candidate.requestId == localRequestId)
                            |> List.head
                            |> Maybe.map (\candidate -> ( candidate.dependsOnId, candidate.action, candidate.echoSeen ))

                    activeGeneration current =
                        Dict.get dependentId current.dependencies.taskDependencyRequests
                            |> Maybe.map .generation
                in
                Expect.equal
                    { wrongEdgeRequest = Just 1
                    , wrongEdgeCorrelation = Just ( prerequisiteId, "add", False )
                    , wrongActionRequest = Just 1
                    , wrongActionCorrelation = Just ( prerequisiteId, "add", False )
                    }
                    { wrongEdgeRequest = activeGeneration wrongEdge
                    , wrongEdgeCorrelation = correlation wrongEdge
                    , wrongActionRequest = activeGeneration wrongAction
                    , wrongActionCorrelation = correlation wrongAction
                    }
        , test "remote edges fetch an uncached prerequisite and refresh dependent project readiness" <|
            \_ ->
                let
                    projectId =
                        "project-uncached-prerequisite"

                    base =
                        remoteModel True

                    baseDependencies =
                        base.dependencies

                    loading =
                        base.dataLoading

                    baseDependentTask =
                        task dependentId "Current task" Api.Todo

                    dependentTask =
                        { baseDependentTask | projectId = Just projectId }

                    staleTaskRollup =
                        { openSubtaskCount = 0
                        , doneSubtaskCount = 0
                        , cancelledSubtaskCount = 0
                        , blockedSubtaskCount = 0
                        , dependencyBlockedTaskCount = 0
                        , openDependencyCount = 0
                        , completionReady = True
                        }

                    staleProjectRollup =
                        { openProjectCount = 0
                        , closedProjectCount = 0
                        , openTaskCount = 1
                        , doneTaskCount = 0
                        , cancelledTaskCount = 0
                        , blockedTaskCount = 0
                        , dependencyBlockedTaskCount = 0
                        , openDependencyCount = 0
                        , completionReady = False
                        }

                    hydrated =
                        { base
                            | projects = Dict.singleton projectId (project projectId)
                            , tasks = Dict.singleton dependentId dependentTask
                            , dataLoading = { loading | cardHydrationLoaded = True }
                            , dependencies =
                                { baseDependencies
                                    | taskReadinessRollups = Dict.singleton dependentId staleTaskRollup
                                    , projectReadinessRollups = Dict.singleton projectId staleProjectRollup
                                }
                        }

                    afterAddEvent =
                        WebSocket.update (WsMessageReceived (dependencyWire workspaceId "uncached-link-add" "created")) hydrated
                            |> Tuple.first

                    afterPrerequisite =
                        WebSocket.update
                            (CanonicalTaskFetched
                                (canonicalGuardFor ("entity:task:" ++ prerequisiteId) afterAddEvent)
                                prerequisiteId
                                (Ok (task prerequisiteId "Uncached prerequisite" Api.Todo))
                            )
                            afterAddEvent
                            |> Tuple.first

                    addProjectRollup =
                        { staleProjectRollup | dependencyBlockedTaskCount = 1, openDependencyCount = 1 }

                    afterAddReadiness =
                        WebSocket.update
                            (CanonicalProjectOverviewFetched
                                (canonicalGuardFor ("dependency-readiness:project:" ++ projectId) afterPrerequisite)
                                projectId
                                (Ok { project = project projectId, tasks = [ dependentTask ], subprojects = [], readinessRollup = addProjectRollup })
                            )
                            afterPrerequisite
                            |> Tuple.first

                    afterRemoveEvent =
                        WebSocket.update (WsMessageReceived (dependencyWire workspaceId "uncached-link-remove" "deleted")) afterAddReadiness
                            |> Tuple.first

                    afterRemoveReadiness =
                        WebSocket.update
                            (CanonicalProjectOverviewFetched
                                (canonicalGuardFor ("dependency-readiness:project:" ++ projectId) afterRemoveEvent)
                                projectId
                                (Ok { project = project projectId, tasks = [ dependentTask ], subprojects = [], readinessRollup = staleProjectRollup })
                            )
                            afterRemoveEvent
                            |> Tuple.first

                    directCount current =
                        Dict.get dependentId (Cards.cardTreeProjection workspaceId current).taskDirectOpenDependencyCounts
                            |> Maybe.withDefault 0

                    canSelectDone current =
                        Cards.taskStatusOptionsForTask (directCount current > 0) dependentTask
                            |> List.member Api.Done

                    readiness current =
                        ( Helpers.taskReadinessRollupForTask current dependentId
                            |> Maybe.map (\rollup -> rollup.openDependencyCount)
                        , Helpers.projectReadinessRollupForProject current projectId
                            |> Maybe.map (\rollup -> ( rollup.openDependencyCount, rollup.dependencyBlockedTaskCount ))
                        )
                in
                Expect.equal
                    { addFetchedPrerequisite = True
                    , addLinks = [ ( dependentId, prerequisiteId ) ]
                    , addDirectCount = 1
                    , addCanSelectDone = False
                    , addReadiness = ( Just 1, Just ( 1, 1 ) )
                    , removeLinks = []
                    , removeDirectCount = 0
                    , removeCanSelectDone = True
                    , removeReadiness = ( Just 0, Just ( 0, 0 ) )
                    }
                    { addFetchedPrerequisite = Dict.member prerequisiteId afterPrerequisite.tasks
                    , addLinks = afterAddReadiness.dependencies.taskDependencyLinks |> List.map (\link -> ( link.taskId, link.dependsOnId ))
                    , addDirectCount = directCount afterAddReadiness
                    , addCanSelectDone = canSelectDone afterAddReadiness
                    , addReadiness = readiness afterAddReadiness
                    , removeLinks = afterRemoveReadiness.dependencies.taskDependencyLinks |> List.map (\link -> ( link.taskId, link.dependsOnId ))
                    , removeDirectCount = directCount afterRemoveReadiness
                    , removeCanSelectDone = canSelectDone afterRemoveReadiness
                    , removeReadiness = readiness afterRemoveReadiness
                    }
        , test "late task overview is inert after a session-context failure reset" <|
            \_ ->
                let
                    before =
                        remoteModel True

                    overviewGeneration =
                        before.dependencies.nextTaskDependencyRequestGeneration

                    afterFailure =
                        AppShell.handleOwned
                            (AppShell.SessionContextLoadedMsg before.sessionRequestEpoch (Just workspaceId) (Err Http.NetworkError))
                            before
                            |> Tuple.first

                    afterLateOverview =
                        Dependencies.update
                            (GotTaskDependencies dependentId
                                (Just workspaceId)
                                before.sessionRequestEpoch
                                overviewGeneration
                                (Ok
                                    { task = task dependentId "Late task" Api.Todo
                                    , dependencies = [ { id = "late-dependency", name = "Late dependency" } ]
                                    , readinessRollup = dependentTaskCardSummary.readinessRollup
                                    }
                                )
                            )
                            afterFailure
                            |> Tuple.first
                in
                Expect.equal
                    { authReady = False
                    , sessionCleared = True
                    , sessionEpoch = before.sessionRequestEpoch + 1
                    , generation = overviewGeneration + 1
                    , tasksEmpty = True
                    , dependenciesEmpty = True
                    }
                    { authReady = afterLateOverview.auth.status == AuthReady
                    , sessionCleared = afterLateOverview.sessionContext == Nothing
                    , sessionEpoch = afterLateOverview.sessionRequestEpoch
                    , generation = afterLateOverview.dependencies.nextTaskDependencyRequestGeneration
                    , tasksEmpty = Dict.isEmpty afterLateOverview.tasks
                    , dependenciesEmpty = Dict.isEmpty afterLateOverview.dependencies.taskDependencies
                    }
        , test "new dependency events fence ancestor readiness without disturbing a newer dependency crawl" <|
            \_ ->
                let
                    ancestorTaskId =
                        "ancestor-task"

                    projectId =
                        "ancestor-project"

                    base =
                        remoteModel True

                    baseDependent =
                        Dict.get dependentId base.tasks
                            |> Maybe.withDefault (task dependentId "Current task" Api.Todo)

                    dependentTask =
                        { baseDependent | parentId = Just ancestorTaskId, projectId = Just projectId }

                    baseAncestorTask =
                        task ancestorTaskId "Ancestor task" Api.Todo

                    ancestorTask =
                        { baseAncestorTask | projectId = Just projectId }

                    staleTaskRollup =
                        { openSubtaskCount = 1
                        , doneSubtaskCount = 0
                        , cancelledSubtaskCount = 0
                        , blockedSubtaskCount = 0
                        , dependencyBlockedTaskCount = 0
                        , openDependencyCount = 0
                        , completionReady = False
                        }

                    staleProjectRollup =
                        { openProjectCount = 0
                        , closedProjectCount = 0
                        , openTaskCount = 2
                        , doneTaskCount = 0
                        , cancelledTaskCount = 0
                        , blockedTaskCount = 0
                        , dependencyBlockedTaskCount = 0
                        , openDependencyCount = 0
                        , completionReady = False
                        }

                    dependencies =
                        base.dependencies

                    prepared =
                        { base
                            | projects = Dict.singleton projectId (project projectId)
                            , tasks = Dict.insert ancestorTaskId ancestorTask (Dict.insert dependentId dependentTask base.tasks)
                            , dependencies =
                                { dependencies
                                    | taskDependencies = Dict.insert ancestorTaskId [ { id = "old-ancestor-row", name = "Old ancestor row" } ] dependencies.taskDependencies
                                    , taskDependencyHasMore = Dict.insert ancestorTaskId False dependencies.taskDependencyHasMore
                                    , taskDependencyNextOffset = Dict.insert ancestorTaskId 1 dependencies.taskDependencyNextOffset
                                    , taskReadinessRollups = Dict.insert ancestorTaskId staleTaskRollup dependencies.taskReadinessRollups
                                    , projectReadinessRollups = Dict.insert projectId staleProjectRollup dependencies.projectReadinessRollups
                                }
                        }

                    afterEventA =
                        WebSocket.update (WsMessageReceived (dependencyWire workspaceId "readiness-event-a" "created")) prepared
                            |> Tuple.first

                    oldTaskGuard =
                        canonicalGuardFor ("dependency-readiness:task:" ++ ancestorTaskId) afterEventA

                    oldProjectGuard =
                        canonicalGuardFor ("dependency-readiness:project:" ++ projectId) afterEventA

                    afterAncestorCrawlStarted =
                        Dependencies.beginDependencyRefresh ancestorTaskId afterEventA
                            |> Tuple.first

                    ancestorCrawlRequest =
                        Dict.get ancestorTaskId afterAncestorCrawlStarted.dependencies.taskDependencyRequests

                    afterEventB =
                        WebSocket.update (WsMessageReceived (dependencyWire workspaceId "readiness-event-b" "deleted")) afterAncestorCrawlStarted
                            |> Tuple.first

                    newTaskGuard =
                        canonicalGuardFor ("dependency-readiness:task:" ++ ancestorTaskId) afterEventB

                    newProjectGuard =
                        canonicalGuardFor ("dependency-readiness:project:" ++ projectId) afterEventB

                    staleOverview =
                        { task = ancestorTask
                        , dependencies = [ { id = "stale-overview-row", name = "Stale overview row" } ]
                        , readinessRollup = { staleTaskRollup | openDependencyCount = 99 }
                        }

                    afterOldTaskReadiness =
                        WebSocket.update (CanonicalTaskReadinessFetched oldTaskGuard ancestorTaskId (Ok staleOverview)) afterEventB
                            |> Tuple.first

                    afterOldProjectReadiness =
                        WebSocket.update
                            (CanonicalProjectOverviewFetched
                                oldProjectGuard
                                projectId
                                (Ok { project = project projectId, tasks = [], subprojects = [], readinessRollup = { staleProjectRollup | openDependencyCount = 99 } })
                            )
                            afterOldTaskReadiness
                            |> Tuple.first

                    afterCanonicalPage =
                        case ancestorCrawlRequest of
                            Just request ->
                                Dependencies.update
                                    (GotTaskDependencyPage ancestorTaskId
                                        request.workspaceId
                                        request.sessionEpoch
                                        request.generation
                                        request.offset
                                        (Ok { items = [ { id = "canonical-ancestor-row", name = "Canonical ancestor row" } ], hasMore = False })
                                    )
                                    afterOldProjectReadiness
                                    |> Tuple.first

                            Nothing ->
                                afterOldProjectReadiness

                    acceptedTaskRollup =
                        { staleTaskRollup | openDependencyCount = 1 }

                    acceptedProjectRollup =
                        { staleProjectRollup | dependencyBlockedTaskCount = 1, openDependencyCount = 1 }

                    afterNewTaskReadiness =
                        WebSocket.update
                            (CanonicalTaskReadinessFetched newTaskGuard
                                ancestorTaskId
                                (Ok { staleOverview | readinessRollup = acceptedTaskRollup })
                            )
                            afterCanonicalPage
                            |> Tuple.first

                    afterNewProjectReadiness =
                        WebSocket.update
                            (CanonicalProjectOverviewFetched
                                newProjectGuard
                                projectId
                                (Ok { project = project projectId, tasks = [], subprojects = [], readinessRollup = acceptedProjectRollup })
                            )
                            afterNewTaskReadiness
                            |> Tuple.first

                    taskTarget =
                        "workspace:" ++ workspaceId ++ "|dependency-readiness:task:" ++ ancestorTaskId

                    projectTarget =
                        "workspace:" ++ workspaceId ++ "|dependency-readiness:project:" ++ projectId
                in
                Expect.equal
                    { taskGeneration = Just 2
                    , projectGeneration = Just 2
                    , oldTaskRejected = Nothing
                    , oldProjectRejected = Nothing
                    , crawlStayedActive = ancestorCrawlRequest |> Maybe.map .generation
                    , canonicalRows = [ "canonical-ancestor-row" ]
                    , crawlCompleted = False
                    , taskRollup = Just 1
                    , projectRollup = Just ( 1, 1 )
                    }
                    { taskGeneration = Dict.get taskTarget afterEventB.webSocket.targetGenerations
                    , projectGeneration = Dict.get projectTarget afterEventB.webSocket.targetGenerations
                    , oldTaskRejected = Dict.get ancestorTaskId afterOldTaskReadiness.dependencies.taskReadinessRollups
                    , oldProjectRejected = Dict.get projectId afterOldProjectReadiness.dependencies.projectReadinessRollups
                    , crawlStayedActive = Dict.get ancestorTaskId afterOldProjectReadiness.dependencies.taskDependencyRequests |> Maybe.map .generation
                    , canonicalRows = Helpers.taskDependencySummariesForTask afterNewProjectReadiness ancestorTaskId |> List.map .id
                    , crawlCompleted = Dict.member ancestorTaskId afterNewProjectReadiness.dependencies.taskDependencyRequests
                    , taskRollup = Dict.get ancestorTaskId afterNewProjectReadiness.dependencies.taskReadinessRollups |> Maybe.map .openDependencyCount
                    , projectRollup = Dict.get projectId afterNewProjectReadiness.dependencies.projectReadinessRollups |> Maybe.map (\rollup -> ( rollup.openDependencyCount, rollup.dependencyBlockedTaskCount ))
                    }
        , test "local HTTP results and canonical echoes share one dependency refresh in either order" <|
            \_ ->
                let
                    localAddRequestId =
                        flags.sessionId ++ "-req-1"

                    afterAddStarted =
                        Dependencies.update (PerformAddDependency dependentId prerequisiteId) (remoteModel True) |> Tuple.first

                    afterAddHttp =
                        Dependencies.update (DependencyMutationDone dependentId localAddRequestId (Ok addResult)) afterAddStarted |> Tuple.first

                    afterAddEcho =
                        WebSocket.update (WsMessageReceived (dependencyWireWithRequestId workspaceId "local-add-echo" "created" localAddRequestId)) afterAddHttp |> Tuple.first

                    afterAddPage =
                        Dependencies.update
                            (GotTaskDependencyPage dependentId
                                workspaceId
                                afterAddEcho.sessionRequestEpoch
                                1
                                0
                                (Ok { items = [ { id = prerequisiteId, name = "Prerequisite" } ], hasMore = False })
                            )
                            afterAddEcho
                            |> Tuple.first

                    localRemoveRequestId =
                        flags.sessionId ++ "-req-2"

                    afterRemoveStarted =
                        Dependencies.update (PerformRemoveDependency dependentId prerequisiteId) afterAddPage |> Tuple.first

                    afterRemoveEcho =
                        WebSocket.update (WsMessageReceived (dependencyWireWithRequestId workspaceId "local-remove-echo" "deleted" localRemoveRequestId)) afterRemoveStarted |> Tuple.first

                    afterRemoveHttp =
                        Dependencies.update (DependencyMutationDone dependentId localRemoveRequestId (Ok removeResult)) afterRemoveEcho |> Tuple.first

                    afterNewRemoteEvent =
                        WebSocket.update (WsMessageReceived (dependencyWireWithRequestId workspaceId "genuinely-remote" "created" "remote-request")) afterRemoveHttp |> Tuple.first

                    correlation requestId current =
                        current.dependencies.taskDependencyMutations
                            |> List.filter (\candidate -> candidate.requestId == requestId)
                            |> List.head
                            |> Maybe.map (\candidate -> ( candidate.httpSucceeded, candidate.echoSeen ))

                    activeRequest current =
                        Dict.get dependentId current.dependencies.taskDependencyRequests
                            |> Maybe.map (\request -> ( request.generation, request.offset ))
                in
                Expect.equal
                    { addHttpRequest = Just ( 1, 0 )
                    , addEchoRequest = Just ( 1, 0 )
                    , addEchoNextGeneration = 2
                    , addCorrelation = Just ( True, True )
                    , removeEchoRequest = Just ( 2, 0 )
                    , removeHttpRequest = Just ( 2, 0 )
                    , removeHttpNextGeneration = 3
                    , removeCorrelation = Just ( True, True )
                    , removedImmediately = []
                    , newerRemoteRequest = Just ( 3, 0 )
                    , newerRemoteNextGeneration = 4
                    }
                    { addHttpRequest = activeRequest afterAddHttp
                    , addEchoRequest = activeRequest afterAddEcho
                    , addEchoNextGeneration = afterAddEcho.dependencies.nextTaskDependencyRequestGeneration
                    , addCorrelation = correlation localAddRequestId afterAddEcho
                    , removeEchoRequest = activeRequest afterRemoveEcho
                    , removeHttpRequest = activeRequest afterRemoveHttp
                    , removeHttpNextGeneration = afterRemoveHttp.dependencies.nextTaskDependencyRequestGeneration
                    , removeCorrelation = correlation localRemoveRequestId afterRemoveHttp
                    , removedImmediately = Helpers.taskDependencySummariesForTask afterRemoveEcho dependentId |> List.map .id
                    , newerRemoteRequest = activeRequest afterNewRemoteEvent
                    , newerRemoteNextGeneration = afterNewRemoteEvent.dependencies.nextTaskDependencyRequestGeneration
                    }
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


dependencySummary : Int -> Api.TaskDependencySummary
dependencySummary index =
    { id = "dependency-" ++ String.fromInt index
    , name = "Dependency " ++ String.padLeft 3 '0' (String.fromInt index)
    }


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


project : String -> Api.Project
project id =
    { id = id
    , workspaceId = workspaceId
    , parentId = Nothing
    , name = "Project"
    , description = Nothing
    , status = Api.ProjActive
    , priority = 1
    , createdAt = "2026-01-01T00:00:00Z"
    , updatedAt = "2026-01-01T00:00:00Z"
    }


model : Model
model =
    AppShell.initModel Nothing url (WorkspacePage workspaceId) flags Nothing { tab = ProjectsTab, focus = Nothing, observationId = Nothing }
        |> AppShell.finalizeInit (WorkspacePage workspaceId)


remoteModel : Bool -> Model
remoteModel expanded =
    let
        base =
            modelWith (Just []) Api.Todo

        cards =
            base.cards
    in
    { base
        | auth = { status = AuthReady, mode = Just "test" }
        , sessionContext = Just editorSession
        , cards = { cards | expandedCards = Dict.singleton dependentId expanded }
    }


staleDependencyRefreshModel : Model
staleDependencyRefreshModel =
    let
        base =
            remoteModel True

        dependencies =
            base.dependencies
    in
    { base
        | dependencies =
            { dependencies
                | taskDependencies = Dict.singleton dependentId [ { id = prerequisiteId, name = "Stale prerequisite" } ]
                , taskDependencyLinks = [ { taskId = dependentId, dependsOnId = prerequisiteId } ]
                , taskDependencyHasMore = Dict.singleton dependentId False
                , taskDependencyNextOffset = Dict.singleton dependentId 1
                , taskDependencyLoading = Dict.singleton dependentId True
                , taskDependencyRequests =
                    Dict.singleton dependentId
                        { workspaceId = workspaceId
                        , sessionEpoch = base.sessionRequestEpoch
                        , offset = 0
                        , generation = 7
                        }
                , taskDependencyRefreshItems = Dict.singleton dependentId [ { id = "partial", name = "Partial" } ]
                , taskDependencyMutations =
                    [ { requestId = "stale-local-request"
                      , taskId = dependentId
                      , dependsOnId = prerequisiteId
                      , action = "add"
                      , workspaceId = workspaceId
                      , sessionEpoch = base.sessionRequestEpoch
                      , httpSucceeded = True
                      , echoSeen = False
                      }
                    ]
                , nextTaskDependencyRequestGeneration = 8
            }
    }


dependentTaskCardSummary : Api.TaskCardSummary
dependentTaskCardSummary =
    { id = dependentId
    , workspaceId = workspaceId
    , projectId = Nothing
    , parentId = Nothing
    , title = "Current task"
    , status = Api.Todo
    , priority = 1
    , dueAt = Nothing
    , completedAt = Nothing
    , dependencyCount = 1
    , createdAt = "2026-01-01T00:00:00Z"
    , updatedAt = "2026-01-01T00:00:00Z"
    , directSubtaskCount = 0
    , hasChildren = False
    , readinessRollup =
        { openSubtaskCount = 0
        , doneSubtaskCount = 0
        , cancelledSubtaskCount = 0
        , blockedSubtaskCount = 0
        , dependencyBlockedTaskCount = 0
        , openDependencyCount = 1
        , completionReady = False
        }
    }


dependencyWire : String -> String -> String -> String
dependencyWire eventWorkspaceId eventId action =
    dependencyWireWithRequestId eventWorkspaceId eventId action ("request-" ++ eventId)


dependencyWireWithRequestId : String -> String -> String -> String -> String
dependencyWireWithRequestId eventWorkspaceId eventId action requestId =
    dependencyWireForEdgeWithRequestId eventWorkspaceId eventId action requestId dependentId prerequisiteId


dependencyWireForEdgeWithRequestId : String -> String -> String -> String -> String -> String -> String
dependencyWireForEdgeWithRequestId eventWorkspaceId eventId action requestId taskId dependsOnId =
    Encode.object
        [ ( "schema_version", Encode.int 1 )
        , ( "transport", Encode.string "frames" )
        , ( "scope"
          , Encode.object
                [ ( "scope", Encode.string "workspace" )
                , ( "workspace_id", Encode.string eventWorkspaceId )
                ]
          )
        , ( "frames"
          , Encode.list identity
                [ Encode.object
                    [ ( "schema_version", Encode.int 1 )
                    , ( "type", Encode.string "change" )
                    , ( "event"
                      , Encode.object
                            [ ( "schema_version", Encode.int 1 )
                            , ( "event_id", Encode.string eventId )
                            , ( "scope", Encode.string "workspace" )
                            , ( "workspace_id", Encode.string eventWorkspaceId )
                            , ( "occurred_at", Encode.string "2026-01-01T00:00:00Z" )
                            , ( "transaction"
                              , Encode.object
                                    [ ( "id", Encode.string ("tx-" ++ eventId) )
                                    , ( "cause", Encode.string "rest" )
                                    , ( "request_id", Encode.string requestId )
                                    ]
                              )
                            , ( "actor"
                              , Encode.object
                                    [ ( "type", Encode.string "user" )
                                    , ( "id", Encode.string "remote-editor" )
                                    ]
                              )
                            , ( "entity"
                              , Encode.object
                                    [ ( "type", Encode.string "task_dependency" )
                                    , ( "id", Encode.string (taskId ++ ":" ++ dependsOnId) )
                                    , ( "action", Encode.string action )
                                    ]
                              )
                            , ( "invalidations"
                              , Encode.list identity
                                    [ Encode.object
                                        [ ( "kind", Encode.string "entity" )
                                        , ( "target", Encode.string ("task_dependency:" ++ taskId ++ ":" ++ dependsOnId) )
                                        ]
                                    , Encode.object
                                        [ ( "kind", Encode.string "readiness" )
                                        , ( "target", Encode.string ("task:" ++ taskId) )
                                        ]
                                    , Encode.object
                                        [ ( "kind", Encode.string "search" )
                                        , ( "target", Encode.string ("workspace:" ++ eventWorkspaceId) )
                                        ]
                                    ]
                              )
                            ]
                      )
                    ]
                ]
          )
        ]
        |> Encode.encode 0


canonicalGuardFor : String -> Model -> Types.CanonicalRequestGuard
canonicalGuardFor targetKey currentModel =
    let
        scopeKey =
            "workspace:" ++ workspaceId
    in
    { scopeKey = scopeKey
    , targetKey = targetKey
    , targetGeneration = Dict.get (scopeKey ++ "|" ++ targetKey) currentModel.webSocket.targetGenerations |> Maybe.withDefault -1
    , sessionEpoch = currentModel.sessionRequestEpoch
    , routeWorkspace = currentModel.selectedWorkspaceId
    , audienceId = editorSession.principal.actorId
    }


canonicalResyncWire : String
canonicalResyncWire =
    Encode.object
        [ ( "schema_version", Encode.int 1 )
        , ( "transport", Encode.string "frame" )
        , ( "scope", Encode.object [ ( "scope", Encode.string "workspace" ), ( "workspace_id", Encode.string workspaceId ) ] )
        , ( "frame", Encode.object [ ( "schema_version", Encode.int 1 ), ( "type", Encode.string "resync_required" ) ] )
        ]
        |> Encode.encode 0


canonicalSnapshotWire : String
canonicalSnapshotWire =
    Encode.object
        [ ( "schema_version", Encode.int 1 )
        , ( "transport", Encode.string "snapshot" )
        , ( "scope", Encode.object [ ( "scope", Encode.string "workspace" ), ( "workspace_id", Encode.string workspaceId ) ] )
        , ( "snapshot_profile", Encode.string "full_v1" )
        , ( "items"
          , Encode.list identity
                [ Encode.object
                    [ ( "schema_version", Encode.int 1 )
                    , ( "kind", Encode.string "workspace" )
                    , ( "data"
                      , Encode.object
                            [ ( "id", Encode.string workspaceId )
                            , ( "name", Encode.string "Workspace" )
                            , ( "workspace_type", Encode.string "repository" )
                            , ( "created_at", Encode.string "2026-01-01T00:00:00Z" )
                            , ( "updated_at", Encode.string "2026-01-01T00:00:00Z" )
                            ]
                      )
                    ]
                ]
          )
        , ( "resume_token", Encode.string "canonical-snapshot-token" )
        ]
        |> Encode.encode 0


editorSession : Api.SessionContext
editorSession =
    { authMode = "test"
    , principal = { actorType = "user", actorId = "editor", actorLabel = "Editor", authority = "local", grantUserId = Nothing }
    , globalPermissions = { createWorkspace = False, superadmin = False }
    , workspace = Just { workspaceId = workspaceId, role = Just "edit", canRead = True, canEdit = True, canAdmin = False }
    }


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
