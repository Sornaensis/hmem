module DataLoadingTest exposing (suite)

import Api
import AppShell
import Dict
import Expect
import Feature.DataLoading as DataLoading
import Feature.Dependencies as Dependencies
import Feature.WebSocket as WebSocket
import Set
import String
import Test exposing (Test, describe, test)
import Types exposing (AuthStatus(..), Flags, Model, Msg(..), Page(..), WorkspaceTab(..))
import Url


suite : Test
suite =
    describe "bounded navigation response guards"
        [ test "a branch response retains its request state and makes returned children visible" <|
            \_ ->
                let
                    ( requested, _ ) =
                        DataLoading.beginNavigationBranch "project" workspaceId (Just "parent") model

                    ( updated, _ ) =
                        DataLoading.update
                            (GotNavigationBranch workspaceId requested.sessionRequestEpoch requested.dataLoading.navigationGeneration "project:parent" filterFingerprint 0 0
                                (Ok { workspaceId = workspaceId, projects = { items = [ project "child" (Just "parent") ], hasMore = True }, tasks = { items = [ task "child-task" (Just "parent") ], hasMore = False } })
                            )
                            requested
                in
                Expect.equal
                    { project = True, task = True, visible = True, succeeded = True }
                    { project = Dict.member "child" updated.dataLoading.projectCardSummaries
                    , task = Dict.member "child-task" updated.dataLoading.taskCardSummaries
                    , visible = Set.member "child" updated.dataLoading.navigationVisibleProjectIds
                    , succeeded = Dict.get "project:parent" updated.dataLoading.loadedNavigationBranches |> Maybe.map .succeeded |> Maybe.withDefault False
                    }
        , test "root navigation rejects stale workspace, session, filter, generation, and page keys" <|
            \_ ->
                let
                    prepared =
                        DataLoading.prepareRootNavigationRequest (Just workspaceId) rootLoadModel

                    staleMessages =
                        [ ( "workspace", GotRootNavigation "other-workspace" prepared.sessionRequestEpoch (Just 1) prepared.dataLoading.navigationGeneration filterFingerprint 0 0 (Ok rootResponse) )
                        , ( "session", GotRootNavigation workspaceId (prepared.sessionRequestEpoch + 1) (Just 1) prepared.dataLoading.navigationGeneration filterFingerprint 0 0 (Ok rootResponse) )
                        , ( "filter", GotRootNavigation workspaceId prepared.sessionRequestEpoch (Just 1) prepared.dataLoading.navigationGeneration "other-filter" 0 0 (Ok rootResponse) )
                        , ( "generation", GotRootNavigation workspaceId prepared.sessionRequestEpoch (Just 1) (prepared.dataLoading.navigationGeneration + 1) filterFingerprint 0 0 (Ok rootResponse) )
                        , ( "project-offset", GotRootNavigation workspaceId prepared.sessionRequestEpoch (Just 1) prepared.dataLoading.navigationGeneration filterFingerprint 50 0 (Ok rootResponse) )
                        , ( "task-offset", GotRootNavigation workspaceId prepared.sessionRequestEpoch (Just 1) prepared.dataLoading.navigationGeneration filterFingerprint 0 50 (Ok rootResponse) )
                        ]
                in
                Expect.equal True
                    (List.all
                        (\( _, message ) ->
                            DataLoading.update message prepared
                                |> Tuple.first
                                |> .dataLoading
                                |> .projectCardSummaries
                                |> Dict.member "stale-root"
                                |> not
                        )
                        staleMessages
                    )
        , test "the prepared root navigation guard accepts the concurrent bootstrap response" <|
            \_ ->
                let
                    prepared =
                        DataLoading.prepareRootNavigationRequest (Just workspaceId) rootLoadModel

                    ( updated, _ ) =
                        DataLoading.update
                            (GotRootNavigation workspaceId prepared.sessionRequestEpoch (Just 1) prepared.dataLoading.navigationGeneration filterFingerprint 0 0 (Ok rootResponse))
                            prepared
                in
                Expect.equal ( True, True )
                    ( Dict.member "stale-root" updated.dataLoading.projectCardSummaries
                    , updated.dataLoading.rootNavigationRequest |> Maybe.map .succeeded |> Maybe.withDefault False
                    )
        , test "branch and next-page navigation reject every stale key dimension" <|
            \_ ->
                let
                    ( branchRequest, _ ) =
                        DataLoading.beginNavigationBranch "project" workspaceId (Just "parent") model

                    staleBranchMessages =
                        [ GotNavigationBranch "other-workspace" branchRequest.sessionRequestEpoch branchRequest.dataLoading.navigationGeneration "project:parent" filterFingerprint 0 0 (Ok staleResponse)
                        , GotNavigationBranch workspaceId (branchRequest.sessionRequestEpoch + 1) branchRequest.dataLoading.navigationGeneration "project:parent" filterFingerprint 0 0 (Ok staleResponse)
                        , GotNavigationBranch workspaceId branchRequest.sessionRequestEpoch (branchRequest.dataLoading.navigationGeneration + 1) "project:parent" filterFingerprint 0 0 (Ok staleResponse)
                        , GotNavigationBranch workspaceId branchRequest.sessionRequestEpoch branchRequest.dataLoading.navigationGeneration "project:parent" "other-filter" 0 0 (Ok staleResponse)
                        , GotNavigationBranch workspaceId branchRequest.sessionRequestEpoch branchRequest.dataLoading.navigationGeneration "project:other" filterFingerprint 0 0 (Ok staleResponse)
                        , GotNavigationBranch workspaceId branchRequest.sessionRequestEpoch branchRequest.dataLoading.navigationGeneration "project:parent" filterFingerprint 50 0 (Ok staleResponse)
                        , GotNavigationBranch workspaceId branchRequest.sessionRequestEpoch branchRequest.dataLoading.navigationGeneration "project:parent" filterFingerprint 0 50 (Ok staleResponse)
                        ]

                    ( firstPage, _ ) =
                        DataLoading.update
                            (GotNavigationBranch workspaceId branchRequest.sessionRequestEpoch branchRequest.dataLoading.navigationGeneration "project:parent" filterFingerprint 0 0 (Ok { workspaceId = workspaceId, projects = { items = [ project "page-one" (Just "parent") ], hasMore = True }, tasks = { items = [], hasMore = False } }))
                            branchRequest

                    ( pageRequest, _ ) =
                        DataLoading.beginNavigationBranchPage "project" "parent" "project" firstPage

                    stalePageMessages =
                        [ GotNavigationBranch "other-workspace" pageRequest.sessionRequestEpoch pageRequest.dataLoading.navigationGeneration "project:parent" filterFingerprint 50 0 (Ok staleResponse)
                        , GotNavigationBranch workspaceId (pageRequest.sessionRequestEpoch + 1) pageRequest.dataLoading.navigationGeneration "project:parent" filterFingerprint 50 0 (Ok staleResponse)
                        , GotNavigationBranch workspaceId pageRequest.sessionRequestEpoch (pageRequest.dataLoading.navigationGeneration + 1) "project:parent" filterFingerprint 50 0 (Ok staleResponse)
                        , GotNavigationBranch workspaceId pageRequest.sessionRequestEpoch pageRequest.dataLoading.navigationGeneration "project:parent" "other-filter" 50 0 (Ok staleResponse)
                        , GotNavigationBranch workspaceId pageRequest.sessionRequestEpoch pageRequest.dataLoading.navigationGeneration "project:other" filterFingerprint 50 0 (Ok staleResponse)
                        , GotNavigationBranch workspaceId pageRequest.sessionRequestEpoch pageRequest.dataLoading.navigationGeneration "project:parent" filterFingerprint 0 0 (Ok staleResponse)
                        , GotNavigationBranch workspaceId pageRequest.sessionRequestEpoch pageRequest.dataLoading.navigationGeneration "project:parent" filterFingerprint 50 50 (Ok staleResponse)
                        ]

                    leavesStaleOut message source =
                        DataLoading.update message source
                            |> Tuple.first
                            |> .dataLoading
                            |> .projectCardSummaries
                            |> Dict.member "stale-branch"
                            |> not
                in
                Expect.equal True
                    (List.all (\message -> leavesStaleOut message branchRequest) staleBranchMessages
                        && List.all (\message -> leavesStaleOut message pageRequest) stalePageMessages
                    )
        , test "focus navigation rejects stale workspace, session, filter, generation, target, and ancestor page" <|
            \_ ->
                let
                    ( requested, _ ) =
                        DataLoading.beginNavigationFocus workspaceId "project" "target" model

                    staleFocus =
                        { workspaceId = workspaceId, target = Api.NavigationProjectSummary (project "stale-focus" Nothing), ancestors = [], ancestorsTruncated = False, nextAncestorOffset = Nothing }

                    staleMessages =
                        [ GotNavigationFocus "other-workspace" requested.sessionRequestEpoch requested.dataLoading.navigationGeneration filterFingerprint "project" "target" 0 (Ok staleFocus)
                        , GotNavigationFocus workspaceId (requested.sessionRequestEpoch + 1) requested.dataLoading.navigationGeneration filterFingerprint "project" "target" 0 (Ok staleFocus)
                        , GotNavigationFocus workspaceId requested.sessionRequestEpoch (requested.dataLoading.navigationGeneration + 1) filterFingerprint "project" "target" 0 (Ok staleFocus)
                        , GotNavigationFocus workspaceId requested.sessionRequestEpoch requested.dataLoading.navigationGeneration "other-filter" "project" "target" 0 (Ok staleFocus)
                        , GotNavigationFocus workspaceId requested.sessionRequestEpoch requested.dataLoading.navigationGeneration filterFingerprint "task" "target" 0 (Ok staleFocus)
                        , GotNavigationFocus workspaceId requested.sessionRequestEpoch requested.dataLoading.navigationGeneration filterFingerprint "project" "other-target" 0 (Ok staleFocus)
                        , GotNavigationFocus workspaceId requested.sessionRequestEpoch requested.dataLoading.navigationGeneration filterFingerprint "project" "target" 64 (Ok staleFocus)
                        ]
                in
                Expect.equal True
                    (List.all
                        (\message ->
                            DataLoading.update message requested
                                |> Tuple.first
                                |> .dataLoading
                                |> .projectCardSummaries
                                |> Dict.member "stale-focus"
                                |> not
                        )
                        staleMessages
                    )
        , test "focus continuation accepts only the expected page and merges target plus ancestors" <|
            \_ ->
                let
                    ( requested, _ ) =
                        DataLoading.beginNavigationFocus workspaceId "project" "target" model

                    ( continued, _ ) =
                        DataLoading.update
                            (GotNavigationFocus workspaceId requested.sessionRequestEpoch requested.dataLoading.navigationGeneration filterFingerprint "project" "target" 0
                                (Ok { workspaceId = workspaceId, target = Api.NavigationProjectSummary (project "target" (Just "ancestor-1")), ancestors = [ Api.NavigationProjectSummary (project "ancestor-1" Nothing) ], ancestorsTruncated = True, nextAncestorOffset = Just 64 })
                            )
                            requested

                    ( ignored, _ ) =
                        DataLoading.update
                            (GotNavigationFocus workspaceId requested.sessionRequestEpoch requested.dataLoading.navigationGeneration filterFingerprint "project" "target" 0
                                (Ok { workspaceId = workspaceId, target = Api.NavigationProjectSummary (project "target" (Just "wrong")), ancestors = [ Api.NavigationProjectSummary (project "wrong" Nothing) ], ancestorsTruncated = False, nextAncestorOffset = Nothing })
                            )
                            continued

                    ( completed, _ ) =
                        DataLoading.update
                            (GotNavigationFocus workspaceId requested.sessionRequestEpoch requested.dataLoading.navigationGeneration filterFingerprint "project" "target" 64
                                (Ok { workspaceId = workspaceId, target = Api.NavigationProjectSummary (project "target" (Just "ancestor-1")), ancestors = [ Api.NavigationProjectSummary (project "ancestor-2" Nothing) ], ancestorsTruncated = False, nextAncestorOffset = Nothing })
                            )
                            ignored
                in
                Expect.equal
                    { offset = Just 64, staleIgnored = False, target = True, firstAncestor = True, secondAncestor = True }
                    { offset = continued.dataLoading.activeNavigationFocus |> Maybe.map .ancestorOffset
                    , staleIgnored = Dict.member "wrong" ignored.dataLoading.projectCardSummaries
                    , target = Dict.member "target" completed.dataLoading.projectCardSummaries
                    , firstAncestor = Dict.member "ancestor-1" completed.dataLoading.projectCardSummaries
                    , secondAncestor = Dict.member "ancestor-2" completed.dataLoading.projectCardSummaries
                    }
        , test "an overlapping dependency refresh rejects the stale same-offset response" <|
            \_ ->
                let
                    ( first, _ ) =
                        Dependencies.beginDependencyRefresh "task" model

                    ( second, _ ) =
                        Dependencies.beginDependencyRefresh "task" first

                    stale =
                        Dependencies.update (GotTaskDependencyPage "task" workspaceId model.sessionRequestEpoch 1 0 (Ok { items = [ { id = "stale", name = "Stale" } ], hasMore = False })) second
                            |> Tuple.first

                    current =
                        Dependencies.update (GotTaskDependencyPage "task" workspaceId model.sessionRequestEpoch 2 0 (Ok { items = [ { id = "current", name = "Current" } ], hasMore = False })) stale
                            |> Tuple.first
                in
                Expect.equal
                    ( Nothing, Just [ "current" ] )
                    ( Dict.get "task" stale.dependencies.taskDependencies
                    , Dict.get "task" current.dependencies.taskDependencies |> Maybe.map (List.map .id)
                    )
        , test "filtered canonical summaries preserve a matching reparented branch card until replay completes" <|
            \_ ->
                let
                    guard =
                        { scopeKey = "workspace:" ++ workspaceId
                        , targetKey = "navigation-summaries:project:child"
                        , targetGeneration = 1
                        , sessionEpoch = model.sessionRequestEpoch
                        , routeWorkspace = Just workspaceId
                        , audienceId = "editor"
                        }

                    baseSearch =
                        model.search

                    filteredSearch =
                        { baseSearch | filterProjectStatuses = [ "completed" ] }

                    baseWebSocket =
                        model.webSocket

                    websocket =
                        { baseWebSocket | targetGenerations = Dict.singleton "workspace:workspace-1|navigation-summaries:project:child" 1 }

                    baseLoading =
                        model.dataLoading

                    loading =
                        { baseLoading
                            | projectCardSummaries = Dict.singleton "child" (project "child" (Just "old-parent"))
                            , navigationVisibleProjectIds = Set.singleton "child"
                            , navigationVisibilityActive = True
                        }

                    source =
                        { model
                            | auth = { status = AuthReady, mode = Just "test" }
                            , sessionContext = Just editorSession
                            , search = filteredSearch
                            , webSocket = websocket
                            , dataLoading = loading
                        }

                    newParentProject =
                        project "child" (Just "new-parent")

                    changedProject =
                        { newParentProject | status = Api.ProjCompleted }

                    ( updated, _ ) =
                        WebSocket.update
                            (CanonicalNavigationSummariesFetched guard workspaceId [ "child" ] []
                                (Ok { projects = [ changedProject ], tasks = [], missingProjectIds = [], missingTaskIds = [] })
                            )
                            source
                in
                Expect.equal
                    ( Just (Just "new-parent"), True )
                    ( Dict.get "child" updated.dataLoading.projectCardSummaries |> Maybe.map .parentId
                    , Set.member "child" updated.dataLoading.navigationVisibleProjectIds
                    )
        , test "filtered canonical summaries remove missing cards from branch membership" <|
            \_ ->
                let
                    guard =
                        { scopeKey = "workspace:" ++ workspaceId, targetKey = "navigation-summaries:project:child", targetGeneration = 1, sessionEpoch = model.sessionRequestEpoch, routeWorkspace = Just workspaceId, audienceId = "editor" }

                    baseLoading =
                        model.dataLoading

                    loading =
                        { baseLoading | projectCardSummaries = Dict.singleton "child" (project "child" Nothing), navigationVisibleProjectIds = Set.singleton "child", navigationVisibilityActive = True }

                    filteredSearch =
                        { baseSearch | filterProjectStatuses = [ "completed" ] }

                    baseSearch =
                        model.search

                    baseWebSocket =
                        model.webSocket

                    websocket =
                        { baseWebSocket | targetGenerations = Dict.singleton "workspace:workspace-1|navigation-summaries:project:child" 1 }

                    source =
                        { model | auth = { status = AuthReady, mode = Just "test" }, sessionContext = Just editorSession, search = filteredSearch, webSocket = websocket, dataLoading = loading }

                    updated =
                        WebSocket.update (CanonicalNavigationSummariesFetched guard workspaceId [ "child" ] [] (Ok { projects = [], tasks = [], missingProjectIds = [ "child" ], missingTaskIds = [] })) source |> Tuple.first
                in
                Expect.equal ( Nothing, False )
                    ( Dict.get "child" updated.dataLoading.projectCardSummaries
                    , Set.member "child" updated.dataLoading.navigationVisibleProjectIds
                    )
        , test "completed root and expanded-branch replays replace filtered membership after a matching card becomes nonmatching" <|
            \_ ->
                let
                    rootCard =
                        project "root" Nothing

                    childCard =
                        project "child" (Just "parent")

                    matchingRoot =
                        { rootCard | status = Api.ProjCompleted }

                    matchingChild =
                        { childCard | status = Api.ProjCompleted }

                    parent =
                        project "parent" Nothing

                    baseSearch =
                        model.search

                    filteredSearch =
                        { baseSearch | filterProjectStatuses = [ "completed" ] }

                    filtered =
                        { model
                            | search = filteredSearch
                        }

                    seeded =
                        DataLoading.mergeNavigationSummaries [ matchingRoot, parent, matchingChild ] [] filtered

                    ( opened, _ ) =
                        DataLoading.beginNavigationBranch "project" workspaceId (Just "parent") seeded

                    ( loaded, _ ) =
                        DataLoading.update
                            (GotNavigationBranch workspaceId opened.sessionRequestEpoch opened.dataLoading.navigationGeneration "project:parent"
                                (opened.dataLoading.loadedNavigationBranches |> Dict.get "project:parent" |> Maybe.map .filterFingerprint |> Maybe.withDefault "")
                                0 0
                                (Ok { workspaceId = workspaceId, projects = { items = [ matchingChild ], hasMore = False }, tasks = { items = [], hasMore = False } })
                            )
                            opened

                    ( replaying, _ ) =
                        DataLoading.revalidateNavigationForFilters loaded

                    rootRequest =
                        replaying.dataLoading.rootNavigationRequest

                    rootCompleted =
                        case rootRequest of
                            Just request ->
                                DataLoading.update
                                    (GotRootNavigation workspaceId request.sessionEpoch Nothing request.generation request.filterFingerprint 0 0
                                        (Ok { workspaceId = workspaceId, projects = { items = [], hasMore = False }, tasks = { items = [], hasMore = False } })
                                    )
                                    replaying
                                    |> Tuple.first

                            Nothing ->
                                replaying

                    branchRequest =
                        Dict.get "project:parent" rootCompleted.dataLoading.loadedNavigationBranches

                    completed =
                        case branchRequest of
                            Just request ->
                                DataLoading.update
                                    (GotNavigationBranch workspaceId request.sessionEpoch request.generation "project:parent" request.filterFingerprint 0 0
                                        (Ok { workspaceId = workspaceId, projects = { items = [], hasMore = False }, tasks = { items = [], hasMore = False } })
                                    )
                                    rootCompleted
                                    |> Tuple.first

                            Nothing ->
                                rootCompleted
                in
                Expect.equal
                    { rootRemoved = True, deepRemoved = True, rootApplied = True, branchApplied = True }
                    { rootRemoved = (Dict.member "root" completed.dataLoading.projectCardSummaries |> not) && (Set.member "root" completed.dataLoading.navigationVisibleProjectIds |> not)
                    , deepRemoved = (Dict.member "child" completed.dataLoading.projectCardSummaries |> not) && (Set.member "child" completed.dataLoading.navigationVisibleProjectIds |> not)
                    , rootApplied = completed.dataLoading.rootNavigationRequest |> Maybe.map .succeeded |> Maybe.withDefault False
                    , branchApplied = Dict.get "project:parent" completed.dataLoading.loadedNavigationBranches |> Maybe.map .succeeded |> Maybe.withDefault False
                    }
        , test "successful branch page 50 merges and deduplicates earlier project and task pages" <|
            \_ ->
                let
                    seeded =
                        DataLoading.mergeNavigationSummaries [ project "parent" Nothing ] [] model

                    ( initialRequest, _ ) =
                        DataLoading.beginNavigationBranch "project" workspaceId (Just "parent") seeded

                    initialFingerprint =
                        Dict.get "project:parent" initialRequest.dataLoading.loadedNavigationBranches
                            |> Maybe.map .filterFingerprint
                            |> Maybe.withDefault ""

                    ( firstPage, _ ) =
                        DataLoading.update
                            (GotNavigationBranch workspaceId initialRequest.sessionRequestEpoch initialRequest.dataLoading.navigationGeneration "project:parent" initialFingerprint 0 0
                                (Ok { workspaceId = workspaceId, projects = { items = [ project "project-0" (Just "parent") ], hasMore = True }, tasks = { items = [ task "task-0" Nothing ], hasMore = True } })
                            )
                            initialRequest

                    ( projectPageRequest, _ ) =
                        DataLoading.beginNavigationBranchPage "project" "parent" "project" firstPage

                    projectPageFingerprint =
                        Dict.get "project:parent" projectPageRequest.dataLoading.loadedNavigationBranches
                            |> Maybe.map .filterFingerprint
                            |> Maybe.withDefault ""

                    ( projectPage, _ ) =
                        DataLoading.update
                            (GotNavigationBranch workspaceId projectPageRequest.sessionRequestEpoch projectPageRequest.dataLoading.navigationGeneration "project:parent" projectPageFingerprint 50 0
                                (Ok { workspaceId = workspaceId, projects = { items = [ project "project-50" (Just "parent") ], hasMore = False }, tasks = { items = [ task "task-0" Nothing ], hasMore = True } })
                            )
                            projectPageRequest

                    ( taskPageRequest, _ ) =
                        DataLoading.beginNavigationBranchPage "project" "parent" "task" projectPage

                    taskPageFingerprint =
                        Dict.get "project:parent" taskPageRequest.dataLoading.loadedNavigationBranches
                            |> Maybe.map .filterFingerprint
                            |> Maybe.withDefault ""

                    ( completed, _ ) =
                        DataLoading.update
                            (GotNavigationBranch workspaceId taskPageRequest.sessionRequestEpoch taskPageRequest.dataLoading.navigationGeneration "project:parent" taskPageFingerprint 50 50
                                (Ok { workspaceId = workspaceId, projects = { items = [ project "project-50" (Just "parent") ], hasMore = False }, tasks = { items = [ task "task-50" Nothing ], hasMore = False } })
                            )
                            taskPageRequest
                in
                Expect.equal
                    ( [ "project-0", "project-50" ], [ "task-0", "task-50" ] )
                    ( completed.dataLoading.projectCardSummaries |> Dict.keys |> List.filter (\id -> String.startsWith "project-" id)
                    , completed.dataLoading.taskCardSummaries |> Dict.keys
                    )
        , test "a reparent into an unloaded branch is checked authoritatively and removes a completed nonmatch" <|
            \_ ->
                let
                    child =
                        project "child" (Just "old-parent")

                    seeded =
                        DataLoading.mergeNavigationSummaries [ child ] [] model

                    ( oldRequest, _ ) =
                        DataLoading.beginNavigationBranch "project" workspaceId (Just "old-parent") seeded

                    oldFingerprint =
                        Dict.get "project:old-parent" oldRequest.dataLoading.loadedNavigationBranches
                            |> Maybe.map .filterFingerprint
                            |> Maybe.withDefault ""

                    ( oldLoaded, _ ) =
                        DataLoading.update
                            (GotNavigationBranch workspaceId oldRequest.sessionRequestEpoch oldRequest.dataLoading.navigationGeneration "project:old-parent" oldFingerprint 0 0
                                (Ok { workspaceId = workspaceId, projects = { items = [ child ], hasMore = False }, tasks = { items = [], hasMore = False } })
                            )
                            oldRequest

                    moved =
                        { child | parentId = Just "new-parent", status = Api.ProjActive }

                    movedModel =
                        DataLoading.mergeNavigationSummaries [ moved ] [] oldLoaded

                    ( replaying, _ ) =
                        DataLoading.revalidateNavigationForAffectedBranches [ moved ] [] movedModel

                    oldReplay =
                        Dict.get "project:old-parent" replaying.dataLoading.loadedNavigationBranches

                    newReplay =
                        Dict.get "project:new-parent" replaying.dataLoading.loadedNavigationBranches

                    afterOld =
                        case oldReplay of
                            Just request ->
                                DataLoading.update
                                    (GotNavigationBranch workspaceId request.sessionEpoch request.generation "project:old-parent" request.filterFingerprint 0 0
                                        (Ok { workspaceId = workspaceId, projects = { items = [], hasMore = False }, tasks = { items = [], hasMore = False } })
                                    )
                                    replaying
                                    |> Tuple.first

                            Nothing ->
                                replaying

                    completed =
                        case newReplay of
                            Just request ->
                                DataLoading.update
                                    (GotNavigationBranch workspaceId request.sessionEpoch request.generation "project:new-parent" request.filterFingerprint 0 0
                                        (Ok { workspaceId = workspaceId, projects = { items = [], hasMore = False }, tasks = { items = [], hasMore = False } })
                                    )
                                    afterOld
                                    |> Tuple.first

                            Nothing ->
                                afterOld
                in
                Expect.equal
                    { oldWasReplayed = True, newWasChecked = True, staleCardRemoved = True }
                    { oldWasReplayed = oldReplay /= Nothing
                    , newWasChecked = newReplay /= Nothing
                    , staleCardRemoved = Dict.member "child" completed.dataLoading.projectCardSummaries |> not
                    }
        ]


workspaceId : String
workspaceId =
    "workspace-1"


filterFingerprint : String
filterFingerprint =
    "all|any|||"


model : Model
model =
    AppShell.initModel Nothing url (WorkspacePage workspaceId) flags Nothing { tab = ProjectsTab, focus = Nothing, observationId = Nothing }
        |> AppShell.finalizeInit (WorkspacePage workspaceId)


rootLoadModel : Model
rootLoadModel =
    let
        loading =
            model.dataLoading
    in
    { model | dataLoading = { loading | activeWorkspaceLoadToken = Just 1, pendingWorkspaceLoads = 1, loadingWorkspaceData = True } }


rootResponse : Api.NavigationBranchResponse
rootResponse =
    { workspaceId = workspaceId
    , projects = { items = [ project "stale-root" Nothing ], hasMore = False }
    , tasks = { items = [], hasMore = False }
    }


staleResponse : Api.NavigationBranchResponse
staleResponse =
    { workspaceId = workspaceId
    , projects = { items = [ project "stale-branch" (Just "parent") ], hasMore = False }
    , tasks = { items = [], hasMore = False }
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


editorSession : Api.SessionContext
editorSession =
    { authMode = "test"
    , principal = { actorType = "user", actorId = "editor", actorLabel = "Editor", authority = "local", grantUserId = Nothing }
    , globalPermissions = { createWorkspace = False, superadmin = False }
    , workspace = Just { workspaceId = workspaceId, role = Just "edit", canRead = True, canEdit = True, canAdmin = False }
    }


project : String -> Maybe String -> Api.ProjectCardSummary
project id parentId =
    { id = id
    , workspaceId = workspaceId
    , parentId = parentId
    , name = id
    , status = Api.ProjActive
    , priority = 1
    , createdAt = "2026-01-01T00:00:00Z"
    , updatedAt = "2026-01-01T00:00:00Z"
    , directProjectCount = 0
    , directTaskCount = 0
    , hasChildren = False
    , readinessRollup = { openProjectCount = 0, closedProjectCount = 0, openTaskCount = 0, doneTaskCount = 0, cancelledTaskCount = 0, blockedTaskCount = 0, dependencyBlockedTaskCount = 0, openDependencyCount = 0, completionReady = True }
    }


task : String -> Maybe String -> Api.TaskCardSummary
task id parentId =
    { id = id
    , workspaceId = workspaceId
    , projectId = Just "parent"
    , parentId = parentId
    , title = id
    , status = Api.Todo
    , priority = 1
    , dueAt = Nothing
    , completedAt = Nothing
    , dependencyCount = 0
    , createdAt = "2026-01-01T00:00:00Z"
    , updatedAt = "2026-01-01T00:00:00Z"
    , directSubtaskCount = 0
    , hasChildren = False
    , readinessRollup = { openSubtaskCount = 0, doneSubtaskCount = 0, cancelledSubtaskCount = 0, blockedSubtaskCount = 0, dependencyBlockedTaskCount = 0, openDependencyCount = 0, completionReady = True }
    }
