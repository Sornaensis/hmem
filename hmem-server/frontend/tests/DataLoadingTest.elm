module DataLoadingTest exposing (suite)

import Api
import AppShell
import Dict
import Expect
import Feature.Cards as Cards
import Feature.DataLoading as DataLoading
import Feature.Dependencies as Dependencies
import Feature.Mutations as Mutations
import Feature.WebSocket as WebSocket
import Http
import Json.Encode as Encode
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
        , test "accepted root navigation hydrates descriptions and loads an initially expanded direct branch" <|
            \_ ->
                let
                    expandedProject =
                        let
                            summary =
                                project "expanded-root" Nothing
                        in
                        { summary | hasChildren = True, directTaskCount = 1 }

                    unassignedTask =
                        let
                            summary =
                                task "unassigned-root" Nothing
                        in
                        { summary | projectId = Nothing }

                    prepared =
                        DataLoading.prepareRootNavigationRequest (Just workspaceId) rootLoadModel

                    ( updated, _ ) =
                        DataLoading.update
                            (GotRootNavigation workspaceId prepared.sessionRequestEpoch (Just 1) prepared.dataLoading.navigationGeneration filterFingerprint 0 0
                                (Ok { workspaceId = workspaceId, projects = { items = [ expandedProject ], hasMore = False }, tasks = { items = [ unassignedTask ], hasMore = False } })
                            )
                            prepared
                in
                Expect.equal
                    { projectDetail = True, taskDetail = True, branchInFlight = True, rootSettled = True }
                    { projectDetail = Dict.get expandedProject.id updated.dataLoading.projectCardDetailRequests |> Maybe.map .inFlight |> Maybe.withDefault False
                    , taskDetail = Dict.get unassignedTask.id updated.dataLoading.taskCardDetailRequests |> Maybe.map .inFlight |> Maybe.withDefault False
                    , branchInFlight = Dict.get ("project:" ++ expandedProject.id) updated.dataLoading.loadedNavigationBranches |> Maybe.map .inFlight |> Maybe.withDefault False
                    , rootSettled = updated.dataLoading.rootNavigationRequest |> Maybe.map (.inFlight >> not) |> Maybe.withDefault False
                    }
        , test "card detail hydration applies canonical descriptions and rejects a late superseded response" <|
            \_ ->
                let
                    summary =
                        project "described" Nothing

                    seeded =
                        DataLoading.mergeNavigationSummaries [ summary ] [] model

                    ( requested, _ ) =
                        DataLoading.ensureNavigationPresentation "workspace_root" Nothing seeded

                    firstRequest =
                        Dict.get summary.id requested.dataLoading.projectCardDetailRequests

                    firstDetail =
                        let
                            detail =
                                Api.projectFromCardSummary summary
                        in
                        { detail | description = Just "Canonical description" }

                    applied =
                        case firstRequest of
                            Just request ->
                                DataLoading.update (GotProjectCardDetail request summary.id (Ok firstDetail)) requested |> Tuple.first

                            Nothing ->
                                requested

                    newerSummary =
                        { summary | updatedAt = "2026-01-02T00:00:00Z" }

                    ( superseded, _ ) =
                        DataLoading.mergeNavigationSummaries [ newerSummary ] [] applied
                            |> DataLoading.ensureNavigationPresentation "workspace_root" Nothing

                    afterLate =
                        case firstRequest of
                            Just request ->
                                DataLoading.update (GotProjectCardDetail request summary.id (Ok firstDetail)) superseded |> Tuple.first

                            Nothing ->
                                superseded
                in
                Expect.equal
                    { applied = Just "Canonical description", clearedForRefresh = Nothing, lateIgnored = Nothing, superseded = True }
                    { applied = Dict.get summary.id applied.projects |> Maybe.andThen .description
                    , clearedForRefresh = Dict.get summary.id superseded.projects |> Maybe.andThen .description
                    , lateIgnored = Dict.get summary.id afterLate.projects |> Maybe.andThen .description
                    , superseded =
                        case ( firstRequest, Dict.get summary.id superseded.dataLoading.projectCardDetailRequests ) of
                            ( Just first, Just second ) ->
                                second.requestId > first.requestId

                            _ ->
                                False
                    }
        , test "newer canonical project and task details remain hydrated across ensure and Expand All" <|
            \_ ->
                let
                    projectSummary =
                        project "detail-newer-project" Nothing

                    taskSummary =
                        let
                            summary =
                                task "detail-newer-task" Nothing
                        in
                        { summary | projectId = Nothing }

                    seeded =
                        DataLoading.mergeNavigationSummaries [ projectSummary ] [ taskSummary ] model

                    ( requested, _ ) =
                        DataLoading.ensureNavigationPresentation "workspace_root" Nothing seeded

                    projectRequest =
                        Dict.get projectSummary.id requested.dataLoading.projectCardDetailRequests

                    taskRequest =
                        Dict.get taskSummary.id requested.dataLoading.taskCardDetailRequests

                    hydrated =
                        case ( projectRequest, taskRequest ) of
                            ( Just pendingProject, Just pendingTask ) ->
                                let
                                    projectDetail =
                                        Api.projectFromCardSummary projectSummary

                                    taskDetail =
                                        Api.taskFromCardSummary taskSummary

                                    newerProject =
                                        { projectDetail | description = Just "newer project detail", updatedAt = "2026-01-05T00:00:00Z" }

                                    newerTask =
                                        { taskDetail | description = Just "newer task detail", updatedAt = "2026-01-05T00:00:00Z" }
                                in
                                requested
                                    |> DataLoading.update (GotProjectCardDetail pendingProject projectSummary.id (Ok newerProject))
                                    |> Tuple.first
                                    |> DataLoading.update (GotTaskCardDetail pendingTask taskSummary.id (Ok newerTask))
                                    |> Tuple.first

                            _ ->
                                requested

                    ensured =
                        DataLoading.ensureNavigationPresentation "workspace_root" Nothing hydrated |> Tuple.first

                    expanded =
                        Cards.update ExpandAllNodes ensured |> Tuple.first

                    projectState =
                        Dict.get projectSummary.id expanded.dataLoading.projectCardDetailRequests

                    taskState =
                        Dict.get taskSummary.id expanded.dataLoading.taskCardDetailRequests
                in
                Expect.equal
                    { descriptions = ( Just "newer project detail", Just "newer task detail" )
                    , projectSettled = True
                    , taskSettled = True
                    , requestIdsUnchanged =
                        ( projectRequest |> Maybe.map .requestId
                        , taskRequest |> Maybe.map .requestId
                        )
                    }
                    { descriptions =
                        ( Dict.get projectSummary.id expanded.projects |> Maybe.andThen .description
                        , Dict.get taskSummary.id expanded.tasks |> Maybe.andThen .description
                        )
                    , projectSettled = projectState |> Maybe.map (\state -> state.expectedUpdatedAt == "2026-01-05T00:00:00Z" && state.succeeded && not state.inFlight) |> Maybe.withDefault False
                    , taskSettled = taskState |> Maybe.map (\state -> state.expectedUpdatedAt == "2026-01-05T00:00:00Z" && state.succeeded && not state.inFlight) |> Maybe.withDefault False
                    , requestIdsUnchanged =
                        ( projectState |> Maybe.map .requestId
                        , taskState |> Maybe.map .requestId
                        )
                    }
        , test "filter reload retires project and task detail requests across the response gap" <|
            \_ ->
                let
                    projectSummary =
                        project "filter-project" Nothing

                    taskSummary =
                        let
                            summary =
                                task "filter-task" Nothing
                        in
                        { summary | projectId = Nothing }

                    seeded =
                        DataLoading.mergeNavigationSummaries [ projectSummary ] [ taskSummary ] model

                    ( requested, _ ) =
                        DataLoading.ensureNavigationPresentation "workspace_root" Nothing seeded

                    projectRequest =
                        Dict.get projectSummary.id requested.dataLoading.projectCardDetailRequests

                    taskRequest =
                        Dict.get taskSummary.id requested.dataLoading.taskCardDetailRequests

                    ( reloaded, _ ) =
                        DataLoading.reloadNavigationForFilters requested

                    afterGap =
                        case ( projectRequest, taskRequest ) of
                            ( Just oldProjectRequest, Just oldTaskRequest ) ->
                                let
                                    detail =
                                        Api.projectFromCardSummary projectSummary
                                in
                                reloaded
                                    |> DataLoading.update (GotProjectCardDetail oldProjectRequest projectSummary.id (Ok { detail | description = Just "stale" }))
                                    |> Tuple.first
                                    |> DataLoading.update (GotTaskCardDetail oldTaskRequest taskSummary.id (Err Http.Timeout))
                                    |> Tuple.first

                            _ ->
                                reloaded

                    replaced =
                        case afterGap.dataLoading.rootNavigationRequest of
                            Just request ->
                                DataLoading.update
                                    (GotRootNavigation workspaceId request.sessionEpoch Nothing request.generation request.filterFingerprint 0 0
                                        (Ok { workspaceId = workspaceId, projects = { items = [ projectSummary ], hasMore = False }, tasks = { items = [ taskSummary ], hasMore = False } })
                                    )
                                    afterGap
                                    |> Tuple.first

                            Nothing ->
                                afterGap
                in
                Expect.equal
                    { retiredDuringGap = True, projectRestarted = True, taskRestarted = True, staleDescriptionIgnored = True }
                    { retiredDuringGap = Dict.isEmpty afterGap.dataLoading.projectCardDetailRequests && Dict.isEmpty afterGap.dataLoading.taskCardDetailRequests
                    , projectRestarted =
                        case ( projectRequest, Dict.get projectSummary.id replaced.dataLoading.projectCardDetailRequests ) of
                            ( Just old, Just fresh ) ->
                                fresh.inFlight && fresh.requestId > old.requestId

                            _ ->
                                False
                    , taskRestarted =
                        case ( taskRequest, Dict.get taskSummary.id replaced.dataLoading.taskCardDetailRequests ) of
                            ( Just old, Just fresh ) ->
                                fresh.inFlight && fresh.requestId > old.requestId

                            _ ->
                                False
                    , staleDescriptionIgnored = Dict.get projectSummary.id afterGap.projects |> Maybe.andThen .description |> (==) Nothing
                    }
        , test "successful project and task mutations fence older in-flight detail responses" <|
            \_ ->
                let
                    projectSummary =
                        project "mutated-project" Nothing

                    taskSummary =
                        let
                            summary =
                                task "mutated-task" Nothing
                        in
                        { summary | projectId = Nothing }

                    seeded =
                        DataLoading.mergeNavigationSummaries [ projectSummary ] [ taskSummary ] model

                    ( requested, _ ) =
                        DataLoading.ensureNavigationPresentation "workspace_root" Nothing seeded

                    projectRequest =
                        Dict.get projectSummary.id requested.dataLoading.projectCardDetailRequests

                    taskRequest =
                        Dict.get taskSummary.id requested.dataLoading.taskCardDetailRequests

                    oldProject =
                        Api.projectFromCardSummary projectSummary

                    oldTask =
                        Api.taskFromCardSummary taskSummary

                    mutatedProject =
                        { oldProject | name = "new project", status = Api.ProjPaused, priority = 9, description = Just "new project description", updatedAt = "2026-01-03T00:00:00Z" }

                    mutatedTask =
                        { oldTask | title = "new task", status = Api.InProgress, priority = 8, description = Just "new task description", updatedAt = "2026-01-03T00:00:00Z" }

                    afterMutations =
                        requested
                            |> Mutations.update (ProjectUpdated (Ok mutatedProject))
                            |> Tuple.first
                            |> Mutations.update (TaskUpdated (Ok { task = mutatedTask, dependencyEffects = [] }))
                            |> Tuple.first

                    afterLateDetails =
                        case ( projectRequest, taskRequest ) of
                            ( Just staleProjectRequest, Just staleTaskRequest ) ->
                                afterMutations
                                    |> DataLoading.update (GotProjectCardDetail staleProjectRequest projectSummary.id (Ok { oldProject | description = Just "old project description" }))
                                    |> Tuple.first
                                    |> DataLoading.update (GotTaskCardDetail staleTaskRequest taskSummary.id (Ok { oldTask | description = Just "old task description" }))
                                    |> Tuple.first

                            _ ->
                                afterMutations
                in
                Expect.equal
                    { project = Just { name = "new project", status = Api.ProjPaused, priority = 9, description = Just "new project description" }
                    , task = Just { title = "new task", status = Api.InProgress, priority = 8, description = Just "new task description" }
                    , projectDetailCurrent = True
                    , taskDetailCurrent = True
                    }
                    { project = Dict.get projectSummary.id afterLateDetails.projects |> Maybe.map (\value -> { name = value.name, status = value.status, priority = value.priority, description = value.description })
                    , task = Dict.get taskSummary.id afterLateDetails.tasks |> Maybe.map (\value -> { title = value.title, status = value.status, priority = value.priority, description = value.description })
                    , projectDetailCurrent = Dict.get projectSummary.id afterLateDetails.dataLoading.projectCardDetailRequests |> Maybe.map (\state -> state.expectedUpdatedAt == mutatedProject.updatedAt && state.succeeded && not state.inFlight) |> Maybe.withDefault False
                    , taskDetailCurrent = Dict.get taskSummary.id afterLateDetails.dataLoading.taskCardDetailRequests |> Maybe.map (\state -> state.expectedUpdatedAt == mutatedTask.updatedAt && state.succeeded && not state.inFlight) |> Maybe.withDefault False
                    }
        , test "unfiltered live summaries rehydrate visible newer project and task descriptions" <|
            \_ ->
                let
                    projectSummary =
                        project "live-project" Nothing

                    taskSummary =
                        let
                            summary =
                                task "live-task" Nothing
                        in
                        { summary | projectId = Nothing }

                    seeded =
                        DataLoading.mergeNavigationSummaries [ projectSummary ] [ taskSummary ] model

                    ( requested, _ ) =
                        DataLoading.ensureNavigationPresentation "workspace_root" Nothing seeded

                    firstProjectRequest =
                        Dict.get projectSummary.id requested.dataLoading.projectCardDetailRequests

                    firstTaskRequest =
                        Dict.get taskSummary.id requested.dataLoading.taskCardDetailRequests

                    hydrated =
                        case ( firstProjectRequest, firstTaskRequest ) of
                            ( Just projectRequest, Just taskRequest ) ->
                                let
                                    projectDetail =
                                        Api.projectFromCardSummary projectSummary

                                    taskDetail =
                                        Api.taskFromCardSummary taskSummary
                                in
                                requested
                                    |> DataLoading.update (GotProjectCardDetail projectRequest projectSummary.id (Ok { projectDetail | description = Just "old project" }))
                                    |> Tuple.first
                                    |> DataLoading.update (GotTaskCardDetail taskRequest taskSummary.id (Ok { taskDetail | description = Just "old task" }))
                                    |> Tuple.first

                            _ ->
                                requested

                    guard =
                        { scopeKey = "workspace:" ++ workspaceId
                        , targetKey = "navigation-summaries:live"
                        , targetGeneration = 1
                        , sessionEpoch = model.sessionRequestEpoch
                        , routeWorkspace = Just workspaceId
                        , audienceId = "editor"
                        }

                    newerProject =
                        { projectSummary | name = "live project newer", updatedAt = "2026-01-04T00:00:00Z" }

                    newerTask =
                        { taskSummary | title = "live task newer", updatedAt = "2026-01-04T00:00:00Z" }

                    baseWebSocket =
                        hydrated.webSocket

                    source =
                        { hydrated
                            | auth = { status = AuthReady, mode = Just "test" }
                            , sessionContext = Just editorSession
                            , webSocket = { baseWebSocket | targetGenerations = Dict.singleton (guard.scopeKey ++ "|" ++ guard.targetKey) 1 }
                        }

                    updated =
                        WebSocket.update
                            (CanonicalNavigationSummariesFetched guard workspaceId [ projectSummary.id ] [ taskSummary.id ]
                                (Ok { projects = [ newerProject ], tasks = [ newerTask ], missingProjectIds = [], missingTaskIds = [] })
                            )
                            source
                            |> Tuple.first
                in
                Expect.equal
                    { descriptionsCleared = ( Nothing, Nothing ), projectRehydrating = True, taskRehydrating = True }
                    { descriptionsCleared =
                        ( Dict.get projectSummary.id updated.projects |> Maybe.andThen .description
                        , Dict.get taskSummary.id updated.tasks |> Maybe.andThen .description
                        )
                    , projectRehydrating =
                        case ( firstProjectRequest, Dict.get projectSummary.id updated.dataLoading.projectCardDetailRequests ) of
                            ( Just old, Just fresh ) ->
                                fresh.expectedUpdatedAt == newerProject.updatedAt && fresh.inFlight && fresh.requestId > old.requestId

                            _ ->
                                False
                    , taskRehydrating =
                        case ( firstTaskRequest, Dict.get taskSummary.id updated.dataLoading.taskCardDetailRequests ) of
                            ( Just old, Just fresh ) ->
                                fresh.expectedUpdatedAt == newerTask.updatedAt && fresh.inFlight && fresh.requestId > old.requestId

                            _ ->
                                False
                    }
        , test "pinned project and task paging hydrate the same cached window the renderer presents" <|
            \_ ->
                let
                    numbered prefix number =
                        prefix ++ String.padLeft 3 '0' (String.fromInt number)

                    projects =
                        List.range 1 60 |> List.map (\number -> project (numbered "project-" number) Nothing)

                    tasks =
                        List.range 1 60
                            |> List.map
                                (\number ->
                                    let
                                        summary =
                                            task (numbered "task-" number) Nothing
                                    in
                                    { summary | projectId = Nothing }
                                )

                    prepare entityKind entityId projectItems taskItems =
                        let
                            focus =
                                model.focus

                            focused =
                                { rootLoadModel | focus = { focus | focusedEntity = Just ( entityKind, entityId ) } }

                            requestedRoot =
                                DataLoading.prepareRootNavigationRequest (Just workspaceId) focused

                            loaded =
                                DataLoading.update
                                    (GotRootNavigation workspaceId requestedRoot.sessionRequestEpoch (Just 1) requestedRoot.dataLoading.navigationGeneration filterFingerprint 0 0
                                        (Ok { workspaceId = workspaceId, projects = { items = projectItems, hasMore = False }, tasks = { items = taskItems, hasMore = False } })
                                    )
                                    requestedRoot
                                    |> Tuple.first

                            loading =
                                loaded.dataLoading

                            offsetPresentation =
                                loading.rootNavigationPresentation
                                    |> Maybe.map
                                        (\presentation ->
                                            if entityKind == "project" then
                                                { presentation | projectOffset = 24 }

                                            else
                                                { presentation | taskOffset = 24 }
                                        )

                            isolated =
                                { loaded
                                    | dataLoading =
                                        { loading
                                            | rootNavigationPresentation = offsetPresentation
                                            , projectCardDetailRequests = Dict.empty
                                            , taskCardDetailRequests = Dict.empty
                                        }
                                }
                        in
                        DataLoading.ensureNavigationPresentation "workspace_root" Nothing isolated |> Tuple.first

                    projectPage =
                        prepare "project" "project-050" projects []

                    taskPage =
                        prepare "task" "task-050" [] tasks

                    projectRequestIds =
                        projectPage.dataLoading.projectCardDetailRequests |> Dict.keys |> Set.fromList

                    taskRequestIds =
                        taskPage.dataLoading.taskCardDetailRequests |> Dict.keys |> Set.fromList

                    expectedProjects =
                        projects |> List.map .id |> Cards.presentationWindow identity (Set.singleton "project-050") 24 |> Set.fromList

                    expectedTasks =
                        tasks |> List.map .id |> Cards.presentationWindow identity (Set.singleton "task-050") 24 |> Set.fromList
                in
                Expect.equal
                    { projectWindow = expectedProjects, taskWindow = expectedTasks, pinnedProjectHydrated = True, pinnedTaskHydrated = True }
                    { projectWindow = projectRequestIds
                    , taskWindow = taskRequestIds
                    , pinnedProjectHydrated = Set.member "project-050" projectRequestIds
                    , pinnedTaskHydrated = Set.member "task-050" taskRequestIds
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
        , test "late stored navigation filters supersede bootstrap loading without leaving the root in flight forever" <|
            \_ ->
                let
                    prepared =
                        DataLoading.prepareRootNavigationRequest (Just workspaceId)
                            { rootLoadModel | auth = { status = AuthReady, mode = Just "test" }, sessionContext = Just editorSession }

                    stored =
                        Encode.object
                            [ ( "workspaceId", Encode.string workspaceId )
                            , ( "filterTaskStatuses", Encode.list Encode.string [ "done" ] )
                            ]

                    ( reloaded, _ ) =
                        AppShell.handleOwned (AppShell.LocalStorageLoadedMsg stored) prepared

                    oldResponse =
                        DataLoading.update
                            (GotRootNavigation workspaceId prepared.sessionRequestEpoch (Just 1) prepared.dataLoading.navigationGeneration filterFingerprint 0 0 (Ok rootResponse))
                            reloaded
                            |> Tuple.first
                in
                Expect.equal
                    { generationAdvanced = True, storedFilterApplied = [ "done" ], blockingLoadSettled = True, replacementInFlight = True, oldRejected = True }
                    { generationAdvanced = reloaded.dataLoading.navigationGeneration > prepared.dataLoading.navigationGeneration
                    , storedFilterApplied = reloaded.search.filterTaskStatuses
                    , blockingLoadSettled = not reloaded.dataLoading.loadingWorkspaceData && reloaded.dataLoading.pendingWorkspaceLoads == 0
                    , replacementInFlight = reloaded.dataLoading.rootNavigationRequest |> Maybe.map .inFlight |> Maybe.withDefault False
                    , oldRejected = Dict.member "stale-root" oldResponse.dataLoading.projectCardSummaries |> not
                    }
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
        , test "root presentation paging advances one bounded offset and retains prior cached membership" <|
            \_ ->
                let
                    prepared =
                        DataLoading.prepareRootNavigationRequest (Just workspaceId) rootLoadModel

                    ( firstPage, _ ) =
                        DataLoading.update
                            (GotRootNavigation workspaceId prepared.sessionRequestEpoch (Just 1) prepared.dataLoading.navigationGeneration filterFingerprint 0 0
                                (Ok { workspaceId = workspaceId, projects = { items = [ project "root-one" Nothing ], hasMore = True }, tasks = { items = [], hasMore = False } })
                            )
                            prepared

                    ( cachedWindow, _ ) =
                        DataLoading.update (LoadRootNavigationPage "project") firstPage

                    ( requested, _ ) =
                        DataLoading.update (LoadRootNavigationPage "project") cachedWindow

                    stale =
                        DataLoading.update
                            (GotRootNavigation workspaceId requested.sessionRequestEpoch Nothing requested.dataLoading.navigationGeneration filterFingerprint 0 0
                                (Ok { workspaceId = workspaceId, projects = { items = [ project "stale-root-page" Nothing ], hasMore = False }, tasks = { items = [], hasMore = False } })
                            )
                            requested
                            |> Tuple.first

                    ( completed, _ ) =
                        DataLoading.update
                            (GotRootNavigation workspaceId requested.sessionRequestEpoch Nothing requested.dataLoading.navigationGeneration filterFingerprint 50 0
                                (Ok { workspaceId = workspaceId, projects = { items = [ project "root-two" Nothing ], hasMore = False }, tasks = { items = [], hasMore = False } })
                            )
                            requested
                in
                Expect.equal
                    { offset = Just 50, first = True, second = True, staleRejected = False, complete = True }
                    { offset = requested.dataLoading.rootNavigationRequest |> Maybe.map .projectOffset
                    , first = Dict.member "root-one" completed.dataLoading.projectCardSummaries
                    , second = Dict.member "root-two" completed.dataLoading.projectCardSummaries
                    , staleRejected = Dict.member "stale-root-page" stale.dataLoading.projectCardSummaries
                    , complete = completed.dataLoading.rootNavigationRequest |> Maybe.map .succeeded |> Maybe.withDefault False
                    }
        , test "root navigation retry keeps its failed cursor and cached membership" <|
            \_ ->
                let
                    prepared =
                        DataLoading.prepareRootNavigationRequest (Just workspaceId) rootLoadModel

                    ( firstPage, _ ) =
                        DataLoading.update
                            (GotRootNavigation workspaceId prepared.sessionRequestEpoch (Just 1) prepared.dataLoading.navigationGeneration filterFingerprint 0 0
                                (Ok { workspaceId = workspaceId, projects = { items = [ project "retained" Nothing ], hasMore = True }, tasks = { items = [], hasMore = False } })
                            )
                            prepared

                    ( requested, _ ) =
                        DataLoading.update (LoadRootNavigationPage "project") firstPage

                    failed =
                        DataLoading.update
                            (GotRootNavigation workspaceId requested.sessionRequestEpoch Nothing requested.dataLoading.navigationGeneration filterFingerprint 50 0 (Err (Http.BadUrl "fixture failure")))
                            requested
                            |> Tuple.first

                    retried =
                        DataLoading.update (LoadRootNavigationPage "project") failed |> Tuple.first
                in
                Expect.equal
                    { retriedProjectOffset = Just 50, retriedTaskOffset = Just 0, inFlight = True, retained = True }
                    { retriedProjectOffset = retried.dataLoading.rootNavigationRequest |> Maybe.map .projectOffset
                    , retriedTaskOffset = retried.dataLoading.rootNavigationRequest |> Maybe.map .taskOffset
                    , inFlight = retried.dataLoading.rootNavigationRequest |> Maybe.map .inFlight |> Maybe.withDefault False
                     , retained = Dict.member "retained" retried.dataLoading.projectCardSummaries
                     }
        , test "task presentation windows use their own root and branch cursors for next, previous, and retry" <|
            \_ ->
                let
                    prepared =
                        DataLoading.prepareRootNavigationRequest (Just workspaceId) rootLoadModel

                    ( rootFirst, _ ) =
                        DataLoading.update
                            (GotRootNavigation workspaceId prepared.sessionRequestEpoch (Just 1) prepared.dataLoading.navigationGeneration filterFingerprint 0 0
                                (Ok { workspaceId = workspaceId, projects = { items = [], hasMore = False }, tasks = { items = [ task "root-task-0" Nothing ], hasMore = True } })
                            )
                            prepared

                    ( rootCached, _ ) =
                        DataLoading.update (LoadRootNavigationPage "task") rootFirst

                    ( rootRequested, _ ) =
                        DataLoading.update (LoadRootNavigationPage "task") rootCached

                    rootFailed =
                        DataLoading.update
                            (GotRootNavigation workspaceId rootRequested.sessionRequestEpoch Nothing rootRequested.dataLoading.navigationGeneration filterFingerprint 0 50 (Err (Http.BadUrl "task root retry")))
                            rootRequested
                            |> Tuple.first

                    rootRetried =
                        DataLoading.update (LoadRootNavigationPage "task") rootFailed |> Tuple.first

                    rootPrevious =
                        DataLoading.update (ShowPreviousRootNavigationPage "task") rootRetried |> Tuple.first

                    ( branchInitial, _ ) =
                        DataLoading.beginNavigationBranch "project" workspaceId (Just "parent") model

                    branchFingerprint =
                        Dict.get "project:parent" branchInitial.dataLoading.loadedNavigationBranches
                            |> Maybe.map .filterFingerprint
                            |> Maybe.withDefault ""

                    ( branchFirst, _ ) =
                        DataLoading.update
                            (GotNavigationBranch workspaceId branchInitial.sessionRequestEpoch branchInitial.dataLoading.navigationGeneration "project:parent" branchFingerprint 0 0
                                (Ok { workspaceId = workspaceId, projects = { items = [], hasMore = False }, tasks = { items = [ task "branch-task-0" Nothing ], hasMore = True } })
                            )
                            branchInitial

                    ( branchCached, _ ) =
                        DataLoading.beginNavigationBranchPage "project" "parent" "task" branchFirst

                    ( branchRequested, _ ) =
                        DataLoading.beginNavigationBranchPage "project" "parent" "task" branchCached

                    branchFailed =
                        DataLoading.update
                            (GotNavigationBranch workspaceId branchRequested.sessionRequestEpoch branchRequested.dataLoading.navigationGeneration "project:parent" branchFingerprint 0 50 (Err (Http.BadUrl "task branch retry")))
                            branchRequested
                            |> Tuple.first

                    branchRetried =
                        DataLoading.beginNavigationBranchPage "project" "parent" "task" branchFailed |> Tuple.first

                    branchPrevious =
                        DataLoading.beginNavigationBranchPreviousPage "project" "parent" "task" branchRetried |> Tuple.first
                in
                Expect.equal
                    { rootRetryOffset = Just 50
                    , rootProjectUntouched = Just 0
                    , rootPreviousOffset = Just 0
                    , branchRetryOffset = Just 50
                    , branchProjectUntouched = Just 0
                    , branchPreviousOffset = Just 0
                    , rootRetained = True
                    , branchRetained = True
                    }
                    { rootRetryOffset = rootRetried.dataLoading.rootNavigationRequest |> Maybe.map .taskOffset
                    , rootProjectUntouched = rootRetried.dataLoading.rootNavigationRequest |> Maybe.map .projectOffset
                    , rootPreviousOffset = rootPrevious.dataLoading.rootNavigationPresentation |> Maybe.map .taskOffset
                    , branchRetryOffset = Dict.get "project:parent" branchRetried.dataLoading.loadedNavigationBranches |> Maybe.map .taskOffset
                    , branchProjectUntouched = Dict.get "project:parent" branchRetried.dataLoading.loadedNavigationBranches |> Maybe.map .projectOffset
                    , branchPreviousOffset = Dict.get "project:parent" branchPrevious.dataLoading.navigationPresentations |> Maybe.map .taskOffset
                    , rootRetained = Dict.member "root-task-0" rootRetried.dataLoading.taskCardSummaries
                    , branchRetained = Dict.member "branch-task-0" branchRetried.dataLoading.taskCardSummaries
                    }
        , test "project transport pages retry their real offset-50 request and retain reversible root and branch cursors" <|
            \_ ->
                let
                    prepared =
                        DataLoading.prepareRootNavigationRequest (Just workspaceId) rootLoadModel

                    ( rootFirst, _ ) =
                        DataLoading.update
                            (GotRootNavigation workspaceId prepared.sessionRequestEpoch (Just 1) prepared.dataLoading.navigationGeneration filterFingerprint 0 0
                                (Ok { workspaceId = workspaceId, projects = { items = [ project "root-project-0" Nothing ], hasMore = True }, tasks = { items = [], hasMore = False } })
                            )
                            prepared

                    rootCached =
                        DataLoading.update (LoadRootNavigationPage "project") rootFirst |> Tuple.first

                    rootRequested =
                        DataLoading.update (LoadRootNavigationPage "project") rootCached |> Tuple.first

                    rootFailed =
                        DataLoading.update
                            (GotRootNavigation workspaceId rootRequested.sessionRequestEpoch Nothing rootRequested.dataLoading.navigationGeneration filterFingerprint 50 0 (Err (Http.BadUrl "root project retry")))
                            rootRequested
                            |> Tuple.first

                    rootRetried =
                        DataLoading.update (LoadRootNavigationPage "project") rootFailed |> Tuple.first

                    rootPrevious =
                        DataLoading.update (ShowPreviousRootNavigationPage "project") rootRetried |> Tuple.first

                    ( branchInitial, _ ) =
                        DataLoading.beginNavigationBranch "project" workspaceId (Just "parent") model

                    fingerprint =
                        Dict.get "project:parent" branchInitial.dataLoading.loadedNavigationBranches
                            |> Maybe.map .filterFingerprint
                            |> Maybe.withDefault ""

                    ( branchFirst, _ ) =
                        DataLoading.update
                            (GotNavigationBranch workspaceId branchInitial.sessionRequestEpoch branchInitial.dataLoading.navigationGeneration "project:parent" fingerprint 0 0
                                (Ok { workspaceId = workspaceId, projects = { items = [ project "branch-project-0" (Just "parent") ], hasMore = True }, tasks = { items = [], hasMore = False } })
                            )
                            branchInitial

                    branchCached =
                        DataLoading.beginNavigationBranchPage "project" "parent" "project" branchFirst |> Tuple.first

                    branchRequested =
                        DataLoading.beginNavigationBranchPage "project" "parent" "project" branchCached |> Tuple.first

                    branchFailed =
                        DataLoading.update
                            (GotNavigationBranch workspaceId branchRequested.sessionRequestEpoch branchRequested.dataLoading.navigationGeneration "project:parent" fingerprint 50 0 (Err (Http.BadUrl "branch project retry")))
                            branchRequested
                            |> Tuple.first

                    branchRetried =
                        DataLoading.beginNavigationBranchPage "project" "parent" "project" branchFailed |> Tuple.first

                    branchPrevious =
                        DataLoading.beginNavigationBranchPreviousPage "project" "parent" "project" branchRetried |> Tuple.first

                    projectOffset current =
                        current.dataLoading.rootNavigationRequest |> Maybe.map .projectOffset

                    branchOffset current =
                        Dict.get "project:parent" current.dataLoading.loadedNavigationBranches |> Maybe.map .projectOffset

                    branchPresentationOffset current =
                        Dict.get "project:parent" current.dataLoading.navigationPresentations |> Maybe.map .projectOffset
                in
                Expect.equal
                    { rootRequested = Just 50, rootRetried = Just 50, rootPrevious = Just 0, branchRequested = Just 50, branchRetried = Just 50, branchPrevious = Just 0, rootCached = True, branchCached = True }
                    { rootRequested = projectOffset rootRequested
                    , rootRetried = projectOffset rootRetried
                    , rootPrevious = rootPrevious.dataLoading.rootNavigationPresentation |> Maybe.map .projectOffset
                    , branchRequested = branchOffset branchRequested
                    , branchRetried = branchOffset branchRetried
                    , branchPrevious = branchPresentationOffset branchPrevious
                    , rootCached = Dict.member "root-project-0" rootRetried.dataLoading.projectCardSummaries
                    , branchCached = Dict.member "branch-project-0" branchRetried.dataLoading.projectCardSummaries
                    }
        , test "root and branch project/task controls stop immediately at exact cached terminal pages" <|
            \_ ->
                let
                    rootPrepared =
                        DataLoading.prepareRootNavigationRequest (Just workspaceId) rootLoadModel

                    rootFingerprint =
                        rootPrepared.dataLoading.rootNavigationRequest |> Maybe.map .filterFingerprint |> Maybe.withDefault ""

                    exactProjects =
                        List.range 1 25 |> List.map (\number -> project ("root-project-" ++ String.fromInt number) Nothing)

                    exactTasks =
                        List.range 1 25 |> List.map (\number -> task ("root-task-" ++ String.fromInt number) Nothing)

                    rootLoaded =
                        DataLoading.update
                            (GotRootNavigation workspaceId rootPrepared.sessionRequestEpoch (Just 1) rootPrepared.dataLoading.navigationGeneration rootFingerprint 0 0
                                (Ok { workspaceId = workspaceId, projects = { items = exactProjects, hasMore = False }, tasks = { items = exactTasks, hasMore = False } })
                            )
                            rootPrepared
                            |> Tuple.first

                    rootProjectTerminal =
                        DataLoading.update (LoadRootNavigationPage "project") rootLoaded |> Tuple.first

                    rootTaskTerminal =
                        DataLoading.update (LoadRootNavigationPage "task") rootProjectTerminal |> Tuple.first

                    ( branchInitial, _ ) =
                        DataLoading.beginNavigationBranch "project" workspaceId (Just "parent") model

                    branchFingerprint =
                        Dict.get "project:parent" branchInitial.dataLoading.loadedNavigationBranches |> Maybe.map .filterFingerprint |> Maybe.withDefault ""

                    branchLoaded =
                        DataLoading.update
                            (GotNavigationBranch workspaceId branchInitial.sessionRequestEpoch branchInitial.dataLoading.navigationGeneration "project:parent" branchFingerprint 0 0
                                (Ok
                                    { workspaceId = workspaceId
                                    , projects = { items = List.range 1 25 |> List.map (\number -> project ("branch-project-" ++ String.fromInt number) (Just "parent")), hasMore = False }
                                    , tasks = { items = List.range 1 25 |> List.map (\number -> task ("branch-task-" ++ String.fromInt number) Nothing), hasMore = False }
                                    }
                                )
                            )
                            branchInitial
                            |> Tuple.first

                    branchProjectTerminal =
                        DataLoading.beginNavigationBranchPage "project" "parent" "project" branchLoaded |> Tuple.first

                    branchTaskTerminal =
                        DataLoading.beginNavigationBranchPage "project" "parent" "task" branchProjectTerminal |> Tuple.first

                    rootOffsets current =
                        current.dataLoading.rootNavigationPresentation
                            |> Maybe.map (\value -> ( value.projectOffset, value.taskOffset ))

                    branchOffsets current =
                        Dict.get "project:parent" current.dataLoading.navigationPresentations
                            |> Maybe.map (\value -> ( value.projectOffset, value.taskOffset ))
                in
                Expect.equal
                    { root = Just ( 0, 0 ), branch = Just ( 0, 0 ), rootProjects = 25, rootTasks = 25, branchProjects = 25, branchTasks = 25 }
                    { root = rootOffsets rootTaskTerminal
                    , branch = branchOffsets branchTaskTerminal
                    , rootProjects = rootTaskTerminal.dataLoading.rootNavigationRequest |> Maybe.map .projectCardCount |> Maybe.withDefault 0
                    , rootTasks = rootTaskTerminal.dataLoading.rootNavigationRequest |> Maybe.map .taskCardCount |> Maybe.withDefault 0
                    , branchProjects = Dict.get "project:parent" branchTaskTerminal.dataLoading.loadedNavigationBranches |> Maybe.map .projectCardCount |> Maybe.withDefault 0
                    , branchTasks = Dict.get "project:parent" branchTaskTerminal.dataLoading.loadedNavigationBranches |> Maybe.map .taskCardCount |> Maybe.withDefault 0
                    }
        , test "root and branch retain the final partial cached window before requesting continuation" <|
            \_ ->
                let
                    rootPrepared =
                        DataLoading.prepareRootNavigationRequest (Just workspaceId) rootLoadModel

                    rootFingerprint =
                        rootPrepared.dataLoading.rootNavigationRequest |> Maybe.map .filterFingerprint |> Maybe.withDefault ""

                    rootLoaded =
                        DataLoading.update
                            (GotRootNavigation workspaceId rootPrepared.sessionRequestEpoch (Just 1) rootPrepared.dataLoading.navigationGeneration rootFingerprint 0 0
                                (Ok { workspaceId = workspaceId, projects = { items = List.range 1 51 |> List.map (\number -> project ("root-" ++ String.fromInt number) Nothing), hasMore = True }, tasks = { items = [], hasMore = False } })
                            )
                            rootPrepared
                            |> Tuple.first

                    rootAt25 =
                        DataLoading.update (LoadRootNavigationPage "project") rootLoaded |> Tuple.first

                    rootAt50 =
                        DataLoading.update (LoadRootNavigationPage "project") rootAt25 |> Tuple.first

                    rootContinuation =
                        DataLoading.update (LoadRootNavigationPage "project") rootAt50 |> Tuple.first

                    ( branchInitial, _ ) =
                        DataLoading.beginNavigationBranch "project" workspaceId (Just "parent") model

                    branchFingerprint =
                        Dict.get "project:parent" branchInitial.dataLoading.loadedNavigationBranches |> Maybe.map .filterFingerprint |> Maybe.withDefault ""

                    branchLoaded =
                        DataLoading.update
                            (GotNavigationBranch workspaceId branchInitial.sessionRequestEpoch branchInitial.dataLoading.navigationGeneration "project:parent" branchFingerprint 0 0
                                (Ok { workspaceId = workspaceId, projects = { items = List.range 1 51 |> List.map (\number -> project ("branch-" ++ String.fromInt number) (Just "parent")), hasMore = True }, tasks = { items = [], hasMore = False } })
                            )
                            branchInitial
                            |> Tuple.first

                    branchAt25 =
                        DataLoading.beginNavigationBranchPage "project" "parent" "project" branchLoaded |> Tuple.first

                    branchAt50 =
                        DataLoading.beginNavigationBranchPage "project" "parent" "project" branchAt25 |> Tuple.first

                    branchContinuation =
                        DataLoading.beginNavigationBranchPage "project" "parent" "project" branchAt50 |> Tuple.first

                    rootState current =
                        current.dataLoading.rootNavigationRequest
                            |> Maybe.map (\request -> ( request.projectOffset, request.inFlight ))

                    rootPresentation current =
                        current.dataLoading.rootNavigationPresentation |> Maybe.map .projectOffset

                    branchState current =
                        Dict.get "project:parent" current.dataLoading.loadedNavigationBranches
                            |> Maybe.map (\request -> ( request.projectOffset, request.inFlight ))

                    branchPresentation current =
                        Dict.get "project:parent" current.dataLoading.navigationPresentations |> Maybe.map .projectOffset
                in
                Expect.equal
                    { rootAt25 = ( Just 25, Just ( 0, False ) )
                    , rootAt50 = ( Just 50, Just ( 0, False ) )
                    , rootContinuation = ( Just 51, Just ( 50, True ) )
                    , branchAt25 = ( Just 25, Just ( 0, False ) )
                    , branchAt50 = ( Just 50, Just ( 0, False ) )
                    , branchContinuation = ( Just 51, Just ( 50, True ) )
                    }
                    { rootAt25 = ( rootPresentation rootAt25, rootState rootAt25 )
                    , rootAt50 = ( rootPresentation rootAt50, rootState rootAt50 )
                    , rootContinuation = ( rootPresentation rootContinuation, rootState rootContinuation )
                    , branchAt25 = ( branchPresentation branchAt25, branchState branchAt25 )
                    , branchAt50 = ( branchPresentation branchAt50, branchState branchAt50 )
                    , branchContinuation = ( branchPresentation branchContinuation, branchState branchContinuation )
                    }
        , test "accepted root and branch transport pages dedupe sibling payloads across project-task alternation" <|
            \_ ->
                let
                    rootPrepared =
                        DataLoading.prepareRootNavigationRequest (Just workspaceId) rootLoadModel

                    rootFingerprint =
                        rootPrepared.dataLoading.rootNavigationRequest |> Maybe.map .filterFingerprint |> Maybe.withDefault ""

                    rootInitial =
                        DataLoading.update
                            (GotRootNavigation workspaceId rootPrepared.sessionRequestEpoch (Just 1) rootPrepared.dataLoading.navigationGeneration rootFingerprint 0 0
                                (Ok { workspaceId = workspaceId, projects = { items = [ project "root-project-0" Nothing ], hasMore = True }, tasks = { items = [ task "root-task-0" Nothing ], hasMore = True } })
                            )
                            rootPrepared
                            |> Tuple.first

                    rootProjectRequest =
                        DataLoading.update (LoadRootNavigationPage "project") rootInitial |> Tuple.first

                    rootProjectOffset =
                        rootProjectRequest.dataLoading.rootNavigationRequest |> Maybe.map .projectOffset |> Maybe.withDefault -1

                    rootProjectAccepted =
                        DataLoading.update
                            (GotRootNavigation workspaceId rootProjectRequest.sessionRequestEpoch Nothing rootProjectRequest.dataLoading.navigationGeneration rootFingerprint rootProjectOffset 0
                                (Ok { workspaceId = workspaceId, projects = { items = [ project "root-project-50" Nothing ], hasMore = True }, tasks = { items = [ task "root-task-0" Nothing ], hasMore = True } })
                            )
                            rootProjectRequest
                            |> Tuple.first

                    rootDuplicateIgnored =
                        DataLoading.update
                            (GotRootNavigation workspaceId rootProjectAccepted.sessionRequestEpoch Nothing rootProjectAccepted.dataLoading.navigationGeneration rootFingerprint rootProjectOffset 0
                                (Ok { workspaceId = workspaceId, projects = { items = [ project "root-project-50" Nothing ], hasMore = True }, tasks = { items = [ task "root-task-0" Nothing ], hasMore = True } })
                            )
                            rootProjectAccepted
                            |> Tuple.first

                    rootTaskRequest =
                        DataLoading.update (LoadRootNavigationPage "task") rootDuplicateIgnored |> Tuple.first

                    rootTaskOffset =
                        rootTaskRequest.dataLoading.rootNavigationRequest |> Maybe.map .taskOffset |> Maybe.withDefault -1

                    rootTaskAccepted =
                        DataLoading.update
                            (GotRootNavigation workspaceId rootTaskRequest.sessionRequestEpoch Nothing rootTaskRequest.dataLoading.navigationGeneration rootFingerprint rootProjectOffset rootTaskOffset
                                (Ok { workspaceId = workspaceId, projects = { items = [ project "root-project-50" Nothing ], hasMore = True }, tasks = { items = [ task "root-task-50" Nothing ], hasMore = True } })
                            )
                            rootTaskRequest
                            |> Tuple.first

                    rootProjectAgainRequest =
                        DataLoading.update (LoadRootNavigationPage "project") rootTaskAccepted |> Tuple.first

                    rootProjectAgainOffset =
                        rootProjectAgainRequest.dataLoading.rootNavigationRequest |> Maybe.map .projectOffset |> Maybe.withDefault -1

                    rootProjectAgainAccepted =
                        DataLoading.update
                            (GotRootNavigation workspaceId rootProjectAgainRequest.sessionRequestEpoch Nothing rootProjectAgainRequest.dataLoading.navigationGeneration rootFingerprint rootProjectAgainOffset rootTaskOffset
                                (Ok { workspaceId = workspaceId, projects = { items = [ project "root-project-100" Nothing ], hasMore = True }, tasks = { items = [ task "root-task-50" Nothing ], hasMore = True } })
                            )
                            rootProjectAgainRequest
                            |> Tuple.first

                    rootTaskAgainRequest =
                        DataLoading.update (LoadRootNavigationPage "task") rootProjectAgainAccepted |> Tuple.first

                    rootTaskAgainOffset =
                        rootTaskAgainRequest.dataLoading.rootNavigationRequest |> Maybe.map .taskOffset |> Maybe.withDefault -1

                    rootTaskAgainAccepted =
                        DataLoading.update
                            (GotRootNavigation workspaceId rootTaskAgainRequest.sessionRequestEpoch Nothing rootTaskAgainRequest.dataLoading.navigationGeneration rootFingerprint rootProjectAgainOffset rootTaskAgainOffset
                                (Ok { workspaceId = workspaceId, projects = { items = [ project "root-project-100" Nothing ], hasMore = True }, tasks = { items = [ task "root-task-100" Nothing ], hasMore = True } })
                            )
                            rootTaskAgainRequest
                            |> Tuple.first

                    ( branchInitial, _ ) =
                        DataLoading.beginNavigationBranch "project" workspaceId (Just "parent") model

                    branchFingerprint =
                        Dict.get "project:parent" branchInitial.dataLoading.loadedNavigationBranches |> Maybe.map .filterFingerprint |> Maybe.withDefault ""

                    branchLoaded =
                        DataLoading.update
                            (GotNavigationBranch workspaceId branchInitial.sessionRequestEpoch branchInitial.dataLoading.navigationGeneration "project:parent" branchFingerprint 0 0
                                (Ok { workspaceId = workspaceId, projects = { items = [ project "branch-project-0" (Just "parent") ], hasMore = True }, tasks = { items = [ task "branch-task-0" Nothing ], hasMore = True } })
                            )
                            branchInitial
                            |> Tuple.first

                    branchProjectRequest =
                        DataLoading.beginNavigationBranchPage "project" "parent" "project" branchLoaded |> Tuple.first

                    branchProjectOffset =
                        Dict.get "project:parent" branchProjectRequest.dataLoading.loadedNavigationBranches |> Maybe.map .projectOffset |> Maybe.withDefault -1

                    branchProjectAccepted =
                        DataLoading.update
                            (GotNavigationBranch workspaceId branchProjectRequest.sessionRequestEpoch branchProjectRequest.dataLoading.navigationGeneration "project:parent" branchFingerprint branchProjectOffset 0
                                (Ok { workspaceId = workspaceId, projects = { items = [ project "branch-project-50" (Just "parent") ], hasMore = True }, tasks = { items = [ task "branch-task-0" Nothing ], hasMore = True } })
                            )
                            branchProjectRequest
                            |> Tuple.first

                    branchDuplicateIgnored =
                        DataLoading.update
                            (GotNavigationBranch workspaceId branchProjectAccepted.sessionRequestEpoch branchProjectAccepted.dataLoading.navigationGeneration "project:parent" branchFingerprint branchProjectOffset 0
                                (Ok { workspaceId = workspaceId, projects = { items = [ project "branch-project-50" (Just "parent") ], hasMore = True }, tasks = { items = [ task "branch-task-0" Nothing ], hasMore = True } })
                            )
                            branchProjectAccepted
                            |> Tuple.first

                    branchTaskRequest =
                        DataLoading.beginNavigationBranchPage "project" "parent" "task" branchDuplicateIgnored |> Tuple.first

                    branchTaskOffset =
                        Dict.get "project:parent" branchTaskRequest.dataLoading.loadedNavigationBranches |> Maybe.map .taskOffset |> Maybe.withDefault -1

                    branchTaskAccepted =
                        DataLoading.update
                            (GotNavigationBranch workspaceId branchTaskRequest.sessionRequestEpoch branchTaskRequest.dataLoading.navigationGeneration "project:parent" branchFingerprint branchProjectOffset branchTaskOffset
                                (Ok { workspaceId = workspaceId, projects = { items = [ project "branch-project-50" (Just "parent") ], hasMore = True }, tasks = { items = [ task "branch-task-50" Nothing ], hasMore = True } })
                            )
                            branchTaskRequest
                            |> Tuple.first

                    branchProjectAgainRequest =
                        DataLoading.beginNavigationBranchPage "project" "parent" "project" branchTaskAccepted |> Tuple.first

                    branchProjectAgainOffset =
                        Dict.get "project:parent" branchProjectAgainRequest.dataLoading.loadedNavigationBranches |> Maybe.map .projectOffset |> Maybe.withDefault -1

                    branchProjectAgainAccepted =
                        DataLoading.update
                            (GotNavigationBranch workspaceId branchProjectAgainRequest.sessionRequestEpoch branchProjectAgainRequest.dataLoading.navigationGeneration "project:parent" branchFingerprint branchProjectAgainOffset branchTaskOffset
                                (Ok { workspaceId = workspaceId, projects = { items = [ project "branch-project-100" (Just "parent") ], hasMore = True }, tasks = { items = [ task "branch-task-50" Nothing ], hasMore = True } })
                            )
                            branchProjectAgainRequest
                            |> Tuple.first

                    branchTaskAgainRequest =
                        DataLoading.beginNavigationBranchPage "project" "parent" "task" branchProjectAgainAccepted |> Tuple.first

                    branchTaskAgainOffset =
                        Dict.get "project:parent" branchTaskAgainRequest.dataLoading.loadedNavigationBranches |> Maybe.map .taskOffset |> Maybe.withDefault -1

                    branchTaskAgainAccepted =
                        DataLoading.update
                            (GotNavigationBranch workspaceId branchTaskAgainRequest.sessionRequestEpoch branchTaskAgainRequest.dataLoading.navigationGeneration "project:parent" branchFingerprint branchProjectAgainOffset branchTaskAgainOffset
                                (Ok { workspaceId = workspaceId, projects = { items = [ project "branch-project-100" (Just "parent") ], hasMore = True }, tasks = { items = [ task "branch-task-100" Nothing ], hasMore = True } })
                            )
                            branchTaskAgainRequest
                            |> Tuple.first

                    counts current =
                        current.dataLoading.rootNavigationRequest
                            |> Maybe.map
                                (\state ->
                                    { projects = state.projectCardCount
                                    , tasks = state.taskCardCount
                                    , projectOffset = state.projectOffset
                                    , taskOffset = state.taskOffset
                                    }
                                )

                    branchCounts current =
                        Dict.get "project:parent" current.dataLoading.loadedNavigationBranches
                            |> Maybe.map
                                (\state ->
                                    { projects = state.projectCardCount
                                    , tasks = state.taskCardCount
                                    , projectOffset = state.projectOffset
                                    , taskOffset = state.taskOffset
                                    }
                                )
                in
                Expect.equal
                    { rootAfterProject = Just { projects = 2, tasks = 1, projectOffset = 50, taskOffset = 0 }
                    , rootAfterDuplicate = Just { projects = 2, tasks = 1, projectOffset = 50, taskOffset = 0 }
                    , rootAfterTask = Just { projects = 2, tasks = 2, projectOffset = 50, taskOffset = 50 }
                    , rootAfterProjectAgain = Just { projects = 3, tasks = 2, projectOffset = 100, taskOffset = 50 }
                    , rootAfterTaskAgain = Just { projects = 3, tasks = 3, projectOffset = 100, taskOffset = 100 }
                    , branchAfterProject = Just { projects = 2, tasks = 1, projectOffset = 50, taskOffset = 0 }
                    , branchAfterDuplicate = Just { projects = 2, tasks = 1, projectOffset = 50, taskOffset = 0 }
                    , branchAfterTask = Just { projects = 2, tasks = 2, projectOffset = 50, taskOffset = 50 }
                    , branchAfterProjectAgain = Just { projects = 3, tasks = 2, projectOffset = 100, taskOffset = 50 }
                    , branchAfterTaskAgain = Just { projects = 3, tasks = 3, projectOffset = 100, taskOffset = 100 }
                    }
                    { rootAfterProject = counts rootProjectAccepted
                    , rootAfterDuplicate = counts rootDuplicateIgnored
                    , rootAfterTask = counts rootTaskAccepted
                    , rootAfterProjectAgain = counts rootProjectAgainAccepted
                    , rootAfterTaskAgain = counts rootTaskAgainAccepted
                    , branchAfterProject = branchCounts branchProjectAccepted
                    , branchAfterDuplicate = branchCounts branchDuplicateIgnored
                    , branchAfterTask = branchCounts branchTaskAccepted
                    , branchAfterProjectAgain = branchCounts branchProjectAgainAccepted
                    , branchAfterTaskAgain = branchCounts branchTaskAgainAccepted
                    }
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

                    ( cachedWindow, _ ) =
                        DataLoading.beginNavigationBranchPage "project" "parent" "project" firstPage

                    ( pageRequest, _ ) =
                        DataLoading.beginNavigationBranchPage "project" "parent" "project" cachedWindow

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
        , test "successful branch presentation pages merge and deduplicate earlier project and task pages" <|
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

                    ( projectCachedWindow, _ ) =
                        DataLoading.beginNavigationBranchPage "project" "parent" "project" firstPage

                    ( projectPageRequest, _ ) =
                        DataLoading.beginNavigationBranchPage "project" "parent" "project" projectCachedWindow

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

                    ( taskCachedWindow, _ ) =
                        DataLoading.beginNavigationBranchPage "project" "parent" "task" projectPage

                    ( taskPageRequest, _ ) =
                        DataLoading.beginNavigationBranchPage "project" "parent" "task" taskCachedWindow

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
