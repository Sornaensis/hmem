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
import Route
import Set
import String
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Types exposing (AuthStatus(..), Flags, Model, Msg(..), Page(..), WorkspaceTab(..))
import Url


suite : Test
suite =
    describe "bounded navigation response guards"
        [ test "an expanded branch automatically continues both kinds past fifty siblings" <|
            \_ ->
                let
                    parent =
                        project "parent" Nothing

                    seeded =
                        loadRoot workspaceId [ { parent | hasChildren = True } ] [] model

                    requested =
                        seeded

                    reply projectOffset taskOffset projects tasks projectMore taskMore source =
                        case Dict.get "project:parent" source.dataLoading.loadedNavigationBranches of
                            Just request ->
                                DataLoading.update
                                    (GotNavigationBranch workspaceId request.sessionEpoch request.generation "project:parent" request.filterFingerprint projectOffset taskOffset
                                        (Ok { workspaceId = workspaceId, projects = { items = projects, hasMore = projectMore }, tasks = { items = tasks, hasMore = taskMore } })
                                    )
                                    source |> Tuple.first

                            Nothing ->
                                source

                    first =
                        reply 0 0
                            (List.range 1 50 |> List.map (\n -> project ("auto-project-" ++ String.fromInt n) (Just "parent")))
                            (List.range 1 50 |> List.map (\n -> task ("auto-task-" ++ String.fromInt n) Nothing))
                            True True requested

                    second =
                        reply 50 50 [ project "auto-project-51" (Just "parent") ] [ task "auto-task-51" Nothing ] False False first
                in
                Expect.equal
                    { continued = Just ( 50, 50, True ), projectCount = 52, taskCount = 51, exhausted = Just ( False, False, False ) }
                    { continued = Dict.get "project:parent" first.dataLoading.loadedNavigationBranches |> Maybe.map (\state -> ( state.projectOffset, state.taskOffset, state.inFlight ))
                    , projectCount = Dict.size second.dataLoading.projectCardSummaries
                    , taskCount = Dict.size second.dataLoading.taskCardSummaries
                    , exhausted = Dict.get "project:parent" second.dataLoading.loadedNavigationBranches |> Maybe.map (\state -> ( state.projectHasMore, state.taskHasMore, state.inFlight ))
                    }
        , test "a shell resync revalidates root, loaded branches, and an already-loaded focus" <|
            \_ ->
                let
                    seeded =
                        loadRoot workspaceId [ project "parent" Nothing ] [] model

                    ( branched, _ ) =
                        DataLoading.beginNavigationBranch "project" workspaceId (Just "parent") seeded

                    focus =
                        branched.focus

                    source =
                        { branched | auth = { status = AuthReady, mode = Just "test" }, sessionContext = Just editorSession, focus = { focus | focusedEntity = Just ( "project", "parent" ) } }

                    recovered =
                        WebSocket.update (WsMessageReceived workspaceShellSnapshotWire) source |> Tuple.first
                    resumed =
                        case Dict.get "project:parent" source.dataLoading.loadedNavigationBranches of
                            Just request -> DataLoading.update (GotNavigationBranch workspaceId request.sessionEpoch request.generation "project:parent" request.filterFingerprint request.projectOffset request.taskOffset (Err Http.Timeout)) recovered |> Tuple.first
                            Nothing -> recovered
                in
                Expect.equal
                    { advanced = True, rootPending = True, branchPending = True, focusPending = True, retained = True }
                    { advanced = recovered.dataLoading.navigationGeneration > source.dataLoading.navigationGeneration
                    , rootPending = recovered.dataLoading.rootNavigationRequest |> Maybe.map .inFlight |> Maybe.withDefault False
                    , branchPending = List.member "project:parent" recovered.dataLoading.navigationQueue && (Dict.get "project:parent" resumed.dataLoading.loadedNavigationBranches |> Maybe.map .inFlight |> Maybe.withDefault False)
                    , focusPending = recovered.dataLoading.activeNavigationFocus |> Maybe.map .inFlight |> Maybe.withDefault False
                    , retained = Dict.member "parent" recovered.dataLoading.projectCardSummaries
                    }
        , test "a branch response retains its request state and makes returned children visible" <|
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
        , test "workspace routes rehydrate unchanged task and project descriptions after returning" <|
            \_ ->
                let
                    projectA =
                        project "route-project-a" Nothing

                    taskA =
                        let
                            summary =
                                task "route-task-a" Nothing
                        in
                        { summary | projectId = Nothing }

                    loadedA =
                        loadRoot workspaceId [ projectA ] [ taskA ] rootLoadModel

                    oldProjectRequest =
                        Dict.get projectA.id loadedA.dataLoading.projectCardDetailRequests

                    oldTaskRequest =
                        Dict.get taskA.id loadedA.dataLoading.taskCardDetailRequests

                    projectADetail =
                        let
                            detail =
                                Api.projectFromCardSummary projectA
                        in
                        { detail | description = Just "old project A" }

                    partiallyHydratedA =
                        case oldProjectRequest of
                            Just request ->
                                DataLoading.update (GotProjectCardDetail request projectA.id (Ok projectADetail)) loadedA |> Tuple.first

                            Nothing ->
                                loadedA

                    ( withBranch, _ ) =
                        DataLoading.beginNavigationBranch "project" workspaceId (Just projectA.id) partiallyHydratedA

                    loadingA =
                        withBranch.dataLoading

                    offsetA =
                        { withBranch
                            | dataLoading =
                                { loadingA
                                    | rootNavigationPresentation =
                                        loadingA.rootNavigationPresentation
                                            |> Maybe.map (\presentation -> { presentation | projectOffset = 25, taskOffset = 25 })
                                    , navigationPresentations =
                                        Dict.map (\_ presentation -> { presentation | projectOffset = 25, taskOffset = 25 }) loadingA.navigationPresentations
                                }
                        }

                    workspaceB =
                        "workspace-2"

                    switchedToB =
                        Route.handleUrlChange { url | path = "/workspace/" ++ workspaceB } offsetA |> Tuple.first

                    afterLateInBGap =
                        case oldTaskRequest of
                            Just request ->
                                DataLoading.update (GotTaskCardDetail request taskA.id (Err Http.Timeout)) switchedToB |> Tuple.first

                            Nothing ->
                                switchedToB

                    projectB =
                        let
                            summary =
                                project "route-project-b" Nothing
                        in
                        { summary | workspaceId = workspaceB }

                    taskB =
                        let
                            summary =
                                task "route-task-b" Nothing
                        in
                        { summary | workspaceId = workspaceB, projectId = Nothing }

                    loadedB =
                        loadRoot workspaceB [ projectB ] [ taskB ] afterLateInBGap

                    hydratedB =
                        hydrateDescriptions "project B" "task B" projectB taskB loadedB

                    returnGap =
                        Route.handleUrlChange url hydratedB |> Tuple.first

                    returnedA =
                        loadRoot workspaceId [ projectA ] [ taskA ] returnGap

                    freshProjectRequest =
                        Dict.get projectA.id returnedA.dataLoading.projectCardDetailRequests

                    freshTaskRequest =
                        Dict.get taskA.id returnedA.dataLoading.taskCardDetailRequests

                    taskADetail =
                        let
                            detail =
                                Api.taskFromCardSummary taskA
                        in
                        { detail | description = Just "late task A" }

                    afterOldResponses =
                        case ( oldProjectRequest, oldTaskRequest ) of
                            ( Just projectRequest, Just taskRequest ) ->
                                returnedA
                                    |> DataLoading.update (GotProjectCardDetail projectRequest projectA.id (Err Http.Timeout))
                                    |> Tuple.first
                                    |> DataLoading.update (GotTaskCardDetail taskRequest taskA.id (Ok taskADetail))
                                    |> Tuple.first

                            _ ->
                                returnedA

                    hydratedAgainA =
                        hydrateDescriptions "fresh project A" "fresh task A" projectA taskA afterOldResponses

                    newerRequest oldRequest freshRequest =
                        case ( oldRequest, freshRequest ) of
                            ( Just old, Just fresh ) ->
                                fresh.requestId > old.requestId
                                    && fresh.workspaceId == workspaceId
                                    && fresh.sessionEpoch == returnedA.sessionRequestEpoch

                            _ ->
                                False
                in
                Expect.all
                    [ \_ ->
                        Expect.equal
                            { retiredInBGap = True
                            , descriptionsInB = ( Just "project B", Just "task B" )
                            , retiredInReturnGap = True
                            , presentationRestarted = True
                            , projectRequestRestarted = True
                            , taskRequestRestarted = True
                            , oldResponsesIgnored = True
                            , descriptionsOnReturn = ( Just "fresh project A", Just "fresh task A" )
                            }
                            { retiredInBGap =
                                Dict.isEmpty afterLateInBGap.dataLoading.projectCardDetailRequests
                                    && Dict.isEmpty afterLateInBGap.dataLoading.taskCardDetailRequests
                                    && afterLateInBGap.dataLoading.rootNavigationPresentation == Nothing
                                    && Dict.isEmpty afterLateInBGap.dataLoading.navigationPresentations
                            , descriptionsInB =
                                ( Dict.get projectB.id hydratedB.projects |> Maybe.andThen .description
                                , Dict.get taskB.id hydratedB.tasks |> Maybe.andThen .description
                                )
                            , retiredInReturnGap =
                                Dict.isEmpty returnGap.dataLoading.projectCardDetailRequests
                                    && Dict.isEmpty returnGap.dataLoading.taskCardDetailRequests
                                    && returnGap.dataLoading.rootNavigationPresentation == Nothing
                                    && Dict.isEmpty returnGap.dataLoading.navigationPresentations
                            , presentationRestarted =
                                returnedA.dataLoading.rootNavigationPresentation
                                    |> Maybe.map (\presentation -> presentation.projectOffset == 0 && presentation.taskOffset == 0)
                                    |> Maybe.withDefault False
                            , projectRequestRestarted = newerRequest oldProjectRequest freshProjectRequest
                            , taskRequestRestarted = newerRequest oldTaskRequest freshTaskRequest
                            , oldResponsesIgnored =
                                Dict.get projectA.id afterOldResponses.dataLoading.projectCardDetailRequests == freshProjectRequest
                                    && Dict.get taskA.id afterOldResponses.dataLoading.taskCardDetailRequests == freshTaskRequest
                                    && (Dict.get taskA.id afterOldResponses.tasks |> Maybe.andThen .description) == Nothing
                            , descriptionsOnReturn =
                                ( Dict.get projectA.id hydratedAgainA.projects |> Maybe.andThen .description
                                , Dict.get taskA.id hydratedAgainA.tasks |> Maybe.andThen .description
                                )
                            }
                    , \_ ->
                        Cards.viewProjectsTree workspaceId hydratedAgainA
                            |> Query.fromHtml
                            |> Query.has [ Selector.text "fresh project A", Selector.text "fresh task A" ]
                    ]
                    ()
        , test "same-workspace routing preserves loaded empty descriptions and home re-entry retires card state" <|
            \_ ->
                let
                    summary =
                        project "empty-description" Nothing

                    loaded =
                        loadRoot workspaceId [ summary ] [] rootLoadModel

                    hydratedEmpty =
                        case Dict.get summary.id loaded.dataLoading.projectCardDetailRequests of
                            Just request ->
                                let
                                    detail =
                                        Api.projectFromCardSummary summary
                                in
                                DataLoading.update (GotProjectCardDetail request summary.id (Ok { detail | description = Nothing })) loaded |> Tuple.first

                            Nothing ->
                                loaded

                    sameWorkspace =
                        Route.handleUrlChange { url | fragment = Just "tab=projects" } hydratedEmpty |> Tuple.first

                    ensured =
                        DataLoading.ensureNavigationPresentation "workspace_root" Nothing sameWorkspace |> Tuple.first

                    home =
                        Route.handleUrlChange { url | path = "/" } ensured |> Tuple.first

                    returned =
                        Route.handleUrlChange url home |> Tuple.first
                in
                Expect.equal
                    { sameRequest = Dict.get summary.id hydratedEmpty.dataLoading.projectCardDetailRequests
                    , samePresentation = hydratedEmpty.dataLoading.rootNavigationPresentation
                    , noEmptyRetry = hydratedEmpty.dataLoading.nextCardDetailRequestId
                    , returnedRequestsEmpty = True
                    , returnedPresentationsEmpty = True
                    , requestCounterPreserved = hydratedEmpty.dataLoading.nextCardDetailRequestId
                    }
                    { sameRequest = Dict.get summary.id sameWorkspace.dataLoading.projectCardDetailRequests
                    , samePresentation = sameWorkspace.dataLoading.rootNavigationPresentation
                    , noEmptyRetry = ensured.dataLoading.nextCardDetailRequestId
                    , returnedRequestsEmpty =
                        Dict.isEmpty returned.dataLoading.projectCardDetailRequests
                            && Dict.isEmpty returned.dataLoading.taskCardDetailRequests
                    , returnedPresentationsEmpty =
                        returned.dataLoading.rootNavigationPresentation == Nothing
                            && Dict.isEmpty returned.dataLoading.navigationPresentations
                    , requestCounterPreserved = returned.dataLoading.nextCardDetailRequestId
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
                            , webSocket = { baseWebSocket | targetGenerations = Dict.fromList [ ( guard.scopeKey ++ "|" ++ guard.targetKey, 1 ), ( guard.scopeKey ++ "|navigation-summary:project:" ++ projectSummary.id, 1 ), ( guard.scopeKey ++ "|navigation-summary:task:" ++ taskSummary.id, 1 ) ] }
                        }

                    updated =
                        WebSocket.update
                            (CanonicalNavigationSummariesFetched (summaryGuard guard source [ projectSummary.id ] [ taskSummary.id ]) workspaceId [ projectSummary.id ] [ taskSummary.id ]
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
        , test "pinned project and task paging admit bounded details from the rendered cached window" <|
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
                                            , cardDetailAdmissions = Set.empty
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
                    { projectWindow = True, taskWindow = True, pinnedProjectHydrated = True, pinnedTaskHydrated = True }
                    { projectWindow = Set.size projectRequestIds == 6 && Set.isEmpty (Set.diff projectRequestIds expectedProjects)
                    , taskWindow = Set.size taskRequestIds == 6 && Set.isEmpty (Set.diff taskRequestIds expectedTasks)
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
        , test "shell-triggered tokenless observation success and failure cannot retire the token-bearing root load" <|
            \_ ->
                let
                    initialLoading =
                        rootLoadModel.dataLoading

                    bootstrap =
                        DataLoading.prepareRootNavigationRequest (Just workspaceId)
                            { rootLoadModel
                                | auth = { status = AuthReady, mode = Just "test" }
                                , sessionContext = Just editorSession
                                , dataLoading = { initialLoading | pendingWorkspaceLoads = 0 }
                            }

                    afterShell =
                        WebSocket.update (WsMessageReceived workspaceShellSnapshotWire) bootstrap |> Tuple.first

                    settleObservation result =
                        DataLoading.update
                            (GotObservations workspaceId Nothing afterShell.observations.requestGeneration afterShell.observations.queryFingerprint 0 result)
                            afterShell
                            |> Tuple.first

                    finishRoot afterObservation =
                        case afterObservation.dataLoading.rootNavigationRequest of
                            Just request ->
                                DataLoading.update
                                    (GotRootNavigation workspaceId request.sessionEpoch (Just 1) request.generation request.filterFingerprint 0 0 (Ok rootResponse))
                                    afterObservation
                                    |> Tuple.first

                            Nothing ->
                                afterObservation

                    afterSuccess =
                        settleObservation (Ok { items = [], hasMore = False })

                    afterFailure =
                        settleObservation (Err Http.Timeout)

                    successRoot =
                        finishRoot afterSuccess

                    failureRoot =
                        finishRoot afterFailure

                    outcome beforeRoot afterRoot =
                        { initialPendingIsZero = bootstrap.dataLoading.pendingWorkspaceLoads == 0
                        , initialTokenIsActive = bootstrap.dataLoading.activeWorkspaceLoadToken == Just 1
                        , tokenPreserved = beforeRoot.dataLoading.activeWorkspaceLoadToken == Just 1
                        , accepted = Dict.member "stale-root" afterRoot.dataLoading.projectCardSummaries
                        , settled = afterRoot.dataLoading.rootNavigationRequest |> Maybe.map (\request -> request.succeeded && not request.inFlight) |> Maybe.withDefault False
                        , loadFinished = afterRoot.dataLoading.activeWorkspaceLoadToken == Nothing && not afterRoot.dataLoading.loadingWorkspaceData
                        }
                in
                Expect.equal
                    [ { initialPendingIsZero = True, initialTokenIsActive = True, tokenPreserved = True, accepted = True, settled = True, loadFinished = True }
                    , { initialPendingIsZero = True, initialTokenIsActive = True, tokenPreserved = True, accepted = True, settled = True, loadFinished = True }
                    ]
                    [ outcome afterSuccess successRoot, outcome afterFailure failureRoot ]
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
        , test "roots request transport immediately and repeated demand coalesces while branch cursors retain cache" <|
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
                    { rootAt25 = ( Just 25, Just ( 50, True ) )
                    , rootAt50 = ( Just 25, Just ( 50, True ) )
                    , rootContinuation = ( Just 25, Just ( 50, True ) )
                    , branchAt25 = ( Just 25, Just ( 50, True ) )
                    , branchAt50 = ( Just 50, Just ( 50, True ) )
                    , branchContinuation = ( Just 50, Just ( 50, True ) )
                    }
                    { rootAt25 = ( rootPresentation rootAt25, rootState rootAt25 )
                    , rootAt50 = ( rootPresentation rootAt50, rootState rootAt50 )
                    , rootContinuation = ( rootPresentation rootContinuation, rootState rootContinuation )
                    , branchAt25 = ( branchPresentation branchAt25, branchState branchAt25 )
                    , branchAt50 = ( branchPresentation branchAt50, branchState branchAt50 )
                    , branchContinuation = ( branchPresentation branchContinuation, branchState branchContinuation )
                    }
        , test "manual roots dedupe alternation while automatic branches end each stream independently" <|
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

                    branchPage number taskMore projectMore source =
                        case Dict.get "project:parent" source.dataLoading.loadedNavigationBranches of
                            Just request ->
                                DataLoading.update (GotNavigationBranch workspaceId request.sessionEpoch request.generation "project:parent" request.filterFingerprint request.projectOffset request.taskOffset
                                    (Ok { workspaceId = workspaceId, projects = { items = [ project ("branch-project-" ++ String.fromInt number) (Just "parent") ], hasMore = projectMore }, tasks = { items = [ task ("branch-task-" ++ String.fromInt number) Nothing ], hasMore = taskMore } })) source |> Tuple.first
                            Nothing -> source

                    branchProjectAccepted = branchPage 50 True True branchLoaded
                    branchDuplicateIgnored =
                        DataLoading.update (GotNavigationBranch workspaceId branchLoaded.sessionRequestEpoch branchLoaded.dataLoading.navigationGeneration "project:parent" branchFingerprint 50 50
                            (Ok { workspaceId = workspaceId, projects = { items = [ project "duplicate" (Just "parent") ], hasMore = True }, tasks = { items = [ task "duplicate" Nothing ], hasMore = True } })) branchProjectAccepted |> Tuple.first
                    branchTaskAccepted = branchPage 100 False True branchDuplicateIgnored
                    branchProjectAgainAccepted = branchPage 150 True True branchTaskAccepted
                    branchTaskAgainAccepted = branchPage 200 True False branchProjectAgainAccepted

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
                    , branchAfterProject = Just { projects = 2, tasks = 2, projectOffset = 100, taskOffset = 100 }
                    , branchAfterDuplicate = Just { projects = 2, tasks = 2, projectOffset = 100, taskOffset = 100 }
                    , branchAfterTask = Just { projects = 3, tasks = 3, projectOffset = 150, taskOffset = 100 }
                    , branchAfterProjectAgain = Just { projects = 4, tasks = 3, projectOffset = 200, taskOffset = 100 }
                    , branchAfterTaskAgain = Just { projects = 5, tasks = 3, projectOffset = 200, taskOffset = 100 }
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
                        { baseWebSocket | targetGenerations = Dict.fromList [ ( "workspace:workspace-1|navigation-summaries:project:child", 1 ), ( "workspace:workspace-1|navigation-summary:project:child", 1 ) ] }

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
                            (CanonicalNavigationSummariesFetched (summaryGuard guard source [ "child" ] []) workspaceId [ "child" ] []
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
                        { baseWebSocket | targetGenerations = Dict.fromList [ ( "workspace:workspace-1|navigation-summaries:project:child", 1 ), ( "workspace:workspace-1|navigation-summary:project:child", 1 ) ] }

                    source =
                        { model | auth = { status = AuthReady, mode = Just "test" }, sessionContext = Just editorSession, search = filteredSearch, webSocket = websocket, dataLoading = loading }

                    updated =
                        WebSocket.update (CanonicalNavigationSummariesFetched (summaryGuard guard source [ "child" ] []) workspaceId [ "child" ] [] (Ok { projects = [], tasks = [], missingProjectIds = [ "child" ], missingTaskIds = [] })) source |> Tuple.first
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
                        let summary = project "parent" Nothing in { summary | status = Api.ProjCompleted }

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
                                        (Ok { workspaceId = workspaceId, projects = { items = [ parent ], hasMore = False }, tasks = { items = [], hasMore = False } })
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
                        DataLoading.mergeNavigationSummaries [ project "old-parent" Nothing, project "new-parent" Nothing, child ] [] model

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
        , test "overlapping summary subsets preserve current members and apply unrelated older members" <|
            \_ ->
                let
                    base = syncModel [ project "a" Nothing, project "b" Nothing ]
                    first = requestSummaries "first" [ ( "project", "a" ), ( "project", "b" ) ] base
                    olderGuard = batchGuard [ ( "project", "a" ), ( "project", "b" ) ] first
                    second = requestSummaries "second" [ ( "project", "a" ) ] first
                    newerGuard = batchGuard [ ( "project", "a" ) ] second
                    freshA = project "a" Nothing
                    olderB = project "b" Nothing
                    newest = summaryReply second newerGuard [ "a" ] [] { projects = [ { freshA | name = "current-a" } ], tasks = [], missingProjectIds = [], missingTaskIds = [] } second
                    late = summaryReply first olderGuard [ "a", "b" ] [] { projects = [ { freshA | name = "stale-a" }, { olderB | name = "current-b" } ], tasks = [], missingProjectIds = [], missingTaskIds = [] } newest
                    lateDeletion = summaryReply first olderGuard [ "a", "b" ] [] { projects = [ { olderB | name = "current-b" } ], tasks = [], missingProjectIds = [ "a" ], missingTaskIds = [] } newest
                in
                Expect.equal ( Just "current-a", Just "current-b", Just "current-a" )
                    ( Dict.get "a" late.projects |> Maybe.map .name, Dict.get "b" late.projects |> Maybe.map .name, Dict.get "a" lateDeletion.projects |> Maybe.map .name )
        , test "resync counters cannot reuse an old summary guard and filter lifetime rejects late responses" <|
            \_ ->
                let
                    first = requestSummaries "before" [ ( "project", "a" ) ] (syncModel [ project "a" Nothing ])
                    oldGuard = batchGuard [ ( "project", "a" ) ] first
                    invalidated = WebSocket.update (WsMessageReceived (scopedFrame (Encode.object [ ( "schema_version", Encode.int 1 ), ( "type", Encode.string "resync_required" ) ]))) first |> Tuple.first
                    snapshot = WebSocket.update (WsMessageReceived workspaceShellSnapshotWire) invalidated |> Tuple.first
                    current = requestSummaries "after" [ ( "project", "a" ) ] snapshot
                    newGuard = batchGuard [ ( "project", "a" ) ] current
                    stale = summaryReply first oldGuard [ "a" ] [] { projects = [], tasks = [], missingProjectIds = [ "a" ], missingTaskIds = [] } current
                    refreshed = DataLoading.revalidateNavigationForFilters first |> Tuple.first
                    afterFilter = summaryReply first oldGuard [ "a" ] [] { projects = [], tasks = [], missingProjectIds = [ "a" ], missingTaskIds = [] } refreshed
                in
                Expect.equal ( True, True, True )
                    ( newGuard.targetGeneration > oldGuard.targetGeneration, Dict.member "a" stale.projects, Dict.member "a" afterFilter.projects )
        , test "foreign unloaded roots and expanded-branch tasks request bounded authoritative membership" <|
            \_ ->
                let
                    base = syncModel [ project "parent" Nothing ]
                    expanded = DataLoading.beginNavigationBranch "project" workspaceId (Just "parent") base |> Tuple.first
                    rooted = requestSummaries "foreign-project" [ ( "project", "new-root" ) ] expanded
                    tasked = requestSummaries "foreign-task" [ ( "task", "new-task" ) ] expanded
                    readyTask = case Dict.get "project:parent" expanded.dataLoading.loadedNavigationBranches of
                        Just request -> DataLoading.update (GotNavigationBranch workspaceId request.sessionEpoch request.generation "project:parent" request.filterFingerprint request.projectOffset request.taskOffset (Err Http.Timeout)) tasked |> Tuple.first
                        Nothing -> tasked
                    rootReturned = acceptRoot [ project "parent" Nothing, project "new-root" Nothing ] [] rooted
                    taskBase = task "new-task" Nothing
                    taskReturned = case Dict.get "project:parent" readyTask.dataLoading.loadedNavigationBranches of
                        Just request ->
                            DataLoading.update (GotNavigationBranch workspaceId request.sessionEpoch request.generation "project:parent" request.filterFingerprint 0 0 (Ok { workspaceId = workspaceId, projects = { items = [], hasMore = False }, tasks = { items = [ { taskBase | projectId = Just "parent" } ], hasMore = False } })) readyTask |> Tuple.first
                        Nothing -> readyTask
                in
                Expect.equal { pending = True, prematurelyInserted = False, root = True, task = True }
                    { pending = rooted.dataLoading.rootNavigationRequest |> Maybe.map .inFlight |> Maybe.withDefault False, prematurelyInserted = Dict.member "new-root" rooted.projects, root = Set.member "new-root" rootReturned.dataLoading.navigationVisibleProjectIds, task = Set.member "new-task" taskReturned.dataLoading.navigationVisibleTaskIds }
        , test "a formerly filtered-out card becomes matching only after authoritative navigation" <|
            \_ ->
                let
                    base = syncModel []
                    search = base.search
                    filtered = { base | search = { search | filterProjectStatuses = [ "completed" ] } }
                    pending = requestSummaries "became-matching" [ ( "project", "newly-matching" ) ] filtered
                    summary = project "newly-matching" Nothing
                    accepted = acceptRoot [ { summary | status = Api.ProjCompleted } ] [] pending
                in
                Expect.equal ( True, True )
                    ( pending.dataLoading.rootNavigationRequest |> Maybe.map .inFlight |> Maybe.withDefault False, Set.member "newly-matching" accepted.dataLoading.navigationVisibleProjectIds )

        , test "revalidation refills only the retained paginated root window" <|
            \_ ->
                let
                    seeded = syncModel (List.range 1 100 |> List.map (\index -> project (String.fromInt index) Nothing))
                    loading = seeded.dataLoading
                    positioned = { seeded | dataLoading = { loading | rootNavigationPresentation = Maybe.map (\presentation -> { presentation | projectOffset = 75 }) loading.rootNavigationPresentation } }
                    refreshing = DataLoading.revalidateNavigationForFilters positioned |> Tuple.first
                    page offset count source =
                        case source.dataLoading.rootNavigationRequest of
                            Just request ->
                                DataLoading.update (GotRootNavigation workspaceId request.sessionEpoch Nothing request.generation request.filterFingerprint offset 0 (Ok { workspaceId = workspaceId, projects = { items = List.range (offset + 1) (offset + count) |> List.map (\index -> project (String.fromInt index) Nothing), hasMore = True }, tasks = { items = [], hasMore = False } })) source |> Tuple.first
                            Nothing -> source
                    firstPage = page 0 50 refreshing
                    restored = page 50 50 firstPage
                in
                Expect.equal { retained = Just 75, fetched = Just 50, restored = Just 75, pending = Just False, count = Just 100 }
                    { retained = refreshing.dataLoading.rootNavigationPresentation |> Maybe.map .projectOffset, fetched = firstPage.dataLoading.rootNavigationRequest |> Maybe.map .projectOffset, restored = restored.dataLoading.rootNavigationPresentation |> Maybe.map .projectOffset, pending = restored.dataLoading.rootNavigationRequest |> Maybe.map .inFlight, count = restored.dataLoading.rootNavigationRequest |> Maybe.map .projectCardCount }
        , test "restoring a pinned ordinary-window boundary fetches the next bounded page" <|
            \_ ->
                let
                    summaries = List.range 1 100 |> List.map (\index -> project (String.padLeft 3 '0' (String.fromInt index)) Nothing)
                    seeded = syncModel summaries
                    loading = seeded.dataLoading
                    focus = seeded.focus
                    positioned = { seeded | focus = { focus | focusedEntity = Just ( "project", "001" ) }, dataLoading = { loading | rootNavigationPresentation = Maybe.map (\presentation -> { presentation | projectOffset = 49 }) loading.rootNavigationPresentation } }
                    refreshing = DataLoading.revalidateNavigationForFilters positioned |> Tuple.first
                    page offset source = case source.dataLoading.rootNavigationRequest of
                        Just request -> DataLoading.update (GotRootNavigation workspaceId request.sessionEpoch Nothing request.generation request.filterFingerprint offset 0 (Ok { workspaceId = workspaceId, projects = { items = List.drop offset summaries |> List.take 50, hasMore = offset == 0 }, tasks = { items = [], hasMore = False } })) source |> Tuple.first
                        Nothing -> source
                    first = page 0 refreshing
                    restored = page 50 first
                    resumed =
                        Dict.toList seeded.dataLoading.projectCardDetailRequests
                            |> List.foldl (\( id, request ) source -> DataLoading.update (GotProjectCardDetail request id (Err Http.Timeout)) source |> Tuple.first) restored
                in
                Expect.equal ( Just ( True, 50 ), Just 49, True )
                    ( first.dataLoading.rootNavigationRequest |> Maybe.map (\request -> ( request.inFlight, request.projectOffset ))
                    , restored.dataLoading.rootNavigationPresentation |> Maybe.map .projectOffset
                    , Dict.member "051" resumed.dataLoading.projectCardDetailRequests
                    )
        , test "loaded reparent with structural invalidation replaces bounded root and expanded membership" <|
            \_ ->
                let
                    parent = project "parent" Nothing
                    moving = project "moving" Nothing
                    others = List.range 1 48 |> List.map (\n -> project ("root-" ++ String.fromInt n) Nothing)
                    base = syncModel (parent :: moving :: others)
                    expanded = DataLoading.beginNavigationBranch "project" workspaceId (Just "parent") base |> Tuple.first
                    before = case Dict.get "project:parent" expanded.dataLoading.loadedNavigationBranches of
                        Just request -> DataLoading.update (GotNavigationBranch workspaceId request.sessionEpoch request.generation "project:parent" request.filterFingerprint 0 0 (Ok { workspaceId = workspaceId, projects = { items = [], hasMore = False }, tasks = { items = [], hasMore = False } })) expanded |> Tuple.first
                        Nothing -> expanded
                    changed = requestSummariesWithInvalidations "loaded-move" [ ( "project", "moving" ) ]
                        [ Encode.object [ ( "kind", Encode.string "tree" ), ( "target", Encode.string ("workspace:" ++ workspaceId) ) ]
                        , Encode.object [ ( "kind", Encode.string "readiness" ), ( "target", Encode.string "project:moving" ) ]
                        ] before
                    summarized = summaryReply changed (batchGuard [ ( "project", "moving" ) ] changed) [ "moving" ] [] { projects = [ { moving | parentId = Just "parent" } ], tasks = [], missingProjectIds = [], missingTaskIds = [] } changed
                    rooted = acceptRoot (parent :: project "replacement" Nothing :: others) [] summarized
                    branched = case Dict.get "project:parent" rooted.dataLoading.loadedNavigationBranches of
                        Just request -> DataLoading.update (GotNavigationBranch workspaceId request.sessionEpoch request.generation "project:parent" request.filterFingerprint 0 0 (Ok { workspaceId = workspaceId, projects = { items = [ { moving | parentId = Just "parent" } ], hasMore = False }, tasks = { items = [], hasMore = False } })) rooted |> Tuple.first
                        Nothing -> rooted
                in
                Expect.equal { rootPending = Just True, branchPending = Just True, root = Just ( 50, False, ( False, True ) ), branch = Just [ "moving" ] }
                    { rootPending = changed.dataLoading.rootNavigationRequest |> Maybe.map .inFlight
                    , branchPending = Dict.get "project:parent" changed.dataLoading.loadedNavigationBranches |> Maybe.map .inFlight
                    , root = branched.dataLoading.rootNavigationRequest |> Maybe.map (\request -> ( request.projectCardCount, request.projectHasMore, ( Set.member "moving" branched.dataLoading.navigationVisibleProjectIds && (Dict.get "moving" branched.dataLoading.projectCardSummaries |> Maybe.andThen .parentId) == Nothing, Set.member "replacement" branched.dataLoading.navigationVisibleProjectIds ) ))
                    , branch = Dict.get "project:parent" branched.dataLoading.loadedNavigationBranches |> Maybe.map (\_ -> Dict.values branched.dataLoading.projectCardSummaries |> List.filter (\summary -> summary.parentId == Just "parent" && Set.member summary.id branched.dataLoading.navigationVisibleProjectIds) |> List.map .id)
                    }
        , test "reordered equal batches are stale per entity and chunks remain bounded at 100" <|
            \_ ->
                let
                    base = syncModel [ project "a" Nothing, project "b" Nothing ]
                    first = requestSummaries "ordered" [ ( "project", "a" ), ( "project", "b" ) ] base
                    second = requestSummaries "reordered" [ ( "project", "b" ), ( "project", "a" ) ] first
                    stale = summaryReply first (batchGuard [ ( "project", "a" ), ( "project", "b" ) ] first) [ "a", "b" ] [] { projects = [], tasks = [], missingProjectIds = [ "a", "b" ], missingTaskIds = [] } second
                    summaries = List.range 1 101 |> List.map (\index -> project ("chunk-" ++ String.fromInt index) Nothing)
                    chunked = requestSummaries "chunked" (List.map (\summary -> ( "project", summary.id )) summaries) (syncModel summaries)
                    batchKeys = Dict.keys chunked.webSocket.targetGenerations |> List.filter (String.contains "|navigation-summaries:")
                in
                Expect.equal ( True, True, [ 1, 100 ] )
                    ( Dict.member "a" stale.projects, Dict.member "b" stale.projects, List.map (String.split "," >> List.length) batchKeys |> List.sort )

        , test "fair branch admissions stay at four and continuation yields to queued siblings" <|
            \_ ->
                let
                    parents = List.range 1 9 |> List.map (\n -> let summary = project ("p" ++ String.fromInt n) Nothing in { summary | hasChildren = True })
                    seeded = loadRoot workspaceId parents [] model
                    continued = branchReply "project:p1" (List.range 1 50 |> List.map (\n -> project ("child" ++ String.fromInt n) (Just "p1"))) [] True False seeded
                in
                Expect.equal { first = 4, waiting = 5, physical = 4, sibling = Just True, continuation = Just False, queued = True }
                    { first = Dict.size seeded.dataLoading.navigationAdmissions
                    , waiting = List.length seeded.dataLoading.navigationQueue
                    , physical = Dict.size continued.dataLoading.navigationAdmissions
                    , sibling = Dict.get "project:p5" continued.dataLoading.loadedNavigationBranches |> Maybe.map .inFlight
                    , continuation = Dict.get "project:p1" continued.dataLoading.loadedNavigationBranches |> Maybe.map .inFlight
                    , queued = List.member "project:p1" continued.dataLoading.navigationQueue
                    }
        , test "collapse and reopen resume pending streams only after stale physical completion" <|
            \_ ->
                let
                    parent = project "parent" Nothing
                    child = project "child" (Just "parent")
                    seeded = loadRoot workspaceId [ { parent | hasChildren = True } ] [] model
                    first = branchReply "project:parent" [ { child | hasChildren = True } ] [] True False seeded
                    old = Dict.get "project:parent" first.dataLoading.loadedNavigationBranches
                    collapsed = Cards.update (ToggleTreeNode "proj-parent") first |> Tuple.first
                    reopened = Cards.update (ToggleTreeNode "proj-parent") collapsed |> Tuple.first
                    afterOld = case old of
                        Just request -> DataLoading.update (GotNavigationBranch workspaceId request.sessionEpoch request.generation "project:parent" request.filterFingerprint request.projectOffset request.taskOffset (Ok { workspaceId = workspaceId, projects = { items = [ project "late-hidden" (Just "parent") ], hasMore = False }, tasks = { items = [], hasMore = False } })) reopened |> Tuple.first
                        Nothing -> reopened
                in
                Expect.equal { held = True, hidden = Just False, pending = Just True, queued = True, resumed = Just ( 50, True ), childVisible = True, staleAbsent = True }
                    { held = Dict.size collapsed.dataLoading.navigationAdmissions == Dict.size first.dataLoading.navigationAdmissions
                    , hidden = Dict.get "project:child" collapsed.dataLoading.loadedNavigationBranches |> Maybe.map .inFlight
                    , pending = Dict.get "project:parent" collapsed.dataLoading.loadedNavigationBranches |> Maybe.map .projectRequestPending
                    , queued = List.member "project:parent" reopened.dataLoading.navigationQueue
                    , resumed = Dict.get "project:parent" afterOld.dataLoading.loadedNavigationBranches |> Maybe.map (\request -> ( request.projectOffset, request.inFlight ))
                    , childVisible = Set.member "child" afterOld.dataLoading.navigationVisibleProjectIds
                    , staleAbsent = not (Dict.member "late-hidden" afterOld.dataLoading.projectCardSummaries)
                    }
        , test "request errors stay paused across unrelated success until explicit retry" <|
            \_ ->
                let
                    initial = DataLoading.beginNavigationBranch "project" workspaceId (Just "a") model |> Tuple.first
                    both = DataLoading.beginNavigationBranch "project" workspaceId (Just "b") initial |> Tuple.first
                    failed = case Dict.get "project:a" both.dataLoading.loadedNavigationBranches of
                        Just request -> DataLoading.update (GotNavigationBranch workspaceId request.sessionEpoch request.generation "project:a" request.filterFingerprint 0 0 (Err Http.Timeout)) both |> Tuple.first
                        Nothing -> both
                    other = branchReply "project:b" [ project "other" (Just "b") ] [] False False failed
                    retried = DataLoading.beginNavigationBranchPage "project" "a" "project" other |> Tuple.first
                in
                Expect.equal ( Just ( False, False, False ), True, Just True )
                    ( Dict.get "project:a" other.dataLoading.loadedNavigationBranches |> Maybe.map (\request -> ( request.inFlight, request.projectRequestPending, request.taskRequestPending ))
                    , Dict.get "project:a" other.dataLoading.navigationPasses |> Maybe.andThen .projectError |> (/=) Nothing
                    , Dict.get "project:a" retried.dataLoading.loadedNavigationBranches |> Maybe.map .inFlight
                    )
        , test "same-filter refresh stages each kind to exhaustion without page-one truncation" <|
            \_ ->
                let
                    seeded = DataLoading.beginNavigationBranch "project" workspaceId (Just "parent") (DataLoading.mergeNavigationSummaries [ project "parent" Nothing ] [] model) |> Tuple.first
                    loaded = branchReply "project:parent" [ project "old-a" (Just "parent"), project "old-b" (Just "parent") ] [ task "old-task" Nothing ] False False seeded
                    fresh = DataLoading.revalidateNavigationForFilters loaded |> Tuple.first
                    first = branchReply "project:parent" (List.range 1 50 |> List.map (\n -> project ("fresh-" ++ String.fromInt n) (Just "parent"))) [ task "fresh-task" Nothing ] True False fresh
                    final = branchReply "project:parent" [ project "fresh-51" (Just "parent") ] [ task "ignored-other-kind" Nothing ] False True first
                in
                Expect.equal { oldDuring = True, freshDuring = False, taskCommitted = True, oldGone = True, allFresh = 52, taskStayedComplete = Just False }
                    { oldDuring = Dict.member "old-a" first.dataLoading.projectCardSummaries && Dict.member "old-b" first.dataLoading.projectCardSummaries
                    , freshDuring = Dict.member "fresh-1" first.dataLoading.projectCardSummaries
                    , taskCommitted = Dict.member "fresh-task" first.dataLoading.taskCardSummaries && not (Dict.member "old-task" first.dataLoading.taskCardSummaries)
                    , oldGone = not (Dict.member "old-a" final.dataLoading.projectCardSummaries)
                    , allFresh = Dict.size final.dataLoading.projectCardSummaries
                    , taskStayedComplete = Dict.get "project:parent" final.dataLoading.loadedNavigationBranches |> Maybe.map .taskHasMore
                    }
        , test "coalesced live refresh discards the shifted pass and restarts at offset zero" <|
            \_ ->
                let
                    initial = DataLoading.beginNavigationBranch "project" workspaceId (Just "parent") (DataLoading.mergeNavigationSummaries [ project "parent" Nothing ] [] model) |> Tuple.first
                    first = branchReply "project:parent" [ project "old" (Just "parent") ] [] True False initial
                    old = Dict.get "project:parent" first.dataLoading.loadedNavigationBranches
                    once = DataLoading.revalidateNavigationForFilters first |> Tuple.first
                    twice = DataLoading.revalidateNavigationForFilters once |> Tuple.first
                    released = case old of
                        Just request -> DataLoading.update (GotNavigationBranch workspaceId request.sessionEpoch request.generation "project:parent" request.filterFingerprint request.projectOffset request.taskOffset (Err Http.Timeout)) twice |> Tuple.first
                        Nothing -> twice
                    complete = branchReply "project:parent" [ project "replacement" (Just "parent") ] [] False False released
                in
                Expect.equal { held = 1, queued = 1, fresh = Just ( 0, True ), old = False, replacement = True }
                    { held = Dict.size twice.dataLoading.navigationAdmissions
                    , queued = List.length (List.filter ((==) "project:parent") twice.dataLoading.navigationQueue)
                    , fresh = Dict.get "project:parent" released.dataLoading.loadedNavigationBranches |> Maybe.map (\request -> ( request.projectOffset, request.inFlight ))
                    , old = Dict.member "old" complete.dataLoading.projectCardSummaries
                    , replacement = Dict.member "replacement" complete.dataLoading.projectCardSummaries
                    }
        , test "duplicate nonprogress stops one kind while the other continues" <|
            \_ ->
                let
                    seeded = DataLoading.beginNavigationBranch "project" workspaceId (Just "parent") model |> Tuple.first
                    first = branchReply "project:parent" [ project "duplicate" (Just "parent") ] [ task "task-one" Nothing ] True True seeded
                    second = branchReply "project:parent" [ project "duplicate" (Just "parent") ] [ task "task-two" Nothing ] True True first
                    final = branchReply "project:parent" [ project "ignored" (Just "parent") ] [ task "task-three" Nothing ] True False second
                in
                Expect.equal { state = Just ( True, False, False ), paused = True, projects = 1, tasks = 3 }
                    { state = Dict.get "project:parent" final.dataLoading.loadedNavigationBranches |> Maybe.map (\request -> ( request.projectHasMore, request.taskHasMore, request.inFlight ))
                    , paused = Dict.get "project:parent" final.dataLoading.navigationPasses |> Maybe.andThen .projectError |> (/=) Nothing
                    , projects = Dict.size final.dataLoading.projectCardSummaries
                    , tasks = Dict.size final.dataLoading.taskCardSummaries
                    }
        , test "the client ceiling retains truthful has-more and stops without issuing an illegal offset" <|
            \_ ->
                let
                    seeded = DataLoading.beginNavigationBranch "project" workspaceId (Just "parent") model |> Tuple.first
                    loading = seeded.dataLoading
                    atBoundary = { seeded | dataLoading = { loading | loadedNavigationBranches = Dict.update "project:parent" (Maybe.map (\request -> { request | projectOffset = 10000 })) loading.loadedNavigationBranches } }
                    result = branchReply "project:parent" (List.range 1 50 |> List.map (\n -> project ("ceiling-" ++ String.fromInt n) (Just "parent"))) [] True False atBoundary
                in
                Expect.equal ( Just ( 10000, True, ( False, False ) ), True, 50 )
                    ( Dict.get "project:parent" result.dataLoading.loadedNavigationBranches |> Maybe.map (\request -> ( request.projectOffset, request.projectHasMore, ( request.inFlight, request.projectRequestPending ) ))
                    , Dict.get "project:parent" result.dataLoading.navigationPasses |> Maybe.andThen .projectError |> (/=) Nothing
                    , Dict.size result.dataLoading.projectCardSummaries
                    )
        , test "visible detail demand is guarded and six physical slots survive workspace churn" <|
            \_ ->
                let
                    summaries = List.range 1 50 |> List.map (\n -> project ("detail-" ++ String.fromInt n) Nothing)
                    seeded = loadRoot workspaceId summaries [] { model | auth = { status = AuthReady, mode = Just "test" } }
                    reloaded = DataLoading.reloadNavigationForFilters seeded |> Tuple.first
                    newWorkspace = { reloaded | selectedWorkspaceId = Just "other" }
                    requested = DataLoading.ensureVisibleCardDetails workspaceId seeded.sessionRequestEpoch seeded.dataLoading.navigationGeneration (Set.fromList (List.map .id summaries)) Set.empty newWorkspace |> Tuple.first
                    released = Dict.toList seeded.dataLoading.projectCardDetailRequests |> List.foldl (\( id, request ) source -> DataLoading.update (GotProjectCardDetail request id (Err Http.Timeout)) source |> Tuple.first) requested
                in
                Expect.equal { first = 6, held = 6, stale = Nothing, released = 0 }
                    { first = Set.size seeded.dataLoading.cardDetailAdmissions
                    , held = Set.size requested.dataLoading.cardDetailAdmissions
                    , stale = requested.dataLoading.visibleDetailDemand
                    , released = Set.size released.dataLoading.cardDetailAdmissions
                    }
        , test "deep focus hydrates the target before ordinary cards without hydrating its ancestors" <|
            \_ ->
                let
                    summaries = List.range 1 100 |> List.map (\n -> project ("deep-" ++ String.fromInt n) (if n == 1 then Nothing else Just ("deep-" ++ String.fromInt (n - 1))))
                    seeded = DataLoading.mergeNavigationSummaries summaries [] { model | auth = { status = AuthReady, mode = Just "test" } }
                    focus = seeded.focus
                    focused = { seeded | focus = { focus | focusedEntity = Just ( "project", "deep-100" ) } }
                    demanded = DataLoading.ensureVisibleCardDetails workspaceId focused.sessionRequestEpoch focused.dataLoading.navigationGeneration Set.empty Set.empty focused |> Tuple.first
                    complete = case ( Dict.get "deep-100" demanded.dataLoading.projectCardDetailRequests, Dict.get "deep-100" demanded.dataLoading.projectCardSummaries ) of
                        ( Just request, Just summary ) -> DataLoading.update (GotProjectCardDetail request summary.id (Ok (Api.projectFromCardSummary summary))) demanded |> Tuple.first
                        _ -> demanded
                in
                Expect.equal ( [ "deep-100" ], [ "deep-100" ], 0 )
                    ( Dict.keys demanded.dataLoading.projectCardDetailRequests
                    , Dict.keys complete.dataLoading.projectCardDetailRequests
                    , Set.size complete.dataLoading.cardDetailAdmissions
                    )
        , test "authoritative parent membership removal retires cached child and grandchild replies" <|
            \_ ->
                let
                    parent = project "p" Nothing
                    child = project "c" (Just "p")
                    grandchild = project "g" (Just "c")
                    seeded = loadRoot workspaceId [ { parent | hasChildren = True } ] [] model
                    children = branchReply "project:p" [ { child | hasChildren = True } ] [] False False seeded
                    descendants = branchReply "project:c" [ { grandchild | hasChildren = True } ] [] True False children
                    old = Dict.get "project:g" descendants.dataLoading.loadedNavigationBranches
                    freshParent = DataLoading.beginNavigationBranch "project" workspaceId (Just "p") descendants |> Tuple.first
                    removed = branchReply "project:p" [] [] False False freshParent
                    afterOld = case old of
                        Just request -> DataLoading.update (GotNavigationBranch workspaceId request.sessionEpoch request.generation "project:g" request.filterFingerprint 0 0 (Ok { workspaceId = workspaceId, projects = { items = [ project "late-grandchild" (Just "g") ], hasMore = True }, tasks = { items = [], hasMore = False } })) removed |> Tuple.first
                        Nothing -> removed
                in
                Expect.equal { parentApplied = True, childRetired = Just False, grandchildRetired = Just False, fenced = True, held = 2, released = 1, lateAbsent = True, queued = [] }
                    { parentApplied = not (Dict.member "c" removed.dataLoading.projectCardSummaries)
                    , childRetired = Dict.get "project:c" removed.dataLoading.loadedNavigationBranches |> Maybe.map .inFlight
                    , grandchildRetired = Dict.get "project:g" removed.dataLoading.loadedNavigationBranches |> Maybe.map .inFlight
                    , fenced = case ( old, Dict.get "project:g" removed.dataLoading.loadedNavigationBranches ) of
                        ( Just oldRequest, Just current ) -> current.generation > oldRequest.generation
                        _ -> False
                    , held = Dict.size removed.dataLoading.navigationAdmissions
                    , released = Dict.size afterOld.dataLoading.navigationAdmissions
                    , lateAbsent = not (Dict.member "late-grandchild" afterOld.dataLoading.projectCardSummaries)
                    , queued = afterOld.dataLoading.navigationQueue
                    }
        , test "explicit branch retry fences old error and success callbacks at the same offsets" <|
            \_ ->
                let
                    initial = DataLoading.beginNavigationBranch "project" workspaceId (Just "parent") model |> Tuple.first
                    old = Dict.get "project:parent" initial.dataLoading.loadedNavigationBranches
                    message result = case old of
                        Just request -> GotNavigationBranch workspaceId request.sessionEpoch request.generation "project:parent" request.filterFingerprint 0 0 result
                        Nothing -> LoadRootNavigationPage "project"
                    failed = DataLoading.update (message (Err Http.Timeout)) initial |> Tuple.first
                    retry = DataLoading.beginNavigationBranchPage "project" "parent" "project" failed |> Tuple.first
                    lateError = DataLoading.update (message (Err Http.Timeout)) retry |> Tuple.first
                    lateSuccess = DataLoading.update (message (Ok { workspaceId = workspaceId, projects = { items = [ project "old-payload" (Just "parent") ], hasMore = False }, tasks = { items = [], hasMore = False } })) lateError |> Tuple.first
                in
                Expect.equal ( True, Just True, ( 1, False ) )
                    ( case ( old, Dict.get "project:parent" retry.dataLoading.loadedNavigationBranches ) of
                        ( Just previous, Just current ) -> current.generation > previous.generation
                        _ -> False
                    , Dict.get "project:parent" lateSuccess.dataLoading.loadedNavigationBranches |> Maybe.map .inFlight
                    , ( Dict.size lateSuccess.dataLoading.navigationAdmissions, Dict.member "old-payload" lateSuccess.dataLoading.projectCardSummaries )
                    )
        , test "Cards reopening a failed continuation retains fifty-plus cached children and both cursors" <|
            \_ ->
                Expect.equal { offsets = Just ( 100, 100 ), pass = Just ( False, 51 ), freshIdentity = True, displayed = ( 51, 51 ), completed = ( 52, 52 ), oldRetained = True, freshComplete = True }
                    (reopenFailedBranchScenario False)
        , test "Cards reopening a failed staged refresh preserves staging until each kind finishes" <|
            \_ ->
                Expect.equal { offsets = Just ( 50, 50 ), pass = Just ( True, 50 ), freshIdentity = True, displayed = ( 51, 51 ), completed = ( 51, 51 ), oldRetained = False, freshComplete = True }
                    (reopenFailedBranchScenario True)
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


loadRoot : String -> List Api.ProjectCardSummary -> List Api.TaskCardSummary -> Model -> Model
loadRoot workspace projects tasks source =
    let
        prepared =
            DataLoading.prepareRootNavigationRequest (Just workspace) source
    in
    case prepared.dataLoading.rootNavigationRequest of
        Just request ->
            DataLoading.update
                (GotRootNavigation workspace request.sessionEpoch prepared.dataLoading.activeWorkspaceLoadToken request.generation request.filterFingerprint 0 0
                    (Ok { workspaceId = workspace, projects = { items = projects, hasMore = False }, tasks = { items = tasks, hasMore = False } })
                )
                prepared
                |> Tuple.first

        Nothing ->
            prepared


hydrateDescriptions : String -> String -> Api.ProjectCardSummary -> Api.TaskCardSummary -> Model -> Model
hydrateDescriptions projectDescription taskDescription projectSummary taskSummary source =
    case
        ( Dict.get projectSummary.id source.dataLoading.projectCardDetailRequests
        , Dict.get taskSummary.id source.dataLoading.taskCardDetailRequests
        )
    of
        ( Just projectRequest, Just taskRequest ) ->
            let
                projectDetail =
                    Api.projectFromCardSummary projectSummary

                taskDetail =
                    Api.taskFromCardSummary taskSummary
            in
            source
                |> DataLoading.update (GotProjectCardDetail projectRequest projectSummary.id (Ok { projectDetail | description = Just projectDescription }))
                |> Tuple.first
                |> DataLoading.update (GotTaskCardDetail taskRequest taskSummary.id (Ok { taskDetail | description = Just taskDescription }))
                |> Tuple.first

        _ ->
            source


editorSession : Api.SessionContext
editorSession =
    { authMode = "test"
    , principal = { actorType = "user", actorId = "editor", actorLabel = "Editor", authority = "local", grantUserId = Nothing }
    , globalPermissions = { createWorkspace = False, superadmin = False }
    , workspace = Just { workspaceId = workspaceId, role = Just "edit", canRead = True, canEdit = True, canAdmin = False }
    }


workspaceShellSnapshotWire : String
workspaceShellSnapshotWire =
    Encode.object
        [ ( "schema_version", Encode.int 1 )
        , ( "transport", Encode.string "snapshot" )
        , ( "scope", Encode.object [ ( "scope", Encode.string "workspace" ), ( "workspace_id", Encode.string workspaceId ) ] )
        , ( "snapshot_profile", Encode.string "workspace_shell_v1" )
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
        , ( "resume_token", Encode.string "workspace-shell-token" )
        ]
        |> Encode.encode 0


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


summaryGuard : Types.CanonicalRequestGuard -> Model -> List String -> List String -> Types.CanonicalNavigationRequestGuard
summaryGuard guard source projectIds taskIds =
    let
        keys =
            List.map ((++) (guard.scopeKey ++ "|navigation-summary:project:")) projectIds ++ List.map ((++) (guard.scopeKey ++ "|navigation-summary:task:")) taskIds
    in
    { request = guard, navigationGeneration = source.dataLoading.navigationGeneration, entityGenerations = Dict.filter (\key _ -> List.member key keys) source.webSocket.targetGenerations }


syncModel : List Api.ProjectCardSummary -> Model
syncModel projects =
    let
        base = loadRoot workspaceId projects [] model
    in
    { base | auth = { status = AuthReady, mode = Just "test" }, sessionContext = Just editorSession }


acceptRoot : List Api.ProjectCardSummary -> List Api.TaskCardSummary -> Model -> Model
acceptRoot projects tasks source =
    case source.dataLoading.rootNavigationRequest of
        Just request ->
            DataLoading.update (GotRootNavigation workspaceId request.sessionEpoch source.dataLoading.activeWorkspaceLoadToken request.generation request.filterFingerprint request.projectOffset request.taskOffset (Ok { workspaceId = workspaceId, projects = { items = projects, hasMore = False }, tasks = { items = tasks, hasMore = False } })) source |> Tuple.first
        Nothing -> source


batchGuard : List ( String, String ) -> Model -> Types.CanonicalRequestGuard
batchGuard targets source =
    let
        key = "navigation-summaries:" ++ String.join "," (List.map (\( kind, entityId ) -> kind ++ ":" ++ entityId) targets)
    in
    { scopeKey = "workspace:" ++ workspaceId, targetKey = key, targetGeneration = Dict.get ("workspace:" ++ workspaceId ++ "|" ++ key) source.webSocket.targetGenerations |> Maybe.withDefault -1, sessionEpoch = source.sessionRequestEpoch, routeWorkspace = Just workspaceId, audienceId = "editor" }


summaryReply : Model -> Types.CanonicalRequestGuard -> List String -> List String -> Api.NavigationSummariesResponse -> Model -> Model
summaryReply captured guard projectIds taskIds result source =
    WebSocket.update (CanonicalNavigationSummariesFetched (summaryGuard guard captured projectIds taskIds) workspaceId projectIds taskIds (Ok result)) source |> Tuple.first


scopedFrame : Encode.Value -> String
scopedFrame frame =
    Encode.encode 0 (Encode.object [ ( "schema_version", Encode.int 1 ), ( "transport", Encode.string "frame" ), ( "scope", Encode.object [ ( "scope", Encode.string "workspace" ), ( "workspace_id", Encode.string workspaceId ) ] ), ( "frame", frame ) ])


requestSummaries : String -> List ( String, String ) -> Model -> Model
requestSummaries eventId targets source =
    requestSummariesWithInvalidations eventId targets [] source


requestSummariesWithInvalidations : String -> List ( String, String ) -> List Encode.Value -> Model -> Model
requestSummariesWithInvalidations eventId targets extraInvalidations source =
    let
        first = List.head targets |> Maybe.withDefault ( "project", "unused" )
        scope = Encode.object [ ( "scope", Encode.string "workspace" ), ( "workspace_id", Encode.string workspaceId ) ]
        envelope = Encode.object
            [ ( "schema_version", Encode.int 1 ), ( "event_id", Encode.string eventId ), ( "scope", Encode.string "workspace" ), ( "workspace_id", Encode.string workspaceId )
            , ( "entity", Encode.object [ ( "type", Encode.string (Tuple.first first) ), ( "id", Encode.string (Tuple.second first) ), ( "action", Encode.string "updated" ) ] )
            , ( "invalidations", Encode.list identity (List.map (\( kind, entityId ) -> Encode.object [ ( "kind", Encode.string "entity" ), ( "target", Encode.string (kind ++ ":" ++ entityId) ) ]) targets ++ extraInvalidations) )
            , ( "occurred_at", Encode.string "2026-10-02T00:00:00Z" ), ( "transaction", Encode.object [ ( "id", Encode.string eventId ), ( "cause", Encode.string "mcp" ), ( "request_id", Encode.null ) ] ), ( "actor", Encode.object [ ( "type", Encode.string "user" ), ( "id", Encode.string "foreign" ) ] )
            ]
    in
    WebSocket.update (WsMessageReceived (scopedFrame (Encode.object [ ( "schema_version", Encode.int 1 ), ( "type", Encode.string "change" ), ( "event", envelope ) ]))) source |> Tuple.first


branchReply : String -> List Api.ProjectCardSummary -> List Api.TaskCardSummary -> Bool -> Bool -> Model -> Model
branchReply key projects tasks projectMore taskMore source =
    case Dict.get key source.dataLoading.loadedNavigationBranches of
        Just request ->
            DataLoading.update (GotNavigationBranch request.workspaceId request.sessionEpoch request.generation key request.filterFingerprint request.projectOffset request.taskOffset
                (Ok { workspaceId = request.workspaceId, projects = { items = projects, hasMore = projectMore }, tasks = { items = tasks, hasMore = taskMore } })) source |> Tuple.first
        Nothing -> source


reopenFailedBranchScenario staged =
    let
        projectPage prefix start end = List.range start end |> List.map (\n -> project (prefix ++ String.fromInt n) (Just "parent"))
        taskPage prefix start end = List.range start end |> List.map (\n -> task (prefix ++ String.fromInt n) Nothing)
        parent = project "parent" Nothing
        seeded = loadRoot workspaceId [ { parent | hasChildren = True } ] [] model
        first = branchReply "project:parent" (projectPage "old-" 1 50) (taskPage "old-" 1 50) True True seeded
        second = branchReply "project:parent" (projectPage "old-" 51 51) (taskPage "old-" 51 51) (not staged) (not staged) first
        pending =
            if staged then
                DataLoading.revalidateNavigationForFilters second |> Tuple.first |> branchReply "project:parent" (projectPage "fresh-" 1 50) (taskPage "fresh-" 1 50) True True
            else second
        failed = case Dict.get "project:parent" pending.dataLoading.loadedNavigationBranches of
            Just request -> DataLoading.update (GotNavigationBranch workspaceId request.sessionEpoch request.generation "project:parent" request.filterFingerprint request.projectOffset request.taskOffset (Err Http.Timeout)) pending |> Tuple.first
            Nothing -> pending
        collapsed = Cards.update (ToggleTreeNode "proj-parent") failed |> Tuple.first
        reopened = Cards.update (ToggleTreeNode "proj-parent") collapsed |> Tuple.first
        completed = case Dict.get "project:parent" reopened.dataLoading.loadedNavigationBranches of
            Just request ->
                if staged && request.projectOffset == 50 then branchReply "project:parent" (projectPage "fresh-" 51 51) (taskPage "fresh-" 51 51) False False reopened
                else if not staged && request.projectOffset == 100 then branchReply "project:parent" (projectPage "old-" 52 52) (taskPage "old-" 52 52) False False reopened
                else branchReply "project:parent" (projectPage (if staged then "fresh-" else "old-") 1 50) (taskPage (if staged then "fresh-" else "old-") 1 50) True True reopened
            Nothing -> reopened
        counts current = ( Dict.values current.dataLoading.projectCardSummaries |> List.filter (\summary -> summary.parentId == Just "parent") |> List.length, Dict.size current.dataLoading.taskCardSummaries )
    in
    { offsets = Dict.get "project:parent" reopened.dataLoading.loadedNavigationBranches |> Maybe.map (\request -> ( request.projectOffset, request.taskOffset ))
    , pass = Dict.get "project:parent" reopened.dataLoading.navigationPasses |> Maybe.map (\progress -> ( progress.refreshing, Dict.size progress.projects ))
    , freshIdentity = case ( Dict.get "project:parent" failed.dataLoading.loadedNavigationBranches, Dict.get "project:parent" reopened.dataLoading.loadedNavigationBranches ) of
        ( Just previous, Just current ) -> current.generation > previous.generation
        _ -> False
    , displayed = counts reopened
    , completed = counts completed
    , oldRetained = Dict.member "old-51" completed.dataLoading.projectCardSummaries && Dict.member "old-51" completed.dataLoading.taskCardSummaries
    , freshComplete = Dict.get "project:parent" completed.dataLoading.loadedNavigationBranches |> Maybe.map (\request -> not request.projectHasMore && not request.taskHasMore && not request.inFlight) |> Maybe.withDefault False
    }
