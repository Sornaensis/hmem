module CardsFocusNavigationTest exposing (suite)

import Api
import AppShell
import Dict
import Expect
import Feature.Cards as Cards
import Feature.DataLoading as DataLoading
import Feature.Focus as Focus
import Helpers
import Http
import Set
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Types exposing (Flags, Model, Msg(..), Page(..), WorkspaceTab(..))
import Url


suite : Test
suite =
    describe "bounded Cards and Focus loading"
        [ test "an unloaded expanded project card starts exactly its branch request" <|
            \_ ->
                let
                    seeded =
                        DataLoading.mergeNavigationSummaries [ project "parent" ] [] model

                    expanded =
                        Cards.update (ToggleCardExpand "parent") seeded
                            |> Tuple.first
                in
                case Dict.get "project:parent" expanded.dataLoading.loadedNavigationBranches of
                    Just request ->
                        Expect.equal
                            { workspace = workspaceId, offset = 0, inFlight = True, succeeded = False }
                            { workspace = request.workspaceId, offset = request.projectOffset, inFlight = request.inFlight, succeeded = request.succeeded }

                    Nothing ->
                        Expect.fail "Expected an unloaded project expansion to request its branch"
        , test "Expand All hydrates and loads a visible project that was collapsed before its branch arrived" <|
            \_ ->
                let
                    seeded =
                        DataLoading.mergeNavigationSummaries [ project "expand-all-parent" ] [] model

                    cards =
                        seeded.cards

                    collapsed =
                        { seeded | cards = { cards | collapsedNodes = Dict.singleton "proj-expand-all-parent" True } }

                    expanded =
                        Cards.update ExpandAllNodes collapsed |> Tuple.first
                in
                Expect.equal
                    { collapsed = False, branchInFlight = True, detailInFlight = True }
                    { collapsed = Dict.get "proj-expand-all-parent" expanded.cards.collapsedNodes |> Maybe.withDefault False
                    , branchInFlight = Dict.get "project:expand-all-parent" expanded.dataLoading.loadedNavigationBranches |> Maybe.map .inFlight |> Maybe.withDefault False
                    , detailInFlight = Dict.get "expand-all-parent" expanded.dataLoading.projectCardDetailRequests |> Maybe.map .inFlight |> Maybe.withDefault False
                    }
        , test "an unhydrated description cannot be edited and a failed detail exposes a working retry" <|
            \_ ->
                let
                    summary =
                        project "description-state"

                    seeded =
                        DataLoading.mergeNavigationSummaries [ summary ] [] model

                    ( requested, _ ) =
                        DataLoading.ensureNavigationPresentation "workspace_root" Nothing seeded

                    pendingView =
                        Cards.viewProjectsTree workspaceId requested |> Query.fromHtml

                    firstRequest =
                        Dict.get summary.id requested.dataLoading.projectCardDetailRequests

                    failed =
                        case firstRequest of
                            Just request ->
                                DataLoading.update (GotProjectCardDetail request summary.id (Err Http.Timeout)) requested |> Tuple.first

                            Nothing ->
                                requested

                    failedView =
                        Cards.viewProjectsTree workspaceId failed |> Query.fromHtml

                    retried =
                        DataLoading.update (RetryCardDetail "project" summary.id) failed |> Tuple.first
                in
                Expect.all
                    [ \_ -> pendingView |> Query.has [ Selector.text "Loading description…" ]
                    , \_ -> pendingView |> Query.findAll [ Selector.text "Click to add description..." ] |> Query.count (Expect.equal 0)
                    , \_ -> failedView |> Query.has [ Selector.text "Description unavailable.", Selector.text "Retry" ]
                    , \_ ->
                        case ( firstRequest, Dict.get summary.id retried.dataLoading.projectCardDetailRequests ) of
                            ( Just first, Just second ) ->
                                Expect.equal True (second.inFlight && second.requestId > first.requestId)

                            _ ->
                                Expect.fail "Expected the failed detail request to restart"
                    ]
                    ()
        , test "direct focus requests only a target absent from the bounded card cache" <|
            \_ ->
                let
                    missing =
                        Focus.update (FocusEntity "project" "outside-root-page") model
                            |> Tuple.first

                    knownModel =
                        DataLoading.mergeNavigationSummaries [ project "inside-root-page" ] [] model

                    known =
                        Focus.update (FocusEntity "project" "inside-root-page") knownModel
                            |> Tuple.first
                in
                Expect.equal
                    { missingTarget = Just "outside-root-page"
                    , missingInFlight = True
                    , knownHasNoRequest = True
                    }
                    { missingTarget = Dict.get "project:outside-root-page" missing.dataLoading.navigationFocuses |> Maybe.map .entityId
                    , missingInFlight = missing.dataLoading.activeNavigationFocus |> Maybe.map .inFlight |> Maybe.withDefault False
                    , knownHasNoRequest = Dict.member "project:inside-root-page" known.dataLoading.navigationFocuses |> not
                    }
        , test "root presentation mounts one 25-card window while retaining the cached 50-card page" <|
            \_ ->
                let
                    seeded =
                        DataLoading.mergeNavigationSummaries (List.range 1 50 |> List.map (\number -> project ("root-" ++ String.fromInt number))) [] model

                    rendered =
                        Cards.viewProjectsTree workspaceId seeded |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> Query.findAll [ Selector.class "card-project" ] rendered |> Query.count (Expect.equal 25)
                    , \_ -> Expect.equal 50 (Dict.size seeded.projects)
                    , \_ -> Query.find [ Selector.class "navigation-load-more" ] rendered |> Query.has [ Selector.text "Load more projects" ]
                    ]
                    ()
        , test "collapsing a loaded branch unmounts its child cards without discarding branch membership" <|
            \_ ->
                let
                    parent =
                        project "parent"

                    child =
                        let
                            base =
                                project "child"
                        in
                        { base | parentId = Just "parent", hasChildren = False }

                    seeded =
                        DataLoading.mergeNavigationSummaries [ parent, child ] [] model

                    collapsed =
                        Cards.update (ToggleTreeNode "proj-parent") seeded |> Tuple.first

                    rendered =
                        Cards.viewProjectsTree workspaceId collapsed |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> Query.findAll [ Selector.class "card-project" ] rendered |> Query.count (Expect.equal 1)
                    , \_ -> Expect.equal True (Dict.member "child" collapsed.projects)
                    ]
                    ()
        , test "collapsed card extras stay unmounted while expanded cards retain editable project controls" <|
            \_ ->
                let
                    seeded =
                        DataLoading.mergeNavigationSummaries [ project "editable-parent" ] [] model

                    collapsed =
                        Cards.viewProjectsTree workspaceId seeded |> Query.fromHtml

                    expandedModel =
                        Cards.update (ToggleCardExpand "editable-parent") seeded |> Tuple.first

                    expanded =
                        Cards.viewProjectsTree workspaceId expandedModel |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> Query.findAll [ Selector.class "card-extras" ] collapsed |> Query.count (Expect.equal 0)
                    , \_ -> Query.findAll [ Selector.class "card-extras" ] expanded |> Query.count (Expect.equal 1)
                    , \_ -> Query.find [ Selector.class "card-project" ] expanded |> Query.has [ Selector.class "editable-text", Selector.text "editable-parent" ]
                    ]
                    ()
        , test "task details unmount while collapsed and remain editable when expanded" <|
            \_ ->
                let
                    seeded =
                        DataLoading.mergeNavigationSummaries [] [ task "editable-task" Nothing ] model

                    collapsed =
                        Cards.viewProjectsTree workspaceId seeded |> Query.fromHtml

                    expandedModel =
                        Cards.update (ToggleCardExpand "editable-task") seeded |> Tuple.first

                    expanded =
                        Cards.viewProjectsTree workspaceId expandedModel |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> Query.findAll [ Selector.class "card-task" ] collapsed |> Query.count (Expect.equal 1)
                    , \_ -> Query.findAll [ Selector.class "card-extras" ] collapsed |> Query.count (Expect.equal 0)
                    , \_ -> Query.findAll [ Selector.class "card-extras" ] expanded |> Query.count (Expect.equal 1)
                    , \_ -> Query.find [ Selector.class "card-task" ] expanded |> Query.has [ Selector.class "editable-text", Selector.text "editable-task" ]
                    ]
                    ()
        , test "focus and collapse reset reversible presentation cursors without dropping cached cards" <|
            \_ ->
                let
                    prepared =
                        DataLoading.prepareRootNavigationRequest (Just workspaceId) model

                    fingerprint =
                        prepared.dataLoading.rootNavigationRequest
                            |> Maybe.map .filterFingerprint
                            |> Maybe.withDefault ""

                    ( loaded, _ ) =
                        DataLoading.update
                            (GotRootNavigation workspaceId prepared.sessionRequestEpoch Nothing prepared.dataLoading.navigationGeneration fingerprint 0 0
                                (Ok { workspaceId = workspaceId, projects = { items = [ project "root" ], hasMore = True }, tasks = { items = [], hasMore = False } })
                            )
                            prepared

                    advanced =
                        DataLoading.update (LoadRootNavigationPage "project") loaded |> Tuple.first

                    focused =
                        Focus.update (FocusEntity "project" "root") advanced |> Tuple.first

                    collapsed =
                        Cards.update CollapseAllNodes advanced |> Tuple.first

                    projectOffset current =
                        current.dataLoading.rootNavigationPresentation |> Maybe.map .projectOffset
                in
                Expect.equal
                    { advanced = Just 1, focusReset = Just 0, collapseReset = Just 0, cached = True }
                    { advanced = projectOffset advanced
                    , focusReset = projectOffset focused
                    , collapseReset = projectOffset collapsed
                    , cached = Dict.member "root" collapsed.projects
                    }
        , test "the projection reads only visible membership while a large off-cache remains available for direct focus" <|
            \_ ->
                let
                    summaries =
                        List.range 1 100
                            |> List.map (\number -> project ("cached-" ++ String.fromInt number))

                    seeded =
                        DataLoading.mergeNavigationSummaries summaries [] model

                    loading =
                        seeded.dataLoading

                    visible =
                        { seeded
                            | dataLoading =
                                { loading
                                    | navigationVisibilityActive = True
                                    , navigationVisibleProjectIds = Set.singleton "cached-1"
                                }
                        }

                    projection =
                        Cards.cardTreeProjection workspaceId visible

                    tree =
                        Cards.viewProjectsTree workspaceId visible |> Query.fromHtml

                    focused =
                        Focus.update (FocusEntity "project" "cached-100") visible |> Tuple.first

                    focusedTree =
                        Cards.viewProjectsTree workspaceId focused |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> Query.findAll [ Selector.class "card-project" ] tree |> Query.count (Expect.equal 1)
                    , \_ -> Query.findAll [ Selector.class "card-project" ] focusedTree |> Query.count (Expect.equal 1)
                    , \_ -> Expect.equal 100 (Dict.size focused.projects)
                    , \_ -> Expect.equal [ "cached-1" ] (List.map .id projection.projects)
                    , \_ -> Expect.equal [] (List.map .id projection.tasks)
                    ]
                    ()
        , test "the pure projection keeps nested roots, children, project tasks, and task children ordered" <|
            \_ ->
                let
                    rootA =
                        project "root-a"

                    rootB =
                        project "root-b"

                    childA =
                        let
                            base =
                                project "child-a"
                        in
                        { base | parentId = Just "root-a" }

                    childB =
                        let
                            base =
                                project "child-b"
                        in
                        { base | parentId = Just "root-a" }

                    projectTaskA =
                        let
                            base =
                                task "project-task-a" Nothing
                        in
                        { base | projectId = Just "root-a" }

                    projectTaskB =
                        let
                            base =
                                task "project-task-b" Nothing
                        in
                        { base | projectId = Just "root-a" }

                    taskChildA =
                        let
                            base =
                                task "task-child-a" (Just "project-task-a")
                        in
                        { base | projectId = Just "root-a" }

                    taskChildB =
                        let
                            base =
                                task "task-child-b" (Just "project-task-a")
                        in
                        { base | projectId = Just "root-a" }

                    projection =
                        DataLoading.mergeNavigationSummaries
                            [ rootB, childB, rootA, childA ]
                            [ taskChildB, projectTaskB, taskChildA, projectTaskA ]
                            model
                            |> Cards.cardTreeProjection workspaceId

                    ids =
                        List.map .id
                in
                Expect.equal
                    { roots = [ "root-a", "root-b" ]
                    , children = [ "child-a", "child-b" ]
                    , projectTasks = [ "project-task-a", "project-task-b" ]
                    , taskChildren = [ "task-child-a", "task-child-b" ]
                    }
                    { roots = projection.projects |> List.filter (\item -> item.parentId == Nothing) |> ids
                    , children = Dict.get "root-a" projection.projectChildren |> Maybe.withDefault [] |> ids
                    , projectTasks = Dict.get "root-a" projection.projectTasks |> Maybe.withDefault [] |> ids
                    , taskChildren = Dict.get "project-task-a" projection.taskChildren |> Maybe.withDefault [] |> ids
                    }
        , test "presentation windows are exact across 25/50/51/75 boundaries and pin an out-of-window focus" <|
            \_ ->
                let
                    window total offset pins =
                        Cards.presentationWindow identity pins offset (List.range 1 total |> List.map String.fromInt)

                    first25 =
                        window 25 0 Set.empty

                    final50 =
                        window 50 25 Set.empty

                    final51 =
                        window 51 50 Set.empty

                    middle75 =
                        window 75 25 Set.empty

                    final80 =
                        window 80 75 Set.empty

                    pinned75 =
                        window 75 0 (Set.singleton "75")

                    fullyPinned =
                        window 75 25 (List.range 51 75 |> List.map String.fromInt |> Set.fromList)

                    pinsBeforeInsideAndAfter =
                        window 100 25 (Set.fromList [ "1", "26", "50", "100" ])

                    ordinaryPage offset =
                        Cards.presentationWindow String.fromInt (Set.singleton "50") offset (List.range 1 100)
                            |> List.filter ((/=) 50)

                    everyOrdinaryCard =
                        [ ordinaryPage 0, ordinaryPage 24, ordinaryPage 48, ordinaryPage 72, ordinaryPage 96 ]
                            |> List.concat

                    unrelatedBranchPins =
                        Cards.presentationWindow identity (Set.singleton "outside-branch") 0 (List.range 1 50 |> List.map String.fromInt)

                    localBranchPin =
                        Cards.presentationWindow identity (Set.singleton "50") 0 (List.range 1 50 |> List.map String.fromInt)

                    overflowingPins =
                        Cards.presentationWindow identity (List.range 1 30 |> List.map String.fromInt |> Set.fromList) 0 (List.range 1 50 |> List.map String.fromInt)

                    overflowingPinsNext =
                        Cards.presentationWindow identity (List.range 1 30 |> List.map String.fromInt |> Set.fromList) 1 (List.range 1 50 |> List.map String.fromInt)

                    terminalPins =
                        Cards.presentationWindow identity (List.range 1 30 |> List.map String.fromInt |> Set.fromList) 0 (List.range 1 30 |> List.map String.fromInt)
                in
                Expect.all
                    [ \_ -> ( List.length first25, List.head first25, List.reverse first25 |> List.head ) |> Expect.equal ( 25, Just "1", Just "25" )
                    , \_ -> ( List.length final50, List.head final50, List.reverse final50 |> List.head ) |> Expect.equal ( 25, Just "26", Just "50" )
                    , \_ -> Expect.equal [ "51" ] final51
                    , \_ -> ( List.length middle75, List.head middle75, List.reverse middle75 |> List.head ) |> Expect.equal ( 25, Just "26", Just "50" )
                    , \_ -> Expect.equal [ "76", "77", "78", "79", "80" ] final80
                    , \_ -> ( List.length pinned75, List.member "75" pinned75, List.member "25" pinned75 ) |> Expect.equal ( 25, True, False )
                    , \_ -> ( List.length fullyPinned, List.head fullyPinned, List.reverse fullyPinned |> List.head ) |> Expect.equal ( 26, Just "26", Just "75" )
                    , \_ ->
                        Expect.equal
                            { before = True, insideStart = True, insideEnd = True, after = True, count = 25 }
                            { before = List.member "1" pinsBeforeInsideAndAfter
                            , insideStart = List.member "26" pinsBeforeInsideAndAfter
                            , insideEnd = List.member "50" pinsBeforeInsideAndAfter
                            , after = List.member "100" pinsBeforeInsideAndAfter
                            , count = List.length pinsBeforeInsideAndAfter
                            }
                    , \_ -> Expect.equal (List.filter ((/=) 50) (List.range 1 100)) everyOrdinaryCard
                    , \_ -> Expect.equal ( 25, Just "25" ) ( List.length unrelatedBranchPins, List.reverse unrelatedBranchPins |> List.head )
                    , \_ -> Expect.equal ( 25, Just "50" ) ( List.length localBranchPin, List.reverse localBranchPin |> List.head )
                    , \_ -> Expect.equal (List.range 1 31 |> List.map String.fromInt) overflowingPins
                    , \_ -> Expect.equal ((List.range 1 30 |> List.map String.fromInt) ++ [ "32" ]) overflowingPinsNext
                    , \_ -> Expect.equal (List.range 1 30 |> List.map String.fromInt) terminalPins
                    , \_ -> Expect.equal [ 25, 24, 1, 1, 1 ] (List.map Helpers.presentationOrdinaryCapacity [ 0, 1, 24, 25, 30 ])
                    ]
                    ()
        ]


workspaceId : String
workspaceId =
    "workspace-1"


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


project : String -> Api.ProjectCardSummary
project id =
    { id = id
    , workspaceId = workspaceId
    , parentId = Nothing
    , name = id
    , status = Api.ProjActive
    , priority = 1
    , createdAt = "2026-01-01T00:00:00Z"
    , updatedAt = "2026-01-01T00:00:00Z"
    , directProjectCount = 0
    , directTaskCount = 0
    , hasChildren = True
    , readinessRollup = { openProjectCount = 0, closedProjectCount = 0, openTaskCount = 0, doneTaskCount = 0, cancelledTaskCount = 0, blockedTaskCount = 0, dependencyBlockedTaskCount = 0, openDependencyCount = 0, completionReady = True }
    }


task : String -> Maybe String -> Api.TaskCardSummary
task id parentId =
    { id = id
    , workspaceId = workspaceId
    , projectId = Nothing
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
