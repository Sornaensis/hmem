module PlanningCountsTest exposing (tests)

import Api
import AppShell
import Dict
import Expect
import Feature.DataLoading as Loading
import Http
import Json.Decode as Decode
import Main
import Page.Workspace as Workspace
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Types exposing (..)
import Url


tests : Test
tests =
    describe "Authoritative workspace planning totals"
        [ test "decodes the two-count HTTP contract" <| \_ ->
            Decode.decodeString Api.workspacePlanningCountsDecoder "{\"open_projects\":2,\"open_tasks\":60}"
                |> Expect.equal (Ok { openProjects = 2, openTasks = 60 })
        , test "collapsed 60-task workspace renders the same totals with empty local task cache and ongoing pages" <| \_ ->
            let
                accepted = settle { openProjects = 2, openTasks = 60 } (start ready)
                loading = accepted.dataLoading
                collapsed = { accepted | tasks = Dict.empty, projects = Dict.empty, dataLoading = { loading | loadingWorkspaceData = True, pendingWorkspaceLoads = 2 } }
                view = Workspace.viewWorkspacePage "workspace-1" collapsed |> Query.fromHtml
            in
            Expect.all
                [ \_ -> view |> Query.find [ Selector.class "workspace-summary" ] |> Query.has [ Selector.text "2 open projects", Selector.text "60 open tasks" ]
                , \_ -> view |> Query.find [ Selector.class "sticky-workspace-summary" ] |> Query.has [ Selector.text "2 open projects", Selector.text "60 open tasks" ]
                , \_ -> Expect.equal 0 (Dict.size collapsed.tasks)
                ] ()
        , test "navigation filters, pagination and focus never recompute or request counts" <| \_ ->
            let
                accepted = settle { openProjects = 2, openTasks = 60 } (start ready)
                search = accepted.search
                focus = accepted.focus
                loading = accepted.dataLoading
                changed = { accepted | search = { search | query = "filtered" }, focus = { focus | focusedEntity = Just ( "task", "unloaded" ) }, dataLoading = { loading | navigationGeneration = loading.navigationGeneration + 1, pendingWorkspaceLoads = 1 } }
                next = start changed
            in
            Expect.equal accepted.dataLoading.planningCounts next.dataLoading.planningCounts
        , test "many invalidations coalesce behind one physical request and discard its superseded result" <| \_ ->
            let
                requested = start ready
                first = guard requested
                dirty = List.foldl (\_ -> Loading.invalidatePlanningCounts >> start) requested (List.range 1 60)
                refreshed = Main.update (GotWorkspacePlanningCounts first (Ok { openProjects = 1, openTasks = 1 })) dirty |> Tuple.first
                finished = settle { openProjects = 3, openTasks = 61 } refreshed
            in
            Expect.equal { occupied = True, before = 2, after = 3, discarded = Nothing, current = True, value = Just { openProjects = 3, openTasks = 61 } }
                { occupied = dirty.dataLoading.planningCounts.active == Just first, before = dirty.dataLoading.planningCounts.nextToken, after = refreshed.dataLoading.planningCounts.nextToken, discarded = refreshed.dataLoading.planningCounts.value, current = finished.dataLoading.planningCounts.current, value = finished.dataLoading.planningCounts.value }
        , test "failed first and stale refresh reads stay truthful and retry gets a fresh token" <| \_ ->
            let
                initial = start ready
                failed = Main.update (GotWorkspacePlanningCounts (guard initial) (Err Http.Timeout)) initial |> Tuple.first
                retried = Main.update RetryWorkspacePlanningCounts failed |> Tuple.first
                zero = settle { openProjects = 0, openTasks = 0 } retried
                refreshing = zero |> Loading.invalidatePlanningCounts |> start
                failedRefresh = Main.update (GotWorkspacePlanningCounts (guard refreshing) (Err Http.NetworkError)) refreshing |> Tuple.first
            in
            Expect.equal
                [ [ "Loading planning totals…" ], [ "Planning totals unavailable" ], [ "0 open projects", "0 open tasks" ], [ "Updating planning totals…" ], [ "Planning totals unavailable" ] ]
                (List.map (.dataLoading >> .planningCounts >> Workspace.workspaceSummaryParts) [ initial, failed, zero, refreshing, failedRefresh ])
        , test "retired workspace and session successes/errors release only their own physical slot" <| \_ ->
            let
                initial = start ready
                old = guard initial
                session = sessionFor "workspace-2" "new-audience"
                loading = initial.dataLoading
                switched = { initial | page = WorkspacePage "workspace-2", selectedWorkspaceId = Just "workspace-2", sessionRequestEpoch = initial.sessionRequestEpoch + 1, sessionContext = Just session, dataLoading = { loading | planningCounts = Loading.retirePlanningCounts loading.planningCounts } } |> start
                released = Main.update (GotWorkspacePlanningCounts old (Err Http.Timeout)) switched |> Tuple.first
                current = guard released
                duplicate = Main.update (GotWorkspacePlanningCounts old (Ok { openProjects = 99, openTasks = 99 })) released |> Tuple.first
                accepted = settle { openProjects = 0, openTasks = 1 } duplicate
            in
            Expect.equal { old = Just old, discarded = Nothing, workspace = "workspace-2", active = Just current, value = Just { openProjects = 0, openTasks = 1 } }
                { old = switched.dataLoading.planningCounts.active, discarded = released.dataLoading.planningCounts.value, workspace = current.workspaceId, active = duplicate.dataLoading.planningCounts.active, value = accepted.dataLoading.planningCounts.value }
        , test "audience changes and same-epoch permission revoke/regrant cannot accept old counts" <| \_ ->
            let
                initial = start ready
                old = guard initial
                changed = { initial | sessionContext = Just (sessionFor "workspace-1" "other-audience") } |> start
                released = Main.update (GotWorkspacePlanningCounts old (Ok { openProjects = 99, openTasks = 99 })) changed |> Tuple.first
                revoked = { released | sessionContext = Nothing } |> start
                restored = { revoked | sessionContext = Just (sessionFor "workspace-1" "other-audience") } |> start
                fresh = Main.update (GotWorkspacePlanningCounts (guard released) (Ok { openProjects = 99, openTasks = 99 })) restored |> Tuple.first
            in
            Expect.equal ( Nothing, Nothing, True ) ( released.dataLoading.planningCounts.value, fresh.dataLoading.planningCounts.value, (guard fresh).token > (guard released).token )
        , test "failed totals expose retry in both header presentations" <| \_ ->
            let
                initial = start ready
                failed = Main.update (GotWorkspacePlanningCounts (guard initial) (Err Http.Timeout)) initial |> Tuple.first
            in
            Workspace.viewWorkspacePage "workspace-1" failed |> Query.fromHtml
                |> Query.findAll [ Selector.tag "button", Selector.text "Retry planning totals" ] |> Query.count (Expect.equal 2)
        ]


start : Model -> Model
start = Loading.syncPlanningCounts >> Tuple.first


guard : Model -> PlanningCountGuard
guard model =
    model.dataLoading.planningCounts.active |> Maybe.withDefault { workspaceId = "missing", sessionEpoch = -1, audience = "", token = -1, generation = -1 }


settle : Api.WorkspacePlanningCounts -> Model -> Model
settle value model = Main.update (GotWorkspacePlanningCounts (guard model) (Ok value)) model |> Tuple.first


sessionFor : String -> String -> Api.SessionContext
sessionFor workspace actor =
    { authMode = "test", effectiveActor = Nothing, principal = { actorType = "user", actorId = actor, actorLabel = actor, authority = "local", grantUserId = Nothing }
    , globalPermissions = { createWorkspace = False, superadmin = False }
    , workspace = Just { workspaceId = workspace, role = Just "edit", canRead = True, canEdit = True, canAdmin = False }
    }


ready : Model
ready =
    let
        flags = { apiUrl = "https://api.example", wsUrl = "wss://api.example", sessionId = "session", runtimeMode = "test", authTokenStorageKey = "hmem-token", authTokenPresent = False, loginUrl = Nothing, logoutUrl = Nothing }
        url = { protocol = Url.Https, host = "app.example", port_ = Nothing, path = "/workspace/workspace-1", query = Nothing, fragment = Nothing }
        initial = AppShell.initModel Nothing url (WorkspacePage "workspace-1") flags Nothing { tab = ProjectsTab, focus = Nothing, observationId = Nothing } |> AppShell.finalizeInit (WorkspacePage "workspace-1")
        loading = initial.dataLoading
        workspace = { id = "workspace-1", name = "Workspace", workspaceType = Api.Planning, ghOwner = Nothing, ghRepo = Nothing, createdAt = "now", updatedAt = "now" }
    in
    { initial | auth = { status = AuthReady, mode = Just "test" }, sessionContext = Just (sessionFor "workspace-1" "reader"), workspaces = Dict.singleton "workspace-1" workspace, dataLoading = { loading | loadingWorkspaceData = False, pendingWorkspaceLoads = 0 } }
