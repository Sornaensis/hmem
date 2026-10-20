module StartupAdmissionTest exposing (tests)

import Api
import AppShell
import Dict
import Expect
import Json.Encode
import Feature.ChangeStream as ChangeStream
import Feature.DataLoading as Loading
import Main
import Set
import StartupAdmission as Admission exposing (Phase(..))
import Test exposing (..)
import Types exposing (..)
import Url


tests : Test
tests =
    describe "Startup physical admission and producer handoff"
        [ test "pre-paint counts and ordinary calls share six nonrefundable transmissions" <| \_ ->
            let
                counted = admit "planning" owned |> admit "observation" |> Admission.openInitialWave
                wave = List.foldl (\key state -> Admission.admitOrdinary key state |> Maybe.withDefault state) counted [ "branch1", "branch2", "detail1", "detail2", "detail3" ]
                completed = List.foldl Admission.complete wave (Set.toList wave.physical)
            in
            Expect.equal ( ( 6, 0 ), ( Nothing, 6 ) ) ( ( wave.spent, Admission.remaining wave ), ( Admission.admit "retry" completed, completed.spent ) )
        , test "retirement, root retries and repeated paint waves cannot refill initial debt" <| \_ ->
            let
                first = owned |> admit "planning" |> Admission.openInitialWave |> admit "ordinary" |> Admission.complete "ordinary" |> Admission.retire |> Admission.reconcile (Just owner)
                repeated = Admission.openInitialWave first
            in
            Expect.equal ( 2, True, first ) ( repeated.spent, repeated.initialWaveOpened, repeated )
        , test "first root receipt is foreground baseline and every later recovery spends persistent debt" <| \_ ->
            let
                first = Admission.recordRecovery "root-first" owned |> Admission.complete "root-first"
                retry = first |> Admission.recordRecovery "root-retry" |> Admission.complete "root-retry" |> Admission.retire |> Admission.reconcile (Just owner)
                exhausted = { retry | spent = 6 } |> Admission.recordRecovery "root-refresh"
            in
            Expect.equal ( ( 0, True ), ( 7, 0, True ) )
                ( ( first.spent, first.rootStarted ), ( exhausted.spent, Admission.remaining exhausted, Set.member "root-refresh" exhausted.physical ) )
        , test "a new owner retains old physical occupancy and exact completion never refunds either owner" <| \_ ->
            let
                previous = admit "old-observation" owned
                next = Admission.reconcile (Just { owner | sessionEpoch = 2 }) previous |> admit "new-planning"
                duplicate = next |> Admission.complete "old-observation" |> Admission.complete "old-observation"
            in
            Expect.equal ( 1, Set.singleton "new-planning" ) ( duplicate.spent, duplicate.physical )
        , test "closure and subsequent normal paint are distinct, with no count or ordinary dispatch in between" <| \_ ->
            let
                armed = owned |> Admission.openInitialWave |> Admission.arm "settled" True
                closing = Admission.acknowledgeClosure "settled" armed
                normal = Admission.acknowledgeNormal "settled" closing
            in
            Expect.equal ( ( AwaitNormalPaint "settled", Nothing ), ( Nothing, Normal ) )
                ( ( closing.phase, Admission.admit "queued-count" closing ), ( Admission.admitOrdinary "queued-branch" closing, normal.phase ) )
        , test "dirty or active physical work revokes an arm and stale ACK cannot reopen admission" <| \_ ->
            let
                armed = owned |> Admission.openInitialWave |> Admission.arm "before" True
                revoked = Admission.arm "after" False armed
            in
            Expect.equal ( Initial, revoked, revoked ) ( revoked.phase, Admission.acknowledgeClosure "before" revoked, Admission.acknowledgeNormal "before" revoked )
        , test "inactive header continuation grants no ordinary request and retains startup debt" <| \_ ->
            let
                exhausted = { owned | spent = 6 } |> Admission.headerOnly
                counted = admit "count-continuation" exhausted
            in
            Expect.equal ( 6, Nothing, Set.singleton "count-continuation" ) ( counted.spent, Admission.admitOrdinary "branch" counted, counted.physical )
        , test "count callback invalidation coalesces while one physical slot is occupied" <| \_ ->
            let
                requested = Loading.syncPlanningCounts ready |> Tuple.first
                first = planningGuard requested
                busy = List.foldl (\_ state -> Loading.invalidatePlanningCounts state |> Loading.syncPlanningCounts |> Tuple.first) requested (List.range 1 40)
                refreshed = Main.update (GotWorkspacePlanningCounts first (Ok { openProjects = 1, openTasks = 2 })) busy |> Tuple.first
            in
            Expect.equal ( ( 1, Just first ), ( 2, Nothing ) )
                ( ( busy.dataLoading.startupAdmission.spent, busy.dataLoading.planningCounts.active ), ( refreshed.dataLoading.startupAdmission.spent, refreshed.dataLoading.planningCounts.value ) )
        , test "exhausted pending counts progress on Observations without a Projects click or false freshness" <| \_ ->
            countOnlyProgress ObservationsTab
        , test "exhausted pending counts progress on Timeline without a Projects click or false freshness" <| \_ ->
            countOnlyProgress TimelineTab
        , test "authorization pending cannot inherit an older live canonical stream" <| \_ ->
            let
                base = exhaustedReady ProjectsTab
                admin = base.workspaceAdmin
                blocked = { base | workspaceAdmin = { admin | authorizationPending = Just { workspaceId = "workspace-1", sessionEpoch = base.sessionRequestEpoch, sessionKey = "pending" } } }
                    |> sync
            in
            Expect.equal ( False, Nothing, 6 ) ( blocked.dataLoading.startupAdmission.authorized, blocked.dataLoading.planningCounts.active, blocked.dataLoading.startupAdmission.spent )
        , test "selected canonical scope must match and explicit scoped failure allows truthful retry progress" <| \_ ->
            let
                base = exhaustedReady ObservationsTab
                socket = base.webSocket
                foreign = { base | webSocket = { socket | streams = Dict.singleton "workspace:workspace-1" { liveStream | scope = ChangeStream.Workspace "foreign" } } } |> sync
                offline = { base | webSocket = { socket | streams = Dict.singleton "workspace:workspace-1" { liveStream | live = False, failure = Just "offline" } } } |> sync
            in
            Expect.equal ( Nothing, True ) ( foreign.dataLoading.planningCounts.active, offline.dataLoading.planningCounts.active /= Nothing )
        , test "current closure ACK leaves Main final synchronization parked until the next paint" <| \_ ->
            let
                base = exhaustedReady ProjectsTab |> sync
                nonce = Loading.backgroundPaintNonce base
                closing = Loading.acceptBackgroundPaint nonce base |> Loading.syncPlanningCountsResult |> Tuple.first
                old = Loading.acceptBackgroundPaint nonce closing |> Tuple.first
                resumed = Loading.acceptBackgroundPaint (Loading.backgroundPaintNonce old) old |> Tuple.first
            in
            Expect.equal ( True, ( Nothing, 0, True ) )
                ( case closing.dataLoading.startupAdmission.phase of
                    AwaitNormalPaint _ -> True
                    _ -> False
                , ( closing.dataLoading.planningCounts.active, closing.dataLoading.backgroundAdmission.remaining, resumed.dataLoading.planningCounts.active /= Nothing ) )
        , test "dirty headers revoke a closure nonce and cannot dispatch through a stale ACK" <| \_ ->
            let
                armed = exhaustedReady ProjectsTab |> sync
                nonce = Loading.backgroundPaintNonce armed
                dirty = Loading.invalidatePlanningCounts armed |> Loading.acceptBackgroundPaint nonce |> Tuple.first |> sync
            in
            Expect.equal ( True, Nothing, 0 )
                ( Loading.backgroundPaintNonce dirty /= nonce, dirty.dataLoading.planningCounts.active, dirty.dataLoading.backgroundAdmission.remaining )
        , test "old Observation physical completion unblocks only the new owner without accepting its value" <| \_ ->
            let
                base = ready
                workspace = Dict.get "workspace-1" base.workspaces |> Maybe.map (\value -> { value | workspaceType = Api.Repository })
                requested = { base | workspaces = workspace |> Maybe.map (Dict.singleton "workspace-1") |> Maybe.withDefault Dict.empty } |> sync
                old = List.head requested.observations.discovery.physical
                switched = { requested | sessionRequestEpoch = requested.sessionRequestEpoch + 1 } |> sync
                released = old |> Maybe.map (\request -> Main.update (GotObservationDiscovery request (Ok (discoveryCounts 99 request))) switched |> Tuple.first) |> Maybe.withDefault switched
                retained = old |> Maybe.map (\request -> List.member request switched.observations.discovery.physical && Set.member (discoveryCountKey request) switched.dataLoading.startupAdmission.physical) |> Maybe.withDefault False
                completed = old |> Maybe.map (\request -> not (List.member request released.observations.discovery.physical) && not (Set.member (discoveryCountKey request) released.dataLoading.startupAdmission.physical)) |> Maybe.withDefault False
            in
            Expect.all
                [ \_ -> Expect.equal ( Nothing, Nothing, True ) ( switched.observations.workspaceCounts.active, released.observations.workspaceCounts.value, released.observations.workspaceCounts.active /= Nothing )
                , \_ -> Expect.equal (True,True,1) (retained,completed,List.length released.observations.discovery.physical)
                , \_ -> Expect.equal requested.observations.orderedIds released.observations.orderedIds
                , \_ -> Expect.equal requested.dataLoading.initialObservationLoad released.dataLoading.initialObservationLoad ] ()
        , test "actual session retirement retains physical count debt until exact stale callback" <| \_ ->
            let
                requested = Loading.syncPlanningCounts ready |> Tuple.first
                old = planningGuard requested
                retired = AppShell.handleOwned AppShell.AuthUnauthorizedMsg requested |> Tuple.first
                released = Main.update (GotWorkspacePlanningCounts old (Ok { openProjects = 99, openTasks = 99 })) retired |> Tuple.first
            in
            Expect.equal ( ( 1, False, True ), ( 1, 1, Nothing ) )
                ( ( retired.dataLoading.startupAdmission.spent, retired.dataLoading.startupAdmission.authorized, not (Set.isEmpty retired.dataLoading.startupAdmission.physical) )
                , ( released.dataLoading.startupAdmission.spent, Set.size retired.dataLoading.startupAdmission.physical - Set.size released.dataLoading.startupAdmission.physical, released.dataLoading.planningCounts.value ) )
        ]


sync : Model -> Model
sync model = Loading.syncPlanningCountsResult ( model, Cmd.none ) |> Tuple.first


countOnlyProgress : WorkspaceTab -> Expect.Expectation
countOnlyProgress tab =
    let
        pending = exhaustedRepository tab
        resumed = sync pending
    in
    Expect.equal ( ( False, True ), ( 6, 0, HeaderOnly ) )
        ( ( resumed.dataLoading.planningCounts.current || resumed.observations.workspaceCounts.current, resumed.dataLoading.planningCounts.active /= Nothing && resumed.observations.workspaceCounts.active /= Nothing ), ( resumed.dataLoading.startupAdmission.spent, resumed.dataLoading.backgroundAdmission.remaining, resumed.dataLoading.startupAdmission.phase ) )


exhaustedRepository : WorkspaceTab -> Model
exhaustedRepository tab =
    let
        base = ready
        workspace = Dict.get "workspace-1" base.workspaces |> Maybe.map (\value -> { value | workspaceType = Api.Repository })
        requested = { base | workspaces = workspace |> Maybe.map (Dict.singleton "workspace-1") |> Maybe.withDefault Dict.empty } |> sync
        planning = Main.update (GotWorkspacePlanningCounts (planningGuard requested) (Ok { openProjects = 1, openTasks = 2 })) requested |> Tuple.first
        accepted = List.head planning.observations.discovery.physical |> Maybe.map (\request -> Main.update (GotObservationDiscovery request (Ok (discoveryCounts 60 request))) planning |> Tuple.first) |> Maybe.withDefault planning
        loading = accepted.dataLoading
        counts = loading.planningCounts
        admission = loading.startupAdmission
        observations = accepted.observations
        observationCounts = observations.workspaceCounts
        root = loading.rootNavigationRequest |> Maybe.map (\request -> { request | inFlight = False, projectCardCount = 1, succeeded = True })
        socket = accepted.webSocket
    in
    { accepted | activeTab = tab
        , dataLoading = { loading | rootNavigationRequest = root, planningCounts = { counts | current = False, pending = True, active = Nothing }, startupAdmission = { admission | spent = 6, physical = Set.empty } }
        , observations = { observations | workspaceCounts = { observationCounts | current = False, pending = True, active = Nothing } }
        , webSocket = { socket | streams = Dict.singleton "workspace:workspace-1" liveStream }
    }


admit : String -> Admission.State -> Admission.State
admit key state = Admission.admit key state |> Maybe.withDefault state


owner : Admission.Owner
owner = { workspaceId = "workspace-1", sessionEpoch = 1, audience = "reader" }


owned : Admission.State
owned = Admission.reconcile (Just owner) Admission.init


planningGuard : Model -> PlanningCountGuard
planningGuard model = model.dataLoading.planningCounts.active |> Maybe.withDefault { workspaceId = "missing", sessionEpoch = -1, audience = "", token = -1, generation = -1 }


exhaustedReady : WorkspaceTab -> Model
exhaustedReady tab =
    let
        requested = Loading.syncPlanningCounts ready |> Tuple.first
        accepted = Main.update (GotWorkspacePlanningCounts (planningGuard requested) (Ok { openProjects = 1, openTasks = 2 })) requested |> Tuple.first
        loading = accepted.dataLoading
        counts = loading.planningCounts
        admission = loading.startupAdmission
        root = loading.rootNavigationRequest |> Maybe.map (\request -> { request | inFlight = False, projectCardCount = 1, succeeded = True })
        socket = accepted.webSocket
    in
    { accepted | activeTab = tab, dataLoading = { loading | rootNavigationRequest = root, planningCounts = { counts | current = False, pending = True, active = Nothing }, startupAdmission = { admission | spent = 6, physical = Set.empty } }, webSocket = { socket | streams = Dict.singleton "workspace:workspace-1" liveStream } }


liveStream : ChangeStream.State
liveStream =
    let initial = ChangeStream.init (ChangeStream.Workspace "workspace-1") [] in
    { initial | snapshotApplied = True, live = True }


ready : Model
ready =
    let
        flags = { apiUrl = "https://api.example", wsUrl = "wss://api.example", sessionId = "session", runtimeMode = "test", authTokenStorageKey = "hmem-token", authTokenPresent = False, loginUrl = Nothing, logoutUrl = Nothing }
        url = { protocol = Url.Https, host = "app.example", port_ = Nothing, path = "/workspace/workspace-1", query = Nothing, fragment = Nothing }
        initial = AppShell.initModel Nothing url (WorkspacePage "workspace-1") flags Nothing { tab = ProjectsTab, focus = Nothing, observationId = Nothing } |> AppShell.finalizeInit (WorkspacePage "workspace-1")
        session = { authMode = "test", effectiveActor = Nothing, principal = { actorType = "user", actorId = "reader", actorLabel = "reader", authority = "local", grantUserId = Nothing }, globalPermissions = { createWorkspace = False, superadmin = False }, workspace = Just { workspaceId = "workspace-1", role = Just "edit", canRead = True, canEdit = True, canAdmin = False } }
        workspace = { id = "workspace-1", name = "Workspace", workspaceType = Api.Planning, ghOwner = Nothing, ghRepo = Nothing, createdAt = "now", updatedAt = "now" }
        loading = initial.dataLoading
        observations = initial.observations
        discovery = observations.discovery
    in
    { initial | auth = { status = AuthReady, mode = Just "test" }, sessionContext = Just session, workspaces = Dict.singleton "workspace-1" workspace, dataLoading = { loading | activeWorkspaceLoadToken = Just 1 }, observations = {observations | discovery = {discovery | sessionId = "10000000-0000-4000-8000-000000000001"}} }
        |> Loading.prepareRootNavigationRequest (Just "workspace-1")


discoveryCounts total request =
    {contextToken = Maybe.withDefault ("startup-context-" ++ String.fromInt request.generation) request.contextToken, expiresAt = "2099-01-01T00:00:00Z", retrieval = {mode = "fts_only", reason = Just "disabled"}, complete = True
    , page = {items = [], tree = Nothing, hasMore = False, counts = {workspaceId = "workspace-1", totalCount = total, matchCount = total}}}


discoveryCountKey request =
    "discovery-count|" ++ Json.Encode.encode 0 (Json.Encode.list Json.Encode.string [request.owner,request.sessionId,String.fromInt request.generation,String.fromInt request.countGeneration,String.fromInt request.workspaceCountGeneration])
