module UiAdjustmentsTest exposing (suite)

import Api
import AppShell exposing (AppShellOwnedMsg(..))
import Dict
import Expect
import Feature.DataLoading as DataLoading
import Feature.Groups as Groups
import Feature.WorkspaceAdmin as Admin
import Feature.WebSocket as WebSocket
import Route
import Helpers
import Http
import Json.Encode as Encode
import Set
import Test exposing (Test, describe, test)
import Types exposing (..)
import Url


suite : Test
suite =
    describe "Workspace UI refinements"
        [ test "workspace filters cannot change globally collapsed groups" <|
            \_ ->
                let
                    collapsed = Groups.update (ToggleWorkspaceGroup "g") base |> Tuple.first
                    loaded = Helpers.applyStoredFiltersIfCurrentWorkspace
                        (Encode.object [ ( "workspaceId", Encode.string "a" ), ( "collapsedNodes", Encode.list Encode.string [ "group-g", "proj-p" ] ) ]) collapsed
                in
                Expect.equal ( Just True, Just True, False )
                    ( Dict.get "g" loaded.groups.collapsedGroups, Dict.get "proj-p" loaded.cards.collapsedNodes, Dict.member "group-g" loaded.cards.collapsedNodes )
        , test "empty project preference defaults checked and persists independently" <|
            \_ ->
                let
                    search = base.search
                    filtered = { base | search = { search | filterShowEmptyProjects = False } }
                    loaded = Helpers.applyStoredFilters (Helpers.encodeFilterState filtered) base
                in
                Expect.equal ( True, False, True )
                    ( base.search.filterShowEmptyProjects, loaded.search.filterShowEmptyProjects, DataLoading.navigationFilterFingerprint base /= DataLoading.navigationFilterFingerprint loaded )
        , test "same catalogue owner survives workspace route admission" <|
            \_ ->
                let
                    switched = { base | page = WorkspacePage "b", selectedWorkspaceId = Just "b", auth = { status = AuthBooting, mode = Just "local" }, sessionContext = Nothing, sessionRequestEpoch = 2 }
                    nextSession = { session | workspace = Just { workspaceId = "b", role = Just "admin", canRead = True, canEdit = True, canAdmin = True } }
                    after = AppShell.handleOwned (SessionContextLoadedMsg 2 (Just "b") (Ok nextSession)) switched |> Tuple.first
                in
                Expect.equal ( base.workspaces, False, base.groups.collapsedGroups ) ( after.workspaces, after.dataLoading.loadingWorkspaces, after.groups.collapsedGroups )
        , test "matching delayed bootstrap catalogue settles during route authorization" <|
            \_ ->
                let
                    loading = base.dataLoading
                    switched = { base | auth = { status = AuthBooting, mode = Just "local" }, sessionContext = Nothing, dataLoading = { loading | loadingWorkspaces = True, activeWorkspaceListLoadToken = Just 7 } }
                    loaded = DataLoading.update (GotWorkspaces 7 (Ok { items = [ workspace "a", workspace "b" ], hasMore = False })) switched |> Tuple.first
                in
                Expect.equal ( False, Nothing, 2 ) ( loaded.dataLoading.loadingWorkspaces, loaded.dataLoading.activeWorkspaceListLoadToken, Dict.size loaded.workspaces )
        , test "deletion confirmation prevents duplicate submission and retains failed state" <|
            \_ ->
                let
                    pending = deletionPending base
                    duplicate = Admin.update PerformWorkspaceDelete pending |> Tuple.first
                    request = pending.groups.workspaceDeletion |> Maybe.withDefault impossible
                    failed = Admin.update (WorkspaceDeleteCompleted request (Err Http.NetworkError)) pending |> Tuple.first
                in
                Expect.equal ( ( True, Just False, True ), base.workspaces )
                    ( ( duplicate.groups.workspaceDeletion == pending.groups.workspaceDeletion, Maybe.map .pending failed.groups.workspaceDeletion, Maybe.andThen .error failed.groups.workspaceDeletion /= Nothing ), failed.workspaces )
        , test "deleting another workspace preserves active selection and fences late catalogue" <|
            \_ ->
                let
                    loading = base.dataLoading
                    pending = deletionPending { base | dataLoading = { loading | loadingWorkspaces = True, activeWorkspaceListLoadToken = Just 7 } }
                    request = pending.groups.workspaceDeletion |> Maybe.withDefault impossible
                    switched = { pending | selectedWorkspaceId = Just "b", page = WorkspacePage "b" }
                    deleted = Admin.update (WorkspaceDeleteCompleted request (Ok ())) switched |> Tuple.first
                    late = DataLoading.update (GotWorkspaces 7 (Ok { items = [ workspace "a", workspace "b" ], hasMore = False })) deleted |> Tuple.first
                in
                Expect.equal ( ( Just "b", WorkspacePage "b", False ), Just [ "b" ], True )
                    ( ( late.selectedWorkspaceId, late.page, Dict.member "a" late.workspaces ), Dict.get "g" late.groups.groupMembers, Set.member "a" late.groups.deletedWorkspaces )
        , test "selected workspace deletion immediately returns home" <|
            \_ ->
                let
                    pending = deletionPending base
                    request = pending.groups.workspaceDeletion |> Maybe.withDefault impossible
                    deleted = Admin.update (WorkspaceDeleteCompleted request (Ok ())) pending |> Tuple.first
                in
                Expect.equal ( Nothing, HomePage, False ) ( deleted.selectedWorkspaceId, deleted.page, Dict.member "a" deleted.workspaces )
        , test "global catalogue response survives a route wait and authority reset retires it" <|
            \_ ->
                let
                    socket = base.webSocket
                    pending = { base | auth = { status = AuthBooting, mode = Just "local" }, sessionContext = Nothing, selectedWorkspaceId = Just "b", sessionRequestEpoch = 99, webSocket = { socket | targetGenerations = Dict.singleton "global|catalogue" 3 } }
                    guard = { scopeKey = "global", targetKey = "catalogue", targetGeneration = 3, sessionEpoch = base.groups.catalogueEpoch, routeWorkspace = Nothing, audienceId = "owner" }
                    response = CanonicalCatalogueFetched guard (Ok { items = [ workspace "c" ], hasMore = False })
                    received = WebSocket.update response pending |> Tuple.first
                    cleared = AppShell.handleOwned AuthUnauthorizedMsg pending |> Tuple.first
                    stale = WebSocket.update response cleared |> Tuple.first
                in
                Expect.equal ( [ "c" ], [], True ) ( Dict.keys received.workspaces, Dict.keys stale.workspaces, cleared.groups.catalogueEpoch > pending.groups.catalogueEpoch )
        , test "a workspace without saved preferences resets empty project visibility" <|
            \_ ->
                let
                    search = base.search
                    prior = { base | search = { search | filterShowEmptyProjects = False } }
                    destination = { protocol = Url.Http, host = "fixture", port_ = Nothing, path = "/workspace/b", query = Nothing, fragment = Nothing }
                    routed = Route.handleUrlChange destination prior |> Tuple.first
                in
                Expect.equal True routed.search.filterShowEmptyProjects
        , test "changed principal rejects a stale deletion completion" <|
            \_ ->
                let
                    pending = deletionPending base
                    request = pending.groups.workspaceDeletion |> Maybe.withDefault impossible
                    principal = session.principal
                    other = { session | principal = { principal | actorId = "other" } }
                    changed = { pending | sessionContext = Just other }
                    ignored = Admin.update (WorkspaceDeleteCompleted request (Ok ())) changed |> Tuple.first
                in
                Expect.equal changed.workspaces ignored.workspaces
        ]


deletionPending : Model -> Model
deletionPending model =
    Admin.update (ConfirmWorkspaceDelete "a") model |> Tuple.first |> Admin.update PerformWorkspaceDelete |> Tuple.first


impossible : WorkspaceDeletion
impossible =
    { workspaceId = "missing", token = -1, sessionKey = "missing", pending = False, error = Nothing }


base : Model
base =
    let
        initial = AppShell.initModel Nothing
            { protocol = Url.Http, host = "fixture", port_ = Nothing, path = "/workspace/a", query = Nothing, fragment = Nothing }
            (WorkspacePage "a")
            { apiUrl = "http://fixture", wsUrl = "ws://fixture", sessionId = "test", runtimeMode = "local", authTokenStorageKey = "test", authTokenPresent = False, loginUrl = Nothing, logoutUrl = Nothing }
            Nothing { tab = ProjectsTab, focus = Nothing, observationId = Nothing }
            |> AppShell.finalizeInit (WorkspacePage "a")
        groups = initial.groups
        loading = initial.dataLoading
    in
    { initial | auth = { status = AuthReady, mode = Just "local" }, sessionContext = Just session
        , workspaces = Dict.fromList [ ( "a", workspace "a" ), ( "b", workspace "b" ) ]
        , groups = { groups | catalogueOwner = Just session, groupMembers = Dict.singleton "g" [ "a", "b" ], collapsedGroups = Dict.empty }
        , dataLoading = { loading | loadingWorkspaces = False, activeWorkspaceListLoadToken = Nothing }
    }


session : Api.SessionContext
session =
    { authMode = "local", principal = { actorType = "user", actorId = "owner", actorLabel = "Owner", authority = "local", grantUserId = Nothing }
    , globalPermissions = { createWorkspace = True, superadmin = True }
    , workspace = Just { workspaceId = "a", role = Just "admin", canRead = True, canEdit = True, canAdmin = True }
    }


workspace : String -> Api.Workspace
workspace id =
    { id = id, name = id, workspaceType = Api.Repository, ghOwner = Nothing, ghRepo = Nothing, createdAt = "now", updatedAt = "now" }
