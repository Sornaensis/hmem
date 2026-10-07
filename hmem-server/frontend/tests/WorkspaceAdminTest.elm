module WorkspaceAdminTest exposing (suite)

import Api
import AppShell exposing (AppShellOwnedMsg(..))
import Dict
import Expect
import Feature.WorkspaceAdmin as Admin
import Helpers
import Html.Attributes exposing (disabled)
import Http
import Page.Workspace
import Permissions
import Route
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Types exposing (..)
import Url

workspace : Api.Workspace
workspace =
    { id = "a", name = "Workspace", workspaceType = Api.Repository, ghOwner = Nothing, ghRepo = Nothing, createdAt = "now", updatedAt = "now" }

session : String -> Bool -> String -> Api.SessionContext
session role superadmin authority =
    { authMode = "local"
    , principal = { actorType = "user", actorId = "actor", actorLabel = "Owner", authority = authority, grantUserId = Nothing }
    , globalPermissions = { createWorkspace = False, superadmin = superadmin }
    , workspace = Just { workspaceId = "a", role = Just role, canRead = True, canEdit = role /= "read", canAdmin = List.member role [ "admin", "owner" ] }
    }

base : Model
base =
    let
        initial = AppShell.initModel Nothing
            { protocol = Url.Https, host = "app.example", port_ = Nothing, path = "/workspace/a", query = Nothing, fragment = Just "tab=administration" }
            (WorkspacePage "a")
            { apiUrl = "https://api.example", wsUrl = "wss://api.example/ws", sessionId = "test", runtimeMode = "local", authTokenStorageKey = "test", authTokenPresent = False, loginUrl = Nothing, logoutUrl = Nothing }
            Nothing { tab = AdministrationTab, focus = Nothing, observationId = Nothing }
    in
    Admin.reconcileAuthority { initial | auth = { status = AuthReady, mode = Just "local" }, sessionContext = Just (session "owner" False "local")
        , selectedWorkspaceId = Just "a", workspaces = Dict.singleton "a" workspace }

member : Api.WorkspaceMembership
member =
    { workspaceId = "a", userId = "user-a", role = "edit", grantedBy = Nothing, createdAt = "now", updatedAt = "now" }

guard : Model -> MembershipRequestGuard
guard model =
    model.workspaceAdmin.activeListRequest |> Maybe.withDefault { workspaceId = "", sessionEpoch = -1, sessionKey = "", token = -1 }

mutationGuard : Model -> MembershipRequestGuard
mutationGuard model =
    model.workspaceAdmin.activeMutation |> Maybe.map .guard |> Maybe.withDefault { workspaceId = "", sessionEpoch = -1, sessionKey = "", token = -1 }

read : Model -> Model
read model =
    Admin.ensureMemberships True "a" model |> Tuple.first

accept : Model -> Model
accept model =
    Admin.update (GotWorkspaceMemberships (guard model) (Ok { items = [ member ], hasMore = False })) model |> Tuple.first

draft : Model
draft =
    accept (read base)
        |> Admin.update (UpdateMembershipUserId "user-b") |> Tuple.first
        |> Admin.update (UpdateMembershipRole "admin") |> Tuple.first

submit : Model -> Model
submit model =
    Admin.update (SubmitWorkspaceMembership "a") model |> Tuple.first

saved : Model -> Model
saved model =
    Admin.update (WorkspaceMembershipSaved (mutationGuard model) (Ok { member | userId = "user-b", role = "admin" })) model |> Tuple.first

admit : Api.SessionContext -> Model -> Model
admit current model =
    AppShell.handleOwned (SessionContextLoadedMsg model.sessionRequestEpoch (Just "a") (Ok current)) model |> Tuple.first

suite : Test
suite =
    describe "dedicated workspace administration"
        [ test "administration fragments survive strict versioned Observation URL context and real route entry" <| \_ ->
            let
                currentUrl = base.url
                url = { currentUrl | fragment = Just (Helpers.buildFragment AdministrationTab Nothing Nothing ++ "&ov=1&oq=" ++ Helpers.encodeObservationQuery Helpers.defaultObservationQuery) }
                routed = Route.handleUrlChange url { base | activeTab = ProjectsTab } |> Tuple.first
            in
            Expect.equal ( AdministrationTab, Nothing, ( AdministrationTab, True ) )
                ( (Helpers.parseFragment url.fragment).tab, (Helpers.observationUrlContext url).notice, ( routed.activeTab, routed.workspaceAdmin.activeListRequest /= Nothing ) )
        , test "explicit eligibility preserves reader edit unknown and implicit local-superadmin suppression" <| \_ ->
            let
                permitted current = Permissions.canViewWorkspaceAdministration { base | sessionContext = Just current }
            in
            Expect.equal [ True, True, True, False, False, False, False ]
                [ permitted (session "owner" False "local"), permitted (session "admin" False "local"), permitted (session "read" True "grant_user")
                , permitted (session "read" False "local"), permitted (session "edit" False "local"), permitted (session "owner" True "local_superadmin")
                , Permissions.canViewWorkspaceAdministration { base | sessionContext = Nothing } ]
        , test "Admin rendering bypasses global search error/loading/results and project loading" <| \_ ->
            let
                search = base.search
                loading = base.dataLoading
                model = { base | search = { search | isSearching = True, searchError = Just "Other search failed", unifiedResults = Just { projects = [], tasks = [], observations = [] } }
                    , dataLoading = { loading | loadingWorkspaceData = True } }
            in
            Page.Workspace.viewWorkspacePage "a" model |> Query.fromHtml
                |> Query.find [ Selector.id "workspace-membership-user" ] |> Query.has [ Selector.tag "input" ]
        , test "working tabs contain neither membership controls nor signed-in summary" <| \_ ->
            Page.Workspace.viewWorkspacePage "a" { base | activeTab = ObservationsTab } |> Query.fromHtml
                |> Query.findAll [ Selector.class "workspace-admin-context" ] |> Query.count (Expect.equal 0)
        , test "list failure preserves cache and Retry owns fresh request rejecting old reply" <| \_ ->
            let
                first = read (accept (read base))
                failed = Admin.update (GotWorkspaceMemberships (guard first) (Err Http.Timeout)) first |> Tuple.first
                retried = Admin.update (RetryWorkspaceMemberships "a") failed |> Tuple.first
                stale = Admin.update (GotWorkspaceMemberships (guard first) (Ok { items = [], hasMore = False })) retried |> Tuple.first
            in
            Expect.equal ( True, Just [ member ], ( True, retried.workspaceAdmin ) )
                ( Dict.member "a" failed.workspaceAdmin.membershipErrors, Dict.get "a" retried.workspaceAdmin.memberships, ( (guard retried).token > (guard first).token, stale.workspaceAdmin ) )
        , test "foreign workspace submission removal and list completion do not acquire ownership" <| \_ ->
            let
                foreignSubmit = Admin.update (SubmitWorkspaceMembership "b") draft |> Tuple.first
                foreignRemove = Admin.update (RemoveWorkspaceMembership "b" "user-a") draft |> Tuple.first
                active = read base
                foreign = Admin.update (GotWorkspaceMemberships { workspaceId = "b", sessionEpoch = (guard active).sessionEpoch, sessionKey = (guard active).sessionKey, token = (guard active).token } (Ok { items = [ member ], hasMore = False })) active |> Tuple.first
            in
            Expect.equal ( draft.workspaceAdmin, draft.workspaceAdmin, active.workspaceAdmin )
                ( foreignSubmit.workspaceAdmin, foreignRemove.workspaceAdmin, foreign.workspaceAdmin )
        , test "held reads cannot revive actor or permission retired state" <| \_ ->
            let
                active = read base
                actorSession = session "owner" False "local"
                principal = actorSession.principal
                retiredActor = admit { actorSession | principal = { principal | actorId = "other" } } active
                retiredRole = admit (session "read" False "local") active
                stale model = Admin.update (GotWorkspaceMemberships (guard active) (Ok { items = [ member ], hasMore = False })) model |> Tuple.first
            in
            Expect.equal ( retiredActor.workspaceAdmin, retiredRole.workspaceAdmin, "" )
                ( (stale retiredActor).workspaceAdmin, (stale retiredRole).workspaceAdmin, retiredRole.workspaceAdmin.membershipUserId )
        , test "held save and delete completions after actor workspace and permission retirement are inert" <| \_ ->
            let
                saving = submit draft
                removing = Admin.update (RemoveWorkspaceMembership "a" "user-a") draft |> Tuple.first
                actorSession = session "owner" False "local"
                principal = actorSession.principal
                replacements model = [ admit { actorSession | principal = { principal | actorId = "other" } } model, admit (session "read" False "local") model, { model | selectedWorkspaceId = Just "b", page = WorkspacePage "b" } ]
                savedInert model = (Admin.update (WorkspaceMembershipSaved (mutationGuard saving) (Ok { member | userId = "user-b" })) model |> Tuple.first).workspaceAdmin == model.workspaceAdmin
                removedInert model = (Admin.update (WorkspaceMembershipDeleted (mutationGuard removing) "user-a" (Ok ())) model |> Tuple.first).workspaceAdmin == model.workspaceAdmin
            in
            Expect.equal True (List.all savedInert (replacements saving) && List.all removedInert (replacements removing))
        , test "failed and duplicate writes retain form and cache while matching request stays owned" <| \_ ->
            let
                pending = submit draft
                duplicate = submit pending
                failed = Admin.update (WorkspaceMembershipSaved (mutationGuard pending) (Err Http.Timeout)) pending |> Tuple.first
            in
            Expect.equal ( ( pending.workspaceAdmin, "user-b" ), ( "admin", Just [ member ] ), True )
                ( ( duplicate.workspaceAdmin, failed.workspaceAdmin.membershipUserId ), ( failed.workspaceAdmin.membershipRole, Dict.get "a" failed.workspaceAdmin.memberships ), failed.workspaceAdmin.mutationError /= Nothing )
        , test "owned authorization recheck blocks another mutation until actual unchanged-authority admission" <| \_ ->
            let
                checking = saved (submit draft)
                blocked = submit checking
                settled = admit (session "owner" False "local") checking
            in
            Expect.equal ( True, checking.workspaceAdmin, ( Nothing, Just 2 ) )
                ( checking.workspaceAdmin.authorizationPending /= Nothing, blocked.workspaceAdmin, ( settled.workspaceAdmin.authorizationPending, Dict.get "a" settled.workspaceAdmin.memberships |> Maybe.map List.length ) )
        , test "failed owned session recheck clears authority and offers scope-owned fresh Retry" <| \_ ->
            let
                checking = saved (submit draft)
                failed = AppShell.handleOwned (SessionContextLoadedMsg checking.sessionRequestEpoch (Just "a") (Err Http.Timeout)) checking |> Tuple.first
                retried = AppShell.handleOwned RetryMembershipAuthorizationMsg failed |> Tuple.first
                settled = admit (session "owner" False "local") retried
            in
            Expect.equal ( ( Just "a", Nothing ), ( AuthBooting, True ), ( Nothing, True ) )
                ( ( failed.workspaceAdmin.authorizationFailure, failed.sessionContext ), ( retried.auth.status, retried.sessionRequestEpoch > failed.sessionRequestEpoch ), ( settled.workspaceAdmin.authorizationFailure, settled.workspaceAdmin.activeListRequest /= Nothing ) )
        , test "canonical guarded membership reread replaces cache and retires older local read without losing a mutation" <| \_ ->
            let
                active = read base
                canonical = Admin.acceptCanonicalMemberships "a" [ member ] active
                old = Admin.update (GotWorkspaceMemberships (guard active) (Ok { items = [], hasMore = False })) canonical |> Tuple.first
                mutating = submit draft
                canonicalDuringWrite = Admin.acceptCanonicalMemberships "a" [ member ] mutating
            in
            Expect.equal ( Just [ member ], Nothing, ( canonical.workspaceAdmin, mutating.workspaceAdmin.activeMutation ) )
                ( Dict.get "a" canonical.workspaceAdmin.memberships, canonical.workspaceAdmin.activeListRequest, ( old.workspaceAdmin, canonicalDuringWrite.workspaceAdmin.activeMutation ) )
        ]

