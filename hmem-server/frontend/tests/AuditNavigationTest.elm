module AuditNavigationTest exposing (suite)

import Api
import AppShell
import AuditNavigation as Navigation
import Dict
import Expect
import Feature.AuditLog as Audit
import Feature.AuditRequest as Request
import Feature.Focus as Focus
import Json.Encode as Encode
import Route
import Test exposing (..)
import Types exposing (..)
import Url


suite : Test
suite = describe "Typed Audit destinations and return ownership"
    [ test "supported direct targets resolve to their own route kinds" <| \_ ->
        [ ( "project", AuditProject ), ( "task", AuditTask ), ( "observation", AuditObservation ), ( "agent_session", AuditSession ), ( "workspace", AuditWorkspace ) ]
            |> List.map (\( kind, expected ) -> Navigation.resolve { entry | entityType = kind } |> Maybe.map .kind |> (==) (Just expected))
            |> Expect.equal [ True, True, True, True, True ]
    , test "session and observation destinations contain selected URL parameters without hierarchy focus" <| \_ ->
        [ Navigation.destinationUrl { workspaceId = workspace, kind = AuditSession, id = target }
        , Navigation.destinationUrl { workspaceId = workspace, kind = AuditObservation, id = target }
        ] |> Expect.equal [ "/workspace/" ++ workspace ++ "#tab=sessions&session=" ++ target, "/workspace/" ++ workspace ++ "#tab=observations&observation=" ++ target ]
    , test "connection and dependency targets use old snapshot fallback and expose the separate task" <| \_ ->
        let
            values = Just (Encode.object [ ( "session_id", Encode.string target ), ( "task_id", Encode.string related ), ( "depends_on_id", Encode.string target ), ( "workspace_id", Encode.string workspace ) ])
            connection = { entry | entityType = "task_session", entityId = target ++ ":" ++ related, workspaceId = Nothing, oldValues = values, newValues = Just (Encode.object []) }
            dependency = { connection | entityType = "task_dependency" }
        in
        Expect.equal
            { session = Just target, owner = Just related, connectionTask = [ related ], dependencyTask = [ target ] }
            { session = Navigation.resolve connection |> Maybe.map .id, owner = Navigation.resolve dependency |> Maybe.map .id
            , connectionTask = Navigation.related connection |> List.map (Tuple.second >> .id), dependencyTask = Navigation.related dependency |> List.map (Tuple.second >> .id) }
    , test "new valid relation values win and malformed new values can fall back to recorded old UUIDs" <| \_ ->
        let old = Just (Encode.object [ ( "task_id", Encode.string target ) ])
            source value = { entry | entityType = "task_dependency", oldValues = old, newValues = Just (Encode.object [ ( "task_id", Encode.string value ) ]) }
        in
        [ Navigation.resolve (source related), Navigation.resolve (source "malformed") ] |> List.map (Maybe.map .id) |> Expect.equal [ Just related, Just target ]
    , test "all unsupported admin and legacy types have no guessed navigation destination" <| \_ ->
        [ "workspace_group", "workspace_group_member", "workspace_member", "user", "token", "personal_access_token", "memory", "category", "memory_link", "project_memory_link", "task_memory_link", "unknown" ]
            |> List.map (\kind -> Navigation.resolve { entry | entityType = kind }) |> List.all ((==) Nothing) |> Expect.equal True
    , test "missing, malformed, composite and whitespace IDs never install a destination" <| \_ ->
        let malformed = [ "", "not-a-uuid", target ++ ":" ++ related, " " ++ target, "00000000-0000-4000-8000-00000000000g" ]
        in malformed |> List.concatMap (\id -> [ "project", "task", "observation", "agent_session", "workspace" ] |> List.map (\kind -> Navigation.resolve { entry | entityId = id, entityType = kind })) |> List.all ((==) Nothing) |> Expect.equal True
    , test "an unavailable workspace never falls back to the currently selected workspace" <| \_ ->
        Navigation.resolve { entry | workspaceId = Nothing } |> Expect.equal Nothing
    , test "workspace records derive their context from the workspace entity ID" <| \_ ->
        Navigation.resolve { entry | entityType = "workspace", workspaceId = Just related } |> Maybe.map .workspaceId |> Expect.equal (Just target)
    , test "only the latest viewport activation can navigate within one query" <| \_ ->
        let first = step (NavigateToAuditEntity entry) base
            secondEntry = { entry | entityId = related }
            second = step (NavigateToAuditEntity secondEntry) first
            stale = viewport first entry second
            latest = viewport second secondEntry stale
        in Expect.equal { stale = Nothing, latest = Just ( "task", related ), scroll = Just 231 }
            { stale = stale.focus.focusedEntity, latest = latest.focus.focusedEntity, scroll = latest.focus.returnContext |> Maybe.andThen .auditScrollY }
    , test "selection, filter, workspace and principal changes retire pending activation" <| \_ ->
        let pending = step (NavigateToAuditEntity entry) base
            changedPrincipal = Maybe.map (\currentSession -> let principal = currentSession.principal in {currentSession | principal = {principal | actorId = "other"}}) pending.sessionContext
            retired = [ step (ToggleAuditExpand entry.id) pending, step ClearAuditFilters pending, {pending | selectedWorkspaceId = Just related}, {pending | sessionContext = changedPrincipal}, {pending | activeTab = ProjectsTab}, {pending | url = {url | fragment = Just "tab=projects"}} ]
        in retired |> List.map (viewport pending entry >> .focus >> .focusedEntity) |> Expect.equal [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
    , test "Observation, Session and Workspace returns survive their matching URLs and reject another selection" <| \_ ->
        [ ( "observation", "tab=observations&observation=" ), ( "agent_session", "tab=sessions&session=" ), ( "workspace", "tab=projects" ) ]
            |> List.map (\( kind, prefix ) ->
                let context = Focus.auditReturnContext ReturnFromWorkspaceAudit workspace base.auditLog.filters True entry ( kind, if kind == "workspace" then workspace else target )
                    focus = base.focus
                    owner = {focus | returnContext = Just context}
                    fragment = if kind == "workspace" then prefix else prefix ++ target
                    exact = Focus.returnContextForUrl {url | fragment = Just fragment} owner /= Nothing
                    other = Focus.returnContextForUrl {url | fragment = Just (if kind == "workspace" then "tab=observations" else prefix ++ related)} owner == Nothing
                in exact && other) |> Expect.equal [ True, True, True ]
    , test "global return restores only metadata belonging to the admitted principal" <| \_ ->
        let state = base.auditLog
            filters = state.filters
            globalFilters = {filters | workspaceId = Nothing, action = Just "create", offset = Just 50}
            original = Focus.auditReturnContext ReturnFromGlobalAudit workspace globalFilters True entry ( "task", target )
            context = {original | auditScrollY = Just 401, auditPrincipal = Maybe.map .principal base.sessionContext, auditAuthMode = Just "local"}
            focus = base.focus
            source = {base | focus = {focus | returnContext = Just context}}
            routed = Route.handleUrlChange {url | path = "/audit", fragment = Nothing} source |> Tuple.first
            admitted = Focus.restoreAuditReturnAuthority routed session {routed | auditLog = Audit.init}
            changed = {session | principal = {actorType="user",actorId="other",actorLabel="Other",authority="local_superadmin",grantUserId=Nothing}}
            rejected = Focus.restoreAuditReturnAuthority routed changed routed
            final = AppShell.prepareOwned (AppShell.SessionContextLoadedMsg routed.sessionRequestEpoch Nothing (Ok changed)) routed |> Tuple.first
        in Expect.equal { filters = globalFilters, scroll = Just 401, expanded = Just True, rejected = True, fetch = False }
            { filters = admitted.auditLog.filters, scroll = admitted.auditLog.restoreScrollY, expanded = Dict.get entry.id admitted.auditLog.expandedEntries
            , rejected = rejected.auditLog.restoreScrollY == Nothing && rejected.auditLog.pendingReturn == Nothing && rejected.auditLog.expandedEntries == Dict.empty && not (Request.hasUserConstraints rejected.auditLog.filters)
            , fetch = Request.hasUserConstraints final.auditLog.filters }
    , test "changing or clearing a query removes an earlier return viewport and selection" <| \_ ->
        let state = base.auditLog
            context = Focus.auditReturnContext ReturnFromWorkspaceAudit workspace state.filters True entry ( "task", target )
            returning = {base | auditLog = {state | pendingReturn = Just context, restoreScrollY = Just 631}}
        in [ step (SetAuditFilter "action" "create") returning, step ClearAuditFilters returning ]
            |> List.map (\model -> (model.auditLog.restoreScrollY,model.auditLog.pendingReturn)) |> Expect.equal [ (Nothing,Nothing),(Nothing,Nothing) ]
    ]


step : Msg -> Model -> Model
step message model = Audit.update message model |> Tuple.first


viewport : Model -> Api.AuditLogEntry -> Model -> Model
viewport source audit targetModel =
    case (source.auditLog.activeNavigationId, Navigation.resolve audit) of
        (Just activation, Just destination) -> step (GotAuditNavigationViewport {sessionEpoch=source.sessionRequestEpoch,workspaceId=source.selectedWorkspaceId,page=source.page,tab=source.activeTab,route=Url.toString source.url,queryId=source.auditLog.nextRequestId,navigationId=activation,principal=Maybe.map .principal source.sessionContext,authMode=Maybe.map .authMode source.sessionContext} audit destination (Ok 231)) targetModel
        _ -> targetModel


workspace : String
workspace = "10000000-0000-4000-8000-000000000001"

target : String
target = "20000000-0000-4000-8000-000000000001"

related : String
related = "30000000-0000-4000-8000-000000000001"

url : Url.Url
url = {protocol=Url.Https,host="app.example",port_=Nothing,path="/workspace/" ++ workspace,query=Nothing,fragment=Just "tab=audit"}

session : Api.SessionContext
session = {authMode="local",effectiveActor=Nothing,principal={actorType="user",actorId="local",actorLabel="Local",authority="local_superadmin",grantUserId=Nothing},globalPermissions={createWorkspace=True,superadmin=True},workspace=Nothing}

base : Model
base =
    let initial = AppShell.initModel Nothing url (WorkspacePage workspace) {apiUrl="https://api.example",wsUrl="wss://api.example",sessionId="session",runtimeMode="local",authTokenStorageKey="token",authTokenPresent=False,loginUrl=Nothing,logoutUrl=Nothing} Nothing {tab=AuditTab,focus=Nothing,observationId=Nothing}
        state = initial.auditLog
        filters = state.filters
    in {initial | selectedWorkspaceId=Just workspace,auth={status=AuthReady,mode=Just "local"},sessionContext=Just session,auditLog={state | filters={filters | workspaceId=Just workspace},entries=[entry]}}

entry : Api.AuditLogEntry
entry = {id="event",entityType="task",entityId=target,workspaceId=Just workspace,action=Api.AuditUpdate,oldValues=Nothing,newValues=Nothing,changedAt="2026-10-29T12:00:00Z",actorType=Nothing,actorId=Nothing,actorLabel=Nothing,botIdentity=Nothing,requestId=Nothing}
