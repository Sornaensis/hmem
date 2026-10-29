module AuditFilterTest exposing (suite)

import Api
import AppShell exposing (AppShellOwnedMsg(..))
import Dict
import Expect
import Feature.AuditLog as Audit
import Feature.AuditRequest as Request
import Feature.Focus as Focus
import Http
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Types exposing (..)
import Url


suite : Test
suite =
    describe "Audit filter lifetimes"
        [ test "every select and valid date change starts a clean first page automatically" <|
            \_ ->
                [ ( "entityType", "agent_session" ), ( "action", "create" ), ( "since", "2026-10-01" ), ( "until", "2026-10-29" ) ]
                    |> List.map (\( field, value ) ->
                        let
                            before = populated
                            after = step (SetAuditFilter field value) before
                        in
                        after.auditLog.loading && after.auditLog.activeRequest /= Nothing
                            && after.auditLog.filters.offset == Nothing && after.auditLog.entryBaseOffset == 0
                            && after.auditLog.entries == [] && not after.auditLog.hasMore
                            && after.auditLog.expandedEntries == Dict.empty && after.auditLog.error == Nothing
                    )
                    |> Expect.equal [ True, True, True, True ]
        , test "clear removes every user constraint and selection but keeps mandatory scope and page size" <|
            \_ ->
                let
                    before = populated
                    state = before.auditLog
                    filters = state.filters
                    after = step ClearAuditFilters { before | auditLog = { state | filters = { filters | entityType = Just "task", entityId = Just "entity", action = Just "update", since = Just "2026-01-01", until = Just "2026-10-29" } } }
                in
                Expect.equal { workspace = Just "ws", limit = Just 50, constraints = False, offset = Nothing, loading = True, entries = [] }
                    { workspace = after.auditLog.filters.workspaceId, limit = after.auditLog.filters.limit, constraints = Request.hasUserConstraints after.auditLog.filters, offset = after.auditLog.filters.offset, loading = after.auditLog.loading, entries = after.auditLog.entries }
        , test "default, paginated and workspace-only queries have no Clear Filters or Apply control" <|
            \_ ->
                let
                    view = Audit.viewWorkspaceAuditPanel "ws" populated |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> view |> Query.hasNot [ Selector.text "Clear Filters" ]
                    , \_ -> view |> Query.hasNot [ Selector.text "Apply" ]
                    , \_ -> view |> Query.has [ Selector.text "Agent Sessions" ]
                    , \_ -> view |> Query.has [ Selector.text "Session task connections" ]
                    , \_ -> Audit.viewWorkspaceAuditPanel "ws" (step (SetAuditFilter "action" "create") base) |> Query.fromHtml |> Query.has [ Selector.text "Clear Filters" ]
                    ] ()
        , test "removing the final constraint immediately fetches the default query and hides clear" <|
            \_ ->
                let
                    after = base |> step (SetAuditFilter "action" "create") |> step (SetAuditFilter "action" "")
                in
                Expect.equal { loading = True, constraints = False, action = Nothing, next = 3 }
                    { loading = after.auditLog.loading, constraints = Request.hasUserConstraints after.auditLog.filters, action = after.auditLog.filters.action, next = after.auditLog.nextRequestId }
        , test "incomplete and invalid dates do not submit and the next valid value wins" <|
            \_ ->
                let
                    invalid = base |> step (SetAuditFilter "since" "2026-0") |> step (SetAuditFilter "since" "2026-02-29")
                    valid = invalid |> step (SetAuditFilter "since" "2024-02-29")
                in
                Expect.equal { next = 1, invalid = Nothing, valid = Just "2024-02-29", loading = True }
                    { next = invalid.auditLog.nextRequestId, invalid = invalid.auditLog.filters.since, valid = valid.auditLog.filters.since, loading = valid.auditLog.loading }
        , test "A to B to A rejects earlier identical-query successes and errors" <|
            \_ ->
                let
                    first = step (SetAuditFilter "entityType" "task") base
                    latest = first |> step (SetAuditFilter "entityType" "project") |> step (SetAuditFilter "entityType" "task")
                    staleSuccess = respond first (Ok { items = [ { entry | id = "stale" } ], hasMore = True }) latest
                    staleError = respond first (Err Http.NetworkError) staleSuccess
                    accepted = respond latest (Ok { items = [ entry ], hasMore = False }) staleError
                in
                Expect.equal { stale = latest.auditLog, entries = [ "event" ], loading = False, error = Nothing }
                    { stale = staleError.auditLog, entries = List.map .id accepted.auditLog.entries, loading = accepted.auditLog.loading, error = accepted.auditLog.error }
        , test "a filter change retires load-more and never mixes old and new pages" <|
            \_ ->
                let
                    more = step LoadMoreAuditLog populated
                    changed = step (SetAuditFilter "entityType" "agent_session") more
                    late = respond more (Ok { items = [ { entry | id = "old-page" } ], hasMore = True }) changed
                    accepted = respond changed (Ok { items = [ { entry | id = "new-page" } ], hasMore = True }) late
                    next = step LoadMoreAuditLog accepted
                in
                Expect.equal { late = [], entries = [ "new-page" ], offset = Just 1, base = 0 }
                    { late = late.auditLog.entries, entries = next.auditLog.entries |> List.map .id, offset = next.auditLog.filters.offset, base = next.auditLog.entryBaseOffset }
        , test "current errors render as errors and a later query clears the error and old selection" <|
            \_ ->
                let
                    pending = step (SetAuditFilter "entityType" "task") populated
                    failed = respond pending (Err Http.NetworkError) pending
                    next = step (SetAuditFilter "action" "create") failed
                in
                Expect.all
                    [ \_ -> Audit.viewWorkspaceAuditPanel "ws" failed |> Query.fromHtml |> Query.has [ Selector.text "Failed to load audit entries for these filters." ]
                    , \_ -> Expect.equal ( Nothing, True, [] ) ( next.auditLog.error, next.auditLog.loading, next.auditLog.entries )
                    ] ()
        , test "workspace, session epoch and authenticated audience independently fence a pending response" <|
            \_ ->
                let
                    pending = step (SetAuditFilter "action" "create") base
                    changedAudience = Maybe.map (\currentSession -> let principal = currentSession.principal in { currentSession | principal = { principal | actorId = "other" } }) pending.sessionContext
                    contexts =
                        [ { pending | selectedWorkspaceId = Just "other" }
                        , { pending | sessionRequestEpoch = pending.sessionRequestEpoch + 1 }
                        , { pending | sessionContext = changedAudience }
                        , { pending | sessionContext = Nothing }
                        , { pending | auth = { status = AuthRequired, mode = Just "local" } }
                        ]
                in
                contexts |> List.map (\context -> (respond pending (Ok { items = [ entry ], hasMore = False }) context).auditLog.entries) |> Expect.equal [ [], [], [], [], [] ]
        , test "authority replacement starts its audit read after retirement under the new epoch" <|
            \_ ->
                let
                    pending = step (SetAuditFilter "action" "create") base
                    changed = { session | principal = { actorType = "user", actorId = "other", actorLabel = "Other", authority = "local_superadmin", grantUserId = Nothing } }
                    admitted = AppShell.prepareOwned (SessionContextLoadedMsg pending.sessionRequestEpoch (Just "ws") (Ok changed)) pending |> Tuple.first
                    current = admitted.auditLog.activeRequest |> Maybe.map (\request -> request.sessionEpoch == admitted.sessionRequestEpoch && request.requestId >= pending.auditLog.nextRequestId) |> Maybe.withDefault False
                    late = respond pending (Ok { items = [ entry ], hasMore = False }) admitted
                in
                Expect.equal ( True, True, [] ) ( current, admitted.auditLog.loading, late.auditLog.entries )
        , test "return restores the originating page, selected event and filters under a fresh request" <|
            \_ ->
                let
                    filtered = step (SetAuditFilter "action" "create") base
                    state = filtered.auditLog
                    filters = state.filters
                    returnFilters = { filters | offset = Just 100 }
                    focus = filtered.focus
                    source = { filtered | activeTab = ProjectsTab, focus = { focus | returnContext = Just (Focus.auditReturnContext ReturnFromWorkspaceAudit "ws" returnFilters True entry ( "task", "task" )) } }
                    returned = Focus.update ReturnToFocusSource source |> Tuple.first
                    accepted = respond returned (Ok { items = [ entry ], hasMore = True }) returned
                    more = step LoadMoreAuditLog accepted
                in
                Expect.equal { tab = AuditTab, filters = returnFilters, expanded = Just True, base = 100, offset = Just 101, fresh = True }
                    { tab = returned.activeTab, filters = returned.auditLog.filters, expanded = Dict.get "event" returned.auditLog.expandedEntries, base = accepted.auditLog.entryBaseOffset, offset = more.auditLog.filters.offset, fresh = returned.auditLog.nextRequestId > filtered.auditLog.nextRequestId }
        ]


step : Msg -> Model -> Model
step message model = Audit.update message model |> Tuple.first


respond : Model -> Result Http.Error (Api.PaginatedResult Api.AuditLogEntry) -> Model -> Model
respond source result target =
    case source.auditLog.activeRequest of
        Just request -> step (GotAuditLog request source.auditLog.filters result) target
        Nothing -> target


populated : Model
populated =
    let
        state = base.auditLog
        filters = state.filters
    in
    { base | auditLog = { state | entries = [ entry ], filters = { filters | offset = Just 100 }, entryBaseOffset = 100, hasMore = True, expandedEntries = Dict.singleton "event" True, error = Just "Previous failure" } }


base : Model
base =
    let
        initial = AppShell.initModel Nothing { protocol = Url.Https, host = "app.example", port_ = Nothing, path = "/workspace/ws", query = Nothing, fragment = Nothing } (WorkspacePage "ws") { apiUrl = "https://api.example", wsUrl = "wss://api.example", sessionId = "session", runtimeMode = "local", authTokenStorageKey = "token", authTokenPresent = False, loginUrl = Nothing, logoutUrl = Nothing } Nothing { tab = AuditTab, focus = Nothing, observationId = Nothing }
        state = initial.auditLog
        filters = state.filters
    in
    { initial | selectedWorkspaceId = Just "ws", activeTab = AuditTab, auth = { status = AuthReady, mode = Just "local" }, sessionContext = Just session, auditLog = { state | filters = { filters | workspaceId = Just "ws" } } }


session : Api.SessionContext
session = { authMode = "local", effectiveActor = Nothing, principal = { actorType = "user", actorId = "local", actorLabel = "Local", authority = "local_superadmin", grantUserId = Nothing }, globalPermissions = { createWorkspace = True, superadmin = True }, workspace = Nothing }


entry : Api.AuditLogEntry
entry = { id = "event", entityType = "task", entityId = "task", workspaceId = Just "ws", action = Api.AuditUpdate, oldValues = Nothing, newValues = Nothing, changedAt = "2026-10-29T12:00:00Z", actorType = Nothing, actorId = Nothing, actorLabel = Nothing, botIdentity = Nothing, requestId = Nothing }
