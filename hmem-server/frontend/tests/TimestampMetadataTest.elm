module TimestampMetadataTest exposing (suite)

import Api
import AppShell
import Array
import Dict
import Expect
import Feature.AuditLog as Audit
import Feature.Cards as Cards
import Feature.Observation as Observation
import Feature.TimestampDisplay as Presentation
import Feature.WorkspaceAdmin as Admin
import Feature.Groups as Groups
import Permissions
import Json.Encode as Encode
import Main
import ObservationTree
import ObservationViewport
import Page.Workspace
import Set
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Event as Event
import Test.Html.Selector as Selector
import TimestampDisplay as Display
import Types exposing (..)
import Url


raw : String
raw = "2026-01-01T00:00:00.123456Z"

later : String
later = "2026-01-01T00:01:00+00:00"

latest : String
latest = "2026-01-01T00:02:00Z"

local : String
local = "2025-12-31 16:00"

display : Display.State
display =
    let state = Display.empty in
    { state | cache = Dict.fromList
        [ ( raw, Just { text = local, day = "2025-12-31" } )
        , ( later, Just { text = "2025-12-31 16:01", day = "2025-12-31" } )
        , ( latest, Just { text = "2025-12-31 16:02", day = "2025-12-31" } )
        , ( "bad", Nothing )
        ] }

workspace : Api.Workspace
workspace =
    { id = "w", name = "Workspace", workspaceType = Api.Repository, ghOwner = Nothing, ghRepo = Nothing, createdAt = raw, updatedAt = later }

base : Model
base =
    let
        initial = AppShell.initModel Nothing
            { protocol = Url.Https, host = "app.example", port_ = Nothing, path = "/workspace/w", query = Nothing, fragment = Nothing }
            (WorkspacePage "w")
            { apiUrl = "", wsUrl = "", sessionId = "test", runtimeMode = "local", authTokenStorageKey = "test", authTokenPresent = False, loginUrl = Nothing, logoutUrl = Nothing }
            Nothing { tab = ProjectsTab, focus = Nothing, observationId = Nothing }
        loading = initial.dataLoading
    in
    { initial | selectedWorkspaceId = Just "w", workspaces = Dict.singleton "w" workspace
        , auth = { status = AuthReady, mode = Just "local" }, timestamps = display
        , dataLoading = { loading | loadingWorkspaceData = False }
        , sessionContext = Just
            { authMode = "local", effectiveActor = Nothing
            , principal = { actorType = "user", actorId = "user", actorLabel = "Owner", authority = "local", grantUserId = Nothing }
            , globalPermissions = { createWorkspace = True, superadmin = True }
            , workspace = Just { workspaceId = "w", role = Just "owner", canRead = True, canEdit = True, canAdmin = True }
            }
    }

task : String -> Api.Task
task id =
    { id = id, workspaceId = "w", projectId = Nothing, parentId = Nothing, title = raw, description = Nothing
    , status = Api.Done, priority = 5, completedAt = Just latest, dependencyCount = 0, memoryLinkCount = 0, createdAt = raw, updatedAt = later }

provenance : String -> Api.ObservationProvenance
provenance recordedAt =
    { sequence = 2, eventKind = "reviewed", reviewedGitSha = String.repeat 40 "b", contentVersion = Just "version", contentDigest = Just "digest"
    , recordedAt = recordedAt, actorType = Just "bot", actorId = Just "agent", actorLabel = Just "Reviewer", botIdentity = Nothing }

observation : String -> Api.Observation
observation id =
    { id = id, workspaceId = "w", subjects = [ { subjectKind = Api.SubjectFile, subject = "src/Main.elm" } ]
    , subjectKind = Api.SubjectFile, subject = "src/Main.elm", gitSha = String.repeat 40 "a", content = raw, contentVersion = "version", latestSequence = 2
    , currentProvenance = Just (provenance later), provenanceMatch = Nothing, createdAt = raw, updatedAt = latest }

audit : Api.AuditLogEntry
audit =
    { id = "audit", workspaceId = Just "w", entityType = "task", entityId = "task", action = Api.AuditUpdate
    , oldValues = Just (Encode.object [ ( "title", Encode.string "2040-01-01T00:00:00Z" ), ( "due_at", Encode.string raw ), ( "completed_at", Encode.null ) ])
    , newValues = Just (Encode.object [ ( "title", Encode.string "2041-01-01T00:00:00Z" ), ( "due_at", Encode.string later ), ( "completed_at", Encode.string latest ) ])
    , requestId = Just "request", actorType = Just "user", actorId = Just "user", actorLabel = Just "Owner", botIdentity = Nothing, changedAt = latest }

suite : Test
suite =
    describe "Timestamp metadata presentation"
        [ test "workspace and membership display local values and canonical updates introduce new demand" <| \_ ->
            let
                member = { workspaceId = "w", userId = "member", role = "read", grantedBy = Nothing, createdAt = raw, updatedAt = latest }
                adminModel = Admin.acceptCanonicalMemberships "w" [ member ] { base | activeTab = AdministrationTab }
                demanded = Presentation.reconcile ( adminModel, Cmd.none ) |> Tuple.first
            in
            Expect.all
                [ \_ -> Page.Workspace.viewWorkspacePage "w" adminModel |> Query.fromHtml |> Query.has [ Selector.text local, Selector.text "2025-12-31 16:01" ]
                , \_ -> Admin.viewWorkspaceAdminPanel workspace adminModel |> Query.fromHtml |> Query.has [ Selector.text "Updated 2025-12-31 16:02" ]
                , \_ -> Set.member latest demanded.timestamps.demand |> Expect.equal True
                , \_ -> demanded.workspaceAdmin.memberships |> Dict.get "w" |> Expect.equal (Just [ member ])
                ] ()
        , test "mounted cards render localized creation update completion while offscreen cache adds no demand" <| \_ ->
            let
                tasks = List.range 1 70 |> List.map (String.fromInt >> task)
                outside = task "outside"
                search = base.search
                loaded = { base | search = { search | filterTaskStatuses = [ "done" ] }, tasks = Dict.fromList (List.map (\value -> ( value.id, value )) (tasks ++ [ { outside | createdAt = "offscreen-only" } ])) }
                model = Cards.refreshViewport loaded ( loaded, Cmd.none ) |> Tuple.first
                demanded = Cards.timestampSources model
            in
            Expect.all
                [ \_ -> List.member "offscreen-only" demanded |> Expect.equal False
                , \_ -> [ raw, later, latest ] |> List.all (\value -> List.member value demanded) |> Expect.equal True
                , \_ -> Cards.viewProjectsTree "w" model |> Query.fromHtml |> Query.has [ Selector.text ("Created: " ++ local), Selector.text "Updated: 2025-12-31 16:01", Selector.text "Completed: 2025-12-31 16:02" ]
                , \_ -> model.tasks |> Expect.equal loaded.tasks
                ] ()
        , test "expanded audit context and old/new timestamp fields convert once and titles remain raw" <| \_ ->
            let
                initial = base.auditLog
                model = { base | page = AuditLogPage, auditLog = { initial | entries = [ audit ], expandedEntries = Dict.singleton audit.id True } }
                view = Audit.viewAuditLogPage model |> Query.fromHtml
            in
            Expect.all
                [ \_ -> Audit.auditContextDetailItems audit |> List.member ( "Timestamp", latest ) |> Expect.equal True
                , \_ -> view |> Query.find [ Selector.class "audit-entry-detail" ] |> Query.has [ Selector.text local, Selector.text "2025-12-31 16:01", Selector.text "2025-12-31 16:02", Selector.text "2040-01-01T00:00:00Z", Selector.text "2041-01-01T00:00:00Z" ]
                , \_ -> view |> Query.hasNot [ Selector.text "Invalid timestamp" ]
                , \_ -> Audit.timestampSources model |> Set.fromList |> Expect.equal (Set.fromList [ raw, later, latest, "" ])
                , \_ -> model.auditLog.entries |> Expect.equal [ audit ]
                , \_ -> view |> Query.findAll [ Selector.class "actor-attribution" ] |> Query.count (Expect.equal 1)
                , \_ -> view |> Query.hasNot [ Selector.text "Actor ID" ]
                ] ()
        , test "audit timestamps appear only with their mounted expanded entity history and use shared invalid fallback" <| \_ ->
            let
                initial = base.auditLog
                broken = { audit | changedAt = "bad" }
                model = { base | auditLog = { initial | entityHistory = Dict.singleton "task" [ broken ], historyExpanded = Dict.singleton "task" True } }
            in
            Expect.all
                [ \_ -> Audit.viewEntityHistory model "task" "task" |> Query.fromHtml |> Query.has [ Selector.text "Invalid timestamp", Selector.text local ]
                , \_ -> Audit.entityHistoryTimestampSources model "hidden" |> Expect.equal []
                , \_ -> Audit.entityHistoryTimestampSources model "task" |> List.member "bad" |> Expect.equal True
                ] ()
        , test "Observation detail current revision and paginated history share context while content remains exact" <| \_ ->
            let
                original = observation "selected"
                reviewed = provenance later
                selected = { original | latestSequence = 26, currentProvenance = Just { reviewed | sequence = 26 } }
                initial = Observation.init
                state = { initial | selectedId = Just selected.id, selectedDetail = Just selected, historyExpanded = True
                    , history = Just { workspaceId = "w", observationId = selected.id, sessionEpoch = 0, head = 26
                        , items = List.range 2 26 |> List.reverse |> List.map (\sequence -> { observationId = selected.id, provenance = let older = provenance raw in { older | sequence = sequence } })
                        , hasMore = True, nextOffset = 25, loading = False, error = Nothing, active = Nothing } }
                view = Observation.viewObservationsState display workspace state |> Query.fromHtml
                request = { workspaceId = "w", observationId = selected.id, sessionEpoch = 0, head = 26, token = 1, offset = 25 }
                loading = { state | history = state.history |> Maybe.map (\saved -> { saved | loading = True, active = Just request }) }
                paginated = Observation.update (GotObservationHistory request (Ok { items = [ { observationId = selected.id, provenance = let older = provenance latest in { older | sequence = 1 } } ], hasMore = False }))
                    { base | activeTab = ObservationsTab, observations = loading } |> Tuple.first |> .observations
            in
            Expect.all
                [ \_ -> view |> Query.find [ Selector.class "observation-detail-card" ] |> Query.has [ Selector.text local, Selector.text "2025-12-31 16:01", Selector.text "2025-12-31 16:02", Selector.text raw, Selector.text "Reviewer" ]
                , \_ -> Observation.viewObservationsState display workspace paginated |> Query.fromHtml |> Query.find [ Selector.class "observation-history-content" ] |> Query.has [ Selector.text "2025-12-31 16:02" ]
                , \_ -> Observation.timestampSources paginated |> List.member latest |> Expect.equal True
                , \_ -> selected.content |> Expect.equal raw
                , \_ -> view |> Query.find [ Selector.class "observation-history-content" ] |> Query.findAll [ Selector.class "observation-revision-value" ] |> Query.count (Expect.equal 25)
                , \_ -> view |> Query.findAll [ Selector.class "actor-details" ] |> Query.count (Expect.equal 0)
                ] ()
        , test "sidebar shows the signed-in principal and sign-out control without effective model or raw authority" <| \_ ->
            case base.sessionContext of
                Nothing -> Expect.fail "Expected session fixture"
                Just original ->
                    let
                        principal = original.principal
                        flags = base.flags
                        session = { original | authMode = "deployed", principal = { principal | actorLabel = "Alice", actorId = "raw-principal-id", authority = "grant_user" }
                            , effectiveActor = Just { actorType = Just "bot", actorId = Just "persona", actorLabel = Just "Reviewer", botIdentity = Just { role = "Reviewer", modelId = "codex:gpt-6.1-sol:high" } } }
                        model = { base | sessionContext = Just session, flags = { flags | authTokenPresent = True } }
                        view = Groups.viewSidebar model |> Query.fromHtml
                    in
                    Expect.all
                        [ \_ -> Permissions.principalAttributionLabel model |> Expect.equal (Just "Alice")
                        , \_ -> view |> Query.has [ Selector.text "Signed in", Selector.text "Alice" ]
                        , \_ -> view |> Query.hasNot [ Selector.text "codex:gpt-6.1-sol:high" ]
                        , \_ -> view |> Query.hasNot [ Selector.text "raw-principal-id" ]
                        , \_ -> view |> Query.hasNot [ Selector.text "grant_user" ]
                        , \_ -> view |> Query.findAll [ Selector.class "actor-details" ] |> Query.count (Expect.equal 0)
                        , \_ -> view |> Query.find [ Selector.class "sidebar-session-action" ] |> Event.simulate Event.click |> Event.expect LogoutRequested
                        , \_ -> model.sessionContext |> Maybe.map .globalPermissions |> Expect.equal (Just original.globalPermissions)
                        ] ()
        , test "local sidebar keeps an honest user fallback when the principal name is absent" <| \_ ->
            case base.sessionContext of
                Nothing -> Expect.fail "Expected session fixture"
                Just original ->
                    let
                        principal = original.principal
                        model = { base | sessionContext = Just { original | principal = { principal | actorLabel = " " } } }
                    in
                    Expect.all
                        [ \_ -> Permissions.principalAttributionLabel model |> Expect.equal (Just "Local User")
                        , \_ -> Groups.viewSidebar model |> Query.fromHtml |> Query.has [ Selector.text "Local session", Selector.text "Local User" ]
                        , \_ -> Groups.viewSidebar model |> Query.fromHtml |> Query.findAll [ Selector.class "sidebar-session-action" ] |> Query.count (Expect.equal 0)
                        ] ()
        , test "Observation mounted demand excludes offscreen cards and accepts live timestamp updates" <| \_ ->
            let
                first = observation "1"
                outside = { first | id = "outside", updatedAt = "offscreen-only" }
                values = (List.range 1 70 |> List.map (String.fromInt >> observation)) ++ [ outside ]
                initial = Observation.init
                rows = Array.fromList (List.map (ObservationCardRow "flat") values)
                keys = List.map (\value -> Observation.observationCardDomId "flat" value.id) values
                state = { initial | resultRows = rows, items = Dict.fromList (List.map (\value -> ( value.id, value )) values), orderedIds = List.map .id values
                    , viewport = ObservationViewport.rebuild { workspace = "w", epoch = 0, generation = "test", revision = 0 } keys initial.viewport }
                changed = { first | latestSequence = 3, updatedAt = "2026-01-01T00:03:00Z" }
                live = Observation.applyCanonicalObservation changed state
                refreshed = Observation.refreshViewport base ( { base | activeTab = ObservationsTab, observations = live }, Cmd.none ) |> Tuple.first
            in
            Expect.all
                [ \_ -> Observation.timestampSources state |> List.member "offscreen-only" |> Expect.equal False
                , \_ -> Observation.timestampSources refreshed.observations |> List.member changed.updatedAt |> Expect.equal True
                , \_ -> Dict.get changed.id refreshed.observations.items |> Maybe.map .updatedAt |> Expect.equal (Just changed.updatedAt)
                ] ()
        , test "presentation reply through Main preserves raw audit payload and date-filter boundaries" <| \_ ->
            let
                initial = base.auditLog
                model = { base | page = AuditLogPage, timestamps = Display.empty, auditLog = { initial | entries = [ audit ], expandedEntries = Dict.singleton audit.id True } }
                pending = Presentation.reconcile ( model, Cmd.none ) |> Tuple.first
            in
            case pending.timestamps.flight of
                Nothing -> Expect.fail "Rendered audit timestamps must be requested"
                Just request ->
                    let
                        payload = Encode.object
                            [ ( "context", Encode.string request.context ), ( "id", Encode.int request.id )
                            , ( "values", Encode.list (\source -> Encode.object [ ( "source", Encode.string source ), ( "local", Encode.object [ ( "text", Encode.string local ), ( "day", Encode.string "2025-12-31" ) ] ) ]) request.sources ) ]
                        accepted = Main.update (TimestampsFormatted payload) pending |> Tuple.first
                    in
                    Expect.equal ( [ audit ], model.auditLog.filters ) ( accepted.auditLog.entries, accepted.auditLog.filters )
        , test "typed terminal results and formatting replies update mounted localized card timestamps" <| \_ ->
            let
                initial = Observation.init
                accept prefix nodes priorTree =
                    let (branchStarted, branchRequest) = ObservationTree.begin prefix 0 priorTree in
                    branchRequest |> Maybe.map (\guard -> ObservationTree.receive guard (Ok {items = nodes, hasMore = False, totalCount = 2, matchCount = 2}) branchStarted) |> Maybe.withDefault branchStarted
                tree = ObservationTree.init |> ObservationTree.retire "fixture"
                    |> accept "" [{path = "src", segment = "src", hasChildren = True, observationCount = 2, terminals = []}]
                    |> accept "src" [{path = "src/Main.elm", segment = "Main.elm", hasChildren = False, observationCount = 2, terminals = [{subjectKind = Api.SubjectFile, subject = "src/Main.elm", observationCount = 2}]}]
                opened = {tree | expanded = Set.fromList ["src", "src/Main.elm"]}
                first = observation "first"
                second = {first | id = "second", updatedAt = later}
                subject = {subjectKind = Api.SubjectFile, subject = "src/Main.elm"}
                (started, request) = ObservationTree.beginTerminal subject 0 opened
                completed = request |> Maybe.map (\guard -> ObservationTree.receiveTerminal guard (Ok {items = [first, second], hasMore = False}) started) |> Maybe.withDefault started
                paginated = {initial | requestMode = ObservationFacetMode, tree = completed}
                view = Observation.viewObservationsState display workspace paginated |> Query.fromHtml
            in
            Expect.all
                [ \_ -> Observation.timestampSources paginated |> Set.fromList |> Expect.equal (Set.fromList [ later, latest ])
                , \_ -> view |> Query.has [ Selector.text "2025-12-31 16:02", Selector.text "2025-12-31 16:01" ]
                , \_ -> Observation.viewObservationsState Display.empty workspace paginated |> Query.fromHtml |> Query.has [ Selector.text "…" ]
                ] ()
        ]
