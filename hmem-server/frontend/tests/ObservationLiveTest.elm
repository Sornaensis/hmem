module ObservationLiveTest exposing (suite)

import Api
import Array
import AppShell
import Dict
import Expect
import Feature.DataLoading as Loading
import Feature.Observation as O
import Feature.Search as Search
import Helpers
import Http
import Json.Decode as D
import Json.Encode as E
import ObservationPreferences as P
import ObservationTree
import Route
import Set
import TimestampDisplay
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Types exposing (..)
import Url


preferenceDefaults = P.empty

workspaceId = "00000000-0000-4000-8000-000000000001"
workspaceFixture = { id = workspaceId, name = "Repository", workspaceType = Api.Repository, ghOwner = Nothing, ghRepo = Nothing, createdAt = "2026-01-01T00:00:00Z", updatedAt = "2026-01-01T00:00:00Z" }
emptyCountGuard = { workspaceId = "", sessionEpoch = -1, actor = "", token = -1, generation = -1, fingerprint = "" }
observationId = "00000000-0000-4000-8000-000000000002"


row identity =
    { id = identity, workspaceId = workspaceId, subjects = [ { subjectKind = Api.SubjectFile, subject = "src/Main.elm" } ], subjectKind = Api.SubjectFile, subject = "src/Main.elm"
    , gitSha = String.repeat 40 "a", content = "canonical", contentVersion = "00000000-0000-4000-8000-000000000003", latestSequence = 1, currentProvenance = Nothing, provenanceMatch = Nothing, createdAt = "2026-01-01T00:00:00Z", updatedAt = "2026-01-01T00:00:00Z" }


model =
    let
        url = { protocol = Url.Https, host = "example.invalid", port_ = Nothing, path = "/workspace/" ++ workspaceId, query = Nothing, fragment = Just "tab=observations" }
        flags = { apiUrl = "https://api.invalid", wsUrl = "wss://api.invalid", sessionId = "runtime", runtimeMode = "test", authTokenStorageKey = "auth", authTokenPresent = False, loginUrl = Nothing, logoutUrl = Nothing }
        base = AppShell.initModel Nothing url (WorkspacePage workspaceId) flags Nothing (Helpers.parseFragment url.fragment) |> AppShell.finalizeInit (WorkspacePage workspaceId)
        workspace = { id = workspaceId, name = "Repository", workspaceType = Api.Repository, ghOwner = Nothing, ghRepo = Nothing, createdAt = "2026-01-01T00:00:00Z", updatedAt = "2026-01-01T00:00:00Z" }
        session = { authMode = "local", effectiveActor = Nothing, principal = { actorType = "user", actorId = "actor", actorLabel = "Actor", authority = "local", grantUserId = Nothing }, globalPermissions = { createWorkspace = False, superadmin = False }
            , workspace = Just { workspaceId = workspaceId, role = Just "owner", canRead = True, canEdit = True, canAdmin = True } }
        state0 = O.startReloadForSession 3 workspaceId flatState
        discovery = state0.discovery
        state = { state0 | discovery = { discovery | sessionId = "10000000-0000-4000-8000-000000000001" } }
    in
    { base | auth = { status = AuthReady, mode = Just "local" }, sessionContext = Just session, sessionRequestEpoch = 3, selectedWorkspaceId = Just workspaceId, activeTab = ObservationsTab, workspaces = Dict.singleton workspaceId workspace
        , observations = { state | loading = False, expectedOffset = Nothing, items = Dict.singleton observationId (row observationId), orderedIds = [ observationId ], nextOffset = 264 } }


update : Msg -> Model -> ( Model, Cmd Msg )
update message current =
    let
        matching guard = current.observations.discovery.physical |> List.filter (\request -> case request.destination of
            DiscoveryTree tree -> tree == guard
            _ -> False) |> List.head
        deliver guard result treePage =
            case matching guard of
                Nothing -> O.update message current
                Just request ->
                    O.update (GotObservationDiscovery request (Result.map (\pageValue ->
                        { contextToken = Maybe.withDefault ("fixture-context-" ++ String.fromInt request.generation) request.contextToken
                        , expiresAt = "2099-01-01T00:00:00Z", retrieval = { mode = "fts_only", reason = Just "disabled" }, complete = True
                        , page = treePage pageValue }) result)) current
    in
    case message of
        GotObservationTree guard result -> deliver guard result (\value -> { items = [], hasMore = value.hasMore, counts = { workspaceId = workspaceId, totalCount = value.totalCount, matchCount = value.matchCount }, tree = Just value })
        GotObservationTreeTerminal guard result -> deliver guard result (\value -> { items = value.items, hasMore = value.hasMore, counts = { workspaceId = workspaceId, totalCount = 1000, matchCount = 1000 }, tree = Nothing })
        _ -> O.update message current


page offset values more current =
    let state = current.observations in
    Loading.update (GotObservations workspaceId Nothing state.requestGeneration state.queryFingerprint offset (Ok { items = values, hasMore = more })) current |> Tuple.first


project previous current = O.refreshViewport previous ( current, Cmd.none ) |> Tuple.first


settleFilters current =
    current.observations.filterDebounce |> Maybe.map (\stamp -> update (ObservationFiltersReady stamp) current |> Tuple.first) |> Maybe.withDefault current


liveFilterTests =
    [ test "scaffold and dual-kind paths select independently from native keyboard focus typed lock and expansion" <| \_ ->
        let
            step message previous = update message previous |> Tuple.first |> project previous
            started = step (SetObservationBrowseMode ObservationFacetMode) model
            node path hasChildren kinds = {path = path, segment = String.split "/" path |> List.reverse |> List.head |> Maybe.withDefault path, hasChildren = hasChildren, observationCount = 2, terminals = List.map (\kind -> {subjectKind = kind, subject = path, observationCount = 1}) kinds}
            accept prefix nodes previous = previous.observations.tree.physical |> List.filter (\request -> request.prefix == prefix && request.kind == Nothing) |> List.head
                |> Maybe.map (\request -> step (GotObservationTree request (Ok {items = nodes, hasMore = False, totalCount = 2, matchCount = 2})) previous) |> Maybe.withDefault previous
            root = accept "" [node "docs" False [], node "src" True []] started
            selected = step (SelectObservationTreePath "src") root
            moved = step (ObservationTreeKey "src" "Home") selected
            native = step (FocusObservationTreePath "docs") moved
            expanded = step (ToggleObservationTreePath "src") native |> accept "src" [node "src/auth" False [Api.SubjectFile, Api.SubjectGlob]]
            dual = step (SelectObservationTreePath "src/auth") expanded
            collapsed = step (ToggleObservationTreePath "src") dual
        in Expect.all
            [ \_ -> selected.observations.tree.selectedPath |> Expect.equal (Just "src")
            , \_ -> selected.observations.tree.selected |> Expect.equal Nothing
            , \_ -> native.observations.tree.focused |> Expect.equal (Just "docs")
            , \_ -> native.observations.tree.selectedPath |> Expect.equal (Just "src")
            , \_ -> expanded.observations.tree.selectedPath |> Expect.equal (Just "src")
            , \_ -> dual.observations.tree.selectedPath |> Expect.equal (Just "src/auth")
            , \_ -> dual.observations.tree.selected |> Expect.equal Nothing
            , \_ -> collapsed.observations.tree.selectedPath |> Expect.equal (Just "src/auth")
            , \_ -> collapsed.observations.selectedFacet |> Expect.equal Nothing ] ()
    , test "late detail membership and generic mode return preserve newer explicit path selection including proven absence" <| \_ ->
        let
            step message previous = update message previous |> Tuple.first |> project previous
            identity = "route-card"
            subject = {subjectKind = Api.SubjectFile, subject = "src/Main.elm"}
            query = Helpers.observationAppliedQuery model.observations
            restored = Helpers.restoreObservationQuery {query | requestMode = ObservationFacetMode} model.observations
            incoming = {model | observations = {restored | selectedId = Just identity, selectedDetail = Nothing, items = Dict.empty}}
            requested = O.bootstrapObservation 1 incoming |> Tuple.first |> O.selectObservation identity |> Tuple.first
            node path children = {path = path, segment = path, hasChildren = children, observationCount = 1, terminals = []}
            accept nodes previous = previous.observations.tree.physical |> List.filter (\request -> request.prefix == "" && request.kind == Nothing) |> List.head
                |> Maybe.map (\request -> step (GotObservationTree request (Ok {items = nodes, hasMore = False, totalCount = 1, matchCount = 1})) previous) |> Maybe.withDefault previous
            root = accept [node "docs" False, node "src" True] requested
            selected = step (SelectObservationTreePath "docs") root
            item = row identity
            receive value previous = previous.observations.activeDetailRequest |> Maybe.map (\request -> step (GotObservationDetail request.workspaceId request.observationId request.sessionEpoch request.token (Ok value)) previous) |> Maybe.withDefault previous
            completed = receive item selected
            returned = completed |> step (SetObservationBrowseMode ObservationFlatMode) |> step (SetObservationBrowseMode ObservationFacetMode)
            fresh = receive item root
            absent = O.refreshActiveResults selected |> Tuple.first |> accept [node "src" True]
            lateAbsent = receive item absent
            left = step (SetObservationBrowseMode ObservationFlatMode) root
            lateOutside = receive item left |> step (SetObservationBrowseMode ObservationFacetMode)
            wrong = receive {item | id = "wrong-target"} root
            superseded = O.selectObservation "different-card" root |> Tuple.first
            activated = step (SelectObservation identity) root
            lateActivated = receive item activated
            failed = root.observations.activeDetailRequest |> Maybe.map (\request -> step (GotObservationDetail request.workspaceId request.observationId request.sessionEpoch request.token (Err Http.NetworkError)) root) |> Maybe.withDefault root
            retry = O.selectObservation identity failed |> Tuple.first |> receive item
            explicit = O.restoreRouteResults completed |> Tuple.first
        in Expect.all
            [ \_ -> root.observations.treeSelectionPending |> Expect.equal True
            , \_ -> selected.observations.treeSelectionPending |> Expect.equal False
            , \_ -> completed.observations.selectedId |> Expect.equal (Just identity)
            , \_ -> completed.observations.tree.selectedPath |> Expect.equal (Just "docs")
            , \_ -> completed.observations.tree.revealTarget |> Maybe.map .subject |> Expect.equal (Just subject)
            , \_ -> returned.observations.tree.selectedPath |> Expect.equal (Just "docs")
            , \_ -> returned.observations.treeSelectionPending |> Expect.equal False
            , \_ -> fresh.observations.tree.selectedPath |> Expect.equal (Just subject.subject)
            , \_ -> fresh.observations.treeSelectionPending |> Expect.equal False
            , \_ -> absent.observations.tree.selectedPath |> Expect.equal Nothing
            , \_ -> lateAbsent.observations.tree.selectedPath |> Expect.equal Nothing
            , \_ -> left.observations.treeSelectionPending |> Expect.equal False
            , \_ -> lateOutside.observations.tree.selectedPath |> Expect.equal Nothing
            , \_ -> wrong.observations.treeSelectionPending |> Expect.equal False
            , \_ -> superseded.observations.treeSelectionPending |> Expect.equal False
            , \_ -> activated.observations.treeSelectionPending |> Expect.equal False
            , \_ -> lateActivated.observations.tree.selectedPath |> Expect.equal Nothing
            , \_ -> retry.observations.tree.selectedPath |> Expect.equal (Just subject.subject)
            , \_ -> retry.observations.treeSelectionPending |> Expect.equal False
            , \_ -> explicit.observations.tree.selectedPath |> Expect.equal (Just subject.subject)
            ] ()
    , test "actual Route dispatch arms only incoming explicit card changes and retires abandoned card reveals" <| \_ ->
        let
            step message previous = update message previous |> Tuple.first |> project previous
            started = step (SetObservationBrowseMode ObservationFacetMode) model
            original = O.selectRouteObservation observationId started |> Tuple.first |> step (SelectObservationTreePath "docs")
            url routeQuery identity previous = let oldUrl = previous.url in {oldUrl | fragment = Just ("tab=observations&ov=1&oq=" ++ Helpers.encodeObservationQuery routeQuery ++ (identity |> Maybe.map ((++) "&observation=") |> Maybe.withDefault ""))}
            query = Helpers.observationAppliedQuery original.observations
            changedQuery = {query | query = "changed"}
            next identity selectedQuery previous = Route.handleUrlChange (url selectedQuery identity previous) previous |> Tuple.first
            sameQuery = next (Just "next-card") query original
            otherQuery = next (Just "next-card") changedQuery original
            noCard = next Nothing changedQuery original
            sameCard = next (Just observationId) changedQuery original
            item = row "next-card"
            completed = sameQuery.observations.activeDetailRequest |> Maybe.map (\request -> step (GotObservationDetail request.workspaceId request.observationId request.sessionEpoch request.token (Ok item)) sameQuery) |> Maybe.withDefault sameQuery
            selected = step (SelectObservationTreePath "docs") completed
            echo = Route.handleUrlChange selected.url selected |> Tuple.first
            state = started.observations
            tree = state.tree
            orphaned = {started | observations = {state | selectedId = Nothing, treeSelectionPending = True, tree = ObservationTree.reveal {subjectKind = Api.SubjectFile, subject = "old/path"} "old-card" tree}}
            cleared = next Nothing query orphaned
        in Expect.all
            [ \_ -> sameQuery.observations.selectedId |> Expect.equal (Just "next-card")
            , \_ -> sameQuery.observations.treeSelectionPending |> Expect.equal True
            , \_ -> sameQuery.observations.tree.revealTarget |> Expect.equal Nothing
            , \_ -> sameQuery.observations.activeDetailRequest |> Maybe.map .observationId |> Expect.equal (Just "next-card")
            , \_ -> otherQuery.observations.treeSelectionPending |> Expect.equal True
            , \_ -> otherQuery.observations.tree.revealTarget |> Expect.equal Nothing
            , \_ -> noCard.observations.selectedId |> Expect.equal Nothing
            , \_ -> noCard.observations.treeSelectionPending |> Expect.equal False
            , \_ -> noCard.observations.tree.revealTarget |> Expect.equal Nothing
            , \_ -> sameCard.observations.tree.selectedPath |> Expect.equal (Just "src/Main.elm")
            , \_ -> sameCard.observations.tree.revealTarget |> Maybe.map .observationId |> Expect.equal (Just (Just observationId))
            , \_ -> completed.observations.tree.selectedPath |> Expect.equal (Just "src/Main.elm")
            , \_ -> echo.observations.tree.selectedPath |> Expect.equal (Just "docs")
            , \_ -> echo.observations.treeSelectionPending |> Expect.equal False
            , \_ -> cleared.observations.treeSelectionPending |> Expect.equal False
            , \_ -> cleared.observations.tree.revealTarget |> Expect.equal Nothing
            ] ()
    , test "selected path preference is optional bounded passive and subordinate to explicit reveal" <| \_ ->
        let
            initial = project model model
            owner = initial.observations.preferenceOwner |> Maybe.withDefault {workspaceId = "", actorId = "", authority = "", runtimeId = "", epoch = 0, requestId = 0, touch = 0}
            prefs = {preferenceDefaults | selectedPath = Just "unloaded/**/target", focusedPath = Just "different-focus"}
            encoded = P.encode owner prefs
            legacy = D.decodeValue (D.keyValuePairs D.value) encoded |> Result.withDefault [] |> List.filter (Tuple.first >> (/=) "selectedPath") |> E.object
            payload = E.object [("owner", P.ownerValue owner), ("value", encoded)]
            hydrated = update (ObservationPreferencesReceived payload) initial |> Tuple.first
            state = initial.observations
            intended = {initial | observations = {state | tree = ObservationTree.revealPath {subjectKind = Api.SubjectFile, subject = "explicit/path"} (ObservationTree.select "explicit/path" Nothing state.tree)}}
            explicit = update (ObservationPreferencesReceived payload) intended |> Tuple.first
            (cacheStarted, cacheGuard) = ObservationTree.begin "" 0 (ObservationTree.retire "current" ObservationTree.init)
            cache = cacheGuard |> Maybe.map (\request -> ObservationTree.receive request (Ok {items = [], hasMore = False, totalCount = 0, matchCount = 0}) cacheStarted) |> Maybe.withDefault cacheStarted
            proven = update (ObservationPreferencesReceived payload) {initial | observations = {state | tree = cache}} |> Tuple.first
        in Expect.all
            [ \_ -> D.decodeValue (P.decoder owner) legacy |> Result.map .selectedPath |> Expect.equal (Ok Nothing)
            , \_ -> D.decodeValue (P.decoder owner) encoded |> Result.map .selectedPath |> Expect.equal (Ok prefs.selectedPath)
            , \_ -> P.normalize {prefs | selectedPath = Just "../invalid"} |> .selectedPath |> Expect.equal Nothing
            , \_ -> hydrated.observations.tree.selectedPath |> Expect.equal prefs.selectedPath
            , \_ -> hydrated.observations.tree.focused |> Expect.equal prefs.focusedPath
            , \_ -> hydrated.observations.tree.expanded |> Expect.equal Set.empty
            , \_ -> hydrated.observations.tree.physical |> Expect.equal initial.observations.tree.physical
            , \_ -> explicit.observations.tree.selectedPath |> Expect.equal (Just "explicit/path")
            , \_ -> proven.observations.tree.selectedPath |> Expect.equal Nothing
            , \_ -> proven.observations.preferenceValue.selectedPath |> Expect.equal Nothing
            , \_ -> proven.observations.tree.physical |> Expect.equal [] ] ()
    , test "simultaneous live branch refresh admits a demanded typed terminal before unrelated branches exhaust" <| \_ ->
        let
            step message previous = update message previous |> Tuple.first |> project previous
            starting = step (SetObservationBrowseMode ObservationFacetMode) model
            node path hasChildren = {path = path, segment = String.split "/" path |> List.reverse |> List.head |> Maybe.withDefault path, hasChildren = hasChildren, observationCount = if hasChildren then 201 else 1, terminals = if path == "a" then [{subjectKind = Api.SubjectFile, subject = path, observationCount = 1}] else []}
            roots = List.map (\path -> node path True) ["a", "b", "c", "d"]
            pageOf nodes more = {items = nodes, hasMore = more, totalCount = 1000, matchCount = 1000}
            rootGuard = List.head starting.observations.tree.physical
            acceptedModel = rootGuard |> Maybe.map (\guard -> update (GotObservationTree guard (Ok (pageOf roots False))) starting |> Tuple.first) |> Maybe.withDefault starting
            accepted = acceptedModel.observations.tree
            children prefix = List.range 1 50 |> List.map (\index -> node (prefix ++ "/" ++ String.padLeft 3 '0' (String.fromInt index)) False)
            seed prefix prior =
                let (admitted, guard) = ObservationTree.begin prefix 0 prior in
                guard |> Maybe.map (\request -> ObservationTree.receive request (Ok (pageOf (children prefix) False)) admitted) |> Maybe.withDefault admitted
            populated = List.foldl seed accepted ["a", "b", "c", "d"]
            subject = {subjectKind = Api.SubjectFile, subject = "a"}
            original = row "fair-terminal"
            item = {original | subjects = [subject], subject = "a"}
            (terminalStarted, terminalGuard) = ObservationTree.beginTerminal subject 0 populated
            terminalReady = terminalGuard |> Maybe.map (\guard -> ObservationTree.receiveTerminal guard (Ok {items = [item], hasMore = False}) terminalStarted) |> Maybe.withDefault terminalStarted
            visible = {terminalReady | expanded = Set.fromList ["a", "b", "c", "d"]}
            state = acceptedModel.observations
            refreshing = O.refreshActiveResults {acceptedModel | observations = {state | tree = visible}} |> Tuple.first
            wave = refreshing.observations.tree.physical |> List.head |> Maybe.map (\guard -> step (GotObservationTree guard (Ok (pageOf roots False))) refreshing) |> Maybe.withDefault refreshing
            progressed = wave.observations.tree.physical |> List.filter (\guard -> guard.prefix == "a" && guard.kind == Nothing) |> List.head
                |> Maybe.map (\guard -> step (GotObservationTree guard (Ok (pageOf (children "a") True))) wave) |> Maybe.withDefault wave
            stale = rootGuard |> Maybe.map (\guard -> step (GotObservationTree guard (Ok (pageOf roots False))) progressed) |> Maybe.withDefault progressed
        in Expect.all
            [ \_ -> wave.observations.tree.physical |> List.map .kind |> Expect.equal [Nothing, Nothing, Nothing, Nothing]
            , \_ -> progressed.observations.tree.physical |> List.any (\guard -> guard.prefix == "a" && guard.kind == Just Api.SubjectFile) |> Expect.equal True
            , \_ -> progressed.observations.tree.physical |> List.any (\guard -> guard.prefix == "a" && guard.kind == Nothing) |> Expect.equal False
            , \_ -> Dict.get "a" progressed.observations.tree.branches |> Maybe.map .hasMore |> Expect.equal (Just True)
            , \_ -> List.length stale.observations.tree.physical |> Expect.equal 4
            , \_ -> progressed.observations.tree.lastOwner |> Expect.equal (Just ("a", Just Api.SubjectFile))
            , \_ -> stale.observations.tree.lastOwner |> Expect.equal progressed.observations.tree.lastOwner
            , \_ -> stale.observations.tree.nextToken |> Expect.equal progressed.observations.tree.nextToken ] ()
    , test "hidden Tree callbacks release only their own slots and return resumes preserved branch and terminal demand" <| \_ ->
        let
            step message previous = update message previous |> Tuple.first |> project previous
            started = step (SetObservationBrowseMode ObservationFacetMode) model
            node path hasChildren terminals = {path = path, segment = path |> String.split "/" |> List.reverse |> List.head |> Maybe.withDefault path, hasChildren = hasChildren, observationCount = 500, terminals = terminals}
            roots = [node "a" True [{subjectKind = Api.SubjectFile, subject = "a", observationCount = 500}], node "b" True [], node "c" True [], node "d" True []]
            pageOf nodes more = {items = nodes, hasMore = more, totalCount = 1000, matchCount = 1000}
            root = List.head started.observations.tree.physical |> Maybe.map (\guard -> step (GotObservationTree guard (Ok (pageOf roots False))) started) |> Maybe.withDefault started
            wave = List.foldl (\path current -> step (ToggleObservationTreePath path) current) root ["a", "b", "c", "d"]
            selected = step (SelectObservationTreePath "a") wave
            hidden = {selected | activeTab = ProjectsTab} |> project selected
            branch = wave.observations.tree.physical |> List.filter (\guard -> guard.prefix == "a" && guard.kind == Nothing) |> List.head
            terminal = wave.observations.tree.physical |> List.filter (\guard -> guard.kind == Just Api.SubjectFile) |> List.head
            children = List.range 1 50 |> List.map (\index -> node ("a/" ++ String.fromInt index) False [])
            branchDone = branch |> Maybe.map (\guard -> step (GotObservationTree guard (Ok (pageOf children True))) hidden) |> Maybe.withDefault hidden
            items = List.range 1 50 |> List.map (\index -> let item = row (String.fromInt index) in {item | subjects = [{subjectKind = Api.SubjectFile, subject = "a"}], subject = "a"})
            terminalDone = terminal |> Maybe.map (\guard -> step (GotObservationTreeTerminal guard (Ok {items = items, hasMore = True})) branchDone) |> Maybe.withDefault branchDone
            hiddenManual = step (LoadObservationTreePage "a" (Just Api.SubjectFile)) terminalDone
            drained = List.foldl (\guard current -> step (GotObservationTree guard (Ok (pageOf [] False))) current) hiddenManual hiddenManual.observations.tree.physical
            returned = {drained | activeTab = ObservationsTab} |> project drained
            resumed = step (LoadObservationTreePage "a" (Just Api.SubjectFile)) returned
        in
        Expect.all
            [ \_ -> Expect.equal [4,4,3,2,2,0] (List.map (\current -> List.length current.observations.tree.physical) [wave,hidden,branchDone,terminalDone,hiddenManual,drained])
            , \_ -> Expect.equal wave.observations.tree.nextToken drained.observations.tree.nextToken
            , \_ -> Expect.equal selected.observations.tree.selectedPath returned.observations.tree.selectedPath
            , \_ -> Expect.equal selected.observations.tree.expanded returned.observations.tree.expanded
            , \_ -> Expect.equal True (List.any (\guard -> guard.prefix == "a" && guard.offset == 50 && guard.kind == Nothing) resumed.observations.tree.physical)
            , \_ -> Expect.equal True (List.any (\guard -> guard.prefix == "d" && guard.kind == Nothing) resumed.observations.tree.physical)
            , \_ -> Expect.equal True (List.any (\guard -> guard.prefix == "a" && guard.offset == 50 && guard.kind == Just Api.SubjectFile) resumed.observations.tree.physical)
            ] ()
    , test "hidden explicit typed reveal admits only its current ancestor chain and matching terminal" <| \_ ->
        let
            subject = {subjectKind = Api.SubjectFile, subject = "a/file"}
            observations = model.observations
            query = Helpers.observationAppliedQuery observations
            restored = Helpers.restoreObservationQuery {query | requestMode = ObservationExactSubjectMode, selectedFacet = Just subject} observations
            started = O.bootstrapObservation 1 {model | activeTab = ProjectsTab, observations = restored} |> Tuple.first
            tree = started.observations.tree
            state = started.observations
            queued = {started | observations = {state | tree = {tree | expanded = Set.insert "b" tree.expanded}}}
            node path children terminals = {path = path, segment = path |> String.split "/" |> List.reverse |> List.head |> Maybe.withDefault path, hasChildren = children, observationCount = 1, terminals = terminals}
            receive nodes previous = previous.observations.tree.physical |> List.filter (\guard -> guard.kind == Nothing) |> List.head
                |> Maybe.map (\guard -> update (GotObservationTree guard (Ok {items = nodes, hasMore = False, totalCount = 1, matchCount = 1})) previous |> Tuple.first |> project previous) |> Maybe.withDefault previous
            root = receive [node "a" True [], node "b" True []] queued
            branch = receive [node "a/file" False [{subjectKind = Api.SubjectFile, subject = "a/file", observationCount = 1}, {subjectKind = Api.SubjectGlob, subject = "a/file", observationCount = 1}]] root
            suspended = {branch | observations = O.selectionForTab ProjectsTab branch.observations} |> project branch
            completed = List.head suspended.observations.tree.physical |> Maybe.map (\guard -> update (GotObservationTreeTerminal guard (Err Http.NetworkError)) suspended |> Tuple.first |> project suspended) |> Maybe.withDefault suspended
        in
        Expect.all
            [ \_ -> Expect.equal [("", Nothing)] (List.map (\guard -> (guard.prefix, guard.kind)) started.observations.tree.physical)
            , \_ -> Expect.equal [("a", Nothing)] (List.map (\guard -> (guard.prefix, guard.kind)) root.observations.tree.physical)
            , \_ -> Expect.equal [("a/file", Just Api.SubjectFile)] (List.map (\guard -> (guard.prefix, guard.kind)) branch.observations.tree.physical)
            , \_ -> Expect.equal [] completed.observations.tree.physical
            , \_ -> Expect.equal branch.observations.tree.expanded completed.observations.tree.expanded
            ] ()
    , test "logical connector topology distinguishes typed owners, sibling endings, incomplete tails and unrelated roots" <| \_ ->
        let
            node path children terminals = {path = path, segment = path |> String.split "/" |> List.reverse |> List.head |> Maybe.withDefault path, hasChildren = children, observationCount = 2, terminals = terminals}
            subject kind = {subjectKind = kind, subject = "a", observationCount = 1}
            accept prefix nodes tree =
                let (started, request) = ObservationTree.begin prefix 0 tree in
                request |> Maybe.map (\guard -> ObservationTree.receive guard (Ok {items = nodes, hasMore = False, totalCount = 2, matchCount = 2}) started) |> Maybe.withDefault started
            catalog = ObservationTree.retire "topology" ObservationTree.init
                |> accept "" [node "a" True [subject Api.SubjectFile, subject Api.SubjectGlob], node "z" False []]
                |> accept "a" [node "a/b" False [], node "a/c" False []]
            terminal kind tree =
                let typed = {subjectKind = kind, subject = "a"}
                    item = row (Api.subjectKindToString kind)
                    (started, request) = ObservationTree.beginTerminal typed 0 tree
                in request |> Maybe.map (\guard -> ObservationTree.receiveTerminal guard (Ok {items = [{item | subjects = [typed], subject = "a", subjectKind = kind}], hasMore = False}) started) |> Maybe.withDefault started
            loaded = terminal Api.SubjectFile catalog |> terminal Api.SubjectGlob
            completeTree = {loaded | expanded = Set.singleton "a"}
            state = flatState
            rows current = O.projectResultRows {state | requestMode = ObservationFacetMode, tree = current} |> Array.toList
            topology matches current = rows current |> List.filterMap (\entry -> case entry of
                ObservationTreeRow value content -> if matches content then Just value else Nothing
                _ -> Nothing) |> List.head
            matchesPath name content = case content of
                ObservationTreePathRow value -> value.path == name
                _ -> False
            card content = case content of
                ObservationCardRow _ value -> value.id == "file"
                _ -> False
            incomplete = {completeTree | branches = Dict.update "a" (Maybe.map (\branch -> {branch | error = Just "Retry paths"})) completeTree.branches}
        in
        Expect.all
            [ \_ -> topology (matchesPath "a") completeTree |> Maybe.andThen .children |> Maybe.map (\rail -> (rail.owner, rail.tail, rail.unknown)) |> Expect.equal (Just ("tree-path:a", "tree-path:a/c", False))
            , \_ -> topology (matchesPath "a/c") completeTree |> Maybe.map .following |> Expect.equal (Just TreeEnd)
            , \_ -> topology (matchesPath "a/c") incomplete |> Maybe.map .following |> Expect.equal (Just TreeUnknown)
            , \_ -> topology (matchesPath "a") incomplete |> Maybe.andThen .children |> Maybe.map (\rail -> (rail.tail, rail.unknown)) |> Expect.equal (Just ("tree-page:branch:a", True))
            , \_ -> topology card completeTree |> Maybe.andThen .parent |> Maybe.map .owner |> Expect.equal (Just "tree-terminal:file:a")
            , \_ -> topology card completeTree |> Maybe.map (\value -> List.map .owner value.ancestors) |> Expect.equal (Just ["tree-path:a"])
            , \_ -> topology (matchesPath "z") completeTree |> Maybe.map (\value -> (value.parent, value.ancestors, value.root)) |> Expect.equal (Just (Nothing, [], "tree-path:z"))
            ] ()
    , test "a stale root callback cannot advance the last completed query while the latest root is pending" <| \_ ->
        let
            step message previous = update message previous |> Tuple.first |> project previous
            started = step (SetObservationBrowseMode ObservationFacetMode) model
            emptyPage = {items = [], hasMore = False, totalCount = 1, matchCount = 0}
            completed = List.head started.observations.tree.physical |> Maybe.map (\request -> step (GotObservationTree request (Ok emptyPage)) started) |> Maybe.withDefault started
            first = step (SetObservationQuery "first") completed |> settleFilters
            second = step (SetObservationQuery "second") first |> settleFilters
            stale = List.head first.observations.tree.physical |> Maybe.map (\request -> step (GotObservationTree request (Ok emptyPage)) second) |> Maybe.withDefault second
        in Expect.all
            [ \_ -> stale.observations.completedQuery |> Expect.equal completed.observations.completedQuery
            , \_ -> stale.observations.query |> Expect.equal "second"
            , \_ -> stale.observations.tree.totals |> Expect.equal completed.observations.tree.totals
            , \_ -> List.length stale.observations.tree.physical |> Expect.equal 1 ] ()
    , test "a filtered catalog retires an existing card only after current complete membership proof" <| \_ ->
        let
            step message previous = update message previous |> Tuple.first |> project previous
            accept prefix nodes previous =
                previous.observations.tree.physical |> List.filter (\request -> request.prefix == prefix && request.kind == Nothing) |> List.head
                    |> Maybe.map (\request -> step (GotObservationTree request (Ok {items = nodes, hasMore = False, totalCount = 1, matchCount = if List.isEmpty nodes then 0 else 1})) previous) |> Maybe.withDefault previous
            directory = {path = "src", segment = "src", hasChildren = True, observationCount = 1, terminals = []}
            leaf = {path = "src/Main.elm", segment = "Main.elm", hasChildren = False, observationCount = 1, terminals = [{subjectKind = Api.SubjectFile, subject = "src/Main.elm", observationCount = 1}]}
            started = step (SetObservationBrowseMode ObservationFacetMode) model
            root = accept "" [directory] started |> step (ToggleObservationTreePath "src")
            branch = accept "src" [leaf] root |> step (ToggleObservationTreePath "src/Main.elm")
            item = row "loaded-card"
            loaded = branch.observations.tree.physical |> List.filter (\request -> request.kind == Just Api.SubjectFile) |> List.head
                |> Maybe.map (\request -> step (GotObservationTreeTerminal request (Ok {items = [item], hasMore = False})) branch) |> Maybe.withDefault branch
            selected = step (SelectObservationFrom item.id (O.observationCardDomId "tree-terminal:file:src/Main.elm" item.id)) loaded
            requested = step (SetObservationQuery "absent") selected |> settleFilters
            completed = accept "" [] requested
        in Expect.all
            [ \_ -> selected.observations.selectedId |> Expect.equal (Just item.id)
            , \_ -> requested.observations.selectedId |> Expect.equal (Just item.id)
            , \_ -> completed.observations.selectedId |> Expect.equal Nothing
            , \_ -> completed.observations.selectedFacet |> Expect.equal Nothing
            , \_ -> completed.observations.query |> Expect.equal "absent"
            , \_ -> completed.observations.activeDetailRequest |> Expect.equal Nothing ] ()
    , test "an old typed catalog cannot reject a newly restored terminal before its authoritative parent arrives" <| \_ ->
        let
            oldNode = {path = "a", segment = "a", hasChildren = False, observationCount = 1, terminals = [{subjectKind = Api.SubjectFile, subject = "a", observationCount = 1}]}
            (oldStarted, oldRequest) = ObservationTree.begin "" 0 (ObservationTree.retire "old" ObservationTree.init)
            oldTree = oldRequest |> Maybe.map (\request -> ObservationTree.receive request (Ok {items = [oldNode], hasMore = False, totalCount = 1, matchCount = 1}) oldStarted) |> Maybe.withDefault oldStarted
            query = Helpers.observationAppliedQuery model.observations
            restored = Helpers.restoreObservationQuery {query | requestMode = ObservationExactSubjectMode, selectedFacet = Just {subjectKind = Api.SubjectGlob, subject = "a"}} model.observations
            started = O.bootstrapObservation 1 {model | observations = {restored | tree = oldTree}} |> Tuple.first
            projected = project started started
        in Expect.all
            [ \_ -> projected.observations.selectedFacet |> Expect.equal restored.selectedFacet
            , \_ -> projected.observations.tree.revealTarget |> Maybe.map (.subject >> .subjectKind) |> Expect.equal (Just Api.SubjectGlob)
            , \_ -> List.length projected.observations.tree.physical |> Expect.equal 1 ] ()
    , test "automatic path recovery keeps newer outside focus while preserving the roving successor" <| \_ ->
        let
            start = update (SetObservationBrowseMode ObservationFacetMode) model |> Tuple.first
            node path = {path = path, segment = path, hasChildren = False, observationCount = 1, terminals = []}
            root = List.head start.observations.tree.physical |> Maybe.map (\request -> update (GotObservationTree request (Ok {items = [node "a"], hasMore = False, totalCount = 1, matchCount = 1})) start |> Tuple.first |> project start) |> Maybe.withDefault start
            focused = update (SelectObservationTreePath "a") root |> Tuple.first |> project root
            replacement = update (SetObservationQuery "b") focused |> Tuple.first |> settleFilters
            state = replacement.observations
            viewport = state.viewport
            outside = {replacement | observations = {state | viewport = {viewport | focus = Nothing}}}
            completed = List.head outside.observations.tree.physical |> Maybe.map (\request -> update (GotObservationTree request (Ok {items = [node "b"], hasMore = False, totalCount = 1, matchCount = 1})) outside |> Tuple.first) |> Maybe.withDefault outside
        in Expect.all
            [ \_ -> completed.observations.tree.focused |> Expect.equal (Just "b")
            , \_ -> completed.observations.tree.focusPending |> Expect.equal Nothing
            , \_ -> completed.observations.viewport.focus |> Expect.equal Nothing ] ()
    , test "explicit Files submission consumes its pending text timer without a duplicate late reload" <| \_ ->
        let
            files = update OpenObservationFileComposer model |> Tuple.first
                |> update (SetObservationQuery "latest text") |> Tuple.first
                |> update (SetObservationMatchPaths "src/Main.elm") |> Tuple.first
            submitted = update ApplyObservationMatch files |> Tuple.first
            late = files.observations.filterDebounce |> Maybe.map (\stamp -> update (ObservationFiltersReady stamp) submitted |> Tuple.first) |> Maybe.withDefault submitted
        in Expect.all
            [ \_ -> submitted.observations.filterDebounce |> Expect.equal Nothing
            , \_ -> submitted.observations.requestGeneration |> Expect.equal late.observations.requestGeneration
            , \_ -> late.observations |> Expect.equal submitted.observations
            , \_ -> (O.matchQuery workspaceId ["src/Main.elm"] 0 submitted.observations).query |> Expect.equal (Just "latest text") ] ()
    , test "contradictory legacy terminal selection retires only after authoritative typed proof and preserves shared filters" <| \_ ->
        let
            query = Helpers.observationAppliedQuery model.observations
            state = Helpers.restoreObservationQuery {query | requestMode = ObservationExactSubjectMode, subjectKind = Just Api.SubjectFile, selectedFacet = Just {subjectKind = Api.SubjectGlob, subject = "a"}} model.observations
            started = O.bootstrapObservation 1 {model | observations = state} |> Tuple.first
            node = {path = "a", segment = "a", hasChildren = False, observationCount = 1, terminals = [{subjectKind = Api.SubjectFile, subject = "a", observationCount = 1}]}
            completed = List.head started.observations.tree.physical |> Maybe.map (\request -> update (GotObservationTree request (Ok {items = [node], hasMore = False, totalCount = 1, matchCount = 1})) started |> Tuple.first |> project started) |> Maybe.withDefault started
        in Expect.all
            [ \_ -> started.observations.tree.revealTarget |> Maybe.map .observationId |> Expect.equal (Just Nothing)
            , \_ -> completed.observations.selectedFacet |> Expect.equal Nothing
            , \_ -> completed.observations.subjectKind |> Expect.equal (Just Api.SubjectFile)
            , \_ -> completed.observations.tree.revealTarget |> Expect.equal Nothing
            , \_ -> completed.observations.activeDetailRequest |> Expect.equal Nothing
            , \_ -> List.map .kind completed.observations.tree.physical |> Expect.equal [Just Api.SubjectFile] ] ()
    , test "terminal-only legacy links reveal an exact typed path without admitting any card detail" <| \_ ->
        let
            query = Helpers.observationAppliedQuery model.observations
            subject = {subjectKind = Api.SubjectFile, subject = "a"}
            state = Helpers.restoreObservationQuery {query | requestMode = ObservationExactSubjectMode, selectedFacet = Just subject} model.observations
            started = O.bootstrapObservation 1 {model | observations = state} |> Tuple.first
            node = {path = "a", segment = "a", hasChildren = False, observationCount = 1, terminals = [{subjectKind = Api.SubjectFile, subject = "a", observationCount = 1}]}
            root = List.head started.observations.tree.physical |> Maybe.map (\request -> update (GotObservationTree request (Ok {items = [node], hasMore = False, totalCount = 1, matchCount = 1})) started |> Tuple.first |> project started) |> Maybe.withDefault started
            original = row observationId
            item = {original | subjects = [subject], subject = "a"}
            completed = List.head root.observations.tree.physical |> Maybe.map (\request -> update (GotObservationTreeTerminal request (Ok {items = [item], hasMore = False})) root |> Tuple.first |> project root) |> Maybe.withDefault root
        in Expect.all
            [ \_ -> completed.observations.tree.revealTarget |> Expect.equal Nothing
            , \_ -> completed.observations.viewport.focus |> Expect.equal (Just "tree-path:a")
            , \_ -> completed.observations.selectedId |> Expect.equal Nothing
            , \_ -> completed.observations.activeDetailRequest |> Expect.equal Nothing ] ()
    , test "encoded explicit query keys outrank a different stored context during hydration" <| \_ ->
        let
            query = Helpers.observationAppliedQuery model.observations
            active = { query | query = "explicit B" }
            stored = { query | query = "stored A" }
            oldUrl = model.url
            initial = { model | url = { oldUrl | fragment = Just ("tab=observations&o%76=1&o%71=" ++ Helpers.encodeObservationQuery active) }, observations = Helpers.restoreObservationQuery active model.observations } |> (\current -> project current current)
            owner = initial.observations.preferenceOwner |> Maybe.withDefault { workspaceId = "", actorId = "", authority = "", runtimeId = "", epoch = 0, requestId = 0, touch = 0 }
            prefs = { preferenceDefaults | context = Just (Helpers.encodeObservationQuery stored), queries = Dict.singleton "flat" "stored A" }
            hydrated = update (ObservationPreferencesReceived (E.object [ ("owner", P.ownerValue owner), ("value", P.encode owner prefs) ])) initial |> Tuple.first
        in Expect.all
            [ \_ -> (Helpers.observationUrlContext initial.url).notice |> Expect.equal Nothing
            , \_ -> hydrated.observations.query |> Expect.equal "explicit B"
            , \_ -> (O.listQuery workspaceId 0 hydrated.observations).query |> Expect.equal (Just "explicit B")
            , \_ -> hydrated.observations.requestGeneration |> Expect.equal initial.observations.requestGeneration ] ()
    , test "terminal selection retires a text timer without releasing physical tree occupancy" <| \_ ->
        let
            tree = update (SetObservationBrowseMode ObservationFacetMode) model |> Tuple.first
            typed = update (SetObservationQuery "src") tree |> Tuple.first
            stamp = typed.observations.filterDebounce
            state = typed.observations
            physical = ObservationTree.begin "" 0 state.tree |> Tuple.first
            selected = update (SelectObservationFacet Api.SubjectFile "src/Main.elm") { typed | observations = { state | tree = physical } } |> Tuple.first
            late = stamp |> Maybe.map (\guard -> update (ObservationFiltersReady guard) selected |> Tuple.first) |> Maybe.withDefault selected
            completed = List.head late.observations.tree.physical |> Maybe.map (\request -> update (GotObservationTree request (Ok {items = [], hasMore = False, totalCount = 1, matchCount = 0})) late |> Tuple.first) |> Maybe.withDefault late
        in Expect.all
            [ \_ -> selected.observations.filterDebounce |> Expect.equal Nothing
            , \_ -> selected.observations.nextFilterToken > typed.observations.nextFilterToken |> Expect.equal True
            , \_ -> List.all (\request -> List.member request selected.observations.tree.physical) physical.physical |> Expect.equal True
            , \_ -> late.observations |> Expect.equal selected.observations
            , \_ -> completed.observations.tree.totals |> Expect.equal (Just (1, 0))
            , \_ -> O.viewObservationsState TimestampDisplay.empty workspaceFixture completed.observations |> Query.fromHtml |> Query.hasNot [ Selector.class "observation-filter-pending" ] ] ()
    , test "invalid restored SHA reserves no bootstrap observation obligation and root completion settles loading" <| \_ ->
        let
            state = model.observations
            loading = model.dataLoading
            base = { model | observations = Helpers.restoreObservationQuery { subjectQuery = "", query = "", requestMode = ObservationFlatMode, subjectKind = Nothing, subject = "", selectedFacet = Nothing, gitSha = "abc", currentGitSha = "", historyGitSha = "", matchAppliedPaths = [] } state
                , dataLoading = { loading | activeWorkspaceLoadToken = Just 1, loadingWorkspaceData = True, pendingWorkspaceLoads = 1 } }
            prepared = Loading.prepareRootNavigationRequest (Just workspaceId) base
            admitted = Loading.update (GotWorkspace workspaceId 1 (Ok workspaceFixture)) prepared |> Tuple.first
            completed = admitted.dataLoading.rootNavigationRequest |> Maybe.map (\request -> Loading.update (GotRootNavigation workspaceId request.sessionEpoch (Just 1) request.generation request.filterFingerprint 0 0 (Ok { workspaceId = workspaceId, projects = {items = [], hasMore = False}, tasks = {items = [], hasMore = False} })) admitted |> Tuple.first) |> Maybe.withDefault admitted
        in Expect.all
            [ \_ -> admitted.dataLoading.initialObservationLoad |> Expect.equal Nothing
            , \_ -> admitted.dataLoading.pendingWorkspaceLoads |> Expect.equal 1
            , \_ -> completed.dataLoading.pendingWorkspaceLoads |> Expect.equal 0
            , \_ -> completed.dataLoading.loadingWorkspaceData |> Expect.equal False
            , \_ -> completed.dataLoading.activeWorkspaceLoadToken |> Expect.equal Nothing ] ()
    , test "accepted saved active scroll survives hydration and switching away and back while detail wins" <| \_ ->
        let
            initial = project model model
            owner = initial.observations.preferenceOwner |> Maybe.withDefault { workspaceId = "", actorId = "", authority = "", runtimeId = "", epoch = 0, requestId = 0, touch = 0 }
            prefs = { preferenceDefaults | scrolls = Dict.fromList [ ("flat", 900), ("tree", 400) ] }
            payload = E.object [ ("owner", P.ownerValue owner), ("value", P.encode owner prefs) ]
            hydrated = update (ObservationPreferencesReceived payload) initial |> Tuple.first |> project initial
            waitingState = initial.observations
            waiting = { initial | observations = { waitingState | loading = True, expectedOffset = Just 0 } }
            deferred = update (ObservationPreferencesReceived payload) waiting |> Tuple.first |> project waiting
            loaded = page 0 [ row observationId ] False deferred |> project deferred
            tree = update (SetObservationBrowseMode ObservationFacetMode) hydrated |> Tuple.first |> project hydrated
            returned = update (SetObservationBrowseMode ObservationFlatMode) tree |> Tuple.first |> project tree
            state = initial.observations
            viewport = state.viewport
            oldUrl = initial.url
            detail = { initial | url = { oldUrl | fragment = Just ("tab=observations&observation=" ++ observationId) }, observations = { state | selectedId = Just observationId, viewport = { viewport | top = 120 } } }
            selected = update (ObservationPreferencesReceived payload) detail |> Tuple.first
        in Expect.all
            [ \_ -> hydrated.observations.viewport.top |> Expect.equal 900
            , \_ -> hydrated.observations.preferenceScroll |> Expect.equal Nothing
            , \_ -> deferred.observations.preferenceScroll |> Expect.equal (Just 900)
            , \_ -> loaded.observations.viewport.top |> Expect.equal 900
            , \_ -> loaded.observations.preferenceScroll |> Expect.equal Nothing
            , \_ -> returned.observations.viewport.top |> Expect.equal 900
            , \_ -> selected.observations.viewport.top |> Expect.equal 120
            , \_ -> selected.observations.preferenceScroll |> Expect.equal Nothing ] ()
    , test "workspace routes retain physical tree admissions while retiring private caches" <| \_ ->
        let
            state = model.observations
            ( tree, request ) = ObservationTree.begin "" 0 (ObservationTree.retire "old-workspace" state.tree)
            started = { model | observations = { state | tree = tree, modeQueries = Dict.fromList [ ("flat", "private"), ("tree", "old-subject") ] } }
            oldUrl = model.url
            next = Route.handleUrlChange { oldUrl | path = "/workspace/00000000-0000-4000-8000-000000000009", fragment = Just "tab=observations" } started |> Tuple.first
            settled = request |> Maybe.map (\guard -> update (GotObservationTree guard (Ok { items = [], hasMore = False, totalCount = 0, matchCount = 0 })) next |> Tuple.first) |> Maybe.withDefault next
        in Expect.all
            [ \_ -> List.length next.observations.tree.physical |> Expect.equal 1
            , \_ -> List.length settled.observations.tree.physical |> Expect.equal 0
            , \_ -> settled.observations.tree.totals |> Expect.equal Nothing
            , \_ -> next.observations.modeQueries |> Expect.equal (Dict.singleton "tree" "") ] ()
    , test "mode scroll positions and bounded legacy Subject preferences migrate independently" <| \_ ->
        let
            original = model.observations
            viewport = original.viewport
            positioned = { model | observations = { original | viewport = { viewport | top = 400 } } }
            tree = update (SetObservationBrowseMode ObservationFacetMode) positioned |> Tuple.first
            treeState = tree.observations
            treeViewport = treeState.viewport
            moved = { tree | observations = { treeState | viewport = { treeViewport | top = 800 } } }
            all = update (SetObservationBrowseMode ObservationFlatMode) moved |> Tuple.first
            back = update (SetObservationBrowseMode ObservationFacetMode) all |> Tuple.first
            legacy = { preferenceDefaults | detail = Just { id = observationId, occurrence = Nothing, mode = "facets" }, queries = Dict.singleton "facets" "src/auth", paths = [ "src", "src/**" ], focusedPath = Just "src/**", scrolls = Dict.singleton "tree" 800 }
            owner = { workspaceId = workspaceId, actorId = "actor", authority = "local", runtimeId = "runtime", epoch = 3, requestId = 1, touch = 0 }
            migrated = D.decodeValue (P.decoder owner) (P.encode owner legacy)
        in Expect.all
            [ \_ -> all.observations.viewport.top |> Expect.equal 400
            , \_ -> back.observations.viewport.top |> Expect.equal 800
            , \_ -> migrated |> Result.map (.queries >> Dict.get "tree") |> Expect.equal (Ok (Just "src/auth"))
            , \_ -> migrated |> Result.map (.detail >> Maybe.map .mode) |> Expect.equal (Ok (Just "tree"))
            , \_ -> migrated |> Result.map .focusedPath |> Expect.equal (Ok (Just "src/**")) ] ()
    , test "tree URL carries literal subject search and all SHA predicates without FTS or offset wrap" <| \_ ->
        let
            original = model.observations
            state = { original | subjectQuery = "src/auth%_", gitSha = String.repeat 40 "a", currentGitSha = String.repeat 40 "b", historyGitSha = String.repeat 40 "c", appliedQuery = Nothing }
            url = Api.observationSubjectTreeUrl "https://api.invalid" (O.treeQuery workspaceId "src/**" 2147483647 state)
        in Expect.all
            [ \_ -> String.contains "subject_query=src%2Fauth%25_" url |> Expect.equal True
            , \_ -> String.contains "prefix=src%2F**" url |> Expect.equal True
            , \_ -> String.contains "offset=2147483647" url |> Expect.equal True
            , \_ -> String.contains "current_git_sha=" url && String.contains "history_git_sha=" url && String.contains "git_sha=" url |> Expect.equal True
            , \_ -> String.contains "&query=" url |> Expect.equal False ] ()
    , test "each invalid populated SHA stays local and clearing immediately restores a valid query" <| \_ ->
        let
            check setter value =
                let invalid = update (setter value) model |> Tuple.first
                    cleared = update (setter "") invalid |> Tuple.first
                in invalid.observations.expectedOffset == Nothing && not invalid.observations.loading && O.filterValidationError invalid.observations /= Nothing && cleared.observations.expectedOffset == Just 0
        in List.concatMap (\setter -> List.map (check setter) [ "abc", String.repeat 40 "A", " " ++ String.repeat 40 "a" ]) [ SetObservationGitSha, SetObservationCurrentGitSha, SetObservationHistoryGitSha ] |> List.all identity |> Expect.equal True
    , test "terminal-only refresh preserves lazy typed path reveal and its original focus intent" <| \_ ->
        let
            selected = {subjectKind = Api.SubjectGlob, subject = "src/**/*.elm"}
            query = Helpers.observationAppliedQuery model.observations
            state = Helpers.restoreObservationQuery {query | requestMode = ObservationExactSubjectMode, selectedFacet = Just selected} model.observations
            refreshing = O.refreshActiveResults {model | observations = state} |> Tuple.first
        in Expect.all
            [ \_ -> refreshing.observations.tree.revealTarget |> Expect.equal (Just {subject = selected, observationId = Nothing, focus = True})
            , \_ -> List.all (\path -> Set.member path refreshing.observations.tree.expanded) ["src", "src/**", "src/**/*.elm"] |> Expect.equal True
            , \_ -> refreshing.observations.selectedId |> Expect.equal Nothing ] ()
    , test "settled terminal-only selection never reopens its collapsed parent during canonical refresh" <| \_ ->
        let
            step message previous = update message previous |> Tuple.first |> project previous
            subject = {subjectKind = Api.SubjectFile, subject = "src/Main.elm"}
            query = Helpers.observationAppliedQuery model.observations
            state = Helpers.restoreObservationQuery {query | requestMode = ObservationExactSubjectMode, selectedFacet = Just subject} model.observations
            started = O.bootstrapObservation 1 {model | observations = state} |> Tuple.first
            accept prefix nodes previous = previous.observations.tree.physical |> List.filter (\request -> request.prefix == prefix && request.kind == Nothing) |> List.head
                |> Maybe.map (\request -> step (GotObservationTree request (Ok {items = nodes, hasMore = False, totalCount = 1, matchCount = 1})) previous) |> Maybe.withDefault previous
            rootNode = {path = "src", segment = "src", hasChildren = True, observationCount = 1, terminals = []}
            terminalNode = {path = subject.subject, segment = "Main.elm", hasChildren = False, observationCount = 1, terminals = [{subjectKind = Api.SubjectFile, subject = subject.subject, observationCount = 1}]}
            branch = started |> accept "" [rootNode] |> accept "src" [terminalNode]
            completed = branch.observations.tree.physical |> List.filter (\request -> request.kind == Just Api.SubjectFile) |> List.head
                |> Maybe.map (\request -> step (GotObservationTreeTerminal request (Ok {items = [row observationId], hasMore = False})) branch) |> Maybe.withDefault branch
            collapsed = step (ToggleObservationTreePath "src") completed
            refreshing = O.refreshActiveResults collapsed |> Tuple.first
            settled = accept "" [rootNode] refreshing
            filtered = update (SetObservationQuery "src") settled |> Tuple.first |> settleFilters
            explicit = O.restoreRouteResults settled |> Tuple.first
            newSession = {settled | sessionRequestEpoch = settled.sessionRequestEpoch + 1} |> O.refreshActiveResults |> Tuple.first
        in Expect.all
            [ \_ -> completed.observations.tree.resolvedPath |> Maybe.map .subject |> Expect.equal (Just subject)
            , \_ -> settled.observations.tree.revealTarget |> Expect.equal Nothing
            , \_ -> Set.member "src" settled.observations.tree.expanded |> Expect.equal False
            , \_ -> settled.observations.tree.focused |> Expect.equal (Just "src")
            , \_ -> settled.observations.selectedFacet |> Expect.equal (Just subject)
            , \_ -> settled.observations.tree.physical |> Expect.equal []
            , \_ -> Set.member "src" filtered.observations.tree.expanded |> Expect.equal False
            , \_ -> filtered.observations.tree.revealTarget |> Expect.equal Nothing
            , \_ -> explicit.observations.tree.revealTarget |> Maybe.map .focus |> Expect.equal (Just True)
            , \_ -> explicit.observations.tree.resolvedPath |> Expect.equal Nothing
            , \_ -> newSession.observations.tree.resolvedPath |> Expect.equal Nothing ] ()
    , test "manual terminal continuation and retry require valid filters mode and current read authority" <| \_ ->
        let
            bootstrapping = update (SetObservationBrowseMode ObservationFacetMode) model |> Tuple.first
            started = List.head bootstrapping.observations.tree.physical |> Maybe.map (\rootRequest -> update (GotObservationTree rootRequest (Ok {items = [], hasMore = False, totalCount = 1000, matchCount = 1000})) bootstrapping |> Tuple.first) |> Maybe.withDefault bootstrapping
            state = started.observations
            initTree = ObservationTree.init
            (admitted, guard) = ObservationTree.beginTerminal {subjectKind = Api.SubjectFile, subject = "src/Main.elm"} 0 {initTree | lifetime = state.tree.lifetime, displayLifetime = state.tree.lifetime}
            cached = guard |> Maybe.map (\request -> ObservationTree.receiveTerminal request (Ok {items = List.range 1 50 |> List.map (String.fromInt >> row), hasMore = True}) admitted) |> Maybe.withDefault admitted
            clean = {started | observations = {state | tree = cached}}
            invalid = update (SetObservationHistoryGitSha "abc") clean |> Tuple.first
            typing = update (SetObservationQuery "pending") clean |> Tuple.first
            denied = {clean | auth = {status = AuthBooting, mode = Nothing}}
            otherMode = {clean | observations = {state | requestMode = ObservationFlatMode, tree = clean.observations.tree}}
            attempt previous = update (LoadObservationTreePage "src/Main.elm" (Just Api.SubjectFile)) previous |> Tuple.first
            admin = clean.workspaceAdmin
            pendingAuthority = {clean | workspaceAdmin = {admin | authorizationPending = Just {workspaceId = workspaceId, sessionEpoch = 3, sessionKey = "pending"}}}
            blocked = [invalid, typing, denied, otherMode, pendingAuthority, {clean | sessionContext = Nothing}, {clean | selectedWorkspaceId = Nothing}]
            valid = attempt clean
            failed = List.head valid.observations.tree.physical |> Maybe.map (\request -> update (GotObservationTreeTerminal request (Err Http.NetworkError)) valid |> Tuple.first) |> Maybe.withDefault valid
            invalidRetry = update (SetObservationHistoryGitSha "abc") failed |> Tuple.first
            retried = attempt failed
        in Expect.all
            [ \_ -> blocked |> List.all (\previous -> (attempt previous).observations.tree == previous.observations.tree) |> Expect.equal True
            , \_ -> List.length valid.observations.tree.physical |> Expect.equal 1
            , \_ -> List.head valid.observations.tree.physical |> Maybe.map .kind |> Expect.equal (Just (Just Api.SubjectFile))
            , \_ -> List.head valid.observations.tree.physical |> Maybe.map .offset |> Expect.equal (Just 50)
            , \_ -> (attempt invalidRetry).observations.tree |> Expect.equal invalidRetry.observations.tree
            , \_ -> List.length retried.observations.tree.physical |> Expect.equal 1 ] ()
    , test "incomplete SHA also blocks explicit Files submission and typed terminal reads" <| \_ ->
        let
            invalid = update OpenObservationFileComposer model |> Tuple.first
                |> update (SetObservationMatchPaths "src/Main.elm") |> Tuple.first
                |> update (SetObservationHistoryGitSha "abc") |> Tuple.first
            submitted = update ApplyObservationMatch invalid |> Tuple.first
            selected = update (SelectObservationFacet Api.SubjectFile "src/Main.elm") invalid |> Tuple.first
        in Expect.all
            [ \_ -> submitted.observations.requestGeneration |> Expect.equal invalid.observations.requestGeneration
            , \_ -> selected.observations.requestGeneration |> Expect.equal invalid.observations.requestGeneration
            , \_ -> submitted.observations.expectedOffset |> Expect.equal Nothing
            , \_ -> selected.observations.expectedOffset |> Expect.equal Nothing ] ()
    , test "typing coalesces under a stamped 250ms lifetime and a retired timer cannot submit" <| \_ ->
        let
            first = update (SetObservationQuery "old") model |> Tuple.first
            old = first.observations.filterDebounce
            latest = update (SetObservationQuery "latest") first |> Tuple.first
            stale = old |> Maybe.map (\stamp -> update (ObservationFiltersReady stamp) latest |> Tuple.first) |> Maybe.withDefault latest
            ready = settleFilters latest
        in Expect.all
            [ \_ -> first.observations.loading |> Expect.equal False
            , \_ -> first.observations.expectedOffset |> Expect.equal Nothing
            , \_ -> stale.observations |> Expect.equal latest.observations
            , \_ -> ready.observations.refreshPass |> Maybe.map (.query >> .query) |> Expect.equal (Just "latest")
            , \_ -> ready.observations.items |> Expect.equal model.observations.items ] ()
    , test "ordinary search edits retain physical viewport ownership while explicit clear resets it" <| \_ ->
        let
            seeded = project model model
            state = seeded.observations
            viewport = state.viewport
            positioned = { seeded | observations = { state | viewport = { viewport | top = 120 } } }
            edit message previous = update message previous |> Tuple.first |> project previous
            typed = edit (SetObservationQuery "latest") positioned
            emptied = edit (SetObservationQuery "") typed
            cleared = edit (ClearObservationSearchText False) typed
            treeMode = edit (SetObservationBrowseMode ObservationFacetMode) positioned
            treeTyped = edit (SetObservationQuery "latest") treeMode
            treeSettled = settleFilters treeTyped |> project treeTyped
            treeState = treeMode.observations
            canonical = project treeMode { treeMode | observations = { treeState | tree = ObservationTree.retire "new-canonical-generation" treeState.tree } }
        in
        Expect.equal { top = [120,120,0], preserve = [True,True,False], clear = [0,0,1], tree = [True,True,False] }
            { top = List.map (.observations >> .viewport >> .top) [typed,emptied,cleared]
            , preserve = List.map (.observations >> .viewport >> .preserveScroll) [typed,emptied,cleared]
            , clear = List.map (.observations >> .clearViewportToken) [typed,emptied,cleared]
            , tree = List.map (.observations >> .viewport >> .preserveScroll) [treeTyped,treeSettled,canonical] }
    , test "clear and selects submit immediately and retire an outstanding text timer" <| \_ ->
        let
            typed = update (SetObservationQuery "latest") model |> Tuple.first
            cleared = update (SetObservationQuery "") typed |> Tuple.first
            selected = update (SetObservationSubjectKind "glob") typed |> Tuple.first
        in Expect.all
            [ \_ -> cleared.observations.filterDebounce |> Expect.equal Nothing
            , \_ -> cleared.observations.expectedOffset |> Expect.equal (Just 0)
            , \_ -> selected.observations.filterDebounce |> Expect.equal Nothing
            , \_ -> O.listQuery workspaceId 0 selected.observations |> .subjectKind |> Expect.equal (Just Api.SubjectGlob)
            , \_ -> O.listQuery workspaceId 0 selected.observations |> .query |> Expect.equal (Just "latest") ] ()
    , test "incomplete SHA input remains local with last-good rows and canonical invalidation waits for completion" <| \_ ->
        let
            editing = update (SetObservationGitSha "abc") model |> Tuple.first
            invalidated = O.refreshActiveResults editing |> Tuple.first |> O.syncCounts |> Tuple.first
            completed = update (SetObservationGitSha (String.repeat 40 "a")) invalidated |> Tuple.first
            current = update (SetObservationCurrentGitSha (String.repeat 40 "b")) completed |> Tuple.first
            history = update (SetObservationHistoryGitSha (String.repeat 40 "c")) current |> Tuple.first
            query = O.listQuery workspaceId 0 history.observations
        in Expect.all
            [ \_ -> (invalidated.observations.loading, invalidated.observations.expectedOffset, invalidated.observations.counts.active) |> Expect.equal (False, Nothing, Nothing)
            , \_ -> invalidated.observations.orderedIds |> Expect.equal model.observations.orderedIds
            , \_ -> O.filterValidationError invalidated.observations |> (/=) Nothing |> Expect.equal True
            , \_ -> O.viewObservationsState TimestampDisplay.empty workspaceFixture invalidated.observations |> Query.fromHtml |> Query.has [ Selector.text "Enter a full lowercase 40-character Git SHA, or clear the field." ]
            , \_ -> O.viewObservationsState TimestampDisplay.empty workspaceFixture invalidated.observations |> Query.fromHtml |> Query.hasNot [ Selector.text "Counting matching Observations…" ]
            , \_ -> (query.gitSha, query.currentGitSha, query.historyGitSha) |> Expect.equal (Just (String.repeat 40 "a"), Just (String.repeat 40 "b"), Just (String.repeat 40 "c"))
            , \_ -> history.observations.expectedOffset |> Expect.equal (Just 0) ] ()
    , test "old timers fail after workspace mode and authority changes" <| \_ ->
        let
            typed = update (SetObservationQuery "old") model |> Tuple.first
            stamp = typed.observations.filterDebounce
            mode = update (SetObservationBrowseMode ObservationFacetMode) typed |> Tuple.first
            workspace = { typed | selectedWorkspaceId = Just "other" }
            authority = { typed | sessionRequestEpoch = 4 }
            deliver current = stamp |> Maybe.map (\value -> update (ObservationFiltersReady value) current |> Tuple.first) |> Maybe.withDefault current
        in Expect.all
            [ \_ -> (deliver mode).observations |> Expect.equal mode.observations
            , \_ -> (deliver workspace).observations |> Expect.equal workspace.observations
            , \_ -> (deliver authority).observations |> Expect.equal authority.observations ] ()
    , test "All Tree and Files retain independent text and manual exact subject never filters other modes" <| \_ ->
        let
            all = update (SetObservationQuery "content token") model |> Tuple.first |> settleFilters
            exact = update (SetObservationSubject "src/Manual.elm") all |> Tuple.first |> settleFilters
            tree = update (SetObservationBrowseMode ObservationFacetMode) exact |> Tuple.first
                |> update (SetObservationQuery "src/auth") |> Tuple.first |> settleFilters
            files = update OpenObservationFileComposer tree |> Tuple.first
                |> update (SetObservationQuery "files token") |> Tuple.first |> settleFilters
                |> update (SetObservationMatchPaths "src/One.elm\nsrc/Two.elm\nsrc/One.elm") |> Tuple.first
            matched = update ApplyObservationMatch files |> Tuple.first
            back = update (SetObservationBrowseMode ObservationFlatMode) matched |> Tuple.first
            treeBack = update (SetObservationBrowseMode ObservationFacetMode) back |> Tuple.first
            fileBack = update OpenObservationFileComposer treeBack |> Tuple.first
        in Expect.all
            [ \_ -> back.observations.query |> Expect.equal "content token"
            , \_ -> treeBack.observations.query |> Expect.equal "src/auth"
            , \_ -> fileBack.observations.query |> Expect.equal "files token"
            , \_ -> fileBack.observations.matchPathsInput |> Expect.equal files.observations.matchPathsInput
            , \_ -> (O.listQuery workspaceId 0 back.observations).subject |> Expect.equal (Just "src/Manual.elm")
            , \_ -> (O.listQuery workspaceId 0 treeBack.observations).subject |> Expect.equal Nothing
            , \_ -> (O.listQuery workspaceId 0 fileBack.observations).subject |> Expect.equal Nothing
            , \_ -> matched.observations.matchAppliedPaths |> Expect.equal [ "src/One.elm", "src/Two.elm" ] ] ()
    , test "retained Files paths never make All or Tree URL tuples invalid" <| \_ ->
        let
            files = update OpenObservationFileComposer model |> Tuple.first
                |> update (SetObservationMatchPaths "src/Main.elm") |> Tuple.first
                |> update ApplyObservationMatch |> Tuple.first
            all = update (SetObservationBrowseMode ObservationFlatMode) files |> Tuple.first
            tree = update (SetObservationBrowseMode ObservationFacetMode) files |> Tuple.first
            roundtrip current = Helpers.encodeObservationQuery (Helpers.observationAppliedQuery current.observations)
                |> Url.percentDecode |> Maybe.andThen (D.decodeString Helpers.observationQueryDecoder >> Result.toMaybe)
        in Expect.all
            [ \_ -> roundtrip all |> Maybe.map .matchAppliedPaths |> Expect.equal (Just [])
            , \_ -> roundtrip tree |> Maybe.map .matchAppliedPaths |> Expect.equal (Just [])
            , \_ -> all.observations.matchAppliedPaths |> Expect.equal [ "src/Main.elm" ] ] ()
    , test "replacement failure preserves last completed rows and retry has a fresh identity" <| \_ ->
        let
            started = update (SetObservationQuery "new") model |> Tuple.first |> settleFilters
            request = started.observations
            failed = Loading.update (GotObservations workspaceId Nothing request.requestGeneration request.queryFingerprint 0 (Err Http.NetworkError)) started |> Tuple.first
            retried = update RetryObservationResults failed |> Tuple.first
        in Expect.all
            [ \_ -> failed.observations.orderedIds |> Expect.equal model.observations.orderedIds
            , \_ -> failed.observations.refreshError |> (/=) Nothing |> Expect.equal True
            , \_ -> retried.observations.requestGeneration > request.requestGeneration |> Expect.equal True
            , \_ -> (O.listQuery workspaceId 0 retried.observations).query |> Expect.equal (Just "new") ] ()
    , test "content-only URL is additive and exact typed terminal loads omit text query" <| \_ ->
        let
            input = O.listQuery workspaceId 0 model.observations
            exact = update (SelectObservationFacet Api.SubjectGlob "src/**/*.elm") model |> Tuple.first
            state = exact.observations
            filtered = { state | subjectQuery = "subject-only text", appliedQuery = Nothing }
        in Expect.all
            [ \_ -> Api.observationContentListUrl "" input |> String.contains "query_scope=content" |> Expect.equal True
            , \_ -> Api.observationListUrl "" input |> String.contains "query_scope" |> Expect.equal False
            , \_ -> (O.listQuery workspaceId 0 filtered).query |> Expect.equal Nothing
            , \_ -> (O.treeQuery workspaceId "src/**" 2147483647 filtered).subjectQuery |> Expect.equal (Just "subject-only text") ] ()
    ]


suite : Test
suite = describe "automatic observation refresh and scoped preferences"
    [ describe "Tree default and entry precedence" defaultModeTests
    , describe "live discovery input lifetimes" liveFilterTests
    , test "Observation pages share 200 while full aggregate query ignores offsets" <| \_ ->
        Expect.equal ( [ 200, 200, 200 ], [ False, False, False ] )
            ( [ (O.listQuery workspaceId 0 model.observations).limit, (O.facetQuery workspaceId 200 model.observations).limit, (O.matchQuery workspaceId [ "src/Main.elm" ] 400 model.observations).limit ]
            , [ O.validPage 0 True [ "one" ] [], O.validPage 200 True (List.repeat 200 "one") [ "one" ], O.hasAppliedCountFilter model.observations ] )
    , test "count decoder rejects negative fractional overflow and inconsistent totals" <| \_ ->
        let value total matched = "{\"workspace_id\":\"" ++ workspaceId ++ "\",\"total_count\":" ++ total ++ ",\"match_count\":" ++ matched ++ "}" in
        Expect.equal [ True, False, False, False, False ] (List.map (\json -> D.decodeString Api.observationCountsDecoder json |> Result.toMaybe |> (/=) Nothing)
            [ value "0" "0", value "-1" "0", value "2.5" "1", value "9007199254740992" "0", value "1" "2" ])
    , describe "authoritative discovery counts" discoveryCountTests
    , test "discovery continuations omit creation predicates and generation while retaining the typed page selector" <| \_ ->
        let filters = {subjectQuery = "src/", contentQuery = "cache", subjectKind = Just Api.SubjectFile, subject = Nothing, gitSha = Nothing, currentGitSha = Nothing, historyGitSha = Nothing}
            query = {workspaceId = workspaceId, searchSession = "10000000-0000-4000-8000-000000000001", generation = 7, contextToken = Just "accepted", filters = filters, view = "list", prefix = "", terminal = Just {subjectKind = Api.SubjectFile, subject = "src/Main.elm"}, limit = 50, offset = 50}
            keys = Api.observationDiscoveryBody query |> D.decodeValue (D.keyValuePairs D.value) |> Result.map (List.map Tuple.first >> List.sort)
        in Expect.equal (Ok ["context_token","limit","offset","terminal","view","workspace_id"]) keys
    , test "discovery decoder enforces its fifty row envelope without weakening the legacy tree page" <| \_ ->
        let node = "{\"path\":\"src\",\"segment\":\"src\",\"has_children\":false,\"observation_count\":1,\"terminals\":[]}"
            tree size = "{\"items\":[" ++ String.join "," (List.repeat size node) ++ "],\"has_more\":true,\"total_count\":100,\"match_count\":100}"
            envelope size complete token mode = "{\"context_token\":\"" ++ token ++ "\",\"expires_at\":\"2099-01-01T00:00:00Z\",\"retrieval\":{\"mode\":\"" ++ mode ++ "\",\"reason\":null},\"complete\":" ++ complete ++ ",\"page\":{\"items\":[],\"has_more\":true,\"counts\":{\"workspace_id\":\"" ++ workspaceId ++ "\",\"total_count\":100,\"match_count\":100},\"tree\":" ++ tree size ++ "}}"
            accepted = D.decodeString Api.observationDiscoveryDecoder >> Result.toMaybe >> (/=) Nothing
        in Expect.equal ([True,False,False,False,False],True)
            (List.map accepted [envelope 50 "true" "token" "hybrid",envelope 51 "true" "token" "hybrid",envelope 50 "false" "token" "fts_only",envelope 50 "true" "" "fts_only",envelope 50 "true" "token" "unknown"],D.decodeString Api.observationSubjectTreeDecoder (tree 200) |> Result.toMaybe |> (/=) Nothing)
    , test "discovery accepts omitted count and list trees but rejects malformed present trees" <| \_ ->
        let
            envelope treeField = "{\"context_token\":\"token\",\"expires_at\":\"2099-01-01T00:00:00Z\",\"retrieval\":{\"mode\":\"fts_only\",\"reason\":\"empty_query\"},\"complete\":true,\"page\":{\"items\":[],\"has_more\":false,\"counts\":{\"workspace_id\":\"" ++ workspaceId ++ "\",\"total_count\":202,\"match_count\":202}" ++ treeField ++ "}}"
            accepted = envelope >> D.decodeString Api.observationDiscoveryDecoder >> Result.toMaybe >> (/=) Nothing
        in
        Expect.equal [ True, True, True, False, False, False, False ]
            (List.map accepted
                [ "", ",\"tree\":null"
                , ",\"tree\":{\"items\":[],\"has_more\":false,\"total_count\":202,\"match_count\":202}"
                , ",\"tree\":false", ",\"tree\":{}"
                , ",\"tree\":{\"items\":[],\"has_more\":false,\"total_count\":1,\"match_count\":2}"
                , ",\"tree\":{\"items\":\"invalid\",\"has_more\":false,\"total_count\":202,\"match_count\":202}"
                ])
    , test "stages demanded pages without replacing cache and commits membership once" <| \_ ->
        let
            initial = project model model
            started = O.refreshActiveResults initial |> Tuple.first
            first = page 0 (List.range 1 200 |> List.map (String.fromInt >> row)) True started
            final = page 200 [ row "201", row observationId ] False first |> project first
        in Expect.all
            [ \_ -> first.observations.orderedIds |> Expect.equal [ observationId ]
            , \_ -> first.observations.expectedOffset |> Expect.equal (Just 200)
            , \_ -> first.observations.viewport.stamp |> Expect.equal initial.observations.viewport.stamp
            , \_ -> final.observations.orderedIds |> List.length |> Expect.equal 202
            , \_ -> final.observations.refreshPass |> Expect.equal Nothing ] ()
    , test "burst retires incomplete staging and starts only one fresh pass at completion, retaining demanded depth" <| \_ ->
        let
            started = O.refreshActiveResults model |> Tuple.first
            burst = List.range 1 8 |> List.foldl (\_ current -> O.refreshActiveResults current |> Tuple.first) started
            completed = page 0 [ row "retired" ] False burst
        in Expect.all
            [ \_ -> burst.observations.requestGeneration |> Expect.equal started.observations.requestGeneration
            , \_ -> completed.observations.requestGeneration |> Expect.equal (started.observations.requestGeneration + 1)
            , \_ -> completed.observations.orderedIds |> Expect.equal [ observationId ]
            , \_ -> Maybe.map .targetOffset completed.observations.refreshPass |> Expect.equal (Just 264) ] ()
    , test "equal background results preserve semantic viewport stamp, selection, owner and disclosure" <| \_ ->
        let
            state = model.observations
            selected = { model | observations = { state | selectedId = Just observationId, selectedDetail = Just (row observationId), detailReturnTarget = Just (O.observationCardDomId "flat" observationId), expandedSubjects = Dict.singleton observationId True } }
            initial = project selected selected
            started = O.refreshActiveResults initial |> Tuple.first |> project initial
            finished = page 0 [ row observationId ] False started |> project started
        in Expect.all
            [ \_ -> started.observations.viewport.stamp |> Expect.equal initial.observations.viewport.stamp
            , \_ -> finished.observations.viewport.stamp |> Expect.equal initial.observations.viewport.stamp
            , \_ -> finished.observations.inlineOwner |> Expect.equal initial.observations.inlineOwner
            , \_ -> finished.observations.expandedSubjects |> Expect.equal initial.observations.expandedSubjects ] ()
    , test "incomplete or failed automatic refresh preserves old cache with fresh retry and no silent retry" <| \_ ->
        let
            started = O.refreshActiveResults model |> Tuple.first
            incomplete = page 0 [] True started
            failed = O.failResultPage workspaceId 0 "HTTP failed" started.observations
            quiet = O.continueAutomaticRefresh { model | observations = failed } |> Tuple.first
            retry = update RetryObservationResults quiet |> Tuple.first
        in Expect.all
            [ \_ -> incomplete.observations.orderedIds |> Expect.equal [ observationId ]
            , \_ -> incomplete.observations.refreshError /= Nothing |> Expect.equal True
            , \_ -> quiet.observations.refreshPass |> Expect.equal Nothing
            , \_ -> retry.observations.expectedOffset |> Expect.equal (Just 0)
            , \_ -> Maybe.map .targetOffset retry.observations.refreshPass |> Expect.equal (Just 264) ] ()
    , test "canonical newer proof cannot be replaced by an older staged page" <| \_ ->
        let
            started = O.refreshActiveResults model |> Tuple.first
            old = row observationId
            canonical = { old | content = "newer", contentVersion = "00000000-0000-4000-8000-000000000004", updatedAt = "2026-01-02T00:00:00Z" }
            state = O.applyCanonicalObservation canonical started.observations
            completed = page 0 [ old ] False { started | observations = state }
        in Dict.get observationId completed.observations.items |> Maybe.map .content |> Expect.equal (Just "newer")
    , test "unified SearchInput retains accepted results and response ownership uses submitted query" <| \_ ->
        let
            search = model.search
            base = { model | search = { search | globalQuery = "submitted" } }
            submitted = Search.update SubmitSearch base |> Tuple.first
            drafted = Search.update (SearchInput "different draft") submitted |> Tuple.first
            request = drafted.search.activeRequest |> Maybe.map .token |> Maybe.withDefault -1
            results = { observations = [], projects = [], tasks = [] }
            accepted = Search.update (GotUnifiedSearchResults workspaceId 3 request "submitted" (Ok results)) drafted |> Tuple.first
            refreshed = Search.refreshAcceptedSearch accepted |> Tuple.first
        in Expect.all
            [ \_ -> drafted.search.activeRequest |> Expect.equal submitted.search.activeRequest
            , \_ -> accepted.search.unifiedResults |> Expect.equal (Just results)
            , \_ -> Maybe.map .query refreshed.search.activeRequest |> Expect.equal (Just "submitted")
            , \_ -> refreshed.search.query |> Expect.equal "different draft"
            , \_ -> refreshed.search.unifiedResults |> Expect.equal (Just results) ] ()
    , test "tagged hydration restores card and independent subjects but foreign owner and local intent win" <| \_ ->
        let
            initial = project model model
            owner = initial.observations.preferenceOwner |> Maybe.withDefault { workspaceId = "", actorId = "", authority = "", runtimeId = "", epoch = 0, requestId = 0, touch = 0 }
            prefs = { preferenceDefaults | detail = Just { id = observationId, occurrence = Just (O.observationCardDomId "flat" observationId), mode = "flat" }, subjects = [ observationId ], groups = [] }
            payload supplied = E.object [ ( "owner", P.ownerValue supplied ), ( "value", P.encode owner prefs ) ]
            restored = update (ObservationPreferencesReceived (payload owner)) initial |> Tuple.first
            foreign = update (ObservationPreferencesReceived (payload { owner | epoch = 99 })) initial |> Tuple.first
            touched = update (ToggleObservationSubjects observationId) initial |> Tuple.first
            late = update (ObservationPreferencesReceived (payload owner)) touched |> Tuple.first
        in Expect.all
            [ \_ -> restored.observations.selectedId |> Expect.equal (Just observationId)
            , \_ -> restored.observations.expandedSubjects |> Expect.equal (Dict.singleton observationId True)
            , \_ -> foreign.observations.preferenceHydrated |> Expect.equal False
            , \_ -> late.observations.selectedId |> Expect.equal Nothing
            , \_ -> late.observations.expandedSubjects |> Expect.equal touched.observations.expandedSubjects ] ()
    , test "off-tab hydration retains scoped detail until first Observation entry and consumes it once" <| \_ ->
        let
            url = model.url
            projects = { model | activeTab = ProjectsTab, url = { url | fragment = Just "tab=projects" } }
            initial = project projects projects
            owner = initial.observations.preferenceOwner |> Maybe.withDefault { workspaceId = "", actorId = "", authority = "", runtimeId = "", epoch = 0, requestId = 0, touch = 0 }
            occurrence = O.observationCardDomId "flat" observationId
            prefs = { preferenceDefaults | detail = Just { id = observationId, occurrence = Just occurrence, mode = "flat" }, subjects = [ observationId ], groups = [] }
            hydrated = update (ObservationPreferencesReceived (E.object [ ( "owner", P.ownerValue owner ), ( "value", P.encode owner prefs ) ])) initial |> Tuple.first |> project initial
            entered = AppShell.handleOwned (AppShell.SwitchTabMsg ObservationsTab) hydrated |> Tuple.first |> project hydrated
            collapsed = update ReturnObservationResults entered |> Tuple.first
            again = O.restorePendingPreferences collapsed |> Tuple.first
        in Expect.all
            [ \_ -> hydrated.observations.selectedId |> Expect.equal Nothing
            , \_ -> hydrated.observations.preferencePendingDetail |> Expect.equal True
            , \_ -> entered.observations.selectedId |> Expect.equal (Just observationId)
            , \_ -> entered.observations.detailReturnTarget |> Expect.equal (Just occurrence)
            , \_ -> entered.observations.preferencePendingDetail |> Expect.equal False
            , \_ -> entered.observations.expandedSubjects |> Expect.equal (Dict.singleton observationId True)
            , \_ -> again.observations.selectedId |> Expect.equal Nothing ] ()
    , test "late tagged read after early tab entry ignores only its exact owned history and external URL still wins" <| \_ ->
        let
            url = model.url
            projects = { model | activeTab = ProjectsTab, url = { url | fragment = Just "tab=projects" } }
            initial = project projects projects
            owner = initial.observations.preferenceOwner |> Maybe.withDefault { workspaceId = "", actorId = "", authority = "", runtimeId = "", epoch = 0, requestId = 0, touch = 0 }
            prefs = { preferenceDefaults | detail = Just { id = observationId, occurrence = Nothing, mode = "flat" }, subjects = [], groups = [] }
            payload = E.object [ ( "owner", P.ownerValue owner ), ( "value", P.encode owner prefs ) ]
            entered = AppShell.handleOwned (AppShell.SwitchTabMsg ObservationsTab) initial |> Tuple.first
            ownUrl = entered.observations.preferenceEntryHistory |> Maybe.andThen Url.fromString |> Maybe.withDefault url
            afterHistory = { entered | url = ownUrl }
            beforeHistory = update (ObservationPreferencesReceived payload) entered |> Tuple.first
            settled = project beforeHistory { beforeHistory | url = ownUrl }
            late = update (ObservationPreferencesReceived payload) afterHistory |> Tuple.first
            external = update (ObservationPreferencesReceived payload) { afterHistory | url = { ownUrl | fragment = Just "tab=observations&oq=invalid" } } |> Tuple.first
            touched = update (ToggleObservationSubjects observationId) afterHistory |> Tuple.first
            superseded = update (ObservationPreferencesReceived payload) touched |> Tuple.first
        in Expect.all
            [ \_ -> late.observations.selectedId |> Expect.equal (Just observationId)
            , \_ -> beforeHistory.observations.selectedId |> Expect.equal Nothing
            , \_ -> beforeHistory.observations.preferencePendingDetail |> Expect.equal True
            , \_ -> settled.observations.selectedId |> Expect.equal (Just observationId)
            , \_ -> late.observations.preferenceEntryHistory |> Expect.equal Nothing
            , \_ -> external.observations.selectedId |> Expect.equal Nothing
            , \_ -> superseded.observations.selectedId |> Expect.equal Nothing
            , \_ -> superseded.observations.preferenceEntryHistory |> Expect.equal Nothing ] ()
    , test "pending off-tab detail is superseded by explicit URL, local preference intent and session retirement" <| \_ ->
        let
            url = model.url
            projects = { model | activeTab = ProjectsTab, url = { url | fragment = Just "tab=projects" } }
            initial = project projects projects
            owner = initial.observations.preferenceOwner |> Maybe.withDefault { workspaceId = "", actorId = "", authority = "", runtimeId = "", epoch = 0, requestId = 0, touch = 0 }
            prefs = { preferenceDefaults | detail = Just { id = observationId, occurrence = Nothing, mode = "flat" }, subjects = [], groups = [] }
            hydrated = update (ObservationPreferencesReceived (E.object [ ( "owner", P.ownerValue owner ), ( "value", P.encode owner prefs ) ])) initial |> Tuple.first |> project initial
            explicit = { hydrated | activeTab = ObservationsTab, url = { url | fragment = Just "tab=observations&oq=invalid" } } |> O.restorePendingPreferences |> Tuple.first
            touched = update (ToggleObservationSubjects observationId) hydrated |> Tuple.first
            local = AppShell.handleOwned (AppShell.SwitchTabMsg ObservationsTab) touched |> Tuple.first |> project touched
            retired = { hydrated | sessionRequestEpoch = hydrated.sessionRequestEpoch + 1 } |> project hydrated
        in Expect.all
            [ \_ -> explicit.observations.selectedId |> Expect.equal Nothing
            , \_ -> explicit.observations.preferencePendingDetail |> Expect.equal False
            , \_ -> local.observations.selectedId |> Expect.equal Nothing
            , \_ -> local.observations.preferencePendingDetail |> Expect.equal False
            , \_ -> retired.observations.preferencePendingDetail |> Expect.equal False
            , \_ -> retired.observations.preferenceValue |> Expect.equal P.empty ] ()
    , test "preference codec rejects invalid UUID, foreign scope and excessive counts, deterministic eviction keeps canonical IDs" <| \_ ->
        let
            owner = { workspaceId = workspaceId, actorId = "actor", authority = "local", runtimeId = "runtime", epoch = 3, requestId = 1, touch = 0 }
            invalid = P.encode owner P.empty |> E.encode 0 |> String.replace workspaceId "foreign"
            prefs = { preferenceDefaults | detail = Just { id = "invalid", occurrence = Nothing, mode = "flat" }, subjects = [ observationId, observationId, "invalid" ], groups = [] }
        in Expect.all
            [ \_ -> P.normalize prefs |> Expect.equal { preferenceDefaults | detail = Nothing, subjects = [ observationId ], groups = [] }
            , \_ -> D.decodeString (P.decoder owner) invalid |> Result.toMaybe |> Expect.equal Nothing
            , \_ -> P.uuid "00000000-0000-4000-8000-000000000002" |> Expect.equal True ] ()
    , test "tree invalidations coalesce behind physical completion and errors pause until a fresh retry" <| \_ ->
        let
            base = update (SetObservationBrowseMode ObservationFacetMode) model |> Tuple.first
            started = O.refreshActiveResults base |> Tuple.first
            invalidated = O.refreshActiveResults started |> Tuple.first
            complete current result = List.head current.observations.tree.physical |> Maybe.map (\request -> update (GotObservationTree request result) current |> Tuple.first |> project current) |> Maybe.withDefault current
            restarted = complete invalidated (Ok {items = [], hasMore = False, totalCount = 0, matchCount = 0})
            failed = complete restarted (Err Http.NetworkError)
            retry = update (LoadObservationTreePage "" Nothing) failed |> Tuple.first
        in Expect.all
            [ \_ -> List.length invalidated.observations.tree.physical |> Expect.equal 1
            , \_ -> restarted.observations.tree.totals |> Expect.equal Nothing
            , \_ -> Dict.get "" failed.observations.tree.branches |> Maybe.andThen .error |> (/=) Nothing |> Expect.equal True
            , \_ -> List.head retry.observations.tree.physical |> Maybe.map .token |> Expect.equal (List.head restarted.observations.tree.physical |> Maybe.map (.token >> (+) 1)) ] ()
    , test "actual canonical, deletion and unloaded invalidation retire active staging without a collection event; equal canonical is no-op" <| \_ ->
        let
            started = O.refreshActiveResults model |> Tuple.first
            old = row observationId
            fresh = { old | content = "changed", updatedAt = "2026-01-02T00:00:00Z" }
            states = [ O.applyCanonicalObservation fresh started.observations, O.removeObservation observationId started.observations, O.markResultsStale started.observations ]
            complete state = page 0 [ row "must-not-be-committed" ] False { started | observations = state }
            completions = List.map complete states
        in Expect.all
            [ \_ -> completions |> List.all (\current -> current.observations.requestGeneration == started.observations.requestGeneration + 1 && not (Dict.member "must-not-be-committed" current.observations.items)) |> Expect.equal True
            , \_ -> O.applyCanonicalObservation old started.observations |> Expect.equal started.observations ] ()
    , test "Search response from former actor epoch cannot match a reused workspace query token" <| \_ ->
        let
            search = model.search
            first = Search.update SubmitSearch { model | search = { search | globalQuery = "same" } } |> Tuple.first
            replacement = Search.update SubmitSearch { model | sessionRequestEpoch = 4, search = { search | globalQuery = "same" } } |> Tuple.first
            oldToken = Maybe.map .token first.search.activeRequest |> Maybe.withDefault -1
            result = { observations = [], projects = [], tasks = [] }
            late = Search.update (GotUnifiedSearchResults workspaceId 3 oldToken "same" (Ok result)) replacement |> Tuple.first
            accepted = Search.update (GotUnifiedSearchResults workspaceId 4 oldToken "same" (Ok result)) replacement |> Tuple.first
        in Expect.all [ \_ -> late.search |> Expect.equal replacement.search, \_ -> accepted.search.unifiedResults |> Expect.equal (Just result) ] ()
    , test "parsed explicit valid-filter, malformed oq-only, excluded and encoded URL intents win over stored card" <| \_ ->
        let
            attempt fragment =
                let
                    url = model.url
                    initial = { model | url = { url | fragment = Just fragment } } |> (\current -> project current current)
                    owner = initial.observations.preferenceOwner |> Maybe.withDefault { workspaceId = "", actorId = "", authority = "", runtimeId = "", epoch = 0, requestId = 0, touch = 0 }
                    prefs = { preferenceDefaults | detail = Just { id = observationId, occurrence = Nothing, mode = "flat" }, subjects = [ observationId ], groups = [] }
                in update (ObservationPreferencesReceived (E.object [ ( "owner", P.ownerValue owner ), ( "value", P.encode owner prefs ) ])) initial |> Tuple.first
            cases = [ "tab=observations&oq=invalid", "tab=observations&ox=excluded", "tab=observations&o%76=1&oq=invalid", "tab=observations&ov=1&oq=" ++ Url.percentEncode "[\"flat\",\"filter\",null,\"\",\"\",null,null,[]]" ]
        in List.map attempt cases |> List.all (\current -> current.observations.selectedId == Nothing && Dict.get observationId current.observations.expandedSubjects == Just True) |> Expect.equal True
    , test "full reload starter retires old staged query and guards new page completion" <| \_ ->
        let
            started = O.refreshActiveResults model |> Tuple.first
            oldState = started.observations
            applied = Helpers.observationAppliedQuery oldState
            changed = Helpers.restoreObservationQuery { applied | query = "different" } oldState
            replacement = O.restoreRouteResults { started | observations = changed } |> Tuple.first
            late = Loading.update (GotObservations workspaceId Nothing oldState.requestGeneration oldState.queryFingerprint 0 (Ok { items = [ row "stale" ], hasMore = False })) replacement |> Tuple.first
            finished = page 0 [ row "fresh" ] False late
        in Expect.all
            [ \_ -> replacement.observations.refreshPass |> Expect.equal Nothing
            , \_ -> late.observations |> Expect.equal replacement.observations
            , \_ -> finished.observations.orderedIds |> Expect.equal [ "fresh" ]
            , \_ -> finished.observations.loading |> Expect.equal False ] ()
    , test "same explicit URL UUID restores only validated occurrence hint, different ID has priority" <| \_ ->
        let
            occurrence = O.observationCardDomId "repeated" observationId
            attempt selected =
                let
                    url = model.url
                    state = model.observations
                    current = { model | url = { url | fragment = Just ("tab=observations&observation=" ++ selected) }, observations = { state | selectedId = Just selected, selectedDetail = Just (row selected) } }
                    initial = project current current
                    owner = initial.observations.preferenceOwner |> Maybe.withDefault { workspaceId = "", actorId = "", authority = "", runtimeId = "", epoch = 0, requestId = 0, touch = 0 }
                    prefs = { preferenceDefaults | detail = Just { id = observationId, occurrence = Just occurrence, mode = "flat" }, subjects = [], groups = [] }
                in update (ObservationPreferencesReceived (E.object [ ( "owner", P.ownerValue owner ), ( "value", P.encode owner prefs ) ])) initial |> Tuple.first
            same = attempt observationId
            other = attempt "00000000-0000-4000-8000-000000000099"
        in Expect.all
            [ \_ -> same.observations.detailReturnTarget |> Expect.equal (Just occurrence)
            , \_ -> other.observations.detailReturnTarget |> Expect.equal Nothing
            , \_ -> other.observations.selectedId |> Expect.equal (Just "00000000-0000-4000-8000-000000000099") ] ()
    ]


-- Flat discovery deliberately emits the existing data-loading message. Deliver
-- that owned callback too, exactly as the production router does after HTTP.
receiveDiscovery request result current =
    let accepted = O.update (GotObservationDiscovery request result) current |> Tuple.first
        settled = case request.destination of
            DiscoveryFlat token generation fingerprint offset -> Loading.update (GotObservations request.workspaceId token generation fingerprint offset (if accepted.observations.discovery.error /= Nothing then Err Http.NetworkError else Result.map (\value -> { items = value.page.items, hasMore = value.page.hasMore }) result)) accepted |> Tuple.first
            _ -> accepted
    in project current settled


discoveryPage total matches request =
    { contextToken = Maybe.withDefault ("counts-context-" ++ String.fromInt request.generation) request.contextToken, expiresAt = "2099-01-01T00:00:00Z"
    , retrieval = { mode = "fts_only", reason = Just "disabled" }, complete = True
    , page = { items = [], hasMore = False, counts = {workspaceId = workspaceId, totalCount = total, matchCount = matches}
        , tree = case request.destination of
            DiscoveryTree tree -> if tree.kind == Nothing then Just {items = [], hasMore = False, totalCount = total, matchCount = matches} else Nothing
            _ -> Nothing } }


bootstrap current = O.refreshActiveResults current |> Tuple.first
acceptBootstrap total matches current = current.observations.discovery.bootstrap |> Maybe.map (\request -> receiveDiscovery request (Ok (discoveryPage total matches request)) current) |> Maybe.withDefault current
applySearch term current = update (SetObservationQuery term) current |> Tuple.first |> settleFilters


discoveryCountTests =
    [ test "same-pair logical hydration shares one physical bootstrap and settles the latest page plus superseded initial load" <| \_ ->
        let initial = O.bootstrapObservation 17 model |> Tuple.first
            loading = initial.dataLoading
            obligation = {workspaceId = workspaceId, sessionEpoch = 3, token = 17, generation = initial.observations.requestGeneration, fingerprint = initial.observations.queryFingerprint}
            started = {initial | dataLoading = {loading | activeWorkspaceLoadToken = Just 17, initialObservationLoad = Just obligation}}
            hydrated = O.restoreRouteResults started |> Tuple.first
            completed = completeLogicalHandoff hydrated
            failed = finishLogicalHandoff True hydrated
            retried = update RetryObservationResults failed |> Tuple.first
            inverseStarted = O.restoreRouteResults model |> Tuple.first
            inverse = O.bootstrapObservation 17 inverseStarted |> Tuple.first
            inverseLoading = inverse.dataLoading
            inverseObligation = {obligation | generation = inverse.observations.requestGeneration, fingerprint = inverse.observations.queryFingerprint}
            inverseCompleted = completeLogicalHandoff {inverse | dataLoading = {inverseLoading | activeWorkspaceLoadToken = Just 17, initialObservationLoad = Just inverseObligation}}
        in Expect.all
            [ \_ -> Expect.equal started.observations.discovery.physical hydrated.observations.discovery.physical
            , \_ -> Expect.equal True (hydrated.observations.discovery.afterBootstrap /= Nothing)
            , \_ -> Expect.equal (False,["latest-hydrated"],Nothing) (completed.observations.loading,completed.observations.orderedIds,completed.dataLoading.initialObservationLoad)
            , \_ -> Expect.equal [] completed.observations.discovery.physical
            , \_ -> Expect.equal (False,Nothing) (failed.observations.loading,failed.observations.discovery.afterBootstrap)
            , \_ -> Expect.equal [True] (List.map (\request -> case request.destination of
                DiscoveryFlat _ _ _ _ -> True
                _ -> False) retried.observations.discovery.physical)
            , \_ -> Expect.equal (False,["latest-hydrated"],Nothing) (inverseCompleted.observations.loading,inverseCompleted.observations.orderedIds,inverseCompleted.dataLoading.initialObservationLoad) ] ()
    , test "a failed count continuation remains retryable from the visible search without a queued page" <| \_ ->
        let accepted = bootstrap model |> acceptBootstrap 300 9
            started = O.syncCounts {accepted | observations = O.invalidateCounts accepted.observations} |> Tuple.first
            failed = List.head started.observations.discovery.physical |> Maybe.map (\request -> receiveDiscovery request (Err Http.NetworkError) started) |> Maybe.withDefault started
            retried = update RetryObservationResults failed |> Tuple.first
        in Expect.all
            [ \_ -> Expect.equal True failed.observations.discovery.failedCount
            , \_ -> Expect.equal Nothing failed.observations.discovery.afterBootstrap
            , \_ -> Expect.equal 1 (List.length retried.observations.discovery.physical)
            , \_ -> Expect.equal accepted.observations.orderedIds retried.observations.orderedIds
            , \_ -> Expect.equal Nothing retried.observations.discovery.error ] ()
    , test "visible invalidation waits for the logical page callback before admitting a count successor" <| \_ ->
        let started = bootstrap model
            invalid = {started | observations = O.invalidateCounts started.observations}
            gap = started.observations.discovery.bootstrap |> Maybe.map (\request -> O.update (GotObservationDiscovery request (Ok (discoveryPage 999 999 request))) invalid |> Tuple.first) |> Maybe.withDefault invalid
            reconciled = project invalid gap
            completed = started.observations.discovery.bootstrap |> Maybe.map (\request -> receiveDiscovery request (Ok (discoveryPage 300 9 request)) invalid) |> Maybe.withDefault invalid
        in Expect.all
            [ \_ -> Expect.equal True gap.observations.loading
            , \_ -> Expect.equal [] reconciled.observations.discovery.physical
            , \_ -> Expect.equal Nothing reconciled.observations.workspaceCounts.value
            , \_ -> Expect.equal True reconciled.observations.workspaceCounts.pending
            , \_ -> Expect.equal 1 (List.length completed.observations.discovery.physical) ] ()
    , test "a workspace header retry after a visible bootstrap failure owns the required page" <| \_ ->
        let started = bootstrap model
            failed = started.observations.discovery.bootstrap |> Maybe.map (\request -> receiveDiscovery request (Err Http.NetworkError) started) |> Maybe.withDefault started
            retried = update RetryWorkspaceObservationCounts failed |> Tuple.first
        in Expect.all
            [ \_ -> Expect.equal True (retried.observations.discovery.bootstrap /= Nothing)
            , \_ -> Expect.equal 1 (List.length retried.observations.discovery.physical)
            , \_ -> Expect.equal True retried.observations.loading ] ()
    , test "accepted bootstrap total survives Tree filters and Files entry with canonical owner" <| \_ ->
        let accepted = bootstrap model |> acceptBootstrap 300 300
            tree = update (SetObservationBrowseMode ObservationFacetMode) accepted |> Tuple.first |> acceptBootstrap 300 300
            filtered = update (SetObservationQuery "needle") tree |> Tuple.first
            files = update (SetObservationBrowseMode ObservationMatchMode) filtered |> Tuple.first |> O.syncCounts |> Tuple.first
        in Expect.all
            [ \_ -> Expect.equal (Just 300) (Maybe.map .totalCount accepted.observations.workspaceCounts.value)
            , \_ -> Expect.equal (Just workspaceId) (Maybe.map .workspaceId accepted.observations.workspaceCounts.owner)
            , \_ -> Expect.equal (True, Just 300) (files.observations.workspaceCounts.current, Maybe.map .totalCount files.observations.workspaceCounts.value)
            , \_ -> Expect.equal Nothing files.observations.workspaceCounts.active ] ()
    , test "foreign bootstrap counts fail truthfully and retry uses a fresh keyed generation" <| \_ ->
        let started = bootstrap model
            foreignReply request =
                let
                    value = discoveryPage 100 2 request
                    pageValue = value.page
                in receiveDiscovery request (Ok {value | page = {pageValue | counts = {workspaceId = "foreign", totalCount = 100, matchCount = 2}}}) started
            failed = started.observations.discovery.bootstrap |> Maybe.map foreignReply |> Maybe.withDefault started
            retry = update RetryObservationResults failed |> Tuple.first
        in Expect.all
            [ \_ -> Expect.equal Nothing failed.observations.workspaceCounts.value
            , \_ -> Expect.equal True (failed.observations.discovery.error /= Nothing)
            , \_ -> Expect.equal True ((Maybe.map .generation retry.observations.discovery.bootstrap |> Maybe.withDefault 0) > (Maybe.map .generation started.observations.discovery.bootstrap |> Maybe.withDefault 0)) ] ()
    , test "bootstrap counts cannot revive across actor epoch workspace or read authority" <| \_ ->
        let started = bootstrap model
            session = started.sessionContext |> Maybe.withDefault { authMode = "local", effectiveActor = Nothing, principal = { actorType = "user", actorId = "actor", actorLabel = "Actor", authority = "local", grantUserId = Nothing }, globalPermissions = {createWorkspace = False, superadmin = False}, workspace = Nothing }
            principal = session.principal
            cases = [ {started | selectedWorkspaceId = Just "other"}, {started | sessionRequestEpoch = 4}, {started | auth = {status = AuthBooting, mode = Nothing}}
                , {started | sessionContext = Just {session | principal = {principal | actorId = "other"}}}
                , {started | sessionContext = Just {session | workspace = Just {workspaceId = workspaceId, role = Just "read", canRead = False, canEdit = False, canAdmin = False}}} ]
            results = List.map (\current -> started.observations.discovery.bootstrap |> Maybe.map (\request -> receiveDiscovery request (Ok (discoveryPage 100 2 request)) current) |> Maybe.withDefault current) cases
        in Expect.equal (List.repeat 5 Nothing) (List.map (\current -> current.observations.workspaceCounts.value) results)
    , test "draft pair sends no bootstrap and applied filters retain completed count while held" <| \_ ->
        let accepted = bootstrap model |> acceptBootstrap 300 9
            draft = update (SetObservationSubjectQuery "src/") accepted |> Tuple.first
            applied = settleFilters draft
        in Expect.all
            [ \_ -> Expect.equal Nothing draft.observations.discovery.bootstrap
            , \_ -> Expect.equal True (applied.observations.discovery.bootstrap /= Nothing)
            , \_ -> Expect.equal (False, Just 9, Just 300) (applied.observations.counts.current, Maybe.map .matchCount applied.observations.counts.value, Maybe.map .totalCount applied.observations.workspaceCounts.value) ] ()
    , test "stale B counts cannot relabel A after C then B gets a new generation" <| \_ ->
        let accepted = bootstrap model |> acceptBootstrap 300 9
            b = applySearch "B" accepted
            c = applySearch "C" b
            stale = b.observations.discovery.bootstrap |> Maybe.map (\request -> receiveDiscovery request (Ok (discoveryPage 300 2 request)) c) |> Maybe.withDefault c
            again = applySearch "B" stale
        in Expect.all
            [ \_ -> Expect.equal (Just 9) (Maybe.map .matchCount again.observations.counts.value)
            , \_ -> Expect.equal True ((Maybe.map .generation again.observations.discovery.bootstrap |> Maybe.withDefault 0) > (Maybe.map .generation b.observations.discovery.bootstrap |> Maybe.withDefault 0))
            , \_ -> Expect.equal False again.observations.counts.current ] ()
    , test "superseded bootstrap success and error release only their own physical owner" <| \_ ->
        let old = bootstrap model
            latest = applySearch "latest" old
            success = old.observations.discovery.bootstrap |> Maybe.map (\request -> receiveDiscovery request (Ok (discoveryPage 99 99 request)) latest) |> Maybe.withDefault latest
            failure = old.observations.discovery.bootstrap |> Maybe.map (\request -> receiveDiscovery request (Err Http.NetworkError) success) |> Maybe.withDefault success
            accepted = acceptBootstrap 20 2 failure
        in Expect.all
            [ \_ -> Expect.equal Nothing failure.observations.counts.value
            , \_ -> Expect.equal Nothing failure.observations.discovery.error
            , \_ -> Expect.equal (Just 2) (Maybe.map .matchCount accepted.observations.counts.value)
            , \_ -> Expect.equal [] accepted.observations.discovery.physical ] ()
    , test "count-only bootstrap preserves membership and releases its exact startup credit" <| \_ ->
        let hidden = {model | activeTab = ProjectsTab}
            started = O.syncCounts hidden |> Tuple.first
            accepted = acceptBootstrap 300 9 started
        in Expect.all
            [ \_ -> Expect.equal 1 (Set.size started.dataLoading.startupAdmission.physical)
            , \_ -> Expect.equal (1,0) (accepted.dataLoading.startupAdmission.spent,Set.size accepted.dataLoading.startupAdmission.physical)
            , \_ -> Expect.equal hidden.observations.items accepted.observations.items
            , \_ -> Expect.equal hidden.observations.orderedIds accepted.observations.orderedIds
            , \_ -> Expect.equal hidden.observations.selectedId accepted.observations.selectedId
            , \_ -> Expect.equal (Just 300) (Maybe.map .totalCount accepted.observations.workspaceCounts.value) ] ()
    , test "view opening queues its own initial page without settling initialLoad from count completion" <| \_ ->
        let started = O.syncCounts {model | activeTab = ProjectsTab} |> Tuple.first
            opened = O.bootstrapObservation 17 {started | activeTab = ObservationsTab} |> Tuple.first
            loading = opened.dataLoading
            obligation = {workspaceId = workspaceId, sessionEpoch = 3, token = 17, generation = opened.observations.requestGeneration, fingerprint = opened.observations.queryFingerprint}
            queued = {opened | dataLoading = {loading | initialObservationLoad = Just obligation}}
            accepted = acceptBootstrap 300 9 queued
            pageToken request = case request.destination of
                DiscoveryFlat token _ _ _ -> token
                _ -> Nothing
        in Expect.all
            [ \_ -> Expect.equal True (opened.observations.discovery.afterBootstrap /= Nothing)
            , \_ -> Expect.equal (Just obligation) accepted.dataLoading.initialObservationLoad
            , \_ -> Expect.equal [Just 17] (List.map pageToken accepted.observations.discovery.physical)
            , \_ -> Expect.equal True (List.all (\request -> request.contextToken /= Nothing) accepted.observations.discovery.physical)
            , \_ -> Expect.equal 0 (Set.size accepted.dataLoading.startupAdmission.physical) ] ()
    , test "count invalidations reject old totals and coalesce one successor without changing hidden rows" <| \_ ->
        let hidden = {model | activeTab = ProjectsTab}
            started = O.syncCounts hidden |> Tuple.first
            invalidated = {started | observations = O.invalidateCounts (O.invalidateCounts started.observations)}
            oldDone = acceptBootstrap 999 999 invalidated
            completed = List.head oldDone.observations.discovery.physical |> Maybe.map (\request -> receiveDiscovery request (Ok (discoveryPage 100 3 request)) oldDone) |> Maybe.withDefault oldDone
        in Expect.all
            [ \_ -> Expect.equal Nothing oldDone.observations.workspaceCounts.value
            , \_ -> Expect.equal 1 (List.length oldDone.observations.discovery.physical)
            , \_ -> Expect.equal (Just 100) (Maybe.map .totalCount completed.observations.workspaceCounts.value)
            , \_ -> Expect.equal hidden.observations.orderedIds completed.observations.orderedIds
            , \_ -> Expect.equal (2,0) (completed.dataLoading.startupAdmission.spent,Set.size completed.dataLoading.startupAdmission.physical) ] ()
    , test "visible count409 admits one fresh owner and late409 cannot restart it again" <| \_ ->
        let accepted = O.syncCounts {model | activeTab = ProjectsTab} |> Tuple.first |> acceptBootstrap 300 9
            invalid = {accepted | activeTab = ObservationsTab, observations = O.invalidateCounts accepted.observations}
            started = O.syncCounts invalid |> Tuple.first
            expired = List.head started.observations.discovery.physical |> Maybe.map (\request -> receiveDiscovery request (Err (Http.BadStatus 409)) started) |> Maybe.withDefault started
            repeated = List.head started.observations.discovery.physical |> Maybe.map (\request -> receiveDiscovery request (Err (Http.BadStatus 409)) expired) |> Maybe.withDefault expired
            completed = acceptBootstrap 301 10 repeated
        in Expect.all
            [ \_ -> Expect.equal True (expired.observations.discovery.bootstrap /= Nothing)
            , \_ -> Expect.equal expired.observations.discovery.generation repeated.observations.discovery.generation
            , \_ -> Expect.equal 1 (List.length repeated.observations.discovery.physical)
            , \_ -> Expect.equal (Just 301) (Maybe.map .totalCount completed.observations.workspaceCounts.value)
            , \_ -> Expect.equal model.observations.orderedIds completed.observations.orderedIds
            , \_ -> Expect.equal 0 (Set.size completed.dataLoading.startupAdmission.physical) ] ()
    , test "hidden invalidation retires a held flat staging pass without committing its old catalog" <| \_ ->
        let started = bootstrap model
            receiveRows values more request current =
                let
                    value = discoveryPage 300 300 request
                    pageValue = value.page
                in receiveDiscovery request (Ok {value | page = {pageValue | items = values, hasMore = more}}) current
            first = started.observations.discovery.bootstrap |> Maybe.map (\request -> receiveRows (List.range 1 50 |> List.map (String.fromInt >> row)) True request started) |> Maybe.withDefault started
            hidden = O.refreshActiveResults {first | activeTab = ProjectsTab} |> Tuple.first
            completed = List.head hidden.observations.discovery.physical |> Maybe.map (\request -> receiveRows [row "stale-final"] False request hidden) |> Maybe.withDefault hidden
            reopened = {completed | activeTab = ObservationsTab} |> project completed
            isFlat request = case request.destination of
                DiscoveryFlat _ _ _ _ -> True
                _ -> False
        in Expect.all
            [ \_ -> Expect.equal (Just True) (Maybe.map .invalidated hidden.observations.refreshPass)
            , \_ -> Expect.equal model.observations.orderedIds completed.observations.orderedIds
            , \_ -> Expect.equal True completed.observations.refreshPending
            , \_ -> Expect.equal True completed.observations.resultsStale
            , \_ -> Expect.equal True (List.any isFlat reopened.observations.discovery.physical) ] ()
    , test "opening the visible collection after a settled header failure remains retryable" <| \_ ->
        let started = O.syncCounts {model | activeTab = ProjectsTab} |> Tuple.first
            failed = started.observations.discovery.bootstrap |> Maybe.map (\request -> receiveDiscovery request (Err Http.NetworkError) started) |> Maybe.withDefault started
            opened = O.bootstrapObservation 17 {failed | activeTab = ObservationsTab} |> Tuple.first
            retry = update RetryObservationResults opened |> Tuple.first
        in Expect.equal True (retry.observations.discovery.bootstrap /= Nothing || List.any (\request -> case request.destination of
            DiscoveryFlat _ _ _ _ -> True
            _ -> False) retry.observations.discovery.physical)
    , test "the real tab entry recovers only a count-origin pause and admits one page before its matching URL echo" <| \_ ->
        let started = O.syncCounts {model | activeTab = ProjectsTab} |> Tuple.first
            failed = started.observations.discovery.bootstrap |> Maybe.map (\request -> receiveDiscovery request (Err (Http.BadStatus 429)) started) |> Maybe.withDefault started
            entered = AppShell.handleOwned (AppShell.SwitchTabMsg ObservationsTab) failed |> Tuple.first |> project failed
            repeated = AppShell.handleOwned (AppShell.SwitchTabMsg ObservationsTab) entered |> Tuple.first |> project entered
            failedPage = entered.observations.discovery.bootstrap |> Maybe.map (\request -> receiveDiscovery request (Err (Http.BadStatus 503)) entered) |> Maybe.withDefault entered
            retried = update RetryObservationResults failedPage |> Tuple.first
        in Expect.all
            [ \_ -> Expect.equal 1 (List.length entered.observations.discovery.physical)
            , \_ -> Expect.equal entered.observations.discovery.physical repeated.observations.discovery.physical
            , \_ -> Expect.equal False failedPage.observations.loading
            , \_ -> Expect.equal True (failedPage.observations.discovery.error /= Nothing)
            , \_ -> Expect.equal 1 (List.length retried.observations.discovery.physical) ] ()
    , test "capacity failure pauses without dropping completed counts and explicit retry owns a fresh generation" <| \_ ->
        let accepted = bootstrap model |> acceptBootstrap 300 9
            started = applySearch "retry" accepted
            failed = started.observations.discovery.bootstrap |> Maybe.map (\request -> receiveDiscovery request (Err (Http.BadStatus 429)) started) |> Maybe.withDefault started
            retry = update RetryObservationResults failed |> Tuple.first
        in Expect.all
            [ \_ -> Expect.equal (Just 9) (Maybe.map .matchCount failed.observations.counts.value)
            , \_ -> Expect.equal True (failed.observations.discovery.error /= Nothing)
            , \_ -> Expect.equal True ((Maybe.map .generation retry.observations.discovery.bootstrap |> Maybe.withDefault 0) > (Maybe.map .generation started.observations.discovery.bootstrap |> Maybe.withDefault 0)) ] ()
    ]


completeLogicalHandoff current =
    finishLogicalHandoff False current


finishLogicalHandoff failed current =
    case current.observations.discovery.bootstrap of
        Nothing -> current
        Just request ->
            let response = discoveryPage 1 1 request
                pageValue = response.page
                value = {response | page = {pageValue | items = [row "latest-hydrated"]}}
                result = if failed then Err Http.NetworkError else Ok value
                accepted = O.update (GotObservationDiscovery request result) current |> Tuple.first
                deliver destination target = case destination of
                    DiscoveryFlat token generation fingerprint offset -> Loading.update (GotObservations request.workspaceId token generation fingerprint offset (Result.map (\responseValue -> {items = responseValue.page.items,hasMore = False}) result)) target |> Tuple.first
                    _ -> target
                oldSettled = deliver request.destination accepted
                settled = current.observations.discovery.afterBootstrap |> Maybe.map (\destination -> deliver destination oldSettled) |> Maybe.withDefault oldSettled
            in project current settled


flatState =
    let initial = O.init in
    { initial | requestMode = ObservationFlatMode }


-- Real entry models retain Tree; older flat-list fixtures above opt into All.
defaultModeTests : List Test
defaultModeTests =
    [ test "originless Files clear restores the independent Tree query" <| \_ ->
        let
            initial = O.init
            source = {model | observations={initial | requestMode=ObservationMatchMode,query="Files query",matchAppliedPaths=["src/Main.elm"],browseReturn=Nothing,modeQueries=Dict.singleton "tree" "Tree query",modeSubjectQueries=Dict.singleton "tree" "src"}}
            cleared = update ClearObservationMatch source |> Tuple.first
        in Expect.equal (ObservationFacetMode,"Tree query","src") (cleared.observations.requestMode,cleared.observations.query,cleared.observations.subjectQuery)
    , test "shared fresh and malformed URL defaults are Tree" <| \_ ->
        Expect.equal (ObservationFacetMode,ObservationFacetMode,ObservationFacetMode)
            (O.init.requestMode,Helpers.defaultObservationQuery.requestMode,(Helpers.observationUrlContext {modelUrl | fragment=Just "tab=observations&ov=1&oq=broken"}).query.requestMode)
    , test "saved Flat arrives before or after the owned generated Tree echo without being overwritten" <| \_ ->
        let
            entered = earlyTreeEntry
            owner = entered.observations.preferenceOwner |> Maybe.withDefault emptyPreferenceOwner
            query = Helpers.defaultObservationQuery
            prefs = {preferenceDefaults | modeIntent=True,context=Just (Helpers.encodeObservationQuery {query | requestMode=ObservationFlatMode})}
            payload = ObservationPreferencesReceived (E.object [("owner",P.ownerValue owner),("value",P.encode owner prefs)])
            echo = entered.observations.preferenceEntryHistory |> Maybe.andThen Url.fromString |> Maybe.withDefault modelUrl
            firstRead = update payload entered |> Tuple.first
            afterEcho = Route.handleUrlChange echo firstRead |> Tuple.first
            firstEcho = Route.handleUrlChange echo entered |> Tuple.first
            afterRead = update payload firstEcho |> Tuple.first
        in Expect.equal (ObservationFlatMode,ObservationFlatMode,ObservationFlatMode)
            (firstRead.observations.requestMode,afterEcho.observations.requestMode,afterRead.observations.requestMode)
    , test "new local mode intent wins both a held read and a delayed old entry echo" <| \_ ->
        let
            entered = earlyTreeEntry
            owner = entered.observations.preferenceOwner |> Maybe.withDefault emptyPreferenceOwner
            query = Helpers.defaultObservationQuery
            prefs = {preferenceDefaults | modeIntent=True,context=Just (Helpers.encodeObservationQuery {query | requestMode=ObservationFlatMode})}
            payload = ObservationPreferencesReceived (E.object [("owner",P.ownerValue owner),("value",P.encode owner prefs)])
            echo = entered.observations.preferenceEntryHistory |> Maybe.andThen Url.fromString |> Maybe.withDefault modelUrl
            flat = update (SetObservationBrowseMode ObservationFlatMode) entered |> Tuple.first
            tree = update (SetObservationBrowseMode ObservationFacetMode) flat |> Tuple.first
            received = update payload tree |> Tuple.first
            echoed = Route.handleUrlChange echo received |> Tuple.first
        in Expect.equal (ObservationFacetMode,True) (echoed.observations.requestMode,echoed.observations.preferenceValue.modeIntent)
    , test "a consumed entry echo cannot hide explicit Back navigation after a different external URL" <| \_ ->
        let
            entered = earlyTreeEntry
            echo = entered.observations.preferenceEntryEcho |> Maybe.andThen Url.fromString |> Maybe.withDefault modelUrl
            echoed = Route.handleUrlChange echo entered |> Tuple.first
            query = Helpers.defaultObservationQuery
            flatUrl = {echo | fragment=Just ("tab=observations&ov=1&oq=" ++ Helpers.encodeObservationQuery {query | requestMode=ObservationFlatMode})}
            external = Route.handleUrlChange flatUrl echoed |> Tuple.first
            back = Route.handleUrlChange echo external |> Tuple.first
        in Expect.equal (ObservationFacetMode,Nothing,Nothing)
            (back.observations.requestMode,back.observations.preferenceEntryHistory,back.observations.preferenceEntryEcho)
    , test "one pending old echo cannot restore a newer tab or focus" <| \_ ->
        let
            entered = earlyTreeEntry
            echo = entered.observations.preferenceEntryEcho |> Maybe.andThen Url.fromString |> Maybe.withDefault modelUrl
            projects = AppShell.handleOwned (AppShell.SwitchTabMsg ProjectsTab) entered |> Tuple.first
            oldFocus = projects.focus
            focused = {projects | focus={oldFocus | focusedEntity=Just ("task","new-focus")}}
            echoed = Route.handleUrlChange echo focused |> Tuple.first
        in Expect.equal (ProjectsTab,Just ("task","new-focus"),Nothing)
            (echoed.activeTab,echoed.focus.focusedEntity,echoed.observations.preferenceEntryEcho)
    , test "saved detail hydration before the entry echo survives the real route callback" <| \_ ->
        let
            entered = earlyTreeEntry
            owner = entered.observations.preferenceOwner |> Maybe.withDefault emptyPreferenceOwner
            query = Helpers.defaultObservationQuery
            prefs = {preferenceDefaults | modeIntent=True,context=Just (Helpers.encodeObservationQuery {query | requestMode=ObservationFlatMode}),detail=Just {id=observationId,occurrence=Nothing,mode="flat"}}
            read = update (ObservationPreferencesReceived (E.object [("owner",P.ownerValue owner),("value",P.encode owner prefs)])) entered |> Tuple.first
            echo = entered.observations.preferenceEntryEcho |> Maybe.andThen Url.fromString |> Maybe.withDefault modelUrl
            routed = Route.handleUrlChange echo read |> Tuple.first |> project read
        in Expect.equal (ObservationFlatMode,Just observationId,Nothing)
            (routed.observations.requestMode,routed.observations.selectedId,routed.observations.preferenceEntryEcho)
    , test "only unmarked empty legacy Flat migrates and explicit URLs survive foreign preferences" <| \_ ->
        let
            base = project {model | observations=O.init} {model | observations=O.init}
            owner = base.observations.preferenceOwner |> Maybe.withDefault emptyPreferenceOwner
            query = Helpers.defaultObservationQuery
            apply intent content current =
                let prefs = {preferenceDefaults | modeIntent=intent,context=Just (Helpers.encodeObservationQuery {query | requestMode=ObservationFlatMode,query=content})} in
                update (ObservationPreferencesReceived (E.object [("owner",P.ownerValue owner),("value",P.encode owner prefs)])) current |> Tuple.first
            emptyLegacy = apply False "" base
            meaningful = apply False "Cache" base
            explicit = apply True "" base
            externalUrl = {modelUrl | fragment=Just ("tab=observations&ov=1&oq=" ++ Helpers.encodeObservationQuery {query | requestMode=ObservationFlatMode})}
            external = Route.handleUrlChange externalUrl base |> Tuple.first |> apply False "Cache"
        in Expect.equal [ObservationFacetMode,ObservationFlatMode,ObservationFlatMode,ObservationFlatMode]
            (List.map (.observations >> .requestMode) [emptyLegacy,meaningful,explicit,external])
    ]


modelUrl = model.url


emptyPreferenceOwner = {workspaceId="",actorId="",authority="",runtimeId="",epoch=0,requestId=0,touch=0}


earlyTreeEntry =
    let
        projects = {model | activeTab=ProjectsTab,url={modelUrl | fragment=Just "tab=projects"},observations=O.init}
        initial = project projects projects
    in AppShell.handleOwned (AppShell.SwitchTabMsg ObservationsTab) initial |> Tuple.first
