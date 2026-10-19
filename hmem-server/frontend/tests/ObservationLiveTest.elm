module ObservationLiveTest exposing (suite)

import Api
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
        state = O.startReloadForSession 3 workspaceId O.init
    in
    { base | auth = { status = AuthReady, mode = Just "local" }, sessionContext = Just session, sessionRequestEpoch = 3, selectedWorkspaceId = Just workspaceId, activeTab = ObservationsTab, workspaces = Dict.singleton workspaceId workspace
        , observations = { state | loading = False, expectedOffset = Nothing, items = Dict.singleton observationId (row observationId), orderedIds = [ observationId ], nextOffset = 264 } }


page offset values more current =
    let state = current.observations in
    Loading.update (GotObservations workspaceId Nothing state.requestGeneration state.queryFingerprint offset (Ok { items = values, hasMore = more })) current |> Tuple.first


project previous current = O.refreshViewport previous ( current, Cmd.none ) |> Tuple.first


settleFilters current =
    current.observations.filterDebounce |> Maybe.map (\stamp -> O.update (ObservationFiltersReady stamp) current |> Tuple.first) |> Maybe.withDefault current


liveFilterTests =
    [ test "scaffold and dual-kind paths select independently from native keyboard focus typed lock and expansion" <| \_ ->
        let
            step message previous = O.update message previous |> Tuple.first |> project previous
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
            step message previous = O.update message previous |> Tuple.first |> project previous
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
            step message previous = O.update message previous |> Tuple.first |> project previous
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
            hydrated = O.update (ObservationPreferencesReceived payload) initial |> Tuple.first
            state = initial.observations
            intended = {initial | observations = {state | tree = ObservationTree.revealPath {subjectKind = Api.SubjectFile, subject = "explicit/path"} (ObservationTree.select "explicit/path" Nothing state.tree)}}
            explicit = O.update (ObservationPreferencesReceived payload) intended |> Tuple.first
            (cacheStarted, cacheGuard) = ObservationTree.begin "" 0 (ObservationTree.retire "current" ObservationTree.init)
            cache = cacheGuard |> Maybe.map (\request -> ObservationTree.receive request (Ok {items = [], hasMore = False, totalCount = 0, matchCount = 0}) cacheStarted) |> Maybe.withDefault cacheStarted
            proven = O.update (ObservationPreferencesReceived payload) {initial | observations = {state | tree = cache}} |> Tuple.first
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
            step message previous = O.update message previous |> Tuple.first |> project previous
            starting = step (SetObservationBrowseMode ObservationFacetMode) model
            node path hasChildren = {path = path, segment = String.split "/" path |> List.reverse |> List.head |> Maybe.withDefault path, hasChildren = hasChildren, observationCount = if hasChildren then 201 else 1, terminals = if path == "a" then [{subjectKind = Api.SubjectFile, subject = path, observationCount = 1}] else []}
            roots = List.map (\path -> node path True) ["a", "b", "c", "d"]
            pageOf nodes more = {items = nodes, hasMore = more, totalCount = 1000, matchCount = 1000}
            rootGuard = List.head starting.observations.tree.physical
            accepted = rootGuard |> Maybe.map (\guard -> ObservationTree.receive guard (Ok (pageOf roots False)) starting.observations.tree) |> Maybe.withDefault starting.observations.tree
            children prefix = List.range 1 200 |> List.map (\index -> node (prefix ++ "/" ++ String.padLeft 3 '0' (String.fromInt index)) False)
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
            state = starting.observations
            refreshing = O.refreshActiveResults {starting | observations = {state | tree = visible}} |> Tuple.first
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
    , test "a stale root callback cannot advance the last completed query while the latest root is pending" <| \_ ->
        let
            step message previous = O.update message previous |> Tuple.first |> project previous
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
            step message previous = O.update message previous |> Tuple.first |> project previous
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
            start = O.update (SetObservationBrowseMode ObservationFacetMode) model |> Tuple.first
            node path = {path = path, segment = path, hasChildren = False, observationCount = 1, terminals = []}
            root = List.head start.observations.tree.physical |> Maybe.map (\request -> O.update (GotObservationTree request (Ok {items = [node "a"], hasMore = False, totalCount = 1, matchCount = 1})) start |> Tuple.first |> project start) |> Maybe.withDefault start
            focused = O.update (SelectObservationTreePath "a") root |> Tuple.first |> project root
            replacement = O.update (SetObservationQuery "b") focused |> Tuple.first |> settleFilters
            state = replacement.observations
            viewport = state.viewport
            outside = {replacement | observations = {state | viewport = {viewport | focus = Nothing}}}
            completed = List.head outside.observations.tree.physical |> Maybe.map (\request -> O.update (GotObservationTree request (Ok {items = [node "b"], hasMore = False, totalCount = 1, matchCount = 1})) outside |> Tuple.first) |> Maybe.withDefault outside
        in Expect.all
            [ \_ -> completed.observations.tree.focused |> Expect.equal (Just "b")
            , \_ -> completed.observations.tree.focusPending |> Expect.equal Nothing
            , \_ -> completed.observations.viewport.focus |> Expect.equal Nothing ] ()
    , test "explicit Files submission consumes its pending text timer without a duplicate late reload" <| \_ ->
        let
            files = O.update OpenObservationFileComposer model |> Tuple.first
                |> O.update (SetObservationQuery "latest text") |> Tuple.first
                |> O.update (SetObservationMatchPaths "src/Main.elm") |> Tuple.first
            submitted = O.update ApplyObservationMatch files |> Tuple.first
            late = files.observations.filterDebounce |> Maybe.map (\stamp -> O.update (ObservationFiltersReady stamp) submitted |> Tuple.first) |> Maybe.withDefault submitted
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
            completed = List.head started.observations.tree.physical |> Maybe.map (\request -> O.update (GotObservationTree request (Ok {items = [node], hasMore = False, totalCount = 1, matchCount = 1})) started |> Tuple.first |> project started) |> Maybe.withDefault started
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
            root = List.head started.observations.tree.physical |> Maybe.map (\request -> O.update (GotObservationTree request (Ok {items = [node], hasMore = False, totalCount = 1, matchCount = 1})) started |> Tuple.first |> project started) |> Maybe.withDefault started
            original = row observationId
            item = {original | subjects = [subject], subject = "a"}
            completed = List.head root.observations.tree.physical |> Maybe.map (\request -> O.update (GotObservationTreeTerminal request (Ok {items = [item], hasMore = False})) root |> Tuple.first |> project root) |> Maybe.withDefault root
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
            hydrated = O.update (ObservationPreferencesReceived (E.object [ ("owner", P.ownerValue owner), ("value", P.encode owner prefs) ])) initial |> Tuple.first
        in Expect.all
            [ \_ -> (Helpers.observationUrlContext initial.url).notice |> Expect.equal Nothing
            , \_ -> hydrated.observations.query |> Expect.equal "explicit B"
            , \_ -> (O.listQuery workspaceId 0 hydrated.observations).query |> Expect.equal (Just "explicit B")
            , \_ -> hydrated.observations.requestGeneration |> Expect.equal initial.observations.requestGeneration ] ()
    , test "terminal selection retires a text timer without releasing physical tree occupancy" <| \_ ->
        let
            tree = O.update (SetObservationBrowseMode ObservationFacetMode) model |> Tuple.first
            typed = O.update (SetObservationQuery "src") tree |> Tuple.first
            stamp = typed.observations.filterDebounce
            state = typed.observations
            physical = ObservationTree.begin "" 0 state.tree |> Tuple.first
            selected = O.update (SelectObservationFacet Api.SubjectFile "src/Main.elm") { typed | observations = { state | tree = physical } } |> Tuple.first
            late = stamp |> Maybe.map (\guard -> O.update (ObservationFiltersReady guard) selected |> Tuple.first) |> Maybe.withDefault selected
            completed = List.head late.observations.tree.physical |> Maybe.map (\request -> O.update (GotObservationTree request (Ok {items = [], hasMore = False, totalCount = 1, matchCount = 0})) late |> Tuple.first) |> Maybe.withDefault late
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
            base = { model | observations = Helpers.restoreObservationQuery { query = "", requestMode = ObservationFlatMode, subjectKind = Nothing, subject = "", selectedFacet = Nothing, gitSha = "abc", currentGitSha = "", historyGitSha = "", matchAppliedPaths = [] } state
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
            hydrated = O.update (ObservationPreferencesReceived payload) initial |> Tuple.first |> project initial
            waitingState = initial.observations
            waiting = { initial | observations = { waitingState | loading = True, expectedOffset = Just 0 } }
            deferred = O.update (ObservationPreferencesReceived payload) waiting |> Tuple.first |> project waiting
            loaded = page 0 [ row observationId ] False deferred |> project deferred
            tree = O.update (SetObservationBrowseMode ObservationFacetMode) hydrated |> Tuple.first |> project hydrated
            returned = O.update (SetObservationBrowseMode ObservationFlatMode) tree |> Tuple.first |> project tree
            state = initial.observations
            viewport = state.viewport
            oldUrl = initial.url
            detail = { initial | url = { oldUrl | fragment = Just ("tab=observations&observation=" ++ observationId) }, observations = { state | selectedId = Just observationId, viewport = { viewport | top = 120 } } }
            selected = O.update (ObservationPreferencesReceived payload) detail |> Tuple.first
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
            settled = request |> Maybe.map (\guard -> O.update (GotObservationTree guard (Ok { items = [], hasMore = False, totalCount = 0, matchCount = 0 })) next |> Tuple.first) |> Maybe.withDefault next
        in Expect.all
            [ \_ -> List.length next.observations.tree.physical |> Expect.equal 1
            , \_ -> List.length settled.observations.tree.physical |> Expect.equal 0
            , \_ -> settled.observations.tree.totals |> Expect.equal Nothing
            , \_ -> next.observations.modeQueries |> Expect.equal (Dict.singleton "flat" "") ] ()
    , test "mode scroll positions and bounded legacy Subject preferences migrate independently" <| \_ ->
        let
            original = model.observations
            viewport = original.viewport
            positioned = { model | observations = { original | viewport = { viewport | top = 400 } } }
            tree = O.update (SetObservationBrowseMode ObservationFacetMode) positioned |> Tuple.first
            treeState = tree.observations
            treeViewport = treeState.viewport
            moved = { tree | observations = { treeState | viewport = { treeViewport | top = 800 } } }
            all = O.update (SetObservationBrowseMode ObservationFlatMode) moved |> Tuple.first
            back = O.update (SetObservationBrowseMode ObservationFacetMode) all |> Tuple.first
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
            state = { original | query = "src/auth%_", gitSha = String.repeat 40 "a", currentGitSha = String.repeat 40 "b", historyGitSha = String.repeat 40 "c", appliedQuery = Nothing }
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
                let invalid = O.update (setter value) model |> Tuple.first
                    cleared = O.update (setter "") invalid |> Tuple.first
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
            step message previous = O.update message previous |> Tuple.first |> project previous
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
            filtered = O.update (SetObservationQuery "src") settled |> Tuple.first |> settleFilters
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
            started = O.update (SetObservationBrowseMode ObservationFacetMode) model |> Tuple.first
            state = started.observations
            initTree = ObservationTree.init
            (admitted, guard) = ObservationTree.beginTerminal {subjectKind = Api.SubjectFile, subject = "src/Main.elm"} 0 {initTree | lifetime = state.tree.lifetime, displayLifetime = state.tree.lifetime}
            cached = guard |> Maybe.map (\request -> ObservationTree.receiveTerminal request (Ok {items = List.range 1 200 |> List.map (String.fromInt >> row), hasMore = True}) admitted) |> Maybe.withDefault admitted
            clean = {started | observations = {state | tree = cached}}
            invalid = O.update (SetObservationHistoryGitSha "abc") clean |> Tuple.first
            typing = O.update (SetObservationQuery "pending") clean |> Tuple.first
            denied = {clean | auth = {status = AuthBooting, mode = Nothing}}
            otherMode = {clean | observations = {state | requestMode = ObservationFlatMode, tree = clean.observations.tree}}
            attempt previous = O.update (LoadObservationTreePage "src/Main.elm" (Just Api.SubjectFile)) previous |> Tuple.first
            admin = clean.workspaceAdmin
            pendingAuthority = {clean | workspaceAdmin = {admin | authorizationPending = Just {workspaceId = workspaceId, sessionEpoch = 3, sessionKey = "pending"}}}
            blocked = [invalid, typing, denied, otherMode, pendingAuthority, {clean | sessionContext = Nothing}, {clean | selectedWorkspaceId = Nothing}]
            valid = attempt clean
            failed = List.head valid.observations.tree.physical |> Maybe.map (\request -> O.update (GotObservationTreeTerminal request (Err Http.NetworkError)) valid |> Tuple.first) |> Maybe.withDefault valid
            invalidRetry = O.update (SetObservationHistoryGitSha "abc") failed |> Tuple.first
            retried = attempt failed
        in Expect.all
            [ \_ -> blocked |> List.all (\previous -> (attempt previous).observations.tree == previous.observations.tree) |> Expect.equal True
            , \_ -> List.length valid.observations.tree.physical |> Expect.equal 1
            , \_ -> List.head valid.observations.tree.physical |> Maybe.map .kind |> Expect.equal (Just (Just Api.SubjectFile))
            , \_ -> List.head valid.observations.tree.physical |> Maybe.map .offset |> Expect.equal (Just 200)
            , \_ -> (attempt invalidRetry).observations.tree |> Expect.equal invalidRetry.observations.tree
            , \_ -> List.length retried.observations.tree.physical |> Expect.equal 1 ] ()
    , test "incomplete SHA also blocks explicit Files submission and typed terminal reads" <| \_ ->
        let
            invalid = O.update OpenObservationFileComposer model |> Tuple.first
                |> O.update (SetObservationMatchPaths "src/Main.elm") |> Tuple.first
                |> O.update (SetObservationHistoryGitSha "abc") |> Tuple.first
            submitted = O.update ApplyObservationMatch invalid |> Tuple.first
            selected = O.update (SelectObservationFacet Api.SubjectFile "src/Main.elm") invalid |> Tuple.first
        in Expect.all
            [ \_ -> submitted.observations.requestGeneration |> Expect.equal invalid.observations.requestGeneration
            , \_ -> selected.observations.requestGeneration |> Expect.equal invalid.observations.requestGeneration
            , \_ -> submitted.observations.expectedOffset |> Expect.equal Nothing
            , \_ -> selected.observations.expectedOffset |> Expect.equal Nothing ] ()
    , test "typing coalesces under a stamped 250ms lifetime and a retired timer cannot submit" <| \_ ->
        let
            first = O.update (SetObservationQuery "old") model |> Tuple.first
            old = first.observations.filterDebounce
            latest = O.update (SetObservationQuery "latest") first |> Tuple.first
            stale = old |> Maybe.map (\stamp -> O.update (ObservationFiltersReady stamp) latest |> Tuple.first) |> Maybe.withDefault latest
            ready = settleFilters latest
        in Expect.all
            [ \_ -> first.observations.loading |> Expect.equal False
            , \_ -> first.observations.expectedOffset |> Expect.equal Nothing
            , \_ -> stale.observations |> Expect.equal latest.observations
            , \_ -> ready.observations.refreshPass |> Maybe.map (.query >> .query) |> Expect.equal (Just "latest")
            , \_ -> ready.observations.items |> Expect.equal model.observations.items ] ()
    , test "clear and selects submit immediately and retire an outstanding text timer" <| \_ ->
        let
            typed = O.update (SetObservationQuery "latest") model |> Tuple.first
            cleared = O.update (SetObservationQuery "") typed |> Tuple.first
            selected = O.update (SetObservationSubjectKind "glob") typed |> Tuple.first
        in Expect.all
            [ \_ -> cleared.observations.filterDebounce |> Expect.equal Nothing
            , \_ -> cleared.observations.expectedOffset |> Expect.equal (Just 0)
            , \_ -> selected.observations.filterDebounce |> Expect.equal Nothing
            , \_ -> O.listQuery workspaceId 0 selected.observations |> .subjectKind |> Expect.equal (Just Api.SubjectGlob)
            , \_ -> O.listQuery workspaceId 0 selected.observations |> .query |> Expect.equal (Just "latest") ] ()
    , test "incomplete SHA input remains local with last-good rows and canonical invalidation waits for completion" <| \_ ->
        let
            editing = O.update (SetObservationGitSha "abc") model |> Tuple.first
            invalidated = O.refreshActiveResults editing |> Tuple.first |> O.syncCounts |> Tuple.first
            completed = O.update (SetObservationGitSha (String.repeat 40 "a")) invalidated |> Tuple.first
            current = O.update (SetObservationCurrentGitSha (String.repeat 40 "b")) completed |> Tuple.first
            history = O.update (SetObservationHistoryGitSha (String.repeat 40 "c")) current |> Tuple.first
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
            typed = O.update (SetObservationQuery "old") model |> Tuple.first
            stamp = typed.observations.filterDebounce
            mode = O.update (SetObservationBrowseMode ObservationFacetMode) typed |> Tuple.first
            workspace = { typed | selectedWorkspaceId = Just "other" }
            authority = { typed | sessionRequestEpoch = 4 }
            deliver current = stamp |> Maybe.map (\value -> O.update (ObservationFiltersReady value) current |> Tuple.first) |> Maybe.withDefault current
        in Expect.all
            [ \_ -> (deliver mode).observations |> Expect.equal mode.observations
            , \_ -> (deliver workspace).observations |> Expect.equal workspace.observations
            , \_ -> (deliver authority).observations |> Expect.equal authority.observations ] ()
    , test "All Tree and Files retain independent text and manual exact subject never filters other modes" <| \_ ->
        let
            all = O.update (SetObservationQuery "content token") model |> Tuple.first |> settleFilters
            exact = O.update (SetObservationSubject "src/Manual.elm") all |> Tuple.first |> settleFilters
            tree = O.update (SetObservationBrowseMode ObservationFacetMode) exact |> Tuple.first
                |> O.update (SetObservationQuery "src/auth") |> Tuple.first |> settleFilters
            files = O.update OpenObservationFileComposer tree |> Tuple.first
                |> O.update (SetObservationQuery "files token") |> Tuple.first |> settleFilters
                |> O.update (SetObservationMatchPaths "src/One.elm\nsrc/Two.elm\nsrc/One.elm") |> Tuple.first
            matched = O.update ApplyObservationMatch files |> Tuple.first
            back = O.update (SetObservationBrowseMode ObservationFlatMode) matched |> Tuple.first
            treeBack = O.update (SetObservationBrowseMode ObservationFacetMode) back |> Tuple.first
            fileBack = O.update OpenObservationFileComposer treeBack |> Tuple.first
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
            files = O.update OpenObservationFileComposer model |> Tuple.first
                |> O.update (SetObservationMatchPaths "src/Main.elm") |> Tuple.first
                |> O.update ApplyObservationMatch |> Tuple.first
            all = O.update (SetObservationBrowseMode ObservationFlatMode) files |> Tuple.first
            tree = O.update (SetObservationBrowseMode ObservationFacetMode) files |> Tuple.first
            roundtrip current = Helpers.encodeObservationQuery (Helpers.observationAppliedQuery current.observations)
                |> Url.percentDecode |> Maybe.andThen (D.decodeString Helpers.observationQueryDecoder >> Result.toMaybe)
        in Expect.all
            [ \_ -> roundtrip all |> Maybe.map .matchAppliedPaths |> Expect.equal (Just [])
            , \_ -> roundtrip tree |> Maybe.map .matchAppliedPaths |> Expect.equal (Just [])
            , \_ -> all.observations.matchAppliedPaths |> Expect.equal [ "src/Main.elm" ] ] ()
    , test "replacement failure preserves last completed rows and retry has a fresh identity" <| \_ ->
        let
            started = O.update (SetObservationQuery "new") model |> Tuple.first |> settleFilters
            request = started.observations
            failed = Loading.update (GotObservations workspaceId Nothing request.requestGeneration request.queryFingerprint 0 (Err Http.NetworkError)) started |> Tuple.first
            retried = O.update RetryObservationResults failed |> Tuple.first
        in Expect.all
            [ \_ -> failed.observations.orderedIds |> Expect.equal model.observations.orderedIds
            , \_ -> failed.observations.refreshError |> (/=) Nothing |> Expect.equal True
            , \_ -> retried.observations.requestGeneration > request.requestGeneration |> Expect.equal True
            , \_ -> (O.listQuery workspaceId 0 retried.observations).query |> Expect.equal (Just "new") ] ()
    , test "content-only URL is additive and exact typed terminal loads omit text query" <| \_ ->
        let
            input = O.listQuery workspaceId 0 model.observations
            exact = O.update (SelectObservationFacet Api.SubjectGlob "src/**/*.elm") model |> Tuple.first
            state = exact.observations
            filtered = { state | query = "subject-only text", appliedQuery = Nothing }
        in Expect.all
            [ \_ -> Api.observationContentListUrl "" input |> String.contains "query_scope=content" |> Expect.equal True
            , \_ -> Api.observationListUrl "" input |> String.contains "query_scope" |> Expect.equal False
            , \_ -> (O.listQuery workspaceId 0 filtered).query |> Expect.equal Nothing
            , \_ -> (O.treeQuery workspaceId "src/**" 2147483647 filtered).subjectQuery |> Expect.equal (Just "subject-only text") ] ()
    ]


suite : Test
suite = describe "automatic observation refresh and scoped preferences"
    [ describe "live discovery input lifetimes" liveFilterTests
    , test "Observation pages share 200 while full aggregate query ignores offsets" <| \_ ->
        Expect.equal ( [ 200, 200, 200 ], [ False, False, False ] )
            ( [ (O.listQuery workspaceId 0 model.observations).limit, (O.facetQuery workspaceId 200 model.observations).limit, (O.matchQuery workspaceId [ "src/Main.elm" ] 400 model.observations).limit ]
            , [ O.validPage 0 True [ "one" ] [], O.validPage 200 True (List.repeat 200 "one") [ "one" ], O.hasAppliedCountFilter model.observations ] )
    , test "count decoder rejects negative fractional overflow and inconsistent totals" <| \_ ->
        let value total matched = "{\"workspace_id\":\"" ++ workspaceId ++ "\",\"total_count\":" ++ total ++ ",\"match_count\":" ++ matched ++ "}" in
        Expect.equal [ True, False, False, False, False ] (List.map (\json -> D.decodeString Api.observationCountsDecoder json |> Result.toMaybe |> (/=) Nothing)
            [ value "0" "0", value "-1" "0", value "2.5" "1", value "9007199254740992" "0", value "1" "2" ])
    , test "draft filters do not request counts and applied filters preserve total while held" <| \_ ->
        let
            started = O.syncCounts model |> Tuple.first
            guard = started.observations.counts.active |> Maybe.withDefault emptyCountGuard
            admitted = O.update (GotObservationCounts guard (Ok { workspaceId = workspaceId, totalCount = 300, matchCount = 300 })) started |> Tuple.first
            draft = O.update (SetObservationQuery "needle") admitted |> Tuple.first |> O.syncCounts |> Tuple.first
            applied = O.update ApplyObservationFilters draft |> Tuple.first |> O.syncCounts |> Tuple.first
        in
        Expect.all
            [ \_ -> Expect.equal Nothing draft.observations.counts.active
            , \_ -> Expect.equal ( False, Just 300 ) ( applied.observations.counts.current, Maybe.map .totalCount applied.observations.counts.value )
            , \_ -> O.viewObservationsState TimestampDisplay.empty workspaceFixture applied.observations |> Query.fromHtml |> Query.has [ Selector.text "Counting matching Observations…" ]
            ] ()
    , test "stale B cannot relabel A when C then B is applied while a successor is held" <| \_ ->
        let
            started = O.syncCounts model |> Tuple.first
            aGuard = started.observations.counts.active |> Maybe.withDefault emptyCountGuard
            a = O.update (GotObservationCounts aGuard (Ok { workspaceId = workspaceId, totalCount = 300, matchCount = 9 })) started |> Tuple.first
            apply term current = let draft = O.update (SetObservationQuery term) current |> Tuple.first in O.update ApplyObservationFilters draft |> Tuple.first |> O.syncCounts |> Tuple.first
            b = apply "B" a
            bGuard = b.observations.counts.active |> Maybe.withDefault emptyCountGuard
            c = apply "C" b
            staleB = O.update (GotObservationCounts bGuard (Ok { workspaceId = workspaceId, totalCount = 300, matchCount = 2 })) c |> Tuple.first
            bAgain = apply "B" staleB
        in
        Expect.all
            [ \_ -> Expect.equal a.observations.counts.valueFingerprint bAgain.observations.counts.valueFingerprint
            , \_ -> O.viewObservationsState TimestampDisplay.empty workspaceFixture bAgain.observations |> Query.fromHtml |> Query.has [ Selector.text "Counting matching Observations…" ]
            ] ()
    , test "inactive-tab invalidations coalesce and stale success dispatches one current successor" <| \_ ->
        let
            initial = O.syncCounts { model | activeTab = ProjectsTab } |> Tuple.first
            guard = initial.observations.counts.active |> Maybe.withDefault emptyCountGuard
            invalidated = { initial | observations = O.invalidateCounts (O.invalidateCounts initial.observations) } |> O.syncCounts |> Tuple.first
            successor = O.update (GotObservationCounts guard (Ok { workspaceId = workspaceId, totalCount = 99, matchCount = 99 })) invalidated |> Tuple.first
            finalGuard = successor.observations.counts.active |> Maybe.withDefault emptyCountGuard
            accepted = O.update (GotObservationCounts finalGuard (Ok { workspaceId = workspaceId, totalCount = 0, matchCount = 0 })) successor |> Tuple.first
        in
        Expect.equal ( ( False, Nothing, guard.token + 1 ), ( True, Just 0, Nothing ) )
            ( ( invalidated.observations.counts.current, successor.observations.counts.value, finalGuard.token ), ( accepted.observations.counts.current, Maybe.map .totalCount accepted.observations.counts.value, accepted.observations.counts.active ) )
    , test "count failure pauses and explicit retry receives a fresh token" <| \_ ->
        let
            started = O.syncCounts model |> Tuple.first
            guard = started.observations.counts.active |> Maybe.withDefault emptyCountGuard
            failed = O.update (GotObservationCounts guard (Err Http.NetworkError)) started |> Tuple.first |> O.syncCounts |> Tuple.first
            retry = O.update RetryObservationCounts failed |> Tuple.first
        in
        Expect.equal ( ( Nothing, False ), ( True, Just (guard.token + 1) ) ) ( ( failed.observations.counts.active, failed.observations.counts.current ), ( failed.observations.counts.error /= Nothing, Maybe.map .token retry.observations.counts.active ) )
    , test "count replies cannot cross actor epoch workspace or read revocation" <| \_ ->
        let
            started = O.syncCounts model |> Tuple.first
            guard = started.observations.counts.active |> Maybe.withDefault emptyCountGuard
            reply current = O.update (GotObservationCounts guard (Ok { workspaceId = workspaceId, totalCount = 100, matchCount = 100 })) current |> Tuple.first
            foreign = { started | selectedWorkspaceId = Just "foreign" } |> reply
            epoch = { started | sessionRequestEpoch = 4 } |> reply
            revoked = { started | auth = { status = AuthBooting, mode = Just "local" } } |> reply
            session = started.sessionContext |> Maybe.withDefault { authMode = "local", effectiveActor = Nothing, principal = { actorType = "user", actorId = "actor", actorLabel = "Actor", authority = "local", grantUserId = Nothing }, globalPermissions = { createWorkspace = False, superadmin = False }, workspace = Nothing }
            principal = session.principal
            actor = { started | sessionContext = Just { session | principal = { principal | actorId = "other-actor" } } } |> reply
            readRevoked = { started | sessionContext = Just { session | workspace = Just { workspaceId = workspaceId, role = Just "read", canRead = False, canEdit = False, canAdmin = False } } } |> reply
        in
        Expect.equal [ Nothing, Nothing, Nothing, Nothing, Nothing ] (List.map (\current -> current.observations.counts.value) [ foreign, epoch, revoked, actor, readRevoked ])
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
            retry = O.update RetryObservationResults quiet |> Tuple.first
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
            base = { model | search = { search | query = "submitted" } }
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
            restored = O.update (ObservationPreferencesReceived (payload owner)) initial |> Tuple.first
            foreign = O.update (ObservationPreferencesReceived (payload { owner | epoch = 99 })) initial |> Tuple.first
            touched = O.update (ToggleObservationSubjects observationId) initial |> Tuple.first
            late = O.update (ObservationPreferencesReceived (payload owner)) touched |> Tuple.first
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
            hydrated = O.update (ObservationPreferencesReceived (E.object [ ( "owner", P.ownerValue owner ), ( "value", P.encode owner prefs ) ])) initial |> Tuple.first |> project initial
            entered = AppShell.handleOwned (AppShell.SwitchTabMsg ObservationsTab) hydrated |> Tuple.first |> project hydrated
            collapsed = O.update ReturnObservationResults entered |> Tuple.first
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
            beforeHistory = O.update (ObservationPreferencesReceived payload) entered |> Tuple.first
            settled = project beforeHistory { beforeHistory | url = ownUrl }
            late = O.update (ObservationPreferencesReceived payload) afterHistory |> Tuple.first
            external = O.update (ObservationPreferencesReceived payload) { afterHistory | url = { ownUrl | fragment = Just "tab=observations&oq=invalid" } } |> Tuple.first
            touched = O.update (ToggleObservationSubjects observationId) afterHistory |> Tuple.first
            superseded = O.update (ObservationPreferencesReceived payload) touched |> Tuple.first
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
            hydrated = O.update (ObservationPreferencesReceived (E.object [ ( "owner", P.ownerValue owner ), ( "value", P.encode owner prefs ) ])) initial |> Tuple.first |> project initial
            explicit = { hydrated | activeTab = ObservationsTab, url = { url | fragment = Just "tab=observations&oq=invalid" } } |> O.restorePendingPreferences |> Tuple.first
            touched = O.update (ToggleObservationSubjects observationId) hydrated |> Tuple.first
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
            base = O.update (SetObservationBrowseMode ObservationFacetMode) model |> Tuple.first
            started = O.refreshActiveResults base |> Tuple.first
            invalidated = O.refreshActiveResults started |> Tuple.first
            complete current result = List.head current.observations.tree.physical |> Maybe.map (\request -> O.update (GotObservationTree request result) current |> Tuple.first |> project current) |> Maybe.withDefault current
            restarted = complete invalidated (Ok {items = [], hasMore = False, totalCount = 0, matchCount = 0})
            failed = complete restarted (Err Http.NetworkError)
            retry = O.update (LoadObservationTreePage "" Nothing) failed |> Tuple.first
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
            first = Search.update SubmitSearch { model | search = { search | query = "same" } } |> Tuple.first
            replacement = Search.update SubmitSearch { model | sessionRequestEpoch = 4, search = { search | query = "same" } } |> Tuple.first
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
                in O.update (ObservationPreferencesReceived (E.object [ ( "owner", P.ownerValue owner ), ( "value", P.encode owner prefs ) ])) initial |> Tuple.first
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
                in O.update (ObservationPreferencesReceived (E.object [ ( "owner", P.ownerValue owner ), ( "value", P.encode owner prefs ) ])) initial |> Tuple.first
            same = attempt observationId
            other = attempt "00000000-0000-4000-8000-000000000099"
        in Expect.all
            [ \_ -> same.observations.detailReturnTarget |> Expect.equal (Just occurrence)
            , \_ -> other.observations.detailReturnTarget |> Expect.equal Nothing
            , \_ -> other.observations.selectedId |> Expect.equal (Just "00000000-0000-4000-8000-000000000099") ] ()
    ]
