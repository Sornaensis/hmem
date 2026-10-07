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
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Types exposing (..)
import Url


workspaceId = "00000000-0000-4000-8000-000000000001"
workspaceFixture = { id = workspaceId, name = "Repository", workspaceType = Api.Repository, ghOwner = Nothing, ghRepo = Nothing, createdAt = "2026-01-01T00:00:00Z", updatedAt = "2026-01-01T00:00:00Z" }
emptyCountGuard = { workspaceId = "", sessionEpoch = -1, actor = "", token = -1, generation = -1, fingerprint = "" }
observationId = "00000000-0000-4000-8000-000000000002"


row identity =
    { id = identity, workspaceId = workspaceId, subjects = [ { subjectKind = Api.SubjectFile, subject = "src/Main.elm" } ], subjectKind = Api.SubjectFile, subject = "src/Main.elm"
    , gitSha = String.repeat 40 "a", content = "canonical", contentVersion = "00000000-0000-4000-8000-000000000003", createdAt = "2026-01-01T00:00:00Z", updatedAt = "2026-01-01T00:00:00Z" }


model =
    let
        url = { protocol = Url.Https, host = "example.invalid", port_ = Nothing, path = "/workspace/" ++ workspaceId, query = Nothing, fragment = Just "tab=observations" }
        flags = { apiUrl = "https://api.invalid", wsUrl = "wss://api.invalid", sessionId = "runtime", runtimeMode = "test", authTokenStorageKey = "auth", authTokenPresent = False, loginUrl = Nothing, logoutUrl = Nothing }
        base = AppShell.initModel Nothing url (WorkspacePage workspaceId) flags Nothing (Helpers.parseFragment url.fragment) |> AppShell.finalizeInit (WorkspacePage workspaceId)
        workspace = { id = workspaceId, name = "Repository", workspaceType = Api.Repository, ghOwner = Nothing, ghRepo = Nothing, createdAt = "2026-01-01T00:00:00Z", updatedAt = "2026-01-01T00:00:00Z" }
        session = { authMode = "local", principal = { actorType = "user", actorId = "actor", actorLabel = "Actor", authority = "local", grantUserId = Nothing }, globalPermissions = { createWorkspace = False, superadmin = False }
            , workspace = Just { workspaceId = workspaceId, role = Just "owner", canRead = True, canEdit = True, canAdmin = True } }
        state = O.startReloadForSession 3 workspaceId O.init
    in
    { base | auth = { status = AuthReady, mode = Just "local" }, sessionContext = Just session, sessionRequestEpoch = 3, selectedWorkspaceId = Just workspaceId, activeTab = ObservationsTab, workspaces = Dict.singleton workspaceId workspace
        , observations = { state | loading = False, expectedOffset = Nothing, items = Dict.singleton observationId (row observationId), orderedIds = [ observationId ], nextOffset = 264 } }


page offset values more current =
    let state = current.observations in
    Loading.update (GotObservations workspaceId Nothing state.requestGeneration state.queryFingerprint offset (Ok { items = values, hasMore = more })) current |> Tuple.first


project previous current = O.refreshViewport previous ( current, Cmd.none ) |> Tuple.first


suite : Test
suite = describe "automatic observation refresh and scoped preferences"
    [ test "Observation pages share 200 while full aggregate query ignores offsets" <| \_ ->
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
            , \_ -> Expect.equal ( True, Just 300 ) ( applied.observations.counts.current, Maybe.map .totalCount applied.observations.counts.value )
            , \_ -> O.viewObservationsState workspaceFixture applied.observations |> Query.fromHtml |> Query.has [ Selector.text "Counting matching Observations…" ]
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
            , \_ -> O.viewObservationsState workspaceFixture bAgain.observations |> Query.fromHtml |> Query.has [ Selector.text "Counting matching Observations…" ]
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
            session = started.sessionContext |> Maybe.withDefault { authMode = "local", principal = { actorType = "user", actorId = "actor", actorLabel = "Actor", authority = "local", grantUserId = Nothing }, globalPermissions = { createWorkspace = False, superadmin = False }, workspace = Nothing }
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
            prefs = { detail = Just { id = observationId, occurrence = Just (O.observationCardDomId "flat" observationId), mode = "flat" }, subjects = [ observationId ], groups = [] }
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
            prefs = { detail = Just { id = observationId, occurrence = Just occurrence, mode = "flat" }, subjects = [ observationId ], groups = [] }
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
            prefs = { detail = Just { id = observationId, occurrence = Nothing, mode = "flat" }, subjects = [], groups = [] }
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
            prefs = { detail = Just { id = observationId, occurrence = Nothing, mode = "flat" }, subjects = [], groups = [] }
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
            prefs = { detail = Just { id = "invalid", occurrence = Nothing, mode = "flat" }, subjects = [ observationId, observationId, "invalid" ], groups = [] }
        in Expect.all
            [ \_ -> P.normalize prefs |> Expect.equal { detail = Nothing, subjects = [ observationId ], groups = [] }
            , \_ -> D.decodeString (P.decoder owner) invalid |> Result.toMaybe |> Expect.equal Nothing
            , \_ -> P.uuid "00000000-0000-4000-8000-000000000002" |> Expect.equal True ] ()
    , test "facet demanded span survives pass invalidation and failure then fresh Retry" <| \_ ->
        let
            state = model.observations
            base = { model | observations = { state | requestMode = ObservationFacetMode, appliedQuery = Nothing, facetNextOffset = 450 } }
            started = O.refreshActiveResults base |> Tuple.first
            invalidated = O.refreshActiveResults started |> Tuple.first
            request = invalidated.observations
            restarted = O.update (GotObservationSubjectFacets workspaceId 3 request.facetRequestGeneration request.facetFingerprint 0 (Ok { items = [], hasMore = False })) invalidated |> Tuple.first
            failure = O.failResultPage workspaceId 0 "Failed" restarted.observations
            retry = O.update RetryObservationResults { restarted | observations = failure } |> Tuple.first
        in Expect.all
            [ \_ -> Maybe.map .targetOffset restarted.observations.refreshPass |> Expect.equal (Just 450)
            , \_ -> Maybe.map .targetOffset retry.observations.refreshPass |> Expect.equal (Just 450)
            , \_ -> retry.observations.facetExpectedOffset |> Expect.equal (Just 0) ] ()
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
                    prefs = { detail = Just { id = observationId, occurrence = Nothing, mode = "flat" }, subjects = [ observationId ], groups = [] }
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
                    prefs = { detail = Just { id = observationId, occurrence = Just occurrence, mode = "flat" }, subjects = [], groups = [] }
                in O.update (ObservationPreferencesReceived (E.object [ ( "owner", P.ownerValue owner ), ( "value", P.encode owner prefs ) ])) initial |> Tuple.first
            same = attempt observationId
            other = attempt "00000000-0000-4000-8000-000000000099"
        in Expect.all
            [ \_ -> same.observations.detailReturnTarget |> Expect.equal (Just occurrence)
            , \_ -> other.observations.detailReturnTarget |> Expect.equal Nothing
            , \_ -> other.observations.selectedId |> Expect.equal (Just "00000000-0000-4000-8000-000000000099") ] ()
    ]
