module ObservationTest exposing (suite)

import Api
import Array
import AppShell
import Browser
import Dict
import Expect
import Feature.AuditLog
import Feature.DataLoading
import Feature.Observation
import Feature.Search
import Feature.WebSocket
import Helpers
import Html.Attributes exposing (attribute, hidden, tabindex)
import Http
import ObservationViewport
import Json.Decode as Decode
import Json.Encode as Encode
import Route
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Event as Event
import Test.Html.Selector as Selector
import Types exposing (AuthStatus(..), Flags, Model, Msg(..), ObservationModel, ObservationRequestMode(..), ObservationResultRow(..), Page(..), WorkspaceTab(..))
import Url


suite : Test
suite =
    describe "observation API boundary"
        [ describe "structured copy origins" structuredCopyTests
        , describe "URL reauthorization, search intent and bootstrap ownership" observationUrlReviewTests
        , describe "cached complete viewport projection" observationProjectionTests
        , describe "receipt-owned Return lifecycle" observationReturnReceiptTests
        , describe "bounded applied URL context" observationUrlTests
        , test "decodes server-shaped file and glob observations" <|
            \_ ->
                [ Decode.decodeString Api.observationDecoder fileFixture
                    |> Result.map (\observation -> ( observation.subjectKind, observation.subject, observation.gitSha ))
                , Decode.decodeString Api.observationDecoder globFixture
                    |> Result.map (\observation -> ( observation.subjectKind, observation.subject, observation.gitSha ))
                ]
                    |> Expect.equal
                        [ Ok ( Api.SubjectFile, "src/Main.elm", fullSha )
                        , Ok ( Api.SubjectGlob, "src/**/*.elm", fullSha )
                        ]
        , test "foreign live observations mark the bounded collection stale without changing membership" <|
            \_ ->
                let
                    loaded =
                        fixtureObservation "loaded" "2026-01-01T00:00:00Z"

                    initialState =
                        Feature.Observation.init

                    initial =
                        { initialState | items = Dict.singleton loaded.id loaded, orderedIds = [ loaded.id ] }

                    stale =
                        Feature.Observation.markResultsStale initial

                    view =
                        Feature.Observation.viewObservationsState (observationWorkspace Api.Repository) stale |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> Feature.Observation.isLoadedOrSelected "loaded" stale |> Expect.equal True
                    , \_ -> Feature.Observation.isLoadedOrSelected "foreign" stale |> Expect.equal False
                    , \_ -> ( stale.resultsStale, stale.orderedIds ) |> Expect.equal ( True, [ "loaded" ] )
                    , \_ -> view |> Query.hasNot [ Selector.text "Results may have changed.", Selector.text "Refresh results" ]
                    ]
                    ()
        , test "canonical selected detail never admits an unproven row to flat, facet, exact, or match membership" <|
            \_ ->
                let
                    selected =
                        fixtureObservation "selected-only" "2026-01-01T00:00:00Z"

                    canonical =
                        { selected | content = "new canonical detail", updatedAt = "2026-01-01T00:00:01Z" }

                    apply mode =
                        let
                            initial =
                                Feature.Observation.init
                        in
                        Feature.Observation.applyCanonicalObservation canonical
                            { initial
                                | requestMode = mode
                                , selectedId = Just selected.id
                                , selectedDetail = Just selected
                            }

                    states =
                        [ ObservationFlatMode, ObservationFacetMode, ObservationExactSubjectMode, ObservationMatchMode ]
                            |> List.map apply
                in
                Expect.all
                    [ \_ -> states |> List.all (\state -> Dict.isEmpty state.items && (state.selectedDetail |> Maybe.map .content) == Just "new canonical detail" && state.resultsStale) |> Expect.equal True
                    , \_ -> states |> List.all (\state -> state.orderedIds == [] && state.selectedId == Just selected.id) |> Expect.equal True
                    ]
                    ()
        , test "rejects malformed observation payloads" <|
            \_ ->
                Decode.decodeString Api.observationDecoder "{\"id\":\"observation-1\"}"
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "decodes complete paginated observation pages and preserves has_more" <|
            \_ ->
                [ Decode.decodeString (Api.paginatedDecoder Api.observationDecoder) (paginatedFixture "true")
                    |> Result.map (\page -> ( List.map .id page.items, page.hasMore ))
                , Decode.decodeString (Api.paginatedDecoder Api.observationDecoder) (paginatedFixture "false")
                    |> Result.map (\page -> ( List.map .id page.items, page.hasMore ))
                ]
                    |> Expect.equal
                        [ Ok ( [ "observation-file", "observation-glob" ], True )
                        , Ok ( [ "observation-file", "observation-glob" ], False )
                        ]
        , test "rejects missing and malformed paginated observation metadata" <|
            \_ ->
                [ "{\"items\":[" ++ fileFixture ++ "]}"
                , "{\"items\":[" ++ fileFixture ++ "],\"has_more\":\"true\"}"
                , "{\"items\":[" ++ fileFixture ++ "],\"has_more\":null}"
                ]
                    |> List.map
                        (\fixture ->
                            Decode.decodeString (Api.paginatedDecoder Api.observationDecoder) fixture
                                |> Result.toMaybe
                        )
                    |> Expect.equal [ Nothing, Nothing, Nothing ]
        , test "composes exact provenance filters, FTS, and pagination with URL encoding" <|
            \_ ->
                Api.observationListUrl "https://api.example"
                    { workspaceId = "workspace/a"
                    , subjectKind = Just Api.SubjectGlob
                    , subject = Just "src/**/*.elm"
                    , gitSha = Just fullSha
                    , query = Just "render & test"
                    , limit = 50
                    , offset = 100
                    }
                    |> Expect.equal
                        ("https://api.example/api/v1/observations?workspace_id=workspace%2Fa&subject_kind=glob&subject=src%2F**%2F*.elm&git_sha=" ++ fullSha ++ "&query=render%20%26%20test&limit=50&offset=100")
        , test "encodes Observation updates as content-only JSON" <|
            \_ ->
                Api.observationUpdateBody "revised"
                    |> Encode.encode 0
                    |> Expect.equal "{\"content\":\"revised\"}"
        , test "content versions are required opaque UUIDs on canonical observations" <|
            \_ ->
                [ fileFixture
                , String.replace "10000000-0000-4000-8000-000000000000" "not-a-uuid" fileFixture
                , String.replace "\"content_version\":\"10000000-0000-4000-8000-000000000000\"," "" fileFixture
                ]
                    |> List.map (Decode.decodeString Api.observationDecoder >> isOk)
                    |> Expect.equal [ True, False, False ]
        , test "only an actual409 with exact version-conflict code and valid canonical latest is a content conflict" <|
            \_ ->
                let
                    latest =
                        fixtureObservation "curated" "2026-01-01T00:00:00Z"

                    body code =
                        Encode.object [ ( "code", Encode.string code ), ( "latest", observationValue latest ) ] |> Encode.encode 0

                    response status payload =
                        Http.BadStatus_ { url = "http://fixture", statusCode = status, statusText = "", headers = Dict.empty } payload
                in
                Expect.all
                    [ \_ -> Api.decodeObservationUpdateResponse (response 409 (body "observation_content_conflict")) |> Expect.equal (Err (Api.ObservationContentConflict latest))
                    , \_ -> Api.decodeObservationUpdateResponse (response 403 (body "observation_content_conflict")) |> Expect.equal (Err (Api.ObservationUpdateHttpError (Http.BadStatus 403)))
                    , \_ -> Api.decodeObservationUpdateResponse (response 409 (body "other_conflict")) |> Expect.equal (Err (Api.ObservationUpdateHttpError (Http.BadStatus 409)))
                    , \_ -> Api.decodeObservationUpdateResponse (response 409 "{\"code\":\"observation_content_conflict\",\"latest\":null}") |> Expect.equal (Err (Api.ObservationUpdateHttpError (Http.BadStatus 409)))
                    , \_ -> Api.decodeObservationUpdateResponse (response 404 "{}") |> Expect.equal (Err (Api.ObservationUpdateHttpError (Http.BadStatus 404)))
                    , \_ -> Api.decodeObservationUpdateResponse Http.Timeout_ |> Expect.equal (Err (Api.ObservationUpdateHttpError Http.Timeout))
                    ] ()
        , test "delayed-notification409 preserves draft and immutable provenance, and explicit rebase saves with a new token at the same timestamp" <|
            \_ ->
                let
                    saving =
                        savingEditModel "My retained draft"

                    latest =
                        { baseline | content = "Competing writer", contentVersion = "10000000-0000-4000-8000-000000000001" }

                    baseline =
                        fixtureObservation "curated" "2026-01-01T00:00:00Z"
                in
                case saving.observations.edit |> Maybe.andThen .activeRequest of
                    Nothing -> Expect.fail "Expected owned save request"
                    Just request ->
                        let
                            conflicted =
                                Feature.Observation.update (ObservationUpdated request (Err (Api.ObservationContentConflict latest))) saving |> Tuple.first

                            rebased =
                                Feature.Observation.update RebaseObservationEdit conflicted |> Tuple.first

                            retried =
                                Feature.Observation.update SaveObservationEdit rebased |> Tuple.first

                            accepted =
                                { latest | content = "My retained draft", contentVersion = "10000000-0000-4000-8000-000000000002" }

                            completed =
                                retried.observations.edit |> Maybe.andThen .activeRequest |> Maybe.map (\next -> Feature.Observation.update (ObservationUpdated next (Ok accepted)) retried |> Tuple.first)

                            useLatest =
                                Feature.Observation.update ReloadObservationEdit conflicted |> Tuple.first
                        in
                        Expect.all
                            [ \_ -> conflicted.observations.edit |> Maybe.map (\edit -> ( edit.draft, edit.conflict, edit.saving )) |> Expect.equal (Just ( "My retained draft", True, False ))
                            , \_ -> conflicted.observations.edit |> Maybe.map .baseContentVersion |> Expect.equal (Just baseline.contentVersion)
                            , \_ -> rebased.observations.edit |> Maybe.map (\edit -> ( edit.draft, edit.baseContentVersion, edit.conflict )) |> Expect.equal (Just ( "My retained draft", latest.contentVersion, False ))
                            , \_ -> useLatest.observations.edit |> Maybe.map (\edit -> ( edit.draft, edit.baseContentVersion )) |> Expect.equal (Just ( latest.content, latest.contentVersion ))
                            , \_ -> completed |> Maybe.andThen (.observations >> .selectedDetail) |> Expect.equal (Just accepted)
                            , \_ -> completed |> Maybe.andThen (.observations >> .edit) |> Expect.equal Nothing
                            , \_ -> Feature.Observation.update (ObservationUpdated request (Err (Api.ObservationContentConflict latest))) { saving | sessionRequestEpoch = saving.sessionRequestEpoch + 1 } |> Tuple.first |> .observations |> Expect.equal saving.observations
                            ] ()
        , test "an equal-content equal-timestamp version advance remains a conflict for an owned dirty draft" <|
            \_ ->
                let
                    saving =
                        savingEditModel "Dirty content"

                    original =
                        fixtureObservation "curated" "2026-01-01T00:00:00Z"

                    next =
                        { original | contentVersion = "ffffffff-ffff-4fff-8fff-ffffffffffff" }

                    after =
                        Feature.Observation.applyCanonicalObservation next saving.observations
                in
                after.edit |> Maybe.map (\edit -> { base = edit.baseContentVersion, latest = edit.latestCanonical.contentVersion, conflict = edit.conflict, draft = edit.draft }) |> Expect.equal (Just { base = original.contentVersion, latest = next.contentVersion, conflict = True, draft = "Dirty content" })
        , test "guarded409 updates an older known token and revalidates an ambiguous equal or later observed token" <|
            \_ ->
                let
                    saving =
                        savingEditModel "Draft with known version"

                    baseline =
                        fixtureObservation "curated" "2026-01-01T00:00:00Z"

                    latest =
                        { baseline | content = "HTTP conflict canonical", contentVersion = "10000000-0000-4000-8000-000000000001", updatedAt = "2026-01-02T00:00:00Z" }

                    check timestamp =
                        let
                            known =
                                { baseline | content = "Observed canonical", contentVersion = "ffffffff-ffff-4fff-8fff-ffffffffffff", updatedAt = timestamp }

                            prepared =
                                { saving | observations = Feature.Observation.applyCanonicalObservation known saving.observations }
                        in
                        prepared.observations.edit |> Maybe.andThen .activeRequest |> Maybe.map (\owned -> Feature.Observation.update (ObservationUpdated owned (Err (Api.ObservationContentConflict latest))) prepared |> Tuple.first |> .observations |> .edit |> Maybe.map .latestCanonical |> Maybe.map .content)
                in
                [ check "2026-01-01T00:00:01Z", check latest.updatedAt, check "2026-01-03T00:00:00Z" ] |> Expect.equal [ Just (Just latest.content), Just (Just "Observed canonical"), Just (Just "Observed canonical") ]
        , test "conditional success fences old equal-content equal-time canonical, detail and result reads before the next edit" <|
            \_ ->
                let
                    original =
                        fixtureObservation "curated" "2026-01-01T00:00:00Z"

                    saving =
                        savingEditModel original.content

                    snapshot =
                        Feature.WebSocket.update (WsMessageReceived (observationSnapshotWire original)) saving |> Tuple.first

                    invalidated =
                        Feature.WebSocket.update (WsMessageReceived (observationInvalidationWire original.id)) snapshot |> Tuple.first

                    prepared =
                        let
                            current =
                                invalidated.observations
                        in
                        Feature.Observation.update RetryObservationDetail { invalidated | observations = { current | detailError = Just "Prior detail needs revalidation" } } |> Tuple.first

                    target =
                        "workspace:workspace-1|entity:observation:curated"

                    generation =
                        Dict.get target prepared.webSocket.targetGenerations |> Maybe.withDefault -1

                    guard =
                        { scopeKey = "workspace:workspace-1", targetKey = "entity:observation:curated", targetGeneration = generation, sessionEpoch = 3, routeWorkspace = Just "workspace-1", audienceId = "editor" }

                    accepted =
                        { original | contentVersion = "ffffffff-ffff-4fff-8fff-ffffffffffff" }
                in
                case ( prepared.observations.edit |> Maybe.andThen .activeRequest, prepared.observations.activeDetailRequest ) of
                    ( Just request, Just detail ) ->
                        let
                            completed =
                                Feature.Observation.update (ObservationUpdated request (Ok accepted)) prepared |> Tuple.first

                            lateCanonical =
                                Feature.WebSocket.update (CanonicalObservationFetched guard "workspace-1" original.id (Ok original)) completed |> Tuple.first

                            lateDetail =
                                Feature.Observation.update (GotObservationDetail detail.workspaceId detail.observationId detail.sessionEpoch detail.token (Ok original)) completed |> Tuple.first

                            latePage =
                                Feature.DataLoading.update (GotObservations "workspace-1" Nothing prepared.observations.requestGeneration prepared.observations.queryFingerprint 0 (Ok { items = [ original ], hasMore = False })) completed |> Tuple.first

                            nextEdit =
                                Feature.Observation.update StartObservationEdit lateCanonical |> Tuple.first

                            nextSave =
                                Feature.Observation.update (SetObservationDraft "Next genuine draft") nextEdit |> Tuple.first |> Feature.Observation.update SaveObservationEdit |> Tuple.first

                            freshGuard =
                                { guard | targetGeneration = generation + 2 }

                            socket =
                                completed.webSocket

                            freshModel =
                                { completed | webSocket = { socket | targetGenerations = Dict.insert target (generation + 2) socket.targetGenerations } }

                            freshCanonical =
                                { accepted | contentVersion = "00000000-0000-4000-8000-000000000003" }

                            fresh =
                                Feature.WebSocket.update (CanonicalObservationFetched freshGuard "workspace-1" original.id (Ok freshCanonical)) freshModel |> Tuple.first
                        in
                        Expect.all
                            [ \_ -> (generation > 0) |> Expect.equal True
                            , \_ -> Dict.get target completed.webSocket.targetGenerations |> Expect.equal (Just (generation + 1))
                            , \_ -> [ lateCanonical.observations, lateDetail.observations ] |> Expect.equal (List.repeat 2 completed.observations)
                            , \_ -> ( latePage.observations.selectedDetail, latePage.observations.items, latePage.observations.edit ) |> Expect.equal ( completed.observations.selectedDetail, completed.observations.items, completed.observations.edit )
                            , \_ -> latePage.observations.requestGeneration |> Expect.equal (completed.observations.requestGeneration + 1)
                            , \_ -> [ Http.BadStatus 404, Http.Timeout ] |> List.map (\error -> Feature.WebSocket.update (CanonicalObservationFetched guard "workspace-1" original.id (Err error)) completed |> Tuple.first |> .observations) |> Expect.equal (List.repeat 2 completed.observations)
                            , \_ -> completed.observations.selectedDetail |> Expect.equal (Just accepted)
                            , \_ -> Dict.get original.id completed.observations.items |> Expect.equal (Just accepted)
                            , \_ -> Dict.get original.id completed.observations.matchEvidence |> Maybe.map (.observation >> .contentVersion) |> Expect.equal (Just accepted.contentVersion)
                            , \_ -> nextSave.observations.edit |> Maybe.map .baseContentVersion |> Expect.equal (Just accepted.contentVersion)
                            , \_ -> fresh.observations.selectedDetail |> Expect.equal (Just freshCanonical)
                            ] ()
                    _ ->
                        Expect.fail "Save and detail read must both be genuinely admitted"
        , test "delayed equal-time409 cannot regress an observed non-base version and one guarded GET enables explicit choices" <|
            \_ ->
                let
                    original =
                        fixtureObservation "curated" "2026-01-01T00:00:00Z"

                    saving =
                        savingEditModel "Preserved delayed conflict draft"

                    known =
                        { original | contentVersion = "ffffffff-ffff-4fff-8fff-ffffffffffff" }

                    stale =
                        { original | contentVersion = "10000000-0000-4000-8000-000000000001" }

                    observed =
                        { saving | observations = Feature.Observation.applyCanonicalObservation known saving.observations }
                in
                case observed.observations.edit |> Maybe.andThen .activeRequest of
                    Nothing ->
                        Expect.fail "Save must remain owned after observing another canonical version"
                    Just request ->
                        let
                            checking =
                                Feature.Observation.update (ObservationUpdated request (Err (Api.ObservationContentConflict stale))) observed |> Tuple.first

                            view =
                                Feature.Observation.viewObservationsStateWithPermission True (observationWorkspace Api.Repository) checking.observations |> Query.fromHtml
                        in
                        case checking.observations.edit |> Maybe.andThen .activeCanonicalRequest of
                            Nothing ->
                                Expect.fail "Ambiguous conflict must issue one fresh owned canonical check"
                            Just refresh ->
                                let
                                    checked =
                                        Feature.Observation.update (ObservationConflictCanonicalFetched refresh known.contentVersion (Ok known)) checking |> Tuple.first

                                    rebased =
                                        Feature.Observation.update RebaseObservationEdit checked |> Tuple.first |> Feature.Observation.update SaveObservationEdit |> Tuple.first

                                    useLatest =
                                        Feature.Observation.update ReloadObservationEdit checked |> Tuple.first

                                    newer =
                                        { known | contentVersion = "00000000-0000-4000-8000-000000000003", content = "Fresh GET changed content at the same timestamp" }

                                    freshlyChanged =
                                        Feature.Observation.update (ObservationConflictCanonicalFetched refresh known.contentVersion (Ok newer)) checking |> Tuple.first

                                    intervening =
                                        { checking | observations = Feature.Observation.applyCanonicalObservation { known | contentVersion = newer.contentVersion } checking.observations }

                                    superseded =
                                        Feature.Observation.update (ObservationConflictCanonicalFetched refresh known.contentVersion (Ok known)) intervening |> Tuple.first
                                in
                                Expect.all
                                    [ \_ -> checking.observations.edit |> Maybe.map (\edit -> ( edit.draft, edit.latestCanonical.contentVersion, edit.canonicalProvisional )) |> Expect.equal (Just ( "Preserved delayed conflict draft", known.contentVersion, True ))
                                    , \_ -> checking.observations.selectedDetail |> Maybe.map .contentVersion |> Expect.equal (Just known.contentVersion)
                                    , \_ -> Feature.Observation.update RebaseObservationEdit checking |> Tuple.first |> .observations |> Expect.equal checking.observations
                                    , \_ -> Feature.Observation.update ReloadObservationEdit checking |> Tuple.first |> .observations |> Expect.equal checking.observations
                                    , \_ -> view |> Query.find [ Selector.tag "button", Selector.containing [ Selector.text "Keep my draft" ] ] |> Query.has [ Selector.disabled True ]
                                    , \_ -> view |> Query.has [ Selector.text "Checking the current version", Selector.text "Use retained version" ]
                                    , \_ -> checked.observations.edit |> Maybe.map (\edit -> ( edit.activeCanonicalRequest, edit.canonicalProvisional, edit.draft )) |> Expect.equal (Just ( Nothing, False, "Preserved delayed conflict draft" ))
                                    , \_ -> rebased.observations.edit |> Maybe.map .baseContentVersion |> Expect.equal (Just known.contentVersion)
                                    , \_ -> useLatest.observations.edit |> Maybe.map (\edit -> ( edit.baseContentVersion, edit.draft )) |> Expect.equal (Just ( known.contentVersion, known.content ))
                                    , \_ -> freshlyChanged.observations.edit |> Maybe.map .latestCanonical |> Expect.equal (Just newer)
                                    , \_ -> freshlyChanged.observations.selectedDetail |> Expect.equal (Just newer)
                                    , \_ -> superseded.observations.edit |> Maybe.map (\edit -> ( edit.latestCanonical.contentVersion, edit.canonicalProvisional, edit.draft )) |> Expect.equal (Just ( newer.contentVersion, True, "Preserved delayed conflict draft" ))
                                    , \_ -> Feature.Observation.update (ObservationUpdated request (Err (Api.ObservationContentConflict stale))) checking |> Tuple.first |> .observations |> Expect.equal checking.observations
                                    , \_ -> Feature.Observation.update (ObservationConflictCanonicalFetched refresh known.contentVersion (Ok known)) { checking | sessionRequestEpoch = checking.sessionRequestEpoch + 1 } |> Tuple.first |> .observations |> Expect.equal checking.observations
                                    , \_ -> Feature.Observation.reconcileCurationPermission { checking | sessionContext = Just readOnlySession } |> Feature.Observation.update (ObservationConflictCanonicalFetched refresh known.contentVersion (Ok known)) |> Tuple.first |> .observations |> .edit |> Expect.equal Nothing
                                    , \_ -> Feature.Observation.update CancelObservationEdit checking |> Tuple.first |> Feature.Observation.update (ObservationConflictCanonicalFetched refresh known.contentVersion (Ok known)) |> Tuple.first |> .observations |> .edit |> Expect.equal Nothing
                                    ] ()
        , test "hidden-owner ambiguity checks preserve B and a failed check permits a safe repeated conditional conflict" <|
            \_ ->
                let
                    original =
                        fixtureObservation "curated" "2026-01-01T00:00:00Z"

                    saving =
                        savingEditModel "Hidden conditional draft"

                    known =
                        { original | contentVersion = "ffffffff-ffff-4fff-8fff-ffffffffffff" }

                    stale =
                        { original | contentVersion = "10000000-0000-4000-8000-000000000001" }

                    hidden =
                        Feature.Observation.selectObservation "other" saving |> Tuple.first

                    observed =
                        { hidden | observations = Feature.Observation.applyCanonicalObservation known hidden.observations }
                in
                case observed.observations.edit |> Maybe.andThen .activeRequest of
                    Nothing ->
                        Expect.fail "Expected hidden save owner"
                    Just request ->
                        let
                            checking =
                                Feature.Observation.update (ObservationUpdated request (Err (Api.ObservationContentConflict stale))) observed |> Tuple.first
                        in
                        case checking.observations.edit |> Maybe.andThen .activeCanonicalRequest of
                            Nothing ->
                                Expect.fail "Expected hidden canonical check"
                            Just refresh ->
                                let
                                    failed =
                                        Feature.Observation.update (ObservationConflictCanonicalFetched refresh known.contentVersion (Err Http.Timeout)) checking |> Tuple.first

                                    retry =
                                        Feature.Observation.update RebaseObservationEdit failed |> Tuple.first |> Feature.Observation.update SaveObservationEdit |> Tuple.first

                                    newest =
                                        { known | contentVersion = "00000000-0000-4000-8000-000000000003", content = "Current after another competing write" }

                                    conflictedAgain =
                                        retry.observations.edit |> Maybe.andThen .activeRequest |> Maybe.map (\next -> Feature.Observation.update (ObservationUpdated next (Err (Api.ObservationContentConflict newest))) retry |> Tuple.first)
                                in
                                Expect.all
                                    [ \_ -> checking.observations.selectedId |> Expect.equal (Just "other")
                                    , \_ -> checking.observations.activeDetailRequest |> Expect.equal observed.observations.activeDetailRequest
                                    , \_ -> checking.observations.orderedIds |> Expect.equal observed.observations.orderedIds
                                    , \_ -> failed.observations.edit |> Maybe.map (\edit -> ( edit.draft, edit.canonicalProvisional, edit.activeCanonicalRequest )) |> Expect.equal (Just ( "Hidden conditional draft", True, Nothing ))
                                    , \_ -> failed.observations.edit |> Maybe.andThen .error |> Maybe.map (String.contains "provisional") |> Expect.equal (Just True)
                                    , \_ -> retry.observations.edit |> Maybe.map .baseContentVersion |> Expect.equal (Just known.contentVersion)
                                    , \_ -> conflictedAgain |> Maybe.andThen (.observations >> .edit) |> Maybe.map (\edit -> ( edit.draft, edit.latestCanonical.contentVersion, edit.conflict )) |> Expect.equal (Just ( "Hidden conditional draft", newest.contentVersion, True ))
                                    , \_ -> conflictedAgain |> Maybe.andThen (.observations >> .edit) |> Maybe.andThen .activeCanonicalRequest |> Expect.equal Nothing
                                    ] ()
        , test "conditional conflict retires only current in-flight applied pages in flat, exact, match and facet modes" <|
            \_ ->
                let
                    original =
                        fixtureObservation "curated" "2026-01-01T00:00:00Z"

                    latest =
                        { original | contentVersion = "10000000-0000-4000-8000-000000000001" }

                    saving =
                        savingEditModel "Draft during page load"

                    facet =
                        facetFixture Api.SubjectFile original.subject 2 original.updatedAt

                    facetKey =
                        Feature.Observation.facetKey facet.subjectKind facet.subject

                    check mode =
                        let
                            state =
                                saving.observations

                            prepared =
                                { saving | observations = { state | requestMode = mode, matchAppliedPaths = [ original.subject ], matchPathsInput = original.subject, selectedFacet = Just { subjectKind = Api.SubjectFile, subject = original.subject }, facets = Dict.singleton facetKey facet, facetKeys = [ facetKey ] } }
                                    |> Feature.Observation.refreshActiveResults
                                    |> Tuple.first
                        in
                        case prepared.observations.edit |> Maybe.andThen .activeRequest of
                            Nothing ->
                                False
                            Just request ->
                                let
                                    completed =
                                        Feature.Observation.update (ObservationUpdated request (Err (Api.ObservationContentConflict latest))) prepared |> Tuple.first

                                    stale =
                                        case mode of
                                            ObservationFacetMode ->
                                                Feature.Observation.update (GotObservationSubjectFacets "workspace-1" 3 prepared.observations.facetRequestGeneration prepared.observations.facetFingerprint 0 (Ok { items = [ { facet | observationCount = 999 } ], hasMore = False })) completed |> Tuple.first
                                            ObservationMatchMode ->
                                                Feature.Observation.update (GotObservationMatches "workspace-1" 3 prepared.observations.requestGeneration prepared.observations.queryFingerprint 0 (Ok { items = [ matchFixture original [] ], hasMore = False })) completed |> Tuple.first
                                            _ ->
                                                Feature.DataLoading.update (GotObservations "workspace-1" Nothing prepared.observations.requestGeneration prepared.observations.queryFingerprint 0 (Ok { items = [ original ], hasMore = False })) completed |> Tuple.first

                                    generationRetired =
                                        if mode == ObservationFacetMode then
                                            completed.observations.facetRequestGeneration == prepared.observations.facetRequestGeneration && stale.observations.facetRequestGeneration == completed.observations.facetRequestGeneration + 1
                                        else
                                            completed.observations.requestGeneration == prepared.observations.requestGeneration && stale.observations.requestGeneration == completed.observations.requestGeneration + 1
                                in
                                generationRetired
                                    && ( stale.observations.selectedDetail, stale.observations.orderedIds, stale.observations.edit ) == ( completed.observations.selectedDetail, completed.observations.orderedIds, completed.observations.edit )
                                    && completed.observations.orderedIds == prepared.observations.orderedIds
                                    && completed.observations.selectedDetail == Just latest
                                    && completed.observations.appliedQuery == prepared.observations.appliedQuery
                                    && (completed.observations.edit |> Maybe.map .draft) == Just "Draft during page load"
                in
                [ ObservationFlatMode, ObservationExactSubjectMode, ObservationMatchMode, ObservationFacetMode ] |> List.map check |> Expect.equal [ True, True, True, True ]
        , test "hidden save owner accepts a provenance-checked409 without selecting or admitting its observation, and rejects mismatched latest" <|
            \_ ->
                let
                    saving =
                        savingEditModel "Hidden preserved draft"

                    hidden =
                        Feature.Observation.selectObservation "other" saving |> Tuple.first

                    baseline =
                        fixtureObservation "curated" "2026-01-01T00:00:00Z"

                    latest =
                        { baseline | content = "Foreign canonical", contentVersion = "10000000-0000-4000-8000-000000000001" }
                in
                case hidden.observations.edit |> Maybe.andThen .activeRequest of
                    Nothing -> Expect.fail "Hidden save owner must remain active"
                    Just owned ->
                        let
                            accepted =
                                Feature.Observation.update (ObservationUpdated owned (Err (Api.ObservationContentConflict latest))) hidden |> Tuple.first

                            mismatch =
                                Feature.Observation.update (ObservationUpdated owned (Err (Api.ObservationContentConflict { latest | gitSha = "different" }))) hidden |> Tuple.first

                            deleted =
                                Feature.Observation.update (ObservationUpdated owned (Err (Api.ObservationUpdateHttpError (Http.BadStatus 404)))) hidden |> Tuple.first
                        in
                        Expect.all
                            [ \_ -> accepted.observations.selectedId |> Expect.equal (Just "other")
                            , \_ -> accepted.observations.edit |> Maybe.map (\edit -> ( edit.draft, edit.conflict, edit.latestCanonical )) |> Expect.equal (Just ( "Hidden preserved draft", True, latest ))
                            , \_ -> mismatch.observations.edit |> Maybe.map .latestCanonical |> Expect.equal (Just baseline)
                            , \_ -> mismatch.observations.edit |> Maybe.map .draft |> Expect.equal (Just "Hidden preserved draft")
                            , \_ -> deleted.observations.edit |> Expect.equal Nothing
                            ] ()
        , test "gates mutation controls and keeps provenance immutable in the render tree" <|
            \_ ->
                let
                    state =
                        selectedObservationState (fixtureObservation "curate" "2026-01-01T00:00:00Z")

                    repository =
                        observationWorkspace Api.Repository

                    readOnly =
                        Feature.Observation.viewObservationsStateWithPermission False repository state |> Query.fromHtml

                    editor =
                        Feature.Observation.viewObservationsStateWithPermission True repository state |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> readOnly |> Query.hasNot [ Selector.text "Edit content" ]
                    , \_ -> readOnly |> Query.hasNot [ Selector.id "observation-delete" ]
                    , \_ -> editor |> Query.hasNot [ Selector.id "observation-edit" ]
                    , \_ -> editor |> Query.has [ Selector.text "Delete", Selector.text "Workspace ID", Selector.text "Subjects", Selector.text "Git SHA" ]
                    , \_ -> editor |> Query.findAll [ Selector.tag "textarea" ] |> Query.count (Expect.equal 1)
                    , \_ -> editor |> Query.findAll [ Selector.tag "input" ] |> Query.count (Expect.equal 3)
                    ]
                    ()
        , test "edit captures canonical base, validates UTF-8 bounds, and renders only a content draft" <|
            \_ ->
                let
                    observation =
                        fixtureObservation "edit" "2026-01-01T00:00:00Z"

                    started =
                        Feature.Observation.update StartObservationEdit (editableModel (selectedObservationState observation))
                            |> Tuple.first

                    view =
                        Feature.Observation.viewObservationsStateWithPermission True (observationWorkspace Api.Repository) started.observations
                            |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> started.observations.edit |> Maybe.map (\edit -> ( edit.baseContent, edit.baseUpdatedAt, edit.draft )) |> Expect.equal (Just ( observation.content, observation.updatedAt, observation.content ))
                    , \_ -> view |> Query.find [ Selector.id "observation-edit-content" ] |> Query.has [ Selector.tag "textarea" ]
                    , \_ -> view |> Query.has [ Selector.text "Only content can be edited", Selector.text "immutable provenance" ]
                    , \_ -> Feature.Observation.observationContentError "   " |> Expect.equal (Just "Observation content must not be blank.")
                    , \_ -> Feature.Observation.observationContentError (String.repeat 524288 "a") |> Expect.equal Nothing
                    , \_ -> Feature.Observation.observationContentError (String.repeat 524289 "a") |> Expect.equal (Just "Observation content must not exceed 512 KiB of UTF-8 text.")
                    , \_ -> Feature.Observation.observationContentError (String.repeat 174763 "€") |> Expect.equal (Just "Observation content must not exceed 512 KiB of UTF-8 text.")
                    ]
                    ()
        , test "retained dirty saving and conflicted editors stay separate from the read-only canonical body" <|
            \_ ->
                let
                    original = savingEditModel "Protected retained draft"
                    check ( saving, conflict ) =
                        let
                            state = original.observations
                            retained = { state | edit = Maybe.map (\edit -> { edit | saving = saving, conflict = conflict }) state.edit }
                            view = Feature.Observation.viewObservationsStateWithPermission True (observationWorkspace Api.Repository) retained |> Query.fromHtml
                            canonical = Maybe.map .content retained.selectedDetail |> Maybe.withDefault ""
                        in
                        Expect.all
                            [ \_ -> view |> Query.find [ Selector.class "observation-detail-content" ] |> Query.has [ Selector.text canonical ]
                            , \_ -> view |> Query.find [ Selector.class "observation-retained-editor" ] |> Query.has [ Selector.attribute (attribute "aria-label" "Retained observation draft"), Selector.text "Retained draft" ]
                            , \_ -> view |> Query.find [ Selector.id "observation-edit-content" ] |> Query.has [ Selector.attribute (Html.Attributes.value "Protected retained draft"), Selector.attribute (Html.Attributes.disabled saving) ]
                            , \_ -> view |> Query.find [ Selector.class "observation-edit-actions" ] |> Query.find [ Selector.tag "button", Selector.class "btn-primary" ] |> Query.has [ Selector.attribute (Html.Attributes.disabled (saving || conflict)) ]
                            , \_ -> view |> Query.hasNot [ Selector.id "observation-edit" ]
                            , \_ -> view |> Query.hasNot [ Selector.id "observation-delete" ]
                            ] ()
                in
                Expect.all (List.map (\variant _ -> check variant) [ ( False, False ), ( True, False ), ( False, True ) ]) ()
        , test "canonical edits refresh clean drafts but preserve dirty drafts as explicit conflicts" <|
            \_ ->
                let
                    observation =
                        fixtureObservation "drift" "2026-01-01T00:00:00Z"

                    newer =
                        { observation | content = "foreign content", updatedAt = "2026-01-02T00:00:00Z" }

                    started =
                        Feature.Observation.update StartObservationEdit (editableModel (selectedObservationState observation))
                            |> Tuple.first

                    clean =
                        Feature.Observation.applyCanonicalObservation newer started.observations

                    dirtyModel =
                        Feature.Observation.update (SetObservationDraft "my draft") started |> Tuple.first

                    dirty =
                        Feature.Observation.applyCanonicalObservation newer dirtyModel.observations
                in
                [ clean.edit |> Maybe.map (\edit -> ( edit.draft, edit.baseUpdatedAt, edit.conflict ))
                , dirty.edit |> Maybe.map (\edit -> ( edit.draft, edit.latestCanonical.updatedAt, edit.conflict ))
                ]
                    |> Expect.equal
                        [ Just ( "foreign content", "2026-01-02T00:00:00Z", False )
                        , Just ( "my draft", "2026-01-02T00:00:00Z", True )
                        ]
        , test "request identity rejects workspace, session, context, and superseded response races" <|
            \_ ->
                let
                    saving =
                        savingEditModel "guarded"
                in
                case saving.observations.edit |> Maybe.andThen .activeRequest of
                    Nothing ->
                        Expect.fail "save should create an active request"

                    Just request ->
                        [ Feature.Observation.mutationResponseMatches request saving
                        , Feature.Observation.mutationResponseMatches request { saving | sessionRequestEpoch = saving.sessionRequestEpoch + 1 }
                        , Feature.Observation.mutationResponseMatches { request | contextToken = request.contextToken + 1 } saving
                        , Feature.Observation.mutationResponseMatches { request | requestToken = request.requestToken + 1 } saving
                        , Feature.Observation.mutationResponseMatches request { saving | selectedWorkspaceId = Just "other-workspace" }
                        ]
                            |> Expect.equal [ True, False, False, False, False ]
        , test "update failure preserves the canonical item and retryable draft" <|
            \_ ->
                let
                    saving =
                        savingEditModel "retry this draft"
                in
                case saving.observations.edit |> Maybe.andThen .activeRequest of
                    Nothing ->
                        Expect.fail "save should create an active request"

                    Just request ->
                        let
                            failed =
                                Feature.Observation.update (ObservationUpdated request (Err (Api.ObservationUpdateHttpError Http.Timeout))) saving
                                    |> Tuple.first
                        in
                        Expect.equal
                            { detail = Just "Observation"
                            , listed = Just "Observation"
                            , draft = Just "retry this draft"
                            , saving = Just False
                            , activeRequest = Just Nothing
                            , retryableError = Just True
                            }
                            { detail = failed.observations.selectedDetail |> Maybe.map .content
                            , listed = Dict.get request.observationId failed.observations.items |> Maybe.map .content
                            , draft = failed.observations.edit |> Maybe.map .draft
                            , saving = failed.observations.edit |> Maybe.map .saving
                            , activeRequest = failed.observations.edit |> Maybe.map .activeRequest
                            , retryableError = failed.observations.edit |> Maybe.map (.error >> (/=) Nothing)
                            }
        , test "update response accepts equivalent RFC3339 created timestamp encodings" <|
            \_ ->
                let
                    saving =
                        savingEditModel "saved content"
                in
                case saving.observations.edit |> Maybe.andThen .activeRequest of
                    Nothing ->
                        Expect.fail "save should create an active request"

                    Just request ->
                        let
                            canonical =
                                fixtureObservation "curated" "2025-12-31T19:00:00-05:00"

                            response =
                                { canonical
                                    | content = "saved content"
                                    , updatedAt = "2026-01-02T00:00:00.120000Z"
                                }

                            updated =
                                Feature.Observation.update (ObservationUpdated request (Ok response)) saving
                                    |> Tuple.first
                        in
                        Expect.equal
                            { detail = Just "saved content"
                            , edit = Nothing
                            }
                            { detail = updated.observations.selectedDetail |> Maybe.map .content
                            , edit = updated.observations.edit
                            }
        , test "event-before-response cannot overwrite a newer canonical version or lose the draft" <|
            \_ ->
                let
                    saving =
                        savingEditModel "local draft"

                    original =
                        fixtureObservation "curated" "2026-01-01T00:00:00Z"

                    foreign =
                        { original | content = "foreign wins", contentVersion = "10000000-0000-4000-8000-000000000002", updatedAt = "2026-01-03T00:00:00Z" }

                    afterEvent =
                        { saving | observations = Feature.Observation.applyCanonicalObservation foreign saving.observations }
                in
                case afterEvent.observations.edit |> Maybe.andThen .activeRequest of
                    Nothing ->
                        Expect.fail "save request should remain active through a canonical event"

                    Just request ->
                        let
                            staleResponse =
                                { original | content = "local draft", contentVersion = "10000000-0000-4000-8000-000000000001", updatedAt = "2026-01-02T00:00:00Z" }

                            afterResponse =
                                Feature.Observation.update (ObservationUpdated request (Ok staleResponse)) afterEvent |> Tuple.first
                        in
                        Expect.all
                            [ \_ -> afterResponse.observations.selectedDetail |> Maybe.map .content |> Expect.equal (Just "foreign wins")
                            , \_ ->
                                afterResponse.observations.edit
                                    |> Maybe.map
                                        (\edit ->
                                            { draft = edit.draft
                                            , conflict = edit.conflict
                                            , saving = edit.saving
                                            , activeRequest = edit.activeRequest
                                            }
                                        )
                                    |> Expect.equal (Just { draft = "local draft", conflict = True, saving = False, activeRequest = Nothing })
                            ]
                            ()
        , test "canonical merge rejects older payloads and immutable provenance drift" <|
            \_ ->
                let
                    base =
                        fixtureObservation "immutable" "2026-01-01T00:00:00Z"

                    current =
                        { base | content = "new", updatedAt = "2026-01-03T00:00:00Z" }

                    older =
                        { current | content = "old", updatedAt = "2026-01-02T00:00:00Z" }

                    wrongSha =
                        { current | content = "wrong", gitSha = "different", updatedAt = "2026-01-04T00:00:00Z" }
                in
                [ Feature.Observation.preferNewerObservation older current
                , Feature.Observation.preferNewerObservation wrongSha current
                ]
                    |> Expect.equal [ current, current ]
        , test "canonical chronology normalizes RFC3339 fractions and offsets and fails closed on malformed timestamps" <|
            \_ ->
                let
                    base =
                        fixtureObservation "timestamp" "2026-01-01T00:00:00.1Z"

                    fractionalNewer =
                        { base | content = "fractional newer", updatedAt = "2026-01-01T00:00:00.12Z" }

                    noFraction =
                        { base | content = "same", updatedAt = "2026-01-01T00:00:00Z" }

                    zeroFraction =
                        { noFraction | updatedAt = "2026-01-01T00:00:00.000Z" }

                    utc =
                        { base | content = "same instant", updatedAt = "2026-01-01T00:00:00Z" }

                    equivalentOffset =
                        { utc | updatedAt = "2026-01-01T01:00:00+01:00" }

                    olderOffset =
                        { utc | content = "older", updatedAt = "2026-01-01T00:30:00+01:00" }

                    invalid =
                        { base | content = "must not win", updatedAt = "2026-13-01T00:00:00Z" }
                in
                [ Feature.Observation.preferNewerObservation fractionalNewer base |> .content
                , Feature.Observation.preferNewerObservation zeroFraction noFraction |> .updatedAt
                , Feature.Observation.preferNewerObservation equivalentOffset utc |> .updatedAt
                , Feature.Observation.preferNewerObservation olderOffset utc |> .content
                , Feature.Observation.preferNewerObservation invalid base |> .content
                ]
                    |> Expect.equal
                        [ "fractional newer"
                        , "2026-01-01T00:00:00.000Z"
                        , "2026-01-01T01:00:00+01:00"
                        , "same instant"
                        , "Observation"
                        ]
        , test "authoritative list refresh reconciles selected detail without losing a dirty draft" <|
            \_ ->
                let
                    existing =
                        fixtureObservation "refresh" "2026-01-01T00:00:00Z"

                    latest =
                        { existing | content = "after refresh", updatedAt = "2026-01-02T00:00:00Z" }

                    editing =
                        Feature.Observation.update StartObservationEdit (editableModel (selectedObservationState existing))
                            |> Tuple.first
                            |> Feature.Observation.update (SetObservationDraft "local draft")
                            |> Tuple.first

                    merged =
                        Feature.DataLoading.mergeObservationPage 0
                            { items = [ latest ], hasMore = False }
                            editing.observations
                in
                Expect.equal
                    { detail = Just "after refresh"
                    , listed = Just "after refresh"
                    , draft = Just "local draft"
                    , conflict = Just True
                    , latest = Just "after refresh"
                    , stale = False
                    }
                    { detail = merged.selectedDetail |> Maybe.map .content
                    , listed = Dict.get latest.id merged.items |> Maybe.map .content
                    , draft = merged.edit |> Maybe.map .draft
                    , conflict = merged.edit |> Maybe.map .conflict
                    , latest = merged.edit |> Maybe.map (.latestCanonical >> .content)
                    , stale = merged.resultsStale
                    }
        , test "an authoritative refresh clears stale state while a facet delete keeps its aggregate page explicitly stale" <|
            \_ ->
                let
                    observation =
                        fixtureObservation "facet-delete" "2026-01-01T00:00:00Z"

                    selected =
                        selectedObservationState observation

                    staleSelected =
                        { selected | resultsStale = True }

                    listed =
                        Feature.DataLoading.mergeObservationPage 0
                            { items = [ observation ], hasMore = False }
                            staleSelected

                    facetDeleted =
                        Feature.Observation.removeObservation observation.id
                            { listed | requestMode = ObservationFacetMode }
                in
                Expect.equal
                    { refreshIsFresh = False, deleteIsStale = True, removed = True, selectionCleared = True }
                    { refreshIsFresh = listed.resultsStale
                    , deleteIsStale = facetDeleted.resultsStale
                    , removed = Dict.member observation.id facetDeleted.items |> not
                    , selectionCleared = facetDeleted.selectedId == Nothing && facetDeleted.selectedDetail == Nothing
                    }
        , test "delete confirmation is modal, named, permanent, and keyboard-addressable" <|
            \_ ->
                let
                    base =
                        fixtureObservation "delete-me" "2026-01-01T00:00:00Z"

                    observation =
                        { base | content = "Permanent target" }

                    opened =
                        Feature.Observation.update OpenObservationDelete (editableModel (selectedObservationState observation))
                            |> Tuple.first

                    view =
                        Feature.Observation.viewObservationsStateWithPermission True (observationWorkspace Api.Repository) opened.observations
                            |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> view |> Query.find [ Selector.class "observation-curation-background" ] |> Query.has [ Selector.attribute (attribute "inert" ""), Selector.attribute (attribute "aria-hidden" "true") ]
                    , \_ -> view |> Query.find [ Selector.class "observation-delete-confirm" ] |> Query.has [ Selector.attribute (attribute "role" "dialog"), Selector.attribute (attribute "aria-modal" "true"), Selector.attribute (tabindex -1), Selector.text "Delete observation permanently?", Selector.text "Permanent target", Selector.text "cannot be undone" ]
                    , \_ -> view |> Query.find [ Selector.tag "button", Selector.containing [ Selector.text "Delete permanently" ] ] |> Query.hasNot [ Selector.disabled True ]
                    , \_ -> view |> Query.find [ Selector.id "observation-delete-cancel" ] |> Query.has [ Selector.tag "button" ]
                    , \_ -> Feature.Observation.deleteDialogFocusTarget False False "observation-delete-cancel" |> Expect.equal "observation-delete-confirm"
                    , \_ -> Feature.Observation.deleteDialogFocusTarget False True "observation-delete-confirm" |> Expect.equal "observation-delete-cancel"
                    , \_ -> Feature.Observation.deleteDialogFocusTarget True False "observation-delete-confirm" |> Expect.equal "observation-delete-dialog"
                    ]
                    ()
        , test "live permission downgrade closes open and in-flight curation state and makes late mutation responses inert" <|
            \_ ->
                let
                    observation =
                        fixtureObservation "permission" "2026-01-01T00:00:00Z"

                    opened =
                        Feature.Observation.update OpenObservationDelete (editableModel (selectedObservationState observation))
                            |> Tuple.first

                    downgradedOpen =
                        AppShell.handleOwned (AppShell.SessionContextLoadedMsg 3 (Just "workspace-1") (Ok readOnlySession)) opened
                            |> Tuple.first

                    deleting =
                        deletingModel

                    downgradedDeleting =
                        { deleting | sessionContext = Just readOnlySession }
                            |> Feature.Observation.reconcileCurationPermission

                    saving =
                        savingEditModel "permission draft"

                    unknown =
                        { saving | sessionContext = Nothing }
                            |> Feature.Observation.reconcileCurationPermission

                    readOnlyView =
                        Feature.Observation.viewObservationsStateWithPermission False (observationWorkspace Api.Repository) opened.observations
                            |> Query.fromHtml

                    lateDeleteIsInert =
                        case deleting.observations.deleteConfirmation |> Maybe.andThen .activeRequest of
                            Just request ->
                                Feature.Observation.update (ObservationDeleted request (Ok ())) downgradedDeleting
                                    |> Tuple.first
                                    |> (\after -> Dict.member request.observationId after.observations.items)

                            Nothing ->
                                False
                in
                Expect.all
                    [ \_ -> downgradedOpen.observations.deleteConfirmation |> Expect.equal Nothing
                    , \_ -> downgradedDeleting.observations.deleteConfirmation |> Expect.equal Nothing
                    , \_ -> unknown.observations.edit |> Expect.equal Nothing
                    , \_ -> lateDeleteIsInert |> Expect.equal True
                    , \_ -> readOnlyView |> Query.hasNot [ Selector.attribute (attribute "role" "dialog"), Selector.text "Delete permanently" ]
                    ]
                    ()
        , test "exact snapshot transport invalidation refetch preserves a dirty draft as a blocking conflict" <|
            \_ ->
                let
                    canonical =
                        fixtureObservation "live" "2026-01-01T00:00:00Z"

                    snapshotCanonical =
                        { canonical | content = "snapshot canonical content", updatedAt = "2026-01-01T00:00:00.1Z" }

                    dirty =
                        Feature.Observation.update StartObservationEdit (editableModel (selectedObservationState canonical))
                            |> Tuple.first
                            |> Feature.Observation.update (SetObservationDraft "local unsaved draft")
                            |> Tuple.first

                    afterSnapshot =
                        Feature.WebSocket.update (WsMessageReceived (observationSnapshotWire snapshotCanonical)) dirty
                            |> Tuple.first

                    afterInvalidation =
                        Feature.WebSocket.update (WsMessageReceived (observationInvalidationWire canonical.id)) afterSnapshot
                            |> Tuple.first

                    targetKey =
                        "workspace:workspace-1|entity:observation:" ++ canonical.id

                    generation =
                        Dict.get targetKey afterInvalidation.webSocket.targetGenerations |> Maybe.withDefault -1

                    guard =
                        { scopeKey = "workspace:workspace-1"
                        , targetKey = "entity:observation:" ++ canonical.id
                        , targetGeneration = generation
                        , sessionEpoch = 3
                        , routeWorkspace = Just "workspace-1"
                        , audienceId = "editor"
                        }

                    foreign =
                        { canonical | content = "foreign canonical content", updatedAt = "2026-01-01T00:00:00.12Z" }

                    final =
                        Feature.WebSocket.update (CanonicalObservationFetched guard "workspace-1" canonical.id (Ok foreign)) afterInvalidation
                            |> Tuple.first

                    finalView =
                        Feature.Observation.viewObservationsStateWithPermission True (observationWorkspace Api.Repository) final.observations
                            |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> Dict.get "workspace:workspace-1" afterSnapshot.webSocket.streams |> Maybe.andThen .resumeToken |> Expect.equal (Just "snapshot-token")
                    , \_ -> afterSnapshot.observations.selectedDetail |> Maybe.map .content |> Expect.equal (Just "snapshot canonical content")
                    , \_ -> afterSnapshot.observations.edit |> Maybe.map (\edit -> ( edit.draft, edit.conflict, edit.latestCanonical.content )) |> Expect.equal (Just ( "local unsaved draft", True, "snapshot canonical content" ))
                    , \_ -> ( afterSnapshot.observations.requestGeneration, afterSnapshot.observations.expectedOffset ) |> Expect.equal ( 1, Just 0 )
                    , \_ -> generation |> Expect.equal 1
                    , \_ -> final.observations.selectedDetail |> Maybe.map .content |> Expect.equal (Just "foreign canonical content")
                    , \_ -> final.observations.edit |> Maybe.map (\edit -> ( edit.draft, edit.conflict, edit.latestCanonical.content )) |> Expect.equal (Just ( "local unsaved draft", True, "foreign canonical content" ))
                    , \_ -> finalView |> Query.find [ Selector.tag "button", Selector.containing [ Selector.text "Save content" ] ] |> Query.has [ Selector.disabled True ]
                    , \_ -> finalView |> Query.has [ Selector.text "Use latest version", Selector.text "Keep my draft" ]
                    ]
                    ()
        , test "canonical snapshots preserve and refresh flat, facet, exact, and match mode caches without admitting unfiltered rows" <|
            \_ ->
                let
                    retained =
                        fixtureObservation "retained" "2026-01-01T00:00:00Z"

                    canonicalRetained =
                        { retained | content = "Canonical retained", updatedAt = "2026-01-01T00:00:01Z" }

                    deleted =
                        fixtureObservation "deleted-by-snapshot" "2026-01-01T00:00:00Z"

                    unrelated =
                        fixtureObservation "unfiltered-snapshot-row" "2026-01-01T00:00:00Z"

                    facet =
                        facetFixture Api.SubjectGlob "src/**/*.elm" 9 "2026-01-01T00:00:01Z"

                    facetKey =
                        Feature.Observation.facetKey facet.subjectKind facet.subject

                    evidence observation =
                        matchFixture observation
                            [ { path = "src/A.elm", matchedSubjects = observation.subjects } ]

                    state mode =
                        let
                            initial =
                                Feature.Observation.init
                        in
                        { initial
                            | items = Dict.fromList [ ( retained.id, retained ), ( deleted.id, deleted ) ]
                            , orderedIds = [ retained.id, deleted.id ]
                            , hasMore = True
                            , query = "needle"
                            , subjectKind = Just Api.SubjectFile
                            , subject = "manual/flat.elm"
                            , selectedFacet = Just { subjectKind = Api.SubjectGlob, subject = "src/**/*.elm" }
                            , gitSha = fullSha
                            , requestMode = mode
                            , matchPathsInput = "src/Draft.elm"
                            , matchAppliedPaths = [ "src/A.elm" ]
                            , matchEvidence = Dict.fromList [ ( retained.id, evidence retained ), ( deleted.id, evidence deleted ) ]
                            , browseReturn = Just { requestMode = ObservationExactSubjectMode, subjectKind = Just Api.SubjectFile, subject = "manual/flat.elm", selectedFacet = Just { subjectKind = Api.SubjectGlob, subject = "src/**/*.elm" }, query = "needle", gitSha = fullSha }
                            , requestGeneration = 11
                            , requestSessionEpoch = 3
                            , queryFingerprint = "pre-snapshot-results"
                            , expectedOffset = Just 50
                            , nextOffset = 50
                            , facets = Dict.singleton facetKey facet
                            , facetKeys = [ facetKey ]
                            , facetHasMore = True
                            , facetRequestGeneration = 7
                            , facetRequestSessionEpoch = 3
                            , facetFingerprint = "pre-snapshot-facets"
                            , facetExpectedOffset = Just 50
                            , facetNextOffset = 50
                            , selectedId = Just retained.id
                            , selectedDetail = Just retained
                            , detailLoading = True
                        }

                    after mode =
                        Feature.WebSocket.update
                            (WsMessageReceived (observationSnapshotWireMany [ canonicalRetained, unrelated ]))
                            (editableModel (state mode))
                            |> Tuple.first

                    flat =
                        after ObservationFlatMode

                    facets =
                        after ObservationFacetMode

                    exact =
                        after ObservationExactSubjectMode

                    matched =
                        after ObservationMatchMode

                    commonPreservation model =
                        { itemIds = Dict.keys model.observations.items
                        , orderedIds = model.observations.orderedIds
                        , evidenceIds = Dict.keys model.observations.matchEvidence
                        , evidenceContent = Dict.get retained.id model.observations.matchEvidence |> Maybe.map (\item -> item.observation.content)
                        , detailContent = model.observations.selectedDetail |> Maybe.map .content
                        , hasMore = model.observations.hasMore
                        , query = model.observations.query
                        , draftPaths = model.observations.matchPathsInput
                        , appliedPaths = model.observations.matchAppliedPaths
                        , facetKeys = model.observations.facetKeys
                        }
                in
                Expect.all
                    [ \_ ->
                        [ flat, facets, exact, matched ]
                            |> List.map commonPreservation
                            |> Expect.equal
                                (List.repeat 4
                                    { itemIds = [ retained.id ]
                                    , orderedIds = [ retained.id ]
                                    , evidenceIds = [ retained.id ]
                                    , evidenceContent = Just "Canonical retained"
                                    , detailContent = Just "Canonical retained"
                                    , hasMore = True
                                    , query = "needle"
                                    , draftPaths = "src/Draft.elm"
                                    , appliedPaths = [ "src/A.elm" ]
                                    , facetKeys = [ facetKey ]
                                    }
                                )
                    , \_ ->
                        [ flat, exact, matched ]
                            |> List.map
                                (\model ->
                                    { loading = model.observations.loading
                                    , generation = model.observations.requestGeneration
                                    , expectedOffset = model.observations.expectedOffset
                                    , fingerprintChanged = model.observations.queryFingerprint /= "pre-snapshot-results"
                                    }
                                )
                            |> Expect.equal (List.repeat 3 { loading = True, generation = 12, expectedOffset = Just 0, fingerprintChanged = True })
                    , \_ ->
                        { loading = facets.observations.facetLoading
                        , generation = facets.observations.facetRequestGeneration
                        , expectedOffset = facets.observations.facetExpectedOffset
                        , fingerprintChanged = facets.observations.facetFingerprint /= "pre-snapshot-facets"
                        }
                            |> Expect.equal { loading = True, generation = 8, expectedOffset = Just 0, fingerprintChanged = True }
                    , \_ -> ( exact.observations.selectedFacet, exact.observations.subjectKind, exact.observations.subject ) |> Expect.equal ( Just { subjectKind = Api.SubjectGlob, subject = "src/**/*.elm" }, Just Api.SubjectFile, "manual/flat.elm" )
                    , \_ -> Dict.member unrelated.id flat.observations.items |> Expect.equal False
                    ]
                    ()
        , test "canonical snapshot generations make every pre-snapshot load-more response inert" <|
            \_ ->
                let
                    retained =
                        fixtureObservation "snapshot-current" "2026-01-01T00:00:00Z"

                    stale =
                        fixtureObservation "stale-load-more" "2026-01-01T00:00:00Z"

                    facet =
                        facetFixture Api.SubjectFile "src/Main.elm" 2 "2026-01-01T00:00:00Z"

                    initial =
                        Feature.Observation.init

                    base mode =
                        { initial
                            | items = Dict.singleton retained.id retained
                            , orderedIds = [ retained.id ]
                            , requestMode = mode
                            , selectedFacet = Just { subjectKind = Api.SubjectFile, subject = "src/Main.elm" }
                            , matchPathsInput = "src/Main.elm"
                            , matchAppliedPaths = [ "src/Main.elm" ]
                            , matchEvidence = Dict.singleton retained.id (matchFixture retained [ { path = "src/Main.elm", matchedSubjects = retained.subjects } ])
                            , requestGeneration = 5
                            , requestSessionEpoch = 3
                            , queryFingerprint = "old-results"
                            , expectedOffset = Just 50
                            , facetRequestGeneration = 6
                            , facetRequestSessionEpoch = 3
                            , facetFingerprint = "old-facets"
                            , facetExpectedOffset = Just 50
                        }

                    snap mode =
                        Feature.WebSocket.update (WsMessageReceived (observationSnapshotWire retained)) (editableModel (base mode))
                            |> Tuple.first

                    staleList mode =
                        Feature.DataLoading.update
                            (GotObservations "workspace-1" Nothing 5 "old-results" 50 (Ok { items = [ stale ], hasMore = False }))
                            (snap mode)
                            |> Tuple.first

                    staleFacet =
                        Feature.Observation.update
                            (GotObservationSubjectFacets "workspace-1" 3 6 "old-facets" 50 (Ok { items = [ facet ], hasMore = False }))
                            (snap ObservationFacetMode)
                            |> Tuple.first

                    staleMatch =
                        Feature.Observation.update
                            (GotObservationMatches "workspace-1" 3 5 "old-results" 50 (Ok { items = [ matchFixture stale [ { path = "src/Main.elm", matchedSubjects = stale.subjects } ] ], hasMore = False }))
                            (snap ObservationMatchMode)
                            |> Tuple.first
                in
                Expect.all
                    [ \_ -> [ ObservationFlatMode, ObservationExactSubjectMode ] |> List.map (\mode -> Dict.member stale.id (staleList mode).observations.items) |> Expect.equal [ False, False ]
                    , \_ -> Dict.member (Feature.Observation.facetKey facet.subjectKind facet.subject) staleFacet.observations.facets |> Expect.equal False
                    , \_ -> ( Dict.member stale.id staleMatch.observations.items, Dict.member stale.id staleMatch.observations.matchEvidence ) |> Expect.equal ( False, False )
                    , \_ -> [ (snap ObservationFlatMode).observations.requestGeneration, (snap ObservationExactSubjectMode).observations.requestGeneration, (snap ObservationMatchMode).observations.requestGeneration, (snap ObservationFacetMode).observations.facetRequestGeneration ] |> Expect.equal [ 6, 6, 6, 7 ]
                    ]
                    ()
        , test "canonical snapshot deletion prunes selection, detail, edit, and match evidence before refresh" <|
            \_ ->
                let
                    deleted =
                        fixtureObservation "selected-deleted" "2026-01-01T00:00:00Z"

                    retained =
                        fixtureObservation "still-present" "2026-01-01T00:00:00Z"

                    selected =
                        selectedObservationState deleted

                    state =
                        { selected
                            | items = Dict.fromList [ ( deleted.id, deleted ), ( retained.id, retained ) ]
                            , orderedIds = [ deleted.id, retained.id ]
                            , requestGeneration = 2
                            , requestSessionEpoch = 3
                            , queryFingerprint = "old"
                            , expectedOffset = Just 50
                        }

                    editing =
                        Feature.Observation.update StartObservationEdit (editableModel state)
                            |> Tuple.first

                    after =
                        Feature.WebSocket.update (WsMessageReceived (observationSnapshotWire retained)) editing
                            |> Tuple.first
                in
                { itemIds = Dict.keys after.observations.items
                , orderedIds = after.observations.orderedIds
                , selectedId = after.observations.selectedId
                , detail = after.observations.selectedDetail
                , edit = after.observations.edit
                , evidenceIsEmpty = Dict.isEmpty after.observations.matchEvidence
                , generation = after.observations.requestGeneration
                , expectedOffset = after.observations.expectedOffset
                }
                    |> Expect.equal
                        { itemIds = [ retained.id ]
                        , orderedIds = [ retained.id ]
                        , selectedId = Nothing
                        , detail = Nothing
                        , edit = Nothing
                        , evidenceIsEmpty = True
                        , generation = 3
                        , expectedOffset = Just 0
                        }
        , test "delete failure preserves item, selection, evidence, and retryable dialog" <|
            \_ ->
                let
                    deleting =
                        deletingModel
                in
                case deleting.observations.deleteConfirmation |> Maybe.andThen .activeRequest of
                    Nothing ->
                        Expect.fail "delete should create an active request"

                    Just request ->
                        let
                            failed =
                                Feature.Observation.update (ObservationDeleted request (Err Http.Timeout)) deleting |> Tuple.first
                        in
                        Expect.all
                            [ \_ -> Dict.member request.observationId failed.observations.items |> Expect.equal True
                            , \_ -> failed.observations.selectedId |> Expect.equal (Just request.observationId)
                            , \_ -> Dict.member request.observationId failed.observations.matchEvidence |> Expect.equal True
                            , \_ -> failed.observations.deleteConfirmation |> Maybe.map (\confirmation -> ( confirmation.deleting, confirmation.activeRequest, confirmation.error /= Nothing )) |> Expect.equal (Just ( False, Nothing, True ))
                            ]
                            ()
        , test "delete success, 404, foreign delete, and repetition converge on idempotent cleanup" <|
            \_ ->
                let
                    deleting =
                        deletingModel
                in
                case deleting.observations.deleteConfirmation |> Maybe.andThen .activeRequest of
                    Nothing ->
                        Expect.fail "delete should create an active request"

                    Just request ->
                        let
                            alreadyDeleted =
                                Feature.Observation.update (ObservationDeleted request (Err (Http.BadStatus 404))) deleting |> Tuple.first

                            once =
                                Feature.Observation.removeObservation request.observationId deleting.observations

                            twice =
                                Feature.Observation.removeObservation request.observationId once
                        in
                        [ Dict.member request.observationId alreadyDeleted.observations.items
                        , alreadyDeleted.observations.selectedId /= Nothing
                        , Dict.member request.observationId alreadyDeleted.observations.matchEvidence
                        , alreadyDeleted.observations.deleteConfirmation /= Nothing
                        , alreadyDeleted.observations.requestGeneration > deleting.observations.requestGeneration
                        , once == twice
                        , once.selectedDetail == Nothing
                        , once.edit == Nothing
                        , once.deleteConfirmation == Nothing
                        ]
                            |> Expect.equal [ False, False, False, False, True, True, True, True, True ]
        , test "cascade results match the four-field server contract" <|
            \_ ->
                [ (Decode.decodeString Api.cascadeResultDecoder "{\"affected\":4,\"project_count\":1,\"task_count\":2,\"dependency_link_count\":3}"
                    |> Result.map (\result -> { affected = result.affected, projects = result.projectCount, tasks = result.taskCount, dependencies = result.dependencyLinkCount })
                  )
                    == Ok { affected = 4, projects = 1, tasks = 2, dependencies = 3 }
                , (Decode.decodeString Api.cascadeResultDecoder "{\"affected\":4,\"project_count\":1,\"task_count\":2,\"memory_count\":9}"
                    |> Result.toMaybe
                  )
                    == Nothing
                ]
                    |> Expect.equal [ True, True ]
        , test "observation direct-link fragment and pagination transition are deterministic" <|
            \_ ->
                let
                    directLink =
                        Helpers.parseFragment (Just "tab=projects&observation=search-only-hit")
                in
                [ (Helpers.parseFragment (Just "tab=observations")).tab == ObservationsTab
                , directLink.tab == ObservationsTab
                , directLink.observationId == Just "search-only-hit"
                , Helpers.buildFragment ObservationsTab Nothing (Just "search-only-hit") == "tab=observations&observation=search-only-hit"
                , Feature.DataLoading.nextPageOffset 0 { items = [ 1, 2 ], hasMore = True } == Just 2
                , Feature.DataLoading.nextPageOffset 0 { items = [], hasMore = True } == Nothing
                ]
                    |> Expect.equal [ True, True, True, True, True, True ]
        , test "same-observation selection preserves dirty and in-flight edit identity" <|
            \_ ->
                let
                    saving =
                        savingEditModel "protected draft"

                    dirty =
                        saving.observations.edit
                            |> Maybe.map (\edit -> { edit | saving = False, activeRequest = Nothing })

                    dirtyState =
                        saving.observations

                    models =
                        [ saving, { saving | observations = { dirtyState | edit = dirty } } ]
                in
                List.map
                    (\model -> Feature.Observation.selectObservation "curated" model |> Tuple.first |> .observations)
                    models
                    |> Expect.equal (List.map .observations models)
        , test "URL-seeded and failed same-selection details hydrate without retiring the retained owner" <|
            \_ ->
                let
                    initial =
                        Feature.Observation.init

                    seeded =
                        editableModel { initial | selectedId = Just "off-page-link" }

                    hydrated =
                        Feature.Observation.selectObservation "off-page-link" seeded |> Tuple.first

                    failedSeedState =
                        hydrated.observations

                    failedSeed =
                        { hydrated | observations = { failedSeedState | selectedDetail = Nothing, detailLoading = False, detailError = Just "Failed", activeDetailRequest = Nothing } }

                    retriedSeed =
                        Feature.Observation.selectObservation "off-page-link" failedSeed |> Tuple.first

                    saving =
                        savingEditModel "owned retry"

                    failedOwnerState =
                        saving.observations

                    failedOwner =
                        { saving | observations = { failedOwnerState | selectedDetail = Nothing, detailLoading = False, detailError = Just "Failed", activeDetailRequest = Nothing } }

                    retriedOwner =
                        Feature.Observation.selectObservation "curated" failedOwner |> Tuple.first
                in
                Expect.all
                    [ \_ -> hydrated.observations.activeDetailRequest |> Expect.equal (Just { workspaceId = "workspace-1", observationId = "off-page-link", sessionEpoch = seeded.sessionRequestEpoch, token = 1 })
                    , \_ -> hydrated.observations.items |> Expect.equal Dict.empty
                    , \_ -> retriedSeed.observations.activeDetailRequest |> Maybe.map .token |> Expect.equal (Just 2)
                    , \_ -> retriedSeed.observations.detailError |> Expect.equal Nothing
                    , \_ -> retriedOwner.observations.detailLoading |> Expect.equal True
                    , \_ -> retriedOwner.observations.edit |> Expect.equal saving.observations.edit
                    , \_ -> retriedOwner.observations.selectedDetail |> Expect.equal (saving.observations.edit |> Maybe.map .latestCanonical)
                    ]
                    ()
        , test "failed detail revalidation after returning keeps draft and conflict controls reachable" <|
            \_ ->
                let
                    saving =
                        savingEditModel "editable retained draft"

                    state =
                        saving.observations

                    dirty =
                        { saving | observations = { state | edit = Maybe.map (\edit -> { edit | saving = False, activeRequest = Nothing, conflict = True }) state.edit } }

                    browsing =
                        Feature.Observation.selectObservation "other" dirty |> Tuple.first

                    returned =
                        Feature.Observation.update ReturnToObservationDraft browsing |> Tuple.first
                in
                case returned.observations.activeDetailRequest of
                    Just request ->
                        let
                            failed =
                                Feature.Observation.update (GotObservationDetail request.workspaceId request.observationId request.sessionEpoch request.token (Err Http.NetworkError)) returned |> Tuple.first

                            view =
                                Feature.Observation.viewObservationsStateWithPermission True (observationWorkspace Api.Repository) failed.observations |> Query.fromHtml
                        in
                        Expect.all
                            [ \_ -> failed.observations.edit |> Expect.equal dirty.observations.edit
                            , \_ -> view |> Query.has [ Selector.text "Failed to load observation detail.", Selector.text "Save content", Selector.text "Cancel", Selector.text "Keep my draft", Selector.text "Use latest version" ]
                            , \_ -> view |> Query.find [ Selector.id "observation-edit-content" ] |> Query.has [ Selector.attribute (Html.Attributes.value "editable retained draft") ]
                            , \_ -> Feature.Observation.update CancelObservationEdit failed |> Tuple.first |> .observations |> .edit |> Expect.equal Nothing
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Return should revalidate the protected owner"
        , test "one retained owner survives rows, tabs, Back, Apply and another edit action" <|
            \_ ->
                let
                    original =
                        savingEditModel "retained draft"

                    state =
                        original.observations

                    dirty =
                        { original | observations = { state | edit = Maybe.map (\edit -> { edit | saving = False, activeRequest = Nothing }) state.edit } }

                    other =
                        Feature.Observation.selectObservation "other" dirty |> Tuple.first

                    tabbed =
                        AppShell.handleOwned (AppShell.SwitchTabMsg ProjectsTab) other |> Tuple.first

                    back =
                        Route.handleUrlChange (workspaceUrl "tab=observations&observation=other") tabbed |> Tuple.first

                    applied =
                        Feature.Observation.update ApplyObservationFilters back |> Tuple.first

                    resumed =
                        Feature.Observation.update StartObservationEdit applied |> Tuple.first
                in
                Expect.all
                    [ \_ -> List.map (.observations >> .edit) [ other, tabbed, back, applied, resumed ] |> Expect.equal (List.repeat 5 dirty.observations.edit)
                    , \_ -> resumed.observations.selectedId |> Expect.equal (Just "curated")
                    , \_ -> resumed.activeTab |> Expect.equal ObservationsTab
                    , \_ -> resumed.observations.selectedDetail |> Maybe.map .id |> Expect.equal (Just "curated")
                    ]
                    ()
        , test "dirty and saving context exits preserve selection, workspace, session and canonical URL" <|
            \_ ->
                let
                    saving =
                        savingEditModel "protected exit"

                    state =
                        saving.observations

                    dirty =
                        { saving | observations = { state | edit = Maybe.map (\edit -> { edit | saving = False, activeRequest = Nothing }) state.edit } }

                    target path =
                        Url.fromString ("https://example.test" ++ path) |> Maybe.withDefault saving.url

                    unchanged model after =
                        after.observations == model.observations
                            && after.url == model.url
                            && after.page == model.page
                            && after.selectedWorkspaceId == model.selectedWorkspaceId
                            && after.sessionRequestEpoch == model.sessionRequestEpoch

                    stays model =
                        List.all
                            (\url ->
                                unchanged model (Route.handleUrlChange url model |> Tuple.first)
                                    && unchanged model (Route.handleUrlRequest (Browser.Internal url) model |> Tuple.first)
                            )
                            [ target "/", target "/audit", target "/workspace/workspace-2", target "/missing" ]
                            && unchanged model (Route.handleUrlRequest (Browser.External "https://elsewhere.test") model |> Tuple.first)
                            && unchanged model (AppShell.handleOwned (AppShell.SelectWorkspaceMsg "workspace-2") model |> Tuple.first)
                in
                [ stays saving, stays dirty ] |> Expect.equal [ True, True ]
        , test "retained draft controls remain reachable on other tabs and cannot interrupt a save" <|
            \_ ->
                let
                    saving =
                        savingEditModel "reachable draft"

                    tabbed =
                        AppShell.handleOwned (AppShell.SwitchTabMsg ProjectsTab) saving |> Tuple.first

                    notice =
                        Feature.Observation.viewRetainedDraft tabbed |> Query.fromHtml

                    returned =
                        Feature.Observation.update ReturnToObservationDraft tabbed |> Tuple.first
                in
                Expect.all
                    [ \_ -> notice |> Query.has [ Selector.text "Return to draft", Selector.text "Discard draft", Selector.text "Saving..." ]
                    , \_ -> notice |> Query.find [ Selector.tag "button", Selector.containing [ Selector.text "Return to draft" ] ] |> Event.simulate Event.click |> Event.expect ReturnToObservationDraft
                    , \_ -> notice |> Query.find [ Selector.tag "button", Selector.containing [ Selector.text "Discard draft" ] ] |> Query.has [ Selector.attribute (Html.Attributes.disabled True) ]
                    , \_ -> Feature.Observation.update CancelObservationEdit tabbed |> Tuple.first |> .observations |> .edit |> Expect.equal saving.observations.edit
                    , \_ -> Feature.Observation.update (SetObservationDraft "unexpected input") tabbed |> Tuple.first |> .observations |> .edit |> Expect.equal saving.observations.edit
                    , \_ -> returned.activeTab |> Expect.equal ObservationsTab
                    , \_ -> returned.observations.selectedId |> Expect.equal (Just "curated")
                    ]
                    ()
        , test "background save completion owns only the retained edit and preserves the new selection" <|
            \_ ->
                let
                    saving =
                        savingEditModel "saved retained draft"

                    other =
                        Feature.Observation.selectObservation "other" saving |> Tuple.first

                    otherObservation =
                        fixtureObservation "other" "2026-01-01T00:00:00Z"

                    otherState =
                        other.observations

                    browsing =
                        { other | observations = { otherState | selectedDetail = Just otherObservation, detailLoading = False } }
                in
                case saving.observations.edit |> Maybe.andThen .activeRequest of
                    Just request ->
                        let
                            saved =
                                saving.observations.edit |> Maybe.map .latestCanonical |> Maybe.withDefault otherObservation

                            completed =
                                Feature.Observation.update (ObservationUpdated request (Ok { saved | content = "saved retained draft", updatedAt = "2026-01-02T00:00:00Z" })) browsing |> Tuple.first

                            failed =
                                Feature.Observation.update (ObservationUpdated request (Err (Api.ObservationUpdateHttpError Http.NetworkError))) browsing |> Tuple.first

                            revoked =
                                Feature.Observation.reconcileCurationPermission { browsing | sessionContext = Just readOnlySession }

                            deleted =
                                Feature.Observation.reconcileDeletedObservation "curated" browsing |> Tuple.first
                        in
                        Expect.all
                            [ \_ -> completed.observations.edit |> Expect.equal Nothing
                            , \_ -> completed.observations.selectedId |> Expect.equal (Just "other")
                            , \_ -> completed.observations.selectedDetail |> Expect.equal (Just otherObservation)
                            , \_ -> failed.observations.edit |> Maybe.map .draft |> Expect.equal (Just "saved retained draft")
                            , \_ -> Feature.Observation.update (ObservationUpdated request (Ok saved)) revoked |> Tuple.first |> .observations |> .edit |> Expect.equal Nothing
                            , \_ -> Feature.Observation.update (ObservationUpdated request (Ok saved)) deleted |> Tuple.first |> .observations |> .edit |> Expect.equal Nothing
                            , \_ -> Feature.Observation.isLoadedOrSelected "curated" { otherState | items = Dict.empty } |> Expect.equal True
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Expected retained save ownership"
        , test "full snapshots reconcile hidden dirty and saving owners without admitting unrelated membership" <|
            \_ ->
                let
                    saving =
                        savingEditModel "hidden draft"

                    selected =
                        Feature.Observation.selectObservation "other" saving |> Tuple.first

                    state =
                        selected.observations

                    other =
                        fixtureObservation "other" "2026-01-01T00:00:00Z"

                    hidden =
                        { selected | observations = { state | items = Dict.empty, orderedIds = [], selectedDetail = Just other } }

                    original =
                        saving.observations.edit |> Maybe.map .latestCanonical |> Maybe.withDefault other

                    changed =
                        { original | content = "new canonical content", updatedAt = "2026-01-02T00:00:00Z" }

                    apply rows model =
                        Feature.WebSocket.update (WsMessageReceived (observationSnapshotWireMany rows)) model |> Tuple.first

                    updated =
                        apply [ changed, other, fixtureObservation "unrelated" "2026-01-01T00:00:00Z" ] hidden

                    dirtyState =
                        hidden.observations

                    dirty =
                        { hidden | observations = { dirtyState | edit = Maybe.map (\edit -> { edit | saving = False, activeRequest = Nothing }) dirtyState.edit } }

                    changedDirty =
                        apply [ changed, other ] dirty

                    older =
                        apply [ original, other ] updated

                    deleted =
                        apply [ other ] hidden

                    sparseWire =
                        observationSnapshotWireMany [] |> String.replace "\"transport\":\"snapshot\"" "\"transport\":\"snapshot\",\"snapshot_profile\":\"workspace_shell_v1\""

                    sparse =
                        Feature.WebSocket.update (WsMessageReceived sparseWire) hidden |> Tuple.first
                in
                Expect.all
                    [ \_ -> updated.observations.edit |> Maybe.map (\edit -> ( edit.draft, edit.latestCanonical.content, edit.conflict )) |> Expect.equal (Just ( "hidden draft", "new canonical content", True ))
                    , \_ -> updated.observations.edit |> Maybe.andThen .activeRequest |> Expect.equal (saving.observations.edit |> Maybe.andThen .activeRequest)
                    , \_ -> changedDirty.observations.edit |> Maybe.map .conflict |> Expect.equal (Just True)
                    , \_ -> updated.observations.selectedId |> Expect.equal (Just "other")
                    , \_ -> updated.observations.selectedDetail |> Expect.equal (Just other)
                    , \_ -> updated.observations.items |> Expect.equal Dict.empty
                    , \_ -> older.observations.edit |> Expect.equal updated.observations.edit
                    , \_ -> deleted.observations.edit |> Expect.equal Nothing
                    , \_ -> deleted.observations.selectedId |> Expect.equal (Just "other")
                    , \_ -> sparse.observations.edit |> Expect.equal hidden.observations.edit
                    , \_ ->
                        case saving.observations.edit |> Maybe.andThen .activeRequest of
                            Just request ->
                                Feature.Observation.update (ObservationUpdated request (Ok changed)) deleted |> Tuple.first |> .observations |> Expect.equal deleted.observations

                            Nothing ->
                                Expect.fail "Expected original request"
                    ]
                    ()
        , test "authoritative workspace retirement clears hidden dirty and saving owners and fences callbacks" <|
            \_ ->
                let
                    saving =
                        let
                            requested =
                                Feature.Observation.update RefreshObservationResults (savingEditModel "retirement draft") |> Tuple.first
                        in
                        applyObservationPage (observationPageMessage 0 (Err Http.NetworkError) requested) requested

                    hidden =
                        Feature.Observation.selectObservation "other" saving |> Tuple.first

                    state =
                        hidden.observations

                    dirty =
                        { hidden | observations = { state | edit = Maybe.map (\edit -> { edit | saving = False, activeRequest = Nothing }) state.edit } }

                    revokedWire workspaceId =
                        "{\"schema_version\":1,\"transport\":\"frames\",\"scope\":{\"scope\":\"workspace\",\"workspace_id\":\"" ++ workspaceId ++ "\"},\"frames\":[{\"schema_version\":1,\"type\":\"access_revoked\",\"workspace_id\":\"" ++ workspaceId ++ "\"}]}"

                    deletedWire =
                        observationInvalidationWire "workspace-1"
                            |> String.replace "\"type\":\"observation\"" "\"type\":\"workspace\""
                            |> String.replace "\"action\":\"updated\"" "\"action\":\"deleted\""
                            |> String.replace "\"scope\":{\"scope\":\"workspace\",\"workspace_id\":\"workspace-1\"}" "\"scope\":{\"scope\":\"global\"}"
                            |> String.replace "\"scope\":\"workspace\"" "\"scope\":\"global\""
                            |> String.replace "\"workspace_id\":\"workspace-1\"" "\"workspace_id\":null"
                            |> String.replace "observation:workspace-1" "workspace:workspace-1"
                            |> String.replace "{\"kind\":\"collection\",\"target\":\"observations:workspace-1\"}" "{\"kind\":\"catalogue\",\"target\":\"workspace-catalog\"}"

                    retire wire model =
                        Feature.WebSocket.update (WsMessageReceived wire) model |> Tuple.first

                    retired =
                        List.concatMap (\model -> List.map (\wire -> retire wire model) [ revokedWire "workspace-1", deletedWire ]) [ hidden, dirty ]

                    unrelated =
                        retire (revokedWire "other-workspace") hidden
                in
                case saving.observations.edit |> Maybe.andThen .activeRequest of
                    Just request ->
                        let
                            canonical =
                                saving.observations.edit |> Maybe.map .latestCanonical |> Maybe.withDefault (fixtureObservation "curated" "2026-01-01T00:00:00Z")

                            inert model =
                                model.observations.edit == Nothing
                                    && model.observations.failedRequest == Nothing
                                    && model.sessionRequestEpoch == hidden.sessionRequestEpoch + 1
                                    && (Feature.Observation.update RetryObservationResults model |> Tuple.first |> .observations) == model.observations
                                    && (applyObservationPage (observationPageMessage 0 (Err Http.NetworkError) saving) model).observations == model.observations
                                    && (Feature.Observation.update (ObservationUpdated request (Ok canonical)) model |> Tuple.first |> .observations) == model.observations
                                    && (Feature.Observation.update (ObservationUpdated request (Err (Api.ObservationUpdateHttpError Http.NetworkError))) model |> Tuple.first |> .observations) == model.observations
                        in
                        Expect.all
                            [ \_ -> List.map inert retired |> Expect.equal [ True, True, True, True ]
                            , \_ -> unrelated.observations.edit |> Expect.equal hidden.observations.edit
                            , \_ -> unrelated.sessionRequestEpoch |> Expect.equal hidden.sessionRequestEpoch
                            , \_ -> unrelated.observations.failedRequest |> Expect.equal hidden.observations.failedRequest
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Expected original owned save request"
        , test "clean or explicitly discarded edits allow immediate context navigation" <|
            \_ ->
                let
                    saving =
                        savingEditModel "draft to discard"

                    state =
                        saving.observations

                    dirty =
                        { saving | observations = { state | edit = Maybe.map (\edit -> { edit | saving = False, activeRequest = Nothing }) state.edit } }

                    discarded =
                        Feature.Observation.update CancelObservationEdit dirty |> Tuple.first

                    clean =
                        Feature.Observation.update StartObservationEdit discarded |> Tuple.first

                    destination =
                        Url.fromString "https://example.test/" |> Maybe.withDefault clean.url
                in
                List.map (\model -> Route.handleUrlChange destination model |> Tuple.first |> .page) [ discarded, clean ]
                    |> Expect.equal [ HomePage, HomePage ]
        , test "same-workspace route exits clear stale Observation detail state" <|
            \_ ->
                let
                    request =
                        { workspaceId = "workspace-1", observationId = "selected-observation", sessionEpoch = 0, token = 7 }

                    selected =
                        let
                            initial =
                                Feature.Observation.init
                        in
                        { initial
                            | selectedId = Just "selected-observation"
                            , selectedDetail = Just (fixtureObservation "selected-observation" "2026-01-01T00:00:00Z")
                            , detailLoading = True
                            , detailError = Just "stale detail error"
                            , activeDetailRequest = Just request
                        }

                    leaves tab =
                        let
                            afterRoute =
                                Route.handleUrlChange
                                    (workspaceUrl (Helpers.buildFragment tab Nothing Nothing))
                                    (sameWorkspaceModel selected)
                                    |> Tuple.first
                        in
                        [ afterRoute.activeTab == tab
                        , afterRoute.observations.selectedId == Nothing
                        , afterRoute.observations.selectedDetail == Nothing
                        , afterRoute.observations.detailLoading == False
                        , afterRoute.observations.detailError == Nothing
                        , afterRoute.observations.activeDetailRequest == Nothing
                        , Feature.Observation.detailResponseMatches "workspace-1" "selected-observation" 0 7 (Just "workspace-1") 0 afterRoute.observations == False
                        ]
                            == [ True, True, True, True, True, True, True ]
                in
                [ leaves ProjectsTab, leaves TimelineTab, leaves AuditTab ]
                    |> Expect.equal [ True, True, True ]
        , test "same-workspace direct Observation routes select and request detail" <|
            \_ ->
                let
                    afterRoute =
                        Route.handleUrlChange
                            (workspaceUrl "tab=observations&observation=direct-observation")
                            (sameWorkspaceModel Feature.Observation.init)
                            |> Tuple.first
                in
                [ afterRoute.activeTab == ObservationsTab
                , afterRoute.observations.selectedId == Just "direct-observation"
                , afterRoute.observations.selectedDetail == Nothing
                , afterRoute.observations.detailLoading
                , afterRoute.observations.detailError == Nothing
                , afterRoute.observations.activeDetailRequest
                    == Just
                        { workspaceId = "workspace-1"
                        , observationId = "direct-observation"
                        , sessionEpoch = afterRoute.sessionRequestEpoch
                        , token = 1
                        }
                ]
                    |> Expect.equal [ True, True, True, True, True, True ]
        , test "session response epochs reject token replacement and removal races" <|
            \_ ->
                [ AppShell.sessionEpochMatches 2 2
                , AppShell.sessionEpochMatches 1 2
                , AppShell.sessionEpochMatches 2 3
                ]
                    |> Expect.equal [ True, False, False ]
        , test "renders unavailable, loading, empty, error, provenance, detail, and pagination observation states" <|
            \_ ->
                let
                    empty =
                        Feature.Observation.init

                    repository =
                        observationWorkspace Api.Repository

                    unavailable =
                        observationWorkspace Api.Personal

                    baseGlob =
                        fixtureObservation "glob" "2026-01-01T00:00:00Z"

                    glob =
                        { baseGlob
                            | subjects = [ { subjectKind = Api.SubjectGlob, subject = "src/**/*.elm" } ]
                            , subjectKind = Api.SubjectGlob
                            , subject = "src/**/*.elm"
                        }

                    loaded =
                        let
                            requested =
                                Feature.Observation.startReload repository.id empty
                        in
                        { requested
                            | items = Dict.singleton "glob" glob
                            , orderedIds = [ "glob" ]
                            , hasMore = True
                            , loading = False
                            , expectedOffset = Nothing
                            , selectedId = Just "glob"
                            , selectedDetail = Just (fixtureObservation "glob" "2026-01-02T00:00:00Z")
                        }

                    detailLoading =
                        { empty | selectedId = Just "glob", detailLoading = True }

                    detailError =
                        { empty | selectedId = Just "glob", detailError = Just "Failed to load observation detail." }

                    paginating =
                        { loaded | loading = True }
                in
                Expect.all
                    [ \_ -> Feature.Observation.viewObservationsState unavailable empty |> Query.fromHtml |> Query.has [ Selector.text "Observations unavailable" ]
                    , \_ -> Feature.Observation.viewObservationsState repository { empty | loading = True } |> Query.fromHtml |> Query.has [ Selector.text "Loading observations..." ]
                    , \_ -> Feature.Observation.viewObservationsState repository empty |> Query.fromHtml |> Query.has [ Selector.text "No observations found" ]
                    , \_ -> Feature.Observation.viewObservationsState repository { empty | error = Just "Failed to load observations." } |> Query.fromHtml |> Query.has [ Selector.text "Unable to load observations", Selector.text "Failed to load observations." ]
                    , \_ -> Feature.Observation.viewObservationsState repository loaded |> Query.fromHtml |> Query.has [ Selector.text "Glob", Selector.text "src/**/*.elm", Selector.text "Provenance revision (Git SHA)", Selector.text fullSha, Selector.text "Subject kind", Selector.text "File", Selector.text "Subject", Selector.text "src/Main.elm" ]
                    , \_ -> Feature.Observation.viewObservationsState repository loaded |> Query.fromHtml |> Query.find [ Selector.tag "button", Selector.containing [ Selector.text "Load more" ] ] |> Query.hasNot [ Selector.disabled True ]
                    , \_ -> Feature.Observation.viewObservationsState repository paginating |> Query.fromHtml |> Query.find [ Selector.tag "button", Selector.containing [ Selector.text "Loading..." ] ] |> Query.has [ Selector.disabled True ]
                    , \_ -> Feature.Observation.viewObservationsState repository detailLoading |> Query.fromHtml |> Query.has [ Selector.text "Loading detail..." ]
                    , \_ -> Feature.Observation.viewObservationsState repository detailError |> Query.fromHtml |> Query.has [ Selector.text "Failed to load observation detail." ]
                    ]
                    ()
        , test "uses scoped app controls, selectable card rows, and structured detail metadata" <|
            \_ ->
                let
                    empty =
                        Feature.Observation.init

                    observation =
                        fixtureObservation "selected" "2026-01-01T00:00:00Z"

                    state =
                        { empty
                            | items = Dict.singleton observation.id observation
                            , orderedIds = [ observation.id ]
                            , selectedId = Just observation.id
                            , selectedDetail = Just observation
                        }

                    view =
                        Feature.Observation.viewObservationsState (observationWorkspace Api.Repository) state
                            |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> view |> Query.find [ Selector.class "observation-search" ] |> Query.has [ Selector.class "filter-bar" ]
                    , \_ -> view |> Query.findAll [ Selector.class "observation-filters" ] |> Query.count (Expect.equal 3)
                    , \_ -> view |> Query.findAll [ Selector.class "observation-filter-input" ] |> Query.count (Expect.equal 4)
                    , \_ -> view |> Query.find [ Selector.class "observation-filter-select" ] |> Query.has [ Selector.tag "select", Selector.class "filter-select" ]
                    , \_ -> view |> Query.find [ Selector.class "observation-filter-apply" ] |> Query.has [ Selector.tag "button", Selector.class "btn", Selector.class "btn-primary", Selector.text "Apply filters" ]
                    , \_ -> view |> Query.find [ Selector.id (Feature.Observation.observationCardDomId "flat" "selected") ] |> Query.has [ Selector.tag "button", Selector.class "tree-toggle", Selector.class "observation-card", Selector.class "observation-card-selected" ]
                    , \_ -> view |> Query.find [ Selector.class "observation-list-rows" ] |> Query.has [ Selector.class "observation-card" ]
                    , \_ -> view |> Query.find [ Selector.class "observation-detail-card" ] |> Query.has [ Selector.tag "article", Selector.class "card", Selector.class "observation-detail-content", Selector.class "observation-detail-meta", Selector.text fullSha ]
                    ]
                    ()
        , test "discovery disclosures preserve applied requests, retained drafts, and pagination in every mode" <|
            \_ ->
                let
                    unchanged mode =
                        let
                            loaded =
                                appliedModeModel mode

                            pending =
                                Feature.Observation.update RefreshObservationResults loaded |> Tuple.first

                            original =
                                applyObservationPage (observationPageMessage 0 (Err Http.NetworkError) pending) pending |> draftQueryInputs

                            edited =
                                Feature.Observation.update StartObservationEdit original |> Tuple.first
                                    |> (\model -> Feature.Observation.update (SetObservationDraft "Protected discovery draft") model |> Tuple.first)

                            saving =
                                Feature.Observation.update SaveObservationEdit edited |> Tuple.first

                            preserved protected =
                                let
                                    opened =
                                        [ OpenObservationFileComposer, ToggleObservationAdvancedFilters ]
                                            |> List.foldl (\message model -> Feature.Observation.update message model |> Tuple.first) protected

                                    closed =
                                        [ CloseObservationFileComposer, ToggleObservationAdvancedFilters ]
                                            |> List.foldl (\message model -> Feature.Observation.update message model |> Tuple.first) opened

                                    openedState =
                                        opened.observations
                                in
                                closed == protected
                                    && { openedState | fileComposerOpen = False, advancedFiltersOpen = False } == protected.observations
                        in
                        edited.observations.edit /= Nothing
                            && edited.observations.refreshError /= Nothing
                            && (saving.observations.edit |> Maybe.map .saving) == Just True
                            && List.all preserved [ edited, saving ]
                in
                [ ObservationFlatMode, ObservationFacetMode, ObservationExactSubjectMode, ObservationMatchMode ]
                    |> List.all unchanged
                    |> Expect.equal True
        , test "native discovery controls expose disclosure state and explicit search submission" <|
            \_ ->
                let
                    initial = Feature.Observation.init
                    view state = Feature.Observation.viewObservationsState (observationWorkspace Api.Repository) state |> Query.fromHtml
                    opened = { initial | fileComposerOpen = True, advancedFiltersOpen = True }
                in
                Expect.all
                    [ \_ -> view initial |> Query.find [ Selector.id "observation-for-files" ] |> Query.has [ Selector.attribute (attribute "aria-expanded" "false"), Selector.attribute (attribute "aria-controls" "observation-file-composer") ]
                    , \_ -> view opened |> Query.find [ Selector.id "observation-advanced-toggle" ] |> Query.has [ Selector.attribute (attribute "aria-expanded" "true"), Selector.attribute (attribute "aria-controls" "observation-advanced-filters") ]
                    , \_ -> view initial |> Query.find [ Selector.id "observation-file-composer" ] |> Query.has [ Selector.attribute (hidden True) ]
                    , \_ -> view initial |> Query.find [ Selector.class "observation-search" ] |> Event.simulate Event.submit |> Event.expect ApplyObservationFilters
                    , \_ -> view initial |> Query.find [ Selector.id "observation-for-files" ] |> Event.simulate Event.click |> Event.expect OpenObservationFileComposer
                    ] ()
        , test "preview excerpts bound Unicode names while preserving full content and ordered provenance outside selection" <|
            \_ ->
                let
                    content = String.repeat 400 "Line <script> 😀\n" ++ "Full content end"
                    original = observationWithSubjects "preview"
                        [ { subjectKind = Api.SubjectFile, subject = String.repeat 120 "😀" ++ ".elm" }
                        , { subjectKind = Api.SubjectGlob, subject = "src/**/*.elm" }
                        , { subjectKind = Api.SubjectFile, subject = "src/Second.elm" }
                        ]
                    observation = { original | content = content }
                    selected = selectedObservationState observation
                    view = Feature.Observation.viewObservationsState (observationWorkspace Api.Repository) { selected | expandedSubjects = Dict.singleton observation.id True } |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> Helpers.plainTextExcerpt 3 "😀 😀 😀" |> Expect.equal "😀 …"
                    , \_ -> Helpers.plainTextExcerpt 240 content |> String.toList |> List.length |> Expect.equal 240
                    , \_ -> view |> Query.find [ Selector.class "observation-card" ] |> Query.hasNot [ Selector.tag "details", Selector.tag "script", Selector.class "observation-sha-copy" ]
                    , \_ -> view |> Query.has [ Selector.class "tree-toggle", Selector.text "▼" ]
                    , \_ -> view |> Query.find [ Selector.class "observation-detail-content" ] |> Query.has [ Selector.text content ]
                    , \_ -> view |> Query.find [ Selector.class "observation-detail-card" ] |> Query.has [ Selector.text "src/**/*.elm", Selector.text "src/Second.elm", Selector.text fullSha ]
                    , \_ -> view |> Query.find [ Selector.class "observation-detail-card" ] |> Query.has [ Selector.text "Provenance revision (Git SHA)", Selector.text "Content updated", Selector.class "copyable-value" ]
                    ] ()
        , test "Observation timestamps retain useful UTC update time and explicit non-UTC offsets" <|
            \_ ->
                [ Helpers.formatObservationTimestamp "2026-10-06T12:34:56.123Z"
                , Helpers.formatObservationTimestamp "2026-10-06T12:35:56+00:00"
                , Helpers.formatObservationTimestamp "2026-10-06T12:34:56+02:00"
                ] |> Expect.equal [ "2026-10-06 12:34:56.123 UTC", "2026-10-06 12:35:56 UTC", "2026-10-06 12:34:56+02:00" ]
        , test "large detail reader uses the UTF-8 threshold and retains an exact read-only value" <|
            \_ ->
                let
                    original = fixtureObservation "reader" "2026-01-01T00:00:00Z"
                    view content =
                        Feature.Observation.viewObservationsState (observationWorkspace Api.Repository)
                            (selectedObservationState { original | content = content }) |> Query.fromHtml
                    large = String.repeat 4097 "😀"
                in
                Expect.all
                    [ \_ -> view (String.repeat 16383 "a") |> Query.findAll [ Selector.id "observation-content-reader" ] |> Query.count (Expect.equal 0)
                    , \_ -> view (String.repeat 16384 "a") |> Query.findAll [ Selector.id "observation-content-reader" ] |> Query.count (Expect.equal 0)
                    , \_ -> view (String.repeat 4096 "😀") |> Query.findAll [ Selector.id "observation-content-reader" ] |> Query.count (Expect.equal 0)
                    , \_ -> view large |> Query.find [ Selector.id "observation-content-reader" ] |> Query.has [ Selector.tag "textarea", Selector.attribute (Html.Attributes.readonly True), Selector.attribute (Html.Attributes.value large) ]
                    ] ()
        , test "presentation states expose scoped loading, error, empty, and disabled pagination classes" <|
            \_ ->
                let
                    empty =
                        Feature.Observation.init

                    repository =
                        observationWorkspace Api.Repository

                    observation =
                        fixtureObservation "page" "2026-01-01T00:00:00Z"

                    paginating =
                        { empty
                            | items = Dict.singleton observation.id observation
                            , orderedIds = [ observation.id ]
                            , hasMore = True
                            , loading = True
                        }
                in
                Expect.all
                    [ \_ -> Feature.Observation.viewObservationsState repository { empty | loading = True } |> Query.fromHtml |> Query.find [ Selector.class "observation-state-loading" ] |> Query.has [ Selector.class "loading-indicator" ]
                    , \_ -> Feature.Observation.viewObservationsState repository empty |> Query.fromHtml |> Query.find [ Selector.class "observation-state-empty" ] |> Query.has [ Selector.class "empty-state" ]
                    , \_ -> Feature.Observation.viewObservationsState repository { empty | error = Just "Failed" } |> Query.fromHtml |> Query.find [ Selector.class "observation-state-error" ] |> Query.has [ Selector.text "Failed" ]
                    , \_ -> Feature.Observation.viewObservationsState repository paginating |> Query.fromHtml |> Query.find [ Selector.class "observation-load-more" ] |> Query.has [ Selector.class "btn-secondary", Selector.disabled True ]
                    , \_ -> Feature.Observation.viewObservationsState (observationWorkspace Api.Personal) empty |> Query.fromHtml |> Query.has [ Selector.class "observation-state-unavailable", Selector.class "empty-state" ]
                    ]
                    ()
        , test "ranked FTS and unfiltered pages retain server order when appended" <|
            \_ ->
                let
                    ranked =
                        Feature.DataLoading.mergeObservationPage 0
                            { items = [ fixtureObservation "ranked-first" "2026-03-01T00:00:00Z", fixtureObservation "ranked-second" "2025-01-01T00:00:00Z" ], hasMore = True }
                            (Feature.Observation.startReload "workspace-1" Feature.Observation.init)

                    unfiltered =
                        Feature.DataLoading.mergeObservationPage ranked.nextOffset
                            { items = [ fixtureObservation "server-third" "2027-01-01T00:00:00Z" ], hasMore = False }
                            ranked
                in
                [ ranked.orderedIds == [ "ranked-first", "ranked-second" ]
                , unfiltered.orderedIds == [ "ranked-first", "ranked-second", "server-third" ]
                ]
                    |> Expect.equal [ True, True ]
        , test "decodes canonical ordered subjects and the legacy singleton fallback" <|
            \_ ->
                let
                    canonical =
                        """{"id":"multi","workspace_id":"workspace-1","subjects":[{"subject_kind":"glob","subject":"src/**/*.elm"},{"subject_kind":"file","subject":"src/Main.elm"}],"git_sha":"0123456789abcdef0123456789abcdef01234567","content_version":"10000000-0000-4000-8000-000000000000","content":"Evidence","created_at":"2026-01-01T00:00:00Z","updated_at":"2026-01-01T00:00:00Z"}"""

                    canonicalSubjects =
                        Decode.decodeString Api.observationDecoder canonical
                            |> Result.map (List.map .subject << .subjects)

                    legacySubjects =
                        Decode.decodeString Api.observationDecoder fileFixture
                            |> Result.map (List.map .subject << .subjects)
                in
                [ canonicalSubjects == Ok [ "src/**/*.elm", "src/Main.elm" ]
                , legacySubjects == Ok [ "src/Main.elm" ]
                , Decode.decodeString Api.observationDecoder "{\"id\":\"bad\",\"workspace_id\":\"workspace-1\",\"subjects\":[],\"git_sha\":\"x\",\"content_version\":\"10000000-0000-4000-8000-000000000000\",\"content\":\"x\",\"created_at\":\"x\",\"updated_at\":\"x\"}" |> isErr
                , Decode.decodeString Api.observationDecoder "{\"id\":\"bad\",\"workspace_id\":\"workspace-1\",\"subjects\":[{\"subject_kind\":\"other\",\"subject\":\"x\"}],\"git_sha\":\"x\",\"content_version\":\"10000000-0000-4000-8000-000000000000\",\"content\":\"x\",\"created_at\":\"x\",\"updated_at\":\"x\"}" |> isErr
                , Decode.decodeString Api.observationDecoder "{\"id\":\"bad\",\"workspace_id\":\"workspace-1\",\"git_sha\":\"x\",\"content_version\":\"10000000-0000-4000-8000-000000000000\",\"content\":\"x\",\"created_at\":\"x\",\"updated_at\":\"x\"}" |> isErr
                , Decode.decodeString Api.observationDecoder "{\"id\":\"bad\",\"workspace_id\":\"workspace-1\",\"subjects\":\"not-an-array\",\"git_sha\":\"x\",\"content_version\":\"10000000-0000-4000-8000-000000000000\",\"content\":\"x\",\"created_at\":\"x\",\"updated_at\":\"x\"}" |> isErr
                , Decode.decodeString Api.observationDecoder "{\"id\":\"bad\",\"workspace_id\":\"workspace-1\",\"subjects\":[],\"subject_kind\":\"file\",\"subject\":\"src/legacy.elm\",\"git_sha\":\"x\",\"content_version\":\"10000000-0000-4000-8000-000000000000\",\"content\":\"x\",\"created_at\":\"x\",\"updated_at\":\"x\"}" |> isErr
                , Decode.decodeString Api.observationDecoder "{\"id\":\"bad\",\"workspace_id\":\"workspace-1\",\"subjects\":\"not-an-array\",\"subject_kind\":\"file\",\"subject\":\"src/legacy.elm\",\"git_sha\":\"x\",\"content_version\":\"10000000-0000-4000-8000-000000000000\",\"content\":\"x\",\"created_at\":\"x\",\"updated_at\":\"x\"}" |> isErr
                ]
                    |> Expect.equal [ True, True, True, True, True, True, True, True ]
        , test "normalizes bounded concrete match paths and encodes the match request" <|
            \_ ->
                let
                    body =
                        Api.observationMatchBody
                            { workspaceId = "workspace-1"
                            , paths = [ "src/Main.elm", "my/src/proj/Main.java" ]
                            , subjectKind = Just Api.SubjectGlob
                            , gitSha = Just fullSha
                            , query = Just "render"
                            , limit = 50
                            , offset = 0
                            }
                            |> Encode.encode 0
                in
                [ Feature.Observation.normalizeMatchPaths "src/Main.elm\nsrc/Main.elm\nmy/src/proj/Main.java" == Ok [ "src/Main.elm", "my/src/proj/Main.java" ]
                , Feature.Observation.normalizeMatchPaths "src/**/*.elm" |> isErr
                , Feature.Observation.normalizeMatchPaths "/src/Main.elm" |> isErr
                , Feature.Observation.normalizeMatchPaths "C:/repo/Main.elm" |> isErr
                , Feature.Observation.normalizeMatchPaths "src/\u{0007}Main.elm" |> isErr
                , Feature.Observation.normalizeMatchPaths (String.repeat 4096 "a") |> isOk
                , Feature.Observation.normalizeMatchPaths (String.repeat 4097 "a") |> isErr
                , Feature.Observation.normalizeMatchPaths (String.repeat 1366 "€") |> isErr
                , Feature.Observation.normalizeMatchPaths (largePathInput 256 1025) |> isErr
                , body == "{\"workspace_id\":\"workspace-1\",\"paths\":[\"src/Main.elm\",\"my/src/proj/Main.java\"],\"limit\":50,\"offset\":0,\"subject_kind\":\"glob\",\"git_sha\":\"0123456789abcdef0123456789abcdef01234567\",\"query\":\"render\"}"
                ]
                    |> Expect.equal [ True, True, True, True, True, True, True, True, True, True ]
        , test "renders grouped canonical match evidence without constructing collapsed duplicate cards" <|
            \_ ->
                let
                    baseObservation =
                        fixtureObservation "matched" "2026-01-01T00:00:00Z"

                    observation =
                        { baseObservation
                            | subjects =
                                [ { subjectKind = Api.SubjectFile, subject = "src/Main.elm" }
                                , { subjectKind = Api.SubjectGlob, subject = "src/**/*.elm" }
                                ]
                        }

                    baseState =
                        Feature.Observation.init

                    state =
                        { baseState
                            | items = Dict.singleton observation.id observation
                            , orderedIds = [ observation.id ]
                            , selectedId = Just observation.id
                            , selectedDetail = Just observation
                            , requestMode = ObservationMatchMode
                            , matchPathsInput = "src/Main.elm"
                            , matchAppliedPaths = [ "src/Main.elm" ]
                            , requestGeneration = 4
                            , queryFingerprint = "match-request"
                            , expectedOffset = Just 0
                            , matchEvidence =
                                Dict.singleton observation.id
                                    { observation = observation
                                    , pathMatches =
                                        [ { path = "src/Main.elm"
                                          , matchedSubjects = List.drop 1 observation.subjects
                                          }
                                        ]
                                    , matchedPaths = [ "src/Main.elm" ]
                                    , matchedSubjects = List.drop 1 observation.subjects
                                    }
                        }

                    view =
                        Feature.Observation.viewObservationsState (observationWorkspace Api.Repository) state |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> view |> Query.find [ Selector.class "observation-match-results" ] |> Query.has [ Selector.text "src/Main.elm", Selector.text "src/**/*.elm", Selector.text "1 loaded", Selector.class "copyable-value" ]
                    , \_ -> view |> Query.find [ Selector.class "observation-match-results" ] |> Query.findAll [ Selector.class "observation-card" ] |> Query.count (Expect.equal 0)
                    , \_ -> view |> Query.find [ Selector.id "observation-match-paths" ] |> Query.has [ Selector.tag "textarea" ]
                    , \_ -> Feature.Observation.matchResponseMatches 0 4 "match-request" 0 state |> Expect.equal True
                    , \_ -> Feature.DataLoading.observationResponseMatches 4 "match-request" 0 state |> Expect.equal True
                    ]
                    ()
        , test "decodes paginated V020 singleton and V021 multi-subject match evidence" <|
            \_ ->
                let
                    response =
                        """{"items":[{"observation":{"id":"v020","workspace_id":"workspace-1","subject_kind":"file","subject":"src/Legacy.elm","git_sha":"0123456789abcdef0123456789abcdef01234567","content_version":"10000000-0000-4000-8000-000000000000","content":"Legacy","created_at":"2026-01-01T00:00:00Z","updated_at":"2026-01-01T00:00:00Z"},"matched_paths":["src/Legacy.elm"],"matched_subjects":[{"subject_kind":"file","subject":"src/Legacy.elm"}]},{"observation":{"id":"v021","workspace_id":"workspace-1","subjects":[{"subject_kind":"file","subject":"src/Main.elm"},{"subject_kind":"glob","subject":"src/**/*.elm"}],"git_sha":"0123456789abcdef0123456789abcdef01234567","content_version":"10000000-0000-4000-8000-000000000000","content":"Canonical","created_at":"2026-01-01T00:00:00Z","updated_at":"2026-01-01T00:00:00Z"},"matched_paths":["src/Main.elm"],"matched_subjects":[{"subject_kind":"glob","subject":"src/**/*.elm"}]}],"has_more":true}"""

                    decoded =
                        Decode.decodeString (Api.paginatedDecoder Api.observationMatchDecoder) response
                in
                case decoded of
                    Ok page ->
                        [ page.hasMore
                        , List.map (\match -> List.map .subject match.observation.subjects) page.items == [ [ "src/Legacy.elm" ], [ "src/Main.elm", "src/**/*.elm" ] ]
                        , List.map (\match -> List.map .subject match.matchedSubjects) page.items == [ [ "src/Legacy.elm" ], [ "src/**/*.elm" ] ]
                        ]
                            |> Expect.equal [ True, True, True ]

                    Err _ ->
                        Expect.fail "match response should decode"
        , test "match mode has accessible controls and distinct loading, empty, error, and pagination states" <|
            \_ ->
                let
                    initial =
                        Feature.Observation.init

                    base =
                        let
                            requested =
                                Feature.Observation.update ApplyObservationMatch (editableModel { initial | matchPathsInput = "src/Main.elm" }) |> Tuple.first |> .observations
                        in
                        { requested | loading = False, expectedOffset = Nothing }

                    baseObservation =
                        fixtureObservation "page" "2026-01-01T00:00:00Z"

                    observation =
                        { baseObservation
                            | subjects =
                                [ { subjectKind = Api.SubjectFile, subject = "src/Main.elm" }
                                , { subjectKind = Api.SubjectGlob, subject = "src/**/*.elm" }
                                ]
                        }

                    loaded =
                        { base
                            | items = Dict.singleton observation.id observation
                            , orderedIds = [ observation.id ]
                            , selectedId = Just observation.id
                            , selectedDetail = Just observation
                            , hasMore = True
                            , matchEvidence =
                                Dict.singleton observation.id
                                    { observation = observation
                                    , pathMatches =
                                        [ { path = "src/Main.elm"
                                          , matchedSubjects = List.drop 1 observation.subjects
                                          }
                                        ]
                                    , matchedPaths = [ "src/Main.elm" ]
                                    , matchedSubjects = List.drop 1 observation.subjects
                                    }
                        }

                    repository =
                        observationWorkspace Api.Repository
                in
                Expect.all
                    [ \_ -> Feature.Observation.viewObservationsState repository { base | loading = True } |> Query.fromHtml |> Query.has [ Selector.text "Matching repository files..." ]
                    , \_ -> Feature.Observation.viewObservationsState repository base |> Query.fromHtml |> Query.has [ Selector.text "No matching observations", Selector.text "Every supplied path is shown below. Try different paths or clear the match.", Selector.text "No loaded matches for this path." ]
                    , \_ -> Feature.Observation.viewObservationsState repository { base | error = Just "Failed to match repository files." } |> Query.fromHtml |> Query.has [ Selector.text "Failed to match repository files." ]
                    , \_ -> Feature.Observation.viewObservationsState repository loaded |> Query.fromHtml |> Query.hasNot [ Selector.id "observation-subject" ]
                    , \_ -> Feature.Observation.viewObservationsState repository loaded |> Query.fromHtml |> Query.has [ Selector.class "observation-subject-copy", Selector.text "1 loaded", Selector.text "src/Main.elm", Selector.text "src/**/*.elm" ]
                    , \_ -> Feature.Observation.viewObservationsState repository loaded |> Query.fromHtml |> Query.findAll [ Selector.class "observation-subject-copy", Selector.tag "button" ] |> Query.count (Expect.equal 1)
                    , \_ -> Feature.Observation.viewObservationsState repository loaded |> Query.fromHtml |> Query.find [ Selector.class "observation-load-more" ] |> Query.hasNot [ Selector.disabled True ]
                    ]
                    ()
        , test "late list, match, and workspace responses cannot overwrite an active match" <|
            \_ ->
                let
                    base =
                        Feature.Observation.init

                    matchState =
                        { base
                            | requestMode = ObservationMatchMode
                            , requestGeneration = 9
                            , queryFingerprint = "match-fingerprint"
                            , expectedOffset = Just 0
                        }

                    workspace =
                        observationWorkspace Api.Repository

                    baseModel =
                        sameWorkspaceModel matchState

                    model =
                        { baseModel | selectedWorkspaceId = Just workspace.id, workspaces = Dict.singleton workspace.id workspace }

                    listResult =
                        Feature.DataLoading.update
                            (GotObservations workspace.id Nothing 9 "match-fingerprint" 0 (Ok { items = [ fixtureObservation "late-list" "2026-01-01T00:00:00Z" ], hasMore = False }))
                            model
                            |> Tuple.first

                    wrongWorkspaceResult =
                        Feature.Observation.update
                            (GotObservationMatches "other-workspace" 0 9 "match-fingerprint" 0 (Ok { items = [], hasMore = False }))
                            model
                            |> Tuple.first

                    wrongMatchResult =
                        Feature.Observation.update
                            (GotObservationMatches workspace.id 0 8 "match-fingerprint" 0 (Ok { items = [], hasMore = False }))
                            model
                            |> Tuple.first
                in
                [ Dict.isEmpty listResult.observations.items
                , wrongWorkspaceResult.observations.expectedOffset == Just 0
                , wrongMatchResult.observations.expectedOffset == Just 0
                , Feature.DataLoading.listObservationResponseMatches 9 "match-fingerprint" 0 matchState == False
                ]
                    |> Expect.equal [ True, True, True, True ]
        , test "workspace batch tokens reject overlap and retain pending work for stale responses" <|
            \_ ->
                let
                    first =
                        Feature.DataLoading.prepareForPageLoad (WorkspacePage "workspace-1") Feature.DataLoading.init

                    second =
                        Feature.DataLoading.prepareForPageLoad (WorkspacePage "workspace-1") first

                    loadingBatch =
                        { second | pendingWorkspaceLoads = 3, loadingWorkspaceData = True }

                    staleFinished =
                        Feature.DataLoading.finishWorkspaceLoad (Just 1) loadingBatch

                    oneFinished =
                        Feature.DataLoading.finishWorkspaceLoad (Just 2) loadingBatch

                    twoFinished =
                        Feature.DataLoading.finishWorkspaceLoad (Just 2) oneFinished

                    complete =
                        Feature.DataLoading.finishWorkspaceLoad (Just 2) twoFinished
                in
                [ first.activeWorkspaceLoadToken == Just 1
                , second.activeWorkspaceLoadToken == Just 2
                , Feature.DataLoading.acceptWorkspaceLoad (Just 1) loadingBatch == False
                , Feature.DataLoading.acceptWorkspaceLoad (Just 2) loadingBatch
                , ( staleFinished.pendingWorkspaceLoads, staleFinished.loadingWorkspaceData ) == ( 3, True )
                , oneFinished.pendingWorkspaceLoads == 2
                , twoFinished.pendingWorkspaceLoads == 1
                , complete.pendingWorkspaceLoads == 0
                , complete.loadingWorkspaceData == False
                ]
                    |> Expect.equal [ True, True, True, True, True, True, True, True, True ]
        , test "editing filters without Apply disables pagination for the old result set" <|
            \_ ->
                let
                    applied =
                        Feature.Observation.startReload "workspace-1" Feature.Observation.init

                    loaded =
                        { applied | loading = False, hasMore = True, expectedOffset = Nothing, nextOffset = 50 }

                    editedDraft =
                        { loaded | query = "different query" }
                in
                [ Feature.Observation.canLoadMore "workspace-1" loaded
                , Feature.Observation.canLoadMore "workspace-1" editedDraft
                ]
                    |> Expect.equal [ True, False ]
        , test "only the current detail token accepts A to B to A responses" <|
            \_ ->
                let
                    finalA =
                        { workspaceId = "workspace-1", observationId = "A", sessionEpoch = 4, token = 3 }

                    base =
                        Feature.Observation.init

                    state =
                        { base | selectedId = Just "A", activeDetailRequest = Just finalA }
                in
                [ Feature.Observation.detailResponseMatches "workspace-1" "A" 4 1 (Just "workspace-1") 4 state
                , Feature.Observation.detailResponseMatches "workspace-1" "B" 4 2 (Just "workspace-1") 4 state
                , Feature.Observation.detailResponseMatches "workspace-1" "A" 4 3 (Just "workspace-1") 4 state
                , Feature.Observation.detailResponseMatches "workspace-1" "A" 4 3 (Just "workspace-2") 4 state
                , Feature.Observation.detailResponseMatches "workspace-1" "A" 4 3 (Just "workspace-1") 5 state
                ]
                    |> Expect.equal [ False, False, True, False, False ]
        , test "generation, full query fingerprint, and expected page reject stale responses" <|
            \_ ->
                let
                    initial =
                        Feature.Observation.startReload "workspace-1" Feature.Observation.init

                    changedFilter =
                        Feature.Observation.startReload "workspace-1" { initial | query = "new query", selectedId = Just "search-only-hit", detailLoading = True }
                in
                [ changedFilter.selectedId == Just "search-only-hit"
                , changedFilter.detailLoading
                , Feature.DataLoading.observationResponseMatches initial.requestGeneration initial.queryFingerprint 0 changedFilter
                , Feature.DataLoading.observationResponseMatches changedFilter.requestGeneration changedFilter.queryFingerprint 50 changedFilter
                , Feature.DataLoading.observationResponseMatches changedFilter.requestGeneration changedFilter.queryFingerprint 0 changedFilter
                ]
                    |> Expect.equal [ True, True, False, False, True ]
        , test "decodes subject facets and composes their filtered paginated URL" <|
            \_ ->
                let
                    decoded =
                        Decode.decodeString Api.observationSubjectFacetDecoder
                            """{"subject_kind":"glob","subject":"src/**/*.elm","observation_count":17,"latest_updated_at":"2026-08-30T12:00:00Z"}"""
                            |> Result.map
                                (\facet ->
                                    { subjectKind = facet.subjectKind
                                    , subject = facet.subject
                                    , observationCount = facet.observationCount
                                    , latestUpdatedAt = facet.latestUpdatedAt
                                    }
                                )

                    url =
                        Api.observationSubjectFacetsUrl "https://api.example"
                            { workspaceId = "workspace/a"
                            , subjectKind = Just Api.SubjectGlob
                            , gitSha = Just fullSha
                            , query = Just "render & test"
                            , limit = 25
                            , offset = 75
                            }
                in
                Expect.all
                    [ \_ ->
                        decoded
                            |> Expect.equal
                                (Ok
                                    { subjectKind = Api.SubjectGlob
                                    , subject = "src/**/*.elm"
                                    , observationCount = 17
                                    , latestUpdatedAt = "2026-08-30T12:00:00Z"
                                    }
                                )
                    , \_ -> Decode.decodeString Api.observationSubjectFacetDecoder "{\"subject_kind\":\"glob\",\"subject\":\"src/**/*.elm\",\"observation_count\":\"17\",\"latest_updated_at\":\"now\"}" |> isErr |> Expect.equal True
                    , \_ -> url |> Expect.equal ("https://api.example/api/v1/observations/subject-facets?workspace_id=workspace%2Fa&subject_kind=glob&git_sha=" ++ fullSha ++ "&query=render%20%26%20test&limit=25&offset=75")
                    ]
                    ()
        , test "decodes canonical path correlation while retaining legacy match arrays only for compatibility" <|
            \_ ->
                let
                    canonical =
                        """{"observation":{"id":"canonical","workspace_id":"workspace-1","subjects":[{"subject_kind":"file","subject":"src/Main.elm"},{"subject_kind":"glob","subject":"src/**/*.elm"}],"git_sha":"0123456789abcdef0123456789abcdef01234567","content_version":"10000000-0000-4000-8000-000000000000","content":"Canonical","created_at":"2026-01-01T00:00:00Z","updated_at":"2026-01-01T00:00:00Z"},"matched_paths":["src/Main.elm"],"matched_subjects":[{"subject_kind":"file","subject":"src/Main.elm"},{"subject_kind":"glob","subject":"src/**/*.elm"}],"path_matches":[{"path":"src/Main.elm","matched_subjects":[{"subject_kind":"file","subject":"src/Main.elm"},{"subject_kind":"glob","subject":"src/**/*.elm"}]}]}"""

                    legacy =
                        """{"observation":{"id":"legacy","workspace_id":"workspace-1","subject_kind":"file","subject":"src/Legacy.elm","git_sha":"0123456789abcdef0123456789abcdef01234567","content_version":"10000000-0000-4000-8000-000000000000","content":"Legacy","created_at":"2026-01-01T00:00:00Z","updated_at":"2026-01-01T00:00:00Z"},"matched_paths":["src/Legacy.elm"],"matched_subjects":[{"subject_kind":"file","subject":"src/Legacy.elm"}]}"""
                in
                Expect.all
                    [ \_ ->
                        Decode.decodeString Api.observationMatchDecoder canonical
                            |> Result.map (\item -> List.map (\pathMatch -> ( pathMatch.path, List.map .subject pathMatch.matchedSubjects )) item.pathMatches)
                            |> Expect.equal (Ok [ ( "src/Main.elm", [ "src/Main.elm", "src/**/*.elm" ] ) ])
                    , \_ ->
                        Decode.decodeString Api.observationMatchDecoder legacy
                            |> Result.map (\item -> List.map .path item.pathMatches)
                            |> Expect.equal (Ok [])
                    , \_ ->
                        Decode.decodeString Api.observationMatchDecoder legacy
                            |> Result.map .matchedPaths
                            |> Expect.equal (Ok [ "src/Legacy.elm" ])
                    ]
                    ()
        , test "facet merge preserves server order, kind-qualified identity, and raw offsets" <|
            \_ ->
                let
                    fileMain =
                        facetFixture Api.SubjectFile "src/Main.elm" 5 "2026-08-30T10:00:00Z"

                    globMain =
                        facetFixture Api.SubjectGlob "src/Main.elm" 4 "2026-08-29T10:00:00Z"

                    updatedFile =
                        facetFixture Api.SubjectFile "src/Main.elm" 7 "2026-08-30T12:00:00Z"

                    first =
                        Feature.Observation.mergeFacetPage 0
                            { items = [ fileMain, globMain, updatedFile ], hasMore = True }
                            Feature.Observation.init

                    third =
                        facetFixture Api.SubjectGlob "test/**/*.elm" 2 "2026-08-28T10:00:00Z"

                    appended =
                        Feature.Observation.mergeFacetPage first.facetNextOffset
                            { items = [ globMain, third ], hasMore = False }
                            first

                    fileKey =
                        Feature.Observation.facetKey Api.SubjectFile "src/Main.elm"

                    globKey =
                        Feature.Observation.facetKey Api.SubjectGlob "src/Main.elm"
                in
                Expect.all
                    [ \_ -> first.facetKeys |> Expect.equal [ fileKey, globKey ]
                    , \_ -> Dict.get fileKey first.facets |> Maybe.map .observationCount |> Expect.equal (Just 7)
                    , \_ -> ( first.facetNextOffset, appended.facetNextOffset ) |> Expect.equal ( 3, 5 )
                    , \_ -> appended.facetKeys |> Expect.equal [ fileKey, globKey, Feature.Observation.facetKey Api.SubjectGlob "test/**/*.elm" ]
                    , \_ -> (fileKey == globKey) |> Expect.equal False
                    ]
                    ()
        , test "facet and result page guards independently bind mode, session, generation, fingerprint, and raw offset" <|
            \_ ->
                let
                    initial =
                        Feature.Observation.init

                    facetState =
                        { initial
                            | requestMode = ObservationFacetMode
                            , facetRequestSessionEpoch = 7
                            , facetRequestGeneration = 3
                            , facetFingerprint = "facet-fingerprint"
                            , facetExpectedOffset = Just 50
                        }

                    matchState =
                        { initial
                            | requestMode = ObservationMatchMode
                            , requestSessionEpoch = 7
                            , requestGeneration = 4
                            , queryFingerprint = "match-fingerprint"
                            , expectedOffset = Just 100
                        }
                in
                [ Feature.Observation.facetResponseMatches 7 3 "facet-fingerprint" 50 facetState
                , Feature.Observation.facetResponseMatches 6 3 "facet-fingerprint" 50 facetState
                , Feature.Observation.facetResponseMatches 7 2 "facet-fingerprint" 50 facetState
                , Feature.Observation.facetResponseMatches 7 3 "old-filter" 50 facetState
                , Feature.Observation.facetResponseMatches 7 3 "facet-fingerprint" 0 facetState
                , Feature.Observation.matchResponseMatches 7 4 "match-fingerprint" 100 matchState
                , Feature.Observation.matchResponseMatches 6 4 "match-fingerprint" 100 matchState
                , Feature.Observation.matchResponseMatches 7 4 "match-fingerprint" 50 matchState
                , Feature.Observation.matchResponseMatches 7 4 "match-fingerprint" 100 { matchState | requestMode = ObservationFlatMode }
                ]
                    |> Expect.equal [ True, False, False, False, False, True, False, False, False ]
        , test "shared, exact, and match modes apply the correct filters and Clear match restores the prior exact facet" <|
            \_ ->
                let
                    initial =
                        Feature.Observation.init

                    exactState =
                        { initial
                            | requestMode = ObservationExactSubjectMode
                            , subjectKind = Just Api.SubjectFile
                            , subject = "manual/flat.elm"
                            , selectedFacet = Just { subjectKind = Api.SubjectGlob, subject = "src/**/*.elm" }
                            , query = "render"
                            , gitSha = fullSha
                            , matchPathsInput = "src/Main.elm"
                        }

                    exactModel =
                        editableModel exactState

                    matching =
                        Feature.Observation.update ApplyObservationMatch exactModel |> Tuple.first

                    restored =
                        Feature.Observation.update ClearObservationMatch matching |> Tuple.first

                    shared =
                        Feature.Observation.update (SetObservationBrowseMode ObservationFacetMode) exactModel |> Tuple.first

                    selectedFacet =
                        Feature.Observation.update (SelectObservationFacet Api.SubjectFile "src/Main.elm") shared |> Tuple.first

                    flatQuery =
                        Feature.Observation.listQuery "workspace-1" 0 { exactState | requestMode = ObservationFlatMode }

                    exactQuery =
                        Feature.Observation.listQuery "workspace-1" 0 exactState

                    matchQuery =
                        Feature.Observation.matchQuery "workspace-1" [ "src/Main.elm" ] 0 exactState

                    subjectFacetQuery =
                        Feature.Observation.facetQuery "workspace-1" 0 exactState
                in
                Expect.all
                    [ \_ -> flatQuery.subject |> Expect.equal (Just "manual/flat.elm")
                    , \_ -> exactQuery.subject |> Expect.equal (Just "src/**/*.elm")
                    , \_ -> exactQuery.subjectKind |> Expect.equal (Just Api.SubjectGlob)
                    , \_ -> ( matchQuery.query, matchQuery.gitSha, matchQuery.subjectKind ) |> Expect.equal ( Just "render", Just fullSha, Just Api.SubjectFile )
                    , \_ -> ( subjectFacetQuery.query, subjectFacetQuery.gitSha, subjectFacetQuery.subjectKind ) |> Expect.equal ( Just "render", Just fullSha, Just Api.SubjectFile )
                    , \_ -> ( matching.observations.requestMode, matching.observations.browseReturn |> Maybe.map .requestMode ) |> Expect.equal ( ObservationMatchMode, Just ObservationExactSubjectMode )
                    , \_ -> matching.observations.matchAppliedPaths |> Expect.equal [ "src/Main.elm" ]
                    , \_ ->
                        ( ( restored.observations.requestMode, restored.observations.subjectKind, restored.observations.selectedFacet )
                        , ( restored.observations.subject, restored.observations.matchPathsInput, restored.observations.matchAppliedPaths )
                        )
                            |> Expect.equal
                                ( ( ObservationExactSubjectMode, Just Api.SubjectFile, Just { subjectKind = Api.SubjectGlob, subject = "src/**/*.elm" } )
                                , ( "manual/flat.elm", "", [] )
                                )
                    , \_ -> ( shared.observations.requestMode, shared.observations.subject ) |> Expect.equal ( ObservationFacetMode, "" )
                    , \_ -> ( selectedFacet.observations.requestMode, selectedFacet.observations.subjectKind, selectedFacet.observations.selectedFacet ) |> Expect.equal ( ObservationExactSubjectMode, Just Api.SubjectFile, Just { subjectKind = Api.SubjectFile, subject = "src/Main.elm" } )
                    ]
                    ()
        , test "exact facet identity survives All and opposite browse-kind changes" <|
            \_ ->
                let
                    initial =
                        Feature.Observation.init

                    facetModel =
                        editableModel { initial | requestMode = ObservationFacetMode }

                    selected =
                        Feature.Observation.update (SelectObservationFacet Api.SubjectGlob "src/**/*.elm") facetModel
                            |> Tuple.first

                    allKinds =
                        Feature.Observation.update (SetObservationSubjectKind "") selected
                            |> Tuple.first

                    oppositeKind =
                        Feature.Observation.update (SetObservationSubjectKind "file") allKinds
                            |> Tuple.first

                    afterApply =
                        Feature.Observation.update ApplyObservationFilters oppositeKind
                            |> Tuple.first

                    exactTuple model =
                        let
                            query =
                                Feature.Observation.listQuery "workspace-1" 0 model.observations
                        in
                        ( query.subjectKind, query.subject )

                    view =
                        Feature.Observation.viewObservationsState (observationWorkspace Api.Repository) oppositeKind.observations
                            |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> exactTuple selected |> Expect.equal ( Just Api.SubjectGlob, Just "src/**/*.elm" )
                    , \_ -> exactTuple allKinds |> Expect.equal ( Just Api.SubjectGlob, Just "src/**/*.elm" )
                    , \_ -> exactTuple oppositeKind |> Expect.equal ( Just Api.SubjectGlob, Just "src/**/*.elm" )
                    , \_ -> exactTuple afterApply |> Expect.equal ( Just Api.SubjectGlob, Just "src/**/*.elm" )
                    , \_ -> oppositeKind.observations.subjectKind |> Expect.equal (Just Api.SubjectFile)
                    , \_ -> view |> Query.has [ Selector.text "Glob: ", Selector.text "src/**/*.elm", Selector.text "Exact results stay locked to this subject tuple." ]
                    ]
                    ()
        , test "match draft changes do not reinterpret applied evidence, paging, filters, or refresh" <|
            \_ ->
                let
                    initial =
                        Feature.Observation.init

                    observation =
                        observationWithSubjects "applied-a"
                            [ { subjectKind = Api.SubjectGlob, subject = "src/**/*.elm" } ]

                    starting =
                        editableModel
                            { initial
                                | requestMode = ObservationFlatMode
                                , matchPathsInput = "src/A.elm"
                            }

                    matching =
                        Feature.Observation.update ApplyObservationMatch starting |> Tuple.first

                    matchingState =
                        matching.observations

                    loadedState =
                        { matchingState
                            | items = Dict.singleton observation.id observation
                            , orderedIds = [ observation.id ]
                            , hasMore = True
                            , loading = False
                            , expectedOffset = Nothing
                            , nextOffset = 1
                            , matchEvidence =
                                Dict.singleton observation.id
                                    (matchFixture observation
                                        [ { path = "src/A.elm", matchedSubjects = observation.subjects } ]
                                    )
                        }

                    loaded =
                        { matching | observations = loadedState }

                    drafted =
                        Feature.Observation.update (SetObservationMatchPaths "src/B.elm") loaded |> Tuple.first

                    refreshed =
                        Feature.Observation.refreshActiveResults drafted |> Tuple.first

                    filtersApplied =
                        Feature.Observation.update ApplyObservationFilters drafted |> Tuple.first

                    results =
                        Feature.Observation.viewObservationsState (observationWorkspace Api.Repository) drafted.observations
                            |> Query.fromHtml
                            |> Query.find [ Selector.class "observation-match-results" ]
                in
                Expect.all
                    [ \_ -> ( drafted.observations.matchAppliedPaths, drafted.observations.matchPathsInput ) |> Expect.equal ( [ "src/A.elm" ], "src/B.elm" )
                    , \_ -> drafted.observations.queryFingerprint |> Expect.equal loaded.observations.queryFingerprint
                    , \_ -> Feature.Observation.canLoadMore "workspace-1" drafted.observations |> Expect.equal True
                    , \_ -> results |> Query.has [ Selector.text "src/A.elm" ]
                    , \_ -> results |> Query.hasNot [ Selector.text "src/B.elm" ]
                    , \_ -> ( refreshed.observations.matchAppliedPaths, refreshed.observations.matchPathsInput, refreshed.observations.queryFingerprint ) |> Expect.equal ( [ "src/A.elm" ], "src/B.elm", loaded.observations.queryFingerprint )
                    , \_ -> ( filtersApplied.observations.matchAppliedPaths, filtersApplied.observations.matchPathsInput ) |> Expect.equal ( [ "src/A.elm" ], "src/B.elm" )
                    ]
                    ()
        , test "Clear match only clears unapplied drafts and restores precise browse state after an applied match" <|
            \_ ->
                let
                    initial =
                        Feature.Observation.init

                    facet =
                        facetFixture Api.SubjectGlob "src/**/*.elm" 3 "2026-01-01T00:00:00Z"

                    facetKey =
                        Feature.Observation.facetKey facet.subjectKind facet.subject

                    facetState =
                        { initial
                            | requestMode = ObservationFacetMode
                            , subjectKind = Just Api.SubjectGlob
                            , matchPathsInput = "src/Draft.elm"
                            , facets = Dict.singleton facetKey facet
                            , facetKeys = [ facetKey ]
                        }

                    clearedFacetDraft =
                        Feature.Observation.update ClearObservationMatch (editableModel facetState) |> Tuple.first

                    exactState =
                        { facetState
                            | requestMode = ObservationExactSubjectMode
                            , selectedFacet = Just { subjectKind = Api.SubjectFile, subject = "src/Main.elm" }
                        }

                    clearedExactDraft =
                        Feature.Observation.update ClearObservationMatch (editableModel exactState) |> Tuple.first

                    appliedFromFacet =
                        Feature.Observation.update ApplyObservationMatch (editableModel facetState) |> Tuple.first

                    restoredFacet =
                        Feature.Observation.update ClearObservationMatch appliedFromFacet |> Tuple.first
                in
                Expect.all
                    [ \_ -> ( clearedFacetDraft.observations.requestMode, clearedFacetDraft.observations.matchPathsInput, clearedFacetDraft.observations.facetKeys ) |> Expect.equal ( ObservationFacetMode, "", [ facetKey ] )
                    , \_ -> ( clearedExactDraft.observations.requestMode, clearedExactDraft.observations.matchPathsInput, clearedExactDraft.observations.selectedFacet ) |> Expect.equal ( ObservationExactSubjectMode, "", Just { subjectKind = Api.SubjectFile, subject = "src/Main.elm" } )
                    , \_ -> Feature.Observation.listQuery "workspace-1" 0 clearedExactDraft.observations |> (\query -> ( query.subjectKind, query.subject )) |> Expect.equal ( Just Api.SubjectFile, Just "src/Main.elm" )
                    , \_ -> ( appliedFromFacet.observations.requestMode, appliedFromFacet.observations.matchAppliedPaths, appliedFromFacet.observations.browseReturn |> Maybe.map .requestMode ) |> Expect.equal ( ObservationMatchMode, [ "src/Draft.elm" ], Just ObservationFacetMode )
                    , \_ ->
                        { mode = restoredFacet.observations.requestMode
                        , kind = restoredFacet.observations.subjectKind
                        , paths = restoredFacet.observations.matchAppliedPaths
                        , browseReturn = restoredFacet.observations.browseReturn
                        }
                            |> Expect.equal
                                { mode = ObservationFacetMode
                                , kind = Just Api.SubjectGlob
                                , paths = []
                                , browseReturn = Nothing
                                }
                    , \_ -> ( restoredFacet.observations.facetLoading, restoredFacet.observations.facetExpectedOffset ) |> Expect.equal ( True, Just 0 )
                    ]
                    ()
        , test "path grouping follows caller and canonical subject order, keeps empty paths, and dedupes only inside a group" <|
            \_ ->
                let
                    firstObservation =
                        observationWithSubjects "first"
                            [ { subjectKind = Api.SubjectGlob, subject = "src/**/*.elm" }
                            , { subjectKind = Api.SubjectFile, subject = "src/Main.elm" }
                            ]

                    secondObservation =
                        observationWithSubjects "second"
                            [ { subjectKind = Api.SubjectFile, subject = "src/Main.elm" }
                            , { subjectKind = Api.SubjectGlob, subject = "src/**/*.elm" }
                            ]

                    evidence =
                        Dict.fromList
                            [ ( firstObservation.id
                              , matchFixture firstObservation
                                    [ { path = "src/Other.elm", matchedSubjects = [ { subjectKind = Api.SubjectFile, subject = "src/Main.elm" } ] }
                                    , { path = "src/Main.elm"
                                      , matchedSubjects =
                                            [ { subjectKind = Api.SubjectGlob, subject = "src/**/*.elm" }
                                            , { subjectKind = Api.SubjectFile, subject = "src/Main.elm" }
                                            , { subjectKind = Api.SubjectGlob, subject = "src/**/*.elm" }
                                            ]
                                      }
                                    ]
                              )
                            , ( secondObservation.id
                              , matchFixture secondObservation
                                    [ { path = "src/Main.elm"
                                      , matchedSubjects =
                                            [ { subjectKind = Api.SubjectFile, subject = "src/Main.elm" }
                                            , { subjectKind = Api.SubjectGlob, subject = "src/**/*.elm" }
                                            ]
                                      }
                                    ]
                              )
                            ]

                    grouped =
                        Feature.Observation.groupPathMatches
                            [ "src/Other.elm", "src/Main.elm", "src/Empty.elm" ]
                            [ firstObservation.id, secondObservation.id ]
                            evidence

                    summary =
                        List.map
                            (\pathGroup ->
                                ( pathGroup.path
                                , List.map (\subjectGroup -> ( Api.subjectKindToString subjectGroup.subjectKind, subjectGroup.subject, subjectGroup.observationIds )) pathGroup.subjectGroups
                                )
                            )
                            grouped
                in
                summary
                    |> Expect.equal
                        [ ( "src/Other.elm", [ ( "file", "src/Main.elm", [ "first" ] ) ] )
                        , ( "src/Main.elm"
                          , [ ( "glob", "src/**/*.elm", [ "first", "second" ] )
                            , ( "file", "src/Main.elm", [ "first", "second" ] )
                            ]
                          )
                        , ( "src/Empty.elm", [] )
                        ]
        , test "match paging advances by raw rows and ignores stale page responses after active deletion refresh" <|
            \_ ->
                let
                    initial =
                        Feature.Observation.init

                    observation =
                        observationWithSubjects "deleted"
                            [ { subjectKind = Api.SubjectGlob, subject = "src/**/*.elm" } ]

                    matchItem =
                        matchFixture observation
                            [ { path = "src/Main.elm", matchedSubjects = observation.subjects } ]

                    state =
                        { initial
                            | requestMode = ObservationMatchMode
                            , matchPathsInput = "src/Main.elm"
                            , matchAppliedPaths = [ "src/Main.elm" ]
                            , requestSessionEpoch = 7
                            , requestGeneration = 4
                            , queryFingerprint = "match-fingerprint"
                            , expectedOffset = Just 0
                        }

                    shell =
                        sameWorkspaceRepositoryModel 7 state

                    firstPage =
                        Feature.Observation.update
                            (GotObservationMatches "workspace-1" 7 4 "match-fingerprint" 0 (Ok { items = [ matchItem, { matchItem | observation = { observation | id = "other" } } ], hasMore = True }))
                            shell
                            |> Tuple.first

                    firstObservations =
                        firstPage.observations

                    loadingMoreState =
                        { firstObservations | expectedOffset = Just 2 }

                    loadingMore =
                        { firstPage | observations = loadingMoreState }

                    ( cleaned, _ ) =
                        Feature.Observation.reconcileDeletedObservation observation.id loadingMore

                    ( refreshed, _ ) =
                        Feature.Observation.refreshActiveResults cleaned

                    stale =
                        Feature.Observation.update
                            (GotObservationMatches "workspace-1" 7 4 "match-fingerprint" 2 (Ok { items = [ matchItem ], hasMore = False }))
                            refreshed
                            |> Tuple.first
                in
                [ firstPage.observations.nextOffset == 2
                , Dict.member observation.id firstPage.observations.items
                , stale.observations.requestGeneration > 4
                , stale.observations.expectedOffset == Just 0
                , Dict.member observation.id stale.observations.items
                , Dict.member observation.id stale.observations.matchEvidence
                ]
                    |> Expect.equal [ True, True, True, True, False, False ]
        , test "delete cleanup and page-zero refresh are mode-aware across flat, facets, exact subject, and match" <|
            \_ ->
                let
                    observation =
                        fixtureObservation "cleanup-all" "2026-01-01T00:00:00Z"

                    base mode =
                        let
                            selectedState =
                                selectedObservationState observation

                            state =
                                { selectedState
                                    | requestMode = mode
                                    , requestSessionEpoch = 8
                                    , requestGeneration = 2
                                    , facetRequestSessionEpoch = 8
                                    , facetRequestGeneration = 5
                                    , subjectKind = Just Api.SubjectFile
                                    , subject = observation.subject
                                    , selectedFacet = Just { subjectKind = Api.SubjectFile, subject = observation.subject }
                                    , matchPathsInput = observation.subject
                                    , matchAppliedPaths = [ observation.subject ]
                                }

                            shell =
                                sameWorkspaceRepositoryModel 8 state

                            ( cleaned, _ ) =
                                Feature.Observation.reconcileDeletedObservation observation.id shell
                        in
                        Feature.Observation.refreshActiveResults cleaned |> Tuple.first

                    flat =
                        base ObservationFlatMode

                    facets =
                        base ObservationFacetMode

                    exact =
                        base ObservationExactSubjectMode

                    matched =
                        base ObservationMatchMode
                in
                [ flat.observations.expectedOffset == Just 0
                , facets.observations.facetExpectedOffset == Just 0
                , facets.observations.facetRequestGeneration > 5
                , exact.observations.expectedOffset == Just 0
                , exact.observations.selectedFacet == Just { subjectKind = Api.SubjectFile, subject = observation.subject }
                , matched.observations.expectedOffset == Just 0
                , Dict.isEmpty matched.observations.items
                , Dict.isEmpty matched.observations.matchEvidence
                , matched.observations.selectedId == Nothing
                ]
                    |> Expect.equal [ True, True, True, True, True, True, True, True, True ]
        , test "save delete snapshot and preserving refresh reuse every applied query despite edited filter inputs" <|
            \_ ->
                let
                    check mode =
                        let
                            applied =
                                appliedModeModel mode

                            drafted =
                                draftQueryInputs applied

                            refreshing =
                                Feature.Observation.update RefreshObservationResults drafted |> Tuple.first

                            snapshot =
                                Feature.WebSocket.update (WsMessageReceived (observationSnapshotWire (fixtureObservation "curated" "2026-01-01T00:00:00Z"))) drafted |> Tuple.first

                            saving =
                                drafted
                                    |> Feature.Observation.update StartObservationEdit |> Tuple.first
                                    |> Feature.Observation.update (SetObservationDraft "saved applied content") |> Tuple.first
                                    |> Feature.Observation.update SaveObservationEdit |> Tuple.first

                            saved =
                                saving.observations.edit |> Maybe.andThen .activeRequest
                                    |> Maybe.map (\request -> Feature.Observation.update (ObservationUpdated request (Ok { observation | content = "saved applied content", updatedAt = "2026-01-02T00:00:00Z" })) saving |> Tuple.first)

                            observation =
                                fixtureObservation "curated" "2026-01-01T00:00:00Z"

                            deleting =
                                drafted
                                    |> Feature.Observation.update OpenObservationDelete |> Tuple.first
                                    |> Feature.Observation.update ConfirmObservationDelete |> Tuple.first

                            deleted =
                                deleting.observations.deleteConfirmation |> Maybe.andThen .activeRequest
                                    |> Maybe.map (\request -> Feature.Observation.update (ObservationDeleted request (Ok ())) deleting |> Tuple.first)

                            preserved candidate =
                                candidate.observations.appliedQuery == applied.observations.appliedQuery
                                    && candidate.observations.query == "draft search"
                                    && Feature.Observation.listQuery "workspace-1" 50 candidate.observations == Feature.Observation.listQuery "workspace-1" 50 applied.observations
                                    && Feature.Observation.facetQuery "workspace-1" 50 candidate.observations == Feature.Observation.facetQuery "workspace-1" 50 applied.observations
                                    && Feature.Observation.matchQuery "workspace-1" [ "wrong/draft.elm" ] 50 candidate.observations == Feature.Observation.matchQuery "workspace-1" [ "wrong/draft.elm" ] 50 applied.observations
                                    && (if mode == ObservationFacetMode then
                                            candidate.observations.facetRequestGeneration > applied.observations.facetRequestGeneration
                                                && candidate.observations.facetFingerprint == applied.observations.facetFingerprint
                                                && candidate.observations.facetExpectedOffset == Just 0
                                        else
                                            candidate.observations.requestGeneration > applied.observations.requestGeneration
                                                && candidate.observations.queryFingerprint == applied.observations.queryFingerprint
                                                && candidate.observations.expectedOffset == Just 0
                                       )
                        in
                        List.all preserved [ refreshing, snapshot ]
                            && Maybe.map preserved saved == Just True
                            && Maybe.map preserved deleted == Just True
                in
                observationModes |> List.map check |> Expect.equal [ True, True, True, True ]
        , describe "dirty filters and paging controls"
            (List.map
                (\mode -> test (Debug.toString mode) <| \_ ->
                let
                    loaded =
                        appliedModeModel mode

                    drafted =
                        draftQueryInputs loaded

                    reverted =
                        Feature.Observation.update RevertObservationFilters drafted |> Tuple.first

                    more model =
                        if mode == ObservationFacetMode then
                            Feature.Observation.canLoadMoreFacets "workspace-1" model.observations
                        else
                            Feature.Observation.canLoadMore "workspace-1" model.observations

                    loadMessage =
                        if mode == ObservationFacetMode then LoadMoreObservationFacets else LoadMoreObservations

                    refused =
                        Feature.Observation.update loadMessage drafted |> Tuple.first

                    view model =
                        Feature.Observation.viewObservationsState (observationWorkspace Api.Repository) model.observations |> Query.fromHtml

                    pagingClass =
                        if mode == ObservationFacetMode then "observation-facet-load-more" else "observation-load-more"
                in
                Expect.all
                    [ \_ -> [ more loaded, more drafted, more reverted, refused.observations == drafted.observations, reverted.observations.matchPathsInput == "src/Draft.elm" ] |> Expect.equal [ True, False, True, True, True ]
                    , \_ -> view drafted |> Query.find [ Selector.class pagingClass ] |> Query.has [ Selector.disabled True ]
                    , \_ -> view drafted |> Query.has [ Selector.text "Filters have unapplied changes. Apply filters or revert them before loading more results." ]
                    , \_ -> view drafted |> Query.find [ Selector.tag "button", Selector.containing [ Selector.text "Revert filters" ] ] |> Event.simulate Event.click |> Event.expect RevertObservationFilters
                    , \_ -> view reverted |> Query.find [ Selector.class pagingClass ] |> Query.hasNot [ Selector.disabled True ]
                    ] ()
                ) observationModes)
        , test "explicit Apply commits draft filters and rejects pre-Apply callbacks in every mode" <|
            \_ ->
                let
                    check mode =
                        let
                            before =
                                appliedModeModel mode

                            applied =
                                draftQueryInputs before |> Feature.Observation.update ApplyObservationFilters |> Tuple.first

                            state =
                                before.observations

                            lateObservation =
                                fixtureObservation "late-query" "2026-01-01T00:00:00Z"

                            lateMessage =
                                case mode of
                                    ObservationFacetMode ->
                                        GotObservationSubjectFacets "workspace-1" 3 state.facetRequestGeneration state.facetFingerprint 0 (Ok { items = [ facetFixture Api.SubjectFile "late-query" 1 "2026-01-01T00:00:00Z" ], hasMore = False })
                                    ObservationMatchMode ->
                                        GotObservationMatches "workspace-1" 3 state.requestGeneration state.queryFingerprint 0 (Ok { items = [ matchFixture lateObservation [ { path = "src/Main.elm", matchedSubjects = lateObservation.subjects } ] ], hasMore = False })
                                    _ ->
                                        GotObservations "workspace-1" Nothing state.requestGeneration state.queryFingerprint 0 (Ok { items = [ lateObservation ], hasMore = False })

                            afterLate =
                                if mode == ObservationFacetMode || mode == ObservationMatchMode then
                                    Feature.Observation.update lateMessage applied |> Tuple.first
                                else
                                    Feature.DataLoading.update lateMessage applied |> Tuple.first

                            query =
                                Feature.Observation.listQuery "workspace-1" 0 applied.observations
                        in
                        query.query == Just "draft search"
                            && query.gitSha == Just (String.repeat 40 "a")
                            && query.subjectKind == Just Api.SubjectGlob
                            && query.subject == (if mode == ObservationFlatMode then Just "draft/**/*.elm" else if mode == ObservationExactSubjectMode then Just "src/**/*.elm" else Nothing)
                            && applied.observations.matchAppliedPaths == before.observations.matchAppliedPaths
                            && applied.observations.matchPathsInput == "src/Draft.elm"
                            && not (Feature.Observation.hasUnappliedFilters applied.observations)
                            && afterLate.observations == applied.observations
                in
                observationModes |> List.map check |> Expect.equal [ True, True, True, True ]
        , test "match browse return restores the prior applied search SHA and exact tuple" <|
            \_ ->
                let
                    original =
                        appliedModeModel ObservationExactSubjectMode

                    matching =
                        draftQueryInputs original |> Feature.Observation.update ApplyObservationMatch |> Tuple.first

                    restored =
                        Feature.Observation.update ClearObservationMatch matching |> Tuple.first
                in
                Feature.Observation.listQuery "workspace-1" 0 restored.observations
                    |> Expect.equal (Feature.Observation.listQuery "workspace-1" 0 original.observations)
        , describe "failed pages retain results and retry the recorded applied request"
            (List.concatMap
                (\mode -> List.map
                    (\offset -> test (Debug.toString mode ++ " offset " ++ String.fromInt offset) <| \_ ->
                        let
                            loaded =
                                appliedModeModel mode

                            loadedState =
                                loaded.observations

                            withCursor =
                                { loaded | observations = { loadedState | nextOffset = 50, facetNextOffset = 50 } }

                            requested =
                                Feature.Observation.update
                                    (if offset == 0 then RefreshObservationResults else if mode == ObservationFacetMode then LoadMoreObservationFacets else LoadMoreObservations)
                                    withCursor |> Tuple.first

                            oldError =
                                observationPageMessage offset (Err Http.NetworkError) requested

                            failed =
                                applyObservationPage oldError requested |> draftQueryInputs

                            failedState =
                                failed.observations

                            movedCursor =
                                { failed | observations = { failedState | nextOffset = 999, facetNextOffset = 999 } }

                            retried =
                                Feature.Observation.update RetryObservationResults movedCursor |> Tuple.first

                            newGeneration =
                                if mode == ObservationFacetMode then retried.observations.facetRequestGeneration else retried.observations.requestGeneration

                            oldGeneration =
                                if mode == ObservationFacetMode then requested.observations.facetRequestGeneration else requested.observations.requestGeneration

                            recovered =
                                applyObservationPage (observationPageMessage offset (Ok (fixtureObservation "replacement" "2026-01-02T00:00:00Z")) retried) retried

                            view =
                                Feature.Observation.viewObservations (observationWorkspace Api.Repository) failed |> Query.fromHtml

                            cachedSelector =
                                case mode of
                                    ObservationFacetMode -> "observation-facet-card"
                                    ObservationMatchMode -> "observation-subject-group-toggle"
                                    _ -> "observation-card"
                        in
                        Expect.all
                            [ \_ -> view |> Query.findAll [ Selector.class cachedSelector ] |> Query.count (Expect.equal 1)
                            , \_ -> view |> Query.has [ Selector.text (if offset == 0 then "Automatic refresh failed." else "Previously loaded results remain available; the last request failed.") ]
                            , \_ -> view |> Query.find [ Selector.class "observation-retry" ] |> Event.simulate Event.click |> Event.expect RetryObservationResults
                            , \_ -> failed.observations.failedRequest |> Maybe.map .offset |> Expect.equal (if offset == 0 then Nothing else Just offset)
                            , \_ -> failed.observations.resultsStale |> Expect.equal False
                            , \_ -> newGeneration > oldGeneration |> Expect.equal True
                            , \_ -> (if mode == ObservationFacetMode then retried.observations.facetExpectedOffset else retried.observations.expectedOffset) |> Expect.equal (Just offset)
                            , \_ -> Feature.Observation.listQuery "workspace-1" offset retried.observations |> Expect.equal (Feature.Observation.listQuery "workspace-1" offset loaded.observations)
                            , \_ -> Feature.Observation.facetQuery "workspace-1" offset retried.observations |> Expect.equal (Feature.Observation.facetQuery "workspace-1" offset loaded.observations)
                            , \_ -> Feature.Observation.matchQuery "workspace-1" [] offset retried.observations |> Expect.equal (Feature.Observation.matchQuery "workspace-1" [] offset loaded.observations)
                            , \_ -> retried.observations.query |> Expect.equal "draft search"
                            , \_ -> (applyObservationPage oldError retried).observations |> Expect.equal retried.observations
                            , \_ -> (applyObservationPage (observationPageMessage offset (Ok (fixtureObservation "stale" "2026-01-02T00:00:00Z")) requested) retried).observations |> Expect.equal retried.observations
                            , \_ -> recovered.observations.failedRequest |> Expect.equal Nothing
                            , \_ -> (if mode == ObservationFacetMode then recovered.observations.facetKeys else recovered.observations.orderedIds) |> List.length |> Expect.equal (if offset == 0 then 1 else 2)
                            , \_ -> if offset == 0 && mode /= ObservationFacetMode then Dict.member "curated" recovered.observations.items |> Expect.equal False else Expect.pass
                            , \_ -> recovered.observations.resultsStale |> Expect.equal False
                            ] ()
                    ) [ 0, 50 ]
                ) observationModes)
        , test "initial failures remain distinct from empty results and new queries retire retry ownership" <|
            \_ ->
                let
                    check mode =
                        let
                            model =
                                appliedModeModel mode

                            state =
                                model.observations

                            empty =
                                { model | observations = { state | items = Dict.empty, orderedIds = [], matchEvidence = Dict.empty, facets = Dict.empty, facetKeys = [], selectedId = Nothing, selectedDetail = Nothing } }

                            requested =
                                Feature.Observation.update ApplyObservationFilters empty |> Tuple.first

                            failed =
                                applyObservationPage (observationPageMessage 0 (Err Http.Timeout) requested) requested

                            applied =
                                draftQueryInputs failed |> Feature.Observation.update ApplyObservationFilters |> Tuple.first

                            view =
                                Feature.Observation.viewObservationsState (observationWorkspace Api.Repository) failed.observations |> Query.fromHtml
                        in
                        Expect.all
                            [ \_ -> view |> Query.has [ Selector.class "observation-state-error", Selector.class "empty-state", Selector.text "Retry results" ]
                            , \_ -> view |> Query.hasNot [ Selector.class "observation-state-empty" ]
                            , \_ -> failed.observations.resultsStale |> Expect.equal False
                            , \_ -> applied.observations.failedRequest |> Expect.equal Nothing
                            , \_ -> (Feature.Observation.update RetryObservationResults applied |> Tuple.first).observations |> Expect.equal applied.observations
                            ] ()
                in
                Expect.all (List.map (\mode _ -> check mode) observationModes) ()
        , test "off-page detail retry has a fresh token and stale detail errors and successes stay inert" <|
            \_ ->
                let
                    selected =
                        Feature.Observation.selectObservation "off-page" (editableModel Feature.Observation.init) |> Tuple.first
                in
                case selected.observations.activeDetailRequest of
                    Nothing -> Expect.fail "Expected initial linked detail request"
                    Just request ->
                        let
                            failure =
                                GotObservationDetail "workspace-1" "off-page" 3 request.token (Err Http.NetworkError)

                            failed =
                                Feature.Observation.update failure selected |> Tuple.first

                            retried =
                                Feature.Observation.update RetryObservationDetail failed |> Tuple.first

                            canonical =
                                fixtureObservation "off-page" "2026-01-01T00:00:00Z"

                            received =
                                retried.observations.activeDetailRequest
                                    |> Maybe.map (\fresh -> Feature.Observation.update (GotObservationDetail "workspace-1" "off-page" 3 fresh.token (Ok canonical)) retried |> Tuple.first)
                        in
                        Expect.all
                            [ \_ -> Feature.Observation.viewObservationsState (observationWorkspace Api.Repository) failed.observations |> Query.fromHtml |> Query.find [ Selector.tag "button", Selector.containing [ Selector.text "Retry detail" ] ] |> Event.simulate Event.click |> Event.expect RetryObservationDetail
                            , \_ -> retried.observations.activeDetailRequest |> Maybe.map (\fresh -> fresh.token > request.token) |> Expect.equal (Just True)
                            , \_ -> (Feature.Observation.update failure retried |> Tuple.first).observations |> Expect.equal retried.observations
                            , \_ -> (Feature.Observation.update (GotObservationDetail "workspace-1" "off-page" 3 request.token (Ok canonical)) retried |> Tuple.first).observations |> Expect.equal retried.observations
                            , \_ -> received |> Maybe.andThen (.observations >> .selectedDetail) |> Expect.equal (Just canonical)
                            , \_ -> received |> Maybe.map (.observations >> .orderedIds) |> Expect.equal (Just [])
                            ] ()
        , test "retrying retained detail revalidation keeps draft and curation controls usable" <|
            \_ ->
                let
                    saving =
                        savingEditModel "retained retry draft"

                    state =
                        saving.observations

                    dirty =
                        { saving | observations = { state | edit = Maybe.map (\edit -> { edit | saving = False, activeRequest = Nothing }) state.edit } }

                    returned =
                        Feature.Observation.selectObservation "other" dirty |> Tuple.first
                            |> Feature.Observation.update ReturnToObservationDraft |> Tuple.first
                in
                case returned.observations.activeDetailRequest of
                    Nothing -> Expect.fail "Expected retained detail revalidation request"
                    Just request ->
                        let
                            failed =
                                Feature.Observation.update (GotObservationDetail "workspace-1" "curated" 3 request.token (Err Http.Timeout)) returned |> Tuple.first

                            retried =
                                Feature.Observation.update RetryObservationDetail failed |> Tuple.first

                            view =
                                Feature.Observation.viewObservationsState (observationWorkspace Api.Repository) failed.observations |> Query.fromHtml
                        in
                        Expect.all
                            [ \_ -> Feature.Observation.viewObservations (observationWorkspace Api.Repository) failed |> Query.fromHtml |> Query.has [ Selector.class "observation-edit-form", Selector.text "Save content", Selector.text "Cancel", Selector.text "Retry detail" ]
                            , \_ -> retried.observations.edit |> Expect.equal dirty.observations.edit
                            , \_ -> retried.observations.activeDetailRequest |> Maybe.map (\fresh -> fresh.token > request.token) |> Expect.equal (Just True)
                            , \_ -> retried.observations.detailError |> Expect.equal Nothing
                            ] ()
        , test "first preserving request freezes an applied query before a failed retry with changed inputs" <|
            \_ ->
                let
                    model =
                        editableModel Feature.Observation.init

                    requested =
                        Feature.Observation.update RefreshObservationResults model |> Tuple.first

                    failed =
                        applyObservationPage (observationPageMessage 0 (Err Http.NetworkError) requested) requested |> draftQueryInputs

                    retried =
                        Feature.Observation.update RetryObservationResults failed |> Tuple.first
                in
                Expect.all
                    [ \_ -> requested.observations.appliedQuery /= Nothing |> Expect.equal True
                    , \_ -> retried.observations.expectedOffset |> Expect.equal (Just 0)
                    , \_ -> Feature.Observation.listQuery "workspace-1" 0 retried.observations |> Expect.equal (Feature.Observation.listQuery "workspace-1" 0 requested.observations)
                    , \_ -> retried.observations.query |> Expect.equal "draft search"
                    ] ()
        , test "same-ID user activation refocuses navigation while preserving saving edit and detail request identity" <|
            \_ ->
                let
                    saving =
                        savingEditModel "navigation owner"

                    activated =
                        Feature.Observation.update (SelectObservationFrom "curated" "duplicate-card") saving |> Tuple.first

                    returned =
                        Feature.Observation.update ReturnObservationResults activated |> Tuple.first
                in
                Expect.all
                    [ \_ -> activated.observations.edit |> Expect.equal saving.observations.edit
                    , \_ -> activated.observations.activeDetailRequest |> Expect.equal saving.observations.activeDetailRequest
                    , \_ -> activated.observations.detailReturnTarget |> Expect.equal (Just "duplicate-card")
                    , \_ -> activated.observations.detailNavigationToken |> Expect.equal (saving.observations.detailNavigationToken + 1)
                    , \_ -> returned.observations.selectedId |> Expect.equal Nothing
                    , \_ -> returned.observations.edit |> Expect.equal saving.observations.edit
                    , \_ -> returned.observations.detailNavigationToken |> Expect.equal (activated.observations.detailNavigationToken + 1)
                    , \_ -> returned.observations.detailReturnTarget |> Expect.equal Nothing
                    ] ()
        , test "originating repeated card uses its exact native arrow and removes redundant detail chrome" <|
            \_ ->
                let
                    model =
                        appliedModeModel ObservationFlatMode

                    observation =
                        fixtureObservation "curated" "2026-01-01T00:00:00Z"

                    view =
                        Feature.Observation.viewObservations (observationWorkspace Api.Repository) model |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> view |> Query.find [ Selector.id (Feature.Observation.observationCardDomId "flat" observation.id) ] |> Event.simulate Event.click |> Event.expect ReturnObservationResults
                    , \_ -> view |> Query.find [ Selector.id (Feature.Observation.observationCardDomId "flat" observation.id) ] |> Query.has [ Selector.tag "button", Selector.text "▼", Selector.attribute (attribute "aria-expanded" "true"), Selector.attribute (attribute "aria-controls" "observation-detail"), Selector.attribute (attribute "data-observation-detail-anchor" "true") ]
                    , \_ -> view |> Query.hasNot [ Selector.id "observation-detail-heading" ]
                    , \_ -> view |> Query.hasNot [ Selector.class "observation-return" ]
                    , \_ -> view |> Query.hasNot [ Selector.id "observation-edit" ]
                    ] ()
        , test "direct user selection chooses results fallback instead of inheriting an earlier card origin" <|
            \_ ->
                let
                    model =
                        appliedModeModel ObservationFlatMode

                    selected =
                        Feature.Observation.update (SelectObservationFrom "curated" "original-card") model |> Tuple.first

                    direct =
                        Feature.Observation.update (SelectObservation "linked-outside-page") selected |> Tuple.first
                in
                direct.observations.detailReturnTarget |> Expect.equal Nothing
        , test "clearing selection retires navigation identity without retiring a protected saving owner" <|
            \_ ->
                let
                    saving =
                        savingEditModel "navigation retirement"

                    activated =
                        Feature.Observation.update (SelectObservationFrom "curated" "original-card") saving |> Tuple.first

                    cleared =
                        Feature.Observation.clearSelection activated.observations
                in
                Expect.all
                    [ \_ -> cleared.detailNavigationToken |> Expect.equal (activated.observations.detailNavigationToken + 1)
                    , \_ -> cleared.detailReturnTarget |> Expect.equal Nothing
                    , \_ -> cleared.edit |> Expect.equal activated.observations.edit
                    , \_ -> cleared.activeDetailRequest |> Expect.equal Nothing
                    ] ()
        , test "facet catalogue and match disclosures expose accessible modes while bounding repeated card DOM" <|
            \_ ->
                let
                    initial =
                        Feature.Observation.init

                    facet =
                        facetFixture Api.SubjectGlob "very/long/shared/**/*.elm" 123 "2026-08-30T12:00:00Z"

                    facetState =
                        { initial
                            | requestMode = ObservationFacetMode
                            , facets = Dict.singleton (Feature.Observation.facetKey facet.subjectKind facet.subject) facet
                            , facetKeys = [ Feature.Observation.facetKey facet.subjectKind facet.subject ]
                            , facetHasMore = True
                        }

                    facetView =
                        Feature.Observation.viewObservationsState (observationWorkspace Api.Repository) facetState |> Query.fromHtml

                    repeated =
                        observationWithSubjects "repeated"
                            [ { subjectKind = Api.SubjectFile, subject = "src/Main.elm" }
                            , { subjectKind = Api.SubjectGlob, subject = "src/**/*.elm" }
                            ]

                    evidence =
                        matchFixture repeated
                            [ { path = "src/Main.elm", matchedSubjects = repeated.subjects } ]

                    fileGroup =
                        Feature.Observation.matchGroupKey "src/Main.elm" Api.SubjectFile "src/Main.elm"

                    globGroup =
                        Feature.Observation.matchGroupKey "src/Main.elm" Api.SubjectGlob "src/**/*.elm"

                    matchState =
                        { initial
                            | requestMode = ObservationMatchMode
                            , matchPathsInput = "src/Main.elm"
                            , matchAppliedPaths = [ "src/Main.elm" ]
                            , items = Dict.singleton repeated.id repeated
                            , orderedIds = [ repeated.id ]
                            , matchEvidence = Dict.singleton repeated.id evidence
                            , selectedId = Just repeated.id
                        }

                    collapsed =
                        Feature.Observation.viewObservationsState (observationWorkspace Api.Repository) matchState |> Query.fromHtml

                    oneExpanded =
                        Feature.Observation.viewObservationsState (observationWorkspace Api.Repository)
                            { matchState | expandedMatchGroups = Dict.singleton fileGroup True }
                            |> Query.fromHtml

                    bothExpanded =
                        Feature.Observation.viewObservationsState (observationWorkspace Api.Repository)
                            { matchState | expandedMatchGroups = Dict.fromList [ ( fileGroup, True ), ( globGroup, True ) ] }
                            |> Query.fromHtml

                    fileCardId =
                        Feature.Observation.observationCardDomId fileGroup repeated.id

                    globCardId =
                        Feature.Observation.observationCardDomId globGroup repeated.id
                in
                Expect.all
                    [ \_ -> facetView |> Query.find [ Selector.class "observation-mode-button", Selector.class "btn-primary" ] |> Query.has [ Selector.text "By subject" ]
                    , \_ -> facetView |> Query.has [ Selector.text "123 observations", Selector.text "Latest update: 2026-08-30", Selector.text "Load more subjects", Selector.attribute (attribute "aria-live" "polite") ]
                    , \_ -> facetView |> Query.hasNot [ Selector.id "observation-subject" ]
                    , \_ -> collapsed |> Query.findAll [ Selector.class "observation-card" ] |> Query.count (Expect.equal 0)
                    , \_ -> collapsed |> Query.findAll [ Selector.class "observation-subject-group-toggle", Selector.attribute (attribute "aria-expanded" "false") ] |> Query.count (Expect.equal 2)
                    , \_ -> oneExpanded |> Query.findAll [ Selector.class "observation-card" ] |> Query.count (Expect.equal 1)
                    , \_ -> oneExpanded |> Query.find [ Selector.id fileCardId ] |> Query.has [ Selector.class "observation-card-selected", Selector.attribute (attribute "aria-current" "true") ]
                    , \_ -> bothExpanded |> Query.findAll [ Selector.class "observation-card" ] |> Query.count (Expect.equal 2)
                    , \_ -> bothExpanded |> Query.find [ Selector.id globCardId ] |> Query.hasNot [ Selector.class "observation-card-selected" ]
                    , \_ -> (fileCardId == globCardId) |> Expect.equal False
                    ]
                    ()
        ]


observationPageMessage : Int -> Result Http.Error Api.Observation -> Model -> Msg
observationPageMessage offset result model =
    let
        state =
            model.observations
    in
    case state.requestMode of
        ObservationFacetMode ->
            GotObservationSubjectFacets "workspace-1" model.sessionRequestEpoch state.facetRequestGeneration state.facetFingerprint offset
                (Result.map (\observation -> { items = [ facetFixture observation.subjectKind observation.subject 1 observation.updatedAt ], hasMore = False }) result)
        ObservationMatchMode ->
            GotObservationMatches "workspace-1" model.sessionRequestEpoch state.requestGeneration state.queryFingerprint offset
                (Result.map (\observation -> { items = [ matchFixture observation [ { path = "src/Main.elm", matchedSubjects = observation.subjects } ] ], hasMore = False }) result)
        _ ->
            GotObservations "workspace-1" Nothing state.requestGeneration state.queryFingerprint offset
                (Result.map (\observation -> { items = [ observation ], hasMore = False }) result)


applyObservationPage : Msg -> Model -> Model
applyObservationPage message model =
    case message of
        GotObservations _ _ _ _ _ _ -> Feature.DataLoading.update message model |> Tuple.first
        _ -> Feature.Observation.update message model |> Tuple.first


observationModes : List ObservationRequestMode
observationModes =
    [ ObservationFlatMode, ObservationFacetMode, ObservationExactSubjectMode, ObservationMatchMode ]


appliedModeModel : ObservationRequestMode -> Model
appliedModeModel mode =
    let
        original =
            selectedObservationState (fixtureObservation "curated" "2026-01-01T00:00:00Z")

        initial =
            editableModel { original | query = "applied search", subjectKind = Just Api.SubjectFile, subject = "src/Main.elm", gitSha = fullSha, matchPathsInput = "src/Main.elm" }

        requested =
            Feature.Observation.update
                (case mode of
                    ObservationExactSubjectMode -> SelectObservationFacet Api.SubjectGlob "src/**/*.elm"
                    ObservationMatchMode -> ApplyObservationMatch
                    _ -> SetObservationBrowseMode mode
                ) initial |> Tuple.first

        state =
            requested.observations

        observation =
            fixtureObservation "curated" "2026-01-01T00:00:00Z"

        facet =
            facetFixture Api.SubjectGlob "src/**/*.elm" 9 "2026-01-01T00:00:00Z"

        key =
            Feature.Observation.facetKey facet.subjectKind facet.subject
    in
    { requested
        | observations =
            { state
                | loading = False
                , expectedOffset = Nothing
                , hasMore = True
                , facetLoading = False
                , facetExpectedOffset = Nothing
                , facetHasMore = True
                , facets = Dict.singleton key facet
                , facetKeys = [ key ]
                , items = original.items
                , orderedIds = original.orderedIds
                , matchEvidence = Dict.singleton observation.id (matchFixture observation [ { path = "src/Main.elm", matchedSubjects = observation.subjects } ])
            }
    }


draftQueryInputs : Model -> Model
draftQueryInputs model =
    [ SetObservationQuery "draft search", SetObservationSubjectKind "glob", SetObservationSubject "draft/**/*.elm", SetObservationGitSha (String.repeat 40 "a"), SetObservationMatchPaths "src/Draft.elm" ]
        |> List.foldl (\message current -> Feature.Observation.update message current |> Tuple.first) model


observationWorkspace : Api.WorkspaceType -> Api.Workspace
observationWorkspace workspaceType =
    { id = "workspace-1"
    , name = "Workspace"
    , workspaceType = workspaceType
    , ghOwner = Nothing
    , ghRepo = Nothing
    , createdAt = "2026-01-01T00:00:00Z"
    , updatedAt = "2026-01-01T00:00:00Z"
    }


fullSha : String
fullSha =
    "0123456789abcdef0123456789abcdef01234567"


fileFixture : String
fileFixture =
    """{"id":"observation-file","workspace_id":"workspace-1","subject_kind":"file","subject":"src/Main.elm","git_sha":"0123456789abcdef0123456789abcdef01234567","content_version":"10000000-0000-4000-8000-000000000000","content":"File observation","created_at":"2026-01-01T00:00:00Z","updated_at":"2026-01-02T00:00:00Z"}"""


routeFlags : Flags
routeFlags =
    { apiUrl = "https://api.example"
    , wsUrl = "wss://api.example"
    , sessionId = "session-1"
    , runtimeMode = "test"
    , authTokenStorageKey = "hmem-auth-token"
    , authTokenPresent = False
    , loginUrl = Nothing
    , logoutUrl = Nothing
    }


workspaceUrl : String -> Url.Url
workspaceUrl fragment =
    { protocol = Url.Https
    , host = "app.example"
    , port_ = Nothing
    , path = "/workspace/workspace-1"
    , query = Nothing
    , fragment = Just fragment
    }


sameWorkspaceModel : ObservationModel -> Model
sameWorkspaceModel observations =
    let
        sourceUrl =
            workspaceUrl "tab=observations&observation=selected-observation"

        initial =
            AppShell.initModel
                Nothing
                sourceUrl
                (WorkspacePage "workspace-1")
                routeFlags
                Nothing
                (Helpers.parseFragment sourceUrl.fragment)
                |> AppShell.finalizeInit (WorkspacePage "workspace-1")
    in
    { initial | observations = observations }


editableModel : ObservationModel -> Model
editableModel observations =
    let
        base =
            sameWorkspaceModel observations

        workspace =
            observationWorkspace Api.Repository
    in
    { base
        | auth = { status = AuthReady, mode = Just "deployed" }
        , sessionContext = Just editorSession
        , sessionRequestEpoch = 3
        , selectedWorkspaceId = Just workspace.id
        , workspaces = Dict.singleton workspace.id workspace
    }


editorSession : Api.SessionContext
editorSession =
    { authMode = "deployed"
    , principal =
        { actorType = "user"
        , actorId = "editor"
        , actorLabel = "Editor"
        , authority = "grant_user"
        , grantUserId = Just "editor"
        }
    , globalPermissions = { createWorkspace = False, superadmin = False }
    , workspace =
        Just
            { workspaceId = "workspace-1"
            , role = Just "edit"
            , canRead = True
            , canEdit = True
            , canAdmin = False
            }
    }


readOnlySession : Api.SessionContext
readOnlySession =
    let
        workspaceContext =
            { workspaceId = "workspace-1"
            , role = Just "read"
            , canRead = True
            , canEdit = False
            , canAdmin = False
            }
    in
    { editorSession | workspace = Just workspaceContext }


selectedObservationState : Api.Observation -> ObservationModel
selectedObservationState observation =
    let
        initial =
            Feature.Observation.init
    in
    { initial
        | items = Dict.singleton observation.id observation
        , orderedIds = [ observation.id ]
        , selectedId = Just observation.id
        , selectedDetail = Just observation
        , matchEvidence =
            Dict.singleton observation.id
                { observation = observation
                , pathMatches =
                    [ { path = observation.subject
                      , matchedSubjects = observation.subjects
                      }
                    ]
                , matchedPaths = [ observation.subject ]
                , matchedSubjects = observation.subjects
                }
    }


savingEditModel : String -> Model
savingEditModel draft =
    let
        observation =
            fixtureObservation "curated" "2026-01-01T00:00:00Z"

        started =
            Feature.Observation.update StartObservationEdit (editableModel (selectedObservationState observation)) |> Tuple.first

        drafted =
            Feature.Observation.update (SetObservationDraft draft) started |> Tuple.first
    in
    Feature.Observation.update SaveObservationEdit drafted |> Tuple.first


deletingModel : Model
deletingModel =
    let
        observation =
            fixtureObservation "curated" "2026-01-01T00:00:00Z"

        opened =
            Feature.Observation.update OpenObservationDelete (editableModel (selectedObservationState observation)) |> Tuple.first
    in
    Feature.Observation.update ConfirmObservationDelete opened |> Tuple.first


fixtureObservation : String -> String -> Api.Observation
fixtureObservation id createdAt =
    { id = id
    , workspaceId = "workspace-1"
    , subjects = [ { subjectKind = Api.SubjectFile, subject = "src/Main.elm" } ]
    , subjectKind = Api.SubjectFile
    , subject = "src/Main.elm"
    , gitSha = fullSha
    , content = "Observation"
    , contentVersion = "10000000-0000-4000-8000-000000000000"
    , createdAt = createdAt
    , updatedAt = createdAt
    }


observationWithSubjects : String -> List Api.ObservationSubject -> Api.Observation
observationWithSubjects observationId subjects =
    case subjects of
        first :: _ ->
            { id = observationId
            , workspaceId = "workspace-1"
            , subjects = subjects
            , subjectKind = first.subjectKind
            , subject = first.subject
            , gitSha = fullSha
            , content = "Observation " ++ observationId
            , contentVersion = "10000000-0000-4000-8000-000000000000"
            , createdAt = "2026-01-01T00:00:00Z"
            , updatedAt = "2026-01-01T00:00:00Z"
            }

        [] ->
            fixtureObservation observationId "2026-01-01T00:00:00Z"


matchFixture : Api.Observation -> List Api.ObservationPathMatch -> Api.ObservationMatch
matchFixture observation pathMatches =
    { observation = observation
    , pathMatches = pathMatches
    , matchedPaths = List.map .path pathMatches
    , matchedSubjects = List.concatMap .matchedSubjects pathMatches
    }


facetFixture : Api.SubjectKind -> String -> Int -> String -> Api.ObservationSubjectFacet
facetFixture subjectKind subject observationCount latestUpdatedAt =
    { subjectKind = subjectKind
    , subject = subject
    , observationCount = observationCount
    , latestUpdatedAt = latestUpdatedAt
    }


sameWorkspaceRepositoryModel : Int -> ObservationModel -> Model
sameWorkspaceRepositoryModel sessionEpoch observations =
    let
        shell =
            editableModel observations

        workspace =
            observationWorkspace Api.Repository
    in
    { shell
        | selectedWorkspaceId = Just workspace.id
        , workspaces = Dict.singleton workspace.id workspace
        , sessionRequestEpoch = sessionEpoch
    }


observationSnapshotWire : Api.Observation -> String
observationSnapshotWire observation =
    observationSnapshotWireMany [ observation ]


observationSnapshotWireMany : List Api.Observation -> String
observationSnapshotWireMany observations =
    Encode.object
        [ ( "schema_version", Encode.int 1 )
        , ( "transport", Encode.string "snapshot" )
        , ( "scope", workspaceScopeValue )
        , ( "items"
          , Encode.list identity
                (snapshotItem "workspace"
                    (Encode.object
                        [ ( "id", Encode.string "workspace-1" )
                        , ( "name", Encode.string "Workspace" )
                        , ( "workspace_type", Encode.string "repository" )
                        , ( "created_at", Encode.string "2026-01-01T00:00:00Z" )
                        , ( "updated_at", Encode.string "2026-01-01T00:00:00Z" )
                        ]
                    )
                    :: List.map (snapshotItem "observation" << observationValue) observations
                )
          )
        , ( "resume_token", Encode.string "snapshot-token" )
        ]
        |> Encode.encode 0


observationInvalidationWire : String -> String
observationInvalidationWire observationId =
    let
        change =
            Encode.object
                [ ( "schema_version", Encode.int 1 )
                , ( "type", Encode.string "change" )
                , ( "event"
                  , Encode.object
                        [ ( "schema_version", Encode.int 1 )
                        , ( "event_id", Encode.string "observation-updated" )
                        , ( "scope", Encode.string "workspace" )
                        , ( "workspace_id", Encode.string "workspace-1" )
                        , ( "occurred_at", Encode.string "2026-01-01T00:00:01Z" )
                        , ( "transaction", Encode.object [ ( "id", Encode.string "tx-observation" ), ( "cause", Encode.string "rest" ), ( "request_id", Encode.null ) ] )
                        , ( "actor", Encode.object [ ( "type", Encode.string "system" ), ( "id", Encode.null ) ] )
                        , ( "entity", Encode.object [ ( "type", Encode.string "observation" ), ( "id", Encode.string observationId ), ( "action", Encode.string "updated" ) ] )
                        , ( "invalidations"
                          , Encode.list identity
                                [ Encode.object [ ( "kind", Encode.string "entity" ), ( "target", Encode.string ("observation:" ++ observationId) ) ]
                                , Encode.object [ ( "kind", Encode.string "collection" ), ( "target", Encode.string "observations:workspace-1" ) ]
                                ]
                          )
                        ]
                  )
                ]
    in
    Encode.object
        [ ( "schema_version", Encode.int 1 )
        , ( "transport", Encode.string "frames" )
        , ( "scope", workspaceScopeValue )
        , ( "frames", Encode.list identity [ change ] )
        ]
        |> Encode.encode 0


workspaceScopeValue : Encode.Value
workspaceScopeValue =
    Encode.object
        [ ( "scope", Encode.string "workspace" )
        , ( "workspace_id", Encode.string "workspace-1" )
        ]


snapshotItem : String -> Encode.Value -> Encode.Value
snapshotItem kind data =
    Encode.object
        [ ( "schema_version", Encode.int 1 )
        , ( "kind", Encode.string kind )
        , ( "data", data )
        ]


observationValue : Api.Observation -> Encode.Value
observationValue observation =
    Encode.object
        [ ( "id", Encode.string observation.id )
        , ( "workspace_id", Encode.string observation.workspaceId )
        , ( "subjects"
          , Encode.list
                (\subject ->
                    Encode.object
                        [ ( "subject_kind", Encode.string (Api.subjectKindToString subject.subjectKind) )
                        , ( "subject", Encode.string subject.subject )
                        ]
                )
                observation.subjects
          )
        , ( "git_sha", Encode.string observation.gitSha )
        , ( "content", Encode.string observation.content )
        , ( "content_version", Encode.string observation.contentVersion )
        , ( "created_at", Encode.string observation.createdAt )
        , ( "updated_at", Encode.string observation.updatedAt )
        ]


paginatedFixture : String -> String
paginatedFixture hasMore =
    "{\"items\":[" ++ fileFixture ++ "," ++ globFixture ++ "],\"has_more\":" ++ hasMore ++ "}"


globFixture : String
globFixture =
    """{"id":"observation-glob","workspace_id":"workspace-1","subject_kind":"glob","subject":"src/**/*.elm","git_sha":"0123456789abcdef0123456789abcdef01234567","content_version":"10000000-0000-4000-8000-000000000000","content":"Glob observation","created_at":"2026-01-01T00:00:00Z","updated_at":"2026-01-02T00:00:00Z"}"""


isErr : Result error value -> Bool
isErr result =
    case result of
        Err _ ->
            True

        Ok _ ->
            False


isOk : Result error value -> Bool
isOk result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False


largePathInput : Int -> Int -> String
largePathInput count bytesPerPath =
    List.range 1 count
        |> List.map
            (\index ->
                let
                    prefix =
                        String.fromInt index ++ "/"
                in
                prefix ++ String.repeat (bytesPerPath - String.length prefix) "a"
            )
        |> String.join "\n"


observationUrlTests : List Test
observationUrlTests =
    [ ObservationFlatMode, ObservationFacetMode, ObservationExactSubjectMode, ObservationMatchMode ]
        |> List.concatMap (\mode ->
            [ test ("complete applied URL round-trips Unicode and reserved values in " ++ Debug.toString mode) <| \_ ->
                let
                    query = { requestMode = mode, query = "café & #+%= 🙂", subjectKind = Just Api.SubjectGlob, subject = "manual & value", selectedFacet = Just { subjectKind = Api.SubjectGlob, subject = "src/**/*.elm" }, gitSha = fullSha, matchAppliedPaths = if mode == ObservationMatchMode then [ "src/é& #+.elm", "src/View.elm" ] else [] }
                    state = Helpers.restoreObservationQuery query Feature.Observation.init
                    draft = { state | query = "unapplied secret", matchPathsInput = "unapplied path" }
                    model = editableModel draft
                    restored = Helpers.completeObservationUrl model |> Result.toMaybe |> Maybe.andThen Url.fromString |> Maybe.map Helpers.observationUrlContext
                in
                Expect.equal (Just query) (Maybe.map .query restored)
            , test ("authorized bootstrap preserves mode and closes initial loading accounting in " ++ Debug.toString mode) <| \_ ->
                let
                    query = { requestMode = mode, query = "Cache", subjectKind = Nothing, subject = "", selectedFacet = if mode == ObservationExactSubjectMode then Just { subjectKind = Api.SubjectGlob, subject = "src/**/*.elm" } else Nothing, gitSha = fullSha, matchAppliedPaths = if mode == ObservationMatchMode then [ "src/Main.elm" ] else [] }
                    original = editableModel (Helpers.restoreObservationQuery query Feature.Observation.init)
                    loading = original.dataLoading
                    initial = { original | dataLoading = { loading | activeWorkspaceLoadToken = Just 44 } }
                    after = Feature.DataLoading.update (GotWorkspace "workspace-1" 44 (Ok (observationWorkspace Api.Repository))) initial |> Tuple.first
                in
                Expect.all
                    [ \_ -> Helpers.observationAppliedQuery after.observations |> Expect.equal query
                    , \_ -> (after.observations.requestMode, after.observations.expectedOffset, after.observations.facetExpectedOffset) |> Expect.equal ( mode, if mode == ObservationFacetMode then Nothing else Just 0, if mode == ObservationFacetMode then Just 0 else Nothing )
                    , \_ -> after.dataLoading.pendingWorkspaceLoads |> Expect.equal (if List.member mode [ ObservationFlatMode, ObservationExactSubjectMode ] then 2 else 1)
                    ] ()
            ])
        |> (\modeTests -> modeTests ++
            [ test "malformed, duplicate, unknown, unsupported and invalid-path links restore atomically" <| \_ ->
                let
                    encoded = Helpers.encodeObservationQuery Helpers.defaultObservationQuery
                    fragments = [ "tab=observations&observation=a&ov=9&oq=" ++ encoded, "tab=observations&ov=1&ov=1&oq=" ++ encoded, "tab=observations&ov=1&oq=" ++ encoded ++ "&unknown=x", "tab=observations&ov=1&oq=%ZZ", "tab=observations&ov=1&oq=" ++ Url.percentEncode "[\"match\",\"\",null,\"\",\"\",null,null,[\"../bad\"]]", "tab=observations&ov=1&oq=" ++ encoded ++ "&focus=bad", "tab=observations&ox=" ]
                    contexts = List.map (workspaceUrl >> Helpers.observationUrlContext) fragments
                in
                contexts |> List.all (\context -> context.query == Helpers.defaultObservationQuery && context.fragment.observationId == Nothing && context.fragment.focus == Nothing && context.notice /= Nothing) |> Expect.equal True
            , test "complete encoded URL cap accepts 4096 and rejects 4097 bytes" <| \_ ->
                let
                    model = editableModel Feature.Observation.init
                    base = Helpers.observationUrl model ("&ov=1&oq=" ++ Helpers.encodeObservationQuery Helpers.defaultObservationQuery)
                    defaultQuery = Helpers.defaultObservationQuery
                    atQuery = { defaultQuery | query = String.repeat (4096 - Helpers.observationUrlBytes base) "a" }
                    at = { model | observations = Helpers.restoreObservationQuery atQuery model.observations }
                    aboveQuery = { atQuery | query = atQuery.query ++ "a" }
                in
                Expect.equal ( True, True ) ( Helpers.completeObservationUrl at |> isOk, Helpers.completeObservationUrl { model | observations = Helpers.restoreObservationQuery aboveQuery model.observations } |> isErr )
            , test "shell refresh hydrates missing restored detail once and keeps its request ownership" <| \_ ->
                let
                    state = Feature.Observation.init
                    model = editableModel { state | selectedId = Just "off-page" }
                    first = Feature.Observation.refreshActiveResults model |> Tuple.first
                    second = Feature.Observation.refreshActiveResults first |> Tuple.first
                in
                Expect.equal ( Just "off-page", first.observations.activeDetailRequest, first.observations.nextDetailRequestToken ) ( second.observations.selectedId, second.observations.activeDetailRequest, second.observations.nextDetailRequestToken )
            , test "match links normalize ordered paths and preserve the legacy off-page selection" <| \_ ->
                let
                    url = workspaceUrl ("tab=observations&observation=off%26page&ov=1&oq=" ++ Url.percentEncode "[\"match\",\"\",null,\"\",\"\",null,null,[\" src/Main.elm \",\"src/View.elm\",\"src/Main.elm\"]]")
                    context = Helpers.observationUrlContext url
                    legacy = Helpers.observationUrlContext (workspaceUrl "observation=off%26page")
                in
                Expect.equal ( [ "src/Main.elm", "src/View.elm" ], Just "off&page", Just "off&page" ) (context.query.matchAppliedPaths, context.fragment.observationId, legacy.fragment.observationId)
            , test "oversized own replacement proof is consumed once and does not resurrect on Back or ABA" <| \_ ->
                let
                    query = { requestMode = ObservationMatchMode, query = "", subjectKind = Nothing, subject = "", selectedFacet = Nothing, gitSha = "", matchAppliedPaths = [ "src/" ++ String.repeat 1800 "é" ++ ".elm" ] }
                    original = editableModel (Helpers.restoreObservationQuery query Feature.Observation.init)
                    issued = Helpers.writeObservationHistory True original |> Tuple.first
                    url = issued.observations.pendingExcludedLink |> Maybe.andThen (.url >> Url.fromString) |> Maybe.withDefault original.url
                    consumed = Route.handleUrlChange url issued |> Tuple.first
                    back = Route.handleUrlChange url consumed |> Tuple.first
                    superseded = Route.handleUrlChange (workspaceUrl "tab=observations") issued |> Tuple.first
                    aba = Route.handleUrlChange url superseded |> Tuple.first
                    next = Helpers.writeObservationHistory True issued |> Tuple.first
                in
                Expect.all
                    [ \_ -> Helpers.completeObservationUrl original |> isErr |> Expect.equal True
                    , \_ -> Helpers.observationAppliedQuery consumed.observations |> Expect.equal query
                    , \_ -> consumed.observations.pendingExcludedLink |> Expect.equal Nothing
                    , \_ -> Helpers.observationAppliedQuery back.observations |> Expect.equal Helpers.defaultObservationQuery
                    , \_ -> Helpers.observationAppliedQuery aba.observations |> Expect.equal Helpers.defaultObservationQuery
                    , \_ -> Maybe.map .url next.observations.pendingExcludedLink == Maybe.map .url issued.observations.pendingExcludedLink |> Expect.equal False
                    ] ()
            , test "same applied route preserves unapplied controls and unchanged request identity" <| \_ ->
                let
                    query = { requestMode = ObservationFlatMode, query = "applied", subjectKind = Nothing, subject = "", selectedFacet = Nothing, gitSha = "", matchAppliedPaths = [] }
                    state = Helpers.restoreObservationQuery query Feature.Observation.init
                    model = editableModel { state | query = "unapplied", requestGeneration = 99, selectedId = Nothing }
                    url = Helpers.completeObservationUrl model |> Result.toMaybe |> Maybe.andThen Url.fromString |> Maybe.withDefault model.url
                    after = Route.handleUrlChange url model |> Tuple.first
                in
                Expect.equal ( "unapplied", 99, Just query ) (after.observations.query, after.observations.requestGeneration, after.observations.appliedQuery)
            , test "changed URL query fences old pages without mutating the protected owner" <| \_ ->
                let
                    initialState = Feature.Observation.init
                    selected = fixtureObservation "selected-observation" "2026-01-01T00:00:00Z"
                    original = editableModel ({ initialState | selectedId = Just selected.id, selectedDetail = Just selected, items = Dict.singleton selected.id selected, orderedIds = [ selected.id ] }) |> Feature.Observation.update StartObservationEdit |> Tuple.first |> Feature.Observation.update (SetObservationDraft "protected URL draft") |> Tuple.first
                    old = Feature.Observation.update ApplyObservationFilters original |> Tuple.first
                    query = { requestMode = ObservationFlatMode, query = "changed", subjectKind = Nothing, subject = "", selectedFacet = Nothing, gitSha = "", matchAppliedPaths = [] }
                    target = Helpers.completeObservationUrl { old | observations = Helpers.restoreObservationQuery query old.observations } |> Result.toMaybe |> Maybe.andThen Url.fromString |> Maybe.withDefault old.url
                    after = Route.handleUrlChange target old |> Tuple.first
                    late = Feature.DataLoading.update (GotObservations "workspace-1" Nothing old.observations.requestGeneration old.observations.queryFingerprint 0 (Ok { items = [ fixtureObservation "old-row" "2026-01-01T00:00:00Z" ], hasMore = False })) after |> Tuple.first
                in
                Expect.all
                    [ \_ -> after.observations.edit |> Expect.equal old.observations.edit
                    , \_ -> after.observations.requestGeneration > old.observations.requestGeneration |> Expect.equal True
                    , \_ -> Dict.member "old-row" late.observations.items |> Expect.equal False
                    ] ()
            ])


observationUrlReviewTests : List Test
observationUrlReviewTests =
    [ ObservationExactSubjectMode, ObservationMatchMode ]
        |> List.concatMap (\mode ->
            [ "unauthorized", "session401", "token", "read-regrant" ] |> List.map (\boundary ->
                test ("fresh URL admission after " ++ boundary ++ " restores " ++ Debug.toString mode ++ " with no retired private state or stale callback") <| \_ ->
                    let
                        state = Helpers.restoreObservationQuery (urlReviewQuery mode) Feature.Observation.init
                        selected = fixtureObservation "off-page" "2026-01-01T00:00:00Z"
                        beforeDetail = Feature.Observation.selectObservation selected.id (editableModel { state | selectedId = Just selected.id }) |> Tuple.first
                        oldRequest = beforeDetail.observations.activeDetailRequest |> Maybe.withDefault { workspaceId = "workspace-1", observationId = selected.id, sessionEpoch = beforeDetail.sessionRequestEpoch, token = 0 }
                        loaded = Feature.Observation.update (GotObservationDetail "workspace-1" selected.id oldRequest.sessionEpoch oldRequest.token (Ok selected)) beforeDetail |> Tuple.first
                        edited = loaded |> Feature.Observation.update StartObservationEdit |> Tuple.first |> Feature.Observation.update (SetObservationDraft "private retired draft") |> Tuple.first
                        sourceUrl = Helpers.completeObservationUrl edited |> Result.toMaybe |> Maybe.andThen Url.fromString |> Maybe.withDefault edited.url
                        source = { edited | url = sourceUrl }
                        deniedWorkspace = editorSession.workspace |> Maybe.map (\permission -> { permission | canRead = False, canEdit = False, canAdmin = False })
                        retired = AppShell.handleOwned
                            (case boundary of
                                "session401" -> AppShell.SessionContextLoadedMsg source.sessionRequestEpoch (Just "workspace-1") (Err (Http.BadStatus 401))
                                "token" -> AppShell.AuthTokenChangedMsg True
                                "read-regrant" -> AppShell.SessionContextLoadedMsg source.sessionRequestEpoch (Just "workspace-1") (Ok { editorSession | workspace = deniedWorkspace })
                                _ -> AppShell.AuthUnauthorizedMsg
                            ) source |> Tuple.first
                        admitted = AppShell.handleOwned (AppShell.SessionContextLoadedMsg retired.sessionRequestEpoch (Just "workspace-1") (Ok editorSession)) retired |> Tuple.first
                        workspace = observationWorkspace Api.Repository
                        freshWorkspace = { admitted | workspaces = Dict.singleton workspace.id workspace }
                        fresh = Feature.Observation.refreshActiveResults freshWorkspace |> Tuple.first
                        lateSuccess = Feature.Observation.update (GotObservationDetail "workspace-1" selected.id oldRequest.sessionEpoch oldRequest.token (Ok selected)) fresh |> Tuple.first
                        lateError = Feature.Observation.update (GotObservationDetail "workspace-1" selected.id oldRequest.sessionEpoch oldRequest.token (Err Http.NetworkError)) fresh |> Tuple.first
                    in
                    Expect.all
                        [ \_ -> Helpers.observationAppliedQuery admitted.observations |> Expect.equal (urlReviewQuery mode)
                        , \_ -> (admitted.observations.selectedId, admitted.observations.edit, admitted.observations.activeDetailRequest) |> Expect.equal (Just selected.id, Nothing, Nothing)
                        , \_ -> admitted.observations.pendingExcludedLink |> Expect.equal Nothing
                        , \_ -> Dict.isEmpty admitted.observations.items |> Expect.equal True
                        , \_ -> fresh.observations.activeDetailRequest |> Maybe.map .token |> Maybe.map ((<) oldRequest.token) |> Expect.equal (Just True)
                        , \_ -> lateSuccess.observations |> Expect.equal fresh.observations
                        , \_ -> lateError.observations |> Expect.equal fresh.observations
                        , \_ -> if boundary == "read-regrant" then retired.sessionRequestEpoch |> Expect.equal source.sessionRequestEpoch else Expect.pass
                        ] ()
            ))
        |> (\admissionTests -> admissionTests ++
            ([ ObservationFlatMode, ObservationExactSubjectMode ] |> List.concatMap (\initialMode ->
                [ ObservationFacetMode, ObservationMatchMode ] |> List.concatMap (\targetMode ->
                    [ True, False ] |> List.map (\success ->
                        test ("superseded tagged " ++ Debug.toString initialMode ++ " bootstrap settles once before " ++ Debug.toString targetMode ++ " and old " ++ (if success then "success" else "error")) <| \_ ->
                            let
                                original = editableModel (Helpers.restoreObservationQuery (urlReviewQuery initialMode) Feature.Observation.init)
                                loading = original.dataLoading
                                initial = { original | dataLoading = { loading | activeWorkspaceLoadToken = Just 99 } }
                                bootstrapped = Feature.DataLoading.update (GotWorkspace "workspace-1" 99 (Ok (observationWorkspace Api.Repository))) initial |> Tuple.first
                                old = bootstrapped.observations
                                query = urlReviewQuery targetMode
                                targetUrl = Helpers.completeObservationUrl { bootstrapped | observations = Helpers.restoreObservationQuery query old } |> Result.toMaybe |> Maybe.andThen Url.fromString |> Maybe.withDefault bootstrapped.url
                                routed = Route.handleUrlChange targetUrl bootstrapped |> Tuple.first
                                rootCompleted = case routed.dataLoading.rootNavigationRequest of
                                    Just request -> Feature.DataLoading.update (GotRootNavigation "workspace-1" request.sessionEpoch (Just 99) request.generation request.filterFingerprint 0 0 (Ok { workspaceId = "workspace-1", projects = { items = [], hasMore = False }, tasks = { items = [], hasMore = False } })) routed |> Tuple.first
                                    Nothing -> routed
                                lateResult = if success then Ok { items = [ fixtureObservation "retired-row" "2026-01-01T00:00:00Z" ], hasMore = False } else Err Http.NetworkError
                                late = Feature.DataLoading.update (GotObservations "workspace-1" (Just 99) old.requestGeneration old.queryFingerprint 0 lateResult) rootCompleted |> Tuple.first
                                completed = if targetMode == ObservationFacetMode then Feature.Observation.update (GotObservationSubjectFacets "workspace-1" late.sessionRequestEpoch late.observations.facetRequestGeneration late.observations.facetFingerprint 0 (Ok { items = [], hasMore = False })) late |> Tuple.first else Feature.Observation.update (GotObservationMatches "workspace-1" late.sessionRequestEpoch late.observations.requestGeneration late.observations.queryFingerprint 0 (Ok { items = [], hasMore = False })) late |> Tuple.first
                                repetition = Feature.DataLoading.update (GotObservations "workspace-1" (Just 99) old.requestGeneration old.queryFingerprint 0 lateResult) completed |> Tuple.first
                            in
                            Expect.all
                                [ \_ -> routed.dataLoading.pendingWorkspaceLoads |> Expect.equal 1
                                , \_ -> routed.dataLoading.initialObservationLoad |> Expect.equal Nothing
                                , \_ -> late.observations |> Expect.equal rootCompleted.observations
                                , \_ -> (completed.dataLoading.pendingWorkspaceLoads, completed.dataLoading.loadingWorkspaceData, completed.dataLoading.activeWorkspaceLoadToken) |> Expect.equal (0, False, Nothing)
                                , \_ -> repetition |> Expect.equal completed
                                ] ()
                    )))) ++
            [ test "initial obligation cannot settle another workspace, session, token or superseding load" <| \_ ->
                let
                    original = editableModel Feature.Observation.init
                    loading = original.dataLoading
                    initial = { original | dataLoading = { loading | activeWorkspaceLoadToken = Just 91 } }
                    bootstrapped = Feature.DataLoading.update (GotWorkspace "workspace-1" 91 (Ok (observationWorkspace Api.Repository))) initial |> Tuple.first
                    old = bootstrapped.observations
                    callback model = Feature.DataLoading.update (GotObservations "workspace-1" (Just 91) old.requestGeneration old.queryFingerprint 0 (Err Http.NetworkError)) model |> Tuple.first
                    newerLoading = bootstrapped.dataLoading
                    cases = [ { bootstrapped | selectedWorkspaceId = Just "other" }, { bootstrapped | sessionRequestEpoch = bootstrapped.sessionRequestEpoch + 1 }, { bootstrapped | dataLoading = { newerLoading | activeWorkspaceLoadToken = Just 92, initialObservationLoad = newerLoading.initialObservationLoad |> Maybe.map (\request -> { request | token = 92 }) } } ]
                in
                let
                    evidence model =
                        ( model.dataLoading.pendingWorkspaceLoads, model.dataLoading.activeWorkspaceLoadToken, model.dataLoading.initialObservationLoad )
                in
                List.map (callback >> evidence) cases |> Expect.equal (List.map evidence cases)
            , test "unified search Observation intent pushes changed selection/tab once and replaces oversized contexts" <| \_ ->
                let
                    selected = fixtureObservation "a" "2026-01-01T00:00:00Z"
                    state = Feature.Observation.init
                    original = editableModel { state | selectedId = Just selected.id, selectedDetail = Just selected }
                    unchanged = Feature.Search.update (NavigateToSearchResult "observation" selected.id) original |> Tuple.first
                    fromTab = Feature.Search.update (NavigateToSearchResult "observation" selected.id) { original | activeTab = ProjectsTab } |> Tuple.first
                    changed = Feature.Search.update (NavigateToSearchResult "observation" "b") original |> Tuple.first
                    query = urlReviewQuery ObservationMatchMode
                    oversizedQuery = { query | matchAppliedPaths = [ "src/" ++ String.repeat 1800 "é" ++ ".elm" ] }
                    oversized = Feature.Search.update (NavigateToSearchResult "observation" "b") { original | observations = Helpers.restoreObservationQuery oversizedQuery original.observations } |> Tuple.first
                in
                Expect.all
                    [ \_ -> unchanged.observations.nextLinkToken |> Expect.equal original.observations.nextLinkToken
                    , \_ -> fromTab.observations.nextLinkToken |> Expect.equal (original.observations.nextLinkToken + 1)
                    , \_ -> changed.observations.nextLinkToken |> Expect.equal (original.observations.nextLinkToken + 1)
                    , \_ -> oversized.observations.pendingExcludedLink /= Nothing |> Expect.equal True
                    ] ()
            ])


urlReviewQuery : ObservationRequestMode -> Types.ObservationAppliedQuery
urlReviewQuery mode =
    { requestMode = mode, query = "Cache", subjectKind = Just Api.SubjectGlob, subject = "manual", selectedFacet = if mode == ObservationExactSubjectMode then Just { subjectKind = Api.SubjectGlob, subject = "src/**/*.elm" } else Nothing, gitSha = fullSha, matchAppliedPaths = if mode == ObservationMatchMode then [ "src/Main.elm", "src/View.elm" ] else [] }


returnReceipt : ObservationViewport.State -> Float -> String -> Maybe String -> Encode.Value
returnReceipt viewport width layout focus =
    measuredReturnReceipt viewport width layout focus []


measuredReturnReceipt : ObservationViewport.State -> Float -> String -> Maybe String -> List ( String, Float ) -> Encode.Value
measuredReturnReceipt viewport width layout focus measurements =
    Encode.object
        [ ( "stamp", ObservationViewport.stampValue viewport.stamp )
        , ( "navigationToken", Encode.int viewport.navigationToken ), ( "detailMounted", Encode.bool True )
        , ( "top", Encode.float viewport.top ), ( "height", Encode.float viewport.height ), ( "width", Encode.float width )
        , ( "layout", Encode.string layout ), ( "measurements", Encode.list (\( key, height ) -> Encode.object [ ( "key", Encode.string key ), ( "height", Encode.float height ) ]) measurements )
        , ( "focus", focus |> Maybe.map Encode.string |> Maybe.withDefault Encode.null ), ( "target", Encode.null ) ]


pendingReturnModel : Model
pendingReturnModel =
    let
        value = fixtureObservation "curated" "2026-01-01T00:00:00Z"
        initial = Feature.Observation.init
        before = editableModel initial
        loaded = editableModel { initial | items = Dict.singleton value.id value, orderedIds = [ value.id ] }
        projected = Feature.Observation.refreshViewport before ( loaded, Cmd.none ) |> Tuple.first
        state = projected.observations
        viewport = state.viewport
        painted = { projected | observations = { state | viewport = { viewport | width = 800, layout = "font16" } } }
        selected = Feature.Observation.update (SelectObservationFrom value.id (Feature.Observation.observationCardDomId "flat" value.id)) painted |> Tuple.first
        started = Feature.Observation.update StartObservationEdit selected |> Tuple.first
        dirty = Feature.Observation.update (SetObservationDraft "Protected return draft") started |> Tuple.first
        selectedState = dirty.observations
        narrow = selectedState.viewport
        detail = { dirty | observations = { selectedState | viewport = { narrow | width = 400 } } }
    in
    Feature.Observation.update ReturnObservationResults detail |> Feature.Observation.refreshViewport detail |> Tuple.first


observationReturnReceiptTests : List Test
observationReturnReceiptTests =
    [ test "restoration requires a subsequent painted receipt of its new revision and consumes Return once" <| \_ ->
        let
            returned = pendingReturnModel
            oldReceipt = returnReceipt returned.observations.viewport 800 "font16" Nothing
            restored = Feature.Observation.updateViewport oldReceipt returned |> Tuple.first
            stale = Feature.Observation.updateViewport oldReceipt restored |> Tuple.first
            settledReceipt = returnReceipt restored.observations.viewport 800 "font16" Nothing
            dispatched = Feature.Observation.updateViewport settledReceipt restored |> Tuple.first
            repeated = Feature.Observation.updateViewport settledReceipt dispatched |> Tuple.first
            focused = Feature.Observation.updateViewport (returnReceipt dispatched.observations.viewport 800 "font16" dispatched.observations.viewport.returnPin) dispatched |> Tuple.first
        in
        Expect.all
            [ \_ -> Expect.equal True returned.observations.viewport.restoring
            , \_ -> Expect.equal True (restored.observations.viewport.stamp.revision > returned.observations.viewport.stamp.revision)
            , \_ -> Expect.equal True (restored.observations.pendingReturnNavigation /= Nothing)
            , \_ -> Expect.equal restored.observations stale.observations
            , \_ -> Expect.equal Nothing dispatched.observations.pendingReturnNavigation
            , \_ -> Expect.equal dispatched.observations repeated.observations
            , \_ -> Expect.equal returned.observations.viewport.returnPin dispatched.observations.viewport.returnPin
            , \_ -> Expect.equal Nothing focused.observations.viewport.returnPin
            , \_ -> Expect.equal returned.observations.edit dispatched.observations.edit
            ] ()
    , test "clear activation tab session and query replacement retire pending Return without losing its draft" <| \_ ->
        let
            returned = pendingReturnModel
            cleared = { returned | observations = Feature.Observation.clearSelection returned.observations }
            activated = Feature.Observation.update (SelectObservationFrom "curated" "new-origin") returned |> Tuple.first
            queryState = returned.observations
            applied = Helpers.observationAppliedQuery queryState
            query = { returned | observations = Helpers.restoreObservationQuery { applied | query = "different applied query" } queryState }
            replacements = [ cleared, activated
                , Feature.Observation.refreshViewport returned ( { returned | activeTab = ProjectsTab }, Cmd.none ) |> Tuple.first
                , Feature.Observation.refreshViewport returned ( { returned | sessionRequestEpoch = returned.sessionRequestEpoch + 1 }, Cmd.none ) |> Tuple.first
                , Feature.Observation.refreshViewport returned ( query, Cmd.none ) |> Tuple.first ]
            replay candidate = Feature.Observation.updateViewport (returnReceipt returned.observations.viewport 800 "font16" Nothing) candidate |> Tuple.first
        in
        Expect.all
            [ \_ -> Expect.equal [ Nothing, Just "detail", Nothing, Nothing, Nothing ] (List.map (.observations >> .pendingReturnNavigation >> Maybe.map .intent) replacements)
            , \_ -> Expect.equal (List.map (.observations >> .pendingReturnNavigation) replacements) (List.map (replay >> .observations >> .pendingReturnNavigation) replacements)
            , \_ -> Expect.equal (List.repeat 5 returned.observations.edit) (List.map (.observations >> .edit) replacements)
            , \_ -> Expect.equal Nothing cleared.observations.viewport.origin
            ] ()
    , test "font resize keeps an authorized exact card while actual target loss uses fresh settled results fallback" <| \_ ->
        let
            returned = pendingReturnModel
            changedLayout = Feature.Observation.updateViewport (returnReceipt returned.observations.viewport 320 "font32" Nothing) returned |> Tuple.first
            settled = Feature.Observation.updateViewport (returnReceipt changedLayout.observations.viewport 320 "font32" Nothing) changedLayout |> Tuple.first
            state = returned.observations
            removed = { returned | observations = { state | orderedIds = [], items = Dict.empty } }
                |> (\model -> Feature.Observation.refreshViewport returned ( model, Cmd.none ) |> Tuple.first)
            returnedWidth = Feature.Observation.updateViewport (returnReceipt removed.observations.viewport 800 "font16" Nothing) removed |> Tuple.first
            fallback = Feature.Observation.updateViewport (returnReceipt returnedWidth.observations.viewport 800 "font16" Nothing) returnedWidth |> Tuple.first
        in
        Expect.all
            [ \_ -> Expect.equal (Just False) (Maybe.map .fallback changedLayout.observations.pendingReturnNavigation)
            , \_ -> Expect.equal Nothing settled.observations.pendingReturnNavigation
            , \_ -> Expect.equal (Just True) (Maybe.map .fallback removed.observations.pendingReturnNavigation)
            , \_ -> Expect.equal Nothing fallback.observations.pendingReturnNavigation
            , \_ -> Expect.equal returned.observations.edit fallback.observations.edit
            ] ()
    , test "same-ID narrow activation keeps its exact card when returned grid width cannot reuse the captured heights" <| \_ ->
        let
            returned = pendingReturnModel
            state = returned.observations
            viewport = state.viewport
            narrowOrigin = { returned | observations = { state | viewport = { viewport | origin = Maybe.map (\origin -> { origin | width = 400 }) viewport.origin } } }
            widened = Feature.Observation.updateViewport (returnReceipt narrowOrigin.observations.viewport 800 "font16" Nothing) narrowOrigin |> Tuple.first
            settled = Feature.Observation.updateViewport (returnReceipt widened.observations.viewport 800 "font16" Nothing) widened |> Tuple.first
        in
        Expect.all
            [ \_ -> Expect.equal (Just False) (Maybe.map .fallback widened.observations.pendingReturnNavigation)
            , \_ -> Expect.equal Nothing widened.observations.viewport.origin
            , \_ -> Expect.equal Nothing settled.observations.pendingReturnNavigation
            , \_ -> Expect.equal returned.observations.viewport.returnPin settled.observations.viewport.returnPin
            , \_ -> Expect.equal returned.observations.edit settled.observations.edit
            ] ()
    , test "newly mounted measurements cannot consume Return before a fresh unchanged-geometry receipt" <| \_ ->
        let
            returned = pendingReturnModel
            restored = Feature.Observation.updateViewport (returnReceipt returned.observations.viewport 800 "font16" Nothing) returned |> Tuple.first
            key = Feature.Observation.observationCardDomId "flat" "curated"
            measuring = Feature.Observation.updateViewport (measuredReturnReceipt restored.observations.viewport 800 "font16" Nothing [ ( key, 300 ) ]) restored |> Tuple.first
            settled = Feature.Observation.updateViewport (measuredReturnReceipt measuring.observations.viewport 800 "font16" Nothing [ ( key, 300 ) ]) measuring |> Tuple.first
        in
        Expect.all
            [ \_ -> Expect.equal True (measuring.observations.pendingReturnNavigation /= Nothing)
            , \_ -> Expect.equal Nothing settled.observations.pendingReturnNavigation
            , \_ -> Expect.equal measuring.observations.viewport.stamp settled.observations.viewport.stamp
            , \_ -> Expect.equal returned.observations.edit settled.observations.edit
            ] ()
    , test "detail entry waits for current heading mount and geometry while preceding navigation reads stay inert" <| \_ ->
        let
            before = pendingReturnModel
            activated = Feature.Observation.update (SelectObservationFrom "curated" (Feature.Observation.observationCardDomId "flat" "curated")) before
                |> Feature.Observation.refreshViewport before |> Tuple.first
            payload current headingMounted =
                measuredReturnReceipt current.observations.viewport 400 "font16" Nothing []
                    |> Decode.decodeValue (Decode.keyValuePairs Decode.value)
                    |> Result.withDefault []
                    |> List.filter (\( key, _ ) -> key /= "detailMounted")
                    |> (\fields -> Encode.object (( "detailMounted", Encode.bool headingMounted ) :: fields))
            measuring = Feature.Observation.updateViewport (payload activated False) activated |> Tuple.first
            mounted = Feature.Observation.updateViewport (payload measuring False) measuring |> Tuple.first
            stale = Feature.Observation.updateViewport (returnReceipt before.observations.viewport 800 "font16" (Just "@outside")) mounted |> Tuple.first
            settled = Feature.Observation.updateViewport (payload mounted True) mounted |> Tuple.first
            outside = Feature.Observation.updateViewport (returnReceipt mounted.observations.viewport 400 "font16" (Just "@outside")) mounted |> Tuple.first
            replayed = Feature.Observation.updateViewport (payload outside True) outside |> Tuple.first
        in
        Expect.all
            [ \_ -> Expect.equal (Just "detail") (Maybe.map .intent activated.observations.pendingReturnNavigation)
            , \_ -> Expect.equal True (mounted.observations.pendingReturnNavigation /= Nothing)
            , \_ -> Expect.equal mounted.observations.pendingReturnNavigation stale.observations.pendingReturnNavigation
            , \_ -> Expect.equal Nothing settled.observations.pendingReturnNavigation
            , \_ -> Expect.equal Nothing replayed.observations.pendingReturnNavigation
            , \_ -> Expect.equal before.observations.edit settled.observations.edit
            ] ()
    , test "passive selection cannot create intentional detail navigation ownership" <| \_ ->
        let before = pendingReturnModel |> (\model -> { model | observations = Feature.Observation.clearSelection model.observations })
            selected = Feature.Observation.selectObservation "curated" before |> Tuple.first
        in
        Expect.equal Nothing selected.observations.pendingReturnNavigation
    ]


observationProjectionTests : List Test
observationProjectionTests =
    [ test "flat and exact projections retain the complete ordered150 members independently of mounted windows" <| \_ ->
        let
            observations = List.range 0 149 |> List.map (\number -> fixtureObservation (String.fromInt number) "2026-01-01T00:00:00Z")
            ids = List.map .id observations
            initial = Feature.Observation.init
            loaded = { initial | items = observations |> List.map (\value -> ( value.id, value )) |> Dict.fromList, orderedIds = ids }
            cardIds state = Feature.Observation.projectResultRows state |> Array.toList |> List.filterMap (\row -> case row of
                ObservationCardRow _ value -> Just value.id
                _ -> Nothing)
        in
        Expect.equal [ ids, ids ] [ cardIds loaded, cardIds { loaded | requestMode = ObservationExactSubjectMode } ]
    , test "ordered match paths and overlapping subject groups preserve every canonical contextual duplicate through collapse" <| \_ ->
        let
            initial = Feature.Observation.init
            value = fixtureObservation "repeated" "2026-01-01T00:00:00Z"
            subjects = [ { subjectKind = Api.SubjectGlob, subject = "src/**/*.elm" }, { subjectKind = Api.SubjectFile, subject = "src/Shared.elm" } ]
            paths = [ "src/Main.elm", "src/View.elm" ]
            evidence = { observation = value, matchedPaths = paths, matchedSubjects = subjects, pathMatches = List.map (\path -> { path = path, matchedSubjects = subjects }) paths }
            loaded = { initial | requestMode = ObservationMatchMode, items = Dict.singleton value.id value, orderedIds = [ value.id ], matchAppliedPaths = paths, matchEvidence = Dict.singleton value.id evidence }
            groupKeys = List.concatMap (\path -> List.map (\subject -> Feature.Observation.matchGroupKey path subject.subjectKind subject.subject) subjects) paths
            expanded = { loaded | expandedMatchGroups = List.map (\key -> ( key, True )) groupKeys |> Dict.fromList }
            projection = Feature.Observation.projectResultRows expanded |> Array.toList
            contexts = projection |> List.filterMap (\row -> case row of
                ObservationCardRow context observation -> Just ( context, observation.id )
                _ -> Nothing)
            headings = projection |> List.filterMap (\row -> case row of
                ObservationPathRow path _ -> Just path
                _ -> Nothing)
            collapsed = Feature.Observation.projectResultRows loaded |> Array.toList
        in
        Expect.equal ( paths, List.map (\key -> ( key, value.id )) groupKeys, 6 ) ( headings, contexts, List.length collapsed )
    , test "draft input reuses projection and layout revision while canonical content changes retire origin geometry" <| \_ ->
        let
            value = fixtureObservation "selected-observation" "2026-01-01T00:00:00Z"
            initial = Feature.Observation.init
            loaded = { initial | items = Dict.singleton value.id value, orderedIds = [ value.id ], selectedId = Just value.id, selectedDetail = Just value }
            before = editableModel initial
            projected = Feature.Observation.refreshViewport before ( editableModel loaded, Cmd.none ) |> Tuple.first
            activeViewport = projected.observations.viewport
            withOrigin = { activeViewport | width = 800, layout = "font16" } |> ObservationViewport.captureOrigin
            state = projected.observations
            owned = { projected | observations = { state | viewport = withOrigin } }
            started = Feature.Observation.update StartObservationEdit owned |> Feature.Observation.refreshViewport owned |> Tuple.first
            drafted = Feature.Observation.update (SetObservationDraft "Protected keystroke") started |> Feature.Observation.refreshViewport started |> Tuple.first
            next = { value | content = "Changed canonical row height", updatedAt = "2026-01-02T00:00:00Z", contentVersion = "20000000-0000-4000-8000-000000000001" }
            canonical = { drafted | observations = Feature.Observation.applyCanonicalObservation next drafted.observations }
            refreshed = Feature.Observation.refreshViewport drafted ( canonical, Cmd.none ) |> Tuple.first
        in
        Expect.all
            [ \_ -> Expect.equal started.observations.resultRows drafted.observations.resultRows
            , \_ -> Expect.equal started.observations.viewport.stamp drafted.observations.viewport.stamp
            , \_ -> Expect.equal Nothing refreshed.observations.viewport.origin
            , \_ -> Expect.equal (Just "Protected keystroke") (Maybe.map .draft refreshed.observations.edit)
            ] ()
    ]


structuredCopyTests : List Test
structuredCopyTests =
    [ test "search titles equal to fallback text remain user prose while blank names copy canonical IDs" <| \_ ->
        let
            project name = { id = "abcd1234-full-project", workspaceId = "workspace-1", parentId = Nothing, name = name, description = Nothing, status = Api.ProjActive, priority = 5, createdAt = "", updatedAt = "" }
            results = Feature.Search.searchResultPresentations { projects = [ project "Project abcd1234", project "" ], tasks = [], observations = [] }
        in
        Expect.equal [ Nothing, Just "abcd1234-full-project" ] (List.map .titleCopyValue results)
    , test "audit dependency fragments copy each raw UUID and coincident user title stays plain" <| \_ ->
        let
            decode = Decode.decodeString Api.auditLogEntryDecoder
            relationship = """{"id":"audit-rel","workspace_id":"workspace-1","entity_type":"task_dependency","entity_id":"relation","action":"create","old_values":null,"new_values":{"task_id":"abcd1234-full-task","depends_on_id":"efgh5678-full-task"},"changed_at":"2026-01-01T00:00:00Z"}"""
            ordinary = """{"id":"audit-title","workspace_id":"workspace-1","entity_type":"task","entity_id":"abcd1234-full-task","action":"create","old_values":null,"new_values":{"title":"abcd1234"},"changed_at":"2026-01-01T00:00:00Z"}"""
        in
        case ( decode relationship, decode ordinary ) of
            ( Ok dependency, Ok title ) ->
                let
                    base = editableModel Feature.Observation.init
                    session = editorSession
                    admin = { session | workspace = Just { workspaceId = "workspace-1", role = Just "admin", canRead = True, canEdit = True, canAdmin = True } }
                    audit = Feature.AuditLog.init
                    filters = audit.filters
                    model = { base | sessionContext = Just admin, auditLog = { audit | entries = [ dependency, title ], filters = { filters | workspaceId = Just "workspace-1" } } }
                    view = Feature.AuditLog.viewWorkspaceAuditPanel "workspace-1" model |> Query.fromHtml
                    copy value = view |> Query.find [ Selector.attribute (attribute "aria-label" ("Copy task ID: " ++ value)) ] |> Event.simulate Event.click |> Event.expect (CopyId value)
                in Expect.all
                    [ \_ -> copy "abcd1234-full-task"
                    , \_ -> copy "efgh5678-full-task"
                    , \_ -> view |> Query.findAll [ Selector.class "audit-entity-summary" ] |> Query.index 1 |> Query.hasNot [ Selector.class "copyable-value" ]
                    ] ()
            _ -> Expect.fail "audit fixture did not decode"
    , test "audit known ordered subjects and version retain long canonical copy values with hidden snapshots excluded" <| \_ ->
        let
            second = String.repeat 200 "segment/" ++ "*.elm"
            version = "12345678-1234-4000-8000-123456789abc"
            values = Encode.object [ ( "content_version", Encode.string version ), ( "subjects", Encode.list identity [ Encode.object [ ( "subject_kind", Encode.string "file" ), ( "subject", Encode.string "src/Main.elm" ) ], Encode.object [ ( "subject_kind", Encode.string "glob" ), ( "subject", Encode.string second ) ] ] ), ( "client_secret", Encode.string "hidden" ) ]
            json = Encode.object [ ( "id", Encode.string "audit-obs" ), ( "workspace_id", Encode.string "workspace-1" ), ( "entity_type", Encode.string "observation" ), ( "entity_id", Encode.string "observation-1" ), ( "action", Encode.string "create" ), ( "old_values", Encode.null ), ( "new_values", values ), ( "changed_at", Encode.string "2026-01-01T00:00:00Z" ) ]
        in case Decode.decodeValue Api.auditLogEntryDecoder json of
            Err _ -> Expect.fail "observation audit fixture did not decode"
            Ok entry ->
                let
                    base = editableModel Feature.Observation.init
                    audit = Feature.AuditLog.init
                    model = { base | auditLog = { audit | entityHistory = Dict.singleton "observation-1" [ entry ], historyExpanded = Dict.singleton "observation-1" True } }
                    view = Feature.AuditLog.viewEntityHistory model "observation" "observation-1" |> Query.fromHtml
                in Expect.all
                    [ \_ -> view |> Query.find [ Selector.attribute (attribute "aria-label" ("Copy content version: " ++ version)) ] |> Event.simulate Event.click |> Event.expect (CopyId version)
                    , \_ -> view |> Query.find [ Selector.attribute (attribute "aria-label" ("Copy glob subject: " ++ second)) ] |> Event.simulate Event.click |> Event.expect (CopyId second)
                    , \_ -> view |> Query.hasNot [ Selector.text "hidden" ]
                    ] ()
    ]
