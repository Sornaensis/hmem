module ObservationTest exposing (suite)

import Api
import AppShell
import Dict
import Expect
import Feature.DataLoading
import Feature.Observation
import Feature.WebSocket
import Helpers
import Html.Attributes exposing (attribute, tabindex)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Route
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Types exposing (AuthStatus(..), Flags, Model, Msg(..), ObservationModel, ObservationRequestMode(..), Page(..), WorkspaceTab(..))
import Url


suite : Test
suite =
    describe "observation API boundary"
        [ test "decodes server-shaped file and glob observations" <|
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
                    , \_ -> readOnly |> Query.hasNot [ Selector.text "Delete observation" ]
                    , \_ -> editor |> Query.has [ Selector.text "Edit content", Selector.text "Delete observation", Selector.text "Workspace ID", Selector.text "Subjects", Selector.text "Git SHA" ]
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
                                Feature.Observation.update (ObservationUpdated request (Err Http.Timeout)) saving
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
        , test "event-before-response cannot overwrite a newer canonical version or lose the draft" <|
            \_ ->
                let
                    saving =
                        savingEditModel "local draft"

                    original =
                        fixtureObservation "curated" "2026-01-01T00:00:00Z"

                    foreign =
                        { original | content = "foreign wins", updatedAt = "2026-01-03T00:00:00Z" }

                    afterEvent =
                        { saving | observations = Feature.Observation.applyCanonicalObservation foreign saving.observations }
                in
                case afterEvent.observations.edit |> Maybe.andThen .activeRequest of
                    Nothing ->
                        Expect.fail "save request should remain active through a canonical event"

                    Just request ->
                        let
                            staleResponse =
                                { original | content = "local draft", updatedAt = "2026-01-02T00:00:00Z" }

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
                    }
                    { detail = merged.selectedDetail |> Maybe.map .content
                    , listed = Dict.get latest.id merged.items |> Maybe.map .content
                    , draft = merged.edit |> Maybe.map .draft
                    , conflict = merged.edit |> Maybe.map .conflict
                    , latest = merged.edit |> Maybe.map (.latestCanonical >> .content)
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

                    dirty =
                        Feature.Observation.update StartObservationEdit (editableModel (selectedObservationState canonical))
                            |> Tuple.first
                            |> Feature.Observation.update (SetObservationDraft "local unsaved draft")
                            |> Tuple.first

                    afterSnapshot =
                        Feature.WebSocket.update (WsMessageReceived (observationSnapshotWire canonical)) dirty
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
                    , \_ -> generation |> Expect.equal 1
                    , \_ -> final.observations.selectedDetail |> Maybe.map .content |> Expect.equal (Just "foreign canonical content")
                    , \_ -> final.observations.edit |> Maybe.map (\edit -> ( edit.draft, edit.conflict, edit.latestCanonical.content )) |> Expect.equal (Just ( "local unsaved draft", True, "foreign canonical content" ))
                    , \_ -> finalView |> Query.find [ Selector.tag "button", Selector.containing [ Selector.text "Save content" ] ] |> Query.has [ Selector.disabled True ]
                    , \_ -> finalView |> Query.has [ Selector.text "Use latest version", Selector.text "Keep my draft" ]
                    ]
                    ()
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
                        { empty
                            | items = Dict.singleton "glob" glob
                            , orderedIds = [ "glob" ]
                            , hasMore = True
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
                    , \_ -> Feature.Observation.viewObservationsState repository loaded |> Query.fromHtml |> Query.has [ Selector.text "Glob", Selector.text "src/**/*.elm", Selector.text ("Git SHA: " ++ fullSha), Selector.text "Observation detail", Selector.text "Subject kind", Selector.text "File", Selector.text "Subject", Selector.text "src/Main.elm" ]
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
                    [ \_ -> view |> Query.find [ Selector.class "observation-filters" ] |> Query.has [ Selector.class "filter-bar" ]
                    , \_ -> view |> Query.findAll [ Selector.class "observation-filter-input" ] |> Query.count (Expect.equal 4)
                    , \_ -> view |> Query.find [ Selector.class "observation-filter-select" ] |> Query.has [ Selector.tag "select", Selector.class "filter-select" ]
                    , \_ -> view |> Query.find [ Selector.class "observation-filter-apply" ] |> Query.has [ Selector.tag "button", Selector.class "btn", Selector.class "btn-primary", Selector.text "Apply filters" ]
                    , \_ -> view |> Query.find [ Selector.id "entity-selected" ] |> Query.has [ Selector.tag "button", Selector.class "card", Selector.class "observation-card", Selector.class "observation-card-selected" ]
                    , \_ -> view |> Query.find [ Selector.class "observation-list-rows" ] |> Query.has [ Selector.class "observation-card" ]
                    , \_ -> view |> Query.find [ Selector.class "observation-detail-card" ] |> Query.has [ Selector.tag "article", Selector.class "card", Selector.class "observation-detail-content", Selector.class "observation-detail-meta", Selector.text fullSha ]
                    ]
                    ()
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
                        """{"id":"multi","workspace_id":"workspace-1","subjects":[{"subject_kind":"glob","subject":"src/**/*.elm"},{"subject_kind":"file","subject":"src/Main.elm"}],"git_sha":"0123456789abcdef0123456789abcdef01234567","content":"Evidence","created_at":"2026-01-01T00:00:00Z","updated_at":"2026-01-01T00:00:00Z"}"""

                    canonicalSubjects =
                        Decode.decodeString Api.observationDecoder canonical
                            |> Result.map (List.map .subject << .subjects)

                    legacySubjects =
                        Decode.decodeString Api.observationDecoder fileFixture
                            |> Result.map (List.map .subject << .subjects)
                in
                [ canonicalSubjects == Ok [ "src/**/*.elm", "src/Main.elm" ]
                , legacySubjects == Ok [ "src/Main.elm" ]
                , Decode.decodeString Api.observationDecoder "{\"id\":\"bad\",\"workspace_id\":\"workspace-1\",\"subjects\":[],\"git_sha\":\"x\",\"content\":\"x\",\"created_at\":\"x\",\"updated_at\":\"x\"}" |> isErr
                , Decode.decodeString Api.observationDecoder "{\"id\":\"bad\",\"workspace_id\":\"workspace-1\",\"subjects\":[{\"subject_kind\":\"other\",\"subject\":\"x\"}],\"git_sha\":\"x\",\"content\":\"x\",\"created_at\":\"x\",\"updated_at\":\"x\"}" |> isErr
                , Decode.decodeString Api.observationDecoder "{\"id\":\"bad\",\"workspace_id\":\"workspace-1\",\"git_sha\":\"x\",\"content\":\"x\",\"created_at\":\"x\",\"updated_at\":\"x\"}" |> isErr
                , Decode.decodeString Api.observationDecoder "{\"id\":\"bad\",\"workspace_id\":\"workspace-1\",\"subjects\":\"not-an-array\",\"git_sha\":\"x\",\"content\":\"x\",\"created_at\":\"x\",\"updated_at\":\"x\"}" |> isErr
                , Decode.decodeString Api.observationDecoder "{\"id\":\"bad\",\"workspace_id\":\"workspace-1\",\"subjects\":[],\"subject_kind\":\"file\",\"subject\":\"src/legacy.elm\",\"git_sha\":\"x\",\"content\":\"x\",\"created_at\":\"x\",\"updated_at\":\"x\"}" |> isErr
                , Decode.decodeString Api.observationDecoder "{\"id\":\"bad\",\"workspace_id\":\"workspace-1\",\"subjects\":\"not-an-array\",\"subject_kind\":\"file\",\"subject\":\"src/legacy.elm\",\"git_sha\":\"x\",\"content\":\"x\",\"created_at\":\"x\",\"updated_at\":\"x\"}" |> isErr
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
        , test "renders all subjects and match evidence without accepting stale list responses in match mode" <|
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
                            , requestGeneration = 4
                            , queryFingerprint = "match-request"
                            , expectedOffset = Just 0
                            , matchEvidence = Dict.singleton observation.id { observation = observation, matchedPaths = [ "src/Main.elm" ], matchedSubjects = List.drop 1 observation.subjects }
                        }

                    view =
                        Feature.Observation.viewObservationsState (observationWorkspace Api.Repository) state |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> view |> Query.has [ Selector.text "src/Main.elm", Selector.text "src/**/*.elm", Selector.text "Matched files: src/Main.elm", Selector.text "Matching subjects: src/**/*.elm" ]
                    , \_ -> view |> Query.find [ Selector.id "observation-match-paths" ] |> Query.has [ Selector.tag "textarea" ]
                    , \_ -> Feature.Observation.matchResponseMatches 4 "match-request" 0 state |> Expect.equal True
                    , \_ -> Feature.DataLoading.observationResponseMatches 4 "match-request" 0 state |> Expect.equal True
                    ]
                    ()
        , test "decodes paginated V020 singleton and V021 multi-subject match evidence" <|
            \_ ->
                let
                    response =
                        """{"items":[{"observation":{"id":"v020","workspace_id":"workspace-1","subject_kind":"file","subject":"src/Legacy.elm","git_sha":"0123456789abcdef0123456789abcdef01234567","content":"Legacy","created_at":"2026-01-01T00:00:00Z","updated_at":"2026-01-01T00:00:00Z"},"matched_paths":["src/Legacy.elm"],"matched_subjects":[{"subject_kind":"file","subject":"src/Legacy.elm"}]},{"observation":{"id":"v021","workspace_id":"workspace-1","subjects":[{"subject_kind":"file","subject":"src/Main.elm"},{"subject_kind":"glob","subject":"src/**/*.elm"}],"git_sha":"0123456789abcdef0123456789abcdef01234567","content":"Canonical","created_at":"2026-01-01T00:00:00Z","updated_at":"2026-01-01T00:00:00Z"},"matched_paths":["src/Main.elm"],"matched_subjects":[{"subject_kind":"glob","subject":"src/**/*.elm"}]}],"has_more":true}"""

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
                        { initial | requestMode = ObservationMatchMode, matchPathsInput = "src/Main.elm" }

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
                            , matchEvidence = Dict.singleton observation.id { observation = observation, matchedPaths = [ "src/Main.elm" ], matchedSubjects = List.drop 1 observation.subjects }
                        }

                    repository =
                        observationWorkspace Api.Repository
                in
                Expect.all
                    [ \_ -> Feature.Observation.viewObservationsState repository { base | loading = True } |> Query.fromHtml |> Query.has [ Selector.text "Matching repository files..." ]
                    , \_ -> Feature.Observation.viewObservationsState repository base |> Query.fromHtml |> Query.has [ Selector.text "No matching observations", Selector.text "Try different concrete repository paths or clear the match." ]
                    , \_ -> Feature.Observation.viewObservationsState repository { base | error = Just "Failed to match repository files." } |> Query.fromHtml |> Query.has [ Selector.text "Failed to match repository files." ]
                    , \_ -> Feature.Observation.viewObservationsState repository loaded |> Query.fromHtml |> Query.find [ Selector.id "observation-subject" ] |> Query.has [ Selector.disabled True ]
                    , \_ -> Feature.Observation.viewObservationsState repository loaded |> Query.fromHtml |> Query.has [ Selector.text "+1", Selector.class "observation-subject-copy", Selector.text "Matched files: src/Main.elm" ]
                    , \_ -> Feature.Observation.viewObservationsState repository loaded |> Query.fromHtml |> Query.findAll [ Selector.class "observation-subject-copy", Selector.tag "button" ] |> Query.count (Expect.equal 2)
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
                            (GotObservationMatches "other-workspace" 9 "match-fingerprint" 0 (Ok { items = [], hasMore = False }))
                            model
                            |> Tuple.first

                    wrongMatchResult =
                        Feature.Observation.update
                            (GotObservationMatches workspace.id 8 "match-fingerprint" 0 (Ok { items = [], hasMore = False }))
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
        ]


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
    """{"id":"observation-file","workspace_id":"workspace-1","subject_kind":"file","subject":"src/Main.elm","git_sha":"0123456789abcdef0123456789abcdef01234567","content":"File observation","created_at":"2026-01-01T00:00:00Z","updated_at":"2026-01-02T00:00:00Z"}"""


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
    , createdAt = createdAt
    , updatedAt = createdAt
    }


observationSnapshotWire : Api.Observation -> String
observationSnapshotWire observation =
    Encode.object
        [ ( "schema_version", Encode.int 1 )
        , ( "transport", Encode.string "snapshot" )
        , ( "scope", workspaceScopeValue )
        , ( "items"
          , Encode.list identity
                [ snapshotItem "workspace"
                    (Encode.object
                        [ ( "id", Encode.string "workspace-1" )
                        , ( "name", Encode.string "Workspace" )
                        , ( "workspace_type", Encode.string "repository" )
                        , ( "created_at", Encode.string "2026-01-01T00:00:00Z" )
                        , ( "updated_at", Encode.string "2026-01-01T00:00:00Z" )
                        ]
                    )
                , snapshotItem "observation" (observationValue observation)
                ]
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
        , ( "created_at", Encode.string observation.createdAt )
        , ( "updated_at", Encode.string observation.updatedAt )
        ]


paginatedFixture : String -> String
paginatedFixture hasMore =
    "{\"items\":[" ++ fileFixture ++ "," ++ globFixture ++ "],\"has_more\":" ++ hasMore ++ "}"


globFixture : String
globFixture =
    """{"id":"observation-glob","workspace_id":"workspace-1","subject_kind":"glob","subject":"src/**/*.elm","git_sha":"0123456789abcdef0123456789abcdef01234567","content":"Glob observation","created_at":"2026-01-01T00:00:00Z","updated_at":"2026-01-02T00:00:00Z"}"""


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
