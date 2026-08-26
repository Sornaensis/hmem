module ObservationTest exposing (suite)

import Api
import AppShell
import Dict
import Expect
import Feature.DataLoading
import Feature.Observation
import Helpers
import Json.Decode as Decode
import Route
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Types exposing (Flags, Model, ObservationModel, Page(..), WorkspaceTab(..))
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
                        { workspaceId = "workspace-1", observationId = "selected-observation", token = 7 }

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
                        , Feature.Observation.detailResponseMatches "workspace-1" "selected-observation" 7 (Just "workspace-1") afterRoute.observations == False
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
                , afterRoute.observations.activeDetailRequest == Just { workspaceId = "workspace-1", observationId = "direct-observation", token = 1 }
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
                        { baseGlob | subjectKind = Api.SubjectGlob, subject = "src/**/*.elm" }

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
                    , \_ -> view |> Query.findAll [ Selector.class "observation-filter-input" ] |> Query.count (Expect.equal 3)
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
                        { workspaceId = "workspace-1", observationId = "A", token = 3 }

                    base =
                        Feature.Observation.init

                    state =
                        { base | selectedId = Just "A", activeDetailRequest = Just finalA }
                in
                [ Feature.Observation.detailResponseMatches "workspace-1" "A" 1 (Just "workspace-1") state
                , Feature.Observation.detailResponseMatches "workspace-1" "B" 2 (Just "workspace-1") state
                , Feature.Observation.detailResponseMatches "workspace-1" "A" 3 (Just "workspace-1") state
                , Feature.Observation.detailResponseMatches "workspace-1" "A" 3 (Just "workspace-2") state
                ]
                    |> Expect.equal [ False, False, True, False ]
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


fixtureObservation : String -> String -> Api.Observation
fixtureObservation id createdAt =
    { id = id
    , workspaceId = "workspace-1"
    , subjectKind = Api.SubjectFile
    , subject = "src/Main.elm"
    , gitSha = fullSha
    , content = "Observation"
    , createdAt = createdAt
    , updatedAt = createdAt
    }


paginatedFixture : String -> String
paginatedFixture hasMore =
    "{\"items\":[" ++ fileFixture ++ "," ++ globFixture ++ "],\"has_more\":" ++ hasMore ++ "}"


globFixture : String
globFixture =
    """{"id":"observation-glob","workspace_id":"workspace-1","subject_kind":"glob","subject":"src/**/*.elm","git_sha":"0123456789abcdef0123456789abcdef01234567","content":"Glob observation","created_at":"2026-01-01T00:00:00Z","updated_at":"2026-01-02T00:00:00Z"}"""
