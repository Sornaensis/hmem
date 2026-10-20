module SearchDisplayTest exposing (suite)

import Api
import AppShell
import Dict
import Helpers
import Http
import Json.Encode as Encode
import Types exposing (..)
import Url
import Expect
import Feature.Search
import Test exposing (..)


suite : Test
suite =
    describe "observation unified-search presentation"
        [ liveSuite
        , test "server-shaped observation, project, and task results are presented" <|
            \_ ->
                let
                    results =
                        { observations = [ observation "observation-1" "src/Main.elm" "Ranked FTS preview" ]
                        , projects = [ project "project-1" "Project Alpha" ]
                        , tasks = [ task "task-1" "Task Alpha" ]
                        }

                    presentations =
                        Feature.Search.searchResultPresentations results
                in
                [ Feature.Search.unifiedSearchResultCount results == 3
                , List.map (\item -> { entityType = item.entityType, entityTypeLabel = item.entityTypeLabel, title = item.title, summary = item.summary, actionLabel = item.actionLabel }) presentations
                    == [ { entityType = "project", entityTypeLabel = "PRJ", title = "Project Alpha", summary = "Project description", actionLabel = "Focus project" }
                       , { entityType = "task", entityTypeLabel = "TSK", title = "Task Alpha", summary = "Task description", actionLabel = "Focus task" }
                       , { entityType = "observation", entityTypeLabel = "OBS", title = "src/Main.elm", summary = "Current binding unknown · Ranked FTS preview", actionLabel = "Open observation" }
                       ]
                ]
                    |> Expect.equal [ True, True ]
        , test "unified search entity types only include observations for repositories" <|
            \_ ->
                [ Feature.Search.unifiedSearchEntityTypes Api.Repository
                , Feature.Search.unifiedSearchEntityTypes Api.Planning
                , Feature.Search.unifiedSearchEntityTypes Api.Personal
                ]
                    |> Expect.equal
                        [ [ "project", "task", "observation" ]
                        , [ "project", "task" ]
                        , [ "project", "task" ]
                        ]
        , test "same-query and cross-workspace stale responses require the active request token" <|
            \_ ->
                let
                    base =
                        Feature.Search.init

                    pending =
                        { base
                            | activeRequest = Just { workspaceId = "workspace-b", sessionEpoch = 0, token = 2, generation = 0, actor = "", entityTypes = [ "project", "task" ], offset = 0, query = "same query" }
                            , activeRequestQuery = Just "same query"
                            , nextRequestToken = 3
                        }
                in
                [ Feature.Search.unifiedSearchResponseMatches "workspace-a" 1 "same query" (Just "workspace-a") pending
                , Feature.Search.unifiedSearchResponseMatches "workspace-b" 1 "same query" (Just "workspace-b") pending
                , Feature.Search.unifiedSearchResponseMatches "workspace-b" 2 "same query" (Just "workspace-a") pending
                , Feature.Search.unifiedSearchResponseMatches "workspace-b" 2 "same query" (Just "workspace-b") pending
                ]
                    |> Expect.equal [ False, False, False, True ]
        , test "empty server-shaped result sets and transient clearing are safe" <|
            \_ ->
                let
                    emptySearchResults : Api.UnifiedSearchResults
                    emptySearchResults =
                        { observations = [], projects = [], tasks = [] }

                    baseSearch =
                        Feature.Search.init

                    staleSearch =
                        { baseSearch
                            | query = "alpha"
                            , unifiedResults = Just emptySearchResults
                            , isSearching = True
                            , searchError = Just "Search failed"
                            , activeRequestQuery = Just "alpha"
                        }

                    cleared =
                        Feature.Search.clearTransientSearchState staleSearch
                in
                [ Feature.Search.unifiedSearchResultCount emptySearchResults == 0
                , Feature.Search.searchResultPresentations emptySearchResults == []
                , { query = cleared.query, unifiedResults = cleared.unifiedResults, isSearching = cleared.isSearching, searchError = cleared.searchError, activeRequestQuery = cleared.activeRequestQuery }
                    == { query = "alpha", unifiedResults = Nothing, isSearching = False, searchError = Nothing, activeRequestQuery = Nothing }
                ]
                    |> Expect.equal [ True, True, True ]
        ]


observation : String -> String -> String -> Api.ObservationSearchHit
observation id subject contentPreview =
    { id = id
    , workspaceId = "workspace-1"
    , subjectKind = Api.SubjectFile
    , subject = subject
    , gitSha = "0123456789abcdef0123456789abcdef01234567"
    , contentPreview = contentPreview
    , contentVersion = "10000000-0000-4000-8000-000000000000"
    , latestSequence = 1
    , currentProvenance = Nothing
    , provenanceMatch = Nothing
    , updatedAt = "2026-01-01T00:00:00Z"
    }


project : String -> String -> Api.Project
project id name =
    { id = id
    , workspaceId = "workspace-1"
    , parentId = Nothing
    , name = name
    , description = Just "Project description"
    , status = Api.ProjActive
    , priority = 7
    , createdAt = "2026-01-01T00:00:00Z"
    , updatedAt = "2026-01-01T00:00:00Z"
    }


task : String -> String -> Api.Task
task id title =
    { id = id
    , workspaceId = "workspace-1"
    , projectId = Nothing
    , parentId = Nothing
    , title = title
    , description = Just "Task description"
    , status = Api.InProgress
    , priority = 5
    , completedAt = Nothing
    , dependencyCount = 0
    , createdAt = "2026-01-01T00:00:00Z"
    , updatedAt = "2026-01-01T00:00:00Z"
    , memoryLinkCount = 0
    }


liveSuite : Test
liveSuite =
    describe "isolated live entity search"
        [ test "typing schedules a guarded continuation without changing hierarchy state" <| \_ ->
            let
                drafted = step (GlobalSearchInput "alpha") liveModel
                request = Feature.Search.requestIdentity drafted
                dispatched = step (GlobalSearchReady request) drafted
            in
            Expect.equal
                { hierarchy = "hierarchy", earlyRequest = False, activated = True, request = Just request, navigation = liveModel.dataLoading.navigationGeneration }
                { hierarchy = drafted.search.query, earlyRequest = drafted.search.activeRequest /= Nothing, activated = drafted.search.globalActivated, request = dispatched.search.activeRequest, navigation = drafted.dataLoading.navigationGeneration }
        , test "IME and clear retire old debounce and request owners" <| \_ ->
            let
                composing = liveModel |> step GlobalSearchCompositionStart |> step (GlobalSearchInput "intermediate")
                ignored = step (GlobalSearchReady (Feature.Search.requestIdentity composing)) composing
                ended = step (GlobalSearchCompositionEnd "complete") ignored
                request = Feature.Search.requestIdentity ended
                active = step (GlobalSearchReady request) ended
                cleared = step (GlobalSearchInput "") active
                late = step (GlobalSearchReady request) cleared |> reply active (Ok emptyResults)
            in
            Expect.equal
                { composing = Nothing, committed = Just "complete", query = "", activated = True, request = Nothing, results = Nothing, hierarchy = "hierarchy" }
                { composing = ignored.search.activeRequest, committed = active.search.activeRequest |> Maybe.map .query, query = late.search.globalQuery, activated = late.search.globalActivated, request = late.search.activeRequest, results = late.search.unifiedResults, hierarchy = late.search.query }
        , test "type toggles are immediate and explicit none never dispatches or live-refreshes" <| \_ ->
            let
                active = liveModel |> step (GlobalSearchInput "alpha") |> step SubmitSearch
                projectsOnly = active |> step (ToggleSearchEntityType "task") |> step (ToggleSearchEntityType "observation")
                none = step (ToggleSearchEntityType "project") projectsOnly
                late = reply active (Ok emptyResults) none |> Feature.Search.refreshAcceptedSearch |> Tuple.first
                reopened = step (ToggleSearchEntityType "task") late
            in
            Expect.equal
                { immediateTypes = Just [ "project" ], noTypes = [], request = Nothing, searching = False, reopenedTypes = Just [ "task" ], activated = True }
                { immediateTypes = projectsOnly.search.activeRequest |> Maybe.map .entityTypes, noTypes = Feature.Search.selectedEntityTypes late, request = late.search.activeRequest, searching = late.search.isSearching, reopenedTypes = reopened.search.activeRequest |> Maybe.map .entityTypes, activated = late.search.globalActivated }
        , test "request ownership includes types, draft generation, actor, workspace and session" <| \_ ->
            let
                active = liveModel |> step (GlobalSearchInput "same") |> step SubmitSearch
                changed = step (ToggleSearchEntityType "task") active
                newer = step (GlobalSearchInput "same") changed
                epoch = { active | sessionRequestEpoch = active.sessionRequestEpoch + 1 }
                otherWorkspace = { active | selectedWorkspaceId = Just "elsewhere" }
                actor = { active | sessionContext = Just (session "other") }
                denied = { active | auth = { status = AuthRequired, mode = Nothing } }
            in
            [ changed, newer, epoch, otherWorkspace, actor, denied ]
                |> List.map (\current -> (reply active (Ok emptyResults) current).search == current.search)
                |> Expect.equal (List.repeat 6 True)
        , test "last complete results survive pending, errors, fresh retry and coalesced live refresh" <| \_ ->
            let
                first = liveModel |> step (GlobalSearchInput "first") |> step SubmitSearch
                complete = reply first (Ok { emptyResults | projects = [ project "p" "First" ] }) first
                drafted = step (GlobalSearchInput "second") complete
                second = step (GlobalSearchReady (Feature.Search.requestIdentity drafted)) drafted
                failed = reply second (Err Http.NetworkError) second
                retry = step SubmitSearch failed
                queued = Feature.Search.refreshAcceptedSearch retry |> Tuple.first
                refreshed = reply retry (Ok emptyResults) queued
            in
            Expect.equal
                { preserved = True, completedQuery = Just "first", errorPreserved = True, retryFresh = True, refreshFresh = True, refreshQuery = Just "second" }
                { preserved = drafted.search.unifiedResults == complete.search.unifiedResults, completedQuery = drafted.search.completedRequest |> Maybe.map .query
                , errorPreserved = failed.search.unifiedResults == complete.search.unifiedResults && failed.search.searchError /= Nothing
                , retryFresh = Maybe.map .token retry.search.activeRequest /= Maybe.map .token second.search.activeRequest
                , refreshFresh = Maybe.map .token refreshed.search.activeRequest /= Maybe.map .token retry.search.activeRequest
                , refreshQuery = refreshed.search.activeRequest |> Maybe.map .query }
        , test "supported defaults and guarded actor/workspace preferences remain separate from hierarchy filters" <| \_ ->
            let
                preference types = Encode.object [ ( "entitySearchPreference", Encode.bool True ), ( "workspaceId", Encode.string "workspace-1" ), ( "actor", Encode.string (Helpers.globalSearchActor liveModel) ), ( "types", Encode.list Encode.string types ) ]
                loaded = Helpers.applyStoredFiltersIfCurrentWorkspace (preference [ "task" ]) liveModel
                foreignActor = Helpers.applyStoredFiltersIfCurrentWorkspace (preference [ "task" ]) { liveModel | sessionContext = Just (session "other") }
                foreignWorkspace = Helpers.applyStoredFiltersIfCurrentWorkspace (preference [ "task" ]) { liveModel | selectedWorkspaceId = Just "other-workspace" }
                planningWorkspace = { workspace | workspaceType = Api.Planning }
                planning = { liveModel | workspaces = Dict.singleton workspace.id planningWorkspace }
                edited = step (GlobalSearchInput "typed") loaded
                late = Helpers.applyStoredFiltersIfCurrentWorkspace (preference []) edited
            in
            Expect.equal
                { defaults = [ "project", "task", "observation" ], loaded = [ "task" ], otherActor = [ "project", "task", "observation" ], otherWorkspace = Nothing, supported = [ "project", "task" ], lateHydration = [ "task" ], hierarchy = "hierarchy" }
                { defaults = Feature.Search.selectedEntityTypes liveModel, loaded = Feature.Search.selectedEntityTypes loaded, otherActor = Feature.Search.selectedEntityTypes foreignActor
                , otherWorkspace = foreignWorkspace.search.entityTypes, supported = Feature.Search.selectedEntityTypes planning, lateHydration = Feature.Search.selectedEntityTypes late, hierarchy = loaded.search.query }
        ]


step : Msg -> Model -> Model
step message source =
    Feature.Search.update message source |> Tuple.first


reply : Model -> Result Http.Error Api.UnifiedSearchResults -> Model -> Model
reply owner result current =
    case owner.search.activeRequest of
        Just request -> step (GotUnifiedSearchResults request.workspaceId request.sessionEpoch request.token request.query result) current
        Nothing -> current


emptyResults : Api.UnifiedSearchResults
emptyResults =
    { projects = [], tasks = [], observations = [] }


workspace : Api.Workspace
workspace =
    { id = "workspace-1", name = "Workspace", workspaceType = Api.Repository, ghOwner = Nothing, ghRepo = Nothing, createdAt = "now", updatedAt = "now" }


session : String -> Api.SessionContext
session actor =
    { authMode = "test", effectiveActor = Nothing
    , principal = { actorType = "user", actorId = actor, actorLabel = actor, authority = "local", grantUserId = Nothing }
    , globalPermissions = { createWorkspace = False, superadmin = False }
    , workspace = Just { workspaceId = "workspace-1", role = Just "edit", canRead = True, canEdit = True, canAdmin = False } }


liveModel : Model
liveModel =
    let
        flags = { apiUrl = "https://api.example", wsUrl = "wss://api.example", sessionId = "session-1", runtimeMode = "test", authTokenStorageKey = "hmem-auth-token", authTokenPresent = False, loginUrl = Nothing, logoutUrl = Nothing }
        url = { protocol = Url.Https, host = "app.example", port_ = Nothing, path = "/workspace/workspace-1", query = Nothing, fragment = Nothing }
        initial = AppShell.initModel Nothing url (WorkspacePage "workspace-1") flags Nothing { tab = ProjectsTab, focus = Nothing, observationId = Nothing } |> AppShell.finalizeInit (WorkspacePage "workspace-1")
        search = initial.search
    in
    { initial | auth = { status = AuthReady, mode = Just "test" }, sessionContext = Just (session "editor"), workspaces = Dict.singleton workspace.id workspace, search = { search | query = "hierarchy" } }
