module SearchDisplayTest exposing (suite)

import Api
import Expect
import Feature.Search
import Test exposing (..)


suite : Test
suite =
    describe "observation unified-search presentation"
        [ test "server-shaped observation, project, and task results are presented" <|
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
                       , { entityType = "observation", entityTypeLabel = "OBS", title = "src/Main.elm", summary = "Ranked FTS preview", actionLabel = "Open observation" }
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
                            | activeRequest = Just { workspaceId = "workspace-b", token = 2, query = "same query" }
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
    , dueAt = Nothing
    , completedAt = Nothing
    , dependencyCount = 0
    , createdAt = "2026-01-01T00:00:00Z"
    , updatedAt = "2026-01-01T00:00:00Z"
    , memoryLinkCount = 0
    }
