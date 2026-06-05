module SearchDisplayTest exposing (suite)

import Api
import Expect
import Feature.Search
import Test exposing (..)


suite : Test
suite =
    describe "unified search result presentation"
        [ test "mixed results expose counts, metadata, and actions" <|
            \_ ->
                let
                    results =
                        { memories = [ memory "memory-1" (Just "Memory summary") "Memory body" Api.LongTerm 9 True [ "elm", "search" ] ]
                        , projects =
                            [ { project = project "project-1" "Project Alpha" (Just "Project description") Api.ProjActive 7
                              , linkedMemories = [ linkedMemory "linked-1" (Just "Linked memory") [ "context" ] 8 ]
                              }
                            ]
                        , tasks =
                            [ { task = task "task-1" "Task Alpha" (Just "Task description") Api.InProgress 5 Nothing
                              , linkedMemories = []
                              }
                            ]
                        }

                    presentations =
                        Feature.Search.searchResultPresentations results
                in
                [ Feature.Search.unifiedSearchResultCount results == 3
                , List.map presentationBasics presentations
                    == [ { entityType = "project", entityTypeLabel = "PRJ", title = "Project Alpha", summary = "Project description", badges = [ "Active", "P7" ], actionLabel = "Focus project" }
                       , { entityType = "task", entityTypeLabel = "TSK", title = "Task Alpha", summary = "Task description", badges = [ "In Progress", "P5" ], actionLabel = "Focus task" }
                       , { entityType = "memory", entityTypeLabel = "MEM", title = "Memory summary", summary = "Memory body", badges = [ "Long term", "I9", "Pinned" ], actionLabel = "Open memory" }
                       ]
                , presentations
                    |> List.head
                    |> Maybe.map (.linkedMemories >> List.map .summary)
                    |> Maybe.withDefault []
                    |> (==) [ "Linked memory" ]
                ]
                    |> Expect.equal [ True, True, True ]
        , test "empty result sets and missing optional fields use safe fallbacks" <|
            \_ ->
                let
                    emptyResults =
                        { memories = [], projects = [], tasks = [] }

                    missingResults =
                        { memories = [ memory "memory-missing" Nothing "   " Api.ShortTerm 0 False [] ]
                        , projects =
                            [ { project = project "project-missing" "   " Nothing Api.ProjPaused 0
                              , linkedMemories = [ linkedMemory "linked-missing" Nothing [] 1 ]
                              }
                            ]
                        , tasks =
                            [ { task = task "task-missing" "" Nothing Api.Blocked 1 (Just "parent-task")
                              , linkedMemories = []
                              }
                            ]
                        }

                    missingPresentations =
                        Feature.Search.searchResultPresentations missingResults
                in
                [ Feature.Search.unifiedSearchResultCount emptyResults == 0
                , Feature.Search.searchResultPresentations emptyResults == []
                , List.map fallbackBasics missingPresentations
                    == [ { entityType = "project", entityTypeLabel = "PRJ", title = "Project project-", summary = "No description" }
                       , { entityType = "task", entityTypeLabel = "SUB", title = "Task task-mis", summary = "No description" }
                       , { entityType = "memory", entityTypeLabel = "MEM", title = "Memory memory-m", summary = "No memory content" }
                       ]
                , missingPresentations
                    |> List.head
                    |> Maybe.map (.linkedMemories >> List.map .summary)
                    |> Maybe.withDefault []
                    |> (==) [ "(no summary)" ]
                ]
                    |> Expect.equal [ True, True, True, True ]
        , test "tab navigation can clear stale unified search state without losing filters" <|
            \_ ->
                let
                    baseSearch =
                        Feature.Search.init

                    staleSearch =
                        { baseSearch
                            | query = "alpha"
                            , unifiedResults = Just { memories = [], projects = [], tasks = [] }
                            , isSearching = True
                            , searchError = Just "Search failed"
                            , activeRequestQuery = Just "alpha"
                            , filterMemoryTypes = [ "long_term" ]
                            , filterTags = [ "tag-a" ]
                        }

                    cleared =
                        Feature.Search.clearTransientSearchState staleSearch
                in
                { query = cleared.query
                , unifiedResults = cleared.unifiedResults
                , isSearching = cleared.isSearching
                , searchError = cleared.searchError
                , activeRequestQuery = cleared.activeRequestQuery
                , filterMemoryTypes = cleared.filterMemoryTypes
                , filterTags = cleared.filterTags
                }
                    |> Expect.equal
                        { query = "alpha"
                        , unifiedResults = Nothing
                        , isSearching = False
                        , searchError = Nothing
                        , activeRequestQuery = Nothing
                        , filterMemoryTypes = [ "long_term" ]
                        , filterTags = [ "tag-a" ]
                        }
        , test "long text and tags are preserved for wrapping by the UI" <|
            \_ ->
                let
                    longTitle =
                        String.repeat 12 "Long memory title segment "

                    longContent =
                        String.repeat 16 "Long memory content segment "

                    longTag =
                        String.repeat 10 "tag-segment-"

                    results =
                        { memories = [ memory "memory-long" (Just longTitle) longContent Api.LongTerm 10 False [ longTag ] ]
                        , projects = []
                        , tasks = []
                        }
                in
                Feature.Search.searchResultPresentations results
                    |> List.head
                    |> Maybe.map (\p -> ( p.title, p.summary, p.tags ))
                    |> Expect.equal (Just ( longTitle, longContent, [ longTag ] ))
        ]


type alias PresentationBasics =
    { entityType : String
    , entityTypeLabel : String
    , title : String
    , summary : String
    , badges : List String
    , actionLabel : String
    }


type alias FallbackBasics =
    { entityType : String
    , entityTypeLabel : String
    , title : String
    , summary : String
    }


presentationBasics : Feature.Search.SearchResultPresentation -> PresentationBasics
presentationBasics presentation =
    { entityType = presentation.entityType
    , entityTypeLabel = presentation.entityTypeLabel
    , title = presentation.title
    , summary = presentation.summary
    , badges = List.map .label presentation.badges
    , actionLabel = presentation.actionLabel
    }


fallbackBasics : Feature.Search.SearchResultPresentation -> FallbackBasics
fallbackBasics presentation =
    { entityType = presentation.entityType
    , entityTypeLabel = presentation.entityTypeLabel
    , title = presentation.title
    , summary = presentation.summary
    }


project : String -> String -> Maybe String -> Api.ProjectStatus -> Int -> Api.Project
project id name description status priority =
    { id = id
    , workspaceId = "workspace-a"
    , parentId = Nothing
    , name = name
    , description = description
    , status = status
    , priority = priority
    , createdAt = "2026-01-01T00:00:00Z"
    , updatedAt = "2026-01-02T00:00:00Z"
    }


task : String -> String -> Maybe String -> Api.TaskStatus -> Int -> Maybe String -> Api.Task
task id title description status priority parentId =
    { id = id
    , workspaceId = "workspace-a"
    , projectId = Just "project-1"
    , parentId = parentId
    , title = title
    , description = description
    , status = status
    , priority = priority
    , dueAt = Nothing
    , completedAt = Nothing
    , dependencyCount = 0
    , memoryLinkCount = 0
    , createdAt = "2026-01-01T00:00:00Z"
    , updatedAt = "2026-01-02T00:00:00Z"
    }


memory : String -> Maybe String -> String -> Api.MemoryType -> Int -> Bool -> List String -> Api.Memory
memory id summary content memoryType importance pinned tags =
    { id = id
    , workspaceId = "workspace-a"
    , content = content
    , summary = summary
    , memoryType = memoryType
    , importance = importance
    , pinned = pinned
    , tags = tags
    , createdAt = "2026-01-01T00:00:00Z"
    , updatedAt = "2026-01-02T00:00:00Z"
    }


linkedMemory : String -> Maybe String -> List String -> Int -> Api.LinkedMemorySummary
linkedMemory id summary tags importance =
    { id = id
    , summary = summary
    , tags = tags
    , importance = importance
    }
