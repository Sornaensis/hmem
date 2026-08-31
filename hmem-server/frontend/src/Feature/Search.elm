module Feature.Search exposing
    ( LinkedMemoryPresentation
    , SearchResultBadge
    , SearchResultPresentation
    , clearTransientSearchState
    , init
    , searchResultPresentations
    , unifiedSearchEntityTypes
    , unifiedSearchResponseMatches
    , unifiedSearchResultCount
    , update
    , viewSearchBar
    , viewUnifiedSearchError
    , viewUnifiedSearchLoading
    , viewUnifiedSearchResults
    )

import Api
import Feature.DataLoading
import Feature.Observation
import Dict
import Helpers exposing (replaceFragment, saveFiltersCmd, scrollToElement, taskStatusBadgeClass, taskStatusDisplayText, taskStatusTitle)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types exposing (..)


init : SearchModel
init =
    { query = ""
    , unifiedResults = Nothing
    , isSearching = False
    , searchError = Nothing
    , activeRequestQuery = Nothing
    , activeRequest = Nothing
    , nextRequestToken = 1
    , filterShowOnly = ShowAll
    , filterPriority = AnyPriority
    , filterProjectStatuses = []
    , filterTaskStatuses = []
    , filterMemoryTypes = []
    , filterImportance = AnyPriority
    , filterMemoryPinned = Nothing
    , filterMemoryActiveLinked = False
    , filterTags = []
    }


type alias SearchResultBadge =
    { label : String
    , className : String
    , title : String
    }


type alias LinkedMemoryPresentation =
    { summary : String
    , importance : Int
    , tags : List String
    }


type alias SearchResultPresentation =
    { entityType : String
    , entityTypeLabel : String
    , entityTypeClass : String
    , entityId : String
    , title : String
    , summary : String
    , badges : List SearchResultBadge
    , tags : List String
    , linkedMemories : List LinkedMemoryPresentation
    , actionLabel : String
    }


reloadNavigation : Model -> ( Model, Cmd Msg )
reloadNavigation model =
    let
        ( updated, navigationCmd ) =
            Feature.DataLoading.reloadNavigationForFilters model
    in
    ( updated, Cmd.batch [ saveFiltersCmd updated, navigationCmd ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchInput query ->
            let
                searchModel =
                    model.search

                updatedSearch =
                    { searchModel
                        | query = query
                        , unifiedResults = Nothing
                        , isSearching = False
                        , searchError = Nothing
                        , activeRequestQuery = Nothing
                        , activeRequest = Nothing
                    }

                newModel =
                    { model | search = updatedSearch }
            in
            reloadNavigation newModel

        SubmitSearch ->
            let
                trimmed =
                    String.trim model.search.query

                searchModel =
                    model.search
            in
            if String.isEmpty trimmed then
                ( { model | search = clearUnifiedSearchState searchModel }, Cmd.none )

            else
                case model.selectedWorkspaceId of
                    Nothing ->
                        ( { model
                            | search =
                                { searchModel
                                    | unifiedResults = Nothing
                                    , isSearching = False
                                    , searchError = Just "Select a workspace before searching."
                                    , activeRequestQuery = Nothing
                                    , activeRequest = Nothing
                                }
                          }
                        , Cmd.none
                        )

                    Just workspaceId ->
                        let
                            entityTypes =
                                workspaceEntityTypes workspaceId model

                            token =
                                searchModel.nextRequestToken

                            request =
                                { workspaceId = workspaceId, token = token, query = trimmed }
                        in
                        ( { model
                            | search =
                                { searchModel
                                    | unifiedResults = Nothing
                                    , isSearching = True
                                    , searchError = Nothing
                                    , activeRequestQuery = Just trimmed
                                    , activeRequest = Just request
                                    , nextRequestToken = token + 1
                                }
                          }
                        , Api.unifiedSearch model.flags.apiUrl trimmed workspaceId entityTypes (GotUnifiedSearchResults workspaceId token trimmed)
                        )

        GotUnifiedSearchResults workspaceId token requestedQuery result ->
            let
                searchModel =
                    model.search

                currentQuery =
                    String.trim searchModel.query
            in
            if not (unifiedSearchResponseMatches workspaceId token requestedQuery model.selectedWorkspaceId searchModel) || currentQuery /= requestedQuery then
                ( model, Cmd.none )

            else
                case result of
                    Ok results ->
                        ( { model
                            | search =
                                { searchModel
                                    | unifiedResults = Just results
                                    , isSearching = False
                                    , searchError = Nothing
                                    , activeRequestQuery = Nothing
                                    , activeRequest = Nothing
                                }
                          }
                        , Cmd.none
                        )

                    Err _ ->
                        ( { model
                            | search =
                                { searchModel
                                    | unifiedResults = Nothing
                                    , isSearching = False
                                    , searchError = Just "Search failed. Please try again."
                                    , activeRequestQuery = Nothing
                                    , activeRequest = Nothing
                                }
                          }
                        , Cmd.none
                        )

        NavigateToSearchResult entityType entityId ->
            navigateToSearchResult entityType entityId model

        SetFilterShowOnly show ->
            let
                searchModel =
                    model.search

                newModel =
                    { model | search = { searchModel | filterShowOnly = show } }
            in
            reloadNavigation newModel

        SetFilterPriority pri ->
            let
                searchModel =
                    model.search

                newModel =
                    { model | search = { searchModel | filterPriority = pri } }
            in
            reloadNavigation newModel

        ToggleFilterProjectStatus status ->
            let
                searchModel =
                    model.search

                newStatuses =
                    if List.member status searchModel.filterProjectStatuses then
                        List.filter (\s -> s /= status) searchModel.filterProjectStatuses

                    else
                        status :: searchModel.filterProjectStatuses

                newModel =
                    { model | search = { searchModel | filterProjectStatuses = newStatuses } }
            in
            reloadNavigation newModel

        ToggleFilterTaskStatus status ->
            let
                searchModel =
                    model.search

                newStatuses =
                    if List.member status searchModel.filterTaskStatuses then
                        List.filter (\s -> s /= status) searchModel.filterTaskStatuses

                    else
                        status :: searchModel.filterTaskStatuses

                newModel =
                    { model | search = { searchModel | filterTaskStatuses = newStatuses } }
            in
            reloadNavigation newModel

        ToggleFilterMemoryType mtype ->
            let
                searchModel =
                    model.search

                newTypes =
                    if List.member mtype searchModel.filterMemoryTypes then
                        List.filter (\s -> s /= mtype) searchModel.filterMemoryTypes

                    else
                        mtype :: searchModel.filterMemoryTypes

                newModel =
                    { model | search = { searchModel | filterMemoryTypes = newTypes } }
            in
            ( newModel, saveFiltersCmd newModel )

        SetFilterImportance imp ->
            let
                searchModel =
                    model.search

                newModel =
                    { model | search = { searchModel | filterImportance = imp } }
            in
            ( newModel, saveFiltersCmd newModel )

        SetFilterMemoryPinned pinned ->
            let
                searchModel =
                    model.search

                newModel =
                    { model | search = { searchModel | filterMemoryPinned = pinned } }
            in
            ( newModel, saveFiltersCmd newModel )

        ToggleFilterMemoryActiveLinked active ->
            let
                searchModel =
                    model.search

                newModel =
                    { model | search = { searchModel | filterMemoryActiveLinked = active } }
            in
            ( newModel, saveFiltersCmd newModel )

        ToggleFilterTag tag ->
            let
                searchModel =
                    model.search

                newTags =
                    if List.member tag searchModel.filterTags then
                        List.filter (\t -> t /= tag) searchModel.filterTags

                    else
                        tag :: searchModel.filterTags

                newModel =
                    { model | search = { searchModel | filterTags = newTags } }
            in
            ( newModel, saveFiltersCmd newModel )

        _ ->
            ( model, Cmd.none )


unifiedSearchResponseMatches : String -> Int -> String -> Maybe String -> SearchModel -> Bool
unifiedSearchResponseMatches workspaceId token query selectedWorkspaceId searchModel =
    selectedWorkspaceId == Just workspaceId
        && searchModel.activeRequest == Just { workspaceId = workspaceId, token = token, query = query }


workspaceEntityTypes : String -> Model -> List String
workspaceEntityTypes workspaceId model =
    Dict.get workspaceId model.workspaces
        |> Maybe.map .workspaceType
        |> Maybe.map unifiedSearchEntityTypes
        |> Maybe.withDefault [ "project", "task" ]


unifiedSearchEntityTypes : Api.WorkspaceType -> List String
unifiedSearchEntityTypes workspaceType =
    if workspaceType == Api.Repository then
        [ "project", "task", "observation" ]

    else
        [ "project", "task" ]


clearTransientSearchState : SearchModel -> SearchModel
clearTransientSearchState searchModel =
    { searchModel
        | unifiedResults = Nothing
        , isSearching = False
        , searchError = Nothing
        , activeRequestQuery = Nothing
        , activeRequest = Nothing
    }


clearUnifiedSearchState : SearchModel -> SearchModel
clearUnifiedSearchState =
    clearTransientSearchState


navigateToSearchResult : String -> String -> Model -> ( Model, Cmd Msg )
navigateToSearchResult entityType entityId model =
    let
        baseClearedSearch =
            clearUnifiedSearchState model.search

        clearedSearch =
            if entityType == "observation" then
                { baseClearedSearch
                    | query = ""
                    , filterMemoryTypes = []
                    , filterImportance = AnyPriority
                    , filterMemoryPinned = Nothing
                    , filterMemoryActiveLinked = False
                    , filterTags = []
                }

            else
                baseClearedSearch

        isFocusableEntity =
            entityType == "project" || entityType == "task"

        focusEntry =
            ( entityType, entityId )

        currentFocus =
            model.focus

        focusedHistory =
            List.take (currentFocus.historyIndex + 1) currentFocus.history ++ [ focusEntry ]

        updatedFocus =
            if isFocusableEntity then
                { currentFocus
                    | focusedEntity = Just focusEntry
                    , breadcrumbAnchor = Just focusEntry
                    , history = focusedHistory
                    , historyIndex = List.length focusedHistory - 1
                    , returnContext = Nothing
                }

            else
                { currentFocus
                    | focusedEntity = Nothing
                    , breadcrumbAnchor = Nothing
                    , history = []
                    , historyIndex = 0
                    , returnContext = Nothing
                }

        targetTab =
            if entityType == "observation" then
                ObservationsTab

            else
                ProjectsTab

        nextModel =
            { model
                | activeTab = targetTab
                , search = clearedSearch
                , focus = updatedFocus
            }
    in
    if entityType == "observation" then
        let
            ( detailModel, detailCmd ) =
                Feature.Observation.selectObservation entityId nextModel
        in
        ( detailModel
        , Cmd.batch
            [ replaceFragment detailModel
            , detailCmd
            , scrollToElement "observation-detail"
            ]
        )

    else
        ( nextModel
        , Cmd.batch
            [ replaceFragment nextModel
            , scrollToElement ("entity-" ++ entityId)
            ]
        )


viewSearchBar : Model -> Html Msg
viewSearchBar model =
    div [ class "search-filter-bar" ]
        [ Html.form [ class "search-bar", onSubmit SubmitSearch ]
            [ input
                [ class "search-input"
                , type_ "text"
                , placeholder "Search all entities... (Enter to search)"
                , value model.search.query
                , onInput SearchInput
                ]
                []
            , if model.search.isSearching then
                span [ class "search-spinner" ] [ text "…" ]

              else if not (String.isEmpty model.search.query) then
                button [ class "search-clear", onClick (SearchInput ""), type_ "button" ] [ text "✕" ]

              else
                text ""
            ]
        , if model.search.isSearching || model.search.searchError /= Nothing || model.search.unifiedResults /= Nothing then
            text ""

          else
            case model.activeTab of
                ProjectsTab ->
                    viewFilterBar model

                ObservationsTab ->
                    text ""

                TimelineTab ->
                    text ""

                AuditTab ->
                    text ""
        ]


viewFilterBar : Model -> Html Msg
viewFilterBar model =
    div [ class "filter-bar" ]
        [ div [ class "filter-group" ]
            [ span [ class "filter-label" ] [ text "Show:" ]
            , viewFilterPill "All" (model.search.filterShowOnly == ShowAll) (SetFilterShowOnly ShowAll)
            , viewFilterPill "Projects" (model.search.filterShowOnly == ShowProjectsOnly) (SetFilterShowOnly ShowProjectsOnly)
            , viewFilterPill "Tasks" (model.search.filterShowOnly == ShowTasksOnly) (SetFilterShowOnly ShowTasksOnly)
            ]
        , div [ class "filter-group" ]
            [ span [ class "filter-label" ] [ text "Project:" ]
            , viewFilterPill "Active" (List.member "active" model.search.filterProjectStatuses) (ToggleFilterProjectStatus "active")
            , viewFilterPill "Paused" (List.member "paused" model.search.filterProjectStatuses) (ToggleFilterProjectStatus "paused")
            , viewFilterPill "Completed" (List.member "completed" model.search.filterProjectStatuses) (ToggleFilterProjectStatus "completed")
            , viewFilterPill "Archived" (List.member "archived" model.search.filterProjectStatuses) (ToggleFilterProjectStatus "archived")
            ]
        , div [ class "filter-group" ]
            [ span [ class "filter-label" ] [ text "Task:" ]
            , viewFilterPill "Todo" (List.member "todo" model.search.filterTaskStatuses) (ToggleFilterTaskStatus "todo")
            , viewFilterPill "In Progress" (List.member "in_progress" model.search.filterTaskStatuses) (ToggleFilterTaskStatus "in_progress")
            , viewFilterPill "Blocked" (List.member "blocked" model.search.filterTaskStatuses) (ToggleFilterTaskStatus "blocked")
            , viewFilterPill "Done" (List.member "done" model.search.filterTaskStatuses) (ToggleFilterTaskStatus "done")
            , viewFilterPill "Cancelled" (List.member "cancelled" model.search.filterTaskStatuses) (ToggleFilterTaskStatus "cancelled")
            ]
        , div [ class "filter-group" ]
            [ span [ class "filter-label" ] [ text "Priority:" ]
            , select
                [ class "filter-select"
                , onInput
                    (\s ->
                        case s of
                            "any" ->
                                SetFilterPriority AnyPriority

                            "exact" ->
                                SetFilterPriority (ExactPriority 5)

                            "above" ->
                                SetFilterPriority (AbovePriority 5)

                            "below" ->
                                SetFilterPriority (BelowPriority 5)

                            _ ->
                                SetFilterPriority AnyPriority
                    )
                ]
                [ option [ value "any", selected (model.search.filterPriority == AnyPriority) ] [ text "Any" ]
                , option
                    [ value "exact"
                    , selected
                        (case model.search.filterPriority of
                            ExactPriority _ ->
                                True

                            _ ->
                                False
                        )
                    ]
                    [ text "Exact" ]
                , option
                    [ value "above"
                    , selected
                        (case model.search.filterPriority of
                            AbovePriority _ ->
                                True

                            _ ->
                                False
                        )
                    ]
                    [ text "Above" ]
                , option
                    [ value "below"
                    , selected
                        (case model.search.filterPriority of
                            BelowPriority _ ->
                                True

                            _ ->
                                False
                        )
                    ]
                    [ text "Below" ]
                ]
            , case model.search.filterPriority of
                AnyPriority ->
                    text ""

                ExactPriority v ->
                    input
                        [ class "filter-priority-input"
                        , type_ "number"
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "10"
                        , value (String.fromInt v)
                        , onInput (\s -> SetFilterPriority (ExactPriority (Maybe.withDefault 5 (String.toInt s))))
                        ]
                        []

                AbovePriority v ->
                    input
                        [ class "filter-priority-input"
                        , type_ "number"
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "10"
                        , value (String.fromInt v)
                        , onInput (\s -> SetFilterPriority (AbovePriority (Maybe.withDefault 5 (String.toInt s))))
                        ]
                        []

                BelowPriority v ->
                    input
                        [ class "filter-priority-input"
                        , type_ "number"
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "10"
                        , value (String.fromInt v)
                        , onInput (\s -> SetFilterPriority (BelowPriority (Maybe.withDefault 5 (String.toInt s))))
                        ]
                        []
            ]
        ]


viewMemoryFilterBar : Model -> Html Msg
viewMemoryFilterBar model =
    div [ class "filter-bar" ]
        [ div [ class "filter-group" ]
            [ span [ class "filter-label" ] [ text "Type:" ]
            , viewFilterPill "Short Term" (List.member "short_term" model.search.filterMemoryTypes) (ToggleFilterMemoryType "short_term")
            , viewFilterPill "Long Term" (List.member "long_term" model.search.filterMemoryTypes) (ToggleFilterMemoryType "long_term")
            ]
        , div [ class "filter-group" ]
            [ span [ class "filter-label" ] [ text "Pinned:" ]
            , viewFilterPill "All" (model.search.filterMemoryPinned == Nothing) (SetFilterMemoryPinned Nothing)
            , viewFilterPill "Pinned" (model.search.filterMemoryPinned == Just True) (SetFilterMemoryPinned (Just True))
            , viewFilterPill "Unpinned" (model.search.filterMemoryPinned == Just False) (SetFilterMemoryPinned (Just False))
            ]
        , div [ class "filter-group" ]
            [ span [ class "filter-label" ] [ text "Usage:" ]
            , button
                [ class
                    (if model.search.filterMemoryActiveLinked then
                        "filter-pill filter-pill-active"

                     else
                        "filter-pill"
                    )
                , title "Show memories linked to at least one non-archived project or non-done/non-cancelled task."
                , onClick (ToggleFilterMemoryActiveLinked (not model.search.filterMemoryActiveLinked))
                ]
                [ text "Active links" ]
            ]
        , div [ class "filter-group" ]
            [ span [ class "filter-label" ] [ text "Importance:" ]
            , select
                [ class "filter-select"
                , onInput
                    (\s ->
                        case s of
                            "exact" ->
                                SetFilterImportance (ExactPriority 5)

                            "above" ->
                                SetFilterImportance (AbovePriority 5)

                            "below" ->
                                SetFilterImportance (BelowPriority 5)

                            _ ->
                                SetFilterImportance AnyPriority
                    )
                ]
                [ option [ value "any", selected (model.search.filterImportance == AnyPriority) ] [ text "Any" ]
                , option
                    [ value "exact"
                    , selected
                        (case model.search.filterImportance of
                            ExactPriority _ ->
                                True

                            _ ->
                                False
                        )
                    ]
                    [ text "Exact" ]
                , option
                    [ value "above"
                    , selected
                        (case model.search.filterImportance of
                            AbovePriority _ ->
                                True

                            _ ->
                                False
                        )
                    ]
                    [ text "Above" ]
                , option
                    [ value "below"
                    , selected
                        (case model.search.filterImportance of
                            BelowPriority _ ->
                                True

                            _ ->
                                False
                        )
                    ]
                    [ text "Below" ]
                ]
            , case model.search.filterImportance of
                AnyPriority ->
                    text ""

                ExactPriority v ->
                    input
                        [ class "filter-priority-input"
                        , type_ "number"
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "10"
                        , value (String.fromInt v)
                        , onInput (\s -> SetFilterImportance (ExactPriority (Maybe.withDefault 5 (String.toInt s))))
                        ]
                        []

                AbovePriority v ->
                    input
                        [ class "filter-priority-input"
                        , type_ "number"
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "10"
                        , value (String.fromInt v)
                        , onInput (\s -> SetFilterImportance (AbovePriority (Maybe.withDefault 5 (String.toInt s))))
                        ]
                        []

                BelowPriority v ->
                    input
                        [ class "filter-priority-input"
                        , type_ "number"
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "10"
                        , value (String.fromInt v)
                        , onInput (\s -> SetFilterImportance (BelowPriority (Maybe.withDefault 5 (String.toInt s))))
                        ]
                        []
            ]
        ]


viewFilterPill : String -> Bool -> Msg -> Html Msg
viewFilterPill label isActive msg =
    button
        [ class
            (if isActive then
                "filter-pill filter-pill-active"

             else
                "filter-pill"
            )
        , onClick msg
        ]
        [ text label ]


unifiedSearchResultCount : Api.UnifiedSearchResults -> Int
unifiedSearchResultCount results =
    List.length results.observations + List.length results.projects + List.length results.tasks


searchResultPresentations : Api.UnifiedSearchResults -> List SearchResultPresentation
searchResultPresentations results =
    List.map projectResultPresentation results.projects
        ++ List.map taskResultPresentation results.tasks
        ++ List.map observationResultPresentation results.observations


viewUnifiedSearchLoading : Model -> Html Msg
viewUnifiedSearchLoading model =
    div [ class "unified-search-results search-results-state", attribute "role" "status", attribute "aria-live" "polite" ]
        [ div [ class "search-state-card" ]
            [ h3 [] [ text "Searching…" ]
            , p [] [ text ("Looking for results matching " ++ searchQueryLabel model ++ ".") ]
            ]
        ]


viewUnifiedSearchError : Model -> String -> Html Msg
viewUnifiedSearchError model message =
    div [ class "unified-search-results search-results-state", attribute "role" "alert" ]
        [ div [ class "search-state-card search-state-error" ]
            [ h3 [] [ text "Search unavailable" ]
            , p [] [ text message ]
            , p [ class "search-state-muted" ] [ text ("Query: " ++ searchQueryLabel model) ]
            , button [ class "btn-small search-result-action", onClick SubmitSearch ] [ text "Retry search" ]
            ]
        ]


viewUnifiedSearchResults : Model -> Api.UnifiedSearchResults -> Html Msg
viewUnifiedSearchResults model results =
    let
        totalCount =
            unifiedSearchResultCount results
    in
    div [ class "unified-search-results" ]
        ([ div [ class "search-results-header" ]
            [ div []
                [ h3 [ class "search-results-title" ] [ text ("Search results for " ++ searchQueryLabel model) ]
                , span [ class "search-results-count" ]
                    [ text (String.fromInt totalCount ++ " " ++ pluralize "result" totalCount) ]
                ]
            ]
         ]
            ++ (if totalCount == 0 then
                    [ viewEmptySearchResults model ]

                else
                    [ viewSearchSection "Projects" results.projects projectResultPresentation
                    , viewSearchSection "Tasks" results.tasks taskResultPresentation
                    , viewSearchSection "Observations" results.observations observationResultPresentation
                    ]
               )
        )


viewSearchSection : String -> List source -> (source -> SearchResultPresentation) -> Html Msg
viewSearchSection label items toPresentation =
    if List.isEmpty items then
        text ""

    else
        div [ class "search-results-section" ]
            [ h3 [ class "search-section-title" ]
                [ text (label ++ " (" ++ String.fromInt (List.length items) ++ ")") ]
            , div [ class "search-result-list" ]
                (List.map (toPresentation >> viewSearchResultCard) items)
            ]


viewEmptySearchResults : Model -> Html Msg
viewEmptySearchResults model =
    div [ class "empty-state search-empty-state" ]
        [ h3 [] [ text "No results found" ]
        , p [] [ text ("No projects, tasks, or observations matched " ++ searchQueryLabel model ++ ".") ]
        ]


viewSearchResultCard : SearchResultPresentation -> Html Msg
viewSearchResultCard result =
    div [ class ("search-result-card search-result-" ++ result.entityType), id ("search-result-" ++ result.entityId) ]
        [ div [ class "search-result-main" ]
            [ div [ class "search-result-topline" ]
                ([ span [ class ("entity-type-label " ++ result.entityTypeClass), title result.entityType ] [ text result.entityTypeLabel ]
                 , span [ class "search-result-title", title result.title ] [ text result.title ]
                 ]
                    ++ List.map viewSearchResultBadge result.badges
                )
            , div [ class "search-result-summary" ] [ text result.summary ]
            , viewSearchResultTags result.tags
            , viewLinkedMemorySummaries result.linkedMemories
            ]
        , div [ class "search-result-actions" ]
            [ button
                [ class "btn-small search-result-action"
                , onClick (NavigateToSearchResult result.entityType result.entityId)
                , title (result.actionLabel ++ " " ++ result.entityType)
                ]
                [ text result.actionLabel ]
            ]
        ]


viewSearchResultBadge : SearchResultBadge -> Html Msg
viewSearchResultBadge badge =
    span [ class badge.className, title badge.title ] [ text badge.label ]


viewSearchResultTags : List String -> Html Msg
viewSearchResultTags tags =
    if List.isEmpty tags then
        text ""

    else
        div [ class "tag-list search-result-tags" ]
            (List.map (\tag -> span [ class "tag", title tag ] [ text tag ]) tags)


viewLinkedMemorySummaries : List LinkedMemoryPresentation -> Html Msg
viewLinkedMemorySummaries memories =
    if List.isEmpty memories then
        text ""

    else
        div [ class "linked-memories-summary search-linked-memories" ]
            (List.map viewLinkedMemorySummary memories)


viewLinkedMemorySummary : LinkedMemoryPresentation -> Html Msg
viewLinkedMemorySummary mem =
    div [ class "linked-memory-chip" ]
        [ span [ class "linked-memory-importance", title "Linked memory importance" ]
            [ text (String.fromInt mem.importance) ]
        , span [ class "linked-memory-text" ]
            [ text mem.summary ]
        , if List.isEmpty mem.tags then
            text ""

          else
            span [ class "linked-memory-tags", title (String.join ", " mem.tags) ]
                [ text (String.join ", " mem.tags) ]
        ]


projectResultPresentation : Api.Project -> SearchResultPresentation
projectResultPresentation project =
    { entityType = "project"
    , entityTypeLabel = "PRJ"
    , entityTypeClass = "entity-type-project"
    , entityId = project.id
    , title = nonBlankString project.name ("Project " ++ shortId project.id)
    , summary = nonBlankMaybe project.description "No description"
    , badges =
        [ { label = humanizeToken (Api.projectStatusToString project.status)
          , className = "badge badge-" ++ Api.projectStatusToString project.status
          , title = "Project status"
          }
        , priorityBadge project.priority
        ]
    , tags = []
    , linkedMemories = []
    , actionLabel = "Focus project"
    }


taskResultPresentation : Api.Task -> SearchResultPresentation
taskResultPresentation task =
    { entityType = "task"
    , entityTypeLabel =
        if task.parentId == Nothing then
            "TSK"

        else
            "SUB"
    , entityTypeClass =
        if task.parentId == Nothing then
            "entity-type-task"

        else
            "entity-type-subtask"
    , entityId = task.id
    , title = nonBlankString task.title ("Task " ++ shortId task.id)
    , summary = nonBlankMaybe task.description "No description"
    , badges =
        [ { label = humanizeToken (taskStatusDisplayText task.status)
          , className = taskStatusBadgeClass task.status
          , title = taskStatusTitle task.status
          }
        , priorityBadge task.priority
        ]
    , tags = []
    , linkedMemories = []
    , actionLabel = "Focus task"
    }


observationResultPresentation : Api.ObservationSearchHit -> SearchResultPresentation
observationResultPresentation observation =
    { entityType = "observation"
    , entityTypeLabel = "OBS"
    , entityTypeClass = "entity-type-observation"
    , entityId = observation.id
    , title = observation.subject
    , summary = nonBlankString observation.contentPreview "No observation content"
    , badges =
        [ { label = subjectKindLabel observation.subjectKind
          , className = "badge badge-observation"
          , title = "Observation subject kind"
          }
        ]
    , tags = []
    , linkedMemories = []
    , actionLabel = "Open observation"
    }


subjectKindLabel : Api.SubjectKind -> String
subjectKindLabel subjectKind =
    case subjectKind of
        Api.SubjectFile ->
            "File"

        Api.SubjectGlob ->
            "Glob"


priorityBadge : Int -> SearchResultBadge
priorityBadge priority =
    { label = "P" ++ String.fromInt priority
    , className = "badge badge-priority"
    , title = "Priority"
    }


memoryTypeLabel : Api.MemoryType -> String
memoryTypeLabel memoryType =
    case memoryType of
        Api.ShortTerm ->
            "Short term"

        Api.LongTerm ->
            "Long term"


searchQueryLabel : Model -> String
searchQueryLabel model =
    let
        trimmed =
            String.trim model.search.query
    in
    if String.isEmpty trimmed then
        "the current query"

    else
        "“" ++ trimmed ++ "”"


nonBlankMaybe : Maybe String -> String -> String
nonBlankMaybe maybeValue fallback =
    maybeValue
        |> Maybe.map (\value -> nonBlankString value fallback)
        |> Maybe.withDefault fallback


nonBlankString : String -> String -> String
nonBlankString value fallback =
    if String.isEmpty (String.trim value) then
        fallback

    else
        value


shortId : String -> String
shortId value =
    String.left 8 value


pluralize : String -> Int -> String
pluralize singular count =
    if count == 1 then
        singular

    else
        singular ++ "s"


humanizeToken : String -> String
humanizeToken token =
    token
        |> String.replace "_" " "
        |> String.words
        |> List.map capitalizeWord
        |> String.join " "


capitalizeWord : String -> String
capitalizeWord word =
    case String.uncons word of
        Nothing ->
            ""

        Just ( first, rest ) ->
            String.fromChar first |> String.toUpper |> (\head -> head ++ rest)
