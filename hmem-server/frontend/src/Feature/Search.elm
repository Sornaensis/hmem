module Feature.Search exposing (update, viewSearchBar, viewUnifiedSearchResults)

import Api
import Helpers exposing (saveFiltersCmd)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchInput query ->
            let
                newModel =
                    if String.isEmpty query then
                        { model | searchQuery = query, unifiedSearchResults = Nothing, isSearching = False }

                    else
                        { model | searchQuery = query }
            in
            ( newModel, saveFiltersCmd newModel )

        SubmitSearch ->
            let
                trimmed =
                    String.trim model.searchQuery
            in
            if String.isEmpty trimmed then
                ( { model | unifiedSearchResults = Nothing, isSearching = False }, Cmd.none )

            else
                ( { model | isSearching = True }
                , Api.unifiedSearch model.flags.apiUrl trimmed model.selectedWorkspaceId GotUnifiedSearchResults
                )

        GotUnifiedSearchResults result ->
            case result of
                Ok results ->
                    ( { model | unifiedSearchResults = Just results, isSearching = False }, Cmd.none )

                Err _ ->
                    ( { model | isSearching = False }
                    , Cmd.none
                    )

        SetFilterShowOnly show ->
            let
                newModel =
                    { model | filterShowOnly = show }
            in
            ( newModel, saveFiltersCmd newModel )

        SetFilterPriority pri ->
            let
                newModel =
                    { model | filterPriority = pri }
            in
            ( newModel, saveFiltersCmd newModel )

        ToggleFilterProjectStatus status ->
            let
                newStatuses =
                    if List.member status model.filterProjectStatuses then
                        List.filter (\s -> s /= status) model.filterProjectStatuses

                    else
                        status :: model.filterProjectStatuses

                newModel =
                    { model | filterProjectStatuses = newStatuses }
            in
            ( newModel, saveFiltersCmd newModel )

        ToggleFilterTaskStatus status ->
            let
                newStatuses =
                    if List.member status model.filterTaskStatuses then
                        List.filter (\s -> s /= status) model.filterTaskStatuses

                    else
                        status :: model.filterTaskStatuses

                newModel =
                    { model | filterTaskStatuses = newStatuses }
            in
            ( newModel, saveFiltersCmd newModel )

        ToggleFilterMemoryType mtype ->
            let
                newTypes =
                    if List.member mtype model.filterMemoryTypes then
                        List.filter (\s -> s /= mtype) model.filterMemoryTypes

                    else
                        mtype :: model.filterMemoryTypes

                newModel =
                    { model | filterMemoryTypes = newTypes }
            in
            ( newModel, saveFiltersCmd newModel )

        SetFilterImportance imp ->
            let
                newModel =
                    { model | filterImportance = imp }
            in
            ( newModel, saveFiltersCmd newModel )

        SetFilterMemoryPinned pinned ->
            let
                newModel =
                    { model | filterMemoryPinned = pinned }
            in
            ( newModel, saveFiltersCmd newModel )

        ToggleFilterTag tag ->
            let
                newTags =
                    if List.member tag model.filterTags then
                        List.filter (\t -> t /= tag) model.filterTags

                    else
                        tag :: model.filterTags

                newModel =
                    { model | filterTags = newTags }
            in
            ( newModel, saveFiltersCmd newModel )

        _ ->
            ( model, Cmd.none )


viewSearchBar : Model -> Html Msg
viewSearchBar model =
    div [ class "search-filter-bar" ]
        [ Html.form [ class "search-bar", onSubmit SubmitSearch ]
            [ input
                [ class "search-input"
                , type_ "text"
                , placeholder "Search all entities... (Enter to search)"
                , value model.searchQuery
                , onInput SearchInput
                ]
                []
            , if model.isSearching then
                span [ class "search-spinner" ] [ text "…" ]

              else if not (String.isEmpty model.searchQuery) then
                button [ class "search-clear", onClick (SearchInput ""), type_ "button" ] [ text "✕" ]

              else
                text ""
            ]
        , case model.unifiedSearchResults of
            Just _ ->
                text ""

            Nothing ->
                case model.activeTab of
                    ProjectsTab ->
                        viewFilterBar model

                    MemoriesTab ->
                        viewMemoryFilterBar model
        ]


viewFilterBar : Model -> Html Msg
viewFilterBar model =
    div [ class "filter-bar" ]
        [ div [ class "filter-group" ]
            [ span [ class "filter-label" ] [ text "Show:" ]
            , viewFilterPill "All" (model.filterShowOnly == ShowAll) (SetFilterShowOnly ShowAll)
            , viewFilterPill "Projects" (model.filterShowOnly == ShowProjectsOnly) (SetFilterShowOnly ShowProjectsOnly)
            , viewFilterPill "Tasks" (model.filterShowOnly == ShowTasksOnly) (SetFilterShowOnly ShowTasksOnly)
            ]
        , div [ class "filter-group" ]
            [ span [ class "filter-label" ] [ text "Project:" ]
            , viewFilterPill "Active" (List.member "active" model.filterProjectStatuses) (ToggleFilterProjectStatus "active")
            , viewFilterPill "Paused" (List.member "paused" model.filterProjectStatuses) (ToggleFilterProjectStatus "paused")
            , viewFilterPill "Completed" (List.member "completed" model.filterProjectStatuses) (ToggleFilterProjectStatus "completed")
            , viewFilterPill "Archived" (List.member "archived" model.filterProjectStatuses) (ToggleFilterProjectStatus "archived")
            ]
        , div [ class "filter-group" ]
            [ span [ class "filter-label" ] [ text "Task:" ]
            , viewFilterPill "Todo" (List.member "todo" model.filterTaskStatuses) (ToggleFilterTaskStatus "todo")
            , viewFilterPill "In Progress" (List.member "in_progress" model.filterTaskStatuses) (ToggleFilterTaskStatus "in_progress")
            , viewFilterPill "Blocked" (List.member "blocked" model.filterTaskStatuses) (ToggleFilterTaskStatus "blocked")
            , viewFilterPill "Done" (List.member "done" model.filterTaskStatuses) (ToggleFilterTaskStatus "done")
            , viewFilterPill "Cancelled" (List.member "cancelled" model.filterTaskStatuses) (ToggleFilterTaskStatus "cancelled")
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
                [ option [ value "any", selected (model.filterPriority == AnyPriority) ] [ text "Any" ]
                , option
                    [ value "exact"
                    , selected
                        (case model.filterPriority of
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
                        (case model.filterPriority of
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
                        (case model.filterPriority of
                            BelowPriority _ ->
                                True

                            _ ->
                                False
                        )
                    ]
                    [ text "Below" ]
                ]
            , case model.filterPriority of
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
            , viewFilterPill "Short Term" (List.member "short_term" model.filterMemoryTypes) (ToggleFilterMemoryType "short_term")
            , viewFilterPill "Long Term" (List.member "long_term" model.filterMemoryTypes) (ToggleFilterMemoryType "long_term")
            ]
        , div [ class "filter-group" ]
            [ span [ class "filter-label" ] [ text "Pinned:" ]
            , viewFilterPill "All" (model.filterMemoryPinned == Nothing) (SetFilterMemoryPinned Nothing)
            , viewFilterPill "Pinned" (model.filterMemoryPinned == Just True) (SetFilterMemoryPinned (Just True))
            , viewFilterPill "Unpinned" (model.filterMemoryPinned == Just False) (SetFilterMemoryPinned (Just False))
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
                [ option [ value "any", selected (model.filterImportance == AnyPriority) ] [ text "Any" ]
                , option
                    [ value "exact"
                    , selected
                        (case model.filterImportance of
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
                        (case model.filterImportance of
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
                        (case model.filterImportance of
                            BelowPriority _ ->
                                True

                            _ ->
                                False
                        )
                    ]
                    [ text "Below" ]
                ]
            , case model.filterImportance of
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


viewUnifiedSearchResults : (Api.Memory -> Html Msg) -> Model -> Api.UnifiedSearchResults -> Html Msg
viewUnifiedSearchResults viewMemoryCardFn model results =
    let
        totalCount =
            List.length results.memories + List.length results.projects + List.length results.tasks
    in
    div [ class "unified-search-results" ]
        [ div [ class "search-results-header" ]
            [ span [ class "search-results-count" ]
                [ text (String.fromInt totalCount ++ " results") ]
            ]
        , if not (List.isEmpty results.projects) then
            div [ class "search-results-section" ]
                [ h3 [ class "search-section-title" ]
                    [ text ("Projects (" ++ String.fromInt (List.length results.projects) ++ ")") ]
                , div [ class "entity-list" ]
                    (List.map (viewSearchProjectResult model) results.projects)
                ]

          else
            text ""
        , if not (List.isEmpty results.tasks) then
            div [ class "search-results-section" ]
                [ h3 [ class "search-section-title" ]
                    [ text ("Tasks (" ++ String.fromInt (List.length results.tasks) ++ ")") ]
                , div [ class "entity-list" ]
                    (List.map (viewSearchTaskResult model) results.tasks)
                ]

          else
            text ""
        , if not (List.isEmpty results.memories) then
            div [ class "search-results-section" ]
                [ h3 [ class "search-section-title" ]
                    [ text ("Memories (" ++ String.fromInt (List.length results.memories) ++ ")") ]
                , div [ class "entity-list" ]
                    (List.map viewMemoryCardFn results.memories)
                ]

          else
            text ""
        , if totalCount == 0 then
            div [ class "empty-state" ] [ text "No results found." ]

          else
            text ""
        ]


viewSearchProjectResult : Model -> Api.ProjectSearchResult -> Html Msg
viewSearchProjectResult model result =
    div [ class "search-result-card" ]
        [ div [ class "card-header" ]
            [ span [ class "card-title" ] [ text result.project.name ]
            , span [ class ("badge badge-" ++ Api.projectStatusToString result.project.status) ]
                [ text (Api.projectStatusToString result.project.status) ]
            , span [ class "badge badge-priority" ]
                [ text ("P" ++ String.fromInt result.project.priority) ]
            ]
        , case result.project.description of
            Just desc ->
                div [ class "card-body" ] [ text desc ]

            Nothing ->
                text ""
        , if not (List.isEmpty result.linkedMemories) then
            div [ class "linked-memories-summary" ]
                (List.map viewLinkedMemorySummary result.linkedMemories)

          else
            text ""
        ]


viewSearchTaskResult : Model -> Api.TaskSearchResult -> Html Msg
viewSearchTaskResult model result =
    div [ class "search-result-card" ]
        [ div [ class "card-header" ]
            [ span [ class "card-title" ] [ text result.task.title ]
            , span [ class ("badge badge-" ++ Api.taskStatusToString result.task.status) ]
                [ text (Api.taskStatusToString result.task.status) ]
            , span [ class "badge badge-priority" ]
                [ text ("P" ++ String.fromInt result.task.priority) ]
            ]
        , case result.task.description of
            Just desc ->
                div [ class "card-body" ] [ text desc ]

            Nothing ->
                text ""
        , if not (List.isEmpty result.linkedMemories) then
            div [ class "linked-memories-summary" ]
                (List.map viewLinkedMemorySummary result.linkedMemories)

          else
            text ""
        ]


viewLinkedMemorySummary : Api.LinkedMemorySummary -> Html Msg
viewLinkedMemorySummary mem =
    div [ class "linked-memory-chip" ]
        [ span [ class "linked-memory-importance" ]
            [ text (String.fromInt mem.importance) ]
        , span [ class "linked-memory-text" ]
            [ text (Maybe.withDefault "(no summary)" mem.summary) ]
        ]
