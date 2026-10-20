module Feature.Search exposing
    ( LinkedMemoryPresentation
    , SearchResultBadge
    , SearchResultPresentation
    , selectedEntityTypes
    , requestIdentity
    , clearTransientSearchState
    , init
    , searchResultPresentations
    , unifiedSearchEntityTypes
    , unifiedSearchResponseMatches
    , unifiedSearchResultCount
    , update
    , refreshAcceptedSearch
    , viewSearchBar
    , viewFocusedSearchBar
    , viewUnifiedSearchError
    , viewUnifiedSearchLoading
    , viewUnifiedSearchResults
    )

import Api
import Feature.DataLoading
import Feature.FocusedFilters
import Feature.Focus
import Feature.Observation
import Dict
import Permissions
import Helpers exposing (replaceFragment, saveFiltersCmd, scrollToElement, taskStatusBadgeClass, taskStatusDisplayText, taskStatusTitle)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types exposing (..)
import Json.Decode as Decode
import Process
import Task


init : SearchModel
init =
    { query = ""
    , globalQuery = ""
    , globalActivated = False
    , globalComposing = False
    , globalGeneration = 0
    , entityTypes = Nothing
    , entityTypesActor = Nothing
    , completedRequest = Nothing
    , submittedQuery = Nothing
    , refreshPending = False
    , unifiedResults = Nothing
    , isSearching = False
    , searchError = Nothing
    , activeRequestQuery = Nothing
    , activeRequest = Nothing
    , nextRequestToken = 1
    , filterShowOnly = ShowAll
    , filterShowEmptyProjects = True
    , filterPriority = AnyPriority
    , filterProjectStatuses = [ "active", "paused" ]
    , filterTaskStatuses = [ "todo", "in_progress", "blocked" ]
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
    , titleCopyValue : Maybe String
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
    case hierarchySearchChange msg model.search of
        Just search ->
            reloadNavigation (Feature.FocusedFilters.edit { model | search = search })

        Nothing ->
            updateOther msg model


hierarchySearchChange : Msg -> SearchModel -> Maybe SearchModel
hierarchySearchChange msg search =
    let
        toggle all status selected =
            if List.isEmpty selected then
                List.filter ((/=) status) all
            else if List.member status selected then
                List.filter ((/=) status) selected
            else
                status :: selected
    in
    case msg of
        SearchInput query -> Just { search | query = query }
        SetFilterShowOnly show -> Just { search | filterShowOnly = show }
        SetFilterShowEmptyProjects show -> Just { search | filterShowEmptyProjects = show }
        SetFilterPriority priority -> Just { search | filterPriority = priority }
        ToggleFilterProjectStatus status ->
            Just { search | filterProjectStatuses = toggle [ "active", "paused", "completed", "archived" ] status search.filterProjectStatuses }
        ToggleFilterTaskStatus status ->
            Just { search | filterTaskStatuses = toggle [ "todo", "in_progress", "blocked", "done", "cancelled" ] status search.filterTaskStatuses }
        _ -> Nothing


updateOther : Msg -> Model -> ( Model, Cmd Msg )
updateOther msg model =
    case msg of
        WorkspaceSearch action ->
            if model.focusedFilters == Nothing then
                update action model
            else
                case hierarchySearchChange action (Feature.FocusedFilters.normalSearch model) of
                    Just search ->
                        let updated = Feature.FocusedFilters.workspace search model in
                        ( updated, saveFiltersCmd updated )
                    Nothing ->
                        ( model, Cmd.none )

        GlobalSearchInput query ->
            changeGlobalQuery False query model

        GlobalSearchCompositionStart ->
            let state = model.search in
            ( { model | search = { state | globalComposing = True, globalGeneration = state.globalGeneration + 1, activeRequest = Nothing, refreshPending = False } }, Cmd.none )

        GlobalSearchCompositionEnd query ->
            let state = model.search in
            changeGlobalQuery False query { model | search = { state | globalComposing = False } }

        GlobalSearchReady request ->
            if request == requestIdentity model && not model.search.globalComposing then
                startGlobalSearch model
            else
                ( model, Cmd.none )

        ToggleSearchEntityType entityType ->
            if not (List.member entityType (supportedEntityTypes model)) then
                ( model, Cmd.none )
            else
                let
                    state = model.search
                    selected = selectedEntityTypes model
                    types = if List.member entityType selected then List.filter ((/=) entityType) selected else entityType :: selected
                    updated = { model | search = { state | entityTypes = Just types, entityTypesActor = Just (Helpers.globalSearchActor model) } }
                    ( searched, command ) = changeGlobalQuery True state.globalQuery updated
                in
                ( searched, Cmd.batch [ command, Helpers.saveEntitySearchPreferencesCmd searched ] )

        SubmitSearch ->
            if model.search.globalComposing then ( model, Cmd.none ) else
            changeGlobalQuery True model.search.globalQuery model

        GotUnifiedSearchResults workspaceId sessionEpoch token requestedQuery result ->
            let state = model.search in
            if model.auth.status /= AuthReady || not (Permissions.canReadCurrentWorkspace model)
                || sessionEpoch /= model.sessionRequestEpoch
                || not (unifiedSearchResponseMatches workspaceId token requestedQuery model.selectedWorkspaceId state)
                || Maybe.map (\request -> { request | token = state.nextRequestToken }) state.activeRequest /= Just (requestIdentity model) then
                ( model, Cmd.none )
            else if state.refreshPending then
                startGlobalSearch { model | search = { state | activeRequest = Nothing, refreshPending = False } }
            else
                case result of
                    Ok results ->
                        ( { model | search = { state | unifiedResults = Just results, completedRequest = state.activeRequest, isSearching = False, searchError = Nothing, activeRequestQuery = Nothing, activeRequest = Nothing } }, Cmd.none )
                    Err _ ->
                        ( { model | search = { state | isSearching = False, searchError = Just "Search failed. Retry to update results.", activeRequestQuery = Nothing, activeRequest = Nothing } }, Cmd.none )

        NavigateToSearchResult entityType entityId ->
            navigateToSearchResult entityType entityId model

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
        && (searchModel.activeRequest |> Maybe.map (\request -> request.workspaceId == workspaceId && request.token == token && request.query == query) |> Maybe.withDefault False)


supportedEntityTypes : Model -> List String
supportedEntityTypes model =
    model.selectedWorkspaceId
        |> Maybe.andThen (\workspaceId -> Dict.get workspaceId model.workspaces)
        |> Maybe.map (.workspaceType >> unifiedSearchEntityTypes)
        |> Maybe.withDefault [ "project", "task" ]


selectedEntityTypes : Model -> List String
selectedEntityTypes model =
    let supported = supportedEntityTypes model in
    if model.search.entityTypesActor == Just (Helpers.globalSearchActor model) then
        model.search.entityTypes |> Maybe.map (\types -> List.filter (\kind -> List.member kind types) supported) |> Maybe.withDefault supported
    else supported


requestIdentity : Model -> SearchRequest
requestIdentity model =
    { workspaceId = Maybe.withDefault "" model.selectedWorkspaceId
    , sessionEpoch = model.sessionRequestEpoch
    , actor = Helpers.globalSearchActor model
    , token = model.search.nextRequestToken
    , generation = model.search.globalGeneration
    , query = String.trim model.search.globalQuery
    , entityTypes = selectedEntityTypes model
    , offset = 0
    }


changeGlobalQuery : Bool -> String -> Model -> ( Model, Cmd Msg )
changeGlobalQuery immediate query model =
    let
        state = model.search
        empty = String.isEmpty (String.trim query)
        updated = { model | search = { state | globalQuery = query, globalActivated = state.globalActivated || not empty, globalGeneration = state.globalGeneration + 1
            , activeRequest = Nothing, activeRequestQuery = Nothing, refreshPending = False, submittedQuery = Nothing, searchError = Nothing
            , isSearching = not empty, unifiedResults = if empty then Nothing else state.unifiedResults
            , completedRequest = if empty then Nothing else state.completedRequest } }
    in
    if empty then ( updated, Cmd.none )
    else if List.isEmpty (selectedEntityTypes updated) then
        let next = updated.search in
        ( { updated | search = { next | isSearching = False, unifiedResults = Just { projects = [], tasks = [], observations = [] }, completedRequest = Nothing } }, Cmd.none )
    else if updated.search.globalComposing then ( updated, Cmd.none )
    else if immediate then startGlobalSearch updated
    else ( updated, Process.sleep 250 |> Task.perform (\_ -> GlobalSearchReady (requestIdentity updated)) )


startGlobalSearch : Model -> ( Model, Cmd Msg )
startGlobalSearch model =
    let
        state = model.search
        request = requestIdentity model
    in
    if model.auth.status /= AuthReady || not (Permissions.canReadCurrentWorkspace model) || model.selectedWorkspaceId == Nothing
        || String.isEmpty request.query || List.isEmpty request.entityTypes || state.globalComposing then
        ( model, Cmd.none )
    else
        ( { model | search = { state | activeRequest = Just request, activeRequestQuery = Just request.query, submittedQuery = Just request.query, isSearching = True, searchError = Nothing, refreshPending = False, nextRequestToken = request.token + 1 } }
        , Api.unifiedSearch model.flags.apiUrl request.query request.workspaceId request.entityTypes (GotUnifiedSearchResults request.workspaceId request.sessionEpoch request.token request.query) )


unifiedSearchEntityTypes : Api.WorkspaceType -> List String
unifiedSearchEntityTypes workspaceType =
    if workspaceType == Api.Repository then
        [ "project", "task", "observation" ]

    else
        [ "project", "task" ]


clearTransientSearchState : SearchModel -> SearchModel
clearTransientSearchState =
    Feature.FocusedFilters.clearTransientSearchState


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
                |> (if isFocusableEntity then Feature.FocusedFilters.enter else identity)
    in
    if entityType == "observation" then
        let
            ( detailModel, detailCmd ) =
                Feature.Observation.selectObservation entityId nextModel

            ( linkedModel, linkCmd ) =
                if detailModel.observations.selectedId /= model.observations.selectedId || detailModel.activeTab /= model.activeTab then
                    Helpers.writeObservationHistory True detailModel
                else
                    ( detailModel, Cmd.none )
        in
        ( linkedModel
        , Cmd.batch
            [ linkCmd
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
    let normal = { model | search = Feature.FocusedFilters.normalSearch model } in
    div [ class ("workspace-search-filter-bar" ++ (if model.search.globalActivated then " entity-search-activated" else "")) ]
        [ div [ class "search-bar" ]
            [ input [ class "search-input", type_ "search", placeholder "Search all entities...", attribute "aria-label" "Search all entities", value model.search.globalQuery
                , onInput GlobalSearchInput
                , on "compositionstart" (Decode.succeed GlobalSearchCompositionStart)
                , on "compositionend" (Decode.map GlobalSearchCompositionEnd (Decode.at [ "target", "value" ] Decode.string)) ] []
            , if not (String.isEmpty model.search.globalQuery) then
                button [ class "search-clear", type_ "button", attribute "aria-label" "Clear all entities search", onClick (GlobalSearchInput "") ] [ text "✕" ]
              else text "" ]
        , if model.search.globalActivated then viewEntityTypeFilters model else text ""
        , if model.activeTab == ProjectsTab && String.isEmpty (String.trim model.search.globalQuery) then
            viewSearchControls "Filter project hierarchy..." normal |> Html.map WorkspaceSearch
          else text "" ]


viewEntityTypeFilters : Model -> Html Msg
viewEntityTypeFilters model =
    div [ class "filter-bar search-entities-filter-bar", attribute "role" "group", attribute "aria-label" "Search Entities" ]
        [ div [ class "filter-group" ]
            (span [ class "filter-label" ] [ text "Search Entities:" ] ::
                List.map (\kind -> button [ type_ "button", class ("filter-pill" ++ (if List.member kind (selectedEntityTypes model) then " filter-pill-active" else ""))
                    , attribute "aria-pressed" (if List.member kind (selectedEntityTypes model) then "true" else "false")
                    , onClick (ToggleSearchEntityType kind) ] [ text (if kind == "project" then "Projects" else if kind == "task" then "Tasks" else "Observations") ]) (supportedEntityTypes model))
        , if List.isEmpty (selectedEntityTypes model) then span [ attribute "role" "status" ] [ text "Select at least one entity type to search." ] else text "" ]


viewFocusedSearchBar : Model -> Html Msg
viewFocusedSearchBar model =
    if model.activeTab == ProjectsTab && model.focusedFilters /= Nothing && String.isEmpty (String.trim model.search.globalQuery) then
        div [ class "focused-search-filter-bar" ]
            [ div [ class "filter-label" ] [ text "Focused view filters" ]
            , viewSearchControls "Filter focused view..." model ]
    else text ""


viewSearchControls : String -> Model -> Html Msg
viewSearchControls searchPlaceholder model =
    div [ class "search-filter-bar" ]
        [ div [ class "search-bar" ]
            [ input [ class "hierarchy-search-input", type_ "text", placeholder searchPlaceholder, attribute "aria-label" searchPlaceholder, value model.search.query, onInput SearchInput ] []
            , if not (String.isEmpty model.search.query) then
                button [ class "search-clear", onClick (SearchInput ""), type_ "button", attribute "aria-label" "Clear hierarchy filter" ] [ text "✕" ]
              else text "" ]
        , viewFilterBar model ]


viewFilterBar : Model -> Html Msg
viewFilterBar model =
    div [ class "filter-bar" ]
        [ div [ class "filter-group" ]
            [ span [ class "filter-label" ] [ text "Show:" ]
            , viewFilterPill "All" (model.search.filterShowOnly == ShowAll) (SetFilterShowOnly ShowAll)
            , viewFilterPill "Projects" (model.search.filterShowOnly == ShowProjectsOnly) (SetFilterShowOnly ShowProjectsOnly)
            , viewFilterPill "Tasks" (model.search.filterShowOnly == ShowTasksOnly) (SetFilterShowOnly ShowTasksOnly)
            ]
        , label [ class "filter-group filter-checkbox" ]
            [ input [ type_ "checkbox", checked model.search.filterShowEmptyProjects, onCheck SetFilterShowEmptyProjects ] []
            , text "Show empty projects"
            ]
        , div [ class "filter-group" ]
            [ span [ class "filter-label" ] [ text "Project:" ]
            , viewFilterPill "Active" (List.isEmpty model.search.filterProjectStatuses || List.member "active" model.search.filterProjectStatuses) (ToggleFilterProjectStatus "active")
            , viewFilterPill "Paused" (List.isEmpty model.search.filterProjectStatuses || List.member "paused" model.search.filterProjectStatuses) (ToggleFilterProjectStatus "paused")
            , viewFilterPill "Completed" (List.isEmpty model.search.filterProjectStatuses || List.member "completed" model.search.filterProjectStatuses) (ToggleFilterProjectStatus "completed")
            , viewFilterPill "Archived" (List.isEmpty model.search.filterProjectStatuses || List.member "archived" model.search.filterProjectStatuses) (ToggleFilterProjectStatus "archived")
            ]
        , div [ class "filter-group" ]
            [ span [ class "filter-label" ] [ text "Task:" ]
            , viewFilterPill "Todo" (List.isEmpty model.search.filterTaskStatuses || List.member "todo" model.search.filterTaskStatuses) (ToggleFilterTaskStatus "todo")
            , viewFilterPill "In Progress" (List.isEmpty model.search.filterTaskStatuses || List.member "in_progress" model.search.filterTaskStatuses) (ToggleFilterTaskStatus "in_progress")
            , viewFilterPill "Blocked" (List.isEmpty model.search.filterTaskStatuses || List.member "blocked" model.search.filterTaskStatuses) (ToggleFilterTaskStatus "blocked")
            , viewFilterPill "Done" (List.isEmpty model.search.filterTaskStatuses || List.member "done" model.search.filterTaskStatuses) (ToggleFilterTaskStatus "done")
            , viewFilterPill "Cancelled" (List.isEmpty model.search.filterTaskStatuses || List.member "cancelled" model.search.filterTaskStatuses) (ToggleFilterTaskStatus "cancelled")
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
        , viewPreviousResults model
        ]


viewPreviousResults : Model -> Html Msg
viewPreviousResults model =
    case ( model.search.completedRequest, model.search.unifiedResults ) of
        ( Just request, Just results ) ->
            let state = model.search in
            div [ class "search-previous-results" ]
                [ p [ class "search-state-muted" ] [ text ("Showing the last completed results (" ++ String.join ", " request.entityTypes ++ ").") ]
                , viewUnifiedSearchResults { model | search = { state | globalQuery = request.query } } results ]
        _ -> text ""


viewUnifiedSearchError : Model -> String -> Html Msg
viewUnifiedSearchError model message =
    div [ class "unified-search-results search-results-state", attribute "role" "alert" ]
        [ div [ class "search-state-card search-state-error" ]
            [ h3 [] [ text "Search unavailable" ]
            , p [] [ text message ]
            , p [ class "search-state-muted" ] [ text ("Query: " ++ searchQueryLabel model) ]
            , button [ class "btn-small search-result-action", onClick SubmitSearch ] [ text "Retry search" ]
            ]
        , viewPreviousResults model
        ]


viewUnifiedSearchResults : Model -> Api.UnifiedSearchResults -> Html Msg
viewUnifiedSearchResults model results =
    let
        totalCount =
            unifiedSearchResultCount results
    in
    if List.isEmpty (selectedEntityTypes model) then
        div [ class "unified-search-results empty-state", attribute "role" "status" ] [ text "Select at least one entity type to search." ]
    else
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
                 , case result.titleCopyValue of
                    Just value -> Helpers.copyableValue "search-result-title" (if result.entityType == "observation" then "repository subject" else "entity ID") value result.title
                    Nothing -> span [ class "search-result-title", title result.title ] [ text result.title ]
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
    , titleCopyValue = if String.isEmpty (String.trim project.name) then Just project.id else Nothing
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
    , titleCopyValue = if String.isEmpty (String.trim task.title) then Just task.id else Nothing
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
    , titleCopyValue = Just observation.subject
    , title = observation.subject
    , summary = (observation.currentProvenance |> Maybe.map (\provenance -> "Reviewed at " ++ String.left 12 provenance.reviewedGitSha ++ " · ") |> Maybe.withDefault "Current binding unknown · ") ++ nonBlankString observation.contentPreview "No observation content"
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
            String.trim model.search.globalQuery
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


refreshAcceptedSearch : Model -> ( Model, Cmd Msg )
refreshAcceptedSearch model =
    let state = model.search in
    if String.isEmpty (String.trim state.globalQuery) || List.isEmpty (selectedEntityTypes model) || state.globalComposing then
        ( model, Cmd.none )
    else if state.activeRequest /= Nothing then
        ( { model | search = { state | refreshPending = True } }, Cmd.none )
    else if state.submittedQuery /= Just (String.trim state.globalQuery) then
        ( model, Cmd.none )
    else
        startGlobalSearch model
