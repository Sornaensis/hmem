module Feature.FocusedFilters exposing (apply, capture, clearTransientSearchState, edit, enter, initialize, normalSearch, reconcile, stored, workspace)

import Dict
import Set
import Types exposing (..)


capture : SearchModel -> HierarchyFilters
capture search =
    { query = search.query, showOnly = search.filterShowOnly, showEmptyProjects = search.filterShowEmptyProjects, hasActiveSession = search.filterHasActiveSession
    , priority = search.filterPriority, projectStatuses = search.filterProjectStatuses, taskStatuses = search.filterTaskStatuses }


apply : HierarchyFilters -> SearchModel -> SearchModel
apply filters search =
    { search | query = filters.query, filterShowOnly = filters.showOnly, filterShowEmptyProjects = filters.showEmptyProjects, filterHasActiveSession = filters.hasActiveSession
        , filterPriority = filters.priority, filterProjectStatuses = filters.projectStatuses, filterTaskStatuses = filters.taskStatuses }


clearTransientSearchState : SearchModel -> SearchModel
clearTransientSearchState search =
    { search | globalQuery = "", globalActivated = False, globalComposing = False, globalGeneration = search.globalGeneration + 1, completedRequest = Nothing, unifiedResults = Nothing, isSearching = False, searchError = Nothing
        , activeRequestQuery = Nothing, activeRequest = Nothing, submittedQuery = Nothing, refreshPending = False }


current : Model -> Maybe FocusedFilterContext
current model =
    model.focusedFilters |> Maybe.andThen (\context ->
        if model.selectedWorkspaceId == Just context.workspaceId && model.sessionRequestEpoch == context.sessionEpoch then Just context else Nothing)


normalSearch : Model -> SearchModel
normalSearch model =
    current model |> Maybe.map (\context -> apply context.base model.search) |> Maybe.withDefault model.search


{-| Storage hydrates the normal context, never deliberate focused overrides.
Changed base filters invalidate the old membership receipt.
-}
stored : SearchModel -> Model -> Model
stored loaded model =
    updateBase False loaded model


workspace : SearchModel -> Model -> Model
workspace loaded model =
    updateBase True loaded model


updateBase : Bool -> SearchModel -> Model -> Model
updateBase preserveFocused loaded model =
    case current model of
        Nothing -> { model | search = loaded }
        Just context ->
            let
                base = capture loaded
                updated = { context | base = base
                    , projectIds = if base == context.base then context.projectIds else Set.empty
                    , taskIds = if base == context.base then context.taskIds else Set.empty }
            in
            { model | search = apply (if preserveFocused || context.edited then capture model.search else base) loaded, focusedFilters = Just updated }


{-| Deliberate hierarchy edits own the focused values, even if they select the
startup defaults again before storage arrives.
-}
edit : Model -> Model
edit model =
    { model | focusedFilters = current model |> Maybe.map (\context -> { context | edited = True }) }


initialize : Model -> Model
initialize model =
    reconcile model model


{-| Explicit focus retires accepted search authority without changing controls.
Passive URL echoes preserve both the controls and current search authority.
-}
enter : Model -> Model
enter model =
    let normalized = initialize model in
    case current normalized of
        Just context ->
            { normalized | search = clearTransientSearchState normalized.search
                , focusedFilters = Just { context | entryRevision = context.entryRevision + 1 } }
        Nothing -> normalized


{-| Focus entry is independent of its navigation source. The receipt contains
only membership IDs, never copied entity metadata or physical request owners.
Any canonical change retires its old membership until filtered revalidation.
-}
reconcile : Model -> Model -> Model
reconcile previous model =
    let
        valid = current model
        focused = model.focus.focusedEntity |> Maybe.map (\( kind, _ ) -> kind == "project" || kind == "task") |> Maybe.withDefault False
        pruned context =
            if previous.projects == model.projects && previous.tasks == model.tasks then context else
            let
                projectIdentity project =
                    ( ( project.parentId, project.status, project.priority ), ( project.name, if context.base.query == "" then Nothing else project.description ) )
                taskIdentity task =
                    ( ( task.parentId, task.projectId, task.status ), ( task.priority, task.title, if context.base.query == "" then Nothing else task.description ) )
                stableProject visiting id =
                    if Set.member id visiting || not (Set.member id context.projectIds) || Maybe.map projectIdentity (Dict.get id previous.projects) /= Maybe.map projectIdentity (Dict.get id model.projects) then False
                    else Dict.get id model.projects |> Maybe.map (\project -> project.parentId |> Maybe.map (stableProject (Set.insert id visiting)) |> Maybe.withDefault True) |> Maybe.withDefault False
                stableTask visiting id =
                    if Set.member id visiting || not (Set.member id context.taskIds) || Maybe.map taskIdentity (Dict.get id previous.tasks) /= Maybe.map taskIdentity (Dict.get id model.tasks) then False
                    else Dict.get id model.tasks |> Maybe.map (\task -> case task.parentId of
                        Just parent -> stableTask (Set.insert id visiting) parent
                        Nothing -> task.projectId |> Maybe.map (stableProject Set.empty) |> Maybe.withDefault True) |> Maybe.withDefault False
                projectIds = Set.filter (stableProject Set.empty) context.projectIds
                taskIds = Set.filter (stableTask Set.empty) context.taskIds
            in
            { context | projectIds = projectIds, taskIds = taskIds }
    in
    case ( focused, valid, model.selectedWorkspaceId ) of
        ( True, Just context, _ ) ->
            let receipt = pruned context in
            { model | focusedFilters = Just { receipt | target = model.focus.focusedEntity }
                , search = if context.target /= model.focus.focusedEntity then clearTransientSearchState model.search else model.search }

        ( True, Nothing, Just workspaceId ) ->
            { model | focusedFilters = Just { workspaceId = workspaceId, sessionEpoch = model.sessionRequestEpoch, target = model.focus.focusedEntity, entryRevision = 0, edited = False, base = capture model.search
                    , projectIds = model.dataLoading.navigationVisibleProjectIds, taskIds = model.dataLoading.navigationVisibleTaskIds }
                , search = clearTransientSearchState model.search }

        ( False, Just context, _ ) ->
            let
                receipt = pruned context
                loading = model.dataLoading
            in
            { model | search = apply context.base (clearTransientSearchState model.search), focusedFilters = Nothing
                , dataLoading = { loading | navigationVisibleProjectIds = receipt.projectIds, navigationVisibleTaskIds = receipt.taskIds } }

        _ ->
            { model | focusedFilters = Nothing }
