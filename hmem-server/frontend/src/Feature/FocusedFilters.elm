module Feature.FocusedFilters exposing (apply, capture, clearTransientSearchState, enter, initialize, normalSearch, reconcile, stored)

import Dict
import Set
import Types exposing (..)


capture : SearchModel -> HierarchyFilters
capture search =
    { query = search.query, showOnly = search.filterShowOnly, showEmptyProjects = search.filterShowEmptyProjects
    , priority = search.filterPriority, projectStatuses = search.filterProjectStatuses, taskStatuses = search.filterTaskStatuses }


apply : HierarchyFilters -> SearchModel -> SearchModel
apply filters search =
    { search | query = filters.query, filterShowOnly = filters.showOnly, filterShowEmptyProjects = filters.showEmptyProjects
        , filterPriority = filters.priority, filterProjectStatuses = filters.projectStatuses, filterTaskStatuses = filters.taskStatuses }


defaults : HierarchyFilters
defaults =
    { query = "", showOnly = ShowAll, showEmptyProjects = True, priority = AnyPriority
    , projectStatuses = []
    , taskStatuses = [] }


clearTransientSearchState : SearchModel -> SearchModel
clearTransientSearchState search =
    { search | unifiedResults = Nothing, isSearching = False, searchError = Nothing
        , activeRequestQuery = Nothing, activeRequest = Nothing, submittedQuery = Nothing, refreshPending = False }


focusSearch : SearchModel -> SearchModel
focusSearch =
    clearTransientSearchState >> apply defaults


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
    case current model of
        Nothing -> { model | search = loaded }
        Just context ->
            let
                base = capture loaded
                updated = { context | base = base
                    , projectIds = if base == context.base then context.projectIds else Set.empty
                    , taskIds = if base == context.base then context.taskIds else Set.empty }
            in
            { model | search = apply (capture model.search) loaded, focusedFilters = Just updated }


initialize : Model -> Model
initialize model =
    reconcile model model


{-| An explicit focus action starts an inclusive view even when its target is
unchanged. Passive URL echoes use initialize/reconcile and preserve overrides.
The revision lets the router restage demand after this pre-transport reset.
-}
enter : Model -> Model
enter model =
    let normalized = initialize model in
    case current normalized of
        Just context ->
            { normalized | search = focusSearch normalized.search
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
                , search = if context.target /= model.focus.focusedEntity then focusSearch model.search else model.search }

        ( True, Nothing, Just workspaceId ) ->
            { model | focusedFilters = Just { workspaceId = workspaceId, sessionEpoch = model.sessionRequestEpoch, target = model.focus.focusedEntity, entryRevision = 0, base = capture model.search
                    , projectIds = model.dataLoading.navigationVisibleProjectIds, taskIds = model.dataLoading.navigationVisibleTaskIds }
                , search = focusSearch model.search }

        ( False, Just context, _ ) ->
            let
                receipt = pruned context
                loading = model.dataLoading
            in
            { model | search = apply context.base (clearTransientSearchState model.search), focusedFilters = Nothing
                , dataLoading = { loading | navigationVisibleProjectIds = receipt.projectIds, navigationVisibleTaskIds = receipt.taskIds } }

        _ ->
            { model | focusedFilters = Nothing }
