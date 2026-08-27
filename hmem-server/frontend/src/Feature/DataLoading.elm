module Feature.DataLoading exposing (acceptWorkspaceLoad, finishWorkspaceLoad, init, listObservationResponseMatches, mergeObservationPage, nextPageOffset, observationResponseMatches, prepareForPageLoad, update)

import Api
import Dict
import Feature.Observation
import Helpers exposing (indexBy)
import Permissions
import Toast exposing (addToast)
import Types exposing (..)


init : DataLoadingModel
init =
    { loadingWorkspaces = True
    , activeWorkspaceListLoadToken = Nothing
    , nextWorkspaceListLoadToken = 1
    , loadingWorkspaceData = False
    , pendingWorkspaceLoads = 0
    , activeWorkspaceLoadToken = Nothing
    , nextWorkspaceLoadToken = 1
    , cardHydrationLoaded = False
    }


prepareForPageLoad : Page -> DataLoadingModel -> DataLoadingModel
prepareForPageLoad page dataLoading =
    { dataLoading
        | loadingWorkspaceData =
            case page of
                WorkspacePage _ ->
                    True

                _ ->
                    False
        , pendingWorkspaceLoads =
            case page of
                WorkspacePage _ ->
                    0

                _ ->
                    0
        , activeWorkspaceLoadToken =
            case page of
                WorkspacePage _ ->
                    Just dataLoading.nextWorkspaceLoadToken

                _ ->
                    Nothing
        , cardHydrationLoaded = False
        , nextWorkspaceLoadToken =
            case page of
                WorkspacePage _ ->
                    dataLoading.nextWorkspaceLoadToken + 1

                _ ->
                    dataLoading.nextWorkspaceLoadToken
    }


finishWorkspaceLoad : Maybe Int -> DataLoadingModel -> DataLoadingModel
finishWorkspaceLoad maybeToken dataLoading =
    let
        remaining =
            case maybeToken of
                Just token ->
                    if dataLoading.activeWorkspaceLoadToken == Just token then
                        max 0 (dataLoading.pendingWorkspaceLoads - 1)

                    else
                        dataLoading.pendingWorkspaceLoads

                Nothing ->
                    dataLoading.pendingWorkspaceLoads
    in
    { dataLoading
        | pendingWorkspaceLoads = remaining
        , loadingWorkspaceData = dataLoading.loadingWorkspaceData && remaining > 0
        , activeWorkspaceLoadToken =
            if remaining == 0 then
                Nothing

            else
                dataLoading.activeWorkspaceLoadToken
        , cardHydrationLoaded = remaining == 0
    }


addInitialHydrationWork : Maybe Int -> Int -> DataLoadingModel -> DataLoadingModel
addInitialHydrationWork maybeToken count dataLoading =
    if count > 0 && acceptWorkspaceLoad maybeToken dataLoading then
        { dataLoading | pendingWorkspaceLoads = dataLoading.pendingWorkspaceLoads + count, loadingWorkspaceData = True, cardHydrationLoaded = False }

    else
        dataLoading


mergeTaskDependencyLinks : String -> List Api.TaskDependencySummary -> List Api.WorkspaceTaskDependencyLink -> List Api.WorkspaceTaskDependencyLink
mergeTaskDependencyLinks taskId summaries links =
    let
        withoutTask =
            List.filter (\link -> link.taskId /= taskId) links
    in
    withoutTask ++ List.map (\summary -> { taskId = taskId, dependsOnId = summary.id }) summaries


acceptWorkspaceLoad : Maybe Int -> DataLoadingModel -> Bool
acceptWorkspaceLoad maybeToken dataLoading =
    case maybeToken of
        Just token ->
            dataLoading.activeWorkspaceLoadToken == Just token

        Nothing ->
            True


maxWorkspacePageOffset : Int
maxWorkspacePageOffset =
    10000


nextPageOffset : Int -> Api.PaginatedResult a -> Maybe Int
nextPageOffset offset paginated =
    let
        nextOffset =
            offset + List.length paginated.items
    in
    if paginated.hasMore && not (List.isEmpty paginated.items) && nextOffset <= maxWorkspacePageOffset then
        Just nextOffset

    else
        Nothing


mergePageById : Int -> List { item | id : String } -> Dict.Dict String { item | id : String } -> Dict.Dict String { item | id : String }
mergePageById offset items existing =
    let
        pageItems =
            indexBy .id items
    in
    if offset == 0 then
        pageItems

    else
        Dict.union pageItems existing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotWorkspaces token result ->
            if model.auth.status /= AuthReady || model.dataLoading.activeWorkspaceListLoadToken /= Just token then
                ( model, Cmd.none )

            else
                case result of
                    Ok paginated ->
                        let
                            currentDataLoading =
                                model.dataLoading

                            updatedDataLoading =
                                { currentDataLoading | loadingWorkspaces = False, activeWorkspaceListLoadToken = Nothing }
                        in
                        ( { model
                            | workspaces = indexBy .id paginated.items
                            , dataLoading = updatedDataLoading
                          }
                        , Cmd.none
                        )

                    Err _ ->
                        let
                            currentDataLoading =
                                model.dataLoading

                            updatedDataLoading =
                                { currentDataLoading | loadingWorkspaces = False, activeWorkspaceListLoadToken = Nothing }
                        in
                        addToast Error "Failed to load workspaces"
                            { model | dataLoading = updatedDataLoading }

        GotWorkspace expectedWsId token result ->
            if model.auth.status /= AuthReady || model.selectedWorkspaceId /= Just expectedWsId || not (Permissions.canReadCurrentWorkspace model) || model.dataLoading.activeWorkspaceLoadToken /= Just token then
                ( model, Cmd.none )

            else
                case result of
                    Ok workspace ->
                        if workspace.id == expectedWsId then
                            let
                                isRepository =
                                    workspace.workspaceType == Api.Repository

                                currentLoading =
                                    model.dataLoading

                                dataLoading =
                                    { currentLoading
                                        | loadingWorkspaceData = True
                                        , pendingWorkspaceLoads = if isRepository then 3 else 2
                                    }

                                currentObservations =
                                    model.observations

                                observations =
                                    if isRepository then
                                        Feature.Observation.startReload expectedWsId currentObservations

                                    else
                                        { currentObservations
                                            | items = Dict.empty
                                            , orderedIds = []
                                            , hasMore = False
                                            , loading = False
                                            , error = Nothing
                                            , expectedOffset = Nothing
                                            , nextOffset = 0
                                        }

                                commands =
                                    [ Api.fetchProjects model.flags.apiUrl expectedWsId (GotProjects expectedWsId (Just token) 0)
                                    , Api.fetchTasks model.flags.apiUrl expectedWsId (GotTasks expectedWsId (Just token) 0)
                                    ]
                                        ++ (if isRepository then
                                                [ Api.fetchObservations model.flags.apiUrl (Feature.Observation.listQuery expectedWsId 0 observations)
                                                    (GotObservations expectedWsId (Just token) observations.requestGeneration observations.queryFingerprint 0)
                                                ]

                                            else
                                                []
                                           )
                            in
                            let
                                loadedModel =
                                    { model
                                        | workspaces = Dict.insert workspace.id workspace model.workspaces
                                        , dataLoading = dataLoading
                                        , observations = observations
                                    }

                                ( detailModel, detailCmd ) =
                                    if isRepository then
                                        case observations.selectedId of
                                            Just observationId ->
                                                Feature.Observation.selectObservation observationId loadedModel

                                            Nothing ->
                                                ( loadedModel, Cmd.none )

                                    else
                                        ( loadedModel, Cmd.none )
                            in
                            ( detailModel, Cmd.batch (detailCmd :: commands) )

                        else
                            ( model, Cmd.none )

                    Err _ ->
                        let
                            currentLoading =
                                model.dataLoading

                            updatedLoading =
                                { currentLoading
                                    | loadingWorkspaceData = False
                                    , pendingWorkspaceLoads = 0
                                    , activeWorkspaceLoadToken = Nothing
                                }
                        in
                        addToast Error "Failed to load workspace" { model | dataLoading = updatedLoading }

        GotProjects wsId maybeToken offset result ->
            if model.selectedWorkspaceId /= Just wsId then
                ( model, Cmd.none )

            else if not (acceptWorkspaceLoad maybeToken model.dataLoading) then
                ( model, Cmd.none )

            else
                case result of
                    Ok paginated ->
                        let
                            updatedProjects =
                                mergePageById offset paginated.items model.projects

                            modelWithPage =
                                { model | projects = updatedProjects }
                        in
                        case nextPageOffset offset paginated of
                            Just nextOffset ->
                                ( modelWithPage
                                , Api.fetchProjectsPage model.flags.apiUrl wsId nextOffset (GotProjects wsId maybeToken nextOffset)
                                )

                            Nothing ->
                                let
                                    completedLoading =
                                        finishWorkspaceLoad maybeToken model.dataLoading

                                    projectIds =
                                        Dict.keys updatedProjects

                                    updatedDataLoading =
                                        addInitialHydrationWork maybeToken (List.length projectIds) completedLoading
                                in
                                ( { modelWithPage | dataLoading = updatedDataLoading }
                                , projectIds
                                    |> List.map (\projectId -> Api.fetchProjectOverview model.flags.apiUrl projectId (GotInitialProjectOverview wsId (Maybe.withDefault -1 maybeToken) projectId))
                                    |> Cmd.batch
                                )

                    Err _ ->
                        let
                            currentDataLoading =
                                model.dataLoading

                            updatedDataLoading =
                                finishWorkspaceLoad maybeToken currentDataLoading
                        in
                        addToast Error "Failed to load projects"
                            { model | dataLoading = updatedDataLoading }

        GotTasks wsId maybeToken offset result ->
            if model.selectedWorkspaceId /= Just wsId then
                ( model, Cmd.none )

            else if not (acceptWorkspaceLoad maybeToken model.dataLoading) then
                ( model, Cmd.none )

            else
                case result of
                    Ok paginated ->
                        let
                            updatedTasks =
                                mergePageById offset paginated.items model.tasks

                            modelWithPage =
                                { model | tasks = updatedTasks }
                        in
                        case nextPageOffset offset paginated of
                            Just nextOffset ->
                                ( modelWithPage
                                , Api.fetchTasksPage model.flags.apiUrl wsId nextOffset (GotTasks wsId maybeToken nextOffset)
                                )

                            Nothing ->
                                let
                                    completedLoading =
                                        finishWorkspaceLoad maybeToken model.dataLoading

                                    taskIds =
                                        Dict.keys updatedTasks

                                    updatedDataLoading =
                                        addInitialHydrationWork maybeToken (List.length taskIds) completedLoading
                                in
                                ( { modelWithPage | dataLoading = updatedDataLoading }
                                , taskIds
                                    |> List.map (\taskId -> Api.fetchTaskOverview model.flags.apiUrl taskId (GotInitialTaskOverview wsId (Maybe.withDefault -1 maybeToken) taskId))
                                    |> Cmd.batch
                                )

                    Err _ ->
                        let
                            updatedDataLoading =
                                finishWorkspaceLoad maybeToken model.dataLoading
                        in
                        addToast Error "Failed to load tasks" { model | dataLoading = updatedDataLoading }

        GotInitialTaskOverview wsId token taskId result ->
            if model.selectedWorkspaceId /= Just wsId || not (acceptWorkspaceLoad (Just token) model.dataLoading) || not (Permissions.canReadCurrentWorkspace model) || not (Dict.member taskId model.tasks) then
                ( model, Cmd.none )

            else
                let
                    completedLoading =
                        finishWorkspaceLoad (Just token) model.dataLoading

                    dependencies =
                        model.dependencies

                    updatedDependencies =
                        case result of
                            Ok overview ->
                                { dependencies
                                    | taskDependencies = Dict.insert taskId overview.dependencies dependencies.taskDependencies
                                    , taskReadinessRollups = Dict.insert taskId overview.readinessRollup dependencies.taskReadinessRollups
                                    , taskDependencyLinks = mergeTaskDependencyLinks taskId overview.dependencies dependencies.taskDependencyLinks
                                }

                            Err _ ->
                                dependencies
                in
                ( { model | dependencies = updatedDependencies, dataLoading = completedLoading }, Cmd.none )

        GotInitialProjectOverview wsId token projectId result ->
            if model.selectedWorkspaceId /= Just wsId || not (acceptWorkspaceLoad (Just token) model.dataLoading) || not (Permissions.canReadCurrentWorkspace model) || not (Dict.member projectId model.projects) then
                ( model, Cmd.none )

            else
                let
                    completedLoading =
                        finishWorkspaceLoad (Just token) model.dataLoading

                    dependencies =
                        model.dependencies

                    updatedDependencies =
                        case result of
                            Ok overview ->
                                { dependencies | projectReadinessRollups = Dict.insert projectId overview.readinessRollup dependencies.projectReadinessRollups }

                            Err _ ->
                                dependencies
                in
                ( { model | dependencies = updatedDependencies, dataLoading = completedLoading }, Cmd.none )

        GotObservations wsId maybeToken generation fingerprint offset result ->
            if model.selectedWorkspaceId /= Just wsId || not (acceptWorkspaceLoad maybeToken model.dataLoading) || not (listObservationResponseMatches generation fingerprint offset model.observations) then
                ( model, Cmd.none )

            else
                case result of
                    Ok paginated ->
                        let
                            currentObservations =
                                model.observations

                            observations =
                                mergeObservationPage offset paginated currentObservations

                            updatedDataLoading =
                                finishWorkspaceLoad maybeToken model.dataLoading
                        in
                        ( { model | observations = observations, dataLoading = updatedDataLoading }, Cmd.none )

                    Err _ ->
                        let
                            currentObservations =
                                model.observations

                            observations =
                                { currentObservations | loading = False, error = Just "Failed to load observations.", expectedOffset = Nothing }

                            updatedDataLoading =
                                finishWorkspaceLoad maybeToken model.dataLoading
                        in
                        ( { model | observations = observations, dataLoading = updatedDataLoading }, Cmd.none )

        _ ->
            ( model, Cmd.none )


observationResponseMatches : Int -> String -> Int -> ObservationModel -> Bool
observationResponseMatches generation fingerprint offset observations =
    observations.requestGeneration == generation
        && observations.queryFingerprint == fingerprint
        && observations.expectedOffset == Just offset


listObservationResponseMatches : Int -> String -> Int -> ObservationModel -> Bool
listObservationResponseMatches generation fingerprint offset observations =
    observations.requestMode == ObservationListMode
        && observationResponseMatches generation fingerprint offset observations


mergeObservationPage : Int -> Api.PaginatedResult Api.Observation -> ObservationModel -> ObservationModel
mergeObservationPage offset paginated observations =
    let
        receivedIds =
            List.map .id paginated.items

        orderedIds =
            if offset == 0 then
                receivedIds

            else
                observations.orderedIds ++ List.filter (\observationId -> not (List.member observationId observations.orderedIds)) receivedIds
    in
    { observations
        | items = mergePageById offset paginated.items observations.items
        , orderedIds = orderedIds
        , hasMore = paginated.hasMore
        , loading = False
        , error = Nothing
        , expectedOffset = Nothing
        , nextOffset = offset + List.length paginated.items
    }
