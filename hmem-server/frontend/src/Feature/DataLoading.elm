module Feature.DataLoading exposing (init, nextPageOffset, prepareForPageLoad, update)

import Api
import Dict
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
                    3

                _ ->
                    0
        , activeWorkspaceLoadToken =
            case page of
                WorkspacePage _ ->
                    Just dataLoading.nextWorkspaceLoadToken

                _ ->
                    Nothing
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
    }


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

        GotWorkspace expectedWsId result ->
            if model.auth.status /= AuthReady || model.selectedWorkspaceId /= Just expectedWsId || not (Permissions.canReadCurrentWorkspace model) then
                ( model, Cmd.none )

            else
                case result of
                    Ok workspace ->
                        if workspace.id == expectedWsId then
                            ( { model | workspaces = Dict.insert workspace.id workspace model.workspaces }
                            , Cmd.none
                            )

                        else
                            ( model, Cmd.none )

                    Err _ ->
                        addToast Error "Failed to load workspace" model

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
                                    updatedDataLoading =
                                        finishWorkspaceLoad maybeToken model.dataLoading
                                in
                                ( { modelWithPage | dataLoading = updatedDataLoading }
                                , Cmd.none
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
                                    updatedDataLoading =
                                        finishWorkspaceLoad maybeToken model.dataLoading
                                in
                                ( { modelWithPage | dataLoading = updatedDataLoading }, Cmd.none )

                    Err _ ->
                        let
                            updatedDataLoading =
                                finishWorkspaceLoad maybeToken model.dataLoading
                        in
                        addToast Error "Failed to load tasks" { model | dataLoading = updatedDataLoading }

        GotMemories wsId maybeToken offset result ->
            if model.selectedWorkspaceId /= Just wsId then
                ( model, Cmd.none )

            else if not (acceptWorkspaceLoad maybeToken model.dataLoading) then
                ( model, Cmd.none )

            else
                case result of
                    Ok paginated ->
                        let
                            updatedMemories =
                                mergePageById offset paginated.items model.memories

                            modelWithPage =
                                { model | memories = updatedMemories }
                        in
                        case nextPageOffset offset paginated of
                            Just nextOffset ->
                                ( modelWithPage
                                , Api.fetchMemoriesPage model.flags.apiUrl wsId nextOffset (GotMemories wsId maybeToken nextOffset)
                                )

                            Nothing ->
                                let
                                    updatedDataLoading =
                                        finishWorkspaceLoad maybeToken model.dataLoading
                                in
                                ( { modelWithPage | dataLoading = updatedDataLoading }, Cmd.none )

                    Err _ ->
                        let
                            updatedDataLoading =
                                finishWorkspaceLoad maybeToken model.dataLoading
                        in
                        addToast Error "Failed to load memories" { model | dataLoading = updatedDataLoading }

        GotSingleMemory result ->
            case result of
                Ok mem ->
                    ( { model | memories = Dict.insert mem.id mem model.memories }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )
