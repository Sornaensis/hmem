module Feature.DataLoading exposing (update)

import Dict
import Helpers exposing (indexBy)
import Toast exposing (addToast)
import Types exposing (..)


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotWorkspaces result ->
            case result of
                Ok paginated ->
                    let
                        currentDataLoading =
                            model.dataLoading

                        updatedDataLoading =
                            { currentDataLoading | loadingWorkspaces = False }
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
                            { currentDataLoading | loadingWorkspaces = False }
                    in
                    addToast Error "Failed to load workspaces"
                        { model | dataLoading = updatedDataLoading }

        GotProjects wsId maybeToken result ->
            if model.selectedWorkspaceId /= Just wsId then
                ( model, Cmd.none )

            else if not (acceptWorkspaceLoad maybeToken model.dataLoading) then
                ( model, Cmd.none )

            else
                case result of
                    Ok paginated ->
                        let
                            currentDataLoading =
                                model.dataLoading

                            updatedDataLoading =
                                finishWorkspaceLoad maybeToken currentDataLoading
                        in
                        ( { model
                            | projects = indexBy .id paginated.items
                            , dataLoading = updatedDataLoading
                          }
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

        GotTasks wsId maybeToken result ->
            if model.selectedWorkspaceId /= Just wsId then
                ( model, Cmd.none )

            else if not (acceptWorkspaceLoad maybeToken model.dataLoading) then
                ( model, Cmd.none )

            else
                case result of
                    Ok paginated ->
                        let
                            updatedDataLoading =
                                finishWorkspaceLoad maybeToken model.dataLoading
                        in
                        ( { model | tasks = indexBy .id paginated.items, dataLoading = updatedDataLoading }, Cmd.none )

                    Err _ ->
                        let
                            updatedDataLoading =
                                finishWorkspaceLoad maybeToken model.dataLoading
                        in
                        addToast Error "Failed to load tasks" { model | dataLoading = updatedDataLoading }

        GotMemories wsId maybeToken result ->
            if model.selectedWorkspaceId /= Just wsId then
                ( model, Cmd.none )

            else if not (acceptWorkspaceLoad maybeToken model.dataLoading) then
                ( model, Cmd.none )

            else
                case result of
                    Ok paginated ->
                        let
                            updatedDataLoading =
                                finishWorkspaceLoad maybeToken model.dataLoading
                        in
                        ( { model | memories = indexBy .id paginated.items, dataLoading = updatedDataLoading }, Cmd.none )

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
