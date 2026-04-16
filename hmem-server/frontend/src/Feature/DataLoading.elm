module Feature.DataLoading exposing (update)

import Dict
import Helpers exposing (indexBy)
import Toast exposing (addToast)
import Types exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotWorkspaces result ->
            case result of
                Ok paginated ->
                    ( { model
                        | workspaces = indexBy .id paginated.items
                        , loadingWorkspaces = False
                      }
                    , Cmd.none
                    )

                Err _ ->
                    addToast Error "Failed to load workspaces"
                        { model | loadingWorkspaces = False }

        GotProjects result ->
            case result of
                Ok paginated ->
                    ( { model
                        | projects = indexBy .id paginated.items
                        , loadingWorkspaceData = False
                      }
                    , Cmd.none
                    )

                Err _ ->
                    addToast Error "Failed to load projects"
                        { model | loadingWorkspaceData = False }

        GotTasks result ->
            case result of
                Ok paginated ->
                    ( { model | tasks = indexBy .id paginated.items }, Cmd.none )

                Err _ ->
                    addToast Error "Failed to load tasks" model

        GotMemories result ->
            case result of
                Ok paginated ->
                    ( { model | memories = indexBy .id paginated.items }, Cmd.none )

                Err _ ->
                    addToast Error "Failed to load memories" model

        GotSingleMemory result ->
            case result of
                Ok mem ->
                    ( { model | memories = Dict.insert mem.id mem model.memories }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )
