module Feature.Mutations exposing (update)

import Dict
import Helpers exposing (beginWorkspaceDataReload, trackLocalMutation)
import Toast exposing (addToast)
import Types exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MutationDone entityType result ->
            case result of
                Ok _ ->
                    refreshAfterMutation model

                Err _ ->
                    addToast Error ("Failed to update " ++ entityType) model

        ProjectCreated result ->
            case result of
                Ok proj ->
                    let
                        currentEditing =
                            model.editing

                        updatedEditing =
                            { currentEditing | createForm = Nothing, inlineCreate = Nothing }

                        updatedModel =
                            { model
                                | projects = Dict.insert proj.id proj model.projects
                                , editing = updatedEditing
                            }

                        ( trackedModel, trackCmd ) =
                            trackLocalMutation proj.id updatedModel

                        ( toastedModel, toastCmd ) =
                            addToast Success ("Created project: " ++ proj.name) trackedModel
                    in
                    ( toastedModel, Cmd.batch [ trackCmd, toastCmd ] )

                Err _ ->
                    addToast Error "Failed to create project" model

        TaskCreated result ->
            case result of
                Ok task ->
                    let
                        currentEditing =
                            model.editing

                        updatedEditing =
                            { currentEditing | createForm = Nothing, inlineCreate = Nothing }

                        updatedModel =
                            { model
                                | tasks = Dict.insert task.id task model.tasks
                                , editing = updatedEditing
                            }

                        ( trackedModel, trackCmd ) =
                            trackLocalMutation task.id updatedModel

                        ( toastedModel, toastCmd ) =
                            addToast Success ("Created task: " ++ task.title) trackedModel
                    in
                    ( toastedModel, Cmd.batch [ trackCmd, toastCmd ] )

                Err _ ->
                    addToast Error "Failed to create task" model

        MemoryCreated result ->
            case result of
                Ok mem ->
                    let
                        currentEditing =
                            model.editing

                        updatedEditing =
                            { currentEditing | createForm = Nothing }

                        updatedModel =
                            { model
                                | memories = Dict.insert mem.id mem model.memories
                                , editing = updatedEditing
                            }

                        ( trackedModel, trackCmd ) =
                            trackLocalMutation mem.id updatedModel

                        ( toastedModel, toastCmd ) =
                            addToast Success "Memory created" trackedModel
                    in
                    ( toastedModel, Cmd.batch [ trackCmd, toastCmd ] )

                Err _ ->
                    addToast Error "Failed to create memory" model

        ProjectUpdated result ->
            case result of
                Ok proj ->
                    let
                        ( trackedModel, trackCmd ) =
                            trackLocalMutation proj.id
                                { model | projects = Dict.insert proj.id proj model.projects }
                    in
                    ( trackedModel, trackCmd )

                Err _ ->
                    addToast Error "Failed to update project" model

        TaskUpdated result ->
            case result of
                Ok task ->
                    let
                        ( trackedModel, trackCmd ) =
                            trackLocalMutation task.id
                                { model | tasks = Dict.insert task.id task model.tasks }
                    in
                    ( trackedModel, trackCmd )

                Err _ ->
                    addToast Error "Failed to update task" model

        MemoryUpdated result ->
            case result of
                Ok mem ->
                    let
                        ( trackedModel, trackCmd ) =
                            trackLocalMutation mem.id
                                { model | memories = Dict.insert mem.id mem model.memories }
                    in
                    ( trackedModel, trackCmd )

                Err _ ->
                    addToast Error "Failed to update memory" model

        WorkspaceUpdated result ->
            case result of
                Ok ws ->
                    let
                        ( trackedModel, trackCmd ) =
                            trackLocalMutation ws.id
                                { model | workspaces = Dict.insert ws.id ws model.workspaces }
                    in
                    ( trackedModel, trackCmd )

                Err _ ->
                    addToast Error "Failed to update workspace" model

        _ ->
            ( model, Cmd.none )


refreshAfterMutation : Model -> ( Model, Cmd Msg )
refreshAfterMutation model =
    beginWorkspaceDataReload False model
