module Feature.Mutations exposing (init, update)

import Dict
import Api
import Helpers exposing (beginWorkspaceDataReload, trackLocalMutation)
import Toast exposing (addToast)
import Types exposing (..)
import Browser.Navigation as Nav


init : MutationsModel
init =
    { pendingMutationIds = Dict.empty
    , pendingRequestIds = Dict.empty
    , nextRequestId = 1
    }


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

                Err err ->
                    handleApiMutationError "Failed to create project" err model

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

                Err err ->
                    handleApiMutationError "Failed to create task" err model

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

        WorkspaceCreated result ->
            case result of
                Ok ws ->
                    let
                        currentEditing =
                            model.editing

                        updatedEditing =
                            { currentEditing | createForm = Nothing }

                        updatedModel =
                            { model
                                | workspaces = Dict.insert ws.id ws model.workspaces
                                , editing = updatedEditing
                            }

                        ( trackedModel, trackCmd ) =
                            trackLocalMutation ws.id updatedModel

                        ( toastedModel, toastCmd ) =
                            addToast Success ("Created workspace: " ++ ws.name) trackedModel
                    in
                    ( toastedModel, Cmd.batch [ trackCmd, toastCmd, Nav.pushUrl model.key ("/workspace/" ++ ws.id) ] )

                Err _ ->
                    addToast Error "Failed to create workspace" model

        ProjectUpdated result ->
            case result of
                Ok proj ->
                    let
                        ( trackedModel, trackCmd ) =
                            trackLocalMutation proj.id
                                { model | projects = Dict.insert proj.id proj model.projects }
                    in
                    ( trackedModel, trackCmd )

                Err err ->
                    handleApiMutationError "Failed to update project" err model

        TaskUpdated result ->
            case result of
                Ok task ->
                    let
                        ( trackedModel, trackCmd ) =
                            trackLocalMutation task.id
                                { model | tasks = Dict.insert task.id task model.tasks }
                    in
                    ( trackedModel, trackCmd )

                Err err ->
                    handleApiMutationError "Failed to update task" err model

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

        ClearPendingMutation entityId ->
            ( updateMutationsModel
                (\records -> { records | pendingMutationIds = Dict.remove entityId records.pendingMutationIds })
                model
            , Cmd.none
            )

        ClearPendingRequest requestId ->
            ( updateMutationsModel
                (\records -> { records | pendingRequestIds = Dict.remove requestId records.pendingRequestIds })
                model
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


refreshAfterMutation : Model -> ( Model, Cmd Msg )
refreshAfterMutation model =
    beginWorkspaceDataReload False model


updateMutationsModel : (MutationsModel -> MutationsModel) -> Model -> Model
updateMutationsModel fn model =
    { model | mutations = fn model.mutations }


handleApiMutationError : String -> Api.ApiError -> Model -> ( Model, Cmd Msg )
handleApiMutationError fallback err model =
    let
        ( toastedModel, toastCmd ) =
            addToast Error (Api.apiErrorToUserMessage fallback err) model

        ( reloadedModel, reloadCmd ) =
            if Api.isLifecycleConflict err then
                beginWorkspaceDataReload False toastedModel

            else
                ( toastedModel, Cmd.none )
    in
    ( reloadedModel, Cmd.batch [ toastCmd, reloadCmd ] )
