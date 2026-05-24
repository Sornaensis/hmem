module Feature.Mutations exposing (init, update)

import Dict
import Api
import Helpers exposing (applyTaskMutationResult, beginWorkspaceDataReload, taskMutationResultIds, trackLocalMutation, trackLocalMutations)
import Toast exposing (addToast)
import Types exposing (..)
import Browser.Navigation as Nav
import String


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
                    ( toastedModel, Cmd.batch [ trackCmd, toastCmd, refreshReadinessCaches toastedModel ] )

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
                    ( toastedModel, Cmd.batch [ trackCmd, toastCmd, refreshReadinessCaches toastedModel ] )

                Err err ->
                    handleApiMutationError "Failed to create task" err model

        MemoryCreated result ->
            case result of
                Ok mem ->
                    let
                        currentEditing =
                            model.editing

                        targetEntityId =
                            memoryCreationTargetEntityId currentEditing

                        updatedEditing =
                            { currentEditing | createForm = Nothing, inlineCreate = Nothing }

                        modelWithMemory =
                            { model
                                | memories = Dict.insert mem.id mem model.memories
                                , editing = updatedEditing
                            }

                        updatedModel =
                            case targetEntityId of
                                Just entityId ->
                                    addCreatedMemoryToEntity entityId mem modelWithMemory

                                Nothing ->
                                    modelWithMemory

                        ( trackedModel, trackCmd ) =
                            trackLocalMutation mem.id updatedModel

                        ( toastedModel, toastCmd ) =
                            addToast Success "Memory created" trackedModel
                    in
                    ( toastedModel, Cmd.batch [ trackCmd, toastCmd ] )

                Err err ->
                    handleApiMutationError "Failed to create memory" err model

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
                    ( trackedModel, Cmd.batch [ trackCmd, refreshReadinessCaches trackedModel ] )

                Err err ->
                    handleApiMutationError "Failed to update project" err model

        TaskUpdated result ->
            case result of
                Ok mutationResult ->
                    let
                        updatedModel =
                            applyTaskMutationResult mutationResult model

                        ( trackedModel, trackCmd ) =
                            trackLocalMutations (taskMutationResultIds mutationResult) updatedModel
                    in
                    ( trackedModel, Cmd.batch [ trackCmd, refreshReadinessCaches trackedModel ] )

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


refreshReadinessCaches : Model -> Cmd Msg
refreshReadinessCaches model =
    Cmd.batch
        [ model.dependencies.taskDependencies
            |> Dict.keys
            |> List.map (\taskId -> Api.fetchTaskOverview model.flags.apiUrl taskId (GotTaskDependencies taskId))
            |> Cmd.batch
        , model.dependencies.projectReadinessRollups
            |> Dict.keys
            |> List.map (\projectId -> Api.fetchProjectOverview model.flags.apiUrl projectId (GotProjectOverview projectId))
            |> Cmd.batch
        , model.cards.projectNextTasks
            |> Dict.keys
            |> List.map
                (\projectId ->
                    Cmd.batch
                        [ Api.fetchProjectNextTasks model.flags.apiUrl projectId 5 False (GotProjectNextTasks projectId)
                        , Api.fetchProjectNextTasks model.flags.apiUrl projectId 200 True (GotProjectNextTaskDiagnostics projectId)
                        ]
                )
            |> Cmd.batch
        ]


updateMutationsModel : (MutationsModel -> MutationsModel) -> Model -> Model
updateMutationsModel fn model =
    { model | mutations = fn model.mutations }


memoryCreationTargetEntityId : EditingModel -> Maybe String
memoryCreationTargetEntityId editing =
    case editing.createForm of
        Just (CreateMemoryForm form) ->
            decodeMemoryTargetEntityId form.target

        _ ->
            case editing.inlineCreate of
                Just (InlineCreateMemory form) ->
                    decodeMemoryTargetEntityId form.target

                _ ->
                    Nothing


decodeMemoryTargetEntityId : String -> Maybe String
decodeMemoryTargetEntityId target =
    if String.startsWith "project:" target then
        target
            |> String.dropLeft (String.length "project:")
            |> nonEmptyString

    else if String.startsWith "task:" target then
        target
            |> String.dropLeft (String.length "task:")
            |> nonEmptyString

    else
        Nothing


nonEmptyString : String -> Maybe String
nonEmptyString value =
    if String.isEmpty value then
        Nothing

    else
        Just value


addCreatedMemoryToEntity : String -> Api.Memory -> Model -> Model
addCreatedMemoryToEntity entityId mem model =
    let
        currentMemory =
            model.memory

        currentIds =
            Dict.get entityId currentMemory.entityMemoryIds |> Maybe.withDefault []

        updatedIds =
            if List.member mem.id currentIds then
                currentIds

            else
                currentIds ++ [ mem.id ]

        updatedEntityMemories =
            case Dict.get entityId currentMemory.entityMemories of
                Just memories ->
                    let
                        updatedMemories =
                            if List.any (\existing -> existing.id == mem.id) memories then
                                memories

                            else
                                memories ++ [ mem ]
                    in
                    Dict.insert entityId updatedMemories currentMemory.entityMemories

                Nothing ->
                    currentMemory.entityMemories

        updatedTasks =
            case Dict.get entityId model.tasks of
                Just task ->
                    Dict.insert entityId { task | memoryLinkCount = List.length updatedIds } model.tasks

                Nothing ->
                    model.tasks
    in
    { model
        | memory =
            { currentMemory
                | entityMemoryIds = Dict.insert entityId updatedIds currentMemory.entityMemoryIds
                , entityMemories = updatedEntityMemories
            }
        , tasks = updatedTasks
    }


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
