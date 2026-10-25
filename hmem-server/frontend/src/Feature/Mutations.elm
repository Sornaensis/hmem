module Feature.Mutations exposing (init, update)

import Feature.WebSocket
import Helpers
import Dict
import Api
import Helpers exposing (applyTaskMutationResult, beginWorkspaceDataReload, pushUrl, taskMutationResultIds, trackLocalMutation, trackLocalMutations)
import Toast exposing (addToast)
import Types exposing (..)
import String


init : MutationsModel
init =
    { pendingMutationIds = Dict.empty
    , pendingRequestIds = Dict.empty
    , projectOperations = Dict.empty
    , taskOperations = Dict.empty
    , taskAcknowledgements = Dict.empty
    , creationOperations = Dict.empty
    , creationSlots = Dict.empty
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

        ProjectCreated operation result ->
            if not (creationAcknowledgementIsCurrent operation model) then ( model, Cmd.none )
            else
                let
                    completed = completeCreation operation model
                in
                case result of
                    Ok project ->
                        if project.workspaceId /= operation.request.scope.workspaceId then ( completed, Cmd.none )
                        else
                            let
                                accepted = Helpers.planningReadIsCurrent operation.request.revision ("project:" ++ project.id) model
                                canonical = (if accepted then acceptCreatedProject operation project completed else completed) |> Helpers.finishCreationForm operation
                                ( refreshed, refreshCmd ) = Feature.WebSocket.refreshMutationEntities [ ( "project", project.id ) ] (if accepted then [] else [ ( "project", project.id ) ]) canonical
                                ( toasted, toastCmd ) = addToast Success ("Created project: " ++ project.name) refreshed
                            in
                            ( toasted, Cmd.batch [ refreshCmd, toastCmd ] )
                    Err error ->
                        if Helpers.creationOwnsForm operation model then handleApiMutationError "Failed to create project" error completed else ( completed, Cmd.none )

        TaskCreated operation result ->
            if not (creationAcknowledgementIsCurrent operation model) then ( model, Cmd.none )
            else
                let
                    completed = completeCreation operation model
                in
                case result of
                    Ok task ->
                        if task.workspaceId /= operation.request.scope.workspaceId then ( completed, Cmd.none )
                        else
                            let
                                accepted = Helpers.taskPayloadIsCurrent operation.request task model
                                canonical = (if accepted then Helpers.acceptTaskMutationPayload operation.request task completed else completed) |> Helpers.finishCreationForm operation
                                ( refreshed, refreshCmd ) = Feature.WebSocket.refreshMutationEntities [ ( "task", task.id ) ] (if accepted then [] else [ ( "task", task.id ) ]) canonical
                                current = if accepted then markTaskDetailsCurrent [ task ] refreshed else refreshed
                                ( toasted, toastCmd ) = addToast Success ("Created task: " ++ task.title) current
                            in
                            ( toasted, Cmd.batch [ refreshCmd, toastCmd ] )
                    Err error ->
                        if Helpers.creationOwnsForm operation model then handleApiMutationError "Failed to create task" error completed else ( completed, Cmd.none )
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
                    ( toastedModel, Cmd.batch [ trackCmd, toastCmd, pushUrl model.key ("/workspace/" ++ ws.id) ] )

                Err _ ->
                    addToast Error "Failed to create workspace" model

        ProjectUpdated guard result ->
            if not (Helpers.projectRequestIsCurrent guard model) then
                ( model, Cmd.none )

            else
                case result of
                    Ok mutation ->
                        if Just mutation.project.workspaceId /= model.selectedWorkspaceId || guard.targetKey /= "entity:project:" ++ mutation.project.id then
                            ( model, Cmd.none )
                        else
                            let
                                proj = mutation.project
                                tasks = mutation.pauseEffects |> Maybe.map (.tasks >> List.map .task) |> Maybe.withDefault []
                                canonical = List.foldl (\task current -> { current | tasks = Dict.insert task.id task current.tasks })
                                    (markProjectDetailCurrent proj { model | projects = Dict.insert proj.id proj model.projects }) tasks |> markTaskDetailsCurrent tasks
                                ( tracked, trackCmd ) = trackLocalMutations (proj.id :: List.map .id tasks) canonical
                                ( refreshing, refreshCmd ) = Feature.WebSocket.refreshProjectCascade proj.id tracked
                                refreshed = markProjectDetailCurrent proj refreshing |> markTaskDetailsCurrent tasks
                                notice = mutation.pauseEffects |> Maybe.andThen (\effects ->
                                    if effects.remainingOwnPause || effects.remainingAncestorPause then
                                        Just (if effects.remainingAncestorPause then "Project remains paused by an ancestor." else "Project remains independently paused.")
                                    else Nothing)
                                ( toasted, toastCmd ) = notice |> Maybe.map (\message -> addToast Info message refreshed) |> Maybe.withDefault ( refreshed, Cmd.none )
                            in
                            ( toasted, Cmd.batch [ trackCmd, refreshCmd, toastCmd, refreshReadinessCaches toasted ] )

                    Err err ->
                        handleApiMutationError "Failed to update project" err model

        TaskUpdated operation result ->
            if not (Helpers.mutationScopeIsCurrent operation.request.scope model) || Dict.get operation.request.revision model.mutations.taskAcknowledgements /= Just operation then
                ( model, Cmd.none )
            else
                let
                    mutations = model.mutations
                    completed = { model | mutations = { mutations | taskAcknowledgements = Dict.remove operation.request.revision mutations.taskAcknowledgements } }
                in
                case result of
                    Ok mutation ->
                        if mutation.task.id /= operation.taskId || mutation.task.workspaceId /= operation.request.scope.workspaceId then ( completed, Cmd.none )
                        else acceptTaskAcknowledgement operation mutation completed
                    Err error ->
                        if Helpers.taskOperationIsLatest operation model then handleApiMutationError "Failed to update task" error completed
                        else ( completed, Cmd.none )
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

        WorkspaceUpdated requestId result ->
            case result of
                Ok ws ->
                    case model.editing.editState of
                        Just (EditingField state) ->
                            if state.entityType == "workspace" && state.entityId == ws.id && state.requestId == Just requestId then
                                if workspaceGenerationIsCurrent state ws.id model then
                                    let
                                        currentEditing =
                                            model.editing

                                        updatedEditing =
                                            { currentEditing | editState = Nothing }

                                        ( trackedModel, trackCmd ) =
                                            trackLocalMutation ws.id
                                                { model
                                                    | workspaces = Dict.insert ws.id ws model.workspaces
                                                    , editing = updatedEditing
                                                }
                                    in
                                    ( trackedModel, trackCmd )

                                else
                                    -- A newer canonical refetch is already in flight. Its
                                    -- response owns the dictionary, so retire this local edit.
                                    let
                                        currentEditing =
                                            model.editing
                                    in
                                    ( { model | editing = { currentEditing | editState = Nothing } }, Cmd.none )

                            else
                                ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Err err ->
                    case model.editing.editState of
                        Just (EditingField state) ->
                            if state.entityType == "workspace" && state.requestId == Just requestId then
                                let
                                    message = Api.apiErrorToUserMessage "Failed to update workspace" err
                                    currentEditing = model.editing
                                    restoredEditing = { currentEditing | editState = Just (EditingField { state | requestId = Nothing, workspaceGeneration = Nothing, error = Just message }) }
                                    restored =
                                        { model | editing = restoredEditing }
                                in
                                addToast Error message restored

                            else
                                ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

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


markProjectDetailCurrent : Api.Project -> Model -> Model
markProjectDetailCurrent project model =
    let
        loading =
            model.dataLoading

        request =
            { workspaceId = project.workspaceId
            , sessionEpoch = model.sessionRequestEpoch
            , navigationGeneration = loading.navigationGeneration
            , requestId = loading.nextCardDetailRequestId
            , expectedUpdatedAt = project.updatedAt
            , presentationOwner = Nothing
            , inFlight = False
            , succeeded = True
            }
    in
    { model
        | dataLoading =
            { loading
                | projectCardDetailRequests = Dict.insert project.id request loading.projectCardDetailRequests
                , nextCardDetailRequestId = request.requestId + 1
            }
    }


creationAcknowledgementIsCurrent : CreationOperation -> Model -> Bool
creationAcknowledgementIsCurrent operation model =
    Helpers.mutationScopeIsCurrent operation.request.scope model
        && Dict.get operation.request.revision model.mutations.creationOperations == Just operation


completeCreation : CreationOperation -> Model -> Model
completeCreation operation model =
    let mutations = model.mutations
    in
    { model | mutations = { mutations | creationOperations = Dict.remove operation.request.revision mutations.creationOperations } }


acceptCreatedProject : CreationOperation -> Api.Project -> Model -> Model
acceptCreatedProject operation project model =
    let
        loading = model.dataLoading
        summaries = Dict.update project.id (Maybe.map (\previous -> { previous | name = project.name, status = project.status, priority = project.priority, parentId = project.parentId, updatedAt = project.updatedAt })) loading.projectCardSummaries
        summary = Dict.get project.id summaries |> Maybe.map Api.NavigationProjectSummary
        readiness = Dict.get project.id model.dependencies.projectReadinessRollups |> Maybe.map ProjectReadiness
    in
    Helpers.recordPlanningAuthority operation.request.revision ("project:" ++ project.id) summary readiness
        { model | projects = Dict.insert project.id project model.projects, dataLoading = { loading | projectCardSummaries = summaries } }


{-| Acknowledgement completion belongs to the issuing operation. Each returned
entity separately competes with accepted reads and newer entity operations.
-}
acceptTaskAcknowledgement : TaskMutationOperation -> Api.TaskMutationResult -> Model -> ( Model, Cmd Msg )
acceptTaskAcknowledgement operation result model =
    let
        returned = result.task :: List.map .task result.dependencyEffects
        accepted = List.filter (\task -> Helpers.taskPayloadIsCurrent operation.request task model) returned
        rejected = List.filter (\task -> task.workspaceId == operation.request.scope.workspaceId && not (Helpers.taskPayloadIsCurrent operation.request task model)) returned
        canonical = List.foldl (Helpers.acceptTaskMutationPayload operation.request) model accepted
        ( refreshed, refreshCmd ) = Feature.WebSocket.refreshMutationEntities (List.map (\task -> ( "task", task.id )) returned) (List.map (\task -> ( "task", task.id )) rejected) canonical
        current = markTaskDetailsCurrent accepted refreshed
        ( tracked, trackCmd ) = trackLocalMutations (List.map .id accepted) current
    in
    ( tracked, Cmd.batch [ refreshCmd, trackCmd ] )


markTaskDetailsCurrent : List Api.Task -> Model -> Model
markTaskDetailsCurrent tasks model =
    let
        loading =
            model.dataLoading

        mark task ( accumulatedRequests, requestId ) =
            ( Dict.insert task.id
                { workspaceId = task.workspaceId
                , sessionEpoch = model.sessionRequestEpoch
                , navigationGeneration = loading.navigationGeneration
                , requestId = requestId
                , expectedUpdatedAt = task.updatedAt
                , presentationOwner = Nothing
                , inFlight = False
                , succeeded = True
                }
                accumulatedRequests
            , requestId + 1
            )

        ( requests, nextRequestId ) =
            List.foldl mark ( loading.taskCardDetailRequests, loading.nextCardDetailRequestId ) tasks
    in
    { model
        | dataLoading =
            { loading
                | taskCardDetailRequests = requests
                , nextCardDetailRequestId = nextRequestId
            }
    }


refreshAfterMutation : Model -> ( Model, Cmd Msg )
refreshAfterMutation model =
    beginWorkspaceDataReload False model


refreshReadinessCaches : Model -> Cmd Msg
refreshReadinessCaches model =
    Cmd.batch
        [ model.dependencies.taskDependencies
            |> Dict.keys
            |> List.map (\taskId -> Helpers.taskOverviewCmd model taskId)
            |> Cmd.batch
        , model.dependencies.projectReadinessRollups
            |> Dict.keys
            |> List.map (\projectId -> Helpers.projectOverviewCmd model projectId)
            |> Cmd.batch
        , model.cards.projectNextTasks
            |> Dict.keys
            |> List.map
                (\projectId ->
                    Cmd.batch
                        [ Helpers.projectNextTasksCmd model projectId False
                        , Helpers.projectNextTasksCmd model projectId True
                        ]
                )
            |> Cmd.batch
        ]


updateMutationsModel : (MutationsModel -> MutationsModel) -> Model -> Model
updateMutationsModel fn model =
    { model | mutations = fn model.mutations }


workspaceGenerationIsCurrent : { a | workspaceGeneration : Maybe Int } -> String -> Model -> Bool
workspaceGenerationIsCurrent state workspaceId model =
    case state.workspaceGeneration of
        Just submittedGeneration ->
            submittedGeneration
                == (Dict.get
                        ("workspace:" ++ workspaceId ++ "|entity:workspace:" ++ workspaceId)
                        model.webSocket.targetGenerations
                        |> Maybe.withDefault 0
                   )

        Nothing ->
            False


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
