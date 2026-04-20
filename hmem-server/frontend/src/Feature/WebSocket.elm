module Feature.WebSocket exposing
    ( connectCmd
    , init
    , subscriptions
    , update
    )

import Api
import Dict
import Helpers exposing (beginWorkspaceDataReload)
import Json.Decode as Decode
import Ports exposing (connectWebSocket, wsConnected, wsDisconnected, wsMessage)
import Toast exposing (addToast)
import Types exposing (..)


init : WebSocketModel
init =
    { state = Disconnected }


connectCmd : String -> Cmd Msg
connectCmd wsUrl =
    connectWebSocket wsUrl


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ wsConnected (\_ -> WsConnectedMsg)
        , wsDisconnected (\_ -> WsDisconnectedMsg)
        , wsMessage WsMessageReceived
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WsConnectedMsg ->
            let
                currentWebSocket =
                    model.webSocket

                updatedWebSocket =
                    { currentWebSocket | state = Connected }
            in
            ( { model | webSocket = updatedWebSocket }, Cmd.none )

        WsDisconnectedMsg ->
            let
                currentWebSocket =
                    model.webSocket

                updatedWebSocket =
                    { currentWebSocket | state = Disconnected }
            in
            addToast Warning "Connection lost. Reconnecting..."
                { model | webSocket = updatedWebSocket }

        WsMessageReceived raw ->
            applyWebSocketChange raw model

        _ ->
            ( model, Cmd.none )


applyWebSocketChange : String -> Model -> ( Model, Cmd Msg )
applyWebSocketChange raw model =
    case Api.decodeChangeEvent raw of
        Just event ->
            handleChangeEvent event model

        Nothing ->
            ( model, Cmd.none )


handleChangeEvent : Api.ChangeEvent -> Model -> ( Model, Cmd Msg )
handleChangeEvent event model =
    let
        ( updatedModel, refreshCmd ) =
            applyChangeEvent event model

        isSelfEvent =
            eventTouchesPendingMutation event model.mutations.pendingMutationIds model.mutations.pendingRequestIds
    in
    if isSelfEvent then
        ( updatedModel
        , if requiresSelfRefresh event then
            refreshCmd

          else
            Cmd.none
        )

    else
        let
            toastMsg =
                changeEventDescription event

            ( toastedModel, toastCmd ) =
                addToast Info toastMsg updatedModel
        in
        ( toastedModel, Cmd.batch [ refreshCmd, toastCmd ] )


eventTouchesPendingMutation : Api.ChangeEvent -> Dict.Dict String Bool -> Dict.Dict String Bool -> Bool
eventTouchesPendingMutation event _ pendingRequestIds =
    let
        requestMatched =
            case event.requestId of
                Just requestId ->
                    Dict.member requestId pendingRequestIds

                Nothing ->
                    case payloadField "request_id" event.payload of
                        Just requestId ->
                            Dict.member requestId pendingRequestIds

                        Nothing ->
                            False
    in
    requestMatched


{-| Try to apply the change event payload directly into the model.
Falls back to a full re-fetch when the payload is missing or cannot be decoded.
-}
applyChangeEvent : Api.ChangeEvent -> Model -> ( Model, Cmd Msg )
applyChangeEvent event model =
    case event.entityType of
        Api.EWorkspace ->
            case event.changeType of
                Api.Deleted ->
                    let
                        currentGroups =
                            model.groups

                        updatedGroups =
                            { currentGroups
                                | groupMembers = Dict.map (\_ memberIds -> List.filter ((/=) event.entityId) memberIds) model.groups.groupMembers
                            }
                    in
                    ( { model | workspaces = Dict.remove event.entityId model.workspaces, groups = updatedGroups }
                    , maybeGraphRefresh model
                    )

                _ ->
                    case Maybe.andThen (tryDecode Api.workspaceDecoder) event.payload of
                        Just ws ->
                            ( { model | workspaces = Dict.insert ws.id ws model.workspaces }
                            , maybeGraphRefresh model
                            )

                        Nothing ->
                            ( model
                            , Api.fetchWorkspaces model.flags.apiUrl GotWorkspaces
                            )

        Api.EProject ->
            case event.changeType of
                Api.Deleted ->
                    ( { model | projects = Dict.remove event.entityId model.projects }
                    , maybeGraphRefresh model
                    )

                _ ->
                    case Maybe.andThen (tryDecode Api.projectDecoder) event.payload of
                        Just proj ->
                            ( { model | projects = Dict.insert proj.id proj model.projects }
                            , maybeGraphRefresh model
                            )

                        Nothing ->
                            if payloadField "linked_memory" event.payload /= Nothing || payloadField "unlinked_memory" event.payload /= Nothing then
                                ( model
                                , Cmd.batch
                                    [ Api.fetchProjectMemories model.flags.apiUrl event.entityId (GotEntityMemories event.entityId)
                                    , maybeGraphRefresh model
                                    ]
                                )

                            else
                                let
                                    ( _, reloadCmd ) =
                                        beginWorkspaceDataReload False model
                                in
                                ( model, Cmd.batch [ reloadCmd, refreshCachedEntityData model, maybeGraphRefresh model ] )

        Api.ETask ->
            case event.changeType of
                Api.Deleted ->
                    let
                        currentDependencies =
                            model.dependencies

                        updatedDependencies =
                            { currentDependencies | taskDependencies = Dict.remove event.entityId model.dependencies.taskDependencies }
                    in
                    ( { model | tasks = Dict.remove event.entityId model.tasks, dependencies = updatedDependencies }
                    , Cmd.batch [ maybeGraphRefresh model, refreshTaskDependencyCaches model ]
                    )

                _ ->
                    case Maybe.andThen (tryDecode Api.taskDecoder) event.payload of
                        Just task ->
                            ( { model | tasks = Dict.insert task.id task model.tasks }
                            , Cmd.batch [ maybeGraphRefresh model, refreshTaskDependencyCaches model ]
                            )

                        Nothing ->
                            if payloadField "linked_memory" event.payload /= Nothing || payloadField "unlinked_memory" event.payload /= Nothing then
                                let
                                    ( _, reloadCmd ) =
                                        beginWorkspaceDataReload False model
                                in
                                ( model
                                , Cmd.batch
                                    [ reloadCmd
                                    , Api.fetchTaskMemories model.flags.apiUrl event.entityId (GotEntityMemories event.entityId)
                                    , maybeGraphRefresh model
                                    ]
                                )

                            else
                                let
                                    ( _, reloadCmd ) =
                                        beginWorkspaceDataReload False model
                                in
                                ( model, Cmd.batch [ reloadCmd, refreshCachedEntityData model, maybeGraphRefresh model ] )

        Api.EMemory ->
            case event.changeType of
                Api.Deleted ->
                    ( { model | memories = Dict.remove event.entityId model.memories }
                    , Cmd.batch [ maybeGraphRefresh model, refreshLinkedMemoryCachesFor event.entityId model ]
                    )

                _ ->
                    case Maybe.andThen (tryDecode Api.memoryDecoder) event.payload of
                        Just mem ->
                            ( { model | memories = Dict.insert mem.id mem model.memories }
                            , Cmd.batch [ maybeGraphRefresh model, refreshLinkedMemoryCachesFor mem.id model ]
                            )

                        Nothing ->
                            beginWorkspaceDataReload False model

        Api.EMemoryLink ->
            let
                ( _, reloadCmd ) =
                    beginWorkspaceDataReload False model

                projectId =
                    payloadField "project_id" event.payload

                taskId =
                    payloadField "task_id" event.payload

                graphCmd =
                    case ( model.page, model.selectedWorkspaceId ) of
                        ( MemoryGraphPage, Just wsId ) ->
                            Api.fetchVisualization model.flags.apiUrl wsId GotVisualization

                        _ ->
                            Cmd.none

                memoryCacheCmds =
                    [ projectId |> Maybe.map (\pid -> Api.fetchProjectMemories model.flags.apiUrl pid (GotEntityMemories pid))
                    , taskId |> Maybe.map (\tid -> Api.fetchTaskMemories model.flags.apiUrl tid (GotEntityMemories tid))
                    ]
                        |> List.filterMap identity
            in
            ( model, Cmd.batch (reloadCmd :: graphCmd :: memoryCacheCmds) )

        Api.ECategory ->
            beginWorkspaceDataReload False model

        Api.ETaskDependency ->
            let
                taskId =
                    payloadField "task_id" event.payload |> Maybe.withDefault event.entityId

                graphCmd =
                    case ( model.page, model.selectedWorkspaceId ) of
                        ( MemoryGraphPage, Just wsId ) ->
                            Api.fetchVisualization model.flags.apiUrl wsId GotVisualization

                        _ ->
                            Cmd.none

                ( _, reloadCmd ) =
                    beginWorkspaceDataReload False model
            in
            ( model
            , Cmd.batch
                [ Api.fetchTaskOverview model.flags.apiUrl taskId (GotTaskDependencies taskId)
                , reloadCmd
                , graphCmd
                ]
            )

        Api.ECategoryLink ->
            beginWorkspaceDataReload False model

        Api.ETag ->
            let
                memoryId =
                    payloadField "memory_id" event.payload |> Maybe.withDefault event.entityId
            in
            ( model
            , Cmd.batch
                [ Api.fetchMemory model.flags.apiUrl memoryId GotSingleMemory
                , refreshLinkedMemoryCachesFor memoryId model
                ]
            )

        Api.EWorkspaceGroup ->
            ( model
            , Cmd.batch
                [ Api.fetchWorkspaces model.flags.apiUrl GotWorkspaces
                , Api.fetchWorkspaceGroups model.flags.apiUrl GotWorkspaceGroups
                ]
            )

        Api.ESavedView ->
            ( model, Cmd.none )

        Api.EOther _ ->
            ( model, Cmd.none )


withWorkspace : Model -> (String -> Cmd Msg) -> ( Model, Cmd Msg )
withWorkspace model mkCmd =
    case model.selectedWorkspaceId of
        Just wsId ->
            ( model, mkCmd wsId )

        Nothing ->
            ( model, Cmd.none )


tryDecode : Decode.Decoder a -> Decode.Value -> Maybe a
tryDecode decoder val =
    Result.toMaybe (Decode.decodeValue decoder val)


payloadField : String -> Maybe Decode.Value -> Maybe String
payloadField fieldName payload =
    payload
        |> Maybe.andThen
            (\value ->
                Decode.decodeValue (Decode.field fieldName Decode.string) value
                    |> Result.toMaybe
            )


requiresSelfRefresh : Api.ChangeEvent -> Bool
requiresSelfRefresh event =
    case event.entityType of
        Api.ECategory ->
            True

        Api.ETag ->
            True

        Api.ETaskDependency ->
            True

        Api.EMemoryLink ->
            True

        Api.EMemory ->
            event.changeType == Api.Deleted

        Api.EProject ->
            payloadField "linked_memory" event.payload /= Nothing || payloadField "unlinked_memory" event.payload /= Nothing

        Api.ETask ->
            event.changeType == Api.Deleted || payloadField "linked_memory" event.payload /= Nothing || payloadField "unlinked_memory" event.payload /= Nothing

        _ ->
            False


maybeGraphRefresh : Model -> Cmd Msg
maybeGraphRefresh model =
    case ( model.page, model.selectedWorkspaceId ) of
        ( MemoryGraphPage, Just wsId ) ->
            Api.fetchVisualization model.flags.apiUrl wsId GotVisualization

        _ ->
            Cmd.none


refreshLinkedMemoryCachesFor : String -> Model -> Cmd Msg
refreshLinkedMemoryCachesFor memoryId model =
    model.memory.entityMemories
        |> Dict.toList
        |> List.filterMap
            (\( entityId, mems ) ->
                if List.any (\mem -> mem.id == memoryId) mems then
                    if Dict.member entityId model.projects then
                        Just (Api.fetchProjectMemories model.flags.apiUrl entityId (GotEntityMemories entityId))

                    else if Dict.member entityId model.tasks then
                        Just (Api.fetchTaskMemories model.flags.apiUrl entityId (GotEntityMemories entityId))

                    else
                        Nothing

                else
                    Nothing
            )
        |> Cmd.batch


refreshCachedEntityData : Model -> Cmd Msg
refreshCachedEntityData model =
    let
        entityMemoryCmds =
            model.memory.entityMemories
                |> Dict.keys
                |> List.filterMap
                    (\entityId ->
                        if Dict.member entityId model.projects then
                            Just (Api.fetchProjectMemories model.flags.apiUrl entityId (GotEntityMemories entityId))

                        else if Dict.member entityId model.tasks then
                            Just (Api.fetchTaskMemories model.flags.apiUrl entityId (GotEntityMemories entityId))

                        else
                            Nothing
                    )

        dependencyCmds =
            model.dependencies.taskDependencies
                |> Dict.keys
                |> List.map (\taskId -> Api.fetchTaskOverview model.flags.apiUrl taskId (GotTaskDependencies taskId))
    in
    Cmd.batch (entityMemoryCmds ++ dependencyCmds)


refreshTaskDependencyCaches : Model -> Cmd Msg
refreshTaskDependencyCaches model =
    model.dependencies.taskDependencies
        |> Dict.keys
        |> List.map (\taskId -> Api.fetchTaskOverview model.flags.apiUrl taskId (GotTaskDependencies taskId))
        |> Cmd.batch


changeEventDescription : Api.ChangeEvent -> String
changeEventDescription event =
    let
        action =
            case event.changeType of
                Api.Created ->
                    "created"

                Api.Updated ->
                    "updated"

                Api.Deleted ->
                    "deleted"

        entity =
            case event.entityType of
                Api.EWorkspace ->
                    "Workspace"

                Api.EProject ->
                    "Project"

                Api.ETask ->
                    "Task"

                Api.EMemory ->
                    "Memory"

                Api.EMemoryLink ->
                    "Memory link"

                Api.ECategory ->
                    "Category"

                Api.EWorkspaceGroup ->
                    "Workspace group"

                Api.ESavedView ->
                    "Saved view"

                Api.ETaskDependency ->
                    "Task dependency"

                Api.ECategoryLink ->
                    "Category link"

                Api.ETag ->
                    "Tags"

                Api.EOther s ->
                    s
    in
    entity ++ " " ++ action
