module Feature.WebSocket exposing
    ( connectCmd
    , init
    , subscriptions
    , update
    )

import Api
import Dict
import Helpers exposing (applyDependencyMutationResult, applyTaskDependencyLinkMutation, beginWorkspaceDataReload)
import Json.Decode as Decode
import Json.Encode as Encode
import Permissions
import Ports exposing (connectWebSocket, wsConnected, wsConnecting, wsConnectionFailed, wsDisconnected, wsMessage)
import Toast exposing (addToast)
import Types exposing (..)


init : WebSocketModel
init =
    { state = Disconnected }


connectCmd : Flags -> Api.SessionContext -> String -> Cmd Msg
connectCmd flags sessionContext workspaceId =
    connectWebSocket
        (Encode.object
            [ ( "url", Encode.string flags.wsUrl )
            , ( "workspaceId", Encode.string workspaceId )
            , ( "sessionId", Encode.string flags.sessionId )
            , ( "authMode", Encode.string sessionContext.authMode )
            , ( "runtimeMode", Encode.string flags.runtimeMode )
            , ( "authTokenPresent", Encode.bool flags.authTokenPresent )
            ]
        )


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ wsConnected (\_ -> WsConnectedMsg)
        , wsConnecting (\_ -> WsConnectingMsg)
        , wsDisconnected (\_ -> WsDisconnectedMsg)
        , wsConnectionFailed WsConnectionFailed
        , wsMessage WsMessageReceived
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WsConnectingMsg ->
            if model.auth.status == AuthReady && Permissions.canReadCurrentWorkspace model then
                ( { model | webSocket = { state = Connecting } }, Cmd.none )

            else
                ( { model | webSocket = { state = Disconnected } }, Cmd.none )

        WsConnectedMsg ->
            if model.auth.status == AuthReady && Permissions.canReadCurrentWorkspace model then
                let
                    currentWebSocket =
                        model.webSocket

                    updatedWebSocket =
                        { currentWebSocket | state = Connected }
                in
                ( { model | webSocket = updatedWebSocket }, Cmd.none )

            else
                ( { model | webSocket = { state = Disconnected } }, Cmd.none )

        WsDisconnectedMsg ->
            if model.auth.status == AuthReady && Permissions.canReadCurrentWorkspace model then
                case model.webSocket.state of
                    Connecting ->
                        ( model, Cmd.none )

                    _ ->
                        addToast Warning "Connection lost. Reconnecting..."
                            { model | webSocket = { state = Connecting } }

            else
                ( { model | webSocket = { state = Disconnected } }, Cmd.none )

        WsConnectionFailed reason ->
            if model.auth.status == AuthReady && Permissions.canReadCurrentWorkspace model then
                let
                    failedModel =
                        { model | webSocket = { state = ConnectionFailed reason } }

                    ( reloadModel, reloadCmd ) =
                        beginWorkspaceDataReload False failedModel

                    message =
                        if String.startsWith "ws-auth:" reason then
                            "WebSocket authentication or workspace access failed; refreshing workspace data"

                        else
                            "WebSocket connection failed; refreshing workspace data"
                in
                addToast Warning message reloadModel
                    |> Tuple.mapSecond (\toastCmd -> Cmd.batch [ reloadCmd, toastCmd ])

            else
                ( { model | webSocket = { state = Disconnected } }, Cmd.none )

        WsMessageReceived raw ->
            if model.auth.status == AuthReady && Permissions.canReadCurrentWorkspace model then
                applyWebSocketChange raw model

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


applyWebSocketChange : String -> Model -> ( Model, Cmd Msg )
applyWebSocketChange raw model =
    case Api.decodeChangeEvent raw of
        Just event ->
            if eventMatchesCurrentWorkspace event model then
                handleChangeEvent event model

            else
                ( model, Cmd.none )

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


eventMatchesCurrentWorkspace : Api.ChangeEvent -> Model -> Bool
eventMatchesCurrentWorkspace event model =
    case event.workspaceId of
        Just eventWsId ->
            model.selectedWorkspaceId == Just eventWsId

        Nothing ->
            case event.entityType of
                Api.EWorkspaceGroup ->
                    Permissions.isSuperadmin model

                _ ->
                    False


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
reloadAfterCascadeDelete : Model -> ( Model, Cmd Msg )
reloadAfterCascadeDelete model =
    let
        dependencies =
            model.dependencies

        cards =
            model.cards

        cacheCleared =
            { model
                | dependencies =
                    { dependencies
                        | taskDependencies = Dict.empty
                        , taskDependencyLinks = []
                        , taskReadinessRollups = Dict.empty
                        , projectReadinessRollups = Dict.empty
                    }
                , cards =
                    { cards
                        | projectNextTasks = Dict.empty
                        , projectNextTaskDiagnostics = Dict.empty
                        , projectNextTasksLoading = Dict.empty
                        , projectNextTaskDiagnosticsLoading = Dict.empty
                        , projectNextTasksErrors = Dict.empty
                        , projectNextTaskDiagnosticsErrors = Dict.empty
                    }
            }
    in
    beginWorkspaceDataReload False cacheCleared


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
                    , Cmd.none
                    )

                _ ->
                    case Maybe.andThen (tryDecode Api.workspaceDecoder) event.payload of
                        Just ws ->
                            ( { model | workspaces = Dict.insert ws.id ws model.workspaces }
                            , Cmd.none
                            )

                        Nothing ->
                            beginWorkspaceDataReload False model

        Api.EProject ->
            case event.changeType of
                Api.Deleted ->
                    reloadAfterCascadeDelete model

                _ ->
                    case Maybe.andThen (tryDecode Api.projectDecoder) event.payload of
                        Just proj ->
                            let
                                updatedModel =
                                    { model | projects = Dict.insert proj.id proj model.projects }
                            in
                            ( updatedModel
                            , refreshReadinessCaches updatedModel
                            )

                        Nothing ->
                            let
                                ( reloadModel, reloadCmd ) =
                                    beginWorkspaceDataReload False model
                            in
                            ( reloadModel, Cmd.batch [ reloadCmd, refreshCachedEntityData reloadModel ] )

        Api.ETask ->
            case event.changeType of
                Api.Deleted ->
                    reloadAfterCascadeDelete model

                _ ->
                    case Maybe.andThen (tryDecode Api.taskDecoder) event.payload of
                        Just task ->
                            let
                                updatedModel =
                                    { model | tasks = Dict.insert task.id task model.tasks }
                            in
                            ( updatedModel
                            , refreshReadinessCaches updatedModel
                            )

                        Nothing ->
                            let
                                ( reloadModel, reloadCmd ) =
                                    beginWorkspaceDataReload False model
                            in
                            ( reloadModel, Cmd.batch [ reloadCmd, refreshCachedEntityData reloadModel ] )

        Api.EMemory ->
            beginWorkspaceDataReload False model

        Api.EObservation ->
            beginWorkspaceDataReload False model

        Api.EMemoryLink ->
            beginWorkspaceDataReload False model

        Api.ECategory ->
            beginWorkspaceDataReload False model

        Api.ETaskDependency ->
            let
                mutationResult =
                    Maybe.andThen (tryDecode Api.dependencyMutationResultDecoder) event.payload

                taskId =
                    mutationResult
                        |> Maybe.map .taskId
                        |> Maybe.withDefault (payloadField "task_id" event.payload |> Maybe.withDefault event.entityId)

                ( patchedModel, reloadCmd ) =
                    case mutationResult of
                        Just result ->
                            let
                                statusPatchedModel =
                                    applyDependencyMutationResult result model

                                dependenciesModel =
                                    statusPatchedModel.dependencies

                                updatedLinks =
                                    applyTaskDependencyLinkMutation result dependenciesModel.taskDependencyLinks
                            in
                            ( { statusPatchedModel | dependencies = { dependenciesModel | taskDependencyLinks = updatedLinks } }, Cmd.none )

                        Nothing ->
                            beginWorkspaceDataReload False model
            in
            ( patchedModel
            , Cmd.batch
                [ Api.fetchTaskOverview model.flags.apiUrl taskId (GotTaskDependencies taskId)
                , refreshTaskReadinessCaches patchedModel
                , refreshProjectReadinessCaches patchedModel
                , reloadCmd
                ]
            )

        Api.ECategoryLink ->
            beginWorkspaceDataReload False model

        Api.ETag ->
            ( model, Cmd.none )

        Api.EWorkspaceGroup ->
            let
                workspaceListLoadToken =
                    model.dataLoading.nextWorkspaceListLoadToken

                currentLoading =
                    model.dataLoading

                loadingModel =
                    { model
                        | workspaces = Dict.empty
                        , dataLoading =
                            { currentLoading
                                | loadingWorkspaces = True
                                , activeWorkspaceListLoadToken = Just workspaceListLoadToken
                                , nextWorkspaceListLoadToken = workspaceListLoadToken + 1
                            }
                    }

                groupCmds =
                    if Permissions.isSuperadmin model then
                        [ Api.fetchWorkspaceGroups model.flags.apiUrl GotWorkspaceGroups ]

                    else
                        []
            in
            ( loadingModel
            , Cmd.batch (Api.fetchWorkspaces model.flags.apiUrl (GotWorkspaces workspaceListLoadToken) :: groupCmds)
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
            True

        Api.EProject ->
            event.changeType == Api.Deleted || payloadField "linked_memory" event.payload /= Nothing || payloadField "unlinked_memory" event.payload /= Nothing

        Api.ETask ->
            event.changeType == Api.Deleted || payloadField "linked_memory" event.payload /= Nothing || payloadField "unlinked_memory" event.payload /= Nothing

        _ ->
            False


refreshCachedEntityData : Model -> Cmd Msg
refreshCachedEntityData model =
    let
        dependencyCmds =
            model.dependencies.taskDependencies
                |> Dict.keys
                |> List.map (\taskId -> Api.fetchTaskOverview model.flags.apiUrl taskId (GotTaskDependencies taskId))

        projectReadinessCmds =
            model.dependencies.projectReadinessRollups
                |> Dict.keys
                |> List.map (\projectId -> Api.fetchProjectOverview model.flags.apiUrl projectId (GotProjectOverview projectId))

        projectNextTaskCmds =
            model.cards.projectNextTasks
                |> Dict.keys
                |> List.map (refreshProjectNextTaskCache model)
    in
    Cmd.batch (dependencyCmds ++ projectReadinessCmds ++ projectNextTaskCmds)


refreshReadinessCaches : Model -> Cmd Msg
refreshReadinessCaches model =
    Cmd.batch
        [ refreshTaskReadinessCaches model
        , refreshProjectReadinessCaches model
        , refreshProjectNextTaskCaches model
        ]


refreshTaskReadinessCaches : Model -> Cmd Msg
refreshTaskReadinessCaches model =
    model.dependencies.taskDependencies
        |> Dict.keys
        |> List.map (\taskId -> Api.fetchTaskOverview model.flags.apiUrl taskId (GotTaskDependencies taskId))
        |> Cmd.batch


refreshProjectReadinessCaches : Model -> Cmd Msg
refreshProjectReadinessCaches model =
    model.dependencies.projectReadinessRollups
        |> Dict.keys
        |> List.map (\projectId -> Api.fetchProjectOverview model.flags.apiUrl projectId (GotProjectOverview projectId))
        |> Cmd.batch


refreshProjectNextTaskCaches : Model -> Cmd Msg
refreshProjectNextTaskCaches model =
    model.cards.projectNextTasks
        |> Dict.keys
        |> List.map (refreshProjectNextTaskCache model)
        |> Cmd.batch


refreshProjectNextTaskCache : Model -> String -> Cmd Msg
refreshProjectNextTaskCache model projectId =
    Cmd.batch
        [ Api.fetchProjectNextTasks model.flags.apiUrl projectId 5 False (GotProjectNextTasks projectId)
        , Api.fetchProjectNextTasks model.flags.apiUrl projectId 200 True (GotProjectNextTaskDiagnostics projectId)
        ]


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

                Api.EObservation ->
                    "Observation"

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
    entity ++ " " ++ action ++ actorSuffix event


actorSuffix : Api.ChangeEvent -> String
actorSuffix event =
    case event.actorLabel of
        Just label ->
            " by " ++ label

        Nothing ->
            ""
