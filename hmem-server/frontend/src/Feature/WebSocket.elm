module Feature.WebSocket exposing
    ( connectCmd
    , subscriptions
    , update
    )

import Api
import Dict
import Json.Decode as Decode
import Ports exposing (connectWebSocket, wsConnected, wsDisconnected, wsMessage)
import Toast exposing (addToast)
import Types exposing (..)


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
            ( { model | wsState = Connected }, Cmd.none )

        WsDisconnectedMsg ->
            addToast Warning "Connection lost. Reconnecting..."
                { model | wsState = Disconnected }

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
            Dict.member event.entityId model.pendingMutationIds
    in
    if isSelfEvent then
        ( updatedModel, refreshCmd )

    else
        let
            toastMsg =
                changeEventDescription event

            ( toastedModel, toastCmd ) =
                addToast Info toastMsg updatedModel
        in
        ( toastedModel, Cmd.batch [ refreshCmd, toastCmd ] )


{-| Try to apply the change event payload directly into the model.
Falls back to a full re-fetch when the payload is missing or cannot be decoded.
-}
applyChangeEvent : Api.ChangeEvent -> Model -> ( Model, Cmd Msg )
applyChangeEvent event model =
    case event.entityType of
        Api.EWorkspace ->
            case event.changeType of
                Api.Deleted ->
                    ( { model | workspaces = Dict.remove event.entityId model.workspaces }
                    , Cmd.none
                    )

                _ ->
                    case Maybe.andThen (tryDecode Api.workspaceDecoder) event.payload of
                        Just ws ->
                            ( { model | workspaces = Dict.insert ws.id ws model.workspaces }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model
                            , Api.fetchWorkspaces model.flags.apiUrl GotWorkspaces
                            )

        Api.EProject ->
            case event.changeType of
                Api.Deleted ->
                    ( { model | projects = Dict.remove event.entityId model.projects }
                    , Cmd.none
                    )

                _ ->
                    case Maybe.andThen (tryDecode Api.projectDecoder) event.payload of
                        Just proj ->
                            ( { model | projects = Dict.insert proj.id proj model.projects }
                            , Cmd.none
                            )

                        Nothing ->
                            withWorkspace model <|
                                \wsId -> Api.fetchProjects model.flags.apiUrl wsId GotProjects

        Api.ETask ->
            case event.changeType of
                Api.Deleted ->
                    ( { model | tasks = Dict.remove event.entityId model.tasks }
                    , Cmd.none
                    )

                _ ->
                    case Maybe.andThen (tryDecode Api.taskDecoder) event.payload of
                        Just task ->
                            ( { model | tasks = Dict.insert task.id task model.tasks }
                            , Cmd.none
                            )

                        Nothing ->
                            withWorkspace model <|
                                \wsId -> Api.fetchTasks model.flags.apiUrl wsId GotTasks

        Api.EMemory ->
            case event.changeType of
                Api.Deleted ->
                    ( { model | memories = Dict.remove event.entityId model.memories }
                    , Cmd.none
                    )

                _ ->
                    case Maybe.andThen (tryDecode Api.memoryDecoder) event.payload of
                        Just mem ->
                            ( { model | memories = Dict.insert mem.id mem model.memories }
                            , Cmd.none
                            )

                        Nothing ->
                            withWorkspace model <|
                                \wsId -> Api.fetchMemories model.flags.apiUrl wsId GotMemories

        Api.EMemoryLink ->
            withWorkspace model <|
                \wsId ->
                    case model.page of
                        MemoryGraphPage ->
                            Api.fetchVisualization model.flags.apiUrl wsId GotVisualization

                        _ ->
                            Cmd.none

        Api.ECategory ->
            withWorkspace model <|
                \wsId -> Api.fetchMemories model.flags.apiUrl wsId GotMemories

        Api.ETaskDependency ->
            ( model
            , Api.fetchTaskOverview model.flags.apiUrl event.entityId (GotTaskDependencies event.entityId)
            )

        Api.ECategoryLink ->
            withWorkspace model <|
                \wsId -> Api.fetchMemories model.flags.apiUrl wsId GotMemories

        Api.ETag ->
            ( model
            , Api.fetchMemory model.flags.apiUrl event.entityId GotSingleMemory
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