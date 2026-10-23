module Feature.TimestampDisplay exposing (receive, reconcile, replyDecoder, requestCommand)

import Api
import Json.Decode as Decode
import Json.Encode as Encode
import Dict
import Feature.AuditLog
import Feature.AuditDiff
import Feature.Cards
import Feature.Observation
import Feature.Timeline
import Feature.Sessions
import Feature.WorkspaceAdmin
import Permissions
import Ports
import TimestampDisplay
import Types exposing (AuthStatus(..), Model, Msg, Page(..), WorkspaceTab(..))


{-| Consumer collectors contribute only current presentation demand: the same
raw values passed to TimestampDisplay.lookup/dayKey by their views.
-}
sources : Model -> List String
sources model =
    if model.auth.status /= AuthReady then []
    else
        case model.page of
            AuditLogPage ->
                if Permissions.canViewGlobalAudit model then Feature.AuditLog.timestampSources model else []

            WorkspacePage workspaceId ->
                if not (Permissions.canReadCurrentWorkspace model) then []
                else
                    case Dict.get workspaceId model.workspaces of
                        Nothing -> []
                        Just workspace ->
                            let
                                body =
                                    if model.activeTab == AdministrationTab then
                                        Feature.WorkspaceAdmin.timestampSources workspaceId model
                                    else if model.dataLoading.loadingWorkspaceData then []
                                    else
                                        case model.activeTab of
                                            AuditTab ->
                                                if Permissions.canViewCurrentWorkspaceAudit model && model.auditLog.filters.workspaceId == Just workspaceId then
                                                    Feature.AuditLog.timestampSources model
                                                else []
                                            TimelineTab -> Feature.Timeline.timestampSources model.timeline
                                            SessionsTab -> Feature.Sessions.timestampSources model
                                            _ ->
                                                if model.search.isSearching || model.search.searchError /= Nothing || model.search.unifiedResults /= Nothing then []
                                                else if model.activeTab == ObservationsTab then
                                                    if workspace.workspaceType == Api.Repository then Feature.Observation.timestampSources model.observations else []
                                                else Feature.Cards.timestampSources model ++ Feature.Sessions.timestampSources model
                            in
                            [ workspace.createdAt, workspace.updatedAt ] ++ body

            _ -> []


context : Model -> String
context model =
    Encode.encode 0
        (Encode.list identity
            [ Encode.int model.sessionRequestEpoch
            , Encode.string (Maybe.withDefault "" model.selectedWorkspaceId)
            , Encode.string model.url.path
            , Encode.string (Maybe.withDefault "" model.url.query)
            , Encode.string (Maybe.withDefault "" model.url.fragment)
            ]
        )


reconcile : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
reconcile ( model, commands ) =
    let
        ( timestamps, request ) =
            TimestampDisplay.reconcile (context model) (sources model ++ Feature.AuditDiff.timestampSources model) model.timestamps
    in
    ( { model | timestamps = timestamps }
    , Cmd.batch
        [ commands
        , request
            |> Maybe.map requestCommand
            |> Maybe.withDefault Cmd.none
        ]
    )


requestCommand : TimestampDisplay.Request -> Cmd msg
requestCommand batch =
    Ports.formatTimestamps
        (Encode.object
            [ ( "context", Encode.string batch.context )
            , ( "id", Encode.int batch.id )
            , ( "sources", Encode.list Encode.string batch.sources )
            ]
        )


receive : Decode.Value -> Model -> ( Model, Cmd Msg )
receive payload model =
    case Decode.decodeValue replyDecoder payload of
        Ok reply ->
            ( { model | timestamps = TimestampDisplay.receive reply model.timestamps }, Cmd.none )

        Err _ ->
            ( model, Cmd.none )


replyDecoder : Decode.Decoder TimestampDisplay.Reply
replyDecoder =
    Decode.map3 TimestampDisplay.Reply
        (Decode.field "context" Decode.string)
        (Decode.field "id" Decode.int)
        (Decode.field "values"
            (Decode.list
                (Decode.map2 Tuple.pair
                    (Decode.field "source" Decode.string)
                    (Decode.field "local"
                        (Decode.nullable
                            (Decode.map2 TimestampDisplay.Local
                                (Decode.field "text" Decode.string)
                                (Decode.field "day" Decode.string)
                            )
                        )
                    )
                )
            )
        )
