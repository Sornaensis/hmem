module Feature.TimestampDisplay exposing (receive, reconcile)

import Json.Decode as Decode
import Json.Encode as Encode
import Ports
import TimestampDisplay
import Types exposing (Model, Msg)


{-| Consumer collectors contribute only current presentation demand: the same
raw values passed to TimestampDisplay.lookup/dayKey by their views.
-}
sources : Model -> List String
sources _ =
    []


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
            TimestampDisplay.reconcile (context model) (sources model) model.timestamps
    in
    ( { model | timestamps = timestamps }
    , Cmd.batch
        [ commands
        , request
            |> Maybe.map
                (\batch ->
                    Ports.formatTimestamps
                        (Encode.object
                            [ ( "context", Encode.string batch.context )
                            , ( "id", Encode.int batch.id )
                            , ( "sources", Encode.list Encode.string batch.sources )
                            ]
                        )
                )
            |> Maybe.withDefault Cmd.none
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
