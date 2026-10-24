module SessionTranscript exposing (Block, Item(..), Piece, project)

{-| Presentation of the current bounded record window. Keys retain source block
positions; original records and byte offsets are unchanged.
-}

import Json.Decode as Decode
import Types exposing (SessionRecord)


type alias Block =
    { kind : String, content : String, label : String, tokens : List ( String, String ) }


type alias Piece =
    { key : String, record : SessionRecord, blocks : List Block, activity : Bool }


type Item
    = Message Piece
    | Activity (List Piece)


project : List SessionRecord -> List Item
project records =
    records
        |> List.concatMap pieces
        |> List.foldr
            (\piece rest ->
                if piece.activity then
                    case rest of
                        Activity run :: tail ->
                            Activity (piece :: run) :: tail

                        _ ->
                            Activity [ piece ] :: rest

                else
                    Message piece :: rest
            )
            []


pieces : SessionRecord -> List Piece
pieces record =
    if record.recordType == "unknown" then
        []

    else if record.recordType /= "conversation" then
        if String.trim record.content == "" then
            []

        else
            [ { key = String.fromInt record.presentationOffset ++ ":0", record = record, blocks = [ { kind = "text", content = record.content, label = "", tokens = [] } ], activity = False } ]

    else
        Decode.decodeString (Decode.list blockDecoder) record.content
            |> Result.withDefault []
            |> List.indexedMap Tuple.pair
            |> List.filter (Tuple.second >> visible)
            |> List.foldr
                (\( index, block ) rest ->
                    let
                        piece = { key = String.fromInt record.presentationOffset ++ ":" ++ String.fromInt index, record = record, blocks = [ block ], activity = block.kind /= "text" }
                    in
                    case rest of
                        next :: tail ->
                            if piece.activity == next.activity then
                                { piece | blocks = block :: next.blocks } :: tail

                            else
                                piece :: rest

                        [] ->
                            [ piece ]
                )
                []


visible : Block -> Bool
visible block =
    case block.kind of
        "encrypted" -> True
        "tool" -> String.trim block.content /= "" || String.trim block.label /= ""
        "result" -> String.trim block.content /= "" || String.trim block.label /= ""
        "thinking" -> String.trim block.content /= ""
        "text" -> String.trim block.content /= ""
        _ -> False


blockDecoder : Decode.Decoder Block
blockDecoder =
    Decode.map4 Block
        (Decode.field "kind" Decode.string)
        (Decode.field "text" Decode.string)
        (Decode.field "label" Decode.string)
        (Decode.oneOf [ Decode.field "tokens" (Decode.list (Decode.map2 Tuple.pair (Decode.field "kind" Decode.string) (Decode.field "text" Decode.string))), Decode.succeed [] ])
