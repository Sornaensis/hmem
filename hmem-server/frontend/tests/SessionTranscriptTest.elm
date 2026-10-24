module SessionTranscriptTest exposing (tests)

import Expect
import Json.Encode as Encode
import SessionTranscript as Transcript exposing (Item(..))
import Test exposing (..)
import Types exposing (SessionRecord)


tests : Test
tests =
    describe "Transcript activity runs"
        [ test "visible prose splits runs while hidden records do not" <| \_ ->
            [ record 1 [ block "thinking" "plan", block "tool" "{}" ]
            , { empty | offset = 2, recordType = "unknown" }
            , record 3 [ block "result" "{}" ]
            , record 4 [ block "text" "visible message" ]
            , record 5 [ block "tool" "{}" ]
            ]
                |> Transcript.project
                |> List.map shape
                |> Expect.equal [ ( "activity", [ "thinking", "tool", "result" ] ), ( "message", [ "text" ] ), ( "activity", [ "tool" ] ) ]
        , test "mixed native content keeps its exact order and original block identities" <| \_ ->
            record 10 [ block "text" "before", block "thinking" "plan", block "unknown" "hidden", block "tool" "{}", block "text" "middle", block "result" "{}", block "encrypted" "", block "text" "after" ]
                |> List.singleton
                |> Transcript.project
                |> List.map (\item -> ( key item, shape item ))
                |> Expect.equal
                    [ ( "10:0", ( "message", [ "text" ] ) )
                    , ( "10:1", ( "activity", [ "thinking", "tool" ] ) )
                    , ( "10:4", ( "message", [ "text" ] ) )
                    , ( "10:5", ( "activity", [ "result", "encrypted" ] ) )
                    , ( "10:7", ( "message", [ "text" ] ) )
                    ]
        , test "single activity uses the same run key through append and a closing message" <| \_ ->
            let
                start = [ record 20 [ block "tool" "{}" ] ]
                appended = start ++ [ record 30 [ block "result" "{}" ] ]
                closed = appended ++ [ record 40 [ block "text" "answer" ] ]
            in
            [ start, appended, closed ]
                |> List.map (Transcript.project >> List.head >> Maybe.map key)
                |> Expect.equal [ Just "20:0", Just "20:0", Just "20:0" ]
        , test "empty and unknown blocks do not create groups or separate adjacent activity" <| \_ ->
            [ record 1 [ block "thinking" " ", block "text" "\n", block "unknown" "hidden" ]
            , record 2 [ block "encrypted" "" ]
            , record 3 [ block "thinking" "", block "text" "" ]
            , record 4 [ block "tool" "{}" ]
            ]
                |> Transcript.project
                |> List.map shape
                |> Expect.equal [ ( "activity", [ "encrypted", "tool" ] ) ]
        , test "adjacent prose remains one message and generic text retains its role" <| \_ ->
            [ record 1 [ block "text" "one", block "text" "two" ], { empty | offset = 2, presentationOffset = 2, recordType = "message", role = "user", content = "hello" } ]
                |> Transcript.project
                |> List.map shape
                |> Expect.equal [ ( "message", [ "text", "text" ] ), ( "message", [ "text" ] ) ]
        , test "canonical byte offset stays distinct from its presentation occurrence anchor" <| \_ ->
            let
                canonical = record 50 [ block "thinking" "same occurrence" ]
            in
            Transcript.project [ { canonical | presentationOffset = 10 } ]
                |> List.map (\item -> ( key item, case item of
                    Message piece -> piece.record.offset
                    Activity pieces -> List.head pieces |> Maybe.map (.record >> .offset) |> Maybe.withDefault -1 ))
                |> Expect.equal [ ( "10:0", 50 ) ]
        ]


shape : Item -> ( String, List String )
shape item =
    case item of
        Message piece -> ( "message", List.map .kind piece.blocks )
        Activity pieces -> ( "activity", List.concatMap (.blocks >> List.map .kind) pieces )


key : Item -> String
key item =
    case item of
        Message piece -> piece.key
        Activity pieces -> List.head pieces |> Maybe.map .key |> Maybe.withDefault ""


block : String -> String -> Encode.Value
block kind content =
    Encode.object [ ( "kind", Encode.string kind ), ( "text", Encode.string content ), ( "label", Encode.string "" ) ]


record : Int -> List Encode.Value -> SessionRecord
record offset blocks =
    { empty | offset = offset, presentationOffset = offset, content = Encode.encode 0 (Encode.list identity blocks) }


empty : SessionRecord
empty =
    { offset = 0, end = 1, raw = "unchanged", delimiter = "\r\n", recordType = "conversation", role = "assistant", time = "2026-10-24T10:00:00Z", content = "[]", presentationOffset = 0 }
