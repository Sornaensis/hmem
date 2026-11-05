module SessionTranscriptTest exposing (tests)

import Expect
import Json.Encode as Encode
import Dict
import SessionTranscript as Transcript exposing (Item(..))
import Test exposing (..)
import Types exposing (SessionRecord)
import Html
import Markdown.Parser
import Markdown.Renderer
import Test.Html.Query as Query
import Test.Html.Selector as Selector


tests : Test
tests =
    describe "Transcript activity runs"
        [ test "a supported indivisible leaf can use more than one slice of the whole budget" <| \_ ->
            let rendered=Transcript.renderWindow 100000 0 "" [Transcript.atom 45000 (\_ -> Html.text "large supported leaf")] in
            Expect.all [\_ -> Expect.equal False rendered.error,\_ -> Html.div [] rendered.nodes |> Query.fromHtml |> Query.has [Selector.text "large supported leaf"]] ()
        , test "admitted fragments and their omitted ranges retain source order" <| \_ ->
            let
                fragments = List.range 0 6 |> List.map (\index -> Transcript.atom 9000 (\_ -> Html.text (String.fromInt index)))
                rendered = Transcript.renderWindow 30000 3 "" fragments
            in
            Expect.equal { keys = [ "gap:0:1", "2", "3", "gap:3:6" ], part = 3, more = True, error = False }
                { keys = rendered.keys, part = rendered.part, more = rendered.more, error = rendered.error }
        , test "exact resolved qualification fixtures retain the original whole-page continuation bound" <| \_ ->
            let
                referenceDestination="https://example.test/" ++ String.repeat 8400 "x"
                structuralDestination="https://example.test/" ++ String.repeat 8000 "x"
                examples=["[r]: " ++ referenceDestination ++ "\n\n" ++ String.join "\n\n" (List.repeat 1000 "[x][r]"),"7. " ++ String.repeat 300 "[x][r]" ++ "\n8. " ++ String.repeat 300 "[x][r]" ++ "\n9. " ++ String.repeat 300 "[x][r]" ++ "\n\n[r]: " ++ structuralDestination,"| Column A | Column B | Column C |\n| --- | --- | --- |\n| a | " ++ String.repeat 500 "[x][r]" ++ " | c |\n\n[r]: " ++ structuralDestination]
                stamp="[\"10000000-0000-4000-8000-000000000001\",\"1\",\"fixture-user\",\"20000000-0000-4000-8000-000000000002\",\"\"]"
                sourceKey=String.repeat 19 "0" ++ ":0000000"
                wrapper=4096+24*String.length stamp+24*String.length sourceKey+6
                -- The list/table initial-full caller used 2 MiB; 1 MiB here is stricter.
                legacy source=Transcript.renderPage 1048576 -1 [Transcript.container wrapper (Html.div []) [Transcript.container 128 (Html.div []) [Transcript.formattedAst Markdown.Renderer.defaultHtmlRenderer source (Markdown.Parser.parse source |> Result.mapError (always "parse error")),Transcript.atom (2048+12*String.length stamp+12*String.length sourceKey) (\_ -> Html.div [] [])]]]
                results=List.map legacy examples
            in Expect.all [\_ -> Expect.equal [False,False,False] (List.map .error results),\_ -> Expect.equal True (List.all (\result -> result.part<100) results)] ()
        , test "visible prose splits runs while hidden records do not" <| \_ ->
            [ record 1 [ block "thinking" "plan", block "tool" "{}" ]
            , { empty | offset = 2, recordType = "unknown" }
            , record 3 [ block "result" "{}" ]
            , record 4 [ block "text" "visible message" ]
            , record 5 [ block "tool" "{}" ]
            ]
                |> Transcript.project
                |> List.map shape
                |> Expect.equal [ ( "activity", [ "thinking", "tool", "result" ] ), ( "message", [ "text" ] ), ( "activity", [ "tool" ] ) ]
        , test "inspectable events join activity and retain source order around messages" <| \_ ->
            [ record 1 [ block "event" "{\"result\":42}" ]
            , record 2 [ block "thinking" "plan" ]
            , record 3 [ block "text" "visible" ]
            , record 4 [ block "event" "null" ]
            ]
                |> Transcript.project
                |> List.map shape
                |> Expect.equal [ ( "activity", [ "event", "thinking" ] ), ( "message", [ "text" ] ), ( "activity", [ "event" ] ) ]
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
        , test "coalescing keeps exact source block indices across hidden siblings" <| \_ ->
            record 10 [block "text" "first",block "unknown" "hidden",block "text" "later",block "thinking" "reason",block "unknown" "hidden",block "tool" "{}"]
                |> List.singleton
                |> Transcript.project
                |> List.map (\item -> case item of
                    Message piece -> piece.blockIndices
                    Activity pieces -> List.concatMap .blockIndices pieces)
                |> Expect.equal [[0,2],[3,5]]
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
        , test "trimmed activity keeps its source run anchor and cannot merge across an omitted prose boundary" <| \_ ->
            let
                retained = record 30 [ block "tool" "{}" ]
                later = record 90 [ block "tool" "{}" ]
                anchored = { retained | activityAnchors = Dict.singleton "0" "10:0" }
                distinct = { later | activityAnchors = Dict.singleton "0" "80:0" }
            in
            Transcript.project [ anchored, distinct ]
                |> List.map key
                |> Expect.equal [ "10:0", "80:0" ]
        , test "identical prose at separate source occurrences stays separate" <| \_ ->
            [ record 10 [ block "text" "Same message" ], record 20 [ block "text" "Same message" ] ]
                |> Transcript.project
                |> List.map key
                |> Expect.equal [ "10:0", "20:0" ]
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
        Activity pieces -> List.head pieces |> Maybe.map (\piece -> Maybe.withDefault piece.key piece.activityAnchor) |> Maybe.withDefault ""


block : String -> String -> Encode.Value
block kind content =
    Encode.object [ ( "kind", Encode.string kind ), ( "text", Encode.string content ), ( "label", Encode.string "" ) ]


record : Int -> List Encode.Value -> SessionRecord
record offset blocks =
    { empty | offset = offset, presentationOffset = offset, content = Encode.encode 0 (Encode.list identity blocks) }


empty : SessionRecord
empty =
    { offset = 0, end = 1, raw = "unchanged", delimiter = "\r\n", recordType = "conversation", role = "assistant", time = "2026-10-24T10:00:00Z", content = "[]", presentationOffset = 0, activityAnchors = Dict.empty }
