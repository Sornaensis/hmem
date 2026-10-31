module SessionWindowTest exposing (tests)

import Expect
import SessionWindow
import Test exposing (describe, test)


row source identity bytes = { source = source, identity = identity, bytes = bytes }
policy = { source = .source, identity = .identity, bytes = .bytes }
keys = List.map .source


tests =
    describe "Continuous authoritative conversation window"
        [ test "prepend and overlap retain unaffected source occurrences" <| \_ ->
            SessionWindow.compose policy 200 4194304 (Just "30") False True
                [row "20" "b" 1,row "30" "c" 1,row "40" "d" 1]
                [row "10" "a" 1,row "20" "b" 2]
                |> .rows |> keys |> Expect.equal ["10","20","30","40"]
        , test "a proven envelope pair replaces its canonical occurrence at the seam" <| \_ ->
            SessionWindow.compose policy 200 4194304 Nothing True False
                [row "10" "pair" 1,row "15" "real-identical-turn" 1]
                [row "20" "pair" 1]
                |> .rows |> keys |> Expect.equal ["15","20"]
        , test "a discovered earlier identity replaces the same raw source, not adjacent equal text" <| \_ ->
            SessionWindow.compose policy 200 4194304 Nothing True False
                [row "20" "old-unpaired" 1,row "30" "next-real-turn" 1]
                [row "20" "earlier-pair" 1]
                |> .rows |> keys |> Expect.equal ["20","30"]
        , test "bounded trimming protects the visible anchor and records omitted sides" <| \_ ->
            let result = SessionWindow.compose policy 4 100 (Just "05") False True
                    (List.range 4 7 |> List.map (\n -> row ("0"++String.fromInt n) (String.fromInt n) 1))
                    (List.range 0 3 |> List.map (\n -> row ("0"++String.fromInt n) (String.fromInt n) 1))
            in Expect.equal (["04","05","06","07"],["00","01","02","03"],[])
                (keys result.rows,keys result.removedBefore,keys result.removedAfter)
        , test "following protects latest while honoring the whole-window byte budget" <| \_ ->
            let result = SessionWindow.compose policy 200 5 Nothing True False
                    [row "10" "a" 3,row "20" "b" 3]
                    [row "30" "c" 3]
            in Expect.equal (["30"],["10","20"]) (keys result.rows,keys result.removedBefore)
        , test "following prioritizes current latest over a remembered historical anchor" <| \_ ->
            SessionWindow.compose policy 200 5 (Just "10") True False
                [row "10" "a" 3] [row "20" "b" 3,row "30" "c" 3]
                |> .rows |> keys |> Expect.equal ["30"]
        , test "source cursor ordering is numeric without relying on caller padding" <| \_ ->
            SessionWindow.compose policy 200 100 Nothing True False
                [row "100" "a" 1] [row "2" "b" 1,row "10" "c" 1]
                |> .rows |> keys |> Expect.equal ["2","10","100"]
        , test "authoritative parsed span retires a backward-discovered hidden pair envelope" <| \_ ->
            SessionWindow.composeRange (Just ("10","30")) policy 200 100 Nothing True True
                [row "20" "20" 1,row "40" "40" 1] [row "10" "10" 1]
                |> .rows |> keys |> Expect.equal ["10","40"]
        , test "large sealed source and block cursors sort without floating-point conversion" <| \_ ->
            SessionWindow.compose policy 200 100 Nothing True False
                [row "9007199254740991:250" "later" 1]
                [row "9007199254740990:9999999" "earlier" 1,row "9007199254740991:249" "adjacent" 1]
                |> .rows |> keys |> Expect.equal ["9007199254740990:9999999","9007199254740991:249","9007199254740991:250"]
        , test "refilling the proven scan interval removes empty inter-record cursor gaps" <| \_ ->
            SessionWindow.reconcileGaps (Just ("10:0:1","30:0:1")) [("10:0","10:0:1"),("20:0","20:0:1"),("30:0","30:0:1")]
                [] [{before="10:0:1",after="20:0",pixels=64,group=""}]
                |> Expect.equal []
        , test "single-record block cuts preserve proportional geometry without packing a large absolute rank" <| \_ ->
            let result=SessionWindow.reconcileGaps Nothing [("9007199254740991:0","9007199254740991:0:1")] [] [{before="9007199254740991:0",after="9007199254740991:100",pixels=10000,group=""}] in
            Expect.equal [{before="9007199254740991:0:1",after="9007199254740991:100",pixels=9900,group=""}] result
        ]
