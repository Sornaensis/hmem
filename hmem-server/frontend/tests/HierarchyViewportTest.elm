module HierarchyViewportTest exposing (tests)

import Dict
import Expect
import HierarchyViewport as Viewport exposing (Piece(..))
import Set
import Test exposing (Test, describe, test)


keys : List String
keys =
    List.range 0 999 |> List.map String.fromInt


rows : List Piece -> List String
rows pieces =
    List.filterMap
        (\piece ->
            case piece of
                Row _ key _ ->
                    Just key

                Gap _ _ ->
                    Nothing
        )
        pieces


tests : Test
tests =
    describe "global hierarchy viewport"
        [ test "protected pivot segments retain logical order and all spacer geometry" <| \_ ->
            let
                pieces = [ Gap 0 200, Row 2 "editor" 100, Gap 3 49700, Row 500 "visible" 100, Gap 501 49900 ]
                segments = Viewport.partition (Just "editor") pieces
            in
            Expect.equal ( pieces, [ Row 2 "editor" 100 ] ) ( segments.before ++ segments.pivot ++ segments.after, segments.pivot )
        , test "absent retired pivot leaves the complete ordinary window" <| \_ ->
            let
                pieces = Viewport.window 50000 600 0 12 Set.empty (Viewport.build 100 Dict.empty keys)
                segments = Viewport.partition (Just "retired") pieces
            in
            Expect.equal ( pieces, [], [] ) ( segments.before, segments.pivot, segments.after )
        , test "short measured rows prioritize the actual viewport over overscan" <| \_ ->
            Viewport.window 500 100 100 12 Set.empty (Viewport.build 1 Dict.empty keys)
                |> rows |> List.head |> Expect.equal (Just "500")
        , test "a thousand cached rows mount only the bounded scroll window" <| \_ ->
            let
                index = Viewport.build 100 Dict.empty keys
                pieces = Viewport.window 50000 600 100 12 Set.empty index
            in
            Expect.equal ( List.range 499 506 |> List.map String.fromInt, 100000 ) ( rows pieces, Viewport.height index )
        , test "distant pins leave a spacer for every omitted run" <| \_ ->
            let
                pieces = Viewport.window 50000 600 0 12 (Set.fromList [ "2", "900" ]) (Viewport.build 100 Dict.empty keys)
                gaps =
                    List.filterMap
                        (\piece ->
                            case piece of
                                Gap _ amount ->
                                    Just amount

                                _ ->
                                    Nothing
                        )
                        pieces
            in
            Expect.equal [ 200, 49700, 39400, 9900 ] gaps
        , test "measured height changes preserve the anchor key and intrarow position" <| \_ ->
            let
                index = Viewport.build 100 Dict.empty keys |> Viewport.measure "2" 180
                anchoredTop = Viewport.offset 500 index + 15
            in
            Expect.equal ( 50095, "500" ) ( anchoredTop, Viewport.window anchoredTop 100 0 12 Set.empty index |> rows |> List.head |> Maybe.withDefault "" )
        ]
