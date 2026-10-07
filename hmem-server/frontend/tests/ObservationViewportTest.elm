module ObservationViewportTest exposing (tests)

import Array
import Dict
import Expect
import HierarchyViewport exposing (Piece(..))
import Json.Encode as Encode
import ObservationViewport as Viewport
import Test exposing (..)


keys : List String
keys = List.range 0 999 |> List.map String.fromInt


state : Viewport.State
state = Viewport.rebuild { workspace = "repo", epoch = 3, generation = "applied", revision = 0 } keys Viewport.init


rows : List Piece -> List String
rows = List.filterMap (\piece -> case piece of
    Row _ key _ -> Just key
    _ -> Nothing)


receipt : Viewport.State -> Float -> Float -> List ( String, Float ) -> Maybe String -> Maybe ( String, String ) -> Encode.Value
receipt current top width measurements focus target =
    Encode.object
        [ ( "stamp", Viewport.stampValue current.stamp ), ( "top", Encode.float top ), ( "height", Encode.float 1000 ), ( "width", Encode.float width )
        , ( "layout", Encode.string current.layout )
        , ( "measurements", Encode.list (\( key, height ) -> Encode.object [ ( "key", Encode.string key ), ( "height", Encode.float height ) ]) measurements )
        , ( "focus", focus |> Maybe.map Encode.string |> Maybe.withDefault Encode.null )
        , ( "target", target |> Maybe.map (\( key, edge ) -> Encode.object [ ( "key", Encode.string key ), ( "edge", Encode.string edge ) ]) |> Maybe.withDefault Encode.null ) ]


tests : Test
tests = describe "Observation global logical viewport"
    [ test "every ordered member survives scrolling with at most25 ordinary rows and two distinct pins" <| \_ ->
        let
            active = { state | top = 50000, height = 100000, focus = Just "900" }
            mounted = Viewport.pieces (Just "2") active |> rows
        in
        Expect.all
            [ \_ -> Expect.atMost 27 (List.length mounted)
            , \_ -> Expect.equal True (List.member "2" mounted && List.member "900" mounted)
            , \_ -> Expect.equal keys (Array.toList active.index.keys)
            , \_ -> Expect.equal 180000 (HierarchyViewport.height active.index)
            ] ()
    , test "a repeated focus and return owner adds one pin and leaves all omitted geometry" <| \_ ->
        let active = { state | top = 50000, height = 100000, focus = Just "2" } in
        Expect.equal 26 (Viewport.pieces (Just "2") active |> rows |> List.length)
    , test "selected native and return owners are three deduplicated pins with truthful geometry" <| \_ ->
        let
            active = { state | top = 50000, height = 100000, focus = Just "900" }
            pieces = Viewport.piecesWithOwner (Just "700") (Just "2") active
            total piece = case piece of
                Row _ _ _ -> active.index.estimate
                Gap _ height -> height
        in
        Expect.all
            [ \_ -> Expect.equal 28 (List.length (rows pieces))
            , \_ -> Expect.equal True (List.all (\key -> List.member key (rows pieces)) [ "700", "900", "2" ])
            , \_ -> Expect.equal 26 (Viewport.piecesWithOwner (Just "900") (Just "900") active |> rows |> List.length)
            , \_ -> Expect.equal 180000 (List.sum (List.map total pieces))
            , \_ -> Expect.equal keys (Array.toList active.index.keys)
            ] ()
    , test "selected offscreen owner measurement is admitted only under current workspace session query and layout" <| \_ ->
        let
            active = { state | top = 50000, focus = Just "900" }
            stamp = active.stamp
            retired = [ { stamp | workspace = "other" }, { stamp | epoch = 2 }, { stamp | generation = "old" }, { stamp | revision = stamp.revision - 1 } ]
        in
        case Viewport.updateWithOwner (Just "700") (Just "2") (receipt active active.top 800 [ ( "700", 450 ), ( "999", 3 ) ] Nothing Nothing) active of
            Nothing -> Expect.fail "selected receipt rejected"
            Just ( updated, _, _ ) -> Expect.all
                [ \_ -> Expect.equal ( Just 450, Nothing ) ( Dict.get "700" updated.index.heights, Dict.get "999" updated.index.heights )
                , \_ -> Expect.equal (List.repeat 4 Nothing) (List.map (\old -> Viewport.updateWithOwner (Just "700") (Just "2") (receipt { active | stamp = old } active.top 800 [ ( "700", 1 ) ] Nothing Nothing) active) retired)
                ] ()
    , test "structural reorder preserves exact anchor key and intrarow scroll" <| \_ ->
        let
            old = { state | top = 18000 + 17 }
            reordered = Viewport.rebuild old.stamp ("999" :: List.take 999 keys) old
            at = HierarchyViewport.positionAt reordered.top reordered.index
        in
        Expect.equal ( Just "100", 17 ) ( Array.get at reordered.index.keys, reordered.top - HierarchyViewport.offset at reordered.index )
    , test "old workspace session query and layout measurement cannot alter current geometry" <| \_ ->
        let
            stamp = state.stamp
            retired = [ { stamp | workspace = "other" }, { stamp | epoch = 2 }, { stamp | generation = "old" }, { stamp | revision = stamp.revision - 1 } ]
        in
        retired |> List.map (\old -> Viewport.update Nothing (receipt { state | stamp = old } 0 800 [ ( "0", 90 ) ] Nothing Nothing) state)
            |> Expect.equal (List.repeat 4 Nothing)
    , test "only actually mounted keys can measure or borrow native focus" <| \_ ->
        case Viewport.update Nothing (receipt state 0 800 [ ( "0", 90 ), ( "999", 3 ) ] (Just "999") Nothing) state of
            Nothing -> Expect.fail "current receipt rejected"
            Just ( updated, _, _ ) -> Expect.equal ( Just 90, Nothing, Nothing )
                ( Dict.get "0" updated.index.heights, Dict.get "999" updated.index.heights, updated.focus )
    , test "valid offscreen keyboard target mounts before focus while unknown target stays inert" <| \_ ->
        case Viewport.update Nothing (receipt state 0 800 [] Nothing (Just ( "999", "first" ))) state of
            Nothing -> Expect.fail "target rejected"
            Just ( updated, target, _ ) -> Expect.equal ( True, Just ( "999", "first" ) ) ( List.member "999" (Viewport.pieces Nothing updated |> rows), target )
    , test "width change retires measurements with a fresh layout revision" <| \_ ->
        let old = { state | width = 800 } in
        case Viewport.update Nothing (receipt old 0 320 [ ( "0", 200 ) ] Nothing Nothing) old of
            Nothing -> Expect.fail "reflow rejected"
            Just ( updated, _, _ ) -> Expect.equal ( old.stamp.revision + 1, Nothing )
                ( updated.stamp.revision, Viewport.update Nothing (receipt old 0 800 [] Nothing Nothing) updated )
    , test "measurement above viewport keeps the visible anchor through exact offset adjustment" <| \_ ->
        let old = { state | top = 18000 + 17, focus = Just "2" } in
        case Viewport.update Nothing (receipt old old.top 800 [ ( "2", 250 ) ] (Just "2") Nothing) old of
            Nothing -> Expect.fail "pin measurement rejected"
            Just ( updated, _, adjustment ) -> Expect.equal ( 70, old.top + 70, Just "100" )
                ( adjustment, updated.top, Array.get (HierarchyViewport.positionAt updated.top updated.index) updated.index.keys )
    , test "one compatible returned-width snapshot restores geometry only under a fresh revision" <| \_ ->
        let
            old = { state | width = 800, layout = "font16:resize0", index = HierarchyViewport.measure "500" 123 state.index }
            captured = Viewport.captureOrigin old
            narrow = Viewport.update Nothing (receipt captured 0 400 [] Nothing Nothing) captured |> Maybe.map (\( value, _, _ ) -> value) |> Maybe.withDefault captured
            returning = Viewport.returnToOrigin (Just "500") narrow
        in
        case Viewport.update Nothing (receipt returning 3000 800 [] Nothing Nothing) returning of
            Nothing -> Expect.fail "current return receipt rejected"
            Just ( restored, _, adjustment ) ->
                Expect.equal ( ( Just 123, 0, True ), ( Nothing, Nothing ) )
                    ( ( Dict.get "500" restored.index.heights, adjustment, restored.stamp.revision > returning.stamp.revision ), ( restored.origin
                    , Viewport.update Nothing (receipt captured 0 800 [] Nothing Nothing) restored ) )
    , test "actual resize font or changed membership retires the origin layout without old geometry reuse" <| \_ ->
        let
            old = { state | width = 800, layout = "font16:resize0", index = HierarchyViewport.measure "500" 123 state.index }
            captured = Viewport.captureOrigin old
            pending = Viewport.returnToOrigin (Just "2") captured
            changedLayout = Viewport.update Nothing (receipt { pending | layout = "font32:resize1" } 0 800 [] Nothing Nothing) pending |> Maybe.map (\( value, _, _ ) -> value)
            reordered = Viewport.rebuild old.stamp (List.reverse keys) captured
            retired = Viewport.rebuild { workspace = "other", epoch = 4, generation = "new", revision = 0 } keys captured
        in
        Expect.equal ( ( Just Nothing, Just Nothing ), ( Nothing, Nothing ) )
            ( ( Maybe.map (\value -> Dict.get "500" value.index.heights) changedLayout, Maybe.map .origin changedLayout ), ( reordered.origin, retired.origin ) )
    , test "same-ID activation retains one compatible wide snapshot across the expected detail-width transition" <| \_ ->
        let
            wide = { state | width = 800, layout = "font16", index = HierarchyViewport.measure "500" 123 state.index } |> Viewport.captureOrigin
            narrow = Viewport.update Nothing (receipt wide 0 400 [] Nothing Nothing) wide |> Maybe.map (\( value, _, _ ) -> value) |> Maybe.withDefault wide
            activatedAgain = Viewport.captureOrigin narrow
        in
        Expect.equal ( Just 800, Just (Just 123) ) ( Maybe.map .width activatedAgain.origin, Maybe.map (\origin -> Dict.get "500" origin.index.heights) activatedAgain.origin )
    , test "pending return pin survives an unrelated focus receipt and retires after exact native focus" <| \_ ->
        let
            pending = Viewport.returnToOrigin (Just "900") state
            unchanged = Viewport.update Nothing (receipt pending 0 800 [] Nothing Nothing) pending |> Maybe.map (\( value, _, _ ) -> value) |> Maybe.withDefault pending
        in
        case Viewport.update Nothing (receipt unchanged 0 800 [] (Just "900") Nothing) unchanged of
            Nothing -> Expect.fail "mounted native focus rejected"
            Just ( focused, _, _ ) -> Expect.equal ( Just "900", Nothing, Just "900" ) ( unchanged.returnPin, focused.returnPin, focused.focus )
    , test "accepted native focus survives null geometry but actual current outside results and replacement retire it" <| \_ ->
        let
            accepted = { state | width = 800, focus = Just "900" }
            apply focus current = Viewport.update Nothing (receipt current 0 800 [] focus Nothing) current |> Maybe.map (\( value, _, _ ) -> value) |> Maybe.withDefault current
            nullGeometry = apply Nothing accepted
            outside = apply (Just "@outside") nullGeometry
            results = apply (Just "@results") nullGeometry
            replacement = apply (Just "0") nullGeometry
            changedStamp = accepted.stamp
            retired = Viewport.rebuild { changedStamp | epoch = changedStamp.epoch + 1 } keys accepted
        in
        Expect.all
            [ \_ -> Expect.equal (Just "900") nullGeometry.focus
            , \_ -> Expect.atMost 27 (Viewport.pieces (Just "999") nullGeometry |> rows |> List.length)
            , \_ -> Expect.equal [ Nothing, Nothing, Just "0", Nothing ] [ outside.focus, results.focus, replacement.focus, retired.focus ]
            , \_ -> Expect.equal Nothing (Viewport.update Nothing (receipt accepted 0 800 [] (Just "@outside") Nothing) retired)
            ] ()
    ]
