module ObservationViewport exposing (State, Stamp, init, rebuild, pieces, update, stampValue, sync, syncReceipt, captureOrigin, returnToOrigin)

import Array
import Dict exposing (Dict)
import HierarchyViewport
import Json.Decode as Decode
import Json.Encode as Encode
import Set exposing (Set)


type alias Stamp =
    { workspace : String, epoch : Int, generation : String, revision : Int }


type alias State =
    { stamp : Stamp
    , navigationToken : Int
    , index : HierarchyViewport.Index
    , top : Float
    , height : Float
    , width : Float
    , focus : Maybe String
    , returnPin : Maybe String
    , layout : String
    , origin : Maybe Origin
    , restoring : Bool
    }

type alias Origin =
    { index : HierarchyViewport.Index, width : Float, layout : String }


init : State
init =
    { stamp = { workspace = "", epoch = -1, generation = "", revision = 0 }
    , navigationToken = 0
    , index = HierarchyViewport.build 180 Dict.empty []
    , top = 0, height = 1000, width = 0, focus = Nothing, returnPin = Nothing
    , layout = "", origin = Nothing, restoring = False
    }

captureOrigin : State -> State
captureOrigin state =
    let
        retained = state.origin |> Maybe.andThen (\origin -> if origin.layout == state.layout && origin.index.keys == state.index.keys then Just origin else Nothing)
    in
    { state | origin = if retained /= Nothing then retained else if state.width > 0 then Just { index = state.index, width = state.width, layout = state.layout } else Nothing, restoring = False }

returnToOrigin : Maybe String -> State -> State
returnToOrigin key state =
    let stamp = state.stamp in
    { state | returnPin = key, restoring = key /= Nothing && state.origin /= Nothing, stamp = { stamp | revision = stamp.revision + 1 } }


sameLifetime : Stamp -> Stamp -> Bool
sameLifetime a b =
    a.workspace == b.workspace && a.epoch == b.epoch && a.generation == b.generation


rebuild : Stamp -> List String -> State -> State
rebuild lifetime keys old =
    let
        retained = sameLifetime lifetime old.stamp
        at = HierarchyViewport.positionAt old.top old.index
        anchor = Array.get at old.index.keys
        within = old.top - HierarchyViewport.offset at old.index
        index = HierarchyViewport.build 180 (if retained then old.index.heights else Dict.empty) keys
        top =
            if retained then
                anchor |> Maybe.andThen (\key -> Dict.get key index.positions)
                    |> Maybe.map (\position -> HierarchyViewport.offset position index + within)
                    |> Maybe.withDefault old.top
            else 0
    in
    { old | stamp = { lifetime | revision = old.stamp.revision + 1 }, index = index
        , top = max 0 top, focus = if retained then old.focus else Nothing, returnPin = if retained then old.returnPin else Nothing
        , origin = if retained && index.keys == old.index.keys then old.origin else Nothing, restoring = False }


pins : Maybe String -> State -> Set String
pins returned state =
    [ if returned == Nothing then state.returnPin else returned, state.focus ] |> List.filterMap identity |> Set.fromList


pieces : Maybe String -> State -> List HierarchyViewport.Piece
pieces returned state =
    HierarchyViewport.window state.top state.height 360 25 (pins returned state) state.index


stampValue : Stamp -> Encode.Value
stampValue stamp =
    Encode.object [ ( "workspace", Encode.string stamp.workspace ), ( "epoch", Encode.int stamp.epoch )
        , ( "generation", Encode.string stamp.generation ), ( "revision", Encode.int stamp.revision ) ]


sync : Bool -> Float -> Maybe ( String, String ) -> State -> Encode.Value
sync changed adjustment target state =
    encodeSync changed False adjustment target state


syncReceipt : Bool -> Float -> Maybe ( String, String ) -> State -> Encode.Value
syncReceipt settled adjustment target state =
    encodeSync False settled adjustment target state


encodeSync : Bool -> Bool -> Float -> Maybe ( String, String ) -> State -> Encode.Value
encodeSync changed settled adjustment target state =
    Encode.object
        [ ( "stamp", stampValue state.stamp )
        , ( "navigationToken", Encode.int state.navigationToken )
        , ( "settled", Encode.bool settled )
        , ( "top", if changed then Encode.float state.top else Encode.null )
        , ( "adjustment", Encode.float adjustment )
        , ( "keys", if changed then Encode.list Encode.string (Array.toList state.index.keys) else Encode.null )
        , ( "target", target |> Maybe.map (\( key, edge ) ->
            Encode.object [ ( "key", Encode.string key ), ( "edge", Encode.string edge )
                , ( "offset", Dict.get key state.index.positions |> Maybe.map (\position -> HierarchyViewport.offset position state.index) |> Maybe.withDefault 0 |> Encode.float ) ]) |> Maybe.withDefault Encode.null )
        ]


type alias Receipt =
    { stamp : Stamp, top : Float, height : Float, width : Float, layout : String
    , measurements : List ( String, Float ), focus : Maybe String, target : Maybe ( String, String ) }


decoder : Decode.Decoder Receipt
decoder =
    Decode.map8 Receipt
        (Decode.field "stamp" (Decode.map4 Stamp (Decode.field "workspace" Decode.string) (Decode.field "epoch" Decode.int) (Decode.field "generation" Decode.string) (Decode.field "revision" Decode.int)))
        (Decode.field "top" Decode.float) (Decode.field "height" Decode.float) (Decode.field "width" Decode.float)
        (Decode.field "layout" Decode.string)
        (Decode.field "measurements" (Decode.list (Decode.map2 Tuple.pair (Decode.field "key" Decode.string) (Decode.field "height" Decode.float))))
        (Decode.field "focus" (Decode.nullable Decode.string))
        (Decode.field "target" (Decode.nullable (Decode.map2 Tuple.pair (Decode.field "key" Decode.string) (Decode.field "edge" Decode.string))))


{-| Only the exact painted layout can measure its mounted rows or request focus.
Unseen keys and offscreen measurements cannot alter geometry.
-}
update : Maybe String -> Encode.Value -> State -> Maybe ( State, Maybe ( String, String ), Float )
update returned value state =
    case Decode.decodeValue decoder value of
        Err _ -> Nothing
        Ok receipt ->
            if receipt.stamp /= state.stamp || receipt.height <= 0 || receipt.width <= 0 || receipt.top < 0 then
                Nothing
            else
                let
                    mounted = pieces returned state |> List.filterMap (\piece -> case piece of
                        HierarchyViewport.Row _ key _ -> Just key
                        _ -> Nothing) |> Set.fromList
                    validKey key = Dict.member key state.index.positions
                    focus = receipt.focus |> Maybe.andThen (\key -> if Set.member key mounted then Just key else Nothing)
                    target = receipt.target |> Maybe.andThen (\( key, edge ) -> if validKey key && List.member edge [ "first", "last", "return" ] then Just ( key, edge ) else Nothing)
                    changedWidth = (state.width > 0 && abs (receipt.width - state.width) > 1) || receipt.layout /= state.layout
                    compatible origin = abs (origin.width - receipt.width) <= 1 && origin.layout == receipt.layout && origin.index.keys == state.index.keys
                    restored = if state.restoring then state.origin |> Maybe.andThen (\origin -> if compatible origin then Just origin.index else Nothing) else Nothing
                    measuredIndex =
                        restored |> Maybe.withDefault (if changedWidth then HierarchyViewport.build 180 Dict.empty (Array.toList state.index.keys) else state.index)
                    index = List.foldl (\( key, height ) current ->
                        if Set.member key mounted && height > 0 && height < 100000 then HierarchyViewport.measure key height current else current)
                        measuredIndex receipt.measurements
                    anchorAt = HierarchyViewport.positionAt receipt.top state.index
                    anchoredTop = if restored /= Nothing then receipt.top else HierarchyViewport.offset anchorAt index + receipt.top - HierarchyViewport.offset anchorAt state.index
                    currentStamp = state.stamp
                    nextStamp = if changedWidth || restored /= Nothing then { currentStamp | revision = currentStamp.revision + 1 } else currentStamp
                    updated = { state | index = index, top = max 0 anchoredTop, height = receipt.height, width = receipt.width, stamp = nextStamp
                        , focus = case target of
                            Just ( key, _ ) -> Just key
                            Nothing -> if focus /= Nothing then focus else if List.member receipt.focus [ Just "@outside", Just "@results" ] then Nothing else state.focus
                        , returnPin = if focus /= Nothing || receipt.focus == Just "@results" then Nothing else state.returnPin
                        , layout = receipt.layout, restoring = False
                        , origin = if state.restoring || receipt.layout /= state.layout then Nothing else state.origin }
                in
                Just ( updated, target, anchoredTop - receipt.top )
