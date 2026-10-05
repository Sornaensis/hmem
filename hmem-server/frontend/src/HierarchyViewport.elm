module HierarchyViewport exposing (Index, Piece(..), build, height, measure, offset, partition, positionAt, window)

import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)


{-| Cached geometry. Building is structural work; scrolling and individual
measurements only traverse the balanced height index.
-}
type alias Index =
    { keys : Array String
    , positions : Dict String Int
    , heights : Dict String Float
    , estimate : Float
    , tree : HeightTree
    }


type HeightTree
    = Empty
    | Leaf Float
    | Branch Int Float HeightTree HeightTree


type Piece
    = Row Int String Float
    | Gap Int Float


{-| Keep a protected row in a stable DOM boundary while both surrounding
windows change. Concatenating the three segments preserves logical order and
every omitted extent; only the bounded mounted pieces are traversed.
-}
partition : Maybe String -> List Piece -> { before : List Piece, pivot : List Piece, after : List Piece }
partition target pieces =
    let
        find preceding remaining =
            case remaining of
                [] -> { before = pieces, pivot = [], after = [] }
                piece :: tail ->
                    case piece of
                        Row _ key _ ->
                            if target == Just key then
                                { before = List.reverse preceding, pivot = [ piece ], after = tail }
                            else
                                find (piece :: preceding) tail
                        _ -> find (piece :: preceding) tail
    in
    find [] pieces


build : Float -> Dict String Float -> List String -> Index
build estimate measurements keys =
    let
        estimated = max 1 estimate
        array = Array.fromList keys
        keySet = Set.fromList keys
        heights = measurements |> Dict.filter (\key _ -> Set.member key keySet)
        make start count =
            if count <= 0 then
                Empty
            else if count == 1 then
                Leaf (Array.get start array |> Maybe.andThen (\key -> Dict.get key heights) |> Maybe.withDefault estimated)
            else
                let
                    leftCount = count // 2
                in
                join (make start leftCount) (make (start + leftCount) (count - leftCount))
    in
    { keys = array
    , positions = keys |> List.indexedMap (\i key -> ( key, i )) |> Dict.fromList
    , heights = heights
    , estimate = estimated
    , tree = make 0 (Array.length array)
    }


size : HeightTree -> Int
size tree =
    case tree of
        Empty -> 0
        Leaf _ -> 1
        Branch count _ _ _ -> count


total : HeightTree -> Float
total tree =
    case tree of
        Empty -> 0
        Leaf value -> value
        Branch _ value _ _ -> value


join : HeightTree -> HeightTree -> HeightTree
join left right =
    Branch (size left + size right) (total left + total right) left right


height : Index -> Float
height index =
    total index.tree


prefix : Int -> HeightTree -> Float
prefix count tree =
    if count <= 0 then
        0
    else if count >= size tree then
        total tree
    else
        case tree of
            Branch _ _ left right ->
                if count <= size left then
                    prefix count left
                else
                    total left + prefix (count - size left) right
            _ -> 0


offset : Int -> Index -> Float
offset position index =
    prefix position index.tree


positionAt : Float -> Index -> Int
positionAt value index =
    let
        find top tree =
            case tree of
                Branch _ _ left right ->
                    if top < total left then
                        find top left
                    else
                        size left + find (top - total left) right
                _ -> 0
    in
    find (max 0 value) index.tree |> min (max 0 (Array.length index.keys - 1))


measure : String -> Float -> Index -> Index
measure key value index =
    case Dict.get key index.positions of
        Nothing -> index
        Just position ->
            let
                measured = max 1 value
                change at tree =
                    case tree of
                        Branch _ _ left right ->
                            if at < size left then
                                join (change at left) right
                            else
                                join left (change (at - size left) right)
                        Leaf _ -> Leaf measured
                        Empty -> Empty
            in
            { index | heights = Dict.insert key measured index.heights, tree = change position index.tree }


window : Float -> Float -> Float -> Int -> Set String -> Index -> List Piece
window top viewport overscan limit pins index =
    let
        count = Array.length index.keys
        visibleFirst = positionAt top index
        visibleLast = positionAt (top + viewport - 0.001) index
        available = max 0 (limit - (visibleLast - visibleFirst + 1))
        desiredFirst = positionAt (max 0 (top - overscan)) index
        before = min (visibleFirst - desiredFirst) ((available + 1) // 2)
        first = visibleFirst - before
        last = min (first + max 0 limit - 1) (positionAt (top + viewport + overscan - 0.001) index)
        ordinary = if count == 0 || limit <= 0 then [] else List.range first last
        pinned = Set.toList pins |> List.filterMap (\key -> Dict.get key index.positions)
        selected = ordinary ++ pinned |> Set.fromList |> Set.toList
        pieces remaining previous =
            case remaining of
                [] ->
                    if previous < count then [ Gap previous (offset count index - offset previous index) ] else []
                at :: rest ->
                    let
                        gap = if at > previous then [ Gap previous (offset at index - offset previous index) ] else []
                        row = Array.get at index.keys |> Maybe.map (\key -> Row at key (offset at index)) |> Maybe.map List.singleton |> Maybe.withDefault []
                    in
                    gap ++ row ++ pieces rest (at + 1)
    in
    pieces selected 0
