module AuditDiff exposing (Value(..), Comparison(..), Line, Hunk, Segment, extract, compare, words, available, describe)

{-| Literal, bounded text comparison. Snapshot absence is never interpreted as removal.
The quadratic line table is limited to 160,000 cells; larger values retain their
complete text in the simplified presentation. Newline terminators belong to lines.
-}
import Array exposing (Array)
import Dict
import Json.Decode as Decode

type Value
    = Text String
    | Null
    | MissingSnapshot
    | MissingField
    | Malformed

type Comparison
    = Equal
    | Unavailable
    | Simplified
    | Unified (List Hunk)

type alias Line =
    { kind : String, content : String, old : Maybe Int, new : Maybe Int }

type alias Hunk =
    { oldStart : Int, newStart : Int, oldCount : Int, newCount : Int, lines : List Line }

type alias Segment =
    { content : String, changed : Bool }

extract : String -> Maybe Decode.Value -> Value
extract field snapshot =
    case snapshot of
        Nothing -> MissingSnapshot
        Just raw ->
            case Decode.decodeValue (Decode.dict Decode.value) raw of
                Err _ -> Malformed
                Ok fields ->
                    case Dict.get field fields of
                        Nothing -> MissingField
                        Just value -> Decode.decodeValue (Decode.oneOf [ Decode.map Text Decode.string, Decode.null Null ]) value |> Result.withDefault Malformed

available : Value -> Bool
available value =
    case value of
        Text _ -> True
        Null -> True
        _ -> False

describe : Value -> String
describe value =
    case value of
        Text "" -> "Empty string"
        Text _ -> "Text"
        Null -> "Explicit null (unset)"
        MissingSnapshot -> "Unavailable: missing snapshot"
        MissingField -> "Unavailable: missing field"
        Malformed -> "Unavailable: malformed data"

literal : Value -> String
literal value =
    case value of
        Text content -> content
        _ -> ""

lines : String -> List String
lines source =
    if source == "" then [] else
    let
        parts = String.split "\n" source
        terminated = List.take (List.length parts - 1) parts |> List.map (\part -> part ++ "\n")
        tail = List.reverse parts |> List.head |> Maybe.withDefault ""
    in
    terminated ++ (if tail == "" then [] else [ tail ])

compare : Value -> Value -> Comparison
compare before after =
    if not (available before && available after) then Unavailable
    else if before == after then Equal
    else if String.length (literal before) + String.length (literal after) > 80000 then Simplified
    else
        let
            left = lines (literal before)
            right = lines (literal after)
        in
        if List.length left > 800 || List.length right > 800 || List.length left * List.length right > 160000 then Simplified
        else
            let
                edits = diff left right
                -- Explicit null and empty text have the same line sequence, but different types.
                typed = if List.isEmpty edits then [ { kind = "-", content = describe before, old = Nothing, new = Nothing }, { kind = "+", content = describe after, old = Nothing, new = Nothing } ] else edits
            in
            Unified (hunks typed)

diff : List String -> List String -> List Line
diff left right =
    let
        a = Array.fromList left
        b = Array.fromList right
        n = Array.length a
        m = Array.length b
        at index values = Array.get index values |> Maybe.withDefault ""
        cell row column matrix = Array.get row matrix |> Maybe.andThen (Array.get column) |> Maybe.withDefault 0
        buildRow i matrix =
            let
                row = List.foldl (\j acc -> Array.set j (if at i a == at j b then 1 + cell (i + 1) (j + 1) matrix else max (cell (i + 1) j matrix) (Array.get (j + 1) acc |> Maybe.withDefault 0)) acc) (Array.repeat (m + 1) 0) (List.reverse (List.range 0 (m - 1)))
            in Array.set i row matrix
        table = List.foldl buildRow (Array.repeat (n + 1) (Array.repeat (m + 1) 0)) (List.reverse (List.range 0 (n - 1)))
        walk i j acc =
            if i >= n && j >= m then List.reverse acc
            else if i < n && j < m && at i a == at j b then walk (i + 1) (j + 1) ({ kind = " ", content = at i a, old = Just (i + 1), new = Just (j + 1) } :: acc)
            else if i < n && (j >= m || cell (i + 1) j table >= cell i (j + 1) table) then walk (i + 1) j ({ kind = "-", content = at i a, old = Just (i + 1), new = Nothing } :: acc)
            else walk i (j + 1) ({ kind = "+", content = at j b, old = Nothing, new = Just (j + 1) } :: acc)
    in walk 0 0 []

hunks : List Line -> List Hunk
hunks edits =
    let
        marked = List.indexedMap Tuple.pair edits |> List.filter (Tuple.second >> .kind >> (/=) " ") |> List.map Tuple.first
        visible index = List.any (\changed -> abs (index - changed) <= 3) marked
        groups = List.foldl (\( index, line ) ( finished, current ) -> if visible index then ( finished, line :: current ) else if List.isEmpty current then ( finished, [] ) else ( List.reverse current :: finished, [] )) ( [], [] ) (List.indexedMap Tuple.pair edits)
        allGroups = List.reverse (if List.isEmpty (Tuple.second groups) then Tuple.first groups else List.reverse (Tuple.second groups) :: Tuple.first groups)
        hunk chunk =
            { oldStart = List.filterMap .old chunk |> List.head |> Maybe.withDefault 0
            , newStart = List.filterMap .new chunk |> List.head |> Maybe.withDefault 0
            , oldCount = List.filterMap .old chunk |> List.length
            , newCount = List.filterMap .new chunk |> List.length
            , lines = chunk
            }
    in List.map hunk allGroups

{-| A bounded token LCS emphasizes changed words while retaining whitespace exactly.
Long lines use one changed segment; no unbounded character/word table is built.
-}
words : String -> String -> ( List Segment, List Segment )
words before after =
    if String.length before + String.length after > 4000 then
        ( [ { content = before, changed = True } ], [ { content = after, changed = True } ] )
    else
    let
        tokens source =
            let
                step char ( done, current, whitespace ) =
                    let space = List.member char [ ' ', '\t', '\n', '\r' ] in
                    if current == "" || space == whitespace then ( done, current ++ String.fromChar char, space ) else ( current :: done, String.fromChar char, space )
                finish ( done, current, _ ) = List.reverse (if current == "" then done else current :: done)
            in String.toList source |> List.foldl step ( [], "", False ) |> finish
        a = tokens before
        b = tokens after
        segments kind edits = edits |> List.filter (\line -> line.kind == " " || line.kind == kind) |> List.map (\line -> { content = line.content, changed = line.kind /= " " })
    in
    if List.length a * List.length b > 4096 then ( [ { content = before, changed = True } ], [ { content = after, changed = True } ] )
    else let edits = diff a b in ( segments "-" edits, segments "+" edits )
