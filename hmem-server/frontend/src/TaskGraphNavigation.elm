module TaskGraphNavigation exposing (Entry, Guard, Route, State, graphUrl, init, lookup, originKey, parse, remember, retire, stampOrigin)

{-| History entries own their origin independently. This registry contains
presentation receipts, never canonical entity data or transport admissions.
-}

import Dict exposing (Dict)
import Url


type alias Guard =
    { workspaceId : String, sessionEpoch : Int, audienceId : String }


type alias Route =
    { workspaceId : String, projectId : String, entryId : Maybe String }


type alias Entry origin =
    { guard : Guard, projectId : String, sourceUrl : String, graphUrl : String, origin : origin }


type alias State origin =
    { entries : Dict String (Entry origin), order : List String, nextId : Int }


init : State origin
init =
    { entries = Dict.empty, order = [], nextId = 1 }


retire : State origin -> State origin
retire state =
    { state | entries = Dict.empty, order = [] }


remember : String -> Entry origin -> State origin -> State origin
remember id entry state =
    let
        order = List.take 64 (id :: List.filter ((/=) id) state.order)
    in
    { state | entries = Dict.insert id entry state.entries |> Dict.filter (\key _ -> List.member key order), order = order, nextId = state.nextId + 1 }


lookup : Guard -> String -> State origin -> Maybe (Entry origin)
lookup guard id state =
    Dict.get id state.entries |> Maybe.andThen (\entry -> if entry.guard == guard then Just entry else Nothing)


pairs : Url.Url -> List ( String, String )
pairs url =
    url.fragment |> Maybe.withDefault "" |> String.split "&"
        |> List.filterMap (\part -> case String.split "=" part of
            [ key, value ] -> Maybe.map2 Tuple.pair (Url.percentDecode key) (Url.percentDecode value)
            _ -> Nothing)


one : String -> Url.Url -> Maybe String
one key url =
    case pairs url |> List.filter (Tuple.first >> (==) key) |> List.map Tuple.second of
        [ value ] -> if String.isEmpty value || String.length value > 256 then Nothing else Just value
        _ -> Nothing


parse : Url.Url -> Maybe Route
parse url =
    case String.split "/" url.path |> List.filter ((/=) "") of
        [ "workspace", workspaceId ] ->
            one "task-graph" url |> Maybe.map (\projectId -> { workspaceId = workspaceId, projectId = projectId, entryId = one "graph-entry" url })
        _ -> Nothing


originKey : Url.Url -> Maybe String
originKey =
    one "graph-origin"


stampOrigin : String -> Url.Url -> String
stampOrigin id url =
    let
        retained = pairs url |> List.filter (\( key, _ ) -> not (List.member key [ "task-graph", "graph-entry", "graph-origin" ]))
        fragment = retained ++ [ ( "graph-origin", id ) ] |> List.map (\( key, value ) -> Url.percentEncode key ++ "=" ++ Url.percentEncode value) |> String.join "&"
    in
    Url.toString { url | fragment = Just fragment }


graphUrl : String -> String -> Url.Url -> String
graphUrl projectId id url =
    Url.toString { url | query = Nothing, fragment = Just ("tab=projects&task-graph=" ++ Url.percentEncode projectId ++ "&graph-entry=" ++ Url.percentEncode id) }
