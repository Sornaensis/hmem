module TimestampDisplay exposing (Local, Reply, Request, State, batchSize, dayKey, empty, lookup, receive, reconcile)

{-| Presentation-only values. Sources remain exact API strings, including offsets
and fractions. The cache contains only current display demand; at most one batch
is outstanding, even when its navigation context has been retired.
-}

import Dict exposing (Dict)
import Set exposing (Set)


type alias Local =
    { text : String, day : String }


type alias Request =
    { context : String, id : Int, sources : List String }


type alias Reply =
    { context : String, id : Int, values : List ( String, Maybe Local ) }


type alias State =
    { context : String
    , demand : Set String
    , cache : Dict String (Maybe Local)
    , flight : Maybe Request
    , nextId : Int
    }


batchSize : Int
batchSize =
    128


empty : State
empty =
    { context = "", demand = Set.empty, cache = Dict.empty, flight = Nothing, nextId = 1 }


lookup : State -> String -> String
lookup state source =
    if String.trim source == "" then
        "—"

    else
        case Dict.get source state.cache of
            Just (Just local) ->
                local.text

            Just Nothing ->
                "Invalid timestamp"

            Nothing ->
                "…"


dayKey : State -> String -> String
dayKey state source =
    case Dict.get source state.cache of
        Just (Just local) ->
            local.day

        Just Nothing ->
            "Unknown date"

        Nothing ->
            if String.trim source == "" then
                "Unknown date"

            else
                "Date pending"


reconcile : String -> List String -> State -> ( State, Maybe Request )
reconcile context sources previous =
    let
        demand =
            sources |> List.filter (String.trim >> (/=) "") |> Set.fromList

        cache =
            if context == previous.context then
                Dict.filter (\source _ -> Set.member source demand) previous.cache

            else
                Dict.empty

        current =
            { previous | context = context, demand = demand, cache = cache }

        missing =
            Set.diff demand (Set.fromList (Dict.keys cache)) |> Set.toList |> List.take batchSize
    in
    case ( current.flight, missing ) of
        ( Nothing, _ :: _ ) ->
            let
                request =
                    { context = context, id = current.nextId, sources = missing }
            in
            ( { current | flight = Just request, nextId = current.nextId + 1 }, Just request )

        _ ->
            ( current, Nothing )


receive : Reply -> State -> State
receive reply state =
    case state.flight of
        Just request ->
            if reply.context == request.context && reply.id == request.id then
                let
                    values =
                        Dict.fromList reply.values

                    cache =
                        if reply.context == state.context then
                            request.sources
                                |> List.filter (\source -> Set.member source state.demand)
                                |> List.foldl (\source -> Dict.insert source (Dict.get source values |> Maybe.withDefault Nothing)) state.cache

                        else
                            state.cache
                in
                { state | flight = Nothing, cache = cache }

            else
                state

        Nothing ->
            state
