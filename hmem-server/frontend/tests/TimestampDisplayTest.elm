module TimestampDisplayTest exposing (suite)

import Dict
import Expect
import Set
import Test exposing (Test, describe, test)
import TimestampDisplay as Display


local : Display.Local
local =
    { text = "2026-01-01 01:00", day = "2026-01-01" }


start : String -> List String -> Display.State -> Display.State
start owner sources state =
    Display.reconcile owner sources state |> Tuple.first


reply : Display.State -> List ( String, Maybe Display.Local ) -> Display.State
reply state values =
    case state.flight of
        Just request ->
            Display.receive { context = request.context, id = request.id, values = values } state

        Nothing ->
            state


suite : Test
suite =
    describe "Browser timestamp display state"
        [ test "deduplicates exact sources, preserves offset/fraction identity, and skips absent values" <|
            \_ ->
                let
                    first = start "a" [ "", " ", "2026-01-01T00:00:00.000001Z", "2026-01-01T01:00:00+01:00", "2026-01-01T00:00:00.000001Z" ] Display.empty
                in
                Expect.equal ( 2, Just 2, 2 )
                    ( Set.size first.demand, Maybe.map (.sources >> List.length) first.flight, first.nextId )
        , test "returns truthful pending, missing and invalid fallbacks and a local grouping key" <|
            \_ ->
                let
                    state = start "a" [ "valid", "bad" ] Display.empty |> (\s -> reply s [ ( "valid", Just local ), ( "bad", Nothing ) ])
                in
                Expect.equal
                    [ "—", "…", "Invalid timestamp", local.text, local.day, "Unknown date", "Date pending", "Unknown date" ]
                    [ Display.lookup state "", Display.lookup state "new", Display.lookup state "bad", Display.lookup state "valid", Display.dayKey state "valid", Display.dayKey state "bad", Display.dayKey state "new", Display.dayKey state "" ]
        , test "one bounded flight drains demand without starving values beyond the first batch" <|
            \_ ->
                let
                    sources = List.range 1 300 |> List.map String.fromInt
                    drain state =
                        case state.flight of
                            Just request ->
                                if List.length request.sources > Display.batchSize then
                                    state
                                else
                                    reply state (List.map (\source -> ( source, Just local )) request.sources)
                                        |> start "a" sources
                                        |> drain

                            Nothing ->
                                state
                    initial = start "a" sources Display.empty
                    final = drain initial
                in
                Expect.equal ( Just 128, 300, ( Nothing, 4 ) ) ( Maybe.map (.sources >> List.length) initial.flight, Dict.size final.cache, ( final.flight, final.nextId ) )
        , test "new paginated/live demand waits for the existing flight then is admitted" <|
            \_ ->
                let
                    first = start "a" [ "old" ] Display.empty
                    waiting = start "a" [ "old", "new" ] first
                    completed = reply waiting [ ( "old", Just local ) ] |> start "a" [ "old", "new" ]
                in
                Expect.equal ( first.flight, Just [ "new" ], Just (Just local) )
                    ( waiting.flight, Maybe.map .sources completed.flight, Dict.get "old" completed.cache )
        , test "retired demand cannot enter cache and successful cache is pruned to current demand" <|
            \_ ->
                let
                    first = start "a" [ "keep", "retire" ] Display.empty
                    state = start "a" [ "keep" ] first |> (\s -> reply s [ ( "keep", Just local ), ( "retire", Just local ), ( "unsolicited", Just local ) ])
                    emptyDemand = start "a" [] state
                in
                Expect.equal ( [ "keep" ], 0, 0 ) ( Dict.keys state.cache, Dict.size emptyDemand.cache, Set.size emptyDemand.demand )
        , test "context retirement holds the physical flight and monotonic identity until completion" <|
            \_ ->
                let
                    first = start "session-1" [ "same" ] Display.empty
                    retired = start "session-2" [ "same" ] first
                    after = reply retired [ ( "same", Just local ) ] |> start "session-2" [ "same" ]
                in
                Expect.equal ( first.flight, 0, ( Just ( "session-2", 2 ), "…" ) )
                    ( retired.flight, Dict.size after.cache, ( Maybe.map (\r -> ( r.context, r.id )) after.flight, Display.lookup after "same" ) )
        , test "wrong context/id and duplicate replies cannot replace current state" <|
            \_ ->
                let
                    state = start "a" [ "source" ] Display.empty
                    wrongId = Display.receive { context = "a", id = 0, values = [ ( "source", Just local ) ] } state
                    wrongContext = Display.receive { context = "b", id = 1, values = [ ( "source", Just local ) ] } state
                    done = reply state [ ( "source", Just local ) ]
                    duplicate = Display.receive { context = "a", id = 1, values = [ ( "source", Nothing ) ] } done
                in
                Expect.equal ( state, state, done ) ( wrongId, wrongContext, duplicate )
        ]
