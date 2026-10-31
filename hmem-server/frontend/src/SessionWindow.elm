module SessionWindow exposing (Gap, Policy, Result, compose, composeRange, reconcileGaps)

import Set
import Char


{-| Source and presentation identity are deliberately separate. A native envelope
pair can change its displayed source row while retaining one proven occurrence.
Neither text nor timestamps participate in window reconciliation.
-}
type alias Policy a =
    { identity : a -> String
    , source : a -> String
    , bytes : a -> Int
    }


type alias Result a =
    { rows : List a
    , removedBefore : List a
    , removedAfter : List a
    , addedBefore : List a
    , addedAfter : List a
    }


type alias Gap =
    { before : String, after : String, pixels : Float, group : String }


{-| Omitted payload is a bounded range descriptor, not a page cache. Refilled
intervals and mounted records cut it; ranges with no mounted record between them
coalesce. There can therefore be at most one more range than mounted records.
Pixel proportions are estimates; source cursors remain exact strings.
-}
reconcileGaps : Maybe ( String, String ) -> List ( String, String ) -> List Gap -> List Gap -> List Gap
reconcileGaps represented mounted added old =
    let
        components value =
            case String.split ":" value of
                raw :: block :: rest -> (raw, (String.toFloat block |> Maybe.withDefault 0)+(if List.isEmpty rest then 0 else 1))
                raw :: _ -> (raw,0)
                [] -> ("0",0)
        distance first last =
            let (firstRaw,firstBlock)=components first
                (lastRaw,lastBlock)=components last
                rawDistance=if firstRaw==lastRaw then 0 else (String.toFloat lastRaw |> Maybe.withDefault 0)-(String.toFloat firstRaw |> Maybe.withDefault 0)
            in rawDistance*10000000+lastBlock-firstBlock
        cut (before,after) gap =
            if compareSource gap.after before /= GT || compareSource gap.before after /= LT then [gap]
            else
                let
                    extent = distance gap.before gap.after
                    fraction part = if extent>0 then clamp 0 1 (part/extent) else 0.5
                    left = if compareSource gap.before before == LT then [{gap | after=before,pixels=gap.pixels*fraction (distance gap.before before)}] else []
                    right = if compareSource gap.after after == GT then [{gap | before=after,pixels=gap.pixels*fraction (distance after gap.after)}] else []
                in left ++ right
        refreshed = represented |> Maybe.map (\range -> List.concatMap (cut range) old) |> Maybe.withDefault old
        uncovered = List.foldl (\range ranges -> List.concatMap (cut range) ranges) (refreshed ++ added) mounted
        valid = List.filter (\gap -> compareSource gap.before gap.after == LT) uncovered |> List.sortWith (\a b -> compareSource a.before b.before)
        append gap ranges = case ranges of
            previous :: rest ->
                let between = List.any (\(start,finish) -> compareSource start gap.before == LT && compareSource finish previous.after == GT) mounted in
                if compareSource gap.before previous.after /= GT || not between then
                    {before=previous.before,after=if compareSource previous.after gap.after == GT then previous.after else gap.after,pixels=min 10000000 (previous.pixels+gap.pixels),group=if previous.group==gap.group then gap.group else ""} :: rest
                else gap :: ranges
            [] -> [gap]
        merged = List.foldl append [] valid |> List.reverse
        pixels = List.sum (List.map .pixels merged)
    in if pixels <= 10000000 then merged else List.map (\gap -> {gap | pixels=gap.pixels*10000000/pixels}) merged


compareSource : String -> String -> Order
compareSource first second =
    let
        withoutZeros parts = case parts of
            '0' :: rest -> withoutZeros rest
            _ -> String.fromList parts
        normalized value =
            String.split ":" value |> List.map (\part ->
                if String.all Char.isDigit part then
                    let number = withoutZeros (String.toList part) in
                    String.padLeft 4 '0' (String.fromInt (String.length number)) ++ ":" ++ number
                else part) |> String.join ":"
    in compare (normalized first) (normalized second)


compose : Policy a -> Int -> Int -> Maybe String -> Bool -> Bool -> List a -> List a -> Result a
compose =
    composeRange Nothing


composeRange : Maybe ( String, String ) -> Policy a -> Int -> Int -> Maybe String -> Bool -> Bool -> List a -> List a -> Result a
composeRange authoritative policy rowLimit byteLimit anchor following earlier old incoming =
    let
        sources = Set.fromList (List.map policy.source incoming)
        identities = Set.fromList (List.map policy.identity incoming)
        covered row = authoritative |> Maybe.map (\(before,after) -> compareSource (policy.source row) before /= LT && compareSource (policy.source row) after == LT) |> Maybe.withDefault False
        retained = List.filter (\row -> not (covered row || Set.member (policy.source row) sources || Set.member (policy.identity row) identities)) old
        joined = List.sortWith (\a b -> compareSource (policy.source a) (policy.source b)) (retained ++ incoming)
        count = List.length joined
        anchorIndex =
            anchor |> Maybe.andThen (\key -> List.indexedMap Tuple.pair joined |> List.filter (\(_,row) -> compareSource (policy.source row) key /= LT) |> List.head |> Maybe.map Tuple.first)
        desired =
            if following then max 0 (count - rowLimit)
            else case anchorIndex of
                Just at -> clamp 0 (max 0 (count - rowLimit)) (at - rowLimit // 3)
                Nothing -> if earlier then 0 else max 0 (count - rowLimit)
        trim begin finish =
            let
                rows = List.take (finish-begin) (List.drop begin joined)
                cost = List.sum (List.map policy.bytes rows)
                protected = if following then count-1 else anchorIndex |> Maybe.withDefault (if earlier then 0 else count-1)
            in
            if cost <= byteLimit || begin >= finish then (begin,finish)
            else if protected-begin > finish-1-protected then trim (begin+1) finish
            else trim begin (finish-1)
        ( first, last ) = trim desired (min count (desired+rowLimit))
        oldFirst = List.head old |> Maybe.map policy.source
        oldLast = List.reverse old |> List.head |> Maybe.map policy.source
    in
    { rows = List.take (last-first) (List.drop first joined)
    , removedBefore = List.take first joined
    , removedAfter = List.drop last joined
    , addedBefore = oldFirst |> Maybe.map (\key -> List.filter (\row -> compareSource (policy.source row) key == LT) incoming) |> Maybe.withDefault []
    , addedAfter = oldLast |> Maybe.map (\key -> List.filter (\row -> compareSource (policy.source row) key == GT) incoming) |> Maybe.withDefault []
    }
