module ObservationTree exposing (State, Request, Branch, init, retire, begin, receive, visiblePaths, toggle, select, recoverFocus, reveal, parent, ancestors)

import Api
import Dict exposing (Dict)
import Set exposing (Set)


type alias Request =
    { lifetime : String, token : Int, prefix : String, offset : Int }


type alias Branch =
    { lifetime : String, paths : List String, hasMore : Bool, nextOffset : Int, request : Maybe Request, error : Maybe String }


type alias State =
    { lifetime : String, displayLifetime : String, nextToken : Int, physical : List Request, totals : Maybe ( Int, Int )
    , nodes : Dict String Api.ObservationSubjectTreeNode, branches : Dict String Branch
    , expanded : Set String, focused : Maybe String, selected : Maybe Api.ObservationSubject
    , revealTarget : Maybe { subject : Api.ObservationSubject, observationId : String }
    }


init : State
init =
    { lifetime = "", displayLifetime = "", nextToken = 1, physical = [], totals = Nothing, nodes = Dict.empty, branches = Dict.empty
    , expanded = Set.empty, focused = Nothing, selected = Nothing, revealTarget = Nothing }


{-| Logical retirement never releases physical occupancy. Only the exact
callback can do that, even after mode, workspace, or authority changes.
-}
retire : String -> State -> State
retire lifetime state =
    { state | lifetime = lifetime, nextToken = state.nextToken + 1
        , branches = Dict.map (\_ branch -> { branch | request = Nothing }) state.branches }


emptyBranch : Branch
emptyBranch =
    { lifetime = "", paths = [], hasMore = False, nextOffset = 0, request = Nothing, error = Nothing }


begin : String -> Int -> State -> ( State, Maybe Request )
begin prefix offset state =
    let branch = Dict.get prefix state.branches |> Maybe.withDefault emptyBranch in
    if List.length state.physical >= 4 || branch.request /= Nothing || offset < 0 || offset > 2147483647 || (prefix /= "" && state.lifetime /= state.displayLifetime) then ( state, Nothing )
    else
        let request = { lifetime = state.lifetime, token = state.nextToken, prefix = prefix, offset = offset } in
        ( { state | nextToken = state.nextToken + 1, physical = request :: state.physical
            , branches = Dict.insert prefix { branch | request = Just request, error = Nothing } state.branches }, Just request )


receive : Request -> Result String Api.ObservationSubjectTreePage -> State -> State
receive request result state =
    let
        settled = { state | physical = List.filter ((/=) request) state.physical }
        branch = Dict.get request.prefix state.branches |> Maybe.withDefault emptyBranch
        fail message = { settled | branches = Dict.insert request.prefix { branch | request = Nothing, error = Just message } settled.branches }
    in
    if request.lifetime /= state.lifetime || branch.request /= Just request then settled
    else
        case result of
            Err message -> fail message
            Ok page ->
                let
                    paths = List.map .path page.items
                    valid node = node.path /= "" && not (String.startsWith "/" node.path) && not (String.contains "\\" node.path)
                        && List.all (\part -> not (List.member part [ "", ".", ".." ])) (String.split "/" node.path)
                        && parent node.path == request.prefix && node.segment == (String.split "/" node.path |> List.reverse |> List.head |> Maybe.withDefault "")
                        && node.observationCount > 0 && node.observationCount <= page.matchCount
                        && List.all (\terminal -> terminal.subject == node.path && terminal.observationCount > 0 && terminal.observationCount <= node.observationCount) node.terminals
                    next = request.offset + List.length paths
                    progress = not page.hasMore || (List.length paths == 200 && List.any (\path -> request.offset == 0 || not (List.member path branch.paths)) paths)
                    newPaths = if request.offset == 0 then paths else branch.paths ++ List.filter (\path -> not (List.member path branch.paths)) paths
                    base = if request.prefix == "" && request.offset == 0 && request.lifetime /= state.displayLifetime then
                        { settled | nodes = Dict.empty, branches = Dict.empty }
                        else settled
                    replaced = { base | nodes = List.foldl (\node -> Dict.insert node.path node) base.nodes page.items
                        , displayLifetime = if request.prefix == "" then request.lifetime else settled.displayLifetime
                        , totals = if request.prefix == "" then Just (page.totalCount, page.matchCount) else settled.totals
                        , branches = Dict.insert request.prefix { lifetime = request.lifetime, paths = newPaths, hasMore = page.hasMore, nextOffset = next, request = Nothing
                            , error = if page.hasMore && next > 2147483647 then Just "Subject Tree is incomplete: the supported offset range is exhausted." else Nothing } base.branches }
                in
                if List.length paths > 200 || Set.size (Set.fromList paths) /= List.length paths || not (List.all valid page.items) || not progress then
                    fail "Subject Tree is incomplete: the page made no valid progress."
                else recoverFocus state replaced


parent : String -> String
parent path =
    path |> String.split "/" |> List.reverse |> List.drop 1 |> List.reverse |> String.join "/"


ancestors : String -> List String
ancestors path =
    let go current = if current == "" then [] else go (parent current) ++ [ current ] in
    go path


visiblePaths : State -> List String
visiblePaths state =
    let walk prefix =
            Dict.get prefix state.branches |> Maybe.map (\branch -> if branch.lifetime /= state.displayLifetime then [] else branch.paths |> List.concatMap (\path ->
                path :: (if Set.member path state.expanded then walk path else []))) |> Maybe.withDefault []
    in walk ""


recoverFocus : State -> State -> State
recoverFocus previous state =
    let
        visible = visiblePaths state
        old = Maybe.withDefault "" previous.focused
        nearestAncestor = ancestors old |> List.reverse |> List.filter (\path -> List.member path visible) |> List.head
        oldIndex = visiblePaths previous |> List.indexedMap Tuple.pair |> List.filter (Tuple.second >> (==) old) |> List.head |> Maybe.map Tuple.first |> Maybe.withDefault 0
        nearest = visible |> List.drop (Basics.min oldIndex (Basics.max 0 (List.length visible - 1))) |> List.head
    in { state | focused = if List.member old visible then Just old else if nearestAncestor /= Nothing then nearestAncestor else nearest }


toggle : String -> State -> State
toggle path state =
    recoverFocus state { state | expanded = if Set.member path state.expanded then Set.remove path state.expanded else Set.insert path state.expanded }


select : String -> Maybe Api.ObservationSubject -> State -> State
select path subject state =
    { state | focused = Just path, selected = subject }


reveal : Api.ObservationSubject -> String -> State -> State
reveal subject observationId state =
    { state | expanded = Set.union state.expanded (Set.fromList (ancestors subject.subject))
        , focused = Just subject.subject, selected = Just subject
        , revealTarget = Just { subject = subject, observationId = observationId } }
