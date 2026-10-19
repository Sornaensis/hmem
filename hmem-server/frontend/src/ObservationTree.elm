module ObservationTree exposing (State, Request, Branch, Terminal, init, retire, revalidate, begin, receive, beginTerminal, receiveTerminal, terminalKey, visiblePaths, toggle, select, recoverFocus, reconcileSelection, reveal, revealPath, parent, ancestors)

import Api
import Dict exposing (Dict)
import Set exposing (Set)


type alias Request =
    { lifetime : String, token : Int, prefix : String, offset : Int, kind : Maybe Api.SubjectKind }


type alias Branch =
    { lifetime : String, paths : List String, hasMore : Bool, nextOffset : Int, request : Maybe Request, error : Maybe String, stagedPaths : Maybe (List String), stagedNodes : Dict String Api.ObservationSubjectTreeNode, targetOffset : Int }


type alias State =
    { lifetime : String, displayLifetime : String, nextToken : Int, physical : List Request, totals : Maybe ( Int, Int )
    , nodes : Dict String Api.ObservationSubjectTreeNode, branches : Dict String Branch, terminals : Dict String Terminal, refreshing : Bool
    , expanded : Set String, focused : Maybe String, selectedPath : Maybe String, selected : Maybe Api.ObservationSubject
    , revealTarget : Maybe { subject : Api.ObservationSubject, observationId : Maybe String, focus : Bool }
    , focusPending : Maybe String, revealFocus : Bool, resolvedPath : Maybe { subject : Api.ObservationSubject, owner : String }
    , lastOwner : Maybe ( String, Maybe Api.SubjectKind )
    }


type alias Terminal =
    { subject : Api.ObservationSubject, lifetime : String, items : List Api.Observation, hasMore : Bool, nextOffset : Int, request : Maybe Request, error : Maybe String, stagedItems : Maybe (List Api.Observation), targetOffset : Int }


terminalKey : Api.ObservationSubject -> String
terminalKey subject = Api.subjectKindToString subject.subjectKind ++ ":" ++ subject.subject


init : State
init =
    { lifetime = "", displayLifetime = "", nextToken = 1, physical = [], totals = Nothing, nodes = Dict.empty, branches = Dict.empty
    , terminals = Dict.empty, refreshing = False, expanded = Set.empty, focused = Nothing, selectedPath = Nothing, selected = Nothing, revealTarget = Nothing, focusPending = Nothing, revealFocus = True, resolvedPath = Nothing, lastOwner = Nothing }


{-| Logical retirement never releases physical occupancy. Only the exact
callback can do that, even after mode, workspace, or authority changes.
-}
retire : String -> State -> State
retire lifetime state =
    { state | lifetime = lifetime, nextToken = state.nextToken + 1
        , refreshing = False, branches = Dict.map (\_ branch -> { branch | request = Nothing, stagedPaths = Nothing, stagedNodes = Dict.empty, error = Nothing }) state.branches
        , terminals = Dict.map (\_ terminal -> { terminal | request = Nothing, stagedItems = Nothing, error = Nothing }) state.terminals }


revalidate : String -> State -> State
revalidate lifetime state =
    let retired = retire lifetime state in { retired | refreshing = True }


emptyBranch : Branch
emptyBranch =
    { lifetime = "", paths = [], hasMore = False, nextOffset = 0, request = Nothing, error = Nothing, stagedPaths = Nothing, stagedNodes = Dict.empty, targetOffset = 0 }


begin : String -> Int -> State -> ( State, Maybe Request )
begin prefix offset state =
    let branch = Dict.get prefix state.branches |> Maybe.withDefault emptyBranch in
    if List.length state.physical >= 4 || branch.request /= Nothing || offset < 0 || offset > 2147483647 || (prefix /= "" && state.lifetime /= state.displayLifetime) then ( state, Nothing )
    else
        let request = { lifetime = state.lifetime, token = state.nextToken, prefix = prefix, offset = offset, kind = Nothing } in
        ( { state | nextToken = state.nextToken + 1, physical = request :: state.physical
            , lastOwner = if prefix == "" then state.lastOwner else Just (prefix, Nothing)
            , branches = Dict.insert prefix { branch | request = Just request, error = Nothing
                , stagedPaths = if offset == 0 && state.refreshing && branch.lifetime /= state.lifetime then Just [] else branch.stagedPaths
                , stagedNodes = if offset == 0 && state.refreshing && branch.lifetime /= state.lifetime then Dict.empty else branch.stagedNodes
                , targetOffset = if offset == 0 then Basics.max 200 (Basics.max branch.nextOffset branch.targetOffset) else branch.targetOffset } state.branches }, Just request )


beginTerminal : Api.ObservationSubject -> Int -> State -> ( State, Maybe Request )
beginTerminal subject offset state =
    let key = terminalKey subject
        terminal = Dict.get key state.terminals |> Maybe.withDefault { subject = subject, lifetime = "", items = [], hasMore = False, nextOffset = 0, request = Nothing, error = Nothing, stagedItems = Nothing, targetOffset = 0 }
    in
    if List.length state.physical >= 4 || terminal.request /= Nothing || offset < 0 || offset > 100000 then (state, Nothing)
    else
        let request = { lifetime = state.lifetime, token = state.nextToken, prefix = subject.subject, offset = offset, kind = Just subject.subjectKind } in
        ({state | nextToken = state.nextToken + 1, physical = request :: state.physical, lastOwner = Just (subject.subject, Just subject.subjectKind), terminals = Dict.insert key {terminal | request = Just request, error = Nothing
            , stagedItems = if offset == 0 && state.refreshing && terminal.lifetime /= state.lifetime then Just [] else terminal.stagedItems
            , targetOffset = if offset == 0 then Basics.max 200 (Basics.max terminal.nextOffset terminal.targetOffset) else terminal.targetOffset} state.terminals}, Just request)


receiveTerminal : Request -> Result String (Api.PaginatedResult Api.Observation) -> State -> State
receiveTerminal request result state =
    let settled = {state | physical = List.filter ((/=) request) state.physical} in
    case request.kind of
        Nothing -> settled
        Just kind ->
            let subject = {subjectKind = kind, subject = request.prefix}
                key = terminalKey subject
            in
            case Dict.get key state.terminals of
                Nothing -> settled
                Just terminal ->
                    if request.lifetime /= state.lifetime || terminal.request /= Just request then settled
                    else
                        let fail message = {settled | terminals = Dict.insert key {terminal | request = Nothing, error = Just message} settled.terminals} in
                        case result of
                            Err message -> fail message
                            Ok page ->
                                let ids = List.map .id page.items
                                    previous = Maybe.withDefault terminal.items terminal.stagedItems
                                    prior = List.map .id previous
                                    next = request.offset + List.length ids
                                    items = if request.offset == 0 then page.items else previous ++ List.filter (\item -> not (List.member item.id prior)) page.items
                                    demandedMissing = state.revealTarget |> Maybe.map (\target -> target.subject == subject && (target.observationId |> Maybe.map (\identity -> not (List.any (\item -> item.id == identity) items)) |> Maybe.withDefault False)) |> Maybe.withDefault False
                                    completed = terminal.stagedItems == Nothing || not page.hasMore || (next >= terminal.targetOffset && not demandedMissing)
                                in
                                if List.length ids > 200 || Set.size (Set.fromList ids) /= List.length ids || not (List.all (\item -> List.member subject item.subjects) page.items)
                                    || (page.hasMore && (List.length ids /= 200 || not (List.any (\identity -> request.offset == 0 || not (List.member identity prior)) ids))) then fail "Subject observations are incomplete: the page made no valid progress."
                                else {settled | terminals = Dict.insert key {terminal | lifetime = if completed then request.lifetime else terminal.lifetime, items = if completed then items else terminal.items, stagedItems = if completed then Nothing else Just items, hasMore = page.hasMore, nextOffset = next, request = Nothing
                                    , error = if page.hasMore && next > 100000 then Just "Subject observations are incomplete: the supported offset range is exhausted." else Nothing} settled.terminals}


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
                    knownPaths = Maybe.withDefault branch.paths branch.stagedPaths
                    progress = not page.hasMore || (List.length paths == 200 && List.any (\path -> request.offset == 0 || not (List.member path knownPaths)) paths)
                    newPaths = if request.offset == 0 then paths else knownPaths ++ List.filter (\path -> not (List.member path knownPaths)) paths
                    demandedChild = state.revealTarget |> Maybe.andThen (\target -> ancestors target.subject.subject |> List.filter (parent >> (==) request.prefix) |> List.head)
                    demandedMissing = demandedChild |> Maybe.map (\path -> not (List.member path newPaths)) |> Maybe.withDefault False
                    completed = branch.stagedPaths == Nothing || not page.hasMore || (request.prefix == "" && next >= branch.targetOffset && not demandedMissing)
                    stagedNodes = List.foldl (\node -> Dict.insert node.path node) branch.stagedNodes page.items
                    acceptedNodes = if not completed then [] else if branch.stagedPaths == Nothing then page.items else Dict.values stagedNodes
                    base = if request.prefix == "" && request.offset == 0 && request.lifetime /= state.displayLifetime && not state.refreshing then
                        { settled | nodes = Dict.empty, branches = Dict.empty, terminals = Dict.empty }
                        else settled
                    replaced = { base | nodes = List.foldl (\node -> Dict.insert node.path node) base.nodes acceptedNodes
                        , displayLifetime = if request.prefix == "" && completed then request.lifetime else settled.displayLifetime
                        , totals = if request.prefix == "" then Just (page.totalCount, page.matchCount) else settled.totals
                        , branches = Dict.insert request.prefix { lifetime = if completed then request.lifetime else branch.lifetime, paths = if completed then newPaths else branch.paths, stagedPaths = if completed then Nothing else Just newPaths, stagedNodes = if completed then Dict.empty else stagedNodes, targetOffset = branch.targetOffset, hasMore = page.hasMore, nextOffset = next, request = Nothing
                            , error = if page.hasMore && next > 2147483647 then Just "Subject Tree is incomplete: the supported offset range is exhausted." else Nothing } base.branches }
                    removed = if completed then branch.paths |> List.filter (\path -> not (List.member path newPaths)) else []
                    retained path = not (List.any (\gone -> path == gone || String.startsWith (gone ++ "/") path) removed)
                    pruned = {replaced | nodes = Dict.filter (\path _ -> retained path) replaced.nodes
                        , branches = Dict.filter (\path _ -> retained path) replaced.branches
                        , terminals = Dict.filter (\_ terminal -> retained terminal.subject.subject && (Dict.get terminal.subject.subject replaced.nodes |> Maybe.map (\node -> List.any (\value -> value.subjectKind == terminal.subject.subjectKind) node.terminals) |> Maybe.withDefault False)) replaced.terminals}
                in
                if List.length paths > 200 || Set.size (Set.fromList paths) /= List.length paths || not (List.all valid page.items) || not progress then
                    fail "Subject Tree is incomplete: the page made no valid progress."
                else recoverFocus state pruned


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
            Dict.get prefix state.branches |> Maybe.map (\branch -> if branch.lifetime /= state.displayLifetime && not state.refreshing then [] else branch.paths |> List.concatMap (\path ->
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
        focus = if state.revealTarget /= Nothing then state.focused else if List.member old visible then Just old else if nearestAncestor /= Nothing then nearestAncestor else nearest
        selected = if state.revealTarget /= Nothing then state.selected else state.selected |> Maybe.andThen (\subject -> if (Dict.get subject.subject state.nodes |> Maybe.map (\node -> List.any (\terminal -> terminal.subjectKind == subject.subjectKind) node.terminals) |> Maybe.withDefault False) then Just subject else Nothing)
        selectedPath = (reconcileSelection state).selectedPath
    in { state | focused = focus, selectedPath = selectedPath, selected = selected, focusPending = if state.revealTarget /= Nothing then Nothing else if previous.focused /= Nothing && focus /= previous.focused then focus else state.focusPending }


reconcileSelection : State -> State
reconcileSelection state =
    let absent path = ancestors path |> List.any (\part -> Dict.get (parent part) state.branches |> Maybe.map (\branch -> branch.lifetime == state.lifetime && branch.stagedPaths == Nothing && not branch.hasMore && branch.request == Nothing && branch.error == Nothing && not (List.member part branch.paths)) |> Maybe.withDefault False)
    in {state | selectedPath = state.selectedPath |> Maybe.andThen (\path -> if absent path then Nothing else Just path)}


toggle : String -> State -> State
toggle path state =
    let closing = Set.member path state.expanded
        hidden prefix = prefix == path || String.startsWith (path ++ "/") prefix
        updated = recoverFocus state { state | expanded = if closing then Set.remove path state.expanded else Set.insert path state.expanded
            , branches = Dict.map (\prefix branch -> if closing && hidden prefix then {branch | request = Nothing} else if not closing && prefix == path then {branch | error = Nothing} else branch) state.branches
            , terminals = Dict.map (\_ terminal -> if closing && hidden terminal.subject.subject then {terminal | request = Nothing} else if not closing && terminal.subject.subject == path then {terminal | error = Nothing} else terminal) state.terminals }
    in {updated | focusPending = updated.focused}


select : String -> Maybe Api.ObservationSubject -> State -> State
select path subject state =
    { state | focused = Just path, selectedPath = Just path, selected = subject }


reveal : Api.ObservationSubject -> String -> State -> State
reveal subject observationId state =
    revealIntent subject (Just observationId) state


revealPath : Api.ObservationSubject -> State -> State
revealPath subject state =
    revealIntent subject Nothing state


revealIntent : Api.ObservationSubject -> Maybe String -> State -> State
revealIntent subject observationId state =
    { state | expanded = Set.union state.expanded (Set.fromList (ancestors subject.subject))
        , focused = Just subject.subject, selected = Just subject, resolvedPath = Nothing
        , revealTarget = Just { subject = subject, observationId = observationId, focus = state.revealFocus } }
