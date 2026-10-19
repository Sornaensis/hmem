module ObservationTreeTest exposing (suite)

import Api
import Dict
import Expect
import ObservationTree as Tree
import Set
import Test exposing (..)


node path children =
    { path = path, segment = String.split "/" path |> List.reverse |> List.head |> Maybe.withDefault ""
    , hasChildren = children, observationCount = 1, terminals = [] }


accept prefix nodes state =
    let ( started, request ) = Tree.begin prefix 0 state in
    request |> Maybe.map (\guard -> Tree.receive guard (Ok { items = nodes, hasMore = False, totalCount = 10, matchCount = 10 }) started) |> Maybe.withDefault started


populated =
    Tree.init |> Tree.retire "repository/session/authority/query"
        |> accept "" [ node "a" True, node "b" True ]
        |> accept "a" [ node "a/x" True ]
        |> accept "a/x" [ node "a/x/leaf" False ]
        |> accept "b" [ node "b/y" False ]


branchPage offset paths more state =
    let (started, request) = Tree.begin "" offset state in
    request |> Maybe.map (\guard -> Tree.receive guard (Ok {items = List.map (\path -> node path False) paths, hasMore = more, totalCount = 1000, matchCount = 1000}) started) |> Maybe.withDefault started


subject = {subjectKind = Api.SubjectFile, subject = "a"}


observation number =
    { id = String.fromInt number, workspaceId = "workspace", subjects = [subject], subjectKind = Api.SubjectFile, subject = "a", gitSha = String.repeat 40 "a"
    , content = "Evidence", contentVersion = "10000000-0000-4000-8000-000000000001", latestSequence = 1, currentProvenance = Nothing, provenanceMatch = Nothing
    , createdAt = "2026-01-01T00:00:00Z", updatedAt = "2026-01-01T00:00:00Z" }


terminalPage offset numbers more state =
    let (started, request) = Tree.beginTerminal subject offset state in
    request |> Maybe.map (\guard -> Tree.receiveTerminal guard (Ok {items = List.map observation numbers, hasMore = more}) started) |> Maybe.withDefault started


suite : Test
suite = describe "Subject Tree explorer state"
    [ test "path selection survives collapse and partial or unloaded catalogs without becoming focus fallback" <| \_ ->
        let
            chosen = populated |> Tree.toggle "a" |> Tree.toggle "a/x" |> Tree.select "a/x/leaf" Nothing
            collapsed = Tree.toggle "a" chosen
            partial = chosen |> Tree.revalidate "new" |> branchPage 0 (List.range 1 200 |> List.map (\n -> "other-" ++ String.fromInt n)) True
            absent = partial |> branchPage 200 ["other-last"] False
            unknown = {chosen | nodes = Dict.remove "a/x/leaf" chosen.nodes, branches = Dict.remove "a/x" chosen.branches} |> Tree.recoverFocus chosen
        in Expect.all
            [ \_ -> collapsed.selectedPath |> Expect.equal (Just "a/x/leaf")
            , \_ -> collapsed.focused |> Expect.equal (Just "a")
            , \_ -> partial.selectedPath |> Expect.equal (Just "a/x/leaf")
            , \_ -> unknown.selectedPath |> Expect.equal (Just "a/x/leaf")
            , \_ -> absent.selectedPath |> Expect.equal Nothing
            , \_ -> absent.focused |> Maybe.map (String.startsWith "other-") |> Expect.equal (Just True)
            , \_ -> List.length partial.physical |> Expect.equal 0 ] ()
    , test "partial branch refresh stages terminal metadata with paths before pruning a displayed typed card" <| \_ ->
        let
            typed path terminalKinds = let base = node path False in {base | terminals = List.map (\kind -> {subjectKind = kind, subject = path, observationCount = 1}) terminalKinds}
            fileNode index = typed ("a/" ++ String.padLeft 3 '0' (String.fromInt index)) [Api.SubjectFile]
            firstNode = typed "a/000" [Api.SubjectFile, Api.SubjectGlob]
            branchPageAt offset items more prior =
                let (admitted, guard) = Tree.begin "a" offset prior in
                guard |> Maybe.map (\request -> Tree.receive request (Ok {items = items, hasMore = more, totalCount = 1000, matchCount = 1000}) admitted) |> Maybe.withDefault admitted
            old = Tree.init |> Tree.retire "old" |> accept "" [node "a" True]
                |> branchPageAt 0 (firstNode :: List.map fileNode (List.range 1 199)) False
            glob = {subjectKind = Api.SubjectGlob, subject = "a/000"}
            original = observation 1
            card = {original | subjects = [glob], subjectKind = Api.SubjectGlob, subject = glob.subject}
            (admittedCard, cardGuard) = Tree.beginTerminal glob 0 old
            cached = cardGuard |> Maybe.map (\guard -> Tree.receiveTerminal guard (Ok {items = [card], hasMore = False}) admittedCard) |> Maybe.withDefault admittedCard
            first = cached |> Tree.revalidate "new" |> accept "" [node "a" True]
                |> branchPageAt 0 (List.map fileNode (List.range 0 199)) True
            final = first |> branchPageAt 200 [fileNode 200] False
            kinds current = Dict.get "a/000" current.nodes |> Maybe.map (.terminals >> List.map .subjectKind)
        in Expect.all
            [ \_ -> kinds first |> Expect.equal (Just [Api.SubjectFile, Api.SubjectGlob])
            , \_ -> Dict.member (Tree.terminalKey glob) first.terminals |> Expect.equal True
            , \_ -> Dict.get "a" first.branches |> Maybe.map (.stagedNodes >> Dict.size) |> Expect.equal (Just 200)
            , \_ -> kinds final |> Expect.equal (Just [Api.SubjectFile])
            , \_ -> Dict.member (Tree.terminalKey glob) final.terminals |> Expect.equal False
            , \_ -> Dict.get "a" final.branches |> Maybe.map (.stagedNodes >> Dict.isEmpty) |> Expect.equal (Just True) ] ()
    , test "pending deep reveal suppresses ordinary hidden-path recovery until the target is ready" <| \_ ->
        let
            pending = populated |> Tree.reveal {subjectKind = Api.SubjectFile, subject = "unloaded/deep/main.elm"} "target"
                |> accept "" [node "a" True, node "b" True]
            ordinary = populated |> Tree.toggle "a" |> Tree.toggle "a/x" |> Tree.select "a/x/leaf" Nothing
                |> accept "a/x" []
        in Expect.all
            [ \_ -> pending.focused |> Expect.equal (Just "unloaded/deep/main.elm")
            , \_ -> pending.focusPending |> Expect.equal Nothing
            , \_ -> ordinary.focused |> Expect.equal (Just "a/x")
            , \_ -> ordinary.focusPending |> Expect.equal (Just "a/x") ] ()
    , test "same-query root refresh retains the complete demanded450 rows until the third replacement page" <| \_ ->
        let
            paths start finish = List.range start finish |> List.map (\n -> "path-" ++ String.fromInt n)
            old = Tree.init |> Tree.retire "old" |> branchPage 0 (paths 0 199) True |> branchPage 200 (paths 200 399) True |> branchPage 400 (paths 400 449) False
            first = old |> Tree.revalidate "new" |> branchPage 0 (paths 1 200) True
            second = first |> branchPage 200 (paths 201 400) True
            final = second |> branchPage 400 (paths 401 447) False
        in Expect.all
            [ \_ -> Tree.visiblePaths first |> Expect.equal (Tree.visiblePaths old)
            , \_ -> Tree.visiblePaths second |> Expect.equal (Tree.visiblePaths old)
            , \_ -> Tree.visiblePaths final |> Expect.equal (paths 1 447)
            , \_ -> Dict.member "path-0" final.nodes |> Expect.equal False
            , \_ -> Dict.get "" final.branches |> Maybe.map .targetOffset |> Expect.equal (Just 450) ] ()
    , test "same-query terminal refresh stages the complete demanded span without exposing a200-row prefix" <| \_ ->
        let
            old = Tree.init |> Tree.retire "old" |> terminalPage 0 (List.range 0 199) True |> terminalPage 200 (List.range 200 249) False
            first = old |> Tree.revalidate "new" |> terminalPage 0 (List.range 1 200) True
            final = first |> terminalPage 200 (List.range 201 249) False
            ids state = Dict.get (Tree.terminalKey subject) state.terminals |> Maybe.map (.items >> List.map .id)
        in Expect.all
            [ \_ -> ids first |> Expect.equal (ids old)
            , \_ -> ids final |> Expect.equal (Just (List.range 1 249 |> List.map String.fromInt))
            , \_ -> List.length final.physical |> Expect.equal 0 ] ()
    , test "live root insertion retains selected subtree until its shifted root is discovered" <| \_ ->
        let
            paths = List.range 0 199 |> List.map (\n -> "root-" ++ String.padLeft 3 '0' (String.fromInt n))
            old = Tree.init |> Tree.retire "old" |> branchPage 0 paths True
            target = {subjectKind = Api.SubjectFile, subject = "root-199/leaf"}
            demanded = {old | revealTarget = Just {subject = target, observationId = Just "card", focus = False}}
            first = demanded |> Tree.revalidate "new" |> branchPage 0 ("earlier" :: List.take 199 paths) True
            final = first |> branchPage 200 ["root-199", "root-200"] False
        in Expect.all
            [ \_ -> Tree.visiblePaths first |> Expect.equal paths
            , \_ -> Dict.member "root-199" first.nodes |> Expect.equal True
            , \_ -> Dict.get "" first.branches |> Maybe.map .stagedPaths |> Expect.equal (Just (Just ("earlier" :: List.take 199 paths)))
            , \_ -> Dict.get "" final.branches |> Maybe.map .lifetime |> Expect.equal (Just "new")
            , \_ -> Dict.member "root-199" final.nodes |> Expect.equal True ] ()
    , test "live terminal reorder retains selected card beyond the prior demanded page boundary" <| \_ ->
        let
            old = Tree.init |> Tree.retire "old" |> terminalPage 0 (List.range 0 199) True
            demanded = {old | revealTarget = Just {subject = subject, observationId = Just "0", focus = False}}
            first = demanded |> Tree.revalidate "new" |> terminalPage 0 (List.range 1 200) True
            final = first |> terminalPage 200 [0] False
            ids state = Dict.get (Tree.terminalKey subject) state.terminals |> Maybe.map (.items >> List.map .id)
        in Expect.all
            [ \_ -> ids first |> Expect.equal (ids old)
            , \_ -> Dict.get (Tree.terminalKey subject) first.terminals |> Maybe.map .stagedItems |> Maybe.map ((/=) Nothing) |> Expect.equal (Just True)
            , \_ -> ids final |> Maybe.map (List.member "0") |> Expect.equal (Just True)
            , \_ -> Dict.get (Tree.terminalKey subject) final.terminals |> Maybe.map .lifetime |> Expect.equal (Just "new") ] ()
    , test "mixed branch and typed-terminal owners share four physical slots through retirement" <| \_ ->
        let
            (one, _) = Tree.begin "" 0 populated
            (two, _) = Tree.beginTerminal subject 0 one
            (three, _) = Tree.beginTerminal {subjectKind = Api.SubjectGlob, subject = "a"} 0 two
            (four, _) = Tree.beginTerminal {subjectKind = Api.SubjectFile, subject = "b"} 0 three
            retired = Tree.retire "next" four
            blocked = Tree.begin "" 0 retired |> Tuple.second
            settled = List.head four.physical |> Maybe.map (\request -> Tree.receiveTerminal request (Err "stale failure") retired) |> Maybe.withDefault retired
        in Expect.all
            [ \_ -> blocked |> Expect.equal Nothing
            , \_ -> List.length settled.physical |> Expect.equal 3
            , \_ -> Tree.begin "" 0 settled |> Tuple.second |> Maybe.map .lifetime |> Expect.equal (Just "next") ] ()
    , test "terminal error keeps completed cards, pauses, and retry has a fresh physical identity" <| \_ ->
        let
            old = Tree.init |> Tree.retire "old" |> terminalPage 0 [1,2] False
            (started, request) = Tree.beginTerminal subject 0 (Tree.revalidate "new" old)
            failed = request |> Maybe.map (\guard -> Tree.receiveTerminal guard (Err "unavailable") started) |> Maybe.withDefault started
            (retry, next) = Tree.beginTerminal subject 0 failed
        in Expect.all
            [ \_ -> Dict.get (Tree.terminalKey subject) failed.terminals |> Maybe.map (.items >> List.map .id) |> Expect.equal (Just ["1","2"])
            , \_ -> next |> Maybe.map .token |> Expect.equal (request |> Maybe.map (.token >> (+) 1))
            , \_ -> List.length retry.physical |> Expect.equal 1 ] ()
    , test "collapse retains typed selection and cards but late hidden terminal replies only settle occupancy" <| \_ ->
        let
            leaf = {path = "a", segment = "a", hasChildren = False, observationCount = 2, terminals = [{subjectKind = Api.SubjectFile, subject = "a", observationCount = 2}]}
            old = Tree.init |> Tree.retire "current" |> accept "" [leaf] |> Tree.toggle "a" |> terminalPage 0 [1,2] False |> Tree.select "a" (Just subject)
            (started, request) = Tree.beginTerminal subject 0 old
            collapsed = Tree.toggle "a" started
            stale = request |> Maybe.map (\guard -> Tree.receiveTerminal guard (Ok {items = [observation 99], hasMore = False}) collapsed) |> Maybe.withDefault collapsed
            reopened = Tree.toggle "a" stale
        in Expect.all
            [ \_ -> collapsed.selected |> Expect.equal (Just subject)
            , \_ -> List.length stale.physical |> Expect.equal 0
            , \_ -> Dict.get (Tree.terminalKey subject) reopened.terminals |> Maybe.map (.items >> List.map .id) |> Expect.equal (Just ["1","2"]) ] ()
    , test "int32 branch and legacy terminal offset exhaustion retain hasMore and never wrap" <| \_ ->
        let
            branch = branchPage 2147483647 (List.range 1 200 |> List.map String.fromInt) True (Tree.retire "current" Tree.init)
            terminal = terminalPage 100000 (List.range 1 200) True (Tree.retire "current" Tree.init)
        in Expect.all
            [ \_ -> Dict.get "" branch.branches |> Maybe.map (\value -> value.hasMore && value.error /= Nothing && value.nextOffset > 2147483647) |> Expect.equal (Just True)
            , \_ -> Tree.begin "" 2147483847 branch |> Tuple.second |> Expect.equal Nothing
            , \_ -> Dict.get (Tree.terminalKey subject) terminal.terminals |> Maybe.map (\value -> value.hasMore && value.error /= Nothing && value.nextOffset == 100200) |> Expect.equal (Just True)
            , \_ -> Tree.beginTerminal subject 100200 terminal |> Tuple.second |> Expect.equal Nothing ] ()
    , test "several branches remain open and reopening a parent preserves descendant expansion" <| \_ ->
        let
            open = populated |> Tree.toggle "a" |> Tree.toggle "a/x" |> Tree.toggle "b"
            collapsed = open |> Tree.select "a/x/leaf" Nothing |> Tree.toggle "a"
            reopened = collapsed |> Tree.toggle "a"
        in Expect.all
            [ \_ -> Tree.visiblePaths open |> Expect.equal [ "a", "a/x", "a/x/leaf", "b", "b/y" ]
            , \_ -> Tree.visiblePaths collapsed |> Expect.equal [ "a", "b", "b/y" ]
            , \_ -> collapsed.focused |> Expect.equal (Just "a")
            , \_ -> Tree.visiblePaths reopened |> Expect.equal (Tree.visiblePaths open)
            , \_ -> Set.member "b" reopened.expanded |> Expect.equal True
            , \_ -> Set.member "a/x" collapsed.expanded |> Expect.equal True ] ()
    , test "filter and live deletion recover hidden focus to a surviving ancestor or nearby visible row" <| \_ ->
        let
            open = populated |> Tree.toggle "a" |> Tree.toggle "a/x" |> Tree.toggle "b" |> Tree.select "a/x/leaf" Nothing
            deleted = accept "a/x" [] open
            filtered = accept "" [ node "b" True ] open
        in Expect.all
            [ \_ -> deleted.focused |> Expect.equal (Just "a/x")
            , \_ -> filtered.focused |> Expect.equal (Just "b/y")
            , \_ -> Tree.visiblePaths filtered |> Expect.equal [ "b", "b/y" ] ] ()
    , test "deep link reveals the full literal ancestor chain and terminal without closing another branch" <| \_ ->
        let
            terminal = { subjectKind = Api.SubjectGlob, subject = "src/**/auth/main.elm" }
            revealed = populated |> Tree.toggle "b" |> Tree.reveal terminal "observation"
        in Expect.all
            [ \_ -> Tree.ancestors terminal.subject |> Expect.equal [ "src", "src/**", "src/**/auth", "src/**/auth/main.elm" ]
            , \_ -> revealed.revealTarget |> Expect.equal (Just { subject = terminal, observationId = Just "observation", focus = True })
            , \_ -> revealed.selected |> Expect.equal (Just terminal)
            , \_ -> Set.member "b" revealed.expanded |> Expect.equal True
            , \_ -> List.all (\path -> Set.member path revealed.expanded) (Tree.ancestors terminal.subject) |> Expect.equal True ] ()
    , test "path focus and typed file/glob selection are independent of branch expansion and card intent" <| \_ ->
        let
            open = populated |> Tree.toggle "a"
            file = Tree.select "a" (Just { subjectKind = Api.SubjectFile, subject = "a" }) open
            glob = Tree.select "a" (Just { subjectKind = Api.SubjectGlob, subject = "a" }) file
        in Expect.equal { changed = True, expanded = True, reveal = Nothing, focus = Just "a" }
            { changed = file.selected /= glob.selected, expanded = file.expanded == glob.expanded, reveal = glob.revealTarget, focus = glob.focused }
    , test "retired callbacks release only their physical slots and cannot repopulate a newer query" <| \_ ->
        let
            start prefix state = Tree.begin prefix 0 state |> Tuple.first
            full = List.foldl start populated [ "one", "two", "three", "four" ]
            retired = Tree.retire "replacement" full
            ( blocked, admission ) = Tree.begin "" 0 retired
            old = List.head full.physical
            settled = old |> Maybe.map (\request -> Tree.receive request (Ok { items = [], hasMore = False, totalCount = 0, matchCount = 0 }) blocked) |> Maybe.withDefault blocked
        in Expect.all
            [ \_ -> admission |> Expect.equal Nothing
            , \_ -> List.length settled.physical |> Expect.equal 3
            , \_ -> settled.nodes |> Expect.equal populated.nodes
            , \_ -> Tree.begin "" 0 settled |> Tuple.second |> Maybe.map .lifetime |> Expect.equal (Just "replacement") ] ()
    , test "a filter replacement retains completed rows on failure and never reuses descendants from the old query" <| \_ ->
        let
            open = populated |> Tree.toggle "a" |> Tree.toggle "a/x"
            retired = Tree.retire "new-filter" open
            ( started, request ) = Tree.begin "" 0 retired
            failed = request |> Maybe.map (\guard -> Tree.receive guard (Err "unavailable") started) |> Maybe.withDefault started
            replaced = accept "" [ node "a" True ] failed
        in Expect.all
            [ \_ -> Tree.visiblePaths failed |> Expect.equal (Tree.visiblePaths open)
            , \_ -> Tree.visiblePaths replaced |> Expect.equal [ "a" ]
            , \_ -> Set.member "a/x" replaced.expanded |> Expect.equal True
            , \_ -> replaced.totals |> Expect.equal (Just (10, 10)) ] ()
    , test "out-of-branch nodes and nonprogress continuation fail without replacing completed rows" <| \_ ->
        let
            ( started, request ) = Tree.begin "a" 0 populated
            invalid = request |> Maybe.map (\guard -> Tree.receive guard (Ok { items = [ node "other" False ], hasMore = False, totalCount = 1, matchCount = 1 }) started) |> Maybe.withDefault started
        in Expect.all
            [ \_ -> invalid.nodes |> Expect.equal populated.nodes
            , \_ -> Dict.get "a" invalid.branches |> Maybe.andThen .error |> (/=) Nothing |> Expect.equal True ] ()
    ]
