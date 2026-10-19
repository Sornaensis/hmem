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


suite : Test
suite = describe "Subject Tree explorer state"
    [ test "several branches remain open and reopening a parent preserves descendant expansion" <| \_ ->
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
            , \_ -> revealed.revealTarget |> Expect.equal (Just { subject = terminal, observationId = "observation" })
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
