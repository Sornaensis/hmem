module TaskGraphFiltersTest exposing (suite)

import Api
import Dict
import Expect
import Feature.TaskGraph as Graph
import Set
import TaskGraphLayout as Layout
import TaskGraphNavigation as History
import Test exposing (describe, test)
import Url


suite = describe "graph presentation filters"
    [ test "all four independent combinations preserve nonterminal statuses and canonical input" <| \_ ->
        let
            ids done cancelled = Graph.visibleGraph {showDone=done,showCancelled=cancelled} raw |> .tasks |> Dict.keys
        in Expect.equal
            ([ ["blocked","cancelled","child","done","progress","shared","todo"],["blocked","cancelled","child","progress","shared","todo"],["blocked","child","done","progress","shared","todo"],["blocked","child","progress","shared","todo"] ],7)
            ([ids True True,ids False True,ids True False,ids False False],Dict.size raw.tasks)
    , test "hidden parent never hides a visible child or invents parent/shortcut edges" <| \_ ->
        let
            graph = Graph.visibleGraph {showDone=False,showCancelled=False} raw
            layout = Layout.build Dict.empty graph
        in Expect.equal (Just (Just "done"),(False,False),[])
            (Dict.get "child" graph.tasks |> Maybe.map .parentId,(Dict.member "done" layout.nodes,Dict.member "done" layout.frames),List.filter (\link -> link.prerequisiteId == "done" || link.dependentId == "done") graph.edges)
    , test "only direct edges from visible anchors retain shared external neighbors" <| \_ ->
        let
            task id status project = {id=id,title=id,status=status,projectId=Just project,parentId=Nothing,external=project=="external"}
            source = {raw|tasks=Dict.fromList [("done",task "done" Api.Done "p"),("todo",task "todo" Api.Todo "p"),("shared",task "shared" Api.Todo "external"),("orphan",task "orphan" Api.Todo "external"),("transitive",task "transitive" Api.Todo "external")]
                ,inScopeTaskIds=Set.fromList ["done","todo"],externalTaskIds=Set.fromList ["shared","orphan","transitive"]
                ,edges=[edge "shared" "done",edge "shared" "todo",edge "orphan" "done",edge "shared" "transitive"]}
            graph=Graph.visibleGraph {showDone=False,showCancelled=True} source
        in Expect.equal (["shared","todo"],[edge "shared" "todo"],["shared"])
            (Dict.keys graph.tasks,graph.edges,Set.toList graph.externalTaskIds)
    , test "empty external circles disappear while empty in-scope circles remain" <| \_ ->
        let graph=Graph.visibleGraph {showDone=False,showCancelled=False} {raw|tasks=Dict.filter (\id _ -> id=="done" || id=="shared") raw.tasks,inScopeTaskIds=Set.singleton "done",edges=[edge "shared" "done"]}
        in Expect.equal ([],["p"],[]) (Dict.keys graph.tasks,Dict.keys graph.projects,graph.edges)
    , test "URL flags default off and preserve every explicit independent choice and graph identity" <| \_ ->
        let
            url=Url.fromString "https://app.test/workspace/w?keep=1#tab=projects&task-graph=p&graph-entry=e&other=hello%20world" |> Maybe.withDefault fallback
            changed=History.withFilters {showDone=True,showCancelled=False} url |> Url.fromString |> Maybe.withDefault fallback
            restored=History.withFilters Graph.defaultFilters changed |> Url.fromString |> Maybe.withDefault fallback
            choices=[{showDone=False,showCancelled=False},{showDone=True,showCancelled=False},{showDone=False,showCancelled=True},{showDone=True,showCancelled=True}]
            roundtrip flags=History.withFilters flags url |> Url.fromString |> Maybe.map History.filters
        in Expect.all [\_ -> Expect.equal {showDone=False,showCancelled=False} Graph.defaultFilters,\_ -> Expect.equal Graph.defaultFilters (History.filters url),\_ -> Expect.equal {showDone=True,showCancelled=False} (History.filters changed)
            ,\_ -> Expect.equal (History.parse url) (History.parse changed),\_ -> Expect.equal (Just "keep=1") changed.query
            ,\_ -> Expect.equal Graph.defaultFilters (History.filters restored),\_ -> Expect.equal True (restored.fragment |> Maybe.map (String.contains "other=hello%20world") |> Maybe.withDefault False)
            ,\_ -> Expect.equal (List.map Just choices) (List.map roundtrip choices)] ()
    ]


edge a b = {prerequisiteId=a,dependentId=b}


fallback = {protocol=Url.Https,host="app.test",port_=Nothing,path="/workspace/w",query=Nothing,fragment=Nothing}


raw : Graph.Graph
raw =
    let
        task id status parent project = (id,{id=id,title=id,status=status,projectId=Just project,parentId=parent,external=project=="external"})
    in {selectedProjectId="p",projects=Dict.fromList [("p",{id="p",name="Project",parentId=Nothing,external=False}),("external",{id="external",name="External",parentId=Nothing,external=True})]
        ,tasks=Dict.fromList [task "done" Api.Done Nothing "p",task "cancelled" Api.Cancelled Nothing "p",task "child" Api.Todo (Just "done") "p",task "todo" Api.Todo Nothing "p",task "progress" Api.InProgress Nothing "p",task "blocked" Api.Blocked Nothing "p",task "shared" Api.Todo Nothing "external"]
        ,edges=[edge "shared" "done",edge "shared" "todo"],inScopeTaskIds=Set.fromList ["done","cancelled","child","todo","progress","blocked"],externalTaskIds=Set.singleton "shared"}
