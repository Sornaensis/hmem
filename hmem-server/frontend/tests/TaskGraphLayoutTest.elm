module TaskGraphLayoutTest exposing (suite)

import Api
import Dict
import Expect
import Feature.TaskGraph as Graph
import Set
import TaskGraphLayout as Layout
import Test exposing (describe,test)


suite = describe "dynamic task graph geometry"
    [ test "owning circles enclose every family corner and never overlap" <| \_ ->
        let layout=Layout.build (Dict.singleton "long" 430) graph
            enclosed circle = Dict.values layout.nodes |> List.filter (\node -> node.task.projectId==Just circle.project.id) |> List.all (.box >> Layout.contains circle)
            disjoint = List.all (\a -> List.all (\b -> a.project.id==b.project.id || sqrt ((a.x-b.x)^2+(a.y-b.y)^2)>a.radius+b.radius) layout.circles) layout.circles
        in Expect.equal (True,True,4) (List.all enclosed layout.circles,disjoint,Dict.size layout.nodes)
    , test "subtask cards and their own incident edges remain inside the decorative parent" <| \_ ->
        let layout=Layout.build Dict.empty graph
            frame=Dict.get "parent" layout.frames
            internal=List.filter (\route -> route.edge.prerequisiteId=="one") layout.edges |> List.head
            contains box point = point.x>=box.x && point.x<=box.x+box.width && point.y>=box.y && point.y<=box.y+box.height
            inside=Maybe.map2 (\box edge -> List.all (contains box) edge.points) frame internal
        in Expect.equal (Just True) inside
    , test "measurement updates expand layout and retain stable identities/directional endpoints" <| \_ ->
        let before=Layout.build Dict.empty graph
            after=Layout.build (Dict.singleton "long" 1200) graph
            directional route = Maybe.map2 (\a b ->
                let onBox box p = (p.x==box.x || p.x==box.x+box.width) && p.y>=box.y && p.y<=box.y+box.height || (p.y==box.y || p.y==box.y+box.height) && p.x>=box.x && p.x<=box.x+box.width
                in Maybe.map2 (\start finish -> onBox a.box start && onBox b.box finish) (List.head route.points) (List.reverse route.points |> List.head) |> Maybe.withDefault False)
                (Dict.get route.edge.prerequisiteId after.nodes) (Dict.get route.edge.dependentId after.nodes) |> Maybe.withDefault False
        in Expect.all [\_ -> Expect.equal (Dict.keys before.nodes) (Dict.keys after.nodes),\_ -> Expect.greaterThan before.bounds.height after.bounds.height,\_ -> Expect.equal True (List.all directional after.edges)] ()
    , test "dense incoming edge lanes are included in Fit bounds" <| \_ ->
        let external id = {id=id,title=id,status=Api.Done,projectId=Just "external",parentId=Nothing,external=True}
            added=List.range 1 100 |> List.map (String.fromInt >> (++) "incoming-")
            dense={graph|tasks=List.foldl (\id -> Dict.insert id (external id)) graph.tasks added,edges=List.map (\id -> {prerequisiteId=id,dependentId="parent"}) added}
            layout=Layout.build Dict.empty dense
            b=layout.bounds
        in Expect.equal True (List.concatMap .points layout.edges |> List.all (\p -> p.x>b.x && p.y>b.y && p.x<b.x+b.width && p.y<b.y+b.height))
    , test "measured project labels enlarge their own circle and remain separated from tasks" <| \_ ->
        let layout=Layout.buildWithLabels Dict.empty (Dict.singleton "selected" 900) graph
            circle=List.filter (\value -> value.project.id=="selected") layout.circles |> List.head
            valid value=Layout.contains value value.label && value.label.height==900 && (Dict.values layout.nodes |> List.filter (\node -> node.task.projectId==Just "selected") |> List.all (\node -> node.box.y>=value.label.y+value.label.height+32 && Layout.contains value node.box))
        in Expect.equal (Just True) (Maybe.map valid circle)
    ]


graph : Graph.Graph
graph =
    let task id parent project = {id=id,title=if id=="long" then String.repeat 120 "Long title " else id,status=Api.Todo,projectId=Just project,parentId=parent,external=project=="external"}
        tasks=[task "parent" Nothing "selected",task "one" (Just "parent") "selected",task "two" (Just "parent") "selected",task "long" Nothing "external"]
    in {selectedProjectId="selected",projects=Dict.fromList [("selected",{id="selected",name="Selected",parentId=Nothing,external=False}),("external",{id="external",name="External",parentId=Nothing,external=True})]
        ,tasks=tasks |> List.map (\value -> (value.id,value)) |> Dict.fromList,edges=[{prerequisiteId="one",dependentId="two"},{prerequisiteId="long",dependentId="parent"}],inScopeTaskIds=Set.fromList ["parent","one","two"],externalTaskIds=Set.singleton "long"}
