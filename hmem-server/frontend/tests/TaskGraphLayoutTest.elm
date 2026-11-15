module TaskGraphLayoutTest exposing (suite)

import Api
import Dict
import Expect
import Feature.TaskGraph as Graph
import Set
import TaskGraphLayout as Layout
import Test exposing (describe,test)


suite = describe "dynamic task graph geometry"
    [ test "unrelated owning circles enclose every family corner and remain disjoint" <| \_ ->
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
    , test "mixed retained forest nests exact direct links including external ancestors, with disjoint unrelated disks" <| \_ ->
        let
            layout=Layout.buildWithLabels (Dict.singleton "long" 430) (Dict.singleton "child-a" 260) nested
            byId=List.map (\circle -> (circle.project.id,circle)) layout.circles |> Dict.fromList
            enclosed child=child.project.parentId |> Maybe.andThen (\id -> Dict.get id byId) |> Maybe.map (\parent -> diskInside parent child) |> Maybe.withDefault True
            ancestor a b=if a==b then True else Dict.get b nested.projects |> Maybe.andThen .parentId |> Maybe.map (\parent -> if Dict.member parent nested.projects then ancestor a parent else False) |> Maybe.withDefault False
            disjoint a b=ancestor a.project.id b.project.id || ancestor b.project.id a.project.id || distance a b>a.radius+b.radius
            ownInside circle=Layout.contains circle circle.label && (Dict.values layout.nodes |> List.filter (\node -> node.task.projectId==Just circle.project.id) |> List.all (.box >> Layout.contains circle))
            paintOrder=List.indexedMap (\index circle -> (circle.project.id,index)) layout.circles |> Dict.fromList
            ancestorFirst child=Maybe.map2 (\parentIndex childIndex -> parentIndex<childIndex) (child.project.parentId |> Maybe.andThen (\id -> Dict.get id paintOrder)) (Dict.get child.project.id paintOrder) |> Maybe.withDefault True
        in Expect.equal (True,True,True) (List.all enclosed layout.circles && List.all ownInside layout.circles,List.all (\a -> List.all (disjoint a) layout.circles) layout.circles,List.all ancestorFirst layout.circles)
    , test "own labels and complete families reserve separate bands above direct child disks" <| \_ ->
        let
            layout=Layout.buildWithLabels Dict.empty (Dict.singleton "selected" 600) nested
            clear parent child=child.project.parentId/=Just parent.project.id ||
                (parent.label.y+parent.label.height<child.y-child.radius &&
                    (Dict.values layout.nodes |> List.filter (\node -> node.task.projectId==Just parent.project.id) |> List.all (\node -> node.box.y+node.box.height<child.y-child.radius)) &&
                    (Dict.toList layout.frames |> List.filter (\(id,_) -> Dict.get id layout.nodes |> Maybe.map (.task >> .projectId >> (==) (Just parent.project.id)) |> Maybe.withDefault False) |> List.all (\(_,box) -> box.y+box.height<child.y-child.radius)))
        in Expect.equal True (List.all (\parent -> List.all (clear parent) layout.circles) layout.circles)
    , test "same-root and cross-root dependency segments avoid unrelated card and label interiors" <| \_ ->
        let
            layout=Layout.buildWithLabels Dict.empty (Dict.singleton "child-a" 260) nested
            valid routed =
                let obstacles=List.map .label layout.circles ++ (Dict.values layout.nodes |> List.filter (\node -> node.task.id/=routed.edge.prerequisiteId && node.task.id/=routed.edge.dependentId) |> List.map .box)
                    segments=List.map2 Tuple.pair routed.points (List.drop 1 routed.points)
                in List.all (\(a,b) -> List.all (\box -> not (intersects box a b)) obstacles) segments
            bounds=layout.bounds
        in Expect.equal (True,True) (List.all valid layout.edges,List.concatMap .points layout.edges |> List.all (\p -> p.x>bounds.x && p.y>bounds.y && p.x<bounds.x+bounds.width && p.y<bounds.y+bounds.height))
    , test "depth1,8,32 single-child chains grow additively and keep the in-scope leaf anchor" <| \_ ->
        let
            layout depth=Layout.build Dict.empty (chain depth)
            radius depth=layout depth |> .circles |> List.head |> Maybe.map .radius |> Maybe.withDefault 0
            deep=layout 32
        in Expect.all [\_ -> Expect.lessThan (radius 1+8*160) (radius 8),\_ -> Expect.lessThan (radius 1+32*160) (radius 32)
            ,\_ -> Expect.equal 32 (List.length deep.circles),\_ -> Expect.equal (Dict.get "leaf" deep.nodes |> Maybe.map .box) deep.initialAnchor] ()
    , test "missing intermediates cut the forest and unassigned external tasks stay loose" <| \_ ->
        let
            project value={value|parentId=Just "missing"}
            task={id="unassigned",title="Unassigned",status=Api.Todo,projectId=Nothing,parentId=Nothing,external=True}
            source={nested|projects=Dict.update "child-a" (Maybe.map project) nested.projects,tasks=Dict.insert task.id task nested.tasks}
            layout=Layout.build Dict.empty source
            a=List.filter (\c -> c.project.id=="child-a") layout.circles |> List.head
            selected=List.filter (\c -> c.project.id=="selected") layout.circles |> List.head
        in Expect.equal (True,True,True) (Maybe.map2 (\x y -> distance x y>x.radius+y.radius) a selected |> Maybe.withDefault False,Dict.member task.id layout.nodes,not (List.any (\c -> c.project.id=="missing" || c.project.id=="") layout.circles))
    , test "malformed retained ancestry cycles settle with a truthful finite fallback" <| \_ ->
        let
            source={graph|projects=Dict.map (\id value -> {value|parentId=Just (if id=="selected" then "external" else "selected")}) graph.projects}
            layout=Layout.build Dict.empty source
        in Expect.equal (True,2,4) (layout.problem/=Nothing,List.length layout.circles,Dict.size layout.nodes)
    ]


distance a b=sqrt ((a.x-b.x)^2+(a.y-b.y)^2)
diskInside parent child=distance parent child+child.radius<=parent.radius+0.000001


intersects box a b =
    if a.x==b.x then a.x>box.x+0.000001 && a.x<box.x+box.width-0.000001 && max (min a.y b.y) box.y<min (max a.y b.y) (box.y+box.height)-0.000001
    else if a.y==b.y then a.y>box.y+0.000001 && a.y<box.y+box.height-0.000001 && max (min a.x b.x) box.x<min (max a.x b.x) (box.x+box.width)-0.000001
    else True


chain depth =
    let
        project index=(String.fromInt index,{id=String.fromInt index,name="Level",parentId=if index==0 then Nothing else Just (String.fromInt (index-1)),external=False})
        leaf={id="leaf",title="Leaf",status=Api.Todo,projectId=Just (String.fromInt (depth-1)),parentId=Nothing,external=False}
    in {selectedProjectId="0",projects=List.range 0 (depth-1) |> List.map project |> Dict.fromList,tasks=Dict.singleton "leaf" leaf,edges=[],inScopeTaskIds=Set.singleton "leaf",externalTaskIds=Set.empty}


nested : Graph.Graph
nested =
    let
        project id parent external=(id,{id=id,name=id,parentId=parent,external=external})
        task id projectId status=(id,{id=id,title=id,status=status,projectId=Just projectId,parentId=Nothing,external=False})
        edge a b={prerequisiteId=a,dependentId=b}
    in {graph|projects=Dict.union (Dict.fromList [project "outer" Nothing True,project "ancestor" (Just "outer") True,project "child-a" (Just "selected") False,project "child-b" (Just "selected") False,project "grandchild" (Just "child-a") False,project "empty" (Just "child-b") False]) (Dict.update "selected" (Maybe.map (\value -> {value|parentId=Just "ancestor"})) graph.projects)
        ,tasks=Dict.union (Dict.fromList [task "a" "child-a" Api.Todo,task "b" "child-b" Api.Done,task "grand" "grandchild" Api.Todo]) graph.tasks
        ,edges=graph.edges++[edge "grand" "parent",edge "parent" "b",edge "long" "grand"]
        ,inScopeTaskIds=Set.fromList ["parent","one","two","a","b","grand"]}


graph : Graph.Graph
graph =
    let task id parent project = {id=id,title=if id=="long" then String.repeat 120 "Long title " else id,status=Api.Todo,projectId=Just project,parentId=parent,external=project=="external"}
        tasks=[task "parent" Nothing "selected",task "one" (Just "parent") "selected",task "two" (Just "parent") "selected",task "long" Nothing "external"]
    in {selectedProjectId="selected",projects=Dict.fromList [("selected",{id="selected",name="Selected",parentId=Nothing,external=False}),("external",{id="external",name="External",parentId=Nothing,external=True})]
        ,tasks=tasks |> List.map (\value -> (value.id,value)) |> Dict.fromList,edges=[{prerequisiteId="one",dependentId="two"},{prerequisiteId="long",dependentId="parent"}],inScopeTaskIds=Set.fromList ["parent","one","two"],externalTaskIds=Set.singleton "long"}
