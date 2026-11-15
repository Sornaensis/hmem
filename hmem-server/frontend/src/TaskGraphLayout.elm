module TaskGraphLayout exposing (Box, Circle, Layout, Node, Point, RoutedEdge, build, buildWithLabels, contains, ranks)

{-| Pack the retained project forest bottom-up, then translate local geometry
once in preorder. Child disks use their radius, never their bounding squares.
World geometry is independent of the canvas camera.
-}

import Dict exposing (Dict)
import Feature.TaskGraph as Graph
import Set


type alias Box = { x : Float, y : Float, width : Float, height : Float }
type alias Point = { x : Float, y : Float }
type alias Node = { task : Graph.GraphTask, box : Box, familyId : String }
type alias Circle = { project : Graph.GraphProject, x : Float, y : Float, radius : Float, label : Box }
type alias RoutedEdge = { edge : Graph.Edge, points : List Point }
type alias Layout = { nodes : Dict String Node, frames : Dict String Box, circles : List Circle, edges : List RoutedEdge, bounds : Box, initialAnchor : Maybe Box, problem : Maybe String }
type alias Own = { nodes : List Node, frames : List (String,Box), label : Maybe Box, bottom : Float }
type alias Pack = { own : Own, children : List (String,Point), radius : Float, centerY : Float }


contains : Circle -> Box -> Bool
contains circle box =
    List.all (\( x, y ) -> (x - circle.x) ^ 2 + (y - circle.y) ^ 2 <= circle.radius ^ 2)
        [ (box.x,box.y), (box.x+box.width,box.y), (box.x,box.y+box.height), (box.x+box.width,box.y+box.height) ]


ranks : Graph.Graph -> Dict String Int
ranks graph =
    let
        advance remaining values =
            if remaining <= 0 then values
            else
                let next = List.foldl (\edge acc -> Dict.update edge.dependentId (\old -> Just (max (Maybe.withDefault 0 old) (1 + Maybe.withDefault 0 (Dict.get edge.prerequisiteId values)))) acc) values graph.edges in
                if next == values then values else advance (remaining-1) next
    in advance (Dict.size graph.tasks) (Dict.map (\_ _ -> 0) graph.tasks)


corners : Box -> List Point
corners box = [{x=box.x,y=box.y},{x=box.x+box.width,y=box.y},{x=box.x,y=box.y+box.height},{x=box.x+box.width,y=box.y+box.height}]


shift : Float -> Float -> Box -> Box
shift x y box = {box|x=box.x+x,y=box.y+y}


pack : Own -> List (String,Pack) -> Pack
pack own children =
    let
        add (id,child) (cursor,accumulated) = (cursor+2*child.radius+64,(id,{x=0,y=cursor+child.radius})::accumulated)
        start = own.bottom+48
        (bottom,positions) = List.foldl add (start,[]) children
        extent = if List.isEmpty children then max 20 own.bottom else bottom-64
        center = extent/2
        ownCorners = List.concatMap (.box >> corners) own.nodes ++ List.concatMap (Tuple.second >> corners) own.frames ++ (own.label |> Maybe.map corners |> Maybe.withDefault [])
        ownRadius = List.map (\p -> sqrt (p.x^2+(p.y-center)^2)) ownCorners |> List.maximum |> Maybe.withDefault 0
        disks = List.map2 (\(_,child) (_,point) -> abs (point.y-center)+child.radius) children (List.reverse positions)
        radius = max ownRadius (List.maximum disks |> Maybe.withDefault 0)+40
    in {own=own,children=List.reverse positions,radius=radius,centerY=center}


build : Dict String Float -> Graph.Graph -> Layout
build measurements = buildWithLabels measurements Dict.empty


buildWithLabels : Dict String Float -> Dict String Float -> Graph.Graph -> Layout
buildWithLabels measurements labelMeasurements graph =
    let
        rank = ranks graph
        edgeCount = List.length graph.edges
        ordering task = ( Maybe.withDefault 0 (Dict.get task.id rank), task.id )
        family task = task.parentId |> Maybe.andThen (\id -> Dict.get id graph.tasks |> Maybe.andThen (\parent -> if parent.projectId == task.projectId then Just id else Nothing)) |> Maybe.withDefault task.id
        height task = max 70 (Dict.get task.id measurements |> Maybe.withDefault (46 + 20 * toFloat (max 1 (ceiling (toFloat (String.length task.title) / 39)))))
        families = Dict.values graph.tasks |> List.foldl (\task -> Dict.update (family task) (\old -> Just (task::Maybe.withDefault [] old))) Dict.empty
        rootsByProject = Dict.values graph.tasks |> List.filter (\task -> family task==task.id)
            |> List.foldl (\task -> Dict.update (Maybe.withDefault "" task.projectId) (\old -> Just (task::Maybe.withDefault [] old))) (Dict.map (\_ _ -> []) graph.projects)
        makeOwn id tasks =
            let
                labelHeight = Dict.get id graph.projects |> Maybe.map (\project -> max 20 (Dict.get id labelMeasurements |> Maybe.withDefault (20*toFloat (max 1 (ceiling (toFloat (String.length project.name)/24)))))) |> Maybe.withDefault 0
                placeFamily task (y,familyNodes,familyFrames) =
                    let
                        children = Dict.get task.id families |> Maybe.withDefault [] |> List.filter (\child -> child.id/=task.id) |> List.sortBy ordering
                        parent = {task=task,box={x=-190,y=y,width=380,height=height task},familyId=task.id}
                        addChild child (childY,childNodes) =
                            let node={task=child,box={x=-158,y=childY,width=316,height=height child},familyId=task.id} in (childY+node.box.height+24,node::childNodes)
                        (childBottom,withChildren) = List.foldl addChild (y+parent.box.height+28,parent::familyNodes) children
                        frameHeight = if List.isEmpty children then parent.box.height else childBottom-y
                    in (y+frameHeight+64,withChildren,if List.isEmpty children then familyFrames else (task.id,{x=-190,y=y,width=380,height=frameHeight})::familyFrames)
                (ownBottom,ownNodes,ownFrames) = List.foldl placeFamily (if labelHeight>0 then labelHeight+32 else 0,[],[]) (List.sortBy ordering tasks)
            in {nodes=ownNodes,frames=ownFrames,label=if labelHeight>0 then Just {x=-190,y=0,width=380,height=labelHeight} else Nothing,bottom=if List.isEmpty tasks then labelHeight else ownBottom-64}
        own = Dict.map makeOwn rootsByProject
        parents = Dict.values graph.projects |> List.filterMap (\project -> project.parentId |> Maybe.andThen (\id -> if Dict.member id graph.projects then Just (project.id,id) else Nothing)) |> Dict.fromList
        projectChildren = Dict.foldl (\id parent -> Dict.update parent (\old -> Just (id::Maybe.withDefault [] old))) Dict.empty parents |> Dict.map (\_ ids -> List.sort ids)
        counts = Dict.map (\id _ -> Dict.get id projectChildren |> Maybe.withDefault [] |> List.length) own
        leaves = Dict.keys (Dict.filter (\_ count -> count==0) counts)
        measure pending remaining packed =
            case pending of
                [] -> packed
                id::rest ->
                    let
                        childPacks = Dict.get id projectChildren |> Maybe.withDefault [] |> List.filterMap (\child -> Dict.get child packed |> Maybe.map (Tuple.pair child))
                        value = Dict.get id own |> Maybe.withDefault {nodes=[],frames=[],label=Nothing,bottom=0}
                        next = Dict.insert id (pack value childPacks) packed
                    in case Dict.get id parents of
                        Nothing -> measure rest remaining next
                        Just parent ->
                            let count=Maybe.withDefault 0 (Dict.get parent remaining)-1 in
                            measure (if count==0 then parent::rest else rest) (Dict.insert parent count remaining) next
        measured = measure leaves counts Dict.empty
        cyclic = Dict.size measured /= Dict.size own
        packs = if cyclic then Dict.map (\_ value -> pack value []) own else measured
        displayedParents = if cyclic then Dict.empty else parents
        rootIds = Dict.keys own |> List.filter (\id -> not (Dict.member id displayedParents))
        laneWidth = 40+toFloat edgeCount*7
        rootPosition id (cursor,positions) =
            let r=Dict.get id packs |> Maybe.map .radius |> Maybe.withDefault 40 in
            (cursor+2*r+2*laneWidth+120,{id=id,x=cursor+r,y=r+80,root=id}::positions)
        initialRoots = List.foldl rootPosition (laneWidth+80,[]) rootIds |> Tuple.second |> List.reverse
        place pending result =
            case pending of
                [] -> result
                position::rest ->
                    case Dict.get position.id packs of
                        Nothing -> place rest result
                        Just value ->
                            let
                                dy=position.y-value.centerY
                                nodes=List.foldl (\node -> Dict.insert node.task.id {node|box=shift position.x dy node.box}) result.nodes value.own.nodes
                                frames=List.foldl (\(taskId,box) -> Dict.insert taskId (shift position.x dy box)) result.frames value.own.frames
                                circle=Maybe.map2 (\project label -> {project=project,x=position.x,y=position.y,radius=value.radius,label=shift position.x dy label}) (Dict.get position.id graph.projects) value.own.label
                                rootBox=if position.id==position.root then Dict.insert position.root {x=position.x-value.radius,y=position.y-value.radius,width=2*value.radius,height=2*value.radius} result.roots else result.roots
                                more=List.map (\(child,point) -> {id=child,x=position.x+point.x,y=dy+point.y,root=position.root}) value.children
                            in place (more++rest) {nodes=nodes,frames=frames,circles=Maybe.map (\c -> c::result.circles) circle |> Maybe.withDefault result.circles,taskRoots=List.foldl (\node -> Dict.insert node.task.id position.root) result.taskRoots value.own.nodes,roots=rootBox}
        placed = place initialRoots {nodes=Dict.empty,frames=Dict.empty,circles=[],taskRoots=Dict.empty,roots=Dict.empty}
        circles = List.reverse placed.circles
        route index edge =
            Maybe.map2 (\source target ->
                let
                    a=source.box
                    b=target.box
                    sourceRoot=Dict.get source.task.id placed.taskRoots |> Maybe.withDefault ""
                    targetRoot=Dict.get target.task.id placed.taskRoots |> Maybe.withDefault ""
                    sourceRegion=Dict.get sourceRoot placed.roots |> Maybe.withDefault a
                    targetRegion=Dict.get targetRoot placed.roots |> Maybe.withDefault b
                    sameRoot=sourceRoot==targetRoot
                    gutter=24+toFloat index*7
                    sx=sourceRegion.x+sourceRegion.width+gutter
                    tx=if sameRoot then targetRegion.x+targetRegion.width+gutter else targetRegion.x-gutter
                    start={x=a.x+a.width,y=a.y+a.height/2}
                    finish={x=if sameRoot then b.x+b.width else b.x,y=b.y+b.height/2}
                    lane=40-toFloat index*22
                    middle=if sameRoot then [{x=sx,y=start.y},{x=sx,y=finish.y}] else [{x=sx,y=start.y},{x=sx,y=lane},{x=tx,y=lane},{x=tx,y=finish.y}]
                    frame=Dict.get source.familyId placed.frames |> Maybe.withDefault a
                    innerX=frame.x+frame.width-10-12*(toFloat index/toFloat (edgeCount+1))
                    parentPort node={x=innerX,y=node.box.y+node.box.height}
                    innerStart=if source.task.id==source.familyId then parentPort source else start
                    innerFinish=if target.task.id==target.familyId then parentPort target else {x=b.x+b.width,y=finish.y}
                    innerStartY=if source.task.id==source.familyId then innerStart.y+14 else innerStart.y
                    innerFinishY=if target.task.id==target.familyId then innerFinish.y+14 else innerFinish.y
                    points=if source.familyId==target.familyId then [innerStart,{x=innerX,y=innerStartY},{x=innerX,y=innerFinishY},innerFinish] else start::middle++[finish]
                in {edge=edge,points=points}) (Dict.get edge.prerequisiteId placed.nodes) (Dict.get edge.dependentId placed.nodes)
        edges=graph.edges |> List.indexedMap route |> List.filterMap identity
        extents=List.concatMap .points edges ++ (Dict.values placed.nodes |> List.concatMap (.box >> corners)) ++ (Dict.values placed.frames |> List.concatMap corners)
            ++ List.concatMap (\circle -> [{x=circle.x-circle.radius,y=circle.y-circle.radius},{x=circle.x+circle.radius,y=circle.y+circle.radius}]) circles
        left=min 0 (List.map .x extents |> List.minimum |> Maybe.withDefault 0)-40
        top=min 0 (List.map .y extents |> List.minimum |> Maybe.withDefault 0)-40
        right=max 500 (List.map .x extents |> List.maximum |> Maybe.withDefault 500)+40
        bottom=max 400 (List.map .y extents |> List.maximum |> Maybe.withDefault 400)+40
        projectOrder=List.indexedMap (\index circle -> (circle.project.id,index)) circles |> Dict.fromList
        anchor=Dict.values placed.nodes |> List.filter (\node -> Set.member node.task.id graph.inScopeTaskIds)
            |> List.sortBy (\node -> (if node.task.projectId==Just graph.selectedProjectId then -1 else node.task.projectId |> Maybe.andThen (\id -> Dict.get id projectOrder) |> Maybe.withDefault 2147483647,Maybe.withDefault 0 (Dict.get node.task.id rank),node.task.id)) |> List.head |> Maybe.map .box
    in {nodes=placed.nodes,frames=placed.frames,circles=circles,edges=edges,bounds={x=left,y=top,width=right-left,height=bottom-top},initialAnchor=anchor,problem=if cyclic then Just "Project ancestry contains a cycle; showing separate circles instead of nesting." else Nothing}
