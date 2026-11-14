module TaskGraphLayout exposing (Box, Circle, Layout, Node, Point, RoutedEdge, build, buildWithLabels, contains, ranks)

{-| World geometry is independent of pan/zoom. Only current unscaled content
measurements affect task height. Project ownership, not hierarchy nesting,
determines the disjoint circular groups.
-}

import Dict exposing (Dict)
import Feature.TaskGraph as Graph


type alias Box = { x : Float, y : Float, width : Float, height : Float }
type alias Point = { x : Float, y : Float }
type alias Node = { task : Graph.GraphTask, box : Box, familyId : String }
type alias Circle = { project : Graph.GraphProject, x : Float, y : Float, radius : Float, label : Box }
type alias RoutedEdge = { edge : Graph.Edge, points : List Point }
type alias Layout = { nodes : Dict String Node, frames : Dict String Box, circles : List Circle, edges : List RoutedEdge, bounds : Box }


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


build : Dict String Float -> Graph.Graph -> Layout
build measurements = buildWithLabels measurements Dict.empty


buildWithLabels : Dict String Float -> Dict String Float -> Graph.Graph -> Layout
buildWithLabels measurements labelMeasurements graph =
    let
        rank = ranks graph
        ordering task = ( Maybe.withDefault 0 (Dict.get task.id rank), task.id )
        family task = task.parentId |> Maybe.andThen (\id -> Dict.get id graph.tasks |> Maybe.andThen (\parent -> if parent.projectId == task.projectId then Just id else Nothing)) |> Maybe.withDefault task.id
        height task = max 70 (Dict.get task.id measurements |> Maybe.withDefault (46 + 20 * toFloat (max 1 (ceiling (toFloat (String.length task.title) / 39)))))
        groups = Dict.values graph.tasks |> List.foldl (\task -> Dict.update (Maybe.withDefault "" task.projectId) (\old -> Just (task :: Maybe.withDefault [] old))) (Dict.map (\_ _ -> []) graph.projects)
        projectOrder ( id, _ ) =
            ( if id == graph.selectedProjectId then 1 else if Dict.get id graph.projects |> Maybe.map .external |> Maybe.withDefault True then 2 else 1, id )
        placeGroup ( id, tasks ) accumulated =
            let
                roots = List.filter (\task -> family task == task.id) tasks |> List.sortBy ordering
                placeFamily task ( y, nodes, frames ) =
                    let
                        children = List.filter (\child -> child.id /= task.id && family child == task.id) tasks |> List.sortBy ordering
                        parentHeight = height task
                        addChild child ( childY, childNodes ) =
                            let box = { x = 32, y = childY, width = 316, height = height child } in
                            (childY + box.height + 24, Dict.insert child.id {task=child,box=box,familyId=task.id} childNodes)
                        parent = {task=task,box={x=0,y=y,width=380,height=parentHeight},familyId=task.id}
                        ( bottom, withChildren ) = List.foldl addChild (y + parentHeight + 28, Dict.insert task.id parent nodes) children
                        frameHeight = if List.isEmpty children then parentHeight else bottom-y
                    in (y+frameHeight+64,withChildren,if List.isEmpty children then frames else Dict.insert task.id {x=0,y=y,width=380,height=frameHeight} frames)
                ( total, localNodes, localFrames ) = List.foldl placeFamily (0,Dict.empty,Dict.empty) roots
                contentHeight = max 80 (total-64)
                labelHeight = Dict.get id graph.projects |> Maybe.map (\project -> max 20 (Dict.get id labelMeasurements |> Maybe.withDefault (20 * toFloat (max 1 (ceiling (toFloat (String.length project.name)/24)))))) |> Maybe.withDefault 0
                groupHeight = contentHeight+labelHeight+32
                radius = sqrt (190^2 + (groupHeight/2)^2) + 40
                cx = accumulated.cursor + radius
                cy = radius + 80
                shift box = {box|x=box.x+cx-190,y=box.y+cy-groupHeight/2+labelHeight+32}
                translatedNodes = Dict.map (\_ node -> {node|box=shift node.box}) localNodes
                translatedFrames = Dict.map (\_ box -> shift box) localFrames
                circle = Dict.get id graph.projects |> Maybe.map (\project -> {project=project,x=cx,y=cy,radius=radius,label={x=cx-190,y=cy-groupHeight/2,width=380,height=labelHeight}})
            in {cursor=accumulated.cursor+2*radius+200+toFloat (List.length graph.edges)*14,nodes=Dict.union translatedNodes accumulated.nodes,frames=Dict.union translatedFrames accumulated.frames,circles=Maybe.map (\value -> value::accumulated.circles) circle |> Maybe.withDefault accumulated.circles,maxRadius=max accumulated.maxRadius radius}
        placed = groups |> Dict.toList |> List.sortBy projectOrder |> List.foldl placeGroup {cursor=80,nodes=Dict.empty,frames=Dict.empty,circles=[],maxRadius=0}
        route index edge =
            Maybe.map2 (\source target ->
                let
                    a = source.box
                    b = target.box
                    familyBox node = Dict.get node.familyId placed.frames |> Maybe.withDefault node.box
                    sourceFamily = familyBox source
                    targetFamily = familyBox target
                    gutter = 24 + toFloat index * 7
                    sx = sourceFamily.x+sourceFamily.width+gutter
                    tx = if source.task.projectId == target.task.projectId then targetFamily.x+targetFamily.width+gutter else targetFamily.x-gutter
                    targetX = if source.task.projectId == target.task.projectId then b.x+b.width else b.x
                    start = {x=a.x+a.width,y=a.y+a.height/2}
                    finish = {x=targetX,y=b.y+b.height/2}
                    lane = 12 - toFloat index * 22
                    middle = if source.task.projectId == target.task.projectId then [{x=sx,y=start.y},{x=sx,y=finish.y}] else [{x=sx,y=start.y},{x=sx,y=lane},{x=tx,y=lane},{x=tx,y=finish.y}]
                    innerX = sourceFamily.x+sourceFamily.width-10-12*(toFloat index / toFloat (List.length graph.edges+1))
                    parentPort node = {x=innerX,y=node.box.y+node.box.height}
                    innerStart = if source.task.id == source.familyId then parentPort source else start
                    innerFinish = if target.task.id == target.familyId then parentPort target else {x=b.x+b.width,y=finish.y}
                    innerStartY = if source.task.id == source.familyId then innerStart.y+14 else innerStart.y
                    innerFinishY = if target.task.id == target.familyId then innerFinish.y+14 else innerFinish.y
                    points = if source.familyId == target.familyId then [innerStart,{x=innerX,y=innerStartY},{x=innerX,y=innerFinishY},innerFinish] else start::middle++[finish]
                in {edge=edge,points=points}) (Dict.get edge.prerequisiteId placed.nodes) (Dict.get edge.dependentId placed.nodes)
        edges = graph.edges |> List.indexedMap route |> List.filterMap identity
        corners box = [{x=box.x,y=box.y},{x=box.x+box.width,y=box.y+box.height}]
        extents = List.concatMap .points edges ++ (Dict.values placed.nodes |> List.concatMap (.box >> corners))
            ++ (List.concatMap (\circle -> [{x=circle.x-circle.radius,y=circle.y-circle.radius},{x=circle.x+circle.radius,y=circle.y+circle.radius}]) placed.circles)
        left = min 0 (List.map .x extents |> List.minimum |> Maybe.withDefault 0)-40
        top = min 0 (List.map .y extents |> List.minimum |> Maybe.withDefault 0)-40
        right = max 500 (List.map .x extents |> List.maximum |> Maybe.withDefault 500)+40
        boundsBottom = max 400 (List.map .y extents |> List.maximum |> Maybe.withDefault 400)+40
    in {nodes=placed.nodes,frames=placed.frames,circles=List.reverse placed.circles,edges=edges,bounds={x=left,y=top,width=right-left,height=boundsBottom-top}}
