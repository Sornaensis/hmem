module Feature.TaskGraphView exposing (init, reconcile, update, view, viewReturn)

import Api
import Browser.Navigation as Nav
import Dict
import Feature.Focus as Focus
import Feature.TaskGraph as Graph
import Feature.TaskGraphNavigation as Navigation
import Helpers
import Html exposing (Html, button, div, span, text)
import Html.Attributes as H
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import Ports
import Svg
import Svg.Attributes as S
import Svg.Keyed
import TaskGraphLayout as Layout
import TaskGraphNavigation as History
import Types exposing (..)
import Url


init : TaskGraphViewModel
init = {data=Graph.init,generation=0,measurements=Dict.empty,projectMeasurements=Dict.empty,layout=Nothing,canvasKey=Nothing,detail=Nothing,returnFocus=Nothing,nextFocusToken=1}


encodeGuard : Graph.Guard -> Encode.Value
encodeGuard owner = Encode.object [("workspaceId",Encode.string owner.workspaceId),("projectId",Encode.string owner.projectId),("audienceId",Encode.string owner.audienceId),("sessionEpoch",Encode.int owner.sessionEpoch),("targetGeneration",Encode.int owner.targetGeneration)]


guardDecoder : Decode.Decoder Graph.Guard
guardDecoder = Decode.map5 Graph.Guard (Decode.field "workspaceId" Decode.string) (Decode.field "projectId" Decode.string) (Decode.field "audienceId" Decode.string) (Decode.field "sessionEpoch" Decode.int) (Decode.field "targetGeneration" Decode.int)


reconcile : Msg -> Model -> (Model,Cmd Msg) -> (Model,Cmd Msg)
reconcile _ previous (model,command) =
    let
        state = model.taskGraph
        scope = Navigation.guard model
        route = Navigation.current model
        taskDetails = model.activeTab == ProjectsTab && (case model.page of
            WorkspacePage _ -> True
            _ -> False) && ((Helpers.parseFragment model.url.fragment).focus |> Maybe.map (Tuple.first >> (==) "task") |> Maybe.withDefault False)
        detail = state.detail |> Maybe.andThen (\receipt -> if Just receipt.scope == scope && taskDetails then Just {receipt|entering=False} else Nothing)
        cancel = Ports.taskGraphDataCommand (Encode.object [("kind",Encode.string "cancel")])
    in
    case (scope,route) of
        (Just owner,Just target) ->
            let
                key = Encode.encode 0 (Encode.list Encode.string [owner.workspaceId,owner.audienceId,String.fromInt owner.sessionEpoch,target.projectId,Maybe.withDefault "direct" target.entryId])
            in
            if state.canvasKey == Just key then
                ( {model|taskGraph={state|detail=state.detail |> Maybe.andThen (\receipt -> if receipt.entering then Just receipt else Nothing)}}, command )
            else
                let
                    generation = max state.generation previous.taskGraph.generation+1
                    requested = {workspaceId=owner.workspaceId,projectId=target.projectId,audienceId=owner.audienceId,sessionEpoch=owner.sessionEpoch,targetGeneration=generation}
                    next = {state|data=Graph.begin requested,generation=generation,measurements=Dict.empty,projectMeasurements=Dict.empty,layout=Nothing,canvasKey=Just key,detail=Nothing,returnFocus=Nothing}
                in ({model|taskGraph=next},Cmd.batch [command,Ports.taskGraphDataCommand (Encode.object [("kind",Encode.string "load"),("guard",encodeGuard requested)])])
        _ ->
            if detail /= Nothing then ({model|taskGraph={state|detail=detail}},command)
            else if state.canvasKey /= Nothing || previous.taskGraph.canvasKey /= Nothing then ({model|taskGraph={init|generation=max state.generation previous.taskGraph.generation,nextFocusToken=max state.nextFocusToken previous.taskGraph.nextFocusToken}},Cmd.batch [command,cancel])
            else (model,command)


update : Msg -> Model -> (Model,Cmd Msg)
update msg model =
    let state=model.taskGraph in
    case msg of
        TaskGraphDataReceived raw ->
            case Decode.decodeValue (Decode.field "guard" guardDecoder) raw of
                Err _ -> (model,Cmd.none)
                Ok owner ->
                    if state.data.guard /= Just owner then (model,Cmd.none)
                    else
                        let
                            next = case Decode.decodeValue (Decode.field "kind" Decode.string) raw of
                                Ok "snapshot" -> case Decode.decodeValue (Decode.field "items" (Decode.list Api.snapshotItemDecoder)) raw of
                                    Ok items -> Graph.acceptSnapshot owner items state.data
                                    Err _ -> Graph.fail owner "The graph snapshot is incomplete or invalid." state.data
                                Ok "failed" -> Graph.fail owner "Task graph could not load. Retry to request a complete snapshot." state.data
                                _ -> state.data
                            layout=next.graph |> Maybe.map (Layout.buildWithLabels state.measurements state.projectMeasurements)
                        in ({model|taskGraph={state|data=next,layout=layout}},Cmd.none)
        TaskGraphCanvasReceived raw ->
            if (Decode.decodeValue (Decode.field "key" Decode.string) raw |> Result.toMaybe) /= state.canvasKey || state.canvasKey == Nothing then (model,Cmd.none)
            else if Decode.decodeValue (Decode.field "kind" Decode.string) raw == Ok "focused" then
                if (state.returnFocus |> Maybe.map .token) == (Decode.decodeValue (Decode.field "token" Decode.int) raw |> Result.toMaybe) then ({model|taskGraph={state|returnFocus=Nothing}},Cmd.none) else (model,Cmd.none)
            else
                let
                    incoming=Decode.decodeValue (Decode.field "measurements" (Decode.list (Decode.map2 Tuple.pair (Decode.field "id" Decode.string) (Decode.field "height" Decode.float)))) raw |> Result.withDefault []
                    measured=List.foldl (\(id,height) values -> if height >= 40 && height < 100000 && (state.data.graph |> Maybe.map (.tasks >> Dict.member id) |> Maybe.withDefault False) then Dict.insert id height values else values) state.measurements incoming
                    labels=Decode.decodeValue (Decode.field "labels" (Decode.list (Decode.map2 Tuple.pair (Decode.field "id" Decode.string) (Decode.field "height" Decode.float)))) raw |> Result.withDefault []
                    projectMeasurements=List.foldl (\(id,height) values -> if height > 0 && height < 100000 && (state.data.graph |> Maybe.map (.projects >> Dict.member id) |> Maybe.withDefault False) then Dict.insert id height values else values) state.projectMeasurements labels
                in if measured==state.measurements && projectMeasurements==state.projectMeasurements then (model,Cmd.none)
                   else ({model|taskGraph={state|measurements=measured,projectMeasurements=projectMeasurements,layout=state.data.graph |> Maybe.map (Layout.buildWithLabels measured projectMeasurements)}},Cmd.none)
        TaskGraphOpenTask id ->
            case (Navigation.guard model,Navigation.current model,state.data.graph) of
                (Just owner,Just route,Just graph) ->
                    if not (Dict.member id graph.tasks) then (model,Cmd.none)
                    else
                        let
                            graphUrl=route.entryId |> Maybe.andThen (\entryId -> History.lookup owner entryId model.taskGraphNavigation.history) |> Maybe.map .graphUrl |> Maybe.withDefault (Url.toString model.url)
                            receipt={scope=owner,graphUrl=graphUrl,projectId=route.projectId,entryId=route.entryId,entering=True,originTaskId=id}
                        in Focus.update (FocusEntity "task" id) {model|taskGraph={state|detail=Just receipt}}
                _ -> (model,Cmd.none)
        TaskGraphReturn ->
            case state.detail of
                Just receipt -> if Navigation.guard model == Just receipt.scope then ({model|taskGraph={state|returnFocus=Just {taskId=receipt.originTaskId,token=state.nextFocusToken},nextFocusToken=state.nextFocusToken+1}},model.key |> Maybe.map (\key -> Nav.replaceUrl key receipt.graphUrl) |> Maybe.withDefault Cmd.none) else (model,Cmd.none)
                Nothing -> (model,Cmd.none)
        TaskGraphRetry ->
            ( {model|taskGraph={state|canvasKey=Nothing}}, Cmd.none )
        _ -> (model,Cmd.none)


viewReturn : Model -> Html Msg
viewReturn model =
    if model.taskGraph.detail /= Nothing then button [H.class "btn-small btn-ghost",H.type_ "button",H.id "task-graph-return",onClick TaskGraphReturn] [text "Back to graph"] else text ""


statusClass : Api.TaskStatus -> String
statusClass status = case status of
    Api.Done -> "graph-status-done"
    Api.InProgress -> "graph-status-in-progress"
    Api.Blocked -> "graph-status-blocked"
    _ -> "graph-status-todo"


view : Model -> Html Msg
view model =
    let state=model.taskGraph in
    div [H.class "task-graph-page",H.id "task-graph-page"]
        [ div [H.class "page-header"] [Html.h2 [] [text "Task Graph"],button [H.class "btn-small btn-ghost",H.type_ "button",H.id "task-graph-back",onClick TaskGraphBack] [text "Back"]]
        , case (state.data.status,state.layout) of
            (Graph.Ready,Just layout) ->
                if Dict.isEmpty layout.nodes then div [H.class "empty-state",H.attribute "role" "status"] [text "This project has no tasks."]
                else canvas model layout
            (Graph.Failed message,_) -> div [H.class "empty-state",H.attribute "role" "alert"] [text message,button [H.class "btn-small",H.type_ "button",onClick TaskGraphRetry] [text "Retry"]]
            _ -> div [H.class "empty-state",H.attribute "role" "status"] [text "Loading a complete task graph…"]
        ]


canvas : Model -> Layout.Layout -> Html Msg
canvas model layout =
    let
        number=String.fromFloat
        boxAttributes box=[S.x (number box.x),S.y (number box.y),S.width (number box.width),S.height (number box.height)]
        nodeView node =
            Svg.foreignObject (boxAttributes node.box)
                [button [H.type_ "button",H.class ("task-graph-node " ++ statusClass node.task.status ++ Helpers.taskCardStatusClass node.task.status ++ (if node.task.parentId /= Nothing then " graph-subtask" else ""))
                    ,H.attribute "data-graph-task" node.task.id,H.attribute "aria-label" (node.task.title ++ ", " ++ Api.taskStatusToString node.task.status)
                    ,onClick (TaskGraphOpenTask node.task.id)]
                    [span [H.class "task-graph-node-title"] [text node.task.title],span [H.class ("task-graph-status " ++ Helpers.taskStatusBadgeClass node.task.status)] [text (Helpers.taskStatusDisplayText node.task.status)]]]
        circleView circle = Svg.g []
            [Svg.circle [S.cx (number circle.x),S.cy (number circle.y),S.r (number circle.radius),S.class (if circle.project.external then "task-graph-project graph-external-project" else "task-graph-project")] []
            ,Svg.foreignObject (boxAttributes circle.label) [div [H.class "task-graph-project-name",H.attribute "data-graph-project-label" circle.project.id] [text circle.project.name]]]
        edgeView routed = Svg.polyline [S.points (routed.points |> List.map (\point -> number point.x ++ "," ++ number point.y) |> String.join " "),S.class "task-graph-edge",S.markerEnd "url(#task-graph-arrow)",H.attribute "data-prerequisite" routed.edge.prerequisiteId,H.attribute "data-dependent" routed.edge.dependentId] []
        measured = Dict.keys layout.nodes |> List.all (\id -> Dict.member id model.taskGraph.measurements)
        labelsMeasured = List.all (\circle -> Dict.member circle.project.id model.taskGraph.projectMeasurements) layout.circles
        metadata=Encode.object [("key",Encode.string (Maybe.withDefault "" model.taskGraph.canvasKey)),("measured",Encode.bool (measured && labelsMeasured))
            ,("focus",model.taskGraph.returnFocus |> Maybe.map (\receipt -> Encode.object [("taskId",Encode.string receipt.taskId),("token",Encode.int receipt.token)]) |> Maybe.withDefault Encode.null)
            ,("bounds",Encode.object [("x",Encode.float layout.bounds.x),("y",Encode.float layout.bounds.y),("width",Encode.float layout.bounds.width),("height",Encode.float layout.bounds.height)])
            ,("selected",Encode.object (layout.circles |> List.filter (\circle -> circle.project.id == (Navigation.current model |> Maybe.map .projectId |> Maybe.withDefault "")) |> List.head |> Maybe.map (\circle -> [("x",Encode.float circle.x),("y",Encode.float circle.y)]) |> Maybe.withDefault [("x",Encode.float 0),("y",Encode.float 0)]))]
    in div [H.class "task-graph-workbench"]
        [div [H.class "task-graph-toolbar"] [span [] [text "Prerequisite → dependent"],div [] (List.map (\(action,label)->button [H.type_ "button",H.class "btn-small btn-ghost",H.attribute "data-graph-control" action,H.attribute "aria-label" label] [text label]) [("out","Zoom out"),("in","Zoom in"),("fit","Fit")])]
        ,Svg.svg [S.class "task-graph-canvas",H.id "task-graph-canvas",H.attribute "aria-label" "Task dependency graph",H.attribute "data-task-graph-canvas" (Encode.encode 0 metadata)]
            [Svg.defs [] [Svg.marker [S.id "task-graph-arrow",S.viewBox "0 0 10 10",S.refX "10",S.refY "5",S.markerWidth "8",S.markerHeight "8",S.orient "auto-start-reverse"] [Svg.path [S.d "M 0 0 L 10 5 L 0 10 z",S.fill "#94a3b8"] []]]
            ,Svg.Keyed.node "g" [H.attribute "data-graph-world" ""]
                (List.map (\circle -> ("project:"++circle.project.id,circleView circle)) layout.circles
                ++ (Dict.toList layout.frames |> List.map (\(id,box) -> ("frame:"++id,Svg.rect (boxAttributes box++[S.rx "10",S.class "task-graph-family"]) [])))
                ++ List.map (\edge -> ("edge:"++Encode.encode 0 (Encode.list Encode.string [edge.edge.prerequisiteId,edge.edge.dependentId]),edgeView edge)) layout.edges
                ++ (Dict.toList layout.nodes |> List.map (\(id,node) -> ("task:"++id,nodeView node))))]
        ]
