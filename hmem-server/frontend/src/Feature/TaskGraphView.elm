module Feature.TaskGraphView exposing (init, reconcile, update, view, viewReturn)

import Api
import Browser.Navigation as Nav
import Dict
import Feature.Focus as Focus
import Feature.TaskGraph as Graph
import Feature.TaskGraphNavigation as Navigation
import Helpers
import Html exposing (Html, button, div, input, label, span, text)
import Html.Attributes as H
import Html.Events exposing (onCheck, onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import Ports
import SessionActivityLight
import Svg
import Svg.Attributes as S
import Svg.Keyed
import TaskGraphLayout as Layout
import TaskGraphNavigation as History
import Types exposing (..)
import Url


init : TaskGraphViewModel
init = {data=Graph.init,generation=0,measurements=Dict.empty,projectMeasurements=Dict.empty,layout=Nothing,canvasKey=Nothing,detail=Nothing,returnFocus=Nothing,nextFocusToken=1,filters=Graph.defaultFilters,fitRevision=0,focusNotice=Nothing,activityTargets=[],activity=Dict.empty}


visible : TaskGraphViewModel -> Maybe Graph.Graph
visible state = state.data.graph |> Maybe.map (Graph.visibleGraph state.filters)


relayout : TaskGraphViewModel -> TaskGraphViewModel
relayout state = {state|layout=visible state |> Maybe.map (Layout.buildWithLabels state.measurements state.projectMeasurements)}


encodeGuard : Graph.Guard -> Encode.Value
encodeGuard owner = Encode.object [("workspaceId",Encode.string owner.workspaceId),("projectId",Encode.string owner.projectId),("audienceId",Encode.string owner.audienceId),("sessionEpoch",Encode.int owner.sessionEpoch),("targetGeneration",Encode.int owner.targetGeneration)]


guardDecoder : Decode.Decoder Graph.Guard
guardDecoder = Decode.map5 Graph.Guard (Decode.field "workspaceId" Decode.string) (Decode.field "projectId" Decode.string) (Decode.field "audienceId" Decode.string) (Decode.field "sessionEpoch" Decode.int) (Decode.field "targetGeneration" Decode.int)


reconcile : Msg -> Model -> (Model,Cmd Msg) -> (Model,Cmd Msg)
reconcile msg previous (model,command) =
    let
        (next,nextCommand)=reconcileGraph msg previous (model,command)
        state=next.taskGraph
        ids=if Navigation.current next == Nothing then [] else visible state |> Maybe.map (.tasks >> Dict.keys) |> Maybe.withDefault []
        changed=ids/=state.activityTargets || state.data.guard/=previous.taskGraph.data.guard
        activityCommand=if changed then state.data.guard |> Maybe.map (\owner -> Ports.taskGraphDataCommand (Encode.object [("kind",Encode.string "activity"),("guard",encodeGuard owner),("taskIds",Encode.list Encode.string ids)])) |> Maybe.withDefault Cmd.none else Cmd.none
    in ({next|taskGraph={state|activityTargets=ids,activity=Dict.filter (\id _ -> List.member id ids) state.activity}},Cmd.batch [nextCommand,activityCommand])


reconcileGraph : Msg -> Model -> (Model,Cmd Msg) -> (Model,Cmd Msg)
reconcileGraph msg previous (model,command) =
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
                let
                    filters = case msg of
                        TaskGraphSetFilter _ _ -> state.filters
                        _ -> History.filters model.url
                    next = {state|filters=filters,detail=state.detail |> Maybe.andThen (\receipt -> if receipt.entering then Just receipt else Nothing)}
                in ( {model|taskGraph=if filters == state.filters then next else relayout next}, command )
            else
                let
                    generation = max state.generation previous.taskGraph.generation+1
                    requested = {workspaceId=owner.workspaceId,projectId=target.projectId,audienceId=owner.audienceId,sessionEpoch=owner.sessionEpoch,targetGeneration=generation}
                    next = {state|data=Graph.begin requested,generation=generation,measurements=Dict.empty,projectMeasurements=Dict.empty,layout=Nothing,canvasKey=Just key,detail=Nothing,returnFocus=Nothing,filters=History.filters model.url,fitRevision=0,focusNotice=Nothing,activity=Dict.empty,activityTargets=[]}
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
                    else if Decode.decodeValue (Decode.field "kind" Decode.string) raw == Ok "activity" then
                        case Decode.decodeValue (Decode.map3 (\id active failed -> (id,{active=active,failed=failed})) (Decode.field "taskId" Decode.string) (Decode.field "active" (Decode.nullable Decode.bool)) (Decode.field "failed" Decode.bool)) raw of
                            Ok (id,proof) -> if List.member id state.activityTargets then ({model|taskGraph={state|activity=Dict.insert id proof state.activity}},Cmd.none) else (model,Cmd.none)
                            Err _ -> (model,Cmd.none)
                    else
                        let
                            kind = Decode.decodeValue (Decode.field "kind" Decode.string) raw |> Result.withDefault ""
                            items = Decode.decodeValue (Decode.field "items" (Decode.list Api.snapshotItemDecoder)) raw
                            next = case kind of
                                "snapshot" -> case items of
                                    Ok snapshotItems -> Graph.acceptSnapshot owner snapshotItems state.data
                                    Err _ -> Graph.fail owner "The graph snapshot is incomplete or invalid." state.data
                                "failed" -> Graph.fail owner "Task graph could not load. Retry to request a complete snapshot." state.data
                                "refreshing" -> Graph.refresh owner state.data
                                "recovering" -> Graph.recover owner state.data
                                "live" -> Graph.markLive owner state.data
                                "retired" -> Graph.invalidate owner "Task graph access is no longer available." state.data
                                _ -> state.data
                            measurements = state.measurements |> Dict.filter (\id _ -> next.graph |> Maybe.map (.tasks >> Dict.member id) |> Maybe.withDefault False)
                            projectMeasurements = state.projectMeasurements |> Dict.filter (\id _ -> next.graph |> Maybe.map (.projects >> Dict.member id) |> Maybe.withDefault False)
                            layout=if next.graph == state.data.graph then state.layout else next.graph |> Maybe.map (Graph.visibleGraph state.filters >> Layout.buildWithLabels measurements projectMeasurements)
                            authoritative = next.revision /= state.data.revision
                            unavailable = kind == "snapshot" && next.graph == Nothing && not next.pending
                            taskPresent id = items |> Result.map (List.any (\item -> item.kind == "task" && Decode.decodeValue (Decode.field "id" Decode.string) item.data == Ok id)) |> Result.withDefault False
                            activeTask receipt = (Helpers.parseFragment model.url.fragment).focus |> Maybe.andThen (\(entity,id) -> if entity == "task" then Just id else Nothing) |> Maybe.withDefault receipt.originTaskId
                            returnUrl = state.detail |> Maybe.andThen (\receipt -> if kind /= "retired" && (unavailable || (authoritative && not (taskPresent (activeTask receipt)))) then Just receipt.graphUrl else Nothing)
                            currentDetail = if kind == "retired" then Nothing else state.detail
                            command = returnUrl |> Maybe.andThen (\url -> model.key |> Maybe.map (\key -> Nav.replaceUrl key url)) |> Maybe.withDefault Cmd.none
                        in ({model|taskGraph={state|data=next,layout=layout,measurements=measurements,projectMeasurements=projectMeasurements,detail=currentDetail,activity=if kind=="retired" then Dict.empty else state.activity,returnFocus=if kind == "retired" || returnUrl /= Nothing then Nothing else state.returnFocus}},command)
        TaskGraphCanvasReceived raw ->
            if (Decode.decodeValue (Decode.field "key" Decode.string) raw |> Result.toMaybe) /= state.canvasKey || state.canvasKey == Nothing then (model,Cmd.none)
            else if Decode.decodeValue (Decode.field "kind" Decode.string) raw == Ok "focused" then
                if (state.returnFocus |> Maybe.map .token) == (Decode.decodeValue (Decode.field "token" Decode.int) raw |> Result.toMaybe) then ({model|taskGraph={state|returnFocus=Nothing,focusNotice=if Decode.decodeValue (Decode.field "fallback" Decode.bool) raw == Ok True then Just "The task is not visible in this graph. Adjust the filters or go Back." else Nothing}},Cmd.none) else (model,Cmd.none)
            else
                let
                    incoming=Decode.decodeValue (Decode.field "measurements" (Decode.list (Decode.map2 Tuple.pair (Decode.field "id" Decode.string) (Decode.field "height" Decode.float)))) raw |> Result.withDefault []
                    measured=List.foldl (\(id,height) values -> if height >= 40 && height < 100000 && (state.data.graph |> Maybe.map (.tasks >> Dict.member id) |> Maybe.withDefault False) then Dict.insert id height values else values) state.measurements incoming
                    labels=Decode.decodeValue (Decode.field "labels" (Decode.list (Decode.map2 Tuple.pair (Decode.field "id" Decode.string) (Decode.field "height" Decode.float)))) raw |> Result.withDefault []
                    projectMeasurements=List.foldl (\(id,height) values -> if height > 0 && height < 100000 && (state.data.graph |> Maybe.map (.projects >> Dict.member id) |> Maybe.withDefault False) then Dict.insert id height values else values) state.projectMeasurements labels
                in if measured==state.measurements && projectMeasurements==state.projectMeasurements then (model,Cmd.none)
                   else ({model|taskGraph=relayout {state|measurements=measured,projectMeasurements=projectMeasurements}},Cmd.none)
        TaskGraphSetFilter name enabled ->
            case Navigation.current model of
                Nothing -> (model,Cmd.none)
                Just _ ->
                    let
                        filters = if name == "done" then {showDone=enabled,showCancelled=state.filters.showCancelled} else if name == "cancelled" then {showDone=state.filters.showDone,showCancelled=enabled} else state.filters
                    in if filters == state.filters then (model,Cmd.none)
                       else ({model|taskGraph=relayout {state|filters=filters,fitRevision=state.fitRevision+1,measurements=Dict.empty,projectMeasurements=Dict.empty,focusNotice=Nothing}},model.key |> Maybe.map (\key -> Nav.replaceUrl key (History.withFilters filters model.url)) |> Maybe.withDefault Cmd.none)
        TaskGraphOpenTask id ->
            case (Navigation.guard model,Navigation.current model,visible state) of
                (Just owner,Just route,Just graph) ->
                    if not (Dict.member id graph.tasks) then (model,Cmd.none)
                    else
                        let
                            currentUrl=if History.parse model.url == Just route then Just (Url.toString model.url) else Nothing
                            remembered=route.entryId |> Maybe.andThen (\entryId -> History.lookup owner entryId model.taskGraphNavigation.history) |> Maybe.andThen (\entry -> if entry.projectId == route.projectId then Just entry.graphUrl else Nothing)
                            graphUrl=case currentUrl of
                                Just current -> Just current
                                Nothing -> remembered
                        in case graphUrl of
                            Nothing -> (model,Cmd.none)
                            Just destination ->
                                let receipt={scope=owner,graphUrl=destination,projectId=route.projectId,entryId=route.entryId,entering=True,originTaskId=id}
                                in Focus.update (FocusEntity "task" id) {model|taskGraph={state|detail=Just receipt,focusNotice=Nothing}}
                _ -> (model,Cmd.none)
        TaskGraphReturn ->
            case state.detail of
                Just receipt -> if Navigation.guard model == Just receipt.scope then ({model|taskGraph={state|returnFocus=Just {taskId=receipt.originTaskId,token=state.nextFocusToken},nextFocusToken=state.nextFocusToken+1}},model.key |> Maybe.map (\key -> Nav.replaceUrl key receipt.graphUrl) |> Maybe.withDefault Cmd.none) else (model,Cmd.none)
                Nothing -> (model,Cmd.none)
        TaskGraphRetry ->
            case state.data.guard of
                Just owner -> ({model|taskGraph={state|data=Graph.refresh owner state.data}},Ports.taskGraphDataCommand (Encode.object [("kind",Encode.string "retry"),("guard",encodeGuard owner)]))
                Nothing -> (model,Cmd.none)
        TaskGraphActivityRetry ->
            (model,state.data.guard |> Maybe.map (\owner -> Ports.taskGraphDataCommand (Encode.object [("kind",Encode.string "activity_retry"),("guard",encodeGuard owner)])) |> Maybe.withDefault Cmd.none)
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
    div [H.class "task-graph-page",H.id "task-graph-page",H.attribute "data-task-graph-scene" (Encode.encode 0 (Encode.object [("key",Encode.string (Maybe.withDefault "" state.canvasKey)),("ready",Encode.bool (state.layout /= Nothing)),("fitRevision",Encode.int state.fitRevision),("focus",focusMetadata state)]))]
        [ div [H.class "page-header"] [Html.h2 [] [text "Task Graph"],button [H.class "btn-small btn-ghost",H.type_ "button",H.id "task-graph-back",onClick TaskGraphBack] [text "Back"]]
        , div [H.class "task-graph-filters"]
            [ label [] [input [H.type_ "checkbox",H.id "task-graph-show-done",H.checked state.filters.showDone,onCheck (TaskGraphSetFilter "done")] [],text " Show Done"]
            , label [] [input [H.type_ "checkbox",H.id "task-graph-show-cancelled",H.checked state.filters.showCancelled,onCheck (TaskGraphSetFilter "cancelled")] [],text " Show Cancelled"] ]
        , state.focusNotice |> Maybe.map (\message -> div [H.class "task-status-text",H.attribute "role" "status"] [text message]) |> Maybe.withDefault (text "")
        , if Dict.values state.activity |> List.any .failed then div [H.class "task-status-text",H.attribute "role" "status"] [text "Agent activity could not be checked. ",button [H.type_ "button",H.class "btn-small btn-ghost",onClick TaskGraphActivityRetry] [text "Retry agent activity"]] else text ""
        , case state.layout of
            Just layout ->
                div [] [liveStatus model,layout.problem |> Maybe.map (\message -> div [H.attribute "role" "alert"] [text message]) |> Maybe.withDefault (text ""),if Dict.isEmpty layout.nodes then div [H.class "empty-state",H.attribute "role" "status"] [text (if state.data.graph |> Maybe.map (.tasks >> Dict.isEmpty) |> Maybe.withDefault True then "This project has no tasks." else "No tasks match filters.")] else canvas model layout]
            Nothing -> case state.data.status of
                Graph.Failed message -> div [H.class "empty-state",H.attribute "role" "alert"] [text message,button [H.class "btn-small",H.type_ "button",onClick TaskGraphRetry] [text "Retry"]]
                _ -> div [H.class "empty-state",H.attribute "role" "status"] [text "Loading a complete task graph…"]
        ]


focusMetadata : TaskGraphViewModel -> Encode.Value
focusMetadata state = state.returnFocus |> Maybe.map (\receipt -> Encode.object [("taskId",Encode.string receipt.taskId),("token",Encode.int receipt.token)]) |> Maybe.withDefault Encode.null


liveStatus : Model -> Html Msg
liveStatus model =
    let state=model.taskGraph.data in
    div [H.class "task-status-text",H.attribute "role" "status",H.attribute "aria-live" "polite",H.attribute "data-graph-live" (if state.live then "true" else "false")]
        (case state.status of
            Graph.Stale message -> [text message,button [H.class "btn-small btn-ghost",H.type_ "button",onClick TaskGraphRetry] [text "Retry"]]
            Graph.Recovering -> [text "Reconnecting live task graph…",button [H.class "btn-small btn-ghost",H.type_ "button",onClick TaskGraphRetry] [text "Retry"]]
            Graph.Refreshing -> [text "Updating task graph…"]
            _ -> [text (if state.live then "Live" else "Connecting live updates…")])


canvas : Model -> Layout.Layout -> Html Msg
canvas model layout =
    let
        number=String.fromFloat
        boxAttributes box=[S.x (number box.x),S.y (number box.y),S.width (number box.width),S.height (number box.height)]
        nodeView node =
            let
                active=Dict.get node.task.id model.taskGraph.activity |> Maybe.andThen .active |> (==) (Just True)
                appearance=Helpers.taskPresentationFromChildren node.task.status (Dict.get node.task.id model.taskGraph.data.runningChildren |> Maybe.withDefault 0) |> Helpers.taskCardStatusClass
            in
            Svg.foreignObject (boxAttributes node.box)
                [button [H.type_ "button",H.class ("task-graph-node " ++ statusClass node.task.status ++ appearance ++ (if node.task.parentId /= Nothing then " graph-subtask" else ""))
                    ,H.attribute "data-graph-task" node.task.id,H.attribute "aria-label" (node.task.title ++ ", " ++ Api.taskStatusToString node.task.status ++ (if active then ", active agent session" else ""))
                    ,onClick (TaskGraphOpenTask node.task.id)]
                    [span [H.class "task-graph-node-title"] [text node.task.title],span [H.class "task-graph-node-meta"] [span [H.class ("task-graph-status " ++ Helpers.statusControlClass "task" (Api.taskStatusToString node.task.status))] [text (Helpers.taskStatusDisplayText node.task.status)],SessionActivityLight.view active "Active agent session on this task"]]]
        circleView circle = Svg.g []
            [Svg.circle [S.cx (number circle.x),S.cy (number circle.y),S.r (number circle.radius),H.attribute "data-graph-project" circle.project.id,S.class (if circle.project.external then "task-graph-project graph-external-project" else "task-graph-project")] []
            ,Svg.foreignObject (boxAttributes circle.label) [div [H.class "task-graph-project-name",H.attribute "data-graph-project-label" circle.project.id] [text circle.project.name]]]
        edgeView routed = Svg.polyline [S.points (routed.points |> List.map (\point -> number point.x ++ "," ++ number point.y) |> String.join " "),S.class "task-graph-edge",S.markerEnd "url(#task-graph-arrow)",H.attribute "data-prerequisite" routed.edge.prerequisiteId,H.attribute "data-dependent" routed.edge.dependentId] []
        measured = Dict.keys layout.nodes |> List.all (\id -> Dict.member id model.taskGraph.measurements)
        labelsMeasured = List.all (\circle -> Dict.member circle.project.id model.taskGraph.projectMeasurements) layout.circles
        metadata=Encode.object [("key",Encode.string (Maybe.withDefault "" model.taskGraph.canvasKey)),("measured",Encode.bool (measured && labelsMeasured)),("revision",Encode.int model.taskGraph.data.revision)
            ,("fitRevision",Encode.int model.taskGraph.fitRevision)
            ,("initialAnchor",layout.initialAnchor |> Maybe.map (\box -> Encode.object [("x",Encode.float (box.x+box.width/2)),("y",Encode.float (box.y+box.height/2))]) |> Maybe.withDefault Encode.null)
            ,("focus",focusMetadata model.taskGraph)
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
