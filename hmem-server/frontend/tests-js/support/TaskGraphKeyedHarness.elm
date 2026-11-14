port module TaskGraphKeyedHarness exposing (main)

import Api
import AppShell
import Browser
import Dict
import Feature.TaskGraph as Graph
import Feature.TaskGraphView as View
import Html
import Set
import TaskGraphLayout as Layout
import Types exposing (..)
import Url


port changeGraph : (Bool -> msg) -> Sub msg

type Msg = Change Bool | Ignore Types.Msg

main = Browser.element {init=\() -> (fixture False,Cmd.none),update=\msg model -> case msg of
    Change added -> (fixture added,Cmd.none)
    Ignore _ -> (model,Cmd.none),view=View.view >> Html.map Ignore,subscriptions=\_ -> changeGraph Change}


fixture : Bool -> Model
fixture added =
    let
        url={protocol=Url.Http,host="fixture.test",port_=Nothing,path="/workspace/w",query=Nothing,fragment=Just "tab=projects&task-graph=p"}
        initial=AppShell.initModel Nothing url (WorkspacePage "w") {apiUrl="",wsUrl="",sessionId="runtime",runtimeMode="test",authTokenStorageKey="token",authTokenPresent=False,loginUrl=Nothing,logoutUrl=Nothing} Nothing {tab=ProjectsTab,focus=Nothing,observationId=Nothing}
        task id={id=id,title=id,status=Api.Todo,projectId=Just "p",parentId=Nothing,external=False}
        ids=if added then ["a-new","m-stable","z-target"] else ["m-stable","z-target"]
        graph={selectedProjectId="p",projects=Dict.singleton "p" {id="p",name="Project",parentId=Nothing,external=False},tasks=List.map (\id -> (id,task id)) ids |> Dict.fromList,edges=[],inScopeTaskIds=Set.fromList ids,externalTaskIds=Set.empty}
    in {initial|taskGraph={data={guard=Nothing,status=Graph.Ready,graph=Just graph},generation=1,measurements=Dict.empty,projectMeasurements=Dict.empty,layout=Just (Layout.build Dict.empty graph),canvasKey=Just "keyed-fixture",detail=Nothing,returnFocus=Nothing,nextFocusToken=1}}
