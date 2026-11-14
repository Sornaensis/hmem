module TaskGraphViewTest exposing (suite)

import Api
import AppShell
import Dict
import Expect
import Feature.Focus as Focus
import Feature.TaskGraph as Graph
import Feature.TaskGraphNavigation as Navigation
import Feature.TaskGraphView as View
import Json.Encode as Encode
import Set
import Test exposing (describe,test)
import Types exposing (..)
import Url


suite = describe "graph detail and snapshot authority"
    [ test "task/dependency hops retain the exact original graph entry without filling ordinary task caches" <| \_ ->
        let
            clicked=View.update (TaskGraphOpenTask "task") base |> View.reconcile (TaskGraphOpenTask "task") base |> Tuple.first
            detail=taskUrl "task" clicked
            hopped=Focus.update (FocusEntity "task" "dependency") detail |> Tuple.first |> taskUrl "dependency"
            receipt=hopped.taskGraph.detail
        in Expect.all [\_ -> Expect.equal (Just (Url.toString url)) (Maybe.map .graphUrl receipt),\_ -> Expect.equal (Just (Just "original-entry")) (Maybe.map .entryId receipt),\_ -> Expect.equal 0 (Dict.size hopped.tasks),\_ -> Expect.equal base.taskGraph.canvasKey hopped.taskGraph.canvasKey] ()
    , test "unrelated navigation or a new authorization epoch retires graph detail authority" <| \_ ->
        let
            clicked=View.update (TaskGraphOpenTask "task") base |> View.reconcile (TaskGraphOpenTask "task") base |> Tuple.first |> taskUrl "task"
            unrelated=View.reconcile NoOp clicked ({clicked|activeTab=TimelineTab},Cmd.none) |> Tuple.first
            changed=View.reconcile NoOp clicked ({clicked|sessionRequestEpoch=2},Cmd.none) |> Tuple.first
            late=View.update (TaskGraphDataReceived (Encode.object [("guard",Encode.object [("workspaceId",Encode.string "w"),("projectId",Encode.string "p"),("audienceId",Encode.string "reader"),("sessionEpoch",Encode.int 1),("targetGeneration",Encode.int 1)]),("kind",Encode.string "failed")])) changed |> Tuple.first
        in Expect.equal (Nothing,Nothing,Nothing) (unrelated.taskGraph.detail,changed.taskGraph.detail,late.taskGraph.data.graph)
    , test "real AppShell credential reset preserves retired generation and rejects delayed graph data" <| \_ ->
        let
            reset=AppShell.prepareOwned (AppShell.AuthTokenChangedMsg False) base |> View.reconcile (AuthTokenChanged False) base |> Tuple.first
            oldGuard=base.taskGraph.data.guard
            reopened=Navigation.reconcile reset ({reset|auth=base.auth,sessionContext=base.sessionContext},Cmd.none) |> View.reconcile NoOp reset |> Tuple.first
        in Expect.all [\_ -> Expect.equal Nothing reset.taskGraph.canvasKey,\_ -> Expect.equal base.taskGraph.generation reset.taskGraph.generation,\_ -> Expect.notEqual oldGuard reopened.taskGraph.data.guard] ()
    ]


taskUrl id model =
    let currentUrl=model.url
        nav=model.taskGraphNavigation
        next={model|url={currentUrl|fragment=Just ("tab=projects&focus=task%3A"++id)},taskGraphNavigation={nav|active=Nothing}}
    in View.reconcile (UrlChanged next.url) model (next,Cmd.none) |> Tuple.first


url : Url.Url
url={protocol=Url.Http,host="app.test",port_=Nothing,path="/workspace/w",query=Nothing,fragment=Just "tab=projects&task-graph=p&graph-entry=original-entry"}


base : Model
base=
    let
        initial=AppShell.initModel Nothing url (WorkspacePage "w") {apiUrl="",wsUrl="",sessionId="runtime",runtimeMode="test",authTokenStorageKey="token",authTokenPresent=False,loginUrl=Nothing,logoutUrl=Nothing} Nothing {tab=ProjectsTab,focus=Nothing,observationId=Nothing}
        session={authMode="test",effectiveActor=Nothing,principal={actorType="user",actorId="reader",actorLabel="Reader",authority="test",grantUserId=Nothing},globalPermissions={superadmin=False,createWorkspace=False},workspace=Just {workspaceId="w",role=Just "read",canRead=True,canEdit=False,canAdmin=False}}
        ready={initial|auth={status=AuthReady,mode=Just "test"},selectedWorkspaceId=Just "w",sessionRequestEpoch=1,sessionContext=Just session}
        routed=Navigation.reconcile ready (ready,Cmd.none) |> View.reconcile NoOp ready |> Tuple.first
        state=routed.taskGraph
        task={id="task",title="Task",status=Api.Todo,projectId=Just "p",parentId=Nothing,external=False}
        graph={selectedProjectId="p",projects=Dict.empty,tasks=Dict.singleton "task" task,edges=[],inScopeTaskIds=Set.singleton "task",externalTaskIds=Set.empty}
        data=state.data
    in {routed|taskGraph={state|data={data|status=Graph.Ready,graph=Just graph}}}
