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
import TaskGraphNavigation as History
import TaskGraphLayout as Layout
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
    , test "live refresh and failed read retain scene key, layout and active detail receipt" <| \_ ->
        let
            detail=View.update (TaskGraphOpenTask "task") base |> Tuple.first |> taskUrl "task"
            updating=receive "refreshing" detail
            failed=receive "failed" updating
        in Expect.all
            [\_ -> Expect.equal base.taskGraph.canvasKey failed.taskGraph.canvasKey
            ,\_ -> Expect.equal base.taskGraph.layout failed.taskGraph.layout
            ,\_ -> Expect.equal detail.taskGraph.detail failed.taskGraph.detail
            ,\_ -> Expect.equal detail.taskGraph.data.graph failed.taskGraph.data.graph
            ,\_ -> Expect.equal False failed.taskGraph.data.live
            ,\_ -> case failed.taskGraph.data.status of
                Graph.Stale _ -> Expect.pass
                _ -> Expect.fail "failed live refresh must be stale and retryable"
            ] ()
    , test "permission retirement clears graph, layout and task-detail receipt immediately" <| \_ ->
        let
            detail=View.update (TaskGraphOpenTask "task") base |> Tuple.first |> taskUrl "task"
            retired=receive "retired" detail
        in Expect.equal (Nothing,Nothing,Nothing) (retired.taskGraph.data.graph,retired.taskGraph.layout,retired.taskGraph.detail)
    , test "retry keeps the existing scene identity and old graph while requesting a fresh read" <| \_ ->
        let
            failed=receive "failed" base
            retry=View.update TaskGraphRetry failed |> Tuple.first
        in Expect.equal (base.taskGraph.canvasKey,base.taskGraph.data.graph,Graph.Refreshing)
            (retry.taskGraph.canvasKey,retry.taskGraph.data.graph,retry.taskGraph.data.status)
    , test "filter URL reconciliation keeps transport identity, canonical data and fit revision" <| \_ ->
        let
            nextUrl=History.withFilters {showDone=False,showCancelled=False} base.url |> Url.fromString |> Maybe.withDefault base.url
            next=Navigation.reconcile base ({base|url=nextUrl},Cmd.none) |> View.reconcile (UrlChanged nextUrl) base |> Tuple.first
            toggled=View.update (TaskGraphSetFilter "done" False) base |> Tuple.first
        in Expect.all [\_ -> Expect.equal base.taskGraph.data next.taskGraph.data,\_ -> Expect.equal base.taskGraph.canvasKey next.taskGraph.canvasKey
            ,\_ -> Expect.equal base.taskGraphNavigation.metadataGeneration next.taskGraphNavigation.metadataGeneration
            ,\_ -> Expect.equal {showDone=False,showCancelled=False} next.taskGraph.filters,\_ -> Expect.equal 0 next.taskGraph.fitRevision
            ,\_ -> Expect.equal 1 toggled.taskGraph.fitRevision] ()
    , test "hidden stale activation is rejected and current filtered URL survives detail hops" <| \_ ->
        let
            state=base.taskGraph
            data=state.data
            graph=data.graph |> Maybe.map (\value -> {value|tasks=Dict.map (\_ task -> {task|status=Api.Done}) value.tasks})
            nextUrl=History.withFilters {showDone=False,showCancelled=True} base.url |> Url.fromString |> Maybe.withDefault base.url
            filtered={base|url=nextUrl,taskGraph={state|data={data|graph=graph},filters={showDone=False,showCancelled=True}}}
            rejected=View.update (TaskGraphOpenTask "task") filtered |> Tuple.first
            opened=View.update (TaskGraphOpenTask "task") {base|url=nextUrl} |> Tuple.first
        in Expect.equal (Nothing,Just (Url.toString nextUrl)) (rejected.taskGraph.detail,opened.taskGraph.detail |> Maybe.map .graphUrl)
    , test "stale graph activation cannot use a remembered different project" <| \_ ->
        let
            currentUrl=base.url
            nav=base.taskGraphNavigation
            history=nav.history
            entry={guard={workspaceId="w",audienceId="reader",sessionEpoch=1},projectId="wrong",sourceUrl="",graphUrl=Url.toString currentUrl,origin={focus=base.focus,filters={query="",showOnly=ShowAll,showEmptyProjects=True,hasActiveSession=False,priority=AnyPriority,projectStatuses=[],taskStatuses=[]},focusedFilters=Nothing,collapsedNodes=Dict.empty,nodesCollapsedByDefault=False,expandedCards=Dict.empty,viewportTop=0,viewportIndex=base.cards.viewport.index,scrollTop=0,anchorDelta=Nothing}}
            stale={base|url={currentUrl|fragment=Just "tab=projects"},taskGraphNavigation={nav|history={history|entries=Dict.singleton "original-entry" entry}}}
            next=View.update (TaskGraphOpenTask "task") stale |> Tuple.first
        in Expect.equal Nothing next.taskGraph.detail
    ]


receive kind model =
    let owner=model.taskGraph.data.guard |> Maybe.withDefault {workspaceId="w",projectId="p",audienceId="reader",sessionEpoch=1,targetGeneration=1} in
    View.update (TaskGraphDataReceived (Encode.object [("kind",Encode.string kind),("guard",Encode.object [("workspaceId",Encode.string owner.workspaceId),("projectId",Encode.string owner.projectId),("audienceId",Encode.string owner.audienceId),("sessionEpoch",Encode.int owner.sessionEpoch),("targetGeneration",Encode.int owner.targetGeneration)])])) model |> Tuple.first


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
        graph={selectedProjectId="p",projects=Dict.singleton "p" {id="p",name="Project",parentId=Nothing,external=False},tasks=Dict.singleton "task" task,edges=[],inScopeTaskIds=Set.singleton "task",externalTaskIds=Set.empty}
        data=state.data
    in {routed|taskGraph={state|data={data|status=Graph.Ready,graph=Just graph,pending=False,live=True,revision=1},layout=Just (Layout.build Dict.empty graph)}}
