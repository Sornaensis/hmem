module SessionActivityTest exposing (suite)
import Api
import AppShell
import Dict
import Expect
import Feature.SessionActivity as Activity
import Http
import Json.Encode as Encode
import Set
import Test exposing (describe,test)
import Time
import Types exposing (..)
import Url

suite = describe "bounded activity probes"
    [ test "ordinary catalogue members converge beyond fifty targets without duplicate group probes" <| \_ ->
        let
            first=pump home
            one=finish first
            all=finish one
            sizes=[first.sessionActivity.pending,one.sessionActivity.pending] |> List.filterMap (Maybe.map (.keys >> List.length))
        in Expect.equal ([50,7],57,Nothing) (sizes,Dict.size all.sessionActivity.cache,all.sessionActivity.pending)
    , test "scope retirement retains physical occupancy and rejects old positive rows" <| \_ ->
        let
            first=pump home
            groups=first.groups
            retired=pump {first | groups={groups | catalogueEpoch=groups.catalogueEpoch+1}}
            next=finish retired
        in Expect.equal (Just 1,Just 2,0) (retired.sessionActivity.pending |> Maybe.map .id,next.sessionActivity.pending |> Maybe.map .id,Dict.size next.sessionActivity.cache)
    , test "hidden and unmounted demand prunes cache but does not free a pending probe" <| \_ ->
        let
            first=pump home
            hidden=Activity.update (SessionActivityVisibility False) first |> Activity.reconcile first |> Tuple.first
            done=finish hidden
        in Expect.equal (([],Just 1),(Nothing,0)) ((hidden.sessionActivity.order,hidden.sessionActivity.pending |> Maybe.map .id),(done.sessionActivity.pending,Dict.size done.sessionActivity.cache))
    , test "hide then remount cannot revive a retired positive response" <| \_ ->
        let
            first=pump home
            hidden=Activity.update (SessionActivityVisibility False) first |> Activity.reconcile first |> Tuple.first
            restored=Activity.update (SessionActivityVisibility True) hidden |> Activity.reconcile hidden |> Tuple.first
            rows=first.sessionActivity.pending |> Maybe.map (.keys >> List.map (String.dropLeft 10 >> (\id -> (id,True))) >> Dict.fromList) |> Maybe.withDefault Dict.empty
            next=Activity.update (SessionActivityFetched 1 (Ok {workspaces=rows,projects=Dict.empty})) restored |> Activity.reconcile restored |> Tuple.first
        in Expect.equal (Just 1,0,Just 2) (restored.sessionActivity.pending |> Maybe.map .id,Dict.size next.sessionActivity.cache,next.sessionActivity.pending |> Maybe.map .id)
    , test "remove then readd the same ID retains its slot and rejects the retired reply" <| \_ ->
        let
            first=pump home
            removed=pump {first | workspaces=Dict.remove "w1" first.workspaces}
            restored=pump {removed | workspaces=home.workspaces}
            next=finish restored
        in Expect.equal (Just 1,0,Just 2) (restored.sessionActivity.pending |> Maybe.map .id,Dict.size next.sessionActivity.cache,next.sessionActivity.pending |> Maybe.map .id)
    , test "initial hidden documents do not probe and old flag sets remain compatible" <| \_ ->
        let
            state=home.sessionActivity
            hidden=pump {home | sessionActivity={state | visible=Activity.initialVisibility (Encode.object [("documentVisible",Encode.bool False)])}}
        in Expect.equal (Nothing,[],True) (hidden.sessionActivity.pending,hidden.sessionActivity.order,Activity.initialVisibility (Encode.object []))
    , test "ten-second polling and invalidations coalesce into one physical probe" <| \_ ->
        let
            settled=finish (finish (pump home))
            early=Activity.update (SessionActivityTick (Time.millisToPosix 9999)) settled |> Activity.reconcile settled |> Tuple.first
            due=Activity.update (SessionActivityTick (Time.millisToPosix 10000)) early |> Activity.reconcile early |> Tuple.first
            state=due.sessionActivity
            repeated=List.foldl (\_ model -> pump {model | sessionActivity={state | version=state.version+1}}) due (List.range 1 20)
        in Expect.equal (Nothing,Just 3,4) (early.sessionActivity.pending |> Maybe.map .id,repeated.sessionActivity.pending |> Maybe.map .id,repeated.sessionActivity.nextId)
    , test "failed and omitted results remain unknown and do not retry before cadence" <| \_ ->
        let
            first=pump home
            failed=Activity.update (SessionActivityFetched 1 (Err Http.Timeout)) first |> Activity.reconcile first |> Tuple.first
            cache=Dict.values failed.sessionActivity.cache
        in Expect.equal (True,Just 2,False) (List.all (.active >> (==) Nothing) cache,failed.sessionActivity.pending |> Maybe.map .id,Activity.active "workspace:w1" failed)
    , test "workspace projects demand only mounted IDs and never infer descendant membership" <| \_ ->
        let
            loading=home.dataLoading
            workspace={home | page=WorkspacePage "ws",selectedWorkspaceId=Just "ws",activeTab=ProjectsTab,sessionContext=Just context,dataLoading={loading | visibleDetailDemand=Just (Set.fromList ["mounted"],Set.fromList ["task"] )}}
        in Expect.equal (True,57) (List.member "project:mounted" (Activity.targets workspace),List.length (List.filter (String.startsWith "workspace:") (Activity.targets workspace)))
    , test "sidebar workspace probes remain present on Sessions and omit collapsed-only members" <| \_ ->
        let
            groupState=home.groups
            workspace={home | page=WorkspacePage "ws",selectedWorkspaceId=Just "ws",activeTab=SessionsTab,sessionContext=Just context,groups={groupState | collapsedGroups=Dict.fromList [("one",True),("two",True)]}}
        in Expect.equal (56,False) (List.length (Activity.targets workspace),List.member "workspace:w1" (Activity.targets workspace))
    ]

pump model = Activity.reconcile model (model,Cmd.none) |> Tuple.first

finish model =
    case model.sessionActivity.pending of
        Nothing -> model
        Just request ->
            let rows=request.keys |> List.filter (String.startsWith "workspace:") |> List.map (\key -> (String.dropLeft 10 key,False)) |> Dict.fromList
            in Activity.update (SessionActivityFetched request.id (Ok {workspaces=rows,projects=Dict.empty})) model |> Activity.reconcile model |> Tuple.first

context = {authMode="test",effectiveActor=Nothing,principal={actorType="user",actorId="actor",actorLabel="Actor",authority="grant",grantUserId=Just "user"},globalPermissions={createWorkspace=False,superadmin=False},workspace=Just {workspaceId="ws",role=Just "read",canRead=True,canEdit=False,canAdmin=False}}

home =
    let
        flags={apiUrl="http://test",wsUrl="ws://test",sessionId="test",runtimeMode="test",authTokenStorageKey="test",authTokenPresent=False,loginUrl=Nothing,logoutUrl=Nothing}
        url={protocol=Url.Http,host="test",port_=Nothing,path="/",query=Nothing,fragment=Nothing}
        base=AppShell.initModel Nothing url HomePage flags Nothing {tab=ProjectsTab,focus=Nothing,observationId=Nothing}
        groups=base.groups
        loading=base.dataLoading
        workspace n=let id="w" ++ String.fromInt n in (id,{id=id,name=id,workspaceType=Api.Repository,ghOwner=Nothing,ghRepo=Nothing,createdAt="",updatedAt=""})
    in {base | auth={status=AuthReady,mode=Just "test"},sessionContext=Just context,groups={groups | catalogueOwner=Just context,workspaceGroups=Dict.fromList (List.map (\id -> (id,{id=id,name=id,description=Nothing,createdAt="",updatedAt=""})) ["one","two"]),groupMembers=Dict.fromList [("one",["w1"]),("two",["w1"])]},dataLoading={loading | loadingWorkspaces=False},workspaces=List.range 1 57 |> List.map workspace |> Dict.fromList}
