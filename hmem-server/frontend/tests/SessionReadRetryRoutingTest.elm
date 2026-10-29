module SessionReadRetryRoutingTest exposing (suite)

import Api
import AppShell
import Dict
import Expect
import Feature.SessionDelivery as Delivery
import Http
import Main
import Test exposing (describe, test)
import Types exposing (..)
import UpdateRouter
import Url


metadata = Delivery.SessionMetadata "session"
window = Delivery.SessionWindow "session" 32 96 False
selection = {workspace="a",session="session",lifetime="selection:1"}


context : Api.SessionContext
context =
    {authMode="test",effectiveActor=Nothing,principal={actorType="user",actorId="actor",actorLabel="Actor",authority="local",grantUserId=Nothing}
    ,globalPermissions={createWorkspace=False,superadmin=True},workspace=Just {workspaceId="a",role=Just "read",canRead=True,canEdit=False,canAdmin=False}}


model : Model
model =
    let
        flags={apiUrl="https://api.example",wsUrl="wss://api.example/ws",sessionId="test",runtimeMode="test",authTokenStorageKey="test",authTokenPresent=False,loginUrl=Nothing,logoutUrl=Nothing}
        url={protocol=Url.Https,host="app.example",port_=Nothing,path="/workspace/a",query=Nothing,fragment=Nothing}
        base=AppShell.initModel Nothing url (WorkspacePage "a") flags Nothing {tab=ProjectsTab,focus=Nothing,observationId=Nothing}
    in {base | auth={status=AuthReady,mode=Just "test"},selectedWorkspaceId=Just "a",sessionContext=Just context,sessionDelivery=Delivery.select (Just selection) Delivery.init}


sample : Api.AgentSession
sample =
    {id="session",workspaceId="a",formatLabel="jsonl",state=Api.SessionActive,creator={actorType=Nothing,actorId=Nothing,actorLabel=Nothing,botIdentity=Nothing}
    ,createdAt="2026-01-01T00:00:00Z",lastSeenAt="2026-01-01T00:00:00Z",committedBytes=96,committedRecords=1,revision=1,endsWithNewline=True
    ,terminatedAt=Nothing,finalBytes=Nothing,finalSha256=Nothing,title=Nothing,parentSessionId=Nothing,ancestors=[],ancestorsTruncated=False,runtimeIdentity=Nothing,runtimeRevision=0}


route msg state = UpdateRouter.update msg state |> Result.map Tuple.first |> Result.withDefault state
request target state = Dict.values state.sessionDelivery.physical |> List.filter (.target >> (==) target) |> List.head
entry target state = Dict.get (Delivery.key "a" target) state.sessionDelivery.resources


guard state owned =
    let target="session-resource:" ++ owned.key in
    {scopeKey="workspace:a",targetKey=target,targetGeneration=Dict.get ("workspace:a|" ++ target) state.webSocket.targetGenerations |> Maybe.withDefault -1
    ,sessionEpoch=state.sessionRequestEpoch,routeWorkspace=Just "a",audienceId="actor"}


complete target result state = request target state |> Maybe.map (\owned -> route (SessionResourceFetched owned.id (guard state owned) result) state) |> Maybe.withDefault state
ready = route (DemandSessionResource "a" metadata) model |> complete metadata (Ok (Delivery.Metadata sample))


waiting target source =
    let
        busy=route (DemandSessionResource "a" target) source
        failed=complete target (Err Http.Timeout) busy
        ticket=entry target failed |> Maybe.andThen (\resource -> case resource.retry of
            Delivery.Waiting value -> request target busy |> Maybe.map (\owned -> (value,guard busy owned))
            _ -> Nothing)
    in (failed,ticket)


fire ticket source = ticket |> Maybe.map (\(value,scope) -> route (SessionResourceRetry value scope) source) |> Maybe.withDefault source


suite = describe "production routed session retry lifetimes"
    [ test "routed same-key remove and re-add rejects the prior timer" <| \_ ->
        let
            (failed,ticket)=waiting metadata model
            readded=failed |> route (RemoveSessionResource "a" metadata) |> route (DemandSessionResource "a" metadata)
            fired=fire ticket readded
        in Expect.equal (True,readded.sessionDelivery)
            (ticket/=Nothing,fired.sessionDelivery)
    , test "routed changed range cannot attach the prior exact-target ticket" <| \_ ->
        let
            (failed,ticket)=waiting window ready
            changed=route (DemandSessionResource "a" (Delivery.SessionWindow "session" 64 96 False)) failed
            fired=fire ticket changed
        in Expect.equal changed.sessionDelivery fired.sessionDelivery
    , test "routed workspace navigation rejects a prior workspace timer" <| \_ ->
        let
            (failed,ticket)=waiting metadata model
            url=failed.url
            switched=Main.update (UrlChanged {url | path="/workspace/b"}) failed |> Tuple.first
            fired=fire ticket switched
        in Expect.equal (Just "b",switched.sessionDelivery.nextId,Nothing)
            (fired.selectedWorkspaceId,fired.sessionDelivery.nextId,entry metadata fired |> Maybe.andThen .data)
    , test "production logout retires a waiting timer and clears its private resources" <| \_ ->
        let
            (failed,ticket)=waiting metadata model
            loggedOut=Main.update LogoutRequested failed |> Tuple.first
            fired=fire ticket loggedOut
        in Expect.equal (AuthRequired,0,loggedOut.sessionDelivery.nextId)
            (fired.auth.status,Dict.size fired.sessionDelivery.resources,fired.sessionDelivery.nextId)
    , test "production principal change rejects an old audience timer" <| \_ ->
        let
            (failed,ticket)=waiting metadata model
            principal=context.principal
            changed=Main.update (GotSessionContext failed.sessionRequestEpoch (Just "a") (Ok {context | principal={principal | actorId="replacement"}})) failed |> Tuple.first
            fired=fire ticket changed
        in Expect.equal (Just "replacement",changed.sessionDelivery.nextId,Nothing)
            (fired.sessionContext |> Maybe.map (.principal >> .actorId),fired.sessionDelivery.nextId,entry metadata fired |> Maybe.andThen .data)
    , test "actual revoked authority through Main rejects a saved timer and stale response" <| \_ ->
        let
            (failed,ticket)=waiting metadata model
            other=Delivery.SessionMetadata "other"
            busy=route (DemandSessionResource "a" other) failed
            deniedContext={context | globalPermissions={createWorkspace=False,superadmin=False},workspace=Just {workspaceId="a",role=Nothing,canRead=False,canEdit=False,canAdmin=False}}
            denied=Main.update (GotSessionContext busy.sessionRequestEpoch (Just "a") (Ok deniedContext)) busy |> Tuple.first
            fired=fire ticket denied
            released=request other busy |> Maybe.map (\owned -> route (SessionResourceFetched owned.id (guard busy owned) (Ok (Delivery.Metadata {sample | id="other"}))) fired) |> Maybe.withDefault fired
        in Expect.equal {canRead=Just False,unchanged=denied.sessionDelivery,held=1,remaining=0,resources=0}
            {canRead=denied.sessionContext |> Maybe.andThen .workspace |> Maybe.map .canRead,unchanged=fired.sessionDelivery,held=Dict.size denied.sessionDelivery.physical,remaining=Dict.size released.sessionDelivery.physical,resources=Dict.size released.sessionDelivery.resources}
    , test "routed logout keeps outstanding physical occupancy until the stale callback" <| \_ ->
        let
            busy=route (DemandSessionResource "a" metadata) model
            loggedOut=Main.update LogoutRequested busy |> Tuple.first
            released=request metadata busy |> Maybe.map (\owned -> route (SessionResourceFetched owned.id (guard busy owned) (Ok (Delivery.Metadata sample))) loggedOut) |> Maybe.withDefault loggedOut
        in Expect.equal (1,0,0) (Dict.size loggedOut.sessionDelivery.physical,Dict.size released.sessionDelivery.physical,Dict.size released.sessionDelivery.resources)
    , test "routed range409 retires read proof and rejects an older waiting window ticket" <| \_ ->
        let
            (failed,ticket)=waiting window ready
            conflict=Delivery.SessionBytes "session" 0 False
            refreshed=route (DemandSessionResource "a" conflict) failed |> complete conflict (Err (Http.BadStatus 409))
            fired=fire ticket refreshed
            oldIncarnation=ticket |> Maybe.map (Tuple.first >> .request >> .incarnation)
        in Expect.equal (True,True,refreshed.sessionDelivery)
            (refreshed.sessionDelivery.proofRetirement>failed.sessionDelivery.proofRetirement,oldIncarnation/=(entry window refreshed |> Maybe.map .incarnation),fired.sessionDelivery)
    , test "routed401 and403 retire resources without creating a delayed retry ticket" <| \_ ->
        let
            busy=route (DemandSessionResource "a" metadata) model
            failed status=complete metadata (Err (Http.BadStatus status)) busy
            receipt state=(Dict.size state.sessionDelivery.resources,Dict.size state.sessionDelivery.physical,state.sessionDelivery.retirement>busy.sessionDelivery.retirement)
        in Expect.equal [(0,0,True),(0,0,True)] (List.map (failed >> receipt) [401,403])
    , test "routed background metadata failure never becomes a selected retry" <| \_ ->
        let
            background=Delivery.SessionMetadata "background"
            busy=route (DemandSessionResource "a" background) model
            failed=complete background (Err Http.NetworkError) busy
        in Expect.equal (Just Delivery.Spent,Just "refresh_failed",0)
            (entry background failed |> Maybe.map .retry,entry background failed |> Maybe.andThen .error,Dict.size failed.sessionDelivery.physical)
    , test "routed list, facet, links, download and archive failures never arm retry tickets" <| \_ ->
        let
            targets=[Delivery.WorkspaceSessions Nothing,Delivery.SessionFacets "projects" Nothing Nothing Nothing Nothing,Delivery.SessionTasks "session" Nothing,Delivery.SessionSave "session" 0 32,Delivery.TaskEvidenceArchive "task" 1]
            check target =
                let busy=route (DemandSessionResource "a" target) ready
                    failed=complete target (Err Http.Timeout) busy
                in entry target failed |> Maybe.map (\resource -> resource.retry==Delivery.Spent && resource.error/=Nothing) |> Maybe.withDefault False
        in Expect.equal (List.repeat 5 True) (List.map check targets)
    , test "routed selected retry queues behind occupied slots while background service proceeds" <| \_ ->
        let
            (failed,ticket)=waiting metadata model
            first=Delivery.SessionMetadata "other-1"
            second=Delivery.SessionMetadata "other-2"
            background=Delivery.WorkspaceSessions Nothing
            occupied=failed |> route (DemandSessionResource "a" first) |> route (DemandSessionResource "a" second) |> route (DemandSessionResource "a" background)
            delivery=occupied.sessionDelivery
            fair={occupied | sessionDelivery={delivery | foregroundStreak=3}}
            fired=fire ticket fair
            released=complete first (Err (Http.BadStatus 404)) fired
        in Expect.equal ((2,2),(True,False))
            ((Dict.size fired.sessionDelivery.physical,Dict.size released.sessionDelivery.physical),(request background released/=Nothing,request metadata released/=Nothing))
    ]
