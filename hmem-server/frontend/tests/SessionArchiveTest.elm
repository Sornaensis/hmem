module SessionArchiveTest exposing (suite)

import AppShell
import Feature.Sessions as Sessions
import Types exposing (..)
import Url
import Json.Encode as Encode
import Dict
import Expect
import Feature.SessionDelivery as Delivery
import Http
import Test exposing (describe, test)


suite =
    describe "sealed archive physical admission"
        [ test "archive transport shares the exact two physical slots and retirement cannot release them" <| \_ ->
            let
                queued=List.foldl (Delivery.demand "w") Delivery.init [Delivery.SessionArchiveHttp "s" 1,Delivery.TaskEvidenceArchive "task" 2,Delivery.SessionArchiveHttp "other" 3]
                (busy,requests)=Delivery.admit "w" queued
                retired=Delivery.retireAll busy
                afterRetirement=Delivery.admit "w" (Delivery.demand "w" (Delivery.SessionArchiveHttp "s" 4) retired) |> Tuple.first
                finished=List.head requests |> Maybe.map (\request -> Delivery.settle request.id False (Err Http.Timeout) afterRetirement) |> Maybe.withDefault afterRetirement
                duplicate=List.head requests |> Maybe.map (\request -> Delivery.settle request.id False (Err Http.Timeout) finished) |> Maybe.withDefault finished
            in
            Expect.equal [2,2,1,1] [List.length requests,Dict.size afterRetirement.physical,Dict.size finished.physical,Dict.size duplicate.physical]
        , test "success applies only to matching current archive target" <| \_ ->
            let
                target=Delivery.SessionArchiveHttp "s" 1
                (busy,requests)=Delivery.admit "w" (Delivery.demand "w" target Delivery.init)
                result=List.head requests |> Maybe.map (\request -> Delivery.settle request.id True (Ok Delivery.ArchiveDownloaded) busy) |> Maybe.withDefault busy
            in
            Expect.equal (0,Just (Just Delivery.ArchiveDownloaded)) (Dict.size result.physical,Dict.get (Delivery.key "w" target) result.resources |> Maybe.map .data)

        , test "accepted clear epoch retires model evidence while stale owners cannot reset it" <| \_ ->
            let
                ws="epoch-workspace"
                flags={apiUrl="http://test",wsUrl="ws://test",sessionId="test",runtimeMode="test",authTokenStorageKey="test",authTokenPresent=False,loginUrl=Nothing,logoutUrl=Nothing}
                url={protocol=Url.Http,host="test",port_=Nothing,path="/workspace/"++ws,query=Nothing,fragment=Just "tab=sessions&session=s"}
                base=AppShell.initModel Nothing url (WorkspacePage ws) flags Nothing {tab=SessionsTab,focus=Nothing,observationId=Nothing}
                state=base.sessions
                empty=emptySealedSessionView
                piece={key="piece",offset="0",kind="tool",label="secret",role="tool",time="",ast=Err "not_loaded",group="",count="0",open=True,groupOpen=False,body=Nothing}
                markdown=empty.markdown
                owned={base | selectedWorkspaceId=Just ws,auth={status=AuthReady,mode=Just "test"}
                    ,sessionContext=Just {authMode="test",effectiveActor=Nothing,principal={actorType="user",actorId="actor",actorLabel="Actor",authority="local",grantUserId=Nothing},globalPermissions={createWorkspace=False,superadmin=True},workspace=Just {workspaceId=ws,role=Just "read",canRead=True,canEdit=False,canAdmin=False}}
                    ,sessions={state | workspace=Just ws,selected=Just "s",stamp="owner",generation=7,inspectedEvent=Just "piece",rows=[SessionRecord 0 32 "secret" "\n" "conversation" "assistant" "" "secret" 0 Dict.empty],sealed={empty | domEpoch="old",rows=[piece],markdown={markdown | key="piece",source="secret source",complete=True}}}}
                event stamp generation epoch=SessionsBridgeEvent (Encode.object [("kind",Encode.string "sealed-clear"),("stamp",Encode.string stamp),("generation",Encode.int generation),("workspace",Encode.string ws),("session",Encode.string "s"),("domEpoch",Encode.string epoch)])
                cleared=Sessions.update (event "owner" 7 "new") owned |> Tuple.first
                stale=Sessions.update (event "old-owner" 7 "retired") cleared |> Tuple.first
                late=Sessions.update (event "owner" 6 "old") stale |> Tuple.first
            in
            Expect.equal {epoch="new",rows=0,rawRows=0,inspector=Nothing,source="",complete=False}
                {epoch=late.sessions.sealed.domEpoch,rows=List.length late.sessions.sealed.rows,rawRows=List.length late.sessions.rows,inspector=late.sessions.inspectedEvent,source=late.sessions.sealed.markdown.source,complete=late.sessions.sealed.markdown.complete}
        ]
