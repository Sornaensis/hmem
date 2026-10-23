module SessionsTest exposing (suite)

import Api
import AppShell
import Dict
import Expect
import Feature.Sessions as Sessions
import Feature.SessionDelivery as Delivery
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (describe, test)
import Types exposing (..)
import Url


suite : Test.Test
suite =
    describe "Sessions production wire contract"
        [ test "decodes canonical HTTP metadata with omitted optional fields and final page cursor" <| \_ ->
            case Decode.decodeString Api.sessionPageDecoder canonicalPage of
                Ok page ->
                    Expect.equal ( Nothing, Just ( Nothing, Nothing, Nothing ) )
                        ( page.nextCursor, List.head page.items |> Maybe.map (\session -> (session.creator.botIdentity,session.terminatedAt,session.finalSha256)) )
                Err error -> Expect.fail (Decode.errorToString error)
        , test "present malformed optional fields remain errors" <| \_ ->
            case Decode.decodeString Api.sessionPageDecoder (String.replace "\"state\":\"active\"" "\"state\":\"active\",\"final_bytes\":\"invalid\"" canonicalPage) of
                Err _ -> Expect.pass
                Ok _ -> Expect.fail "Malformed optional receipt accepted"
        , test "explicit null is accepted for optional attribution, receipts and final cursor" <| \_ ->
            let
                fields = "\"creator_bot_identity\":null,\"terminated_at\":null,\"final_bytes\":null,\"final_sha256\":null,"
                wire = canonicalPage |> String.replace "\"state\":\"active\"" (fields ++ "\"state\":\"active\"") |> String.replace "{\"items\":" "{\"next_cursor\":null,\"items\":"
            in
            Decode.decodeString Api.sessionPageDecoder wire |> Result.map (always True) |> Result.withDefault False |> Expect.equal True
        , test "every optional field rejects a present wrong type instead of falling back to absence" <| \_ ->
            let
                fields = ["creator_actor_type","creator_actor_id","creator_actor_label","creator_bot_identity","terminated_at","final_bytes","final_sha256"]
                malformed field = canonicalPage |> String.replace "\"state\":\"active\"" ("\"" ++ field ++ "\":false,\"state\":\"active\"")
                withoutAttribution = canonicalPage |> String.replace "\"creator_actor_type\":\"user\"," "" |> String.replace "\"creator_actor_id\":\"43db6893-cc92-4daa-bcf9-51e13c444501\"," "" |> String.replace "\"creator_actor_label\":\"Sandbox Superadmin\"," ""
                invalid wire = case Decode.decodeString Api.sessionPageDecoder wire of
                    Err _ -> True
                    Ok _ -> False
            in
            Expect.equal (True,True,True)
                ( List.all (malformed >> invalid) fields
                , invalid (String.replace "{\"items\":" "{\"next_cursor\":false,\"items\":" canonicalPage)
                , Decode.decodeString Api.sessionPageDecoder withoutAttribution |> Result.map (always True) |> Result.withDefault False )
        , test "canonical resync retires pending work without resuming a paused viewer or moving its anchor" <| \_ ->
            let
                entered = Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                previous = entered.sessions
                paused = {entered | sessions = {previous | following = False,loadWindow = False,start = Just 0,offset = 32,end = 32,rows = [sampleRecord]}}
                resync = Sessions.reconcile ({paused | sessionDelivery = Delivery.invalidateWorkspace workspace paused.sessionDelivery},Cmd.none) |> Tuple.first
            in
            Expect.equal {following=False,offset=32,rows=[sampleRecord],physical=2}
                {following=resync.sessions.following,offset=resync.sessions.offset,rows=resync.sessions.rows,physical=Dict.size resync.sessionDelivery.physical}
        , test "viewer decode-error retry retires the exact window and resets the bridge without releasing physical requests" <| \_ ->
            let
                entered = Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                previous = entered.sessions
                target = Delivery.SessionWindow sessionId 0 32 False
                demanded = Delivery.demand workspace target entered.sessionDelivery
                reading = {entered | sessions = {previous | start=Just 0,offset=0,end=32,pending=True,owned=(workspace,target)::previous.owned},sessionDelivery=demanded}
                failed = Sessions.update (SessionsBridgeEvent (Encode.object [("kind",Encode.string "viewer-error"),("stamp",Encode.string previous.stamp),("generation",Encode.int previous.generation)])) reading |> Tuple.first
                retried = Sessions.update SessionsRetry failed |> Tuple.first
            in
            Expect.equal {removed=True,physical=Dict.size demanded.physical,retirement=demanded.retirement,offset=0,reset=Just 0}
                {removed=not (Dict.member (Delivery.key workspace target) retried.sessionDelivery.resources),physical=Dict.size retried.sessionDelivery.physical,retirement=retried.sessionDelivery.retirement,offset=retried.sessions.offset,reset=Sessions.viewerReset retried.sessions}
        , test "older window ends at the first retained record boundary even after row eviction" <| \_ ->
            let
                previous=emptySessionsModel
                row={sampleRecord | offset=3000000,end=3000032}
                state={previous | start=Just 2000000,offset=4097152,end=4097152,rows=[row]}
            in
            Expect.equal (902848,3000000) (Sessions.olderWindow state)
        , test "late viewer callbacks after a same-context snapshot cannot replace the paused rows" <| \_ ->
            let
                entered = Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                previous = entered.sessions
                paused = {entered | sessions={previous | following=False,start=Just 0,offset=32,end=64,rows=[sampleRecord],pending=True}}
                resync = Sessions.reconcile ({paused | sessionDelivery=Delivery.invalidateWorkspace workspace paused.sessionDelivery},Cmd.none) |> Tuple.first
                oldEvent = Encode.object [("kind",Encode.string "viewer"),("stamp",Encode.string previous.stamp),("generation",Encode.int previous.generation),("through",Encode.int 64),("rows",Encode.list identity [])]
                late = Sessions.update (SessionsBridgeEvent oldEvent) resync |> Tuple.first
            in
            Expect.equal (32,[sampleRecord],False) (late.sessions.offset,late.sessions.rows,late.sessions.following)
        , test "bounded viewer and save purposes settle one chunk and await explicit consumption and rearm" <| \_ ->
            case Decode.decodeString Api.sessionPageDecoder canonicalPage |> Result.toMaybe |> Maybe.andThen (.items >> List.head) of
                Nothing -> Expect.fail "Canonical fixture did not decode"
                Just metadata ->
                    let
                        session = {metadata | committedBytes=6,state=Api.SessionTerminated,finalBytes=Just 6,finalSha256=Just (String.repeat 64 "a")}
                        window = Delivery.SessionWindow sessionId 0 3 True
                        save = Delivery.SessionSave sessionId 0 3
                        (waiting,details) = Delivery.demand workspace window Delivery.init |> Delivery.demand workspace save |> Delivery.admit workspace
                        prepared = List.foldl (\request state -> Delivery.settle request.id True (Ok (Delivery.Metadata session)) state) waiting details
                        (busy,requests) = Delivery.admit workspace prepared
                        done = List.foldl (\request state -> Delivery.settle request.id True (Ok (Delivery.Bytes {session=session,offset=0,nextOffset=3,dataBase64="e30K",hasMore=True})) state) busy requests
                        (idle,automatic) = Delivery.admit workspace done
                        consumed = Delivery.consume workspace window idle |> Delivery.consume workspace save
                        (rearmed,next) = Delivery.demand workspace (Delivery.SessionSave sessionId 3 3) consumed |> Delivery.admit workspace
                    in
                    Expect.equal {purposes=2,automatic=0,next=[Delivery.SessionSave sessionId 3 3],slots=1,cleared=True}
                        {purposes=List.length requests,automatic=List.length automatic,next=List.map .target next,slots=Dict.size rearmed.physical
                        ,cleared=Dict.values consumed.resources |> List.filter (\resource -> resource.target==window || resource.target==save) |> List.all (\resource -> resource.data==Nothing && List.isEmpty resource.ranges)}
        , test "filter changes retire old cursor demand and a hidden or unauthorized tab cannot restore demand" <| \_ ->
            let
                entered = Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                oldTarget = Delivery.WorkspaceSessionsFiltered Api.defaultSessionFilters Nothing
                filtered = Sessions.update (SessionsStateFilter "terminated") entered |> (\pair -> Sessions.reconcile pair) |> Tuple.first
                hidden = Sessions.reconcile ({filtered | activeTab=ProjectsTab},Cmd.none) |> Tuple.first
                denied = Sessions.reconcile ({filtered | auth={status=AuthRequired,mode=Nothing}},Cmd.none) |> Tuple.first
            in
            Expect.equal {old=False,filter="terminated",hidden=0,denied=0,physical=2}
                {old=Dict.member (Delivery.key workspace oldTarget) filtered.sessionDelivery.resources,filter=filtered.sessions.filters.state
                ,hidden=Dict.size hidden.sessionDelivery.resources,denied=Dict.size denied.sessionDelivery.resources,physical=Dict.size denied.sessionDelivery.physical}
        , test "late physical callback from an old filter releases only its slot and cannot install that page" <| \_ ->
            let
                oldTarget=Delivery.WorkspaceSessionsFiltered Api.defaultSessionFilters Nothing
                (busy,requests)=Delivery.demand workspace oldTarget Delivery.init |> Delivery.admit workspace
                newTarget=Delivery.WorkspaceSessionsFiltered {state="terminated",freshness=""} Nothing
                changed=Delivery.removeDemand workspace oldTarget busy |> Delivery.demand workspace newTarget
                completed=List.foldl (\request state -> Delivery.settle request.id True (Ok (Delivery.SessionsPage {items=[],nextCursor=Nothing})) state) changed requests
                (_,next)=Delivery.admit workspace completed
            in
            Expect.equal (0,[newTarget],Nothing)
                (Dict.size completed.physical,List.map .target next,Dict.get (Delivery.key workspace newTarget) completed.resources |> Maybe.andThen .data)
        ]


workspace : String
workspace = "f96e817f-9981-4178-8611-37a4e42b0896"


sessionId : String
sessionId = "4eccfa7f-e68c-41fe-8598-eb1533152190"


sampleRecord : SessionRecord
sampleRecord = SessionRecord 0 32 "{}" "\r\n" "unknown" "" "" "{}"


sessionModel : Model
sessionModel =
    let
        flags = {apiUrl="http://test",wsUrl="ws://test",sessionId="test",runtimeMode="test",authTokenStorageKey="test",authTokenPresent=False,loginUrl=Nothing,logoutUrl=Nothing}
        url = {protocol=Url.Http,host="test",port_=Nothing,path="/workspace/" ++ workspace,query=Nothing,fragment=Just "tab=sessions&session=4eccfa7f-e68c-41fe-8598-eb1533152190"}
        base = AppShell.initModel Nothing url (WorkspacePage workspace) flags Nothing {tab=SessionsTab,focus=Nothing,observationId=Nothing}
    in
    {base | selectedWorkspaceId = Just workspace,auth={status=AuthReady,mode=Just "test"},sessionRequestEpoch=1
        ,sessionContext=Just {authMode="test",effectiveActor=Nothing,principal={actorType="user",actorId="actor",actorLabel="Actor",authority="local",grantUserId=Nothing},globalPermissions={createWorkspace=False,superadmin=True},workspace=Just {workspaceId=workspace,role=Just "read",canRead=True,canEdit=False,canAdmin=False}}}


-- Canonical deployed HTTP shape from the real-backend browser fixture. The
-- server intentionally omits Nothing values rather than emitting JSON null.
canonicalPage : String
canonicalPage =
    """{"items":[{"committed_bytes":92,"committed_records":2,"created_at":"2026-10-23T20:46:19.453875Z","creator_actor_id":"43db6893-cc92-4daa-bcf9-51e13c444501","creator_actor_label":"Sandbox Superadmin","creator_actor_type":"user","ends_with_newline":true,"format_label":"Provider-neutral JSONL","id":"4eccfa7f-e68c-41fe-8598-eb1533152190","last_seen_at":"2026-10-23T20:46:19.470361Z","revision":1,"state":"active","workspace_id":"f96e817f-9981-4178-8611-37a4e42b0896"}]}"""
