module SessionsTest exposing (suite)

import Api
import AppShell
import Dict
import Expect
import Feature.Sessions as Sessions
import Feature.SessionDelivery as Delivery
import Helpers
import Http
import Set
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (describe, test)
import Types exposing (..)
import Url


suite : Test.Test
suite =
    describe "Sessions production wire contract"
        [ describe "card-owned dock authority" dockTests
        , describe "same-authority metadata presentation" metadataPresentationTests
        , test "session and authority changes restore the compact linked-task default" <| \_ ->
            let
                entered=Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                state=entered.sessions
                expanded={entered | sessions={state | showAllLinkedTasks=True}}
                epoch=Sessions.reconcile ({expanded | sessionRequestEpoch=expanded.sessionRequestEpoch+1},Cmd.none) |> Tuple.first
                url=expanded.url
                selected=Sessions.reconcile ({expanded | url={url | fragment=Just "tab=sessions&session=aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa"}},Cmd.none) |> Tuple.first
            in Expect.equal (False,False) (epoch.sessions.showAllLinkedTasks,selected.sessions.showAllLinkedTasks)
        , test "decodes canonical HTTP metadata with omitted optional fields and final page cursor" <| \_ ->
            case Decode.decodeString Api.sessionPageDecoder canonicalPage of
                Ok page ->
                    Expect.equal ( Nothing, Just ( Nothing, Nothing, Nothing ) )
                        ( page.nextCursor, List.head page.items |> Maybe.map (\session -> (session.creator.botIdentity,session.terminatedAt,session.finalSha256)) )
                Err error -> Expect.fail (Decode.errorToString error)
        , test "runtime is unknown without a declaration and model-only runtime stays separate from caller" <| \_ ->
            let
                absent=Decode.decodeString Api.sessionPageDecoder canonicalPage |> Result.toMaybe |> Maybe.andThen (.items >> List.head)
                explicit=canonicalPage |> String.replace "\"state\":\"active\"" "\"state\":\"active\",\"runtime_identity\":{\"model\":\"Actual target\"},\"runtime_revision\":3"
                known=Decode.decodeString Api.sessionPageDecoder explicit |> Result.toMaybe |> Maybe.andThen (.items >> List.head)
            in
            Expect.equal ((Just Nothing,Just 0),(Just (Just {model="Actual target",provider=Nothing,reasoning=Nothing}),Just 3,Just (Just "Sandbox Superadmin")))
                ((Maybe.map .runtimeIdentity absent,Maybe.map .runtimeRevision absent),(Maybe.map .runtimeIdentity known,Maybe.map .runtimeRevision known,Maybe.map (.creator >> .actorLabel) known))
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
                fields = ["creator_actor_type","creator_actor_id","creator_actor_label","creator_bot_identity","terminated_at","final_bytes","final_sha256","runtime_identity","runtime_revision"]
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
        , test "decode Retry bounds recovery near the current finite end and retains displayed rows" <| \_ ->
            let
                entered = Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                old = entered.sessions
                failed = {entered | sessions={old | start=Just 0,offset=8388608,end=8388672,error=Just "decode",rows=[sampleRecord],resetPending=False}}
                retried = Sessions.update SessionsRetry failed |> Tuple.first
            in
            Expect.equal {start=Just 6291520,offset=6291520,end=8388672,rows=[sampleRecord],reset=Just 6291520}
                {start=retried.sessions.start,offset=retried.sessions.offset,end=retried.sessions.end,rows=retried.sessions.rows,reset=Sessions.viewerReset retried.sessions}
        , test "transport Retry keeps the exact failed cursor without resetting presentation" <| \_ ->
            let
                entered = Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                old = entered.sessions
                failed = {entered | sessions={old | start=Just 0,offset=8388608,end=8388672,rows=[sampleRecord],pending=True,resetPending=False}}
                retried = Sessions.update SessionsRetry failed |> Tuple.first
            in
            Expect.equal (8388608,[sampleRecord],Nothing)
                (retried.sessions.offset,retried.sessions.rows,Sessions.viewerReset retried.sessions)
        , test "intermediate framing acknowledgements advance one chunk without replacing displayed rows" <| \_ ->
            let
                entered = Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                old = entered.sessions
                reading = {entered | sessions={old | start=Just 0,offset=0,end=64,rows=[sampleRecord],pending=True,resetPending=False}}
                event = SessionsBridgeEvent (Encode.object [("kind",Encode.string "viewer-progress"),("stamp",Encode.string old.stamp),("generation",Encode.int old.generation),("through",Encode.int 32)])
                advanced = Sessions.update event reading |> Tuple.first
                retired = Sessions.reconcile ({reading | sessionDelivery=Delivery.invalidateWorkspace workspace reading.sessionDelivery},Cmd.none) |> Tuple.first
                late = Sessions.update event retired |> Tuple.first
            in
            Expect.equal ((32,[sampleRecord],False),(0,[sampleRecord],False))
                ((advanced.sessions.offset,advanced.sessions.rows,advanced.sessions.pending),(late.sessions.offset,late.sessions.rows,late.sessions.pending))
        , test "malformed or unowned intermediate progress never advances the acknowledged cursor" <| \_ ->
            let
                entered = Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                old = entered.sessions
                reading = {entered | sessions={old | start=Just 0,offset=32,end=64,rows=[sampleRecord],pending=True,resetPending=False}}
                event stamp generation through = SessionsBridgeEvent (Encode.object [("kind",Encode.string "viewer-progress"),("stamp",Encode.string stamp),("generation",Encode.int generation),("through",through)])
                rejected = [ event old.stamp old.generation (Encode.int 32),event old.stamp old.generation (Encode.int 65),event old.stamp old.generation (Encode.string "64"),event "other" old.generation (Encode.int 64),event old.stamp (old.generation+1) (Encode.int 64) ]
                    |> List.map (\message -> Sessions.update message reading |> Tuple.first |> .sessions |> (\state -> (state.offset,state.rows)))
            in
            Expect.equal (List.repeat 5 (32,[sampleRecord])) rejected
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
        , test "following resync preserves presentation while retiring only transport callbacks" <| \_ ->
            let
                entered = Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                previous = entered.sessions
                reading = { entered | sessions = { previous | start = Just 0, offset = 32, end = 64, rows = [sampleRecord], pending = True, resetPending = False } }
                resync = Sessions.reconcile ({ reading | sessionDelivery = Delivery.invalidateWorkspace workspace reading.sessionDelivery },Cmd.none) |> Tuple.first
                oldEvent = Encode.object [("kind",Encode.string "viewer"),("stamp",Encode.string previous.stamp),("generation",Encode.int previous.generation),("through",Encode.int 64),("rows",Encode.list identity [])]
                late = Sessions.update (SessionsBridgeEvent oldEvent) resync |> Tuple.first
            in
            Expect.equal {stamp=previous.stamp,offset=32,end=64,rows=[sampleRecord],reset=Nothing,retired=True}
                {stamp=late.sessions.stamp,offset=late.sessions.offset,end=late.sessions.end,rows=late.sessions.rows,reset=Sessions.viewerReset late.sessions,retired=late.sessions.generation>previous.generation}
        , test "unacknowledged finite tail keeps its exact end when resync metadata grows" <| \_ ->
            case Decode.decodeString Api.sessionPageDecoder canonicalPage |> Result.toMaybe |> Maybe.andThen (.items >> List.head) of
                Nothing -> Expect.fail "Session fixture did not decode"
                Just metadata ->
                    let
                        entered = Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                        old = entered.sessions
                        reading = {entered | sessions={old | start=Just 0,offset=32,end=64,rows=[sampleRecord],pending=True,resetPending=False}}
                        resync = Sessions.reconcile ({reading | sessionDelivery=Delivery.invalidateWorkspace workspace reading.sessionDelivery},Cmd.none) |> Tuple.first
                        delivery = resync.sessionDelivery
                        settled = {delivery | resources=Dict.update (Delivery.key workspace (Delivery.SessionMetadata sessionId)) (Maybe.map (\entry -> {entry | data=Just (Delivery.Metadata {metadata | committedBytes=128}),settled=entry.dirty})) delivery.resources}
                        renewed = Sessions.reconcile ({resync | sessionDelivery=settled},Cmd.none) |> Tuple.first
                    in
                    Expect.equal (32,64,Nothing)
                        (renewed.sessions.offset,renewed.sessions.end,Sessions.viewerReset renewed.sessions)
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
                filtered = Sessions.update (SessionsFreshnessFilter "stale") entered |> (\pair -> Sessions.reconcile pair) |> Tuple.first
                hidden = Sessions.reconcile ({filtered | activeTab=ProjectsTab},Cmd.none) |> Tuple.first
                denied = Sessions.reconcile ({filtered | auth={status=AuthRequired,mode=Nothing}},Cmd.none) |> Tuple.first
            in
            Expect.equal {old=False,filter="stale",hidden=0,denied=0,physical=2}
                {old=Dict.member (Delivery.key workspace oldTarget) filtered.sessionDelivery.resources,filter=filtered.sessions.filters.freshness
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
        , test "rotating mounted card pages converge beyond the resource cap and clear at authority retirement" <| \_ ->
            let
                ids = List.range 1 22 |> List.map (\n -> "task-" ++ String.fromInt n)
                base = sessionModel
                loading = base.dataLoading
                cards = base.cards
                model = {base | activeTab=ProjectsTab,dataLoading={loading | visibleDetailDemand=Just (Set.empty,Set.fromList ids)},cards={cards | expandedCards=Dict.fromList (List.map (\id -> (id,True)) ids)}}
                advance _ (current,withinBounds) =
                    let
                        requested = Sessions.reconcile (current,Cmd.none) |> Tuple.first
                        requests = Dict.values requested.sessionDelivery.physical
                        settled = List.foldl (\request delivery -> Delivery.settle request.id True (Ok (Delivery.SessionsPage {items=[],nextCursor=Nothing})) delivery) requested.sessionDelivery requests
                    in
                    ({requested | sessionDelivery=settled},withinBounds && Dict.size requested.sessionDelivery.resources <= 16 && List.length requests <= 2)
                (ready,bounded) = List.foldl advance (model,True) (List.range 1 100)
                retired = Sessions.reconcile ({ready | auth={status=AuthRequired,mode=Nothing}},Cmd.none) |> Tuple.first
            in
            Expect.equal {pages=22,hints=22,bounded=True,retiredPages=0,retiredHints=0}
                {pages=Dict.size ready.sessions.taskPages,hints=Dict.size ready.sessions.taskHints,bounded=bounded,retiredPages=Dict.size retired.sessions.taskPages,retiredHints=Dict.size retired.sessions.taskHints}
        , test "active and evidence cursors remain independent between tasks" <| \_ ->
            let
                cursor={createdAt="2026-10-24T00:00:00Z",id=sessionId}
                advanced=Sessions.update (SessionsTaskNextPage "active:one" cursor) sessionModel |> Tuple.first
                next=Sessions.update (SessionsTaskNextPage "terminated:two" cursor) advanced |> Tuple.first
                back=Sessions.update (SessionsTaskPreviousPage "active:one") next |> Tuple.first
            in
            Expect.equal (Just Nothing,Just (Just cursor),Nothing)
                (Dict.get "active:one" back.sessions.taskCursors,Dict.get "terminated:two" back.sessions.taskCursors,Dict.get "active:two" back.sessions.taskCursors)
        , test "versioned history permits Sessions and preserves task/session context" <| \_ ->
            let
                base=sessionModel
                state=base.sessions
                scoped={base | url=let url=base.url in {url | fragment=Just ("tab=sessions&task=" ++ workspace ++ "&session=" ++ sessionId)},sessions={state | taskFilter=Just workspace}}
                target=Helpers.observationHistoryUrl scoped
                context=Url.fromString target |> Maybe.map Helpers.observationUrlContext
            in
            Expect.equal {tab=Just SessionsTab,notice=Just Nothing,task=True,session=True}
                {tab=Maybe.map (.fragment >> .tab) context,notice=Maybe.map .notice context,task=String.contains ("task=" ++ workspace) target,session=String.contains ("session=" ++ sessionId) target}
        , test "paired metadata failure retires a card download without trapping its save reservation" <| \_ ->
            let
                entered=Sessions.reconcile ({sessionModel | activeTab=ProjectsTab},Cmd.none) |> Tuple.first
                state=entered.sessions
                operation=SessionSaveOperation 1 sessionId 5 (String.repeat 64 "a") 1 0 "writing" False
                downloading=Sessions.reconcile ({entered | sessions={state | selected=Nothing,save=Just operation,nextOperation=2},activeTab=ProjectsTab},Cmd.none) |> Tuple.first
                (failed,_) = List.foldl (\request (delivery,ignored) -> if request.target==Delivery.SessionMetadata sessionId then (Delivery.settle request.id True (Err (Http.BadBody "metadata failed")) delivery,ignored) else (delivery,ignored)) (downloading.sessionDelivery,()) (Dict.values downloading.sessionDelivery.physical)
                rejected=Sessions.reconcile ({downloading | sessionDelivery=failed},Cmd.none) |> Tuple.first
            in
            Expect.equal (Nothing,"Download could not finish. Please retry.") (rejected.sessions.save,rejected.sessions.saveMessage)
        , test "zero-byte download emits its pinned final receipt after card caches are unmounted" <| \_ ->
            let
                entered=Sessions.reconcile ({sessionModel | activeTab=ProjectsTab},Cmd.none) |> Tuple.first
                state=entered.sessions
                operation=SessionSaveOperation 1 sessionId 0 "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" 1 0 "writing" False
                ready=Sessions.reconcile ({entered | activeTab=ProjectsTab,sessions={state | selected=Nothing,save=Just operation,nextOperation=2,taskPages=Dict.empty},sessionDelivery=Delivery.init},Cmd.none) |> Tuple.first
            in
            Expect.equal (Just True,0) (ready.sessions.save |> Maybe.map .pending,Dict.size ready.sessions.taskPages)
        , test "binary archive cancellation holds its physical slot until the exact native callback" <| \_ ->
            let
                entered=Sessions.reconcile ({sessionModel | activeTab=ProjectsTab},Cmd.none) |> Tuple.first
                state=entered.sessions
                archive=SessionArchiveOperation 1 workspace "Task evidence" "waiting"
                downloading=Sessions.reconcile ({entered | sessions={state | archive=Just archive,nextOperation=2}},Cmd.none) |> Tuple.first
                request=Dict.values downloading.sessionDelivery.physical |> List.head
                cancelled=Sessions.update SessionsCancelSave downloading |> Sessions.reconcile |> Tuple.first
                completed=request |> Maybe.map (\owned -> Sessions.update (SessionsBridgeEvent (Encode.object [("kind",Encode.string "archive-http"),("stamp",Encode.string state.downloadStamp),("operation",Encode.int 1),("request",Encode.int owned.id),("status",Encode.int 0)])) cancelled |> Tuple.first) |> Maybe.withDefault cancelled
            in
            Expect.equal (1,1,0) (Dict.size downloading.sessionDelivery.physical,Dict.size cancelled.sessionDelivery.physical,Dict.size completed.sessionDelivery.physical)
        , test "binary401 retires current authority while stale-token errors release only their old slot" <| \_ ->
            let
                entered=Sessions.reconcile ({sessionModel | activeTab=ProjectsTab},Cmd.none) |> Tuple.first
                state=entered.sessions
                downloading=Sessions.reconcile ({entered | sessions={state | archive=Just (SessionArchiveOperation 1 workspace "Task" "waiting"),nextOperation=2}},Cmd.none) |> Tuple.first
                id=Dict.keys downloading.sessionDelivery.physical |> List.head |> Maybe.withDefault -1
                event=SessionsBridgeEvent (Encode.object [("kind",Encode.string "archive-http"),("stamp",Encode.string state.downloadStamp),("operation",Encode.int 1),("request",Encode.int id),("status",Encode.int 401)])
                current=Sessions.update event downloading |> Tuple.first
                stale=Sessions.update event {downloading | sessionRequestEpoch=downloading.sessionRequestEpoch+1} |> Tuple.first
            in
            Expect.equal (0,AuthReady,0) (Dict.size current.sessionDelivery.resources,stale.auth.status,Dict.size stale.sessionDelivery.physical)
        , test "late archive cleanup failure after cancellation blocks reuse globally" <| \_ ->
            let
                entered=Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                state=entered.sessions
                cancelled={entered | sessions={state | archive=Nothing,save=Nothing,nextOperation=2}}
                failed=Sessions.update (SessionsBridgeEvent (Encode.object [("kind",Encode.string "archive-error"),("stamp",Encode.string state.downloadStamp),("operation",Encode.int 1),("code",Encode.string "writer_cleanup_failed")])) cancelled |> Tuple.first
            in
            Expect.equal True failed.sessions.saveBlocked
        ]


workspace : String
workspace = "f96e817f-9981-4178-8611-37a4e42b0896"


sessionId : String
sessionId = "4eccfa7f-e68c-41fe-8598-eb1533152190"


sampleRecord : SessionRecord
sampleRecord = SessionRecord 0 32 "{}" "\r\n" "unknown" "" "" "{}" 0 Dict.empty


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


metadataPresentationTests : List Test.Test
metadataPresentationTests =
    let
        metadata = Decode.decodeString Api.sessionPageDecoder canonicalPage |> Result.toMaybe |> Maybe.andThen (.items >> List.head)
        withMetadata check = \_ -> case metadata of
            Just session -> check session
            Nothing -> Expect.fail "Invalid canonical metadata fixture"
        listTarget = Delivery.WorkspaceSessions Nothing
        detailTarget = Delivery.SessionMetadata sessionId
        windowTarget = Delivery.SessionWindow sessionId 0 32 False
        legacyTarget = Delivery.SessionBytes sessionId 0 False
        entry target value = {workspace=workspace,target=target,dirty=0,settled=0,data=Just value,ranges=[],error=Nothing}
        ready session =
            let initial=Delivery.init
                range={session=session,offset=0,nextOffset=3,dataBase64="e30K",hasMore=True}
                legacy=entry legacyTarget (Delivery.Bytes range)
                resources=[entry detailTarget (Delivery.Metadata session),entry listTarget (Delivery.SessionsPage {items=[session],nextCursor=Nothing}),entry windowTarget (Delivery.Bytes range),{legacy | ranges=[range]}]
            in {initial | resources=Dict.fromList (List.map (\resource -> (Delivery.key workspace resource.target,resource)) resources),order=List.map (.target >> Delivery.key workspace) resources}
        payload target state = Dict.get (Delivery.key workspace target) state.resources |> Maybe.andThen .data
        requestFor target requests = List.filter (.target >> (==) target) requests |> List.head
    in
    [ test "canonical refresh retains bounded metadata display but clears bytes and waits for fresh read authority" <| withMetadata (\session ->
        let refreshed=Delivery.invalidateWorkspace workspace (ready session)
            (_,requests)=Delivery.admit workspace refreshed
            global=Delivery.invalidateAll (ready session)
        in Expect.equal {detail=Just (Delivery.Metadata session),page=Just (Delivery.SessionsPage {items=[session],nextCursor=Nothing}),bytes=Nothing,admitted=False,global=Just (Delivery.Metadata session),legacyRanges=Just [],globalRanges=Just []}
            {detail=payload detailTarget refreshed,page=payload listTarget refreshed,bytes=payload windowTarget refreshed,admitted=List.any (.target >> (==) windowTarget) requests,global=payload detailTarget global,legacyRanges=Dict.get (Delivery.key workspace legacyTarget) refreshed.resources |> Maybe.map .ranges,globalRanges=Dict.get (Delivery.key workspace legacyTarget) global.resources |> Maybe.map .ranges})
    , test "owned refresh errors clear stale display without admitting paired byte reads" <| withMetadata (\session ->
        let (busy,requests)=Delivery.invalidateWorkspace workspace (ready session) |> Delivery.admit workspace
            failed=requestFor detailTarget requests |> Maybe.map (\request -> Delivery.settle request.id True (Err (Http.BadStatus 404)) busy) |> Maybe.withDefault busy
            (_,next)=Delivery.admit workspace failed
        in Expect.equal (Nothing,Just (Just "refresh_failed"),False)
            (payload detailTarget failed,Dict.get (Delivery.key workspace detailTarget) failed.resources |> Maybe.map .error,List.any (.target >> (==) windowTarget) next))
    , test "a retired response error cannot clear retained metadata or release another request" <| withMetadata (\session ->
        let (busy,requests)=Delivery.invalidateWorkspace workspace (ready session) |> Delivery.admit workspace
            newer=Delivery.invalidateWorkspace workspace busy
            completed=requestFor detailTarget requests |> Maybe.map (\request -> Delivery.settle request.id True (Err Http.Timeout) newer) |> Maybe.withDefault newer
        in Expect.equal (Just (Delivery.Metadata session),Just Nothing,1)
            (payload detailTarget completed,Dict.get (Delivery.key workspace detailTarget) completed.resources |> Maybe.map .error,Dict.size completed.physical))
    , test "fresh empty membership and cleared runtime replace old presentation instead of falling back" <| withMetadata (\session ->
        let initial={session | runtimeIdentity=Just {model="Previous runtime",provider=Nothing,reasoning=Nothing}}
            (busy,requests)=Delivery.invalidateWorkspace workspace (ready initial) |> Delivery.admit workspace
            changed={session | state=Api.SessionTerminated,runtimeIdentity=Nothing,runtimeRevision=1}
            settled=List.foldl (\request current -> if request.target==detailTarget then Delivery.settle request.id True (Ok (Delivery.Metadata changed)) current else if request.target==listTarget then Delivery.settle request.id True (Ok (Delivery.SessionsPage {items=[],nextCursor=Nothing})) current else current) busy requests
        in Expect.equal (Just (Delivery.Metadata changed),Just (Delivery.SessionsPage {items=[],nextCursor=Nothing})) (payload detailTarget settled,payload listTarget settled))
    , test "authority retirement clears all retained display while stale callbacks only release their physical slots" <| withMetadata (\session ->
        let (busy,requests)=Delivery.invalidateWorkspace workspace (ready session) |> Delivery.admit workspace
            retired=Delivery.retireWorkspace workspace busy
            drained=List.foldl (\request current -> Delivery.settle request.id False (Ok (Delivery.Metadata session)) current) retired requests
        in Expect.equal {resources=0,physical=2,drainedResources=0,drainedPhysical=0} {resources=Dict.size retired.resources,physical=Dict.size retired.physical,drainedResources=Dict.size drained.resources,drainedPhysical=Dict.size drained.physical})
    , test "stale display cannot choose a new initial transcript window" <| withMetadata (\session ->
        let entered=Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
            delivery=entered.sessionDelivery
            seeded={delivery | resources=Dict.update (Delivery.key workspace detailTarget) (Maybe.map (\resource -> {resource | data=Just (Delivery.Metadata session),settled=resource.dirty})) delivery.resources}
            stale=Sessions.reconcile ({entered | sessionDelivery=Delivery.invalidateWorkspace workspace seeded},Cmd.none) |> Tuple.first
        in Expect.equal {start=Nothing,offset=0,end=0,window=False,metadata=Just (Delivery.Metadata session)}
            {start=stale.sessions.start,offset=stale.sessions.offset,end=stale.sessions.end,window=List.any (\(_,target) -> case target of
                Delivery.SessionWindow _ _ _ _ -> True
                _ -> False) stale.sessions.owned,metadata=payload detailTarget stale.sessionDelivery})
    ]



dockFixture : Model
dockFixture =
    let
        base = sessionModel
        loading = base.dataLoading
        entered = Sessions.reconcile ({base | activeTab=ProjectsTab,dataLoading={loading | visibleDetailDemand=Just (Set.empty,Set.fromList ["one","two"])}},Cmd.none) |> Tuple.first
        state = entered.sessions
        page = Decode.decodeString Api.sessionPageDecoder canonicalPage |> Result.withDefault {items=[],nextCursor=Nothing}
        second = page.items |> List.map (\session -> {session | id="aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa"})
        all = {page | items=page.items ++ second}
        cached = {result=Ok all,epoch=entered.sessionDelivery.listInvalidation,cursor=Nothing}
    in {entered | tasks=Dict.fromList (List.map (\id -> (id,{id=id,workspaceId=workspace,projectId=Nothing,parentId=Nothing,title=id,description=Nothing,status=Api.InProgress,priority=1,completedAt=Nothing,dependencyCount=0,memoryLinkCount=0,createdAt="",updatedAt=""})) ["one","two"]), sessionDelivery=List.foldl (\request delivery -> Delivery.settle request.id True (Ok (Delivery.SessionsPage all)) delivery) entered.sessionDelivery (Dict.values entered.sessionDelivery.physical), sessions={state | taskPages=Dict.fromList [("active:one",cached),("active:two",cached)],taskHints=Dict.fromList [("one",{active=True,error=False,epoch=entered.sessionDelivery.listInvalidation}),("two",{active=True,error=False,epoch=entered.sessionDelivery.listInvalidation})]}}


dockTests : List Test.Test
dockTests =
    [ test "invalidated membership cannot admit a new viewer or manually select a stale card" <| \_ ->
        let
            initial = dockFixture
            stale = {initial | sessionDelivery=Delivery.invalidateLists workspace initial.sessionDelivery}
            opened = Sessions.update (SessionsToggleDock "one") stale |> Sessions.reconcile |> Tuple.first
            admitted = Sessions.update (SessionsToggleDock "one") initial |> Sessions.reconcile |> Tuple.first
            refreshed = {admitted | sessionDelivery=Delivery.invalidateLists workspace admitted.sessionDelivery}
            attempted = Sessions.update (SessionsSelectDock "one" "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa") refreshed |> Sessions.reconcile |> Tuple.first
            metadataDemand current = List.any (\(_,target) -> case target of
                Delivery.SessionMetadata _ -> True
                Delivery.SessionWindow _ _ _ _ -> True
                _ -> False) current.sessions.owned
        in Expect.equal {opened=Nothing,demand=False,admitted=Just sessionId,attempted=Just sessionId}
            {opened=opened.sessions.selected,demand=metadataDemand opened,admitted=admitted.sessions.selected,attempted=attempted.sessions.selected}
    , test "unmount and authority retirement close the dock and reject old viewer callbacks while physical admissions remain" <| \_ ->
        let
            opened = Sessions.update (SessionsToggleDock "one") dockFixture |> Sessions.reconcile |> Tuple.first
            state = opened.sessions
            loading = opened.dataLoading
            withRows = {opened | sessions={state | rows=[sampleRecord],pending=True}}
            unmounted = Sessions.reconcile ({withRows | dataLoading={loading | visibleDetailDemand=Just (Set.empty,Set.empty)}},Cmd.none) |> Tuple.first
            missing = Sessions.reconcile ({withRows | tasks=Dict.remove "one" withRows.tasks,focus={focused | focusedEntity=Just ("task","one")}},Cmd.none) |> Tuple.first
            focused = withRows.focus
            retired = Sessions.reconcile ({withRows | sessionRequestEpoch=withRows.sessionRequestEpoch+1},Cmd.none) |> Tuple.first
            event = SessionsBridgeEvent (Encode.object [("kind",Encode.string "viewer"),("stamp",Encode.string state.stamp),("generation",Encode.int state.generation),("through",Encode.int 32),("rows",Encode.list identity [])])
            late = Sessions.update event unmounted |> Tuple.first
        in Expect.equal {dock=Nothing,selected=Nothing,rows=[],retired=Nothing,missing=Nothing,physical=Dict.size withRows.sessionDelivery.physical}
            {dock=late.sessions.taskPanel,selected=late.sessions.selected,rows=late.sessions.rows,retired=retired.sessions.taskPanel,missing=missing.sessions.taskPanel,physical=Dict.size unmounted.sessionDelivery.physical}
    , test "dock selection close and task switches preserve Evidence acquisition; full Sessions selection still retires it" <| \_ ->
        let
            initial = dockFixture
            state = initial.sessions
            operation = SessionArchiveOperation 1 "one" "Evidence" "picking"
            picking = {initial | sessions={state | archive=Just operation,nextOperation=2}}
            opened = Sessions.update (SessionsToggleDock "one") picking |> Sessions.reconcile |> Tuple.first
            switched = Sessions.update (SessionsToggleDock "two") opened |> Sessions.reconcile |> Tuple.first
            closed = Sessions.update (SessionsToggleDock "two") switched |> Sessions.reconcile |> Tuple.first
            global = Sessions.reconcile ({closed | activeTab=SessionsTab},Cmd.none) |> Tuple.first
        in Expect.equal ([Just operation,Just operation,Just operation],Nothing,True)
            (List.map (.sessions >> .archive) [opened,switched,closed],global.sessions.archive,opened.sessions.downloadStamp==closed.sessions.downloadStamp)
    , test "same session in another card and full view has distinct owner stamps" <| \_ ->
        let
            first = Sessions.update (SessionsToggleDock "one") dockFixture |> Sessions.reconcile |> Tuple.first
            second = Sessions.update (SessionsToggleDock "two") first |> Sessions.reconcile |> Tuple.first |> settleDockPages |> (\current -> Sessions.reconcile (current,Cmd.none) |> Tuple.first)
            full = Sessions.reconcile ({second | activeTab=SessionsTab},Cmd.none) |> Tuple.first
        in Expect.equal {first=Just sessionId,second=Just sessionId,card=True,full=True}
            {first=first.sessions.selected,second=second.sessions.selected,card=first.sessions.stamp/=second.sessions.stamp,full=second.sessions.stamp/=full.sessions.stamp}
    ]


settleDockPages : Model -> Model
settleDockPages current =
    let
        page = Decode.decodeString Api.sessionPageDecoder canonicalPage |> Result.withDefault {items=[],nextCursor=Nothing}
        settle request delivery = case request.target of
            Delivery.TaskSessionsFiltered _ _ _ -> Delivery.settle request.id True (Ok (Delivery.SessionsPage page)) delivery
            _ -> delivery
    in {current | sessionDelivery=List.foldl settle current.sessionDelivery (Dict.values current.sessionDelivery.physical)}
