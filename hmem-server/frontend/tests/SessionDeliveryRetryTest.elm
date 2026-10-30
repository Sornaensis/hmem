module SessionDeliveryRetryTest exposing (suite)

import Api
import Dict
import Expect
import Feature.SessionDelivery as Delivery
import Http
import Test exposing (describe, test)


workspace = "w"
session = "s"
metadata = Delivery.SessionMetadata session
window = Delivery.SessionWindow session 32 64 False
selection = {workspace=workspace,session=session,lifetime="selected:1"}
selected = Delivery.select (Just selection) Delivery.init


sample : Api.AgentSession
sample =
    { id=session,workspaceId=workspace,formatLabel="jsonl",state=Api.SessionActive
    ,creator={actorType=Nothing,actorId=Nothing,actorLabel=Nothing,botIdentity=Nothing}
    ,createdAt="2026-01-01T00:00:00Z",lastSeenAt="2026-01-01T00:00:00Z",lastActivityAt=Nothing
    ,committedBytes=64,committedRecords=1,revision=1,endsWithNewline=True
    ,terminatedAt=Nothing,finalBytes=Nothing,finalSha256=Nothing,title=Nothing,parentSessionId=Nothing
    ,ancestors=[],ancestorsTruncated=False,runtimeIdentity=Nothing,runtimeRevision=0 }


start target state =
    let
        (busy,requests)=Delivery.admit workspace (Delivery.demand workspace target state)
        fallback={id= -1,key="",workspace=workspace,target=target,dirty= -1,proofRetirement= -1,incarnation= -1}
    in (busy,List.filter (.target >> (==) target) requests |> List.head |> Maybe.withDefault fallback)


failed target state =
    let (busy,request)=start target state
        (waiting,ticket)=Delivery.settleSelected request.id True (Err Http.Timeout) busy
    in (waiting,ticket)


readyWindow =
    let (busy,request)=start metadata selected in
    Delivery.settle request.id True (Ok (Delivery.Metadata sample)) busy


entry target state = Dict.get (Delivery.key workspace target) state.resources
queued state = Delivery.admit workspace state |> Tuple.second |> List.map .target
fire ticket state = ticket |> Maybe.map (\value -> Delivery.retry value state) |> Maybe.withDefault state
progress state = Delivery.invalidateProgress workspace session (Delivery.invalidateSession workspace session state)


suite = describe "selected read bounded recovery"
    [ test "only completed transient errors qualify and delay is fixed at 500ms" <| \_ ->
        Expect.equal (500,[True,True,True,True,True,True,False,False,False,False,False,False])
            (Delivery.retryDelay,List.map Delivery.transient [Http.Timeout,Http.NetworkError,Http.BadStatus 500,Http.BadStatus 502,Http.BadStatus 503,Http.BadStatus 504,Http.BadStatus 401,Http.BadStatus 403,Http.BadStatus 409,Http.BadStatus 404,Http.BadBody "bad",Http.BadUrl "bad"])
    , test "waiting releases its slot, ignores progress, retries once then remains actionable" <| \_ ->
        let
            (waiting,ticket)=failed metadata selected
            invalidated=progress waiting
            (retrying,requests)=Delivery.admit workspace (fire ticket invalidated)
            exhausted=List.foldl (\request state -> Delivery.settle request.id True (Err Http.NetworkError) state) retrying requests
            stillFailed=progress exhausted |> Delivery.demand workspace metadata
        in Expect.equal {waiting=[],released=0,retries=[metadata],final=[],error=Just "refresh_failed",spent=Just Delivery.Spent}
            {waiting=queued invalidated,released=Dict.size waiting.physical,retries=List.map .target requests,final=queued stillFailed,error=entry metadata stillFailed |> Maybe.andThen .error,spent=entry metadata stillFailed |> Maybe.map .retry}
    , test "failure after progress during the original request still spends one bounded attempt" <| \_ ->
        let
            (busy,request)=start metadata selected
            (waiting,ticket)=Delivery.settleSelected request.id True (Err Http.Timeout) (progress busy)
        in Expect.equal (True,[],[metadata]) (ticket/=Nothing,queued waiting,queued (fire ticket waiting))
    , test "current range retry keeps exact offset, finite end, seal and metadata prerequisite" <| \_ ->
        let
            (waiting,ticket)=failed window readyWindow
            resumed=fire ticket (progress waiting)
            (busy,requests)=Delivery.admit workspace resumed
            rangeRequest=List.filter (.target >> (==) window) requests |> List.head
            done=rangeRequest |> Maybe.map (\request -> Delivery.settle request.id True (Ok (Delivery.Bytes {session=sample,offset=32,nextOffset=64,dataBase64=String.repeat 10 "AAAA" ++ "AAA=",hasMore=False})) busy) |> Maybe.withDefault busy
        in Expect.equal {targets=[metadata,window],payload=True,reset=Just Delivery.Available}
            {targets=List.map .target requests,payload=entry window done |> Maybe.andThen .data |> (/=) Nothing,reset=entry window done |> Maybe.map .retry}
    , test "accepted success restores one retry for the next refresh" <| \_ ->
        let
            (waiting,ticket)=failed metadata selected
            (busy,request)=start metadata (fire ticket waiting)
            done=Delivery.settle request.id True (Ok (Delivery.Metadata sample)) busy
            (again,nextTicket)=failed metadata (progress done)
        in Expect.equal (True,0,[]) (nextTicket/=Nothing,Dict.size again.physical,queued again)
    , test "permanent and malformed responses never arm a retry" <| \_ ->
        let
            check result =
                let (busy,request)=start metadata selected
                    (done,ticket)=Delivery.settleSelected request.id True result busy
                in (ticket==Nothing,queued (progress done)==[],(entry metadata done |> Maybe.andThen .error) /= Nothing)
        in Expect.equal True (List.all (\(a,b,c)->a&&b&&c) (List.map check [Err (Http.BadStatus 401),Err (Http.BadStatus 403),Err (Http.BadStatus 409),Err (Http.BadStatus 404),Err (Http.BadBody "bad"),Ok (Delivery.Metadata {sample | id="foreign"})]))
    , test "background metadata and download resources retain manual failure behavior" <| \_ ->
        let
            (busy,request)=start metadata Delivery.init
            (done,ticket)=Delivery.settleSelected request.id True (Err Http.Timeout) busy
            (saving,saveRequest)=start (Delivery.SessionSave session 0 32) readyWindow
            (_,saveTicket)=Delivery.settleSelected saveRequest.id True (Err Http.Timeout) saving
        in Expect.equal (Nothing,Nothing) (ticket,saveTicket)
    , test "deselection cancels waiting without automatically retrying a retained background owner" <| \_ ->
        let
            (waiting,ticket)=failed metadata selected
            background=Delivery.select Nothing waiting
        in Expect.equal ([],background) (queued background,fire ticket background)
    , test "same-key removal and re-add rejects an old timer" <| \_ ->
        let
            (waiting,ticket)=failed metadata selected
            new=waiting |> Delivery.removeDemand workspace metadata |> Delivery.demand workspace metadata
        in Expect.equal new (fire ticket new)
    , test "same-key stale success and failure release exactly their old physical slot" <| \_ ->
        let
            (busy,request)=start metadata selected
            new=busy |> Delivery.removeDemand workspace metadata |> Delivery.demand workspace metadata
            done result=Delivery.settle request.id True result new
        in Expect.equal [(0,Nothing,Nothing),(0,Nothing,Nothing)]
            (List.map (\result -> let next=done result in (Dict.size next.physical,entry metadata next |> Maybe.andThen .data,entry metadata next |> Maybe.andThen .error)) [Ok (Delivery.Metadata sample),Err Http.Timeout])
    , test "A to B to A changes lifetime even with the same resource key" <| \_ ->
        let
            (waiting,ticket)=failed metadata selected
            new=waiting |> Delivery.select (Just {selection | session="b",lifetime="selected:2"}) |> Delivery.select (Just {selection | lifetime="selected:3"})
        in Expect.equal new (fire ticket new)
    , test "target change cannot attach an old range ticket" <| \_ ->
        let
            (waiting,ticket)=failed window readyWindow
            new=Delivery.demand workspace (Delivery.SessionWindow session 48 64 False) waiting
        in Expect.equal new (fire ticket new)
    , test "manual retry cancels the old timer and starts a fresh budget" <| \_ ->
        let
            (waiting,ticket)=failed metadata selected
            manual=Delivery.retrySelected waiting
            (again,nextTicket)=failed metadata manual
        in Expect.equal (manual,True) (fire ticket manual,nextTicket/=Nothing)
    , test "proof, workspace, logout and authority retirement cancel delayed tickets" <| \_ ->
        let
            (waiting,ticket)=failed metadata selected
            retired=[Delivery.invalidateReadProof workspace waiting,Delivery.invalidateWorkspace workspace waiting,Delivery.invalidateAll waiting,Delivery.retireWorkspace workspace waiting,Delivery.retireAll waiting,Delivery.select Nothing waiting]
        in Expect.equal retired (List.map (fire ticket) retired)
    , test "retirement keeps two old physical requests until their own completions" <| \_ ->
        let
            target=Delivery.SessionMetadata "other"
            (busy,requests)=Delivery.admit workspace (selected |> Delivery.demand workspace metadata |> Delivery.demand workspace target)
            new=Delivery.retireAll busy |> Delivery.demand workspace metadata
            (capped,admitted)=Delivery.admit workspace new
            released=List.head requests |> Maybe.map (\request -> Delivery.settle request.id False (Err Http.Timeout) capped) |> Maybe.withDefault capped
            (next,replacements)=Delivery.admit workspace released
        in Expect.equal ((2,[]),(2,1)) ((Dict.size capped.physical,List.map .target admitted),(Dict.size next.physical,List.length replacements))
    , test "requeued foreground retry respects the existing background fairness budget" <| \_ ->
        let
            (waiting,ticket)=failed metadata selected
            background=Delivery.WorkspaceSessions Nothing
            resumed=fire ticket waiting |> Delivery.demand workspace background
            oneSlot={resumed | foregroundStreak=3,physical=Dict.singleton 999 {id=999,key="held",workspace=workspace,target=metadata,dirty=0,proofRetirement=0,incarnation=0}}
        in Expect.equal [background] (queued oneSlot)
    ]
