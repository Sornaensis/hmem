module SessionsTest exposing (suite)

import Api
import Char
import AppShell
import Dict
import Expect
import Feature.Sessions as Sessions
import Feature.Documents as Documents
import Feature.Cards as Cards
import Feature.SessionDelivery as Delivery
import Helpers
import Http
import Html.Attributes
import Set
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Types exposing (..)
import Url


suite : Test.Test
suite =
    describe "Sessions production wire contract"
        [ describe "workspace completed reader" completedReaderTests
        , test "native context disclosures preserve source authority, transport, following and existing state" <| \_ ->
            let
                entered=Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                old=entered.sessions
                state={old | rows=[sampleRecord],pending=True,offset=64,end=128,windowPublication=9,historyOpen=Set.singleton "existing",manualUp=True,following=False}
                current={entered | sessions=state}
                packet stamp generation scope=SessionsBridgeEvent (Encode.object [("kind",Encode.string "viewer-mode-disclosure"),("stamp",Encode.string stamp),("generation",Encode.int generation),("scope",Encode.string scope),("key",Encode.string "context:source"),("open",Encode.bool True)])
                next=Sessions.update (packet state.stamp state.generation state.viewerScope) current |> Tuple.first
                ignored=[packet "retired" state.generation state.viewerScope,packet state.stamp (state.generation-1) state.viewerScope,packet state.stamp state.generation "retired"]
            in Expect.all
                [\_ -> Expect.equal {state | historyOpen=Set.insert "mode:context:source" state.historyOpen} next.sessions
                ,\_ -> Expect.equal current.sessionDelivery next.sessionDelivery
                ,\_ -> Expect.equal True (List.all (\event -> (Sessions.update event current |> Tuple.first |> .sessions)==state) ignored)] ()
        , test "active conversation is a named region without presentation tabs" <| \_ ->
            let reader=Query.fromHtml (Sessions.view workspace (Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first))
            in Expect.all
                [\_ -> reader |> Query.find [Selector.id "session-records"] |> Query.has [Selector.attribute (Html.Attributes.attribute "role" "region"),Selector.attribute (Html.Attributes.attribute "aria-label" "Session conversation")]
                ,\_ -> reader |> Query.hasNot [Selector.class "session-presentation-tabs"]
                ,\_ -> reader |> Query.hasNot [Selector.attribute (Html.Attributes.attribute "role" "tabpanel")]] ()
        , test "disclosure UI acknowledgements preserve exact generation and transport authority" <| \_ ->
            let
                entered=Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                old=entered.sessions
                current={entered | sessions={old | rows=[sampleRecord],pending=True,offset=64,end=128,windowPublication=9,positionPending=True}}
                packet kind stamp generation isOpen=SessionsBridgeEvent (Encode.object [("kind",Encode.string kind),("stamp",Encode.string stamp),("generation",Encode.int generation),("scope",Encode.string old.viewerScope),("namespace",Encode.string "group:owned-run"),("key",Encode.string "owned-run"),("request",Encode.int 7),("open",Encode.bool isOpen)])
                stale=Sessions.update (packet "viewer-disclosure" old.stamp (old.generation-1) True) current |> Tuple.first
                foreign=Sessions.update (packet "viewer-disclosure" "retired" old.generation True) current |> Tuple.first
                opened=Sessions.update (packet "viewer-disclosure" old.stamp old.generation True) current |> Tuple.first
                repeated=Sessions.update (packet "viewer-disclosure" old.stamp old.generation True) opened |> Tuple.first
                closed=Sessions.update (packet "viewer-disclosure" old.stamp old.generation False) repeated |> Tuple.first
                staleError=Sessions.update (packet "viewer-disclosure-error" old.stamp (old.generation-1) True) opened |> Tuple.first
                failed=Sessions.update (packet "viewer-disclosure-error" old.stamp old.generation True) current |> Tuple.first
                unrelated=Sessions.update (SessionsBridgeEvent (Encode.object [("kind",Encode.string "viewer-disclosure"),("stamp",Encode.string old.stamp),("generation",Encode.int old.generation),("scope",Encode.string old.viewerScope),("namespace",Encode.string "group:unrelated"),("key",Encode.string "unrelated"),("request",Encode.int 8),("open",Encode.bool True)])) failed |> Tuple.first
            in Expect.equal {stale=(False,False),values=(True,True,False),error=(Nothing,True),unrelated=(True,False,True),transport={rows=[sampleRecord],pending=True,offset=64,end=128,publication=9,position=True}}
                {stale=(Set.member "owned-run" stale.sessions.historyOpen,Set.member "owned-run" foreign.sessions.historyOpen),values=(Set.member "owned-run" opened.sessions.historyOpen,Set.member "owned-run" repeated.sessions.historyOpen,Set.member "owned-run" closed.sessions.historyOpen),error=(staleError.sessions.error,failed.sessions.error/=Nothing),unrelated=(unrelated.sessions.error==failed.sessions.error,Set.member "owned-run" unrelated.sessions.historyOpen,Set.member "unrelated" unrelated.sessions.historyOpen),transport={rows=failed.sessions.rows,pending=failed.sessions.pending,offset=failed.sessions.offset,end=failed.sessions.end,publication=failed.sessions.windowPublication,position=failed.sessions.positionPending}}
        , test "bounded disclosure acknowledgement reserves the requested key and its already-open ancestor" <| \_ ->
            let
                entered=Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                old=entered.sessions
                parent="999999:0"
                child=parent++":0"
                others=List.range 0 398 |> List.map (\index -> "1"++String.padLeft 4 '0' (String.fromInt index)) |> Set.fromList
                seeded={entered | sessions={old | historyOpen=Set.insert parent others}}
                packet key ancestor isOpen=SessionsBridgeEvent (Encode.object [("kind",Encode.string "viewer-disclosure"),("stamp",Encode.string old.stamp),("generation",Encode.int old.generation),("scope",Encode.string old.viewerScope),("namespace",Encode.string ("event:"++key)),("key",Encode.string key),("ancestor",Encode.string ancestor),("request",Encode.int 7),("open",Encode.bool isOpen)])
                requested=Sessions.update (packet "zz-latest" "" True) seeded |> Tuple.first
                nested=Sessions.update (packet child parent True) seeded |> Tuple.first
                closedParent=Sessions.update (packet parent "" False) seeded |> Tuple.first
                declined=Sessions.update (packet child parent True) closedParent |> Tuple.first
                cancelledError=Sessions.update (SessionsBridgeEvent (Encode.object [("kind",Encode.string "viewer-disclosure-error"),("stamp",Encode.string old.stamp),("generation",Encode.int old.generation),("scope",Encode.string old.viewerScope),("namespace",Encode.string ("event:"++child)),("key",Encode.string child),("ancestor",Encode.string parent),("request",Encode.int 7),("open",Encode.bool True)])) closedParent |> Tuple.first
            in Expect.equal {latest=(400,True),nested=(400,True,True),newerClose=(False,False),cancelledError=Nothing}
                {latest=(Set.size requested.sessions.historyOpen,Set.member "zz-latest" requested.sessions.historyOpen),nested=(Set.size nested.sessions.historyOpen,Set.member parent nested.sessions.historyOpen,Set.member child nested.sessions.historyOpen),newerClose=(Set.member parent declined.sessions.historyOpen,Set.member child declined.sessions.historyOpen),cancelledError=cancelledError.sessions.error}
        , test "a backward omitted-gap read preserves its proven first record boundary" <| \_ ->
            let
                entered=Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                old=entered.sessions
                historical={entered | sessions={old | following=False,pending=False,positionPending=False,loadWindow=False,rows=[{sampleRecord | offset=1000,end=1100}]}}
                packet before after fraction=SessionsBridgeEvent (Encode.object [("kind",Encode.string "viewer-boundary"),("stamp",Encode.string old.stamp),("generation",Encode.int old.generation),("edge",Encode.string "gap"),("before",Encode.int before),("after",Encode.int after),("fraction",Encode.float fraction),("direction",Encode.string "tail")])
                exact=Sessions.update (packet 900 1000 1) historical |> Tuple.first |> .sessions
                partial=Sessions.update (packet 900 200000 1) historical |> Tuple.first |> .sessions
                interior=Sessions.update (packet 900 200000 0.5) historical |> Tuple.first |> .sessions
            in Expect.equal {exact=(Just 900,1100,True),tailPartial=False,interior=False,tailDirection="tail"}
                {exact=(exact.start,exact.end,exact.windowAligned),tailPartial=partial.windowAligned,interior=interior.windowAligned,tailDirection=exact.windowDirection}
        , test "current publication position protects retained source and presentation before a prepend is composed" <| \_ ->
            let
                entered=Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                old=entered.sessions
                retained={sampleRecord | offset=60050,presentationOffset=60050,end=60238}
                anchor=String.padLeft 20 '0' "60050"
                fragment=anchor ++ ":0000000:content:0:0:0"
                reading={entered | sessions={old | following=False,start=Just 0,offset=0,end=64,rows=[retained],pending=True,resetPending=False,windowPurpose="gap",historyAnchor="100",presentationAnchor="request-time"}}
                event stamp generation=SessionsBridgeEvent (Encode.object [("kind",Encode.string "viewer"),("stamp",Encode.string stamp),("generation",Encode.int generation),("through",Encode.int 64),("before",Encode.int 0),("completeThrough",Encode.int 64),("rows",Encode.list identity []),("position",Encode.object [("anchor",Encode.string anchor),("fragmentAnchor",Encode.string fragment)])])
                current=Sessions.update (event old.stamp old.generation) reading |> Tuple.first |> .sessions
                stale=Sessions.update (event old.stamp (old.generation-1)) reading |> Tuple.first |> .sessions
                foreign=Sessions.update (event "retired" old.generation) reading |> Tuple.first |> .sessions
                following={reading | sessions={old | following=True,start=Just 0,offset=0,end=64,rows=[retained],pending=True,windowPurpose="append",historyAnchor="owned-latest",presentationAnchor="owned-latest"}}
                latest=Sessions.update (event old.stamp old.generation) following |> Tuple.first |> .sessions
            in
            Expect.equal {position=(anchor,fragment,[retained]),authority=(64,64),stale=("100","100"),latest=("owned-latest","owned-latest")}
                {position=(current.historyAnchor,current.presentationAnchor,current.rows),authority=(current.offset,current.historyScanAfter),stale=(stale.historyAnchor,foreign.historyAnchor),latest=(latest.historyAnchor,latest.presentationAnchor)}
        , test "active latest starts a new bounded tail lifetime without discarding the loaded conversation" <| \_ ->
            let
                entered=Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                old=entered.sessions
                historical={entered | sessions={old | following=False,start=Just 0,offset=32,end=32,rows=[sampleRecord],latestRequest=7,latestError=Just "failed"}}
                next=Sessions.update SessionsJumpLatest historical |> Tuple.first |> .sessions
            in Expect.equal {rows=[sampleRecord],start=Nothing,following=True,pending=True,request=8,generation=old.generation+1,error=Nothing}
                {rows=next.rows,start=next.start,following=next.following,pending=next.latestPending,request=next.latestRequest,generation=next.generation,error=next.latestError}
        , test "queued historical follow cannot stop an accepted latest acquisition" <| \_ ->
            let
                entered=Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                jumped=Sessions.update SessionsJumpLatest entered |> Tuple.first
                state=jumped.sessions
                queued=SessionsBridgeEvent (Encode.object [("kind",Encode.string "viewer-follow"),("stamp",Encode.string state.stamp),("generation",Encode.int state.generation),("manualUp",Encode.bool True),("following",Encode.bool False)])
                next=Sessions.update queued jumped |> Tuple.first |> .sessions
            in Expect.equal (True,True,True) (next.latestPending,next.following,next.loadWindow)
        , test "only matching active publication positioning releases the logical barrier" <| \_ ->
            let
                entered=Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                old=entered.sessions
                waiting={entered | sessions={old | positionPending=True,windowPublication=17}}
                ack generation publication=SessionsBridgeEvent (Encode.object [("kind",Encode.string "viewer-positioned"),("stamp",Encode.string old.stamp),("generation",Encode.int generation),("publication",Encode.int publication)])
                stale=Sessions.update (ack (old.generation-1) 17) waiting |> Tuple.first
                wrong=Sessions.update (ack old.generation 18) stale |> Tuple.first
                done=Sessions.update (ack old.generation 17) wrong |> Tuple.first
            in Expect.equal (True,True,False) (stale.sessions.positionPending,wrong.sessions.positionPending,done.sessions.positionPending)
        , test "a stale latest completion cannot finish a newer jump" <| \_ ->
            let
                entered=Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                jumped=Sessions.update SessionsJumpLatest entered |> Tuple.first
                next=Sessions.update SessionsJumpLatest jumped |> Tuple.first
                state=next.sessions
                packet request generation=SessionsBridgeEvent (Encode.object [("kind",Encode.string "latest-complete"),("stamp",Encode.string state.stamp),("generation",Encode.int generation),("request",Encode.int request)])
                stale=Sessions.update (packet (state.latestRequest-1) state.generation) next |> Tuple.first
                wrong=Sessions.update (packet state.latestRequest (state.generation-1)) stale |> Tuple.first
                complete=Sessions.update (packet state.latestRequest state.generation) wrong |> Tuple.first
            in Expect.equal (True,True,False) (stale.sessions.latestPending,wrong.sessions.latestPending,complete.sessions.latestPending)
        , test "an incomplete forward replacement and failed next boundary preserve the admitted conversation" <| \_ ->
            let
                entered=Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                old=entered.sessions
                reading={entered | sessions={old | start=Just 32,offset=32,end=180032,pending=True,rows=[sampleRecord],windowDirection="forward"}}
                event=Encode.object [("kind",Encode.string "viewer"),("stamp",Encode.string old.stamp),("generation",Encode.int old.generation),("through",Encode.int 65568),("pendingBytes",Encode.int 65536),("rows",Encode.list identity [])]
                partial=Sessions.update (SessionsBridgeEvent event) reading |> Tuple.first
                target=Delivery.SessionWindow sessionId 65568 180032 False
                (metadataWaiting,metadataRequests)=Delivery.admit workspace (Delivery.demand workspace target Delivery.init)
                metadata=Decode.decodeString Api.sessionPageDecoder canonicalPage |> Result.toMaybe |> Maybe.andThen (.items >> List.head)
                ready=metadata |> Maybe.map (\session -> List.foldl (\request delivery -> Delivery.settle request.id True (Ok (Delivery.Metadata session)) delivery) metadataWaiting metadataRequests) |> Maybe.withDefault metadataWaiting
                (admitted,requests)=Delivery.admit workspace ready
                failed=List.filter (.target >> (==) target) requests |> List.head |> Maybe.map (\request -> Delivery.settle request.id True (Err (Http.BadStatus 400)) admitted) |> Maybe.withDefault admitted
                boundary={partial | sessionDelivery=failed}
                retried=Sessions.update SessionsRetry boundary |> Tuple.first
                failedBoundary=Dict.get (Delivery.key workspace target) failed.resources |> Maybe.andThen .error
            in Expect.equal {admitted=[sampleRecord],retried=[sampleRecord],offset=65568,failed=True} {admitted=partial.sessions.rows,retried=retried.sessions.rows,offset=retried.sessions.offset,failed=failedBoundary /= Nothing}
        , test "reader backpressure acknowledges a physical chunk and continues the same logical conversation" <| \_ ->
            let
                entered=Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                old=entered.sessions
                reading={entered | sessions={old | start=Just 0,offset=0,end=65536,pending=True,rows=[sampleRecord]}}
                event=Encode.object [("kind",Encode.string "viewer"),("stamp",Encode.string old.stamp),("generation",Encode.int old.generation),("through",Encode.int 32000),("before",Encode.int 0),("pageFull",Encode.bool True),("rows",Encode.list identity [])]
                next=Sessions.update (SessionsBridgeEvent event) reading |> Tuple.first |> .sessions
            in Expect.equal {offset=32000,after=32000,pending=False,loading=True} {offset=next.offset,after=next.historyAfter,pending=next.pending,loading=next.loadWindow}
        , test "directional loading retains the current conversation until a matching page arrives" <| \_ ->
            let
                old=emptySessionsModel
                model={sessionModel | sessions={old | historyBefore=3000000,historyAfter=3000032,rows=[{sampleRecord | offset=3000000,end=3000032}],offset=3000032,end=3000032}}
                earlier=Sessions.update SessionsOlder model |> Tuple.first |> .sessions
                later=Sessions.update SessionsNewer model |> Tuple.first |> .sessions
            in Expect.equal {earlier=[{sampleRecord | offset=3000000,end=3000032}],later=[{sampleRecord | offset=3000000,end=3000032}],backward="tail",forward="forward",aligned=True}
                {earlier=earlier.rows,later=later.rows,backward=earlier.windowDirection,forward=later.windowDirection,aligned=later.windowAligned}
        , test "a maximum-sized overlap cannot strand forward traversal before the next record" <| \_ ->
            let
                old=emptySessionsModel
                row={sampleRecord | offset=100,end=1048678}
                model={sessionModel | sessions={old | historyBefore=100,historyAfter=row.end,rows=[row],offset=row.end,end=4000000}}
                next=Sessions.update SessionsNewer model |> Tuple.first |> .sessions
            in Expect.equal (Just row.end,row.end,True) (next.start,next.offset,next.windowAligned)
        , test "backward scanning starts with a bounded request without replaying mounted maximum-sized records" <| \_ ->
            let
                old=emptySessionsModel
                first={sampleRecord | offset=3000000,end=4048578}
                last={sampleRecord | offset=4048578,end=5097156}
                model={sessionModel | sessions={old | historyBefore=3000000,historyAfter=last.end,rows=[first,last],offset=last.end,end=last.end}}
                next=Sessions.update SessionsOlder model |> Tuple.first |> .sessions
            in Expect.equal (Just 2868928,2868928,3000000) (next.start,next.offset,next.end)
        , test "older reads reserve new history before a saturated retained overlap" <| \_ ->
            let
                first=3000000
                rows=List.range 0 19 |> List.map (\index -> {sampleRecord | offset=first+index*3276,end=first+(index+1)*3276})
                old=emptySessionsModel
                reading={sessionModel | sessions={old | following=False,historyBefore=first,historyAfter=first+65520,rows=rows,historyAnchor=String.fromInt first}}
                next=Sessions.update SessionsOlder reading |> Tuple.first |> .sessions
            in Expect.equal {start=Just (first-131072),end=first+65520,rows=rows,anchor=String.fromInt first} {start=next.start,end=next.end,rows=next.rows,anchor=next.historyAnchor}
        , test "short-view fill advances from its scanned boundary before compacted mounted rows" <| \_ ->
            let
                old=emptySessionsModel
                reading={sessionModel | sessions={old | windowPurpose="fill",historyBefore=4000000,historyScanBefore=3000000,historyScanEnd=3000500,rows=[{sampleRecord | offset=4000000,end=4000500}]}}
                next=Sessions.update SessionsOlder reading |> Tuple.first |> .sessions
            in Expect.equal {start=Just 2868928,end=3000500,purpose="fill",aligned=False} {start=next.start,end=next.end,purpose=next.windowPurpose,aligned=next.windowAligned}
        , test "short-view fill preserves a maximum-sized scanned overlap and its occurrence context" <| \_ ->
            let
                old=emptySessionsModel
                rows=[{sampleRecord | offset=5000000,end=5000500,activityAnchors=Dict.fromList [("0","3000000:0")]}]
                reading={sessionModel | sessions={old | windowPurpose="fill",historyBefore=5000000,historyScanBefore=3000000,historyScanEnd=4048578,rows=rows,historyAnchor="5000000",presentationAnchor="5000000:0:0"}}
                next=Sessions.update SessionsOlder reading |> Tuple.first |> .sessions
            in Expect.equal {range=(Just 2868928,4048578),context=(rows,"5000000","5000000:0:0")} {range=(next.start,next.end),context=(next.rows,next.historyAnchor,next.presentationAnchor)}
        , test "repeated older reads move before each accepted boundary and clamp at the beginning" <| \_ ->
            let
                old=emptySessionsModel
                reading={sessionModel | sessions={old | historyBefore=300000,rows=[{sampleRecord | offset=300000,end=300100}]}}
                first=Sessions.update SessionsOlder reading |> Tuple.first
                previous=first.sessions
                advanced={first | sessions={previous | historyBefore=40000,rows=[{sampleRecord | offset=40000,end=40100}],pending=False}}
                next=Sessions.update SessionsOlder advanced |> Tuple.first |> .sessions
            in Expect.equal {first=Just 168928,next=Just 0,end=40100,generation=old.generation+2} {first=first.sessions.start,next=next.start,end=next.end,generation=next.generation}
        , test "a tail entirely inside one record expands once within the existing bounded lookback" <| \_ ->
            let
                entered=Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                old=entered.sessions
                reading={entered | sessions={old | start=Just 3000000,offset=3000000,end=3065536,pending=True}}
                event=Encode.object [("kind",Encode.string "viewer"),("stamp",Encode.string old.stamp),("generation",Encode.int old.generation),("through",Encode.int 3065536),("skipping",Encode.bool True),("rows",Encode.list identity [])]
                next=Sessions.update (SessionsBridgeEvent event) reading |> Tuple.first |> .sessions
            in Expect.equal {start=Just 968384,offset=968384,end=3065536,reset=True} {start=next.start,offset=next.offset,end=next.end,reset=next.resetPending}
        , test "a final progressive fill compares progress with its original boundary instead of its intermediate scan cursor" <| \_ ->
            let
                entered=Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                old=entered.sessions
                reading={entered | sessions={old | windowPurpose="fill",start=Just 3000000,offset=3065500,end=3065536,pending=True,historyBefore=3030000,historyScanBefore=3000100}}
                event progress generation skipping=SessionsBridgeEvent (Encode.object [("kind",Encode.string "viewer"),("stamp",Encode.string old.stamp),("generation",Encode.int generation),("through",Encode.int 3065536),("before",Encode.int 3030000),("parsedBefore",Encode.int 3000100),("olderProgress",Encode.bool progress),("skipping",Encode.bool skipping),("rows",Encode.list identity [])])
                complete=Sessions.update (event True old.generation False) reading |> Tuple.first |> .sessions
                noProgress=Sessions.update (event False old.generation False) reading |> Tuple.first |> .sessions
                fragment=Sessions.update (event True old.generation True) reading |> Tuple.first |> .sessions
                stale=Sessions.update (event True (old.generation-1) False) reading |> Tuple.first |> .sessions
            in Expect.equal {complete=(Just 3000000,3065536,old.generation),recovery=(Just 968384,Just 968384),stale=(3000000+65500,True)}
                {complete=(complete.start,complete.offset,complete.generation),recovery=(noProgress.start,fragment.start),stale=(stale.offset,stale.pending)}
        , test "proven run corrections apply count deltas before merging evicted rows and gap labels" <| \_ ->
            let
                entered=Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                old=entered.sessions
                retained={sampleRecord | activityAnchors=Dict.fromList [("0","temporary")]}
                gaps=[{before="100",after="110",pixels=100,group="temporary"},{before="120",after="130",pixels=100,group="canonical"}]
                reading={entered | sessions={old | windowPurpose="append",following=False,start=Just 10000,offset=10000,end=10064,pending=True,rows=[retained],historyGaps=gaps,historyActivity=Dict.fromList [("temporary",408),("canonical",492)],historyOpen=Set.singleton "temporary",historyClosed=Set.singleton "canonical"}}
                packet generation=SessionsBridgeEvent (Encode.object [("kind",Encode.string "viewer"),("stamp",Encode.string old.stamp),("generation",Encode.int generation),("through",Encode.int 10064),("before",Encode.int 10000),("rows",Encode.list identity []),("groups",Encode.object [("temporary",Encode.int -408),("canonical",Encode.int 408)]),("runAliases",Encode.object [("temporary",Encode.string "canonical")])])
                accepted=Sessions.update (packet old.generation) reading |> Tuple.first
                repeated=Sessions.update (packet old.generation) accepted |> Tuple.first |> .sessions
                stale=Sessions.update (packet (old.generation-1)) reading |> Tuple.first |> .sessions
                next=accepted.sessions
            in Expect.equal {counts=Dict.singleton "canonical" 900,anchors=[Dict.singleton "0" "canonical"],gaps=Set.singleton "canonical",open=Set.empty,replay=True,stale=Dict.fromList [("temporary",408),("canonical",492)]}
                {counts=next.historyActivity,anchors=List.map .activityAnchors next.rows,gaps=List.map .group next.historyGaps |> Set.fromList,open=next.historyOpen,replay=repeated==next,stale=stale.historyActivity}
        , describe "direct task association presence" associationTests
        , describe "authoritative evidence availability" evidenceAvailabilityTests
        , describe "card-owned dock authority" dockTests
        , describe "same-authority metadata presentation" metadataPresentationTests
        , test "session and authority changes reset linked-task paging" <| \_ ->
            let
                entered=Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                state=entered.sessions
                expanded={entered | sessions={state | linksCursor=Just "old-task"}}
                epoch=Sessions.reconcile ({expanded | sessionRequestEpoch=expanded.sessionRequestEpoch+1},Cmd.none) |> Tuple.first
                url=expanded.url
                selected=Sessions.reconcile ({expanded | url={url | fragment=Just "tab=sessions&session=aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa"}},Cmd.none) |> Tuple.first
            in Expect.equal (Nothing,Nothing) (epoch.sessions.linksCursor,selected.sessions.linksCursor)
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
            Expect.equal ((Just Nothing,Just 0),(Just (Just {model="Actual target",provider=Nothing,reasoning=Nothing,role=Nothing,harness=Nothing}),Just 3,Just (Just "Sandbox Superadmin")))
                ((Maybe.map .runtimeIdentity absent,Maybe.map .runtimeRevision absent),(Maybe.map .runtimeIdentity known,Maybe.map .runtimeRevision known,Maybe.map (.creator >> .actorLabel) known))
        , test "canonical occurrence is optional nullable metadata and never inferred from transport timestamps" <| \_ ->
            let
                decode extra=canonicalPage |> String.replace "\"state\":\"active\"" ("\"state\":\"active\"" ++ extra) |> Decode.decodeString Api.sessionPageDecoder |> Result.toMaybe |> Maybe.andThen (.items >> List.head) |> Maybe.andThen .lastActivityAt
            in Expect.equal [Nothing,Nothing,Just "2026-10-23T09:00:00Z"]
                (List.map decode ["",",\"last_activity_at\":null",",\"last_activity_at\":\"2026-10-23T09:00:00Z\""])
        , test "runtime harness is explicit and optional on the wire" <| \_ ->
            let
                decode fields = canonicalPage |> String.replace "\"state\":\"active\"" ("\"state\":\"active\",\"runtime_identity\":{" ++ fields ++ "}") |> Decode.decodeString Api.sessionPageDecoder |> Result.toMaybe |> Maybe.andThen (.items >> List.head) |> Maybe.andThen .runtimeIdentity |> Maybe.andThen .harness
            in
            Expect.equal [Just "actual-harness",Nothing,Nothing]
                [decode "\"model\":\"Actual model\",\"harness\":\"actual-harness\"",decode "\"model\":\"Legacy model\",\"provider\":\"not-a-harness\"",decode "\"model\":\"Legacy model\",\"harness\":null"]
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
        , test "a captured incomplete suffix settles without complete-record authority and growth reframes its prefix" <| \_ ->
            let
                entered = Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                old = entered.sessions
                reading = {entered | sessions={old | start=Just 0,offset=0,end=8,historyAfter=0,historyScanAfter=0,pending=True,resetPending=False,rows=[]}}
                event generation = SessionsBridgeEvent (Encode.object [("kind",Encode.string "viewer-progress"),("stamp",Encode.string old.stamp),("generation",Encode.int generation),("through",Encode.int 8),("pendingBytes",Encode.int 8),("notice",Encode.string "Incomplete record at this finite window boundary.")])
                captured = Sessions.update (event old.generation) reading |> Tuple.first
                stale = Sessions.update (event (old.generation-1)) reading |> Tuple.first
                -- Existing metadata has a larger committed end: only that real
                -- growth may reopen the range, at the complete boundary zero.
                grown = Sessions.reconcile (captured,Cmd.none) |> Tuple.first
            in
            Expect.equal (((8,8,0),(False,False,[])),(0,0,True),Just 0)
                (((captured.sessions.offset,captured.sessions.historyAfter,captured.sessions.historyScanAfter),(captured.sessions.pending,captured.sessions.loadWindow,captured.sessions.rows)),(stale.sessions.offset,stale.sessions.historyAfter,stale.sessions.pending),grown.sessions.start)
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
                        settled = {delivery | resources=Dict.update (Delivery.key workspace (Delivery.SessionMetadata sessionId)) (Maybe.map (\entry -> {entry | data=Just (Delivery.Metadata {metadata | committedBytes=128}),settled=entry.dirty,proven=True})) delivery.resources}
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
        , test "activity toggles compose canonically and reset list and both facet continuations" <| \_ ->
            let
                state=sessionModel.sessions
                paged={sessionModel | sessions={state | cursor=Just {createdAt="old",id="old"},previous=[Nothing],projectFacetCursor=Just "old-project",taskFacetCursor=Just "old-task"}}
                changed=Sessions.update (SessionsActivityFilter "completed") paged |> Tuple.first
                toggled=Sessions.update (SessionsActivityFilter "quiet") changed |> Tuple.first
                empty=List.foldl (\name model -> Sessions.update (SessionsActivityFilter name) model |> Tuple.first) toggled ["completed","live","quiet"]
            in
            Expect.equal {activity=Just "live,quiet,completed",empty=Just "",cursor=Nothing,previous=[],project=Nothing,task=Nothing}
                {activity=toggled.sessions.filters.activity,empty=empty.sessions.filters.activity,cursor=changed.sessions.cursor,previous=changed.sessions.previous,project=changed.sessions.projectFacetCursor,task=changed.sessions.taskFacetCursor}
        , test "filter changes retire old cursor demand and a hidden or unauthorized tab cannot restore demand" <| \_ ->
            let
                entered = Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                oldTarget = Delivery.WorkspaceSessionsFiltered Api.workspaceSessionFilters Nothing
                filtered = Sessions.update (SessionsFreshnessFilter "stale") entered |> (\pair -> Sessions.reconcile pair) |> Tuple.first
                hidden = Sessions.reconcile ({filtered | activeTab=ProjectsTab},Cmd.none) |> Tuple.first
                denied = Sessions.reconcile ({filtered | auth={status=AuthRequired,mode=Nothing}},Cmd.none) |> Tuple.first
            in
            Expect.equal {old=False,filter="stale",hidden=0,denied=0,physical=2}
                {old=Dict.member (Delivery.key workspace oldTarget) filtered.sessionDelivery.resources,filter=filtered.sessions.filters.freshness
                ,hidden=Dict.size hidden.sessionDelivery.resources,denied=Dict.size denied.sessionDelivery.resources,physical=Dict.size denied.sessionDelivery.physical}
        , test "late physical callback from an old filter releases only its slot and cannot install that page" <| \_ ->
            let
                oldTarget=Delivery.WorkspaceSessionsFiltered Api.workspaceSessionFilters Nothing
                (busy,requests)=Delivery.demand workspace oldTarget Delivery.init |> Delivery.admit workspace
                newTarget=Delivery.WorkspaceSessionsFiltered {state="terminated",freshness="",activity=Nothing} Nothing
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
            Expect.equal {pages=22,hints=22,associations=22,bounded=True,retiredPages=0,retiredHints=0,retiredAssociations=0}
                {pages=Dict.size ready.sessions.taskPages,hints=Dict.size ready.sessions.taskHints,associations=Dict.size ready.sessions.taskAssociations,bounded=bounded,retiredPages=Dict.size retired.sessions.taskPages,retiredHints=Dict.size retired.sessions.taskHints,retiredAssociations=Dict.size retired.sessions.taskAssociations}
        , test "active and evidence cursors remain independent between tasks" <| \_ ->
            let
                cursor={createdAt="2026-10-24T00:00:00Z",id=sessionId}
                advanced=Sessions.update (SessionsTaskNextPage "active:one" cursor) sessionModel |> Tuple.first
                next=Sessions.update (SessionsTaskNextPage "terminated:two" cursor) advanced |> Tuple.first
                back=Sessions.update (SessionsTaskPreviousPage "active:one") next |> Tuple.first
            in
            Expect.equal (Just Nothing,Just (Just cursor),Nothing)
                (Dict.get "active:one" back.sessions.taskCursors,Dict.get "terminated:two" back.sessions.taskCursors,Dict.get "active:two" back.sessions.taskCursors)
        , test "retrying a rotated-out task section retires only its current cached failure" <| \_ ->
            let
                entered = Sessions.reconcile (associationFixture,Cmd.none) |> Tuple.first
                state = entered.sessions
                failure = {result=Err "session read failed",epoch=entered.sessionDelivery.listInvalidation,cursor=Nothing}
                pages = Dict.fromList [("terminated:one",failure),("active:one",failure),("terminated:two",failure)]
                failed = {entered | sessions={state | taskPages=pages}}
                retried = Sessions.update (SessionsRetryTaskSection workspace True "one") failed |> Tuple.first
            in
            Expect.equal {pages=["active:one","terminated:two"],generation=state.generation,delivery=failed.sessionDelivery}
                {pages=Dict.keys retried.sessions.taskPages,generation=retried.sessions.generation,delivery=retried.sessionDelivery}
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
        entry target value = {workspace=workspace,target=target,dirty=0,settled=0,data=Just value,ranges=[],error=Nothing,proven=True,incarnation=0,retry=Delivery.Available}
        ready session =
            let initial=Delivery.init
                range={session=session,offset=0,nextOffset=3,dataBase64="e30K",hasMore=True}
                legacy=entry legacyTarget (Delivery.Bytes range)
                resources=[entry detailTarget (Delivery.Metadata session),entry listTarget (Delivery.SessionsPage {items=[session],nextCursor=Nothing}),entry windowTarget (Delivery.Bytes range),{legacy | ranges=[range]}]
            in {initial | resources=Dict.fromList (List.map (\resource -> (Delivery.key workspace resource.target,resource)) resources),order=List.map (.target >> Delivery.key workspace) resources}
        payload target state = Dict.get (Delivery.key workspace target) state.resources |> Maybe.andThen .data
        requestFor target requests = List.filter (.target >> (==) target) requests |> List.head
    in
    [ test "membership read barriers reject old responses without retiring download authority" <| withMetadata (\session ->
        let
            (busy,requests)=Delivery.demand workspace detailTarget Delivery.init |> Delivery.admit workspace
            refreshed=Delivery.invalidateReadProof workspace busy
            rejected=requestFor detailTarget requests |> Maybe.map (\request -> Delivery.settle request.id True (Ok (Delivery.Metadata session)) refreshed) |> Maybe.withDefault refreshed
        in Expect.equal {authority=busy.retirement,proof=True,occupied=1,rejected=Nothing,released=0}
            {authority=refreshed.retirement,proof=refreshed.proofRetirement>busy.proofRetirement,occupied=Dict.size refreshed.physical,rejected=payload detailTarget rejected,released=Dict.size rejected.physical})
    , test "membership preserves a task archive while resync and permission revocation still retire it" <| \_ ->
        let
            initial=Sessions.reconcile (dockFixture,Cmd.none) |> Tuple.first
            state=initial.sessions
            operation=SessionArchiveOperation 1 "one" "Task evidence" "picking"
            picking={initial | sessions={state | archive=Just operation,nextOperation=2}}
            membership=Sessions.reconcile ({picking | sessionDelivery=Delivery.invalidateReadProof workspace picking.sessionDelivery},Cmd.none) |> Tuple.first
            resync=Sessions.reconcile ({picking | sessionDelivery=Delivery.invalidateWorkspace workspace picking.sessionDelivery},Cmd.none) |> Tuple.first
            context=initial.sessionContext |> Maybe.map (\value -> {value | globalPermissions={createWorkspace=False,superadmin=False},workspace=Maybe.map (\grant -> {grant | canRead=False,canEdit=False}) value.workspace})
            revoked=Sessions.reconcile ({picking | sessionContext=context},Cmd.none) |> Tuple.first
        in Expect.equal {kept=Just operation,stable=True,resync=Nothing,revoked=Nothing}
            {kept=membership.sessions.archive,stable=membership.sessions.downloadStamp==state.downloadStamp,resync=resync.sessions.archive,revoked=revoked.sessions.archive}
    , test "progress coalesces selected metadata and patches proven pages without invalidating membership" <| withMetadata (\session ->
        let
            initial = ready session
            dirty = List.foldl (\_ -> Delivery.invalidateProgress workspace sessionId) initial (List.range 1 100)
            (busy,requests) = Delivery.admit workspace dirty
            nextSession = {session | committedBytes=session.committedBytes+3,revision=session.revision+1,runtimeRevision=2}
            settled = requestFor detailTarget requests |> Maybe.map (\request -> Delivery.settle request.id True (Ok (Delivery.Metadata nextSession)) busy) |> Maybe.withDefault busy
        in Expect.equal {requests=[detailTarget],epoch=initial.listInvalidation,page=Just (Delivery.SessionsPage {items=[nextSession],nextCursor=Nothing}),physical=0}
            {requests=List.map .target requests,epoch=settled.listInvalidation,page=payload listTarget settled,physical=Dict.size settled.physical})
    , test "equal transcript revision accepts newer runtime metadata while stale runtime cannot roll it back" <| withMetadata (\session ->
        let
            initial = ready session
            update value current =
                let (busy,requests) = Delivery.invalidateProgress workspace sessionId current |> Delivery.admit workspace
                in requestFor detailTarget requests |> Maybe.map (\request -> Delivery.settle request.id True (Ok (Delivery.Metadata value)) busy) |> Maybe.withDefault busy
            newer = {session | runtimeRevision=2}
            settled = update session (update newer initial)
        in Expect.equal (Just (Delivery.Metadata newer),Just (Delivery.SessionsPage {items=[newer],nextCursor=Nothing}))
            (payload detailTarget settled,payload listTarget settled))
    , test "opt-in activity progress preserves known Live members and freshness-independent unions" <| withMetadata (\session ->
        let
            dirty activity contains =
                let target=Delivery.WorkspaceSessionsFiltered {state="",freshness="",activity=Just activity} Nothing
                    seed=ready session
                    seeded={seed | resources=Dict.insert (Delivery.key workspace target) (entry target (Delivery.SessionsPage {items=if contains then [session] else [],nextCursor=Nothing})) seed.resources}
                    changed=Delivery.invalidateProgress workspace sessionId seeded
                in Dict.get (Delivery.key workspace target) changed.resources |> Maybe.map (\resource -> resource.dirty/=resource.settled) |> Maybe.withDefault False
            activities=["","live","quiet","completed","live,quiet","live,completed","quiet,completed","live,quiet,completed"]
        in Expect.equal [(False,False),(False,True),(True,True),(False,False),(False,False),(False,True),(True,True),(False,False)]
            (List.map (\activity -> (dirty activity True,dirty activity False)) activities))
    , test "unselected progress refreshes only its displayed pages and freshness membership" <| withMetadata (\session ->
        let
            fresh = Delivery.WorkspaceSessionsFiltered {state="active",freshness="fresh",activity=Nothing} Nothing
            unrelated = Delivery.TaskSessions "unrelated" Nothing
            seed = ready session |> Delivery.removeDemand workspace windowTarget |> Delivery.removeDemand workspace legacyTarget |> Delivery.removeDemand workspace detailTarget
            seeded = {seed | resources=seed.resources |> Dict.insert (Delivery.key workspace fresh) (entry fresh (Delivery.SessionsPage {items=[],nextCursor=Nothing})) |> Dict.insert (Delivery.key workspace unrelated) (entry unrelated (Delivery.SessionsPage {items=[],nextCursor=Nothing})), order=seed.order ++ List.map (Delivery.key workspace) [fresh,unrelated]}
            dirty = Delivery.invalidateProgress workspace sessionId seeded
            (_,requests) = Delivery.admit workspace dirty
        in Expect.equal ([listTarget,fresh],seed.listInvalidation) (List.map .target requests,dirty.listInvalidation))
    , test "metadata and list callback races cannot rewind canonical heartbeat fractions" <| withMetadata (\session ->
        let
            newer = {session | lastSeenAt="2026-10-23T20:46:19.5Z"}
            initial = ready newer
            (busy,requests) = Delivery.invalidateProgress workspace sessionId initial |> Delivery.admit workspace
            old = {session | lastSeenAt="2026-10-23T20:46:19Z"}
            settled = requestFor detailTarget requests |> Maybe.map (\request -> Delivery.settle request.id True (Ok (Delivery.Metadata old)) busy) |> Maybe.withDefault busy
        in Expect.equal (Just (Delivery.Metadata newer),Just (Delivery.SessionsPage {items=[newer],nextCursor=Nothing}))
            (payload detailTarget settled,payload listTarget settled))
    , test "same-cursor occurrence completion refines pending metadata and rejects stale pending or older replies" <| withMetadata (\session ->
        let
            completed={session | lastActivityAt=Just "2026-10-23T20:46:19.5Z"}
            update value current =
                let (busy,requests)=Delivery.invalidateProgress workspace sessionId current |> Delivery.admit workspace
                in requestFor detailTarget requests |> Maybe.map (\request -> Delivery.settle request.id True (Ok (Delivery.Metadata value)) busy) |> Maybe.withDefault busy
            refined=update completed (ready session)
            old={completed | lastActivityAt=Just "2026-10-23T20:46:19Z"}
            settled=update old (update session refined)
        in Expect.equal (Just (Delivery.Metadata completed),Just (Delivery.SessionsPage {items=[completed],nextCursor=Nothing}))
            (payload detailTarget settled,payload listTarget settled))
    , test "a newer committed cursor may explicitly await its canonical occurrence projection" <| withMetadata (\session ->
        let
            known={session | lastActivityAt=Just "2026-10-23T20:46:19Z"}
            pending={session | committedBytes=session.committedBytes+3,revision=session.revision+1,lastActivityAt=Nothing}
            (busy,requests)=Delivery.invalidateProgress workspace sessionId (ready known) |> Delivery.admit workspace
            settled=requestFor detailTarget requests |> Maybe.map (\request -> Delivery.settle request.id True (Ok (Delivery.Metadata pending)) busy) |> Maybe.withDefault busy
        in Expect.equal (Just (Delivery.Metadata pending)) (payload detailTarget settled))
    , test "fresh metadata revealing a title or lifecycle change conservatively rechecks list membership" <| withMetadata (\session ->
        let
            (busy,requests) = Delivery.invalidateProgress workspace sessionId (ready session) |> Delivery.admit workspace
            settled = requestFor detailTarget requests |> Maybe.map (\request -> Delivery.settle request.id True (Ok (Delivery.Metadata {session | title=Just "Changed title"})) busy) |> Maybe.withDefault busy
            (_,next) = Delivery.admit workspace settled
        in Expect.equal (True,True) (settled.listInvalidation>busy.listInvalidation,List.any (.target >> (==) listTarget) next))
    , test "a selected fresh member reuses metadata while quiet and missing-fresh membership recheck" <| withMetadata (\session ->
        let
            fresh = Delivery.WorkspaceSessionsFiltered {state="active",freshness="fresh",activity=Nothing} Nothing
            quiet = Delivery.TaskSessionsFiltered "task" {state="active",freshness="stale",activity=Nothing} Nothing
            initial = ready session
            seeded = {initial | resources=initial.resources |> Dict.insert (Delivery.key workspace fresh) (entry fresh (Delivery.SessionsPage {items=[session],nextCursor=Nothing})) |> Dict.insert (Delivery.key workspace quiet) (entry quiet (Delivery.SessionsPage {items=[session],nextCursor=Nothing})),order=initial.order ++ List.map (Delivery.key workspace) [fresh,quiet]}
            dirty = Delivery.invalidateProgress workspace sessionId seeded
            isDirty target = Dict.get (Delivery.key workspace target) dirty.resources |> Maybe.map (\value -> value.dirty /= value.settled)
        in Expect.equal (Just False,Just True) (isDirty fresh,isDirty quiet))
    , test "first detail metadata detects structural changes against proven list entries" <| withMetadata (\session ->
        let
            initial = ready session |> Delivery.removeDemand workspace windowTarget |> Delivery.removeDemand workspace legacyTarget |> Delivery.removeDemand workspace detailTarget |> Delivery.demand workspace detailTarget
            changed value =
                let (busy,requests) = Delivery.admit workspace initial
                    settled = requestFor detailTarget requests |> Maybe.map (\request -> Delivery.settle request.id True (Ok (Delivery.Metadata value)) busy) |> Maybe.withDefault busy
                in settled.listInvalidation > initial.listInvalidation && (Dict.get (Delivery.key workspace listTarget) settled.resources |> Maybe.map (\entryValue -> entryValue.dirty /= entryValue.settled) |> Maybe.withDefault False)
        in Expect.equal [True,True,True] (List.map changed [{session | title=Just "New title"},{session | parentSessionId=Just "parent"},{session | state=Api.SessionTerminated}]))
    , test "first detail and list callbacks cannot regress each other's runtime revision" <| withMetadata (\session ->
        let
            newer = {session | runtimeRevision=2}
            initial = ready newer |> Delivery.removeDemand workspace windowTarget |> Delivery.removeDemand workspace legacyTarget |> Delivery.removeDemand workspace detailTarget |> Delivery.demand workspace detailTarget
            (busy,requests) = Delivery.admit workspace initial
            stale = requestFor detailTarget requests |> Maybe.map (\request -> Delivery.settle request.id True (Ok (Delivery.Metadata session)) busy) |> Maybe.withDefault busy
            detail = Dict.get (Delivery.key workspace detailTarget) stale.resources
            known = ready newer
            oldPage = {known | resources=Dict.insert (Delivery.key workspace listTarget) (entry listTarget (Delivery.SessionsPage {items=[session],nextCursor=Nothing})) known.resources}
            refresh = Delivery.invalidateLists workspace oldPage
            (listBusy,listRequests) = Delivery.admit workspace refresh
            listStale = requestFor listTarget listRequests |> Maybe.map (\request -> Delivery.settle request.id True (Ok (Delivery.SessionsPage {items=[session],nextCursor=Nothing})) listBusy) |> Maybe.withDefault listBusy
        in Expect.equal {detail=Nothing,proven=Just False,listPending=Just True}
            {detail=payload detailTarget stale,proven=Maybe.map .proven detail,listPending=Dict.get (Delivery.key workspace listTarget) listStale.resources |> Maybe.map (\currentEntry -> currentEntry.dirty /= currentEntry.settled)})
    , test "foreground admissions yield to the oldest background request after three turns" <| withMetadata (\session ->
        let
            wanted = List.range 1 6 |> List.foldl (\id -> Delivery.demand workspace (Delivery.SessionMetadata (String.fromInt id))) (Delivery.demand workspace listTarget Delivery.init)
            (first,firstRequests) = Delivery.admit workspace wanted
            drained = List.foldl (\request -> Delivery.settle request.id True (Err Http.Timeout)) first firstRequests
            (_,secondRequests) = Delivery.admit workspace drained
        in Expect.equal ([Delivery.SessionMetadata "1",Delivery.SessionMetadata "2"],[Delivery.SessionMetadata "3",listTarget])
            (List.map .target firstRequests,List.map .target secondRequests))
    , test "continuous output admits a canonical read prefix while retaining newer refresh demand" <| withMetadata (\session ->
        let
            initial=Delivery.demand workspace windowTarget Delivery.init
            (busy,requests)=Delivery.admit workspace initial
            dirty=List.foldl (\_ -> Delivery.invalidateSession workspace sessionId) busy (List.range 1 100)
            settled=requestFor detailTarget requests |> Maybe.map (\request -> Delivery.settle request.id True (Ok (Delivery.Metadata session)) dirty) |> Maybe.withDefault dirty
            (_,next)=Delivery.admit workspace settled
            detail=Dict.get (Delivery.key workspace detailTarget) settled.resources
        in Expect.equal (True,Just True,Just True)
            (List.any (.target >> (==) windowTarget) next,Maybe.map .proven detail,Maybe.map (\currentEntry -> currentEntry.dirty /= currentEntry.settled) detail))
    , test "prefix admission cannot regress sealed state, transcript revision or runtime revision" <| withMetadata (\session ->
        let
            sealed={session | state=Api.SessionTerminated,runtimeRevision=2}
            (busy,requests)=Delivery.invalidateSession workspace sessionId (ready sealed) |> Delivery.admit workspace
            dirty=Delivery.invalidateSession workspace sessionId busy
            settled=requestFor detailTarget requests |> Maybe.map (\request -> Delivery.settle request.id True (Ok (Delivery.Metadata session)) dirty) |> Maybe.withDefault dirty
        in Expect.equal (Just (Delivery.Metadata sealed),Just Nothing) (payload detailTarget settled,Dict.get (Delivery.key workspace detailTarget) settled.resources |> Maybe.map .error))
    , test "canonical refresh retains bounded metadata display but clears bytes and waits for fresh read authority" <| withMetadata (\session ->
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
        let initial={session | runtimeIdentity=Just {model="Previous runtime",provider=Nothing,reasoning=Nothing,role=Nothing,harness=Nothing}}
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
    in {entered | tasks=Dict.fromList (List.map (\id -> (id,{id=id,workspaceId=workspace,projectId=Nothing,parentId=Nothing,title=id,description=Nothing,status=Api.InProgress,priority=1,completedAt=Nothing,dependencyCount=0,memoryLinkCount=0,createdAt="",updatedAt=""})) ["one","two"]), sessionDelivery=List.foldl (\request delivery -> Delivery.settle request.id True (Ok (Delivery.SessionsPage all)) delivery) entered.sessionDelivery (Dict.values entered.sessionDelivery.physical), sessions={state | taskPages=Dict.fromList [("active:one",cached),("active:two",cached)],taskAssociations=Dict.fromList (List.map (\id -> (id,{present=True,error=False,epoch=entered.sessionDelivery.listInvalidation})) ["one","two"]),taskHints=Dict.fromList [("one",{active=True,error=False,epoch=entered.sessionDelivery.listInvalidation}),("two",{active=True,error=False,epoch=entered.sessionDelivery.listInvalidation})]}}


dockTests : List Test.Test
dockTests =
    [ test "settled same-authority task/full return keeps bounded viewer presentation and owner fences" <| \_ ->
        let
            dock = Sessions.update (SessionsToggleDock "one") dockFixture |> Sessions.reconcile |> Tuple.first
            state = dock.sessions
            reading = {dock | sessions={state | start=Just 0,offset=32,end=32,rows=[sampleRecord],following=False,resetPending=False}}
            full = Sessions.reconcile ({reading | activeTab=SessionsTab},Cmd.none) |> Tuple.first
            returned = Sessions.reconcile ({full | activeTab=ProjectsTab},Cmd.none) |> Tuple.first
            retired = Sessions.reconcile ({full | sessionRequestEpoch=full.sessionRequestEpoch+1,activeTab=ProjectsTab},Cmd.none) |> Tuple.first
        in
        Expect.equal {full=[sampleRecord],returned=[sampleRecord],following=False,owner=True,retired=[]}
            {full=full.sessions.rows,returned=returned.sessions.rows,following=returned.sessions.following,owner=full.sessions.stamp/=returned.sessions.stamp,retired=retired.sessions.rows}
    , test "pending same-authority handoff carries accepted display but replays transport after positioning" <| \_ ->
        let
            run reset =
                let
                    dock=Sessions.update (SessionsToggleDock "one") dockFixture |> Sessions.reconcile |> Tuple.first
                    state=dock.sessions
                    delivery=dock.sessionDelivery
                    metadata=Decode.decodeString Api.sessionPageDecoder canonicalPage |> Result.toMaybe |> Maybe.andThen (.items >> List.head)
                    metadataTarget=Delivery.SessionMetadata sessionId
                    resource={workspace=workspace,target=metadataTarget,dirty=0,settled=0,data=Maybe.map Delivery.Metadata metadata,ranges=[],error=Nothing,proven=True,incarnation=0,retry=Delivery.Available}
                    reading={dock | sessionDelivery={delivery | resources=Dict.insert (Delivery.key workspace metadataTarget) resource delivery.resources},sessions={state | start=Just 0,offset=32,end=32,loadWindow=True,rows=[sampleRecord],following=False,manualUp=True,pending=not reset,resetPending=reset,historyAnchor="reading",presentationAnchor="fragment",historyOpen=Set.singleton "opened"}}
                    full=Sessions.reconcile ({reading | activeTab=SessionsTab},Cmd.none) |> Tuple.first
                    packet stamp generation kind=SessionsBridgeEvent (Encode.object [("kind",Encode.string kind),("stamp",Encode.string stamp),("generation",Encode.int generation),("publication",Encode.int state.windowPublication),("through",Encode.int 32),("rows",Encode.list identity [])])
                    stale=Sessions.update (packet state.stamp state.generation "viewer") full |> Tuple.first
                    positioned=Sessions.update (packet full.sessions.stamp full.sessions.generation "viewer-positioned") stale |> Sessions.reconcile |> Tuple.first
                    retired=Sessions.reconcile ({reading | activeTab=SessionsTab,sessionRequestEpoch=reading.sessionRequestEpoch+1},Cmd.none) |> Tuple.first
                    windowOwned current=List.any (\(_,target)->case target of
                        Delivery.SessionWindow _ _ _ _ -> True
                        _ -> False) current.sessions.owned
                in
                Expect.all
                    [\_ -> Expect.equal {rows=[sampleRecord],following=False,manual=True,anchor="reading",fragment="fragment",open=Set.singleton "opened"} {rows=full.sessions.rows,following=full.sessions.following,manual=full.sessions.manualUp,anchor=full.sessions.historyAnchor,fragment=full.sessions.presentationAnchor,open=full.sessions.historyOpen}
                    ,\_ -> Expect.equal (0,True,False) (full.sessions.offset,full.sessions.positionPending,windowOwned full)
                    ,\_ -> Expect.equal [sampleRecord] stale.sessions.rows
                    ,\_ -> Expect.equal True (windowOwned positioned)
                    ,\_ -> Expect.equal [] retired.sessions.rows
                    ,\_ -> Expect.equal Set.empty (Set.diff (Dict.keys reading.sessionDelivery.physical |> Set.fromList) (Dict.keys full.sessionDelivery.physical |> Set.fromList))
                    ] ()
        in Expect.all [\_ -> run False,\_ -> run True] ()
    , test "invalidated membership cannot admit a new viewer or manually select a stale card" <| \_ ->
        let
            initial = dockFixture
            stale = {initial | sessionDelivery=Delivery.invalidateWorkspace workspace initial.sessionDelivery}
            opened = Sessions.update (SessionsToggleDock "one") stale |> Sessions.reconcile |> Tuple.first
            admitted = Sessions.update (SessionsToggleDock "one") initial |> Sessions.reconcile |> Tuple.first
            refreshed = {admitted | sessionDelivery=Delivery.invalidateWorkspace workspace admitted.sessionDelivery}
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


associationFixture : Model
associationFixture =
    let base = dockFixture
        state = base.sessions
    in {base | sessions={state | taskHints=Dict.empty,taskAssociations=Dict.empty,taskPages=Dict.empty,owned=[],taskPanel=Nothing,selected=Nothing},sessionDelivery=Delivery.init}


driveAssociations : List Api.AgentSession -> Result Http.Error (List Api.AgentSession) -> Model -> (Model,List Delivery.Target)
driveAssociations active associated model =
    let advance _ (current,targets) =
            let requested = Sessions.reconcile (current,Cmd.none) |> Tuple.first
                requests = Dict.values requested.sessionDelivery.physical
                reply request = case request.target of
                    Delivery.TaskSessionsFiltered _ filters _ ->
                        (if filters.state == "active" then Ok active else associated) |> Result.map (\items -> Delivery.SessionsPage {items=items,nextCursor=Nothing})
                    _ -> Ok (Delivery.SessionsPage {items=[],nextCursor=Nothing})
                settled = List.foldl (\request delivery -> Delivery.settle request.id True (reply request) delivery) requested.sessionDelivery requests
            in ({requested | sessionDelivery=settled},targets ++ List.map .target requests)
    in List.foldl advance (model,[]) (List.range 1 12)


associationTests : List Test.Test
associationTests =
    let sample = Decode.decodeString Api.sessionPageDecoder canonicalPage |> Result.toMaybe |> Maybe.andThen (.items >> List.head)
        presence model = Dict.get "one" model.sessions.taskAssociations |> Maybe.andThen (\hint -> if hint.error || hint.epoch /= model.sessionDelivery.listInvalidation then Nothing else Just hint.present)
        withSession check = \_ -> sample |> Maybe.map check |> Maybe.withDefault (Expect.fail "Invalid session fixture")
        allState target = case target of
            Delivery.TaskSessionsFiltered _ filters _ -> filters.state == ""
            _ -> False
    in
    [ test "active positive proves association without a second all-state read" <| withSession (\session ->
        let (ready,targets) = driveAssociations [session] (Ok []) associationFixture
        in Expect.equal (Just True,True,0) (presence ready,Sessions.taskActive workspace ready "one",List.filter allState targets |> List.length))
    , test "an active-negative page alone is unresolved rather than proof of no association" <| \_ ->
        let entered = Sessions.reconcile (associationFixture,Cmd.none) |> Tuple.first
            settled = List.foldl (\request delivery -> Delivery.settle request.id True (Ok (Delivery.SessionsPage {items=[],nextCursor=Nothing})) delivery) entered.sessionDelivery (Dict.values entered.sessionDelivery.physical)
            unresolved = Sessions.reconcile ({entered | sessionDelivery=settled},Cmd.none) |> Tuple.first
        in Expect.all
            [ \_ -> Expect.equal Nothing (presence unresolved)
            , \_ -> Query.fromHtml (Sessions.viewTaskIcon workspace unresolved "one") |> Query.has [Selector.tag "button",Selector.attribute (Html.Attributes.disabled True)]
            , \_ -> Query.fromHtml (Sessions.viewTaskIcon workspace unresolved "one") |> Query.hasNot [Selector.class "session-state-inactive"]
            ] ()
    , test "current complete empty all-state direct membership omits the session control" <| \_ ->
        let (ready,_) = driveAssociations [] (Ok []) associationFixture
            toggled = Sessions.update (SessionsToggleDock "one") ready |> Tuple.first
        in Expect.all
            [ \_ -> Expect.equal (Just False,Nothing) (presence ready,toggled.sessions.taskPanel)
            , \_ -> Query.fromHtml (Sessions.viewTaskIcon workspace ready "one") |> Query.hasNot [Selector.tag "button"]
            ] ()
    , test "an empty all-state page with a continuation never proves absent association" <| \_ ->
        let entered=Sessions.reconcile (associationFixture,Cmd.none) |> Tuple.first
            settled=List.foldl (\request delivery -> Delivery.settle request.id True (Ok (Delivery.SessionsPage {items=[],nextCursor=Nothing})) delivery) entered.sessionDelivery (Dict.values entered.sessionDelivery.physical)
            allBusy=Sessions.reconcile ({entered | sessionDelivery=settled},Cmd.none) |> Tuple.first
            partial=List.foldl (\request delivery -> Delivery.settle request.id True (Ok (Delivery.SessionsPage {items=[],nextCursor=Just {createdAt="2026-10-24T00:00:00Z",id="older"}})) delivery) allBusy.sessionDelivery (Dict.values allBusy.sessionDelivery.physical)
            final=Sessions.reconcile ({allBusy | sessionDelivery=partial},Cmd.none) |> Tuple.first
        in Expect.all
            [ \_ -> Expect.equal Nothing (presence final)
            , \_ -> Query.fromHtml (Sessions.viewTaskIcon workspace final "one") |> Query.has [Selector.tag "button"]
            , \_ -> Expect.equal Nothing (Sessions.taskEvidenceAvailable workspace final "one")
            ] ()
    , test "terminated-only association retains an enabled grey toggle without an inline terminated reader" <| withSession (\session ->
        let (ready,_) = driveAssociations [] (Ok [{session | state=Api.SessionTerminated}]) associationFixture
            toggled = Sessions.update (SessionsToggleDock "one") ready |> Tuple.first
        in Expect.all
            [ \_ -> Expect.equal {present=Just True,active=False,panel=Just "one",selected=Nothing} {present=presence ready,active=Sessions.taskActive workspace ready "one",panel=toggled.sessions.taskPanel,selected=toggled.sessions.selected}
            , \_ -> Query.fromHtml (Sessions.viewTaskIcon workspace ready "one") |> Query.has [Selector.class "session-state-inactive"]
            , \_ -> Query.fromHtml (Sessions.viewTaskIcon workspace ready "one") |> Query.has [Selector.text "View Agents"]
            ] ())
    , test "all-state error stays unresolved and never becomes a confirmed grey or empty result" <| \_ ->
        let (ready,_) = driveAssociations [] (Err (Http.BadStatus 503)) associationFixture
        in Expect.all
            [ \_ -> Expect.equal Nothing (presence ready)
            , \_ -> Query.fromHtml (Sessions.viewTaskIcon workspace ready "one") |> Query.has [Selector.tag "button"]
            , \_ -> Query.fromHtml (Sessions.viewTaskIcon workspace ready "one") |> Query.hasNot [Selector.class "session-state-inactive"]
            ] ()
    , test "retained dirty empty membership cannot be restamped current after invalidation" <| \_ ->
        let (ready,_) = driveAssociations [] (Ok []) associationFixture
            invalid = {ready | sessionDelivery=Delivery.invalidateReadProof workspace ready.sessionDelivery}
            checking = Sessions.reconcile (invalid,Cmd.none) |> Tuple.first
        in Expect.equal Nothing (presence checking)
    , test "ordinary list invalidation makes both stored empty and stored positive association proofs unresolved" <| withSession (\session ->
        let check items =
                let (ready,_) = driveAssociations [] (Ok items) associationFixture
                    invalid = {ready | sessionDelivery=Delivery.invalidateLists workspace ready.sessionDelivery}
                    checking = Sessions.reconcile (invalid,Cmd.none) |> Tuple.first
                in presence checking
        in Expect.equal [Nothing,Nothing] [check [],check [{session | state=Api.SessionTerminated}]])
    , test "active to terminated retains association while clearing the active indicator" <| withSession (\session ->
        let (active,_) = driveAssociations [session] (Ok []) associationFixture
            invalid = {active | sessionDelivery=Delivery.invalidateLists workspace active.sessionDelivery}
            (inactive,_) = driveAssociations [] (Ok [{session | state=Api.SessionTerminated}]) invalid
        in Expect.equal (Just True,False) (presence inactive,Sessions.taskActive workspace inactive "one"))
    , test "dirty retained active presentation cannot admit a closed dock and remains labelled refreshing" <| withSession (\session ->
        let (ready,_) = driveAssociations [session] (Ok []) associationFixture
            checking = Sessions.reconcile ({ready | sessionDelivery=Delivery.invalidateLists workspace ready.sessionDelivery},Cmd.none) |> Tuple.first
            clicked = Sessions.update (SessionsToggleDock "one") checking |> Tuple.first
        in Expect.all
            [ \_ -> Expect.equal (Nothing,Nothing,True) (presence checking,clicked.sessions.taskPanel,Sessions.taskActive workspace checking "one")
            , \_ -> Query.fromHtml (Sessions.viewTaskIcon workspace checking "one") |> Query.has [Selector.tag "button",Selector.attribute (Html.Attributes.disabled True)]
            , \_ -> Query.fromHtml (Sessions.viewTaskIcon workspace checking "one") |> Query.has [Selector.attribute (Html.Attributes.title "View Agents · active sessions · refreshing")]
            ] ())
    , test "an open inactive dock stays unresolved while its active refresh is held despite fresh positive association" <| withSession (\session ->
        let (ready,_) = driveAssociations [] (Ok [{session | state=Api.SessionTerminated}]) associationFixture
            opened = Sessions.update (SessionsToggleDock "one") ready |> Tuple.first
            waiting = Sessions.reconcile ({opened | sessionDelivery=Delivery.invalidateLists workspace opened.sessionDelivery},Cmd.none) |> Tuple.first
            state = waiting.sessions
            -- The independent all-state first page has confirmed direct membership;
            -- its result cannot confirm activity while the retained active page is dirty.
            associated = {waiting | sessions={state | taskAssociations=Dict.insert "one" {present=True,error=False,epoch=waiting.sessionDelivery.listInvalidation} state.taskAssociations}}
            checking = Sessions.reconcile (associated,Cmd.none) |> Tuple.first
            settled = List.foldl (\request delivery -> Delivery.settle request.id True (Ok (Delivery.SessionsPage {items=[],nextCursor=Nothing})) delivery) checking.sessionDelivery (Dict.values checking.sessionDelivery.physical)
            confirmed = Sessions.reconcile ({checking | sessionDelivery=settled},Cmd.none) |> Tuple.first
        in Expect.all
            [ \_ -> Expect.equal (Just "one",Just True) (checking.sessions.taskPanel,presence checking)
            , \_ -> Expect.notEqual (Just checking.sessionDelivery.listInvalidation) (Dict.get "one" checking.sessions.taskHints |> Maybe.map .epoch)
            , \_ -> Query.fromHtml (Sessions.viewTaskIcon workspace checking "one") |> Query.hasNot [Selector.class "session-state-inactive"]
            , \_ -> Query.fromHtml (Sessions.viewTaskIcon workspace checking "one") |> Query.has [Selector.text "Hide Agents"]
            , \_ -> Query.fromHtml (Sessions.viewTaskIcon workspace checking "one") |> Query.has [Selector.attribute (Html.Attributes.title "Hide Agents · checking associations and activity")]
            , \_ -> Query.fromHtml (Sessions.viewTaskActiveChoices workspace checking "one") |> Query.hasNot [Selector.text "No active sessions for this task."]
            , \_ -> Query.fromHtml (Sessions.viewTaskActiveChoices workspace checking "one") |> Query.has [Selector.attribute (Html.Attributes.attribute "role" "status"), Selector.attribute (Html.Attributes.attribute "aria-label" "Loading active sessions")]
            , \_ -> Query.fromHtml (Sessions.viewTaskIcon workspace confirmed "one") |> Query.has [Selector.class "session-state-inactive"]
            , \_ -> Query.fromHtml (Sessions.viewTaskActiveChoices workspace confirmed "one") |> Query.has [Selector.text "No active sessions for this task."]
            ] ())
    , test "current proven empty beats stale active presentation but an already open dock can always close" <| withSession (\session ->
        let (empty,_) = driveAssociations [] (Ok []) associationFixture
            state = empty.sessions
            stale = {empty | sessions={state | taskHints=Dict.insert "one" {active=True,error=False,epoch=empty.sessionDelivery.listInvalidation-1} state.taskHints}}
            denied = Sessions.update (SessionsToggleDock "one") stale |> Tuple.first
            opened = {stale | sessions={state | taskPanel=Just "one",taskAssociations=Dict.empty}}
            closed = Sessions.update (SessionsToggleDock "one") opened |> Tuple.first
        in Expect.all
            [ \_ -> Expect.equal (Nothing,Nothing) (denied.sessions.taskPanel,closed.sessions.taskPanel)
            , \_ -> Query.fromHtml (Sessions.viewTaskIcon workspace stale "one") |> Query.hasNot [Selector.tag "button"]
            , \_ -> Query.fromHtml (Sessions.viewTaskIcon workspace opened "one") |> Query.has [Selector.text "Hide Agents"]
            ] ())
    , test "authority retirement clears association hints and late old admissions cannot restore them" <| withSession (\session ->
        let (ready,_) = driveAssociations [] (Ok [{session | state=Api.SessionTerminated}]) associationFixture
            invalid = {ready | sessionDelivery=Delivery.invalidateLists workspace ready.sessionDelivery}
            busy = Sessions.reconcile (invalid,Cmd.none) |> Tuple.first
            requests = Dict.values busy.sessionDelivery.physical
            retired = Sessions.reconcile ({busy | sessionRequestEpoch=busy.sessionRequestEpoch+1,auth={status=AuthRequired,mode=Nothing}},Cmd.none) |> Tuple.first
            late = List.foldl (\request delivery -> Delivery.settle request.id False (Ok (Delivery.SessionsPage {items=[session],nextCursor=Nothing})) delivery) retired.sessionDelivery requests
            final = Sessions.reconcile ({retired | sessionDelivery=late},Cmd.none) |> Tuple.first
        in Expect.equal (0,0) (Dict.size final.sessions.taskAssociations,Dict.size final.sessionDelivery.physical))
    ]


evidenceAvailabilityTests : List Test.Test
evidenceAvailabilityTests =
    let
        base = associationFixture
        available model = Sessions.taskEvidenceAvailable workspace model "one"
        received source documents sessions =
            let
                state = base.documents
                value = DocumentCountsValue { sourceCount = source, documentEvidenceCount = documents, agentSessionEvidenceCount = sessions, evidenceTotal = documents + sessions }
                entry = { request = Nothing, dirty = False, failed = False, value = Just value, cursor = Nothing }
            in
            { base | documents = { state | scope = Documents.scope base, reads = Dict.singleton ( "one", "counts" ) entry } }
        empty = received 2 0 0
        positive = received 0 3 27
        selected model =
            let cards = model.cards in
            { model | cards = { cards | expandedCards = Dict.singleton "one" True, taskSections = Dict.singleton "one" "evidence" } }
        section model = Dict.get "one" model.cards.taskSections
    in
    [ test "availability is unknown before a canonical response and excludes source-only links" <| \_ ->
        Expect.equal [ Nothing, Just False, Just True ] (List.map available [ base, empty, positive ])
    , test "workspace or session epoch retirement cannot reuse canonical counts" <| \_ ->
        Expect.equal ( Nothing, Nothing )
            ( Sessions.taskEvidenceAvailable "foreign" empty "one", available { empty | sessionRequestEpoch = empty.sessionRequestEpoch + 1 } )
    , test "only a current zero falls back to Work and blocks manual selection" <| \_ ->
        let
            fallback = Sessions.reconcile (selected empty, Cmd.none) |> Tuple.first
            denied = Cards.update (SetTaskCardSection "one" "evidence") fallback |> Tuple.first
            retained = Sessions.reconcile (selected positive, Cmd.none) |> Tuple.first
        in
        Expect.equal [ Just "work", Just "work", Just "evidence" ] (List.map section [ fallback, denied, retained ])
    ]


completedReaderTests : List Test.Test
completedReaderTests =
    let
        metadata = Decode.decodeString Api.sessionPageDecoder canonicalPage |> Result.toMaybe |> Maybe.andThen (.items >> List.head)
        model phase =
            let
                entered = Sessions.reconcile (sessionModel,Cmd.none) |> Tuple.first
                delivery = entered.sessionDelivery
                target = Delivery.SessionMetadata sessionId
                resource = {workspace=workspace,target=target,dirty=0,settled=0,data=Maybe.map (\session -> Delivery.Metadata {session | state=Api.SessionTerminated}) metadata,ranges=[],error=Nothing,proven=True,incarnation=0,retry=Delivery.Available}
                state = entered.sessions
                sealed = state.sealed
            in {entered | sessionDelivery={delivery | resources=Dict.insert (Delivery.key workspace target) resource delivery.resources},sessions={state | taskPanel=Just "one",sealed={sealed | phase=phase,canRetry=True,canPrevious=False,canNext=True}}}
        workspaceView phase = Query.fromHtml (Sessions.view workspace (model phase))
    in
    [ test "verified conversation is a named region without presentation tabs" <| \_ ->
        Expect.all
            [\_ -> workspaceView "ready" |> Query.find [Selector.id "session-records"] |> Query.has [Selector.attribute (Html.Attributes.attribute "role" "region"),Selector.attribute (Html.Attributes.attribute "aria-label" "Verified archive records")]
            ,\_ -> workspaceView "ready" |> Query.hasNot [Selector.class "session-presentation-tabs"]
            ,\_ -> workspaceView "ready" |> Query.hasNot [Selector.attribute (Html.Attributes.attribute "role" "tabpanel")]] ()
    , test "sealed latest retains the verified reader lifetime while issuing a fresh jump request" <| \_ ->
        let ready=model "ready"
            state=ready.sessions
            next=Sessions.update SessionsJumpLatest ready |> Tuple.first |> .sessions
        in Expect.equal (state.generation,True,state.latestRequest+1) (next.generation,next.latestPending,next.latestRequest)
    , test "a finite handoff failure reports only the current reader authority" <| \_ ->
        let ready=model "ready"
            state=ready.sessions
            packet stamp generation=SessionsBridgeEvent (Encode.object [("kind",Encode.string "viewer-handoff-error"),("stamp",Encode.string stamp),("generation",Encode.int generation),("publication",Encode.int state.windowPublication)])
            apply stamp generation=Sessions.update (packet stamp generation) ready |> Tuple.first |> .sessions |> .error
        in Expect.equal [Nothing,Nothing,Just "Conversation position could not be restored. Please retry."] [apply "retired" state.generation,apply state.stamp (state.generation-1),apply state.stamp state.generation]
    , test "canceled handoff releases only its exact logical pause without claiming restoration or releasing transport" <| \_ ->
        let
            ready=model "ready"
            state=ready.sessions
            waiting={ready | sessions={state | positionPending=True,windowPublication=88,manualUp=True,following=False,loadWindow=True,historyAnchor="newer-reading"}}
            packet stamp generation publication kind=SessionsBridgeEvent (Encode.object [("kind",Encode.string kind),("stamp",Encode.string stamp),("generation",Encode.int generation),("publication",Encode.int publication)])
            apply stamp generation publication kind=Sessions.update (packet stamp generation publication kind) waiting |> Tuple.first
            final=apply state.stamp state.generation 88 "viewer-handoff-cancelled"
            newer={waiting | sessions={state | positionPending=True,windowPublication=89}}
            late=Sessions.update (packet state.stamp state.generation 88 "viewer-handoff-cancelled") newer |> Tuple.first
        in Expect.all
            [\_ -> Expect.equal [True,True,True,True] [ (apply "retired" state.generation 88 "viewer-handoff-cancelled").sessions.positionPending,(apply state.stamp (state.generation-1) 88 "viewer-handoff-cancelled").sessions.positionPending,(apply state.stamp state.generation 87 "viewer-handoff-cancelled").sessions.positionPending,late.sessions.positionPending]
            ,\_ -> Expect.equal {position=False,manual=True,following=False,load=True,anchor="newer-reading",error=Nothing,physical=Dict.keys waiting.sessionDelivery.physical} {position=final.sessions.positionPending,manual=final.sessions.manualUp,following=final.sessions.following,load=final.sessions.loadWindow,anchor=final.sessions.historyAnchor,error=final.sessions.error,physical=Dict.keys final.sessionDelivery.physical}
            ,\_ -> Expect.equal (True,Nothing) ((apply state.stamp state.generation 87 "viewer-handoff-error").sessions.positionPending,(apply state.stamp state.generation 87 "viewer-handoff-error").sessions.error)
            ,\_ -> Expect.equal (False,Just "Conversation position could not be restored. Please retry.") ((apply state.stamp state.generation 88 "viewer-handoff-error").sessions.positionPending,(apply state.stamp state.generation 88 "viewer-handoff-error").sessions.error)
            ] ()
    , test "a current complete lazy Markdown body acquires the same AST as an initial page" <| \_ ->
        let
            ready=model "ready"
            state=ready.sessions
            sealed=state.sealed
            key="0000000000000000000:0000000"
            piece={key=key,offset="0",kind="thinking",label="",role="assistant",time="",ast=Err "not_loaded",group="g",count="1",endTime="",open=True,groupOpen=True,body=Nothing}
            waiting={ready | sessions={state | sealed={sealed | rows=[piece]}}}
            packet stamp generation wanted part more format=SessionsBridgeEvent (Encode.object [("kind",Encode.string "sealed-body"),("stamp",Encode.string stamp),("generation",Encode.int generation),("workspace",Encode.string workspace),("session",Encode.string sessionId),("key",Encode.string wanted),("body",Encode.object [("text",Encode.string "[Open](https://example.test/normal)"),("part",Encode.int part),("more",Encode.bool more),("format",Encode.string format)])])
            apply stamp generation wanted part more format=Sessions.update (packet stamp generation wanted part more format) waiting |> Tuple.first |> .sessions |> .sealed |> .rows |> List.head |> Maybe.map (.ast >> Result.toMaybe >> (/=) Nothing)
        in Expect.equal [Just True,Just False,Just False,Just False,Just False,Just False,Just False]
            [apply state.stamp state.generation key 0 False "markdown",apply "retired" state.generation key 0 False "markdown",apply state.stamp (state.generation-1) key 0 False "markdown",apply state.stamp state.generation "missing" 0 False "markdown",apply state.stamp state.generation key 1 False "markdown",apply state.stamp state.generation key 0 True "markdown",apply state.stamp state.generation key 0 False "json"]
    , test "admitted Markdown parses once per body while overlap preserves the cached AST" <| \_ ->
        let
            ready = model "ready"
            state = ready.sessions
            key index = String.padLeft 19 '0' (String.fromInt index) ++ ":0000000"
            kept = "[Kept](https://example.test/kept)"
            piece index source more format opened =
                Encode.object
                    [ ("key",Encode.string (key index)),("offset",Encode.string (String.fromInt index))
                    , ("kind",Encode.string "text"),("role",Encode.string "assistant"),("label",Encode.string "")
                    , ("time",Encode.string ""),("group",Encode.string ""),("count",Encode.string "0")
                    , ("open",Encode.bool opened),("groupOpen",Encode.bool False)
                    , ("body",Encode.object [("text",Encode.string source),("part",Encode.int 0),("more",Encode.bool more),("format",Encode.string format)])
                    ]
            apply rows current =
                Sessions.update (SessionsBridgeEvent (Encode.object
                    [ ("kind",Encode.string "sealed-page"),("stamp",Encode.string state.stamp),("generation",Encode.int state.generation)
                    , ("workspace",Encode.string workspace),("session",Encode.string sessionId),("rows",Encode.list identity rows)
                    , ("first",Encode.string (key 1)),("last",Encode.string (key 3))
                    ])) current |> Tuple.first
            first = apply [piece 1 kept False "markdown" False] ready
            firstAst = List.head first.sessions.sealed.rows |> Maybe.map .ast
            overlap = apply [piece 1 "repeated page body" False "markdown" True,piece 2 "partial" True "markdown" False,piece 3 "{}" False "json" False] first
            cached = List.head overlap.sessions.sealed.rows
            blocked current = (current.sessions.sealed.error,List.length current.sessions.sealed.rows)
        in Expect.all
            [ \_ -> Expect.equal True (firstAst |> Maybe.andThen Result.toMaybe |> (/=) Nothing)
            , \_ -> Expect.equal (Just kept,firstAst,Just True) (cached |> Maybe.andThen (.body >> Maybe.map .text),Maybe.map .ast cached,Maybe.map .open cached)
            , \_ -> Expect.equal [Err "not_loaded",Err "not_loaded"] (List.drop 1 overlap.sessions.sealed.rows |> List.map .ast)
            , \_ -> Expect.equal (Just "archive_page_limit",0) (apply (List.repeat 201 (piece 1 kept False "markdown" False)) ready |> blocked)
            , \_ -> Expect.equal (Just "archive_page_limit",0) (apply [piece 1 (String.repeat 4194305 "x") False "markdown" False] ready |> blocked)
            , \_ -> Expect.equal (Just "archive_page_invalid",0) (apply [Encode.object []] ready |> blocked)
            ] ()
    , test "partial multilingual Markdown and acquired rows share the actual serialized UTF8 cap" <| \_ ->
        let
            ready=model "ready"
            state=ready.sessions
            sealed=state.sealed
            key index=String.padLeft 19 '0' (String.fromInt index) ++ ":0000000"
            content=String.repeat 900000 "x"
            piece index={key=key index,offset=String.fromInt index,kind="text",label="",role="assistant",time="",ast=Err "not_loaded",group="",count="0",endTime="",open=False,groupOpen=False,body=Just {text=content,part=0,more=False,tokens=[],format="markdown",structured=False,exception=False}}
            source=String.repeat 120000 "界\t\n"
            waiting={ready | sessions={state | following=True,sealed={sealed | rows=List.map piece [1,2,3],markdown={key=key 3,source="",ast=Err "not_loaded",structured=False,complete=False,nextPart=0,request=0}}}}
            event fields=SessionsBridgeEvent (Encode.object ([("stamp",Encode.string state.stamp),("generation",Encode.int state.generation),("workspace",Encode.string workspace),("session",Encode.string sessionId)]++fields))
            hydrate part current=Sessions.update (event [("kind",Encode.string "sealed-markdown-chunk"),("key",Encode.string (key 3)),("part",Encode.int part),("text",Encode.string (String.slice (part*16000) ((part+1)*16000) source)),("complete",Encode.bool False)]) current |> Tuple.first
            partial=List.foldl hydrate waiting (List.range 0 ((String.length source-1)//16000))
            bodyJson=Encode.object [("text",Encode.string content),("part",Encode.int 0),("more",Encode.bool False),("format",Encode.string "markdown")]
            incoming=Encode.object [("key",Encode.string (key 4)),("offset",Encode.string "4"),("kind",Encode.string "text"),("role",Encode.string "assistant"),("label",Encode.string ""),("time",Encode.string ""),("group",Encode.string ""),("count",Encode.string "0"),("open",Encode.bool False),("groupOpen",Encode.bool False),("body",bodyJson)]
            composed=Sessions.update (event [("kind",Encode.string "sealed-page"),("rows",Encode.list identity [incoming]),("first",Encode.string (key 4)),("last",Encode.string (key 4)),("representedBefore",Encode.string (key 4)),("representedAfter",Encode.string (key 4 ++ ":1"))]) partial |> Tuple.first
            bytes value=Encode.encode 0 value |> String.foldl (\char total -> let code=Char.toCode char in total+(if code<128 then 1 else if code<2048 then 2 else if code<65536 then 3 else 4)) 0
            retained current=2*bytes (Encode.string current.sessions.sealed.markdown.source)+List.sum (List.map (\row -> 1024+(row.body |> Maybe.map (.text >> Encode.string >> bytes) |> Maybe.withDefault 0)) current.sessions.sealed.rows)
        in Expect.all
            [\_ -> Expect.equal source partial.sessions.sealed.markdown.source
            ,\_ -> Expect.equal 2 (List.length partial.sessions.sealed.rows)
            ,\_ -> Expect.atMost 4194304 (retained partial)
            ,\_ -> Expect.equal 2 (List.length composed.sessions.sealed.rows)
            ,\_ -> Expect.atMost 4194304 (retained composed)
            ] ()
    , test "latest retries a retired sealed reader but cannot bypass a cleanup failure" <| \_ ->
        let
            failed=model "error"
            state=failed.sessions
            sealed=state.sealed
            retried=Sessions.update SessionsJumpLatest failed |> Tuple.first |> .sessions
            blocked={failed | sessions={state | sealed={sealed | canRetry=False,error=Just "Reload required"}}}
            refused=Sessions.update SessionsJumpLatest blocked |> Tuple.first |> .sessions
        in Expect.equal ((state.generation+1,True),(state.generation,False,Just "Reload required")) ((retried.generation,retried.latestPending),(refused.generation,refused.latestPending,refused.latestError))
    , test "matching latest render retires a historical anchor republished by an in-flight page" <| \_ ->
        let
            ready=model "ready"
            jumped=Sessions.update SessionsJumpLatest ready |> Tuple.first
            state=jumped.sessions
            packet kind latest generation fields=SessionsBridgeEvent (Encode.object ([("kind",Encode.string kind),("stamp",Encode.string state.stamp),("generation",Encode.int generation),("workspace",Encode.string workspace),("session",Encode.string sessionId),("latestRequest",Encode.int latest)]++fields))
            historical=Sessions.update (packet "sealed-follow" state.latestRequest state.generation [("manualUp",Encode.bool True),("fragmentAnchor",Encode.string "historical:content:0")]) jumped |> Tuple.first
            render latest generation current=Sessions.update (packet "sealed-latest-render" latest generation [("request",Encode.int 90)]) current |> Tuple.first
            stale=render (state.latestRequest-1) state.generation historical
            wrong=render state.latestRequest (state.generation-1) historical
            final=render state.latestRequest state.generation historical
            cancelled=Sessions.update (packet "sealed-latest-cancelled" state.latestRequest state.generation []) historical |> Tuple.first |> render state.latestRequest state.generation
        in Expect.equal
            {stale="historical:content:0",wrong="historical:content:0",final={anchor="",part=-1,request=90,pending=True,manual=True},cancelled="historical:content:0"}
            {stale=stale.sessions.presentationAnchor,wrong=wrong.sessions.presentationAnchor,final={anchor=final.sessions.presentationAnchor,part=final.sessions.sealed.renderPart,request=final.sessions.sealed.renderRequest,pending=final.sessions.latestPending,manual=final.sessions.manualUp},cancelled=cancelled.sessions.presentationAnchor}
    , test "sealed render requests retain their acknowledgement stamp and reject older handoffs" <| \_ ->
        let
            ready=model "ready"
            state=ready.sessions
            packet request part=SessionsBridgeEvent (Encode.object [("kind",Encode.string "sealed-render"),("stamp",Encode.string state.stamp),("generation",Encode.int state.generation),("workspace",Encode.string workspace),("session",Encode.string sessionId),("request",Encode.int request),("part",Encode.int part)])
            first=Sessions.update (packet 90 1) ready |> Tuple.first
            stale=Sessions.update (packet 89 2) first |> Tuple.first
            next=Sessions.update (packet 91 2) stale |> Tuple.first
        in Expect.equal ((1,90),(1,90),(2,91)) ((first.sessions.sealed.renderPart,first.sessions.sealed.renderRequest),(stale.sessions.sealed.renderPart,stale.sessions.sealed.renderRequest),(next.sessions.sealed.renderPart,next.sessions.sealed.renderRequest))
    , test "both continuous readers omit page-replacement search and offset controls" <| \_ ->
        Expect.all
            [ \_ -> workspaceView "ready" |> Query.hasNot [Selector.id "sealed-search"]
            , \_ -> workspaceView "ready" |> Query.hasNot [Selector.id "sealed-position"]
            , \_ -> Query.fromHtml (Sessions.viewTaskActiveConversation workspace (model "ready") "one") |> Query.hasNot [Selector.id "sealed-search"]
            , \_ -> Query.fromHtml (Sessions.viewTaskActiveConversation workspace (model "ready") "one") |> Query.hasNot [Selector.id "sealed-position"]
            ] ()
    , test "reload-only cleanup failure cannot offer Retry on either continuous reader" <| \_ ->
        let
            ready = model "ready"
            state = ready.sessions
            packet = Encode.object [("kind",Encode.string "sealed-error"),("stamp",Encode.string state.stamp),("generation",Encode.int state.generation),("workspace",Encode.string workspace),("session",Encode.string sessionId),("code",Encode.string "archive_cleanup_failed")]
            failed = Sessions.update (SessionsBridgeEvent packet) ready |> Tuple.first
        in
        Expect.all
            [ \_ -> Query.fromHtml (Sessions.view workspace failed) |> Query.hasNot [Selector.text "Retry"]
            , \_ -> Query.fromHtml (Sessions.view workspace failed) |> Query.has [Selector.text "Archive cleanup could not be confirmed. Reload this page before another archive."]
            , \_ -> Query.fromHtml (Sessions.viewTaskActiveConversation workspace failed "one") |> Query.hasNot [Selector.text "Retry"]
            ] ()
    , test "continuous workspace exposes accepted edge availability without page buttons and retains contextual recovery" <| \_ ->
        Expect.all
            [ \_ -> workspaceView "ready" |> Query.find [Selector.id "session-records"] |> Query.has [Selector.attribute (Html.Attributes.attribute "data-archive-can-next" "true")]
            , \_ -> workspaceView "ready" |> Query.hasNot [Selector.attribute (Html.Attributes.attribute "data-sealed-action" "previous")]
            , \_ -> workspaceView "ready" |> Query.hasNot [Selector.attribute (Html.Attributes.attribute "data-sealed-action" "next")]
            , \_ -> workspaceView "ready" |> Query.hasNot [Selector.text "Cancel"]
            , \_ -> workspaceView "ready" |> Query.hasNot [Selector.text "Retry"]
            , \_ -> workspaceView "downloading" |> Query.has [Selector.text "Cancel"]
            , \_ -> workspaceView "downloading" |> Query.hasNot [Selector.text "Retry"]
            , \_ -> workspaceView "cancelled" |> Query.has [Selector.text "Load archive again"]
            , \_ -> workspaceView "cancelled" |> Query.hasNot [Selector.text "Retry"]
            , \_ -> workspaceView "error" |> Query.has [Selector.text "Retry"]
            ] ()
    ]
