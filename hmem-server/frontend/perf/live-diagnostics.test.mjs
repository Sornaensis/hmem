import test from 'node:test'
import assert from 'node:assert/strict'
import { createLiveDiagnostics, serializeLiveDiagnostic } from './live-diagnostics.mjs'
import { waitForLiveSettle } from './harness.mjs'

const state = { dispatchTurnComplete: true, activeRequests: 0, loading: false, focused: false, anchorVisible: true, logicalNavigationComplete: true, followUpRequests: 1 }
function finish(d, start = 10) {
  d.dispatched(start)
  for (let i = 0; i < 10; i += 1) d.readiness(start + 2*i + 1, state, i > 0, { startedAt: start + 2*i, endedAt: start + 2*i + 0.5 })
  d.paintStarted(start+20); d.paintEnded(start+30, 0, true)
  d.settled(start+31); d.stable(start+32, start+532)
  return d.snapshot()
}
test('diagnostic retains distinct dispatch, proof, paint, settle and untimed stability intervals', () => {
  const d=createLiveDiagnostics();d.start(0)
  d.arrival(1,'navigation:branch',1);d.prepared(1,2,200,90);d.fulfilled(1,3,true)
  const receipt=finish(d)
  assert.deepEqual(receipt.intervals,{dispatchMs:10,settleMs:41,twoPaintMs:10,stabilityMs:500})
  assert.equal(receipt.clockDomain,'Node performance.now; browser clock is not combined')
  assert.equal(receipt.qualified,false)
  assert.deepEqual(receipt.requests[0],{id:1,key:'navigation:branch',arrivalAt:1,preparedAt:2,status:200,bytes:90,fulfilledAt:3,fulfilled:true})
  assert.equal(receipt.readiness.first.ready,false);assert.equal(receipt.readiness.decisive.ready,true)
  assert.equal(receipt.readiness.latest.logicalProof.durationMs,0.5)
  assert.equal(receipt.readiness.evaluations,10);assert.equal(receipt.readiness.omittedEvaluations,8)
})
test('request storage is capped at32 with exact omissions and no DTO bodies', () => {
  const d=createLiveDiagnostics();d.start(0)
  for(let id=1;id<=1000;id+=1){d.arrival(id,'observations:list',id*3);d.prepared(id,id*3+1,200,100);d.fulfilled(id,id*3+2,true)}
  const r=finish(d,4000)
  assert.equal(r.requests.length,32);assert.equal(r.omittedRequests,968)
  assert.ok(Buffer.byteLength(serializeLiveDiagnostic(r))<256*1024)
  assert.equal(r.requests[0].body,undefined)
})
test('paint attempts retain first and latest with exact omitted count',()=>{
  const d=createLiveDiagnostics();d.start(0);d.dispatched(1)
  d.readiness(3,state,true,{startedAt:2,endedAt:2.5})
  for(let i=0;i<5;i+=1){d.paintStarted(4+2*i);d.paintEnded(5+2*i,0,true)}
  d.settled(14);d.stable(15,515)
  const r=d.snapshot();assert.equal(r.paints.attempts,5);assert.equal(r.paints.omittedAttempts,3)
})
test('rejects nonfinite, reversed and repeated host boundaries',()=>{
  const d=createLiveDiagnostics();assert.throws(()=>d.start(NaN),/host/);d.start(1)
  assert.throws(()=>d.dispatched(0),/host/);assert.throws(()=>d.start(2),/Repeated/)
})
test('rejects malformed request, readiness, proof and paint boundaries',()=>{
  const d=createLiveDiagnostics();d.start(0)
  assert.throws(()=>d.arrival(-1,'route',1),/Malformed/)
  d.arrival(1,'route',1)
  assert.throws(()=>d.fulfilled(1,2,true),/Malformed/)
  assert.throws(()=>d.prepared(1,3,0,2),/Malformed/)
  assert.throws(()=>d.readiness(5,{...state,activeRequests:-1},true,{startedAt:4,endedAt:4.5}),/readiness/)
  assert.throws(()=>d.readiness(6,state,true,{startedAt:6,endedAt:5}),/proof/)
  assert.throws(()=>d.paintEnded(7,0,true),/Missing paint/)
})
test('missing settle, incomplete response and postpaint failure cannot finalize',()=>{
  assert.throws(()=>createLiveDiagnostics().snapshot(),/Missing boundary/)
  const d=createLiveDiagnostics();d.start(0);d.arrival(1,'route',1)
  assert.throws(()=>finish(d),/Incomplete/)
})
test('oversized diagnostics fail rather than truncate causal evidence',()=>{
  assert.throws(()=>serializeLiveDiagnostic({data:'x'.repeat(256*1024)}),/256 KiB/)
})
test('two-paint await excludes the independently observed postpaint proof interval',()=>{
  const d=createLiveDiagnostics();d.start(0);d.dispatched(1)
  d.readiness(3,state,true,{startedAt:2,endedAt:2.5})
  d.paintStarted(4);d.paintEnded(14,0,true,{startedAt:15,endedAt:18})
  d.settled(19);d.stable(20,520)
  const r=d.snapshot();assert.equal(r.intervals.twoPaintMs,10)
  assert.equal(r.paints.latest.logicalProof.durationMs,3)
  assert.equal(r.intervals.settleMs,19)
})
test('passive capture preserves actual settle polling, evaluations and proof calls',async()=>{
  const execute=async diagnostic=>{
    const calls=[]
    const page={evaluate:async fn=>{const source=String(fn);calls.push(source);return source.includes('.loading-indicator')?{loading:false,focused:false,anchorVisible:true}:source.includes('dispatchTurnComplete')?true:undefined},waitForTimeout:async ms=>{calls.push(ms)}}
    const tracker={maxBranches:0,maxExpandedBranches:0,maxDetails:0,unhandledApiRoutes:[],requests:[],active:0}
    let proofs=0
    if(diagnostic){tracker.liveDiagnostics=diagnostic;diagnostic.start(performance.now());diagnostic.dispatched(performance.now())}
    const end=await waitForLiveSettle(page,tracker,{index:0},'anchor',()=>{proofs+=1;return true})
    if(diagnostic){diagnostic.settled(end);const start=performance.now();diagnostic.stable(start,start+500)}
    return {calls,proofs}
  }
  const collector=createLiveDiagnostics()
  assert.deepEqual(await execute(collector),await execute(null))
  assert.equal(collector.snapshot().readiness.evaluations,1)
})
