import test from 'node:test'
import assert from 'node:assert/strict'
import path from 'node:path'
import os from 'node:os'
import { generateFixture, createObservationDiscoveryFixture } from './fixtures.mjs'
import { hashJson, assertQualificationEnvironment, createPhysicalRequestAccounting, perfApiRouteKey, observationDiscoveryReceipt, assertObservationDiscoveryReceipt } from './contracts.mjs'
import { evidenceProfile, evidenceOwnerFields, assertEvidenceIdentity, assertEvidenceWrite, isolatedRecordOutputs,
  LIVE_OBSERVATION_SEARCH_BASE as BASE, LIVE_OBSERVATION_SEARCH_PROJECT as PROJECT, LIVE_OBSERVATION_SEARCH_TASK as TASK,
  SUBJECT_TREE_LIVE_FILTERS_BASE, SUBJECT_TREE_LIVE_FILTERS_TASK } from './evidence-profile.mjs'
import { createTracker, fixtureResponder, inputQualification, assertQualifiedInputs, assertCurrentDiscoveryTransport, assertCurrentSearchRun, finalWorkingTreeEvidence, qualificationFailureIdentity, qualificationAttempt, currentUnifiedSearchFixture, currentSearchFixture, CURRENT_SEARCH_TOKEN } from './harness.mjs'

const session = '90000000-0000-4000-8000-000000000001'
function fixture() {
  const f = generateFixture('small'), original = f.observations[0]
  const subjects = (kind,path) => [{subject_kind:kind,subject:path}]
  const row = (index,content,items) => ({...original,id:f.observations[index].id,content,subjects:items,
    updated_at:`2026-01-0${index+1}T00:00:00Z`,git_sha:'creation',current_provenance:{reviewed_git_sha:'current'},revision_history:[{reviewed_git_sha:'history'}]})
  f.observations = [
    row(0,'unrelated semantic passage',subjects('file','src/a')),
    row(1,'needle',subjects('glob','src/a')),
    row(2,'needle needle',[...subjects('file','src/a'),...subjects('file','src/deep/b')]),
    row(3,'needle',subjects('file','elsewhere/a')),
    row(4,'needle',[...subjects('file','elsewhere/b'),...subjects('glob','src/c')])]
  return f
}
const create = (f,filters = {subject_query:'src/',content_query:'needle'},generation = 1) => ({workspace_id:f.workspace.id,filters,search_session:session,generation})

test('discovery full pair unites FTS-only and semantic-only before pages/counts/tree/typed leaves', () => {
  const f=fixture(), [semantic,fts,both]=f.observations
  const service=createObservationDiscoveryFixture(f,{similarities:()=>({[semantic.id]:.8,[both.id]:.9,[f.observations[3].id]:1})})
  const first=service.handle({...create(f),limit:1}).value
  assert.equal(first.retrieval.mode,'hybrid'); assert.equal(first.page.has_more,true)
  assert.equal(first.page.counts.match_count,4); assert.equal(first.page.items[0].id,both.id)
  const continuation={workspace_id:f.workspace.id,context_token:first.context_token}
  const all=service.handle({...continuation,limit:200}).value
  assert.deepEqual(new Set(all.page.items.map(o=>o.id)),new Set([semantic.id,fts.id,both.id,f.observations[4].id]))
  const second=service.handle({...continuation,offset:1,limit:1}).value
  assert.equal(second.page.counts.match_count,4); assert.notEqual(second.page.items[0].id,both.id)
  const count=service.handle({...continuation,view:'count'}).value
  assert.equal(count.page.counts.match_count,4); assert.deepEqual(count.page.items,[])
  const tree=service.handle({...continuation,view:'tree',prefix:'src'}).value.page.tree
  assert.equal(tree.match_count,4); assert.equal(tree.items.find(n=>n.path==='src/a').observation_count,3)
  assert.deepEqual(tree.items.find(n=>n.path==='src/a').terminals.map(t=>[t.subject_kind,t.observation_count]),[['file',2],['glob',1]])
  const leaf=service.handle({...continuation,terminal:{subject_kind:'glob',subject:'src/a'}}).value
  assert.deepEqual(leaf.page.items.map(o=>o.id),[fts.id]); assert.equal(leaf.page.counts.match_count,4)
  const narrowed=service.handle(create(f,{subject_query:'src/',subject_kind:'file',content_query:'needle',git_sha:'creation',current_git_sha:'current',history_git_sha:'history'},2)).value
  assert.deepEqual(new Set(narrowed.page.items.map(o=>o.id)),new Set([semantic.id,both.id]),'subject kind/query must match the same subject row')
  const noHistory=service.handle(create(f,{content_query:'needle',history_git_sha:'absent'},3)).value
  assert.equal(noHistory.page.counts.match_count,0)
  for(const [index,key] of ['git_sha','current_git_sha','history_git_sha'].entries())
    assert.equal(service.handle(create(f,{subject_query:'src/',content_query:'needle',[key]:'missing'},4+index)).value.page.counts.match_count,0)
})

test('discovery pins mode and membership inputs and rejects stale/mixed/foreign/expired context lifetimes', () => {
  const f=fixture(); let clock=1000, scores={[f.observations[0].id]:.8}, mode='hybrid'
  const service=createObservationDiscoveryFixture(f,{now:()=>clock,similarities:()=>scores,mode:()=>mode})
  const one=service.handle(create(f)).value; scores={}; mode='fts_only'
  const follow={workspace_id:f.workspace.id,context_token:one.context_token}
  assert.equal(service.handle(follow).value.page.counts.match_count,4,'completed query keeps its injected semantic input')
  assert.equal(service.handle(follow).value.retrieval.mode,'hybrid','provider mode changes cannot alter an accepted context')
  assert.equal(service.handle(follow,'other-actor').status,409)
  assert.equal(service.handle({...follow,filters:{}}).status,400)
  assert.equal(service.handle({...create(f),search_session:undefined,generation:undefined,replace_context_token:one.context_token}).status,409)
  const two=service.handle(create(f,{},2)).value
  assert.equal(service.handle(follow).status,409); assert.equal(service.handle(create(f)).status,409)
  service.retire(two.context_token)
  assert.equal(service.handle({workspace_id:f.workspace.id,context_token:two.context_token}).status,409)
  assert.equal(service.handle(create(f,{},1)).status,409,'explicit retirement keeps the generation high-water mark')
  assert.equal(service.handle(create(f,{},3)).status,200)
  clock+=300001
  assert.equal(service.handle(follow).status,409)
  const fallback=createObservationDiscoveryFixture(f,{mode:'fts_only',reason:'provider_unavailable',similarities:()=>({[f.observations[0].id]:1})})
  const result=fallback.handle(create(f)).value
  assert.equal(result.page.counts.match_count,3); assert.deepEqual(result.retrieval,{mode:'fts_only',reason:'provider_unavailable'})
})

test('discovery preserves nonempty literal filter bytes and uses wire fts_only for empty or unavailable queries', () => {
  const f=fixture(),seen=[]
  const service=createObservationDiscoveryFixture(f,{similarities:filters=>{seen.push(filters);return {}}})
  const literal={subject_query:'src/',content_query:' needle ',subject:'src/a',git_sha:'creation',current_git_sha:'current',history_git_sha:'history'}
  const accepted=service.handle(create(f,literal,1)).value
  assert.equal(accepted.page.counts.match_count,2)
  assert.deepEqual(seen[0],{subject_kind:null,...literal},'content and all nonempty filter bytes survive normalization')
  for(const [index,[key,value]] of [['subject_query',' src/'],['subject_query',' '],['subject',' src/a'],['git_sha',' creation'],['current_git_sha',' current'],['history_git_sha',' history']].entries()) {
    const reply=service.handle(create(f,{...literal,[key]:value},index+2)).value
    assert.equal(reply.page.counts.match_count,0,`${key} must keep its literal whitespace`)
    assert.equal(seen.at(-1)[key],value)
  }
  for(const [index,content] of ['', '   '].entries()) {
    const request=create(f,{subject_query:'',content_query:content},index+20), response=service.handle(request).value
    assert.deepEqual(response.retrieval,{mode:'fts_only',reason:'empty_query'})
    assert.equal(response.page.counts.match_count,content === '' ? 5 : 0,'only an exactly empty body removes the FTS predicate')
    assert.doesNotThrow(()=>assertObservationDiscoveryReceipt(observationDiscoveryReceipt(request,response),{
      workspaceId:f.workspace.id,contextToken:response.context_token,filters:request.filters,mode:'fts_only',reason:'empty_query',view:'list',
      searchSession:session,generation:request.generation,currentGeneration:request.generation,hasMore:false,
      ids:content === '' ? [...f.observations].reverse().map(o=>o.id) : []}))
  }
  const fallback=createObservationDiscoveryFixture(f,{mode:'fts_only',reason:'provider_unavailable',similarities:()=>assert.fail('FTS-only must not produce semantic inputs')})
  const request=create(f), response=fallback.handle(request).value
  const expected={workspaceId:f.workspace.id,contextToken:response.context_token,filters:request.filters,mode:'fts_only',reason:'provider_unavailable',view:'list',searchSession:session,generation:1,currentGeneration:1,
    hasMore:false,ids:[f.observations[2].id,f.observations[4].id,f.observations[1].id]}
  assert.doesNotThrow(()=>assertObservationDiscoveryReceipt(observationDiscoveryReceipt(request,response),expected))
  const invalid={...response,retrieval:{mode:'fts',reason:'provider_unavailable'}}
  assert.throws(()=>assertObservationDiscoveryReceipt(observationDiscoveryReceipt(request,invalid),{...expected,mode:'fts'}),/mismatch/)
  assert.equal(createObservationDiscoveryFixture(f,{mode:'fts'}).handle(request).status,400)
})

test('discovery receipt rejects incomplete identity, stale generations, rebound tokens and false byte counts', () => {
  const f=fixture(), service=createObservationDiscoveryFixture(f), request=create(f)
  const response=service.handle(request).value, receipt=observationDiscoveryReceipt(request,response)
  const expected={workspaceId:f.workspace.id,contextToken:response.context_token,filters:request.filters,mode:'hybrid',view:'list',searchSession:session,generation:1,currentGeneration:1,matchCount:3,
    hasMore:false,ids:[f.observations[2].id,f.observations[4].id,f.observations[1].id]}
  assert.doesNotThrow(()=>assertObservationDiscoveryReceipt(receipt,expected))
  for(const bad of [{...expected,filters:{}},{...expected,currentGeneration:2},{...expected,workspaceId:'foreign'},{...expected,contextToken:'foreign'},{...expected,filters:null}])
    assert.throws(()=>assertObservationDiscoveryReceipt(receipt,bad),/mismatch/)
  for(const bad of [{...receipt,fulfilled:false},{...receipt,requestBytes:1},{...receipt,response:{...response,complete:false}}])
    assert.throws(()=>assertObservationDiscoveryReceipt(bad,expected),/mismatch/)
  const next={workspace_id:f.workspace.id,context_token:response.context_token,view:'count'}
  const nextReceipt=observationDiscoveryReceipt(next,service.handle(next).value)
  assert.doesNotThrow(()=>assertObservationDiscoveryReceipt(nextReceipt,{...expected,view:'count',currentContextToken:response.context_token}))
  assert.throws(()=>assertObservationDiscoveryReceipt(nextReceipt,{...expected,view:'count',currentContextToken:'new-generation-token'}),/mismatch/)
})

test('discovery bounded admission coalesces the same session and retains keyed retirement fences', () => {
  const f=fixture(),service=createObservationDiscoveryFixture(f)
  const first=service.handle(create(f)).value
  for(let i=0;i<7;i++) assert.equal(service.handle({workspace_id:f.workspace.id,filters:{}}).status,200)
  assert.equal(service.handle({workspace_id:f.workspace.id,filters:{}}).status,429)
  assert.equal(service.handle(create(f,{},2)).status,200,'same-session higher generation reuses its occupied slot')
  assert.equal(service.handle({workspace_id:f.workspace.id,context_token:first.context_token}).status,409)
  for(let actor=1;actor<8;actor++)for(let i=0;i<8;i++)assert.equal(service.handle({workspace_id:f.workspace.id,filters:{}},'actor-'+actor).status,200)
  assert.equal(service.handle({workspace_id:f.workspace.id,filters:{}},'ninth-actor').status,429)
  assert.equal(service.handle(create(f,{},3)).status,200,'generation coalescing also works at the global bound')
  assert.equal(service.handle(create(f,{},2)).status,409)
})

test('discovery generated creation history fallback remains separate from current provenance', () => {
  const f=generateFixture('small'),original=f.observations[0]
  f.observations=[{...original,current_provenance:{reviewed_git_sha:'current-review'}}]
  assert.equal(f.observations[0].revision_history,undefined)
  const service=createObservationDiscoveryFixture(f)
  const creation=service.handle(create(f,{history_git_sha:original.git_sha,current_git_sha:'current-review'},1)).value
  assert.deepEqual(creation.page.items.map(o=>o.id),[original.id])
  assert.equal(service.handle(create(f,{history_git_sha:'current-review'},2)).value.page.counts.match_count,0)
  f.observations[0].revision_history=[{reviewed_git_sha:'actual-history'}]
  assert.equal(service.handle(create(f,{history_git_sha:original.git_sha},3)).value.page.counts.match_count,0,'an explicit history is authoritative')
  assert.equal(service.handle(create(f,{history_git_sha:'actual-history',current_git_sha:'current-review'},4)).value.page.counts.match_count,1)
})

test('discovery receipts reject wrong selectors, cardinality, has_more, IDs and tree/count shapes', () => {
  const f=fixture(),service=createObservationDiscoveryFixture(f),request={...create(f),limit:1}
  const response=service.handle(request).value
  const expected={workspaceId:f.workspace.id,contextToken:response.context_token,filters:request.filters,mode:'hybrid',view:'list',searchSession:session,
    generation:1,currentGeneration:1,limit:1,offset:0,hasMore:true,ids:[f.observations[2].id],matchCount:3}
  const accept=(body,value,expect=expected)=>assertObservationDiscoveryReceipt(observationDiscoveryReceipt(body,value),expect)
  assert.doesNotThrow(()=>accept(request,response))
  for(const limit of [0,201,1.5,2]) assert.throws(()=>accept({...request,limit},response),/mismatch/)
  for(const offset of [-1,2147483648,0.5,1]) assert.throws(()=>accept({...request,offset},response),/mismatch/)
  assert.throws(()=>accept(request,{...response,page:{...response.page,has_more:false}}),/mismatch/)
  assert.throws(()=>accept(request,{...response,page:{...response.page,items:[...response.page.items,...response.page.items]}}),/mismatch/)
  assert.throws(()=>accept(request,{...response,page:{...response.page,items:[f.observations[1]]}}),/mismatch/)
  assert.throws(()=>accept(request,response,{...expected,ids:undefined}),/mismatch/)
  const continuation={workspace_id:f.workspace.id,context_token:response.context_token}
  const treeRequest={...continuation,view:'tree',prefix:'src',limit:1},tree=service.handle(treeRequest).value
  const treeExpected={...expected,view:'tree',prefix:'src',currentContextToken:response.context_token,treePaths:['src/a']}
  assert.doesNotThrow(()=>accept(treeRequest,tree,treeExpected))
  for(const page of [
    {...tree.page,has_more:true},
    {...tree.page,tree:{...tree.page.tree,has_more:false}},
    {...tree.page,tree:{...tree.page.tree,items:[...tree.page.tree.items,...tree.page.tree.items]}},
    {...tree.page,tree:{...tree.page.tree,items:tree.page.tree.items.map(n=>({...n,terminals:[{subject_kind:'file',subject:n.path,observation_count:999}]}))}}
  ]) assert.throws(()=>accept(treeRequest,{...tree,page},treeExpected),/mismatch/)
  const countRequest={...continuation,view:'count'},count=service.handle(countRequest).value
  const countExpected={...expected,view:'count',limit:50,hasMore:false,currentContextToken:response.context_token}
  assert.doesNotThrow(()=>accept(countRequest,count,countExpected))
  assert.throws(()=>accept(countRequest,{...count,page:{...count.page,items:[f.observations[0]]}},countExpected),/mismatch/)
})

test('discovery physical accounting is once per actual POST including continuation and failed fulfillment', async () => {
  const f=fixture(),tracker=createTracker(f),respond=fixtureResponder(f,tracker)
  const send=async(body,fail=false)=>{
    const raw=JSON.stringify(body,null,2), request={url:()=> 'http://fixture/api/v1/observations/discovery',method:()=> 'POST',postData:()=>raw,postDataJSON:()=>body}
    let result
    await respond({request:()=>request,fulfill:async response=>{result=response;if(fail)throw new Error('controlled fulfill failure')}})
    return result
  }
  const first=await send(create(f)), token=JSON.parse(first.body).context_token
  const second=await send({workspace_id:f.workspace.id,context_token:token,view:'count'})
  await assert.rejects(send({workspace_id:f.workspace.id,context_token:token,view:'tree'},true),/controlled fulfill/)
  const stale=await send({workspace_id:f.workspace.id,context_token:'retired-token',view:'count'})
  assert.equal(stale.status,409)
  const totals=tracker.physicalAccounting.snapshot()
  assert.equal(totals.requests,4);assert.equal(totals.completed,4);assert.equal(tracker.completed,4);assert.equal(tracker.active,0)
  assert.equal(totals.requestBytes,tracker.requests.reduce((n,r)=>n+r.requestBytes,0))
  assert.equal(totals.responseBytes,tracker.requests.reduce((n,r)=>n+r.bytes,0))
  assert.equal(tracker.bytes['observations:discovery'],totals.responseBytes)
  assert.equal(tracker.requests[0].discovery.fulfilled,true);assert.equal(tracker.requests[2].discovery.fulfilled,false)
  assert.equal(tracker.requests[0].discovery.requestBytes,tracker.requests[0].requestBytes,'physical whitespace is counted, not reserialized logical bytes')
  assert.equal(JSON.parse(second.body).page.counts.match_count,3)
  const ledger=createPhysicalRequestAccounting();ledger.begin('physical','中文');ledger.finish('physical','réponse')
  assert.equal(ledger.snapshot().requestBytes,Buffer.byteLength('中文'))
  assert.throws(()=>ledger.begin('physical',''),/Duplicate/);assert.throws(()=>ledger.finish('physical',''),/duplicate/)
  assert.throws(()=>ledger.finish('logical-tree',''),/Missing/)
  assert.equal(perfApiRouteKey('/api/v1/observations/discovery','POST'),'observations:discovery')
})

test('discovery profile owns only current qualification and rejects foreign or incomplete source identities', () => {
  const frontend=path.resolve(import.meta.dirname,'..'),temporary=os.tmpdir()
  const profile=evidenceProfile(BASE,frontend,temporary)
  assert.equal(profile.task.taskId,TASK);assert.deepEqual(evidenceOwnerFields(profile),{projectId:PROJECT})
  assert.equal(profile.liveObservationSearch,true);assert.equal(profile.subjectTreeLiveFilters,false)
  assert.equal(evidenceProfile(SUBJECT_TREE_LIVE_FILTERS_BASE,frontend,temporary).task.taskId,SUBJECT_TREE_LIVE_FILTERS_TASK)
  const hash='a'.repeat(64),head='1'.repeat(40)
  const record={taskId:TASK,projectId:PROJECT,evidenceBaseCommit:BASE,measurementRevision:profile.revision,
    sourceProvenance:{reviewBaseCommit:BASE,headCommit:head},inputQualification:{evidenceBaseCommit:BASE,implementationCommit:head,
      source:[{path:'src/Main.elm',sha256:hash}],productionAssets:[{path:'static/app.js',sha256:hash}]}}
  assert.doesNotThrow(()=>assertEvidenceIdentity(profile,record))
  for(const key of ['taskId','projectId','evidenceBaseCommit','measurementRevision'])assert.throws(()=>assertEvidenceIdentity(profile,{...record,[key]:'foreign'}),/mismatch/)
  assert.throws(()=>assertEvidenceIdentity(profile,{...record,inputQualification:null}),/mismatch/)
  assert.throws(()=>assertEvidenceIdentity({...profile,liveObservationSearch:false},record),/mismatch/)
  assert.throws(()=>assertEvidenceWrite(profile,path.join(frontend,'perf/baseline.v1.json'),'{}'),/Unowned/)
  assert.throws(()=>isolatedRecordOutputs(profile,'foreign','foreign','legacy','legacy'),/task-owned/)
  assert.throws(()=>evidenceProfile('unknown',frontend,temporary),/Unmapped/)
})

test('discovery qualification input collector accepts injected sources/assets and detects drift without Git or builds', () => {
  const frontend=path.resolve(import.meta.dirname,'..'), profile=evidenceProfile(BASE,frontend,os.tmpdir())
  const io={readdirSync:(directory,options)=>options?.withFileTypes ? [{name:'input',isDirectory:()=>false}] : ['input.mjs'],readFileSync:file=>Buffer.from('injected:'+file)}
  const capture={read:()=>Buffer.from('1'.repeat(40)+'\n')}
  const inputs=inputQualification({plan:profile,io,capture})
  assert.ok(inputs.source.length>0);assert.equal(inputs.productionAssets.length,1);assert.equal(inputs.implementationCommit,'1'.repeat(40))
  assert.doesNotThrow(()=>assertQualifiedInputs(inputs,structuredClone(inputs),'input drift'))
  assert.throws(()=>assertQualifiedInputs(inputs,{...inputs,productionAssets:[]},'input drift'),/input drift/)
  const environment={os:'injected OS',cpu:'injected CPU',ramBytes:1024,browserControl:'injected owned control',node:'test',npm:'test',elm:'test',vite:'test',playwright:'test',chromium:'test',viewport:'1440x1000',headless:true}
  const recorded={...environment,fingerprint:hashJson(environment)}
  assert.equal(assertQualificationEnvironment(recorded),recorded)
  assert.throws(()=>assertQualificationEnvironment({...recorded,cpu:'changed'}),/identity/)
  assert.throws(()=>assertQualificationEnvironment({fingerprint:hashJson({})}),/incomplete/)
  for(const key of ['vite','playwright']) {
    const incomplete={...environment};delete incomplete[key]
    assert.throws(()=>assertQualificationEnvironment({...incomplete,fingerprint:hashJson(incomplete)}),/incomplete/)
  }
})

// Current-profile qualification preserves historical fixture bytes.
test('current global fixture applies whole lexemes, independent pages and compact observation DTOs',()=>{
  const f=currentSearchFixture(generateFixture('large')),query=body=>currentUnifiedSearchFixture(f,{workspace_id:f.workspace.id,query:CURRENT_SEARCH_TOKEN,...body})
  const all=query({}).value
  assert.deepEqual([all.projects.length,all.tasks.length,all.observations.length],[1,1,1])
  assert.equal(all.observations[0].id,f.observations[0].id)
  assert.ok(!('content' in all.observations[0])&&!('created_at' in all.observations[0]))
  assert.equal([...all.observations[0].content_preview].length,Math.min(280,[...f.observations[0].content].length))
  assert.deepEqual(query({query:'qualificationsearc'}).value.projects,[])
  const broad=query({query:'Observation',entity_types:['observation'],limit:200,offset:200}).value
  assert.equal(broad.observations.length,200);assert.equal(broad.has_more.observations,true);assert.equal(broad.next_offset.observations,400)
  assert.deepEqual(query({entity_types:[]}).value,{projects:[],tasks:[],observations:[],has_more:{},next_offset:{}})
  for(const body of [{limit:0},{limit:201},{offset:-1},{entity_types:['unknown']},{workspace_id:'foreign'}])assert.equal(query(body).status,400)
})

test('current aggregate validates actual physical bytes and strict owned discovery envelopes against backing IDs',()=>{
  const f=currentSearchFixture(generateFixture('small')),service=createObservationDiscoveryFixture(f)
  const first={workspace_id:f.workspace.id,filters:{},search_session:session,generation:1,view:'list',limit:50,offset:0}
  const response=service.handle(first).value
  const second={workspace_id:f.workspace.id,context_token:response.context_token,view:'list',limit:50,offset:50}
  const receipts=[first,second].map((request,i)=>{const discovery=observationDiscoveryReceipt(request,i===0?response:service.handle(request).value);return {requestId:i+1,key:'observations:discovery',done:true,fulfilled:true,status:200,requestBytes:discovery.requestBytes,responseBytes:discovery.responseBytes,discovery}})
  const run={fixture:'small',physicalReceipts:receipts,physicalAccounting:{requests:2,completed:2,requestBytes:receipts.reduce((n,r)=>n+r.requestBytes,0),responseBytes:receipts.reduce((n,r)=>n+r.responseBytes,0)}}
  assert.doesNotThrow(()=>assertCurrentDiscoveryTransport(run))
  const reject=mutate=>{const changed=structuredClone(run);mutate(changed);assert.throws(()=>assertCurrentDiscoveryTransport(changed),/mismatch/)}
  reject(r=>r.physicalReceipts[1].requestId=1)
  reject(r=>r.physicalAccounting.requestBytes++)
  reject(r=>r.physicalReceipts[1].fulfilled=false)
  // Reserialize tampering and repair its physical byte ledger: rejection must
  // come from context/page ownership, rather than only a raw JSON mismatch.
  const wireTamper=mutate=>reject(r=>{const p=r.physicalReceipts[1],d=p.discovery;mutate(d);d.rawRequestBody=JSON.stringify(d.request);d.rawResponseBody=JSON.stringify(d.response);d.requestBytes=Buffer.byteLength(d.rawRequestBody);d.responseBytes=Buffer.byteLength(d.rawResponseBody);p.requestBytes=d.requestBytes;p.responseBytes=d.responseBytes;r.physicalAccounting.requestBytes=r.physicalReceipts.reduce((n,x)=>n+x.requestBytes,0);r.physicalAccounting.responseBytes=r.physicalReceipts.reduce((n,x)=>n+x.responseBytes,0)})
  wireTamper(d=>d.request.workspace_id='foreign')
  wireTamper(d=>d.request.filters={content_query:'other'})
  wireTamper(d=>d.request.generation=2)
  wireTamper(d=>d.request.context_token='foreign')
  wireTamper(d=>d.response.page.items[0].id=f.observations[0].id)
  wireTamper(d=>d.response.page.has_more=true)
  wireTamper(d=>d.response.complete=false)
  wireTamper(d=>d.response.page.items.push(f.observations[0]))
})

test('current success/failure producers persist complete identities accepted by startup and check',()=>{
  const frontend=path.resolve(import.meta.dirname,'..'),profile=evidenceProfile(BASE,frontend,os.tmpdir()),head='1'.repeat(40),baseline=Buffer.from('immutable baseline')
  const inputs={evidenceBaseCommit:BASE,implementationCommit:head,source:[{path:'src/Main.elm',sha256:'a'.repeat(64)}],productionAssets:[{path:'static/app.js',sha256:'b'.repeat(64)}]}
  const provenance={reviewBaseCommit:BASE,headCommit:head},identity=qualificationFailureIdentity(profile,inputs,provenance)
  for(const fields of [{passed:false,failure:'controlled'},{qualificationPassed:false,failure:'controlled'}])assert.doesNotThrow(()=>assertEvidenceIdentity(profile,{...identity,...fields}))
  const files=new Map(Object.values(profile.files).map(file=>[file,Buffer.from('{}')]))
  const validation={...identity,passed:true};files.set(profile.files.validation,Buffer.from(JSON.stringify(validation)))
  const io={existsSync:file=>files.has(file),readFileSync:(file,encoding)=>{const bytes=file===path.resolve('perf/baseline.v1.json')?baseline:files.get(file);assert.ok(bytes,'known virtual file '+file);return encoding?bytes.toString(encoding):bytes},statSync:file=>({size:files.get(file).length})}
  const capture={read:args=>args[0]==='show'?baseline:args[0]==='rev-parse'?Buffer.from(head+'\n'):Buffer.alloc(0)}
  const produce=()=>finalWorkingTreeEvidence({plan:profile,io,capture,write:(file,bytes)=>files.set(file,Buffer.from(bytes))})
  produce();const manifest=JSON.parse(files.get(profile.files.manifest))
  assert.deepEqual(manifest.inputQualification,inputs);assert.equal(manifest.qualificationPassed,true)
  assert.doesNotThrow(()=>assertEvidenceIdentity(profile,manifest))
  // The same strict consumer is used at startup and before check finalization.
  for(const source of [manifest,validation,identity]){assert.doesNotThrow(()=>assertEvidenceIdentity(profile,source));const bad=structuredClone(source);delete bad.inputQualification;assert.throws(()=>assertEvidenceIdentity(profile,bad),/input identity/)}
  const failure=new Error('controlled production failure')
  assert.throws(()=>qualificationAttempt({command:'node perf/harness.mjs record',exitCode:2},inputs,{passed:true},()=>{throw failure},[],{plan:profile,sourceProvenance:provenance,write:(file,bytes)=>files.set(file,Buffer.from(bytes)),remove:file=>files.delete(file),verify:()=>{},sha256File:file=>hashJson(files.get(file).toString()),evidenceArtifactPath:file=>file}),/controlled production failure/)
  const failedManifest=JSON.parse(files.get(profile.files.manifest)),failedValidation=JSON.parse(files.get(profile.files.validation))
  assert.doesNotThrow(()=>assertEvidenceIdentity(profile,failedManifest));assert.doesNotThrow(()=>assertEvidenceIdentity(profile,failedValidation))
  assert.equal(failedManifest.failure,failure.message);assert.equal(failedManifest.qualificationPassed,false);assert.equal(failedValidation.passed,false)
  assert.equal(failedManifest.validationRecord.sha256,hashJson(files.get(profile.files.validation).toString()))
  const missing=structuredClone(failedManifest);delete missing.inputQualification
  assert.throws(()=>assertEvidenceIdentity(profile,missing),/input identity/)
  const recovered={...missing,inputQualification:failedValidation.inputQualification};assert.doesNotThrow(()=>assertEvidenceIdentity(profile,recovered));assert.equal(recovered.failure,failure.message)
  validation.inputQualification={...inputs,implementationCommit:'2'.repeat(40)};files.set(profile.files.validation,Buffer.from(JSON.stringify(validation)))
  assert.throws(produce,/identity mismatch/)
})

function currentCausalRun(){
  const f=currentSearchFixture(generateFixture('small')),service=createObservationDiscoveryFixture(f,{similarities:filters=>filters.content_query==='Observation 00001 evidence'?{[f.observations[25].id]:.9}:{}}),physical=[]
  const send=request=>{const response=service.handle(request);assert.equal(response.status,200);const discovery=observationDiscoveryReceipt(request,response.value);physical.push({requestId:physical.length+1,key:'observations:discovery',status:200,done:true,fulfilled:true,requestBytes:discovery.requestBytes,responseBytes:discovery.responseBytes,discovery});return discovery}
  const start=(generation,filters,view='list')=>send({workspace_id:f.workspace.id,search_session:session,generation,filters,view,limit:50,offset:0})
  const initial=start(1,{}),loadMore=send({workspace_id:f.workspace.id,context_token:initial.response.context_token,view:'list',limit:50,offset:50})
  const filters={subject_query:'src/feature-0/',content_query:'Observation 00001 evidence'},pair=start(2,filters),tree=start(3,filters,'tree'),terminal=f.observations[0].subjects.find(s=>s.subject_kind==='file')
  const leaf=send({workspace_id:f.workspace.id,context_token:tree.response.context_token,view:'list',terminal,limit:50,offset:0}),old=start(4,{...filters,content_query:'obsolete-unmatched-query'},'tree'),fresh=start(5,filters,'tree'),reset=start(6,{})
  const jumps=['project','task','observation'].map(type=>{const targetId=(type==='project'?f.projects:type==='task'?f.tasks:f.observations)[0].id,requestId=physical.length+1;physical.push({requestId,key:(type==='project'?'projects':type==='task'?'tasks':'observations')+':entity:'+targetId,status:200,done:true,fulfilled:true,requestBytes:0,responseBytes:1});return {type,targetId,canonicalLookup:requestId,focus:'search-jump-'+type+'-'+targetId,query:CURRENT_SEARCH_TOKEN}})
  return {fixture:'small',physicalReceipts:physical,physicalAccounting:{requests:physical.length,completed:physical.length,requestBytes:physical.reduce((n,r)=>n+r.requestBytes,0),responseBytes:physical.reduce((n,r)=>n+r.responseBytes,0)},currentSearch:{revision:'live-observation-search.production.v1',initial,loadMore:{loaded:60,receipt:loadMore},pair,tree,leaf,stale:[old,fresh],staleDom:{ids:[f.observations[0].id],matchCount:2,contextToken:fresh.response.context_token},reset,clears:['Subject Tree','All Observations'].flatMap(mode=>['subjects','body'].map(field=>({mode,field,prior:500,top:0,other:field==='subjects'?'Observation':'src/'}))),jumps,selectedTypes:['project','task','observation'],noTypesRequests:0}}
}

test('current aggregate independently anchors exact FILE leaf and excludes obsolete post-release DOM membership',()=>{
  const run=currentCausalRun();assert.doesNotThrow(()=>assertCurrentSearchRun(run))
  const extra=structuredClone(run);extra.currentSearch.staleDom.ids.push('obsolete');assert.throws(()=>assertCurrentSearchRun(extra),/causal workflow/)
  const changed=structuredClone(run),old=changed.currentSearch.leaf,request={...old.request,terminal:{...old.request.terminal,subject_kind:'glob'}},response={...old.response,page:{...old.response.page,items:[]}},replacement=observationDiscoveryReceipt(request,response)
  const physical=changed.physicalReceipts.find(r=>r.discovery?.rawRequestBody===old.rawRequestBody);physical.discovery=replacement;physical.requestBytes=replacement.requestBytes;physical.responseBytes=replacement.responseBytes;changed.currentSearch.leaf=replacement
  changed.physicalAccounting.requestBytes=changed.physicalReceipts.reduce((n,r)=>n+r.requestBytes,0);changed.physicalAccounting.responseBytes=changed.physicalReceipts.reduce((n,r)=>n+r.responseBytes,0)
  assert.doesNotThrow(()=>assertCurrentDiscoveryTransport(changed),'the self-consistent glob envelope is valid transport')
  assert.throws(()=>assertCurrentSearchRun(changed),/causal workflow/,'it cannot stand in for the clicked FILE leaf')
})
