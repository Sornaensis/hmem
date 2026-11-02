import assert from 'node:assert/strict'
import {readFile,writeFile,readdir} from 'node:fs/promises'
import {createHash} from 'node:crypto'
import {gzipSync} from 'node:zlib'
import {cpus,totalmem,release} from 'node:os'
import {fileURLToPath} from 'node:url'
import {parseAst} from 'rollup/parseAst'
import {openSessionExplorer,continuousScrollStep} from '../tests-js/support/sessions-ux-fixture.mjs'

// This runner owns only the bounded qualification JSON beside it. Browser and
// fixture resources belong to each cell's finally block. No raw trace is saved.
const root=new URL('../../../',import.meta.url), frontend=new URL('../',import.meta.url)
const hash=value=>createHash('sha256').update(value).digest('hex')
const args=process.argv.slice(2), option=key=>args.includes(key)?args[args.indexOf(key)+1]:null
const revision=option('--revision')
assert.match(revision??'',/^[a-f0-9]{40}$/,'Pass the Git-MCP-verified implementation revision via --revision')
const until=async(fn,label)=>{const end=Date.now()+20000;while(Date.now()<end){if(await fn())return;await new Promise(r=>setTimeout(r,25))}throw Error('qualification_timeout:'+label)}
const twoFrames=page=>page.evaluate(()=>new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve))))
const workload=()=>{
 const records=[],offsets=[];let offset=0
 const add=record=>{offsets.push(offset);const line=JSON.stringify(record)+'\n';records.push(line);offset+=Buffer.byteLength(line)}
 for(let i=0;i<180;i++)add({type:'event_msg',payload:{type:'agent_message',message:`History prose ${i}. `+'readable λ context. '.repeat(i%9===0?3400:270)}})
 const activityStart=offset
 for(let i=0;i<900;i++)add({type:'response_item',payload:{type:'function_call',name:'inspect',call_id:'qualification-'+i,arguments:JSON.stringify({index:i,text:'activity context '.repeat(12)})}})
 add({type:'event_msg',payload:{type:'agent_message',message:'After activity run. A short stable reading anchor.'}})
 const finalOffset=offset
 add({type:'event_msg',payload:{type:'agent_message',message:'Final split record. '+'tail context. '.repeat(14000)+' FINAL END'}})
 const bytes=Buffer.from(records.join(''))
 assert.ok(bytes.length>2*1024*1024);assert.ok(Buffer.byteLength(records.at(-1))>65536)
 return {bytes,description:{sha256:hash(bytes),bytes:bytes.length,records:records.length,activityCount:900,activityStart,activityMiddle:offsets[630],finalOffset,largestRecordBytes:Math.max(...records.map(x=>Buffer.byteLength(x))),kind:'deterministic UTF-8 JSONL: prose, one 900-call activity run, split final prose'}}
}
const input=workload()
const probeSource=`;(()=>{const p=globalThis.__hmemReaderProbe={metrics:{},caps:{rows:0,retainedBytes:0,rawBytes:0},pending:0};const add=(name,ms,units=0)=>{const m=p.metrics[name]??={count:0,totalMs:0,maxMs:0,units:0};m.count++;m.totalMs+=ms;m.maxMs=Math.max(m.maxMs,ms);m.units+=units};globalThis.__hmemReaderMeasure=(name,fn,units=0)=>{const t=performance.now();try{return fn()}finally{add(name,performance.now()-t,units)}};globalThis.__hmemReaderWrap=(name,fn)=>function(...args){const t=performance.now();let value;try{value=fn.apply(this,args);return value}finally{add(name+'Entry',performance.now()-t);if(value&&typeof value.then==='function'){p.pending++;value.then(()=>{add(name+'Lifetime',performance.now()-t);p.pending--},()=>{add(name+'Lifetime',performance.now()-t);p.pending--})}}};globalThis.__hmemReaderObserve=value=>{if(value?.kind==='viewer'){p.caps.rows=Math.max(p.caps.rows,value.rows?.length??0);p.caps.retainedBytes=Math.max(p.caps.retainedBytes,value.retainedBytes??0);p.caps.rawBytes=Math.max(p.caps.rawBytes,(value.rows??[]).reduce((n,row)=>n+new TextEncoder().encode(row.raw??'').length,0))}return value}})();\n`
const workerTelemetry=`;{const original=self.postMessage;self.postMessage=function(value,...args){return original.call(this,{...value,__hmemReaderNumeric:globalThis.__hmemReaderProbe.metrics},...args)}}\n`
const walk=(node,visit)=>{if(!node||typeof node!=='object')return;visit(node);for(const [key,value] of Object.entries(node)){if(key==='parent')continue;if(Array.isArray(value))for(const child of value)walk(child,visit);else if(value&&typeof value==='object')walk(value,visit)}}
function instrument(source,worker=false){
 const patches=[],bindings=[]
 const patch=(kind,start,end,replacement)=>{const original=source.slice(start,end);bindings.push({kind,count:1,startUtf16:start,endUtf16:end,spanSha256:hash(original),replacementSha256:hash(replacement)});patches.push({start,end,replacement})}
 const unique=(regex,kind,replace)=>{const matches=[...source.matchAll(regex)];assert.equal(matches.length,1,kind+' binding count');const m=matches[0];patch(kind,m.index,m.index+m[0].length,replace(m))}
 if(worker){
  unique(/try\{(?<value>[\w$]+)=JSON\.parse\((?<raw>[\w$]+)\)\}catch\{throw Error\("invalid_json_record"\)\}/g,'sealedRawParse',m=>m[0].replace('JSON.parse('+m.groups.raw+')',`globalThis.__hmemReaderMeasure("rawParse",()=>JSON.parse(${m.groups.raw}),${m.groups.raw}.length)`))
 }else{
  unique(/function (?<dispatch>[\w$]+)\((?<msg>[\w$]+),(?<viewMeta>[\w$]+)\)\{var (?<pair>[\w$]+)=(?<a2>[\w$]+)\((?<update>[\w$]+),\k<msg>,(?<model>[\w$]+)\);(?<stepper>[\w$]+)\(\k<model>=\k<pair>\.a,\k<viewMeta>\),(?<enqueue>[\w$]+)\((?<managers>[\w$]+),\k<pair>\.b,(?<subscriptions>[\w$]+)\(\k<model>\)\)\}/g,'elmUpdate',m=>{const g=m.groups,call=`${g.a2}(${g.update},${g.msg},${g.model})`;return m[0].replace(call,`globalThis.__hmemReaderMeasure("elmUpdate",()=>${call})`)})
  unique(/try\{(?<value>[\w$]+)=JSON\.parse\((?<raw>[\w$]+)\)\}catch\{if\((?<finite>[\w$]+)\)\{(?<notice>[\w$]+)="Incomplete record at this finite window boundary\.";return\}throw new Error\("invalid_json_record"\)\}/g,'activeRawParse',m=>m[0].replace('JSON.parse('+m.groups.raw+')',`globalThis.__hmemReaderMeasure("rawParse",()=>JSON.parse(${m.groups.raw}),${m.groups.raw}.length)`))
  const tree=parseAst(source),animators=[],callbacks=[]
  walk(tree,node=>{
   if(node.type==='FunctionDeclaration'&&node.params.length===2){const [model,draw]=node.params,first=node.body.body[0],last=node.body.body.at(-1);if(first?.type==='ExpressionStatement'&&first.expression?.type==='CallExpression'&&first.expression.callee?.name===draw.name&&first.expression.arguments[0]?.name===model.name&&last?.type==='ReturnStatement'&&last.argument?.type==='FunctionExpression'&&source.slice(node.start,node.end).includes('===2'))animators.push(node)}
   if(node.type==='CallExpression'&&node.callee?.property?.name==='subscribe'&&node.callee.object?.property?.name==='sessionBridgeCommand')callbacks.push(node.arguments[0])
  })
  assert.equal(animators.length,1,'animator binding count');const animator=animators[0],draw=animator.params[1].name,pos=animator.body.start+1
  patch('elmDraw',pos,pos,`${draw}=globalThis.__hmemReaderWrap("elmDraw",${draw});`)
  Object.assign(bindings.at(-1),{startUtf16:animator.start,endUtf16:animator.end,spanSha256:hash(source.slice(animator.start,animator.end)),insertionUtf16:pos})
  assert.equal(callbacks.length,1,'bridge callback binding count');const callback=callbacks[0]
  patch('bridgeCommand',callback.start,callback.end,`globalThis.__hmemReaderWrap("bridgeCommand",(${source.slice(callback.start,callback.end)}))`)
  unique(/([\w$]+)\.ports\.sessionBridgeEvent\.send\(([\w$]+)\)/g,'viewerCaps',m=>`${m[1]}.ports.sessionBridgeEvent.send(globalThis.__hmemReaderObserve(${m[2]}))`)
 }
 patches.sort((a,b)=>b.start-a.start)
 for(let i=1;i<patches.length;i++)assert.ok(patches[i].end<=patches[i-1].start,'nonoverlapping instrumentation spans')
 let output=source;for(const p of patches)output=output.slice(0,p.start)+p.replacement+output.slice(p.end)
 output=probeSource+(worker?workerTelemetry:'')+output;parseAst(output)
 return {source:output,sha256:hash(output),bindings}
}
const assetDir=new URL('hmem-server/static/assets/',root),names=await readdir(assetDir)
const mainName=names.find(x=>/^index-.*\.js$/.test(x)),workerName=names.find(x=>/^session-archive-worker-.*\.js$/.test(x))
assert.ok(mainName&&workerName,'production build required')
const main=await readFile(new URL(mainName,assetDir),'utf8'),worker=await readFile(new URL(workerName,assetDir),'utf8')
assert.doesNotMatch(worker,/import\.meta|import\(/,'instrumented worker blob must remain self-contained')
const patched={main:instrument(main),worker:instrument(worker,true)}
const sources={}
for(const path of ['hmem-server/frontend/src/main.js','hmem-server/frontend/src/session-save.js','hmem-server/frontend/src/session-viewer.js','hmem-server/frontend/src/session-archive-worker.js','hmem-server/frontend/src/Feature/Sessions.elm','hmem-server/frontend/src/SessionWindow.elm','hmem-server/frontend/package.json','hmem-server/frontend/tests-js/hierarchy-fixture.mjs','hmem-server/frontend/tests-js/support/sessions-ux-fixture.mjs'])sources[path]=hash(await readFile(new URL(path,root)))
const supportingAssets={};for(const name of names.filter(x=>/^(vendor-.*\.js|index-.*\.css)$/.test(x)))supportingAssets[name]=hash(await readFile(new URL(name,assetDir)))
const playwrightVersion=JSON.parse(await readFile(new URL('node_modules/@playwright/test/package.json',frontend),'utf8')).version
const provenance={implementationRevision:revision,harnessSha256:hash(await readFile(fileURLToPath(import.meta.url))),sources,workload:input.description,assets:{main:{name:mainName,sha256:hash(main),instrumentedSha256:patched.main.sha256,bindings:patched.main.bindings},worker:{name:workerName,sha256:hash(worker),instrumentedSha256:patched.worker.sha256,bindings:patched.worker.bindings},supportingAssets,helperSha256:hash(probeSource),workerTelemetrySha256:hash(workerTelemetry)}}
if(args.includes('--self-check')){console.log(JSON.stringify({status:'PASS',...provenance}));process.exit(0)}

const aggregate=values=>{const sorted=[...values].sort((a,b)=>a-b);return {samples:sorted.length,totalMs:values.reduce((n,x)=>n+x,0),maxMs:sorted.at(-1)??0,p50Ms:sorted[Math.floor((sorted.length-1)*.5)]??0,p95Ms:sorted[Math.floor((sorted.length-1)*.95)]??0}}
async function archive(h){
 const raw=input.bytes,gzip=gzipSync(raw),generation='33333333-3333-4333-8333-333333333333',etag='"reader-qualification"'
 const manifest={workspace_id:h.fixture.workspace.id,session_id:h.saved.id,state:'ready',generation_id:generation,codec:'gzip-v1',raw_bytes:String(raw.length),raw_records:String(input.description.records),raw_sha256:hash(raw),compressed_bytes:String(gzip.length),compressed_sha256:hash(gzip),content_path:`/api/v1/workspaces/${h.fixture.workspace.id}/sessions/${h.saved.id}/archive/${generation}/content`}
 await h.page.route('**/sessions/**/archive**',route=>new URL(route.request().url()).pathname.endsWith('/content')?route.fulfill(route.request().headers()['if-none-match']?{status:304,headers:{ETag:etag}}:{headers:{'Content-Type':'application/gzip',ETag:etag,'Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)},body:gzip}):route.fulfill({json:manifest}))
 return {compressedBytes:gzip.length,compressedSha256:hash(gzip)}
}
const snapshot=page=>page.locator('#session-records').evaluate(node=>{
 const offsets=[...node.querySelectorAll('.session-record')].map(n=>Number(n.dataset.offset))
 return {before:Number(node.dataset.historyBefore),after:Number(node.dataset.historyAfter),latest:Number(node.dataset.historyLatest),loading:node.dataset.historyLoading==='true',offsets,sourceGaps:node.querySelectorAll('[data-history-gap]').length,fragmentBytes:[...node.querySelectorAll('[data-fragment-cost]')].reduce((n,e)=>n+Number(e.dataset.fragmentCost),0),top:node.scrollTop,scope:node.dataset.viewerScope,owner:node.dataset.viewerOwner}
})
async function cell(context,instrumented){
 const id=context.id+'-'+(instrumented?'instrumented':'clean'),h=await openSessionExplorer({transcriptBytes:input.bytes,stamp:'2026-11-02T12:00:00Z'}),{page}=h
 const phases=[],assertions=[],samples=[],requests=new Map(),requestTimes=[],inputTimes=[],requestKinds={},observed={active:0,peak:0},started=performance.now();let releaseHeld=null,held=false
 const phase=async(name,fn)=>{const t=performance.now();await fn();phases.push({name,elapsedMs:performance.now()-t});console.log(JSON.stringify({cell:id,phase:name,status:'PASS'}))}
 const sample=async()=>{const s=await snapshot(page);assert.ok(s.offsets.length<=200,'mounted rows');assert.ok(new Set(s.offsets).size===s.offsets.length,'unique occurrences');assert.ok(s.offsets.every((x,i)=>i===0||x>s.offsets[i-1]),'source ordering');assert.ok(s.sourceGaps<=201);assert.ok(s.fragmentBytes<=1048576);samples.push(s);return s}
 let cdp,tracePaint={count:0,durationMs:0,prePaintCount:0,prePaintDurationMs:0,dataLoss:false},traceDone,baseline
 const requestStart=request=>{if(!request.url().includes('/sessions'))return;const u=new URL(request.url()),kind=u.pathname.endsWith('/transcript')?'transcript':u.pathname.endsWith('/content')?'archiveContent':u.pathname.endsWith('/archive')?'archiveManifest':'metadata';requests.set(request,{start:performance.now(),kind});observed.active++;observed.peak=Math.max(observed.peak,observed.active);requestKinds[kind]=(requestKinds[kind]??0)+1}
 const requestEnd=request=>{const x=requests.get(request);if(!x)return;requests.delete(request);observed.active--;assert.ok(requestTimes.length<4096,'finite request observation bound');requestTimes.push(performance.now()-x.start)}
 page.on('request',requestStart);page.on('requestfinished',requestEnd);page.on('requestfailed',requestEnd)
 try{
  if(instrumented){
   await page.route('**/assets/index-*.js',route=>route.fulfill({status:200,contentType:'text/javascript',body:patched.main.source}))
   await page.context().route(url=>url.pathname==='/assets/'+workerName,route=>route.fulfill({status:200,contentType:'text/javascript',body:patched.worker.source}))
   await page.addInitScript(({name})=>{window.__readerWorkerMetrics=[];const Native=window.Worker;window.Worker=class extends Native{constructor(url,options){super(url,options);if(new URL(String(url),location.href).pathname!=='/assets/'+name)return;const index=window.__readerWorkerMetrics.length;if(index>=16)throw Error('qualification_worker_observation_bound');window.__readerWorkerMetrics.push(null);this.addEventListener('message',event=>{if(event.data?.__hmemReaderNumeric)window.__readerWorkerMetrics[index]=event.data.__hmemReaderNumeric})}}},{name:workerName})
  }
  await page.addInitScript(()=>{const s=window.__readerSensors={rafCount:0,rafMaxMs:0,rafOver50:0,longTasks:0,longTaskMs:0};let last;const tick=t=>{if(last!==undefined){const d=t-last;s.rafCount++;s.rafMaxMs=Math.max(s.rafMaxMs,d);if(d>50)s.rafOver50++}last=t;requestAnimationFrame(tick)};requestAnimationFrame(tick);if(PerformanceObserver.supportedEntryTypes.includes('longtask'))new PerformanceObserver(list=>{for(const e of list.getEntries()){s.longTasks++;s.longTaskMs+=e.duration}}).observe({type:'longtask',buffered:true})})
  const archiveReceipt=context.sealed?await archive(h):null
  if(!context.sealed){const stop=input.bytes.length-2097152+131072;await page.route('**/sessions/**/transcript**',async route=>{const u=new URL(route.request().url());if(!held&&Number(u.searchParams.get('offset'))===stop){held=true;await new Promise(resolve=>{releaseHeld=resolve})}await route.fallback()})}
  await h.start();await h.idle()
  requestTimes.length=0;for(const key of Object.keys(requestKinds))delete requestKinds[key];observed.peak=observed.active
  await page.evaluate(()=>{if(globalThis.__hmemReaderProbe){globalThis.__hmemReaderProbe.metrics={};globalThis.__hmemReaderProbe.caps={rows:0,retainedBytes:0,rawBytes:0}};for(const key of Object.keys(window.__readerSensors))window.__readerSensors[key]=0})
  cdp=await page.context().newCDPSession(page);await cdp.send('Performance.enable');baseline=(await cdp.send('Performance.getMetrics')).metrics
  traceDone=new Promise(resolve=>cdp.once('Tracing.tracingComplete',resolve));cdp.on('Tracing.dataCollected',({value})=>{for(const event of value){if(event.ph==='X'&&event.name==='Paint'){tracePaint.count++;tracePaint.durationMs+=(event.dur??0)/1000}if(event.ph==='X'&&event.name==='PrePaint'){tracePaint.prePaintCount++;tracePaint.prePaintDurationMs+=(event.dur??0)/1000}}});cdp.on('Tracing.bufferUsage',value=>{tracePaint.dataLoss ||= !!value.dataLossOccurred})
  await cdp.send('Tracing.start',{categories:'devtools.timeline',transferMode:'ReportEvents'})
  await phase('open-and-useful-progress',async()=>{
   if(context.surface==='task-dock')await page.locator('#entity-'+h.fixture.tasks[0].id+' .task-sessions-icon').click()
   else await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+(context.sealed?h.saved.id:h.active.id)+(context.task?'&task='+h.fixture.tasks[0].id:''))
   if(!context.sealed){await until(()=>releaseHeld!==null,'held recovery response');await until(async()=>await page.locator('.session-record').count()>0,'intermediate rows');const s=await sample();assert.ok(s.loading&&s.after<input.bytes.length);assertions.push('complete intermediate rows visible while later response held');releaseHeld();releaseHeld=null}
   await page.locator('.session-record').first().waitFor();await until(async()=>!(await snapshot(page)).loading,'initial load');await h.idle();await twoFrames(page);await sample()
  })
  await phase('native-scroll-history-resize',async()=>{
   const view=page.locator('#session-records');await view.hover()
   for(let i=0;i<3;i++){const t=performance.now();await page.mouse.wheel(0,-650);await twoFrames(page);inputTimes.push(performance.now()-t);await h.idle();await sample()}
   const owner=(await snapshot(page)).owner
   const readingKey=await view.evaluate(node=>{const box=node.getBoundingClientRect();return [...node.querySelectorAll('[data-block-key]')].find(row=>{const rect=row.getBoundingClientRect();return rect.height>0&&rect.bottom>box.top&&rect.top<box.bottom})?.dataset.blockKey})
   assert.ok(readingKey,'a visible source block before resize')
   await page.setViewportSize({width:1180,height:900});await h.idle();await twoFrames(page);assert.equal((await sample()).owner,owner)
   await page.setViewportSize({width:1440,height:1000});await h.idle();await twoFrames(page);await sample()
   assert.ok(await view.evaluate((node,key)=>{const row=[...node.querySelectorAll('[data-block-key]')].find(row=>row.dataset.blockKey===key);if(!row)return false;const box=node.getBoundingClientRect(),rect=row.getBoundingClientRect();return rect.height>0&&rect.bottom>box.top&&rect.top<box.bottom},readingKey),'source reading block remains visible after resize round trip')
   assertions.push('native history motion and resize preserve visible source block, viewer ownership and ordered bounded occurrences')
  })
  await phase('activity-occurrences',async()=>{
   const view=page.locator('#session-records');await view.focus();await page.keyboard.press('Home');await h.idle();await twoFrames(page)
   for(let i=0;i<24&&await page.locator('.session-activity-summary').count()===0;i++){await continuousScrollStep(page,-1);await h.idle()}
   const summaries=page.locator('.session-activity-summary');assert.ok(await summaries.count()>0,'activity run reachable')
   await until(async()=>/900 events/.test(await summaries.allTextContents().then(x=>x.join(' '))),'exact 900 source contributions')
   const summary=summaries.filter({hasText:'900 events'}).first();await summary.click();await h.idle();await twoFrames(page);await sample()
   assertions.push('activity summary retains exactly 900 distinct source contributions; opened rows keep occurrence ordering')
  })
  await phase('logical-latest-and-append',async()=>{
   const view=page.locator('#session-records');await view.focus();await page.keyboard.press('End');await h.idle();await twoFrames(page)
   const jump=page.getByRole('button',{name:'Jump to latest',exact:true});if(await jump.isVisible())await jump.click()
   await until(async()=>context.sealed?await view.evaluate((node,offset)=>node.dataset.archiveCanNext==='false'&&node.dataset.renderMore==='false'&&node.dataset.lastProseMore==='false'&&!!node.querySelector('[data-offset="'+offset+'"]'),input.description.finalOffset):((await snapshot(page)).after===(await snapshot(page)).latest),'logical latest');await h.idle();await sample()
   if(!context.sealed){await h.appendRecords({text:'Qualification append while following.'});await until(()=>page.getByText('Qualification append while following.',{exact:true}).isVisible(),'following append');await view.hover();await page.mouse.wheel(0,-650);await h.idle();await until(()=>jump.isVisible(),'manual pause');await h.appendRecords({text:'Qualification append while paused.'});await h.idle();assert.equal(await jump.isVisible(),true);await jump.click();await until(()=>page.getByText('Qualification append while paused.',{exact:true}).isVisible(),'resume latest');assertions.push('live append follows, native historical intent pauses, explicit latest resumes')}
   else assertions.push('verified sealed reader reaches logical latest without a live append capability')
  })
  await h.idle();await twoFrames(page);await sample()
  const after=(await cdp.send('Performance.getMetrics')).metrics,performanceDelta={}
  for(const name of ['ScriptDuration','TaskDuration','LayoutDuration','RecalcStyleDuration','LayoutCount','RecalcStyleCount'])performanceDelta[name]=(after.find(x=>x.name===name)?.value??0)-(baseline.find(x=>x.name===name)?.value??0)
  await cdp.send('Tracing.end');let traceTimer
  try{const done=await Promise.race([traceDone,new Promise((_,reject)=>{traceTimer=setTimeout(()=>reject(Error('trace_cleanup_timeout')),10000)})]);tracePaint.dataLoss ||= !!done.dataLossOccurred;assert.equal(tracePaint.dataLoss,false,'complete trace attribution')}finally{clearTimeout(traceTimer)}
  const probe=instrumented?await page.evaluate(()=>globalThis.__hmemReaderProbe):null,workers=instrumented?await page.evaluate(()=>window.__readerWorkerMetrics):[]
  if(instrumented){assert.ok(probe.metrics.elmUpdate?.count>0);assert.ok(probe.metrics.elmDrawEntry?.count>0);assert.ok(probe.metrics.bridgeCommandEntry?.count>0);if(context.sealed)assert.ok(workers.some(w=>w?.rawParse?.count>0),'worker numeric snapshots: '+JSON.stringify(workers));else assert.ok(probe.metrics.rawParse?.count>0);assert.ok(probe.caps.rows<=200);assert.ok(probe.caps.retainedBytes<=4194304);assert.ok(probe.caps.rawBytes<=4194304)}
  const transcript=h.reads.filter(x=>x.path.endsWith('/transcript'));assert.ok(transcript.every(x=>Number(new URLSearchParams(x.query).get('limit'))<=65536));assert.ok(h.peak()<=2);assert.deepEqual(h.errors,[]);assert.deepEqual(h.unhandled,[])
  const environment={browser:page.context().browser().version(),playwright:playwrightVersion,executableSha256:hash(await readFile(h.browserImage.executable)),viewport:page.viewportSize(),timezone:'UTC',mode:h.browserImage.mode}
  return {id,lifecycle:context.sealed?'sealed':'active',scope:context.task?'task-linked':'workspace',surface:context.surface,navigationOrigin:context.surface==='task-dock'?'Projects task Active Agents button':context.task?'canonical task-filtered Sessions route':'workspace Sessions route',instrumented,elapsedMs:performance.now()-started,phases,assertions,metrics:{requests:{counts:requestKinds,roundTrip:aggregate(requestTimes),observedSessionPeak:observed.peak,fixtureSourcePeak:h.peak(),transcriptRanges:transcript.length,maxTranscriptLimit:Math.max(0,...transcript.map(x=>Number(new URLSearchParams(x.query).get('limit'))))},inputToTwoFramesHostProxy:aggregate(inputTimes),sampledBounds:{samples:samples.length,mountedRows:Math.max(...samples.map(x=>x.offsets.length)),sourceGaps:Math.max(...samples.map(x=>x.sourceGaps)),fragmentBytes:Math.max(...samples.map(x=>x.fragmentBytes))},cdp:performanceDelta,paintTrace:tracePaint,sensors:await page.evaluate(()=>window.__readerSensors),mainProbe:probe,workerProbes:workers},archive:archiveReceipt,environment}
 }finally{releaseHeld?.();await h.close()}
}
const contexts=[{id:'active-workspace',sealed:false,task:false,surface:'Sessions'},{id:'active-task',sealed:false,task:true,surface:'task-dock'},{id:'sealed-workspace',sealed:true,task:false,surface:'Sessions'},{id:'sealed-task',sealed:true,task:true,surface:'Sessions'}]
const cells=[]
for(const context of contexts)for(const instrumented of [false,true]){const id=context.id+'-'+(instrumented?'instrumented':'clean');if(option('--only')&&option('--only')!==id)continue;cells.push(await cell(context,instrumented))}
assert.ok(cells.length>0,'unknown --only cell')
for(const [path,expected] of Object.entries(sources))assert.equal(hash(await readFile(new URL(path,root))),expected,'source freshness: '+path)
assert.equal(hash(await readFile(new URL(mainName,assetDir),'utf8')),hash(main),'main asset freshness');assert.equal(hash(await readFile(new URL(workerName,assetDir),'utf8')),hash(worker),'worker asset freshness')
assert.equal(hash(await readFile(fileURLToPath(import.meta.url))),provenance.harnessSha256,'harness freshness')
const report={schemaVersion:1,taskId:'e6c5b3b6-9984-421b-af24-ccc8a59d8e06',recordedAt:new Date().toISOString(),status:'PASS',retention:{scope:'bounded reader qualification summary; no raw transcript/trace/samples',reason:'reproducible release qualification and phase attribution',owner:'project maintainer',disposition:'retain until explicitly superseded'},provenance,host:{node:process.version,platform:process.platform,release:release(),arch:process.arch,cpu:cpus()[0]?.model,logicalCpus:cpus().length,totalMemoryBytes:totalmem()},cells,limitations:['Synthetic local HTTP fixture measures frontend mechanics, not production database/network latency.','Clean cells retain light RAF/long-task/CDP observation overhead; instrumented cells separately attribute compiled production bindings.','Bridge entry is synchronous callback invocation elapsed; lifetime includes awaits and is not CPU. Nested parse/update/draw and CDP style/layout can overlap; never sum these into exclusive CPU or wall time.','Raw parse units are UTF-16 code units. Worker durations use worker-local clocks.','DOM bounds are sampled at phase boundaries, not a full heap or every transient frame measurement.','Task dock supports active readers only. Sealed task-linked reader uses canonical task-filtered full Sessions route; Evidence-click navigation is not measured.','Finite workflow deadlines are correctness/cleanup guards, not latency budgets. No cross-machine, cross-browser, or hard timing threshold is introduced.']}
const json=JSON.stringify(report,null,2)+'\n';assert.ok(Buffer.byteLength(json)<=131072,'bounded summary <=128KiB')
if(!option('--only')){assert.equal(cells.length,8);await writeFile(new URL('session-reader-history.qualification.v1.json',import.meta.url),json)}
console.log(JSON.stringify({status:'PASS',cells:cells.map(x=>({id:x.id,elapsedMs:x.elapsedMs})),summaryBytes:Buffer.byteLength(json),partial:!!option('--only')}))
