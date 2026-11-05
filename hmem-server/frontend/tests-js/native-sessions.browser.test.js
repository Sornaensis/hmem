import assert from 'node:assert/strict'
import test from 'node:test'
import {gzipSync} from 'node:zlib'
import {createHash} from 'node:crypto'
import {openSessionExplorer,continuousScrollStep,traceSessionBridge} from './support/sessions-ux-fixture.mjs'
import {createEventPayloadPager} from '../src/session-event-json.js'
import {copilotRecords,piRecords} from './support/native-session-fixtures.mjs'

const hash=value=>createHash('sha256').update(value).digest('hex')
const generation='33333333-3333-4333-8333-333333333333'
const delay=milliseconds=>new Promise(resolve=>setTimeout(resolve,milliseconds))
const until=async action=>{const end=Date.now()+12000;while(Date.now()<end){if(await action())return;await delay(25)}throw Error('native session did not converge')}
async function watchSealedPrefix(host){
 await host.evaluate(node=>{const parts=new Map();node.nativePayloadPrefix={parts,error:null};const collect=()=>{for(const part of node.querySelectorAll('[data-body-part-index]')){const index=Number(part.dataset.bodyPartIndex);if(index>1)continue;const text=part.textContent;if(parts.has(index)&&parts.get(index)!==text)node.nativePayloadPrefix.error='a repeated sealed payload part changed';parts.set(index,text)}};node.nativePayloadObserver=new MutationObserver(collect);node.nativePayloadObserver.observe(node,{childList:true,subtree:true});collect()})
}
async function payloadPrefix(page,host,source,active=false,complete=false){
 const key=await host.getAttribute('data-event-key');if(key)host=page.locator('[data-event-key="'+key+'"]')
 const selector=active?'[data-event-json-part]':'[data-body-part-index]',attribute=active?'eventJsonPart':'bodyPartIndex',parts=new Map(),pager=createEventPayloadPager(source,active?1024:16384)
 const collect=async()=>{const observed=active?[]:await host.evaluate(node=>{if(node.nativePayloadPrefix?.error)throw Error(node.nativePayloadPrefix.error);return [...(node.nativePayloadPrefix?.parts||[])].map(([part,text])=>({part,text}))});for(const part of [...observed,...await host.locator(selector).evaluateAll((nodes,attribute)=>nodes.map(node=>({part:Number(node.dataset[attribute]),text:node.textContent})),attribute)]){if(parts.has(part.part))assert.equal(parts.get(part.part),part.text,'a repeated keyed payload part stays exact');parts.set(part.part,part.text)}}
 let wanted=2;if(complete){wanted=0;while(pager.read(wanted++).more)assert.ok(wanted<100)}
 for(let step=0;Array.from({length:wanted},(_,index)=>index).some(index=>!parts.has(index));step++){assert.ok(step<100,JSON.stringify({phase:'native-payload-prefix',active,key,parts:[...parts.keys()].filter((_,index,all)=>index<4||index>=all.length-4),state:await page.locator('#session-records').evaluate(node=>({...node.dataset,top:node.scrollTop,height:node.clientHeight,scroll:node.scrollHeight,bodyGaps:[...node.querySelectorAll('[data-body-gap]')].slice(0,4).map(gap=>({...gap.dataset,top:gap.getBoundingClientRect().top-node.getBoundingClientRect().top}))}))}));await collect();if(Array.from({length:wanted},(_,index)=>index).some(index=>!parts.has(index))){await (active?host:page.locator('#session-records')).press('PageDown');await delay(100)}}
 assert.equal(parts.get(0),pager.read(0).text);assert.equal(parts.get(1),pager.read(1).text)
 if(active)assert.ok(await host.evaluate(node=>new TextEncoder().encode(node.outerHTML).length)<=262144)
 else await host.evaluate(node=>{node.nativePayloadObserver?.disconnect();delete node.nativePayloadObserver;delete node.nativePayloadPrefix})
 return [...parts].sort((a,b)=>a[0]-b[0]).map(([,text])=>text).join('')
}
const resync=async h=>{const response=h.page.waitForResponse(response=>new URL(response.url()).pathname==='/api/v1/change-stream/resync'&&response.request().method()==='POST');await h.page.evaluate(()=>window.pushHierarchyFrames([{schema_version:1,type:'resync_required'}]));await response;await h.idle()}
async function activeControl(h){await h.page.route('**/api/v1/session-activity?*',async route=>{const url=new URL(route.request().url()),active=h.activeSessions.some(session=>session.state==='active'),rows=key=>url.searchParams.getAll(key).map(id=>({id,active}));await route.fulfill({json:{workspaces:rows('workspace_id'),projects:rows('project_id')}})})}
async function assertNativeContent(page,format){
 const records=page.locator('#session-records'),groups=records.locator('.session-activity-group')
 for(let index=0;index<await groups.count();index++)if(await groups.nth(index).getAttribute('open')===null)await groups.nth(index).locator(':scope > summary').click()
 for(const kind of ['thinking','tool','result']){
  const details=records.locator('.session-content-'+kind)
  for(let index=0;index<await details.count();index++)if(await details.nth(index).getAttribute('open')===null)await details.nth(index).locator(':scope > summary').click()
 }
 // Completed bodies hydrate asynchronously after their native toggle event.
 await until(async()=>{const text=await records.innerText();return format==='Copilot'?text.includes('Missing file')&&text.includes('complete hello λ😀'):text.includes('pi tool output λ😀')&&text.includes('visible bash output')})
 const content=await records.innerText()
 if(format==='Copilot'){
  for(const expected of ['Message thinking','Check the source first.','Request · read_file · call-1','Execution · read_file · call-1','Result · call-1','Error result · call-error','Missing file','complete hello λ😀'])assert.ok(content.includes(expected),expected+' visible')
  assert.equal(await records.locator('.session-content-tool').count(),2);assert.equal(await records.locator('.session-content-result').count(),2)
 }else{
  for(const expected of ['Check pi input.','Thinking...','pi-call','pi tool output λ😀','Alternate pi answer.','Compaction summary','Branch summary','Visible pi custom message.','visible bash output','Later pi root.'])assert.ok(content.includes(expected),expected+' visible')
  for(const hidden of ['redacted-secret','opaque-secret','hidden-extension-state','hidden-custom-message','not-authoritative'])assert.ok(!content.includes(hidden),hidden+' hidden')
  assert.equal(await records.locator('.session-content-tool').count(),2);assert.equal(await records.locator('.session-content-result').count(),2)
 }
}

// Observe one actual connected sidebar ring across transcript work. Keep only
// bounded aggregates and 256 samples; no performance trace/profile is retained.
async function startProbe(page){
 await page.locator('.sidebar .session-activity-ring').first().waitFor()
 await page.evaluate(()=>{
  const node=document.querySelector('.sidebar .session-activity-ring'),samples=[]
  let previousAnimation=null,previousTime=null,resets=0,replacements=0,maxFrameGap=0,lastFrame=performance.now(),frames=0,raf,observer=null,maxLongTask=0,longTasks=0
  const tick=now=>{maxFrameGap=Math.max(maxFrameGap,now-lastFrame);lastFrame=now;frames++;raf=requestAnimationFrame(tick)}
  raf=requestAnimationFrame(tick)
  if(globalThis.PerformanceObserver?.supportedEntryTypes?.includes('longtask')){observer=new PerformanceObserver(list=>{for(const entry of list.getEntries()){longTasks++;maxLongTask=Math.max(maxLongTask,entry.duration)}});observer.observe({type:'longtask',buffered:false})}
  const sample=()=>{
   const animation=node.getAnimations()[0],currentTime=typeof animation?.currentTime==='number'?animation.currentTime:null,style=getComputedStyle(node)
   if(previousAnimation && animation!==previousAnimation)replacements++
   if(previousTime!==null && currentTime!==null && currentTime<previousTime-20)resets++
   previousAnimation=animation;previousTime=currentTime
   if(samples.length===256)samples.shift()
   samples.push({at:performance.now(),currentTime,transform:style.transform,connected:node.isConnected,visibility:document.visibilityState,reduced:matchMedia('(prefers-reduced-motion: reduce)').matches,name:style.animationName,duration:style.animationDuration,playState:style.animationPlayState})
  }
  sample();const timer=setInterval(sample,80)
  window.nativeRingProbe={stop(){sample();clearInterval(timer);cancelAnimationFrame(raf);observer?.disconnect();delete window.nativeRingProbe;return {samples,resets,replacements,maxFrameGap,frames,longTasks,maxLongTask,longTasksSupported:!!observer}}}
 })
}
async function stopProbe(page,label){
 await delay(700)
 const receipt=await page.evaluate(()=>window.nativeRingProbe.stop()),samples=receipt.samples
 assert.ok(samples.length>=6,label+' multiple observations')
 assert.ok(samples.every(sample=>sample.connected && sample.visibility==='visible' && !sample.reduced && sample.name==='session-activity-spin' && sample.duration==='1.8s' && sample.playState==='running'),JSON.stringify({label,receipt}))
 assert.equal(receipt.resets,0,label+' no animation time resets');assert.equal(receipt.replacements,0,label+' stable animation object')
 assert.ok(samples.at(-1).currentTime-samples[0].currentTime>500,label+' animation currentTime advances')
 let lastProgress=samples[0].at,lastTime=samples[0].currentTime,maxStall=0
 for(const sample of samples){if(sample.currentTime>lastTime+1){maxStall=Math.max(maxStall,sample.at-lastProgress);lastProgress=sample.at;lastTime=sample.currentTime}}
 maxStall=Math.max(maxStall,samples.at(-1).at-lastProgress)
 assert.ok(maxStall<700,label+' no sustained animation stall: '+maxStall)
 assert.ok(new Set(samples.map(sample=>sample.transform)).size>3,label+' computed transforms advance')
 console.log(JSON.stringify({scenario:label,observations:samples.length,animationAdvanceMs:samples.at(-1).currentTime-samples[0].currentTime,maxStallMs:maxStall,resets:receipt.resets,replacements:receipt.replacements,maxFrameGapMs:receipt.maxFrameGap,longTasksSupported:receipt.longTasksSupported,longTasks:receipt.longTasks,maxLongTaskMs:receipt.maxLongTask}))
}
async function installArchive(h){
 await h.page.addInitScript(()=>{const Native=Worker,trace=[];window.nativeWorkerTrace=trace;const record=(direction,data)=>{if(!['page','body','error','retire','ready','adopt'].includes(data.kind))return;trace.push({direction,kind:data.kind,request:data.request,load:data.load??data.stamp?.load,key:data.key,part:data.part??data.body?.part,before:data.before,after:data.after,last:data.last,first:data.first,rows:data.rows?.length,code:data.code,dom:(()=>{const node=document.getElementById('session-records');return node?{first:node.dataset.sealedFirst,last:node.dataset.sealedLast,request:node.dataset.sealedPageRequest,part:node.dataset.renderPart}:null})()});if(trace.length>48)trace.shift()};window.Worker=class extends Native{constructor(...args){super(...args);this.addEventListener('message',({data})=>record('reply',data))}postMessage(data,transfer){record('request',data);return super.postMessage(data,transfer)}}})
 await h.page.route('**/sessions/**/archive**',async route=>{
  const id=new URL(route.request().url()).pathname.split('/sessions/')[1].split('/')[0],raw=h.content.get(id),gzip=gzipSync(raw),headers={ETag:'"native-completed"','Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)}
  if(new URL(route.request().url()).pathname.endsWith('/content'))return route.request().headers()['if-none-match']?route.fulfill({status:304,headers:{ETag:headers.ETag}}):route.fulfill({body:gzip,headers})
  await delay(350)
  return route.fulfill({json:{workspace_id:h.fixture.workspace.id,session_id:id,state:'ready',generation_id:generation,codec:'gzip-v1',raw_bytes:String(raw.length),raw_records:String(raw.toString('utf8').split('\n').filter(Boolean).length),raw_sha256:hash(raw),compressed_bytes:String(gzip.length),compressed_sha256:hash(gzip),content_path:`/api/v1/workspaces/${h.fixture.workspace.id}/sessions/${id}/archive/${generation}/content`}})
 })
}

test('sealed inspectable events rebuild an old index and retain the complete group time across pages and reload',{timeout:60000},async()=>{
 const final='2026-10-30T09:00:00Z',values=Array.from({length:140},(_,index)=>({type:index===0?'future.tool_call':'event_msg',timestamp:index===139?final:'2026-10-30T12:00:00Z',payload:{type:'future_event',index,result:'needle '+index,...(index===0?{large:'x'.repeat(19000)}:{})}}))
 const raw=Buffer.from(values.map(value=>JSON.stringify(value)).join('\n')+'\n'),h=await openSessionExplorer({transcriptBytes:raw}),{page}=h
 try{
  // Downgrade one real completed derived index immediately before adoption.
  // The current viewer must discard it and rebuild from the same evidence.
  await page.addInitScript(()=>{
   const NativeWorker=window.Worker
   window.Worker=class extends NativeWorker {
    postMessage(message,transfer){
     if(message.kind!=='adopt' || window.oldEventIndexInjected)return super.postMessage(message,transfer)
     window.oldEventIndexInjected=true
     const send=()=>super.postMessage(message,transfer),opening=indexedDB.open('hmem-session-archive-v1-'+message.name)
     opening.onsuccess=()=>{const db=opening.result,tx=db.transaction('meta','readwrite'),store=tx.objectStore('meta'),request=store.get('verified')
      request.onsuccess=()=>store.put({...request.result,indexVersion:2});tx.oncomplete=()=>{db.close();send()};tx.onerror=()=>{db.close();throw Error('index downgrade fixture failed')}
     }
    }
   }
  })
  await installArchive(h);await h.start();await h.idle();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id)
  await page.getByText('Verified complete archive.',{exact:true}).waitFor({timeout:8000}).catch(async error=>{console.error(JSON.stringify({phase:'stale-index-rebuild',text:(await page.locator('body').innerText()).slice(-1400),errors:h.errors}));throw error});assert.equal(await page.evaluate(()=>window.oldEventIndexInjected),true)
  const group=page.locator('.session-activity-group').first(),footer=page.locator('.session-archive-footer')
  assert.equal(await group.locator(':scope > summary time').getAttribute('datetime'),final);assert.match(await group.locator(':scope > summary').innerText(),/140 events/)
  await group.locator(':scope > summary').click();await group.locator('article').first().waitFor();await h.idle();await page.locator('#session-records').press('End');await until(()=>page.locator('#session-records').evaluate(node=>node.dataset.latestPending==='false'&&node.scrollHeight-node.scrollTop-node.clientHeight<=4));await page.locator('#session-records').press('Home');await until(async()=>await group.locator('.session-content-event').first().locator('summary').innerText().then(text=>text.includes('future.tool_call')));const event=group.locator('.session-content-event').first();await event.locator('summary').click();await event.locator('pre').waitFor()
  assert.match(await event.locator('summary').innerText(),/Payload · future.tool_call · future_event/)
  assert.deepEqual(JSON.parse(await payloadPrefix(page,event,JSON.stringify(values[0]))),values[0])
  await page.locator('#session-records').press('End');await until(()=>page.locator('#session-records').evaluate(node=>node.dataset.latestPending==='false'&&node.dataset.archiveCanNext==='false'&&node.scrollHeight-node.scrollTop-node.clientHeight<=4));await until(async()=>await page.locator('.session-record').last().getAttribute('data-offset')===String(raw.lastIndexOf(Buffer.from(JSON.stringify(values[139])))))
  assert.equal(await page.locator('.session-activity-group').first().locator(':scope > summary time').getAttribute('datetime'),final)
  for(const width of [1440,430]){await page.setViewportSize({width,height:1000});assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=window.innerWidth))}
  await page.reload();await page.getByText('Verified complete archive.',{exact:true}).waitFor({timeout:20000})
  assert.equal(await page.locator('.session-activity-group').first().locator(':scope > summary time').getAttribute('datetime'),final)
  assert.equal(hash(h.content.get(h.saved.id)),hash(raw));assert.deepEqual(h.errors,[]);assert.deepEqual(h.unhandled,[])
 }finally{await h.close()}
})

test('raw event JSON preserves deep and numeric source tokens in live and sealed browsers',{timeout:60000},async()=>{
 const numeric='{"type":"event_msg","payload":{"type":"precision","n":9007199254740993,"overflow":1e400,"negative":-0,"escaped":"\\u0061","padding":"'+'x'.repeat(19000)+'"}}\n'
 const deep='{"type":"event_msg","payload":'+'['.repeat(10000)+'0'+']'.repeat(10000)+'}\n'
 const raw=Buffer.from(numeric+deep),h=await openSessionExplorer({transcriptBytes:raw}),{page}=h
 const exact=text=>{for(const token of ['9007199254740993','1e400','-0','\\u0061'])assert.ok(text.includes(token),token+' preserved')}
 try{
  await installArchive(h);await h.start();await h.idle();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.active.id)
  const group=page.locator('.session-activity-group').first();await group.locator(':scope > summary').click()
  let events=group.locator('.session-content-event'),first=events.first();await first.locator('summary').click()
  const numericPayload=numeric.trim().slice(numeric.indexOf('"payload":')+10,-1),deepPayload=deep.trim().slice(deep.indexOf('"payload":')+10,-1)
  const json=first.locator('session-event-json');await json.locator('pre').waitFor();exact(await payloadPrefix(page,json,numericPayload,true))
  await events.nth(1).locator('summary').click();const deepJson=events.nth(1).locator('session-event-json');for(let step=0;!await deepJson.locator('pre').count();step++){assert.ok(step<30,'the opened lazy inspector becomes reachable by real scrolling');await page.locator('#session-records').press('PageDown');await delay(100)}
  await payloadPrefix(page,deepJson,deepPayload,true);assert.equal(await deepJson.getByRole('button',{name:/^(Continue|Previous) JSON$/}).count(),0)
  await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id);await page.getByText('Verified complete archive.',{exact:true}).waitFor()
  const sealed=page.locator('.session-activity-group').first();await sealed.locator(':scope > summary').click();events=sealed.locator('.session-content-event');first=events.first();await first.locator('summary').click();await first.locator('pre').first().waitFor()
  exact(await payloadPrefix(page,first,numericPayload));await watchSealedPrefix(events.nth(1));await events.nth(1).locator('summary').click();await events.nth(1).locator('pre').first().waitFor();await page.locator('#session-records').press('Home');await payloadPrefix(page,events.nth(1),deepPayload)
  assert.deepEqual(h.content.get(h.saved.id),raw);assert.deepEqual(h.errors,[]);assert.deepEqual(h.unhandled,[])
 }finally{await h.close()}
})
const nativeMessage=(format,index,text)=>format==='Copilot'?{type:'assistant.message',id:'extra-'+index,parentId:null,data:{messageId:'message-'+index,content:text}}:{type:'message',id:'extra-'+index,parentId:index%7?'extra-'+(index-1):'p1',timestamp:'2026-10-30T10:20:00Z',message:{role:'assistant',content:[{type:'text',text}],timestamp:1793355600000}}

for(const [format,native,first]of[['Copilot',copilotRecords,'Inspect café λ😀'],['pi',piRecords,'Inspect pi café λ😀']])test(format+' native live/dock/archive history preserves evidence while a connected normal-motion ring advances',{timeout:60000},async()=>{
 const values=[...native,...Array.from({length:75},(_,index)=>nativeMessage(format,index,format+' page '+String(index).padStart(3,'0')))],raw=Buffer.from(values.map(value=>JSON.stringify(value)).join('\r\n')+'\r\n'),h=await openSessionExplorer({transcriptBytes:raw,activeCount:2}),{page}=h
 let probing=false,phase='setup'
 try{
  await traceSessionBridge(h);await page.emulateMedia({reducedMotion:'no-preference'});await activeControl(h);await installArchive(h);await h.start();await h.idle()
  phase='start ring probe';await startProbe(page);probing=true
  phase='open native live reader'
  await page.getByRole('button',{name:'Sessions',exact:true}).click();await page.locator('.session-summary a[href*="'+h.active.id+'"]').click();await page.getByText(format+' page 074',{exact:true}).waitFor();await h.idle();await page.locator('#session-records').press('Home');await page.getByText(first,{exact:true}).waitFor()
  phase='assert live native disclosures';await assertNativeContent(page,format)
  phase='restore explicit live follow after disclosure inspection'
  await page.locator('#session-records').press('End')
  await page.waitForFunction(()=>{const node=document.getElementById('session-records');return node.dataset.latestPending==='false'&&Number(node.dataset.historyAfter)>=Number(node.dataset.historyLatest)&&node.scrollHeight-node.scrollTop-node.clientHeight<=4})
  phase='append and resync'
  const appended=format+' appended native message';await h.appendRecords(nativeMessage(format,1000,appended));await page.getByText(appended,{exact:true}).waitFor();await resync(h);await page.getByText(appended,{exact:true}).waitFor()
  phase='open task dock';await page.getByRole('button',{name:'Projects',exact:true}).click();await h.idle();await page.getByRole('button',{name:'Expand All',exact:true}).click();await h.scrollTo('task:'+h.fixture.tasks[0].id);const task=page.locator('#entity-'+h.fixture.tasks[0].id),agent=task.locator(':scope > .card-header .task-sessions-icon');await until(()=>agent.isEnabled());await agent.click();await task.getByText(appended,{exact:true}).waitFor()
  phase='return from dock'
  await task.getByRole('link',{name:'Jump To Session ↗'}).click();await page.getByText(appended,{exact:true}).waitFor()
  const sealed=h.content.get(h.active.id);await page.getByRole('button',{name:'Completed',exact:true}).click();Object.assign(h.active,{state:'terminated',terminated_at:new Date().toISOString(),final_bytes:sealed.length,final_sha256:hash(sealed)})
  phase='prepare completed archive after actual lifecycle update';await resync(h);await page.getByText('Verified complete archive.',{exact:true}).waitFor();await h.idle();await page.locator('#session-records').press('Home');await page.getByText(first,{exact:true}).waitFor()
  phase='assert completed native disclosures';await assertNativeContent(page,format)
  phase='archive paging and download'
  for(let cycle=0;cycle<2;cycle++){
   await page.locator('#session-records').press('End');await until(()=>page.locator('#session-records').evaluate(node=>node.dataset.latestPending==='false'&&node.dataset.archiveCanNext==='false'&&node.dataset.renderMore==='false'&&node.dataset.lastProseMore==='false'&&node.scrollHeight-node.scrollTop-node.clientHeight<=4));await page.getByText(appended,{exact:true}).waitFor()
   await page.locator('#session-records').press('Home');await page.getByText(first,{exact:true}).waitFor();await h.idle();assert.equal(await page.getByText(first,{exact:true}).evaluate(node=>{const r=node.getBoundingClientRect(),box=document.getElementById('session-records').getBoundingClientRect();return r.height>0&&r.bottom>box.top&&r.top<box.bottom}),true,'each real End→Home cycle reaches the physically visible first native occurrence')
  }
  assert.equal(await page.locator('.session-archive-footer').getByRole('button',{name:/^(First|Previous|Next|Last)$/}).count(),0)
  const ready=page.waitForEvent('download');await page.getByRole('button',{name:/Download session/}).click();const download=await ready,parts=[];for await(const part of await download.createReadStream())parts.push(part)
  assert.deepEqual(Buffer.concat(parts),sealed);assert.equal(hash(Buffer.concat(parts)),h.active.final_sha256)
  if(format==='pi'){assert.match(await page.locator('#session-records').innerText(),/root p1|parent p2/);assert.match(await page.locator('#session-records').innerText(),/alternate branches/)}
  await stopProbe(page,format+' load/append/resync/dock/archive/page/download');probing=false
  await page.emulateMedia({reducedMotion:'reduce'});const ring=page.locator('.sidebar .session-activity-ring').first(),before=await ring.evaluate(node=>({name:getComputedStyle(node).animationName,transform:getComputedStyle(node).transform,animations:node.getAnimations().length}));await delay(250);assert.deepEqual(await ring.evaluate(node=>({name:getComputedStyle(node).animationName,transform:getComputedStyle(node).transform,animations:node.getAnimations().length})),before);assert.equal(before.name,'none');assert.equal(before.animations,0)
  assert.ok(h.peak()<=2);assert.deepEqual(h.errors,[]);assert.deepEqual(h.unhandled,[])
 }catch(error){console.error(JSON.stringify({scenario:format,phase,error:String(error),trace:await page.evaluate(()=>window.nativeWorkerTrace.slice(-8)),bridge:await page.evaluate(()=>window.sessionBridgeFacts.filter(fact=>fact.kind==='sealed-body'||fact.kind==='sealed-error'||fact.direction==='command'&&fact.markdownAck?.request>0).map(({dom,...fact})=>({...fact,dom:{part:dom?.part,top:dom?.top}})).slice(-12)),state:await page.locator('#session-records').evaluate(node=>({...node.dataset,top:node.scrollTop,height:node.clientHeight,total:node.scrollHeight,groups:[...node.querySelectorAll('[data-group-key]')].map(group=>({key:group.dataset.groupKey,open:group.open})),first:node.textContent.slice(0,350)})).catch(()=>null),text:(await page.locator('body').innerText().catch(()=>'' )).slice(-600)}));throw error}
 finally{if(probing)await page.evaluate(()=>window.nativeRingProbe?.stop()).catch(()=>{});await h.close()}
})

for(const [scenario,bytes,expected]of[
 ['unfamiliar-only',Buffer.from(JSON.stringify({type:'future.output',data:{content:{unknown:true,large:'x'.repeat(19000)}}})+'\n'),'Payload · future.output'],
 ['failure-without-error',Buffer.from('{"type":"hook.end","data":{"success":false}}\n'),'Transcript operation failed (hook.end). Original evidence is preserved.'],
 ['malformed',Buffer.from('{\n'),'Record window could not be decoded. Retry or load another window.'],
 ['partial',Buffer.from('{"type":'),'Incomplete record at this finite window boundary.'],
])test(scenario+' settles finitely while unrelated active-ring animation advances',{timeout:30000},async()=>{
 const h=await openSessionExplorer({transcriptBytes:bytes}),{page}=h;let probing=false
 try{
  await page.emulateMedia({reducedMotion:'no-preference'});await activeControl(h);await h.start();await h.idle();await startProbe(page);probing=true
  await page.getByRole('button',{name:'Sessions',exact:true}).click();await page.locator('.session-summary a[href*="'+h.active.id+'"]').click()
  if(scenario==='unfamiliar-only')await page.locator('.session-activity-group').first().locator(':scope > summary').click()
  await page.getByText(scenario==='partial'?'No complete messages in this window.':expected,{exact:false}).first().waitFor({timeout:8000}).catch(async error=>{throw Error(JSON.stringify({scenario,text:(await page.locator('body').innerText()).slice(-1200),state:await page.locator('#session-records').evaluate(node=>({...node.dataset})),error:error.message}))})
  assert.equal(await page.getByText(expected,{exact:false}).count(),1,'the finite input has an explicit visible disposition')
  if(scenario==='unfamiliar-only'){
   const event=page.locator('.session-content-event').first();await event.locator('summary').click();const json=event.locator('session-event-json');await json.locator('pre').waitFor();assert.deepEqual(JSON.parse(await payloadPrefix(page,json,bytes.toString().trim(),true,true)),JSON.parse(bytes.toString()));assert.equal(await page.getByText(/Unsupported transcript content/).count(),0)
  }
  await h.idle();const reads=h.reads.filter(value=>value.path.endsWith('/transcript')).length;await delay(500);assert.equal(h.reads.filter(value=>value.path.endsWith('/transcript')).length,reads,'settled input does not request-loop');assert.equal(await page.getByRole('status',{name:/^(Reading transcript|Loading transcript metadata|Loading session details)$/}).count(),0)
  if(scenario==='partial'){
   await h.appendBytes(Buffer.from('"text","text":"Completed partial source"}\n'))
   await page.getByText('Completed partial source',{exact:true}).waitFor();await until(()=>page.locator('#session-records').evaluate(node=>node.dataset.historyLoading==='false'))
   assert.equal(await page.locator('.session-record[data-offset="0"]').count(),1,'real growth reframes the incomplete prefix exactly once')
   assert.ok(h.reads.filter(value=>value.path.endsWith('/transcript')).slice(reads).some(value=>new URLSearchParams(value.query).get('offset')==='0'))
   assert.equal(await page.getByText(expected,{exact:false}).count(),0)
  }
  await stopProbe(page,scenario);probing=false;assert.deepEqual(h.errors,[]);assert.deepEqual(h.unhandled,[])
 }finally{if(probing)await page.evaluate(()=>window.nativeRingProbe?.stop()).catch(()=>{});await h.close()}
})
