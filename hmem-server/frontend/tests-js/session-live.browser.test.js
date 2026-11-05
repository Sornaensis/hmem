import assert from 'node:assert/strict'
import test from 'node:test'
import { open } from 'node:fs/promises'
import { createHash } from 'node:crypto'
import { join, resolve } from 'node:path'
import { openSessionExplorer,continuousScrollStep } from './support/sessions-ux-fixture.mjs'
import { createRecordFramer } from '../src/session-viewer.js'

const message=text=>({type:'response_item',payload:{type:'message',role:'assistant',content:[{type:'output_text',text}]}})
const call=index=>({type:'response_item',payload:{type:'function_call',name:'inspect',arguments:JSON.stringify({index})}})
const until=async fn=>{const end=Date.now()+12000;while(Date.now()<end){if(await fn())return;await new Promise(resolve=>setTimeout(resolve,25))}throw Error('Live transcript did not converge')}
const enter=async(h,dock=false)=>{await h.start();await h.idle();if(dock){const button=h.page.locator('#entity-'+h.fixture.tasks[0].id+' .task-sessions-icon');await until(()=>button.isEnabled());await button.click()}else{await h.page.getByRole('button',{name:'Sessions',exact:true}).click();await h.page.locator('.session-summary a').first().click()}await h.page.locator('.session-record:not(.session-record-activity) .session-record-text').first().waitFor()}
const resync=async h=>{const response=h.page.waitForResponse(response=>new URL(response.url()).pathname==='/api/v1/change-stream/resync'&&response.request().method()==='POST');await h.page.evaluate(()=>window.pushHierarchyFrames([{schema_version:1,type:'resync_required'}]));await response;await h.idle()}

for(const dock of [false,true])test(`progress coalesces ${dock?'dock':'Sessions'} reads without refreshing unrelated membership`,{timeout:45000},async t=>{
 const h=await openSessionExplorer(),{page}=h
 let activityReads=0
 try{
  page.on('request',request=>{if(new URL(request.url()).pathname==='/api/v1/session-activity')activityReads++})
  await enter(h,dock);await page.waitForTimeout(1200);await h.idle()
  const initialReads=h.reads.length,initialActivity=activityReads
  // Keep several canonical updates ahead of the HTTP response. The producer is
  // independent of visibility; only the final coalesced record is awaited.
  await page.route('**/api/v1/**/sessions/'+h.active.id,async route=>{await new Promise(resolve=>setTimeout(resolve,90));await route.fallback()})
  const started=performance.now()
  for(let index=0;index<24;index++){
   await h.appendRecords(message('Coalesced progress '+index))
   await new Promise(resolve=>setTimeout(resolve,15))
  }
  const produced=performance.now()
  await page.getByText('Coalesced progress 23',{exact:true}).waitFor()
  const converged=performance.now();await h.idle()
  const reads=h.reads.slice(initialReads),metadata=reads.filter(row=>row.path.endsWith('/sessions/'+h.active.id)),ranges=reads.filter(row=>row.path.endsWith('/transcript'))
  t.diagnostic(JSON.stringify({mode:dock?'dock':'sessions',events:24,paths:reads.reduce((counts,row)=>({...counts,[row.path]:(counts[row.path]||0)+1}),{}),activityBefore:initialActivity,activityAfter:activityReads,peak:h.peak(),producerMs:produced-started,tailDomConvergenceMs:converged-produced,scope:'intercepted HTTP/WS production assets; DOM witness, not paint or real backend qualification'}))
  assert.equal(reads.filter(row=>row.path.endsWith('/sessions')||row.path.endsWith('/filters')||row.path.endsWith('/tasks')).length,0,'output does not re-read lists, facets or links')
  assert.equal(activityReads,initialActivity,'output does not probe active-session projections')
  assert.ok(metadata.length>0&&metadata.length<24,'pending progress coalesces into fewer metadata reads than events')
  assert.ok(ranges.length>0);assert.ok(h.peak()<=2);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

for(const dock of [false,true])test(`canonical refresh keeps ${dock?'dock':'Sessions'} metadata chrome and transcript geometry while independent responses are pending`,{timeout:60000},async()=>{
 const h=await openSessionExplorer({runtime:{model:'Stable runtime',reasoning:'high'}}),{page}=h
 let heldKind=null
 const releases=[]
 const kind=path=>path.endsWith('/filters')?'facets':path.endsWith('/sessions')?'list':path.endsWith('/tasks')?'links':path.endsWith(h.active.id)?'metadata':null
 try{
  await page.route('**/api/v1/**',async route=>{if(heldKind&&kind(new URL(route.request().url()).pathname)===heldKind)await new Promise(resolve=>releases.push(resolve));await route.fallback()})
  await enter(h,dock);await h.idle()
  const group=page.locator('.session-activity-group').first();await group.locator(':scope > summary').click();await group.locator('.session-content-tool summary').first().click();await h.idle()
  await page.evaluate(async()=>{await document.fonts.ready;for(let index=0;index<8;index++)await new Promise(resolve=>requestAnimationFrame(resolve))})
  await page.evaluate(()=>{
   const root=document.getElementById('session-records')
   window.chromeNodes=[root,root.querySelector('[data-block-key]'),root.querySelector('.session-activity-group'),root.querySelector('.session-content-tool'),document.querySelector(':is(.session-viewer,.task-session-selected-context) .session-runtime-value'),...document.querySelectorAll('.session-summary,.session-detail-title,.session-task-card')]
   window.chromeLost=false
   window.chromeObserver=new MutationObserver(()=>{if(window.chromeNodes.some(node=>!node.isConnected)||!window.chromeNodes[2].open||!window.chromeNodes[3].open)window.chromeLost=true})
   window.chromeObserver.observe(document,{childList:true,subtree:true})
  })
  const snapshot=async()=>({titles:await page.locator('.session-detail-title').allTextContents(),summaries:await page.locator('.session-summary').allTextContents(),links:await page.locator('.session-task-card').allTextContents(),runtime:await page.locator(':is(.session-viewer,.task-session-selected-context) .session-runtime-value').innerText(),owner:dock?await page.locator('.task-session-dock').boundingBox():{y:0},card:dock?await page.locator('#entity-'+h.fixture.tasks[0].id).boundingBox():null,ancestor:dock?await page.locator('#entity-'+h.fixture.projects[0].id).boundingBox():null,scroll:await page.locator('#main-content-scroll').evaluate(node=>node.scrollTop),viewer:await page.locator('#session-records').boundingBox(),prose:await page.locator('.session-record-text').first().boundingBox()})
  const before=await snapshot()
  await page.waitForTimeout(350)
  assert.deepEqual(await snapshot(),before,'No-refresh control must settle before measuring canonical refresh')
  for(const pendingKind of (dock?['metadata','list']:['metadata','list','links','facets'])){
   heldKind=pendingKind
   const response=page.waitForResponse(response=>new URL(response.url()).pathname==='/api/v1/change-stream/resync'&&response.request().method()==='POST')
   await page.evaluate(()=>window.pushHierarchyFrames([{schema_version:1,type:'resync_required'}]));await response;await until(()=>releases.length>0)
   await page.evaluate(()=>new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve))))
   const pending=await snapshot()
   assert.deepEqual([pending.titles,pending.summaries,pending.links,pending.runtime],[before.titles,before.summaries,before.links,before.runtime],pendingKind+' must retain admitted metadata display')
   assert.ok(Math.abs((pending.viewer.y-pending.owner.y)-(before.viewer.y-before.owner.y))<=1,pendingKind+' moved the viewer within its owner')
   assert.ok(Math.abs((pending.prose.y-pending.owner.y)-(before.prose.y-before.owner.y))<=1,pendingKind+' moved admitted prose within its owner')
   if(dock)assert.equal(pending.owner.height,before.owner.height,pendingKind+' changed dock height')
   for(const key of (dock?['owner','card','ancestor','viewer','prose']:['viewer','prose']))assert.ok(Math.abs(pending[key].y-before[key].y)<=1,pendingKind+' moved absolute '+key)
   assert.equal(pending.scroll,before.scroll,pendingKind+' moved the outer reading position')
   assert.equal(await page.getByRole('status',{name:/^(Loading sessions|Loading session details|Loading linked tasks|Loading active sessions)$/}).count(),0)
   if(dock&&pendingKind==='list'){
    const picker=page.locator('.task-session-picker')
    assert.equal(await picker.getAttribute('aria-busy'),'true')
    assert.equal(await picker.locator('.task-session-choice button').first().isDisabled(),true)
   }
   assert.equal(await page.evaluate(()=>window.chromeLost),false)
   heldKind=null;for(const release of releases.splice(0))release();await h.idle()
  }
  assert.equal(await page.evaluate(()=>window.chromeLost),false)
  assert.ok(h.peak()<=2);assert.deepEqual(h.errors,[])
 }finally{heldKind=null;for(const release of releases.splice(0))release();await h.close()}
})

test('retained dock ancestor descriptions cannot start editing and fresh clears and errors replace them',{timeout:60000},async()=>{
 const h=await openSessionExplorer(),{page}=h
 let holding=false
 const releases=[]
 try{
  await page.route('**/api/v1/**',async route=>{const path=new URL(route.request().url()).pathname;if(holding&&/^\/api\/v1\/(projects|tasks)\/[^/]+$/.test(path))await new Promise(resolve=>releases.push(resolve));await route.fallback()})
  await enter(h,true);await h.idle()
  const ancestor=page.locator('#entity-'+h.fixture.projects[0].id)
  holding=true
  const response=page.waitForResponse(response=>new URL(response.url()).pathname==='/api/v1/change-stream/resync'&&response.request().method()==='POST')
  await page.evaluate(()=>window.pushHierarchyFrames([{schema_version:1,type:'resync_required'}]));await response;await until(()=>releases.length>0)
  const retained=ancestor.locator('.editable-textarea.readonly')
  await retained.waitFor();assert.match(await retained.innerText(),/Markdown link/)
  await retained.click();assert.equal(await page.locator('.inline-edit-textarea').count(),0,'Retained text must not carry StartEdit')
  h.fixture.projects[0].description='Fresh replacement description'
  holding=false;for(const release of releases.splice(0))release();await h.idle()
  // The held fixture response may already have been serialized. A new guarded
  // refresh must replace it, including an explicit null description.
  await resync(h);assert.match(await ancestor.innerText(),/Fresh replacement description/)
  h.fixture.projects[0].description=null
  await resync(h);assert.doesNotMatch(await ancestor.innerText(),/Fresh replacement description|Markdown link/)
  const failure=h.gate(request=>request.path==='/api/v1/projects/'+h.fixture.projects[0].id,{error:404})
  await resync(h);await failure.arrived
  assert.match(await ancestor.innerText(),/Description unavailable/)
  assert.ok(h.caps().maxDetails<=6);assert.deepEqual(h.errors,[])
 }finally{holding=false;for(const release of releases.splice(0))release();await h.close()}
})

test('reader confirmed-empty ancestor preserves its original pre-origin scroll through repeated refreshes',{timeout:60000},async()=>{
 const h=await openSessionExplorer(),{page}=h
 let holding=false
 const releases=[]
 try{
  h.fixture.projects[0].description=null
  await page.route('**/api/v1/**',async route=>{
   const path=new URL(route.request().url()).pathname
   if(path==='/api/v1/session')return route.fulfill({json:{auth_mode:'local',principal:{actor_type:'user',actor_id:'fixture-user',actor_label:'Fixture User',authority:'local',grant_user_id:null},global_permissions:{create_workspace:false,superadmin:false},workspace:{workspace_id:h.fixture.workspace.id,role:'reader',can_read:true,can_edit:false,can_admin:false}}})
   if(holding&&path.includes('/sessions'))await new Promise(resolve=>releases.push(resolve))
   await route.fallback()
  })
  await enter(h,true);await h.idle();await page.evaluate(async()=>{await document.fonts.ready;for(let index=0;index<8;index++)await new Promise(resolve=>requestAnimationFrame(resolve))})
  const description=page.locator('#entity-'+h.fixture.projects[0].id+' .editable-textarea.readonly')
  assert.equal(await description.innerText(),'')
  await page.waitForTimeout(350)
  const before=await description.boundingBox()
  const outerScroll=await page.locator('#main-content-scroll').evaluate(node=>node.scrollTop)
  await page.waitForTimeout(350)
  assert.deepEqual(await description.boundingBox(),before)
  for(let iteration=0;iteration<2;iteration++){
   holding=true
   const response=page.waitForResponse(response=>new URL(response.url()).pathname==='/api/v1/change-stream/resync'&&response.request().method()==='POST')
   await page.evaluate(()=>window.pushHierarchyFrames([{schema_version:1,type:'resync_required'}]));await response;await until(()=>releases.length>0)
   await page.evaluate(()=>new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve))))
   assert.equal(await description.innerText(),'');assert.deepEqual(await description.boundingBox(),before)
   assert.equal(await page.locator('#main-content-scroll').evaluate(node=>node.scrollTop),outerScroll)
   holding=false;for(const release of releases.splice(0))release();await h.idle()
   await page.evaluate(async()=>{for(let index=0;index<8;index++)await new Promise(resolve=>requestAnimationFrame(resolve))})
   assert.deepEqual(await description.boundingBox(),before)
   assert.equal(await page.locator('#main-content-scroll').evaluate(node=>node.scrollTop),outerScroll)
  }
  await description.click({force:true});assert.equal(await page.locator('.inline-edit-textarea').count(),0)
 }finally{holding=false;for(const release of releases.splice(0))release();await h.close()}
})

test('successful canonical empty membership and runtime clearing replace retained metadata',{timeout:60000},async()=>{
 const h=await openSessionExplorer({runtime:{model:'Previous runtime'}}),{page}=h
 try{
  await enter(h);await h.idle()
  h.active.runtime_identity=null;h.active.runtime_revision++
  await resync(h);await until(async()=>await page.locator(':is(.session-viewer,.task-session-selected-context) .session-runtime-value').innerText()==='unknown:unknown:unknown')
  h.sessionLinks.get(h.active.id).clear();await resync(h)
  const linked=page.locator('.session-linked-tasks');if(await linked.getAttribute('open')===null)await linked.locator(':scope > summary').click()
  assert.equal(await linked.locator('.popover-card').count(),0);await linked.getByText('No linked tasks.',{exact:true}).waitFor()
  h.active.state='terminated';h.active.terminated_at=new Date().toISOString();h.active.final_bytes=h.active.committed_bytes;h.active.final_sha256=createHash('sha256').update(h.content.get(h.active.id)).digest('hex')
  await resync(h);assert.equal(await page.locator('.session-summary').count(),0);assert.equal(await page.getByText('Saved session',{exact:true}).textContent(),'Saved session');assert.ok((await page.locator(':is(.session-viewer,.task-session-selected-context) .session-runtime').getAttribute('title')).includes('Session agent'))
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('current metadata errors clear stale chrome and workspace refusal retires admitted private display',{timeout:60000},async()=>{
 const h=await openSessionExplorer({runtime:{model:'Private runtime'}}),{page}=h
 let refusal=0
 try{
  await page.route('**/api/v1/**',async route=>{
   const path=new URL(route.request().url()).pathname
   if(refusal&&path.endsWith('/sessions/'+h.active.id))return route.fulfill({status:refusal,json:{error:'current metadata refused'}})
   if(refusal===403&&path==='/api/v1/session')return route.fulfill({json:{auth_mode:'local',principal:{actor_type:'user',actor_id:'fixture-user',actor_label:'Fixture User',authority:'local',grant_user_id:null},global_permissions:{create_workspace:false,superadmin:false},workspace:{workspace_id:h.fixture.workspace.id,role:null,can_read:false,can_edit:false,can_admin:false}}})
   await route.fallback()
  })
  await enter(h);await h.idle();await page.locator('#session-records').evaluate(node=>window.privateRoot=node)
  refusal=404;await resync(h)
  await page.getByRole('alert').filter({hasText:'Session could not be loaded.'}).waitFor()
  assert.equal(await page.locator('.session-detail-title,:is(.session-viewer,.task-session-selected-context) .session-runtime-value').count(),0)
  refusal=403;await page.getByRole('alert').filter({hasText:'Session could not be loaded.'}).getByRole('button',{name:'Retry',exact:true}).click()
  await until(async()=>await page.locator('#session-records').count()===0)
  assert.equal(await page.evaluate(()=>window.privateRoot.isConnected),false)
  assert.equal(await page.locator('.session-summary,.session-task-card').count(),0)
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

for(const dock of [false,true])test(`live ${dock?'task dock':'Sessions'} retains admitted prose and outer/nested disclosure nodes through noise, head eviction and canonical resync`,{timeout:60000},async()=>{
 const h=await openSessionExplorer({extraRecords:[message('Admitted before the noisy tool run.'),call('first')]}),{page}=h
 try{
  const resume=async()=>{await h.idle();const latest=page.getByRole('button',{name:'Jump to latest',exact:true});if(await latest.isVisible()){await latest.click();await until(()=>page.locator('#session-records').evaluate(node=>node.dataset.latestPending==='false'&&Number(node.dataset.historyAfter)>=Number(node.dataset.historyLatest)&&node.scrollHeight-node.scrollTop-node.clientHeight<=4))}}
  await enter(h,dock);await page.getByText('Admitted before the noisy tool run.',{exact:true}).waitFor()
  const group=page.locator('.session-activity-group').last();await group.locator(':scope > summary').click();await group.locator('.session-content-tool summary').click()
  const anchor=await group.getAttribute('data-group-key');await group.evaluate(el=>window.keptLiveGroup=el)
  await resume()
  await h.appendRecords(...Array.from({length:240},(_,i)=>({type:'turn_context',payload:{index:i}})),...Array.from({length:80},(_,i)=>call(i)))
  await until(async()=>/81 events$/.test(await group.locator(':scope > summary').innerText()))
  assert.equal(await group.evaluate(el=>el===window.keptLiveGroup&&el.open),true)
  assert.equal(await group.locator('.session-content-tool').first().getAttribute('open'),'')
  const tool=group.locator('.session-content-tool').filter({hasText:/Call · inspect · "index": 60\b/});assert.equal(await tool.count(),1);await tool.locator('summary').click();await tool.evaluate(el=>window.keptLiveTool=el)
  const viewport=page.locator('#session-records'),box=await viewport.boundingBox();await viewport.hover({position:{x:box.width-5,y:box.height/2}});await page.mouse.wheel(0,-120);await page.getByRole('button',{name:'Jump to latest',exact:true}).waitFor({state:'visible'});await page.mouse.wheel(0,await tool.evaluate(node=>node.getBoundingClientRect().top-document.getElementById('session-records').getBoundingClientRect().top-80));await h.idle()
  await viewport.evaluate(async node=>{let top=node.scrollTop,stable=0;for(let frame=0;frame<90&&stable<4;frame++){await new Promise(resolve=>requestAnimationFrame(resolve));const next=node.scrollTop;stable=Math.abs(next-top)<.25?stable+1:0;top=next}if(stable<4)throw Error('Visible disclosure input did not settle')});await tool.evaluate(node=>window.keptLiveTool=node)
  assert.equal(await tool.evaluate(node=>{const r=node.getBoundingClientRect(),box=document.getElementById('session-records').getBoundingClientRect();return r.bottom>box.top&&r.top<box.bottom}),true,'the protected nested disclosure is actually visible')
  const beforeTool=await viewport.evaluate(node=>{const tool=window.keptLiveTool,r=tool.getBoundingClientRect(),box=node.getBoundingClientRect();return {attrs:{...node.dataset},host:{...node.closest('.session-viewer').dataset},top:node.scrollTop,toolTop:r.top-box.top,toolBottom:r.bottom-box.top,key:tool.dataset.eventKey,fragment:tool.closest('[data-fragment-path]')?.dataset.fragmentPath}})
  await page.evaluate(()=>{window.livePresentationLost=false;new MutationObserver(()=>{if(!window.keptLiveGroup.isConnected||!window.keptLiveTool.isConnected)window.livePresentationLost=true}).observe(document.getElementById('session-records'),{childList:true,subtree:true})})
  await h.appendRecords(...Array.from({length:80},(_,i)=>call(80+i)));await until(()=>viewport.evaluate((node,end)=>Number(node.dataset.historyLatest)>=end,h.active.committed_bytes))
  await h.appendRecords(...Array.from({length:80},(_,i)=>call(160+i)))
  await until(()=>viewport.evaluate((node,end)=>Number(node.dataset.historyLatest)>=end,h.active.committed_bytes));await h.idle()
  assert.equal(await group.getAttribute('data-group-key'),anchor,'trimming the run head retains its logical anchor')
  assert.equal(await group.evaluate(el=>el===window.keptLiveGroup&&el.open),true)
  assert.equal(await page.evaluate(()=>window.keptLiveTool.isConnected&&window.keptLiveTool.open),true,JSON.stringify({phase:'visible-tool-growth',dock,before:beforeTool,after:await viewport.evaluate(node=>({attrs:{...node.dataset},host:{...node.closest('.session-viewer').dataset},top:node.scrollTop,saved:{connected:window.keptLiveTool.isConnected,open:window.keptLiveTool.open},tools:[...node.querySelectorAll('.session-content-tool')].filter(tool=>/"index": 60\b/.test(tool.textContent)).map(tool=>({key:tool.dataset.eventKey,open:tool.open,top:tool.getBoundingClientRect().top-node.getBoundingClientRect().top,fragment:tool.closest('[data-fragment-path]')?.dataset.fragmentPath}))})),reads:h.reads.filter(read=>read.path.endsWith('/transcript')).slice(-3)}))
  await resync(h)
  assert.equal(await page.evaluate(()=>window.keptLiveGroup.isConnected&&window.keptLiveGroup.open&&window.keptLiveTool.isConnected&&window.keptLiveTool.open&&!window.livePresentationLost),true)
  await viewport.press('End');await until(()=>viewport.evaluate(node=>node.dataset.latestPending==='false'&&Number(node.dataset.historyAfter)>=Number(node.dataset.historyLatest)&&node.scrollHeight-node.scrollTop-node.clientHeight<=4));assert.ok(await group.locator('article').count()<=200,'offscreen presentation remains bounded after returning to the grown tail')
  assert.equal(await page.getByText('Admitted before the noisy tool run.',{exact:true}).count(),0,'offscreen prose may retire while following the grown tail')
  const source=h.content.get(h.active.id),expected=[message('Admitted before the noisy tool run.'),call('first'),...Array.from({length:240},(_,i)=>call(i))].map(record=>{const match=source.indexOf(Buffer.from(JSON.stringify(record.payload)));assert.ok(match>=0);return source.lastIndexOf(10,match)+1}),visited=new Set()
  const collect=async()=>{for(const offset of await page.locator('#session-records .session-record').evaluateAll(nodes=>nodes.map(node=>Number(node.dataset.offset))))visited.add(offset)}
  await collect()
  let acquisitions=0,previous='',stalled=0
  for(let steps=0;await page.locator('#session-records').evaluate(node=>Number(node.dataset.historyScanBefore)>0)||expected.some(offset=>!visited.has(offset));steps++){
   const fingerprint=await viewport.evaluate(node=>[node.dataset.historyBefore,node.dataset.historyAfter,node.dataset.renderPart,Math.round(node.scrollTop)].join(':'));stalled=fingerprint===previous?stalled+1:0;assert.ok(stalled<6,JSON.stringify({phase:'live-complete-history',dock,seen:visited.size,missing:expected.filter(offset=>!visited.has(offset)).slice(0,5),attrs:await viewport.evaluate(node=>({...node.dataset,top:node.scrollTop,gaps:[...node.querySelectorAll('[data-history-gap]')].map(gap=>({...gap.dataset}))}))}));previous=fingerprint
   assert.ok(steps<200);const before=Number(await page.locator('#session-records').getAttribute('data-history-before'))
   await continuousScrollStep(page,-1);await h.idle();await collect()
   if(Number(await page.locator('#session-records').getAttribute('data-history-before'))<before)acquisitions++
   assert.ok(acquisitions<10,'automatic source backfill retains the original acquisition bound')
  }
  assert.deepEqual(expected.filter(offset=>visited.has(offset)),expected,'retired prose and every activity occurrence remain reachable across contiguous pages')
  assert.equal(await page.getByText('Admitted before the noisy tool run.',{exact:true}).count(),1)
  if(process.env.SESSION_LIVE_SCREENSHOTS){await page.locator('#session-records').evaluate(el=>{const prose=[...el.querySelectorAll('.session-record:not(.session-record-activity)')].find(node=>node.textContent.includes('Admitted before the noisy tool run.'));el.scrollTop+=prose.getBoundingClientRect().top-el.getBoundingClientRect().top-100});await page.screenshot({path:join(process.env.SESSION_LIVE_SCREENSHOTS,dock?'live-session-stable-dock.png':'live-session-stable-desktop.png'),fullPage:true})}
  assert.ok(h.peak()<=2);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('finite hydration publishes usable bounded history and append/resync preserve a scrolled-up reader',{timeout:60000},async()=>{
 const records=Array.from({length:20},(_,i)=>message('Visible message '+i+'\n\n'+'Readable paragraph. '.repeat(350)))
 const h=await openSessionExplorer({extraRecords:records}),{page}=h
 try{
  await page.addInitScript(()=>{window.viewerWrites=[];window.firstPublishedMessages=null;const descriptor=Object.getOwnPropertyDescriptor(Element.prototype,'scrollTop');Object.defineProperty(Element.prototype,'scrollTop',{...descriptor,set(value){if(this.id==='session-records')window.viewerWrites.push(value);descriptor.set.call(this,value)}});new MutationObserver(()=>{const root=document.getElementById('session-records'),count=root?.querySelectorAll('.session-record:not(.session-record-activity)').length||0;if(count&&window.firstPublishedMessages===null)window.firstPublishedMessages=count}).observe(document,{childList:true,subtree:true})})
  await enter(h);await page.getByText('Visible message 19',{exact:false}).waitFor();await until(async()=>await page.evaluate(()=>window.viewerWrites.length>0))
  assert.ok(await page.evaluate(()=>window.firstPublishedMessages>0&&window.firstPublishedMessages<23),'a usable finite batch appears before the whole conversation is mounted')
  assert.ok(h.reads.filter(read=>read.path.endsWith('/transcript')).every(read=>Number(new URLSearchParams(read.query).get('limit'))<=65536),'progressive publication retains bounded byte requests')
  await page.locator('#session-records').hover();await page.mouse.wheel(0,-500);await h.idle()
  const before=await page.locator('#session-records').evaluate(el=>{const bounds=el.getBoundingClientRect(),row=[...el.querySelectorAll('[data-block-key]')].find(row=>row.getBoundingClientRect().bottom>bounds.top&&row.getBoundingClientRect().height>0);window.readerKey=row.getAttribute('data-block-key');return {top:el.scrollTop,relative:row.getBoundingClientRect().top-bounds.top}})
  await h.appendRecords(message('A new message while reading above the tail.'));await h.idle();await resync(h)
  await page.getByRole('button',{name:'Jump to latest',exact:true}).waitFor({state:'visible'})
  const after=await page.locator('#session-records').evaluate(el=>{const row=[...el.querySelectorAll('[data-block-key]')].find(row=>row.getAttribute('data-block-key')===window.readerKey);return {top:el.scrollTop,relative:row.getBoundingClientRect().top-el.getBoundingClientRect().top}})
  assert.ok(Math.abs(after.top-before.top)<=2);assert.ok(Math.abs(after.relative-before.relative)<=2)
  await page.getByRole('button',{name:'Jump to latest',exact:true}).click();await page.getByText('A new message while reading above the tail.',{exact:true}).waitFor()
  const seen=new Set(),collect=async()=>{for(const text of await page.locator('.session-record-text').allTextContents())for(const match of text.matchAll(/Visible message (\d+)/g))seen.add(Number(match[1]))}
  await collect()
  let acquisitions=0
  for(let steps=0;await page.locator('#session-records').evaluate(node=>Number(node.dataset.historyScanBefore)>0)||seen.size<20;steps++){
   assert.ok(steps<200);const before=Number(await page.locator('#session-records').getAttribute('data-history-before'))
   await continuousScrollStep(page,-1);await h.idle();await collect()
   if(Number(await page.locator('#session-records').getAttribute('data-history-before'))<before)acquisitions++
   assert.ok(acquisitions<10,'automatic source backfill retains the original acquisition bound')
  }
  assert.deepEqual([...seen].sort((a,b)=>a-b),Array.from({length:20},(_,i)=>i))
  assert.ok(h.peak()<=2);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('an at-tail reader continues following when bounded prose retention evicts rows above the viewport',{timeout:60000},async()=>{
 const h=await openSessionExplorer({extraRecords:Array.from({length:100},(_,i)=>message('Earlier '+i+'\n\n'+'Paragraph. '.repeat(35)))}),{page}=h
 try{
  await enter(h);await page.getByText('Earlier 99',{exact:false}).waitFor();await until(async()=>await page.locator('#session-records').evaluate(el=>el.scrollHeight-el.clientHeight-el.scrollTop<=2))
  await h.appendRecords(...Array.from({length:5},(_,i)=>message('Later '+i+'\n\n'+'Paragraph. '.repeat(35))))
  await page.getByText('Later 4',{exact:false}).waitFor();await h.idle()
  await until(async()=>await page.locator('#session-records').evaluate(el=>el.scrollHeight-el.clientHeight-el.scrollTop<=2))
  assert.ok(await page.locator('.session-record:not(.session-record-activity)').count()<=200,'the composed mounted conversation stays bounded');assert.ok(await page.locator('#session-records').evaluate(node=>Number(node.dataset.renderPart)>0||Number(node.dataset.historyBefore)>0),'latest rendering omits older offscreen material')
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('an expanded activity run follows at the tail while its retained head is evicted',{timeout:60000},async()=>{
 const h=await openSessionExplorer({extraRecords:[message('Agent prose before the long activity run.'),...Array.from({length:100},(_,i)=>call(i))]}),{page}=h
 try{
  await enter(h);const group=page.locator('.session-activity-group').last();await group.locator(':scope > summary').click();await h.idle()
  await page.locator('#session-records').evaluate(el=>{el.scrollTop=el.scrollHeight})
  await until(async()=>await page.locator('#session-records').evaluate(el=>el.scrollHeight-el.clientHeight-el.scrollTop<=2))
  await h.appendRecords(...Array.from({length:5},(_,i)=>call(100+i)))
  await until(async()=>/104/.test(await group.locator('.session-content-tool summary').last().innerText()));await h.idle()
  await until(async()=>await page.locator('#session-records').evaluate(el=>el.scrollHeight-el.clientHeight-el.scrollTop<=2)).catch(async error=>{throw Error(error.message+' '+JSON.stringify(await page.locator('#session-records').evaluate(el=>({top:el.scrollTop,height:el.scrollHeight,viewport:el.clientHeight}))))})
  assert.ok(await group.locator('article').count()<=200,'composed activity rendering stays bounded');assert.match(await group.locator(':scope > summary').innerText(),/105 events/);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('successive proven native Codex byte prefixes retain admitted prose and disclosures during append and synchronization',{timeout:60000,skip:!process.env.SESSION_LIVE_NATIVE_PATH},async()=>{
 const handle=await open(process.env.SESSION_LIVE_NATIVE_PATH,'r');let bytes
 try{const limit=Number(process.env.SESSION_LIVE_NATIVE_MAX_BYTES||4194304);assert.ok(Number.isSafeInteger(limit)&&limit>2097152&&limit<=4194304);const boundary=Math.min((await handle.stat()).size,limit),buffer=Buffer.alloc(boundary);let offset=0;while(offset<boundary){const result=await handle.read(buffer,offset,boundary-offset,offset);if(!result.bytesRead)break;offset+=result.bytesRead}bytes=buffer.subarray(0,buffer.lastIndexOf(10,offset-1)+1)}finally{await handle.close()}
 assert.ok(bytes.length>65536)
 const metadata=JSON.parse(bytes.subarray(0,bytes.indexOf(10)).toString('utf8')).payload
 assert.match(process.env.SESSION_LIVE_NATIVE_ID||'',/^[0-9a-f-]{36}$/,'a proven expected native ID is required with the source path')
 assert.equal(metadata.id,process.env.SESSION_LIVE_NATIVE_ID);assert.equal(resolve(metadata.cwd).toLowerCase(),resolve(import.meta.dirname,'../../..').toLowerCase())
 if(process.env.SESSION_LIVE_NATIVE_PARENT)assert.equal(metadata.parent_thread_id,process.env.SESSION_LIVE_NATIVE_PARENT)
 if(process.env.SESSION_LIVE_NATIVE_AGENT_PATH)assert.equal(metadata.source?.subagent?.thread_spawn?.agent_path,process.env.SESSION_LIVE_NATIVE_AGENT_PATH)
 const initialEnd=bytes.lastIndexOf(10,Math.min(bytes.length-1,2097151))+1,framer=createRecordFramer()
 const feed=(begin,end)=>{for(let offset=begin;offset<end;offset+=65536)framer.push(offset,new Uint8Array(bytes.subarray(offset,Math.min(end,offset+65536))),offset+65536>=end)}
 feed(0,initialEnd);const initialRows=framer.snapshot().rows
 assert.ok(bytes.length>initialEnd,'the proven source includes a later actual append')
 feed(initialEnd,bytes.length);const finalRows=framer.snapshot().rows,retained=new Set(finalRows.map(row=>row.presentationOffset))
 const prose=initialRows.filter(row=>retained.has(row.presentationOffset)&&row.role==='assistant'&&row.type==='conversation'&&JSON.parse(row.text).some(block=>block.kind==='text'&&block.text.trim())).at(-1)
 const payload=initialRows.filter(row=>retained.has(row.presentationOffset)&&row.type==='conversation'&&JSON.parse(row.text).some(block=>['tool','result'].includes(block.kind))).at(-1)
 assert.ok(prose&&payload,'the native initial prefix has agent prose and a retained tool occurrence')
 const digest=createHash('sha256').update(bytes).digest('hex'),h=await openSessionExplorer({transcriptBytes:bytes.subarray(0,initialEnd)}),{page}=h
 try{
  await enter(h);await h.idle()
  const proseNode=page.locator('.session-record:not(.session-record-activity)[data-block-key^="'+prose.presentationOffset+':"]').first()
  await proseNode.evaluate(el=>window.nativeLiveProse=el)
  const tool=page.locator('.session-record[data-offset="'+payload.offset+'"] .session-content-tool,.session-record[data-offset="'+payload.offset+'"] .session-content-result').last()
  const group=tool.locator('xpath=ancestor::details[contains(@class,"session-activity-group")]')
  await group.locator(':scope > summary').click();await tool.locator(':scope > summary').click()
  await group.evaluate(el=>window.nativeLiveGroup=el);await tool.evaluate(el=>window.nativeLiveTool=el)
  const finalRead=page.waitForResponse(response=>{const url=new URL(response.url());return url.pathname.endsWith('/transcript')&&Number(url.searchParams.get('offset'))+Number(url.searchParams.get('limit'))>=bytes.length})
  await h.appendBytes(bytes.subarray(initialEnd));await finalRead;await h.idle()
  assert.equal(await page.evaluate(()=>window.nativeLiveProse.isConnected&&window.nativeLiveGroup.isConnected&&window.nativeLiveGroup.open&&window.nativeLiveTool.isConnected&&window.nativeLiveTool.open),true)
  await resync(h)
  assert.equal(await group.evaluate(el=>el===window.nativeLiveGroup&&el.open),true)
  assert.equal(await page.evaluate(()=>window.nativeLiveProse.isConnected&&window.nativeLiveTool.isConnected&&window.nativeLiveTool.open),true)
  const rendered=await page.locator('#session-records [data-block-key]').evaluateAll(nodes=>nodes.map(node=>node.getAttribute('data-block-key')))
  assert.ok(finalRows.filter(row=>row.role==='assistant'&&row.type==='conversation'&&JSON.parse(row.text).some(block=>block.kind==='text'&&block.text.trim())).every(row=>rendered.some(key=>key.startsWith(row.presentationOffset+':'))))
  assert.equal(createHash('sha256').update(h.content.get(h.active.id)).digest('hex'),digest)
  assert.deepEqual(h.errors,[])
  console.log(JSON.stringify({native_session:metadata.id,initial_bytes:initialEnd,finite_bytes:bytes.length,sha256:digest,outcome:'actual-source append preserves admitted prose and outer/inner disclosures through canonical resync'}))
 }finally{await h.close()}
})

for(const following of [true,false])test(`natural byte reserve eviction preserves ${following?'tail following at every append':'a surviving above-tail reading anchor'}`,{timeout:60000},async()=>{
 const record=(bytes,marker)=>Buffer.from('{"text":"'+marker+'x'.repeat(bytes-marker.length-13)+'"}\r\n')
 const prefix=Buffer.concat(Array.from({length:16},(_,index)=>record(65536,`prefix-${index}-`)))
 const added=Array.from({length:100},(_,index)=>record([1024,1024,1024,65536][index%4],`rollover-${index}-`))
 assert.equal(prefix.length,1048576)
 const h=await openSessionExplorer({transcriptBytes:prefix}),{page}=h
 try{
  await enter(h);await h.idle()
  const viewport=page.locator('#session-records')
  const paint=()=>page.evaluate(()=>new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve))))
  const tail=()=>viewport.evaluate(el=>el.scrollHeight-el.scrollTop-el.clientHeight)
  await viewport.scrollIntoViewIfNeeded();await viewport.press('End');await paint()
  assert.ok(await tail()<=2)
  const append=async(lo,hi,visible=true)=>{
   await h.appendBytes(Buffer.concat(added.slice(lo,hi)))
   if(visible)await until(()=>viewport.evaluate((el,marker)=>el.textContent.includes(marker),`rollover-${hi-1}-`))
   else await until(()=>viewport.evaluate((el,end)=>Number(el.dataset.historyLatest)>=end&&el.dataset.historyLoading==='false',h.active.committed_bytes))
   await h.idle();await paint()
   assert.ok(await page.locator('#session-records [data-block-key]').count()<=100,'the prose row reserve stays bounded')
  }
  await append(0,55);assert.equal(h.active.committed_bytes,1943552)
  assert.ok(await tail()<=2,'following reaches the last pre-eviction append')
  let reading
  if(!following){
   for(let step=0;!await viewport.locator('[data-block-key]').filter({hasText:'prefix-8-'}).count();step++){assert.ok(step<60,'the historical prefix remains reachable');await continuousScrollStep(page,-1);await h.idle()}
   await viewport.hover();await page.mouse.wheel(0,await viewport.evaluate(el=>{const row=[...el.querySelectorAll('[data-block-key]')].find(row=>row.textContent.includes('prefix-8-'));return row.getBoundingClientRect().top-el.getBoundingClientRect().top-80}));await h.idle();await paint()
   reading=await viewport.evaluate(el=>{
    const row=[...el.querySelectorAll('[data-block-key]')].find(row=>row.textContent.includes('prefix-8-'))
    if(!row)throw Error('The surviving prefix row must be admitted')
    return {key:row.getAttribute('data-block-key')}
   })
   await paint()
   reading.relative=await viewport.evaluate((el,key)=>[...el.querySelectorAll('[data-block-key]')].find(row=>row.getAttribute('data-block-key')===key).getBoundingClientRect().top-el.getBoundingClientRect().top,reading.key)
   assert.ok(await tail()>48,'the selected surviving row is above the tail')
  }
  await append(55,67,following);assert.equal(h.active.committed_bytes,2149376)
  if(following){
   assert.equal(await viewport.evaluate(el=>el.textContent.includes('prefix-0-')),false,'crossing 2MiB actually evicts the old head')
   assert.ok(await tail()<=2,'natural byte eviction must not veto tail following without new input')
   await append(67,100);assert.equal(h.active.committed_bytes,2763776)
   assert.ok(await tail()<=2,'each settled later append keeps the final committed record visible at the tail')
  }else{
   const after=await viewport.evaluate((el,key)=>{
    const row=[...el.querySelectorAll('[data-block-key]')].find(row=>row.getAttribute('data-block-key')===key)
    return {key:row?.getAttribute('data-block-key'),relative:row?.getBoundingClientRect().top-el.getBoundingClientRect().top}
   },reading.key)
   assert.equal(after.key,reading.key,'the visible row survives head eviction')
   assert.ok(Math.abs(after.relative-reading.relative)<=2,'explicit anchoring retains the visible row through live growth beyond byte windows')
   assert.ok(await tail()>48,'an above-tail reader must not be pulled to the new tail')
   assert.ok(await viewport.evaluate(el=>Number(el.dataset.historyAfter)<Number(el.dataset.historyLatest)),'new committed content remains logically available beyond the historical page')
   await page.getByRole('button',{name:'Jump to latest',exact:true}).click()
   await until(()=>viewport.evaluate(el=>el.dataset.latestPending==='false'&&Number(el.dataset.historyAfter)>=Number(el.dataset.historyLatest)&&el.scrollHeight-el.scrollTop-el.clientHeight<=2))
   assert.equal(await viewport.evaluate(el=>el.textContent.includes('rollover-66-')),true,'explicit latest navigation reaches the deferred append')
   assert.equal(await viewport.evaluate(el=>el.textContent.includes('prefix-0-')),false,'the current bounded tail retires the old head')
  }
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})
