import assert from 'node:assert/strict'
import test from 'node:test'
import { join } from 'node:path'
import {createHash} from 'node:crypto'
import {gzipSync} from 'node:zlib'
import {openSessionExplorer,continuousScrollStep,traceSessionBridge} from './support/sessions-ux-fixture.mjs'
const sleep=ms=>new Promise(r=>setTimeout(r,ms))
const downloadedBytes=async download=>{const chunks=[];for await(const chunk of await download.createReadStream())chunks.push(chunk);return Buffer.concat(chunks)}
const until=async fn=>{const end=Date.now()+15000;while(Date.now()<end){if(await fn())return;await sleep(25)}throw Error('Busy session did not converge')}
const message=text=>({type:'response_item',payload:{type:'message',role:'assistant',content:[{type:'output_text',text}]}})

for(const proof of ['model error','unconfirmed error'])test(`disclosure recovery keeps real Retry with ${proof} and unchanged native open state`,{timeout:45000},async()=>{
 const tool=name=>({type:'response_item',payload:{type:'function_call',name,arguments:'{}'}})
 const h=await openSessionExplorer({transcriptBytes:Buffer.from([tool('first_activity'),message('Separate readable context.'),tool('other_activity')].map(value=>JSON.stringify(value)+'\n').join(''))}),{page}=h
 try{
  await traceSessionBridge(h)
  await page.addInitScript(proof=>{
   window.sessionBridgeDispatch=(send,value)=>{if(value.key===window.failedDisclosureKey&&(value.kind==='viewer-disclosure'||value.kind==='viewer-disclosure-error'&&proof==='unconfirmed error'))return;send(value)}
   window.sessionCommandGate=value=>{if(value.disclosureAck?.error){window.droppedDisclosureErrorAck=value.disclosureAck;return false}return true}
  },proof)
  await h.start();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.active.id)
  await page.locator('.session-activity-group').last().waitFor();await h.idle()
  const key=await page.locator('.session-activity-group').first().getAttribute('data-group-key'),group=page.locator('[data-group-key="'+key+'"]')
  await page.evaluate(key=>window.failedDisclosureKey=key,key);await group.locator(':scope > summary').click()
  const alert=proof==='model error'?page.locator('.session-viewer [role="alert"]').filter({hasText:'Activity could not be updated.'}):page.locator('[data-disclosure-error-owner]')
  await alert.waitFor({state:'visible',timeout:12000});assert.equal(await group.evaluate(node=>node.open),true);assert.equal(await group.locator('article').count(),0)
  assert.equal(await page.locator('#session-records').getAttribute('data-history-error'),proof==='model error'?'true':'false')
  if(proof==='model error')assert.ok(await page.evaluate(()=>window.droppedDisclosureErrorAck?.error))
  await page.locator('.session-activity-group').last().locator(':scope > summary').click();await page.locator('.session-activity-group').last().locator('article').waitFor();assert.equal(await alert.isVisible(),true,'unrelated activity success cannot erase the failed group recovery')
  const retry=alert.getByRole('button',{name:'Retry',exact:true});await retry.focus();assert.equal(await retry.evaluate(node=>node===document.activeElement),true)
  await page.evaluate(()=>{window.failedDisclosureKey=null});await retry.press('Enter')
  await group.locator('article').waitFor();await until(async()=>await page.locator('.session-viewer').getByRole('alert').count()===0)
  assert.equal(await group.evaluate(node=>node.open),true,'ordinary Retry reissues the unchanged native desired state')
  assert.ok(await page.evaluate(()=>window.sessionBridgeFacts.some(value=>value.direction==='command'&&value.disclosureRetry===true)))
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

for(const dock of [false,true])test(`initial ${dock?'task dock':'Sessions list and links'} loads a latest long transcript while progress invalidates faster than responses`,{timeout:45000},async t=>{
 const records=[...Array.from({length:2200},(_,i)=>({type:'mystery',padding:'p'.repeat(1100),i})),message('Visible latest agent response'),{type:'response_item',payload:{type:'function_call',name:'busy_tool',arguments:'{"initial":true}'}}]
 const h=await openSessionExplorer({runtime:{model:'Running target',role:'Developer'},transcriptBytes:Buffer.from(records.map(r=>JSON.stringify(r)).join('\n')+'\n')}),{page}=h
 let running=true,producer
 try{
  await traceSessionBridge(h)
  await page.addInitScript(()=>{
   let held=null,timer=null
   const release=generation=>{if(!held)return;const current=held;held=null;clearTimeout(timer);window.busyHeldDisclosure.releasedGeneration=generation;current.send(current.value)}
   window.sessionBridgeDispatch=(send,value)=>{
    if(!window.busyHeldDisclosure&&value.kind==='viewer-disclosure'&&value.key===window.busyInitialClicked?.key&&value.open){
     window.busyHeldDisclosure={request:value.request,key:value.key,generation:value.generation};held={send,value};timer=setTimeout(()=>release(null),1500);return
    }
    send(value)
   }
   window.sessionCommandGate=value=>{
    if(held&&!value.disclosureAck&&value.generation>held.value.generation)queueMicrotask(()=>release(value.generation))
    if(value.disclosureAck&&value.disclosureAck.request===window.busyHeldDisclosure?.request&&!value.disclosureAck.error&&!value.disclosureAck.cancelled)window.busyAcceptedDisclosure={generation:value.generation,...value.disclosureAck}
    return true
   }
  })
  await page.route('**/api/v1/**/sessions**',async route=>{if(!new URL(route.request().url()).pathname.endsWith('/transcript'))await sleep(140);await route.fallback()})
  await h.start();let index=0
  producer=(async()=>{while(running){await h.appendRecords({type:'response_item',payload:{type:'function_call',name:'busy_tool',arguments:JSON.stringify({index:index++})}});await sleep(35)}})()
  if(dock){const icon=page.locator('#entity-'+h.fixture.tasks[0].id+' .task-sessions-icon');await until(()=>icon.isEnabled());await icon.click()}
  else{await page.getByRole('button',{name:'Sessions',exact:true}).click();await page.locator('.session-summary a').first().click();await page.locator('.session-linked-tasks > summary').click();await until(async()=>await page.locator('.session-task-cards .popover-card').count()===2)}
  await page.getByText('Visible latest agent response',{exact:true}).waitFor();assert.ok(running,'load completed during active progress, without reselection')
  const prose=page.getByText('Visible latest agent response',{exact:true});let group=page.locator('.session-activity-group').last();const clicked=await group.evaluate(node=>({key:node.dataset.groupKey,open:node.open,attrs:{...document.getElementById('session-records').dataset},manual:document.querySelector('.session-viewer').dataset.manualUp,top:document.getElementById('session-records').scrollTop,height:document.getElementById('session-records').scrollHeight,client:document.getElementById('session-records').clientHeight}));await page.evaluate(value=>window.busyInitialClicked=value,clicked);group=page.locator('[data-group-key="'+clicked.key+'"]');await group.locator(':scope > summary').click();await group.evaluate(node=>window.busyGroup=node);await prose.evaluate(node=>window.busyProse=node)
  const busyState=()=>page.locator('#session-records').evaluate(node=>({attrs:{...node.dataset},top:node.scrollTop,height:node.clientHeight,parts:[...new Set([...node.querySelectorAll("[data-fragment-part]")].map(leaf=>Number(leaf.dataset.fragmentPart)))],centerLeaves:[...node.querySelectorAll("[data-fragment-part]")].filter(leaf=>leaf.dataset.fragmentPart===node.dataset.renderPart).slice(0,4).map(leaf=>({path:leaf.dataset.fragmentPath,record:leaf.closest(".session-record")?.dataset.blockKey,summary:!!leaf.closest("summary"),nested:!!leaf.querySelector("[data-fragment-path]"),gap:!!leaf.querySelector("[data-fragment-from]"),top:leaf.getBoundingClientRect().top-node.getBoundingClientRect().top,bottom:leaf.getBoundingClientRect().bottom-node.getBoundingClientRect().top})),groups:[...node.querySelectorAll('.session-activity-group')].map(group=>({key:group.dataset.groupKey,open:group.open,count:group.querySelectorAll('article').length,top:group.getBoundingClientRect().top-node.getBoundingClientRect().top,height:group.getBoundingClientRect().height})),saved:{group:window.busyGroup?.isConnected,open:window.busyGroup?.open,prose:window.busyProse?.isConnected}}))
  await until(async()=>await group.locator('article').count()>0).catch(async error=>{console.log(JSON.stringify({phase:'busy-group-publication',dock,clicked,events:await page.evaluate(()=>({disclosures:window.busyDisclosureFacts,commands:window.sessionBridgeFacts.slice(-8)})),state:await busyState(),reads:h.reads.filter(row=>row.path.endsWith('/transcript')).slice(-3)}));throw error})
  const delivery=await page.evaluate(()=>({held:window.busyHeldDisclosure,accepted:window.busyAcceptedDisclosure}));assert.ok(delivery.held.releasedGeneration>delivery.held.generation);assert.equal(delivery.accepted.request,delivery.held.request);assert.ok(delivery.accepted.generation>delivery.held.generation,'the same current owned action is really applied after its original generation became stale')
  await until(()=>page.locator('#session-records').evaluate(node=>node.dataset.historyLoading==='false'))
  await group.evaluate(node=>window.busyGroup=node);const initialOpen=await page.evaluate(()=>({clicked:window.busyInitialClicked,disclosures:window.busyDisclosureFacts,delivery:{held:window.busyHeldDisclosure,accepted:window.busyAcceptedDisclosure}}));const latest=await group.evaluate(node=>{window.busyGroup=node;return node.querySelector('article:last-child')?.dataset.offset});assert.notEqual(latest,undefined);await until(async()=>await group.locator('article').last().getAttribute('data-offset')!==latest)
  assert.equal(await page.evaluate(()=>window.busyGroup.isConnected&&window.busyGroup.open&&window.busyProse.isConnected),true,JSON.stringify({phase:'busy-connected-witness',dock,state:await busyState()}))
  const reads=h.reads.filter(row=>row.path.endsWith('/transcript'))
  assert.ok(reads.length>0&&reads.every(row=>Number(new URLSearchParams(row.query).get('limit'))<=65536),'progressive latest acquisition keeps each request bounded')
  running=false;await producer
  await until(()=>page.locator('#session-records').evaluate((node,end)=>Number(node.dataset.historyLatest)>=end,h.active.committed_bytes))
  const jump=page.getByRole('button',{name:'Jump to latest',exact:true});if(await jump.isVisible())await jump.click()
  await until(()=>page.locator('#session-records').evaluate(node=>node.dataset.historyLoading==='false'&&Number(node.dataset.historyAfter)>=Number(node.dataset.historyLatest)))
  const expected=[];let offset=0
  for(const line of h.content.get(h.active.id).toString().trimEnd().split('\n')){expected.push(offset);offset+=Buffer.byteLength(line+'\n')}
  await page.evaluate(expected=>{window.busySeen=new Set();window.busyPhases=[];window.busyInvalid=false;window.busyPeak=0;const indices=new Map(expected.map((offset,index)=>[offset,index]));window.busySourceIndex=indices;const collect=()=>{const node=document.getElementById('session-records'),offsets=[...node.querySelectorAll('.session-record')].map(row=>Number(row.dataset.offset));window.busyPeak=Math.max(window.busyPeak,offsets.length);if(offsets.some((offset,index)=>index>0&&offset<=offsets[index-1]))window.busyInvalid=true;for(const offset of offsets)window.busySeen.add(offset);const source=offsets.map(offset=>indices.get(offset));if(source.some(index=>index>=1850&&index<2200)){const runs=[];for(const index of source){const last=runs.at(-1);if(last&&last[1]+1===index)last[1]=index;else runs.push([index,index])}const phase={before:node.dataset.historyBefore,scan:node.dataset.historyScanBefore,after:node.dataset.historyAfter,part:node.dataset.renderPart,top:Math.round(node.scrollTop),runs};if(JSON.stringify(window.busyPhases.at(-1))!==JSON.stringify(phase))window.busyPhases.push(phase);if(window.busyPhases.length>32)window.busyPhases.shift()}};window.collectBusySeen=collect;new MutationObserver(collect).observe(document.getElementById('session-records'),{childList:true,subtree:true});collect()},expected)
  const coverageReads=h.reads.filter(read=>read.path.endsWith('/transcript')).length;let acquisitions=0,inputSteps=0,expandedRuns=0
  const visited=new Set(),forwardRecovered=new Set(),capture=async()=>{for(const offset of await page.evaluate(()=>{window.collectBusySeen();return [...window.busySeen]}))visited.add(offset)}
  const alreadyOpenRecovered=new Set();let alreadyOpenSweeps=0
  const move=async direction=>{t.signal.throwIfAborted();assert.ok(++inputSteps<600,'continuous busy history cannot strand an occurrence');const before=await page.locator('#session-records').evaluate(node=>[node.dataset.historyBefore,node.dataset.historyAfter].join(':'));const motion=await continuousScrollStep(page,direction);await h.idle();const after=await page.locator('#session-records').evaluate(node=>[node.dataset.historyBefore,node.dataset.historyAfter].join(':'));if(before!==after)acquisitions++;assert.ok(acquisitions<60,'source acquisition retains the original finite busy-history bound');await capture();return motion}
  const collect=async()=>{
   const keys=await page.locator('.session-activity-group:not([open]) > summary').evaluateAll(nodes=>nodes.reverse().filter(node=>{const r=node.getBoundingClientRect(),box=document.getElementById('session-records').getBoundingClientRect();return r.height>0&&r.bottom>box.top&&r.top<box.bottom}).map(node=>node.parentElement.dataset.groupKey));let expanded=false
   for(const key of keys){const summary=page.locator('[data-group-key="'+key+'"] > summary');if(await summary.evaluateAll(nodes=>nodes.some(node=>{const r=node.getBoundingClientRect(),box=document.getElementById('session-records').getBoundingClientRect();return !node.parentElement.open&&r.bottom>box.top&&r.top<box.bottom}))){await summary.click();await h.idle();expanded=true;expandedRuns++}}
   await capture()
   const interval=await page.locator('#session-records').evaluate(node=>({range:[Math.min(...[...node.querySelectorAll('.session-record')].map(row=>Number(row.dataset.offset))),Number(node.dataset.historyAfter)],omitted:!!node.querySelector('.session-activity-group[open]')&&[...node.querySelectorAll('[data-history-gap="source"],[data-fragment-from]')].some(gap=>!gap.closest('details:not([open])')&&gap.getBoundingClientRect().height>0)})),range=interval.range,targets=expected.filter(offset=>offset>=range[0]&&offset<range[1])
   if(expanded||interval.omitted&&targets.some(offset=>!visited.has(offset))){
    const beforeSweep=new Set(visited);if(!expanded)alreadyOpenSweeps++
    // Opening or composing an already-open run can expose a retained prefix
    // while later source occurrences remain behind an omitted interval. Read
    // this captured semantic suffix before continuing backward, even if its
    // raw window changes while those occurrences are being rehydrated.
    let sweepPrevious='',sweepStalled=0
    while(targets.some(offset=>!visited.has(offset))){const position=await page.locator('#session-records').evaluate(node=>[node.dataset.historyBefore,node.dataset.historyAfter,node.dataset.renderPart,Math.round(node.scrollTop)].join(':'));sweepStalled=position===sweepPrevious?sweepStalled+1:0;assert.ok(sweepStalled<6,JSON.stringify({phase:'busy-opened-suffix',dock,range,targets:[expected.indexOf(targets[0]),expected.indexOf(targets.at(-1))],missing:targets.filter(offset=>!visited.has(offset)).slice(0,8).map(offset=>expected.indexOf(offset)),seen:visited.size,inputSteps,acquisitions,state:await busyState()}));sweepPrevious=position;await move(1)}
    for(const offset of visited)if(!beforeSweep.has(offset)){forwardRecovered.add(expected.indexOf(offset));if(!expanded)alreadyOpenRecovered.add(expected.indexOf(offset))}
   }
  }
  await page.locator('#session-records').press('End');await until(()=>page.locator('#session-records').evaluate(node=>node.dataset.latestPending==='false'&&Number(node.dataset.historyAfter)>=Number(node.dataset.historyLatest)&&node.scrollHeight-node.scrollTop-node.clientHeight<=4))
  for(let opened=0;await page.locator('#session-records').evaluate(node=>node.scrollHeight<=node.clientHeight+1);opened++){assert.ok(opened<8,'a full busy source must provide a scrollable expanded run');const summary=page.locator('.session-activity-group:not([open]) > summary').last();await summary.click();await h.idle();await page.locator('#session-records').press('End');await until(()=>page.locator('#session-records').evaluate(node=>node.dataset.latestPending==='false'&&Number(node.dataset.historyAfter)>=Number(node.dataset.historyLatest)&&node.scrollHeight-node.scrollTop-node.clientHeight<=4))}
  const beforeHistory=await busyState(),historyMotion=await continuousScrollStep(page,-1);await until(()=>page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible()).catch(async error=>{console.log(JSON.stringify({phase:'busy-history-input',dock,before:beforeHistory,motion:historyMotion,after:await busyState()}));throw error});await collect()
  let previous='',stalled=0
  for(let steps=0;await page.locator('#session-records').evaluate(node=>Number(node.dataset.historyScanBefore)>0)||expected.some(offset=>!visited.has(offset));steps++){
   const fingerprint=await page.locator('#session-records').evaluate(node=>[node.dataset.historyBefore,node.dataset.historyAfter,node.dataset.renderPart,Math.round(node.scrollTop)].join(':'));stalled=fingerprint===previous?stalled+1:0;assert.ok(stalled<6,JSON.stringify({phase:'busy-complete-source',dock,seen:visited.size,expected:expected.length,missing:expected.filter(offset=>!visited.has(offset)).slice(0,8).map(offset=>({offset,index:expected.indexOf(offset)})),state:await busyState(),continuum:await page.locator('#session-records').evaluate(node=>({phases:window.busyPhases.slice(-8),mounted:[...node.querySelectorAll('.session-record')].map(row=>window.busySourceIndex.get(Number(row.dataset.offset))).filter((_,index,all)=>index<3||index>=all.length-3),gaps:[...node.querySelectorAll('[data-history-gap],[data-fragment-from]')].slice(0,8).map(gap=>({...gap.dataset,top:gap.getBoundingClientRect().top-node.getBoundingClientRect().top,bottom:gap.getBoundingClientRect().bottom-node.getBoundingClientRect().top}))})),reads:h.reads.filter(read=>read.path.endsWith('/transcript')&&Number(new URLSearchParams(read.query).get('offset'))>2100000).slice(-6)}));previous=fingerprint
   await move(-1);await collect()
  }
  assert.equal(await page.evaluate(()=>window.busyInvalid),false,'every mounted source interval is ordered without duplicate occurrences');assert.ok(await page.evaluate(()=>window.busyPeak)<=200);assert.deepEqual([...visited].sort((a,b)=>a-b),expected,'every source occurrence remains reachable in order beyond the initial byte and row limits')
  console.log(JSON.stringify({busyTraversal:{dock,initialOpen,expected:expected.length,seen:visited.size,acquisitions,actualRequests:h.reads.filter(read=>read.path.endsWith('/transcript')).length-coverageReads,repeatedIntervals:h.reads.filter(read=>read.path.endsWith('/transcript')).slice(coverageReads).map(read=>read.query).filter((query,index,all)=>all.indexOf(query)!==index).length,inputSteps,expandedRuns,alreadyOpenSweeps,alreadyOpenRecovered:[...alreadyOpenRecovered].sort((a,b)=>a-b).reduce((runs,index)=>{const last=runs.at(-1);if(last&&last[1]+1===index)last[1]=index;else runs.push([index,index]);return runs},[]),forwardRecovered:[...forwardRecovered].sort((a,b)=>a-b).filter(index=>index>=2100).slice(0,100)}}))
  assert.ok(h.peak()<=2);assert.deepEqual(h.errors,[])
 }finally{running=false;await producer;await h.close()}
})
test('membership preserves an immutable session download while its native write acknowledgement is pending',{timeout:20000},async()=>{
 const bytes=Buffer.from(JSON.stringify({text:'x'.repeat(150000)})+'\n'),h=await openSessionExplorer({transcriptBytes:bytes}),{page}=h
 try{
  await page.addInitScript(()=>{const get=StorageManager.prototype.getDirectory;let released=false;const held=new Promise(resolve=>window.releaseSaveWrite=()=>{released=true;resolve()});StorageManager.prototype.getDirectory=async function(...args){const root=await get.apply(this,args),directory=await root.getDirectoryHandle('hmem-downloads',{create:true}),file=directory.getFileHandle.bind(directory);directory.getFileHandle=async(...fileArgs)=>{const handle=await file(...fileArgs),create=handle.createWritable.bind(handle);handle.createWritable=async(...writeArgs)=>{const writer=await create(...writeArgs),write=writer.write.bind(writer);writer.write=async bytes=>{if(!released){window.saveWriteStarted=true;await held}return write(bytes)};return writer};return handle};return {getDirectoryHandle:async()=>directory}}})
  await h.start();await h.idle();const card=page.locator('#entity-'+h.fixture.tasks[0].id);await card.locator('.btn-extras-toggle').click();await card.getByRole('button',{name:'Evidence',exact:true}).click();const group=card.locator('.task-session-evidence');await group.waitFor();await h.idle()
  const first=page.waitForEvent('download');await group.getByRole('button',{name:/Download session/}).click();await page.waitForFunction(()=>window.saveWriteStarted)
  await h.associate(h.fixture.tasks[0].id);await page.waitForTimeout(100);assert.equal(await group.getByText('Save cancelled because its authority changed.',{exact:true}).count(),0)
  await page.evaluate(()=>window.releaseSaveWrite());assert.deepEqual(await downloadedBytes(await first),bytes)
  await until(()=>group.getByRole('button',{name:/Download session/}).isEnabled())
  const offsets=h.reads.filter(row=>row.path.endsWith('/evidence')).map(row=>new URLSearchParams(row.query).get('offset'));assert.deepEqual(offsets,['0','65536','131072'],'the pending write ACK keeps its exact chunk ownership across the read barrier')
  const second=page.waitForEvent('download');await group.getByRole('button',{name:/Download session/}).click();assert.deepEqual(await downloadedBytes(await second),bytes);assert.ok(h.peak()<=2);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})
test('shared compact cards retain parent tree inside a forty-percent wider dock and stack narrowly',{timeout:45000},async()=>{
 const h=await openSessionExplorer({activeCount:3,runtime:{model:'Actual running model',role:'Developer'}}),{page}=h
 h.activeSessions[1].parent_session_id=h.active.id;h.activeSessions[2].parent_session_id=h.active.id
 for(const session of h.activeSessions.slice(1))session.ancestors=[{id:h.active.id,title:'Parent session',format_label:h.active.format_label,state:'active'}]
 try{
  await page.setViewportSize({width:2400,height:1600});await h.start();await h.idle();const card=page.locator('#entity-'+h.fixture.tasks[0].id),before=await card.boundingBox()
  await card.locator('.task-sessions-icon').click();await page.locator('#session-records').waitFor();await h.idle();const after=await card.boundingBox()
  assert.ok(after.width/before.width>=1.35&&after.width/before.width<=1.45,JSON.stringify({before,after,ratio:after.width/before.width}))
  assert.equal(await card.locator('.task-session-work-context .session-tree-children .session-card').count(),2);assert.equal(await card.locator('#session-records').count(),1)
  assert.equal(await card.locator('.task-session-work-context .session-card .session-connection').count(),0)
  assert.equal(await card.locator('.task-session-work-context .session-card .session-runtime-value').first().innerText(),'[Developer] unknown:Actual running model:unknown')
  assert.ok(await card.locator('.session-summary-heading').first().evaluate(node=>node.firstElementChild.classList.contains('session-state-dot')))
  if(process.env.SESSION_PUSH_CARDS_SCREENSHOTS)await page.screenshot({path:join(process.env.SESSION_PUSH_CARDS_SCREENSHOTS,'session-dock-wide.png'),fullPage:true})
  await page.setViewportSize({width:430,height:1600});assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth));const layout=await card.evaluate(node=>{const body=node.querySelector(':scope > .task-card-work > .card-body').getBoundingClientRect(),dock=node.querySelector('.task-session-dock').getBoundingClientRect();return {bodyBottom:body.bottom,dockTop:dock.top}});assert.ok(layout.dockTop>=layout.bodyBottom)
  await card.locator('.task-session-dock').scrollIntoViewIfNeeded();await card.locator('.task-session-picker').evaluate(node=>node.scrollTop=node.scrollHeight);await card.locator('.task-session-choice').last().locator('button').focus();assert.equal(await card.locator('.task-session-choice').last().locator('button').evaluate(node=>node===document.activeElement),true);if(process.env.SESSION_PUSH_CARDS_SCREENSHOTS)await page.screenshot({path:join(process.env.SESSION_PUSH_CARDS_SCREENSHOTS,'session-dock-narrow.png'),fullPage:true})
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})
test('tasks with only terminated sessions retain an enabled grey agent control',{timeout:30000},async()=>{
 const h=await openSessionExplorer({activeCount:0}),{page}=h
 try{await h.start();await h.idle();const icon=page.locator('#entity-'+h.fixture.tasks[0].id+' .task-sessions-icon');await until(()=>icon.isEnabled());assert.equal(await icon.innerText(),'View Agents');assert.equal(await icon.locator('.session-state-inactive').count(),1);assert.match(await icon.getAttribute('title'),/no active sessions/);assert.equal(await icon.isDisabled(),false)}finally{await h.close()}
})
test('Live becomes Quiet after thirty seconds without events while lifecycle activity stays green',{timeout:25000},async()=>{
 const h=await openSessionExplorer(),{page}=h
 try{
  await h.start();await h.idle();h.active.last_seen_at=new Date(Date.now()-27000).toISOString();await page.getByRole('button',{name:'Sessions',exact:true}).click();await page.locator('.session-summary').waitFor()
  await until(async()=>await page.locator('.session-summary').count()===0)
  await page.getByRole('button',{name:'Quiet',exact:true}).click();await page.locator('.session-summary').waitFor()
  await page.getByRole('button',{name:'Projects',exact:true}).click()
  const icon=page.locator('#entity-'+h.fixture.tasks[0].id+' > .card-header .task-sessions-icon')
  await until(async()=>await icon.getAttribute('aria-label')==='View Agents'&&await icon.isEnabled()&&(await icon.getAttribute('class')).includes('has-active-sessions'))
  assert.match(await icon.getAttribute('title'),/active sessions/);assert.equal(await icon.locator('.session-state-inactive').count(),0)
  assert.equal(h.active.state,'active');assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})
test('an append racing a sealed live range waits for verified archive ownership and retains transcript capabilities',{timeout:30000},async()=>{
 const h=await openSessionExplorer(),{page}=h;let race=false,conflicts=0,phase='initial',releaseArchive,contentStarted=false
 const archiveGate=new Promise(resolve=>{releaseArchive=resolve}),generation='33333333-3333-4333-8333-333333333333'
 const hash=bytes=>createHash('sha256').update(bytes).digest('hex')
 try{
  await page.route('**/sessions/'+h.active.id+'/archive**',async route=>{
   const raw=h.content.get(h.active.id),gzip=gzipSync(raw),url=new URL(route.request().url())
   if(!url.pathname.endsWith('/content'))return route.fulfill({json:{workspace_id:h.fixture.workspace.id,session_id:h.active.id,state:'ready',generation_id:generation,codec:'gzip-v1',raw_bytes:String(raw.length),raw_records:String(h.active.committed_records),raw_sha256:hash(raw),compressed_bytes:String(gzip.length),compressed_sha256:hash(gzip),content_path:`/api/v1/workspaces/${h.fixture.workspace.id}/sessions/${h.active.id}/archive/${generation}/content`}})
   contentStarted=true;await archiveGate
   const etag='"seal-race-verified-archive"'
   if(route.request().headers()['if-none-match'])return route.fulfill({status:304,headers:{ETag:etag}})
   return route.fulfill({body:gzip,headers:{ETag:etag,'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)}})
  })
  await page.route('**/sessions/*/transcript?*',async route=>{if(race){race=false;conflicts++;Object.assign(h.active,{state:'terminated',terminated_at:new Date().toISOString(),final_bytes:h.active.committed_bytes,final_sha256:createHash('sha256').update(h.content.get(h.active.id)).digest('hex')});await route.fulfill({status:409,json:{error:'session_terminated'}})}else await route.fallback()})
  await h.start();await h.idle();await page.getByRole('button',{name:'Sessions',exact:true}).click();await page.locator('.session-summary a').first().click();const prose=page.getByRole('heading',{name:'A calmer session explorer'});await prose.waitFor({timeout:8000});phase='open-group';const group=page.locator('.session-activity-group').first();await group.locator(':scope > summary').click();await prose.evaluate(node=>window.sealProse=node);await group.evaluate(node=>window.sealGroup=node)
  phase='append';race=true;await h.appendRecords(message('Last committed reply before seal'));phase='verified-seal-transfer'
  await until(()=>contentStarted);assert.equal(await page.getByText('Last committed reply before seal',{exact:true}).count(),0,'sealed content is not published before complete transfer and receipt verification');assert.equal(await page.locator('.session-sealed-viewer #session-records .session-record').count(),0,'no sealed transcript is published before complete verification');releaseArchive()
  await page.getByText('Verified complete archive.',{exact:true}).waitFor();phase='render-final-reply'
  await page.getByText('Last committed reply before seal',{exact:true}).waitFor({timeout:8000});phase='render-sealed';await page.getByText('Saved session',{exact:true}).waitFor({state:'attached',timeout:8000});await page.getByRole('button',{name:/Download session/}).waitFor({timeout:8000})
  assert.equal(conflicts,1);await page.getByRole('heading',{name:'A calmer session explorer'}).waitFor()
  const sealedGroup=page.locator('.session-activity-group').first();await sealedGroup.locator(':scope > summary').click()
  const tool=page.locator('.session-content-tool').first();await tool.locator(':scope > summary').click();await tool.locator('.json-key').first().waitFor()
  assert.deepEqual(JSON.parse(await tool.locator('pre').textContent()),{path:'src/Feature/Sessions.elm'})
  const thinking=page.locator('.session-content-thinking').first(),thinkingGroup=thinking.locator('xpath=ancestor::details[contains(@class,"session-activity-group")]')
  if(await thinkingGroup.getAttribute('open')===null)await thinkingGroup.locator(':scope > summary').click()
  await thinking.locator(':scope > summary').click();await page.getByText('Private reasoning stays collapsed.',{exact:true}).waitFor()
  const download=page.waitForEvent('download');await page.getByRole('button',{name:/Download session/}).click();assert.deepEqual(await downloadedBytes(await download),h.content.get(h.active.id))
  assert.ok(h.peak()<=2);assert.deepEqual(h.errors,[])
 }catch(error){throw Error(JSON.stringify({phase,conflicts,reads:h.reads.slice(-12),text:(await page.locator('body').innerText()).slice(-1500),error:error.message}))}finally{releaseArchive();await h.close()}
})
test('non-admin Home closes the probe-to-subscribe gap and reconnects its separate activity capability',{timeout:30000},async()=>{
 const h=await openSessionExplorer(),{page}=h;let active=false,reads=0,tickets=0,baselineResolve;const baselineStarted=new Promise(resolve=>baselineResolve=resolve)
 try{
  await page.route('**/api/v1/session-activity?*',async route=>{const value=active;reads++;if(reads===1){baselineResolve();await sleep(200)};const u=new URL(route.request().url());await route.fulfill({json:{workspaces:u.searchParams.getAll('workspace_id').map(id=>({id,active:value})),projects:[]}})})
  await page.route('**/api/v1/session-activity/ticket',async route=>{tickets++;await baselineStarted;active=true;await route.fallback()})
  await page.goto(h.origin+'/');const light=page.locator('.card.clickable').getByRole('img',{name:'Active sessions in this workspace',exact:true});await light.waitFor();assert.ok(reads>=2,'ready must refresh after the initial baseline')
  assert.equal(h.requests.filter(r=>r.path==='/api/v1/change-stream/ticket').length,0,'non-admin Home never asks for Global canonical authority')
  active=false;await page.evaluate(workspace=>window.pushActivityFrames([{schema_version:1,type:'session_activity_changed',workspace_id:workspace}]),h.fixture.workspace.id);await until(async()=>await light.count()===0)
  const stable=reads;await sleep(1200);assert.equal(reads,stable,'settled controls do not cause periodic activity HTTP')
  await page.evaluate(()=>window.closeActivityStream());await until(()=>tickets>=2);await light.waitFor();assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})
test('Home clears an omitted workspace activity projection and converges after restoration',{timeout:20000},async()=>{
 const h=await openSessionExplorer(),{page}=h;let present=true
 try{
  await page.route('**/api/v1/session-activity?*',async route=>{const u=new URL(route.request().url());await route.fulfill({json:{workspaces:present?u.searchParams.getAll('workspace_id').map(id=>({id,active:true})):[],projects:[]}})})
  await page.goto(h.origin+'/');const card=page.locator('.card.clickable').filter({hasText:h.fixture.workspace.name}),light=card.getByRole('img',{name:'Active sessions in this workspace',exact:true});await light.waitFor()
  present=false;await page.evaluate(workspace=>window.pushActivityFrames([{schema_version:1,type:'session_activity_changed',workspace_id:workspace}]),h.fixture.workspace.id);await until(async()=>await light.count()===0);assert.equal(await card.count(),1,'an unknown activity result does not remove catalogue membership')
  present=true;await page.evaluate(workspace=>window.pushActivityFrames([{schema_version:1,type:'session_activity_changed',workspace_id:workspace}]),h.fixture.workspace.id);await light.waitFor();assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})
for(const phase of ['acquisition','streaming'])test(`membership read invalidation preserves an evidence archive during ${phase}`,{timeout:20000},async()=>{
 const h=await openSessionExplorer({savedCount:53}),{page}=h,body=Buffer.alloc(70000,113);let binary=0
 try{
  await page.addInitScript(phase=>{const get=StorageManager.prototype.getDirectory;let released=false;const held=new Promise(resolve=>window.releaseArchivePhase=()=>{released=true;resolve()});StorageManager.prototype.getDirectory=async function(...args){if(phase==='acquisition'&&!released){window.archivePhaseStarted=true;await held}const root=await get.apply(this,args);const directory=await root.getDirectoryHandle('hmem-downloads',{create:true});const file=directory.getFileHandle.bind(directory);directory.getFileHandle=async(...fileArgs)=>{const handle=await file(...fileArgs),create=handle.createWritable.bind(handle);handle.createWritable=async(...writeArgs)=>{const writer=await create(...writeArgs),write=writer.write.bind(writer);writer.write=async bytes=>{if(phase==='streaming'&&!released){window.archivePhaseStarted=true;await held}return write(bytes)};return writer};return handle};return {getDirectoryHandle:async()=>directory}}},phase)
  await page.route('**/evidence.zip',route=>{binary++;return route.fulfill({status:200,headers:{'Content-Type':'application/zip'},body})})
  await h.start();await h.idle();const card=page.locator('#entity-'+h.fixture.tasks[0].id);await card.locator('.btn-extras-toggle').click();await card.getByRole('button',{name:'Evidence',exact:true}).click();const group=card.locator('.task-session-evidence');await group.waitFor();await h.idle()
  const first=page.waitForEvent('download');await group.getByRole('button',{name:'↓ Download all',exact:true}).click();await page.waitForFunction(()=>window.archivePhaseStarted)
  await page.evaluate(({workspace,session})=>window.pushHierarchyFrames([{schema_version:1,type:'change',event:{schema_version:1,event_id:'archive-membership',scope:'workspace',workspace_id:workspace,occurred_at:new Date().toISOString(),transaction:{id:'archive-membership',cause:'rest',request_id:null},actor:{type:'system',id:null},entity:{type:'agent_session',id:session,action:'updated'},invalidations:[{kind:'entity',target:'agent_session:'+session},{kind:'collection',target:'sessions:'+workspace},{kind:'collection',target:'active_sessions:'+workspace}]}}]),{workspace:h.fixture.workspace.id,session:h.active.id})
  await page.waitForTimeout(100);assert.equal(await group.getByText('Save cancelled because its authority changed.',{exact:true}).count(),0);assert.equal(binary,phase==='streaming'?1:0)
  await page.evaluate(()=>window.releaseArchivePhase());const download=await first;assert.deepEqual(await downloadedBytes(download),body)
  await until(()=>group.getByRole('button',{name:'↓ Download all',exact:true}).isEnabled());assert.equal(binary,1)
  const second=page.waitForEvent('download');await group.getByRole('button',{name:'↓ Download all',exact:true}).click();await second;assert.equal(binary,2,'the old archive physical admission and writer must complete before reuse');assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})
