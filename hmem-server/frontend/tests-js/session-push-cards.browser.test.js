import assert from 'node:assert/strict'
import test from 'node:test'
import { join } from 'node:path'
import {createHash} from 'node:crypto'
import {openSessionExplorer} from './support/sessions-ux-fixture.mjs'
const sleep=ms=>new Promise(r=>setTimeout(r,ms))
const downloadedBytes=async download=>{const chunks=[];for await(const chunk of await download.createReadStream())chunks.push(chunk);return Buffer.concat(chunks)}
const until=async fn=>{const end=Date.now()+15000;while(Date.now()<end){if(await fn())return;await sleep(25)}throw Error('Busy session did not converge')}
const message=text=>({type:'response_item',payload:{type:'message',role:'assistant',content:[{type:'output_text',text}]}})
for(const dock of [false,true])test(`initial ${dock?'task dock':'Sessions list and links'} loads a latest long transcript while progress invalidates faster than responses`,{timeout:45000},async()=>{
 const records=[...Array.from({length:2200},(_,i)=>({type:'mystery',padding:'p'.repeat(1100),i})),message('Visible latest agent response'),{type:'response_item',payload:{type:'function_call',name:'busy_tool',arguments:'{"initial":true}'}}]
 const h=await openSessionExplorer({runtime:{model:'Running target',role:'Developer'},transcriptBytes:Buffer.from(records.map(r=>JSON.stringify(r)).join('\n')+'\n')}),{page}=h
 let running=true,producer
 try{
  await page.route('**/api/v1/**/sessions**',async route=>{if(!new URL(route.request().url()).pathname.endsWith('/transcript'))await sleep(140);await route.fallback()})
  await h.start();let index=0
  producer=(async()=>{while(running){await h.appendRecords({type:'response_item',payload:{type:'function_call',name:'busy_tool',arguments:JSON.stringify({index:index++})}});await sleep(35)}})()
  if(dock){const icon=page.locator('#entity-'+h.fixture.tasks[0].id+' .task-sessions-icon');await until(()=>icon.isEnabled());await icon.click()}
  else{await page.getByRole('button',{name:'Sessions',exact:true}).click();await page.locator('.session-summary a').first().click();await until(async()=>await page.locator('.session-task-card').count()===2)}
  await page.getByText('Visible latest agent response',{exact:true}).waitFor();assert.ok(running,'load completed during active progress, without reselection')
  const prose=page.getByText('Visible latest agent response',{exact:true}),group=page.locator('.session-activity-group').last();await group.locator(':scope > summary').click();await group.evaluate(node=>window.busyGroup=node);await prose.evaluate(node=>window.busyProse=node)
  const latest=await group.locator('article').last().getAttribute('data-offset');await until(async()=>await group.locator('article').last().getAttribute('data-offset')!==latest)
  assert.equal(await page.evaluate(()=>window.busyGroup.isConnected&&window.busyGroup.open&&window.busyProse.isConnected),true)
  assert.ok(h.reads.filter(row=>row.path.endsWith('/transcript')).length>=32);assert.ok(h.peak()<=2);assert.deepEqual(h.errors,[])
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
  assert.equal(await card.locator('.task-session-work-context .session-card .session-runtime-value').first().innerText(),'Developer')
  assert.ok(await card.locator('.session-summary-heading').first().evaluate(node=>node.firstElementChild.classList.contains('session-state-dot')))
  if(process.env.SESSION_PUSH_CARDS_SCREENSHOTS)await page.screenshot({path:join(process.env.SESSION_PUSH_CARDS_SCREENSHOTS,'session-dock-wide.png'),fullPage:true})
  await page.setViewportSize({width:430,height:1600});assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth));const layout=await card.evaluate(node=>{const body=node.querySelector(':scope > .card-body').getBoundingClientRect(),dock=node.querySelector('.task-session-dock').getBoundingClientRect();return {bodyBottom:body.bottom,dockTop:dock.top}});assert.ok(layout.dockTop>=layout.bodyBottom)
  await card.locator('.task-session-dock').scrollIntoViewIfNeeded();await card.locator('.task-session-picker').evaluate(node=>node.scrollTop=node.scrollHeight);await card.locator('.task-session-choice').last().locator('button').focus();assert.equal(await card.locator('.task-session-choice').last().locator('button').evaluate(node=>node===document.activeElement),true);if(process.env.SESSION_PUSH_CARDS_SCREENSHOTS)await page.screenshot({path:join(process.env.SESSION_PUSH_CARDS_SCREENSHOTS,'session-dock-narrow.png'),fullPage:true})
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})
test('tasks without running sessions show only an accessible grey indicator',{timeout:30000},async()=>{
 const h=await openSessionExplorer({activeCount:0}),{page}=h
 try{await h.start();await h.idle();const icon=page.locator('#entity-'+h.fixture.tasks[0].id+' .task-sessions-icon');assert.equal(await icon.innerText(),'');assert.equal(await icon.locator('.session-state-inactive').count(),1);assert.match(await icon.getAttribute('aria-label'),/no active sessions/);assert.equal(await icon.isDisabled(),true)}finally{await h.close()}
})
test('Live becomes Quiet after thirty seconds without events while lifecycle activity stays green',{timeout:25000},async()=>{
 const h=await openSessionExplorer(),{page}=h
 try{
  await h.start();await h.idle();h.active.last_seen_at=new Date(Date.now()-27000).toISOString();await page.getByRole('button',{name:'Sessions',exact:true}).click();await page.locator('.session-summary').waitFor()
  await until(async()=>await page.locator('.session-summary').count()===0)
  await page.getByRole('button',{name:'Quiet',exact:true}).click();await page.locator('.session-summary').waitFor()
  await page.getByRole('button',{name:'Projects',exact:true}).click();await until(async()=>await page.locator('#entity-'+h.fixture.tasks[0].id+' .task-sessions-icon').innerText()==='Active Agent →')
  assert.equal(h.active.state,'active');assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})
test('an append racing a sealed live range recovers canonical proof without removing admitted prose or disclosures',{timeout:30000},async()=>{
 const h=await openSessionExplorer(),{page}=h;let race=false,conflicts=0,phase='initial'
 try{
  await page.route('**/sessions/*/transcript?*',async route=>{if(race){race=false;conflicts++;Object.assign(h.active,{state:'terminated',terminated_at:new Date().toISOString(),final_bytes:h.active.committed_bytes,final_sha256:createHash('sha256').update(h.content.get(h.active.id)).digest('hex')});await route.fulfill({status:409,json:{error:'session_terminated'}})}else await route.fallback()})
  await h.start();await h.idle();await page.getByRole('button',{name:'Sessions',exact:true}).click();await page.locator('.session-summary a').first().click();const prose=page.getByRole('heading',{name:'A calmer session explorer'});await prose.waitFor({timeout:8000});phase='open-group';const group=page.locator('.session-activity-group').first();await group.locator(':scope > summary').click();await prose.evaluate(node=>window.sealProse=node);await group.evaluate(node=>window.sealGroup=node)
  phase='append';race=true;await h.appendRecords(message('Last committed reply before seal'));phase='render-final-reply'
  await page.getByText('Last committed reply before seal',{exact:true}).waitFor({timeout:8000});phase='render-sealed';await page.getByText('Saved session',{exact:true}).waitFor({state:'attached',timeout:8000});await page.getByRole('button',{name:/Download session/}).waitFor({timeout:8000})
  assert.equal(conflicts,1);assert.equal(await page.evaluate(()=>window.sealProse.isConnected&&window.sealGroup.isConnected&&window.sealGroup.open),true);assert.ok(h.peak()<=2);assert.deepEqual(h.errors,[])
 }catch(error){throw Error(JSON.stringify({phase,conflicts,reads:h.reads.slice(-12),text:(await page.locator('body').innerText()).slice(-1500),error:error.message}))}finally{await h.close()}
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
