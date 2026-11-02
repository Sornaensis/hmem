import assert from 'node:assert/strict'
import test from 'node:test'
import {gzipSync} from 'node:zlib'
import {createHash} from 'node:crypto'
import {openSessionExplorer} from './support/sessions-ux-fixture.mjs'

const message=(role,text)=>({type:'response_item',timestamp:'2026-10-24T10:00:00Z',payload:{type:'message',role,content:[{type:'input_text',text}]}})
const until=async fn=>{const end=Date.now()+12000;while(Date.now()<end){if(await fn())return;await new Promise(resolve=>setTimeout(resolve,25))}throw Error('Conversation did not settle')}
async function singleConversation(page,saved=false){
 assert.equal(await page.locator('.session-presentation-tabs,[data-presentation-mode],#session-records[role="tabpanel"]').count(),0)
 const region=page.getByRole('region',{name:saved?'Verified archive records':'Session conversation',exact:true})
 assert.equal(await region.count(),1)
 assert.equal(await region.getAttribute('aria-labelledby'),null)
 assert.equal(await region.getAttribute('tabindex'),'0')
}
async function archive(h,session=h.saved){
 const raw=h.content.get(session.id),gzip=gzipSync(raw),hash=bytes=>createHash('sha256').update(bytes).digest('hex'),generation='33333333-3333-4333-8333-333333333333'
 await h.page.route('**/sessions/**/archive**',async route=>{
  if(new URL(route.request().url()).pathname.endsWith('/content'))return route.request().headers()['if-none-match']?route.fulfill({status:304,headers:{ETag:'"conversation"'}}):route.fulfill({body:gzip,headers:{ETag:'"conversation"','Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)}})
  return route.fulfill({json:{workspace_id:h.fixture.workspace.id,session_id:session.id,state:'ready',generation_id:generation,codec:'gzip-v1',raw_bytes:String(raw.length),raw_records:String(raw.toString().trim().split('\n').length),raw_sha256:hash(raw),compressed_bytes:String(gzip.length),compressed_sha256:hash(gzip),content_path:'/api/v1/workspaces/'+h.fixture.workspace.id+'/sessions/'+session.id+'/archive/'+generation+'/content'}})
 })
}
const open=async(h,saved=false)=>{if(saved)await archive(h);await h.start();await h.page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+(saved?h.saved.id:h.active.id));await h.page.locator('#session-records').waitFor();await h.idle()}

for(const saved of [false,true])test((saved?'verified':'active')+' conversation retains native context, source order, group counts and record inspection',{timeout:60000},async()=>{
 const records=[message('system','Native system <script>window.injected=true</script> context.'),message('developer','Native developer context.'),message('user','Quoted developer instructions are ordinary user prose.'),
  {type:'response_item',timestamp:'2026-10-24T10:00:00Z',payload:{type:'function_call',name:'read_file',arguments:'{"path":"safe"}'}},
  {type:'response_item',payload:{type:'function_call_output',call_id:'read',output:'{"ok":true}'}},message('assistant','Original answer after the tool.'),
  {type:'response_item',payload:{type:'reasoning',encrypted_content:'never-render-ciphertext'}},{type:'event_msg',payload:{type:'future_event',value:42}}]
 const h=await openSessionExplorer({transcriptBytes:Buffer.from(records.map(JSON.stringify).join('\n')+'\n'),activeCount:2}),{page}=h
 h.activeSessions[1].last_seen_at=new Date().toISOString()
 try{
  await open(h,saved);await page.locator('[data-mode-key^="context:"]').first().waitFor();await singleConversation(page,saved)
  assert.equal(await page.getByText('Native developer context.',{exact:true}).count(),0)
  assert.equal(await page.getByText('Quoted developer instructions are ordinary user prose.',{exact:true}).isVisible(),true)
  const system=page.locator('[data-mode-key^="context:"]').first();await system.locator(':scope > summary').click();await page.getByText('Native system <script>window.injected=true</script> context.',{exact:true}).waitFor();await h.idle()
  assert.equal(await page.evaluate(()=>window.injected),undefined)
  const count=()=>h.reads.filter(read=>read.path.endsWith('/transcript')).length,readCount=count()
  assert.match(await page.locator('.session-activity-group').first().locator(':scope > summary').innerText(),/Time unavailable.*Activity · 2 events/s)
  for(const group of await page.locator('.session-activity-group').all())await group.locator(':scope > summary').click()
  const tool=page.locator('.session-content-tool').first();await tool.locator(':scope > summary').click();await tool.locator('pre').waitFor();assert.match(await tool.innerText(),/"path"/)
  assert.equal(await page.getByText('never-render-ciphertext',{exact:false}).count(),0)
  await page.locator('.session-content-event summary').click();await page.locator('.session-content-event pre').waitFor();assert.match(await page.locator('.session-content-event').innerText(),/future_event/)
  assert.equal(await page.locator('#session-records').evaluate(node=>{
   const user=node.querySelector('.session-role-user'),activity=node.querySelector('.session-activity-group'),answer=node.querySelector('.session-role-assistant:not(.session-record-activity)')
   return !!(user.compareDocumentPosition(activity)&Node.DOCUMENT_POSITION_FOLLOWING)&&!!(activity.compareDocumentPosition(answer)&Node.DOCUMENT_POSITION_FOLLOWING)
  }),true)
  assert.equal(await system.getAttribute('open'),'');assert.equal(count(),readCount)
  assert.equal(await page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible(),false)
  await page.locator('.session-summary a[href*="session='+h.activeSessions[1].id+'"]').click();await until(()=>page.locator('[data-mode-key^="context:"]').first().getAttribute('open').then(value=>value===null))
  await singleConversation(page);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('embedded conversation keeps disclosures and single-view semantics through workspace transfer',{timeout:60000},async()=>{
 const raw=Buffer.from([message('system','Embedded native context.'),message('assistant','Embedded conversation prose.')].map(JSON.stringify).join('\n')+'\n')
 const h=await openSessionExplorer({transcriptBytes:raw}),{page}=h
 try{
  await h.start();await h.idle();const card=page.locator('#entity-'+h.fixture.tasks[0].id),agent=card.locator(':scope > .card-header .task-sessions-icon');await until(()=>agent.isEnabled());await agent.click();await page.getByText('Embedded conversation prose.',{exact:true}).waitFor();await h.idle();await singleConversation(page)
  const context=page.locator('[data-mode-key^="context:"]');await context.locator(':scope > summary').click();await page.getByText('Embedded native context.',{exact:true}).waitFor()
  await page.getByRole('link',{name:'Jump To Session ↗',exact:true}).click();await h.idle();await singleConversation(page);assert.equal(await context.getAttribute('open'),'')
  await page.getByRole('button',{name:'Projects',exact:true}).click();await h.idle();await singleConversation(page);assert.equal(await context.getAttribute('open'),'')
  assert.equal(await page.locator('#session-records').count(),1);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('context disclosures survive appends and the same-authority active to verified transition',{timeout:60000},async()=>{
 const raw=Buffer.from([message('system','Original native context.'),message('assistant','Original prose occurrence.')].map(JSON.stringify).join('\n')+'\n')
 const h=await openSessionExplorer({transcriptBytes:raw}),{page}=h
 try{
  await open(h);await page.getByText('Original prose occurrence.',{exact:true}).waitFor()
  const context=page.locator('[data-mode-key^="context:"]');await context.locator(':scope > summary').click();await page.getByText('Original native context.',{exact:true}).waitFor();await h.idle()
  const key=await context.getAttribute('data-mode-key')
  await h.appendRecords(message('assistant','Appended prose occurrence.'));await page.getByText('Appended prose occurrence.',{exact:true}).waitFor();assert.equal(await page.getByText('Original prose occurrence.',{exact:true}).isVisible(),true)
  const bytes=h.content.get(h.active.id);Object.assign(h.active,{state:'terminated',terminated_at:new Date().toISOString(),final_bytes:bytes.length,final_sha256:createHash('sha256').update(bytes).digest('hex')});await archive(h,h.active)
  await page.evaluate(({workspace,id})=>window.pushHierarchyFrames([{schema_version:1,type:'change',event:{schema_version:1,event_id:'conversation-termination',scope:'workspace',workspace_id:workspace,occurred_at:new Date().toISOString(),transaction:{id:'conversation-termination',cause:'rest',request_id:null},actor:{type:'system',id:null},entity:{type:'agent_session',id,action:'updated'},invalidations:[{kind:'entity',target:'agent_session:'+id},{kind:'collection',target:'sessions:'+workspace}]}}]),{workspace:h.fixture.workspace.id,id:h.active.id})
  await page.getByText('Verified complete archive.',{exact:true}).waitFor();await page.getByText('Original prose occurrence.',{exact:true}).waitFor();await page.getByText('Original native context.',{exact:true}).waitFor();await singleConversation(page,true)
  assert.equal(await context.getAttribute('data-mode-key'),key);assert.equal(await context.getAttribute('open'),'');assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

async function assertReadingResize(saved,widths){
 const bytes=Buffer.from(Array.from({length:85},(_,i)=>message(i%2?'assistant':'user','Occurrence '+i+': '+'readable words '.repeat(15))).map(JSON.stringify).join('\n')+'\n')
 const h=await openSessionExplorer({transcriptBytes:bytes}),{page}=h
 try{
  await open(h,saved);await page.getByText('Occurrence 84:',{exact:false}).waitFor();await h.idle()
  const viewport=page.locator('#session-records');await viewport.hover();await page.mouse.wheel(0,-270);await until(()=>page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible());await h.idle()
  const latestVisible=await page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible()
  const before=h.reads.filter(read=>/\/(transcript|evidence|archive)$/.test(read.path)).length
  for(const width of widths){
   const source=await viewport.evaluate(node=>{const box=node.getBoundingClientRect(),row=[...node.querySelectorAll('[data-block-key]')].find(row=>{const r=row.getBoundingClientRect();return r.bottom>box.top+5&&r.top<box.bottom});return row.dataset.blockKey})
   await page.setViewportSize({width,height:900});await h.idle()
   const position=await viewport.evaluate((node,key)=>{const row=[...node.querySelectorAll('[data-block-key]')].find(row=>row.dataset.blockKey===key),box=node.getBoundingClientRect(),r=row?.getBoundingClientRect();return {visible:!!r&&r.bottom>box.top&&r.top<box.bottom,source:key,row:r?.toJSON(),bounds:box.toJSON(),top:node.scrollTop,attrs:{...node.dataset}}},source)
   assert.equal(position.visible,true,JSON.stringify({width,position}))
   assert.equal(await page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible(),latestVisible)
  }
  assert.equal(h.reads.filter(read=>/\/(transcript|evidence|archive)$/.test(read.path)).length,before)
  assert.ok(await viewport.locator('[data-block-key]').count()<=200);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
}

for(const saved of [false,true])test((saved?'verified':'active')+' conversation resizing preserves manual reading and Jump to latest state without extra history',{timeout:60000},()=>assertReadingResize(saved,[1920,800]))

// Both callbacks remain runnable: the exact same narrow-to-wide clamp fails on
// baseline 8268a43b and is owned by the separate frontend repair below.
for(const saved of [false,true])test((saved?'verified':'active')+' paused conversation retains its occurrence after narrow-to-wide resize',{
 timeout:60000,todo:'Inherited resize regression: hmem b1253a0a-122d-4ed8-9a72-8235e4c4ff58'
},()=>assertReadingResize(saved,[1920,800,1280]))

for(const saved of [false,true])test((saved?'verified':'active')+' closed activity occurrence and disclosure remain stable through reader resizing',{timeout:60000},async()=>{
 const records=[...Array.from({length:45},(_,i)=>message('assistant','Before group '+i+': '+'preceding prose '.repeat(12))),
  {type:'response_item',payload:{type:'function_call',name:'closed_group_tool',arguments:'{"original":true}'}},{type:'response_item',payload:{type:'function_call_output',call_id:'closed_group',output:'{"result":42}'}},
  ...Array.from({length:5},(_,i)=>message('assistant','After group '+i+': '+'following prose '.repeat(12)))]
 const h=await openSessionExplorer({transcriptBytes:Buffer.from(records.map(JSON.stringify).join('\n')+'\n')}),{page}=h
 try{
  await open(h,saved);await page.getByText('After group 4:',{exact:false}).waitFor();await h.idle()
  const viewport=page.locator('#session-records'),group=page.locator('.session-activity-group').first(),key=await group.getAttribute('data-group-key')
  await viewport.evaluate((node,key)=>{const group=[...node.querySelectorAll('[data-group-key]')].find(group=>group.dataset.groupKey===key);node.scrollTop+=group.getBoundingClientRect().top-node.getBoundingClientRect().top},key);await h.idle()
  const top=()=>group.evaluate(node=>node.getBoundingClientRect().top-document.getElementById('session-records').getBoundingClientRect().top),initial=await top()
  const before=h.reads.filter(read=>/\/(transcript|evidence|archive)$/.test(read.path)).length
  const authority=await viewport.evaluate(node=>[node.dataset.viewerGeneration,node.dataset.sealedDomLoad,node.dataset.windowPublication,node.dataset.sealedPageRequest])
  assert.equal(await group.getAttribute('open'),null);assert.ok(initial>=-1)
  for(const width of [1920,1000]){
   await page.setViewportSize({width,height:900});await h.idle()
   assert.equal(await group.getAttribute('data-group-key'),key);assert.equal(await group.getAttribute('open'),null);assert.ok(Math.abs(await top()-initial)<=32)
  }
  await group.locator(':scope > summary').click();await group.locator('.session-content-tool').first().waitFor();await group.locator(':scope > summary').click();await h.idle()
  assert.equal(await group.getAttribute('open'),null);assert.ok(Math.abs(await top()-initial)<=32)
  assert.equal(h.reads.filter(read=>/\/(transcript|evidence|archive)$/.test(read.path)).length,before)
  assert.deepEqual(await viewport.evaluate(node=>[node.dataset.viewerGeneration,node.dataset.sealedDomLoad,node.dataset.windowPublication,node.dataset.sealedPageRequest]),authority)
  assert.equal(await page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible(),false);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

for(const saved of [false,true])test((saved?'verified':'active')+' workspace keeps metadata, activity and descriptions aligned at wide, narrow and enlarged widths',{timeout:60000},async()=>{
 const fence=String.fromCharCode(96).repeat(3),records=[message('system','Geometry context '+'context words '.repeat(40)),message('user','Geometry user '+'readable words '.repeat(50)),message('assistant','Geometry prose '+'readable words '.repeat(80)),
  {type:'response_item',timestamp:'2026-10-24T10:00:01Z',payload:{type:'function_call',name:'geometry_tool',arguments:JSON.stringify({payload:'structured value '.repeat(35)})}},
  message('assistant',fence+'text\n'+'wide structured value '.repeat(12)+'\n'+fence),message('assistant','| Name | Value |\n| --- | --- |\n| A wide table | '+'table value '.repeat(20)+' |')]
 const h=await openSessionExplorer({transcriptBytes:Buffer.from(records.map(JSON.stringify).join('\n')+'\n')}),{page}=h
 try{
  await open(h,saved);await page.getByText('Geometry prose ',{exact:false}).waitFor()
  await page.locator('[data-mode-key^="context:"] > summary').click();await page.locator('.session-activity-group > summary').click();await page.locator('.session-content-tool > summary').click();await page.locator('.session-content-tool pre').waitFor();await h.idle()
  let ordinaryGlyph=null
  for(const [width,zoom] of [[3366,1],[1920,1],[430,1],[1920,2]]){
   await page.setViewportSize({width,height:1000});await page.evaluate(value=>{document.body.style.zoom=String(value)},zoom);await h.idle();await singleConversation(page,saved)
   const geometry=await page.locator('#session-records').evaluate(node=>{
    const rect=element=>element.getBoundingClientRect(),textNodes=[...node.querySelectorAll('.session-record-text')],body=prefix=>textNodes.find(element=>element.textContent.startsWith(prefix))
    const pair=prefix=>{const prose=body(prefix),meta=prose.closest('.session-record').querySelector('.session-record-meta');return {body:rect(prose).toJSON(),meta:rect(meta).toJSON(),cap:getComputedStyle(prose).maxWidth}}
    const glyph=document.createRange(),text=document.createTreeWalker(body('Geometry prose '),NodeFilter.SHOW_TEXT).nextNode();glyph.setStart(text,0);glyph.setEnd(text,1)
    return {glyph:glyph.getBoundingClientRect().width,ordinary:pair('Geometry prose '),user:pair('Geometry user '),context:pair('Geometry context '),summary:rect(node.querySelector('.session-activity-summary')).toJSON(),bounds:rect(node).toJSON(),client:node.clientWidth,scroll:node.scrollWidth,wide:textNodes.filter(element=>element.querySelector('pre,table')).map(element=>({width:rect(element).width,cap:getComputedStyle(element).maxWidth}))}
   })
   if(ordinaryGlyph===null)ordinaryGlyph=geometry.glyph
   assert.ok(Math.abs(geometry.glyph/ordinaryGlyph-zoom)<.05,JSON.stringify({width,zoom,glyph:geometry.glyph,ordinaryGlyph}))
   assert.ok(geometry.ordinary.body.width>0)
   for(const pair of [geometry.ordinary,geometry.user,geometry.context])assert.ok(Math.abs(pair.body.left-pair.meta.left)<=2*zoom,JSON.stringify({width,zoom,pair}))
   assert.ok(Math.abs(geometry.summary.left-geometry.ordinary.meta.left)<=20*zoom,JSON.stringify({width,zoom,geometry}))
   assert.ok(geometry.ordinary.body.width<=parseFloat(geometry.ordinary.cap)*zoom+1,JSON.stringify({width,zoom,geometry}))
   assert.ok(geometry.scroll<=geometry.client+1,JSON.stringify({width,zoom,geometry}))
   assert.equal(geometry.wide.length,2);assert.ok(geometry.wide.every(body=>body.cap==='none'&&body.width>=geometry.ordinary.body.width-2),JSON.stringify({width,zoom,geometry}))
   assert.equal(await page.locator('[data-mode-key^="context:"]').getAttribute('open'),'');assert.equal(await page.locator('.session-activity-group').getAttribute('open'),'')
  }
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})
