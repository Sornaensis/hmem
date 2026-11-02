import assert from 'node:assert/strict'
import test from 'node:test'
import {gzipSync} from 'node:zlib'
import {createHash} from 'node:crypto'
import {openSessionExplorer} from './support/sessions-ux-fixture.mjs'

const message=(role,text)=>({type:'response_item',payload:{type:'message',role,content:[{type:'input_text',text}]}})
const until=async fn=>{const end=Date.now()+12000;while(Date.now()<end){if(await fn())return;await new Promise(resolve=>setTimeout(resolve,25))}throw Error('Presentation did not settle')}
const change=async(page,mode)=>{await page.getByRole('tab',{name:mode,exact:true}).click();await until(()=>page.locator('#session-records').getAttribute('data-presentation').then(value=>value===({'Conversation':'conversation','Activity':'activity','Full transcript':'full'}[mode])));await page.evaluate(()=>new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve))))}
async function archive(h,session=h.saved){
 const raw=h.content.get(session.id),gzip=gzipSync(raw),hash=bytes=>createHash('sha256').update(bytes).digest('hex'),generation='33333333-3333-4333-8333-333333333333'
 await h.page.route('**/sessions/**/archive**',async route=>{
  if(new URL(route.request().url()).pathname.endsWith('/content'))return route.request().headers()['if-none-match']?route.fulfill({status:304,headers:{ETag:'"presentation"'}}):route.fulfill({body:gzip,headers:{ETag:'"presentation"','Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)}})
  return route.fulfill({json:{workspace_id:h.fixture.workspace.id,session_id:session.id,state:'ready',generation_id:generation,codec:'gzip-v1',raw_bytes:String(raw.length),raw_records:String(raw.toString().trim().split('\n').length),raw_sha256:hash(raw),compressed_bytes:String(gzip.length),compressed_sha256:hash(gzip),content_path:`/api/v1/workspaces/${h.fixture.workspace.id}/sessions/${session.id}/archive/${generation}/content`}})
 })
}

for(const saved of [false,true])test(`${saved?'verified':'active'} reader modes retain native context, source order, group counts and local state without extra reads`,{timeout:60000},async()=>{
 const records=[message('system','Native system <script>window.injected=true</script> context.'),message('developer','Native developer context.'),message('user','Quoted developer instructions are ordinary user prose.'),
  {type:'response_item',timestamp:'2026-10-24T10:00:00Z',payload:{type:'function_call',name:'read_file',arguments:'{"path":"safe"}'}},
  {type:'response_item',payload:{type:'function_call_output',call_id:'read',output:'{"ok":true}'}},message('assistant','Original answer after the tool.'),
  {type:'response_item',payload:{type:'reasoning',encrypted_content:'never-render-ciphertext'}},{type:'event_msg',payload:{type:'future_event',value:42}}]
 const h=await openSessionExplorer({transcriptBytes:Buffer.from(records.map(JSON.stringify).join('\n')+'\n'),activeCount:2});const{page}=h
 h.activeSessions[1].last_seen_at=new Date().toISOString()
 try{
  if(saved)await archive(h)
  await h.start();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+(saved?h.saved.id:h.active.id));await page.locator('[data-mode-key^="context:"]').first().waitFor();await h.idle()
  assert.equal(await page.getByRole('tab',{name:'Conversation',exact:true}).getAttribute('aria-selected'),'true')
  assert.equal(await page.getByText('Native developer context.',{exact:true}).count(),0)
  assert.equal(await page.getByText('Quoted developer instructions are ordinary user prose.',{exact:true}).isVisible(),true)
  const system=page.locator('[data-mode-key^="context:"]').first();await system.locator(':scope > summary').click();await page.getByText('Native system <script>window.injected=true</script> context.',{exact:true}).waitFor();await h.idle()
  assert.equal(await page.evaluate(()=>window.injected),undefined)
  const count=()=>h.reads.filter(read=>read.path.endsWith('/transcript')).length,readCount=count()
  await change(page,'Activity')
  assert.equal(await system.getAttribute('open'),'')
  assert.equal(await page.getByText('Quoted developer instructions are ordinary user prose.',{exact:true}).count(),0)
  const marker=page.locator('[data-mode-key^="message:"]').first();await marker.locator('summary').click();await page.getByText('Quoted developer instructions are ordinary user prose.',{exact:true}).waitFor()
  assert.match(await page.locator('.session-activity-group').first().locator(':scope > summary').innerText(),/Time unavailable.*Activity · 2 events/s)
  await change(page,'Full transcript')
  assert.equal(await page.locator('.session-activity-group').count(),0)
  assert.equal(await page.locator('.session-content-tool').count(),1)
  await page.locator('.session-content-tool summary').click();assert.match(await page.locator('.session-content-tool pre').innerText(),/"path"/)
  assert.equal(await page.getByText('never-render-ciphertext',{exact:false}).count(),0)
  await page.locator('.session-content-event summary').click();await page.locator('.session-content-event pre').waitFor();assert.match(await page.locator('.session-content-event').innerText(),/future_event/)
  await change(page,'Activity');assert.equal(await marker.getAttribute('open'),'');await change(page,'Conversation')
  assert.equal(await system.getAttribute('open'),'');assert.equal(count(),readCount)
  assert.equal(await page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible(),false)
  await change(page,'Activity');await page.locator('.session-summary a[href*="session='+h.activeSessions[1].id+'"]').click();await until(()=>page.locator('[data-mode-key^="context:"]').first().getAttribute('open').then(value=>value===null))
  assert.equal(await page.getByRole('tab',{name:'Conversation',exact:true}).getAttribute('aria-selected'),'true');assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('embedded presentation tabs support keyboard selection and same-authority workspace transfer',{timeout:60000},async()=>{
 const h=await openSessionExplorer(),{page}=h
 try{
  await h.start();await h.idle();const card=page.locator('#entity-'+h.fixture.tasks[0].id),agent=card.locator(':scope > .card-header .task-sessions-icon');await until(()=>agent.isEnabled());await agent.click();await page.getByRole('tab',{name:'Conversation',exact:true}).waitFor();await h.idle()
  await page.getByRole('tab',{name:'Conversation',exact:true}).focus();await page.keyboard.press('ArrowRight');await until(()=>page.getByRole('tab',{name:'Activity',exact:true}).getAttribute('aria-selected').then(value=>value==='true'))
  await page.keyboard.press('End');await until(()=>page.getByRole('tab',{name:'Full transcript',exact:true}).getAttribute('aria-selected').then(value=>value==='true'))
  await change(page,'Activity');await page.getByRole('link',{name:'Jump To Session ↗',exact:true}).click();await h.idle()
  assert.equal(await page.getByRole('tab',{name:'Activity',exact:true}).getAttribute('aria-selected'),'true')
  await page.getByRole('button',{name:'Projects',exact:true}).click();await h.idle();assert.equal(await page.getByRole('tab',{name:'Activity',exact:true}).getAttribute('aria-selected'),'true')
  assert.equal(await page.locator('#session-records').count(),1);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('presentation disclosures survive appends and the same-authority active to verified transition',{timeout:60000},async()=>{
 const raw=Buffer.from([message('system','Original native context.'),message('assistant','Original prose occurrence.')].map(JSON.stringify).join('\n')+'\n')
 const h=await openSessionExplorer({transcriptBytes:raw}),{page}=h
 try{
  await h.start();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.active.id);await page.getByText('Original prose occurrence.',{exact:true}).waitFor();await change(page,'Activity')
  for(const marker of await page.locator('[data-mode-key]').all())await marker.locator(':scope > summary').click()
  await page.getByText('Original native context.',{exact:true}).waitFor();await page.getByText('Original prose occurrence.',{exact:true}).waitFor();await h.idle()
  const keys=await page.locator('[data-mode-key]').evaluateAll(nodes=>nodes.map(node=>node.dataset.modeKey))
  await h.appendRecords(message('assistant','Appended prose occurrence.'));await until(()=>page.locator('[data-mode-key]').count().then(count=>count===3));assert.equal(await page.getByText('Original prose occurrence.',{exact:true}).isVisible(),true)
  const bytes=h.content.get(h.active.id);Object.assign(h.active,{state:'terminated',terminated_at:new Date().toISOString(),final_bytes:bytes.length,final_sha256:createHash('sha256').update(bytes).digest('hex')});await archive(h,h.active)
  await page.evaluate(({workspace,id})=>window.pushHierarchyFrames([{schema_version:1,type:'change',event:{schema_version:1,event_id:'presentation-termination',scope:'workspace',workspace_id:workspace,occurred_at:new Date().toISOString(),transaction:{id:'presentation-termination',cause:'rest',request_id:null},actor:{type:'system',id:null},entity:{type:'agent_session',id,action:'updated'},invalidations:[{kind:'entity',target:'agent_session:'+id},{kind:'collection',target:'sessions:'+workspace}]}}]),{workspace:h.fixture.workspace.id,id:h.active.id})
  await page.getByText('Verified complete archive.',{exact:true}).waitFor();await page.getByText('Original prose occurrence.',{exact:true}).waitFor();await page.getByText('Original native context.',{exact:true}).waitFor()
  assert.equal(await page.getByRole('tab',{name:'Activity',exact:true}).getAttribute('aria-selected'),'true')
  assert.equal(await page.locator('[data-mode-key]').evaluateAll((nodes,keys)=>keys.every(key=>nodes.find(node=>node.dataset.modeKey===key)?.open),keys),true);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

for(const saved of [false,true])test(`${saved?'verified':'active'} mode changes preserve the reading occurrence and do not request history or reveal Jump to latest`,{timeout:60000},async()=>{
 const bytes=Buffer.from(Array.from({length:85},(_,i)=>message(i%2?'assistant':'user',`Occurrence ${i}: `+'readable words '.repeat(15))).map(JSON.stringify).join('\n')+'\n')
 const h=await openSessionExplorer({transcriptBytes:bytes});const{page}=h
 try{
  if(saved)await archive(h)
  await h.start();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+(saved?h.saved.id:h.active.id));await page.getByText('Occurrence 84:',{exact:false}).waitFor();await h.idle()
  const viewport=page.locator('#session-records');await viewport.evaluate(node=>{node.scrollTop=Math.max(0,node.scrollTop-270)})
  await h.idle()
  const source=await viewport.evaluate(node=>{const box=node.getBoundingClientRect(),row=[...node.querySelectorAll('[data-block-key]')].find(row=>{const r=row.getBoundingClientRect();return r.bottom>box.top+5&&r.top<box.bottom});return row.dataset.blockKey})
  const before=h.reads.filter(read=>/\/(transcript|evidence|archive)$/.test(read.path)).length
  for(const mode of ['Activity','Full transcript','Conversation']){
   await change(page,mode)
   const position=await viewport.evaluate((node,key)=>{const row=[...node.querySelectorAll('[data-block-key]')].find(row=>row.dataset.blockKey===key),box=node.getBoundingClientRect(),r=row?.getBoundingClientRect();return !!r&&r.bottom>box.top&&r.top<box.bottom},source)
   assert.equal(position,true,`${mode} retains the same source occurrence`)
   assert.equal(await page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible(),false)
  }
  assert.equal(h.reads.filter(read=>/\/(transcript|evidence|archive)$/.test(read.path)).length,before)
  assert.ok(await viewport.locator('[data-block-key]').count()<=200);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

for(const saved of [false,true])test(`${saved?'verified':'active'} closed activity groups retain their occurrence across Full transcript and Conversation`,{timeout:60000},async()=>{
 const records=[...Array.from({length:45},(_,i)=>message('assistant',`Before group ${i}: `+'preceding prose '.repeat(12))),
  {type:'response_item',payload:{type:'function_call',name:'closed_group_tool',arguments:'{"original":true}'}},
  {type:'response_item',payload:{type:'function_call_output',call_id:'closed_group',output:'{"result":42}'}},
  ...Array.from({length:5},(_,i)=>message('assistant',`After group ${i}: `+'following prose '.repeat(12)))]
 const h=await openSessionExplorer({transcriptBytes:Buffer.from(records.map(JSON.stringify).join('\n')+'\n')}),{page}=h
 try{
  if(saved)await archive(h)
  await h.start();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+(saved?h.saved.id:h.active.id));await page.getByText('After group 4:',{exact:false}).waitFor();await h.idle()
  const viewport=page.locator('#session-records'),group=page.locator('.session-activity-group').first(),key=await group.getAttribute('data-group-key')
  await viewport.evaluate((node,key)=>{const group=[...node.querySelectorAll('[data-group-key]')].find(group=>group.dataset.groupKey===key);node.scrollTop+=group.getBoundingClientRect().top-node.getBoundingClientRect().top},key);await h.idle()
  const initial=await group.evaluate(node=>node.getBoundingClientRect().top-document.getElementById('session-records').getBoundingClientRect().top)
  const before=h.reads.filter(read=>/\/(transcript|evidence|archive)$/.test(read.path)).length
  const authority=await viewport.evaluate(node=>[node.dataset.viewerGeneration,node.dataset.sealedDomLoad,node.dataset.windowPublication,node.dataset.sealedPageRequest])
  assert.equal(await group.getAttribute('open'),null);assert.ok(initial>=-1)
  await change(page,'Full transcript')
  const position=await viewport.evaluate((node,key)=>{const row=[...node.querySelectorAll('[data-block-key]')].find(row=>row.dataset.sourceGroup===key),box=node.getBoundingClientRect(),r=row?.getBoundingClientRect();return {visible:!!r&&r.bottom>box.top&&r.top<box.bottom,top:r?r.top-box.top:null}},key)
  assert.equal(position.visible,true,'the original closed group occurrence must remain in the reader, not preceding prose')
  assert.ok(Math.abs(position.top-initial)<=32,JSON.stringify({initial,position}))
  await change(page,'Conversation')
  assert.equal(await group.getAttribute('data-group-key'),key);assert.equal(await group.getAttribute('open'),null)
  assert.ok(Math.abs(await group.evaluate(node=>node.getBoundingClientRect().top-document.getElementById('session-records').getBoundingClientRect().top)-initial)<=32)
  assert.equal(h.reads.filter(read=>/\/(transcript|evidence|archive)$/.test(read.path)).length,before)
  assert.deepEqual(await viewport.evaluate(node=>[node.dataset.viewerGeneration,node.dataset.sealedDomLoad,node.dataset.windowPublication,node.dataset.sealedPageRequest]),authority)
  assert.equal(await page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible(),false);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})
