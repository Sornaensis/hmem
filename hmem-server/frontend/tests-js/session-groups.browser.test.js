import assert from 'node:assert/strict'
import test from 'node:test'
import { readFile } from 'node:fs/promises'
import { createHash } from 'node:crypto'
import { openSessionExplorer } from './support/sessions-ux-fixture.mjs'

const until=async(fn)=>{const end=Date.now()+12000;while(Date.now()<end){if(await fn())return;await new Promise(resolve=>setTimeout(resolve,25))}throw new Error('Transcript groups did not converge')}
const call=name=>({type:'response_item',payload:{type:'function_call',name,arguments:'{"path":"src/example.elm"}'}})
const result=()=>({type:'response_item',payload:{type:'function_call_output',call_id:'call-1',output:'{"ok":true}'}})
const message=text=>({type:'response_item',payload:{type:'message',role:'assistant',content:[{type:'output_text',text}]}})

test('Codex event JSON stays inert and expanded while the group follows final-entry time',{timeout:60000},async()=>{
 const first='2026-10-30T12:00:00Z',final='2026-10-30T09:00:00Z'
 const event=(timestamp,payload)=>({type:'event_msg',timestamp,payload})
 const h=await openSessionExplorer({extraRecords:[message('Before inspectable events.')]});const {page}=h
 try{
  await h.start();await h.idle();await page.getByRole('button',{name:'Sessions',exact:true}).click();await page.locator('.session-summary a').click();await page.getByText('Before inspectable events.',{exact:true}).waitFor()
  const payload={type:'future_event',message:'<img src=x onerror="window.injected=true">',values:[null,false,7,'λ😀'],large:'x'.repeat(19000)}
  await h.appendRecords(event(first,payload))
  const group=page.locator('.session-activity-group').last();await until(async()=>await group.locator(':scope > summary time').getAttribute('datetime')===first)
  assert.equal(await group.getAttribute('open'),null);await group.locator(':scope > summary').click()
  const disclosure=group.locator('.session-content-event').first();assert.equal(await disclosure.getAttribute('open'),null);await disclosure.locator('summary').click()
  const json=disclosure.locator('session-event-json');await json.locator('pre').waitFor({timeout:3000});const begin=await json.locator('pre').textContent()
  await json.getByRole('button',{name:'Continue JSON',exact:true}).click();const end=await json.locator('pre').textContent()
  assert.deepEqual(JSON.parse(begin+end),payload);assert.equal(await page.locator('#session-records img').count(),0);assert.equal(await page.evaluate(()=>window.injected),undefined)
  await json.evaluate(el=>window.eventPayloadNode=el)
  await h.appendRecords(event(final,{type:'older_final_event',result:42}))
  await until(async()=>await group.locator(':scope > summary time').getAttribute('datetime')===final)
  assert.equal(await group.getAttribute('open'),'');assert.equal(await disclosure.getAttribute('open'),'')
  assert.equal(await json.evaluate(el=>el===window.eventPayloadNode),true);assert.equal(await json.locator('pre').textContent(),end)
  await h.appendRecords(event(undefined,{type:'undated_final_event',result:true}))
  await group.locator(':scope > summary').getByText('Time unavailable',{exact:true}).waitFor();assert.equal(await group.locator(':scope > summary time').count(),0)
  await h.appendRecords(message('After inspectable events.'));await page.getByText('After inspectable events.',{exact:true}).waitFor()
  assert.equal(await page.getByText(/Unsupported transcript content \(event_msg\)/).count(),0)
  for(const width of [1440,430]){await page.setViewportSize({width,height:1000});assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=window.innerWidth))}
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('mixed native blocks collect activity without hiding or reordering prose',{timeout:60000},async()=>{
 const h=await openSessionExplorer({extraRecords:[{type:'assistant',message:{role:'assistant',content:[
  {type:'text',text:'Visible before activity.'},{type:'thinking',thinking:'Grouped reasoning.'},{type:'tool_use',name:'Search',input:{query:'safe example'}},
  {type:'text',text:''},{type:'unknown',text:'hidden attachment'},{type:'text',text:'Visible between activity.'},
  {type:'tool_result',content:'{"ok":true}'},{type:'redacted_thinking',data:'ciphertext-never-rendered'},{type:'text',text:'Visible after activity.'}
 ]}}]});const {page}=h
 try{
  await h.start();await h.idle();await page.getByRole('button',{name:'Sessions',exact:true}).click();await page.locator('.session-summary a').click()
  await page.getByText('Visible after activity.',{exact:true}).waitFor()
  const groups=page.locator('.session-activity-group');assert.equal(await groups.count(),4)
  assert.equal(await groups.locator('.session-record:not(.session-record-activity)').count(),0,'messages never enter activity disclosures')
  assert.equal(await groups.evaluateAll(elements=>elements.every(el=>!el.open)),true)
  const order=await page.locator('#session-records').evaluate(el=>[...el.children].slice(-5).flatMap(child=>child.matches('details')?[child.querySelector('summary span').textContent]:[...child.querySelectorAll('.session-record-text')].map(node=>node.textContent)))
  assert.deepEqual(order,['Visible before activity.','Activity · 2 events','Unsupported transcript content (unknown). Original evidence is preserved.','Visible between activity.','Activity · 2 events','Visible after activity.'])
  for(const text of ['Visible before activity.','Visible between activity.','Visible after activity.'])assert.equal(await page.getByText(text,{exact:true}).isVisible(),true)
  const mixed=groups.nth(2);await mixed.locator(':scope > summary').focus();await page.keyboard.press('Enter');assert.equal(await mixed.getAttribute('open'),'')
  const tool=mixed.locator('.session-content-tool');assert.equal(await tool.getAttribute('open'),null);await tool.locator('summary').click()
  assert.deepEqual(JSON.parse(await tool.locator('pre').textContent()),{query:'safe example'});assert.ok(await tool.locator('.json-key').isVisible())
  await groups.nth(3).locator(':scope > summary').click();assert.equal(await groups.nth(3).locator('.session-thinking').innerText(),'Thinking...')
  assert.equal(await page.locator('#session-records').getByText(/ciphertext-never-rendered|hidden attachment/).count(),0)
  await mixed.locator(':scope > summary').click();assert.equal(await page.getByText('Visible between activity.',{exact:true}).isVisible(),true)
  for(const width of [1440,430]){await page.setViewportSize({width,height:1000});assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=window.innerWidth))}
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('a live activity run retains group and nested disclosure state across append and message boundaries',{timeout:60000},async()=>{
 const h=await openSessionExplorer({activeCount:2,extraRecords:[message('Before the live run.')]});const {page}=h
 h.activeSessions[1].last_seen_at=new Date().toISOString()
 try{
  await h.start();await h.idle();await page.getByRole('button',{name:'Sessions',exact:true}).click();await page.locator('.session-summary a').first().click();await page.getByText('Before the live run.',{exact:true}).waitFor()
  await h.appendRecords({type:'response_item',payload:{type:'reasoning',summary:[{text:'Live reasoning.'}]}})
  let group=page.locator('.session-activity-group').last();await until(async()=>/1 event$/.test(await group.locator(':scope > summary').innerText()))
  const key=await group.getAttribute('data-group-key');await group.evaluate(el=>window.liveGroup=el);await group.locator(':scope > summary').click();await group.locator('.session-content-thinking summary').click()
  await h.appendRecords(call('read_file'));await until(async()=>/2 events$/.test(await group.locator(':scope > summary').innerText()))
  assert.equal(await group.evaluate(el=>el===window.liveGroup&&el.open),true);assert.equal(await group.locator('.session-content-thinking').getAttribute('open'),'')
  const tool=group.locator('.session-content-tool');await tool.locator('summary').click();await tool.evaluate(el=>window.liveTool=el)
  await h.appendRecords(result());await until(async()=>/3 events$/.test(await group.locator(':scope > summary').innerText()))
  assert.equal(await tool.evaluate(el=>el===window.liveTool&&el.open),true)
  await h.appendRecords(message('After the live run.'));await page.getByText('After the live run.',{exact:true}).waitFor()
  assert.equal(await group.getAttribute('data-group-key'),key);assert.equal(await group.evaluate(el=>el===window.liveGroup&&el.open),true)
  await h.appendRecords(call('next_tool'));await until(async()=>await page.locator('.session-activity-group').count()===4)
  const previous=page.locator('.session-activity-group').nth(2),next=page.locator('.session-activity-group').last()
  assert.equal(await previous.getAttribute('open'),'');assert.equal(await next.getAttribute('open'),null)
  await previous.locator(':scope > summary').click();await h.appendRecords(result());await until(async()=>/2 events$/.test(await next.locator(':scope > summary').innerText()))
  assert.equal(await previous.getAttribute('open'),null);assert.equal(await next.getAttribute('open'),null)
  await next.locator(':scope > summary').click();await h.appendRecords({type:'mystery'},{type:'response_item',payload:{type:'reasoning',summary:[]}})
  await until(()=>h.reads.some(read=>read.path.endsWith('/transcript')));assert.equal(await next.getAttribute('open'),'')
  await page.locator('.session-summary a[href*="session='+h.activeSessions[1].id+'"]').click();await until(()=>page.url().includes('session='+h.activeSessions[1].id))
  await until(async()=>await page.locator('.session-activity-group').count()===2)
  assert.equal(await page.locator('.session-activity-group').evaluateAll(elements=>elements.every(el=>!el.open)),true,'session contexts do not inherit disclosure state')
  assert.ok(h.peak()<=2);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('a paired native reasoning envelope retains its group and inner disclosure as the canonical record arrives',{timeout:60000},async()=>{
 const h=await openSessionExplorer({extraRecords:[message('Before paired thinking.')]});const {page}=h
 try{
  await h.start();await h.idle();await page.getByRole('button',{name:'Sessions',exact:true}).click();await page.locator('.session-summary a').click();await page.getByText('Before paired thinking.',{exact:true}).waitFor()
  const timestamp=new Date().toISOString(),event={type:'event_msg',timestamp,payload:{type:'agent_reasoning',message:'The same reasoning occurrence.'}}
  await h.appendRecords(event)
  const group=page.locator('.session-activity-group').last();await until(async()=>/1 event$/.test(await group.locator(':scope > summary').innerText()))
  const key=await group.getAttribute('data-group-key');await group.evaluate(el=>window.pairedGroup=el);await group.locator(':scope > summary').click()
  const thinking=group.locator('.session-content-thinking');await thinking.locator('summary').click();await thinking.evaluate(el=>window.pairedThinking=el)
  const originalOffset=await group.locator('article').getAttribute('data-offset')
  await h.appendRecords({type:'response_item',timestamp,payload:{type:'reasoning',summary:[{type:'summary_text',text:event.payload.message}]}})
  await until(async()=>await group.locator('article').getAttribute('data-offset')!==originalOffset)
  assert.equal(await group.getAttribute('data-group-key'),key);assert.equal(await group.evaluate(el=>el===window.pairedGroup&&el.open),true)
  assert.equal(await thinking.evaluate(el=>el===window.pairedThinking&&el.open),true)
  assert.equal(await group.locator('.session-content-thinking').count(),1)
  await h.appendRecords(event,{type:'response_item',timestamp,payload:{type:'reasoning',summary:[{text:event.payload.message}]}})
  await until(async()=>/2 events$/.test(await group.locator(':scope > summary').innerText()))
  assert.equal(await group.locator('.session-content-thinking').count(),2,'identical later occurrences are distinct')
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('a same-read pair keeps open disclosure state when that read also evicts the hidden envelope',{timeout:60000},async()=>{
 const h=await openSessionExplorer({extraRecords:[message('Before the batch boundary.')]});const {page}=h
 try{
  await h.start();await h.idle();await page.getByRole('button',{name:'Sessions',exact:true}).click();await page.locator('.session-summary a').click();await page.getByText('Before the batch boundary.',{exact:true}).waitFor()
  const timestamp=new Date().toISOString(),text='Reasoning at the batch boundary.'
  await h.appendRecords({type:'event_msg',timestamp,payload:{type:'agent_reasoning',message:text}})
  const group=page.locator('.session-activity-group').last();await until(async()=>/1 event$/.test(await group.locator(':scope > summary').innerText()))
  await group.evaluate(el=>window.batchGroup=el);await group.locator(':scope > summary').click()
  const thinking=group.locator('.session-content-thinking');await thinking.locator('summary').click();await thinking.evaluate(el=>window.batchThinking=el)
  const originalOffset=await group.locator('article').getAttribute('data-offset')
  await h.appendRecords({type:'response_item',timestamp,payload:{type:'reasoning',summary:[{text}]}},...Array.from({length:99},()=>({type:'mystery'})))
  await until(async()=>await group.locator('article').getAttribute('data-offset')!==originalOffset)
  assert.equal(await group.evaluate(el=>el===window.batchGroup&&el.open),true)
  assert.equal(await thinking.evaluate(el=>el===window.batchThinking&&el.open),true)
  assert.equal(await group.locator('.session-content-thinking').count(),1)
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('bounded proven native JSONL replays as collapsed activity and visible messages',{timeout:60000,skip:!process.env.SESSION_GROUPS_NATIVE_PATH},async()=>{
 const bytes=await readFile(process.env.SESSION_GROUPS_NATIVE_PATH)
 assert.ok(bytes.length>0&&bytes.length<=131072)
 assert.equal(createHash('sha256').update(bytes).digest('hex'),process.env.SESSION_GROUPS_NATIVE_SHA256)
 const h=await openSessionExplorer({transcriptBytes:bytes});const {page}=h
 try{
  await h.start();await h.idle();await page.getByRole('button',{name:'Sessions',exact:true}).click();await page.locator('.session-summary a').click()
  await page.locator('.session-activity-group').first().waitFor()
  assert.ok(await page.locator('.session-record:not(.session-record-activity)').count()>0)
  assert.equal(await page.locator('.session-activity-group').evaluateAll(elements=>elements.every(el=>!el.open)),true)
  assert.equal(await page.locator('.session-activity-group .session-record:not(.session-record-activity)').count(),0)
  assert.deepEqual(h.content.get(h.active.id),bytes)
  for(const width of [1440,430]){await page.setViewportSize({width,height:1000});assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=window.innerWidth))}
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})
