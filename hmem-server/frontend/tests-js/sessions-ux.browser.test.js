import assert from 'node:assert/strict'
import test from 'node:test'
import { readFile, mkdtemp, rm } from 'node:fs/promises'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { openSessionExplorer } from './support/sessions-ux-fixture.mjs'
const until=async(fn)=>{const end=Date.now()+10000;while(Date.now()<end){if(await fn())return;await new Promise(r=>setTimeout(r,25))}throw new Error('Explorer did not converge')}

test('active filters, linked task focus, encrypted thinking and local-day timestamps form a compact transcript',{timeout:45000},async()=>{
 const h=await openSessionExplorer({activeCount:2,linkedCount:3,encrypted:true,timezoneId:'Europe/Copenhagen',eventTimes:['2026-10-24T22:01:00Z','2026-10-24T22:01:00Z','2026-10-24T21:59:00Z']})
 const {page}=h
 try{
  await page.clock.setFixedTime(new Date('2026-10-24T22:05:00Z'))
  await h.start();await h.idle();await page.getByRole('button',{name:'Sessions',exact:true}).click()
  await page.locator('.session-summary').waitFor()
  assert.equal(await page.getByRole('button',{name:'Live',exact:true}).getAttribute('aria-pressed'),'true')
  assert.equal(await page.locator('.session-summary').count(),1)
  assert.ok(h.reads.some(read=>read.query.includes('state=active')&&read.query.includes('freshness=fresh')))
  await page.getByRole('button',{name:'Quiet',exact:true}).click();await until(async()=>await page.locator('.session-summary a').filter({hasText:'Follow-up'}).count()===1)
  assert.equal(await page.locator('.session-summary').count(),1)
  await page.getByRole('button',{name:'Live',exact:true}).click();await page.locator('.session-summary a').filter({hasText:'Session explorer'}).click()
  await page.getByRole('heading',{name:'A calmer session explorer'}).waitFor()
  assert.equal(await page.locator('.session-task-card').count(),2)
  const positions=await page.locator('.session-task-card').evaluateAll(elements=>elements.map(el=>el.getBoundingClientRect().top))
  assert.equal(positions[0],positions[1],'first linked task cards share a row')
  await page.getByRole('button',{name:'Show All Linked Tasks',exact:true}).click();await until(async()=>await page.locator('.session-task-card').count()===3)
  assert.equal(await page.locator('.session-thinking').count(),2)
  assert.equal(await page.locator('#session-records').getByText(/ciphertext|Unrecognized record|Window begins|Following|Original JSON/).count(),0)
  for(const name of ['Pause','Older messages','Refresh'])assert.equal(await page.getByRole('button',{name,exact:true}).count(),0)
  await until(async()=>await page.locator('.session-event-time').first().innerText()==='00:01')
  assert.equal(await page.locator('.session-event-time').nth(1).innerText(),'2026-10-24 23:59')
  const identityText=await page.locator('.session-viewer .session-identity').innerText()
  assert.ok(identityText.includes('codex:gpt-6.1-sol:high'),identityText)
  const latest=page.locator('.session-latest-activity time')
  assert.equal(await latest.getAttribute('datetime'),h.active.last_seen_at)
  assert.notEqual(h.active.created_at,h.active.last_seen_at)
  const result=page.locator('.session-content-result').first();await result.locator('xpath=ancestor::details[contains(@class,"session-activity-group")]').locator(':scope > summary').click();await result.locator('summary').click()
  assert.ok(await result.locator('.json-key').isVisible())
  assert.deepEqual(JSON.parse(await result.locator('pre').textContent()),{output:'module Feature.Sessions exposing (view)'})
  const assistant=page.locator('.session-role-assistant:not(.session-record-activity)').first()
  assert.ok(await assistant.evaluate(el=>el.getBoundingClientRect().width>el.closest('.session-detail').clientWidth*.85),'transcript uses its panel width')
  await page.locator('.session-task-card').nth(2).click();await until(()=>page.url().includes('focus=task%3A'+h.fixture.tasks[2].id)||page.url().includes('focus=task:'+h.fixture.tasks[2].id))
  await page.locator('#entity-'+h.fixture.tasks[2].id).waitFor()
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('appended live transcript chunks preserve readers above the tail and follow readers at the bottom',{timeout:45000},async()=>{
 const h=await openSessionExplorer({extraRecords:[{text:Array.from({length:60},(_,i)=>'Paragraph '+i+' keeps the transcript scrollable.').join('\n\n')}]})
 const {page}=h
 try{
  await h.start();await h.idle();await page.getByRole('button',{name:'Sessions',exact:true}).click();await page.locator('.session-summary a').click()
  const viewport=page.locator('#session-records')
  await until(async()=>await viewport.evaluate(el=>el.scrollHeight>el.clientHeight+300))
  await until(async()=>await viewport.evaluate(el=>el.scrollHeight-el.clientHeight-el.scrollTop<=48))
  await viewport.evaluate(el=>{el.scrollTop=40})
  await h.appendRecords({text:'Arrived while reading older messages.'})
  await page.getByText('Arrived while reading older messages.',{exact:true}).waitFor({state:'attached'})
  assert.ok(await viewport.evaluate(el=>el.scrollTop<=44),'a reader above the tail keeps the current position')
  await viewport.evaluate(el=>{el.scrollTop=el.scrollHeight})
  await h.appendRecords({text:'Arrived at the live tail.'})
  await page.getByText('Arrived at the live tail.',{exact:true}).waitFor({state:'attached'})
  await until(async()=>await viewport.evaluate(el=>el.scrollHeight-el.clientHeight-el.scrollTop<=1))
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('Sessions tab, task filters, native conversations and direct evidence downloads', {timeout:60000},async()=>{
 const scratch=await mkdtemp(join(tmpdir(),'sessions-ux-download-'))
 const h=await openSessionExplorer();const {page}=h
 try{
  await h.start();await h.idle()
  await page.getByRole('button',{name:'Sessions',exact:true}).click()
  await page.getByRole('heading',{name:'Sessions',exact:true}).waitFor()
  assert.match(page.url(),/tab=sessions/)
  await page.locator('.session-summary a').filter({hasText:'Codex'}).click()
  await page.getByRole('heading',{name:'A calmer session explorer'}).waitFor()
  assert.equal(await page.locator('.session-record').count(),8,'duplicate Codex envelope and unknown record omitted')
  assert.equal(await page.getByText('Private reasoning stays collapsed.',{exact:true}).isVisible(),false)
  assert.equal(await page.getByText('Claude thinking is hidden too.',{exact:true}).isVisible(),false)
  assert.equal(await page.locator('.session-content-tool').first().getAttribute('open'),null)
  await page.locator('.session-activity-group').first().locator(':scope > summary').click()
  await page.locator('.session-content-tool').first().locator('summary').click()
  assert.deepEqual(JSON.parse(await page.locator('.session-content-tool pre').first().textContent()),{path:'src/Feature/Sessions.elm'})
  assert.equal(await page.locator('.session-content-tool .json-key').first().isVisible(),true)
  assert.equal(await page.locator('.session-task-card').count(),2)
  assert.equal(await page.locator('.session-record a[href^="javascript:"]').count(),0)
  assert.equal(await page.locator('.session-original,.session-raw-details').count(),0)
  if(process.env.SESSIONS_UX_SCREENSHOTS){await page.screenshot({path:process.env.SESSIONS_UX_SCREENSHOTS+'/after-sessions.png',fullPage:true});await page.setViewportSize({width:430,height:900});await page.locator('.session-detail').scrollIntoViewIfNeeded();await page.screenshot({path:process.env.SESSIONS_UX_SCREENSHOTS+'/after-sessions-narrow.png',fullPage:true});assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=window.innerWidth));await page.setViewportSize({width:1440,height:1000})}
  await page.getByRole('button',{name:'Projects',exact:true}).click();await h.idle()
  const card=page.locator('#entity-'+h.fixture.tasks[0].id)
  await until(async()=>await card.locator('.has-active-sessions').count()===1)
  await card.locator('.btn-extras-toggle').click();await h.idle()
  const active=card.locator('.task-session-active'),evidence=card.locator('.task-session-evidence')
  await active.locator('summary').click();await evidence.locator('summary').click()
  await active.getByRole('link',{name:/Codex/}).waitFor()
  assert.equal(await card.evaluate(el=>{const children=[...el.querySelector('.card-extras').children];return children.indexOf(el.querySelector('.task-session-active'))<children.indexOf(el.querySelector('.task-dependencies-section'))&&children.indexOf(el.querySelector('.task-session-evidence'))>children.indexOf(el.querySelector('.task-dependencies-section'))}),true)
  const downloadPromise=page.waitForEvent('download');await evidence.getByRole('button',{name:/Download session/}).click();const download=await downloadPromise
  const target=join(scratch,'session.jsonl');await download.saveAs(target);assert.deepEqual(await readFile(target),h.bytes)
  await page.evaluate(()=>Object.defineProperty(navigator.storage,'getDirectory',{configurable:true,value:async()=>{throw new Error('storage denied')}}))
  await evidence.getByRole('button',{name:/Download session/}).waitFor({state:'visible'})
  await until(async()=>await evidence.getByRole('button',{name:/Download session/}).isEnabled())
  const fallbackEvent=page.waitForEvent('download');await evidence.getByRole('button',{name:/Download session/}).click()
  const fallback=await fallbackEvent;const fallbackPath=join(scratch,'fallback.jsonl');await fallback.saveAs(fallbackPath);assert.deepEqual(await readFile(fallbackPath),h.bytes)
  if(process.env.SESSIONS_UX_SCREENSHOTS)await page.screenshot({path:process.env.SESSIONS_UX_SCREENSHOTS+'/after-task.png',fullPage:true})
  await card.locator('.task-sessions-icon').click();await page.locator('.sessions-task-filter').waitFor()
  assert.match(page.url(),new RegExp('task='+h.fixture.tasks[0].id))
  await page.locator('.session-summary a').filter({hasText:'Codex'}).click();await page.locator('.session-record').first().waitFor()
  assert.match(page.url(),new RegExp('task='+h.fixture.tasks[0].id))
  await page.getByRole('link',{name:'Close session'}).click();await page.locator('.sessions-task-filter').waitFor();await until(async()=>await page.locator('.session-record').count()===0)
  await page.goBack();await page.locator('.session-record').first().waitFor()
  assert.ok(h.peak()<=2,'session HTTP admission stays at two')
  await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=projects');await h.idle()
  const second=page.locator('#entity-'+h.fixture.tasks[1].id)
  await second.locator('.task-sessions-icon').click();await until(()=>page.url().includes('task='+h.fixture.tasks[1].id));await page.locator('.sessions-task-filter').waitFor()
  assert.ok(h.reads.some(read=>read.path.includes('/tasks/'+h.fixture.tasks[1].id+'/sessions')))
  assert.deepEqual(h.errors,[])
 }finally{await h.close();await rm(scratch,{recursive:true,force:true})}
})

test('active hints converge beyond sixteen mounted tasks and refresh after an association', {timeout:60000},async()=>{
 const h=await openSessionExplorer({many:true});try{
  await h.page.setViewportSize({width:1440,height:10000});await h.start();await h.idle()
  const last=h.page.locator('#entity-'+h.fixture.tasks.at(-1).id)
  await last.waitFor();await until(async()=>await last.locator('.has-active-sessions').count()===1)
  assert.ok(h.reads.filter(r=>r.query.includes('state=active')).length>16)
  const first=h.page.locator('#entity-'+h.fixture.tasks[0].id)
  await until(async()=>/no active sessions/.test(await first.locator('.task-sessions-icon').getAttribute('aria-label')))
  await h.associate(h.fixture.tasks[0].id)
  await until(async()=>await first.locator('.has-active-sessions').count()===1)
  for(const task of h.fixture.tasks)await h.page.locator('#entity-'+task.id+' .btn-extras-toggle').click()
  await until(()=>new Set(h.reads.filter(read=>read.query.includes('state=terminated')).map(read=>read.path)).size===22)
  await until(async()=>await last.locator('.task-session-evidence summary').innerText()==='Evidence · 1')
  assert.ok(h.peak()<=2)
 }finally{await h.close()}
})


test('card metadata download failures abort staging and permit retry', {timeout:60000},async()=>{
 const h=await openSessionExplorer();const {page}=h
 try{
  await h.start();await h.idle()
  const card=page.locator('#entity-'+h.fixture.tasks[0].id)
  await card.locator('.btn-extras-toggle').click();await h.idle();await card.locator('.task-session-evidence summary').click()
  const button=card.getByRole('button',{name:/Download session/})
  const metadataPattern='**/sessions/'+h.saved.id
  await page.route(metadataPattern,route=>route.fulfill({status:503,json:{error:'controlled metadata failure'}}))
  await button.click();await card.getByText('Download could not finish. Please retry.',{exact:true}).waitFor()
  await until(async()=>await button.isEnabled())
  await page.waitForFunction(async()=>{try{const root=await (await navigator.storage.getDirectory()).getDirectoryHandle('hmem-downloads');for await(const key of root.keys())if(key.startsWith('hmem-download-'))return false;return true}catch{return true}})
  await page.unroute(metadataPattern)
  const retryEvent=page.waitForEvent('download');await button.click();const retry=await retryEvent
  const parts=[];for await(const part of await retry.createReadStream())parts.push(part)
  assert.deepEqual(Buffer.concat(parts),h.bytes)
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('zero-byte evidence completes when writer acquisition outlives card collapse', {timeout:60000},async()=>{
 const scratch=await mkdtemp(join(tmpdir(),'sessions-ux-empty-'))
 const h=await openSessionExplorer({empty:true});const {page}=h
 try{
  await h.start();await h.idle()
  const card=page.locator('#entity-'+h.fixture.tasks[0].id)
  await card.locator('.btn-extras-toggle').click();await h.idle();await card.locator('.task-session-evidence summary').click()
  const button=card.getByRole('button',{name:/Download session/})
  await page.evaluate(()=>{const original=navigator.storage.getDirectory.bind(navigator.storage);Object.defineProperty(navigator.storage,'getDirectory',{configurable:true,value:()=>new Promise(resolve=>{window.releaseEmptyDownload=async()=>resolve(await original())})})})
  await button.click();await page.waitForFunction(()=>typeof window.releaseEmptyDownload==='function')
  await card.locator('.btn-extras-toggle').click();await until(async()=>await card.locator('.task-session-evidence').count()===0)
  const received=page.waitForEvent('download');await page.evaluate(()=>window.releaseEmptyDownload());const download=await received
  const target=join(scratch,'empty.jsonl');await download.saveAs(target);assert.equal((await readFile(target)).length,0)
  assert.deepEqual(h.errors,[])
 }finally{await h.close();await rm(scratch,{recursive:true,force:true})}
})


test('conversation flow preserves declared identity and wraps long model names without inventing missing metadata', {timeout:60000},async()=>{
 const cases=[{creator:{creator_actor_type:'bot',creator_bot_identity:{role:'Developer','model-id':'declared-harness:actual-model:high'}},width:1440},{creator:{creator_actor_type:'bot',creator_bot_identity:{role:'Reviewer','model-id':'declared-harness:'+ 'long-model-name-'.repeat(12)+':high'}},width:430},{creator:null,width:430}]
 for(const scenario of cases){
  const h=await openSessionExplorer({creator:scenario.creator});const {page}=h
  try{
   await page.setViewportSize({width:scenario.width,height:1000});await h.start();await h.idle()
   await page.getByRole('button',{name:'Sessions',exact:true}).click();await page.getByRole('heading',{name:'Sessions',exact:true}).waitFor()
   const selected=page.locator('.session-summary a').filter({hasText:'Codex'})
   if(scenario.creator){assert.match(await selected.locator('.session-identity').innerText(),new RegExp(scenario.creator.creator_bot_identity.role));assert.ok(await selected.locator('.actor-attribution-kind').evaluate(el=>el.getBoundingClientRect().height<24),'Model badge stays on one line')}
   else assert.match(await selected.locator('.session-connection').innerText(),/Unknown/)
   await selected.click();await page.getByRole('heading',{name:'A calmer session explorer'}).waitFor()
   const identity=page.locator('.session-viewer .session-identity')
   if(scenario.creator){const declared=scenario.creator.creator_bot_identity;assert.ok((await identity.innerText()).includes(declared['model-id']));assert.ok((await identity.innerText()).includes(declared.role));assert.equal(await identity.evaluate(el=>getComputedStyle(el).userSelect),'text')}
   else assert.match(await identity.locator('.session-connection').innerText(),/Unknown/)
   const user=page.locator('.session-role-user:not(.session-record-activity)').first(),assistant=page.locator('.session-role-assistant:not(.session-record-activity)').first()
   assert.notEqual(await user.evaluate(el=>getComputedStyle(el).borderRadius),'0px')
   assert.equal(await assistant.evaluate(el=>getComputedStyle(el).borderTopWidth),'0px')
   assert.equal(await assistant.evaluate(el=>getComputedStyle(el).backgroundColor),'rgba(0, 0, 0, 0)')
   assert.ok(await assistant.locator('.session-record-text').evaluate(el=>parseFloat(getComputedStyle(el).lineHeight)>=20))
   const tool=page.locator('.session-content-tool').first();assert.equal(await tool.getAttribute('open'),null);await page.locator('.session-activity-group').first().locator(':scope > summary').click();await tool.locator('summary').focus();await page.keyboard.press('Enter');assert.notEqual(await tool.getAttribute('open'),null)
   assert.equal(await page.getByText('Private reasoning stays collapsed.',{exact:true}).isVisible(),false)
   assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=window.innerWidth))
   if(scenario.creator&&scenario.width===1440){
    await page.getByRole('button',{name:'Projects',exact:true}).click();await h.idle()
    const taskCard=page.locator('#entity-'+h.fixture.tasks[0].id);await taskCard.locator('.btn-extras-toggle').click();await h.idle()
    await taskCard.locator('.task-session-active summary').click();await taskCard.locator('.task-session-evidence summary').click()
    assert.equal(await taskCard.locator('.task-session-card .session-identity').count(),2)
    for(const label of await taskCard.locator('.task-session-card .session-identity').allTextContents())assert.ok(label.includes(scenario.creator.creator_bot_identity['model-id']))
   }
   assert.deepEqual(h.errors,[])
  }finally{await h.close()}
 }
})


test('housekeeping and unrecognized records stay hidden without original JSON UI', {timeout:30000},async()=>{
 const h=await openSessionExplorer({housekeeping:true});const {page}=h
 try{
  await h.start();await h.idle();await page.getByRole('button',{name:'Sessions',exact:true}).click()
  await page.locator('.session-summary a').filter({hasText:'Codex'}).click();await page.getByRole('heading',{name:'A calmer session explorer'}).waitFor()
  assert.equal(await page.locator('.session-record').count(),8)
  assert.equal(await page.locator('.session-original').count(),0)
  assert.equal(await page.locator('#session-records').getByText(/housekeeping-session|synthetic-workspace|total_tokens/).count(),0)
  assert.equal(await page.locator('.session-raw-details').count(),0)
  assert.equal(await page.locator('#session-records').getByText(/novel_event|preserved/).count(),0)
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})
