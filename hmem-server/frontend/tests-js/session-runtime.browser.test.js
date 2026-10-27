import assert from 'node:assert/strict'
import test from 'node:test'
import { openSessionExplorer } from './support/sessions-ux-fixture.mjs'
const until=async fn=>{const end=Date.now()+12000;while(Date.now()<end){if(await fn())return;await new Promise(r=>setTimeout(r,25))}throw Error('Runtime role did not converge')}
async function runtimeChanged(h,identity){h.active.runtime_identity=identity;h.active.runtime_revision++;await h.page.evaluate(({workspace,id,revision})=>window.pushHierarchyFrames([{schema_version:1,type:'change',event:{schema_version:1,event_id:'runtime-'+revision,scope:'workspace',workspace_id:workspace,occurred_at:new Date().toISOString(),transaction:{id:'runtime-'+revision,cause:'rest',request_id:null},actor:{type:'service',id:'updater'},entity:{type:'agent_session',id,action:'updated'},invalidations:[{kind:'entity',target:'agent_session:'+id},{kind:'collection',target:'sessions:'+workspace}]}}]),{workspace:h.fixture.workspace.id,id:h.active.id,revision:h.active.runtime_revision})}
test('actual runtime role switches across shared cards and transcript while connection attribution stays hidden',{timeout:60000},async()=>{
 const creator={creator_actor_type:'bot',creator_bot_identity:{role:'Connector','model-id':'codex:connection-only:high'}},h=await openSessionExplorer({creator,runtime:{model:'Actual A',role:'Developer'}}),{page}=h
 try{
  await h.start();await h.idle();await page.getByRole('button',{name:'Sessions',exact:true}).click()
  const card=page.locator('.session-summary').first();await until(async()=>await card.locator('.session-runtime-value').innerText()==='Developer')
  assert.equal(await page.locator('.session-connection,.session-runtime-source').count(),0);assert.match(await card.locator('.session-runtime').getAttribute('title'),/Actual A/)
  await card.locator('a').click();await page.getByRole('heading',{name:'A calmer session explorer'}).waitFor()
  const transcript=page.locator('.session-viewer .session-runtime-value'),counters=[h.active.revision,h.active.committed_bytes,h.active.last_seen_at]
  await runtimeChanged(h,{model:'Actual B',role:'Reviewer',provider:'Known provider',reasoning:'high'});await until(async()=>await transcript.innerText()==='Reviewer');await until(async()=>await card.locator('.session-runtime-value').innerText()==='Reviewer')
  assert.match(await page.locator('.session-viewer .session-runtime').getAttribute('title'),/Actual B.*Known provider.*high/);assert.deepEqual([h.active.revision,h.active.committed_bytes,h.active.last_seen_at],counters)
  await page.getByRole('button',{name:'Projects',exact:true}).click();await h.idle();const task=page.locator('#entity-'+h.fixture.tasks[0].id)
  await task.locator('.btn-extras-toggle').click();await h.idle();await task.locator('.task-sessions-icon').click();await task.getByRole('button',{name:'Evidence',exact:true}).click();await task.locator('.task-session-evidence').waitFor()
  await until(async()=>await task.locator('.task-session-dock .session-viewer .session-runtime-value').innerText()==='Reviewer')
  assert.equal(await task.locator('.task-session-evidence .session-runtime-value').innerText(),'Developer');assert.equal(await task.locator('.session-connection,.session-runtime-source').count(),0)
  await runtimeChanged(h,null);await until(async()=>await task.locator('.task-session-dock .session-viewer .session-runtime-value').innerText()==='Unknown role')
  await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.active.id);await until(async()=>await page.locator('.session-viewer .session-runtime-value').innerText()==='Unknown role')
  assert.equal(await page.getByRole('heading',{name:'Transcript',exact:true}).count(),0);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})
test('legacy unknown/model-only and long runtime roles remain honest and compact at desktop and narrow widths',{timeout:60000},async()=>{
 for(const runtime of [null,{model:'Legacy model only'},{model:'Running model',role:'Worker'},{model:'model-'.repeat(40),role:'role-'.repeat(24)}]){
  const h=await openSessionExplorer({creator:{creator_actor_type:'bot',creator_bot_identity:{role:'Connection role','model-id':'codex:caller:high'}},runtime}),{page}=h
  try{
   await h.start();await h.idle();await page.getByRole('button',{name:'Sessions',exact:true}).click();await page.locator('.session-summary a').first().click();await page.getByRole('heading',{name:'A calmer session explorer'}).waitFor()
   for(const root of [page.locator('.session-summary').first(),page.locator('.session-viewer')]){assert.equal(await root.locator('.session-runtime-value').innerText(),runtime?.role||'Unknown role');assert.equal(await root.locator('.session-connection').count(),0);assert.ok((await root.locator('.session-runtime').getAttribute('title')).includes(runtime?.model||'Unknown'))}
   for(const width of [1440,430]){await page.setViewportSize({width,height:1000});assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth))}
   assert.deepEqual(h.errors,[])
  }finally{await h.close()}
 }
})
