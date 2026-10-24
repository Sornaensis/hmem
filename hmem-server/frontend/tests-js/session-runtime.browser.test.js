import assert from 'node:assert/strict'
import test from 'node:test'
import { openSessionExplorer } from './support/sessions-ux-fixture.mjs'

const until=async(fn)=>{const end=Date.now()+12000;while(Date.now()<end){if(await fn())return;await new Promise(resolve=>setTimeout(resolve,25))}throw new Error('Runtime identity did not converge')}
async function runtimeChanged(h,identity){
 h.active.runtime_identity=identity;h.active.runtime_revision++
 await h.page.evaluate(({workspace,id,revision})=>window.pushHierarchyFrames([{schema_version:1,type:'change',event:{schema_version:1,event_id:'runtime-'+revision,scope:'workspace',workspace_id:workspace,occurred_at:new Date().toISOString(),transaction:{id:'runtime-'+revision,cause:'rest',request_id:null},actor:{type:'service',id:'updater'},entity:{type:'agent_session',id,action:'updated'},invalidations:[{kind:'entity',target:'agent_session:'+id},{kind:'collection',target:'sessions:'+workspace}]}}]),{workspace:h.fixture.workspace.id,id:h.active.id,revision:h.active.runtime_revision})
}

test('runtime switches and unknown clearing refresh all session surfaces without relabelling the original connection caller',{timeout:60000},async()=>{
 const creator={creator_actor_type:'bot',creator_bot_identity:{role:'Connector','model-id':'codex:connection-model:high'}}
 const h=await openSessionExplorer({creator,runtime:{model:'Actual target A'}});const {page}=h
 try{
  await h.start();await h.idle();await page.getByRole('button',{name:'Sessions',exact:true}).click()
  const card=page.locator('.session-summary').first()
  await until(async()=>await card.locator('.session-runtime-value').innerText()==='Actual target A')
  assert.ok((await card.locator('.session-connection').innerText()).includes('codex:connection-model:high'))
  assert.ok(!(await card.locator('.session-runtime').innerText()).includes('connection-model'))
  assert.equal(await card.locator('.session-runtime-source').innerText(),'Declared')
  await card.locator('a').click();await page.getByRole('heading',{name:'A calmer session explorer'}).waitFor()
  const transcript=page.locator('.session-viewer .session-identity'),caller=await transcript.locator('.session-connection').innerText()
  assert.equal(await transcript.locator('.session-runtime-value').innerText(),'Actual target A')
  const counters=[h.active.revision,h.active.committed_bytes,h.active.last_seen_at]
  await runtimeChanged(h,{model:'Actual target B',provider:'Known provider',reasoning:'high'})
  await until(async()=>await transcript.locator('.session-runtime-value').innerText()==='Actual target B · Known provider · high')
  await until(async()=>await card.locator('.session-runtime-value').innerText()==='Actual target B · Known provider · high')
  assert.equal(await transcript.locator('.session-connection').innerText(),caller)
  assert.deepEqual([h.active.revision,h.active.committed_bytes,h.active.last_seen_at],counters)
  await page.getByRole('button',{name:'Projects',exact:true}).click();await h.idle()
  const task=page.locator('#entity-'+h.fixture.tasks[0].id)
  await task.locator('.btn-extras-toggle').click();await h.idle()
  await task.locator('.task-sessions-icon').click();await task.locator('.task-session-evidence summary').click()
  const active=task.locator('.task-session-dock .session-viewer .session-identity'),saved=task.locator('.task-session-evidence .session-identity')
  await until(async()=>(await active.locator('.session-runtime-value').innerText()).includes('Actual target B'))
  assert.equal((await saved.locator('.session-identity-label').first().innerText()).trim(),'Session model ·')
  assert.equal(await saved.locator('.session-runtime-value').innerText(),'Actual target A')
  assert.equal(await active.locator('.session-connection').innerText(),caller)
  await runtimeChanged(h,null)
  await until(async()=>await active.locator('.session-runtime-value').innerText()==='Unknown')
  assert.equal(await active.locator('.session-runtime-source').count(),0)
  assert.equal(await active.locator('.session-connection').innerText(),caller)
  await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.active.id)
  await until(async()=>await page.locator('.session-viewer .session-runtime-value').innerText()==='Unknown')
  assert.equal(await page.locator('.session-viewer .session-connection').innerText(),caller)
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('unknown, model-only, equal and long runtime declarations remain distinct from caller at desktop and narrow widths',{timeout:60000},async()=>{
 const cases=[
  {creator:{creator_actor_type:'bot',creator_bot_identity:{role:'Connector','model-id':'codex:caller-only:high'}},runtime:null},
  {creator:null,runtime:{model:'Model only'}},
  {creator:{creator_actor_type:'bot',creator_bot_identity:{role:'Worker','model-id':'codex:same-model:high'}},runtime:{model:'codex:same-model:high'}},
  {creator:{creator_actor_type:'bot',creator_bot_identity:{role:'Connector','model-id':'codex:'+ 'long-caller-'.repeat(18)+':high'}},runtime:{model:'target-model-'.repeat(18)}}
 ]
 for(const scenario of cases){
  const h=await openSessionExplorer(scenario);const {page}=h
  try{
   await h.start();await h.idle();await page.getByRole('button',{name:'Sessions',exact:true}).click()
   await page.locator('.session-summary a').first().click();await page.getByRole('heading',{name:'A calmer session explorer'}).waitFor()
   for(const root of [page.locator('.session-summary').first(),page.locator('.session-viewer')]){
    assert.equal(await root.locator('.session-runtime-value').innerText(),scenario.runtime?.model||'Unknown')
    assert.equal(await root.locator('.session-connection').count(),1)
    assert.ok((await root.locator('.session-connection').getAttribute('title')).includes(scenario.creator?.creator_bot_identity['model-id']||'Unknown'))
   }
   for(const width of [1440,430]){await page.setViewportSize({width,height:1000});assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=window.innerWidth));assert.ok((await page.locator('.session-viewer .session-runtime').getAttribute('title')).includes(scenario.runtime?.model||'Unknown'))}
   assert.deepEqual(h.errors,[])
  }finally{await h.close()}
 }
})
