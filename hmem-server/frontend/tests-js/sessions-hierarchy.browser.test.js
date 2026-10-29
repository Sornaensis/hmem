import assert from 'node:assert/strict'
import test from 'node:test'
import { openSessionExplorer } from './support/sessions-ux-fixture.mjs'

const until=async(fn)=>{const end=Date.now()+12000;while(Date.now()<end){if(await fn())return;await new Promise(resolve=>setTimeout(resolve,25))}throw new Error('Session hierarchy did not converge')}
async function change(h,type,id,action='updated',membership=false){
 await h.page.evaluate(({workspace,type,id,action,membership})=>window.pushHierarchyFrames([{schema_version:1,type:'change',event:{schema_version:1,event_id:'change-'+crypto.randomUUID(),scope:'workspace',workspace_id:workspace,occurred_at:new Date().toISOString(),transaction:{id:'session-hierarchy',cause:'rest',request_id:null},actor:{type:'system',id:null},entity:{type,id,action},invalidations:[{kind:'entity',target:type+':'+id},...(type==='agent_session'?[{kind:'collection',target:'sessions:'+workspace}]:[]),...(membership?[{kind:'collection',target:'active_sessions:'+workspace}]:[])]}}]),{workspace:h.fixture.workspace.id,type,id,action,membership})
}

test('active project/task facets page beyond50 and retire only invalid entity scopes after successful authority',{timeout:60000},async()=>{
 const h=await openSessionExplorer({linkedCount:55});const {page}=h
 try{
  const project=h.fixture.projects[0],other={...project,id:'50000000-0000-4000-8000-000000000002',name:'Other active project'}
  h.fixture.projects.push(other);h.active.title='Repair session filters'
  await h.start();await h.idle();await page.getByRole('button',{name:'Sessions',exact:true}).click()
  const projects=page.getByRole('combobox',{name:'Project sessions'}),tasks=page.getByRole('combobox',{name:'Task sessions'})
  await until(async()=>await tasks.locator('option').count()===51)
  assert.equal(await projects.locator('option').count(),2,'inactive project is excluded')
  await page.getByRole('button',{name:'More tasks',exact:true}).click();await until(async()=>await tasks.locator('option').count()===6)
  const selected=h.fixture.tasks.at(-1)
  await tasks.selectOption(selected.id);await until(()=>page.url().includes('task='+selected.id))
  await projects.selectOption(project.id);await until(()=>page.url().includes('project='+project.id)&&!page.url().includes('&task='))
  await page.getByRole('button',{name:'More tasks',exact:true}).click();await until(async()=>await tasks.locator('option').count()===6)
  await tasks.selectOption(selected.id);await until(()=>page.url().includes('task='+selected.id))
  selected.project_id=other.id;await change(h,'task',selected.id)
  await until(()=>!page.url().includes('&task='));assert.ok(page.url().includes('project='+project.id))
  await until(async()=>await projects.locator('option').count()===3)
  await projects.selectOption(other.id);await until(()=>page.url().includes('project='+other.id));await until(async()=>await tasks.locator('option').count()===2)
  await tasks.selectOption(selected.id);await until(()=>page.url().includes('task='+selected.id))
  const retained=page.url()
  await page.route('**/sessions/filters?**',route=>route.fulfill({status:503,json:{error:'controlled facet failure'}}))
  await change(h,'task',selected.id);await page.getByRole('button',{name:'Retry projects',exact:true}).waitFor()
  assert.equal(page.url(),retained,'failed refresh preserves authoritative selection')
  await page.unroute('**/sessions/filters?**');await page.getByRole('button',{name:'Retry projects',exact:true}).click()
  await until(async()=>await page.getByRole('button',{name:'Retry projects',exact:true}).count()===0)
  h.fixture.tasks=h.fixture.tasks.filter(task=>task.id!==selected.id);await change(h,'task',selected.id,'deleted')
  await until(()=>!page.url().includes('&task='));assert.ok(page.url().includes('project='+other.id))
  await projects.selectOption(project.id);await until(()=>page.url().includes('project='+project.id))
  h.active.state='terminated';await change(h,'agent_session',h.active.id,'updated',true)
  await until(async()=>await page.locator('.session-summary').count()===0);assert.ok(page.url().includes('project='+project.id))
  assert.ok(h.peak()<=2);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('titled deep session trees keep inactive ancestor context and bounded indentation at desktop and narrow widths',{timeout:60000},async()=>{
 const h=await openSessionExplorer({activeCount:25,creator:{creator_actor_type:'bot',creator_bot_identity:{role:'Developer','model-id':'codex:gpt-6.1-sol:high'}}});const {page}=h
 try{
  const now=new Date().toISOString()
  h.activeSessions.forEach((session,index)=>{session.title='Session work '+index;session.last_seen_at=now;session.parent_session_id=index?h.activeSessions[index-1].id:null;session.ancestors=h.activeSessions.slice(Math.max(0,index-16),index).map(parent=>({id:parent.id,title:parent.title,format_label:parent.format_label,state:parent.state,parent_session_id:parent.parent_session_id}));session.ancestors_truncated=index>16})
  const outside=h.saved;outside.title='Completed parent context';h.active.parent_session_id=outside.id;h.active.ancestors=[{id:outside.id,title:outside.title,format_label:outside.format_label,state:'terminated',parent_session_id:null}]
  await h.start();await h.idle();await page.getByRole('button',{name:'Sessions',exact:true}).click()
  await until(async()=>await page.locator('.session-summary').count()===25)
  assert.equal(await page.locator('.session-summary-heading').filter({hasText:'Completed parent context'}).count(),0)
  assert.ok(await page.locator('.session-parent-context').filter({hasText:'Completed parent context'}).isVisible())
  const deepest=page.locator('.session-summary').filter({has:page.locator('.session-summary-heading',{hasText:'Session work 24'})})
  assert.ok((await deepest.locator('.session-parent-context').getAttribute('title')).includes('16-level context limit'))
  for(const width of [1440,430]){await page.setViewportSize({width,height:1000});assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=window.innerWidth));assert.ok(await deepest.evaluate(el=>el.getBoundingClientRect().width>=140))}
  await page.setViewportSize({width:1440,height:1000});const card=page.locator('.session-summary').first();const before=await card.evaluate(el=>getComputedStyle(el).borderColor);await card.hover();await page.waitForTimeout(180);assert.notEqual(await card.evaluate(el=>getComputedStyle(el).borderColor),before)
  await card.locator('a').click();await page.getByRole('heading',{name:'A calmer session explorer'}).waitFor()
  assert.equal(await page.locator('.session-detail-header h3').innerText(),'Session work 0')
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('Has Active Session filters retain ancestor paths and ordinary heartbeat updates do not restart branch membership',{timeout:60000},async()=>{
 const h=await openSessionExplorer({many:true});const {page}=h
 try{
  await h.start();await h.idle();const box=page.getByRole('checkbox',{name:'Has Active Session',exact:true});await box.check();await h.idle()
  assert.ok(await box.isChecked())
  const keys=await h.logicalKeys();assert.ok(keys.has('project:'+h.fixture.projects[0].id));assert.ok(keys.has('task:'+h.fixture.tasks.at(-1).id));assert.ok(!keys.has('task:'+h.fixture.tasks[0].id))
  const before=h.requests.filter(request=>request.path?.endsWith('/navigation')&&!request.logical).length
  for(let i=0;i<4;i++)await change(h,'agent_session',h.active.id)
  await page.waitForTimeout(1300)
  assert.equal(h.requests.filter(request=>request.path?.endsWith('/navigation')&&!request.logical).length,before,'ordinary output does not restart hierarchy reads')
  h.active.state='terminated';await change(h,'agent_session',h.active.id,'updated',true)
  await until(async()=>!(await h.logicalKeys()).has('task:'+h.fixture.tasks.at(-1).id))
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('ordinary output cannot starve a held multi-page Has Active Session membership pass',{timeout:60000},async()=>{
 const h=await openSessionExplorer({many:true});const {page}=h
 try{
  const template=h.fixture.tasks[0]
  h.fixture.tasks=Array.from({length:151},(_,index)=>({...template,id:`11000000-0000-4000-8000-${String(index+1).padStart(12,'0')}`,title:'Active branch '+String(index+1).padStart(3,'0')}))
  h.sessionLinks.set(h.active.id,new Set(h.fixture.tasks.map(task=>task.id)))
  await h.start();await h.idle()
  const gate=h.gate(request=>request.parent===h.fixture.projects[0].id&&request.includeTasks&&request.taskOffset===50,{hold:true})
  await page.getByRole('checkbox',{name:'Has Active Session',exact:true}).check()
  await gate.arrived
  const roots=h.requests.filter(request=>request.kind==='workspace_root'&&request.path?.endsWith('/navigation')).length
  for(let index=0;index<4;index++){await change(h,'agent_session',h.active.id);await page.waitForTimeout(350)}
  assert.equal(h.requests.filter(request=>request.kind==='workspace_root'&&request.path?.endsWith('/navigation')).length,roots)
  gate.release();await h.idle()
  assert.ok(h.requests.some(request=>request.parent===h.fixture.projects[0].id&&request.taskOffset===100&&request.done),'the same pass reaches the later page')
  const keys=await h.logicalKeys();assert.ok(keys.has('task:'+h.fixture.tasks.at(-1).id))
  assert.ok(h.caps().maxBranches<=4);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})
