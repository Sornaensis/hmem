import assert from 'node:assert/strict'
import test from 'node:test'
import { join } from 'node:path'
import { openSessionExplorer } from './support/sessions-ux-fixture.mjs'
const until=async fn=>{const end=Date.now()+12000;while(Date.now()<end){if(await fn())return;await new Promise(r=>setTimeout(r,25))}throw Error('Task dock did not converge')}
const card=(h,index)=>h.page.locator('#entity-'+h.fixture.tasks[index].id)
const agent=(h,index)=>card(h,index).locator(':scope > .card-header .task-sessions-icon')
const changed=async(h,session)=>h.page.evaluate(({workspace,session})=>window.pushHierarchyFrames([{schema_version:1,type:'change',event:{schema_version:1,event_id:'dock-change-'+Date.now(),scope:'workspace',workspace_id:workspace,occurred_at:new Date().toISOString(),transaction:{id:'dock-change',cause:'rest',request_id:null},actor:{type:'system',id:null},entity:{type:'agent_session',id:session,action:'updated'},invalidations:[{kind:'entity',target:'agent_session:'+session},{kind:'collection',target:'sessions:'+workspace}]}}]),{workspace:h.fixture.workspace.id,session})

test('task-owned Active Agent dock retains task context, grouped transcript, evidence and a single viewer across switches',{timeout:60000},async()=>{
 const h=await openSessionExplorer({activeCount:2,runtime:{model:'Dock runtime',role:'Developer',reasoning:'high'}}),{page}=h
 const other=h.activeSessions[1],bytes=Buffer.from(JSON.stringify({type:'response_item',timestamp:new Date().toISOString(),payload:{type:'message',role:'assistant',content:[{type:'output_text',text:'The second agent transcript.'}]}})+'\n')
 Object.assign(other,{committed_bytes:bytes.length,committed_records:1});h.content.set(other.id,bytes)
 try{
  await h.start();await h.idle();await until(()=>agent(h,0).isEnabled())
  assert.deepEqual((await page.locator('.workspace-tabs > button').allTextContents()).slice(0,4),['Projects','Sessions','Observations','Timeline'])
  assert.equal(await page.locator('.task-session-active').count(),0)
  assert.equal(await card(h,0).locator('.card-extras').count(),0)
  await agent(h,0).locator('.session-activity-core').click();await page.getByRole('heading',{name:'A calmer session explorer'}).waitFor()
  assert.ok(page.url().includes('tab=projects'));assert.ok(!page.url().includes('focus=task'))
  assert.equal(await card(h,0).locator('.task-session-dock').count(),1);assert.equal(await card(h,0).getAttribute('draggable'),'false');assert.equal(await page.locator('#session-records').count(),1)
  assert.equal(await card(h,0).locator('.card-extras').count(),0,'dock is independent of extras')
  assert.equal(await card(h,0).locator('.task-session-choice').count(),2,'quiet active sessions remain available')
  assert.ok((await page.locator('.session-viewer .session-runtime').getAttribute('title')).includes('Dock runtime'))
  const group=page.locator('.session-activity-group').first();assert.equal(await group.getAttribute('open'),null);await group.locator(':scope > summary').click();assert.equal(await group.getAttribute('open'),'')
  await card(h,0).locator('.task-session-choice').nth(1).locator('span').last().click();await page.getByText('The second agent transcript.',{exact:true}).waitFor()
  assert.equal(await page.getByRole('heading',{name:'A calmer session explorer'}).count(),0)
  await h.scrollTo('task:'+h.fixture.tasks[1].id)
  const arrow=await agent(h,1).boundingBox();await agent(h,1).click({position:{x:arrow.width-9,y:arrow.height/2}});await page.getByRole('heading',{name:'A calmer session explorer'}).waitFor();assert.equal(await card(h,0).locator('.task-session-dock').count(),0);assert.equal(await card(h,1).locator('.task-session-dock').count(),1)
  assert.equal(await page.locator('#session-records').count(),1);assert.equal(await page.locator('.session-activity-group').first().getAttribute('open'),null)
  const prose=page.getByRole('heading',{name:'A calmer session explorer'});await prose.scrollIntoViewIfNeeded();const textBox=await prose.boundingBox();await page.mouse.move(textBox.x+5,textBox.y+textBox.height/2);await page.mouse.down();await page.mouse.move(textBox.x+Math.min(180,textBox.width-10),textBox.y+textBox.height/2,{steps:12});await page.mouse.up();assert.ok((await page.evaluate(()=>getSelection().toString())).length>0,'transcript prose remains selectable without dragging the task');assert.equal(await page.locator('.dragging').count(),0)
  await page.evaluate(()=>getSelection().removeAllRanges())
  const extras=card(h,1).locator(':scope > .card-body .btn-extras-toggle');await extras.click();await card(h,1).getByRole('button',{name:'Evidence',exact:true}).click();await card(h,1).locator('.task-session-evidence summary').click();await card(h,1).getByRole('button',{name:'↓ Download all',exact:true}).waitFor()
  assert.equal(await page.locator('.task-session-active').count(),0);await extras.click();assert.equal(await card(h,1).locator('.task-session-dock').count(),1)
  await page.setViewportSize({width:1440,height:1000});const rects=await card(h,1).evaluate(el=>{const a=el.querySelector(':scope > .card-body').getBoundingClientRect(),b=el.querySelector('.task-session-dock').getBoundingClientRect();return {a:{right:a.right,top:a.top},b:{left:b.left,top:b.top}}});assert.ok(rects.b.left>=rects.a.right)
  if(process.env.HMEM_DOCK_SCREENSHOTS)await page.screenshot({path:join(process.env.HMEM_DOCK_SCREENSHOTS,'task-agent-dock-desktop.png'),fullPage:true})
  await page.setViewportSize({width:430,height:1600});assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth));await card(h,1).evaluate(el=>{const scroll=document.getElementById('main-content-scroll'),sticky=document.querySelector('.sticky-workspace-bar');scroll.scrollTop+=el.getBoundingClientRect().top-(sticky?.getBoundingClientRect().bottom||scroll.getBoundingClientRect().top)-12});if(process.env.HMEM_DOCK_SCREENSHOTS)await page.screenshot({path:join(process.env.HMEM_DOCK_SCREENSHOTS,'task-agent-dock-narrow.png'),fullPage:true})
  await page.getByRole('button',{name:'Close active agents',exact:true}).click();assert.equal(await page.locator('#session-records').count(),0)
  await agent(h,1).click();await page.locator('#session-records').waitFor();await agent(h,1).click();assert.equal(await page.locator('.task-session-dock').count(),0);assert.equal(await card(h,1).getAttribute('draggable'),'true')
  assert.ok(h.peak()<=2);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('terminated metadata and lost membership retire the dock viewer, and leaving Projects retires the owner',{timeout:45000},async()=>{
 const h=await openSessionExplorer(),{page}=h
 try{
  await h.start();await until(()=>agent(h,0).isEnabled());await agent(h,0).click();await page.getByRole('heading',{name:'A calmer session explorer'}).waitFor()
  h.active.state='terminated';await changed(h,h.active.id);await until(async()=>await page.locator('#session-records').count()===0)
  await until(async()=>await page.getByText('No active sessions for this task.',{exact:true}).count()===1)
  await page.getByRole('button',{name:'Close active agents',exact:true}).click();assert.equal(await agent(h,0).isDisabled(),true)
  h.active.state='active';await changed(h,h.active.id);await until(()=>agent(h,0).isEnabled());await agent(h,0).click();await page.locator('#session-records').waitFor()
  await page.getByRole('button',{name:'Sessions',exact:true}).click();await until(async()=>await page.locator('.task-session-dock').count()===0);assert.equal(await page.locator('#session-records').count(),0)
  await page.getByRole('button',{name:'Projects',exact:true}).click();await until(async()=>await agent(h,0).count()===1);assert.equal(await page.locator('.task-session-dock').count(),0)
  assert.ok(h.peak()<=2);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})


test('removing a task association keeps the global session active but retires only that task dock viewer',{timeout:45000},async()=>{
 const h=await openSessionExplorer(),{page}=h
 try{
  await h.start();await until(()=>agent(h,0).isEnabled());await agent(h,0).click();await page.getByRole('heading',{name:'A calmer session explorer'}).waitFor()
  h.sessionLinks.get(h.active.id).delete(h.fixture.tasks[0].id);await changed(h,h.active.id)
  await until(async()=>await page.locator('#session-records').count()===0);assert.equal(h.active.state,'active');assert.equal(await card(h,0).getByText('No active sessions for this task.',{exact:true}).count(),1)
  await agent(h,1).click();await page.getByRole('heading',{name:'A calmer session explorer'}).waitFor();assert.equal(await card(h,1).locator('#session-records').count(),1)
  assert.ok(h.peak()<=2);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})


test('an unfocused dock retires when its card leaves the bounded mounted hierarchy window',{timeout:45000},async()=>{
 const h=await openSessionExplorer(),{page}=h
 for(let i=2;i<55;i++)h.fixture.tasks.push({...h.fixture.tasks[0],id:'10000000-0000-4000-8000-'+String(i+1).padStart(12,'0'),title:'Later task '+i})
 try{
  await h.start();await until(()=>agent(h,0).isEnabled());await agent(h,0).click();await page.locator('#session-records').waitFor()
  await page.getByPlaceholder('Filter project hierarchy...').focus()
  await page.locator('#main-content-scroll').evaluate(el=>{el.scrollTop=el.scrollHeight;el.dispatchEvent(new Event('scroll'))})
  await until(async()=>await page.locator('.task-session-dock').count()===0)
  assert.equal(await page.locator('#session-records').count(),0)
  assert.ok(await page.locator('[data-hierarchy-key]').count()<=25,'ordinary mounted rows stay bounded')
  assert.ok(h.peak()<=2);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})
