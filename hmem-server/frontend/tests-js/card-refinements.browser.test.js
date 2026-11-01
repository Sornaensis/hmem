import assert from 'node:assert/strict'
import test from 'node:test'
import {hierarchyFixture, openHierarchy} from './hierarchy-fixture.mjs'
import {openSessionExplorer} from './support/sessions-ux-fixture.mjs'

const until=async fn=>{const end=Date.now()+12000;while(Date.now()<end){if(await fn())return;await new Promise(resolve=>setTimeout(resolve,25))}throw Error('Card refinements did not converge')}
const focusTarget=page=>new URLSearchParams(new URL(page.url()).hash.slice(1)).get('focus')

test('card controls align, share motion, and nested Focus/Move content preserves canonical navigation',{timeout:60000},async()=>{
 const f=hierarchyFixture();f.projects=[f.projects[0]];f.tasks=[{...f.tasks[0],id:'leaf',title:'Leaf task'}]
 const h=await openHierarchy(f),{page}=h
 try{
  await h.start();await h.idle();await page.getByRole('button',{name:'Expand All',exact:true}).click();await h.idle()
  const task=page.locator('#entity-leaf'),project=page.locator('#entity-root-project > .card-project')
  for(const width of [1920,430,320]){
   await page.setViewportSize({width,height:1400});await h.idle()
   for(const [kind,id,card] of [['project','root-project',project],['task','leaf',task]]){
    await h.scrollTo(kind+':'+id)
    const row=card.locator('.card-inline-actions').first(),details=row.locator('.card-details-control'),create=row.locator('.btn-inline-create')
    const [r,d,c]=await Promise.all([row.boundingBox(),details.boundingBox(),create.last().boundingBox()])
    assert.ok(Math.abs(d.x-r.x-14)<2,JSON.stringify({width,r,d,c}));assert.ok(Math.abs(c.x+c.width-(r.x+r.width-14))<2)
    const durations=await card.locator('.card-details-control,.card-action-control,.btn-inline-create').evaluateAll(nodes=>nodes.map(n=>getComputedStyle(n).transitionDuration))
    assert.ok(durations.every(value=>value.split(',').every(part=>part.trim()==='0.3s')),durations.join(';'))
    assert.ok((await card.locator('.card-footer').boundingBox()).height<75,'metadata footer remains compact even when wrapping')
   }
   assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth),false)
  }
  await page.setViewportSize({width:1920,height:1400});await h.scrollTo('task:leaf')
  const empty=page.locator('[data-task-family="leaf"]')
  assert.equal(await empty.evaluate(n=>getComputedStyle(n).boxShadow),'none');assert.equal(await empty.evaluate(n=>getComputedStyle(n).backgroundColor),'rgba(0, 0, 0, 0)')
  const focus=task.getByRole('button',{name:'Focus task',exact:true}),move=task.getByRole('button',{name:'Move task',exact:true})
  await page.keyboard.press('Tab');await focus.focus();await until(async()=>await focus.locator('.card-action-label').evaluate(n=>n.getBoundingClientRect().width)>20)
  await focus.locator('svg path').click();await until(()=>Promise.resolve(focusTarget(page)==='task:leaf'))
  await page.locator('.focus-clear-btn').click();await h.idle();await h.scrollTo('task:leaf')
  await page.keyboard.press('Tab');await move.focus();await until(async()=>await move.locator('.card-action-label').evaluate(n=>n.getBoundingClientRect().width)>30)
  assert.equal(await move.locator('.card-action-label').innerText(),'Move To')
  const [labelBox,arrowBox]=await Promise.all([move.locator('.card-action-label').boundingBox(),move.locator('.card-action-icon').boundingBox()]);assert.ok(arrowBox.x>=labelBox.x+labelBox.width)
  await move.locator('.card-action-label').click()
  await page.getByRole('dialog').waitFor();assert.equal(focusTarget(page),null)
  await page.keyboard.press('Escape');await project.getByRole('button',{name:'Focus project',exact:true}).focus();await page.keyboard.press('Enter')
  await until(()=>Promise.resolve(focusTarget(page)==='project:root-project'))
  await page.emulateMedia({reducedMotion:'reduce'})
  assert.equal(await page.locator('.card-details-control').first().evaluate(n=>getComputedStyle(n).transitionDuration.split(',').every(x=>x.trim()==='0s')),true)
  assert.deepEqual(h.errors,[]);assert.deepEqual(h.unhandled,[])
 }finally{await h.close()}
})

test('embedded summary fills the reader, resizable columns and full-width details keep the title and badges aligned, and populated tray tracks dock width',{timeout:60000},async t=>{
 const h=await openSessionExplorer({runtime:{harness:'codex',model:'card-layout-model',role:'developer',reasoning:'high'}}),{page,fixture:f}=h
 const owner=f.tasks[0],child={...f.tasks[1],id:'10000000-0000-4000-8000-000000000003',title:'Child task',parent_id:owner.id}
 f.tasks.push(child);owner.description='Summary context. '.repeat(250)
 try{
  await page.setViewportSize({width:2560,height:1800});await h.start();await page.getByRole('button',{name:'Expand All',exact:true}).click();await h.idle()
  const card=page.locator('#entity-'+owner.id),family=page.locator('[data-task-family="'+owner.id+'"]'),agents=card.locator(':scope > .card-header .task-sessions-icon')
  await until(()=>agents.isEnabled());const childBefore=await page.locator('#entity-'+child.id).boundingBox();await agents.click();await card.locator('#session-records .session-record').first().waitFor();await h.idle()
  const geometry=await card.evaluate(n=>{
   const box=s=>n.querySelector(s).getBoundingClientRect().toJSON()
   const description=n.querySelector('.card-desc-row'),copy=description.querySelector('.editable-textarea')
   return {work:box(':scope > .task-card-work'),dock:box(':scope > .task-session-dock'),description:box('.card-desc-row'),descriptionScroll:{height:description.clientHeight,content:description.scrollHeight,copyHeight:copy.clientHeight,copyContent:copy.scrollHeight},actions:box('.task-card-work > .card-inline-actions'),title:box('.task-session-selected-context h4'),identity:box(':is(.session-viewer,.task-session-selected-context) .session-identity'),footer:box(':scope > .card-footer'),linkSize:getComputedStyle(n.querySelector('.task-session-open-full')).fontSize}
  })
  assert.ok(Math.abs(geometry.work.height-geometry.dock.height)<1);assert.ok(geometry.description.height>350,JSON.stringify(geometry))
  assert.ok(geometry.identity.top-geometry.title.bottom<=8);assert.ok(geometry.title.height<24);assert.ok(geometry.footer.height<=38);assert.equal(geometry.linkSize,'11px')
  const [tray,face,childAfter]=await Promise.all([family.boundingBox(),card.boundingBox(),page.locator('#entity-'+child.id).boundingBox()])
  assert.ok(Math.abs(tray.width-face.width)<1,JSON.stringify({tray,face}));assert.ok(Math.abs(childAfter.width-childBefore.width)<1,'other faces retain their width')
  const actions=await card.locator(':scope > .card-header > .card-actions > button').evaluateAll(nodes=>nodes.map(n=>n.getAttribute('aria-label')))
  assert.deepEqual(actions.slice(0,3),['Hide Agents','Focus task','Move task'])
  const jump=card.getByRole('link',{name:'Jump To Session ↗'});assert.ok((await jump.getAttribute('href')).includes('&task='+owner.id))
  t.diagnostic(JSON.stringify({desktop:geometry,trayWidth:tray.width,cardWidth:face.width}))
  const handle=card.getByRole('separator',{name:'Resize task summary and session'})
  const work=card.locator(':scope > .task-card-work'),before=(await work.boundingBox()).width
  await handle.focus();await page.keyboard.press('ArrowRight');await h.idle();assert.ok((await work.boundingBox()).width>before)
  assert.equal(await handle.getAttribute('aria-valuenow'),'36')
  await page.keyboard.press('Home');assert.equal(await handle.getAttribute('aria-valuenow'),'20')
  await page.keyboard.press('End');assert.equal(await handle.getAttribute('aria-valuenow'),'60')
  await agents.click();await agents.click();await card.locator('#session-records .session-record').first().waitFor();await h.idle()
  assert.equal(await handle.getAttribute('aria-valuenow'),'34','reopened handle and rendered default width agree')
  assert.ok(Math.abs((await work.boundingBox()).width-before)<1)
  await handle.focus();await page.keyboard.press('ArrowRight');assert.equal(await handle.getAttribute('aria-valuenow'),'36')
  await page.keyboard.press('End')
  const grip=await handle.boundingBox();await page.mouse.move(grip.x+grip.width/2,grip.y+30);await page.mouse.down();await page.mouse.move(grip.x-150,grip.y+30,{steps:8});await page.mouse.up()
  assert.ok(Number(await handle.getAttribute('aria-valuenow'))<60,'pointer changes the summary share')
  assert.equal(await card.getAttribute('draggable'),'false');assert.equal(await page.locator('#session-records').count(),1)
  await card.locator('.btn-extras-toggle').click();await h.idle()
  const expanded=await card.evaluate(n=>{const box=s=>n.querySelector(s).getBoundingClientRect().toJSON(),header=n.querySelector('.task-session-selected-context');return{card:n.getBoundingClientRect().toJSON(),details:box(':scope > .card-extras'),dock:box(':scope > .task-session-dock'),agents:box(':scope > .task-session-context-choices'),header:box('.task-session-selected-context'),identity:box('.task-session-selected-context .session-identity'),rule:getComputedStyle(header).borderBottomWidth,padding:getComputedStyle(n.querySelector('.card-desc-row > .editable-textarea')).paddingLeft,roleBounds:box('.task-session-selected-context .session-role'),modelBounds:box('.task-session-selected-context .session-runtime'),role:header.querySelector('.session-role').textContent,model:header.querySelector('.session-runtime').textContent}})
  assert.ok(expanded.details.top>=Math.max(expanded.dock.bottom,expanded.agents.bottom)-1)
  assert.ok(Math.abs(expanded.details.width-(expanded.card.width-2))<4)
  assert.ok(Math.abs(expanded.header.right-expanded.identity.right)<1);assert.equal(expanded.rule,'1px');assert.ok(parseFloat(expanded.padding)>=16)
  assert.ok(expanded.modelBounds.left>=expanded.roleBounds.right-1);assert.ok(Math.abs(expanded.modelBounds.top-expanded.roleBounds.top)<1);assert.equal(expanded.role,'developer');assert.equal(expanded.model,'codex:card-layout-model:high')
  const guides=await card.evaluate(n=>getComputedStyle(n.closest('[data-hierarchy-depth]'),'::after').backgroundImage)
  assert.ok(guides.includes('repeating-linear-gradient'));assert.ok(await page.locator('[data-hierarchy-key]').count()<=25)
  if(process.env.HMEM_DOCK_SCREENSHOTS)await card.screenshot({path:process.env.HMEM_DOCK_SCREENSHOTS+'/card-refinements-desktop.png'})
  for(const width of [736,430,320]){await page.setViewportSize({width,height:1800});await h.idle();assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth),false);assert.equal(await card.getByText(owner.description,{exact:true}).isVisible(),true)}
  const stacked=await card.evaluate(n=>{const box=s=>n.querySelector(s).getBoundingClientRect().toJSON();return{work:box(':scope > .task-card-work'),dock:box(':scope > .task-session-dock'),agents:box(':scope > .task-session-context-choices')}})
  assert.ok(stacked.dock.top>=stacked.work.bottom);assert.ok(stacked.agents.top>=stacked.dock.bottom);assert.equal(await handle.isVisible(),false);const details=await card.locator(':scope > .card-extras').boundingBox();assert.ok(details.y>=stacked.agents.bottom-1)
  t.diagnostic(JSON.stringify({narrow:stacked}))
  if(process.env.HMEM_DOCK_SCREENSHOTS){await page.setViewportSize({width:320,height:6000});await h.idle();await card.screenshot({path:process.env.HMEM_DOCK_SCREENSHOTS+'/card-refinements-narrow.png'});await page.setViewportSize({width:1920,height:6000});await h.idle();await page.locator('.hierarchy-viewport').screenshot({path:process.env.HMEM_DOCK_SCREENSHOTS+'/card-hierarchy-desktop.png'})}
  assert.deepEqual(h.errors,[]);assert.deepEqual(h.unhandled,[]);assert.ok(h.peak()<=2)
 }finally{await h.close()}
})

test('touch cards expose action labels and preserve large copy and navigation targets',{timeout:30000},async t=>{
 const f=hierarchyFixture();f.projects=[f.projects[0]];f.tasks=[]
 const h=await openHierarchy(f,[],null,{hasTouch:true,isMobile:true,viewport:{width:430,height:1200}})
 try{
  await h.start();await h.idle();const card=h.page.locator('#entity-root-project > .card-project')
  const result=await card.evaluate(n=>({footer:n.querySelector('.card-footer').getBoundingClientRect().height,controls:[...n.querySelectorAll('.card-action-control,.card-details-control,.card-id-copy')].map(el=>({height:el.getBoundingClientRect().height,text:el.textContent,labelWidth:el.querySelector('.card-action-label,.card-details-label')?.getBoundingClientRect().width??null}))}))
  assert.ok(result.controls.every(c=>c.height>=44));assert.ok(result.controls.filter(c=>c.labelWidth!==null).every(c=>c.labelWidth>20));t.diagnostic(JSON.stringify({touch:result}));assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('all split family trays retain the open dock width during inline creation and parent/child edits',{timeout:45000},async()=>{
 const h=await openSessionExplorer(),{page,fixture:f}=h,owner=f.tasks[0]
 const child={...f.tasks[1],id:'10000000-0000-4000-8000-000000000003',title:'Editable child',parent_id:owner.id};f.tasks.push(child)
 try{
  await page.setViewportSize({width:2560,height:6000});await h.start();await page.locator('#entity-'+owner.id+' .task-sessions-icon').click();await page.locator('#session-records .session-record').first().waitFor();await h.idle()
  const card=page.locator('#entity-'+owner.id),families=page.locator('[data-task-family="'+owner.id+'"]')
  const assertSplit=async()=>{
   await h.idle();const face=await card.boundingBox(),bounds=await families.evaluateAll(nodes=>nodes.map(n=>n.getBoundingClientRect().toJSON()))
   assert.ok(bounds.length>=2,'protected input partitions the family into multiple wrappers')
   assert.ok(bounds.every(b=>Math.abs(b.width-face.width)<1),JSON.stringify({face,bounds}))
   assert.ok(await page.locator('[data-hierarchy-key]').count()<=25);assert.equal(await page.locator('#session-records').count(),1)
  }
  await card.getByRole('button',{name:'+ Subtask',exact:true}).click();await page.getByPlaceholder('Subtask title...').waitFor();await assertSplit();await page.getByPlaceholder('Subtask title...').press('Escape')
  for(const id of [owner.id,child.id]){
   await page.locator('#entity-'+id+' > .card-header .editable-text').click();const input=page.locator('#entity-'+id+' .inline-edit-input');await input.waitFor();await assertSplit();await input.press('Escape')
  }
  assert.deepEqual(h.errors,[]);assert.deepEqual(h.unhandled,[])
 }finally{await h.close()}
})

