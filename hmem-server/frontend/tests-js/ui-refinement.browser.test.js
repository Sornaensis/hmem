import assert from 'node:assert/strict'
import test from 'node:test'
import { mkdir } from 'node:fs/promises'
import { resolve } from 'node:path'
import { openSessionExplorer } from './support/sessions-ux-fixture.mjs'

const until=async fn=>{const end=Date.now()+12000;while(Date.now()<end){if(await fn())return;await new Promise(done=>setTimeout(done,25))}throw Error('UI refinement did not converge')}
const shots=resolve('../../output/ui-refinement')
const screenshot=async(page,name)=>{await mkdir(shots,{recursive:true});await page.screenshot({path:resolve(shots,name+'.png'),fullPage:true})}

test('local card sections retain task operations and a single contextual preview under persistent workspace controls',{timeout:60000},async()=>{
 const h=await openSessionExplorer(),{page}=h
 try{
  h.active.title='Review session navigation';h.active.parent_session_id=h.saved.id
  h.active.ancestors=[{id:h.saved.id,title:'Completed parent context',format_label:'Codex JSONL',state:'terminated',parent_session_id:null}]
  await h.start();await h.idle()
  assert.equal(await page.locator('.workspace-context-persistent').count(),1)
  assert.equal(await page.getByRole('searchbox',{name:'Search all entities',exact:true}).count(),1)
  assert.equal(await page.getByPlaceholder('Filter project hierarchy...').count(),1)
  const card=page.locator('#entity-'+h.fixture.tasks[0].id)
  const metadata=card.locator('.card-metadata'),metadataSummary=metadata.locator(':scope > summary'),location=page.url()
  await metadataSummary.dblclick();assert.equal(page.url(),location,'Metadata is not a card focus gesture')
  await card.locator(':scope > .card-inline-actions button').last().focus();await page.keyboard.press('Tab')
  assert.ok(await metadataSummary.evaluate(node=>node===document.activeElement),'native Tab reaches Metadata before the logical row edge')
  await page.keyboard.press('Enter');assert.equal(await metadata.getAttribute('open'),'')
  await page.keyboard.press('Space');assert.equal(await metadata.getAttribute('open'),null)
  await card.locator('.btn-extras-toggle').click()
  const controls=card.locator('.card-actions select'),before=await controls.evaluateAll(nodes=>nodes.map(node=>({value:node.value,disabled:node.disabled})))
  for(const section of ['Work','Agents','Evidence','History']){
   const button=card.getByRole('button',{name:section,exact:true});await button.focus();await page.keyboard.press('Enter')
   await until(async()=>await button.getAttribute('aria-pressed')==='true')
   const target=await button.getAttribute('aria-controls')
   assert.equal(await page.locator('#'+target).count(),1);assert.ok(await page.locator('#'+target).isVisible())
   assert.deepEqual(await controls.evaluateAll(nodes=>nodes.map(node=>({value:node.value,disabled:node.disabled}))),before)
  }
  await card.getByRole('button',{name:'Work',exact:true}).click();await card.locator('.task-dependencies-section').waitFor({state:'visible'})
  await screenshot(page,'workspace-cards-desktop')
  await card.getByRole('button',{name:'Agents',exact:true}).click();await until(async()=>await card.locator('.card-agents-open').isEnabled());await card.getByRole('button',{name:'Open in card',exact:true}).click()
  const dock=card.locator('.task-session-dock');await dock.locator('.session-record').first().waitFor()
  assert.equal(await page.locator('#session-records').count(),1)
  assert.match(await dock.getByRole('region',{name:'Selected session lineage'}).innerText(),/Completed parent context.*ancestry context.*terminated/s)
  assert.equal(await dock.locator('.task-session-choice').count(),1,'a contextual ancestor is not an active result')
  await card.getByRole('button',{name:'Evidence',exact:true}).click();await card.locator('.task-session-evidence summary').click();await card.getByRole('button',{name:/Download session/}).waitFor()
  assert.equal(await page.locator('#session-records').count(),1)
  await screenshot(page,'context-dock-desktop')
  await card.locator('.btn-extras-toggle').click();await card.locator('.card-extras').waitFor({state:'detached'});assert.equal(await dock.count(),1)
  await page.setViewportSize({width:430,height:1000});await dock.scrollIntoViewIfNeeded();assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth));await screenshot(page,'context-dock-narrow')
  await dock.getByRole('button',{name:'Close active agents',exact:true}).click();await page.locator('#session-records').waitFor({state:'detached'})
  await page.setViewportSize({width:1440,height:1000});await page.locator('#main-content-scroll').evaluate(node=>{node.scrollTop=node.scrollHeight})
  const context=page.locator('.workspace-context-persistent');assert.ok(await context.isVisible());await page.getByRole('searchbox',{name:'Search all entities',exact:true}).focus();assert.ok(await context.evaluate(node=>node.contains(document.activeElement)))
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('audit ledger selection keeps current filters, protected detail and existing diff navigation',{timeout:60000},async()=>{
 const h=await openSessionExplorer(),{page}=h
 const events=h.fixture.tasks.map((task,index)=>({id:'layout-audit-'+index,workspace_id:h.fixture.workspace.id,entity_type:'task',entity_id:task.id,action:'update',old_values:{title:task.title,description:'Earlier task description'},new_values:{title:task.title,description:'Updated task description'},changed_at:'2026-10-25T10:00:00Z',request_id:null,actor_type:'user',actor_id:'fixture-user',actor_label:'Fixture User'}))
 try{
  await page.route('**/api/v1/audit?**',route=>route.fulfill({json:{items:events,has_more:false}}))
  await h.start();await h.idle();await page.getByRole('button',{name:'Audit',exact:true}).click();await page.locator('.audit-entry-row').first().waitFor()
  assert.deepEqual(await page.getByLabel('Entity type',{exact:true}).locator('option').evaluateAll(nodes=>nodes.map(node=>node.value)),['','workspace','project','task','observation','category'])
  assert.deepEqual(await page.getByLabel('Action',{exact:true}).locator('option').evaluateAll(nodes=>nodes.map(node=>node.value)),['','create','update','delete'])
  await page.getByLabel('Since',{exact:true}).fill('2026-10-01');await page.getByLabel('Until',{exact:true}).fill('2026-10-25');await page.getByRole('button',{name:'Apply',exact:true}).click()
  const rows=page.locator('.audit-entry-row');await rows.first().getByRole('button',{name:/Inspect audit event/}).focus();await page.keyboard.press('Enter')
  await page.getByRole('complementary',{name:'Selected audit event'}).waitFor()
  await rows.nth(1).getByRole('button',{name:/Inspect audit event/}).click();await until(async()=>await rows.nth(1).getByRole('button',{name:/Inspect audit event/}).getAttribute('aria-expanded')==='true');assert.equal(await page.locator('.audit-entry-detail').count(),1)
  assert.equal(await page.getByLabel('Since',{exact:true}).inputValue(),'2026-10-01');assert.equal(await page.getByLabel('Until',{exact:true}).inputValue(),'2026-10-25')
  await screenshot(page,'audit-ledger-desktop')
  await rows.nth(1).getByRole('button',{name:/View diff/}).click();await page.locator('#audit-diff-dialog').waitFor();await page.locator('#audit-diff-dialog').getByRole('button',{name:'Close',exact:true}).click()
  assert.equal(await page.locator('.audit-entry-detail').count(),1)
  await page.setViewportSize({width:430,height:1000});await page.locator('.audit-entry-detail').scrollIntoViewIfNeeded();assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth));await screenshot(page,'audit-ledger-narrow')
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('stacked inspector returns to its selected card across a virtualized hierarchy',{timeout:60000},async()=>{
 const h=await openSessionExplorer({linkedCount:80}),{page}=h
 try{
  await page.setViewportSize({width:1000,height:850});await h.start();await h.idle()
  const target=page.locator('#entity-'+h.fixture.projects[0].id),task=page.locator('#entity-'+h.fixture.tasks[0].id)
  await target.locator('.entity-type-project').dblclick();await page.locator('.focused-search-filter-bar').waitFor()
  await task.getByRole('button',{name:/Active Agent/}).click()
  await task.locator('.task-session-dock .session-record').first().waitFor()
  await page.getByPlaceholder('Filter focused view...').focus()
  await page.locator('#main-content-scroll').evaluate(node=>{node.scrollTop=node.scrollHeight;node.dispatchEvent(new Event('scroll'))})
  await page.locator('.task-session-dock').waitFor({state:'detached'})
  const inspector=page.getByRole('complementary',{name:'Selected task context'}),back=inspector.getByRole('button',{name:'Return to selected card',exact:true})
  await back.scrollIntoViewIfNeeded();await until(async()=>await target.evaluate(node=>node.getBoundingClientRect().bottom<document.getElementById('main-content-scroll').getBoundingClientRect().top))
  await screenshot(page,'workspace-inspector-stacked');await back.click()
  await until(()=>target.evaluate(node=>{const box=node.getBoundingClientRect(),host=document.getElementById('main-content-scroll').getBoundingClientRect(),header=document.querySelector('.workspace-context-persistent').getBoundingClientRect();return box.top>=Math.max(host.top,header.bottom)&&box.top<host.bottom}))
  assert.ok(await page.locator('[data-hierarchy-key]').count()<=31)
  assert.equal(await page.locator('#session-records').count(),0,'return navigation must not revive the preview retired by virtual unmount')
  await screenshot(page,'workspace-inspector-return')
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})
