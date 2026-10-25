import assert from 'node:assert/strict'
import test from 'node:test'
import {openSessionExplorer} from './support/sessions-ux-fixture.mjs'
const until=async fn=>{const end=Date.now()+15000;while(Date.now()<end){if(await fn())return;await new Promise(r=>setTimeout(r,25))}throw Error('activity did not converge')}
test('shared activity lights use compact accessible rings on project, task and workspace cards',{timeout:40000},async()=>{
 const h=await openSessionExplorer();let active=true,reads=0
 try{
  await h.page.route('**/api/v1/session-activity?*',async route=>{reads++;const u=new URL(route.request().url());const rows=key=>u.searchParams.getAll(key).map(id=>({id,active}));await route.fulfill({json:{workspaces:rows('workspace_id'),projects:rows('project_id')}})})
  await h.page.setViewportSize({width:1440,height:1200});await h.start();await h.idle()
  const project=h.page.locator('#entity-'+h.fixture.projects[0].id)
  await project.getByRole('img',{name:'Active sessions in this project',exact:true}).waitFor()
  await h.page.locator('.sidebar').getByRole('img',{name:'Active sessions in this workspace',exact:true}).waitFor()
  await h.page.getByRole('button',{name:'Sessions',exact:true}).click()
  await h.page.getByRole('button',{name:'Quiet',exact:true}).click()
  await h.page.locator('.sidebar').getByRole('img',{name:'Active sessions in this workspace',exact:true}).waitFor()
  await h.page.getByRole('button',{name:'Projects',exact:true}).click();await h.idle()
  const task=h.page.locator('#entity-'+h.fixture.tasks[0].id)
  await task.locator('.task-sessions-icon .session-activity-light').waitFor()
  const ring=project.locator(':scope > .card > .card-header .session-activity-ring')
  assert.equal(await ring.evaluate(el=>getComputedStyle(el).animationName),'session-activity-spin')
  const initial=await ring.evaluate(el=>getComputedStyle(el).transform);await new Promise(resolve=>setTimeout(resolve,150));assert.notEqual(await ring.evaluate(el=>getComputedStyle(el).transform),initial)
  await h.page.emulateMedia({reducedMotion:'reduce'});assert.equal(await ring.evaluate(el=>getComputedStyle(el).animationName),'none');await h.page.emulateMedia({reducedMotion:'no-preference'})
  if(process.env.HMEM_ACTIVITY_CAPTURE)await project.screenshot({path:process.env.HMEM_ACTIVITY_CAPTURE+'/after-project-desktop.png'})
  await h.page.setViewportSize({width:430,height:1400});await project.scrollIntoViewIfNeeded();assert.ok(await h.page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth))
  if(process.env.HMEM_ACTIVITY_CAPTURE)await project.screenshot({path:process.env.HMEM_ACTIVITY_CAPTURE+'/after-project-narrow.png'})
  await h.page.goto(h.origin+'/');await h.page.locator('.card.clickable').getByRole('img',{name:'Active sessions in this workspace',exact:true}).waitFor()
  const workspace=h.page.locator('.card.clickable').filter({hasText:h.fixture.workspace.name});const light=workspace.getByRole('img',{name:'Active sessions in this workspace',exact:true})
  assert.ok(await light.evaluate(el=>el.previousElementSibling.classList.contains('badge')))
  if(process.env.HMEM_ACTIVITY_CAPTURE)await workspace.screenshot({path:process.env.HMEM_ACTIVITY_CAPTURE+'/after-workspace-narrow.png'})
  await h.page.setViewportSize({width:1440,height:1000});if(process.env.HMEM_ACTIVITY_CAPTURE)await workspace.screenshot({path:process.env.HMEM_ACTIVITY_CAPTURE+'/after-workspace-desktop.png'})
  active=false;await h.page.evaluate(workspace=>window.pushActivityFrames([{schema_version:1,type:'session_activity_changed',workspace_id:workspace}]),h.fixture.workspace.id);await until(async()=>await light.count()===0)
  assert.ok(reads>=3);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('standalone authorized hints wake canonical-socket clients without a checkpoint or append request storm',{timeout:25000},async()=>{
 const h=await openSessionExplorer();let active=false,reads=0
 try{
  await h.page.route('**/api/v1/session-activity?*',async route=>{reads++;const u=new URL(route.request().url()),rows=key=>u.searchParams.getAll(key).map(id=>({id,active}));await route.fulfill({json:{workspaces:rows('workspace_id'),projects:rows('project_id')}})})
  await h.start();await h.idle();const project=h.page.locator('#entity-'+h.fixture.projects[0].id),light=project.getByRole('img',{name:'Active sessions in this project',exact:true}),resyncs=h.requests.filter(row=>row.path==='/api/v1/change-stream/resync').length
  assert.equal(await light.count(),0);active=true
  await h.page.evaluate(workspace=>window.pushHierarchyRawFrames([{type:'session_activity_changed',schema_version:1,workspace_id:workspace}]),h.fixture.workspace.id)
  await light.waitFor();await new Promise(resolve=>setTimeout(resolve,1200));assert.equal(h.requests.filter(row=>row.path==='/api/v1/change-stream/resync').length,resyncs,'a standalone hint is not an unfinished canonical transaction')
  const stable=reads;await h.appendRecords({text:'Ordinary output does not change lifecycle membership.'});await new Promise(resolve=>setTimeout(resolve,1200));assert.equal(reads,stable);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})