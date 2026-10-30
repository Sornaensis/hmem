import test from 'node:test'
import assert from 'node:assert/strict'
import {openSessionExplorer} from './support/sessions-ux-fixture.mjs'

const until=async action=>{const end=Date.now()+12000;while(Date.now()<end){if(await action())return;await new Promise(resolve=>setTimeout(resolve,25))}throw Error('occurrence presentation did not converge')}
const event=(timestamp,value)=>({type:'event_msg',timestamp,payload:{type:'future_event',result:value}})
const resync=async h=>{const response=h.page.waitForResponse(response=>new URL(response.url()).pathname==='/api/v1/change-stream/resync'&&response.request().method()==='POST');await h.page.evaluate(()=>window.pushHierarchyFrames([{schema_version:1,type:'resync_required'}]));await response;await h.idle()}

test('canonical occurrence is shared by cards detail and dock while group time follows source order',{timeout:60000},async()=>{
 const first='2026-10-30T12:00:00Z',older='2026-10-30T09:00:00Z',later='2026-10-30T15:00:00Z'
 const raw=Buffer.from([event(first,1),{type:'event_msg',timestamp:'2099-01-01T00:00:00Z',payload:{type:'token_count',tokens:42}},event(older,2)].map(value=>JSON.stringify(value)+'\n').join(''))
 const h=await openSessionExplorer({transcriptBytes:raw,lastActivityAt:first}),{page}=h
 const card=()=>page.locator('.session-summary').filter({has:page.locator('a[href*="session='+h.active.id+'"]')})
 const stamp=async value=>until(async()=>await card().locator('.session-latest-activity time').getAttribute('datetime')===value)
 try{
  await h.start();await h.idle();await page.getByRole('button',{name:'Sessions',exact:true}).click();await stamp(first);await card().locator('a').click()
  await until(async()=>await page.locator('.session-detail-header .session-latest-activity time').getAttribute('datetime')===first)
  await until(async()=>await page.locator('.session-activity-group > summary time').getAttribute('datetime')===older)
  Object.assign(h.active,{last_seen_at:new Date().toISOString(),runtime_revision:1,runtime_identity:{model:'transport-only',role:'test'}});await resync(h);await stamp(first)
  assert.equal(await page.locator('.session-detail-header .session-latest-activity time').getAttribute('datetime'),first)
  await page.getByRole('link',{name:'Close session',exact:true}).click();h.active.last_activity_at=later;await h.appendRecords(event(later,3));await stamp(later)
  await h.appendRecords(event(older,4));await stamp(later);await card().locator('a').click()
  await until(async()=>await page.locator('.session-activity-group > summary time').getAttribute('datetime')===older)
  await h.appendRecords(event(undefined,5));await page.locator('.session-activity-group > summary').getByText('Time unavailable',{exact:true}).waitFor();await stamp(later)
  await page.getByRole('button',{name:'Projects',exact:true}).click();const task=page.locator('#entity-'+h.fixture.tasks[0].id);await until(()=>task.locator('.task-sessions-icon').isEnabled());await task.locator('.task-sessions-icon').click()
  await until(async()=>await task.locator('.task-session-choice .session-latest-activity time').first().getAttribute('datetime')===later)
  for(const width of [1440,430]){await page.setViewportSize({width,height:1000});assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth))}
  await page.getByRole('button',{name:'Sessions',exact:true}).click();const close=page.getByRole('link',{name:'Close session',exact:true});if(await close.count())await close.click()
  Object.assign(h.active,{state:'terminated',terminated_at:new Date().toISOString(),final_bytes:h.active.committed_bytes,final_sha256:'a'.repeat(64)});await resync(h);await page.getByRole('button',{name:'Completed',exact:true}).click();await stamp(later)
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('missing or pending occurrence metadata stays unavailable with no transport or lifecycle fallback',{timeout:45000},async()=>{
 const h=await openSessionExplorer({empty:true,lastActivityAt:null}),{page}=h
 try{
  await h.start();await h.idle();await page.getByRole('button',{name:'Sessions',exact:true}).click()
  await page.locator('.session-summary [aria-label="Last activity unavailable"]').waitFor();await page.locator('.session-summary a').click()
  await page.locator('.session-detail-header [aria-label="Last activity unavailable"]').waitFor();assert.equal(await page.locator('.session-latest-activity time').count(),0)
  h.active.last_seen_at=new Date(Date.now()+60000).toISOString();await resync(h);assert.equal(await page.locator('.session-latest-activity time').count(),0)
  delete h.active.last_activity_at;await page.reload();await page.locator('.session-detail-header [aria-label="Last activity unavailable"]').waitFor()
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})
