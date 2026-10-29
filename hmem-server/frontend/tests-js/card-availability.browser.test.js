import assert from 'node:assert/strict'
import test from 'node:test'
import {openSessionExplorer} from './support/sessions-ux-fixture.mjs'
import {hierarchyFixture,openHierarchy} from './hierarchy-fixture.mjs'
const until=async fn=>{const end=Date.now()+12000;while(Date.now()<end){if(await fn())return;await new Promise(resolve=>setTimeout(resolve,25))}throw Error('Card availability did not converge')}
// The shared fixture matches task IDs within URLs; keep them distinct from its workspace ID.
const separateTaskIds=h=>{const ids=new Map(h.fixture.tasks.map(task=>[task.id,task.id.replace(/^10000000/,'11000000')]));for(const task of h.fixture.tasks)task.id=ids.get(task.id);for(const [session,tasks] of h.sessionLinks)h.sessionLinks.set(session,new Set([...tasks].map(id=>ids.get(id)||id)));return h}
const changed=async h=>h.page.evaluate(({workspace,session})=>window.pushHierarchyFrames([{schema_version:1,type:'change',event:{schema_version:1,event_id:'availability-'+crypto.randomUUID(),scope:'workspace',workspace_id:workspace,occurred_at:new Date().toISOString(),transaction:{id:'availability',cause:'rest',request_id:null},actor:{type:'system',id:null},entity:{type:'agent_session',id:session,action:'updated'},invalidations:[{kind:'collection',target:'sessions:'+workspace},{kind:'collection',target:'active_sessions:'+workspace}]}}]),{workspace:h.fixture.workspace.id,session:h.active.id})

test('Evidence remains accessible for unknown, loading, error and partial pages and disables only confirmed absence',{timeout:60000},async()=>{
 const h=separateTaskIds(await openSessionExplorer({savedCount:0})),{page}=h,id=h.fixture.tasks[0].id
 let mode='held',release,arrived=false,reads=0
 const held=new Promise(resolve=>release=resolve)
 await page.route('**/api/v1/**/tasks/'+id+'/sessions?**',async route=>{
  if(new URL(route.request().url()).searchParams.get('state')!=='terminated')return route.fallback()
  reads++;if(mode==='held'){arrived=true;await held}
  if(mode==='error')return route.fulfill({status:503,json:{error:'controlled availability failure'}})
  return route.fulfill({json:{items:mode==='present'?[h.saved]:[],next_cursor:mode==='partial'?{created_at:h.saved.created_at,id:h.saved.id}:null}})
 })
 try{
  await h.start();await h.idle();const card=page.locator('#entity-'+id)
  assert.equal(reads,0,'collapsed cards add no evidence read')
  await card.locator('.btn-extras-toggle').click();await until(()=>arrived)
  const evidence=card.getByRole('button',{name:'Evidence',exact:true}),work=card.getByRole('button',{name:'Work',exact:true})
  assert.equal(await evidence.isDisabled(),false);await evidence.click();await until(async()=>await evidence.getAttribute('aria-pressed')==='true')
  mode='empty';release();await until(()=>evidence.isDisabled());assert.equal(await work.getAttribute('aria-pressed'),'true');assert.equal(await evidence.getAttribute('aria-pressed'),'false')
  assert.ok(Number(await evidence.evaluate(node=>getComputedStyle(node).opacity))<1)
  mode='error';await changed(h);await until(()=>evidence.isEnabled());await evidence.click();await card.locator('.task-session-evidence p').filter({hasText:'Sessions could not be loaded.'}).waitFor();assert.equal(await evidence.isDisabled(),false)
  mode='partial';await changed(h);await card.getByRole('button',{name:'Older sessions',exact:true}).waitFor();assert.equal(await evidence.isDisabled(),false)
  h.allSessions.push(h.saved);h.content.set(h.saved.id,h.bytes);h.sessionLinks.set(h.saved.id,new Set([id]));mode='present';await changed(h)
  await card.locator('.task-session-evidence .session-card').waitFor();assert.equal(await evidence.isDisabled(),false);await card.getByRole('button',{name:/Download session/}).waitFor()
  assert.deepEqual(h.errors,[]);assert.deepEqual(h.unhandled,[])
 }catch(error){throw new Error(error.message+' '+JSON.stringify({mode,reads,text:(await page.locator('#entity-'+id).innerText()).slice(-1000),queries:h.reads.slice(-12)}),{cause:error})}finally{release();await h.close()}
})

test('session icon is omitted only for current complete absence while active and terminated associations retain controls',{timeout:60000},async()=>{
 const h=separateTaskIds(await openSessionExplorer({linkedCount:3})),{page}=h,none=h.fixture.tasks[2].id
 h.sessionLinks.set(h.active.id,new Set([h.fixture.tasks[0].id]));h.sessionLinks.set(h.saved.id,new Set([h.fixture.tasks[1].id]))
 let mode='held',release,arrived=false
 const held=new Promise(resolve=>release=resolve)
 await page.route('**/api/v1/**/tasks/'+none+'/sessions?**',async route=>{
  if(new URL(route.request().url()).searchParams.get('state'))return route.fallback()
  if(mode==='held'){arrived=true;await held}
  if(mode==='error')return route.fulfill({status:503,json:{error:'controlled association failure'}})
  return route.fulfill({json:{items:[],next_cursor:mode==='partial'?{created_at:h.saved.created_at,id:h.saved.id}:null}})
 })
 try{
  await h.start();await h.scrollTo('task:'+none);await until(()=>arrived);const absent=page.locator('#entity-'+none+' .task-sessions-icon')
  assert.equal(await absent.count(),1);assert.equal(await absent.isDisabled(),true)
  mode='partial';release();await h.idle();assert.equal(await absent.count(),1)
  mode='error';await changed(h);await h.idle();assert.equal(await absent.count(),1)
  mode='empty';await changed(h);await until(async()=>await absent.count()===0)
  await h.scrollTo('task:'+h.fixture.tasks[0].id);assert.equal(await page.locator('#entity-'+h.fixture.tasks[0].id+' .task-sessions-icon .session-activity-light').count(),1)
  await h.scrollTo('task:'+h.fixture.tasks[1].id)
  const inactive=page.locator('#entity-'+h.fixture.tasks[1].id+' .task-sessions-icon');assert.equal(await inactive.isEnabled(),true);assert.equal(await inactive.locator('.session-state-inactive').count(),1)
  assert.deepEqual(h.errors,[]);assert.deepEqual(h.unhandled,[])
 }finally{release();await h.close()}
})

test('touch Details has a visible label and native target and moves beside tabs after expansion',{timeout:30000},async()=>{
 const f=hierarchyFixture(),h=await openHierarchy(f,[],null,{hasTouch:true,isMobile:true,viewport:{width:430,height:1000}}),{page}=h
 try{
  await h.start();await h.idle();const row=await h.scrollTo('task:'+f.tasks[0].id),control=row.locator('.card-details-control').first()
  assert.equal(await page.evaluate(()=>matchMedia('(pointer: coarse)').matches),true);assert.equal(await control.locator('.card-details-label').isVisible(),true);assert.ok((await control.boundingBox()).height>=44)
  await control.tap();await until(async()=>await control.getAttribute('aria-expanded')==='true');assert.equal(await control.innerText(),'Hide Details');assert.ok(await control.evaluate(node=>node.parentElement.classList.contains('card-details-toolbar')))
  assert.deepEqual(h.errors,[]);assert.deepEqual(h.unhandled,[])
 }finally{await h.close()}
})
