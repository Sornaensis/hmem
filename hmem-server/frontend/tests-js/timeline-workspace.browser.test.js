import assert from 'node:assert/strict'
import test from 'node:test'
import {hierarchyFixture,openHierarchy} from './hierarchy-fixture.mjs'
const until=async fn=>{const end=Date.now()+12000;while(Date.now()<end){if(await fn())return;await new Promise(r=>setTimeout(r,25))}throw Error('timeline layout did not settle')}
const paint=page=>page.evaluate(()=>new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve))))

test('workspace plot fills and resizes its own container independently of event cards',{timeout:45000},async()=>{
 const f=hierarchyFixture(),h=await openHierarchy(f),{page}=h
 const zero={created:0,completed:0,deleted:0,archived:0,cancelled:0},legacy={created:1,completed:1,cancelled:1}
 const events=['created','completed'].map((action,index)=>({id:'plot-event-'+index,workspace_id:f.workspace.id,event_type:'task_'+action,entity_type:'task',entity_id:f.tasks[0].id,title:'Timeline task '+action,occurred_at:'2026-01-02T12:00:00Z',navigation:{entity_type:'task',entity_id:f.tasks[0].id}}))
 f.timelineEvents=events
 try{
  await page.clock.setFixedTime(new Date('2026-01-04T12:00:00Z'))
  await page.route('**/timeline/buckets?*',route=>{
   const u=new URL(route.request().url()),action={...zero,created:2,completed:1,deleted:1,archived:1,cancelled:1}
   const buckets=[1,2,3].map(day=>({bucket_start:`2026-01-0${day}T00:00:00Z`,bucket_end:`2026-01-0${day+1}T00:00:00Z`,label:'Controlled presentation interval',counts:{project:legacy,subproject:legacy,task:legacy,subtask:legacy},totals:legacy,series:{project:action,task:action,subtask:action,observation:action},series_totals:action,open_counts:{projects:2,tasks:4},open_as_of:`2026-01-0${day+1}T00:00:00Z`,open_status:'available',observation_total:3,observation_as_of:`2026-01-0${day+1}T00:00:00Z`,observation_status:'available'}))
   return route.fulfill({json:{workspace_id:f.workspace.id,since:u.searchParams.get('since')||'',until:u.searchParams.get('until'),bucket:u.searchParams.get('bucket'),buckets,next_since:null}})
  })
  await page.setViewportSize({width:2560,height:1100});await h.start();await h.idle();await page.getByRole('button',{name:'Timeline',exact:true}).click()
  await page.locator('.timeline-line-chart').waitFor();await page.locator('.timeline-event-card').first().waitFor()
  const metrics=()=>page.evaluate(()=>{
   const width=s=>document.querySelector(s).getBoundingClientRect().width
   return {panel:width('.timeline-panel'),plot:width('.timeline-plot-container'),scroll:width('.timeline-svg-scroll'),svg:width('.timeline-line-chart'),cards:width('.timeline-event-list'),card:width('.timeline-event-card'),body:width('.timeline-events-section'),viewport:innerWidth,outer:document.documentElement.scrollWidth}
  })
  const wide=await metrics();assert.ok(wide.plot>2000,JSON.stringify(wide));assert.ok(Math.abs(wide.svg-wide.scroll)<1);assert.ok(wide.cards<=960)
  const controls=await page.locator('.timeline-plot-container :is(select,button)').allTextContents(),series=await page.locator('.timeline-line').count()
  // Resize only the production plot container; the window and event cards stay put.
  await page.locator('.timeline-plot-container').evaluate(n=>{n.style.width='1000px'})
  await paint(page);const contracted=await metrics()
  assert.equal(contracted.viewport,wide.viewport);assert.ok(contracted.svg<wide.svg);assert.ok(Math.abs(contracted.svg-contracted.scroll)<1);assert.equal(contracted.card,wide.card);assert.equal(contracted.cards,wide.cards)
  await page.locator('.timeline-plot-container').evaluate(n=>{n.style.removeProperty('width')});await paint(page)
  assert.ok(Math.abs((await metrics()).svg-wide.svg)<1)
  for(const [width,zoom] of [[1440,1],[430,1],[1440,1.5],[2560,1]]){
   await page.setViewportSize({width,height:1100});await page.evaluate(z=>{document.body.style.zoom=String(z)},zoom);await paint(page)
   const m=await metrics();assert.ok(m.outer<=m.viewport,JSON.stringify(m));assert.ok(m.svg>=m.scroll-1);assert.ok(Math.abs(m.plot-m.panel)<3)
   assert.deepEqual(await page.locator('.timeline-plot-container :is(select,button)').allTextContents(),controls);assert.equal(await page.locator('.timeline-line').count(),series)
   const point=page.locator('#timeline-point-lifecycle-created-1');await point.focus();await page.locator('.timeline-chart-tooltip').waitFor();await page.keyboard.press('Enter')
   await page.locator('.timeline-selection-note').waitFor();await page.getByRole('button',{name:'Reset range',exact:true}).click();await page.locator('.timeline-selection-note').waitFor({state:'detached'})
  }
  const entities=page.locator('[aria-label="Lifecycle line entities"]')
  await entities.getByRole('button',{name:'Observations',exact:true}).click();await until(async()=>await page.locator('.timeline-series-observations.timeline-line').count()===0)
  await entities.getByRole('button',{name:'Observations',exact:true}).click();await until(async()=>await page.locator('.timeline-line').count()===series)
  await page.locator('.timeline-events-section').getByRole('button',{name:'Completed',exact:true}).click();await until(async()=>await page.locator('.timeline-event-card').count()===1)
  assert.ok((await page.locator('.timeline-event-card').innerText()).includes('Timeline task completed'))
  await page.locator('.timeline-graph-control select').nth(0).selectOption('14');await page.locator('.timeline-graph-control select').nth(1).selectOption('day');await page.locator('.timeline-line-chart').waitFor()
  await page.locator('.timeline-event-card').click();await page.locator('#entity-'+f.tasks[0].id).waitFor();assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})
