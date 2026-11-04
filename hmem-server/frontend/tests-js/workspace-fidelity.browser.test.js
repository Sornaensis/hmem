import assert from 'node:assert/strict'
import test from 'node:test'
import {writeFile} from 'node:fs/promises'
import {join} from 'node:path'
import {hierarchyFixture,openHierarchy} from './hierarchy-fixture.mjs'
import {openSessionExplorer} from './support/sessions-ux-fixture.mjs'

const evidence=process.env.HMEM_WORKSPACE_FIDELITY_OUTPUT ?? '../../output/playwright/fidelity-91d9ee2f/'
const settle=page=>page.evaluate(()=>new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve))))

test('hierarchy search and actions share the top row and retain card alignment, icons and filter grouping', {timeout:45000}, async()=>{
 const fixture=hierarchyFixture();fixture.workspace.name='Treacletart Discord Bot';fixture.projects=fixture.projects.slice(0,1);fixture.projects[0].name='WEDL Story Discord Bot';fixture.projects[0].priority=5;fixture.tasks=fixture.tasks.slice(0,3)
 const h=await openHierarchy(fixture)
 await h.page.route(url=>url.pathname==='/api/v1/session',route=>route.fulfill({contentType:'application/json',body:JSON.stringify({auth_mode:'local',principal:{actor_type:'user',actor_id:'fixture-user',actor_label:'Fixture User',authority:'local',grant_user_id:null},global_permissions:{create_workspace:false,superadmin:true},workspace:{workspace_id:fixture.workspace.id,role:'owner',can_read:true,can_edit:true,can_admin:true}})}))
 const groups=['AI Tool','Story Automation'].map((name,index)=>({id:'fidelity-group-'+index,name,created_at:fixture.workspace.created_at,updated_at:fixture.workspace.updated_at}))
 await h.page.route('**/api/v1/change-stream/resync',route=>route.request().postDataJSON().scope.scope==='global'?route.fulfill({json:{snapshot_profile:'full_v1',items:[{schema_version:1,kind:'workspace',data:fixture.workspace},...groups.map(data=>({schema_version:1,kind:'workspace_group',data}))],has_more:false,resume_token:'fidelity-global'}}):route.fallback())
 await h.page.route('**/api/v1/groups**',route=>route.fulfill({contentType:'application/json',body:JSON.stringify(new URL(route.request().url()).pathname.endsWith('/members')?[fixture.workspace.id]:{items:groups,has_more:false})}))
 await h.page.route('**/api/v1/search',route=>route.fulfill({contentType:'application/json',body:JSON.stringify({projects:fixture.projects,tasks:[],observations:[]})}))
 try{
  await h.start();await h.idle()
  await h.page.locator('.sidebar-catalog .sidebar-group-title').first().waitFor()
  const dividers=await h.page.locator('.sidebar-catalog > .sidebar-section').evaluateAll(elements=>elements.map(element=>({top:getComputedStyle(element).borderTopWidth,bottom:getComputedStyle(element).borderBottomWidth})))
  assert.ok(dividers.length>=2);assert.ok(dividers.every(value=>value.bottom==='0px'));assert.ok(dividers.slice(1).every(value=>value.top==='1px'))
  assert.deepEqual(await h.page.locator('.workspace-tabs .tab > svg').evaluateAll(values=>values.map(value=>value.dataset.lucide)).then(values=>values.slice(0,6)),['folder-kanban','terminal','scan-eye','history','search','shield-check'])
  assert.deepEqual(await h.page.locator('.hierarchy-filter-controls .filter-label').allTextContents(),['Show','Project','Priority','Task','Options'])
  assert.equal(await h.page.getByRole('heading',{name:'Project hierarchy',exact:true}).count(),0)
  assert.equal(await h.page.locator('.workspace-tab-count').innerText(),'1')
  const details=h.page.getByRole('button',{name:'Details',exact:true}),before=h.page.url()
  assert.equal(await details.getAttribute('aria-expanded'),'true')
  await details.click();await h.page.locator('#workspace-metadata').waitFor({state:'hidden'})
  assert.equal(h.page.url(),before)
  await details.click();await h.page.locator('#workspace-metadata').waitFor({state:'visible'})
  const measurements=[]
  for(const width of [1920,635,390]){
   await h.page.setViewportSize({width,height:1100})
   await h.page.locator('#main-content-scroll').evaluate(element=>{element.scrollTop=0});await settle(h.page)
   const geometry=await h.page.evaluate(()=>{
    const box=selector=>document.querySelector(selector).getBoundingClientRect().toJSON()
    const css=selector=>{const value=getComputedStyle(document.querySelector(selector));return{background:value.backgroundColor,fontSize:value.fontSize,padding:value.padding,borderStyle:value.borderStyle}}
    const toolbar=document.querySelector('.hierarchy-toolbar')
    const filters=[...document.querySelectorAll('.hierarchy-filter-controls .filter-group')].map(group=>group.getBoundingClientRect().toJSON())
    const priority=document.querySelector('.hierarchy-priority-mode')
    return{width:innerWidth,toolbar:toolbar.getBoundingClientRect().toJSON(),search:box('.hierarchy-toolbar .search-bar'),
      actions:[...toolbar.querySelectorAll('.tree-toolbar button')].map(action=>action.getBoundingClientRect().toJSON()),
      filters,priority:{label:priority.selectedOptions[0].textContent,align:getComputedStyle(priority).textAlignLast},
      panel:box('.project-filter-panel'),card:box('#entity-root-project'),sidebar:box('.sidebar'),create:css('.btn-inline-create-top'),
      groupGap:getComputedStyle(document.querySelector('.hierarchy-filter-controls>.filter-bar')).gap,chip:css('.project-filter-panel .filter-pill'),
      icons:[...toolbar.querySelectorAll('svg')].map(svg=>({name:svg.dataset.lucide,width:svg.getBoundingClientRect().width,stroke:svg.getAttribute('stroke-width')})),
      overflow:document.documentElement.scrollWidth>innerWidth}
   })
   for(const boundary of ['left','right'])assert.ok(Math.abs(geometry.panel[boundary]-geometry.card[boundary])<=1,JSON.stringify(geometry))
   assert.ok(Math.abs(geometry.card.top-geometry.panel.bottom-16)<=1,JSON.stringify(geometry))
   const {toolbar,search,actions,filters,priority}=geometry
   const [expand,collapse,create]=actions
   const centerY=box=>(box.top+box.bottom)/2
   assert.ok(Math.abs(search.left-toolbar.left)<=1&&Math.abs(create.right-toolbar.right)<=1,JSON.stringify(geometry))
   assert.ok(expand.left>=toolbar.left-1&&expand.right<=collapse.left+1&&collapse.right<=create.left+1,JSON.stringify(geometry))
   if(width===1920){
    assert.ok(actions.every(action=>Math.abs(centerY(search)-centerY(action))<=1),JSON.stringify(geometry))
    assert.ok(search.width>create.right-expand.left&&search.right<=expand.left,JSON.stringify(geometry))
   }else{
    assert.ok(search.bottom<=expand.top+1,JSON.stringify(geometry))
   }
   assert.ok(filters.slice(0,-1).every(filter=>filter.top>=toolbar.bottom&&filter.bottom<=filters.at(-1).top),JSON.stringify(geometry))
   assert.deepEqual(priority,{label:'Any',align:'center'})
   assert.equal(geometry.create.background,'rgb(99, 102, 241)');assert.equal(geometry.create.borderStyle,'solid')
   const seam=await h.page.locator('.hierarchy-action-pair').evaluate(element=>{const [a,b]=element.children,first=getComputedStyle(a),second=getComputedStyle(b);return{gap:b.getBoundingClientRect().left-a.getBoundingClientRect().right,top:first.borderTopRightRadius,bottom:first.borderBottomRightRadius,nextTop:second.borderTopLeftRadius,nextBottom:second.borderBottomLeftRadius,border:second.borderLeftWidth}})
   assert.deepEqual(seam,{gap:0,top:'0px',bottom:'0px',nextTop:'0px',nextBottom:'0px',border:'0px'})
   assert.equal(geometry.chip.fontSize,'11px');assert.equal(geometry.chip.padding,'5px 8px');assert.equal(geometry.groupGap,'10px 20px')
   assert.deepEqual(geometry.icons.map(icon=>icon.name),['search','unfold-vertical','fold-vertical','plus'])
   assert.ok(geometry.icons.every(icon=>icon.width===13&&icon.stroke==='1.6'),JSON.stringify(geometry.icons))
   assert.equal(geometry.overflow,false)
   if(width===1920)assert.equal(geometry.sidebar.width,224)
   else if(width===635){assert.equal(geometry.sidebar.width,180);assert.equal(await h.page.locator('.sidebar-link.active:visible').count(),2)}
   else{assert.equal(await h.page.locator('.sidebar-catalog .sidebar-section:visible').count(),0);assert.equal(await h.page.locator('.sidebar-catalog a:visible,.sidebar-catalog button:visible').count(),0);assert.equal(await h.page.getByRole('link',{name:'Workspaces',exact:true}).isVisible(),true);assert.equal(await h.page.getByRole('link',{name:'Audit Log',exact:true}).isVisible(),true)}
   if(width!==635)await h.page.screenshot({path:join(evidence,'fixture-projects-'+width+'.png')});measurements.push(geometry)
   await h.page.locator('.workspace-tabs').getByRole('button',{name:'Search',exact:true}).click();await settle(h.page)
   const entities=h.page.getByRole('group',{name:'Search Entities',exact:true})
   for(const name of ['Projects','Tasks','Observations']){const control=entities.getByRole('button',{name,exact:true});if(await control.getAttribute('aria-pressed')!=='true')await control.click()}
   await h.page.locator('.search-input').fill('WEDL');await h.page.locator('.search-result-card').first().waitFor();await h.idle()
   await h.page.locator('.workspace-search-page h2').click()
   await h.page.locator('.workspace-tabs').getByRole('button',{name:'Search',exact:true}).hover()
   await h.page.waitForFunction(()=>getComputedStyle([...document.querySelectorAll('.workspace-tabs .tab')].find(element=>element.getAttribute('aria-label')==='Projects')).borderTopColor==='rgba(0, 0, 0, 0)')
   const selected=await h.page.locator('.workspace-tabs .tab').evaluateAll(elements=>elements.map(element=>({name:element.getAttribute('aria-label'),active:element.classList.contains('active'),background:getComputedStyle(element).backgroundColor})))
   assert.deepEqual(selected.filter(value=>value.active).map(value=>value.name),['Search'])
   assert.equal(selected.find(value=>value.name==='Search').background,'rgb(48, 51, 82)')
   const entity=await h.page.locator('.search-entities-filter-bar').evaluate(element=>({border:getComputedStyle(element).borderTopWidth,background:getComputedStyle(element).backgroundColor}))
   assert.equal(entity.border,'0px');assert.equal(entity.background,'rgba(0, 0, 0, 0)')
   if(width!==635)await h.page.screenshot({path:join(evidence,'fixture-search-'+width+'.png')})
   await h.page.locator('.workspace-tabs').getByRole('button',{name:'Projects',exact:true}).click();await h.idle()
  }
  await writeFile(join(evidence,'fixture-geometry.json'),JSON.stringify(measurements,null,2))
  assert.deepEqual(h.errors,[]);assert.deepEqual(h.unhandled,[])
 }finally{await h.close()}
})

test('control-column sizing leaves the existing wide task agent dock canvas intact', {timeout:30000}, async()=>{
 const h=await openSessionExplorer()
 try{
  await h.page.setViewportSize({width:1920,height:1100});await h.start()
  const owner=h.page.locator('#entity-'+h.fixture.tasks[0].id)
  await owner.locator(':scope > .card-header .task-sessions-icon').click()
  await h.page.getByRole('heading',{name:'A calmer session explorer'}).waitFor();await h.idle()
  const geometry=await owner.evaluate(element=>{const card=element.getBoundingClientRect(),controls=document.querySelector('.project-filter-panel').getBoundingClientRect();return{cardWidth:card.width,controlWidth:controls.width,right:card.right,viewport:innerWidth,overflow:document.documentElement.scrollWidth>innerWidth}})
  assert.ok(geometry.cardWidth>geometry.controlWidth+100,JSON.stringify(geometry));assert.equal(geometry.controlWidth,1248)
  assert.ok(geometry.right<=geometry.viewport);assert.equal(geometry.overflow,false)
 }finally{await h.close()}
})

test('coarse-pointer tabs and hierarchy actions keep44px targets while glyphs stay contextual', {timeout:20000}, async()=>{
 const fixture=hierarchyFixture();fixture.projects=fixture.projects.slice(0,1);fixture.tasks=[]
 const h=await openHierarchy(fixture,[],null,{viewport:{width:390,height:1100},hasTouch:true,isMobile:true})
 try{
  await h.start();await h.idle()
  assert.equal(await h.page.evaluate(()=>matchMedia('(pointer:coarse)').matches),true)
  assert.equal(await h.page.getByRole('link',{name:'Audit Log',exact:true}).count(),0)
  const sizes=await h.page.locator('.workspace-tabs .tab,.hierarchy-toolbar button,.project-filter-panel .filter-pill').evaluateAll(elements=>elements.map(element=>element.getBoundingClientRect().height))
  assert.ok(sizes.length>=15&&sizes.every(height=>height>=44),JSON.stringify(sizes))
 }finally{await h.close()}
})
