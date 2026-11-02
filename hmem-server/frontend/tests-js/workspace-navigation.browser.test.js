import assert from 'node:assert/strict'
import test from 'node:test'
import { createHash } from 'node:crypto'
import { readFile } from 'node:fs/promises'
import { hierarchyFixture, openHierarchy } from './hierarchy-fixture.mjs'

const tabs = h => h.page.locator('.workspace-tabs')
const section = (h,name) => tabs(h).getByRole('button',{name,exact:true})

test('initial Search, soft tabs and always-visible hierarchy filters retain independent state at desktop and narrow widths', {timeout:60000}, async()=>{
  const fixture=hierarchyFixture(); fixture.workspace.name='A long workspace name that wraps without hiding workspace controls'
  const h=await openHierarchy(fixture)
  try {
    await h.page.addInitScript(id=>localStorage.setItem('hmem-ws-'+id,JSON.stringify({workspaceId:id,searchSectionExpanded:false,searchQuery:'root',filterShowOnly:'all'})),fixture.workspace.id)
    await h.page.goto(h.origin+'/workspace/'+fixture.workspace.id+'#tab=search');await section(h,'Search').waitFor();await h.idle()
    assert.equal(await section(h,'Search').getAttribute('aria-current'),'page')
    assert.deepEqual((await tabs(h).getByRole('button').allTextContents()).slice(0,6),['Projects','Sessions','Observations','Timeline','Search','Audit'])
    assert.equal(await h.page.getByRole('group',{name:'Search Entities',exact:true}).getByRole('button').count(),3)
    assert.equal(await h.page.locator('.hierarchy-search-input').count(),0)
    assert.equal(createHash('sha256').update(await readFile(new URL('../public/hmem-logo.svg',import.meta.url))).digest('hex'),'6b07f1af02d677188064a57ace9a0b57809fb3674923cbb87de60a805da8c9f5')
    await section(h,'Projects').click();await h.idle()
    assert.equal(await h.page.locator('.hierarchy-search-input').inputValue(),'root')
    assert.equal(await h.page.locator('.search-section-toggle').count(),0)
    for(const width of [1440,390]) {
      await h.page.setViewportSize({width,height:850})
      await h.page.locator('#main-content-scroll').evaluate(element=>{element.scrollTop=0});await h.page.evaluate(()=>new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve))))
      const geometry=await h.page.evaluate(()=>{const logo=document.querySelector('.global-logo').getBoundingClientRect(),image=document.querySelector('.global-logo img').getBoundingClientRect();return{logo:logo.toJSON(),image:image.toJSON(),overflow:document.documentElement.scrollWidth>innerWidth}})
      assert.ok(Math.abs((geometry.logo.left+geometry.logo.right)/2-(geometry.image.left+geometry.image.right)/2)<=1,JSON.stringify(geometry))
      assert.equal(geometry.overflow,false)
      await h.page.screenshot({path:'../../output/workspace-navigation-f3a34ca9/navigation-'+width+'.png'})
      assert.equal(await h.page.locator('.titlebar-breadcrumbs').count(),1)
      assert.equal(await h.page.locator('#main-content-scroll .focus-breadcrumb-bar, .popover-card-breadcrumb, .audit-return-bar').count(),0)
      const filters=await h.page.locator('.project-filter-panel button,.project-filter-panel select').evaluateAll(values=>values.map(value=>{const r=value.getBoundingClientRect(),p=value.closest('.project-filter-panel').getBoundingClientRect();return{shown:r.width>0&&r.height>0,fit:r.left>=p.left-1&&r.right<=p.right+1}}))
      assert.ok(filters.length>=15&&filters.every(value=>value.shown&&value.fit),JSON.stringify({width,filters}))
      await section(h,'Search').focus();assert.equal(await section(h,'Search').evaluate(element=>element===document.activeElement),true)
      await h.page.keyboard.press('Enter');assert.equal(await h.page.locator('.search-input').inputValue(),'')
      await section(h,'Projects').click();assert.equal(await h.page.locator('.hierarchy-search-input').inputValue(),'root')
    }
    await h.page.emulateMedia({reducedMotion:'reduce'})
    assert.deepEqual(h.errors,[]);assert.deepEqual(h.unhandled,[])
  }finally{await h.close()}
})

test('next-task pointer and keyboard previews share title navigation without changing canonical focus or history', {timeout:30000}, async()=>{
 const fixture=hierarchyFixture();fixture.projects=fixture.projects.slice(0,1);fixture.tasks=fixture.tasks.slice(0,2)
 const h=await openHierarchy(fixture)
 try{
  await h.start();await h.idle();await h.page.locator('#entity-root-project .btn-extras-toggle').first().click();await h.idle()
  const card=h.page.locator('.next-task-candidate').first();await card.waitFor()
  const before=h.page.url(),requests=h.requests.length
  await card.hover();assert.equal(await h.page.locator('.titlebar-preview-trail').count(),1)
  assert.match(await h.page.locator('.titlebar-preview-trail').innerText(),/root-project/)
  assert.equal(await h.page.locator('.popover-card-breadcrumb').count(),0)
  assert.equal(h.page.url(),before);assert.equal(h.requests.length,requests)
  await h.page.locator('.global-logo').hover();assert.equal(await h.page.locator('.titlebar-preview-trail').count(),0)
  await card.focus();await h.page.locator('.titlebar-preview-trail').waitFor();assert.equal(await h.page.locator('.titlebar-preview-trail').count(),1)
  await section(h,'Search').click();await h.page.locator('.titlebar-preview-trail').waitFor({state:'detached'});assert.equal(await h.page.locator('.titlebar-preview-trail').count(),0)
  assert.deepEqual(h.errors,[]);assert.deepEqual(h.unhandled,[])
 }finally{await h.close()}
})


test('long focused and preview breadcrumbs wrap inside the narrow title bar', {timeout:30000}, async()=>{
  const fixture=hierarchyFixture();fixture.projects=fixture.projects.slice(0,1);fixture.tasks=fixture.tasks.slice(0,1)
  fixture.projects[0].name='Project-'+ 'LongUnbroken'.repeat(25);fixture.tasks[0].title='Task-'+ 'LongUnbroken'.repeat(25)
  const h=await openHierarchy(fixture)
  const fits=async selector=>{
    await h.page.evaluate(()=>new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve))))
    const geometry=await h.page.locator(selector).evaluateAll(elements=>elements.map(element=>{const r=element.getBoundingClientRect(),owner=element.closest('.titlebar-breadcrumbs').getBoundingClientRect();return{left:r.left,right:r.right,ownerLeft:owner.left,ownerRight:owner.right,whiteSpace:getComputedStyle(element).whiteSpace}}))
    assert.ok(geometry.length>=2&&geometry.every(value=>value.left>=value.ownerLeft-1&&value.right<=value.ownerRight+1&&value.whiteSpace==='normal'),JSON.stringify(geometry))
    assert.equal(await h.page.evaluate(()=>document.documentElement.scrollWidth>innerWidth),false)
  }
  try{
    await h.page.setViewportSize({width:390,height:950});await h.start();await h.idle()
    await h.page.locator('#entity-root-project .btn-extras-toggle').first().click();await h.idle()
    await h.page.locator('.next-task-candidate').first().hover();await h.page.locator('.titlebar-preview-trail').waitFor();await fits('.titlebar-preview-trail .focus-crumb')
    await h.page.goto(h.origin+'/workspace/'+fixture.workspace.id+'#tab=projects&focus=task:'+fixture.tasks[0].id)
    await h.page.locator('#entity-'+fixture.tasks[0].id).waitFor();await h.idle();await fits('.titlebar-focus-trail .focus-crumb:not(.focus-crumb-link) , .titlebar-focus-trail .focus-crumb-link')
    assert.deepEqual(h.errors,[]);assert.deepEqual(h.unhandled,[])
  }finally{await h.close()}
})
