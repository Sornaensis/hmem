import assert from 'node:assert/strict'
import test from 'node:test'
import { hierarchyFixture, openHierarchy } from './hierarchy-fixture.mjs'

function fixture() {
  const f = hierarchyFixture(), t = f.tasks[0]
  f.projects = [f.projects[0]]
  f.tasks = [
    ...Array.from({length: 207}, (_, i) => ({...t, id: 'ready-'+i, title: 'Ready '+i})),
    ...Array.from({length: 204}, (_, i) => ({...t, id: 'blocked-'+i, title: 'Blocked '+i, status:'blocked', auto_blocked:false})),
    {...t,id:'parent',title:'Blocked parent',status:'blocked',auto_blocked:false},
    {...t,id:'child',title:'Parent gated child',parent_id:'parent'},
    {...t,id:'dependent',title:'Dependent'},
    {...t,id:'closed-done',title:'Closed done',status:'done'},
    {...t,id:'closed-cancelled',title:'Closed cancelled',status:'cancelled'},
  ]
  f.dependencies = ['ready-0','closed-done','closed-cancelled'].map(id => ({task_id:'dependent',depends_on_id:id}))
  return f
}

async function assertDetails(page, control, expanded) {
  assert.equal(await control.getAttribute('type'), 'button')
  assert.equal(await control.getAttribute('aria-expanded'), String(expanded))
  const label = await control.getAttribute('aria-label')
  assert.match(label, expanded ? /^Hide details for / : /^Show details for /)
  assert.equal(await control.getAttribute('title'), label)
  const region = page.locator('[id=' + JSON.stringify(await control.getAttribute('aria-controls')) + ']')
  assert.equal(await region.count(), 1)
  assert.equal(await region.isVisible(), expanded)
  assert.equal(await control.locator('svg[aria-hidden="true"][focusable="false"]').count(), 1)
  assert.equal(await control.locator('svg path').getAttribute('d'), expanded ? 'M7 4 L3 8 L7 12 M12 4 L8 8 L12 12' : 'M4 4 L8 8 L4 12 M9 4 L13 8 L9 12')
  const box = await control.boundingBox()
  assert.ok(box.width>=32); assert.ok(box.height>=32)
}

for (const width of [1440, 430]) test(`native card details keyboard and unique regions at ${width}px`, {timeout:60000}, async () => {
  const f = hierarchyFixture()
  f.projects = [f.projects[0], {...f.projects[1], id:'nested-project', parent_id:f.projects[0].id, name:'Nested project'}]
  f.tasks = [{...f.tasks[0], id:'parent', title:'Parent'}, {...f.tasks[0], id:'child', parent_id:'parent', title:'Child'}]
  const h = await openHierarchy(f)
  try {
    await h.page.setViewportSize({width,height:1000})
    await h.start(); await h.idle()
    for (const key of ['project:root-project','project:nested-project','task:parent','task:child']) {
      const row = await h.scrollTo(key), control = row.locator('.btn-extras-toggle').first()
      await assertDetails(h.page,control,false)
      assert.ok(await control.evaluate(node=>node.parentElement.classList.contains('card-inline-actions')))
      await h.page.mouse.move(0,0);await control.evaluate(node=>node.blur());assert.equal(await control.locator('.card-details-label').isVisible(),false)
      await control.hover();assert.equal(await control.locator('.card-details-label').isVisible(),true)
      await h.page.mouse.move(0,0);await control.focus();assert.equal(await control.locator('.card-details-label').isVisible(),true)
      await control.focus(); await h.page.keyboard.press('Enter'); await h.idle()
      await assertDetails(h.page,control,true)
      assert.equal(await control.innerText(),'Hide Details');assert.ok(await control.evaluate(node=>node.parentElement.classList.contains('card-details-toolbar')))
      assert.equal(await control.evaluate(node=>node===document.activeElement),true)
      await control.focus(); await h.page.keyboard.press('Space'); await h.idle()
      await assertDetails(h.page,control,false)
      assert.ok(await control.evaluate(node=>node===document.activeElement))
    }
    assert.deepEqual(h.errors,[]); assert.deepEqual(h.unhandled,[])
  } finally {await h.close()}
})

test('complete next work and blocked disclosure exceed both old limits and are independent of closed dependencies', {timeout:60000}, async () => {
  const h = await openHierarchy(fixture())
  try {
    await h.start(); await h.idle()
    await h.page.locator('#entity-root-project .btn-extras-toggle').first().click(); await h.idle()
    const panel = h.page.locator('#entity-root-project .project-next-tasks-section')
    assert.equal(await panel.locator(':scope > .project-next-tasks-list .next-task-candidate').count(),207)
    const blocked = panel.locator('details')
    assert.equal(await blocked.getAttribute('open'),null)
    assert.match(await blocked.locator('summary').innerText(),/207 blocked tasks\/subtasks/i)
    assert.equal(await blocked.locator('.next-task-candidate').first().isVisible(),false)
    await blocked.locator('summary').focus(); await h.page.keyboard.press('Enter')
    assert.equal(await blocked.getAttribute('open'),'')
    assert.match(await blocked.locator('.next-task-candidate').filter({hasText:'Parent gated child'}).innerText(),/Waiting for parent blockers/)
    const pages = h.requests.filter(r=>r.path.endsWith('/next-task-page'))
    assert.ok(pages.some(r=>!r.includeBlocked && r.offset===200))
    assert.ok(pages.some(r=>r.includeBlocked && r.offset===400))
    await h.page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=projects&focus=task:dependent'); await h.idle()
    await h.page.locator('#entity-dependent .btn-extras-toggle').first().click(); await h.idle()
    const dependencies = h.page.locator('#entity-dependent .task-dependencies-section')
    assert.deepEqual(await dependencies.locator(':scope > .task-dependencies-list .popover-card-title').allTextContents(),['Ready 0'])
    const closed = dependencies.locator('details')
    assert.match(await closed.locator('summary').innerText(),/2 closed dependencies/)
    assert.equal(await closed.getAttribute('open'),null)
    await closed.locator('summary').click()
    assert.deepEqual((await closed.locator('.popover-card-title').allTextContents()).sort(),['Closed cancelled','Closed done'])
    assert.deepEqual(h.errors,[]); assert.deepEqual(h.unhandled,[])
  } finally { await h.close() }
})

test('initial task expansion automatically loads closed dependencies beyond the first 50 open rows', {timeout:30000}, async () => {
  const f=fixture(), t=f.tasks.find(t=>t.id==='dependent')
  f.tasks=[t, ...Array.from({length:55},(_,i)=>({...t,id:'open-'+i,title:'A open '+String(i).padStart(2,'0')})),
    {...t,id:'closed-done',title:'Z done',status:'done'}, {...t,id:'closed-cancelled',title:'Z cancelled',status:'cancelled'}]
  f.dependencies=f.tasks.slice(1).map(t=>({task_id:'dependent',depends_on_id:t.id}))
  const h=await openHierarchy(f)
  h.gate(r=>/^\/api\/v1\/tasks\/closed-done(?:\/overview)?$/.test(r.path),{error:404})
  h.gate(r=>/^\/api\/v1\/tasks\/closed-cancelled(?:\/overview)?$/.test(r.path),{error:404})
  try {
    await h.start('tab=projects&focus=task:dependent','dependent'); await h.idle()
    await h.page.locator('#entity-dependent .btn-extras-toggle').first().click(); await h.idle()
    const section=h.page.locator('#entity-dependent .task-dependencies-section')
    assert.equal(await section.locator(':scope > .task-dependencies-list .dep-item').count(),55)
    const disclosure=section.locator('details')
    assert.match(await disclosure.locator('summary').innerText(),/2 closed dependencies/)
    assert.equal(await disclosure.getAttribute('open'),null)
    assert.equal(await section.getByRole('button',{name:'Load more dependencies'}).count(),0)
    assert.equal(h.requests.filter(r=>r.path.endsWith('/dependent/dependencies')).length,2)
    await disclosure.locator('summary').click()
    assert.deepEqual(await disclosure.locator('.popover-card-title').allTextContents(),['Z cancelled','Z done'])
    assert.deepEqual(h.errors,[]); assert.deepEqual(h.unhandled,[])
  } finally { await h.close() }
})
