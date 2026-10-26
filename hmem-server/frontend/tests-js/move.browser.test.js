import assert from 'node:assert/strict'
import test from 'node:test'
import { mkdir } from 'node:fs/promises'
import { hierarchyFixture, openHierarchy } from './hierarchy-fixture.mjs'

function fixture() {
  const f = hierarchyFixture(), p = f.projects[0], t = f.tasks[0]
  f.projects = [p,
    {...p,id:'destination',name:'Destination',parent_id:null},
    {...p,id:'closed',name:'Closed',status:'completed',parent_id:null},
    {...p,id:'paused',name:'Paused',status:'paused',parent_id:null},
    {...p,id:'nested',name:'Nested context',parent_id:p.id},
    {...p,id:'needle',name:'Needle project',parent_id:'nested'},
    {...p,id:'description',name:'Description match',description:'needle only in description',parent_id:null},
  ]
  f.tasks = [{...t,id:'parent',title:'Parent'}, {...t,id:'child',title:'Child',parent_id:'parent'}]
  return f
}

function authority(data) {
  return {
    fail:false,
    updateProject(f,id,update) {
      assert.equal(f,data); assert.deepEqual(Object.keys(update).sort(),['parent_id','request_id'])
      const p=data.projects.find(p=>p.id===id)
      if (!this.fail) p.parent_id=update.parent_id
      return structuredClone(p)
    },
    updateTask(f,id,update) {
      assert.equal(f,data); assert.deepEqual(Object.keys(update).sort(),['parent_id','project_id','request_id'])
      const t=data.tasks.find(t=>t.id===id)
      if (!this.fail) {
        t.project_id=update.project_id; t.parent_id=update.parent_id
        for (const child of data.tasks) if (child.parent_id===id && !['done','cancelled'].includes(child.status)) child.project_id=update.project_id
      }
      return {...structuredClone(t),dependency_effects:[]}
    },
    async broadcast() {},
  }
}

const visualDirectory = process.env.HMEM_MOVE_VISUAL_DIR || '.task-scratch/move-dialog'
const dialog = h => h.page.locator('#move-dialog')
async function open(h,kind,id) {
  await h.scrollTo(kind+':'+id)
  await h.page.locator('#move-origin-'+kind+'-'+id).click()
  await dialog(h).waitFor()
  await h.idle()
}
async function choose(h,name) { await dialog(h).locator('.move-project-choice').filter({hasText:name}).click(); await dialog(h).locator('.selected').waitFor() }
const save = h => dialog(h).getByRole('button',{name:'Confirm',exact:true}).click()

function frame(f,kind,id,eventId,requestId='foreign-request',invalidations=[{kind:'entity',target:kind+':'+id},{kind:'tree',target:'workspace:'+f.workspace.id}]) {
  return {schema_version:1,type:'change',event:{schema_version:1,event_id:eventId,scope:'workspace',workspace_id:f.workspace.id,occurred_at:'2026-10-14T12:00:00Z',transaction:{id:eventId,cause:'rest',request_id:requestId},actor:{type:'system',id:null},entity:{type:kind,id,action:'updated'},invalidations}}
}

test('production task move refreshes live subtasks and root placement without unrelated field changes', {timeout:45000}, async()=>{
  const f=fixture(), a=authority(f), h=await openHierarchy(f,[],a)
  try {
    await h.start(); await h.idle(); await h.scrollTo('task:parent')
    await open(h,'task','parent')
    await choose(h,'Destination'); await save(h); await h.idle()
    assert.equal(await dialog(h).count(),0)
    assert.equal(f.tasks.find(t=>t.id==='child').project_id,'destination')
    await h.scrollTo('task:child')
    const childRow=h.page.locator('#entity-child')
    assert.equal(await childRow.count(),1)
    await h.scrollTo('task:parent'); await open(h,'task','parent')
    assert.match(await dialog(h).locator('.move-current').innerText(),/Destination/)
    await choose(h,'No project'); await save(h); await h.idle()
    assert.equal(f.tasks.find(t=>t.id==='parent').project_id,null)
    assert.equal(f.tasks.find(t=>t.id==='child').project_id,null)
    assert.equal(f.tasks.find(t=>t.id==='parent').priority,1)
    assert.equal(f.tasks.find(t=>t.id==='parent').status,'todo')
    assert.ok(h.requests.some(r=>r.path.endsWith('/navigation/summaries')))
  } finally { await h.close() }
})

test('subtask detach is explained and only placement fields are submitted', {timeout:30000}, async()=>{
  const f=fixture(), h=await openHierarchy(f,[],authority(f))
  try {
    await h.start('tab=projects&focus=task:child','child'); await h.idle()
    await open(h,'task','child')
    assert.match(await dialog(h).getAttribute('aria-description'),/detaches this subtask.*top-level task/)
    assert.match(await dialog(h).locator('.move-current').innerText(),/root-project/)
    await choose(h,'Destination'); await h.page.locator('#move-search').press('Enter'); await h.idle()
    assert.equal(h.requests.filter(r=>r.method==='PUT').length,0)
    await save(h); await h.idle()
    const update=h.requests.find(r=>r.method==='PUT').update
    assert.deepEqual(Object.keys(update).sort(),['parent_id','project_id','request_id'])
    assert.equal(update.parent_id,null); assert.equal(update.project_id,'destination')
    await h.scrollTo('task:child')
    assert.equal(f.tasks.find(t=>t.id==='child').parent_id,null)
  } finally { await h.close() }
})

test('project move carries subtree and can return to workspace root; current/self/subtree choices disabled', {timeout:45000}, async()=>{
  const f=fixture(), h=await openHierarchy(f,[],authority(f))
  try {
    await h.start(); await h.idle(); await open(h,'project','root-project')
    assert.equal(await dialog(h).getByRole('button',{name:'Workspace root',exact:true}).isDisabled(),true)
    assert.equal(await dialog(h).locator('.move-project-choice').filter({hasText:'root-project'}).isDisabled(),true)
    assert.equal(await dialog(h).getByRole('button',{name:'Collapse root-project',exact:true}).count(),1)
    assert.equal(await dialog(h).locator('.move-project-choice').filter({hasText:'Nested context'}).isDisabled(),true)
    await choose(h,'Destination'); await save(h); await h.idle()
    assert.equal(f.projects.find(p=>p.id==='root-project').parent_id,'destination')
    assert.equal(f.projects.find(p=>p.id==='nested').parent_id,'root-project')
    await h.scrollTo('project:root-project'); await open(h,'project','root-project')
    await choose(h,'Workspace root')
    if (process.env.HMEM_MOVE_VISUALS === '1') {
      await mkdir(visualDirectory, {recursive:true})
      assert.equal(await dialog(h).locator('.move-tree-guides, .move-project-status').count(),0)
      assert.equal(await dialog(h).locator('[data-project-id] .entity-type-label.entity-type-project').count(),await dialog(h).locator('[data-project-id]').count())
      assert.ok(await dialog(h).locator('[data-project-id] .move-project-meta').evaluateAll(badges=>badges.every(badge=>badge.children.length===1)))
      assert.ok(await dialog(h).locator('[data-project-id] .move-project-name').evaluateAll(names=>names.every(name=>name.title===name.textContent)))
      assert.match(await dialog(h).locator('.move-current .entity-type-label').innerText(),/^PRJ · Current parent$/)
      assert.match(await dialog(h).locator('[data-project-id="root-project"] .entity-type-label').innerText(),/^PRJ · Moving$/)
      assert.match(await dialog(h).locator('[data-project-id="nested"] .entity-type-label').innerText(),/^PRJ · Moves with$/)
      assert.ok(await dialog(h).locator('.move-project-row').evaluateAll(rows=>rows.every(row=>row.getBoundingClientRect().height===56 && row.querySelector('.move-project-card').getBoundingClientRect().height===48)))
      await h.page.screenshot({path:`${visualDirectory}/project-desktop.png`})
      await h.page.setViewportSize({width:390,height:844})
      await h.page.screenshot({path:`${visualDirectory}/project-narrow.png`})
      await h.page.setViewportSize({width:860,height:480})
      const confirmBox=await dialog(h).getByRole('button',{name:'Confirm',exact:true}).boundingBox()
      assert.ok(confirmBox.y>=0 && confirmBox.y+confirmBox.height<=480,'short viewport keeps footer visible')
      await h.page.screenshot({path:`${visualDirectory}/project-short.png`})
      await h.page.setViewportSize({width:1440,height:1000})
    }
    await save(h); await h.idle()
    assert.equal(f.projects.find(p=>p.id==='root-project').parent_id,null)
  } finally { await h.close() }
})

test('search discovers unloaded descendants and description matches, statuses/main state remain independent; keyboard and responsive layout', {timeout:45000}, async()=>{
  const f=fixture(), h=await openHierarchy(f,[],authority(f))
  try {
    await h.start(); await h.idle()
    const mainBefore=await h.page.locator('#entity-root-project .tree-toggle').first().innerText()
    await open(h,'task','parent')
    assert.equal(await h.page.locator('#move-search').evaluate(e=>document.activeElement===e),true)
    assert.equal(await dialog(h).evaluate(e=>e.matches(':modal')),true)
    await h.page.keyboard.press('Shift+Tab')
    assert.equal(await dialog(h).evaluate(e=>e.contains(document.activeElement)),true)
    await h.page.locator('#move-search').fill('needle'); await h.idle()
    assert.equal(await dialog(h).locator('.move-project-choice').filter({hasText:'Description match'}).isDisabled(),false)
    const root=dialog(h).locator('.move-project-choice').filter({hasText:'root-project'})
    assert.equal(await root.isDisabled(),true)
    assert.equal(await dialog(h).getByRole('button',{name:'Collapse root-project',exact:true}).count(),1)
    await dialog(h).getByRole('button',{name:'Expand Nested context',exact:true}).click(); await h.idle()
    await choose(h,'Needle project')
    assert.match(await dialog(h).locator('.move-selected').innerText(),/Needle project.*Selected/s)
    assert.ok(h.requests.some(r=>r.showOnly==='projects' && r.query==='needle' && r.parent==='nested'))
    assert.ok(h.requests.filter(r=>r.showOnly==='projects').every(r=>r.projectStatuses.includes('active') && r.projectStatuses.includes('paused')))
    await h.page.locator('#move-search').fill(''); await h.idle()
    await dialog(h).getByRole('button',{name:'completed',exact:true}).click(); await h.idle()
    const closed=dialog(h).locator('.move-project-choice').filter({hasText:'Closed'})
    assert.equal(await closed.isDisabled(),true)
    if (process.env.HMEM_MOVE_VISUALS === '1') {
      await mkdir(visualDirectory, {recursive:true})
      await h.page.screenshot({path:`${visualDirectory}/desktop.png`})
    }
    await h.page.setViewportSize({width:390,height:844})
    const box=await dialog(h).boundingBox(); assert.ok(box.x>=0 && box.x+box.width<=390)
    if (process.env.HMEM_MOVE_VISUALS === '1') await h.page.screenshot({path:`${visualDirectory}/narrow.png`})
    await h.page.keyboard.press('Escape'); await h.idle()
    assert.equal(await dialog(h).count(),0)
    assert.match(await h.page.evaluate(()=>document.activeElement?.id||''),/^(move-origin-task-parent|main-content-scroll)$/)
    await h.page.setViewportSize({width:1440,height:1000}); await h.scrollTo('project:root-project')
    assert.equal(await h.page.locator('#entity-root-project .tree-toggle').first().innerText(),mainBefore)
  } finally { await h.close() }
})

test('continuous bounded picker scrolling and retry preserve incomplete diagnostics and retire delayed query/close callbacks', {timeout:45000}, async()=>{
  const f=fixture(), p=f.projects[0]
  f.projects.push(...Array.from({length:72},(_,i)=>({...p,id:'extra-'+i,name:'Extra '+String(i).padStart(2,'0'),parent_id:null})))
  const h=await openHierarchy(f,[],authority(f))
  try {
    await h.start('tab=projects&focus=task:parent','parent'); await h.idle()
    const failed=h.gate(r=>r.showOnly==='projects' && r.kind==='workspace_root' && r.projectOffset===50,{error:500})
    await open(h,'task','parent'); await failed.arrived
    assert.match(await dialog(h).innerText(),/incomplete/)
    assert.ok(await dialog(h).locator('.move-project-row[data-project-id]').count()<=26)
    await dialog(h).getByRole('button',{name:'Retry Projects',exact:true}).click(); await h.idle()
    assert.equal(await dialog(h).getByRole('button',{name:/Next projects|Previous projects/}).count(),0)
    await dialog(h).locator('.move-tree').evaluate(e=>{e.scrollTop=e.scrollHeight})
    await h.page.waitForFunction(()=>document.querySelector('#move-choice-extra-71'))
    assert.ok(h.requests.some(r=>r.showOnly==='projects' && r.projectOffset===50))
    assert.ok(await dialog(h).locator('.move-project-row[data-project-id]').count()<=26)
    const old=h.gate(r=>r.showOnly==='projects' && r.query==='extra',{hold:true})
    await h.page.locator('#move-search').fill('extra'); await h.bounded(old.arrived,10000,'old query')
    await h.page.locator('#move-search').fill('needle'); old.release(); await h.idle()
    assert.doesNotMatch(await dialog(h).locator('.move-tree').innerText(),/Extra/)
    const held=h.gate(r=>r.showOnly==='projects' && r.query==='paused',{hold:true})
    await h.page.locator('#move-search').fill('paused'); await h.bounded(held.arrived,10000,'retired dialog')
    await dialog(h).getByRole('button',{name:'Cancel',exact:true}).click()
    await h.page.locator('#move-origin-task-parent').click(); held.release(); await h.idle()
    assert.equal(await h.page.locator('#move-search').inputValue(),'')
    assert.ok(h.caps().maxBranches<=4); assert.ok(h.caps().maxDetails<=6)
  } finally { await h.close() }
})

test('server refusal retains selection; duplicate submission and closed-dialog acknowledgement are fenced', {timeout:45000}, async()=>{
  const f=fixture(), a=authority(f), h=await openHierarchy(f,[],a)
  try {
    await h.start(); await h.idle(); await open(h,'task','parent'); await choose(h,'Destination')
    a.fail=true
    const refusal=h.gate(r=>r.method==='PUT',{error:409})
    await save(h); await refusal.arrived; await h.idle()
    assert.match(await dialog(h).getByRole('alert').innerText(),/closed dependencies.*same project/i)
    assert.match(await dialog(h).locator('.selected').innerText(),/Destination.*Selected/s)
    a.fail=false
    const held=h.gate(r=>r.method==='PUT',{hold:true})
    await save(h); await h.bounded(held.arrived,10000,'held save')
    assert.equal(await dialog(h).getByRole('button',{name:'Moving…',exact:true}).isDisabled(),true)
    await dialog(h).getByRole('button',{name:'Cancel',exact:true}).click()
    await h.page.locator('#move-origin-task-parent').click()
    held.release(); await h.idle()
    assert.equal(await dialog(h).count(),1)
    assert.equal(await dialog(h).locator('.selected').count(),0)
    assert.equal(h.requests.filter(r=>r.method==='PUT').length,2)
  } finally { await h.close() }
})

test('foreign canonical task placement retires pending authority; permission loss retires dialog and header actions', {timeout:45000}, async()=>{
  const f=fixture(), h=await openHierarchy(f,[],authority(f))
  try {
    await h.start(); await h.idle(); await open(h,'task','parent'); await choose(h,'Destination')
    const held=h.gate(r=>r.method==='PUT',{hold:true})
    await save(h); await h.bounded(held.arrived,10000,'held stale mutation')
    f.tasks.find(t=>t.id==='parent').project_id=null
    await h.page.evaluate(frames=>window.pushHierarchyFrames(frames),[frame(f,'task','parent','superseding-placement')])
    await dialog(h).waitFor({state:'detached'})
    held.release(); await h.idle()
    await h.scrollTo('task:parent'); await open(h,'task','parent')
    assert.match(await dialog(h).locator('.move-current').innerText(),/No project/)
    const session=h.gate(r=>r.path==='/api/v1/session',{transform:value=>({...value,workspace:{...value.workspace,role:'viewer',can_edit:false,can_admin:false}})})
    await h.page.evaluate(frames=>window.pushHierarchyFrames(frames),[frame(f,'workspace',f.workspace.id,'permission-loss','permissions',[{kind:'session_authorization',target:'session-authorization'}])])
    await h.bounded(session.arrived,10000,'permission refresh'); await h.idle()
    assert.equal(await dialog(h).count(),0)
    assert.equal(await h.page.locator('.btn-move').count(),0)
  } finally { await h.close() }
})

test('own project event before acknowledgement preserves exact saving authority', {timeout:30000}, async()=>{
  const f=fixture(), h=await openHierarchy(f,[],authority(f))
  try {
    await h.start(); await h.idle(); await open(h,'project','root-project'); await choose(h,'Destination')
    const held=h.gate(r=>r.method==='PUT',{hold:true})
    await save(h); const receipt=await h.bounded(held.arrived,10000,'project acknowledgement')
    await h.page.evaluate(frames=>window.pushHierarchyFrames(frames),[frame(f,'project','root-project','own-move',receipt.update.request_id)])
    await h.page.waitForTimeout(100)
    assert.equal(await dialog(h).count(),1)
    held.release(); await h.idle()
    assert.equal(await dialog(h).count(),0)
    await h.scrollTo('project:root-project'); await open(h,'project','root-project')
    assert.match(await dialog(h).locator('.move-current').innerText(),/Destination/)
  } finally { await h.close() }
})

async function assertInTree(h, selector) {
  const tree=await dialog(h).locator('.move-tree').boundingBox()
  const row=await dialog(h).locator(selector).boundingBox()
  assert.ok(row && row.y>=tree.y && row.y+row.height<=tree.y+tree.height, `${selector} must be physically visible in the tree`)
}

test('deep current location near the end of all roots unfolds and reveals; keyboard traverses the continuous tree', {timeout:45000}, async()=>{
  const f=fixture(), p=f.projects[0]
  p.name='Z Current root'
  f.projects.push(...Array.from({length:80},(_,i)=>({...p,id:'scroll-'+i,name:'A root '+String(i).padStart(2,'0'),parent_id:null})))
  f.tasks.forEach(t=>{t.project_id='needle'})
  const h=await openHierarchy(f,[],authority(f))
  try {
    await h.start('tab=projects&focus=task:parent','parent'); await h.idle(); await open(h,'task','parent')
    await assertInTree(h,'#move-choice-needle')
    assert.equal(await dialog(h).getByRole('button',{name:'Collapse Z Current root',exact:true}).count(),1)
    assert.equal(await dialog(h).getByRole('button',{name:'Collapse Nested context',exact:true}).count(),1)
    assert.match(await dialog(h).locator('.move-current').innerText(),/Needle project.*Current parent/s)
    assert.ok(await dialog(h).locator('.move-tree').evaluate(e=>e.scrollTop)>50*56)
    assert.equal(await h.page.locator('#move-search').evaluate(e=>document.activeElement===e),true)
    await dialog(h).locator('.move-tree').focus(); await h.page.keyboard.press('End'); await h.page.waitForTimeout(50)
    let end=await h.page.evaluate(()=>({id:document.activeElement.id,index:Number(document.activeElement.closest('[data-row-index]').dataset.rowIndex)}))
    await assertInTree(h,'#'+end.id)
    await h.page.keyboard.press('ArrowUp'); await h.page.waitForTimeout(50)
    const previous=await h.page.evaluate(()=>({id:document.activeElement.id,index:Number(document.activeElement.closest('[data-row-index]').dataset.rowIndex)}))
    assert.equal(previous.index,end.index-1); await assertInTree(h,'#'+previous.id)
    await h.page.keyboard.press('Home'); await h.page.waitForTimeout(50)
    const first=await h.page.evaluate(()=>({id:document.activeElement.id,index:Number(document.activeElement.closest('[data-row-index]').dataset.rowIndex)}))
    assert.equal(first.index,0); await assertInTree(h,'#'+first.id)
    await dialog(h).getByRole('button',{name:'Cancel',exact:true}).click(); await h.idle()
    await h.start('tab=projects&focus=project:nested','nested'); await h.idle(); await open(h,'project','nested')
    await assertInTree(h,'#move-choice-nested')
    assert.match(await dialog(h).locator('#move-choice-nested').innerText(),/Moving/)
    assert.match(await dialog(h).locator('#move-choice-needle').innerText(),/Moves with/)
    assert.equal(await dialog(h).locator('#move-choice-needle').isDisabled(),true)
  } finally { await h.close() }
})

test('live project insertion stages full membership and retains valid selection; reparent/status/delete repair context', {timeout:45000}, async()=>{
  const f=fixture(), p=f.projects[0]
  f.projects.push(...Array.from({length:70},(_,i)=>({...p,id:'live-'+i,name:'Live '+String(i).padStart(2,'0'),parent_id:null})))
  const h=await openHierarchy(f,[],authority(f))
  try {
    await h.start('tab=projects&focus=task:parent','parent'); await h.idle(); await open(h,'task','parent')
    await dialog(h).locator('.move-tree').evaluate(e=>{e.scrollTop=0}); await h.page.waitForTimeout(50)
    await choose(h,'Destination')
    await dialog(h).locator('.move-tree').evaluate(e=>{e.scrollTop=31*56+9}); await h.page.waitForTimeout(50)
    const anchor=await dialog(h).locator('.move-tree').evaluate(e=>{
      const rect=e.getBoundingClientRect()
      const row=[...e.querySelectorAll('[data-project-id]')].find(r=>{const b=r.getBoundingClientRect();return b.top<=rect.top+1 && b.bottom>rect.top+1})
      return {id:row.dataset.projectId, delta:row.getBoundingClientRect().top-rect.top}
    })
    const held=h.gate(r=>r.showOnly==='projects' && r.kind==='workspace_root' && r.projectOffset===50,{hold:true})
    f.projects.push({...p,id:'new-live',name:'A new streamed project',parent_id:null})
    await h.page.evaluate(frames=>window.pushHierarchyFrames(frames),[frame(f,'project','new-live','created-live')])
    await h.bounded(held.arrived,10000,'staged picker continuation')
    assert.equal(await dialog(h).count(),1)
    assert.match(await dialog(h).locator('.selected').innerText(),/Destination/)
    assert.equal(await dialog(h).locator('#move-choice-new-live').count(),0)
    held.release(); await h.idle()
    const after=await dialog(h).locator('.move-tree').evaluate((e,id)=>{
      const row=[...e.querySelectorAll('[data-project-id]')].find(r=>r.dataset.projectId===id)
      return row.getBoundingClientRect().top-e.getBoundingClientRect().top
    },anchor.id)
    assert.ok(Math.abs(after-anchor.delta)<2,'live insertion preserves row and intrarow scroll anchor')
    await dialog(h).locator('.move-tree').evaluate(e=>{e.scrollTop=0}); await h.page.waitForTimeout(50)
    assert.equal(await dialog(h).locator('#move-choice-new-live').count(),1)
    assert.match(await dialog(h).locator('.selected').innerText(),/Destination/)
    const root=f.projects.find(p=>p.id==='root-project'); root.parent_id='destination'
    await h.page.evaluate(frames=>window.pushHierarchyFrames(frames),[frame(f,'project',root.id,'source-reparent')]); await h.idle()
    assert.equal(await dialog(h).count(),1)
    assert.equal(await dialog(h).getByRole('button',{name:'Collapse Destination',exact:true}).count(),1)
    assert.match(await dialog(h).locator('.move-current').innerText(),/root-project/)
    assert.match(await dialog(h).locator('.selected').innerText(),/Destination/)
    f.projects.find(p=>p.id==='destination').status='completed'
    await h.page.evaluate(frames=>window.pushHierarchyFrames(frames),[frame(f,'project','destination','closed-destination')]); await h.idle()
    assert.equal(await dialog(h).locator('.selected').count(),0)
    assert.equal(await dialog(h).getByRole('button',{name:'Confirm',exact:true}).isDisabled(),true)
    root.parent_id=null
    f.projects=f.projects.filter(p=>p.id!=='destination')
    await h.page.evaluate(frames=>window.pushHierarchyFrames(frames),[frame(f,'project','destination','deleted-destination')]); await h.idle()
    assert.equal(await dialog(h).count(),1)
    assert.equal(await dialog(h).locator('#move-choice-destination').count(),0)
    await dialog(h).locator('.move-tree').evaluate(e=>{e.scrollTop=e.scrollHeight}); await h.page.waitForTimeout(50)
    assert.match(await dialog(h).locator('.move-current').innerText(),/root-project/)
    assert.ok(h.caps().maxBranches<=4); assert.ok(h.caps().maxDetails<=6)
  } finally { await h.close() }
})

for (const newParentFirst of [true,false]) {
  test(`selected non-context destination survives ${newParentFirst?'new':'old'} parent membership completing first`, {timeout:45000}, async()=>{
    const f=fixture(), p=f.projects[0]
    f.projects.push({...p,id:'branch-a',name:'Branch A',parent_id:null}, {...p,id:'branch-b',name:'Branch B',parent_id:null},
      {...p,id:'chosen',name:'Chosen destination',parent_id:'branch-a'}, {...p,id:'b-child',name:'Existing B child',parent_id:'branch-b'})
    const h=await openHierarchy(f,[],authority(f))
    try {
      await h.start('tab=projects&focus=task:parent','parent'); await h.idle(); await open(h,'task','parent')
      await dialog(h).getByRole('button',{name:'Expand Branch A',exact:true}).click(); await h.idle()
      await dialog(h).getByRole('button',{name:'Expand Branch B',exact:true}).click(); await h.idle()
      await choose(h,'Chosen destination')
      assert.doesNotMatch(await dialog(h).locator('#move-choice-chosen').innerText(),/Moving|Moves with|Current parent/)
      const heldParent=newParentFirst?'branch-a':'branch-b'
      const held=h.gate(r=>r.showOnly==='projects' && r.kind==='project' && r.parent===heldParent,{hold:true})
      const completed=h.gate(r=>r.showOnly==='projects' && r.kind==='project' && r.parent===(newParentFirst?'branch-b':'branch-a'),{})
      f.projects.find(p=>p.id==='chosen').parent_id='branch-b'
      await h.page.evaluate(frames=>window.pushHierarchyFrames(frames),[frame(f,'project','chosen','reparent-chosen')])
      await h.bounded(held.arrived,10000,'out-of-order parent refresh')
      await h.bounded(completed.arrived,10000,'first completing parent refresh')
      await h.page.waitForTimeout(50)
      assert.equal(completed.receipt.done,true); assert.equal(held.receipt.done,false)
      if (newParentFirst) {
        await h.page.waitForFunction(()=>{
          const row=document.getElementById('move-choice-chosen')?.closest('[data-row-index]')
          const parent=document.getElementById('move-choice-branch-b')?.closest('[data-row-index]')
          return row && parent && Number(row.dataset.rowIndex)>Number(parent.dataset.rowIndex)
        })
        assert.match(await dialog(h).locator('.selected').innerText(),/Chosen destination/)
      } else {
        assert.equal(await dialog(h).getByRole('button',{name:'Confirm',exact:true}).isDisabled(),true)
      }
      held.release(); await h.idle()
      assert.match(await dialog(h).locator('.selected').innerText(),/Chosen destination/)
      const ids=await dialog(h).locator('[data-project-id]').evaluateAll(rows=>rows.map(r=>r.dataset.projectId))
      assert.ok(ids.indexOf('chosen')>ids.indexOf('branch-b'))
      assert.equal(await dialog(h).getByRole('button',{name:'Confirm',exact:true}).isDisabled(),false)
      await save(h); await h.idle()
      assert.equal(f.tasks.find(t=>t.id==='parent').project_id,'chosen')
      assert.ok(h.caps().maxBranches<=4); assert.ok(h.caps().maxDetails<=6)
    } finally { await h.close() }
  })
}

test('all status filters off retain root confirmation after a live project refresh', {timeout:45000}, async()=>{
  const f=fixture(), p=f.projects[0], h=await openHierarchy(f,[],authority(f))
  try {
    await h.start(); await h.idle(); await open(h,'task','parent')
    await dialog(h).getByRole('button',{name:'active',exact:true}).click(); await h.idle()
    await dialog(h).getByRole('button',{name:'paused',exact:true}).click(); await h.idle()
    assert.equal(await dialog(h).locator('.filter-pill-active').count(),0)
    await choose(h,'No project')
    const branchesBefore=h.requests.filter(r=>r.showOnly==='projects').length
    f.projects.push({...p,id:'empty-filter-new',name:'Created while all statuses off',parent_id:null})
    await h.page.evaluate(frames=>window.pushHierarchyFrames(frames),[frame(f,'project','empty-filter-new','empty-filter-live')]); await h.idle()
    assert.equal(await dialog(h).count(),1)
    assert.match(await dialog(h).locator('.selected').innerText(),/No project/)
    assert.equal(h.requests.filter(r=>r.showOnly==='projects').length,branchesBefore)
    assert.equal(await dialog(h).getByRole('button',{name:'Confirm',exact:true}).isDisabled(),false)
    await save(h); await h.idle()
    assert.equal(f.tasks.find(t=>t.id==='parent').project_id,null)
    assert.equal(await dialog(h).count(),0)
  } finally { await h.close() }
})

test('canonical selection revalidation retains a destination moved under a collapsed unloaded parent', {timeout:45000}, async()=>{
  const f=fixture(), p=f.projects[0]
  f.projects.push({...p,id:'collapsed-a',name:'Branch A',parent_id:null}, {...p,id:'collapsed-b',name:'Branch B',parent_id:null},
    {...p,id:'chosen-hidden',name:'Chosen hidden destination',parent_id:'collapsed-a'})
  const h=await openHierarchy(f,[],authority(f))
  try {
    await h.start('tab=projects&focus=task:parent','parent'); await h.idle(); await open(h,'task','parent')
    await dialog(h).getByRole('button',{name:'Expand Branch A',exact:true}).click(); await h.idle()
    assert.equal(h.requests.filter(r=>r.showOnly==='projects' && r.parent==='collapsed-b').length,0)
    await choose(h,'Chosen hidden destination')
    const validation=h.gate(r=>r.path.endsWith('/navigation/focus/project/chosen-hidden'),{hold:true})
    f.projects.find(p=>p.id==='chosen-hidden').parent_id='collapsed-b'
    await h.page.evaluate(frames=>window.pushHierarchyFrames(frames),[frame(f,'project','chosen-hidden','collapsed-reparent')])
    await h.bounded(validation.arrived,10000,'canonical selected-project ancestry')
    await h.page.waitForFunction(()=>[...document.querySelectorAll('#move-dialog button')].some(b=>b.textContent==='Confirm' && b.disabled))
    assert.equal(await dialog(h).getByRole('button',{name:'Confirm',exact:true}).isDisabled(),true)
    validation.release(); await h.idle()
    assert.equal(await dialog(h).getByRole('button',{name:'Collapse Branch B',exact:true}).count(),1)
    assert.match(await dialog(h).locator('.selected').innerText(),/Chosen hidden destination/)
    await assertInTree(h,'#move-choice-chosen-hidden')
    assert.equal(await dialog(h).getByRole('button',{name:'Confirm',exact:true}).isDisabled(),false)
    await save(h); await h.idle()
    assert.equal(f.tasks.find(t=>t.id==='parent').project_id,'chosen-hidden')
    assert.ok(h.caps().maxBranches<=4); assert.ok(h.caps().maxDetails<=6)
  } finally { await h.close() }
})
