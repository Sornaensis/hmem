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

const dialog = h => h.page.locator('#move-dialog')
async function open(h,kind,id) {
  await h.page.locator('#move-origin-'+kind+'-'+id).click()
  await dialog(h).waitFor()
  await h.idle()
}
async function choose(h,name) { await dialog(h).locator('.move-project-choice').filter({hasText:name}).click(); await dialog(h).locator('.selected').waitFor() }
const save = h => dialog(h).getByRole('button',{name:'Move',exact:true}).click()

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
    assert.match(await dialog(h).locator('.move-location').first().innerText(),/Destination/)
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
    assert.match(await dialog(h).innerText(),/detaches this subtask.*top-level task/)
    assert.match(await dialog(h).locator('.move-location').first().innerText(),/root-project \/ Parent/)
    await choose(h,'Destination'); await h.page.locator('#move-search').press('Enter'); await h.idle()
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
    await dialog(h).getByRole('button',{name:'Expand root-project',exact:true}).click(); await h.idle()
    assert.equal(await dialog(h).locator('.move-project-choice').filter({hasText:'Nested context'}).isDisabled(),true)
    await choose(h,'Destination'); await save(h); await h.idle()
    assert.equal(f.projects.find(p=>p.id==='root-project').parent_id,'destination')
    assert.equal(f.projects.find(p=>p.id==='nested').parent_id,'root-project')
    await h.scrollTo('project:root-project'); await open(h,'project','root-project')
    await choose(h,'Workspace root'); await save(h); await h.idle()
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
    await dialog(h).getByRole('button',{name:'Expand root-project',exact:true}).click(); await h.idle()
    await dialog(h).getByRole('button',{name:'Expand Nested context',exact:true}).click(); await h.idle()
    await choose(h,'Needle project')
    assert.match(await dialog(h).locator('.move-location').last().innerText(),/root-project \/ Nested context \/ Needle project/)
    assert.ok(h.requests.some(r=>r.showOnly==='projects' && r.query==='needle' && r.parent==='nested'))
    assert.ok(h.requests.filter(r=>r.showOnly==='projects').every(r=>r.projectStatuses.includes('active') && r.projectStatuses.includes('paused')))
    await h.page.locator('#move-search').fill(''); await h.idle()
    await dialog(h).getByLabel('completed',{exact:true}).check(); await h.idle()
    const closed=dialog(h).locator('.move-project-choice').filter({hasText:'Closed'})
    assert.equal(await closed.isDisabled(),true)
    if (process.env.HMEM_MOVE_VISUALS === '1') {
      await mkdir('.task-scratch/move-dialog', {recursive:true})
      await h.page.screenshot({path:'.task-scratch/move-dialog/desktop.png'})
    }
    await h.page.setViewportSize({width:390,height:844})
    const box=await dialog(h).boundingBox(); assert.ok(box.x>=0 && box.x+box.width<=390)
    if (process.env.HMEM_MOVE_VISUALS === '1') await h.page.screenshot({path:'.task-scratch/move-dialog/narrow.png'})
    await h.page.keyboard.press('Escape'); await h.idle()
    assert.equal(await dialog(h).count(),0)
    assert.match(await h.page.evaluate(()=>document.activeElement?.id||''),/^(move-origin-task-parent|main-content-scroll)$/)
    await h.page.setViewportSize({width:1440,height:1000}); await h.scrollTo('project:root-project')
    assert.equal(await h.page.locator('#entity-root-project .tree-toggle').first().innerText(),mainBefore)
  } finally { await h.close() }
})

test('bounded picker paging and retry preserve incomplete diagnostics and retire delayed query/close callbacks', {timeout:45000}, async()=>{
  const f=fixture(), p=f.projects[0]
  f.projects.push(...Array.from({length:72},(_,i)=>({...p,id:'extra-'+i,name:'Extra '+String(i).padStart(2,'0'),parent_id:null})))
  const h=await openHierarchy(f,[],authority(f))
  try {
    await h.start('tab=projects&focus=task:parent','parent'); await h.idle()
    const failed=h.gate(r=>r.showOnly==='projects' && r.kind==='workspace_root' && r.projectOffset===50,{error:500})
    await open(h,'task','parent'); await failed.arrived
    assert.match(await dialog(h).innerText(),/incomplete/)
    assert.ok(await dialog(h).locator('.move-project-row').count()<=25)
    await dialog(h).getByRole('button',{name:'Retry Projects',exact:true}).click(); await h.idle()
    assert.match(await dialog(h).innerText(),/76 loaded/)
    await dialog(h).getByRole('button',{name:'Next projects',exact:true}).click()
    assert.ok(await dialog(h).locator('.move-project-row').count()<=25)
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
    assert.equal(await dialog(h).locator('.selected').innerText(),'Destinationactive')
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
    assert.match(await dialog(h).locator('.move-location').first().innerText(),/No project/)
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
    assert.match(await dialog(h).locator('.move-location').first().innerText(),/Destination/)
  } finally { await h.close() }
})
