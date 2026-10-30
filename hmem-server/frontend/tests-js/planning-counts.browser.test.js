import assert from 'node:assert/strict'
import test from 'node:test'
import { hierarchyFixture, openHierarchy } from './hierarchy-fixture.mjs'

function fixture() {
  const data = hierarchyFixture(), root = data.projects[0], task = data.tasks[0]
  data.projects = [root, {...root, id:'child-project', name:'Child project', parent_id:root.id}]
  data.tasks = [
    ...Array.from({length:60}, (_,i)=>({...task,id:'task-'+String(i).padStart(3,'0'),title:'Task '+i,parent_id:null})),
    {...task,id:'nested',title:'Nested',parent_id:'task-000'},
    {...task,id:'unassigned',title:'Unassigned',description:'Unassigned work',project_id:null,parent_id:null},
    {...task,id:'done',title:'Done',status:'done',parent_id:null},
    {...task,id:'cancelled',title:'Cancelled',status:'cancelled',parent_id:null},
  ]
  return data
}

function event(data, id, action, sequence) {
  return {schema_version:1,type:'change',event:{schema_version:1,event_id:'count-'+sequence,scope:'workspace',workspace_id:data.workspace.id,
    occurred_at:'2026-10-13T12:00:00Z',transaction:{id:'count-tx-'+sequence,cause:'rest',request_id:null},actor:{type:'system',id:null},
    entity:{type:'task',id,action},invalidations:[{kind:'entity',target:'task:'+id},{kind:'tree',target:'workspace:'+data.workspace.id}]}}
}

const countRead = receipt => receipt.path.endsWith('/planning-counts')
const both = async (h, text) => {
  await h.page.waitForFunction(expected => [...document.querySelectorAll('.workspace-context-persistent .workspace-planning-total')].length === 1 && [...document.querySelectorAll('.workspace-context-persistent .workspace-planning-total')].every(element=>element.textContent.includes(expected)),text)
}
const push = (h, frames) => h.page.evaluate(frames=>window.pushHierarchyFrames(frames),frames)

for (const [name, statuses, label] of [
  ['mixed', ['todo','blocked','done','done','done','cancelled','cancelled','cancelled','cancelled'], '2/5'],
  ['done', ['done','done','done','cancelled','cancelled','cancelled','cancelled'], '3'],
  ['cancelled', ['cancelled','cancelled','cancelled','cancelled'], '0'],
]) test('production card totals exclude cancelled work: '+name, {timeout:60000}, async()=>{
  const data=hierarchyFixture(),root=data.projects[0],base=data.tasks[0]
  data.projects=[root]
  data.tasks=[...statuses.map((status,i)=>({...base,id:'project-'+i,title:'Project '+i,status,parent_id:null})),
    {...base,id:'parent',title:'Parent',project_id:null,parent_id:null,dependency_count:1},
    ...statuses.map((status,i)=>({...base,id:'child-'+i,title:'Child '+i,status,project_id:null,parent_id:'parent'}))]
  data.dependencies=[{task_id:'parent',depends_on_id:'project-0'}]
  const h=await openHierarchy(data)
  const projectSummary=h.page.locator('#entity-root-project > .card > .card-summary')
  const taskSummary=h.page.locator('#entity-parent > .task-card-work > .card-summary')
  try{
    await h.start();await h.idle()
    const suffix=name==='mixed'?' remaining':name==='done'?' (all done)':''
    assert.equal(await projectSummary.innerText(),label+' tasks'+suffix)
    assert.equal(await taskSummary.innerText(),name==='cancelled'?'1 dependency':label+' subtasks'+suffix+' · '+(name==='mixed'?'1 open dependency':'1 dependency'))
    const original=await projectSummary.innerText()
    await h.page.getByRole('button',{name:'Cancelled',exact:true}).click();await h.idle()
    assert.equal(await projectSummary.innerText(),original,'Explicit cancelled filtering does not change canonical totals')
    await h.page.locator('#entity-root-project .tree-toggle').click();await h.idle()
    await h.page.locator('#entity-project-'+statuses.indexOf('cancelled')).waitFor()
    if(name==='mixed'){
      data.tasks.find(row=>row.id==='project-0').status='cancelled'
      await push(h,[event(data,'project-0','updated',1)]);await h.idle()
      assert.equal(await projectSummary.innerText(),'1/4 tasks remaining')
      data.tasks.find(row=>row.id==='project-0').status='todo'
      await push(h,[event(data,'project-0','updated',2)]);await h.idle()
      assert.equal(await projectSummary.innerText(),original,'Reopening restores the planning denominator')
    }
    assert.ok(h.caps().maxBranches<=4);assert.ok(h.caps().maxDetails<=6)
    assert.deepEqual(h.errors,[]);assert.deepEqual(h.unhandled,[])
  }finally{await h.close()}
})

test('production headers include unloaded 60-task work and survive partial pages, collapse and filters', {timeout:60000}, async()=>{
  const data=fixture(),h=await openHierarchy(data)
  try{
    const first=h.gate(r=>r.kind==='project'&&r.parent==='root-project'&&r.taskOffset===0,{hold:true})
    const second=h.gate(r=>r.kind==='project'&&r.parent==='root-project'&&r.taskOffset===50,{hold:true})
    await h.start();await h.idle()
    await h.page.locator('#entity-root-project .tree-toggle').click()
    await h.bounded(first.arrived,10000,'First child page')
    await both(h,'2 open projects · 62 open tasks')
    assert.equal(await h.page.locator('[data-hierarchy-key^="task:task-"]').count(),0,'Parent task pages are still held')
    first.release();await h.bounded(second.arrived,10000,'Partial 50-item continuation')
    await both(h,'2 open projects · 62 open tasks')
    second.release();await h.idle()
    const reads=h.requests.filter(countRead).length
    await h.page.locator('#entity-root-project .tree-toggle').click();await h.idle()
    await h.page.locator('#entity-unassigned').waitFor()
    const filtered=h.gate(request=>request.kind==='workspace_root'&&request.query==='Task')
    await h.page.locator('.hierarchy-search-input').fill('Task')
    await h.bounded(filtered.arrived,10000,'Hierarchy filter membership request');await h.idle()
    assert.equal(filtered.receipt.status,200);assert.equal(filtered.receipt.done,true)
    assert.equal(await h.page.locator('#entity-unassigned').count(),0,'Hierarchy query excludes unmatched membership')
    assert.equal(await h.page.locator('.search-input').inputValue(),'','Hierarchy filtering does not activate global search')
    assert.equal(await h.page.locator('.search-result-card').count(),0)
    await both(h,'2 open projects · 62 open tasks')
    assert.equal(h.requests.filter(countRead).length,reads,'Presentation changes do not refetch workspace totals')
    assert.ok(h.caps().maxBranches<=4);assert.ok(h.caps().maxDetails<=6)
    assert.deepEqual(h.errors,[]);assert.deepEqual(h.unhandled,[])
  }finally{await h.close()}
})

test('production live totals fail truthfully, retry, coalesce stale reads and refresh after resync', {timeout:60000}, async()=>{
  const data=fixture(),h=await openHierarchy(data)
  try{
    await h.start();await h.idle();await both(h,'62 open tasks')
    const fail=h.gate(countRead,{error:503})
    data.tasks.find(row=>row.id==='task-059').status='done'
    await push(h,[event(data,'task-059','updated',1)])
    await h.bounded(fail.arrived,10000,'Failed count refresh');await both(h,'Planning totals unavailable')
    await h.page.getByRole('button',{name:'Retry planning totals',exact:true}).first().click()
    await both(h,'61 open tasks');await h.idle()
    const old=h.gate(countRead,{hold:true}),before=h.requests.filter(countRead).length
    data.tasks.find(row=>row.id==='task-059').status='todo'
    await push(h,[event(data,'task-059','updated',2)])
    await h.bounded(old.arrived,10000,'Held obsolete totals');await both(h,'Updating planning totals…')
    data.tasks=data.tasks.filter(row=>row.id!=='task-058')
    await push(h,[event(data,'task-058','deleted',3),event(data,'task-059','updated',4)])
    await h.page.waitForTimeout(100)
    assert.equal(h.requests.filter(countRead).length,before+1,'Outstanding read retains its single slot')
    old.release();await both(h,'61 open tasks');await h.idle()
    assert.equal(h.requests.filter(countRead).length,before+2,'Burst coalesces into one fresh read')
    data.tasks.push({...data.tasks[0],id:'restored',title:'Restored',project_id:null,parent_id:null})
    await push(h,[{schema_version:1,type:'resync_required'}])
    await both(h,'62 open tasks');await h.idle()
    assert.deepEqual(h.errors,[]);assert.deepEqual(h.unhandled,[])
  }finally{await h.close()}
})
