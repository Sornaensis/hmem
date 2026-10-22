import assert from 'node:assert/strict'
import test from 'node:test'
import { hierarchyFixture, openHierarchy } from './hierarchy-fixture.mjs'

function pauseFixture() {
  const data = hierarchyFixture(), project = data.projects[0], task = data.tasks[0]
  data.projects = [project, { ...project, id: 'hidden-project', name: 'Hidden branch', parent_id: project.id }, { ...project, id: 'own-project', name: 'Independent pause', parent_id: project.id, status: 'paused' }]
  data.tasks = [
    ...Array.from({ length: 45 }, (_, i) => ({ ...task, id: 'work-' + String(i).padStart(3, '0'), title: 'Work ' + String(i).padStart(3, '0'), status: 'todo' })),
    { ...task, id: 'hidden-work', title: 'Hidden work', project_id: 'hidden-project' },
    { ...task, id: 'own-work', title: 'Independent work', project_id: 'own-project', status: 'blocked', auto_blocked: true },
    { ...task, id: 'mixed-work', title: 'Mixed blockers', status: 'blocked', auto_blocked: true },
    { ...task, id: 'mixed-child', title: 'Mixed subtask', parent_id: 'mixed-work' },
    { ...task, id: 'done-work', title: 'Done work', status: 'done' },
    { ...task, id: 'cancelled-work', title: 'Cancelled work', status: 'cancelled' }
  ]
  data.dependencies = [{ task_id: 'mixed-work', depends_on_id: 'work-044' }]
  return data
}

// One authority owns canonical data. Both production Elm pages make mutations
// against it and receive the same transaction's committed project event.
function pauseAuthority(data) {
  const pages = new Set(), own = new Set(['own-project']); let pending = [], sequence = 0
  const descendants = root => { const ids = new Set([root]); for (let changed = true; changed;) { changed = false; for (const p of data.projects) if (ids.has(p.parent_id) && !ids.has(p.id)) { ids.add(p.id); changed = true } }; return ids }
  const paused = projectId => { const byId = new Map(data.projects.map(p => [p.id, p])); for (let id = projectId; id != null; id = byId.get(id)?.parent_id) if (own.has(id)) return true; return false }
  return {
    pages,
    updateProject(fixture, id, update) {
      assert.equal(fixture, data); assert.deepEqual(Object.keys(update).sort(), ['request_id','status']); assert.ok(['active','paused'].includes(update.status))
      const ids = descendants(id), beforeProjects = structuredClone(data.projects), beforeTasks = structuredClone(data.tasks)
      if (update.status === 'paused') own.add(id); else own.delete(id)
      for (const p of data.projects) if (ids.has(p.id)) p.status = paused(p.id) ? 'paused' : 'active'
      for (const t of data.tasks) if (ids.has(t.project_id) && !['done','cancelled'].includes(t.status)) {
        const deps = data.dependencies.filter(edge => edge.task_id === t.id && !['done','cancelled'].includes(data.tasks.find(other => other.id === edge.depends_on_id)?.status)).length
        const children = data.tasks.filter(child => child.parent_id === t.id && !['done','cancelled'].includes(child.status)).length
        if (paused(t.project_id) || deps || children) { t.status = 'blocked'; t.auto_blocked = true }
        else if (t.auto_blocked) { t.status = 'todo'; t.auto_blocked = false }
      }
      const projects = data.projects.filter((p, i) => p.status !== beforeProjects[i].status).map(p => ({ id:p.id,previous_status:beforeProjects.find(old => old.id === p.id).status,current_status:p.status }))
      const tasks = data.tasks.filter((t, i) => t.status !== beforeTasks[i].status).map(t => ({ task: structuredClone(t),previous_status:beforeTasks.find(old => old.id === t.id).status,current_status:t.status,previous_auto_blocked:false,auto_blocked:t.auto_blocked,previous_open_dependency_count:0,open_dependency_count:0,previous_open_subtask_count:0,open_subtask_count:0,previous_project_paused:update.status !== 'paused',project_paused:paused(t.project_id),reason:update.status === 'paused' ? 'blocked_by_paused_project' : 'unblocked_project_pause_resolved' }))
      sequence++
      pending.push({ schema_version:1,type:'change',event:{schema_version:1,event_id:'pause-'+sequence,scope:'workspace',workspace_id:data.workspace.id,occurred_at:'2026-10-12T12:00:00Z',transaction:{id:'pause-tx-'+sequence,cause:'rest',request_id:update.request_id},actor:{type:'system',id:null},entity:{type:'project',id,action:'updated'},invalidations:[{kind:'entity',target:'project:'+id},{kind:'tree',target:'workspace:'+data.workspace.id},{kind:'next_task',target:'workspace:'+data.workspace.id}]}})
      return { ...structuredClone(data.projects.find(p => p.id === id)),pause_effects:{project_count:projects.length,task_count:tasks.length,projects:projects.slice(0,20),tasks:tasks.slice(0,20),has_more_projects:projects.length>20,has_more_tasks:tasks.length>20,remaining_own_pause:own.has(id),remaining_ancestor_pause:paused(data.projects.find(p => p.id === id).parent_id)} }
    },
    async broadcast() { const frames = pending; pending = []; await Promise.all([...pages].map(page => page.evaluate(frames => window.pushHierarchyFrames(frames), frames))) }
  }
}

const paint = page => page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))))

test('blocked task menus show all statuses with only blocked and cancelled enabled under direct or inherited pause and restore active choices on resume', { timeout:60000 }, async () => {
  for (const inherited of [false, true]) {
    const data = hierarchyFixture(), root = { ...data.projects[0], status:'paused' }, base = data.tasks[0]
    data.projects = [root, ...(inherited ? [{ ...root, id:'child-project', parent_id:root.id, status:'active' }] : [])]
    const projectId = inherited ? 'child-project' : root.id
    data.tasks = ['cancel-work', 'resume-work'].map(id => ({ ...base, id, title:id, project_id:projectId, status:'blocked', auto_blocked:false }))
    const updates = []
    const h = await openHierarchy(data, [], {
      updateTask(fixture, id, update) {
        updates.push(update.status)
        const task = fixture.tasks.find(value => value.id === id)
        task.status = update.status
        return task
      },
      updateProject(fixture, id, update) {
        assert.equal(id, root.id); assert.equal(update.status, 'active')
        const project = fixture.projects.find(value => value.id === id)
        project.status = update.status
        return project
      },
      async broadcast() {}
    })
    const control = id => h.page.locator('#entity-' + id + ' select').first()
    const options = id => control(id).locator('option').evaluateAll(values => values.map(option => ({ value:option.value, disabled:option.disabled })))
    try {
      await h.page.addInitScript(id => localStorage.setItem('hmem-ws-' + id, JSON.stringify({ filterTaskStatuses:['todo', 'in_progress', 'blocked', 'done', 'cancelled'] })), data.workspace.id)
      await h.start(); await h.idle()
      for (const id of ['cancel-work', 'resume-work']) {
        await h.scrollTo('task:' + id)
        assert.equal(await control(id).inputValue(), 'blocked')
        assert.deepEqual(await options(id), ['todo', 'in_progress', 'blocked', 'done', 'cancelled'].map(value => ({ value, disabled:['todo', 'in_progress', 'done'].includes(value) })), inherited ? 'ancestor pause' : 'direct pause')
        assert.deepEqual(await control(id).locator('option').allTextContents(), ['todo', 'in progress', 'blocked', 'done', 'cancelled'])
        const appearance = await control(id).locator('option').evaluateAll(values => values.map(option => ({ disabled:option.disabled, color:getComputedStyle(option).color, title:option.title })))
        for (const option of appearance.filter(value => value.disabled)) {
          assert.match(option.title, /Resume the paused project or ancestor/)
          assert.equal(option.color, 'rgb(148, 163, 184)', 'disabled options use the existing muted grey color')
          assert.notEqual(option.color, appearance[2].color)
        }
      }
      await control('cancel-work').selectOption('cancelled'); await h.idle()
      assert.equal(await control('cancel-work').inputValue(), 'cancelled')
      assert.deepEqual(updates, ['cancelled'])
      await h.scrollTo('project:' + root.id)
      await h.page.locator('#entity-' + root.id + ' .card-project select').first().selectOption('active'); await h.idle()
      await h.scrollTo('task:resume-work')
      assert.equal(await control('resume-work').inputValue(), 'blocked', 'resume preserves a manual block')
      assert.deepEqual(await options('resume-work'), ['todo', 'in_progress', 'blocked', 'done', 'cancelled'].map(value => ({ value, disabled:false })))
      await control('resume-work').selectOption('in_progress'); await h.idle()
      assert.equal(await control('resume-work').inputValue(), 'in_progress')
      assert.deepEqual(updates, ['cancelled', 'in_progress'])
      assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
      assert.ok(h.caps().maxBranches <= 4); assert.ok(h.caps().maxDetails <= 6)
    } finally { await h.close() }
  }
})

test('two independent clients converge after canonical pause/resume, including collapsed, offscreen and unchanged blocked causes', { timeout:90000 }, async () => {
  const data = pauseFixture(), authority = pauseAuthority(data)
  const a = await openHierarchy(data, [], authority), b = await openHierarchy(data, [], authority)
  authority.pages.add(a.page); authority.pages.add(b.page)
  const oldBranch = b.gate(r => r.kind === 'project' && r.parent === 'hidden-project', { hold:true })
  let oldNext, oldReady
  try {
    await a.start(); await b.start(); await b.bounded(oldBranch.arrived, 10000, 'Hidden branch arrival')
    await b.page.locator('#entity-hidden-project .tree-toggle').first().click(); oldBranch.release()
    await a.idle(); await b.idle()
    assert.equal(await b.page.locator('#entity-hidden-work').count(), 0)
    await b.scrollTo('task:work-044'); await b.scrollTo('project:root-project')
    oldNext = b.gate(r => r.path.endsWith('/root-project/next-task-page') && r.includeBlocked, { hold:true })
    oldReady = b.gate(r => r.path.endsWith('/root-project/next-task-page') && !r.includeBlocked, { hold:true })
    await b.page.locator('#entity-root-project .btn-extras-toggle').first().click()
    await b.bounded(oldNext.arrived, 10000, 'Old diagnostic response')
    await b.bounded(oldReady.arrived, 10000, 'Old first ready response')
    assert.equal(await b.page.locator('#entity-root-project .project-next-tasks-section > .project-next-tasks-list').count(), 0, 'neither first-load response is admitted yet')
    assert.equal(await b.page.locator('#entity-work-044').count(), 0, 'offscreen loaded task is outside ordinary window')
    const before = b.requests.length
    await a.page.locator('#entity-root-project .card-project select').first().selectOption('paused')
    await a.idle()
    await b.page.locator('#entity-root-project .project-next-tasks-diagnostics > summary').click()
    await b.page.locator('#entity-root-project .project-next-tasks-diagnostics').getByText('Paused by this project or an ancestor.', { exact:false }).first().waitFor()
    oldReady.release(); oldNext.release(); await b.idle(); await paint(b.page)
    assert.equal(await b.page.locator('#entity-root-project .card-project select').first().inputValue(), 'paused')
    assert.equal(await b.page.locator('#entity-root-project .project-next-tasks-section > .project-next-tasks-list').count(), 0, 'late ready response cannot revive work under pause')
    const mixed = b.page.locator('#entity-root-project .next-task-candidate').filter({ has:b.page.locator('.popover-card-title').filter({ hasText:/^Mixed blockers$/ }) })
    assert.match(await mixed.innerText(), /Paused by this project or an ancestor/)
    assert.match(await mixed.innerText(), /1 open dependency/)
    assert.match(await mixed.innerText(), /1 open subtask/)
    assert.ok(b.requests.slice(before).some(r => r.path.endsWith('/navigation/summaries')), 'unchanged blocked cause receives canonical summary refresh')
    await b.scrollTo('task:work-044'); assert.equal(await b.page.locator('#entity-work-044 select').first().inputValue(), 'blocked')
    await b.scrollTo('project:hidden-project'); await b.page.locator('#entity-hidden-project .tree-toggle').first().click(); await b.idle()
    await b.scrollTo('task:hidden-work'); assert.equal(await b.page.locator('#entity-hidden-work select').first().inputValue(), 'blocked')
    await a.scrollTo('project:root-project'); await a.page.locator('#entity-root-project .card-project select').first().selectOption('active')
    await a.idle(); await b.idle()
    await b.scrollTo('task:work-043'); assert.equal(await b.page.locator('#entity-work-043 select').first().inputValue(), 'todo')
    await b.scrollTo('project:own-project'); assert.equal(await b.page.locator('#entity-own-project .card-project select').first().inputValue(), 'paused')
    assert.equal(data.tasks.find(t => t.id === 'done-work').status, 'done'); assert.equal(data.tasks.find(t => t.id === 'cancelled-work').status, 'cancelled')
    await b.scrollTo('project:root-project'); assert.doesNotMatch(await mixed.innerText(), /Paused by this project/)
    for (const h of [a,b]) { assert.ok(h.caps().maxBranches<=4); assert.ok(h.caps().maxDetails<=6); assert.ok(await h.page.evaluate(() => window.hierarchyMaximum)<=31) }
  } finally { oldBranch.release(); oldReady?.release(); oldNext?.release(); authority.pages.clear(); await Promise.all([a.close(),b.close()]) }
})

test('held project diagnostics cannot contaminate a replacement workspace', { timeout:60000 }, async () => {
  const original = pauseFixture(), replacement = pauseFixture()
  replacement.workspace = { ...replacement.workspace,id:'20000000-0000-4000-8000-000000000002',name:'Replacement workspace' }
  replacement.projects = replacement.projects.map(p => ({ ...p,id:'replacement-'+p.id,parent_id:p.parent_id ? 'replacement-'+p.parent_id : null,workspace_id:replacement.workspace.id }))
  replacement.tasks = replacement.tasks.map(t => ({ ...t,id:'replacement-'+t.id,parent_id:t.parent_id ? 'replacement-'+t.parent_id : null,project_id:'replacement-'+t.project_id,workspace_id:replacement.workspace.id }))
  replacement.dependencies = []
  const h = await openHierarchy(original, [replacement])
  let held
  try {
    await h.start(); await h.idle()
    held = h.gate(r => r.path.endsWith('/root-project/next-task-page') && r.includeBlocked, { hold:true,transform(page) { return { ...page, items: page.items.map(value => ({ ...value,task:{ ...value.task,title:'Retired diagnostic sentinel' } })) } } })
    await h.page.locator('#entity-root-project .btn-extras-toggle').first().click(); await h.bounded(held.arrived, 10000, 'Held old workspace diagnostics')
    await h.page.locator('.sidebar-nav').getByRole('link', { name:'Replacement workspace' }).click(); await h.page.locator('#entity-replacement-root-project').waitFor()
    held.release(); await h.idle()
    await h.page.locator('#entity-replacement-root-project .btn-extras-toggle').first().click(); await h.idle()
    assert.equal(await h.page.getByText('Retired diagnostic sentinel', { exact:true }).count(), 0)
    assert.equal(await h.page.locator('#entity-root-project').count(), 0)
  } finally { held?.release(); await h.close() }
})
