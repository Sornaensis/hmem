import assert from 'node:assert/strict'
import test from 'node:test'
import { hierarchyFixture, openHierarchy } from './hierarchy-fixture.mjs'

test('cold held first membership paints the real root at scroll origin before large expanded batches drain', { timeout: 60000 }, async () => {
  const h=await openHierarchy()
  try{
    const first=h.gate(r=>r.kind==='project'&&r.parent==='root-project'&&r.projectOffset===0&&r.taskOffset===0,{hold:true})
    const opening=h.start()
    await h.bounded(first.arrived,10000,'Held first expanded page')
    await opening
    await h.page.evaluate(()=>new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve))))
    const initial=await h.page.evaluate(()=>JSON.parse(document.getElementById('hierarchy-viewport').dataset.hierarchyContext).top)
    assert.ok(initial<2,'Untouched origin must not follow the initial end-status sentinel: '+initial)
    first.release();await h.idle()
    const final=await h.page.evaluate(()=>{const scroll=document.getElementById('main-content-scroll'),root=document.getElementById('entity-root-project');return {top:JSON.parse(document.getElementById('hierarchy-viewport').dataset.hierarchyContext).top,visible:!!root&&root.getBoundingClientRect().bottom>scroll.getBoundingClientRect().top&&root.getBoundingClientRect().top<scroll.getBoundingClientRect().bottom}})
    assert.ok(final.top<2&&final.visible,JSON.stringify(final))
    assert.ok(h.caps().maxBranches<=4);assert.ok(h.caps().maxDetails<=6)
  }finally{await h.close()}
})

test('production expanded hierarchy drains independent pages and keeps every endpoint reachable under one mounted cap', { timeout: 60000 }, async () => {
  const h = await openHierarchy()
  try {
    const held = h.gate(r => r.parent === 'root-project' && r.projectOffset === 50, { hold: true })
    await h.start()
    let timer
    try { await Promise.race([held.arrived, new Promise((_, reject) => { timer = setTimeout(() => reject(new Error('Continuation did not arrive')), 10000) })]) } finally { clearTimeout(timer) }
    assert.ok(h.requests.some(r => r.parent === 'root-project' && r.projectOffset === 0 && r.taskOffset === 0))
    assert.ok(await h.page.locator('[data-hierarchy-key]').count() <= 31)
    held.release()
    await h.idle()
    const branch = h.requests.filter(r => r.parent === 'root-project' && r.path.endsWith('/navigation'))
    assert.deepEqual(branch.map(r => [r.projectOffset, r.taskOffset]), [[0, 0], [50, 50], [50, 100]], JSON.stringify(h.requests.filter(r => r.path.endsWith('/navigation'))))
    assert.equal(branch.at(-1).projectHasMore, false); assert.equal(branch.at(-1).taskHasMore, false)
    const deep = h.requests.filter(r => r.parent === 'task-000' && r.path.endsWith('/navigation'))
    assert.deepEqual(deep.map(r => r.taskOffset), [0, 50, 100])
    for (const key of ['project:project-000', 'project:project-062', 'task:task-000', 'task:subtask-112', 'task:task-120']) await h.scrollTo(key)
    const membership = await h.logicalKeys()
    for (const project of h.fixture.projects) assert.ok(membership.has('project:' + project.id), 'Missing scroll-reachable project ' + project.id)
    for (const task of h.fixture.tasks) assert.ok(membership.has('task:' + task.id), 'Missing scroll-reachable task ' + task.id)
    await h.page.locator('#main-content-scroll').evaluate(element => { element.scrollTop = element.scrollHeight })
    await h.page.getByText('All children loaded', { exact: true }).last().waitFor()
    assert.ok((await h.page.evaluate(() => window.hierarchyMaximum)) <= 31)
    assert.ok((await h.page.evaluate(() => window.hierarchyObserverMaximum)) <= 31)
    assert.ok(h.caps().maxBranches <= 4); assert.ok(h.caps().maxDetails <= 6)
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { await h.close() }
})

test('collapsed continuation replies stay retired and reopening resumes incomplete cached pages', { timeout: 60000 }, async () => {
  const h = await openHierarchy()
  try {
    const held = h.gate(r => r.parent === 'root-project' && r.projectOffset === 50, { hold: true })
    await h.start()
    let timer
    try { await Promise.race([held.arrived, new Promise((_, reject) => { timer = setTimeout(() => reject(new Error('Continuation did not arrive')), 10000) })]) } finally { clearTimeout(timer) }
    await h.page.locator('#entity-root-project .tree-toggle').click()
    await h.page.waitForFunction(() => document.querySelectorAll('.card-project,.card-task,.card-subtask').length === 1)
    const before = h.requests.length
    held.release(); await h.idle()
    assert.equal(h.requests.slice(before).filter(r => r.path.endsWith('/navigation')).length, 0)
    assert.equal(await h.page.locator('#entity-project-062').count(), 0)
    const reopen = h.requests.length
    await h.page.locator('#entity-root-project .tree-toggle').click(); await h.idle()
    const resumed = h.requests.slice(reopen).filter(r => r.parent === 'root-project' && r.path.endsWith('/navigation'))
    assert.equal(resumed[0].projectOffset, 50); assert.equal(resumed[0].taskOffset, 50)
    await h.scrollTo('project:project-000'); await h.scrollTo('project:project-062'); await h.scrollTo('task:task-120')
    assert.ok(h.caps().maxBranches <= 4); assert.ok(h.caps().maxDetails <= 6)
  } finally { await h.close() }
})

test('failed independent continuation pauses with truthful error and explicit Retry completes it', { timeout: 60000 }, async () => {
  const h = await openHierarchy()
  try {
    h.gate(r => r.parent === 'root-project' && r.taskOffset === 50, { error: 500 })
    await h.start(); await h.idle()
    const failed = h.requests.filter(r => r.parent === 'root-project' && r.status === 500)
    assert.equal(failed.length, 1)
    const afterError = h.requests.length
    await h.page.waitForTimeout(100)
    assert.equal(h.requests.length, afterError, 'Failure must pause instead of automatically retrying')
    const status = await h.scrollTo('status:project:root-project')
    assert.equal(await status.getByText('All children loaded', { exact: true }).count(), 0)
    assert.equal(await status.getByRole('button', { name: /Retry/ }).count(), 2, 'The failed mixed request pauses both independent streams')
    await status.getByRole('button', { name: /Retry/ }).last().click(); await h.idle()
    try { await h.scrollTo('task:task-120') } catch (error) { throw new Error(error.message + '\n' + JSON.stringify({ navigation: h.requests.filter(r => r.path.endsWith('/navigation')), keys: [...await h.logicalKeys()], text: (await h.page.locator('body').innerText()).slice(-2000) })) }
    const remaining = await h.scrollTo('status:project:root-project')
    await remaining.getByRole('button', { name: /Retry/ }).click(); await h.idle()
    const complete = await h.scrollTo('status:project:root-project')
    assert.equal(await complete.getByText('All children loaded', { exact: true }).count(), 1)
  } finally { await h.close() }
})

test('filter retirement rejects delayed old membership and physical slots stay bounded', { timeout: 60000 }, async () => {
  const h = await openHierarchy()
  try {
    const held = h.gate(r => r.parent === 'root-project' && r.projectOffset === 50, { hold: true,
      transform(value) { return { ...value, projects: { ...value.projects, items: value.projects.items.map(item => ({ ...item, id: 'retired-' + item.id })) } } } })
    await h.start()
    let timer
    try { await Promise.race([held.arrived, new Promise((_, reject) => { timer = setTimeout(() => reject(new Error('Continuation did not arrive')), 10000) })]) } finally { clearTimeout(timer) }
    await h.page.locator('.filter-bar').getByRole('button', { name: 'Tasks', exact: true }).click()
    assert.equal(held.receipt.done, false)
    assert.ok(h.caps().activeBranches >= 1, 'Held physical admission remains occupied after filter retirement')
    held.release(); await h.idle()
    assert.equal(held.receipt.done, true)
    assert.equal(await h.page.locator('[id^="entity-retired-"]').count(), 0)
    assert.equal(await h.page.locator('.card-project').count(), 0)
    assert.ok(h.caps().maxBranches <= 4); assert.ok(h.caps().maxDetails <= 6)
    await h.page.locator('.filter-bar').getByRole('button', { name: 'All', exact: true }).click(); await h.idle()
    await h.scrollTo('project:project-062')
    const keys = await h.logicalKeys()
    assert.ok(keys.has('project:project-000'), JSON.stringify({ keys: [...keys], navigation: h.requests.filter(r => r.path.endsWith('/navigation')), scroll: await h.page.locator('#main-content-scroll').evaluate(e => ({ top: e.scrollTop, height: e.scrollHeight })) })); assert.ok(keys.has('project:project-062'))
    assert.equal([...keys].some(key => key.includes('retired-')), false, 'Retired membership must stay absent across the complete scrollable logical hierarchy')
    assert.equal(h.requests.some(r => r.path.includes('/retired-')), false)
    assert.equal(await h.page.locator('[id^="entity-retired-"]').count(), 0)
  } finally { await h.close() }
})

test('real Markdown and native Tab cross virtual rows, while removing the tab retires observers', { timeout: 60000 }, async () => {
  const h = await openHierarchy()
  try {
    await h.start(); await h.idle()
    const root = h.page.locator('#entity-root-project')
    await root.locator('.btn-extras-toggle').click()
    const link = root.getByRole('link', { name: 'Markdown link', exact: true })
    await link.waitFor(); await link.focus(); await h.page.keyboard.press('Tab')
    assert.ok(await h.page.evaluate(() => document.activeElement?.closest('[data-hierarchy-key]')?.dataset.hierarchyKey), 'Native Tab must remain in a mounted hierarchy row')
    const next = await h.page.evaluate(() => {
      const rows = [...document.querySelectorAll('[data-hierarchy-key]')].filter(row => row.dataset.hierarchyNext && row.querySelector('button'))
      const row = rows.at(-1)
      const controls = [...row.querySelectorAll('a[href],button,input,textarea,select,[tabindex]')].filter(element => !element.disabled && element.tabIndex >= 0 && element.getClientRects().length)
      controls.at(-1).focus()
      return { current: row.dataset.hierarchyKey, next: row.dataset.hierarchyNext }
    })
    assert.equal(await h.page.locator('[data-hierarchy-key="' + next.next + '"]').count(), 0, 'Keyboard successor must start outside the mounted window')
    await h.page.keyboard.press('Tab')
    await h.page.waitForFunction(key => document.activeElement?.closest('[data-hierarchy-key]')?.dataset.hierarchyKey === key, next.next)
    await h.page.keyboard.press('Shift+Tab')
    await h.page.waitForFunction(key => document.activeElement?.closest('[data-hierarchy-key]')?.dataset.hierarchyKey === key, next.current)
    await h.page.locator('.tabs button').nth(1).click()
    await h.page.locator('#hierarchy-viewport').waitFor({ state: 'detached' })
    await h.page.waitForFunction(() => window.hierarchyObserved.size === 0)
    await h.page.evaluate(() => window.dispatchEvent(new Event('resize')))
    await h.page.waitForTimeout(40)
    assert.equal(await h.page.locator('#hierarchy-viewport').count(), 0)
    assert.deepEqual(h.errors, [])
  } finally { await h.close() }
})

test('duplicate continuation pauses only its nonprogressing stream and explicit Retry reaches the exact partial terminal page', { timeout: 60000 }, async () => {
  const h = await openHierarchy()
  try {
    h.gate(r => r.parent === 'root-project' && r.projectOffset === 50, {
      transform(value) { return { ...value, projects: { ...value.projects, has_more: true, items: value.projects.items.map(item => ({ ...item, id: 'project-000' })) } } }
    })
    await h.start(); await h.idle()
    const status = await h.scrollTo('status:project:root-project')
    assert.match(await status.innerText(), /no new IDs; branch is incomplete/)
    assert.equal(await status.getByRole('button', {name:'Retry', exact:true}).count(), 1)
    const terminalTask = h.requests.filter(r => r.parent === 'root-project' && r.taskOffset === 100).at(-1)
    assert.equal(terminalTask.taskHasMore, false); assert.equal(terminalTask.taskIds.length, 21)
    const before = h.requests.length
    await status.getByRole('button', {name:'Retry', exact:true}).click(); await h.idle()
    const retried = h.requests.slice(before).find(r => r.parent === 'root-project' && r.path.endsWith('/navigation'))
    assert.equal(retried.projectOffset, 50); assert.equal(retried.projectHasMore, false); assert.equal(retried.projectIds.length, 13)
    await h.scrollTo('project:project-062'); await h.scrollTo('task:task-120')
    assert.ok(h.caps().maxBranches <= 4); assert.ok(h.caps().maxDetails <= 6)
  } finally { await h.close() }
})

test('cold collapsed parent stays lazy until reopening, and deep focus keeps an offscreen edit mounted', { timeout: 60000 }, async () => {
  const fixture = hierarchyFixture()
  for (let index=0;index<9;index++) fixture.tasks.push({ ...fixture.tasks[0], id:'deep-'+index, parent_id:index===0?'subtask-112':'deep-'+(index-1), title:'Deep '+index })
  const h = await openHierarchy(fixture)
  try {
    const held = h.gate(r => r.parent === 'root-project' && r.projectOffset === 0, {hold:true})
    await h.start(); await h.bounded(held.arrived, 10000, 'Initial held branch')
    await h.page.locator('#entity-root-project .tree-toggle').click()
    held.release(); await h.idle()
    assert.equal(await h.page.locator('.card-task,.card-subtask').count(), 0)
    assert.equal(h.requests.filter(r => r.parent?.startsWith('task-') || r.parent?.startsWith('subtask-') || r.parent?.startsWith('deep-')).length, 0)
    await h.page.locator('#entity-root-project .tree-toggle').click(); await h.idle()
    await h.page.evaluate(() => { location.hash='tab=projects&focus=task:deep-8' })
    await h.page.locator('#entity-deep-8').waitFor()
    assert.ok(await h.page.locator('.focus-breadcrumb-bar').count())
    assert.ok(await h.page.locator('[data-hierarchy-key]').count() <= 31)
    await h.page.evaluate(() => { location.hash='tab=projects' })
    await h.idle(); await h.scrollTo('task:deep-8')
    await h.page.evaluate(() => { window.editEvents=[]; for (const name of ['focusin','focusout','blur','hashchange']) window.addEventListener(name, event=>window.editEvents.push({name,id:event.target?.id,related:event.relatedTarget?.id,connected:event.target?.isConnected,hash:location.hash,time:performance.now()}),true) })
    await h.page.locator('#entity-deep-8 .editable-text').first().click()
    const input = h.page.locator('#entity-deep-8 .inline-edit-input')
    await input.waitFor()
    await input.evaluate(element => element.setSelectionRange(2,5))
    await input.evaluate(element => { window.originalEditInput=element; window.originalEditRow=element.closest('[data-hierarchy-key]'); window.editMoves=[]; for (const name of ['appendChild','insertBefore','removeChild']) { const original=Node.prototype[name]; Node.prototype[name]=function(node,...args) { const tracked=node===window.originalEditRow; if(tracked&&window.editMoves.length<20)window.editMoves.push({name,parent:this.nodeName,before:true,connected:element.isConnected,active:document.activeElement?.id});const result=original.call(this,node,...args);if(tracked&&window.editMoves.length<20)window.editMoves.push({name,parent:this.nodeName,before:false,connected:element.isConnected,active:document.activeElement?.id});return result } } })
    await h.page.locator('#main-content-scroll').evaluate(element => { element.scrollTop=0 })
    await h.page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))))
    await h.page.waitForFunction(() => { const row=document.querySelector('[data-hierarchy-key="task:deep-8"]'),scroll=document.getElementById('main-content-scroll');return row&&row.getBoundingClientRect().top>scroll.getBoundingClientRect().bottom })
    assert.equal(await input.count(), 1, 'Active edit survives leaving the ordinary mounted window: '+JSON.stringify(await h.page.evaluate(()=>({events:window.editEvents,moves:window.editMoves,hash:location.hash,active:document.activeElement?.id,inputConnected:window.originalEditInput.isConnected,rowConnected:window.originalEditRow.isConnected,sameRow:window.originalEditRow===document.querySelector('[data-hierarchy-key="task:deep-8"]'),inputs:[...document.querySelectorAll('.inline-edit-input')].map(element=>element.id)}))))
    assert.ok(await h.page.locator('[data-hierarchy-key]').count() <= 31)
    assert.deepEqual(await input.evaluate(element=>({sameInput:element===window.originalEditInput,sameRow:element.closest('[data-hierarchy-key]')===window.originalEditRow,connected:element.isConnected,selection:[element.selectionStart,element.selectionEnd]})),{sameInput:true,sameRow:true,connected:true,selection:[2,5]})
    assert.deepEqual(await h.page.evaluate(()=>window.editMoves),[], 'Virtual scrolling must never move the active editor into a detached fragment')
    await input.fill('Edited deep task')
    await h.page.locator('.search-input').focus();await h.idle()
    assert.equal(await input.count(),0,'Genuine outside focus still saves and closes the edit')
    assert.equal(h.fixture.tasks.find(item=>item.id==='deep-8').title,'Edited deep task')
    assert.equal(h.requests.filter(r=>r.method==='PUT'&&r.path.endsWith('/deep-8')).length,1)
    assert.ok(h.caps().maxDetails <= 6)
  } finally { await h.close() }
})

function frame(workspace, type, id, action, invalidations, identity) {
  return { schema_version:1, type:'change', event:{ schema_version:1,event_id:identity,scope:'workspace',workspace_id:workspace,occurred_at:'2026-08-30T12:00:00Z',transaction:{id:identity,cause:'rest',request_id:null},actor:{type:'service',id:'controlled-browser'},entity:{type,id,action},invalidations } }
}

test('live create delete and reparent restart offset membership, and sparse resync restores complete scroll reachability', { timeout: 90000 }, async () => {
  const h = await openHierarchy()
  try {
    await h.start(); await h.idle()
    const workspace = h.fixture.workspace.id
    const created = {...h.fixture.tasks[0], id:'live-inserted',title:'Live inserted',priority:100}
    h.fixture.tasks.push(created)
    h.fixture.projects = h.fixture.projects.filter(item => item.id !== 'project-000')
    h.fixture.tasks.find(item => item.id==='task-120').parent_id='task-000'
    const tree={kind:'tree',target:'workspace:'+workspace}
    const before=h.requests.length
    await h.page.evaluate(frames => window.pushHierarchyFrames(frames), [
      frame(workspace,'task',created.id,'created',[{kind:'entity',target:'task:'+created.id},tree],'live-create'),
      frame(workspace,'project','project-000','deleted',[{kind:'entity',target:'project:project-000'},tree],'live-delete'),
      frame(workspace,'task','task-120','updated',[{kind:'entity',target:'task:task-120'},tree],'live-reparent')
    ])
    await h.idle()
    const refresh=h.requests.slice(before).filter(r=>r.path.endsWith('/navigation')&&r.parent==='root-project')
    assert.ok(refresh.some(r=>r.projectOffset===0&&r.taskOffset===0), 'Live offset membership restarts authoritatively')
    const keys=await h.logicalKeys()
    for (const project of h.fixture.projects) assert.ok(keys.has('project:'+project.id), project.id)
    for (const task of h.fixture.tasks) assert.ok(keys.has('task:'+task.id), task.id)
    assert.equal(keys.has('project:project-000'),false)
    const moved=await h.scrollTo('task:task-120')
    assert.equal(await moved.locator('.card-subtask').count(),1)
    assert.equal(await moved.evaluate(element=>element.style.paddingLeft),'40px')
    assert.ok(h.requests.slice(before).some(r=>r.parent==='task-000'&&r.taskIds?.includes('task-120')))
    const resyncBefore=h.requests.filter(r=>r.path.endsWith('/resync')).length
    await h.page.evaluate(() => window.pushHierarchyFrames([{schema_version:1,type:'resync_required'}]))
    await h.idle()
    assert.ok(h.requests.filter(r=>r.path.endsWith('/resync')).length>resyncBefore)
    await h.scrollTo('task:live-inserted');await h.scrollTo('task:task-120');await h.scrollTo('project:project-062')
    assert.ok(h.caps().maxBranches<=4);assert.ok(h.caps().maxDetails<=6)
    assert.ok(await h.page.evaluate(()=>window.hierarchyMaximum)<=31)
  } finally { await h.close() }
})

test('session authorization retires a held old response while preserving physical occupancy until its completion', { timeout: 60000 }, async () => {
  const h=await openHierarchy()
  try {
    const old=h.gate(r=>r.parent==='root-project'&&r.projectOffset===50,{hold:true,transform(value){return {...value,projects:{...value.projects,items:value.projects.items.map(item=>({...item,id:'retired-session-'+item.id}))}}}})
    await h.start();await h.bounded(old.arrived,10000,'Old session continuation')
    const authorization=h.gate(r=>r.path==='/api/v1/session')
    h.setPrincipal('replacement-user')
    await h.page.evaluate(frames=>window.pushHierarchyFrames(frames),[frame(h.fixture.workspace.id,'workspace',h.fixture.workspace.id,'updated',[{kind:'session_authorization',target:'session-authorization'}],'new-session')])
    await h.bounded(authorization.arrived,10000,'Replacement session authorization')
    assert.equal(old.receipt.done,false);assert.ok(h.caps().activeBranches>=1)
    old.release();await h.idle()
    const keys=await h.logicalKeys()
    assert.equal([...keys].some(key=>key.includes('retired-session-')),false)
    await h.scrollTo('project:project-062');await h.scrollTo('task:task-120')
    assert.ok(h.caps().maxBranches<=4);assert.ok(h.caps().maxDetails<=6)
  } finally {await h.close()}
})

test('real workspace navigation rejects held old membership and completes the replacement workspace', { timeout: 60000 }, async () => {
  const original=hierarchyFixture(), replacement=hierarchyFixture()
  replacement.workspace={...replacement.workspace,id:'20000000-0000-4000-8000-000000000002',name:'Replacement workspace'}
  const projectId=id=>id?'replacement-'+id:null, taskId=id=>id?'replacement-'+id:null
  replacement.projects=replacement.projects.map(item=>({...item,id:projectId(item.id),parent_id:projectId(item.parent_id),workspace_id:replacement.workspace.id}))
  replacement.tasks=replacement.tasks.map(item=>({...item,id:taskId(item.id),parent_id:taskId(item.parent_id),project_id:projectId(item.project_id),workspace_id:replacement.workspace.id}))
  const h=await openHierarchy(original,[replacement])
  try {
    const old=h.gate(r=>r.parent==='root-project'&&r.projectOffset===50,{hold:true,transform(value){return {...value,projects:{...value.projects,items:value.projects.items.map(item=>({...item,id:'retired-workspace-'+item.id}))}}}})
    await h.start();await h.bounded(old.arrived,10000,'Old workspace continuation')
    await h.page.locator('.sidebar-nav').getByRole('link',{name:'Replacement workspace'}).click()
    await h.page.locator('#entity-replacement-root-project').waitFor()
    assert.equal(old.receipt.done,false)
    old.release();await h.idle()
    const keys=await h.logicalKeys()
    for (const project of replacement.projects)assert.ok(keys.has('project:'+project.id),project.id)
    for (const task of replacement.tasks)assert.ok(keys.has('task:'+task.id),task.id)
    assert.equal([...keys].some(key=>key.includes('retired-workspace-')||key==='project:root-project'),false)
    assert.ok(h.caps().maxBranches<=4);assert.ok(h.caps().maxDetails<=6)
  }finally{await h.close()}
})

test('native Markdown focus stays on its connected logical row while scrolling outside the ordinary window', {timeout:60000}, async()=>{
  const h=await openHierarchy()
  try{
    await h.start();await h.idle();await h.scrollTo('project:project-005')
    const row=h.page.locator('[data-hierarchy-key="project:project-005"]')
    await row.locator('.btn-extras-toggle').click()
    await h.idle()
    await h.page.evaluate(()=>new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve))))
    const link=row.getByRole('link',{name:'Markdown link',exact:true})
    await link.focus()
    await link.evaluate(element=>{window.nativeLink=element;window.nativeRow=element.closest('[data-hierarchy-key]');window.nativeEvents=[];window.nativeMoves=[];element.addEventListener('blur',event=>window.nativeEvents.push({related:event.relatedTarget?.id??null,connected:element.isConnected}));window.nativeObserver=new MutationObserver(records=>{for(const record of records){for(const kind of ['removedNodes','addedNodes'])for(const node of record[kind])if(node===window.nativeRow||node.contains?.(window.nativeRow))window.nativeMoves.push(kind)}});window.nativeObserver.observe(document.documentElement,{subtree:true,childList:true})})
    await h.page.evaluate(()=>{window.nativeDocumentEvents=[];for(const type of ['focusout','focusin'])document.addEventListener(type,event=>window.nativeDocumentEvents.push({type,target:event.target.tagName,same:event.target===window.nativeLink,related:event.relatedTarget?.tagName??null}));return new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve)))})
    await h.page.locator('#main-content-scroll').evaluate(element=>{element.scrollTop=element.scrollHeight})
    await h.page.evaluate(()=>new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve))))
    await h.page.waitForFunction(()=>document.activeElement===window.nativeLink,undefined,{timeout:1000}).catch(()=>{})
    const actual=await h.page.evaluate(()=>({focused:document.activeElement===window.nativeLink,connected:window.nativeLink.isConnected,sameRow:document.querySelector('[data-hierarchy-key="project:project-005"]')===window.nativeRow,contained:window.nativeRow.contains(window.nativeLink),events:window.nativeEvents,documentEvents:window.nativeDocumentEvents,moves:window.nativeMoves,context:document.getElementById('hierarchy-viewport')?.dataset.hierarchyContext}))
    assert.ok(actual.focused&&actual.connected&&actual.sameRow&&actual.contained,JSON.stringify(actual))
    assert.ok(actual.events.every(event=>event.related===null),'Recovery must not override an intentional focus destination')
    if(actual.events.length) assert.ok(actual.moves.includes('removedNodes')&&actual.moves.includes('addedNodes'),'Temporary blur must correlate with proven same-row movement')
    await h.page.evaluate(()=>window.nativeObserver.disconnect())
    const positions=await h.page.locator('[data-hierarchy-index]').evaluateAll(rows=>rows.map(row=>Number(row.dataset.hierarchyIndex)))
    assert.deepEqual(positions,[...positions].sort((a,b)=>a-b),'Accessible DOM retains logical preorder')
  }finally{await h.close()}
})

test('inline create stays on its connected parent across virtual scrolling and genuine blur still cancels it', {timeout:60000}, async()=>{
  const h=await openHierarchy()
  try{
    await h.start();await h.idle();await h.scrollTo('project:project-005')
    const row=h.page.locator('[data-hierarchy-key="project:project-005"]')
    await row.getByRole('button',{name:'+ Task',exact:true}).click()
    const input=row.locator('.inline-create-input')
    await input.waitFor()
    await h.page.waitForFunction(()=>document.activeElement===document.getElementById('inline-create-input'))
    await input.evaluate(element=>{window.inlineInput=element;window.inlineRow=element.closest('[data-hierarchy-key]')})
    await h.page.locator('#main-content-scroll').evaluate(element=>{element.scrollTop=element.scrollHeight})
    await h.page.evaluate(()=>new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve))))
    assert.deepEqual(await h.page.evaluate(()=>({input:document.getElementById('inline-create-input')===window.inlineInput,row:document.querySelector('[data-hierarchy-key="project:project-005"]')===window.inlineRow,connected:window.inlineInput.isConnected,focused:document.activeElement===window.inlineInput})),{input:true,row:true,connected:true,focused:true})
    await h.page.locator('.search-input').focus()
    await input.waitFor({state:'detached'})
  }finally{await h.close()}
})

test('native drag keeps its original card connected while virtual scrolling and retains logical drop neighbors', {timeout:60000}, async()=>{
  const h=await openHierarchy()
  try{
    await h.start();await h.idle();await h.scrollTo('project:project-005')
    const card=h.page.locator('#entity-project-005')
    await card.locator('.entity-type-label').evaluate(element=>{const scroll=document.getElementById('main-content-scroll');scroll.scrollTop+=element.getBoundingClientRect().top-scroll.getBoundingClientRect().top-scroll.clientHeight/2})
    await h.page.evaluate(()=>new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve))))
    await card.evaluate(element=>{window.dragCard=element;window.dragRow=element.closest('[data-hierarchy-key]');window.dragEvents=[];document.addEventListener('dragstart',event=>window.dragEvents.push({type:'start',id:event.target.closest('[id^="entity-"]')?.id}));document.addEventListener('dragend',event=>window.dragEvents.push({type:'end',id:event.target.closest('[id^="entity-"]')?.id}))})
    const box=await card.locator('.entity-type-label').boundingBox()

    assert.equal(await h.page.evaluate(({x,y})=>document.elementFromPoint(x,y)?.closest('[draggable="true"]')?.closest('[id^="entity-"]')?.id,{x:box.x+box.width/2,y:box.y+box.height/2}),'entity-project-005','Native gesture must hit the actual draggable card')
    await h.page.mouse.move(box.x+box.width/2,box.y+box.height/2);await h.page.mouse.down();await h.page.mouse.move(box.x+box.width/2+40,box.y+box.height/2+20,{steps:5})
    await h.page.waitForFunction(()=>window.dragEvents.some(event=>event.type==='start'&&event.id==='entity-project-005'),undefined,{timeout:5000})
    await h.page.locator('#main-content-scroll').evaluate(element=>{element.scrollTop=element.scrollHeight})
    await h.page.evaluate(()=>new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve))))
    assert.deepEqual(await h.page.evaluate(()=>({card:document.getElementById('entity-project-005')===window.dragCard,row:document.querySelector('[data-hierarchy-key="project:project-005"]')===window.dragRow,connected:window.dragCard.isConnected,ended:window.dragEvents.some(event=>event.type==='end')})),{card:true,row:true,connected:true,ended:false})
    assert.ok(await h.page.locator('[data-hierarchy-key^="drop:"]').count()>0)
    await h.page.keyboard.press('Escape');await h.page.mouse.up()
  }finally{await h.page.mouse.up().catch(()=>{});await h.close()}
})
