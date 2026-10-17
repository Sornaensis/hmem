import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import { LOADING_EFFICIENCY_BASE, LOADING_EFFICIENCY_TASK, LOADING_EFFICIENCY_PROJECT, LOADING_EFFICIENCY_REVISION, evidenceProfile, evidenceOwnerFields, isolatedRecordOutputs, assertEvidenceIdentity, assertEvidenceWrite, prepareEvidenceScratch, createEvidenceOperations } from './evidence-profile.mjs'
import { generateFixture, fixtureHash, navigationBranchResponse, navigationSummariesResponse } from './fixtures.mjs'
import { createNavigationCompletionIndex, HARNESS_CONFIGURATION } from './contracts.mjs'
import { createTracker, fixtureResponder, inputQualification, completedExpandedMembership, assertSettledHierarchyFeedback, liveNavigationInvalidatedOwners, prepareLiveNavigationProof } from './harness.mjs'

const frontend = path.resolve('fixture-root/hmem-server/frontend'), temporary = path.resolve('outside-temporary')
const plan = () => evidenceProfile(LOADING_EFFICIENCY_BASE, frontend, temporary)
const implementation = '1'.repeat(40)
const identity = profile => ({ taskId: profile.task.taskId, ...evidenceOwnerFields(profile), evidenceBaseCommit: profile.baseCommit, measurementRevision: profile.revision,
  sourceProvenance: { reviewBaseCommit: profile.baseCommit, headCommit: implementation }, inputQualification: { implementationCommit: implementation } })

test('loading qualification owns isolated evidence with ordinary unchanged scenarios and fixtures', () => {
  const profile = plan()
  assert.deepEqual([profile.task.taskId, profile.task.projectId, profile.revision], [LOADING_EFFICIENCY_TASK, LOADING_EFFICIENCY_PROJECT, LOADING_EFFICIENCY_REVISION])
  assert.deepEqual([profile.scaling, profile.rendering, profile.planningCounts, profile.dueRemoval], [false, false, false, false])
  assert.equal(profile.strictSourceIdentity, true)
  assert.equal(profile.root, path.join(temporary, 'hmem-loading-efficiency-' + LOADING_EFFICIENCY_TASK))
  assert.equal(profile.trace, path.join(profile.root, 'temporary/large-loading-efficiency-trace.zip'))
  assert.equal(profile.qualifiedRetirement, true)
  assert.equal(new Set(Object.values(profile.files)).size, 5)
  assert.match(profile.retention, /Final verification task .*project .*closure plus 30 days, then dispose/)
  assert.equal(HARNESS_CONFIGURATION.warmups, 2); assert.equal(HARNESS_CONFIGURATION.samples, 5)
  assert.equal(fixtureHash(generateFixture('small')), '0c425c77e5069b8d4cf26dbdebcf94037a22ee977e61d63ab3cceb1a7f090bc4')
  assert.equal(fixtureHash(generateFixture('large')), 'b98d53a1a1ca28641dc90cbb598f3026949edf702f0ae4696614520b5140dd63')
  assert.throws(() => evidenceProfile(LOADING_EFFICIENCY_BASE, frontend, frontend), /outside/)
})

test('loading outputs reject baseline redirects and foreign owner/source identities', () => {
  const profile = plan(), legacy = path.join(frontend, 'perf/final-working-tree.after.v1.json'), trace = path.join(frontend, 'perf/final-working-tree.trace-manifest.v1.json')
  assert.deepEqual(isolatedRecordOutputs(profile, legacy, trace, legacy, trace), { output: profile.files.after, traceManifest: profile.files.traceManifest })
  for (const file of [path.join(frontend, 'perf/baseline.v1.json'), path.join(temporary, 'foreign.json')]) {
    assert.throws(() => isolatedRecordOutputs(profile, file, trace, legacy, trace), /task-owned/)
    assert.throws(() => assertEvidenceWrite(profile, file, '{}'), /Unowned/)
  }
  const record = identity(profile)
  assert.doesNotThrow(() => assertEvidenceIdentity(profile, record))
  for (const field of ['taskId', 'projectId', 'evidenceBaseCommit', 'measurementRevision']) assert.throws(() => assertEvidenceIdentity(profile, { ...record, [field]: 'foreign' }), /identity mismatch/)
  for (const source of [{ ...record.sourceProvenance, reviewBaseCommit: '2'.repeat(40) }, { ...record.sourceProvenance, headCommit: '2'.repeat(40) }]) assert.throws(() => assertEvidenceIdentity(profile, { ...record, sourceProvenance: source }), /identity mismatch/)
  for (const changed of [{ ...profile, root: path.join(temporary, 'foreign') }, { ...profile, scaling: true }, { ...profile, files: { ...profile.files, after: legacy } }]) assert.throws(() => assertEvidenceWrite(changed, changed.files.after, '{}'), /mismatch/)
  assert.throws(() => assertEvidenceWrite(profile, profile.files.after, Buffer.alloc(64 * 1024 * 1024 + 1)), /64 MiB/)
  const inputs = inputQualification({ plan: profile, capture: { read: () => Buffer.from(implementation) } })
  assert.equal(inputs.implementationCommit, implementation)
  assert.ok(inputs.source.some(value => value.path.endsWith('/tests-js/observation-viewport-fixture.mjs')))
})

test('loading persistence requires admitted unchanged directory identity before any write', () => {
  const profile = plan(), entries = new Map([[temporary, { ino: 1 }], [profile.root, { ino: 2 }]])
  let writes = 0
  const io = { existsSync: file => entries.has(file), realpathSync: file => file,
    lstatSync: file => ({ dev: 1, ino: entries.get(file).ino, isSymbolicLink: () => false, isDirectory: () => true, isFile: () => false }),
    readdirSync: () => [], writeFileSync() { writes++ } }
  const operations = createEvidenceOperations(profile, temporary, io)
  assert.throws(() => operations.write(profile.files.after, '{}'), /not been admitted/)
  prepareEvidenceScratch(profile, temporary, io)
  entries.get(profile.root).ino = 3
  assert.throws(() => operations.write(profile.files.after, '{}'), /lifetime changed.*no fallback/)
  assert.equal(writes, 0)
})

test('navigation fixture skips requested kinds before selection and preserves legacy defaults and summary batches', () => {
  const fixture = generateFixture('small'), legacy = navigationBranchResponse(fixture)
  assert.deepEqual(navigationBranchResponse(fixture, { includeProjects: 'true', includeTasks: true }), legacy)
  assert.deepEqual(navigationBranchResponse(fixture, { includeProjects: false }).projects, { items: [], has_more: false })
  assert.deepEqual(navigationBranchResponse(fixture, { includeTasks: 'false' }).tasks, { items: [], has_more: false })
  Object.defineProperty(fixture, 'projects', { get() { throw new Error('skipped project query') } })
  assert.deepEqual(navigationBranchResponse(fixture, { includeProjects: false, includeTasks: false }).projects, { items: [], has_more: false })
  assert.throws(() => navigationBranchResponse(generateFixture('small'), { includeProjects: 'yes' }), /boolean/)
  const fresh = generateFixture('small'), ids = fresh.tasks.slice(0, 3).map(task => task.id)
  assert.deepEqual(navigationSummariesResponse(fresh, [], [...ids].reverse()).tasks.map(task => task.id), [...ids].reverse())
})

test('completion receipts leave skipped kinds pending rather than accepting authoritative emptiness', () => {
  const proof = createNavigationCompletionIndex({ children: {}, rootProjects: ['p'], rootTasks: ['t'], context: 'normal' })
  proof.completeSession(proof.beginSession(), true)
  const request = { owner: 'workspace_root', context: 'normal', projectOffset: 0, taskOffset: 0, projectLimit: 50, taskLimit: 50 }
  const skipped = proof.admit({ ...request, projectIncluded: false, taskIncluded: true })
  assert.deepEqual(Object.keys(skipped.slots), ['task'])
  const previous = { ...skipped.state.kinds.project }
  proof.complete(skipped, { projects: { ids: [], hasMore: false }, tasks: { ids: ['t'], hasMore: false } })
  assert.deepEqual(skipped.state.kinds.project, previous)
  assert.equal(proof.ready(), false)
  const legacy = proof.admit(request)
  assert.deepEqual(Object.keys(legacy.slots), ['project', 'task'])
})

test('actual fixture route records demand, empty skipped payloads and their exact body bytes', async () => {
  const fixture = generateFixture('small'), tracker = createTracker(fixture), respond = fixtureResponder(fixture, tracker)
  const url = `http://perf.local/api/v1/workspaces/${fixture.workspace.id}/navigation?parent_kind=workspace_root&project_limit=50&project_offset=0&task_limit=50&task_offset=0&include_projects=false&include_tasks=true`
  const request = { url: () => url, method: () => 'GET' }
  let body
  await respond({ request: () => request, fulfill: async response => { body = response.body } })
  const receipt = tracker.requests[0], value = JSON.parse(body)
  assert.deepEqual(Object.keys(receipt.proofNavigation.slots), ['task'])
  assert.deepEqual(value.projects, { items: [], has_more: false })
  assert.ok(value.tasks.items.length > 0)
  assert.equal(receipt.bytes, Buffer.byteLength(body))
  assert.equal(tracker.bytes['navigation:branch'], Buffer.byteLength(body))
  assert.ok(JSON.parse(fs.readFileSync('package.json')).scripts.test.includes('tests-js/readiness-batches.browser.test.js'))
})


function replayFixture() {
  const base = generateFixture('small'), project = base.projects[0], task = base.tasks[0]
  return { ...base, projects: [{ ...project, id: 'p', parent_id: null, priority: 0 }, { ...project, id: 'q', parent_id: 'p', priority: 0 }],
    tasks: ['root', 'branch'].flatMap(owner => Array.from({ length: 60 }, (_, i) => ({ ...task, id: owner + String(i).padStart(3, '0'),
      title: owner + String(i).padStart(3, '0'), parent_id: null, project_id: owner === 'root' ? null : 'p', status: 'todo', priority: 0 }))), dependencies: [] }
}

const asymmetricPages = [
  ['workspace_root'], ['workspace_root', null, '&task_offset=50&include_projects=false&include_tasks=true'],
  ['project', 'p'], ['project', 'p', '&task_offset=50&include_projects=false&include_tasks=true']
]

// Build each request with the final unique parameter values before the responder.
async function membershipReplay(fixture, specifications = asymmetricPages, filters = '') {
  const tracker = createTracker(fixture), respond = fixtureResponder(fixture, tracker)
  for (const [parent, id, demand = ''] of specifications) {
    const url = new URL(`http://perf.local/api/v1/workspaces/${fixture.workspace.id}/navigation?parent_kind=${parent}${id ? '&parent_id=' + id : ''}&project_limit=50&project_offset=0&task_limit=50&task_offset=0&priority_mode=any${filters}`)
    for (const [name, value] of new URLSearchParams(demand.replace(/^&/, ''))) url.searchParams.set(name, value)
    const request = { url: () => url.href, method: () => 'GET' }
    await respond({ request: () => request, fulfill: async () => {} })
  }
  return tracker
}

test('membership replay preserves asymmetric root and branch streams with legacy defaults', async () => {
  const fixture = replayFixture(), tracker = await membershipReplay(fixture)
  const result = completedExpandedMembership(tracker, fixture)
  assert.deepEqual([result.projectIds, result.taskIds, result.expandedBranches, result.terminalStreams], [2, 120, 1, 2])
  assert.deepEqual(result.entityKeys, ['project:p', 'project:q', ...fixture.tasks.map(task => 'task:' + task.id)])
  assert.ok(tracker.requests.slice(0, 1).every(request => !new URL(request.url).searchParams.has('include_projects')))
  assert.deepEqual(tracker.requests[1].navigation.projects, { items: [], has_more: false })
  assert.deepEqual(tracker.requests[3].navigation.projects, { items: [], has_more: false })
})

test('root membership permits manual prefixes while expanded branches require terminal coverage', async () => {
  const fixture = replayFixture(), tracker = await membershipReplay(fixture, [asymmetricPages[0], ...asymmetricPages.slice(2)])
  assert.equal(completedExpandedMembership(tracker, fixture).taskIds, 110)
  tracker.requests.pop()
  assert.throws(() => completedExpandedMembership(tracker, fixture), /lacks independent terminal membership/)
})

test('filtered membership retains task ancestors and preserves the project outer gate', async () => {
  const base = replayFixture(), p = base.projects[0], t = base.tasks[0]
  const fixture = { ...base, projects: [p, { ...p, id: 'closed', status: 'completed' }, { ...p, id: 'nested', parent_id: 'p', status: 'paused' }],
    tasks: [{ ...t, id: 'ancestor', project_id: 'p', status: 'done' }, { ...t, id: 'matching', parent_id: 'ancestor', project_id: 'p', status: 'todo' },
      { ...t, id: 'excluded', project_id: 'p', status: 'done' }, { ...t, id: 'under-closed', project_id: 'closed', status: 'todo' }], dependencies: [] }
  const pages = [['workspace_root'], ['project', 'p'], ['task', 'ancestor']]
  const defaults = '&show_empty_projects=true&project_status=active&project_status=paused&task_status=todo&task_status=in_progress&task_status=blocked'
  const tracker = await membershipReplay(fixture, pages, defaults)
  const result = completedExpandedMembership(tracker, fixture)
  assert.deepEqual(result.entityKeys, ['project:p', 'project:nested', 'task:ancestor', 'task:matching'])
  assert.equal(result.projectIds, 2)
  const unfiltered = await membershipReplay(fixture, [...pages, ['project', 'closed']])
  assert.equal(completedExpandedMembership(unfiltered, fixture).taskIds, 4)
  const queried = { ...fixture, projects: fixture.projects.map(item => ({ ...item, name: item.id === 'nested' ? 'needle' : 'container' })),
    tasks: fixture.tasks.map(item => ({ ...item, title: item.id === 'matching' ? 'needle' : 'container' })) }
  const retained = await membershipReplay(queried, pages, defaults + '&query=needle')
  assert.deepEqual(completedExpandedMembership(retained, queried).entityKeys, result.entityKeys)
})

test('membership replay rejects malformed skipped/requested pages, order, terminal state and mixed lifetimes', async () => {
  const fixture = replayFixture(), original = await membershipReplay(fixture)
  const rejects = (change, expression) => {
    const copy = { requests: JSON.parse(JSON.stringify(original.requests.map(({ key, method, url, done, navigation, navigationStatus, navigationFulfilled }) => ({ key, method, url, done, navigation, navigationStatus, navigationFulfilled })))) }
    change(copy.requests)
    assert.throws(() => completedExpandedMembership(copy, fixture), expression)
  }
  for (const index of [1, 3]) {
    rejects(requests => { requests[index].navigation.projects.items = [{ id: 'p' }] }, /Malformed skipped/)
    rejects(requests => { requests[index].navigation.projects.has_more = true }, /Malformed skipped/)
    rejects(requests => { requests[index].navigation.projects.extra = true }, /Malformed skipped/)
  }
  for (const index of [0, 2]) {
    rejects(requests => { requests[index].navigation.tasks.items.pop() }, /membership\/order/)
    rejects(requests => { requests[index].navigation.tasks.items.push({ id: 'extra' }) }, /membership\/order/)
    rejects(requests => { requests[index].navigation.tasks.items.reverse() }, /membership\/order/)
    rejects(requests => { requests[index].navigation.tasks.has_more = false }, /membership\/order|terminal state/)
    rejects(requests => { requests[index].navigation.tasks = { items: [], has_more: false } }, /membership\/order/)
  }
  rejects(requests => { requests[1].url += '&task_status=done' }, /Mixed navigation filter lifetimes/)
  rejects(requests => { requests[1].url = requests[1].url.replace('include_projects=false', 'include_projects=yes') }, /Invalid navigation demand/)
  rejects(requests => { requests[1].done = false }, /Unfulfilled/)
  rejects(requests => { requests[1].navigation.workspace_id = 'foreign' }, /Foreign/)
  rejects(requests => { requests.splice(3, 0, structuredClone(requests[2])) }, /unfinished, repeated, or out-of-order/)
})


test('filtered root continuation keeps authority and current-context epochs without manual demand', async () => {
  const base = replayFixture(), p = base.projects[0]
  const fixture = { ...base, projects: Array.from({ length: 60 }, (_, i) => ({ ...p, id: 'p' + String(i).padStart(3, '0'), name: 'p' + String(i).padStart(3, '0'), status: i === 0 ? 'completed' : 'active' })),
    tasks: base.tasks.filter(task => task.project_id == null) }
  const tracker = createTracker(fixture), respond = fixtureResponder(fixture, tracker)
  const send = async path => {
    const request = { url: () => 'http://perf.local/api/v1/' + path, method: () => 'GET' }
    await respond({ request: () => request, fulfill: async () => {} })
  }
  const root = `workspaces/${fixture.workspace.id}/navigation?parent_kind=workspace_root&project_limit=50&project_offset=0&task_limit=50&task_offset=0&show_empty_projects=true&priority_mode=any`
  const filtered = root + '&project_status=active&project_status=paused&task_status=todo&task_status=in_progress&task_status=blocked'
  const row = { key: 'root-status:project', text: 'Load more projects', status: false, buttons: [{ text: 'Load more projects', disabled: false, continuation: true }] }
  await send(filtered)
  assert.equal(tracker.navigationProof.ready(), false, 'response membership cannot authorize a session')
  assert.throws(() => assertSettledHierarchyFeedback([row], tracker, fixture), /not settled/)
  await send('session'); await send(filtered)
  assert.equal(tracker.navigationProof.ready(), true)
  assert.deepEqual(assertSettledHierarchyFeedback([row], tracker, fixture), { rootContinuationKinds: ['project'] })
  const filteredRequest = tracker.requests.at(-1), count = tracker.requests.length
  assert.equal(completedExpandedMembership({ requests: [filteredRequest] }, fixture).projectIds, 50)
  assert.equal(tracker.requests.length, count, 'proof never consumes root More')
  await send(root)
  assert.equal(tracker.navigationProof.ready(), true)
  assert.deepEqual(assertSettledHierarchyFeedback([row], tracker, fixture), { rootContinuationKinds: ['project'] })
  const latest = tracker.requests.at(-1)
  assert.notEqual(latest.proofNavigation.context, filteredRequest.proofNavigation.context)
  assert.throws(() => completedExpandedMembership(tracker, fixture), /Mixed navigation filter lifetimes/)
  // Reusing an earlier context must retire its former admitted root coverage.
  await send(filtered)
  assert.equal(tracker.navigationProof.ready(), true)
  assert.notEqual(tracker.requests.at(-1).proofNavigation.rootEpoch, filteredRequest.proofNavigation.rootEpoch)
  tracker.requests.at(-1).proofNavigation.retired = true
  assert.throws(() => assertSettledHierarchyFeedback([row], tracker, fixture), /current admitted root/)
  for (const context of [undefined, 'invalid', JSON.stringify([])]) assert.throws(() => tracker.navigationProof.admit({ owner: 'workspace_root', context }))
})


const liveFilters = '&show_empty_projects=true&project_status=active&project_status=paused&task_status=todo&task_status=in_progress&task_status=blocked'
async function initialLiveNavigation(fixture) {
  const tracker = createTracker(fixture), respond = fixtureResponder(fixture, tracker)
  const send = async (kind, id = null, demand = '') => {
    const url = kind === 'session' ? 'http://perf.local/api/v1/session'
      : `http://perf.local/api/v1/workspaces/${fixture.workspace.id}/navigation?parent_kind=${kind}${id ? '&parent_id=' + id : ''}&project_limit=50&project_offset=0&task_limit=50&task_offset=0&priority_mode=any${liveFilters}${demand}`
    const request = { url: () => url, method: () => 'GET' }
    await respond({ request: () => request, fulfill: async () => {} })
    return tracker.requests.at(-1)
  }
  await send('session')
  const first = await send('workspace_root'), pending = [first], seen = new Set()
  for (let index = 0; index < pending.length; index++) for (const kind of ['project', 'task']) for (const summary of pending[index].navigation[kind + 's'].items) {
    const key = kind + ':' + summary.id
    if (summary.has_children && !seen.has(key)) { seen.add(key); pending.push(await send(kind, summary.id)) }
  }
  assert.equal(tracker.navigationProof.ready(), true)
  return { tracker, send, respond }
}

test('live source-mapped unknown DONE child retires its filtered-empty parent and exact ancestor path', async () => {
  const fixture = generateFixture('small'), { tracker, send } = await initialLiveNavigation(fixture)
  const child = fixture.tasks.find(task => task.id.endsWith('000044')), parent = fixture.tasks.find(task => task.id === child.parent_id)
  assert.equal(child.status, 'done'); assert.ok(parent.id.endsWith('000012'))
  assert.equal(tracker.navigationMetadata.has('task:' + child.id), false)
  const earlier = tracker.requests.find(request => new URL(request.url).searchParams.get('parent_id') === parent.id)
  assert.ok(earlier, 'real terminal empty pass exists before the invalidation')
  assert.deepEqual(earlier.navigation.tasks, { items: [], has_more: false })
  const frame = fixture.liveFrames[13]
  assert.equal(frame.event.entity.id, child.id)
  const url = new URL(tracker.requests.find(request => request.key === 'navigation:branch').url)
  const expected = ['project:' + fixture.projects[0].id, 'project:' + fixture.projects[1].id, 'task:' + parent.id, 'workspace_root'].sort()
  assert.deepEqual(liveNavigationInvalidatedOwners(fixture, tracker.navigationMetadata, [frame], url), expected)
  const visible = new Set(tracker.navigationMetadata.keys())
  const owners = prepareLiveNavigationProof(tracker, fixture, fixture.liveFrames)
  assert.ok(owners.includes('task:' + parent.id)); assert.equal(tracker.navigationProof.ready(), false)
  for (const owner of owners.filter(owner => owner !== 'task:' + parent.id && (owner === 'workspace_root' || visible.has(owner)))) {
    const [kind, id] = owner.split(':'); await send(kind, id)
  }
  assert.equal(tracker.navigationProof.ready(), false, 'HTTP idle and old empty coverage cannot prove queued replacement')
  const replacement = await send('task', parent.id)
  assert.deepEqual(replacement.navigation.tasks, { items: [], has_more: false })
  assert.equal(tracker.navigationProof.ready(), true)
})

test('targeted owner retirement rejects stale empty replies, honors collapse and retains unrelated/root authority', () => {
  const proof = createNavigationCompletionIndex({ children: { workspace_root: { projects: [], tasks: ['p', 'other'] },
    'task:p': { projects: [], tasks: [] }, 'task:other': { projects: [], tasks: [] } }, rootProjects: [], rootTasks: ['p', 'other'], context: 'normal', advertisedOwners: ['task:p', 'task:other'] })
  proof.completeSession(proof.beginSession(), true)
  const request = owner => ({ owner, context: 'normal', projectOffset: 0, taskOffset: 0, projectLimit: 50, taskLimit: 50 })
  const empty = { projects: { ids: [], hasMore: false }, tasks: { ids: [], hasMore: false } }
  proof.complete(proof.admit(request('workspace_root')), { ...empty, tasks: { ids: ['p', 'other'], hasMore: false } })
  const old = proof.admit(request('task:p'))
  proof.complete(old, empty); proof.complete(proof.admit(request('task:other')), empty)
  assert.equal(proof.ready(), true)
  proof.invalidateOwners(['task:p'])
  assert.equal(proof.ready(), false)
  proof.complete(old, empty); assert.equal(proof.ready(), false)
  const stale = proof.admit(request('task:p')); proof.invalidateOwners(['task:p']); proof.complete(stale, empty)
  assert.equal(proof.ready(), false)
  const malformed = proof.admit(request('task:p')); proof.complete(malformed, { ...empty, tasks: { ids: [], hasMore: true } })
  assert.equal(proof.ready(), false)
  proof.invalidateOwners(['task:p']); proof.setCollapsed(['task:p']); assert.equal(proof.ready(), true)
  proof.setCollapsed([]); assert.equal(proof.ready(), false)
  proof.complete(proof.admit(request('task:p')), empty)
  assert.equal(proof.ready(), true, 'unrelated owner and manual root coverage survive')
})

test('advertised filtered-empty owners need a first terminal empty pass before completion', () => {
  const proof = createNavigationCompletionIndex({ children: { workspace_root: { projects: [], tasks: ['p'] }, 'task:p': { projects: [], tasks: [] } },
    rootProjects: [], rootTasks: ['p'], context: 'normal', advertisedOwners: ['task:p'] })
  proof.completeSession(proof.beginSession(), true)
  const request = { context: 'normal', projectOffset: 0, taskOffset: 0, projectLimit: 50, taskLimit: 50 }
  proof.complete(proof.admit({ ...request, owner: 'workspace_root' }), { projects: { ids: [], hasMore: false }, tasks: { ids: ['p'], hasMore: false } })
  assert.equal(proof.ready(), false)
  const page = proof.admit({ ...request, owner: 'task:p' })
  proof.complete(page, { projects: { ids: [], hasMore: false }, tasks: { ids: [], hasMore: false } })
  assert.equal(proof.ready(), true)
})

test('owner derivation preserves known unchanged metadata and exact placement/children/query/readiness predicates', async () => {
  const fixture = generateFixture('small'), { tracker } = await initialLiveNavigation(fixture)
  const url = new URL(tracker.requests.find(request => request.key === 'navigation:branch').url)
  const known = fixture.tasks.find(task => task.id.endsWith('000012'))
  const frame = structuredClone(fixture.liveFrames[13]); frame.event.entity.id = known.id; frame.event.invalidations[0].target = 'task:' + known.id
  assert.deepEqual(liveNavigationInvalidatedOwners(fixture, tracker.navigationMetadata, [frame], url), [])
  const changed = structuredClone(fixture); changed.tasks.find(task => task.id === known.id).project_id = fixture.projects[8].id
  const paths = liveNavigationInvalidatedOwners(changed, tracker.navigationMetadata, [frame], url)
  assert.ok(paths.includes('project:' + fixture.projects[1].id)); assert.ok(paths.includes('project:' + fixture.projects[8].id)); assert.ok(paths.includes('workspace_root'))
  const queried = new URL(url); queried.searchParams.set('query', 'needle')
  assert.ok(liveNavigationInvalidatedOwners(fixture, tracker.navigationMetadata, [frame], queried).includes('project:' + known.project_id))
  const previous = new Map(tracker.navigationMetadata); previous.set('task:' + known.id, { ...previous.get('task:' + known.id), has_children: false, direct_subtask_count: 0 })
  assert.ok(liveNavigationInvalidatedOwners(fixture, previous, [frame], url).includes('task:' + known.id))
  const rankFixture = structuredClone(fixture); rankFixture.tasks.find(task => task.id === known.id).status = 'blocked'
  const rankLedger = new Map(tracker.navigationMetadata); rankLedger.set('task:' + known.id, { ...rankLedger.get('task:' + known.id), status: 'blocked', readiness_rollup: { in_progress_subtask_count: 1 } })
  assert.ok(liveNavigationInvalidatedOwners(rankFixture, rankLedger, [frame], url).includes('project:' + known.project_id))
  for (const mutate of [copy => { copy.event.entity.action = 'deleted' }, copy => { copy.event.workspace_id = 'foreign' }, copy => { copy.event.invalidations = [] }]) {
    const copy = structuredClone(frame); mutate(copy)
    assert.throws(() => liveNavigationInvalidatedOwners(fixture, tracker.navigationMetadata, [copy], url), /Unsupported live navigation mutation/)
  }
  const foreign = new URL(url); foreign.pathname = foreign.pathname.replace(fixture.workspace.id, 'foreign')
  assert.throws(() => liveNavigationInvalidatedOwners(fixture, tracker.navigationMetadata, [frame], foreign), /Unsupported live navigation context/)
})


test('metadata ledger rejects unsuccessful and session-retired summary replies', async () => {
  const fixture = generateFixture('small'), tracker = createTracker(fixture), respond = fixtureResponder(fixture, tracker)
  let arrived, release
  const arrival = new Promise(resolve => { arrived = resolve }), held = new Promise(resolve => { release = resolve })
  const taskId = fixture.tasks.find(task => task.id.endsWith('000044')).id
  const summary = { url: () => `http://perf.local/api/v1/workspaces/${fixture.workspace.id}/navigation/summaries`, method: () => 'POST', postDataJSON: () => ({ project_ids: [], task_ids: [taskId] }) }
  const old = respond({ request: () => summary, fulfill: async () => { arrived(); await held } })
  await arrival
  const session = { url: () => 'http://perf.local/api/v1/session', method: () => 'GET' }
  await respond({ request: () => session, fulfill: async () => {} })
  release(); await old
  assert.equal(tracker.navigationMetadata.has('task:' + taskId), false)
  await respond({ request: () => summary, fulfill: async () => {} })
  assert.equal(tracker.navigationMetadata.has('task:' + taskId), true)
  await respond({ request: () => session, fulfill: async () => {} })
  const malformed = { ...summary, postDataJSON: () => ({ project_ids: [], task_ids: Array(101).fill(taskId) }) }
  await respond({ request: () => malformed, fulfill: async () => {} })
  assert.equal(tracker.requests.at(-1).status, 400)
  assert.equal(tracker.navigationMetadata.size, 0)
})

test('root owner invalidation preserves exactly the prior explicit manual demand', () => {
  const ids = Array.from({ length: 100 }, (_, i) => 't' + i)
  for (const more of [false, true]) {
    const children = Object.fromEntries(ids.map(id => ['task:' + id, { projects: [], tasks: [] }]))
    const proof = createNavigationCompletionIndex({ children, rootProjects: [], rootTasks: ids, context: 'normal' })
    proof.completeSession(proof.beginSession(), true)
    const admit = offset => proof.admit({ owner: 'workspace_root', context: 'normal', projectOffset: 0, taskOffset: offset, projectLimit: 50, taskLimit: 50, projectIncluded: offset === 0 })
    const finish = (stamp, offset) => proof.complete(stamp, { projects: { ids: [], hasMore: false }, tasks: { ids: ids.slice(offset, offset + 50), hasMore: offset === 0 } })
    finish(admit(0), 0); if (more) finish(admit(50), 50)
    assert.equal(proof.ready(), true)
    proof.invalidateOwners(['workspace_root']); assert.equal(proof.ready(), false)
    finish(admit(0), 0)
    assert.equal(proof.ready(), !more)
    if (more) { finish(admit(50), 50); assert.equal(proof.ready(), true) }
  }
})


test('live delivery rejects prior summary/focus reads and repeated or ambiguous planning lifetimes', async () => {
  const fixture = generateFixture('small')
  for (const kind of ['summary', 'focus', 'mutation']) {
    const { tracker, respond } = await initialLiveNavigation(fixture)
    if (kind === 'mutation') tracker.requests.push({ key: 'tasks:entity:' + fixture.tasks[0].id, method: 'PATCH', done: true })
    else {
      const request = kind === 'summary'
        ? { url: () => `http://perf.local/api/v1/workspaces/${fixture.workspace.id}/navigation/summaries`, method: () => 'POST', postDataJSON: () => ({ project_ids: [], task_ids: [fixture.tasks[0].id] }) }
        : { url: () => `http://perf.local/api/v1/workspaces/${fixture.workspace.id}/navigation/focus/project/${fixture.projects[0].id}?ancestor_offset=0`, method: () => 'GET' }
      await respond({ request: () => request, fulfill: async () => {} })
    }
    assert.throws(() => prepareLiveNavigationProof(tracker, fixture, fixture.liveFrames), /Unsupported live navigation read\/mutation prefix/)
  }
  const { tracker, respond } = await initialLiveNavigation(fixture)
  let arrived, release
  const arrival = new Promise(resolve => { arrived = resolve }), held = new Promise(resolve => { release = resolve })
  const root = tracker.requests.find(request => request.key === 'navigation:branch')
  const pending = respond({ request: () => ({ url: () => root.url, method: () => 'GET' }), fulfill: async () => { arrived(); await held } })
  await arrival
  assert.throws(() => prepareLiveNavigationProof(tracker, fixture, fixture.liveFrames), /not settled/)
  release(); await pending
  prepareLiveNavigationProof(tracker, fixture, fixture.liveFrames)
  assert.throws(() => prepareLiveNavigationProof(tracker, fixture, fixture.liveFrames), /one-shot/)
})

test('known equal metadata can prepare exactly one zero-navigation-work delivery', async () => {
  const fixture = generateFixture('small'), { tracker } = await initialLiveNavigation(fixture)
  const task = fixture.tasks.find(task => task.id.endsWith('000012')), frame = structuredClone(fixture.liveFrames[13])
  frame.event.entity.id = task.id; frame.event.invalidations[0].target = 'task:' + task.id
  assert.deepEqual(prepareLiveNavigationProof(tracker, fixture, [frame]), [])
  assert.equal(tracker.navigationProof.ready(), true)
  assert.throws(() => prepareLiveNavigationProof(tracker, fixture, [frame]), /one-shot/)
})
