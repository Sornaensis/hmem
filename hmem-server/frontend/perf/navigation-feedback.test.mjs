import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { generateFixture } from './fixtures.mjs'
import { createTracker, fixtureResponder, hierarchyFeedbackRows, assertSettledHierarchyFeedback, collectSettledHierarchyFeedback, scrollExpandedHierarchy, settledNavigationSource, SETTLED_NAVIGATION_FEEDBACK_CONTRACT } from './harness.mjs'

// Exact production query captured in task51; normalize only origin and fixture workspace.
const CAPTURED_ROOT_QUERY = 'parent_kind=workspace_root&project_limit=50&project_offset=0&task_limit=50&task_offset=0&show_empty_projects=true&priority_mode=any'

function rootRow(kind) {
  return { key: 'root-status:' + kind, text: 'Load more ' + kind + 's', status: false,
    buttons: [{ text: 'Load more ' + kind + 's', disabled: false, continuation: true }] }
}

async function rootTracker(scale = 'large') {
  const fixture = generateFixture(scale), tracker = createTracker(fixture), respond = fixtureResponder(fixture, tracker)
  const url = `http://perf.local/api/v1/workspaces/${fixture.workspace.id}/navigation?${CAPTURED_ROOT_QUERY}`
  const request = { url: () => url, method: () => 'GET' }
  await respond({ request: () => request, fulfill: async () => {} })
  // The root guard consumes the existing independently tested expanded-completion
  // result; these focused tests isolate its row and current root wire corroboration.
  tracker.navigationProof = { ready: () => true }
  return { fixture, tracker, request: tracker.requests[0] }
}

test('settled root project/task pagers remain demand-driven with actual fulfilled admitted ordered prefixes', async () => {
  const { fixture, tracker, request } = await rootTracker()
  assert.equal(request.navigationFulfilled, true); assert.equal(request.navigationStatus, 200)
  assert.equal(request.navigation.projects.items.length, 50); assert.equal(request.navigation.tasks.items.length, 50)
  for (const kind of ['project', 'task']) assert.deepEqual(assertSettledHierarchyFeedback([rootRow(kind)], tracker, fixture), { rootContinuationKinds: [kind] })
  assert.deepEqual(assertSettledHierarchyFeedback([rootRow('project'), rootRow('task')], tracker, fixture), { rootContinuationKinds: ['project', 'task'] })
  assert.deepEqual(assertSettledHierarchyFeedback([], tracker, fixture), { rootContinuationKinds: [] })
  assert.equal(tracker.requests.length, 1, 'qualification must not demand more root pages')
})

test('realistic row snapshots preserve enabled Load-more semantics and bound failure fields', async () => {
  const prior = globalThis.document
  globalThis.document = { querySelectorAll: selector => {
    assert.match(selector, /root-status:/)
    return [{ dataset: { hierarchyKey: 'root-status:task' }, textContent: ' Load more tasks ', querySelector: () => null,
      querySelectorAll: () => [{ textContent: ' Load more tasks ', disabled: false, classList: { contains: value => value === 'navigation-load-more' } }] }]
  } }
  try {
    const rows = hierarchyFeedbackRows(), { fixture, tracker } = await rootTracker()
    assert.deepEqual(rows, [rootRow('task')])
    assert.deepEqual(assertSettledHierarchyFeedback(rows, tracker, fixture).rootContinuationKinds, ['task'])
  } finally { if (prior === undefined) delete globalThis.document; else globalThis.document = prior }
})

test('branch/loading/retry/malformed/unknown/disabled and duplicate root rows all fail', async () => {
  const { fixture, tracker } = await rootTracker()
  for (const patch of [
    { key: 'status:project:any' }, { key: 'root-status:observation' }, { key: 'root-status:task:extra' },
    { text: 'Loading…' }, { text: 'Retry loading tasks' }, { status: true }, { status: undefined },
    { buttons: [] }, { buttons: [rootRow('task').buttons[0], rootRow('task').buttons[0]] },
    { buttons: [{ text: 'Load more tasks', disabled: true, continuation: true }] },
    { buttons: [{ text: 'Retry', disabled: false, continuation: true }] },
    { buttons: [{ text: 'Load more tasks', disabled: false, continuation: false }] }
  ]) assert.throws(() => assertSettledHierarchyFeedback([{ ...rootRow('task'), ...patch }], tracker, fixture), /feedback/)
  assert.throws(() => assertSettledHierarchyFeedback([rootRow('task'), rootRow('task')], tracker, fixture), /feedback/)
})

test('uncorroborated unfinished failed stale false-hasMore and wrong wire prefix requests fail closed', async () => {
  const mutations = [
    ({ tracker }) => { tracker.navigationProof.ready = () => false },
    ({ tracker }) => { tracker.activeRootBranches = 1 },
    ({ tracker }) => { tracker.requests = [] },
    ({ request }) => { request.method = 'POST' },
    ({ request }) => { request.url = request.url.replace('/workspaces/', '/workspaces/foreign-') },
    ({ request }) => { request.url = request.url.replace('show_empty_projects=true', 'show_empty_projects=false') },
    ({ request }) => { request.url = request.url.replace('show_empty_projects=true', 'show_empty=true') },
    ({ request }) => { request.url += '&show_empty=true' },
    ({ request }) => { request.url += '&priority_mode=any' },
    ({ request }) => { request.url = request.url.replace('priority_mode=any', 'priority_mode=exact') },
    ({ request }) => { request.url = request.url.replace('task_limit=50', 'task_limit=49') },
    ({ request }) => { request.done = false },
    ({ request }) => { request.navigationFulfilled = false },
    ({ request }) => { request.navigationStatus = 403 },
    ({ request }) => { request.navigation.tasks.has_more = false },
    ({ request }) => { request.navigation.tasks.items.reverse() },
    ({ request }) => { request.navigation.tasks.items.pop() },
    ({ request }) => { request.proofNavigation.retired = true },
    ({ request }) => { request.proofNavigation.error = true },
    ({ request }) => { request.proofNavigation.context = 'stale filter' },
    ({ request }) => { request.proofNavigation.state.latest = {} },
    ({ request }) => { request.proofNavigation.state.unknown = true },
    ({ request }) => { request.proofNavigation.state.kinds.task.paused = true },
    ({ request }) => { request.proofNavigation.state.kinds.task.pending = true },
    ({ request }) => { request.proofNavigation.state.kinds.task.more = false },
    ({ request }) => { request.proofNavigation.slots.task.limit = 0 },
    ({ request }) => { request.proofNavigation.slots.task.offset = 50 },
    ({ request }) => { request.proofNavigation.slots.task.stream.ids.reverse() },
    ({ request }) => { request.proofNavigation.slots.task.error = true }
  ]
  for (const mutate of mutations) {
    const value = await rootTracker(); mutate(value)
    assert.throws(() => assertSettledHierarchyFeedback([rootRow('task')], value.tracker, value.fixture), /feedback/)
  }
})

test('terminal small roots cannot justify controls and one kind cannot borrow another kind continuation', async () => {
  const small = await rootTracker('small')
  for (const kind of ['project', 'task']) assert.throws(() => assertSettledHierarchyFeedback([rootRow(kind)], small.tracker, small.fixture), /feedback/)
  const large = await rootTracker()
  delete large.request.proofNavigation.slots.task
  assert.deepEqual(assertSettledHierarchyFeedback([rootRow('project')], large.tracker, large.fixture).rootContinuationKinds, ['project'])
  assert.throws(() => assertSettledHierarchyFeedback([rootRow('task')], large.tracker, large.fixture), /remaining/)
})

test('root current lifetime cannot borrow retired prefix and same-kind companion echoes are not admissions', async () => {
  const { fixture, tracker, request } = await rootTracker()
  const latest = { ...request, proofNavigation: { ...request.proofNavigation, rootEpoch: request.proofNavigation.rootEpoch + 1, slots: {} } }
  latest.proofNavigation.state = { ...request.proofNavigation.state, latest: latest.proofNavigation }
  tracker.requests.push(latest)
  assert.throws(() => assertSettledHierarchyFeedback([rootRow('task')], tracker, fixture), /remaining/)
  tracker.requests.pop()
  const echo = { ...request, proofNavigation: { ...request.proofNavigation, slots: {} } }
  echo.proofNavigation.state.latest = echo.proofNavigation
  tracker.requests.push(echo)
  assert.deepEqual(assertSettledHierarchyFeedback([rootRow('task')], tracker, fixture).rootContinuationKinds, ['task'])
})

test('current contract preserves reachability, branch exhaustion, physical/observer/transport caps and bounded diagnostics', () => {
  const source = fs.readFileSync('perf/harness.mjs', 'utf8')
  assert.equal(SETTLED_NAVIGATION_FEEDBACK_CONTRACT.rootPageSize, 50)
  assert.deepEqual(SETTLED_NAVIGATION_FEEDBACK_CONTRACT.rootFilters, { show_empty_projects: 'true', priority_mode: 'any' })
  assert.equal(settledNavigationSource().length, 3)
  const api = fs.readFileSync('src/Api.elm', 'utf8'), loading = fs.readFileSync('src/Feature/DataLoading.elm', 'utf8')
  assert.match(api, /navigation\?parent_kind=/)
  assert.match(api, /&project_limit=50&project_offset=/)
  assert.match(api, /&task_limit=50&task_offset=/)
  assert.match(loading, /&show_empty_projects=/)
  assert.match(loading, /&priority_mode=any/)
  assert.match(source, /settledNavigationFeedbackContractHash: hashJson\(SETTLED_NAVIGATION_FEEDBACK_CONTRACT\)/)
  assert.match(source, /if \(missing.length\) throw new Error\('Expanded members were not scroll-reachable/)
  assert.match(source, /assertCompleteNavigationStream/)
  assert.match(source, /deadline = performance.now\(\) \+ 30000/)
  assert.match(source, /hierarchyRows > 31/)
  assert.match(source, /lifetimeRender\.observers > 31/)
  assert.match(source, /assertNavigationCapacity/)
  assert.equal((source.match(/navigationFeedback: error.navigationFeedback/g) || []).length, 3)
  assert.match(source, /navigationFeedback: failure.navigationFeedback/)
})

test('actual scroll collector validates each raw window before dedup and retains invalid snapshots', async () => {
  const { fixture, tracker } = await rootTracker()
  function pageFor(windows) {
    let position = 0, captured = 0
    return { captures: () => captured,
      evaluate: async callback => callback === hierarchyFeedbackRows ? (captured++, windows[position]) : undefined,
      locator: selector => ({
        evaluateAll: async callback => callback(windows[position].map(row => ({ dataset: { hierarchyKey: row.key } }))),
        evaluate: async callback => {
          assert.equal(selector, '#main-content-scroll')
          if (!callback.toString().includes('scrollTop +=')) return undefined
          if (position === windows.length - 1) return true
          position++; return false
        }
      }) }
  }
  const repeated = pageFor([[rootRow('project')], [rootRow('project')], [rootRow('task')]])
  assert.deepEqual((await scrollExpandedHierarchy(repeated, [], tracker, fixture)).rootContinuationKinds, ['project', 'task'])
  const duplicate = pageFor([[rootRow('task'), rootRow('task')]])
  await assert.rejects(scrollExpandedHierarchy(duplicate, [], tracker, fixture), /feedback/)
  assert.equal(duplicate.captures(), 1)
  const badThenGood = pageFor([[{ ...rootRow('task'), status: true }], [rootRow('task')]])
  await assert.rejects(scrollExpandedHierarchy(badThenGood, [], tracker, fixture), /feedback/)
  assert.equal(badThenGood.captures(), 1, 'bad snapshot fails before a later good snapshot can overwrite it')
  const sticky = new Map()
  assert.throws(() => collectSettledHierarchyFeedback(sticky, [{ ...rootRow('task'), status: true }], tracker, fixture), /feedback/)
  assert.throws(() => collectSettledHierarchyFeedback(sticky, [rootRow('task')], tracker, fixture), /feedback/)
  assert.equal(sticky.size, 0)
})
