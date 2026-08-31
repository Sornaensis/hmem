import assert from 'node:assert/strict'
import fs from 'node:fs'
import test from 'node:test'
import { fileURLToPath } from 'node:url'
import { assertFiveSamples, HARNESS_CONFIGURATION, hashJson, liveSettleReady, liveTimingSummary, liveWholeWorkspaceReload, nearestRankP95, perfApiRouteKey, renderBudgetEvaluation, representativeReadiness, transportContractReady } from './contracts.mjs'
import { DIRECT_FOCUS_CONTRACT, FIXTURE_SCHEMA_VERSION, FIXTURE_SEED, OBSERVATION_MEASURED_QUERY, TIMELINE_BROWSER_NOW, TIMELINE_BUCKET_RESPONSE_MAX, TIMELINE_BUCKET_SQL_CAP, TIMELINE_DEFAULT_UI_QUERY, TimelineBucketRequestError, deepFocusFixture, directFocusFixture, fixtureHash, generateFixture, navigationBranchResponse, navigationFocusResponse, navigationSummariesResponse, orderedTimelineBuckets, paginate, projectOverviewResponse, projectReadinessRollup, queryObservationFacets, queryObservations, queryProjects, queryTasks, queryTimelineBuckets, queryTimelineEvents, snapshotHash, snapshotItems, stableFixtureJson, taskOverviewResponse, taskReadinessRollup, validateFixture, workspaceShellSnapshotItems } from './fixtures.mjs'

const here = fileURLToPath(new URL('.', import.meta.url))

function independentlyOrderedSnapshot(fixture) {
  const records = [
    ...fixture.observations.map(data => ({ schema_version: 1, kind: 'observation', data })),
    ...fixture.dependencies.map(data => ({ schema_version: 1, kind: 'task_dependency', data })),
    ...fixture.tasks.map(data => ({ schema_version: 1, kind: 'task', data })),
    ...fixture.projects.map(data => ({ schema_version: 1, kind: 'project', data })),
    { schema_version: 1, kind: 'workspace', data: fixture.workspace }
  ]
  const rank = new Map([['workspace', 10], ['project', 20], ['task', 30], ['task_dependency', 40], ['observation', 50]])
  const identity = item => item.kind === 'task_dependency' ? `${item.data.task_id}:${item.data.depends_on_id}` : item.data.id
  return records.sort((left, right) => rank.get(left.kind) - rank.get(right.kind) || identity(left).localeCompare(identity(right), 'en-US'))
}

for (const size of ['small', 'large']) {
  test(`${size} fixture is byte-stable and schema-valid`, () => {
    const first = generateFixture(size)
    const second = generateFixture(size)
    assert.equal(first.schemaVersion, FIXTURE_SCHEMA_VERSION)
    assert.equal(first.seed, FIXTURE_SEED)
    assert.deepEqual(validateFixture(first), [])
    assert.equal(stableFixtureJson(first), stableFixtureJson(second))
    assert.equal(fixtureHash(first), fixtureHash(second))
    assert.equal(fixtureHash(first), {
      small: '827312e056054ad5f144284fe198e22d4f8bac97980afa0ecb3142b9d86e0851',
      large: '1ae208983d613c84b9a96500c477615c0b3e27686ff08f03bb20f15f99b35a08'
    }[size])
  })
}

test('fixture cardinalities, hierarchy, DAG, pagination, direct focus and live mix stay frozen', () => {
  const small = generateFixture('small')
  const large = generateFixture('large')
  assert.deepEqual(
    [small.projects.length, small.tasks.length, small.dependencies.length, small.observations.length, small.timeline.events.length, small.timeline.buckets.length],
    [10, 60, 24, 60, 40, 20]
  )
  assert.deepEqual(
    [large.projects.length, large.tasks.length, large.dependencies.length, large.observations.length, large.timeline.events.length, large.timeline.buckets.length, large.liveFrames.length],
    [250, 1500, 1200, 2000, 1000, 500, 50]
  )
  assert.equal(paginate(large.projects, 0, 200).items.length, 200)
  assert.equal(paginate(large.projects, 200, 200).items.length, 50)
  assert.equal(paginate(large.tasks, 1400, 200).items.length, 100)
  assert.equal(paginate(large.tasks).items.length, 50)
  assert.equal(paginate(large.tasks, 0, 999).items.length, 200)
  assert.ok(large.projects[225])
  assert.ok(large.tasks[1200])
  assert.ok(large.observations[100])
  assert.equal(new Set(large.liveFrames.map(frame => frame.event.entity.type)).size, 3)
  assert.ok(new Set(large.liveFrames.map(frame => `${frame.event.entity.type}:${frame.event.entity.id}`)).size < 50)
  assert.equal(snapshotItems(large).length, 1 + 250 + 1500 + 1200 + 2000)
  for (const fixture of [small, large]) {
    const starts = fixture.timeline.buckets.map(bucket => bucket.bucket_start)
    assert.equal(new Set(starts).size, fixture.timeline.buckets.length)
    fixture.timeline.buckets.forEach((bucket, index) => {
      assert.ok(Date.parse(bucket.bucket_start) < Date.parse(bucket.bucket_end))
      if (index > 0) assert.ok(Date.parse(fixture.timeline.buckets[index - 1].bucket_end) <= Date.parse(bucket.bucket_start))
      for (const field of ['created', 'completed', 'cancelled']) {
        assert.equal(bucket.totals[field], Object.values(bucket.counts).reduce((sum, value) => sum + value[field], 0))
      }
      for (const field of ['created', 'completed', 'deleted']) {
        assert.equal(bucket.series_totals[field], Object.values(bucket.series).reduce((sum, value) => sum + value[field], 0))
      }
    })
  }
})

test('navigation summary batches preserve request order and enforce the shared 100-ID cap', () => {
  const fixture = generateFixture('small')
  const [firstProject, secondProject] = fixture.projects
  const [firstTask, secondTask] = fixture.tasks
  const response = navigationSummariesResponse(fixture, [secondProject.id, 'missing-project', firstProject.id], [secondTask.id, 'missing-task', firstTask.id])
  assert.deepEqual(response.projects.map(item => item.id), [secondProject.id, firstProject.id])
  assert.deepEqual(response.tasks.map(item => item.id), [secondTask.id, firstTask.id])
  assert.deepEqual(response.missing_project_ids, ['missing-project'])
  assert.deepEqual(response.missing_task_ids, ['missing-task'])
  assert.equal(navigationSummariesResponse(fixture, [firstProject.id, firstProject.id], []), null)
  assert.equal(navigationSummariesResponse(fixture, Array.from({ length: 101 }, (_, index) => `missing-${index}`), []), null)
})

test('direct-focus resync preserves canonical ordered bytes and uses an absent root target from empty state', () => {
  for (const [size, fullItems] of [['small', 155], ['large', 4951]]) {
    const fixture = generateFixture(size)
    const canonical = snapshotItems(fixture)
    const direct = directFocusFixture(fixture)
    assert.equal(canonical.length, fullItems)
    assert.deepEqual(direct.snapshots, canonical)
    assert.equal(JSON.stringify(direct.snapshots), JSON.stringify(canonical))
    assert.equal(snapshotHash(fixture), snapshotHash(generateFixture(size)))
    assert.equal(DIRECT_FOCUS_CONTRACT.pauseBeforeItems, 0)
    assert.equal(direct.targetProject.id, DIRECT_FOCUS_CONTRACT[size].targetProjectId)
    assert.equal(direct.targetProject.parent_id, null)
    assert.equal(direct.targetProject.workspace_id, fixture.workspace.id)
    assert.equal(canonical.some(item => item.kind === 'project' && item.data.id === direct.targetProject.id), true)
  }
})

test('workspace_shell_v1 is an explicit bounded production transport with bounded navigation and focus', () => {
  const fixture = generateFixture('large')
  const shell = workspaceShellSnapshotItems(fixture)
  assert.deepEqual(shell, [{ schema_version: 1, kind: 'workspace', data: fixture.workspace }])
  assert.equal(shell.length < snapshotItems(fixture).length, true)
  const root = navigationBranchResponse(fixture, { parentKind: 'workspace_root', projectLimit: 50, taskLimit: 50 })
  assert.equal(root.projects.items.length <= 50 && root.tasks.items.length <= 50, true)
  assert.equal(root.projects.items.every(item => !Object.hasOwn(item, 'description') && !Object.hasOwn(item, 'metadata')), true)
  assert.equal(root.tasks.items.every(item => !Object.hasOwn(item, 'description') && !Object.hasOwn(item, 'metadata')), true)
  const target = DIRECT_FOCUS_CONTRACT.large.targetProjectId
  const focus = navigationFocusResponse(fixture, 'project', target)
  assert.equal(focus.workspace_id, fixture.workspace.id)
  assert.equal(focus.target.summary.id, target)
  assert.equal(focus.ancestors.length <= 64, true)
})

test('deep focus continuation keeps the target and exposes deterministic 64-row ancestor windows', () => {
  const deep = deepFocusFixture()
  const first = navigationFocusResponse(deep.fixture, 'project', deep.targetProject.id, 0)
  const second = navigationFocusResponse(deep.fixture, 'project', deep.targetProject.id, first.next_ancestor_offset)
  const final = navigationFocusResponse(deep.fixture, 'project', deep.targetProject.id, second.next_ancestor_offset)
  assert.equal(deep.ancestorCount, 129)
  assert.equal(first.target.summary.id, deep.targetProject.id)
  assert.equal(second.target.summary.id, deep.targetProject.id)
  assert.equal(final.target.summary.id, deep.targetProject.id)
  assert.deepEqual([first.ancestors.length, second.ancestors.length, final.ancestors.length], [64, 64, 1])
  assert.deepEqual([first.next_ancestor_offset, second.next_ancestor_offset, final.next_ancestor_offset], [64, 128, null])
})

test('navigation fixture mirrors production lifecycle ranks, tree-filter retention, and independently lazy pages', () => {
  const fixture = generateFixture('large')
  const fixtureWithRootPages = {
    ...fixture,
    projects: fixture.projects.map(project => ({ ...project, parent_id: null })),
    tasks: fixture.tasks.map(task => ({ ...task, project_id: null, parent_id: null }))
  }
  const first = navigationBranchResponse(fixtureWithRootPages, { parentKind: 'workspace_root', projectLimit: 100, taskLimit: 100 })
  const secondProjects = navigationBranchResponse(fixtureWithRootPages, { parentKind: 'workspace_root', projectLimit: 100, projectOffset: 100, taskLimit: 100 })
  const secondTasks = navigationBranchResponse(fixtureWithRootPages, { parentKind: 'workspace_root', projectLimit: 100, taskLimit: 100, taskOffset: 100 })
  assert.equal(first.projects.items.length, 100)
  assert.equal(first.tasks.items.length, 100)
  assert.equal(first.projects.has_more, true)
  assert.equal(first.tasks.has_more, true)
  assert.equal(secondProjects.projects.items.length, 100)
  assert.equal(secondTasks.tasks.items.length, 100)
  assert.deepEqual(first.projects.items.map(item => item.id).filter(id => secondProjects.projects.items.some(other => other.id === id)), [])
  assert.deepEqual(first.tasks.items.map(item => item.id).filter(id => secondTasks.tasks.items.some(other => other.id === id)), [])
  assert.equal(first.projects.items.every((item, index, values) => index === 0 || ['active', 'paused', 'completed', 'archived'].indexOf(values[index - 1].status) <= ['active', 'paused', 'completed', 'archived'].indexOf(item.status)), true)
  assert.equal(first.tasks.items.every((item, index, values) => index === 0 || ['todo', 'in_progress', 'blocked', 'done', 'cancelled'].indexOf(values[index - 1].status) <= ['todo', 'in_progress', 'blocked', 'done', 'cancelled'].indexOf(item.status)), true)

  const retainedRoot = fixture.projects.find(project => project.parent_id == null)
  const retainedDescendant = fixture.projects.find(project => project.parent_id && project.parent_id !== retainedRoot.id && project.name.includes('0004'))
  const retained = navigationBranchResponse(fixture, { parentKind: 'workspace_root', query: retainedDescendant.name, projectLimit: 100, taskLimit: 100 })
  assert.equal(retained.projects.items.some(item => item.id === retainedRoot.id), true)
  const taskChild = fixture.tasks.find(task => task.parent_id != null)
  const nestedChild = fixture.tasks.find(task => task.parent_id == null && task.id !== taskChild.parent_id)
  const nestedFixture = {
    ...fixture,
    tasks: fixture.tasks.map(task => task.id === nestedChild.id ? { ...task, parent_id: taskChild.id, project_id: taskChild.project_id, title: 'Nested task retention probe' } : task)
  }
  const retainedTask = navigationBranchResponse(nestedFixture, { parentKind: 'task', parentId: taskChild.parent_id, query: 'retention probe', projectLimit: 100, taskLimit: 100 })
  assert.equal(retainedTask.tasks.items.some(item => item.id === taskChild.id), true)
  const tasksOnly = navigationBranchResponse(fixture, { parentKind: 'workspace_root', showOnly: 'tasks', projectLimit: 100, taskLimit: 100 })
  assert.deepEqual(tasksOnly.projects.items, [])
})

test('canonical snapshot independently follows production kind rank and identity boundaries', () => {
  const expectedBoundaries = {
    small: [
      [0, 'workspace', '10000000-0000-4000-8000-000000000001'],
      [1, 'project', '20000000-0000-4000-8000-000000000001'],
      [10, 'project', '20000000-0000-4000-8000-000000000010'],
      [11, 'task', '30000000-0000-4000-8000-000000000001'],
      [70, 'task', '30000000-0000-4000-8000-000000000060'],
      [71, 'task_dependency', '30000000-0000-4000-8000-000000000002:30000000-0000-4000-8000-000000000001'],
      [94, 'task_dependency', '30000000-0000-4000-8000-000000000060:30000000-0000-4000-8000-000000000038'],
      [95, 'observation', '40000000-0000-4000-8000-000000000001'],
      [154, 'observation', '40000000-0000-4000-8000-000000000060']
    ],
    large: [
      [0, 'workspace', '10000000-0000-4000-8000-000000000002'],
      [1, 'project', '20000000-0000-4000-8000-000000000001'],
      [250, 'project', '20000000-0000-4000-8000-000000000250'],
      [251, 'task', '30000000-0000-4000-8000-000000000001'],
      [1750, 'task', '30000000-0000-4000-8000-000000001500'],
      [1751, 'task_dependency', '30000000-0000-4000-8000-000000000002:30000000-0000-4000-8000-000000000001'],
      [2950, 'task_dependency', '30000000-0000-4000-8000-000000001500:30000000-0000-4000-8000-000000000158'],
      [2951, 'observation', '40000000-0000-4000-8000-000000000001'],
      [4950, 'observation', '40000000-0000-4000-8000-000000002000']
    ]
  }
  for (const size of ['small', 'large']) {
    const fixture = generateFixture(size)
    const actual = snapshotItems(fixture)
    const independentlyExpected = independentlyOrderedSnapshot(fixture)
    assert.deepEqual(actual, independentlyExpected)
    const reversed = { ...fixture, projects: [...fixture.projects].reverse(), tasks: [...fixture.tasks].reverse(), dependencies: [...fixture.dependencies].reverse(), observations: [...fixture.observations].reverse() }
    assert.deepEqual(snapshotItems(reversed), actual)
    for (const [index, kind, identity] of expectedBoundaries[size]) {
      const item = actual[index]
      assert.equal(item.kind, kind)
      assert.equal(kind === 'task_dependency' ? `${item.data.task_id}:${item.data.depends_on_id}` : item.data.id, identity)
    }
  }
})

test('Observation route semantics freeze lexeme AND search, same-subject filtering, rank ties and paging', () => {
  const fixture = generateFixture('large')
  const measured = queryObservations(fixture, { query: OBSERVATION_MEASURED_QUERY, limit: 50 })
  assert.deepEqual(measured.items.map(item => item.id), ['40000000-0000-4000-8000-000000000001'])
  assert.equal(measured.has_more, false)
  assert.deepEqual(queryObservations(fixture, { query: 'Observation 0001', limit: 50 }).items, [])
  assert.deepEqual(queryObservations(fixture, { query: 'Observation 00001 absent', limit: 50 }).items, [])
  const firstSubject = fixture.observations[0].subjects[0]
  assert.deepEqual(queryObservations(fixture, { subjectKind: 'file', subject: firstSubject.subject, limit: 100 }).items, [])
  assert.ok(queryObservations(fixture, { subjectKind: firstSubject.subject_kind, subject: firstSubject.subject, limit: 100 }).items.length > 0)
  const firstPage = queryObservations(fixture, { limit: 50 })
  const secondPage = queryObservations(fixture, { offset: 50, limit: 50 })
  assert.equal(firstPage.items[0].id, '40000000-0000-4000-8000-000000002000')
  assert.equal(firstPage.items.at(-1).id, '40000000-0000-4000-8000-000000001951')
  assert.equal(secondPage.items[0].id, '40000000-0000-4000-8000-000000001950')
  assert.equal(secondPage.items.at(-1).id, '40000000-0000-4000-8000-000000001901')
  assert.equal(firstPage.has_more, true)

  const facets = queryObservationFacets(fixture, { limit: 2 })
  assert.deepEqual(facets.items.map(item => [item.subject_kind, item.subject, item.observation_count]), [
    ['glob', 'src/feature-24/**/*.elm', 54],
    ['glob', 'src/feature-21/**/*.elm', 54]
  ])
  assert.equal(facets.has_more, true)
  assert.equal(queryObservationFacets(fixture, { offset: 2, limit: 2 }).items[0].subject, 'src/feature-18/**/*.elm')
})

test('Timeline routes return newest audit events and ascending bucket pages in production order', () => {
  const fixture = generateFixture('large')
  const firstPage = queryTimelineEvents(fixture, { limit: 50 })
  const secondPage = queryTimelineEvents(fixture, { offset: 50, limit: 50 })
  assert.deepEqual(firstPage.items.slice(0, 3).map(item => item.id), [
    'audit:50000000-0000-4000-8000-000000001000',
    'audit:50000000-0000-4000-8000-000000000999',
    'audit:50000000-0000-4000-8000-000000000998'
  ])
  assert.equal(firstPage.items.at(-1).id, 'audit:50000000-0000-4000-8000-000000000951')
  assert.equal(secondPage.items[0].id, 'audit:50000000-0000-4000-8000-000000000950')
  assert.equal(secondPage.items.at(-1).id, 'audit:50000000-0000-4000-8000-000000000901')
  assert.equal(firstPage.has_more, true)
  assert.ok(firstPage.items[0].occurred_at >= firstPage.items.at(-1).occurred_at)
  assert.equal(firstPage.items.some(item => item.entity_type === 'observation'), false)
  const taskOnly = queryTimelineEvents(fixture, { entityType: 'task', limit: 50 })
  assert.equal(taskOnly.items.every(item => item.entity_type === 'task'), true)

  const buckets = orderedTimelineBuckets(fixture)
  assert.equal(buckets.length, 500)
  assert.equal(buckets[0].bucket_start, '2025-01-01T00:00:00.000Z')
  assert.equal(buckets.at(-1).bucket_start, '2025-01-21T19:00:00.000Z')
  assert.equal(buckets.every((bucket, index) => index === 0 || bucket.bucket_start > buckets[index - 1].bucket_start), true)
})

test('Timeline bucket route honors production UTC granularity, clipping, totals, defaults and cap', () => {
  const fixture = generateFixture('large')
  const assertTotals = bucket => {
    for (const action of ['created', 'completed', 'cancelled']) {
      assert.equal(bucket.totals[action], Object.values(bucket.counts).reduce((sum, counts) => sum + counts[action], 0))
    }
    for (const action of ['created', 'completed', 'deleted']) {
      assert.equal(bucket.series_totals[action], Object.values(bucket.series).reduce((sum, counts) => sum + counts[action], 0))
    }
  }
  const cases = [
    ['day', '2025-01-01T00:00:00Z', '2025-01-04T00:00:00Z', 3, '2025-01-01T00:00:00.000Z', '2025-01-03T00:00:00.000Z'],
    ['week', '2025-01-01T00:00:00Z', '2025-02-01T00:00:00Z', 5, '2024-12-30T00:00:00.000Z', '2025-01-27T00:00:00.000Z'],
    ['month', '2025-01-15T00:00:00Z', '2025-04-01T00:00:00Z', 3, '2025-01-01T00:00:00.000Z', '2025-03-01T00:00:00.000Z'],
    ['quarter', '2024-12-15T00:00:00Z', '2025-07-01T00:00:00Z', 3, '2024-10-01T00:00:00.000Z', '2025-04-01T00:00:00.000Z']
  ]
  for (const [bucket, since, until, count, first, last] of cases) {
    const response = queryTimelineBuckets(fixture, { since, until, bucket })
    assert.equal(response.buckets.length, count)
    assert.equal(response.buckets[0].bucket_start, first)
    assert.equal(response.buckets.at(-1).bucket_start, last)
    assert.equal(response.buckets.every((value, index) => index === 0 || value.bucket_start > response.buckets[index - 1].bucket_start), true)
    response.buckets.forEach(assertTotals)
  }

  const firstHour = queryTimelineBuckets(fixture, { since: '2025-01-01T00:00:00Z', until: '2025-01-01T01:00:00Z', bucket: 'day' }).buckets[0]
  const secondHour = queryTimelineBuckets(fixture, { since: '2025-01-01T01:00:00Z', until: '2025-01-01T02:00:00Z', bucket: 'day' }).buckets[0]
  assert.deepEqual(firstHour.counts, fixture.timeline.buckets[0].counts)
  assert.deepEqual(secondHour.counts, fixture.timeline.buckets[1].counts)
  assert.notDeepEqual(firstHour.counts, secondHour.counts)

  assert.equal(TIMELINE_BROWSER_NOW, '2026-08-30T12:00:00Z')
  const defaultUi = queryTimelineBuckets(fixture, TIMELINE_DEFAULT_UI_QUERY)
  assert.equal(defaultUi.buckets.length, 13)
  assert.equal(defaultUi.buckets[0].bucket_start, '2026-06-01T00:00:00.000Z')
  assert.equal(defaultUi.buckets[0].label, '2026-06-01')
  assert.equal(defaultUi.buckets.at(-1).bucket_start, '2026-08-24T00:00:00.000Z')
  assert.equal(defaultUi.buckets.at(-1).bucket_end, '2026-08-31T00:00:00.000Z')
  assert.equal(defaultUi.buckets.every(bucket => Object.values(bucket.totals).every(count => count === 0) && Object.values(bucket.series_totals).every(count => count === 0)), true)

  const maximum = queryTimelineBuckets(fixture, { since: '2024-01-01T00:00:00Z', until: '2025-01-01T00:00:00Z', bucket: 'day' })
  assert.equal(TIMELINE_BUCKET_RESPONSE_MAX, 366)
  assert.equal(TIMELINE_BUCKET_SQL_CAP, 367)
  assert.equal(maximum.buckets.length, TIMELINE_BUCKET_RESPONSE_MAX)
  assert.throws(
    () => queryTimelineBuckets(fixture, { since: '2024-01-01T00:00:00Z', until: '2025-01-02T00:00:00Z', bucket: 'day' }),
    error => error instanceof TimelineBucketRequestError && error.status === 400 && error.observedRows === TIMELINE_BUCKET_SQL_CAP
  )
  assert.throws(() => queryTimelineBuckets(fixture, { since: '2025-01-01T00:00:00Z', until: '2025-01-02T00:00:00Z', bucket: 'hour' }), /bucket must be one of/)
  assert.throws(() => queryTimelineBuckets(fixture, { until: '2025-01-02T00:00:00Z', bucket: 'day' }), /since is required/)
  assert.throws(() => queryTimelineBuckets(fixture, { since: '2025-01-02T00:00:00Z', until: '2025-01-02T00:00:00Z', bucket: 'day' }), /since must be before until/)
})

test('Project, Task, entity and overview DTOs retain production list ordering and shapes', () => {
  const fixture = generateFixture('large')
  assert.deepEqual(queryProjects(fixture, { limit: 3 }).items.map(item => item.id), [
    '20000000-0000-4000-8000-000000000010',
    '20000000-0000-4000-8000-000000000020',
    '20000000-0000-4000-8000-000000000030'
  ])
  assert.deepEqual(queryTasks(fixture, { limit: 3 }).items.map(item => item.id), [
    '30000000-0000-4000-8000-000000000010',
    '30000000-0000-4000-8000-000000000020',
    '30000000-0000-4000-8000-000000000030'
  ])
  assert.deepEqual(fixture.projects[0].metadata, {})
  assert.deepEqual(fixture.tasks[0].metadata, {})
  assert.equal(Object.hasOwn(fixture.tasks[0], 'memory_link_count'), false)
  const project = fixture.projects.find(item => item.id === '20000000-0000-4000-8000-000000000002')
  const overview = projectOverviewResponse(fixture, project.id)
  assert.equal(overview.tasks.every(task => task.project_id === project.id), true)
  assert.equal(overview.subprojects.every(child => child.parent_id === project.id), true)
  assert.deepEqual(overview.tasks, queryTasks(fixture, { projectId: project.id, limit: 200 }).items)
  assert.deepEqual(taskOverviewResponse(fixture, fixture.dependencies[0].task_id).dependencies, [...taskOverviewResponse(fixture, fixture.dependencies[0].task_id).dependencies].sort((left, right) => left.name.localeCompare(right.name, 'en-US')))
})

test('intercepted measured-route semantics remain anchored to production SQL contracts', () => {
  const snapshotSource = fs.readFileSync(fileURLToPath(new URL('../../src/HMem/Server/Snapshot.hs', import.meta.url)), 'utf8')
  const observationSource = fs.readFileSync(fileURLToPath(new URL('../../../hmem-core/src/HMem/DB/Observation.hs', import.meta.url)), 'utf8')
  const timelineSource = fs.readFileSync(fileURLToPath(new URL('../../../hmem-core/src/HMem/DB/Timeline.hs', import.meta.url)), 'utf8')
  const projectSource = fs.readFileSync(fileURLToPath(new URL('../../../hmem-core/src/HMem/DB/Project.hs', import.meta.url)), 'utf8')
  const taskSource = fs.readFileSync(fileURLToPath(new URL('../../../hmem-core/src/HMem/DB/Task.hs', import.meta.url)), 'utf8')
  const workspaceSource = fs.readFileSync(fileURLToPath(new URL('../../../hmem-core/src/HMem/DB/Workspace.hs', import.meta.url)), 'utf8')
  const membershipSource = fs.readFileSync(fileURLToPath(new URL('../../../hmem-core/src/HMem/DB/Auth.hs', import.meta.url)), 'utf8')
  const harnessSource = fs.readFileSync(fileURLToPath(new URL('./harness.mjs', import.meta.url)), 'utf8')

  assert.match(snapshotSource, /ORDER BY kind_rank, identity/)
  assert.match(snapshotSource, /td\.task_id::text \|\| ':' \|\| td\.depends_on_id::text/)
  assert.match(observationSource, /search_vector @@ plainto_tsquery\('simple', \$5\)/)
  assert.match(observationSource, /ts_rank\(search_vector, plainto_tsquery\('simple', \$5\)\).*DESC/)
  assert.match(observationSource, /o\.updated_at DESC, o\.id DESC/)
  assert.match(observationSource, /COUNT\(DISTINCT o\.id\) DESC, MAX\(o\.updated_at\) DESC, s\.subject_kind::text ASC, s\.subject ASC/)
  assert.match(timelineSource, /ORDER BY e\.occurred_at DESC, e\.audit_id DESC/)
  assert.match(timelineSource, /date_trunc\(\$4, \$2 AT TIME ZONE 'UTC'\)/)
  assert.match(timelineSource, /date_trunc\(\$4, \(\$3 - interval '1 microsecond'\) AT TIME ZONE 'UTC'\)/)
  assert.match(timelineSource, /ORDER BY b\.bucket_start_utc ASC/)
  assert.match(timelineSource, /LIMIT 367/)
  assert.match(projectSource, /row\.projPriority.*desc.*row\.projName.*asc.*row\.projId.*asc/)
  assert.match(taskSource, /row\.taskPriority.*desc.*row\.taskTitle.*asc.*row\.taskId.*asc/)
  assert.match(workspaceSource, /ORDER BY name ASC, id ASC/)
  assert.match(membershipSource, /ORDER BY created_at ASC, user_id ASC/)
  for (const helper of ['queryProjects', 'queryTasks', 'queryObservations', 'queryObservationFacets', 'queryTimelineEvents', 'queryTimelineBuckets']) {
    assert.match(harnessSource, new RegExp(`\\b${helper}\\b`))
  }
  assert.match(harnessSource, /unimplemented perf API route/)
})

test('overview fixtures match decoder dependency schema and recursive readiness semantics', () => {
  const fixture = {
    projects: [
      { id: 'p0', parent_id: null, status: 'active' },
      { id: 'p1', parent_id: 'p0', status: 'completed' },
      { id: 'p2', parent_id: 'p1', status: 'active' },
      { id: 'p3', parent_id: null, status: 'completed' }
    ],
    tasks: [
      { id: 't0', title: 'Done root', parent_id: null, project_id: 'p0', status: 'done' },
      { id: 't1', title: 'Open child', parent_id: 't0', project_id: 'p0', status: 'todo' },
      { id: 't2', title: 'Done nested project', parent_id: null, project_id: 'p1', status: 'done' },
      { id: 't3', title: 'Blocked dependency', parent_id: null, project_id: 'p2', status: 'blocked' },
      { id: 't4', title: 'Ready project task', parent_id: null, project_id: 'p3', status: 'done' }
    ],
    dependencies: [{ task_id: 't1', depends_on_id: 't3' }]
  }
  assert.deepEqual(taskReadinessRollup(fixture, 't0'), {
    open_subtask_count: 1, done_subtask_count: 0, cancelled_subtask_count: 0, blocked_subtask_count: 0,
    dependency_blocked_task_count: 1, open_dependency_count: 1, completion_ready: false
  })
  assert.equal(taskReadinessRollup(fixture, 't2').completion_ready, true)
  assert.deepEqual(taskOverviewResponse(fixture, 't1').dependencies, [{ id: 't3', name: 'Blocked dependency' }])
  assert.deepEqual(projectReadinessRollup(fixture, 'p0'), {
    open_project_count: 1, closed_project_count: 1, open_task_count: 2, done_task_count: 2,
    cancelled_task_count: 0, blocked_task_count: 1, dependency_blocked_task_count: 1,
    open_dependency_count: 1, completion_ready: false
  })
  assert.equal(projectReadinessRollup(fixture, 'p3').completion_ready, true)
})

test('nearest-rank p95 and live whole-workspace reload classification are frozen', () => {
  assert.equal(nearestRankP95([1, 2, 3, 4, 5]), 5)
  assert.equal(liveWholeWorkspaceReload({ 'change-stream:resync': 1 }), true)
  assert.equal(liveWholeWorkspaceReload({ 'projects:list': 1 }), true)
  assert.equal(liveWholeWorkspaceReload({ 'tasks:entity:t1': 1 }), false)
  assert.throws(() => nearestRankP95([]), /non-empty finite samples/)
  assert.throws(() => assertFiveSamples([1, 2, 3, 4], 'missing scenario'), /exactly 5 finite samples/)
})

test('transport/anchor readiness is independent of full or virtualized DOM cardinality while render budgets remain exact', () => {
  const signals = { modelComplete: true, activeRequests: 0, loading: false, focused: false, anchorVisible: true }
  assert.equal(representativeReadiness({ ...signals, renderedRows: 1750 }), true)
  assert.equal(representativeReadiness({ ...signals, renderedRows: 180 }), true)
  assert.equal(representativeReadiness({ ...signals, anchorVisible: false }), false)
  assert.equal(representativeReadiness({ ...signals, modelComplete: false }), false)
  const limits = { maxDomNodes: 2500, maxCollectionRows: 250 }
  assert.deepEqual(renderBudgetEvaluation({ nodes: 66996, rows: 1750 }, limits), { nodesPass: false, rowsPass: false, passed: false })
  assert.deepEqual(renderBudgetEvaluation({ nodes: 2200, rows: 180 }, limits), { nodesPass: true, rowsPass: true, passed: true })
  const current = { protocol: 'current-full', transportedItems: 4951, expectedItems: 4951, fullBackingItems: 4951, transportedPages: 50, expectedPages: 50, complete: true }
  assert.equal(transportContractReady(current), true)
  assert.equal(transportContractReady({ ...current, transportedItems: 278, transportedPages: 3 }), false)
  assert.equal(transportContractReady({ ...current, expectedItems: 278, transportedItems: 278, transportedPages: 3, expectedPages: 3 }), false)
  const dormantBounded = { protocol: 'bounded', transportedItems: 278, expectedItems: 278, fullBackingItems: 4951, transportedPages: 3, expectedPages: 3, complete: true }
  assert.equal(transportContractReady(dormantBounded), true)
  assert.equal(transportContractReady({ ...dormantBounded, transportedItems: 277 }), false)
  assert.equal(transportContractReady({ ...dormantBounded, complete: false }), false)
})

test('live settle excludes stability and API interception distinguishes exact known and unknown routes', () => {
  assert.deepEqual(liveTimingSummary({ startedAt: 100, settledAt: 225, stabilityStartedAt: 225, stabilityEndedAt: 725 }), { settleMs: 125, stabilityMs: 500 })
  assert.throws(() => liveTimingSummary({ startedAt: 100, settledAt: 90, stabilityStartedAt: 90, stabilityEndedAt: 590 }), /finite and monotonic/)
  assert.equal(perfApiRouteKey('http://perf.local/api/v1/change-stream/resync', 'POST'), 'change-stream:resync')
  assert.equal(perfApiRouteKey('http://perf.local/api/v1/workspaces/ws/timeline/buckets?bucket=hour', 'GET'), 'timeline:buckets')
  assert.equal(perfApiRouteKey('http://perf.local/api/v1/observations/match', 'POST'), 'observations:match')
  assert.equal(perfApiRouteKey('http://perf.local/api/v1/unknown', 'GET'), null)
  assert.equal(perfApiRouteKey('http://perf.local/api/v1/change-stream/resync', 'GET'), null)
  const settled = { dispatchTurnComplete: true, activeRequests: 0, loading: false, focused: false, anchorVisible: true }
  assert.equal(liveSettleReady({ ...settled, followUpRequests: 0 }), true)
  assert.equal(liveSettleReady({ ...settled, followUpRequests: 7 }), true)
  assert.equal(liveSettleReady({ ...settled, followUpRequests: 7, activeRequests: 1 }), false)
})

test('checked budget and baseline schemas carry the required provenance', () => {
  const budgets = JSON.parse(fs.readFileSync(`${here}/budgets.v1.json`, 'utf8'))
  assert.deepEqual(budgets, {
    schemaVersion: 1,
    environmentPolicy: 'timing-and-heap-require-recorded-environment-fingerprint',
    large: {
      cold: { maxHttpRequests: 12, maxFixtureBytes: 2097152, maxMedianMs: 2000, maxP95Ms: 3500, maxEntityOverviewRequests: 0, requiredCanonicalSnapshotItems: 1, requiredCanonicalSnapshotPages: 1 },
      scaling: { maxSmallToLargeRequestDelta: 3 },
      render: { maxDomNodes: 2500, maxCollectionRows: 250 },
      localInteraction: { maxP95Ms: 100 },
      directFocus: { maxP95Ms: 500, maxRequests: 2, requireRequested: true, requireRendered: true },
      observationLoadMore: { maxP95Ms: 500, maxRequests: 2 },
      liveBatch: { frameCount: 50, maxFollowUpRequests: 12, maxP95Ms: 500, allowWholeWorkspaceReload: false, maxDuplicateRequestsPerRepeatedTarget: 0 },
      heap: { maxAttributableBytes: 67108864 }
    }
  })
  assert.equal(hashJson(budgets), 'bcf994aeb8074f53a75ab89ce24ee17c4059db5cb1e3e7061934641321b2da90')
  assert.equal(HARNESS_CONFIGURATION.browserClockUtc, TIMELINE_BROWSER_NOW)
  assert.equal(hashJson(HARNESS_CONFIGURATION), '6154f092da5dcd381a1fa7ddae7a0719d88f0123cc517184a168b2a39f11fc5c')
  assert.equal(hashJson(DIRECT_FOCUS_CONTRACT), 'e2a132adb8d9386e17be584946a790268ad58e5d10cd9e90d3a4810848864f50')

  const baselinePath = `${here}/baseline.v1.json`
  const traceManifestPath = `${here}/trace-manifest.v1.json`
  const afterPath = `${here}/final-working-tree.after.v1.json`
  const afterTraceManifestPath = `${here}/final-working-tree.trace-manifest.v1.json`
  assert.equal(fs.existsSync(baselinePath), true)
  assert.equal(fs.existsSync(traceManifestPath), true)
  assert.equal(fs.existsSync(afterPath), true)
  assert.equal(fs.existsSync(afterTraceManifestPath), true)
  const baseline = JSON.parse(fs.readFileSync(baselinePath, 'utf8'))
  const traceManifest = JSON.parse(fs.readFileSync(traceManifestPath, 'utf8'))
  const after = JSON.parse(fs.readFileSync(afterPath, 'utf8'))
  const afterTraceManifest = JSON.parse(fs.readFileSync(afterTraceManifestPath, 'utf8'))
  const currentContracts = {
    budgetsHash: 'bcf994aeb8074f53a75ab89ce24ee17c4059db5cb1e3e7061934641321b2da90',
    configurationHash: '6154f092da5dcd381a1fa7ddae7a0719d88f0123cc517184a168b2a39f11fc5c',
    directFocusContractHash: 'e2a132adb8d9386e17be584946a790268ad58e5d10cd9e90d3a4810848864f50',
    fixtures: {
      small: '827312e056054ad5f144284fe198e22d4f8bac97980afa0ecb3142b9d86e0851',
      large: '1ae208983d613c84b9a96500c477615c0b3e27686ff08f03bb20f15f99b35a08'
    },
    snapshots: {
      small: 'c49d1b7d57cd229cae0835e26604f9c526d20be2d6fda4cf1094cf16394ff5f0',
      large: '49222dd4d70fdf34c636188ebd7b5be08fbbddad6391a3276bc57e25214ffa1b'
    }
  }
  const baselineContracts = {
    ...currentContracts,
    budgetsHash: '27c1bd99928e763d6064369c55b9e3781e09fe3b298fa5d346da5e65d74cf77c',
    configurationHash: '4e2d9de5424f938e0ca2ef7bcf252d260f612a8b5e49a8e2a7d6701f644202df'
  }
  assert.equal(baseline.schemaVersion, 1)
  assert.equal(baseline.baseCommit, 'ca04f51c3494c83c433d12bd7791f89be876daea')
  assert.equal(baseline.recordAuthorization, 'explicit --authorize-baseline')
  assert.match(baseline.configuration.initialTransport, /must transport every canonical snapshot item/)
  assert.deepEqual(baseline.contracts, baselineContracts)
  assert.deepEqual(baseline.fixtures.directFocusContract, DIRECT_FOCUS_CONTRACT)
  assert.equal(baseline.fixtures.seed, FIXTURE_SEED)
  assert.equal(baseline.runs.small.length, 5)
  assert.equal(baseline.runs.large.length, 5)
  assert.equal(baseline.runs.large.every(run => run.liveBatch.frames === 50), true)
  assert.equal(baseline.runs.large.every(run => run.cold.canonicalSnapshotItems === 4951 && run.cold.canonicalSnapshotPages === 50), true)
  assert.equal(baseline.runs.large.every(run => run.readiness.protocol === 'current-full' && run.readiness.fullBackingSnapshotItems === 4951 && run.readiness.transportedSnapshotItems === 4951 && run.readiness.transportedSnapshotPages === 50), true)
  assert.equal(baseline.runs.large.every(run => run.readiness.timelineBuckets === 13 && JSON.stringify(run.readiness.timelineBucketRequest) === JSON.stringify(TIMELINE_DEFAULT_UI_QUERY)), true)
  assert.equal(baseline.fixtures.small.scale.timelineBuckets, 20)
  assert.equal(baseline.fixtures.large.scale.timelineBuckets, 500)
  assert.equal(baseline.fixtures.small.snapshotHash, baselineContracts.snapshots.small)
  assert.equal(baseline.fixtures.large.snapshotHash, baselineContracts.snapshots.large)
  assert.equal(baseline.fixtures.small.directFocusTarget.id, DIRECT_FOCUS_CONTRACT.small.targetProjectId)
  assert.equal(baseline.fixtures.large.directFocusTarget.id, DIRECT_FOCUS_CONTRACT.large.targetProjectId)
  assert.equal(baseline.fixtures.small.directFocusTarget.parent_id, null)
  assert.equal(baseline.fixtures.large.directFocusTarget.parent_id, null)
  assert.equal(baseline.runs.large.every(run => run.directFocus.targetAbsentBeforeCanonicalResync && run.directFocus.pausedSnapshotItems === 0 && run.directFocus.pausedSnapshotPages === 0 && run.directFocus.targetParentId == null), true)
  assert.equal(baseline.runs.large.every(run => run.directFocus.canonicalSnapshotItems === 4951 && run.directFocus.canonicalSnapshotPages === 50 && run.directFocus.canonicalSnapshotHash === baselineContracts.snapshots.large && run.directFocus.renderedAfterFullResync), true)
  assert.equal(baseline.runs.large.every(run => typeof run.directFocus.directFocusRequested === 'boolean' && typeof run.directFocus.directFocusRendered === 'boolean'), true)
  assert.equal(baseline.runs.large.every(run => run.directFocus.productFailureReason == null || typeof run.directFocus.productFailureReason === 'string'), true)
  assert.ok(baseline.environment.fingerprint)
  const assertRequestDelta = (delta, label) => {
    assert.deepEqual(Object.keys(delta).sort(), ['bytes', 'count', 'routeBytes', 'routes'], `${label} fields`)
    assert.equal(Number.isInteger(delta.count) && delta.count >= 0, true, `${label} count`)
    assert.equal(Number.isInteger(delta.bytes) && delta.bytes >= 0, true, `${label} bytes`)
    assert.deepEqual(Object.keys(delta.routes).sort(), Object.keys(delta.routeBytes).sort(), `${label} route keys`)
    assert.equal(Object.values(delta.routes).every(value => Number.isInteger(value) && value > 0), true, `${label} route counts`)
    assert.equal(Object.values(delta.routeBytes).every(value => Number.isInteger(value) && value >= 0), true, `${label} route bytes`)
    assert.equal(Object.values(delta.routes).reduce((sum, value) => sum + value, 0), delta.count, `${label} count total`)
    assert.equal(Object.values(delta.routeBytes).reduce((sum, value) => sum + value, 0), delta.bytes, `${label} byte total`)
  }
  for (const size of ['small', 'large']) {
    for (const run of baseline.runs[size]) {
      for (const [name, scenario] of Object.entries(run.interactions)) {
        assert.equal(Number.isFinite(scenario.ms), true, `${size} ${name} latency`)
        assertRequestDelta({ count: scenario.count, bytes: scenario.bytes, routes: scenario.routes, routeBytes: scenario.routeBytes }, `${size} ${name}`)
      }
      assertRequestDelta({ count: run.liveBatch.count, bytes: run.liveBatch.bytes, routes: run.liveBatch.routes, routeBytes: run.liveBatch.routeBytes }, `${size} live batch`)
    }
    for (const [name, values] of Object.entries(baseline.aggregates[size].localInteractions.scenarios)) {
      assert.equal(values.rawMs.length, 5, `${size} ${name} latency samples`)
      assert.equal(values.requestDeltas.length, 5, `${size} ${name} request-delta samples`)
      assert.equal(values.requestCounts.length, 5, `${size} ${name} request-count samples`)
      assert.equal(values.fixtureBytes.length, 5, `${size} ${name} byte samples`)
      assert.equal(values.routeCounts.length, 5, `${size} ${name} route-count samples`)
      assert.equal(values.routeBytes.length, 5, `${size} ${name} route-byte samples`)
      values.requestDeltas.forEach((delta, index) => {
        assertRequestDelta(delta, `${size} ${name} aggregate ${index}`)
        assert.equal(values.requestCounts[index], delta.count, `${size} ${name} aggregate count ${index}`)
        assert.equal(values.fixtureBytes[index], delta.bytes, `${size} ${name} aggregate bytes ${index}`)
        assert.deepEqual(values.routeCounts[index], delta.routes, `${size} ${name} aggregate routes ${index}`)
        assert.deepEqual(values.routeBytes[index], delta.routeBytes, `${size} ${name} aggregate route bytes ${index}`)
      })
    }
    const live = baseline.aggregates[size].liveBatch
    assert.equal(live.requestDeltas.length, 5, `${size} live request-delta samples`)
    assert.equal(live.fixtureBytes.length, 5, `${size} live byte samples`)
    assert.equal(live.routeCounts.length, 5, `${size} live route-count samples`)
    assert.equal(live.routeBytes.length, 5, `${size} live route-byte samples`)
    live.requestDeltas.forEach((delta, index) => {
      assertRequestDelta(delta, `${size} live aggregate ${index}`)
      assert.equal(live.requestCounts[index], delta.count, `${size} live aggregate count ${index}`)
      assert.equal(live.fixtureBytes[index], delta.bytes, `${size} live aggregate bytes ${index}`)
      assert.deepEqual(live.routeCounts[index], delta.routes, `${size} live aggregate routes ${index}`)
      assert.deepEqual(live.routeBytes[index], delta.routeBytes, `${size} live aggregate route bytes ${index}`)
    })
  }
  assert.deepEqual(traceManifest.contracts, baselineContracts)
  assert.deepEqual(traceManifest.trace, baseline.trace)
  assert.equal(baseline.trace.verifiedDuringRecord, true)
  assert.match(baseline.trace.retention, /intentionally removed/)
  assert.ok(Array.isArray(baseline.evaluation.metrics))
  assert.equal(after.schemaVersion, 1)
  assert.equal(after.recordAuthorization, 'explicit --authorize-after-artifact')
  assert.deepEqual(after.configuration, HARNESS_CONFIGURATION)
  assert.deepEqual(after.contracts, currentContracts)
  assert.equal(after.runs.large.every(run => run.cold.canonicalSnapshotItems === 1 && run.cold.canonicalSnapshotPages === 1), true)
  assert.equal(after.runs.large.every(run => run.readiness.protocol === 'workspace_shell_v1' && run.readiness.transportedSnapshotItems === 1 && run.readiness.transportedSnapshotPages === 1), true)
  assert.deepEqual(afterTraceManifest.contracts, currentContracts)
  assert.deepEqual(afterTraceManifest.trace, after.trace)
})
