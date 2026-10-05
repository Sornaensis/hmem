import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import { createHash } from 'node:crypto'
import { spawnSync } from 'node:child_process'
import test from 'node:test'
import { fileURLToPath } from 'node:url'
import { createColdDiagnostics, createEvidenceCapture, atomicEvidenceWrite, persistEvidenceAttempt, createUsablePaintReadiness, createNavigationCompletionIndex, currentRootNavigationPass, currentNavigationPassComplete, logicalExpandedNavigationComplete, assertNavigationCapacity, retireOwnedResources, assertCompleteNavigationStream, createHierarchyObserverLedger, assertFiveSamples, HARNESS_CONFIGURATION, hashJson, liveSettleReady, liveTimingSummary, liveWholeWorkspaceReload, nearestRankP95, perfApiRouteKey, renderBudgetEvaluation, renderMaximum, representativeReadiness, transportContractReady } from './contracts.mjs'
import { DIRECT_FOCUS_CONTRACT, FIXTURE_SCHEMA_VERSION, FIXTURE_SEED, OBSERVATION_MEASURED_QUERY, TIMELINE_BROWSER_NOW, TIMELINE_BUCKET_RESPONSE_MAX, TIMELINE_BUCKET_SQL_CAP, TIMELINE_DEFAULT_UI_QUERY, TimelineBucketRequestError, deepFocusFixture, directFocusFixture, fixtureHash, generateFixture, navigationBranchResponse, navigationFocusResponse, navigationSummariesResponse, orderedTimelineBuckets, paginate, projectOverviewResponse, projectReadinessRollup, queryObservationFacets, queryObservations, queryProjects, queryTasks, queryTimelineBuckets, queryTimelineEvents, snapshotHash, snapshotItems, stableFixtureJson, taskOverviewResponse, taskReadinessRollup, validateFixture, workspaceShellSnapshotItems } from './fixtures.mjs'

import { untrackedFileDiff, navigateToFirstUsefulViewport, waitForFirstUsefulViewport, retryNavigationKind, createTracker, fixtureResponder } from './harness.mjs'

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
  const expandedBranchMaximum = renderMaximum([{ nodes: 2200, rows: 50 }, { nodes: 2501, rows: 55 }, { nodes: 2200, rows: 50 }])
  assert.deepEqual(expandedBranchMaximum, { nodes: 2501, rows: 55 })
  assert.deepEqual(renderBudgetEvaluation(expandedBranchMaximum, limits), { nodesPass: false, rowsPass: true, passed: false })
  assert.throws(() => renderMaximum([]), /non-empty DOM\/row measurements/)
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
  const settled = { logicalNavigationComplete: true, dispatchTurnComplete: true, activeRequests: 0, loading: false, focused: false, anchorVisible: true }
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
  assert.equal(hashJson(HARNESS_CONFIGURATION), 'b6c007fd0f72c240355646d97a8661948b95cc8fa9649aeb336b1718881bb01f')
  assert.equal(hashJson(DIRECT_FOCUS_CONTRACT), 'e2a132adb8d9386e17be584946a790268ad58e5d10cd9e90d3a4810848864f50')

  const baselinePath = `${here}/baseline.v1.json`
  const traceManifestPath = `${here}/trace-manifest.v1.json`
  assert.equal(fs.existsSync(baselinePath), true)
  assert.equal(fs.existsSync(traceManifestPath), true)
  const baseline = JSON.parse(fs.readFileSync(baselinePath, 'utf8'))
  const traceManifest = JSON.parse(fs.readFileSync(traceManifestPath, 'utf8'))
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
})

test('expanded project/task streams terminate independently with exact authoritative order', () => {
  const ids = Array.from({ length: 120 }, (_, index) => 'project-' + index)
  const page = (offset, values, hasMore) => ({ offset, ids: values, hasMore, limit: 50, done: true })
  const pages = [page(0, ids.slice(0, 50), true), page(50, ids.slice(50, 100), true), page(100, ids.slice(100), false)]
  assert.equal(assertCompleteNavigationStream(pages, ids, 'projects').terminal, true)
  assert.equal(assertCompleteNavigationStream([...pages, pages[2]], ids, 'projects').echoes, 1)
  assert.throws(() => assertCompleteNavigationStream([...pages, { ...pages[2], ids: [] }], ids, 'projects'), /order/)
  const tasks = ['task-1']
  assert.equal(assertCompleteNavigationStream([page(0, tasks, false), { offset: 0, ids: [], hasMore: false, limit: 0, done: true }], tasks, 'tasks').pages, 1)
  assert.throws(() => assertCompleteNavigationStream(pages.slice(0, 2), ids, 'projects'), /terminal/)
  assert.throws(() => assertCompleteNavigationStream([pages[0], pages[0], pages[2]], ids, 'projects'), /repeated|order/)
  assert.throws(() => assertCompleteNavigationStream([{ ...pages[0], ids: [...pages[0].ids].reverse() }, ...pages.slice(1)], ids, 'projects'), /order/)
  assert.throws(() => assertCompleteNavigationStream([{ ...pages[0], done: false }, ...pages.slice(1)], ids, 'projects'), /unfinished/)
  assert.throws(() => assertCompleteNavigationStream([{ ...pages[0], hasMore: false }, ...pages.slice(1)], ids, 'projects'), /terminal/)
})

test('observer high-water retains shared targets through independent disconnects', () => {
  const ledger = createHierarchyObserverLedger()
  const first = {}, second = {}, a = {}, b = {}, c = {}
  ledger.observe(first, a); ledger.observe(first, a); ledger.observe(second, a)
  ledger.observe(first, b); ledger.disconnect(first)
  assert.deepEqual(ledger.metrics(), { current: 1, maximum: 2 })
  ledger.observe(second, b); ledger.observe(second, c)
  assert.deepEqual(ledger.metrics(), { current: 3, maximum: 3 })
  ledger.unobserve(second, a); ledger.disconnect(second)
  assert.deepEqual(ledger.metrics(), { current: 0, maximum: 3 })
})

test('descendant physical admission remains four even after the root request finishes', () => {
  assert.doesNotThrow(() => assertNavigationCapacity({ aggregate: 5, expanded: 4, details: 6 }))
  assert.throws(() => assertNavigationCapacity({ aggregate: 5, expanded: 5, details: 0 }), /four descendants/)
  assert.throws(() => assertNavigationCapacity({ aggregate: 6, expanded: 4, details: 0 }), /aggregate/)
  assert.throws(() => assertNavigationCapacity({ aggregate: 1, expanded: 1, details: 7 }), /six details/)
})

test('finite owned retirement rejects persisted success and still attempts later resources', async () => {
  const retired = []
  const cleanup = await retireOwnedResources([
    { resource: 'stalled browser', close: () => new Promise(() => {}) },
    { resource: 'browser process', close: () => { retired.push('process') } },
    { resource: 'rejected server', close: () => { throw new Error('fixture failure') } },
    { resource: 'trace', close: () => { retired.push('trace') } }
  ], 10)
  assert.equal(cleanup.passed, false)
  assert.deepEqual(retired, ['process', 'trace'])
  assert.deepEqual(cleanup.receipts.map(receipt => receipt.passed), [false, true, false, true])
  assert.match(cleanup.receipts[0].error, /exceeded/)
  const success = await retireOwnedResources([{ resource: 'owned close', close: async () => {} }], 10)
  assert.equal(success.passed, true)
})

test('owned retirement rejects missing or non-callable close callbacks without skipping later resources', async () => {
  const retired = []
  const cleanup = await retireOwnedResources([
    { resource: 'miswired cleanup', cleanup: () => { retired.push('wrong interface') } },
    { resource: 'non-callable close', close: true },
    { resource: 'required later close', close: () => { retired.push('later') } }
  ], 10)
  assert.equal(cleanup.passed, false)
  assert.deepEqual(cleanup.receipts.map(receipt => receipt.passed), [false, false, true])
  assert.match(cleanup.receipts[0].error, /requires a callable close/)
  assert.match(cleanup.receipts[1].error, /requires a callable close/)
  assert.deepEqual(retired, ['later'])
})

test('live logical completion rejects queued descendants, unfinished kinds and newly admitted resets', () => {
  const terminal = { done: true, projectOffset: 0, taskOffset: 0, projectLimit: 50, taskLimit: 50, projects: { ids: [], hasMore: false }, tasks: { ids: ['child'], hasMore: false } }
  const children = { 'project:root': { projects: [], tasks: ['child'] }, 'task:child': { projects: [], tasks: ['grandchild'] }, 'task:grandchild': { projects: [], tasks: [] } }
  const base = { roots: ['project:root'], children, passes: { 'project:root': [terminal] } }
  const physicalIdle = { dispatchTurnComplete: true, activeRequests: 0, loading: false, focused: false, anchorVisible: true, followUpRequests: 1 }
  assert.equal(logicalExpandedNavigationComplete(base), false, 'queued child has not made a physical request')
  assert.equal(liveSettleReady({ ...physicalIdle, logicalNavigationComplete: false }), false)
  const childTerminal = { ...terminal, tasks: { ids: ['grandchild'], hasMore: false } }
  const complete = { ...base, passes: { ...base.passes, 'task:child': [childTerminal] } }
  assert.equal(logicalExpandedNavigationComplete(complete), true)
  assert.equal(liveSettleReady({ ...physicalIdle, logicalNavigationComplete: true }), true)
  assert.equal(logicalExpandedNavigationComplete({ ...complete, passes: { ...complete.passes, 'task:child': [childTerminal, { ...childTerminal, done: false }] } }), false, 'admission retires old terminal before response')
  assert.equal(currentNavigationPassComplete([{ ...childTerminal, projects: { ids: [], hasMore: true } }], [], ['grandchild']), false, 'independent project stream is not terminal')
  assert.equal(logicalExpandedNavigationComplete({ ...base, collapsed: ['task:child'] }), true, 'collapsed child keeps its untouched cache without demanding a pass')
  assert.equal(liveSettleReady(physicalIdle), false, 'missing logical proof cannot report settle')
})

test('fresh root admission cannot borrow older demanded per-kind spans', () => {
  const first = { done: true, projectOffset: 0, taskOffset: 0, projectLimit: 50, taskLimit: 50, projects: { ids: ['p0'] }, tasks: { ids: ['t0'] } }
  const later = { ...first, projectOffset: 50, taskOffset: 100, projects: { ids: ['p50'] }, tasks: { ids: ['t100'] } }
  assert.ok(currentRootNavigationPass([first, later]))
  assert.equal(currentRootNavigationPass([first, later, { ...first, done: false }]), null, 'old root terminal retires at admission before response')
  assert.equal(currentRootNavigationPass([first, later, first]), null, 'unstarted demanded continuation cannot borrow its old page')
  assert.equal(currentRootNavigationPass([first, later, first, { ...later, done: false }]), null, 'admitted unfinished continuation remains incomplete')
  assert.equal(currentRootNavigationPass([first, { ...later, done: true }, first]), null, 'late completion of an older admitted page cannot revive current coverage')
  assert.ok(currentRootNavigationPass([first]), 'undemanded later root spans remain lazy')
  const projectIds = Array.from({ length: 51 }, (_, index) => 'p' + index)
  const companionFirst = { ...first, projects: { ids: projectIds.slice(0, 50), hasMore: true }, tasks: { ids: ['t0'], hasMore: false } }
  const companionNext = { ...companionFirst, projectOffset: 50, projects: { ids: projectIds.slice(50), hasMore: false } }
  assert.ok(currentRootNavigationPass([companionFirst, companionNext]), 'project50/task0 companion echo does not begin a new root pass')
  assert.equal(currentNavigationPassComplete([companionFirst, companionNext], projectIds, ['t0']), true, 'independent terminal task echo remains valid while projects continue')
  const exhausted = { ...first, projects: { ids: ['p0'], hasMore: false }, tasks: { ids: ['t0'], hasMore: false } }
  assert.ok(currentRootNavigationPass([first, later, exhausted]), 'current early exhaustion supersedes older demand beyond its terminal membership')
  assert.equal(currentRootNavigationPass([first, later, first, { ...later, taskLimit: 0 }]), null, 'independent task demand remains unfinished')
  const complete = currentRootNavigationPass([first, later, first, later])
  assert.deepEqual(complete.project.map(page => page.projectOffset), [0, 50])
  assert.deepEqual(complete.task.map(page => page.taskOffset), [0, 100])
})


function referenceAcceptedOwnerComplete(pages, expected) {
  const start = pages.findLastIndex(page => page.projectOffset === 0 && page.taskOffset === 0 && !page.selectedRetryKind)
  if (start < 0) return false
  const state = { project: { offset: 0, pending: true, paused: false }, task: { offset: 0, pending: true, paused: false } }
  const accepted = { project: [], task: [] }
  let unknown = false
  for (const page of pages.slice(start)) {
    const selected = page.selectedRetryKind
    const selectionMatches = selected && state[selected].paused && page.projectOffset === state.project.offset && page.taskOffset === state.task.offset
    const automatic = ['project', 'task'].filter(kind => state[kind].pending && state[kind].offset === page[kind + 'Offset'])
    const eligible = selectionMatches ? [...new Set([selected, ...automatic])] : selected ? [] : automatic
    unknown = eligible.length === 0
    for (const kind of eligible) {
      const stream = page[kind + 's'], offset = page[kind + 'Offset'], limit = page[kind + 'Limit']
      const wanted = expected[kind + 's'].slice(offset, offset + limit)
      const valid = page.done && !page.error && stream && limit === 50
        && JSON.stringify(stream.ids) === JSON.stringify(wanted) && stream.hasMore === (offset + 50 < expected[kind + 's'].length)
      accepted[kind] = accepted[kind].filter(previous => previous.offset !== offset)
      accepted[kind].push({ offset, limit, ids: stream?.ids || [], hasMore: stream?.hasMore, done: page.done, valid })
      if (page.done) state[kind] = { offset: offset + (valid && stream.hasMore ? 50 : 0), pending: !!(valid && stream.hasMore), paused: !valid }
    }
    if (page.done && page.error) for (const kind of ['project', 'task']) state[kind].pending = false
  }
  if (unknown || Object.values(state).some(kind => kind.paused || kind.pending)) return false
  try {
    for (const kind of ['project', 'task']) {
      if (!accepted[kind].length || accepted[kind].some(page => !page.done || !page.valid)) return false
      assertCompleteNavigationStream([...accepted[kind]].sort((a, b) => a.offset - b.offset), expected[kind + 's'], 'source-mask reference ' + kind)
    }
    return true
  } catch { return false }
}


function referenceAcceptedRoots(pages, expected, retiredBefore) {
  const demand = { project: new Set(), task: new Set() }, accepted = { project: new Map(), task: new Map() }
  let state = null, unknown = true, retired = false, acceptedRoot = false, refreshing = false, wholeErrorMask = null
  for (const page of pages) {
    if (!retired && retiredBefore(page)) { state = null; accepted.project.clear(); accepted.task.clear(); retired = true; refreshing = true }
    const selected = page.selectedRetryKind
    if (!state || (!selected && page.projectOffset === 0 && page.taskOffset === 0 && !Object.values(state).some(kind => kind.paused))) {
      refreshing = refreshing || acceptedRoot
      wholeErrorMask = null
      state = { project: { offset: 0, pending: true, paused: false }, task: { offset: 0, pending: true, paused: false } }
      accepted.project.clear(); accepted.task.clear()
    }
    const selectionMatches = selected && state[selected].paused && page.projectOffset === state.project.offset && page.taskOffset === state.task.offset
    const automatic = ['project', 'task'].filter(kind => state[kind].pending && state[kind].offset === page[kind + 'Offset'])
    const manual = !selected && !automatic.length ? ['project', 'task'].filter(kind => !state[kind].paused && page[kind + 'Offset'] > state[kind].offset) : []
    const retryMask = selectionMatches && wholeErrorMask && !Object.values(state).some(kind => kind.pauseReason === 'stream') ? wholeErrorMask : null
    const eligible = selectionMatches ? retryMask || [selected] : selected ? [] : automatic.length ? automatic : manual
    if (eligible.length) wholeErrorMask = null
    unknown = eligible.length === 0
    for (const kind of eligible) {
      const offset = page[kind + 'Offset'], limit = page[kind + 'Limit'], stream = page[kind + 's'], wanted = expected[kind].slice(offset, offset + limit)
      demand[kind].add(offset)
      const valid = page.done && !page.error && stream && limit === 50 && JSON.stringify(stream.ids) === JSON.stringify(wanted) && stream.hasMore === (offset + 50 < expected[kind].length)
      accepted[kind].set(offset, { offset, limit, stream, valid })
      if (page.done) {
        const pending = !!(refreshing && valid && stream.hasMore && [...demand[kind]].some(demanded => demanded >= offset + stream.ids.length))
        state[kind] = { offset: offset + (pending ? 50 : 0), pending, paused: !valid, pauseReason: valid ? null : page.error ? 'http' : 'stream' }
      }
    }
    if (page.done && page.error) { wholeErrorMask = eligible; for (const kind of ['project', 'task']) state[kind].pending = false }
    else if (page.done) acceptedRoot = true
  }
  if (!state || unknown || Object.values(state).some(kind => kind.pending || kind.paused)) return null
  const roots = []
  for (const kind of ['project', 'task']) for (const offset of demand[kind]) {
    const page = accepted[kind].get(offset)
    if (!page) {
      if ([...accepted[kind].values()].some(value => value.valid && !value.stream.hasMore && value.offset < offset && value.offset + value.stream.ids.length <= offset)) continue
      return null
    }
    if (!page.valid) return null
    roots.push(...page.stream.ids.map(id => kind + ':' + id))
  }
  return roots
}

function indexedProofControl(children, rootProjects, rootTasks) {
  let currentChildren = structuredClone(children), collapsed = [], session = 0, authorized = false
  const history = [], normal = 'normal'
  let proofStart = 0, hasSnapshot = false, selectedRetry = null
  const index = createNavigationCompletionIndex({ children: currentChildren, rootProjects, rootTasks, context: normal })
  const reference = () => {
    if (!authorized) return false
    const lifetime = history.filter(page => page.session === session)
    let start = 0, active = null
    for (let i = 0; i < lifetime.length; i++) if (lifetime[i].owner === 'workspace_root' && lifetime[i].context !== active) { active = lifetime[i].context; start = i }
    if (active !== normal) return false
    const current = lifetime.slice(start).filter(page => page.context === normal)
    if (!current.some(page => page.owner === 'workspace_root' && history.indexOf(page) >= proofStart)) return false
    const roots = referenceAcceptedRoots(current.filter(page => page.owner === 'workspace_root'), { project: rootProjects, task: rootTasks }, page => proofStart > 0 && history.indexOf(page) >= proofStart)
    if (!roots) return false
    const passes = {}
    for (const page of current) if (page.owner !== 'workspace_root' && history.indexOf(page) >= proofStart) (passes[page.owner] ||= []).push(page)
    const pending = [...roots], seen = new Set()
    for (let i = 0; i < pending.length; i++) {
      const key = pending[i]
      if (seen.has(key) || collapsed.includes(key)) continue
      seen.add(key)
      const expected = currentChildren[key]
      if (!expected) return false
      if (!expected.projects.length && !expected.tasks.length) continue
      if (!referenceAcceptedOwnerComplete(passes[key] || [], expected)) return false
      pending.push(...expected.projects.map(id => 'project:' + id), ...expected.tasks.map(id => 'task:' + id))
    }
    return true
  }
  const check = expected => {
    const actual = index.ready()
    assert.equal(actual, reference(), 'indexed proof must match independently replayed history')
    if (expected !== undefined) assert.equal(actual, expected)
    assert.equal(index.ready(), actual, 'unchanged cached answer')
  }
  return {
    check,
    session(canRead = true) { const stamp = index.beginSession(); session++; authorized = false; selectedRetry = null; hasSnapshot = false; proofStart = history.length; check(false); index.completeSession(stamp, canRead); authorized = canRead; check(false); return stamp },
    staleSession(stamp) { index.completeSession(stamp, true); check() },
    snapshot(profile = 'workspace_shell_v1') { if (hasSnapshot || profile !== 'workspace_shell_v1') selectedRetry = null; const stamp = index.admitSnapshot(true); index.completeSnapshot(stamp, profile); if (hasSnapshot || profile !== 'workspace_shell_v1') proofStart = history.length; hasSnapshot = true; check() },
    retry(owner, kind) { index.armRetry(index.retrySelection(owner, kind)); selectedRetry = { owner, kind } },
    admit(owner = 'workspace_root', projectOffset = 0, taskOffset = 0, context = normal) {
      const selectedRetryKind = selectedRetry?.owner === owner ? selectedRetry.kind : null
      if (selectedRetryKind) selectedRetry = null
      const page = { owner, projectOffset, taskOffset, projectLimit: 50, taskLimit: 50, context, session, done: false, selectedRetryKind }
      history.push(page); page.stamp = index.admit(page); check(); return page
    },
    complete(page, projects, tasks, status = 200, hasProjectMore = false, hasTaskMore = false) {
      if (page.done || history.findLast(candidate => candidate.owner === page.owner && candidate.session === page.session && candidate.context === page.context) !== page) { index.complete(page.stamp, { projects: { ids: projects, hasMore: hasProjectMore }, tasks: { ids: tasks, hasMore: hasTaskMore } }, status); check(); return }
      page.done = true; page.error = status >= 400
      page.projects = { ids: projects, hasMore: hasProjectMore }; page.tasks = { ids: tasks, hasMore: hasTaskMore }
      index.complete(page.stamp, { projects: page.projects, tasks: page.tasks }, status); check()
    },
    collapse(keys) { selectedRetry = null; collapsed = keys; index.setCollapsed(keys); check() },
    replace(key, value) { selectedRetry = null; currentChildren = { ...currentChildren, [key]: value }; index.replaceChildren(key, value); check() }
  }
}

test('indexed proof agrees with replay through queued descendants, dirty ancestors, errors and stale lifetimes', () => {
  const control = indexedProofControl({
    'project:p': { projects: ['c'], tasks: [] },
    'project:c': { projects: [], tasks: ['t'] },
    'task:t': { projects: [], tasks: [] }
  }, ['p'], [])
  const oldSession = control.session()
  const root = control.admit()
  control.complete(root, ['p'], []); control.check(false)
  const parent = control.admit('project:p')
  control.complete(parent, ['c'], []); control.check(false) // physical idle, child logically queued
  const child = control.admit('project:c')
  control.complete(child, [], ['t']); control.check(true)
  const retired = control.admit('project:c')
  const fresh = control.admit('project:c')
  control.complete(retired, [], ['t']); control.check(false)
  control.complete(fresh, [], ['t']); control.check(true)
  const bad = control.admit('project:c')
  control.complete(bad, [], ['t'], 500); control.check(false)
  control.collapse(['project:c']); control.check(true)
  control.collapse([]); control.check(false)
  const repair = control.admit('project:c')
  control.complete(repair, [], ['t']); control.check(true)
  control.replace('task:t', { projects: [], tasks: ['new'] }); control.check(false)
  control.replace('task:new', { projects: [], tasks: [] }); control.check(false)
  const newlyRequired = control.admit('task:t')
  control.complete(newlyRequired, [], ['new']); control.check(true)
  const filtered = control.admit('workspace_root', 0, 0, 'filtered')
  control.complete(filtered, ['p'], []); control.check(false)
  const normal = control.admit()
  control.complete(normal, ['p'], []); control.check(false) // filter lifetime retires descendant coverage
  control.collapse(['project:p']); control.check(true)
  control.session(false); control.staleSession(oldSession); control.check(false)
  control.session()
  const pending = control.admit()
  control.session()
  control.complete(pending, ['p'], []); control.check(false)
})

test('indexed root admission retires old terminal coverage and preserves independent cohost demand', () => {
  const ids = Array.from({ length: 60 }, (_, i) => 'p' + i)
  const leaves = Object.fromEntries(ids.map(id => ['project:' + id, { projects: [], tasks: [] }]))
  const control = indexedProofControl(leaves, ids, [])
  control.session()
  const zero = control.admit()
  control.complete(zero, ids.slice(0, 50), [], 200, true); control.check(true) // only manually demanded root span
  const fifty = control.admit('workspace_root', 50, 0)
  control.complete(fifty, ids.slice(50), []); control.check(true)
  const held = control.admit('workspace_root', 50, 0)
  const fresh = control.admit()
  control.complete(fresh, ids.slice(0, 50), [], 200, true); control.check(false)
  control.complete(held, ids.slice(50), []); control.check(false) // old completion cannot restore new pass
  const continuation = control.admit('workspace_root', 50, 0)
  control.check(false)
  control.complete(continuation, ids.slice(50), []); control.check(true) // task zero companion doesn't retire project zero
  const malformedRefresh = control.admit()
  control.complete(malformedRefresh, ids.slice(0, 50), [], 200, true)
  const malformed = control.admit('workspace_root', 50, 0)
  control.complete(malformed, ids.slice(50).reverse(), []); control.check(false)
  control.retry('workspace_root', 'project')
  const corrected = control.admit('workspace_root', 50, 0)
  control.complete(corrected, ids.slice(50), []); control.check(true)
  control.snapshot(); control.check(true)
  control.snapshot(); control.check(false)
  const snapshotZero = control.admit()
  control.complete(snapshotZero, ids.slice(0, 50), [], 200, true); control.check(false)
  const snapshotFifty = control.admit('workspace_root', 50, 0)
  control.complete(snapshotFifty, ids.slice(50), []); control.check(true)
  const short = indexedProofControl({ 'project:p': { projects: [], tasks: [] } }, ['p'], [])
  short.session()
  const first = short.admit()
  short.complete(first, ['p'], [])
  const beyond = short.admit('workspace_root', 50, 0)
  short.complete(beyond, [], [])
  const refreshed = short.admit()
  short.complete(refreshed, ['p'], []); short.check(true) // authoritative shorter prefix satisfies old overscroll demand
})

test('indexed unequal child streams preserve terminal echoes and reject missing or nonprogress pages', () => {
  const tasks = Array.from({ length: 75 }, (_, i) => 't' + i)
  const children = { 'project:p': { projects: [], tasks }, ...Object.fromEntries(tasks.map(id => ['task:' + id, { projects: [], tasks: [] }])) }
  const control = indexedProofControl(children, ['p'], [])
  control.session()
  const root = control.admit(); control.complete(root, ['p'], [])
  const zero = control.admit('project:p')
  control.complete(zero, [], tasks.slice(0, 50), 200, false, true); control.check(false)
  const continuation = control.admit('project:p', 0, 50)
  control.complete(continuation, [], tasks.slice(50)); control.check(true)
  const reset = control.admit('project:p')
  control.complete(reset, [], tasks.slice(0, 50), 200, false, true); control.check(false)
  const wrong = control.admit('project:p', 0, 25)
  control.complete(wrong, [], tasks.slice(25)); control.check(false)
  const newReset = control.admit('project:p')
  control.complete(newReset, [], tasks.slice(0, 50), 200, false, true)
  const end = control.admit('project:p', 0, 50)
  control.complete(end, [], tasks.slice(50)); control.check(true)
})

test('actual resync responder preserves first shell and retires later authoritative descendant proof', async () => {
  const fixture = generateFixture('small')
  const tracker = createTracker(fixture), respond = fixtureResponder(fixture, tracker)
  const reply = async (url, body = null) => {
    const request = { url: () => 'http://fixture' + url, method: () => body ? 'POST' : 'GET', postDataJSON: () => body }
    await respond({ request: () => request, fulfill: async () => {} })
  }
  const navigation = (kind, id = '') => '/api/v1/workspaces/' + fixture.workspace.id + '/navigation?parent_kind=' + kind + (id ? '&parent_id=' + id : '') + '&project_offset=0&task_offset=0&project_limit=50&task_limit=50&priority_mode=any'
  await reply('/api/v1/session')
  await reply(navigation('workspace_root'))
  // Complete the real nonleaf traversal through the same responder callbacks.
  const children = new Map(fixture.projects.map(item => ['project:' + item.id, { projects: [], tasks: [] }]))
  for (const task of fixture.tasks) children.set('task:' + task.id, { projects: [], tasks: [] })
  for (const project of fixture.projects) if (project.parent_id) children.get('project:' + project.parent_id).projects.push(project.id)
  for (const task of fixture.tasks) {
    const owner = task.parent_id ? 'task:' + task.parent_id : task.project_id ? 'project:' + task.project_id : null
    if (owner) children.get(owner).tasks.push(task.id)
  }
  const rootResponse = navigationBranchResponse(fixture)
  const queue = [...rootResponse.projects.items.map(item => 'project:' + item.id), ...rootResponse.tasks.items.map(item => 'task:' + item.id)]
  for (let i = 0; i < queue.length; i++) {
    const key = queue[i], expected = children.get(key)
    if (!expected.projects.length && !expected.tasks.length) continue
    const [kind, id] = key.split(':')
    await reply(navigation(kind, id))
    queue.push(...expected.projects.map(id => 'project:' + id), ...expected.tasks.map(id => 'task:' + id))
  }
  assert.equal(tracker.navigationProof.ready(), true)
  await reply('/api/v1/change-stream/resync', { snapshot_profile: 'workspace_shell_v1', page_size: 100 })
  assert.equal(tracker.navigationProof.ready(), true, 'first authoritative shell retains accepted bootstrap ownership')
  await reply(navigation('workspace_root'))
  assert.equal(tracker.navigationProof.ready(), true, 'ordinary selective root refresh retains untouched descendant proof')
  await reply('/api/v1/change-stream/resync', { snapshot_profile: 'workspace_shell_v1', page_size: 100 })
  await reply(navigation('workspace_root'))
  assert.equal(tracker.navigationProof.ready(), false, 'later authoritative shell retires completed proof before any new child admission')
  for (const key of queue) {
    const expected = children.get(key)
    if (expected.projects.length || expected.tasks.length) { const [kind, id] = key.split(':'); await reply(navigation(kind, id)) }
  }
  assert.equal(tracker.navigationProof.ready(), true)
  const rootProject = rootResponse.projects.items.find(item => children.get('project:' + item.id).tasks.length || children.get('project:' + item.id).projects.length)
  const gate = createTestGate()
  const heldRequest = { url: () => 'http://fixture' + navigation('project', rootProject.id), method: () => 'GET' }
  const held = respond({ request: () => heldRequest, fulfill: async () => { gate.arrived(); await gate.released } })
  try {
    await gate.waitForArrival
    await reply('/api/v1/change-stream/resync', { snapshot_profile: 'workspace_shell_v1', page_size: 100 })
    await reply(navigation('workspace_root'))
    assert.equal(tracker.navigationProof.ready(), false, 'current root cannot borrow pre-resync descendants while new work is queued')
  } finally { gate.release(); await held }
  assert.equal(tracker.navigationProof.ready(), false, 'late retired callback cannot complete authoritative lifetime')
  assert.equal(tracker.requests.length, tracker.completed, 'every physical arrival and completion stays accounted')
})

function createTestGate() {
  let arrived, release
  return { waitForArrival: new Promise(resolve => { arrived = resolve }), released: new Promise(resolve => { release = resolve }), arrived: () => arrived(), release: () => release() }
}

test('indexed explicit continuation retry preserves prefix and rejects superseded callbacks', () => {
  const tasks = Array.from({ length: 75 }, (_, i) => 'retry' + i)
  const children = { 'project:p': { projects: [], tasks }, ...Object.fromEntries(tasks.map(id => ['task:' + id, { projects: [], tasks: [] }])) }
  const control = indexedProofControl(children, ['p'], [])
  control.session()
  const root = control.admit(); control.complete(root, ['p'], [])
  const zero = control.admit('project:p')
  control.complete(zero, [], tasks.slice(0, 50), 200, false, true)
  const failed = control.admit('project:p', 0, 50)
  control.complete(failed, [], tasks.slice(50), 500); control.check(false)
  control.retry('project:p', 'task')
  const retry = control.admit('project:p', 0, 50)
  control.check(false)
  control.complete(retry, [], tasks.slice(50)); control.check(true)
  const newPass = control.admit('project:p')
  control.complete(newPass, [], tasks.slice(0, 50), 200, false, true)
  const oldHeld = control.admit('project:p', 0, 50)
  const freshRetry = control.admit('project:p', 0, 50)
  control.complete(oldHeld, [], tasks.slice(50), 500); control.check(false)
  control.complete(freshRetry, [], tasks.slice(50)); control.check(true)
  control.complete(oldHeld, [], tasks.slice(50), 500); control.check(true)
})

test('independent replay distinguishes first snapshot, selective root and later authoritative lifetime', () => {
  const control = indexedProofControl({ 'project:p': { projects: [], tasks: ['t'] }, 'task:t': { projects: [], tasks: [] } }, ['p'], [])
  control.session()
  const root = control.admit(); control.complete(root, ['p'], [])
  const child = control.admit('project:p'); control.complete(child, [], ['t']); control.check(true)
  control.snapshot(); control.check(true)
  const selective = control.admit(); control.complete(selective, ['p'], []); control.check(true)
  const held = control.admit('project:p')
  control.snapshot(); control.check(false)
  const freshRoot = control.admit(); control.complete(freshRoot, ['p'], []); control.check(false)
  control.complete(held, [], ['t']); control.check(false)
  const newChild = control.admit('project:p'); control.complete(newChild, [], ['t']); control.check(true)
  control.session()
  const nextRoot = control.admit(); control.complete(nextRoot, ['p'], [])
  const nextChild = control.admit('project:p'); control.complete(nextChild, [], ['t']); control.check(true)
  control.snapshot('full_v1'); control.check(false) // full snapshots have no sparse bootstrap exception
})

test('independent kind retry replaces failed offset after its healthy sibling advances', () => {
  const projects = Array.from({ length: 75 }, (_, i) => 'P' + i)
  const tasks = Array.from({ length: 125 }, (_, i) => 'T' + i)
  const children = {
    'project:owner': { projects, tasks },
    ...Object.fromEntries(projects.map(id => ['project:' + id, { projects: [], tasks: [] }])),
    ...Object.fromEntries(tasks.map(id => ['task:' + id, { projects: [], tasks: [] }]))
  }
  const control = indexedProofControl(children, ['owner'], [])
  control.session()
  const root = control.admit(); control.complete(root, ['owner'], [])
  const first = control.admit('project:owner')
  control.complete(first, projects.slice(0, 50), tasks.slice(0, 50), 200, true, true)
  const badProject = control.admit('project:owner', 50, 50)
  control.complete(badProject, projects.slice(0, 50), tasks.slice(50, 100), 200, true, true); control.check(false)
  const healthyContinuation = control.admit('project:owner', 50, 100)
  control.complete(healthyContinuation, projects.slice(50), tasks.slice(100), 200, false, false); control.check(false)
  control.retry('project:owner', 'project')
  const projectRetry = control.admit('project:owner', 50, 100)
  control.check(false)
  control.complete(projectRetry, projects.slice(50), tasks.slice(100)); control.check(true)
  control.complete(badProject, projects.slice(0, 50), tasks.slice(50, 100), 500); control.check(true)

  const queuedRestart = control.admit('project:owner')
  control.complete(queuedRestart, projects.slice(0, 50), tasks.slice(0, 50), 200, true, true)
  const queuedBad = control.admit('project:owner', 50, 50)
  control.complete(queuedBad, projects.slice(0, 50), tasks.slice(50, 100), 200, true, true); control.check(false)
  control.retry('project:owner', 'project')
  const selectedWhileTaskQueued = control.admit('project:owner', 50, 100)
  control.complete(selectedWhileTaskQueued, projects.slice(50), tasks.slice(100)); control.check(true) // selected project plus healthy automatic task both accepted

  const restart = control.admit('project:owner')
  control.complete(restart, projects.slice(0, 50), tasks.slice(0, 50), 200, true, true)
  const failedBoth = control.admit('project:owner', 50, 50)
  control.complete(failedBoth, projects.slice(50), tasks.slice(50, 100), 500); control.check(false)
  const unstamped = control.admit('project:owner', 50, 50)
  control.complete(unstamped, projects.slice(50), tasks.slice(50, 100), 200, false, true); control.check(false)
  control.retry('project:owner', 'project')
  const selectedProject = control.admit('project:owner', 50, 50)
  control.complete(selectedProject, projects.slice(50), tasks.slice(50, 100), 200, false, true); control.check(false) // companion must not clear independently paused task
  control.retry('project:owner', 'task')
  const selectedTask = control.admit('project:owner', 50, 50)
  control.complete(selectedTask, projects.slice(50), tasks.slice(50, 100), 200, false, true); control.check(false)
  const taskTerminal = control.admit('project:owner', 50, 100)
  control.complete(taskTerminal, projects.slice(50), tasks.slice(100)); control.check(true)

})

test('actual valid companion cannot recover a paused kind during healthy continuation', async () => {
  const base = generateFixture('small'), projectTemplate = base.projects[0], taskTemplate = base.tasks[0]
  const fixture = { ...base, projects: [
    { ...projectTemplate, id: 'owner', parent_id: null, name: 'Owner', status: 'active', priority: 0 },
    ...Array.from({ length: 75 }, (_, i) => ({ ...projectTemplate, id: 'P' + i, parent_id: 'owner', name: 'P' + String(i).padStart(3, '0'), status: 'active', priority: 0 }))
  ], tasks: Array.from({ length: 125 }, (_, i) => ({ ...taskTemplate, id: 'T' + i, parent_id: null, project_id: 'owner', title: 'T' + String(i).padStart(3, '0'), status: 'todo', priority: 0 })), dependencies: [] }
  let injectOnce = true
  const tracker = createTracker(fixture), respond = fixtureResponder(fixture, tracker, {
    navigationResponseTransform(value, url) {
      if (injectOnce && url.searchParams.get('parent_kind') === 'project' && url.searchParams.get('project_offset') === '50' && url.searchParams.get('task_offset') === '50') {
        injectOnce = false
        return { ...value, projects: { ...value.projects, items: navigationBranchResponse(fixture, { parentKind: 'project', parentId: 'owner' }).projects.items, has_more: true } }
      }
      return value
    }
  })
  const send = async (kind, projectOffset = 0, taskOffset = 0) => {
    const url = kind === 'session' ? '/api/v1/session' : '/api/v1/workspaces/' + fixture.workspace.id + '/navigation?parent_kind=' + kind + (kind === 'project' ? '&parent_id=owner' : '') + '&project_offset=' + projectOffset + '&task_offset=' + taskOffset + '&project_limit=50&task_limit=50&priority_mode=any'
    let received
    const request = { url: () => 'http://fixture' + url, method: () => 'GET' }
    await respond({ request: () => request, fulfill: async value => { received = JSON.parse(value.body) } })
    return received
  }
  await send('session'); await send('workspace_root')
  await send('project')
  const bad = await send('project', 50, 50)
  assert.equal(bad.projects.has_more, true)
  assert.equal(bad.projects.items[0].id, 'P0')
  assert.equal(tracker.navigationProof.ready(), false)
  let actualDomClicked = false, preClickReady = false, unsettledRejected = false
  const racingAutomation = { locator() { return { getByRole() { return { nth() { return {
    async waitFor() {}, async click() {
      await send('project', 50, 100) // Automatic same-owner admission during actionability/IPC, before actual DOM click.
      preClickReady = tracker.navigationProof.ready()
      actualDomClicked = true
      await send('project', 50, 100)
    }
  } } } } } } }
  try { await retryNavigationKind(racingAutomation, tracker, 'project:owner', 'project') }
  catch (error) { unsettledRejected = /unsettled/.test(error.message) }
  assert.equal(preClickReady, false, 'automatic companion before actual click cannot consume selected intent')
  assert.equal(actualDomClicked, false, 'unsettled owner is rejected before click or arm')
  assert.equal(unsettledRejected, true)
  const healthy = await send('project', 50, 100)
  assert.equal(healthy.projects.items.length, 25)
  assert.equal(healthy.projects.items[0].id, 'P50', 'healthy response carries the actual valid companion DTO')
  assert.equal(healthy.tasks.items[0].id, 'T100')
  assert.equal(tracker.navigationProof.ready(), false, 'production paused project ignores valid companion while task advances')
  let resyncClick = false
  const resyncDuringActionability = { locator() { return { getByRole() { return { nth() { return {
    async waitFor() {
      const request = { url: () => 'http://fixture/api/v1/change-stream/resync', method: () => 'POST', postDataJSON: () => ({ snapshot_profile: 'workspace_shell_v1', page_size: 100 }) }
      await respond({ request: () => request, fulfill: async () => {} })
    },
    async click() { resyncClick = true; await send('project', 50, 100) }
  } } } } } } }
  await assert.rejects(retryNavigationKind(resyncDuringActionability, tracker, 'project:owner', 'project'), /unsettled/)
  assert.equal(resyncClick, false, 'resync admission retires actionability selection before it can arm or click')
  let clicked = false
  const actualRetryAutomation = { locator(selector) {
    assert.equal(selector, '.hierarchy-row[data-hierarchy-key="status:project:owner"] .card-description-error')
    return { getByRole(role, options) { assert.equal(role, 'button'); assert.deepEqual(options, { name: 'Retry', exact: true }); return { nth(index) { assert.equal(index, 0); return {
      async waitFor() {}, async click() { clicked = true; await send('project', 50, 100) }
    } } } } }
  } }
  await retryNavigationKind(actualRetryAutomation, tracker, 'project:owner', 'project')
  assert.equal(clicked, true, 'actual selected Retry automation dispatched the matching request')
  assert.equal(tracker.navigationProof.ready(), true, 'explicit project retry after task terminal accepts only the selected kind')
  assert.equal(tracker.requests.length, tracker.completed)
})

test('retry intent is one-shot and retired by authoritative lifetime, pass, filter, session and collapse', () => {
  const paused = () => {
    const tasks = Array.from({ length: 75 }, (_, i) => 'intent' + i)
    const proof = createNavigationCompletionIndex({ children: { 'project:p': { projects: [], tasks }, ...Object.fromEntries(tasks.map(id => ['task:' + id, { projects: [], tasks: [] }])) }, rootProjects: ['p'], rootTasks: [], context: 'normal' })
    proof.completeSession(proof.beginSession(), true)
    const admit = (owner, projectOffset = 0, taskOffset = 0, context = 'normal') => proof.admit({ owner, projectOffset, taskOffset, projectLimit: 50, taskLimit: 50, context })
    proof.complete(admit('workspace_root'), { projects: { ids: ['p'], hasMore: false }, tasks: { ids: [], hasMore: false } })
    proof.complete(admit('project:p'), { projects: { ids: [], hasMore: false }, tasks: { ids: tasks.slice(0, 50), hasMore: true } })
    proof.complete(admit('project:p', 0, 50), { projects: { ids: [], hasMore: false }, tasks: { ids: tasks.slice(50), hasMore: false } }, 500)
    return { proof, admit, selection: proof.retrySelection('project:p', 'task') }
  }
  for (const retire of [
    value => value.proof.beginSession(),
    value => value.admit('workspace_root', 0, 0, 'filtered'),
    value => value.admit('project:p'),
    value => { value.proof.completeSnapshot(value.proof.admitSnapshot(true)); value.proof.completeSnapshot(value.proof.admitSnapshot(true)) },
    value => { value.proof.setCollapsed(['project:p']); value.proof.setCollapsed([]) }
  ]) {
    const value = paused(); retire(value)
    assert.throws(() => value.proof.armRetry(value.selection), /retired/)
  }
  const value = paused()
  value.proof.armRetry(value.selection)
  const wrong = value.admit('project:p', 0, 25)
  value.proof.complete(wrong, { projects: { ids: [], hasMore: false }, tasks: { ids: [], hasMore: false } })
  const unstamped = value.admit('project:p', 0, 50)
  value.proof.complete(unstamped, { projects: { ids: [], hasMore: false }, tasks: { ids: Array.from({ length: 25 }, (_, i) => 'intent' + (i + 50)), hasMore: false } })
  assert.equal(value.proof.ready(), false, 'unmatched admission consumed intent; valid bytes do not infer retry')
  assert.throws(() => value.proof.armRetry(value.selection), /retired/)
})


test('actual root healthy companion stays incomplete until selected paused-root retry', async () => {
  const base = generateFixture('small'), p = base.projects[0], t = base.tasks[0]
  const fixture = { ...base, projects: Array.from({ length: 100 }, (_, i) => ({ ...p, id: 'RP' + i, parent_id: null, name: 'P' + String(i).padStart(3, '0'), status: 'active', priority: 0 })), tasks: Array.from({ length: 150 }, (_, i) => ({ ...t, id: 'RT' + i, parent_id: null, project_id: null, title: 'T' + String(i).padStart(3, '0'), status: 'todo', priority: 0 })), dependencies: [] }
  let malformed = false
  const tracker = createTracker(fixture), respond = fixtureResponder(fixture, tracker, {
    navigationResponseTransform(value, url) {
      if (malformed && url.searchParams.get('project_offset') === '50' && url.searchParams.get('task_offset') === '50') {
        malformed = false
        return { ...value, projects: { items: navigationBranchResponse(fixture).projects.items, has_more: true } }
      }
      return value
    }
  })
  const reply = async (po, to) => {
    const url = '/api/v1/workspaces/' + fixture.workspace.id + '/navigation?parent_kind=workspace_root&project_offset=' + po + '&task_offset=' + to + '&project_limit=50&task_limit=50&priority_mode=any'
    const request = { url: () => 'http://fixture' + url, method: () => 'GET' }
    await respond({ request: () => request, fulfill: async () => {} })
  }
  const session = { url: () => 'http://fixture/api/v1/session', method: () => 'GET' }
  await respond({ request: () => session, fulfill: async () => {} })
  await reply(0, 0); await reply(50, 0); await reply(50, 50); await reply(50, 100)
  assert.equal(tracker.navigationProof.ready(), true)
  await reply(0, 0)
  malformed = true; await reply(50, 50)
  assert.equal(tracker.navigationProof.ready(), false)
  await reply(50, 100)
  assert.equal(tracker.navigationProof.ready(), false, 'actual valid inactive project companion cannot clear its paused kind')
  const page = { locator(selector) {
    assert.equal(selector, '.hierarchy-row[data-hierarchy-key="root-status:project"]')
    return { getByRole(role, options) {
      assert.equal(role, 'button'); assert.equal(options.name, 'Retry loading projects')
      return { nth() { return this }, async waitFor() {}, async click() { await reply(50, 100) } }
    } }
  } }
  await retryNavigationKind(page, tracker, 'workspace_root', 'project')
  assert.equal(tracker.navigationProof.ready(), true)
  assert.equal(tracker.requests.length, tracker.completed)
})


test('root zero retry preserves independent terminal coverage and requires observable lifetime retirement', () => {
  const control = indexedProofControl({ 'project:p': { projects: [], tasks: [] }, 'task:t': { projects: [], tasks: [] } }, ['p'], ['t'])
  control.session()
  const bootstrap = control.admit()
  control.complete(bootstrap, ['p'], ['t']); control.check(true)
  const badZero = control.admit()
  control.complete(badZero, ['wrong'], ['t']); control.check(false)
  const ambiguousZero = control.admit()
  control.complete(ambiguousZero, ['p'], ['wrong']); control.check(false)
  control.retry('workspace_root', 'project')
  const selectedZero = control.admit()
  control.complete(selectedZero, ['p'], ['wrong']); control.check(true) // terminal task companion ignored, not reset
  control.snapshot(); control.check(true) // first shell preserves bootstrap
  const held = control.admit('workspace_root', 50, 0)
  control.snapshot(); control.check(false)
  const genuinelyFresh = control.admit()
  control.complete(genuinelyFresh, ['p'], ['t']); control.check(true) // short authoritative prefix satisfies retained demand
  control.complete(held, [], ['wrong']); control.check(true)
})


test('actual bootstrap whole-root HTTP failure recovers both kinds through one rendered project Retry', async () => {
  const base = generateFixture('small')
  const fixture = { ...base, projects: [{ ...base.projects[0], id: 'initial-p', parent_id: null }], tasks: [{ ...base.tasks[0], id: 'initial-t', project_id: null, parent_id: null }], dependencies: [] }
  let failedOnce = false, clicks = 0
  const statuses = []
  const tracker = createTracker(fixture), respond = fixtureResponder(fixture, tracker, { navigationResponseStatus() { if (!failedOnce) { failedOnce = true; return 500 } return 200 } })
  const reply = async (url, body = null) => {
    const request = { url: () => 'http://fixture' + url, method: () => body ? 'POST' : 'GET', postDataJSON: () => body }
    await respond({ request: () => request, fulfill: async response => { statuses.push(response.status) } })
  }
  const navigation = '/api/v1/workspaces/' + fixture.workspace.id + '/navigation?parent_kind=workspace_root&project_offset=0&task_offset=0&project_limit=50&task_limit=50&priority_mode=any'
  await reply('/api/v1/session')
  await reply(navigation)
  assert.equal(tracker.navigationProof.ready(), false)
  await reply('/api/v1/change-stream/resync', { snapshot_profile: 'workspace_shell_v1', page_size: 100 })
  assert.equal(tracker.navigationProof.ready(), false, 'first sparse shell preserves failed bootstrap ownership')
  const page = { locator(selector) {
    assert.equal(selector, '.hierarchy-row[data-hierarchy-key="root-status:project"]')
    return { getByRole(role, options) {
      assert.equal(options.name, 'Retry loading projects')
      return { nth() { return this }, async waitFor() {}, async click() { clicks++; await reply(navigation) } }
    } }
  } }
  await retryNavigationKind(page, tracker, 'workspace_root', 'project')
  assert.equal(tracker.navigationProof.ready(), true, 'nonrefresh producer accepts both successful companion streams')
  assert.equal(clicks, 1, 'no second task Retry is needed by the producer')
  assert.equal(statuses.filter(status => status === 500).length, 1)
  assert.equal(tracker.requests.length, tracker.completed)
})


test('manual nonrefresh root HTTP retry preserves its pre-error single-kind mask', () => {
  const projects = Array.from({ length: 60 }, (_, i) => 'manual' + i)
  const leaves = { ...Object.fromEntries(projects.map(id => ['project:' + id, { projects: [], tasks: [] }])), 'task:t': { projects: [], tasks: [] } }
  const control = indexedProofControl(leaves, projects, ['t'])
  control.session()
  const bootstrap = control.admit()
  control.complete(bootstrap, projects.slice(0, 50), ['t'], 200, true); control.check(true)
  const projectPage = control.admit('workspace_root', 50, 0)
  control.complete(projectPage, projects.slice(50), ['t'], 500); control.check(false)
  control.retry('workspace_root', 'project')
  const retry = control.admit('workspace_root', 50, 0)
  assert.deepEqual(Object.keys(retry.stamp.slots), ['project'], 'terminal task companion is not re-admitted by a project-only failed request')
  control.complete(retry, projects.slice(50), ['t']); control.check(true)
})



test('sampler accepts current shell completion during its existing final paint observation', async () => {
  const fixture = generateFixture('small'), tracker = createTracker(fixture), resources = []
  const gate = createTestGate()
  const resyncGate = { pauseOffset: 0, used: false, signalPaused: gate.arrived, waitForRelease: gate.released }
  const respond = fixtureResponder(fixture, tracker, { resyncGate })
  const reply = async (path, body = null) => {
    const request = { url: () => 'http://fixture' + path, method: () => body ? 'POST' : 'GET', postDataJSON: () => body }
    await respond({ request: () => request, fulfill: async response => {
      const id = response.headers['Server-Timing'].split('"')[1]
      resources.push({ id, url: request.url(), startTime: performance.now(), responseEnd: performance.now() })
    } })
  }
  await reply('/api/v1/session')
  const heldShell = reply('/api/v1/change-stream/resync', { snapshot_profile: 'workspace_shell_v1', page_size: 100 })
  await gate.waitForArrival
  let evaluations = 0
  const page = { async evaluate() {
    const startedAt = performance.now()
    evaluations++; gate.release(); await heldShell
    // Model the existing second animation frame after first-frame shell release.
    await new Promise(resolve => setTimeout(resolve, 16))
    const frameAt = performance.now()
    return { painted: true, origin: 'http://fixture', receipts: resources, clock: { startedAt, frameAt, endedAt: performance.now() } }
  } }
  await waitForFirstUsefulViewport(page, tracker, fixture, 'unused-mocked-anchor')
  assert.equal(evaluations, 1, 'current shell completion before final paint must not add another two-frame wait')
  assert.equal(tracker.requests.length, tracker.completed)
})
test('browser receipt timing accepts entry skew without accepting late or unmatched receipts', () => {
  const url = 'http://fixture/api/v1/session', shellUrl = 'http://fixture/api/v1/change-stream/resync'
  const proof = createUsablePaintReadiness('w', 1)
  const session = proof.beginSession(100, 'run-session', url)
  proof.completeSession(session, { canRead: true, workspaceId: 'w' }, 102)
  const shell = proof.admitSnapshot(true, 103, 'run-shell', shellUrl), lifetime = proof.lifetime()
  proof.completeSnapshot(shell, { profile: 'workspace_shell_v1', workspaceId: 'w', items: 1, complete: true }, 237)
  const observation = { origin: 'http://fixture', clock: { startedAt: 223, frameAt: 246, endedAt: 247 },
    receipts: [{ id: 'run-session', url, startTime: 90, responseEnd: 102 }, { id: 'run-shell', url: shellUrl, startTime: 103, responseEnd: 237 }] }
  assert.equal(proof.readyAt(lifetime, observation), true, '12 ms entry skew cannot make the proved same-clock 9 ms completion margin stale')
  assert.equal(proof.readyAt(lifetime, { ...observation, receipts: observation.receipts.map(r => r.id === 'run-shell' ? { ...r, responseEnd: 248 } : r) }), false)
  assert.equal(proof.readyAt(lifetime, { ...observation, receipts: observation.receipts.map(r => r.id === 'run-shell' ? { ...r, responseEnd: 245.9 } : r) }), false, 'uncertain clock boundary fails closed')
  assert.equal(proof.readyAt(lifetime, { ...observation, receipts: [observation.receipts[0]] }), false)
  assert.equal(proof.readyAt(lifetime, { ...observation, receipts: [...observation.receipts, observation.receipts[1]] }), false)
  assert.equal(proof.readyAt(lifetime, { ...observation, receipts: observation.receipts.map(r => ({ ...r, url: 'http://other/retired' })) }), false)
  assert.equal(proof.readyAt(lifetime, { ...observation, origin: 'http://other' }), false)
  for (const clock of [null, {startedAt:223,frameAt:NaN,endedAt:247}, {startedAt:247,frameAt:246,endedAt:248}, {startedAt:223,frameAt:246,endedAt:245}]) assert.equal(proof.readyAt(lifetime, {...observation,clock}), false)
  for (const responseEnd of [0, NaN, Infinity, -1]) assert.equal(proof.readyAt(lifetime, { ...observation, receipts: observation.receipts.map(r => r.id === 'run-shell' ? { ...r, responseEnd } : r) }), false)
})

test('paint receipt lifetimes reject retired callbacks, ABA and unsuccessful fulfillment', async () => {
  const fixture = generateFixture('small'), tracker = createTracker(fixture), resources = []
  const respond = fixtureResponder(fixture, tracker)
  const reply = async (path, body = null, fail = false) => {
    const request = { url: () => 'http://fixture' + path, method: () => body ? 'POST' : 'GET', postDataJSON: () => body }
    return respond({ request: () => request, fulfill: async response => {
      const id = response.headers['Server-Timing'].split('"')[1]
      resources.push({ id, url: request.url(), startTime: 10, responseEnd: 20 })
      if (fail) throw new Error('fixture fulfillment failed')
    } })
  }
  const observe = () => ({ origin: 'http://fixture', receipts: resources, clock: { startedAt: 10, frameAt: 30, endedAt: 31 } })
  await assert.rejects(reply('/api/v1/session', null, true), /fixture fulfillment failed/)
  await reply('/api/v1/change-stream/resync', { snapshot_profile: 'workspace_shell_v1', page_size: 100 })
  assert.equal(tracker.paintReadiness.readyAt(tracker.paintReadiness.lifetime(), observe()), false, 'thrown session fulfill cannot authorize despite matching timing')
  await reply('/api/v1/session')
  await assert.rejects(reply('/api/v1/change-stream/resync', { snapshot_profile: 'workspace_shell_v1', page_size: 100 }, true), /fixture fulfillment failed/)
  assert.equal(tracker.paintReadiness.readyAt(tracker.paintReadiness.lifetime(), observe()), false, 'thrown shell fulfill cannot complete despite matching timing')
  await reply('/api/v1/change-stream/resync', { snapshot_profile: 'workspace_shell_v1', page_size: 100 })
  const old = tracker.paintReadiness.lifetime()
  assert.equal(tracker.paintReadiness.readyAt(old, observe()), true)
  await reply('/api/v1/session')
  await reply('/api/v1/change-stream/resync', { snapshot_profile: 'workspace_shell_v1', page_size: 100 })
  assert.equal(tracker.paintReadiness.readyAt(old, observe()), false, 'same-workspace session ABA retires the captured lifetime')
  assert.equal(tracker.paintReadiness.readyAt(tracker.paintReadiness.lifetime(), observe()), true)
  const staleShell = tracker.requests.find(r => r.paintSnapshot).paintSnapshot
  tracker.paintReadiness.admitSnapshot(true, performance.now(), 'new-snapshot', 'http://fixture/api/v1/change-stream/resync')
  tracker.paintReadiness.completeSnapshot(staleShell, { profile: 'workspace_shell_v1', workspaceId: fixture.workspace.id, items: 1, complete: true }, performance.now())
  assert.equal(tracker.paintReadiness.readyAt(tracker.paintReadiness.lifetime(), observe()), false)
  assert.equal(tracker.requests.length, tracker.completed, 'failed fulfill still retires physical accounting')
})

const writerRepositoryRoot = path.resolve(here, '../../..')
const writerScratchRoot = path.join(writerRepositoryRoot, '.scratch', 'expanded-navigation-perf')
function withWriterScratch(action) {
  fs.mkdirSync(writerScratchRoot, { recursive: true })
  const directory = fs.mkdtempSync(path.join(writerScratchRoot, 'writer-'))
  try { return action(directory) }
  finally {
    if (!path.resolve(directory).startsWith(path.resolve(writerScratchRoot) + path.sep)) throw new Error('Writer scratch escaped its owner')
    fs.rmSync(directory, { recursive: true, force: true, maxRetries: 2, retryDelay: 20 })
  }
}


test('complete evidence writer retains real binary and multibyte no-index output above one MiB', () => {
  withWriterScratch(directory => {
    const binary = Buffer.alloc(1200000)
    for (let offset = 0; offset < binary.length; offset += 32) createHash('sha256').update(String(offset)).digest().copy(binary, offset)
    const text = Buffer.from(Array.from({ length: 26000 }, (_, i) => i.toString().padStart(8, '0') + ' 中文完整证据 ' + createHash('sha256').update(String(i)).digest('hex')).join('\n') + '\n')
    for (const [name, bytes] of [['binary.dat', binary], ['multibyte.txt', text]]) {
      const file = path.join(directory, name), expected = path.join(directory, name + '.diff')
      fs.writeFileSync(file, bytes)
      const descriptor = fs.openSync(expected, 'wx')
      let result
      try { result = spawnSync('git', ['diff', '--binary', '--no-index', '--no-ext-diff', '--src-prefix=a/', '--dst-prefix=b/', '--', '/dev/null', file], { cwd: writerRepositoryRoot, stdio: ['ignore', descriptor, 'pipe'], timeout: 30000, windowsHide: true }) }
      finally { fs.closeSync(descriptor) }
      assert.equal(result.error, undefined); assert.equal(result.signal, null); assert.equal(result.status, 1)
      const independent = fs.readFileSync(expected)
      assert.ok(independent.length > 1024 * 1024)
      const actual = untrackedFileDiff(writerRepositoryRoot, file)
      assert.deepEqual(actual, independent)
      assert.equal(createHash('sha256').update(actual).digest('hex'), createHash('sha256').update(independent).digest('hex'))
    }
  })
})

test('complete evidence capture rejects overflow, timeout, failed exit and partial output', () => {
  withWriterScratch(directory => {
    const file = path.join(directory, 'bounded.txt')
    fs.writeFileSync(file, Array.from({ length: 300 }, (_, i) => String(i) + ' 完整输出').join('\n'))
    const tiny = { commandBytes: 512, totalBytes: 1024, commandTimeoutMs: 30000, totalTimeoutMs: 60000 }
    assert.throws(() => untrackedFileDiff(writerRepositoryRoot, file, createEvidenceCapture({ cwd: writerRepositoryRoot, limits: tiny })), /bytes=.*512|ENOBUFS/)
    let timedChild
    const timed = createEvidenceCapture({ cwd: writerRepositoryRoot, limits: { ...tiny, commandBytes: 65536, commandTimeoutMs: 100 },
      run: (_command, _args, options) => (timedChild = spawnSync(process.execPath, ['-e', 'setTimeout(() => {}, 10000)'], options)) })
    assert.throws(() => timed.read([], { label: 'owned timeout regression' }), /ETIMEDOUT|signal=/)
    assert.ok(timedChild.error)
    assert.throws(() => process.kill(timedChild.pid, 0), error => error.code === 'ESRCH', 'timed-out child is joined and absent')
    const valid = { status: 1, signal: null, stdout: Buffer.from('complete'), stderr: Buffer.alloc(0) }
    for (const result of [
      { ...valid, error: Object.assign(new Error('overflow'), { code: 'ENOBUFS' }) },
      { ...valid, status: 2 }, { ...valid, signal: 'SIGTERM' },
      { ...valid, stdout: Buffer.alloc(0) }, { ...valid, stdout: 'decoded partial string' }
    ]) {
      const capture = createEvidenceCapture({ cwd: writerRepositoryRoot, run: () => result })
      assert.throws(() => capture.read([], { acceptedExitCodes: [0, 1] }), /Evidence command failed/)
    }
    const aggregate = createEvidenceCapture({ cwd: writerRepositoryRoot, limits: { ...tiny, commandBytes: 100, totalBytes: 100 }, run: () => ({ ...valid, stdout: Buffer.alloc(60) }) })
    aggregate.read([], { acceptedExitCodes: [1] })
    assert.throws(() => aggregate.read([], { acceptedExitCodes: [1] }), /bytes=60.40/)
    let time = 0
    const expired = createEvidenceCapture({ cwd: writerRepositoryRoot, limits: tiny, now: () => time, run: () => valid })
    time = tiny.totalTimeoutMs
    assert.throws(() => expired.read([]), /collection limit reached/)
  })
})

test('complete evidence persistence removes stale success and retains truthful failure after partial write', () => {
  withWriterScratch(directory => {
    const manifestPath = path.join(directory, 'manifest.json'), validationPath = path.join(directory, 'validation.json'), diffPath = path.join(directory, 'complete.diff')
    const seedSuccess = () => {
      fs.writeFileSync(manifestPath, JSON.stringify({ qualificationPassed: true }))
      fs.writeFileSync(validationPath, JSON.stringify({ passed: true }))
      fs.writeFileSync(diffPath, 'previous complete diff')
    }
    const options = { manifestPath, validationPath,
      failureValidation: error => ({ passed: false, failure: error.message, commands: [{ exitCode: 2 }] }),
      failureManifest: error => ({ qualificationPassed: false, failure: error.message }) }
    seedSuccess()
    assert.throws(() => persistEvidenceAttempt(options, () => {
      atomicEvidenceWrite(diffPath, Buffer.from('new incomplete diff'), { write: (file, bytes) => {
        fs.writeFileSync(file, bytes.subarray(0, 3)); throw new Error('fixture partial write')
      } })
    }), /fixture partial write/)
    assert.equal(fs.readFileSync(diffPath, 'utf8'), 'previous complete diff')
    assert.deepEqual(JSON.parse(fs.readFileSync(validationPath)), { passed: false, failure: 'fixture partial write', commands: [{ exitCode: 2 }] })
    assert.equal(JSON.parse(fs.readFileSync(manifestPath)).qualificationPassed, false)
    assert.deepEqual(fs.readdirSync(directory).sort(), ['complete.diff', 'manifest.json', 'validation.json'])
    seedSuccess()
    assert.throws(() => persistEvidenceAttempt({ ...options, write: () => { throw new Error('fixture receipt write failure') } }, () => { throw new Error('fixture collection failure') }), /failure receipt persistence failed/)
    assert.equal(fs.existsSync(manifestPath), false)
    assert.equal(fs.existsSync(validationPath), false)
    seedSuccess()
    persistEvidenceAttempt(options, () => {
      atomicEvidenceWrite(diffPath, Buffer.from('complete new diff'))
      atomicEvidenceWrite(validationPath, JSON.stringify({ passed: true }))
      atomicEvidenceWrite(manifestPath, JSON.stringify({ qualificationPassed: true }))
    })
    assert.equal(fs.readFileSync(diffPath, 'utf8'), 'complete new diff')
    assert.equal(JSON.parse(fs.readFileSync(manifestPath)).qualificationPassed, true)
  })
})


test('cold diagnostics preserve current between-frame acceptance and late/missing rejection reasons', () => {
  const decisions=[], url='http://fixture/api/v1/session', shellUrl='http://fixture/api/v1/change-stream/resync';
  const initialize=proof=>{
    const session=proof.beginSession(100,'session',url);proof.completeSession(session,{canRead:true,workspaceId:'w'},102);
    const shell=proof.admitSnapshot(true,103,'shell',shellUrl);proof.completeSnapshot(shell,{profile:'workspace_shell_v1',workspaceId:'w',items:1,complete:true},237);
  };
  const plain=createUsablePaintReadiness('w',1), observed=createUsablePaintReadiness('w',1,decision=>decisions.push(decision));
  initialize(plain);initialize(observed);
  const base={painted:true,origin:'http://fixture',clock:{startedAt:223,frameAt:246,endedAt:247},receipts:[
    {id:'session',url,startTime:90,responseEnd:102},{id:'shell',url:shellUrl,startTime:103,responseEnd:237}]};
  const controls=[
    [base,true,'accepted'],
    [{...base,receipts:base.receipts.map(r=>r.id==='shell'?{...r,responseEnd:248}:r)},false,'snapshot-late-or-uncertain-timing'],
    [{...base,receipts:[base.receipts[0]]},false,'snapshot-missing-timing'],
    [{...base,receipts:[...base.receipts,base.receipts[1]]},false,'snapshot-duplicate-timing'],
    [{...base,origin:'http://other'},false,'session-foreign-origin'],
    [{...base,clock:{startedAt:247,frameAt:246,endedAt:248}},false,'invalid-clock']
  ];
  for(const [observation,expected,reason] of controls){
    assert.equal(plain.readyAt(plain.lifetime(),observation),expected);
    assert.equal(observed.readyAt(observed.lifetime(),observation),expected);
    assert.equal(decisions.at(-1).reason,reason);
  }
  assert.equal(decisions.length,controls.length,'one decision record per existing readyAt call');
  assert.deepEqual(decisions[0].observation.clock,base.clock);
  assert.equal(decisions[0].session.receiptId,'session');assert.equal(decisions[0].snapshot.receiptId,'shell');
  assert.equal(decisions[0].observation.receipts[1].responseEnd,237);
});

test('cold diagnostics distinguish ABA lifetime retirement from invalid current metadata', () => {
  const decisions=[], proof=createUsablePaintReadiness('w',1,decision=>decisions.push(decision));
  const url='http://fixture/api/v1/session',shellUrl='http://fixture/api/v1/change-stream/resync';
  const oldSession=proof.beginSession(10,'old-session',url), oldLifetime=proof.lifetime();
  proof.beginSession(20,'new-session',url);proof.completeSession(oldSession,{canRead:true,workspaceId:'w'},21);
  const observation={origin:'http://fixture',clock:{startedAt:20,frameAt:30,endedAt:31},receipts:[]};
  assert.equal(proof.readyAt(oldLifetime,observation),false);assert.equal(decisions.at(-1).reason,'retired-lifetime');
  assert.equal(proof.readyAt(proof.lifetime(),observation),false);assert.equal(decisions.at(-1).reason,'session-metadata');
  const session=proof.beginSession(40,'current-session',url);proof.completeSession(session,{canRead:true,workspaceId:'w'},41);
  const shell=proof.admitSnapshot(true,42,'current-shell',shellUrl);proof.completeSnapshot(shell,{profile:'full_v1',workspaceId:'w',items:1,complete:true},43);
  const current={origin:'http://fixture',clock:{startedAt:40,frameAt:50,endedAt:51},receipts:[{id:'current-session',url,startTime:40,responseEnd:41}]};
  assert.equal(proof.readyAt(proof.lifetime(),current),false);assert.equal(decisions.at(-1).reason,'snapshot-metadata');
  assert.equal(decisions.at(-1).snapshot.profile,'full_v1');
});

test('cold diagnostics retain bounded first decisive latest chronology and freeze at the host cut', () => {
  const diagnostics=createColdDiagnostics();
  for(let i=1;i<=1000;i++){diagnostics.arrival({requestId:i,at:i});diagnostics.completion({requestId:i,at:i+1,bytes:3})}
  for(let i=1;i<=100;i++)diagnostics.evaluation({ready:i===7,reason:i===7?'accepted':'unusable-dom',startedAt:i,returnedAt:i+1});
  diagnostics.cut(1200,1000,999,1);const before=diagnostics.snapshot();
  diagnostics.arrival({requestId:1001,at:1201});diagnostics.completion({requestId:1000,at:1202});diagnostics.evaluation({ready:true});
  assert.deepEqual(diagnostics.snapshot(),before,'post-cut traffic cannot be relabeled pre-cut');
  assert.equal(before.histories.arrivals.total,1000);assert.equal(before.histories.arrivals.retained.length,9);assert.equal(before.histories.arrivals.omitted,991);
  assert.deepEqual(before.histories.arrivals.retained.map(r=>r.requestId),[1,993,994,995,996,997,998,999,1000]);
  assert.equal(before.histories.evaluations.total,100);assert.equal(before.histories.evaluations.decisiveIndex,7);assert.equal(before.histories.evaluations.omitted,94);
  assert.deepEqual(before.cut,{at:1200,requestCount:1000,completedCount:999,activeCount:1});
  assert.match(before.paintAcknowledgements,/not observed/);
});

test('cold sampler diagnostics preserve unusable DOM short circuit and the existing evaluation count', async () => {
  const fixture=generateFixture('small'),tracker=createTracker(fixture),proof=tracker.paintReadiness;
  const url='http://fixture/api/v1/session',shellUrl='http://fixture/api/v1/change-stream/resync';
  const session=proof.beginSession(1,'session',url);proof.completeSession(session,{canRead:true,workspaceId:fixture.workspace.id},2);
  const shell=proof.admitSnapshot(true,3,'shell',shellUrl);proof.completeSnapshot(shell,{profile:'workspace_shell_v1',workspaceId:fixture.workspace.id,items:1,complete:true},4);
  let evaluations=0,decisions=0;const originalReady=proof.readyAt;proof.readyAt=(...args)=>{decisions++;return originalReady(...args)};
  const page={async evaluate(){evaluations++;return {painted:evaluations===2,anchorVisible:evaluations===2,loading:evaluations===1,origin:'http://fixture',
    clock:{startedAt:10,frameAt:20,endedAt:21},receipts:[{id:'session',url,startTime:1,responseEnd:2},{id:'shell',url:shellUrl,startTime:3,responseEnd:4}]}}};
  await waitForFirstUsefulViewport(page,tracker,fixture,'anchor');
  assert.equal(evaluations,2);assert.equal(decisions,1);
  const history=tracker.coldDiagnostics.snapshot().histories.evaluations;
  assert.equal(history.total,2);assert.equal(history.retained[0].reason,'unusable-dom');assert.deepEqual(history.retained[0].observation.clock,{startedAt:10,frameAt:20,endedAt:21});
  assert.equal(history.retained[1].reason,'accepted');assert.equal(history.decisiveIndex,2);
  assert.ok(history.retained.every(e=>e.returnedAt>=e.startedAt));
});

test('cold navigation enters the authorized paint sampler without a selector prerequisite', async () => {
  const fixture=generateFixture('small'), tracker=createTracker(fixture), proof=tracker.paintReadiness;
  const url='http://fixture/api/v1/session', shellUrl='http://fixture/api/v1/change-stream/resync';
  let evaluations=0;const sequence=[];
  const page={
    async goto(actual,options){sequence.push('goto');assert.equal(actual,`http://fixture/workspace/${fixture.workspace.id}`);assert.deepEqual(options,{waitUntil:'domcontentloaded'})},
    async waitForSelector(){assert.fail('the sampler already rejects missing DOM; a selector must not delay admission')},
    async evaluate(_sample,anchor){
      sequence.push('sample');assert.equal(anchor,'anchor');evaluations++;
      const observation={painted:evaluations!==1,anchorVisible:evaluations!==1,loading:evaluations===1,origin:'http://fixture',
        clock:{startedAt:10,frameAt:20,endedAt:21},receipts:[]};
      if(evaluations===2){
        const session=proof.beginSession(1,'session',url);proof.completeSession(session,{canRead:true,workspaceId:fixture.workspace.id},2);
        const shell=proof.admitSnapshot(true,3,'shell',shellUrl);proof.completeSnapshot(shell,{profile:'workspace_shell_v1',workspaceId:fixture.workspace.id,items:1,complete:true},4);
      }
      if(evaluations>=3) observation.receipts=[{id:'session',url,startTime:1,responseEnd:2},{id:'shell',url:shellUrl,startTime:3,responseEnd:evaluations===3?21:4}];
      return observation;
    }
  };
  await navigateToFirstUsefulViewport(page,'http://fixture',tracker,fixture,'anchor');
  assert.deepEqual(sequence,['goto','sample','sample','sample','sample']);
  assert.deepEqual(tracker.coldDiagnostics.snapshot().histories.evaluations.retained.map(e=>e.reason),
    ['unusable-dom','retired-lifetime','snapshot-late-or-uncertain-timing','accepted']);
});

test('cold diagnostics survive failed persistence and owned cleanup without a stale success receipt', async () => {
  const diagnostics=createColdDiagnostics();diagnostics.arrival({requestId:1,at:10,paintReceiptId:'current'});diagnostics.evaluation({ready:false,reason:'snapshot-missing-timing',returnedAt:20});
  const retirement=await retireOwnedResources([{resource:'browser',close:async()=>{throw new Error('fixture cleanup failure')}},{resource:'server',close:async()=>{}}],100);
  withWriterScratch(directory=>{
    const manifestPath=path.join(directory,'manifest.json'),validationPath=path.join(directory,'validation.json');
    fs.writeFileSync(manifestPath,JSON.stringify({qualificationPassed:true}));
    assert.throws(()=>persistEvidenceAttempt({manifestPath,validationPath,
      failureValidation:error=>({passed:false,failure:error.message,retirement,coldDiagnostics:[diagnostics.snapshot()]}),
      failureManifest:error=>({qualificationPassed:false,failure:error.message})},()=>{throw new Error('fixture persistence failure')}),/fixture persistence failure/);
    const failed=JSON.parse(fs.readFileSync(validationPath));assert.equal(failed.passed,false);assert.equal(failed.retirement.passed,false);
    assert.equal(failed.coldDiagnostics[0].histories.evaluations.firstIndex,1);
    assert.equal(failed.coldDiagnostics[0].histories.evaluations.retained[0].reason,'snapshot-missing-timing');
    assert.equal(JSON.parse(fs.readFileSync(manifestPath)).qualificationPassed,false);
  });
});
