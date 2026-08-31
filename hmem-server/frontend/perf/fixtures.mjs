import { createHash } from 'node:crypto'

export const FIXTURE_SCHEMA_VERSION = 1
export const FIXTURE_SEED = 'hmem-large-workspace-v1'
export const OBSERVATION_MEASURED_QUERY = 'Observation 00001 evidence'
export const TIMELINE_BROWSER_NOW = '2026-08-30T12:00:00Z'
export const TIMELINE_DEFAULT_UI_QUERY = Object.freeze({ since: '2026-06-01T00:00:00Z', until: '2026-08-31T00:00:00Z', bucket: 'week' })
export const TIMELINE_BUCKET_SQL_CAP = 367
export const TIMELINE_BUCKET_RESPONSE_MAX = 366

const SCALES = Object.freeze({
  small: { projects: 10, projectLevels: 2, topTasks: 40, subtasks: 20, dependencies: 24, observations: 60, timelineEvents: 40, timelineBuckets: 20, liveFrames: 50 },
  large: { projects: 250, projectLevels: 4, topTasks: 1000, subtasks: 500, dependencies: 1200, observations: 2000, timelineEvents: 1000, timelineBuckets: 500, liveFrames: 50 }
})

export const DIRECT_FOCUS_CONTRACT = Object.freeze({
  pageSize: 100,
  observationDeadlineMs: 750,
  pauseBeforeItems: 0,
  small: { targetProjectId: '20000000-0000-4000-8000-000000000009' },
  large: { targetProjectId: '20000000-0000-4000-8000-000000000249' }
})

function id(kind, index) {
  const prefix = { workspace: '10000000', project: '20000000', task: '30000000', observation: '40000000', event: '50000000' }[kind]
  return `${prefix}-0000-4000-8000-${String(index + 1).padStart(12, '0')}`
}

function timestamp(index, minuteStride = 1) {
  const base = Date.UTC(2025, 0, 1, 8, 0, 0)
  return new Date(base + Math.floor(index / 3) * minuteStride * 60000).toISOString()
}

function makeProjects(workspaceId, scale) {
  return Array.from({ length: scale.projects }, (_, index) => {
    const level = index % scale.projectLevels
    const parentIndex = level === 0 ? null : index - 1
    return {
      id: id('project', index),
      workspace_id: workspaceId,
      parent_id: parentIndex == null ? null : id('project', parentIndex),
      name: `Project ${String(index + 1).padStart(4, '0')} level ${level + 1}`,
      description: index % 7 === 0 ? `Deterministic project fixture ${index + 1}` : null,
      status: ['active', 'paused', 'completed', 'archived'][index % 4],
      priority: (index % 10) + 1,
      metadata: {},
      created_at: timestamp(index),
      updated_at: timestamp(index + 5)
    }
  })
}

function makeTasks(workspaceId, projects, scale) {
  const top = Array.from({ length: scale.topTasks }, (_, index) => ({
    id: id('task', index),
    workspace_id: workspaceId,
    project_id: index % 10 === 0 ? null : projects[index % projects.length].id,
    parent_id: null,
    title: `Task ${String(index + 1).padStart(5, '0')}`,
    description: index % 9 === 0 ? `Stable task description ${index + 1}` : null,
    status: ['todo', 'in_progress', 'blocked', 'done', 'cancelled'][index % 5],
    priority: (index % 10) + 1,
    metadata: {},
    due_at: index % 4 === 0 ? timestamp(index + 5000) : null,
    completed_at: index % 5 === 3 ? timestamp(index + 2500) : null,
    dependency_count: 0,
    created_at: timestamp(index),
    updated_at: timestamp(index + 7)
  }))
  const children = Array.from({ length: scale.subtasks }, (_, childIndex) => {
    const index = scale.topTasks + childIndex
    const parent = top[(childIndex * 17) % top.length]
    return {
      id: id('task', index),
      workspace_id: workspaceId,
      project_id: parent.project_id,
      parent_id: parent.id,
      title: `Subtask ${String(childIndex + 1).padStart(5, '0')}`,
      description: childIndex % 11 === 0 ? `Stable subtask description ${childIndex + 1}` : null,
      status: ['todo', 'in_progress', 'blocked', 'done', 'cancelled'][childIndex % 5],
      priority: (childIndex % 10) + 1,
      metadata: {},
      due_at: childIndex % 4 === 0 ? timestamp(index + 5000) : null,
      completed_at: childIndex % 5 === 3 ? timestamp(index + 2500) : null,
      dependency_count: 0,
      created_at: timestamp(index),
      updated_at: timestamp(index + 7)
    }
  })
  return [...top, ...children]
}

function makeDependencies(tasks, wanted) {
  const edges = []
  const seen = new Set()
  for (let candidate = 0; edges.length < wanted; candidate += 1) {
    const taskIndex = 1 + (candidate * 29 + Math.floor(candidate / 11)) % (tasks.length - 1)
    const dependsIndex = (candidate * 17 + 3) % taskIndex
    const key = `${taskIndex}:${dependsIndex}`
    if (seen.has(key)) continue
    seen.add(key)
    edges.push({ task_id: tasks[taskIndex].id, depends_on_id: tasks[dependsIndex].id })
  }
  const counts = new Map()
  for (const edge of edges) counts.set(edge.task_id, (counts.get(edge.task_id) || 0) + 1)
  for (const task of tasks) task.dependency_count = counts.get(task.id) || 0
  return edges
}

const OPEN_TASK_STATUSES = new Set(['todo', 'in_progress', 'blocked'])
const OPEN_PROJECT_STATUSES = new Set(['active', 'paused'])

function descendantIds(items, rootId) {
  const children = new Map()
  for (const item of items) {
    if (item.parent_id == null) continue
    const values = children.get(item.parent_id) || []
    values.push(item.id)
    children.set(item.parent_id, values)
  }
  const walk = value => [value, ...(children.get(value) || []).flatMap(walk)]
  return walk(rootId)
}

function openDependencyEdges(fixture, taskIds) {
  const included = new Set(taskIds)
  const tasks = new Map(fixture.tasks.map(task => [task.id, task]))
  return fixture.dependencies.filter(edge => {
    const task = tasks.get(edge.task_id)
    const dependency = tasks.get(edge.depends_on_id)
    return included.has(edge.task_id) && OPEN_TASK_STATUSES.has(task?.status) && OPEN_TASK_STATUSES.has(dependency?.status)
  })
}

export function taskReadinessRollup(fixture, taskId) {
  const tasks = new Map(fixture.tasks.map(task => [task.id, task]))
  if (!tasks.has(taskId)) throw new Error(`unknown task for readiness rollup: ${taskId}`)
  const treeIds = descendantIds(fixture.tasks, taskId)
  const descendants = treeIds.slice(1).map(id => tasks.get(id))
  const openEdges = openDependencyEdges(fixture, treeIds)
  return {
    open_subtask_count: descendants.filter(task => OPEN_TASK_STATUSES.has(task.status)).length,
    done_subtask_count: descendants.filter(task => task.status === 'done').length,
    cancelled_subtask_count: descendants.filter(task => task.status === 'cancelled').length,
    blocked_subtask_count: descendants.filter(task => task.status === 'blocked').length,
    dependency_blocked_task_count: new Set(openEdges.map(edge => edge.task_id)).size,
    open_dependency_count: openEdges.length,
    completion_ready: !descendants.some(task => OPEN_TASK_STATUSES.has(task.status))
  }
}

export function projectReadinessRollup(fixture, projectId) {
  const projects = new Map(fixture.projects.map(project => [project.id, project]))
  const tasks = new Map(fixture.tasks.map(task => [task.id, task]))
  if (!projects.has(projectId)) throw new Error(`unknown project for readiness rollup: ${projectId}`)
  const projectTreeIds = descendantIds(fixture.projects, projectId)
  const projectTree = new Set(projectTreeIds)
  const descendantProjects = projectTreeIds.slice(1).map(id => projects.get(id))
  const seeded = fixture.tasks.filter(task => task.project_id != null && projectTree.has(task.project_id))
  const projectTaskIds = new Set(seeded.flatMap(task => descendantIds(fixture.tasks, task.id)))
  const projectTasks = [...projectTaskIds].map(id => tasks.get(id))
  const openEdges = openDependencyEdges(fixture, projectTaskIds)
  return {
    open_project_count: descendantProjects.filter(project => OPEN_PROJECT_STATUSES.has(project.status)).length,
    closed_project_count: descendantProjects.filter(project => ['completed', 'archived'].includes(project.status)).length,
    open_task_count: projectTasks.filter(task => OPEN_TASK_STATUSES.has(task.status)).length,
    done_task_count: projectTasks.filter(task => task.status === 'done').length,
    cancelled_task_count: projectTasks.filter(task => task.status === 'cancelled').length,
    blocked_task_count: projectTasks.filter(task => task.status === 'blocked').length,
    dependency_blocked_task_count: new Set(openEdges.map(edge => edge.task_id)).size,
    open_dependency_count: openEdges.length,
    completion_ready: !descendantProjects.some(project => OPEN_PROJECT_STATUSES.has(project.status)) && !projectTasks.some(task => OPEN_TASK_STATUSES.has(task.status))
  }
}

export function taskOverviewResponse(fixture, taskId) {
  const tasks = new Map(fixture.tasks.map(task => [task.id, task]))
  const task = tasks.get(taskId)
  if (!task) throw new Error(`unknown task overview: ${taskId}`)
  const dependencies = fixture.dependencies
    .filter(edge => edge.task_id === task.id)
    .map(edge => tasks.get(edge.depends_on_id))
    .filter(Boolean)
    .map(dependency => ({ id: dependency.id, name: dependency.title }))
    .sort((left, right) => compareText(left.name, right.name))
  return { task, dependencies, readiness_rollup: taskReadinessRollup(fixture, taskId) }
}

export function projectOverviewResponse(fixture, projectId) {
  const project = fixture.projects.find(item => item.id === projectId)
  if (!project) throw new Error(`unknown project overview: ${projectId}`)
  return {
    project,
    tasks: queryTasks(fixture, { projectId: project.id, limit: 200 }).items,
    subprojects: queryProjects(fixture, { limit: 200 }).items.filter(item => item.parent_id === project.id),
    readiness_rollup: projectReadinessRollup(fixture, projectId)
  }
}

function makeObservations(workspaceId, count) {
  return Array.from({ length: count }, (_, index) => {
    const subjectCount = 2 + (index % 3)
    const subjects = Array.from({ length: subjectCount }, (_, subjectIndex) => {
      const glob = (index + subjectIndex) % 3 === 0
      return {
        subject_kind: glob ? 'glob' : 'file',
        subject: glob
          ? `src/feature-${index % 25}/**/*.${subjectIndex % 2 ? 'js' : 'elm'}`
          : `src/feature-${index % 25}/module-${(index + subjectIndex) % 80}.${subjectIndex % 2 ? 'js' : 'elm'}`
      }
    })
    return {
      id: id('observation', index),
      workspace_id: workspaceId,
      subjects,
      git_sha: createHash('sha1').update(`${FIXTURE_SEED}:observation:${index}`).digest('hex'),
      content: `Observation ${String(index + 1).padStart(5, '0')} records deterministic repository evidence.`,
      subject_kind: subjects[0].subject_kind,
      subject: subjects[0].subject,
      created_at: timestamp(index),
      updated_at: timestamp(index + 13)
    }
  })
}

function makeTimeline(workspaceId, eventCount, bucketCount, projects, tasks) {
  const projectsById = new Map(projects.map(project => [project.id, project]))
  const tasksById = new Map(tasks.map(task => [task.id, task]))
  const events = Array.from({ length: eventCount }, (_, index) => {
    const auditId = id('event', index)
    const projectEvent = index % 3 === 0
    const entity = projectEvent ? projects[index % projects.length] : tasks[(index * 17) % tasks.length]
    const entityType = projectEvent ? 'project' : entity.parent_id == null ? 'task' : 'subtask'
    const lifecycleIndex = Math.floor(index / 3) % 3
    const eventType = projectEvent
      ? ['project_created', 'project_completed', 'project_archived'][lifecycleIndex]
      : entity.parent_id == null
        ? ['task_created', 'task_completed', 'task_cancelled'][lifecycleIndex]
        : ['subtask_created', 'subtask_completed', 'subtask_cancelled'][lifecycleIndex]
    const project = projectEvent || entity.project_id == null ? null : projectsById.get(entity.project_id)
    const parentTask = projectEvent || entity.parent_id == null ? null : tasksById.get(entity.parent_id)
    const statusTransition = eventType.endsWith('_completed')
      ? { from: projectEvent ? 'active' : 'todo', to: projectEvent ? 'completed' : 'done' }
      : eventType.endsWith('_archived')
        ? { from: 'active', to: 'archived' }
        : eventType.endsWith('_cancelled')
          ? { from: 'todo', to: 'cancelled' }
          : null
    return {
      id: `audit:${auditId}`, workspace_id: workspaceId, event_type: eventType,
      entity_type: entityType, entity_id: entity.id, title: projectEvent ? entity.name : entity.title, occurred_at: timestamp(Math.floor(index / 2), 5),
      actor: null,
      project: project ? { id: project.id, name: project.name } : null,
      parent_task: parentTask ? { id: parentTask.id, title: parentTask.title } : null,
      status_transition: statusTransition,
      navigation: { entity_type: projectEvent ? 'project' : 'task', entity_id: entity.id }, source_audit_id: auditId
    }
  })
  const bucketBase = Date.UTC(2025, 0, 1, 0, 0, 0)
  const buckets = Array.from({ length: bucketCount }, (_, index) => {
    const counts = {
      project: { created: index % 3, completed: index % 2, cancelled: 0 },
      subproject: { created: index % 2, completed: 0, cancelled: 0 },
      task: { created: (index + 1) % 5, completed: index % 4, cancelled: index % 2 },
      subtask: { created: index % 4, completed: index % 3, cancelled: 0 }
    }
    const series = {
      project: { created: index % 3, completed: index % 2, deleted: 0 },
      task: { created: index % 5, completed: index % 4, deleted: index % 2 },
      subtask: { created: index % 4, completed: index % 3, deleted: 0 },
      observation: { created: index % 6, completed: 0, deleted: index % 2 }
    }
    const sum = (values, field) => Object.values(values).reduce((total, value) => total + value[field], 0)
    return {
      bucket_start: new Date(bucketBase + index * 3600000).toISOString(),
      bucket_end: new Date(bucketBase + (index + 1) * 3600000).toISOString(),
      label: `Bucket ${index + 1}`,
      counts,
      totals: { created: sum(counts, 'created'), completed: sum(counts, 'completed'), cancelled: sum(counts, 'cancelled') },
      series,
      series_totals: { created: sum(series, 'created'), completed: sum(series, 'completed'), deleted: sum(series, 'deleted') }
    }
  })
  return { events, buckets }
}

function makeLiveFrames(workspaceId, projects, tasks, observations, count) {
  const sources = [projects, tasks, observations]
  const entityTypes = ['project', 'task', 'observation']
  return Array.from({ length: count }, (_, index) => {
    const typeIndex = index % 3
    const sourceIndex = index > 0 && index % 10 === 0 ? 0 : (index * 31) % sources[typeIndex].length
    const entity = sources[typeIndex][sourceIndex]
    return {
      schema_version: 1,
      type: 'change',
      event: {
        schema_version: 1, event_id: `live-${String(index + 1).padStart(3, '0')}`,
        scope: 'workspace', workspace_id: workspaceId,
        occurred_at: timestamp(index + 10000), transaction: { id: `tx-${index + 1}`, cause: 'rest', request_id: null },
        actor: { type: 'service', id: 'perf-harness' },
        entity: { type: entityTypes[typeIndex], id: entity.id, action: 'updated' },
        invalidations: [{ kind: 'entity', target: `${entityTypes[typeIndex]}:${entity.id}` }]
      }
    }
  })
}

export function generateFixture(size = 'large') {
  const scale = SCALES[size]
  if (!scale) throw new Error(`unknown fixture scale: ${size}`)
  const workspace = {
    id: id('workspace', size === 'large' ? 1 : 0), name: `Performance ${size} repository`, workspace_type: 'repository',
    gh_owner: 'hmem-perf', gh_repo: `${size}-fixture`, created_at: timestamp(0), updated_at: timestamp(1)
  }
  const projects = makeProjects(workspace.id, scale)
  const tasks = makeTasks(workspace.id, projects, scale)
  const dependencies = makeDependencies(tasks, scale.dependencies)
  const observations = makeObservations(workspace.id, scale.observations)
  const timeline = makeTimeline(workspace.id, scale.timelineEvents, scale.timelineBuckets, projects, tasks)
  const liveFrames = makeLiveFrames(workspace.id, projects, tasks, observations, scale.liveFrames)
  return { schemaVersion: FIXTURE_SCHEMA_VERSION, seed: FIXTURE_SEED, size, scale, workspace, projects, tasks, dependencies, observations, timeline, liveFrames }
}

export function stableFixtureJson(fixture) {
  return `${JSON.stringify(fixture, null, 2)}\n`
}

export function fixtureHash(fixture) {
  return createHash('sha256').update(stableFixtureJson(fixture)).digest('hex')
}

export function paginate(items, offset, limit) {
  const safeOffset = Math.min(100000, Math.max(0, Number(offset) || 0))
  const safeLimit = Math.min(200, Math.max(1, Number(limit) || 50))
  return { items: items.slice(safeOffset, safeOffset + safeLimit), has_more: safeOffset + safeLimit < items.length }
}

function compareText(left, right) {
  return left < right ? -1 : left > right ? 1 : 0
}

function compareTimestampDesc(left, right) {
  return compareText(right, left)
}

// Keep the perf transport's navigation projection in lockstep with the SQL
// ordering in HMem.DB.Project/Task.  Alphabetical lifecycle ordering looks
// plausible for this deterministic fixture but does not exercise the actual
// offset-page contract.
const PROJECT_STATUS_RANK = new Map([['active', 0], ['paused', 1], ['completed', 2]])
const TASK_STATUS_RANK = new Map([['todo', 0], ['in_progress', 1], ['blocked', 2], ['done', 3]])

function projectStatusRank(status) {
  return PROJECT_STATUS_RANK.get(status) ?? 3
}

function taskStatusRank(status) {
  return TASK_STATUS_RANK.get(status) ?? 4
}

function navigationPage(items, offset, limit) {
  const safeOffset = Math.min(100000, Math.max(0, Number(offset) || 0))
  const safeLimit = Math.min(100, Math.max(1, Number(limit) || 50))
  return { items: items.slice(safeOffset, safeOffset + safeLimit), has_more: safeOffset + safeLimit < items.length }
}

function navigationTextMatches(entity, query, fields) {
  const needle = String(query || '').trim().toLowerCase()
  return needle === '' || fields.some(field => String(entity[field] || '').toLowerCase().includes(needle))
}

function priorityMatches(entity, mode, value) {
  if (mode == null || mode === 'any') return true
  const expected = Number(value)
  return Number.isInteger(expected)
    && ((mode === 'exact' && entity.priority === expected)
      || (mode === 'above' && entity.priority > expected)
      || (mode === 'below' && entity.priority < expected))
}

function statusMatches(entity, statuses) {
  return !Array.isArray(statuses) || statuses.length === 0 || statuses.includes(entity.status)
}

function projectFilterMatches(project, filters) {
  return statusMatches(project, filters.projectStatuses)
    && priorityMatches(project, filters.priorityMode, filters.priorityValue)
    && navigationTextMatches(project, filters.query, ['name', 'description'])
}

function taskFilterMatches(task, filters) {
  return statusMatches(task, filters.taskStatuses)
    && priorityMatches(task, filters.priorityMode, filters.priorityValue)
    && navigationTextMatches(task, filters.query, ['title', 'description'])
}

function descendantProjects(fixture, projectId) {
  const ids = new Set(descendantIds(fixture.projects, projectId))
  return fixture.projects.filter(project => ids.has(project.id))
}

function descendantProjectTasks(fixture, projectId) {
  const projectIds = new Set(descendantIds(fixture.projects, projectId))
  const rootIds = fixture.tasks.filter(task => task.project_id != null && projectIds.has(task.project_id)).map(task => task.id)
  const ids = new Set(rootIds.flatMap(taskId => descendantIds(fixture.tasks, taskId)))
  return fixture.tasks.filter(task => ids.has(task.id))
}

function projectBranchMatches(fixture, project, filters) {
  // The SQL has an outer project lifecycle/priority gate before descendant
  // retention; preserve that subtlety here.
  if (!statusMatches(project, filters.projectStatuses) || !priorityMatches(project, filters.priorityMode, filters.priorityValue)) return false
  const showOnly = filters.showOnly || 'all'
  const projectMatch = showOnly !== 'tasks'
    && descendantProjects(fixture, project.id).some(candidate => projectFilterMatches(candidate, filters))
  const taskMatch = showOnly !== 'projects'
    && descendantProjectTasks(fixture, project.id).some(candidate => taskFilterMatches(candidate, filters))
  return projectMatch || taskMatch
}

function taskBranchMatches(fixture, task, filters) {
  return descendantIds(fixture.tasks, task.id)
    .map(id => fixture.tasks.find(candidate => candidate.id === id))
    .some(candidate => candidate && taskFilterMatches(candidate, filters))
}

export function queryProjects(fixture, options = {}) {
  let values = fixture.projects
  if (options.status) values = values.filter(project => project.status === options.status)
  values = [...values].sort((left, right) => (right.priority - left.priority) || compareText(left.name, right.name))
  return paginate(values, options.offset, options.limit)
}

export function queryTasks(fixture, options = {}) {
  let values = fixture.tasks
  if (options.projectId) values = values.filter(task => task.project_id === options.projectId)
  if (options.status) values = values.filter(task => task.status === options.status)
  if (options.priority != null) values = values.filter(task => task.priority === Number(options.priority))
  values = [...values].sort((left, right) => (right.priority - left.priority) || compareText(left.created_at, right.created_at))
  return paginate(values, options.offset, options.limit)
}

function simpleLexemes(value) {
  return (String(value).toLowerCase().match(/[\p{L}\p{N}_]+/gu) || [])
}

function observationSearch(fixture, query) {
  if (query == null) return fixture.observations.map(observation => ({ observation, rank: 0 }))
  const wanted = [...new Set(simpleLexemes(query))]
  if (wanted.length === 0) return []
  return fixture.observations.flatMap(observation => {
    const document = simpleLexemes(`${observation.content} ${observation.subjects.map(subject => subject.subject).join(' ')}`)
    const counts = new Map()
    for (const lexeme of document) counts.set(lexeme, (counts.get(lexeme) || 0) + 1)
    if (!wanted.every(lexeme => counts.has(lexeme))) return []
    return [{ observation, rank: wanted.reduce((total, lexeme) => total + counts.get(lexeme), 0) }]
  })
}

export function queryObservations(fixture, options = {}) {
  let ranked = observationSearch(fixture, options.query)
  if (options.subjectKind || options.subject) {
    ranked = ranked.filter(({ observation }) => observation.subjects.some(candidate =>
      (!options.subjectKind || candidate.subject_kind === options.subjectKind)
      && (!options.subject || candidate.subject === options.subject)
    ))
  }
  if (options.gitSha) ranked = ranked.filter(({ observation }) => observation.git_sha === options.gitSha)
  ranked.sort((left, right) =>
    (right.rank - left.rank)
    || compareTimestampDesc(left.observation.updated_at, right.observation.updated_at)
    || compareText(right.observation.id, left.observation.id)
  )
  return paginate(ranked.map(value => value.observation), options.offset, options.limit)
}

export function queryObservationFacets(fixture, options = {}) {
  let ranked = observationSearch(fixture, options.query)
  if (options.gitSha) ranked = ranked.filter(({ observation }) => observation.git_sha === options.gitSha)
  const groups = new Map()
  for (const { observation } of ranked) {
    for (const subject of observation.subjects) {
      if (options.subjectKind && subject.subject_kind !== options.subjectKind) continue
      const key = `${subject.subject_kind}\u0000${subject.subject}`
      const group = groups.get(key) || { ...subject, ids: new Set(), latest_updated_at: observation.updated_at }
      group.ids.add(observation.id)
      if (observation.updated_at > group.latest_updated_at) group.latest_updated_at = observation.updated_at
      groups.set(key, group)
    }
  }
  const facets = [...groups.values()].map(group => ({
    subject_kind: group.subject_kind,
    subject: group.subject,
    observation_count: group.ids.size,
    latest_updated_at: group.latest_updated_at
  })).sort((left, right) =>
    (right.observation_count - left.observation_count)
    || compareTimestampDesc(left.latest_updated_at, right.latest_updated_at)
    || compareText(left.subject_kind, right.subject_kind)
    || compareText(left.subject, right.subject)
  )
  return paginate(facets, options.offset, options.limit)
}

export function queryTimelineEvents(fixture, options = {}) {
  let values = fixture.timeline.events
  if (options.entityType) values = values.filter(event => event.entity_type === options.entityType)
  if (options.eventType) values = values.filter(event => event.event_type === options.eventType)
  if (options.since) values = values.filter(event => event.occurred_at >= options.since)
  if (options.until) values = values.filter(event => event.occurred_at < options.until)
  values = [...values].sort((left, right) =>
    compareTimestampDesc(left.occurred_at, right.occurred_at)
    || compareText(right.source_audit_id, left.source_audit_id)
  )
  return paginate(values, options.offset, options.limit)
}

export function orderedTimelineBuckets(fixture) {
  return [...fixture.timeline.buckets].sort((left, right) => compareText(left.bucket_start, right.bucket_start))
}

export class TimelineBucketRequestError extends Error {
  constructor(message, observedRows = 0) {
    super(message)
    this.name = 'TimelineBucketRequestError'
    this.status = 400
    this.observedRows = observedRows
  }
}

function requiredTimelineInstant(value, name) {
  if (value == null || value === '') throw new TimelineBucketRequestError(`${name} is required`)
  const parsed = Date.parse(value)
  if (!Number.isFinite(parsed)) throw new TimelineBucketRequestError(`${name} must be a valid UTC instant`)
  return parsed
}

function truncateTimelineBucket(instant, bucket) {
  const value = new Date(instant)
  const year = value.getUTCFullYear()
  const month = value.getUTCMonth()
  const day = value.getUTCDate()
  if (bucket === 'day') return Date.UTC(year, month, day)
  if (bucket === 'week') {
    const mondayOffset = (value.getUTCDay() + 6) % 7
    return Date.UTC(year, month, day - mondayOffset)
  }
  if (bucket === 'month') return Date.UTC(year, month, 1)
  return Date.UTC(year, Math.floor(month / 3) * 3, 1)
}

function nextTimelineBucket(instant, bucket) {
  const value = new Date(instant)
  if (bucket === 'day') return Date.UTC(value.getUTCFullYear(), value.getUTCMonth(), value.getUTCDate() + 1)
  if (bucket === 'week') return Date.UTC(value.getUTCFullYear(), value.getUTCMonth(), value.getUTCDate() + 7)
  if (bucket === 'month') return Date.UTC(value.getUTCFullYear(), value.getUTCMonth() + 1, 1)
  return Date.UTC(value.getUTCFullYear(), value.getUTCMonth() + 3, 1)
}

function timelineBucketLabel(instant, bucket) {
  const value = new Date(instant)
  if (bucket === 'quarter') return `Q${Math.floor(value.getUTCMonth() / 3) + 1} ${value.getUTCFullYear()}`
  if (bucket === 'month') return `${['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'][value.getUTCMonth()]} ${value.getUTCFullYear()}`
  return value.toISOString().slice(0, 10)
}

function emptyTimelineBucket(start, end, bucket) {
  const lifecycle = () => ({ created: 0, completed: 0, cancelled: 0 })
  const series = () => ({ created: 0, completed: 0, deleted: 0 })
  return {
    bucket_start: new Date(start).toISOString(), bucket_end: new Date(end).toISOString(), label: timelineBucketLabel(start, bucket),
    counts: { project: lifecycle(), subproject: lifecycle(), task: lifecycle(), subtask: lifecycle() },
    totals: lifecycle(),
    series: { project: series(), task: series(), subtask: series(), observation: series() },
    series_totals: series()
  }
}

function mergeTimelineBucket(target, source) {
  for (const kind of Object.keys(target.counts)) {
    for (const action of Object.keys(target.counts[kind])) target.counts[kind][action] += source.counts[kind][action]
  }
  for (const kind of Object.keys(target.series)) {
    for (const action of Object.keys(target.series[kind])) target.series[kind][action] += source.series[kind][action]
  }
  for (const action of Object.keys(target.totals)) target.totals[action] = Object.values(target.counts).reduce((sum, counts) => sum + counts[action], 0)
  for (const action of Object.keys(target.series_totals)) target.series_totals[action] = Object.values(target.series).reduce((sum, counts) => sum + counts[action], 0)
}

export function queryTimelineBuckets(fixture, options = {}) {
  const bucket = options.bucket || 'week'
  if (!['day', 'week', 'month', 'quarter'].includes(bucket)) throw new TimelineBucketRequestError('bucket must be one of day, week, month, or quarter')
  const since = requiredTimelineInstant(options.since, 'since')
  const until = requiredTimelineInstant(options.until, 'until')
  if (since >= until) throw new TimelineBucketRequestError('since must be before until')
  if (until - since > 10 * 366 * 24 * 60 * 60 * 1000) throw new TimelineBucketRequestError('timeline bucket range must not exceed ten years')

  const seriesStart = truncateTimelineBucket(since, bucket)
  const seriesEnd = truncateTimelineBucket(until - 1, bucket)
  const buckets = []
  for (let start = seriesStart; start <= seriesEnd && buckets.length < TIMELINE_BUCKET_SQL_CAP; start = nextTimelineBucket(start, bucket)) {
    buckets.push(emptyTimelineBucket(start, nextTimelineBucket(start, bucket), bucket))
  }
  if (buckets.length > TIMELINE_BUCKET_RESPONSE_MAX) {
    throw new TimelineBucketRequestError('timeline bucket range produces too many buckets; narrow the range or choose a larger bucket', buckets.length)
  }

  const sources = orderedTimelineBuckets(fixture).filter(source => {
    const occurredAt = Date.parse(source.bucket_start)
    return occurredAt >= since && occurredAt < until
  })
  for (const source of sources) {
    const occurredAt = Date.parse(source.bucket_start)
    const target = buckets.find(candidate => occurredAt >= Date.parse(candidate.bucket_start) && occurredAt < Date.parse(candidate.bucket_end))
    if (target) mergeTimelineBucket(target, source)
  }
  return { workspace_id: fixture.workspace.id, since: options.since, until: options.until, bucket, buckets }
}

export function snapshotItems(fixture) {
  const kindRank = { workspace: 10, project: 20, task: 30, task_dependency: 40, observation: 50 }
  const identity = item => item.kind === 'task_dependency' ? `${item.data.task_id}:${item.data.depends_on_id}` : item.data.id
  return snapshotRecords(fixture).sort((left, right) =>
    kindRank[left.kind] - kindRank[right.kind]
    || compareText(identity(left), identity(right))
  )
}

// The production bounded bootstrap explicitly requests this profile.  Keep the
// full snapshot helper above untouched: baseline.v1.json records the historical
// full_v1 behaviour and must remain comparable evidence.
export function workspaceShellSnapshotItems(fixture) {
  return [{ schema_version: 1, kind: 'workspace', data: fixture.workspace }]
}

function projectCardSummary(fixture, project) {
  const directProjects = fixture.projects.filter(candidate => candidate.parent_id === project.id).length
  const directTasks = fixture.tasks.filter(candidate => candidate.project_id === project.id && candidate.parent_id == null).length
  return {
    id: project.id, workspace_id: project.workspace_id, parent_id: project.parent_id, name: project.name,
    status: project.status, priority: project.priority, created_at: project.created_at, updated_at: project.updated_at,
    direct_project_count: directProjects, direct_task_count: directTasks, has_children: directProjects + directTasks > 0,
    readiness_rollup: projectReadinessRollup(fixture, project.id)
  }
}

function taskCardSummary(fixture, task) {
  const directSubtasks = fixture.tasks.filter(candidate => candidate.parent_id === task.id).length
  return {
    id: task.id, workspace_id: task.workspace_id, project_id: task.project_id, parent_id: task.parent_id, title: task.title,
    status: task.status, priority: task.priority, due_at: task.due_at, completed_at: task.completed_at,
    dependency_count: task.dependency_count, created_at: task.created_at, updated_at: task.updated_at,
    direct_subtask_count: directSubtasks, has_children: directSubtasks > 0, readiness_rollup: taskReadinessRollup(fixture, task.id)
  }
}

export function navigationBranchResponse(fixture, options = {}) {
  const parentKind = options.parentKind || 'workspace_root'
  const parentId = options.parentId || null
  const projectLimit = Math.min(100, Math.max(1, Number(options.projectLimit) || 50))
  const taskLimit = Math.min(100, Math.max(1, Number(options.taskLimit) || 50))
  const projectOffset = Math.max(0, Number(options.projectOffset) || 0)
  const taskOffset = Math.max(0, Number(options.taskOffset) || 0)
  const filters = {
    showOnly: options.showOnly || 'all',
    projectStatuses: options.projectStatuses || [],
    taskStatuses: options.taskStatuses || [],
    priorityMode: options.priorityMode || 'any',
    priorityValue: options.priorityValue,
    query: options.query || null
  }
  const projectChildren = parentKind === 'task' || filters.showOnly === 'tasks' ? [] : fixture.projects
    .filter(project => project.parent_id === parentId && projectBranchMatches(fixture, project, filters))
  const taskChildren = filters.showOnly === 'projects' ? [] : parentKind === 'project'
    ? fixture.tasks.filter(task => task.project_id === parentId && task.parent_id == null)
    : parentKind === 'task'
      ? fixture.tasks.filter(task => task.parent_id === parentId)
      : fixture.tasks.filter(task => task.project_id == null && task.parent_id == null)
  const filteredTaskChildren = taskChildren.filter(task => taskBranchMatches(fixture, task, filters))
  const projectOrder = [...projectChildren].sort((a, b) => projectStatusRank(a.status) - projectStatusRank(b.status) || (b.priority - a.priority) || compareText(a.name.toLowerCase(), b.name.toLowerCase()) || compareText(a.id, b.id))
  const taskOrder = [...filteredTaskChildren].sort((a, b) => taskStatusRank(a.status) - taskStatusRank(b.status) || (b.priority - a.priority) || compareText(a.title.toLowerCase(), b.title.toLowerCase()) || compareText(a.id, b.id))
  return {
    workspace_id: fixture.workspace.id,
    parent: parentKind === 'workspace_root' ? { kind: 'workspace_root' } : { kind: parentKind, parent_id: parentId },
    projects: navigationPage(projectOrder.map(project => projectCardSummary(fixture, project)), projectOffset, projectLimit),
    tasks: navigationPage(taskOrder.map(task => taskCardSummary(fixture, task)), taskOffset, taskLimit)
  }
}

export function navigationFocusResponse(fixture, entityType, entityId, ancestorOffset = 0) {
  const target = entityType === 'project' ? fixture.projects.find(item => item.id === entityId) : fixture.tasks.find(item => item.id === entityId)
  if (!target) return null
  const projectById = new Map(fixture.projects.map(item => [item.id, item]))
  const taskById = new Map(fixture.tasks.map(item => [item.id, item]))
  const ancestors = []
  const appendProject = project => { if (project.parent_id) appendProject(projectById.get(project.parent_id)); ancestors.push({ entity_type: 'project', summary: projectCardSummary(fixture, project) }) }
  if (entityType === 'project' && target.parent_id) appendProject(projectById.get(target.parent_id))
  if (entityType === 'task') {
    const appendTask = task => { if (task.parent_id) appendTask(taskById.get(task.parent_id)); else if (task.project_id) appendProject(projectById.get(task.project_id)); ancestors.push({ entity_type: 'task', summary: taskCardSummary(fixture, task) }) }
    if (target.parent_id) appendTask(taskById.get(target.parent_id)); else if (target.project_id) appendProject(projectById.get(target.project_id))
  }
  const offset = Number(ancestorOffset)
  if (!Number.isInteger(offset) || offset < 0 || offset > 100000) return null
  const page = ancestors.slice(offset, offset + 64)
  const hasMore = offset + page.length < ancestors.length
  return { workspace_id: fixture.workspace.id, target: { entity_type: entityType, summary: entityType === 'project' ? projectCardSummary(fixture, target) : taskCardSummary(fixture, target) }, ancestors: page, ancestors_truncated: hasMore, next_ancestor_offset: hasMore ? offset + page.length : null }
}

export function navigationSummariesResponse(fixture, projectIds = [], taskIds = []) {
  const allIds = [...projectIds, ...taskIds]
  if (allIds.length > 100 || new Set(allIds).size !== allIds.length) return null
  const projects = new Map(fixture.projects.map(item => [item.id, item]))
  const tasks = new Map(fixture.tasks.map(item => [item.id, item]))
  const selectedProjects = projectIds.flatMap(id => projects.has(id) ? [projectCardSummary(fixture, projects.get(id))] : [])
  const selectedTasks = taskIds.flatMap(id => tasks.has(id) ? [taskCardSummary(fixture, tasks.get(id))] : [])
  return {
    projects: selectedProjects,
    tasks: selectedTasks,
    missing_project_ids: projectIds.filter(id => !projects.has(id)),
    missing_task_ids: taskIds.filter(id => !tasks.has(id))
  }
}

export function snapshotHash(fixture) {
  return createHash('sha256').update(JSON.stringify(snapshotItems(fixture))).digest('hex')
}

function snapshotRecords(values) {
  return [
    { schema_version: 1, kind: 'workspace', data: values.workspace },
    ...values.projects.map(data => ({ schema_version: 1, kind: 'project', data })),
    ...values.tasks.map(data => ({ schema_version: 1, kind: 'task', data })),
    ...values.dependencies.map(data => ({ schema_version: 1, kind: 'task_dependency', data })),
    ...values.observations.map(data => ({ schema_version: 1, kind: 'observation', data }))
  ]
}

export function directFocusFixture(fixture) {
  const contract = DIRECT_FOCUS_CONTRACT[fixture.size]
  if (!contract) throw new Error(`unknown direct-focus scale: ${fixture.size}`)
  const targetProject = fixture.projects.find(project => project.id === contract.targetProjectId)
  if (!targetProject) throw new Error(`${fixture.size} fixture requires direct-focus target ${contract.targetProjectId}`)
  return { snapshots: snapshotItems(fixture), targetProject }
}

// Test-only transport fixture for the client continuation path.  It derives
// from the immutable small fixture rather than changing either recorded
// baseline fixture or its canonical snapshot hash.
export function deepFocusFixture() {
  const fixture = generateFixture('small')
  const chain = Array.from({ length: 130 }, (_, index) => ({
    id: `21000000-0000-4000-8000-${String(index + 1).padStart(12, '0')}`,
    workspace_id: fixture.workspace.id,
    parent_id: index === 0 ? null : `21000000-0000-4000-8000-${String(index).padStart(12, '0')}`,
    name: `Deep focus project ${String(index + 1).padStart(3, '0')}`,
    description: null,
    status: 'active',
    priority: 5,
    metadata: {},
    created_at: timestamp(10000 + index),
    updated_at: timestamp(11000 + index)
  }))
  return { fixture: { ...fixture, projects: [...fixture.projects, ...chain] }, targetProject: chain[chain.length - 1], ancestorCount: chain.length - 1 }
}

export function validateFixture(fixture) {
  const errors = []
  const scale = SCALES[fixture.size]
  if (!scale) return ['unknown scale']
  const assert = (condition, message) => { if (!condition) errors.push(message) }
  assert(fixture.schemaVersion === FIXTURE_SCHEMA_VERSION, 'schema version')
  assert(fixture.seed === FIXTURE_SEED, 'seed')
  assert(fixture.projects.length === scale.projects, 'project count')
  assert(fixture.tasks.filter(task => task.parent_id == null).length === scale.topTasks, 'top task count')
  assert(fixture.tasks.filter(task => task.parent_id != null).length === scale.subtasks, 'subtask count')
  assert(fixture.dependencies.length === scale.dependencies, 'dependency count')
  assert(fixture.observations.length === scale.observations, 'observation count')
  assert(fixture.timeline.events.length === scale.timelineEvents, 'timeline event count')
  assert(fixture.timeline.buckets.length === scale.timelineBuckets, 'timeline bucket count')
  assert(fixture.liveFrames.length === scale.liveFrames, 'live frame count')
  const projectById = new Map(fixture.projects.map((value, index) => [value.id, { value, index }]))
  const levels = new Map()
  const levelOf = project => project.parent_id == null ? 0 : 1 + (levels.get(project.parent_id) ?? -100)
  for (const project of fixture.projects) {
    if (project.parent_id != null) assert(projectById.has(project.parent_id) && projectById.get(project.parent_id).index < projectById.get(project.id).index, `project parent ${project.id}`)
    levels.set(project.id, levelOf(project))
  }
  assert(new Set(levels.values()).size === scale.projectLevels, 'project levels')
  const taskIndex = new Map(fixture.tasks.map((task, index) => [task.id, index]))
  for (const task of fixture.tasks) {
    if (task.parent_id != null) {
      assert(taskIndex.has(task.parent_id), `subtask parent ${task.id}`)
      assert(fixture.tasks[taskIndex.get(task.parent_id)].parent_id == null, `subtask depth ${task.id}`)
    }
  }
  const edgeKeys = new Set()
  for (const edge of fixture.dependencies) {
    assert(taskIndex.has(edge.task_id) && taskIndex.has(edge.depends_on_id), 'dependency endpoint')
    assert(taskIndex.get(edge.depends_on_id) < taskIndex.get(edge.task_id), `dependency DAG ${edge.task_id}`)
    edgeKeys.add(`${edge.task_id}:${edge.depends_on_id}`)
  }
  assert(edgeKeys.size === fixture.dependencies.length, 'dependency uniqueness')
  assert(fixture.observations.every(item => item.subjects.length >= 2 && item.subjects.length <= 4), 'observation subjects')
  assert(fixture.observations.some(item => item.subjects.some(subject => subject.subject_kind === 'file')), 'file subjects')
  assert(fixture.observations.some(item => item.subjects.some(subject => subject.subject_kind === 'glob')), 'glob subjects')
  assert(fixture.projects.every(project => project.metadata != null && typeof project.metadata === 'object'), 'project metadata DTO')
  assert(fixture.tasks.every(task => task.metadata != null && typeof task.metadata === 'object' && !Object.hasOwn(task, 'memory_link_count')), 'task snapshot DTO')
  assert(fixture.observations.every(item => item.subject_kind === item.subjects[0]?.subject_kind && item.subject === item.subjects[0]?.subject), 'observation legacy subject compatibility')
  assert(fixture.tasks.some(task => task.parent_id == null && task.project_id == null), 'projectless top tasks')
  assert(fixture.projects.length <= 200 || paginate(fixture.projects, 200, 200).items.length > 0, 'project pagination boundary')
  assert(fixture.tasks.length <= 200 || paginate(fixture.tasks, 200, 200).items.length > 0, 'task pagination boundary')
  assert(new Set(fixture.liveFrames.map(frame => frame.event.entity.type)).size === 3, 'mixed live types')
  assert(new Set(fixture.liveFrames.map(frame => `${frame.event.entity.type}:${frame.event.entity.id}`)).size < fixture.liveFrames.length, 'repeated live target')
  assert(fixture.liveFrames.every(frame => frame.schema_version === 1 && frame.event.schema_version === 1 && frame.event.scope === 'workspace' && frame.event.workspace_id === fixture.workspace.id), 'canonical live schema')
  assert(!fixture.timeline.events.some(event => event.entity_type === 'observation'), 'timeline events exclude observation audit rows')
  assert(new Set(fixture.timeline.events.map(event => event.event_type)).size === 9, 'timeline lifecycle event mix')
  const newestTimeline = queryTimelineEvents(fixture, { limit: fixture.timeline.events.length }).items
  assert(newestTimeline.every((event, index) => index === 0 || event.occurred_at < newestTimeline[index - 1].occurred_at || (event.occurred_at === newestTimeline[index - 1].occurred_at && event.source_audit_id < newestTimeline[index - 1].source_audit_id)), 'timeline event production order')
  const direct = directFocusFixture(fixture)
  const canonicalSnapshot = snapshotItems(fixture)
  assert(JSON.stringify(direct.snapshots) === JSON.stringify(canonicalSnapshot), 'direct resync preserves canonical ordered bytes')
  assert(direct.targetProject.parent_id == null, 'direct target is a root project')
  assert(direct.targetProject.workspace_id === fixture.workspace.id, 'direct target belongs to fixture workspace')
  assert(direct.snapshots.some(item => item.kind === 'project' && item.data.id === direct.targetProject.id), 'direct target belongs to eventual canonical snapshot')
  const bucketStarts = fixture.timeline.buckets.map(bucket => bucket.bucket_start)
  assert(new Set(bucketStarts).size === fixture.timeline.buckets.length, 'timeline bucket start uniqueness')
  assert(fixture.timeline.buckets.every((bucket, index) => {
    const ordered = Date.parse(bucket.bucket_start) < Date.parse(bucket.bucket_end) && (index === 0 || Date.parse(fixture.timeline.buckets[index - 1].bucket_end) <= Date.parse(bucket.bucket_start))
    const totals = ['created', 'completed', 'cancelled'].every(field => bucket.totals[field] === Object.values(bucket.counts).reduce((sum, value) => sum + value[field], 0))
    const seriesTotals = ['created', 'completed', 'deleted'].every(field => bucket.series_totals[field] === Object.values(bucket.series).reduce((sum, value) => sum + value[field], 0))
    const nonNegative = [...Object.values(bucket.counts), ...Object.values(bucket.series), bucket.totals, bucket.series_totals].every(value => Object.values(value).every(count => Number.isInteger(count) && count >= 0))
    return ordered && totals && seriesTotals && nonNegative
  }), 'timeline bucket ordering/totals')
  const defaultTimelineBuckets = queryTimelineBuckets(fixture, TIMELINE_DEFAULT_UI_QUERY)
  assert(defaultTimelineBuckets.buckets.length === 13, 'default UI weekly bucket count')
  assert(defaultTimelineBuckets.buckets[0]?.bucket_start === '2026-06-01T00:00:00.000Z' && defaultTimelineBuckets.buckets.at(-1)?.bucket_end === '2026-08-31T00:00:00.000Z', 'default UI weekly bucket boundaries')
  const taskRollups = fixture.tasks.map(task => taskReadinessRollup(fixture, task.id))
  const projectRollups = fixture.projects.map(project => projectReadinessRollup(fixture, project.id))
  assert(taskRollups.some(rollup => rollup.completion_ready) && taskRollups.some(rollup => !rollup.completion_ready), 'task ready and gated rollups')
  assert(taskRollups.some(rollup => rollup.open_dependency_count > 0) && taskRollups.some(rollup => rollup.open_dependency_count === 0), 'task dependency rollups')
  assert(projectRollups.some(rollup => rollup.completion_ready) && projectRollups.some(rollup => !rollup.completion_ready), 'project ready and gated rollups')
  return errors
}
