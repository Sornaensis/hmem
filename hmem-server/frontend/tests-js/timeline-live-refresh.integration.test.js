import test from 'node:test';
import assert from 'node:assert/strict';
import {
  api,
  json,
  restoreLatestSoftDelete,
  seededWorkspace,
  startLocalMcp,
} from './support/local-timeline-harness.js';

if (!process.env.HMEM_TIMELINE_API) {
  throw new Error('HMEM_TIMELINE_API is required; start hmem-test-harness and set its explicit local URL');
}

const request = (path, method, body) => json(path, {
  method,
  ...(body === undefined ? {} : { body: JSON.stringify(body) }),
});

const sumSeries = (buckets, entity, action) =>
  buckets.reduce((total, bucket) => total + bucket.series[entity][action], 0);

test('isolated REST and explicitly bound MCP lifecycles reach authoritative Timeline projections', { timeout: 60000 }, async (t) => {
  const workspace = await seededWorkspace();
  const suffix = Date.now().toString(36);

  const project = await request('/api/v1/projects', 'POST', {
    workspace_id: workspace,
    name: `live-project-${suffix}`,
  });
  await request(`/api/v1/projects/${project.id}`, 'PUT', {
    description: `updated project ${suffix}`,
  });
  await request(`/api/v1/projects/${project.id}`, 'PUT', { status: 'completed' });
  await request(`/api/v1/projects/${project.id}`, 'DELETE');
  const restoredProject = await restoreLatestSoftDelete(workspace, 'project', project.id);
  assert.equal(restoredProject.entity.id, project.id);

  const task = await request('/api/v1/tasks', 'POST', {
    workspace_id: workspace,
    title: `live-task-${suffix}`,
  });
  await request(`/api/v1/tasks/${task.id}`, 'PUT', {
    description: `updated task ${suffix}`,
  });
  await request(`/api/v1/tasks/${task.id}`, 'PUT', { status: 'done' });
  await request(`/api/v1/tasks/${task.id}`, 'DELETE');
  const restoredTask = await restoreLatestSoftDelete(workspace, 'task', task.id);
  assert.equal(restoredTask.entity.id, task.id);

  const observation = await request('/api/v1/observations', 'POST', {
    workspace_id: workspace,
    subjects: [{ subject_kind: 'file', subject: `src/live-${suffix}.hs` }],
    git_sha: '0123456789abcdef0123456789abcdef01234567',
    content: `live observation ${suffix}`,
  });
  await request(`/api/v1/observations/${observation.id}`, 'DELETE');

  const cascadeRoot = await request('/api/v1/projects', 'POST', {
    workspace_id: workspace,
    name: `cascade-root-${suffix}`,
  });
  await request('/api/v1/projects', 'POST', {
    workspace_id: workspace,
    parent_id: cascadeRoot.id,
    name: `cascade-child-${suffix}`,
  });
  const cascadeTask = await request('/api/v1/tasks', 'POST', {
    workspace_id: workspace,
    project_id: cascadeRoot.id,
    title: `cascade-task-${suffix}`,
  });
  await request('/api/v1/tasks', 'POST', {
    workspace_id: workspace,
    project_id: cascadeRoot.id,
    parent_id: cascadeTask.id,
    title: `cascade-subtask-${suffix}`,
  });
  const cascade = await request(`/api/v1/projects/${cascadeRoot.id}`, 'DELETE');
  assert.deepEqual(
    { projects: cascade.project_count, tasks: cascade.task_count },
    { projects: 2, tasks: 2 },
  );

  const mcp = await startLocalMcp(workspace);
  t.after(() => mcp.close());
  const mcpResult = await mcp.callTool('project_create', { name: `mcp-project-${suffix}` });
  const mcpAck = JSON.parse(mcpResult.content[0].text);
  assert.equal(mcpAck.ok, true);
  assert.equal(mcpAck.entity_type, 'project');

  const projectionStarted = performance.now();
  const events = await json(`/api/v1/workspaces/${workspace}/timeline?limit=200`);
  const now = Date.now();
  const since = new Date(now - 2 * 86400000).toISOString();
  const until = new Date(now + 2 * 86400000).toISOString();
  const buckets = await json(`/api/v1/workspaces/${workspace}/timeline/buckets?${new URLSearchParams({ since, until, bucket: 'day' })}`);
  const projectionLatencyMs = performance.now() - projectionStarted;

  assert.ok(events.items.some((event) => event.title === project.name && event.event_type === 'project_created'));
  assert.ok(events.items.some((event) => event.title === project.name && event.event_type === 'project_completed'));
  assert.ok(events.items.some((event) => event.title === task.title && event.event_type === 'task_created'));
  assert.ok(events.items.some((event) => event.title === task.title && event.event_type === 'task_completed'));
  assert.ok(events.items.some((event) => event.title === `mcp-project-${suffix}`));

  assert.ok(sumSeries(buckets.buckets, 'project', 'created') >= 4);
  assert.ok(sumSeries(buckets.buckets, 'project', 'completed') >= 1);
  assert.ok(sumSeries(buckets.buckets, 'project', 'deleted') >= 3);
  assert.ok(sumSeries(buckets.buckets, 'task', 'created') >= 2);
  assert.ok(sumSeries(buckets.buckets, 'task', 'completed') >= 1);
  assert.ok(sumSeries(buckets.buckets, 'task', 'deleted') >= 2);
  assert.ok(sumSeries(buckets.buckets, 'subtask', 'created') >= 1);
  assert.ok(sumSeries(buckets.buckets, 'subtask', 'deleted') >= 1);
  assert.ok(sumSeries(buckets.buckets, 'observation', 'created') >= 1);
  assert.ok(sumSeries(buckets.buckets, 'observation', 'deleted') >= 1);
  assert.ok(projectionLatencyMs < 1000, `authoritative Timeline reads completed in ${projectionLatencyMs.toFixed(1)} ms from ${api}`);
});
