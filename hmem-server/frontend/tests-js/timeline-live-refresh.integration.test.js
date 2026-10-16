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

async function assertOpenInventory(workspace, expected) {
  const canonical = await json(`/api/v1/workspaces/${workspace}/planning-counts`);
  assert.deepEqual({ projects: canonical.open_projects, tasks: canonical.open_tasks }, expected);
  const started = Date.now();
  const query = new URLSearchParams({ since: new Date(started - 86400000).toISOString(), until: new Date(started + 2 * 86400000).toISOString(), bucket: 'day' });
  const response = await json(`/api/v1/workspaces/${workspace}/timeline/buckets?${query}`);
  const current = response.buckets.find(bucket => bucket.open_status === 'available' && Date.parse(bucket.open_as_of) >= started);
  assert.ok(current, 'current partial bucket carries the server snapshot cutoff');
  assert.deepEqual(current.open_counts, expected, 'current stock agrees with stable canonical base-row counts');
  assert.ok(Date.parse(current.open_as_of) <= Date.now());
  const future = response.buckets.filter(bucket => bucket.open_status === 'future');
  assert.ok(future.length > 0);
  assert.ok(future.every(bucket => bucket.open_counts === null && bucket.open_as_of === null), 'future buckets never project inventory');
}

test('isolated REST and explicitly bound MCP lifecycles reach authoritative Timeline projections', { timeout: 60000 }, async (t) => {
  const workspace = await seededWorkspace();
  const suffix = Date.now().toString(36);
  const initial = await json(`/api/v1/workspaces/${workspace}/planning-counts`);
  const open = { projects: initial.open_projects, tasks: initial.open_tasks };
  await assertOpenInventory(workspace, open);

  const project = await request('/api/v1/projects', 'POST', {
    workspace_id: workspace,
    name: `live-project-${suffix}`,
  });
  open.projects++;
  await assertOpenInventory(workspace, open);
  await request(`/api/v1/projects/${project.id}`, 'PUT', {
    description: `updated project ${suffix}`,
  });
  await request(`/api/v1/projects/${project.id}`, 'PUT', { status: 'completed' });
  open.projects--;
  await assertOpenInventory(workspace, open);
  await request(`/api/v1/projects/${project.id}`, 'DELETE');
  const restoredProject = await restoreLatestSoftDelete(workspace, 'project', project.id);
  assert.equal(restoredProject.entity.id, project.id);
  await assertOpenInventory(workspace, open); // Restoring terminal work keeps it closed.
  await request(`/api/v1/projects/${project.id}`, 'PUT', { status: 'active' });
  open.projects++;
  await assertOpenInventory(workspace, open);

  const task = await request('/api/v1/tasks', 'POST', {
    workspace_id: workspace,
    title: `live-task-${suffix}`,
  });
  open.tasks++;
  await assertOpenInventory(workspace, open);
  await request(`/api/v1/tasks/${task.id}`, 'PUT', {
    description: `updated task ${suffix}`,
  });
  await request(`/api/v1/tasks/${task.id}`, 'PUT', { status: 'done' });
  open.tasks--;
  await assertOpenInventory(workspace, open);
  await request(`/api/v1/tasks/${task.id}`, 'DELETE');
  const restoredTask = await restoreLatestSoftDelete(workspace, 'task', task.id);
  assert.equal(restoredTask.entity.id, task.id);
  await assertOpenInventory(workspace, open);
  await request(`/api/v1/tasks/${task.id}`, 'PUT', { status: 'todo' });
  open.tasks++;
  await assertOpenInventory(workspace, open);

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
  const cascadeChild = await request('/api/v1/tasks', 'POST', {
    workspace_id: workspace,
    project_id: cascadeRoot.id,
    parent_id: cascadeTask.id,
    title: `cascade-subtask-${suffix}`,
  });
  open.projects += 2;
  open.tasks += 2;
  await assertOpenInventory(workspace, open);
  const cascade = await request(`/api/v1/projects/${cascadeRoot.id}`, 'DELETE');
  assert.deepEqual(
    { projects: cascade.project_count, tasks: cascade.task_count },
    { projects: 2, tasks: 2 },
  );
  open.projects -= 2;
  open.tasks -= 2;
  await assertOpenInventory(workspace, open);
  await restoreLatestSoftDelete(workspace, 'project', cascadeRoot.id);
  open.projects += 2;
  open.tasks += 2;
  await assertOpenInventory(workspace, open);
  await request(`/api/v1/projects/${cascadeRoot.id}`, 'PUT', { status: 'archived' });
  open.projects -= 2;
  open.tasks -= 2;
  await assertOpenInventory(workspace, open);
  await request(`/api/v1/projects/${cascadeRoot.id}`, 'PUT', { status: 'active' });
  open.projects++;
  await request(`/api/v1/tasks/${cascadeTask.id}`, 'PUT', { status: 'todo' });
  await request(`/api/v1/tasks/${cascadeChild.id}`, 'PUT', { status: 'todo' });
  open.tasks += 2;
  await assertOpenInventory(workspace, open);
  await request(`/api/v1/tasks/${cascadeTask.id}`, 'PUT', { status: 'cancelled' });
  open.tasks -= 2;
  await assertOpenInventory(workspace, open);

  const mcp = await startLocalMcp(workspace);
  t.after(() => mcp.close());
  const mcpResult = await mcp.callTool('project_create', { name: `mcp-project-${suffix}` });
  const mcpAck = JSON.parse(mcpResult.content[0].text);
  assert.equal(mcpAck.ok, true);
  assert.equal(mcpAck.entity_type, 'project');
  open.projects++;
  await assertOpenInventory(workspace, open);

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
