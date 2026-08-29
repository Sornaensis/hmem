import { spawn } from 'node:child_process';
import { once } from 'node:events';
import { createInterface } from 'node:readline';
import { fileURLToPath } from 'node:url';

export const api = process.env.HMEM_TIMELINE_API ?? 'http://127.0.0.1:5180';
export const token = 'sandbox-local-bot-token';

export async function json(path, options = {}) {
  const response = await fetch(api + path, {
    ...options,
    headers: {
      Authorization: `Bearer ${token}`,
      'Content-Type': 'application/json',
      ...(options.headers ?? {}),
    },
  });
  const text = await response.text();
  if (!response.ok) throw new Error(`${options.method ?? 'GET'} ${path}: ${response.status} ${text}`);
  return text ? JSON.parse(text) : null;
}

export async function seededWorkspace() {
  return (await json('/api/v1/workspaces')).items[0].id;
}

export async function restoreLatestSoftDelete(workspaceId, entityType, entityId) {
  const query = new URLSearchParams({
    workspace_id: workspaceId,
    entity_type: entityType,
    entity_id: entityId,
    action: 'update',
    limit: '50',
  });
  const audit = await json(`/api/v1/audit?${query}`);
  const deletion = audit.items.find((entry) =>
    entry.old_values?.deleted_at == null && entry.new_values?.deleted_at != null);
  if (!deletion) throw new Error(`No soft-delete audit entry for ${entityType} ${entityId}`);
  return json(`/api/v1/audit/${deletion.id}/revert`, { method: 'POST' });
}

export async function startLocalMcp(workspaceId) {
  const repositoryRoot = process.env.HMEM_REPO_ROOT
    ?? fileURLToPath(new URL('../../../../', import.meta.url));
  const child = spawn('stack', [
    'exec', 'hmem-mcp', '--',
    '--server-url', api,
    '--auth-token', token,
  ], {
    cwd: repositoryRoot,
    env: process.env,
    stdio: ['pipe', 'pipe', 'pipe'],
    windowsHide: true,
  });
  await once(child, 'spawn');

  const pending = new Map();
  const stderr = [];
  createInterface({ input: child.stdout }).on('line', (line) => {
    let response;
    try { response = JSON.parse(line); } catch { return; }
    const request = pending.get(String(response.id));
    if (!request) return;
    pending.delete(String(response.id));
    if (response.error) request.reject(new Error(JSON.stringify(response.error)));
    else request.resolve(response.result);
  });
  createInterface({ input: child.stderr }).on('line', (line) => stderr.push(line));

  let nextId = 1;
  const rpc = (method, params) => new Promise((resolve, reject) => {
    const id = String(nextId++);
    const timeout = setTimeout(() => {
      pending.delete(id);
      reject(new Error(`Timed out waiting for hmem-mcp ${method}: ${stderr.join('\n')}`));
    }, 15000);
    pending.set(id, {
      resolve: (value) => { clearTimeout(timeout); resolve(value); },
      reject: (error) => { clearTimeout(timeout); reject(error); },
    });
    child.stdin.write(`${JSON.stringify({ jsonrpc: '2.0', id, method, ...(params ? { params } : {}) })}\n`);
  });

  await rpc('initialize');
  await rpc('tools/call', { name: 'set_workspace', arguments: { workspace_id: workspaceId } });

  return {
    callTool: (name, args) => rpc('tools/call', { name, arguments: args }),
    async close() {
      child.stdin.end();
      await Promise.race([
        once(child, 'close'),
        new Promise((resolve) => setTimeout(() => { child.kill(); resolve(); }, 5000)),
      ]);
    },
  };
}
