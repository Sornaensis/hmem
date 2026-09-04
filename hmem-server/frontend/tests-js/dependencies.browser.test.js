import assert from 'node:assert/strict'
import { readFile, unlink } from 'node:fs/promises'
import { createServer } from 'node:http'
import { resolve } from 'node:path'
import test, { after } from 'node:test'
import { chromium } from '@playwright/test'

const fixtureBundle = resolve(process.cwd(), '.dependencies-fixture-test.js')
const dependentId = 'current-task'
const prerequisiteId = 'selected-prerequisite'

after(() => unlink(fixtureBundle).catch(() => {}))

function fixtureDocument() {
  return `<!DOCTYPE html>
<html lang="en">
<head><meta charset="UTF-8" /><title>Dependency selector fixture</title><link rel="stylesheet" href="/style.css" /></head>
<body><div id="client-a"><div class="elm-mount"></div></div><div id="client-b"><div class="elm-mount"></div></div><script src="/DependenciesFixture.js"></script><script>
window.dependencyClients = {
  clientA: Elm.DependenciesFixture.init({ node: document.querySelector('#client-a .elm-mount') }),
  clientB: Elm.DependenciesFixture.init({ node: document.querySelector('#client-b .elm-mount') }),
}
</script></body>
</html>`
}

async function startFixtureServer(requests) {
  let dependencyPresent = false
  let dependencyPageRequests = 0
  let httpRequests = 0
  let markMutationReceived
  const mutationReceived = new Promise((resolveMutationReceived) => { markMutationReceived = resolveMutationReceived })
  let releaseMutationResponse
  const mutationResponseReleased = new Promise((resolveMutationResponse) => { releaseMutationResponse = resolveMutationResponse })
  const [ bundle, stylesheet ] = await Promise.all([
    readFile(fixtureBundle),
    readFile(resolve(process.cwd(), 'src/style.css')),
  ])
  const server = createServer((request, response) => {
    httpRequests += 1
    if (request.url === '/DependenciesFixture.js') {
      response.writeHead(200, { 'content-type': 'application/javascript; charset=utf-8' })
      response.end(bundle)
      return
    }
    if (request.url === '/style.css') {
      response.writeHead(200, { 'content-type': 'text/css; charset=utf-8' })
      response.end(stylesheet)
      return
    }
    if (request.method === 'POST' && request.url === `/api/v1/tasks/${dependentId}/dependencies`) {
      let body = ''
      request.on('data', (chunk) => { body += chunk })
      request.on('end', () => {
        requests.push({ method: request.method, url: request.url, body: JSON.parse(body) })
        dependencyPresent = true
        markMutationReceived()
        mutationResponseReleased.then(() => {
          response.writeHead(200, { 'content-type': 'application/json' })
          response.end(JSON.stringify({ action: 'add', task_id: dependentId, depends_on_id: prerequisiteId, affected_tasks: [] }))
        })
      })
      return
    }
    if (request.method === 'POST' && request.url === '/api/v1/workspaces/workspace-1/navigation/summaries') {
      response.writeHead(200, { 'content-type': 'application/json' })
      response.end(JSON.stringify({
        projects: [],
        tasks: [{
          id: dependentId,
          workspace_id: 'workspace-1',
          project_id: null,
          parent_id: null,
          title: 'Current task',
          status: 'todo',
          priority: 1,
          due_at: null,
          completed_at: null,
          dependency_count: dependencyPresent ? 1 : 0,
          created_at: '2026-01-01T00:00:00Z',
          updated_at: '2026-01-01T00:00:00Z',
          direct_subtask_count: 0,
          has_children: false,
          readiness_rollup: {
            open_subtask_count: 0,
            done_subtask_count: 0,
            cancelled_subtask_count: 0,
            blocked_subtask_count: 0,
            dependency_blocked_task_count: 0,
            open_dependency_count: dependencyPresent ? 1 : 0,
            completion_ready: !dependencyPresent,
          },
        }],
        missing_project_ids: [],
        missing_task_ids: [],
      }))
      return
    }
    if (request.method === 'DELETE' && request.url === `/api/v1/tasks/${dependentId}/dependencies/${prerequisiteId}`) {
      let body = ''
      request.on('data', (chunk) => { body += chunk })
      request.on('end', () => {
        requests.push({ method: request.method, url: request.url, body: JSON.parse(body) })
        dependencyPresent = false
        response.writeHead(200, { 'content-type': 'application/json' })
        response.end(JSON.stringify({ action: 'remove', task_id: dependentId, depends_on_id: prerequisiteId, affected_tasks: [] }))
      })
      return
    }
    if (request.method === 'GET' && request.url === `/api/v1/tasks/${dependentId}/dependencies?limit=50&offset=0`) {
      dependencyPageRequests += 1
      response.writeHead(200, { 'content-type': 'application/json' })
      response.end(JSON.stringify({ items: dependencyPresent ? [ { id: prerequisiteId, name: 'Prerequisite' } ] : [], has_more: false }))
      return
    }
    response.writeHead(200, { 'content-type': 'text/html; charset=utf-8' })
    response.end(fixtureDocument())
  })
  await new Promise((resolveServer) => server.listen(0, '127.0.0.1', resolveServer))
  const address = server.address()
  if (!address || typeof address === 'string') throw new Error('Fixture server did not receive a TCP port')
  return {
    server,
    url: `http://127.0.0.1:${address.port}`,
    waitForMutation: () => mutationReceived,
    releaseMutationResponse,
    dependencyPageRequestCount: () => dependencyPageRequests,
    httpRequestCount: () => httpRequests,
  }
}

async function waitForCount(readCount, expected) {
  const deadline = Date.now() + 5000
  while (readCount() !== expected) {
    if (Date.now() > deadline) throw new Error(`Expected ${expected} dependency page requests, received ${readCount()}`)
    await new Promise(resolve => setTimeout(resolve, 10))
  }
}

function dependencyFrame(workspaceId, eventId, action) {
  return JSON.stringify({
    schema_version: 1,
    transport: 'frames',
    scope: { scope: 'workspace', workspace_id: workspaceId },
    frames: [{
      schema_version: 1,
      type: 'change',
      event: {
        schema_version: 1,
        event_id: eventId,
        scope: 'workspace',
        workspace_id: workspaceId,
        occurred_at: '2026-01-01T00:00:00Z',
        transaction: { id: `tx-${eventId}`, cause: 'rest', request_id: `request-${eventId}` },
        actor: { type: 'user', id: 'client-a' },
        entity: { type: 'task_dependency', id: `${dependentId}:${prerequisiteId}`, action },
        invalidations: [
          { kind: 'entity', target: `task_dependency:${dependentId}:${prerequisiteId}` },
          { kind: 'readiness', target: `task:${dependentId}` },
          { kind: 'search', target: `workspace:${workspaceId}` },
        ],
      },
    }],
  })
}

function resyncFrame() {
  return JSON.stringify({
    schema_version: 1,
    transport: 'frame',
    scope: { scope: 'workspace', workspace_id: 'workspace-1' },
    frame: { schema_version: 1, type: 'resync_required' },
  })
}

test('rendered +Dep candidate click emits the current-task dependency command', async () => {
  const requests = []
  const { server, url, waitForMutation, releaseMutationResponse } = await startFixtureServer(requests)
  let browser

  try {
    browser = await chromium.launch({ headless: true })
    const page = await browser.newPage()
    const pageErrors = []
    page.on('pageerror', error => pageErrors.push(error.message))
    await page.goto(url)
    await new Promise(resolve => setTimeout(resolve, 100))
    assert.deepEqual(pageErrors, [])
    const clientA = page.locator('#client-a')
    await clientA.getByRole('button', { name: '+ Dep' }).click()
    const candidate = clientA.locator('.popover-card')
    await candidate.waitFor()
    await assert.equal(await candidate.count(), 1)
    await candidate.click()
    await waitForMutation()
    await assert.equal(await clientA.locator('.dep-item').count(), 0)
    await assert.equal(await clientA.locator('.task-dependencies-title').textContent(), 'Dependencies (0)')
    releaseMutationResponse()
    await clientA.locator('.popover-container').waitFor({ state: 'detached' })
    const dependencyRow = clientA.locator('.dep-item')
    await dependencyRow.waitFor()
    await assert.equal(await dependencyRow.locator('.popover-card-title').textContent(), 'Prerequisite')
    await assert.equal(await clientA.locator('.task-dependencies-title').textContent(), 'Dependencies (1)')
    assert.equal(requests.length, 1)
    assert.deepEqual(requests[0], {
      method: 'POST',
      url: `/api/v1/tasks/${dependentId}/dependencies`,
      body: { depends_on_id: prerequisiteId, request_id: requests[0].body.request_id },
    })
    assert.match(requests[0].body.request_id, /.+/)
  } finally {
    await browser?.close()
    await new Promise((resolveServer, rejectServer) => server.close((error) => error ? rejectServer(error) : resolveServer()))
  }
})

test('a mutation response arriving after canonical reset is inert and does not fetch dependencies', async () => {
  const requests = []
  const { server, url, waitForMutation, releaseMutationResponse, dependencyPageRequestCount } = await startFixtureServer(requests)
  let browser

  try {
    browser = await chromium.launch({ headless: true })
    const page = await browser.newPage()
    await page.goto(url)
    const clientA = page.locator('#client-a')

    await clientA.getByRole('button', { name: '+ Dep' }).click()
    await clientA.locator('.popover-card').click()
    await waitForMutation()
    await page.evaluate(raw => window.dependencyClients.clientA.ports.wsMessage.send(raw), resyncFrame())
    releaseMutationResponse()
    await new Promise(resolve => setTimeout(resolve, 200))

    assert.equal(dependencyPageRequestCount(), 0)
    assert.equal(await clientA.locator('.dep-item').count(), 0)
    assert.equal(await clientA.locator('.task-dependencies-title').textContent(), 'Dependencies (0)')
  } finally {
    await browser?.close()
    await new Promise((resolveServer, rejectServer) => server.close((error) => error ? rejectServer(error) : resolveServer()))
  }
})

test('a second expanded client converges once for remote add/remove and makes foreign frames fully inert', async () => {
  const requests = []
  const { server, url, waitForMutation, releaseMutationResponse, dependencyPageRequestCount, httpRequestCount } = await startFixtureServer(requests)
  let browser

  try {
    browser = await chromium.launch({ headless: true })
    const page = await browser.newPage()
    await page.goto(url)
    const clientA = page.locator('#client-a')
    const clientB = page.locator('#client-b')

    await clientA.getByRole('button', { name: '+ Dep' }).click()
    await clientA.locator('.popover-card').click()
    await waitForMutation()
    releaseMutationResponse()
    await clientA.locator('.dep-item').waitFor()
    await waitForCount(dependencyPageRequestCount, 1)
    assert.equal(await clientB.locator('.dep-item').count(), 0)

    const beforeRemoteAdd = httpRequestCount()
    const addFrame = dependencyFrame('workspace-1', 'remote-add', 'created')
    await page.evaluate(raw => window.dependencyClients.clientB.ports.wsMessage.send(raw), addFrame)
    await clientB.locator('.dep-item').waitFor()
    await waitForCount(dependencyPageRequestCount, 2)
    await waitForCount(httpRequestCount, beforeRemoteAdd + 2)
    assert.equal(await clientB.locator('.task-dependencies-title').textContent(), 'Dependencies (1)')

    const beforeIgnoredFrames = httpRequestCount()
    await page.evaluate(raw => window.dependencyClients.clientB.ports.wsMessage.send(raw), addFrame)
    await page.evaluate(raw => window.dependencyClients.clientB.ports.wsMessage.send(raw), dependencyFrame('workspace-2', 'foreign-add', 'created'))
    await page.evaluate(raw => window.dependencyClients.clientB.ports.wsMessage.send(raw), dependencyFrame('workspace-2', 'foreign-delete', 'deleted'))
    await new Promise(resolve => setTimeout(resolve, 100))
    assert.equal(dependencyPageRequestCount(), 2)
    assert.equal(httpRequestCount(), beforeIgnoredFrames)
    assert.equal(await clientB.locator('.dep-item').count(), 1)

    const beforeLocalRemove = httpRequestCount()
    await clientA.getByTitle('Remove dependency').click()
    await waitForCount(dependencyPageRequestCount, 3)
    await waitForCount(httpRequestCount, beforeLocalRemove + 2)
    await clientA.locator('.dep-item').waitFor({ state: 'detached' })

    const beforeRemoteRemove = httpRequestCount()
    await page.evaluate(raw => window.dependencyClients.clientB.ports.wsMessage.send(raw), dependencyFrame('workspace-1', 'remote-remove', 'deleted'))
    await clientB.locator('.dep-item').waitFor({ state: 'detached' })
    await waitForCount(dependencyPageRequestCount, 4)
    await waitForCount(httpRequestCount, beforeRemoteRemove + 2)
    assert.equal(await clientB.locator('.task-dependencies-title').textContent(), 'Dependencies (0)')
    assert.deepEqual(requests.map(request => request.method), ['POST', 'DELETE'])
  } finally {
    await browser?.close()
    await new Promise((resolveServer, rejectServer) => server.close((error) => error ? rejectServer(error) : resolveServer()))
  }
})
