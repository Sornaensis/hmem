import assert from 'node:assert/strict'
import { readFile, unlink } from 'node:fs/promises'
import { createServer } from 'node:http'
import { resolve } from 'node:path'
import test from 'node:test'
import { chromium } from '@playwright/test'

const fixtureBundle = resolve(process.cwd(), '.dependencies-fixture-test.js')
const dependentId = 'current-task'
const prerequisiteId = 'selected-prerequisite'

function fixtureDocument() {
  return `<!DOCTYPE html>
<html lang="en">
<head><meta charset="UTF-8" /><title>Dependency selector fixture</title><link rel="stylesheet" href="/style.css" /></head>
<body><div id="app"></div><script src="/DependenciesFixture.js"></script><script>Elm.DependenciesFixture.init({ node: document.getElementById('app') })</script></body>
</html>`
}

async function startFixtureServer(requests) {
  let markMutationReceived
  const mutationReceived = new Promise((resolveMutationReceived) => { markMutationReceived = resolveMutationReceived })
  let releaseMutationResponse
  const mutationResponseReleased = new Promise((resolveMutationResponse) => { releaseMutationResponse = resolveMutationResponse })
  const [ bundle, stylesheet ] = await Promise.all([
    readFile(fixtureBundle),
    readFile(resolve(process.cwd(), 'src/style.css')),
  ])
  const server = createServer((request, response) => {
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
        markMutationReceived()
        mutationResponseReleased.then(() => {
          response.writeHead(200, { 'content-type': 'application/json' })
          response.end(JSON.stringify({ action: 'add', task_id: dependentId, depends_on_id: prerequisiteId, affected_tasks: [] }))
        })
      })
      return
    }
    if (request.method === 'GET' && request.url === `/api/v1/tasks/${dependentId}/dependencies?limit=50&offset=0`) {
      response.writeHead(200, { 'content-type': 'application/json' })
      response.end(JSON.stringify({ items: [ { id: prerequisiteId, name: 'Prerequisite' } ], has_more: false }))
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
  }
}

test('rendered +Dep candidate click emits the current-task dependency command', async () => {
  const requests = []
  const { server, url, waitForMutation, releaseMutationResponse } = await startFixtureServer(requests)
  let browser

  try {
    browser = await chromium.launch({ headless: true })
    const page = await browser.newPage()
    await page.goto(url)
    await page.getByRole('button', { name: '+ Dep' }).click()
    const candidate = page.locator('.popover-card')
    await candidate.waitFor()
    await assert.equal(await candidate.count(), 1)
    await candidate.click()
    await waitForMutation()
    await assert.equal(await page.locator('.dep-item').count(), 0)
    await assert.equal(await page.locator('.task-dependencies-title').textContent(), 'Dependencies (0)')
    releaseMutationResponse()
    await page.waitForFunction(() => document.querySelector('.popover-card') === null)
    const dependencyRow = page.locator('.dep-item')
    await dependencyRow.waitFor()
    await assert.equal(await dependencyRow.locator('.popover-card-title').textContent(), 'Prerequisite')
    await assert.equal(await page.locator('.task-dependencies-title').textContent(), 'Dependencies (1)')
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
    await unlink(fixtureBundle).catch(() => {})
  }
})
