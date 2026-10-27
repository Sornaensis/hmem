import assert from 'node:assert/strict'
import test from 'node:test'
import { openHierarchy } from './hierarchy-fixture.mjs'

function openAuthHarness() {
  const executablePath = process.env.HMEM_PLAYWRIGHT_CHROMIUM
  return openHierarchy(undefined, [], null, {}, executablePath ? { executablePath } : {})
}

async function configure(h, config, requests) {
  await h.page.route('**/hmem-runtime-config.js', route => route.fulfill({
    contentType: 'application/javascript',
    body: `window.HMEM_CONFIG = ${JSON.stringify({ apiUrl: h.origin, wsUrl: 'ws://fixture.invalid/api/v1/ws', authTokenStorage: 'memory', ...config })};`,
  }))
  await h.page.route('**/api/v1/**', route => {
    const request = route.request()
    requests.push({ method: request.method(), path: new URL(request.url()).pathname, headers: request.headers() })
    return route.fallback()
  })
}

const sessionRequestWith = (page, token) => page.waitForRequest(request =>
  new URL(request.url()).pathname === '/api/v1/session'
    && request.headers().authorization === `Bearer ${token}`
)

test('local static-bearer links bypass provider state, update an open app, and declare browser identity', { timeout: 45000 }, async () => {
  const h = await openAuthHarness()
  const requests = []
  const identity = { role: 'Browser Operator', 'model-id': 'hmem:web:manual' }
  try {
    await configure(h, { authMode: 'local', requireAuthState: true, botIdentity: identity }, requests)
    await h.start('hmem_token=first-browser-token')
    await h.idle()

    assert.equal(new URL(h.page.url()).hash, '')
    assert.ok(requests.length > 0)
    assert.ok(requests.every(request => request.headers.authorization === 'Bearer first-browser-token'))
    assert.ok(requests.some(request => request.method === 'POST'))
    assert.ok(requests.every(request => request.headers['x-hmem-bot-identity'] === JSON.stringify(identity)))

    await h.page.evaluate(() => { window.authRuntimeMarker = 'preserved' })
    const changed = sessionRequestWith(h.page, 'second-browser-token')
    await h.page.evaluate(() => { window.location.hash = '#hmem_token=second-browser-token' })
    await changed
    await h.page.waitForFunction(() => window.location.hash === '')
    assert.equal(await h.page.evaluate(() => window.authRuntimeMarker), 'preserved')
    await h.idle()
    assert.ok(requests.some(request => request.headers.authorization === 'Bearer second-browser-token'))
    assert.ok(requests.filter(request => request.headers.authorization === 'Bearer second-browser-token')
      .every(request => request.headers['x-hmem-bot-identity'] === JSON.stringify(identity)))
  } finally {
    await h.close()
  }
})

test('deployed bearer callbacks still require matching state', { timeout: 45000 }, async () => {
  const h = await openAuthHarness()
  const requests = []
  const identity = { role: 'Browser Operator', 'model-id': 'hmem:web:manual' }
  try {
    await configure(h, { authMode: 'deployed', requireAuthState: true, botIdentity: identity }, requests)

    const rejectedSession = h.page.waitForRequest(request => new URL(request.url()).pathname === '/api/v1/session')
    await h.page.goto(`${h.origin}/#hmem_token=rejected-without-state`)
    const rejected = await rejectedSession
    assert.equal(rejected.headers().authorization, undefined)
    assert.equal(rejected.headers()['x-hmem-bot-identity'], undefined)
    await h.page.waitForFunction(() => window.location.hash === '')

    await h.page.evaluate(() => sessionStorage.setItem('hmem-auth-login-state', 'expected-state'))
    const acceptedSession = sessionRequestWith(h.page, 'accepted-provider-token')
    await h.page.goto(`${h.origin}/#access_token=accepted-provider-token&state=expected-state`)
    await acceptedSession
    await h.page.waitForFunction(() => window.location.hash === '')
    assert.ok(requests.some(request => request.headers.authorization === 'Bearer accepted-provider-token'))
    assert.ok(requests.filter(request => request.headers.authorization === 'Bearer accepted-provider-token')
      .every(request => request.headers['x-hmem-bot-identity'] === JSON.stringify(identity)))
  } finally {
    await h.close()
  }
})
