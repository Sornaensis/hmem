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
const asciiJson = value => JSON.stringify(value)
  .replace(/[\u0080-\uffff]/g, character => `\\u${character.charCodeAt(0).toString(16).padStart(4, '0')}`)

async function waitForScrub(page) {
  await page.waitForFunction(() => !/[?&#](?:hmem_token|auth_token|access_token|id_token|refresh_token)=/.test(location.href))
}

test('local static-bearer links update an open app and send ASCII-safe declared identity', { timeout: 45000 }, async () => {
  const h = await openAuthHarness()
  const requests = []
  const identity = { role: '审阅é', 'model-id': 'hmem:web:manual' }
  const identityHeader = asciiJson(identity)
  try {
    await configure(h, { authMode: 'local', requireAuthState: true, botIdentity: identity }, requests)
    await h.start('hmem_token=first-browser-token')
    await h.idle()

    assert.equal(new URL(h.page.url()).hash, '')
    assert.ok(requests.length > 0)
    assert.ok(requests.every(request => request.headers.authorization === 'Bearer first-browser-token'))
    assert.ok(requests.some(request => request.method === 'POST'))
    assert.ok(requests.every(request => request.headers['x-hmem-bot-identity'] === identityHeader))
    assert.match(identityHeader, /^[\x00-\x7f]+$/)

    await h.page.evaluate(() => { window.authRuntimeMarker = 'preserved' })
    const changed = sessionRequestWith(h.page, 'second-browser-token')
    await h.page.evaluate(() => { window.location.hash = '#hmem_token=second-browser-token' })
    await changed
    await waitForScrub(h.page)
    assert.equal(await h.page.evaluate(() => window.authRuntimeMarker), 'preserved')
    await h.idle()
    assert.ok(requests.filter(request => request.headers.authorization === 'Bearer second-browser-token')
      .every(request => request.headers['x-hmem-bot-identity'] === identityHeader))
  } finally {
    await h.close()
  }
})

test('a fresh logged-out app accepts a later local token hash without reloading', { timeout: 45000 }, async () => {
  const h = await openAuthHarness()
  const requests = []
  try {
    await configure(h, { authMode: 'local', requireAuthState: true }, requests)
    await h.page.route('**/api/v1/session**', route => route.request().headers().authorization
      ? route.fallback()
      : route.fulfill({ status: 401, contentType: 'application/json', body: '{}' }))
    await h.page.goto(`${h.origin}/workspace/${h.fixture.workspace.id}#tab=projects`)
    await h.page.getByRole('heading', { name: 'Local session unavailable', exact: true }).waitFor()
    await h.page.evaluate(() => { window.authRuntimeMarker = 'logged-out-state' })

    const accepted = sessionRequestWith(h.page, 'fresh-local-token')
    await h.page.evaluate(() => { window.location.hash = '#hmem_token=fresh-local-token' })
    await accepted
    await waitForScrub(h.page)
    await h.page.locator('#entity-root-project').waitFor()
    assert.equal(await h.page.evaluate(() => window.authRuntimeMarker), 'logged-out-state')
  } finally {
    await h.close()
  }
})

test('a successful later storage write replaces an earlier in-memory fallback token', { timeout: 45000 }, async () => {
  const h = await openAuthHarness()
  const requests = []
  try {
    await h.page.addInitScript(() => {
      const original = Storage.prototype.setItem
      let failed = false
      Storage.prototype.setItem = function (key, value) {
        if (this === localStorage && key === 'hmem-auth-token' && !failed) {
          failed = true
          throw new DOMException('controlled storage failure', 'QuotaExceededError')
        }
        return original.call(this, key, value)
      }
    })
    await configure(h, { authMode: 'local', requireAuthState: true, authTokenStorage: 'local' }, requests)
    await h.start('hmem_token=fallback-token')
    await h.idle()
    assert.ok(requests.some(request => request.headers.authorization === 'Bearer fallback-token'))

    const accepted = sessionRequestWith(h.page, 'stored-token')
    await h.page.evaluate(() => { window.location.hash = '#hmem_token=stored-token' })
    await accepted
    await waitForScrub(h.page)
    assert.deepEqual(await h.page.evaluate(() => ({ stored: localStorage.getItem('hmem-auth-token'), fallback: window.HMEM_CONFIG.authToken })),
      { stored: 'stored-token', fallback: null })
    assert.ok(!requests.slice(requests.findIndex(request => request.headers.authorization === 'Bearer stored-token'))
      .some(request => request.headers.authorization === 'Bearer fallback-token'))
  } finally {
    await h.close()
  }
})

test('a deployed callback storage failure uses the accepted token over an older stored token', { timeout: 45000 }, async () => {
  const h = await openAuthHarness()
  const requests = []
  try {
    await h.page.addInitScript(() => {
      const original = Storage.prototype.setItem
      original.call(localStorage, 'hmem-auth-token', 'stored-token-a')
      original.call(sessionStorage, 'hmem-auth-login-state', 'expected-state')
      let failed = false
      Storage.prototype.setItem = function (key, value) {
        if (this === localStorage && key === 'hmem-auth-token' && !failed) {
          failed = true
          throw new DOMException('controlled storage failure', 'QuotaExceededError')
        }
        return original.call(this, key, value)
      }
    })
    await configure(h, { authMode: 'deployed', requireAuthState: true, authTokenStorage: 'local' }, requests)
    const accepted = sessionRequestWith(h.page, 'accepted-token-b')
    await h.page.goto(`${h.origin}/#access_token=accepted-token-b&state=expected-state`)
    await accepted
    await waitForScrub(h.page)
    assert.deepEqual(await h.page.evaluate(() => ({ stored: localStorage.getItem('hmem-auth-token'), fallback: window.HMEM_CONFIG.authToken })),
      { stored: 'stored-token-a', fallback: 'accepted-token-b' })
    assert.ok(!requests.some(request => request.headers.authorization === 'Bearer stored-token-a'))
  } finally {
    await h.close()
  }
})

test('provider and query tokens retain state protections in local and deployed modes', { timeout: 90000 }, async () => {
  const local = await openAuthHarness()
  const localRequests = []
  try {
    await configure(local, { authMode: 'local', requireAuthState: true }, localRequests)
    const localSession = local.page.waitForRequest(request => new URL(request.url()).pathname === '/api/v1/session')
    await local.page.goto(`${local.origin}/#access_token=local-provider-token`)
    assert.equal((await localSession).headers().authorization, undefined)
    await waitForScrub(local.page)

    const querySession = local.page.waitForRequest(request => new URL(request.url()).pathname === '/api/v1/session')
    await local.page.goto(`${local.origin}/?hmem_token=query-token#tab=projects`)
    assert.equal((await querySession).headers().authorization, undefined)
    await waitForScrub(local.page)
    assert.equal(new URL(local.page.url()).searchParams.has('hmem_token'), false)
  } finally {
    await local.close()
  }

  const deployed = await openAuthHarness()
  const deployedRequests = []
  try {
    await configure(deployed, { authMode: 'deployed', requireAuthState: true }, deployedRequests)
    await deployed.page.addInitScript(() => sessionStorage.setItem('hmem-auth-login-state', 'expected-state'))

    const noStateStaticSession = deployed.page.waitForRequest(request => new URL(request.url()).pathname === '/api/v1/session')
    await deployed.page.goto(`${deployed.origin}/#hmem_token=deployed-static-without-state`)
    assert.equal((await noStateStaticSession).headers().authorization, undefined)
    await waitForScrub(deployed.page)

    const mismatchedStaticSession = deployed.page.waitForRequest(request => new URL(request.url()).pathname === '/api/v1/session')
    await deployed.page.goto(`${deployed.origin}/#hmem_token=deployed-static-mismatch&state=wrong-state`)
    assert.equal((await mismatchedStaticSession).headers().authorization, undefined)
    await waitForScrub(deployed.page)

    const mismatchedSession = deployed.page.waitForRequest(request => new URL(request.url()).pathname === '/api/v1/session')
    await deployed.page.goto(`${deployed.origin}/#access_token=mismatched-token&state=wrong-state`)
    assert.equal((await mismatchedSession).headers().authorization, undefined)
    await waitForScrub(deployed.page)

    const acceptedSession = sessionRequestWith(deployed.page, 'accepted-provider-token')
    await deployed.page.goto(`${deployed.origin}/#access_token=accepted-provider-token&state=expected-state`)
    await acceptedSession
    await waitForScrub(deployed.page)
    assert.equal(await deployed.page.evaluate(() => sessionStorage.getItem('hmem-auth-login-state')), null)

    await deployed.page.evaluate(() => { window.location.hash = '#access_token=replayed-token&state=expected-state' })
    await waitForScrub(deployed.page)
    await deployed.page.waitForTimeout(100)
    assert.ok(!deployedRequests.some(request => request.headers.authorization === 'Bearer replayed-token'))
    assert.ok(deployedRequests.some(request => request.headers.authorization === 'Bearer accepted-provider-token'))
  } finally {
    await deployed.close()
  }
})

test('popstate scrubs callback data before other navigation observers and login return paths', { timeout: 45000 }, async () => {
  const h = await openAuthHarness()
  const requests = []
  let tokenlessSessionHeaders = null
  try {
    await h.page.addInitScript(() => {
      window.observedPopstateUrls = []
      const addEventListener = window.addEventListener.bind(window)
      window.addEventListener = function (type, listener, options) {
        if (type !== 'popstate') return addEventListener(type, listener, options)
        return addEventListener(type, function (event) {
          window.observedPopstateUrls.push(location.href)
          return listener.call(this, event)
        }, options)
      }
    })
    await configure(h, {
      authMode: 'deployed',
      requireAuthState: true,
      loginUrl: '/login',
      serverSideOidcLogin: true,
      botIdentity: { role: 'Browser Operator', 'model-id': 'hmem:web:manual' },
    }, requests)
    await h.page.route('**/api/v1/session**', route => {
      tokenlessSessionHeaders = route.request().headers()
      return route.fulfill({ status: 401, contentType: 'application/json', body: '{}' })
    })
    await h.page.goto(`${h.origin}/workspace/${h.fixture.workspace.id}#tab=projects`)
    const signIn = h.page.getByRole('button', { name: 'Sign in', exact: true })
    await signIn.waitFor()
    assert.equal(tokenlessSessionHeaders['x-hmem-bot-identity'], undefined)

    await h.page.evaluate(() => {
      history.pushState({ callback: true }, '', `${location.pathname}?hmem_token=popstate-secret#tab=projects`)
      history.pushState({ clean: true }, '', `${location.pathname}#tab=projects`)
    })
    await h.page.goBack()
    await waitForScrub(h.page)
    const observed = await h.page.evaluate(() => window.observedPopstateUrls)
    assert.ok(observed.length >= 2)
    assert.ok(observed[0].includes('popstate-secret'))
    assert.ok(observed.slice(1).every(url => !url.includes('popstate-secret')))

    await signIn.click()
    await h.page.waitForURL(url => url.pathname === '/login')
    const loginUrl = new URL(h.page.url())
    const returnTo = loginUrl.searchParams.get('return_to') || ''
    assert.ok(!returnTo.includes('popstate-secret'))
    assert.ok(!/(?:hmem_token|auth_token|access_token|id_token|refresh_token)=/.test(returnTo))
  } finally {
    await h.close()
  }
})
