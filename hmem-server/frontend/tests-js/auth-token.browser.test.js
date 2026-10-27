import assert from 'node:assert/strict'
import test from 'node:test'
import { openHierarchy } from './hierarchy-fixture.mjs'

const storageKey = 'hmem-auth-token'
const stateKey = 'hmem-auth-login-state'
const token = 'browser-regression-token'

async function openAuth({ mode = 'local', storage = 'local', expectedState = null } = {}) {
  const h = await openHierarchy()
  const authorization = []
  h.page.on('request', request => {
    if (new URL(request.url()).pathname.startsWith('/api/v1/')) authorization.push(request.headers().authorization || null)
  })
  await h.page.route('**/hmem-runtime-config.js', route => route.fulfill({
    contentType: 'application/javascript',
    body: `window.HMEM_CONFIG=${JSON.stringify({ authMode: mode, authTokenStorage: storage })};`
  }))
  if (expectedState !== null) await h.page.addInitScript(({ key, value }) => sessionStorage.setItem(key, value), { key: stateKey, value: expectedState })
  return { ...h, authorization }
}

for (const storage of ['local', 'session', 'memory']) test('local hmem_token bootstrap authenticates with ' + storage + ' storage and scrubs the URL', { timeout: 30000 }, async () => {
  const h = await openAuth({ storage })
  try {
    const authenticated = h.page.waitForRequest(request => new URL(request.url()).pathname === '/api/v1/session' && request.headers().authorization === 'Bearer ' + token)
    await h.page.goto(h.origin + '/#hmem_token=' + token + '&tab=observations')
    await authenticated
    const state = await h.page.evaluate(({ storageKey, stateKey }) => ({
      local: localStorage.getItem(storageKey), session: sessionStorage.getItem(storageKey),
      loginState: sessionStorage.getItem(stateKey), hash: location.hash
    }), { storageKey, stateKey })
    assert.equal(state.local, storage === 'local' ? token : null)
    assert.equal(state.session, storage === 'session' ? token : null)
    assert.equal(state.loginState, null)
    assert.equal(state.hash, '#tab=observations')
    assert.deepEqual(h.errors, [])
  } finally { await h.close() }
})

for (const scenario of [
  { name: 'deployed static-token fragment', mode: 'deployed', hash: '#hmem_token=' + token },
  { name: 'local provider token', hash: '#access_token=' + token },
  { name: 'local mixed provider metadata', hash: '#hmem_token=' + token + '&access_token=other' },
  { name: 'local returned state without a login', hash: '#hmem_token=' + token + '&state=unexpected' },
  { name: 'local outstanding login', expectedState: 'pending-login', hash: '#hmem_token=' + token },
  { name: 'deployed mismatched state', mode: 'deployed', expectedState: 'pending-login', hash: '#hmem_token=' + token + '&state=wrong' }
]) test(scenario.name + ' retains callback state validation', { timeout: 30000 }, async () => {
  const h = await openAuth(scenario)
  try {
    await h.page.goto(h.origin + '/' + scenario.hash)
    await h.page.getByText('Ignored auth callback because login state did not match. Please sign in again.', { exact: false }).waitFor()
    assert.equal(await h.page.evaluate(key => localStorage.getItem(key), storageKey), null)
    assert.equal(await h.page.evaluate(key => sessionStorage.getItem(key), stateKey), scenario.expectedState || null)
    assert.equal(new URL(h.page.url()).hash, '')
    assert.ok(!h.authorization.some(value => value?.includes(token)))
    assert.deepEqual(h.errors, [])
  } finally { await h.close() }
})

test('deployed provider callback with matching state still authenticates', { timeout: 30000 }, async () => {
  const h = await openAuth({ mode: 'deployed', expectedState: 'pending-login' })
  try {
    const authenticated = h.page.waitForRequest(request => new URL(request.url()).pathname === '/api/v1/session' && request.headers().authorization === 'Bearer ' + token)
    await h.page.goto(h.origin + '/#access_token=' + token + '&state=pending-login')
    await authenticated
    assert.equal(await h.page.evaluate(key => localStorage.getItem(key), storageKey), token)
    assert.equal(await h.page.evaluate(key => sessionStorage.getItem(key), stateKey), null)
    assert.equal(new URL(h.page.url()).hash, '')
    assert.deepEqual(h.errors, [])
  } finally { await h.close() }
})

test('query-string credentials cannot use local bootstrap', { timeout: 30000 }, async () => {
  const h = await openAuth()
  try {
    await h.page.goto(h.origin + '/?hmem_token=' + token + '#hmem_token=' + token)
    await h.page.getByText('Ignored auth token in URL query string.', { exact: false }).waitFor()
    assert.equal(await h.page.evaluate(key => localStorage.getItem(key), storageKey), null)
    assert.equal(new URL(h.page.url()).search, '')
    assert.equal(new URL(h.page.url()).hash, '')
    assert.ok(!h.authorization.some(value => value?.includes(token)))
    assert.deepEqual(h.errors, [])
  } finally { await h.close() }
})
