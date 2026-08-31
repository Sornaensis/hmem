import { Elm } from './Main.elm'
import { createCanonicalFrameBatcher, createChangeStreamManager } from './change-stream.js'

function createSessionId() {
  if (window.crypto && window.crypto.randomUUID) {
    return window.crypto.randomUUID()
  }

  return `session-${Date.now()}-${Math.random().toString(36).slice(2)}`
}

const runtimeConfig = window.HMEM_CONFIG || {}
const authTokenStorageKey = runtimeConfig.authTokenStorageKey || 'hmem-auth-token'
const authTokenStorageMode = normalizeAuthTokenStorageMode(runtimeConfig.authTokenStorage || runtimeConfig.authTokenStorageMode || 'local')
const runtimeMode = runtimeConfig.authMode || runtimeConfig.runtimeMode || import.meta.env.VITE_HMEM_AUTH_MODE || 'unknown'
const defaultAuthTokenUrlParams = ['hmem_token', 'auth_token', 'access_token']
function normalizeStringArray(value, fallback) {
  if (Array.isArray(value)) return value.filter((item) => typeof item === 'string' && item.length > 0)
  if (typeof value === 'string' && value.length > 0) return [value]
  return fallback
}
const authTokenUrlParams = normalizeStringArray(runtimeConfig.authTokenUrlParams, defaultAuthTokenUrlParams)
const sensitiveAuthUrlParams = Array.from(new Set([...defaultAuthTokenUrlParams, ...authTokenUrlParams, 'access_token', 'id_token', 'refresh_token']))
const authCallbackUrlParams = Array.from(new Set([...sensitiveAuthUrlParams, ...normalizeStringArray(runtimeConfig.authCallbackUrlParams, []), 'code', 'state', 'token_type', 'expires_in', 'scope', 'error', 'error_description']))
const authLoginStateStorageKey = runtimeConfig.authLoginStateStorageKey || 'hmem-auth-login-state'
const csrfCookieName = runtimeConfig.csrfCookieName || 'hmem_csrf'
const csrfHeaderName = runtimeConfig.csrfHeaderName || 'X-CSRF-Token'
const requireAuthState = runtimeConfig.requireAuthState !== false
let pendingAuthSessionError = null
let ignoreStoredAuthToken = false

function normalizeAuthTokenStorageMode(value) {
  if (value === 'memory' || value === 'session' || value === 'local') return value
  return 'local'
}

function configuredApiUrl() {
  return runtimeConfig.apiUrl || import.meta.env.VITE_HMEM_API_URL || window.location.origin
}

function normalizeBaseUrl(url) {
  return new URL(url, window.location.origin).toString().replace(/\/$/, '')
}

function configuredWsUrl(apiUrl) {
  if (runtimeConfig.wsUrl || import.meta.env.VITE_HMEM_WS_URL) {
    return runtimeConfig.wsUrl || import.meta.env.VITE_HMEM_WS_URL
  }

  const parsed = new URL(apiPath('/api/v1/ws'), apiUrl)
  parsed.protocol = parsed.protocol === 'https:' ? 'wss:' : 'ws:'
  return parsed.toString()
}

function apiBasePath() {
  return new URL(apiUrl, window.location.origin).pathname.replace(/\/$/, '')
}

function apiPath(path) {
  return `${apiBasePath()}${path.startsWith('/') ? path : `/${path}`}` || path
}

function apiResourceUrl(path) {
  return new URL(apiPath(path), apiUrl).toString()
}

function safeLocalStorageGet(key) {
  try {
    return localStorage.getItem(key)
  } catch (e) {
    return null
  }
}

function safeLocalStorageSet(key, value) {
  try {
    localStorage.setItem(key, value)
    return true
  } catch (e) {
    return false
  }
}

function safeLocalStorageRemove(key) {
  try {
    localStorage.removeItem(key)
    return true
  } catch (e) {}
  return false
}

function safeSessionStorageGet(key) {
  try {
    return sessionStorage.getItem(key)
  } catch (e) {
    return null
  }
}

function safeSessionStorageSet(key, value) {
  try {
    sessionStorage.setItem(key, value)
    return true
  } catch (e) {
    return false
  }
}

function safeSessionStorageRemove(key) {
  try {
    sessionStorage.removeItem(key)
  } catch (e) {}
}

function storedAuthTokenGet() {
  if (authTokenStorageMode === 'memory') return null
  if (authTokenStorageMode === 'session') return safeSessionStorageGet(authTokenStorageKey)
  return safeLocalStorageGet(authTokenStorageKey)
}

function storedAuthTokenSet(value) {
  if (authTokenStorageMode === 'memory') {
    runtimeConfig.authToken = value
    return true
  }
  if (authTokenStorageMode === 'session') return safeSessionStorageSet(authTokenStorageKey, value)
  return safeLocalStorageSet(authTokenStorageKey, value)
}

function storedAuthTokenRemove() {
  safeSessionStorageRemove(authTokenStorageKey)
  safeLocalStorageRemove(authTokenStorageKey)
}

function clearInactiveStoredAuthTokens() {
  if (authTokenStorageMode !== 'session') safeSessionStorageRemove(authTokenStorageKey)
  if (authTokenStorageMode !== 'local') safeLocalStorageRemove(authTokenStorageKey)
}

function extractTokenFromParams(params) {
  for (const name of authTokenUrlParams) {
    const value = params.get(name)
    if (value) return { name, value }
  }
  return null
}

function extractSensitiveAuthParam(params) {
  for (const name of sensitiveAuthUrlParams) {
    const value = params.get(name)
    if (value) return { name, value }
  }
  return null
}

function scrubAuthCallbackParams(url, hashParams, hashPrefix) {
  for (const name of authCallbackUrlParams) {
    url.searchParams.delete(name)
    if (hashParams) hashParams.delete(name)
  }
  if (hashParams) {
    const nextHash = hashParams.toString()
    const cleanPrefix = hashPrefix && hashPrefix.endsWith('?') ? hashPrefix.slice(0, -1) : hashPrefix
    url.hash = nextHash ? `#${hashPrefix || ''}${nextHash}` : (cleanPrefix ? `#${cleanPrefix}` : '')
  }
}

function parseHashParams(hash) {
  if (!hash) return { params: null, prefix: '' }
  const raw = hash.replace(/^#/, '')
  const queryIndex = raw.indexOf('?')
  if (queryIndex >= 0) {
    return { prefix: raw.slice(0, queryIndex + 1), params: new URLSearchParams(raw.slice(queryIndex + 1)) }
  }
  if (raw.includes('=')) return { prefix: '', params: new URLSearchParams(raw) }
  return { params: null, prefix: '' }
}

function captureAuthTokenFromUrl() {
  const url = new URL(window.location.href)
  const blockedSearchToken = extractSensitiveAuthParam(url.searchParams)
  const fromSearch = null
  const hasSearchCallbackMetadata = authCallbackUrlParams.some((name) => url.searchParams.has(name))

  let fromHash = null
  let hasHashCallbackMetadata = false
  const parsedHash = parseHashParams(url.hash)
  const hashParams = parsedHash.params
  const hashPrefix = parsedHash.prefix
  if (hashParams) {
    fromHash = extractTokenFromParams(hashParams)
    hasHashCallbackMetadata = authCallbackUrlParams.some((name) => hashParams.has(name))
  }

  const found = fromSearch || fromHash
  if (!found && !blockedSearchToken && !hasSearchCallbackMetadata && !hasHashCallbackMetadata) return null

  if (blockedSearchToken) {
    scrubAuthCallbackParams(url, hashParams, hashPrefix)
    window.history.replaceState(window.history.state, document.title, url.toString())
    pendingAuthSessionError = 'Ignored auth token in URL query string. Use a fragment-based provider callback instead.'
    return null
  }

  if (!found) {
    const callbackError = url.searchParams.get('error') || (hashParams ? hashParams.get('error') : null)
    const callbackErrorDescription = url.searchParams.get('error_description') || (hashParams ? hashParams.get('error_description') : null)
    scrubAuthCallbackParams(url, hashParams, hashPrefix)
    window.history.replaceState(window.history.state, document.title, url.toString())
    if (callbackError || callbackErrorDescription) {
      pendingAuthSessionError = 'Login provider did not return a usable token. Please sign in again.'
    }
    return null
  }

  const returnedState = (fromSearch ? url.searchParams.get('state') : null) || (hashParams ? hashParams.get('state') : null)
  const expectedState = safeSessionStorageGet(authLoginStateStorageKey)
  const stateAccepted = !requireAuthState || (expectedState !== null && returnedState === expectedState)

  if (stateAccepted) {
    ignoreStoredAuthToken = false
    if (!storedAuthTokenSet(found.value)) {
      runtimeConfig.authToken = found.value
      pendingAuthSessionError = 'Browser storage is unavailable; using the returned auth token for this tab only.'
    }
    safeSessionStorageRemove(authLoginStateStorageKey)
  } else {
    pendingAuthSessionError = 'Ignored auth callback because login state did not match. Please sign in again.'
    scrubAuthCallbackParams(url, hashParams, hashPrefix)
    window.history.replaceState(window.history.state, document.title, url.toString())
    return null
  }

  scrubAuthCallbackParams(url, hashParams, hashPrefix)
  window.history.replaceState(window.history.state, document.title, url.toString())
  return found.value
}

clearInactiveStoredAuthTokens()
captureAuthTokenFromUrl()

function currentAuthToken() {
  const storedToken = ignoreStoredAuthToken ? null : storedAuthTokenGet()
  if (runtimeMode === 'deployed') return storedToken || runtimeConfig.authToken || null
  return runtimeConfig.authToken || storedToken || null
}

function authTokenPresent() {
  return currentAuthToken() !== null
}

function authTokenSignature() {
  return currentAuthToken() || null
}

function authHeaderObject() {
  const token = currentAuthToken()
  return token ? { Authorization: `Bearer ${token}` } : {}
}

function readCookie(name) {
  const prefix = `${name}=`
  return document.cookie
    .split(';')
    .map((part) => part.trim())
    .find((part) => part.startsWith(prefix))
    ?.slice(prefix.length) || null
}

function csrfHeaderObject() {
  const token = readCookie(csrfCookieName)
  return token ? { [csrfHeaderName]: token } : {}
}

function isUnsafeMethod(method) {
  const normalized = String(method || 'GET').toUpperCase()
  return normalized !== 'GET' && normalized !== 'HEAD' && normalized !== 'OPTIONS'
}

function currentRelativeReturnPath(returnPath) {
  const parsed = new URL(returnPath || '/', window.location.href)
  return `${parsed.pathname}${parsed.search}${parsed.hash}`
}

const apiUrl = normalizeBaseUrl(configuredApiUrl())
const wsUrl = configuredWsUrl(apiUrl)
let lastUnauthorizedNotificationAt = 0
let notifyUnauthorized = function () {}
let lastAuthTokenSignature = authTokenSignature()
let authTokenGeneration = 0

function isApiRequestUrl(rawUrl) {
  try {
    const parsed = new URL(rawUrl, window.location.href)
    const apiBase = new URL(apiUrl, window.location.href)
    const apiPathPrefix = `${apiBasePath()}/api/`.replace(/^\/\//, '/')
    return parsed.origin === apiBase.origin && parsed.pathname.startsWith(apiPathPrefix)
  } catch (e) {
    return false
  }
}

function installAuthHeaderInterceptor() {
  if (XMLHttpRequest.prototype.__hmemAuthInterceptorInstalled) return
  XMLHttpRequest.prototype.__hmemAuthInterceptorInstalled = true

  const originalOpen = XMLHttpRequest.prototype.open
  const originalSend = XMLHttpRequest.prototype.send
  const originalSetRequestHeader = XMLHttpRequest.prototype.setRequestHeader

  XMLHttpRequest.prototype.open = function (method, url) {
    this.__hmemMethod = method
    this.__hmemRequestUrl = url
    this.__hmemHeaders = {}
    return originalOpen.apply(this, arguments)
  }

  XMLHttpRequest.prototype.setRequestHeader = function (name, value) {
    this.__hmemHeaders = this.__hmemHeaders || {}
    this.__hmemHeaders[String(name).toLowerCase()] = value
    return originalSetRequestHeader.apply(this, arguments)
  }

  XMLHttpRequest.prototype.send = function () {
    const requestAuthSignature = authTokenSignature()
    const requestAuthGeneration = authTokenGeneration
    if (isApiRequestUrl(this.__hmemRequestUrl)) {
      this.withCredentials = true
      const token = currentAuthToken()
      const hasAuthorization = this.__hmemHeaders && this.__hmemHeaders.authorization
      if (token && !hasAuthorization) {
        originalSetRequestHeader.call(this, 'Authorization', `Bearer ${token}`)
      }

      const hasCsrf = this.__hmemHeaders && this.__hmemHeaders[String(csrfHeaderName).toLowerCase()]
      if (isUnsafeMethod(this.__hmemMethod) && !hasCsrf) {
        const csrfToken = readCookie(csrfCookieName)
        if (csrfToken) originalSetRequestHeader.call(this, csrfHeaderName, csrfToken)
      }

      this.addEventListener('loadend', function () {
        if (this.status === 401 && requestAuthGeneration === authTokenGeneration && requestAuthSignature === authTokenSignature()) notifyUnauthorized()
      })
    }

    return originalSend.apply(this, arguments)
  }
}

installAuthHeaderInterceptor()

// Determine workspace ID from URL for loading stored filters at init
function getWorkspaceFilters() {
  const match = window.location.pathname.match(/^\/workspace\/([^/]+)/)
  if (!match) return null
  const key = 'hmem-ws-' + match[1]
  try {
    const raw = localStorage.getItem(key)
    return raw ? JSON.parse(raw) : null
  } catch (e) {
    return null
  }
}

const app = Elm.Main.init({
  node: document.getElementById('app'),
  flags: {
    apiUrl,
    wsUrl,
    sessionId: createSessionId(),
    runtimeMode,
    authTokenStorageKey: authTokenStorageMode === 'memory' ? 'in-memory' : authTokenStorageKey,
    authTokenPresent: authTokenPresent(),
    loginUrl: runtimeConfig.loginUrl || import.meta.env.VITE_HMEM_LOGIN_URL || null,
    logoutUrl: runtimeConfig.logoutUrl || import.meta.env.VITE_HMEM_LOGOUT_URL || null,
    storedFilters: getWorkspaceFilters()
  }
})

notifyUnauthorized = function () {
  const now = Date.now()
  if (now - lastUnauthorizedNotificationAt < 2000) return
  lastUnauthorizedNotificationAt = now
  if (app.ports.authUnauthorized) app.ports.authUnauthorized.send(null)
}

function notifyAuthTokenChanged(force) {
  const signature = authTokenSignature()
  if (!force && signature === lastAuthTokenSignature) return
  if (signature !== lastAuthTokenSignature) authTokenGeneration += 1
  lastAuthTokenSignature = signature
  if (app.ports.authTokenChanged) app.ports.authTokenChanged.send(signature !== null)
}

function notifyAuthSessionError(message) {
  if (app.ports.authSessionError) app.ports.authSessionError.send(message)
}

if (pendingAuthSessionError) {
  setTimeout(function () {
    notifyAuthSessionError(pendingAuthSessionError)
    pendingAuthSessionError = null
  }, 0)
}

function createAuthState() {
  if (window.crypto && window.crypto.randomUUID) return window.crypto.randomUUID()
  if (window.crypto && window.crypto.getRandomValues) {
    const bytes = new Uint8Array(16)
    window.crypto.getRandomValues(bytes)
    return Array.from(bytes, (byte) => byte.toString(16).padStart(2, '0')).join('')
  }
  return `auth-state-${Date.now()}-${Math.random().toString(36).slice(2)}`
}

window.addEventListener('storage', function (event) {
  if (authTokenStorageMode === 'local' && event.key === authTokenStorageKey) {
    if (event.newValue !== null) ignoreStoredAuthToken = false
    if (runtimeMode === 'deployed' && event.newValue === null) runtimeConfig.authToken = null
    notifyAuthTokenChanged(false)
  }
})

window.addEventListener('focus', function () {
  notifyAuthTokenChanged(false)
})

setInterval(function () {
  notifyAuthTokenChanged(false)
}, 5000)

if (app.ports.logoutAuth) {
  app.ports.logoutAuth.subscribe(function () {
    ignoreStoredAuthToken = true
    storedAuthTokenRemove()
    runtimeConfig.authToken = null
    notifyAuthTokenChanged(true)

    const logoutUrl = runtimeConfig.logoutUrl || import.meta.env.VITE_HMEM_LOGOUT_URL || null
    if (logoutUrl) {
      fetch(new URL(logoutUrl, window.location.href).toString(), {
        method: 'POST',
        headers: { ...authHeaderObject(), ...csrfHeaderObject() },
        credentials: 'include'
      }).finally(function () {
        window.location.assign(runtimeConfig.logoutRedirectUrl || '/')
      })
    }
  })
}

if (app.ports.loginAuth) {
  app.ports.loginAuth.subscribe(function (returnPath) {
    const loginUrl = runtimeConfig.loginUrl || import.meta.env.VITE_HMEM_LOGIN_URL || null
    if (!loginUrl) return
    const parsed = new URL(loginUrl, window.location.href)
    const serverSideOidcLogin = runtimeConfig.serverSideOidcLogin === true || parsed.pathname.endsWith('/api/v1/auth/login')
    if (!serverSideOidcLogin) {
      const state = createAuthState()
      if (requireAuthState && !safeSessionStorageSet(authLoginStateStorageKey, state)) {
        notifyAuthSessionError('Browser session storage is unavailable, so a secure provider login cannot be started.')
        return
      }
      parsed.searchParams.set('state', state)
    }
    if (!parsed.searchParams.has('return_to')) parsed.searchParams.set('return_to', currentRelativeReturnPath(returnPath))
    window.location.assign(parsed.toString())
  })
}

// ---------------------------------------------------------------------------
// WebSocket port
// ---------------------------------------------------------------------------

const canonicalScopeAudiences = new Map()
function canonicalScopeKey(scope, workspaceId) { return scope === 'global' ? 'global' : `workspace:${workspaceId}` }
const canonicalFrameBatcher = createCanonicalFrameBatcher({
  onBatch: function (scope, workspaceId, frames) {
    if (app.ports.wsMessage) app.ports.wsMessage.send(JSON.stringify({ transport: 'frames', schema_version: 1, scope: scope === 'global' ? { scope: 'global' } : { scope: 'workspace', workspace_id: workspaceId }, frames }))
  },
  onIncomplete: function (scope, workspaceId) {
    if (app.ports.wsMessage) app.ports.wsMessage.send(JSON.stringify({ transport: 'frame', schema_version: 1, scope: scope === 'global' ? { scope: 'global' } : { scope: 'workspace', workspace_id: workspaceId }, frame: { schema_version: 1, type: 'resync_required' } }))
  }
})
function clearCanonicalFrameBatch(scope, workspaceId) { canonicalFrameBatcher.clear(scope, workspaceId) }
function clearAllCanonicalFrameBatches() { canonicalFrameBatcher.clearAll() }

const canonicalStreams = createChangeStreamManager({
  apiUrl,
  wsUrl,
  storage: localStorage,
  onState: function (scope, workspaceId, state) {
    if (state === 'connecting' || state === 'replaying') { if (app.ports.wsConnecting) app.ports.wsConnecting.send(null) }
    else if (state === 'unauthenticated') { notifyUnauthorized() }
    else if (state === 'scope_forbidden') {
      clearCanonicalFrameBatch(scope, workspaceId)
      canonicalScopeAudiences.delete(canonicalScopeKey(scope, workspaceId))
      canonicalStreams.clear(scope, workspaceId)
      canonicalStreams.disconnect(scope, workspaceId)
      if (app.ports.wsMessage) app.ports.wsMessage.send(JSON.stringify({ transport: 'frame', schema_version: 1, scope: scope === 'global' ? { scope: 'global' } : { scope: 'workspace', workspace_id: workspaceId }, frame: { schema_version: 1, type: 'access_revoked', ...(scope === 'workspace' ? { workspace_id: workspaceId } : {}) } }))
    }
    else if (state === 'resync_required' || state === 'invalid_ticket') {
      if (app.ports.wsMessage) app.ports.wsMessage.send(JSON.stringify({ transport: 'frame', schema_version: 1, scope: scope === 'global' ? { scope: 'global' } : { scope: 'workspace', workspace_id: workspaceId }, frame: { schema_version: 1, type: 'resync_required' } }))
    }
    else if (state !== 'resyncing' && state !== 'control_closed' && app.ports.wsConnectionFailed) app.ports.wsConnectionFailed.send(`canonical:${state}:${scope}:${workspaceId || ''}`)
  },
  onSnapshot: function (scope, workspaceId, snapshot) {
    clearCanonicalFrameBatch(scope, workspaceId)
    if (app.ports.wsMessage) app.ports.wsMessage.send(JSON.stringify({ transport: 'snapshot', schema_version: 1, scope: scope === 'global' ? { scope: 'global' } : { scope: 'workspace', workspace_id: workspaceId }, items: snapshot.items, resume_token: snapshot.resumeToken, snapshot_profile: snapshot.snapshotProfile }))
  },
  onFrame: function (scope, workspaceId, frame) {
    let parsedFrame = null
    try { parsedFrame = JSON.parse(frame) } catch (_) { /* Elm fails closed below. */ }
    canonicalFrameBatcher.queue(scope, workspaceId, parsedFrame)
  }
})

app.ports.connectWebSocket.subscribe(function (config) {
  if (!config || (config.scope !== 'global' && (!config.workspaceId || config.scope !== 'workspace'))) return
  const streamWorkspaceId = config.scope === 'workspace' ? config.workspaceId : null
  const streamKey = canonicalScopeKey(config.scope, streamWorkspaceId)
  if (canonicalScopeAudiences.has(streamKey) && canonicalScopeAudiences.get(streamKey) !== config.audienceId) clearCanonicalFrameBatch(config.scope, streamWorkspaceId)
  canonicalScopeAudiences.set(streamKey, config.audienceId)
  if (config.forceResync) {
    clearCanonicalFrameBatch(config.scope, streamWorkspaceId)
    canonicalStreams.clear(config.scope, streamWorkspaceId, config.audienceId)
    canonicalStreams.disconnect(config.scope, streamWorkspaceId)
  }
  canonicalStreams.connect({
    audienceId: config.audienceId,
    scope: config.scope,
    workspaceId: streamWorkspaceId,
    snapshotProfile: config.snapshotProfile,
    headers: { ...authHeaderObject(), ...csrfHeaderObject() }
  })
})

if (app.ports.disconnectWebSocket) {
  app.ports.disconnectWebSocket.subscribe(function () {
    clearAllCanonicalFrameBatches()
    canonicalScopeAudiences.clear()
    canonicalStreams.disconnectAll()
  })
}

if (app.ports.disconnectChangeStreamScope) {
  app.ports.disconnectChangeStreamScope.subscribe(function (config) {
    if (!config || (config.scope !== 'global' && config.scope !== 'workspace')) return
    const streamWorkspaceId = config.scope === 'workspace' ? config.workspaceId : null
    clearCanonicalFrameBatch(config.scope, streamWorkspaceId)
    canonicalScopeAudiences.delete(canonicalScopeKey(config.scope, streamWorkspaceId))
    canonicalStreams.disconnect(config.scope, streamWorkspaceId)
  })
}

if (app.ports.clearChangeStreamScope) {
  app.ports.clearChangeStreamScope.subscribe(function (config) {
    if (!config || (config.scope !== 'global' && config.scope !== 'workspace')) return
    canonicalStreams.clear(config.scope, config.scope === 'workspace' ? config.workspaceId : null, config.audienceId)
  })
}

// Clipboard
app.ports.copyToClipboard.subscribe(function (text) {
  navigator.clipboard.writeText(text)
})

// ---------------------------------------------------------------------------
// Local storage
// ---------------------------------------------------------------------------

app.ports.saveToLocalStorage.subscribe(function (data) {
  try {
    localStorage.setItem(data.key, JSON.stringify(data.value))
  } catch (e) {
    // localStorage may be full or unavailable
  }
})

app.ports.requestLocalStorage.subscribe(function (key) {
  try {
    const raw = localStorage.getItem(key)
    const value = raw ? JSON.parse(raw) : null
    if (value) {
      // Defer send to ensure Elm runtime is ready to receive subscription messages
      requestAnimationFrame(function () {
        app.ports.localStorageReceived.send(value)
      })
    }
  } catch (e) {
    // ignore parse errors
  }
})

// ---------------------------------------------------------------------------
// Scroll tracking for sticky workspace bar
// ---------------------------------------------------------------------------

;(function () {
  let ticking = false
  let wasAbove = false
  const threshold = 200
  function onScroll () {
    if (!ticking) {
      requestAnimationFrame(function () {
        const el = document.getElementById('main-content-scroll')
        if (el) {
          const isAbove = el.scrollTop > threshold
          if (isAbove !== wasAbove) {
            wasAbove = isAbove
            app.ports.onMainContentScroll.send(el.scrollTop)
          }
        }
        ticking = false
      })
      ticking = true
    }
  }
  function attach (el) {
    el.addEventListener('scroll', onScroll, { passive: true })
  }
  const existing = document.getElementById('main-content-scroll')
  if (existing) {
    attach(existing)
  } else {
    const observer = new MutationObserver(function () {
      const el = document.getElementById('main-content-scroll')
      if (el) {
        attach(el)
        observer.disconnect()
      }
    })
    observer.observe(document.body, { childList: true, subtree: true })
  }
})()
