import { Elm } from './Main.elm'
import cytoscape from 'cytoscape'

function createSessionId() {
  if (window.crypto && window.crypto.randomUUID) {
    return window.crypto.randomUUID()
  }

  return `session-${Date.now()}-${Math.random().toString(36).slice(2)}`
}

const runtimeConfig = window.HMEM_CONFIG || {}
const authTokenStorageKey = runtimeConfig.authTokenStorageKey || 'hmem-auth-token'
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
const requireAuthState = runtimeConfig.requireAuthState !== false
let pendingAuthSessionError = null
let ignoreStoredAuthToken = false

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
    if (!safeLocalStorageSet(authTokenStorageKey, found.value)) {
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

captureAuthTokenFromUrl()

function currentAuthToken() {
  const storedToken = ignoreStoredAuthToken ? null : safeLocalStorageGet(authTokenStorageKey)
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
      const token = currentAuthToken()
      const hasAuthorization = this.__hmemHeaders && this.__hmemHeaders.authorization
      if (token && !hasAuthorization) {
        originalSetRequestHeader.call(this, 'Authorization', `Bearer ${token}`)
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
    authTokenStorageKey,
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
  if (event.key === authTokenStorageKey) {
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
    safeLocalStorageRemove(authTokenStorageKey)
    runtimeConfig.authToken = null
    notifyAuthTokenChanged(true)

    const logoutUrl = runtimeConfig.logoutUrl || import.meta.env.VITE_HMEM_LOGOUT_URL || null
    if (logoutUrl) window.location.assign(logoutUrl)
  })
}

if (app.ports.loginAuth) {
  app.ports.loginAuth.subscribe(function (returnPath) {
    const loginUrl = runtimeConfig.loginUrl || import.meta.env.VITE_HMEM_LOGIN_URL || null
    if (!loginUrl) return
    const state = createAuthState()
    if (requireAuthState && !safeSessionStorageSet(authLoginStateStorageKey, state)) {
      notifyAuthSessionError('Browser session storage is unavailable, so a secure provider login cannot be started.')
      return
    }
    const parsed = new URL(loginUrl, window.location.href)
    parsed.searchParams.set('state', state)
    if (!parsed.searchParams.has('return_to')) parsed.searchParams.set('return_to', new URL(returnPath || '/', window.location.href).toString())
    window.location.assign(parsed.toString())
  })
}

// ---------------------------------------------------------------------------
// WebSocket port
// ---------------------------------------------------------------------------

let ws = null
let reconnectTimer = null
let connectGeneration = 0
let ticketRetryAttempts = 0
let socketRetryAttempts = 0
let preOpenFailureCount = 0
const maxPreOpenHandshakeFailures = 3

function currentWorkspaceId() {
  const basePath = apiBasePath()
  let pathname = window.location.pathname
  if (basePath && pathname.startsWith(basePath + '/')) {
    pathname = pathname.slice(basePath.length)
  } else if (basePath && pathname === basePath) {
    pathname = '/'
  }

  const match = pathname.match(/^\/workspace\/([^/]+)/)
  return match ? decodeURIComponent(match[1]) : null
}

function wsUrlWithTicket(url, ticket) {
  const parsed = new URL(url, window.location.href)
  parsed.searchParams.set('ticket', ticket)
  return parsed.toString()
}

function wsUrlWithLocalToken(url) {
  const token = currentAuthToken()
  if (!token) return url
  const parsed = new URL(url, window.location.href)
  parsed.searchParams.set('token', token)
  return parsed.toString()
}

function normalizeWsConfig(rawConfig) {
  if (typeof rawConfig === 'string') {
    return {
      url: rawConfig,
      workspaceId: currentWorkspaceId(),
      authMode: runtimeMode,
      sessionId: null
    }
  }

  return {
    url: rawConfig && rawConfig.url ? rawConfig.url : wsUrl,
    workspaceId: rawConfig && rawConfig.workspaceId ? rawConfig.workspaceId : null,
    authMode: rawConfig && rawConfig.authMode ? rawConfig.authMode : runtimeMode,
    sessionId: rawConfig && rawConfig.sessionId ? rawConfig.sessionId : null
  }
}

function notifyWsConnecting() {
  if (app.ports.wsConnecting) app.ports.wsConnecting.send(null)
}

function notifyWsConnectionFailed(reason) {
  if (app.ports.wsConnectionFailed) app.ports.wsConnectionFailed.send(reason)
}

async function resolveWsUrl(config) {
  if (!config.workspaceId) throw new Error('ws-config:no-workspace')

  if (config.authMode === 'local') {
    return wsUrlWithLocalToken(config.url)
  }

  const requestAuthSignature = authTokenSignature()
  const requestAuthGeneration = authTokenGeneration
  const response = await fetch(apiResourceUrl('/api/v1/ws-ticket'), {
    method: 'POST',
    headers: { 'Content-Type': 'application/json', ...authHeaderObject() },
    credentials: 'include',
    body: JSON.stringify({ workspace_id: config.workspaceId })
  })

  if (!response.ok) {
    if (response.status === 401) {
      if (requestAuthGeneration === authTokenGeneration && requestAuthSignature === authTokenSignature()) {
        notifyUnauthorized()
      } else {
        throw new Error('ws-ticket-transient:stale-401')
      }
      throw new Error('ws-auth:unauthorized')
    }
    if (response.status === 403) throw new Error('ws-auth:forbidden')
    if (isTransientTicketStatus(response.status)) throw new Error(`ws-ticket-transient:${response.status}`)
    throw new Error(`ws-config:ticket-${response.status}`)
  }

  let body
  try {
    body = await response.json()
  } catch (err) {
    throw new Error('ws-config:invalid-ticket-response')
  }
  if (!body || !body.ticket) throw new Error('ws-config:missing-ticket')
  return wsUrlWithTicket(config.url, body.ticket)
}

function isTransientTicketStatus(status) {
  return status === 408 || status === 429 || status >= 500
}

function retryDelayMs(attempt) {
  return Math.min(30000, 1000 * Math.pow(2, attempt))
}

function nextTicketRetryDelayMs() {
  const delay = retryDelayMs(ticketRetryAttempts)
  ticketRetryAttempts += 1
  return delay
}

function nextSocketRetryDelayMs(opened) {
  const attempt = opened ? socketRetryAttempts : Math.max(0, preOpenFailureCount - 1)
  const delay = retryDelayMs(attempt)
  if (opened) socketRetryAttempts += 1
  return delay
}

function shouldStopReconnect(reason) {
  return reason.startsWith('ws-auth:') || reason.startsWith('ws-config:')
}

function terminalPreOpenFailureReason(config, opened) {
  if (opened) return null
  preOpenFailureCount += 1
  // Treat the third consecutive pre-open close as terminal. This bounds
  // handshake failures to one initial attempt plus two retries, independent
  // of transient ticket-fetch retries.
  if (preOpenFailureCount < maxPreOpenHandshakeFailures) return null
  return config.authMode === 'local'
    ? 'ws-auth:local-handshake-failed'
    : 'ws-config:handshake-failed'
}

function connectWs(rawConfig, preserveRetryState) {
  const config = normalizeWsConfig(rawConfig)
  if (!preserveRetryState) {
    ticketRetryAttempts = 0
    socketRetryAttempts = 0
    preOpenFailureCount = 0
  }
  connectGeneration += 1
  const generation = connectGeneration
  if (ws) { ws.close() }
  if (reconnectTimer) { clearTimeout(reconnectTimer); reconnectTimer = null }
  notifyWsConnecting()

  resolveWsUrl(config).then(function (resolvedUrl) {
    if (generation !== connectGeneration) return
    ticketRetryAttempts = 0

    let socket
    try {
      socket = new WebSocket(resolvedUrl)
    } catch (err) {
      notifyWsConnectionFailed('connect:invalid-url')
      return
    }
    ws = socket
    let opened = false

    socket.onopen = function () {
      if (generation !== connectGeneration || ws !== socket) return
      opened = true
      ticketRetryAttempts = 0
      socketRetryAttempts = 0
      preOpenFailureCount = 0
      app.ports.wsConnected.send(null)
    }

    socket.onmessage = function (event) {
      if (generation !== connectGeneration || ws !== socket) return
      app.ports.wsMessage.send(event.data)
    }

    socket.onclose = function () {
      if (generation !== connectGeneration || ws !== socket) return
      ws = null
      const terminalPreOpenReason = terminalPreOpenFailureReason(config, opened)
      if (terminalPreOpenReason) {
        notifyWsConnectionFailed(terminalPreOpenReason)
        return
      }
      app.ports.wsDisconnected.send(null)
      // Auto-reconnect after 3 seconds. Resolve a fresh ticket each time.
      reconnectTimer = setTimeout(function () {
        connectWs(config, true)
      }, nextSocketRetryDelayMs(opened))
    }

    socket.onerror = function () {
      // onclose will fire after onerror
    }
  }).catch(function (err) {
    if (generation !== connectGeneration) return
    const reason = err && err.message ? String(err.message) : 'connect:failed'
    if (shouldStopReconnect(reason)) {
      notifyWsConnectionFailed(reason)
      return
    }
    app.ports.wsDisconnected.send(null)
    reconnectTimer = setTimeout(function () {
      connectWs(config, true)
    }, nextTicketRetryDelayMs())
  })
}

app.ports.connectWebSocket.subscribe(connectWs)

if (app.ports.sendWebSocket) {
  app.ports.sendWebSocket.subscribe(function (message) {
    if (ws && ws.readyState === WebSocket.OPEN) {
      ws.send(message)
    }
  })
}

if (app.ports.disconnectWebSocket) {
  app.ports.disconnectWebSocket.subscribe(function () {
    connectGeneration += 1
    ticketRetryAttempts = 0
    socketRetryAttempts = 0
    preOpenFailureCount = 0
    if (reconnectTimer) { clearTimeout(reconnectTimer); reconnectTimer = null }
    if (ws) { ws.close(); ws = null }
  })
}

// ---------------------------------------------------------------------------
// Cytoscape port
// ---------------------------------------------------------------------------

let cy = null

const defaultStyle = [
  {
    selector: 'node',
    style: {
      'label': 'data(label)',
      'background-color': '#6366f1',
      'color': '#f8fafc',
      'text-valign': 'center',
      'text-halign': 'center',
      'font-size': '12px',
      'width': 'label',
      'height': 'label',
      'padding': '10px',
      'shape': 'roundrectangle',
      'border-width': 1,
      'border-color': '#818cf8'
    }
  },
  {
    selector: 'node.memory',
    style: {
      'background-color': '#0d9488',
      'border-color': '#2dd4bf'
    }
  },
  {
    selector: 'node.project',
    style: {
      'background-color': '#6366f1',
      'border-color': '#818cf8'
    }
  },
  {
    selector: 'node.task',
    style: {
      'background-color': '#d97706',
      'border-color': '#fbbf24'
    }
  },
  {
    selector: 'edge',
    style: {
      'width': 2,
      'line-color': '#475569',
      'target-arrow-color': '#475569',
      'target-arrow-shape': 'triangle',
      'curve-style': 'bezier',
      'label': 'data(label)',
      'font-size': '10px',
      'color': '#94a3b8'
    }
  },
  {
    selector: 'edge.dependency',
    style: {
      'line-color': '#ef4444',
      'target-arrow-color': '#ef4444',
      'line-style': 'dashed'
    }
  },
  {
    selector: 'edge.entity-memory',
    style: {
      'line-color': '#64748b',
      'target-arrow-color': '#64748b',
      'target-arrow-shape': 'none',
      'line-style': 'dotted',
      'width': 1.5
    }
  },
  {
    selector: ':selected',
    style: {
      'border-width': 3,
      'border-color': '#f59e0b'
    }
  }
]

let currentPositionsKey = null

app.ports.initCytoscape.subscribe(function (config) {
  // Delay to ensure Elm has rendered the container into the DOM
  requestAnimationFrame(function () {
    initCytoscapeGraph(config)
  })
})

app.ports.destroyCytoscape.subscribe(function () {
  if (cy) {
    saveGraphPositions()
    cy.destroy()
    cy = null
  }
})

function saveGraphPositions() {
  if (!cy || !currentPositionsKey) return
  try {
    const positions = {}
    cy.nodes().forEach(function (node) {
      const pos = node.position()
      positions[node.id()] = { x: pos.x, y: pos.y }
    })
    localStorage.setItem(currentPositionsKey, JSON.stringify(positions))
  } catch (e) {
    // localStorage may be full or unavailable
  }
}

function loadGraphPositions(key) {
  try {
    const raw = localStorage.getItem(key)
    return raw ? JSON.parse(raw) : null
  } catch (e) {
    return null
  }
}

function initCytoscapeGraph(config) {
  const container = document.getElementById(config.containerId)
  if (!container) return

  if (cy) {
    saveGraphPositions()
    cy.destroy()
  }

  currentPositionsKey = config.positionsKey || null
  const savedPositions = currentPositionsKey ? loadGraphPositions(currentPositionsKey) : null
  const hasSavedPositions = savedPositions && Object.keys(savedPositions).length > 0

  cy = cytoscape({
    container: container,
    elements: config.elements || [],
    style: config.style || defaultStyle,
    layout: { name: 'preset' } // start with no layout, apply below
  })

  if (hasSavedPositions) {
    // Apply saved positions to nodes that have them
    let hasUnsaved = false
    cy.nodes().forEach(function (node) {
      const saved = savedPositions[node.id()]
      if (saved) {
        node.position(saved)
      } else {
        hasUnsaved = true
      }
    })
    // Run layout only for nodes without saved positions
    if (hasUnsaved) {
      const unsavedNodes = cy.nodes().filter(function (node) {
        return !savedPositions[node.id()]
      })
      unsavedNodes.layout({
        name: 'cose',
        animate: true,
        animationDuration: 300,
        fit: false
      }).run()
    }
    cy.fit(undefined, 30)
  } else {
    // No saved positions, run full cose layout
    cy.layout({ name: 'cose', animate: true, animationDuration: 500 }).run()
  }

  // Save positions on node drag end
  cy.on('dragfree', 'node', function () {
    saveGraphPositions()
  })

  cy.on('tap', 'node', function (evt) {
    app.ports.cytoscapeNodeClicked.send(evt.target.id())
  })

  cy.on('tap', 'edge', function (evt) {
    app.ports.cytoscapeEdgeClicked.send(evt.target.id())
  })
}

if (app.ports.updateCytoscape) {
  app.ports.updateCytoscape.subscribe(function (elements) {
    if (!cy) return
    cy.json({ elements: elements })
    cy.layout({ name: 'cose', animate: true, animationDuration: 300 }).run()
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
