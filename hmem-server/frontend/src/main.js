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

function currentAuthToken() {
  return runtimeConfig.authToken || localStorage.getItem(authTokenStorageKey) || null
}

function authHeaderObject() {
  const token = currentAuthToken()
  return token ? { Authorization: `Bearer ${token}` } : {}
}

const apiUrl = normalizeBaseUrl(configuredApiUrl())
const wsUrl = configuredWsUrl(apiUrl)
let lastUnauthorizedNotificationAt = 0
let notifyUnauthorized = function () {}

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
    if (isApiRequestUrl(this.__hmemRequestUrl)) {
      const token = currentAuthToken()
      const hasAuthorization = this.__hmemHeaders && this.__hmemHeaders.authorization
      if (token && !hasAuthorization) {
        originalSetRequestHeader.call(this, 'Authorization', `Bearer ${token}`)
      }

      this.addEventListener('loadend', function () {
        if (this.status === 401) notifyUnauthorized()
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
    authTokenPresent: currentAuthToken() !== null,
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

// ---------------------------------------------------------------------------
// WebSocket port
// ---------------------------------------------------------------------------

let ws = null
let reconnectTimer = null
let connectGeneration = 0

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

async function resolveWsUrl(url) {
  const workspaceId = currentWorkspaceId()
  if (!workspaceId) return url

  const response = await fetch(apiResourceUrl('/api/v1/ws-ticket'), {
    method: 'POST',
    headers: { 'Content-Type': 'application/json', ...authHeaderObject() },
    credentials: 'include',
    body: JSON.stringify({ workspace_id: workspaceId })
  })

  if (!response.ok) {
    if (response.status === 401) notifyUnauthorized()
    throw new Error(`ws-ticket-failed:${response.status}`)
  }

  const body = await response.json()
  if (!body || !body.ticket) throw new Error('ws-ticket-failed:missing-ticket')
  return wsUrlWithTicket(url, body.ticket)
}

function connectWs(url) {
  connectGeneration += 1
  const generation = connectGeneration
  if (ws) { ws.close() }
  if (reconnectTimer) { clearTimeout(reconnectTimer); reconnectTimer = null }

  resolveWsUrl(url).then(function (resolvedUrl) {
    if (generation !== connectGeneration) return

    const socket = new WebSocket(resolvedUrl)
    ws = socket

    socket.onopen = function () {
      if (generation !== connectGeneration || ws !== socket) return
      app.ports.wsConnected.send(null)
    }

    socket.onmessage = function (event) {
      if (generation !== connectGeneration || ws !== socket) return
      app.ports.wsMessage.send(event.data)
    }

    socket.onclose = function () {
      if (generation !== connectGeneration || ws !== socket) return
      app.ports.wsDisconnected.send(null)
      ws = null
      // Auto-reconnect after 3 seconds. Resolve a fresh ticket each time.
      reconnectTimer = setTimeout(function () {
        connectWs(url)
      }, 3000)
    }

    socket.onerror = function () {
      // onclose will fire after onerror
    }
  }).catch(function (err) {
    if (generation !== connectGeneration) return
    app.ports.wsDisconnected.send(null)
    if (err && String(err.message || '').startsWith('ws-ticket-failed:')) return
    reconnectTimer = setTimeout(function () {
      connectWs(url)
    }, 3000)
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
