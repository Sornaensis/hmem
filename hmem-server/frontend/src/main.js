import { Elm } from './Main.elm'
import cytoscape from 'cytoscape'

function createSessionId() {
  if (window.crypto && window.crypto.randomUUID) {
    return window.crypto.randomUUID()
  }

  return `session-${Date.now()}-${Math.random().toString(36).slice(2)}`
}

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

// Initialize Elm application
const app = Elm.Main.init({
  node: document.getElementById('app'),
  flags: {
    apiUrl: window.location.origin,
    wsUrl: `${window.location.protocol === 'https:' ? 'wss:' : 'ws:'}//${window.location.host}/api/v1/ws`,
    sessionId: createSessionId(),
    storedFilters: getWorkspaceFilters()
  }
})

// ---------------------------------------------------------------------------
// WebSocket port
// ---------------------------------------------------------------------------

let ws = null
let reconnectTimer = null

function connectWs(url) {
  if (ws) { ws.close() }
  if (reconnectTimer) { clearTimeout(reconnectTimer); reconnectTimer = null }

  ws = new WebSocket(url)

  ws.onopen = function () {
    app.ports.wsConnected.send(null)
  }

  ws.onmessage = function (event) {
    app.ports.wsMessage.send(event.data)
  }

  ws.onclose = function () {
    app.ports.wsDisconnected.send(null)
    ws = null
    // Auto-reconnect after 3 seconds
    reconnectTimer = setTimeout(function () {
      connectWs(url)
    }, 3000)
  }

  ws.onerror = function () {
    // onclose will fire after onerror
  }
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
