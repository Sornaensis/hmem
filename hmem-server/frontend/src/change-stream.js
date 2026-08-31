/* Canonical change-stream browser boundary.  This module deliberately owns
 * bearer persistence and socket lifecycle so Elm never sees a ticket, resume
 * token, or a cursor.  It is dependency-free to make its security-sensitive
 * behaviour executable under node:test as well as in the browser. */
export const STORAGE_VERSION = 1
export const MAX_EVENT_IDS = 128
export const FULL_SNAPSHOT_PROFILE = 'full_v1'
export const WORKSPACE_SHELL_SNAPSHOT_PROFILE = 'workspace_shell_v1'
const MAX_RETRIES = 6
const SNAPSHOT_KINDS = new Set(['workspace', 'workspace_group', 'project', 'task', 'task_dependency', 'observation'])

export function opaqueAudienceKey(audienceId) {
  if (typeof audienceId !== 'string' || !audienceId) throw new Error('audience id required')
  const seeds = [0x811c9dc5, 0x9e3779b9, 0x85ebca6b, 0xc2b2ae35]
  return seeds.map(seed => {
    let hash = seed
    for (let index = 0; index < audienceId.length; index += 1) {
      hash = Math.imul(hash ^ audienceId.charCodeAt(index), 0x01000193)
    }
    return (hash >>> 0).toString(16).padStart(8, '0')
  }).join('')
}

export function scopeStorageKey(audienceId, scope, workspaceId) {
  return `hmem.change-stream.v${STORAGE_VERSION}.audience.${opaqueAudienceKey(audienceId)}.${scope === 'workspace' ? `workspace.${workspaceId}` : 'global'}`
}

export function startIdempotencyKey(cryptoImpl = globalThis.crypto) {
  const bytes = new Uint8Array(24)
  if (!cryptoImpl || typeof cryptoImpl.getRandomValues !== 'function') throw new Error('csprng-unavailable')
  cryptoImpl.getRandomValues(bytes)
  return Array.from(bytes, byte => byte.toString(16).padStart(2, '0')).join('')
}

export function readCheckpoint(storage, audienceId, scope, workspaceId) {
  try {
    const raw = storage.getItem(scopeStorageKey(audienceId, scope, workspaceId))
    if (!raw) return null
    const value = JSON.parse(raw)
    if (!value || value.version !== STORAGE_VERSION || typeof value.resumeToken !== 'string' || !value.resumeToken || !Array.isArray(value.eventIds) || value.eventIds.some(id => typeof id !== 'string')) return null
    return { resumeToken: value.resumeToken, eventIds: value.eventIds.slice(-MAX_EVENT_IDS) }
  } catch (_) { return null }
}

export function writeCheckpoint(storage, audienceId, scope, workspaceId, resumeToken, eventIds) {
  if (typeof resumeToken !== 'string' || !resumeToken) return false
  try {
    storage.setItem(scopeStorageKey(audienceId, scope, workspaceId), JSON.stringify({ version: STORAGE_VERSION, resumeToken, eventIds: Array.from(new Set(eventIds)).slice(-MAX_EVENT_IDS) }))
    return true
  } catch (_) { return false }
}

export function clearCheckpoint(storage, audienceId, scope, workspaceId) {
  try { storage.removeItem(scopeStorageKey(audienceId, scope, workspaceId)); return true } catch (_) { return false }
}

export function canonicalTicketBody(scope, workspaceId, resumeToken) {
  const scopeValue = scope === 'global' ? { scope: 'global' } : { scope: 'workspace', workspace_id: workspaceId }
  return { scope: scopeValue, resume_token: resumeToken }
}

export function canonicalEndpoint(apiUrl, path) {
  const url = new URL(apiUrl)
  const prefix = url.pathname.replace(/\/+$/, '')
  url.pathname = `${prefix}/${String(path).replace(/^\/+/, '')}`.replace(/\/{2,}/g, '/')
  url.search = ''
  url.hash = ''
  return url.toString()
}

export function createCanonicalFrameBatcher({ onBatch, onIncomplete = () => {}, setTimer = setTimeout, clearTimer = clearTimeout, incompleteDelay = 1000 }) {
  const batches = new Map()
  const failedScopes = new Set()
  const scopeKey = (scope, workspaceId) => scope === 'global' ? 'global' : `workspace:${workspaceId}`

  function flushKey(key) {
    const pending = batches.get(key)
    if (!pending) return
    if (pending.timer != null) clearTimer(pending.timer)
    batches.delete(key)
    onBatch(pending.scope, pending.workspaceId, pending.frames)
  }

  function clear(scope, workspaceId) {
    const key = scopeKey(scope, workspaceId)
    const pending = batches.get(key)
    if (pending && pending.timer != null) clearTimer(pending.timer)
    batches.delete(key)
    failedScopes.delete(key)
  }

  return {
    queue(scope, workspaceId, frame) {
      const key = scopeKey(scope, workspaceId)
      if (failedScopes.has(key)) return
      const pending = batches.get(key) || { scope, workspaceId, frames: [], timer: null }
      pending.frames.push(frame)
      if (pending.timer != null) clearTimer(pending.timer)
      batches.set(key, pending)
      const closesDelivery = frame && frame.schema_version === 1 && ['checkpoint', 'access_granted', 'access_revoked', 'resync_required'].includes(frame.type)
      if (closesDelivery) {
        pending.timer = null
        flushKey(key)
      } else {
        pending.timer = setTimer(() => {
          if (batches.get(key) !== pending) return
          batches.delete(key)
          failedScopes.add(key)
          onIncomplete(scope, workspaceId)
        }, incompleteDelay)
      }
    },
    clear,
    clearAll() {
      for (const pending of batches.values()) if (pending.timer != null) clearTimer(pending.timer)
      batches.clear()
      failedScopes.clear()
    }
  }
}

export function createChangeStreamManager({ fetchImpl = fetch, WebSocketImpl = WebSocket, storage = localStorage, cryptoImpl = globalThis.crypto, AbortControllerImpl = globalThis.AbortController, setTimer = setTimeout, clearTimer = clearTimeout, onState = () => {}, onFrame = () => {}, onSnapshot = () => {}, apiUrl = window.location.origin, wsUrl }) {
  const sockets = new Map()
  const endpoint = path => canonicalEndpoint(apiUrl, path)
  const socketUrl = (ticket) => { const url = new URL(wsUrl, apiUrl); url.searchParams.set('ticket', ticket); return url.toString() }

  function key(scope, workspaceId) { return scope === 'global' ? 'global' : `workspace:${workspaceId}` }
  function isCurrent(entry, generation) { return sockets.get(key(entry.scope, entry.workspaceId)) === entry && entry.generation === generation }
  function cancelled() { const error = new Error('cancelled'); error.cancelled = true; return error }
  function stop(entry) {
    entry.generation += 1
    if (entry.timer != null) clearTimer(entry.timer)
    entry.timer = null
    if (entry.retryDelayTimer != null) clearTimer(entry.retryDelayTimer)
    entry.retryDelayTimer = null
    const rejectDelay = entry.retryDelayReject
    entry.retryDelayReject = null
    if (rejectDelay) rejectDelay()
    if (entry.abortController) { entry.abortController.abort(); entry.abortController = null }
    if (entry.socket) { entry.socket.close(); entry.socket = null }
  }
  async function guardedFetch(entry, generation, url, options) {
    if (!isCurrent(entry, generation)) throw cancelled()
    const controller = typeof AbortControllerImpl === 'function' ? new AbortControllerImpl() : null
    if (controller) entry.abortController = controller
    try {
      const response = await fetchImpl(url, { ...options, ...(controller ? { signal: controller.signal } : {}) })
      if (!isCurrent(entry, generation)) throw cancelled()
      return response
    } catch (error) {
      if (!isCurrent(entry, generation) || (error && error.name === 'AbortError')) throw cancelled()
      throw error
    } finally {
      if (entry.abortController === controller) entry.abortController = null
    }
  }
  function throwForAuthorization(status) {
    if (status === 401) throw new Error('unauthenticated')
    if (status === 403) throw new Error('scope_forbidden')
  }
  function retryAfterMilliseconds(response) {
    if (!response || !response.headers || typeof response.headers.get !== 'function') return null
    const raw = response.headers.get('Retry-After')
    if (typeof raw !== 'string' || !raw.trim()) return null
    const seconds = Number(raw)
    if (Number.isFinite(seconds) && seconds >= 0) return Math.min(30000, Math.round(seconds * 1000))
    const date = Date.parse(raw)
    return Number.isFinite(date) ? Math.min(30000, Math.max(0, date - Date.now())) : null
  }
  function retryDelayMilliseconds(attempt, response) {
    return retryAfterMilliseconds(response) ?? Math.min(5000, 100 * 2 ** attempt)
  }
  function abortableDelay(entry, generation, milliseconds) {
    if (!isCurrent(entry, generation)) return Promise.reject(cancelled())
    return new Promise((resolve, reject) => {
      let settled = false
      const finish = callback => {
        if (settled) return
        settled = true
        entry.retryDelayTimer = null
        entry.retryDelayReject = null
        callback()
      }
      const timer = setTimer(() => finish(() => isCurrent(entry, generation) ? resolve() : reject(cancelled())), milliseconds)
      entry.retryDelayTimer = timer
      entry.retryDelayReject = () => finish(() => reject(cancelled()))
    })
  }
  async function obtainTicket(entry, generation) {
    const response = await guardedFetch(entry, generation, endpoint('/api/v1/change-stream/ticket'), {
      method: 'POST', credentials: 'include', headers: { 'Content-Type': 'application/json', ...(entry.headers || {}) },
      body: JSON.stringify(canonicalTicketBody(entry.scope, entry.workspaceId, entry.resumeToken))
    })
    if (response.status === 409) throw new Error('resync_required')
    throwForAuthorization(response.status)
    if (!response.ok) throw new Error(`ticket_${response.status}`)
    const body = await response.json()
    if (!isCurrent(entry, generation)) throw cancelled()
    if (!body || typeof body.ticket !== 'string' || !body.ticket) throw new Error('invalid_ticket')
    return body.ticket
  }
  function validSnapshotItem(entry, item) {
    if (!item || item.schema_version !== 1 || !SNAPSHOT_KINDS.has(item.kind) || !item.data || typeof item.data !== 'object' || Array.isArray(item.data)) return false
    const allowed = entry.snapshotProfile === WORKSPACE_SHELL_SNAPSHOT_PROFILE
      ? entry.scope === 'workspace' && item.kind === 'workspace'
      : entry.scope === 'global'
      ? item.kind === 'workspace' || item.kind === 'workspace_group'
      : ['workspace', 'project', 'task', 'task_dependency', 'observation'].includes(item.kind)
    if (!allowed) return false
    if (item.kind === 'task_dependency') return typeof item.data.task_id === 'string' && item.data.task_id.length > 0 && typeof item.data.depends_on_id === 'string' && item.data.depends_on_id.length > 0
    if (typeof item.data.id !== 'string' || !item.data.id) return false
    if (entry.scope !== 'workspace') return true
    if (item.kind === 'workspace') return item.data.id === entry.workspaceId
    return item.data.workspace_id === entry.workspaceId
  }
  async function resync(entry, generation) {
    const startKey = startIdempotencyKey(cryptoImpl)
    let pageToken = null
    let resumeToken = null
    const items = []
    const pageTokens = new Set()
    // Pages are intentionally consumed before any socket/ticket is created.
    // Elm's ordinary REST bootstrap remains visible until its canonical
    // replacement is wired through the snapshot port, but this preserves the
    // server's atomic snapshot/replay hand-off at the transport boundary.
    for (let attempts = 0; attempts < 256; attempts += 1) {
      const body = pageToken ? { scope: canonicalTicketBody(entry.scope, entry.workspaceId, 'x').scope, page_token: pageToken } : { scope: canonicalTicketBody(entry.scope, entry.workspaceId, 'x').scope, snapshot_profile: entry.snapshotProfile, page_size: 100, start_idempotency_key: startKey }
      const response = await postResyncPage(entry, generation, body)
      if (response.status === 409) throw new Error('resync_required')
      throwForAuthorization(response.status)
      if (!response.ok) throw new Error(`resync_${response.status}`)
      const page = await response.json()
      if (!isCurrent(entry, generation)) throw cancelled()
      if (!page || page.snapshot_profile !== entry.snapshotProfile || !Array.isArray(page.items) || page.items.some(item => !validSnapshotItem(entry, item)) || typeof page.has_more !== 'boolean') throw new Error('invalid_snapshot')
      if (page.has_more) {
        if (typeof page.next_page_token !== 'string' || !page.next_page_token || pageTokens.has(page.next_page_token) || page.resume_token != null) throw new Error('invalid_snapshot')
        items.push(...page.items)
        pageTokens.add(page.next_page_token)
        pageToken = page.next_page_token
        continue
      }
      if (typeof page.resume_token !== 'string' || !page.resume_token || page.next_page_token != null) throw new Error('invalid_snapshot')
      items.push(...page.items)
      resumeToken = page.resume_token
      break
    }
    if (!resumeToken) throw new Error('resync_exhausted')
    if (!isCurrent(entry, generation)) throw cancelled()
    entry.resumeToken = resumeToken
    // The Elm port delivery is synchronous.  Emitting the fully accumulated
    // snapshot before requesting a ticket prevents replay from racing a
    // partial model replacement.
    onSnapshot(entry.scope, entry.workspaceId, { items, resumeToken, snapshotProfile: entry.snapshotProfile })
    if (!isCurrent(entry, generation)) throw cancelled()
    writeCheckpoint(storage, entry.audienceId, entry.scope, entry.workspaceId, resumeToken, [])
  }
  async function postResyncPage(entry, generation, body) {
    let lastError = null
    const requestBody = JSON.stringify(body)
    for (let attempt = 0; attempt < MAX_RETRIES; attempt += 1) {
      let retryResponse = null
      try {
        const response = await guardedFetch(entry, generation, endpoint('/api/v1/change-stream/resync'), { method: 'POST', credentials: 'include', headers: { 'Content-Type': 'application/json', ...(entry.headers || {}) }, body: requestBody })
        if (response.status === 409 || response.status === 401 || response.status === 403 || response.ok || (response.status < 500 && response.status !== 408 && response.status !== 429)) return response
        lastError = new Error(`resync_${response.status}`)
        retryResponse = response
      } catch (error) {
        if (error && error.cancelled) throw error
        lastError = error
      }
      if (attempt + 1 < MAX_RETRIES) await abortableDelay(entry, generation, retryDelayMilliseconds(attempt, retryResponse))
    }
    throw (lastError || new Error('resync_retry_exhausted'))
  }
  async function open(entry, generation) {
    try {
      const ticket = await obtainTicket(entry, generation)
      if (!isCurrent(entry, generation)) return
      const socket = new WebSocketImpl(socketUrl(ticket)); entry.socket = socket
      socket.onopen = () => { if (isCurrent(entry, generation)) onState(entry.scope, entry.workspaceId, 'replaying') }
      socket.onmessage = event => {
        if (!isCurrent(entry, generation)) return
        try {
          const frame = JSON.parse(event.data)
          if (frame && frame.schema_version === 1 && frame.type === 'checkpoint' && typeof frame.resume_token === 'string' && frame.catch_up === 'complete') {
            entry.resumeToken = frame.resume_token; entry.attempt = 0
            writeCheckpoint(storage, entry.audienceId, entry.scope, entry.workspaceId, entry.resumeToken, entry.eventIds || [])
          }
          if (frame && frame.schema_version === 1 && frame.type === 'change' && frame.event && typeof frame.event.event_id === 'string') entry.eventIds = Array.from(new Set([...(entry.eventIds || []), frame.event.event_id])).slice(-MAX_EVENT_IDS)
          const revokesThisWorkspace = frame && frame.schema_version === 1 && frame.type === 'access_revoked' && entry.scope === 'workspace' && (!frame.workspace_id || frame.workspace_id === entry.workspaceId)
          if (revokesThisWorkspace) {
            entry.closedByControl = true
            clearCheckpoint(storage, entry.audienceId, entry.scope, entry.workspaceId)
          }
          if (frame && frame.schema_version === 1 && frame.type === 'resync_required') {
            entry.closedByControl = true
            clearCheckpoint(storage, entry.audienceId, entry.scope, entry.workspaceId)
          }
        } catch (_) { /* Elm will fail closed and request a resync. */ }
        onFrame(entry.scope, entry.workspaceId, event.data)
      }
      socket.onclose = () => {
        if (!isCurrent(entry, generation)) return
        entry.socket = null
        if (entry.closedByControl) {
          onState(entry.scope, entry.workspaceId, 'control_closed')
          return
        }
        if (entry.attempt >= MAX_RETRIES) { onState(entry.scope, entry.workspaceId, 'retry_exhausted'); return }
        entry.timer = setTimer(() => open(entry, generation), Math.min(30000, 500 * 2 ** Math.min(entry.attempt++, 6)))
      }
    } catch (error) {
      if (!isCurrent(entry, generation) || (error && error.cancelled)) return
      const reason = error && error.message ? error.message : 'transport_failure'
      if (reason === 'resync_required' || reason === 'unauthenticated' || reason === 'scope_forbidden' || reason === 'invalid_ticket') { onState(entry.scope, entry.workspaceId, reason); return }
      if (entry.attempt >= MAX_RETRIES) { onState(entry.scope, entry.workspaceId, 'retry_exhausted'); return }
      entry.timer = setTimer(() => open(entry, generation), Math.min(30000, 500 * 2 ** Math.min(entry.attempt++, 6)))
    }
  }
  return {
    connect({ audienceId, scope, workspaceId = null, resumeToken, headers = {}, snapshotProfile }) {
      if (typeof audienceId !== 'string' || !audienceId) throw new Error('audience id required')
      if (scope !== 'global' && (!workspaceId || typeof workspaceId !== 'string')) throw new Error('workspace scope requires workspaceId')
      const entryKey = key(scope, workspaceId)
      const old = sockets.get(entryKey)
      if (old && old.audienceId === audienceId) {
        old.headers = headers
        return
      }
      const storageValue = readCheckpoint(storage, audienceId, scope, workspaceId)
      // A new manager has a new Elm model, so a stored token cannot replace the
      // canonical snapshot that seeds that model.  Explicit resumeToken is only
      // safe for a caller that retained its corresponding projections.
      const resolvedProfile = snapshotProfile || (scope === 'workspace' ? WORKSPACE_SHELL_SNAPSHOT_PROFILE : FULL_SNAPSHOT_PROFILE)
      if ((scope === 'global' && resolvedProfile !== FULL_SNAPSHOT_PROFILE) || ![FULL_SNAPSHOT_PROFILE, WORKSPACE_SHELL_SNAPSHOT_PROFILE].includes(resolvedProfile)) throw new Error('invalid snapshot profile')
      const entry = { audienceId, scope, workspaceId, resumeToken: resumeToken || null, eventIds: resumeToken && storageValue ? storageValue.eventIds : [], headers, snapshotProfile: resolvedProfile, generation: 0, attempt: 0, socket: null, timer: null, abortController: null, closedByControl: false }
      if (!entry.resumeToken) { onState(scope, workspaceId, 'resyncing') }
      if (old) stop(old)
      sockets.set(entryKey, entry); const generation = ++entry.generation
      const begin = entry.resumeToken ? Promise.resolve() : resync(entry, generation)
      begin.then(() => { if (isCurrent(entry, generation)) { onState(scope, workspaceId, 'connecting'); open(entry, generation) } }).catch(error => {
        if (!isCurrent(entry, generation) || (error && error.cancelled)) return
        onState(scope, workspaceId, error && error.message ? error.message : 'resync_required')
      })
    },
    checkpoint(scope, workspaceId, resumeToken, eventIds = []) {
      const entry = sockets.get(key(scope, workspaceId)); if (!entry) return false
      entry.resumeToken = resumeToken; entry.attempt = 0
      return writeCheckpoint(storage, entry.audienceId, scope, workspaceId, resumeToken, eventIds)
    },
    disconnect(scope, workspaceId) { const entryKey = key(scope, workspaceId); const entry = sockets.get(entryKey); if (entry) { stop(entry); sockets.delete(entryKey) } },
    disconnectAll() { for (const [entryKey, entry] of sockets) { stop(entry); sockets.delete(entryKey) } },
    clear(scope, workspaceId, audienceId) {
      const entry = sockets.get(key(scope, workspaceId))
      const scopedAudienceId = entry ? entry.audienceId : audienceId
      return typeof scopedAudienceId === 'string' && scopedAudienceId
        ? clearCheckpoint(storage, scopedAudienceId, scope, workspaceId)
        : false
    },
    newStartKey() { return startIdempotencyKey(cryptoImpl) }
  }
}
