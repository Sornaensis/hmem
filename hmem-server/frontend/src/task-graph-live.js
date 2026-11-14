import { createChangeStreamManager, FULL_SNAPSHOT_PROFILE, MAX_EVENT_IDS } from './change-stream.js'
import { createTaskGraphDataLoader } from './task-graph-data.js'

export const GRAPH_REFRESH_DELAY_MS = 150
export const GRAPH_MIN_REFRESH_INTERVAL_MS = 500
export const GRAPH_MAX_RESEEDS = 3
const validGuard = guard => guard && ['workspaceId', 'projectId', 'audienceId'].every(key => typeof guard[key] === 'string' && guard[key])
  && ['sessionEpoch', 'targetGeneration'].every(key => Number.isSafeInteger(guard[key]) && guard[key] >= 0)
const nonempty = value => typeof value === 'string' && value.length > 0
const nullableString = value => value === null || nonempty(value)
const entityTypes = new Set(['workspace', 'workspace_group', 'project', 'task', 'observation', 'task_dependency', 'workspace_group_membership', 'workspace_membership', 'agent_session', 'task_session', 'document', 'task_document'])
function validEnvelope(event, workspaceId) {
  return event?.schema_version === 1 && nonempty(event.event_id) && event.scope === 'workspace' && event.workspace_id === workspaceId
    && nonempty(event.occurred_at) && ['user', 'service', 'system'].includes(event.actor?.type) && nullableString(event.actor?.id)
    && (event.actor.label == null || typeof event.actor.label === 'string')
    && (event.actor.bot_identity == null || (nonempty(event.actor.bot_identity.role) && nonempty(event.actor.bot_identity['model-id'])))
    && nonempty(event.transaction?.id) && ['rest', 'mcp', 'audit_revert', 'core', 'migration'].includes(event.transaction?.cause)
    && nullableString(event.transaction?.request_id) && entityTypes.has(event.entity?.type) && nonempty(event.entity?.id)
    && (['created', 'updated', 'deleted', 'restored'].includes(event.entity?.action) || (event.entity.type === 'task_document' && event.entity.action === 'removed'))
    && Array.isArray(event.invalidations) && event.invalidations.length > 0
    && event.invalidations.every(value => nonempty(value?.kind) && nonempty(value?.target))
}

// This stream owns its snapshot boundary and private checkpoints. The ordinary
// workspace-shell stream is never an authority for the graph's projection.
export function createTaskGraphLive({ apiUrl, wsUrl, isCurrent, onSnapshot, onState = () => {},
  onUnauthorized = () => {}, onForbidden = () => {}, fetchImpl = globalThis.fetch,
  WebSocketImpl = globalThis.WebSocket, cryptoImpl = globalThis.crypto,
  AbortControllerImpl = globalThis.AbortController, setTimer = setTimeout, clearTimer = clearTimeout,
  now = () => performance.now(), streamFactory = createChangeStreamManager, loaderFactory = createTaskGraphDataLoader }) {
  if (typeof isCurrent !== 'function' || typeof onSnapshot !== 'function') throw new Error('graph context and snapshot consumers required')
  let current = null
  const active = entry => {
    try { return current === entry && isCurrent(entry.guard) } catch { return false }
  }
  const emit = (entry, kind, reason) => { if (active(entry)) onState(entry.guard, kind, reason) }
  const stopTimer = (entry, field) => { if (entry[field] != null) clearTimer(entry[field]); entry[field] = null }

  function dispose(entry) {
    stopTimer(entry, 'refreshTimer'); stopTimer(entry, 'reseedTimer')
    entry.loader.cancel(); entry.manager?.disconnectAll(); entry.storage.clear()
  }
  function cancel() { const entry = current; current = null; if (entry) dispose(entry) }
  function retire(entry, reason, authorization) {
    if (!active(entry)) return
    onState(entry.guard, 'retired', reason)
    cancel()
    if (authorization === 'unauthenticated') onUnauthorized(entry.guard)
    else if (authorization === 'forbidden') onForbidden(entry.guard)
  }
  function live(entry) {
    if (entry.seeded && entry.streamLive && !entry.reading && !entry.dirty && !entry.failed) emit(entry, 'live')
  }
  function failed(entry, reason, stream = false) {
    if (!active(entry)) return
    entry.failed = true
    stopTimer(entry, 'refreshTimer')
    if (stream) {
      entry.restartStream = true; entry.streamLive = false
      entry.readSerial++; entry.loader.cancel(); entry.reading = false; entry.manager?.disconnectAll(); entry.manager = null
    }
    emit(entry, 'failed', reason)
  }
  function refresh(entry) {
    if (!active(entry) || entry.failed || entry.reading || !entry.dirty) return
    stopTimer(entry, 'refreshTimer')
    entry.reading = true; entry.dirty = false; entry.lastRead = now()
    const serial = ++entry.readSerial
    emit(entry, 'refreshing')
    entry.loader.load(entry.guard, entry.headers).then(success => {
      if (!active(entry) || serial !== entry.readSerial) return
      entry.reading = false
      if (!success || entry.failed) return
      if (entry.dirty) schedule(entry)
      else live(entry)
    })
  }
  function schedule(entry) {
    if (!active(entry) || entry.failed || entry.reading || entry.refreshTimer != null) return
    const delay = Math.max(GRAPH_REFRESH_DELAY_MS, Math.min(GRAPH_MIN_REFRESH_INTERVAL_MS, GRAPH_MIN_REFRESH_INTERVAL_MS - (now() - entry.lastRead)))
    entry.refreshTimer = setTimer(() => { entry.refreshTimer = null; refresh(entry) }, delay)
    entry.refreshTimer?.unref?.()
  }
  function dirty(entry) {
    entry.dirty = true
    if (entry.failed) return
    emit(entry, 'refreshing')
    schedule(entry)
  }
  function reseed(entry) {
    if (!active(entry) || entry.reseedTimer != null) return
    entry.streamLive = false
    if (entry.reseeds >= GRAPH_MAX_RESEEDS) { failed(entry, 'Live graph recovery needs a retry.', true); return }
    entry.reseeds++
    entry.manager?.disconnectAll(); entry.manager = null; entry.readSerial++; entry.loader.cancel(); entry.reading = false
    stopTimer(entry, 'refreshTimer')
    entry.dirty = false; entry.storage.clear()
    emit(entry, 'recovering')
    entry.reseedTimer = setTimer(() => { entry.reseedTimer = null; if (active(entry)) connect(entry) }, GRAPH_MIN_REFRESH_INTERVAL_MS * entry.reseeds)
    entry.reseedTimer?.unref?.()
  }
  function connect(entry) {
    if (!active(entry)) return
    entry.failed = false; entry.streamLive = false; entry.seeded = false; entry.eventIds.clear()
    emit(entry, 'refreshing')
    const storage = { getItem: key => entry.storage.get(key) ?? null, setItem: (key, value) => entry.storage.set(key, value), removeItem: key => entry.storage.delete(key) }
    let manager
    const owned = () => active(entry) && entry.manager === manager
    manager = streamFactory({ apiUrl, wsUrl, fetchImpl, WebSocketImpl, cryptoImpl, AbortControllerImpl, setTimer, clearTimer, storage,
      onSnapshot(scope, workspaceId, snapshot) {
        if (!owned() || scope !== 'workspace' || workspaceId !== entry.guard.workspaceId || snapshot.snapshotProfile !== FULL_SNAPSHOT_PROFILE) return false
        if (onSnapshot(entry.guard, snapshot.items) === false) return false
        entry.seeded = true
        return owned()
      },
      onState(scope, workspaceId, state) {
        if (!owned() || scope !== 'workspace' || workspaceId !== entry.guard.workspaceId) return
        if (state === 'unauthenticated' || state === 'scope_forbidden') retire(entry, state, state === 'unauthenticated' ? 'unauthenticated' : 'forbidden')
        else if (state === 'resync_required' || state === 'invalid_ticket') reseed(entry)
        else if (['resyncing', 'connecting', 'replaying', 'reconnecting'].includes(state)) { entry.streamLive = false; if (!entry.failed) emit(entry, 'recovering') }
        else if (state !== 'control_closed') failed(entry, state, true)
      },
      onFrame(scope, workspaceId, raw) {
        if (!owned() || scope !== 'workspace' || workspaceId !== entry.guard.workspaceId) return false
        let frame
        try { frame = JSON.parse(raw) } catch { reseed(entry); return false }
        if (frame?.schema_version !== 1) { reseed(entry); return false }
        if (frame.type === 'access_revoked' && (frame.workspace_id == null || frame.workspace_id === workspaceId)) { retire(entry, 'scope_forbidden', 'forbidden'); return true }
        if (frame.type === 'resync_required') { reseed(entry); return true }
        if (frame.type === 'checkpoint') {
          if (frame.catch_up !== 'complete' || typeof frame.resume_token !== 'string' || !frame.resume_token) { reseed(entry); return false }
          entry.streamLive = true; entry.reseeds = 0; live(entry)
        } else if (frame.type === 'change') {
          const event = frame.event
          if (!validEnvelope(event, workspaceId)) { reseed(entry); return false }
          if (entry.eventIds.has(event.event_id)) return true
          entry.eventIds.add(event.event_id)
          if (entry.eventIds.size > MAX_EVENT_IDS) entry.eventIds.delete(entry.eventIds.values().next().value)
          if (['task', 'project', 'task_dependency', 'workspace'].includes(event.entity?.type)) dirty(entry)
        } else if (!['access_revoked', 'access_granted', 'session_activity_changed'].includes(frame.type)) { reseed(entry); return false }
        return true
      }
    })
    entry.manager = manager
    // No resumeToken: this manager must seed from its own complete boundary.
    manager.connect({ audienceId: entry.guard.audienceId, scope: 'workspace', workspaceId: entry.guard.workspaceId,
      snapshotProfile: FULL_SNAPSHOT_PROFILE, headers: entry.headers })
  }

  return {
    load(guard, headers = {}) {
      cancel()
      if (!validGuard(guard)) throw new Error('invalid graph guard')
      const entry = { guard: { ...guard }, headers: { ...headers }, manager: null, storage: new Map(),
        eventIds: new Set(), seeded: false, streamLive: false, dirty: false, reading: false, failed: false,
        restartStream: false, refreshTimer: null, reseedTimer: null, lastRead: -Infinity, reseeds: 0, readSerial: 0 }
      entry.loader = loaderFactory({ apiUrl, fetchImpl, cryptoImpl, AbortControllerImpl, setTimer, clearTimer,
        isCurrent: () => active(entry),
        onSnapshot: (_, items) => active(entry) && onSnapshot(entry.guard, items) !== false,
        onState: (_, state, reason) => {
          if (!active(entry) || state !== 'failed') return
          if (reason === 'unauthenticated' || reason === 'scope_forbidden') retire(entry, reason, reason === 'unauthenticated' ? 'unauthenticated' : 'forbidden')
          else failed(entry, reason)
        } })
      current = entry
      connect(entry)
    },
    retry() {
      const entry = current
      if (!entry || !active(entry)) return
      entry.failed = false
      if (entry.restartStream || entry.reseedTimer != null || !entry.seeded) {
        // Retry during recovery owns a new seed, never a competing finite read.
        // Fence both a queued reseed and an already in-flight manager snapshot.
        stopTimer(entry, 'reseedTimer'); stopTimer(entry, 'refreshTimer')
        entry.readSerial++; entry.loader.cancel(); entry.reading = false
        entry.manager?.disconnectAll(); entry.manager = null
        entry.restartStream = false; entry.reseeds = 0; entry.dirty = false; entry.storage.clear(); emit(entry, 'recovering'); connect(entry)
      } else { entry.dirty = true; refresh(entry) }
    },
    retire() { const entry = current; if (entry) { onState(entry.guard, 'retired', 'authority_changed'); cancel() } },
    cancel
  }
}
