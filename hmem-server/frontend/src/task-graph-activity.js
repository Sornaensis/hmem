import { canonicalEndpoint } from './change-stream.js'

const identity = guard => JSON.stringify([guard?.workspaceId, guard?.projectId, guard?.audienceId, guard?.sessionEpoch, guard?.targetGeneration])
const nonempty = value => typeof value === 'string' && value.length > 0
const validGuard = guard => guard && ['workspaceId', 'projectId', 'audienceId'].every(key => nonempty(guard[key])) && ['sessionEpoch', 'targetGeneration'].every(key => Number.isSafeInteger(guard[key]) && guard[key] >= 0)
const validCursor = cursor => cursor == null || (nonempty(cursor.created_at) && nonempty(cursor.id))

export function activeSessionProof(page, workspaceId) {
  if (!page || !Array.isArray(page.items) || page.items.length > 1 || !validCursor(page.next_cursor)) throw Error('invalid_activity_page')
  if (!page.items.length) { if (page.next_cursor != null) throw Error('incomplete_activity_page'); return false }
  const row = page.items[0]
  if (!row || !nonempty(row.id) || row.workspace_id !== workspaceId || row.state !== 'active' || !nonempty(row.format_label)
    || !nonempty(row.created_at) || !nonempty(row.last_seen_at) || !['committed_bytes', 'committed_records', 'revision'].every(key => Number.isSafeInteger(row[key]) && row[key] >= 0)
    || typeof row.ends_with_newline !== 'boolean') throw Error('invalid_active_session')
  return true
}

// Logical owner/target retirement never releases physical occupancy. Even a
// fetch implementation ignoring abort holds its slot until the real callback.
export function createTaskGraphActivity({ apiUrl, isCurrent, onProof, onUnauthorized = () => {}, fetchImpl = globalThis.fetch,
  AbortControllerImpl = globalThis.AbortController, setTimer = setTimeout, clearTimer = clearTimeout, timeoutMs = 20000 }) {
  let current = null, wakeup = null
  const occupied = new Set()
  const active = entry => { try { return current === entry && isCurrent(entry.guard) } catch { return false } }
  const owns = (entry, record, receipt) => active(entry) && entry.records.get(record.id) === record && record.read === receipt && record.revision === receipt.revision && !receipt.expired
  const emit = (entry, record, value, failed = false) => { if (active(entry) && entry.records.get(record.id) === record) onProof(entry.guard, record.id, value, failed) }
  const stopWakeup = () => { if (wakeup != null) clearTimer(wakeup); wakeup = null }
  function abort(entry) { for (const receipt of occupied) if (receipt.entry === entry) receipt.controller?.abort() }
  function cancel() { const previous = current; current = null; stopWakeup(); if (previous) abort(previous) }
  function invalidate(entry, pause = false) {
    if (!entry || !active(entry)) return
    if (pause) entry.ready = false
    for (const record of entry.records.values()) { record.revision++; record.dirty = true; record.failed = false; emit(entry, record, null) }
    if (pause) { stopWakeup(); abort(entry) }
    else if (wakeup == null) { wakeup = setTimer(() => { wakeup = null; pump() }, 150); wakeup?.unref?.() }
  }
  async function read(entry, record) {
    const controller = typeof AbortControllerImpl === 'function' ? new AbortControllerImpl() : null
    const receipt = { entry, controller, revision: record.revision, expired: false, timer: null }
    occupied.add(receipt); record.read = receipt; record.dirty = false
    receipt.timer = setTimer(() => {
      receipt.timer = null
      if (owns(entry, record, receipt)) { record.failed = true; emit(entry, record, null, true) }
      receipt.expired = true; controller?.abort()
    }, timeoutMs)
    receipt.timer?.unref?.()
    try {
      const url = canonicalEndpoint(apiUrl, `/api/v1/workspaces/${encodeURIComponent(entry.guard.workspaceId)}/tasks/${encodeURIComponent(record.id)}/sessions`) + '?limit=1&state=active'
      const response = await fetchImpl(url, { credentials: 'include', headers: entry.headers, ...(controller ? { signal: controller.signal } : {}) })
      if (!owns(entry, record, receipt) || !entry.ready) return
      if (response.status === 401) { record.failed = true; emit(entry, record, null, true); onUnauthorized(entry.guard); return }
      if (!response.ok) throw Error('activity_read_failed')
      const page = await response.json()
      if (!owns(entry, record, receipt) || !entry.ready) return
      emit(entry, record, activeSessionProof(page, entry.guard.workspaceId))
    } catch {
      if (owns(entry, record, receipt) && entry.ready) { record.failed = true; emit(entry, record, null, true) }
    } finally {
      if (receipt.timer != null) clearTimer(receipt.timer)
      occupied.delete(receipt)
      if (record.read === receipt) record.read = null
      pump()
    }
  }
  function pump() {
    const entry = current
    if (!entry || !active(entry) || !entry.ready || wakeup != null) return
    for (const record of entry.records.values()) {
      if (occupied.size >= 2) break
      if (record.dirty && !record.read && !record.failed) read(entry, record)
    }
  }
  const matching = guard => current && identity(current.guard) === identity(guard) && active(current)
  return {
    load(guard, headers = {}) { cancel(); if (!validGuard(guard)) throw Error('invalid_activity_owner'); current = { guard: { ...guard }, headers: { ...headers }, records: new Map(), ready: false } },
    reconcile(guard, taskIds) {
      if (!matching(guard)) return
      const entry = current, ids = new Set(taskIds.filter(nonempty))
      for (const [id, record] of entry.records) if (!ids.has(id)) { entry.records.delete(id); record.read?.controller?.abort() }
      for (const id of ids) if (!entry.records.has(id)) entry.records.set(id, { id, revision: 0, dirty: true, failed: false, read: null })
      pump()
    },
    signal(guard, kind) {
      if (!matching(guard)) return
      if (kind === 'ready') { if (!current.ready) { current.ready = true; pump() } }
      else if (kind === 'invalidate') invalidate(current)
      else if (kind === 'pause') invalidate(current, true)
      else if (kind === 'retire') cancel()
    },
    retry(guard) { if (matching(guard)) { for (const record of current.records.values()) if (record.failed) { record.failed = false; record.dirty = true; record.revision++; emit(current, record, null) }; pump() } },
    cancel
  }
}
