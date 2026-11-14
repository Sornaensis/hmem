import { canonicalEndpoint, FULL_SNAPSHOT_PROFILE, startIdempotencyKey, STREAM_PHASE_TIMEOUT_MS, validSnapshotItem } from './change-stream.js'

const validGuard = guard => guard && ['workspaceId', 'projectId', 'audienceId'].every(key => typeof guard[key] === 'string' && guard[key])
  && ['sessionEpoch', 'targetGeneration'].every(key => Number.isSafeInteger(guard[key]) && guard[key] >= 0)
const cancelled = () => Object.assign(new Error('cancelled'), { cancelled: true })

/** Isolated full snapshots: no socket, primary checkpoint, or partial publication.
 * isCurrent must check the caller's current workspace/session/audience/target.
 * onSnapshot receives only completed items; its consumer validates the full Elm
 * snapshot and graph projection before making a graph ready.
 */
export function createTaskGraphDataLoader({ apiUrl, isCurrent, onSnapshot, onState = () => {}, fetchImpl = globalThis.fetch,
  cryptoImpl = globalThis.crypto, AbortControllerImpl = globalThis.AbortController, setTimer = setTimeout,
  clearTimer = clearTimeout, timeoutMs = STREAM_PHASE_TIMEOUT_MS }) {
  if (typeof isCurrent !== 'function' || typeof onSnapshot !== 'function') throw new Error('graph context and snapshot consumers required')
  const endpoint = canonicalEndpoint(apiUrl, '/api/v1/change-stream/resync')
  let current = null
  const active = entry => current === entry && isCurrent(entry.guard)

  function cancel() {
    const previous = current
    current = null
    previous?.cancelRequest?.()
  }

  function request(entry, body) {
    if (!active(entry)) return Promise.reject(cancelled())
    const controller = typeof AbortControllerImpl === 'function' ? new AbortControllerImpl() : null
    return new Promise((resolve, reject) => {
      let settled = false
      let timer = null
      const finish = (error, value) => {
        if (settled) return
        settled = true
        if (timer != null) clearTimer(timer)
        entry.cancelRequest = null
        if (error) reject(error)
        else resolve(value)
      }
      entry.cancelRequest = () => { controller?.abort(); finish(cancelled()) }
      timer = setTimer(() => { controller?.abort(); finish(new Error('transport_timeout')) }, timeoutMs)
      timer?.unref?.()
      ;(async () => {
        try {
          const response = await fetchImpl(endpoint, { method: 'POST', credentials: 'include',
            headers: { ...entry.headers, 'Content-Type': 'application/json' }, body: JSON.stringify(body),
            ...(controller ? { signal: controller.signal } : {}) })
          if (!active(entry) || settled) throw cancelled()
          const page = response.ok ? await response.json() : null
          if (!active(entry)) throw cancelled()
          finish(null, { response, page })
        } catch (error) { finish(!active(entry) ? cancelled() : error) }
      })()
    })
  }

  async function hydrate(entry) {
    const scope = { scope: 'workspace', workspace_id: entry.guard.workspaceId }
    const startKey = startIdempotencyKey(cryptoImpl)
    const context = { scope: 'workspace', workspaceId: entry.guard.workspaceId, snapshotProfile: FULL_SNAPSHOT_PROFILE }
    const items = []
    const identities = new Set()
    const pageTokens = new Set()
    let pageToken = null
    for (;;) {
      const body = pageToken ? { scope, page_token: pageToken }
        : { scope, snapshot_profile: FULL_SNAPSHOT_PROFILE, page_size: 100, start_idempotency_key: startKey }
      const { response, page } = await request(entry, body)
      if (response.status === 401) throw new Error('unauthenticated')
      if (response.status === 403) throw new Error('scope_forbidden')
      if (response.status === 409) throw new Error('resync_required')
      if (!response.ok) throw new Error(`snapshot_${response.status}`)
      if (!page || page.snapshot_profile !== FULL_SNAPSHOT_PROFILE || !Array.isArray(page.items)
        || page.items.some(item => !validSnapshotItem(context, item)) || typeof page.has_more !== 'boolean') throw new Error('invalid_snapshot')
      for (const item of page.items) {
        const identity = JSON.stringify(item.kind === 'task_dependency'
          ? [item.kind, item.data.task_id, item.data.depends_on_id] : [item.kind, item.data.id])
        if (identities.has(identity)) throw new Error('invalid_snapshot')
        identities.add(identity)
      }
      items.push(...page.items)
      if (page.has_more) {
        if (!page.items.length || typeof page.next_page_token !== 'string' || !page.next_page_token
          || pageTokens.has(page.next_page_token) || page.resume_token != null) throw new Error('invalid_snapshot')
        pageTokens.add(page.next_page_token)
        pageToken = page.next_page_token
      } else {
        if (typeof page.resume_token !== 'string' || !page.resume_token || page.next_page_token != null) throw new Error('invalid_snapshot')
        break
      }
    }
    if (!active(entry)) throw cancelled()
    if (onSnapshot(entry.guard, items) === false) throw new Error('snapshot_delivery_failed')
    if (!active(entry)) throw cancelled()
    onState(entry.guard, 'complete')
  }

  return {
    load(guard, headers = {}) {
      cancel()
      if (!validGuard(guard)) return Promise.reject(new Error('invalid graph guard'))
      const entry = { guard: { ...guard }, headers: { ...headers }, cancelRequest: null }
      current = entry
      onState(entry.guard, 'loading')
      return hydrate(entry).then(() => true, error => {
        if (active(entry) && !error?.cancelled) onState(entry.guard, 'failed', error?.message || 'transport_failure')
        return false
      })
    },
    cancel
  }
}
