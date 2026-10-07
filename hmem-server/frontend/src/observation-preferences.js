const MAX_BYTES = 32768
const sameOwner = (a, b) => a && b && ['workspaceId', 'actorId', 'authority', 'runtimeId', 'epoch', 'requestId', 'touch'].every(k => a[k] === b[k])
const validOwner = o => o && /^[0-9a-f]{8}(-[0-9a-f]{4}){3}-[0-9a-f]{12}$/i.test(o.workspaceId)
  && ['actorId', 'authority', 'runtimeId'].every(k => typeof o[k] === 'string' && o[k].length > 0 && o[k].length <= 256)
  && ['epoch', 'requestId', 'touch'].every(k => Number.isSafeInteger(o[k]) && o[k] >= 0)
const keyFor = o => `hmem.observation-ui.v1:${encodeURIComponent(o.workspaceId)}:${encodeURIComponent(o.actorId)}:${encodeURIComponent(o.authority)}`
const bytes = text => new TextEncoder().encode(text).length

// The trusted Elm session owns admission. Every asynchronous read retains its
// complete owner; a replacement/retirement invalidates the scheduled response.
export function installObservationPreferences(app, { storage, schedule = callback => requestAnimationFrame(callback) } = {}) {
  let owner = null
  app.ports.observationPreferenceCommand.subscribe(command => {
    if (command.operation === 'retire') { owner = null; return }
    if (!validOwner(command.owner)) return
    if (command.operation === 'read') {
      owner = { ...command.owner }
      const captured = owner
      let value = null, available = true
      try { const text = (storage || globalThis.localStorage).getItem(keyFor(owner)); if (text !== null && bytes(text) <= MAX_BYTES) value = JSON.parse(text) } catch { available = false }
      schedule(() => { if (sameOwner(owner, captured)) app.ports.observationPreferencesReceived.send({ owner: captured, value, available }) })
    } else if (command.operation === 'write' && sameOwner(owner, command.owner)) {
      try {
        const value = { ...command.value, subjects: [...(command.value?.subjects || [])], groups: [...(command.value?.groups || [])] }
        if (!value || value.version !== 1 || value.workspaceId !== owner.workspaceId || value.actorId !== owner.actorId || value.authority !== owner.authority) return
        let text = JSON.stringify(value)
        // Sorted Elm preferences have deterministic eviction; retain detail first.
        while (bytes(text) > MAX_BYTES && (value.groups.length || value.subjects.length)) {
          if (value.groups.length) value.groups.pop(); else value.subjects.pop()
          text = JSON.stringify(value)
        }
        if (bytes(text) <= MAX_BYTES) (storage || globalThis.localStorage).setItem(keyFor(owner), text)
      } catch { /* UI remains usable if storage is disabled or full. */ }
    }
  })
}
