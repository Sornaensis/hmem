// Elm owns per-history-entry context. This bridge captures and restores only
// actual DOM geometry/focus under the same workspace/session/audience stamp.
export function installTaskGraphNavigation(app, { document: doc = document, window: win = window, schedule = callback => requestAnimationFrame(callback),
  now = () => performance.now(), setTimer = (callback, delay) => win.setTimeout(callback, delay), clearTimer = timer => win.clearTimeout(timer) } = {}) {
  let queued = false, applied = null, pending = null, intent = 0, applying = false, deadline = null, disposed = false
  const clearPending = () => { if (deadline !== null) clearTimer(deadline); deadline = null; pending = null }
  const userIntent = () => { if (!applying) { intent++; if (pending) queue() } }
  const sameScope = (a, b) => !!a && !!b && a.workspace === b.workspace && a.epoch === b.epoch && a.audience === b.audience
  const current = () => {
    const node = doc.querySelector('[data-task-graph-navigation]')
    try { return node ? { node, text: node.dataset.taskGraphNavigation, value: JSON.parse(node.dataset.taskGraphNavigation) } : null } catch { return null }
  }
  const send = value => app.ports.taskGraphNavigationEvent.send(value)
  const finish = (state, receipt) => send({ kind: 'restored', id: receipt.id, token: receipt.token, scope: state.value.scope })
  function flush() {
    queued = false
    if (disposed) return
    const state = current(), receipt = state?.value.return
    if (!state || !receipt || !state.value.scope) { clearPending(); applied = null; return }
    const key = JSON.stringify([state.value.scope, receipt.id, receipt.token])
    if (key === applied) return
    if (!pending || pending.key !== key) {
      clearPending()
      pending = { key, intent, expires: now() + 5000 }
      // A failed load or missing target may never mutate the DOM again.
      deadline = setTimer(() => { deadline = null; queue() }, 5000)
    }
    if (intent !== pending.intent || now() >= pending.expires) { applied = key; clearPending(); finish(state, receipt); return }
    if (!state.value.ready) return
    const scroll = doc.getElementById('main-content-scroll'), card = doc.getElementById('entity-' + receipt.project)
    const control = doc.getElementById('task-graph-open-' + receipt.project)
    if (!scroll || !card || !control || !state.node.contains(card) || !card.getClientRects().length) return
    // A newer render may supersede this scheduled callback before mutation.
    if (current()?.text !== state.text) return
    const before = scroll.scrollTop
    const delta = Number.isFinite(receipt.anchor) ? card.getBoundingClientRect().top - scroll.getBoundingClientRect().top - receipt.anchor : 0
    const stable = Math.abs(delta) < 0.5 && (Number.isFinite(receipt.anchor) || Math.abs(before - receipt.top) < 0.5)
    const layout = state.node.querySelector('#hierarchy-viewport')?.dataset.hierarchyContext
    pending.stable = stable && pending.layout === layout ? (pending.stable || 0) + 1 : 0
    pending.layout = layout
    if (pending.stable >= 3) { applied = key; clearPending(); finish(state, receipt); return }
    applying = true
    try {
      if (!stable) {
        scroll.scrollTop = Math.max(0, Number(receipt.top) || 0)
        if (Number.isFinite(receipt.anchor)) scroll.scrollTop += card.getBoundingClientRect().top - scroll.getBoundingClientRect().top - receipt.anchor
      }
      control.focus({ preventScroll: true })
    } finally { applying = false }
    // Existing hierarchy measurement/layout receipts may arrive on the next
    // paint. Retain the origin until its current rows and anchor settle.
    queue()
  }
  const queue = () => { if (!disposed && !queued) { queued = true; schedule(flush) } }
  for (const kind of ['pointerdown', 'keydown', 'wheel']) doc.addEventListener(kind, userIntent, true)
  const observer = new win.MutationObserver(queue)
  observer.observe(doc.body, { subtree: true, childList: true, attributes: true, attributeFilter: ['data-task-graph-navigation'] })
  const capture = command => {
    const state = current(), scroll = doc.getElementById('main-content-scroll'), card = doc.getElementById('entity-' + command.project)
    if (!state || !sameScope(state.value.scope, command.scope) || state.value.url !== command.url || win.location.href !== command.url
      || !scroll || !card || !state.node.contains(card) || !card.getClientRects().length) return
    const viewport = state.node.querySelector('#hierarchy-viewport')
    const rows = viewport ? Array.from(viewport.querySelectorAll('[data-hierarchy-key]')).slice(0, 64)
      .map(row => [row.dataset.hierarchyKey, row.getBoundingClientRect().height]) : []
    send({ kind: 'captured', id: command.id, scope: command.scope, top: scroll.scrollTop, rows,
      viewportTop: viewport ? scroll.getBoundingClientRect().top - viewport.getBoundingClientRect().top : 0,
      anchor: card.getBoundingClientRect().top - scroll.getBoundingClientRect().top })
  }
  app.ports.taskGraphNavigationCommand.subscribe(capture)
  queue()
  return () => { disposed = true; clearPending(); observer.disconnect(); app.ports.taskGraphNavigationCommand.unsubscribe?.(capture); for (const kind of ['pointerdown', 'keydown', 'wheel']) doc.removeEventListener(kind, userIntent, true) }
}
