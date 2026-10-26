// Elm owns canonical reads and return receipts. This bridge only captures the
// actual result scroller and applies focus after the exact stamped DOM mounts.
export function installSearchNavigation(app, { document: doc = document, schedule = callback => requestAnimationFrame(callback) } = {}) {
  let scheduled = false, applied = null, captureToken = null, returnOwner = null, intent = 0, applying = false
  const userIntent = () => { if (!applying) intent++ }
  for (const type of ['pointerdown', 'keydown', 'wheel', 'focusin']) doc.addEventListener(type, userIntent, true)
  const current = () => {
    const node = doc.querySelector('[data-search-navigation]')
    try { return node ? { node, text: node.dataset.searchNavigation, value: JSON.parse(node.dataset.searchNavigation) } : null } catch { return null }
  }
  const scroll = () => doc.getElementById('main-content-scroll')
  const focusMounted = () => {
    scheduled = false
    const state = current(), scroller = scroll()
    if (!state || !scroller || !['target', 'return'].includes(state.value.phase) || !state.value.ready || !state.value.stamp) return
    const remountedReturn = state.value.phase === 'return' && returnOwner?.stamp === state.value.stamp && returnOwner.intent === intent
      && (!returnOwner.node.isConnected || returnOwner.node.id !== returnOwner.nodeId)
    if (state.value.stamp === applied && !remountedReturn) return
    const captured = state.text
    const target = doc.getElementById(state.value.target) || (state.value.phase === 'return' ? doc.querySelector('.search-input') : null)
    if (!target || !state.node.contains(target) && !target.matches('.search-input') || !target.isConnected) return
    // Recheck at the physical mutation boundary; a retired receipt cannot move
    // the viewport or focus even when an older render callback was queued.
    if (current()?.text !== captured) return
    applied = state.value.stamp
    if (state.value.phase === 'return') scroller.scrollTop = Math.max(0, Number(state.value.top) || 0)
    else {
      target.tabIndex = -1
      if (!target.hasAttribute('aria-label')) target.setAttribute('aria-label', state.value.label || 'Selected entity')
      const rect = target.getBoundingClientRect(), parent = scroller.getBoundingClientRect()
      const context=doc.querySelector('.workspace-context-persistent')?.getBoundingClientRect?.()
      const visibleTop=context?Math.max(parent.top,Math.min(parent.top+scroller.clientHeight,context.bottom)):parent.top
      scroller.scrollTop += rect.top - visibleTop - 12
    }
    applying = true
    try { target.focus({ preventScroll: true }) } finally { applying = false }
    // Revalidation may replace the pending-results DOM. Only this owned node's
    // remount can restore again, and any intervening user intent retires it.
    returnOwner = state.value.phase === 'return' ? { stamp: state.value.stamp, node: target, nodeId: target.id, intent } : null
  }
  const queue = () => { if (!scheduled) { scheduled = true; schedule(focusMounted) } }
  const observer = new MutationObserver(queue)
  observer.observe(doc.body, { childList: true, subtree: true, attributes: true, attributeFilter: ['data-search-navigation'] })
  app.ports.searchNavigationCommand.subscribe(command => {
    const state = current(), scroller = scroll(), result = doc.getElementById(command.resultId)
    if (!state || !scroller || state.value.guard !== command.guard || !result || !state.node.contains(result)) return
    captureToken = command.token
    const top = scroller.scrollTop
    schedule(() => {
      if (captureToken === command.token && current()?.value.guard === command.guard && result.isConnected) {
        app.ports.searchNavigationEvent.send({ token: command.token, guard: command.guard, top })
      }
    })
  })
  queue()
  return () => { observer.disconnect(); for (const type of ['pointerdown', 'keydown', 'wheel', 'focusin']) doc.removeEventListener(type, userIntent, true) }
}
