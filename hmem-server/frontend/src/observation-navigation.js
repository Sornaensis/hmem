// Observation selection has one physical origin, including repeated match cards.
// Never recover focus across a different workspace, session, or DOM lifetime.
export function installObservationNavigation(app, options = {}) {
  const port = app.ports.navigateObservationDetail
  if (!port) return { dispose() {} }
  const doc = options.document || document
  const win = options.window || window
  const raf = options.requestAnimationFrame || win.requestAnimationFrame.bind(win)
  const cancel = options.cancelAnimationFrame || win.cancelAnimationFrame.bind(win)
  const Observer = options.MutationObserver || win.MutationObserver
  let frame = null
  let origin = null

  const validStamp = value => value && typeof value.workspaceId === 'string'
    && Number.isInteger(value.sessionEpoch) && Number.isInteger(value.token)
    && (value.selectedId === null || typeof value.selectedId === 'string')
  const sameContext = (a, b) => a && b && a.workspaceId === b.workspaceId && a.sessionEpoch === b.sessionEpoch
  const sameStamp = (a, b) => sameContext(a, b) && a.token === b.token && a.selectedId === b.selectedId
  const panel = () => doc.getElementById('observation-panel')
  const currentStamp = root => {
    try { return JSON.parse(root?.dataset.observationContext || 'null') } catch { return null }
  }
  const visible = (element, root) => {
    if (!element || !root?.contains(element) || element.closest('[inert]') || !element.getClientRects().length) return false
    const style = win.getComputedStyle(element)
    return style.display !== 'none' && style.visibility !== 'hidden'
  }

  const observer = Observer && new Observer(records => {
    if (origin && records.some(record => [...record.removedNodes].some(node =>
      node === origin.panel || node.contains?.(origin.panel)
      || node === origin.scroller || node.contains?.(origin.scroller)))) {
      origin = null
      if (frame !== null) cancel(frame)
      frame = null
    }
  })

  function capture(card, root, scroll, context) {
    const rect = card?.getBoundingClientRect()
    const viewport = scroll?.getBoundingClientRect()
    return scroll && validStamp(context) && visible(card, root) && card.classList.contains('observation-card')
        && rect.bottom > viewport.top && rect.top < viewport.bottom
      ? { id: card.id, top: scroll.scrollTop, context, panel: root, scroller: scroll }
      : null
  }

  // Elm may paint before delivering its port. Native click capture, including
  // keyboard-generated clicks, observes the activation's old DOM lifetime.
  function captureActivation(event) {
    const card = event.target?.closest('.observation-card')
    if (!card) return
    const root = panel()
    origin = capture(card, root, doc.getElementById('main-content-scroll'), currentStamp(root))
  }

  function navigate(command) {
    if (frame !== null) cancel(frame)
    frame = null
    if (!validStamp(command?.previous) || !validStamp(command?.destination)
        || !sameContext(command.previous, command.destination)
        || command.destination.token !== command.previous.token + 1
        || !['detail', 'return'].includes(command.intent)) {
      origin = null
      return
    }
    const before = panel()
    const scroll = doc.getElementById('main-content-scroll')
    const observed = currentStamp(before)
    if (!scroll || !(sameStamp(observed, command.previous) || sameStamp(observed, command.destination))) {
      origin = null
      return
    }
    if (command.intent === 'detail') {
      if (origin?.id !== command.originId || !sameStamp(origin?.context, command.previous)) {
        origin = sameStamp(observed, command.previous)
          ? capture(command.originId && doc.getElementById(command.originId), before, scroll, command.previous)
          : null
      }
      if (origin) origin.context = command.destination
    } else if (!sameStamp(origin?.context, command.previous) || origin?.id !== command.originId
        || origin?.panel !== before || origin?.scroller !== scroll) {
      origin = null
    }
    frame = raf(() => {
      frame = null
      const root = panel()
      const scroller = doc.getElementById('main-content-scroll')
      if (!scroller || root !== before || scroller !== scroll || !sameStamp(currentStamp(root), command.destination)) {
        origin = null
        return
      }
      if (command.intent === 'detail') {
        const heading = doc.getElementById('observation-detail-heading')
        if (visible(heading, root)) {
          heading.focus({ preventScroll: true })
          heading.scrollIntoView({ block: 'start', behavior: 'instant' })
        }
      } else {
        const card = origin && doc.getElementById(origin.id)
        if (visible(card, root) && card.classList.contains('observation-card')) {
          card.focus({ preventScroll: true })
          scroller.scrollTop = origin.top
          const rect = card.getBoundingClientRect()
          const viewport = scroller.getBoundingClientRect()
          // A canonical save may reorder the row. Preserve the physical origin
          // where possible, then reveal the same card if its layout has moved.
          if (rect.bottom <= viewport.top || rect.top >= viewport.bottom) {
            card.scrollIntoView({ block: 'nearest', behavior: 'instant' })
          }
        } else {
          const results = doc.getElementById('observation-results')
          if (visible(results, root)) {
            results.focus({ preventScroll: true })
            results.scrollIntoView({ block: 'start', behavior: 'instant' })
          }
        }
        origin = null
      }
    })
  }
  doc.addEventListener('click', captureActivation, true)
  observer?.observe(doc.documentElement, { childList: true, subtree: true })
  port.subscribe(navigate)
  return {
    dispose() {
      if (frame !== null) cancel(frame)
      frame = null
      origin = null
      doc.removeEventListener('click', captureActivation, true)
      observer?.disconnect()
      port.unsubscribe?.(navigate)
    }
  }
}
