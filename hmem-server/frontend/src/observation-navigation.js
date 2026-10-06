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
  let toolbar = null, toolbarFrame = null
  let toolbarActivation = null, nativeIntent = 0
  const toolbarPort = app.ports.focusObservationToolbar
  const viewport = options.viewport
  const cancelToolbar = () => { if (toolbarFrame !== null) cancel(toolbarFrame); toolbarFrame = null; toolbar = null }

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
    if (toolbarActivation && records.some(record => [...record.removedNodes].some(node => node === toolbarActivation.panel || node.contains?.(toolbarActivation.panel)))) toolbarActivation = null
    if (toolbar && records.some(record => [...record.removedNodes].some(node => node === toolbar.panel || node.contains?.(toolbar.panel)))) cancelToolbar()
    if (origin && records.some(record => [...record.removedNodes].some(node =>
      node === origin.panel || node.contains?.(origin.panel)
      || node === origin.scroller || node.contains?.(origin.scroller)))) {
      origin = null
      if (frame !== null) cancel(frame)
      frame = null
      viewport?.cancelNavigation()
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
    const target = event.target?.closest?.('#observation-close-file-composer')
    if (target) toolbarActivation = { id: target.id, panel: panel(), context: currentStamp(panel()), nativeIntent }
    const card = event.target?.closest('.observation-card')
    if (!card) return
    const root = panel()
    origin = capture(card, root, doc.getElementById('main-content-scroll'), currentStamp(root))
  }

  function navigate(command) {
    cancelToolbar(); toolbarActivation = null; viewport?.cancelNavigation()
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
    const finish = owned => {
      frame = null
      const root = panel()
      const scroller = doc.getElementById('main-content-scroll')
      if (!scroller || root !== before || scroller !== scroll || !sameStamp(currentStamp(root), command.destination)) {
        origin = null
        return
      }
      if (viewport && !viewport.claimNavigation(owned)) {
        viewport.awaitNavigation(owned, scheduleNavigation)
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
    }
    const scheduleNavigation = owned => { frame = raf(() => {
      frame = null
      if (panel() !== before || doc.getElementById('main-content-scroll') !== scroll
          || !sameStamp(currentStamp(before), command.destination)) { origin = null; return }
      // One bounded receipt/paint turn lets returned-width geometry restore
      // under its new stamp before the exact physical origin is focused.
      if ((command.intent === 'return' || owned.viewport) && doc.getElementById('observation-viewport')) {
        const viewport = doc.getElementById('observation-viewport')
        const lifetime = viewport.dataset.observationViewportContext
        frame = raf(() => {
          const current = doc.getElementById('observation-viewport')
          let oldStamp, newStamp
          try { oldStamp = JSON.parse(lifetime); newStamp = JSON.parse(current?.dataset.observationViewportContext || 'null') } catch {}
          if (current !== viewport || !oldStamp || !newStamp
              || !['workspace', 'epoch', 'generation'].every(key => oldStamp[key] === newStamp[key])
              || current.dataset.observationLayoutReady !== 'true') { origin = null }
          finish(owned)
        })
      } else finish(owned)
    }) }
    if (viewport && command.viewport) viewport.awaitNavigation(command, scheduleNavigation)
    else scheduleNavigation(command)
  }
  function focusToolbar(command) {
    cancelToolbar()
    const root = panel(), context = currentStamp(root)
    const current = doc.activeElement
    const activation = toolbarActivation
    toolbarActivation = null
    if (!validStamp(command?.context) || !sameStamp(context, command.context)
        || typeof command.targetId !== 'string' || typeof command.originId !== 'string'
        || !activation || activation.id !== command.originId || activation.panel !== root
        || !sameStamp(activation.context, command.context) || activation.nativeIntent !== nativeIntent
        || (current && current !== doc.body && current.id !== command.originId && current.id !== command.targetId)) return
    const intent = { ...command, panel: root }
    toolbar = intent
    const finish = () => {
      toolbarFrame = null
      if (toolbar !== intent) return
      const actualViewport = doc.getElementById('observation-viewport')
      let actual
      try { actual = JSON.parse(actualViewport?.dataset.observationViewportContext || 'null') } catch {}
      const active = doc.activeElement, target = doc.getElementById(intent.targetId)
      if (panel() !== root || !sameStamp(currentStamp(root), intent.context)
          || !actual || !['workspace', 'epoch', 'generation'].every(key => actual[key] === intent.viewport?.[key])
          || (active && active !== doc.body && active.id !== intent.originId && active.id !== intent.targetId)
          || !visible(target, root)) { cancelToolbar(); return }
      toolbar = null
      target.focus({ preventScroll: true }); target.scrollIntoView({ block: 'nearest', behavior: 'instant' })
    }
    toolbarFrame = raf(() => { toolbarFrame = raf(finish) })
  }
  function newerToolbarIntent(event) {
    nativeIntent++
    if (toolbarActivation && event.target?.id !== toolbarActivation.id) toolbarActivation = null
    if (toolbar && event.target?.id !== toolbar.originId && event.target?.id !== toolbar.targetId) cancelToolbar()
  }
  doc.addEventListener('click', captureActivation, true)
  observer?.observe(doc.documentElement, { childList: true, subtree: true })
  port.subscribe(navigate)
  if (toolbarPort) {
    toolbarPort.subscribe(focusToolbar)
    for (const name of ['focusin', 'keydown', 'pointerdown']) doc.addEventListener(name, newerToolbarIntent, true)
  }
  return {
    dispose() {
      if (frame !== null) cancel(frame)
      frame = null
      origin = null
      cancelToolbar(); viewport?.cancelNavigation()
      toolbarActivation = null
      doc.removeEventListener('click', captureActivation, true)
      observer?.disconnect()
      port.unsubscribe?.(navigate)
      toolbarPort?.unsubscribe?.(focusToolbar)
      if (toolbarPort) for (const name of ['focusin', 'keydown', 'pointerdown']) doc.removeEventListener(name, newerToolbarIntent, true)
    }
  }
}
