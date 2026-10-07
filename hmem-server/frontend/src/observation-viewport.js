// Independent mounted-only Observation geometry. Membership stays in Elm.
export function installObservationViewport(app, options = {}) {
  const inbound = app.ports.onObservationViewport, outbound = app.ports.syncObservationViewport
  if (!inbound || !outbound) return { dispose() {} }
  const doc = options.document || document, win = options.window || window
  const raf = options.requestAnimationFrame || win.requestAnimationFrame.bind(win)
  const cancel = options.cancelAnimationFrame || win.cancelAnimationFrame.bind(win)
  const RO = options.ResizeObserver || win.ResizeObserver, MO = options.MutationObserver || win.MutationObserver
  let root = null, scroller = null, stamp = null, keys = [], pending = null, nativeOwner = null, anchorTop = null, adjustment = 0, frame = null, last = ''
  const observed = new Set()
  let layoutEpoch = 0
  let restoringNative = false
  let nativeFrame = null, settledStamp = null, settlement = 0
  let navigationToken = null, revealOwner = null, pendingNavigation = null
  const same = (a, b) => a && b && ['workspace', 'epoch', 'generation', 'revision'].every(key => a[key] === b[key])
  const sameLifetime = (a, b) => a && b && ['workspace', 'epoch', 'generation'].every(key => a[key] === b[key])
  const readStamp = element => { try { return JSON.parse(element?.dataset.observationViewportContext || 'null') } catch { return null } }
  const mounted = () => root ? [...root.querySelectorAll('[data-observation-key]')] : []
  const controls = row => [...row.querySelectorAll('button,a[href],input,textarea,select,summary,[tabindex]')].filter(el =>
    !el.disabled && el.tabIndex >= 0 && !el.closest('[hidden],[inert]') && el.getClientRects().length)
  const focusControls = controls
  const detailAnchor = () => doc.querySelector('[data-observation-detail-anchor]')
  const controlIdentity = control => JSON.stringify([control.tagName, control.id, control.type, control.className, control.getAttribute?.('role')])
  const rowFor = key => mounted().find(row => row.dataset.observationKey === key)
  const navigation = () => root?.closest('#observation-panel')?.dataset.observationContext || null
  const navigationContext = () => { try { return JSON.parse(navigation()) } catch { return null } }
  const currentReveal = () => revealOwner && revealOwner.root === root && sameLifetime(revealOwner.stamp, stamp)
    && revealOwner.navigation === navigation() && same(stamp, readStamp(root))
  const origin = () => root.getBoundingClientRect().top - scroller.getBoundingClientRect().top + scroller.scrollTop
  const resize = RO ? new RO(() => schedule()) : null
  function schedule() { if (frame === null) frame = raf(flush) }
  function cancelNativePaint() { if (nativeFrame !== null) cancel(nativeFrame); nativeFrame = null }
  function detach() {
    if (scroller) scroller.removeEventListener('scroll', schedule)
    resize?.disconnect(); observed.clear()
    root = scroller = stamp = null; keys = []; pending = nativeOwner = null; anchorTop = null; adjustment = 0; last = ''
    cancelNativePaint(); settledStamp = null; revealOwner = null; navigationToken = null; pendingNavigation = null
  }
  function receipt(target = null, focusChange = false) {
    if (!root || !scroller || !same(stamp, readStamp(root)) || root.closest('[inert]')) return null
    const rows = mounted(), focusRow = doc.activeElement?.closest('[data-observation-key]')
    const style = win.getComputedStyle?.(root)
    const layout = JSON.stringify([win.innerWidth, win.innerHeight, style?.fontSize, style?.lineHeight, style?.fontFamily, style?.letterSpacing, layoutEpoch])
    return { stamp, navigationToken: navigationContext()?.token, detailMounted: !!detailAnchor(), layout, top: Math.max(0, scroller.scrollTop - origin()), height: scroller.clientHeight, width: root.clientWidth
      , measurements: rows.map(row => ({ key: row.dataset.observationKey, height: row.getBoundingClientRect().height }))
      , focus: root.contains(focusRow) ? focusRow.dataset.observationKey : (doc.activeElement?.id === 'observation-results' && root.closest('#observation-results') === doc.activeElement ? '@results' : (focusChange ? '@outside' : null)), target }
  }
  function send(target = null, focusChange = false) {
    const value = receipt(target, focusChange)
    if (!value || value.height <= 0 || value.width <= 0) return
    const serialized = JSON.stringify(value)
    if (target || serialized !== last) { last = serialized; settledStamp = null; inbound.send(value) }
  }
  function flush() {
    frame = null
    const next = doc.getElementById('observation-viewport'), scroll = doc.getElementById('main-content-scroll')
    if (!next || !scroll) { detach(); return }
    if (root !== next || scroller !== scroll) {
      const retainedStamp = stamp, retainedKeys = keys, retainedPending = pending, retainedTop = anchorTop, retainedAdjustment = adjustment, retainedNavigation = navigationToken
      detach(); root = next; scroller = scroll
      // A painted command may arrive before its first container is mounted.
      if (same(retainedStamp, readStamp(root))) { stamp = retainedStamp; keys = retainedKeys; pending = retainedPending; anchorTop = retainedTop; adjustment = retainedAdjustment; navigationToken = retainedNavigation }
      scroller.addEventListener('scroll', schedule, { passive: true }); resize?.observe(scroller)
    }
    if (!same(stamp, readStamp(root)) || navigationContext()?.token !== navigationToken) return
    if (!currentReveal()) {
      revealOwner = null
      if (anchorTop !== null) scroller.scrollTop = Math.max(0, origin() + anchorTop)
      else if (adjustment) scroller.scrollTop = Math.max(0, scroller.scrollTop + adjustment)
    }
    anchorTop = null
    adjustment = 0
    const rows = mounted(), current = new Set(rows)
    for (const row of observed) if (!current.has(row)) { resize?.unobserve(row); observed.delete(row) }
    for (const row of rows) if (!observed.has(row)) { observed.add(row); resize?.observe(row) }
    if (pending) {
      const row = rowFor(pending.key)
      if (row) {
        const target = pending; pending = null
        const listOrigin = origin()
        scroller.scrollTop = Math.max(0, listOrigin + target.offset)
        const tabbable = controls(row)
        const control = target.edge === 'last' ? tabbable.at(-1) : tabbable[0]
        control?.focus({ preventScroll: true })
        row.scrollIntoView({ block: 'nearest', behavior: 'instant' })
      }
    }
    send(); settleNativeOwner(); completeNavigation()
    // Inline detail shares the measured list. Preserve an owned reveal through
    // accepted reflow only while its original target still owns native focus.
    if (currentReveal() && same(settledStamp, stamp) && doc.activeElement?.id === revealOwner.targetId) {
      const target = doc.getElementById(revealOwner.targetId)
      const bounds = target?.getBoundingClientRect?.(), host = scroller.getBoundingClientRect()
      if (bounds && (bounds.top < host.top || bounds.bottom > host.bottom)) target.scrollIntoView?.({ block: 'nearest', behavior: 'instant' })
    }
  }
  function completeNavigation() {
    const waiting = pendingNavigation, current = navigationContext()
    if (!waiting) return
    if (waiting.root !== root || !sameLifetime(waiting.command.viewport, stamp)
        || !current || current.workspaceId !== waiting.command.destination.workspaceId || current.sessionEpoch !== waiting.command.destination.sessionEpoch
        || current.token > waiting.command.destination.token) { pendingNavigation = null; return }
    if (current.token !== waiting.command.destination.token || current.selectedId !== waiting.command.destination.selectedId
        || !same(stamp, readStamp(root)) || !same(settledStamp, stamp) || pending || anchorTop !== null || adjustment) return
    pendingNavigation = null
    waiting.ready({ ...waiting.command, viewport: stamp })
  }
  function settleNativeOwner() {
    const owner = nativeOwner
    if (!owner) return
    if (owner.root !== root || owner.navigation !== navigation() || !sameLifetime(owner.stamp, stamp)) {
      nativeOwner = null; cancelNativePaint(); return
    }
    if (!same(settledStamp, stamp) || settlement <= owner.afterSettlement || !same(stamp, readStamp(root))
        || root.dataset.observationFocusKey !== owner.key || pending || anchorTop !== null || adjustment) return
    const row = rowFor(owner.key), control = row && focusControls(row)[owner.control]
    if (!row) return
    if (control !== owner.node || controlIdentity(control) !== owner.identity) { nativeOwner = null; cancelNativePaint(); return }
    // Only the model's acknowledged successor layout may transfer this native
    // owner. A second paint verifies that corrections and mount work are done.
    owner.stamp = stamp
    if (nativeFrame !== null) return
    nativeFrame = raf(() => {
      nativeFrame = null
      if (nativeOwner !== owner || owner.root !== root || owner.navigation !== navigation() || !same(owner.stamp, stamp)
          || !same(settledStamp, stamp) || !same(stamp, readStamp(root)) || root.dataset.observationFocusKey !== owner.key
          || pending || anchorTop !== null || adjustment) return
      const current = rowFor(owner.key), target = current && focusControls(current)[owner.control]
      if (target !== owner.node || controlIdentity(target) !== owner.identity) { nativeOwner = null; return }
      if (doc.activeElement && doc.activeElement !== doc.body && doc.activeElement !== target) { nativeOwner = null; return }
      // Recovery keeps this exact active control owned. A later accepted keyed
      // movement can blur it again without emitting a new user focus event.
      owner.afterSettlement = settlement
      if (!doc.activeElement || doc.activeElement === doc.body) {
        restoringNative = true
        try { target.focus({ preventScroll: true }) } finally { restoringNative = false }
      }
    })
  }
  function sync(command) {
    const next = command?.stamp
    if (!next || typeof next.workspace !== 'string' || !Number.isInteger(next.epoch)
      || typeof next.generation !== 'string' || !Number.isInteger(next.revision) || !Number.isInteger(command.navigationToken)) return
    if (sameLifetime(stamp, next) && (next.revision < stamp.revision || (navigationToken !== null && command.navigationToken < navigationToken))) return
    if (!same(stamp, next)) {
      last = ''; pending = null; adjustment = 0; anchorTop = null; cancelNativePaint()
      if (nativeOwner && (!sameLifetime(nativeOwner.stamp, next) || next.revision < nativeOwner.stamp.revision)) nativeOwner = null
    }
    if (navigationToken !== command.navigationToken) { anchorTop = null; adjustment = 0; pending = null; revealOwner = null; nativeOwner = null; cancelNativePaint() }
    stamp = next; navigationToken = command.navigationToken
    if (command.settled === true && !command.target && !command.adjustment && command.top === null) {
      settledStamp = next; settlement++
    } else {
      settledStamp = null; cancelNativePaint()
      // One new receipt is required after an accepted geometry change, even
      // if its DOM measurements serialize identically to the initiating read.
      if (command.settled === false) last = ''
    }
    if (Array.isArray(command.keys)) keys = command.keys.slice()
    if (Number.isFinite(command.top)) anchorTop = Math.max(0, command.top)
    if (Number.isFinite(command.adjustment)) adjustment += command.adjustment
    if (command.target && keys.includes(command.target.key) && ['first', 'last', 'return'].includes(command.target.edge)
      && Number.isFinite(command.target.offset)) pending = { ...command.target }
    schedule()
  }
  function keyboard(event) {
    userIntent(event)
    if (event.key !== 'Tab' || event.ctrlKey || event.altKey || event.metaKey || !root || !same(stamp, readStamp(root))) return
    const row = event.target?.closest('[data-observation-key]')
    if (!root.contains(row)) return
    const tabbable = controls(row), backwards = event.shiftKey
    if (event.target !== (backwards ? tabbable[0] : tabbable.at(-1))) return
    const position = keys.indexOf(row.dataset.observationKey)
    let nextPosition = position + (backwards ? -1 : 1)
    const next = keys[nextPosition]
    if (position < 0 || !next || rowFor(next)) return
    event.preventDefault(); send({ key: next, edge: backwards ? 'last' : 'first' })
  }
  function nativeFocus() {
    // Capture the currently painted focused row before a queued measurement
    // can evict it. The receipt retains the exact DOM/model stamp guard.
    if (restoringNative) { send(null, true); schedule(); return }
    if (pendingNavigation && doc.activeElement?.id !== (pendingNavigation.command.intent === 'detail' ? detailAnchor()?.id : pendingNavigation.command.originId)) pendingNavigation = null
    if (revealOwner && doc.activeElement?.id !== revealOwner.targetId) revealOwner = null
    cancelNativePaint()
    const row = doc.activeElement?.closest('[data-observation-key]')
    const control = root?.contains(row) ? focusControls(row).indexOf(doc.activeElement) : -1
    nativeOwner = control >= 0 && same(stamp, readStamp(root)) ? { root, stamp, navigation: navigation(), afterSettlement: settlement,
      key: row.dataset.observationKey, control, node: doc.activeElement, identity: controlIdentity(doc.activeElement) } : null
    send(null, true); schedule()
  }
  const mutation = MO ? new MO(() => schedule()) : null
  const invalidateLayout = () => { layoutEpoch++; schedule() }
  function userIntent(event) {
    revealOwner = null; pendingNavigation = null; anchorTop = null; adjustment = 0
    if (nativeOwner && ['pointerdown', 'touchstart', 'keydown'].includes(event?.type)
        && event.target !== nativeOwner.node && !nativeOwner.node.contains?.(event.target)) {
      nativeOwner = null; cancelNativePaint()
    }
  }
  for (const event of ['wheel', 'touchstart', 'pointerdown']) doc.addEventListener(event, userIntent, true)
  mutation?.observe(doc.body, { subtree: true, childList: true, attributes: true, attributeFilter: ['data-observation-viewport-context', 'data-observation-focus-key', 'data-observation-context'] })
  doc.addEventListener('keydown', keyboard, true); doc.addEventListener('focusin', nativeFocus, true)
  win.addEventListener('resize', invalidateLayout); doc.fonts?.addEventListener('loadingdone', invalidateLayout); outbound.subscribe(sync)
  schedule()
  return {
    awaitNavigation(command, ready) {
      pendingNavigation = { root, command, ready }
      schedule()
    },
    cancelNavigation() { pendingNavigation = null },
    retainNavigationFocus(target) {
      // Expanding the already focused arrow does not emit a second focusin.
      // Capture its current rendered identity only after an owned reveal.
      if (currentReveal() && doc.activeElement === target && target?.id === revealOwner.targetId) nativeFocus()
    },
    claimNavigation(command) {
      const current = navigationContext(), expected = command?.destination
      if (!current || !expected || !['workspaceId', 'sessionEpoch', 'token', 'selectedId'].every(key => current[key] === expected[key])
          || !same(stamp, command.viewport) || !same(stamp, readStamp(root)) || !same(settledStamp, stamp)) return false
      anchorTop = null; adjustment = 0; pending = null; nativeOwner = null; cancelNativePaint()
      revealOwner = { root, stamp, navigation: navigation(), targetId: command.intent === 'detail' ? detailAnchor()?.id : (command.originId || 'observation-results') }
      return true
    },
    dispose() {
    if (frame !== null) cancel(frame)
    frame = null; detach(); mutation?.disconnect()
    doc.removeEventListener('keydown', keyboard, true); doc.removeEventListener('focusin', nativeFocus, true)
    for (const event of ['wheel', 'touchstart', 'pointerdown']) doc.removeEventListener(event, userIntent, true)
    win.removeEventListener('resize', invalidateLayout); doc.fonts?.removeEventListener('loadingdone', invalidateLayout); outbound.unsubscribe?.(sync)
  } }
}
