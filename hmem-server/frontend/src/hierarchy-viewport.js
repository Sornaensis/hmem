// One observer set tracks only mounted rows; no whole-cache DOM or measurement walk.
export function installHierarchyViewport(app, options = {}) {
  const doc = options.document || document
  const win = options.window || window
  const raf = options.requestAnimationFrame || win.requestAnimationFrame.bind(win)
  const cancel = options.cancelAnimationFrame || win.cancelAnimationFrame.bind(win)
  const RO = options.ResizeObserver || win.ResizeObserver
  const MO = options.MutationObserver || win.MutationObserver
  let frame = null
  let container = null
  let scroller = null
  let context = null
  let lifetime = null
  let pendingLayout = null
  let pendingTarget = null
  let pendingElement = null
  let lastReceipt = ''
  const observed = new Set()
  const heights = new Map()
  const observer = RO ? new RO(() => schedule()) : null
  const mounted = () => container ? Array.from(container.querySelectorAll('[data-hierarchy-key]')) : []
  const rowFor = (key) => mounted().find(row => row.dataset.hierarchyKey === key)
  const sameStamp = (a, b) => a && b && ['workspace', 'epoch', 'generation', 'revision'].every(key => a[key] === b[key])
  const sameLifetime = (a, b) => a && b && ['workspace', 'epoch', 'generation'].every(key => a[key] === b[key])
  const origin = () => container.getBoundingClientRect().top - scroller.getBoundingClientRect().top + scroller.scrollTop
  function tabbable(row) {
    return Array.from(row.querySelectorAll('a[href],button,input,textarea,select,[tabindex],[contenteditable="true"]')).filter(control => {
      if (control.disabled || control.tabIndex < 0 || control.closest('[hidden],[inert]')) return false
      const style = win.getComputedStyle(control)
      return style.display !== 'none' && style.visibility !== 'hidden' && style.visibility !== 'collapse' && control.getClientRects().length > 0
    })
  }
  function schedule() {
    if (frame == null) frame = raf(flush)
  }
  function flush() {
    frame = null
    const next = doc.getElementById('hierarchy-viewport')
    let mountedStamp = null
    try { if (next) mountedStamp = JSON.parse(next.dataset.hierarchyContext) } catch {}
    if (pendingElement && (!sameLifetime(pendingElement.stamp, lifetime) || (mountedStamp && !sameLifetime(pendingElement.stamp, mountedStamp)))) pendingElement = null
    if (pendingElement) {
      const element = doc.getElementById(pendingElement.id)
      if (Date.now() > pendingElement.expires) pendingElement = null
      else if (element) {
        element.scrollIntoView?.({ block: 'center' })
        pendingElement = null
      }
    }
    const scroll = doc.getElementById('main-content-scroll')
    if (!next || !scroll) {
      for (const row of observed) observer?.unobserve(row)
      observed.clear(); heights.clear(); context = null; container = null
      pendingLayout = null; pendingTarget = null; lastReceipt = ''
      if (scroller) scroller.removeEventListener('scroll', schedule)
      scroller = null
      return
    }
    if (scroll !== scroller) {
      scroller?.removeEventListener('scroll', schedule)
      scroller = scroll
      scroller.addEventListener('scroll', schedule, { passive: true })
    }
    container = next
    let stamp
    try { stamp = JSON.parse(container.dataset.hierarchyContext) } catch { return }
    if (!stamp.workspace) return
    if (!context || context.workspace !== stamp.workspace || context.epoch !== stamp.epoch) {
      heights.clear(); lastReceipt = ''; pendingTarget = null
    }
    if (pendingTarget && !sameStamp(pendingTarget.stamp, stamp)) pendingTarget = null
    context = stamp
    if (!lifetime) lifetime = stamp
    const rows = mounted()
    const active = new Set(rows)
    for (const row of observed) if (!active.has(row)) { observer?.unobserve(row); observed.delete(row); heights.delete(row.dataset.hierarchyKey) }
    for (const row of rows) if (!observed.has(row)) { observer?.observe(row); observed.add(row) }
    if (sameStamp(pendingLayout, stamp)) {
      const anchor = rowFor(pendingLayout.anchor)
      if (anchor) scroller.scrollTop = scroller.scrollTop + anchor.getBoundingClientRect().top - scroller.getBoundingClientRect().top + pendingLayout.delta
      else scroller.scrollTop = origin() + Math.max(0, pendingLayout.top)
      if (pendingLayout.target) pendingTarget = { key: pendingLayout.target, focus: pendingTarget?.focus || false, stamp }
      pendingLayout = null
    } else if (pendingLayout && (pendingLayout.workspace !== stamp.workspace || pendingLayout.epoch !== stamp.epoch || pendingLayout.generation !== stamp.generation || pendingLayout.revision < stamp.revision)) pendingLayout = null
    let acknowledged = false
    if (pendingTarget) {
      const target = rowFor(pendingTarget.key)
      if (target) {
        scroller.scrollTop += target.getBoundingClientRect().top - scroller.getBoundingClientRect().top
        if (pendingTarget.focus) {
          const controls = tabbable(target)
          const control = pendingTarget.focus === 'last' ? controls.at(-1) : controls[0]
          control?.focus({ preventScroll: true })
        }
        pendingTarget = null; acknowledged = true
      }
    }
    const measurements = []
    for (const row of rows) {
      const height = row.getBoundingClientRect().height
      if (height > 0 && Math.abs((heights.get(row.dataset.hierarchyKey) ?? -1) - height) > 0.5) {
        heights.set(row.dataset.hierarchyKey, height)
        measurements.push({ key: row.dataset.hierarchyKey, height })
      }
    }
    const focused = doc.activeElement?.closest?.('[data-hierarchy-key]')
    const pins = focused && active.has(focused) ? [focused.dataset.hierarchyKey] : []
    const receipt = { workspace: stamp.workspace, epoch: stamp.epoch, generation: stamp.generation, revision: stamp.revision,
      top: Math.max(0, scroller.scrollTop - origin()), height: scroller.clientHeight, measurements, pins,
      request: pendingTarget?.key || null, acknowledged }
    const serialized = JSON.stringify(receipt)
    if (serialized !== lastReceipt || measurements.length || acknowledged) {
      lastReceipt = serialized
      app.ports.onHierarchyViewport.send(receipt)
    }
  }
  function sync(layout) {
    if (!sameLifetime(lifetime, layout)) pendingElement = null
    lifetime = layout
    pendingLayout = layout; schedule()
  }
  function target(id) {
    pendingElement = null
    if (!id.startsWith('entity-')) {
      // Ports run before Elm's next DOM paint. Keep one short-lived request
      // for the mounted-frame or mutation receipt, including non-tree tabs.
      if (!lifetime) return
      pendingElement = { id, stamp: lifetime, expires: Date.now() + 1000 }
      schedule()
      return
    }
    const element = doc.getElementById(id)
    if (element && !element.closest?.('[data-hierarchy-key]')) { element.scrollIntoView?.({ block: 'center' }); return }
    const row = element?.closest?.('[data-hierarchy-key]')
    if (row) pendingTarget = { key: row.dataset.hierarchyKey, focus: false, stamp: context }
    else if (id.startsWith('entity-')) {
      const entity = id.slice(7)
      // The Elm handler resolves an ID request against its logical index.
      pendingTarget = { key: 'entity:' + entity, focus: false, stamp: context }
    } else return
    schedule()
  }
  function keyboard(event) {
    if (event.key !== 'Tab' || event.ctrlKey || event.metaKey || event.altKey) return
    const row = event.target.closest?.('[data-hierarchy-key]')
    if (!row || !container?.contains(row)) return
    const focusable = tabbable(row)
    const edge = event.shiftKey ? focusable[0] : focusable[focusable.length - 1]
    if (event.target !== edge) return
    const key = event.shiftKey ? row.dataset.hierarchyPrevious : row.dataset.hierarchyNext
    if (!key) return
    event.preventDefault()
    pendingTarget = { key, focus: event.shiftKey ? 'last' : 'first', stamp: context }; schedule()
  }
  const mutations = MO ? new MO(schedule) : null
  mutations?.observe(doc.documentElement, { childList: true, subtree: true, attributes: true, attributeFilter: ['data-hierarchy-context'] })
  doc.addEventListener('focusin', schedule)
  doc.addEventListener('focusout', schedule)
  doc.addEventListener('keydown', keyboard, true)
  win.addEventListener('resize', schedule)
  app.ports.syncHierarchyViewport.subscribe(sync)
  app.ports.scrollHierarchyTarget.subscribe(target)
  schedule()
  return {
    flush,
    dispose() {
      if (frame != null) cancel(frame)
      observer?.disconnect(); mutations?.disconnect()
      scroller?.removeEventListener('scroll', schedule)
      doc.removeEventListener('focusin', schedule); doc.removeEventListener('focusout', schedule)
      doc.removeEventListener('keydown', keyboard, true); win.removeEventListener('resize', schedule)
      app.ports.syncHierarchyViewport.unsubscribe?.(sync); app.ports.scrollHierarchyTarget.unsubscribe?.(target)
      observed.clear(); heights.clear(); pendingElement = null
    }
  }
}
