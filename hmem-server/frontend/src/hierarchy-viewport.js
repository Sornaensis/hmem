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
  let layoutScrollTop = null
  let pendingTarget = null
  let targetAdmission = 0
  let latestTargetAdmission = 0
  let layoutTargetIntent = null
  let pendingLayoutTargetAdmission = 0
  let pendingElement = null
  let retainedFocus = null
  let bridgeFocusedControl = null
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
  function focusControl(control) {
    if (!control) return
    bridgeFocusedControl = control
    try { control.focus({ preventScroll: true }) }
    finally { bridgeFocusedControl = null }
  }
  function admitTarget() { return latestTargetAdmission = ++targetAdmission }
  function retireTargetIntent() { pendingTarget = null; admitTarget() }
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
      pendingLayout = null; pendingTarget = null; layoutTargetIntent = null; retainedFocus = null; lastReceipt = ''
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
    // Elm's keyed renderer can temporarily move a retained row through a
    // detached fragment. Recover only the exact non-editor control after a
    // proven remove/reinsert, never an intentional user focus transition.
    if (retainedFocus?.lost && retainedFocus.removed && retainedFocus.added
      && sameLifetime(retainedFocus.stamp, lifetime) && sameLifetime(retainedFocus.stamp, stamp)
      && retainedFocus.control.isConnected && active.has(retainedFocus.row)
      && retainedFocus.row.contains(retainedFocus.control)
      && rowFor(retainedFocus.key) === retainedFocus.row
      && (!doc.activeElement || doc.activeElement === doc.body)) {
      const saved = retainedFocus
      saved.lost = false; saved.removed = false; saved.added = false
      focusControl(saved.control)
    }
    // A removal and a null-relatedTarget blur must belong to this one paint
    // episode. Neither receipt may authorize a later unrelated focus loss.
    if (retainedFocus) {
      if (retainedFocus.lost) retainedFocus = null
      else { retainedFocus.removed = false; retainedFocus.added = false }
    }
    for (const row of observed) if (!active.has(row)) { observer?.unobserve(row); observed.delete(row); heights.delete(row.dataset.hierarchyKey) }
    for (const row of rows) if (!observed.has(row)) { observer?.observe(row); observed.add(row) }
    if (sameStamp(pendingLayout, stamp)) {
      const currentTarget = !pendingLayout.target || pendingLayoutTargetAdmission >= latestTargetAdmission
      const anchor = rowFor(pendingLayout.anchor)
      // Sample at admission and compare the actual position before writing:
      // a newer scroll can arrive before its event is delivered. Layouts
      // admitted after our own clamped writes sample their resulting position.
      if (currentTarget && (layoutScrollTop == null || scroller.scrollTop === layoutScrollTop)) {
        if (anchor) scroller.scrollTop = scroller.scrollTop + anchor.getBoundingClientRect().top - scroller.getBoundingClientRect().top + pendingLayout.delta
        else scroller.scrollTop = origin() + Math.max(0, pendingLayout.top)
      }
      if (pendingLayout.target && currentTarget) {
        latestTargetAdmission = pendingLayoutTargetAdmission
        const focus = pendingTarget?.key === pendingLayout.target ? pendingTarget.focus : false
        pendingTarget = { key: pendingLayout.target, focus, stamp, admission: pendingLayoutTargetAdmission }
      }
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
          focusControl(control)
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
    if (!sameLifetime(lifetime, layout)) { pendingElement = null; retainedFocus = null }
    lifetime = layout
    layoutScrollTop = doc.getElementById('main-content-scroll')?.scrollTop ?? null
    // Repeated same-stamp target echoes retain their original admission.
    // They cannot outrank a later keyboard intent merely by arriving again.
    if (layout.target) {
      if (!sameStamp(layoutTargetIntent?.stamp, layout) || layoutTargetIntent.key !== layout.target) {
        layoutTargetIntent = { key: layout.target, stamp: layout, admission: ++targetAdmission }
      }
      pendingLayoutTargetAdmission = layoutTargetIntent.admission
    } else layoutTargetIntent = null
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
    if (row) pendingTarget = { key: row.dataset.hierarchyKey, focus: false, stamp: context, admission: admitTarget() }
    else if (id.startsWith('entity-')) {
      const entity = id.slice(7)
      // The Elm handler resolves an ID request against its logical index.
      pendingTarget = { key: 'entity:' + entity, focus: false, stamp: context, admission: admitTarget() }
    } else return
    schedule()
  }
  function keyboard(event) {
    if (['Tab', 'Escape', 'Enter'].includes(event.key)) retainedFocus = null
    if (event.key !== 'Tab' || event.ctrlKey || event.metaKey || event.altKey) return
    const row = event.target.closest?.('[data-hierarchy-key]')
    if (!row || !container?.contains(row)) return
    const focusable = tabbable(row)
    const edge = event.shiftKey ? focusable[0] : focusable[focusable.length - 1]
    if (event.target !== edge) return
    const key = event.shiftKey ? row.dataset.hierarchyPrevious : row.dataset.hierarchyNext
    if (!key) return
    event.preventDefault()
    pendingTarget = { key, focus: event.shiftKey ? 'last' : 'first', stamp: context, admission: admitTarget() }; schedule()
  }
  function focusIn(event) {
    if (event.target !== bridgeFocusedControl) retireTargetIntent()
    const control = event.target, row = control.closest?.('[data-hierarchy-key]')
    let stamp = null
    try { stamp = JSON.parse(container?.dataset.hierarchyContext) } catch {}
    retainedFocus = row && container?.contains(row) && !control.closest?.('.inline-edit,.inline-create-row')
      ? { control, row, key: row.dataset.hierarchyKey, stamp, lost: false, removed: false, added: false } : null
    schedule()
  }
  function focusOut(event) {
    if (retainedFocus?.control === event.target) {
      if (event.relatedTarget) retainedFocus = null
      else retainedFocus.lost = true
    }
    schedule()
  }
  function pointerIntent(event) {
    retireTargetIntent()
    if (retainedFocus && event.target !== retainedFocus.control && !retainedFocus.control.contains?.(event.target)) retainedFocus = null
  }
  function navigationIntent() { retainedFocus = null; retireTargetIntent() }
  const mutations = MO ? new MO(records => {
    if (retainedFocus) for (const record of records) {
      const includesRow = nodes => Array.from(nodes || []).some(node => node === retainedFocus.row || node.contains?.(retainedFocus.row))
      if (includesRow(record.removedNodes)) retainedFocus.removed = true
      if (includesRow(record.addedNodes)) retainedFocus.added = true
    }
    schedule()
  }) : null
  mutations?.observe(doc.documentElement, { childList: true, subtree: true, attributes: true, attributeFilter: ['data-hierarchy-context'] })
  doc.addEventListener('focusin', focusIn)
  doc.addEventListener('focusout', focusOut)
  doc.addEventListener('pointerdown', pointerIntent, true)
  doc.addEventListener('keydown', keyboard, true)
  win.addEventListener('resize', schedule)
  win.addEventListener('hashchange', navigationIntent)
  win.addEventListener('popstate', navigationIntent)
  app.ports.syncHierarchyViewport.subscribe(sync)
  app.ports.scrollHierarchyTarget.subscribe(target)
  schedule()
  return {
    flush,
    dispose() {
      if (frame != null) cancel(frame)
      observer?.disconnect(); mutations?.disconnect()
      scroller?.removeEventListener('scroll', schedule)
      doc.removeEventListener('focusin', focusIn); doc.removeEventListener('focusout', focusOut)
      doc.removeEventListener('pointerdown', pointerIntent, true)
      doc.removeEventListener('keydown', keyboard, true); win.removeEventListener('resize', schedule)
      win.removeEventListener('hashchange', navigationIntent); win.removeEventListener('popstate', navigationIntent)
      app.ports.syncHierarchyViewport.unsubscribe?.(sync); app.ports.scrollHierarchyTarget.unsubscribe?.(target)
      observed.clear(); heights.clear(); pendingElement = null; retainedFocus = null
    }
  }
}
