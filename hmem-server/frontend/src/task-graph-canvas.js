export function zoomAt(view, scale, point) {
  const next = Math.max(Number.MIN_VALUE, Math.min(3, scale))
  return { scale: next, x: point.x - (point.x - view.x) * next / view.scale, y: point.y - (point.y - view.y) * next / view.scale }
}

// Pointer state and world transforms stay outside layout geometry. Buttons keep
// native click/keyboard behavior; only a real pan/pinch suppresses pointer click.
export function createTaskGraphCanvas(canvas, initial, { saved, save = () => {}, measure = () => {}, ResizeObserverImpl = globalThis.ResizeObserver, now = () => performance.now() } = {}) {
  let metadata = initial, view, gesture = null, suppressUntil = 0, disposed = false
  let settling = !saved && !initial.measured, refinements = 0
  const settleUntil = now() + 1500
  const pointers = new Map(), owners = new Map(), sizes = new Map(), labelSizes = new Map(), observed = new Set()
  const world = () => canvas.querySelector('[data-graph-world]')
  const local = event => { const box = canvas.getBoundingClientRect(); return { x: event.clientX - box.left, y: event.clientY - box.top } }
  const midpoint = (a, b) => ({ x: (a.x + b.x) / 2, y: (a.y + b.y) / 2 })
  const distance = (a, b) => Math.hypot(a.x - b.x, a.y - b.y)
  const apply = next => { view = next; world()?.setAttribute('transform', `translate(${view.x} ${view.y}) scale(${view.scale})`); canvas.dataset.graphTransform = JSON.stringify(view); save({ ...view }) }
  function fit(initialFit = false) {
    const box = metadata.bounds, width = canvas.clientWidth, height = canvas.clientHeight
    const scale = Math.max(Number.MIN_VALUE, Math.min(1, (width - 48) / box.width, (height - 48) / box.height))
    if (initialFit && (width < 650 || scale < 0.75)) apply({ scale: 1, x: width / 2 - metadata.selected.x, y: height / 2 - metadata.selected.y })
    else apply({ scale, x: (width - box.width * scale) / 2 - box.x * scale, y: (height - box.height * scale) / 2 - box.y * scale })
  }
  function start() {
    const values = [...pointers.values()]
    gesture = values.length >= 2 ? { view: { ...view }, point: midpoint(values[0], values[1]), distance: distance(values[0], values[1]), dragged: true }
      : values.length ? { view: { ...view }, point: values[0], dragged: false } : null
  }
  const down = event => {
    if (event.button !== 0 && event.pointerType !== 'touch') return
    suppressUntil = 0
    settling = false
    pointers.set(event.pointerId, local(event))
    const owner = event.target.closest?.('[data-graph-task]') || canvas
    owners.set(event.pointerId, owner); owner.setPointerCapture?.(event.pointerId)
    start()
  }
  const move = event => {
    if (!pointers.has(event.pointerId) || !gesture) return
    pointers.set(event.pointerId, local(event))
    const values = [...pointers.values()]
    if (values.length >= 2) {
      const point = midpoint(values[0], values[1]), scale = Math.max(Number.MIN_VALUE, Math.min(3, gesture.view.scale * distance(values[0], values[1]) / Math.max(1, gesture.distance)))
      apply({ scale, x: point.x - (gesture.point.x - gesture.view.x) * scale / gesture.view.scale, y: point.y - (gesture.point.y - gesture.view.y) * scale / gesture.view.scale })
      gesture.dragged = true
    } else {
      const point = values[0], dx = point.x - gesture.point.x, dy = point.y - gesture.point.y
      if (!gesture.dragged && Math.hypot(dx, dy) < 5) return
      gesture.dragged = true; apply({ ...gesture.view, x: gesture.view.x + dx, y: gesture.view.y + dy })
    }
    canvas.classList.add('is-panning'); event.preventDefault()
  }
  const release = id => { const owner = owners.get(id); owners.delete(id); pointers.delete(id); if (owner?.hasPointerCapture?.(id)) owner.releasePointerCapture(id) }
  const up = event => {
    if (!pointers.has(event.pointerId)) return
    if (gesture?.dragged) suppressUntil = performance.now() + 500
    release(event.pointerId); start(); if (!pointers.size) canvas.classList.remove('is-panning')
  }
  const cancel = () => { if (gesture?.dragged) suppressUntil = performance.now() + 500; for (const id of [...owners.keys()]) release(id); gesture = null; canvas.classList.remove('is-panning') }
  const lost = event => { if (owners.has(event.pointerId)) cancel() }
  const click = event => { if (event.detail !== 0 && performance.now() < suppressUntil) { event.preventDefault(); event.stopImmediatePropagation() } }
  const wheel = event => { settling = false; event.preventDefault(); apply(zoomAt(view, view.scale * Math.exp(-Math.max(-300, Math.min(300, event.deltaY)) * 0.002), local(event))) }
  const handlers = { pointerdown: down, pointermove: move, pointerup: up, pointercancel: cancel, lostpointercapture: lost, click, wheel, keydown: () => { settling = false } }
  for (const [kind, handler] of Object.entries(handlers)) canvas.addEventListener(kind, handler, kind === 'click' ? true : kind === 'wheel' ? { passive: false } : false)
  function measurements() {
    if (disposed) return
    const doc = canvas.ownerDocument, scroll = doc?.getElementById('main-content-scroll')
    if (scroll) {
      const available = Math.max(1, Math.floor(Math.min(scroll.getBoundingClientRect().bottom, doc.defaultView.innerHeight) - canvas.getBoundingClientRect().top - 16))
      if (canvas.style.height !== available + 'px') canvas.style.height = available + 'px'
    }
    const buttons = new Set(canvas.querySelectorAll('[data-graph-task]')), labels = new Set(canvas.querySelectorAll('[data-graph-project-label]'))
    const current = new Set([...buttons, ...labels]), changed = [], changedLabels = []
    for (const element of observed) if (!current.has(element)) { observer?.unobserve(element); observed.delete(element); sizes.delete(element.dataset.graphTask); labelSizes.delete(element.dataset.graphProjectLabel) }
    for (const button of current) {
      if (!observed.has(button)) { observed.add(button); observer?.observe(button) }
      // offset/scroll metrics are unscaled. Buttons have intrinsic auto height,
      // rather than stretching to the SVG's provisional foreignObject box.
      const border = Math.max(0, button.offsetHeight - button.clientHeight)
      const height = Math.ceil(Math.max(button.offsetHeight, button.scrollHeight + border))
      const isLabel = labels.has(button), id = isLabel ? button.dataset.graphProjectLabel : button.dataset.graphTask, cache = isLabel ? labelSizes : sizes
      if (height >= (isLabel ? 1 : 40) && cache.get(id) !== height) { cache.set(id, height); (isLabel ? changedLabels : changed).push({ id, height }) }
    }
    if (changed.length || changedLabels.length) measure({ key: metadata.key, measurements: changed, labels: changedLabels })
  }
  const observer = ResizeObserverImpl ? new ResizeObserverImpl(measurements) : null
  observer?.observe(canvas)
  const doc = canvas.ownerDocument
  for (const target of [doc?.getElementById('main-content-scroll'), doc?.querySelector('.workspace-context-persistent'), canvas.parentElement?.querySelector('.task-graph-toolbar')]) if (target) observer?.observe(target)
  measurements()
  if (saved) apply(saved); else fit(true)
  return {
    update(next) {
      const changed = JSON.stringify([metadata.selected, metadata.bounds]) !== JSON.stringify([next.selected, next.bounds])
      metadata = next; measurements()
      if (settling) {
        if (now() > settleUntil || refinements >= 8) settling = false
        else { if (changed) { fit(true); refinements++ } if (next.measured) settling = false }
      }
    },
    control(action) { settling = false; if (action === 'fit') fit(); else apply(zoomAt(view, view.scale * (action === 'in' ? 1.2 : 1 / 1.2), { x: canvas.clientWidth / 2, y: canvas.clientHeight / 2 })) },
    dispose() { disposed = true; cancel(); observer?.disconnect(); for (const [kind, handler] of Object.entries(handlers)) canvas.removeEventListener(kind, handler, kind === 'click') }
  }
}

export function installTaskGraphCanvas(app, { document: doc = document, window: win = window } = {}) {
  let active = null, queued = false, disposed = false, focusedReceipt = null
  const views = new Map()
  function refresh() {
    queued = false
    if (disposed) return
    const canvas = doc.querySelector('[data-task-graph-canvas]')
    let metadata
    try { metadata = canvas && JSON.parse(canvas.dataset.taskGraphCanvas) } catch { metadata = null }
    if (!canvas || !metadata) { active?.controller.dispose(); active = null; return }
    if (active?.canvas !== canvas || active.key !== metadata.key) {
      active?.controller.dispose()
      const controller = createTaskGraphCanvas(canvas, metadata, { saved: views.get(metadata.key),
        save(value) { views.delete(metadata.key); views.set(metadata.key, value); if (views.size > 64) views.delete(views.keys().next().value) },
        measure: value => app.ports.taskGraphCanvasEvent.send(value), ResizeObserverImpl: win.ResizeObserver })
      active = { canvas, key: metadata.key, controller }
    } else active.controller.update(metadata)
    if (metadata.focus) {
      const identity = JSON.stringify([metadata.key, metadata.focus.token])
      if (identity !== focusedReceipt) {
        const button = [...canvas.querySelectorAll('[data-graph-task]')].find(element => element.dataset.graphTask === metadata.focus.taskId)
        if (button) { button.focus({ preventScroll: true }); focusedReceipt = identity; app.ports.taskGraphCanvasEvent.send({ kind: 'focused', key: metadata.key, token: metadata.focus.token }) }
      }
    }
  }
  const queue = () => { if (!disposed && !queued) { queued = true; win.requestAnimationFrame(refresh) } }
  const observer = new win.MutationObserver(queue)
  // Deliberately exclude transform/style attributes written by the canvas.
  observer.observe(doc.body, { childList: true, subtree: true, attributes: true, attributeFilter: ['data-task-graph-canvas'] })
  const control = event => { const button = event.target.closest?.('[data-graph-control]'); if (button) active?.controller.control(button.dataset.graphControl) }
  doc.addEventListener('click', control); queue()
  return () => { disposed = true; observer.disconnect(); active?.controller.dispose(); doc.removeEventListener('click', control); views.clear() }
}
