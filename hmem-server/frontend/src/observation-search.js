// Runtime IDs and physical viewport resets belong to this independent search view.
export function installObservationSearch(app, { document: doc = document, schedule = callback => requestAnimationFrame(callback) } = {}) {
  const uuid = () => {
    if (globalThis.crypto?.randomUUID) return globalThis.crypto.randomUUID()
    const bytes = new Uint8Array(16)
    if (globalThis.crypto?.getRandomValues) globalThis.crypto.getRandomValues(bytes)
    else for (let index = 0; index < bytes.length; index++) bytes[index] = Math.floor(Math.random() * 256)
    bytes[6] = (bytes[6] & 15) | 64; bytes[8] = (bytes[8] & 63) | 128
    const hex = Array.from(bytes, value => value.toString(16).padStart(2, '0')).join('')
    return [hex.slice(0, 8), hex.slice(8, 12), hex.slice(12, 16), hex.slice(16, 20), hex.slice(20)].join('-')
  }
  app.ports.observationSearchRuntime.subscribe(request => {
    const id = uuid()
    queueMicrotask(() => app.ports.observationSearchRuntimeReady.send({ ...request, uuid: id }))
  })
  let pending = null, owner = null, scheduled = false
  const current = () => doc.getElementById('observation-panel')?.dataset.observationSearchReset
  const apply = () => {
    scheduled = false
    const stamp = current(), scroller = doc.getElementById('main-content-scroll')
    if (pending) { owner = stamp === pending ? pending : null; pending = null }
    if (!owner || stamp !== owner || !scroller) { owner = null; return }
    // Older layout/anchor commands can finish later. This explicit clear keeps
    // page-top authority until genuine navigation/scroll intent supersedes it.
    if (current() === owner && scroller.scrollTop !== 0) scroller.scrollTop = 0
  }
  const queue = () => { if (!scheduled) { scheduled = true; schedule(apply) } }
  const retire = () => { owner = null; pending = null }
  const pointer = event => { if ((event.target === doc.getElementById('main-content-scroll') || event.target.closest?.('#observation-panel')) && !event.target.closest?.('input, textarea, select, .observation-query-clear')) retire() }
  const keyboard = event => { if (!event.target.closest?.('input, textarea, select') && ['ArrowDown', 'ArrowUp', 'PageDown', 'PageUp', 'Home', 'End', ' '].includes(event.key)) retire() }
  const scroll = event => { if (owner && event.target.id === 'main-content-scroll') queue() }
  for (const type of ['wheel', 'touchmove']) doc.addEventListener(type, retire, true)
  doc.addEventListener('pointerdown', pointer, true); doc.addEventListener('keydown', keyboard, true); doc.addEventListener('scroll', scroll, true)
  const observer = new MutationObserver(queue)
  observer.observe(doc.body, { subtree: true, childList: true, attributes: true, attributeFilter: ['data-observation-search-reset'] })
  app.ports.resetObservationSearchViewport.subscribe(command => { pending = command.stamp; queue() })
  return () => {
    observer.disconnect(); retire()
    for (const type of ['wheel', 'touchmove']) doc.removeEventListener(type, retire, true)
    doc.removeEventListener('pointerdown', pointer, true); doc.removeEventListener('keydown', keyboard, true); doc.removeEventListener('scroll', scroll, true)
  }
}
