// The card owns the ratio for its mounted lifetime; Elm retains its content and
// hierarchy measurements. Delegation also covers cards mounted by virtualization.
export function installTaskCardLayout({ document: doc = document, window: win = window } = {}) {
  let drag = null, mountedHandle = null
  const separator = target => target?.closest?.('.task-summary-separator')
  const setShare = (handle, value) => {
    const share = Math.max(20, Math.min(60, value))
    handle.closest('.task-card-with-dock')?.style.setProperty('--task-summary-share', share + '%')
    handle.setAttribute('aria-valuenow', String(Math.round(share)))
  }
  const down = event => {
    const handle = separator(event.target)
    if (!handle || event.button !== 0 || win.getComputedStyle(handle).display === 'none') return
    event.preventDefault()
    event.stopPropagation()
    handle.focus({ preventScroll: true })
    handle.setPointerCapture(event.pointerId)
    drag = { handle, pointer: event.pointerId }
  }
  const move = event => {
    if (!drag || event.pointerId !== drag.pointer) return
    const card = drag.handle.closest('.task-card-with-dock')
    if (!card?.isConnected) { end(); return }
    const rect = card.getBoundingClientRect()
    setShare(drag.handle, 100 * (event.clientX - rect.left) / rect.width)
  }
  const end = () => {
    if (drag?.handle.hasPointerCapture(drag.pointer)) drag.handle.releasePointerCapture(drag.pointer)
    drag = null
  }
  const key = event => {
    const handle = separator(event.target)
    if (!handle || !['ArrowLeft', 'ArrowRight', 'Home', 'End'].includes(event.key)) return
    event.preventDefault()
    event.stopPropagation()
    const current = Number(handle.getAttribute('aria-valuenow')) || 34
    setShare(handle, event.key === 'Home' ? 20 : event.key === 'End' ? 60 : current + (event.key === 'ArrowLeft' ? -2 : 2))
  }
  const reconcile = () => {
    const handle = doc.querySelector('.task-card-with-dock > .task-summary-separator')
    if (handle === mountedHandle) return
    end()
    mountedHandle?.closest('.task-card-with-dock')?.style.removeProperty('--task-summary-share')
    mountedHandle = handle
    if (handle) setShare(handle, 34)
  }
  const mutations = new win.MutationObserver(reconcile)
  mutations.observe(doc.getElementById('main-content-scroll') || doc.documentElement, { childList: true, subtree: true })
  reconcile()
  doc.addEventListener('pointerdown', down)
  doc.addEventListener('pointermove', move)
  doc.addEventListener('pointerup', end)
  doc.addEventListener('pointercancel', end)
  doc.addEventListener('lostpointercapture', end)
  doc.addEventListener('keydown', key)
  return () => {
    end()
    mutations.disconnect()
    mountedHandle?.closest('.task-card-with-dock')?.style.removeProperty('--task-summary-share')
    mountedHandle = null
    doc.removeEventListener('pointerdown', down)
    doc.removeEventListener('pointermove', move)
    doc.removeEventListener('pointerup', end)
    doc.removeEventListener('pointercancel', end)
    doc.removeEventListener('lostpointercapture', end)
    doc.removeEventListener('keydown', key)
  }
}
