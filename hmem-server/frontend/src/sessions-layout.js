// Measure only the mounted Sessions page. Chrome and wrapped filters keep their
// natural size; the remaining grid owns scrolling within the current viewport.
export function installSessionsLayout({ document: doc = document, window: win = window } = {}) {
  const scroll = doc.getElementById('main-content-scroll')
  if (!scroll) return () => {}
  let owner = null, frame = null, disposed = false
  const release = () => {
    owner?.resize?.disconnect()
    owner?.root.style.removeProperty('--sessions-available-height')
    owner?.root.style.removeProperty('--sessions-linked-height')
    owner = null
  }
  const measure = () => {
    frame = null
    if (disposed) return
    const root = scroll.querySelector('.sessions-page')
    if (owner && (owner.root !== root || !root?.isConnected)) release()
    if (!root) return
    if (!owner) {
      owner = { root, resize: null }
      if (win.ResizeObserver) {
        owner.resize = new win.ResizeObserver(schedule)
        owner.resize.observe(scroll)
        const sidebar = root.querySelector('.sessions-list')
        if (sidebar) owner.resize.observe(sidebar)
        for (const child of root.children) if (!child.matches('.sessions-layout')) owner.resize.observe(child)
      }
    }
    const rect = scroll.getBoundingClientRect(), scale = rect.height / scroll.offsetHeight || 1
    const offset = (root.getBoundingClientRect().top - rect.top) / scale + scroll.scrollTop
    const bottom = Number.parseFloat(win.getComputedStyle(root.parentElement).paddingBottom) || 0
    const available = Math.max(0, Math.floor(scroll.clientHeight - offset - bottom))
    const css = available + 'px'
    if (root.style.getPropertyValue('--sessions-available-height') !== css) root.style.setProperty('--sessions-available-height', css)
    const sidebar = root.querySelector('.sessions-list'), summary = sidebar?.querySelector('.session-linked-tasks > summary')
    const linked = Math.max(0, Math.floor((sidebar?.clientHeight || 0) / 2 - (summary?.offsetHeight || 0))) + 'px'
    if (root.style.getPropertyValue('--sessions-linked-height') !== linked) root.style.setProperty('--sessions-linked-height', linked)
  }
  function schedule() {
    if (!disposed && frame === null) frame = win.requestAnimationFrame(measure)
  }
  const mutations = new win.MutationObserver(schedule)
  mutations.observe(scroll, { childList: true, subtree: true })
  win.addEventListener('resize', schedule)
  doc.fonts?.addEventListener('loadingdone', schedule)
  scroll.addEventListener('toggle', schedule, true)
  schedule()
  return () => {
    if (disposed) return
    disposed = true
    mutations.disconnect()
    win.removeEventListener('resize', schedule)
    doc.fonts?.removeEventListener('loadingdone', schedule)
    scroll.removeEventListener('toggle', schedule, true)
    if (frame !== null) win.cancelAnimationFrame(frame)
    frame = null
    release()
  }
}
