// One owner follows the mounted Audit filter bar. The app grid already places
// persistent chrome outside this scroll surface; offsets use logical CSS pixels.
export function installAuditLayout({ document: doc = document, window: win = window } = {}) {
  const scroll = doc.getElementById('main-content-scroll')
  if (!scroll) return () => {}
  let owner = null, frame = null, disposed = false
  const release = () => {
    owner?.resize?.disconnect()
    owner?.root.style.removeProperty('--audit-filter-height')
    owner?.root.style.removeProperty('--audit-scroll-height')
    owner = null
  }
  const measure = () => {
    frame = null
    if (disposed) return
    const root = scroll.querySelector('.audit-log-view'), filters = root?.querySelector('.audit-log-filters')
    if (owner && (owner.root !== root || owner.filters !== filters || !root?.isConnected)) release()
    if (!root || !filters) return
    if (!owner) {
      owner = { root, filters, resize: null }
      if (win.ResizeObserver) {
        owner.resize = new win.ResizeObserver(schedule)
        owner.resize.observe(filters)
        owner.resize.observe(scroll)
      }
    }
    // offsetHeight/clientHeight remain layout CSS units under browser/CSS zoom;
    // transformed bounding rectangles would count zoom twice in sticky `top`.
    const values = [['--audit-filter-height', filters.offsetHeight], ['--audit-scroll-height', scroll.clientHeight]]
    for (const [key, value] of values) {
      const css = Math.ceil(value) + 'px'
      if (Number.isFinite(value) && value >= 0 && root.style.getPropertyValue(key) !== css) root.style.setProperty(key, css)
    }
  }
  function schedule() {
    if (!disposed && frame === null) frame = win.requestAnimationFrame(measure)
  }
  const mutations = new win.MutationObserver(schedule)
  mutations.observe(scroll, { childList: true, subtree: true })
  win.addEventListener('resize', schedule)
  doc.fonts?.addEventListener('loadingdone', schedule)
  schedule()
  return () => {
    if (disposed) return
    disposed = true
    mutations.disconnect()
    win.removeEventListener('resize', schedule)
    doc.fonts?.removeEventListener('loadingdone', schedule)
    if (frame !== null) win.cancelAnimationFrame(frame)
    frame = null
    release()
  }
}
