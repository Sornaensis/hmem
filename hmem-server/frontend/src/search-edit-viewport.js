// Search edits may replace a tall collection with loading or empty results.
// Reserve the current physical window; never restore scroll or consume input.
export function installSearchEditViewport({ document: doc = document, window: win = window } = {}) {
  let owner = null, pointerActive = false, releaseFrame = null, focusFrame = null
  const search = element => element?.matches?.('input[type="search"], input.hierarchy-search-input')
  const context = () => win.location.pathname + ':' + (new URLSearchParams(win.location.hash.slice(1)).get('tab') || 'projects')
  const release = () => {
    pointerActive = false
    if (releaseFrame !== null) win.cancelAnimationFrame(releaseFrame)
    if (focusFrame !== null) win.cancelAnimationFrame(focusFrame)
    releaseFrame = focusFrame = null
    if (!owner) return
    owner.page.style.minHeight = owner.minHeight
    owner.host.style.overflowAnchor = owner.overflowAnchor
    if (owner.editing === undefined) delete owner.host.dataset.searchViewportEditing
    else owner.host.dataset.searchViewportEditing = owner.editing
    owner = null
  }
  const valid = () => owner && owner.input.isConnected && owner.page.isConnected && owner.host.isConnected
    && owner.context === context() && !owner.input.disabled && !owner.input.closest('[hidden], [inert]')
    && owner.input.getClientRects().length > 0 && win.getComputedStyle(owner.input).visibility !== 'hidden'
  const current = () => valid() && (doc.activeElement === owner.input || pointerActive || focusFrame !== null)
  const reserve = () => {
    if (!current()) { release(); return }
    const { host, page } = owner
    const origin = page.getBoundingClientRect().top - host.getBoundingClientRect().top + host.scrollTop
    const needed = Math.ceil(host.scrollTop + host.clientHeight - origin) + 1
    if (Number.isFinite(needed) && needed > owner.floor) {
      owner.floor = needed
      page.style.minHeight = needed + 'px'
    }
  }
  const begin = event => {
    const input = event.target, host = doc.getElementById('main-content-scroll')
    if (!search(input) || doc.activeElement !== input || !host?.contains(input) || input.closest('[inert], dialog')) return
    const page = input.closest('.page')
    if (!page || !host.contains(page)) return
    if (owner && (!current() || owner.page !== page)) release()
    if (!owner) owner = { input, page, host, context: context(), floor: 0, minHeight: page.style.minHeight, overflowAnchor: host.style.overflowAnchor, editing: host.dataset.searchViewportEditing }
    owner.input = input
    host.style.overflowAnchor = 'none'
    host.dataset.searchViewportEditing = 'true'
    reserve()
  }
  const focus = event => {
    if (!owner) return
    if (valid() && search(event.target) && event.target.closest('.page') === owner.page) owner.input = event.target
    else if (!pointerActive) release()
  }
  const blur = () => {
    const previous = owner
    if (focusFrame !== null) win.cancelAnimationFrame(focusFrame)
    focusFrame = win.requestAnimationFrame(() => {
      focusFrame = null
      if (owner === previous && owner && !current()) release()
    })
  }
  const visibility = () => { if (doc.visibilityState === 'hidden') release() }
  // Keep a clicked control under the pointer until its click is dispatched.
  // Releasing short-result geometry on mousedown/focus would move that target.
  const pointer = () => { pointerActive = true }
  const click = event => { pointerActive = false; if (!search(event.target)) release() }
  const pointerEnd = () => {
    if (releaseFrame !== null) win.cancelAnimationFrame(releaseFrame)
    const previous = owner
    releaseFrame = win.requestAnimationFrame(() => {
      releaseFrame = null; pointerActive = false
      if (owner === previous && owner && doc.activeElement !== owner.input) release()
    })
  }
  const scroll = event => { if (owner && event.target === owner.host) reserve() }
  const observer = new MutationObserver(() => { if (owner && !current()) release() })
  observer.observe(doc.body, { childList: true, subtree: true, attributes: true, attributeFilter: ['hidden', 'disabled', 'inert', 'class', 'style'] })
  for (const type of ['beforeinput', 'input', 'compositionstart']) doc.addEventListener(type, begin, true)
  doc.addEventListener('focusin', focus, true)
  doc.addEventListener('focusout', blur, true); doc.addEventListener('visibilitychange', visibility)
  doc.addEventListener('pointerdown', pointer, true)
  doc.addEventListener('pointerup', pointerEnd, true); doc.addEventListener('pointercancel', pointerEnd, true); doc.addEventListener('click', click, true)
  doc.addEventListener('scroll', scroll, true)
  for (const type of ['popstate', 'hashchange', 'resize', 'blur']) win.addEventListener(type, release)
  return () => {
    release(); observer.disconnect()
    for (const type of ['beforeinput', 'input', 'compositionstart']) doc.removeEventListener(type, begin, true)
    doc.removeEventListener('focusin', focus, true); doc.removeEventListener('pointerdown', pointer, true); doc.removeEventListener('scroll', scroll, true)
    doc.removeEventListener('focusout', blur, true); doc.removeEventListener('visibilitychange', visibility)
    doc.removeEventListener('pointerup', pointerEnd, true); doc.removeEventListener('pointercancel', pointerEnd, true); doc.removeEventListener('click', click, true)
    for (const type of ['popstate', 'hashchange', 'resize', 'blur']) win.removeEventListener(type, release)
  }
}
