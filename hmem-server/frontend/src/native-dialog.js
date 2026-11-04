// Elm owns modal lifetime; the native top layer provides inertness and Escape.
// Retain the actual invoking element across field switches and restore it on close.
export function installNativeDialog({ document, id }) {
  let active = null, token = null, origin = null, originId = null, owner = null
  const observer = new MutationObserver(() => {
    const next = document.getElementById(id)
    if (next === active && next?.dataset.dialogToken === token) return
    if (active) {
      const fallback = document.getElementById('main-content-scroll')
      const currentOrigin = owner ? document.getElementById(originId) : origin
      if (currentOrigin?.isConnected && !currentOrigin.disabled && currentOrigin.getClientRects().length
        && (!owner || currentOrigin.dataset.dialogOwner === owner)) currentOrigin.focus({ preventScroll: true })
      else if (fallback) { fallback.tabIndex = -1; fallback.focus({ preventScroll: true }) }
    }
    active = next
    token = next?.dataset.dialogToken
    if (!next) return
    originId = next.dataset.dialogOrigin
    owner = next.dataset.dialogOwner
    origin = document.activeElement
    if (!origin || origin === document.body || next.contains(origin)) origin = document.getElementById(next.dataset.dialogOrigin)
    if (!next.open) next.showModal()
    next.querySelector('[autofocus], button')?.focus()
    if (!next.dataset.dialogFocusInstalled) {
      next.dataset.dialogFocusInstalled = 'true'
      next.addEventListener('keydown', event => {
        if (event.key !== 'Tab') return
        const choices = [...next.querySelectorAll('button, input, select, textarea, a[href], iframe, [tabindex]')]
          .filter(element => !element.disabled && element.tabIndex >= 0 && element.getClientRects().length)
        if (!choices.length) return
        const first = choices[0], last = choices.at(-1), focused = document.activeElement
        if (!next.contains(focused) || (event.shiftKey && focused === first) || (!event.shiftKey && focused === last)) {
          event.preventDefault()
          ;(event.shiftKey ? last : first).focus()
        }
      })
      next.addEventListener('focusin', event => {
        if (event.target.matches('[data-dialog-return]')) {
          next.querySelector('[data-document-preview-close]')?.focus()
        }
      })
    }
  })
  observer.observe(document.documentElement, { childList: true, subtree: true, attributes: true, attributeFilter: ['data-dialog-token'] })
  return () => observer.disconnect()
}
