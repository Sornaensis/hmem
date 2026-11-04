const maxOriginal = 25 * 1024 * 1024
const maxRaster = 100 * 1024 * 1024
const rasterTypes = new Set(['image/png', 'image/jpeg', 'image/webp'])

const mediaTypes = {
  png: 'image/png', jpg: 'image/jpeg', jpeg: 'image/jpeg', webp: 'image/webp', mp4: 'video/mp4',
  pdf: 'application/pdf', html: 'text/html', htm: 'text/html', svg: 'image/svg+xml',
  txt: 'text/plain', md: 'text/markdown', markdown: 'text/markdown',
  docx: 'application/vnd.openxmlformats-officedocument.wordprocessingml.document',
  pptx: 'application/vnd.openxmlformats-officedocument.presentationml.presentation',
  xlsx: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
  fig: 'application/x-figma', sketch: 'application/x-sketch',
}

// The header is ASCII even when a filename includes a UTF-16 surrogate pair.
export function asciiMetadata(value) {
  return JSON.stringify(value).replace(/[\u007f-\uffff]/g,
    char => '\\u' + char.charCodeAt(0).toString(16).padStart(4, '0'))
}

async function boundedBlob(response, limit, type) {
  const length = response.headers.get('content-length')
  if (length !== null && (!/^[0-9]+$/.test(length) || Number(length) > limit)) {
    await response.body?.cancel()
    throw new Error('The document exceeds the browser transfer limit.')
  }
  if (!response.body) throw new Error('The response did not contain a document.')
  const reader = response.body.getReader()
  const chunks = []
  let size = 0
  try {
    while (true) {
      const { done, value } = await reader.read()
      if (done) break
      size += value.byteLength
      if (size > limit) {
        await reader.cancel()
        throw new Error('The document exceeds the browser transfer limit.')
      }
      chunks.push(value)
    }
    if (length !== null && size !== Number(length)) throw new Error('The document transfer was incomplete.')
    return new Blob(chunks, { type })
  } finally {
    reader.releaseLock()
  }
}

function messageFor(status) {
  if (status === 401 || status === 403) return 'Document access is no longer available.'
  if (status === 409) return 'The document changed. Refresh or retry the operation.'
  if (status === 413) return 'The document exceeds the upload limit.'
  if (status === 422) return 'The file contents do not match a supported document type.'
  if (status === 429 || status === 507) return 'Document capacity is unavailable. Try again later.'
  return 'The document operation could not finish. Please retry.'
}

export function installDocumentTransfer({ document, apiUrl, authHeaders, csrfHeaders, authGuard, onUnauthorized, onEvent }) {
  const api = new URL(apiUrl, document.baseURI)
  const base = api.pathname.replace(/\/$/, '') + '/api/v1/workspaces/'
  let owner = { scope: '', dialog: '' }
  let upload = null
  let busy = null
  const assets = new Map()
  const observed = new Set()
  const visible = new Set()
  const running = new Set()
  const savedUrls = new Map()
  let scheduled = false

  function current(scope, dialog) {
    return scope !== '' && scope === owner.scope && (dialog === undefined || dialog === owner.dialog)
  }

  function send(kind, task, message, extra = {}) {
    onEvent({ kind, task, message, ...owner, ...extra })
  }

  function thumbnailEvent(element, kind) {
    const { documentScope, documentTask, documentThumbnail } = element.dataset
    if (current(documentScope)) send(kind, documentTask, '', { document: documentThumbnail })
  }

  async function request(raw, signal, isCurrent, options = {}) {
    const url = new URL(raw, document.baseURI)
    if (url.origin !== api.origin || !url.pathname.startsWith(base) || url.username || url.password || url.hash) {
      throw new Error('The document URL is unavailable.')
    }
    const currentAuth = authGuard()
    const response = await fetch(url, {
      ...options, signal, credentials: 'include', redirect: 'error', cache: 'no-store', referrerPolicy: 'no-referrer',
      headers: { ...authHeaders(), ...(options.method ? csrfHeaders() : {}), ...options.headers },
    })
    if (signal.aborted || !isCurrent() || !currentAuth()) {
      await response.body?.cancel()
      throw new DOMException('Document authority changed.', 'AbortError')
    }
    if (!response.ok) {
      if (response.status === 401) onUnauthorized()
      await response.body?.cancel()
      throw new Error(messageFor(response.status))
    }
    return response
  }

  function revokeAsset(element, record) {
    record.controller.abort()
    if (record.url) URL.revokeObjectURL(record.url)
    if (element.dataset.documentStyled) {
      record.frame?.remove()
      delete element.dataset.documentFrameState
    } else {
      element.removeAttribute('src')
    }
    assets.delete(element)
  }

  function releaseSave(url) {
    clearTimeout(savedUrls.get(url))
    savedUrls.delete(url)
    URL.revokeObjectURL(url)
  }

  const intersection = new IntersectionObserver(entries => {
    for (const { target, isIntersecting } of entries) {
      if (!observed.has(target) || !current(target.dataset.documentScope)) continue
      if (isIntersecting && !visible.has(target)) {
        visible.add(target)
        thumbnailEvent(target, 'thumbnail-visible')
      } else if (!isIntersecting && visible.delete(target)) {
        thumbnailEvent(target, 'thumbnail-hidden')
      }
    }
    schedule()
  })

  function assetKey(element) {
    const data = element.dataset
    if (!current(data.documentScope) || (data.documentThumbnail && !visible.has(element))) return null
    if (data.documentStyled) {
      if (!current(data.documentScope, 'preview:' + data.documentAssociation)) return null
      return JSON.stringify([data.documentStyled, data.documentGeneration, data.documentAssociation, data.documentRole])
    }
    return data.documentRaster ? JSON.stringify([data.documentRaster, data.documentRasterType, data.documentRasterBytes]) : null
  }

  function schedule() {
    if (scheduled) return
    scheduled = true
    queueMicrotask(() => {
      scheduled = false
      for (const element of document.querySelectorAll('[data-document-thumbnail]')) {
        if (observed.has(element) || !current(element.dataset.documentScope)) continue
        observed.add(element)
        intersection.observe(element)
      }
      for (const element of observed) {
        if (element.isConnected && current(element.dataset.documentScope) && element.dataset.documentThumbnail) continue
        if (visible.delete(element)) thumbnailEvent(element, 'thumbnail-hidden')
        observed.delete(element)
        intersection.unobserve(element)
      }
      for (const [element, record] of assets) {
        if (!element.isConnected || assetKey(element) !== record.key) revokeAsset(element, record)
      }
      const targets = document.querySelectorAll('[data-document-styled], [data-document-raster]')
      for (const element of targets) {
        if (running.size >= 2) break
        const key = assetKey(element)
        if (!key || assets.has(element)) continue
        startAsset(element, key)
      }
    })
  }

  function previewError(element, message) {
    if (element.dataset.documentThumbnail) return
    const task = element.closest('[data-document-preview-task]')?.dataset.documentPreviewTask
    if (task) send('status', task, message)
  }

  function styledState(element, state, message) {
    element.dataset.documentFrameState = state
    element.querySelector('[data-document-frame-status]').textContent = message
  }

  function loadStyledFrame(element, record, url) {
    const { signal } = record.controller
    signal.throwIfAborted()
    const frame = document.createElement('iframe')
    record.frame = frame
    frame.className = 'document-styled-preview'
    frame.title = element.dataset.documentFrameTitle
    frame.setAttribute('sandbox', '')
    frame.referrerPolicy = 'no-referrer'
    frame.tabIndex = -1
    return new Promise((resolve, reject) => {
      const fail = () => finish(new Error('The isolated styled preview did not load.'))
      const loaded = () => {
        // An opaque frame still exposes its child count. Only the complete
        // isolated response creates a child, after the full srcdoc arrives.
        if (frame.contentWindow?.length === 1) finish()
        else fail()
      }
      const aborted = () => finish(signal.reason)
      function finish(error) {
        signal.removeEventListener('abort', aborted)
        frame.removeEventListener('load', loaded)
        frame.removeEventListener('error', fail)
        if (error) {
          frame.remove()
          reject(error)
        } else resolve()
      }
      frame.addEventListener('load', loaded)
      frame.addEventListener('error', fail)
      signal.addEventListener('abort', aborted)
      // Set the URL before connecting the frame so it never loads about:blank.
      frame.src = url.href
      element.querySelector('.document-styled-mount').append(frame)
    })
  }

  async function startAsset(element, key) {
    const record = { key, controller: new AbortController(), url: null }
    assets.set(element, record)
    running.add(record)
    const timer = setTimeout(() => record.controller.abort(), 30000)
    const currentAuth = authGuard()
    const same = () => currentAuth() && element.isConnected && assets.get(element) === record && assetKey(element) === key
    try {
      const { signal } = record.controller
      const { documentStyled, documentGeneration, documentAssociation, documentRole, documentRaster, documentRasterType, documentRasterBytes } = element.dataset
      if (documentStyled) {
        styledState(element, 'loading', 'Loading preview…')
        const response = await request(documentStyled, signal, same, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({
            role: documentRole, association_id: documentAssociation, generation_id: documentGeneration,
            variant: 'styled_html_preview', page: 0,
          }),
        })
        const grant = JSON.parse(await (await boundedBlob(response, 16384, 'application/json')).text())
        const url = new URL(grant.url)
        const loopback = ['localhost', '127.0.0.1', '[::1]'].includes(url.hostname)
        const app = new URL(document.baseURI)
        if ((url.protocol !== 'https:' && !(url.protocol === 'http:' && loopback))
          || url.hostname === app.hostname || url.hostname === api.hostname || url.username || url.password || url.hash
          || !url.pathname.endsWith('/styled_html_preview/0') || !url.searchParams.has('token') || url.searchParams.size !== 1
          || grant.role !== documentRole || grant.association_id !== documentAssociation
          || grant.generation_id !== documentGeneration || grant.variant !== 'styled_html_preview' || grant.page !== 0) {
          throw new Error('The isolated styled preview is unavailable.')
        }
        if (!same()) return
        url.searchParams.append('view', 'frame')
        await loadStyledFrame(element, record, url)
        if (same()) {
          record.frame.tabIndex = 0
          styledState(element, 'ready', 'Preview loaded.')
        }
      } else {
        const expected = Number(documentRasterBytes)
        if (!rasterTypes.has(documentRasterType) || !Number.isSafeInteger(expected) || expected < 1 || expected > maxRaster) {
          throw new Error('The inert image descriptor is unavailable.')
        }
        const response = await request(documentRaster, signal, same)
        if (response.headers.get('content-type')?.split(';')[0].trim() !== documentRasterType) {
          await response.body?.cancel()
          throw new Error('The inert image preview is unavailable.')
        }
        const blob = await boundedBlob(response, expected, documentRasterType)
        if (blob.size !== expected) throw new Error('The preview transfer was incomplete.')
        if (same()) {
          record.url = URL.createObjectURL(blob)
          element.src = record.url
        }
      }
    } catch (error) {
      if (same()) {
        if (element.dataset.documentStyled) {
          styledState(element, 'failed', error.name === 'AbortError' ? 'Preview loading timed out.' : 'Preview could not be loaded.')
        } else {
          previewError(element, error.name === 'AbortError' ? 'Preview transfer timed out. Reopen the preview to retry.' : 'Preview is unavailable. You can download the original.')
        }
      }
    } finally {
      clearTimeout(timer)
      running.delete(record)
      schedule()
    }
  }

  async function transfer(element, callback) {
    const { documentScope, documentTask } = element.dataset
    if (busy || !current(documentScope)) return
    const dialog = owner.dialog
    const controller = new AbortController()
    busy = controller
    const currentAuth = authGuard()
    const isCurrent = () => currentAuth() && element.isConnected && current(documentScope, dialog)
    const timer = setTimeout(() => controller.abort(), 60000)
    try {
      await callback(controller.signal, isCurrent)
    } catch (error) {
      if (isCurrent()) send('status', documentTask, error.name === 'AbortError' ? 'Document transfer timed out. Please retry.' : (error instanceof TypeError ? 'The document transfer failed. Please retry.' : error.message))
    } finally {
      clearTimeout(timer)
      busy = null
    }
  }

  document.addEventListener('submit', event => {
    const form = event.target.closest('form[data-document-upload]')
    if (!form) return
    event.preventDefault()
    const input = form.elements.original
    const file = input.files?.[0]
    if (!file) return
    transfer(form, async (signal, isCurrent) => {
      const title = form.elements.title.value.trim() || file.name
      const mediaType = mediaTypes[file.name.split('.').at(-1).toLowerCase()]
      if (!mediaType) throw new Error('Choose a supported document type.')
      if (file.size < 1 || file.size > maxOriginal) throw new Error('Choose a file from 1 byte to 25 MiB.')
      const encoding = new TextEncoder()
      if (encoding.encode(file.name).length > 255 || encoding.encode(title).length > 1024) throw new Error('The filename or title is too long.')
      const task = form.dataset.documentTask
      if (!upload || upload.file !== file || upload.title !== title) {
        upload = { file, title, id: crypto.randomUUID() }
      }
      const attempt = upload
      const submit = form.querySelector('button[type="submit"]')
      submit.disabled = true
      send('status', task, 'Uploading source…')
      try {
        if (!attempt.hash) {
          const digest = await crypto.subtle.digest('SHA-256', await file.arrayBuffer())
          attempt.hash = Array.from(new Uint8Array(digest), byte => byte.toString(16).padStart(2, '0')).join('')
        }
        if (signal.aborted || !isCurrent()) return
        const metadata = asciiMetadata({
          request_id: attempt.id, role: 'source',
          manifest: { filename: file.name, title, media_type: mediaType, raw_bytes: file.size, raw_sha256: attempt.hash },
        })
        const response = await request(form.dataset.documentUpload, signal, isCurrent, {
          method: 'POST', body: file,
          headers: { 'Content-Type': 'application/octet-stream', 'X-HMem-Document-Metadata': metadata },
        })
        await response.body?.cancel()
        if (isCurrent()) {
          upload = null
          send('uploaded', task, '')
        }
      } finally {
        if (form.isConnected) submit.disabled = false
      }
    })
  })

  document.addEventListener('click', event => {
    const retry = event.target.closest('[data-document-frame-retry]')
    if (retry) {
      const element = retry.closest('[data-document-styled]')
      const record = assets.get(element)
      if (record && element.dataset.documentFrameState === 'failed' && assetKey(element) === record.key) {
        const dialog = retry.closest('[data-document-preview-task]')
        if (document.activeElement === retry) dialog.querySelector('[data-document-preview-close]').focus({ preventScroll: true })
        styledState(element, 'loading', 'Loading preview…')
        send('retry-styled', dialog.dataset.documentPreviewTask, '')
      }
      return
    }
    const button = event.target.closest('[data-document-download]')
    if (!button) return
    transfer(button, async (signal, isCurrent) => {
      button.disabled = true
      try {
        const response = await request(button.dataset.documentDownload, signal, isCurrent)
        const blob = await boundedBlob(response, maxOriginal, 'application/octet-stream')
        if (!isCurrent()) return
        const url = URL.createObjectURL(blob)
        const link = document.createElement('a')
        link.href = url
        link.download = button.dataset.documentFilename
        link.rel = 'noreferrer'
        document.body.append(link)
        link.click()
        link.remove()
        savedUrls.set(url, setTimeout(() => releaseSave(url), 30000))
      } finally {
        if (button.isConnected) button.disabled = false
      }
    })
  })

  const observer = new MutationObserver(schedule)
  observer.observe(document.documentElement, { childList: true, subtree: true, attributes: true,
    attributeFilter: ['data-document-scope', 'data-document-thumbnail', 'data-document-raster', 'data-document-raster-type', 'data-document-raster-bytes', 'data-document-styled', 'data-document-generation', 'data-document-association'] })

  function retire() {
    owner = { scope: '', dialog: '' }
    busy?.abort()
    upload = null
    for (const [element, record] of assets) revokeAsset(element, record)
    for (const url of savedUrls.keys()) releaseSave(url)
    intersection.disconnect()
    observed.clear()
    visible.clear()
  }

  return {
    configure(next) {
      if (owner.scope !== next.scope) retire()
      else if (owner.dialog !== next.dialog) {
        busy?.abort()
        upload = null
      }
      owner = next
      schedule()
    },
    retire,
  }
}

