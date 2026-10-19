import assert from 'node:assert/strict'
import test from 'node:test'
import { mkdir } from 'node:fs/promises'
import { join } from 'node:path'
import { openDiscovery, subjectTreePage } from './observation-discovery-fixture.mjs'
import { hierarchyFixture } from './hierarchy-fixture.mjs'
import { paint, revealObservationRow } from './observation-viewport-fixture.mjs'

const subject = (kind, path) => ({ subject_kind: kind, subject: path })
const file = path => subject('file', path)
const glob = path => subject('glob', path)
const examples = values => {
  const make = (id, subjects, content = 'Evidence without subject search tokens') => ({ ...values[0], id, subjects, subject_kind: subjects[0].subject_kind, subject: subjects[0].subject, content })
  return [make('auth-main', [file('src/auth/main.elm'), glob('src/**/auth/*.elm')]),
    make('auth-direct', [file('src/auth'), glob('src/auth')]),
    make('shared-doc', [file('docs/readme.md'), file('src/auth/readme.md')]),
    make('content-only', [file('other/path.md')], 'src/auth content only'),
    make('literal', [file('src/auth%_literal.elm')])]
}
const pathRow = (h, path) => h.page.locator('[data-tree-path=' + JSON.stringify(path) + ']')
const rootReads = h => h.receipts.filter(value => value.endpoint.endsWith('/subject-tree') && value.params.prefix === '')
async function tree(h) { await h.start(); await h.page.getByRole('button', { name: 'Subject Tree', exact: true }).click(); await h.idle(); await pathRow(h, 'src').waitFor() }
async function toggle(h, path) { await revealObservationRow(h.page, pathRow(h, path)); await pathRow(h, path).locator('.observation-tree-chevron').click(); await h.idle(); await paint(h.page) }
async function search(h, value) { await h.page.locator('#observation-query').fill(value); await h.page.locator('#observation-query').press('Enter'); await h.idle(); await paint(h.page) }

test('authoritative literal subject search retains ancestors, deduplicates counts and omits unrelated/content-only terminals', { timeout: 60000 }, async () => {
  const h = await openDiscovery(undefined, examples)
  try {
    await tree(h); await search(h, 'src/auth')
    assert.equal(rootReads(h).at(-1).params.subject_query, 'src/auth')
    assert.equal(rootReads(h).at(-1).params.query, undefined)
    assert.deepEqual(rootReads(h).at(-1).response.items.map(value => [value.path, value.observation_count]), [['src', 4]])
    assert.equal(rootReads(h).at(-1).response.total_count, 5)
    assert.equal(rootReads(h).at(-1).response.match_count, 4)
    await toggle(h, 'src'); await toggle(h, 'src/auth')
    assert.equal(await pathRow(h, 'docs').count(), 0)
    assert.equal(await pathRow(h, 'src/**').count(), 0)
    assert.equal(await pathRow(h, 'src/auth/main.elm').count(), 1)
    const lists = h.receipts.filter(value => value.endpoint === '/api/v1/observations' && value.params.subject === 'src/auth')
    assert.deepEqual(lists.map(value => value.params.subject_kind).sort(), ['file', 'glob'])
    assert.ok(lists.every(value => value.params.query === undefined && value.response.items.length === 1))
    assert.equal(await h.page.locator('.observation-detail').count(), 0)
    assert.match(await h.page.locator('.observation-mode-announcement').innerText(), /4 matching observations.*5 in workspace/)
    await search(h, 'docs')
    assert.equal(await h.page.locator('#observation-query').evaluate(element => document.activeElement === element), true, 'Membership focus recovery cannot override newer filter input focus')
    await search(h, 'src/Auth'); assert.equal(rootReads(h).at(-1).response.match_count, 0)
    await h.page.getByRole('button', { name: 'All', exact: true }).click(); await h.idle(); await search(h, 'src/auth')
    const all = h.receipts.filter(value => value.endpoint === '/api/v1/observations' && !value.params.subject).at(-1)
    assert.equal(all.params.query_scope, 'content'); assert.deepEqual(all.response.items.map(value => value.id), ['content-only'])
    await h.page.getByRole('button', { name: 'Subject Tree', exact: true }).click(); await h.idle(); await search(h, '**')
    if (await pathRow(h, 'src').getAttribute('aria-expanded') === 'false') await toggle(h, 'src')
    await pathRow(h, 'src/**').waitFor()
    assert.match(await pathRow(h, 'src/**').innerText(), /Literal pattern/)
    assert.match(await pathRow(h, 'src/**').locator('.observation-tree-copy').innerText(), /src\/\*\*/)
    await h.page.setViewportSize({width: 320, height: 800}); await h.page.addStyleTag({content: 'html {font-size: 200%;}'}); await paint(h.page)
    const layout = await h.page.evaluate(() => ({overflow: document.documentElement.scrollWidth - innerWidth, rows: [...document.querySelectorAll('[data-tree-path]')].map(element => ({path: element.dataset.treePath, right: element.getBoundingClientRect().right}))}))
    assert.ok(layout.overflow <= 1, 'Enlarged narrow Subject Tree fits the viewport: ' + JSON.stringify(layout))
  } finally { await h.close() }
})

test('classic path labels, chevrons, card controls and every path key keep independent in-place identities', { timeout: 60000 }, async () => {
  const h = await openDiscovery(undefined, examples)
  try {
    await tree(h)
    await pathRow(h, 'src').locator('.observation-tree-label').click(); await paint(h.page)
    assert.equal(await pathRow(h, 'src').getAttribute('aria-expanded'), 'false')
    assert.equal(await pathRow(h, 'src/auth').count(), 0)
    await pathRow(h, 'src').focus(); await h.page.keyboard.press('Enter'); await h.idle()
    await h.page.keyboard.press('ArrowRight'); await paint(h.page)
    assert.equal(await h.page.locator(':focus').getAttribute('data-tree-path'), 'src/**')
    await h.page.keyboard.press('ArrowDown'); await paint(h.page)
    assert.equal(await h.page.locator(':focus').getAttribute('data-tree-path'), 'src/auth')
    await h.page.keyboard.press(' '); await h.idle()
    assert.equal(await pathRow(h, 'src/auth').getAttribute('aria-expanded'), 'true')
    await toggle(h, 'docs')
    assert.equal(await pathRow(h, 'src/auth').getAttribute('aria-expanded'), 'true')
    await toggle(h, 'src'); assert.equal(await pathRow(h, 'src/auth').count(), 0)
    await toggle(h, 'src'); assert.equal(await pathRow(h, 'src/auth').getAttribute('aria-expanded'), 'true')
    assert.equal(await h.page.getByRole('tree', { name: 'Stored subject paths' }).count(), 1)
    await pathRow(h, 'src/auth/main.elm').focus(); await h.page.keyboard.press('ArrowLeft'); await paint(h.page)
    assert.equal(await h.page.locator(':focus').getAttribute('data-tree-path'), 'src/auth')
    await h.page.keyboard.press('ArrowLeft'); await paint(h.page)
    assert.equal(await pathRow(h, 'src/auth').getAttribute('aria-expanded'), 'false')
    await h.page.keyboard.press('ArrowUp'); await paint(h.page)
    assert.equal(await h.page.locator(':focus').getAttribute('data-tree-path'), 'src/**')
    await h.page.keyboard.press('ArrowUp'); await paint(h.page)
    assert.equal(await h.page.locator(':focus').getAttribute('data-tree-path'), 'src')
    await h.page.keyboard.press('Home'); await paint(h.page)
    assert.equal(await h.page.locator(':focus').getAttribute('data-tree-path'), 'docs')
    await h.page.keyboard.press('ArrowUp'); await paint(h.page)
    assert.equal(await h.page.locator(':focus').getAttribute('data-tree-path'), 'docs')
    await h.page.keyboard.press('End'); await paint(h.page)
    const last = await h.page.locator(':focus').getAttribute('data-tree-path')
    await h.page.keyboard.press('ArrowDown'); await paint(h.page)
    assert.equal(await h.page.locator(':focus').getAttribute('data-tree-path'), last)
    await toggle(h, 'src/auth'); await toggle(h, 'src/auth/main.elm')
    await pathRow(h, 'src/auth/main.elm').focus(); await h.page.keyboard.press('ArrowRight'); await paint(h.page)
    assert.equal(await h.page.locator(':focus').getAttribute('data-tree-path'), 'src/auth/main.elm')
    assert.equal(await h.page.locator('.observation-detail').count(), 0)
    const card = h.page.locator('.observation-result[data-observation-id="auth-main"] .observation-card').first()
    await card.click(); await h.idle(); assert.equal(await h.page.locator('.observation-detail').count(), 1)
    const expanded = await pathRow(h, 'src/auth/main.elm').getAttribute('aria-expanded')
    await card.focus(); await h.page.keyboard.press('ArrowLeft'); await paint(h.page)
    assert.equal(await pathRow(h, 'src/auth/main.elm').getAttribute('aria-expanded'), expanded)
    await toggle(h, 'src'); await toggle(h, 'src')
    assert.equal(await h.page.locator('.observation-detail').count(), 1)
    await search(h, 'docs')
    assert.equal(JSON.parse(await h.page.locator('#observation-panel').getAttribute('data-observation-context')).selectedId, null, 'An authoritative filtered catalog retires a removed card selection')
    assert.equal(new URLSearchParams(new URL(h.page.url()).hash.slice(1)).has('observation'), false)
    assert.equal(await h.page.locator('#observation-query').inputValue(), 'docs')
  } finally { await h.close() }
})

test('wide continuation lazily reveals a direct observation beyond the first child page under the global viewport bound', { timeout: 60000 }, async () => {
  const h = await openDiscovery(undefined, values => {
    const base = examples(values)[0]
    const make = (id, path) => ({ ...base, id, subject_kind: 'file', subject: path, subjects: [file(path)] })
    return [...Array.from({ length: 225 }, (_, i) => make('wide-' + i, 'wide/branch-' + String(i).padStart(3, '0') + '/file.elm')),
      make('deep-target', 'wide/branch-224/deep/auth/main.elm')]
  })
  try {
    const query = ['tree', '', null, '', '', null, null, [], '', '']
    await h.page.goto(h.origin + '/workspace/' + h.fixture.workspace.id + '#tab=observations&ov=1&oq=' + encodeURIComponent(JSON.stringify(query)) + '&observation=deep-target')
    await h.page.waitForFunction(() => document.querySelector('.observation-detail-content')?.textContent.includes('Evidence'), null, { timeout: 10000 })
    await h.idle(); await paint(h.page)
    assert.ok(h.receipts.some(value => value.params.prefix === 'wide' && value.params.offset === '200'))
    assert.ok(h.receipts.some(value => value.params.prefix === 'wide/branch-224/deep/auth'))
    assert.ok(!h.receipts.some(value => value.params.prefix === 'wide/branch-000'))
    assert.equal(await h.page.locator('.observation-detail').count(), 1)
    assert.ok(await h.page.locator('[data-observation-key]').count() <= 28)
    assert.ok(Number(await h.page.locator('#observation-viewport').getAttribute('data-observation-logical-count')) > 225)
    await h.page.waitForFunction(() => document.activeElement?.matches('[data-observation-detail-anchor]'), null, { timeout: 5000 })
  } finally { await h.close() }
})

test('branch errors retry and stale prior-filter root replies cannot revive retired membership', { timeout: 60000 }, async () => {
  const h = await openDiscovery(undefined, examples)
  let release, arrived
  try {
    let fail = true
    await h.page.route('**/api/v1/observations/subject-tree?**', async route => {
      const url = new URL(route.request().url())
      if (url.searchParams.get('prefix') === 'src' && fail) { fail = false; return route.fulfill({status: 503, body: '{}'}) }
      return route.fallback()
    })
    await tree(h); await toggle(h, 'src')
    await h.page.getByRole('button', { name: 'Retry paths', exact: true }).click(); await h.idle()
    await pathRow(h, 'src/auth').waitFor()
    const held = new Promise(resolve => { release = resolve }), arrival = new Promise(resolve => { arrived = resolve })
    await h.page.route('**/api/v1/observations/subject-tree?**', async route => {
      const url = new URL(route.request().url())
      if (url.searchParams.get('subject_query') !== 'src/auth' || url.searchParams.get('prefix') !== '') return route.fallback()
      arrived(); await held
      return route.fulfill({status: 200, contentType: 'application/json', body: JSON.stringify(subjectTreePage(h.fixture, {prefix: '', subjectQuery: 'src/auth', offset: 0, limit: 200}))})
    })
    await h.page.locator('#observation-query').fill('src/auth'); await h.page.locator('#observation-query').press('Enter')
    await h.bounded(arrival, 5000, 'Old literal subject read admitted')
    await search(h, 'docs'); release(); await h.idle(); await paint(h.page)
    assert.equal(await pathRow(h, 'src').count(), 0)
    assert.equal(await pathRow(h, 'docs').count(), 1)
  } finally { release?.(); await h.close() }
})

test('terminal continuation and canonical replacement retain a demanded off-page card until the full staged span completes', { timeout: 60000 }, async () => {
  const h = await openDiscovery(undefined, values => Array.from({length: 205}, (_, i) => ({ ...values[0], id: 'many-' + String(i).padStart(3, '0'), subjects: [file('src/auth/main.elm')], subject_kind: 'file', subject: 'src/auth/main.elm' })))
  let release
  try {
    const query = ['tree', '', null, '', '', null, null, [], '', '']
    await h.page.goto(h.origin + '/workspace/' + h.fixture.workspace.id + '#tab=observations&ov=1&oq=' + encodeURIComponent(JSON.stringify(query)) + '&observation=many-000')
    await h.page.waitForFunction(() => document.querySelector('#observation-viewport')?.dataset.observationLoadedCount === '205', null, {timeout: 10000})
    await h.idle()
    assert.ok(h.receipts.some(value => value.params.subject === 'src/auth/main.elm' && value.params.offset === '200'))
    assert.equal(await h.page.locator('.observation-detail').count(), 1)
    assert.ok(await h.page.locator('[data-observation-key]').count() <= 28)
    let arrived
    const arrival = new Promise(resolve => { arrived = resolve }), held = new Promise(resolve => { release = resolve })
    await h.page.route('**/api/v1/observations?**', async route => {
      const url = new URL(route.request().url())
      if (url.searchParams.get('subject') !== 'src/auth/main.elm' || url.searchParams.get('offset') !== '200') return route.fallback()
      arrived(); await held; return route.fallback()
    })
    h.fixture.observations = h.fixture.observations.filter(value => value.id !== 'many-204')
    await h.page.evaluate(workspace => window.pushHierarchyFrames([{schema_version: 1, type: 'change', event: {schema_version: 1, event_id: 'tree-refresh', scope: 'workspace', workspace_id: workspace, occurred_at: '2026-10-19T10:00:00Z', transaction: {id: 'tree-refresh-tx', cause: 'rest', request_id: null}, actor: {type: 'system', id: null}, entity: {type: 'observation', id: 'unloaded-canonical-change', action: 'created'}, invalidations: [{kind: 'collection', target: 'observations:' + workspace}]}}]), h.fixture.workspace.id)
    await h.bounded(arrival, 10000, 'Staged terminal continuation admitted')
    assert.equal(await h.page.locator('#observation-viewport').getAttribute('data-observation-loaded-count'), '205')
    assert.equal(await h.page.locator('.observation-detail').count(), 1)
    release(); await h.idle()
    await h.page.waitForFunction(() => document.querySelector('#observation-viewport')?.dataset.observationLoadedCount === '204', null, {timeout: 10000})
    assert.equal(rootReads(h).at(-1).response.match_count, 204)
    assert.equal(await h.page.locator('.observation-detail').count(), 1)
  } finally { release?.(); await h.close() }
})


test('five expanded branches share four physical slots through collapse and stale completion', { timeout: 60000 }, async () => {
  const h = await openDiscovery(undefined, values => ['a', 'b', 'c', 'd', 'e', 'src'].map((path, i) => ({...values[0], id: 'slot-' + i, subjects: [file(path + '/child.elm')], subject: path + '/child.elm'})))
  const releases = new Map(), arrivals = [], active = new Set()
  let maximum = 0
  try {
    await h.page.route('**/api/v1/observations/subject-tree?**', async route => {
      const url = new URL(route.request().url()), prefix = url.searchParams.get('prefix')
      if (!['a', 'b', 'c', 'd', 'e'].includes(prefix)) return route.fallback()
      arrivals.push(prefix); active.add(prefix); maximum = Math.max(maximum, active.size)
      await new Promise(resolve => releases.set(prefix, resolve))
      active.delete(prefix)
      await route.fulfill({status: 200, contentType: 'application/json', body: JSON.stringify(subjectTreePage(h.fixture, {prefix, offset: 0, limit: 200}))})
    })
    await tree(h)
    for (const prefix of ['a', 'b', 'c', 'd', 'e']) await toggle(h, prefix)
    assert.deepEqual(arrivals, ['a', 'b', 'c', 'd'])
    await toggle(h, 'a'); await paint(h.page)
    assert.deepEqual(arrivals, ['a', 'b', 'c', 'd'], 'collapse does not refund an outstanding transport')
    releases.get('a')()
    await h.bounded((async () => { while (!releases.has('e')) await h.page.waitForTimeout(20) })(), 5000, 'Queued fifth branch after exact callback')
    assert.equal(await pathRow(h, 'a/child.elm').count(), 0)
    for (const prefix of ['b', 'c', 'd', 'e']) releases.get(prefix)()
    await h.idle(); await paint(h.page)
    assert.equal(maximum, 4)
    assert.equal(await pathRow(h, 'e/child.elm').count(), 1)
    assert.equal(await pathRow(h, 'a').getAttribute('aria-expanded'), 'false')
    assert.ok(await h.page.locator('[data-observation-key]').count() <= 28)
  } finally { for (const release of releases.values()) release(); await h.close() }
})


test('late detail membership and mode return preserve a newer path selection and outside focus', { timeout: 60000 }, async () => {
  const h = await openDiscovery(undefined, examples)
  let release, arrived
  const held = new Promise(resolve => {release = resolve}), arrival = new Promise(resolve => {arrived = resolve})
  try {
    await h.page.route('**/api/v1/observations/auth-main', async route => {
      arrived(); await held; return route.fallback()
    })
    const query = ['tree', '', null, '', '', null, null, [], '', '']
    await h.page.goto(h.origin + '/workspace/' + h.fixture.workspace.id + '#tab=observations&ov=1&oq=' + encodeURIComponent(JSON.stringify(query)) + '&observation=auth-main')
    await h.bounded(arrival, 5000, 'Deep-link ancestor admitted')
    await pathRow(h, 'docs').waitFor()
    await pathRow(h, 'docs').locator('.observation-tree-label').click(); await paint(h.page)
    assert.equal(await pathRow(h, 'docs').getAttribute('aria-selected'), 'true')
    await h.page.locator('#observation-query').focus(); await paint(h.page)
    release(); await h.idle()
    await h.page.locator('.observation-detail-content').waitFor({timeout: 5000}); await paint(h.page)
    assert.equal(await h.page.locator('#observation-query').evaluate(element => document.activeElement === element), true)
    assert.equal(await pathRow(h, 'src/auth/main.elm').getAttribute('aria-expanded'), 'true')
    assert.equal(await h.page.locator('.observation-detail').count(), 1)
    assert.equal(await pathRow(h, 'docs').getAttribute('aria-selected'), 'true')
    assert.equal(await pathRow(h, 'src/auth/main.elm').getAttribute('aria-selected'), 'false')
    await h.page.getByRole('button', {name: 'All', exact: true}).click(); await h.idle()
    await h.page.getByRole('button', {name: 'Subject Tree', exact: true}).click(); await h.idle(); await paint(h.page)
    assert.equal(await pathRow(h, 'docs').getAttribute('aria-selected'), 'true')
    assert.equal(await pathRow(h, 'src/auth/main.elm').getAttribute('aria-selected'), 'false')
  } finally {release?.(); await h.close()}
})

test('same-query explicit card navigation selects its resolved path while an internal URL echo preserves newer path selection', {timeout: 60000}, async () => {
  const h = await openDiscovery(undefined, examples)
  let release, arrived
  const held = new Promise(resolve => {release = resolve}), arrival = new Promise(resolve => {arrived = resolve})
  try {
    await tree(h)
    await pathRow(h, 'docs').locator('.observation-tree-label').click(); await paint(h.page)
    const roots = rootReads(h).length
    await h.page.route('**/api/v1/observations/auth-main', async route => {arrived(); await held; return route.fallback()})
    const query = ['tree', '', null, '', '', null, null, [], '', '']
    await h.page.evaluate(fragment => {location.hash = fragment}, 'tab=observations&ov=1&oq=' + encodeURIComponent(JSON.stringify(query)) + '&observation=auth-main')
    await h.bounded(arrival, 5000, 'Explicit same-query card detail admitted')
    release(); await h.idle(); await h.page.locator('.observation-detail-content').waitFor(); await paint(h.page)
    assert.equal(rootReads(h).length, roots, 'Same-query navigation uses the current root membership')
    assert.equal(await pathRow(h, 'src/auth/main.elm').getAttribute('aria-selected'), 'true')
    assert.equal(await pathRow(h, 'docs').getAttribute('aria-selected'), 'false')
    await revealObservationRow(h.page, pathRow(h, 'docs'))
    await pathRow(h, 'docs').locator('.observation-tree-label').click(); await paint(h.page)
    await h.page.evaluate(() => window.dispatchEvent(new PopStateEvent('popstate'))); await paint(h.page)
    assert.equal(await pathRow(h, 'docs').getAttribute('aria-selected'), 'true')
    assert.equal(await pathRow(h, 'src/auth/main.elm').getAttribute('aria-selected'), 'false')
  } finally {release?.(); await h.close()}
})

async function invalidateTree(h, id) {
  await h.page.evaluate(({workspace, id}) => window.pushHierarchyFrames([{schema_version: 1, type: 'change', event: {schema_version: 1, event_id: id, scope: 'workspace', workspace_id: workspace, occurred_at: '2026-10-19T10:00:00Z', transaction: {id, cause: 'rest', request_id: null}, actor: {type: 'system', id: null}, entity: {type: 'observation', id: 'unloaded-' + id, action: 'created'}, invalidations: [{kind: 'collection', target: 'observations:' + workspace}]}}]), {workspace: h.fixture.workspace.id, id})
}

for (const boundary of ['root', 'terminal']) test('live ' + boundary + ' boundary insertion retains an open card throughout held continuation without stealing focus', {timeout: 60000}, async () => {
  const targetId = boundary === 'root' ? 'root-card-199' : 'many-005'
  const targetPath = boundary === 'root' ? 'root-199' : 'src/auth/main.elm'
  const h = await openDiscovery(undefined, values => Array.from({length: boundary === 'root' ? 201 : 205}, (_, i) => {
    const path = boundary === 'root' ? 'root-' + String(i).padStart(3, '0') + (i === 0 ? '/child.elm' : '') : targetPath
    return {...values[0], id: (boundary === 'root' ? 'root-card-' : 'many-') + String(i).padStart(3, '0'), subjects: [file(path)], subject_kind: 'file', subject: path}
  }))
  let release
  try {
    const query = ['tree', '', null, '', '', null, null, [], '', '']
    await h.page.goto(h.origin + '/workspace/' + h.fixture.workspace.id + '#tab=observations&ov=1&oq=' + encodeURIComponent(JSON.stringify(query)) + '&observation=' + targetId)
    await h.page.locator('.observation-detail-content').waitFor(); await h.idle(); await paint(h.page)
    assert.equal(await h.page.locator('#observation-viewport').getAttribute('data-observation-loaded-count'), boundary === 'root' ? '1' : '200')
    if (boundary === 'root') {
      await toggle(h, 'root-000')
      assert.equal(await pathRow(h, 'root-000').getAttribute('aria-expanded'), 'true')
      assert.equal(await pathRow(h, 'root-000').getAttribute('aria-setsize'), '-1')
    }
    let arrived
    const arrival = new Promise(resolve => {arrived = resolve}), held = new Promise(resolve => {release = resolve})
    await h.page.route(boundary === 'root' ? '**/api/v1/observations/subject-tree?**' : '**/api/v1/observations?**', async route => {
      const params = new URL(route.request().url()).searchParams
      if (params.get('offset') !== '200' || (boundary === 'root' ? params.get('prefix') !== '' : params.get('subject') !== targetPath)) return route.fallback()
      arrived(); await held; return route.fallback()
    })
    const base = h.fixture.observations[0], insertedPath = boundary === 'root' ? 'earlier' : targetPath
    h.fixture.observations.push({...base, id: 'zz-inserted', subjects: [file(insertedPath)], subject: insertedPath})
    await h.page.locator('#observation-query').focus(); await paint(h.page)
    await invalidateTree(h, 'shift-' + boundary)
    await h.bounded(arrival, 10000, 'Selected card continuation admitted after live boundary insertion')
    assert.equal(await h.page.locator('.observation-detail').count(), 1, 'The old demanded card stays displayed before offset200 returns')
    assert.equal(JSON.parse(await h.page.locator('#observation-panel').getAttribute('data-observation-context')).selectedId, targetId)
    assert.equal(await h.page.locator('#observation-query').evaluate(element => document.activeElement === element), true)
    if (boundary === 'terminal') assert.equal(await h.page.locator('#observation-viewport').getAttribute('data-observation-loaded-count'), '200')
    release(); await h.idle(); await paint(h.page)
    assert.equal(await h.page.locator('.observation-detail').count(), 1)
    assert.equal(JSON.parse(await h.page.locator('#observation-panel').getAttribute('data-observation-context')).selectedId, targetId)
    assert.equal(await h.page.locator('#observation-query').evaluate(element => document.activeElement === element), true)
    if (boundary === 'root') {
      await revealObservationRow(h.page, pathRow(h, 'root-000'))
      assert.equal(await pathRow(h, 'root-000').getAttribute('aria-expanded'), 'true')
      assert.equal(await pathRow(h, 'root-000').getAttribute('aria-setsize'), '202')
      assert.ok(!h.receipts.some(value => value.params.subject === 'root-198'), 'Other terminal cards are not eagerly fetched')
    }
    assert.ok(await h.page.locator('[data-observation-key]').count() <= 28)
  } finally { release?.(); await h.close() }
})

test('a settled terminal-only link preserves newer parent collapse and native focus through canonical refresh', {timeout: 60000}, async () => {
  const h = await openDiscovery()
  try {
    const query = ['exact', 'src', null, '', h.sha, 'glob', 'src/**/*.elm', [], '', '']
    await h.page.goto(h.origin + '/workspace/' + h.fixture.workspace.id + '#tab=observations&ov=1&oq=' + encodeURIComponent(JSON.stringify(query)))
    await h.page.waitForFunction(() => document.activeElement?.getAttribute('data-tree-path') === 'src/**/*.elm', null, {timeout: 5000})
    await h.idle()
    await toggle(h, 'src')
    await pathRow(h, 'src').focus(); await paint(h.page)
    const reads = h.receipts.length
    await invalidateTree(h, 'settled-terminal-collapse'); await h.idle(); await paint(h.page)
    assert.equal(await pathRow(h, 'src').getAttribute('aria-expanded'), 'false')
    assert.equal(await pathRow(h, 'src/**/*.elm').count(), 0)
    assert.equal(await pathRow(h, 'src').evaluate(element => document.activeElement === element), true)
    assert.equal(await h.page.locator('.observation-detail').count(), 0)
    const context = JSON.parse(new URLSearchParams(new URL(h.page.url()).hash.slice(1)).get('oq'))
    assert.equal(context[0], 'exact'); assert.equal(context[5], 'glob'); assert.equal(context[6], 'src/**/*.elm')
    assert.ok(!h.receipts.slice(reads).some(value => value.params.prefix === 'src' || value.params.subject), 'Collapsed selection does not admit hidden subtree or terminal refresh')
  } finally { await h.close() }
})

test('scaffold and dual-kind label selection stays separate from focus expansion and passive preferences', {timeout: 60000}, async () => {
  const h = await openDiscovery(undefined, examples)
  try {
    await tree(h)
    await pathRow(h, 'src').locator('.observation-tree-label').click(); await paint(h.page)
    assert.equal(await pathRow(h, 'src').getAttribute('aria-selected'), 'true')
    assert.equal(await pathRow(h, 'src').getAttribute('aria-expanded'), 'false')
    await h.page.keyboard.press('Home'); await paint(h.page)
    assert.equal(await pathRow(h, 'docs').evaluate(element => document.activeElement === element), true)
    assert.equal(await pathRow(h, 'docs').getAttribute('aria-selected'), 'false')
    assert.equal(await pathRow(h, 'src').getAttribute('aria-selected'), 'true')
    await pathRow(h, 'docs').focus(); await paint(h.page)
    assert.equal(await pathRow(h, 'src').getAttribute('aria-selected'), 'true')
    await toggle(h, 'docs'); await toggle(h, 'src')
    await pathRow(h, 'src/auth').locator('.observation-tree-label').click(); await paint(h.page)
    assert.equal(await pathRow(h, 'src/auth').getAttribute('aria-selected'), 'true', 'Both terminal kinds share one selectable path')
    assert.equal(await pathRow(h, 'src/auth').getAttribute('aria-expanded'), 'false')
    assert.equal(await h.page.locator('.observation-detail').count(), 0)
    await toggle(h, 'src'); await toggle(h, 'src')
    assert.equal(await pathRow(h, 'src/auth').getAttribute('aria-selected'), 'true')
    await toggle(h, 'src')
    await h.page.goto(h.origin + '/'); await h.idle()
    const before = h.receipts.length
    await h.page.goto(h.origin + '/workspace/' + h.fixture.workspace.id + '#tab=observations'); await h.idle(); await paint(h.page)
    assert.equal(await h.page.getByRole('button', {name: 'Subject Tree', exact: true}).getAttribute('aria-pressed'), 'true')
    assert.equal(await pathRow(h, 'src').getAttribute('aria-expanded'), 'false')
    assert.ok(!h.receipts.slice(before).some(value => value.params.prefix === 'src' || value.params.subject === 'src/auth'), 'Saved selection does not reveal or read its hidden ancestry')
    await toggle(h, 'src')
    assert.equal(await pathRow(h, 'src/auth').getAttribute('aria-selected'), 'true')
    assert.equal(await pathRow(h, 'src').getAttribute('aria-selected'), 'false')
  } finally {await h.close()}
})

test('eighteen-segment paths keep path terminal card copy and continuation controls inside the narrow enlarged-text scrollport', {timeout: 90000}, async () => {
  const parts = Array.from({length: 18}, (_, i) => 'd' + String(i).padStart(2, '0')), deep = parts.join('/')
  const h = await openDiscovery({width: 320, height: 1100}, values => Array.from({length: 205}, (_, i) => ({...values[0], id: 'deep-' + String(i).padStart(3, '0'), subjects: [file(deep)], subject_kind: 'file', subject: deep})))
  const inside = async (control, name, interactive = true) => {
    if (interactive) await revealObservationRow(h.page, control)
    else {await control.scrollIntoViewIfNeeded(); await paint(h.page)}
    const rect = await control.evaluate(element => {
      const box = element.getBoundingClientRect(), host = document.getElementById('main-content-scroll').getBoundingClientRect()
      return {left: box.left, right: box.right, top: box.top, bottom: box.bottom, width: box.width, host: {left: host.left, right: host.right, top: host.top, bottom: host.bottom}}
    })
    assert.ok(rect.width > 0 && rect.left >= rect.host.left - 1 && rect.right <= rect.host.right + 1 && rect.top >= rect.host.top - 1 && rect.bottom <= rect.host.bottom + 1, name + ': ' + JSON.stringify(rect))
  }
  const indented = async (row, name) => {
    const value = await row.evaluate(element => {const row = element.closest('[data-observation-key]'), content = row.querySelector('.observation-tree-row-content'); return {padding: content.getBoundingClientRect().left - row.getBoundingClientRect().left, width: row.getBoundingClientRect().width}})
    assert.ok(value.padding > 0 && value.padding <= value.width * .2 + 1, name + ': ' + JSON.stringify(value))
  }
  const snapshot = async (name, target) => {
    if (!process.env.HMEM_HEADER_SCREENSHOT_DIR) return
    await mkdir(process.env.HMEM_HEADER_SCREENSHOT_DIR, {recursive: true})
    await target.evaluate(element => element.scrollIntoView({block: 'center'})); await paint(h.page)
    await h.page.locator('#main-content-scroll').screenshot({path: join(process.env.HMEM_HEADER_SCREENSHOT_DIR, name + '-320.png')})
  }
  try {
    await h.start(); await h.page.addStyleTag({content: 'html {font-size: 200%;}'})
    await h.page.getByRole('button', {name: 'Subject Tree', exact: true}).click(); await h.idle()
    for (let depth = 1; depth <= parts.length; depth++) {
      const path = parts.slice(0, depth).join('/'), row = pathRow(h, path)
      await inside(row, 'Path level ' + depth)
      if (depth > 1) {
        const indent = await row.evaluate(element => {const row = element.closest('[data-observation-key]'), content = row.querySelector('.observation-tree-row-content'); return {padding: content.getBoundingClientRect().left - row.getBoundingClientRect().left, width: row.getBoundingClientRect().width}})
        assert.ok(indent.padding > 0 && indent.padding <= indent.width * .2 + 1, 'Actual indentation remains positive and bounded: ' + JSON.stringify(indent))
      }
      await inside(row.locator('.observation-tree-chevron'), 'Chevron level ' + depth)
      await row.locator('.observation-tree-chevron').click(); await h.idle(); await paint(h.page)
    }
    const leaf = pathRow(h, deep)
    assert.equal(await leaf.getAttribute('aria-level'), '18')
    assert.equal(await leaf.getAttribute('aria-selected'), 'false', 'Expansion does not synthesize selection')
    await inside(leaf, 'Deep selectable path'); await leaf.locator('.observation-tree-label').click(); await paint(h.page)
    assert.equal(await leaf.getAttribute('aria-selected'), 'true')
    assert.equal(await h.page.locator('.observation-tree-depth').count(), 0)
    assert.ok(!/Level\s+\d+/.test(await h.page.locator('#observation-panel').innerText()))
    assert.ok(await h.page.locator('.observation-tree-pattern, .observation-tree-path .observation-loaded-count').evaluateAll(elements => elements.every(element => element.scrollWidth <= element.clientWidth + 1)), 'Path metadata remains readable within deep/narrow cards')
    await inside(leaf.locator('.observation-tree-copy'), 'Deep canonical path copy')
    assert.equal(await leaf.locator('.observation-tree-copy').getAttribute('aria-label'), 'Copy canonical subject path: ' + deep)
    const terminal = h.page.getByRole('region', {name: 'File observations for ' + deep, exact: true})
    await inside(terminal, 'Deep typed terminal', false)
    await inside(terminal.locator('span').last(), 'Deep typed terminal heading', false)
    await indented(terminal, 'Terminal indentation')
    const card = h.page.locator('.observation-result[data-observation-id="deep-204"] .observation-card')
    await inside(card, 'Deep observation disclosure')
    await indented(card.locator('xpath=ancestor::div[contains(@class,"observation-tree-observation")][1]'), 'Observation indentation')
    await card.click(); await h.idle()
    assert.equal(await h.page.locator('.observation-detail').count(), 1)
    await inside(h.page.locator('.observation-result[data-observation-id="deep-204"] .observation-subject').first(), 'Deep observation canonical copy')
    await snapshot('deep-tree', leaf)
    const more = h.page.getByRole('button', {name: 'Load more observations', exact: true})
    await inside(more, 'Deep terminal continuation')
    await indented(more.locator('..'), 'Continuation indentation')
    await snapshot('deep-continuation', more)
    await more.click(); await h.idle(); await paint(h.page)
    assert.ok(h.receipts.some(value => value.params.subject === deep && value.params.offset === '200'))
    assert.equal(await h.page.locator('#observation-viewport').getAttribute('data-observation-loaded-count'), '205')
    assert.equal(await h.page.locator('#observation-panel').evaluate(element => JSON.parse(element.dataset.observationContext).selectedId), 'deep-204')
    assert.ok(await h.page.locator('[data-observation-key]').count() <= 28)
    assert.ok(await h.page.evaluate(() => document.documentElement.scrollWidth <= innerWidth + 1))
  } finally {await h.close()}
})


test('hidden ordinary Tree branch and terminal callbacks drain four slots without admitting queued work while Projects progresses', {timeout: 60000}, async () => {
  const h = await openDiscovery(undefined, values => {
    const make = (id, path) => ({...values[0], id, subjects: [file(path)], subject_kind: 'file', subject: path})
    return [...Array.from({length: 205}, (_, i) => make('terminal-' + i, 'a')),
      ...['a', 'b', 'c', 'd'].flatMap(prefix => Array.from({length: 225}, (_, i) => make(prefix + '-' + i, prefix + '/child-' + String(i).padStart(3, '0'))))]
  })
  const hierarchy = hierarchyFixture()
  h.fixture.projects = hierarchy.projects.slice(0, 3); h.fixture.tasks = hierarchy.tasks.slice(0, 3)
  const requests = [], held = []
  let holding = true, active = 0, maximum = 0
  try {
    await h.page.route('**/api/v1/observations**', async route => {
      const url = new URL(route.request().url())
      const branch = url.pathname.endsWith('/subject-tree')
      const terminal = url.pathname === '/api/v1/observations' && url.searchParams.has('subject')
      if (!branch && !terminal) return route.fallback()
      const receipt = {prefix: url.searchParams.get(branch ? 'prefix' : 'subject'), kind: terminal ? url.searchParams.get('subject_kind') : null, offset: Number(url.searchParams.get('offset')), done: false}
      requests.push(receipt); active++; maximum = Math.max(maximum, active)
      if (holding && receipt.prefix) await new Promise(resolve => held.push({release: resolve, receipt}))
      await route.fallback(); receipt.done = true; active--
    })
    await h.start(); await h.page.getByRole('button', {name: 'Subject Tree', exact: true}).click(); await h.idle()
    for (const path of ['a', 'b', 'c', 'd']) await toggle(h, path)
    await h.bounded((async () => { while (held.length < 4) await h.page.waitForTimeout(20) })(), 5000, 'Four held Tree owners')
    assert.equal(held.filter(value => value.receipt.kind).length, 1, 'A typed terminal shares the four physical slots')
    await pathRow(h, 'd').locator('.observation-tree-label').click(); await paint(h.page)
    await h.page.getByRole('button', {name: 'Projects', exact: true}).click(); await h.idle()
    const hiddenAt = requests.length
    for (const gate of held) {
      gate.release()
      await h.bounded((async () => { while (!gate.receipt.done) await h.page.waitForTimeout(20) })(), 5000, 'Exact hidden Tree callback')
      await h.idle(); await paint(h.page)
      assert.equal(requests.length, hiddenAt, 'A hidden completion must not replace its physical slot')
    }
    assert.equal(active, 0)
    assert.ok(h.requests.some(value => value.parent === 'root-project' && value.done), 'Project branches progress with Tree hidden')
    assert.ok(h.requests.some(value => /\/api\/v1\/(projects|tasks)\/[^/]+$/.test(value.path) && value.done), 'Planning detail work progresses')
    assert.ok(h.caps().maxBranches <= 4 && h.caps().maxDetails <= 6)
    holding = false
    await h.page.getByRole('button', {name: 'Observations', exact: true}).click(); await h.idle()
    await h.bounded((async () => { while (!requests.slice(hiddenAt).some(value => value.prefix === 'a' && value.kind === null && value.offset === 200 && value.done)) await h.page.waitForTimeout(20) })(), 5000, 'Visible branch continuation resumes')
    assert.ok(requests.slice(hiddenAt).some(value => value.prefix === 'd' && value.done), 'Previously queued expansion resumes')
    await revealObservationRow(h.page, pathRow(h, 'd'))
    assert.equal(await pathRow(h, 'd').getAttribute('aria-expanded'), 'true')
    assert.equal(await pathRow(h, 'd').getAttribute('aria-selected'), 'true')
    assert.ok(maximum <= 4)
    assert.ok(await h.page.locator('[data-observation-key]').count() <= 28)
  } finally { for (const gate of held) gate.release(); await h.close() }
})


test('hidden staged terminal refresh pauses continuation and retains scroll, selection and expansion until return', {timeout: 60000}, async () => {
  const h = await openDiscovery(undefined, values => Array.from({length: 405}, (_, i) => ({...values[0], id: 'staged-' + i, subjects: [file('a')], subject_kind: 'file', subject: 'a'})))
  let release, held = false
  const requests = []
  try {
    await h.start(); await h.page.getByRole('button', {name: 'Subject Tree', exact: true}).click(); await h.idle(); await toggle(h, 'a')
    const more = h.page.getByRole('button', {name: 'Load more observations', exact: true})
    await revealObservationRow(h.page, more); await more.click(); await h.idle()
    assert.ok(h.receipts.some(value => value.params.subject === 'a' && value.params.offset === '200'))
    await revealObservationRow(h.page, pathRow(h, 'a')); await pathRow(h, 'a').locator('.observation-tree-label').click()
    await h.page.locator('#main-content-scroll').evaluate(element => { element.scrollTop = 600 }); await paint(h.page)
    await h.page.route('**/api/v1/observations?**', async route => {
      const url = new URL(route.request().url())
      if (url.searchParams.get('subject') !== 'a') return route.fallback()
      const receipt = {offset: Number(url.searchParams.get('offset')), done: false}; requests.push(receipt)
      if (!held) { held = true; await new Promise(resolve => { release = resolve }) }
      await route.fallback(); receipt.done = true
    })
    await h.page.evaluate(workspace => window.pushHierarchyFrames([{schema_version: 1, type: 'change', event: {
      schema_version: 1, event_id: 'terminal-stage', scope: 'workspace', workspace_id: workspace, occurred_at: '2026-10-19T00:00:00Z',
      transaction: {id: 'terminal-stage-tx', cause: 'rest', request_id: null}, actor: {type: 'system', id: null}, entity: {type: 'observation', id: 'unloaded', action: 'updated'},
      invalidations: [{kind: 'collection', target: 'observations:' + workspace}]
    }}]), h.fixture.workspace.id)
    await h.bounded((async () => { while (!release) await h.page.waitForTimeout(20) })(), 5000, 'Staged terminal first page held')
    const savedScroll = await h.page.locator('#main-content-scroll').evaluate(element => element.scrollTop)
    const savedAnchor = await h.page.evaluate(() => {
      const edge = document.getElementById('main-content-scroll').getBoundingClientRect().top + 12
      const row = [...document.querySelectorAll('[data-observation-key]')].find(element => element.getBoundingClientRect().bottom > edge)
      return {key: row.dataset.observationKey, offset: edge - row.getBoundingClientRect().top}
    })
    await h.page.getByRole('button', {name: 'Projects', exact: true}).click(); await h.idle()
    release(); release = null; await h.idle(); await paint(h.page)
    assert.deepEqual(requests.map(value => value.offset), [0], 'Hidden staged success does not fetch terminal page two')
    const treeBefore = h.receipts.filter(value => value.endpoint.endsWith('/subject-tree')).length
    await h.page.evaluate(() => window.pushHierarchyFrames([{schema_version: 1, type: 'resync_required'}])); await h.idle()
    assert.equal(h.receipts.filter(value => value.endpoint.endsWith('/subject-tree')).length, treeBefore, 'Hidden canonical invalidation defers ordinary root work')
    assert.match(await h.page.locator('.workspace-header .workspace-observation-total').textContent(), /405 observations/)
    const resumedAt = requests.length
    await h.page.getByRole('button', {name: 'Observations', exact: true}).click(); await h.idle(); await paint(h.page)
    assert.ok(requests.slice(resumedAt).some(value => value.offset === 200 && value.done), 'Return resumes current staged terminal span')
    const restoredScroll = await h.page.locator('#main-content-scroll').evaluate(element => element.scrollTop)
    const restoredAnchor = await h.page.locator('[data-observation-key=' + JSON.stringify(savedAnchor.key) + ']').evaluate(element => ({key: element.dataset.observationKey, offset: document.getElementById('main-content-scroll').getBoundingClientRect().top + 12 - element.getBoundingClientRect().top}))
    assert.ok(Math.abs(restoredAnchor.offset - savedAnchor.offset) < 3, 'Explorer row and intrarow scroll survive the hidden refresh: ' + JSON.stringify({savedScroll, restoredScroll, savedAnchor, restoredAnchor}))
    await revealObservationRow(h.page, pathRow(h, 'a'))
    assert.equal(await pathRow(h, 'a').getAttribute('aria-expanded'), 'true')
    assert.equal(await pathRow(h, 'a').getAttribute('aria-selected'), 'true')
    assert.ok(await h.page.locator('[data-observation-key]').count() <= 28)
  } finally { release?.(); await h.close() }
})


for (const width of [1440, 320]) test(`connector cards ${width}: logical typed owners, decorative gutters and accessible depth`, {timeout: 60000}, async () => {
  const h = await openDiscovery({width: 1440, height: 1000}, examples)
  try {
    await tree(h)
    await toggle(h, 'src'); await toggle(h, 'src/auth'); await toggle(h, 'docs')
    await revealObservationRow(h.page, pathRow(h, 'src/auth'))
    await pathRow(h, 'src/auth').locator('.observation-tree-label').click(); await paint(h.page)
    const wrapper = pathRow(h, 'src/auth').locator('..').locator('..')
    assert.equal(await wrapper.getAttribute('data-tree-parent'), 'tree-path:src')
    assert.equal(await pathRow(h, 'src/auth').getAttribute('aria-level'), '2')
    assert.equal(await pathRow(h, 'src/auth').getAttribute('aria-selected'), 'true')
    assert.equal(await h.page.locator('.observation-tree-depth').count(), 0)
    assert.ok(!/Level\s+\d+/.test(await h.page.locator('#observation-panel').innerText()))
    assert.ok(await h.page.locator('.observation-tree-pattern, .observation-tree-path .observation-loaded-count').evaluateAll(elements => elements.every(element => element.scrollWidth <= element.clientWidth + 1)), 'Path metadata remains readable within deep/narrow cards')
    const root = pathRow(h, 'src').locator('..').locator('..')
    assert.equal(await root.getAttribute('data-tree-parent'), '')
    assert.equal(await root.locator('.tree-rail-incoming').count(), 0)
    const typed = h.page.getByRole('region', {name: 'File observations for src/auth', exact: true})
    await typed.scrollIntoViewIfNeeded(); await paint(h.page)
    const header = typed.locator('..').locator('..')
    assert.equal(await header.getAttribute('data-tree-parent'), 'tree-path:src/auth')
    const cards = h.page.locator('.observation-tree-presentation[data-tree-parent="tree-terminal:file:src/auth"]')
    assert.ok(await cards.count() > 0, 'Direct cards belong to their typed header')
    assert.ok(await h.page.getByRole('region', {name: 'Glob observations for src/auth', exact: true}).count() > 0)
    assert.ok(await h.page.locator('.observation-tree-connectors').evaluateAll(elements => elements.every(element => element.getAttribute('aria-hidden') === 'true' && getComputedStyle(element).pointerEvents === 'none' && !element.querySelector('button,a,input,[tabindex]'))))
    await pathRow(h, 'src/auth').focus(); await paint(h.page)
    await h.page.setViewportSize({width, height: 1000})
    if (width === 320) await h.page.addStyleTag({content: 'html {font-size: 200%;}'})
    await pathRow(h, 'src/auth').scrollIntoViewIfNeeded(); await paint(h.page)
    const elbows = await h.page.locator('.observation-tree-elbow').evaluateAll(elements => elements.map(element => {
      const bounds = element.getBoundingClientRect(), content = element.closest('.observation-tree-presentation').querySelector('.observation-tree-row-content').getBoundingClientRect()
      return {width: bounds.width, left: bounds.left, right: bounds.right, contentLeft: content.left}
    }))
    assert.ok(elbows.length > 0 && elbows.every(value => value.width > 0 && value.right <= value.contentLeft + 1), 'Connectors stay in the common responsive gutter: ' + JSON.stringify(elbows))
    const requests = h.receipts.length; await paint(h.page); await paint(h.page)
    assert.equal(h.receipts.length, requests, 'Decoration creates no reads')
    assert.ok(await h.page.locator('[data-observation-key]').count() <= 28)
    const overflow = await h.page.evaluate(() => {const host = document.getElementById('main-content-scroll'); return {document: document.documentElement.scrollWidth, width: innerWidth, host: host.scrollWidth, client: host.clientWidth, offenders: [...host.querySelectorAll('*')].filter(element => element.getBoundingClientRect().right > innerWidth + 1).slice(0, 8).map(element => ({class: element.className, right: element.getBoundingClientRect().right, text: element.textContent.slice(0, 60)}))}})
    assert.ok(overflow.document <= overflow.width + 1 && overflow.host <= overflow.client + 1, JSON.stringify(overflow))
    if (process.env.HMEM_TREE_SCREENSHOT_DIR) {
      await mkdir(process.env.HMEM_TREE_SCREENSHOT_DIR, {recursive: true})
      await h.page.screenshot({path: join(process.env.HMEM_TREE_SCREENSHOT_DIR, (width === 1440 ? 'desktop.png' : 'narrow.png'))})
    }
  } finally {await h.close()}
})


test('connector virtual gaps clip parent-owned rails across roots and preserve unknown paginated siblings with distant pins', {timeout: 60000}, async () => {
  const h = await openDiscovery(undefined, values => ['alpha', 'beta'].flatMap(prefix => Array.from({length: 450}, (_, i) => ({...values[0], id: prefix + '-' + i,
    subject_kind: 'file', subject: prefix + '/child-' + String(i).padStart(3, '0'), subjects: [file(prefix + '/child-' + String(i).padStart(3, '0'))]}))).concat([{...values[0], id: 'unrelated', subjects: [file('az')], subject_kind: 'file', subject: 'az'}]))
  let release, waiting
  try {
    await h.start(); await h.page.getByRole('button', {name: 'Subject Tree', exact: true}).click(); await h.idle()
    await h.page.route('**/api/v1/observations/subject-tree?**', async route => {
      const url = new URL(route.request().url())
      if (url.searchParams.get('prefix') !== 'alpha' || url.searchParams.get('offset') !== '200' || waiting) return route.fallback()
      waiting = true; await new Promise(resolve => {release = resolve}); await route.fallback()
    })
    await toggle(h, 'alpha')
    await h.bounded((async () => {while (!release) await h.page.waitForTimeout(20)})(), 5000, 'Unknown second branch page held')
    await revealObservationRow(h.page, pathRow(h, 'alpha/child-199').locator('.observation-tree-copy'))
    const last = pathRow(h, 'alpha/child-199').locator('..').locator('..')
    assert.equal(await last.getAttribute('data-tree-following'), 'unknown')
    assert.equal(await last.locator('.tree-rail-following').evaluate(element => getComputedStyle(element).borderLeftStyle), 'dashed')
    release(); release = null; await h.idle()
    await revealObservationRow(h.page, pathRow(h, 'alpha/child-449').locator('.observation-tree-copy'))
    const end = pathRow(h, 'alpha/child-449').locator('..').locator('..')
    assert.equal(await end.getAttribute('data-tree-following'), 'end')
    assert.equal(await end.locator('.tree-rail-following').count(), 0)
    await toggle(h, 'beta')
    await revealObservationRow(h.page, pathRow(h, 'alpha')); await pathRow(h, 'alpha').focus(); await paint(h.page)
    // An actual scroll keeps the distant native focus pin while moving the ordinary window.
    await h.page.locator('#main-content-scroll').hover(); await h.page.mouse.wheel(0, 52000); await paint(h.page)
    await h.bounded((async () => {while (!(await h.page.locator('.observation-viewport-spacer').evaluateAll(elements => elements.some(element => element.querySelector('[data-tree-owner="tree-path:alpha"]') && element.querySelector('[data-tree-owner="tree-path:beta"]'))))) {await h.page.mouse.wheel(0, -1500); await paint(h.page)}})(), 10000, 'Gap spanning distinct parent owners')
    const gap = h.page.locator('.observation-viewport-spacer').filter({has: h.page.locator('[data-tree-owner="tree-path:alpha"]')}).filter({has: h.page.locator('[data-tree-owner="tree-path:beta"]')}).first()
    const rails = await gap.locator('.tree-rail-gap').evaluateAll(elements => elements.map(element => ({owner: element.dataset.treeOwner, top: parseFloat(element.style.top), height: parseFloat(element.style.height)})))
    const alpha = rails.find(value => value.owner === 'tree-path:alpha'), beta = rails.find(value => value.owner === 'tree-path:beta')
    assert.ok(alpha && beta && alpha.top + alpha.height < beta.top, 'No line joins through the unrelated root or beta parent card: ' + JSON.stringify(rails))
    assert.ok(await pathRow(h, 'alpha').count() > 0, 'Distant native focus remains pinned')
    assert.equal(await pathRow(h, 'beta').count(), 0, 'Parent may stay offscreen while its rail remains truthful')
    assert.ok(await h.page.locator('.observation-tree-presentation[data-tree-parent="tree-path:beta"] .tree-rail-incoming').count() > 0)
    assert.ok(await h.page.locator('[data-observation-key]').count() <= 28)
    assert.ok(await h.page.locator('.tree-rail-gap').count() <= 6, 'Omitted middle trees do not create decorative DOM')
    assert.ok(await h.page.locator('.tree-rail-gap').evaluateAll(elements => elements.every(element => element.closest('[aria-hidden="true"]') && getComputedStyle(element).pointerEvents === 'none' && !element.hasAttribute('tabindex'))))
    assert.deepEqual(h.errors, [])
  } finally {release?.(); await h.close()}
})
