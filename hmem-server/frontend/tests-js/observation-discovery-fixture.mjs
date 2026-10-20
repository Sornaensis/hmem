import assert from 'node:assert/strict'
import { hierarchyFixture, openHierarchy, fixtureObservationCounts, allFixtureObservations } from './hierarchy-fixture.mjs'
import { queryObservations, queryObservationFacets } from '../perf/fixtures.mjs'

// Controlled concrete examples, not a replacement for the server glob matcher.
const patterns = new Map([
  ['src/**/*.elm', /^src\/(?:[^/]+\/)*[^/]+\.elm$/],
  ['src/**/*.js', /^src\/(?:[^/]+\/)*[^/]+\.js$/],
  ['docs/**/*.md', /^docs\/(?:[^/]+\/)*[^/]+\.md$/]
])

export function subjectTreePage(fixture, options) {
  const rows = allFixtureObservations(fixture, { gitSha: options.gitSha, currentGitSha: options.currentGitSha, historyGitSha: options.historyGitSha })
  const terminals = rows.flatMap(observation => observation.subjects.filter(subject => (!options.subjectKind || subject.subject_kind === options.subjectKind)
    && (!options.subjectQuery || subject.subject.includes(options.subjectQuery))).map(subject => ({ ...subject, id: observation.id })))
  const prefix = options.prefix ? options.prefix + '/' : ''
  const children = new Map()
  for (const terminal of terminals) {
    if (!terminal.subject.startsWith(prefix) || terminal.subject === options.prefix) continue
    const segment = terminal.subject.slice(prefix.length).split('/')[0], path = prefix + segment
    if (!children.has(path)) children.set(path, { path, segment, ids: new Set(), terminals: new Map(), has_children: false })
    const node = children.get(path); node.ids.add(terminal.id)
    if (terminal.subject === path) {
      const key = terminal.subject_kind + ':' + terminal.subject
      if (!node.terminals.has(key)) node.terminals.set(key, { subject_kind: terminal.subject_kind, subject: path, ids: new Set() })
      node.terminals.get(key).ids.add(terminal.id)
    } else node.has_children = true
  }
  const items = [...children.values()].sort((a, b) => Buffer.compare(Buffer.from(a.segment), Buffer.from(b.segment))).map(node => ({ path: node.path, segment: node.segment, has_children: node.has_children, observation_count: node.ids.size,
    terminals: [...node.terminals.values()].sort((a, b) => a.subject_kind.localeCompare(b.subject_kind)).map(value => ({ subject_kind: value.subject_kind, subject: value.subject, observation_count: value.ids.size })) }))
  return { items: items.slice(options.offset, options.offset + options.limit), has_more: options.offset + options.limit < items.length,
    total_count: new Set(fixture.observations.map(value => value.id)).size, match_count: new Set(terminals.map(value => value.id)).size }
}
export async function openDiscovery(viewport = { width: 1440, height: 900 }, transformObservations = values => values, additionalFixtures = [], launchOptions = {}) {
  const fixture = hierarchyFixture()
  fixture.projects = []; fixture.tasks = []
  const sha = '0123456789abcdef0123456789abcdef01234567'
  const observation = (id, file, glob, content, index) => ({ id, workspace_id: fixture.workspace.id,
    subjects: [{ subject_kind: 'file', subject: file }, { subject_kind: 'glob', subject: glob }],
    subject_kind: 'file', subject: file, git_sha: sha, content,
    content_version: '10000000-0000-4000-8000-' + String(index).padStart(12, '0'),
    latest_sequence: 1, current_provenance: null,
    created_at: '2026-01-01T00:00:00Z', updated_at: '2026-01-01T00:00:00Z' })
  fixture.observations = [
    observation('cache-main', 'src/Main.elm', 'src/**/*.elm', 'Cache evidence for Main', 1),
    observation('cache-view', 'src/View.elm', 'src/**/*.elm', 'Cache evidence for View', 2),
    observation('other-js', 'src/Other.js', 'src/**/*.js', 'Unrelated JavaScript evidence', 3),
    ...Array.from({ length: 61 }, (_, index) => observation('guide-' + index, 'docs/Guide-' + String(index).padStart(3, '0') + '.md', 'docs/**/*.md', 'Documentation guide ' + index, index + 4))
  ]
  fixture.observations = transformObservations(fixture.observations)
  const h = await openHierarchy(fixture, additionalFixtures, null, {}, launchOptions)
  await h.page.setViewportSize(viewport)
  const receipts = h.discovery.receipts
  await h.page.route('**/api/v1/observations**', async route => {
    const request = route.request(), url = new URL(request.url())
    if (url.pathname === '/api/v1/observations/discovery') { await route.fallback(); return }
    const scopedFixture = [fixture, ...additionalFixtures].find(value => value.workspace.id === url.searchParams.get('workspace_id')) || fixture
    const receipt = { endpoint: url.pathname, method: request.method(), params: Object.fromEntries(url.searchParams), done: false }
    receipts.push(receipt)
    try {
      let body
      const options = { query: url.searchParams.get('query'), subjectKind: url.searchParams.get('subject_kind'), subject: url.searchParams.get('subject'), gitSha: url.searchParams.get('git_sha'), currentGitSha: url.searchParams.get('current_git_sha'), historyGitSha: url.searchParams.get('history_git_sha'), offset: Number(url.searchParams.get('offset') || 0), limit: Number(url.searchParams.get('limit') || 50) }
      const contentScope = (source, query) => url.searchParams.get('query_scope') !== 'content' || !query ? source
        : { ...source, observations: source.observations.filter(value => query.toLowerCase().split(/\s+/).every(token => value.content.toLowerCase().includes(token))) }
      if (url.pathname.endsWith('/count')) {
        assert.equal(request.method(), 'POST')
        receipt.payload = request.postDataJSON()
        body = fixtureObservationCounts(contentScope(scopedFixture, receipt.payload.query), receipt.payload)
        body.total_count = scopedFixture.observations.filter(value => value.workspace_id === receipt.payload.workspace_id).length
      } else if (url.pathname.endsWith('/match')) {
        assert.equal(request.method(), 'POST')
        const query = request.postDataJSON(); receipt.payload = query; options.gitSha = query.git_sha; options.currentGitSha = query.current_git_sha; options.historyGitSha = query.history_git_sha
        const candidates = allFixtureObservations(fixture, { query: query.query, gitSha: query.git_sha, currentGitSha: query.current_git_sha, historyGitSha: query.history_git_sha })
        const matched = candidates.flatMap(observation => {
          const path_matches = query.paths.flatMap(path => {
            const matched_subjects = observation.subjects.filter(subject => (!query.subject_kind || subject.subject_kind === query.subject_kind) && (subject.subject_kind === 'file' ? subject.subject === path : patterns.get(subject.subject)?.test(path)))
            return matched_subjects.length ? [{ path, matched_subjects }] : []
          })
          return path_matches.length ? [{ observation, path_matches }] : []
        })
        body = { items: matched.slice(query.offset, query.offset + query.limit), has_more: query.offset + query.limit < matched.length }
      } else if (url.pathname.endsWith('/subject-tree')) {
        assert.equal(request.method(), 'GET'); assert.equal(options.query, null)
        body = subjectTreePage(scopedFixture, { ...options, prefix: url.searchParams.get('prefix') || '', subjectQuery: url.searchParams.get('subject_query') })
      } else if (url.pathname.endsWith('/subject-facets')) body = queryObservationFacets(scopedFixture, options)
      else if (url.pathname === '/api/v1/observations') body = queryObservations(contentScope(scopedFixture, options.query), options)
      else {
        assert.equal(request.method(), 'GET')
        body = fixture.observations.find(value => value.id === decodeURIComponent(url.pathname.split('/').at(-1)))
        assert.ok(body, 'Unknown controlled observation')
      }
      if (options.gitSha || options.currentGitSha || options.historyGitSha) {
        const context = { ...(options.gitSha ? { original_git_sha: options.gitSha } : {}), ...(options.currentGitSha ? { current_git_sha: options.currentGitSha } : {}), ...(options.historyGitSha ? { history_git_sha: options.historyGitSha } : {}) };
        if (body.items) body.items = body.items.map(value => value.observation ? { ...value, observation: { ...value.observation, provenance_match: context } } : value.content ? { ...value, provenance_match: context } : value);
      }
      receipt.response = body
      await route.fulfill({ status: 200, contentType: 'application/json', body: JSON.stringify(body) })
    } catch (error) { h.errors.push(error.message); await route.abort().catch(() => {}) }
    finally { receipt.done = true }
  })
  return { ...h, receipts, sha,
    async start() { await h.page.goto(h.origin + '/workspace/' + fixture.workspace.id + '#tab=observations'); await h.page.locator('#observation-panel').waitFor(); await this.idle() },
    async idle() { await h.page.waitForFunction(()=>!document.querySelector('.observation-filter-pending') || !!document.querySelector('#observation-filter-validation')); await h.idle(); await h.bounded((async () => { while (receipts.some(value => !value.done)) await h.page.waitForTimeout(20) })(), 10000, 'Discovery HTTP quiet'); assert.deepEqual(h.errors, []) }
  }
}


// Resolve the immutable accepted predicate separately from raw POST receipts.
export function discoverySelection(h, receipt) {
  const payload = receipt.payload || receipt
  if (!payload || !('view' in payload)) return {}
  const filters = payload.filters || h.discovery.contexts.get(payload.context_token)?.filters || {}
  return {...filters, ...(payload.terminal || {}), prefix:payload.prefix || '', offset:payload.offset, limit:payload.limit, view:payload.view}
}

// Controlled route middleware owns a real discovery request/envelope. Reserve
// its generation before any callback delay; fallback sends that prepared reply.
export async function interceptDiscovery(h, handler) {
  await h.page.route('**/api/v1/observations/discovery', async route => {
    const outcome=h.discovery.handle(route.request().postDataJSON()), receipt=outcome.receipt
    const send=async(options={})=>{
      const status=options.status ?? outcome.status
      const body=options.json ?? (options.body ? JSON.parse(options.body) : outcome.body)
      receipt.status=status; receipt.response=body
      await route.fulfill({status,contentType:'application/json',body:JSON.stringify(body)})
    }
    try { await handler({request:()=>route.request(),fallback:()=>send(),fulfill:send,abort:()=>route.abort()},outcome) }
    catch(error) {h.errors.push(error.message);await route.abort().catch(()=>{})}
    finally {receipt.done=true}
  })
}
