import assert from 'node:assert/strict'
import { queryObservations } from '../../perf/fixtures.mjs'

const uuid = value => /^[0-9a-f]{8}(-[0-9a-f]{4}){3}-[0-9a-f]{12}$/i.test(value)
const slice = (items, offset, limit) => ({ items: items.slice(offset, offset + limit), has_more: offset + limit < items.length })
const ordered = values => [...values].sort((a, b) => b.updated_at.localeCompare(a.updated_at) || b.id.localeCompare(a.id))

// Controlled server examples. Actual SQL/rank/provider equivalence belongs to
// backend qualification; browser receipts always describe the real POST wire.
export function createObservationDiscoveryAdapter(fixtures, { allObservations, actor = () => 'fixture-user' }) {
  const contexts = new Map(), generations = new Map(), receipts = []
  let nextToken = 1, mode = 'fts_only', reason = 'disabled', semantic = () => false, maximum = 0
  const failure = (status, receipt) => ({ status, body: { error: status === 409 ? 'discovery_restart_required' : 'discovery_capacity' }, receipt })
  const adapter = {
    contexts, receipts, configure(value) { mode = value.mode || mode; reason = value.reason === undefined ? reason : value.reason; semantic = value.semantic || semantic },
    expire(token) { const context = contexts.get(token); assert.ok(context, 'Known fixture token'); context.retired = true },
    get maximumContexts() { return maximum },
    handle(payload) {
      const receipt = { endpoint: '/api/v1/observations/discovery', method: 'POST', payload: structuredClone(payload), done: false }
      receipts.push(receipt)
      const fixture = fixtures.find(value => value.workspace.id === payload.workspace_id)
      assert.ok(fixture, 'Discovery belongs to a registered fixture workspace')
      assert.equal(payload.limit, 50, 'Production discovery uses bounded50-item pages')
      assert.ok(Number.isInteger(payload.offset) && payload.offset >= 0)
      let context
      if (payload.context_token) {
        assert.ok(Object.keys(payload).every(key => ['workspace_id', 'context_token', 'view', 'prefix', 'terminal', 'limit', 'offset'].includes(key)), 'Continuation must omit all creation fields')
        context = contexts.get(payload.context_token)
        if (!context || context.retired || context.workspace !== payload.workspace_id || context.actor !== actor()) return failure(409, receipt)
      } else {
        assert.ok(uuid(payload.search_session), 'Dedicated discovery session is a UUID')
        assert.ok(Number.isInteger(payload.generation) && payload.generation > 0 && payload.generation <= 2147483647)
        assert.ok(payload.filters && typeof payload.filters.subject_query === 'string' && typeof payload.filters.content_query === 'string')
        const key = [actor(), payload.workspace_id, payload.search_session].join(':')
        if ((generations.get(key) || 0) >= payload.generation) return failure(409, receipt)
        // Reserve the generation before a response gate can delay completion.
        generations.set(key, payload.generation)
        for (const old of contexts.values()) if (old.key === key) old.retired = true
        const body = payload.filters.content_query
        context = { key, workspace: payload.workspace_id, actor: actor(), filters: structuredClone(payload.filters), generation: payload.generation,
          token: 'fixture-discovery-' + nextToken++, retired: false, retrieval: { mode: body ? mode : 'fts_only', reason: body ? (mode === 'hybrid' ? null : reason) : 'empty_query' }, semantic }
        contexts.set(context.token, context)
        maximum = Math.max(maximum, [...contexts.values()].filter(value => !value.retired).length)
      }
      const filters = context.filters
      const all = allObservations(fixture, {})
      const subjects = observation => observation.subjects.filter(subject => (!filters.subject_kind || subject.subject_kind === filters.subject_kind)
        && (!filters.subject || subject.subject === filters.subject) && (!filters.subject_query || subject.subject.includes(filters.subject_query)))
      const scoped = allObservations(fixture, { gitSha: filters.git_sha, currentGitSha: filters.current_git_sha, historyGitSha: filters.history_git_sha }).filter(value => subjects(value).length)
      const fts = []
      // The shared pure helper retains its legacy200-page cap; collect the
      // controlled candidate set before slicing this new50-item wire response.
      for (let offset = 0; offset <= scoped.length; offset += 200) {
        const page = queryObservations({ ...fixture, observations: scoped }, { query: filters.content_query || null, queryScope: 'content', limit: 200, offset })
        fts.push(...page.items)
        if (!page.has_more) break
      }
      const similar = context.retrieval.mode === 'hybrid' ? ordered(scoped.filter(value => context.semantic(filters.content_query, value))) : []
      const scores = new Map()
      for (const component of [fts, similar]) component.forEach((value, rank) => scores.set(value.id, (scores.get(value.id) || 0) + 1 / (61 + rank)))
      const values = ordered(scoped.filter(value => scores.has(value.id))).sort((a, b) => scores.get(b.id) - scores.get(a.id))
      const counts = { workspace_id: fixture.workspace.id, total_count: new Set(all.map(value => value.id)).size, match_count: new Set(values.map(value => value.id)).size }
      let page
      if(payload.view==='count') page={items:[],has_more:false,counts,tree:null}
      else if (payload.view === 'tree') {
        const tree = discoveryTreePage(values, subjects, payload.prefix || '', payload.offset, payload.limit, counts)
        page = { items: [], has_more: tree.has_more, counts, tree }
      } else {
        const rows = payload.terminal ? values.filter(value => subjects(value).some(subject => subject.subject_kind === payload.terminal.subject_kind && subject.subject === payload.terminal.subject)) : values
        page = { ...slice(rows, payload.offset, payload.limit), counts, tree: null }
      }
      const provenance = {...(filters.git_sha?{original_git_sha:filters.git_sha}:{}),...(filters.current_git_sha?{current_git_sha:filters.current_git_sha}:{}),...(filters.history_git_sha?{history_git_sha:filters.history_git_sha}:{})}
      if(Object.keys(provenance).length) page.items=page.items.map(value=>({...value,provenance_match:provenance}))
      const body = { context_token: context.token, expires_at: '2099-01-01T00:00:00Z', retrieval: context.retrieval, complete: true, page }
      receipt.response = body
      return { status: 200, body, receipt }
    }
  }
  return adapter
}

export function discoveryTreePage(observations, subjects, prefix, offset, limit, counts) {
  const base = prefix ? prefix + '/' : '', children = new Map()
  for (const observation of observations) for (const terminal of subjects(observation)) {
    if (!terminal.subject.startsWith(base) || terminal.subject === prefix) continue
    const segment = terminal.subject.slice(base.length).split('/')[0], path = base + segment
    if (!children.has(path)) children.set(path, { path, segment, ids: new Set(), terminals: new Map(), has_children: false })
    const node = children.get(path); node.ids.add(observation.id)
    if (terminal.subject === path) {
      const key = terminal.subject_kind + ':' + terminal.subject
      if (!node.terminals.has(key)) node.terminals.set(key, { subject_kind: terminal.subject_kind, subject: path, ids: new Set() })
      node.terminals.get(key).ids.add(observation.id)
    } else node.has_children = true
  }
  const nodes = [...children.values()].sort((a, b) => Buffer.compare(Buffer.from(a.segment), Buffer.from(b.segment))).map(node => ({ path: node.path, segment: node.segment, has_children: node.has_children, observation_count: node.ids.size,
    terminals: [...node.terminals.values()].sort((a, b) => a.subject_kind.localeCompare(b.subject_kind)).map(value => ({ subject_kind: value.subject_kind, subject: value.subject, observation_count: value.ids.size })) }))
  return { ...slice(nodes, offset, limit), total_count: counts.total_count, match_count: counts.match_count }
}
