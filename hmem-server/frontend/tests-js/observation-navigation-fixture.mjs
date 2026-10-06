import assert from 'node:assert/strict'
import { hierarchyFixture, openHierarchy } from './hierarchy-fixture.mjs'
import { queryObservations, queryObservationFacets } from '../perf/fixtures.mjs'

export async function openObservations(viewport = { width: 1440, height: 900 }) {
  const fixture = hierarchyFixture()
  fixture.projects = []; fixture.tasks = []
  const sha = '0123456789abcdef0123456789abcdef01234567'
  const path = 'src/' + 'long-repository-segment/'.repeat(8) + 'Main.elm'
  const observation = index => {
    const subjects = [{ subject_kind: 'file', subject: path }, { subject_kind: 'glob', subject: 'src/**/*.elm' }]
    return { id: 'observation-' + index, workspace_id: fixture.workspace.id, subjects,
      subject_kind: subjects[0].subject_kind, subject: path, git_sha: sha,
      content_version: '10000000-0000-4000-8000-' + String(index).padStart(12, '0'),
      content: 'Observation ' + index + '\n' + 'Long readable content '.repeat(30),
      created_at: '2026-01-01T00:00:00Z', updated_at: '2026-01-01T00:00:00Z' }
  }
  fixture.observations = Array.from({ length: 40 }, (_, index) => observation(index))
  const offPage = observation(99)
  const h = await openHierarchy(fixture)
  await h.page.setViewportSize(viewport)
  const receipts = [], controls = [], held = new Set()
  const values = new Map([...fixture.observations, offPage].map(value => [value.id, value]))
  let nextVersion = 100
  await h.page.route('**/api/v1/observations**', async route => {
    const request = route.request(), url = new URL(request.url()), endpoint = url.pathname
    const receipt = { endpoint, method: request.method(), offset: Number(url.searchParams.get('offset') || 0), done: false }
    receipts.push(receipt)
    try {
      let body, status = 200
      if (endpoint.endsWith('/match')) {
        const query = request.postDataJSON()
        const paths = query.paths
        assert.ok(paths.length)
        receipt.paths = paths
        body = { items: fixture.observations.filter(value => values.has(value.id)).slice(query.offset, query.offset + query.limit).map(value => ({ observation: values.get(value.id), path_matches: paths.map(path => ({ path, matched_subjects: value.subjects })) })), has_more: false }
      } else if (endpoint.endsWith('/subject-facets')) {
        body = queryObservationFacets(fixture, { query: url.searchParams.get('query'), subjectKind: url.searchParams.get('subject_kind'), gitSha: url.searchParams.get('git_sha'), offset: receipt.offset, limit: 50 })
      } else if (endpoint === '/api/v1/observations') {
        body = queryObservations({ ...fixture, observations: fixture.observations.filter(value => values.has(value.id)).map(value => values.get(value.id)) }, { query: url.searchParams.get('query'), subjectKind: url.searchParams.get('subject_kind'), subject: url.searchParams.get('subject'), gitSha: url.searchParams.get('git_sha'), offset: receipt.offset, limit: 50 })
      } else {
        const id = decodeURIComponent(endpoint.split('/').at(-1))
        body = values.get(id)
        if (!body) { status = 404; body = { error: 'Missing controlled observation' } }
        else if (request.method() === 'PUT') {
          const update = request.postDataJSON(); assert.deepEqual(Object.keys(update), ['content'])
          assert.equal(request.headers()['if-match'], '"' + body.content_version + '"')
          body = { ...body, content: update.content, content_version: '20000000-0000-4000-8000-' + String(nextVersion++).padStart(12, '0'), updated_at: '2026-01-02T00:00:00Z' }; values.set(id, body)
        } else if (request.method() === 'DELETE') { values.delete(id); body = {} }
        else assert.equal(request.method(), 'GET')
      }
      const control = controls.find(value => !value.used && value.match(receipt))
      if (control) {
        control.used = true; control.arrivedResolve(receipt)
        if (control.hold) { held.add(control.release); await control.wait; held.delete(control.release) }
        if (control.error) { status = control.error; body = { error: 'Controlled detail failure' } }
      }
      await route.fulfill({ status, contentType: 'application/json', body: JSON.stringify(body) })
    } catch (error) { h.errors.push(error.message); await route.abort().catch(() => {}) }
    finally { receipt.done = true }
  })
  return { ...h, path, receipts, values,
    holdDetail(id, error = null) {
      let release, arrivedResolve
      const wait = new Promise(resolve => { release = resolve }), arrived = new Promise(resolve => { arrivedResolve = resolve })
      controls.push({ match: request => request.endpoint.endsWith('/' + id), hold: true, error, wait, release, arrivedResolve, used: false })
      return { release, arrived }
    },
    async start(fragment = 'tab=observations') {
      await h.page.goto(h.origin + '/workspace/' + fixture.workspace.id + '#' + fragment)
      await h.page.locator('#observation-panel').waitFor()
      await this.idle()
    },
    async idle() {
      await h.idle()
      await h.bounded((async () => {
        while (receipts.some(value => !value.done)) await h.page.waitForTimeout(20)
      })(), 10000, 'Observation HTTP quiet')
      await h.page.waitForFunction(() => !document.querySelector('.loading-indicator'))
      assert.deepEqual(h.errors, [])
    },
    async close() { for (const release of held) release(); for (const control of controls) control.release(); await h.close() }
  }
}
