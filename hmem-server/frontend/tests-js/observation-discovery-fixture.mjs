import assert from 'node:assert/strict'
import { hierarchyFixture, openHierarchy, fixtureObservationCounts, allFixtureObservations } from './hierarchy-fixture.mjs'
import { queryObservations, queryObservationFacets } from '../perf/fixtures.mjs'

// Controlled concrete examples, not a replacement for the server glob matcher.
const patterns = new Map([
  ['src/**/*.elm', /^src\/(?:[^/]+\/)*[^/]+\.elm$/],
  ['src/**/*.js', /^src\/(?:[^/]+\/)*[^/]+\.js$/],
  ['docs/**/*.md', /^docs\/(?:[^/]+\/)*[^/]+\.md$/]
])
export async function openDiscovery(viewport = { width: 1440, height: 900 }, transformObservations = values => values) {
  const fixture = hierarchyFixture()
  fixture.projects = []; fixture.tasks = []
  const sha = '0123456789abcdef0123456789abcdef01234567'
  const observation = (id, file, glob, content, index) => ({ id, workspace_id: fixture.workspace.id,
    subjects: [{ subject_kind: 'file', subject: file }, { subject_kind: 'glob', subject: glob }],
    subject_kind: 'file', subject: file, git_sha: sha, content,
    content_version: '10000000-0000-4000-8000-' + String(index).padStart(12, '0'),
    created_at: '2026-01-01T00:00:00Z', updated_at: '2026-01-01T00:00:00Z' })
  fixture.observations = [
    observation('cache-main', 'src/Main.elm', 'src/**/*.elm', 'Cache evidence for Main', 1),
    observation('cache-view', 'src/View.elm', 'src/**/*.elm', 'Cache evidence for View', 2),
    observation('other-js', 'src/Other.js', 'src/**/*.js', 'Unrelated JavaScript evidence', 3),
    ...Array.from({ length: 61 }, (_, index) => observation('guide-' + index, 'docs/Guide-' + String(index).padStart(3, '0') + '.md', 'docs/**/*.md', 'Documentation guide ' + index, index + 4))
  ]
  fixture.observations = transformObservations(fixture.observations)
  const h = await openHierarchy(fixture)
  await h.page.setViewportSize(viewport)
  const receipts = []
  await h.page.route('**/api/v1/observations**', async route => {
    const request = route.request(), url = new URL(request.url())
    const receipt = { endpoint: url.pathname, method: request.method(), params: Object.fromEntries(url.searchParams), done: false }
    receipts.push(receipt)
    try {
      let body
      const options = { query: url.searchParams.get('query'), subjectKind: url.searchParams.get('subject_kind'), subject: url.searchParams.get('subject'), gitSha: url.searchParams.get('git_sha'), offset: Number(url.searchParams.get('offset') || 0), limit: Number(url.searchParams.get('limit') || 50) }
      if (url.pathname.endsWith('/count')) {
        assert.equal(request.method(), 'POST')
        receipt.payload = request.postDataJSON()
        body = fixtureObservationCounts(fixture, receipt.payload)
      } else if (url.pathname.endsWith('/match')) {
        assert.equal(request.method(), 'POST')
        const query = request.postDataJSON(); receipt.payload = query
        const candidates = allFixtureObservations(fixture, { query: query.query, gitSha: query.git_sha })
        const matched = candidates.flatMap(observation => {
          const path_matches = query.paths.flatMap(path => {
            const matched_subjects = observation.subjects.filter(subject => (!query.subject_kind || subject.subject_kind === query.subject_kind) && (subject.subject_kind === 'file' ? subject.subject === path : patterns.get(subject.subject)?.test(path)))
            return matched_subjects.length ? [{ path, matched_subjects }] : []
          })
          return path_matches.length ? [{ observation, path_matches }] : []
        })
        body = { items: matched.slice(query.offset, query.offset + query.limit), has_more: query.offset + query.limit < matched.length }
      } else if (url.pathname.endsWith('/subject-facets')) body = queryObservationFacets(fixture, options)
      else if (url.pathname === '/api/v1/observations') body = queryObservations(fixture, options)
      else {
        assert.equal(request.method(), 'GET')
        body = fixture.observations.find(value => value.id === decodeURIComponent(url.pathname.split('/').at(-1)))
        assert.ok(body, 'Unknown controlled observation')
      }
      receipt.response = body
      await route.fulfill({ status: 200, contentType: 'application/json', body: JSON.stringify(body) })
    } catch (error) { h.errors.push(error.message); await route.abort().catch(() => {}) }
    finally { receipt.done = true }
  })
  return { ...h, receipts, sha,
    async start() { await h.page.goto(h.origin + '/workspace/' + fixture.workspace.id + '#tab=observations'); await h.page.locator('#observation-panel').waitFor(); await this.idle() },
    async idle() { await h.idle(); await h.bounded((async () => { while (receipts.some(value => !value.done)) await h.page.waitForTimeout(20) })(), 10000, 'Discovery HTTP quiet'); assert.deepEqual(h.errors, []) }
  }
}
