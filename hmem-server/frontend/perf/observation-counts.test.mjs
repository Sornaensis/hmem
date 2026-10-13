import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { generateFixture, fixtureHash, queryObservationCounts } from './fixtures.mjs'
import { perfApiRouteKey } from './contracts.mjs'
import { fixtureResponder, createTracker } from './harness.mjs'
import { generateObservationScalingFixture } from './observation-scaling.mjs'

function countFixture() {
  const fixture = generateFixture('small')
  const base = fixture.observations[0]
  const row = (id, subjects, extra = {}) => ({ ...base, id, subjects, content: 'shared repository evidence', git_sha: 'a'.repeat(40), current_provenance: { reviewed_git_sha: 'b'.repeat(40) }, revision_history: [{ reviewed_git_sha: 'c'.repeat(40) }], ...extra })
  fixture.observations = [
    row('overlap', [{ subject_kind: 'file', subject: 'src/Shared.elm' }, { subject_kind: 'glob', subject: 'src/**/*.elm' }]),
    row('glob', [{ subject_kind: 'glob', subject: 'src/*.elm' }]),
    row('other', [{ subject_kind: 'file', subject: 'tests/Other.elm' }], { content: 'different evidence' }),
    row('literal', [{ subject_kind: 'glob', subject: 'src/a.b?.elm' }]),
    row('foreign', [{ subject_kind: 'file', subject: 'src/Shared.elm' }], { workspace_id: 'foreign' })
  ]
  return fixture
}

test('Observation count DTO matches the production decoder and scopes total independently of filters', () => {
  const fixture = countFixture(), query = { workspace_id: fixture.workspace.id }
  assert.deepEqual(queryObservationCounts(fixture, query), { workspace_id: fixture.workspace.id, total_count: 4, match_count: 4 })
  assert.deepEqual(queryObservationCounts(fixture, { ...query, query: 'different' }), { workspace_id: fixture.workspace.id, total_count: 4, match_count: 1 })
  assert.deepEqual(queryObservationCounts(fixture, { workspace_id: 'foreign' }), { workspace_id: 'foreign', total_count: 1, match_count: 1 })
  assert.deepEqual(queryObservationCounts(fixture, { workspace_id: 'empty' }), { workspace_id: 'empty', total_count: 0, match_count: 0 })
  fixture.observations = []
  assert.deepEqual(queryObservationCounts(fixture, query), { workspace_id: fixture.workspace.id, total_count: 0, match_count: 0 })
  const decoder = fs.readFileSync('src/Api.elm', 'utf8').split('observationCountsDecoder =')[1].split('fetchObservationCounts :')[0]
  assert.match(decoder, /D\.field "workspace_id" D\.string/)
  assert.match(decoder, /D\.field "total_count" count/)
  assert.match(decoder, /D\.field "match_count" count/)
})

test('Observation counts exhaust all backing pages while the DTO remains constant size', () => {
  const fixture = generateFixture('large')
  const query = { workspace_id: fixture.workspace.id }
  assert.deepEqual(queryObservationCounts(fixture, query), { workspace_id: fixture.workspace.id, total_count: 2000, match_count: 2000 })
  assert.equal(queryObservationCounts(fixture, { ...query, query: 'Observation 02000' }).match_count, 1, 'last backing page contributes')
  assert.equal(Object.keys(queryObservationCounts(fixture, query)).length, 3)
  assert.ok(Buffer.byteLength(JSON.stringify(queryObservationCounts(fixture, query))) < 128)
  assert.equal(fixtureHash(fixture), '0c966ae9c5476317ba8f7f05ad0200a9c8526e16c430e6068a8a4daf9e35a077', 'count reads do not change frozen fixture rows')
})

test('Observation count composes search, same-subject filters and creation/current/history provenance', () => {
  const fixture = countFixture(), query = { workspace_id: fixture.workspace.id }
  assert.equal(queryObservationCounts(fixture, { ...query, query: 'shared evidence', subject_kind: 'file', subject: 'src/Shared.elm', git_sha: 'a'.repeat(40), current_git_sha: 'b'.repeat(40), history_git_sha: 'c'.repeat(40) }).match_count, 1)
  assert.equal(queryObservationCounts(fixture, { ...query, subject_kind: 'file', subject: 'src/**/*.elm' }).match_count, 0, 'kind and subject must belong to the same subject entry')
  for (const field of ['git_sha', 'current_git_sha', 'history_git_sha']) assert.equal(queryObservationCounts(fixture, { ...query, [field]: 'd'.repeat(40) }).match_count, 0, field)
  assert.equal(queryObservationCounts(fixture, { ...query, query: 'shared missing' }).match_count, 0, 'simple lexeme AND search stays authoritative')
  const ordinary = generateFixture('small'), creation = ordinary.observations[0].git_sha
  assert.equal(queryObservationCounts(ordinary, { workspace_id: ordinary.workspace.id, git_sha: creation }).match_count, 1)
  assert.equal(queryObservationCounts(ordinary, { workspace_id: ordinary.workspace.id, current_git_sha: creation }).match_count, 0, 'null current provenance must not inherit creation SHA')
  assert.equal(queryObservationCounts(ordinary, { workspace_id: ordinary.workspace.id, history_git_sha: creation }).match_count, 0, 'absent revision history must not inherit creation SHA')
})

test('Observation count uses concrete path OR with glob boundaries and counts overlapping matches once', () => {
  const fixture = countFixture(), query = { workspace_id: fixture.workspace.id }
  assert.equal(queryObservationCounts(fixture, { ...query, paths: ['src/Shared.elm', 'src/Shared.elm', 'src/nested/Child.elm'] }).match_count, 2)
  assert.equal(queryObservationCounts(fixture, { ...query, subject_kind: 'file', paths: ['src/Shared.elm', 'src/nested/Child.elm'] }).match_count, 1)
  assert.equal(queryObservationCounts(fixture, { ...query, subject_kind: 'glob', paths: ['src/nested/Child.elm'] }).match_count, 1, 'single star does not cross directories')
  assert.equal(queryObservationCounts(fixture, { ...query, paths: ['src/a.b1.elm'] }).match_count, 3, 'literal dot and question mark match correctly')
  assert.equal(queryObservationCounts(fixture, { ...query, paths: ['src/axb1.elm'] }).match_count, 2, 'literal dot is not a regex wildcard')
  assert.equal(queryObservationCounts(fixture, { ...query, paths: ['src/Shared.elm'], query: 'shared', git_sha: 'a'.repeat(40), current_git_sha: 'b'.repeat(40), history_git_sha: 'c'.repeat(40) }).match_count, 2)
  assert.equal(queryObservationCounts(fixture, { ...query, paths: ['SRC/Shared.elm'] }).match_count, 0, 'matching remains case-sensitive')
})

test('Observation glob question mark consumes one Unicode code point, including astral paths', () => {
  const fixture = countFixture()
  fixture.observations = [{ ...fixture.observations[0], subjects: [{ subject_kind: 'glob', subject: 'src/?.elm' }] }]
  const query = { workspace_id: fixture.workspace.id }
  assert.equal(queryObservationCounts(fixture, { ...query, paths: ['src/🐱.elm'] }).match_count, 1)
  assert.equal(queryObservationCounts(fixture, { ...query, paths: ['src/a.elm'] }).match_count, 1)
  assert.equal(queryObservationCounts(fixture, { ...query, paths: ['src/🐱🐱.elm'] }).match_count, 0)
  assert.equal(queryObservationCounts(fixture, { ...query, paths: ['src/ab.elm'] }).match_count, 0)
})

test('actual Observation count POST accounts exact bytes for ordinary and scaling fixtures without changing Match behavior', async () => {
  for (const fixture of [generateFixture('large'), generateObservationScalingFixture('large')]) {
    const tracker = createTracker(fixture), respond = fixtureResponder(fixture, tracker)
    const query = { workspace_id: fixture.workspace.id, query: 'Observation 02000' }
    let response
    const request = { url: () => 'http://perf.local/api/v1/observations/count', method: () => 'POST', postDataJSON: () => query }
    await respond({ request: () => request, fulfill: async value => { response = value } })
    assert.equal(response.status, 200)
    assert.deepEqual(JSON.parse(response.body), queryObservationCounts(fixture, query))
    assert.equal(tracker.counts['observations:count'], 1)
    assert.equal(tracker.bytes['observations:count'], Buffer.byteLength(response.body))
    assert.equal(tracker.active, 0)
    assert.equal(tracker.requests[0].done, true)
    assert.deepEqual(tracker.unhandledApiRoutes, [])
    if (!fixture.observationScaling) {
      const matchRequest = { url: () => 'http://perf.local/api/v1/observations/match', method: () => 'POST', postDataJSON: () => ({ workspace_id: fixture.workspace.id, paths: ['src/feature-0/module-0.elm'] }) }
      await respond({ request: () => matchRequest, fulfill: async value => { response = value } })
      assert.deepEqual(JSON.parse(response.body), { items: [], has_more: false }, 'historical ordinary Match limitation remains separate')
    }
  }
})

test('count key is exact POST; GET cannot use count semantics and unknown POST still fails the route gate', async () => {
  const url = 'http://perf.local/api/v1/observations/count'
  assert.equal(perfApiRouteKey(url, 'POST'), 'observations:count')
  assert.notEqual(perfApiRouteKey(url, 'GET'), 'observations:count')
  assert.equal(perfApiRouteKey(url + '-unknown', 'POST'), null)
  assert.equal(perfApiRouteKey(url + '/extra', 'POST'), null)
  const fixture = generateFixture('small'), tracker = createTracker(fixture), respond = fixtureResponder(fixture, tracker)
  let response
  await respond({ request: () => ({ url: () => url, method: () => 'GET', postDataJSON: () => { throw new Error('GET must not decode a count query') } }), fulfill: async value => { response = value } })
  assert.equal(response.status, 404)
  await respond({ request: () => ({ url: () => url + '-unknown', method: () => 'POST' }), fulfill: async value => { response = value } })
  assert.equal(response.status, 501)
  assert.equal(tracker.unhandledApiRoutes.length, 1)
  assert.equal(tracker.counts['observations:count'] || 0, 0)
})

test('failure phase is stamped before either deep-focus prerequisite guard', () => {
  const source = fs.readFileSync('perf/harness.mjs', 'utf8')
  const phase = source.indexOf("attemptState.phase = 'deep-focus prerequisite'")
  const stale = source.indexOf('await verifyStaleDeepFocusContinuation(browser, server.origin)')
  const ordinary = source.indexOf('await verifyDeepFocusContinuation(browser, server.origin)')
  assert.ok(phase > 0 && phase < stale && stale < ordinary)
  assert.match(source, /phase: attemptState\?\.phase/)
})
