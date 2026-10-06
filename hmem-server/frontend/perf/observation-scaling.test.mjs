import test from 'node:test'
import assert from 'node:assert/strict'
import path from 'node:path'
import { generateFixture, fixtureHash, validateFixture, OBSERVATION_MEASURED_QUERY } from './fixtures.mjs'
import { OBSERVATION_SCALING_CONTRACT, generateObservationScalingFixture, queryObservationScalingMatches, observationScalingFrames, aggregateObservationScaling, observationScalingMetrics, observationTraceCaptureOptions, traceAdmissionReceipt } from './observation-scaling.mjs'
import { OBSERVATION_SCALING_BASE, OBSERVATION_SCALING_TASK, evidenceProfile, scalingRecordOutputs, assertEvidenceIdentity, assertScalingEvidenceWrite, prepareScalingScratch, createEvidenceOperations, checkQualification, startupEvidenceDisposition, persistOrRetainFailureDiagnostics } from './evidence-profile.mjs'
import { persistEvidenceAttempt, hashJson } from './contracts.mjs'
import { finalWorkingTreeEvidence } from './harness.mjs'

const frontend = path.resolve('fixture-root/hmem-server/frontend')
const temporary = path.resolve('outside-temporary')

test('scaling evidence binds every output to the fresh task and rejects identity/path fallback', () => {
  const profile = evidenceProfile(OBSERVATION_SCALING_BASE, frontend, temporary)
  assert.equal(profile.task.taskId, OBSERVATION_SCALING_TASK)
  assert.equal(profile.revision, 'observation-scaling.v1')
  assert.equal(new Set(Object.values(profile.files)).size, 5)
  for (const file of Object.values(profile.files)) {
    assert.equal(path.dirname(file), profile.root)
    assert.ok(path.basename(file).includes('observation-scaling.v1'))
  }
  assert.equal(path.dirname(path.dirname(profile.trace)), profile.root)
  const legacyOutput = path.join(frontend, 'perf/final-working-tree.after.v1.json')
  const legacyTrace = path.join(frontend, 'perf/final-working-tree.trace-manifest.v1.json')
  assert.deepEqual(scalingRecordOutputs(profile, legacyOutput, legacyTrace, legacyOutput, legacyTrace), { output: profile.files.after, traceManifest: profile.files.traceManifest })
  assert.deepEqual(scalingRecordOutputs(profile, profile.files.after, profile.files.traceManifest, legacyOutput, legacyTrace), { output: profile.files.after, traceManifest: profile.files.traceManifest })
  assert.throws(() => scalingRecordOutputs(profile, path.join(frontend, 'perf/baseline.v1.json'), legacyTrace, legacyOutput, legacyTrace), /task-owned/)
  assert.throws(() => evidenceProfile('894df9043da7ff0a1595ec7064e5761930d014f2', frontend, temporary), /Unmapped/)
  assert.throws(() => evidenceProfile(OBSERVATION_SCALING_BASE, frontend, frontend), /outside/)
  const identity = { taskId: OBSERVATION_SCALING_TASK, evidenceBaseCommit: OBSERVATION_SCALING_BASE, measurementRevision: profile.revision }
  assert.doesNotThrow(() => assertEvidenceIdentity(profile, identity))
  for (const field of Object.keys(identity)) assert.throws(() => assertEvidenceIdentity(profile, { ...identity, [field]: 'historical' }), /identity mismatch/)
  assert.doesNotThrow(() => assertScalingEvidenceWrite(profile, profile.files.after, 'bounded'))
  assert.throws(() => assertScalingEvidenceWrite(profile, legacyOutput, 'bounded'), /Unowned/)
  assert.throws(() => assertScalingEvidenceWrite(profile, profile.files.after, Buffer.alloc(64 * 1024 * 1024 + 1)), /64 MiB/)
})

test('historical evidence mappings keep their original files and task identities', () => {
  for (const [base, revision, task] of [
    ['818cc3cc634bfa8c0ebe14c13fc70b4c2d059e83', 'v1', null],
    ['04fd7ba26b1a63b5f1c601939b046ef2fe5f51d6', 'v2', '2503e08f-ff82-4f2c-adff-24e14fec8299'],
    ['1bdbe3d071d1fbbb994bc515841103adedff77ec', 'v3', '755a7286-6da5-494e-9b9b-fa1edd43d0b0'],
    ['d9ff753763f086fa4faef078b451f2e48ca1a7a4', 'expanded-hierarchy.v1', '555ebf6e-8442-43f4-a75d-c33fcffc76f4']
  ]) {
    const profile = evidenceProfile(base, frontend, temporary)
    assert.equal(profile.scaling, false)
    assert.equal(profile.revision, revision)
    assert.equal(profile.task?.taskId || null, task)
    assert.equal(profile.files.after, path.join(frontend, `perf/final-working-tree.after.${revision}.json`))
    assert.equal(profile.files.diff, path.join(frontend, 'perf', revision === 'expanded-hierarchy.v1' ? 'final-working-tree.complete.expanded-hierarchy.v1.diff' : 'final-working-tree.complete.diff'))
  }
})

test('deterministic long/boundary overlay is schema-valid without changing historical generators', () => {
  for (const size of ['small', 'large']) {
    const oldHash = fixtureHash(generateFixture(size))
    const fixture = generateObservationScalingFixture(size)
    assert.doesNotThrow(() => validateFixture(fixture))
    assert.equal(fixture.schemaVersion, 2)
    assert.equal(fixtureHash(fixture), fixtureHash(generateObservationScalingFixture(size)))
    assert.equal(fixtureHash(generateFixture(size)), oldHash)
    assert.equal(fixture.observations.length, size === 'small' ? 60 : 2000)
    assert.equal(Buffer.byteLength(fixture.observations.at(-1).content), 512 * 1024)
    assert.ok(fixture.observations.every(value => value.content.includes('\n')))
    assert.ok(fixture.observations.every(value => value.subjects.map(subject => subject.subject).join('|').endsWith('src/**/*.elm|src/Shared.elm')))
    const frames = observationScalingFrames(fixture)
    assert.equal(frames.length, 50)
    assert.ok(frames.every(value => value.event.entity.type === 'observation' && value.event.entity.id === fixture.observationScaling.boundaryId && value.event.invalidations[0].target === 'observation:' + fixture.observationScaling.boundaryId))
    assert.equal(new Set(frames.map(value => value.event.event_id)).size, 50)
  }
})

test('ordered Match pages preserve exact overlapping provenance rather than matching every subject', () => {
  const fixture = generateObservationScalingFixture('large')
  const query = { paths: OBSERVATION_SCALING_CONTRACT.paths, limit: 50, offset: 0 }
  const pages = [0, 50, 100].map(offset => queryObservationScalingMatches(fixture, { ...query, offset }))
  assert.deepEqual(pages.map(page => page.items.length), [50, 50, 50])
  assert.equal(new Set(pages.flatMap(page => page.items.map(value => value.observation.id))).size, 150)
  assert.ok(pages.every(page => page.has_more))
  assert.equal(pages[0].items[0].observation.id, fixture.observationScaling.boundaryId)
  const boundary = pages[0].items[0]
  assert.deepEqual(boundary.path_matches.map(value => value.path), query.paths)
  assert.deepEqual(boundary.path_matches[0].matched_subjects.map(value => value.subject), ['src/**/*.elm', 'src/Shared.elm'])
  assert.deepEqual(boundary.path_matches[1].matched_subjects.map(value => value.subject), ['src/**/*.elm'])
  const fileOnly = queryObservationScalingMatches(fixture, { ...query, subject_kind: 'file' })
  assert.ok(fileOnly.items.every(value => value.path_matches.every(match => match.matched_subjects.every(subject => subject.subject_kind === 'file' && subject.subject === match.path))))
  const filtered = queryObservationScalingMatches(fixture, { ...query, query: OBSERVATION_MEASURED_QUERY })
  assert.equal(filtered.items.length, 1)
  const shaOnly = queryObservationScalingMatches(fixture, { ...query, git_sha: boundary.observation.git_sha })
  assert.deepEqual(shaOnly.items.map(value => value.observation.id), [boundary.observation.id])
  const terminal = queryObservationScalingMatches(generateObservationScalingFixture('small'), { ...query, offset: 50 })
  assert.equal(terminal.items.length, 10)
  assert.equal(terminal.has_more, false)
  assert.throws(() => queryObservationScalingMatches(fixture, { ...query, paths: ['src/**/*.elm'] }), /concrete/)
})

test('Match requests remain unconditional while timing requires full comparability; research stays separate', () => {
  const runs = Array.from({ length: 5 }, (_, index) => ({ observationScaling: {
    pages: [{ count: 2, ms: 100 + index }, { count: 3, ms: 600 + index }],
    activeEditorHeapBytes: 70 * 1024 * 1024, live: { count: 13 }, researchTriggers: { activeEditorHeap: true, observationLiveFollowUps: true }
  } }))
  const summary = aggregateObservationScaling(runs)
  assert.equal(summary.matchLoadMore.p95Ms, 604)
  assert.deepEqual(summary.matchLoadMore.requestCounts, [3, 3, 3, 3, 3])
  const budget = { maxRequests: 2, maxP95Ms: 500 }
  assert.deepEqual(observationScalingMetrics(summary, budget, true).map(value => value.pass), [false, false])
  assert.deepEqual(observationScalingMetrics(summary, budget, false).map(value => value.pass), [false, true])
  assert.equal(summary.research.triggers.length, 5)
  assert.equal(observationScalingMetrics(summary, budget, true).length, 2)
  assert.throws(() => aggregateObservationScaling(runs.slice(1)), /five|5/i)
})

// In-memory filesystem faults exercise ownership without creating another scratch.
function scratchFixture() {
  const profile = evidenceProfile(OBSERVATION_SCALING_BASE, frontend, temporary)
  const entries = new Map([[temporary, { kind: 'directory', ino: 1 }], [profile.root, { kind: 'directory', ino: 2 }]])
  for (const file of Object.values(profile.files)) entries.set(file, { kind: 'file', ino: 3, size: 1 })
  const operations = []
  const missing = () => Object.assign(new Error('Missing virtual file'), { code: 'ENOENT' })
  const io = {
    existsSync: file => entries.has(file),
    realpathSync: file => { const info = entries.get(file); if (!info) throw missing(); return info.real || file },
    lstatSync: file => { const info = entries.get(file); if (!info) throw missing(); return { dev: 1, ino: info.ino, size: info.size || 0,
      isSymbolicLink: () => info.kind === 'symlink', isDirectory: () => info.kind === 'directory', isFile: () => info.kind === 'file' } },
    readdirSync: directory => [...entries.keys()].filter(file => path.dirname(file) === directory).map(file => path.basename(file)),
    mkdirSync: file => { operations.push(['mkdir', file]); entries.set(file, { kind: 'directory', ino: 4 }) },
    writeFileSync: (file, bytes) => { operations.push(['write', file]); entries.set(file, { kind: 'file', ino: 5, size: Buffer.byteLength(bytes), bytes: Buffer.from(bytes) }) },
    renameSync: (source, destination) => { operations.push(['rename', source, destination]); entries.set(destination, entries.get(source)); entries.delete(source) },
    unlinkSync: file => { operations.push(['remove', file]); if (!entries.delete(file)) throw missing() }
  }
  const owned = createEvidenceOperations(profile, temporary, io)
  const attempt = action => persistEvidenceAttempt({ manifestPath: profile.files.manifest, validationPath: profile.files.validation,
    verify: owned.verify, write: owned.write, remove: owned.remove,
    failureValidation: error => ({ passed: false, failure: error.message }), failureManifest: () => ({ qualificationPassed: false }) }, action)
  return { profile, entries, operations, io, owned, attempt }
}

test('normal working-tree producer persists a manifest accepted by the next startup identity check', () => {
  const h = scratchFixture()
  const baseline = Buffer.from('{"immutable":"baseline"}\n')
  for (const file of Object.values(h.profile.files)) h.entries.get(file).bytes = Buffer.from('{}')
  h.entries.get(h.profile.files.validation).bytes = Buffer.from('{"passed":true}')
  h.io.readFileSync = (file, encoding) => {
    const bytes = file === path.resolve('perf/baseline.v1.json') ? baseline : h.entries.get(file)?.bytes
    assert.ok(bytes, 'producer must read a known fixture file: ' + file)
    return encoding ? bytes.toString(encoding) : bytes
  }
  h.io.statSync = h.io.lstatSync
  const reads = []
  const capture = { read(args) {
    reads.push(args)
    if (args[0] === 'show') return baseline
    if (args[0] === 'rev-parse') return Buffer.from(h.profile.baseCommit + '\n')
    assert.ok(['diff', 'ls-files', 'rev-list'].includes(args[0]))
    return Buffer.alloc(0)
  } }
  prepareScalingScratch(h.profile, temporary, h.io)
  finalWorkingTreeEvidence({ plan: h.profile, io: h.io, write: h.owned.write, capture })
  const persisted = JSON.parse(h.io.readFileSync(h.profile.files.manifest, 'utf8'))
  assert.doesNotThrow(() => assertEvidenceIdentity(h.profile, persisted))
  assert.equal(persisted.measurementRevision, h.profile.revision)
  assert.equal(persisted.qualificationPassed, true)
  assert.equal(persisted.sourceProvenance.reviewBaseCommit, h.profile.baseCommit)
  assert.deepEqual(persisted.artifacts.map(value => value.path), [h.profile.files.after, h.profile.files.traceManifest, h.profile.files.diff, h.profile.files.validation])
  assert.ok(reads.some(args => args[0] === 'show' && args[1].startsWith(h.profile.baseCommit + ':')))
  assert.deepEqual(h.operations.map(value => value[0]), ['write', 'rename', 'remove', 'write', 'rename', 'remove'])
  const { measurementRevision, ...withoutRevision } = persisted
  assert.throws(() => assertEvidenceIdentity(h.profile, withoutRevision), /identity mismatch/, 'startup remains strict')
})

test('rejected, redirected, and replaced scratch never writes or removes foreign files', () => {
  for (const fault of ['unadmitted', 'symlink', 'parent-replaced', 'root-replaced']) {
    const h = scratchFixture()
    if (fault === 'unadmitted') {
      h.entries.get(h.profile.root).kind = 'symlink'
      assert.throws(() => prepareScalingScratch(h.profile, temporary, h.io), /symlink/)
    } else {
      prepareScalingScratch(h.profile, temporary, h.io)
      if (fault === 'symlink') Object.assign(h.entries.get(h.profile.root), { kind: 'symlink', real: path.resolve('foreign') })
      if (fault === 'parent-replaced') h.entries.get(temporary).ino = 100
      if (fault === 'root-replaced') h.entries.get(h.profile.root).ino = 100
    }
    assert.throws(() => h.owned.write(h.profile.files.validation, '{}'), /ownership|symlink|lifetime/)
    assert.throws(() => h.owned.remove(h.profile.files.manifest), /ownership|symlink|lifetime/)
    assert.throws(() => h.attempt(() => { throw new Error('cleanup failed') }), /ownership|symlink|lifetime/)
    assert.deepEqual(h.operations, [], fault + ' must not mutate rejected storage')
  }
})

test('failure fallback stops when admitted parent ownership changes during the action', () => {
  const h = scratchFixture()
  prepareScalingScratch(h.profile, temporary, h.io)
  let atRejection
  assert.throws(() => h.attempt(() => {
    h.entries.get(temporary).ino = 999
    atRejection = h.operations.length
    throw new Error('owned cleanup rejected')
  }), /lifetime/)
  assert.equal(h.operations.length, atRejection, 'no fallback write/removal after rejection')
  assert.ok(h.entries.has(h.profile.files.validation), 'foreign validation remains intact')
})

test('atomic evidence checks ownership again before rename and cleanup', () => {
  const h = scratchFixture()
  prepareScalingScratch(h.profile, temporary, h.io)
  const write = h.io.writeFileSync
  h.io.writeFileSync = (file, bytes) => { write(file, bytes); h.entries.get(h.profile.root).ino = 999 }
  assert.throws(() => h.owned.write(h.profile.files.validation, '{}'), /lifetime/)
  assert.deepEqual(h.operations.map(operation => operation[0]), ['write'])
})

test('admitted atomic writes and ordinary failure receipts retain bounded owned paths', () => {
  const h = scratchFixture()
  prepareScalingScratch(h.profile, temporary, h.io)
  h.owned.write(h.profile.files.validation, '{}')
  assert.ok(h.entries.has(h.profile.files.validation))
  assert.equal([...h.entries.keys()].filter(file => file.endsWith('.tmp')).length, 0)
  assert.throws(() => h.attempt(() => { throw new Error('assertion failed') }), /assertion failed/)
  assert.ok(h.entries.has(h.profile.files.validation))
  assert.ok(h.entries.has(h.profile.files.manifest))
  assert.ok(h.operations.every(operation => operation.slice(1).every(file => path.dirname(file) === h.profile.root)))
})

test('complete qualification rejects a failed record even when a noncomparable check passes', () => {
  const commands = [{ command: 'npm run perf:self-check', exitCode: 0 }, { command: 'node perf/harness.mjs record', exitCode: 0 }, { command: 'node perf/harness.mjs check', exitCode: 0 }]
  const recordEvaluation = { passed: false, comparableEnvironment: true, metrics: [{ pass: false, category: 'pinned-environment' }] }
  const checkEvaluation = { passed: true, comparableEnvironment: false, metrics: [{ pass: true, category: 'informational' }] }
  const before = JSON.stringify({ recordEvaluation, checkEvaluation })
  assert.deepEqual(checkQualification({ recordEvaluation, checkEvaluation, commands, retirement: { passed: true } }), { passed: false, exitCode: 1 })
  assert.equal(JSON.stringify({ recordEvaluation, checkEvaluation }), before, 'both actual evaluations remain unchanged')
  const valid = { recordEvaluation: { passed: true }, checkEvaluation: { passed: true }, commands, retirement: { passed: true } }
  assert.deepEqual(checkQualification(valid), { passed: true, exitCode: 0 })
  for (const invalid of [{ retirement: { passed: false } }, { commands: commands.slice(1) }, { commands: commands.map(value => ({ ...value, exitCode: 2 })) }, { checkEvaluation: { passed: false } }, { recordEvaluation: null }]) {
    assert.deepEqual(checkQualification({ ...valid, ...invalid }), { passed: false, exitCode: 1 })
  }
})

test('scaling trace preserves DOM/network snapshots, fingerprints options, and reports exact rejected size', () => {
  assert.deepEqual(observationTraceCaptureOptions(false), { screenshots: true, snapshots: true, sources: false }, 'historical capture stays unchanged')
  const options = observationTraceCaptureOptions(true)
  assert.deepEqual(options, { screenshots: false, snapshots: true, sources: false })
  assert.equal(options, OBSERVATION_SCALING_CONTRACT.traceCaptureOptions)
  assert.ok(Object.isFrozen(options))
  assert.notEqual(hashJson(OBSERVATION_SCALING_CONTRACT), hashJson({ ...OBSERVATION_SCALING_CONTRACT, traceCaptureOptions: observationTraceCaptureOptions(false) }), 'capture options change the contract fingerprint')
  const limit = 64 * 1024 * 1024
  const receipt = traceAdmissionReceipt(limit + 123, limit, options)
  assert.deepEqual(receipt, { sizeBytes: limit + 123, limitBytes: limit, captureOptions: options, capturedRun: 'large sample 5/5 after two warmups', withinLimit: false })
  assert.equal(traceAdmissionReceipt(limit, limit, options).withinLimit, true)
  assert.throws(() => traceAdmissionReceipt(undefined, limit, options), /actual trace size/)
})

test('pending failed evidence survives startup and new failures until full trace verification and retirement', () => {
  assert.deepEqual(startupEvidenceDisposition({ qualificationPassed: false, failure: 'original size rejection' }), { invalidate: false, pendingFailure: true })
  assert.deepEqual(startupEvidenceDisposition({ qualificationPassed: true }), { invalidate: true, pendingFailure: false })
  assert.deepEqual(startupEvidenceDisposition(null), { invalidate: false, pendingFailure: false })
  const h = scratchFixture()
  prepareScalingScratch(h.profile, temporary, h.io)
  const original = Object.values(h.profile.files).map(file => [file, h.entries.get(file)])
  for (const state of [{ traceVerified: false, retirement: { passed: true } }, { traceVerified: true, retirement: { passed: false } }, { traceVerified: false, retirement: null }]) {
    const result = persistOrRetainFailureDiagnostics({ ...state, pendingFailure: true }, () => h.attempt(() => { throw new Error('new failure') }))
    assert.equal(result.preserved, true)
    assert.deepEqual(h.operations, [], 'new pre-verification failure must not retire old diagnostics')
    for (const [file, entry] of original) assert.equal(h.entries.get(file), entry)
  }
  const resolved = persistOrRetainFailureDiagnostics({ pendingFailure: true, traceVerified: true, retirement: { passed: true } }, () => h.owned.write(h.profile.files.validation, 'replacement'))
  assert.equal(resolved.preserved, false)
  assert.ok(h.operations.some(value => value[0] === 'write'))
  assert.notEqual(h.entries.get(h.profile.files.validation), original.find(([file]) => file === h.profile.files.validation)[1])
})
