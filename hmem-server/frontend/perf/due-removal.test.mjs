import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import { createHash } from 'node:crypto'
import { DUE_REMOVAL_BASE, DUE_REMOVAL_TASK, DUE_REMOVAL_PROJECT, DUE_REMOVAL_REVISION, evidenceProfile, evidenceOwnerFields, isolatedRecordOutputs, assertEvidenceIdentity, assertEvidenceWrite, prepareEvidenceScratch, createEvidenceOperations, checkQualification } from './evidence-profile.mjs'
import { generateFixture, fixtureHash, stableFixtureJson } from './fixtures.mjs'
import { inputQualification, assertQualifiedInputs, finalWorkingTreeEvidence, verifyTaskTrace, retireTaskTrace } from './harness.mjs'
import { retireOwnedResources, HARNESS_CONFIGURATION } from './contracts.mjs'
import { observationTraceCaptureOptions } from './observation-scaling.mjs'

const frontend = path.resolve('fixture-root/hmem-server/frontend')
const temporary = path.resolve('outside-temporary')
const implementation = '1'.repeat(40)
const plan = () => evidenceProfile(DUE_REMOVAL_BASE, frontend, temporary)
const identity = profile => ({ taskId: profile.task.taskId, ...evidenceOwnerFields(profile), evidenceBaseCommit: profile.baseCommit, measurementRevision: profile.revision,
  sourceProvenance: { reviewBaseCommit: profile.baseCommit, headCommit: implementation }, inputQualification: { implementationCommit: implementation } })

for (const [size, current, historical] of [
  ['small', '0c425c77e5069b8d4cf26dbdebcf94037a22ee977e61d63ab3cceb1a7f090bc4', '2f269eb2256dc4dde701cf769e2d83c73a99c29de26b1da8ae24a343cd39e3cb'],
  ['large', 'b98d53a1a1ca28641dc90cbb598f3026949edf702f0ae4696614520b5140dd63', '0c966ae9c5476317ba8f7f05ad0200a9c8526e16c430e6068a8a4daf9e35a077']
]) {
  test(`${size} due-free generator differs from its historical bytes only by task due payloads`, () => {
    const fixture = generateFixture(size)
    assert.equal(fixtureHash(fixture), current)
    assert.ok(fixture.tasks.every(task => !Object.hasOwn(task, 'due_at')))
    // Original review-base due rules, including non-null values and property order.
    const legacy = { ...fixture, tasks: fixture.tasks.map((task, index) => {
      const eligibilityIndex = index < fixture.scale.topTasks ? index : index - fixture.scale.topTasks
      const due = eligibilityIndex % 4 === 0 ? new Date(Date.UTC(2025, 0, 1, 8, 0, 0) + Math.floor((index + 5000) / 3) * 60000).toISOString() : null
      return Object.fromEntries(Object.entries(task).flatMap(([key, value]) => key === 'completed_at' ? [['due_at', due], [key, value]] : [[key, value]]))
    }) }
    assert.equal(fixtureHash(legacy), historical, 'all other exact fixture bytes retain the historical golden')
    for (const task of legacy.tasks) delete task.due_at
    assert.equal(stableFixtureJson(legacy), stableFixtureJson(fixture))
  })
}

test('due-removal evidence belongs to final verification and preserves historical profiles and phases', () => {
  const profile = plan()
  assert.equal(profile.task.taskId, '1e420484-81b9-4251-b485-82abca34c800')
  assert.equal(profile.task.projectId, '9f1a1a40-afc5-4825-851c-b71aaf498b58')
  assert.equal(profile.baseCommit, '33abd80dd13dbc6c850397eeb13f79047d0f7c91')
  assert.equal(profile.revision, 'due-removal.v1')
  assert.equal(profile.scaling, false)
  assert.equal(profile.rendering, false)
  assert.equal(profile.qualifiedRetirement, true)
  assert.equal(profile.root, path.join(temporary, 'hmem-due-removal-' + DUE_REMOVAL_TASK))
  assert.equal(new Set(Object.values(profile.files)).size, 5)
  for (const file of Object.values(profile.files)) assert.equal(path.dirname(file), profile.root)
  assert.equal(profile.trace, path.join(profile.root, 'temporary/large-due-removal-trace.zip'))
  assert.match(profile.retention, /Final verification task .*project .*closure plus 30 days, then dispose/)
  assert.throws(() => evidenceProfile(DUE_REMOVAL_BASE, frontend, frontend), /outside/)
  assert.throws(() => evidenceProfile('foreign', frontend, temporary), /Unmapped/)
  for (const [base, revision, task, prefix] of [
    ['818cc3cc634bfa8c0ebe14c13fc70b4c2d059e83', 'v1', null, null],
    ['04fd7ba26b1a63b5f1c601939b046ef2fe5f51d6', 'v2', '2503e08f-ff82-4f2c-adff-24e14fec8299', null],
    ['1bdbe3d071d1fbbb994bc515841103adedff77ec', 'v3', '755a7286-6da5-494e-9b9b-fa1edd43d0b0', null],
    ['d9ff753763f086fa4faef078b451f2e48ca1a7a4', 'expanded-hierarchy.v1', '555ebf6e-8442-43f4-a75d-c33fcffc76f4', null],
    ['c5230077a643d06e7ca5a4ff6b5616cfb90862ec', 'observation-scaling.v1', 'b9f1de65-f016-4178-89a0-ac13a5d7fff8', 'hmem-observation-scaling-'],
    ['ed3aca53afe346f928dc00f2e2a7b0a8e07aaecb', 'observation-rendering.v1', 'cbb38fd2-fc89-449c-a308-168374f23f82', 'hmem-observation-rendering-'],
    ['f546cafd26adae37772d2383262e87596ea7151b', 'planning-counts.v1', 'b401c358-1a68-404e-bd18-86f40bb3b771', 'hmem-planning-counts-']
  ]) {
    const old = evidenceProfile(base, frontend, temporary)
    assert.equal(old.revision, revision)
    assert.equal(old.task?.taskId || null, task)
    assert.equal(old.root, prefix ? path.join(temporary, prefix + task) : path.join(frontend, 'perf'))
    assert.equal(old.files.after, path.join(old.root, `final-working-tree.after.${revision}.json`))
    assert.deepEqual(evidenceOwnerFields(old), {})
  }
  assert.equal(HARNESS_CONFIGURATION.warmups, 2)
  assert.equal(HARNESS_CONFIGURATION.samples, 5)
})

test('existing npm record-after arguments map to five isolated outputs and reject redirects/baseline', () => {
  const profile = plan(), scripts = JSON.parse(fs.readFileSync('package.json', 'utf8')).scripts
  const output = path.resolve(scripts['perf:record-after'].match(/--output (\S+)/)[1])
  const trace = path.resolve(scripts['perf:record-after'].match(/--trace-output (\S+)/)[1])
  assert.match(scripts['perf:record-after'], /--authorize-after-artifact/)
  assert.equal(scripts['perf:check'], 'npm run build && node perf/harness.mjs check')
  assert.deepEqual(isolatedRecordOutputs(profile, output, trace, output, trace), { output: profile.files.after, traceManifest: profile.files.traceManifest })
  assert.deepEqual(isolatedRecordOutputs(profile, profile.files.after, profile.files.traceManifest, output, trace), { output: profile.files.after, traceManifest: profile.files.traceManifest })
  for (const target of [path.resolve('perf/baseline.v1.json'), path.resolve('perf/final-working-tree.after.planning-counts.v1.json'), path.join(temporary, 'redirect.json')]) {
    assert.throws(() => isolatedRecordOutputs(profile, target, trace, output, trace), /task-owned/)
    assert.throws(() => assertEvidenceWrite(profile, target, '{}'), /Unowned/)
  }
  assert.throws(() => isolatedRecordOutputs(profile, output, path.join(temporary, 'redirect.json'), output, trace), /task-owned/)
})

test('due-removal identity rejects wrong task, project, base, measurement and implementation revisions', () => {
  const profile = plan(), record = identity(profile)
  assert.doesNotThrow(() => assertEvidenceIdentity(profile, record))
  for (const field of ['taskId', 'projectId', 'evidenceBaseCommit', 'measurementRevision']) assert.throws(() => assertEvidenceIdentity(profile, { ...record, [field]: 'foreign' }), /identity mismatch/)
  for (const source of [{ ...record.sourceProvenance, reviewBaseCommit: '2'.repeat(40) }, { ...record.sourceProvenance, headCommit: 'invalid' }, { ...record.sourceProvenance, headCommit: '2'.repeat(40) }]) {
    assert.throws(() => assertEvidenceIdentity(profile, { ...record, sourceProvenance: source }), /identity mismatch/)
  }
  assert.throws(() => assertEvidenceIdentity(profile, null), /identity mismatch/)
})

// Ownership and failure faults use a virtual filesystem, with no extra scratch.
function scratchFixture() {
  const profile = plan(), entries = new Map([[temporary, { kind: 'directory', ino: 1 }], [profile.root, { kind: 'directory', ino: 2 }]]), operations = []
  const missing = () => Object.assign(new Error('Missing virtual entry'), { code: 'ENOENT' })
  const io = {
    existsSync: file => entries.has(file),
    realpathSync: file => { const info = entries.get(file); if (!info) throw missing(); return info.real || file },
    lstatSync: file => { const info = entries.get(file); if (!info) throw missing(); return { dev: 1, ino: info.ino, size: info.size ?? info.bytes?.length ?? 0, isSymbolicLink: () => info.kind === 'symlink', isDirectory: () => info.kind === 'directory', isFile: () => info.kind === 'file' } },
    readdirSync: directory => [...entries.keys()].filter(file => path.dirname(file) === directory).map(file => path.basename(file)),
    mkdirSync: file => { operations.push(['mkdir', file]); entries.set(file, { kind: 'directory', ino: 4 }) },
    rmdirSync: file => { operations.push(['rmdir', file]); entries.delete(file) },
    writeFileSync: (file, bytes) => { operations.push(['write', file]); entries.set(file, { kind: 'file', ino: 5, bytes: Buffer.from(bytes) }) },
    renameSync: (source, destination) => { operations.push(['rename', source, destination]); entries.set(destination, entries.get(source)); entries.delete(source) },
    unlinkSync: file => { operations.push(['remove', file]); if (!entries.delete(file)) throw missing() },
    readFileSync: (file, encoding) => { const bytes = entries.get(file)?.bytes; if (!bytes) throw missing(); return encoding ? bytes.toString(encoding) : bytes }
  }
  io.statSync = io.lstatSync
  const owned = createEvidenceOperations(profile, temporary, io)
  return { profile, entries, operations, io, owned }
}

test('root lifetime, admitted filenames and file size prevent any fallback persistence', () => {
  for (const fault of ['unadmitted', 'symlink', 'parent-replaced', 'root-replaced', 'unexpected-file', 'oversized-file', 'wrong-root', 'redirected-output']) {
    const h = scratchFixture()
    if (fault !== 'unadmitted') prepareEvidenceScratch(h.profile, temporary, h.io)
    if (fault === 'symlink') Object.assign(h.entries.get(h.profile.root), { kind: 'symlink', real: path.resolve('foreign') })
    if (fault === 'parent-replaced') h.entries.get(temporary).ino = 100
    if (fault === 'root-replaced') h.entries.get(h.profile.root).ino = 100
    if (fault === 'unexpected-file') h.entries.set(path.join(h.profile.root, 'unowned.json'), { kind: 'file' })
    if (fault === 'oversized-file') h.entries.set(h.profile.files.after, { kind: 'file', size: 64 * 1024 * 1024 + 1 })
    if (fault === 'wrong-root') h.profile.root = path.join(temporary, 'unowned-directory')
    if (fault === 'redirected-output') h.profile.files.after = path.resolve('perf/baseline.v1.json')
    assert.throws(() => h.owned.write(h.profile.files.validation, '{}'), /ownership|symlink|lifetime|Unexpected|64 MiB|mismatch/)
    assert.throws(() => h.owned.remove(h.profile.files.manifest), /ownership|symlink|lifetime|Unexpected|64 MiB|mismatch/)
    assert.deepEqual(h.operations, [], fault)
  }
  assert.throws(() => assertEvidenceWrite(plan(), plan().files.after, Buffer.alloc(64 * 1024 * 1024 + 1)), /64 MiB/)
})

test('producer binds complete diff and current revision to the declared review base without rewriting baseline', () => {
  const h = scratchFixture(), baseline = Buffer.from('{"immutable":"baseline"}\n'), reads = []
  for (const file of Object.values(h.profile.files)) h.entries.set(file, { kind: 'file', ino: 3, bytes: Buffer.from('{}') })
  h.entries.get(h.profile.files.validation).bytes = Buffer.from(JSON.stringify({ ...identity(h.profile), passed: true }))
  const read = h.io.readFileSync
  h.io.readFileSync = (file, encoding) => file === path.resolve('perf/baseline.v1.json') ? (encoding ? baseline.toString(encoding) : baseline) : read(file, encoding)
  let baselineAtBase = baseline, head = implementation
  const capture = { read(args) {
    reads.push(args)
    if (args[0] === 'show') return baselineAtBase
    if (args[0] === 'rev-parse') return Buffer.from(head + '\n')
    if (args[0] === 'rev-list') return Buffer.from('2'.repeat(40) + '\n' + head + '\n')
    assert.ok(['diff', 'ls-files'].includes(args[0])); return Buffer.alloc(0)
  } }
  prepareEvidenceScratch(h.profile, temporary, h.io)
  const produce = () => finalWorkingTreeEvidence({ plan: h.profile, io: h.io, write: h.owned.write, capture })
  produce()
  const manifest = JSON.parse(h.io.readFileSync(h.profile.files.manifest, 'utf8'))
  assertEvidenceIdentity(h.profile, manifest)
  assert.equal(manifest.sourceProvenance.headCommit, implementation)
  assert.equal(manifest.sourceProvenance.reviewBaseCommit, DUE_REMOVAL_BASE)
  assert.equal(manifest.equality.byteForByteEqual, true)
  assert.equal(manifest.traceArchive.available, false)
  assert.equal(manifest.qualificationPassed, true)
  assert.deepEqual(manifest.artifacts.map(value => value.path), [h.profile.files.after, h.profile.files.traceManifest, h.profile.files.diff, h.profile.files.validation])
  assert.ok(reads.some(args => args[0] === 'show' && args[1] === DUE_REMOVAL_BASE + ':hmem-server/frontend/perf/baseline.v1.json'))
  assert.equal(h.entries.size, 7, 'one parent/root and five admitted artifacts')
  baselineAtBase = Buffer.from('changed baseline')
  assert.throws(produce, /immutable baseline differs/)
  baselineAtBase = baseline; head = '3'.repeat(40)
  assert.throws(produce, /implementation revision changed/)
})

test('temporary trace must fit, double-hash, and retire with all resources before qualification', async () => {
  const h = scratchFixture(), trace = Buffer.from('bounded trace'), options = observationTraceCaptureOptions(true)
  const temporaryTrace = path.dirname(h.profile.trace)
  h.entries.set(temporaryTrace, { kind: 'directory', ino: 8 })
  h.entries.set(h.profile.trace, { kind: 'file', ino: 9, bytes: trace })
  prepareEvidenceScratch(h.profile, temporary, h.io)
  assert.deepEqual(options, { screenshots: false, snapshots: true, sources: false })
  const verify = () => verifyTaskTrace({ plan: h.profile, io: h.io, operations: h.owned, captureOptions: options })
  assert.deepEqual(verify(), { sha256: createHash('sha256').update(trace).digest('hex'), sizeBytes: trace.length })
  h.entries.get(h.profile.trace).size = 64 * 1024 * 1024 + 1
  assert.throws(verify, error => /64 MiB/.test(error.message) && error.traceAdmission.withinLimit === false)
  delete h.entries.get(h.profile.trace).size
  const read = h.io.readFileSync; let count = 0
  h.io.readFileSync = (file, ...args) => file === h.profile.trace ? (++count === 1 ? trace : Buffer.from('changed trace')) : read(file, ...args)
  assert.throws(verify, /not reproducible/)
  h.io.readFileSync = read
  const closed = []
  const retirement = await retireOwnedResources([
    { resource: 'browser', close: async () => { closed.push('browser'); throw new Error('browser cleanup failed') } },
    { resource: 'server', close: async () => { closed.push('server') } },
    { resource: 'trace', close: async () => { closed.push('trace'); retireTaskTrace(h.profile, h.owned, h.io) } }
  ], 100)
  assert.deepEqual(closed, ['browser', 'server', 'trace'])
  assert.equal(h.entries.has(h.profile.trace), false)
  assert.equal(h.entries.has(temporaryTrace), false)
  const commands = [{ command: 'npm run perf:self-check', exitCode: 0 }, { command: 'node perf/harness.mjs record', exitCode: 0 }, { command: 'node perf/harness.mjs check', exitCode: 0 }]
  assert.equal(checkQualification({ commands, recordEvaluation: { passed: true }, checkEvaluation: { passed: true }, retirement }).passed, false)
  const clean = await retireOwnedResources([{ resource: 'trace', close: async () => retireTaskTrace(h.profile, h.owned, h.io) }], 100)
  assert.equal(checkQualification({ commands, recordEvaluation: { passed: true }, checkEvaluation: { passed: true }, retirement: clean }).passed, true)
})

test('due-removal fingerprints reject executable adapter, producer, assets and implementation commit drift', () => {
  const profile = plan(), recorded = inputQualification({ plan: profile })
  assert.match(recorded.implementationCommit, /^[a-f0-9]{40}$/)
  for (const suffix of ['/perf/evidence-profile.mjs', '/perf/harness.mjs', '/perf/fixtures.mjs', '/tests-js/observation-viewport-fixture.mjs']) {
    const subject = recorded.source.find(value => value.path.endsWith(suffix))
    assert.ok(subject, suffix)
    const changed = inputQualification({ plan: profile, io: { ...fs, readFileSync(file, ...args) {
      return file.split(path.sep).join('/').endsWith(subject.path) ? Buffer.concat([fs.readFileSync(file), Buffer.from('\nchanged bytes')]) : fs.readFileSync(file, ...args)
    } } })
    assert.throws(() => assertQualifiedInputs(recorded, changed, 'input drift'), /input drift/)
  }
  const asset = recorded.productionAssets[0]
  const changed = inputQualification({ plan: profile, io: { ...fs, readFileSync(file, ...args) {
    return file.split(path.sep).join('/').endsWith(asset.path) ? Buffer.concat([fs.readFileSync(file), Buffer.from('changed')]) : fs.readFileSync(file, ...args)
  } } })
  assert.throws(() => assertQualifiedInputs(recorded, changed, 'asset drift'), /asset drift/)
  const differentHead = inputQualification({ plan: profile, capture: { read: () => Buffer.from('4'.repeat(40) + '\n') } })
  assert.throws(() => assertQualifiedInputs(recorded, differentHead, 'commit drift'), /commit drift/)
  assert.doesNotThrow(() => assertQualifiedInputs(recorded, inputQualification({ plan: profile }), 'input drift'))
})
