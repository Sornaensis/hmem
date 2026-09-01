import { execFileSync } from 'node:child_process'
import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

const here = path.dirname(fileURLToPath(import.meta.url))
const frontendRoot = path.dirname(here)
const baseCommit = '04fd7ba26b1a63b5f1c601939b046ef2fe5f51d6'
const recordPath = path.join(here, 'final-working-tree.validation-record.v2.json')
const npm = process.platform === 'win32' ? 'npm.cmd' : 'npm'
const environment = { ...process.env, HMEM_EVIDENCE_BASE_COMMIT: baseCommit }
const records = []

function run(label, args) {
  const startedAtUtc = new Date().toISOString()
  const started = performance.now()
  try {
    const executable = process.platform === 'win32' ? (process.env.ComSpec || 'cmd.exe') : npm
    const commandArgs = process.platform === 'win32' ? ['/d', '/s', '/c', npm, ...args] : args
    execFileSync(executable, commandArgs, { cwd: frontendRoot, env: environment, stdio: 'inherit' })
    records.push({ command: label, exitCode: 0, startedAtUtc, finishedAtUtc: new Date().toISOString(), durationMs: Math.round(performance.now() - started) })
  } catch (error) {
    records.push({ command: label, exitCode: Number.isInteger(error.status) ? error.status : 1, startedAtUtc, finishedAtUtc: new Date().toISOString(), durationMs: Math.round(performance.now() - started) })
    fs.writeFileSync(recordPath, `${JSON.stringify({ schemaVersion: 1, taskId: '2503e08f-ff82-4f2c-adff-24e14fec8299', evidenceBaseCommit: baseCommit, commands: records, passed: false }, null, 2)}\n`)
    throw error
  }
}

run('npm test', ['test'])
run('npm run build', ['run', 'build'])
run('npm run perf:self-check', ['run', 'perf:self-check'])
run('HMEM_EVIDENCE_BASE_COMMIT=04fd7ba npm run perf:record-after', ['run', 'perf:record-after'])
run('HMEM_EVIDENCE_BASE_COMMIT=04fd7ba npm run perf:check', ['run', 'perf:check'])

fs.writeFileSync(recordPath, `${JSON.stringify({ schemaVersion: 1, taskId: '2503e08f-ff82-4f2c-adff-24e14fec8299', evidenceBaseCommit: baseCommit, commands: records, passed: true, summary: { commandCount: records.length, totalDurationMs: records.reduce((total, record) => total + record.durationMs, 0) } }, null, 2)}\n`)

// This final check sees the completed immutable record and writes the manifest
// last; it does not alter the source or the approved baseline.
execFileSync('node', ['perf/harness.mjs', 'check'], { cwd: frontendRoot, env: environment, stdio: 'inherit' })
