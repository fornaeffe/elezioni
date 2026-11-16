/** Curate only completed acceptance receipts; large archives stay untracked. */
import { readFile, writeFile } from 'node:fs/promises';
import { resolve, relative } from 'node:path';
import { fileURLToPath } from 'node:url';
import { createHash } from 'node:crypto';
import { strict as assert } from 'node:assert';
const root = fileURLToPath(new URL('../../../', import.meta.url));
const directory = 'data/derived/model-acceptance-2026-11';
const read = async (path: string) => JSON.parse(await readFile(resolve(root, path), 'utf8'));
const pin = async (path: string) => {
  const bytes = await readFile(resolve(root, path));
  return { path, bytes: bytes.length, sha256: createHash('sha256').update(bytes).digest('hex') };
};
const gatePath = process.argv[2] ?? `${directory}/production-gate/report.json`;
const gate = await read(gatePath); assert.equal(gate.status, 'passed');
const units = await read(`${directory}/production-gate/unit.json`);
assert.equal(units.numFailedTests, 0); assert.equal(units.numPendingTests, 0);
const panel = await read(`${directory}/full-panel.json`);
assert.equal(panel.M06.status, 'rejected-at-full-component-law-gate');
const parity = await read(`${directory}/camera-parity.json`), full = await read(`${directory}/shared-full-panel.json`);
assert.equal(parity.status, 'passed'); assert.equal(full.status, 'passed'); assert.equal(parity.implementationId, full.implementationId);
const campaigns: object[] = [];
for (const model of ['M01', 'M02']) for (const law of ['politiche-r-parity-v1', 'politiche-reform-2026-v1']) {
  const path = `${directory}/resources/${model}-${law}-1000-4.json`, r = await read(path);
  const version = model === 'M02' ? '3.0.0' : '2.0.0';
  assert.equal(r.state, 'passed'); assert.equal(r.simulations, 1000); assert.equal(r.legalAcceptance.accepted, true);
  assert.deepEqual(r.infrastructureFailures, []); assert.deepEqual(r.pageErrors, []);
  assert(r.totalMs < 60 * 60 * 1000); assert(r.archive?.content);
  assert.equal(r.stored.execution.generator.version, version);
  assert.equal(r.stored.execution.implementationSha256, full.implementationId.replace(/^implementation-v1-/, ''));
  campaigns.push({ law, model, version, origin: 'fresh-browser-campaign',
    receipt: await pin(path), browser: r.browser, requestedWorkers: r.requestedWorkers,
    actualWorkers: r.stored.execution.actualWorkers, simulations: r.simulations, preparationMs: r.preparationMs,
    fittingAndExecutionMs: r.totalMs, outcomes: r.outcomes, legalAcceptance: r.legalAcceptance,
    peakPrivateBytes: r.peakPrivateBytes, peakWorkingSetBytes: r.peakWorkingSetBytes,
    archive: { ...r.archive, path: relative(root, r.archive.path).replaceAll('\\', '/'),
      roundtrip: relative(root, r.archive.roundtrip).replaceAll('\\', '/') } });
}
const report = { schemaVersion: 1, date: '2026-11-16', status: 'accepted-with-model-rejection',
  scope: 'Local adoption: M01 2.0.0 and M02 3.0.0 for both laws. M06 research-v1 rejected at the production law gate. No public deployment.',
  implementationId: full.implementationId,
  decisions: { M01: { version: '2.0.0', accepted: true }, M02: { version: '3.0.0', accepted: true }, M06: panel.M06 },
  limitations: ['Fixed descriptive and explicit-reference chamber contrasts; temporal contrast innovations and future joint-bonus calibration are not established.',
    'Research six-family historical scores are not full-list accuracy validation. M01 retains its reported chamber-gap qualification.',
    'Memory is sampled renderer-wide, not a guaranteed maximum or isolated model workspace.'],
  inputs: await Promise.all(['data/politics-default-scenario.json', 'docs/models/M01.md', 'docs/models/M02.md',
    'web/scripts/acceptance/shared-model-parity.ts', 'web/scripts/acceptance/shared-model-acceptance.ts',
    'web/scripts/acceptance/model-panel.ts', 'web/scripts/acceptance/common-politics-outcomes.ts',
    'web/scripts/acceptance/common-politics-baseline.test.ts', 'web/tests/fixtures/common-politics-baseline.json'].map(pin)),
  evidence: [...await Promise.all(['full-panel.json', 'camera-parity.json', 'shared-full-panel.json', 'production-gate/unit.json'].map(name => pin(`${directory}/${name}`))), await pin(gatePath)],
  units: { passed: units.numPassedTests, failed: units.numFailedTests, pending: units.numPendingTests },
  browserChecks: gate.checks.filter((c: { name?: string; passed?: number }) => c.passed !== undefined && c.name !== 'unit acceptance'), campaigns };
await writeFile(resolve(root, 'docs/model-acceptance-2026-11.json'), JSON.stringify(report, null, 2) + '\n');
console.log(JSON.stringify({ status: report.status, units: report.units, campaigns: campaigns.length }));
