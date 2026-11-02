import { requireGenerationModel } from '../../src/lib/politics/reform-2026/vote-share/model-registry';
/** Source-eligibility probe only: no fitting, generation or old-model execution. */
import { readFile, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { createHash } from 'node:crypto';
import { loadCandidate, loadCandidateSource } from '../../src/lib/politics/reform-2026/vote-share/release';
import { latestLegacyPair } from '../../src/lib/politics/reform-2026/vote-share/legacy-adapter';
import { compilePairedHistory } from '../../src/lib/politics/reform-2026/vote-share/paired-source';
import { readBytes } from '../data-preparation/fast-share-release-io';

requireGenerationModel({ model: 'M02', modelVersion: '1.0.0' }); // Historical: checkout 8dcb830/965f7ba.
const root = resolve('..');
const directory = resolve('static/data/reform-2026/fast-share');
const read = readBytes(directory);
const pointer = JSON.parse(await readFile(resolve(directory, 'candidate.json'), 'utf8'));
const loaded = await loadCandidate(pointer, read, { validateGeneration: false });
const source = await loadCandidateSource(loaded.catalog, read);
const previousPath = 'test/fixtures/benchmarks/vote_share_legacy_comparison.json';
const previousBytes = await readFile(resolve(root, previousPath));
const previous = JSON.parse(previousBytes.toString()) as {
  rows: { target: string; horizon: number; cutoff: string; municipalities: number }[];
};
const events = source.payload.events;
// Include even the age-incompatible pair, to show that changing the selected IDs
// cannot produce an eligible baseline from the supplied source bundle.
const pairs = events.filter((e) => e.context === 'camera').flatMap((camera) =>
  events.filter((e) => e.context === 'senato' && e.date === camera.date)
    .map((senato) => ({ camera: camera.id, senato: senato.id, electoratePolicy: 'equal-only' as const })),
);
const cases = [...new Map(previous.rows.map((r) => [r.target + '/' + r.horizon, r])).values()];
const rows = [];
for (const entry of cases) {
  let m01: { eligible: boolean; baseline?: string; error?: string };
  try {
    m01 = { eligible: true, baseline: latestLegacyPair(source, entry.cutoff).camera.id };
  } catch (error) {
    if ((error as Error).message !== 'No eligible common-18+ Camera/Senato pair at this reference date') throw error;
    m01 = { eligible: false, error: (error as Error).message };
  }
  const m02 = [];
  for (const pair of pairs) {
    try {
      const compiled = await compilePairedHistory(source, read,
        { ...loaded.selection, referenceDate: entry.cutoff }, pair);
      m02.push({ pair, eligible: true, matchedRows: compiled.history.rows.length });
    } catch (error) {
      if (!(error as Error).message.startsWith('PAIRED_SOURCE_UNAVAILABLE:')) throw error;
      m02.push({ pair, eligible: false, error: (error as Error).message });
    }
  }
  rows.push({ target: entry.target, horizon: entry.horizon, cutoff: entry.cutoff, m01, m02 });
}
// Positive control distinguishes missing historical eligibility from a broken
// source loader. It compiles original paired observations, without any draws.
const currentPair = latestLegacyPair(source, loaded.scenario.referenceDate);
const current = await compilePairedHistory(source, read, loaded.selection, {
  camera: currentPair.camera.id,
  senato: currentPair.senato.id,
  electoratePolicy: 'equal-only',
});
const receipt = {
  schemaVersion: 1,
  checkedAt: new Date().toISOString(),
  scope: 'M01@1.0.0 and M02@1.0.0 historical comparison feasibility; no model simulations executed',
  catalogId: pointer.catalogId,
  sourceId: source.id,
  previous: { path: previousPath, sha256: createHash('sha256').update(previousBytes).digest('hex'), rows: previous.rows.length },
  codeHashes: Object.fromEntries(await Promise.all([
    'web/scripts/acceptance/model-comparison-feasibility.ts',
    'web/src/lib/politics/reform-2026/vote-share/legacy-adapter.ts',
    'web/src/lib/politics/reform-2026/vote-share/paired-source.ts',
  ].map(async (path) => [path, createHash('sha256').update(await readFile(resolve(root, path))).digest('hex')]))),
  events: events.map(({ id, date, availableDate, context, minimumAge, role }) => ({ id, date, availableDate, context, minimumAge, role })),
  cases: rows,
  positiveControl: {
    referenceDate: loaded.scenario.referenceDate,
    camera: currentPair.camera.id,
    senato: currentPair.senato.id,
    matchedRows: current.history.rows.length,
  },
  result: rows.every((r) => !r.m01.eligible && r.m02.every((p) => !p.eligible))
    ? 'blocked-all-historical-cases' : 'review-case-eligibility',
};
const output = resolve(root, 'test/fixtures/benchmarks/model_comparison_feasibility.json');
await writeFile(output, JSON.stringify(receipt, null, 2) + '\n');
console.log(JSON.stringify({ output, result: receipt.result, cases: rows.length, positiveControl: receipt.positiveControl }));
