/** Full-list structural acceptance; predictive research scores remain separate. */
import { readFile, writeFile, mkdir } from 'node:fs/promises';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { strict as assert } from 'node:assert';
import { readAuthoredPolitics } from '../workspaces/politics-default';
import { implementationKey } from '../data-preparation/fast-share-release-io';
import { preparePoliticsExecution, PoliticsPreparationCache } from '../../src/lib/politics/execution/prepare';
import { prepareM02Votes } from '../../src/lib/politics/generation/m02';
import { REFORM, ROSATELLUM } from '../../src/lib/politics/scenario/schema';
const root = fileURLToPath(new URL('../../../', import.meta.url));
const implementationId = await implementationKey(root, 'web/src/lib/workers/politics-compute.ts');
const authored = await readAuthoredPolitics(root), cache = new PoliticsPreparationCache();
const readers = { implementationId,
  asset: async (p: { uri: string }) => new Uint8Array(await readFile(resolve(root, 'web/static', p.uri))),
  sourceChunk: async (path: string) => new Uint8Array(await readFile(resolve(root, 'web/static/data/reform-2026/fast-share', path))) };
const cases: object[] = [];
for (const law of [ROSATELLUM, REFORM] as const) {
  const scenario = { ...authored, selection: { ...authored.selection, scrutiny: { ...authored.selection.scrutiny, id: law },
    generatorsByLaw: { ...authored.selection.generatorsByLaw, [law]: { id: 'M02' as const, version: '3.0.0' as const } } } };
  const prepared = await preparePoliticsExecution(scenario, readers, { cache }), g = prepared.computeInputs.generator;
  if (g.id !== 'M02') throw new Error('Expected M02');
  const create = (gap: number) => {
    const context = structuredClone(g.context);
    context.models.chambers.profiles.find(p => p.id === g.settings.chamberProfileId)!.gapMultiplier = gap;
    return prepareM02Votes(g.settings, g.assets, context);
  };
  const checkCounts = (run: ReturnType<ReturnType<typeof create>['run']>) => {
    for (const row of run.counts.values()) {
      const values = [...row.votes.values()];
      assert(values.every(v => Number.isSafeInteger(v) && v >= 0));
      assert(values.reduce((n, v) => n + v, 0) <= row.electors);
    }
  };
  const baseline = create(1), byDraw = new Map([1, 2, 7].map(id => [id, baseline.run(id)]));
  for (const gap of [0, 1, 2]) {
    const p = create(gap);
    const gaps = [1, 2, 7].map(id => {
      const run = p.run(id); checkCounts(run);
      return Math.max(...run.audit.countryForecast.camera!.map((v, j) => Math.abs(v - run.audit.countryForecast.senato![j])));
    });
    cases.push({ law, case: 'paired-contrast-sensitivity', gap, components: p.componentIds.length, draws: [1, 2, 7],
      largestElectorShareGap: gaps,
      qualification: 'Physical filings and explicit chamber references remain active; these gaps are not held-out coverage or bonus probabilities.' });
  }
  const changed = structuredClone(g.settings), absent = changed.lists.camera.find(l => l.componentIds.every(id => changed.components.find(c => c.id === id)?.kind === 'fitted'))!.id;
  const cell = changed.coverage.find(c => c.chamber === 'camera' && c.listIds.includes(absent))!;
  cell.listIds = cell.listIds.filter(id => id !== absent);
  const p = prepareM02Votes(changed, g.assets, g.context);
  for (const id of [1, 2, 7]) {
    const run = p.run(id); checkCounts(run);
    assert.deepEqual(run.audit.countryForecast.senato, byDraw.get(id)!.audit.countryForecast.senato);
    for (const college of g.context.geography.colleges.filter(c => c.chamber === 'senato'))
      assert.deepEqual(run.counts.get(college.id), byDraw.get(id)!.counts.get(college.id));
    assert(!run.counts.get(cell.collegeId)!.votes.has(absent));
  }
  cases.push({ law, case: 'physical-filing-independence', absentList: absent, college: cell.collegeId, draws: [1, 2, 7], senateCountsExactlyPreserved: true });
  cases.push({ law, case: 'new-components-and-joint-memberships', futureComponents: g.settings.components.length,
    newComponents: g.settings.components.filter(c => c.kind === 'new').map(c => c.id),
    jointLists: g.settings.lists.camera.filter(l => l.componentIds.length > 1).map(l => l.id),
    fitComponents: g.assets.fit.componentIds.length, draws: [1, 2, 7], validCounts: true });
}
const report = { status: 'passed', date: '2026-11-16', implementationId, source: authored.data.historical,
  contrastAssumption: 'Fixed paired and explicit-reference log-ratio/log-odds contrast through future movement. No temporal contrast innovation is estimated.', cases };
const output = resolve(root, 'data/derived/model-acceptance-2026-11'); await mkdir(output, { recursive: true });
await writeFile(resolve(output, 'shared-full-panel.json'), JSON.stringify(report, null, 2) + '\n');
console.log(JSON.stringify({ status: report.status, cases: cases.length, implementationId,
  report: resolve(output, 'shared-full-panel.json') }));
