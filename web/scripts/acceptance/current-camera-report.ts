import { requireGenerationModel } from '../../src/lib/politics/reform-2026/vote-share/model-registry';
/** Curate only complete current-model runs; older model receipts stay immutable. */
import { readFile, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { createHash } from 'node:crypto';
import { gunzipSync, gzipSync } from 'node:zlib';
import { loadPartyResearchData } from '../data-preparation/party-drift-panel-data';
import { membershipFor } from '../data-preparation/party-drift-memberships';
requireGenerationModel({ model: 'M02', modelVersion: '1.0.0' }); // Historical: checkout 8dcb830/965f7ba.
const root = resolve('..'), directory = resolve(root, 'data/derived/current-camera-comparison'),
  json = async (path: string) => JSON.parse(await readFile(path, 'utf8')),
  sha = (bytes: Uint8Array | string) => createHash('sha256').update(bytes).digest('hex');
const reduced = await json(resolve(directory, 'reduced-report.json')), full = await json(resolve(directory, 'full-report.json')),
  future = await json(resolve(directory, 'future-1000-report.json'));
const reportPaths = ['reduced-report.json', 'full-report.json', 'future-1000-report.json'];
// Full elections/models may run in separate processes. Keep their individual
// reports and require an exact, nonduplicated final case set.
if (full.rows.length < 4) {
  for (const model of ['M01', 'M02']) {
    const name = 'full-2022-' + model + '-report.json', part = await json(resolve(directory, name));
    reportPaths.push(name);
    if (JSON.stringify(part.codeHashes) !== JSON.stringify(full.codeHashes) || part.draws !== 512 || part.failures.length)
      throw new Error('INCOMPATIBLE_FULL_REPORT_PART');
    full.rows.push(...part.rows);
  }
}
const actualCases = new Set(full.rows.map((r: { target: string; model: string }) => r.target + '/' + r.model));
if (actualCases.size !== 4 || ![2018, 2022].every(year => ['M01', 'M02'].every(model => actualCases.has('camera-' + year + '/' + model))))
  throw new Error('INCOMPLETE_FULL_CASE_SET');
const reducedCases = new Set(reduced.rows.map((r: { target: string; horizon: number; model: string }) => r.target + '/' + r.horizon + '/' + r.model));
if (reducedCases.size !== 24 || ![2013, 2018, 2022].every(year => [730, 365, 90, 15]
  .every(horizon => ['M01', 'M02'].every(model => reducedCases.has('camera-' + year + '/' + horizon + '/' + model)))))
  throw new Error('INCOMPLETE_REDUCED_CASE_SET');
for (const [report, expected] of [[reduced, 24], [full, 4]] as const) {
  if (report.draws !== 512 || report.rows.length !== expected || report.failures.length) throw new Error('INCOMPLETE_HISTORICAL_COMPARISON');
  for (const [path, hash] of Object.entries(report.codeHashes))
    if (sha(await readFile(resolve(root, path))) !== hash) throw new Error('SOURCE_CHANGED_SINCE_RUN:' + path);
  for (const r of report.rows) {
    if (r.draws !== 512 || r.maximumResidual !== 0 || !r.fitDiagnostics.political.passed || !r.fitDiagnostics.participation.passed ||
        !r.originalStrata?.every((e: { strata: number }) => e.strata > 0)) throw new Error('INVALID_RUN_EVIDENCE');
  }
}
if (future.draws !== 1000 || future.models.length !== 2 || future.models.some((m: { draws: number; runs: object[] }) => m.draws !== 1000 || m.runs.length !== 1000))
  throw new Error('INCOMPLETE_FUTURE_COMPARISON');
const rows = [...reduced.rows.map((r: object) => ({ ...r, mode: 'reduced' })), ...full.rows.map((r: object) => ({ ...r, mode: 'full' }))];
const compact = rows.map(r => ({ ...r,
  fitDiagnostics: Object.fromEntries(Object.entries(r.fitDiagnostics).map(([k, v]) => {
    const d = v as { passed: boolean; maxRhat: number; minEss: number; minTailEss: number };
    return [k, { passed: d.passed, maxRhat: d.maxRhat, minEss: d.minEss, minTailEss: d.minTailEss }];
  })),
  monteCarlo: Object.fromEntries(Object.entries(r.monteCarlo).map(([scope, values]) => [scope,
    Object.fromEntries(Object.entries(values as Record<string, { draws: number[] }>).map(([metric, { draws, ...summary }]) => [metric, summary]))])),
}));
const details = [], sources = [];
for (const name of reportPaths) {
  const path = resolve(directory, name); sources.push({ path: path.slice(root.length + 1), sha256: sha(await readFile(path)) });
}
for (const r of rows) {
  const path = resolve(directory, `${r.target}-${r.horizon}-${r.model}-${r.mode}-scores.json.gz`), bytes = await readFile(path);
  const scores = JSON.parse(gunzipSync(bytes).toString()), bias: object[] = [];
  for (const [scope, entries] of Object.entries(scores)) {
    const groups = new Map<string, { members: string[]; units: number; truth: number; expected: number; signedError: number; absoluteError: number }>();
    for (const entry of entries as { members: string[]; truth: number; score: { mean: number; meanBias: number; meanMae: number } }[]) {
      const key = JSON.stringify(entry.members), g = groups.get(key) ?? { members: entry.members, units: 0, truth: 0, expected: 0, signedError: 0, absoluteError: 0 };
      g.units++; g.truth += entry.truth; g.expected += entry.score.mean; g.signedError += entry.score.meanBias; g.absoluteError += entry.score.meanMae; groups.set(key, g);
    }
    for (const g of groups.values()) bias.push({ scope, ...g, truth: g.truth / g.units, expected: g.expected / g.units,
      signedError: g.signedError / g.units, absoluteError: g.absoluteError / g.units });
  }
  details.push({ target: r.target, horizon: r.horizon, model: r.model, mode: r.mode,
    bias, college: scores.college.map(({ values, ...row }: { values: number[] }) => row),
    constituency: scores.constituency.map(({ values, ...row }: { values: number[] }) => row),
    sampledArea: scores.sampledArea.map(({ values, ...row }: { values: number[] }) => row), monteCarlo: r.monteCarlo });
  sources.push({ path: path.slice(root.length + 1), sha256: sha(bytes) });
}
const previousPaths = ['test/fixtures/benchmarks/vote_share_legacy_comparison.json',
  'test/fixtures/benchmarks/vote_share_legacy_details.json.gz', 'test/fixtures/benchmarks/vote_share_feasible_research.json',
  'docs/vote-generation-legacy-comparison-results.md'];
const oldHashes = Object.fromEntries(await Promise.all(previousPaths.map(async path => [path, sha(await readFile(resolve(root, path)))])));
const pins = Object.fromEntries(await Promise.all(['data/legacy-comparison-policy.json', 'docs/models/M01.md', 'docs/models/M02.md',
  'web/scripts/acceptance/current-camera-future.ts', 'web/scripts/acceptance/current-camera-future.worker.ts', 'web/scripts/acceptance/current-camera-report.ts']
  .map(async path => [path, sha(await readFile(resolve(root, path)))])));
const raw = await loadPartyResearchData(root), inputAudit = [];
if (JSON.stringify(raw.versions) !== JSON.stringify(reduced.inputVersions)) throw new Error('HISTORICAL_SOURCE_CHANGED');
for (const event of raw.history.events.filter(e => e.date < '2022-09-10')) {
  const memberships = new Map<string, string[]>(); let municipalities = 0;
  for (const m of event.municipalities) {
    if (!m.matchable || m.electors <= 0 || m.validVotes > m.electors) continue;
    const members = m.lists.flatMap(l => {
      if (!memberships.has(l.name)) memberships.set(l.name, membershipFor(raw.policy, event.id, l.name).members);
      return memberships.get(l.name)!;
    });
    if (new Set(members).size !== members.length || m.lists.reduce((s, l) => s + l.votes, 0) !== m.validVotes)
      throw new Error('INVALID_ORIGINAL_HISTORICAL_ROW:' + event.id + '/' + m.id);
    municipalities++;
  }
  inputAudit.push({ event: event.id, municipalities, uniqueRawIdentities: true, originalCountTotals: true });
}
const receipt = { schemaVersion: 1, created: new Date().toISOString(),
  scope: 'M01@1.0.0 and M02@1.0.0 kernels with approved Camera-only historical preparation; frozen cutoff-specific admissible research panels, not the current 34-component default basis.',
  qualification: 'Reused historical elections and conditional target electorates/filing; no unseen-election calibration, Senate validation or production model-law release. Historical kernels and exact observable-group scoring are current. Original national offered-set strata restored from pinned municipal counts. Whole-draw Monte Carlo differences use independent model stream namespaces. Future campaign uses the current default, Camera only, and a loaded prefit.',
  catalogId: reduced.catalogId, sourceId: reduced.sourceId, codeHashes: reduced.codeHashes, pins, previousEvidenceReadOnly: oldHashes,
  sources, inputAudit, rows: compact, future };
await writeFile(resolve(root, 'test/fixtures/benchmarks/current_camera_comparison.json'), JSON.stringify(receipt, null, 2) + '\n');
await writeFile(resolve(root, 'test/fixtures/benchmarks/current_camera_details.json.gz'), gzipSync(JSON.stringify(details)));

const f = (x: number, digits = 2) => x.toFixed(digits), metric = (r: any, scope: string) => r.scores[scope];
const lines = [
  '<!-- BEGIN GENERATED CURRENT CAMERA COMPARISON -->',
  '## Full-domain historical results', '',
  '15-day cutoff; 512 draws per model. Error30 and coverage are percentages; MAE is in percentage points. Lower Error30/MAE/WIS is better.', '',
  '| Election | Scope | Model | Error30 % | Coverage90 % | Mean MAE pp | WIS |',
  '| --- | --- | --- | ---: | ---: | ---: | ---: |',
];
for (const year of [2018, 2022]) for (const scope of ['municipality', 'college', 'constituency', 'sampledArea']) for (const model of ['M01', 'M02']) {
  const r = full.rows.find((r: any) => r.target === 'camera-' + year && r.model === model), m = metric(r, scope);
  lines.push(`| ${year} | ${scope === 'sampledArea' ? 'evaluated area' : scope} | ${model} | ${f(m.error30 * 100)} | ${f(m.coverage90 * 100)} | ${f(m.meanMae * 100)} | ${f(m.wis, 4)} |`);
}
lines.push('', '## Reduced panels across all four cutoffs', '',
  'Equal weight per case. Conditional component bases can differ across cutoffs, as in the previous comparison.', '',
  '| Election | Model | Municipal Error30 % | Municipal coverage90 % | Municipal MAE pp | College Error30 % | College coverage90 % |',
  '| --- | --- | ---: | ---: | ---: | ---: | ---: |');
for (const year of [2013, 2018, 2022]) for (const model of ['M01', 'M02']) {
  const rs = reduced.rows.filter((r: any) => r.target === 'camera-' + year && r.model === model);
  const mean = (scope: string, name: string) => rs.reduce((n: number, r: any) => n + r.scores[scope][name] / rs.length, 0);
  lines.push(`| ${year} | ${model} | ${f(100 * mean('municipality', 'error30'))} | ${f(100 * mean('municipality', 'coverage90'))} | ${f(100 * mean('municipality', 'meanMae'))} | ${f(100 * mean('college', 'error30'))} | ${f(100 * mean('college', 'coverage90'))} |`);
}
lines.push('', '## Whole-draw Monte Carlo uncertainty', '',
  'Full-domain college Error30: M01 minus M02, in percentage points. Independent-stream normal intervals describe simulation error conditional on these fixed cases.', '',
  '| Election | Difference pp | Monte Carlo SE pp | Normal 95% interval pp |', '| --- | ---: | ---: | --- |');
for (const year of [2018, 2022]) {
  const a = full.rows.find((r: any) => r.target === 'camera-' + year && r.model === 'M01').monteCarlo.college.error30,
    b = full.rows.find((r: any) => r.target === 'camera-' + year && r.model === 'M02').monteCarlo.college.error30,
    difference = 100 * (a.mean - b.mean), se = 100 * Math.hypot(a.mcse, b.mcse);
  lines.push(`| ${year} | ${f(difference)} | ${f(se)} | ${f(difference - 1.96 * se)} to ${f(difference + 1.96 * se)} |`);
}
lines.push('', '## Current future Camera ballot in Chromium', '',
  `1,000 attempted simulations per model; Chromium ${future.browser}; ${future.workers} workers. All attempted draws, including errors, count toward wall time.`, '',
  '| Model | Wall min | Max worker load s | Max preparation s | Generation worker-min | Scrutiny worker-min | Exceptions | Non-computable | Complete 392-seat results / returned |',
  '| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- |');
for (const r of future.models) lines.push(`| ${r.model} | ${f(r.elapsedMs / 60000)} | ${f(r.loadMaxMs / 1000)} | ${f(r.preparationMaxMs / 1000)} | ${f(r.generationWorkerMs / 60000)} | ${f(r.scrutinyWorkerMs / 60000)} | ${r.exceptions.length} | ${r.nonComputable.length} | ${r.filled392} / ${r.returned} |`);
lines.push('', 'Phase sums accumulate time across workers and are not additive phases of campaign wall time. These Camera-only timings have a smaller legal workload than the old paired 588-seat runs. They are not an isolated speed ranking.', '', '<!-- END GENERATED CURRENT CAMERA COMPARISON -->');
const resultPath = resolve(root, 'docs/current-camera-comparison-results.md'), text = await readFile(resultPath, 'utf8'),
  begin = '<!-- BEGIN GENERATED CURRENT CAMERA COMPARISON -->', end = '<!-- END GENERATED CURRENT CAMERA COMPARISON -->';
if (!text.includes(begin) || !text.includes(end)) throw new Error('Missing report markers');
await writeFile(resultPath, text.slice(0, text.indexOf(begin)) + lines.join('\n') + text.slice(text.indexOf(end) + end.length));
console.log(JSON.stringify({ historicalRows: compact.length, future: future.models.map((r: any) => ({ model: r.model, attempts: r.draws, filled: r.filled392 })) }));
