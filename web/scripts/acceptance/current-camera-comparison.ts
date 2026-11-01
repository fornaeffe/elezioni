/** Runs only current M01/M02 kernels; old receipts are read-only comparators. */
import { readFile, writeFile, mkdir } from 'node:fs/promises';
import { resolve } from 'node:path';
import { createHash } from 'node:crypto';
import { gzipSync } from 'node:zlib';
import { loadPartyResearchData } from '../data-preparation/party-drift-panel-data';
import { loadCandidate, loadCandidateSource } from '../../src/lib/politics/reform-2026/vote-share/release';
import { readBytes } from '../data-preparation/fast-share-release-io';
import { normalizeName } from '../data-preparation/formats';
import { apportion } from '../../src/lib/politics/reform-2026/generation-math';
import { forecastRandom } from '../../src/lib/politics/reform-2026/vote-share/forecast-random';
import type { PhysicalCell } from '../../src/lib/politics/reform-2026/vote-share/forecast-types';
import type { ReplaySource } from './fast-share-replay';
import { distributionScores, summarizeUnits, scoreAreas, type AreaMapping } from './filing-aware-scores';
import { JointTailScores } from './filing-aware-monte-carlo';
import { design, restoreOriginalStrata, prepareCameraM01, prepareCameraM02, generateCameraDraw } from './current-camera-models';

const root = resolve('..'), output = resolve(root, 'data/derived/current-camera-comparison');
await mkdir(output, { recursive: true });
const json = async (path: string) => JSON.parse(await readFile(path, 'utf8'));
const hash = (bytes: Uint8Array | string) => createHash('sha256').update(bytes).digest('hex');
const mode = process.argv[2] ?? 'pilot', draws = Number(process.env.CAMERA_DRAWS ?? (mode === 'pilot' ? 8 : 512));
if (!Number.isSafeInteger(draws) || draws < 2) throw new Error('Invalid draw count');
const directory = resolve('static/data/reform-2026/fast-share'), read = readBytes(directory),
  pointer = await json(resolve(directory, 'candidate.json')),
  loaded = await loadCandidate(pointer, read, { validateGeneration: false }),
  source = await loadCandidateSource(loaded.catalog, read), input = await loadPartyResearchData(root);
const keys = new Map<string, Set<string>>();
for (const u of source.payload.units) if (u.targetCode) {
  const key = u.region + ':' + normalizeName(u.name), codes = keys.get(key) ?? new Set();
  codes.add(u.targetCode); keys.set(key, codes);
}
const colleges = new Map(source.payload.geography.colleges.map(c => [c.id, c])),
  targets = new Map(source.payload.geography.targets.map(t => [t.code, t])),
  geography = new Map<string, typeof source.payload.geography.cells>();
for (const cell of source.payload.geography.cells.filter(c => c.chamber === 'camera')) {
  const parts = geography.get(cell.municipality) ?? []; parts.push(cell); geography.set(cell.municipality, parts);
}
type Entry = { target: string; horizon: number; inputDirectory: string };
let entries: Entry[] = [];
for (const phase of ['development', 'confirmation']) entries.push(...(await json(resolve(root,
  'research/vote-generation/fv01/evidence/data/derived/party-drift/fast-' + phase + '-final-20261027/cases.json'))).cases
  .filter((e: { variant: string; target: string }) => e.variant === 'kind-common-conservative' && e.target.startsWith('camera')));
if (mode === 'full') entries = [
  { target: 'camera-2018', horizon: 15, inputDirectory: 'modular-1d5b31b17cfb' },
  { target: 'camera-2022', horizon: 15, inputDirectory: 'modular-939576fd812f' },
];
if (mode === 'pilot') entries = entries.filter(e => e.horizon === 15);
if (process.env.CAMERA_TARGET) entries = entries.filter(e => e.target === process.env.CAMERA_TARGET);
if (process.env.CAMERA_HORIZON) entries = entries.filter(e => e.horizon === Number(process.env.CAMERA_HORIZON));
const codePaths = ['web/scripts/acceptance/current-camera-models.ts', 'web/scripts/acceptance/current-camera-comparison.ts',
  'web/scripts/acceptance/filing-aware-scores.ts', 'web/scripts/acceptance/filing-aware-monte-carlo.ts',
  ...['information', 'profiles', 'component-policy', 'legacy-components', 'feasible-field', 'reference-noise', 'forecast-random', 'interventions', 'projection']
    .map(p => 'web/src/lib/politics/reform-2026/vote-share/' + p + '.ts')];
const report = { schemaVersion: 1, mode, draws, started: new Date().toISOString(),
  scope: 'Camera-only test preparation exception; current kernels on the frozen cutoff-specific research component panels. No Senate fit/adaptation and no historical scrutiny.',
  catalogId: pointer.catalogId, sourceId: source.id, inputVersions: input.versions,
  codeHashes: Object.fromEntries(await Promise.all(codePaths.map(async path => [path, hash(await readFile(resolve(root, path)))]))),
  rows: [] as object[], failures: [] as object[] };
for (const entry of entries) {
  const folder = entry.inputDirectory.split(/[\\/]/).at(-1)!,
    path = resolve(root, 'data/derived/party-drift', folder, 'profiles.json'), bytes = await readFile(path),
    historical = JSON.parse(bytes.toString()) as ReplaySource;
  historical.truth.sort((a, b) => a.id.localeCompare(b.id));
  const d = design(historical), seed = 'current-camera-comparison-v1:' + entry.target + ':' + entry.horizon,
    cells: PhysicalCell[] = [], mapping: AreaMapping[] = [], territories: AreaMapping[] = [], unmatched: string[] = [];
  for (const t of d.targets) {
    const at = t.id.indexOf(':'), codes = keys.get(t.id.slice(0, at) + ':' + normalizeName(t.id.slice(at + 1))),
      code = codes?.size === 1 ? [...codes][0] : undefined,
      parts = code ? [...geography.get(code) ?? []].sort((a, b) => a.unitId.localeCompare(b.unitId) || a.collegeId.localeCompare(b.collegeId)) : [];
    if (!code || !parts.length) {
      unmatched.push(t.id);
      cells.push({ id: JSON.stringify(['camera', t.id, 'unmatched']), municipality: t.id, region: t.region,
        province: '', collegeId: 'unmatched:' + t.region, chamber: 'camera', electors: t.electors });
      continue;
    }
    const weights = parts.map(p => p.population), counts = apportion(t.electors, weights,
      forecastRandom(seed, 0, 'electorate-reconstruction:camera:' + code).uniform),
      areas = parts.map(p => ({ id: p.collegeId, weight: p.population / targets.get(code)!.population }));
    parts.forEach((p, i) => cells.push({ id: JSON.stringify(['camera', code, p.unitId, p.collegeId]), municipality: t.id,
      region: t.region, province: targets.get(code)!.province, collegeId: p.collegeId, chamber: 'camera', electors: counts[i] }));
    mapping.push({ municipality: t.id, areas });
    const byTerritory = new Map<string, number>();
    areas.forEach(a => { const id = colleges.get(a.id)!.territoryId; byTerritory.set(id, (byTerritory.get(id) ?? 0) + a.weight); });
    territories.push({ municipality: t.id, areas: [...byTerritory].map(([id, weight]) => ({ id, weight })) });
  }
  const preparationStarted = performance.now(),
    // These sources contain only historical counts; targets have no held-out counts.
    { truth: _truth, nationalTruth: _nationalTruth, ...training } = historical,
    stratified = restoreOriginalStrata(training, input),
    fitted = prepareCameraM02(stratified, d.targets, seed),
    legacy = prepareCameraM01(d, input, loaded.scenario, loaded.context, cells,
      [...colleges.values()].filter(c => c.chamber === 'camera' && c.kind === 'ordinary').map(c => c.id)
        .concat([...new Set(cells.filter(c => c.collegeId.startsWith('unmatched:') && c.region !== '04' && c.region !== '02').map(c => c.collegeId))]), seed),
    anchors = legacy.setPolls(historical.polls), preparationMs = performance.now() - preparationStarted;
  await writeFile(resolve(output, entry.target + '-' + entry.horizon + '-' + mode + '-inputs.json'), JSON.stringify({
    sourceHash: hash(bytes), design: d, seed, cells, compositions: legacy.compiled.compositions, anchors,
    baseline: legacy.compiled.audit, preparationMs,
    originalStrata: stratified.nationalHistory.map(e => ({ id: e.id, strata: e.strata?.length })),
  }));
  for (const model of (process.env.CAMERA_MODELS ?? 'M01,M02').split(',') as ('M01' | 'M02')[]) {
    if (!['M01', 'M02'].includes(model)) throw new Error('Only current models permitted');
    const started = performance.now(), encoded: Uint32Array[] = [], n = fitted.fit.componentIds.length + 1, modelSeed = seed + ':' + model;
    try {
      for (let draw = 1; draw <= draws; draw++) {
        encoded.push(generateCameraDraw(model, d, cells, modelSeed, draw, loaded.context, fitted, legacy, anchors));
        if (draw % 64 === 0) console.log(JSON.stringify({ mode, target: entry.target, horizon: entry.horizon, model, draw, draws, seconds: (performance.now() - started) / 1000 }));
      }
      const generationMs = performance.now() - started, ids = d.targets.map(t => t.id),
        position = new Map(fitted.fit.componentIds.map((id, j) => [id, j + 1])),
        municipal: { unit: string; score: ReturnType<typeof distributionScores> }[] = [],
        elector: typeof municipal = [], details: object[] = [], tail = new JointTailScores(draws);
      historical.truth.forEach((t, i) => {
        t.groups.forEach((members, g) => {
          const counts = encoded.map(row => members.reduce((v, id) => v + row[i * n + position.get(id)!], 0)),
            values = counts.map((v, draw) => v / Math.max(1, t.electors - encoded[draw][i * n])),
            actual = t.counts[g] / Math.max(1, t.electors - t.nonValid), score = distributionScores(values, actual, t.electors - t.nonValid);
          municipal.push({ unit: t.id, score }); tail.add(t.id, values, actual);
          elector.push({ unit: t.id, score: distributionScores(counts.map(v => v / t.electors), t.counts[g] / t.electors, t.electors) });
          details.push({ municipality: t.id, members, truth: actual, score });
        });
        elector.push({ unit: t.id, score: distributionScores(encoded.map(row => row[i * n] / t.electors), t.nonValid / t.electors, t.electors) });
      });
      const college = scoreAreas(historical, ids, fitted.fit.componentIds, encoded, mapping),
        constituency = scoreAreas(historical, ids, fitted.fit.componentIds, encoded, territories),
        area = scoreAreas(historical, ids, fitted.fit.componentIds, encoded,
          ids.map(municipality => ({ municipality, areas: [{ id: 'evaluated-area', weight: 1 }] }))),
        mc = (rows: typeof college.rows) => { const joint = new JointTailScores(draws); rows.forEach(r => joint.add(r.area, r.values, r.truth)); return joint.finish(); };
      const name = entry.target + '-' + entry.horizon + '-' + model + '-' + mode,
        row = { target: entry.target, horizon: entry.horizon, cutoff: d.cutoff, model, modelVersion: '1.0.0', draws, sourceHash: hash(bytes), seed: modelSeed,
          originalStrata: stratified.nationalHistory.map(e => ({ id: e.id, strata: e.strata?.length })),
          municipalities: ids.length, physicalCells: cells.length, unmatched, colleges: college.areas, constituencies: constituency.areas,
          baseline: legacy.compiled.audit.baseline.camera, missingHistory: legacy.compiled.audit.missingHistory.length, anchors: model === 'M01' ? anchors : [],
          preparationMs, generationMs, elapsedMs: performance.now() - started, maximumResidual: 0,
          fitDiagnostics: { political: fitted.fit.political.diagnostics, participation: fitted.fit.participation.diagnostics },
          scores: { municipality: summarizeUnits(municipal), college: college.scores, constituency: constituency.scores, sampledArea: area.scores },
          electorScores: { municipality: summarizeUnits(elector) },
          monteCarlo: { municipality: tail.finish(), college: mc(college.rows), constituency: mc(constituency.rows), sampledArea: mc(area.rows) } };
      report.rows.push(row);
      await writeFile(resolve(output, name + '-scores.json.gz'), gzipSync(JSON.stringify({ municipality: details, college: college.rows,
        constituency: constituency.rows, sampledArea: area.rows })));
      console.log(JSON.stringify({ completed: name, elapsedMs: row.elapsedMs, scores: row.scores.municipality }));
    } catch (error) {
      report.failures.push({ target: entry.target, horizon: entry.horizon, model, error: String(error), completedDraws: encoded.length });
      process.exitCode = 1; console.error(error);
    }
    await writeFile(resolve(output, mode + (process.env.CAMERA_REPORT ? '-' + process.env.CAMERA_REPORT : '') + '-report.json'), JSON.stringify(report, null, 2) + '\n');
  }
}
