/** Full physical/geography boundary check, NOT a proposed production scenario. */
import { readFile, mkdir, writeFile } from 'node:fs/promises';
import { dirname, resolve } from 'node:path';
import {
  decodeAsset,
  type ByteAsset,
  sourceKey
} from '../../src/lib/politics/reform-2026/vote-share/source-codec';
import type { CompactSource } from '../../src/lib/politics/reform-2026/vote-share/source-types';
import { compileSource } from '../../src/lib/politics/reform-2026/vote-share/source-compiler';
import { compilePairedHistory } from '../../src/lib/politics/reform-2026/vote-share/paired-source';
import { fitReference } from '../../src/lib/politics/reform-2026/vote-share/information';
import { fitProfiles } from '../../src/lib/politics/reform-2026/vote-share/profiles';
import { prepareSenateAdapter } from '../../src/lib/politics/reform-2026/vote-share/senate-adapter';
import { prepareFastGeneration } from '../../src/lib/politics/reform-2026/vote-share/generation';
import { fastGenerationFixture } from '../../tests/fixtures/fast-generation-input';
import { getBundledReformSeatTables } from '../../src/lib/politics/reform-2026/bundled-data';
import { runPoliticsReformScrutiny } from '../../src/lib/politics/reform-2026/scrutiny';
import { sum } from '../../src/lib/politics/reform-2026/vote-share/types';
const args = process.argv.slice(2);
if (args.length !== 2 || args[0] !== '--asset')
  throw new Error('Use --asset SOURCE_ID.asset.json; no latest/default source');
const gate = JSON.parse(
  await readFile(resolve('../data/derived/fv04-generation/numerical-acceptance.json'), 'utf8')
);
if (!gate.accepted || gate.reduced.estimated7904MunicipalityMs > 30 * 60000)
  throw new Error('Run the reduced numerical/resource gate first; full-country estimate must be plausible.');
const descriptorPath = resolve(args[1]),
  directory = dirname(descriptorPath),
  descriptor: ByteAsset = JSON.parse(await readFile(descriptorPath, 'utf8'));
const source: CompactSource = JSON.parse(
  new TextDecoder().decode(await decodeAsset(descriptor, await readFile(resolve(directory, descriptor.path))))
);
const read = (path: string) => readFile(resolve(directory, path)),
  referenceDate = '2026-10-19',
  fitSeed = 'fv04-country-boundary';
// Close every original joint group under whole-component pooling. Choose the
// largest connected block versus its complement as two observable test pools.
// This is mechanical test coarsening, not a historical split or political default.
const original = [...new Set(source.payload.choices.flatMap((c) => c.members))].sort(),
  parent = new Map(original.map((id) => [id, id]));
const root = (id: string): string => (parent.get(id) === id ? id : root(parent.get(id)!));
for (const c of source.payload.choices) for (const id of c.members) parent.set(root(id), root(c.members[0]));
const blocks = new Map<string, string[]>();
for (const id of original) {
  const key = root(id);
  blocks.set(key, [...(blocks.get(key) ?? []), id]);
}
const largest = [...blocks.values()].sort((a, b) => b.length - a.length || a[0].localeCompare(b[0]))[0],
  left = new Set(largest);
const componentMap = Object.fromEntries(original.map((id) => [id, left.has(id) ? 'pool-a' : 'pool-b'])),
  components = ['pool-a', 'pool-b'];
const paired = await compilePairedHistory(
  source,
  read,
  { referenceDate, components, componentMap },
  { camera: 'camera-2022', senato: 'senato-2022', electoratePolicy: 'equal-only' }
);
// Conditional 18+ totals are explicitly held at the observed paired-baseline
// vintage, not claimed to predict the number of electors on election day.
const { decodeSourceRows } = await import('../../src/lib/politics/reform-2026/vote-share/source-codec');
const ce = source.payload.events.find((e) => e.id === 'camera-2022')!,
  rows = await decodeSourceRows(source, ce.id, await read(ce.chunk.path));
const units = new Map(source.payload.units.map((u) => [u.id, u])),
  electorates = new Map(
    rows.filter((r) => units.get(r.unit)?.targetCode).map((r) => [units.get(r.unit)!.targetCode!, r.electors])
  );
const targets = source.payload.geography.targets.map((t) => {
  const electors = electorates.get(t.code);
  if (!electors) throw new Error('Missing conditional baseline electorate ' + t.code);
  return { code: t.code, electors };
});
const compiled = await compileSource(source, read, {
  referenceDate,
  components,
  componentMap,
  targets,
  targetContext: 'camera',
  electionIds: source.payload.events.filter((e) => e.role === 'reference').map((e) => e.id),
  pollIds: []
});
let t = performance.now();
const fit = fitReference(compiled.national, fitSeed),
  fitMs = performance.now() - t;
const reducedIds = new Set(compiled.profiles.targets.slice(0, 128).map((t) => t.id));
t = performance.now();
const reduced = fitProfiles({
  ...compiled.profiles,
  targets: compiled.profiles.targets.filter((t) => reducedIds.has(t.id)),
  elections: compiled.profiles.elections.map((e) => ({
    ...e,
    rows: e.rows.filter((r) => reducedIds.has(r.id))
  }))
});
const reducedProfileMs = performance.now() - t,
  estimate = (reducedProfileMs * targets.length) / 128;
console.log(
  JSON.stringify({
    phase: 'reduced-source-backed-gate',
    fitMs,
    reducedProfileMs,
    estimatedFullProfileMs: estimate,
    estimatedGenerationMs: gate.reduced.estimated7904MunicipalityMs,
    profileWorkspace: reduced.audit.workspaceBytes
  })
);
if (estimate > 30 * 60000)
  throw new Error('Reduced profile estimate exceeds 30 minutes; full execution stopped');
t = performance.now();
const cameraProfile = fitProfiles(compiled.profiles),
  profileMs = performance.now() - t;
t = performance.now();
const senateAdapter = prepareSenateAdapter(cameraProfile, referenceDate, paired.history),
  adapterMs = performance.now() - t;
const f = fastGenerationFixture();
f.context.seats = getBundledReformSeatTables();
f.scenario.referenceDate = referenceDate;
f.scenario.fitSeed = fitSeed;
f.scenario.components = components.map((id) => ({
  id,
  kind: 'fitted',
  provenance: 'Explicit whole-observable-block test pooling; not a default'
}));
for (const ch of ['camera', 'senato'] as const)
  f.scenario.lists[ch].forEach((l, i) => (l.componentIds = [components[i]]));
const fittedStateId = await sourceKey('fv04-boundary-fit', { national: compiled.nationalInputKey, fitSeed }),
  profileStateId = compiled.profileInputKey;
const assets = {
  ...f.assets,
  fit,
  cameraProfile,
  senateAdapter,
  fitSeed,
  profileReferenceDate: referenceDate,
  geography: source.payload.geography,
  sourceBundleId: source.id,
  fittedStateId,
  profileStateId,
  electorates: targets.map((t) => ({
    code: t.code,
    camera: t.electors,
    senato: t.electors,
    date: ce.date,
    provenance:
      'Conditional common 18+ Camera-2022 electorate held constant for a source-backed generation boundary check; not a future-electorate forecast.'
  }))
};
f.scenario.data = {
  sourceBundleId: source.id,
  fittedStateId,
  profileStateId,
  geographyVersion: assets.geography.version
};
t = performance.now();
const prepared = prepareFastGeneration(f.scenario, assets, f.context),
  preparationMs = performance.now() - t;
t = performance.now();
const result = prepared.run(1),
  generationMs = performance.now() - t;
const scrutiny = runPoliticsReformScrutiny(result.input),
  nonComputable = scrutiny.stages.filter((s) => s.status === 'non-computable');
const totals = ['camera', 'senato'].map((ch) => ({
  chamber: ch,
  electors: sum(prepared.cells[ch as 'camera' | 'senato'].map((c) => c.electors))
}));
if (totals.some((t) => t.electors !== sum(targets.map((t) => t.electors))))
  throw new Error('Full-country electorate conservation failed');
const report = {
  step: 'FV-04',
  sourceId: source.id,
  accepted: true,
  qualification:
    'Source-backed physical-boundary mechanics only. Two mechanically pooled observable blocks, no polls, constructed preference/dependence settings. Not default selection, political predictive validation or maximum-envelope acceptance.',
  pooling: { poolA: largest, poolB: original.filter((id) => !left.has(id)) },
  municipalities: targets.length,
  physicalCells: prepared.cells.camera.length + prepared.cells.senato.length,
  totals,
  paired: paired.audit,
  fitMs,
  reducedProfileMs,
  estimatedFullProfileMs: estimate,
  profileMs,
  adapterMs,
  preparationMs,
  generationMs,
  projectionResidual: result.audit.projectionResidual,
  projectionIterations: result.audit.projectionIterations,
  nonComputable,
  peakRssKiB: process.resourceUsage().maxRSS
};
const path = resolve('../data/derived/fv04-generation/country-boundary.json');
await mkdir(dirname(path), { recursive: true });
await writeFile(path, JSON.stringify(report, null, 2) + '\n');
console.log(
  JSON.stringify(
    {
      ...report,
      pooling: undefined,
      paired: { ...report.paired, excluded: report.paired.excluded.length },
      report: path
    },
    null,
    2
  )
);
