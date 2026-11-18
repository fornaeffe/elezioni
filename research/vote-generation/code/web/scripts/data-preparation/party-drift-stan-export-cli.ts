import { mkdir, readFile, readdir, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { basis, zeros } from './drift-math';
import { calibrateReferenceOffset, makeReferenceWalk } from './party-drift-prior';
import { calibrateSoftReference, eliminateHardReference, type ReferenceCell } from './party-drift-reference';
import type { ReducedPartyProblem } from './party-drift-fit';
import { sha256 } from './sources';

const root = fileURLToPath(new URL('../../../', import.meta.url));
const modelHash = sha256(await readFile(resolve(root, 'web/scripts/data-preparation/stan/restricted-party-drift.stan')));
const sourceFiles = ['party-drift-stan-export-cli.ts', 'party-drift-reference.ts', 'party-drift-prior.ts', 'party-drift-fit.ts', 'party-drift-likelihood.ts', 'drift-math.ts', 'stan/run_restricted.py', 'stan/requirements.txt'];
const sourceVersions = await Promise.all(sourceFiles.map(async path => ({ path, sha256: sha256(await readFile(resolve(root, 'web/scripts/data-preparation', path))) })));
let referenceSummary:string|undefined;
for(const name of (await readdir(resolve(root,'data/derived/party-drift'))).filter(n=>n.startsWith('restricted-party-drift-')).sort()) {
  const path='data/derived/party-drift/'+name+'/summary.json';
  let summary:{candidatePassed:boolean;versions:{path:string;sha256:string}[]};
  try {summary=JSON.parse(await readFile(resolve(root,path),'utf8'));} catch {continue;}
  if(!summary.candidatePassed)continue;
  let matches=true;for(const item of summary.versions)if(sha256(await readFile(resolve(root,'web/scripts/data-preparation',item.path)))!==item.sha256)matches=false;
  if(matches)referenceSummary=path;
}
if(!referenceSummary)throw new Error('Run data:forecast:party:restricted first: a passed independent reference artifact matching the current sources is required');
const referenceHash = sha256(await readFile(resolve(root, referenceSummary)));
const version = sha256(JSON.stringify({ modelHash, sourceVersions, referenceHash })).slice(0, 12);
const dir = resolve(root, 'data/derived/party-drift/stan-reduced-' + version);
await mkdir(dir, { recursive: true });
const cases = [];
for (const specification of [{ id: 'soft-15pp-one-city', cities: 1, hard: 0, sd: 15 }, { id: 'soft-15pp-eight-cities', cities: 8, hard: 0, sd: 15 }, { id: 'hard-one-city', cities: 1, hard: 1, sd: 1 }]) {
  const cities = Array.from({ length: specification.cities }, (_, i) => 'c' + i), context = { components: 3, cells: cities.map(() => ({ electors: 100, available: [1, 2, 3] })), nationalSd: 1.5, localSd: .5, kindSd: 0 };
  const calibration = calibrateReferenceOffset(context), soft = calibrateSoftReference(.2, specification.sd);
  const walk = makeReferenceWalk({ j: 4, cities, dates: ['2010-01-01'], referenceDate: '2010-01-01', offset: calibration.offset, nationalSd: 1.5, localSd: .5, nationalInnovationSd: .35, localInnovationSd: .25 });
  const reference: ReferenceCell[] = cities.map(city => ({ state: walk.state(city, '2010-01-01'), electors: 100, groups: [[0], [1], [2], [3]] }));
  const design = reference.flatMap(c => Array.from({ length: 4 }, (_, r) => Array.from(c.state.design.slice(r * walk.dimension, (r + 1) * walk.dimension))));
  const offset = reference.flatMap(c => Array.from(c.state.offset)), direction = zeros(walk.dimension, 1), h = basis(4);
  for (let k = 0; k < 3; k++) direction[k] = h[3 + k] / 1.5;
  const original: ReducedPartyProblem = { dimension: walk.dimension, factors: reference.map(c => ({ state: c.state, observation: { groups: [[0], [1, 2], [3]], counts: [30, 40, 30], electors: 100 } })) };
  const constrained = eliminateHardReference(original, reference, 1, .2, direction), n = walk.dimension, width = n - specification.hard;
  const complement = Array.from({ length: n }, (_, r) => Array.from({ length: width }, (_, c) => specification.hard ? constrained.complement[r * width + c] : Number(r === c)));
  const data = { D: n, C: cities.length, hard: specification.hard, design, state_offset: offset, counts: cities.map(() => [30, 40, 30]), electors: cities.map(() => 100), reference_mean: .2,
    reference_location: soft.location, reference_scale: soft.scale, complement, direction: Array.from(constrained.direction) };
  await writeFile(resolve(dir, specification.id + '.json'), JSON.stringify(data) + '\n');
  cases.push({ ...specification, dataFile: specification.id + '.json', dataSha256: sha256(Buffer.from(JSON.stringify(data) + '\n')), calibration, soft,
    referenceCaseId: specification.id === 'hard-one-city' ? 'hard-reference' : specification.cities === 1 ? 'soft-reference-15pp' : null });
}
await writeFile(resolve(dir, 'manifest.json'), JSON.stringify({ version, modelHash, sourceVersions, referenceSummary, referenceHash, cases, policy: { chains: 4, warmup: 1000, sampling: 2000, adaptDelta: .95, maxTreedepth: 12, seed: 20261026, maxRhat: 1.05, minEss: 400, maxDivergences: 0 }, scope: 'Reduced gold-standard validation only; no full-data or production fit' }, null, 2) + '\n');
console.log(JSON.stringify({ path: dir, modelHash, cases: cases.map(c => c.id) }));
