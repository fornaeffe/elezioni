/** Increasing-geography check for global importance correction: the same
 * admitted shared-national model, with a broad aggregate reference share. */
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { zeros } from './drift-math';
import { calibrateReferenceOffset, makeReferenceWalk } from './party-drift-prior';
import { fitReducedParty, type ReducedPartyProblem } from './party-drift-fit';
import { calibrateSoftReference, referenceValue, softReferenceLogDensity } from './party-drift-reference';
import { reducedImportance } from './party-drift-importance';
import { sha256 } from './sources';

const root = fileURLToPath(new URL('../../../', import.meta.url));
const files = ['party-drift-importance-stress-cli.ts', 'party-drift-fit.ts', 'party-drift-importance.ts', 'party-drift-reference.ts', 'party-drift-likelihood.ts', 'party-drift-prior.ts', 'drift-math.ts', 'drift-gaussian.ts'];
const versions = await Promise.all(files.map(async p => ({ path: p, sha256: sha256(await readFile(resolve(root, 'web/scripts/data-preparation', p))) })));
const policy = { cities: [1, 8, 32, 60], draws: 32768, minEss: 1000, maxWeight: .02, share: .2, sdPp: 15 };
const version = 'importance-geography-' + sha256(JSON.stringify({ versions, policy })).slice(0, 12), dir = resolve(root, 'data/derived/party-drift', version);
await mkdir(dir, { recursive: true });
const results = [], soft = calibrateSoftReference(policy.share, policy.sdPp);
for (const count of policy.cities) {
  console.log(JSON.stringify({ phase: 'start', cities: count }));
  const cities = Array.from({ length: count }, (_, i) => 'c' + i), context = { components: 3, cells: cities.map(() => ({ electors: 100, available: [1, 2, 3] })), nationalSd: 1.5, localSd: .5, kindSd: 0 };
  const calibration = calibrateReferenceOffset(context, 8192);
  const walk = makeReferenceWalk({ j: 4, cities, dates: ['2010-01-01'], referenceDate: '2010-01-01', offset: calibration.offset, nationalSd: 1.5, localSd: .5, nationalInnovationSd: .35, localInnovationSd: .25 });
  const reference = cities.map(city => ({ state: walk.state(city, '2010-01-01'), electors: 100, groups: [[0], [1], [2], [3]] }));
  const problem: ReducedPartyProblem = { dimension: walk.dimension, factors: reference.map(c => ({ state: c.state, observation: { groups: [[0], [1, 2], [3]], counts: [30, 40, 30], electors: 100 } })),
    extraLogLikelihood: x => softReferenceLogDensity(referenceValue(reference, x, 1), soft) };
  const fit = fitReducedParty(problem, zeros(problem.dimension, 1));
  console.log(JSON.stringify({ phase: 'fit', cities: count, dimension: problem.dimension, seconds: fit.seconds, converged: fit.converged, gradient: fit.gradient }));
  if (!fit.converged || !fit.factor) { results.push({ cities: count, dimension: problem.dimension, passed: false, reason: 'mode failure', gradient: fit.gradient }); break; }
  const replicate = [0, 1].map(seed => {
    const result = reducedImportance(problem, fit, policy.draws, 'importance-geography:' + count + ':' + seed);
    return { ess: result.ess, maxWeight: result.maxWeight, seconds: result.seconds, evaluations: result.evaluations };
  });
  const row = { cities: count, dimension: problem.dimension, fitSeconds: fit.seconds, replicate,
    passed: replicate.every(r => r.ess >= policy.minEss && r.maxWeight <= policy.maxWeight) };
  results.push(row); console.log(JSON.stringify({ phase: 'complete', ...row }));
  await writeFile(resolve(dir, 'summary.json'), JSON.stringify({ version, versions, policy, results, productionChanged: false }, null, 2) + '\n');
  if (!row.passed) break;
}
if (results.some(r => !r.passed)) process.exitCode = 2;
