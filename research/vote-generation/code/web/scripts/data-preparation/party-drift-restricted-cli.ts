import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { cpus, totalmem } from 'node:os';
import { createNormalSampler, createSeededRng } from '../../src/lib/core/rng';
import { basis, chol, dot, inverse, mv, zeros, type Mat } from './drift-math';
import { aggregatePoliticalShares, groupedProbabilities, pollContrastFactor } from './party-drift-likelihood';
import { calibrateReferenceOffset, evaluateAffine, makeReferenceWalk, PARTY_PRIOR_POLICY, type AffineScores } from './party-drift-prior';
import { fitReducedParty, laplacePartyDraws, reducedChainDiagnostics, REDUCED_FIT_POLICY, sampleReducedParty, type ReducedPartyProblem } from './party-drift-fit';
import { calibrateSoftReference, eliminateHardReference, referenceValue, softReferenceLogDensity, type ReferenceCell } from './party-drift-reference';
import { reducedImportance } from './party-drift-importance';
import { componentResolution } from './party-drift-identifiability';
import { summarizeScalarSamples } from './generation-metrics';
import { sha256 } from './sources';

const root = fileURLToPath(new URL('../../../', import.meta.url));
if (process.argv.length !== 2) throw new Error('Usage: party-drift-restricted-cli');
const policy = { version: 'restricted-scenarios-v1', reference: { burn: 6000, draws: 12000, thin: 2, chains: 4 }, starts: 5,
  referenceDrawsByFixture: { 'separate-history': 48000 },
  importanceDraws: 32768, minImportanceEss: 1000, maxImportanceWeight: .02, prior: PARTY_PRIOR_POLICY, gates: REDUCED_FIT_POLICY };
const files = ['party-drift-restricted-cli.ts', 'party-drift-fit.ts', 'party-drift-prior.ts', 'party-drift-likelihood.ts', 'party-drift-reference.ts', 'party-drift-importance.ts', 'party-drift-identifiability.ts', 'drift-math.ts', 'drift-gaussian.ts', 'generation-metrics.ts'];
const versions = await Promise.all(files.map(async p => ({ path: p, sha256: sha256(await readFile(resolve(root, 'web/scripts/data-preparation', p))) })));
const version = 'restricted-party-drift-' + sha256(JSON.stringify({ policy, versions })).slice(0, 12), dir = resolve(root, 'data/derived/party-drift', version);
await mkdir(dir, { recursive: true });
const fixtures = [
  { id: 'separate-history', evidence: 'counts', cities: 1 },
  { id: 'separate-poll', evidence: 'poll', cities: 1 },
  { id: 'hard-reference', evidence: 'hard', cities: 1 },
  { id: 'soft-reference-1pp', evidence: 'soft', sdPp: 1, cities: 1 },
  { id: 'soft-reference-5pp', evidence: 'soft', sdPp: 5, cities: 1 },
  { id: 'soft-reference-15pp', evidence: 'soft', sdPp: 15, cities: 1 },
  { id: 'hard-reference-shared-geography', evidence: 'hard', cities: 2 },
  { id: 'soft-reference-shared-geography', evidence: 'soft', sdPp: 5, cities: 2 }
];
const results = [], start = performance.now();
for (const fixture of fixtures) {
  console.log(JSON.stringify({ phase: 'start', fixture }));
  const cityIds = Array.from({ length: fixture.cities }, (_, i) => 'city-' + i), date = '2010-01-01';
  const context = { components: 3, cells: cityIds.map(() => ({ electors: 100, available: [1, 2, 3] })), nationalSd: 1.5, localSd: .5, kindSd: 0 };
  const calibration = calibrateReferenceOffset(context);
  const walk = makeReferenceWalk({ j: 4, cities: cityIds, dates: fixture.evidence === 'counts' ? ['2009-01-01', date] : [date], referenceDate: date,
    offset: calibration.offset, nationalSd: 1.5, localSd: .5, nationalInnovationSd: .35, localInnovationSd: .25 });
  const dimension = walk.dimension + (fixture.evidence === 'poll' ? 2 : 0);
  const state = (city: string, time: string): AffineScores => {
    const s = walk.state(city, time), design = zeros(4, dimension);
    for (let r = 0; r < 4; r++) for (let c = 0; c < walk.dimension; c++) design[r * dimension + c] = s.design[r * walk.dimension + c];
    return { offset: s.offset, design, dimension };
  };
  const reference: ReferenceCell[] = cityIds.map(city => ({ state: state(city, date), electors: 100, groups: [[0], [1], [2], [3]] }));
  const original: ReducedPartyProblem = { dimension, factors: cityIds.map((city, i) => ({ state: state(city, date), observation: {
    groups: [[0], [1, 2], [3]], counts: i ? [20, 50, 30] : [30, 40, 30], electors: 100 } })) };
  let soft;
  if (fixture.evidence === 'counts') original.factors.push({ state: state(cityIds[0], '2009-01-01'), observation: { groups: [[0], [1], [2], [3]], counts: [90, 45, 75, 90], electors: 300 } });
  if (fixture.evidence === 'soft') {
    soft = calibrateSoftReference(.2, fixture.sdPp!);
    original.extraLogLikelihood = x => softReferenceLogDensity(referenceValue(reference, x, 1), soft!);
  }
  if (fixture.evidence === 'poll') {
    const printed = [.2, .37, .43], cov = pollContrastFactor(printed, printed, 400).covariance, inv = inverse(chol(cov, 2), 2), h = basis(3);
    original.extraLogLikelihood = x => {
      const bias = [0, ...Array.from({ length: 3 }, (_, j) => .12 * (h[j * 2] * x[walk.dimension] + h[j * 2 + 1] * x[walk.dimension + 1]))];
      const prediction = aggregatePoliticalShares(reference.map(c => ({ ...c, scores: evaluateAffine(c.state, x) })), bias);
      const residual = pollContrastFactor(prediction, printed, 400).residual;
      return -.5 * dot(residual, mv(inv, residual, 2));
    };
  }
  let problem = original, decode = (x: Mat) => x;
  if (fixture.evidence === 'hard') {
    const direction = zeros(dimension, 1), h = basis(4);
    for (let k = 0; k < 3; k++) direction[k] = h[3 + k] / 1.5;
    const constrained = eliminateHardReference(original, reference, 1, .2, direction);
    problem = constrained.problem; decode = x => constrained.decode(x).x;
  }
  const resolution = componentResolution(['a', 'b', 'c'], [{ id: 'joint', groups: [['a', 'b'], ['c']] },
    { id: fixture.evidence, groups: fixture.evidence === 'counts' || fixture.evidence === 'poll' ? [['a'], ['b'], ['c']] : [['a']] }]);
  if (!resolution.resolved) throw new Error('Restricted fixture is unresolved');
  const normal = createNormalSampler(createSeededRng('restricted-starts:' + fixture.id));
  const fits = Array.from({ length: policy.starts }, (_, i) => fitReducedParty(problem, i ? Float64Array.from({ length: problem.dimension }, () => normal(0, .5)) : undefined));
  const modes = fits.filter(f => f.converged && f.factor).sort((a, b) => b.logPosterior - a.logPosterior);
  if (!modes.length) throw new Error('Restricted fixture has no usable mode: ' + fixture.id);
  const values = (z: Mat) => {
    const x = decode(z), p = reference.map(c => groupedProbabilities(evaluateAffine(c.state, x), c.groups));
    const national = Array.from({ length: 4 }, (_, j) => p.reduce((s, cell) => s + cell[j], 0) / p.length), first = evaluateAffine(reference[0].state, x);
    return [...national, national[1] / (national[1] + national[2]), first[1] - first[2]];
  };
  const chains: number[][][] = [], sampling = [];
  for (let chain = 0; chain < policy.reference.chains; chain++) {
    const retained = policy.referenceDrawsByFixture[fixture.id as keyof typeof policy.referenceDrawsByFixture] ?? policy.reference.draws;
    const sample = sampleReducedParty(problem, modes[chain % modes.length].mode, { ...policy.reference, draws: retained, seed: 'restricted-reference:' + fixture.id + ':' + chain });
    chains.push(sample.draws.map(values)); sampling.push({ evaluations: sample.evaluations, seconds: sample.seconds });
    console.log(JSON.stringify({ phase: 'reference-chain', fixture: fixture.id, chain, seconds: sample.seconds }));
  }
  const diagnostics = Array.from({ length: 6 }, (_, k) => reducedChainDiagnostics(chains.map(c => c.map(x => x[k]))));
  const truth = chains.flat(), samplingPassed = diagnostics.every(d => d.rhat <= policy.gates.maxRhat && d.ess >= policy.gates.minEss);
  function compare(draws: Mat[]) {
    const predictions = draws.map(values);
    const variables = Array.from({ length: 6 }, (_, k) => {
      const a = summarizeScalarSamples(truth.map(x => x[k]), 0), b = summarizeScalarSamples(predictions.map(x => x[k]), 0);
      return { referenceMean: a.predictiveMean, candidateMean: b.predictiveMean, meanDifference: b.predictiveMean - a.predictiveMean, referenceWidth90: a.width90, candidateWidth90: b.width90, widthRatio: b.width90 / a.width90 };
    });
    return { variables, passed: variables.every((v, k) => (k === 5 || Math.abs(v.meanDifference) <= policy.gates.posteriorMeanTolerance) && v.widthRatio >= .65 && v.widthRatio <= 1.5) };
  }
  const laplace = compare(laplacePartyDraws(problem, modes[0], 48000, 'restricted-laplace:' + fixture.id));
  let alternative;
  if (!laplace.passed) {
    const sample = reducedImportance(problem, modes[0], policy.importanceDraws, 'restricted-importance:' + fixture.id);
    const check = compare(sample.draws);
    const replicate = reducedImportance(problem, modes[0], policy.importanceDraws, 'restricted-importance-replicate:' + fixture.id), replicateCheck = compare(replicate.draws);
    alternative = { method: 'exact-density Student-t mixture importance', ...check, ess: sample.ess, maxWeight: sample.maxWeight, seconds: sample.seconds,
      replicate: { ...replicateCheck, ess: replicate.ess, maxWeight: replicate.maxWeight, seconds: replicate.seconds },
      passed: check.passed && replicateCheck.passed && Math.min(sample.ess, replicate.ess) >= policy.minImportanceEss && Math.max(sample.maxWeight, replicate.maxWeight) <= policy.maxImportanceWeight };
  }
  const hardResidual = fixture.evidence === 'hard' ? Math.max(...modes.map(m => Math.abs(referenceValue(reference, decode(m.mode), 1) - .2))) : null;
  const row = { fixture, dimension: problem.dimension, resolution, soft, samplingPassed, diagnostics, sampling, hardResidual, laplace, alternative,
    modes: fits.map(f => ({ converged: f.converged, positiveDefinite: f.positiveDefinite, gradient: f.gradient, logPosterior: f.logPosterior, seconds: f.seconds })) };
  results.push(row);
  await writeFile(resolve(dir, fixture.id + '.json'), JSON.stringify({ version, policy, versions, ...row }, null, 2) + '\n');
  console.log(JSON.stringify({ phase: 'fixture-complete', fixture: fixture.id, samplingPassed, laplacePassed: laplace.passed, alternativePassed: alternative?.passed,
    maxMeanDifference: Math.max(...laplace.variables.slice(0, 5).map(v => Math.abs(v.meanDifference))) }));
}
const summary = { version, machine: { cpu: cpus()[0].model, logicalProcessors: cpus().length, memoryBytes: totalmem(), node: process.version }, policy, versions, results,
  seconds: (performance.now() - start) / 1000, laplacePassed: results.every(r => r.samplingPassed && r.laplace.passed),
  candidatePassed: results.every(r => r.samplingPassed && (r.laplace.passed || r.alternative?.passed)), productionChanged: false };
await writeFile(resolve(dir, 'summary.json'), JSON.stringify(summary, null, 2) + '\n');
console.log(JSON.stringify({ phase: 'restricted-complete', version, path: resolve(dir, 'summary.json'), seconds: summary.seconds, laplacePassed: summary.laplacePassed, candidatePassed: summary.candidatePassed }));
if (!summary.candidatePassed) process.exitCode = 2;
