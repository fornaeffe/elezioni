import { readFile, readdir, mkdir, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { cpus, totalmem } from 'node:os';
import { createNormalSampler, createSeededRng } from '../../src/lib/core/rng';
import { zeros, type Mat } from './drift-math';
import { groupedCountDerivatives, groupedCountLogLikelihood, groupedProbabilities } from './party-drift-likelihood';
import { groupedReferenceQuadrature } from './party-drift-quadrature';
import { calibrateReferenceOffset, evaluateAffine, makeReferenceWalk, PARTY_PRIOR_POLICY } from './party-drift-prior';
import { fitReducedParty, laplacePartyDraws, REDUCED_FIT_POLICY, reducedChainDiagnostics, sampleReducedParty, type ReducedPartyProblem } from './party-drift-fit';
import { summarizeScalarSamples } from './generation-metrics';
import { sha256 } from './sources';

const root = fileURLToPath(new URL('../../../', import.meta.url));
const args = process.argv.slice(2), command = args.shift();
if (!['validate', 'profile'].includes(command ?? '') || args.length) throw new Error('Usage: party-drift-research-cli validate|profile');
const sources = ['party-drift-contract.ts', 'party-drift-likelihood.ts', 'party-drift-prior.ts', 'party-drift-fit.ts', 'party-drift-research-cli.ts',
  'party-drift-quadrature.ts', 'drift-math.ts', 'drift-gaussian.ts', 'generation-metrics.ts', 'sources.ts',
  'generation-history.ts', 'poll-calibration-sources.ts', 'reform-history.ts', 'formats.ts'];
const versions = await Promise.all([...sources.map(n => 'web/scripts/data-preparation/' + n), 'web/src/lib/core/rng.ts', 'web/package-lock.json'].map(async p => ({ path: p, sha256: sha256(await readFile(resolve(root, p))) })));
const policy = { prior: PARTY_PRIOR_POLICY, fit: REDUCED_FIT_POLICY, reference: { chains: 4, burn: 6000, draws: 12000, thin: 2 }, starts: 9,
  quadrature: { step: .04, splitStep: .06, coarseStep: .08, coarseSplitStep: .12, meanTolerance: .005, resolutionTolerance: 1e-4 } };
const version = 'party-drift-stage0-' + sha256(JSON.stringify({ versions, policy })).slice(0, 12);
const output = resolve(root, 'data/derived/drift', version);
await mkdir(output, { recursive: true });
const machine = { cpu: cpus()[0]?.model, logicalProcessors: cpus().length, memoryBytes: totalmem(), node: process.version, platform: process.platform };
// Measurement artifacts include a timestamp: separate files preserve every attempt.
const path = resolve(output, command + '-' + new Date().toISOString().replaceAll(/[:.]/g, '-') + '.json');
if (command === 'profile') {
  // Deliberately a census/evaluation benchmark, NOT a full fit. Accuracy must be
  // green before any countrywide optimizer or stage comparison can be enabled.
  const { loadGenerationHistory } = await import('./generation-history');
  const tic = performance.now(), history = await loadGenerationHistory(root);
  const loadSeconds = (performance.now() - tic) / 1000;
  const events = history.events.map(e => {
    const cells = e.municipalities.filter(m => m.regionId !== '02' && m.matchable && m.electors > 0 && m.validVotes <= m.electors);
    const lists = [...new Set(cells.flatMap(c => c.lists.map(l => l.name)))];
    return { id: e.id, date: e.date, municipalities: cells.length, sourceLists: lists.length,
      maxLocalLists: Math.max(...cells.map(c => c.lists.length)), cells, lists };
  });
  const allCities = [...new Set(events.flatMap(e => e.cells.map(c => c.id)))];
  const allLabels = [...new Set(events.flatMap(e => e.lists))];
  // Source lists remain atomic for this benchmark; no guessed party identity or
  // historical counts are used to define a fit. Hypothetical split count is explicit.
  const benchmarks = [];
  for (const maximum of [32, 128, 512]) {
    const rows = events.flatMap(e => {
      const count = Math.min(maximum, e.cells.length);
      return Array.from({ length: count }, (_, i) => e.cells[Math.floor((i + .5) * e.cells.length / count)]);
    }).map(c => ({
      scores: zeros(c.lists.length + 2, 1), observation: { groups: [[0], [1, 2], ...c.lists.slice(1).map((_, i) => [i + 3])],
        counts: [c.electors - c.validVotes, ...c.lists.map(l => l.votes)], electors: c.electors } }));
    // Only a likelihood-kernel performance probe, not a political prior.
    for (const r of rows) r.scores[0] = Math.log(r.scores.length - 1);
    const times: number[] = [], likelihoodTimes: number[] = [];
    let checksum = 0;
    for (let repeat = 0; repeat < 7; repeat++) {
      const t = performance.now();
      for (const r of rows) checksum += groupedCountDerivatives(r.scores, r.observation).logLikelihood;
      if (repeat) times.push((performance.now() - t) / 1000);
      const l = performance.now();
      for (const r of rows) checksum += groupedCountLogLikelihood(r.scores, r.observation);
      if (repeat) likelihoodTimes.push((performance.now() - l) / 1000);
    }
    times.sort((a, b) => a - b); likelihoodTimes.sort((a, b) => a - b);
    benchmarks.push({ rows: rows.length, medianSeconds: (times[2] + times[3]) / 2, minSeconds: times[0], maxSeconds: times.at(-1),
      likelihoodMedianSeconds: (likelihoodTimes[2] + likelihoodTimes[3]) / 2, checksum });
  }
  const totalRows = events.reduce((n, e) => n + e.cells.length, 0), perSweep = benchmarks.at(-1)!.medianSeconds * totalRows / benchmarks.at(-1)!.rows;
  const likelihoodSweep = benchmarks.at(-1)!.likelihoodMedianSeconds * totalRows / benchmarks.at(-1)!.rows;
  const minimumReferenceEvaluations = policy.reference.chains * (1 + policy.reference.burn + policy.reference.draws * policy.reference.thin);
  const validationPath = (await readdir(output)).filter(n => n.startsWith('validate-')).sort().at(-1);
  const reducedReference = validationPath ? JSON.parse(await readFile(resolve(output, validationPath), 'utf8')) as { results: { sampling?: { evaluations: number }[] }[] } : undefined;
  const observedEvaluations = reducedReference?.results.flatMap(r => r.sampling ? [r.sampling.reduce((s, c) => s + c.evaluations, 0)] : []);
  const result = { version, machine, versions, policy, phase: 'profile-only', historyVersion: history.version, sourceVersions: history.sourceVersions,
    loadSeconds, events: events.map(({ cells: _cells, lists: _lists, ...e }) => e), totalRows, uniqueMunicipalities: allCities.length,
    distinctSourceLabels: allLabels.length, benchmarks, samplePolicy: 'Systematic municipal midpoints across each source event; no fitted values or target scores',
    timing: { estimatedCountDerivativeSweepSeconds: perSweep,
      estimatedCountLikelihoodSweepSeconds: likelihoodSweep,
      hundredSweepsSeconds: perSweep * 100,
      referenceSamplingMinimumEvaluations: minimumReferenceEvaluations,
      referenceSamplingOptimisticSerialSeconds: minimumReferenceEvaluations * likelihoodSweep,
      referenceSamplingExceeds30Minutes: minimumReferenceEvaluations * likelihoodSweep > 1800,
      observedReferenceEvaluations: observedEvaluations,
      projectionUsingReducedProposalCountsSerialSeconds: observedEvaluations?.map(n => n * likelihoodSweep),
      projectionAssumption: 'Same four-chain iteration protocol and reduced-problem proposal counts; single-thread count kernel only. No claim that countrywide mixing or state evaluation would be as fast.',
      fullFitSeconds: null, comparisonPlanSeconds: null,
      reason: 'A count-kernel sweep omits sparse factorization, nonlinear polls, hyperparameter integration, mode exploration and posterior draws. Accuracy gate must pass before a defensible fitting-time estimate exists.' },
    fullFitStarted: false };
  await writeFile(path, JSON.stringify(result, null, 2) + '\n', { encoding: 'utf8', flag: 'wx' });
  console.log(JSON.stringify({ path, ...result, versions: undefined, sourceVersions: undefined }));
} else {
  const tic = performance.now();
  // Prespecified synthetic stage-0 fixtures: no held-out election informs these.
  const fixtures = [
    { id: 'separate-components', groups: [[0], [1], [2], [3]], counts: [[30, 35, 25, 10]] },
    { id: 'inseparable-balanced', groups: [[0], [1, 2], [3]], counts: [[30, 40, 30]] },
    { id: 'inseparable-small-list', groups: [[0], [1, 2], [3]], counts: [[30, 5, 65]] },
    { id: 'inseparable-dominant-list', groups: [[0], [1, 2], [3]], counts: [[15, 80, 5]] },
    { id: 'observed-zero-joint-list', groups: [[0], [1, 2], [3]], counts: [[30, 0, 70]] },
    { id: 'shared-national-split', groups: [[0], [1, 2], [3]], counts: [[30, 40, 30], [20, 50, 30]] }
  ];
  const results = [];
  for (const fixture of fixtures) {
    const cities = fixture.counts.map((_, i) => 'city-' + i), context = { components: 3, cells: cities.map(() => ({ electors: 100, available: [1, 2, 3] })),
      nationalSd: PARTY_PRIOR_POLICY.nationalSd, localSd: PARTY_PRIOR_POLICY.localSd, kindSd: 0 };
    const calibration = calibrateReferenceOffset(context);
    const walk = makeReferenceWalk({ j: 4, cities, dates: ['2010-01-01'], referenceDate: '2010-01-01', offset: calibration.offset,
      nationalSd: context.nationalSd, localSd: context.localSd, nationalInnovationSd: .35, localInnovationSd: .25 });
    const problem: ReducedPartyProblem = { dimension: walk.dimension, factors: fixture.counts.map((counts, i) => ({
      state: walk.state(cities[i], '2010-01-01'), observation: { groups: fixture.groups, counts, electors: counts.reduce((a, b) => a + b, 0) } })) };
    const normal = createNormalSampler(createSeededRng('party-mode-starts:' + fixture.id));
    const starts = Array.from({ length: policy.starts }, (_, i) => i ? Float64Array.from({ length: problem.dimension }, () => normal(0, 1.5)) : zeros(problem.dimension, 1));
    const modes = starts.map(x => fitReducedParty(problem, x)), candidates = modes.filter(m => m.converged && m.factor).sort((a, b) => b.logPosterior - a.logPosterior);
    if (!candidates.length) {
      results.push({ id: fixture.id, fixture, passed: false, reason: 'No converged positive-definite posterior mode', modes }); break;
    }
    const best = candidates[0], sample = laplacePartyDraws(problem, best, policy.reference.draws * policy.reference.chains, 'party-laplace:' + fixture.id);
    // Inspect component probabilities, national split share and latent log ratio.
    const variables = (x: Mat) => {
      const states = problem.factors.map(f => evaluateAffine(f.state, x)), probabilities = states.map(s => groupedProbabilities(s, [[0], [1], [2], [3]]));
      const national = Array.from({ length: 4 }, (_, j) => probabilities.reduce((s, p) => s + p[j], 0) / probabilities.length);
      return [...national, national[1] / (national[1] + national[2]), states[0][1] - states[0][2]];
    };
    const chains: number[][][] = [], sampling = [];
    for (let chain = 0; chain < policy.reference.chains; chain++) {
      const result = sampleReducedParty(problem, candidates[chain % candidates.length].mode, { ...policy.reference, seed: 'party-reference:' + fixture.id + ':' + chain });
      chains.push(result.draws.map(variables)); sampling.push({ evaluations: result.evaluations, seconds: result.seconds });
    }
    const reference = chains.flat(), approximate = sample.map(variables);
    const comparisons = ['non-valid', 'component-a', 'component-b', 'component-c', 'national-a-within-joint', 'local-log-a-over-b'].map((name, k) => {
      const exact = summarizeScalarSamples(reference.map(x => x[k]), 0), laplace = summarizeScalarSamples(approximate.map(x => x[k]), 0);
      const diagnostics = reducedChainDiagnostics(chains.map(c => c.map(x => x[k])));
      return { name, referenceMean: exact.predictiveMean, laplaceMean: laplace.predictiveMean, meanDifference: laplace.predictiveMean - exact.predictiveMean,
        referenceWidth90: exact.width90, laplaceWidth90: laplace.width90, widthRatio: laplace.width90 / exact.width90, ...diagnostics,
        referenceInterval90: exact.intervals[2], laplaceInterval90: laplace.intervals[2] };
    });
    const samplingPassed = comparisons.every(c => c.rhat <= REDUCED_FIT_POLICY.maxRhat && c.ess >= REDUCED_FIT_POLICY.minEss);
    const approximationPassed = comparisons.every((c, i) => (i === 5 || Math.abs(c.meanDifference) <= REDUCED_FIT_POLICY.posteriorMeanTolerance) &&
      c.widthRatio >= REDUCED_FIT_POLICY.posteriorWidthRatio[0] && c.widthRatio <= REDUCED_FIT_POLICY.posteriorWidthRatio[1]);
    let quadrature;
    if (fixture.id === 'inseparable-balanced') {
      const options = { counts: fixture.counts[0] as [number, number, number], offset: calibration.offset, scoreVariance: context.nationalSd ** 2 + context.localSd ** 2 };
      const coarse = groupedReferenceQuadrature({ ...options, step: policy.quadrature.coarseStep, splitStep: policy.quadrature.coarseSplitStep });
      const fine = groupedReferenceQuadrature({ ...options, step: policy.quadrature.step, splitStep: policy.quadrature.splitStep });
      const maxResolutionDifference = Math.max(...fine.means.map((m, k) => Math.abs(m - coarse.means[k])));
      const maxSamplerDifference = Math.max(...fine.means.map((m, k) => Math.abs(m - comparisons[k].referenceMean)));
      quadrature = { coarse, fine, maxResolutionDifference, maxSamplerDifference,
        passed: maxResolutionDifference < policy.quadrature.resolutionTolerance && maxSamplerDifference < policy.quadrature.meanTolerance && fine.boundaryWeightFraction < 1e-8 };
    }
    const result = { id: fixture.id, fixture, dimension: problem.dimension, calibration, samplingPassed, approximationPassed, quadrature,
      passed: samplingPassed && approximationPassed && (quadrature?.passed ?? true), comparisons, sampling,
      modes: modes.map(m => ({ converged: m.converged, positiveDefinite: m.positiveDefinite, gradient: m.gradient,
        logPosterior: m.logPosterior, seconds: m.seconds, iterations: m.iterations.length, output: variables(m.mode) })) };
    results.push(result);
    console.log(JSON.stringify({ phase: 'reduced-posterior', id: fixture.id, passed: result.passed, samplingPassed, approximationPassed,
      maxMeanDifference: Math.max(...comparisons.slice(0, 5).map(c => Math.abs(c.meanDifference))), minWidthRatio: Math.min(...comparisons.map(c => c.widthRatio)), maxWidthRatio: Math.max(...comparisons.map(c => c.widthRatio)),
      maxRhat: Math.max(...comparisons.map(c => c.rhat)), minEss: Math.min(...comparisons.map(c => c.ess)) }));
    // Complete only the diagnostic fixtures after a failed accuracy check. Never
    // proceed to data fitting or stages 1-7 in this command.
  }
  const result = { version, machine, versions, policy, phase: 'stage-0-reduced-conditional-posterior', results,
    passed: results.length === fixtures.length && results.every(r => r.passed), seconds: (performance.now() - tic) / 1000,
    productionChanged: false, fullFitStarted: false,
    remainingStage0: ['Joint scale/hyperparameter inference', 'Exact hard/soft reference posterior conditioning', 'Grouped poll posterior checks', 'Backward-trajectory posterior checks', 'Scenario/default identity preparation'],
    stageComparisonsStarted: false };
  await writeFile(path, JSON.stringify(result, null, 2) + '\n', { encoding: 'utf8', flag: 'wx' });
  console.log(JSON.stringify({ phase: 'validation-complete', path, passed: result.passed, seconds: result.seconds }));
  if (!result.passed) process.exitCode = 2;
}
