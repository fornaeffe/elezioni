import { createSeededRng, createNormalSampler } from '../../src/lib/core/rng';
import { multinomial } from '../../src/lib/politics/reform-2026/generation-math';
import { add, zeros, chol, inverse, psdChol, mm, mv, tr, toScores, softmax, ouStep, type Mat } from './drift-math';
import { localMean, DRIFT_POLICY } from './drift-structure';
import type { DriftFit } from './drift-fit';
import { posteriorDrift } from './drift-fit';
import type { DriftElection } from './drift-data';

interface ReferenceCity {
  id: string; region: string; mean: Mat; design: Mat; covariance: Mat; factor: Mat;
  cross: Mat; crossInverse: Mat; aggregateFromLocal: Mat;
}
export function makeDriftPredictor(fit: DriftFit, electionDate: string, draws: number, seed: string) {
  const { problem, counts, geometry, parameters } = fit, { d, j, layout } = problem, m = layout.m, p = geometry.p;
  const posterior = posteriorDrift(fit, draws, seed), reference: ReferenceCity[] = [];
  const bridge = geometry.geographic.slice(), inversePoll = fit.system.pollInverse;
  const referenceMode = fit.evaluation.mean;
  for (const city of problem.cities) {
    const cell = geometry.cities.get(city.id)!, gp = counts.locals.get(city.id)!;
    const local = localMean(problem, city, gp, referenceMode, problem.cutoffTime), design = layout.design(city, problem.cutoffTime, problem.targetKind, parameters.cityScale.get(city.id) ?? 1);
    const mean = add(city.theta, local.mean);
    for (let r = 0; r < d; r++) city.columns.forEach((c, k) => design[r * m + c] += local.query.f[r * city.columns.length + k]);
    // design includes local affine coefficients; mean must contain the matching
    // affine offset rather than double-count F*g.
    const conditionalMean = add(city.theta, local.query.u);
    const covariance = cell.grid.p.at(-1)!;
    const factor = chol(covariance, d, 1e-12), ci = inverse(factor, d);
    const cross = cell.referenceCross, aggregateFromLocal = mm(tr(cross, d, p), ci, p, d, d);
    const removed = mm(aggregateFromLocal, cross, p, d, p);
    for (let x = 0; x < bridge.length; x++) bridge[x] -= removed[x];
    reference.push({ id: city.id, region: city.regionId, mean: conditionalMean, design, covariance, factor, cross,
      crossInverse: mm(cross, inversePoll, d, p, p), aggregateFromLocal });
  }
  const bridgeFactor = p ? psdChol(bridge, p) : { factor: zeros(0), jitter: 0 };
  const measurementFactor = p ? chol(geometry.measurement, p) : zeros(0);
  const indices = new Map(reference.map((c, i) => [c.id, i]));
  const dt = (Date.parse(electionDate) - Date.parse(problem.cutoff)) / (365.25 * 86400000);
  if (dt < 0) throw new Error('Forecast precedes reference cutoff');
  const localStep = ouStep(dt, parameters.kappa);
  return {
    diagnostics: { hyper: posterior.diagnostics, geographicBridgeJitter: bridgeFactor.jitter },
    draw(simulation: number) {
      const item = posterior.draws[simulation % posterior.draws.length], g = item.global;
      const rng = createSeededRng(seed + ':future:' + simulation), normal = createNormalSampler(rng),
        countRng = createSeededRng(seed + ':count-observation:' + simulation);
      const norm = () => normal(0, 1);
      const residuals = reference.map(c => mv(c.factor, Float64Array.from({ length: d }, norm), d));
      const field = p ? mv(bridgeFactor.factor, Float64Array.from({ length: p }, norm), p) : zeros(0, 1);
      reference.forEach((c, i) => {
        const contribution = mv(c.aggregateFromLocal, residuals[i], p);
        for (let r = 0; r < p; r++) field[r] += contribution[r];
      });
      const measurement = p ? mv(measurementFactor, Float64Array.from({ length: p }, norm), p) : zeros(0, 1);
      const innovation = add(add(add(geometry.response, mv(geometry.jacobian, g, p), -1), field, -1), measurement, -1);
      const etaRaw = Float64Array.from({ length: j }, (_, k) => norm() * item.eta * item.etaShape[k] * Math.sqrt(dt));
      const eta = mv(tr(problem.h, j, d), etaRaw, d);
      const globalDeltaMultiplier = Math.exp(fit.localAudit.scaleLogSd * norm() - fit.localAudit.scaleLogSd ** 2);
      const regionalRaw = new Map(layout.regions.map(region => [region, Float64Array.from({ length: j }, (_, k) =>
        norm() * parameters.delta * parameters.deltaShape[k] * globalDeltaMultiplier * Math.sqrt(dt * parameters.regionFraction))]));
      const regional = new Map([...regionalRaw].map(([r, raw]) => [r, mv(tr(problem.h, j, d), raw, d)]));
      const output = new Float64Array(problem.forecast.length * j);
      let invalid = 0;
      problem.forecast.forEach((row, ci) => {
        const index = indices.get(row.city.id)!, c = reference[index];
        const local = add(residuals[index], mv(c.crossInverse, innovation, d));
        let referenceScore = add(c.mean, add(mv(c.design, g, d), local));
        // For OU, revert the local process to persistent initial geography,
        // retaining the national/kind score difference and its uncertainty.
        if (parameters.kappa) {
          const base = add(row.city.theta, mv(layout.design(row.city, problem.cutoffTime, problem.targetKind, parameters.cityScale.get(row.city.id) ?? 1), g, d));
          referenceScore = add(base, add(referenceScore, base, -1), localStep.phi);
        }
        const logSd = parameters.cityLogSd.get(row.city.id) ?? 0, cityFactor = (parameters.cityScale.get(row.city.id) ?? 1) * Math.exp(logSd * norm() - logSd ** 2);
        const rawLocal = Float64Array.from({ length: j }, (_, k) => norm() * parameters.delta * parameters.deltaShape[k] *
          globalDeltaMultiplier * cityFactor * Math.sqrt(localStep.varianceTime * (1 - parameters.regionFraction)));
        const shock = mv(tr(problem.h, j, d), rawLocal, d);
        const state = add(referenceScore, add(eta, shock));
        for (let k = 0; k < d; k++) state[k] += cityFactor * (regional.get(c.region)?.[k] ?? 0);
        const probabilities = softmax(toScores(state, problem.h));
        const votes = multinomial(row.electors, Array.from(probabilities), countRng);
        invalid += Number(votes.reduce((a, b) => a + b, 0) !== row.electors || votes.some(n => !Number.isSafeInteger(n) || n < 0));
        votes.forEach((n, k) => output[ci * j + k] = n / row.electors);
      });
      return { output, invalid };
    }
  };
}
