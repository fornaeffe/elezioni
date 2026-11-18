/** Exact-density importance correction of reduced Laplace proposals. This is
 * a separate candidate, not an assertion that full-field importance sampling
 * remains accurate in arbitrarily high dimension. */
import { createNormalSampler, createSeededRng } from '../../src/lib/core/rng';
import { precisionNoise } from './drift-gaussian';
import { dot, logGamma, logdet, mv, type Mat } from './drift-math';
import { reducedDerivatives, reducedLogLikelihood, type ReducedPartyProblem, type fitReducedParty } from './party-drift-fit';

export function reducedImportance(problem: ReducedPartyProblem, fit: ReturnType<typeof fitReducedParty>, draws: number, seed: string) {
  if (!fit.factor || !fit.converged) throw new Error('Importance proposal needs a converged positive-definite mode');
  const start = performance.now(), rng = createSeededRng(seed), normal = createNormalSampler(rng), n = problem.dimension, df = 5;
  const precision = reducedDerivatives(problem, fit.mode).precision, logDet = logdet(fit.factor, n);
  const mixture = [{ weight: .7, inflation: 1, prior: false }, { weight: .25, inflation: 4, prior: false }, { weight: .05, inflation: 1, prior: true }];
  const constant = logGamma((df + n) / 2) - logGamma(df / 2) - n / 2 * Math.log(df * Math.PI);
  const proposals: Mat[] = [], logWeights: number[] = [];
  for (let draw = 0; draw < draws; draw++) {
    const select = rng(), component = select < .7 ? mixture[0] : select < .95 ? mixture[1] : mixture[2];
    let chi = 0; for (let k = 0; k < df; k++) chi += normal(0, 1) ** 2;
    const noise = component.prior ? Float64Array.from({ length: n }, () => normal(0, 1)) : precisionNoise(fit.factor, () => normal(0, 1), n);
    const x = noise.map((v, i) => (component.prior ? 0 : fit.mode[i]) + v * Math.sqrt(df / chi * component.inflation));
    const diff = x.map((v, i) => v - fit.mode[i]), mahalanobis = dot(diff, mv(precision, diff, n)), priorNorm = dot(x, x);
    const logs = mixture.map(c => Math.log(c.weight) + constant + (c.prior ? 0 : .5 * logDet) - n / 2 * Math.log(c.inflation) - (df + n) / 2 * Math.log1p((c.prior ? priorNorm : mahalanobis) / c.inflation / df));
    const max = Math.max(...logs), logQ = max + Math.log(logs.reduce((s, x) => s + Math.exp(x - max), 0));
    const logP = reducedLogLikelihood(problem, x) - .5 * priorNorm;
    proposals.push(x); logWeights.push(logP - logQ);
  }
  const max = Math.max(...logWeights), weights = logWeights.map(x => Math.exp(x - max)), total = weights.reduce((s, x) => s + x, 0);
  for (let i = 0; i < weights.length; i++) weights[i] /= total;
  const ess = 1 / weights.reduce((s, x) => s + x * x, 0), output: Mat[] = [];
  // Systematic resampling only converts weighted draws for common reporting;
  // report the ORIGINAL importance ESS and largest weight, never resample ESS.
  let cursor = 0, cumulative = weights[0]; const offset = rng();
  for (let k = 0; k < draws; k++) { const target = (k + offset) / draws; while (cumulative < target && cursor < draws - 1) cumulative += weights[++cursor]; output.push(proposals[cursor]); }
  return { draws: output, ess, maxWeight: Math.max(...weights), evaluations: draws, seconds: (performance.now() - start) / 1000 };
}
