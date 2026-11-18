/** Exact grouped-likelihood, fixed-scale reduced Laplace reference. Dense linear
 * algebra here is deliberately bounded: this is an accuracy gate, not a full-
 * geography solver or an empirical-Bayes volatility estimator. */
import { add, chol, dot, inverse, mv, solve, zeros, type Mat } from './drift-math';
import { precisionNoise } from './drift-gaussian';
import { groupedCountDerivatives, groupedCountLogLikelihood, type GroupedCounts } from './party-drift-likelihood';
import { evaluateAffine, type AffineScores } from './party-drift-prior';
import { createNormalSampler, createSeededRng } from '../../src/lib/core/rng';

export interface GroupedFactor { state: AffineScores; observation: GroupedCounts; }
export interface ReducedPartyProblem {
  dimension: number; factors: GroupedFactor[];
  /** Additional exact likelihood/constraint-elimination density with respect
   * to the standard Gaussian base measure, never a post-fit adjustment. */
  extraLogLikelihood?: (x: Mat) => number;
}
export const REDUCED_FIT_POLICY = {
  version: 'exact-grouped-laplace-reference-v1', maxDimension: 192,
  maxIterations: 120, gradientTolerance: 1e-6, maxRidge: 1e8,
  posteriorMeanTolerance: .025, posteriorWidthRatio: [.65, 1.5],
  maxRhat: 1.05, minEss: 400
} as const;
export function reducedLogLikelihood(problem: ReducedPartyProblem, x: Mat) {
  let out = 0;
  for (const f of problem.factors) {
    out += groupedCountLogLikelihood(evaluateAffine(f.state, x), f.observation);
  }
  return out + (problem.extraLogLikelihood?.(x) ?? 0);
}
export function reducedDerivatives(problem: ReducedPartyProblem, x: Mat) {
  const n = problem.dimension, gradient = x.map(v => -v), precision = zeros(n);
  let logPosterior = -.5 * dot(x, x);
  for (let k = 0; k < n; k++) precision[k * n + k] = 1;
  for (const f of problem.factors) {
    const scores = evaluateAffine(f.state, x), der = groupedCountDerivatives(scores, f.observation), j = scores.length, a = f.state.design;
    logPosterior += der.logLikelihood;
    const ha = zeros(j, n);
    for (let r = 0; r < j; r++) for (let s = 0; s < j; s++) for (let c = 0; c < n; c++) ha[r * n + c] += der.precision[r * j + s] * a[s * n + c];
    for (let r = 0; r < n; r++) {
      for (let k = 0; k < j; k++) gradient[r] += a[k * n + r] * der.gradient[k];
      for (let c = 0; c < n; c++) for (let k = 0; k < j; k++) precision[r * n + c] += a[k * n + r] * ha[k * n + c];
    }
  }
  if (problem.extraLogLikelihood) {
    const extra = problem.extraLogLikelihood, f = extra(x), step = 1e-4;
    logPosterior += f;
    for (let r = 0; r < n; r++) {
      const a = x.slice(), b = x.slice(); a[r] += step; b[r] -= step;
      const fa = extra(a), fb = extra(b);
      gradient[r] += (fa - fb) / (2 * step);
      precision[r * n + r] -= (fa + fb - 2 * f) / step ** 2;
      for (let c = 0; c < r; c++) {
        let second = 0;
        for (const sr of [-1, 1]) for (const sc of [-1, 1]) {
          const trial = x.slice(); trial[r] += sr * step; trial[c] += sc * step;
          second += sr * sc * extra(trial);
        }
        precision[r * n + c] -= second / (4 * step ** 2);
        precision[c * n + r] -= second / (4 * step ** 2);
      }
    }
  }
  return { gradient, precision, logPosterior };
}
export function fitReducedParty(problem: ReducedPartyProblem, initial?: Mat) {
  const start = performance.now(), n = problem.dimension;
  if (!Number.isInteger(n) || n < 1 || n > REDUCED_FIT_POLICY.maxDimension || !problem.factors.length && !problem.extraLogLikelihood || problem.factors.some(f => f.state.dimension !== n)) throw new Error('Reduced Laplace gate cannot fit this problem');
  let mode = initial?.slice() ?? zeros(n, 1), lastChange = Infinity;
  if (mode.length !== n) throw new Error('Initial dimension mismatch');
  const iterations: { gradient: number; change: number; ridge: number; rate: number; logPosterior: number }[] = [];
  let converged = false;
  for (let iteration = 0; iteration < REDUCED_FIT_POLICY.maxIterations; iteration++) {
    const der = reducedDerivatives(problem, mode), norm = Math.max(...Array.from(der.gradient, Math.abs));
    if (norm < REDUCED_FIT_POLICY.gradientTolerance) { converged = true; break; }
    let ridge = 0, factor: Mat | undefined;
    for (;;) {
      const a = der.precision.slice(); for (let k = 0; k < n; k++) a[k * n + k] += ridge;
      try { factor = chol(a, n); break; } catch { ridge = ridge ? ridge * 10 : 1e-6; }
      if (ridge > REDUCED_FIT_POLICY.maxRidge) throw new Error('Reduced posterior has no usable Newton direction');
    }
    const direction = solve(factor, der.gradient, n);
    let rate = 1, trial = add(mode, direction), next = reducedLogLikelihood(problem, trial) - .5 * dot(trial, trial);
    const ascent = dot(direction, der.gradient);
    while (next < der.logPosterior + 1e-4 * rate * ascent - 1e-12 && rate > 1e-10) {
      rate /= 2; trial = add(mode, direction, rate); next = reducedLogLikelihood(problem, trial) - .5 * dot(trial, trial);
    }
    lastChange = Math.max(...Array.from(trial, (v, i) => Math.abs(v - mode[i])));
    iterations.push({ gradient: norm, change: lastChange, ridge, rate, logPosterior: next });
    mode = trial;
    if (rate <= 1e-10) break;
  }
  const final = reducedDerivatives(problem, mode);
  converged ||= Math.max(...Array.from(final.gradient, Math.abs)) < REDUCED_FIT_POLICY.gradientTolerance;
  // A ridge can help find a mode, but cannot repair the reported posterior.
  let factor: Mat | undefined;
  try { factor = chol(final.precision, n); } catch { /* symmetric saddles must remain visible */ }
  return { mode, factor, covariance: factor ? inverse(factor, n) : undefined, converged,
    gradient: Math.max(...Array.from(final.gradient, Math.abs)), logPosterior: final.logPosterior,
    positiveDefinite: !!factor, iterations, lastChange, seconds: (performance.now() - start) / 1000 };
}
export function laplacePartyDraws(problem: ReducedPartyProblem, fit: ReturnType<typeof fitReducedParty>, count: number, seed: string) {
  if (!fit.converged || !fit.factor) throw new Error('Cannot sample an unconverged/saddle Laplace fit');
  const normal = createNormalSampler(createSeededRng(seed));
  return Array.from({ length: count }, () => add(fit.mode, precisionNoise(fit.factor!, () => normal(0, 1), problem.dimension)));
}
/** Exact-likelihood elliptical slice sampler (Murray, Adams & MacKay 2010).
 * Whole-vector updates retain shared national split uncertainty. This reference
 * sampler is not proposed as a scalable countrywide fitting algorithm. */
export function sampleReducedParty(problem: ReducedPartyProblem, initial: Mat, options: { seed: string; burn: number; draws: number; thin: number }) {
  const start = performance.now(), rng = createSeededRng(options.seed), normal = createNormalSampler(rng), n = problem.dimension;
  const draws: Mat[] = []; let x = initial.slice(), evaluations = 1, ll = reducedLogLikelihood(problem, x);
  for (let iteration = 0; iteration < options.burn + options.draws * options.thin; iteration++) {
    const nu = Float64Array.from({ length: n }, () => normal(0, 1)), threshold = ll + Math.log(Math.max(Number.MIN_VALUE, rng()));
    let angle = rng() * 2 * Math.PI, low = angle - 2 * Math.PI, high = angle;
    for (let attempt = 0;; attempt++) {
      const proposal = Float64Array.from(x, (v, k) => v * Math.cos(angle) + nu[k] * Math.sin(angle)), value = reducedLogLikelihood(problem, proposal); evaluations++;
      if (value >= threshold) { x = proposal; ll = value; break; }
      if (attempt >= 10000) throw new Error('Elliptical slice bracket failed');
      if (angle < 0) low = angle; else high = angle;
      angle = low + rng() * (high - low);
    }
    if (iteration >= options.burn && (iteration - options.burn) % options.thin === 0) draws.push(x.slice());
  }
  return { draws, evaluations, seconds: (performance.now() - start) / 1000 };
}
/** Split-chain R-hat and Geyer initial-positive, monotone paired ESS. These
 * moment diagnostics are reported for both latent split and probability outputs;
 * they are not evidence of posterior accuracy without the comparison itself. */
export function reducedChainDiagnostics(chains: number[][]) {
  if (chains.length < 2 || chains.some(c => c.length !== chains[0].length || c.length < 8)) throw new Error('Need equally sized reference chains');
  const half = Math.floor(chains[0].length / 2), split = chains.flatMap(c => [c.slice(0, half), c.slice(c.length - half)]), m = split.length, n = half;
  const means = split.map(c => c.reduce((s, x) => s + x, 0) / n), grand = means.reduce((s, x) => s + x, 0) / m;
  const variances = split.map((c, i) => c.reduce((s, x) => s + (x - means[i]) ** 2, 0) / (n - 1));
  const w = variances.reduce((s, x) => s + x, 0) / m, b = n / (m - 1) * means.reduce((s, x) => s + (x - grand) ** 2, 0), v = (n - 1) / n * w + b / n;
  if (v < 1e-25) return { rhat: 1, ess: n * m };
  const rho = (lag: number) => {
    let cov = 0;
    split.forEach((c, i) => { for (let k = 0; k < n - lag; k++) cov += (c[k] - means[i]) * (c[k + lag] - means[i]) / n / m; });
    return 1 - (w - cov) / v;
  };
  let sum = 0, previous = Infinity;
  for (let t = 0; t + 1 < Math.min(n, 2000); t += 2) {
    const pair = Math.min(previous, (t === 0 ? 1 : rho(t)) + rho(t + 1));
    if (pair < 0) break;
    sum += pair; previous = pair;
  }
  return { rhat: Math.max(1, Math.sqrt(v / w)), ess: Math.min(n * m, n * m / Math.max(1, -1 + 2 * sum)) };
}
