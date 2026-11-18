import { zeros, add, mv, mm, tr, chol, solve, inverse, logdet, innovationCov, dot, type Mat } from './drift-math';
import { precisionNoise } from './drift-gaussian';
import { globalPrior, DRIFT_POLICY, type DriftProblem, type DriftParameters, type CollapsedCounts } from './drift-structure';
import type { PollGeometry } from './drift-polls';
import { createNormalSampler, createSeededRng } from '../../src/lib/core/rng';

export interface GlobalEvaluation {
  score: number; factor: Mat; mean: Mat; coreMean: Mat; parameters: DriftParameters;
}
function corePrior(problem: DriftProblem, params: DriftParameters) {
  const { layout, d } = problem, n = layout.core, a = zeros(n), b = zeros(n, 1);
  let determinant = 0, quadratic = 0;
  const initialVariance = DRIFT_POLICY.nationalInitialSd ** 2;
  for (let j = 0; j < d; j++) {
    a[j * n + j] = 1 / initialVariance; b[j] = problem.nationalMean[j] / initialVariance; quadratic += problem.nationalMean[j] ** 2 / initialVariance;
  }
  determinant -= d * Math.log(initialVariance);
  const q = innovationCov(params.etaShape.map(v => v * params.eta), problem.h), ql = chol(q, d), qi = inverse(ql, d);
  for (let t = 1; t < layout.times.length; t++) {
    const dt = layout.times[t] - layout.times[t - 1], left = (t - 1) * d, right = t * d;
    for (let j = 0; j < d; j++) for (let k = 0; k < d; k++) {
      const v = qi[j * d + k] / dt;
      a[(left + j) * n + left + k] += v; a[(right + j) * n + right + k] += v;
      a[(left + j) * n + right + k] -= v; a[(right + j) * n + left + k] -= v;
    }
    determinant -= logdet(ql, d) + d * Math.log(dt);
  }
  for (let i = layout.nationalLength; i < n; i++) a[i * n + i] += 1 / params.poll ** 2;
  determinant -= (n - layout.nationalLength) * Math.log(params.poll ** 2);
  return { a, b, determinant, quadratic };
}
/** Eliminate fixed-variance kind/region states once. Hyperparameter sampling
 * then factors only the small national/poll-bias state precision. */
export class GlobalSystem {
  readonly dataPrecision: Mat; readonly dataLinear: Mat; readonly constant: number;
  readonly nuisanceFactor: Mat; readonly nuisanceLinear: Mat; readonly nuisanceRegression: Mat;
  readonly nuisance: number;
  readonly pollInverse: Mat;
  constructor(readonly problem: DriftProblem, readonly params: DriftParameters, counts: CollapsedCounts,
    readonly polls: PollGeometry, countCorrection: number) {
    const { m, core } = problem.layout, p = polls.p, c = counts.precision.slice(), b = counts.linear.slice();
    this.pollInverse = p ? inverse(chol(polls.covariance, p), p) : zeros(0);
    const rij = mm(this.pollInverse, polls.jacobian, p, p, m), riy = mv(this.pollInverse, polls.response, p), jt = tr(polls.jacobian, p, m);
    const extra = mm(jt, rij, m, p, m), extraB = mv(jt, riy, m);
    for (let i = 0; i < c.length; i++) c[i] += extra[i];
    for (let i = 0; i < m; i++) b[i] += extraB[i];
    let constant = counts.constant + countCorrection - .5 * (p * Math.log(2 * Math.PI) + (p ? logdet(chol(polls.covariance, p), p) : 0) + dot(polls.response, riy));
    this.nuisance = m - core;
    this.dataPrecision = zeros(core); this.dataLinear = b.slice(0, core);
    for (let i = 0; i < core; i++) for (let j = 0; j < core; j++) this.dataPrecision[i * core + j] = c[i * m + j];
    this.nuisanceFactor = zeros(0); this.nuisanceLinear = zeros(0, 1); this.nuisanceRegression = zeros(0, core);
    if (this.nuisance) {
      const full = globalPrior(problem, params), cp = corePrior(problem, params), nn = this.nuisance;
      const arr = zeros(nn), arc = zeros(nn, core), br = b.slice(core);
      for (let i = 0; i < nn; i++) {
        br[i] += full.linear[core + i];
        for (let j = 0; j < nn; j++) arr[i * nn + j] = c[(core + i) * m + core + j] + full.precision[(core + i) * m + core + j];
        for (let j = 0; j < core; j++) arc[i * core + j] = c[(core + i) * m + j];
      }
      const l = chol(arr, nn), mean = solve(l, br, nn), regression = zeros(nn, core);
      for (let j = 0; j < core; j++) {
        const rhs = Float64Array.from({ length: nn }, (_, i) => arc[i * core + j]), sol = solve(l, rhs, nn);
        for (let i = 0; i < nn; i++) regression[i * core + j] = sol[i];
      }
      for (let i = 0; i < core; i++) {
        for (let k = 0; k < nn; k++) this.dataLinear[i] -= arc[k * core + i] * mean[k];
        for (let j = 0; j < core; j++) for (let k = 0; k < nn; k++) this.dataPrecision[i * core + j] -= arc[k * core + i] * regression[k * core + j];
      }
      constant += .5 * (full.logPrecisionDet - cp.determinant - logdet(l, nn) + dot(br, mean));
      this.nuisanceFactor = l; this.nuisanceLinear = mean; this.nuisanceRegression = regression;
    }
    this.constant = constant;
  }
  evaluate(params: DriftParameters): GlobalEvaluation {
    const prior = corePrior(this.problem, params), a = add(this.dataPrecision, prior.a), b = add(this.dataLinear, prior.b);
    const n = this.problem.layout.core, l = chol(a, n), coreMean = solve(l, b, n);
    const mean = zeros(this.problem.layout.m, 1); mean.set(coreMean);
    if (this.nuisance) mean.set(add(this.nuisanceLinear, mv(this.nuisanceRegression, coreMean, this.nuisance), -1), n);
    const shapePenalty = params.etaShape.reduce((s, v) => s + Math.log(v) ** 2 / DRIFT_POLICY.outcomeEffectPriorSd ** 2, 0);
    const score = this.constant + .5 * (prior.determinant - logdet(l, n) + dot(b, coreMean) - prior.quadratic)
      - .5 * ((Math.log(params.eta / DRIFT_POLICY.scaleEtaPrior) / DRIFT_POLICY.logScalePriorSd) ** 2 +
        (Math.log(params.poll / DRIFT_POLICY.scalePollPrior) / DRIFT_POLICY.logPollPriorSd) ** 2 + shapePenalty);
    return { score, factor: l, mean, coreMean, parameters: params };
  }
  draw(evaluation: GlobalEvaluation, normal: () => number) {
    const { core, m } = this.problem.layout, out = zeros(m, 1);
    const coreDraw = add(evaluation.coreMean, precisionNoise(evaluation.factor, normal, core)); out.set(coreDraw);
    if (this.nuisance) {
      const conditional = add(this.nuisanceLinear, mv(this.nuisanceRegression, coreDraw, this.nuisance), -1);
      out.set(add(conditional, precisionNoise(this.nuisanceFactor, normal, this.nuisance)), core);
    }
    return out;
  }
}
function clone(p: DriftParameters) { return { ...p, etaShape: [...p.etaShape], deltaShape: [...p.deltaShape] }; }
function admissible(p: DriftParameters) { return p.eta >= .02 && p.eta <= 3 && p.poll >= .015 && p.poll <= .7 && p.etaShape.every(v => v > .1 && v < 10); }
function change(p: DriftParameters, direction: number, step: number, outcomes: number) {
  const out = clone(p);
  if (direction === 0) out.eta *= Math.exp(step);
  else if (direction === 1) out.poll *= Math.exp(step);
  else {
    const j = direction - 2;
    out.etaShape[j] *= Math.exp(step); out.etaShape[outcomes - 1] *= Math.exp(-step);
  }
  return out;
}
export function optimizeGlobal(system: GlobalSystem, start: DriftParameters) {
  let current = system.evaluate(start), evaluations = 1;
  const dimensions = 2 + (system.problem.variant.national === 'outcome' ? system.problem.j - 1 : 0);
  for (const radius of [.5, .25, .12]) for (let sweep = 0; sweep < 2; sweep++) {
    let improved = false;
    for (let direction = 0; direction < dimensions; direction++) {
      let best = current;
      for (const sign of [-1, 1]) {
        const proposal = change(current.parameters, direction, sign * radius, system.problem.j); if (!admissible(proposal)) continue;
        const candidate = system.evaluate(proposal); evaluations++;
        if (candidate.score > best.score + 1e-8) best = candidate;
      }
      improved ||= best !== current; current = best;
    }
    if (!improved) break;
  }
  return { evaluation: current, evaluations };
}
export interface HyperDiagnostics { acceptance: number; splitRhat: number[]; ess: number[]; retained: number; chainLength: number; }
function chainDiagnostics(chains: number[][][]) {
  const dim = chains[0][0].length, n = chains[0].length, split = chains.flatMap(c => [c.slice(0, Math.floor(n / 2)), c.slice(Math.floor(n / 2))]);
  const k = split[0].length, means = split.map(c => Array.from({ length: dim }, (_, j) => c.reduce((s, x) => s + x[j] / k, 0)));
  const vars = split.map((c, h) => means[h].map((mu, j) => c.reduce((s, x) => s + (x[j] - mu) ** 2 / (k - 1), 0)));
  const rhat = Array.from({ length: dim }, (_, j) => {
    const mu = means.reduce((s, a) => s + a[j] / split.length, 0), w = vars.reduce((s, a) => s + a[j] / split.length, 0);
    const b = k * means.reduce((s, a) => s + (a[j] - mu) ** 2 / (split.length - 1), 0);
    return Math.sqrt(((k - 1) / k * w + b / k) / Math.max(1e-20, w));
  });
  const ess = Array.from({ length: dim }, (_, j) => {
    let sum = 0;
    for (let lag = 1; lag < Math.min(40, k / 2); lag += 2) {
      let pair = 0;
      for (const l of [lag, lag + 1]) {
        let covariance = 0, variance = 0;
        split.forEach((c, h) => {
          variance += vars[h][j];
          for (let t = 0; t < k - l; t++) covariance += (c[t][j] - means[h][j]) * (c[t + l][j] - means[h][j]) / (k - l);
        });
        pair += covariance / Math.max(1e-20, variance);
      }
      if (pair < 0) break; sum += pair;
    }
    return Math.min(2 * n, 2 * n / Math.max(1, 1 + 2 * sum));
  });
  return { rhat, ess };
}
export function sampleGlobal(system: GlobalSystem, mode: GlobalEvaluation, draws: number, seed: string, minimumRetained = 128) {
  const retained: { global: Mat; eta: number; etaShape: number[] }[][] = [], chains: number[][][] = [];
  let accepted = 0, attempts = 0;
  const dimensions = 2 + (system.problem.variant.national === 'outcome' ? system.problem.j - 1 : 0);
  const perChain = Math.max(minimumRetained, Math.ceil(draws / 2)), burn = 256, thin = Math.max(4, dimensions);
  for (let chain = 0; chain < 2; chain++) {
    const rng = createSeededRng(seed + ':hyper:' + chain), normal = createNormalSampler(rng); let current = mode;
    const trace: number[][] = [], chainDraws: (typeof retained)[number] = [];
    for (let step = 0; step < burn + perChain * thin; step++) {
      const direction = Math.floor(rng() * dimensions), proposal = change(current.parameters, direction, normal(0, direction < 2 ? .32 : .2), system.problem.j);
      attempts++;
      if (admissible(proposal)) {
        const candidate = system.evaluate(proposal);
        if (Math.log(rng()) < candidate.score - current.score) { current = candidate; accepted++; }
      }
      if (step >= burn && (step - burn) % thin === 0) {
        trace.push([Math.log(current.parameters.eta), Math.log(current.parameters.poll), ...(dimensions > 2 ? current.parameters.etaShape.slice(0, -1).map(Math.log) : [])]);
        chainDraws.push({ global: system.draw(current, () => normal(0, 1)), eta: current.parameters.eta, etaShape: [...current.parameters.etaShape] });
      }
    }
    chains.push(trace); retained.push(chainDraws);
  }
  const diagnostic = chainDiagnostics(chains);
  if ((Math.max(...diagnostic.rhat) > 1.1 || Math.min(...diagnostic.ess) < 30) && perChain < 1024)
    return sampleGlobal(system, mode, draws, seed, perChain * 2);
  const selected = retained.flatMap((chain, index) => {
    const want = index === 0 ? Math.ceil(draws / 2) : Math.floor(draws / 2);
    return Array.from({ length: want }, (_, i) => chain[Math.floor(i * (perChain - 1) / Math.max(1, want - 1))]);
  });
  return { draws: selected, diagnostics: { acceptance: accepted / attempts, splitRhat: diagnostic.rhat,
    ess: diagnostic.ess, retained: perChain * 2, chainLength: perChain } satisfies HyperDiagnostics };
}
