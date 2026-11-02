/** Reference-information factors and exact scalar-constraint elimination for
 * reduced research checks. All shares use valid voters unless explicitly 0. */
import { dot, zeros, type Mat } from './drift-math';
import { reducedLogLikelihood, type ReducedPartyProblem } from './party-drift-fit';
import { aggregatePoliticalShares, aggregateNonValid, type AggregateCell } from './party-drift-likelihood';
import { evaluateAffine, type AffineScores } from './party-drift-prior';

const sigmoid = (x: number) => 1 / (1 + Math.exp(-x));
export function logisticNormalMoments(location: number, scale: number) {
  let mean = 0, second = 0, mass = 0;
  // Independent deterministic normal integration, including the normalized
  // density rather than equating mean shares with logistic(mean logits).
  for (let i = 0; i <= 400; i++) {
    const z = -10 + i / 20, w = Math.exp(-z * z / 2) * (i === 0 || i === 400 ? .5 : 1), p = sigmoid(location + scale * z);
    mean += w * p; second += w * p * p; mass += w;
  }
  mean /= mass; second /= mass;
  return { mean, variance: Math.max(0, second - mean * mean) };
}
export function calibrateSoftReference(mean: number, sdPercentagePoints: number) {
  const sd = sdPercentagePoints / 100;
  if (!(mean > 0 && mean < 1) || !(sd > 0) || sd * sd >= mean * (1 - mean)) throw new Error('Infeasible logistic-normal reference moments');
  const locationAt = (scale: number) => {
    let lo = -100, hi = 100;
    for (let i = 0; i < 65; i++) { const m = (lo + hi) / 2; if (logisticNormalMoments(m, scale).mean < mean) lo = m; else hi = m; }
    return (lo + hi) / 2;
  };
  let lo = 1e-6, hi = 1;
  while (logisticNormalMoments(locationAt(hi), hi).variance < sd * sd && hi < 64) hi *= 2;
  if (hi >= 64) throw new Error('Reference moments outside reliable quadrature range');
  for (let i = 0; i < 55; i++) { const s = (lo + hi) / 2; if (logisticNormalMoments(locationAt(s), s).variance < sd * sd) lo = s; else hi = s; }
  const scale = (lo + hi) / 2, location = locationAt(scale), moments = logisticNormalMoments(location, scale);
  return { enteredMean: mean, enteredSdPercentagePoints: sdPercentagePoints, location, scale, moments };
}
export function softReferenceLogDensity(share: number, factor: ReturnType<typeof calibrateSoftReference>) {
  if (!(share > 0 && share < 1)) return -Infinity;
  const logit = Math.log(share) - Math.log1p(-share);
  // A bounded logistic-normal density on share space; the Jacobian is needed.
  return -.5 * ((logit - factor.location) / factor.scale) ** 2 - Math.log(factor.scale) - Math.log(share) - Math.log1p(-share);
}
export interface ReferenceCell { state: AffineScores; groups: number[][]; electors: number; }
export function referenceValue(cells: ReferenceCell[], x: Mat, answer: number) {
  const scope: AggregateCell[] = cells.map(c => ({ ...c, scores: evaluateAffine(c.state, x) }));
  return answer === 0 ? aggregateNonValid(scope) : aggregatePoliticalShares(scope)[answer - 1];
}
/** Eliminate one exact aggregate-share constraint from a standard-Gaussian
 * base. For x=v*a+T*z, integrating delta(F(x)-target) gives the factor
 * exp(-a(z)^2/2)/|dF/da|. Retain it at EVERY density evaluation; root solving
 * only after fitting or dropping the derivative changes the posterior. */
export function eliminateHardReference(original: ReducedPartyProblem, cells: ReferenceCell[], answer: number, target: number, direction: Mat) {
  const n = original.dimension;
  if (!(target > 0 && target < 1) || n < 2 || direction.length !== n) throw new Error('Invalid hard reference elimination');
  const norm = Math.sqrt(dot(direction, direction));
  if (!(norm > 0)) throw new Error('Hard reference has no independent direction');
  const v = direction.map(x => x / norm), u = v.slice(); u[0] += v[0] >= 0 ? 1 : -1;
  const un = Math.sqrt(dot(u, u)); for (let i = 0; i < n; i++) u[i] /= un;
  // Householder columns 1..n-1 form an orthonormal complement of v.
  const complement = zeros(n, n - 1);
  for (let r = 0; r < n; r++) for (let c = 1; c < n; c++) complement[r * (n - 1) + c - 1] = (r === c ? 1 : 0) - 2 * u[r] * u[c];
  function decode(z: Mat) {
    const base = zeros(n, 1);
    for (let r = 0; r < n; r++) for (let c = 0; c < n - 1; c++) base[r] += complement[r * (n - 1) + c] * z[c];
    const at = (a: number) => base.map((b, i) => b + a * v[i]);
    const f = (a: number) => referenceValue(cells, at(a), answer);
    let lo = -16, hi = 16;
    while (f(lo) > target && lo > -1024) lo *= 2;
    while (f(hi) < target && hi < 1024) hi *= 2;
    if (f(lo) >= target || f(hi) <= target) throw new Error('Hard reference is infeasible in the declared direction');
    for (let k = 0; k < 45; k++) { const mid = (lo + hi) / 2; if (f(mid) < target) lo = mid; else hi = mid; }
    const a = (lo + hi) / 2, x = at(a), derivative = (f(a + 1e-4) - f(a - 1e-4)) / 2e-4;
    if (!(derivative > 1e-14)) throw new Error('Singular exact reference constraint');
    return { x, a, derivative, share: referenceValue(cells, x, answer) };
  }
  const problem: ReducedPartyProblem = { dimension: n - 1, factors: [], extraLogLikelihood(z) {
    const d = decode(z);
    return reducedLogLikelihood(original, d.x) - .5 * d.a ** 2 - Math.log(d.derivative);
  } };
  return { problem, decode, direction: v, complement };
}
