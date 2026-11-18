import { basis, mv, tr, zeros, logGamma, type Mat } from './drift-math';

export interface GroupedCounts {
  /** Numeric index 0 is reserved for non-valid; political indices start at 1. */
  groups: number[][]; counts: number[]; electors: number;
}
export function validateGroups(groups: number[][], j: number, includeNonValid = true) {
  if (!Number.isInteger(j) || j < 2 || !groups.length || groups.some(g => !g.length)) throw new Error('Empty grouped observation');
  const ids = groups.flat();
  if (new Set(ids).size !== ids.length || ids.some(i => !Number.isInteger(i) || i < (includeNonValid ? 0 : 1) || i >= j)) throw new Error('Overlapping/invalid component membership');
  if (includeNonValid && (!groups.some(g => g.length === 1 && g[0] === 0))) throw new Error('Non-valid requires its own outcome');
}
const logSumExp = (values: number[]) => {
  const m = Math.max(...values); return m + Math.log(values.reduce((s, x) => s + Math.exp(x - m), 0));
};
export function groupedProbabilities(scores: ArrayLike<number>, groups: number[][]) {
  const logs = groups.map(g => logSumExp(g.map(j => scores[j]))), z = logSumExp(logs);
  return logs.map(v => Math.exp(v - z));
}
/** Stable log likelihood after the observation has passed validation. */
export function groupedCountLogLikelihood(scores: ArrayLike<number>, observation: GroupedCounts) {
  const logs = observation.groups.map(g => logSumExp(g.map(j => scores[j]))), z = logSumExp(logs);
  return observation.counts.reduce((sum, y, g) => sum + y * (logs[g] - z), 0);
}
/** Exact original grouped multinomial. Its observed Hessian CAN be indefinite;
 * never substitute a complete-data/latent-allocation information matrix. */
export function groupedCountDerivatives(scores: ArrayLike<number>, observation: GroupedCounts, constants = false) {
  const j = scores.length, { groups, counts, electors } = observation;
  validateGroups(groups, j);
  if (counts.length !== groups.length || !Number.isSafeInteger(electors) || electors <= 0 ||
    counts.some(y => !Number.isSafeInteger(y) || y < 0) || counts.reduce((s, y) => s + y, 0) !== electors ||
    Array.from(scores).some(x => !Number.isFinite(x))) throw new Error('Invalid grouped counts/scores');
  const ids = groups.flat(), logZ = logSumExp(ids.map(i => scores[i]));
  const p = zeros(j, 1), gradient = zeros(j, 1), precision = zeros(j);
  for (const i of ids) p[i] = Math.exp(scores[i] - logZ);
  let logLikelihood = constants ? logGamma(electors + 1) - counts.reduce((s, y) => s + logGamma(y + 1), 0) : 0;
  for (let g = 0; g < groups.length; g++) {
    const members = groups[g], y = counts[g], logG = logSumExp(members.map(i => scores[i]));
    logLikelihood += y * (logG - logZ);
    const w = members.map(i => Math.exp(scores[i] - logG));
    for (let a = 0; a < members.length; a++) {
      const r = members[a]; gradient[r] += y * w[a];
      for (let b = 0; b < members.length; b++) precision[r * j + members[b]] -= y * ((a === b ? w[a] : 0) - w[a] * w[b]);
    }
  }
  for (let r = 0; r < j; r++) {
    gradient[r] -= electors * p[r];
    for (let c = 0; c < j; c++) precision[r * j + c] += electors * ((r === c ? p[r] : 0) - p[r] * p[c]);
  }
  return { logLikelihood, gradient, precision, probabilities: groupedProbabilities(scores, groups) };
}
export interface AggregateCell { scores: ArrayLike<number>; groups: number[][]; electors: number; }
/** Expected valid-vote weights are computed before any shared polling tilt. */
export function aggregatePoliticalShares(cells: AggregateCell[], bias?: ArrayLike<number>) {
  if (!cells.length) throw new Error('Empty geographic scope');
  const answerCount = cells[0].groups.length - 1, sums = Array(answerCount).fill(0);
  let total = 0;
  for (const c of cells) {
    validateGroups(c.groups, c.scores.length);
    if (c.groups[0].length !== 1 || c.groups[0][0] !== 0 || c.groups.length !== answerCount + 1 || !Number.isFinite(c.electors) || !(c.electors > 0)) throw new Error('Incompatible aggregate answer groups');
    const p = groupedProbabilities(c.scores, c.groups), valid = c.electors * (1 - p[0]);
    const scores = Array.from(c.scores, (v, j) => v + (bias?.[j] ?? 0));
    const conditional = groupedProbabilities(scores, c.groups.slice(1));
    conditional.forEach((v, a) => sums[a] += valid * v); total += valid;
  }
  if (!(total > 0)) throw new Error('No expected valid votes');
  return sums.map(v => v / total);
}
export function aggregateNonValid(cells: AggregateCell[]) {
  if (!cells.length || cells.some(c => !Number.isFinite(c.electors) || c.electors <= 0 || c.groups[0]?.length !== 1 || c.groups[0][0] !== 0)) throw new Error('Invalid non-valid aggregate scope');
  const n = cells.reduce((s, c) => s + c.electors, 0);
  if (!(n > 0)) throw new Error('No electors');
  return cells.reduce((s, c) => s + c.electors * groupedProbabilities(c.scores, c.groups)[0], 0) / n;
}
/** A root solve checks the aggregate operator. It is NOT posterior conditioning. */
export function referenceGroupShift(cells: AggregateCell[], answer: number, target: number) {
  if (!(target > 0 && target < 1) || !Number.isInteger(answer) || answer < 0 || answer >= cells[0].groups.length - 1) throw new Error('Infeasible interior reference share');
  const shifted = (shift: number) => cells.map(c => ({ ...c, scores: Array.from(c.scores, (v, j) => v + (c.groups[answer + 1].includes(j) ? shift : 0)) }));
  let lo = -50, hi = 50;
  if (aggregatePoliticalShares(shifted(lo))[answer] >= target || aggregatePoliticalShares(shifted(hi))[answer] <= target) throw new Error('Unsupported scoped reference constraint');
  for (let k = 0; k < 75; k++) { const mid = (lo + hi) / 2; if (aggregatePoliticalShares(shifted(mid))[answer] < target) lo = mid; else hi = mid; }
  const shift = (lo + hi) / 2;
  return { shift, cells: shifted(shift) };
}
/** Observed answer contrasts only; no invented respondent counts or epsilon
 * substitution for a printed zero. Zero/rounding interval factors are a later gate. */
export function pollContrastFactor(predicted: number[], printed: number[], effectiveDecided: number, roundingPp = .1) {
  if (predicted.length !== printed.length || printed.length < 2 || [...printed, ...predicted].some(p => !(p > 0 && p < 1)) ||
    !(effectiveDecided > 0) || !Number.isFinite(effectiveDecided) || roundingPp < 0 || !Number.isFinite(roundingPp) ||
    Math.abs(printed.reduce((s, p) => s + p, 0) - 1) > 1e-8 || Math.abs(predicted.reduce((s, p) => s + p, 0) - 1) > 1e-8)
    throw new Error('Invalid poll contrast or printed zero requires interval likelihood');
  const j = printed.length, h = basis(j), d = j - 1;
  const residual = mv(tr(h, j, d), printed.map((p, i) => Math.log(predicted[i] / p)), d);
  const covariance = zeros(d);
  for (let a = 0; a < d; a++) for (let b = 0; b < d; b++) for (let i = 0; i < j; i++)
    covariance[a * d + b] += h[i * d + a] * h[i * d + b] * (1 / (effectiveDecided * printed[i]) + (roundingPp / 100) ** 2 / (12 * printed[i] ** 2));
  return { residual, covariance };
}
