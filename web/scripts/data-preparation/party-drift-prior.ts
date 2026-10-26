import { createNormalSampler, createSeededRng } from '../../src/lib/core/rng';
import { basis, zeros, type Mat } from './drift-math';
import { groupedProbabilities, aggregateNonValid, type AggregateCell } from './party-drift-likelihood';
import { checkedDate } from './party-drift-contract';

export const PARTY_PRIOR_POLICY = {
  version: 'neutral-reference-conditional-v1', nationalSd: 1.5, localSd: .5, kindSd: .5,
  nationalInnovationSd: .35, localInnovationSd: .25,
  calibrationDraws: 32768, calibrationSeed: 'party-drift-prior-calibration-v1',
  // These are conditional research scales, not estimated party-level hyperparameters.
  scaleLogPriorSd: .7, effectLogPriorSd: .35
} as const;
export interface PriorContext {
  components: number; cells: { electors: number; available: number[]; kind?: 'camera' | 'european' }[];
  nationalSd: number; localSd: number; kindSd: number;
}
/** Calibrate E[aggregate p0], using common random numbers conditional on scales.
 * Exchangeable independent raw Gaussians are projected by gauge centering;
 * equivalent orthonormal contrast priors are used by the fitting reference. */
export function calibrateReferenceOffset(context: PriorContext, draws: number = PARTY_PRIOR_POLICY.calibrationDraws, seed: string = PARTY_PRIOR_POLICY.calibrationSeed) {
  const j = context.components + 1;
  if (j < 2 || !context.cells.length || !Number.isInteger(draws) || draws < 2 ||
    [context.nationalSd, context.localSd, context.kindSd].some(sd => !Number.isFinite(sd) || sd < 0) ||
    context.cells.some(c => !(c.electors > 0) || !c.available.length || c.available.some(k => !Number.isInteger(k) || k < 1 || k >= j) || new Set(c.available).size !== c.available.length)) throw new Error('Invalid prior calibration context');
  const normal = createNormalSampler(createSeededRng(seed)), total = context.cells.reduce((s, c) => s + c.electors, 0);
  // Store log(sum exp political)-score0, so scalar calibration is inexpensive.
  const gaps = new Float64Array(draws * context.cells.length);
  for (let n = 0; n < draws; n++) {
    const national = Array.from({ length: j }, () => normal(0, context.nationalSd));
    const kind = Array.from({ length: j }, () => normal(0, context.kindSd));
    for (let c = 0; c < context.cells.length; c++) {
      const cell = context.cells[c], s = national.map((v, i) => v + normal(0, context.localSd) + (cell.kind === 'european' ? kind[i] : 0));
      const max = Math.max(...cell.available.map(i => s[i]));
      gaps[n * context.cells.length + c] = max + Math.log(cell.available.reduce((sum, i) => sum + Math.exp(s[i] - max), 0)) - s[0];
    }
  }
  const expectation = (offset: number) => {
    let sum = 0;
    for (let n = 0; n < draws; n++) for (let c = 0; c < context.cells.length; c++)
      sum += context.cells[c].electors / total / draws / (1 + Math.exp(gaps[n * context.cells.length + c] - offset));
    return sum;
  };
  let lo = -50, hi = 50;
  for (let i = 0; i < 60; i++) { const m = (lo + hi) / 2; if (expectation(m) < .5) lo = m; else hi = m; }
  const offset = (lo + hi) / 2;
  return { offset, expectedNonValid: expectation(offset), draws, seed, context };
}
export interface AffineScores { offset: Mat; design: Mat; dimension: number; }
export function evaluateAffine(model: AffineScores, x: ArrayLike<number>) {
  if (x.length !== model.dimension) throw new Error('Latent dimension mismatch');
  const out = model.offset.slice();
  for (let r = 0; r < out.length; r++) for (let c = 0; c < x.length; c++) out[r] += model.design[r * x.length + c] * x[c];
  return out;
}
/** Fixed-scale reference-anchored Gaussian construction for reduced checks.
 * All coordinates have N(0,1) priors; normalized transitions are represented by
 * their generative innovations. No start-of-window prior or duplicated counts.
 * This builder is intentionally conditional on scales and uses a common basis. */
export function makeReferenceWalk(options: {
  j: number; cities: string[]; dates: string[]; referenceDate: string; offset: number;
  nationalSd: number; localSd: number; nationalInnovationSd: number; localInnovationSd: number;
  supportStarts?: string[];
}) {
  const { j, cities, referenceDate } = options, d = j - 1, h = basis(j), ref = checkedDate(referenceDate);
  if (j < 2 || !cities.length || new Set(cities).size !== cities.length ||
    [options.nationalSd, options.localSd, options.nationalInnovationSd, options.localInnovationSd].some(sd => !Number.isFinite(sd) || sd < 0)) throw new Error('Invalid reference walk');
  const starts = options.supportStarts?.map(checkedDate) ?? Array(j).fill(-Infinity);
  if (starts.length !== j || starts.some(t => t > ref)) throw new Error('Future birth needs explicit forward initialization');
  const times = [...new Set([...options.dates.map(checkedDate), ref, ...starts.filter(t => Number.isFinite(t) && t >= Math.min(...options.dates.map(checkedDate), ref))])].sort((a, b) => a - b);
  const refIndex = times.indexOf(ref), blocks: { side: 'backward' | 'forward'; left: number; right: number; active: number[]; offset: number; width: number }[] = [];
  let dimension = (1 + cities.length) * d;
  for (let t = 0; t < times.length - 1; t++) {
    const active = Array.from({ length: j }, (_, i) => i).filter(i => starts[i] <= times[t]);
    const width = active.length === j ? d : active.length;
    blocks.push({ side: t < refIndex ? 'backward' : 'forward', left: t, right: t + 1, active, offset: dimension, width });
    dimension += width * (1 + cities.length);
  }
  function state(city: string, date: string): AffineScores {
    const ci = cities.indexOf(city), time = checkedDate(date), ti = times.indexOf(time);
    if (ci < 0 || ti < 0) throw new Error('Unknown walk state');
    const design = zeros(j, dimension), offset = zeros(j, 1); offset[0] = options.offset;
    // Centering the deterministic offset leaves all observation probabilities unchanged.
    for (let r = 0; r < j; r++) offset[r] -= options.offset / j;
    for (let r = 0; r < j; r++) for (let k = 0; k < d; k++) {
      design[r * dimension + k] = options.nationalSd * h[r * d + k];
      design[r * dimension + (ci + 1) * d + k] = options.localSd * h[r * d + k];
    }
    for (const b of blocks) {
      const used = b.side === 'backward' ? ti <= b.left : ti >= b.right;
      if (!used) continue;
      const dt = times[b.right] - times[b.left];
      for (let r = 0; r < j; r++) for (let k = 0; k < b.width; k++) {
        const coefficient = b.active.length === j ? h[r * d + k] : (r === b.active[k] ? 1 : 0) - 1 / j;
        design[r * dimension + b.offset + k] = coefficient * options.nationalInnovationSd * Math.sqrt(dt);
        design[r * dimension + b.offset + (ci + 1) * b.width + k] = coefficient * options.localInnovationSd * Math.sqrt(dt);
      }
    }
    return { offset, design, dimension };
  }
  return { dimension, dates: times, blocks, state };
}
export function priorProbabilityDraw(context: PriorContext, offset: number, seed: string, draws: number) {
  const normal = createNormalSampler(createSeededRng(seed)), totals = Array(context.components + 1).fill(0);
  let nonValid = 0;
  for (let n = 0; n < draws; n++) {
    const national = Array.from(totals, () => normal(0, context.nationalSd)), kind = Array.from(totals, () => normal(0, context.kindSd));
    const cells: AggregateCell[] = context.cells.map(c => ({ electors: c.electors, groups: [[0], ...c.available.map(i => [i])],
      scores: national.map((v, j) => v + normal(0, context.localSd) + (c.kind === 'european' ? kind[j] : 0) + (j === 0 ? offset : 0)) }));
    nonValid += aggregateNonValid(cells) / draws;
    const total = context.cells.reduce((s, c) => s + c.electors, 0);
    for (const c of cells) {
      const p = groupedProbabilities(c.scores, c.groups);
      c.groups.forEach((g, k) => totals[g[0]] += c.electors / total * p[k] / draws);
    }
  }
  return { expectedNonValid: nonValid, electorShares: totals };
}
