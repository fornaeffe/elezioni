import { createNormalSampler, createSeededRng } from '../../src/lib/core/rng';
import { ilr, inverseIlr } from './poll-uncertainty';
import { logistic, logit } from '../../src/lib/politics/reform-2026/generation-math';

/** Experimental composition layer. No imports from runtime registration. */
export const GENERATION_STUDY_POLICY = {
  version: 'municipal-generation-study-v1', horizons: [730, 365, 90, 15], draws: 256,
  categories: ['pd', 'm5s', 'lega', 'other'], priorElectionWeight: 1,
  nationalNearSigma: .2, nationalAnnualSigma: .35, turnoutPriorSigma: .2,
  regionalAnnualSigma: .12, municipalAnnualSigma: .18,
  emergingPartyMass: .005, profileHalfLifeYears: 8,
  tailDiagnostics: { nationalConcentration: .8, municipalConcentration: .95, minimumValidVotes: 500 },
  intervalLevels: [.5, .8, .9, .95]
} as const;
export type GenerationModel = 'joint-gaussian' | 'joint-student-t' | 'transition-bootstrap';
export interface ResearchMunicipality {
  id: string; regionId: string; electors: number; validVotes: number; votes: number[];
}
export interface ResearchEvent {
  id: string; kind: 'camera' | 'european'; date: string; municipalities: ResearchMunicipality[];
  national: { electors: number; validVotes: number; votes: number[] };
}
export interface ResearchPoll {
  id: string; publicationDate: string; fieldworkEnd: string; kind: string;
  shares: number[]; sampleSize: number; sourceUrl: string;
  targetElectionId?: string;
}
export interface ForecastInput {
  targetId: string; kind: ResearchEvent['kind']; date: string; cutoff: string; horizon: number;
  history: ResearchEvent[]; baseline: ResearchEvent; polls: ResearchPoll[];
  point: number[]; pointDate: string; pollIds: string[]; validPoint: number;
}
const sum = (xs: readonly number[]) => xs.reduce((a, b) => a + b, 0);
export const yearsBetween = (a: string, b: string) => (Date.parse(a) - Date.parse(b)) / (365.25 * 86400000);
export const cutoffFor = (date: string, days: number) => new Date(Date.parse(date) - days * 86400000).toISOString().slice(0, 10);
export function positiveShares(votes: readonly number[]) {
  if (votes.length !== 4 || votes.some(x => !Number.isFinite(x) || x < 0) || !sum(votes)) throw new Error('Invalid research share vector');
  // An explicit new-party proxy, never a claim that missing historical votes exist.
  const shares = votes.map(x => Math.max(x / sum(votes), GENERATION_STUDY_POLICY.emergingPartyMass));
  return shares.map(x => x / sum(shares));
}
export function selectForecastInput(target: Pick<ResearchEvent, 'id' | 'kind' | 'date'>,
  events: ResearchEvent[], polls: ResearchPoll[], horizon: number, regime: 'camera-only' | 'mixed'): ForecastInput | null {
  if (!Number.isFinite(Date.parse(target.date)) || !Number.isSafeInteger(horizon) || horizon < 0) throw new Error('Invalid research forecast date or horizon');
  const cutoff = cutoffFor(target.date, horizon);
  const history = events.filter(e => e.date < cutoff && (regime === 'mixed' || e.kind === 'camera')).sort((a, b) => a.date.localeCompare(b.date));
  const baseline = history.at(-1);
  if (!baseline) return null;
  // General national intentions are eligible predictors of either ballot.
  // Exact target-ballot terminal polls cannot be reused for a different event.
  const eligible = polls.filter(p => p.publicationDate <= cutoff && p.fieldworkEnd <= cutoff &&
    (!p.targetElectionId || p.targetElectionId === target.id) &&
    (p.kind !== 'european' || regime === 'mixed' || target.kind === 'european') &&
    (Date.parse(cutoff) - Date.parse(p.publicationDate)) / 86400000 <= 180);
  const terminal = eligible.filter(p => p.targetElectionId === target.id);
  const pool = terminal.length ? terminal : eligible;
  const newest = pool.map(p => p.publicationDate).sort().at(-1);
  const recent = newest ? pool.filter(p => (Date.parse(newest) - Date.parse(p.publicationDate)) / 86400000 <= 35) : [];
  const weights = recent.map(p => Math.min(p.sampleSize, 2000) * 2 ** (-(Date.parse(cutoff) - Date.parse(p.publicationDate)) / 86400000 / 30));
  const point = recent.length ? [0, 1, 2, 3].map(j => recent.reduce((n, p, i) => n + weights[i] * p.shares[j], 0) / sum(weights)) : positiveShares(baseline.national.votes);
  const turnoutBaseline = history.filter(e => e.kind === target.kind).at(-1) ?? baseline;
  return { targetId: target.id, kind: target.kind, date: target.date, cutoff, horizon, history, baseline, polls: recent,
    point: positiveShares(point), pointDate: recent.length ? cutoff : baseline.date, pollIds: recent.map(p => p.id),
    validPoint: turnoutBaseline.national.validVotes / turnoutBaseline.national.electors };
}
/** Positive-support iterative proportional fitting. Called AFTER every geographic
 * and valid-ballot draw. Weights always use forecast electors/valid fractions. */
export function reconcileMargins(shares: Float64Array, weights: Float64Array, target: readonly number[], tolerance = 1e-8) {
  if (shares.length !== weights.length * 4 || target.length !== 4 || Math.abs(sum(target) - 1) > 1e-8 ||
      target.some(p => !Number.isFinite(p) || p < 0) || weights.some(w => !Number.isFinite(w) || w < 0) ||
      shares.some(p => !Number.isFinite(p) || p < 0) || !Number.isFinite(tolerance) || tolerance <= 0 || tolerance >= 1) throw new Error('Invalid margin reconciliation input');
  const total = sum([...weights]);
  if (!total) throw new Error('No valid-vote weight');
  let error = Infinity;
  for (let iteration = 0; iteration < 300; iteration++) {
    const totals = [0, 0, 0, 0];
    for (let i = 0; i < weights.length; i++) for (let j = 0; j < 4; j++) totals[j] += weights[i] * shares[4 * i + j];
    const factors = totals.map((t, j) => {
      if (!t && target[j]) throw new Error('Infeasible positive target with zero support');
      return t ? target[j] * total / t : 0;
    });
    for (let i = 0; i < weights.length; i++) {
      let mass = 0;
      for (let j = 0; j < 4; j++) { shares[4 * i + j] *= factors[j]; mass += shares[4 * i + j]; }
      if (!mass) throw new Error('Municipality without supported category');
      for (let j = 0; j < 4; j++) shares[4 * i + j] /= mass;
    }
    const check = [0, 0, 0, 0];
    for (let i = 0; i < weights.length; i++) for (let j = 0; j < 4; j++) check[j] += weights[i] * shares[4 * i + j] / total;
    error = Math.max(...check.map((x, j) => Math.abs(x - target[j])));
    if (error < tolerance) return { iterations: iteration + 1, error };
  }
  throw new Error(`Margin reconciliation did not converge: ${error}`);
}
type Matrix = number[][];
const zeroMatrix = (d = 4): Matrix => Array.from({ length: d }, () => Array(d).fill(0));
function outerAdd(m: Matrix, v: number[], weight: number) {
  for (let i = 0; i < v.length; i++) for (let j = 0; j < v.length; j++) m[i][j] += weight * v[i] * v[j];
}
function factor(m: Matrix): Matrix {
  const l = zeroMatrix(m.length);
  for (let i = 0; i < m.length; i++) for (let j = 0; j <= i; j++) {
    const value = m[i][j] - l[i].slice(0, j).reduce((n, x, k) => n + x * l[j][k], 0);
    if (i === j && !(value > 0)) throw new Error('Nonpositive research covariance');
    l[i][j] = i === j ? Math.sqrt(value) : value / l[j][j];
  }
  return l;
}
export function eventPosition(e: ResearchEvent) {
  return [...ilr(positiveShares(e.national.votes)), logit(e.national.validVotes / e.national.electors)];
}
function forecastPosition(input: ForecastInput) {
  return [...ilr(input.point), logit(input.validPoint)];
}
interface SpatialBlock { id: string; years: number; regions: Map<string, number[]>; locals: Map<string, number[]>; }
function spatialBlock(input: ForecastInput, outcome: ResearchEvent): SpatialBlock {
  const past = new Map(input.baseline.municipalities.map(m => [m.id, m]));
  const shift = eventPosition(outcome).map((x, j) => x - eventPosition(input.baseline)[j]);
  const regions = new Map<string, { values: number[]; weight: number }>();
  const locals = new Map<string, number[]>(), regionFor = new Map<string, string>();
  for (const m of outcome.municipalities) {
    const p = past.get(m.id); if (!p || !m.validVotes || !p.validVotes) continue;
    const before = [...ilr(positiveShares(p.votes)), logit(p.validVotes / p.electors)];
    const after = [...ilr(positiveShares(m.votes)), logit(m.validVotes / m.electors)];
    const error = after.map((v, j) => v - before[j] - shift[j]);
    locals.set(m.id, error); regionFor.set(m.id, m.regionId);
    const r = regions.get(m.regionId) ?? { values: [0, 0, 0, 0], weight: 0 };
    // Outcome weights used only inside a completed historical training event.
    r.weight += m.validVotes; error.forEach((v, j) => r.values[j] += m.validVotes * v); regions.set(m.regionId, r);
  }
  const means = new Map([...regions].map(([id, r]) => [id, r.values.map(v => v / r.weight)]));
  for (const [id, value] of locals) locals.set(id, value.map((v, j) => v - means.get(regionFor.get(id)!)![j]));
  return { id: outcome.id, years: Math.max(.25, yearsBetween(outcome.date, input.baseline.date)), regions: means, locals };
}
export function fitGenerationModel(input: ForecastInput, events: ResearchEvent[], polls: ResearchPoll[], regime: 'camera-only' | 'mixed') {
  const training = events.filter(e => e.date < input.cutoff && (regime === 'mixed' || e.kind === 'camera')).flatMap(e => {
    const prior = selectForecastInput(e, events, polls, input.horizon, regime);
    return prior ? [{ event: e, prior }] : [];
  });
  const n = training.length, horizonYears = input.horizon / 365.25;
  // Priors are explicit conventions. Errors are learned at the SAME forecast
  // horizon, one complete vector per earlier election, with no sqrt-time reuse
  // of the old terminal-only polling covariance.
  const nationalPsi = zeroMatrix();
  for (let j = 0; j < 4; j++) nationalPsi[j][j] = j < 3 ? .2 ** 2 + .35 ** 2 * horizonYears : .2 ** 2;
  const errors = training.map(({ event, prior }) => eventPosition(event).map((v, j) => v - forecastPosition(prior)[j]));
  errors.forEach(e => outerAdd(nationalPsi, e, 1));
  const blocks = training.map(({ event, prior }) => spatialBlock(prior, event));
  const regionCov = zeroMatrix(), localCov = zeroMatrix();
  for (let j = 0; j < 4; j++) { regionCov[j][j] = .12 ** 2; localCov[j][j] = .18 ** 2; }
  for (const b of blocks) {
    for (const v of b.regions.values()) outerAdd(regionCov, v, 1 / Math.max(1, b.regions.size) / b.years);
    for (const v of b.locals.values()) outerAdd(localCov, v, 1 / Math.max(1, b.locals.size) / b.years);
  }
  const divide = (m: Matrix, x: number) => m.map(row => row.map(v => v / x));
  return { trainingIds: training.map(t => t.event.id), errors, blocks, df: n + 3,
    nationalFactor: factor(divide(nationalPsi, n + 1)), regionFactor: factor(divide(regionCov, n + 1)),
    localFactor: factor(divide(localCov, n + 1)) };
}
export function prepareProfiles(input: ForecastInput, pooled: boolean) {
  const histories = input.history.map(e => ({ event: e, municipalities: new Map(e.municipalities.map(m => [m.id, m])) }));
  return input.baseline.municipalities.map(m => {
    const latest = [...ilr(positiveShares(m.votes)), logit(m.validVotes / m.electors)];
    if (!pooled) return latest;
    const values = histories.flatMap(h => {
      const p = h.municipalities.get(m.id); if (!p || !p.validVotes) return [];
      const w = 2 ** (-yearsBetween(input.baseline.date, h.event.date) / GENERATION_STUDY_POLICY.profileHalfLifeYears);
      const national = eventPosition(h.event), current = eventPosition(input.baseline);
      return [{ w, z: [...ilr(positiveShares(p.votes)), logit(p.validVotes / p.electors)].map((v, j) => v - national[j] + current[j]) }];
    });
    return latest.map((_, j) => values.reduce((s, v) => s + v.w * v.z[j], 0) / sum(values.map(v => v.w)));
  });
}
export function makeModelGenerator(input: ForecastInput, fit: ReturnType<typeof fitGenerationModel>, model: GenerationModel, seed: string) {
  const profiles = prepareProfiles(input, model === 'transition-bootstrap'), centre = forecastPosition(input);
  const municipalities = input.baseline.municipalities, age = Math.max(.25, yearsBetween(input.date, input.baseline.date));
  const regionIds = [...new Set(municipalities.map(m => m.regionId))].sort();
  return (draw: number) => {
    if (!Number.isSafeInteger(draw) || draw < 0) throw new Error('Invalid research simulation ID');
    const rng = createSeededRng(`${seed}:${input.targetId}:${input.horizon}:${draw}`), normal = createNormalSampler(rng);
    const gaussian = (l: Matrix) => { const z = l.map(() => normal(0, 1)); return l.map(row => row.reduce((n, v, j) => n + v * z[j], 0)); };
    const tail = () => Math.sqrt((fit.df - 2) / sum(Array.from({ length: fit.df }, () => normal(0, 1) ** 2)));
    let nationalError = gaussian(fit.nationalFactor);
    const selected = fit.blocks.length ? Math.floor(rng() * fit.blocks.length) : -1, sign = rng() < .5 ? -1 : 1;
    if (model === 'joint-student-t') {
      // One common scale for the entire national vector, including valid rate.
      const t = tail(); nationalError = nationalError.map(v => v * t);
    }
    const national = inverseIlr(centre.slice(0, 3).map((v, j) => v + nationalError[j]));
    const targetValid = logistic(centre[3] + nationalError[3]);
    const region = new Map(regionIds.map(id => {
      let z = gaussian(fit.regionFactor).map(v => v * Math.sqrt(age));
      if (model === 'joint-student-t') { const t = tail(); z = z.map(v => v * t); }
      if (model === 'transition-bootstrap' && selected >= 0) {
        const block = fit.blocks[selected], historic = block.regions.get(id);
        z = z.map((v, j) => .35 * v + (historic?.[j] ?? 0) * sign * Math.sqrt(age / block.years));
      }
      return [id, z] as const;
    }));
    const shares = new Float64Array(municipalities.length * 4), valid = new Float64Array(municipalities.length);
    for (let i = 0; i < municipalities.length; i++) {
      const m = municipalities[i], r = region.get(m.regionId)!;
      let z = gaussian(fit.localFactor).map(v => v * Math.sqrt(age));
      if (model === 'joint-student-t') { const t = tail(); z = z.map(v => v * t); }
      if (model === 'transition-bootstrap' && selected >= 0) {
        const b = fit.blocks[selected], historic = b.locals.get(m.id);
        z = z.map((v, j) => .35 * v + (historic?.[j] ?? 0) * sign * Math.sqrt(age / b.years));
      }
      const p = inverseIlr(profiles[i].slice(0, 3).map((v, j) => v + r[j] + z[j]));
      p.forEach((v, j) => shares[4 * i + j] = v);
      valid[i] = logistic(profiles[i][3] + r[3] + z[3]);
    }
    // Reconcile the valid-ballot rate separately; it includes invalid ballots.
    const electors = sum(municipalities.map(m => m.electors));
    let low = -40, high = 40;
    const logits = valid.map(logit);
    for (let iteration = 0; iteration < 45; iteration++) {
      const mid = (low + high) / 2;
      let current = 0; for (let i = 0; i < valid.length; i++) current += municipalities[i].electors * logistic(logits[i] + mid);
      if (current / electors < targetValid) low = mid; else high = mid;
    }
    const weights = new Float64Array(valid.length);
    for (let i = 0; i < valid.length; i++) { valid[i] = logistic(logits[i] + (low + high) / 2); weights[i] = municipalities[i].electors * valid[i]; }
    const margin = reconcileMargins(shares, weights, national);
    const output = new Float64Array(valid.length * 5);
    for (let i = 0; i < valid.length; i++) { for (let j = 0; j < 4; j++) output[5 * i + j] = shares[4 * i + j]; output[5 * i + 4] = valid[i]; }
    return { output, national, targetValid, margin };
  };
}
