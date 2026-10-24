import type { DriftInputs, DriftElection, DriftMunicipality, DriftPoll } from './drift-data';
import { availableInputs, DRIFT_OUTCOMES } from './drift-data';
import { LocalGaussian, type GaussianObservation } from './drift-gaussian';
import { zeros, eye, basis, toContrasts, toScores, initialCountPosterior, countMessage, innovationCov,
  mv, mm, tr, add, chol, inverse, logdet, dateDay, type Mat, type CountMessage } from './drift-math';

export const DRIFT_POLICY = {
  version: 'elector-latent-drift-v1', initialScoreSd: 3, nationalInitialSd: 1.5, kindSd: .5,
  emergingPriorMass: .01, scaleEtaPrior: .35, scaleDeltaPrior: .25, scalePollPrior: .12,
  emergingGeographySd: .5,
  logScalePriorSd: .7, logPollPriorSd: .6, outcomeEffectPriorSd: .35, municipalityPriorDf: 12,
  stateIterations: 20, stateTolerance: .002, momentDraws: 12,
  developmentIds: ['camera-2013', 'european-2014', 'camera-2018', 'european-2019'],
  confirmationIds: ['camera-2022', 'european-2024'], horizons: [730, 365, 90, 15],
  screeningDraws: 128, finalDraws: 512,
  scope: 'four named research political outcomes plus actual non-valid voting'
} as const;
export interface DriftVariant {
  id: string; pollLink: 'literal' | 'aggregate'; kindEffects: boolean;
  national: 'common' | 'outcome'; local: 'common' | 'outcome' | 'municipality' | 'separable';
  regional: boolean; reversion: boolean;
}
export const BASE_DRIFT: DriftVariant = { id: 'literal-pooled-common', pollLink: 'literal', kindEffects: false,
  national: 'common', local: 'common', regional: false, reversion: false };
export interface DriftParameters {
  eta: number; delta: number; poll: number; etaShape: number[]; deltaShape: number[];
  cityScale: Map<string, number>; cityLogSd: Map<string, number>;
  regionFraction: number; kappa: number;
}
export interface CityRecord { election: DriftElection; row: DriftMunicipality; time: number; state: Mat; message?: CountMessage; momentCovariance?: Mat; }
export interface DriftCity {
  id: string; regionId: string; records: CityRecord[]; theta: Mat; initial: Mat;
  first: number; firstKind: DriftElection['kind']; columns: number[];
  columnMap: Map<number, number>;
}
export interface DriftProblem {
  inputVersion: string; cutoff: string; cutoffTime: number; targetKind: DriftElection['kind']; variant: DriftVariant;
  outcomes: readonly string[];
  elections: DriftElection[]; polls: DriftPoll[]; cities: DriftCity[]; cityById: Map<string, DriftCity>;
  forecast: { city: DriftCity; electors: number; electorSource: string }[];
  layout: DriftLayout; j: number; d: number; h: Mat; nationalMean: Mat; sourceVersions: string[];
}
export class DriftLayout {
  readonly times: number[]; readonly regions: string[]; readonly regionTimes: number[];
  readonly groups: string[]; readonly m: number; readonly core: number; readonly kindOffset: number;
  readonly regionOffset: number; readonly nationalLength: number;
  private readonly timeIndex: Map<number, number>; private readonly regionIndex: Map<string, number>;
  private readonly groupIndex: Map<string, number>;
  constructor(readonly d: number, readonly elections: DriftElection[], readonly polls: DriftPoll[],
    readonly cutoff: number, readonly kindEffects: boolean, readonly regional: boolean) {
    this.times = [...new Set([...elections.map(e => dateDay(e.date) / 365.25), ...polls.map(p => dateDay(p.date) / 365.25), cutoff])].sort((a, b) => a - b);
    this.regionTimes = [...new Set([...elections.map(e => dateDay(e.date) / 365.25), cutoff])].sort((a, b) => a - b);
    this.regions = [...new Set(elections.flatMap(e => e.municipalities.map(m => m.regionId)))].sort();
    this.groups = [...new Set(polls.map(p => p.group))].sort();
    this.timeIndex = new Map(this.times.map((t, i) => [t, i])); this.regionIndex = new Map(this.regions.map((r, i) => [r, i]));
    this.groupIndex = new Map(this.groups.map((g, i) => [g, i]));
    this.nationalLength = this.times.length * d;
    this.core = this.nationalLength + this.groups.length * (d - 1);
    this.kindOffset = this.core; this.regionOffset = this.core + (kindEffects ? d : 0);
    this.m = this.regionOffset + (regional ? this.regions.length * this.regionTimes.length * d : 0);
  }
  national(time: number) {
    const i = this.timeIndex.get(time); if (i === undefined) throw new Error('Unknown national state time'); return i * this.d;
  }
  group(id: string) {
    const i = this.groupIndex.get(id); if (i === undefined) throw new Error('Unknown shared survey period'); return this.nationalLength + i * (this.d - 1);
  }
  regionalWeights(region: string, time: number): [number, number][] {
    if (!this.regional) return [];
    const r = this.regionIndex.get(region); if (r === undefined) throw new Error('Unknown region');
    const exact = this.regionTimes.indexOf(time);
    const offset = this.regionOffset + r * this.regionTimes.length * this.d;
    if (exact >= 0) return [[offset + exact * this.d, 1]];
    const right = this.regionTimes.findIndex(t => t > time);
    if (right < 1) throw new Error('Regional state outside knots');
    const w = (time - this.regionTimes[right - 1]) / (this.regionTimes[right] - this.regionTimes[right - 1]);
    return [[offset + (right - 1) * this.d, 1 - w], [offset + right * this.d, w]];
  }
  blocks(city: DriftCity, time: number, kind: DriftElection['kind'], factor: number): [number, number][] {
    const out: [number, number][] = [[this.national(time), 1], [this.national(city.first), -1]];
    if (this.kindEffects && kind !== city.firstKind) out.push([this.kindOffset, kind === 'european' ? 1 : -1]);
    for (const [o, w] of this.regionalWeights(city.regionId, time)) out.push([o, w * factor]);
    for (const [o, w] of this.regionalWeights(city.regionId, city.first)) out.push([o, -w * factor]);
    return out;
  }
  design(city: DriftCity, time: number, kind: DriftElection['kind'], factor: number): Mat {
    const a = zeros(this.d, this.m), now = this.national(time), first = this.national(city.first);
    for (let r = 0; r < this.d; r++) {
      a[r * this.m + now + r] += 1; a[r * this.m + first + r] -= 1;
      if (this.kindEffects) a[r * this.m + this.kindOffset + r] += Number(kind === 'european') - Number(city.firstKind === 'european');
      for (const [idx, w] of this.regionalWeights(city.regionId, time)) a[r * this.m + idx + r] += w * factor;
      for (const [idx, w] of this.regionalWeights(city.regionId, city.first)) a[r * this.m + idx + r] -= w * factor;
    }
    return a;
  }
}
function priorProbabilities(e: DriftElection) {
  const total = e.national.electors;
  const p = e.national.counts.map((n, i) => n > 0 ? n / total : i ? DRIFT_POLICY.emergingPriorMass : .05);
  const s = p.reduce((a, b) => a + b, 0); return p.map(v => v / s);
}
export function makeDriftProblem(inputs: DriftInputs, cutoff: string, kind: DriftElection['kind'], variant: DriftVariant): DriftProblem {
  const available = availableInputs(inputs, cutoff), elections = available.elections.sort((a, b) => a.date.localeCompare(b.date));
  if (!elections.length) throw new Error('No historical election before cutoff');
  const firstTime = dateDay(elections[0].date) / 365.25;
  const polls = available.polls.filter(p => dateDay(p.date) / 365.25 >= firstTime).sort((a, b) => a.date.localeCompare(b.date) || a.id.localeCompare(b.id));
  const j = inputs.outcomes?.length ?? DRIFT_OUTCOMES.length, d = j - 1, h = basis(j), cutoffTime = dateDay(cutoff) / 365.25;
  const layout = new DriftLayout(d, elections, polls, cutoffTime, variant.kindEffects, variant.regional);
  const byId = new Map<string, CityRecord[]>(), means = new Map(elections.map(e => [e.id, toContrasts(priorProbabilities(e), h)]));
  for (const e of elections) for (const row of e.municipalities) {
    if (row.active.reduce((n, a) => n + row.counts[a], 0) !== row.electors) throw new Error('Ballot mask drops actual votes: ' + e.id + '/' + row.id);
    const records = byId.get(row.id) ?? [];
    records.push({ election: e, row, time: dateDay(e.date) / 365.25, state: zeros(d, 1) }); byId.set(row.id, records);
  }
  const cities: DriftCity[] = [...byId].sort(([a], [b]) => a.localeCompare(b)).map(([id, records]) => {
    const first = records[0], priorMean = means.get(first.election.id)!;
    const observed = countMessage(first.row.counts, first.row.active, priorMean, h).a;
    const observedProjection = mm(tr(observed, first.row.active.length - 1, d), observed, d, first.row.active.length - 1, d);
    const priorCovariance = add(observedProjection.map(v => v * DRIFT_POLICY.initialScoreSd ** 2),
      add(eye(d), observedProjection, -1), DRIFT_POLICY.emergingGeographySd ** 2);
    const initial = initialCountPosterior(first.row.counts, first.row.active, priorMean, h, DRIFT_POLICY.initialScoreSd, priorCovariance);
    first.state = initial.mean; first.momentCovariance = initial.covariance;
    for (const r of records.slice(1)) {
      const cell = initialCountPosterior(r.row.counts, r.row.active, means.get(r.election.id)!, h, DRIFT_POLICY.initialScoreSd);
      r.state = cell.mean; r.momentCovariance = cell.covariance;
    }
    const city: DriftCity = { id, regionId: first.row.regionId, records, theta: initial.mean, initial: initial.covariance,
      first: first.time, firstKind: first.election.kind, columns: [], columnMap: new Map() };
    const columns = new Set<number>();
    for (const rec of records) {
      const design = layout.design(city, rec.time, rec.election.kind, 1);
      for (let c = 0; c < layout.m; c++) for (let r = 0; r < d; r++) if (design[r * layout.m + c]) columns.add(c);
    }
    city.columns = [...columns].sort((a, b) => a - b); city.columnMap = new Map(city.columns.map((c, i) => [c, i]));
    return city;
  });
  const cityById = new Map(cities.map(c => [c.id, c]));
  const baseline = elections.at(-1)!, same = elections.filter(e => e.kind === kind).at(-1) ?? baseline;
  const sameRows = new Map(same.municipalities.map(m => [m.id, m]));
  const forecast = baseline.municipalities.map(row => ({ city: cityById.get(row.id)!, electors: sameRows.get(row.id)?.electors ?? row.electors,
    electorSource: sameRows.has(row.id) ? same.id : baseline.id }));
  return { inputVersion: inputs.version, cutoff, cutoffTime, targetKind: kind, variant, outcomes: inputs.outcomes ?? DRIFT_OUTCOMES, elections, polls, cities, cityById,
    forecast, layout, j, d, h, nationalMean: means.get(elections[0].id)!, sourceVersions: inputs.sourceVersions };
}
export function defaultParameters(problem: DriftProblem): DriftParameters {
  return { eta: DRIFT_POLICY.scaleEtaPrior, delta: DRIFT_POLICY.scaleDeltaPrior, poll: DRIFT_POLICY.scalePollPrior,
    etaShape: Array(problem.j).fill(1), deltaShape: Array(problem.j).fill(1), cityScale: new Map(), cityLogSd: new Map(),
    regionFraction: problem.variant.regional ? .25 : 0, kappa: problem.variant.reversion ? .2 : 0 };
}
export function updateWorkingObservations(problem: DriftProblem) {
  let correction = 0;
  for (const c of problem.cities) for (const r of c.records.slice(1)) {
    r.message = countMessage(r.row.counts, r.row.active, r.state, problem.h);
    const m = r.message, o = m.y.length, l = chol(m.r, o), diff = add(m.y, mv(m.a, r.state, o), -1);
    const iv = mv(inverse(l, o), diff, o);
    correction += m.logLikelihood + .5 * (logdet(l, o) + o * Math.log(2 * Math.PI) + diff.reduce((s, v, i) => s + v * iv[i], 0));
  }
  return correction;
}
export interface CollapsedCounts { precision: Mat; linear: Mat; constant: number; locals: Map<string, LocalGaussian>; }
export function collapseCounts(problem: DriftProblem, parameters: DriftParameters): CollapsedCounts {
  const { m } = problem.layout, { d } = problem, precision = zeros(m), linear = zeros(m, 1), locals = new Map<string, LocalGaussian>();
  const baseQ = innovationCov(parameters.deltaShape.map(v => v * parameters.delta), problem.h);
  let constant = 0;
  for (const city of problem.cities) {
    const factor = parameters.cityScale.get(city.id) ?? 1, q = baseQ.map(v => v * factor ** 2 * (1 - parameters.regionFraction));
    const g = city.columns.length;
    const obs: GaussianObservation[] = city.records.slice(1).map(record => {
      const msg = record.message!, full = problem.layout.design(city, record.time, record.election.kind, factor);
      const reduced = zeros(d, g);
      for (let r = 0; r < d; r++) city.columns.forEach((c, i) => reduced[r * g + i] = full[r * m + c]);
      return { time: record.time, message: msg, design: mm(msg.a, reduced, msg.y.length, d, g) };
    });
    const gp = new LocalGaussian(d, g, city.first, city.initial, q, parameters.kappa, obs, city.theta);
    for (let i = 0; i < g; i++) {
      linear[city.columns[i]] += gp.linear[i];
      for (let k = 0; k < g; k++) precision[city.columns[i] * m + city.columns[k]] += gp.precision[i * g + k];
    }
    constant += gp.constant; locals.set(city.id, gp);
  }
  return { precision, linear, constant, locals };
}
export function localMean(problem: DriftProblem, city: DriftCity, gp: LocalGaussian, global: Mat, time: number) {
  const query = gp.query(time), reduced = Float64Array.from(city.columns, c => global[c]);
  return { query, mean: add(query.u, mv(query.f, reduced, problem.d)) };
}
export function globalPrior(problem: DriftProblem, parameters: DriftParameters) {
  const { d, layout } = problem, { m } = layout, a = zeros(m), b = zeros(m, 1);
  let logPrecisionDet = 0, priorQuadratic = 0;
  const diagonal = (offset: number, n: number, variance: number, mean?: Mat) => {
    for (let j = 0; j < n; j++) { a[(offset + j) * m + offset + j] += 1 / variance; if (mean) { b[offset + j] += mean[j] / variance; priorQuadratic += mean[j] ** 2 / variance; } }
    logPrecisionDet -= n * Math.log(variance);
  };
  const chain = (offsets: number[], times: number[], covariance: Mat) => {
    const l = chol(covariance, d), inv = inverse(l, d), ld = logdet(l, d);
    for (let t = 1; t < offsets.length; t++) {
      const dt = times[t] - times[t - 1], left = offsets[t - 1], right = offsets[t];
      for (let j = 0; j < d; j++) for (let k = 0; k < d; k++) {
        const v = inv[j * d + k] / dt;
        a[(left + j) * m + left + k] += v; a[(right + j) * m + right + k] += v;
        a[(left + j) * m + right + k] -= v; a[(right + j) * m + left + k] -= v;
      }
      logPrecisionDet -= ld + d * Math.log(dt);
    }
  };
  diagonal(0, d, DRIFT_POLICY.nationalInitialSd ** 2, problem.nationalMean);
  chain(layout.times.map(t => layout.national(t)), layout.times, innovationCov(parameters.etaShape.map(v => v * parameters.eta), problem.h));
  for (const group of layout.groups) diagonal(layout.group(group), d - 1, parameters.poll ** 2);
  if (layout.kindEffects) diagonal(layout.kindOffset, d, DRIFT_POLICY.kindSd ** 2);
  if (layout.regional) for (const region of layout.regions) {
    const offsets = layout.regionTimes.map(t => layout.regionalWeights(region, t)[0][0]);
    diagonal(offsets[0], d, .01 ** 2);
    chain(offsets, layout.regionTimes, innovationCov(parameters.deltaShape.map(v => v * parameters.delta * Math.sqrt(parameters.regionFraction)), problem.h));
  }
  return { precision: a, linear: b, logPrecisionDet, priorQuadratic };
}
