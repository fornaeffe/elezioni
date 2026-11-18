import { DRIFT_POLICY, type DriftProblem, type DriftParameters, type DriftCity, type CityRecord } from './drift-structure';
import { toScores, toContrasts, basis, mv, tr, dot, zeros, ouStep, type Mat } from './drift-math';

interface PairObservation { city: DriftCity; region: string; change: number; noise: number; previous: CityRecord; next: CityRecord; }
interface PairMoment { event: string; pair: [number, number]; rate: number; rows: PairObservation[]; mean: number; dt: number; regionalRate: number; }
const average = (xs: number[]) => xs.reduce((a, b) => a + b, 0) / Math.max(1, xs.length);
function measurementVariance(problem: DriftProblem, row: CityRecord, j: number, k: number) {
  const vector = Float64Array.from({ length: problem.d }, (_, a) => problem.h[j * problem.d + a] - problem.h[k * problem.d + a]);
  // The weak proper count posterior is used only for uncertainty correction of
  // this moment initializer/scale estimator; the state fit uses true count IRLS.
  const covariance = row.momentCovariance;
  return covariance ? dot(vector, mv(covariance, vector, problem.d)) : 0;
}
export interface LocalScaleAudit {
  pairs: { event: string; pair: number[]; observations: number; varianceRate: number; regionalRate: number }[];
  scale: number; outcomeShape: number[]; regionFraction: number; kappa: number;
  scaleLogSd: number; citiesWithTransitions: number; reversionProfile: { kappa: number; loss: number }[];
}
export function estimateLocalScales(problem: DriftProblem, parameters: DriftParameters): LocalScaleAudit {
  const byEvent = new Map<string, Map<string, CityRecord>>();
  for (const e of problem.elections) byEvent.set(e.id, new Map());
  for (const city of problem.cities) for (const r of city.records) byEvent.get(r.election.id)!.set(city.id, r);
  const moments: PairMoment[] = [];
  for (let ei = 1; ei < problem.elections.length; ei++) {
    const before = problem.elections[ei - 1], after = problem.elections[ei], previous = byEvent.get(before.id)!, next = byEvent.get(after.id)!;
    const dt = (Date.parse(after.date) - Date.parse(before.date)) / (365.25 * 86400000);
    for (let j = 0; j < problem.j; j++) for (let k = j + 1; k < problem.j; k++) {
      const rows: PairObservation[] = [];
      for (const city of problem.cities) {
        const a = previous.get(city.id), b = next.get(city.id);
        if (!a || !b || ![a, b].every(r => r.row.active.includes(j) && r.row.active.includes(k))) continue;
        const x = toScores(a.state, problem.h), y = toScores(b.state, problem.h);
        rows.push({ city, region: city.regionId, change: (y[j] - y[k]) - (x[j] - x[k]),
          noise: measurementVariance(problem, a, j, k) + measurementVariance(problem, b, j, k), previous: a, next: b });
      }
      if (rows.length < 3) continue;
      const mean = average(rows.map(r => r.change));
      const variance = rows.reduce((s, r) => s + (r.change - mean) ** 2, 0) / (rows.length - 1);
      const rate = (variance - average(rows.map(r => r.noise))) / dt;
      const regions = [...new Set(rows.map(r => r.region))].map(region => {
        const group = rows.filter(r => r.region === region);
        return { mean: average(group.map(r => r.change)), count: group.length, noise: average(group.map(r => r.noise)) / group.length };
      });
      const regionMean = average(regions.map(r => r.mean));
      const regionVariance = regions.reduce((s, r) => s + (r.mean - regionMean) ** 2, 0) / Math.max(1, regions.length - 1);
      const sampling = average(regions.map(r => r.noise + Math.max(0, variance - average(rows.map(x => x.noise))) / r.count));
      moments.push({ event: before.id + ':' + after.id, pair: [j, k], rate, rows, mean, dt,
        regionalRate: Math.max(0, (regionVariance - sampling) / dt) });
    }
  }
  const commonVariance = moments.length ? Math.max(.0004, average(moments.map(m => m.rate)) / 2) : DRIFT_POLICY.scaleDeltaPrior ** 2;
  const variances = Array(problem.j).fill(commonVariance);
  const outcome = problem.variant.local === 'outcome' || problem.variant.local === 'separable';
  if (outcome) for (let iteration = 0; iteration < 60; iteration++) for (let j = 0; j < problem.j; j++) {
    const relevant = moments.filter(m => m.pair.includes(j));
    if (!relevant.length) continue;
    const estimate = average(relevant.map(m => m.rate - variances[m.pair[0] === j ? m.pair[1] : m.pair[0]]));
    const information = Math.max(1, new Set(relevant.map(m => m.event)).size * problem.layout.regions.length);
    const shrink = information / (information + 1 / DRIFT_POLICY.outcomeEffectPriorSd ** 2);
    variances[j] = Math.exp((1 - shrink) * Math.log(commonVariance) + shrink * Math.log(Math.max(.0001, estimate)));
  }
  let scale = outcome ? Math.exp(average(variances.map(v => .5 * Math.log(v)))) : Math.sqrt(commonVariance);
  const shape = variances.map(v => outcome ? Math.sqrt(v) / scale : 1);
  const cityStats = new Map<string, { sum: number; pairs: number; events: Set<string> }>();
  for (const m of moments) for (const row of m.rows) {
    const stats = cityStats.get(row.city.id) ?? { sum: 0, pairs: 0, events: new Set<string>() };
    const rate = ((row.change - m.mean) ** 2 - row.noise) / m.dt;
    stats.sum += rate / (variances[m.pair[0]] + variances[m.pair[1]]);
    stats.pairs++; stats.events.add(m.event); cityStats.set(row.city.id, stats);
  }
  const cityEffects = problem.variant.local === 'municipality' || problem.variant.local === 'separable';
  parameters.cityScale = new Map(); parameters.cityLogSd = new Map();
  for (const city of problem.cities) {
    const stats = cityStats.get(city.id), effective = (stats?.events.size ?? 0) * problem.d, df = DRIFT_POLICY.municipalityPriorDf;
    const ratio = stats ? stats.sum / stats.pairs : 1;
    const factor2 = (df + effective * Math.max(0, ratio)) / (df + effective);
    if (cityEffects) parameters.cityScale.set(city.id, Math.sqrt(factor2));
    parameters.cityLogSd.set(city.id, cityEffects ? 1 / Math.sqrt(2 * (df + effective)) : 0);
  }
  if (cityEffects) {
    const centre = Math.exp(average([...parameters.cityScale.values()].map(Math.log)));
    for (const [id, value] of parameters.cityScale) parameters.cityScale.set(id, value / centre);
    scale *= centre;
  }
  parameters.delta = scale; parameters.deltaShape = shape;
  const ratio = moments.length ? average(moments.map(m => m.regionalRate / Math.max(.0008, m.rate))) : .2;
  parameters.regionFraction = problem.variant.regional ? Math.max(.01, Math.min(.85, (ratio * moments.length + .2 * 4) / (moments.length + 4))) : 0;
  const reversionProfile: { kappa: number; loss: number }[] = [];
  if (problem.variant.reversion) {
    // Spatial deviations are relative to persistent first observed geography.
    const centres = new Map<string, number[]>();
    for (const e of problem.elections) {
      const rows = [...byEvent.get(e.id)!.values()];
      centres.set(e.id, Array.from({ length: problem.j }, (_, j) => average(rows.map(r => toScores(r.state, problem.h)[j]))));
    }
    for (const kappa of [0, .1, .25, .5, 1, 2]) {
      let loss = .5 * (kappa / .5) ** 2;
      for (const m of moments) {
        const [j, k] = m.pair, step = ouStep(m.dt, kappa);
        let part = 0;
        for (const row of m.rows) {
          const first = row.city.records.find(r => r.row.active.includes(j) && r.row.active.includes(k));
          if (!first || first.time > row.previous.time) continue;
          const f = toScores(first.state, problem.h), a = toScores(row.previous.state, problem.h), b = toScores(row.next.state, problem.h);
          const cf = centres.get(first.election.id)!, ca = centres.get(row.previous.election.id)!, cb = centres.get(row.next.election.id)!;
          const initial = f[j] - f[k] - cf[j] + cf[k];
          const old = a[j] - a[k] - ca[j] + ca[k] - initial, next = b[j] - b[k] - cb[j] + cb[k] - initial;
          const variance = (variances[j] + variances[k]) * step.varianceTime + row.noise;
          part += .5 * (Math.log(variance) + (next - step.phi * old) ** 2 / variance);
        }
        // Equal election blocks, not all correlated pair contrasts as independent.
        loss += part / Math.max(1, m.rows.length) / (problem.j * (problem.j - 1) / 2) * problem.layout.regions.length;
      }
      reversionProfile.push({ kappa, loss });
    }
    parameters.kappa = reversionProfile.reduce((best, r) => r.loss < best.loss ? r : best).kappa;
  }
  const blocks = Math.max(3, new Set(moments.map(m => m.event)).size * problem.layout.regions.length);
  return { pairs: moments.map(m => ({ event: m.event, pair: m.pair, observations: m.rows.length, varianceRate: m.rate, regionalRate: m.regionalRate })),
    scale, outcomeShape: shape, regionFraction: parameters.regionFraction, kappa: parameters.kappa,
    scaleLogSd: 1 / Math.sqrt(2 * blocks), citiesWithTransitions: cityStats.size, reversionProfile };
}
