import type { DriftPoll } from './drift-data';
import type { DriftProblem, DriftParameters, DriftCity, CollapsedCounts } from './drift-structure';
import { localMean } from './drift-structure';
import { basis, zeros, eye, add, mm, mv, tr, sandwich, toScores, softmax, toContrasts,
  innovationCov, dateDay, type Mat } from './drift-math';
import type { LocalGaussian } from './drift-gaussian';

export interface CovarianceGrid {
  times: number[]; globalStart: number; p: Mat[]; gains: Mat[]; conditional: Mat[];
}
export interface CityPollGeometry {
  grid: CovarianceGrid; jacobian: Mat; probabilities: Mat; shift: Mat[];
  referenceCross: Mat;
}
export interface PollGeometry {
  p: number; jacobian: Mat; response: Mat; measurement: Mat; geographic: Mat; covariance: Mat;
  cities: Map<string, CityPollGeometry>; regionalJacobian: Map<string, Mat>;
  offsets: number[]; times: number[]; update?: Mat; mode: Mat;
}
export function covarianceResponse(grid: CovarianceGrid, forces: Mat[], d: number) {
  const n = grid.times.length, accumulated: Mat[] = [];
  accumulated.push(forces[0].slice());
  for (let t = 1; t < n; t++) accumulated.push(add(forces[t], mv(tr(grid.gains[t - 1], d, d), accumulated[t - 1], d)));
  const out = Array.from({ length: n }, () => zeros(d, 1));
  out[n - 1] = mv(grid.p[n - 1], accumulated[n - 1], d);
  for (let t = n - 2; t >= 0; t--) out[t] = add(mv(grid.gains[t], out[t + 1], d), mv(grid.conditional[t], accumulated[t], d));
  return out;
}
export function jacobianForces(geometry: PollGeometry, local: CityPollGeometry, update: Mat, d: number) {
  const forces = local.grid.times.map(() => zeros(d, 1));
  geometry.times.forEach((time, q) => {
    const index = local.grid.times.indexOf(time); if (index < 0) return;
    const o = (q + 1 < geometry.offsets.length ? geometry.offsets[q + 1] : geometry.p) - geometry.offsets[q];
    for (let r = 0; r < o; r++) for (let j = 0; j < d; j++) forces[index][j] += local.jacobian[(geometry.offsets[q] + r) * d + j] * update[geometry.offsets[q] + r];
  });
  return forces;
}
function sampleCovariance(poll: DriftPoll) {
  const o = poll.shares.length - 1, hb = basis(poll.shares.length), r = zeros(o);
  for (let a = 0; a < o; a++) for (let b = 0; b < o; b++) for (let j = 0; j < poll.shares.length; j++)
    r[a * o + b] += hb[j * o + a] * hb[j * o + b] * (1 / (poll.sample * poll.shares[j]) + (.0005 ** 2 / 3) / poll.shares[j] ** 2);
  return r;
}
/** Jacobian of observed grouped log-ratios with respect to full political log
 * masses. Grouping is exact; no independent allocation to missing parties. */
function pollTransform(politicalLogMass: Mat, poll: DriftPoll) {
  const p = softmax(politicalLogMass), groups = poll.partitions.map(g => g.reduce((s, j) => s + p[j - 1], 0));
  const k = groups.length, o = k - 1, hb = basis(k), j = zeros(o, p.length);
  for (let a = 0; a < o; a++) poll.partitions.forEach((group, g) => {
    for (const idx of group) j[a * p.length + idx - 1] = hb[g * o + a] * p[idx - 1] / groups[g];
  });
  return { mean: toContrasts(groups, hb), derivative: j };
}
function gridFor(gp: LocalGaussian, problem: DriftProblem): CovarianceGrid {
  const globalStart = problem.layout.times.findIndex(t => t >= gp.first - 1e-10);
  const times = problem.layout.times.slice(globalStart), grid = gp.grid(times);
  return { times, globalStart, p: grid.queries.map(q => q.p), gains: grid.gains, conditional: grid.conditional };
}
function bridgeTime(problem: DriftProblem, left: number, right: number) {
  if (!problem.layout.regional) return 0;
  const knots = problem.layout.regionTimes;
  const interval = knots.findIndex(t => t > left);
  if (interval < 1 || left === knots[interval - 1] || right >= knots[interval]) return 0;
  const a = knots[interval - 1], b = knots[interval];
  return Math.min(left, right) - a - (left - a) * (right - a) / (b - a);
}
export function buildPollGeometry(problem: DriftProblem, parameters: DriftParameters, counts: CollapsedCounts,
  mode: Mat, previous?: PollGeometry): PollGeometry {
  const { d, j, layout } = problem, m = layout.m, times = problem.polls.map(p => dateDay(p.date) / 365.25);
  const offsets: number[] = []; let p = 0;
  for (const q of problem.polls) { offsets.push(p); p += q.shares.length - 1; }
  const jacobian = zeros(p, m), response = zeros(p, 1), measurement = zeros(p), geographic = zeros(p);
  const cells = new Map<string, CityPollGeometry>(), regionalJacobian = new Map(layout.regions.map(r => [r, zeros(p, d)]));
  const politicalBasis = basis(j - 1), rawTransforms: ReturnType<typeof pollTransform>[] = [];
  const priorUpdate = previous?.update;
  const oldRegionalShifts = new Map<string, Mat[]>();
  if (parameters.regionFraction && priorUpdate && previous) {
    const rq = innovationCov(parameters.deltaShape.map(v => v * parameters.delta * Math.sqrt(parameters.regionFraction)), problem.h);
    for (const region of layout.regions) {
      const oldJacobian = previous.regionalJacobian.get(region)!;
      oldRegionalShifts.set(region, times.map(time => {
        const out = zeros(d, 1);
        previous.times.forEach((s, si) => {
          const bt = bridgeTime(problem, Math.min(time, s), Math.max(time, s)); if (!bt) return;
          const os = (si + 1 < offsets.length ? offsets[si + 1] : p) - offsets[si];
          for (let a = 0; a < d; a++) for (let b = 0; b < d; b++) for (let r = 0; r < os; r++)
            out[a] += bt * rq[a * d + b] * oldJacobian[(offsets[si] + r) * d + b] * priorUpdate[offsets[si] + r];
        });
        return out;
      }));
    }
  }
  const domain = problem.polls.map(q => {
    const eligible = problem.elections.filter(e => e.date < q.date.slice(0, 10));
    const e = eligible.filter(e => e.kind === q.kind).at(-1) ?? eligible.at(-1);
    return new Map((e?.municipalities ?? []).map(row => [row.id, row.electors]));
  });
  const totals = problem.polls.map(() => zeros(j, 1)), totalDerivatives = problem.polls.map(() => zeros(j, m));
  const localDot = (full: Mat, reduced: Mat, city: DriftCity, row: number) => {
    let out = 0; for (let k = 0; k < city.columns.length; k++) out += full[city.columns[k]] * reduced[row * city.columns.length + k]; return out;
  };
  for (const city of problem.cities) {
    const gp = counts.locals.get(city.id)!, grid = gridFor(gp, problem);
    const old = previous?.cities.get(city.id), shift = priorUpdate && old ?
      covarianceResponse(grid, jacobianForces(previous!, { ...old, grid }, priorUpdate, d), d) : grid.times.map(() => zeros(d, 1));
    const cell: CityPollGeometry = { grid, jacobian: zeros(p, d), probabilities: zeros(problem.polls.length, j), shift, referenceCross: zeros(d, p) };
    cells.set(city.id, cell);
    if (problem.variant.pollLink === 'literal') continue;
    problem.polls.forEach((poll, qi) => {
      const electors = domain[qi].get(city.id); if (!electors || times[qi] < city.first) return;
      const local = localMean(problem, city, gp, mode, times[qi]), idx = grid.times.indexOf(times[qi]);
      const blocks = layout.blocks(city, times[qi], poll.kind, parameters.cityScale.get(city.id) ?? 1);
      const state = add(city.theta, add(local.mean, shift[idx]));
      for (let r = 0; r < d; r++) for (const [o, w] of blocks) state[r] += w * mode[o + r];
      const regionalShift = oldRegionalShifts.get(city.regionId)?.[qi];
      if (regionalShift) for (let r = 0; r < d; r++) state[r] += (parameters.cityScale.get(city.id) ?? 1) * regionalShift[r];
      const probability = softmax(toScores(state, problem.h));
      probability.forEach((v, k) => { cell.probabilities[qi * j + k] = v; totals[qi][k] += electors * v; });
      const meanH = zeros(d, 1);
      for (let r = 0; r < d; r++) for (let k = 0; k < j; k++) meanH[r] += probability[k] * problem.h[k * d + r];
      for (let k = 0; k < j; k++) {
        const coeff = Float64Array.from({ length: d }, (_, r) => electors * probability[k] * (problem.h[k * d + r] - meanH[r]));
        for (let r = 0; r < d; r++) for (const [o, w] of blocks) totalDerivatives[qi][k * m + o + r] += coeff[r] * w;
        city.columns.forEach((col, c) => {
          for (let r = 0; r < d; r++) totalDerivatives[qi][k * m + col] += coeff[r] * local.query.f[r * city.columns.length + c];
        });
      }
    });
  }
  problem.polls.forEach((poll, qi) => {
    const offset = offsets[qi], o = poll.shares.length - 1, bias = layout.group(poll.group);
    const mass = zeros(j - 1, 1);
    let rawScores: Mat;
    if (problem.variant.pollLink === 'literal') {
      const a = mode.slice(layout.national(times[qi]), layout.national(times[qi]) + d);
      if (layout.kindEffects && poll.kind === 'european') for (let r = 0; r < d; r++) a[r] += mode[layout.kindOffset + r];
      rawScores = toScores(a, problem.h);
      for (let k = 0; k < j - 1; k++) mass[k] = rawScores[k + 1];
    } else for (let k = 0; k < j - 1; k++) {
      if (!(totals[qi][k + 1] > 0)) throw new Error('Aggregate poll has no available geographic support');
      mass[k] = Math.log(totals[qi][k + 1]);
    }
    for (let k = 0; k < j - 1; k++) for (let b = 0; b < d - 1; b++) mass[k] += politicalBasis[k * (d - 1) + b] * mode[bias + b];
    const transform = pollTransform(mass, poll); rawTransforms.push(transform);
    const observed = toContrasts(poll.shares, basis(poll.shares.length)), rq = sampleCovariance(poll);
    for (let r = 0; r < o; r++) {
      for (let b = 0; b < d - 1; b++) for (let k = 0; k < j - 1; k++) jacobian[(offset + r) * m + bias + b] += transform.derivative[r * (j - 1) + k] * politicalBasis[k * (d - 1) + b];
      if (problem.variant.pollLink === 'literal') {
        for (let a = 0; a < d; a++) for (let k = 0; k < j - 1; k++) {
          const v = transform.derivative[r * (j - 1) + k] * problem.h[(k + 1) * d + a];
          jacobian[(offset + r) * m + layout.national(times[qi]) + a] += v;
          if (layout.kindEffects && poll.kind === 'european') jacobian[(offset + r) * m + layout.kindOffset + a] += v;
        }
      } else {
        for (let k = 0; k < j - 1; k++) {
          const coeff = transform.derivative[r * (j - 1) + k] / totals[qi][k + 1];
          for (let c = 0; c < m; c++) jacobian[(offset + r) * m + c] += coeff * totalDerivatives[qi][(k + 1) * m + c];
        }
      }
      let jm = 0; for (let c = 0; c < m; c++) jm += jacobian[(offset + r) * m + c] * mode[c];
      response[offset + r] = observed[r] - transform.mean[r] + jm;
      for (let s = 0; s < o; s++) measurement[(offset + r) * p + offset + s] = rq[r * o + s];
    }
  });
  if (problem.variant.pollLink === 'aggregate') for (const city of problem.cities) {
    const cell = cells.get(city.id)!, { grid } = cell;
    problem.polls.forEach((poll, qi) => {
      const electors = domain[qi].get(city.id); if (!electors || times[qi] < city.first) return;
      const offset = offsets[qi], o = poll.shares.length - 1, probability = cell.probabilities.slice(qi * j, qi * j + j);
      const meanH = zeros(d, 1);
      for (let r = 0; r < d; r++) for (let k = 0; k < j; k++) meanH[r] += probability[k] * problem.h[k * d + r];
      for (let r = 0; r < o; r++) for (let a = 0; a < d; a++) {
        let v = 0;
        for (let k = 0; k < j - 1; k++) v += rawTransforms[qi].derivative[r * (j - 1) + k] * electors * probability[k + 1] *
          (problem.h[(k + 1) * d + a] - meanH[a]) / totals[qi][k + 1];
        cell.jacobian[(offset + r) * d + a] = v;
        regionalJacobian.get(city.regionId)![(offset + r) * d + a] += v * (parameters.cityScale.get(city.id) ?? 1);
        response[offset + r] += v * cell.shift[grid.times.indexOf(times[qi])][a];
      }
    });
    // Exact Gaussian cross-time field covariance, using backward Markov gains.
    for (let qi = 0; qi < problem.polls.length; qi++) {
      let index = grid.times.indexOf(times[qi]); if (index < 0) continue;
      const oq = problem.polls[qi].shares.length - 1, offq = offsets[qi];
      let transport = cell.jacobian.slice(offq * d, (offq + oq) * d);
      if (!transport.some(v => v !== 0)) continue;
      for (let ri = qi; ri < problem.polls.length; ri++) {
        const next = grid.times.indexOf(times[ri]); if (next < 0) continue;
        while (index < next) { transport = mm(transport, grid.gains[index], oq, d, d); index++; }
        const or = problem.polls[ri].shares.length - 1, offr = offsets[ri];
        const jr = cell.jacobian.slice(offr * d, (offr + or) * d);
        const block = mm(mm(transport, grid.p[index], oq, d, d), tr(jr, or, d), oq, d, or);
        for (let a = 0; a < oq; a++) for (let b = 0; b < or; b++) {
          geographic[(offq + a) * p + offr + b] += block[a * or + b];
          if (ri !== qi) geographic[(offr + b) * p + offq + a] += block[a * or + b];
        }
      }
      while (index < grid.times.length - 1) { transport = mm(transport, grid.gains[index], oq, d, d); index++; }
      const cross = mm(grid.p[index], tr(transport, oq, d), d, d, oq);
      for (let a = 0; a < d; a++) for (let b = 0; b < oq; b++) cell.referenceCross[a * p + offq + b] = cross[a * oq + b];
    }
  }
  if (parameters.regionFraction && problem.variant.pollLink === 'aggregate') {
    const rq = innovationCov(parameters.deltaShape.map(v => v * parameters.delta * Math.sqrt(parameters.regionFraction)), problem.h);
    for (const region of layout.regions) for (let qi = 0; qi < problem.polls.length; qi++) for (let ri = qi; ri < problem.polls.length; ri++) {
      const weight = bridgeTime(problem, times[qi], times[ri]); if (!weight) continue;
      const oq = problem.polls[qi].shares.length - 1, or = problem.polls[ri].shares.length - 1;
      const j = regionalJacobian.get(region)!, jq = j.slice(offsets[qi] * d, (offsets[qi] + oq) * d), jr = j.slice(offsets[ri] * d, (offsets[ri] + or) * d);
      const block = mm(mm(jq, rq, oq, d, d), tr(jr, or, d), oq, d, or);
      for (let a = 0; a < oq; a++) for (let b = 0; b < or; b++) {
        geographic[(offsets[qi] + a) * p + offsets[ri] + b] += weight * block[a * or + b];
        if (ri !== qi) geographic[(offsets[ri] + b) * p + offsets[qi] + a] += weight * block[a * or + b];
      }
    }
    for (const region of layout.regions) {
      const shifts = oldRegionalShifts.get(region); if (!shifts) continue;
      const jr = regionalJacobian.get(region)!;
      problem.polls.forEach((poll, qi) => {
        for (let r = 0; r < poll.shares.length - 1; r++) for (let a = 0; a < d; a++) response[offsets[qi] + r] += jr[(offsets[qi] + r) * d + a] * shifts[qi][a];
      });
    }
  }
  return { p, jacobian, response, measurement, geographic, covariance: add(measurement, geographic), cities: cells,
    regionalJacobian, offsets, times, mode: mode.slice() };
}
