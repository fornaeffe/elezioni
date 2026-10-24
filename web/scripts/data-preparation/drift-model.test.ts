import { describe, expect, it } from 'vitest';
import { createSeededRng, createNormalSampler } from '../../src/lib/core/rng';
import { fitDrift } from './drift-fit';
import { makeDriftPredictor } from './drift-predict';
import { makeDriftProblem, defaultParameters, updateWorkingObservations, collapseCounts, BASE_DRIFT, globalPrior } from './drift-structure';
import { buildPollGeometry } from './drift-polls';
import { GlobalSystem } from './drift-global';
import { currentDriftComparator, evaluateDriftSamples } from './drift-evaluate';
import { zeros, eye, add, chol, solve, inverse, mv, mm, tr, softmax, toContrasts, basis, dot } from './drift-math';
import type { DriftInputs, DriftElection, DriftPoll } from './drift-data';

function event(id: string, date: string, kind: DriftElection['kind'], shift: number): DriftElection {
  const municipalities = Array.from({ length: 6 }, (_, i) => {
    const electors = 1000 + 100 * i, counts = [250 + i * 5, 200 + shift + i * 10, 100 - shift, 150, 0];
    counts[4] = electors - counts.slice(0, 4).reduce((a, b) => a + b, 0);
    return { id: 'c' + i, regionId: 'r' + (i % 2), electors, counts, active: [0, 1, 2, 3, 4] };
  });
  return { id, date, kind, municipalities, national: { electors: municipalities.reduce((s, r) => s + r.electors, 0),
    counts: Array.from({ length: 5 }, (_, j) => municipalities.reduce((s, r) => s + r.counts[j], 0)) } };
}
const poll = (id: string, date: string, shares: number[]): DriftPoll => ({ id, date, publication: date, fieldworkEnd: date,
  group: 'shared', surveyId: id, house: id, sample: 300, kind: 'camera', partitions: [[1], [2], [3], [4]], shares,
  source: 'synthetic', conditional: null });
const data: DriftInputs = { version: 'synthetic', sourceVersions: [], elections: [
  event('camera-2008', '2008-04-13', 'camera', 0), event('european-2014', '2014-05-25', 'european', 10),
  event('camera-2022', '2022-09-25', 'camera', 30)],
  polls: [poll('p1', '2012-01-01', [.3, .1, .2, .4]), poll('p2', '2015-01-01', [.32, .1, .2, .38])],
  audit: { sourceHistory: 'synthetic', surveys: 2, questions: 2, excluded: [], elections: [] } };

describe('hierarchical count drift inference', () => {
  it('Schur elimination agrees with the full joint Gaussian precision', () => {
    const problem = makeDriftProblem(data, '2016-01-01', 'camera', { ...BASE_DRIFT, kindEffects: true, regional: true });
    const parameters = defaultParameters(problem); updateWorkingObservations(problem);
    const counts = collapseCounts(problem, parameters), mode = zeros(problem.layout.m, 1);
    const geo = buildPollGeometry(problem, parameters, counts, mode), system = new GlobalSystem(problem, parameters, counts, geo, 0);
    const result = system.evaluate(parameters), prior = globalPrior(problem, parameters), p = geo.p, m = problem.layout.m;
    const ri = inverse(chol(geo.covariance, p), p), jt = tr(geo.jacobian, p, m);
    const a = add(add(prior.precision, counts.precision), mm(jt, mm(ri, geo.jacobian, p, p, m), m, p, m));
    const b = add(add(prior.linear, counts.linear), mv(jt, mv(ri, geo.response, p), m));
    const direct = solve(chol(a, m), b, m);
    result.mean.forEach((v, i) => expect(v).toBeCloseTo(direct[i], 7));
  });
  it('aggregate polling derivatives match finite differences without fixing national margins', () => {
    const problem = makeDriftProblem(data, '2016-01-01', 'camera', { ...BASE_DRIFT, pollLink: 'aggregate' });
    const params = defaultParameters(problem); updateWorkingObservations(problem); const counts = collapseCounts(problem, params);
    const mode = zeros(problem.layout.m, 1), centre = buildPollGeometry(problem, params, counts, mode), step = 1e-5;
    const truth = problem.polls.flatMap(p => Array.from(toContrasts(p.shares, basis(p.shares.length))));
    const columns = [0, 1, problem.layout.national(centre.times[0]), problem.layout.group('shared')];
    for (const column of columns) {
      const a = mode.slice(), b = mode.slice(); a[column] += step; b[column] -= step;
      const left = buildPollGeometry(problem, params, counts, a), right = buildPollGeometry(problem, params, counts, b);
      const jl = mv(left.jacobian, a, left.p), jr = mv(right.jacobian, b, right.p);
      for (let i = 0; i < centre.p; i++) {
        const finite = ((truth[i] + jl[i] - left.response[i]) - (truth[i] + jr[i] - right.response[i])) / (2 * step);
        expect(centre.jacobian[i * problem.layout.m + column]).toBeCloseTo(finite, 5);
      }
    }
  });
  it('geographic poll covariance agrees with independent conditional-field sampling', () => {
    const problem = makeDriftProblem(data, '2016-01-01', 'camera', { ...BASE_DRIFT, pollLink: 'aggregate' });
    const params = defaultParameters(problem); updateWorkingObservations(problem);
    const counts = collapseCounts(problem, params), geo = buildPollGeometry(problem, params, counts, zeros(problem.layout.m, 1));
    const rng = createSeededRng('geographic-covariance-test'), normal = createNormalSampler(rng);
    const factors = [...geo.cities.values()].map(c => ({ c, end: chol(c.grid.p.at(-1)!, problem.d),
      conditional: c.grid.conditional.map(p => chol(p, problem.d, 1e-12)) }));
    const n = 10000, sum = zeros(geo.p, 1), square = zeros(geo.p);
    for (let draw = 0; draw < n; draw++) {
      const f = zeros(geo.p, 1);
      for (const { c, end, conditional } of factors) {
        const states = c.grid.times.map(() => zeros(problem.d, 1)); states[states.length - 1] = mv(end, Float64Array.from({ length: problem.d }, () => normal(0, 1)), problem.d);
        for (let t = states.length - 2; t >= 0; t--) states[t] = add(mv(c.grid.gains[t], states[t + 1], problem.d), mv(conditional[t], Float64Array.from({ length: problem.d }, () => normal(0, 1)), problem.d));
        geo.times.forEach((time, qi) => {
          const state = states[c.grid.times.indexOf(time)], o = problem.polls[qi].shares.length - 1;
          for (let r = 0; r < o; r++) for (let j = 0; j < problem.d; j++) f[geo.offsets[qi] + r] += c.jacobian[(geo.offsets[qi] + r) * problem.d + j] * state[j];
        });
      }
      for (let i = 0; i < geo.p; i++) { sum[i] += f[i] / n; for (let j = 0; j < geo.p; j++) square[i * geo.p + j] += f[i] * f[j] / n; }
    }
    for (let i = 0; i < geo.p; i++) for (let j = 0; j < geo.p; j++) {
      const empirical = square[i * geo.p + j] - sum[i] * sum[j], scale = Math.sqrt(geo.geographic[i * geo.p + i] * geo.geographic[j * geo.p + j]);
      expect(Math.abs(empirical - geo.geographic[i * geo.p + j]) / scale).toBeLessThan(.06);
    }
  });
  it('is unchanged by unavailable election outcomes and conserves every forecast electorate', () => {
    const variant = { ...BASE_DRIFT, pollLink: 'aggregate' as const };
    const altered = structuredClone(data); altered.elections[2] = event('camera-2022', '2022-09-25', 'camera', 150);
    const first = fitDrift(data, '2016-01-01', 'camera', variant), second = fitDrift(altered, '2016-01-01', 'camera', variant);
    expect(first.parameters).toEqual(second.parameters); expect(first.evaluation.mean).toEqual(second.evaluation.mean);
    const predictor = makeDriftPredictor(first, '2018-03-04', 16, 'forecast-test');
    const replay = makeDriftPredictor(second, '2018-03-04', 16, 'forecast-test');
    for (let i = 0; i < 4; i++) {
      const draw = predictor.draw(i); expect(draw.invalid).toBe(0); expect(draw).toEqual(replay.draw(i));
      first.problem.forecast.forEach((r, c) => {
        const counts = Array.from(draw.output.slice(c * 5, c * 5 + 5), p => p * r.electors);
        expect(counts.reduce((a, b) => a + b, 0)).toBeCloseTo(r.electors, 8);
        expect(counts.every(n => Math.abs(n - Math.round(n)) < 1e-7)).toBe(true);
      });
    }
  });
  it('conditions aggregate polls with regional bridges and supports a seven-outcome panel', () => {
    const regional = fitDrift(data, '2016-01-01', 'camera', { ...BASE_DRIFT, pollLink: 'aggregate', regional: true, kindEffects: true });
    expect(regional.converged).toBe(true);
    const predictive = makeDriftPredictor(regional, '2018-03-04', 16, 'regional-joint-field');
    expect(predictive.diagnostics.geographicBridgeJitter).toBeLessThan(1e-7);
    expect(predictive.draw(0).invalid).toBe(0);
    const seven = structuredClone(data);
    seven.outcomes = ['non-valid', 'pd', 'm5s', 'lega', 'fdi', 'fi-pdl', 'other'];
    seven.elections.forEach(e => {
      e.municipalities.forEach(r => { const other = r.counts.pop()!; r.counts.push(Math.floor(other / 3), Math.floor(other / 3), other - 2 * Math.floor(other / 3)); r.active = [0, 1, 2, 3, 4, 5, 6]; });
      e.national.counts = Array.from({ length: 7 }, (_, j) => e.municipalities.reduce((s, r) => s + r.counts[j], 0));
    });
    seven.polls.forEach(p => p.partitions[3] = [4, 5, 6]);
    const expanded = fitDrift(seven, '2016-01-01', 'camera', BASE_DRIFT);
    expect(expanded.converged).toBe(true);
    expect(makeDriftPredictor(expanded, '2018-03-04', 16, 'seven-outcome-test').draw(0).invalid).toBe(0);
    const comparator = currentDriftComparator(expanded.problem, '2018-03-04', 'native-current-expanded');
    const score = evaluateDriftSamples(expanded.problem, seven.elections[2], 16, comparator.draw);
    expect(score.plausibility.invalid).toBe(0);
    expect(score.nationalPoliticalValidRows).toHaveLength(6);
    expect(score.matching.matched).toBe(6);
  });
});
