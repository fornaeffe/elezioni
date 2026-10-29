import { describe, expect, it } from 'vitest';
import oracle from '../../../../../tests/fixtures/fast-share-reference.generated.json';
import { reducedShareInput } from '../../../../../tests/fixtures/fast-share-input';
import { runNumericalGate } from '../../../../../tests/fixtures/fast-share-gates';
import { buildParticipationTarget, buildPoliticalTarget, fitReference, normalizeSource } from './information';
import { diagnose, normalQuantile } from './diagnostics';
import { gaussianMode, sampleGaussian } from './gaussian';
import { fitProfiles, type ProfileSource } from './profiles';
import { fitRandom } from './random';
import { factorSpd, solveSpd } from './linalg';

function closeArrays(actual: ArrayLike<number>, expected: ArrayLike<number>, tolerance = 1e-11) {
  expect(actual.length).toBe(expected.length);
  const scale = Math.max(1, ...Array.from(expected, Math.abs));
  for (let i = 0; i < actual.length; i++) expect(Math.abs(actual[i] - expected[i]) / scale).toBeLessThan(tolerance);
}
describe('FV-02 independent bounded Gaussian acceptance', () => {
  for (const fixture of oracle.quadrature) for (let seed = 0; seed < 3; seed++) {
    it(fixture.id + ' agrees with refined independent quadrature, seed ' + seed, () => {
      const result = runNumericalGate(fixture, 'fv02-numeric-' + seed);
      expect(result.comparison.passed, JSON.stringify(result.comparison)).toBe(true);
      expect(result.samples.chains).toBe(4);
    });
  }
  it('matches the independent rank/folded R-hat and FFT bulk/tail ESS including ties', () => {
    const chains = oracle.diagnostics.chains, actual = diagnose(Float64Array.from(chains.flat(2)), chains.length, chains[0].length, chains[0][0].length);
    expect(actual.maxRhat).toBeCloseTo(oracle.diagnostics.expected.maxRhat, 8);
    closeArrays(actual.coordinates.map(c => c.bulkEss), oracle.diagnostics.expected.essByCoordinate, 1e-7);
    closeArrays(actual.coordinates.map(c => c.tailEss), oracle.diagnostics.expected.tailEssByCoordinate, 1e-7);
    expect(actual.passed).toBe(oracle.diagnostics.expected.passed);
  });
  it('rejects stuck/free chains and nonfinite states while labelling proven constants', () => {
    const stuck = new Float64Array(4 * 64 * 2);
    expect(diagnose(stuck, 4, 64, 2).passed).toBe(false);
    expect(diagnose(stuck, 4, 64, 2, [0, 1]).passed).toBe(true);
    stuck[0] = NaN; expect(() => diagnose(stuck, 4, 64, 2)).toThrow();
  });
  it('matches inverse-normal reference values', () => {
    for (const r of oracle.normalQuantiles) expect(Math.abs(normalQuantile(r.p) - r.value)).toBeLessThan(3e-8);
  });
  it('samples far tails, reflections and narrow intervals without CDF subtraction', () => {
    for (const r of oracle.truncatedNormals) {
      const rng = fitRandom('fv02-tails', JSON.stringify([r.a, r.b])), n = 50000;
      let mean = 0, square = 0;
      for (let i = 0; i < n; i++) {
        const z = rng.truncatedNormal(r.a, r.b);
        expect(z >= r.a && z <= r.b).toBe(true);
        mean += z / n; square += (z - r.mean) ** 2 / n;
      }
      expect(Math.abs(mean - r.mean)).toBeLessThan(4 * Math.sqrt(r.variance / n) + 1e-10);
      expect(Math.abs(square - r.variance)).toBeLessThan(r.variance * 0.04 + 1e-10);
    }
    const rng = fitRandom('fv02-tails', 'extreme');
    for (let i = 0; i < 100; i++) {
      const z = rng.truncatedNormal(1000, 1000.000001);
      expect(z >= 1000 && z <= 1000.000001).toBe(true);
    }
    expect(() => rng.truncatedNormal(1, 1)).toThrow();
  });
  it('solves true boundary QP modes, rejects invalid precision, and bounds allocations', () => {
    const t = { components: 3, blocks: 1, precision: Float64Array.from([900, 0, 0, 0, 200, 0, 0, 0, 200]), linear: Float64Array.from([-90, 140, 80]) };
    const result = gaussianMode(t);
    closeArrays(result.point, [0, 0.65, 0.35]);
    expect(result.equalityResidual).toBeLessThan(1e-12); expect(result.scaledKktResidual).toBeLessThan(1e-8);
    expect(() => sampleGaussian(t, { seed: 'x', maximumWorkspaceBytes: 10 })).toThrow('FIT_WORKSPACE_LIMIT');
    expect(() => gaussianMode({ ...t, precision: new Float64Array(9) })).toThrow();
    const f = factorSpd(Float64Array.from([1e12, 1e3, 1e3, 2]), 2);
    closeArrays(solveSpd(f, [1e12 + 1000, 1002]), [1, 1]);
  });
  it('has deterministic independent chain/entity streams and strictly interior uniforms', () => {
    const a = fitRandom('seed', 'a'), b = fitRandom('seed', 'a'), other = fitRandom('seed', 'b');
    let different = false;
    for (let i = 0; i < 10000; i++) { const x = a.uniform(); expect(x).toBe(b.uniform()); expect(x > 0 && x < 1).toBe(true); different ||= x !== other.uniform(); }
    expect(different).toBe(true);
  });
});
describe('FV-02 grouped country information and bounded covariance', () => {
  it('matches independent dense Python construction, mode, scales and participation', () => {
    const input = reducedShareInput(); expect(input).toEqual(oracle.input);
    const result = buildPoliticalTarget(input.national, { solver: 'dense' }), expected = oracle.national;
    closeArrays(result.target.precision, expected.precision.flat());
    closeArrays(result.target.linear, expected.linear);
    closeArrays(result.mode.point, expected.mode);
    expect(result.audit.scales.political).toBeCloseTo(expected.sigmaPolitical, 12);
    expect(result.audit.scales.participation).toBeCloseTo(expected.sigmaParticipation, 12);
    expect(result.audit.electionDiscrepancy).toBeCloseTo(expected.electionDiscrepancy, 12);
    closeArrays(result.audit.historicalScaleBounds, expected.historicalScaleBounds);
    const p = buildParticipationTarget(input.national);
    closeArrays(p.target.precision, expected.participationPrecision.flat()); closeArrays(p.target.linear, expected.participationLinear);
    closeArrays(p.mode.point, expected.participationMode);
  });
  it('matrix-free exact covariance solves agree with the dense reference', () => {
    const input = reducedShareInput().national;
    const dense = buildPoliticalTarget(input, { solver: 'dense' });
    const free = buildPoliticalTarget(input, { solver: 'matrix-free' });
    closeArrays(free.target.precision, dense.target.precision, 1e-9);
    closeArrays(free.mode.point, dense.mode.point, 1e-9);
    expect(free.audit.iterations.every(i => i.maximumRelativeResidual <= 1e-11)).toBe(true);
    expect(free.audit.iterations.some(i => i.iterations > 0)).toBe(true);
    expect(() => buildPoliticalTarget(input, { maximumWorkspaceBytes: 10 })).toThrow('FIT_WORKSPACE_LIMIT');
    expect(() => buildPoliticalTarget(input, { solver: 'matrix-free', maximumIterations: 1 })).toThrow('COVARIANCE_SOLVE_FAILED');
    expect(() => buildPoliticalTarget(input, { relativeTolerance: 0.01 })).toThrow('cannot weaken');
  });
  it('keeps shared poll error, cutoff roles and group/record ordering explicit', () => {
    const input = reducedShareInput().national, before = JSON.stringify(input), a = buildPoliticalTarget(input);
    const reordered = structuredClone(input);
    reordered.components.reverse(); reordered.elections.reverse(); reordered.polls.reverse();
    for (const e of reordered.elections) { e.groups.reverse(); e.counts = [e.counts[0], ...e.counts.slice(1).reverse()]; }
    for (const p of reordered.polls) { p.groups.reverse(); p.shares.reverse(); }
    closeArrays(buildPoliticalTarget(reordered).target.precision, a.target.precision, 1e-14);
    expect(JSON.stringify(input)).toBe(before);
    const independent = structuredClone(input); independent.polls[2].errorGroup = 'different-group';
    expect([...buildPoliticalTarget(independent).target.precision]).not.toEqual([...a.target.precision]);
    const future = structuredClone(input); future.polls[0].availableDate = '2017-01-01';
    expect(() => normalizeSource(future)).toThrow('availability');
    const zero = structuredClone(input); zero.polls[0].shares = [0, 0.5, 0.5];
    expect(() => normalizeSource(zero)).toThrow('printed zeros');
    const duplicate = structuredClone(input); duplicate.polls[1].surveyId = duplicate.polls[0].id;
    expect(() => normalizeSource(duplicate)).toThrow('Duplicate survey');
  });
  it('fits both C/E political and non-valid reference targets with accepted draws', () => {
    const result = fitReference(reducedShareInput().national, 'fv02-reference');
    expect(result.political.diagnostics.passed).toBe(true); expect(result.participation.diagnostics.passed).toBe(true);
    expect(result.contexts).toEqual(['camera', 'european']);
    expect(result.political.dimensions).toBe(8); expect(result.participation.dimensions).toBe(4);
  });
});
describe('FV-02 empirical profiles without future counts', () => {
  it('matches the independent Python profiles, filtering scales and common future scale', () => {
    const input = reducedShareInput().profiles, before = JSON.stringify(input), result = fitProfiles(input), expected = oracle.profiles;
    expect(result.municipalityIds).toEqual(expected.municipalityIds);
    closeArrays(result.means, expected.means.flat()); closeArrays(result.profileSd, expected.profileSd.flat());
    closeArrays(result.filterScales, expected.filterScales);
    closeArrays(result.futureScales, expected.filterScales.map(() => Math.sqrt(expected.commonLocalVariance)));
    expect(result.futureScales.some((x, i) => Math.abs(x - result.filterScales[i]) > 1e-6)).toBe(true);
    expect(result.audit.unmatchedGroups).toBe(2);
    expect(JSON.stringify(input)).toBe(before);
    const j = 5, pooled = result.municipalityIds.indexOf('joint') * j, missing = result.municipalityIds.indexOf('fresh') * j;
    expect(result.fallback[pooled + 1]).toBe(1); expect(result.fallback[missing + 1]).toBe(2);
    expect(result.profileSd[missing + 1]).toBeCloseTo(Math.sqrt(1 + 0.45 ** 2), 12);
    expect(result.fallback[result.municipalityIds.indexOf('n2') * j + 1]).toBe(0);
    expect(result.means.every(Number.isFinite)).toBe(true);
    expect(() => fitProfiles(input, 10)).toThrow('FIT_WORKSPACE_LIMIT');
  });
  it('has independently worked known-zero, context-noise and reference-date propagation', () => {
    const source: ProfileSource = { components: ['a', 'b'], referenceDate: '2016-01-01', targetContext: 'camera',
      nationalElections: [{ id: 'e', date: '2016-01-01', context: 'camera', electors: 1000, groups: [['a'], ['b']], counts: [200, 400, 400] }],
      elections: [{ id: 'e', date: '2016-01-01', context: 'camera', rows: [{ id: 'x', region: 'r', electors: 100, nonValid: 20, groups: [['a'], ['b']], counts: [0, 80] }] }],
      targets: [{ id: 'x', region: 'r', electors: 200 }] };
    const a = fitProfiles(source), noise = 1 / 0.5;
    expect(a.means[1]).toBeCloseTo(Math.log((0.5 / 81) / 0.5), 12);
    expect(a.profileSd[1] ** 2).toBeCloseTo(noise / (1 + noise) + 0.01, 12);
    const later = fitProfiles({ ...source, referenceDate: '2017-01-01' });
    expect(later.profileSd[1] ** 2 - a.profileSd[1] ** 2).toBeCloseTo(0.25 ** 2 * 366 / 365.25, 12);
    const other = fitProfiles({ ...source, targetContext: 'european' });
    expect(other.profileSd[1] ** 2).toBeCloseTo((noise + 0.15 ** 2) / (1 + noise + 0.15 ** 2) + 0.01, 12);
    const missing = fitProfiles({ ...source, elections: [], nationalElections: [] });
    expect([...missing.means]).toEqual([0, 0, 0]); expect([...missing.fallback]).toEqual([2, 2, 2]);
    expect(missing.profileSd[1]).toBeCloseTo(Math.sqrt(1 + 0.45 ** 2), 12);
  });
});
