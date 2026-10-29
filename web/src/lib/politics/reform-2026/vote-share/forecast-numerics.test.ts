import { describe, it, expect } from 'vitest';
import { fitRandom } from './random';
import { betaDraw, forecastRandom, normalCdf } from './forecast-random';
import { betaTransition } from './transition';
import { calibrateReferences } from './reference-family';
import { tensorReferenceMoments as tensorMoments } from '../../../../../tests/fixtures/fast-reference-oracle';
import { projectMargins } from './projection';
import { nonValidIntervention, politicalIntervention, removeAbsent } from './interventions';
describe('FV-04 reduced computability and independent numerical gates', () => {
  it('samples Beta moments and normal copula marginals without extra variance', () => {
    expect(normalCdf(1)).toBeCloseTo(0.8413447460685429, 14);
    expect(normalCdf(-3)).toBeCloseTo(0.0013498980316301, 14);
    const rng = fitRandom('fv04-beta', 'moments');
    let total = 0,
      square = 0;
    const n = 20000;
    for (let i = 0; i < n; i++) {
      const x = betaDraw(2, 3, rng);
      total += x;
      square += x * x;
    }
    expect(total / n).toBeCloseTo(0.4, 2);
    expect(square / n - (total / n) ** 2).toBeCloseTo(0.04, 2);
    for (const rho of [0.5, 0.85, 0.98]) {
      const c = forecastRandom('coupling', 1, 'coordinate', 'camera', rho),
        s = forecastRandom('coupling', 1, 'coordinate', 'senato', rho);
      let cm = 0,
        sm = 0,
        cv = 0,
        sv = 0,
        covariance = 0;
      for (let i = 0; i < n; i++) {
        const a = c.normal(),
          b = s.normal();
        cm += a;
        sm += b;
        cv += a * a;
        sv += b * b;
        covariance += a * b;
      }
      expect(Math.abs(cm / n)).toBeLessThan(0.035);
      expect(Math.abs(sm / n)).toBeLessThan(0.035);
      expect(Math.abs(cv / n - 1)).toBeLessThan(0.04);
      expect(Math.abs(sv / n - 1)).toBeLessThan(0.04);
      expect(Math.abs(covariance / n - rho)).toBeLessThan(0.04);
    }
  });
  it('preserves zero-horizon/scales/endpoints and forecast means', () => {
    const rng = fitRandom('fv04-transition', 'moment');
    expect([...betaTransition([0, 0.4, 0.6], [1, 1, 1], 0, rng)]).toEqual([0, 0.4, 0.6]);
    expect([...betaTransition([0.2, 0.8], [0, 0], 5, rng)]).toEqual([0.2, 0.8]);
    expect([...betaTransition([0, 1, 0], [1, 1, 1], 5, rng)]).toEqual([0, 1, 0]);
    const initial = [0, 0.3, 0.25, 0.2, 0.1, 0.08, 0.07],
      total = initial.map(() => 0),
      n = 16000;
    for (let i = 0; i < n; i++) {
      const p = betaTransition(
        initial,
        initial.map(() => 0.4),
        2,
        rng
      );
      expect(p[0]).toBe(0);
      p.forEach((x, j) => (total[j] += x));
    }
    expect(Math.max(...total.map((x, j) => Math.abs(x / n - initial[j])))).toBeLessThan(0.004);
  });
  it.each([
    { means: [0.3], sd: [2], free: true },
    { means: [0.3, 0.4], sd: [2, 3], free: true },
    { means: [0.4, 0.6], sd: [2, 2], free: false },
    { means: [0.2, 0.3, 0.5], sd: [2, 3, 4], free: false }
  ])(
    'calibrates $means via Laplace factors and checks independent tensor integration',
    ({ means, sd, free }) => {
      const f = calibrateReferences(
        means.map((mean, i) => ({ id: 'g' + i, members: ['p' + i], mean, sdPercentagePoints: sd[i] })),
        free
      );
      const a = tensorMoments(f.mu, f.sigma, 40),
        b = tensorMoments(f.mu, f.sigma, 56);
      means.forEach((m, i) => {
        expect(Math.abs(b.mean[i] - m)).toBeLessThan(1e-6);
        expect(Math.abs(b.sd[i] - sd[i] / 100)).toBeLessThan(1e-6);
        expect(Math.abs(a.mean[i] - b.mean[i])).toBeLessThan(1e-7);
        expect(Math.abs(a.sd[i] - b.sd[i])).toBeLessThan(1e-7);
      });
      expect(f.integrationError).toBeLessThan(1e-7);
    }
  );
  it('retains exact groups, normalizes full centres and rejects unsupported moments', () => {
    const f = calibrateReferences(
      [
        { id: 'A', members: ['a'], mean: 0.2, sdPercentagePoints: 0 },
        { id: 'B', members: ['b'], mean: 0.3, sdPercentagePoints: 2 }
      ],
      true
    );
    expect(f.upper).toBe(0.8);
    expect(f.momentError).toBeLessThan(1e-6);
    const full = calibrateReferences(
      [
        { id: 'A', members: ['a'], mean: 0.2, sdPercentagePoints: 0 },
        { id: 'B', members: ['b'], mean: 0.3, sdPercentagePoints: 0 }
      ],
      false,
      true
    );
    expect(full.groups.map((g) => g.mean)).toEqual([0.4, 0.6]);
    expect(full.normalization).toBe(0.5);
    expect(() =>
      calibrateReferences(
        [
          { id: 'A', members: ['a'], mean: 0.4, sdPercentagePoints: 2 },
          { id: 'B', members: ['b'], mean: 0.6, sdPercentagePoints: 3 }
        ],
        false
      )
    ).toThrow('REFERENCE_MOMENTS_UNSUPPORTED');
    expect(() =>
      calibrateReferences(
        [
          { id: 'A', members: ['a', 'b'], mean: 0.3, sdPercentagePoints: 0 },
          { id: 'B', members: ['b'], mean: 0.4, sdPercentagePoints: 0 }
        ],
        true
      )
    ).toThrow('REFERENCE_OVERLAP');
  });
  it('projects global margins identically across chunk sizes, with joint capacities and exact boundary faces', () => {
    const source = (i: number) => ({ weight: i + 1, scores: [0, i / 10, -i / 10] });
    const a = projectMargins([0.2, 0.3, 0.5], 20, source, { chunkSize: 1 }),
      b = projectMargins([0.2, 0.3, 0.5], 20, source, { chunkSize: 7 });
    expect(a.margins).toEqual(b.margins);
    expect(a.residual).toBeLessThanOrEqual(1e-9);
    expect(() =>
      projectMargins([0.3, 0.3, 0.4], 2, (i) => ({
        weight: 1,
        scores: i ? [-Infinity, -Infinity, 0] : [0, 0, 0]
      }))
    ).toThrow('REFERENCE_CAPACITY');
    const boundary = projectMargins([0.5, 0.5], 2, (i) => ({
      weight: 1,
      scores: i ? [-Infinity, 0] : [0, 0]
    }));
    expect([...boundary.probabilities(0)]).toEqual([1, 0]);
    expect([...boundary.probabilities(1)]).toEqual([0, 1]);
  });
  it('keeps turnout fixed for political references, shadows national references locally and separates non-valid interventions', () => {
    const rows = [Float64Array.from([0.3, 0.14, 0.21, 0.35])],
      backup = rows.map((r) => r.slice());
    politicalIntervention(rows, [100], [[1]], [0.4], (i, j) => backup[i][j]);
    [0.3, 0.28, 0.1575, 0.2625].forEach((x, j) => expect(rows[0][j]).toBeCloseTo(x, 12));
    nonValidIntervention(rows, [100], 0.4, (i, j) => backup[i][j]);
    expect(rows[0][0]).toBeCloseTo(0.4, 12);
    expect(rows[0][1] / 0.6).toBeCloseTo(0.4, 12);
    const local = [Float64Array.from([0.3, 0.28, 0.42]), Float64Array.from([0.3, 0.28, 0.42])];
    politicalIntervention([local[0]], [100], [[1]], [0.8], (_, j) => backup[0][j]);
    expect((local[0][1] + local[1][1]) / 1.4).toBeCloseTo(0.6, 12);
    expect(local[0][0]).toBe(0.3);
    expect([...removeAbsent([0.2, 0.3, 0.5], [true, false, true])]).toEqual([0.2 / 0.7, 0, 0.5 / 0.7]);
  });
  it('preserves each Beta forecast marginal under all three copula assumptions, including adaptive Gamma rejection', () => {
    const n = 12000,
      initial = [0.3, 0.4, 0.3],
      stats = (ch: 'camera' | 'senato', rho: number) => {
        const rng = forecastRandom('fv04-marginals', 1, 'beta', ch, rho),
          values = new Float64Array(n);
        let total = 0;
        for (let i = 0; i < n; i++) {
          const p = betaTransition(initial, [0.4, 0.4, 0.4], 1, rng);
          values[i] = p[0];
          total += p[0];
        }
        const mean = total / n;
        let v = 0,
          fourth = 0;
        for (const x of values) {
          v += (x - mean) ** 2;
          fourth += (x - mean) ** 4;
        }
        v /= n;
        return {
          mean,
          variance: v,
          meanMcse: Math.sqrt(v / n),
          varianceMcse: Math.sqrt((fourth / n - v * v) / n)
        };
      },
      base = stats('camera', 0);
    for (const rho of [0.5, 0.85, 0.98]) {
      const paired = stats('senato', rho);
      expect(Math.abs(paired.mean - base.mean)).toBeLessThan(4 * Math.hypot(paired.meanMcse, base.meanMcse));
      expect(Math.abs(paired.variance - base.variance)).toBeLessThan(
        4 * Math.hypot(paired.varianceMcse, base.varianceMcse)
      );
    }
  });
});
