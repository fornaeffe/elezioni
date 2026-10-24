import { describe, expect, it } from 'vitest';
import { eye, zeros, add, mm, mv, tr, chol, solve, inverse, basis, toContrasts, toScores, softmax,
  initialCountPosterior, countMessage, innovationCov, ouStep, dot } from './drift-math';
import { LocalGaussian } from './drift-gaussian';
import { collapsePollRows, availableInputs, type DriftInputs } from './drift-data';

describe('elector-wide drift arithmetic and observations', () => {
  it('roundtrips positive outcome compositions and preserves contrast variance', () => {
    const h = basis(5), p = [.3, .2, .1, .15, .25];
    const out = softmax(toScores(toContrasts(p, h), h));
    out.forEach((v, i) => expect(v).toBeCloseTo(p[i], 12));
    const q = innovationCov([.2, .2, .2, .2, .2], h);
    q.forEach((v, i) => expect(v).toBeCloseTo(i % 5 === 0 ? .04 : 0, 12));
  });
  it('uses the multinomial likelihood for zero counts and leaves absent categories unobserved', () => {
    const h = basis(5), prior = toContrasts([.3, .2, .1, .15, .25], h);
    const full = initialCountPosterior([30, 40, 0, 10, 20], [0, 1, 2, 3, 4], prior, h);
    expect(full.mean.every(Number.isFinite)).toBe(true);
    expect(full.change).toBeLessThan(1e-6);
    const absent = countMessage([30, 40, 0, 10, 20], [0, 1, 3, 4], full.mean, h);
    expect(absent.y.length).toBe(3);
    const q = chol(full.covariance, 4);
    expect(q.every(Number.isFinite)).toBe(true);
  });
  it('multinomial quadratic derivatives agree with finite differences', () => {
    const h = basis(5), state = Float64Array.from([.1, -.2, .4, .3]), counts = [30, 40, 0, 10, 20];
    const message = countMessage(counts, [0, 1, 2, 3, 4], state, h), ri = inverse(chol(message.r, 4), 4);
    const gradient = mv(ri, add(message.y, mv(message.a, state, 4), -1), 4), step = 1e-5;
    const loglik = (x: Float64Array) => {
      const p = softmax(toScores(x, h)); return counts.reduce((s, n, i) => s + n * Math.log(p[i]), 0);
    };
    for (let i = 0; i < 4; i++) {
      const a = state.slice(), b = state.slice(); a[i] += step; b[i] -= step;
      expect(gradient[i]).toBeCloseTo((loglik(a) - loglik(b)) / (2 * step), 6);
    }
  });
  it('local Markov elimination agrees with a direct Gaussian conditioning calculation', () => {
    const h = basis(3), theta = zeros(2, 1), a = eye(2), r = eye(2, .1), y = Float64Array.from([.4, -.2]);
    const message = { y, r, a, active: [0, 1, 2], logLikelihood: 0, gradientNorm: 0 };
    const gp = new LocalGaussian(2, 2, 0, eye(2, .2), eye(2, .3), 0, [{ time: 1, message, design: eye(2) }], theta);
    const query = gp.query(1), g = Float64Array.from([.1, .1]), actual = add(query.u, mv(query.f, g, 2));
    expect(actual[0]).toBeCloseTo((.4 - .1) * .5 / .6, 12);
    expect(actual[1]).toBeCloseTo((-.2 - .1) * .5 / .6, 12);
    expect(query.p[0]).toBeCloseTo(.5 * .1 / .6, 12);
    const middle = gp.query(.5);
    expect(middle.p[0]).toBeCloseTo(.35 - .35 ** 2 / .6, 12);
    expect(gp.precision[0]).toBeCloseTo(1 / .6, 12);
    void h;
  });
  it('conditions polls on valid votes with valid-vote geographic weights', () => {
    const a = [.5, .4, .1], b = [.2, .2, .6], e = [100, 200];
    const direct = (e[0] * a[1] + e[1] * b[1]) / (e[0] * (1 - a[0]) + e[1] * (1 - b[0]));
    const weighted = (e[0] * (1 - a[0]) * a[1] / (1 - a[0]) + e[1] * (1 - b[0]) * b[1] / (1 - b[0])) / (e[0] * (1 - a[0]) + e[1] * (1 - b[0]));
    expect(direct).toBeCloseTo(weighted, 14);
    expect(direct).not.toBeCloseTo((a[1] / (1 - a[0]) + b[1] / (1 - b[0])) / 2, 3);
  });
  it('coarsens grouped poll answers without guessing unreported political shares', () => {
    const p = collapsePollRows([{ entities: ['pd', 'avs'], share: .3 }, { entities: ['m5s'], share: .15 },
      { entities: ['lega'], share: .1 }, { entities: ['fi'], share: .45 }]);
    expect(p.partitions).toContainEqual([1, 4]);
    expect(p.shares.reduce((a, b) => a + b, 0)).toBeCloseTo(1, 14);
    expect(p.partitions.flat()).not.toContain(0);
  });
  it('excludes unavailable outcomes and publications while using fieldwork time', () => {
    const input = { elections: [{ date: '2018-03-04' }, { date: '2022-09-25' }], polls: [
      { date: '2020-01-01', publication: '2020-01-05', fieldworkEnd: '2020-01-03' },
      { date: '2019-01-01', publication: '2021-01-01', fieldworkEnd: '2019-01-03' }
    ] } as DriftInputs;
    const selected = availableInputs(input, '2020-02-01');
    expect(selected.elections).toHaveLength(1); expect(selected.polls).toHaveLength(1);
    expect(ouStep(2, 0).varianceTime).toBe(2); expect(ouStep(2, .4).varianceTime).toBeLessThan(2);
  });
});
