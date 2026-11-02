import { describe, expect, it } from 'vitest';
import { fileURLToPath } from 'node:url';
import { loadPollCalibrationCases, weightedPollPoint } from './poll-calibration';
import { evaluateEndpointUncertainty, fitEndpointErrors, ilr, inverseIlr } from './poll-uncertainty';
import { auditGeographicNationalDrift } from './poll-geography-audit';

const root = fileURLToPath(new URL('../../../', import.meta.url));
const { cases } = await loadPollCalibrationCases(root);
const points = cases.map(c => ({ id: c.id, kind: c.kind, date: c.date, cutoff: c.cutoff, point: weightedPollPoint(c), truth: c.truth }));
const study = evaluateEndpointUncertainty(points);

describe('national endpoint uncertainty research', () => {
  it('uses a reversible orthonormal log-ratio representation of positive compositions', () => {
    for (const c of points) {
      const roundtrip = inverseIlr(ilr(c.point));
      roundtrip.forEach((p, i) => expect(p).toBeCloseTo(c.point[i], 12));
      expect(roundtrip.reduce((a, b) => a + b, 0)).toBeCloseTo(1, 12);
    }
    for (const z of ilr([.25, .25, .25, .25])) expect(z).toBeCloseTo(0, 12);
    const z = [1, -2, .3], log = inverseIlr(z).map(Math.log), average = log.reduce((a, b) => a + b, 0) / 4;
    expect(log.reduce((n, x) => n + (x - average) ** 2, 0)).toBeCloseTo(z.reduce((n, x) => n + x * x, 0), 12);
    expect(() => ilr([0, .2, .3, .5])).toThrow('positive');
  });
  it('fits covariance from whole error vectors with the stated inverse-Wishart prior', () => {
    const fit = fitEndpointErrors([points[0]]), e = fit.errors[0];
    expect(fit.predictiveDf).toBe(4); expect(fit.trainingElectionIds).toHaveLength(1);
    for (let i = 0; i < 3; i++) for (let j = 0; j < 3; j++) {
      expect(fit.posteriorPsi[i][j]).toBeCloseTo((i === j ? .04 : 0) + e[i] * e[j], 12);
      expect(fit.posteriorMeanCovariance[i][j]).toBeCloseTo(fit.posteriorPsi[i][j] / 2, 12);
      expect(fit.factor[i].reduce((n, l, k) => n + l * fit.factor[j][k], 0)).toBeCloseTo(fit.posteriorPsi[i][j], 12);
    }
    expect(fitEndpointErrors([]).predictiveDf).toBe(3);
    expect(() => fitEndpointErrors([], 0)).toThrow('prior');
  });
  it('reports predictive intervals, widths, joint scores and mean/median changes without redefining the polling point', () => {
    expect(study.folds).toHaveLength(5);
    for (const f of study.folds) for (const score of f.scores) {
      const c = points.find(c => c.id === f.electionId)!;
      expect(score.rows.map(r => r.point)).toEqual(c.point);
      expect(score.fit.trainingElectionIds).not.toContain(c.id);
      for (const row of score.rows) {
        expect(row.lower).toBeGreaterThanOrEqual(0); expect(row.upper).toBeLessThanOrEqual(1);
        expect(row.lower).toBeLessThanOrEqual(row.upper); expect(row.intervalScore).toBeGreaterThanOrEqual(0);
      }
      if (score.energyScore !== null) expect(score.energyScore).toBeGreaterThanOrEqual(0);
    }
    expect(study.metrics.find(m => m.mode === 'student-t')!.mae).toBe(study.metrics[0].mae);
  });
  it('does not use a held-out or future outcome to fit its intervals or select its prior', () => {
    const changed = structuredClone(points), c = changed.find(c => c.id === 'camera-2022')!;
    c.truth[0] += .03; c.truth[3] -= .03;
    const next = evaluateEndpointUncertainty(changed);
    expect(next.folds.slice(0, 3)).toEqual(study.folds.slice(0, 3));
    for (const [i, score] of next.folds[3].scores.entries()) {
      const old = study.folds[3].scores[i];
      expect(score.fit).toEqual(old.fit);
      expect(score.rows.map(r => [r.lower, r.upper, r.predictiveMean])).toEqual(old.rows.map(r => [r.lower, r.upper, r.predictiveMean]));
    }
    expect(next.folds[4].scores[1].fit).not.toEqual(study.folds[4].scores[1].fit);
    expect(next.folds[4].scores.find(s => s.mode === 'student-t-same-kind')!.fit)
      .toEqual(study.folds[4].scores.find(s => s.mode === 'student-t-same-kind')!.fit);
  });
  it('shows why exact 90% election-block conformal bounds are vacuous with at most five calibration events', () => {
    for (const f of study.folds) {
      const s = f.scores.find(s => s.mode === 'conformal-box')!;
      expect(s.conformal!.rank).toBeGreaterThan(s.conformal!.calibrationEvents);
      expect(s.conformal!.finite).toBe(false);
      expect(s.width).toBe(1); expect(s.intervalScore).toBe(1);
      expect(s.coverage).toBe(1); expect(s.rows.every(r => r.lower === 0 && r.upper === 1)).toBe(true);
    }
  });
  it('replays deterministically and refuses unsupported horizons, boundaries or late training outcomes', () => {
    expect(evaluateEndpointUncertainty(points)).toEqual(study);
    const long = structuredClone(points); long[0].cutoff = '2013-01-01';
    expect(() => evaluateEndpointUncertainty(long)).toThrow('horizons');
    const duplicate = [points[0], points[0]];
    expect(() => evaluateEndpointUncertainty(duplicate)).toThrow('Distinct');
    const overlap = structuredClone(points); overlap[0].date = '2014-05-10'; overlap[0].cutoff = '2014-04-24';
    expect(() => evaluateEndpointUncertainty(overlap)).toThrow('not available');
  });
});

it('demonstrates national leakage from actual regional/local generation streams with no national noise', () => {
  const audit = auditGeographicNationalDrift(200);
  expect(audit.rows[0].maximumAbsoluteDeviation).toBe(0);
  for (const r of audit.rows.slice(1)) expect(r.rmsNationalDeviation).toBeGreaterThan(.005);
});
