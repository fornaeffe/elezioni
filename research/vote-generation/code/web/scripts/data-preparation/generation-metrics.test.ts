import { describe, expect, it } from 'vitest';
import { aggregateScores, scoreGeneration, scoreVectorEnergy, summarizeScalarSamples } from './generation-metrics';

describe('vote-generation research metrics', () => {
  it('scores an exact deterministic forecast with zero loss and complete coverage', () => {
    const score = summarizeScalarSamples([0.3, 0.3, 0.3], 0.3);
    expect(score.predictiveMean).toBeCloseTo(0.3, 14);
    expect(score.predictiveMedian).toBe(0.3);
    expect(score.intervals.every(interval => interval.lower === 0.3 && interval.upper === 0.3 && interval.covered)).toBe(true);
    expect(score.coverage50).toBe(1); expect(score.coverage95).toBe(1);
    expect(score.width90).toBe(0); expect(score.intervalScore90).toBe(0); expect(score.wis).toBe(0);
    expect(score.meanMae).toBe(0); expect(score.meanRmse).toBe(0); expect(score.meanBias).toBe(0);
  });

  it('uses interpolated type-7 quantiles and inclusive coverage', () => {
    const score = summarizeScalarSamples([4, 1, 3, 0, 2], 1);
    expect(score.predictiveMean).toBe(2); expect(score.predictiveMedian).toBe(2);
    expect(score.intervals.map(interval => [interval.lower, interval.upper])).toEqual([
      [1, 3], [expect.closeTo(0.4), 3.6], [expect.closeTo(0.2), 3.8], [expect.closeTo(0.1), 3.9]
    ]);
    expect(score.coverage50).toBe(1);
    expect(score.width90).toBeCloseTo(3.6, 12);
    expect(score.intervalScore90).toBeCloseTo(3.6, 12);
    expect(score.wis).toBeCloseTo((0.5 + 0.25 * 2 + 0.1 * 3.2 + 0.05 * 3.6 + 0.025 * 3.8) / 4.5, 12);
  });

  it('penalizes both missed tails and uses the predictive mean for point errors', () => {
    const low = summarizeScalarSamples([0, 1, 2, 3, 4], -1);
    const high = summarizeScalarSamples([0, 1, 2, 3, 4], 5);
    expect(low.coverage90).toBe(0); expect(high.coverage90).toBe(0);
    expect(low.intervalScore90).toBeCloseTo(3.6 + 20 * 1.2, 12);
    expect(high.intervalScore90).toBeCloseTo(low.intervalScore90, 12);
    expect(low.meanBias).toBe(3); expect(high.meanBias).toBe(-3);
    const asymmetric = summarizeScalarSamples([0, 0, 6], 0);
    expect(asymmetric.predictiveMedian).toBe(0); expect(asymmetric.predictiveMean).toBe(2);
    expect(asymmetric.meanMae).toBe(2);
  });

  it('normalizes variable weights and computes RMSE from squared mean errors', () => {
    const samples = [Float64Array.from([0, 0]), Float64Array.from([0, 0])];
    const score = scoreGeneration(samples, [1, -3], [3, 1]);
    expect(score.meanMae).toBe(1.5); expect(score.meanBias).toBe(0);
    expect(score.meanRmse).toBeCloseTo(Math.sqrt(3), 12);
    expect(score.coverage90).toBe(0);
    expect(score.intervalScore90).toBeCloseTo(30, 12);
    expect(score.wis).toBe(1.5);
    const onlyFirst = scoreGeneration(samples, [1, -3], [1, 0]);
    expect(onlyFirst).toEqual(aggregateScores([summarizeScalarSamples([0, 0], 1)]));
    expect(scoreGeneration(samples.map(sample => Float32Array.from(sample)), [1, -3], [3, 1])).toEqual(score);
  });

  it('uses whole-vector paired distances and ignores an unmatched odd draw', () => {
    expect(scoreVectorEnergy([[0, 0, 0, 0]], [3, 4, 0, 0])).toBe(5);
    expect(scoreVectorEnergy([[0, 0, 0, 0], [0, 0, 0, 0]], [3, 4, 0, 0])).toBe(5);
    expect(scoreVectorEnergy([[0, 0], [2, 0]], [3, 0])).toBe(1);
    expect(scoreVectorEnergy([[0, 0], [2, 0], [999, 999]], [3, 0])).toBe(1);
    expect(scoreVectorEnergy([[3, 4], [3, 4]], [3, 4])).toBe(0);
    const samples = [[0, 0], [1, 1], [2, 0], [0, 2]];
    const expected = ((0 + 2 - 2) + (Math.sqrt(2) + 2 - Math.sqrt(2))) / 4;
    expect(scoreVectorEnergy(samples, [0, 0])).toBeCloseTo(expected, 12);
  });

  it('rejects empty, nonfinite or mismatched data and invalid weights', () => {
    expect(() => summarizeScalarSamples([], 0)).toThrow('samples');
    expect(() => summarizeScalarSamples([NaN], 0)).toThrow('Non-finite');
    expect(() => summarizeScalarSamples([0], Infinity)).toThrow('Non-finite');
    expect(() => scoreGeneration([[0]], [])).toThrow('truths');
    expect(() => scoreGeneration([[0]], [0, 1])).toThrow('dimension');
    expect(() => scoreGeneration([[0]], [0], [1, 2])).toThrow('dimension');
    expect(() => scoreGeneration([[0]], [0], [-1])).toThrow('nonnegative');
    expect(() => scoreGeneration([[0]], [0], [Infinity])).toThrow('finite');
    expect(() => scoreGeneration([[0]], [0], [0])).toThrow('positive');
    expect(() => scoreVectorEnergy([], [0])).toThrow('samples');
    expect(() => scoreVectorEnergy([[0]], [0, 0])).toThrow('dimension');
    expect(() => scoreVectorEnergy([[NaN]], [0])).toThrow('Non-finite');
  });
});
