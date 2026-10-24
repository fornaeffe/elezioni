/** Research scores use the input unit (fractions for vote shares). An election
 * remains the validation unit; variable weights do not create more elections. */
export interface GenerationScores {
  coverage50: number;
  coverage80: number;
  coverage90: number;
  coverage95: number;
  width90: number;
  intervalScore90: number;
  wis: number;
  meanMae: number;
  meanRmse: number;
  /** Predictive mean minus observation. */
  meanBias: number;
}

export interface ScalarSampleScore extends GenerationScores {
  predictiveMean: number;
  predictiveMedian: number;
  intervals: { level: number; lower: number; upper: number; covered: boolean; score: number }[];
}

const LEVELS = [0.5, 0.8, 0.9, 0.95];
const COVERAGE_KEYS = ['coverage50', 'coverage80', 'coverage90', 'coverage95'] as const;

function checkFinite(value: number, description: string) {
  if (!Number.isFinite(value)) throw new Error(`Non-finite ${description}`);
}

/** R type-7 / linear empirical quantile: index=(n-1)*p, interpolating adjacent
 * sorted samples. Inclusive interval endpoints count as covered. */
function sortedQuantile(sorted: readonly number[], probability: number): number {
  const index = (sorted.length - 1) * probability;
  const lower = Math.floor(index), fraction = index - lower;
  return sorted[lower] + fraction * (sorted[Math.min(lower + 1, sorted.length - 1)] - sorted[lower]);
}

export function summarizeScalarSamples(samples: ArrayLike<number>, truth: number): ScalarSampleScore {
  if (!samples.length) throw new Error('Scalar scores require predictive samples');
  checkFinite(truth, 'truth');
  const sorted = Array.from(samples);
  for (const value of sorted) checkFinite(value, 'predictive sample');
  const predictiveMean = sorted.reduce((a, b) => a + b / sorted.length, 0);
  sorted.sort((a, b) => a - b);
  const predictiveMedian = sortedQuantile(sorted, 0.5);
  const intervals = LEVELS.map(level => {
    const alpha = 1 - level;
    const lower = sortedQuantile(sorted, alpha / 2), upper = sortedQuantile(sorted, 1 - alpha / 2);
    const score = upper - lower + 2 / alpha * (Math.max(0, lower - truth) + Math.max(0, truth - upper));
    return { level, lower, upper, covered: lower <= truth && truth <= upper, score };
  });
  const meanBias = predictiveMean - truth;
  // Standard WIS with K=4 central intervals and the median as the 0.5-weighted
  // point term: [0.5|median-y| + sum(alpha/2 * IS_alpha)] / (K+0.5).
  const wis = (0.5 * Math.abs(predictiveMedian - truth) + intervals.reduce((total, interval) =>
    total + (1 - interval.level) / 2 * interval.score, 0)) / (intervals.length + 0.5);
  return { predictiveMean, predictiveMedian, intervals,
    coverage50: +intervals[0].covered, coverage80: +intervals[1].covered,
    coverage90: +intervals[2].covered, coverage95: +intervals[3].covered,
    width90: intervals[2].upper - intervals[2].lower, intervalScore90: intervals[2].score,
    wis, meanMae: Math.abs(meanBias), meanRmse: Math.abs(meanBias), meanBias };
}

/** Aggregates variable scores with normalized nonnegative weights. RMSE is
 * recomputed from squared errors of predictive means, not averaged RMSEs. */
export function aggregateScores(scores: readonly GenerationScores[], weights?: readonly number[]): GenerationScores {
  if (!scores.length) throw new Error('Aggregate scores require variables');
  if (weights && weights.length !== scores.length) throw new Error('Score weights have the wrong dimension');
  const usedWeights = weights ?? scores.map(() => 1);
  if (usedWeights.some(weight => !Number.isFinite(weight) || weight < 0)) throw new Error('Score weights must be finite and nonnegative');
  const total = usedWeights.reduce((a, b) => a + b, 0);
  if (!Number.isFinite(total) || total <= 0) throw new Error('Score weights must have a positive finite sum');
  const result: GenerationScores = { coverage50: 0, coverage80: 0, coverage90: 0, coverage95: 0,
    width90: 0, intervalScore90: 0, wis: 0, meanMae: 0, meanRmse: 0, meanBias: 0 };
  const ordinaryKeys = [...COVERAGE_KEYS, 'width90', 'intervalScore90', 'wis', 'meanMae', 'meanBias'] as const;
  scores.forEach((score, index) => {
    const weight = usedWeights[index] / total;
    for (const key of ordinaryKeys) {
      checkFinite(score[key], `score ${key}`);
      result[key] += weight * score[key];
    }
    checkFinite(score.meanRmse, 'score meanRmse');
    result.meanRmse += weight * score.meanRmse ** 2;
  });
  result.meanRmse = Math.sqrt(result.meanRmse);
  return result;
}

/** Samples are draw-major, with the same flattened variable order as truths.
 * Call summarizeScalarSamples separately to retain national interval audits. */
export function scoreGeneration(
  samples: readonly ArrayLike<number>[], truths: readonly number[], weights?: readonly number[]
): GenerationScores {
  if (!samples.length || !truths.length) throw new Error('Generation scores require samples and truths');
  if (samples.some(sample => sample.length !== truths.length)) throw new Error('Predictive sample dimension does not match truths');
  const scores = truths.map((truth, index) => summarizeScalarSamples(samples.map(sample => sample[index]), truth));
  return aggregateScores(scores, weights);
}

/** O(draws * dimension) unbiased Monte Carlo energy-score estimator. Split the
 * draws into two independent halves and pair corresponding vectors, retaining
 * only complete pairs; an odd final draw is unused. Every pair contributes
 * (||X-y||+||X'-y||-||X-X'||)/2, hence nonnegativity follows from the triangle
 * inequality. Unlike the quadratic empirical-ensemble score this estimates the
 * underlying predictive distribution without a finite-ensemble bias. One draw
 * is treated as a degenerate distribution, giving its distance to the truth. */
export function scoreVectorEnergy(samples: readonly ArrayLike<number>[], truth: readonly number[]): number {
  if (!samples.length || !truth.length) throw new Error('Energy score requires samples and truth');
  for (const value of truth) checkFinite(value, 'energy truth');
  for (const sample of samples) {
    if (sample.length !== truth.length) throw new Error('Energy sample dimension does not match truth');
    for (let index = 0; index < sample.length; index++) checkFinite(sample[index], 'energy sample');
  }
  const distance = (a: ArrayLike<number>, b: ArrayLike<number>) => {
    let squared = 0;
    for (let index = 0; index < truth.length; index++) squared += (a[index] - b[index]) ** 2;
    return Math.sqrt(squared);
  };
  if (samples.length === 1) return distance(samples[0], truth);
  const pairs = Math.floor(samples.length / 2);
  let score = 0;
  for (let index = 0; index < pairs; index++) {
    const a = samples[index], b = samples[index + pairs];
    score += (distance(a, truth) + distance(b, truth) - distance(a, b)) / (2 * pairs);
  }
  return Math.max(0, score);
}
