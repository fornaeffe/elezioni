import { fitGaussian } from '../../src/lib/politics/reform-2026/vote-share/gaussian';
import { fitRandom } from '../../src/lib/politics/reform-2026/vote-share/random';
import { quantile } from '../../src/lib/politics/reform-2026/vote-share/diagnostics';
import type { GaussianSamples, GaussianTarget } from '../../src/lib/politics/reform-2026/vote-share/types';

export interface QuadratureFixture {
  id: string; components: number; blocks: number; Q: number[][]; b: number[];
  mean: number[]; interval90: number[][]; meanRefinement: number; endpointRefinement: number;
}
export const fixtureTarget = (f: QuadratureFixture): GaussianTarget => ({
  components: f.components, blocks: f.blocks, precision: Float64Array.from(f.Q.flat()), linear: Float64Array.from(f.b)
});
/** Predeclared numerical MCSE: 200 chain-stratified nonoverlapping block resamples,
 * blocks of 50 retained draws. Weighted order statistics avoid sorting each replay.
 * This is numerical-gate tooling, not a production uncertainty estimator. */
export function compareQuadrature(samples: GaussianSamples, fixture: QuadratureFixture, seed: string) {
  const { values, drawsPerChain: draws, chains, dimensions: d } = samples, n = draws * chains;
  if (draws % 50) throw new Error('Numerical block bootstrap requires full blocks of 50');
  const blocksPerChain = draws / 50, blocks = blocksPerChain * chains, replicates = 200;
  const rng = fitRandom(seed, 'numerical-mcse'), weights = new Uint16Array(blocks), coordinates = [];
  for (let j = 0; j < d; j++) {
    const order = Array.from({ length: n }, (_, i) => i).sort((a, b) => values[a * d + j] - values[b * d + j]);
    const sorted = Float64Array.from(order, i => values[i * d + j]), mean = sorted.reduce((s, x) => s + x, 0) / n;
    const interval = [quantile(sorted, 0.05), quantile(sorted, 0.95)];
    const blockSums = new Float64Array(blocks);
    for (let i = 0; i < n; i++) blockSums[Math.floor(i / 50)] += values[i * d + j];
    const means: number[] = [], low: number[] = [], high: number[] = [];
    for (let b = 0; b < replicates; b++) {
      weights.fill(0);
      for (let c = 0; c < chains; c++) for (let h = 0; h < blocksPerChain; h++) weights[c * blocksPerChain + rng.integer(blocksPerChain)]++;
      means.push(blockSums.reduce((s, x, i) => s + x * weights[i], 0) / n);
      let cumulative = 0, lo: number | undefined, hi: number | undefined;
      for (const i of order) {
        cumulative += weights[Math.floor(i / 50)];
        if (lo === undefined && cumulative > 0.05 * (n - 1)) lo = values[i * d + j];
        if (cumulative > 0.95 * (n - 1)) { hi = values[i * d + j]; break; }
      }
      low.push(lo!); high.push(hi!);
    }
    const sd = (xs: number[]) => { const mean = xs.reduce((s, x) => s + x, 0) / xs.length; return Math.sqrt(xs.reduce((s, x) => s + (x - mean) ** 2, 0) / (xs.length - 1)); };
    const meanMcse = sd(means), endpointMcse = [sd(low), sd(high)];
    const meanError = Math.abs(mean - fixture.mean[j]), endpointErrors = interval.map((x, h) => Math.abs(x - fixture.interval90[j][h]));
    coordinates.push({ mean, interval, meanError, endpointErrors, meanMcse, endpointMcse,
      passed: meanError <= Math.min(0.005, 0.001510236 + 4 * meanMcse) &&
        endpointErrors.every((error, h) => error <= Math.min(0.008, 0.003551512 + 4 * endpointMcse[h])) });
  }
  return { passed: samples.diagnostics.passed && fixture.meanRefinement <= 1e-6 && fixture.endpointRefinement <= 1e-5 && coordinates.every(x => x.passed),
    coordinates, diagnostics: samples.diagnostics, drawsPerChain: draws };
}
export function runNumericalGate(fixture: QuadratureFixture, seed: string) {
  const samples = fitGaussian(fixtureTarget(fixture), { seed });
  return { samples, comparison: compareQuadrature(samples, fixture, seed) };
}
