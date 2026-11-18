/** Reduced, seven-dimensional reference: two municipalities, three elector
 * outcomes, a national factor and a shared political polling error. */
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { basis, toScores, softmax, zeros, chol, inverse, mv, dot, add } from './drift-math';
import { precisionNoise } from './drift-gaussian';
import { createNormalSampler, createSeededRng } from '../../src/lib/core/rng';
import { writeVersion } from './sources';
import { summarizeScalarSamples } from './generation-metrics';

const root = fileURLToPath(new URL('../../../', import.meta.url)), d = 7, h = basis(3);
const scales = [.5, .5, .4, .4, .4, .4, .12], centre = [-.2, .15];
function probabilities(x: Float64Array, c: number) {
  return softmax(toScores(Float64Array.from(centre, (v, i) => v + x[i] + x[2 + 2 * c + i]), h));
}
const results = [];
for (const counts of [[[20, 35, 45], [50, 80, 70]], [[0, 8, 12], [15, 4, 11]]]) {
  function likelihood(x: Float64Array) {
    let log = 0, support = 0, total = 0;
    for (let c = 0; c < 2; c++) {
      const p = probabilities(x, c);
      for (let j = 0; j < 3; j++) log += counts[c][j] * Math.log(p[j]);
      const n = counts[c].reduce((a, b) => a + b, 0);
      support += n * p[1]; total += n * (p[1] + p[2]);
    }
    const q = Math.log(support / (total - support)) + x[6], observation = Math.log(.43 / .57), variance = 1 / (300 * .43 * .57);
    return log - .5 * (q - observation) ** 2 / variance;
  }
  const posterior = (x: Float64Array) => likelihood(x) - .5 * x.reduce((s, v, i) => s + (v / scales[i]) ** 2, 0);
  function derivatives(x: Float64Array) {
    const step = 1e-4, a = zeros(d), b = zeros(d, 1), f = posterior(x);
    for (let i = 0; i < d; i++) {
      const plus = x.slice(), minus = x.slice(); plus[i] += step; minus[i] -= step;
      const fp = posterior(plus), fm = posterior(minus); b[i] = (fp - fm) / (2 * step); a[i * d + i] = -(fp + fm - 2 * f) / step ** 2;
      for (let j = 0; j < i; j++) {
        let second = 0;
        for (const si of [-1, 1]) for (const sj of [-1, 1]) { const xx = x.slice(); xx[i] += si * step; xx[j] += sj * step; second += si * sj * posterior(xx); }
        a[i * d + j] = a[j * d + i] = -second / (4 * step ** 2);
      }
    }
    return { precision: a, gradient: b };
  }
  let mode = zeros(d, 1);
  for (let i = 0; i < 40; i++) {
    const der = derivatives(mode);
    let factor: ReturnType<typeof chol> | undefined;
    for (let ridge = 0; !factor; ridge = ridge ? ridge * 10 : 1) {
      const a = der.precision.slice(); for (let j = 0; j < d; j++) a[j * d + j] += ridge;
      try { factor = chol(a, d); } catch { if (ridge > 1e6) throw new Error('Reduced mode Hessian failed'); }
    }
    const direction = mv(inverse(factor, d), der.gradient, d);
    let step = 1;
    while (posterior(add(mode, direction, step)) < posterior(mode) && step > 1e-6) step /= 2;
    mode = add(mode, direction, step);
    if (Math.max(...Array.from(direction, Math.abs)) * step < 1e-7) break;
  }
  const factor = chol(derivatives(mode).precision, d), approximate: number[][] = [], exact: number[][] = [];
  const rng = createSeededRng('reduced-drift-reference:' + JSON.stringify(counts)), normal = createNormalSampler(rng);
  let state = mode.slice(), evaluations = 0;
  for (let iteration = 0; iteration < 24000; iteration++) {
    // Elliptical slice sampling, Murray et al. 2010. No Gaussian observation approximation.
    const nu = Float64Array.from(scales, sd => normal(0, sd)), threshold = likelihood(state) + Math.log(rng());
    let angle = rng() * 2 * Math.PI, low = angle - 2 * Math.PI, high = angle;
    for (;;) {
      const proposed = Float64Array.from(state, (v, j) => v * Math.cos(angle) + nu[j] * Math.sin(angle)); evaluations++;
      if (likelihood(proposed) >= threshold) { state = proposed; break; }
      if (angle < 0) low = angle; else high = angle; angle = low + rng() * (high - low);
    }
    if (iteration >= 4000 && iteration % 2 === 0) {
      exact.push([0, 1].flatMap(c => Array.from(probabilities(state, c))));
      const x = add(mode, precisionNoise(factor, () => normal(0, 1), d));
      approximate.push([0, 1].flatMap(c => Array.from(probabilities(x, c))));
    }
  }
  const variables = Array.from({ length: 6 }, (_, j) => {
    const reference = summarizeScalarSamples(exact.map(x => x[j]), 0), laplace = summarizeScalarSamples(approximate.map(x => x[j]), 0);
    return { meanExact: reference.predictiveMean, meanLaplace: laplace.predictiveMean, widthExact90: reference.width90, widthLaplace90: laplace.width90,
      meanDifference: laplace.predictiveMean - reference.predictiveMean, widthRatio: laplace.width90 / reference.width90 };
  });
  const maxMeanDifference = Math.max(...variables.map(x => Math.abs(x.meanDifference)));
  if (maxMeanDifference > .025 || variables.some(x => x.widthRatio < .65 || x.widthRatio > 1.5)) throw new Error('Reduced posterior check failed: ' + JSON.stringify(variables));
  results.push({ counts, draws: exact.length, evaluations, maxMeanDifference, variables });
}
await writeVersion(resolve(root, 'data/derived/drift/reduced-posterior-check.json'), { method: 'elliptical-slice versus finite-difference joint Laplace; independent Gaussian priors', reference: 'https://proceedings.mlr.press/v9/murray10a.html', results });
console.log(JSON.stringify(results));
