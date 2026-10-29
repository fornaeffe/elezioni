import { dot, factorSpd, multiply, solveSpd } from './linalg';
import { diagnose } from './diagnostics';
import { fitRandom } from './random';
import { requireWorkspace, type GaussianSamples, type GaussianTarget, type ModeResult, type SamplerOptions } from './types';

export function validateTarget(t: GaussianTarget): number {
  const n = t.components * t.blocks;
  if (!Number.isInteger(t.components) || t.components < 1 || !Number.isInteger(t.blocks) || t.blocks < 1 ||
    t.precision.length !== n * n || t.linear.length !== n || t.precision.some(x => !Number.isFinite(x)) ||
    t.linear.some(x => !Number.isFinite(x))) throw new RangeError('Invalid bounded Gaussian target');
  return n;
}
/** Feasible active-set QP, retaining true boundary modes. Sampling starts inside.
 * Equality multipliers are solved with the same equilibrated SPD factors. */
export function gaussianMode(t: GaussianTarget): ModeResult {
  const n = validateTarget(t), k = t.components, blocks = t.blocks;
  factorSpd(t.precision, n); // A proper Gaussian target is required even at a boundary.
  const x = new Float64Array(n).fill(1 / k), active = new Uint8Array(n);
  for (let iteration = 0; iteration < 20 * n; iteration++) {
    const free = Array.from({ length: n }, (_, i) => i).filter(i => !active[i]), nf = free.length;
    const q = new Float64Array(nf * nf), rhs = Float64Array.from(free, i => t.linear[i]);
    free.forEach((i, a) => free.forEach((j, b) => { q[a * nf + b] = t.precision[i * n + j]; }));
    const f = factorSpd(q, nf), u = solveSpd(f, rhs);
    const v = Array.from({ length: blocks }, (_, h) => solveSpd(f, Float64Array.from(free, i => Number(Math.floor(i / k) === h))));
    const schur = new Float64Array(blocks * blocks), projected = new Float64Array(blocks);
    for (let h = 0; h < blocks; h++) {
      projected[h] = -1;
      free.forEach((i, a) => {
        if (Math.floor(i / k) !== h) return;
        projected[h] += u[a];
        for (let g = 0; g < blocks; g++) schur[h * blocks + g] += v[g][a];
      });
    }
    const lagrange = solveSpd(factorSpd(schur, blocks), projected);
    const candidate = u.map((value, a) => value - v.reduce((s, row, h) => s + row[a] * lagrange[h], 0));
    let hit = -1, step = 1;
    for (let a = 0; a < nf; a++) if (candidate[a] < -1e-12) {
      const direction = candidate[a] - x[free[a]], ratio = -x[free[a]] / direction;
      if (direction < 0 && ratio <= step) { step = ratio; hit = free[a]; }
    }
    if (hit >= 0) {
      free.forEach((i, a) => { x[i] += Math.max(0, step) * (candidate[a] - x[i]); });
      x[hit] = 0; active[hit] = 1; continue;
    }
    x.fill(0); free.forEach((i, a) => { x[i] = Math.max(0, candidate[a]); });
    const qx = multiply(t.precision, x, n), gradient = qx.map((v, i) => v - t.linear[i] + lagrange[Math.floor(i / k)]);
    let release = -1, worst = 0, scaledKktResidual = 0, stationarityResidual = 0;
    for (let i = 0; i < n; i++) {
      let scale = Math.abs(t.linear[i]) + Math.abs(lagrange[Math.floor(i / k)]);
      for (let j = 0; j < n; j++) scale += Math.abs(t.precision[i * n + j] * x[j]);
      scale = Math.max(1, scale);
      const violation = active[i] ? Math.max(0, -gradient[i]) : Math.abs(gradient[i]);
      scaledKktResidual = Math.max(scaledKktResidual, violation / scale);
      stationarityResidual = Math.max(stationarityResidual, violation);
      if (active[i] && gradient[i] < -1e-8 - 64 * Number.EPSILON * scale && gradient[i] < worst) { worst = gradient[i]; release = i; }
    }
    if (release >= 0) { active[release] = 0; continue; }
    let equalityResidual = 0;
    for (let h = 0; h < blocks; h++) {
      let total = 0; for (let j = 0; j < k; j++) total += x[h * k + j];
      equalityResidual = Math.max(equalityResidual, Math.abs(total - 1));
    }
    if (equalityResidual > 1e-12 || scaledKktResidual > 1e-8) throw new Error('MODE_KKT_FAILED: scaled stationarity or equality');
    return { point: x, objective: 0.5 * dot(x, qx) - dot(t.linear, x),
      equalityResidual, scaledKktResidual, stationarityResidual, iterations: iteration + 1 };
  }
  throw new Error('MODE_NOT_CONVERGED: bounded Gaussian active set');
}
export function sampleGaussian(t: GaussianTarget, options: SamplerOptions): GaussianSamples {
  const n = validateTarget(t), k = t.components, blocks = t.blocks;
  const { seed, draws = 2000, chains = 4, burn = 1000, thin = 2 } = options;
  if ([draws, chains, burn, thin].some(x => !Number.isSafeInteger(x)) || draws < 8 || chains < 2 || chains > 4 || burn < 0 || thin < 1)
    throw new RangeError('Invalid Gaussian sampler budget');
  // Includes diagnostic ranks/FFT/sorting workspace as a conservative allocation bound.
  requireWorkspace((n * n * 8 + chains * draws * n * 8) * 2 + chains * draws * 192, options.maximumWorkspaceBytes);
  const point = gaussianMode(t).point, values = new Float64Array(chains * draws * n), q = t.precision;
  for (let chain = 0; chain < chains; chain++) {
    const rng = fitRandom(seed, 'chain:' + chain), x = point.slice(), gradient = new Float64Array(n), indexes = new Int32Array(2 * blocks);
    for (let h = 0; h < blocks; h++) {
      const u = Float64Array.from({ length: k }, () => -Math.log(rng.uniform())), total = u.reduce((a, b) => a + b, 0);
      for (let i = 0; i < k; i++) x[h * k + i] = (1 - 0.02 - 0.15 * chain) * point[h * k + i] + (0.02 + 0.15 * chain) * u[i] / total;
    }
    for (let sweep = 0; sweep < burn + draws * thin; sweep++) {
      multiply(q, x, n, gradient);
      for (let a = 0; a < n; a++) gradient[a] -= t.linear[a];
      for (let update = 0; k > 1 && update < k * (blocks + 1); update++) {
        const i = rng.integer(k), j = (i + 1 + rng.integer(k - 1)) % k, shared = blocks > 1 && update % 3 === 0;
        const block = rng.integer(blocks); let count = 0;
        for (let h = 0; h < blocks; h++) if (shared || h === block) { indexes[count++] = h * k + i; indexes[count++] = h * k + j; }
        let lower = -Infinity, upper = Infinity, linear = 0, precision = 0;
        for (let a = 0; a < count; a++) {
          const sign = a % 2 ? -1 : 1;
          if (sign > 0) lower = Math.max(lower, -x[indexes[a]]); else upper = Math.min(upper, x[indexes[a]]);
          linear += sign * gradient[indexes[a]];
          for (let z = 0; z < count; z++) precision += sign * (z % 2 ? -1 : 1) * q[indexes[a] * n + indexes[z]];
        }
        if (!(precision > 0) || !Number.isFinite(precision)) throw new Error('Nonpositive Gaussian transfer precision');
        if (upper - lower < 1e-16) continue;
        const sd = 1 / Math.sqrt(precision), mean = -linear / precision;
        const rawMove = mean + sd * rng.truncatedNormal((lower - mean) / sd, (upper - mean) / sd);
        const move = Math.max(lower, Math.min(upper, rawMove));
        for (let a = 0; a < count; a++) x[indexes[a]] += move * (a % 2 ? -1 : 1);
        for (let a = 0; a < n; a++) {
          let change = 0;
          for (let z = 0; z < count; z++) change += q[indexes[z] * n + a] * (z % 2 ? -1 : 1);
          gradient[a] += move * change;
        }
      }
      if (sweep >= burn && (sweep - burn) % thin === 0) values.set(x, (chain * draws + (sweep - burn) / thin) * n);
    }
  }
  for (let offset = 0; offset < values.length; offset += k) {
    let total = 0;
    for (let j = 0; j < k; j++) { if (values[offset + j] < 0) throw new Error('Negative Gaussian simplex coordinate'); total += values[offset + j]; }
    if (Math.abs(total - 1) > 1e-12) throw new Error('Gaussian simplex sum drift');
  }
  return { values, chains, drawsPerChain: draws, dimensions: n,
    diagnostics: diagnose(values, chains, draws, n, k === 1 ? Array.from({ length: n }, (_, i) => i) : []) };
}
export function fitGaussian(t: GaussianTarget, options: Omit<SamplerOptions, 'draws'>): GaussianSamples {
  if (options.chains !== undefined && options.chains !== 4) throw new RangeError('Accepted fits require four chains');
  for (const draws of [2000, 4000, 8000]) {
    const result = sampleGaussian(t, { ...options, draws });
    if (result.diagnostics.passed) return result;
  }
  throw new Error('FIT_DIAGNOSTICS_FAILED: four-chain R-hat/bulk ESS/tail ESS after 8000 retained draws');
}
