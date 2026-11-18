import { createNormalSampler, createSeededRng, type Rng } from '../../core/rng';

export const GENERATION_STREAM_VERSION = 'r26-generation-streams-v1';
/** Entity-addressed streams are invariant to iteration order, chunk size and
 * consumption in a different model or chamber. Legal draws use another namespace. */
export function generationRng(seed: string, simulationId: number, ...scope: (string | number)[]): Rng {
  return createSeededRng(JSON.stringify([GENERATION_STREAM_VERSION, seed, simulationId, ...scope]));
}
export function generationNormal(seed: string, simulationId: number, ...scope: (string | number)[]): number {
  return createNormalSampler(generationRng(seed, simulationId, ...scope))(0, 1);
}
export const sum = (xs: readonly number[]): number => xs.reduce((a, b) => a + b, 0);
export const mean = (xs: readonly number[]): number => xs.length ? sum(xs) / xs.length : 0;
export const variance = (xs: readonly number[]): number => mean(xs.map(x => (x - mean(xs)) ** 2));
export const logit = (p: number): number => Math.log(Math.max(1e-9, Math.min(1 - 1e-9, p)) / Math.max(1e-9, 1 - Math.min(1 - 1e-9, p)));
export const logistic = (x: number): number => 1 / (1 + Math.exp(-x));

/** Integer reconstruction for statistical preparation, never legal scrutiny.
 * Exact integer weights use BigInt, with canonical seeded ties. */
export function apportion(total: number, weights: readonly number[], rng: Rng): number[] {
  if (!Number.isSafeInteger(total) || total < 0 || weights.some(w => !Number.isSafeInteger(w) || w < 0)) throw new RangeError('Invalid integer reconstruction');
  const denominator = weights.reduce((s, w) => s + BigInt(w), 0n);
  if (denominator === 0n) {
    if (total) throw new RangeError('Positive total has no reconstruction support');
    return weights.map(() => 0);
  }
  const rows = weights.map((w, i) => ({ i, n: BigInt(total) * BigInt(w), tie: rng() }));
  const result = rows.map(r => Number(r.n / denominator));
  const rank = rows.sort((a, b) => {
    const ra = a.n % denominator, rb = b.n % denominator;
    return ra === rb ? a.tie - b.tie : ra > rb ? -1 : 1;
  });
  const remaining = total - sum(result);
  for (let i = 0; i < remaining; i++) result[rank[i].i]++;
  return result;
}

// Marsaglia-Tsang gamma(shape), with a private normal stream supplied by caller.
function gamma(shape: number, rng: Rng): number {
  if (shape < 1) return gamma(shape + 1, rng) * rng() ** (1 / shape);
  const normal = createNormalSampler(rng), d = shape - 1 / 3, c = 1 / Math.sqrt(9 * d);
  for (;;) {
    const x = normal(0, 1), t = 1 + c * x;
    if (t <= 0) continue;
    const v = t ** 3, u = rng();
    if (u < 1 - 0.0331 * x ** 4 || Math.log(u) < x * x / 2 + d * (1 - v + Math.log(v))) return d * v;
  }
}
/** Exact beta order-statistic recursion, avoiding per-voter simulation. */
export function binomial(n: number, p: number, rng: Rng): number {
  if (!Number.isSafeInteger(n) || n < 0 || !Number.isFinite(p) || p < 0 || p > 1) throw new RangeError('Invalid binomial parameters');
  if (p === 0 || n === 0) return 0;
  if (p === 1) return n;
  if (p > 0.5) return n - binomial(n, 1 - p, rng);
  if (n <= 32) { let k = 0; for (let i = 0; i < n; i++) k += Number(rng() < p); return k; }
  if (n * p < 20) {
    // Waiting times between successes are geometric.
    let position = -1, k = 0;
    for (;;) { position += 1 + Math.floor(Math.log(1 - rng()) / Math.log1p(-p)); if (position >= n) return k; k++; }
  }
  const a = 1 + Math.floor(n / 2), b = n + 1 - a, ga = gamma(a, rng), gb = gamma(b, rng), x = ga / (ga + gb);
  return x >= p ? binomial(a - 1, p / x, rng) : a + binomial(b - 1, (p - x) / (1 - x), rng);
}
export function multinomial(n: number, probabilities: readonly number[], rng: Rng): number[] {
  if (!probabilities.length || probabilities.some(p => !Number.isFinite(p) || p < 0) || !(sum(probabilities) > 0)) throw new RangeError('Invalid category probabilities');
  let remaining = n, mass = sum(probabilities);
  return probabilities.map((p, i) => {
    const k = !remaining || !p ? 0 : i === probabilities.length - 1 || mass <= p ? remaining : binomial(remaining, Math.max(0, Math.min(1, p / mass)), rng);
    remaining -= k; mass -= p;
    return k;
  });
}
