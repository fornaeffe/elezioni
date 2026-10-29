import { type ChainDiagnostics, type CoordinateDiagnostic, type Vector } from './types';

const NORMAL_A = [-39.69683028665376, 220.9460984245205, -275.9285104469687, 138.357751867269, -30.66479806614716, 2.506628277459239];
const NORMAL_B = [-54.47609879822406, 161.5858368580409, -155.6989798598866, 66.80131188771972, -13.28068155288572];
const NORMAL_C = [-0.007784894002430293, -0.3223964580411365, -2.400758277161838, -2.549732539343734, 4.374664141464968, 2.938163982698783];
const NORMAL_D = [0.007784695709041462, 0.3224671290700398, 2.445134137142996, 3.754408661907416];
const polynomial = (xs: number[], x: number) => xs.reduce((value, coefficient) => value * x + coefficient, 0);
/** Acklam's inverse-normal rational approximation (absolute error about 4e-9).
 * Diagnostics use Blom ranks, so probabilities never approach machine endpoints. */
export function normalQuantile(p: number): number {
  if (!(p > 0 && p < 1)) throw new RangeError('Normal quantile requires an interior probability');
  if (p < 0.02425 || p > 0.97575) {
    const q = Math.sqrt(-2 * Math.log(p < 0.5 ? p : 1 - p));
    const value = polynomial(NORMAL_C, q) / (polynomial(NORMAL_D, q) * q + 1);
    return p < 0.5 ? value : -value;
  }
  const q = p - 0.5, r = q * q;
  return polynomial(NORMAL_A, r) * q / (polynomial(NORMAL_B, r) * r + 1);
}
export function quantile(sorted: ArrayLike<number>, probability: number): number {
  const position = (sorted.length - 1) * probability, lower = Math.floor(position);
  return sorted[lower] + (sorted[Math.ceil(position)] - sorted[lower]) * (position - lower);
}
function rankNormal(values: Vector): Vector {
  const order = Array.from({ length: values.length }, (_, i) => i).sort((i, j) => values[i] - values[j]);
  const out = new Float64Array(values.length);
  for (let first = 0; first < order.length;) {
    let end = first + 1;
    while (end < order.length && values[order[first]] === values[order[end]]) end++;
    const z = normalQuantile(((first + end + 1) / 2 - 0.375) / (order.length + 0.25));
    for (let i = first; i < end; i++) out[order[i]] = z;
    first = end;
  }
  return out;
}
function fft(real: Vector, imaginary: Vector, inverse: boolean): void {
  const n = real.length;
  for (let i = 1, j = 0; i < n; i++) {
    let bit = n >> 1;
    for (; j & bit; bit >>= 1) j ^= bit;
    j ^= bit;
    if (i < j) { [real[i], real[j]] = [real[j], real[i]]; [imaginary[i], imaginary[j]] = [imaginary[j], imaginary[i]]; }
  }
  let previousCos = 0, previousSin = 1;
  for (let length = 2; length <= n; length *= 2) {
    // Binary half-angle roots use only correctly rounded sqrt/arithmetic, so
    // diagnostic bytes do not depend on a runtime's sin/cos implementation.
    let wr = length === 2 ? -1 : 0, positiveSin = length === 2 ? 0 : 1;
    if (length >= 8) {
      wr = Math.sqrt((1 + previousCos) / 2); positiveSin = previousSin / (2 * wr);
      previousCos = wr; previousSin = positiveSin;
    }
    const wi = (inverse ? 1 : -1) * positiveSin;
    for (let first = 0; first < n; first += length) {
      let ur = 1, ui = 0;
      for (let j = 0; j < length / 2; j++) {
        const a = first + j, b = a + length / 2;
        const vr = real[b] * ur - imaginary[b] * ui, vi = real[b] * ui + imaginary[b] * ur;
        real[b] = real[a] - vr; imaginary[b] = imaginary[a] - vi;
        real[a] += vr; imaginary[a] += vi;
        const next = ur * wr - ui * wi; ui = ur * wi + ui * wr; ur = next;
      }
    }
  }
  if (inverse) for (let i = 0; i < n; i++) { real[i] /= n; imaginary[i] /= n; }
}
function moments(values: Vector, chains: number, draws: number) {
  const means = new Float64Array(chains), variances = new Float64Array(chains);
  for (let c = 0; c < chains; c++) {
    for (let i = 0; i < draws; i++) means[c] += values[c * draws + i] / draws;
    for (let i = 0; i < draws; i++) variances[c] += (values[c * draws + i] - means[c]) ** 2 / (draws - 1);
  }
  const mean = means.reduce((s, x) => s + x, 0) / chains;
  const within = variances.reduce((s, x) => s + x, 0) / chains;
  const between = means.reduce((s, x) => s + (x - mean) ** 2, 0) * draws / (chains - 1);
  return { means, within, variance: (draws - 1) / draws * within + between / draws };
}
export function effectiveSampleSize(values: Vector, chains: number, draws: number): number {
  const { means, within, variance } = moments(values, chains, draws);
  if (variance <= 1e-30) return values.length;
  let length = 1; while (length < draws * 2) length *= 2;
  const ac = new Float64Array(draws), real = new Float64Array(length), imaginary = new Float64Array(length);
  for (let c = 0; c < chains; c++) {
    real.fill(0); imaginary.fill(0);
    for (let i = 0; i < draws; i++) real[i] = values[c * draws + i] - means[c];
    fft(real, imaginary, false);
    for (let i = 0; i < length; i++) { real[i] = real[i] ** 2 + imaginary[i] ** 2; imaginary[i] = 0; }
    fft(real, imaginary, true);
    for (let i = 0; i < draws; i++) ac[i] += real[i] / (draws * chains);
  }
  let pairs = 0, previous = Infinity;
  for (let t = 0; t < draws - 1; t += 2) {
    const rho0 = t === 0 ? 1 : 1 - (within - ac[t]) / variance;
    const pair = rho0 + 1 - (within - ac[t + 1]) / variance;
    if (pair < 0) break;
    previous = Math.min(pair, previous); pairs += previous;
  }
  return Math.min(values.length, values.length / Math.max(1, -1 + 2 * pairs));
}
function rhat(values: Vector, chains: number, draws: number): number {
  const { within, variance } = moments(values, chains, draws);
  return within > 0 ? Math.sqrt(variance / within) : Infinity;
}
/** Rank-normalized split/folded R-hat and Geyer initial-positive/monotone ESS.
 * See the FV-01 protocol and Stan's posterior-analysis definitions. */
export function diagnose(values: Vector, chains: number, draws: number, dimensions: number, fixed: readonly number[] = []): ChainDiagnostics {
  if (chains < 2 || draws < 8 || values.length !== chains * draws * dimensions || values.some(x => !Number.isFinite(x)))
    throw new RangeError('Invalid diagnostic chains');
  const half = Math.floor(draws / 2), splitChains = chains * 2;
  const coordinates: CoordinateDiagnostic[] = [];
  for (let j = 0; j < dimensions; j++) {
    const raw = new Float64Array(splitChains * half);
    for (let c = 0; c < chains; c++) for (let i = 0; i < half; i++) {
      raw[c * half + i] = values[(c * draws + i) * dimensions + j];
      raw[(c + chains) * half + i] = values[(c * draws + draws - half + i) * dimensions + j];
    }
    if (raw.every(x => x === raw[0])) {
      const known = fixed.includes(j);
      coordinates.push({ rhat: null, bulkEss: known ? raw.length : 0, tailEss: known ? raw.length : 0, constant: known ? 'fixed' : 'stuck' });
      continue;
    }
    const sorted = Float64Array.from(raw).sort(), centre = quantile(sorted, 0.5);
    const z = rankNormal(raw), folded = rankNormal(raw.map(x => Math.abs(x - centre)));
    const r = Math.max(rhat(z, splitChains, half), rhat(folded, splitChains, half));
    const lo = quantile(sorted, 0.05), hi = quantile(sorted, 0.95);
    const tail = Math.min(effectiveSampleSize(raw.map(x => Number(x <= lo)), splitChains, half),
      effectiveSampleSize(raw.map(x => Number(x >= hi)), splitChains, half));
    coordinates.push({ rhat: Number.isFinite(r) ? r : null, bulkEss: effectiveSampleSize(z, splitChains, half), tailEss: tail, constant: false });
  }
  const valid = coordinates.every(x => x.constant === 'fixed' || x.rhat !== null && x.rhat <= 1.05 && x.bulkEss >= 400 && x.tailEss >= 200);
  const finite = coordinates.flatMap(x => x.rhat === null ? [] : [x.rhat]);
  return { passed: valid, maxRhat: finite.length ? Math.max(...finite) : null,
    minEss: Math.min(...coordinates.map(x => x.bulkEss)), minTailEss: Math.min(...coordinates.map(x => x.tailEss)), coordinates };
}
