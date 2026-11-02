/** Small dense blocks for the research Gaussian/Laplace solver. Row major. */
export type Mat = Float64Array<ArrayBuffer>;
export const zeros = (n: number, m = n): Mat => new Float64Array(n * m);
export const eye = (n: number, scale = 1): Mat => {
  const a = zeros(n); for (let i = 0; i < n; i++) a[i * n + i] = scale; return a;
};
export const dot = (a: ArrayLike<number>, b: ArrayLike<number>) => {
  let v = 0; for (let i = 0; i < a.length; i++) v += a[i] * b[i]; return v;
};
export function mv(a: Mat, x: ArrayLike<number>, rows: number, cols = x.length): Mat {
  const y = zeros(rows, 1);
  for (let i = 0; i < rows; i++) for (let j = 0; j < cols; j++) y[i] += a[i * cols + j] * x[j];
  return y;
}
export function mm(a: Mat, b: Mat, n: number, k: number, m: number): Mat {
  const out = zeros(n, m);
  for (let i = 0; i < n; i++) for (let h = 0; h < k; h++) {
    const v = a[i * k + h]; if (!v) continue;
    for (let j = 0; j < m; j++) out[i * m + j] += v * b[h * m + j];
  }
  return out;
}
export function tr(a: Mat, n: number, m: number): Mat {
  const out = zeros(m, n); for (let i = 0; i < n; i++) for (let j = 0; j < m; j++) out[j * n + i] = a[i * m + j]; return out;
}
export function add(a: Mat, b: Mat, scale = 1): Mat {
  const out = a.slice(); for (let i = 0; i < a.length; i++) out[i] += scale * b[i]; return out;
}
export function chol(a: Mat, n: number, jitter = 0): Mat {
  const l = zeros(n);
  for (let i = 0; i < n; i++) for (let j = 0; j <= i; j++) {
    let v = a[i * n + j] + (i === j ? jitter : 0);
    for (let k = 0; k < j; k++) v -= l[i * n + k] * l[j * n + k];
    if (i === j) {
      if (!(v > 0) || !Number.isFinite(v)) throw new Error('Non-positive research covariance at ' + i + ': ' + v);
      l[i * n + j] = Math.sqrt(v);
    } else l[i * n + j] = v / l[j * n + j];
  }
  return l;
}
export function solve(l: Mat, b: ArrayLike<number>, n: number): Mat {
  const y = Float64Array.from(b);
  for (let i = 0; i < n; i++) { for (let j = 0; j < i; j++) y[i] -= l[i * n + j] * y[j]; y[i] /= l[i * n + i]; }
  for (let i = n - 1; i >= 0; i--) { for (let j = i + 1; j < n; j++) y[i] -= l[j * n + i] * y[j]; y[i] /= l[i * n + i]; }
  return y;
}
export function inverse(l: Mat, n: number): Mat {
  const out = zeros(n);
  for (let j = 0; j < n; j++) {
    const e = zeros(n, 1); e[j] = 1; const x = solve(l, e, n);
    for (let i = 0; i < n; i++) out[i * n + j] = x[i];
  }
  return out;
}
export function logdet(l: Mat, n: number) {
  let out = 0; for (let i = 0; i < n; i++) out += 2 * Math.log(l[i * n + i]); return out;
}
export function sandwich(a: Mat, v: Mat, n: number, k: number): Mat {
  return mm(mm(a, v, n, k, k), tr(a, n, k), n, k, n);
}
export function symmetrize(a: Mat, n: number): Mat {
  const out = a.slice();
  for (let i = 0; i < n; i++) for (let j = 0; j < i; j++) out[i * n + j] = out[j * n + i] = (a[i * n + j] + a[j * n + i]) / 2;
  return out;
}
export function psdChol(a: Mat, n: number) {
  const sym = symmetrize(a, n); let jitter = 1e-12;
  for (let i = 0; i < 9; i++, jitter *= 10) {
    try { return { factor: chol(sym, n, jitter), jitter }; } catch { /* diagnostic returned on success */ }
  }
  throw new Error('Covariance needs material PSD repair');
}
export function softmax(x: ArrayLike<number>) {
  let max = -Infinity; for (let i = 0; i < x.length; i++) max = Math.max(max, x[i]);
  const p = Float64Array.from(x, v => Math.exp(v - max));
  const total = p.reduce((a, b) => a + b, 0); for (let i = 0; i < p.length; i++) p[i] /= total; return p;
}
/** J x (J-1) orthonormal Helmert basis; rows correspond to named outcomes. */
export function basis(j: number): Mat {
  const h = zeros(j, j - 1);
  for (let k = 0; k < j - 1; k++) for (let i = 0; i < j; i++) h[i * (j - 1) + k] =
    i <= k ? 1 / Math.sqrt((k + 1) * (k + 2)) : i === k + 1 ? -(k + 1) / Math.sqrt((k + 1) * (k + 2)) : 0;
  return h;
}
export function toScores(z: ArrayLike<number>, h: Mat) { return mv(h, z, z.length + 1); }
export function toContrasts(p: ArrayLike<number>, h: Mat) {
  if (Array.from(p).some(v => !Number.isFinite(v) || v <= 0)) throw new Error('Positive probabilities required');
  return mv(tr(h, p.length, p.length - 1), Float64Array.from(p, Math.log), p.length - 1);
}
export function innovationCov(scales: ArrayLike<number>, h: Mat): Mat {
  const d = scales.length - 1, q = zeros(d);
  for (let j = 0; j < scales.length; j++) for (let r = 0; r < d; r++) for (let s = 0; s < d; s++) q[r * d + s] += h[j * d + r] * h[j * d + s] * scales[j] ** 2;
  return q;
}
export interface CountMessage { y: Mat; r: Mat; a: Mat; active: number[]; logLikelihood: number; gradientNorm: number; }
/** Exact multinomial derivatives expressed in an active-ballot contrast basis.
 * The quadratic is recomputed at each joint Laplace/IRLS iteration. */
export function countMessage(counts: number[], active: number[], state: Mat, fullBasis: Mat): CountMessage {
  const j = counts.length, d = j - 1, k = active.length, o = k - 1, hb = basis(k);
  if (o < 1 || counts.some(v => !Number.isSafeInteger(v) || v < 0) || active.some(i => counts[i] < 0)) throw new Error('Invalid multinomial observation');
  const a = zeros(o, d), scores = toScores(state, fullBasis), p = softmax(active.map(i => scores[i]));
  for (let r = 0; r < o; r++) for (let c = 0; c < d; c++) for (let i = 0; i < k; i++) a[r * d + c] += hb[i * o + r] * fullBasis[active[i] * d + c];
  const n = active.reduce((s, i) => s + counts[i], 0), gradient = zeros(o, 1), w = zeros(o);
  const hp = zeros(o, 1);
  for (let r = 0; r < o; r++) for (let i = 0; i < k; i++) { gradient[r] += hb[i * o + r] * (counts[active[i]] - n * p[i]); hp[r] += p[i] * hb[i * o + r]; }
  for (let r = 0; r < o; r++) for (let c = 0; c < o; c++) {
    let v = -hp[r] * hp[c]; for (let i = 0; i < k; i++) v += p[i] * hb[i * o + r] * hb[i * o + c]; w[r * o + c] = n * v;
  }
  const l = chol(w, o, 1e-9), r = inverse(l, o), correction = solve(l, gradient, o), y = mv(a, state, o);
  for (let i = 0; i < o; i++) y[i] += correction[i];
  const logLikelihood = active.reduce((s, idx, i) => s + counts[idx] * Math.log(Math.max(1e-300, p[i])), 0);
  return { y, r, a, active, logLikelihood, gradientNorm: Math.hypot(...gradient) };
}
/** Proper weak initial score prior, conditioned once on first observed counts.
 * Missing/absent outcomes remain unobserved, never pseudo-vote observations. */
export function initialCountPosterior(counts: number[], active: number[], mean: Mat, h: Mat, priorSd = 3, priorCovariance?: Mat) {
  const d = mean.length, prior = priorCovariance ? inverse(chol(priorCovariance, d), d) : eye(d, 1 / priorSd ** 2);
  let state = mean.slice(), change = Infinity;
  const nll = (x: Mat) => {
    const scores = toScores(x, h), p = softmax(active.map(i => scores[i]));
    const diff = add(x, mean, -1);
    return -active.reduce((s, idx, i) => s + counts[idx] * Math.log(Math.max(1e-300, p[i])), 0) + .5 * dot(diff, mv(prior, diff, d));
  };
  let covariance = eye(d);
  for (let iteration = 0; iteration < 80; iteration++) {
    const msg = countMessage(counts, active, state, h), o = active.length - 1, ri = inverse(chol(msg.r, o), o);
    const wa = mm(ri, msg.a, o, o, d), mat = add(prior, mm(tr(msg.a, o, d), wa, d, o, d));
    const diff = add(msg.y, mv(msg.a, state, o), -1);
    const g = mv(tr(wa, o, d), diff, d);
    const pg = mv(prior, add(state, mean, -1), d);
    for (let r = 0; r < d; r++) g[r] -= pg[r];
    const l = chol(mat, d), step = solve(l, g, d); covariance = inverse(l, d);
    let rate = 1, trial = add(state, step, rate), old = nll(state);
    while (nll(trial) > old + 1e-9 && rate > 1e-8) { rate /= 2; trial = add(state, step, rate); }
    change = Math.max(...trial.map((v, i) => Math.abs(v - state[i]))); state = trial;
    if (change < 1e-7) break;
  }
  return { mean: state, covariance, change };
}
export const years = (later: string, earlier: string) => (Date.parse(later) - Date.parse(earlier)) / (365.25 * 86400000);
export const dateDay = (s: string) => Date.parse(s) / 86400000;
export function ouStep(dt: number, kappa: number) {
  return { phi: Math.exp(-kappa * dt), varianceTime: kappa > 1e-9 ? -Math.expm1(-2 * kappa * dt) / (2 * kappa) : dt };
}
export function logGamma(x: number): number {
  const cof = [676.5203681218851,-1259.1392167224028,771.32342877765313,-176.61502916214059,12.507343278686905,-.13857109526572012,9.9843695780195716e-6,1.5056327351493116e-7];
  if (x < .5) return Math.log(Math.PI) - Math.log(Math.sin(Math.PI * x)) - logGamma(1 - x);
  x -= 1; let a = .99999999999980993;
  cof.forEach((c, i) => a += c / (x + i + 1)); const t = x + cof.length - .5;
  return .5 * Math.log(2 * Math.PI) + (x + .5) * Math.log(t) - t + Math.log(a);
}
