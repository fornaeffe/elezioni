import { zeros, eye, add, mm, mv, tr, chol, solve, inverse, logdet, sandwich, ouStep, type Mat, type CountMessage } from './drift-math';

export interface GaussianObservation { time: number; message: CountMessage; design: Mat; }
export interface GaussianNode {
  time: number; u: Mat; f: Mat; p: Mat;
  smoothU: Mat; smoothF: Mat; smoothP: Mat;
}
export interface GPQuery { u: Mat; f: Mat; p: Mat; filteredP: Mat; }
/** Local Markov chain conditioned on its Gaussian working observations.
 * Affine means u+F*g retain dependence on the global Gaussian state. */
export class LocalGaussian {
  readonly nodes: GaussianNode[] = [];
  readonly precision: Mat;
  readonly linear: Mat;
  constant = 0;
  constructor(readonly d: number, readonly g: number, readonly first: number, readonly initial: Mat,
    readonly q: Mat, readonly kappa: number, readonly observations: GaussianObservation[], readonly initialScore: Mat) {
    this.precision = zeros(g); this.linear = zeros(g, 1);
    let u = zeros(d, 1), f = zeros(d, g), p = initial.slice(), last = first;
    this.nodes.push({ time: first, u: u.slice(), f: f.slice(), p: p.slice(), smoothU: u.slice(), smoothF: f.slice(), smoothP: p.slice() });
    for (const ob of observations) {
      const step = ouStep(ob.time - last, kappa), o = ob.message.y.length;
      u = u.map(v => v * step.phi); f = f.map(v => v * step.phi);
      p = add(p.map(v => v * step.phi ** 2), q, step.varianceTime);
      const a = ob.message.a, s = add(sandwich(a, p, o, d), ob.message.r), l = chol(s, o), si = inverse(l, o);
      const y = add(add(ob.message.y, mv(a, initialScore, o), -1), mv(a, u, o), -1);
      const c = add(ob.design, mm(a, f, o, d, g));
      const sic = mm(si, c, o, o, g), siy = mv(si, y, o);
      const ct = tr(c, o, g);
      const extraP = mm(ct, sic, g, o, g), extraB = mv(ct, siy, g);
      for (let i = 0; i < extraP.length; i++) this.precision[i] += extraP[i];
      for (let i = 0; i < g; i++) this.linear[i] += extraB[i];
      this.constant -= .5 * (logdet(l, o) + o * Math.log(2 * Math.PI) + y.reduce((n, v, i) => n + v * siy[i], 0));
      const gain = mm(mm(p, tr(a, o, d), d, d, o), si, d, o, o);
      u = add(u, mv(gain, y, d));
      f = add(f, mm(gain, c, d, o, g), -1);
      p = add(p, mm(mm(gain, a, d, o, d), p, d, d, d), -1);
      this.nodes.push({ time: ob.time, u: u.slice(), f: f.slice(), p: p.slice(), smoothU: u.slice(), smoothF: f.slice(), smoothP: p.slice() });
      last = ob.time;
    }
    for (let i = this.nodes.length - 2; i >= 0; i--) {
      const left = this.nodes[i], right = this.nodes[i + 1], step = ouStep(right.time - left.time, kappa);
      const predicted = add(left.p.map(v => v * step.phi ** 2), q, step.varianceTime);
      const gain = mm(left.p.map(v => v * step.phi), inverse(chol(predicted, d), d), d, d, d);
      left.smoothU = add(left.u, mv(gain, add(right.smoothU, left.u.map(v => v * step.phi), -1), d));
      left.smoothF = add(left.f, mm(gain, add(right.smoothF, left.f.map(v => v * step.phi), -1), d, d, g));
      left.smoothP = add(left.p, sandwich(gain, add(right.smoothP, predicted, -1), d, d));
    }
  }
  query(time: number): GPQuery {
    if (time < this.first - 1e-10) throw new Error('Query precedes municipality initialization');
    const exact = this.nodes.find(n => Math.abs(n.time - time) < 1e-10);
    if (exact) return { u: exact.smoothU, f: exact.smoothF, p: exact.smoothP, filteredP: exact.p };
    let left = this.nodes[0], right: GaussianNode | undefined;
    for (const node of this.nodes) { if (node.time < time) left = node; else { right = node; break; } }
    const step = ouStep(time - left.time, this.kappa), u = left.u.map(v => v * step.phi), f = left.f.map(v => v * step.phi);
    const p = add(left.p.map(v => v * step.phi ** 2), this.q, step.varianceTime);
    if (!right) return { u, f, p, filteredP: p };
    const next = ouStep(right.time - time, this.kappa), pred = add(p.map(v => v * next.phi ** 2), this.q, next.varianceTime);
    const gain = mm(p.map(v => v * next.phi), inverse(chol(pred, this.d), this.d), this.d, this.d, this.d);
    return { u: add(u, mv(gain, add(right.smoothU, u.map(v => v * next.phi), -1), this.d)),
      f: add(f, mm(gain, add(right.smoothF, f.map(v => v * next.phi), -1), this.d, this.d, this.g)),
      p: add(p, sandwich(gain, add(right.smoothP, pred, -1), this.d, this.d)), filteredP: p };
  }
  grid(times: number[]) {
    const qs = times.map(t => this.query(t)), gains: Mat[] = [], conditional: Mat[] = [];
    for (let i = 0; i < times.length - 1; i++) {
      const step = ouStep(times[i + 1] - times[i], this.kappa);
      const pred = add(qs[i].filteredP.map(v => v * step.phi ** 2), this.q, step.varianceTime);
      const gain = mm(qs[i].filteredP.map(v => v * step.phi), inverse(chol(pred, this.d), this.d), this.d, this.d, this.d);
      gains.push(gain); conditional.push(add(qs[i].p, sandwich(gain, qs[i + 1].p, this.d, this.d), -1));
    }
    return { queries: qs, gains, conditional };
  }
}
export function addGaussianFactor(a: Mat, b: Mat, m: number, design: Mat, response: Mat, covariance: Mat) {
  const o = response.length, l = chol(covariance, o), ri = inverse(l, o), rt = tr(design, o, m);
  const extra = mm(rt, mm(ri, design, o, o, m), m, o, m), linear = mv(rt, mv(ri, response, o), m);
  for (let i = 0; i < a.length; i++) a[i] += extra[i];
  for (let i = 0; i < m; i++) b[i] += linear[i];
  return -.5 * (logdet(l, o) + response.reduce((n, v, i) => n + v * mv(ri, response, o)[i], 0) + o * Math.log(2 * Math.PI));
}
export function precisionNoise(factor: Mat, normal: () => number, n: number) {
  const out = Float64Array.from({ length: n }, normal);
  for (let i = n - 1; i >= 0; i--) { for (let j = i + 1; j < n; j++) out[i] -= factor[j * n + i] * out[j]; out[i] /= factor[i * n + i]; }
  return out;
}
