/** Independent deterministic check of the one-city, inseparable a+b fixture.
 * Integrate in aggregate logits u=log(p_ab/p0), v=log(pc/p0), w=log(pa/pb).
 * The map from orthonormal Gaussian contrasts has a CONSTANT Jacobian: u is
 * a linear aggregate score plus log(2 cosh(w/2)). The reference Gaussian density
 * is evaluated at the ORIGINAL component scores, never an inferred count.
 * National+local isotropic Gaussian score covariances add for one municipality.
 * This diagnostic has no temporal/poll/hyperparameter or production role. */
export function groupedReferenceQuadrature(options: { counts: [number, number, number]; offset: number; scoreVariance: number; step: number; splitStep: number }) {
  const [n0, nab, nc] = options.counts, n = n0 + nab + nc;
  if ([n0, nab, nc].some(y => y <= 0) || !(options.scoreVariance > 0) || options.step < .01 || options.splitStep < .01) throw new Error('Unsupported reference quadrature');
  const uc = Math.log(nab / n0), vc = Math.log(nc / n0), maxLikelihood = n0 * Math.log(n0 / n) + nab * Math.log(nab / n) + nc * Math.log(nc / n);
  const sums = Array(5).fill(0); let weight = 0, edgeWeight = 0, evaluations = 0;
  const steps = Math.round(4 / options.step), splitSteps = Math.round(32 / options.splitStep);
  for (let iu = 0; iu <= steps; iu++) for (let iv = 0; iv <= steps; iv++) {
    const u = uc - 2 + 4 * iu / steps, v = vc - 2 + 4 * iv / steps, eu = Math.exp(u), ev = Math.exp(v), z = 1 + eu + ev;
    const p0 = 1 / z, pab = eu / z, pc = ev / z;
    const ll = n0 * Math.log(p0) + nab * Math.log(pab) + nc * Math.log(pc) - maxLikelihood;
    for (let iw = 0; iw <= splitSteps; iw++) {
      const w = -16 + 32 * iw / splitSteps, rho = 1 / (1 + Math.exp(-w));
      const raw = [-options.offset, u + Math.log(rho), u + Math.log1p(-rho), v], centre = raw.reduce((s, x) => s + x, 0) / 4;
      const penalty = raw.reduce((s, x) => s + (x - centre) ** 2, 0) / (2 * options.scoreVariance);
      const q = Math.exp(ll - penalty) * (iu === 0 || iu === steps ? .5 : 1) * (iv === 0 || iv === steps ? .5 : 1) * (iw === 0 || iw === splitSteps ? .5 : 1);
      const values = [p0, pab * rho, pab * (1 - rho), pc, rho];
      values.forEach((x, k) => sums[k] += q * x); weight += q; evaluations++;
      if (iu === 0 || iu === steps || iv === 0 || iv === steps || iw === 0 || iw === splitSteps) edgeWeight += q;
    }
  }
  return { means: sums.map(s => s / weight), boundaryWeightFraction: edgeWeight / weight, evaluations, options };
}
