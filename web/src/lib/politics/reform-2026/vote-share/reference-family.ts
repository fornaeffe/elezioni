import { sum } from './types';
import type { ForecastRandom } from './forecast-random';
export const REFERENCE_POLICY = 'fixed-turnout-reference-v1';
export const REFERENCE_CALIBRATION = 'independent-logweights-laplace-quadrature-v1';
export interface ReferenceMoment {
  id: string;
  members: string[];
  mean: number;
  sdPercentagePoints: number;
}
export interface ReferenceFamily {
  version: typeof REFERENCE_CALIBRATION;
  groups: ReferenceMoment[];
  free: boolean;
  upper: number;
  uncertain: number[];
  mu: number[];
  sigma: number[];
  residualCategory: boolean;
  integrationError: number;
  momentError: number;
  iterations: number;
  normalization: number | null;
}
const cache = new Map<number, { nodes: number[]; weights: number[] }>();
/** Legendre integration on [-1,1], constructed independently of the model. */
export function legendre(order: number) {
  const saved = cache.get(order);
  if (saved) return saved;
  const nodes = Array(order).fill(0),
    weights = Array(order).fill(0);
  for (let i = 0; i < Math.ceil(order / 2); i++) {
    let x = Math.cos((Math.PI * (i + 0.75)) / (order + 0.5)),
      derivative = 0;
    for (let iteration = 0; iteration < 30; iteration++) {
      let a = 1,
        b = x;
      for (let k = 2; k <= order; k++) {
        const next = ((2 * k - 1) * x * b - (k - 1) * a) / k;
        a = b;
        b = next;
      }
      derivative = (order * (x * b - a)) / (x * x - 1);
      const next = x - b / derivative;
      if (Math.abs(next - x) < 2e-16) {
        x = next;
        break;
      }
      x = next;
    }
    nodes[i] = -x;
    nodes[order - 1 - i] = x;
    weights[i] = weights[order - 1 - i] = 2 / ((1 - x * x) * derivative * derivative);
  }
  const value = { nodes, weights };
  cache.set(order, value);
  return value;
}
/** E[X_i/S] and E[(X_i/S)^2] via the positive Laplace identities
 * 1/S=int exp(-tS)dt and 1/S^2=int t exp(-tS)dt. Independent
 * logweights factor the integrand, so dimension does not cause a tensor grid.
 * z is truncated at +/-9 (omitted normal mass < 3e-19 per category). */
export function logisticNormalMoments(
  mu: readonly number[],
  sigma: readonly number[],
  order = 64,
  outerOrder = 256
) {
  const n = mu.length,
    gaussian = legendre(order),
    outer = legendre(outerOrder),
    centre = Math.max(...mu);
  const means = mu.map((m) => m - centre),
    samples = means.map((m, i) =>
      sigma[i] === 0 ? [Math.exp(m)] : gaussian.nodes.map((z) => Math.exp(m + 9 * sigma[i] * z))
    );
  const weights = sigma.map((s) =>
    s === 0
      ? [1]
      : gaussian.nodes.map(
          (z, i) => (gaussian.weights[i] * 9 * Math.exp(-0.5 * (9 * z) ** 2)) / Math.sqrt(2 * Math.PI)
        )
  );
  const low = -Math.max(...means.map((m, i) => m + 12 * sigma[i])) - 32,
    high = -Math.max(...means.map((m, i) => m - 12 * sigma[i])) + 32;
  const mean = new Float64Array(n),
    second = new Float64Array(n),
    l0 = new Float64Array(n),
    l1 = new Float64Array(n),
    l2 = new Float64Array(n);
  for (let u = 0; u < outer.nodes.length; u++) {
    const v = low + ((outer.nodes[u] + 1) * (high - low)) / 2,
      t = Math.exp(v),
      weight = (outer.weights[u] * (high - low)) / 2;
    let logProduct = 0,
      alive = true;
    for (let i = 0; i < n; i++) {
      let a = 0,
        b = 0,
        c = 0;
      for (let k = 0; k < samples[i].length; k++) {
        const x = samples[i][k],
          w = weights[i][k] * Math.exp(-t * x);
        a += w;
        b += w * x;
        c += w * x * x;
      }
      l0[i] = a;
      l1[i] = b;
      l2[i] = c;
      if (!(a > 0)) {
        alive = false;
        break;
      }
      logProduct += Math.log(a);
    }
    if (!alive) continue;
    for (let i = 0; i < n; i++) {
      const productOthers = Math.exp(logProduct - Math.log(l0[i]));
      mean[i] += weight * t * productOthers * l1[i];
      second[i] += weight * t * t * productOthers * l2[i];
    }
  }
  return { mean: [...mean], sd: [...mean].map((m, i) => Math.sqrt(Math.max(0, second[i] - m * m))) };
}
function linearSolve(a: number[][], b: number[]) {
  const n = b.length,
    rows = a.map((r, i) => [...r, b[i]]);
  for (let k = 0; k < n; k++) {
    let pivot = k;
    for (let i = k + 1; i < n; i++) if (Math.abs(rows[i][k]) > Math.abs(rows[pivot][k])) pivot = i;
    if (Math.abs(rows[pivot][k]) < 1e-13) throw new Error('Singular reference moment equations');
    [rows[k], rows[pivot]] = [rows[pivot], rows[k]];
    const d = rows[k][k];
    for (let j = k; j <= n; j++) rows[k][j] /= d;
    for (let i = 0; i < n; i++)
      if (i !== k) {
        const f = rows[i][k];
        for (let j = k; j <= n; j++) rows[i][j] -= f * rows[k][j];
      }
  }
  return rows.map((r) => r[n]);
}
export function calibrateReferences(
  input: ReferenceMoment[],
  free: boolean,
  normalizeFullCentres = false
): ReferenceFamily {
  const groups = structuredClone(input).sort((a, b) => a.id.localeCompare(b.id)),
    ids = groups.flatMap((g) => g.members);
  if (new Set(groups.map((g) => g.id)).size !== groups.length || new Set(ids).size !== ids.length)
    throw new Error('REFERENCE_OVERLAP');
  if (
    groups.some(
      (g) =>
        !g.id ||
        !g.members.length ||
        !Number.isFinite(g.mean) ||
        g.mean < 0 ||
        !Number.isFinite(g.sdPercentagePoints) ||
        g.sdPercentagePoints < 0
    )
  )
    throw new Error('REFERENCE_CONFLICT');
  let normalization: number | null = null,
    mass = sum(groups.map((g) => g.mean));
  if (!free && normalizeFullCentres && Math.abs(mass - 1) > 1e-12) {
    if (!(mass > 0)) throw new Error('REFERENCE_CONFLICT: zero full reference total');
    normalization = mass;
    for (const g of groups) {
      g.mean /= mass;
      g.sdPercentagePoints /= mass;
    }
    mass = 1;
  }
  if (mass > 1 + 1e-12 || (!free && Math.abs(mass - 1) > 1e-12))
    throw new Error('REFERENCE_CONFLICT: total mean');
  const uncertain = groups.flatMap((g, i) => (g.sdPercentagePoints > 0 ? [i] : [])),
    upper = 1 - sum(groups.filter((g) => !g.sdPercentagePoints).map((g) => g.mean));
  const residualCategory = free && mass < 1 - 1e-12,
    n = uncertain.length;
  const failure = (detail: string): never => {
    throw new Error(
      'REFERENCE_MOMENTS_UNSUPPORTED: ' +
        detail +
        '; ' +
        groups.map((g) => g.id + '=' + g.mean + '+/-' + g.sdPercentagePoints + 'pp').join(', ')
    );
  };
  const result = (
    mu: number[],
    sigma: number[],
    integrationError: number,
    momentError: number,
    iterations: number
  ): ReferenceFamily => ({
    version: REFERENCE_CALIBRATION,
    groups,
    free,
    upper,
    uncertain,
    mu,
    sigma,
    residualCategory,
    integrationError,
    momentError,
    iterations,
    normalization
  });
  if (!n) return result([], [], 0, 0, 0);
  if (n > 16)
    throw new Error(
      'REFERENCE_CALIBRATION_RESOURCE_LIMIT: at most 16 simultaneous uncertain categories in numerical v1'
    );
  const means = uncertain.map((i) => groups[i].mean / upper),
    sd = uncertain.map((i) => groups[i].sdPercentagePoints / 100 / upper);
  if (!(upper > 0) || means.some((m, i) => !(m > 0 && m < 1) || !(sd[i] < Math.sqrt(m * (1 - m)))))
    return failure('boundary/variance bound');
  if (!residualCategory && (n < 2 || Math.max(...sd) > sum(sd) - Math.max(...sd) + 1e-12))
    return failure('closed-simplex variance consistency');
  // With two closed categories only their logweight difference is observable.
  // Symmetric variances achieve the minimum total variance and a stable tie.
  if (!residualCategory && n === 2) {
    if (Math.abs(sd[0] - sd[1]) > 1e-12) return failure('complementary SDs differ');
    const inner = calibrateReferences(
      [{ id: 'anchor', members: ['anchor'], mean: means[0], sdPercentagePoints: sd[0] * 100 }],
      true
    );
    const mu = [inner.mu[0] / 2, -inner.mu[0] / 2],
      sigma = [inner.sigma[0] / Math.sqrt(2), inner.sigma[0] / Math.sqrt(2)];
    return result(mu, sigma, inner.integrationError, inner.momentError * upper, inner.iterations);
  }
  const count = n + Number(residualCategory),
    meanParameters = residualCategory ? n : n - 1;
  const residualMean = 1 - sum(means),
    initial = means.map((m) => Math.log(m / (residualCategory ? residualMean : means[n - 1])));
  const initialParameters = [
    ...initial.slice(0, meanParameters),
    ...sd.map((d, i) => Math.log(Math.max(1e-10, d / (means[i] * (1 - means[i])))))
  ];
  const unpack = (p: number[]) => {
    const mu = [...p.slice(0, meanParameters), 0],
      sigma = p.slice(meanParameters).map(Math.exp);
    if (!residualCategory) {
      const avg = sum(mu) / n;
      for (let i = 0; i < mu.length; i++) mu[i] -= avg;
    }
    if (residualCategory) sigma.push(0);
    return { mu, sigma };
  };
  const evaluate = (p: number[], order = 48, outer = 320) => {
    const values = unpack(p),
      moments = logisticNormalMoments(values.mu, values.sigma, order, outer);
    return {
      ...values,
      moments,
      error: [
        ...moments.mean.slice(0, meanParameters).map((m, i) => m - means[i]),
        ...moments.sd.slice(0, n).map((s, i) => s - sd[i])
      ]
    };
  };
  const solve = (startScale: number) => {
    let parameters = initialParameters.map((p, i) => (i < meanParameters ? p : p + Math.log(startScale))),
      iterations = 0;
    for (; iterations < 35; iterations++) {
      const base = evaluate(parameters),
        norm = Math.max(...base.error.map(Math.abs));
      if (norm < 2e-9) break;
      const jacobian = Array.from({ length: parameters.length }, () => Array(parameters.length).fill(0));
      for (let k = 0; k < parameters.length; k++) {
        const step = 1e-4,
          plus = [...parameters],
          minus = [...parameters];
        plus[k] += step;
        minus[k] -= step;
        const a = evaluate(plus).error,
          b = evaluate(minus).error;
        for (let i = 0; i < parameters.length; i++) jacobian[i][k] = (a[i] - b[i]) / (2 * step);
      }
      const move = linearSolve(
        jacobian,
        base.error.map((x) => -x)
      );
      let accepted = false;
      for (let scale = 1; scale >= 1 / 1024; scale /= 2) {
        const next = parameters.map((p, k) =>
          Math.max(k < meanParameters ? -35 : -25, Math.min(k < meanParameters ? 35 : 2, p + scale * move[k]))
        );
        if (Math.max(...evaluate(next).error.map(Math.abs)) < norm) {
          parameters = next;
          accepted = true;
          break;
        }
      }
      if (!accepted) break;
    }
    const a = evaluate(parameters, 80, 320),
      b = evaluate(parameters, 112, 448);
    const integrationError =
      Math.max(
        ...a.moments.mean.map((x, i) => Math.abs(x - b.moments.mean[i])),
        ...a.moments.sd.map((x, i) => Math.abs(x - b.moments.sd[i]))
      ) * upper;
    const momentError =
      Math.max(
        ...b.moments.mean.slice(0, n).map((m, i) => Math.abs(m - means[i])),
        ...b.moments.sd.slice(0, n).map((s, i) => Math.abs(s - sd[i]))
      ) * upper;
    if (integrationError > 1e-7 || momentError > 1e-6)
      return failure('moment/refinement gate ' + momentError + '/' + integrationError);
    if (b.mu.length !== count) return failure('category ordering');
    return result(b.mu, b.sigma, integrationError, momentError, iterations);
  };
  // Closed score families can have multiple numerical roots. Compare fixed,
  // deterministic starts by the contract objectives, then canonical ID order.
  // Numerical ties retain the first start rather than amplifying roundoff.
  const candidates: ReferenceFamily[] = [],
    errors: string[] = [];
  for (const scale of residualCategory ? [1] : [1, 0.5, 1.5, 2])
    try {
      candidates.push(solve(scale));
    } catch (e) {
      errors.push(e instanceof Error ? e.message : String(e));
    }
  if (!candidates.length) return failure(errors[0] ?? 'No calibrated root');
  const objective = (f: ReferenceFamily) => [
    sum(f.sigma.map((x) => x * x)),
    sum(f.mu.map((x) => x * x)),
    ...f.sigma,
    ...f.mu
  ];
  candidates.sort((a, b) => {
    const x = objective(a),
      y = objective(b);
    for (let i = 0; i < x.length; i++) if (Math.abs(x[i] - y[i]) > 1e-7) return x[i] - y[i];
    return 0;
  });
  return candidates[0];
}
export function drawReference(family: ReferenceFamily, rng: ForecastRandom, central = false) {
  const values = family.groups.map((g) => g.mean);
  if (!central && family.uncertain.length) {
    const scores = family.mu.map((m, i) => m + family.sigma[i] * rng.normal()),
      maximum = Math.max(...scores),
      weights = scores.map((s) => Math.exp(s - maximum)),
      total = sum(weights);
    family.uncertain.forEach((index, i) => {
      values[index] = (family.upper * weights[i]) / total;
    });
  }
  const residual = 1 - sum(values);
  if (residual < -1e-12 || (!family.free && Math.abs(residual) > 1e-12))
    throw new Error('REFERENCE_CONFLICT: drawn simplex');
  return { values, residual: Math.max(0, residual) };
}
