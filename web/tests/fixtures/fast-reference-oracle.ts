/** Direct normal tensor integration, independent of the production Laplace
 * factorization. Deliberately reduced to at most three stochastic dimensions. */
import { legendre } from '../../src/lib/politics/reform-2026/vote-share/reference-family';
export function tensorReferenceMoments(mu: number[], sigma: number[], order: number) {
  const g = legendre(order),
    mean = mu.map(() => 0),
    second = mu.map(() => 0),
    x = mu.map(() => 0);
  const visit = (i: number, weight: number) => {
    if (i === mu.length) {
      const maximum = Math.max(...x),
        a = x.map((v) => Math.exp(v - maximum)),
        total = a.reduce((s, v) => s + v, 0);
      a.forEach((v, j) => {
        mean[j] += (weight * v) / total;
        second[j] += weight * (v / total) ** 2;
      });
      return;
    }
    if (sigma[i] === 0) {
      x[i] = mu[i];
      visit(i + 1, weight);
      return;
    }
    for (let k = 0; k < order; k++) {
      const z = 9 * g.nodes[k];
      x[i] = mu[i] + sigma[i] * z;
      visit(i + 1, (weight * g.weights[k] * 9 * Math.exp((-z * z) / 2)) / Math.sqrt(2 * Math.PI));
    }
  };
  visit(0, 1);
  return { mean, sd: mean.map((m, i) => Math.sqrt(Math.max(0, second[i] - m * m))) };
}
export const referenceMomentFixtures = [
  { id: 'single-partial', means: [0.3], sd: [2], free: true },
  { id: 'two-partial', means: [0.3, 0.4], sd: [2, 3], free: true },
  { id: 'two-closed', means: [0.4, 0.6], sd: [2, 2], free: false },
  { id: 'three-closed', means: [0.2, 0.3, 0.5], sd: [2, 3, 4], free: false }
];
