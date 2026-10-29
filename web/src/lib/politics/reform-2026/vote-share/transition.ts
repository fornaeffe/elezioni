import { betaDraw, type ForecastRandom } from './forecast-random';
import { clip, sum } from './types';
/** Selected finite-step conditional Beta-stick martingale. Random ordering is
 * over canonical component IDs supplied by the caller, never presentation rows. */
export function betaTransition(
  input: ArrayLike<number>,
  scales: ArrayLike<number>,
  horizon: number,
  rng: ForecastRandom
): Float64Array {
  const p = Float64Array.from(input),
    n = p.length;
  if (
    !n ||
    scales.length !== n ||
    !Number.isFinite(horizon) ||
    horizon < 0 ||
    p.some((x) => !Number.isFinite(x) || x < 0) ||
    Array.from(scales).some((x) => !Number.isFinite(x) || x < 0) ||
    Math.abs(sum(p) - 1) > 1e-12
  )
    throw new RangeError('Invalid Beta transition');
  if (!horizon || !Array.from(scales).some((x) => x > 0)) return p;
  const steps = Math.ceil(horizon / 0.25),
    dt = horizon / steps;
  for (let step = 0; step < steps; step++) {
    const order = Array.from({ length: n }, (_, i) => i).filter((i) => p[i] > 0);
    for (let i = order.length - 1; i > 0; i--) {
      const j = rng.integer(i + 1);
      [order[i], order[j]] = [order[j], order[i]];
    }
    let oldRemaining = 1,
      newRemaining = 1;
    const next = new Float64Array(n);
    for (let i = 0; i < order.length - 1; i++) {
      const j = order[i],
        mean = oldRemaining > 0 ? Math.max(0, Math.min(1, p[j] / oldRemaining)) : 0;
      let stick = mean;
      if (mean > 0 && mean < 1 && scales[j] > 0) {
        const m = clip(mean, 1e-12, 1 - 1e-12),
          varianceControl = scales[j] ** 2 * dt * m * (1 - m),
          concentration = Math.max(2, 1 / Math.max(varianceControl, 1e-15) - 1);
        stick = betaDraw(m * concentration, (1 - m) * concentration, rng);
      }
      next[j] = newRemaining * stick;
      newRemaining *= 1 - stick;
      oldRemaining = Math.max(0, oldRemaining - p[j]);
    }
    next[order[order.length - 1]] = newRemaining;
    p.set(next);
  }
  return p;
}
export function movementScales(political: number, nonValid: number, rng: ForecastRandom) {
  if (![political, nonValid].every((x) => Number.isFinite(x) && x >= 0))
    throw new RangeError('Invalid movement scale');
  return {
    political: clip(political * Math.exp(0.2 * rng.normal()), 0.06, 1),
    nonValid: clip(nonValid * Math.exp(0.2 * rng.normal()), 0.025, 0.7)
  };
}
