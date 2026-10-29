import { fitRandom } from './random';
export const FORECAST_STREAM_VERSION = 'fast-share-forecast-streams-v1';
export interface ForecastRandom {
  uniform(): number;
  normal(): number;
  integer(n: number): number;
}
/** Positive-term normal integral; avoids subtractive Taylor cancellation and trig. */
export function normalCdf(z: number): number {
  const x = Math.abs(z);
  if (x > 9) return z < 0 ? 0 : 1;
  let term = x,
    total = x;
  for (let n = 1; n < 250; n++) {
    term *= (x * x) / (2 * n + 1);
    total += term;
    if (term <= total * 1e-16) break;
  }
  const half = (Math.exp((-x * x) / 2) * total) / Math.sqrt(2 * Math.PI);
  return Math.max(0, Math.min(1, 0.5 + (z < 0 ? -half : half)));
}
/** Gaussian copula allocates one marginal innovation budget. No extra shock.
 * Every scalar counter is independent within a chamber, including adaptive
 * gamma rejection. The profile correlation is latent, not Beta/count correlation. */
export function forecastRandom(
  seed: string,
  simulationId: number,
  entity: string,
  chamber: 'camera' | 'senato' = 'camera',
  correlation = 0
): ForecastRandom {
  if (
    !Number.isSafeInteger(simulationId) ||
    simulationId < 0 ||
    !Number.isFinite(correlation) ||
    Math.abs(correlation) > 1
  )
    throw new RangeError('Invalid forecast stream');
  const key = JSON.stringify([FORECAST_STREAM_VERSION, seed, simulationId]);
  const streams = new Map<string, ReturnType<typeof fitRandom>>();
  const stream = (name: string) => {
    let value = streams.get(name);
    if (!value) {
      value = fitRandom(key, entity + ':' + name);
      streams.set(name, value);
    }
    return value;
  };
  const draw = (kind: 'normal' | 'uniform') => {
    const common = stream(kind + ':common').normal();
    return chamber === 'camera'
      ? common
      : correlation * common + Math.sqrt(1 - correlation ** 2) * stream(kind + ':residual').normal();
  };
  const uniform = () => Math.max(2 ** -53, Math.min(1 - 2 ** -53, normalCdf(draw('uniform'))));
  return {
    uniform,
    normal: () => draw('normal'),
    integer: (n) => {
      if (!Number.isSafeInteger(n) || n < 1) throw new RangeError('Invalid random integer bound');
      return Math.floor(uniform() * n);
    }
  };
}
function logGammaDraw(shape: number, rng: ForecastRandom): number {
  if (!(shape > 0) || !Number.isFinite(shape)) throw new RangeError('Invalid Gamma shape');
  if (shape < 1) return logGammaDraw(shape + 1, rng) + Math.log(rng.uniform()) / shape;
  const d = shape - 1 / 3,
    c = 1 / Math.sqrt(9 * d);
  for (let i = 0; i < 100000; i++) {
    const z = rng.normal(),
      t = 1 + c * z;
    if (t <= 0) continue;
    const v = t * t * t,
      u = rng.uniform();
    if (u < 1 - 0.0331 * z ** 4 || Math.log(u) < (z * z) / 2 + d * (1 - v + Math.log(v)))
      return Math.log(d) + Math.log(v);
  }
  throw new Error('GAMMA_SAMPLING_FAILED');
}
export function betaDraw(a: number, b: number, rng: ForecastRandom): number {
  const x = logGammaDraw(a, rng),
    y = logGammaDraw(b, rng),
    difference = y - x;
  return difference > 0
    ? Math.exp(-difference) / (1 + Math.exp(-difference))
    : 1 / (1 + Math.exp(difference));
}
