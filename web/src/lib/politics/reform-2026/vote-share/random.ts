import { FIT_STREAM_VERSION } from './types';

/** Independently addressed sfc32 state; every addition wraps at 32 bits. */
export function fitRandom(seed: string, entity: string) {
  const text = JSON.stringify([FIT_STREAM_VERSION, seed, entity]);
  const states = Array.from({ length: 4 }, (_, lane) => {
    let h = (2166136261 ^ Math.imul(lane + 1, 0x9e3779b9)) >>> 0;
    for (let i = 0; i < text.length; i++) h = Math.imul(h ^ text.charCodeAt(i), 16777619) >>> 0;
    return h;
  });
  let [a, b, c, d] = states, spare: number | undefined;
  const uint = () => {
    let t = (a + b) | 0; a = b ^ (b >>> 9); b = (c + (c << 3)) | 0;
    c = (c << 21) | (c >>> 11); d = (d + 1) | 0; t = (t + d) | 0; c = (c + t) | 0;
    return t >>> 0;
  };
  for (let i = 0; i < 20; i++) uint();
  // 52 random bits plus a half-unit stay strictly inside (0, 1) in binary64.
  const uniform = () => ((uint() >>> 6) * 67108864 + (uint() >>> 6) + 0.5) / 4503599627370496;
  const integer = (n: number) => {
    const ceiling = Math.floor(4294967296 / n) * n;
    let value; do { value = uint(); } while (value >= ceiling);
    return value % n;
  };
  const normal = () => {
    if (spare !== undefined) { const value = spare; spare = undefined; return value; }
    // Marsaglia polar form avoids runtime-dependent native trig rounding.
    for (;;) {
      const x = 2 * uniform() - 1, y = 2 * uniform() - 1, square = x * x + y * y;
      if (!(square > 0 && square < 1)) continue;
      const factor = Math.sqrt(-2 * Math.log(square) / square);
      spare = y * factor; return x * factor;
    }
  };
  /** Robert-style normal/tail/uniform rejection; no inverse-CDF tail cancellation. */
  const truncatedNormal = (a: number, b: number): number => {
    if (!(a < b)) throw new RangeError('Empty Gaussian conditional');
    if (b < 0) return -truncatedNormal(-b, -a);
    for (let proposals = 0; proposals < 1000000; proposals++) {
      if (a > 0) {
        if ((b - a) * Math.max(1, a) < 1.5) {
          const z = a + (b - a) * uniform();
          if (Math.log(uniform()) <= -0.5 * (z - a) * (z + a)) return z;
        } else {
          const rate = a / 2 + Math.hypot(a, 2) / 2, z = a - Math.log(uniform()) / rate;
          if (z <= b && Math.log(uniform()) <= -0.5 * (z - rate) ** 2) return z;
        }
      } else if (b - a < 1.5) {
        const z = a + (b - a) * uniform();
        if (Math.log(uniform()) <= -0.5 * z * z) return z;
      } else { const z = normal(); if (z >= a && z <= b) return z; }
    }
    throw new Error('TRUNCATED_NORMAL_FAILED: rejection budget exhausted');
  };
  return { uniform, integer, normal, truncatedNormal };
}
