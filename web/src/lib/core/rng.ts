export type Rng = () => number;
export type NormalSampler = (mean: number, standardDeviation: number) => number;

function hashSeed(seed: string): number {
  let hash = 2166136261;
  for (let index = 0; index < seed.length; index += 1) {
    hash ^= seed.charCodeAt(index);
    hash = Math.imul(hash, 16777619);
  }
  return hash >>> 0;
}

export function createSeededRng(seed: string | number): Rng {
  let state = typeof seed === 'number' ? seed >>> 0 : hashSeed(seed);

  return () => {
    state += 0x6d2b79f5;
    let value = state;
    value = Math.imul(value ^ (value >>> 15), value | 1);
    value ^= value + Math.imul(value ^ (value >>> 7), value | 61);
    return ((value ^ (value >>> 14)) >>> 0) / 4294967296;
  };
}

export function createNormalSampler(rng: Rng): NormalSampler {
  let spare: number | null = null;

  return (mean: number, standardDeviation: number) => {
    if (standardDeviation === 0) return mean;

    if (spare !== null) {
      const value = spare;
      spare = null;
      return mean + standardDeviation * value;
    }

    let first = 0;
    let second = 0;

    while (first <= Number.EPSILON) first = rng();
    while (second <= Number.EPSILON) second = rng();

    const magnitude = Math.sqrt(-2 * Math.log(first));
    const angle = 2 * Math.PI * second;
    spare = magnitude * Math.sin(angle);

    return mean + standardDeviation * magnitude * Math.cos(angle);
  };
}
