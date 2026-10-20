import { describe, expect, it } from 'vitest';
import { estimateReformWorkerCapacity, requireReformWorkerCount } from './reform-worker-capacity';

describe('reform compute resource budget', () => {
  it.each([
    [{ hardwareConcurrency: 28, deviceMemory: 8 }, 2],
    [{ hardwareConcurrency: 28, deviceMemory: 16 }, 5],
    [{ hardwareConcurrency: 2, deviceMemory: 32 }, 1],
    [{ hardwareConcurrency: 8, deviceMemory: 4 }, 1],
    [{ hardwareConcurrency: 28 }, 1],
    [{ deviceMemory: 32 }, 1],
    [{ hardwareConcurrency: 1, deviceMemory: 0.25 }, 1],
    [{ hardwareConcurrency: NaN, deviceMemory: Infinity }, 1]
  ])('uses CPU and memory hints conservatively: %j', (hints, maximum) => {
    expect(estimateReformWorkerCapacity(hints).maximum).toBe(maximum);
  });
  it('requires a choice within the estimated maximum instead of silently reducing it', () => {
    expect(requireReformWorkerCount(2, 2)).toBe(2);
    for (const value of [0, -1, 1.5, 3, NaN, Infinity]) expect(() => requireReformWorkerCount(value, 2)).toThrow(/1 to 2/);
  });
});
