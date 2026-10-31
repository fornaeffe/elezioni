import { describe, expect, it } from 'vitest';
import { estimateReformWorkerCapacity, requireReformWorkerCount, planReformWorkers, workerMemoryWarning, workerHeapWarning } from './reform-worker-capacity';

describe('reform compute resource budget', () => {
  it.each([
    [{ hardwareConcurrency: 28, deviceMemory: 8 }, 26],
    [{ hardwareConcurrency: 28, deviceMemory: 16 }, 26],
    [{ hardwareConcurrency: 2, deviceMemory: 32 }, 1],
    [{ hardwareConcurrency: 8, deviceMemory: 4 }, 6],
    [{ hardwareConcurrency: 28 }, 26],
    [{ deviceMemory: 32 }, 1],
    [{ hardwareConcurrency: 1, deviceMemory: 0.25 }, 1],
    [{ hardwareConcurrency: NaN, deviceMemory: Infinity }, 1]
  ])('uses CPU and memory hints conservatively: %j', (hints, maximum) => {
    expect(estimateReformWorkerCapacity(hints).maximum).toBe(maximum);
  });
  it('uses half declared memory for defaults without hiding CPU options, and warns at total RAM',()=>{
    const GiB=1024**3,p=planReformWorkers({hardwareConcurrency:28,deviceMemory:8},GiB/2,GiB*1.5);
    expect(p.maximum).toBe(26);expect(p.recommended).toBe(2);
    expect(workerMemoryWarning(4,p)).toBe(false);expect(workerMemoryWarning(5,p)).toBe(true);
    expect(planReformWorkers({hardwareConcurrency:28},GiB/2,GiB).recommended).toBe(1);
  });
  it('requires a choice within the estimated maximum instead of silently reducing it', () => {
    expect(requireReformWorkerCount(2, 2)).toBe(2);
    for (const value of [0, -1, 1.5, 3, NaN, Infinity]) expect(() => requireReformWorkerCount(value, 2)).toThrow(/1 to 2/);
  });
  it('accounts for a renderer heap constraint independently of abundant device RAM', () => {
    const MiB=1024**2, p=planReformWorkers({hardwareConcurrency:28,deviceMemory:64},128*MiB,384*MiB,
      {budgetBytes:3072*MiB,coordinatorBytes:512*MiB,perWorkerBytes:256*MiB});
    expect(p.maximum).toBe(26); expect(p.recommended).toBe(10);
    expect(workerHeapWarning(10,p)).toBe(false);
    expect(workerHeapWarning(26,p)).toBe(true);
    expect(workerMemoryWarning(26,p)).toBe(false);
  });
});
