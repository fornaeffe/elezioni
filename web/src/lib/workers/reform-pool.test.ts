import { expect, it, vi } from 'vitest';
import { runOrderedReformPool, type ReformComputeConnection } from './reform-pool';
import type { ReformRunReady } from './reform-compute';

const ready = (simulation: number): ReformRunReady => ({ simulation, tables: [],
  counts: { complete: 1, partial: 0, failed: 0, awarded: 0 }, timings: { generationMs: 1, scrutinyMs: 1, presentationMs: 1 } });

it('bounds outstanding runs and commits in global order despite out-of-order completion', async () => {
  const assigned: number[] = [], computed: number[] = [], committed: number[] = [];
  let outstanding = 0, peak = 0;
  const workers: ReformComputeConnection[] = Array.from({ length: 3 }, () => ({
    async compute(simulation) {
      assigned.push(simulation); peak = Math.max(peak, ++outstanding);
      await new Promise(resolve => setTimeout(resolve, simulation === 1 ? 20 : 1));
      computed.push(simulation); return ready(simulation);
    },
    async pull() { return null; }, close: vi.fn()
  }));
  await runOrderedReformPool(workers, 12, () => false, async simulation => {
    // A slow commit is also backpressure, not permission to fill a result queue.
    await new Promise(resolve => setTimeout(resolve, 1));
    committed.push(simulation); outstanding--;
  });
  expect(assigned).toEqual(Array.from({ length: 12 }, (_, index) => index + 1));
  expect(committed).toEqual(assigned); expect(computed[0]).not.toBe(1);
  expect(peak).toBe(3); expect(outstanding).toBe(0);
  for (const worker of workers) expect(worker.close).toHaveBeenCalledOnce();
});

it('cancellation finishes only the already assigned prefix', async () => {
  let cancelled = false;
  const assigned: number[] = [], committed: number[] = [];
  const workers: ReformComputeConnection[] = Array.from({ length: 3 }, () => ({
    async compute(simulation) { assigned.push(simulation); return ready(simulation); },
    async pull() { return null; }, close: vi.fn()
  }));
  await runOrderedReformPool(workers, 100, () => cancelled, async simulation => { committed.push(simulation); cancelled = true; });
  expect(assigned).toEqual([1, 2, 3]); expect(committed).toEqual(assigned);
});

it.each(['compute', 'commit', 'identity'])('interrupts on %s failure and closes every worker without retrying', async mode => {
  const assigned: number[] = [], committed: number[] = [];
  const workers: ReformComputeConnection[] = Array.from({ length: 2 }, () => ({
    async compute(simulation) {
      assigned.push(simulation);
      if (mode === 'compute' && simulation === 2) throw new Error('Worker crashed');
      return ready(mode === 'identity' ? simulation + 1 : simulation);
    },
    async pull() { return null; }, close: vi.fn()
  }));
  await expect(runOrderedReformPool(workers, 4, () => false, async simulation => {
    if (mode === 'commit') throw new Error('Quota exceeded');
    committed.push(simulation);
  })).rejects.toThrow();
  expect(new Set(assigned).size).toBe(assigned.length);
  expect(committed).toEqual(mode === 'compute' ? [1] : []);
  for (const worker of workers) expect(worker.close).toHaveBeenCalledOnce();
});
