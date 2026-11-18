import { describe, expect, it } from 'vitest';
import { reconcilePopulation } from './population';
import { createSeededDrawContext } from './ties';

const run = (population: number, parts: Parameters<typeof reconcilePopulation>[1]) =>
  reconcilePopulation(population, parts, createSeededDrawContext('population-fixture', 'N03-estimate'), 'municipality-fixture');

describe('census reconstruction with explicit estimates', () => {
  it('uses complete observed parts even when historical weights differ', () => {
    const result = run(10, [{ id: 'a', observed: 8, historicalWeight: 1 }, { id: 'b', observed: 2, historicalWeight: 9 }]);
    expect(result.populations).toEqual([{ id: 'a', population: 8 }, { id: 'b', population: 2 }]);
    expect(result.estimated).toBe(false);
  });
  it('preserves known observations and estimates only the missing residue', () => {
    const result = run(100, [{ id: 'known', observed: 40, historicalWeight: 1 },
      { id: 'a', observed: null, historicalWeight: 2 }, { id: 'b', observed: null, historicalWeight: 1 }]);
    expect(result.populations).toEqual([{ id: 'known', population: 40 }, { id: 'a', population: 40 }, { id: 'b', population: 20 }]);
    expect(result.estimated).toBe(true);
    expect(result.method).toBe('observed-parts-plus-weighted-residue');
  });
  it('rescales non-reconciling fine census observations to the legal total', () => {
    const result = run(100, [{ id: 'a', observed: 80, historicalWeight: 1 }, { id: 'b', observed: 40, historicalWeight: 1 }]);
    expect(result.populations).toEqual([{ id: 'a', population: 67 }, { id: 'b', population: 33 }]);
    expect(result.estimated).toBe(true);
  });
  it('falls back to historical weights, conserving the total exactly', () => {
    const result = run(11, [{ id: 'a', observed: null, historicalWeight: 2 }, { id: 'b', observed: null, historicalWeight: 1 }]);
    expect(result.populations).toEqual([{ id: 'a', population: 7 }, { id: 'b', population: 4 }]);
    expect(result.estimated).toBe(true);
    expect(run(11, [{ id: 'a', observed: 0, historicalWeight: 2 }, { id: 'b', observed: 0, historicalWeight: 1 }]).populations)
      .toEqual([{ id: 'a', population: 7 }, { id: 'b', population: 4 }]);
  });
  it('keeps an observed zero distinct from a missing observation', () => {
    expect(run(10, [{ id: 'a', observed: 0, historicalWeight: 100 }, { id: 'b', observed: null, historicalWeight: 1 }]).populations)
      .toEqual([{ id: 'a', population: 0 }, { id: 'b', population: 10 }]);
  });
  it('supports zero totals and discloses seeded ties rather than inventing precision', () => {
    const parts = [{ id: 'a', observed: null, historicalWeight: 0 }, { id: 'b', observed: null, historicalWeight: 0 }];
    expect(run(0, parts).populations.every(p => p.population === 0)).toBe(true);
    const result = run(1, parts);
    expect(result).toEqual(run(1, parts));
    expect(result.draws).toHaveLength(1);
    expect(result.warnings[0].interpretationId).toBe('I03');
    expect(result.populations.reduce((s, p) => s + p.population, 0)).toBe(1);
    expect(new Map(result.populations.map(p => [p.id, p.population])))
      .toEqual(new Map(run(1, [...parts].reverse()).populations.map(p => [p.id, p.population])));
  });
  it('rejects malformed counts and ambiguous part identities', () => {
    expect(() => run(10, [{ id: 'a', observed: -1, historicalWeight: 1 }])).toThrow();
    expect(() => run(10, [{ id: 'a', observed: 5, historicalWeight: 1 }, { id: 'a', observed: 5, historicalWeight: 1 }])).toThrow();
    expect(() => run(10.1, [{ id: 'a', observed: null, historicalWeight: 1 }])).toThrow();
  });
});
