import { expect, it } from 'vitest';
import { structuredHash } from './structured-hash';
import { retainedBytes, ownedBuffers } from '../politics/reform-2026/vote-share/fit-resources';

it('binds transferred columns and ordered maps without JSON numeric expansion or ignoring aliases', async () => {
  const a = new Float64Array([0, 1, 2]);
  const graph = { column: a, map: new Map([['same', a]]) };
  const original = await structuredHash(graph), copy = structuredClone(graph);
  expect(ownedBuffers(copy)).toHaveLength(1);
  const moved = structuredClone(copy, { transfer: ownedBuffers(copy) });
  expect(await structuredHash(moved)).toBe(original);
  moved.map.get('same')![1] = 3;
  expect(await structuredHash(moved)).not.toBe(original);
  expect(retainedBytes(graph)).toBeGreaterThan(retainedBytes({ column: a }));
  expect(await structuredHash({ z: 1, a: 2 })).toBe(await structuredHash({ a: 2, z: 1 }));
});
