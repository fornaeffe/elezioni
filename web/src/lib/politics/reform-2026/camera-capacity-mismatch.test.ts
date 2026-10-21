import { describe, expect, it } from 'vitest';
import { allocateTerritorialMatrix, type TerritorialMatrixInput } from './territorial-matrix';
import { exactFraction } from './exact';
import { createSeededDrawContext } from './ties';

function fixture(targets = [5, 1], votes = [[400, 100]], capacities = [4]): TerritorialMatrixInput {
  const ids = ['majority', 'minority'];
  return { chamber: 'camera', simulationId: 15, norm: 'N18', contextId: 'camera/I15/groups',
    lawReference: 'C83-bis(1-bis)(a)-(c)', correction: 'college-groups', initialTieFigure: 'constituency-electoral-figure',
    targets: ids.map((id, i) => ({ id, seats: targets[i], votes: votes.reduce((n, v) => n + v[i], 0) })),
    areas: votes.map((v, i) => ({ id: `P${i}`, seats: capacities[i], entries: ids.map((id, j) => ({ id, votes: v[j], tieVotes: 0 })) })),
    indexQuotas: ids.map(id => ({ id, quota: exactFraction(100) })) };
}
const run = (input: TerritorialMatrixInput) => allocateTerritorialMatrix(input, createSeededDrawContext('I15-replay', 'legal'));
function complete(input: TerritorialMatrixInput) {
  const result = run(input);
  expect(result.status).toBe('complete');
  if (result.status !== 'complete') throw new Error(result.reason);
  for (const t of input.targets) expect(result.cells.filter(c => c.targetId === t.id).reduce((n, c) => n + c.seats, 0)).toBe(t.seats);
  expect(result.cells.every(c => Number.isSafeInteger(c.seats) && c.seats >= 0)).toBe(true);
  return result;
}

describe('I15 Camera cap-group capacity mismatch', () => {
  it('adds by unused decimals before used decimals without changing their values or initial evidence', () => {
    const result = complete(fixture());
    expect(result.areas).toEqual([expect.objectContaining({ capacity: 4, finalSeats: 6 })]);
    expect(result.transfers.map(t => [t.route, t.remainders[0].before, t.remainders[0].after]))
      .toEqual([['addition', 'unused', 'used'], ['addition', 'used', 'used']]);
    expect(result.transfers.every(t => t.fromCellId === null && t.interpretationId === 'I15')).toBe(true);
    expect(result.cells.find(c => c.targetId === 'majority')).toMatchObject({ integerSeats: 3, provisionalSeats: 3,
      decimalPart: exactFraction(1, 5), seats: 5, remainderState: 'used' });
    expect(result.warnings.filter(w => w.interpretationId === 'I15')).toHaveLength(1);
    expect(JSON.parse(JSON.stringify(result))).toEqual(result);
  });

  it('removes the smallest used group remainders without reassigning seats', () => {
    const result = complete(fixture([3, 2], [[180, 120], [170, 130]], [3, 3]));
    expect(result.transfers).toMatchObject([{ fromCellId: '["P1","majority"]', toCellId: null,
      route: 'removal', interpretationId: 'I15', remainders: [{ before: 'used', after: 'consumed' }] }]);
    expect(result.areas.map(a => a.finalSeats)).toEqual([3, 2]);
  });

  it('exhausts the largest unused decimals across groups before ranking used decimals', () => {
    const result = complete(fixture([8, 1], [[400, 100], [450, 50]], [2, 2]));
    expect(result.transfers.map(t => [t.toCellId, t.remainders[0].before])).toEqual([
      ['["P0","minority"]', 'unused'], ['["P1","majority"]', 'used'], ['["P0","majority"]', 'used'],
      ['["P1","majority"]', 'used'], ['["P0","majority"]', 'used']]);
  });

  it('retains I13 integer donor fallback and preserves its remainder state', () => {
    const result = complete(fixture([1, 1], [[400, 100]], [5]));
    expect(result.transfers).toHaveLength(3);
    expect(result.transfers.every(t => t.interpretationId === 'I13' && t.toCellId === null)).toBe(true);
    expect(result.cells.find(c => c.targetId === 'majority')).toMatchObject({ integerSeats: 4, seats: 1, remainderState: 'unused' });
    expect(result.warnings.filter(w => w.interpretationId === 'I13')).toHaveLength(3);
  });

  it('I15-v2 repeats used-decimal passes without a reuse limit', () => {
    const result = complete(fixture([9, 1]));
    expect(result.transfers).toHaveLength(6);
    expect(result.transfers.slice(1).every(t => t.remainders[0].before === 'used' && t.remainders[0].after === 'used')).toBe(true);
    expect(result.cells.find(c => c.targetId === 'majority')).toMatchObject({ integerSeats: 3, provisionalSeats: 3, seats: 9 });
    expect(result.warnings.some(w => w.interpretationId === 'I15')).toBe(true);
  });

  it('handles simultaneous surplus and deficit groups independently', () => {
    const result = complete(fixture([1, 5]));
    expect(result.transfers.map(t => t.route)).toEqual(['removal', 'removal', 'addition', 'addition', 'addition', 'addition']);
    expect(result.transfers.at(-1)?.remainders[0]).toMatchObject({ before: 'used', after: 'used' });
  });

  it('replays I03 ties with identical traces after target, college and figure permutations', () => {
    const input = fixture([2, 1], [[180, 120], [180, 120]], [2, 2]);
    const result = complete(input);
    expect(result.draws.some(d => d.rule === 'I03')).toBe(true);
    expect(complete({ ...input, targets: [...input.targets].reverse(), indexQuotas: [...input.indexQuotas!].reverse(),
      areas: [...input.areas].reverse().map(a => ({ ...a, entries: [...a.entries].reverse() })) })).toEqual(result);
  });

  it('does not change balanced Camera groups or authorize Senate capacity changes', () => {
    const balanced = fixture([4, 2], [[180, 120], [170, 130]], [3, 3]);
    expect(complete(balanced).warnings.some(w => w.interpretationId === 'I15')).toBe(false);
    expect(() => run({ ...fixture(), chamber: 'senato', norm: 'N22' })).toThrow(/capacities must sum/);
    expect(() => run({ ...fixture(), norm: 'N17' })).toThrow(/capacities must sum/);
    expect(() => run({ ...fixture(), correction: 'colleges' })).toThrow(/capacities must sum/);
  });

  it('retains undefined positive-target group quotas as explicit failures', () => {
    const input = fixture();
    expect(run({ ...input, indexQuotas: input.indexQuotas!.map(q => ({ ...q, quota: null })) }))
      .toMatchObject({ status: 'non-computable', reason: 'zero-index-quota' });
  });
});
