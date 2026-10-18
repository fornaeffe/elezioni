import { describe, expect, it } from 'vitest';
import { aggregateElectoralFigures, calculatePercentages } from './figures';
import { figureFixture } from './electoral-figures.fixture';
import { exactFraction } from './exact';

describe('N06 disjoint electoral figures', () => {
  it.each(['camera', 'senato'] as const)('aggregates %s colleges, territories, regions and national scopes independently', side => {
    const f = figureFixture(side);
    const result = aggregateElectoralFigures(f.chamber, f.ordinary, f.special);
    expect(result.colleges.map(c => c.figures.validVotes)).toEqual([107, 20, 20, 50, 10, 10]);
    expect(result.territories.map(t => t.figures.validVotes)).toEqual([127, 20, 50, 20]);
    expect(result.regions.map(r => r.figures.validVotes)).toEqual([147, 50, 20]);
    expect(result.national.validVotes).toBe(217);
    expect(result.national.lists.map(l => l.votes)).toEqual([89, 65, 13, 50, 0]);
    expect(result.national.coalitions[0].votes).toBe(167);
    expect(result.ordinaryAllocation.validVotes).toBe(147);
    expect(result.ordinaryAllocation.lists.map(l => l.votes)).toEqual([79, 57, 11, 0, 0]);
    expect(result.taaProportional).toHaveLength(side === 'camera' ? 1 : 0);
    if (side === 'camera') {
      expect(result.taaProportional[0].figures.validVotes).toBe(20);
      expect(result.taaProportional[0].figures.lists.map(l => l.votes)).toEqual([10, 8, 2, 0, 0]);
    }
  });

  it('retains special-only lists, sub-1% coalition votes and all valid denominator votes', () => {
    const f = figureFixture();
    f.ordinary[0] = { ...f.ordinary[0], listVotes: [{ listId: 'A', votes: 1000 }, { listId: 'B', votes: 30 }, { listId: 'X', votes: 1 }] };
    const result = aggregateElectoralFigures(f.chamber, f.ordinary, f.special);
    expect(result.national.validVotes).toBe(1141);
    expect(result.national.coalitions[0].votes).toBe(1091); // includes X's three votes
    expect(result.national.lists.find(l => l.listId === 'S')!.percentage).toEqual({ status: 'defined', value: exactFraction(5000, 1141) });
    expect(result.ordinaryAllocation.lists.find(l => l.listId === 'S')!.votes).toBe(0);
    expect(result.national.lists.find(l => l.listId === 'Z')!.percentage).toEqual({ status: 'defined', value: exactFraction(0) });
  });

  it('rejects missing, duplicated, extra and ambiguous college contributions including a second TAA count', () => {
    const f = figureFixture();
    expect(() => aggregateElectoralFigures(f.chamber, f.ordinary, [])).toThrow(/each expected college/);
    expect(() => aggregateElectoralFigures(f.chamber, [...f.ordinary, f.ordinary[0]], f.special)).toThrow();
    expect(() => aggregateElectoralFigures(f.chamber, [...f.ordinary, { ...f.ordinary[0], collegeId: 'TP' }], f.special)).toThrow();
    expect(() => aggregateElectoralFigures(f.chamber, [{ ...f.ordinary[0], listVotes: f.ordinary[0].listVotes.slice(1) }, ...f.ordinary.slice(1)], f.special)).toThrow(/explicit zeros/);
  });

  it('keeps all-zero elections computable as counts and undefined as percentages', () => {
    const f = figureFixture();
    const zero = (rows: typeof f.ordinary) => rows.map(row => ({ ...row, listVotes: row.listVotes.map(v => ({ ...v, votes: 0 })) }));
    const result = aggregateElectoralFigures(f.chamber, zero(f.ordinary), zero(f.special));
    expect(result.national.validVotes).toBe(0);
    expect(result.national.lists.every(l => l.percentage.status === 'undefined')).toBe(true);
    expect(result.national.coalitions[0].percentage).toEqual({ status: 'undefined', reason: 'zero-valid-votes' });
    expect(calculatePercentages([0, 0], 0)).toEqual([{ status: 'undefined', reason: 'zero-valid-votes' }, { status: 'undefined', reason: 'zero-valid-votes' }]);
  });

  it('uses exact arithmetic beyond floating-point multiplication and rejects unsafe sums', () => {
    expect(calculatePercentages([Number.MAX_SAFE_INTEGER - 1], Number.MAX_SAFE_INTEGER)[0]).toEqual({ status: 'defined', value: exactFraction(BigInt(Number.MAX_SAFE_INTEGER - 1) * 100n, Number.MAX_SAFE_INTEGER) });
    const f = figureFixture();
    f.ordinary[0] = { ...f.ordinary[0], listVotes: f.ordinary[0].listVotes.map(v => ({ ...v, votes: Number.MAX_SAFE_INTEGER })) };
    expect(() => aggregateElectoralFigures(f.chamber, f.ordinary, f.special)).toThrow(/safe integer/);
  });

  it('owns output rows and is independent of contribution ordering', () => {
    const f = figureFixture();
    const before = structuredClone(f);
    const a = aggregateElectoralFigures(f.chamber, f.ordinary, f.special);
    const b = aggregateElectoralFigures(f.chamber, [...f.ordinary].reverse(), [...f.special].reverse());
    expect(a.national).toEqual(b.national);
    expect(a.territories).toEqual(b.territories);
    expect(f).toEqual(before);
    expect(a.taaProportional[0].figures).not.toBe(a.territories[3].figures);
  });
});
