import { describe, expect, it } from 'vitest';
import { allocateByQuotaAndRemainders, type QuotaAllocationInput, type QuotaAllocationResult } from './allocation';
import { exactFraction as f, truncateToSixDecimals } from './exact';
import type { ScrutinyExecutionContext } from './result';
import { createSeededDrawContext, type TieContext } from './ties';

const context: TieContext = { simulationId: 1, request: {
  contextId: 'N13/fixture', chamber: 'camera', norm: 'N13', rule: 'statutory', lawReference: 'C83(1)(f)'
} };
const neverDraw: ScrutinyExecutionContext = { draw: () => { throw new Error('Unexpected draw'); } };
function input(votes: readonly number[], seats: number, quotaMode: 'integer' | 'exact' = 'integer'): QuotaAllocationInput {
  return { seats, quotaMode, subjects: votes.map((value, index) => ({ id: String(index), figure: f(value), tieValues: [f(value)] })),
    tieCriteria: [{ name: 'national-votes', direction: 'descending' }], context };
}
const seats = (result: QuotaAllocationResult) => result.assignments.map((row) => row.seats);

describe('reform quota allocations', () => {
  it('truncates the quota before dividing votes for ordinary allocation (N13 anchor)', () => {
    const result = allocateByQuotaAndRemainders(input([10, 3, 1], 6), neverDraw);
    expect(result.status).toBe('complete');
    expect(result.quota).toEqual(f(2));
    expect(result.rawQuota).toEqual(f(7, 3));
    expect(result.initialSeatTotal).toBe('6');
    expect(seats(result)).toEqual([5, 1, 0]);
    expect(result.assignments.map((row) => row.remainder)).toEqual([f(0), f(1), f(1)]);
    expect(result.resolutions).toEqual([]);
  });

  it('retains the exact quota for TAA (N11 anchor) and exposes decimal/remainder state', () => {
    const result = allocateByQuotaAndRemainders(input([10, 3, 1], 6, 'exact'), neverDraw);
    expect(result.quota).toEqual(f(7, 3));
    expect(seats(result)).toEqual([4, 1, 1]);
    expect(result.assignments.map((row) => row.integerSeats)).toEqual([4, 1, 0]);
    expect(result.assignments.map((row) => row.remainder)).toEqual([f(2, 3), f(2, 3), f(1)]);
    expect(result.assignments.map((row) => row.decimalPart)).toEqual([f(2, 7), f(2, 7), f(3, 7)]);
    expect(result.assignments.map((row) => row.remainderState)).toEqual(['unused', 'unused', 'used']);
  });

  it('provides the rational arithmetic for the N04 attribution anchor, without attributing ballots here', () => {
    const result = allocateByQuotaAndRemainders(input([60, 30, 10], 7, 'exact'), neverDraw);
    expect(result.quota).toEqual(f(100, 7));
    expect(seats(result)).toEqual([4, 2, 1]);
    expect(result.assignments.map((row) => row.remainder)).toEqual([f(20, 7), f(10, 7), f(10)]);
  });

  it('uses the caller-selected geographic electoral figure after tied remainders', () => {
    // q=3: votes (7,4,1), four seats -> one residual, all remainders=1.
    const tied = input([7, 4, 1], 4, 'exact');
    const result = allocateByQuotaAndRemainders({ ...tied, subjects: tied.subjects.map((row, index) => ({
      ...row, tieValues: [f([1, 20, 3][index])]
    })), tieCriteria: [{ name: 'college-votes', direction: 'descending' }] }, neverDraw);
    expect(seats(result)).toEqual([2, 2, 0]);
    expect(result.resolutions[0].steps[1]).toEqual({ criterion: 'college-votes', remainingIds: ['1'] });
  });

  it('demonstrates a seat change after symmetric six-decimal index truncation (I05 arithmetic)', () => {
    const base = input([1, 2], 1, 'exact');
    const figures = [f(2, 3), f(6666665, 10000000)];
    const raw = { ...base, subjects: base.subjects.map((row, index) => ({ ...row, figure: figures[index] })) };
    expect(seats(allocateByQuotaAndRemainders(raw, neverDraw))).toEqual([1, 0]);
    const truncated = { ...raw, subjects: raw.subjects.map((row) => ({ ...row, figure: truncateToSixDecimals(row.figure) })) };
    expect(seats(allocateByQuotaAndRemainders(truncated, neverDraw))).toEqual([0, 1]);
  });

  it('keeps sums and cross-products exact beyond the safe integer range', () => {
    const max = Number.MAX_SAFE_INTEGER;
    const result = allocateByQuotaAndRemainders(input([max, max - 1, max - 2], 2, 'exact'), neverDraw);
    expect(result.quota).toEqual(f('13510798882111485'));
    expect(seats(result)).toEqual([1, 1, 0]);
    expect(JSON.parse(JSON.stringify(result))).toEqual(result);
  });

  it('does not evaluate undefined indices for a zero-target group', () => {
    const result = allocateByQuotaAndRemainders(input([0, 0], 0), neverDraw);
    expect(result).toMatchObject({ status: 'complete', quota: null, draws: [], warnings: [] });
    expect(seats(result)).toEqual([0, 0]);
    expect(result.assignments.every((row) => row.quotient === null && row.remainder === null)).toBe(true);
    expect(allocateByQuotaAndRemainders(input([], 0), neverDraw).status).toBe('complete');
  });

  it.each([
    [[], 1, 'integer', 'no-eligible-subjects'],
    [[0, 0], 1, 'exact', 'zero-vote-total'],
    [[1, 1], 3, 'integer', 'zero-quota'],
    [[2, 2, 2], 4, 'integer', 'initial-overallocation']
  ] as const)('reports non-computable arithmetic for %j', (votes, requested, mode, reason) => {
    const result = allocateByQuotaAndRemainders(input(votes, requested, mode), neverDraw);
    expect(result).toMatchObject({ status: 'non-computable', reason, draws: [], warnings: [] });
    if (reason === 'initial-overallocation') {
      expect(result.initialSeatTotal).toBe('6');
      expect(result.assignments).toEqual([]);
    }
  });

  it('excludes reached targets from residual seats and fails explicitly if no legal recipient remains', () => {
    const source = input([10, 3, 1], 6, 'exact');
    const excluded = { ...source, subjects: source.subjects.map((row, index) => ({ ...row, remainderEligible: index !== 2 })) };
    expect(seats(allocateByQuotaAndRemainders(excluded, neverDraw))).toEqual([5, 1, 0]);
    const impossible = { ...source, subjects: source.subjects.map((row) => ({ ...row, remainderEligible: false })) };
    expect(allocateByQuotaAndRemainders(impossible, neverDraw)).toMatchObject({ status: 'non-computable', reason: 'impossible-residual' });
  });

  it('replays I03 allocation draws and warnings regardless of source order', () => {
    const source = input([1, 1, 1], 2, 'exact');
    const warned = { ...source, context: { ...context, request: { ...context.request, rule: 'I03' as const } } };
    const run = (value: QuotaAllocationInput) => allocateByQuotaAndRemainders(value, createSeededDrawContext('seed', 'legal-v1'));
    const left = run(warned);
    const right = run({ ...warned, subjects: [...warned.subjects].reverse() });
    expect(left.draws).toEqual(right.draws);
    expect(left.warnings).toEqual(right.warnings);
    expect([...left.assignments].sort((a, b) => a.id.localeCompare(b.id)))
      .toEqual([...right.assignments].sort((a, b) => a.id.localeCompare(b.id)));
    expect(left.draws).toHaveLength(2);
    expect(left.warnings).toHaveLength(2);
    expect(new Set(left.draws.map((draw) => draw.chosenId)).size).toBe(2);
    expect(left.draws.map((draw) => draw.contextId)).toEqual(['N13/fixture/remainder-1', 'N13/fixture/remainder-2']);
  });

  it('conserves seats and consumes every residual at most once across small exact fixtures', () => {
    for (let a = 0; a <= 4; a += 1) for (let b = 0; b <= 4; b += 1) for (let c = 0; c <= 4; c += 1) {
      if (a + b + c === 0) continue;
      for (const requested of [1, 2, 3, 6]) {
        const result = allocateByQuotaAndRemainders(input([a, b, c], requested, 'exact'), createSeededDrawContext('invariants', 'legal-v1'));
        expect(result.status).toBe('complete');
        expect(seats(result).reduce((sum, value) => sum + value, 0)).toBe(requested);
        expect(result.assignments.every((row) => row.remainderSeats <= 1 && row.seats === row.integerSeats + row.remainderSeats)).toBe(true);
        expect(result.draws.every((draw) => draw.rule === 'statutory')).toBe(true);
        expect(result.warnings).toEqual([]);
      }
    }
  });

  it('rejects malformed primitive arguments separately from lawful non-computable inputs', () => {
    for (const requested of [-1, 0.5, NaN, Infinity, Number.MAX_SAFE_INTEGER + 1]) {
      expect(() => allocateByQuotaAndRemainders(input([1], requested), neverDraw)).toThrow(RangeError);
    }
    const source = input([1, 2], 1);
    expect(() => allocateByQuotaAndRemainders({ ...source, subjects: [source.subjects[0], source.subjects[0]] }, neverDraw)).toThrow(RangeError);
    expect(() => allocateByQuotaAndRemainders({ ...source, tieCriteria: [] }, neverDraw)).toThrow(RangeError);
  });
});
