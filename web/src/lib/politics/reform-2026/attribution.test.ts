import { describe, expect, it } from 'vitest';
import { distributeBonusOnlyVotes, type BonusAttributionInput, type BonusAttributionResult } from './attribution';
import { exactFraction } from './exact';
import { supplied } from './contracts.fixture';
import { createSeededDrawContext, resolveTie } from './ties';
import { aggregateElectoralFigures } from './figures';
import { figureFixture, row } from './electoral-figures.fixture';
import { rankPreferenceCandidates } from './rankings';
import type { ScrutinyExecutionContext } from './result';

const noDraw: ScrutinyExecutionContext = { draw() { throw new Error('Unexpected draw'); } };
function input(counts = [60, 30, 10], only = 7): BonusAttributionInput {
  const ids = counts.map((_, i) => ['A', 'B', 'X'][i]);
  return { chamber: 'camera', collegeId: 'P1', simulationId: 8,
    coalitions: [{ id: 'C', listIds: ids }], votes: { kind: 'components',
      directListVotes: ids.map((listId, i) => ({ listId, votes: counts[i] })),
      bonusOnlyVotes: [{ subject: { kind: 'coalition', id: 'C' }, votes: only }], provenance: supplied } };
}
function complete(result: BonusAttributionResult) {
  if (result.status !== 'complete') throw new Error(result.reason);
  return result;
}

describe('N04 bonus-only attribution', () => {
  it.each(['camera', 'senato'] as const)('uses the non-truncated quota for %s: (60,30,10) + 7 = (64,32,11)', chamber => {
    const result = complete(distributeBonusOnlyVotes({ ...input(), chamber }, noDraw));
    expect(result.validVotes).toBe(107);
    expect(result.listVotes.map(v => v.votes)).toEqual([64, 32, 11]);
    const a = result.traces[0].allocation!;
    expect(a.quota).toEqual(exactFraction(100, 7));
    expect(a.assignments.map(v => v.integerSeats)).toEqual([4, 2, 0]);
    expect(a.assignments.map(v => v.decimalPart)).toEqual([exactFraction(1, 5), exactFraction(1, 10), exactFraction(7, 10)]);
    expect(a.assignments.map(v => v.remainder)).toEqual([exactFraction(20, 7), exactFraction(10, 7), exactFraction(10)]);
    expect(a.assignments.map(v => v.remainderSeats)).toEqual([0, 0, 1]);
    expect(result.draws).toEqual([]);
  });

  it('attributes at each college, rather than redistributing summed direct votes', () => {
    const first = complete(distributeBonusOnlyVotes(input([1, 0], 1), noDraw));
    const second = complete(distributeBonusOnlyVotes({ ...input([0, 9], 1), collegeId: 'P2' }, noDraw));
    expect(first.listVotes.map(v => v.votes)).toEqual([2, 0]);
    expect(second.listVotes.map(v => v.votes)).toEqual([0, 10]);
    expect(complete(distributeBonusOnlyVotes(input([1, 9], 2), noDraw)).listVotes.map(v => v.votes)).toEqual([1, 11]);
  });

  it('does not evaluate a quota for zero only-ballots or an attributed representation', () => {
    expect(complete(distributeBonusOnlyVotes(input([0, 0], 0), noDraw)).traces[0]).toMatchObject({ method: 'no-additions', allocation: null });
    const data = input();
    const attributed = { ...data, votes: { kind: 'attributed' as const, listVotes: row('P1', ['A', 'B', 'X'], [64, 32, 11]).listVotes, provenance: supplied } };
    const result = complete(distributeBonusOnlyVotes(attributed, noDraw));
    expect(result.listVotes.map(v => v.votes)).toEqual([64, 32, 11]);
    expect(result.traces).toEqual([]);
    expect(result.draws).toEqual([]);
  });

  it('handles a single linked list with zero direct support and an absent coalition member', () => {
    const data = input([0], 17);
    expect(complete(distributeBonusOnlyVotes(data, noDraw)).listVotes[0].votes).toBe(17);
    expect(complete(distributeBonusOnlyVotes({ ...data, coalitions: [{ id: 'C', listIds: ['A', 'absent'] }] }, noDraw)).traces[0].method).toBe('single-list');
    const standalone = { ...data, votes: { kind: 'components' as const, directListVotes: [{ listId: 'A', votes: 0 }],
      bonusOnlyVotes: [{ subject: { kind: 'list' as const, id: 'A' }, votes: 17 }], provenance: supplied } };
    expect(complete(distributeBonusOnlyVotes(standalone, noDraw)).listVotes[0].votes).toBe(17);
  });

  it('returns no invented additions and consumes no draws for positive B without direct weights', () => {
    expect(distributeBonusOnlyVotes(input([0, 0], 5), noDraw)).toMatchObject({ status: 'non-computable', reason: 'zero-direct-support', traces: [], draws: [], warnings: [] });
  });

  it('uses warned injected I03 draws for equal remainders, without a direct-vote secondary criterion', () => {
    // q=4; 1/4 and 5/4 have the same decimal remainder despite unequal votes.
    const data = input([1, 5, 2], 2);
    const result = complete(distributeBonusOnlyVotes(data, createSeededDrawContext('tie', 'attribution')));
    // X wins the sole residual here; use (1,5), B=3 for a real cutoff tie.
    expect(result.draws).toHaveLength(0);
    const tied = input([1, 5], 3);
    const a = complete(distributeBonusOnlyVotes(tied, createSeededDrawContext('tie', 'attribution')));
    expect(a.draws).toHaveLength(1);
    expect(a.draws[0]).toMatchObject({ norm: 'N04', rule: 'I03', alternativeIds: ['A', 'B'] });
    expect(a.warnings[0]).toMatchObject({ severity: 'warning', simulationId: 8, chamber: 'camera', interpretationId: 'I03' });
    if (tied.votes.kind !== 'components') throw new Error();
    const reversed = { ...tied, votes: { ...tied.votes, directListVotes: [...tied.votes.directListVotes].reverse() } };
    const b = complete(distributeBonusOnlyVotes(reversed, createSeededDrawContext('tie', 'attribution')));
    expect(b.draws).toEqual(a.draws);
    expect(b.traces).toEqual(a.traces);
  });

  it('conserves ballots over many integer inputs including B larger than direct support', () => {
    for (let a = 0; a < 5; a++) for (let b = 0; b < 5; b++) for (const only of [0, 1, 3, 17]) {
      if (!a && !b && only) continue;
      const result = complete(distributeBonusOnlyVotes(input([a, b], only), createSeededDrawContext('conservation', 'N04')));
      expect(result.listVotes.reduce((n, v) => n + v.votes, 0)).toBe(a + b + only);
      expect(result.listVotes.every(v => Number.isSafeInteger(v.votes))).toBe(true);
    }
  });

  it('rejects mixed representations, duplicate rows, invalid counts and overflow', () => {
    const data = input();
    expect(() => distributeBonusOnlyVotes({ ...data, votes: { ...data.votes, listVotes: [] } } as unknown as BonusAttributionInput, noDraw)).toThrow(/Mixed/);
    expect(() => distributeBonusOnlyVotes(input([-1, 2], 1), noDraw)).toThrow(RangeError);
    expect(() => distributeBonusOnlyVotes(input([Number.MAX_SAFE_INTEGER, 1], 0), noDraw)).toThrow(/safe integer/);
    const duplicate = { ...data, votes: { kind: 'components' as const, directListVotes: [{ listId: 'A', votes: 1 }, { listId: 'A', votes: 2 }], bonusOnlyVotes: [], provenance: supplied } };
    expect(() => distributeBonusOnlyVotes(duplicate, noDraw)).toThrow(/Duplicate/);
  });

  it.each(['camera', 'senato'] as const)('produces identical %s downstream figures, rankings and legal draws for raw and final totals', chamber => {
    const f = figureFixture(chamber);
    // A tied attribution deliberately consumes its own stream before downstream work.
    const raw = complete(distributeBonusOnlyVotes({ ...input([1, 5, 0], 3), chamber }, createSeededDrawContext('same', 'attribution')));
    expect(raw.draws).toHaveLength(1);
    const final = complete(distributeBonusOnlyVotes({ ...input(), chamber, votes: { kind: 'attributed', listVotes: raw.listVotes, provenance: supplied } }, noDraw));
    const downstream = (college: typeof raw) => {
      const figures = aggregateElectoralFigures(f.chamber, [college, ...f.ordinary.slice(1)], f.special);
      const slate = f.chamber.ordinarySlates[0];
      const ranking = rankPreferenceCandidates({ ...slate, preferences: { kind: 'counts',
        counts: slate.candidateIds.slice(1).map((candidateId, i) => ({ candidateId, count: i < 3 ? 1 : 0 })),
        provenance: supplied, validity: { kind: 'adjudicated', reference: 'matched valid profile' }
      } }, figures.colleges[0].figures);
      const tie = resolveTie([{ id: 'one', values: [exactFraction(figures.national.validVotes)] },
        { id: 'two', values: [exactFraction(figures.national.validVotes)] }], [{ name: 'figure', direction: 'descending' }],
        { simulationId: 8, request: { contextId: 'downstream-probe', chamber, norm: 'N12', rule: 'I03', lawReference: 'stream-isolation test' } }, createSeededDrawContext('same', 'downstream'));
      return { figures, ranking, tie };
    };
    expect(downstream(raw)).toEqual(downstream(final));
    expect(JSON.parse(JSON.stringify(downstream(raw)))).toEqual(downstream(raw));
  });

  it('preflights every group and keeps multi-subject draw order canonical', () => {
    const data: BonusAttributionInput = { ...input(), coalitions: [
      { id: 'C', listIds: ['A', 'B'] }, { id: 'D', listIds: ['X', 'Y'] }
    ], votes: { kind: 'components', directListVotes: ['A', 'B', 'X', 'Y'].map(listId => ({ listId, votes: 1 })),
      bonusOnlyVotes: [{ subject: { kind: 'coalition', id: 'D' }, votes: 1 }, { subject: { kind: 'coalition', id: 'C' }, votes: 1 }], provenance: supplied } };
    if (data.votes.kind !== 'components') throw new Error();
    const a = complete(distributeBonusOnlyVotes(data, createSeededDrawContext('multiple', 'attribution')));
    const b = complete(distributeBonusOnlyVotes({ ...data, votes: { ...data.votes,
      bonusOnlyVotes: [...data.votes.bonusOnlyVotes].reverse(), directListVotes: [...data.votes.directListVotes].reverse()
    } }, createSeededDrawContext('multiple', 'attribution')));
    expect(a.draws).toHaveLength(2);
    expect(a.draws).toEqual(b.draws);
    expect(a.traces).toEqual(b.traces);
    const failed = distributeBonusOnlyVotes({ ...data, votes: { ...data.votes,
      directListVotes: data.votes.directListVotes.map(v => ({ ...v, votes: ['X', 'Y'].includes(v.listId) ? 0 : 1 }))
    } }, noDraw);
    expect(failed).toMatchObject({ status: 'non-computable', traces: [], draws: [] });
  });
});
