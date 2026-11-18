import { describe, expect, it } from 'vitest';
import { attributeSpecialVotes, type SpecialAttributionResult } from './special-attribution';
import { specialAttributionFixture } from './special-territories.fixture';
import { exactFraction } from './exact';
import { aggregateElectoralFigures } from './figures';
import { determineSpecialWinners } from './special-winners';
import { createSeededDrawContext } from './ties';
import type { ChamberInput } from './input';
import type { ScrutinyExecutionContext } from './result';

const noDraw: ScrutinyExecutionContext = { draw() { throw new Error('Unexpected draw'); } };
function complete(r: SpecialAttributionResult) {
  if (r.status !== 'complete') throw new Error(r.reason);
  return r;
}

describe('N10 special candidate-only vote attribution', () => {
  it.each(['camera', 'senato'] as const)('attributes (60,30,10) + 7 to (64,32,11) in %s', side => {
    const f = specialAttributionFixture(undefined, undefined, side);
    const r = complete(attributeSpecialVotes(f, noDraw));
    expect(r.listVotes.map(l => l.votes)).toEqual([64, 32, 11]);
    expect(r.validVotes).toBe(107);
    expect(r.candidateTotals[0].votes).toBe(107);
    expect(r.traces[0].allocation).toMatchObject({ quota: exactFraction(100, 7) });
    expect(r.lawReference).toBe(side === 'camera' ? 'C93-septies(1)(c)' : 'S21(2-bis)(a)');
    expect(r.draws).toEqual([]);
  });
  it('retains VdA votes nationally with no ordinary slate anywhere', () => {
    const f = specialAttributionFixture([0], 17);
    const chamber: ChamberInput = { ...f.chamber,
      territories: f.chamber.territories.map(t => ({ ...t, kind: 'vda' })) };
    const r = complete(attributeSpecialVotes({ ...f, chamber }, noDraw));
    expect(r).toMatchObject({ lawReference: 'C93(3-bis)(a)', validVotes: 17 });
    expect(r.listVotes).toEqual([{ listId: 'A', votes: 17 }]);
    expect(r.traces[0].method).toBe('single-list');
    const figures = aggregateElectoralFigures(chamber, [], [r]);
    expect(figures.national.validVotes).toBe(17);
    expect(figures.ordinaryAllocation.validVotes).toBe(0);
  });
  it('does no division for zero only-votes, including all-zero support', () => {
    const r = complete(attributeSpecialVotes(specialAttributionFixture([0, 0], 0), noDraw));
    expect(r.validVotes).toBe(0);
    expect(r.traces[0]).toMatchObject({ method: 'no-additions', allocation: null });
  });
  it('blocks positive only-votes without multi-list support before any draw', () => {
    expect(attributeSpecialVotes(specialAttributionFixture([0, 0], 5), noDraw)).toMatchObject({
      status: 'non-computable', reason: 'zero-direct-support', traces: [], draws: [], warnings: [] });
  });
  it('rounds at each uninominal college, then sums; never at constituency level', () => {
    const a = complete(attributeSpecialVotes(specialAttributionFixture([1, 0], 1), noDraw));
    const b = complete(attributeSpecialVotes(specialAttributionFixture([0, 9], 1), noDraw));
    const combined = complete(attributeSpecialVotes(specialAttributionFixture([1, 9], 2), noDraw));
    expect(a.listVotes.map((l, i) => l.votes + b.listVotes[i].votes)).toEqual([2, 10]);
    expect(combined.listVotes.map(l => l.votes)).toEqual([1, 11]);
  });
  it('warns on I03 remainder ties with no secondary direct-vote criterion', () => {
    const r = complete(attributeSpecialVotes(specialAttributionFixture([1, 5], 3), createSeededDrawContext('tie', 'N10')));
    expect(r.draws).toHaveLength(1);
    expect(r.draws[0]).toMatchObject({ norm: 'N10', rule: 'I03', alternativeIds: ['A', 'B'] });
    expect(r.warnings[0]).toMatchObject({ simulationId: 9, interpretationId: 'I03', severity: 'warning' });
    expect(r.listVotes.reduce((s, l) => s + l.votes, 0)).toBe(9);
  });
  it('bypasses attributed totals and preserves exact figures and preliminary winners', () => {
    const f = specialAttributionFixture();
    const r = complete(attributeSpecialVotes(f, noDraw));
    const chamber: ChamberInput = { ...f.chamber, specialVotes: f.chamber.specialVotes.map(row => ({ ...row,
      votes: { kind: 'attributed', listVotes: r.listVotes, candidateTotals: r.candidateTotals, provenance: row.votes.provenance } })) };
    const bypassed = complete(attributeSpecialVotes({ ...f, chamber }, noDraw));
    expect(bypassed).toMatchObject({ listVotes: r.listVotes, candidateTotals: r.candidateTotals, traces: [], draws: [] });
    expect(aggregateElectoralFigures(chamber, [], [bypassed])).toEqual(aggregateElectoralFigures(f.chamber, [], [r]));
    expect(determineSpecialWinners({ ...f, chamber }, noDraw)).toEqual(determineSpecialWinners(f, noDraw));
  });
  it('is canonical across candidate/list order, exposes owned JSON-safe output', () => {
    const f = specialAttributionFixture([1, 5], 3);
    const before = structuredClone(f);
    const row = f.chamber.specialVotes[0];
    if (row.votes.kind !== 'components') throw new Error();
    const chamber: ChamberInput = { ...f.chamber,
      specialCandidatures: f.chamber.specialCandidatures.map(c => ({ ...c, linkedListIds: [...c.linkedListIds].reverse() })),
      specialVotes: [{ ...row, votes: { ...row.votes, directListVotes: [...row.votes.directListVotes].reverse() } }] };
    const a = complete(attributeSpecialVotes(f, createSeededDrawContext('same', 'N10')));
    const b = complete(attributeSpecialVotes({ ...f, chamber }, createSeededDrawContext('same', 'N10')));
    expect(a.draws).toEqual(b.draws);
    expect(a.traces).toEqual(b.traces);
    expect(JSON.parse(JSON.stringify(a))).toEqual(a);
    expect(f).toEqual(before);
    expect(a.candidateTotals).not.toBe(row.votes.candidateTotals);
  });
  it('preflights multiple candidate groups and consumes attribution draws in canonical candidate order', () => {
    const f = specialAttributionFixture([1, 5], 3);
    const original = f.chamber.specialVotes[0];
    if (original.votes.kind !== 'components') throw new Error();
    const make = (zeroSupport = false): ChamberInput => ({ ...f.chamber,
      lists: [...f.chamber.lists, ...['D', 'E'].map(id => ({ id, name: id, coalitionId: 'Z', protectedMinority: null }))],
      coalitions: [...f.chamber.coalitions, { id: 'Z', listIds: ['D', 'E'] }],
      specialCandidatures: [...f.chamber.specialCandidatures,
        { collegeId: 'T0', candidateId: 'zz', linkedListIds: ['D', 'E'], affiliationListId: null }],
      specialVotes: [{ ...original, votes: { kind: 'components', provenance: original.votes.provenance,
        directListVotes: [{ listId: 'A', votes: 1 }, { listId: 'B', votes: 5 },
          { listId: 'D', votes: zeroSupport ? 0 : 1 }, { listId: 'E', votes: zeroSupport ? 0 : 5 }],
        candidateOnlyVotes: [{ candidateId: 'T0-C', votes: 3 }, { candidateId: 'zz', votes: 3 }],
        candidateTotals: [{ candidateId: 'T0-C', votes: 9 }, { candidateId: 'zz', votes: zeroSupport ? 3 : 9 }] } }] });
    expect(attributeSpecialVotes({ ...f, chamber: make(true) }, noDraw)).toMatchObject({
      status: 'non-computable', affectedCandidateId: 'zz', draws: [] });
    const chamber = make();
    const a = complete(attributeSpecialVotes({ ...f, chamber }, createSeededDrawContext('groups', 'N10')));
    const row = chamber.specialVotes[0];
    if (row.votes.kind !== 'components') throw new Error();
    const reversed: ChamberInput = { ...chamber, specialCandidatures: [...chamber.specialCandidatures].reverse(),
      specialVotes: [{ ...row, votes: { ...row.votes, candidateOnlyVotes: [...row.votes.candidateOnlyVotes].reverse(),
        candidateTotals: [...row.votes.candidateTotals].reverse() } }] };
    const b = complete(attributeSpecialVotes({ ...f, chamber: reversed }, createSeededDrawContext('groups', 'N10')));
    expect(a.draws).toHaveLength(2);
    expect(a.draws).toEqual(b.draws);
    expect(a.listVotes).toEqual(b.listVotes);
    expect(a.listVotes.reduce((sum, l) => sum + l.votes, 0)).toBe(18);
  });
  it('conserves every vote for small integer cases', () => {
    for (let a = 0; a <= 5; a++) for (let b = 0; b <= 5; b++) for (let only = 0; only <= 5; only++) {
      const r = attributeSpecialVotes(specialAttributionFixture([a, b], only), createSeededDrawContext('counts', 'N10'));
      if (a + b === 0 && only > 0) expect(r.status).toBe('non-computable');
      else {
        const c = complete(r);
        expect(c.listVotes.reduce((s, l) => s + l.votes, 0)).toBe(a + b + only);
        expect(c.listVotes.every(l => Number.isSafeInteger(l.votes))).toBe(true);
      }
    }
  });
  it('rejects inconsistent candidate totals, duplicated rows, missing counts and mixed representations', () => {
    const f = specialAttributionFixture();
    const row = f.chamber.specialVotes[0];
    const variants = [
      { ...row, votes: { ...row.votes, candidateTotals: [{ candidateId: 'T0-C', votes: 10 }] } },
      { ...row, votes: { ...row.votes, candidateTotals: [] } },
      { ...row, votes: { ...row.votes, listVotes: [] } }
    ];
    for (const v of variants) expect(() => attributeSpecialVotes({ ...f,
      chamber: { ...f.chamber, specialVotes: [v] } }, noDraw)).toThrow(RangeError);
    expect(() => attributeSpecialVotes({ ...f, chamber: { ...f.chamber, specialVotes: [row, row] } }, noDraw)).toThrow(RangeError);
  });
});
