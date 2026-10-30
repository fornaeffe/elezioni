import { describe, expect, it } from 'vitest';
import { determineCameraAdmission } from './admission';
import { exactFraction } from './exact';
import type { Candidate, ChamberInput } from './input';
import type { ScrutinyExecutionContext } from './result';
import { determineSpecialWinners } from './special-winners';
import { taaFixture } from './special-territories.fixture';
import { allocateCameraTaaSeats, type CameraTaaAllocationResult } from './taa-allocation';
import { createSeededDrawContext } from './ties';

const noDraw: ScrutinyExecutionContext = { draw() { throw new Error('Unexpected allocation draw'); } };
function prepare(f: ReturnType<typeof taaFixture>) {
  const preliminary = determineSpecialWinners(f, createSeededDrawContext('winners', 'N09'));
  const admission = determineCameraAdmission({ ...f, specialWinners: preliminary }, createSeededDrawContext('admission', 'N07'));
  return { ...f, preliminary, admission };
}
function allocated(r: CameraTaaAllocationResult) {
  if (r.status === 'non-computable') throw new Error(r.reason);
  return r;
}
const seatCounts = (r: CameraTaaAllocationResult) => Object.fromEntries(allocated(r).listEntitlements.map(l => [l.listId, l.seats]));

describe('N11 Camera TAA proportional allocation', () => {
  it('allocates only the proportional endowment: (60,30,10), 3 seats -> (2,1,0)', () => {
    const f = prepare(taaFixture({ A: 60, B: 30, X: 10 }));
    const r = allocated(allocateCameraTaaSeats(f, noDraw));
    expect(seatCounts(r)).toEqual({ A: 2, B: 1, X: 0 });
    expect(r.subjectAllocation).toMatchObject({ quota: exactFraction(100, 3), rawQuota: exactFraction(100, 3) });
    expect(r.seats).toHaveLength(3);
    expect(r.seats.filter(s => s.status === 'filled').map(s => s.candidateId)).toEqual(['TP-A-0', 'TP-A-2', 'TP-B-0']);
    expect(r.listEntitlements[0].ranking.listPercentage).toMatchObject({ value: exactFraction(60) });
    expect(r.listEntitlements[0].ranking).toMatchObject({ candidates: [{ candidateId: 'TP-A-2', percentage: { value: exactFraction(3) } },
      { candidateId: 'TP-A-3' }, { candidateId: 'TP-A-6' }, { candidateId: 'TP-A-1' }, { candidateId: 'TP-A-4' }, { candidateId: 'TP-A-5' }] });
    expect(r.draws).toEqual([]);
  });
  it('allocates exact quotas twice: coalition (60,30), standalone 10 -> coalition 3 -> (2,1)', () => {
    const f = prepare(taaFixture({ A: 60, B: 30, X: 10 }, 3, { C: ['A', 'B'] }));
    const r = allocated(allocateCameraTaaSeats(f, noDraw));
    expect(seatCounts(r)).toEqual({ A: 2, B: 1, X: 0 });
    expect(r.coalitionAllocations).toHaveLength(1);
    expect(r.coalitionAllocations[0].allocation.quota).toEqual(exactFraction(30));
    expect(r.listEntitlements.find(l => l.listId === 'A')!.subject).toEqual({ kind: 'coalition', id: 'C' });
  });
  it('keeps the fraction that changes (10,3,1), six seats to (4,1,1), instead of integer quota (5,1,0)', () => {
    const r = allocated(allocateCameraTaaSeats(prepare(taaFixture({ A: 10, B: 3, X: 1 }, 6)), noDraw));
    expect(r.subjectAllocation!.quota).toEqual(exactFraction(7, 3));
    expect(seatCounts(r)).toEqual({ A: 4, B: 1, X: 1 });
  });
  it.each([false, true])('uses local votes before a draw at the %s coalition level', coalition => {
    // q=2: equal 1/2 fractional remainders; B's LOCAL five votes beat A's one.
    const f = prepare(taaFixture({ A: 1, B: 5 }, 3, coalition ? { C: ['A', 'B'] } : {}, { A: 1000, B: 100 }));
    const r = allocated(allocateCameraTaaSeats(f, noDraw));
    expect(seatCounts(r)).toEqual({ A: 0, B: 3 });
    const a = coalition ? r.coalitionAllocations[0].allocation : r.subjectAllocation!;
    expect(a.resolutions[0].steps.map(s => s.criterion)).toEqual(['remainder', 'local-electoral-figure']);
    expect(r.draws).toEqual([]);
  });
  it.each([false, true])('uses a statutory seeded draw after equal local figures (coalition=%s)', coalition => {
    const f = prepare(taaFixture({ A: 50, B: 50 }, 1, coalition ? { C: ['A', 'B'] } : {}));
    const a = allocated(allocateCameraTaaSeats(f, createSeededDrawContext('statutory', 'N11')));
    const b = allocated(allocateCameraTaaSeats(f, createSeededDrawContext('statutory', 'N11')));
    expect(a).toEqual(b);
    expect(a.draws).toHaveLength(1);
    expect(a.draws[0]).toMatchObject({ norm: 'N11', rule: 'statutory',
      lawReference: coalition ? 'C93-septies(2)(b)' : 'C93-septies(2)(a)' });
    expect(a.warnings).toEqual([]);
    expect(a.seats).toHaveLength(1);
  });
  it('retains excluded member votes for the coalition but restricts its internal quota to admitted lists', () => {
    const f = prepare(taaFixture({ A: 1, X: 5, Y: 0 }, 3, { C: ['A', 'X', 'Y'] }, { A: 9000, X: 0, Y: 100 }));
    if (f.admission.status !== 'complete') throw new Error();
    const before = structuredClone(f.admission);
    expect(f.admission.lists.find(l => l.listId === 'X')!.admitted).toBe(false);
    expect(f.admission.coalitions[0].bestExcludedListId).toBe('Y');
    const r = allocated(allocateCameraTaaSeats(f, noDraw));
    expect(r.subjectAllocation!.quota).toEqual(exactFraction(2));
    expect(r.coalitionAllocations[0].allocation.quota).toEqual(exactFraction(1, 3));
    expect(seatCounts(r)).toEqual({ A: 3, Y: 0 });
    expect(r.coalitionAllocations[0].allocation.assignments.map(a => a.id)).toEqual(['A', 'Y']);
    expect(f.admission).toEqual(before);
  });
  it('includes the admitted best excluded list in the internal allocation', () => {
    const f = prepare(taaFixture({ A: 1, X: 50, Y: 2 }, 3, { C: ['A', 'X', 'Y'] }, { A: 9000, X: 0, Y: 100 }));
    const r = allocated(allocateCameraTaaSeats(f, noDraw));
    expect(r.subjectAllocation!.quota).toEqual(exactFraction(53, 3));
    expect(r.coalitionAllocations[0].allocation.quota).toEqual(exactFraction(1));
    expect(seatCounts(r)).toEqual({ A: 1, Y: 2 });
  });
  it('does not use excluded votes to repair zero local support among admitted members', () => {
    const f = prepare(taaFixture({ A: 0, X: 50, Y: 0 }, 3, { C: ['A', 'X', 'Y'] }, { A: 9000, X: 0, Y: 100 }));
    expect(allocateCameraTaaSeats(f, noDraw)).toMatchObject({ status: 'non-computable', reason: 'zero-vote-total' });
  });
  it('preserves the coalition target when none of its admitted members is filed locally', () => {
    const f = prepare(taaFixture({ X: 50 }, 3, { C: ['A', 'X', 'Y'] }, { A: 9000, X: 0, Y: 100 }));
    const r = allocateCameraTaaSeats(f, noDraw);
    expect(r).toMatchObject({ status: 'non-computable', reason: 'no-eligible-subjects',
      subjectAllocation: { assignments: [{ seats: 3 }] } });
  });
  it('applies I01 only locally, without rescuing the failed coalition or changing national access', () => {
    const f = prepare(taaFixture({ M: 25, A: 75 }, 3, { F: ['M', 'X'] }, { A: 10000, X: 0 }, { M: 'T' }));
    const before = structuredClone(f.admission);
    if (f.admission.status !== 'complete') throw new Error();
    expect(f.admission.lists.find(l => l.listId === 'M')!.admitted).toBe(false);
    expect(f.admission.coalitions.find(c => c.coalitionId === 'F')!.admitted).toBe(false);
    const r = allocated(allocateCameraTaaSeats(f, noDraw));
    expect(seatCounts(r)).toEqual({ A: 2, M: 1 });
    expect(r.localExceptionListIds).toEqual(['M']);
    expect(r.listEntitlements.find(l => l.listId === 'M')!.subject).toEqual({ kind: 'list', id: 'M' });
    expect(f.admission).toEqual(before);
  });
  it('deduplicates a locally qualifying minority already admitted independently', () => {
    const f = prepare(taaFixture({ M: 25, A: 75 }, 3, { F: ['M', 'X'] }, { A: 500, X: 0 }, { M: 'T' }));
    if (f.admission.status !== 'complete') throw new Error();
    expect(f.admission.lists.find(l => l.listId === 'M')!.admitted).toBe(true);
    const r = allocated(allocateCameraTaaSeats(f, noDraw));
    expect(r.listEntitlements.filter(l => l.listId === 'M')).toHaveLength(1);
    expect(r.seats).toHaveLength(3);
  });
  it('does not grant local seats to an excluded standalone non-minority or failed-coalition member', () => {
    const f = prepare(taaFixture({ A: 70, X: 30 }, 3, { F: ['X', 'Y'] }, { A: 10000, X: 0, Y: 0 }));
    expect(seatCounts(allocateCameraTaaSeats(f, noDraw))).toEqual({ A: 3 });
  });
  it('preserves entitlements, fills the head and leaves missing preference-dependent seats unresolved', () => {
    const f = taaFixture({ A: 100 });
    const chamber: ChamberInput = { ...f.chamber, ordinarySlates: f.chamber.ordinarySlates.map(s => s.collegeId === 'TP'
      ? { ...s, preferences: { kind: 'missing', reason: 'Unknown, not zero' } } : s) };
    const r = allocated(allocateCameraTaaSeats(prepare({ ...f, chamber }), noDraw));
    expect(r.status).toBe('partial');
    expect(r.entitlementStatus).toBe('complete');
    expect(seatCounts(r)).toEqual({ A: 3 });
    expect(r.seats.map(s => s.status)).toEqual(['filled', 'unresolved', 'unresolved']);
    expect(r.seats[1]).toMatchObject({ reason: 'missing-preferences' });
  });
  it('can proclaim the head without preferences when only one seat is owed', () => {
    const f = taaFixture({ A: 100 }, 1);
    const chamber: ChamberInput = { ...f.chamber, ordinarySlates: f.chamber.ordinarySlates.map(s => s.collegeId === 'TP'
      ? { ...s, preferences: { kind: 'missing', reason: 'Not supplied' } } : s) };
    expect(allocateCameraTaaSeats(prepare({ ...f, chamber }), noDraw).status).toBe('complete');
  });
  it('keeps explicit zero preferences computable in filed order', () => {
    const f = taaFixture({ A: 0 }, 3, {}, { A: 100 });
    const zero = allocateCameraTaaSeats(prepare(f), noDraw);
    expect(zero).toMatchObject({ status: 'non-computable', reason: 'zero-vote-total' });
    const g = taaFixture({ A: 100 });
    const chamber: ChamberInput = { ...g.chamber, ordinarySlates: g.chamber.ordinarySlates.map(s => s.collegeId !== 'TP' || s.preferences.kind === 'missing'
      ? s : { ...s, preferences: { ...s.preferences, counts: s.preferences.counts.map(c => ({ ...c, count: 0 })) } }) };
    const r = allocated(allocateCameraTaaSeats(prepare({ ...g, chamber }), noDraw));
    expect(r.status).toBe('complete');
    expect(r.seats.map(s => s.status === 'filled' ? s.candidateId : null)).toEqual(['TP-A-0', 'TP-A-1', 'TP-A-2']);
  });
  it('does not silently skip unavailable or exhausted candidates before R26-16/17', () => {
    const f = taaFixture({ A: 100 }, 8);
    const candidates: Candidate[] = f.candidates.map(c => c.id === 'TP-A-0'
      ? { ...c, availability: { kind: 'unavailable', reason: 'constructed' } } : c);
    const r = allocated(allocateCameraTaaSeats(prepare({ ...f, candidates }), noDraw));
    expect(r.status).toBe('partial');
    expect(r.seats[0]).toMatchObject({ status: 'unresolved', reason: 'candidate-unavailable-requires-recovery' });
    expect(r.seats[7]).toMatchObject({ status: 'unresolved', reason: 'candidate-exhaustion-requires-recovery' });
    expect(seatCounts(r)).toEqual({ A: 8 });
  });
  it('returns explicit non-computable admission and positive-seat zero-support stages', () => {
    expect(allocateCameraTaaSeats(prepare(taaFixture({ A: 0 }, 3)), noDraw)).toMatchObject({
      status: 'non-computable', reason: 'admission-non-computable' });
    const f = prepare(taaFixture({ A: 0 }, 3, {}, { A: 100 }));
    expect(allocateCameraTaaSeats(f, noDraw)).toMatchObject({ status: 'non-computable', reason: 'zero-vote-total' });
  });
  it('avoids division/draws for a zero-seat proportional endowment', () => {
    const r = allocated(allocateCameraTaaSeats(prepare(taaFixture({ A: 0 }, 0, {}, { A: 100 })), noDraw));
    expect(r.status).toBe('complete');
    expect(r.subjectAllocation!.quota).toBeNull();
    expect(r.seats).toEqual([]);
  });
  it('is canonical across arrays and does not mutate or alias the input', () => {
    const f = prepare(taaFixture({ A: 50, B: 50 }, 3, { C: ['A', 'B'] }));
    const before = structuredClone(f);
    const chamber: ChamberInput = { ...f.chamber, ordinarySlates: [...f.chamber.ordinarySlates].reverse(),
      coalitions: f.chamber.coalitions.map(c => ({ ...c, listIds: [...c.listIds].reverse() })) };
    const a = allocateCameraTaaSeats(f, createSeededDrawContext('same', 'N11'));
    const b = allocateCameraTaaSeats({ ...f, chamber }, createSeededDrawContext('same', 'N11'));
    expect(a).toEqual(b);
    expect(f).toEqual(before);
    expect(JSON.parse(JSON.stringify(a))).toEqual(a);
    expect(allocated(a).listEntitlements[0].ranking).not.toBe(f.chamber.ordinarySlates[0]);
  });
  it('rejects swapped chambers, non-proportional colleges and inconsistent special capacities', () => {
    const f = prepare(taaFixture({ A: 100 }));
    expect(() => allocateCameraTaaSeats({ ...f, chamber: { ...f.chamber, chamber: 'senato' } }, noDraw)).toThrow(RangeError);
    expect(() => allocateCameraTaaSeats({ ...f, collegeId: 'T0' }, noDraw)).toThrow(RangeError);
    expect(() => allocateCameraTaaSeats({ ...f, chamber: { ...f.chamber, colleges: f.chamber.colleges.map(c =>
      c.id === 'TP' ? { ...c, seatsWithBonus: 2 } : c) } }, noDraw)).toThrow(RangeError);
  });
});
