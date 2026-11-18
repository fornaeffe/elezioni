import { describe, expect, it } from 'vitest';
import { proclaimCandidates, type CandidateProclamations } from './proclamations';
import { proclamationFixture, repeatPerson } from './proclamations.fixture';
import { resolveMultipleElections } from './multiple-elections';
import { createSeededDrawContext } from './ties';
import { exactFraction } from './exact';
import type { ScrutinyExecutionContext } from './result';

const noDraw: ScrutinyExecutionContext = { draw() { throw new Error('Unexpected draw'); } };
type Mutable<T> = T extends object ? { -readonly [K in keyof T]: Mutable<T[K]> } : T;
function batch(a = 0, b = 0, side: 'camera' | 'senato' = 'camera'): { result: Mutable<CandidateProclamations>; id: string } {
  const input = proclamationFixture(side); const id = repeatPerson(input, a, b);
  return { result: structuredClone(proclaimCandidates(input)) as Mutable<CandidateProclamations>, id };
}
function changeRole(r: Mutable<CandidateProclamations>, id: string, role: 'bonus' | 'special-uninominal' | 'taa-proportional') {
  const e = r.elections.find(e => e.seat.candidateId === id)!;
  if (role !== 'taa-proportional') e.role = role;
  e.seat.category = role;
  if (role === 'bonus') { e.seat.entitlement.collegeId = null; e.seat.electedAt.collegeId = null; }
  // structuredClone preserves the shared seat object in the proclamation batch.
}
describe('N24 precedence and percentage criteria', () => {
  it.each(['camera', 'senato'] as const)('%s keeps repeated capolista in the lowest list percentage college', side => {
    const { result, id } = batch(0, 0, side), r = resolveMultipleElections(result, noDraw);
    expect(r.retained.find(e => e.seat.candidateId === id)?.seat.entitlement.collegeId).toBe('P1');
    expect(r.decisions.find(d => d.candidateId === id)).toMatchObject({ reason: 'lowest-list-percentage', eligibleSeatIds: result.elections.filter(e => e.seat.candidateId === id).map(e => e.seat.seatId) });
    expect(r.released).toHaveLength(1); expect(r.status).toBe('partial');
  });
  it.each([[0, 2, 'P1'], [2, 0, 'P2']] as const)('capolista beats a preference win (%i,%i)', (a, b, college) => {
    const { result, id } = batch(a, b), r = resolveMultipleElections(result, noDraw);
    expect(r.retained.find(e => e.seat.candidateId === id)?.seat.entitlement.collegeId).toBe(college);
    expect(r.decisions.find(d => d.candidateId === id)?.reason).toBe('capolista-priority');
  });
  it('compares only capolista wins when the same person also has a preference win', () => {
    const { result, id } = batch(0, 2);
    const head = structuredClone(result.elections.find(e => e.seat.candidateId === id && e.role === 'capolista')!);
    head.seat.seatId = 'third-college'; head.seat.entitlement.collegeId = 'P3'; head.seat.electedAt.collegeId = 'P3';
    head.listPercentage = { status: 'defined', value: exactFraction(10) };
    result.elections.push(head); result.seats.push(head.seat);
    const preference = result.elections.find(e => e.seat.candidateId === id && e.role === 'preference')!;
    preference.individualPercentage = null;
    const r = resolveMultipleElections(result, noDraw);
    expect(r.retained.find(e => e.seat.candidateId === id)?.seat.seatId).toBe('third-college');
    expect(r.released).toHaveLength(2);
    expect(r.decisions.find(d => d.candidateId === id)?.eligibleSeatIds).toHaveLength(2);
  });
  it.each(['camera', 'senato'] as const)('%s uses highest individual percentage, independently of list share and raw preferences', side => {
    const input = proclamationFixture(side); const id = repeatPerson(input, 2, 2);
    const second = input.chamber.ordinarySlates[1]; if (second.preferences.kind !== 'counts') throw new Error();
    second.preferences.counts.find(p => p.candidateId === id)!.count = 30;
    input.figures.colleges[1].figures.validVotes = 200;
    input.figures.colleges[1].figures.lists[0].votes = 180;
    input.figures.colleges[1].figures.lists[1].votes = 20;
    if (input.ordinary.status !== 'complete') throw new Error();
    input.ordinary.targets[1].votes = 180;
    // 20/100 > 30/200 although second has more preferences and higher list share.
    const result = proclaimCandidates(input);
    const e = result.elections.find(e => e.seat.candidateId === id && e.seat.entitlement.collegeId === 'P2')!;
    expect(e.individualPercentage).toEqual({ status: 'defined', value: exactFraction(15) });
    expect(resolveMultipleElections(result, noDraw).retained.find(e => e.seat.candidateId === id)?.seat.entitlement.collegeId).toBe('P1');
  });
  it.each(['bonus', 'special-uninominal'] as const)('%s precedes capolista and preserves personal affiliation', role => {
    const { result, id } = batch(); changeRole(result, id, role);
    const e = result.elections.find(e => e.seat.candidateId === id)!;
    e.seat.entitlement.subject = { kind: 'coalition', id: 'C' }; e.seat.entitlement.listId = null;
    e.seat.electedAt.subject = { kind: 'coalition', id: 'C' }; e.seat.electedAt.listId = null;
    const r = resolveMultipleElections(result, noDraw);
    expect(r.retained.find(e => e.seat.candidateId === id)).toMatchObject({ role, affiliationListId: 'A', seat: { entitlement: { listId: null, subject: { kind: 'coalition', id: 'C' } } } });
    expect(r.decisions.find(d => d.candidateId === id)?.reason).toBe(`${role}-priority`);
  });
  it('special uninominal takes precedence over a TAA proportional election', () => {
    const { result, id } = batch(); changeRole(result, id, 'special-uninominal');
    const other = result.elections.find(e => e.seat.candidateId === id && e.role !== 'special-uninominal')!; other.seat.category = 'taa-proportional';
    expect(resolveMultipleElections(result, noDraw).retained.find(e => e.seat.candidateId === id)?.role).toBe('special-uninominal');
  });
  it('preserves all released seat identities, categories and entitlements without replacing them', () => {
    const { result } = batch(), before = structuredClone(result), r = resolveMultipleElections(result, noDraw);
    expect(result).toEqual(before);
    for (const release of r.released) {
      const seat = r.seats.find(s => s.seatId === release.seatId)!, original = before.seats.find(s => s.seatId === release.seatId)!;
      expect(seat).toEqual({ seatId: original.seatId, category: original.category, entitlement: original.entitlement, status: 'unfilled', reason: 'multiple-election-requires-recovery' });
    }
    const filled = r.seats.filter(s => s.status === 'filled');
    expect(new Set(filled.map(s => s.candidateId)).size).toBe(filled.length);
  });
  it('keeps independent elections complete and supports no elections', () => {
    const r = resolveMultipleElections(proclaimCandidates(proclamationFixture()), noDraw);
    expect(r).toMatchObject({ status: 'complete', released: [], draws: [], warnings: [] });
    expect(resolveMultipleElections(proclaimCandidates(proclamationFixture('senato', [0, 0])), noDraw).seats).toEqual([]);
  });
});

describe('N24 unresolved comparisons and deterministic I03 ties', () => {
  it.each([0, 2])('resolves equal percentages for role position %i by warned seeded draw', position => {
    const { result, id } = batch(position, position);
    for (const e of result.elections.filter(e => e.seat.candidateId === id)) {
      e.listPercentage = { status: 'defined', value: exactFraction(50) };
      e.individualPercentage = { status: 'defined', value: exactFraction(10) };
    }
    const r = resolveMultipleElections(result, createSeededDrawContext('same', 'N24'));
    expect(r.draws).toHaveLength(1); expect(r.draws[0]).toMatchObject({ rule: 'I03', norm: 'N24' });
    expect(r.warnings).toMatchObject([{ severity: 'warning', interpretationId: 'I03' }]);
    result.elections.reverse(); result.seats.reverse();
    expect(resolveMultipleElections(result, createSeededDrawContext('same', 'N24'))).toEqual(r);
    expect(JSON.parse(JSON.stringify(r))).toEqual(r);
  });
  it.each([0, 2])('does not treat undefined percentages as zero for position %i', position => {
    const { result, id } = batch(position, position);
    const e = result.elections.find(e => e.seat.candidateId === id)!;
    if (position === 0) e.listPercentage = { status: 'undefined', reason: 'zero-valid-votes' };
    else e.individualPercentage = null;
    const r = resolveMultipleElections(result, noDraw);
    expect(r.decisions.find(d => d.candidateId === id)).toMatchObject({ retainedSeatId: null, reason: 'undefined-percentage' });
    expect(r.retained.some(e => e.seat.candidateId === id)).toBe(false);
    expect(r.released).toEqual([]); expect(r.seats.filter(s => s.status === 'unresolved')).toHaveLength(2);
  });
  it('preserves non-computable stage status', () => {
    const result = proclaimCandidates(proclamationFixture('camera', [0, 0]));
    expect(resolveMultipleElections({ ...result, status: 'non-computable', blockers: ['upstream'] }, noDraw).status).toBe('non-computable');
  });
  it.each(['bonus-special', 'bonus-taa', 'two-bonus', 'two-special'] as const)('rejects incompatible %s before drawing', kind => {
    const { result, id } = batch(); changeRole(result, id, kind === 'two-special' ? 'special-uninominal' : 'bonus');
    const second = result.elections.filter(e => e.seat.candidateId === id)[1];
    second.seat.category = kind === 'bonus-taa' ? 'taa-proportional' : kind === 'two-bonus' ? 'bonus' : 'special-uninominal';
    if (second.seat.category !== 'taa-proportional') second.role = second.seat.category;
    expect(() => resolveMultipleElections(result, noDraw)).toThrow(/Incompatible/);
  });
  it.each(['missing', 'duplicate', 'foreign-chamber', 'role'] as const)('rejects %s proclamation metadata', kind => {
    const { result } = batch();
    if (kind === 'missing') result.elections.pop();
    if (kind === 'duplicate') result.seats.push(result.seats[0]);
    if (kind === 'foreign-chamber') result.seats[0].entitlement.chamber = 'senato';
    if (kind === 'role') result.elections[0].role = 'bonus';
    expect(() => resolveMultipleElections(result, noDraw)).toThrow(/metadata/);
  });
});
