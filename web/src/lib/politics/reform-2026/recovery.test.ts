import { describe, expect, it } from 'vitest';
import { recoverInitialSeats } from './recovery';
import { bonusSeat, enable, recoveryFixture, remainder } from './recovery.fixture';
import { proclamationFixture, repeatPerson } from './proclamations.fixture';
import { proclaimCandidates } from './proclamations';
import { resolveMultipleElections } from './multiple-elections';
import { createSeededDrawContext } from './ties';
import type { ScrutinyExecutionContext } from './result';
import { exactFraction } from './exact';

const noDraw: ScrutinyExecutionContext = { draw() { throw new Error('Unexpected draw'); } };
const winner = (r: ReturnType<typeof recoverInitialSeats>) => r.seats[0].status === 'filled' ? r.seats[0].candidateId : null;

describe('N25/N27 ordered ordinary initial recovery and I07 worked cases', () => {
  it.each(['camera', 'senato'] as const)('%s fills the original college before moving a seat', side => {
    const input = recoveryFixture(side); enable(input, 'R1-A-1', 'R1-A-2', 'R2-A-0');
    const result = recoverInitialSeats(input, noDraw);
    expect(winner(result)).toBe('R1-A-2'); // six preferences beats one; head unavailable
    expect(result.recoveries[0]).toMatchObject({ route: 'same-college', vacancyAt: input.initial.seats[0].entitlement,
      candidateId: 'R1-A-2', outcome: 'filled', disposition: 'retained' });
    expect(result.status).toBe('complete'); expect(result.rounds).toEqual([{ round: 1, filledBefore: 0, filledAfter: 1, proposed: 1, releasedSeatIds: [] }]);
  });
  it('I07-A uses the regional original list before a coalition partner', () => {
    const input = recoveryFixture(); enable(input, 'R2-A-0', 'R1-B-0');
    const result = recoverInitialSeats(input, noDraw);
    expect(winner(result)).toBe('R2-A-0'); expect(result.recoveries[0].route).toBe('same-list-local'); expect(result.warnings).toEqual([]);
  });
  it('I07-B uses the regional coalition before an original list elsewhere', () => {
    const input = recoveryFixture(); enable(input, 'R1-B-0', 'S1-A-0'); remainder(input, 'S', null, 'A', 9);
    const result = recoverInitialSeats(input, noDraw);
    expect(winner(result)).toBe('R1-B-0'); expect(result.seats[0]).toMatchObject({ entitlement: { listId: 'A' }, electedAt: { listId: 'B' } });
    expect(result.recoveries[0].route).toBe('coalition-original-college'); expect(result.warnings).toEqual([]);
  });
  it('Camera searches the original list nationally before the local coalition', () => {
    const input = recoveryFixture('camera'); enable(input, 'R1-B-0', 'S1-A-0');
    expect(winner(recoverInitialSeats(input, noDraw))).toBe('S1-A-0');
  });
  it.each([false, true])('I07-C unused before used, or all used (%s)', allUsed => {
    const input = recoveryFixture(); enable(input, 'S1-A-0', 'T1-A-0', 'U1-A-0');
    remainder(input, 'S', null, 'A', 7, allUsed ? 'used' : 'unused');
    remainder(input, 'T', null, 'A', 4, allUsed ? 'used' : 'unused'); remainder(input, 'U', null, 'A', 9, 'used');
    const result = recoverInitialSeats(input, noDraw);
    expect(winner(result)).toBe(allUsed ? 'U1-A-0' : 'S1-A-0');
    expect(result.seats[0]).toMatchObject({ seatId: 'seat-1', entitlement: input.initial.seats[0].entitlement,
      electedAt: { territoryId: allUsed ? 'U' : 'S' } });
    expect(result.warnings).toEqual([expect.objectContaining({ interpretationId: 'I07', severity: 'warning', seatId: 'seat-1' })]);
    expect(result.recoveries[0].rankings[0].alternatives).toHaveLength(3);
    expect(result.recoveries[0].remainders).toHaveLength(2); expect(result.remainders).toEqual(input.remainders.toSorted((a, b) => a.id < b.id ? -1 : 1));
  });
  it('I07-D never imports cross-region coalition C84(7)', () => {
    const input = recoveryFixture(); enable(input, 'S1-B-0');
    const result = recoverInitialSeats(input, noDraw);
    expect(result.seats[0]).toMatchObject({ status: 'unfilled', reason: 'permitted-candidates-exhausted' });
    expect(result.status).toBe('partial'); expect(result.warnings).toEqual([]); expect(result.recoveries[0].outcome).toBe('unfilled');
  });
  it.each([false, true])('Camera C84(7) is available only for initial exhaustion, released=%s', release => {
    const input = recoveryFixture('camera'); enable(input, 'S1-B-0');
    if (release && input.initial.seats[0].status !== 'filled') { input.initial.seats[0].reason = 'multiple-election-requires-recovery';
      input.initial.released.push({ seatId: 'seat-1', candidateId: 'R1-A-0', retainedSeatId: 'elsewhere' }); }
    const result = recoverInitialSeats(input, noDraw);
    expect(winner(result)).toBe(release ? null : 'S1-B-0');
    if (!release) expect(result.recoveries[0].route).toBe('coalition-other-territory');
  });
  it('uses other local coalition colleges after the original college', () => {
    const input = recoveryFixture(); enable(input, 'R2-B-0', 'S1-A-0');
    expect(recoverInitialSeats(input, noDraw).recoveries[0]).toMatchObject({ route: 'coalition-local-colleges', candidateId: 'R2-B-0' });
  });
  it.each(['unused', 'used', 'consumed'] as const)('retains the exact source remainder state %s', state => {
    const input = recoveryFixture(); enable(input, 'R2-A-0'); remainder(input, 'R', 'R2', 'A', 3, state);
    const result = recoverInitialSeats(input, noDraw);
    expect(result.recoveries[0].remainders).toEqual([{ id: '["R","R2","A"]', value: exactFraction(3, 10), before: state, after: state }]);
  });
  it('orders multiple college alternatives by unused remainders, then descending decimals', () => {
    const input = recoveryFixture(); enable(input, 'S1-A-0', 'S2-A-0');
    remainder(input, 'S', 'S1', 'A', 9, 'used'); remainder(input, 'S', 'S2', 'A', 2);
    expect(winner(recoverInitialSeats(input, noDraw))).toBe('S2-A-0');
    remainder(input, 'S', 'S2', 'A', 2, 'used'); expect(winner(recoverInitialSeats(input, noDraw))).toBe('S1-A-0');
  });
  it('orders coalition colleges before comparing member lists within the chosen college', () => {
    const input = recoveryFixture();
    input.chamber.colleges.find(c => c.id === 'S1')!.territoryId = 'R';
    for (const r of input.remainders.filter(r => r.collegeId === 'S1')) r.territoryId = 'R';
    enable(input, 'R2-B-0', 'S1-B-0'); remainder(input, 'R', 'R2', 'G', 2); remainder(input, 'R', 'S1', 'G', 8);
    remainder(input, 'R', 'R2', 'B', 9); remainder(input, 'R', 'S1', 'B', 1);
    expect(winner(recoverInitialSeats(input, noDraw))).toBe('S1-B-0');
  });
  it.each([false, true])('I07-F statutory remainder draws replay under permutations, release=%s', release => {
    const input = recoveryFixture(); enable(input, 'S1-A-0', 'T1-A-0');
    if (release && input.initial.seats[0].status !== 'filled') input.initial.seats[0].reason = 'multiple-election-requires-recovery';
    const run = () => recoverInitialSeats(input, createSeededDrawContext(1234, 'I07-F'));
    const result = run();
    expect(result.draws).toHaveLength(1); expect(result.draws[0]).toMatchObject({ rule: 'statutory', norm: 'N27' });
    expect(result.warnings.map(w => w.interpretationId)).toEqual(['I07']);
    input.candidates.reverse(); input.chamber.ordinarySlates.reverse(); input.chamber.territories.reverse(); input.chamber.colleges.reverse();
    input.eligibleLists.reverse(); input.remainders.reverse(); input.figures.colleges.reverse();
    expect(run()).toEqual(result);
  });
});

describe('N26/N27 bonus shortages, separate from ordinary recovery', () => {
  it.each(['camera', 'senato'] as const)('%s single-list bonus chooses the highest college list percentage', side => {
    const input = recoveryFixture(side); bonusSeat(input, false); enable(input, 'S1-A-0', 'S2-A-0');
    const result = recoverInitialSeats(input, noDraw);
    expect(winner(result)).toBe('S2-A-0'); expect(result.seats[0]).toMatchObject({ category: 'bonus', entitlement: { territoryId: 'R', listId: 'A' }, electedAt: { territoryId: 'S', collegeId: 'S2' } });
  });
  it.each([false, true])('I07-E coalition bonus subject, then list, then highest-percentage college, all used=%s', used => {
    const input = recoveryFixture(); bonusSeat(input); enable(input, 'S1-A-0', 'S2-A-2', 'T1-B-0');
    // G:S=.6 beats G:T=.3; inside S, A=.5 beats B=.2 (B has no candidates).
    remainder(input, 'S', null, 'G', 6, used ? 'used' : 'unused'); remainder(input, 'T', null, 'G', 3, used ? 'used' : 'unused');
    const result = recoverInitialSeats(input, noDraw);
    expect(winner(result)).toBe('S2-A-2'); expect(result.recoveries[0]).toMatchObject({ route: 'bonus-other-territory', interpretationId: 'I07' });
    expect(result.seats[0]).toMatchObject({ entitlement: { subject: { kind: 'coalition', id: 'G' }, listId: null }, electedAt: { listId: 'A' } });
    expect(result.warnings).toHaveLength(1);
  });
  it('I07-E exact worked list B remainder .50 beats A .20, then B college 12% beats 10%', () => {
    const input = recoveryFixture(); bonusSeat(input);
    enable(input, 'S1-A-0', 'S1-B-0', 'S2-B-0', 'T1-A-0');
    for (const f of input.figures.colleges.filter(f => f.collegeId.startsWith('S'))) {
      const percentage = f.figures.lists.find(l => l.listId === 'B')!.percentage;
      if (percentage.status !== 'defined') throw new Error(); percentage.value = exactFraction(f.collegeId === 'S2' ? 12 : 10);
    }
    remainder(input, 'S', null, 'G', 6); remainder(input, 'T', null, 'G', 3);
    remainder(input, 'S', null, 'A', 2); remainder(input, 'S', null, 'B', 5);
    expect(winner(recoverInitialSeats(input, noDraw))).toBe('S2-B-0');
  });
  it('fills a local bonus replacement before a larger remainder elsewhere', () => {
    const input = recoveryFixture(); bonusSeat(input); enable(input, 'R1-A-0', 'S1-B-0'); remainder(input, 'S', null, 'G', 9);
    const result = recoverInitialSeats(input, noDraw);
    expect(winner(result)).toBe('R1-A-0'); expect(result.recoveries[0].route).toBe('bonus-local'); expect(result.warnings).toEqual([]);
  });
  it('uses unused member-list remainder before a larger used remainder', () => {
    const input = recoveryFixture(); bonusSeat(input); enable(input, 'R1-A-0', 'R1-B-0');
    remainder(input, 'R', null, 'A', 9, 'used'); remainder(input, 'R', null, 'B', 2);
    expect(winner(recoverInitialSeats(input, noDraw))).toBe('R1-B-0');
  });
  it('keeps bonus exhaustion explicit without redistributing to an outside subject', () => {
    const input = recoveryFixture(); bonusSeat(input, false); enable(input, 'S1-B-0');
    expect(recoverInitialSeats(input, noDraw).seats[0]).toMatchObject({ category: 'bonus', status: 'unfilled' });
  });
  it('warns I03 for unspecified final bonus ties, independently of I07', () => {
    const input = recoveryFixture(); bonusSeat(input); enable(input, 'R1-A-0', 'R1-B-0');
    const result = recoverInitialSeats(input, createSeededDrawContext(123, 'bonus'));
    expect(result.draws[0].rule).toBe('I03'); expect(result.warnings.map(w => w.interpretationId)).toEqual(['I03']);
  });
});

describe('initial recovery composition, uncertainty and boundaries', () => {
  it.each(['camera', 'senato'] as const)('%s repeatedly resolves replacement double elections and fills released seats', side => {
    const p = proclamationFixture(side, [1, 1]);
    repeatPerson(p, 0, 0); repeatPerson(p, 2, 2);
    const preferences = p.chamber.ordinarySlates[1].preferences;
    if (preferences.kind !== 'counts') throw new Error();
    preferences.counts.find(p => p.candidateId === 'P1-2')!.count = 30;
    // Both heads unavailable, both first replacements share identity. N24
    // retains it in P2 (higher individual percentage), then P1 takes next.
    p.candidates.find(c => c.id === 'P1-0')!.availability = { kind: 'unavailable', reason: 'test' };
    const initial = resolveMultipleElections(proclaimCandidates(p), noDraw);
    const result = recoverInitialSeats({ ...p, initial, eligibleLists: [{ territoryId: 'R', listId: 'A' }], remainders: [] }, noDraw);
    expect(result.status).toBe('complete'); expect(result.rounds).toHaveLength(2);
    expect(result.rounds.map(r => [r.filledBefore, r.filledAfter, r.proposed])).toEqual([[0, 1, 2], [1, 2, 1]]);
    expect(result.recoveries.map(t => t.disposition)).toEqual(['released', 'retained', 'retained']);
    expect(new Set(result.retained.map(e => e.seat.candidateId)).size).toBe(2);
    expect(result.seats.map(s => s.entitlement)).toEqual(initial.seats.map(s => s.entitlement));
  });
  it('fills multiple missing seats from one slate without repeating the same candidate', () => {
    const input = recoveryFixture(); enable(input, 'R2-A-0', 'R2-A-2', 'R2-A-3');
    input.initial.seats.push({ ...structuredClone(input.initial.seats[0]), seatId: 'seat-2' }, { ...structuredClone(input.initial.seats[0]), seatId: 'seat-3' });
    const result = recoverInitialSeats(input, noDraw);
    expect(result.retained.map(e => e.seat.candidateId).sort()).toEqual(['R2-A-0', 'R2-A-2', 'R2-A-3']);
    expect(result.status).toBe('complete');
  });
  it('replaces a released recovered seat at its actual location, preserving the original entitlement', () => {
    const input = recoveryFixture(); enable(input, 'S1-A-0', 'S2-A-2');
    input.initial.seats.push({ ...structuredClone(input.initial.seats[0]), seatId: 'seat-2',
      entitlement: { ...input.initial.seats[0].entitlement, collegeId: 'R2' } });
    input.chamber.ordinarySlates.find(s => s.collegeId === 'S2' && s.listId === 'A')!.candidateIds[0] = 'S1-A-0';
    remainder(input, 'S', 'S1', 'A', 9); remainder(input, 'S', 'S2', 'A', 2);
    const result = recoverInitialSeats(input, noDraw);
    expect(result.status).toBe('complete'); expect(result.rounds).toHaveLength(2);
    const last = result.recoveries.at(-1)!;
    expect(last).toMatchObject({ round: 2, seatId: 'seat-2', route: 'same-college',
      originalEntitlement: { territoryId: 'R', collegeId: 'R2' }, vacancyAt: { territoryId: 'S', collegeId: 'S2' },
      electedAt: { territoryId: 'S', collegeId: 'S2' }, candidateId: 'S2-A-2' });
    expect(result.warnings.filter(w => w.interpretationId === 'I07')).toHaveLength(3);
  });
  it('freezes undefined repeated replacement percentages and terminates without duplicate identities', () => {
    const p = proclamationFixture('camera', [1, 1]); repeatPerson(p, 0, 0); repeatPerson(p, 2, 2);
    p.candidates.find(c => c.id === 'P1-0')!.availability = { kind: 'unavailable', reason: 'test' };
    // The exact figure provider explicitly reports an undefined percentage.
    for (const f of p.figures.colleges) f.figures.validVotes = 0;
    const initial = resolveMultipleElections(proclaimCandidates(p), noDraw);
    const result = recoverInitialSeats({ ...p, initial, eligibleLists: [{ territoryId: 'R', listId: 'A' }], remainders: [] }, noDraw);
    expect(result.status).toBe('partial'); expect(result.rounds).toHaveLength(1);
    expect(result.retained).toEqual([]); expect(result.seats.every(s => s.status === 'unresolved')).toBe(true);
    expect(result.recoveries.every(t => t.disposition === 'unresolved')).toBe(true);
  });
  it('resolves two bonus recovery proposals for one ordinary person with warned I03, then replaces the release', () => {
    const input = recoveryFixture(); bonusSeat(input, false);
    input.initial.seats.push({ ...structuredClone(input.initial.seats[0]), seatId: 'seat-2',
      entitlement: { ...input.initial.seats[0].entitlement, territoryId: 'S' } });
    enable(input, 'R1-A-0', 'S1-A-0', 'R1-A-2', 'S1-A-2');
    const second = input.chamber.ordinarySlates.find(s => s.collegeId === 'S1' && s.listId === 'A')!;
    second.candidateIds[0] = 'R1-A-0';
    const result = recoverInitialSeats(input, createSeededDrawContext(9, 'two-bonus'));
    expect(result.status).toBe('complete'); expect(result.rounds).toHaveLength(2);
    expect(result.decisions.some(d => d.reason === 'multiple-bonus-recovery')).toBe(true);
    expect(result.warnings.some(w => w.interpretationId === 'I03')).toBe(true);
    expect(new Set(result.retained.map(e => e.seat.candidateId)).size).toBe(2);
  });
  it('does not skip missing regional preferences in favour of a known external candidate', () => {
    const input = recoveryFixture(); enable(input, 'R1-A-1', 'S1-A-0');
    input.chamber.ordinarySlates.find(s => s.collegeId === 'R1' && s.listId === 'A')!.preferences = { kind: 'missing', reason: 'unknown' };
    const result = recoverInitialSeats(input, noDraw);
    expect(result.seats[0]).toMatchObject({ status: 'unresolved', reason: 'missing-preferences' }); expect(result.warnings).toEqual([]);
  });
  it('can prove exhaustion without preferences when every filed person is unavailable', () => {
    const input = recoveryFixture(); enable(input, 'S1-A-0');
    input.chamber.ordinarySlates[0].preferences = { kind: 'missing', reason: 'unknown' };
    expect(winner(recoverInitialSeats(input, noDraw))).toBe('S1-A-0');
  });
  it('can select a known head despite missing preferences; explicit zero counts keep filed order', () => {
    const input = recoveryFixture(); enable(input, 'R1-A-0', 'R1-A-1', 'R1-A-2');
    const slate = input.chamber.ordinarySlates[0]; slate.preferences = { kind: 'missing', reason: 'unknown' };
    expect(winner(recoverInitialSeats(input, noDraw))).toBe('R1-A-0');
    input.candidates.find(c => c.id === 'R1-A-0')!.availability = { kind: 'unavailable', reason: 'test' };
    const original = recoveryFixture().chamber.ordinarySlates[0].preferences;
    if (original.kind !== 'counts') throw new Error(); for (const p of original.counts) p.count = 0;
    slate.preferences = original; expect(winner(recoverInitialSeats(input, noDraw))).toBe('R1-A-1');
  });
  it('does not turn missing/null quotients into zero or claim candidates are exhausted', () => {
    const input = recoveryFixture(); enable(input, 'R2-A-0'); input.remainders = [];
    expect(recoverInitialSeats(input, noDraw).seats[0]).toMatchObject({ status: 'unresolved', reason: 'missing-recovery-remainder' });
  });
  it('does not require a lower used-tier value before a known unused destination', () => {
    const input = recoveryFixture(); enable(input, 'S1-A-0', 'T1-A-0');
    remainder(input, 'T', null, 'A', 1, 'used');
    input.remainders.find(r => r.territoryId === 'T' && r.collegeId === null && r.subject.id === 'A')!.value = null;
    expect(winner(recoverInitialSeats(input, noDraw))).toBe('S1-A-0');
  });
  it('does not choose a bonus college using an undefined percentage', () => {
    const input = recoveryFixture(); bonusSeat(input, false); enable(input, 'R1-A-0');
    input.figures.colleges[0].figures.lists[0].percentage = { status: 'undefined', reason: 'zero-valid-votes' };
    expect(recoverInitialSeats(input, noDraw).seats[0]).toMatchObject({ status: 'unresolved', reason: 'undefined-recovery-percentage' });
  });
  it('does not use a filed but unadmitted coalition partner', () => {
    const input = recoveryFixture(); enable(input, 'R1-B-0'); input.eligibleLists = input.eligibleLists.filter(r => r.listId !== 'B');
    expect(recoverInitialSeats(input, noDraw).seats[0].status).toBe('unfilled');
  });
  it('does not process future vacancies or special supplementary elections', () => {
    const input = recoveryFixture(); enable(input, 'R1-A-0');
    if (input.initial.seats[0].status === 'filled') throw new Error();
    input.initial.seats[0].reason = 'future-vacancy';
    expect(recoverInitialSeats(input, noDraw).recoveries).toEqual([]);
    input.initial.seats[0].category = 'special-uninominal';
    input.initial.seats[0].reason = 'candidate-unavailable-requires-recovery';
    expect(recoverInitialSeats(input, noDraw).recoveries).toEqual([]);
  });
  it('preserves non-computable prerequisites without claiming a terminal filled result', () => {
    const input = recoveryFixture(); input.initial.status = 'non-computable'; input.initial.blockers = ['upstream'];
    expect(recoverInitialSeats(input, noDraw)).toMatchObject({ status: 'non-computable', blockers: ['upstream'], terminalReason: 'prerequisite-non-computable', recoveries: [] });
  });
  it('owns JSON-safe results without mutating candidate or quotient state', () => {
    const input = recoveryFixture(); enable(input, 'S1-A-0'); const before = structuredClone(input);
    const result = recoverInitialSeats(input, noDraw); expect(input).toEqual(before);
    expect(JSON.parse(JSON.stringify(result))).toEqual(result);
    input.initial.seats[0].entitlement.territoryId = 'changed'; input.remainders[0].subject.id = 'changed';
    expect(result.seats[0].entitlement.territoryId).toBe('R'); expect(result.remainders[0].subject.id).toBe('A');
  });
  it.each(['chamber', 'identity', 'remainder', 'scope', 'fraction', 'slate'] as const)('rejects invalid %s before any draw', kind => {
    const input = recoveryFixture();
    if (kind === 'chamber') input.initial.chamber = 'camera';
    if (kind === 'identity') input.candidates.push(input.candidates[0]);
    if (kind === 'remainder') input.remainders.push(input.remainders[0]);
    if (kind === 'scope') input.remainders[0].territoryId = 'unknown';
    if (kind === 'fraction') input.remainders[0].value = exactFraction(1);
    if (kind === 'slate') input.chamber.ordinarySlates[0].candidateIds[0] = 'unknown';
    expect(() => recoverInitialSeats(input, noDraw)).toThrow(RangeError);
  });
});
