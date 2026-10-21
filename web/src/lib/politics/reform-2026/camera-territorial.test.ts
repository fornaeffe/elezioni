import { describe, expect, it } from 'vitest';
import { admissionFixture } from './admission.fixture';
import type { CameraNationalAllocationResult } from './camera-national';
import {
  allocateCameraCapConstituencies, allocateCameraCoalitionListsInConstituencies, allocateCameraColleges,
  allocateCameraConstituencies, allocateCameraTerritorialSeats,
  type CameraConstituencyListResult, type CameraTerritorialInput
} from './camera-territorial';
import { exactFraction } from './exact';
import { aggregateElectoralFigures } from './figures';
import type { Subject } from './input';
import type { ScrutinyExecutionContext } from './result';
import { createSeededDrawContext } from './ties';
import { assembleRecoveryRemainders } from './recovery-communications';
import { allocationTransfers } from './scrutiny-transfers';
import type { ReformChamberExecution } from './scrutiny';

const noDraw: ScrutinyExecutionContext = { draw() { throw new Error('Unexpected draw'); } };
const first: ScrutinyExecutionContext = { draw(r) { return { ...r, chosenId: r.alternativeIds[0], streamId: 'controlled' }; } };
const list = (id: string): Subject => ({ kind: 'list', id });
type Mutable<T> = T extends object ? { -readonly [K in keyof T]: Mutable<T[K]> } : T;

/** Tiny independently constructed N13/N14 communications. These are stage
 * fixtures, not a full 392-seat candidature envelope or decree table. */
function fixture(targets: Record<string, number> = { A: 3, B: 2 },
  areas = [{ territory: 'O', seats: 3, A: 210, B: 90 }, { territory: 'O', seats: 2, A: 90, B: 110 }] as
    readonly ({ territory: string; seats: number } & Record<string, string | number>)[],
  groups: Record<string, string[]> = {}, cap = false): Mutable<CameraTerritorialInput> {
  const ids = Object.keys(targets);
  const f = admissionFixture('camera', Object.fromEntries([...new Set(areas.map(a => a.territory))].map(t => [t,
    Object.fromEntries(ids.map(id => [id, areas.filter(a => a.territory === t).reduce((s, a) => s + Number(a[id]), 0)]))])), groups);
  const colleges = areas.map((a, i) => ({ id: `P${i}`, territoryId: a.territory, kind: 'ordinary' as const, seatsWithoutBonus: a.seats, seatsWithBonus: a.seats }));
  const chamber = { ...f.chamber, colleges, ordinarySlates: colleges.flatMap(c => ids.map(id => ({ collegeId: c.id, listId: id,
    candidateIds: [], preferences: { kind: 'missing' as const, reason: 'Entitlements only' } }))) };
  const figures = aggregateElectoralFigures(chamber, colleges.map((c, i) => ({ collegeId: c.id,
    listVotes: ids.map(id => ({ listId: id, votes: Number(areas[i][id]) })) })), []);
  const subject = (id: string): Subject => { const g = Object.keys(groups).find(g => groups[g].includes(id)); return g ? { kind: 'coalition', id: g } : list(id); };
  const subjects = [...new Map(ids.map(id => [JSON.stringify(subject(id)), subject(id)])).values()];
  const listTargets = ids.map(id => ({ subject: subject(id), listId: id, votes: figures.ordinaryAllocation.lists.find(l => l.listId === id)!.votes, ordinarySeats: targets[id] }));
  const subjectTargets = subjects.map(s => ({ subject: s,
    votes: listTargets.filter(l => JSON.stringify(l.subject) === JSON.stringify(s)).reduce((n, l) => n + l.votes, 0),
    ordinarySeats: listTargets.filter(l => JSON.stringify(l.subject) === JSON.stringify(s)).reduce((n, l) => n + l.ordinarySeats, 0) }));
  const national: CameraNationalAllocationResult = { chamber: 'camera', simulationId: 13, seatTable: chamber.seatTable,
    ordinarySeats: Object.values(targets).reduce((n, v) => n + v, 0), status: 'complete', path: cap ? 'bonus-with-cap' : 'no-bonus',
    subjectTargets, listTargets, bonus: cap ? { subject: subjects[0], seats: 70 } : null, provisionalAllocation: null,
    capCheck: null, coalitionAllocations: [], draws: [], warnings: [],
    capCorrection: cap ? { majorityOrdinarySeats: targets.A, minorityOrdinarySeats: targets.B,
      majorityRawQuota: exactFraction(subjectTargets[0].votes, targets.A),
      majorityQuota: exactFraction(Math.floor(subjectTargets[0].votes / targets.A)),
      minorityAllocation: { status: 'complete', rawQuota: targets.B ? exactFraction(subjectTargets[1].votes, targets.B) : null,
        quota: targets.B ? exactFraction(Math.floor(subjectTargets[1].votes / targets.B)) : null,
        initialSeatTotal: null, assignments: [], resolutions: [], draws: [], warnings: [] } } : null };
  const input: CameraTerritorialInput = { chamber, figures, national };
  return structuredClone(input) as Mutable<CameraTerritorialInput>;
}

function collegeCommunication(f: CameraTerritorialInput): CameraConstituencyListResult {
  if (f.national.status !== 'complete') throw new Error();
  return { chamber: 'camera', simulationId: f.national.simulationId, seatTable: f.chamber.seatTable,
    matrices: [], draws: [], warnings: [], interpretations: [], status: 'complete',
    targets: f.national.listTargets.map(l => ({ ...l, territoryId: 'O' })) };
}

describe('N15-N18 Camera territorial stages', () => {
  it.each([false, true])('I12 constrains constituency subject and coalition-list corrections to filed territories (coalition=%s)', coalition => {
    const f = coalition ? fixture({ A: 0, B: 1, C: 1, E: 0 }, [
      { territory: 'X', seats: 1, A: 0, B: 0, C: 1, E: 99 },
      { territory: 'Y', seats: 1, A: 0, B: 0, C: 1, E: 99 }
    ], { G: ['B', 'C', 'E'] }) : fixture({ A: 0, B: 1, C: 1 }, [
      { territory: 'X', seats: 1, A: 99, B: 0, C: 1 },
      { territory: 'Y', seats: 1, A: 99, B: 0, C: 1 }
    ]);
    f.chamber.ordinarySlates = f.chamber.ordinarySlates.filter(s => !(s.listId === 'B' && s.collegeId === 'P1'));
    const constituencies = allocateCameraConstituencies(f, first);
    const lists = allocateCameraCoalitionListsInConstituencies(f, constituencies, first);
    expect(lists.status).toBe('complete');
    if (lists.status !== 'complete') throw new Error();
    expect(lists.targets.find(t => t.listId === 'B' && t.territoryId === 'X')?.ordinarySeats).toBe(1);
    expect(lists.targets.find(t => t.listId === 'B' && t.territoryId === 'Y')?.ordinarySeats).toBe(0);
  });
  it('composes constituency/list/college entitlements without preferences or candidate proclamations', () => {
    const r = allocateCameraTerritorialSeats(fixture(), noDraw);
    expect(r.status).toBe('complete');
    expect(r.colleges.status === 'complete' && r.colleges.targets.map(t => [t.collegeId, t.listId, t.ordinarySeats]))
      .toEqual([['P0', 'A', 2], ['P0', 'B', 1], ['P1', 'A', 1], ['P1', 'B', 1]]);
    expect(r.constituencies.interpretations).toEqual([]);
  });

  it('passes corrected constituency targets into one-college constituencies', () => {
    const f = fixture({ A: 3, B: 3 }, [{ territory: 'X', seats: 3, A: 180, B: 120 }, { territory: 'Y', seats: 3, A: 170, B: 130 }]);
    const r = allocateCameraTerritorialSeats(f, noDraw);
    expect(r.status).toBe('complete');
    expect(r.constituencies.matrices[0].transfers[0]).toMatchObject({ route: 'same-area' });
    expect(r.colleges.status === 'complete' && r.colleges.targets.map(t => t.ordinarySeats)).toEqual([2, 1, 1, 2]);
  });

  it('uses all coalition votes for subject allocation, admitted internal votes for lists, and positive-target lists for colleges', () => {
    const f = fixture({ A: 3, B: 1, E: 0, C: 2 }, [{ territory: 'O', seats: 6, A: 210, B: 90, E: 100, C: 200 }], { G: ['A', 'B', 'E'] });
    if (f.national.status !== 'complete') throw new Error();
    f.national.listTargets = f.national.listTargets.filter(l => l.listId !== 'E'); // non-admitted linked list
    const r = allocateCameraTerritorialSeats(f, noDraw);
    expect(r.status).toBe('complete');
    expect(r.constituencies.status === 'complete' && r.constituencies.targets.find(t => t.subject.id === 'G'))
      .toMatchObject({ votes: 400, ordinarySeats: 4 });
    expect(r.lists.matrices[0].areas[0].quota).toEqual(exactFraction(75)); // (210+90)/4
    expect(r.colleges.matrices[0].areas[0].quota).toEqual(exactFraction(83)); // (210+90+200)/6
    expect(r.colleges.status === 'complete' && r.colleges.targets.map(t => [t.listId, t.ordinarySeats])).toEqual([['A', 3], ['B', 1], ['C', 2]]);
  });

  it('leaves zero-seat coalitions complete without requiring a quota', () => {
    const f = fixture({ A: 0, B: 0, C: 6 }, [{ territory: 'O', seats: 6, A: 0, B: 0, C: 600 }], { G: ['A', 'B'] });
    const r = allocateCameraTerritorialSeats(f, noDraw);
    expect(r.status).toBe('complete');
    expect(r.lists.matrices[0]).toMatchObject({ status: 'complete', areas: [{ quota: null }] });
    expect(r.colleges.status === 'complete' && r.colleges.targets.map(t => t.listId)).toEqual(['C']);
  });

  it('uses corrected cap quotas and retains I05-v1 at the constituency boundary', () => {
    const f = fixture({ A: 3, B: 3 }, [{ territory: 'O', seats: 3, A: 180, B: 120 }, { territory: 'O', seats: 3, A: 170, B: 130 }], {}, true);
    const r = allocateCameraTerritorialSeats(f, noDraw);
    expect(r.status).toBe('complete');
    expect(r.constituencies.interpretations).toEqual([{ id: 'I05', version: 'I05-v1' }]);
    const cells = r.constituencies.matrices[0].cells;
    expect(cells[0].rawIndex).toEqual(exactFraction(350, 116));
    expect(cells[0].index).toEqual(exactFraction(3017241, 1000000));
    expect(cells[1].index).toEqual(exactFraction(3012048, 1000000));
    expect(r.colleges.matrices.map(m => m.contextId.split('/').at(-1))).toEqual(['groups', 'majority', 'minority']);
    expect(r.colleges.matrices[0].cells.every(c => c.rawIndex !== null && c.index !== null)).toBe(true);
    expect(r.colleges.status === 'complete' && r.colleges.targets.reduce((n, t) => n + t.ordinarySeats, 0)).toBe(6);
  });

  it('supports inactive zero-seat cap groups through colleges without dividing their quota', () => {
    const f = fixture({ A: 6, B: 0 }, [{ territory: 'O', seats: 3, A: 300, B: 0 }, { territory: 'O', seats: 3, A: 300, B: 0 }], {}, true);
    const r = allocateCameraTerritorialSeats(f, noDraw);
    expect(r.status).toBe('complete');
    expect(r.colleges.matrices[0].cells.filter(c => c.targetId === 'minority').every(c => c.rawIndex === null && c.seats === 0)).toBe(true);
    expect(r.colleges.matrices).toHaveLength(2); // groups and active majority
  });

  it('imports I14 into cap-group list splits and carries used decimals into recovery', () => {
    const f = fixture({ A: 1, B: 1, C: 2, D: 2 }, [
      { territory: 'O', seats: 3, A: 40, B: 35, C: 25, D: 50 },
      { territory: 'O', seats: 3, A: 100, B: 100, C: 0, D: 50 }
    ], { G: ['A', 'B', 'C'] }, true);
    const r = allocateCameraColleges(f, collegeCommunication(f), noDraw);
    expect(r.status).toBe('complete');
    const group = r.matrices[0], lists = r.matrices[1];
    expect(group.cells.filter(c => c.targetId === 'majority').map(c => c.seats)).toEqual([2, 2]);
    expect(group.warnings.some(w => w.interpretationId === 'I14')).toBe(false);
    expect(lists.cells.find(c => c.targetId === 'C' && c.areaId === 'P0')).toMatchObject({
      integerSeats: 0, provisionalSeats: 2, seats: 2, remainderState: 'used' });
    expect(r.warnings).toContainEqual(expect.objectContaining({ interpretationId: 'I14', severity: 'warning' }));
    expect(assembleRecoveryRemainders(f.chamber, { colleges: r })).toContainEqual(expect.objectContaining({
      collegeId: 'P0', subject: list('C'), value: exactFraction(1, 2), state: 'used' }));
  });

  it('keeps bonus-without-cap on the ordinary formula and selects the reduced college capacities', () => {
    const f = fixture();
    if (f.national.status !== 'complete') throw new Error();
    f.national.path = 'bonus-without-cap'; f.national.bonus = { subject: list('A'), seats: 70 };
    f.chamber.colleges.forEach(c => { c.seatsWithoutBonus += 10; });
    const r = allocateCameraTerritorialSeats(f, noDraw);
    expect(r.status).toBe('complete'); expect(r.constituencies.matrices[0].norm).toBe('N15');
    expect(r.constituencies.matrices[0].areas[0].capacity).toBe(5);
    expect(r.constituencies.interpretations).toEqual([]);
  });

  it('allocates college lists independently of a same-college transfer preference', () => {
    const f = fixture({ A: 2, B: 3, C: 1 }, [
      { territory: 'O', seats: 2, A: 40, B: 35, C: 25 },
      { territory: 'O', seats: 2, A: 45, B: 40, C: 15 },
      { territory: 'O', seats: 2, A: 47, B: 22, C: 31 }]);
    const r = allocateCameraColleges(f, collegeCommunication(f), noDraw);
    expect(r.status).toBe('complete');
    expect(r.matrices[0].areas.map(a => a.finalSeats)).toEqual([1, 2, 3]);
    expect(r.matrices[0].transfers.map(t => t.route)).toEqual(['removal', 'addition']);
  });

  it('retains cross-constituency corrections and resolves the integer donor gap under I13', () => {
    const f = fixture({ A: 2, B: 3, C: 2 }, [
      { territory: 'X', seats: 2, A: 40, B: 35, C: 25 },
      { territory: 'Y', seats: 2, A: 45, B: 40, C: 15 },
      { territory: 'Z', seats: 3, A: 100, B: 45, C: 155 }]);
    const r = allocateCameraTerritorialSeats(f, noDraw);
    expect(r.constituencies.status).toBe('complete'); expect(r.lists.status).toBe('complete');
    expect(r.constituencies.matrices[0].areas.map(a => a.finalSeats)).toEqual([1, 2, 4]);
    expect(r.colleges.status).toBe('complete');
    expect(r.colleges.matrices[0].areas[0]).toMatchObject({ capacity: 2, finalSeats: 1 });
    expect(r.colleges.warnings).toContainEqual(expect.objectContaining({ code: 'I13_INTEGER_SEAT_REMOVAL', severity: 'warning' }));
    const remainders = assembleRecoveryRemainders(f.chamber, r);
    expect(remainders.find(row => row.collegeId === 'P0' && row.subject.id === 'B'))
      .toMatchObject({ value: exactFraction(1, 17), state: 'unused' });
    const transfers = allocationTransfers([f.chamber], [{ chamber: 'camera', cameraTerritorial: r } as ReformChamberExecution]);
    expect(transfers).toContainEqual(expect.objectContaining({ interpretationId: 'I13',
      from: expect.objectContaining({ collegeId: 'P0', listId: 'B' }), to: null,
      remainders: [expect.objectContaining({ before: 'unused', after: 'unused' })] }));
  });

  it('carries I13 cap-group corrections into list targets and recovery without changing college capacities', () => {
    // Group quotas A=100/1, B=500/5. P0/P1 give A both seats, P2 gives
    // B both seats. After two remainder removals, A must cede an integer seat.
    const f = fixture({ A: 1, B: 5 }, [
      { territory: 'O', seats: 2, A: 50, B: 10 },
      { territory: 'O', seats: 2, A: 50, B: 10 },
      { territory: 'O', seats: 2, A: 0, B: 480 }], {}, true);
    const r = allocateCameraColleges(f, collegeCommunication(f), first);
    expect(r.status).toBe('complete');
    expect(r.matrices[0].transfers[2]).toMatchObject({ interpretationId: 'I13',
      fromCellId: '["P0","majority"]', toCellId: '["P0","minority"]', route: 'same-area',
      remainders: [expect.objectContaining({ before: 'consumed', after: 'consumed' }), expect.anything()] });
    expect(r.matrices[0].areas.map(a => a.finalSeats)).toEqual([2, 2, 2]);
    if (r.status !== 'complete') throw new Error();
    expect(['A', 'B'].map(id => r.targets.filter(t => t.listId === id).reduce((n, t) => n + t.ordinarySeats, 0))).toEqual([1, 5]);
    expect(assembleRecoveryRemainders(f.chamber, { colleges: r }).every(row => !row.id.includes('/groups'))).toBe(true);
    expect(r.warnings.filter(w => w.interpretationId === 'I13')).toHaveLength(1);
  });

  it('completes N15-N18 after a real cross-constituency correction changes local totals', () => {
    // N15 gives A/B/C (1,2,1), (0,1,1), (1,2,1). B's used 51/60
    // in X is removed; A's unused 4/80 in Y receives the cross-area seat.
    const f = fixture({ A: 3, B: 4, C: 3 }, [
      { territory: 'X', seats: 2, A: 38, B: 33, C: 76 },
      { territory: 'X', seats: 2, A: 7, B: 78, C: 9 },
      { territory: 'Y', seats: 2, A: 4, B: 91, C: 66 },
      { territory: 'Z', seats: 2, A: 9, B: 36, C: 27 },
      { territory: 'Z', seats: 2, A: 22, B: 25, C: 12 }]);
    const before = structuredClone(f), r = allocateCameraTerritorialSeats(f, noDraw);
    expect(r.status).toBe('complete');
    expect(r.constituencies.matrices[0].areas.map(a => [a.capacity, a.finalSeats])).toEqual([[4, 3], [2, 3], [4, 4]]);
    expect(r.constituencies.matrices[0].transfers[0]).toMatchObject({ route: 'cross-area' });
    expect(r.colleges.matrices.map(m => m.areas.map(a => a.finalSeats))).toEqual([[2, 1], [3], [2, 2]]);
    expect(r.colleges.matrices[0].transfers[0]).toMatchObject({ route: 'removal', fromCellId: '["P1","A"]', toCellId: null });
    expect(r.colleges.matrices[1].transfers[0]).toMatchObject({ route: 'addition', fromCellId: null, toCellId: '["P2","A"]' });
    if (r.colleges.status !== 'complete') throw new Error();
    expect(['A', 'B', 'C'].map(id => r.colleges.status === 'complete' && r.colleges.targets.filter(t => t.listId === id)
      .reduce((n, t) => n + t.ordinarySeats, 0))).toEqual([3, 4, 3]);
    expect(f).toEqual(before);
  });

  it.each(['no-bonus', 'bonus-without-cap'] as const)('reconciles different ordinary local totals for %s and exports honest correction evidence', path => {
    const f = fixture({ A: 3, B: 3 }, [
      { territory: 'X', seats: 2, A: 40, B: 60 },
      { territory: 'X', seats: 2, A: 45, B: 55 },
      { territory: 'Y', seats: 2, A: 60, B: 40 }]);
    if (f.national.status !== 'complete') throw new Error();
    f.national.path = path;
    f.national.bonus = path === 'no-bonus' ? null : { subject: list('A'), seats: 70 };
    const lists: CameraConstituencyListResult = { ...collegeCommunication(f), status: 'complete', targets: [
      { territoryId: 'X', subject: list('A'), listId: 'A', votes: 85, ordinarySeats: 1 },
      { territoryId: 'X', subject: list('B'), listId: 'B', votes: 115, ordinarySeats: 2 },
      { territoryId: 'Y', subject: list('A'), listId: 'A', votes: 60, ordinarySeats: 2 },
      { territoryId: 'Y', subject: list('B'), listId: 'B', votes: 40, ordinarySeats: 1 }] };
    const colleges = allocateCameraColleges(f, lists, noDraw);
    expect(colleges.status).toBe('complete');
    if (colleges.status !== 'complete') throw new Error();
    expect(colleges.matrices.map(m => m.areas.map(a => [a.capacity, a.finalSeats]))).toEqual([[[2, 1], [2, 2]], [[2, 3]]]);
    expect(['A', 'B'].map(id => colleges.targets.filter(t => t.listId === id).reduce((n, t) => n + t.ordinarySeats, 0))).toEqual([3, 3]);
    const remainders = assembleRecoveryRemainders(f.chamber, { colleges });
    expect(remainders.find(r => r.collegeId === 'P0' && r.subject.id === 'A')).toMatchObject({ state: 'consumed', value: exactFraction(4, 5) });
    expect(remainders.find(r => r.collegeId === 'P2' && r.subject.id === 'A')).toMatchObject({ state: 'used', value: exactFraction(1, 5) });
    const executions = [{ chamber: 'camera', cameraTerritorial: { constituencies: { matrices: [] }, lists: { matrices: [] }, colleges } }] as unknown as ReformChamberExecution[];
    const transfers = allocationTransfers([f.chamber], executions);
    expect(transfers).toMatchObject([
      { from: { territoryId: 'X', collegeId: 'P0', listId: 'A' }, to: null },
      { from: null, to: { territoryId: 'Y', collegeId: 'P2', listId: 'A' } }]);
    expect(JSON.parse(JSON.stringify(transfers))).toEqual(transfers);
  });

  it('retains the cap-group local capacity blocker before any group allocation or draw', () => {
    const f = fixture({ A: 3, B: 3 }, [{ territory: 'X', seats: 3, A: 180, B: 120 }, { territory: 'Y', seats: 3, A: 170, B: 130 }], {}, true);
    const lists: CameraConstituencyListResult = { ...collegeCommunication(f), status: 'complete', targets: [
      { territoryId: 'X', subject: list('A'), listId: 'A', votes: 180, ordinarySeats: 1 },
      { territoryId: 'X', subject: list('B'), listId: 'B', votes: 120, ordinarySeats: 1 },
      { territoryId: 'Y', subject: list('A'), listId: 'A', votes: 170, ordinarySeats: 2 },
      { territoryId: 'Y', subject: list('B'), listId: 'B', votes: 130, ordinarySeats: 2 }] };
    expect(allocateCameraColleges(f, lists, noDraw)).toMatchObject({ status: 'non-computable',
      reason: 'constituency-capacity-target-mismatch', affectedId: 'X', matrices: [], draws: [] });
  });

  it('rejects communications that change national list entitlements', () => {
    const f = fixture(), lists = collegeCommunication(f);
    if (lists.status !== 'complete') throw new Error();
    expect(() => allocateCameraColleges(f, { ...lists, targets: lists.targets.map(t => ({ ...t, ordinarySeats: t.ordinarySeats + 1 })) }, noDraw))
      .toThrow(/preserve national list entitlements/);
  });

  it('blocks incomplete national prerequisites without consuming draws or publishing targets', () => {
    const f = fixture();
    f.national = { ...f.national, status: 'non-computable', stage: 'provisional', reason: 'zero-quota' };
    const r = allocateCameraTerritorialSeats(f, noDraw);
    expect(r.status).toBe('non-computable');
    expect(r.constituencies).toMatchObject({ status: 'non-computable', reason: 'national-allocation-non-computable', matrices: [], draws: [] });
    expect(r.constituencies).not.toHaveProperty('targets');
  });

  it('rejects the wrong cap branch, chamber, seat source or simulation communication', () => {
    const f = fixture();
    expect(() => allocateCameraCapConstituencies(f, noDraw)).toThrow(/cap path/);
    const c = allocateCameraConstituencies(f, noDraw);
    expect(() => allocateCameraCoalitionListsInConstituencies(f, { ...c, simulationId: 999 }, noDraw)).toThrow(/matching/);
    f.chamber.chamber = 'senato'; expect(() => allocateCameraConstituencies(f, noDraw)).toThrow(/matching/);
    f.chamber.chamber = 'camera'; f.chamber.seatTable = { ...f.chamber.seatTable, version: 'changed' };
    expect(() => allocateCameraConstituencies(f, noDraw)).toThrow(/matching/);
  });

  it('rejects missing figures and inconsistent national subject/list targets', () => {
    const f = fixture(); f.figures.territories = [];
    expect(() => allocateCameraConstituencies(f, noDraw)).toThrow(/figure/);
    const g = fixture(); if (g.national.status !== 'complete') throw new Error();
    g.national.listTargets[0].ordinarySeats++;
    expect(() => allocateCameraConstituencies(g, noDraw)).toThrow(/capacity/);
  });

  it('preserves special territories outside every ordinary allocation denominator and matrix', () => {
    const f = fixture();
    f.chamber.territories.push({ id: 'TAA', regionId: 'TAA', kind: 'taa', bonusSeats: 0 });
    f.chamber.colleges.push({ id: 'SPECIAL', territoryId: 'TAA', kind: 'special-uninominal', seatsWithBonus: 1, seatsWithoutBonus: 1 });
    f.figures.national.validVotes += 9000000;
    const r = allocateCameraTerritorialSeats(f, noDraw);
    expect(r).toEqual(allocateCameraTerritorialSeats(fixture(), noDraw));
  });

  it('has stable, owned, JSON-safe output after source-array permutations', () => {
    const f = fixture({ A: 3, B: 3 }, [{ territory: 'X', seats: 3, A: 150, B: 150 }, { territory: 'Y', seats: 3, A: 150, B: 150 }]);
    const before = structuredClone(f);
    const r = allocateCameraTerritorialSeats(f, createSeededDrawContext(13, 'legal'));
    expect(f).toEqual(before);
    f.chamber.territories.reverse(); f.chamber.colleges.reverse(); f.figures.territories.reverse();
    if (f.national.status !== 'complete') throw new Error();
    f.national.subjectTargets.reverse(); f.national.listTargets.reverse();
    expect(allocateCameraTerritorialSeats(f, createSeededDrawContext(13, 'legal'))).toEqual(r);
    expect(JSON.parse(JSON.stringify(r))).toEqual(r);
    expect(r.constituencies.seatTable).not.toBe(f.chamber.seatTable);
  });
});
