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
    expect(r.matrices[0].transfers[0]).toMatchObject({ route: 'cross-area' });
  });

  it('retains cross-constituency corrections and reports an incompatible next-stage local capacity', () => {
    const f = fixture({ A: 2, B: 3, C: 2 }, [
      { territory: 'X', seats: 2, A: 40, B: 35, C: 25 },
      { territory: 'Y', seats: 2, A: 45, B: 40, C: 15 },
      { territory: 'Z', seats: 3, A: 100, B: 45, C: 155 }]);
    const r = allocateCameraTerritorialSeats(f, noDraw);
    expect(r.constituencies.status).toBe('complete'); expect(r.lists.status).toBe('complete');
    expect(r.constituencies.matrices[0].areas.map(a => a.finalSeats)).toEqual([1, 2, 4]);
    expect(r.colleges).toMatchObject({ status: 'non-computable', reason: 'constituency-capacity-target-mismatch', affectedId: 'X' });
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
