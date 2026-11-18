import type { Chamber, ChamberInput, OrdinarySlate } from './input';
import type { CollegeListFigure } from './figures';
import { supplied } from './contracts.fixture';

/** Small independent legal-stage inputs; no generator or national seat fixture. */
export function figureFixture(side: Chamber = 'camera'): {
  chamber: ChamberInput; ordinary: CollegeListFigure[]; special: CollegeListFigure[];
} {
  const lists = ['A', 'B', 'X', 'S', 'Z'].map(id => ({ id, name: id,
    coalitionId: ['A', 'B', 'X'].includes(id) ? 'C' : null, protectedMinority: null }));
  const ordinary = [row('P1', ['A', 'B', 'X'], [64, 32, 11]),
    row('P2', ['A', 'B', 'X'], [10, 10, 0]), row('P3', ['A', 'B', 'X'], [5, 15, 0])];
  const special = [row('V0', ['S'], [50]), row('T1', ['A', 'B', 'X'], [3, 6, 1]), row('T2', ['A', 'B', 'X'], [7, 2, 1])];
  const chamber: ChamberInput = {
    chamber: side, lists, coalitions: [{ id: 'C', listIds: ['A', 'B', 'X'] }],
    regions: [{ id: 'R', autonomous: false }, { id: 'V', autonomous: true }, { id: 'T', autonomous: true }],
    territories: [{ id: 'O1', regionId: 'R', kind: 'ordinary', bonusSeats: 0 },
      { id: 'O2', regionId: 'R', kind: 'ordinary', bonusSeats: 0 },
      { id: 'V', regionId: 'V', kind: 'vda', bonusSeats: 0 }, { id: 'T', regionId: 'T', kind: 'taa', bonusSeats: 0 }],
    colleges: [
      ...['P1', 'P2', 'P3'].map(id => ({ id, territoryId: id === 'P3' ? 'O2' : 'O1', kind: 'ordinary' as const, seatsWithoutBonus: 1, seatsWithBonus: 1 })),
      ...['V0', 'T1', 'T2'].map(id => ({ id, territoryId: id === 'V0' ? 'V' : 'T', kind: 'special-uninominal' as const, seatsWithoutBonus: 1, seatsWithBonus: 1 })),
      ...(side === 'camera' ? [{ id: 'TP', territoryId: 'T', kind: 'taa-proportional' as const, seatsWithoutBonus: 3, seatsWithBonus: 3 }] : [])
    ],
    seatTable: { kind: 'constructed', reference: 'figure tests only', version: 'test-1' },
    ordinarySlates: ordinary.flatMap(r => r.listVotes.map(v => preferenceSlate(r.collegeId, v.listId))),
    bonusSlates: [], ordinaryVotes: [], specialVotes: [],
    specialCandidatures: special.map(r => ({ collegeId: r.collegeId, candidateId: `candidate-${r.collegeId}`,
      linkedListIds: r.listVotes.map(v => v.listId), affiliationListId: null }))
  };
  return { chamber, ordinary, special };
}

export function row(collegeId: string, ids: readonly string[], counts: readonly number[]): CollegeListFigure {
  return { collegeId, listVotes: ids.map((listId, i) => ({ listId, votes: counts[i] })) };
}

export function preferenceSlate(collegeId = 'P1', listId = 'A'): OrdinarySlate {
  const candidateIds = Array.from({ length: 7 }, (_, i) => `${collegeId}-${listId}-${i}`);
  return { collegeId, listId, candidateIds, preferences: { kind: 'counts',
    counts: candidateIds.slice(1).map((candidateId, i) => ({ candidateId, count: [20, 30, 30, 0, 0, 15][i] })),
    provenance: supplied, validity: { kind: 'adjudicated', reference: 'valid test counts' } } };
}
