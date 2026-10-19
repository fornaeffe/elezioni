import { calculatePercentages, type ElectoralFigures } from './figures';
import type { Chamber } from './input';
import { supplied } from './contracts.fixture';
import type { ProclamationInput } from './proclamations';

type Mutable<T> = T extends object ? { -readonly [K in keyof T]: Mutable<T[K]> } : T;
/** Small independently worked stage communication, not national geography. */
export function proclamationFixture(side: Chamber = 'camera', counts = [3, 3]): Mutable<ProclamationInput> {
  const chamber: Mutable<ProclamationInput['chamber']> = {
    chamber: side, regions: [{ id: 'R', autonomous: false }],
    territories: [{ id: 'R', regionId: 'R', kind: 'ordinary', bonusSeats: 0 }],
    colleges: ['P1', 'P2'].map((id, i) => ({ id, territoryId: 'R', kind: 'ordinary', seatsWithBonus: counts[i], seatsWithoutBonus: counts[i] })),
    seatTable: { kind: 'constructed', reference: 'worked proclamation fixture', version: '1' },
    lists: ['A', 'B'].map(id => ({ id, name: id, coalitionId: null, protectedMinority: null })),
    coalitions: [], ordinarySlates: ['P1', 'P2'].map(collegeId => {
      const candidateIds = Array.from({ length: 7 }, (_, i) => `${collegeId}-${i}`);
      return { collegeId, listId: 'A', candidateIds, preferences: { kind: 'counts',
        counts: candidateIds.slice(1).map((candidateId, i) => ({ candidateId, count: [10, 20, 20, 0, 0, 5][i] })),
        provenance: supplied, validity: { kind: 'adjudicated', reference: 'valid supplied fixture counts' } } };
    }), bonusSlates: [], specialCandidatures: [], ordinaryVotes: [], specialVotes: []
  };
  const candidates = chamber.ordinarySlates.flatMap(s => s.candidateIds.map((id, i) => ({ id, name: id,
    birthDate: '1980-01-01', gender: i % 2 ? 'female' as const : 'male' as const,
    availability: { kind: 'available' as const }, externalCandidacies: [] })));
  const figure = (A: number, B: number): Mutable<ElectoralFigures> => ({ validVotes: A + B,
    lists: ['A', 'B'].map((listId, i) => ({ listId, votes: [A, B][i], percentage: calculatePercentages([A, B], A + B)[i] })), coalitions: [] });
  const f = figure(60, 40), g = figure(90, 10);
  const ordinaryBase = { simulationId: 7, seatTable: structuredClone(chamber.seatTable), status: 'complete' as const,
    matrices: [], draws: [], warnings: [], targets: counts.map((ordinarySeats, i) => ({ collegeId: `P${i + 1}`, territoryId: 'R', regionId: 'R',
      subject: { kind: 'list' as const, id: 'A' }, listId: 'A', votes: i ? 90 : 60, ordinarySeats })) };
  return {
    simulationId: 7, chamber, candidates,
    figures: { colleges: [{ collegeId: 'P1', figures: f }, { collegeId: 'P2', figures: g }],
      territories: [], regions: [], national: f, ordinaryAllocation: f, taaProportional: [] },
    ordinary: side === 'camera' ? { ...ordinaryBase, chamber: 'camera', interpretations: [] } :
      { ...ordinaryBase, chamber: 'senato', coalitionAllocations: [] },
    bonus: { norm: 'N12', simulationId: 7, renewedChambers: [side], lawReference: 'C83/S16-bis',
      firstSubjects: [], draws: [], warnings: [], status: 'not-awarded', awards: [], reasons: [{ code: 'single-chamber-renewal', chamber: side }] },
    special: { chamber: side, simulationId: 7, status: 'complete', seats: [],
      preliminary: { status: 'complete', chamber: side, simulationId: 7, winners: [], draws: [], warnings: [] } }
  };
}

/** Preserve the filed slot and its supplied preference count when sharing identity. */
export function repeatPerson(input: Mutable<ProclamationInput>, firstPosition: number, secondPosition: number): string {
  const [a, b] = input.chamber.ordinarySlates;
  const id = a.candidateIds[firstPosition], old = b.candidateIds[secondPosition];
  b.candidateIds[secondPosition] = id;
  if (b.preferences.kind === 'counts') for (const p of b.preferences.counts) if (p.candidateId === old) p.candidateId = id;
  return id;
}
