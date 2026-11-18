import { calculatePercentages, type ElectoralFigures } from './figures';
import type { Chamber } from './input';
import type { RecoveryInput } from './recovery';
import { exactFraction } from './exact';
import { supplied } from './contracts.fixture';

type Mutable<T> = T extends object ? { -readonly [K in keyof T]: Mutable<T[K]> } : T;
/** Small legal-stage fixture. All people start unavailable; tests explicitly
 * enable the destinations needed for their independently worked expectation. */
export function recoveryFixture(side: Chamber = 'senato'): Mutable<RecoveryInput> {
  const territories = ['R', 'S', 'T', 'U'];
  const colleges = ['R1', 'R2', 'S1', 'S2', 'T1', 'U1'];
  const lists = ['A', 'B'];
  const f = (id: string): Mutable<ElectoralFigures> => {
    const votes = id === 'S2' ? [12, 88] : [10, 90];
    return { validVotes: 100, lists: lists.map((listId, i) => ({ listId, votes: votes[i], percentage: calculatePercentages(votes, 100)[i] })),
      coalitions: [{ coalitionId: 'G', votes: 100, percentage: calculatePercentages([100], 100)[0] }] };
  };
  const chamber: Mutable<RecoveryInput['chamber']> = { chamber: side,
    regions: territories.map(id => ({ id, autonomous: false })),
    territories: territories.map(id => ({ id, regionId: id, kind: 'ordinary', bonusSeats: 0 })),
    colleges: colleges.map(id => ({ id, territoryId: id[0], kind: 'ordinary', seatsWithoutBonus: 1, seatsWithBonus: 1 })),
    seatTable: { kind: 'constructed', reference: 'worked recovery fixture', version: '1' },
    lists: lists.map(id => ({ id, name: id, coalitionId: 'G', protectedMinority: null })),
    coalitions: [{ id: 'G', listIds: lists }],
    ordinarySlates: colleges.flatMap(collegeId => lists.map(listId => {
      const candidateIds = Array.from({ length: 7 }, (_, i) => `${collegeId}-${listId}-${i}`);
      return { collegeId, listId, candidateIds, preferences: { kind: 'counts',
        counts: candidateIds.slice(1).map((candidateId, i) => ({ candidateId, count: [1, 6, 5, 4, 3, 2][i] })),
        provenance: supplied, validity: { kind: 'adjudicated', reference: 'supplied legal fixture' } } };
    })), ordinaryVotes: [], specialVotes: [], bonusSlates: [], specialCandidatures: [] };
  return { chamber,
    candidates: chamber.ordinarySlates.flatMap(s => s.candidateIds.map((id, i) => ({ id, name: id, birthDate: '1980-01-01', gender: i % 2 ? 'female' : 'male',
      availability: { kind: 'unavailable', reason: 'fixture' }, externalCandidacies: [] }))),
    figures: { colleges: colleges.map(collegeId => ({ collegeId, figures: f(collegeId) })), taaProportional: [], territories: [], regions: [], national: f(''), ordinaryAllocation: f('') },
    initial: { chamber: side, simulationId: 7, status: 'partial', blockers: [], seats: [{ seatId: 'seat-1', category: 'ordinary',
      entitlement: { chamber: side, territoryId: 'R', collegeId: 'R1', subject: { kind: 'coalition', id: 'G' }, listId: 'A' },
      status: 'unresolved', reason: 'candidate-exhaustion-requires-recovery' }], retained: [], released: [], decisions: [], draws: [], warnings: [] },
    eligibleLists: territories.flatMap(territoryId => lists.map(listId => ({ territoryId, listId, ordinarySeats: 1 }))),
    remainders: territories.flatMap(territoryId => [null, ...colleges.filter(id => id[0] === territoryId)].flatMap(collegeId =>
      [...lists.map(id => ({ kind: 'list' as const, id })), { kind: 'coalition' as const, id: 'G' }].map(subject => ({
        id: JSON.stringify([territoryId, collegeId, subject.id]), territoryId, collegeId, subject, value: exactFraction(1, 10), state: 'unused' as const })))) };
}
export function enable(input: Mutable<RecoveryInput>, ...ids: string[]): void {
  for (const id of ids) input.candidates.find(p => p.id === id)!.availability = { kind: 'available' };
}
export function remainder(input: Mutable<RecoveryInput>, territoryId: string, collegeId: string | null, subjectId: string,
  tenths: number, state: 'unused' | 'used' | 'consumed' = 'unused'): void {
  const r = input.remainders.find(r => r.territoryId === territoryId && r.collegeId === collegeId && r.subject.id === subjectId)!;
  r.value = exactFraction(tenths, 10); r.state = state;
}
export function bonusSeat(input: Mutable<RecoveryInput>, coalition = true): void {
  const s = input.initial.seats[0];
  s.category = 'bonus'; s.entitlement.collegeId = null;
  s.entitlement.subject = { kind: coalition ? 'coalition' : 'list', id: coalition ? 'G' : 'A' };
  s.entitlement.listId = coalition ? null : 'A';
}
