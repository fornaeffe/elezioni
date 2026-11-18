import type { Candidate, Chamber, ChamberInput, ReformScrutinyInput } from './input';

type Mutable<T> = T extends object ? { -readonly [K in keyof T]: Mutable<T[K]> } : T;
export type Fixture = Mutable<ReformScrutinyInput>;
export const supplied = { kind: 'supplied', reference: 'constructed test counts' } as const;

/** Small, invented geography with full national totals; not an official seat table. */
export function jointFixture(): Fixture {
  const candidates: Mutable<Candidate>[] = [];
  function chamber(chamber: Chamber): Mutable<ChamberInput> {
    const camera = chamber === 'camera';
    const data: Mutable<ChamberInput> = {
      chamber,
      regions: [{ id: 'O', autonomous: false }, { id: 'V', autonomous: true }, { id: 'T', autonomous: true }],
      territories: [
        { id: 'O', regionId: 'O', kind: 'ordinary', bonusSeats: camera ? 70 : 35 },
        { id: 'V', regionId: 'V', kind: 'vda', bonusSeats: 0 },
        { id: 'T', regionId: 'T', kind: 'taa', bonusSeats: 0 }
      ],
      colleges: [], seatTable: { kind: 'constructed', version: 'test-1', reference: 'contract fixture' },
      lists: [{ id: 'A', name: 'List A', coalitionId: null, protectedMinority: null }],
      coalitions: [], ordinarySlates: [], bonusSlates: [], specialCandidatures: [], ordinaryVotes: [], specialVotes: []
    };
    function person(id: string, index: number): string {
      const fullId = `${chamber}-${id}`;
      candidates.push({ id: fullId, name: fullId, birthDate: '1980-01-01', gender: index % 2 ? 'female' : 'male',
        availability: { kind: 'available' }, externalCandidacies: [] });
      return fullId;
    }
    function slate(id: string): string[] {
      const ids = Array.from({ length: 7 }, (_, i) => person(`${id}-${i}`, i));
      data.ordinarySlates.push({ collegeId: id, listId: 'A', candidateIds: ids, preferences: { kind: 'missing', reason: 'No preferences supplied' } });
      data.ordinaryVotes.push({ collegeId: id, electorate: { minimumAge: 18, electors: 200 },
        votes: { kind: 'attributed', listVotes: [{ listId: 'A', votes: 100 }], provenance: supplied } });
      return ids;
    }
    const bonus: { candidateId: string; affiliationListId: string }[] = [];
    const capacities = camera ? [39, 39, 39, 39, 38, 38, 38, 38, 38, 38] : [38, 38, 38, 38, 37];
    capacities.forEach((seats, i) => {
      const id = `O${i}`;
      data.colleges.push({ id, territoryId: 'O', kind: 'ordinary', seatsWithoutBonus: seats, seatsWithBonus: seats - 7 });
      bonus.push(...slate(id).map(candidateId => ({ candidateId, affiliationListId: 'A' })));
    });
    data.bonusSlates.push({ territoryId: 'O', subject: { kind: 'list', id: 'A' }, candidates: bonus });
    if (camera) {
      data.colleges.push({ id: 'TP', territoryId: 'T', kind: 'taa-proportional', seatsWithoutBonus: 3, seatsWithBonus: 3 });
      slate('TP');
    }
    const specialIds = ['V0', ...Array.from({ length: camera ? 4 : 6 }, (_, i) => `T${i}`)];
    specialIds.forEach((id, i) => {
      const candidateId = person(id, i);
      data.colleges.push({ id, territoryId: id[0], kind: 'special-uninominal', seatsWithoutBonus: 1, seatsWithBonus: 1 });
      data.specialCandidatures.push({ collegeId: id, candidateId, linkedListIds: ['A'], affiliationListId: 'A' });
      data.specialVotes.push({ collegeId: id, electorate: { minimumAge: 18, electors: 200 },
        votes: { kind: 'attributed', listVotes: [{ listId: 'A', votes: 100 }], candidateTotals: [{ candidateId, votes: 100 }], provenance: supplied } });
    });
    return data;
  }
  return {
    schemaVersion: 1, algorithmId: 'politiche-reform-2026-v1', simulationId: 1,
    electionDate: '2027-05-01', legalVersion: 'reform-test', geographyVersion: 'invented-1',
    randomness: { seed: 'contract-seed', streamVersion: 'legal-draws-v1' }, candidates,
    renewal: { kind: 'joint', camera: chamber('camera'), senato: chamber('senato') },
    bonusLinks: [{ camera: { kind: 'list', id: 'A' }, senato: { kind: 'list', id: 'A' }, cameraDeclares: true, senatoDeclares: true }],
    chamberModel: { kind: 'supplied', reference: 'constructed independent chamber counts' }
  };
}

export function singleFixture(side: Chamber): Fixture {
  const f = jointFixture();
  if (f.renewal.kind !== 'joint') throw new Error('Expected joint fixture');
  const chamber = f.renewal[side];
  chamber.bonusSlates = [];
  f.renewal = { kind: 'single', chamber };
  f.bonusLinks = [];
  f.candidates = f.candidates.filter(p => p.id.startsWith(side));
  return f;
}
