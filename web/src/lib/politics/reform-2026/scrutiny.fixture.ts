import { jointFixture, supplied, type Fixture } from './contracts.fixture';
import type { ChamberInput } from './input';

/** Constructed full domestic election, with six-seat colleges and disjoint
 * seven-person slates. Uniform figures make national/regional targets checkable
 * by hand; the fixture does not call any scrutiny/allocation function. */
export function completeScrutinyFixture(majority = 45, linked = true): Fixture {
  const f = jointFixture();
  if (f.renewal.kind !== 'joint') throw new Error();
  for (const c of [f.renewal.camera, f.renewal.senato]) {
    const camera = c.chamber === 'camera', count = camera ? 64 : 32;
    c.lists = ['A', 'B', 'C'].map(id => ({ id, name: id, coalitionId: null, protectedMinority: null }));
    c.colleges = c.colleges.filter(p => p.kind !== 'ordinary');
    c.ordinarySlates = []; c.ordinaryVotes = []; c.bonusSlates = [];
    const votes = [majority * 1000, (100 - majority) * 700, (100 - majority) * 300];
    function person(id: string, index: number) {
      f.candidates.push({ id, name: id, birthDate: '1980-01-01', gender: index % 2 ? 'female' : 'male',
        availability: { kind: 'available' }, externalCandidacies: [] });
      return id;
    }
    for (let i = 0; i < count; i++) {
      const id = `P${String(i).padStart(2, '0')}`, seats = camera || i < 29 ? 6 : 5;
      c.colleges.push({ id, territoryId: 'O', kind: 'ordinary', seatsWithoutBonus: seats,
        seatsWithBonus: seats - 1 - Number(i < (camera ? 6 : 3)) });
    }
    function slate(collegeId: string, listId: string, offset: number) {
      const candidateIds = Array.from({ length: 7 }, (_, j) => person(`${c.chamber}-${collegeId}-${listId}-${j}`, j + offset));
      c.ordinarySlates.push({ collegeId, listId, candidateIds, preferences: { kind: 'counts',
        counts: candidateIds.slice(1).map((candidateId, j) => ({ candidateId, count: 60 - j * 10 })),
        provenance: supplied, validity: { kind: 'adjudicated', reference: 'constructed distinct valid preference counts' } } });
    }
    for (const [i, p] of c.colleges.filter(p => p.kind !== 'special-uninominal').entries()) {
      for (const listId of ['A', 'B', 'C']) slate(p.id, listId, i);
      // The TAA row is a view, not an additional ballot contribution.
      c.ordinaryVotes.push({ collegeId: p.id, electorate: { minimumAge: 18, electors: 200000 },
        votes: { kind: 'attributed', listVotes: c.lists.map((l, j) => ({ listId: l.id, votes: votes[j] })), provenance: supplied } });
    }
    for (const listId of ['A', 'B', 'C']) {
      const slates = c.ordinarySlates.filter(s => s.listId === listId && s.collegeId !== 'TP');
      const candidates = slates.map(s => ({ candidateId: s.candidateIds[0], affiliationListId: listId }));
      candidates.push(...slates.slice(0, camera ? 6 : 3).map(s => ({ candidateId: s.candidateIds[1], affiliationListId: listId })));
      c.bonusSlates.push({ territoryId: 'O', subject: { kind: 'list', id: listId }, candidates });
    }
    c.specialCandidatures = [];
    for (const [i, p] of c.colleges.filter(p => p.kind === 'special-uninominal').entries()) {
      const candidateTotals = c.lists.map((l, j) => {
        const candidateId = person(`${c.chamber}-special-${p.id}-${l.id}`, i);
        c.specialCandidatures.push({ collegeId: p.id, candidateId, linkedListIds: [l.id], affiliationListId: l.id });
        return { candidateId, votes: votes[j] };
      });
      const row = c.specialVotes.find(r => r.collegeId === p.id)!;
      row.electorate.electors = 200000;
      row.votes = { kind: 'attributed', listVotes: c.lists.map((l, j) => ({ listId: l.id, votes: votes[j] })), candidateTotals, provenance: supplied };
    }
  }
  if (!linked) f.bonusLinks = [];
  // Unused people from the base contract fixture carry no candidatures.
  return f;
}

export function renewedChambers(f: Fixture): ChamberInput[] {
  return f.renewal.kind === 'joint' ? [f.renewal.camera, f.renewal.senato] : [f.renewal.chamber];
}

/** A/C coalition, with personal affiliations retained on a mixed bonus slate. */
export function coalitionScrutinyFixture(): Fixture {
  const f = completeScrutinyFixture();
  if (f.renewal.kind !== 'joint') throw new Error();
  for (const c of [f.renewal.camera, f.renewal.senato]) {
    for (const l of c.lists) if (l.id !== 'B') l.coalitionId = 'G';
    c.coalitions = [{ id: 'G', listIds: ['A', 'C'] }];
    const a = c.bonusSlates.find(s => s.subject.id === 'A')!, other = c.bonusSlates.find(s => s.subject.id === 'C')!;
    const first = c.chamber === 'camera' ? 35 : 18;
    const candidates = [...a.candidates.slice(0, first), ...other.candidates.slice(0, a.candidates.length - first)];
    c.bonusSlates = c.bonusSlates.filter(s => s.subject.id === 'B');
    c.bonusSlates.push({ territoryId: 'O', subject: { kind: 'coalition', id: 'G' }, candidates });
    c.specialCandidatures = c.specialCandidatures.filter(s => s.affiliationListId !== 'C');
    for (const s of c.specialCandidatures) if (s.affiliationListId === 'A') s.linkedListIds.push('C');
    for (const row of c.specialVotes) {
      const a = row.votes.candidateTotals.find(p => p.candidateId.endsWith('-A'))!;
      a.votes += row.votes.candidateTotals.find(p => p.candidateId.endsWith('-C'))!.votes;
      row.votes.candidateTotals = row.votes.candidateTotals.filter(p => !p.candidateId.endsWith('-C'));
    }
  }
  f.bonusLinks = [{ camera: { kind: 'coalition', id: 'G' }, senato: { kind: 'coalition', id: 'G' }, cameraDeclares: true, senatoDeclares: true }];
  return f;
}
