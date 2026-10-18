import type { Candidate, Chamber, ChamberInput, College, OrdinarySlate, SpecialCandidature } from './input';
import { aggregateElectoralFigures, sumVoteCounts, type CollegeListFigure } from './figures';
import { supplied } from './contracts.fixture';

/** Constructed stage geography, not a national seat table or a generated model. */
export function admissionFixture(
  side: Chamber,
  votes: Record<string, Record<string, number>>,
  groups: Record<string, string[]> = {},
  minorities: Record<string, string> = {},
  specialCount = 0,
  specialDistribution?: readonly Record<string, number>[]
) {
  const ids = [...new Set(Object.values(votes).flatMap(r => Object.keys(r)))];
  const candidates: Candidate[] = [];
  const ordinary: CollegeListFigure[] = [];
  const special: CollegeListFigure[] = [];
  const chamber: ChamberInput = {
    chamber: side, regions: Object.keys(votes).map(id => ({ id, autonomous: id === 'T' })),
    territories: Object.keys(votes).map(id => ({ id, regionId: id, kind: id === 'T' && specialCount ? 'taa' : 'ordinary', bonusSeats: 0 })),
    colleges: [], lists: ids.map(id => ({ id, name: id,
      coalitionId: Object.keys(groups).find(g => groups[g].includes(id)) ?? null,
      protectedMinority: minorities[id] ? { regionId: minorities[id], legalReference: 'constructed protected minority' } : null })),
    coalitions: Object.entries(groups).map(([id, listIds]) => ({ id, listIds })),
    seatTable: { kind: 'constructed', reference: 'N07-N09 stage fixture', version: 'test-1' },
    ordinarySlates: [], bonusSlates: [], specialCandidatures: [], ordinaryVotes: [], specialVotes: []
  };
  const colleges: College[] = [], slates: OrdinarySlate[] = [], candidatures: SpecialCandidature[] = [];
  const specialVotes: ChamberInput['specialVotes'][number][] = [];
  for (const [region, counts] of Object.entries(votes)) {
    const count = region === 'T' && specialCount ? specialCount : 1;
    for (let i = 0; i < count; i++) {
      const collegeId = `${region}${i}`;
      const isSpecial = region === 'T' && specialCount > 0;
      colleges.push({ id: collegeId, territoryId: region, kind: isSpecial ? 'special-uninominal' as const : 'ordinary' as const,
        seatsWithoutBonus: 1, seatsWithBonus: 1 });
      const listVotes = Object.entries(counts).map(([listId, n]) => ({ listId,
        votes: isSpecial && specialDistribution ? specialDistribution[i][listId] : Math.floor(n / count) + (i < n % count ? 1 : 0) }));
      (isSpecial ? special : ordinary).push({ collegeId, listVotes });
      if (!isSpecial) {
        slates.push(...listVotes.map(row => ({ collegeId, listId: row.listId, candidateIds: [`${collegeId}-${row.listId}`],
          preferences: { kind: 'missing' as const, reason: 'No preferences needed for admission' } })));
      } else {
        const subjectIds = [...new Set(listVotes.map(row => chamber.lists.find(l => l.id === row.listId)!.coalitionId ?? row.listId))];
        const totals = subjectIds.map(subject => {
          const linkedListIds = listVotes.filter(row => (chamber.lists.find(l => l.id === row.listId)!.coalitionId ?? row.listId) === subject).map(r => r.listId);
          const candidateId = `${collegeId}-${subject}`;
          candidates.push({ id: candidateId, name: candidateId, birthDate: '1980-01-01', gender: 'female',
            availability: { kind: 'available' }, externalCandidacies: [] });
          candidatures.push({ collegeId, candidateId, linkedListIds, affiliationListId: linkedListIds[0] });
          return { candidateId, votes: sumVoteCounts(listVotes.filter(r => linkedListIds.includes(r.listId)).map(r => r.votes)) };
        });
        specialVotes.push({ collegeId, electorate: { minimumAge: 18 as const, electors: null },
          votes: { kind: 'attributed' as const, listVotes, candidateTotals: totals, provenance: supplied } });
      }
    }
  }
  const completed: ChamberInput = { ...chamber, colleges, ordinarySlates: slates, specialCandidatures: candidatures, specialVotes };
  return { chamber: completed, candidates, simulationId: 9, figures: aggregateElectoralFigures(completed, ordinary, special) };
}
