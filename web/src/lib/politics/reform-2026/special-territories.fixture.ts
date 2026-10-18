import { admissionFixture } from './admission.fixture';
import { preferenceSlate } from './electoral-figures.fixture';
import { aggregateElectoralFigures, type CollegeListFigure } from './figures';
import type { Chamber, ChamberInput } from './input';
import { supplied } from './contracts.fixture';

/** Independent stage fixtures; national seat-table validation uses singleFixture. */
export function specialAttributionFixture(counts = [60, 30, 10], only = 7, side: Chamber = 'camera') {
  const ids = counts.map((_, i) => ['A', 'B', 'X'][i]);
  const f = admissionFixture(side, { T: Object.fromEntries(ids.map((id, i) => [id, counts[i]])) },
    { C: ids }, {}, 1);
  const candidateId = f.chamber.specialCandidatures[0].candidateId;
  const chamber: ChamberInput = { ...f.chamber, specialVotes: [{ collegeId: 'T0',
    electorate: { minimumAge: 18, electors: null }, votes: { kind: 'components',
      directListVotes: ids.map((listId, i) => ({ listId, votes: counts[i] })),
      candidateOnlyVotes: [{ candidateId, votes: only }],
      candidateTotals: [{ candidateId, votes: counts.reduce((s, n) => s + n, 0) + only }], provenance: supplied } }] };
  return { ...f, chamber, collegeId: 'T0' };
}

export function taaFixture(local: Record<string, number>, seats = 3, groups: Record<string, string[]> = {},
  national?: Record<string, number>, minorities: Record<string, string> = {}) {
  const f = admissionFixture('camera', { O: national ?? Object.fromEntries(Object.entries(local).map(([id, n]) => [id, n * 100])), T: local },
    groups, minorities, 4);
  const slates = Object.keys(local).map(id => {
    const slate = preferenceSlate('TP', id);
    return { ...slate, preferences: { kind: 'counts' as const, provenance: supplied,
      validity: { kind: 'adjudicated' as const, reference: 'constructed valid preference counts' },
      counts: slate.candidateIds.slice(1).map((candidateId, i) => ({ candidateId,
        count: Math.min(local[id], [1, 3, 3, 0, 0, 2][i]) })) } };
  });
  const candidates = [...f.candidates, ...slates.flatMap(s => s.candidateIds.map((id, i) => ({
    id, name: id, birthDate: '1980-01-01', gender: i % 2 ? 'female' as const : 'male' as const,
    availability: { kind: 'available' as const }, externalCandidacies: [] })))];
  const chamber: ChamberInput = { ...f.chamber, colleges: [...f.chamber.colleges,
    { id: 'TP', territoryId: 'T', kind: 'taa-proportional', seatsWithoutBonus: seats, seatsWithBonus: seats }],
    ordinarySlates: [...f.chamber.ordinarySlates, ...slates] };
  const rows = f.figures.colleges.map(r => ({ collegeId: r.collegeId,
    listVotes: r.figures.lists.filter(l => chamber.colleges.find(c => c.id === r.collegeId)!.kind === 'ordinary'
      ? chamber.ordinarySlates.some(s => s.collegeId === r.collegeId && s.listId === l.listId)
      : chamber.specialCandidatures.some(s => s.collegeId === r.collegeId && s.linkedListIds.includes(l.listId)))
      .map(l => ({ listId: l.listId, votes: l.votes })) }));
  const ordinary: CollegeListFigure[] = rows.filter(r => r.collegeId === 'O0');
  const special = rows.filter(r => r.collegeId !== 'O0');
  return { chamber, candidates, figures: aggregateElectoralFigures(chamber, ordinary, special),
    simulationId: f.simulationId, collegeId: 'TP', ordinary, special };
}
