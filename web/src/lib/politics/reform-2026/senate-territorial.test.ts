import { describe, expect, it } from 'vitest';
import { determineSenateAdmission } from './admission';
import { admissionFixture } from './admission.fixture';
import { getBundledReformSeatTables } from './bundled-data';
import { exactFraction as f, compareExact } from './exact';
import { aggregateElectoralFigures } from './figures';
import type { Subject } from './input';
import type { ScrutinyExecutionContext } from './result';
import type { SenateRegionalAllocationResult } from './senate-regional';
import {
  allocateSenateCapColleges, allocateSenateColleges, allocateSenateListsAndColleges, allocateSenateRegionalLists,
  type SenateTerritorialInput, type SenateCollegeResult
} from './senate-territorial';
import { createSeededDrawContext } from './ties';
import { assembleRecoveryRemainders } from './recovery-communications';
import { allocationTransfers } from './scrutiny-transfers';

const noDraw: ScrutinyExecutionContext = { draw() { throw new Error('Unexpected draw'); } };
const first: ScrutinyExecutionContext = { draw(r) { return { ...r, chosenId: r.alternativeIds[0], streamId: 'controlled' }; } };
const list = (id: string): Subject => ({ kind: 'list', id });
type Mutable<T> = T extends object ? { -readonly [K in keyof T]: Mutable<T[K]> } : T;
type Area = { region: string; seats: number; votes: Record<string, number> };

/** Tiny N19/N20 communications, independent of national cap calculation.
 * They are ordinary entitlement fixtures, not full candidature envelopes. */
function fixture(targets: Record<string, Record<string, number>> = { R: { A: 3, B: 2 } },
  areas: readonly Area[] = [{ region: 'R', seats: 3, votes: { A: 210, B: 90 } }, { region: 'R', seats: 2, votes: { A: 90, B: 110 } }],
  groups: Record<string, string[]> = {}, path: 'no-bonus' | 'bonus-without-cap' | 'bonus-with-cap' = 'no-bonus'): Mutable<SenateTerritorialInput> {
  const ids = Object.keys(areas[0].votes);
  const totals = Object.fromEntries(Object.keys(targets).map(region => [region, Object.fromEntries(ids.map(id => [id,
    areas.filter(a => a.region === region).reduce((n, a) => n + a.votes[id], 0)]))]));
  const a = admissionFixture('senato', totals, groups);
  const chamber = { ...a.chamber, colleges: areas.map((a, i) => ({ id: `P${i}`, territoryId: a.region, kind: 'ordinary' as const,
    seatsWithBonus: a.seats, seatsWithoutBonus: a.seats })), ordinarySlates: areas.flatMap((_, i) => ids.map(listId => ({
      collegeId: `P${i}`, listId, candidateIds: [], preferences: { kind: 'missing' as const, reason: 'Entitlements only' }
    }))) };
  const figures = aggregateElectoralFigures(chamber, areas.map((a, i) => ({ collegeId: `P${i}`, listVotes: ids.map(listId => ({ listId, votes: a.votes[listId] })) })), []);
  const admission = determineSenateAdmission({ chamber, figures, simulationId: 9 }, first);
  const subject = (id: string): Subject => groups[id] ? { kind: 'coalition', id } : list(id);
  const rows = Object.entries(targets).flatMap(([regionId, subjects]) => Object.entries(subjects).map(([id, ordinarySeats]) => ({ regionId,
    subject: subject(id), ordinarySeats, votes: (groups[id] ?? [id]).reduce((n, l) => n + totals[regionId][l], 0) })));
  const majority = subject(Object.keys(targets[Object.keys(targets)[0]])[0]);
  const regional: SenateRegionalAllocationResult = { chamber: 'senato', simulationId: 9, seatTable: structuredClone(chamber.seatTable),
    status: 'complete', path, ordinarySeats: areas.reduce((n, a) => n + a.seats, 0), regions: [], provisionalTotals: [],
    targets: rows, subjectTotals: [], draws: [], warnings: [], bonus: path === 'no-bonus' ? null : { subject: majority, seats: 35 },
    cap: path !== 'bonus-with-cap' ? null : { status: 'complete', targets: rows,
      check: { subject: majority, provisionalOrdinarySeats: 80, specialSeats: 0, bonusSeats: 35, provisionalDomesticSeats: 115, exceedsCap: true },
      majorityOrdinaryTarget: 78, ranking: [], movements: [], remainders: [], draws: [], warnings: [], interpretations: [{ id: 'I06', version: 'I06-v1' }] }
  };
  const input: SenateTerritorialInput = { chamber, figures, admission, regional };
  return structuredClone(input) as Mutable<SenateTerritorialInput>;
}
function worked(targets: number[], votes: number[][], seats: number[], path: 'no-bonus' | 'bonus-with-cap' = 'no-bonus') {
  const ids = targets.map((_, i) => String.fromCharCode(65 + i));
  return fixture({ R: Object.fromEntries(ids.map((id, i) => [id, targets[i]])) }, votes.map((v, i) => ({ region: 'R', seats: seats[i],
    votes: Object.fromEntries(ids.map((id, j) => [id, v[j]])) })), {}, path);
}
function run(input = fixture(), execution = noDraw) { return allocateSenateListsAndColleges(input, execution); }
function complete(input = fixture(), execution = noDraw) {
  const r = run(input, execution);
  expect(r.status).toBe('complete');
  if (r.lists.status !== 'complete' || r.colleges.status !== 'complete') throw new Error(JSON.stringify(r));
  for (const t of r.lists.targets) expect(r.colleges.targets.filter(c => c.regionId === t.regionId && c.listId === t.listId)
    .reduce((n, c) => n + c.ordinarySeats, 0)).toBe(t.ordinarySeats);
  for (const region of Object.keys(Object.fromEntries(r.lists.targets.map(t => [t.regionId, true])))) {
    expect(r.colleges.targets.filter(t => t.regionId === region).reduce((n, t) => n + t.ordinarySeats, 0))
      .toBe(input.regional.status === 'complete' ? input.regional.targets.filter(t => t.regionId === region).reduce((n, t) => n + t.ordinarySeats, 0) : -1);
  }
  return r as typeof r & { lists: Extract<typeof r.lists, { status: 'complete' }>; colleges: Extract<SenateCollegeResult, { status: 'complete' }> };
}

describe('N21 Senate regional coalition-list allocation', () => {
  it('splits corrected regional seats only among 2-bis/2-ter members, using an integer quota', () => {
    // A/D >=3%; E is the best excluded member; F remains excluded.
    // G=961 votes at subject level; admitted sum=960, seats=5, q=192.
    const input = fixture({ R: { G: 5, B: 3 } }, [{ region: 'R', seats: 8, votes: { A: 900, D: 50, E: 10, F: 1, B: 600 } }], { G: ['A', 'D', 'E', 'F'] });
    const r = complete(input);
    expect(r.lists.targets.map(t => [t.listId, t.ordinarySeats])).toEqual([['A', 5], ['D', 0], ['E', 0], ['B', 3]]);
    expect(r.lists.coalitionAllocations[0].allocation).toMatchObject({ quota: f(192), rawQuota: f(192),
      assignments: [{ id: 'A', integerSeats: 4, seats: 5, remainder: f(132) }, { id: 'D', seats: 0 }, { id: 'E', seats: 0 }] });
    expect(r.lists.targets.some(t => t.listId === 'F')).toBe(false);
    expect(r.colleges.matrices).toEqual([]); // single college takes the targets
  });
  it('uses regional list votes for equal coalition remainders', () => {
    // G sum600, q200: A=1.5, D=.5. A wins on regional 300>100.
    const r = complete(fixture({ R: { G: 3, B: 1 } }, [{ region: 'R', seats: 4, votes: { A: 300, D: 100, E: 200, B: 200 } }], { G: ['A', 'D', 'E'] }));
    expect(r.lists.targets.map(t => [t.listId, t.ordinarySeats])).toEqual([['A', 2], ['D', 0], ['E', 1], ['B', 1]]);
    expect(r.lists.coalitionAllocations[0].allocation.resolutions[0].steps.map(s => s.criterion))
      .toEqual(['remainder', 'regional-electoral-figure']);
  });
  it('drops the fractional part of the regional coalition quota', () => {
    // 14/6 -> q2. Integer allocation (5,1,0), whereas exact quota gives (4,1,1).
    const r = complete(fixture({ R: { G: 6, B: 4 } }, [{ region: 'R', seats: 10, votes: { A: 10, D: 3, E: 1, B: 10 } }], { G: ['A', 'D', 'E'] }));
    expect(r.lists.coalitionAllocations[0].allocation).toMatchObject({ rawQuota: f(7, 3), quota: f(2),
      assignments: [{ id: 'A', seats: 5 }, { id: 'D', seats: 1 }, { id: 'E', seats: 0 }] });
  });
  it('draws a terminal regional tie statutorily and keeps upstream events out of college output', () => {
    const r = complete(fixture({ R: { G: 3, B: 1 } }, [{ region: 'R', seats: 4, votes: { A: 200, D: 200, B: 200 } }], { G: ['A', 'D'] }), first);
    expect(r.lists.draws).toHaveLength(1);
    expect(r.lists.draws[0]).toMatchObject({ chamber: 'senato', norm: 'N21', rule: 'statutory' });
    expect(r.lists.warnings).toEqual([]); expect(r.colleges.draws).toEqual([]);
  });
  it('keeps separate list targets and totals in each region', () => {
    const r = complete(fixture({ R: { G: 3, B: 1 }, S: { G: 1, B: 3 } }, [
      { region: 'R', seats: 4, votes: { A: 600, D: 200, B: 200 } }, { region: 'S', seats: 4, votes: { A: 100, D: 300, B: 600 } }
    ], { G: ['A', 'D'] }));
    expect(r.lists.targets.map(t => [t.regionId, t.listId, t.ordinarySeats])).toEqual([
      ['R', 'A', 2], ['R', 'D', 1], ['R', 'B', 1], ['S', 'A', 0], ['S', 'D', 1], ['S', 'B', 3]
    ]);
  });
  it('passes single-college regional targets through even when a local quota would be zero', () => {
    const r = complete(worked([3, 2], [[1, 1]], [5]));
    expect(r.colleges.targets.map(t => t.ordinarySeats)).toEqual([3, 2]);
    expect(r.colleges.matrices).toEqual([]);
  });
  it('preserves coalition ownership of the bonus instead of splitting it among lists', () => {
    const input = fixture({ R: { G: 3, B: 1 } }, [{ region: 'R', seats: 4, votes: { A: 600, D: 200, B: 200 } }], { G: ['A', 'D'] }, 'bonus-with-cap');
    const before = structuredClone(input.regional);
    const r = complete(input);
    expect(r.lists.targets.reduce((n, t) => n + t.ordinarySeats, 0)).toBe(4);
    expect(input.regional).toEqual(before); expect(input.regional).toMatchObject({ bonus: { subject: { kind: 'coalition', id: 'G' }, seats: 35 } });
  });
  it('skips division for a coalition with zero regional entitlement', () => {
    const r = complete(fixture({ R: { G: 0, B: 4 } }, [{ region: 'R', seats: 4, votes: { A: 1, D: 1, B: 10 } }], { G: ['A', 'D'] }));
    expect(r.lists.coalitionAllocations[0].allocation).toMatchObject({ status: 'complete', quota: null, assignments: [{ seats: 0 }, { seats: 0 }] });
  });
  it.each([
    ['zero-vote-total', { A: 0, D: 0, B: 1000 }],
    ['zero-quota', { A: 1, D: 1, B: 1 }],
    ['initial-overallocation', { A: 5, D: 1, B: 1 }]
  ])('retains coalition I04 %s without final targets', (reason, votes) => {
    const input = fixture({ R: { G: 4, B: 1 } }, [{ region: 'R', seats: 5, votes }], { G: ['A', 'D'] });
    // Construct N08 admission even for a zero-support coalition to exercise I04.
    if (input.admission.status !== 'complete') throw new Error();
    input.admission.admittedSubjects = [{ kind: 'coalition', id: 'G' }, list('B')];
    input.admission.coalitions[0].admitted = true; input.admission.coalitions[0].admittedListIds = ['A', 'D'];
    const r = run(input);
    expect(r.lists).toMatchObject({ status: 'non-computable', reason, affectedRegionId: 'R', draws: [] });
    expect(r.lists).not.toHaveProperty('targets'); expect(r.colleges).not.toHaveProperty('targets');
  });
});

describe('N21 ordinary Senate college allocation', () => {
  it('allocates integer parts and remainders with exact traces', () => {
    const r = complete();
    expect(r.colleges.targets.map(t => [t.collegeId, t.listId, t.ordinarySeats])).toEqual([
      ['P0', 'A', 2], ['P0', 'B', 1], ['P1', 'A', 1], ['P1', 'B', 1]
    ]);
    expect(r.colleges.matrices[0]).toMatchObject({ chamber: 'senato', norm: 'N21', initialTieFigure: 'college-electoral-figure',
      areas: [{ rawQuota: f(100), quota: f(100) }, { rawQuota: f(100), quota: f(100) }] });
  });
  it('uses college votes when equal decimals favor the opposite list regionally', () => {
    // P0 q100: A1.5/B.5 -> A (150>50). Regional votes favor B 240>160.
    const r = complete(worked([2, 2], [[150, 50], [10, 190]], [2, 2]));
    expect(r.colleges.matrices[0].cells.find(c => c.id === '["P0","A"]')).toMatchObject({ remainderSeat: true });
    expect(r.colleges.matrices[0].resolutions[0].steps.map(s => s.criterion)).toEqual(['decimal-part', 'college-electoral-figure']);
    expect(r.colleges.draws).toEqual([]);
  });
  it('corrects surplus/deficit by independently ranked college decimals', () => {
    const r = complete(worked([2, 3, 1], [[40, 35, 25], [45, 40, 15], [47, 22, 31]], [2, 2, 2]));
    expect(r.colleges.matrices[0].transfers[0]).toMatchObject({ fromCellId: '["P0","A"]', toCellId: '["P2","B"]', route: 'cross-area',
      remainders: [{ before: 'used', after: 'consumed' }, { before: 'unused', after: 'used' }] });
    expect(r.colleges.matrices[0].areas.map(a => a.finalSeats)).toEqual([1, 2, 3]);
  });
  it('excludes lists whose integer regional target is reached before residual allocation', () => {
    const r = complete(worked([2, 1, 1], [[190, 10, 0], [110, 50, 40]], [2, 2]));
    expect(r.colleges.matrices[0].cells.filter(c => c.targetId === 'A').map(c => [c.remainderExcluded, c.remainderSeat, c.seats]))
      .toEqual([[true, false, 1], [true, false, 1]]);
  });
  it('omits zero-target lists from college denominators', () => {
    const input = worked([3, 2, 0], [[210, 90, 900], [90, 110, 900]], [3, 2]);
    const r = complete(input);
    expect(r.colleges.matrices[0].areas.map(a => a.quota)).toEqual([f(100), f(100)]);
    expect(r.colleges.targets.some(t => t.listId === 'C')).toBe(false);
  });
  it('keeps bonus-without-cap on S17(4), with reduced college capacities', () => {
    const input = fixture(); if (input.regional.status !== 'complete') throw new Error();
    input.regional.path = 'bonus-without-cap'; input.regional.bonus = { subject: list('A'), seats: 35 };
    input.chamber.colleges.forEach(c => { c.seatsWithoutBonus += 20; });
    const r = complete(input);
    expect(r.colleges.matrices.map(m => m.norm)).toEqual(['N21']);
    expect(r.colleges.matrices[0].areas.map(a => a.capacity)).toEqual([3, 2]);
  });
  it('I16 combines with integer donors and carries used recipient evidence into recovery and transfers', () => {
    const input = worked([1, 3], [[140, 60], [130, 70]], [2, 2]);
    const r = complete(input), m = r.colleges.matrices[0];
    expect(m.transfers[0]).toMatchObject({ interpretationId: 'I13', interpretationIds: ['I13', 'I16'],
      toCellId: '["P1","B"]', remainders: [{ before: 'unused', after: 'unused' }, { before: 'used', after: 'used' }] });
    expect(r.colleges.warnings).toContainEqual(expect.objectContaining({ code: 'I16_REUSED_RECIPIENT_REMAINDER', severity: 'warning', interpretationId: 'I16' }));
    expect(assembleRecoveryRemainders(input.chamber, r)).toContainEqual(expect.objectContaining({ collegeId: 'P1', subject: list('B'), state: 'used', value: f(7, 10) }));
    expect(allocationTransfers([input.chamber], [{ chamber: 'senato', senateTerritorial: r, admission: input.admission } as unknown as Parameters<typeof allocationTransfers>[1][number]]))
      .toContainEqual(expect.objectContaining({ interpretationId: 'I13', interpretationIds: ['I13', 'I16'] }));
  });
  it('I13 completes Campania 31 after strict donors, exporting unchanged unused donor evidence', () => {
    const input = worked([11, 3, 1, 2, 1], [
      [1204066, 201193, 71775, 105738, 89770],
      [758885, 168070, 60688, 148719, 70725]
    ], [10, 8]);
    const r = complete(input), m = r.colleges.matrices[0];
    expect(m.transfers.map(t => [t.fromCellId, t.toCellId, t.interpretationId])).toEqual([
      ['["P1","E"]', '["P0","C"]', undefined], ['["P1","A"]', '["P0","B"]', 'I13']
    ]);
    expect(m.cells.find(c => c.id === '["P1","A"]')).toMatchObject({
      integerSeats: 5, provisionalSeats: 5, seats: 4, remainderState: 'unused', decimalPart: f(892, 30177)
    });
    expect(r.colleges.warnings.filter(w => w.interpretationId === 'I13')).toEqual([
      expect.objectContaining({ code: 'I13_INTEGER_SEAT_REMOVAL', severity: 'warning', chamber: 'senato', lawReference: 'S17(4)(a)-(c)' })
    ]);
    expect(assembleRecoveryRemainders(input.chamber, r)).toContainEqual(expect.objectContaining({
      collegeId: 'P1', subject: list('A'), value: f(892, 30177), state: 'unused'
    }));
    const transfers = allocationTransfers([input.chamber], [{ chamber: 'senato', senateTerritorial: r, admission: input.admission } as unknown as Parameters<typeof allocationTransfers>[1][number]]);
    expect(transfers).toContainEqual(expect.objectContaining({ chamber: 'senato', norm: 'N21', interpretationId: 'I13',
      from: expect.objectContaining({ collegeId: 'P1', listId: 'A' }),
      remainders: [expect.objectContaining({ before: 'unused', after: 'unused' }), expect.anything()] }));
    expect(m.draws).toEqual([]);
  });
  it('skips zero-capacity college divisions while preserving positive regional targets', () => {
    const r = complete(worked([3, 2], [[0, 0], [300, 200]], [0, 5]));
    expect(r.colleges.matrices[0].areas[0]).toMatchObject({ capacity: 0, quota: null, finalSeats: 0 });
    expect(r.colleges.targets.filter(c => c.collegeId === 'P0').every(c => c.ordinarySeats === 0)).toBe(true);
  });
  it('groups colleges by region across separate ordinary territory IDs', () => {
    const input = fixture(), expected = complete(input);
    input.chamber.territories.push({ ...input.chamber.territories[0], id: 'R-other' });
    input.chamber.colleges[1].territoryId = 'R-other';
    const r = complete(input);
    expect(r.lists.targets).toEqual(expected.lists.targets);
    expect(r.colleges.targets.filter(t => t.collegeId === 'P1').every(t => t.regionId === 'R' && t.territoryId === 'R-other')).toBe(true);
    expect(r.colleges.matrices).toEqual(expected.colleges.matrices);
  });
  it('records statutory residual ties and warned I03 correction ties as Senate events', () => {
    const r = complete(worked([3, 3], [[150, 150], [150, 150]], [3, 3]), first);
    expect(r.colleges.draws.map(d => d.rule)).toEqual(['statutory', 'statutory', 'I03', 'I03']);
    expect(r.colleges.draws.every(d => d.chamber === 'senato' && d.norm === 'N21')).toBe(true);
    expect(r.colleges.warnings).toHaveLength(2);
    expect(r.colleges.warnings[0]).toMatchObject({ severity: 'warning', interpretationId: 'I03' });
  });
  it.each([
    ['zero-vote-total', [2, 2], [[0, 0], [100, 100]], [2, 2]],
    ['zero-quota', [2, 2], [[1, 1], [100, 100]], [3, 1]],
    ['initial-overallocation', [3, 2, 2], [[10, 1, 1], [1, 1, 1]], [7, 0]]
  ] as const)('reports college I04 %s', (reason, targets, votes, seats) => {
    const r = run(worked([...targets], votes.map(v => [...v]), [...seats]));
    expect(r.colleges).toMatchObject({ status: 'non-computable', reason, affectedRegionId: 'R', draws: [] });
    expect(r.colleges).not.toHaveProperty('targets');
  });
  it('I14 repeats the nonempty ranking formerly reported as impossible-residual', () => {
    const r = complete(worked([1, 1, 2], [[40, 35, 25], [100, 100, 0]], [2, 2]));
    expect(r.colleges.matrices[0].cells.filter(c => c.targetId === 'C').map(c => c.seats)).toEqual([2, 0]);
    expect(r.colleges.warnings).toEqual([expect.objectContaining({ interpretationId: 'I14', chamber: 'senato',
      severity: 'warning', message: expect.stringContaining('Reached regional targets excluded') })]);
  });
  it('carries I14 donor consumption and I16 recipient reuse through recovery and transfer exports', () => {
    const input = worked([4, 5, 1, 1, 1, 1], [
      [20, 19, 16, 15, 15, 15], [20, 19, 16, 15, 15, 15], [20, 19, 16, 15, 15, 15], [0, 0, 100, 100, 100, 100]
    ], [3, 3, 3, 4]);
    const r = complete(input, first), m = r.colleges.matrices[0];
    // The unused zero decimal in P3 is used before I16 reuses a used decimal.
    expect(m.transfers.map(t => t.interpretationId)).toEqual(['I14', 'I14']);
    expect(m.transfers.map(t => t.interpretationIds)).toEqual([undefined, ['I14', 'I16']]);
    const donor = m.cells.find(c => c.id === m.transfers[0].fromCellId)!;
    expect(assembleRecoveryRemainders(input.chamber, r)).toContainEqual(expect.objectContaining({
      collegeId: donor.areaId, subject: list(donor.targetId), value: donor.decimalPart, state: 'consumed' }));
    expect(allocationTransfers([input.chamber], [{ chamber: 'senato', senateTerritorial: r, admission: input.admission } as unknown as Parameters<typeof allocationTransfers>[1][number]]))
      .toContainEqual(expect.objectContaining({ chamber: 'senato', norm: 'N21', interpretationIds: ['I14', 'I16'] }));
  });
});

describe('N22 Senate cap college groups and lists', () => {
  it('imports I14 into the internal list split without repeating group-index remainders', () => {
    const input = worked([2, 1, 1, 2], [[100, 40, 35, 25], [200, 100, 100, 0]], [3, 3], 'bonus-with-cap');
    const r = complete(input);
    const [groups, majority, minority] = r.colleges.matrices;
    expect(groups.cells.filter(c => c.targetId === 'minority').map(c => c.seats)).toEqual([2, 2]);
    expect(groups.warnings.some(w => w.interpretationId === 'I14')).toBe(false);
    expect(majority.warnings.some(w => w.interpretationId === 'I14')).toBe(false);
    expect(minority.cells.filter(c => c.targetId === 'D').map(c => [c.provisionalSeats, c.seats, c.remainderState]))
      .toEqual([[2, 2, 'used'], [0, 0, 'unused']]);
    expect(minority.warnings).toEqual([expect.objectContaining({ code: 'I14_REPEATED_REMAINDER_PASS',
      chamber: 'senato', lawReference: 'S17(5)(d); S17(4)(a)-(c)' })]);
    expect(minority.transfers).toEqual([]);
    expect(assembleRecoveryRemainders(input.chamber, r))
      .toContainEqual(expect.objectContaining({ collegeId: 'P0', subject: list('D'), state: 'used' }));
  });
  it('replays repeated I13 group removals with I03 draws after input permutations', () => {
    const input = worked([1, 9], [[50, 4], [50, 4], [50, 4], [0, 888]], [2, 2, 2, 4], 'bonus-with-cap');
    const seeded = () => createSeededDrawContext('senate-I13', 'legal');
    const r = complete(input, seeded()), m = r.colleges.matrices[0];
    expect(m.transfers.filter(t => t.interpretationId === 'I13')).toHaveLength(2);
    expect(m.transfers.slice(0, 3).every(t => !t.interpretationId)).toBe(true);
    expect(m.transfers.slice(3).every(t => t.route === 'same-area' &&
      t.remainders[0].before === 'consumed' && t.remainders[0].after === 'consumed')).toBe(true);
    expect(m.areas.map(a => a.finalSeats)).toEqual([2, 2, 2, 4]);
    expect(m.cells.every(c => c.seats >= 0)).toBe(true);
    expect(m.draws.length).toBeGreaterThan(0);
    expect(m.draws.every(d => d.rule === 'I03')).toBe(true);
    expect(JSON.parse(JSON.stringify(r))).toEqual(r);
    input.chamber.colleges.reverse(); input.chamber.lists.reverse(); input.figures.colleges.reverse();
    input.figures.colleges.forEach(c => c.figures.lists.reverse());
    expect(complete(input, seeded())).toEqual(r);
  });
  it('I13 removes an integer group seat after strict donors and completes internal lists and recovery evidence', () => {
    const input = worked([1, 5], [[50, 10], [50, 10], [0, 480]], [2, 2, 2], 'bonus-with-cap');
    const r = complete(input, first), m = r.colleges.matrices[0];
    expect(m.transfers.map(t => t.interpretationId)).toEqual([undefined, undefined, 'I13']);
    expect(m.transfers[2]).toMatchObject({ route: 'same-area', fromCellId: '["P0","majority"]', toCellId: '["P0","minority"]',
      remainders: [expect.objectContaining({ before: 'consumed', after: 'consumed' }), expect.anything()] });
    expect(m.areas.map(a => a.finalSeats)).toEqual([2, 2, 2]);
    expect(m.cells.every(c => c.seats >= 0)).toBe(true);
    expect(r.colleges.matrices).toHaveLength(3);
    expect(r.colleges.warnings.filter(w => w.interpretationId === 'I13')).toEqual([
      expect.objectContaining({ chamber: 'senato', severity: 'warning', lawReference: 'S17(5)(a)-(c)' })
    ]);
    expect(assembleRecoveryRemainders(input.chamber, r).every(row => !row.id.includes('groups'))).toBe(true);
  });
  it('truncates both indices before normalization, changing a provisional seat', () => {
    const input = worked([1, 1], [[1000000, 2000000], [500001, 1000003]], [1, 1], 'bonus-with-cap');
    const r = complete(input, first), m = r.colleges.matrices[0];
    const a = m.cells.find(c => c.areaId === 'P0' && c.targetId === 'majority')!;
    const b = m.cells.find(c => c.areaId === 'P0' && c.targetId === 'minority')!;
    expect(a).toMatchObject({ rawIndex: f(1000000, 1500001), index: f(666666, 1000000), quotient: f(1, 2) });
    expect(b.index).toEqual(f(666666, 1000000)); expect(compareExact(a.rawIndex!, b.rawIndex!)).toBe(1);
    expect(b.remainderSeat).toBe(true); // raw indices award A; tied indices use college B votes
    expect(m.resolutions[0].steps.map(s => s.criterion)).toEqual(['decimal-part', 'college-electoral-figure']);
    expect(r.colleges.matrices.every(m => m.chamber === 'senato' && m.norm === 'N22')).toBe(true);
  });
  it('uses college group votes even when regional votes favor the other group', () => {
    // Divisors600/200; P0 indices .5/.5, college A300>B100, region B800>A600.
    const r = complete(worked([1, 4], [[300, 100], [300, 700]], [1, 4], 'bonus-with-cap'));
    expect(r.colleges.matrices[0].cells.find(c => c.areaId === 'P0' && c.targetId === 'majority')?.remainderSeat).toBe(true);
    expect(r.colleges.draws).toEqual([]);
  });
  it('corrects groups within the same college before running list matrices', () => {
    // R divisors A400, B400; P0/P1 indices1.5/1.5, both choose A.
    // Target A3/B3, provisional A4/B2 -> one same-college transfer.
    const r = complete(worked([3, 3], [[600, 600], [600, 600]], [3, 3], 'bonus-with-cap'), first);
    const m = r.colleges.matrices[0];
    expect(m.transfers).toHaveLength(1); expect(m.transfers[0].route).toBe('same-area');
    expect(m.areas.map(a => a.finalSeats)).toEqual([3, 3]);
    expect(r.colleges.matrices).toHaveLength(3);
    expect(r.colleges.matrices.slice(1).every(m => m.lawReference.startsWith('S17(5)(d)'))).toBe(true);
  });
  it.each([true, false])('skips an inactive zero-target group (majority active=%s)', majorityActive => {
    const input = worked(majorityActive ? [4, 0] : [0, 4], [[200, 200], [200, 200]], [2, 2], 'bonus-with-cap');
    const r = complete(input), inactive = majorityActive ? 'minority' : 'majority';
    expect(r.colleges.matrices[0].cells.filter(c => c.targetId === inactive).every(c => c.rawIndex === null && c.index?.numerator === '0' && c.seats === 0)).toBe(true);
    expect(r.colleges.matrices).toHaveLength(2);
  });
  it('retains a zero-index-quota failure without inventing group weights', () => {
    const r = run(worked([3, 1], [[1, 10], [1, 10]], [2, 2], 'bonus-with-cap'));
    expect(r.colleges).toMatchObject({ status: 'non-computable', reason: 'zero-index-quota', affectedRegionId: 'R', draws: [] });
  });
  it('applies college vote ties inside a coalition majority after the group allocation', () => {
    const r = complete(fixture({ R: { G: 4, B: 2 } }, [
      { region: 'R', seats: 3, votes: { A: 150, D: 50, B: 100 } }, { region: 'R', seats: 3, votes: { A: 50, D: 150, B: 100 } }
    ], { G: ['A', 'D'] }, 'bonus-with-cap'));
    expect(r.colleges.matrices[1].cells.find(c => c.id === '["P0","A"]')?.remainderSeat).toBe(true);
    expect(r.colleges.matrices[1].cells.find(c => c.id === '["P1","D"]')?.remainderSeat).toBe(true);
  });
  it('exposes the independently callable cap boundary and rejects an ordinary path', () => {
    const input = worked([3, 3], [[600, 600], [600, 600]], [3, 3], 'bonus-with-cap');
    const ls = allocateSenateRegionalLists(input, noDraw);
    expect(allocateSenateCapColleges(input, ls, first)).toEqual(allocateSenateColleges(input, ls, first));
    const ordinary = fixture(); expect(() => allocateSenateCapColleges(ordinary, allocateSenateRegionalLists(ordinary, noDraw), noDraw)).toThrow(/cap path/);
  });
});

describe('Senate territorial stage communication and replay', () => {
  it('blocks incomplete upstream stages with no draws and no final targets', () => {
    const input = fixture(); input.regional = { ...input.regional, status: 'non-computable', stage: 'provisional', reason: 'zero-quota', affectedRegionId: 'R' };
    const r = run(input); expect(r.lists).toMatchObject({ status: 'non-computable', reason: 'regional-allocation-non-computable', draws: [] });
    expect(r.colleges).not.toHaveProperty('targets');
    const a = fixture(); a.admission = { chamber: 'senato', norm: 'N08', status: 'non-computable', reason: 'zero-valid-votes', draws: [], warnings: [] };
    expect(run(a).lists).toMatchObject({ status: 'non-computable', reason: 'admission-non-computable' });
  });
  it('rejects mismatched chamber, seat-table, run, capacity, duplicate targets or missing figures', () => {
    const a = fixture(); a.chamber.chamber = 'camera'; expect(() => run(a)).toThrow(/matching/);
    const b = fixture(); b.chamber.seatTable.version = 'changed'; expect(() => run(b)).toThrow(/matching/);
    const c = fixture(); const lists = allocateSenateRegionalLists(c, noDraw);
    expect(() => allocateSenateColleges(c, { ...lists, simulationId: 99 }, noDraw)).toThrow(/matching/);
    const d = fixture(); if (d.regional.status !== 'complete') throw new Error();
    d.regional.targets[0].ordinarySeats++; expect(() => run(d)).toThrow(/endowment/);
    const e = fixture(); if (e.regional.status !== 'complete') throw new Error();
    e.regional.targets.push(e.regional.targets[0]); expect(() => run(e)).toThrow(/subject targets/);
    const g = fixture(); g.figures.regions = []; expect(() => run(g)).toThrow(/figure/);
    const h = fixture(); h.figures.colleges = []; expect(() => run(h)).toThrow(/figure/);
  });
  it('rejects list communications that lose targets, change votes or link the wrong subject', () => {
    const a = fixture(); const ls = allocateSenateRegionalLists(a, noDraw); if (ls.status !== 'complete') throw new Error();
    expect(() => allocateSenateColleges(a, { ...ls, targets: [] }, noDraw)).toThrow(/each admitted/);
    expect(() => allocateSenateColleges(a, { ...ls, targets: ls.targets.map((t, i) => i ? t : { ...t, subject: list('B') }) }, noDraw)).toThrow(/each admitted/);
    expect(() => allocateSenateColleges(a, { ...ls, targets: ls.targets.map((t, i) => i ? t : { ...t, votes: t.votes + 1 }) }, noDraw)).toThrow(/disagrees/);
    const bad = fixture(); bad.figures.colleges[0].figures.lists[0].votes++; bad.figures.colleges[0].figures.validVotes++;
    expect(() => run(bad)).toThrow(/sum to/);
  });
  it('excludes national/special figures and retains provisional source metadata', () => {
    const a = fixture(); const before = run(a);
    a.chamber.territories.push({ id: 'T', regionId: 'T', kind: 'taa', bonusSeats: 0 });
    a.chamber.colleges.push({ id: 'SPECIAL', territoryId: 'T', kind: 'special-uninominal', seatsWithBonus: 1, seatsWithoutBonus: 1 });
    a.figures.national.validVotes = 10000000; a.figures.ordinaryAllocation.lists[0].votes = 0;
    expect(run(a)).toEqual(before);
    const p = fixture(); p.chamber.seatTable = structuredClone(getBundledReformSeatTables().chambers.find(c => c.chamber === 'senato')!.seatTable) as Mutable<SenateTerritorialInput['chamber']['seatTable']>;
    p.regional.seatTable = structuredClone(p.chamber.seatTable);
    expect(run(p).colleges.seatTable).toEqual(p.chamber.seatTable);
  });
  it('owns JSON-safe outputs and replays after all relevant source permutations', () => {
    const input = worked([3, 3], [[150, 150], [150, 150]], [3, 3]); const before = structuredClone(input);
    const r = complete(input, createSeededDrawContext('senate-territorial', 'legal'));
    expect(input).toEqual(before); expect(JSON.parse(JSON.stringify(r))).toEqual(r);
    expect(r.colleges.seatTable).not.toBe(input.chamber.seatTable);
    input.chamber.colleges.reverse(); input.chamber.territories.reverse(); input.chamber.lists.reverse(); input.figures.colleges.reverse(); input.figures.regions.reverse();
    input.figures.colleges.forEach(c => c.figures.lists.reverse()); input.figures.regions.forEach(c => c.figures.lists.reverse());
    if (input.admission.status !== 'complete' || input.regional.status !== 'complete') throw new Error();
    input.admission.admittedSubjects.reverse(); input.regional.targets.reverse();
    expect(complete(input, createSeededDrawContext('senate-territorial', 'legal'))).toEqual(r);
  });
});
