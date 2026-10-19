import { describe, expect, it } from 'vitest';
import { determineSenateAdmission, determineCameraAdmission } from './admission';
import { admissionFixture } from './admission.fixture';
import { allocateByQuotaAndRemainders } from './allocation';
import { determineGovernabilityBonus } from './bonus';
import { getBundledReformSeatTables } from './bundled-data';
import { jointFixture, singleFixture } from './contracts.fixture';
import { exactFraction as f } from './exact';
import { aggregateElectoralFigures } from './figures';
import type { Subject } from './input';
import type { ScrutinyExecutionContext } from './result';
import { allocateSenateRegions, applySenateCap, type SenateCapResult, type SenateRegionAllocation, type SenateRegionalInput } from './senate-regional';
import { allocateSenateListsAndColleges } from './senate-territorial';
import { attributeSpecialVotes } from './special-attribution';
import { proclaimSpecialUninominalWinners, summarizeSpecialSeats } from './special-seats';
import { determineSpecialWinners } from './special-winners';
import { createSeededDrawContext } from './ties';
import { validateReformScrutinyInput } from './validation';

const noDraw: ScrutinyExecutionContext = { draw() { throw new Error('Unexpected draw'); } };
const first: ScrutinyExecutionContext = { draw(r) { return { ...r, chosenId: r.alternativeIds[0], streamId: 'first' }; } };
const list = (id: string): Subject => ({ kind: 'list', id });
type Mutable<T> = T extends object ? { -readonly [K in keyof T]: Mutable<T[K]> } : T;

/** Constructed N06/N08/N10/N12 communications with the complete statutory
 * 189/154 ordinary + 7 special endowment; not a candidature envelope.
 */
function fixture(votes: Record<string, Record<string, number>> = { R: { A: 90000, B: 64000 } }, bonus = true,
  special = 0, capacities: Record<string, number> = { R: 154 }, groups: Record<string, string[]> = {}): Mutable<SenateRegionalInput> {
  const a = admissionFixture('senato', votes, groups);
  const subject: Subject = groups.G ? { kind: 'coalition', id: 'G' } : list('A');
  const admission = determineSenateAdmission(a, first);
  const chamber = { ...a.chamber,
    regions: [...a.chamber.regions, { id: 'V', autonomous: true }, { id: 'T', autonomous: true }],
    territories: [...a.chamber.territories.map((t, i) => ({ ...t, bonusSeats: i ? 0 : 35 })),
      { id: 'V', regionId: 'V', kind: 'vda' as const, bonusSeats: 0 }, { id: 'T', regionId: 'T', kind: 'taa' as const, bonusSeats: 0 }],
    colleges: [...a.chamber.colleges.map((c, i) => ({ ...c, seatsWithBonus: capacities[c.territoryId], seatsWithoutBonus: capacities[c.territoryId] + (i ? 0 : 35) })),
      ...Array.from({ length: 7 }, (_, i) => ({ id: `special${i}`, territoryId: i ? 'T' : 'V', kind: 'special-uninominal' as const, seatsWithBonus: 1, seatsWithoutBonus: 1 }))]
  };
  const input: SenateRegionalInput = { ...a, chamber, admission,
    bonus: { norm: 'N12', simulationId: 9, renewedChambers: ['camera', 'senato'], lawReference: 'constructed communication',
      firstSubjects: [], draws: [], warnings: [], ...(bonus ? { status: 'awarded', awards: [
        { chamber: 'camera', subject: list('A'), seats: 70 }, { chamber: 'senato', subject, seats: 35 }
      ] } : { status: 'not-awarded', awards: [], reasons: [{ code: 'missing-link' }] }) },
    specialSeats: { chamber: 'senato', status: 'complete', uninominalSeats: 7, taaProportionalSeats: 0, totalSeats: 7,
      subjects: [{ subject, uninominalSeats: special, taaProportionalSeats: 0, totalSeats: special },
        { subject: list('special-other'), uninominalSeats: 7 - special, taaProportionalSeats: 0, totalSeats: 7 - special }] }
  };
  return structuredClone(input) as Mutable<SenateRegionalInput>;
}
function run(input = fixture(), execution = noDraw) { return allocateSenateRegions(input, execution); }
function complete(input = fixture(), execution = noDraw) {
  const r = run(input, execution);
  expect(r.status).toBe('complete');
  if (r.status !== 'complete') throw new Error(JSON.stringify(r));
  return r;
}
function cap(regions: SenateRegionAllocation[], specialSeats = 0, execution = noDraw) {
  return applySenateCap({ regions, subject: list('A'), specialSeats, simulationId: 9 }, execution);
}
function region(regionId: string, ordinarySeats: number, votes: Record<string, number>, validVotes?: number): SenateRegionAllocation {
  return { regionId, ordinarySeats, validVotes: validVotes ?? Object.values(votes).reduce((n, v) => n + v, 0),
    subjects: Object.entries(votes).map(([id, votes]) => ({ subject: list(id), votes })),
    allocation: allocateByQuotaAndRemainders({ seats: ordinarySeats, quotaMode: 'integer',
      subjects: Object.entries(votes).map(([id, v]) => ({ id: JSON.stringify(['list', id]), figure: f(v), tieValues: [f(v)] })),
      tieCriteria: [{ name: 'regional-votes', direction: 'descending' }],
      context: { simulationId: 9, request: { chamber: 'senato', norm: 'N19', rule: 'statutory', lawReference: 'S16-bis(1)(f)', contextId: regionId } }
    }, noDraw) };
}
function conservation(r: SenateCapResult, regions: readonly SenateRegionAllocation[], special = 0) {
  expect(r.status).toBe('complete');
  if (r.status !== 'complete') throw new Error(JSON.stringify(r));
  for (const region of regions) expect(r.targets.filter(t => t.regionId === region.regionId).reduce((n, t) => n + t.ordinarySeats, 0)).toBe(region.ordinarySeats);
  if (r.check.exceedsCap) expect(r.targets.filter(t => t.subject.id === 'A').reduce((n, t) => n + t.ordinarySeats, 0) + special + 35).toBe(113);
  expect(r.movements.every(m => m.removal.seatsBefore - m.removal.seatsAfter === 1 && m.recipient.seatsAfter - m.recipient.seatsBefore === 1)).toBe(true);
}

describe('N19 Senate regional allocation', () => {
  it('uses all 189 ordinary seats without bonus, independently in each region', () => {
    const r = complete(fixture({ R: { A: 80000, B: 55000 }, S: { A: 10000, B: 44000 } }, false, 0, { R: 100, S: 54 }));
    expect(r).toMatchObject({ path: 'no-bonus', ordinarySeats: 189, bonus: null, cap: null });
    expect(r.targets.map(t => t.ordinarySeats)).toEqual([80, 55, 10, 44]);
  });
  it('retains the regional result below the cap', () => {
    const r = complete(fixture({ R: { A: 77000, B: 77000 } }));
    expect(r).toMatchObject({ path: 'bonus-without-cap', ordinarySeats: 154, bonus: { seats: 35 }, cap: { check: { provisionalDomesticSeats: 112, exceedsCap: false }, movements: [] } });
  });
  it('does not apply the bonus cap to a no-bonus supermajority', () => {
    const r = complete(fixture({ R: { A: 189000 } }, false, 7));
    expect(r).toMatchObject({ path: 'no-bonus', cap: null, subjectTotals: [{ ordinarySeats: 189 }] });
  });
  it('exposes the used-remainder ranking even when exactly 113 needs no correction', () => {
    const r = complete(fixture({ R: { A: 77500, B: 76500 } }));
    expect(r.cap).toMatchObject({ ranking: [{ regionId: 'R', remainder: f(500), rank: 1 }], movements: [], interpretations: [] });
    expect(r.draws).toEqual([]);
  });
  it.each([0, 1, 2, 3, 4, 5, 6, 7])('keeps exactly 113 with %i linked special seats', special => {
    const a = 78 - special;
    const r = complete(fixture({ R: { A: a * 1000, B: (154 - a) * 1000 } }, true, special));
    expect(r.path).toBe('bonus-without-cap');
    expect(r.cap?.check.provisionalDomesticSeats).toBe(113);
    expect(r.cap?.movements).toHaveLength(0);
  });
  it.each([0, 1, 2, 3, 4, 5, 6, 7])('corrects 114 to 113 including %i special seats and keeps the 35 bonus', special => {
    const a = 79 - special;
    const r = complete(fixture({ R: { A: a * 1000, B: (154 - a) * 1000 } }, true, special));
    expect(r).toMatchObject({ path: 'bonus-with-cap', bonus: { subject: list('A'), seats: 35 }, cap: { majorityOrdinaryTarget: 78 - special, check: { provisionalDomesticSeats: 114 } } });
    conservation(r.cap!, r.regions, special);
  });
  it('uses regional, rather than national, votes after equal initial remainders', () => {
    // q=100 in both. R: 20.5/114.5 -> B; S: 39.5/14.5 -> A,
    // although B has more votes nationally. No draw is required.
    const r = complete(fixture({ R: { A: 2050, B: 11450 }, S: { A: 3950, B: 1450 } }, false, 0, { R: 100, S: 54 }));
    expect(r.targets.map(t => t.ordinarySeats)).toEqual([20, 115, 40, 14]);
  });
  it('uses an unwarned statutory draw for equal initial remainder and regional votes', () => {
    const input = fixture({ R: { A: 5000, B: 5000 }, S: { A: 26500, B: 26500 } }, true, 0, { R: 101, S: 53 });
    const r = complete(input, first);
    expect(r.draws.filter(d => d.norm === 'N19').length).toBeGreaterThan(0);
    expect(r.draws.filter(d => d.norm === 'N19').every(d => d.rule === 'statutory')).toBe(true);
    expect(r.warnings.filter(w => w.drawContextId && r.draws.some(d => d.norm === 'N19' && d.contextId === w.drawContextId))).toEqual([]);
  });
  it('counts every coalition member at subject level and leaves internal list targets to N21', () => {
    const r = complete(fixture({ R: { A: 69500, D: 20000, E: 1000, F: 500, B: 63000 } }, true, 0, { R: 154 }, { G: ['A', 'D', 'E', 'F'] }), first);
    expect(r.regions[0].subjects).toContainEqual({ subject: { kind: 'coalition', id: 'G' }, votes: 91000 });
    expect(r.bonus?.subject).toEqual({ kind: 'coalition', id: 'G' });
    expect(r.targets).toHaveLength(2);
    expect(r).not.toHaveProperty('listTargets');
  });
  it('distinguishes standalone-list and coalition identities with the same ID', () => {
    const r = complete(fixture({ R: { A: 90000, G: 64000 } }, true, 0, { R: 154 }, { G: ['A'] }));
    expect(r.subjectTotals.map(t => [t.subject, t.ordinarySeats])).toEqual([
      [{ kind: 'coalition', id: 'G' }, 78], [list('G'), 76]
    ]);
  });
  it('does not use national/special votes or exclude non-admitted votes from the percentage denominator', () => {
    const input = fixture({ R: { A: 80000, B: 20000, X: 1000 }, S: { A: 44000, B: 10000, X: 0 } }, true, 0, { R: 100, S: 54 });
    input.figures.national.validVotes = 999999999;
    input.figures.ordinaryAllocation.lists[0].votes = 0;
    const r = complete(input);
    expect(r.regions[0]).toMatchObject({ validVotes: 101000, subjects: [{ subject: list('A'), votes: 80000 }, { subject: list('B'), votes: 20000 }] });
    expect(r.cap?.movements[0]).toMatchObject({ regionId: 'R', removal: { rankValue: f(8000000, 101000) } });
  });
  it('uses the bundled provisional endowment without changing its provenance', () => {
    const input = fixture();
    const bundle = getBundledReformSeatTables().chambers.find(c => c.chamber === 'senato')!;
    input.chamber = structuredClone({ ...input.chamber, ...bundle }) as Mutable<SenateRegionalInput['chamber']>;
    input.figures.regions = input.chamber.regions.map(r => ({ regionId: r.id, figures: structuredClone(input.figures.regions[0].figures) }));
    const r = complete(input, first);
    expect(r.seatTable).toEqual(bundle.seatTable);
    expect(r.seatTable.kind).toBe('provisional-census');
    conservation(r.cap!, r.regions);
  });
});

describe('N20 Senate cap progression under I06', () => {
  it('ranks raw used vote remainders across regions, rather than normalized decimals', () => {
    const regions = [region('R', 100, { A: 51500, B: 48500 }), region('S', 54, { A: 2755, B: 2645 })];
    const r = cap(regions);
    expect(r.ranking).toEqual([{ regionId: 'S', remainder: f(55), rank: 1 }, { regionId: 'R', remainder: f(500), rank: 2 }]);
    expect(r.movements.slice(0, 2).map(m => m.regionId)).toEqual(['S', 'R']);
    expect(r.movements.slice(0, 2).every(m => m.removal.remainderBefore === 'used' && m.removal.remainderAfter === 'consumed')).toBe(true);
    conservation(r, regions);
  });
  it('excludes used-remainder regions in the first percentage pass and readmits them in later passes', () => {
    const regions = [region('R', 100, { A: 60500, B: 39500 }), region('S', 54, { A: 20000, B: 34000 })];
    const r = cap(regions);
    expect(r.movements.slice(0, 4).map(m => [m.regionId, m.removal.route, m.removal.pass])).toEqual([
      ['R', 'used-remainder', 0], ['S', 'regional-percentage', 1], ['S', 'regional-percentage', 2]
    ]);
    expect(r.movements.filter(m => m.removal.route === 'used-remainder')).toHaveLength(1);
    conservation(r, regions);
  });
  it('readmits a remainder-phase region during a repeated pass when further removals are needed', () => {
    const regions = [region('R', 100, { A: 80500, B: 19500 }), region('S', 54, { A: 20000, B: 34000 })];
    const r = cap(regions);
    expect(r.movements.slice(0, 4).map(m => [m.regionId, m.removal.route, m.removal.pass])).toEqual([
      ['R', 'used-remainder', 0], ['S', 'regional-percentage', 1], ['S', 'regional-percentage', 2], ['R', 'regional-percentage', 2]
    ]);
    conservation(r, regions);
  });
  it('continues with pass two when every eligible region lost a used remainder', () => {
    const regions = [region('R', 154, { A: 90500, B: 63500 })];
    const r = cap(regions);
    expect(r.movements[0].removal.route).toBe('used-remainder');
    expect(r.movements[1].removal.pass).toBe(2);
    conservation(r, regions);
  });
  it('protects a final integer seat in percentage passes, but can remove a sole used remainder seat', () => {
    const regions = [region('R', 100, { A: 80000, B: 20000 }), region('S', 1, { A: 60, B: 40 }), region('T', 1, { A: 100, B: 0 })];
    const r = cap(regions);
    expect(r.movements[0]).toMatchObject({ regionId: 'S', removal: { seatsBefore: 1, seatsAfter: 0, route: 'used-remainder' } });
    expect(r.movements.some(m => m.regionId === 'T')).toBe(false);
    expect(r.movements.filter(m => m.removal.route === 'regional-percentage').every(m => m.removal.seatsAfter >= 1)).toBe(true);
    conservation(r, regions);
  });
  it('consumes each recipient remainder once, then repeatedly falls back to regional votes', () => {
    const regions = [region('R', 154, { A: 90000, B: 30100, C: 23400, D: 10500 })];
    const r = cap(regions);
    expect(r.movements.slice(0, 3).map(m => [m.recipient.subject.id, m.recipient.route])).toEqual([
      ['C', 'unused-remainder'], ['B', 'unused-remainder'], ['B', 'regional-votes']
    ]);
    expect(r.movements.filter(m => m.recipient.route === 'unused-remainder')).toHaveLength(2);
    expect(r.remainders.map(s => [s.subject.id, s.initialState, s.state])).toEqual([['A', 'unused', 'unused'], ['B', 'unused', 'used'], ['C', 'unused', 'used'], ['D', 'used', 'used']]);
    conservation(r, regions);
  });
  it('never assigns a second seat on an already used minority remainder', () => {
    const regions = [region('R', 154, { A: 90100, B: 40600, C: 23300 })];
    const r = cap(regions);
    expect(r.movements[0].recipient.subject).toEqual(list('C'));
    expect(r.movements[1].recipient.route).toBe('regional-votes');
    expect(r.remainders.find(s => s.subject.id === 'B')).toMatchObject({ initialState: 'used', state: 'used' });
    conservation(r, regions);
  });
  it('uses warned I03 draws for equal removal remainders without adding a regional-vote criterion', () => {
    const regions = [region('R', 100, { A: 51500, B: 48500 }), region('S', 54, { A: 27500, B: 26500 })];
    const r = cap(regions, 0, first);
    expect(r.ranking.map(row => row.rank)).toEqual([1, 1]);
    const d = r.draws.find(d => d.contextId.includes('removal-1'))!;
    expect(d).toMatchObject({ norm: 'N20', rule: 'I03', alternativeIds: ['R', 'S'] });
    expect(r.warnings).toContainEqual(expect.objectContaining({ code: 'I03_DRAW', severity: 'warning', drawContextId: d.contextId, simulationId: 9, chamber: 'senato' }));
    expect(r.interpretations).toEqual([{ id: 'I06', version: 'I06-v1' }]);
    conservation(r, regions);
  });
  it('draws for tied regional percentages after using the exact full-vote denominator', () => {
    // Equal 80%, different vote totals. No used majority remainders.
    const regions = [region('R', 100, { A: 80000, B: 20000 }), region('S', 50, { A: 8000, B: 2000 })];
    const r = cap(regions, 0, first);
    expect(r.movements[0].removal.resolution.draws[0]).toMatchObject({ alternativeIds: ['R', 'S'], rule: 'I03' });
    conservation(r, regions);
  });
  it('draws for equal unused recipient remainders even if regional votes differ', () => {
    const regions = [region('R', 154, { A: 90000, B: 40000, C: 24000 })];
    const r = cap(regions, 0, first);
    expect(r.movements[0].recipient.resolution.draws[0]).toMatchObject({ alternativeIds: [JSON.stringify(['list', 'B']), JSON.stringify(['list', 'C'])], rule: 'I03' });
    conservation(r, regions);
  });
  it('warns when regional-vote fallback has a final tie', () => {
    const regions = [region('R', 154, { A: 90000, B: 32000, C: 32000 })];
    const r = cap(regions, 0, first);
    expect(r.movements[2].recipient).toMatchObject({ route: 'regional-votes', resolution: { draws: [{ rule: 'I03' }] } });
    expect(r.draws.length).toBe(r.warnings.length);
    conservation(r, regions);
  });
  it('returns an explicit no-progress state when all remaining seats are protected', () => {
    const regions = Array.from({ length: 80 }, (_, i) => region(`R${i}`, 1, { A: 1000, B: 0 }));
    const r = cap(regions);
    expect(r).toMatchObject({ status: 'non-computable', reason: 'no-lawful-removal-progress', movements: [] });
    expect(r).not.toHaveProperty('targets');
  });
  it('retains partial movements and remainder state if repeated passes eventually stop making progress', () => {
    const regions = Array.from({ length: 79 }, (_, i) => region(`R${i}`, 2, { A: 2000, B: 0 }));
    const r = cap(regions, 0, first);
    expect(r).toMatchObject({ status: 'non-computable', reason: 'no-lawful-removal-progress' });
    expect(r.movements).toHaveLength(79);
    expect(r.movements.every(m => m.removal.seatsAfter === 1)).toBe(true);
    expect(r.remainders.filter(s => s.subject.id === 'B').every(s => s.state === 'used')).toBe(true);
    expect(r).not.toHaveProperty('targets');
  });
  it('fails explicitly with no minority recipient and retains the original allocation', () => {
    expect(cap([region('R', 154, { A: 154000 })])).toMatchObject({ status: 'non-computable', reason: 'no-eligible-recipient', movements: [] });
  });
  it('preserves targets, traces and input ownership across replay and region/subject permutations', () => {
    const original = [region('R', 100, { A: 51500, B: 48500 }), region('S', 54, { A: 27500, B: 26500 })];
    const before = structuredClone(original);
    const r = cap(original, 0, createSeededDrawContext('replay', 'N20'));
    const reversed = original.toReversed().map(r => ({ ...r, subjects: r.subjects.toReversed(), allocation: { ...r.allocation, assignments: r.allocation.assignments.toReversed() } }));
    const other = cap(reversed, 0, createSeededDrawContext('replay', 'N20'));
    expect(other).toEqual(r);
    expect(original).toEqual(before);
    expect(JSON.parse(JSON.stringify(r))).toEqual(r);
  });
});

describe('Senate stage prerequisites and arithmetic failures', () => {
  it.each(['admission', 'bonus', 'special'] as const)('blocks incomplete %s before draws', stage => {
    const input = fixture();
    if (stage === 'admission') input.admission = { ...input.admission, status: 'non-computable', reason: 'zero-valid-votes' } as typeof input.admission;
    else if (stage === 'bonus') input.bonus = { ...input.bonus, status: 'non-computable', awards: [], blockers: [] };
    else input.specialSeats.status = 'partial';
    expect(run(input)).toMatchObject({ status: 'non-computable', stage: 'prerequisites', regions: [], draws: [] });
  });
  it('does not need special totals when no bonus is awarded', () => {
    const input = fixture({ R: { A: 100000, B: 89000 } }, false);
    input.specialSeats.status = 'partial'; input.specialSeats.totalSeats = null;
    expect(complete(input).path).toBe('no-bonus');
  });
  it.each([
    [0, 0, 'zero-vote-total'], [1, 1, 'zero-quota'], [155, 150, 'initial-overallocation']
  ])('retains explicit I04 failure for votes %i/%i', (A, B, reason) => {
    const input = fixture();
    for (const row of input.figures.regions[0].figures.lists) row.votes = row.listId === 'A' ? A : B;
    input.figures.regions[0].figures.validVotes = A + B;
    expect(run(input)).toMatchObject({ status: 'non-computable', stage: 'provisional', reason, affectedRegionId: 'R' });
  });
  it.each(['capacity', 'regional-bonus', 'special-ledger', 'simulation', 'figure', 'duplicate-figure', 'winner'] as const)('rejects invalid %s before any allocation draw', kind => {
    const input = fixture();
    if (kind === 'capacity') input.chamber.colleges[0].seatsWithBonus--;
    if (kind === 'regional-bonus') input.chamber.territories[0].bonusSeats--;
    if (kind === 'special-ledger') input.specialSeats.subjects[0].totalSeats = 8;
    if (kind === 'simulation') input.simulationId++;
    if (kind === 'figure') input.figures.regions[0].figures.lists.pop();
    if (kind === 'duplicate-figure') input.figures.regions.push(input.figures.regions[0]);
    if (kind === 'winner' && input.bonus.status === 'awarded') input.bonus.awards[1].subject = list('unknown');
    expect(() => run(input)).toThrow(RangeError);
  });
  it('preflights later regional figures before using an earlier statutory draw', () => {
    const input = fixture({ R: { A: 50000, B: 50000 }, S: { A: 50000, B: 50000 } }, true, 0, { R: 101, S: 53 });
    input.figures.regions[1].figures.lists.pop();
    expect(() => run(input)).toThrow(RangeError);
  });
  it('preserves an explicit cap failure and provisional totals without publishing final targets', () => {
    const input = fixture({ R: { A: 154000 } });
    const r = run(input);
    expect(r).toMatchObject({ status: 'non-computable', stage: 'cap', reason: 'no-eligible-recipient',
      provisionalTotals: [{ subject: list('A'), ordinarySeats: 154 }], cap: { movements: [] } });
    expect(r).not.toHaveProperty('targets');
  });
  it('returns no-eligible-subjects explicitly and unknown capacity stays null', () => {
    const input = fixture();
    if (input.admission.status !== 'complete') throw new Error();
    input.admission.admittedSubjects = [];
    input.bonus = { ...input.bonus, status: 'not-awarded', awards: [], reasons: [{ code: 'missing-link' }] };
    expect(run(input)).toMatchObject({ status: 'non-computable', stage: 'provisional', reason: 'no-eligible-subjects', provisionalTotals: null });
    input.bonus = { ...input.bonus, status: 'non-computable', awards: [], blockers: [] };
    expect(run(input)).toMatchObject({ ordinarySeats: null, regions: [] });
  });
  it('supports zero-seat regions without dividing or consuming a draw', () => {
    const input = fixture({ R: { A: 0, B: 0 }, S: { A: 78000, B: 76000 } }, true, 0, { R: 0, S: 154 });
    const r = complete(input);
    expect(r.regions[0].allocation).toMatchObject({ status: 'complete', quota: null, draws: [] });
    expect(r.targets.filter(t => t.regionId === 'R').map(t => t.ordinarySeats)).toEqual([0, 0]);
  });
  it('does not alias metadata, provisional regions, final targets or upstream events', () => {
    const input = fixture(), before = structuredClone(input);
    input.admission.warnings.push({ code: 'upstream', electionKind: 'politiche', simulationId: 9, severity: 'warning', message: 'upstream' });
    const r = complete(input);
    expect(r.warnings).toEqual([]);
    expect(r.seatTable).not.toBe(input.chamber.seatTable);
    expect(r.targets[0].subject).not.toBe(input.admission.status === 'complete' && input.admission.admittedSubjects[0]);
    expect(input.chamber).toEqual(before.chamber);
    expect(JSON.parse(JSON.stringify(r))).toEqual(r);
  });
  it('replays the entire regional stage independently of source-array order', () => {
    const input = fixture({ R: { A: 50000, B: 50000 }, S: { A: 50000, B: 50000 } }, true, 0, { R: 101, S: 53 });
    const r = complete(input, createSeededDrawContext('stage-replay', 'legal'));
    input.chamber.colleges.reverse(); input.chamber.territories.reverse(); input.chamber.lists.reverse();
    input.figures.regions.reverse();
    for (const row of input.figures.regions) row.figures.lists.reverse();
    if (input.admission.status !== 'complete') throw new Error();
    input.admission.admittedSubjects.reverse();
    expect(complete(input, createSeededDrawContext('stage-replay', 'legal'))).toEqual(r);
  });
});

describe('validated reform-envelope composition through N19-N22', () => {
  it.each([false, true])('continues the legal stages with Senate regional totals (joint=%s)', joint => {
    const full = joint ? jointFixture() : singleFixture('senato');
    const chambers = full.renewal.kind === 'joint' ? [full.renewal.camera, full.renewal.senato] : [full.renewal.chamber];
    for (const c of chambers) {
      c.lists.push({ ...c.lists[0], id: 'B', name: 'B' });
      const ids = new Set([...c.ordinarySlates.flatMap(s => s.candidateIds), ...c.specialCandidatures.map(s => s.candidateId)]);
      full.candidates.push(...full.candidates.filter(p => ids.has(p.id)).map(p => ({ ...p, id: `${p.id}-B` })));
      c.ordinarySlates.push(...c.ordinarySlates.map(s => ({ ...s, listId: 'B', candidateIds: s.candidateIds.map(id => `${id}-B`) })));
      c.bonusSlates.push(...c.bonusSlates.map(s => ({ ...s, subject: list('B'), candidates: s.candidates.map(p => ({ candidateId: `${p.candidateId}-B`, affiliationListId: 'B' })) })));
      c.specialCandidatures.push(...c.specialCandidatures.map(s => ({ ...s, candidateId: `${s.candidateId}-B`, linkedListIds: ['B'], affiliationListId: 'B' })));
      for (const r of [...c.ordinaryVotes, ...c.specialVotes]) {
        r.electorate.electors = 200000;
        if (r.votes.kind !== 'attributed') throw new Error();
        r.votes.listVotes[0].votes = 60000;
        r.votes.listVotes.push({ listId: 'B', votes: 40000 });
      }
      for (const r of c.specialVotes) {
        r.votes.candidateTotals[0].votes = 60000;
        r.votes.candidateTotals.push({ candidateId: `${r.votes.candidateTotals[0].candidateId}-B`, votes: 40000 });
      }
    }
    const checked = validateReformScrutinyInput(full);
    expect(checked.issues).toEqual([]);
    if (!checked.valid) throw new Error();
    const { input } = checked;
    const validated = input.renewal.kind === 'joint' ? [input.renewal.camera, input.renewal.senato] : [input.renewal.chamber];
    const stages = validated.map(chamber => {
      const args = { chamber, candidates: input.candidates, simulationId: input.simulationId };
      const winners = determineSpecialWinners(args, noDraw);
      const ordinary = chamber.ordinaryVotes.filter(r => chamber.colleges.find(c => c.id === r.collegeId)!.kind === 'ordinary').map(r => {
        if (r.votes.kind !== 'attributed') throw new Error();
        return { collegeId: r.collegeId, listVotes: r.votes.listVotes };
      });
      const special = chamber.specialVotes.map(r => {
        const attributed = attributeSpecialVotes({ ...args, collegeId: r.collegeId }, noDraw);
        if (attributed.status !== 'complete') throw new Error();
        return attributed;
      });
      const figures = aggregateElectoralFigures(chamber, ordinary, special);
      const admission = chamber.chamber === 'camera' ? determineCameraAdmission({ ...args, figures, specialWinners: winners }, noDraw) : determineSenateAdmission({ ...args, figures }, noDraw);
      return { ...args, figures, admission, winners };
    });
    const bonus = determineGovernabilityBonus({ simulationId: input.simulationId, renewedChambers: joint ? ['camera', 'senato'] : ['senato'],
      chambers: stages.map(s => ({ chamber: s.chamber.chamber, admission: s.admission, national: s.figures.national })), bonusLinks: input.bonusLinks }, noDraw);
    const senate = stages.find(s => s.chamber.chamber === 'senato')!;
    const specialSeats = summarizeSpecialSeats(proclaimSpecialUninominalWinners({ ...senate, preliminary: senate.winners }));
    const r = allocateSenateRegions({ ...senate, bonus, specialSeats }, noDraw);
    expect(r).toMatchObject({ status: 'complete', path: joint ? 'bonus-with-cap' : 'no-bonus', ordinarySeats: joint ? 154 : 189,
      provisionalTotals: [{ subject: list('A'), ordinarySeats: joint ? 92 : 113 }, { subject: list('B'), ordinarySeats: joint ? 62 : 76 }],
      subjectTotals: [{ subject: list('A'), ordinarySeats: joint ? 71 : 113 }, { subject: list('B'), ordinarySeats: joint ? 83 : 76 }] });
    if (r.status !== 'complete') throw new Error();
    expect(r.targets.reduce((n, t) => n + t.ordinarySeats, 0)).toBe(joint ? 154 : 189);
    if (r.cap) conservation(r.cap, r.regions, 7);
    const territorial = allocateSenateListsAndColleges({ ...senate, regional: r }, createSeededDrawContext('validated-N21-N22', 'legal'));
    expect(territorial.status).toBe('complete');
    if (territorial.colleges.status !== 'complete') throw new Error(JSON.stringify(territorial));
    expect(territorial.colleges.targets.reduce((n, t) => n + t.ordinarySeats, 0)).toBe(joint ? 154 : 189);
    expect(territorial.colleges.targets.filter(t => t.listId === 'A').reduce((n, t) => n + t.ordinarySeats, 0)).toBe(joint ? 71 : 113);
  });
});
