import { describe, expect, it } from 'vitest';
import { determineCameraAdmission, determineSenateAdmission } from './admission';
import { admissionFixture } from './admission.fixture';
import { determineGovernabilityBonus } from './bonus';
import { getBundledReformSeatTables } from './bundled-data';
import { allocateCameraNationalSeats, type CameraNationalInput } from './camera-national';
import { jointFixture, singleFixture } from './contracts.fixture';
import { aggregateElectoralFigures } from './figures';
import type { Subject } from './input';
import type { ScrutinyExecutionContext } from './result';
import { attributeSpecialVotes } from './special-attribution';
import { proclaimSpecialUninominalWinners, summarizeSpecialSeats } from './special-seats';
import { determineSpecialWinners } from './special-winners';
import { allocateCameraTaaSeats } from './taa-allocation';
import { createSeededDrawContext } from './ties';
import { validateReformScrutinyInput } from './validation';

const noDraw: ScrutinyExecutionContext = { draw() { throw new Error('Unexpected draw'); } };
const list = (id: string): Subject => ({ kind: 'list', id });
const coalition = (id: string): Subject => ({ kind: 'coalition', id });
type Mutable<T> = T extends object ? { -readonly [K in keyof T]: Mutable<T[K]> } : T;

/** Constructed N06/N07/N10/N12 communications, with the real R26-07 endowment.
 * Figures deliberately differ from special/national figures. This is a stage
 * fixture, not a complete candidature envelope or a statistical model.
 */
function fixture(votes = { A: 146000, B: 100000, C: 68000 } as Record<string, number>, bonus = true,
  special = 5, groups: Record<string, string[]> = {}): Mutable<CameraNationalInput> {
  const f = admissionFixture('camera', { R: votes }, groups);
  const admission = determineCameraAdmission({ ...f, specialWinners: determineSpecialWinners(f, noDraw) }, noDraw);
  const subject = groups.G ? coalition('G') : list('A');
  const input: CameraNationalInput = { ...f, chamber: { ...f.chamber, ...getBundledReformSeatTables().chambers[0] }, admission,
    bonus: { norm: 'N12', simulationId: 9, renewedChambers: ['camera', 'senato'], lawReference: 'constructed communication',
      firstSubjects: [], draws: [], warnings: [], ...(bonus ? { status: 'awarded', awards: [
        { chamber: 'camera', subject, seats: 70 }, { chamber: 'senato', subject: list('A'), seats: 35 }
      ] } : { status: 'not-awarded', awards: [], reasons: [{ code: 'missing-link' }] }) },
    specialSeats: { chamber: 'camera', status: 'complete', uninominalSeats: 5, taaProportionalSeats: 3, totalSeats: 8,
      subjects: [{ subject, uninominalSeats: Math.min(special, 5), taaProportionalSeats: Math.max(special - 5, 0), totalSeats: special },
        { subject: list('B'), uninominalSeats: 5 - Math.min(special, 5), taaProportionalSeats: 3 - Math.max(special - 5, 0), totalSeats: 8 - special }] }
  };
  return structuredClone(input) as Mutable<CameraNationalInput>;
}
function run(f = fixture(), execution = noDraw) { return allocateCameraNationalSeats(f, execution); }
function complete(f = fixture(), execution = noDraw) {
  const r = run(f, execution);
  if (r.status !== 'complete') throw new Error(`${r.stage}: ${r.reason}`);
  return r;
}
const seats = (r: ReturnType<typeof complete>) => r.subjectTargets.map(t => [t.subject.id, t.ordinarySeats]);

describe('N13/N14 Camera national allocation', () => {
  it('uses the full ordinary endowment without a bonus, with no cap even above 220', () => {
    const r = complete(fixture({ A: 300000, B: 50000, C: 34000 }, false));
    expect(r).toMatchObject({ path: 'no-bonus', ordinarySeats: 384, bonus: null, capCheck: null, capCorrection: null });
    expect(seats(r)).toEqual([['A', 300], ['B', 50], ['C', 34]]);
    expect(r.provisionalAllocation).toMatchObject({ rawQuota: { numerator: '1000', denominator: '1' }, quota: { numerator: '1000', denominator: '1' } });
  });

  it('reserves 70 seats and preserves provisional seats below the cap', () => {
    const r = complete(fixture({ A: 140000, B: 110000, C: 64000 }));
    expect(r).toMatchObject({ path: 'bonus-without-cap', ordinarySeats: 314, capCorrection: null,
      capCheck: { provisionalDomesticSeats: 215, exceedsCap: false }, bonus: { subject: list('A'), seats: 70 } });
    expect(seats(r)).toEqual([['A', 140], ['B', 110], ['C', 64]]);
  });

  it('leaves exactly 220 unchanged, including five special seats', () => {
    const r = complete(fixture({ A: 145000, B: 100000, C: 69000 }));
    expect(r).toMatchObject({ path: 'bonus-without-cap', capCheck: { provisionalOrdinarySeats: 145,
      bonusSeats: 70, specialSeats: 5, provisionalDomesticSeats: 220, exceedsCap: false } });
    expect(seats(r)).toEqual([['A', 145], ['B', 100], ['C', 69]]);
  });

  it('corrects 221 to 220 with independent majority/minority quotas and the bonus intact', () => {
    const r = complete();
    expect(r).toMatchObject({ path: 'bonus-with-cap', capCheck: { provisionalDomesticSeats: 221, exceedsCap: true },
      capCorrection: { majorityOrdinarySeats: 145, minorityOrdinarySeats: 169,
        majorityRawQuota: { numerator: '29200', denominator: '29' }, majorityQuota: { numerator: '1006', denominator: '1' },
        minorityAllocation: { rawQuota: { numerator: '168000', denominator: '169' }, quota: { numerator: '994', denominator: '1' },
          initialSeatTotal: '168', assignments: [
            { integerSeats: 100, remainder: { numerator: '600', denominator: '1' }, remainderState: 'used', remainderSeats: 1, seats: 101 },
            { integerSeats: 68, remainder: { numerator: '408', denominator: '1' }, remainderState: 'unused', seats: 68 }
          ] } }, bonus: { seats: 70 } });
    expect(seats(r)).toEqual([['A', 145], ['B', 101], ['C', 68]]);
    expect(r.provisionalAllocation?.assignments.map(a => a.seats)).toEqual([146, 100, 68]);
  });

  it.each([0, 1, 5, 8])('conserves 314 ordinary seats and 220 winner domestic seats with %i linked special seats', special => {
    const r = complete(fixture({ A: 200000, B: 70000, C: 44000 }, true, special));
    expect(r.path).toBe('bonus-with-cap');
    expect(r.subjectTargets[0].ordinarySeats).toBe(150 - special);
    expect(r.subjectTargets.reduce((n, t) => n + t.ordinarySeats, 0)).toBe(314);
    expect(r.listTargets.reduce((n, t) => n + t.ordinarySeats, 0)).toBe(314);
    expect(r.subjectTargets[0].ordinarySeats + 70 + special).toBe(220);
    expect(r.capCorrection?.minorityOrdinarySeats).toBe(314 - (150 - special));
  });

  it('uses special seats to trigger the cap without adding their votes to ordinary figures', () => {
    const f = fixture();
    const national = f.figures.national;
    national.lists.forEach(l => { l.votes = l.listId === 'C' ? 9000000 : 0; });
    national.coalitions = [];
    national.validVotes = 9000000;
    expect(seats(complete(f))).toEqual([['A', 145], ['B', 101], ['C', 68]]);
    expect(complete(fixture(undefined, true, 4)).path).toBe('bonus-without-cap');
  });

  it('uses remainder, then ordinary national figure before a statutory subject draw', () => {
    // 100500 + 200500 + 83000 = 384000, q=1000, initial=383; B wins on votes.
    const r = complete(fixture({ A: 100500, B: 200500, C: 83000 }, false));
    expect(seats(r)).toEqual([['A', 100], ['B', 201], ['C', 83]]);
    expect(r.provisionalAllocation?.resolutions[0].steps.map(s => s.criterion)).toEqual(['remainder', 'national-electoral-figure']);
    expect(r.draws).toEqual([]);
  });

  it('draws tied minority remainders once, with replay and permutation-invariant traces', () => {
    const f = fixture({ A: 200000, B: 57000, C: 57000 });
    const r = complete(f, createSeededDrawContext('cap-tie', 'legal'));
    expect(r.draws).toHaveLength(1);
    expect(r.draws[0]).toMatchObject({ norm: 'N14', chamber: 'camera', rule: 'statutory', lawReference: 'C83(1-ter)(b)' });
    expect(r.warnings).toEqual([]);
    expect(r.capCorrection?.minorityAllocation.assignments.map(a => a.seats).sort()).toEqual([84, 85]);
    if (f.admission.status !== 'complete') throw new Error();
    f.admission.admittedSubjects.reverse(); f.figures.ordinaryAllocation.lists.reverse(); f.chamber.lists.reverse();
    expect(complete(f, createSeededDrawContext('cap-tie', 'legal'))).toEqual(r);
  });

  it('uses statutory subject draws with the chosen seat recorded', () => {
    // 100500+100500+183000=384000, one residual seat tied A/B.
    const r = complete(fixture({ A: 100500, B: 100500, C: 183000 }, false), {
      draw(request) { return { ...request, chosenId: JSON.stringify(['list', 'B']), streamId: 'controlled' }; }
    });
    expect(seats(r)).toEqual([['A', 100], ['B', 101], ['C', 183]]);
    expect(r.draws[0]).toMatchObject({ norm: 'N13', rule: 'statutory' });
  });

  it.each([false, true])('uses all coalition votes at subject level and only admitted members internally (bonus=%s)', bonus => {
    const f = fixture({ A: 150000, D: 50000, E: 2000, F: 1000, B: 100000, C: 81000 }, bonus, 5, { G: ['A', 'D', 'E', 'F'] });
    const r = complete(f);
    const group = r.subjectTargets.find(t => t.subject.kind === 'coalition')!;
    const internal = r.coalitionAllocations[0].allocation;
    expect(group.votes).toBe(203000);
    // E is the single best excluded member; F's votes count for G but not internally.
    expect(internal.assignments.map(a => a.id)).toEqual(['A', 'D', 'E']);
    expect(r.listTargets.some(t => t.listId === 'F')).toBe(false);
    expect(r.listTargets.filter(t => t.subject.kind === 'coalition').reduce((n, t) => n + t.ordinarySeats, 0)).toBe(group.ordinarySeats);
    expect(internal.rawQuota).toEqual(bonus ? { numerator: '40400', denominator: '29' }
      : { numerator: '202000', denominator: '203' });
    expect(internal.quota).toEqual({ numerator: bonus ? '1393' : '995', denominator: '1' });
    expect(group.ordinarySeats).toBe(bonus ? 145 : 203);
    expect(r.listTargets.filter(t => t.subject.kind === 'coalition').map(t => [t.listId, t.ordinarySeats]))
      .toEqual(bonus ? [['A', 108], ['D', 36], ['E', 1]] : [['A', 151], ['D', 50], ['E', 2]]);
  });

  it('apportions coalition lists by integer quota, remainder, national votes, then statutory draw', () => {
    const f = fixture({ A: 100500, D: 100500, B: 100000, C: 83000 }, false, 5, { G: ['A', 'D'] });
    const r = complete(f, createSeededDrawContext('list-tie', 'legal'));
    // G=201 seats; internal q=1000, equal 100+1/2 with one remainder seat.
    expect(r.coalitionAllocations[0].allocation).toMatchObject({ quota: { numerator: '1000', denominator: '1' },
      assignments: [{ integerSeats: 100 }, { integerSeats: 100 }] });
    expect(r.draws).toHaveLength(1);
    expect(r.draws[0]).toMatchObject({ norm: 'N13', rule: 'statutory', lawReference: 'C83(1)(g)', alternativeIds: ['A', 'D'] });
    expect(r.bonus).toBeNull();
  });

  it('does not divide a zero-seat coalition, even with zero internal support', () => {
    const f = fixture({ A: 1000, D: 1000, B: 200000, C: 182000 }, false, 5, { G: ['A', 'D'] });
    if (f.admission.status !== 'complete') throw new Error();
    // A legally admitted subject can have zero ordinary votes when all support is special.
    f.admission.admittedSubjects.push(coalition('G'));
    f.admission.lists.filter(l => l.coalitionId === 'G').forEach(l => { l.admitted = true; l.admittedAs = 'coalition-member'; });
    f.figures.ordinaryAllocation.lists.filter(l => ['A', 'D'].includes(l.listId)).forEach(l => { l.votes = 0; });
    const r = complete(f);
    expect(r.coalitionAllocations[0].allocation).toMatchObject({ status: 'complete', quota: null, rawQuota: null,
      assignments: [{ seats: 0 }, { seats: 0 }] });
  });

  it('applies the corrected minority coalition target before dividing its internal lists', () => {
    const f = fixture({ A: 50000, D: 40000, B: 24000, C: 200000 }, true, 5, { G: ['A', 'D'] });
    if (f.bonus.status !== 'awarded') throw new Error();
    f.bonus.awards[0].subject = list('C');
    f.specialSeats.subjects[0].subject = list('C');
    const r = complete(f);
    // Minority q=floor(114000/169)=674. G=133 (remainder 358),
    // B=35 (remainder 410) +1, so G=133, B=36.
    // G's internal q=floor(90000/133)=676: A=73+1, D=59.
    expect(seats(r)).toEqual([['G', 133], ['B', 36], ['C', 145]]);
    expect(r.listTargets.filter(t => t.subject.kind === 'coalition').map(t => [t.listId, t.ordinarySeats]))
      .toEqual([['A', 74], ['D', 59]]);
    expect(r.coalitionAllocations[0].allocation.quota).toEqual({ numerator: '676', denominator: '1' });
  });

  it('keeps coalition/list identities distinct when their IDs coincide', () => {
    const f = fixture({ A: 120000, D: 100000, G: 164000 }, false, 5, { G: ['A', 'D'] });
    const r = complete(f);
    expect(r.subjectTargets.map(t => [t.subject, t.ordinarySeats])).toEqual([
      [coalition('G'), 220], [list('G'), 164]
    ]);
    expect(r.listTargets.map(t => [t.listId, t.ordinarySeats])).toEqual([['A', 120], ['D', 100], ['G', 164]]);
  });

  it('blocks positive coalition entitlement without admitted-list support and retains the failed trace', () => {
    const f = fixture({ A: 150000, D: 50000, E: 2000, F: 1000, B: 100000, C: 81000 }, false, 5, { G: ['A', 'D', 'E', 'F'] });
    f.figures.ordinaryAllocation.lists.forEach(l => {
      if (['A', 'D', 'E'].includes(l.listId)) l.votes = 0;
      if (l.listId === 'F') l.votes = 203000;
    });
    const r = run(f);
    expect(r).toMatchObject({ status: 'non-computable', stage: 'coalition-lists', reason: 'zero-vote-total',
      provisionalAllocation: { status: 'complete' }, coalitionAllocations: [{ coalitionId: 'G', allocation: { status: 'non-computable' } }] });
    expect('listTargets' in r).toBe(false);
  });

  it.each(['admission', 'bonus', 'special'] as const)('blocks incomplete %s before consuming any draws', stage => {
    const f = fixture({ A: 100500, B: 100500, C: 113000 });
    if (stage === 'admission') f.admission = { chamber: 'camera', norm: 'N07', status: 'non-computable', reason: 'zero-valid-votes', draws: [], warnings: [] };
    if (stage === 'bonus') f.bonus = { ...f.bonus, status: 'non-computable', awards: [], blockers: [{ chamber: 'senato', reason: 'missing-chamber-data' }] };
    if (stage === 'special') f.specialSeats.status = 'partial';
    expect(run(f)).toMatchObject({ status: 'non-computable', stage: 'prerequisites', draws: [], provisionalAllocation: null });
    if (stage === 'bonus') expect(run(f).ordinarySeats).toBeNull();
  });

  it('needs no special communication for the computed no-bonus path', () => {
    const f = fixture(undefined, false); f.specialSeats.status = 'partial'; f.specialSeats.totalSeats = null;
    expect(complete(f).path).toBe('no-bonus');
  });

  it.each([
    [{ A: 0, B: 0, C: 0 }, 'zero-vote-total'],
    [{ A: 1, B: 1, C: 1 }, 'zero-quota'],
    [{ A: 383, B: 383, C: 383 }, 'initial-overallocation']
  ] as const)('reports I04 provisional failure %j: %s without partial targets', (counts, reason) => {
    const f = fixture(undefined, false);
    f.figures.ordinaryAllocation.lists.forEach(l => { l.votes = counts[l.listId as keyof typeof counts]; });
    const r = run(f);
    expect(r).toMatchObject({ status: 'non-computable', stage: 'provisional', reason, draws: [] });
    expect('subjectTargets' in r).toBe(false);
  });

  it('blocks a cap minority with no eligible subjects instead of inventing seats', () => {
    const r = run(fixture({ A: 314000 }));
    expect(r).toMatchObject({ status: 'non-computable', stage: 'cap', reason: 'no-eligible-subjects',
      capCheck: { provisionalDomesticSeats: 389 }, capCorrection: { majorityOrdinarySeats: 145, minorityOrdinarySeats: 169 } });
  });

  it('reports a zero-vote cap minority', () => {
    const f = fixture();
    f.figures.ordinaryAllocation.lists.forEach(l => { l.votes = l.listId === 'A' ? 314000 : 0; });
    expect(run(f)).toMatchObject({ status: 'non-computable', stage: 'cap', reason: 'zero-vote-total' });
  });

  it.each(['capacity', 'special-count', 'bonus-count', 'special-total', 'simulation', 'winner', 'duplicate-figure'])(
    'rejects incompatible %s inputs', kind => {
      const f = fixture();
      if (kind === 'capacity') f.chamber.colleges[0].seatsWithBonus++;
      if (kind === 'special-count') f.chamber.colleges.find(c => c.kind === 'taa-proportional')!.seatsWithoutBonus++;
      if (kind === 'bonus-count') f.chamber.territories[0].bonusSeats++;
      if (kind === 'special-total') f.specialSeats.totalSeats = 9;
      if (kind === 'simulation') f.bonus.simulationId++;
      if (kind === 'winner' && f.bonus.status === 'awarded') f.bonus.awards[0].subject = list('unknown');
      if (kind === 'duplicate-figure') f.figures.ordinaryAllocation.lists.push(f.figures.ordinaryAllocation.lists[0]);
      expect(() => run(f)).toThrow(RangeError);
    });

  it('returns owned JSON-safe results and preserves provisional and final evidence', () => {
    const f = fixture(), before = structuredClone(f), r = complete(f);
    expect(f).toEqual(before);
    expect(JSON.parse(JSON.stringify(r))).toEqual(r);
    expect(r.seatTable).not.toBe(f.chamber.seatTable);
    expect(r.subjectTargets[0].subject).not.toBe(f.admission.status === 'complete' ? f.admission.admittedSubjects[0] : null);
    expect(r.bonus?.subject).not.toBe(f.bonus.status === 'awarded' ? f.bonus.awards[0].subject : null);
  });

  it.each([false, true])('composes a validated envelope through N06/N07/N09/N10/N12 (joint=%s)', joint => {
    const full = joint ? jointFixture() : singleFixture('camera');
    const chambers = full.renewal.kind === 'joint' ? [full.renewal.camera, full.renewal.senato] : [full.renewal.chamber];
    for (const c of chambers) {
      if (joint) {
        c.lists.push({ ...c.lists[0], id: 'B', name: 'B' });
        const ids = new Set([...c.ordinarySlates.flatMap(s => s.candidateIds), ...c.specialCandidatures.map(s => s.candidateId)]);
        full.candidates.push(...full.candidates.filter(p => ids.has(p.id)).map(p => ({ ...p, id: `${p.id}-B` })));
        c.ordinarySlates.push(...c.ordinarySlates.map(s => ({ ...s, listId: 'B', candidateIds: s.candidateIds.map(id => `${id}-B`) })));
        c.bonusSlates.push(...c.bonusSlates.map(s => ({ ...s, subject: list('B'), candidates: s.candidates.map(p => ({ candidateId: `${p.candidateId}-B`, affiliationListId: 'B' })) })));
        c.specialCandidatures.push(...c.specialCandidatures.map(s => ({ ...s, candidateId: `${s.candidateId}-B`, linkedListIds: ['B'], affiliationListId: 'B' })));
      }
      for (const r of [...c.ordinaryVotes, ...c.specialVotes]) {
        r.electorate.electors = 200000;
        if (r.votes.kind !== 'attributed') throw new Error();
        r.votes.listVotes[0].votes = joint ? 60000 : 100000;
        if (joint) r.votes.listVotes.push({ listId: 'B', votes: 40000 });
      }
      for (const r of c.specialVotes) {
        r.votes.candidateTotals[0].votes = joint ? 60000 : 100000;
        if (joint) r.votes.candidateTotals.push({ candidateId: `${r.votes.candidateTotals[0].candidateId}-B`, votes: 40000 });
      }
    }
    const checked = validateReformScrutinyInput(full);
    expect(checked.issues).toEqual([]);
    if (!checked.valid) throw new Error();
    const { input } = checked, c = input.renewal.kind === 'joint' ? input.renewal.camera : input.renewal.chamber;
    const args = { chamber: c, candidates: input.candidates, simulationId: input.simulationId };
    const winners = determineSpecialWinners(args, noDraw);
    const special = c.specialVotes.map(r => {
      const result = attributeSpecialVotes({ ...args, collegeId: r.collegeId }, noDraw);
      if (result.status !== 'complete') throw new Error();
      return result;
    });
    const ordinary = c.ordinaryVotes.filter(r => c.colleges.find(p => p.id === r.collegeId)!.kind === 'ordinary').map(r => {
      if (r.votes.kind !== 'attributed') throw new Error();
      return { collegeId: r.collegeId, listVotes: r.votes.listVotes };
    });
    const figures = aggregateElectoralFigures(c, ordinary, special);
    const admission = determineCameraAdmission({ ...args, figures, specialWinners: winners }, noDraw);
    const taa = allocateCameraTaaSeats({ ...args, figures, admission, collegeId: 'TP' }, noDraw);
    expect(taa.status).toBe('partial'); // missing preferences affect identities, not entitlements
    const specialSeats = summarizeSpecialSeats(proclaimSpecialUninominalWinners({ ...args, preliminary: winners }), taa);
    const paired = [];
    if (input.renewal.kind === 'joint') {
      const senate = input.renewal.senato;
      const senateSpecial = senate.specialVotes.map(r => {
        const result = attributeSpecialVotes({ chamber: senate, collegeId: r.collegeId, simulationId: input.simulationId }, noDraw);
        if (result.status !== 'complete') throw new Error();
        return result;
      });
      const senateFigures = aggregateElectoralFigures(senate, senate.ordinaryVotes.map(r => {
        if (r.votes.kind !== 'attributed') throw new Error();
        return { collegeId: r.collegeId, listVotes: r.votes.listVotes };
      }), senateSpecial);
      paired.push({ chamber: 'senato' as const, admission: determineSenateAdmission({ chamber: senate, figures: senateFigures, simulationId: input.simulationId }, noDraw), national: senateFigures.national });
    }
    const bonus = determineGovernabilityBonus({ simulationId: input.simulationId, renewedChambers: joint ? ['camera', 'senato'] : ['camera'],
      chambers: [{ chamber: 'camera', admission, national: figures.national }, ...paired], bonusLinks: input.bonusLinks }, noDraw);
    expect(allocateCameraNationalSeats({ ...args, figures, admission, bonus, specialSeats }, noDraw)).toMatchObject({
      status: 'complete', path: joint ? 'bonus-with-cap' : 'no-bonus', ordinarySeats: joint ? 314 : 384,
      subjectTargets: joint ? [{ ordinarySeats: 143 }, { ordinarySeats: 171 }] : [{ ordinarySeats: 384 }] });
  });
});
