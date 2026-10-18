import { describe, expect, it } from 'vitest';
import { determineCameraAdmission, determineSenateAdmission } from './admission';
import { admissionFixture } from './admission.fixture';
import { determineGovernabilityBonus, type BonusChamberInput, type GovernabilityBonusInput } from './bonus';
import { jointFixture } from './contracts.fixture';
import { aggregateElectoralFigures } from './figures';
import type { Chamber, Subject } from './input';
import type { ScrutinyExecutionContext } from './result';
import { attributeSpecialVotes } from './special-attribution';
import { determineSpecialWinners } from './special-winners';
import { createSeededDrawContext } from './ties';
import { validateReformScrutinyInput } from './validation';

const noDraw: ScrutinyExecutionContext = { draw() { throw new Error('Unexpected draw'); } };
const list = (id: string): Subject => ({ kind: 'list', id });
const coalition = (id: string): Subject => ({ kind: 'coalition', id });
const link = (camera = list('A'), senato = list('A')) => ({ camera, senato, cameraDeclares: true, senatoDeclares: true });
function chamber(side: Chamber, votes: Record<string, number> = { A: 420, B: 350, C: 230 }, groups: Record<string, string[]> = {}): BonusChamberInput {
  const f = admissionFixture(side, { R: votes }, groups);
  const admission = side === 'camera'
    ? determineCameraAdmission({ ...f, specialWinners: determineSpecialWinners(f, noDraw) }, noDraw)
    : determineSenateAdmission(f, noDraw);
  return { chamber: side, admission, national: f.figures.national };
}
function fixture(camera = chamber('camera'), senato = chamber('senato')): GovernabilityBonusInput {
  return { simulationId: 9, renewedChambers: ['camera', 'senato'], chambers: [camera, senato], bonusLinks: [link()] };
}
const run = (input = fixture(), execution = noDraw) => determineGovernabilityBonus(input, execution);

describe('N12 paired governability bonus', () => {
  it.each([[420, 420], [421, 420], [700, 650]])('awards exactly 70/35 at %i/%i per thousand', (c, s) => {
    const counts = (a: number) => ({ A: a, B: Math.ceil((1000 - a) / 2), C: Math.floor((1000 - a) / 2) });
    const result = run(fixture(chamber('camera', counts(c)), chamber('senato', counts(s))));
    expect(result).toMatchObject({ status: 'awarded', awards: [{ seats: 70 }, { seats: 35 }] });
  });

  it('includes exactly 42% in both chambers and returns exact evidence', () => {
    const result = run();
    expect(result).toMatchObject({ status: 'awarded', norm: 'N12', awards: [
      { chamber: 'camera', subject: list('A'), seats: 70 }, { chamber: 'senato', subject: list('A'), seats: 35 }
    ], draws: [], warnings: [] });
    expect(result.firstSubjects.map(f => [f.votes, f.validVotes, f.percentage, f.reaches42Percent])).toEqual(
      Array.from({ length: 2 }, () => [420, 1000, { status: 'defined', value: { numerator: '42', denominator: '1' } }, true]));
  });

  it.each([[419, 420, ['camera']], [420, 419, ['senato']], [419, 419, ['camera', 'senato']]] as const)(
    'withholds both awards below threshold (%i/%i)', (c, s, failed) => {
      const result = run(fixture(chamber('camera', { A: c, B: 350, C: 650 - c }), chamber('senato', { A: s, B: 350, C: 650 - s })));
      expect(result).toMatchObject({ status: 'not-awarded', awards: [],
        reasons: failed.map(chamber => ({ code: 'below-42-percent', chamber })) });
    });

  it('ranks the admitted first subject, never a lower linked subject above 42%', () => {
    const result = run(fixture(chamber('camera', { A: 450, B: 500, C: 50 })));
    expect(result).toMatchObject({ status: 'not-awarded', awards: [], reasons: [{ code: 'unlinked-first-subjects' }] });
    expect(result.firstSubjects.map(f => f.subject)).toEqual([list('B'), list('A')]);
  });

  it('supports different declared identities and coalition/list links without splitting coalition awards', () => {
    const f = fixture();
    const camera = chamber('camera', { A: 300, B: 200, C: 300, D: 200 }, { G: ['A', 'B'] });
    const result = run({ ...f, chambers: [camera, f.chambers[1]], bonusLinks: [link(coalition('G'), list('A'))] });
    expect(result).toMatchObject({ status: 'awarded', awards: [
      { subject: coalition('G'), seats: 70 }, { subject: list('A'), seats: 35 }
    ] });
    if (result.status !== 'awarded') throw new Error();
    expect(result.firstSubjects[0].votes).toBe(500);
    expect('listId' in result.awards[0]).toBe(false);
  });

  it('does not infer links from equal IDs or match a list to a coalition with the same ID', () => {
    expect(run({ ...fixture(), bonusLinks: [] })).toMatchObject({ status: 'not-awarded', reasons: [{ code: 'missing-link' }] });
    expect(run({ ...fixture(), bonusLinks: [link(coalition('A'), list('A'))] })).toMatchObject({
      status: 'not-awarded', reasons: [{ code: 'unlinked-first-subjects' }] });
  });

  it('keeps list and coalition IDs distinct while ranking only admitted subjects', () => {
    const c = chamber('camera', { A: 450, B: 50, C: 300, D: 200 }, { A: ['C', 'D'] });
    expect(run({ ...fixture(c), bonusLinks: [link(coalition('A'), list('A'))] })).toMatchObject({
      status: 'awarded', firstSubjects: [{ subject: coalition('A'), votes: 500 }, {}] });
    if (c.admission.status !== 'complete') throw new Error();
    // Construct an explicit admission communication: the larger coalition may
    // not participate once excluded. Do not recompute admission in this stage.
    const excluded = { ...c, admission: { ...c.admission, admittedSubjects: [list('A'), list('B')] } };
    expect(run(fixture(excluded))).toMatchObject({ status: 'awarded', firstSubjects: [{ subject: list('A'), votes: 450 }, {}] });
  });

  it('uses special ballots once for first place and the all-valid threshold denominator', () => {
    // Ordinary: A=400, B=350, C=150 (A=44.44%); special: C=100.
    // All domestic: A=400/1000=40%, so the bonus must be absent.
    const f = admissionFixture('camera', { R: { A: 400, B: 350, C: 150 }, T: { C: 100 } }, {}, {}, 1);
    const admission = determineCameraAdmission({ ...f, specialWinners: determineSpecialWinners(f, noDraw) }, noDraw);
    const national = f.figures.national;
    expect(run(fixture({ chamber: 'camera', admission, national }))).toMatchObject({
      status: 'not-awarded', firstSubjects: [{ votes: 400, validVotes: 1000 }, {}],
      reasons: [{ code: 'below-42-percent', chamber: 'camera' }] });
    // A special-territory-only list can instead become the national first.
    const g = admissionFixture('camera', { R: { A: 400, B: 350, C: 150 }, T: { X: 1000 } }, {}, {}, 1);
    const admitted = determineCameraAdmission({ ...g, specialWinners: determineSpecialWinners(g, noDraw) }, noDraw);
    const result = run(fixture({ chamber: 'camera', admission: admitted, national: g.figures.national }));
    expect(result).toMatchObject({ status: 'not-awarded', firstSubjects: [{ subject: list('X'), votes: 1000, validVotes: 1900 }, {}] });
  });

  it.each([[false, true], [true, false], [false, false]])('requires both declarations (%s/%s)', (cameraDeclares, senatoDeclares) => {
    expect(run({ ...fixture(), bonusLinks: [{ ...link(), cameraDeclares, senatoDeclares }] })).toMatchObject({
      status: 'not-awarded', awards: [], reasons: [{ code: 'non-reciprocal-link' }] });
  });

  it('reports threshold and linkage failures together', () => {
    expect(run({ ...fixture(chamber('camera', { A: 419, B: 350, C: 231 })), bonusLinks: [] })).toMatchObject({
      status: 'not-awarded', reasons: [{ code: 'below-42-percent', chamber: 'camera' }, { code: 'missing-link' }] });
  });

  it.each(['camera', 'senato'] as const)('single %s renewal takes the no-bonus path without draws', side => {
    expect(run({ ...fixture(), renewedChambers: [side], chambers: [chamber(side, { A: 500, B: 500, C: 0 })], bonusLinks: [] })).toMatchObject({
      status: 'not-awarded', awards: [], firstSubjects: [], draws: [], reasons: [{ code: 'single-chamber-renewal', chamber: side }] });
  });

  it.each(['camera', 'senato'] as const)('missing %s in a joint renewal is incomplete, never single renewal', side => {
    expect(run({ ...fixture(), chambers: fixture().chambers.filter(c => c.chamber !== side) })).toMatchObject({
      status: 'non-computable', awards: [], firstSubjects: [], blockers: [{ chamber: side, reason: 'missing-chamber-data' }] });
  });

  it('preflights a blocked second chamber before drawing a first-place tie', () => {
    const f = fixture(chamber('camera', { A: 500, B: 500, C: 0 }), chamber('senato', { A: 0, B: 0, C: 0 }));
    expect(run(f)).toMatchObject({ status: 'non-computable', draws: [], warnings: [],
      blockers: [{ chamber: 'senato', reason: 'admission-non-computable' }] });
  });

  it.each(['zero-valid-votes', 'no-admitted-subjects', 'missing-subject-figure'] as const)('keeps %s explicitly non-computable', reason => {
    const f = fixture();
    const c = f.chambers[0];
    if (c.admission.status !== 'complete') throw new Error();
    const changed: BonusChamberInput = reason === 'no-admitted-subjects'
      ? { ...c, admission: { ...c.admission, admittedSubjects: [] } }
      : { ...c, national: { ...c.national, ...(reason === 'zero-valid-votes' ? { validVotes: 0 } : { lists: [] }) } };
    expect(run({ ...f, chambers: [changed, f.chambers[1]] })).toMatchObject({
      status: 'non-computable', blockers: [{ chamber: 'camera', reason }], draws: [] });
  });

  it('uses large exact counts and ignores forged display percentages', () => {
    const total = 9_000_000_000_000_001, a = 3_780_000_000_000_000;
    const c = chamber('camera', { A: a, B: 3_000_000_000_000_000, C: total - a - 3_000_000_000_000_000 });
    const forged = { ...c, national: { ...c.national, lists: c.national.lists.map(l => ({ ...l,
      percentage: { status: 'defined' as const, value: { numerator: '42', denominator: '1' } } })) } };
    expect(run(fixture(forged))).toMatchObject({ status: 'not-awarded', reasons: [{ code: 'below-42-percent', chamber: 'camera' }] });
  });

  it('warns for each tied first place, replays and ignores input ordering', () => {
    const f = fixture(chamber('camera', { A: 500, B: 500, C: 0 }), chamber('senato', { A: 500, B: 500, C: 0 }));
    const a = run(f, createSeededDrawContext('bonus-tie', 'legal'));
    const shuffled = { ...f, renewedChambers: [...f.renewedChambers].reverse(), chambers: [...f.chambers].reverse().map(c => {
      if (c.admission.status !== 'complete') throw new Error();
      return { ...c, admission: { ...c.admission, admittedSubjects: [...c.admission.admittedSubjects].reverse() },
        national: { ...c.national, lists: [...c.national.lists].reverse() } };
    }) };
    expect(run(shuffled, createSeededDrawContext('bonus-tie', 'legal'))).toEqual(a);
    expect(a.draws).toHaveLength(2);
    expect(a.warnings.map(w => [w.code, w.severity, w.interpretationId, w.simulationId, w.chamber])).toEqual([
      ['I03_DRAW', 'warning', 'I03', 9, 'camera'], ['I03_DRAW', 'warning', 'I03', 9, 'senato']
    ]);
    expect(a.draws.every(d => d.norm === 'N12' && d.rule === 'I03' && d.alternativeIds.length === 2)).toBe(true);
  });

  it.each([true, false])('records the tied choice and its paired bonus effect (linked choice %s)', chooseLinked => {
    const execution: ScrutinyExecutionContext = { draw(request) { return { ...request,
      chosenId: JSON.stringify(['list', chooseLinked ? 'A' : 'B']), streamId: 'constructed-choice' }; } };
    const result = run(fixture(chamber('camera', { A: 500, B: 500, C: 0 })), execution);
    expect(result.status).toBe(chooseLinked ? 'awarded' : 'not-awarded');
    expect(result.firstSubjects[0].subject).toEqual(list(chooseLinked ? 'A' : 'B'));
    expect(result.warnings).toHaveLength(1);
  });

  it('returns owned JSON-safe evidence without changing its inputs', () => {
    const f = fixture(), before = structuredClone(f), result = run(f);
    expect(f).toEqual(before);
    expect(JSON.parse(JSON.stringify(result))).toEqual(result);
    expect(result.firstSubjects[0].subject).not.toBe(f.bonusLinks[0].camera);
    if (result.status !== 'awarded') throw new Error();
    expect(result.awards[0].subject).not.toBe(result.firstSubjects[0].subject);
  });

  it('composes validated joint inputs, N06 special totals and N07/N08 admission', () => {
    const checked = validateReformScrutinyInput(jointFixture());
    expect(checked.issues).toEqual([]);
    if (!checked.valid || checked.input.renewal.kind !== 'joint') throw new Error();
    const input = checked.input, renewal = checked.input.renewal;
    const chambers = (['camera', 'senato'] as const).map(side => {
      const c = renewal[side], args = { chamber: c, candidates: input.candidates, simulationId: input.simulationId };
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
      const admission = side === 'camera'
        ? determineCameraAdmission({ ...args, figures, specialWinners: determineSpecialWinners(args, noDraw) }, noDraw)
        : determineSenateAdmission({ ...args, figures }, noDraw);
      return { chamber: side, admission, national: figures.national };
    });
    const result = run({ simulationId: input.simulationId, renewedChambers: ['camera', 'senato'], chambers, bonusLinks: input.bonusLinks });
    expect(result).toMatchObject({ status: 'awarded', firstSubjects: [{ validVotes: 1500 }, { validVotes: 1200 }] });
  });
});
