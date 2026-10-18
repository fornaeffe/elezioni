import { describe, expect, it } from 'vitest';
import { determineCameraAdmission, determineSenateAdmission, type AdmissionResult } from './admission';
import { admissionFixture } from './admission.fixture';
import { determineSpecialWinners } from './special-winners';
import { createSeededDrawContext } from './ties';
import type { Chamber } from './input';
import type { ScrutinyExecutionContext } from './result';
import { singleFixture } from './contracts.fixture';
import { validateReformScrutinyInput } from './validation';
import { aggregateElectoralFigures } from './figures';

const noDraw: ScrutinyExecutionContext = { draw() { throw new Error('Unexpected draw'); } };
function complete(result: AdmissionResult) {
  if (result.status !== 'complete') throw new Error(result.reason);
  return result;
}
function run(f: ReturnType<typeof admissionFixture>, execution = noDraw) {
  return complete(f.chamber.chamber === 'camera'
    ? determineCameraAdmission({ ...f, specialWinners: determineSpecialWinners(f, execution) }, execution)
    : determineSenateAdmission(f, execution));
}
function list(result: ReturnType<typeof run>, id: string) { return result.lists.find(l => l.listId === id)!; }

describe('N07/N08 national admission', () => {
  it.each(['camera', 'senato'] as const)('%s: exact 3%% boundary with all-valid denominator', side => {
    for (const [votes, admitted] of [[299, false], [300, true], [301, true]] as const) {
      const result = run(admissionFixture(side, { R: { A: votes, Z: 10000 - votes } }));
      expect(list(result, 'A').admitted).toBe(admitted);
      expect(list(result, 'A').admittedAs).toBe(admitted ? 'standalone' : null);
      expect(list(result, 'A').reasons).toEqual([admitted ? 'national-3-percent' : 'below-access-thresholds']);
    }
  });

  it.each(['camera', 'senato'] as const)('%s: exact 10%% coalition boundary and failed-coalition independent access', side => {
    for (const votes of [999, 1000, 1001]) {
      const result = run(admissionFixture(side, { R: { A: 700, B: votes - 700, Z: 10000 - votes } }, { C: ['A', 'B'] }));
      expect(result.coalitions[0].admitted).toBe(votes >= 1000);
      expect(list(result, 'A').admittedAs).toBe(votes >= 1000 ? 'coalition-member' : 'standalone');
      expect(result.admittedSubjects).toContainEqual(votes >= 1000 ? { kind: 'coalition', id: 'C' } : { kind: 'list', id: 'A' });
      expect(result.admittedSubjects).not.toContainEqual(votes >= 1000 ? { kind: 'list', id: 'A' } : { kind: 'coalition', id: 'C' });
      expect(list(result, 'B').admitted).toBe(votes >= 1000); // best excluded at 300? independently qualifies at/above
    }
  });

  it.each(['camera', 'senato'] as const)('%s: 10%% alone cannot admit a coalition with no qualifying member', side => {
    const result = run(admissionFixture(side, { R: { A: 250, B: 250, D: 250, E: 250, Z: 9000 } }, { C: ['A', 'B', 'D', 'E'] }));
    expect(result.coalitions[0]).toMatchObject({ admitted: false, reasons: ['no-qualifying-list'], admittedListIds: [], bestExcludedListId: null });
    expect(result.lists.filter(l => l.coalitionId === 'C').every(l => !l.admitted)).toBe(true);
  });

  it.each(['camera', 'senato'] as const)('%s: 2-bis includes all qualifying members, 2-ter only the best excluded, even below 1%%', side => {
    const result = run(admissionFixture(side, { R: { A: 800, B: 300, D: 70, E: 40, Z: 8790 } }, { C: ['A', 'B', 'D', 'E'] }));
    expect(result.coalitions[0]).toMatchObject({ admitted: true, qualifyingListIds: ['A', 'B'], admittedListIds: ['A', 'B', 'D'], bestExcludedListId: 'D' });
    expect(list(result, 'D')).toMatchObject({ admittedAs: 'coalition-member', reasons: ['best-excluded-list'] });
    expect(list(result, 'E')).toMatchObject({ admitted: false, reasons: ['not-best-excluded-list'] });
    expect(result.draws).toEqual([]);
    const allPass = run(admissionFixture(side, { R: { A: 700, B: 300, Z: 9000 } }, { C: ['A', 'B'] }));
    expect(allPass.coalitions[0].bestExcludedListId).toBeNull();
  });

  it.each(['camera', 'senato'] as const)('%s: tied best excluded draws once per coalition, warns and is order independent', side => {
    const f = admissionFixture(side, { R: { A: 1100, B: 20, D: 20, E: 0, F: 1200, G: 0, H: 0, Z: 7660 } },
      { C: ['A', 'B', 'D', 'E'], Q: ['F', 'G', 'H'] });
    const a = run(f, createSeededDrawContext('admission', 'legal'));
    const shuffled = { ...f, chamber: { ...f.chamber, lists: [...f.chamber.lists].reverse(),
      coalitions: [...f.chamber.coalitions].reverse().map(c => ({ ...c, listIds: [...c.listIds].reverse() })) } };
    expect(run(shuffled, createSeededDrawContext('admission', 'legal'))).toEqual(a);
    expect(a.draws.map(d => d.alternativeIds)).toEqual([['B', 'D'], ['G', 'H']]);
    expect(a.warnings).toHaveLength(2);
    expect(a.warnings.every(w => w.severity === 'warning' && w.interpretationId === 'I03' && w.simulationId === 9 && w.chamber === side)).toBe(true);
    for (const draw of a.draws) expect(list(a, draw.chosenId).reasons).toEqual(['best-excluded-list']);
  });

  it.each(['camera', 'senato'] as const)('%s: all-zero national votes are non-computable, with no exclusion or draw', side => {
    const f = admissionFixture(side, { R: { A: 0, B: 0 } }, { C: ['A', 'B'] });
    const result = side === 'camera' ? determineCameraAdmission({ ...f, specialWinners: determineSpecialWinners(f, noDraw) }, noDraw) : determineSenateAdmission(f, noDraw);
    expect(result).toMatchObject({ status: 'non-computable', reason: 'zero-valid-votes', draws: [], warnings: [] });
    expect('lists' in result).toBe(false);
  });

  it('compares large counts exactly and ignores display percentages', () => {
    const total = 9_000_000_000_000_001;
    const votes = 270_000_000_000_000; // below 3% by 0.03 votes
    const f = admissionFixture('camera', { R: { A: votes, Z: total - votes } });
    const forgedDisplay = { ...f, figures: { ...f.figures, national: { ...f.figures.national,
      lists: f.figures.national.lists.map(l => ({ ...l, percentage: { status: 'defined' as const, value: { numerator: '3', denominator: '1' } } })) } } };
    expect(list(run(forgedDisplay), 'A').admitted).toBe(false);
  });

  it('owns results without changing the inputs or sharing decisions with them', () => {
    const f = admissionFixture('camera', { R: { A: 1000, B: 30, Z: 8970 } }, { C: ['A', 'B'] });
    const before = structuredClone(f);
    const result = run(f);
    expect(f).toEqual(before);
    expect(result.coalitions[0].qualifyingListIds).not.toBe(f.chamber.coalitions[0].listIds);
    expect(JSON.parse(JSON.stringify(result))).toEqual(result);
  });
});

describe('N07 protected minority access and preliminary special wins', () => {
  it.each([199, 200, 201])('Camera minority at %i/1000 regional votes: standalone, coalition qualifier and 2-bis member', votes => {
    for (const linked of [false, true]) {
      const f = admissionFixture('camera', { R: { A: 1000, Z: 8000 }, T: { M: votes, Y: 1000 - votes } },
        linked ? { C: ['A', 'M'] } : {}, { M: 'T' });
      const result = run(f);
      expect(list(result, 'M').reasons).toEqual([votes >= 200 ? 'minority-regional-20-percent' : linked ? 'best-excluded-list' : 'below-access-thresholds']);
      if (votes >= 200) expect(list(result, 'M').admittedAs).toBe(linked ? 'coalition-member' : 'standalone');
    }
    // Coalition with no national-3% member is itself qualified only by M's 20%.
    const f = admissionFixture('camera', { R: { A: 220, B: 220, D: 220, E: 220, Z: 8120 }, T: { M: votes, Y: 1000 - votes } },
      { C: ['A', 'B', 'D', 'E', 'M'] }, { M: 'T' });
    expect(run(f, createSeededDrawContext(7, 'legal')).coalitions[0].admitted).toBe(votes >= 200);
  });

  it('rejects an unprotected, nonautonomous or nonexclusive Camera 20% route', () => {
    const plain = admissionFixture('camera', { R: { Z: 9000 }, T: { M: 200, Y: 800 } });
    expect(list(run(plain), 'M').admitted).toBe(false);
    const nonexclusive = admissionFixture('camera', { R: { M: 0, Z: 9000 }, T: { M: 200, Y: 800 } }, {}, { M: 'T' });
    expect(list(run(nonexclusive), 'M').minorityQualification).toBeNull();
    const protectedF = admissionFixture('camera', { R: { Z: 9000 }, T: { M: 200, Y: 800 } }, {}, { M: 'T' });
    expect(list(run({ ...protectedF, chamber: { ...protectedF.chamber, regions: protectedF.chamber.regions.map(r => ({ ...r, autonomous: false })) } }), 'M').admitted).toBe(false);
  });

  it('keeps I01 local access distinct from nationwide admission for a minority in a failed coalition', () => {
    const f = admissionFixture('camera', { R: { A: 400, Z: 8600 }, T: { M: 200, Y: 800 } }, { C: ['A', 'M'] }, { M: 'T' }, 4);
    const result = run(f);
    expect(result.coalitions[0].admitted).toBe(false);
    expect(list(result, 'A').admittedAs).toBe('standalone');
    expect(list(result, 'M')).toMatchObject({ admitted: false, reasons: ['failed-coalition-no-independent-access'] });
    expect(result.taaLocalExceptions).toEqual([{ territoryId: 'T', listId: 'M', reason: 'minority-taa-local-20-percent', interpretationId: 'I01', lawReference: 'C93-septies(2)' }]);
    expect(result.admittedSubjects).not.toContainEqual({ kind: 'list', id: 'M' });
  });

  it('requires ceil(5/4)=2 wins, counts linked winners, and reserves the quarter route to standalone minorities', () => {
    for (const wins of [1, 2]) {
      for (const linked of [false, true]) {
        const distribution = Array.from({ length: 5 }, (_, i) => ({ M: i < wins ? 100 / wins : 0, Y: i < wins ? 0 : 900 / (5 - wins) }));
        const f = admissionFixture('camera', { R: { A: 1000, Z: 8000 }, T: { M: 100, Y: 900 } },
          linked ? { C: ['A', 'M'] } : {}, { M: 'T' }, 5, distribution);
        const result = run(f);
        expect(list(result, 'M').minorityQualification!.uninominalTerritories).toEqual([{ territoryId: 'T', colleges: 5, requiredWins: 2, wins }]);
        expect(list(result, 'M').reasons).toEqual(linked ? ['best-excluded-list'] : [wins === 2 ? 'minority-quarter-uninominal-wins' : 'below-access-thresholds']);
      }
    }
  });

  it('does not let quarter wins qualify a coalition, and does not rerun the preliminary draw', () => {
    // Preserve the 100/900 attributed totals while tying one college, so its
    // winner requires I03 before admission (which must not draw again).
    const tied = admissionFixture('camera', { R: { A: 240, B: 240, D: 240, E: 240, Z: 8040 }, T: { M: 100, Y: 900 } },
      { C: ['A', 'B', 'D', 'E', 'M'] }, { M: 'T' }, 4,
      [{ M: 100, Y: 100 }, { M: 0, Y: 266 }, { M: 0, Y: 267 }, { M: 0, Y: 267 }]);
    const preliminary = determineSpecialWinners(tied, createSeededDrawContext(3, 'special'));
    const result = complete(determineCameraAdmission({ ...tied, specialWinners: preliminary }, noDraw));
    expect(preliminary.draws).toHaveLength(1);
    expect(result.coalitions[0].admitted).toBe(false);
    expect(result.draws).toEqual([]);
    expect(() => determineCameraAdmission({ ...tied, specialWinners: { ...preliminary, simulationId: 77 } }, noDraw)).toThrow(/this simulation/);
    expect(() => determineCameraAdmission({ ...tied, specialWinners: { ...preliminary, winners: [] } }, noDraw)).toThrow(/complete preliminary/);
  });
});

describe('validated envelope to R26-09 stage boundary', () => {
  it.each(['camera', 'senato'] as const)('accepts full validated %s geography and attributed votes without generators or special proportional allocation', side => {
    const checked = validateReformScrutinyInput(singleFixture(side));
    if (!checked.valid) throw new Error(JSON.stringify(checked.issues));
    const envelope = checked.input;
    if (envelope.renewal.kind !== 'single') throw new Error('Expected single renewal');
    const chamber = envelope.renewal.chamber;
    const ordinary = chamber.ordinaryVotes.filter(row => chamber.colleges.find(c => c.id === row.collegeId)!.kind === 'ordinary')
      .map(row => {
        if (row.votes.kind !== 'attributed') throw new Error('Expected attributed fixture');
        return { collegeId: row.collegeId, listVotes: row.votes.listVotes };
      });
    const special = chamber.specialVotes.map(row => {
      if (row.votes.kind !== 'attributed') throw new Error('Expected attributed fixture');
      return { collegeId: row.collegeId, listVotes: row.votes.listVotes };
    });
    const input = { chamber, candidates: envelope.candidates, simulationId: envelope.simulationId,
      figures: aggregateElectoralFigures(chamber, ordinary, special) };
    const preliminary = determineSpecialWinners(input, noDraw);
    const result = complete(side === 'camera' ? determineCameraAdmission({ ...input, specialWinners: preliminary }, noDraw) : determineSenateAdmission(input, noDraw));
    expect(result.admittedSubjects).toEqual([{ kind: 'list', id: 'A' }]);
    expect(result.lists[0]).toMatchObject({ admitted: true, admittedAs: 'standalone', reasons: side === 'camera' ? ['national-3-percent'] : ['national-3-percent', 'regional-20-percent'] });
    expect(preliminary.winners).toHaveLength(side === 'camera' ? 5 : 7);
  });

  it('rejects swapped chamber entry points', () => {
    const f = admissionFixture('senato', { R: { A: 100 } });
    expect(() => determineCameraAdmission({ ...f, specialWinners: determineSpecialWinners(f, noDraw) }, noDraw)).toThrow(/Camera input/);
    expect(() => determineSenateAdmission({ ...f, chamber: { ...f.chamber, chamber: 'camera' } }, noDraw)).toThrow(/Senate input/);
  });
});

describe('N08 Senate regional access', () => {
  it.each([199, 200, 201])('admits an unprotected list at %i/1000 in any region nationwide, including a failed-coalition member', votes => {
    for (const linked of [false, true]) {
      const result = run(admissionFixture('senato', { R: { A: 300, Z: 8700 }, T: { M: votes, Y: 1000 - votes } }, linked ? { C: ['A', 'M'] } : {}));
      expect(list(result, 'M').admitted).toBe(votes >= 200);
      if (votes >= 200) expect(list(result, 'M')).toMatchObject({ admittedAs: 'standalone', reasons: ['regional-20-percent'], qualifyingRegionIds: ['T'] });
    }
  });

  it('uses ordinary or special regions, ignores zero-denominator regions, and qualifies coalition plus internal member', () => {
    const result = run(admissionFixture('senato', { R: { A: 220, B: 220, D: 220, E: 220, Z: 8120 },
      T: { M: 200, Y: 800 }, EMPTY: { Z: 0 } }, { C: ['A', 'B', 'D', 'E', 'M'] }), createSeededDrawContext(2, 'legal'));
    expect(result.coalitions[0]).toMatchObject({ admitted: true, qualifyingListIds: ['M'] });
    expect(list(result, 'M')).toMatchObject({ admittedAs: 'coalition-member', qualifyingRegionIds: ['T'] });
    // Rename the qualifying region to an ordinary, nonautonomous region.
    const ordinary = admissionFixture('senato', { R: { Z: 9000 }, N: { M: 200, Y: 800 } });
    expect(list(run(ordinary), 'M')).toMatchObject({ admittedAs: 'standalone', qualifyingRegionIds: ['N'] });
  });

  it.each(['camera', 'senato'] as Chamber[])('%s national admission includes special ballots and all excluded votes', side => {
    const f = admissionFixture(side, { R: { A: 299, Z: 9700 }, T: { A: 1 } }, {}, {}, 1);
    expect(list(run(f), 'A')).toMatchObject({ admitted: true, nationalVotes: 300 });
    expect(f.figures.ordinaryAllocation.validVotes).toBe(9999);
    expect(f.figures.national.validVotes).toBe(10000);
  });
});
