import { describe, expect, it } from 'vitest';
import { admissionFixture } from './admission.fixture';
import { determineSpecialWinners } from './special-winners';
import { createSeededDrawContext } from './ties';
import type { ScrutinyExecutionContext } from './result';

const noDraw: ScrutinyExecutionContext = { draw() { throw new Error('Unexpected draw'); } };

describe('N09 preliminary special uninominal winners', () => {
  it.each(['camera', 'senato'] as const)('%s: highest votes win despite national exclusion or candidate age', side => {
    const f = admissionFixture(side, { R: { Z: 99000 }, T: { M: 12, Y: 8 } }, {}, {}, 4);
    const opposingAges = { ...f, candidates: f.candidates.map(c => ({ ...c,
      birthDate: c.id.endsWith('-M') === (side === 'camera') ? '1970-01-01' : '1990-01-01' })) };
    const result = determineSpecialWinners(opposingAges, noDraw);
    expect(result.winners.map(w => w.votes)).toEqual([3, 3, 3, 3]);
    expect(result.winners.every(w => w.subject.kind === 'list' && w.subject.id === 'M' && w.linkedListIds[0] === 'M')).toBe(true);
    expect(result.draws).toEqual([]);
    expect(JSON.parse(JSON.stringify(result))).toEqual(result);
  });

  it.each(['camera', 'senato'] as const)('%s: exact birth dates resolve vote ties with opposite chamber age criteria', side => {
    const f = admissionFixture(side, { T: { M: 200, Y: 200 } }, {}, {}, 4);
    const dated = { ...f, candidates: f.candidates.map(c => ({ ...c, birthDate: c.id.endsWith('-M') ? '1980-01-02' : '1980-01-01' })) };
    const result = determineSpecialWinners(dated, noDraw);
    expect(result.winners.every(w => w.candidateId.endsWith(side === 'camera' ? '-M' : '-Y'))).toBe(true);
    expect(result.draws).toEqual([]);
    expect(result.warnings).toEqual([]);
    expect(result.winners[0].tie.steps).toEqual([{ criterion: 'candidate-votes', remainingIds: ['T0-M', 'T0-Y'] },
      { criterion: 'birth-date', remainingIds: [side === 'camera' ? 'T0-M' : 'T0-Y'] }]);
  });

  it.each(['camera', 'senato'] as const)('%s: equal dates use warned I03 draws in canonical college order', side => {
    const f = admissionFixture(side, { T: { M: 200, Y: 200 } }, {}, {}, 4);
    const before = structuredClone(f);
    const a = determineSpecialWinners(f, createSeededDrawContext(55, 'special'));
    const reordered = { ...f, candidates: [...f.candidates].reverse(), chamber: { ...f.chamber,
      colleges: [...f.chamber.colleges].reverse(), specialCandidatures: [...f.chamber.specialCandidatures].reverse(),
      specialVotes: [...f.chamber.specialVotes].reverse().map(row => ({ ...row, votes: { ...row.votes, candidateTotals: [...row.votes.candidateTotals].reverse() } })) } };
    expect(determineSpecialWinners(reordered, createSeededDrawContext(55, 'special'))).toEqual(a);
    expect(a.draws).toHaveLength(4);
    expect(a.warnings).toHaveLength(4);
    expect(a.warnings.every(w => w.interpretationId === 'I03' && w.severity === 'warning' && w.simulationId === f.simulationId)).toBe(true);
    expect(f).toEqual(before);
  });

  it('works before raw special list-vote attribution and preserves every coalition link separately from affiliation', () => {
    const f = admissionFixture('camera', { T: { M: 100, A: 200, Y: 100 } }, { C: ['A', 'M'] }, {}, 4);
    const components = { ...f, chamber: { ...f.chamber, specialVotes: f.chamber.specialVotes.map(row => ({ ...row,
      votes: { kind: 'components' as const, directListVotes: row.votes.kind === 'attributed' ? row.votes.listVotes : [],
        candidateOnlyVotes: row.votes.candidateTotals.map(c => ({ candidateId: c.candidateId, votes: 0 })),
        candidateTotals: row.votes.candidateTotals, provenance: row.votes.provenance } })) } };
    const a = determineSpecialWinners(f, noDraw);
    expect(determineSpecialWinners(components, noDraw)).toEqual(a);
    expect(a.winners[0]).toMatchObject({ subject: { kind: 'coalition', id: 'C' }, linkedListIds: ['A', 'M'], affiliationListId: 'M', votes: 75 });
  });

  it('uses Camera VdA versus TAA law references and the same younger-age rule', () => {
    const f = admissionFixture('camera', { T: { M: 200, Y: 200 } }, {}, {}, 1);
    const vda = { ...f, chamber: { ...f.chamber, territories: f.chamber.territories.map(t => ({ ...t, kind: 'vda' as const })) },
      candidates: f.candidates.map(c => ({ ...c, birthDate: c.id.endsWith('-M') ? '1980-01-01' : '1981-01-01' })) };
    expect(determineSpecialWinners(vda, noDraw).winners[0]).toMatchObject({ candidateId: 'T0-Y', lawReference: 'C93(2)-(3)' });
  });

  it('does not invent percentages for zero votes and can still resolve an all-zero candidate tie by age', () => {
    const f = admissionFixture('senato', { T: { M: 0, Y: 0 } }, {}, {}, 1);
    const dated = { ...f, candidates: f.candidates.map(c => ({ ...c, birthDate: c.id.endsWith('-M') ? '1980-01-01' : '1981-01-01' })) };
    expect(determineSpecialWinners(dated, noDraw).winners[0]).toMatchObject({ candidateId: 'T0-M', votes: 0 });
  });

  it('rejects missing, duplicate and unknown contributions before any draw', () => {
    const f = admissionFixture('camera', { T: { M: 200, Y: 200 } }, {}, {}, 4);
    expect(() => determineSpecialWinners({ ...f, chamber: { ...f.chamber, specialVotes: f.chamber.specialVotes.slice(1) } }, noDraw)).toThrow(/each|per special/);
    expect(() => determineSpecialWinners({ ...f, chamber: { ...f.chamber, specialVotes: [...f.chamber.specialVotes, f.chamber.specialVotes[0]] } }, noDraw)).toThrow(/special college/);
    const broken = { ...f, chamber: { ...f.chamber, specialVotes: f.chamber.specialVotes.map((r, i) => i === 3 ? { ...r,
      votes: { ...r.votes, candidateTotals: r.votes.candidateTotals.slice(1) } } : r) } };
    expect(() => determineSpecialWinners(broken, noDraw)).toThrow(/all filed special candidate totals/);
    expect(() => determineSpecialWinners({ ...f, candidates: [] }, noDraw)).toThrow(/validated candidate/);
  });
});
