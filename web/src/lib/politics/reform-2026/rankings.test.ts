import { describe, expect, it } from 'vitest';
import { rankPreferenceCandidates } from './rankings';
import { aggregateElectoralFigures } from './figures';
import { figureFixture, preferenceSlate } from './electoral-figures.fixture';
import { exactFraction } from './exact';

describe('N05 preferences and percentages', () => {
  it('excludes the capolista, sorts by preferences then filed order, and divides by all valid college votes', () => {
    const f = figureFixture();
    const figures = aggregateElectoralFigures(f.chamber, f.ordinary, f.special).colleges[0].figures;
    const slate = preferenceSlate();
    const result = rankPreferenceCandidates(slate, figures);
    if (result.status !== 'complete') throw new Error();
    expect(result.capolistaId).toBe('P1-A-0');
    expect(result.candidates.map(c => c.candidateId)).toEqual(['P1-A-2', 'P1-A-3', 'P1-A-1', 'P1-A-6', 'P1-A-4', 'P1-A-5']);
    expect(result.candidates[0].percentage).toEqual({ status: 'defined', value: exactFraction(3000, 107) });
    expect(result.listPercentage).toEqual({ status: 'defined', value: exactFraction(6400, 107) });
    // 95 valid preference marks can come from fewer list ballots; do not sum
    // preference marks into the list or all-valid denominator.
    expect(result.candidates.reduce((n, c) => n + c.preferences, 0)).toBe(95);
  });

  it('keeps missing preferences distinct from six explicit zeros', () => {
    const f = figureFixture();
    const figures = aggregateElectoralFigures(f.chamber, f.ordinary, f.special).colleges[0].figures;
    const slate = preferenceSlate();
    expect(rankPreferenceCandidates({ ...slate, preferences: { kind: 'missing', reason: 'unavailable' } }, figures)).toMatchObject({ status: 'missing-preferences', reason: 'unavailable' });
    if (slate.preferences.kind !== 'counts') throw new Error();
    const zero = rankPreferenceCandidates({ ...slate, preferences: { ...slate.preferences, counts: slate.preferences.counts.map(c => ({ ...c, count: 0 })) } }, figures);
    if (zero.status !== 'complete') throw new Error();
    expect(zero.candidates.map(c => c.candidateId)).toEqual(slate.candidateIds.slice(1));
    expect(zero.candidates[0].percentage).toEqual({ status: 'defined', value: exactFraction(0) });
  });

  it('uses the Camera TAA constituency as denominator and list figure', () => {
    const f = figureFixture();
    const taa = aggregateElectoralFigures(f.chamber, f.ordinary, f.special).taaProportional[0].figures;
    const slate = preferenceSlate('TP');
    const result = rankPreferenceCandidates({ ...slate, preferences: { kind: 'counts', counts: slate.candidateIds.slice(1).map(candidateId => ({ candidateId, count: 2 })),
      provenance: { kind: 'supplied', reference: 'test' }, validity: { kind: 'adjudicated', reference: 'test' } } }, taa);
    if (result.status !== 'complete') throw new Error();
    expect(result.listPercentage).toEqual({ status: 'defined', value: exactFraction(50) });
    expect(result.candidates[0].percentage).toEqual({ status: 'defined', value: exactFraction(10) });
  });

  it('reports zero denominators and rejects missing counts or capolista counts', () => {
    const f = figureFixture();
    const zero = (rows: typeof f.ordinary) => rows.map(r => ({ ...r, listVotes: r.listVotes.map(v => ({ ...v, votes: 0 })) }));
    const figures = aggregateElectoralFigures(f.chamber, zero(f.ordinary), zero(f.special)).colleges[0].figures;
    const slate = preferenceSlate();
    if (slate.preferences.kind !== 'counts') throw new Error();
    const prefs = { ...slate.preferences, counts: slate.preferences.counts.map(c => ({ ...c, count: 0 })) };
    const result = rankPreferenceCandidates({ ...slate, preferences: prefs }, figures);
    if (result.status !== 'complete') throw new Error();
    expect(result.candidates.every(c => c.percentage.status === 'undefined')).toBe(true);
    expect(() => rankPreferenceCandidates({ ...slate, preferences: { ...prefs, counts: prefs.counts.slice(1) } }, figures)).toThrow(/non-capolista/);
    expect(() => rankPreferenceCandidates({ ...slate, preferences: { ...prefs, counts: [{ candidateId: slate.candidateIds[0], count: 0 }, ...prefs.counts.slice(1)] } }, figures)).toThrow(/non-capolista/);
  });
});
