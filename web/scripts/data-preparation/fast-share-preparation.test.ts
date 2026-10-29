import { fileURLToPath } from 'node:url';
import { describe, it, expect } from 'vitest';
import { prepareSharePolls, reviewedEarlyView } from './fast-share-polls';
import { loadSourceMemberships, sourceMembership } from './fast-share-memberships';
import { createUnitResolver, resultAvailability, shareSourceManifests, type ResultAvailabilityPolicy } from './fast-share-history';
import type { TerritoryCatalog } from '../../src/lib/politics/reform-2026/geography';
const root = fileURLToPath(new URL('../../../', import.meta.url));
describe('FV-03 promoted reviewed source preparation', () => {
  it('uses the next day after final polling, across multi-day contests and calendar boundaries', async () => {
    const { policy } = await shareSourceManifests(root);
    for (const [id, date, expected] of [
      ['camera-2022','2022-09-25','2022-09-26'],
      ['camera-2008','2008-04-13','2008-04-15'],
      ['camera-2013','2013-02-24','2013-02-26'],
      ['fixture-leap','2024-02-29','2024-03-01'],
      ['fixture-year','2023-12-31','2024-01-01']
    ]) {
      const actual = resultAvailability(id, date, policy.resultAvailability);
      expect(actual.availableDate).toBe(expected); expect(actual.availabilityBasis).toContain('Assumed available by end');
    }
  });
  it('preserves evidenced availability dates and rejects invalid overrides', () => {
    const policy: ResultAvailabilityPolicy = { method: 'retrospective-next-day-end-v1', lagDaysAfterFinalPollingDay: 1,
      finalPollingDays: { e: '2022-09-26' }, knownDates: { e: { date: '2022-09-29', citation: 'Pinned publication evidence' } } };
    expect(resultAvailability('e','2022-09-25',policy)).toEqual({ availableDate: '2022-09-29', availabilityBasis: 'Known availability by end of civil day; source: Pinned publication evidence' });
    policy.knownDates.e.date = '2022-09-26';
    expect(resultAvailability('e','2022-09-25',policy).availableDate).toBe('2022-09-26');
    policy.knownDates.e.date = '2022-09-25'; expect(() => resultAvailability('e','2022-09-25',policy)).toThrow('known result');
    policy.knownDates.e = { date: '2022-09-29', citation: '' }; expect(() => resultAvailability('e','2022-09-25',policy)).toThrow('citation');
    policy.finalPollingDays.e = '2022-09-24'; expect(() => resultAvailability('e','2022-09-25',policy)).toThrow('Final polling');
  });
  it('repairs early FDI/FI only by refining the same conserved original residual', () => {
    const original = { id: 'q', shares: [0.2,0.1,0.3,0.4] }, refined = { id: 'q', shares: [0.2,0.1,0.3,0.1,0.2,0.1] };
    expect(reviewedEarlyView(original, refined)).toEqual({ groups: [['pd'],['m5s'],['lega'],['fdi'],['fi-pdl']], shares: [0.2,0.1,0.3,0.1,0.2], other: 0.1 });
    expect(() => reviewedEarlyView(original, { ...refined, id: 'different-survey' })).toThrow();
    expect(() => reviewedEarlyView(original, { ...refined, shares: [0.1,0.2,0.3,0.1,0.2,0.1] })).toThrow();
  });
  it('retains joint memberships and treats special groups as opaque instead of splitting them', async () => {
    const input = await loadSourceMemberships(root);
    const joint = sourceMembership(input.policy, 'camera-2018', 'LIBERI E UGUALI');
    expect(new Set(joint.members)).toEqual(new Set(['sel-si','possible','art1']));
    const special = sourceMembership(input.policy, 'senato-2022', 'PARTITO DEMOCRATICO', true);
    expect(special.opaque).toBe(true); expect(special.members).toHaveLength(1); expect(special.members[0]).not.toBe('pd');
    expect(input.citations.sources.some(c => c.id === 'official-label')).toBe(true);
  });
  it('prepares authoritative poll JSON offline, preserving exclusions, sample policy and contemporary atoms', async () => {
    const { polls } = await prepareSharePolls(root), supported = polls.filter(q => q.status === 'supported');
    expect(polls).toHaveLength(41); expect(supported).toHaveLength(38);
    expect(new Set(supported.map(q => q.surveyId)).size).toBe(supported.length);
    for (const p of supported) {
      expect(p.effectiveSample).toBe(Math.min(600,(p.questionSampleSize ?? p.sampleSize * p.decidedFraction) / 2));
      expect(p.normalized.reduce((n, a) => n + a.share, 0)).toBeCloseTo(1, 10);
      expect(p.original.length).toBeGreaterThan(0); expect(p.fieldworkEnd <= p.availableDate).toBe(true);
    }
    expect(polls.filter(p => p.method === 'reviewed-six-category-FDI-FI-repair').length).toBeGreaterThan(0);
    const contemporary = polls.find(p => p.id === 'pcm-14342-q1')!;
    expect(contemporary.normalized.some(a => a.members.includes('poll-only-futuro-nazionale'))).toBe(true);
    expect(contemporary.normalized.some(a => a.members.includes('lega'))).toBe(true);
    expect(polls.filter(p => p.status === 'excluded').every(p => p.reasons.some(r => r.startsWith('Conditional')))).toBe(true);
  });
  it('keeps ambiguous names unmatched and same-place target identities explicit', () => {
    const catalog = { regions: [{ id: '01', name: 'Piemonte' }], colleges: [], memberships: [], municipalities: [
      { code: '001', name: 'Alpha', regionId: '01', provinceId: '001', population: 10 },
      { code: '002', name: 'Same', regionId: '01', provinceId: '001', population: 10 },
      { code: '003', name: 'Same', regionId: '01', provinceId: '002', population: 10 }
    ] } as unknown as TerritoryCatalog;
    const locate = createUnitResolver(catalog);
    expect(locate('PIEMONTE','ALPHA',null).targetCode).toBe('001');
    const ambiguous = locate('PIEMONTE','SAME',null);
    expect(ambiguous.match).toBe('unmatched'); expect(ambiguous.targetCode).toBeNull();
  });
});
