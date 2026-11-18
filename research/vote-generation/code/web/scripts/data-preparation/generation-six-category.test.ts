import { existsSync } from 'node:fs';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';
import { createSixCandidate, createSixCurrent, evaluateSixCase, fitSixModel, prepareSixCategoryStudy,
  reconcileSixMargins, roundSixDraw, sixIlr, sixInverseIlr, sixListCategory, type SixCase, type SixEvent } from './generation-six-category';

const event = (id: string, date: string, votes: number[]): SixEvent => ({ id, date,
  national: { electors: 2000, validVotes: 1400, votes: votes.map(v => v * 1400) },
  municipalities: ['a', 'b'].map((id, i) => ({ id, regionId: String(i + 1), electors: 1000, validVotes: 700,
    votes: votes.map(v => v * 700) })) });
const p = [.2, .15, .2, .1, .2, .15];
const before = event('old', '2008-04-13', p), target = event('camera-2013', '2013-02-24', p);
const c: SixCase = { event: target, baseline: before, cutoff: '2013-02-08', point: p, sigma: p.map(() => .08), pollIds: ['synthetic'], validPoint: .7 };

describe('six-category major-party study', () => {
  it('roundtrips six-part compositions and resolves only reviewed source labels', () => {
    sixInverseIlr(sixIlr(p)).forEach((v, i) => expect(v).toBeCloseTo(p[i], 12));
    expect(sixListCategory("FRATELLI D'ITALIA CON GIORGIA MELONI")).toBe('fdi');
    expect(sixListCategory("IL POPOLO DELLA LIBERTA'")).toBe('fi-pdl');
    expect(sixListCategory('ALLEANZA VERDI E SINISTRA')).toBe('other');
    expect(() => sixIlr([.2, .2, .2, .2, .2, 0])).toThrow('positive');
  });
  it('reconciles six national margins and rejects impossible support', () => {
    const shares = new Float64Array([...p, ...[.1, .2, .1, .2, .1, .3]]), weights = new Float64Array([1, 3]);
    const margin = reconcileSixMargins(shares, weights, p);
    expect(margin.error).toBeLessThan(1e-8);
    for (let j = 0; j < 6; j++) expect((shares[j] + 3 * shares[6 + j]) / 4).toBeCloseTo(p[j], 7);
    expect(() => reconcileSixMargins(new Float64Array(6), new Float64Array([1]), p)).toThrow('Infeasible');
  });
  it('fits only outcomes strictly preceding cutoff and does not fit its own target', () => {
    const first = fitSixModel(c, [c]); expect(first.trainingIds).toEqual([]); expect(first.df).toBe(3);
    const later: SixCase = { ...c, event: event('camera-2018', '2018-03-04', p), baseline: target, cutoff: '2018-02-16' };
    const fit = fitSixModel(later, [c, later]); expect(fit.trainingIds).toEqual(['camera-2013']); expect(fit.df).toBe(4);
    const changed = { ...later, event: event('camera-2018', '2018-03-04', [.1, .1, .1, .3, .3, .1]) };
    expect(fitSixModel(changed, [c, changed])).toEqual(fit);
  });
  it('generates deterministic joint six-category draws, then conserves integer local mass', () => {
    const fit = fitSixModel(c, [c]), generate = createSixCandidate(c, fit, 'joint-student-t', 'test');
    const a = generate(2), b = generate(2); expect(a).toEqual(b); expect(a.margin.error).toBeLessThan(1e-8);
    roundSixDraw(a.output, before.municipalities);
    for (let i = 0; i < 2; i++) {
      const valid = a.output[7 * i + 6] * 1000;
      const counts = Array.from(a.output.slice(7 * i, 7 * i + 6)).map(v => v * valid);
      expect(counts.reduce((n, v) => n + v, 0)).toBeCloseTo(valid, 8);
      counts.forEach(n => expect(n).toBeCloseTo(Math.round(n), 8));
      expect(valid).toBeGreaterThanOrEqual(0); expect(valid).toBeLessThanOrEqual(1000);
    }
  });
  it('uses the actual runtime generator and scores empirical predictive means', () => {
    const current = createSixCurrent(c, 'test'); expect(current(1)).toEqual(current(1)); expect(current(1)).toHaveLength(14);
    const scored = evaluateSixCase(c, fitSixModel(c, [c]), 'joint-gaussian', 16);
    expect(scored.plausibility.invalid).toBe(0); expect(scored.geography.matchedMunicipalities).toBe(2);
    expect(scored.nationalRows).toHaveLength(7); expect(scored.municipalByParty.map(r => r.party)).toContain('fdi');
    expect(scored.nationalRows[0].meanMae).toBeCloseTo(Math.abs(scored.nationalRows[0].predictiveMean - p[0]));
  });
});

const root = fileURLToPath(new URL('../../../', import.meta.url));
const available = ['camera-20080413', 'camera-20130224', 'camera-20180304', 'camera-20220925', 'europee-20090607', 'europee-20140525', 'europee-20190526', 'europee-20240609']
  .every(id => existsSync(resolve(root, `data/sources/poll-history/${id}.zip`)));
describe.skipIf(!available)('pinned six-category Camera input (requires all official archives)', () => {
  it('keeps FdI and FI separate from complement and uses no unreported FdI poll zero', async () => {
    const study = await prepareSixCategoryStudy(root);
    expect(study.cases.map(c => c.pollIds.length)).toEqual([1, 3, 3]);
    expect(study.cases[0].pollIds).toEqual(['pcm-551-q1']);
    expect(study.events[0].national.votes[3]).toBe(0);
    for (const e of study.events) expect(e.national.votes.reduce((a, b) => a + b, 0)).toBe(e.national.validVotes);
    expect(study.events.find(e => e.id === 'camera-2022')!.national.votes[3]).toBeGreaterThan(7000000);
  }, 30000);
});
