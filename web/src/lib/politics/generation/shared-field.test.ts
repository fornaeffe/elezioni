import { describe, expect, it } from 'vitest';
import { fastGenerationFixture } from '../../../../tests/fixtures/fast-generation-input';
import { prepareM02Votes } from './m02';

const fixture = () => {
  const f = fastGenerationFixture();
  f.scenario.models.forecast = 'bounded-mass-pair-field-v2';
  f.scenario.references = []; f.scenario.nonValidReferences = [];
  f.context.models.chambers.profiles.find(p => p.id === f.scenario.chamberProfileId)!.gapMultiplier = 0;
  return f;
};
const prepare = (f: ReturnType<typeof fixture>) => prepareM02Votes(f.scenario, f.assets,
  { geography: f.context.seats.geography, models: f.context.models });
describe('M02 3.0.0 common political field acceptance', () => {
  it('keeps the entire future field shared at zero contrast, including a long horizon', () => {
    const f = fixture(), p = prepare(f);
    for (const draw of [1, 2, 7, 23]) {
      const audit = p.run(draw).audit;
      audit.countryForecast.camera!.forEach((v, j) => expect(audit.countryForecast.senato![j]).toBeCloseTo(v, 10));
    }
  });
  it('conditions each physical ballot after the shared movement without changing the other chamber', () => {
    const f = fixture(), baseline = prepare(f).run(7);
    const absent = f.scenario.lists.camera[0].id;
    for (const row of f.scenario.coverage.filter(c => c.chamber === 'camera')) row.listIds = row.listIds.filter(id => id !== absent);
    const changed = prepare(f).run(7);
    expect(changed.audit.countryForecast.senato).toEqual(baseline.audit.countryForecast.senato);
    for (const c of f.context.seats.geography.colleges.filter(c => c.chamber === 'senato'))
      expect(changed.counts.get(c.id)).toEqual(baseline.counts.get(c.id));
    for (const c of f.context.seats.geography.colleges.filter(c => c.chamber === 'camera'))
      expect(changed.counts.get(c.id)?.votes.has(absent)).toBe(false);
  });
  it('preserves intentionally different chamber references through a zero-movement forecast', () => {
    const f = fixture(); f.scenario.electionDate = f.scenario.referenceDate;
    f.assets.fit.audit.electionDiscrepancy = 0;
    f.scenario.references = (['camera', 'senato'] as const).map((channel, i) => ({ id: channel,
      scope: { kind: 'national' }, channel, subject: { kind: 'list', id: f.scenario.lists[channel][0].id },
      mean: i ? 0.7 : 0.2, sdPercentagePoints: 0 }));
    const p = prepare(f), result = p.run(7);
    for (const [ch, share] of [['camera', 0.2], ['senato', 0.7]] as const) {
      const row = result.audit.countryForecast[ch]!;
      const members = f.scenario.lists[ch][0].componentIds.map(id => p.componentIds.indexOf(id) + 1);
      expect(members.reduce((n, j) => n + row[j], 0) / (1 - row[0])).toBeCloseTo(share, 7);
    }
  });
  it('supports a new component referenced only in Senate while Camera retains an explicit zero', () => {
    const f = fixture();
    f.scenario.components.push({ id: 'new', kind: 'new', provenance: 'Declared new component',
      fallback: { geographicLogProfile: 0, profileSd: 0.45, localInnovationSd: 0.25, nationalInnovationSd: 0.35 } });
    for (const ch of ['camera', 'senato'] as const) {
      f.scenario.lists[ch].push({ ...f.scenario.lists[ch][0], id: 'new-list', componentIds: ['new'] });
      for (const c of f.scenario.coverage.filter(c => c.chamber === ch)) c.listIds.push('new-list');
    }
    f.scenario.references = (['camera', 'senato'] as const).map((channel, i) => ({ id: channel,
      scope: { kind: 'national' }, channel, subject: { kind: 'component', id: 'new' }, mean: i ? 0.1 : 0, sdPercentagePoints: 0 }));
    const result = prepare(f).run(2);
    for (const c of f.context.seats.geography.colleges.filter(c => c.chamber === 'camera'))
      expect(result.counts.get(c.id)!.votes.get('new-list')).toBe(0);
    expect(result.audit.countryForecast.senato!.at(-1)).toBeGreaterThan(0);
    f.scenario.renewal = 'senato'; f.scenario.references = f.scenario.references.filter(r => r.channel === 'senato');
    const single = prepare(f).run(2);
    expect(single.audit.countryForecast.camera).toBeUndefined();
    expect(single.audit.countryForecast.senato!.at(-1)).toBeGreaterThan(0);
  });
  it('shares the geographic fallback noise of a common new-component reference', () => {
    const f = fixture();
    f.scenario.components.push({ id: 'new', kind: 'new', provenance: 'Declared common support',
      fallback: { geographicLogProfile: 0, profileSd: 0.45, localInnovationSd: 0.25, nationalInnovationSd: 0.35 } });
    for (const ch of ['camera', 'senato'] as const) {
      f.scenario.lists[ch].push({ ...f.scenario.lists[ch][0], id: 'new-list', componentIds: ['new'] });
      for (const c of f.scenario.coverage.filter(c => c.chamber === ch)) c.listIds.push('new-list');
    }
    f.scenario.references = [{ id: 'new-common', scope: { kind: 'national' }, channel: 'common',
      subject: { kind: 'component', id: 'new' }, mean: 0.1, sdPercentagePoints: 1 }];
    const run = prepare(f).run(7);
    run.audit.countryForecast.camera!.forEach((v, j) => expect(run.audit.countryForecast.senato![j]).toBeCloseTo(v, 10));
  });
});
