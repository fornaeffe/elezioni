import { describe, it, expect } from 'vitest';
import { generationKeys } from './release';
import { fastGenerationFixture } from '../../../../../tests/fixtures/fast-generation-input';
describe('FV-05 composition/run semantic boundaries', () => {
  it('ignores list labels, display/set order and source commentary', async () => {
    const f = fastGenerationFixture(),
      before = await generationKeys(f.scenario, f.context, 'electorate');
    f.scenario.lists.camera = f.scenario.lists.camera
      .map((l) => ({
        ...l,
        name: l.name + ' revised',
        componentIds: [...l.componentIds].reverse(),
      }))
      .reverse();
    f.scenario.components.reverse();
    f.scenario.components.forEach(
      (c) => (c.provenance += ' citation correction'),
    );
    f.scenario.coverage.reverse();
    f.scenario.coverage.forEach((c) => c.listIds.reverse());
    expect(await generationKeys(f.scenario, f.context, 'electorate')).toEqual(
      before,
    );
  });
  it('separates simulation seeds, future reference edits and conditional electorates', async () => {
    const f = fastGenerationFixture(),
      base = await generationKeys(f.scenario, f.context, 'e1');
    f.scenario.seed += '2';
    const seed = await generationKeys(f.scenario, f.context, 'e1');
    expect(seed.compositionKey).toBe(base.compositionKey);
    expect(seed.runKey).not.toBe(base.runKey);
    f.scenario.references = [
      {
        id: 'a',
        scope: { kind: 'national' },
        channel: 'camera',
        subject: { kind: 'list', id: f.scenario.lists.camera[0].id },
        mean: 0.4,
        sdPercentagePoints: 2,
      },
    ];
    expect(
      (await generationKeys(f.scenario, f.context, 'e1')).compositionKey,
    ).not.toBe(base.compositionKey);
    f.scenario.references = [];
    expect(
      (await generationKeys(f.scenario, f.context, 'e2')).compositionKey,
    ).not.toBe(base.compositionKey);
  });
  it('binds actual chamber/preference parameters even when a profile ID stays the same', async () => {
    const f = fastGenerationFixture(),
      base = await generationKeys(f.scenario, f.context, 'e');
    f.context.models.preferences.profiles[0].intensityMultiplier = 0.9;
    const pref = await generationKeys(f.scenario, f.context, 'e');
    expect(pref.compositionKey).toBe(base.compositionKey);
    expect(pref.runKey).not.toBe(base.runKey);
    f.context.models.chambers.profiles[0].gapMultiplier = 2;
    expect(
      (await generationKeys(f.scenario, f.context, 'e')).compositionKey,
    ).not.toBe(base.compositionKey);
  });
});
