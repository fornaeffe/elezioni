import { describe, expect, it } from 'vitest';
import { commonPoliticsFixture } from '../../../../tests/fixtures/common-politics-input';
import { preparePoliticsExecution, PoliticsPreparationCache } from './prepare';
import { executionCheckpoint, checkpointStates } from './checkpoint';
import { politicsRunResources } from './resources';
import { REFORM, ROSATELLUM } from '../scenario/schema';
import { loadExecutionData } from './assets';

describe('CP-07 consumed-stage reuse and checkpoints', () => {
  it('reuses M02 fits for cosmetic/inactive/candidate/generation edits and only invalidates consumed stages', async () => {
    const f = await commonPoliticsFixture(REFORM, 'M02'), cache = new PoliticsPreparationCache();
    const a = await preparePoliticsExecution(f.scenario, f.readers, { cache });
    f.scenario.name = 'Renamed'; f.scenario.subjects.lists[0].name = 'Changed label'; f.scenario.subjects.lists[0].color = '#aabbcc';
    f.scenario.lawSettings[ROSATELLUM].candidateGeneration.uninominalToPlurinominalShare = .4;
    f.scenario.modelSettings.M01.parameterDate = '2026-10-15';
    f.scenario.lawSettings[REFORM].candidateGeneration.reuseProbability = .1;
    f.scenario.execution.simulationSeed = 'visible-new-run-seed';
    const b = await preparePoliticsExecution(f.scenario, f.readers, { cache });
    expect(b.reuse).toEqual({ fit: true, profiles: true }); expect(b.numericStates!.fit).toBe(a.numericStates!.fit);
    expect(b.keys.generationKey).not.toBe(a.keys.generationKey);
    f.scenario.execution.fittingSeed = 'visible-new-fit-seed';
    const c = await preparePoliticsExecution(f.scenario, f.readers, { cache });
    expect(c.reuse).toEqual({ fit: false, profiles: true });
    const file = await executionCheckpoint(c); expect(file).not.toBeNull();
    const restored = await checkpointStates(file, f.scenario, f.readers.implementationId);
    const d = await preparePoliticsExecution(f.scenario, f.readers, { numericStates: restored.states });
    expect(d.reuse).toEqual({ fit: true, profiles: true }); expect(d.generate(7)).toEqual(c.generate(7));
    f.scenario.name += ' edited';
    await expect(checkpointStates(file, f.scenario, f.readers.implementationId)).rejects.toThrow('CHECKPOINT_IDENTITY');
  }, 60000);
  it('restores M01 preparation on presentation edits without retaining unbounded revisions', async () => {
    const f = await commonPoliticsFixture(ROSATELLUM), cache = new PoliticsPreparationCache();
    await preparePoliticsExecution(f.scenario, f.readers, { cache }); const compiled = cache.m01!.compiled;
    f.scenario.name = 'Cosmetic'; await preparePoliticsExecution(f.scenario, f.readers, { cache });
    expect(cache.m01!.compiled).toBe(compiled); expect(await executionCheckpoint(await preparePoliticsExecution(f.scenario, f.readers, { cache }))).toBeNull();
    cache.clear(); expect(cache.m01).toBeUndefined();
    expect((await loadExecutionData(f.scenario, f.readers)).geography).toBeDefined();
  }, 60000);
  it('accepts more than 1000 requested simulations without running a campaign and rejects invalid resources', async () => {
    const f = await commonPoliticsFixture(ROSATELLUM), capacity = { maximum: 8, recommended: 3 } as Parameters<typeof politicsRunResources>[1];
    f.scenario.execution.simulations = 1001;
    expect(politicsRunResources(f.scenario, capacity)).toEqual({ requested: 3, actual: 3 });
    f.scenario.execution.workers = { kind: 'fixed', count: 9 };
    expect(() => politicsRunResources(f.scenario, capacity)).toThrow('1 to 8');
    f.scenario.execution.workers.count = 2; f.scenario.execution.simulations = Number.MAX_SAFE_INTEGER;
    expect(() => politicsRunResources(f.scenario, capacity)).toThrow('metadata budget');
  });
});
