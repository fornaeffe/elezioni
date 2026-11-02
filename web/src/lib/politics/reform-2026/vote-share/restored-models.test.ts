import { describe, expect, it } from 'vitest';
import { fastGenerationFixture } from '../../../../../tests/fixtures/fast-generation-input';
import { prepareFastGeneration } from './generation';

describe('M02 2.0.0 restored reference law', () => {
  it('uses local reference normalization rather than compulsory country calibration', () => {
    const f = fastGenerationFixture();
    f.scenario.models.forecast = 'bounded-mass-pair-field-v2';
    f.scenario.renewal = 'camera';
    f.scenario.references = [];
    const n = f.assets.fit.componentIds.length + 1;
    for (let i = 0; i < f.assets.cameraProfile.municipalityIds.length; i++)
      f.assets.cameraProfile.means[i * n + 1] += 1.5;
    const local = prepareFastGeneration(f.scenario, f.assets, f.context);
    f.scenario.models.forecastReference = 'country-calibrated';
    const calibrated = prepareFastGeneration(f.scenario, f.assets, f.context);
    const aggregate = (p: typeof local) => p.cells.camera.reduce((total, c, i) =>
      total + c.electors * p.centralReference.camera[i][1], 0) /
      p.cells.camera.reduce((total, c) => total + c.electors, 0);
    expect(aggregate(local) - aggregate(calibrated)).toBeGreaterThan(0.05);
  });
  it('removes unfiled components before the movement field is built', () => {
    const f = fastGenerationFixture();
    f.scenario.models.forecast = 'bounded-mass-pair-field-v2';
    f.scenario.renewal = 'camera';
    f.scenario.references = [];
    const absent = f.scenario.lists.camera[1];
    for (const row of f.scenario.coverage.filter(row => row.chamber === 'camera'))
      row.listIds = row.listIds.filter(id => id !== absent.id);
    const prepared = prepareFastGeneration(f.scenario, f.assets, f.context);
    for (const id of absent.componentIds) {
      const j = prepared.componentIds.indexOf(id) + 1;
      expect(prepared.centralReference.camera.every(row => row[j] === 0)).toBe(true);
    }
    const generated = prepared.run(7);
    expect(generated.trace.modelVersion).toBe('M02@2.0.0');
    const ch = generated.input.renewal;
    expect(ch.kind).toBe('single');
  });
});
