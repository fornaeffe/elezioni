import { describe, it, expect, vi } from 'vitest';
import { fastReleaseFixture } from '../../../../../tests/fixtures/fast-release-input';
import { loadCandidate, loadCandidateSource } from './release';
import { encodeState } from './state-codec';
import * as information from './information';
import * as profiles from './profiles';
describe('FV-05 compatible precomputed startup', () => {
  it('loads accepted buffers without fitting or fetching original refit data', async () => {
    const f = await fastReleaseFixture(),
      requests: string[] = [],
      fit = vi.spyOn(information, 'fitReference').mockImplementation(() => {
        throw new Error('Unexpected default refit');
      }),
      profile = vi.spyOn(profiles, 'fitProfiles').mockImplementation(() => {
        throw new Error('Unexpected default profile fit');
      });
    try {
      const loaded = await loadCandidate(
        f.pointer,
        async (p) => {
          requests.push(p);
          return f.read(p);
        },
        { expectedGenerationImplementationId: f.implementation },
      );
      expect(loaded.assets.fit.political.values).toEqual(
        f.fitState.fit.political.values,
      );
      expect(loaded.prepared!.run(1).input.simulationId).toBe(1);
      expect(fit).not.toHaveBeenCalled();
      expect(profile).not.toHaveBeenCalled();
      expect(requests).not.toContain('unrequested-source.gz');
      await expect(loadCandidateSource(loaded.catalog, f.read)).rejects.toThrow(
        'Missing required',
      );
    } finally {
      fit.mockRestore();
      profile.mockRestore();
    }
  });
  it('rejects an incompatible implementation before loading the heavy states', async () => {
    const f = await fastReleaseFixture(),
      read = vi.fn(f.read);
    await expect(
      loadCandidate(f.pointer, read, {
        expectedGenerationImplementationId: 'different',
      }),
    ).rejects.toThrow('IMPLEMENTATION_MISMATCH');
    expect(read).toHaveBeenCalledTimes(1);
  });
  it('rejects mixed fit/catalog parents and never falls back to a default fit', async () => {
    const f = await fastReleaseFixture(),
      changed = await encodeState(
        'fast-share-fit',
        {
          ...f.fitState,
          parents: { ...f.fitState.parents, fitSeed: 'another-fit' },
        },
        f.metadata,
      );
    const catalog = await encodeState(
      'fast-share-catalog',
      {
        ...f.catalogState,
        assets: { ...f.catalogState.assets, fit: changed.descriptor },
      },
      f.metadata,
    );
    for (const set of [changed, catalog])
      for (const [p, b] of set.assets) f.assets.set(p, b);
    await expect(
      loadCandidate(
        {
          ...f.pointer,
          catalog: catalog.descriptor,
          catalogId: catalog.envelope.id,
        },
        f.read,
      ),
    ).rejects.toThrow('STATE_MISMATCH');
    f.assets.delete(f.fit.descriptor.path);
    await expect(loadCandidate(f.pointer, f.read)).rejects.toThrow(
      'Missing required',
    );
  });
  it('binds the scenario evidence selection to the fitted state declaration', async () => {
    const f = await fastReleaseFixture(),
      changed = await encodeState(
        'fast-share-scenario',
        {
          ...f.scenarioState,
          selection: {
            ...f.scenarioState.selection,
            referenceDate: '2026-10-20',
          },
        },
        f.metadata,
      ),
      catalog = await encodeState(
        'fast-share-catalog',
        {
          ...f.catalogState,
          scenarioId: changed.envelope.id,
          assets: { ...f.catalogState.assets, scenario: changed.descriptor },
        },
        f.metadata,
      );
    for (const set of [changed, catalog])
      for (const [p, b] of set.assets) f.assets.set(p, b);
    await expect(
      loadCandidate(
        {
          ...f.pointer,
          catalog: catalog.descriptor,
          catalogId: catalog.envelope.id,
        },
        f.read,
      ),
    ).rejects.toThrow('SOURCE_SELECTION_BINDING');
  });
});
